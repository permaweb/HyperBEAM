%%% @doc This module implements HTTP Message Signatures
%%% as described in RFC-9421 https://datatracker.ietf.org/doc/html/rfc9421

-module(hb_http_signature).

% Signing/Verifying
-export([authority/3, sign/2, sign/3, verify/2, verify/3]).
% Mapping
-export([sf_signature/1, sf_signature_params/2, sf_signature_param/1]).

% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.7-14
-define(EMPTY_QUERY_PARAMS, $?).
% https://datatracker.ietf.org/doc/html/rfc9421#name-signature-parameters
-define(SIGNATURE_PARAMS, [created, expired, nonce, alg, keyid, tag]).

-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").

-type fields() :: #{
	binary() | atom() | string() => binary() | atom() | string()
}.
-type request_message() :: #{
	url => binary(),
	method => binary(),
	headers => fields(),
	trailers => fields(),
	is_absolute_form => boolean()
}.
-type response_message() :: #{
	status => integer(),
	headers => fields(),
	trailers => fields()
}.
-type component_identifier() :: {
	item,
	{string, binary()},
	{binary(), integer() | boolean() | {string | token | binary, binary()}}
}.

%%% A map that contains signature parameters metadata as described
%%% in https://datatracker.ietf.org/doc/html/rfc9421#name-signature-parameters
%%%
%%% All values are optional, but in our use-case "alg" and "keyid" will
%%% almost always be provided.
%%%
%%% #{
%%% 	created => 1733165109, % a unix timestamp
%%% 	expires => 1733165209, % a unix timestamp
%%% 	nonce => <<"foobar">,
%%% 	alg => <<"rsa-pss-sha512">>,
%%% 	keyid => <<"6eVuWgpNgv3bxfNgFrIiTkzE8Yb0V2omShxS4uKyzpw">>
%%% 	tag => <<"HyperBEAM">>
%%% }
-type signature_params() :: #{atom() | binary() | string() => binary() | integer()}.

%%% The state encapsulated as the "Authority".
%%% It includes an ordered list of parsed component identifiers, used for extracting values
%%% from the Request/Response Message Context, as well as the signature parameters
%%% used when creating the signature and encode in the signature base.
%%%
%%% This is effectively the State of an Authority, used to sign a Request/Response Message
%%% Context.
%%%
%%% #{
%%% 	component_identifiers => [{item, {string, <<"@method">>}, []}]
%%% 	sig_params => #{
%%% 		created => 1733165109, % a unix timestamp
%%% 		expires => 1733165209, % a unix timestamp
%%% 		nonce => <<"foobar">,
%%% 		alg => <<"rsa-pss-sha512">>,
%%% 		keyid => <<"6eVuWgpNgv3bxfNgFrIiTkzE8Yb0V2omShxS4uKyzpw">>
%%% 		tag => <<"HyperBEAM">>
%%% 	}
%%% }
-type authority_state() :: #{
	component_identifiers => [component_identifier()],
	% TODO: maybe refine this to be more explicit w.r.t valid signature params
	sig_params => signature_params(),
	key => binary()
}.

%%% @doc A helper to validate and produce an "Authority" State
-spec authority(
	[binary() | component_identifier()],
	#{binary() => binary() | integer()},
	{} %TODO: type out a key_pair
) -> authority_state().
authority(ComponentIdentifiers, SigParams, PubKey = {KeyType = {ALG, _}, _Pub}) when is_atom(ALG) ->
    % Only the public key is provided, so use an stub binary for private
    % which will trigger errors downstream if it's needed, which is what we want
    authority(ComponentIdentifiers, SigParams, {{KeyType, <<>>, PubKey}, PubKey});
authority(ComponentIdentifiers, SigParams, PrivKey = {KeyType = {ALG, _}, _Priv, Pub}) when is_atom(ALG) ->
    % Only the private key was provided, so derive the public from private
    authority(ComponentIdentifiers, SigParams, {PrivKey, {KeyType, Pub}});
authority(ComponentIdentifiers, SigParams, KeyPair = {{_, _, _}, {_, _}}) ->
    #{
		% parse each component identifier into a Structured Field Item:
		%
		% <<"\"Example-Dict\";key=\"foo\"">> -> {item, {string, <<"Example-Dict">>}, [{<<"key">>, {string, <<"foo">>}}]}
		% See hb_http_structuted_fields for parsed Structured Fields full data structures
		%
		% sf_item/1 handles when the argument is already parsed.
		% This provides a feedback loop, in case any encoded component identifier is
		% not properly encoded
		component_identifiers => lists:map(fun sf_item/1, ComponentIdentifiers),
		% TODO: add checks to allow only valid signature parameters
		% https://datatracker.ietf.org/doc/html/rfc9421#name-signature-parameters
		sig_params => SigParams,
        % TODO: validate the key is supported?
		key_pair => KeyPair
	}.

%%% @doc using the provided Authority and Request Message Context, and create a Signature and SignatureInput
%%% that can be used to additional signatures to a corresponding HTTP Message
-spec sign(authority_state(), request_message()) -> {ok, {binary(), binary(), binary()}}.
sign(Authority, Req) ->
	sign(Authority, Req, #{}).
%%% @doc using the provided Authority and Request/Response Messages Context, create a Name, Signature and SignatureInput
%%% that can be used to additional signatures to a corresponding HTTP Message
-spec sign(authority_state(), request_message(), response_message()) -> {ok, {binary(), binary(), binary()}}.
sign(Authority, Req, Res) ->
    {Priv, {KeyType, PubKey}} = maps:get(key_pair, Authority),
    % Create the signature base and signature-input values
    SigParamsWithKeyAndAlg = maps:merge(
        maps:get(sig_params, Authority),
        % TODO: determine alg based on KeyType from authority
        % TODO: is there a more turn-key way to get the wallet address
        #{ alg => <<"rsa-pss-sha512">>, keyid => hb_util:encode(bin(ar_wallet:to_address(PubKey, KeyType))) } 
    ),
    ?no_prod(<<"Is the wallet address as keyid kosher here?">>),
    AuthorityWithSigParams = maps:put(sig_params, SigParamsWithKeyAndAlg, Authority),
	{SignatureInput, SignatureBase} = signature_base(AuthorityWithSigParams, Req, Res),
    % Now perform the actual signing
	Signature = ar_wallet:sign(Priv, SignatureBase, sha512),
	{ok, {SignatureInput, Signature}}.

%%% @doc same verify/3, but with an empty Request Message Context
verify(Verifier, Msg) ->
    % Assume that the Msg is a response message, and use an empty Request message context
    %
    % A corollary is that a signature containing any components from the request will produce
    % an error. It is the caller's responsibility to provide the required Message Context
    % in order to verify the signature
    verify(Verifier, #{}, Msg).

%%% @doc Given the signature name, and the Request/Response Message Context
%%% verify the named signature by constructing the signature base and comparing
verify(#{ sig_name := SigName, key := Key }, Req, Res) ->
    % Signature and Signature-Input fields are each themself a dictionary structured field.
    % Ergo, we can use our same utilities to extract the value at the desired key, in this case,
    % the signature name. Because our utilities already implement the relevant portions
    % of RFC-9421, we get the error handling here as well.
    % 
    %  See https://datatracker.ietf.org/doc/html/rfc9421#section-3.2-3.2
    SigNameParams = [{<<"key">>, {string, bin(SigName)}}],
    SignatureIdentifier = {item, {string, <<"signature">>}, SigNameParams},
    SignatureInputIdentifier = {item, {string, <<"signature-input">>}, SigNameParams},
    % extract signature and signature params
    case {extract_field(SignatureIdentifier, Req, Res), extract_field(SignatureInputIdentifier, Req, Res)} of
        {{ok, {_, EncodedSignature}}, {ok, {_, SignatureInput}}} ->
            % The signature may be encoded ie. as binary, so we need to parse it further
            %  as a structured field
            {item, {_, Signature}, _} = hb_http_structured_fields:parse_item(EncodedSignature),
            % The value encoded within signature input is also a structured field,
            % specifically an inner list that encodes the ComponentIdentifiers
            % and the Signature Params.
            % 
            % So we must parse this value, and then use it to construct the signature base
            [{list, ComponentIdentifiers, SigParams}] = hb_http_structured_fields:parse_list(SignatureInput),
            % TODO: HACK convert parsed sig params into a map that authority() can handle
            % maybe authority() should handle both parsed and unparsed SigParams, similar to ComponentIdentifiers
            SigParamsMap = lists:foldl(
                % TODO: does not support SF decimal params
                fun
                    ({Name, {_Kind, Value}}, Map) -> maps:put(Name, Value, Map);
                    ({Name, Value}, Map) -> maps:put(Name, Value, Map)
                end,
                #{},
                SigParams
            ),
            % Construct the signature base using the parsed parameters
            Authority = authority(ComponentIdentifiers, SigParamsMap, Key),
            {_, SignatureBase} = signature_base(Authority, Req, Res),
            {_Priv, Pub} = maps:get(key_pair, Authority),
            % Now verify the signature base signed with the provided key matches the signature
            ar_wallet:verify(Pub, SignatureBase, Signature, sha512);
        % An issue with parsing the signature
        {SignatureErr, {ok, _}} -> SignatureErr;
        % An issue with parsing the signature input
        {{ok, _}, SignatureInputErr} -> SignatureInputErr;
        % An issue with parsing both, so just return the first one from the signature parsing
        % TODO: maybe could merge the errors?
        {SignatureErr, _} -> SignatureErr
    end.

%%% @doc create the signature base that will be signed in order to create the Signature and SignatureInput.
%%%
%%% This implements a portion of RFC-9421
%%% See https://datatracker.ietf.org/doc/html/rfc9421#name-creating-the-signature-base
signature_base(Authority, Req, Res) when is_map(Authority) ->
    ComponentIdentifiers = maps:get(component_identifiers, Authority),
	ComponentsLine = signature_components_line(ComponentIdentifiers, Req, Res),
	ParamsLine = signature_params_line(ComponentIdentifiers, maps:get(sig_params, Authority)),
    SignatureBase = join_signature_base(ComponentsLine, ParamsLine),
	{ParamsLine, SignatureBase}.

join_signature_base(ComponentsLine, ParamsLine) ->
    SignatureBase = <<ComponentsLine/binary, <<"\n">>/binary, <<"\"@signature-params\": ">>/binary, ParamsLine/binary>>,
    SignatureBase.

%%% @doc Given a list of Component Identifiers and a Request/Response Message context, create the
%%% "signature-base-line" portion of the signature base
%%% TODO: catch duplicate identifier: https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.1
%%%
%%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.1
signature_components_line(ComponentIdentifiers, Req, Res) ->
	ComponentsLines = lists:map(
		fun(ComponentIdentifier) ->
			% TODO: handle errors?
			{ok, {I, V}} = identifier_to_component(ComponentIdentifier, Req, Res),
			<<I/binary, <<": ">>/binary, V/binary>>
		end,
		ComponentIdentifiers
	),
	ComponentsLine = lists:join(<<"\n">>, ComponentsLines),
	bin(ComponentsLine).

%%% @doc construct the "signature-params-line" part of the signature base.
%%%
%%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4
signature_params_line(ComponentIdentifiers, SigParams) ->
	SfList = sf_signature_params(ComponentIdentifiers, SigParams),
	Res = hb_http_structured_fields:list(SfList),
	bin(Res).

%%% @doc Given a Component Identifier and a Request/Response Messages Context
%%% extract the value represented by the Component Identifier, from the Messages Context,
%%% and return the normalized form of the identifier, along with the extracted encoded value.
%%%
%%% Generally speaking, a Component Identifier may reference a "Derived" Component, a Message Field,
%%% or a sub-component of a Message Field.
%%%
%%% Since a Component Identifier is itself a Structured Field, it may also specify parameters, which are
%%% used to describe behavior such as which Message to derive a field or sub-component of the field,
%%% and how to encode the value as part of the signature base.
identifier_to_component(Identifier, Req, Res) when is_list(Identifier) ->
	identifier_to_component(list_to_binary(Identifier), Req, Res);
identifier_to_component(Identifier, Req, Res) when is_atom(Identifier) ->
	identifier_to_component(atom_to_binary(Identifier), Req, Res);
identifier_to_component(Identifier, Req, Res) when is_binary(Identifier) ->
	identifier_to_component(hb_http_structured_fields:parse_item(Identifier), Req, Res);
identifier_to_component(ParsedIdentifier = {item, {_Kind, Value}, _Params}, Req, Res) ->
	case Value of
		<<$@, _R/bits>> -> derive_component(ParsedIdentifier, Req, Res);
		_ -> extract_field(ParsedIdentifier, Req, Res)
	end.

%%% @doc Given a Component Identifier and a Request/Response Messages Context
%%% extract the value represented by the Component Identifier, from the Messages Context,
%%% specifically a field on a Message within the Messages Context,
%%% and return the normalized form of the identifier, along with the extracted encoded value.
%%%
%%% This implements a portion of RFC-9421
%%% See https://datatracker.ietf.org/doc/html/rfc9421#name-http-fields
extract_field(Identifier, Req, Res) when map_size(Res) == 0 ->
	extract_field(Identifier, Req, Res);
extract_field({item, {_Kind, IParsed}, IParams}, Req, Res) ->
	[IsStrictFormat, IsByteSequenceEncoded, DictKey] = [
		find_sf_strict_format_param(IParams),
		find_sf_byte_sequence_param(IParams),
		find_sf_key_param(IParams)
	],
	case (IsStrictFormat orelse DictKey =/= false) andalso IsByteSequenceEncoded of
		true ->
			% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.2
			{conflicting_params_error, <<"Component Identifier parameter 'bs' MUST not be used with 'sf' or 'key'">>};
		_ ->
			Lowered = lower_bin(IParsed),
			NormalizedItem = hb_http_structured_fields:item({item, {string, Lowered}, IParams}),
			[IsRequestIdentifier, IsTrailerField] = [find_sf_request_param(IParams), find_sf_trailer_param(IParams)],
			% There may be multiple fields that match the identifier on the Msg,
			% so we filter, instead of find
			MaybeRawFields = lists:filter(
				fun({Key, _Value}) -> Key =:= Lowered end,
				% Field names are normalized to lowercase in the signature base and also are case insensitive.
				% So by converting all the names to lowercase here, we simoultaneously normalize them, and prepare
				% them for comparison in one pass.
				[
					{lower_bin(Key), Value}
                % TODO: how can we maintain the order msg fields, especially in the case where there are
                % multiple fields with the same name, and order must be preserved
				 || {Key, Value} <- maps:to_list(
						maps:get(
							% The field will almost certainly be a header, but could also be a trailer
							% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-18.10.1
							case IsTrailerField of true -> trailers; false -> headers end,
							% The header may exist on any message in the context of the signature
							% which could be the Request or Response Message
							% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-18.8.1
							case IsRequestIdentifier of true -> Req; false -> Res end
						)
					)
				]
			),
			case MaybeRawFields of
				[] ->
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.6
					{field_not_found_error, <<"Component Identifier for a field MUST be present on the message">>};
				FieldPairs ->
					% The Field was found, but we still need to potentially parse it
					% (it could be a Structured Field) and potentially extract
					% subsequent values ie. specific dictionary key and its parameters, or further encode it
					case
						extract_field_value(
							[bin(Value) || {_Key, Value} <- FieldPairs],
							[DictKey, IsStrictFormat, IsByteSequenceEncoded]
						)
					of
						{ok, Extracted} -> {ok, {bin(NormalizedItem), bin(Extracted)}};
						E -> E
					end
			end
	end.

%%% @doc Extract values from the field and return the normalized field,
%%% along with encoded value
extract_field_value(RawFields, [Key, IsStrictFormat, IsByteSequenceEncoded]) ->
	% TODO: (maybe this already works?) empty string for empty header
	HasKey = case Key of false -> false; _ -> true end,
	case not (HasKey orelse IsStrictFormat orelse IsByteSequenceEncoded) of
		% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-5
		true ->
			Normalized = [trim_and_normalize(Field) || Field <- RawFields],
			{ok, bin(lists:join(<<", ">>, Normalized))};
		_ ->
			case IsByteSequenceEncoded of
				% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.3-2
				true ->
					SfList = [
						{item, {binary, trim_and_normalize(Field)}, []}
					 || Field <- RawFields
					],
					sf_encode(SfList);
				_ ->
					Combined = bin(lists:join(<<", ">>, RawFields)),
					case sf_parse(Combined) of
						% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.1-3
						{error, _} ->
							{sf_parsing_error, <<"Component Identifier value could not be parsed as a structured field">>};
						{ok, SF} ->
							case Key of
								% Not accessing a key, so just re-serialize, which should
								% properly format the data in Strict-Formatting style
								false -> sf_encode(SF);
								_ -> extract_dictionary_field_value(SF, Key)
							end
					end
			end
	end.

%%% @doc Extract a value from a Structured Field, and return the normalized field,
%%% along with the encoded value
extract_dictionary_field_value(StructuredField = [Elem | _Rest], Key) ->
	case Elem of
		{Name, _} when is_binary(Name) ->
			case lists:keyfind(Key, 1, StructuredField) of
				% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.2-5
				false ->
					{sf_dicionary_key_not_found_error,
						<<"Component Identifier references key not found in dictionary structured field">>};
				{_, Value} ->
					sf_encode(Value)
			end;
		_ ->
			{sf_not_dictionary_error, <<"Component Identifier cannot reference key on a non-dictionary structured field">>}
	end.

%%% @doc Given a Component Identifier and a Request/Response Messages Context
%%% extract the value represented by the Component Identifier, from the Messages Context,
%%% specifically a "Derived" Component within the Messages Context,
%%% and return the normalized form of the identifier, along with the extracted encoded value.
%%%
%%% This implements a portion of RFC-9421
%%% See https://datatracker.ietf.org/doc/html/rfc9421#name-derived-components
derive_component(Identifier, Req, Res) when map_size(Res) == 0 ->
	derive_component(Identifier, Req, Res, req);
derive_component(Identifier, Req, Res) ->
	derive_component(Identifier, Req, Res, res).
derive_component({item, {_Kind, IParsed}, IParams}, Req, Res, Subject) ->
	case find_sf_request_param(IParams) andalso Subject =:= req of
		% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.3
		true ->
			{req_identifier_error,
				<<"A Component Identifier may not contain a req parameter if the target is a request message">>};
		_ ->
			Lowered = lower_bin(IParsed),
			NormalizedItem = hb_http_structured_fields:item({item, {string, Lowered}, IParams}),
			Result =
				case Lowered of
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.2.1
					<<"@method">> ->
						{ok, upper_bin(maps:get(method, Req))};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.4.1
					<<"@target-uri">> ->
						{ok, bin(maps:get(url, Req))};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.6.1
					<<"@authority">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						Authority = maps:get(host, URI),
						{ok, lower_bin(Authority)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.8.1
					<<"@scheme">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						Scheme = maps:get(scheme, URI),
						{ok, lower_bin(Scheme)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.10.1
					<<"@request-target">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						% If message contains the absolute form value, then
						% the value must be the absolut url
						%
						% TODO: maybe better way to distinguish besides a flag
						% on the request?
						%
						% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.5-10
						RequestTarget =
							case maps:get(is_absolute_form, Req, false) of
								true -> maps:get(url, Req);
								_ -> lists:join($?, [maps:get(path, URI), maps:get(query, URI, ?EMPTY_QUERY_PARAMS)])
							end,
						{ok, bin(RequestTarget)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.12.1
					<<"@path">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						Path = maps:get(path, URI),
						{ok, bin(Path)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.14.1
					<<"@query">> ->
						URI = uri_string:parse(maps:get(url, Req)),
						% No query params results in a "?" value
						% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.7-14
						Query =
							case maps:get(query, URI, <<>>) of
								<<>> -> ?EMPTY_QUERY_PARAMS;
								Q -> Q
							end,
						{ok, bin(Query)};
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.16.1
					<<"@query-param">> ->
						case find_sf_name_param(IParams) of
							% The name parameter MUST be provided when specifiying a @query-param
							% Derived Component. See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.8-1
							false ->
								{req_identifier_error, <<"@query_param Derived Component Identifier must specify a name parameter">>};
							Name ->
								URI = uri_string:parse(maps:get(url, Req)),
								QueryParams = uri_string:dissect_query(maps:get(query, URI, "")),
								QueryParam =
									case lists:keyfind(Name, 1, QueryParams) of
										{_, QP} -> QP;
										% An missing or empty query param value results in
										% an empty string value in the signature base
										% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.8-4
										_ -> ""
									end,
								{ok, bin(QueryParam)}
						end;
					% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-4.18.1
					<<"@status">> ->
						case Subject =:= req of
							% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.9-8
							true ->
								{res_identifier_error, <<"@status Derived Component must not be used if target is a request message">>};
							_ ->
								Status = maps:get(status, Res, <<"200">>),
								{ok, Status}
						end
				end,
			case Result of
				{ok, V} -> {ok, {bin(NormalizedItem), V}};
				E -> E
			end
	end.

%%%
%%% Strucutured Field Utilities
%%%

%%% @doc construct the structured field Parameter for the signature parameter,
%%% checking whether the parameter name is valid according RFC-9421
%%% 
%%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.3-3
sf_signature_param({Name, Param}) ->
    NormalizedName = bin(Name),
    NormalizedNames = lists:map(fun bin/1, ?SIGNATURE_PARAMS),
    case lists:member(NormalizedName, NormalizedNames) of
        false -> {unknown_signature_param, NormalizedName};
        % all signature params are either integer or string values
        true -> case Param of
            I when is_integer(I) -> {ok, {NormalizedName, Param}};
            P when is_atom(P) orelse is_list(P) orelse is_binary(P) -> {ok, {NormalizedName, {string, bin(P)}}};
            P -> {invalid_signature_param_value, P}
        end
    end.

%%% @doc construct the structured field List for the
%%% "signature-params-line" part of the signature base.
%%% 
%%% Can be parsed into a binary by simply passing to hb_structured_fields:list/1
%%%
%%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4
sf_signature_params(ComponentIdentifiers, SigParams) when is_map(SigParams) ->
    AsList = maps:to_list(SigParams),
    Sorted = lists:sort(fun({Key1, _}, {Key2, _}) -> Key1 < Key2 end, AsList),
    sf_signature_params(ComponentIdentifiers, Sorted);
sf_signature_params(ComponentIdentifiers, SigParams) when is_list(SigParams) ->
    [
		{
			list,
			lists:map(
                fun(ComponentIdentifier) ->
					{item, {_Kind, Value}, Params} = sf_item(ComponentIdentifier),
					{item, {string, lower_bin(Value)}, Params}
				end,
                ComponentIdentifiers
            ),
			lists:foldl(
                fun (RawParam, Params) ->
                    case sf_signature_param(RawParam) of
                        {ok, Param} -> Params ++ [Param];
                        % Ignore unknown signature parameters
                        {unknown_signature_param, _} -> Params
                        % TODO: what to do about invalid_signature_param_value?
                        % For now will cause badmatch
                    end
                end,
                [],
                SigParams  
            )
		}
	].

% TODO: should this also handle the Signature already being encoded?
sf_signature(Signature) ->
    {item, {binary, Signature}, []}.

%%% @doc Attempt to parse the binary into a data structure that represents
%%% an HTTP Structured Field.
%%%
%%% Lacking some sort of "hint", there isn't a way to know which "kind" of Structured Field
%%% the binary is, apriori. So we simply try each parser, and return the first invocation that
%%% doesn't result in an error.
%%%
%%% If no parser is successful, then we return an error tuple
sf_parse(Raw) when is_list(Raw) -> sf_parse(list_to_binary(Raw));
sf_parse(Raw) when is_binary(Raw) ->
	Parsers = [
		fun hb_http_structured_fields:parse_list/1,
		fun hb_http_structured_fields:parse_dictionary/1,
		fun hb_http_structured_fields:parse_item/1
	],
	sf_parse(Parsers, Raw).

sf_parse([], _Raw) ->
    {error, undefined};
sf_parse([Parser | Rest], Raw) ->
    case catch Parser(Raw) of
        % skip parsers that fail
        {'EXIT', _} -> sf_parse(Rest, Raw);
        Parsed -> {ok, Parsed}
    end.

%%% @doc Attempt to encode the data structure into an HTTP Structured Field.
%%% This is the inverse of sf_parse.
sf_encode(StructuredField = {list, _, _}) ->
	% The value is an inner_list, and so needs to be wrapped with an outer list
	% before being serialized
	sf_encode(fun hb_http_structured_fields:list/1, [StructuredField]);
sf_encode(StructuredField = {item, _, _}) ->
	sf_encode(fun hb_http_structured_fields:item/1, StructuredField);
sf_encode(StructuredField = [Elem | _Rest]) ->
	sf_encode(
		% Both an sf list and dictionary is represented in Erlang as a List of pairs
		% but a dictionary's members will always be a pair whose first value
		% is a binary, so we can match on that to determine which serializer to use
		case Elem of
			{Name, _} when is_binary(Name) -> fun hb_http_structured_fields:dictionary/1;
			_ -> fun hb_http_structured_fields:list/1
		end,
		StructuredField
	).
sf_encode(Serializer, StructuredField) ->
	case catch Serializer(StructuredField) of
		{'EXIT', _} -> {error, <<"Could not serialize into structured field">>};
		Parsed -> {ok, Parsed}
	end.

%%% @doc Attempt to parse the provided value into an HTTP Structured Field Item
sf_item(SfItem = {item, {_Kind, _Parsed}, _Params}) ->
	SfItem;
sf_item(ComponentIdentifier) when is_list(ComponentIdentifier) ->
	sf_item(list_to_binary(ComponentIdentifier));
sf_item(ComponentIdentifier) when is_binary(ComponentIdentifier) ->
    sf_item(hb_http_structured_fields:parse_item(ComponentIdentifier)).

%%% @doc Given a parameter Name, extract the Parameter value from the HTTP Structured Field
%%% data structure.
%%%
%%% If no value is found, then false is returned
find_sf_param(Name, Params, Default) when is_list(Name) ->
    find_sf_param(list_to_binary(Name), Params, Default);
find_sf_param(Name, Params, Default) ->
	% [{<<"name">>,{string,<<"baz">>}}]
	case lists:keyfind(Name, 1, Params) of
		false -> Default;
		{_, {string, Value}} -> Value;
		{_, {token, Value}} -> Value;
		{_, {binary, Value}} -> Value;
		{_, Value} -> Value
	end.

%%%
%%% https://datatracker.ietf.org/doc/html/rfc9421#section-6.5.2-1
%%% using functions allows encapsulating default values
%%%
find_sf_strict_format_param(Params) -> find_sf_param(<<"sf">>, Params, false).
find_sf_key_param(Params) -> find_sf_param(<<"key">>, Params, false).
find_sf_byte_sequence_param(Params) -> find_sf_param(<<"bs">>, Params, false).
find_sf_trailer_param(Params) -> find_sf_param(<<"tr">>, Params, false).
find_sf_request_param(Params) -> find_sf_param(<<"req">>, Params, false).
find_sf_name_param(Params) -> find_sf_param(<<"name">>, Params, false).

%%%
%%% Data Utilities
%%%

% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-5
trim_and_normalize(Bin) ->
	binary:replace(trim_ws(Bin), <<$\n>>, <<" ">>, [global]).

upper_bin(Item) when is_atom(Item) -> upper_bin(atom_to_list(Item));
upper_bin(Item) when is_binary(Item) -> upper_bin(binary_to_list(Item));
upper_bin(Item) when is_list(Item) -> bin(string:uppercase(Item)).

lower_bin(Item) when is_atom(Item) -> lower_bin(atom_to_list(Item));
lower_bin(Item) when is_binary(Item) -> lower_bin(binary_to_list(Item));
lower_bin(Item) when is_list(Item) -> bin(string:lowercase(Item)).

bin(Item) when is_atom(Item) -> atom_to_binary(Item, utf8);
bin(Item) when is_integer(Item) ->
    case Item of
        % Treat integer as an ASCII code
        N when N > 0 andalso N < 256 -> <<N>>;
        N -> integer_to_binary(N)
    end;
bin(Item) ->
    iolist_to_binary(Item).

%%% @doc Recursively trim space characters from the beginning of the binary
trim_ws(<<$\s, Bin/bits>>) -> trim_ws(Bin);
%%% @doc No space characters at the beginning so now trim them from the end
%%% recrusively
trim_ws(Bin) -> trim_ws_end(Bin, byte_size(Bin) - 1).

trim_ws_end(_, -1) ->
	<<>>;
trim_ws_end(Value, N) ->
	case binary:at(Value, N) of
		$\s ->
			trim_ws_end(Value, N - 1);
		% No more space characters matches on the end
		% So extract the bytes up to N, and this is our trimmed value
		_ ->
			S = N + 1,
			<<Trimmed:S/binary, _/bits>> = Value,
			Trimmed
	end.

%%%
%%% TESTS
%%%

trim_ws_test() ->
	?assertEqual(<<"hello world">>, trim_ws(<<"      hello world      ">>)),
	?assertEqual(<<>>, trim_ws(<<"">>)),
	?assertEqual(<<>>, trim_ws(<<"         ">>)),
	ok.

sign_test() ->
	Req = #{
		url => <<"https://foo.bar/id-123/Data?another=one&fizz=buzz">>,
		method => "get",
		headers => #{
			<<"foo">> => <<"req-b-bar">>
		},
		trailers => #{}
	},
	Res = #{
		status => 202,
		headers => #{
			"fizz" => "res-l-bar",
			<<"Foo">> => "a=1, b=2;x=1;y=2, c=(a b   c), d"
		},
		trailers => #{}
	},
	% Ensure both parsed, and serialized SFs are handled
	ComponentIdentifiers = [
		{item, {string, <<"@method">>}, []},
		<<"\"@path\"">>,
		{item, {string, <<"foo">>}, [{<<"req">>, true}]},
		"\"foo\";key=\"a\""
	],
	SigParams = #{},
    Key = hb:wallet(),
	Authority = authority(ComponentIdentifiers, SigParams, Key),

    ?assertMatch(
        {ok, {_SignatureInput, _Signature}},
        sign(Authority, Req, Res)
    ),
	ok.

verify_test() ->
    Req = #{
		url => <<"https://foo.bar/id-123/Data?another=one&fizz=buzz">>,
		method => "get",
		headers => #{
			<<"foo">> => <<"req-b-bar">>
		},
		trailers => #{}
	},
	Res = #{
		status => 202,
		headers => #{
			"fizz" => "res-l-bar",
			<<"Foo">> => "a=1, b=2;x=1;y=2, c=(a b   c), d"
		},
		trailers => #{}
	},
	ComponentIdentifiers = [
		{item, {string, <<"@method">>}, []},
		<<"\"@path\"">>,
		{item, {string, <<"foo">>}, [{<<"req">>, true}]},
		"\"foo\";key=\"a\""
	],
	SigParams = #{},
    Key = {_Priv, Pub} = hb:wallet(),
	Authority = authority(ComponentIdentifiers, SigParams, Key),

    % Create the signature and signature input
    % TODO: maybe return the SF data structures instead, to make appending to headers easier?
    % OR we could wrap behind an api ie. sf_dictionary_put(Key, SfValue, Dict)
    {ok, {SignatureInput, Signature}} = sign(Authority, Req, Res),
    SigName = <<"awesome">>,
    [ParsedSignatureInput] = hb_http_structured_fields:parse_list(SignatureInput),
    NewHeaders = maps:merge(
        maps:get(headers, Res),
        #{
            % https://datatracker.ietf.org/doc/html/rfc9421#section-4.2-1
            <<"signature">> => bin(hb_http_structured_fields:dictionary(#{ SigName => {item, {binary, Signature}, []} })),
            <<"signature-input">> => bin(hb_http_structured_fields:dictionary(#{ SigName => ParsedSignatureInput }))
        }
    ),

    SignedRes = maps:put(headers, NewHeaders, Res),
    Result = verify(#{ sig_name => SigName, key => Pub }, Req, SignedRes),
    ?assert(Result),
	ok.

join_signature_base_test() ->
	ParamsLine =
		<<"(\"@method\" \"@path\" \"foo\";req \"foo\";key=\"a\");created=1733165109501;nonce=\"foobar\";keyid=\"key1\"">>,
	ComponentsLine = <<"\"@method\": GET\n\"@path\": /id-123/Data\n\"foo\";req: req-b-bar\n\"foo\";key=\"a\": 1">>,
	?assertEqual(
		<<ComponentsLine/binary, <<"\n">>/binary, <<"\"@signature-params\": ">>/binary, ParamsLine/binary>>,
		join_signature_base(ComponentsLine, ParamsLine)
	).

signature_components_line_test() ->
	Req = #{
		url => <<"https://foo.bar/id-123/Data?another=one&fizz=buzz">>,
		method => "get",
		headers => #{
			<<"foo">> => <<"req-b-bar">>
		},
		trailers => #{}
	},
	Res = #{
		status => 202,
		headers => #{
			"fizz" => "res-l-bar",
			<<"Foo">> => "a=1, b=2;x=1;y=2, c=(a b   c), d"
		},
		trailers => #{}
	},
	ComponentIdentifiers = [
		<<"\"@method\"">>,
		<<"\"@path\"">>,
		% parsed SF items are also handled
		{item, {string, <<"foo">>}, [{<<"req">>, true}]},
		<<"\"foo\";key=\"a\"">>
	],
	?assertEqual(
		<<"\"@method\": GET\n\"@path\": /id-123/Data\n\"foo\";req: req-b-bar\n\"foo\";key=\"a\": 1">>,
		signature_components_line(ComponentIdentifiers, Req, Res)
	).

signature_params_line_test() ->
	Params = #{created => 1733165109501, nonce => "foobar", keyid => "key1"},
	ContentIdentifiers = [
		<<"\"Content-Length\"">>, <<"\"@method\"">>, "\"@Path\"", "\"content-type\";req", "\"example-dict\";sf"
	],
	Result = signature_params_line(ContentIdentifiers, Params),
	?assertEqual(
		<<"(\"content-length\" \"@method\" \"@path\" \"content-type\";req \"example-dict\";sf);created=1733165109501;keyid=\"key1\";nonce=\"foobar\"">>,
		Result
	).

extract_field_msg_access_test() ->
	Req = #{
		url => <<"https://foo.bar/id-123/Data?another=one&fizz=buzz">>,
		method => "get",
		headers => #{
			<<"foo">> => <<"req-b-bar">>
		},
		trailers => #{
			another => <<"req-tr-atom-one">>
		}
	},
	Res = #{
		status => 202,
		headers => #{
			"fizz" => "res-l-bar",
			"A-field" => "   first\none",
			"a-field" => "   second   "
		},
		trailers => #{
			<<"Woo">> => <<"res-tr-uppercase-woo">>
		}
	},
	% req header + binary key + binary value
	?assertEqual(
		{ok, {<<"\"foo\";req">>, <<"req-b-bar">>}},
		extract_field({item, {string, <<"foo">>}, [{<<"req">>, true}]}, Req, Res)
	),

	% req trailer + atom key + binary value
	?assertEqual(
		{ok, {<<"\"another\";req;tr">>, <<"req-tr-atom-one">>}},
		extract_field({item, {string, <<"another">>}, [{<<"req">>, true}, {<<"tr">>, true}]}, Req, Res)
	),

	% res header + list key + list value
	?assertEqual(
		{ok, {<<"\"fizz\"">>, <<"res-l-bar">>}},
		extract_field({item, {string, <<"fizz">>}, []}, Req, Res)
	),

	% res trailer + binary uppercase key + binary value
	?assertEqual(
		{ok, {<<"\"woo\";tr">>, <<"res-tr-uppercase-woo">>}},
		extract_field({item, {string, <<"woo">>}, [{<<"tr">>, true}]}, Req, Res)
	),

	% multiple fields, with obs and newlines
	?assertEqual(
		{ok, {<<"\"a-field\"">>, <<"first one, second">>}},
		extract_field({item, {string, <<"a-field">>}, []}, Req, Res)
	).

extract_field_bs_test() ->
	Req = #{},
	Res = #{
		status => 202,
		headers => #{
			<<"Foo">> => "foobar",
			<<"A-Field">> => "first",
			<<"a-field">> => "second",
			<<"b-field">> => "first, second"
		},
		trailers => #{}
	},

	% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.3-4

	?assertEqual(
		{ok, {<<"\"foo\";bs">>, <<":Zm9vYmFy:">>}},
		extract_field({item, {string, <<"foo">>}, [{<<"bs">>, true}]}, Req, Res)
	),

	?assertEqual(
		{ok, {<<"\"a-field\";bs">>, <<":Zmlyc3Q=:, :c2Vjb25k:">>}},
		extract_field({item, {string, <<"a-field">>}, [{<<"bs">>, true}]}, Req, Res)
	),

	?assertEqual(
		{ok, {<<"\"b-field\";bs">>, <<":Zmlyc3QsIHNlY29uZA==:">>}},
		extract_field({item, {string, <<"b-field">>}, [{<<"bs">>, true}]}, Req, Res)
	).

extract_field_sf_test() ->
	Req = #{},
	Res = #{
		status => 202,
		headers => #{
			<<"Foo">> => "a=1, b=2;x=1;y=2, c=(a b   c), d"
		},
		trailers => #{}
	},
	% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.2-6
	?assertEqual(
		{ok, {<<"\"foo\"">>, <<"a=1, b=2;x=1;y=2, c=(a b   c), d">>}},
		extract_field({item, {string, <<"foo">>}, []}, Req, Res)
	),

	?assertEqual(
		{ok, {<<"\"foo\";sf">>, <<"a=1, b=2;x=1;y=2, c=(a b c), d=?1">>}},
		extract_field({item, {string, <<"foo">>}, [{<<"sf">>, true}]}, Req, Res)
	),

	?assertEqual(
		{ok, {<<"\"foo\";key=\"a\"">>, <<"1">>}},
		extract_field({item, {string, <<"foo">>}, [{<<"key">>, {string, <<"a">>}}]}, Req, Res)
	),
	% inner-list
	?assertEqual(
		{ok, {<<"\"foo\";key=\"c\"">>, <<"(a b c)">>}},
		extract_field({item, {string, <<"foo">>}, [{<<"key">>, {string, <<"c">>}}]}, Req, Res)
	),
	% boolean
	?assertEqual(
		{ok, {<<"\"foo\";key=\"d\"">>, <<"?1">>}},
		extract_field({item, {string, <<"foo">>}, [{<<"key">>, {string, <<"d">>}}]}, Req, Res)
	),
	% params
	?assertEqual(
		{ok, {<<"\"foo\";key=\"b\"">>, <<"2;x=1;y=2">>}},
		extract_field({item, {string, <<"foo">>}, [{<<"key">>, {string, <<"b">>}}]}, Req, Res)
	).

extract_field_error_conflicting_params_test() ->
	Req = #{
		url => <<"https://foo.bar/id-123/Data?another=one&fizz=buzz">>,
		method => "get",
		headers => #{
			<<"foo">> => "a=1, b=2;x=1;y=2, c=(a b   c), d"
		},
		trailers => #{}
	},
	Res = #{
		status => 202,
		headers => #{},
		trailers => #{}
	},
	Expected = conflicting_params_error,
	{E, _} = extract_field({item, {string, <<"foo">>}, [{<<"bs">>, true}, {<<"sf">>, true}]}, Req, Res),
	?assertEqual(Expected, E),

	{E2, _} = extract_field({item, {string, <<"foo">>}, [{<<"bs">>, true}, {<<"key">>, {string, <<"foo">>}}]}, Req, Res),
	?assertEqual(Expected, E2).

extract_field_error_field_not_found_test() ->
	Req = #{
		url => <<"https://foo.bar/id-123/Data?another=one&fizz=buzz">>,
		method => "get",
		headers => #{
			<<"foo">> => "req-b-bar"
		},
		trailers => #{}
	},
	Res = #{
		status => 202,
		headers => #{},
		trailers => #{}
	},
	Expected = field_not_found_error,
	% req headers
	{E, _} = extract_field({item, {string, <<"not-foo">>}, [{<<"req">>, true}]}, Req, Res),
	?assertEqual(Expected, E),
	% req trailers
	{E2, _} = extract_field({item, {string, <<"not-foo">>}, [{<<"req">>, true}, {<<"tr">>, true}]}, Req, Res),
	?assertEqual(Expected, E2),
	% res headers
	{E3, _} = extract_field({item, {string, <<"not-foo">>}, []}, Req, Res),
	?assertEqual(Expected, E3),
	% res trailers
	{E4, _} = extract_field({item, {string, <<"not-foo">>}, [{<<"tr">>, true}]}, Req, Res),
	?assertEqual(Expected, E4).

extract_field_error_not_sf_dictionary_test() ->
	Req = #{
		url => <<"https://foo.bar/id-123/Data?another=one&fizz=buzz">>,
		method => "get",
		headers => #{
			<<"foo">> => "req-b-bar"
		},
		trailers => #{}
	},
	Res = #{
		status => 202,
		headers => #{},
		trailers => #{}
	},
	Expected = sf_not_dictionary_error,
	{E, _M} = extract_field({item, {string, <<"foo">>}, [{<<"req">>, true}, {<<"key">>, {string, <<"smth">>}}]}, Req, Res),
	?assertEqual(Expected, E).

extract_field_error_sf_dictionary_key_not_found_test() ->
	Req = #{
		url => <<"https://foo.bar/id-123/Data?another=one&fizz=buzz">>,
		method => "get",
		headers => #{
			<<"foo">> => "a=1, b=2;x=1;y=2, c=(a b   c), d"
		},
		trailers => #{}
	},
	Res = #{
		status => 202,
		headers => #{},
		trailers => #{}
	},
	Expected = sf_dicionary_key_not_found_error,
	{E, _M} = extract_field({item, {string, <<"foo">>}, [{<<"req">>, true}, {<<"key">>, {string, <<"smth">>}}]}, Req, Res),
	?assertEqual(Expected, E).

derive_component_test() ->
	Url = <<"https://foo.bar/id-123/Data?another=one&fizz=buzz">>,
	Req = #{
		url => Url,
		method => "get",
		headers => #{}
	},
	Res = #{
		status => 202
	},

	% normalize method (uppercase) + method
	?assertEqual(
		{ok, {<<"\"@method\"">>, <<"GET">>}},
		derive_component({item, {string, <<"@method">>}, []}, Req, Res)
	),

	?assertEqual(
		{ok, {<<"\"@target-uri\"">>, Url}},
		derive_component({item, {string, <<"@target-uri">>}, []}, Req, Res)
	),

	?assertEqual(
		{ok, {<<"\"@authority\"">>, <<"foo.bar">>}},
		derive_component({item, {string, <<"@authority">>}, []}, Req, Res)
	),

	?assertEqual(
		{ok, {<<"\"@scheme\"">>, <<"https">>}},
		derive_component({item, {string, <<"@scheme">>}, []}, Req, Res)
	),

	?assertEqual(
		{ok, {<<"\"@request-target\"">>, <<"/id-123/Data?another=one&fizz=buzz">>}},
		derive_component({item, {string, <<"@request-target">>}, []}, Req, Res)
	),

	% absolute form
	?assertEqual(
		{ok, {<<"\"@request-target\"">>, Url}},
		derive_component({item, {string, <<"@request-target">>}, []}, maps:merge(Req, #{is_absolute_form => true}), Res)
	),

	?assertEqual(
		{ok, {<<"\"@path\"">>, <<"/id-123/Data">>}},
		derive_component({item, {string, <<"@path">>}, []}, Req, Res)
	),

	?assertEqual(
		{ok, {<<"\"@query\"">>, <<"another=one&fizz=buzz">>}},
		derive_component({item, {string, <<"@query">>}, []}, Req, Res)
	),

	% no query params
	?assertEqual(
		{ok, {<<"\"@query\"">>, <<"?">>}},
		derive_component({item, {string, <<"@query">>}, []}, maps:merge(Req, #{url => <<"https://foo.bar/id-123/Data">>}), Res)
	),

	% empty query params
	?assertEqual(
		{ok, {<<"\"@query\"">>, <<"?">>}},
		derive_component({item, {string, <<"@query">>}, []}, maps:merge(Req, #{url => <<"https://foo.bar/id-123/Data?">>}), Res)
	),

	?assertEqual(
		{ok, {<<"\"@query-param\";name=\"fizz\"">>, <<"buzz">>}},
		derive_component({item, {string, <<"@query-param">>}, [{<<"name">>, {string, <<"fizz">>}}]}, Req, Res)
	),

	% normalize identifier (lowercase) + @status
	?assertEqual(
		{ok, {<<"\"@status\"">>, 202}},
		derive_component({item, {string, <<"@Status">>}, []}, Req, Res)
	),
	ok.

derive_component_error_req_param_on_request_target_test() ->
	Result = derive_component({item, {string, <<"@query-param">>}, [{<<"req">>, true}]}, #{}, #{}, req),
	?assertEqual(
		{req_identifier_error, <<"A Component Identifier may not contain a req parameter if the target is a request message">>},
		Result
	).

derive_component_error_query_param_no_name_test() ->
	Result = derive_component({item, {string, <<"@query-param">>}, [{<<"noname">>, {string, <<"foo">>}}]}, #{}, #{}, req),
	?assertEqual(
		{req_identifier_error, <<"@query_param Derived Component Identifier must specify a name parameter">>},
		Result
	).

derive_component_error_status_req_target_test() ->
	Result = derive_component({item, {string, <<"@status">>}, []}, #{}, #{}, req),
	{E, _M} = Result,
	?assertEqual(res_identifier_error, E).
