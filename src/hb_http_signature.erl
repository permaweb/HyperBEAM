-module(hb_http_signature).

-export([authority/3, sign/2, sign/3, signature_params_line/2]).

% https://datatracker.ietf.org/doc/html/rfc9421#section-2.2.7-14
-define(EMPTY_QUERY_PARAMS, $?).

-include_lib("eunit/include/eunit.hrl").

%%%
%%% Ideal API
%%% authority(ComponentIdentifiers, Params) -> Authority
%%%
%%% sign(Authority, Req, Res) -> {ok, {SigName, SigInput, Sig}
%%% verify(Authority, SigName, Msg) -> {ok}
%%%

authority(ComponentIdentifiers, SigParams, Key) ->
    #{
        component_identifiers => ComponentIdentifiers,
        sig_params => SigParams,
        key => Key
    }.

sign(Authority, Req) ->
    sign(Authority, Req, #{}).
sign(Authority, Req, Res) ->
	ComponentIdentifiers = maps:get(component_identifiers, Authority),
	SignatureComponentsLine = signature_components_line(ComponentIdentifiers, Req, Res),
	SignatureParamsLine = signature_params_line(ComponentIdentifiers, maps:get(sig_params, Authority)),
	SignatureBase =
		<<SignatureComponentsLine/binary, <<"\n">>/binary, <<"\"@signature-params\": ">>/binary, SignatureParamsLine/binary>>,
	Name = random_an_binary(5),
	SignatureInput = SignatureParamsLine,
	% Create signature using SignatureBase and authority#key
	Signature = create_signature(Authority, SignatureBase),
	{ok, {Name, SignatureInput, Signature}}.

create_signature(Authority, SignatureBase) ->
    Key = maps:get(key, Authority),
    % TODO: implement
    Signature = <<"SIGNED", SignatureBase/binary>>,
    Signature.

%%%
%%% TODO: Ensure error cases are covered
%%% - dup CI: https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.2.5.2.1
%%%
signature_components_line(ComponentIdentifiers, Req, Res) ->
	ComponentsLine = lists:foldl(
		fun(Line, Identifier) ->
			% TODO: handle errors?
			{ok, {I, V}} = identifier_to_component(Identifier, Req, Res),
			% https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.2.1
			<<Line/binary, I/binary, <<": ">>/binary, V/binary, <<"\n">>/binary>>
		end,
		<<>>,
		ComponentIdentifiers
	),
	bin(ComponentsLine).

%%%
%%% @doc construct the signature-params line part of the signature base.
%%%
%%% ComponentIdentifiers: a list of "component identifiers" to be included
%%% in the signature.
%%%
%%% SigParams: a map or list of pairs that contain the metadata parameters
%%% for the signature
%%%
%%% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4
%%%
signature_params_line(ComponentIdentifiers, SigParams) when is_map(SigParams) ->
    signature_params_line(ComponentIdentifiers, maps:to_list(SigParams));
signature_params_line(ComponentIdentifiers, SigParams) when is_list(SigParams) ->
    SfList = [
        {
            list,
            lists:map(fun sf_item/1, ComponentIdentifiers),
            lists:map(
                fun
                    ({K, V}) when is_integer(V) -> {bin(K), V};
                    ({K, V}) -> {bin(K), {string, bin(V)}}
                end,
                SigParams
            )
        }
    ],
    Res = hb_http_structured_fields:list(SfList),
    bin(Res).

identifier_to_component(Identifier = <<"@", _R/bits>>, Req, Res) -> derive_component(Identifier, Req, Res);
identifier_to_component(Identifier, Req, Res) -> extract_field(Identifier, Req, Res).

extract_field(Identifier, Req, Res) when map_size(Res) == 0 ->
	extract_field(Identifier, Req, Res, req);
extract_field(Identifier, Req, Res) ->
	extract_field(Identifier, Req, Res, res).
extract_field(Identifier, Req, Res, Subject) when is_list(Identifier) ->
	extract_field(list_to_binary(Identifier), Req, Res, Subject);
extract_field(Identifier, Req, Res, Subject) when is_atom(Identifier) ->
	extract_field(atom_to_binary(Identifier), Req, Res, Subject);
extract_field(Identifier, Req, Res, _Subject) ->
	% The Identifier may have params and so we need to parse it
	% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-6
	{item, {_Kind, IParsed}, IParams} = sf_item(Identifier),
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
				% Fields are case-insensitive, so we perform a case-insensitive search across the Msg fields
				[
					{lower_bin(Key), Value}
				 || {Key, Value} <- maps:to_list(
						maps:get(
							% The field will almost certainly be a header, but could also be a trailer
							% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-18.10.1
							case IsTrailerField of
								true -> trailers;
								false -> headers
							end,
							% The header may exist on any message in the context of the signature
							% which could be the Request or Response Message
							% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1-18.8.1
							case IsRequestIdentifier of
								true -> Req;
								false -> Res
							end#{}
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

extract_field_value(RawFields, [Key, IsStrictFormat, IsByteSequenceEncoded]) ->
	% TODO: (maybe this already works?) empty string for empty header
	HasKey =
		case Key of
			false -> false;
			_ -> true
		end,
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
					sf_serialize(SfList);
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
								false -> sf_serialize(SF);
								_ -> extract_dictionary_field_value(SF, Key)
							end
					end
			end
	end.

extract_dictionary_field_value(StructuredField = [Elem | _Rest], Key) ->
	case Elem of
		{Name, _} when is_binary(Name) ->
			case lists:keyfind(Key, 1, StructuredField) of
				% https://datatracker.ietf.org/doc/html/rfc9421#section-2.1.2-5
				false ->
					{sf_dicionary_key_not_found_error,
						<<"Component Identifier references key not found in dictionary structured field">>};
				{_, Value} ->
					sf_serialize(Value)
			end;
		_ ->
			{sf_not_dictionary_error, <<"Component Identifier cannot reference key on a non-dictionary structured field">>}
	end.

derive_component(Identifier, Req, Res) when map_size(Res) == 0 ->
	derive_component(Identifier, Req, Res, req);
derive_component(Identifier, Req, Res) ->
    derive_component(Identifier, Req, Res, res).
derive_component(Identifier, Req, Res, Subject) when is_list(Identifier) ->
    derive_component(list_to_binary(Identifier), Req, Res, Subject);
derive_component(Identifier, Req, Res, Subject) when is_atom(Identifier) ->
    derive_component(atom_to_binary(Identifier), Req, Res, Subject);
derive_component(Identifier, Req, Res, Subject) when is_binary(Identifier) ->
	% The Identifier may have params and so we need to parse it
	% See https://datatracker.ietf.org/doc/html/rfc9421#section-2.2-6
	{item, {_Kind, IParsed}, IParams} = sf_item(Identifier),
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

sf_serialize(StructuredField = {list, _, _}) ->
	% The value is an inner_list, and so needs to be wrapped with an outer list
	% before being serialized
	sf_serialize(fun hb_http_structured_fields:list/1, [StructuredField]);
sf_serialize(StructuredField = {item, _, _}) ->
	sf_serialize(fun hb_http_structured_fields:item/1, StructuredField);
sf_serialize(StructuredField = [Elem | _Rest]) ->
	sf_serialize(
		% Both an sf list and dictionary is represented in Erlang as a List of pairs
		% but a dictionary's members will always be a pair whose first value
		% is a binary, so we can match on that to determine which serializer to use
		case Elem of
			{Name, _} when is_binary(Name) -> fun hb_http_structured_fields:dictionary/1;
			_ -> fun hb_http_structured_fields:list/1
		end,
		StructuredField
	).
sf_serialize(Serializer, StructuredField) ->
	case catch Serializer(StructuredField) of
		{'EXIT', _} -> {error, <<"Could not serialize into structured field">>};
		Parsed -> {ok, Parsed}
	end.

sf_item(SfItem = {item, {_Kind, _Parsed}, _Params}) ->
	SfItem;
% TODO: should we check whether the string is already quoted?
sf_item(ComponentIdentifier) when is_list(ComponentIdentifier) ->
    sf_item(<<$", (lower_bin(ComponentIdentifier))/binary, $">>);
sf_item(ComponentIdentifier) when is_binary(ComponentIdentifier) ->
    sf_item(hb_http_structured_fields:parse_item(ComponentIdentifier)).

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

random_an_binary(Length) ->
	Characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
	ListLength = length(Characters),
	RandomIndexes = [rand:uniform(ListLength) || _ <- lists:seq(1, Length)],
	RandomChars = [lists:nth(Index, Characters) || Index <- RandomIndexes],
	list_to_binary(RandomChars).

trim_ws(<<$\s, Bin/bits>>) -> trim_ws(Bin);
trim_ws(Bin) -> trim_ws_end(Bin, byte_size(Bin) - 1).

trim_ws_end(_, -1) ->
	<<>>;
trim_ws_end(Value, N) ->
	case binary:at(Value, N) of
		$\s ->
			trim_ws_end(Value, N - 1);
		_ ->
			S = N + 1,
			<<Value2:S/binary, _/bits>> = Value,
			Value2
	end.

%%%
%%% TESTS
%%%

sign_test() ->
	ok.

signature_components_line_test() ->
	ok.

signature_params_line_test() ->
	Params = #{created => 1733165109501, nonce => "foobar", keyid => "key1"},
	ContentIdentifiers = ["Content-Length", <<"\"@method\"">>, "@Path", "content-type;req", "example-dict;sf"],
	Result = signature_params_line(ContentIdentifiers, Params),
	?assertEqual(
		<<"(\"content-length\" \"@method\" \"@path\" \"content-type;req\" \"example-dict;sf\");created=1733165109501;nonce=\"foobar\";keyid=\"key1\"">>,
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
	{ok, {<<"\"foo\";req">>, <<"req-b-bar">>}} = extract_field("\"foo\";req", Req, Res),

	% req trailer + atom key + binary value
	{ok, {<<"\"another\";req;tr">>, <<"req-tr-atom-one">>}} = extract_field("\"another\";req;tr", Req, Res),

	% res header + list key + list value
	{ok, {<<"\"fizz\"">>, <<"res-l-bar">>}} = extract_field("\"fizz\"", Req, Res),

	% res trailer + binary uppercase key + binary value
	{ok, {<<"\"woo\";tr">>, <<"res-tr-uppercase-woo">>}} = extract_field("\"woo\";tr", Req, Res),

	% multiple fields, with obs and newlines
	{ok, {<<"\"a-field\"">>, <<"first one, second">>}} = extract_field("\"a-field\"", Req, Res).

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

	{ok, {<<"\"foo\";bs">>, <<":Zm9vYmFy:">>}} = extract_field("\"foo\";bs", Req, Res),

	{ok, {<<"\"a-field\";bs">>, <<":Zmlyc3Q=:, :c2Vjb25k:">>}} = extract_field("\"a-field\";bs", Req, Res),

	{ok, {<<"\"b-field\";bs">>, <<":Zmlyc3QsIHNlY29uZA==:">>}} = extract_field("\"b-field\";bs", Req, Res).

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
	{ok, {<<"\"foo\"">>, <<"a=1, b=2;x=1;y=2, c=(a b   c), d">>}} = extract_field("\"foo\"", Req, Res),

	{ok, {<<"\"foo\";sf">>, <<"a=1, b=2;x=1;y=2, c=(a b c), d=?1">>}} = extract_field("\"foo\";sf", Req, Res),

	{ok, {<<"\"foo\";key=\"a\"">>, <<"1">>}} = extract_field("\"foo\";key=\"a\"", Req, Res),
	% inner-list
	{ok, {<<"\"foo\";key=\"c\"">>, <<"(a b c)">>}} = extract_field("\"foo\";key=\"c\"", Req, Res),
	% boolean
	{ok, {<<"\"foo\";key=\"d\"">>, <<"?1">>}} = extract_field("\"foo\";key=\"d\"", Req, Res),
	% params
	{ok, {<<"\"foo\";key=\"b\"">>, <<"2;x=1;y=2">>}} = extract_field("\"foo\";key=\"b\"", Req, Res).

extract_field_error_conflicting_params_test() ->
	{conflicting_params_error, _} = extract_field("\"foobar\";bs;sf", #{}, #{}),
	{conflicting_params_error, _} = extract_field("\"foobar\";bs;key=\"foo\"", #{}, #{}).

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
	% req headers
	{field_not_found_error, _} = extract_field("\"not-foo\";req", Req, Res),
	% req trailers
	{field_not_found_error, _} = extract_field("\"not-foo\";req;tr", Req, Res),
	% res headers
	{field_not_found_error, _} = extract_field("\"not-foo\"", Req, Res),
	% res trailers
	{field_not_found_error, _} = extract_field("\"not-foo\";tr", Req, Res).

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
	{sf_not_dictionary_error, _M} = extract_field("\"foo\";req;key=\"smth\"", Req, Res).

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
	{sf_dicionary_key_not_found_error, _M} = extract_field("\"foo\";req;key=\"smth\"", Req, Res).

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
	{ok, {<<"\"@method\"">>, <<"GET">>}} = derive_component("\"@method\"", Req, Res),

	{ok, {<<"\"@target-uri\"">>, Url}} = derive_component("\"@target-uri\"", Req, Res),

	{ok, {<<"\"@authority\"">>, <<"foo.bar">>}} = derive_component("\"@authority\"", Req, Res),

	{ok, {<<"\"@scheme\"">>, <<"https">>}} = derive_component("\"@scheme\"", Req, Res),

	{ok, {<<"\"@request-target\"">>, <<"/id-123/Data?another=one&fizz=buzz">>}} = derive_component(
		"\"@request-target\"", Req, Res
	),

	% absolute form
	{ok, {<<"\"@request-target\"">>, Url}} = derive_component(
		"\"@request-target\"", maps:merge(Req, #{is_absolute_form => true}), Res
	),

	{ok, {<<"\"@path\"">>, <<"/id-123/Data">>}} = derive_component("\"@path\"", Req, Res),

	{ok, {<<"\"@query\"">>, <<"another=one&fizz=buzz">>}} = derive_component("\"@query\"", Req, Res),

	% no query params
	{ok, {<<"\"@query\"">>, <<"?">>}} = derive_component(
		"\"@query\"", maps:merge(Req, #{url => <<"https://foo.bar/id-123/Data">>}), Res
	),

	% empty query params
	{ok, {<<"\"@query\"">>, <<"?">>}} = derive_component(
		"\"@query\"", maps:merge(Req, #{url => <<"https://foo.bar/id-123/Data?">>}), Res
	),

	{ok, {<<"\"@query-param\";name=\"fizz\"">>, <<"buzz">>}} = derive_component(
		<<"\"@query-param\";name=\"fizz\"">>, Req, Res
	),

	% normalize identifier (lowercase) + @status
	{ok, {<<"\"@status\"">>, 202}} = derive_component("\"@Status\"", Req, Res),
	ok.

derive_component_error_req_param_on_request_target_test() ->
	Result = derive_component("\"@query-param\";req", #{}, #{}, req),
	{req_identifier_error, <<"A Component Identifier may not contain a req parameter if the target is a request message">>} =
		Result.

derive_component_error_query_param_no_name_test() ->
	Result = derive_component("\"@query-param\";noname=foo", #{}, #{}, req),
	{req_identifier_error, <<"@query_param Derived Component Identifier must specify a name parameter">>} = Result.

derive_component_error_status_req_target_test() ->
	Result = derive_component("\"@status\"", #{}, #{}, req),
	{res_identifier_error, _M} = Result.
