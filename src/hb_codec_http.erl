
%%% @doc A codec for the that marshals TABM encoded messages to and from the
%%% "HTTP" message structure.
%%% 
%%% The HTTP Message is an Erlang Map with the following shape:
%%% #{ 
%%%     headers => [
%%%         {<<"Example-Header">>, <<"Value">>}
%%%     ],
%%%     body: <<"Some body">>
%%% }
%%% 
%%% Every HTTP message is an HTTP multipart message.
%%% See https://datatracker.ietf.org/doc/html/rfc7578
%%%
%%% For each TABM Key:
%%% 
%%% The TABM Key will be ignored if:
%%% - The field is private (according to hb_private:is_private/1)
%%% - The field is one of ?REGEN_KEYS
%%%
%%% The Key/Value Pair will be encoded according to the following rules:
%%%     "signatures" -> {SignatureInput, Signature} header Tuples, each encoded
%%% 					as a Structured Field Dictionary
%%%     "body" ->
%%%         - if a map, then recursively encode as its own HyperBEAM message
%%%         - otherwise encode as a normal field
%%%     _ -> encode as a normal field
%%% 
%%% Each field will be mapped to the HTTP Message according to the following 
%%% rules:
%%%     "body" -> always encoded part of the body as with Content-Disposition
%%% 			  type of "inline"
%%%     _ ->
%%%         - If the byte size of the value is less than the ?MAX_TAG_VALUE,
%%% 		  then encode as a header, also attempting to encode as a
%%% 		  structured field.
%%%         - Otherwise encode the value as a part in the multipart response
%%% 
-module(hb_codec_http).
-export([to/1, from/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

% The max header length is 4KB
-define(MAX_HEADER_LENGTH, 4096).
% https://datatracker.ietf.org/doc/html/rfc7231#section-3.1.1.4
-define(CRLF, <<"\r\n">>).
-define(DOUBLE_CRLF, <<?CRLF/binary, ?CRLF/binary>>).

%%% @doc Convert an HTTP Message into a TABM.
%%% HTTP Structured Field is encoded into it's equivalent TABM encoding.
from(Bin) when is_binary(Bin) -> Bin;
from(#{ headers := Headers, body := Body }) when is_map(Headers) ->
    from(#{ headers => maps:to_list(Headers), body => Body });
from(#{ headers := Headers, body := Body }) ->
    % First, parse all headers and add as key-value pairs to the TABM
    Map = from_headers(#{}, Headers),
    % Next, we need to potentially parse the body and add to the TABM
    % potentially as additional key-binary value pairs, or as sub-TABMs
    {_, ContentType} = find_header(Headers, <<"Content-Type">>),
    Map1 = from_body(Map, ContentType, Body),
    Map2 = maps:remove(<<"Content-Type">>, Map1),
    hb_codec_converge:from(Map2).

from_headers(Map, Headers) -> from_headers(Map, Headers, Headers).
from_headers(Map, [], _) -> Map;
from_headers(Map, [{Name, Value} | Rest], Headers) ->
    NewMap = case Name of
        % Handled as part of "Signature" so simply skip it
        <<"Signature-Input">> -> Map;
        <<"signature-input">> -> Map;

        <<"Signature">> ->
            {_, SigInput} = find_header(Headers, <<"Signature-Input">>),
            from_signature(Map, Value, SigInput);
        <<"signature">> ->
            {_, SigInput} = find_header(Headers, <<"Signature-Input">>),
            from_signature(Map, Value, SigInput);
        % Decode the header as normal
        N -> from_pair(Map, {N, Value})
    end,
    from_headers(NewMap, Rest, Headers).

%%% @doc attempt to parse the value as a structured field
%%% First as an SF Item,
%%% then an SF Dictionary,
%%% then an SF List.
%%%
%%% If parsing is unsuccessful, simply use the raw value
from_pair(Map, {Name, Value}) when is_binary(Value) ->
    Parsed = case to_sf(Value) of
        % item()
        {item, Item} ->
            % TODO: reserialize back to item?
            from_sf_item(Item);
         % [{binary(), item()}]
        {dict, Pairs} ->
            % erlang:display({dDict, Pairs}),
            Map_Pairs = lists:foldl(
                fun ({MapKey, Item}, Acc) -> [{MapKey, from_sf_item(Item)} | Acc] end,
                [],
                Pairs
            ),
            maps:from_list(Map_Pairs);
        % [item()]
        {list, List} ->
            ?no_prod("Is this proper way to Map encode a list?"),
            % erlang:display({dList, List}),
            lists:map(
                fun
                    ({item, BareItem, []}) -> from_sf_bare_item(BareItem);
                    % Re-encode to preserve original binary
                    (Item) -> iolist_to_binary( hb_http_structured_fields:item(Item))
                end,
                List
            );
        % Not able to parse into an SF, so just set the key to the binary
        _ ->
            % erlang:display({dBinary, Value}),
            dequote(Value)
    end,
    maps:put(Name, Parsed, Map).

to_sf(Raw) when is_binary(Raw) ->
	Parsers = [
		{item, fun hb_http_structured_fields:parse_item/1},
		{dict, fun hb_http_structured_fields:parse_dictionary/1},
		{list, fun hb_http_structured_fields:parse_list/1}
	],
	to_sf(Parsers, Raw).
to_sf([], _Raw) ->
    {error, undefined};
to_sf([{Type, Parser} | Rest], Raw) ->
    case catch Parser(Raw) of
        % skip parsers that fail
        {'EXIT', _} -> to_sf(Rest, Raw);
        Parsed -> {Type, Parsed}
    end.
    

from_sf_item(Item = {item, _BareItem, _Params}) -> 
    case Item of
        {item, BareItem, []} -> from_sf_bare_item(BareItem);
        {item, _, _} ->
            ?no_prod("How do we handle SF Params?"),
            % Re-encode to preserve original binary
            iolist_to_binary(hb_http_structured_fields:item(Item))
    end.

from_sf_bare_item (BareItem) ->
    case BareItem of
        I when is_integer(I) -> I;
        B when is_boolean(B) -> B;
        D = {decimal, _} -> list_to_float(hb_http_structured_fields:bare_item(D));
        {string, S} -> S;
        {token, T} -> binary_to_existing_atom(T);
        {binary, B} -> B
    end.

from_signature(Map, RawSig, RawSigInput) ->
    SfSigs = hb_http_structured_fields:parse_dictionary(RawSig),
    SfInputs = hb_http_structured_fields:parse_dictionary(RawSigInput),
    % Build a Map for Signatures by gathering each Signature
    % with its corresponding Inputs.
    % 
    % Inputs are merged as fields on the Signature Map
    Signatures = maps:fold(
        fun (SigName, {item, {_, Sig}, _}, Sigs) ->
            {list, SfInputItems, SfInputParams} = lists:keyfind(SigName, 1, SfInputs),
            % [<<"foo">>, <<"bar">>]
            Inputs = lists:map(fun({item, {_, Input}, _}) -> Input end, SfInputItems),

            SigMap = lists:foldl(
                fun({PName, PBareItem}, PAcc) ->
                    PValue = from_sf_bare_item(PBareItem),
                    maps:put(PName, PValue, PAcc)
                end,
                #{ <<"signature">> => Sig, <<"inputs">> => Inputs },
                 % Signature parameters are converted into top-level keys on the signature Map
                SfInputParams    
            ),
            % #{ [SigName/binary] => #{ <<"signature">> => <<>>, <<"inputs">> => , ... } }
            maps:put(SigName, SigMap, Sigs)
        end,
        #{},
        SfSigs
    ),
    % Finally place the Signatures as a top-level Map on the parent Map
    maps:put(<<"Signatures">>, Signatures, Map).

find_header(Headers, Name) ->
    find_header(Headers, Name, []).
find_header(Headers, Name, Opts) when is_list(Headers) ->
    Matcher = case lists:member(strict, Opts) of
        true -> fun ({N, _Value}) -> N =:= Name end;
        _ -> fun ({N, _Value}) -> hb_util:to_lower(N) =:= hb_util:to_lower(Name) end
    end,
    case lists:filter(Matcher, Headers) of
        [] -> {undefined, undefined};
        Found -> case lists:member(global, Opts) of
            true -> Found;
            _ ->
                [First | _] = Found,
                First
        end
    end.

dequote(Bin) when is_binary(Bin) ->
    Rest = case Bin of
        <<"\"", R/binary>> -> R;
        B -> B
    end,
    Str = binary_to_list(Rest),
    Trimmed = case lists:suffix("\"", Str) of
        true ->
            [_ | Trim] = lists:reverse(Str),
            list_to_binary(lists:reverse(Trim));
        _ -> Rest
    end,
    Trimmed.

from_body(TABM, _ContentType, <<>>) -> TABM;
from_body(TABM, ContentType, Body) ->
    {item, {_, _BodyType}, Params} = hb_http_structured_fields:parse_item(ContentType),
    case lists:keyfind(<<"boundary">>, 1, Params) of
        % The body is not a multipart, so just set as is to the Body key on the TABM
        false ->
            from_pair(TABM, {<<"Body">>, Body});
        % We need to manually parse the multipart body into key/values on the TABM
        {_, {_Type, Boundary}} ->
            % Find the sub-part of the body within the boundary
            BegPat = <<"--", Boundary/binary>>,
            EndPat = <<"--", Boundary/binary, "--">>,
            {Start, SL} = binary:match(Body, BegPat),
            {End, _} = binary:match(Body, EndPat),
            BodyPart = binary:part(Body, Start + SL, End - (Start + SL)),

            Parts = binary:split(BodyPart, [<<"--", Boundary/binary>>], [global]),
            % Finally, for each body part, we need to parse it into its
            % own HTTP Message, then recursively convert into a TABM
            TABM1 = lists:foldl(
                fun
                    (Part, CurTABM) ->
                    {ok, NewTABM} = append_body_part(CurTABM, Part),
                    NewTABM 
                end,
                TABM,
                Parts
            ),
            TABM1
    end.

append_body_part(TABM, Part) ->
    % Extract the Headers block and Body. Only split on the FIRST double CRLF
    [RawHeadersBlock, RawBody] = case binary:split(Part, [?DOUBLE_CRLF], []) of
        % no body
        [RHB] -> [RHB, <<>>];
        [RHB, RB] -> [RHB, RB]
    end,
    % Extract individual headers
    RawHeaders = binary:split(RawHeadersBlock, ?CRLF, [global]),
    % Now we parse each header, splitting into {Key, Value}
    Headers = lists:filtermap(
        fun
            % Skip empty headers that are missing in splitting
            (<<>>) -> false;
            (RawHeader) -> 
                case binary:split(RawHeader, [<<": ">>]) of
                    [Name, Value] -> {true, {Name, Value}};
                    % skip lines that aren't properly formatted headers
                    _ -> false
                end
        end,
        RawHeaders
    ),
    % The Content-Disposition is from the parent message,
    % so we separate off from the rest of the headers
    {AllContentDisposition, RestHeaders} = lists:partition(
        fun
            ({<<"Content-Disposition">>, _}) -> true;
            ({<<"content-disposition">>, _}) -> true;
            (_) -> false
        end,
        Headers    
    ),
    ContentDisposition = case AllContentDisposition of
        [] -> undefined;
        [{_, CD} | _Rest] -> CD
    end,
    case ContentDisposition of
        undefined -> no_content_disposition_header_found;
        RawDisposition when is_binary(RawDisposition) ->
            {item, {_, _Disposition}, Params} = hb_http_structured_fields:parse_item(RawDisposition),
            PartName = case lists:keyfind(<<"name">>, 1, Params) of
                false -> <<"Body">>;
                {_, {_type, PN}} -> PN
            end,
            SubTABM = from(#{ headers => RestHeaders, body => RawBody }),
            {ok, maps:put(PartName, SubTABM, TABM)}
    end.

%%% @doc Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map
%%% that can translated to a given web server Response API
to(Bin) when is_binary(Bin) -> Bin;
to(TABM) when is_map(TABM) ->
    % PublicMsg = hb_private:reset(TABM),
    % MinimizedMsg = hb_message:minimize(PublicMsg),
    Map = hb_codec_converge:to(TABM),
    Http = maps:fold(
        fun
            % Signatures (note abbr. & case-insensitivity) are mapped according to RFC-9421
            (<<"signatures">>, Signatures, Http) -> signatures_to_http(Http, Signatures);
            (<<"Signatures">>, Signatures, Http) -> signatures_to_http(Http, Signatures);
            (<<"signature">>, Signature, Http) -> signatures_to_http(Http, [Signature]);
            (<<"Signature">>, Signature, Http) -> signatures_to_http(Http, [Signature]);

            % Body (note case-insensitivity) is mapped into a multipart according to RFC-7578
            (<<"body">>, Body, Http) -> body_to_http(Http, Body);
            (<<"Body">>, Body, Http) -> body_to_http(Http, Body);

            % All other values are encoded as an HTTP Structured Fields.
            % non-map/list is encoded as an HTTP Structured Field Item
            (Name, Value, Http) when not (is_map(Value) orelse is_list(Value)) ->
                {ok, Item} = hb_http_structured_fields:to_item(Value),
                field_to_http(Http, {Name, iolist_to_binary(hb_http_structured_fields:item(Item))}, #{});
            % Further mapping of lists and maps delegated to field_to_http
            (Name, Value, Http) -> field_to_http(Http, {Name, Value}, #{})
        end,
        #{ headers => [], body => #{} },
        Map
    ),
    Body = maps:get(body, Http),
    NewHttp = case maps:size(Body) of
        0 -> maps:put(body, <<>>, Http);
        _ ->
            ?no_prod("What should the Boundary be?"),
            Boundary = base64:encode(crypto:strong_rand_bytes(8)),
            % Transform body into a binary, delimiting each part with the Boundary
            BodyList = maps:fold(
                fun (_, BodyPart, Acc) ->
                    [<<"--", Boundary/binary, ?CRLF/binary, BodyPart/binary>> | Acc]
                end,
                [],
                Body
            ),
            BodyBin = iolist_to_binary(lists:join(?CRLF, lists:reverse(BodyList))),
            #{ 
                headers => [
                    {
						<<"Content-Type">>,
						<<"multipart/form-data; boundary=", "\"" , Boundary/binary, "\"">>
					}
                    | maps:get(headers, Http)
                ],
                % End the body with a final terminating Boundary
                body => <<BodyBin/binary, ?CRLF/binary, "--", Boundary/binary, "--">>
            }
    end,
    NewHttp.

encode_http_msg (_Http = #{ headers := SubHeaders, body := SubBody }) ->
    % Serialize the headers, to be included in the part of the multipart response
    HeaderList = lists:foldl(
        fun ({HeaderName, HeaderValue}, Acc) ->
            [<<HeaderName/binary, ": ", HeaderValue/binary>> | Acc]
        end,
        [],
        SubHeaders
    ),
    EncodedHeaders = iolist_to_binary(lists:join(?CRLF, lists:reverse(HeaderList))),
    case SubBody of
        <<>> -> EncodedHeaders;
        % Some-Headers: some-value
        % Content-Type: image/png
        % 
        % <body>
        _ -> <<EncodedHeaders/binary, ?DOUBLE_CRLF/binary, SubBody/binary>>
    end.

signatures_to_http(Http, Signatures) when is_map(Signatures) ->
    signatures_to_http(Http, maps:to_list(Signatures));
signatures_to_http(Http, Signatures) when is_list(Signatures) ->
    {SfSigInputs, SfSigs} = lists:foldl(
        fun ({SigName, SignatureMap = #{ <<"inputs">> := Inputs, <<"signature">> := Signature }}, {SfSigInputs, SfSigs}) ->
            NextSigInput = hb_http_signature:sf_signature_params(Inputs, SignatureMap),
            NextSig = hb_http_signature:sf_signature(Signature),
            NextName = hb_converge:key_to_binary(SigName),
            {
                [{NextName, NextSigInput} | SfSigInputs],
                [{NextName, NextSig} | SfSigs]
            }
        end,
        % Start with empty Structured Field Dictionaries
        {[], []},
        Signatures
    ),
    % Signature and Signature-Input are always encoded as Structured Field dictionaries, and then
    % each transmitted either as a header, or as a part in the multi-part body
    WithSig = field_to_http(Http, {<<"Signature">>, hb_http_structured_fields:dictionary(SfSigs)}, #{}),
    WithSigAndInput = field_to_http(WithSig, {<<"Signature-Input">>, hb_http_structured_fields:dictionary(SfSigInputs)}, #{}),
    WithSigAndInput.

body_to_http(Http, Body) when is_map(Body) ->
    Disposition = <<"Content-Disposition: inline">>,
    SubHttp = to(Body),
    EncodedBody = encode_http_msg(SubHttp),
    field_to_http(Http, {<<"body">>, EncodedBody}, #{ disposition => Disposition, where => body });
body_to_http(Http, Body) when is_binary(Body) ->
    Disposition = <<"Content-Disposition: inline">>,
    field_to_http(Http, {<<"body">>, Body}, #{ disposition => Disposition, where => body }).

field_to_http(Http, {Name, Map}, Opts) when is_map(Map) ->
    MaybeEncoded = case catch hb_http_structured_fields:to_dictionary(Map) of
        {'EXIT', _} ->
            {false, undefined};
        {ok, Sf} ->
            % Check the size of the encoded value, and signal to store
            % as a Structured Field in the header
            %
            % Otherwise, we will need to convert the Map into
            % its own HTTP message and then append as a part of the parent's multi-part body
            % TODO: haven't been able to figure out how to distinguish bonafide structured fields from messages
            % so skipping this optimization
            % EncodedSf = iolist_to_binary(hb_http_structured_fields:dictionary(Sf)),
            % Fits = byte_size(EncodedSf) =< ?MAX_HEADER_LENGTH,
            % {Fits, EncodedSf};
            {false, undefined};
        _ ->
            {false, undefined}
    end,
    NormalizedName = hb_converge:key_to_binary(Name),
    case MaybeEncoded of
        {true, Encoded} ->
            field_to_http(Http, {NormalizedName, Encoded}, Opts);
        {false, _} ->
            SubHttp = to(Map),
            EncodedHttpMap = encode_http_msg(SubHttp),
            field_to_http(Http, {Name, EncodedHttpMap}, maps:put(where, body, Opts))
    end;
field_to_http(Http, {Name, List}, Opts) when is_list(List) ->
    MaybeEncoded = case catch hb_http_structured_fields:to_list(List) of
        {'EXIT', _} ->
            {false, undefined};
        {ok, Sf} ->
            % Check the size of the encoded value, and signal to store
            % as a Structured Field in the header
            %
            % Otherwise, we can still use the Structured field encoding,
            % but the value is appended as a part of the parent's multi-part body
            EncodedSf = iolist_to_binary(hb_http_structured_fields:list(Sf)),
            Fits = byte_size(EncodedSf) =< ?MAX_HEADER_LENGTH,
            {Fits, EncodedSf};
        _ ->
            {false, undefined}
    end,
    NormalizedName = hb_converge:key_to_binary(Name),
    case MaybeEncoded of
        {true, Encoded} ->
            field_to_http(Http, {NormalizedName, Encoded}, Opts);
        % Encode the SF list as a sub part, to be appended to the body
        {false, Encoded} when is_binary(Encoded) ->
            field_to_http(Http, {NormalizedName, Encoded}, maps:put(where, body, Opts));
        {false, _} ->
            ?no_prod("how do we encode a list in HTTP message if it cannot be encoded as a structured field?"),
            not_implemented
    end;
field_to_http(Http, {Name, Value}, Opts) when is_binary(Value) ->
    NormalizedName = hb_converge:key_to_binary(Name),

    % The default location where the value is encoded within the HTTP
    % message depends on its size.
    % 
    % So we check whether the size of the value is within the threshold
    % to encode as a header, and other default to encoding in the body
    DefaultWhere = case byte_size(Value) of
        Fits when Fits =< ?MAX_HEADER_LENGTH -> headers;
        _ -> maps:get(where, Opts, headers)
    end,

    case maps:get(where, Opts, DefaultWhere) of
        headers ->
            Headers = maps:get(headers, Http),
            NewHeaders = lists:append(Headers, [{NormalizedName, Value}]),
            maps:put(headers, NewHeaders, Http);
        % Append the value as a part of the multipart body
        %
        % We'll need to prepend a Content-Disposition header to the part, using
        % the field name as the form part name. (see https://www.rfc-editor.org/rfc/rfc7578#section-4.2).
        % We allow the caller to provide a Content-Disposition in Opts, but default
        % to appending as a field on the form-data
        body ->
            Body = maps:get(body, Http),
            Disposition = maps:get(disposition, Opts, <<"Content-Disposition: form-data;name=", NormalizedName/binary>>),
            BodyPart = <<Disposition/binary, ?CRLF/binary, Value/binary>>,
            NewBody = maps:put(NormalizedName, BodyPart, Body),
            maps:put(body, NewBody, Http)
    end.
