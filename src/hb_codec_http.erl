
%%% @doc A codec for the that marshals TABM encoded messages to and from the
%%% "HTTP" message structure.
%%% 
%%% The HTTP Message is an Erlang Map with the following shape:
%%% #{ 
%%%     headers => [
%%%         {<<"example-header">>, <<"value">>}
%%%     ],
%%%     body: <<"some body">>
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

%% @doc Convert an HTTP Message into a TABM.
%% HTTP Structured Field is encoded into it's equivalent TABM encoding.
from(Bin) when is_binary(Bin) -> Bin;
from(#{ <<"headers">> := Headers, <<"body">> := Body }) when is_map(Headers) ->
    from(#{ <<"headers">> => maps:to_list(Headers), <<"body">> => Body });
from(#{ <<"headers">> := Headers, <<"body">> := Body }) ->
    % First, parse all headers and add as key-value pairs to the TABM
    Map = from_headers(#{}, Headers),
    % Next, we need to potentially parse the body and add to the TABM
    % potentially as additional key-binary value pairs, or as sub-TABMs
    ContentType =
        case find_header(Headers, <<"content-type">>) of
            {undefined, undefined} -> undefined;
            {_, CT} -> CT
        end,
    maps:remove(<<"content-type">>, from_body(Map, ContentType, Body)).

from_headers(Map, Headers) -> from_headers(Map, Headers, Headers).
from_headers(Map, [], _) -> Map;
from_headers(Map, [{Name, Value} | Rest], Headers) ->
    NewMap = case Name of
        % Handled as part of "Signature" so simply skip it
        <<"signature-input">> -> Map;
        <<"signature">> ->
            {_, SigInput} = find_header(Headers, <<"signature-input">>),
            from_signature(Map, Value, SigInput);
        % Decode the header as normal
        N -> maps:put(N, Value, Map)
    end,
    from_headers(NewMap, Rest, Headers).

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
                    maps:put(PName, from_sf_bare_item(PBareItem), PAcc)
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
    maps:put(<<"signatures">>, Signatures, Map).

from_sf_bare_item (BareItem) ->
    case BareItem of
        I when is_integer(I) -> I;
        B when is_boolean(B) -> B;
        D = {decimal, _} -> list_to_float(hb_http_structured_fields:bare_item(D));
        {string, S} -> S;
        {token, T} -> binary_to_existing_atom(T);
        {binary, B} -> B
    end.

find_header(Headers, Name) ->
    find_header(Headers, Name, []).
find_header(Headers, Name, Opts) when is_list(Headers) ->
    Matcher =
        fun ({N, _Value}) ->
            hb_converge:normalize_key(N) =:= hb_converge:normalize_key(Name)
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

from_body(TABM, _ContentType, <<>>) -> TABM;
from_body(TABM, ContentType, Body) ->
    {BodyType, Params} =
        case ContentType of
            undefined -> {undefined, []};
            _ ->
                {item, {_, XT}, XParams} =
                    hb_http_structured_fields:parse_item(ContentType),
                {XT, XParams}
        end,
    case lists:keyfind(<<"boundary">>, 1, Params) of
        % The body is not a multipart, so just set as is to the Body key on the TABM
        false ->
            maps:put(<<"body">>, Body, TABM);
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
            ({Str, _}) -> hb_util:to_lower(Str) =:= <<"content-disposition">>;
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
                false -> <<"body">>;
                {_, {_type, PN}} -> PN
            end,
            SubTABM = from(#{ <<"headers">> => RestHeaders, <<"body">> => RawBody }),
            {ok, maps:put(PartName, SubTABM, TABM)}
    end.

%%% @doc Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map
%%% that can translated to a given web server Response API
to(Bin) when is_binary(Bin) -> Bin;
to(TABM) when is_map(TABM) ->
    % PublicMsg = hb_private:reset(TABM),
    % MinimizedMsg = hb_message:minimize(PublicMsg),
    Http = maps:fold(
        fun(RawKey, Value, Http) ->
            Key = hb_converge:normalize_key(RawKey),
            case hb_util:to_lower(Key) of
                <<"body">> -> body_to_http(Http, Value);
                <<"signature">> -> signatures_to_http(Http, [Value]);
                <<"signatures">> -> signatures_to_http(Http, Value);
                _ -> field_to_http(Http, {Key, Value}, #{})
            end
        end,
        #{ <<"headers">> => [], <<"body">> => #{} },
        TABM
    ),
    Body = maps:get(<<"body">>, Http),
    NewHttp =
        case Body of
            % If the body is empty, then we need to set it to an empty binary.
            X when map_size(X) =:= 0 -> maps:put(<<"body">>, <<>>, Http);
            % % If the body has one element with the key "body", then we need to
            % % set the body to the value of the "body" key.
            #{ <<"body">> := BodyValue } ->
                [_ContentDisposition, UserBody] = binary:split(BodyValue, ?CRLF, []),
                maps:put(<<"body">>, UserBody, Http);
            % Otherwise, we need to encode the body as a multipart message.
            _ ->
                {ok, RawBoundary} = dev_message:id(TABM),
                Boundary = hb_util:encode(RawBoundary),
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
                    <<"headers">> => [
                        {
                            <<"content-type">>,
                            <<"multipart/form-data; boundary=", "\"" , Boundary/binary, "\"">>
                        }
                        | maps:get(<<"headers">>, Http)
                    ],
                    % End the body with a final terminating Boundary
                    <<"body">> => <<BodyBin/binary, ?CRLF/binary, "--", Boundary/binary, "--">>
                }
        end,
    NewHttp.

encode_http_msg (_Http = #{ <<"headers">> := SubHeaders, <<"body">> := SubBody }) ->
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
        % content-type: image/png
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
            NextName = hb_converge:normalize_key(SigName),
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
    WithSig = field_to_http(Http, {<<"signature">>, hb_http_structured_fields:dictionary(SfSigs)}, #{}),
    WithSigAndInput = field_to_http(WithSig, {<<"signature-input">>, hb_http_structured_fields:dictionary(SfSigInputs)}, #{}),
    WithSigAndInput.

body_to_http(Http, Body) when is_map(Body) ->
    Disposition = <<"content-disposition: inline">>,
    SubHttp = to(Body),
    EncodedBody = encode_http_msg(SubHttp),
    field_to_http(Http, {<<"body">>, EncodedBody}, #{ disposition => Disposition, where => body });
body_to_http(Http, Body) when is_binary(Body) ->
    Disposition = <<"content-disposition: inline">>,
    field_to_http(Http, {<<"body">>, Body}, #{ disposition => Disposition, where => body }).

field_to_http(Http, {Name, Value}, Opts) when is_map(Value) ->
    SubHttp = to(Value),
    EncodedHttpMap = encode_http_msg(SubHttp),
    field_to_http(Http, {Name, EncodedHttpMap}, maps:put(where, body, Opts));
field_to_http(Http, {Name, Value}, Opts) when is_binary(Value) ->
    NormalizedName = hb_converge:normalize_key(Name),
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
            Headers = maps:get(<<"headers">>, Http),
            NewHeaders = lists:append(Headers, [{NormalizedName, Value}]),
            maps:put(<<"headers">>, NewHeaders, Http);
        % Append the value as a part of the multipart body
        %
        % We'll need to prepend a Content-Disposition header to the part, using
        % the field name as the form part name. (see https://www.rfc-editor.org/rfc/rfc7578#section-4.2).
        % We allow the caller to provide a Content-Disposition in Opts, but default
        % to appending as a field on the form-data
        body ->
            Body = maps:get(<<"body">>, Http),
            Disposition = maps:get(disposition, Opts, <<"content-disposition: form-data;name=", NormalizedName/binary>>),
            BodyPart = <<Disposition/binary, ?CRLF/binary, Value/binary>>,
            NewBody = maps:put(NormalizedName, BodyPart, Body),
            maps:put(<<"body">>, NewBody, Http)
    end.
