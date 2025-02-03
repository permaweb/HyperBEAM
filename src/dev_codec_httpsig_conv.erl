
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
-module(dev_codec_httpsig_conv).
-export([to/1, from/1]).
%%% Helper utilities
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
from(HTTP) ->
    Body = maps:get(<<"body">>, HTTP, <<>>),
    % First, parse all headers excluding the signature-related headers, as they
    % are handled separately.
    Headers = maps:without([<<"body">>], HTTP),
    ContentType = maps:get(<<"content-type">>, Headers, undefined),
    % We only should omit the content-type header if the content-type
    % happens to be the HyperBEAM native http encoding: multipart/form-data
    % (and is ergo not part of the message's contents, but HyperBEAM's encoding)
    %
    % Otherwise, the header ought not be omitted
    SigHeaderNames = case ContentType of
        <<"multipart/form-data", _/binary>> ->
            [<<"content-type">>, <<"signature">>, <<"signature-input">>, <<"attestations">>];
        _ ->
            [<<"signature">>, <<"signature-input">>, <<"attestations">>]
    end,
    % Next, we need to potentially parse the body and add to the TABM
    % potentially as sub-TABMs.
    MsgWithoutSigs = maps:without(SigHeaderNames, from_body(Headers, ContentType, Body)),
    ?event({from_body, {headers, Headers}, {body, Body}, {msgwithoutatts, MsgWithoutSigs}}),
    % Extract all hashpaths from the attestations of the message
    HPs = extract_hashpaths(HTTP),
    % Finally, we need to add the signatures to the TABM
    {ok, MsgWithSigs} = attestations_from_signature(
        maps:without(maps:keys(HPs), MsgWithoutSigs),
        HPs,
        maps:get(<<"signature">>, Headers, not_found),
        maps:get(<<"signature-input">>, Headers, not_found)
    ),
    ?event({message_with_atts, MsgWithSigs}),
    Res = maps:without(Removed = maps:keys(HPs), MsgWithSigs),
    ?event({message_without_atts, Res, Removed}),
    Res.

from_body(TABM, _ContentType, <<>>) -> TABM;
from_body(TABM, ContentType, Body) ->
    ?event({from_body, {from_headers, TABM}, {content_type, ContentType}, {body, Body}}),
    Params =
        case ContentType of
            undefined -> [];
            _ ->
                {item, {_, _XT}, XParams} =
                    dev_codec_structured_conv:parse_item(ContentType),
                XParams
        end,
    case lists:keyfind(<<"boundary">>, 1, Params) of
        false ->
            % The body is not a multipart, so just set as is to the body key on
            % the TABM.
            maps:put(<<"body">>, Body, TABM);
        {_, {_Type, Boundary}} ->
            % We need to manually parse the multipart body into key/values on the
            % TABM.
            % 
            % Find the sub-part of the body within the boundary.
            % We also make sure to account for the CRLF at end and beginning
            % of the starting and terminating part boundary, respectively
            % 
            % ie.
            % --foo-boundary\r\n
            % My-Awesome: Part
            %
            % an awesome body\r\n
            % --foo-boundary--
            BegPat = <<"--", Boundary/binary, ?CRLF/binary>>,
            EndPat = <<?CRLF/binary, "--", Boundary/binary, "--">>,
            {Start, SL} = binary:match(Body, BegPat),
            {End, _} = binary:match(Body, EndPat),
            BodyPart = binary:part(Body, Start + SL, End - (Start + SL)),
            % By taking into account all parts of the surrounding boundary above,
            % we get precisely the sub-part that we're interested without any
            % additional parsing
            Parts = binary:split(BodyPart, [<<?CRLF/binary, "--", Boundary/binary>>], [global]),
            % Finally, for each part within the sub-part, we need to parse it,
            % potentially recursively as a sub-TABM, and then add it to the
            % current TABM
            {ok, FlattendTABM} = from_body_parts(TABM, Parts),
            dev_codec_flat:from(FlattendTABM)
    end.

from_body_parts (TABM, []) -> {ok, TABM};
from_body_parts(TABM, [Part | Rest]) ->
    % Extract the Headers block and Body. Only split on the FIRST double CRLF
    [RawHeadersBlock, RawBody] =
        case binary:split(Part, [?DOUBLE_CRLF], []) of
            [RHB] ->
                % no body
                [RHB, <<>>];
            [RHB, RB] -> [RHB, RB]
        end,
    % Extract individual headers
    RawHeaders = binary:split(RawHeadersBlock, ?CRLF, [global]),
    % Now we parse each header, splitting into {Key, Value}
    Headers =
        maps:from_list(lists:filtermap(
            fun (<<>>) -> false;
                (RawHeader) -> 
                    case binary:split(RawHeader, [<<": ">>]) of
                        [Name, Value] -> {true, {Name, Value}};
                        _ ->
                            % skip lines that aren't properly formatted headers
                            false
                    end
            end,
            RawHeaders
        )),
    % The Content-Disposition is from the parent message,
    % so we separate off from the rest of the headers
    case maps:get(<<"content-disposition">>, Headers, undefined) of
        undefined ->
            % A Content-Disposition header is required for each part
            % in the multipart body
            throw({error, no_content_disposition_in_multipart, Headers});
        RawDisposition when is_binary(RawDisposition) ->
            % Extract the name 
            {item, {_, Disposition}, DispositionParams} =
                dev_codec_structured_conv:parse_item(RawDisposition),
            {ok, PartName} = case Disposition of
                <<"inline">> ->
                    % The inline part is the body
                    {ok, <<"body">>};
                _ ->
                    % Otherwise, we need to extract the name of the part
                    % from the Content-Disposition parameters
                    case lists:keyfind(<<"name">>, 1, DispositionParams) of
                        {_, {_type, PN}} -> {ok, PN};
                        false -> no_part_name_found
                    end
            end,
            RestHeaders = maps:without([<<"content-disposition">>], Headers),
            ParsedPart =
                case maps:size(RestHeaders) of
                    0 ->
                        % There are no headers besides the content disposition header
                        % So simply use the the raw body binary as the part
                        RawBody;
                    _ ->
                        % TODO: this branch may no longer be needed
                        % since we flatten the maps prior to HTTP encoding
                        % 
                        % For now, keeping recursive logic.
                        % We need to recursively parse the sub part into its own TABM
                        from(RestHeaders#{ <<"body">> => RawBody })
                end,
            from_body_parts(maps:put(PartName, ParsedPart, TABM), Rest)
    end.

%% @doc Populate the `/attestations` key on the TABM with the dictionary of 
%% signatures and their corresponding inputs.
attestations_from_signature(Map, _HPs, not_found, _RawSigInput) ->
    ?event({no_sigs_found_in_from, {msg, Map}}),
    {ok, maps:without([<<"attestations">>], Map)};
attestations_from_signature(Map, HPs, RawSig, RawSigInput) ->
    SfSigsKV = dev_codec_structured_conv:parse_dictionary(RawSig),
    SfInputs = maps:from_list(dev_codec_structured_conv:parse_dictionary(RawSigInput)),
    ?event({adding_sigs_and_inputs, {sigs, SfSigsKV}, {inputs, SfInputs}}),
    % Build a Map for Signatures by gathering each Signature
    % with its corresponding Inputs.
    % 
    % Inputs are merged as fields on the Signature Map
    Attestations = maps:from_list(lists:map(
        fun ({SigName, Signature}) ->
            ?event({adding_attestation, {sig, SigName}, {sig, Signature}, {inputs, SfInputs}}),
            {list, SigInputs, ParamsKVList} = maps:get(SigName, SfInputs, #{}),
            ?event({inputs, {signame, SigName}, {inputs, SigInputs}, {params, ParamsKVList}}),
            % Find all hashpaths from the signature and add them to the 
            % attestations message.
            Hashpath =
                lists:filtermap(
                    fun ({item, BareItem, _}) ->
                        case dev_codec_structured_conv:from_bare_item(BareItem) of
                            HP = <<"hash", _/binary>> -> {true, HP};
                            _ -> false
                        end;
                    (_) -> false
                end,
                SigInputs
            ),
            ?event({all_hashpaths, HPs}),
            Hashpaths = maps:from_list(lists:map(
                fun (HP) ->
                    {HP, maps:get(HP, HPs, <<>>)}
                end,
                Hashpath
            )),
            ?event({hashpaths, Hashpaths}),
            Params = maps:from_list(ParamsKVList),
            {string, EncPubKey} = maps:get(<<"keyid">>, Params),
            PubKey = hb_util:decode(EncPubKey),
            Address = hb_util:human_id(ar_wallet:to_address(PubKey)),
            ?event({calculated_name, {address, Address}, {sig, Signature}, {inputs, {explicit, SfInputs}, {implicit, Params}}}),
            SerializedSig = iolist_to_binary(
                dev_codec_structured_conv:dictionary(
                    #{ SigName => Signature }
                )
            ),
            {item, {binary, UnencodedSig}, _} = Signature,
            {
                Address,
                Hashpaths#{
                    <<"signature">> => SerializedSig,
                    <<"signature-input">> =>
                        iolist_to_binary(
                            dev_codec_structured_conv:dictionary(
                                #{ SigName => maps:get(SigName, SfInputs) }
                            )
                        ),
                    <<"id">> => crypto:hash(sha256, UnencodedSig),
                    <<"attestation-device">> => <<"httpsig@1.0">>
                }
            }
        end,
        SfSigsKV
    )),
    Msg = Map#{ <<"attestations">> => Attestations },
    ?event({adding_attestations, {msg, Msg}}),
    % Finally place the attestations as a top-level message on the parent message
    dev_codec_httpsig:reset_hmac(Msg).

%%% @doc Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map
%%% that can translated to a given web server Response API
to(Bin) when is_binary(Bin) -> Bin;
to(TABM) -> to(TABM, []).
to(TABM, Opts) when is_map(TABM) ->
    % Calculate the initial encoding from the TABM
    Enc0 =
        maps:fold(
            fun
                (<<"body">>, Value, AccMap) ->
                    AccMap#{ <<"body">> => #{ <<"body">> => Value } };
                (Key, Value, AccMap) ->
                    field_to_http(AccMap, {Key, Value}, #{})
            end,
            #{},
            maps:without([<<"attestations">>, <<"signature">>, <<"signature-input">>], TABM)
        ),
    ?event({prepared_body_map, {msg, Enc0}}),
    BodyMap = maps:get(<<"body">>, Enc0, #{}),
    FlattenedBodyMap = dev_codec_flat:to(BodyMap),
    Enc1 =
        case {FlattenedBodyMap, lists:member(sub_part, Opts)} of
            {X, _} when map_size(X) =:= 0 ->
                % If the body map is empty, then simply set the body to be a 
                % corresponding empty binary.
                ?event({encoding_empty_body, {msg, Enc0}}),
                maps:put(<<"body">>, <<>>, Enc0);
            {#{ <<"body">> := UserBody }, false}
                    when map_size(FlattenedBodyMap) =:= 1 andalso is_binary(UserBody) ->
                % Simply set the sole body binary as the body of the
                % HTTP message, no further encoding required
                % 
                % NOTE: this may only be done for the top most message as 
                % sub-messages MUST be encoded as sub-parts, in order to preserve 
                % the nested hierarchy of messages, even in the case of a sole 
                % body binary.
                % 
                % In all other cases, the mapping fallsthrough to the case below 
                % that properly encodes a nested body within a sub-part
                ?event({encoding_single_body, {body, UserBody}, {http, Enc0}}),
                maps:put(<<"body">>, UserBody, Enc0);
            _ ->
                % Otherwise, we need to encode the body map as the
                % multipart body of the HTTP message
                ?event({encoding_multipart, {bodymap, {explicit, FlattenedBodyMap}}}),
                PartList = to_sorted_list(maps:map(fun encode_body_part/2, FlattenedBodyMap)), 
                Boundary = boundary_from_parts(PartList),
                % Transform body into a binary, delimiting each part with the
                % boundary
                BodyList = lists:foldl(
                    fun ({_PartName, BodyPart}, Acc) ->
                        [
                            <<
                                "--", Boundary/binary, ?CRLF/binary,
                                BodyPart/binary
                            >>
                        |
                            Acc
                        ]
                    end,
                    [],
                    PartList
                ),
                % Finally, join each part of the multipart body into a single binary
                % to be used as the body of the Http Message
                FinalBody = iolist_to_binary(lists:join(?CRLF, lists:reverse(BodyList))),
                % Ensure we append the Content-Type to be a multipart response
                Enc0#{ 
                    <<"content-type">> =>
                        <<"multipart/form-data; boundary=", "\"" , Boundary/binary, "\"">>,
                    <<"body">> => <<FinalBody/binary, ?CRLF/binary, "--", Boundary/binary, "--">>
                }
        end,
    % Finally, add the signatures to the HTTP message
    case maps:get(<<"attestations">>, TABM, not_found) of
        #{ <<"hmac-sha256">> :=
                #{ <<"signature">> := Sig, <<"signature-input">> := SigInput } } ->
            HPs = hashpaths_from_message(TABM),
            EncWithHPs = maps:merge(Enc1, HPs),
            % Add the original signature encodings to the HTTP message
            EncWithHPs#{
                <<"signature">> => Sig,
                <<"signature-input">> => SigInput
            };
        not_found -> Enc1
    end.

% We need to generate a unique, reproducible boundary for the
% multipart body, however we cannot use the id of the message as
% the boundary, as the id is not known until the message is
% encoded. Subsequently, we generate each body part individually,
% concatenate them, and apply a SHA2-256 hash to the result.
% This ensures that the boundary is unique, reproducible, and
% secure.
boundary_from_parts(PartList) ->
    BodyBin =
        iolist_to_binary(
            lists:join(?CRLF,
                lists:map(
                    fun ({_PartName, PartBin}) -> PartBin end,
                    PartList
                )
            )
        ),
    RawBoundary = crypto:hash(sha256, BodyBin),
    hb_util:encode(RawBoundary).

%% Extract all hashpaths from the attestations of a given message
hashpaths_from_message(Msg) ->
    maps:fold(
        fun (_, Att, Acc) ->
            maps:merge(Acc, extract_hashpaths(Att))
        end,
        #{},
        maps:get(<<"attestations">>, Msg, #{})
    ).

%% @doc Extract all keys labelled `hashpath*` from the attestations, and add them
%% to the HTTP message as `hashpath*` keys.
extract_hashpaths(Map) ->
    maps:filter(
        fun (<<"hashpath", _/binary>>, _) -> true;
            (_, _) -> false
        end,
        Map
    ).

%% @doc Encode a multipart body part to a flat binary.
encode_body_part(PartName, BodyPart) ->
    % We'll need to prepend a Content-Disposition header
    % to the part, using the field name as the form part
    % name.
    % (See https://www.rfc-editor.org/rfc/rfc7578#section-4.2).
    Disposition =
        case PartName of
            % The body is always made the inline part of
            % the multipart body
            <<"body">> -> <<"inline">>;
            _ -> <<"form-data;name=", "\"", PartName/binary, "\"">>
        end,
    % Sub-parts MUST have at least one header, according to the
    % multipart spec. Adding the Content-Disposition not only
    % satisfies that requirement, but also encodes the
    % HB message field that resolves to the sub-message
    case BodyPart of
        % TODO: this branch may no longer be needed
        % since we flatten the maps prior to HTTP encoding
        % 
        % For now, keeping recursive logic
        BPMap when is_map(BPMap) ->
            WithDisposition = maps:put(
                <<"content-disposition">>,
                Disposition,
                BPMap
            ),
            SubHttp = to(WithDisposition, [sub_part]),
            encode_http_msg(SubHttp);
        BPBin when is_binary(BPBin) ->
            % A properly encoded inlined body part MUST have a CRLF between
            % it and the header block, so we MUST use two CRLF:
            % - first to signal end of the Content-Disposition header
            % - second to signal the end of the header block
            <<
                "content-disposition: ", Disposition/binary, ?CRLF/binary,
                ?CRLF/binary,
                BPBin/binary
            >>
    end.

%%% @doc Given a map or KVList, return a sorted list of its key-value pairs.
to_sorted_list(Msg) when is_map(Msg) ->
    to_sorted_list(maps:to_list(Msg));
to_sorted_list(Msg) when is_list(Msg) ->
    lists:sort(fun({Key1, _}, {Key2, _}) -> Key1 < Key2 end, Msg).

encode_http_msg(Httpsig) ->
    % Serialize the headers, to be included in the part of the multipart response
    HeaderList = lists:foldl(
        fun ({HeaderName, HeaderValue}, Acc) ->
            ?event({encoding_http_header, {header, HeaderName}, {value, HeaderValue}}),
            [<<HeaderName/binary, ": ", HeaderValue/binary>> | Acc]
        end,
        [],
        maps:to_list(maps:without([<<"body">>], Httpsig))
    ),
    EncodedHeaders = iolist_to_binary(lists:join(?CRLF, lists:reverse(HeaderList))),
    case maps:get(<<"body">>, Httpsig, <<>>) of
        <<>> -> EncodedHeaders;
        % Some-Headers: some-value
        % content-type: image/png
        % 
        % <body>
        SubBody -> <<EncodedHeaders/binary, ?DOUBLE_CRLF/binary, SubBody/binary>>
    end.

signatures_to_httpsig(Httpsig, SignaturesBin, SigInputsBin) ->
    ?event({encoding_signatures, SignaturesBin, SigInputsBin, {msg, Httpsig}}),
    SigDict = maps:from_list(dev_codec_structured_conv:parse_dictionary(SignaturesBin)),
    ?event({signature_dicts, {sigs, SigDict}, {inputs, SigInputsBin}}),
    {SfSigInputs, SfSigs} = maps:map(
        fun (Attestor, SignatureMap) ->
            {Attestor, SignatureMap}
        end,
        % Start with empty Structured Field Dictionaries
        {[], []},
        SigDict
    ),
    % Signature and Signature-Input are always encoded as Structured Field dictionaries, and then
    % each transmitted either as a header, or as a part in the multi-part body
    WithSig = field_to_http(Httpsig, {<<"signature">>, dev_codec_structured_conv:dictionary(SfSigs)}, #{}),
    WithSigAndInput = field_to_http(WithSig, {<<"signature-input">>, dev_codec_structured_conv:dictionary(SfSigInputs)}, #{}),
    WithSigAndInput.

% All maps are encoded into the body of the HTTP message
% to be further encoded later.
field_to_http(Httpsig, {Name, Value}, _Opts) when is_map(Value) ->
    NormalizedName = hb_converge:normalize_key(Name),
    OldBody = maps:get(<<"body">>, Httpsig, #{}),
    Httpsig#{ <<"body">> => OldBody#{ NormalizedName => Value } };
field_to_http(Httpsig, {Name, Value}, Opts) when is_binary(Value) ->
    NormalizedName = hb_converge:normalize_key(Name),
    % The default location where the value is encoded within the HTTP
    % message depends on its size.
    % 
    % So we check whether the size of the value is within the threshold
    % to encode as a header, and otherwise default to encoding in the body.
    %
    % Note that a "where" Opts may force the location of the encoded
    % value -- this is only a default location if not specified in Opts 
    DefaultWhere = case {maps:get(where, Opts, headers), byte_size(Value)} of
        {headers, Fits} when Fits =< ?MAX_HEADER_LENGTH -> headers;
        _ -> body
    end,
    case maps:get(where, Opts, DefaultWhere) of
        headers ->
            Httpsig#{ NormalizedName => Value };
        body ->
            OldBody = maps:get(<<"body">>, Httpsig, #{}),
            Httpsig#{ <<"body">> => OldBody#{ NormalizedName => Value } }
    end.
