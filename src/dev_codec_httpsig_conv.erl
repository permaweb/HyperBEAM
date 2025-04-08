-module(dev_codec_httpsig_conv).
-moduledoc """
A codec for the that marshals TABM encoded messages to and from the
"HTTP" message structure.

Every HTTP message is an HTTP multipart message.
See https://datatracker.ietf.org/doc/html/rfc7578

For each TABM Key:

The Key/Value Pair will be encoded according to the following rules:
    "signatures" -> {SignatureInput, Signature} header Tuples, each encoded
					as a Structured Field Dictionary
    "body" ->
        - if a map, then recursively encode as its own HyperBEAM message
        - otherwise encode as a normal field
    _ -> encode as a normal field

Each field will be mapped to the HTTP Message according to the following 
rules:
    "body" -> always encoded part of the body as with Content-Disposition
			  type of "inline"
    _ ->
        - If the byte size of the value is less than the ?MAX_TAG_VALUE,
		  then encode as a header, also attempting to encode as a
		  structured field.
        - Otherwise encode the value as a part in the multipart response

""".
-export([to/1, from/1]).
%% Helper utilities
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

% The max header length is 4KB
-define(MAX_HEADER_LENGTH, 4096).
% https://datatracker.ietf.org/doc/html/rfc7231#section-3.1.1.4
-define(CRLF, <<"\r\n">>).
-define(DOUBLE_CRLF, <<?CRLF/binary, ?CRLF/binary>>).

-doc """
Convert a HTTP Message into a TABM.
HTTP Structured Field is encoded into it's equivalent TABM encoding.
""".
from(Bin) when is_binary(Bin) -> Bin;
from(HTTP) ->
    % Decode the keys of the HTTP message
    Body = maps:get(<<"body">>, HTTP, <<>>),
    % First, parse all headers excluding the signature-related headers, as they
    % are handled separately.
    {_, InlinedKey} = inline_key(HTTP),
    ?event({inlined_body_key, InlinedKey}),
    Headers = maps:without([<<"body">>, <<"body-keys">>], HTTP),
    ContentType = maps:get(<<"content-type">>, Headers, undefined),
    % Next, we need to potentially parse the body and add to the TABM
    % potentially as sub-TABMs.
    WithBodyKeys = from_body(Headers, InlinedKey, ContentType, Body),
    % Decode the `ao-ids' key into a map. `ao-ids` is an encoding of literal
    % binaries whose keys (given that they are IDs) cannot be distributed as
    % HTTP headers.
    WithIDs = ungroup_ids(WithBodyKeys),
    % Remove the signature-related headers, such that they can be reconstructed
    % from the commitments.
    MsgWithoutSigs = maps:without(
        [<<"signature">>, <<"signature-input">>, <<"commitments">>],
        WithIDs
    ),
    ?event({from_body, {headers, Headers}, {body, Body}, {msgwithoutatts, MsgWithoutSigs}}),
    % Extract all hashpaths from the commitments of the message
    HPs = extract_hashpaths(HTTP),
    % Finally, we need to add the signatures to the TABM
    {ok, MsgWithSigs} = commitments_from_signature(
        maps:without(maps:keys(HPs), MsgWithoutSigs),
        HPs,
        maps:get(<<"signature">>, Headers, not_found),
        maps:get(<<"signature-input">>, Headers, not_found)
    ),
    ?event({message_with_atts, MsgWithSigs}),
    Res = maps:without(Removed = maps:keys(HPs) ++ [<<"content-digest">>], MsgWithSigs),
    ?event({message_without_atts, Res, Removed}),
    Res.

from_body(TABM, _InlinedKey, _ContentType, <<>>) -> TABM;
from_body(TABM, InlinedKey, ContentType, Body) ->
    ?event({from_body, {from_headers, TABM}, {content_type, {explicit, ContentType}}, {body, Body}}),
    Params =
        case ContentType of
            undefined -> [];
            _ ->
                {item, {_, _XT}, XParams} =
                    hb_structured_fields:parse_item(ContentType),
                XParams
        end,
    case lists:keyfind(<<"boundary">>, 1, Params) of
        false ->
            % The body is not a multipart, so just set as is to the Inlined key on
            % the TABM.
            maps:put(InlinedKey, Body, TABM);
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
            {ok, GroupedTABM} = from_body_parts(TABM, InlinedKey, Parts),
            FullTABM = dev_codec_flat:from(GroupedTABM),
            FullTABM
    end.

from_body_parts (TABM, _InlinedKey, []) ->
    % Ensure the accumulated body keys, if any, are encoded
    % adhering to the TABM structure that all values must be
    % maps or binaries
    % 
    % This prevents needing to have exceptions for <<"body-keys">>
    % during parsing (it's just another binary)
    WithEncodedBodyKeys =
        case maps:get(<<"body-keys">>, TABM, undefined) of
            undefined -> TABM;
            % Assume already encoded
            Bin when is_binary(Bin) -> TABM;
            List when is_list(List) ->
                TABM#{ <<"body-keys">> => encode_body_keys(List) }
        end,
    {ok, WithEncodedBodyKeys};
from_body_parts(TABM, InlinedKey, [Part | Rest]) ->
    % Extract the Headers block and Body. Only split on the FIRST double CRLF
    [RawHeadersBlock, RawBody] =
        case binary:split(Part, [?DOUBLE_CRLF], []) of
            [XRawHeadersBlock] ->
                % no body
                [XRawHeadersBlock, <<>>];
            [XRawHeadersBlock, XRawBody] -> [XRawHeadersBlock, XRawBody]
        end,
    % Extract individual headers
    RawHeaders = binary:split(RawHeadersBlock, ?CRLF, [global]),
    % Now we parse each header, splitting into {Key, Value}
    Headers =
        maps:from_list(lists:filtermap(
            fun(<<>>) -> false;
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
                hb_structured_fields:parse_item(RawDisposition),
            {ok, PartName} =
                case Disposition of
                    <<"inline">> ->
                        {ok, InlinedKey};
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
                        case RawBody of
                            <<>> -> RestHeaders;
                            _ -> RestHeaders#{ <<"body">> => RawBody }
                        end
                end,
            BodyKey = hd(binary:split(PartName, <<"/">>)),
            TABMNext = TABM#{
                PartName => ParsedPart,
                <<"body-keys">> => maps:get(<<"body-keys">>, TABM, []) ++ [BodyKey]
            },
            from_body_parts(TABMNext, InlinedKey, Rest)
    end.

-doc """
Populate the `/commitments' key on the TABM with the dictionary of 
signatures and their corresponding inputs.
""".
commitments_from_signature(Map, _HPs, not_found, _RawSigInput) ->
    ?event({no_sigs_found_in_from, {msg, Map}}),
    {ok, maps:without([<<"commitments">>], Map)};
commitments_from_signature(Map, HPs, RawSig, RawSigInput) ->
    SfSigsKV = hb_structured_fields:parse_dictionary(RawSig),
    SfInputs = maps:from_list(hb_structured_fields:parse_dictionary(RawSigInput)),
    ?event({adding_sigs_and_inputs, {sigs, SfSigsKV}, {inputs, SfInputs}}),
    % Build a Map for Signatures by gathering each Signature
    % with its corresponding Inputs.
    % 
    % Inputs are merged as fields on the Signature Map
    Commitments = maps:from_list(lists:map(
        fun ({SigName, Signature}) ->
            ?event({adding_commitment, {sig, SigName}, {sig, Signature}, {inputs, SfInputs}}),
            {list, SigInputs, ParamsKVList} = maps:get(SigName, SfInputs, #{}),
            ?event({inputs, {signame, SigName}, {inputs, SigInputs}, {params, ParamsKVList}}),
            % Find all hashpaths from the signature and add them to the 
            % commitments message.
            Hashpath =
                lists:filtermap(
                    fun ({item, BareItem, _}) ->
                        case hb_structured_fields:from_bare_item(BareItem) of
                            HP = <<"hashpath", _/binary>> -> {true, HP};
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
            {string, Alg} = maps:get(<<"alg">>, Params),
            PubKey = hb_util:decode(EncPubKey),
            Address = hb_util:human_id(ar_wallet:to_address(PubKey)),
            ?event({calculated_name,
                {address, Address},
                {sig, Signature},
                {inputs, {explicit, SfInputs},
                {implicit, Params}}
            }),
            SerializedSig = iolist_to_binary(
                hb_structured_fields:dictionary(
                    #{ SigName => Signature }
                )
            ),
            {item, {binary, UnencodedSig}, _} = Signature,
            {
                hb_util:human_id(crypto:hash(sha256, UnencodedSig)),
                Hashpaths#{
                    <<"commitment-device">> => <<"httpsig@1.0">>,
                    <<"committer">> => Address,
                    <<"alg">> => Alg,
                    <<"signature">> => SerializedSig,
                    <<"signature-input">> =>
                        iolist_to_binary(
                            hb_structured_fields:dictionary(
                                #{ SigName => maps:get(SigName, SfInputs) }
                            )
                        )
                }
            }
        end,
        SfSigsKV
    )),
    % Place the commitments as a top-level message on the parent message
    ?event({adding_commitments, {msg, Map}, {commitments, Commitments}}),
    Msg = Map#{ <<"commitments">> => Commitments },
    % Reset the HMAC on the message if none is present
    case hb_message:commitment(#{ <<"alg">> => <<"hmac-sha256">> }, Msg) of
        X when (X == not_found) or (X == multiple_matches) ->
            ?event({resetting_hmac, {msg, Msg}}),
            dev_codec_httpsig:reset_hmac(Msg);
        _ ->
            ?event({hmac_already_present, {msg, Msg}}),
            Msg
    end.

-doc """
Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map
that can translated to a given web server Response API
""".
to(Bin) when is_binary(Bin) -> Bin;
to(TABM) -> to(TABM, []).
to(TABM, Opts) when is_map(TABM) ->
    % Group the IDs into a dictionary, so that they can be distributed as
    % HTTP headers. If we did not do this, ID keys would be lower-cased and
    % their comparability against the original keys would be lost.
    WithGroupedIDs = group_ids(TABM),
    Stripped =
        maps:without(
            [
                <<"commitments">>,
                <<"signature">>,
                <<"signature-input">>,
                <<"priv">>
            ],
            WithGroupedIDs
        ),
    ?event({stripped, Stripped}),
    {InlineFieldHdrs, InlineKey} = inline_key(TABM),
    Intermediate = do_to(Stripped, Opts ++ [{inline, InlineFieldHdrs, InlineKey}]),
    % Finally, add the signatures to the HTTP message
    case hb_message:commitment(#{ <<"alg">> => <<"hmac-sha256">> }, TABM) of
        {ok, _, #{ <<"signature">> := Sig, <<"signature-input">> := SigInput }} ->
            HPs = hashpaths_from_message(TABM),
            EncWithHPs = maps:merge(Intermediate, HPs),
            % Add the original signature encodings to the HTTP message
            Res = EncWithHPs#{
                <<"signature">> => Sig,
                <<"signature-input">> => SigInput
            },
            ?event({final_encoded_msg, sigs_added, Res}),
            Res;
        _ ->
            ?event({final_encoded_msg, no_sigs_added, Intermediate}),
            Intermediate
    end.

do_to(Binary, _Opts) when is_binary(Binary) -> Binary;
do_to(TABM, Opts) when is_map(TABM) ->
    InlineKey =
        case lists:keyfind(inline, 1, Opts) of
            {inline, _InlineFieldHdrs, Key} -> Key;
            _ -> not_set
        end,
    % Calculate the initial encoding from the TABM
    Enc0 =
        maps:fold(
            fun(<<"body">>, Value, AccMap) ->
                    OldBody = maps:get(<<"body">>, AccMap, #{}),
                    AccMap#{ <<"body">> => OldBody#{ <<"body">> => Value } };
               (Key, Value, AccMap) when Key =:= InlineKey andalso InlineKey =/= not_set ->
                    OldBody = maps:get(<<"body">>, AccMap, #{}),
                    AccMap#{ <<"body">> => OldBody#{ InlineKey => Value } };
               (Key, Value, AccMap) ->
                    field_to_http(AccMap, {Key, Value}, #{})
            end,
            % Add any inline field denotations to the HTTP message
            case lists:keyfind(inline, 1, Opts) of
                {inline, InlineFieldHdrs, _InlineKey} -> InlineFieldHdrs;
                _ -> #{}
            end,
            maps:without([<<"priv">>], TABM)
        ),
    ?event({prepared_body_map, {msg, Enc0}}),
    BodyMap = maps:get(<<"body">>, Enc0, #{}),
    GroupedBodyMap = group_maps(BodyMap),
    Enc1 =
        case GroupedBodyMap of
            EmptyBody when map_size(EmptyBody) =:= 0 ->
                % If the body map is empty, then simply set the body to be a 
                % corresponding empty binary.
                ?event({encoding_empty_body, {msg, Enc0}}),
                Enc0;
            #{ InlineKey := UserBody }
                    when map_size(GroupedBodyMap) =:= 1 andalso is_binary(UserBody) ->
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
                ?event({encoding_multipart, {bodymap, {explicit, GroupedBodyMap}}}),
                PartList = hb_util:to_sorted_list(
                    maps:map(
                        fun(Key, M = #{ <<"body">> := _ }) when map_size(M) =:= 1 ->
                            % If the map has only one key, and it is `body`,
                            % then we must encode part name with the additional
                            % `/body` suffix. This is because otherwise, the `body`
                            % element will be assumed to be an inline part, removing
                            % the necessary hierarchy.
                            encode_body_part(
                                <<Key/binary, "/body">>,
                                M,
                                <<"body">>
                            );
                           (Key, Value) ->
                            encode_body_part(Key, Value, InlineKey)
                        end,
                        GroupedBodyMap
                    )
                ),
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
                    % TODO: Is this needed here?
                    % We ought not be sending body-keys over the wire, so we either need
                    % to remove this here, or at the edge
                    <<"body-keys">> => encode_body_keys(PartList),
                    <<"content-type">> =>
                        <<"multipart/form-data; boundary=", "\"" , Boundary/binary, "\"">>,
                    <<"body">> => <<FinalBody/binary, ?CRLF/binary, "--", Boundary/binary, "--">>
                }
        end,
    % Add the content-digest to the HTTP message. `generate_content_digest/1'
    % will return a map with the `content-digest' key set, but the body removed,
    % so we merge the two maps together to maintain the body and the content-digest.
    Enc2 = case maps:get(<<"body">>, Enc1, <<>>) of
        <<>> -> Enc1;
        _ ->
            ?event({adding_content_digest, {msg, Enc1}}),
            maps:merge(
                Enc1,
                dev_codec_httpsig:add_content_digest(Enc1)
            )
    end,
    ?event({final_body_map, {msg, Enc2}}),
    Enc2.

-doc """
Group all elements with:
1. A key that ?IS_ID returns true for, and
2. A value that is immediate
into a combined SF dict-_like_ structure. If not encoded, these keys would 
be sent as headers and lower-cased, losing their comparability against the
original keys. The structure follows all SF dict rules, except that it allows
for keys to contain capitals. The HyperBEAM SF parser will accept these keys,
but standard RFC 8741 parsers will not. Subsequently, the resulting `ao-cased'
key is not added to the `ao-types' map.
""".
group_ids(Map) ->
    % Find all keys that are IDs
    IDDict = maps:filter(fun(K, V) -> ?IS_ID(K) andalso is_binary(V) end, Map),
    % Convert the dictionary into a list of key-value pairs
    IDDictStruct =
        lists:map(
            fun({K, V}) ->
                {K, {item, {string, V}, []}}
            end,
            maps:to_list(IDDict)
        ),
    % Convert the list of key-value pairs into a binary
    IDBin = iolist_to_binary(hb_structured_fields:dictionary(IDDictStruct)),
    % Remove the encoded keys from the map
    Stripped = maps:without(maps:keys(IDDict), Map),
    % Add the ID binary to the map if it is not empty
    case map_size(IDDict) of
        0 -> Stripped;
        _ -> Stripped#{ <<"ao-ids">> => IDBin }
    end.

-doc """
Decode the `ao-ids' key into a map.
""".
ungroup_ids(Msg = #{ <<"ao-ids">> := IDBin }) ->
    % Extract the ID binary from the Map
    EncodedIDsMap = hb_structured_fields:parse_dictionary(IDBin),
    % Convert the value back into a raw binary
    IDsMap =
        lists:map(
            fun({K, {item, {string, Bin}, _}}) -> {K, Bin} end,
            EncodedIDsMap
        ),
    % Add the decoded IDs to the Map and remove the `ao-ids' key
    maps:merge(maps:without([<<"ao-ids">>], Msg), maps:from_list(IDsMap));
ungroup_ids(Msg) -> Msg.

-doc """
Encode a list of body parts into a binary.
""".
encode_body_keys(PartList) when is_list(PartList) ->
    iolist_to_binary(hb_structured_fields:list(lists:map(
        fun
            ({PartName, _}) -> {item, {string, PartName}, []};
            (PartName) when is_binary(PartName) -> {item, {string, PartName}, []}
        end,
        PartList
    ))).

-doc """
Merge maps at the same level, if possible.
""".
group_maps(Map) ->
    group_maps(Map, <<>>, #{}).
group_maps(Map, Parent, Top) when is_map(Map) ->
    ?event({group_maps, {map, Map}, {parent, Parent}, {top, Top}}),
    {Flattened, NewTop} = maps:fold(
        fun(Key, Value, {CurMap, CurTop}) ->
            ?event({group_maps, {key, Key}, {value, Value}}),
            NormKey = hb_ao:normalize_key(Key),
            FlatK =
                case Parent of
                    <<>> -> NormKey;
                    _ -> <<Parent/binary, "/", NormKey/binary>>
                end,
            case Value of
                _ when is_map(Value) ->
                    NewTop = group_maps(Value, FlatK, CurTop),
                    {CurMap, NewTop};
                _ ->
                    ?event({group_maps, {norm_key, NormKey}, {value, Value}}),
                    case byte_size(Value) > ?MAX_HEADER_LENGTH of
                        % the value is too large to be encoded as a header
                        % within a part, so instead lift it to be a top level
                        % part
                        true ->
                            NewTop = maps:put(FlatK, Value, CurTop),
                            {CurMap, NewTop};
                        % Encode the value in the current part
                        false ->
                            NewCurMap = maps:put(NormKey, Value, CurMap),
                            {NewCurMap, CurTop}
                    end
            end
        end,
        {#{}, Top},
        Map
    ),
    case maps:size(Flattened) of
        0 -> NewTop;
        _ -> case Parent of
            <<>> -> maps:merge(NewTop, Flattened);
            _ ->
                Res = NewTop#{ Parent => Flattened },
                ?event({returning_res, {res, Res}}),
                Res
        end
    end.

-doc """
Generate a unique, reproducible boundary for the
multipart body, however we cannot use the id of the message as
the boundary, as the id is not known until the message is
encoded. Subsequently, we generate each body part individually,
concatenate them, and apply a SHA2-256 hash to the result.
This ensures that the boundary is unique, reproducible, and
secure.
""".
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

%% Extract all hashpaths from the commitments of a given message
hashpaths_from_message(Msg) ->
    maps:fold(
        fun (_, Comm, Acc) ->
            maps:merge(Acc, extract_hashpaths(Comm))
        end,
        #{},
        maps:get(<<"commitments">>, Msg, #{})
    ).

-doc """
Extract all keys labelled `hashpath*' from the commitments, and add them
to the HTTP message as `hashpath*' keys.
""".
extract_hashpaths(Map) ->
    maps:filter(
        fun (<<"hashpath", _/binary>>, _) -> true;
            (_, _) -> false
        end,
        Map
    ).

-doc """
Encode a multipart body part to a flat binary.
""".
encode_body_part(PartName, BodyPart, InlineKey) ->
    % We'll need to prepend a Content-Disposition header
    % to the part, using the field name as the form part
    % name.
    % (See https://www.rfc-editor.org/rfc/rfc7578#section-4.2).
    Disposition =
        case PartName of
            % The body is always made the inline part of
            % the multipart body
            InlineKey -> <<"inline">>;
            _ -> <<"form-data;name=", "\"", PartName/binary, "\"">>
        end,
    % Sub-parts MUST have at least one header, according to the
    % multipart spec. Adding the Content-Disposition not only
    % satisfies that requirement, but also encodes the
    % HB message field that resolves to the sub-message
    case BodyPart of
        BPMap when is_map(BPMap) ->
            WithDisposition = maps:put(
                <<"content-disposition">>,
                Disposition,
                BPMap
            ),
            encode_http_msg(WithDisposition);
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

-doc """
given a message, returns a binary tuple:
- A list of pairs to add to the msg, if any
- the field name for the inlined key

In order to preserve the field name of the inlined
part, an additional field may need to be added
""".
inline_key(Msg) ->
    % The message can named a key whose value will be placed
    % in the body as the inline part
    % Otherwise, the Msg <<"body">> is used
    % Otherwise, the Msg <<"data">> is used
    InlineBodyKey = maps:get(<<"inline-body-key">>, Msg, false),
    ?event({inlined, InlineBodyKey}),
    case [
        InlineBodyKey,
        maps:is_key(<<"body">>, Msg),
        maps:is_key(<<"data">>, Msg)
    ] of
        % inline-body-key already exists, so no need to add one
        [Explicit, _, _] when Explicit =/= false -> {#{}, InlineBodyKey};
        % inline-body-key defaults to <<"body">> (see below)
        % So no need to add one
        [_, true, _] -> {#{}, <<"body">>};
        % We need to preserve the inline-body-key, as the <<"data">> field,
        % so that it is preserved during encoding and decoding
        [_, _, true] -> {#{<<"inline-body-key">> => <<"data">>}, <<"data">>};
        % default to body being the inlined part.
        % This makes this utility compatible for both encoding
        % and decoding httpsig@1.0 messages
        _ -> {#{}, <<"body">>}
    end.

-doc """
Encode a HTTP message into a binary.
""".
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

-doc """
All maps are encoded into the body of the HTTP message
to be further encoded later.
""".
field_to_http(Httpsig, {Name, Value}, _Opts) when is_map(Value) ->
    NormalizedName = hb_ao:normalize_key(Name),
    OldBody = maps:get(<<"body">>, Httpsig, #{}),
    Httpsig#{ <<"body">> => OldBody#{ NormalizedName => Value } };
field_to_http(Httpsig, {Name, Value}, Opts) when is_binary(Value) ->
    NormalizedName = hb_ao:normalize_key(Name),
    % The default location where the value is encoded within the HTTP
    % message depends on its size.
    % 
    % So we check whether the size of the value is within the threshold
    % to encode as a header, and otherwise default to encoding in the body.
    
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

group_maps_test() ->
   Map = #{
        <<"a">> => <<"1">>,
        <<"b">> => #{
            <<"x">> => <<"10">>,
            <<"y">> => #{
                <<"z">> => <<"20">>
            },
            <<"foo">> => #{
                <<"bar">> => #{
                    <<"fizz">> => <<"buzz">>
                }
            } 
        },
        <<"c">> => #{
            <<"d">> => <<"30">>
        },
        <<"e">> => <<"2">>,
        <<"buf">> => <<"hello">>,
        <<"nested">> => #{
            <<"foo">> => <<"iiiiii">>,
            <<"here">> => #{
                <<"bar">> => <<"baz">>,
                <<"fizz">> => <<"buzz">>,
                <<"pop">> => #{
                    <<"very-fizzy">> => <<"very-buzzy">>
                }
            }
        }
    },
    Lifted = group_maps(Map),
    ?assertEqual(
        Lifted,
        #{
            <<"a">> => <<"1">>,
            <<"b">> => #{<<"x">> => <<"10">>},
            <<"b/foo/bar">> => #{<<"fizz">> => <<"buzz">>},
            <<"b/y">> => #{<<"z">> => <<"20">>},
            <<"buf">> => <<"hello">>,
            <<"c">> => #{<<"d">> => <<"30">>},
            <<"e">> => <<"2">>,
            <<"nested">> => #{<<"foo">> => <<"iiiiii">>},
            <<"nested/here">> => #{<<"bar">> => <<"baz">>, <<"fizz">> => <<"buzz">>},
            <<"nested/here/pop">> => #{<<"very-fizzy">> => <<"very-buzzy">>}
        }
    ),
    ok.

-doc """
The grouped maps encoding is a subset of the flat encoding,
where on keys with maps values are flattened.

So despite needing a special encoder to produce it
We can simply apply the flat encoder to it to get back
the original message.

The test asserts that is indeed the case.
""".
group_maps_flat_compatible_test() ->
    Map = #{
        <<"a">> => <<"1">>,
        <<"b">> => #{
            <<"x">> => <<"10">>,
            <<"y">> => #{
                <<"z">> => <<"20">>
            },
            <<"foo">> => #{
                <<"bar">> => #{
                    <<"fizz">> => <<"buzz">>
                }
            } 
        },
        <<"c">> => #{
            <<"d">> => <<"30">>
        },
        <<"e">> => <<"2">>,
        <<"buf">> => <<"hello">>,
        <<"nested">> => #{
            <<"foo">> => <<"iiiiii">>,
            <<"here">> => #{
                <<"bar">> => <<"baz">>,
                <<"fizz">> => <<"buzz">>
            }
        }
    },
    Lifted = group_maps(Map),
    ?assertEqual(dev_codec_flat:from(Lifted), Map),
    ok.
