%%% @doc Codec for managing transformations from `ar_bundles'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_ans104).
-export([id/1, to/1, from/1, attest/3, verify/3, attested/3, content_type/1]).
-export([serialize/1, deserialize/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).
%% The list of TX fields that users can set directly. Data is excluded because
%% it may be set by the codec in order to support nested messages.
-define(TX_KEYS,
    [
        <<"id">>,
        <<"last_tx">>,
        <<"owner">>,
        <<"target">>,
        <<"signature">>
    ]
).
%% The list of tags that a user is explicitly attesting to when they sign an
%% ANS-104 message.
-define(ATTESTED_TAGS, ?TX_KEYS ++ [<<"data">>]).
%% List of tags that should be removed during `to'. These relate to the nested
%% ar_bundles format that is used by the `ans104@1.0' codec.
-define(FILTERED_TAGS,
    [
        <<"bundle-format">>,
        <<"bundle-map">>,
        <<"bundle-version">>
    ]
).

%% @doc Return the content type for the codec.
content_type(_) -> {ok, <<"application/ans104">>}.

%% @doc Serialize a message or TX to a binary.
serialize(Msg) when is_map(Msg) ->
    serialize(to(Msg));
serialize(TX) when is_record(TX, tx) ->
    {ok, ar_bundles:serialize(TX)}.

%% @doc Deserialize a binary ans104 message to a TABM.
deserialize(#{ <<"body">> := Binary }) ->
    deserialize(Binary);
deserialize(Binary) when is_binary(Binary) ->
    deserialize(ar_bundles:deserialize(Binary));
deserialize(TX) when is_record(TX, tx) ->
    {ok, from(TX)}.

%% @doc Return the ID of a message.
id(Msg) ->
    TABM = dev_codec_structured:from(Msg),
    {ok, (to(TABM))#tx.id}.

%% @doc Sign a message using the `priv_wallet' key in the options.
attest(Msg, _Req, Opts) ->
    ?event({attesting, {input, Msg}}),
    Signed = ar_bundles:sign_item(
        to(hb_private:reset(Msg)),
        Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts)
    ),
    ?event({signed_tx, Signed}),
    ID = Signed#tx.id,
    Owner = Signed#tx.owner,
    Sig = Signed#tx.signature,
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    % Gather the prior attestations.
    PriorAttestations = maps:get(<<"attestations">>, Msg, #{}),
    PriorUnsigned = maps:get(<<"ans104-unsigned">>, PriorAttestations, #{}),
    PriorOriginalTags = maps:get(<<"original-tags">>, PriorUnsigned, undefined),
    Attestation =
        #{
            <<"attestation-device">> => <<"ans104@1.0">>,
            <<"id">> => hb_util:human_id(ID),
            <<"owner">> => Owner,
            <<"signature">> => Sig
        },
    AttestationWithOriginalTags =
        case PriorOriginalTags of
            undefined -> Attestation;
            OriginalTags -> Attestation#{ <<"original-tags">> => OriginalTags }
        end,
    AttestationWithHP =
        case Msg of
            #{ <<"hashpath">> := Hashpath } ->
                AttestationWithOriginalTags#{ <<"hashpath">> => Hashpath };
            _ -> AttestationWithOriginalTags
        end,
    MsgWithoutHP = maps:without([<<"hashpath">>], Msg),
    {ok,
        MsgWithoutHP#{
            <<"attestations">> =>
                (maps:without([<<"ans104-unsigned">>], PriorAttestations))#{
                    Address => AttestationWithHP
                }
        }
    }.

%% @doc Return a list of attested keys from an ANS-104 message.
attested(Msg = #{ <<"trusted-keys">> := RawTKeys, <<"attestations">> := Atts }, _Req, _Opts) ->
    % If the message has a `trusted-keys' field in the immediate layer, we validate
    % that it also exists in the attestation's sub-map. If it exists there (which
    % cannot be written to directly by users), we can trust that the stated keys
    % are present in the message.
    case hb_converge:get(hd(hb_converge:keys(Atts)), Atts, #{}) of
        #{ <<"trusted-keys">> := RawTKeys } ->
            NestedKeys = maps:keys(maps:filter(fun(_, V) -> is_map(V) end, Msg)),
            TKeys = maps:values(hb_converge:normalize_keys(RawTKeys)),
            Implicit =
                case lists:member(<<"ao-types">>, TKeys) of
                    true -> dev_codec_structured:implicit_keys(Msg);
                    false -> []
                end,
            {ok, maps:values(hb_converge:normalize_keys(TKeys))
                ++ Implicit
                ++ NestedKeys ++ ?ATTESTED_TAGS};
        _ ->
            % If the key is not repeated, we cannot trust that the message has
            % the keys in the attestation so we return an error.
            {error, []}
    end;
attested(Msg, Req, Opts) ->
    ?event({running_attested, {input, Msg}}),
    % Remove the attestation that was 'promoted' to the base layer of the message
    % by `message@1.0/attested'. This is safe because `to' will only proceed if 
    % there is a single signature on the message. Subsequently, we can trust that
    % the keys signed by that single attestation speak for 'all' of the 
    % attestations.
    MsgLessGivenAtt = maps:without([<<"attestations">>], Msg),
    ?event({to_verify, {input, MsgLessGivenAtt}}),
    case verify(MsgLessGivenAtt, Req, Opts) of
        {ok, true} ->
            % The message validates, so we can trust that the original keys are
            % all present in the message in its converted state.
            Encoded = to(Msg),
            ?event({verified_tx, Encoded}),
            % Get the immediate (first-level) keys from the encoded message.
            % This is safe because we know that the message is valid. We normalize
            % the keys such that callers can rely on the keys being in a canonical
            % form.
            TagKeys = [ hb_converge:normalize_key(Key) || {Key ,_} <- Encoded#tx.tags ],
            % Get the nested keys from the original message.
            NestedKeys = maps:keys(maps:filter(fun(_, V) -> is_map(V) end, Msg)),
            Implicit =
                case lists:member(<<"ao-types">>, maps:keys(Msg)) of
                    true -> dev_codec_structured:implicit_keys(Msg);
                    false -> []
                end,
            % Return the immediate and nested keys. The `data' field is always
            % attested, so we include it in the list of keys.
            {ok, TagKeys ++ NestedKeys ++ Implicit ++ ?ATTESTED_TAGS};
        _ ->
            ?event({could_not_verify, {msg, MsgLessGivenAtt}}),
            {ok, []}
    end.

%% @doc Verify an ANS-104 attestation.
verify(Msg, _Req, _Opts) ->
    MsgWithoutAttestations = maps:without([<<"attestations">>], hb_private:reset(Msg)),
    TX = to(MsgWithoutAttestations),
    Res = ar_bundles:verify_item(TX),
    {ok, Res}.

%% @doc Convert a #tx record into a message map recursively.
from(Binary) when is_binary(Binary) -> Binary;
from(TX) when is_record(TX, tx) ->
    case lists:keyfind(<<"converge-type">>, 1, TX#tx.tags) of
        false ->
            do_from(TX);
        {<<"converge-type">>, <<"binary">>} ->
            TX#tx.data
    end.
do_from(RawTX) ->
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(ar_bundles:normalize(RawTX)), % <- Is norm necessary?
    OriginalTagMap = encoded_tags_to_map(TX#tx.tags),
    % Get the raw fields and values of the tx record and pair them. Then convert 
    % the list of key-value pairs into a map, removing irrelevant fields.
    TXKeysMap =
        maps:with(?TX_KEYS,
            hb_converge:normalize_keys(
                maps:from_list(
                    lists:zip(
                        record_info(fields, tx),
                        tl(tuple_to_list(TX))
                    )
                )
            )
        ),
    TagsFromTX = hb_converge:normalize_keys(maps:from_list(TX#tx.tags)),
    ?event({tags_from_tx, {explicit, TagsFromTX}}),
    % Check that the original tags did not contain any duplicated keys after 
    % normalization.
    case maps:size(TagsFromTX) =/= maps:size(OriginalTagMap) of
        true ->
            ?event(warning,
                {unsupported_ans104, tag_duplication,
                    {tx, TX},
                    {original_tag_map, OriginalTagMap},
                    {tags_from_tx, TagsFromTX}
                }
            ),
            throw({unsupported_ans104, tag_duplication});
        false -> ok
    end,
    % Generate a TABM from the tags.
    MapWithoutData = maps:merge(TXKeysMap, maps:from_list(TX#tx.tags)),
    DataMap =
        case TX#tx.data of
            Data when is_map(Data) ->
                % If the data is a map, we need to recursively turn its children
                % into messages from their tx representations.
                maps:merge(
                    MapWithoutData,
                    maps:map(fun(_, InnerValue) -> from(InnerValue) end, Data)
                );
            Data when Data == ?DEFAULT_DATA -> MapWithoutData;
            Data when is_binary(Data) -> MapWithoutData#{ <<"data">> => Data };
            Data ->
                ?event({unexpected_data_type, {explicit, Data}}),
                ?event({was_processing, {explicit, TX}}),
                throw(invalid_tx)
        end,
    % Merge the data map with the rest of the TX map and remove any keys that
    % are not part of the message.
    NormalizedDataMap =
        hb_converge:normalize_keys(maps:merge(DataMap, MapWithoutData)),
    %% Add the attestations to the message if the TX has a signature.
    ?event({message_before_attestations, NormalizedDataMap}),
    WithAttestations =
        case TX#tx.signature of
            ?DEFAULT_SIG ->
                case normal_tags(TX#tx.tags) of
                    true -> NormalizedDataMap;
                    false ->
                        NormalizedDataMap#{
                            <<"attestations">> => #{
                                <<"ans-104-unsigned">> => #{
                                    <<"original-tags">> => OriginalTagMap
                                }
                            }
                        }
                end;
            _ ->
                Address = hb_util:human_id(ar_wallet:to_address(TX#tx.owner)),
                WithoutBaseAttestation =
                    maps:without(
                        [
                            <<"id">>,
                            <<"owner">>,
                            <<"signature">>,
                            <<"attestation-device">>,
                            <<"original-tags">>
                        ],
                        NormalizedDataMap
                    ),
                Attestation = #{
                    <<"attestation-device">> => <<"ans104@1.0">>,
                    <<"id">> => hb_util:human_id(TX#tx.id),
                    <<"owner">> => TX#tx.owner,
                    <<"signature">> => TX#tx.signature
                },
                WithoutBaseAttestation#{
                    <<"attestations">> => #{
                        Address =>
                            case normal_tags(TX#tx.tags) of
                                true -> Attestation;
                                false -> Attestation#{
                                    <<"original-tags">> => OriginalTagMap
                                }
                            end
                    }
                }
        end,
    Res = maps:without(?FILTERED_TAGS, WithAttestations),
    ?event({message_after_attestations, Res}),
    Res.

%% @doc Check whether a list of key-value pairs contains only normalized keys.
normal_tags(Tags) ->
    lists:all(
        fun({Key, _}) ->
            hb_converge:normalize_key(Key) =:= Key
        end,
        Tags
    ).

%% @doc Convert an ANS-104 encoded tag list into a HyperBEAM-compatible map.
encoded_tags_to_map(Tags) ->
    hb_util:list_to_numbered_map(
        lists:map(
            fun({Key, Value}) ->
                #{
                    <<"name">> => Key,
                    <<"value">> => Value
                }
            end,
            Tags
        )
    ).

%% @doc Convert a HyperBEAM-compatible map into an ANS-104 encoded tag list,
%% recreating the original order of the tags.
tag_map_to_encoded_tags(TagMap) ->
    OrderedList =
        hb_util:message_to_ordered_list(
            maps:without([<<"priv">>], TagMap)),
    %?event({ordered_list, {explicit, OrderedList}, {input, {explicit, Input}}}),
    lists:map(
        fun(#{ <<"name">> := Key, <<"value">> := Value }) ->
            {Key, Value}
        end,
        OrderedList
    ).

%% @doc Internal helper to translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
to(Binary) when is_binary(Binary) ->
    % ar_bundles cannot serialize just a simple binary or get an ID for it, so
    % we turn it into a TX record with a special tag, tx_to_message will
    % identify this tag and extract just the binary.
    #tx{
        tags= [{<<"converge-type">>, <<"binary">>}],
        data = Binary
    };
to(TX) when is_record(TX, tx) -> TX;
to(RawTABM) when is_map(RawTABM) ->
    % The path is a special case so we normalized it first. It may have been
    % modified by `hb_converge' in order to set it to the current key that is
    % being executed. We should check whether the path is in the
    % `priv/Converge/Original-Path' field, and if so, use that instead of the
    % stated path. This normalizes the path, such that the signed message will
    % continue to validate correctly.
    TABM = hb_converge:normalize_keys(maps:without([<<"attestations">>], RawTABM)),
    Attestations = maps:get(<<"attestations">>, RawTABM, #{}),
    TABMWithAtt =
        case maps:keys(Attestations) of
            [] -> TABM;
            [Address] ->
                maps:merge(
                    TABM,
                    maps:without(
                        [<<"attestation-device">>],
                        maps:get(Address, Attestations)
                    )
                );
            _ -> throw({multisignatures_not_supported_by_ans104, RawTABM})
        end,
    OriginalTagMap = maps:get(<<"original-tags">>, TABMWithAtt, #{}),
    OriginalTags = tag_map_to_encoded_tags(OriginalTagMap),
    TABMNoOrigTags = maps:without([<<"original-tags">>], TABMWithAtt),
    % TODO: Is this necessary now? Do we want to pursue `original-path` as the
    % mechanism for restoring original tags?
    M =
        case {maps:find(<<"path">>, TABMNoOrigTags), hb_private:from_message(TABMNoOrigTags)} of
            {{ok, _}, #{ <<"converge">> := #{ <<"original-path">> := Path } }} ->
                maps:put(<<"path">>, Path, TABMNoOrigTags);
            _ -> TABMNoOrigTags
        end,
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages. Notably, we do not simply
    % call message_to_tx/1 on the inner map because that would lead to adding
    % an extra layer of nesting to the data.
    %?event({message_to_tx, {keys, Keys}, {map, M}}),
    MsgKeyMap =
        maps:map(
            fun(_Key, Msg) when is_map(Msg) -> to(Msg);
            (_Key, Value) -> Value
            end,
            M
        ),
    NormalizedMsgKeyMap = hb_converge:normalize_keys(MsgKeyMap),
    % Iterate through the default fields, replacing them with the values from
    % the message map if they are present.
    {RemainingMap, BaseTXList} =
        lists:foldl(
            fun({Field, Default}, {RemMap, Acc}) ->
                NormKey = hb_converge:normalize_key(Field),
                case maps:find(NormKey, NormalizedMsgKeyMap) of
                    error -> {RemMap, [Default | Acc]};
                    {ok, Value} when is_binary(Default) andalso ?IS_ID(Value) ->
                        {
                            maps:remove(NormKey, RemMap),
                            [hb_util:native_id(Value)|Acc]
                        };
                    {ok, Value} ->
                        {
                            maps:remove(NormKey, RemMap),
                            [Value|Acc]
                        }
                end
            end,
            {NormalizedMsgKeyMap, []},
            hb_message:default_tx_list()
        ),
    % Rebuild the tx record from the new list of fields and values.
    TXWithoutTags = list_to_tuple([tx | lists:reverse(BaseTXList)]),
    % Calculate which set of the remaining keys will be used as tags.
    {Remaining, RawDataItems} =
        lists:partition(
            fun({_Key, Value}) when is_binary(Value) ->
                    case unicode:characters_to_binary(Value) of
                        {error, _, _} -> false;
                        _ -> byte_size(Value) =< ?MAX_TAG_VAL
                    end;
                (_) -> false
            end,
            [ 
                    {Key, maps:get(Key, RemainingMap)}
                ||
                    Key <- maps:keys(RemainingMap)
            ]
        ),
    ?event({remaining_keys_to_convert_to_tags, {explicit, Remaining}}),
    ?event({original_tags, {explicit, OriginalTags}}),
    % Restore the original tags into the tx record.
    % First, we check that the value of the original tags matches the expected
    % values.
    lists:all(
        fun({OriginalKey, OriginalValue}) ->
            NormOriginalKey = hb_converge:normalize_key(OriginalKey),
            Value = maps:get(NormOriginalKey, RemainingMap, undefined),
            case Value of
                OriginalValue -> true;
                undefined ->
                    throw({original_tag_missing, OriginalKey, RemainingMap});
                OtherValue ->
                    throw(
                        {original_tag_mismatch,
                            OriginalKey,
                            {original, OriginalValue},
                            {actual, OtherValue}
                        }
                    )
            end
        end,
        OriginalTags
    ),
    TX = TXWithoutTags#tx {
        tags = case OriginalTags of
            [] -> Remaining;
            _ -> OriginalTags
        end
    },
    % Recursively turn the remaining data items into tx records.
    DataItems = maps:from_list(lists:map(
        fun({Key, Value}) ->
            {hb_converge:normalize_key(Key), to(Value)}
        end,
        RawDataItems
    )),
    % Set the data based on the remaining keys.
    TXWithData = 
        case {TX#tx.data, maps:size(DataItems)} of
            {Binary, 0} when is_binary(Binary) ->
                TX;
            {?DEFAULT_DATA, _} ->
                TX#tx { data = DataItems };
            {Data, _} when is_map(Data) ->
                TX#tx { data = maps:merge(Data, DataItems) };
            {Data, _} when is_record(Data, tx) ->
                TX#tx { data = DataItems#{ <<"data">> => Data } };
            {Data, _} when is_binary(Data) ->
                TX#tx { data = DataItems#{ <<"data">> => to(Data) } }
        end,
    % ar_bundles:reset_ids(ar_bundles:normalize(TXWithData));
    Res = try ar_bundles:reset_ids(ar_bundles:normalize(TXWithData))
    catch
        _:Error ->
            ?event({{reset_ids_error, Error}, {tx_without_data, TX}}),
            ?event({prepared_tx_before_ids,
                {tags, {explicit, TXWithData#tx.tags}},
                {data, TXWithData#tx.data}
            }),
            throw(Error)
    end,
    %?event({result, {explicit, Res}}),
    Res;
to(_Other) ->
    throw(invalid_tx).

%%% ANS-104-specific testing cases.

normal_tags_test() ->
    Msg = #{
        <<"first-tag">> => <<"first-value">>,
        <<"second-tag">> => <<"second-value">>
    },
    Encoded = to(Msg),
    ?event({encoded, Encoded}),
    Decoded = from(Encoded),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, Decoded)).

from_maintains_tag_name_case_test() ->
    TX = #tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>}
        ]
    },
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    ?event({signed_tx, SignedTX}),
    ?assert(ar_bundles:verify_item(SignedTX)),
    TABM = from(SignedTX),
    ?event({tabm, TABM}),
    ConvertedTX = to(TABM),
    ?event({converted_tx, ConvertedTX}),
    ?assert(ar_bundles:verify_item(ConvertedTX)),
    ?assertEqual(ConvertedTX, ar_bundles:normalize(SignedTX)).

restore_tag_name_case_from_cache_test() ->
    TX = #tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>}
        ]
    },
    SignedTX = ar_bundles:sign_item(TX, ar_wallet:new()),
    SignedMsg =
        hb_message:convert(
            SignedTX,
            <<"structured@1.0">>,
            <<"ans104@1.0">>,
            #{}
        ),
    SignedID = hb_message:id(SignedMsg, all),
    ?event({signed_msg, SignedMsg}),
    {ok, ID} = hb_cache:write(SignedMsg, #{}),
    ?event({id, ID}),
    {ok, ReadMsg} = hb_cache:read(SignedID, #{}),
    ?event({restored_msg, ReadMsg}),
    ReadTX = to(ReadMsg),
    ?event({restored_tx, ReadTX}),
    ?assert(hb_message:match(ReadMsg, SignedMsg)),
    ?assert(ar_bundles:verify_item(ReadTX)).

unsupported_duplicated_name_tag_test() ->
    TX = #tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag">>, <<"test-value-2">>}
        ]
    },
    ?assertThrow(
        {unsupported_ans104, tag_duplication},
        from(TX)
    ).
    
simple_to_conversion_test() ->
    Msg = #{
        <<"first-tag">> => <<"first-value">>,
        <<"second-tag">> => <<"second-value">>
    },
    Encoded = to(Msg),
    ?event({encoded, Encoded}),
    Decoded = from(Encoded),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, hb_message:unattested(Decoded))).

only_attested_maintains_target_test() ->
    TX = ar_bundles:sign_item(#tx {
        target = crypto:strong_rand_bytes(32),
        tags = [
            {<<"test-tag">>, <<"test-value">>},
            {<<"test-tag-2">>, <<"test-value-2">>}
        ],
        data = <<"test-data">>
    }, ar_wallet:new()),
    ?event({tx, TX}),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, Decoded}),
    {ok, OnlyAttested} = hb_message:with_only_attested(Decoded),
    ?event({only_attested, OnlyAttested}),
    Encoded = hb_message:convert(OnlyAttested, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({encoded, Encoded}),
    ?assertEqual(TX, Encoded).