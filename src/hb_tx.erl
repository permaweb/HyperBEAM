-module(hb_tx).
-export([tx_to_tabm/6, tabm_to_tx/3, encoded_tags_to_map/1]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).

tx_to_tabm(TX, TXKeys, Device, CommittedTags, Req, Opts) ->
    OriginalTagMap = encoded_tags_to_map(TX#tx.tags),
    % Get the raw fields and values of the tx record and pair them. Then convert 
    % the list of key-value pairs into a map, removing irrelevant fields.
    RawTXKeysMap =
        hb_maps:with(TXKeys,
            hb_ao:normalize_keys(
                hb_maps:from_list(
                    lists:zip(
                        record_info(fields, tx),
                        tl(tuple_to_list(TX))
                    )
                ),
				Opts
            ),
			Opts
        ),
    % Normalize `owner' to `keyid', remove 'id', and remove 'signature'
    TXKeysMap =
        maps:without(
            [<<"owner">>, <<"signature">>],
            case maps:get(<<"owner">>, RawTXKeysMap, ?DEFAULT_OWNER) of
                ?DEFAULT_OWNER -> RawTXKeysMap;
                Owner -> RawTXKeysMap#{ <<"keyid">> => Owner }
            end
        ),
    % Generate a TABM from the tags.
    MapWithoutData =
        hb_maps:merge(
            TXKeysMap,
            deduplicating_from_list(TX#tx.tags, Opts),
			Opts
        ),
    ?event({tags_from_tx, {explicit, MapWithoutData}}),
    DataMap =
        case TX#tx.data of
            Data when is_map(Data) ->
                % If the data is a map, we need to recursively turn its children
                % into messages from their tx representations.
                hb_maps:merge(
                    MapWithoutData,
                    hb_maps:map(
                        fun(_, InnerValue) ->
                            hb_util:ok(dev_codec_ans104:from(InnerValue, Req, Opts))
                        end,
                        Data,
                        Opts
                    ),
					Opts
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
        hb_ao:normalize_keys(hb_maps:merge(DataMap, MapWithoutData, Opts), Opts),
    %% Add the commitments to the message if the TX has a signature.
    ?event({message_before_commitments, NormalizedDataMap}),
    WithCommitments =
        case TX#tx.signature of
            ?DEFAULT_SIG ->
                ?event({no_signature_detected, NormalizedDataMap}),
                case normal_tags(TX#tx.tags) of
                    true -> NormalizedDataMap;
                    false ->
                        ID = hb_util:human_id(TX#tx.unsigned_id),
                        NormalizedDataMap#{
                            <<"commitments">> => #{
                                ID => #{
                                    <<"commitment-device">> => Device,
                                    <<"type">> => <<"unsigned-sha256">>,
                                    <<"original-tags">> => OriginalTagMap
                                }
                            }
                        }
                end;
            _ ->
                Address = hb_util:human_id(ar_wallet:to_address(TX#tx.owner)),
                WithoutBaseCommitment =
                    hb_maps:without(
                        [
                            <<"id">>,
                            <<"keyid">>,
                            <<"signature">>,
                            <<"commitment-device">>,
                            <<"original-tags">>
                        ],
                        NormalizedDataMap,
						Opts
                    ),
                ID = hb_util:human_id(TX#tx.id),
                ?event({raw_tx_id, {id, ID}, {explicit, WithoutBaseCommitment}}),
                Commitment = #{
                    <<"commitment-device">> => Device,
                    <<"committer">> => Address,
                    <<"committed">> =>
                        hb_util:unique(
                            CommittedTags
                                ++ [ hb_ao:normalize_key(Tag) || {Tag, _} <- TX#tx.tags ]
                        ),
                    <<"keyid">> => hb_util:encode(TX#tx.owner),
                    <<"signature">> => hb_util:encode(TX#tx.signature),
                    <<"type">> => <<"rsa-pss-sha256">>
                },
                WithoutBaseCommitment#{
                    <<"commitments">> => #{
                        ID =>
                            case normal_tags(TX#tx.tags) of
                                true -> Commitment;
                                false -> Commitment#{
                                    <<"original-tags">> => OriginalTagMap
                                }
                            end
                    }
                }
        end,
    ?event({message_after_commitments, WithCommitments}),
    WithCommitments.

tabm_to_tx(NormTABM, Req, Opts) ->
    % Ensure that the TABM is fully loaded, for now.
    ?event({to, {norm, NormTABM}}),
    TABM =
        hb_ao:normalize_keys(
            hb_maps:without([<<"commitments">>], NormTABM, Opts),
            Opts
        ),
    Commitments = hb_maps:get(<<"commitments">>, NormTABM, #{}, Opts),
    TABMWithComm =
        case hb_maps:keys(Commitments, Opts) of
            [] -> TABM;
            [ID] ->
                Commitment = hb_maps:get(ID, Commitments),
                TABMWithoutCommitmentKeys =
                    TABM#{
                        <<"signature">> =>
                            hb_util:decode(
                                maps:get(<<"signature">>, Commitment,
                                    hb_util:encode(?DEFAULT_SIG)
                                )
                            ),
                        <<"owner">> =>
                            hb_util:decode(
                                maps:get(<<"keyid">>, Commitment,
                                    hb_util:encode(?DEFAULT_OWNER)
                                )
                            )
                    },
                WithOrigKeys =
                    case maps:get(<<"original-tags">>, Commitment, undefined) of
                        undefined -> TABMWithoutCommitmentKeys;
                        OrigKeys ->
                            TABMWithoutCommitmentKeys#{
                                <<"original-tags">> => OrigKeys
                            }
                    end,
                ?event({flattened_tabm, WithOrigKeys}),
                WithOrigKeys;
            _ -> throw({multisignatures_not_supported_by_ans104, NormTABM})
        end,
    OriginalTagMap = hb_maps:get(<<"original-tags">>, TABMWithComm, #{}, Opts),
    OriginalTags = tag_map_to_encoded_tags(OriginalTagMap),
    TABMNoOrigTags = hb_maps:without([<<"original-tags">>], TABMWithComm, Opts),
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages. Notably, we do not simply
    % call message_to_tx/1 on the inner map because that would lead to adding
    % an extra layer of nesting to the data.
    MsgKeyMap =
        hb_maps:map(
            fun(_Key, Msg) when is_map(Msg) -> hb_util:ok(dev_codec_ans104:to(Msg, Req, Opts));
            (_Key, Value) -> Value
            end,
            TABMNoOrigTags,
            Opts
        ),
    NormalizedMsgKeyMap = hb_ao:normalize_keys(MsgKeyMap, Opts),
    % Iterate through the default fields, replacing them with the values from
    % the message map if they are present.
    {RemainingMap, BaseTXList} =
        lists:foldl(
            fun({Field, Default}, {RemMap, Acc}) ->
                NormKey = hb_ao:normalize_key(Field),
                case maps:find(NormKey, NormalizedMsgKeyMap) of
                    error -> {RemMap, [Default | Acc]};
                    {ok, Value} when is_binary(Default) andalso ?IS_ID(Value) ->
                        % NOTE: Do we really want to do this type coercion?
                        {
                            maps:remove(NormKey, RemMap),
                            [
                                try hb_util:native_id(Value) catch _:_ -> Value end
                            |
                                Acc
                            ]
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
            hb_maps:to_list(RemainingMap, Opts)
        ),
    ?event({remaining_keys_to_convert_to_tags, {explicit, Remaining}}),
    ?event({original_tags, {explicit, OriginalTags}}),
    % Check that the remaining keys are as we expect them to be, given the 
    % original tags. We do this by re-calculating the expected tags from the
    % original tags and comparing the result to the remaining keys.
    if length(OriginalTags) > 0 ->
        ExpectedTagsFromOriginal = deduplicating_from_list(OriginalTags, Opts),
        NormRemaining = maps:from_list(Remaining),
        case NormRemaining == ExpectedTagsFromOriginal of
            true -> ok;
            false ->
                ?event(warning,
                    {invalid_original_tags,
                        {expected, ExpectedTagsFromOriginal},
                        {given, NormRemaining}
                    }
                ),
                throw({invalid_original_tags, OriginalTags, NormRemaining})
        end;
    true -> ok
    end,
    % Restore the original tags, or the remaining keys if there are no original
    % tags.
    TX =
        TXWithoutTags#tx {
            tags =
                case OriginalTags of
                    [] -> Remaining;
                    _ -> OriginalTags
                end
        },
    ?event({tx_before_data, TX}),
    % Recursively turn the remaining data items into tx records.
    DataItems = hb_maps:from_list(lists:map(
        fun({Key, Value}) ->
            ?event({data_item, {key, Key}, {value, Value}}),
            {hb_ao:normalize_key(Key), hb_util:ok(dev_codec_ans104:to(Value, Req, Opts))}
        end,
        RawDataItems
    )),
    % Set the data based on the remaining keys.
    TXWithData = 
        case {TX#tx.data, hb_maps:size(DataItems, Opts)} of
            {Binary, 0} when is_binary(Binary) ->
                TX;
            {?DEFAULT_DATA, _} ->
                TX#tx { data = DataItems };
            {Data, _} when is_map(Data) ->
                TX#tx { data = hb_maps:merge(Data, DataItems, Opts) };
            {Data, _} when is_record(Data, tx) ->
                TX#tx { data = DataItems#{ <<"data">> => Data } };
            {Data, _} when is_binary(Data) ->
                TX#tx {
                    data =
                        DataItems#{
                            <<"data">> => hb_util:ok(dev_codec_ans104:to(Data, Req, Opts))
                        }
                }
        end,
    
    ?event({to_result, {explicit, TXWithData}}),
    TXWithData.

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

%%%===================================================================
%%% Private functions.
%%%===================================================================

%% @doc Check whether a list of key-value pairs contains only normalized keys.
normal_tags(Tags) ->
    lists:all(
        fun({Key, _}) ->
            hb_ao:normalize_key(Key) =:= Key
        end,
        Tags
    ).

%% @doc Deduplicate a list of key-value pairs by key, generating a list of
%% values for each normalized key if there are duplicates.
deduplicating_from_list(Tags, Opts) ->
    % Aggregate any duplicated tags into an ordered list of values.
    Aggregated =
        lists:foldl(
            fun({Key, Value}, Acc) ->
                NormKey = hb_ao:normalize_key(Key),
                ?event({deduplicating_from_list, {key, NormKey}, {value, Value}, {acc, Acc}}),
                case hb_maps:get(NormKey, Acc, undefined, Opts) of
                    undefined -> hb_maps:put(NormKey, Value, Acc, Opts);
                    Existing when is_list(Existing) ->
                        hb_maps:put(NormKey, Existing ++ [Value], Acc, Opts);
                    ExistingSingle ->
                        hb_maps:put(NormKey, [ExistingSingle, Value], Acc, Opts)
                end
            end,
            #{},
            Tags
        ),
    ?event({deduplicating_from_list, {aggregated, Aggregated}}),
    % Convert aggregated values into a structured-field list.
    Res =
        hb_maps:map(
            fun(_Key, Values) when is_list(Values) ->
                % Convert Erlang lists of binaries into a structured-field list.
                iolist_to_binary(
                    hb_structured_fields:list(
                        [
                            {item, {string, Value}, []}
                        ||
                            Value <- Values
                        ]
                    )
                );
            (_Key, Value) ->
                Value
            end,
            Aggregated,
            Opts
        ),
    ?event({deduplicating_from_list, {result, Res}}),
    Res.


%% @doc Convert a HyperBEAM-compatible map into an ANS-104 encoded tag list,
%% recreating the original order of the tags.
tag_map_to_encoded_tags(TagMap) ->
    OrderedList = hb_util:message_to_ordered_list(hb_private:reset(TagMap)),
    ?event({ordered_tagmap, {explicit, OrderedList}, {input, {explicit, TagMap}}}),
    lists:map(
        fun(#{ <<"name">> := Key, <<"value">> := Value }) ->
            {Key, Value}
        end,
        OrderedList
    ).

%%%===================================================================
%%% Tests.
%%%===================================================================
encoded_tags_to_map_test_() ->
    [
        fun test_encoded_tags_to_map_happy/0
    ].

%% @doc Test encoded_tags_to_map/1 with multiple test cases in a list.
test_encoded_tags_to_map_happy() ->
    TestCases = [
        {empty, [], #{}},
        {single, [
            {<<"test-key">>, <<"test-val">>}
        ], #{
            <<"1">> => #{<<"name">> => <<"test-key">>, <<"value">> => <<"test-val">>}
        }},
        {multiple, [
            {<<"alpha">>, <<"1">>},
            {<<"beta">>, <<"2">>},
            {<<"gamma">>, <<"3">>}
        ], #{
            <<"1">> => #{<<"name">> => <<"alpha">>, <<"value">> => <<"1">>},
            <<"2">> => #{<<"name">> => <<"beta">>, <<"value">> => <<"2">>},
            <<"3">> => #{<<"name">> => <<"gamma">>, <<"value">> => <<"3">>}
        }}
    ],
    lists:foreach(
        fun({Label, Input, Expected}) ->
            ?assertEqual(Expected, encoded_tags_to_map(Input), Label)
        end,
        TestCases
    ).
