%%% @doc Codec for managing transformations from `ar_bundles'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_tx).
-export([from/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).

%% THe list of TX fields to include in the TABM. Excludes generated fields as well as
%% fields that are unique to ans104
-define(TX_KEYS,
    [
        <<"format">>,
        <<"last_tx">>,
        <<"owner">>,
        <<"target">>,
        <<"quantity">>,
        <<"data">>,
        <<"data_size">>,
        <<"data_root">>,
        <<"signature">>,
        <<"reward">>,
        <<"denomination">>,
        <<"signature_type">>
    ]
).
%% The list of tags that a user is explicitly committing to when they sign an
%% Arweave Transaction message.
-define(BASE_COMMITTED_TAGS, ?TX_KEYS).

%% @doc Convert an arweave #tx record into a message map.
%% Current Arweave #tx restrictions (enforced by ar_tx:enforce_valid_tx/1):
%% - can not be unsigned
%% - owner must be set
%% - only RSA signatures are supported
from(Binary, _Req, _Opts) when is_binary(Binary) -> {ok, Binary};
from(TX, Req, Opts) when is_record(TX, tx) ->
    true = ar_tx:enforce_valid_signed_tx(TX),
    OriginalTagMap = encoded_tags_to_map(TX#tx.tags),
    RawTXKeysMap =
        hb_maps:with(?TX_KEYS,
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
    % Merge the data map with the rest of the TX map and remove any keys that
    % are not part of the message.
    NormalizedDataMap =
        hb_ao:normalize_keys(MapWithoutData, Opts),

    %% Add the commitments to the message if the TX has a signature.
    ?event({message_before_commitments, NormalizedDataMap}),
    WithCommitments =
        case TX#tx.signature of
            ?DEFAULT_SIG ->
                %% XXX this branch should never be hit due to the enforce_valid_signed_tx/1
                %% check above
                ?event({no_signature_detected, NormalizedDataMap}),
                case normal_tags(TX#tx.tags) of
                    true -> NormalizedDataMap;
                    false ->
                        ID = hb_util:human_id(TX#tx.unsigned_id),
                        NormalizedDataMap#{
                            <<"commitments">> => #{
                                ID => #{
                                    <<"commitment-device">> => <<"tx@1.0">>,
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
                    <<"commitment-device">> => <<"tx@1.0">>,
                    <<"committer">> => Address,
                    <<"committed">> =>
                        hb_util:unique(
                            ?BASE_COMMITTED_TAGS
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

    ?event({from,
        raw_tx_keys_map, RawTXKeysMap,
        tx_keys_map, TXKeysMap,
        map_without_data, MapWithoutData,
        normalized_data_map, NormalizedDataMap,
        with_commitments, WithCommitments
    }),
    {ok, WithCommitments}.


%% @doc Check whether a list of key-value pairs contains only normalized keys.
normal_tags(Tags) ->
    lists:all(
        fun({Key, _}) ->
            hb_ao:normalize_key(Key) =:= Key
        end,
        Tags
    ).


%% XXX: move to a common module and write tests for it
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

to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
to(NormTABM, Req, Opts) when is_map(NormTABM) ->
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
            fun(_Key, Msg) when is_map(Msg) -> hb_util:ok(to(Msg, Req, Opts));
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
    ?event({raw_data_items, {explicit, RawDataItems}}),
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
    % ?event({tx_before_data, TX}),
    % Recursively turn the remaining data items into tx records.
    DataItems = hb_maps:from_list(lists:map(
        fun({Key, Value}) ->
            ?event({data_item, {key, Key}, {value, Value}}),
            {hb_ao:normalize_key(Key), hb_util:ok(to(Value, Req, Opts))}
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
                            <<"data">> => hb_util:ok(to(Data, Req, Opts))
                        }
                }
        end,
    Res = TXWithData#tx{ 
        id = crypto:hash(sha256, TXWithData#tx.signature),
        owner_address = ar_wallet:to_address(TXWithData#tx.owner)
    },
        % try ar_bundles:reset_ids(ar_bundles:normalize(TXWithData))
        % catch
        %     _:Error ->
        %         ?event({{reset_ids_error, Error}, {tx_without_data, TX}}),
        %         ?event({prepared_tx_before_ids,
        %             {tags, {explicit, TXWithData#tx.tags}},
        %             {data, TXWithData#tx.data}
        %         }),
        %         throw(Error)
        % end,
    ?event({to_result, {explicit, Res}}),
    true = ar_tx:enforce_valid_signed_tx(Res),
    {ok, Res};
to(_Other, _Req, _Opts) ->
    throw(invalid_tx).

%%%===================================================================
%%% Tests.
%%%===================================================================

from_test_() ->
    [
        fun test_from_enforce_valid_signed_tx/0,
        {timeout, 30, fun test_from_happy/0}
    ].

test_from_enforce_valid_signed_tx() ->
    TX = (ar_tx:new())#tx{ 
        format = 2, 
        id = crypto:strong_rand_bytes(32),
        signature = ?DEFAULT_SIG },
    hb_test_utils:assert_throws(
        fun from/3,
        [TX, #{}, #{}],
        {invalid_field, signature, ?DEFAULT_SIG},
        "from/3 failed to enforce_valid_signed_tx"
    ).

test_from_happy() ->
    Wallet = ar_wallet:new(),

    BaseTX = ar_tx:new(),

    TestCases = [
        {default_values, BaseTX, false, #{}},
        {non_default_values, 
            BaseTX#tx{
                last_tx = crypto:strong_rand_bytes(32),
                target = crypto:strong_rand_bytes(32),
                quantity = ?AR(5),
                data = <<"test-data">>,
                data_size = byte_size(<<"test-data">>),
                data_root = crypto:strong_rand_bytes(32),
                reward = ?AR(10)
            },
            false,
            #{}
        },
        {single_tag,
            BaseTX#tx{ tags = [{<<"test-tag">>, <<"test-value">>}] },
            false,
            #{ <<"test-tag">> => <<"test-value">> }
        },
        {not_normalized_tag,
            BaseTX#tx{ tags = [{<<"Test-Tag">>, <<"test-value">>}] },
            true,
            #{ <<"test-tag">> => <<"test-value">> }
        },
        {duplicate_tags, 
            BaseTX#tx{
                tags = [{<<"Dup-Tag">>, <<"value-1">>}, {<<"dup-tag">>, <<"value-2">>}]
            },
            true,
            #{ <<"dup-tag">> => <<"\"value-1\", \"value-2\"">> }
        }
    ],

    %% Iterate over each pair, call from/3, and assert the result matches the
    %% expected TABM.
    lists:foreach(
        fun({Label, TX, IncludeOriginalTags, ExpectedTags}) ->
            SignedTX = ar_tx:sign(TX, Wallet),
            Expected = make_expected_tabm(SignedTX, IncludeOriginalTags, ExpectedTags),
            {ok, Actual} = from(SignedTX, #{}, #{}),
            ?assertEqual(Expected, Actual, Label),

            {ok, RoundTrip} = to(Actual, #{}, #{}),
            ?assertEqual(SignedTX, RoundTrip, Label)
        end,
        TestCases
    ).

%%%-------------------------------------------------------------------
%%% Tests for `to/3`.
%%%-------------------------------------------------------------------

to_test_() ->
    [
        fun test_to_multisignatures_not_supported/0,
        fun test_to_invalid_original_tags/0,
        fun test_to_enforce_valid_signed_tx/0,
        {timeout, 30, fun test_to_happy/0}
    ].

test_to_multisignatures_not_supported() ->
    Wallet = ar_wallet:new(),
    SignedTX = ar_tx:sign(ar_tx:new(), Wallet),
    ValidTABM = make_expected_tabm(SignedTX, false, #{}),

    Commitments = maps:get(<<"commitments">>, ValidTABM),
    [{_CID, Commitment}] = maps:to_list(Commitments),
    MultiSigTABM = ValidTABM#{
        <<"commitments">> => Commitments#{ <<"extra-id">> => Commitment }
    },

    hb_test_utils:assert_throws(
        fun to/3,
        [MultiSigTABM, #{}, #{}],
        {multisignatures_not_supported_by_ans104, MultiSigTABM},
        "multisignatures_not_supported_by_ans104"
    ).

test_to_invalid_original_tags() ->
    Wallet = ar_wallet:new(),

    Tag = <<"test-tag">>,
    TagVal = <<"test">>,
    BadTag = <<"bad-tag">>,
    BadVal = <<"bad">>,

    SignedTX = ar_tx:sign((ar_tx:new())#tx{ tags = [{Tag, TagVal}] }, Wallet),
    ValidTABM = make_expected_tabm(SignedTX, true, #{ Tag => TagVal }),

    InvalidOrigTABM = ValidTABM#{ BadTag => BadVal },

    OriginalTags = SignedTX#tx.tags,
    NormRemaining = #{ Tag => TagVal, BadTag => BadVal },

    hb_test_utils:assert_throws(
        fun to/3,
        [InvalidOrigTABM, #{}, #{}],
        {invalid_original_tags, OriginalTags, NormRemaining},
        "invalid_original_tags"
    ).

test_to_enforce_valid_signed_tx() ->
    TX = (ar_tx:new())#tx{ 
        format = 2, 
        id = crypto:strong_rand_bytes(32),
        signature = ?DEFAULT_SIG },
    ExpectedTABM = make_expected_tabm(TX, false, #{}),

    hb_test_utils:assert_throws(
        fun to/3,
        [ExpectedTABM, #{}, #{}],
        {invalid_field, signature, ?DEFAULT_SIG},
        "to/3 failed to enforce_valid_signed_tx"
    ).

test_to_happy() ->
    Wallet = ar_wallet:new(),

    BaseTX = ar_tx:new(),

    TestCases = [
        {default_values, BaseTX, false, #{}},
        {non_default_values,
            BaseTX#tx{
                last_tx = crypto:strong_rand_bytes(32),
                target = crypto:strong_rand_bytes(32),
                quantity = ?AR(5),
                data = <<"test-data">>,
                data_size = byte_size(<<"test-data">>),
                data_root = crypto:strong_rand_bytes(32),
                reward = ?AR(10)
            },
            false,
            #{}
        },
        {single_tag,
            BaseTX#tx{ tags = [{<<"test-tag">>, <<"test-value">>}] },
            false,
            #{ <<"test-tag">> => <<"test-value">> }
        },
        {not_normalized_tag,
            BaseTX#tx{ tags = [{<<"Test-Tag">>, <<"test-value">>}] },
            true,
            #{ <<"test-tag">> => <<"test-value">> }
        },
        {duplicate_tags,
            BaseTX#tx{
                tags = [{<<"Dup-Tag">>, <<"value-1">>}, {<<"dup-tag">>, <<"value-2">>}]
            },
            true,
            #{ <<"dup-tag">> => <<"\"value-1\", \"value-2\"">> }
        }
    ],

    %% Iterate over each test case, exercising to/3 and ensuring round-trip fidelity.
    lists:foreach(
        fun({Label, TX, IncludeOriginalTags, ExpectedTags}) ->
            SignedTX = ar_tx:sign(TX, Wallet),
            ExpectedTABM = make_expected_tabm(SignedTX, IncludeOriginalTags, ExpectedTags),

            {ok, ActualTX} = to(ExpectedTABM, #{}, #{}),
            ?assertEqual(SignedTX, ActualTX, Label),

            {ok, RoundTripTABM} = from(ActualTX, #{}, #{}),
            ?assertEqual(ExpectedTABM, RoundTripTABM, Label)
        end,
        TestCases
    ).

    
make_expected_tabm(TX, IncludeOriginalTags, ExpectedTags) ->
    Commitment0 = #{
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"committer">> => hb_util:safe_encode(TX#tx.owner_address),
        <<"committed">> => hb_util:unique(
            ?BASE_COMMITTED_TAGS
                ++ [ hb_ao:normalize_key(Tag) || {Tag, _} <- TX#tx.tags ]
        ),
        <<"keyid">> => hb_util:safe_encode(TX#tx.owner),
        <<"signature">> => hb_util:safe_encode(TX#tx.signature),
        <<"type">> => <<"rsa-pss-sha256">>
    },
    Commitment1 = case IncludeOriginalTags of
        true -> Commitment0#{
            <<"original-tags">> => encoded_tags_to_map(TX#tx.tags)
        };
        false -> Commitment0
    end,

    TABM = #{
        <<"commitments">> => #{ hb_util:safe_encode(TX#tx.id) => Commitment1 },
        <<"format">> => TX#tx.format,
        <<"last_tx">> => TX#tx.last_tx,
        <<"target">> => TX#tx.target,
        <<"quantity">> => TX#tx.quantity,
        <<"data">> => TX#tx.data,
        <<"data_size">> => TX#tx.data_size,
        <<"data_root">> => TX#tx.data_root,
        <<"reward">> => TX#tx.reward,
        <<"denomination">> => TX#tx.denomination,
        <<"signature_type">> => TX#tx.signature_type
    },
    hb_maps:merge(TABM, ExpectedTags, #{}).