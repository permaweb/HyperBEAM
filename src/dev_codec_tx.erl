%%% @doc Codec for managing transformations from `ar_bundles'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_tx).
-export([from/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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
            ?assertEqual(Expected, Actual, Label)
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