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
%% - only RSA signatures are supported
from(Binary, _Req, _Opts) when is_binary(Binary) -> {ok, Binary};
from(RawTX, Req, Opts) 
        when is_record(RawTX, tx), (RawTX#tx.format =:= 1 orelse RawTX#tx.format =:= 2) ->  
    TX = hb_tx:reset_ids(RawTX),
    true = ar_tx:enforce_valid_tx(TX),
    TABM = hb_tx:tx_to_tabm(TX, ?TX_KEYS, <<"tx@1.0">>, ?BASE_COMMITTED_TAGS, Req, Opts),
    {ok, TABM}.

to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
to(NormTABM, Req, Opts) when is_map(NormTABM) ->
    TX = hb_tx:tabm_to_tx(NormTABM, Req, Opts),
    TXWithIDs = hb_tx:reset_ids(TX),
    TXWithAddress = TXWithIDs#tx{ owner_address = ar_tx:get_owner_address(TXWithIDs) },
    true = ar_tx:enforce_valid_tx(TXWithAddress),
    {ok, TXWithAddress};
to(_Other, _Req, _Opts) ->
    throw(invalid_tx).

%%%===================================================================
%%% Tests.
%%%===================================================================

from_test_() ->
    [
        fun test_from_enforce_valid_tx/0,
        {timeout, 30, fun test_from_happy/0}
    ].

test_from_enforce_valid_tx() ->
    LastTX = crypto:strong_rand_bytes(33),
    TX = (ar_tx:new())#tx{ 
        format = 2, 
        last_tx = LastTX },
    hb_test_utils:assert_throws(
        fun from/3,
        [TX, #{}, #{}],
        {invalid_field, last_tx, LastTX},
        "from/3 failed to enforce_valid_tx"
    ).

test_from_happy() ->
    Wallet = ar_wallet:new(),

    BaseTX = ar_tx:new(),

    TestCases = [
        {default_values, BaseTX, exclude_original_tags, #{}},
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
            exclude_original_tags,
            #{}
        },
        {single_tag,
            BaseTX#tx{ tags = [{<<"test-tag">>, <<"test-value">>}] },
            exclude_original_tags,
            #{ <<"test-tag">> => <<"test-value">> }
        },
        {not_normalized_tag,
            BaseTX#tx{ tags = [{<<"Test-Tag">>, <<"test-value">>}] },
            include_original_tags,
            #{ <<"test-tag">> => <<"test-value">> }
        },
        {duplicate_tags, 
            BaseTX#tx{
                tags = [{<<"Dup-Tag">>, <<"value-1">>}, {<<"dup-tag">>, <<"value-2">>}]
            },
            include_original_tags,
            #{ <<"dup-tag">> => <<"\"value-1\", \"value-2\"">> }
        }
    ],

    %% Iterate over each pair, call from/3, and assert the result matches the
    %% expected TABM.
    lists:foreach(
        fun({Label, UnsignedTX, IncludeOriginalTags, ExpectedTags}) ->
            %% Unsigned test
            ExpectedUnsignedTABM = make_expected_tabm(UnsignedTX, unsigned, IncludeOriginalTags, ExpectedTags),
            {ok, ActualUnsignedTABM} = from(UnsignedTX, #{}, #{}),
            ?assertEqual(ExpectedUnsignedTABM, ActualUnsignedTABM,
                lists:flatten(io_lib:format("~p unsigned", [Label]))),

            ExpectedUnsignedTX = hb_tx:reset_ids(UnsignedTX),
            {ok, RoundTripUnsignedTX} = to(ActualUnsignedTABM, #{}, #{}),
            ?assertEqual(ExpectedUnsignedTX, RoundTripUnsignedTX,
                lists:flatten(io_lib:format("~p unsigned round-trip", [Label]))),

            %% Signed test
            SignedTX = ar_tx:sign(UnsignedTX, Wallet),
            ExpectedSignedTABM = make_expected_tabm(SignedTX, signed, IncludeOriginalTags, ExpectedTags),
            {ok, ActualSignedTABM} = from(SignedTX, #{}, #{}),
            ?assertEqual(ExpectedSignedTABM, ActualSignedTABM,
                lists:flatten(io_lib:format("~p signed", [Label]))),

            ExpectedSignedTX = hb_tx:reset_ids(SignedTX),
            {ok, RoundTripSignedTX} = to(ActualSignedTABM, #{}, #{}),
            ?assertEqual(ExpectedSignedTX, RoundTripSignedTX,
                lists:flatten(io_lib:format("~p signed round-trip", [Label]))),
            ok
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
        fun test_to_enforce_valid_tx/0,
        {timeout, 30, fun test_to_happy/0}
    ].

test_to_multisignatures_not_supported() ->
    Wallet = ar_wallet:new(),
    SignedTX = ar_tx:sign(ar_tx:new(), Wallet),
    ValidTABM = make_expected_tabm(SignedTX, signed, exclude_original_tags, #{}),

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
    ValidTABM = make_expected_tabm(SignedTX, signed, include_original_tags, #{ Tag => TagVal }),

    InvalidOrigTABM = ValidTABM#{ BadTag => BadVal },

    OriginalTags = SignedTX#tx.tags,
    NormRemaining = #{ Tag => TagVal, BadTag => BadVal },

    hb_test_utils:assert_throws(
        fun to/3,
        [InvalidOrigTABM, #{}, #{}],
        {invalid_original_tags, OriginalTags, NormRemaining},
        "invalid_original_tags"
    ).

test_to_enforce_valid_tx() ->
    LastTX = crypto:strong_rand_bytes(33),
    TX = (ar_tx:new())#tx{ 
        format = 2, 
        last_tx = LastTX },
    ExpectedTABM = make_expected_tabm(TX, unsigned, exclude_original_tags, #{}),

    hb_test_utils:assert_throws(
        fun to/3,
        [ExpectedTABM, #{}, #{}],
        {invalid_field, last_tx, LastTX},
        "to/3 failed to enforce_valid_tx"
    ).

test_to_happy() ->
    Wallet = ar_wallet:new(),

    BaseTX = ar_tx:new(),

    TestCases = [
        {default_values, BaseTX, exclude_original_tags, #{}},
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
            exclude_original_tags,
            #{}
        },
        {single_tag,
            BaseTX#tx{ tags = [{<<"test-tag">>, <<"test-value">>}] },
            exclude_original_tags,
            #{ <<"test-tag">> => <<"test-value">> }
        },
        {not_normalized_tag,
            BaseTX#tx{ tags = [{<<"Test-Tag">>, <<"test-value">>}] },
            include_original_tags,
            #{ <<"test-tag">> => <<"test-value">> }
        },
        {duplicate_tags,
            BaseTX#tx{
                tags = [{<<"Dup-Tag">>, <<"value-1">>}, {<<"dup-tag">>, <<"value-2">>}]
            },
            include_original_tags,
            #{ <<"dup-tag">> => <<"\"value-1\", \"value-2\"">> }
        }
    ],

    %% Iterate over each test case, exercising to/3 and ensuring round-trip fidelity.
    lists:foreach(
        fun({Label, UnsignedTX, IncludeOriginalTags, ExpectedTags}) ->
            %% Unsigned test
            ExpectedUnsignedTABM = make_expected_tabm(
                UnsignedTX, unsigned, IncludeOriginalTags, ExpectedTags),
            ExpectedUnsignedTX = hb_tx:reset_ids(UnsignedTX),

            {ok, ActualUnsignedTX} = to(ExpectedUnsignedTABM, #{}, #{}),
            ?assertEqual(ExpectedUnsignedTX, ActualUnsignedTX, 
                lists:flatten(io_lib:format("~p unsigned", [Label]))),

            {ok, RoundTripUnsignedTABM} = from(ActualUnsignedTX, #{}, #{}),
            ?assertEqual(ExpectedUnsignedTABM, RoundTripUnsignedTABM, 
                lists:flatten(io_lib:format("~p unsigned round-trip", [Label]))),

            %% Signed test
            SignedTX = ar_tx:sign(UnsignedTX, Wallet),
            ExpectedSignedTABM = make_expected_tabm(
                SignedTX, signed, IncludeOriginalTags, ExpectedTags),
            ExpectedSignedTX = hb_tx:reset_ids(SignedTX),

            {ok, ActualSignedTX} = to(ExpectedSignedTABM, #{}, #{}),
            ?assertEqual(ExpectedSignedTX, ActualSignedTX, 
                lists:flatten(io_lib:format("~p signed", [Label]))),

            {ok, RoundTripSignedTABM} = from(ActualSignedTX, #{}, #{}),
            ?assertEqual(ExpectedSignedTABM, RoundTripSignedTABM, 
                lists:flatten(io_lib:format("~p signed round-trip", [Label]))),
            ok
        end,
        TestCases
    ).

    
make_expected_tabm(TX, Signed, IncludeOriginalTags, ExpectedTags) ->
    HasCommitment = (Signed =:= signed) orelse (IncludeOriginalTags =:= include_original_tags),
    Commitment0 = case {Signed, HasCommitment} of
        {signed, _} ->
            ExpectedOwnerAddress = ar_wallet:to_address(TX#tx.owner),
            #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"committer">> => hb_util:safe_encode(ExpectedOwnerAddress),
                <<"committed">> => hb_util:unique(
                    ?BASE_COMMITTED_TAGS
                        ++ [ hb_ao:normalize_key(Tag) || {Tag, _} <- TX#tx.tags ]
                ),
                <<"keyid">> => hb_util:safe_encode(TX#tx.owner),
                <<"signature">> => hb_util:safe_encode(TX#tx.signature),
                <<"type">> => <<"rsa-pss-sha256">>
            };
        {unsigned, true} ->
            #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"type">> => <<"unsigned-sha256">>
            };
       _ ->
            #{}
    end,
    Commitment1 = case IncludeOriginalTags of
        include_original_tags -> Commitment0#{
            <<"original-tags">> => hb_tx:encoded_tags_to_map(TX#tx.tags)
        };
        exclude_original_tags -> Commitment0
    end,

    Commitments = case {Signed, HasCommitment} of
        {signed, _} ->
            ID = ar_tx:generate_id(TX, signed),
            #{ <<"commitments">> => #{ hb_util:safe_encode(ID) => Commitment1 } };
        {unsigned, true} ->
            ID = ar_tx:generate_id(TX, unsigned),
            #{ <<"commitments">> => #{ hb_util:safe_encode(ID) => Commitment1 }};
        _ ->
            #{}
    end,

    TABM0 = #{
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
    TABM1 = hb_maps:merge(TABM0, Commitments, #{}),
    hb_maps:merge(TABM1, ExpectedTags, #{}).