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
    TABM = hb_tx:tx_to_tabm(TX, <<"tx@1.0">>, ?BASE_COMMITTED_TAGS, Req, Opts),
    {ok, TABM}.

to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
to(NormTABM, Req, Opts) when is_map(NormTABM) ->
    TX = hb_tx:tabm_to_tx(NormTABM, Req, Opts),
    ?event({to_result, {explicit, TX}}),
    true = ar_tx:enforce_valid_signed_tx(TX),
    {ok, TX};
to(_Other, _Req, _Opts) ->
    throw(invalid_tx).

%%% ------------------------------------------------------------------------------------------
%%% Tests.
%%% ------------------------------------------------------------------------------------------

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
            <<"original-tags">> => hb_tx:encoded_tags_to_map(TX#tx.tags)
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