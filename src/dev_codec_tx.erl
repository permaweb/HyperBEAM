%%% @doc Codec for managing transformations from `ar_bundles'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_tx).
-export([from/3, to/3, commit/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% THe list of TX fields to include in the TABM. Excludes generated fields as well as
%% fields that are unique to ans104. This is the base list included in both format 1 and
%% format 2 transactions.
-define(TX_KEYS,
    [
        <<"last_tx">>,
        <<"owner">>,
        <<"target">>,
        <<"quantity">>,
        <<"signature">>,
        <<"reward">>,
        <<"denomination">>,
        <<"signature_type">>
    ]
).

%% Default #tx format to use if no format is provided. Limited to formats that are valid
%% for Arweave L1 transcations (1 and 2 as of June 2025)
-define(DEFAULT_FORMAT, 2).

%% XXX: need to test format=1 vs. format=2. I think now we migth not be getting test
%% coverage for format=2.

%% @doc Sign a message using the `priv_wallet' key in the options. Supports both
%% the `hmac-sha256' and `rsa-pss-sha256' algorithms, offering unsigned and
%% signed commitments.
commit(Msg, Req, Opts) ->
    hb_tx:commit_message(<<"tx@1.0">>, Msg, Req, Opts).

%% @doc Verify an Arweave L1 transaction commitment.
verify(Msg, Req, Opts) ->
    hb_tx:verify_message(<<"tx@1.0">>, Msg, Req, Opts).

%% @doc Convert an arweave #tx record into a message map.
%% Current Arweave #tx restrictions (enforced by ar_tx:enforce_valid_tx/1):
%% - only RSA signatures are supported
from(Binary, _Req, _Opts) when is_binary(Binary) -> {ok, Binary};
from(TX, Req, Opts) when is_record(TX, tx) ->
    case TX#tx.format of
        Format when Format =:= 1 orelse Format =:= 2 ->
            true = ar_tx:enforce_valid_tx(TX),
            TABM = hb_tx:tx_to_tabm(TX, committed_tags(TX), Req, Opts),
            {ok, TABM};
        _ ->
            ?event({invalid_arweave_tx_format, {format, TX#tx.format}, {tx, TX}}),
            throw(invalid_tx)
    end.

to(Binary, _Req, _Opts) when is_binary(Binary) ->
    TX = hb_tx:binary_to_tx(Binary),
    {ok, TX#tx{ format = ?DEFAULT_FORMAT }};
to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
to(NormTABM, Req, Opts) when is_map(NormTABM) ->
    ?event({to, NormTABM}),
    % If no format is provided, set to format 2 since we're in the dev_codec_tx module.
    % If the ans104 format is explicitly provided, throw an error as this is the wrong
    % device to use.
    NormTABM2 = case hb_maps:get(<<"format">>, NormTABM, ?DEFAULT_FORMAT) of
        ans104 ->
            throw(invalid_tx);
        Value ->
            NormTABM#{ <<"format">> => Value }
    end,
    TXWithIDs = hb_tx:tabm_to_tx(NormTABM2, Req, Opts),

    TXWithAddress = TXWithIDs#tx{
        owner_address = ar_tx:get_owner_address(TXWithIDs)
    },

    true = ar_tx:enforce_valid_tx(TXWithAddress),
    {ok, TXWithAddress};
to(_Other, _Req, _Opts) ->
    throw(invalid_tx).

%%%===================================================================
%%% Private functions.
%%%===================================================================
tx_keys(#tx{ format = 1 }) ->
    % Format 1 is the non-default value for Arweave transactions, so we want to include
    % the format field.
    ?TX_KEYS ++ [<<"format">>];
tx_keys(TX = #tx{ format = 2 }) ->
    % Normally we can rebuild the data_root from the data, but since format 2 transactions
    % can be signed and posted without data, we sometimes need to force that the data_root
    % and data_size fields are included.
    IncludeDataRoot = (TX#tx.data == ?DEFAULT_DATA) andalso (TX#tx.data_root /= <<>>),
    case IncludeDataRoot of
        true -> ?TX_KEYS ++ [<<"data_size">>, <<"data_root">>];
        false -> ?TX_KEYS
    end.

committed_tags(#tx{ format = 1 }) ->
    ?TX_KEYS ++ [<<"format">>, <<"data">>];
committed_tags(#tx{ format = 2 }) ->
    ?TX_KEYS ++ [<<"format">>, <<"data">>, <<"data_size">>, <<"data_root">>].

%%%===================================================================
%%% Tests.
%%%===================================================================
    
make_expected_tabm(TX, TestOpts, ExpectedTags) ->
    Signed = maps:get(signed, TestOpts, false),
    IncludeOriginalTags = maps:get(include_original_tags, TestOpts, false),
    HasCommitment = Signed orelse IncludeOriginalTags,
    Commitment0 = case {Signed, HasCommitment} of
        {true, _} ->
            ExpectedOwnerAddress = ar_wallet:to_address(TX#tx.owner),
            #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"committer">> => hb_util:safe_encode(ExpectedOwnerAddress),
                <<"committed">> => hb_util:unique(
                    committed_tags(TX)
                        ++ [ hb_ao:normalize_key(Tag) || {Tag, _} <- TX#tx.tags ]
                ),
                <<"keyid">> => hb_util:safe_encode(TX#tx.owner),
                <<"signature">> => hb_util:safe_encode(TX#tx.signature),
                <<"type">> => <<"rsa-pss-sha256">>
            };
        {false, true} ->
            #{
                <<"commitment-device">> => <<"tx@1.0">>,
                <<"type">> => <<"unsigned-sha256">>
            };
       _ ->
            #{}
    end,
    Commitment1 = case IncludeOriginalTags of
        true -> Commitment0#{
            <<"original-tags">> => hb_tx:encoded_tags_to_map(TX#tx.tags)
        };
        false -> Commitment0
    end,

    Commitments = case {Signed, HasCommitment} of
        {true, _} ->
            ID = ar_tx:generate_id(TX, signed),
            #{ <<"commitments">> => #{ hb_util:safe_encode(ID) => Commitment1 } };
        {false, true} ->
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

%%%-------------------------------------------------------------------
%%% Tests for `from/3`.
%%%-------------------------------------------------------------------

% from_test_() ->
%     [
%         fun test_from_enforce_valid_tx/0,
%         {timeout, 30, fun test_from_happy/0}
%     ].

% test_from_enforce_valid_tx() ->
%     LastTX = crypto:strong_rand_bytes(33),
%     TX = (ar_tx:new())#tx{ 
%         format = 2, 
%         last_tx = LastTX },
%     hb_test_utils:assert_throws(
%         fun from/3,
%         [TX, #{}, #{}],
%         {invalid_field, last_tx, LastTX},
%         "from/3 failed to enforce_valid_tx"
%     ).

% test_from_happy() ->
%     Wallet = ar_wallet:new(),

%     BaseTX = ar_tx:new(),

%     TestCases = [
%         {default_values, BaseTX, #{ include_original_tags => false }, #{}},
%         {non_default_values, 
%             BaseTX#tx{
%                 last_tx = crypto:strong_rand_bytes(32),
%                 target = crypto:strong_rand_bytes(32),
%                 quantity = ?AR(5),
%                 data = <<"test-data">>,
%                 data_size = byte_size(<<"test-data">>),
%                 data_root = crypto:strong_rand_bytes(32),
%                 reward = ?AR(10)
%             },
%             #{ include_original_tags => false },
%             #{}
%         },
%         {single_tag,
%             BaseTX#tx{ tags = [{<<"test-tag">>, <<"test-value">>}] },
%             #{ include_original_tags => false },
%             #{ <<"test-tag">> => <<"test-value">> }
%         },
%         {not_normalized_tag,
%             BaseTX#tx{ tags = [{<<"Test-Tag">>, <<"test-value">>}] },
%             #{ include_original_tags => true },
%             #{ <<"test-tag">> => <<"test-value">> }
%         },
%         {duplicate_tags, 
%             BaseTX#tx{
%                 tags = [{<<"Dup-Tag">>, <<"value-1">>}, {<<"dup-tag">>, <<"value-2">>}]
%             },
%             #{ include_original_tags => true },
%             #{ <<"dup-tag">> => <<"\"value-1\", \"value-2\"">> }
%         }
%     ],

    % %% Iterate over each pair, call from/3, and assert the result matches the
    % %% expected TABM.
    % lists:foreach(
    %     fun({Label, UnsignedTX, TestOpts, ExpectedTags}) ->
    %         %% Unsigned test
    %         ExpectedUnsignedTABM = make_expected_tabm(
    %             UnsignedTX, TestOpts#{ signed => false }, ExpectedTags),
    %         {ok, ActualUnsignedTABM} = from(UnsignedTX, #{}, #{}),
    %         ?assertEqual(ExpectedUnsignedTABM, ActualUnsignedTABM,
    %             lists:flatten(io_lib:format("~p unsigned", [Label]))),

    %         ExpectedUnsignedTX = hb_tx:reset_ids(UnsignedTX),
    %         {ok, RoundTripUnsignedTX} = to(ActualUnsignedTABM, #{}, #{}),
    %         ?assertEqual(ExpectedUnsignedTX, RoundTripUnsignedTX,
    %             lists:flatten(io_lib:format("~p unsigned round-trip", [Label]))),

    %         %% Signed test
    %         SignedTX = ar_tx:sign(UnsignedTX, Wallet),
    %         ExpectedSignedTABM = make_expected_tabm(
    %             SignedTX, TestOpts#{ signed => true }, ExpectedTags),
    %         {ok, ActualSignedTABM} = from(SignedTX, #{}, #{}),
    %         ?assertEqual(ExpectedSignedTABM, ActualSignedTABM,
    %             lists:flatten(io_lib:format("~p signed", [Label]))),

    %         ExpectedSignedTX = hb_tx:reset_ids(SignedTX),
    %         {ok, RoundTripSignedTX} = to(ActualSignedTABM, #{}, #{}),
    %         ?assertEqual(ExpectedSignedTX, RoundTripSignedTX,
    %             lists:flatten(io_lib:format("~p signed round-trip", [Label]))),
    %         ok
    %     end,
    %     TestCases
    % ).

% %%%-------------------------------------------------------------------
% %%% Tests for `to/3`.
% %%%-------------------------------------------------------------------

% to_test_() ->
%     [
%         fun test_to_multisignatures_not_supported/0,
%         fun test_to_invalid_original_tags/0,
%         fun test_to_enforce_valid_tx/0,
%         {timeout, 30, fun test_to_happy/0}
%     ].

% test_to_multisignatures_not_supported() ->
%     Wallet = ar_wallet:new(),
%     SignedTX = ar_tx:sign(ar_tx:new(), Wallet),
%     ValidTABM = make_expected_tabm(SignedTX, #{ signed => true }, #{}),

%     Commitments = maps:get(<<"commitments">>, ValidTABM),
%     [{_CID, Commitment}] = maps:to_list(Commitments),
%     MultiSigTABM = ValidTABM#{
%         <<"commitments">> => Commitments#{ <<"extra-id">> => Commitment }
%     },

%     hb_test_utils:assert_throws(
%         fun to/3,
%         [MultiSigTABM, #{}, #{}],
%         {multisignatures_not_supported_by_ans104, MultiSigTABM},
%         "multisignatures_not_supported_by_ans104"
%     ).

% test_to_invalid_original_tags() ->
%     Wallet = ar_wallet:new(),

%     Tag = <<"test-tag">>,
%     TagVal = <<"test">>,
%     BadTag = <<"bad-tag">>,
%     BadVal = <<"bad">>,

%     SignedTX = ar_tx:sign((ar_tx:new())#tx{ tags = [{Tag, TagVal}] }, Wallet),
%     ValidTABM = make_expected_tabm(SignedTX, #{ signed => true, include_original_tags => true }, #{ Tag => TagVal }),

%     InvalidOrigTABM = ValidTABM#{ BadTag => BadVal },

%     OriginalTags = SignedTX#tx.tags,
%     NormRemaining = #{ Tag => TagVal, BadTag => BadVal },

%     hb_test_utils:assert_throws(
%         fun to/3,
%         [InvalidOrigTABM, #{}, #{}],
%         {invalid_original_tags, OriginalTags, NormRemaining},
%         "invalid_original_tags"
%     ).

% test_to_enforce_valid_tx() ->
%     LastTX = crypto:strong_rand_bytes(33),
%     TX = (ar_tx:new())#tx{ 
%         format = 2, 
%         last_tx = LastTX },
%     ExpectedTABM = make_expected_tabm(TX, #{ signed => false }, #{}),

%     hb_test_utils:assert_throws(
%         fun to/3,
%         [ExpectedTABM, #{}, #{}],
%         {invalid_field, last_tx, LastTX},
%         "to/3 failed to enforce_valid_tx"
%     ).

% test_to_happy() ->
%     Wallet = ar_wallet:new(),

%     BaseTX = ar_tx:new(),

%     TestCases = [
%         {default_values, BaseTX, exclude_original_tags, #{}},
%         {non_default_values,
%             BaseTX#tx{
%                 last_tx = crypto:strong_rand_bytes(32),
%                 target = crypto:strong_rand_bytes(32),
%                 quantity = ?AR(5),
%                 data = <<"test-data">>,
%                 data_size = byte_size(<<"test-data">>),
%                 data_root = crypto:strong_rand_bytes(32),
%                 reward = ?AR(10)
%             },
%             #{ include_original_tags => false },
%             #{}
%         },
%         {single_tag,
%             BaseTX#tx{ tags = [{<<"test-tag">>, <<"test-value">>}] },
%             #{ include_original_tags => false },
%             #{ <<"test-tag">> => <<"test-value">> }
%         },
%         {not_normalized_tag,
%             BaseTX#tx{ tags = [{<<"Test-Tag">>, <<"test-value">>}] },
%             #{ include_original_tags => true },
%             #{ <<"test-tag">> => <<"test-value">> }
%         },
%         {duplicate_tags,
%             BaseTX#tx{
%                 tags = [{<<"Dup-Tag">>, <<"value-1">>}, {<<"dup-tag">>, <<"value-2">>}]
%             },
%             #{ include_original_tags => true },
%             #{ <<"dup-tag">> => <<"\"value-1\", \"value-2\"">> }
%         }
%     ],

%     %% Iterate over each test case, exercising to/3 and ensuring round-trip fidelity.
%     lists:foreach(
%         fun({Label, UnsignedTX, TestOpts, ExpectedTags}) ->
%             %% Unsigned test
%             ExpectedUnsignedTABM = make_expected_tabm(
%                 UnsignedTX, TestOpts#{ signed => false }, ExpectedTags),
%             ExpectedUnsignedTX = hb_tx:reset_ids(UnsignedTX),

%             {ok, ActualUnsignedTX} = to(ExpectedUnsignedTABM, #{}, #{}),
%             ?assertEqual(ExpectedUnsignedTX, ActualUnsignedTX, 
%                 lists:flatten(io_lib:format("~p unsigned", [Label]))),

%             {ok, RoundTripUnsignedTABM} = from(ActualUnsignedTX, #{}, #{}),
%             ?assertEqual(ExpectedUnsignedTABM, RoundTripUnsignedTABM, 
%                 lists:flatten(io_lib:format("~p unsigned round-trip", [Label]))),

%             %% Signed test
%             SignedTX = ar_tx:sign(UnsignedTX, Wallet),
%             ExpectedSignedTABM = make_expected_tabm(
%                 SignedTX, TestOpts#{ signed => true }, ExpectedTags),
%             ExpectedSignedTX = hb_tx:reset_ids(SignedTX),

%             {ok, ActualSignedTX} = to(ExpectedSignedTABM, #{}, #{}),
%             ?assertEqual(ExpectedSignedTX, ActualSignedTX, 
%                 lists:flatten(io_lib:format("~p signed", [Label]))),

%             {ok, RoundTripSignedTABM} = from(ActualSignedTX, #{}, #{}),
%             ?assertEqual(ExpectedSignedTABM, RoundTripSignedTABM, 
%                 lists:flatten(io_lib:format("~p signed round-trip", [Label]))),
%             ok
%         end,
%         TestCases
%     ).


%%%-------------------------------------------------------------------
%%% convert tests
%%%-------------------------------------------------------------------

hb_message_test_() ->
    [
        {timeout, 30, fun test_hb_message_no_format_happy/0}
        % {timeout, 30, fun test_hb_message_format_1_happy/0},
        % {timeout, 30, fun test_hb_message_format_2_happy/0}
    ].

test_hb_message_no_format_happy() ->
    BaseMsg = #{ <<"normal_key">> => <<"NORMAL_VALUE">> },

    RandomID = crypto:strong_rand_bytes(32),
    ExtraTestCases = [
        {data_root_only, 
            #{ <<"data_root">> => RandomID, <<"data_size">> => 1024 },
            #{ <<"data_root">> => RandomID, <<"data_size">> => 1024 }
        }
    ],

    % When no format is provided, it defaults to 2
    do_test_hb_message(no_format, BaseMsg, BaseMsg, ExtraTestCases).

test_hb_message_format_1_happy() ->
    BaseMsg = #{ <<"normal_key">> => <<"NORMAL_VALUE">> },

    RandomID = crypto:strong_rand_bytes(32),
    ExtraTestCases = [
        % format 1 transactions don't support data_root so it won't be preserved across
        % the conversion.
        {data_root_only, 
            #{},
            #{ <<"data_root">> => RandomID, <<"data_size">> => 1024 }
        }
    ],

    do_test_hb_message(
        format_1_integer,
        BaseMsg#{ <<"format">> => 1 },
        BaseMsg#{ <<"format">> => 1 }, 
        ExtraTestCases),
    do_test_hb_message(
        format_1_binary,
        BaseMsg#{ <<"format">> => 1 },
        BaseMsg#{ <<"format">> => <<"1">> },
        ExtraTestCases).

test_hb_message_format_2_happy() ->
    BaseMsg = #{ <<"normal_key">> => <<"NORMAL_VALUE">> },

    RandomID = crypto:strong_rand_bytes(32),
    ExtraTestCases = [
        {data_root_only, 
            #{ <<"data_root">> => RandomID, <<"data_size">> => 1024 },
            #{ <<"data_root">> => RandomID, <<"data_size">> => 1024 }
        }
    ],

    do_test_hb_message(
        format_2_integer,
        BaseMsg,
        BaseMsg#{ <<"format">> => 2 },
        ExtraTestCases),
    do_test_hb_message(
        format_2_binary,
        BaseMsg,
        BaseMsg#{ <<"format">> => <<"2">> },
        ExtraTestCases).

do_test_hb_message(BaseLabel, BaseExpectedMsg, BaseInputMsg, FormatTestCases) ->
    Wallet = hb:wallet(),
    RandomID = crypto:strong_rand_bytes(32),

    Data = <<"DATA_VALUE">>,

    % Testcases that apply to all formats.
    CommonTestCases = [
        {base, #{}, #{}},
        {data,
            #{ <<"data">> => Data },
            #{ 
                <<"data">> => Data,
                <<"data_root">> => ar_tx:data_root(Data), 
                <<"data_size">> => byte_size(Data) }
        },
        {quantity, 
            #{ <<"quantity">> => 10 },
            #{ <<"quantity">> => 10 }
        },
        {last_tx,
            #{ <<"last_tx">> => RandomID },
            #{ <<"last_tx">> => RandomID }
        },
        {target,
            #{ <<"target">> => RandomID },
            #{ <<"target">> => RandomID }
        },
        {reward,
            #{ <<"reward">> => 10 }, 
            #{ <<"reward">> => 10 }
        }
    ],

    TestCases = CommonTestCases ++ FormatTestCases,

    lists:foreach(
        fun({TestLabel, ExtraExpectedMsg, ExtraInputMsg}) ->
            
            ExpectedMsg = maps:merge(BaseExpectedMsg, ExtraExpectedMsg),
            InputMsg = maps:merge(BaseInputMsg, ExtraInputMsg),
            Codec = <<"tx@1.0">>,
            Opts = #{ priv_wallet => Wallet },

            % Unsigned rountrip test
            UnsignedLabel = lists:flatten(io_lib:format(
                "~p ~p unsigned", [BaseLabel, TestLabel])),
            UnsignedEncoded = hb_message:convert(InputMsg, Codec, <<"structured@1.0">>, Opts),
            ?event({{test, UnsignedLabel}, {encoded, UnsignedEncoded}}),
            UnsignedDecoded = hb_message:convert(UnsignedEncoded, <<"structured@1.0">>, Codec, Opts),
            ?event({{test, UnsignedLabel}, {decoded, UnsignedDecoded}}),
            ?assert(hb_message:match(ExpectedMsg, UnsignedDecoded, strict, Opts), UnsignedLabel),

            ?event({{start, UnsignedLabel}, {input, {explicit, InputMsg}}}),
            % Signed roundtrip test
            SignedLabel = lists:flatten(io_lib:format(
                "~p ~p signed", [BaseLabel, TestLabel])),
            Signed = hb_message:commit(InputMsg, Opts, Codec),
            ?event({{test, SignedLabel}, {signed, {explicit, Signed}}}),
            ?assert(hb_message:verify(Signed, all, Opts), SignedLabel),
            SignedEncoded = hb_message:convert(Signed, Codec, <<"structured@1.0">>, Opts),
            ?event({{test, SignedLabel}, {encoded, SignedEncoded}}),
            SignedDecoded = hb_message:convert(SignedEncoded, <<"structured@1.0">>, Codec, Opts),
            ?event({{test, SignedLabel}, {decoded, SignedDecoded}}),
            ?assert(hb_message:match(Signed, SignedDecoded, strict, Opts), SignedLabel),
            ok
        end,
        TestCases
    ).



%%%-------------------------------------------------------------------
%%% Tests for `commit/3`.
%%%-------------------------------------------------------------------

% commit_test_() ->
%     [
%         {timeout, 30, fun test_commit_happy/0}
%     ].

% test_commit_happy() ->
%     Wallet = ar_wallet:new(),
%     TX = ar_tx:new(),
%     SignedTX = ar_tx:sign(TX, Wallet),
%     {ok, Msg} = from(TX, #{}, #{}),
%     {ok, Committed} = commit(Msg, #{ <<"type">> => <<"signed">> }, #{ priv_wallet => Wallet }),
%     Expected = make_expected_tabm(SignedTX, signed, exclude_original_tags, #{}),
%     ?event({{expected, {explicit, Expected}}, {actual, {explicit, Committed}}}),
%     ?assertEqual(Expected, Committed),
%     ok.