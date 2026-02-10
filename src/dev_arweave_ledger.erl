%%% @doc AO-Core Arweave ledger device.
%%%
%%% Implements deterministic balance transitions, transaction validation, and
%%% block assembly/validation primitives using Arweave transaction semantics.
-module(dev_arweave_ledger).

-export([info/1, info/3, default/4]).
-export([
    init/3,
    balance/3,
    validate_tx/3,
    apply_tx/3,
    validate_block/3,
    apply_block/3,
    generate_block/3
]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(AR_FORK_2_5_HEIGHT, 812970).
-define(AR_FORK_2_6_HEIGHT, 1132210).
-define(AR_FORK_2_7_HEIGHT, 1275480).
-define(AR_FORK_2_8_HEIGHT, 1547120).
-define(AR_FORK_2_9_HEIGHT, 1602350).
-define(AR_DATA_CHUNK_SIZE, (256 * 1024)).
-define(PADDING_NODE_DATA_ROOT, <<>>).
-define(RSA_BLOCK_SIG_SIZE, 512).
-define(RSA_BLOCK_PUB_SIZE, 512).
-define(ECDSA_BLOCK_SIG_SIZE, 65).
-define(ECDSA_BLOCK_PUB_SIZE, 33).

info(_Opts) ->
    #{
        default => fun default/4
    }.

info(_Base, _Req, _Opts) ->
    {ok,
        #{
            <<"name">> => <<"arweave-ledger@2.9.5">>,
            <<"description">> => <<"Arweave block/tx validation and ledger transitions">>,
            <<"exports">> =>
                [
                    <<"init">>,
                    <<"balance">>,
                    <<"validate-tx">>,
                    <<"apply-tx">>,
                    <<"validate-block">>,
                    <<"apply-block">>,
                    <<"generate-block">>
                ]
        }
    }.

default(<<"set">>, Base, Req, Opts) ->
    dev_message:set(Base, Req, Opts);
default(<<"keys">>, Base, _Req, _Opts) ->
    dev_message:keys(Base);
default(<<"init">>, Base, Req, Opts) ->
    init(Base, Req, Opts);
default(<<"balance">>, Base, Req, Opts) ->
    balance(Base, Req, Opts);
default(<<"validate-tx">>, Base, Req, Opts) ->
    validate_tx(Base, Req, Opts);
default(<<"apply-tx">>, Base, Req, Opts) ->
    apply_tx(Base, Req, Opts);
default(<<"validate-block">>, Base, Req, Opts) ->
    validate_block(Base, Req, Opts);
default(<<"apply-block">>, Base, Req, Opts) ->
    apply_block(Base, Req, Opts);
default(<<"generate-block">>, Base, Req, Opts) ->
    generate_block(Base, Req, Opts);
default(_, Base, Req, Opts) ->
    validate_tx(Base, Req, Opts).

init(_Base, Req, Opts) ->
    RawBalances = read_any([<<"balances">>], Req, Req, #{}, Opts),
    {
        ok,
        #{
            <<"height">> => read_int([<<"height">>], Req, Req, 0, Opts),
            <<"balances">> => normalize_balances(RawBalances, Opts),
            <<"pending-reward">> => 0,
            <<"tx-history">> => []
        }
    }.

balance(Base, Req, Opts) ->
    State = ensure_state(read_state(Base, Req, Opts), Opts),
    Account = read_any([<<"account">>, <<"address">>], Base, Req, <<>>, Opts),
    {ok, get_balance(State, addr_key(Account), Opts)}.

validate_tx(Base, Req, Opts) ->
    State = ensure_state(read_state(Base, Req, Opts), Opts),
    TXInput = read_any([<<"tx">>], Base, Req, Req, Opts),
    case validate_tx_with_state(State, TXInput, Opts) of
        {ok, Validation} ->
            {ok, Validation};
        {error, Reason} ->
            {ok, #{<<"valid">> => false, <<"error">> => error_to_binary(Reason)}}
    end.

apply_tx(Base, Req, Opts) ->
    State = ensure_state(read_state(Base, Req, Opts), Opts),
    TXInput = read_any([<<"tx">>], Base, Req, Req, Opts),
    case validate_tx_with_state(State, TXInput, Opts) of
        {error, Reason} ->
            {error, Reason};
        {ok, Validation = #{<<"valid">> := true, <<"tx-record">> := TX}} ->
            {ok, apply_tx_to_state(State, TX, Validation, Opts)}
    end.

validate_block(Base, Req, Opts) ->
    State = ensure_state(read_state(Base, Req, Opts), Opts),
    Block = read_any([<<"block">>], Base, Req, Req, Opts),
    case validate_block_with_state(State, Block, Opts) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {ok, #{<<"valid">> => false, <<"error">> => error_to_binary(Reason)}}
    end.

apply_block(Base, Req, Opts) ->
    State = ensure_state(read_state(Base, Req, Opts), Opts),
    Block = read_any([<<"block">>], Base, Req, Req, Opts),
    case validate_block_with_state(State, Block, Opts) of
        {error, Reason} ->
            {error, Reason};
        {ok,
            #{
                <<"valid">> := true,
                <<"state-after">> := InterimState,
                <<"height">> := Height,
                <<"hash-raw">> := HashRaw,
                <<"tx-root-raw">> := TXRootRaw
            }} ->
            RewardAddrRaw =
                read_binary_optional(
                    [<<"reward-addr">>, <<"reward_addr">>],
                    Block,
                    Block,
                    Opts
                ),
            PendingReward = hb_maps:get(<<"pending-reward">>, InterimState, 0, Opts),
            InterimBalances = hb_maps:get(<<"balances">>, InterimState, #{}, Opts),
            FinalBalances =
                case RewardAddrRaw of
                    {ok, RewardAddr} when PendingReward > 0 ->
                        update_balance(
                            addr_key(RewardAddr),
                            PendingReward,
                            InterimBalances,
                            Opts
                        );
                    _ ->
                        InterimBalances
                end,
            {
                ok,
                InterimState#{
                    <<"balances">> => FinalBalances,
                    <<"pending-reward">> => 0,
                    <<"height">> => Height,
                    <<"last-block-hash">> => hb_util:encode(HashRaw),
                    <<"last-block">> =>
                        Block#{
                            <<"hash">> => hb_util:encode(HashRaw),
                            <<"tx-root">> => hb_util:encode(TXRootRaw),
                            <<"height">> => Height
                        }
                }
            }
    end.

generate_block(Base, Req, Opts) ->
    State = ensure_state(read_state(Base, Req, Opts), Opts),
    TXInputs = read_any([<<"txs">>], Base, Req, [], Opts),
    case apply_txs(State, TXInputs, Opts) of
        {error, Reason} ->
            {error, Reason};
        {ok, InterimState, TXRecords, _TXIDs} ->
            Height = hb_maps:get(<<"height">>, State, 0, Opts) + 1,
            TXRootRaw = compute_tx_root(TXRecords, Height),
            PrevHashRaw =
                read_binary_optional(
                    [<<"last-block-hash">>],
                    State,
                    State,
                    Opts
                ),
            PrevHash =
                case PrevHashRaw of
                    {ok, B} -> B;
                    error -> <<>>
                end,
            Timestamp = read_int([<<"timestamp">>], Base, Req, os:system_time(second), Opts),
            HashRaw = compute_block_hash(PrevHash, Height, Timestamp, TXRootRaw, TXRecords),
            RewardAddr =
                read_any([<<"reward-addr">>, <<"reward_addr">>], Base, Req, <<>>, Opts),
            {
                ok,
                #{
                    <<"height">> => Height,
                    <<"previous-block">> => hb_util:safe_encode(PrevHash),
                    <<"timestamp">> => Timestamp,
                    <<"tx-root">> => hb_util:encode(TXRootRaw),
                    <<"hash">> => hb_util:encode(HashRaw),
                    <<"reward-addr">> => RewardAddr,
                    <<"miner-reward">> => hb_maps:get(<<"pending-reward">>, InterimState, 0, Opts),
                    <<"txs">> =>
                        lists:map(
                            fun(TX) ->
                                hb_message:convert(
                                    TX,
                                    <<"structured@1.0">>,
                                    <<"tx@1.0">>,
                                    Opts
                                )
                            end,
                            TXRecords
                        )
                }
            }
    end.

validate_block_with_state(State, Block, Opts) ->
    TXInputs = read_any([<<"txs">>], Block, Block, [], Opts),
    Height = read_int([<<"height">>], Block, Block, hb_maps:get(<<"height">>, State, 0, Opts) + 1, Opts),
    ExpectedHeight = hb_maps:get(<<"height">>, State, 0, Opts) + 1,
    case Height =:= ExpectedHeight of
        false ->
            {error, invalid_height};
        true ->
            case apply_txs(State, TXInputs, Opts) of
                {error, Reason} ->
                    {error, Reason};
                {ok, InterimState, TXRecords, _TXIDs} ->
                    TXRootRaw = compute_tx_root(TXRecords, Height),
                    case verify_tx_root(Block, TXRootRaw, Opts) of
                        false ->
                            {error, invalid_tx_root};
                        true ->
                            Timestamp = read_int([<<"timestamp">>], Block, Block, 0, Opts),
                            BlockPrevHash =
                                read_binary_optional(
                                    [<<"previous-block">>, <<"previous_block">>],
                                    Block,
                                    Block,
                                    Opts
                                ),
                            StatePrevHash =
                                read_binary_optional(
                                    [<<"last-block-hash">>],
                                    State,
                                    State,
                                    Opts
                                ),
                            case {StatePrevHash, BlockPrevHash} of
                                {{ok, S}, {ok, B}} when S =/= B ->
                                    {error, invalid_previous_block};
                                _ ->
                                    PrevHashRaw =
                                        case BlockPrevHash of
                                            {ok, H} -> H;
                                            error ->
                                                case StatePrevHash of
                                                    {ok, H2} -> H2;
                                                    error -> <<>>
                                                end
                                        end,
                                    HashRaw = compute_block_hash(
                                        PrevHashRaw,
                                        Height,
                                        Timestamp,
                                        TXRootRaw,
                                        TXRecords
                                    ),
                                    case verify_block_hash(Block, HashRaw, Opts) of
                                        {true, VerifiedHashRaw} ->
                                            {ok,
                                                #{
                                                    <<"valid">> => true,
                                                    <<"height">> => Height,
                                                    <<"tx-root-raw">> => TXRootRaw,
                                                    <<"hash-raw">> => VerifiedHashRaw,
                                                    <<"block-hash-valid">> => true,
                                                    <<"state-after">> => InterimState
                                                }
                                            };
                                        false ->
                                            {error, invalid_block_hash}
                                    end
                            end
                    end
            end
    end.

apply_txs(State, TXInputs, Opts) ->
    lists:foldl(
        fun(TXInput, {ok, CurState, TXRecordsAcc, TXIDsAcc}) ->
                case validate_tx_with_state(CurState, TXInput, Opts) of
                    {error, Reason} ->
                        {error, Reason};
                    {ok, Validation = #{<<"valid">> := true, <<"tx-record">> := TXRecord}} ->
                        NextState = apply_tx_to_state(CurState, TXRecord, Validation, Opts),
                        TXIDRaw = hb_maps:get(<<"tx-id-raw">>, Validation, <<>>, Opts),
                        {
                            ok,
                            NextState,
                            TXRecordsAcc ++ [TXRecord],
                            TXIDsAcc ++ [TXIDRaw]
                        }
                end;
           (_, Error = {error, _}) ->
                Error
        end,
        {ok, State, [], []},
        TXInputs
    ).

validate_tx_with_state(State, TXInput, Opts) ->
    case to_tx_record(TXInput, Opts) of
        {error, Reason} ->
            {error, Reason};
        {ok, TX} ->
            case ar_tx:verify(TX) of
                false ->
                    {error, invalid_tx};
                true ->
                    Quantity = TX#tx.quantity,
                    Reward = TX#tx.reward,
                    SenderRaw = ar_wallet:to_address(TX#tx.owner, TX#tx.signature_type),
                    Sender = addr_key(SenderRaw),
                    Recipient =
                        case TX#tx.target of
                            <<>> -> <<>>;
                            Target -> addr_key(Target)
                        end,
                    Spend = Quantity + Reward,
                    SenderBalance = get_balance(State, Sender, Opts),
                    TXIDRaw = tx_id_raw(TX),
                    TXID = hb_util:encode(TXIDRaw),
                    case Quantity < 0 orelse Reward < 0 of
                        true ->
                            {error, invalid_tx_amounts};
                        false ->
                            case history_has_tx(State, TXID, Opts) of
                                true ->
                                    {error, tx_already_seen};
                                false ->
                                    case SenderBalance >= Spend of
                                        false ->
                                            {error, insufficient_balance};
                                        true ->
                                            {
                                                ok,
                                                #{
                                                    <<"valid">> => true,
                                                    <<"sender">> => Sender,
                                                    <<"recipient">> => Recipient,
                                                    <<"quantity">> => Quantity,
                                                    <<"reward">> => Reward,
                                                    <<"spend">> => Spend,
                                                    <<"tx-id">> => TXID,
                                                    <<"tx-id-raw">> => TXIDRaw,
                                                    <<"tx-record">> => TX
                                                }
                                            }
                                    end
                            end
                    end
            end
    end.

apply_tx_to_state(State, TX, Validation, Opts) ->
    Sender = hb_maps:get(<<"sender">>, Validation, <<>>, Opts),
    Recipient = hb_maps:get(<<"recipient">>, Validation, <<>>, Opts),
    Spend = hb_maps:get(<<"spend">>, Validation, 0, Opts),
    Quantity = hb_maps:get(<<"quantity">>, Validation, 0, Opts),
    Reward = hb_maps:get(<<"reward">>, Validation, 0, Opts),
    TXID = hb_maps:get(<<"tx-id">>, Validation, <<>>, Opts),
    Balances0 = hb_maps:get(<<"balances">>, State, #{}, Opts),
    Balances1 = update_balance(Sender, -Spend, Balances0, Opts),
    Balances2 =
        case Recipient of
            <<>> -> Balances1;
            _ -> update_balance(Recipient, Quantity, Balances1, Opts)
        end,
    PendingReward = hb_maps:get(<<"pending-reward">>, State, 0, Opts),
    History = hb_maps:get(<<"tx-history">>, State, [], Opts),
    State#{
        <<"balances">> => Balances2,
        <<"pending-reward">> => PendingReward + Reward,
        <<"tx-history">> => [TXID | History],
        <<"last-tx-record">> => TX
    }.

normalize_balances(RawBalances, Opts) ->
    hb_maps:fold(
        fun(Address, Balance, Acc) ->
            case hb_util:safe_int(Balance) of
                {ok, I} -> hb_maps:put(addr_key(Address), I, Acc, Opts);
                _ -> Acc
            end
        end,
        #{},
        RawBalances,
        Opts
    ).

ensure_state(State, Opts) ->
    State#{
        <<"height">> => hb_maps:get(<<"height">>, State, 0, Opts),
        <<"balances">> => hb_maps:get(<<"balances">>, State, #{}, Opts),
        <<"pending-reward">> => hb_maps:get(<<"pending-reward">>, State, 0, Opts),
        <<"tx-history">> => hb_maps:get(<<"tx-history">>, State, [], Opts)
    }.

read_state(Base, Req, Opts) ->
    Candidate = read_any([<<"state">>], Base, Req, Base, Opts),
    case is_map(Candidate) of
        true -> Candidate;
        false -> #{}
    end.

read_any(Keys, Base, Req, Default, Opts) ->
    read_any_local(Keys, Req, Base, Default, Opts).

read_any_local([], _Req, _Base, Default, _Opts) ->
    Default;
read_any_local([Key | Rest], Req, Base, Default, Opts) ->
    case hb_maps:find(Key, Req, Opts) of
        {ok, Value} ->
            Value;
        error ->
            case is_map(Base) of
                true ->
                    case hb_maps:find(Key, Base, Opts) of
                        {ok, Value2} -> Value2;
                        error -> read_any_local(Rest, Req, Base, Default, Opts)
                    end;
                false ->
                    read_any_local(Rest, Req, Base, Default, Opts)
            end
    end.

read_int(Keys, Base, Req, Default, Opts) ->
    case hb_util:safe_int(read_any(Keys, Base, Req, Default, Opts)) of
        {ok, I} -> I;
        _ -> Default
    end.

read_binary_optional(Keys, Base, Req, Opts) ->
    Raw = read_any(Keys, Base, Req, not_found, Opts),
    case Raw of
        not_found ->
            error;
        B when is_binary(B) ->
            case hb_util:safe_decode(B) of
                {ok, Decoded} -> {ok, Decoded};
                _ -> {ok, B}
            end;
        _ ->
            error
    end.

to_tx_record(TX, _Opts) when is_record(TX, tx) ->
    {ok, TX};
to_tx_record(TX, Opts) when is_map(TX) ->
    try
        {ok, hb_message:convert(TX, <<"tx@1.0">>, Opts)}
    catch
        _:_ ->
            {error, invalid_tx}
    end;
to_tx_record(_, _) ->
    {error, invalid_tx}.

verify_tx_root(Block, TXRootRaw, Opts) ->
    case read_binary_optional([<<"tx-root">>, <<"tx_root">>], Block, Block, Opts) of
        {ok, DeclaredRoot} -> DeclaredRoot =:= TXRootRaw;
        error -> true
    end.

verify_block_hash(Block, LegacyHashRaw, Opts) ->
    case verify_full_block_hash(Block, Opts) of
        {ok, FullHashRaw} ->
            {true, FullHashRaw};
        {error, not_applicable} ->
            case verify_legacy_hash(Block, LegacyHashRaw, Opts) of
                true -> {true, LegacyHashRaw};
                false -> false
            end;
        {error, _Reason} ->
            false
    end.

verify_legacy_hash(Block, HashRaw, Opts) ->
    case read_binary_optional([<<"hash">>], Block, Block, Opts) of
        {ok, DeclaredHash} -> DeclaredHash =:= HashRaw;
        error -> true
    end.

verify_full_block_hash(Block, Opts) ->
    Height = read_int([<<"height">>], Block, Block, 0, Opts),
    case full_hash_applicable(Block, Height, Opts) of
        false ->
            {error, not_applicable};
        true ->
            case {
                read_binary_optional([<<"indep_hash">>, <<"indep-hash">>], Block, Block, Opts),
                read_binary_optional([<<"signature">>], Block, Block, Opts)
            } of
                {{ok, DeclaredIndepHash}, {ok, Signature}} ->
                    case generate_signed_hash(Block, Opts) of
                        {ok, SignedHash} ->
                            PrevCDiff =
                                read_int(
                                    [<<"previous_cumulative_diff">>, <<"previous-cumulative-diff">>],
                                    Block,
                                    Block,
                                    0,
                                    Opts
                                ),
                            case verify_block_signature(
                                SignedHash,
                                PrevCDiff,
                                Signature,
                                Block,
                                Height,
                                Opts
                            ) of
                                true ->
                                    ComputedIndepHash = indep_hash2(SignedHash, Signature),
                                    case ComputedIndepHash =:= DeclaredIndepHash of
                                        true -> {ok, ComputedIndepHash};
                                        false -> {error, indep_hash_mismatch}
                                    end;
                                false ->
                                    {error, invalid_block_signature}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                _ ->
                    {error, missing_full_hash_fields}
            end
    end.

full_hash_applicable(Block, Height, Opts) ->
    Height >= ?AR_FORK_2_6_HEIGHT
        andalso has_any_key([<<"indep_hash">>, <<"indep-hash">>], Block, Opts)
        andalso has_any_key([<<"signature">>], Block, Opts)
        andalso has_any_key([<<"reward_key">>, <<"reward-key">>], Block, Opts).

has_any_key([], _Map, _Opts) ->
    false;
has_any_key(_Keys, Map, _Opts) when not is_map(Map) ->
    false;
has_any_key([Key | Rest], Map, Opts) ->
    case hb_maps:find(Key, Map, Opts) of
        {ok, _} -> true;
        error -> has_any_key(Rest, Map, Opts)
    end.

verify_block_signature(SignedHash, PrevCDiff, Signature, Block, Height, Opts) ->
    case {
        read_binary_optional([<<"reward_key">>, <<"reward-key">>], Block, Block, Opts),
        read_binary_optional([<<"reward_addr">>, <<"reward-addr">>], Block, Block, Opts),
        read_binary_optional(
            [<<"previous_solution_hash">>, <<"previous-solution-hash">>],
            Block,
            Block,
            Opts
        )
    } of
        {{ok, RewardKey}, {ok, RewardAddr}, {ok, PrevSolutionHash}} ->
            CDiff = read_int([<<"cumulative_diff">>, <<"cumulative-diff">>], Block, Block, 0, Opts),
            SignaturePreimage =
                get_block_signature_preimage(
                    CDiff,
                    PrevCDiff,
                    <<PrevSolutionHash/binary, SignedHash/binary>>,
                    Height
                ),
            case byte_size(RewardKey) of
                ?RSA_BLOCK_PUB_SIZE when byte_size(Signature) =:= ?RSA_BLOCK_SIG_SIZE ->
                    ar_wallet:to_address(RewardKey, {rsa, 65537}) =:= RewardAddr
                        andalso ar_wallet:verify(
                            {{rsa, 65537}, RewardKey},
                            SignaturePreimage,
                            Signature
                        );
                ?ECDSA_BLOCK_PUB_SIZE ->
                    ar_wallet:to_address(RewardKey, {ecdsa, secp256k1}) =:= RewardAddr
                        andalso ar_wallet:verify(
                            {{ecdsa, secp256k1}, RewardKey},
                            SignaturePreimage,
                            Signature
                        );
                _ ->
                    false
            end;
        _ ->
            false
    end.

get_block_signature_preimage(CDiff, PrevCDiff, Preimage, Height) ->
    EncodedCDiff = encode_int(CDiff, 16),
    EncodedPrevCDiff = encode_int(PrevCDiff, 16),
    SignaturePreimage = <<EncodedCDiff/binary, EncodedPrevCDiff/binary, Preimage/binary>>,
    case Height >= ?AR_FORK_2_9_HEIGHT of
        true -> <<0:(32 * 8), SignaturePreimage/binary>>;
        false -> SignaturePreimage
    end.

indep_hash2(SignedHash, Signature) ->
    crypto:hash(sha384, <<SignedHash:32/binary, Signature/binary>>).

generate_signed_hash(Block, Opts) ->
    try
        PrevH = read_binary([<<"previous_block">>, <<"previous-block">>], Block, <<>>, Opts),
        TS = read_int([<<"timestamp">>], Block, Block, 0, Opts),
        Nonce = read_nonce(Block, Opts),
        Height = read_int([<<"height">>], Block, Block, 0, Opts),
        Diff = read_int([<<"diff">>], Block, Block, 0, Opts),
        CDiff = read_int([<<"cumulative_diff">>, <<"cumulative-diff">>], Block, Block, 0, Opts),
        LastRetarget = read_int([<<"last_retarget">>, <<"last-retarget">>], Block, Block, 0, Opts),
        Hash = read_binary([<<"hash">>], Block, <<>>, Opts),
        BlockSize = read_int([<<"block_size">>, <<"block-size">>], Block, Block, 0, Opts),
        WeaveSize = read_int([<<"weave_size">>, <<"weave-size">>], Block, Block, 0, Opts),
        TXRoot = read_binary([<<"tx_root">>, <<"tx-root">>], Block, <<>>, Opts),
        WalletList = read_binary([<<"wallet_list">>, <<"wallet-list">>], Block, <<>>, Opts),
        HashListMerkle =
            read_binary(
                [<<"hash_list_merkle">>, <<"hash-list-merkle">>],
                Block,
                <<>>,
                Opts
            ),
        RewardPool = read_int([<<"reward_pool">>, <<"reward-pool">>], Block, Block, 0, Opts),
        Packing_2_5_Threshold =
            read_int(
                [<<"packing_2_5_threshold">>, <<"packing-2-5-threshold">>],
                Block,
                Block,
                0,
                Opts
            ),
        RewardAddrRaw = read_binary([<<"reward_addr">>, <<"reward-addr">>], Block, <<>>, Opts),
        Addr2 =
            case RewardAddrRaw of
                <<"unclaimed">> -> <<>>;
                _ -> RewardAddrRaw
            end,
        RewardKey2 =
            read_binary_or_undefined(
                [<<"reward_key">>, <<"reward-key">>],
                Block,
                Opts
            ),
        StrictChunkThreshold =
            read_int(
                [<<"strict_data_split_threshold">>, <<"strict-data-split-threshold">>],
                Block,
                Block,
                0,
                Opts
            ),
        {RateDividend, RateDivisor} =
            read_int_pair([<<"usd_to_ar_rate">>, <<"usd-to-ar-rate">>], Block, {0, 0}, Opts),
        {ScheduledRateDividend, ScheduledRateDivisor} =
            read_int_pair(
                [<<"scheduled_usd_to_ar_rate">>, <<"scheduled-usd-to-ar-rate">>],
                Block,
                {0, 0},
                Opts
            ),
        Tags =
            normalize_binary_list(
                read_any([<<"tags">>], Block, Block, [], Opts)
            ),
        TXs = read_any([<<"txs">>], Block, Block, [], Opts),
        {ok, TXIDs} = extract_block_tx_ids(TXs, Opts),
        Reward = read_int([<<"reward">>], Block, Block, 0, Opts),
        HashPreimage = read_binary([<<"hash_preimage">>, <<"hash-preimage">>], Block, <<>>, Opts),
        RecallByte = read_int([<<"recall_byte">>, <<"recall-byte">>], Block, Block, 0, Opts),
        PartitionNumber =
            read_int(
                [<<"partition_number">>, <<"partition-number">>],
                Block,
                Block,
                0,
                Opts
            ),
        RecallByte2 =
            read_int(
                [<<"recall_byte2">>, <<"recall-byte2">>],
                Block,
                Block,
                0,
                Opts
            ),
        NonceLimiterInfo =
            read_any(
                [<<"nonce_limiter_info">>, <<"nonce-limiter-info">>],
                Block,
                Block,
                #{},
                Opts
            ),
        Output = read_binary([<<"output">>], NonceLimiterInfo, <<>>, Opts),
        N =
            read_int(
                [<<"global_step_number">>, <<"global-step-number">>],
                NonceLimiterInfo,
                NonceLimiterInfo,
                1,
                Opts
            ),
        Seed = read_binary([<<"seed">>], NonceLimiterInfo, <<>>, Opts),
        NextSeed = read_binary([<<"next_seed">>, <<"next-seed">>], NonceLimiterInfo, <<>>, Opts),
        PartitionUpperBound =
            read_int(
                [
                    <<"partition_upper_bound">>,
                    <<"partition-upper-bound">>,
                    <<"zone_upper_bound">>,
                    <<"zone-upper-bound">>
                ],
                NonceLimiterInfo,
                NonceLimiterInfo,
                0,
                Opts
            ),
        NextPartitionUpperBound =
            read_int(
                [
                    <<"next_partition_upper_bound">>,
                    <<"next-partition-upper-bound">>,
                    <<"next_zone_upper_bound">>,
                    <<"next-zone-upper-bound">>
                ],
                NonceLimiterInfo,
                NonceLimiterInfo,
                0,
                Opts
            ),
        Steps =
            normalize_binary_list(
                read_any(
                    [<<"steps">>, <<"checkpoints">>],
                    NonceLimiterInfo,
                    NonceLimiterInfo,
                    [],
                    Opts
                )
            ),
        PrevOutput = read_binary([<<"prev_output">>, <<"prev-output">>], NonceLimiterInfo, <<>>, Opts),
        LastStepCheckpoints =
            normalize_binary_list(
                read_any(
                    [<<"last_step_checkpoints">>, <<"last-step-checkpoints">>],
                    NonceLimiterInfo,
                    NonceLimiterInfo,
                    [],
                    Opts
                )
            ),
        VDFDifficulty =
            read_int(
                [<<"vdf_difficulty">>, <<"vdf-difficulty">>],
                NonceLimiterInfo,
                NonceLimiterInfo,
                0,
                Opts
            ),
        NextVDFDifficulty =
            read_int(
                [<<"next_vdf_difficulty">>, <<"next-vdf-difficulty">>],
                NonceLimiterInfo,
                NonceLimiterInfo,
                0,
                Opts
            ),
        PreviousSolutionHash =
            read_binary(
                [<<"previous_solution_hash">>, <<"previous-solution-hash">>],
                Block,
                <<>>,
                Opts
            ),
        PricePerGiBMinute =
            read_int(
                [<<"price_per_gib_minute">>, <<"price-per-gib-minute">>],
                Block,
                Block,
                0,
                Opts
            ),
        ScheduledPricePerGiBMinute =
            read_int(
                [<<"scheduled_price_per_gib_minute">>, <<"scheduled-price-per-gib-minute">>],
                Block,
                Block,
                0,
                Opts
            ),
        RewardHistoryHash =
            read_binary([<<"reward_history_hash">>, <<"reward-history-hash">>], Block, <<>>, Opts),
        BlockTimeHistoryHash =
            read_binary(
                [<<"block_time_history_hash">>, <<"block-time-history-hash">>],
                Block,
                <<>>,
                Opts
            ),
        DebtSupply = read_int([<<"debt_supply">>, <<"debt-supply">>], Block, Block, 0, Opts),
        KryderPlusRateMultiplier =
            read_int(
                [<<"kryder_plus_rate_multiplier">>, <<"kryder-plus-rate-multiplier">>],
                Block,
                Block,
                0,
                Opts
            ),
        KryderPlusRateMultiplierLatch =
            read_int(
                [
                    <<"kryder_plus_rate_multiplier_latch">>,
                    <<"kryder-plus-rate-multiplier-latch">>
                ],
                Block,
                Block,
                0,
                Opts
            ),
        Denomination = read_int([<<"denomination">>], Block, Block, 0, Opts),
        RedenominationHeight =
            read_int(
                [<<"redenomination_height">>, <<"redenomination-height">>],
                Block,
                Block,
                0,
                Opts
            ),
        DoubleSigningProof =
            parse_double_signing_proof(
                read_any(
                    [<<"double_signing_proof">>, <<"double-signing-proof">>],
                    Block,
                    Block,
                    undefined,
                    Opts
                ),
                Height,
                Opts
            ),
        PrevCDiff =
            read_int(
                [<<"previous_cumulative_diff">>, <<"previous-cumulative-diff">>],
                Block,
                Block,
                0,
                Opts
            ),
        RebaseThreshold =
            read_int(
                [<<"merkle_rebase_support_threshold">>, <<"merkle-rebase-support-threshold">>],
                Block,
                Block,
                0,
                Opts
            ),
        PoA = read_any([<<"poa">>], Block, Block, #{}, Opts),
        DataPath = read_binary([<<"data_path">>, <<"data-path">>], PoA, <<>>, Opts),
        TXPath = read_binary([<<"tx_path">>, <<"tx-path">>], PoA, <<>>, Opts),
        PoA2 = read_any([<<"poa2">>], Block, Block, #{}, Opts),
        DataPath2 = read_binary([<<"data_path">>, <<"data-path">>], PoA2, <<>>, Opts),
        TXPath2 = read_binary([<<"tx_path">>, <<"tx-path">>], PoA2, <<>>, Opts),
        ChunkHash = read_binary([<<"chunk_hash">>, <<"chunk-hash">>], Block, <<>>, Opts),
        Chunk2Hash = read_binary([<<"chunk2_hash">>, <<"chunk2-hash">>], Block, undefined, Opts),
        PackingDifficulty =
            read_int(
                [<<"packing_difficulty">>, <<"packing-difficulty">>],
                Block,
                Block,
                0,
                Opts
            ),
        UnpackedChunkHash =
            read_binary_or_undefined(
                [<<"unpacked_chunk_hash">>, <<"unpacked-chunk-hash">>],
                Block,
                Opts
            ),
        UnpackedChunk2Hash =
            read_binary_or_undefined(
                [<<"unpacked_chunk2_hash">>, <<"unpacked-chunk2-hash">>],
                Block,
                Opts
            ),
        ReplicaFormat =
            read_int(
                [<<"replica_format">>, <<"replica-format">>],
                Block,
                Block,
                0,
                Opts
            ),
        Nonce2 = binary:encode_unsigned(Nonce),
        {RebaseThresholdBin, DataPathBin, TXPathBin, DataPath2Bin, TXPath2Bin,
                ChunkHashBin, Chunk2HashBin, BlockTimeHistoryHashBin,
                VDFDifficultyBin, NextVDFDifficultyBin} =
            case Height >= ?AR_FORK_2_7_HEIGHT of
                true ->
                    {
                        encode_int(RebaseThreshold, 16),
                        encode_bin(DataPath, 24),
                        encode_bin(TXPath, 24),
                        encode_bin(DataPath2, 24),
                        encode_bin(TXPath2, 24),
                        <<ChunkHash:32/binary>>,
                        encode_bin(Chunk2Hash, 8),
                        <<BlockTimeHistoryHash:32/binary>>,
                        encode_int(VDFDifficulty, 8),
                        encode_int(NextVDFDifficulty, 8)
                    };
                false ->
                    {<<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>, <<>>}
            end,
        {PackingDifficultyBin, UnpackedChunkHashBin, UnpackedChunk2HashBin} =
            case Height >= ?AR_FORK_2_8_HEIGHT of
                true ->
                    {
                        <<PackingDifficulty:8>>,
                        encode_bin(UnpackedChunkHash, 8),
                        encode_bin(UnpackedChunk2Hash, 8)
                    };
                false ->
                    {<<>>, <<>>, <<>>}
            end,
        ReplicaFormatBin =
            case Height >= ?AR_FORK_2_9_HEIGHT of
                true ->
                    <<ReplicaFormat:8>>;
                false ->
                    <<>>
            end,
        Segment =
            <<
                (encode_bin(PrevH, 8))/binary,
                (encode_int(TS, 8))/binary,
                (encode_bin(Nonce2, 16))/binary,
                (encode_int(Height, 8))/binary,
                (encode_int(Diff, 16))/binary,
                (encode_int(CDiff, 16))/binary,
                (encode_int(LastRetarget, 8))/binary,
                (encode_bin(Hash, 8))/binary,
                (encode_int(BlockSize, 16))/binary,
                (encode_int(WeaveSize, 16))/binary,
                (encode_bin(Addr2, 8))/binary,
                (encode_bin(TXRoot, 8))/binary,
                (encode_bin(WalletList, 8))/binary,
                (encode_bin(HashListMerkle, 8))/binary,
                (encode_int(RewardPool, 8))/binary,
                (encode_int(Packing_2_5_Threshold, 8))/binary,
                (encode_int(StrictChunkThreshold, 8))/binary,
                (encode_int(RateDividend, 8))/binary,
                (encode_int(RateDivisor, 8))/binary,
                (encode_int(ScheduledRateDividend, 8))/binary,
                (encode_int(ScheduledRateDivisor, 8))/binary,
                (encode_bin_list(Tags, 16, 16))/binary,
                (encode_bin_list(TXIDs, 16, 8))/binary,
                (encode_int(Reward, 8))/binary,
                (encode_int(RecallByte, 16))/binary,
                (encode_bin(HashPreimage, 8))/binary,
                (encode_int(RecallByte2, 16))/binary,
                (encode_bin(RewardKey2, 16))/binary,
                (encode_int(PartitionNumber, 8))/binary,
                Output:32/binary,
                N:64,
                Seed:48/binary,
                NextSeed:48/binary,
                PartitionUpperBound:256,
                NextPartitionUpperBound:256,
                (encode_bin(PrevOutput, 8))/binary,
                (length(Steps)):16,
                (iolist_to_binary(Steps))/binary,
                (length(LastStepCheckpoints)):16,
                (iolist_to_binary(LastStepCheckpoints))/binary,
                (encode_bin(PreviousSolutionHash, 8))/binary,
                (encode_int(PricePerGiBMinute, 8))/binary,
                (encode_int(ScheduledPricePerGiBMinute, 8))/binary,
                RewardHistoryHash:32/binary,
                (encode_int(DebtSupply, 8))/binary,
                KryderPlusRateMultiplier:24,
                KryderPlusRateMultiplierLatch:8,
                Denomination:24,
                (encode_int(RedenominationHeight, 8))/binary,
                (encode_double_signing_proof(DoubleSigningProof, Height))/binary,
                (encode_int(PrevCDiff, 16))/binary,
                RebaseThresholdBin/binary,
                DataPathBin/binary,
                TXPathBin/binary,
                DataPath2Bin/binary,
                TXPath2Bin/binary,
                ChunkHashBin/binary,
                Chunk2HashBin/binary,
                BlockTimeHistoryHashBin/binary,
                VDFDifficultyBin/binary,
                NextVDFDifficultyBin/binary,
                PackingDifficultyBin/binary,
                UnpackedChunkHashBin/binary,
                UnpackedChunk2HashBin/binary,
                ReplicaFormatBin/binary
            >>,
        {ok, crypto:hash(sha256, Segment)}
    catch
        _:_ ->
            {error, invalid_block_hash_preimage}
    end.

read_binary(Keys, Base, Default, Opts) ->
    case read_binary_optional(Keys, Base, Base, Opts) of
        {ok, Bin} -> Bin;
        error -> Default
    end.

read_binary_or_undefined(Keys, Base, Opts) ->
    case read_binary_optional(Keys, Base, Base, Opts) of
        {ok, Bin} -> Bin;
        error -> undefined
    end.

read_int_pair(Keys, Base, Default, Opts) ->
    Value = read_any(Keys, Base, Base, not_found, Opts),
    case Value of
        [A, B] ->
            {safe_int_or_default(A, element(1, Default)), safe_int_or_default(B, element(2, Default))};
        {A, B} ->
            {safe_int_or_default(A, element(1, Default)), safe_int_or_default(B, element(2, Default))};
        _ ->
            Default
    end.

safe_int_or_default(Value, Default) ->
    case hb_util:safe_int(Value) of
        {ok, I} -> I;
        _ -> Default
    end.

read_nonce(Block, Opts) ->
    NonceRaw = read_any([<<"nonce">>], Block, Block, 0, Opts),
    case hb_util:safe_int(NonceRaw) of
        {ok, NonceInt} ->
            NonceInt;
        _ ->
            case NonceRaw of
                Bin when is_binary(Bin) ->
                    case hb_util:safe_decode(Bin) of
                        {ok, NonceBin} -> binary:decode_unsigned(NonceBin);
                        _ -> 0
                    end;
                _ ->
                    0
            end
    end.

normalize_binary_list(List) when is_list(List) ->
    lists:map(fun normalize_binary_item/1, List);
normalize_binary_list(_) ->
    [].

normalize_binary_item(Bin) when is_binary(Bin) ->
    case hb_util:safe_decode(Bin) of
        {ok, Decoded} -> Decoded;
        _ -> Bin
    end;
normalize_binary_item(Item) ->
    hb_util:bin(Item).

extract_block_tx_ids(TXs, Opts) when is_list(TXs) ->
    case lists:foldl(
        fun(TX, {ok, Acc}) ->
                case tx_id_from_block_item(TX, Opts) of
                    {ok, TXID} -> {ok, [TXID | Acc]};
                    Error -> Error
                end;
           (_, Error = {error, _}) ->
                Error
        end,
        {ok, []},
        TXs
    ) of
        {ok, IDsRev} -> {ok, lists:reverse(IDsRev)};
        Error -> Error
    end;
extract_block_tx_ids(_, _) ->
    {ok, []}.

tx_id_from_block_item(TXID, _Opts) when is_binary(TXID) ->
    case hb_util:safe_decode(TXID) of
        {ok, Decoded} -> {ok, Decoded};
        _ -> {ok, TXID}
    end;
tx_id_from_block_item(TX, _Opts) when is_record(TX, tx) ->
    {ok, tx_id_raw(TX)};
tx_id_from_block_item(TX, Opts) when is_map(TX) ->
    case read_binary_optional([<<"id">>, <<"tx-id">>, <<"tx_id">>], TX, TX, Opts) of
        {ok, TXID} ->
            {ok, TXID};
        error ->
            case to_tx_record(TX, Opts) of
                {ok, TXRecord} -> {ok, tx_id_raw(TXRecord)};
                _ -> {error, invalid_block_tx_item}
            end
    end;
tx_id_from_block_item(_, _) ->
    {error, invalid_block_tx_item}.

parse_double_signing_proof(undefined, _Height, _Opts) ->
    undefined;
parse_double_signing_proof(not_found, _Height, _Opts) ->
    undefined;
parse_double_signing_proof(<<>>, _Height, _Opts) ->
    undefined;
parse_double_signing_proof(Proof, _Height, _Opts) when Proof =:= [] ->
    undefined;
parse_double_signing_proof(Proof, _Height, _Opts) when Proof =:= null ->
    undefined;
parse_double_signing_proof(Proof, _Height, _Opts) when is_map(Proof), map_size(Proof) =:= 0 ->
    undefined;
parse_double_signing_proof(Proof, _Height, Opts) when is_map(Proof) ->
    case has_any_key([<<"pub_key">>, <<"pub-key">>], Proof, Opts) of
        false ->
            undefined;
        true ->
            {
                read_binary([<<"pub_key">>, <<"pub-key">>], Proof, <<>>, Opts),
                read_binary([<<"sig1">>], Proof, <<>>, Opts),
                read_int([<<"cdiff1">>], Proof, Proof, 0, Opts),
                read_int([<<"prev_cdiff1">>, <<"prev-cdiff1">>], Proof, Proof, 0, Opts),
                read_binary([<<"preimage1">>], Proof, <<>>, Opts),
                read_binary([<<"sig2">>], Proof, <<>>, Opts),
                read_int([<<"cdiff2">>], Proof, Proof, 0, Opts),
                read_int([<<"prev_cdiff2">>, <<"prev-cdiff2">>], Proof, Proof, 0, Opts),
                read_binary([<<"preimage2">>], Proof, <<>>, Opts)
            }
    end;
parse_double_signing_proof(_, _Height, _Opts) ->
    undefined.

encode_double_signing_proof(undefined, _Height) ->
    <<0:8>>;
encode_double_signing_proof(Proof, Height) ->
    {Key, Sig1, CDiff1, PrevCDiff1, Preimage1, Sig2, CDiff2, PrevCDiff2, Preimage2} = Proof,
    case Height >= ?AR_FORK_2_9_HEIGHT of
        false ->
            <<
                1:8,
                Key:512/binary,
                Sig1:512/binary,
                (encode_int(CDiff1, 16))/binary,
                (encode_int(PrevCDiff1, 16))/binary,
                Preimage1:64/binary,
                Sig2:512/binary,
                (encode_int(CDiff2, 16))/binary,
                (encode_int(PrevCDiff2, 16))/binary,
                Preimage2:64/binary
            >>;
        true ->
            <<
                1:8,
                (encode_bin(Key, 16))/binary,
                (encode_bin(Sig1, 16))/binary,
                (encode_int(CDiff1, 16))/binary,
                (encode_int(PrevCDiff1, 16))/binary,
                Preimage1:64/binary,
                (encode_bin(Sig2, 16))/binary,
                (encode_int(CDiff2, 16))/binary,
                (encode_int(PrevCDiff2, 16))/binary,
                Preimage2:64/binary
            >>
    end.

encode_int(undefined, SizeBits) ->
    <<0:SizeBits>>;
encode_int(N, SizeBits) ->
    Bin = binary:encode_unsigned(N, big),
    <<(byte_size(Bin)):SizeBits, Bin/binary>>.

encode_bin(undefined, SizeBits) ->
    <<0:SizeBits>>;
encode_bin(Bin, SizeBits) when is_binary(Bin) ->
    <<(byte_size(Bin)):SizeBits, Bin/binary>>.

encode_bin_list(Bins, LenBits, ElemSizeBits) ->
    encode_bin_list(Bins, [], 0, LenBits, ElemSizeBits).

encode_bin_list([], Encoded, N, LenBits, _ElemSizeBits) ->
    <<N:LenBits, (iolist_to_binary(Encoded))/binary>>;
encode_bin_list([Bin | Bins], Encoded, N, LenBits, ElemSizeBits) ->
    Elem = encode_bin(Bin, ElemSizeBits),
    encode_bin_list(Bins, [Elem | Encoded], N + 1, LenBits, ElemSizeBits).

compute_tx_root([], _Height) ->
    <<>>;
compute_tx_root(TXs, Height) ->
    SizeTaggedTXs = generate_size_tagged_list_from_txs(TXs, Height),
    SizeTaggedDataRoots = [{Root, Offset} || {{_, Root}, Offset} <- SizeTaggedTXs],
    {Root, _Tree} = ar_merkle:generate_tree(SizeTaggedDataRoots),
    Root.

generate_size_tagged_list_from_txs(TXs, Height) ->
    lists:reverse(
        element(
            2,
            lists:foldl(
                fun(TX, {Pos, List}) ->
                    DataSize = tx_data_size(TX),
                    End = Pos + DataSize,
                    Entry = {{tx_id_for_root(TX), tx_data_root(TX)}, End},
                    case Height >= ?AR_FORK_2_5_HEIGHT of
                        true ->
                            Padding = weave_size_increase(DataSize, Height) - DataSize,
                            case Padding > 0 of
                                true ->
                                    {
                                        End + Padding,
                                        [
                                            {{padding, ?PADDING_NODE_DATA_ROOT}, End + Padding},
                                            Entry
                                            | List
                                        ]
                                    };
                                false ->
                                    {End, [Entry | List]}
                            end;
                        false ->
                            {End, [Entry | List]}
                    end
                end,
                {0, []},
                lists:sort(TXs)
            )
        )
    ).

tx_data_size(#tx{data_size = DataSize}) when is_integer(DataSize), DataSize >= 0 ->
    DataSize;
tx_data_size(_) ->
    0.

tx_data_root(#tx{format = 2, data_root = DataRoot}) when is_binary(DataRoot) ->
    DataRoot;
tx_data_root(#tx{data = Data}) when is_binary(Data) ->
    ar_tx:data_root(Data);
tx_data_root(_) ->
    ar_tx:data_root(<<>>).

tx_id_for_root(#tx{id = ID}) when is_binary(ID), byte_size(ID) > 0 ->
    ID;
tx_id_for_root(TX) ->
    tx_id_raw(TX).

weave_size_increase(0, _Height) ->
    0;
weave_size_increase(DataSize, _Height) ->
    padded_offset(DataSize, 0).

padded_offset(Offset, StrictDataSplitThreshold) ->
    Diff = Offset - StrictDataSplitThreshold,
    StrictDataSplitThreshold +
        (((Diff - 1) div ?AR_DATA_CHUNK_SIZE) + 1) * ?AR_DATA_CHUNK_SIZE.

compute_block_hash(PrevHashRaw, Height, Timestamp, TXRootRaw, TXRecords) ->
    TXIDs = lists:map(fun tx_id_raw/1, TXRecords),
    Payload = term_to_binary([PrevHashRaw, Height, Timestamp, TXRootRaw, TXIDs]),
    crypto:hash(sha256, Payload).

tx_id_raw(TX) ->
    case ar_tx:id(TX, signed) of
        not_signed -> ar_tx:id(TX, unsigned);
        ID -> ID
    end.

get_balance(State, Address, Opts) ->
    hb_maps:get(
        Address,
        hb_maps:get(<<"balances">>, State, #{}, Opts),
        0,
        Opts
    ).

update_balance(Address, Delta, Balances, Opts) ->
    Existing = hb_maps:get(Address, Balances, 0, Opts),
    hb_maps:put(Address, Existing + Delta, Balances, Opts).

history_has_tx(State, TXID, Opts) ->
    lists:member(TXID, hb_maps:get(<<"tx-history">>, State, [], Opts)).

addr_key(Address) when is_binary(Address), byte_size(Address) == 32 ->
    hb_util:encode(Address);
addr_key(Address) when is_binary(Address) ->
    Address;
addr_key(Address) ->
    hb_util:bin(Address).

error_to_binary(Reason) when is_atom(Reason) ->
    hb_util:bin(atom_to_binary(Reason));
error_to_binary(Reason) when is_binary(Reason) ->
    Reason;
error_to_binary(Reason) ->
    hb_util:bin(io_lib:format("~p", [Reason])).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

resolve_ledger(Path, Req, Opts) ->
    hb_ao:resolve(
        #{<<"device">> => dev_arweave_ledger},
        Req#{<<"path">> => Path},
        test_opts(Opts)
    ).

test_opts(Opts) ->
    case maps:is_key(store, Opts) of
        true -> Opts;
        false -> Opts#{store => [hb_test_utils:test_store()]}
    end.

test_signed_tx(Quantity, Reward) ->
    SenderWallet = ar_wallet:new(),
    RecipientWallet = ar_wallet:new(),
    Recipient = ar_wallet:to_address(RecipientWallet),
    TX0 =
        #tx{
            format = 2,
            target = Recipient,
            quantity = Quantity,
            reward = Reward,
            data = <<>>,
            data_size = 0
        },
    SignedTX = ar_tx:sign(TX0, SenderWallet),
    {SenderWallet, Recipient, SignedTX}.

base_state(SenderWallet, Balance) ->
    SenderAddr = ar_wallet:to_address(SenderWallet),
    #{
        <<"height">> => 0,
        <<"balances">> => #{addr_key(SenderAddr) => Balance},
        <<"pending-reward">> => 0,
        <<"tx-history">> => []
    }.

apply_tx_balance_transition_test() ->
    {SenderWallet, Recipient, SignedTX} = test_signed_tx(10, 2),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, State1} = resolve_ledger(<<"apply-tx">>, #{<<"state">> => State0, <<"tx">> => TXMsg}, #{}),
    SenderKey = addr_key(ar_wallet:to_address(SenderWallet)),
    RecipientKey = addr_key(Recipient),
    ?assertEqual(88, get_balance(State1, SenderKey, #{})),
    ?assertEqual(10, get_balance(State1, RecipientKey, #{})),
    ?assertEqual(2, hb_maps:get(<<"pending-reward">>, State1, 0, #{})).

reject_overspend_tx_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(10, 2),
    State0 = base_state(SenderWallet, 5),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Validation} =
        resolve_ledger(<<"validate-tx">>, #{<<"state">> => State0, <<"tx">> => TXMsg}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})).

generate_and_apply_block_test() ->
    {SenderWallet, Recipient, SignedTX} = test_signed_tx(7, 3),
    MinerWallet = ar_wallet:new(),
    MinerAddr = addr_key(ar_wallet:to_address(MinerWallet)),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => State0,
                <<"txs">> => [TXMsg],
                <<"reward-addr">> => MinerAddr,
                <<"timestamp">> => 1000
            },
            #{}
        ),
    {ok, Validation} =
        resolve_ledger(<<"validate-block">>, #{<<"state">> => State0, <<"block">> => Block}, #{}),
    ?assertEqual(true, hb_maps:get(<<"valid">>, Validation, false, #{})),
    {ok, State1} =
        resolve_ledger(<<"apply-block">>, #{<<"state">> => State0, <<"block">> => Block}, #{}),
    SenderKey = addr_key(ar_wallet:to_address(SenderWallet)),
    RecipientKey = addr_key(Recipient),
    ?assertEqual(90, get_balance(State1, SenderKey, #{})),
    ?assertEqual(7, get_balance(State1, RecipientKey, #{})),
    ?assertEqual(3, get_balance(State1, MinerAddr, #{})).

reject_invalid_tx_root_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(5, 1),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => State0,
                <<"txs">> => [TXMsg],
                <<"timestamp">> => 1000
            },
            #{}
        ),
    BrokenBlock = Block#{<<"tx-root">> => hb_util:encode(crypto:strong_rand_bytes(32))},
    {ok, Validation} =
        resolve_ledger(
            <<"validate-block">>,
            #{<<"state">> => State0, <<"block">> => BrokenBlock},
            #{}
        ),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})).

reject_replay_tx_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(5, 1),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, State1} = resolve_ledger(<<"apply-tx">>, #{<<"state">> => State0, <<"tx">> => TXMsg}, #{}),
    {ok, Validation} =
        resolve_ledger(<<"validate-tx">>, #{<<"state">> => State1, <<"tx">> => TXMsg}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})),
    ?assertEqual(<<"tx_already_seen">>, hb_maps:get(<<"error">>, Validation, <<>>, #{})).

reject_duplicate_tx_in_block_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(5, 1),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {error, tx_already_seen} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => State0,
                <<"txs">> => [TXMsg, TXMsg],
                <<"timestamp">> => 1000
            },
            #{}
        ).

reject_invalid_height_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(5, 1),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => State0,
                <<"txs">> => [TXMsg],
                <<"timestamp">> => 1000
            },
            #{}
        ),
    Broken = Block#{<<"height">> => 10},
    {ok, Validation} =
        resolve_ledger(<<"validate-block">>, #{<<"state">> => State0, <<"block">> => Broken}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})),
    ?assertEqual(<<"invalid_height">>, hb_maps:get(<<"error">>, Validation, <<>>, #{})).

reject_invalid_block_hash_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(5, 1),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => State0,
                <<"txs">> => [TXMsg],
                <<"timestamp">> => 1000
            },
            #{}
        ),
    Broken = Block#{<<"hash">> => hb_util:encode(crypto:strong_rand_bytes(32))},
    {ok, Validation} =
        resolve_ledger(<<"validate-block">>, #{<<"state">> => State0, <<"block">> => Broken}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})),
    ?assertEqual(<<"invalid_block_hash">>, hb_maps:get(<<"error">>, Validation, <<>>, #{})).

reject_previous_block_mismatch_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(5, 1),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block1} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => State0,
                <<"txs">> => [TXMsg],
                <<"timestamp">> => 1000
            },
            #{}
        ),
    {ok, State1} =
        resolve_ledger(<<"apply-block">>, #{<<"state">> => State0, <<"block">> => Block1}, #{}),
    {ok, Block2} =
        resolve_ledger(
            <<"generate-block">>,
            #{
                <<"state">> => State1,
                <<"txs">> => [],
                <<"timestamp">> => 1001
            },
            #{}
        ),
    Broken2 = Block2#{<<"previous-block">> => hb_util:encode(crypto:strong_rand_bytes(32))},
    {ok, Validation} =
        resolve_ledger(<<"validate-block">>, #{<<"state">> => State1, <<"block">> => Broken2}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})),
    ?assertEqual(<<"invalid_previous_block">>, hb_maps:get(<<"error">>, Validation, <<>>, #{})).

verify_block_signature_ecdsa_test() ->
    Height = ?AR_FORK_2_9_HEIGHT,
    SignedHash = crypto:strong_rand_bytes(32),
    PrevCDiff = 100,
    CDiff = PrevCDiff + 1,
    PrevSolutionHash = crypto:strong_rand_bytes(32),
    SignaturePreimage =
        get_block_signature_preimage(
            CDiff,
            PrevCDiff,
            <<PrevSolutionHash/binary, SignedHash/binary>>,
            Height
        ),
    {UncompressedPub, Priv} = crypto:generate_key(ecdh, secp256k1),
    RewardKey = compress_ecdsa_pubkey(UncompressedPub),
    RewardAddr = ar_wallet:to_address(RewardKey, {ecdsa, secp256k1}),
    Signature = crypto:sign(ecdsa, sha256, SignaturePreimage, [Priv, secp256k1]),
    Block =
        #{
            <<"reward_key">> => RewardKey,
            <<"reward_addr">> => RewardAddr,
            <<"previous_solution_hash">> => PrevSolutionHash,
            <<"cumulative_diff">> => CDiff
        },
    ?assertEqual(
        true,
        verify_block_signature(SignedHash, PrevCDiff, Signature, Block, Height, #{})
    ).

compress_ecdsa_pubkey(<<4, X:32/binary, Y:32/binary>>) ->
    Prefix =
        case (binary:decode_unsigned(Y) band 1) of
            0 -> 2;
            _ -> 3
        end,
    <<Prefix, X/binary>>.
