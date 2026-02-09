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
            TXRootRaw = compute_tx_root(TXRecords),
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
                    TXRootRaw = compute_tx_root(TXRecords),
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
                                        false ->
                                            {error, invalid_block_hash};
                                        true ->
                                            {ok,
                                                #{
                                                    <<"valid">> => true,
                                                    <<"height">> => Height,
                                                    <<"tx-root-raw">> => TXRootRaw,
                                                    <<"hash-raw">> => HashRaw,
                                                    <<"state-after">> => InterimState
                                                }
                                            }
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
        <<"last-tx-record">> =>
            hb_message:convert(TX, <<"structured@1.0">>, <<"tx@1.0">>, Opts)
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
    Candidates = [{Req, Key} || Key <- Keys] ++ [{Base, Key} || Key <- Keys],
    hb_ao:get_first(Candidates, Default, Opts).

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

verify_block_hash(Block, HashRaw, Opts) ->
    case read_binary_optional([<<"hash">>], Block, Block, Opts) of
        {ok, DeclaredHash} -> DeclaredHash =:= HashRaw;
        error -> true
    end.

compute_tx_root([]) ->
    <<>>;
compute_tx_root(TXs) ->
    {_, SizeTagged} =
        lists:foldl(
            fun(TX, {Offset, Acc}) ->
                Size = max(1, tx_size_increase(TX)),
                End = Offset + Size,
                {End, Acc ++ [{tx_id_raw(TX), End}]}
            end,
            {0, []},
            TXs
        ),
    {Root, _Tree} = ar_merkle:generate_tree(SizeTagged),
    Root.

compute_block_hash(PrevHashRaw, Height, Timestamp, TXRootRaw, TXRecords) ->
    TXIDs = lists:map(fun tx_id_raw/1, TXRecords),
    Payload = term_to_binary([PrevHashRaw, Height, Timestamp, TXRootRaw, TXIDs]),
    crypto:hash(sha256, Payload).

tx_id_raw(TX) ->
    case ar_tx:id(TX, signed) of
        not_signed -> ar_tx:id(TX, unsigned);
        ID -> ID
    end.

tx_size_increase(#tx{format = 1, data = Data}) when is_binary(Data) ->
    byte_size(Data);
tx_size_increase(#tx{data_size = DataSize, data = Data}) ->
    case DataSize > 0 of
        true -> DataSize;
        false when is_binary(Data) -> byte_size(Data);
        false -> 0
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
    {ok, State1} = apply_tx(State0, #{<<"tx">> => TXMsg}, #{}),
    SenderKey = addr_key(ar_wallet:to_address(SenderWallet)),
    RecipientKey = addr_key(Recipient),
    ?assertEqual(88, get_balance(State1, SenderKey, #{})),
    ?assertEqual(10, get_balance(State1, RecipientKey, #{})),
    ?assertEqual(2, hb_maps:get(<<"pending-reward">>, State1, 0, #{})).

reject_overspend_tx_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(10, 2),
    State0 = base_state(SenderWallet, 5),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Validation} = validate_tx(State0, #{<<"tx">> => TXMsg}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})).

generate_and_apply_block_test() ->
    {SenderWallet, Recipient, SignedTX} = test_signed_tx(7, 3),
    MinerWallet = ar_wallet:new(),
    MinerAddr = addr_key(ar_wallet:to_address(MinerWallet)),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block} =
        generate_block(
            State0,
            #{
                <<"txs">> => [TXMsg],
                <<"reward-addr">> => MinerAddr,
                <<"timestamp">> => 1000
            },
            #{}
        ),
    {ok, Validation} = validate_block(State0, #{<<"block">> => Block}, #{}),
    ?assertEqual(true, hb_maps:get(<<"valid">>, Validation, false, #{})),
    {ok, State1} = apply_block(State0, #{<<"block">> => Block}, #{}),
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
        generate_block(
            State0,
            #{
                <<"txs">> => [TXMsg],
                <<"timestamp">> => 1000
            },
            #{}
        ),
    BrokenBlock = Block#{<<"tx-root">> => hb_util:encode(crypto:strong_rand_bytes(32))},
    {ok, Validation} = validate_block(State0, #{<<"block">> => BrokenBlock}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})).

reject_replay_tx_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(5, 1),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, State1} = apply_tx(State0, #{<<"tx">> => TXMsg}, #{}),
    {ok, Validation} = validate_tx(State1, #{<<"tx">> => TXMsg}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})),
    ?assertEqual(<<"tx_already_seen">>, hb_maps:get(<<"error">>, Validation, <<>>, #{})).

reject_duplicate_tx_in_block_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(5, 1),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {error, tx_already_seen} =
        generate_block(
            State0,
            #{
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
        generate_block(
            State0,
            #{
                <<"txs">> => [TXMsg],
                <<"timestamp">> => 1000
            },
            #{}
        ),
    Broken = Block#{<<"height">> => 10},
    {ok, Validation} = validate_block(State0, #{<<"block">> => Broken}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})),
    ?assertEqual(<<"invalid_height">>, hb_maps:get(<<"error">>, Validation, <<>>, #{})).

reject_invalid_block_hash_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(5, 1),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block} =
        generate_block(
            State0,
            #{
                <<"txs">> => [TXMsg],
                <<"timestamp">> => 1000
            },
            #{}
        ),
    Broken = Block#{<<"hash">> => hb_util:encode(crypto:strong_rand_bytes(32))},
    {ok, Validation} = validate_block(State0, #{<<"block">> => Broken}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})),
    ?assertEqual(<<"invalid_block_hash">>, hb_maps:get(<<"error">>, Validation, <<>>, #{})).

reject_previous_block_mismatch_test() ->
    {SenderWallet, _Recipient, SignedTX} = test_signed_tx(5, 1),
    State0 = base_state(SenderWallet, 100),
    TXMsg = hb_message:convert(SignedTX, <<"structured@1.0">>, <<"tx@1.0">>, #{}),
    {ok, Block1} =
        generate_block(
            State0,
            #{
                <<"txs">> => [TXMsg],
                <<"timestamp">> => 1000
            },
            #{}
        ),
    {ok, State1} = apply_block(State0, #{<<"block">> => Block1}, #{}),
    {ok, Block2} =
        generate_block(
            State1,
            #{
                <<"txs">> => [],
                <<"timestamp">> => 1001
            },
            #{}
        ),
    Broken2 = Block2#{<<"previous-block">> => hb_util:encode(crypto:strong_rand_bytes(32))},
    {ok, Validation} = validate_block(State1, #{<<"block">> => Broken2}, #{}),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Validation, true, #{})),
    ?assertEqual(<<"invalid_previous_block">>, hb_maps:get(<<"error">>, Validation, <<>>, #{})).
