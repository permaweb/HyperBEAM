%%% @doc Native token process invariant tests.
-module(hb_token_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([opts/0]).
-export([generate_identities/1, generate_initial_balances/1, user_wallets/1]).

-define(USERS, 5).
-define(MAX_INITIAL_BALANCE, 1_000_000_000_000_000_000).
-define(MAX_TRANSFER_AMOUNT, 1_000_000_000_000_000_000 div 5).
-define(NODE_WALLET_CACHE_KEY, {?MODULE, node_wallet}).
-define(IDENTITIES_CACHE_KEY, {?MODULE, identities}).
-define(PROCESS_OUTBOX_DEVICE, <<"process-outbox@1.0">>).
-define(PROCESS_OUTBOX_IMPL, <<"HOcPV7wxMHYb3rSQ3EfykQhHx_b8waRWhXolhcBNgHo">>).
-define(SECURITY_DEVICE, <<"security@1.0">>).
-define(SECURITY_IMPL, <<"ARgymad5oYZcWPpxuV-A9hoSgmm4ElgPIvxMwmeh674">>).

opts() ->
    hb:init(),
    #{
        <<"load-remote-devices">> => false,
        <<"trusted-devices">> => #{
            ?PROCESS_OUTBOX_DEVICE => ?PROCESS_OUTBOX_IMPL,
            ?SECURITY_DEVICE => ?SECURITY_IMPL
        },
        <<"store">> => [hb_test_utils:test_store() | default_stores()]
    }.

default_stores() ->
    hb_opts:get(store, [], hb_opts:default_message()).

simulate_native_token_test_() ->
    {timeout, 120, fun simulate_native_token/0}.

simulate_native_token() ->
    simulate(#{ <<"execution-device">> => <<"token@1.0">> }).

simulate_hyper_token_test_() ->
    {timeout, 180, fun simulate_hyper_token/0}.

simulate_hyper_token() ->
    simulate(
        #{
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"module">> => hyper_token_script()
        }
    ).

compare_native_and_hyper_token_smoke_test_() ->
    {timeout, 120, fun compare_native_and_hyper_token_smoke/0}.

compare_native_and_hyper_token_smoke() ->
    simulate_and_compare(
        #{ <<"execution-device">> => <<"token@1.0">> },
        #{
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"module">> => hyper_token_script()
        },
        1,
        5
    ).

hyper_token_script() ->
    #{
        <<"content-type">> => <<"application/lua">>,
        <<"module">> => <<"hyper-token.lua">>,
        <<"body">> => hb_util:ok(file:read_file(hb_script_file("hyper-token.lua")))
    }.

hb_script_file(File) ->
    Candidates =
        [filename:join(["_build/default/lib/hb", "scripts", File])] ++
            case code:lib_dir(hb) of
                {error, _} -> [];
                HBDir -> [filename:join([HBDir, "scripts", File])]
            end,
    case lists:dropwhile(fun(Path) -> not filelib:is_regular(Path) end, Candidates) of
        [Path | _] -> Path;
        [] -> filename:join(["_build/default/lib/hb", "scripts", File])
    end.

simulate(Extras) ->
    ok =
        hb_invariant:state_machine(
            #{
                <<"opts">> => fun generate_sim_opts/1,
                <<"states">> => fun generate_ledger/1,
                <<"requests">> => fun generate_sim_request/2,
                <<"properties">> =>
                    [
                        fun verify_net_balance_unchanged/4,
                        fun verify_no_negative_balances/4,
                        fun verify_slot_increment/4
                    ],
                <<"runs">> => 3,
                <<"length">> => 25,
                <<"spawn-extras">> => Extras,
                <<"users">> => ?USERS
            }
        ).

simulate_and_compare(Extras1, Extras2, Runs, Length) ->
    ok =
        hb_invariant:state_machine(
            #{
                <<"opts">> => fun generate_sim_opts/1,
                <<"states">> =>
                    fun(Opts) ->
                        generate_ledger(Opts#{ <<"spawn-extras">> => Extras1 })
                    end,
                <<"models">> =>
                    fun(Opts) ->
                        generate_ledger(Opts#{ <<"spawn-extras">> => Extras2 })
                    end,
                <<"requests">> => fun generate_sim_request/2,
                <<"properties">> =>
                    [
                        fun verify_all_balances_match/6,
                        fun verify_net_balance_unchanged/4,
                        fun verify_no_negative_balances/4,
                        fun verify_slot_increment/4
                    ],
                <<"runs">> => Runs,
                <<"length">> => Length,
                <<"users">> => ?USERS
            }
        ).

generate_sim_opts(Spec) ->
    Users = hb_opts:get(<<"users">>, ?USERS, Spec),
    NodeWallet = cached_node_wallet(),
    (opts())#{
        <<"priv-wallet">> => NodeWallet,
        <<"identities">> => cached_identities(Users),
        <<"spawn-extras">> => hb_opts:get(<<"spawn-extras">>, #{}, Spec)
    }.

cached_node_wallet() ->
    case persistent_term:get(?NODE_WALLET_CACHE_KEY, not_found) of
        not_found ->
            Wallet = ar_wallet:new(),
            persistent_term:put(?NODE_WALLET_CACHE_KEY, Wallet),
            Wallet;
        Wallet ->
            Wallet
    end.

cached_identities(Users) ->
    Cached = persistent_term:get(?IDENTITIES_CACHE_KEY, #{}),
    case maps:get(Users, Cached, not_found) of
        not_found ->
            Identities = generate_identities(Users),
            persistent_term:put(?IDENTITIES_CACHE_KEY, Cached#{ Users => Identities }),
            Identities;
        Identities ->
            Identities
    end.

generate_identities(Users) ->
    lists:foldl(
        fun(_, IDs) ->
            UserWallet = ar_wallet:new(),
            ID = hb_util:human_id(UserWallet),
            IDs#{ ID => #{ <<"priv-wallet">> => UserWallet } }
        end,
        #{},
        lists:seq(1, Users)
    ).

generate_ledger(Opts) ->
    Extras = hb_opts:get(<<"spawn-extras">>, #{}, Opts),
    BalanceKey = balance_key(Extras, Opts),
    Ledger =
        dev_token_lib:ledger(
            Extras#{
                BalanceKey => generate_initial_balances(Opts),
                <<"ledger-nonce">> => hb_invariant:int(small)
            },
            Opts
        ),
    hb_cache:ensure_all_loaded(Ledger, Opts).

balance_key(Extras, Opts) ->
    case hb_maps:get(<<"execution-device">>, Extras, <<"token@1.0">>, Opts) of
        <<"lua@5.3a">> -> <<"balance">>;
        _ -> <<"balances">>
    end.

user_wallets(Opts) ->
    NodeWallet = hb_opts:get(<<"priv-wallet">>, hb:wallet(), Opts),
    maps:filtermap(
        fun
            (_, #{ <<"priv-wallet">> := Wallet }) when Wallet == NodeWallet ->
                false;
            (_, #{ <<"priv-wallet">> := Wallet }) ->
                {true, Wallet}
        end,
        hb_opts:identities(Opts)
    ).

generate_initial_balances(Opts) ->
    hb_maps:map(
        fun(_, _) -> hb_invariant:int(?MAX_INITIAL_BALANCE) end,
        user_wallets(Opts),
        Opts
    ).

generate_sim_request(_State, Opts) ->
    SenderWallet = hb_invariant:pick(user_wallets(Opts)),
    RecipientWallet = hb_invariant:pick(user_wallets(Opts)),
    Amount = hb_invariant:int(?MAX_TRANSFER_AMOUNT),
    fun(ExecState, ExecOpts) ->
        Proc = hb_maps:get(<<"process">>, ExecState, ExecState, ExecOpts),
        UserOpts = ExecOpts#{ <<"priv-wallet">> => SenderWallet },
        SystemOpts =
            ExecOpts#{
                <<"priv-wallet">> =>
                    hb_opts:get(<<"priv-wallet">>, hb:wallet(), ExecOpts)
            },
        InnerReq =
            hb_message:commit(
                #{
                    <<"action">> => <<"Transfer">>,
                    <<"recipient">> => hb_util:human_id(RecipientWallet),
                    <<"quantity">> => Amount,
                    <<"target">> => process_id(Proc, #{}, SystemOpts)
                },
                UserOpts
            ),
        {ok, _} = hb_cache:write(InnerReq, ExecOpts),
        OuterReq =
            hb_message:commit(
                #{
                    <<"path">> => <<"push">>,
                    <<"body">> => InnerReq
                },
                UserOpts
            ),
        case hb_ao:resolve(ExecState, OuterReq, SystemOpts) of
            {ok, PushRes} ->
                Slot = hb_ao:get(<<"slot">>, PushRes, ExecOpts),
                hb_ao:resolve(
                    ExecState,
                    #{
                        <<"path">> => <<"compute">>,
                        <<"slot">> => Slot,
                        <<"intent">> =>
                            #{
                                <<"action">> => <<"transfer">>,
                                <<"ledger">> => process_id(Proc, #{}, ExecOpts),
                                <<"sender">> => hb_util:human_id(SenderWallet),
                                <<"recipient">> =>
                                    hb_util:human_id(RecipientWallet),
                                <<"amount">> => Amount
                            }
                    },
                    ExecOpts
                );
            {error, Reason} ->
                {error, Reason}
        end
    end.

process_id(Process, Req, Opts) ->
    ProcMsg =
        case hb_ao:get(<<"process">>, Process, Opts#{ <<"hashpath">> => ignore }) of
            not_found ->
                {ok, Committed} = hb_message:with_only_committed(Process, Opts),
                Committed;
            Committed ->
                Committed
        end,
    Signers = hb_message:signers(ProcMsg, Opts),
    case {hb_message:verify(ProcMsg, all, Opts), Signers} of
        {false, _} ->
            ?event({process_not_verified, {process, ProcMsg}}),
            throw({process_not_verified, ProcMsg});
        {true, []} ->
            ?event({process_has_no_signers, {process, ProcMsg}}),
            throw({process_has_no_signers, ProcMsg});
        {true, _} ->
            hb_message:id(
                ProcMsg,
                hb_util:atom(maps:get(<<"commitments">>, Req, <<"signed">>)),
                Opts
            )
    end.

verify_net_balance_unchanged(OldState, _Req, NewState, Opts) ->
    supply(initial, OldState, Opts)
        =:= supply(initial, NewState, Opts) orelse
        {error,
            {supply_changed,
                {old_supply, supply(initial, OldState, Opts)},
                {new_supply, supply(initial, NewState, Opts)}
            }
        }.

verify_no_negative_balances(_OldState, _Req, NewState, Opts) ->
    Wallets = hb_maps:keys(user_wallets(Opts), Opts),
    lists:all(
        fun(Wallet) ->
            ID = hb_util:human_id(Wallet),
            case balance(ID, NewState, Opts) of
                not_found -> true;
                Balance -> Balance >= 0
            end
        end,
        Wallets
    ) orelse {error, {negative_balance, Wallets}}.

verify_slot_increment(OldState, _Req, NewState, Opts) ->
    OldSlot = hb_ao:get(<<"at-slot">>, OldState, Opts),
    NewSlot = hb_ao:get(<<"at-slot">>, NewState, Opts),
    case OldSlot of
        not_found ->
            true;
        _ ->
            NewSlot > OldSlot orelse
                {error,
                    {new_slot_not_greater_than_old_slot,
                        {old_slot, OldSlot},
                        {new_slot, NewSlot}
                    }
                }
    end.

verify_all_balances_match(_Old1, _Old2, _Req, NewState, NewModelState, Opts) ->
    NewBalances = canonical_balances(balances(initial, NewState, Opts)),
    NewModelBalances = canonical_balances(balances(initial, NewModelState, Opts)),
    NewBalances =:= NewModelBalances orelse
        {
            error,
            {balances_mismatch,
                {state, NewBalances},
                {model, NewModelBalances}
            }
        }.

supply(Mode, ProcMsg, Opts) ->
    lists:sum(
        lists:filter(
            fun is_number/1,
            maps:values(balances(Mode, ProcMsg, Opts))
        )
    ).

balances(Mode, ProcMsg, Opts) when is_atom(Mode) ->
    balances(hb_util:bin(Mode), ProcMsg, Opts);
balances(Prefix, ProcMsg, Opts) ->
    Raw =
        case hb_ao:get(<<Prefix/binary, "/balances">>, ProcMsg, not_found, Opts) of
            not_found ->
                hb_ao:get(<<Prefix/binary, "/balance">>, ProcMsg, #{}, Opts);
            Found ->
                Found
        end,
    hb_private:reset(
        hb_message:uncommitted(
            hb_cache:ensure_all_loaded(Raw, Opts),
            Opts
        )
    ).

balance(ID, ProcMsg, Opts) ->
    Account = account_key(ID),
    case hb_ao:get(<<"balances/", Account/binary>>, ProcMsg, not_found, Opts) of
        not_found ->
            case hb_ao:get(<<"balance/", Account/binary>>, ProcMsg, not_found, Opts) of
                not_found ->
                    case hb_ao:get(<<"balances/", ID/binary>>, ProcMsg, not_found, Opts) of
                        not_found ->
                            hb_ao:get(<<"balance/", ID/binary>>, ProcMsg, not_found, Opts);
                        Found ->
                            Found
                    end;
                Found ->
                    Found
            end;
        Found ->
            Found
    end.

canonical_balances(Balances) ->
    maps:fold(
        fun(Account, Amount, Acc) when is_number(Amount) ->
            Key = account_key(Account),
            Acc#{ Key => maps:get(Key, Acc, 0) + Amount };
            (_Account, _Amount, Acc) ->
                Acc
        end,
        #{},
        Balances
    ).

account_key(Account) when is_binary(Account) ->
    hb_util:to_lower(Account).
