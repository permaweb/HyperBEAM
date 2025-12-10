%%% @doc A simulation and property-based testing suite for implementations of the
%%% AO token standard.
-module(dev_token_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([opts/0]).

%%% Public utilities

%% @doc Generate a test environment options map.
opts() ->
    hb:init(),
    #{ store => [hb_test_utils:test_store()] }.

%%% Simulation tests and properties.

-define(USERS, 5).
-define(MAX_INITIAL_BALANCE, 1_000_000_000_000_000_000).
-define(MAX_TRANSFER_AMOUNT, 1_000_000_000_000_000_000 div 5).

simulate_native_token_test() ->
    simulate(#{ <<"execution-device">> => <<"token@1.0">> }).

simulate_hyper_token_test() ->
    simulate(
        #{
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"module">> =>
                dev_lua_test_ledgers:lua_script(<<"scripts/hyper-token.lua">>)
        }
    ).

compare_native_and_hyper_token_test() ->
    simulate_and_compare(
        #{ <<"execution-device">> => <<"token@1.0">> },
        #{
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"module">> =>
                dev_lua_test_ledgers:lua_script(<<"scripts/hyper-token.lua">>)
        }
    ).

simulate(Extras) ->
    ok = hb_prop:state_machine(
        #{
            opts => fun generate_sim_opts/1,
            states => fun generate_ledger/1,
            requests => fun generate_sim_request/2,
            properties =>
                [
                    fun verify_net_balance_unchanged/4,
                    fun verify_no_negative_balances/4,
                    fun verify_slot_increment/4
                ],
            runs => 3,
            length => 100,
            spawn_extras => Extras,
            users => ?USERS
        }
    ).

simulate_and_compare(Extras1, Extras2) ->
    ok = hb_prop:state_machine(
        #{
            opts => fun generate_sim_opts/1,
            states =>
                fun(Opts) ->
                    generate_ledger(Opts#{ spawn_extras => Extras1 })
                end,
            models =>
                fun(Opts) ->
                    generate_ledger(Opts#{ spawn_extras => Extras2 })
                end,
            requests => fun generate_sim_request/2,
            properties =>
                [
                    fun verify_all_balances_match/6,
                    fun verify_net_balance_unchanged/4,
                    fun verify_no_negative_balances/4,
                    fun verify_slot_increment/4
                ],
            runs => 3,
            length => 4,
            users => ?USERS
        }
    ).

generate_sim_opts(#{ users := Users, spawn_extras := Extras }) ->
    BaseOpts = dev_token_props:opts(),
    NodeWallet = ar_wallet:new(),
    BaseOpts#{
        priv_wallet => NodeWallet,
        identities =>
            lists:foldl(
                fun(_, IDs) ->
                    UserWallet = ar_wallet:new(),
                    ID = hb_util:human_id(UserWallet),
                    IDs#{ ID => #{ priv_wallet => UserWallet } }
                end,
                #{},
                lists:seq(1, Users)
            ),
        spawn_extras => Extras
    }.

%% @doc Generate a ledger process, including any extra properties specified in
%% the `spawn_extras' option.
generate_ledger(Opts) ->
    Extras = hb_opts:get(spawn_extras, #{}, Opts),
    dev_token_lib:ledger(
        Extras#{
            <<"balances">> => generate_initial_balances(Opts)
        },
        Opts
    ).

user_wallets(Opts = #{ priv_wallet := NodeWallet }) ->
    maps:filtermap(
        fun(_, #{ priv_wallet := Wallet }) when Wallet == NodeWallet -> false;
           (_, #{ priv_wallet := Wallet }) -> {true, Wallet}
        end,
        hb_opts:identities(Opts)
    ).

generate_initial_balances(Opts) ->
    hb_maps:map(
        fun(_, _) -> hb_prop:int(?MAX_INITIAL_BALANCE) end,
        user_wallets(Opts),
        Opts
    ).

generate_sim_request(State, Opts) ->
    {ok, PushRes} =
        dev_token_lib:transfer(
            State,
            SenderWallet = hb_prop:pick(user_wallets(Opts)),
            RecipientWallet = hb_prop:pick(user_wallets(Opts)),
            Amount = hb_prop:int(?MAX_TRANSFER_AMOUNT),
            Opts
        ),
    Slot = hb_ao:get(<<"slot">>, PushRes, Opts),
    #{
        <<"path">> => <<"compute">>,
        <<"slot">> => Slot,
        <<"intent">> =>
            #{
                <<"action">> => <<"transfer">>,
                <<"sender">> => hb_util:human_id(SenderWallet),
                <<"recipient">> => hb_util:human_id(RecipientWallet),
                <<"amount">> => Amount
            }
    }.

verify_net_balance_unchanged(OldState, _Req, NewState, Opts) ->
    dev_token_lib:supply(initial, OldState, Opts)
        =:= dev_token_lib:supply(initial, NewState, Opts) orelse
        {error,
            {supply_changed,
                {old_supply, dev_token_lib:supply(initial, OldState, Opts)},
                {new_supply, dev_token_lib:supply(initial, NewState, Opts)}
            }
        }.

verify_no_negative_balances(_OldState, _Req, NewState, Opts) ->
    Wallets = hb_maps:keys(user_wallets(Opts), Opts),
    lists:all(
        fun(Wallet) ->
            ID = hb_util:human_id(Wallet),
            case hb_ao:get(<<"balances/", ID/binary>>, NewState, Opts) of
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
        not_found -> true;
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
    NewBalances = dev_token_lib:balances(initial, NewState, Opts),
    NewModelBalances = dev_token_lib:balances(initial, NewModelState, Opts),
    NewBalances =:= NewModelBalances orelse
        {
            error,
            {balances_mismatch,
                {state, NewBalances},
                {model, NewModelBalances}
            }
        }.