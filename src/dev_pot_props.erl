-module(dev_pot_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(USERS, 10).

simulation_test() ->
    hb:init(),
    ok = hb_invariant:state_machine(
        #{
            opts => fun generate_opts/1,
            requests => generate_request(),
            states => fun generate_initial_state/1,
            properties => [
                fun verify_deposit/4
            ],
            runs => 3,
            length => 4,
            next => fun next/4,
            users => ?USERS
        }
    ).

generate_opts(#{ users := Users }) ->
    #{
        resources =>
            [
                hb_invariant:string(id)
            ||
                _ <- lists:seq(1, 10)
            ],
        identities => dev_token_props:generate_identities(Users)
    }.

generate_initial_state(Opts) ->
    MintCap = hb_invariant:int(1, 1_000_000_000_000_000),
    PropN = 1 + hb_invariant:int(1, 10_000),
    PropD = PropN + hb_invariant:int(1, 10_000),
    StartWeight = hb_invariant:int(1, 10_000),
    StartQty = hb_invariant:int(1, 1_000_000),
    StartResource = hb_invariant:string(id),
    StartAddr = hb_invariant:pick(dev_token_props:user_wallets(Opts)),
    dev_token_lib:ledger(
        #{
            <<"mint-device">> => <<"pot@1.0">>,
            <<"t">> => 0,
            <<"last-drip">> => 0,
            <<"mint-cap">> => MintCap,
            <<"mint-prop-numerator">> => PropN,
            <<"mint-prop-denominator">> => PropD,
            <<"resources">> => #{
                StartResource => #{
                    <<"accumulator">> => 1,
                    <<"last-global-accumulator">> => 1,
                    <<"weight">> => StartWeight,
                    <<"total-deposits">> => StartQty,
                    <<"deposits">> => #{
                        hb_util:human_id(StartAddr) => #{
                            <<"quantity">> => StartQty,
                            <<"last-resource-accumulator">> => 1 % TODO: randomize this?
                        }
                    }
                }
            },
            <<"balances">> => dev_token_props:generate_initial_balances(Opts),
            <<"users">> => #{
                hb_util:human_id(StartAddr) => #{
                    <<"deposits">> => #{
                        StartResource => StartQty
                    }
                }
            }
        },
        Opts
    ).

generate_request() ->
    [
        fun deposit_generator/2
        % fun withdraw_generator/0,
        % fun delegate_generator/0,
        % fun undelegate_generator/0
    ].

deposit_generator(_State, Opts) ->
    Wallet = hb_invariant:pick(dev_token_props:user_wallets(Opts)),
    hb_message:commit(
        #{
            <<"path">> => <<"deposit">>,
            <<"address">> =>
                hb_util:human_id(
                    hb_invariant:pick(
                        dev_token_props:user_wallets(Opts)
                    )
                ),
            <<"amount">> => hb_invariant:int(1, 1_000_000)
        },
        Opts#{ priv_wallet => Wallet }
    ).

verify_deposit(OldState, _Req, NewState, Opts) ->
  true.

next(OldS, _Req, NewS, Opts) -> OldS.