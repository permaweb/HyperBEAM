-module(dev_pot_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(USERS, 5).

simulation_test() ->
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
    NodeWallet = ar_wallet:new(),
    #{
        priv_wallet => NodeWallet,
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
    #{
        <<"device">> => <<"pot@1.0">>,
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
                    StartAddr => #{
                        <<"quantity">> => StartQty,
                        <<"last-resource-accumulator">> => 1 % TODO: randomize this?
                    }
                }
            }
        },
        <<"balances">> => dev_token_props:generate_initial_balances(Opts),
        <<"users">> => #{
            StartAddr => #{
                <<"deposits">> => #{
                    StartResource => StartQty
                }
            }
        }
    }.

generate_request() ->
    [
        fun deposit_generator/2
        % fun withdraw_generator/0,
        % fun delegate_generator/0,
        % fun undelegate_generator/0
    ].

deposit_generator(_State, Opts) ->
    hb_message:commit(
        #{
            <<"path">> => <<"deposit">>,
            <<"address">> => hb_invariant:pick(dev_token_props:user_wallets(Opts)),
            <<"amount">> => hb_invariant:int(1, 1_000_000)
        },
        hb_opts:as(hb_invariant:pick(dev_token_props:user_wallets(Opts)), Opts)
    ).

verify_deposit(OldState, _Req, NewState, Opts) ->
  true.

next(OldS, _Req, NewS, Opts) -> OldS.