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
        identities => dev_token_props:generate_identities(Users),
        priv_wallet => ar_wallet:new()
    }.

generate_initial_state(Opts) ->
    MintCap = hb_invariant:int(1, 1_000_000_000_000_000),
    PropN = 1 + hb_invariant:int(1, 10_000),
    PropD = PropN + hb_invariant:int(1, 10_000),
    StartWeight = hb_invariant:int(1, 10_000),
    StartQty = hb_invariant:int(1, 1_000_000),
    StartResource = hb_invariant:string(id),
    StartAddr = hb_invariant:pick(dev_token_props:user_wallets(Opts)),
    S0 =
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
    Resources = hb_maps:get(resources, Opts),
    S1 = lists:foldl(
        fun(Resource, State) ->
            dev_pot:register_resource(Resource, hb_invariant:int(), State, Opts)
        end,
        S0,
        Resources
    ),
    dev_token_lib:ledger(S1, Opts).

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
            <<"body">> => #{
                <<"address">> =>
                    hb_util:human_id(
                        hb_invariant:pick(
                            dev_token_props:user_wallets(Opts)
                        )
                    ),
                <<"quantity">> => hb_invariant:int(1, 1_000_000),
                <<"resource">> => hb_invariant:pick(hb_maps:get(resources, Opts)),
                <<"from">> => <<"foo">>
            }
        },
        Opts#{ priv_wallet => Wallet }
    ).

verify_deposit(OldState, _Req, NewState, Opts) ->
  true.

next(OldS, _Req, NewS, Opts) -> OldS.