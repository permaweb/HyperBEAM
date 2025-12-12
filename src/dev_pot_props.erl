-module(dev_pot_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

simulation_test() ->
    ok = hb_prop:state_machine(
        #{
            opts => #{},
            states => fun generate_initial_state/1,
            requests => fun generate_request/2,
            properties => [
                fun verify_deposit/4
            ],
            runs => 3,
            length => 4,
            next => fun next/4
        }
    ).

% TODO: replace these generators with generators from hb_prop
weight_gen() ->
    1 + rand:uniform(10_000).

qty_gen() ->
    1 + rand:uniform(1_000_000).

addr_gen() ->
    base64:encode(crypto:strong_rand_bytes(32), #{mode => urlsafe, padding => false}).

resource_gen() ->
    base64:encode(crypto:strong_rand_bytes(32), #{mode => urlsafe, padding => false}).

generate_initial_state(Opts) ->
    % TODO: replace these generators with generators from hb_prop
    MintCap = 100 + rand:uniform(1_000_000_000_000_000),
    PropN = 1 + rand:uniform(1_000),
    PropD = PropN + rand:uniform(10_000),
    StartWeight = weight_gen(),
    StartQty = qty_gen(),
    StartResource = resource_gen(),
    StartAddr = addr_gen(),
    #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => MintCap,
        <<"mint-prop-numerator">> => PropN,
        <<"mint-prop-denominator">> => PropD,
        <<"resources">> => #{
            StartResource => #{
                <<"accumulator">> => 1, % TODO: randomize this?
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
        <<"balances">> => #{ },
        <<"users">> => #{
            StartAddr => #{
                <<"deposits">> => #{
                    StartResource => StartQty
                }
            }
        }
    }.

generate_request(State, Opts) ->
    #{
        <<"path">> => <<"deposit">>,
        <<"address">> => hb_prop:key(),
        <<"amount">> => hb_prop:int()
    }.

verify_deposit(OldState, _Req, NewState, Opts) ->
  true.

next(OldS, _Req, NewS, Opts) -> OldS.