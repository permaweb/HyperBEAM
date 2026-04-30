%%% @doc Dynamic pricing device for P4.
%%%
%%% `metering@1.0' records resource usage in the current process during a P4
%%% request/response lifecycle. `estimate/3' starts the metering session and
%%% captures the initial BEAM reductions count. Other devices can then call
%%% `meter/3' or `increase/3' to add resource usage. Finally, `price/3' adds the
%%% reductions delta, applies the operator's `metering-rates' table, and returns
%%% the total integer token charge to P4.
-module(dev_metering).
-export([info/1, estimate/3, price/3, meter/3, meter/4]).
-export([increase/3, increase/4, totals/3]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(METERING_KEY, {dev_metering, state}).
-define(BEAM_REDUCTIONS, <<"beam-reductions">>).

%% @doc Device API information.
info(_) ->
    #{
        exports =>
            [
                <<"estimate">>,
                <<"price">>,
                <<"meter">>,
                <<"increase">>,
                <<"totals">>
            ]
    }.

%% @doc Start a metering session for the request.
estimate(_Base, EstimateReq, Opts) ->
    Request = hb_maps:get(<<"request">>, EstimateReq, #{}, Opts),
    {reductions, Reductions} = erlang:process_info(self(), reductions),
    erlang:put(
        ?METERING_KEY,
        #{
            default_payer => payer(Request, Opts),
            start_reductions => Reductions,
            meters => #{}
        }
    ),
    {ok, hb_util:int(hb_opts:get(metering_minimum_price, 0, Opts))}.

%% @doc Close the metering session and calculate the final AO token price.
price(_Base, PriceReq, Opts) ->
    Request = hb_maps:get(<<"request">>, PriceReq, #{}, Opts),
    State0 = metering_state(Request, Opts),
    State = meter_reductions(State0),
    Meters = maps:get(meters, State, #{}),
    Rates = hb_opts:get(<<"metering-rates">>, #{}, Opts),
    Price =
        case hb_maps:find(<<"party">>, PriceReq, Opts) of
            {ok, Party} ->
                price_for(
                    maps:get(normalize_party(Party, State, Opts), Meters, #{}),
                    Rates,
                    Opts
                );
            error ->
                price_for_all(Meters, Rates, Opts)
        end,
    erlang:erase(?METERING_KEY),
    {ok, Price}.

%% @doc Device API for incrementing a resource counter.
meter(Base, Req, Opts) when is_map(Base), is_map(Req) ->
    Party = hb_maps:get(<<"party">>, Req, default, Opts),
    Resource =
        hb_maps:get(
            <<"resource">>,
            Req,
            hb_maps:get(<<"resource">>, Base, undefined, Opts),
            Opts
        ),
    Amount = hb_maps:get(<<"amount">>, Req, 1, Opts),
    ok = meter(Party, Resource, Amount, Opts),
    {ok, current_totals()};
%% @doc Helper API for other devices. Increments the default payer.
meter(Resource, Amount, Opts) ->
    meter(default, Resource, Amount, Opts).

%% @doc Helper API for other devices. Increments a specific payer.
meter(Party, Resource, Amount, Opts) ->
    case erlang:get(?METERING_KEY) of
        undefined ->
            ok;
        State ->
            Payer = normalize_party(Party, State, Opts),
            ResourceKey = normalize_resource(Resource),
            AmountInt = non_negative_int(Amount),
            erlang:put(
                ?METERING_KEY,
                add_meter(Payer, ResourceKey, AmountInt, State)
            ),
            ok
    end.

%% @doc Alias for `meter/3' in helper and device contexts.
increase(Base, Req, Opts) when is_map(Base), is_map(Req) ->
    meter(Base, Req, Opts);
increase(Resource, Amount, Opts) ->
    meter(Resource, Amount, Opts).

%% @doc Alias for `meter/4'.
increase(Party, Resource, Amount, Opts) ->
    meter(Party, Resource, Amount, Opts).

%% @doc Return the current process's metering totals.
totals(_Base, _Req, _Opts) ->
    {ok, current_totals()}.

%% @doc Return the active metering state, creating one if needed.
metering_state(Request, Opts) ->
    case erlang:get(?METERING_KEY) of
        undefined ->
            {reductions, Reductions} = erlang:process_info(self(), reductions),
            #{
                default_payer => payer(Request, Opts),
                start_reductions => Reductions,
                meters => #{}
            };
        State ->
        State
    end.

%% @doc Add the process reductions delta to the active metering state.
meter_reductions(State = #{ start_reductions := Start }) ->
    {reductions, Current} = erlang:process_info(self(), reductions),
    Delta = max(0, Current - Start),
    add_meter(
        maps:get(default_payer, State),
        ?BEAM_REDUCTIONS,
        Delta,
        State
    ).

%% @doc Add a resource amount to a party's meters.
add_meter(_Payer, _Resource, 0, State) ->
    State;
add_meter(Payer, Resource, Amount, State) ->
    Meters = maps:get(meters, State, #{}),
    PayerMeters = maps:get(Payer, Meters, #{}),
    Current = maps:get(Resource, PayerMeters, 0),
    State#{
        meters =>
            Meters#{
                Payer =>
                    PayerMeters#{ Resource => Current + Amount }
            }
    }.

%% @doc Calculate a token price across all metered parties.
price_for_all(Meters, Rates, Opts) ->
    maps:fold(
        fun(_Payer, PayerMeters, Acc) ->
            Acc + price_for(PayerMeters, Rates, Opts)
        end,
        0,
        Meters
    ).

%% @doc Calculate a token price from resource meters and operator rates.
price_for(Meters, Rates, Opts) ->
    maps:fold(
        fun(Resource, Amount, Acc) ->
            Acc + (Amount * rate(Resource, Rates, Opts))
        end,
        0,
        Meters
    ).

%% @doc Return the operator-configured token rate for a resource.
rate(Resource, Rates, Opts) ->
    case hb_maps:get(Resource, Rates, 0, Opts) of
        Rate when is_integer(Rate) -> Rate;
        Rate -> hb_util:int(Rate)
    end.

%% @doc Determine the default payer for a request.
payer(Request, Opts) ->
    case hb_message:signers(Request, Opts) of
        [Signer] -> normalize_party(Signer, #{}, Opts);
        [] -> <<"unknown">>;
        Multiple ->
            hb_util:bin(lists:join(<<",">>, lists:map(fun hb_util:bin/1, Multiple)))
    end.

%% @doc Normalize a payer identifier for storage in the meter map.
normalize_party(default, State, _Opts) ->
    maps:get(default_payer, State, <<"unknown">>);
normalize_party(Payer, _State, _Opts) when ?IS_ID(Payer) ->
    hb_util:human_id(Payer);
normalize_party(Payer, _State, _Opts) ->
    hb_util:bin(Payer).

%% @doc Normalize a resource name for storage in the meter map.
normalize_resource(Resource) ->
    hb_ao:normalize_key(Resource).

%% @doc Convert an amount to a non-negative integer.
non_negative_int(Amount) ->
    Int = hb_util:int(Amount),
    case Int >= 0 of
        true -> Int;
        false -> error({invalid_meter_amount, Amount})
    end.

%% @doc Return the current process's raw metering map.
current_totals() ->
    case erlang:get(?METERING_KEY) of
        undefined -> #{};
        State -> maps:get(meters, State, #{})
    end.

%%% Tests

%% @doc The helper API meters resources and prices them via configured rates.
helper_price_test() ->
    Wallet = ar_wallet:new(),
    Request =
        hb_message:commit(
            #{ <<"path">> => <<"/metered">> },
            #{ <<"priv-wallet">> => Wallet }
        ),
    Opts = #{
        <<"metering-rates">> => #{
            <<"arweave-bytes">> => 3,
            ?BEAM_REDUCTIONS => 0
        }
    },
    {ok, 0} = estimate(#{}, #{ <<"request">> => Request }, Opts),
    ok = meter(<<"arweave-bytes">>, 5, Opts),
    {ok, 15} = price(#{}, #{ <<"request">> => Request }, Opts).

%% @doc BEAM reductions are metered between estimate and price.
beam_reductions_price_test() ->
    Wallet = ar_wallet:new(),
    Request =
        hb_message:commit(
            #{ <<"path">> => <<"/metered">> },
            #{ <<"priv-wallet">> => Wallet }
        ),
    Opts = #{ <<"metering-rates">> => #{ ?BEAM_REDUCTIONS => 1 } },
    {ok, 0} = estimate(#{}, #{ <<"request">> => Request }, Opts),
    lists:foreach(fun(_) -> erlang:phash2(rand:bytes(16)) end, lists:seq(1, 10)),
    {ok, Price} = price(#{}, #{ <<"request">> => Request }, Opts),
    ?assert(Price > 0).

%% @doc Pricing without a party includes all explicitly metered parties.
explicit_party_price_test() ->
    Wallet = ar_wallet:new(),
    Request =
        hb_message:commit(
            #{ <<"path">> => <<"/metered">> },
            #{ <<"priv-wallet">> => Wallet }
        ),
    Other = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"metering-rates">> => #{
            <<"arweave-bytes">> => 2,
            ?BEAM_REDUCTIONS => 0
        }
    },
    {ok, 0} = estimate(#{}, #{ <<"request">> => Request }, Opts),
    ok = meter(<<"arweave-bytes">>, 3, Opts),
    ok = meter(Other, <<"arweave-bytes">>, 5, Opts),
    {ok, 10} =
        price(
            #{},
            #{ <<"request">> => Request, <<"party">> => Other },
            Opts
        ),
    {ok, 0} = estimate(#{}, #{ <<"request">> => Request }, Opts),
    ok = meter(<<"arweave-bytes">>, 3, Opts),
    ok = meter(Other, <<"arweave-bytes">>, 5, Opts),
    {ok, 16} = price(#{}, #{ <<"request">> => Request }, Opts).

%% @doc P4 charges a dynamic metering price during response processing.
p4_response_charge_test() ->
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Processor =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"simple-pay@1.0">>,
            <<"pricing-device">> => <<"metering@1.0">>
        },
    Node =
        hb_http_server:start_node(
            #{
                <<"simple-pay-ledger">> => #{ Address => 100 },
                <<"metering-rates">> => #{
                    <<"arweave-bytes">> => 2,
                    ?BEAM_REDUCTIONS => 0
                },
                <<"operator">> => hb:address(),
                <<"on">> => #{
                    <<"request">> => Processor,
                    <<"response">> => Processor
                }
            }
        ),
    MeterReq =
        hb_message:commit(
            #{
                <<"path">> => <<"/~metering@1.0/meter">>,
                <<"resource">> => <<"arweave-bytes">>,
                <<"amount">> => 5
            },
            #{ <<"priv-wallet">> => Wallet }
        ),
    ?assertMatch({ok, _}, hb_http:post(Node, MeterReq, #{})),
    {ok, Balance} =
        hb_http:get(
            Node,
            hb_message:commit(
                #{ <<"path">> => <<"/~p4@1.0/balance">> },
                #{ <<"priv-wallet">> => Wallet }
            ),
            #{}
        ),
    ?assertEqual(90, Balance).
