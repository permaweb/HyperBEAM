%%% @doc Dynamic pricing device for P4.
%%%
%%% `metering@1.0' records resource usage in the current process during a P4
%%% request/response lifecycle. It is intended to be used as a P4 pricing
%%% device:
%%%
%%%     `estimate/3' opens a process-local metering session.
%%%     `meter/3' increments resource usage during that session.
%%%     `price/3' closes the session and returns the integer charge.
%%%
%%% Calls to `meter/3' outside an active session are no-ops, so callers do
%%% not need to check whether metering is enabled. Resource names are normalized
%%% keys, such as `arweave-bytes' and `beam-reductions'. The operator sets
%%% `metering-rates' in the node message as a map of resource name to AO token
%%% units per resource unit.
-module(dev_metering).
-export([info/1, estimate/3, price/3, is_active/0, meter/3]).

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
                <<"meter">>
            ]
    }.

%% @doc Start a metering session for the request.
estimate(_Base, _EstimateReq, _Opts) ->
    {reductions, Reductions} = erlang:process_info(self(), reductions),
    erlang:put(
        ?METERING_KEY,
        #{
            start_reductions => Reductions,
            meters => #{}
        }
    ),
    {ok, 0}.

%% @doc Close the metering session and calculate the final AO token price.
price(_Base, _PriceReq, Opts) ->
    Rates = hb_opts:get(<<"metering-rates">>, #{}, Opts),
    Price =
        maps:fold(
            fun(Resource, Amount, Acc) ->
                Rate = hb_util:int(hb_maps:get(Resource, Rates, 0, Opts)),
                Acc + (Amount * Rate)
            end,
            0,
            maps:get(
                meters,
                meter_reductions(erlang:get(?METERING_KEY)),
                #{}
            )
        ),
    erlang:erase(?METERING_KEY),
    {ok, Price}.

%% @doc Return whether the current process has an active metering session.
is_active() ->
    erlang:get(?METERING_KEY) =/= undefined.

%% @doc Device API for incrementing a resource counter.
meter(Base, Req, Opts) when is_map(Base), is_map(Req) ->
    Resource =
        hb_maps:get(
            <<"resource">>,
            Req,
            hb_maps:get(<<"resource">>, Base, undefined, Opts),
            Opts
        ),
    Amount = hb_maps:get(<<"amount">>, Req, 1, Opts),
    ok = meter(Resource, Amount, Opts),
    {ok, true};

%% @doc Helper API for other devices.
meter(Resource, Amount, _Opts) ->
    case erlang:get(?METERING_KEY) of
        undefined ->
            ok;
        State ->
            AmountInt = hb_util:int(Amount),
            case AmountInt >= 0 of
                true ->
                    erlang:put(
                        ?METERING_KEY,
                        add_meter(
                            hb_ao:normalize_key(Resource),
                            AmountInt,
                            State
                        )
                    ),
                    ok;
                false ->
                    error({invalid_meter_amount, Amount})
            end
    end.

%% @doc Add the process reductions delta to the active metering state.
meter_reductions(undefined) ->
    #{ meters => #{} };
meter_reductions(State = #{ start_reductions := Start }) ->
    {reductions, Current} = erlang:process_info(self(), reductions),
    add_meter(
        ?BEAM_REDUCTIONS,
        max(0, Current - Start),
        State
    ).

%% @doc Add a resource amount to the meter state.
add_meter(Resource, Amount, State) ->
    Meters = maps:get(meters, State, #{}),
    State#{
        meters =>
            Meters#{
                Resource => maps:get(Resource, Meters, 0) + Amount
            }
    }.

%%% Tests

%% @doc Metering outside an active session is a no-op.
inactive_meter_noop_test() ->
    erlang:erase(?METERING_KEY),
    ok = meter(<<"arweave-bytes">>, 5, #{}),
    ?assertEqual(false, is_active()).

%% @doc The helper API meters resources and prices them via configured rates.
helper_price_test() ->
    Opts = #{
        <<"metering-rates">> => #{
            <<"arweave-bytes">> => 3,
            ?BEAM_REDUCTIONS => 0
        }
    },
    {ok, 0} = estimate(#{}, #{}, Opts),
    ok = meter(<<"arweave-bytes">>, 5, Opts),
    {ok, 15} = price(#{}, #{}, Opts).

%% @doc BEAM reductions are metered between estimate and price.
beam_reductions_price_test() ->
    Opts = #{ <<"metering-rates">> => #{ ?BEAM_REDUCTIONS => 1 } },
    {ok, 0} = estimate(#{}, #{}, Opts),
    lists:foreach(
        fun(_) -> erlang:phash2(rand:bytes(16)) end,
        lists:seq(1, 10)
    ),
    {ok, Price} = price(#{}, #{}, Opts),
    ?assert(Price > 0).

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
