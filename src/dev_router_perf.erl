%%% @doc An Erlang execution device for `process@1.0' that provides
%%% performance-based routing. Replaces the Lua `dynamic-router.lua' for
%%% Arweave gateway routing, fixing two gaps:
%%% 1. Supports `match'/`with' route format (not just `prefix'/`price'/`topup').
%%% 2. Handles monitor duration POSTs that lack an `action' field.
%%%
%%% Device state is stored in the Base message (standard HyperBEAM architecture).
%%% Configuration keys read from Base:
%%% - `performance-period': EMA smoothing period (default 1000).
%%% - `initial-performance': Starting perf score for new nodes (default 30000).
%%% - `sampling-rate': Fraction of random sampling (default 0.1).
%%% - `performance-weight': Weight factor for perf scoring (default 1).
%%% - `pricing-weight': Weight factor for price scoring (default 1).
%%% - `score-preference': Decay exponent for scoring (default 1).
-module(dev_router_perf).
-export([init/3, compute/3, snapshot/3, normalize/3]).
-export([duration/3, register/3, recalculate/3]).
%%% Helper API
-export([to_float/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Compute dispatcher for process@1.0 compatibility.
compute(Base, Req, Opts) ->
    Body = hb_ao:get(<<"body">>, Req, Req, Opts),
    %% TODO: THIS IS WIERD IS THERE A BETTER WAY???
    %% Falls back to `path' because hb_http_client:maybe_invoke_monitor
    %% sends duration data with `path => <<"duration">>' (not `action').
    Action = 
        case hb_ao:get(<<"action">>, Body, not_found, Opts) of
            not_found -> hb_ao:get(<<"path">>, Body, not_found, Opts);
            A -> A
        end,
    case Action of
        <<"register">> -> register(Base, Body, Opts);
        <<"recalculate">> -> recalculate(Base, Body, Opts);
        <<"duration">> -> duration(Base, Body, Opts);
        _ ->
            ?event({erroe_report, <<"Action not supported.">>}),
            {ok, Base}
    end.

%% @doc Initialize the device state with defaults if not already set.
init(Base, _Req, Opts) ->
    {ok, ensure_defaults(Base, Opts)}.

%% @doc Return a snapshot of the execution device state for caching.
snapshot(Base, _Req, _Opts) ->
    {ok, Base}.

%% @doc Restore execution device state from a snapshot.
normalize(Base, _Req, _Opts) ->
    {ok, Base}.

%% @doc Update the performance score for a node identified by `reference'.
%% Uses exponential weighted average:
%%   new_perf = current * (1 - 1/period) + duration * (1/period)
duration(RawBase, Req, Opts) ->
    Base = ensure_defaults(RawBase, Opts),
    Body = hb_ao:get(<<"body">>, Req, Req, Opts),
    case hb_ao:get(<<"reference">>, Body, not_found, Opts) of
        not_found -> {ok, Base};
        Reference ->
            Duration = to_float(hb_ao:get(<<"duration">>, Body, Opts)),
            Period = to_float(
                hb_ao:get(<<"performance-period">>, Base, 1000, Opts)
            ),
            ChangeFactor = 1.0 / Period,
            Routes = hb_ao:get(<<"routes">>, Base, [], Opts),
            UpdatedRoutes = update_node_performance(
                Routes, 
                Reference, 
                Duration, 
                ChangeFactor, 
                Opts
            ),
            {ok, hb_ao:set(Base, #{ <<"routes">> => UpdatedRoutes }, Opts)}
    end.

%% @doc Register a new node on a route. Supports both:
%% - `match'/`with' format 
%% - `prefix'/`price'/`topup' format 
%% Generates `http_reference' from the node's URL (with or prefix).
register(RawBase, Req, Opts) ->
    Base = ensure_defaults(RawBase, Opts),
    Body = hb_ao:get(<<"body">>, Req, Req, Opts),
    Route = hb_ao:get(<<"route">>, Body, Opts),
    Template = hb_ao:get(<<"template">>, Route, Opts),
    Routes = hb_ao:get(<<"routes">>, Base, [], Opts),
    InitialPerf = 
        to_float(hb_ao:get(<<"initial-performance">>, Base, 30000, Opts)),
    HttpRef = node_url(Route, Opts),
    Node = build_node(Route, HttpRef, InitialPerf, Opts),
    RouteConfig = extract_route_config(Route, Opts),
    {UpdatedRoutes, _} = 
        add_node_to_route(Routes, Template, Node, RouteConfig, Opts),
    NewBase = hb_ao:set(Base, #{ <<"routes">> => UpdatedRoutes }, Opts),
    recalculate(NewBase, Req, Opts).

%% @doc Recompute weights for all nodes in all routes.
%% Weight = inverse performance -- lower ms = higher weight, normalized.
recalculate(RawBase, _Req, Opts) ->
    Base = ensure_defaults(RawBase, Opts),
    Routes = hb_ao:get(<<"routes">>, Base, [], Opts),
    SamplingRate = to_float(hb_ao:get(<<"sampling-rate">>, Base, 0.1, Opts)),
    PerfWeight = to_float(hb_ao:get(<<"performance-weight">>, Base, 1, Opts)),
    PricingWeight = to_float(hb_ao:get(<<"pricing-weight">>, Base, 1, Opts)),
    ScorePref = to_float(hb_ao:get(<<"score-preference">>, Base, 1, Opts)),
    ScoringParams = #{
        sampling_rate => SamplingRate,
        perf_weight => PerfWeight,
        pricing_weight => PricingWeight,
        score_pref => ScorePref
    },
    UpdatedRoutes = lists:map(
        fun(R) ->
            Nodes = hb_ao:get(<<"nodes">>, R, [], Opts),
            recalculate_route(R, ScoringParams, Opts, Nodes)
        end,
        Routes
    ),
    {ok, hb_ao:set(Base, #{ <<"routes">> => UpdatedRoutes }, Opts)}.

%%% Internal functions

-define(
    ROUTE_CONFIG_KEYS, 
    [
        <<"strategy">>, 
        <<"parallel">>, 
        <<"choose">>,
        <<"admissible-status">>
    ]
).

%% @doc Extract route-level configuration keys from a registration message.
extract_route_config(Route, Opts) ->
    lists:foldl(
        fun(Key, Acc) ->
            case hb_ao:get(Key, Route, not_found, Opts) of
                not_found -> Acc;
                Value -> Acc#{ Key => Value }
            end
        end,
        #{},
        ?ROUTE_CONFIG_KEYS
    ).

ensure_defaults(Base, Opts) ->
    Defaults = [
        {<<"routes">>, []},
        {<<"sampling-rate">>, 0.1},
        {<<"pricing-weight">>, 1},
        {<<"performance-weight">>, 1},
        {<<"score-preference">>, 1},
        {<<"performance-period">>, 1000},
        {<<"initial-performance">>, 30000}
    ],
    lists:foldl(
        fun({Key, Default}, Acc) ->
            case hb_ao:get(Key, Acc, not_found, Opts) of
                not_found -> hb_ao:set(Acc, #{Key => Default}, Opts);
                _ -> Acc
            end
        end,
        Base,
        Defaults
    ).


node_url(Node, Opts) ->
    case hb_ao:get(<<"with">>, Node, not_found, Opts) of
        not_found -> hb_ao:get(<<"prefix">>, Node, <<"unknown">>, Opts);
        With -> With
    end.

%% @doc Build a node map from a registration route.
%% `http_reference' is stored at the top level AND in opts.
build_node(Route, HttpRef, InitialPerf, Opts) ->
    ExistingNodeOpts = 
        case hb_ao:get(<<"opts">>, Route, not_found, Opts) of
            NodeOpts when is_map(NodeOpts) ->
                %% TODO: PLEASE CROSS CHECK ONCE I THINK IT's OKAY BUT VERIFY
                %% Strip old commitments so http_reference gets included
                %% when the process pipeline commits the full state.
                hb_maps:without([<<"commitments">>], NodeOpts, Opts);
            _ -> #{}
        end,
    Node = #{
        <<"performance">> => InitialPerf,
        <<"price">> => to_float(hb_ao:get(<<"price">>, Route, 0, Opts)),
        <<"weight">> => 1.0,
        <<"http_reference">> => HttpRef,
        <<"opts">> => ExistingNodeOpts#{ <<"http_reference">> => HttpRef }
    },
    WithMatch = 
        case hb_ao:get(<<"with">>, Route, not_found, Opts) of
            not_found -> #{};
            With -> 
                #{
                    <<"with">> => With,
                    <<"match">> => hb_ao:get(<<"match">>, Route, <<"">>, Opts)
                }
        end,
    WithPrefix = 
        case hb_ao:get(<<"prefix">>, Route, not_found, Opts) of
            not_found -> #{};
            Prefix -> #{ <<"prefix">> => Prefix }
        end,
    hb_maps:merge(hb_maps:merge(Node, WithMatch, Opts), WithPrefix, Opts).

%% @doc Append a node to an existing route or create a new one.
add_node_to_route(Routes, Template, Node, RouteConfig, Opts) ->
    case find_route_index(Routes, Template, Opts) of
        not_found ->
            BaseRoute = #{
                <<"template">> => Template,
                <<"strategy">> => <<"By-Weight">>,
                <<"nodes">> => [Node]
            },
            NewRoute = hb_maps:merge(BaseRoute, RouteConfig, Opts),
            {Routes ++ [NewRoute], length(Routes) + 1};
        Idx ->
            Route = lists:nth(Idx, Routes),
            ExistingNodes = hb_ao:get(<<"nodes">>, Route, [], Opts),
            UpdatedRoute = 
                hb_ao:set(
                    Route,
                    #{ <<"nodes">> => ExistingNodes ++ [Node] },
                    Opts
                ),
            MergedRoute = hb_maps:merge(UpdatedRoute, RouteConfig, Opts),
            {list_replace(Routes, Idx, MergedRoute), Idx}
    end.

%% @doc Find the 1-based index of the route matching Template, or not_found.
find_route_index(Routes, Template, Opts) ->
    find_route_index(Routes, Template, 1, Opts).
find_route_index([], _Template, _Idx, _Opts) -> not_found;
find_route_index([Route | Rest], Template, Idx, Opts) ->
    case hb_ao:get(<<"template">>, Route, undefined, Opts) of
        Template -> Idx;
        _ -> find_route_index(Rest, Template, Idx + 1, Opts)
    end.

%% @doc Replace the element at 1-based Idx in List with Value.
list_replace(List, Idx, Value) ->
    {Before, [_ | After]} = lists:split(Idx - 1, List),
    Before ++ [Value | After].

%% @doc Apply a duration update to the node matching Reference across all routes.
update_node_performance(Routes, Reference, Duration, ChangeFactor, Opts) ->
    lists:map(
        fun(Route) ->
            Nodes = hb_ao:get(<<"nodes">>, Route, [], Opts),
            UpdatedNodes = 
                [
                    apply_node_perf(Node, Reference, Duration, ChangeFactor, Opts)
                || 
                    Node <- Nodes
                ],
            hb_ao:set(Route, #{ <<"nodes">> => UpdatedNodes }, Opts)
        end,
        Routes
    ).

%% @doc Update a single node's performance if its http_reference matches.
apply_node_perf(Node, Reference, Duration, ChangeFactor, Opts) ->
    case hb_ao:get(<<"http_reference">>, Node, not_found, Opts) of
        Reference ->
            OldPerf = to_float(hb_ao:get(<<"performance">>, Node, 30000, Opts)),
            NewPerf = (OldPerf * (1.0 - ChangeFactor)) + (Duration * ChangeFactor),
            hb_ao:set(Node, #{ <<"performance">> => NewPerf }, Opts);
        _ ->
            Node
    end.

%% @doc Recompute weights for all nodes in a single route.
recalculate_route(Route, ScoringParams, Opts, Nodes) when is_list(Nodes) ->
    #{
        perf_weight := PerfW,
        pricing_weight := PriceW,
        sampling_rate := SamplingRate,
        score_pref := ScorePref
    } = ScoringParams,
    TotalWeight = PerfW + PriceW,
    PerfFactor = PerfW / TotalWeight,
    PriceFactor = PriceW / TotalWeight,
    PerfValues = 
        [
            to_float(hb_ao:get(<<"performance">>, N, 30000, Opts))
        || 
            N <- Nodes
        ],
    PriceValues = 
        [
            to_float(hb_ao:get(<<"price">>, N, 0, Opts))
        || 
            N <- Nodes
        ],
    SortedPerf = lists:sort(PerfValues),
    SortedPrice = lists:sort(PriceValues),
    ScoredNodes = 
        lists:map(
            fun(Node) ->
                Perf = 
                    to_float(hb_ao:get(<<"performance">>, Node, 30000, Opts)),
                Price = 
                    to_float(hb_ao:get(<<"price">>, Node, 0, Opts)),
                PerfPercentile = percentile(Perf, SortedPerf),
                PricePercentile = percentile(Price, SortedPrice),
                PerfScore = 
                    (decay(ScorePref, PerfPercentile) * (1.0 - SamplingRate)) 
                    + SamplingRate,
                PriceScore = decay(ScorePref, PricePercentile),
                Weight = (PerfScore * PerfFactor) + (PriceScore * PriceFactor),
                hb_ao:set(Node, #{ <<"weight">> => Weight }, Opts)
            end,
            Nodes
        ),
    hb_ao:set(Route, #{ <<"nodes">> => ScoredNodes }, Opts);
recalculate_route(Route, _ScoringParams, _Opts, _Nodes) ->
    Route.

%% @doc Exponential decay function for score weighting.
decay(Preference, Score) ->
    math:exp(-Preference * Score).

%% @doc Compute the percentile rank of Value in a sorted list.
percentile(_Value, []) -> 0.0;
percentile(_Value, [_]) -> 0.0;
percentile(Value, Sorted) ->
    Pos = count_less_or_equal(Value, Sorted, 0),
    (Pos - 1) / length(Sorted).

count_less_or_equal(_Value, [], Count) -> Count;
count_less_or_equal(Value, [H | T], Count) when H =< Value ->
    count_less_or_equal(Value, T, Count + 1);
count_less_or_equal(_Value, _Rest, Count) -> Count.

to_float(V) when is_float(V) -> V;
to_float(V) when is_integer(V) -> float(V);
to_float(V) when is_binary(V) ->
    try binary_to_float(V)
    catch _:_ ->
        try float(binary_to_integer(V))
        catch _:_ -> 0.0
        end
    end;
to_float(V) when is_list(V) ->
    try list_to_float(V)
    catch _:_ ->
        try float(list_to_integer(V))
        catch _:_ -> 0.0
        end
    end;
to_float(_) -> 0.0.

%%% ============================================================
%%% Tests
%%% ============================================================

register_test() ->
    Base = #{},
    Req = #{
        <<"route">> => #{
            <<"template">> => <<"/test-key">>,
            <<"prefix">> => <<"host1">>,
            <<"price">> => 5
        }
    },
    {ok, Base2} = register(Base, Req, #{}),
    Routes = hb_maps:get(<<"routes">>, Base2, [], #{}),
    ?assertEqual(1, length(Routes)),
    [Route] = Routes,
    Nodes = hb_maps:get(<<"nodes">>, Route, [], #{}),
    ?assertEqual(1, length(Nodes)),
    [Node] = Nodes,
    ?assertEqual(<<"host1">>, hb_maps:get(<<"prefix">>, Node, undefined, #{})),
    ?assertEqual(5.0, hb_maps:get(<<"price">>, Node, undefined, #{})).

register_match_with_test() ->
    Base = #{},
    Req = #{
        <<"route">> => #{
            <<"template">> => <<"^/arweave">>,
            <<"match">> => <<"^/arweave">>,
            <<"with">> => <<"http://chain-1.arweave.xyz:1984">>
        }
    },
    {ok, Base2} = register(Base, Req, #{}),
    Routes = hb_maps:get(<<"routes">>, Base2, [], #{}),
    ?assertEqual(1, length(Routes)),
    [Route] = Routes,
    Nodes = hb_maps:get(<<"nodes">>, Route, [], #{}),
    ?assertEqual(1, length(Nodes)),
    [Node] = Nodes,
    ?assertEqual(<<"http://chain-1.arweave.xyz:1984">>,
        hb_maps:get(<<"with">>, Node, undefined, #{})),
    ?assertEqual(<<"^/arweave">>,
        hb_maps:get(<<"match">>, Node, undefined, #{})),
    ?assertEqual(<<"http://chain-1.arweave.xyz:1984">>,
        hb_maps:get(<<"http_reference">>, Node, undefined, #{})),
    NodeOpts = hb_maps:get(<<"opts">>, Node, #{}, #{}),
    ?assertEqual(<<"http://chain-1.arweave.xyz:1984">>,
        hb_maps:get(<<"http_reference">>, NodeOpts, undefined, #{})).

duration_test() ->
    Base = #{
        <<"performance-period">> => 2,
        <<"initial-performance">> => 1000,
        <<"routes">> => [
            #{
                <<"template">> => <<"^/arweave">>,
                <<"strategy">> => <<"By-Weight">>,
                <<"nodes">> => [
                    #{
                        <<"with">> => <<"http://chain-1.arweave.xyz:1984">>,
                        <<"match">> => <<"^/arweave">>,
                        <<"performance">> => 1000.0,
                        <<"price">> => 0,
                        <<"weight">> => 1.0,
                        <<"http_reference">> =>
                            <<"http://chain-1.arweave.xyz:1984">>,
                        <<"opts">> => #{
                            <<"http_reference">> =>
                                <<"http://chain-1.arweave.xyz:1984">>
                        }
                    }
                ]
            }
        ]
    },
    Req = #{
        <<"duration">> => 200,
        <<"reference">> => <<"http://chain-1.arweave.xyz:1984">>
    },
    {ok, Base2} = duration(Base, Req, #{}),
    Routes = hb_maps:get(<<"routes">>, Base2, [], #{}),
    [Route] = Routes,
    [Node] = hb_maps:get(<<"nodes">>, Route, [], #{}),
    %% With period=2, change_factor=0.5.
    %% new_perf = 1000 * 0.5 + 200 * 0.5 = 600.
    ?assertEqual(600.0, hb_maps:get(<<"performance">>, Node, undefined, #{})).

duration_no_reference_test() ->
    Base = #{ <<"routes">> => [] },
    Req = #{
        <<"duration">> => 200
    },
    {ok, Base2} = duration(Base, Req, #{}),
    ?assertEqual(Base2, ensure_defaults(Base, #{})).

recalculate_test() ->
    Base = #{
        <<"performance-period">> => 6,
        <<"initial-performance">> => 30000,
        <<"performance-weight">> => 1,
        <<"pricing-weight">> => 1,
        <<"sampling-rate">> => 0.1,
        <<"score-preference">> => 1,
        <<"routes">> => [
            #{
                <<"template">> => <<"/test">>,
                <<"strategy">> => <<"By-Weight">>,
                <<"nodes">> => [
                    #{
                        <<"prefix">> => <<"host1">>,
                        <<"performance">> => 200.0,
                        <<"price">> => 5,
                        <<"weight">> => 1.0,
                        <<"http_reference">> => <<"host1">>
                    },
                    #{
                        <<"prefix">> => <<"host2">>,
                        <<"performance">> => 55500.0,
                        <<"price">> => 5,
                        <<"weight">> => 1.0,
                        <<"http_reference">> => <<"host2">>
                    }
                ]
            }
        ]
    },
    {ok, Base2} = recalculate(Base, #{}, #{}),
    Routes = hb_maps:get(<<"routes">>, Base2, [], #{}),
    [Route] = Routes,
    [N1, N2] = hb_maps:get(<<"nodes">>, Route, [], #{}),
    W1 = hb_maps:get(<<"weight">>, N1, undefined, #{}),
    W2 = hb_maps:get(<<"weight">>, N2, undefined, #{}),
    ?assert(W1 > W2).

hb_gateway_load_balancer_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(hb),
        ID = <<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>,
        PerfProcess = <<"/perf-router~node-process@1.0">>,
        SchedulePath = <<PerfProcess/binary, "/schedule">>,
        RoutesPath = <<PerfProcess/binary, "/now/routes">>,
        HBGateways = [
            #{ 
                <<"match">> => <<"^/">>,
                <<"with">> => <<"https://blue.hyperbeam.zephyrdev.xyz/">> 
            },
            #{ 
                <<"match">> => <<"^/">>,
                <<"with">> => <<"https://neo.hyperbeam.zephyrdev.xyz/">> 
            },
            #{ <<"match">> => <<"^/">>,
               <<"with">> => <<"https://neo2.hyperbeam.zephyrdev.xyz/">> }
        ],
        Opts = #{
            store => hb_test_utils:test_store(),
            priv_wallet => ar_wallet:new(),
            router_opts => #{
                <<"provider">> => #{ <<"path">> => RoutesPath }
            },
            on => #{
                <<"request">> => #{
                    <<"device">> => <<"router@1.0">>,
                    <<"path">> => <<"preprocess">>
                },
                <<"http-client">> => #{
                    <<"response">> => [
                        #{
                            <<"device">> => <<"relay@1.0">>,
                            <<"path">> => <<"call">>,
                            <<"method">> => <<"POST">>,
                            <<"relay-path">> => SchedulePath,
                            <<"hook/result">> => <<"ignore">>
                        }
                    ]
                }
            },
            node_processes => #{
                <<"perf-router">> => #{
                    <<"device">> => <<"process@1.0">>,
                    <<"execution-device">> => <<"router-perf@1.0">>,
                    <<"scheduler-device">> => <<"scheduler@1.0">>,
                    <<"performance-period">> => 2,
                    <<"initial-performance">> => 1000
                }
            }
        },
        Node = hb_http_server:start_node(Opts),
        %% Register HB gateways with the perf-router process.
        RouteConfig = #{
            <<"template">> => <<"^/(?!.*~)">>,
            <<"strategy">> => <<"By-Weight">>,
            <<"choose">> => 2
        },
        lists:foreach(
            fun(GW) ->
                Body = 
                    hb_message:commit(
                        #{ 
                            <<"action">> => <<"register">>,
                            <<"route">> => maps:merge(GW, RouteConfig) 
                        },
                        Opts
                    ),
                {ok, _} = 
                    hb_http:post(
                        Node,
                        #{ 
                            <<"path">> => SchedulePath,
                            <<"method">> => <<"POST">>,
                            <<"body">> => Body 
                        }, 
                        Opts
                    )
            end,
            HBGateways
        ),
        {ok, _} = hb_http:get(Node, RoutesPath, Opts),
        {ok, InitPerf} = 
            hb_http:get(
                Node,
                <<PerfProcess/binary, "/now/routes/1/nodes/1/performance">>,
                Opts
            ),
        ?assertEqual(1000.0, to_float(InitPerf)),
        {ok, Res} = hb_http:get(Node, <<"/", ID/binary>>, Opts),
        ?assert(is_map(Res)),
        timer:sleep(2000),
        {ok, _} = hb_http:get(Node, RoutesPath, Opts),
        Perfs = 
            lists:map(
                fun(I) ->
                    IB = integer_to_binary(I),
                    {ok, V} = 
                        hb_http:get(
                            Node,
                            <<
                                PerfProcess/binary,
                                "/now/routes/1/nodes/", 
                                IB/binary,
                                "/performance"
                            >>, 
                        Opts
                    ),
                    to_float(V)
                end,
                [1, 2, 3]
            ),
        ?assert(lists:any(fun(P) -> P =/= 1000.0 end, Perfs)),
        RecalcBody = 
            hb_message:commit(#{ <<"action">> => <<"recalculate">> }, Opts),
        {ok, _} = 
            hb_http:post(
                Node,
                #{ 
                    <<"path">> => SchedulePath,
                    <<"method">> => <<"POST">>,
                    <<"body">> => RecalcBody 
                }, 
                Opts
            ),
        {ok, _} = hb_http:get(Node, RoutesPath, Opts),
        Weights = 
            lists:map(
                fun(I) ->
                    IB = integer_to_binary(I),
                    {ok, W} = 
                        hb_http:get(
                            Node,
                            <<
                                PerfProcess/binary,
                                "/now/routes/1/nodes/", 
                                IB/binary,
                                "/weight"
                            >>, 
                            Opts
                        ),
                    to_float(W)
                end,
                [1, 2, 3]
            ),
        UniqueWeights = lists:usort(Weights),
        ?assert(length(UniqueWeights) > 1),
        ok
    end}.