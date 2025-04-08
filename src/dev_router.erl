-module(dev_router).
-moduledoc """
A device that routes outbound messages from the node to their
appropriate network recipients via HTTP. All messages are initially
routed to a single process per node, which then load-balances them
between downstream workers that perform the actual requests.

The routes for the router are defined in the `routes' key of the `Opts',
as a precidence-ordered list of maps. The first map that matches the
message will be used to determine the route.

Multiple nodes can be specified as viable for a single route, with the
`Choose' key determining how many nodes to choose from the list (defaulting
to 1). The `Strategy' key determines the load distribution strategy,
which can be one of `Random', `By-Base', or `Nearest'. The route may also 
define additional parallel execution parameters, which are used by the
`hb_http' module to manage control of requests.

The structure of the routes should be as follows:
```
    Node?: The node to route the message to.
    Nodes?: A list of nodes to route the message to.
    Strategy?: The load distribution strategy to use.
    Choose?: The number of nodes to choose from the list.
    Template?: A message template to match the message against, either as a
               map or a path regex.
'''
""".
%%% Device API:
-export([routes/3, route/2, route/3]).
%%% Public utilities:
-export([match_routes/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-doc """
Device function that returns all known routes.
""".
routes(M1, M2, Opts) ->
    ?event({routes_msg, M1, M2}),
    Routes = hb_opts:get(routes, [], Opts),
    ?event({routes, Routes}),
    case hb_ao:get(<<"method">>, M2, Opts) of
        <<"POST">> ->
            Owner = hb_opts:get(operator, undefined, Opts),
            RouteOwners = hb_opts:get(route_owners, [Owner], Opts),
            Signers = hb_message:signers(M2),
            IsTrusted =
                lists:any(
                    fun(Signer) -> lists:member(Signer, Signers) end,
                    RouteOwners
                ),
            case IsTrusted of
                true ->
                    % Minimize the work performed by AO-Core to make the sort
                    % more efficient.
                    SortOpts = Opts#{ hashpath => ignore },
                    NewRoutes =
                        lists:sort(
                            fun(X, Y) ->
                                hb_ao:get(<<"priority">>, X, SortOpts)
                                    < hb_ao:get(<<"priority">>, Y, SortOpts)
                            end,
                            [M2|Routes]
                        ),
                    ok = hb_http_server:set_opts(Opts#{ routes => NewRoutes }),
                    {ok, <<"Route added.">>};
                false -> {error, not_authorized}
            end;
        _ ->
            {ok, Routes}
    end.

-doc """
Find the appropriate route for the given message. If we are able to 
resolve to a single host+path, we return that directly. Otherwise, we return
the matching route (including a list of nodes under `nodes') from the list of
routes.

If we have a route that has multiple resolving nodes, check
the load distribution strategy and choose a node. Supported strategies:
```
      All:     Return all nodes (default).
      Random:  Distribute load evenly across all nodes, non-deterministically.
      By-Base: According to the base message's hashpath.
      Nearest: According to the distance of the node's wallet address to the
               base message's hashpath.
'''
`By-Base' will ensure that all traffic for the same hashpath is routed to the
same node, minimizing work duplication, while `Random' ensures a more even
distribution of the requests.

Can operate as a `~router@1.0' device, which will ignore the base message,
routing based on the Opts and request message provided, or as a standalone
function, taking only the request message and the `Opts' map.
""".
route(Msg, Opts) -> route(undefined, Msg, Opts).
route(_, Msg, Opts) ->
    Routes = hb_opts:get(routes, [], Opts),
    R = match_routes(Msg, Routes, Opts),
    ?event({find_route, {msg, Msg}, {routes, Routes}, {res, R}}),
    case (R =/= no_matches) andalso hb_ao:get(<<"node">>, R, Opts) of
        false -> {error, no_matches};
        Node when is_binary(Node) -> {ok, Node};
        Node when is_map(Node) -> apply_route(Msg, Node);
        not_found ->
            ModR = apply_routes(Msg, R, Opts),
            case hb_ao:get(<<"strategy">>, R, Opts) of
                not_found -> {ok, ModR};
                <<"All">> -> {ok, ModR};
                Strategy ->
                    ChooseN = hb_ao:get(<<"choose">>, R, 1, Opts),
                    % Get the first element of the path -- the `base' message
                    % of the request.
                    Base = extract_base(Msg, Opts),
                    Nodes = hb_ao:get(<<"nodes">>, ModR, Opts),
                    Chosen = choose(ChooseN, Strategy, Base, Nodes, Opts),
                    ?event({choose, {strategy, Strategy}, {choose_n, ChooseN}, {base, Base}, {nodes, Nodes}, {chosen, Chosen}}),
                    case Chosen of
                        [Node] when is_map(Node) ->
                            apply_route(Msg, Node);
                        [NodeURI] -> {ok, NodeURI};
                        ChosenNodes ->
                            {ok,
                                hb_ao:set(
                                    <<"nodes">>,
                                    maps:map(
                                        fun(Node) ->
                                            hb_util:ok(apply_route(Msg, Node))
                                        end,
                                        Chosen
                                    ),
                                    Opts
                                )
                            }
                    end
            end
    end.

-doc """
Extract the base message ID from a request message. Produces a single
binary ID that can be used for routing decisions.
""".
extract_base(#{ <<"path">> := Path }, Opts) ->
    extract_base(Path, Opts);
extract_base(RawPath, Opts) when is_binary(RawPath) ->
    BasePath = hb_path:hd(#{ <<"path">> => RawPath }, Opts),
    case ?IS_ID(BasePath) of
        true -> BasePath;
        false ->
            case binary:split(BasePath, [<<"~">>, <<"?">>, <<"&">>], [global]) of
                [BaseMsgID|_] when ?IS_ID(BaseMsgID) -> BaseMsgID;
                _ -> hb_crypto:sha256(BasePath)
            end
    end.

-doc """
Generate a `uri' key for each node in a route.
""".
apply_routes(Msg, R, Opts) ->
    Nodes = hb_ao:get(<<"nodes">>, R, Opts),
    NodesWithRouteApplied =
        lists:map(
            fun(N) ->
                case apply_route(Msg, N) of
                    {ok, URI} when is_binary(URI) -> N#{ <<"uri">> => URI };
                    {ok, Map} -> Map;
                    {error, _} -> N
                end
            end,
            hb_util:message_to_ordered_list(Nodes)
        ),
    R#{ <<"nodes">> => NodesWithRouteApplied }.

-doc """
Apply a node map's rules for transforming the path of the message.
Supports the following keys:
- `opts': A map of options to pass to the request.
- `prefix': The prefix to add to the path.
- `suffix': The suffix to add to the path.
- `replace': A regex to replace in the path.
""".
apply_route(Msg, Route = #{ <<"opts">> := Opts }) ->
    {ok, #{
        <<"opts">> => Opts,
        <<"uri">> => hb_util:ok(apply_route(Msg, maps:without([<<"opts">>], Route)))
    }};
apply_route(#{ <<"path">> := Path }, #{ <<"prefix">> := Prefix }) ->
    {ok, <<Prefix/binary, Path/binary>>};
apply_route(#{ <<"path">> := Path }, #{ <<"suffix">> := Suffix }) ->
    {ok, <<Path/binary, Suffix/binary>>};
apply_route(#{ <<"path">> := Path }, #{ <<"match">> := Match, <<"with">> := With }) ->
    % Apply the regex to the path and replace the first occurrence.
    case re:replace(Path, Match, With, [global]) of
        NewPath when is_binary(NewPath) ->
            {ok, NewPath};
        _ -> {error, invalid_replace_args}
    end.

-doc """
Find the first matching template in a list of known routes.
""".
match_routes(ToMatch, Routes, Opts) ->
    match_routes(
        ToMatch,
        Routes,
        hb_ao:keys(hb_ao:normalize_keys(Routes)),
        Opts
    ).
match_routes(#{ <<"path">> := Explicit = <<"http://", _/binary>> }, _, _, _) ->
    % If the route is an explicit HTTP URL, we can match it directly.
    #{ <<"node">> => Explicit };
match_routes(#{ <<"path">> := Explicit = <<"https://", _/binary>> }, _, _, _) ->
    #{ <<"node">> => Explicit };
match_routes(_, _, [], _) -> no_matches;
match_routes(ToMatch, Routes, [XKey|Keys], Opts) ->
    XM = hb_ao:get(XKey, Routes, Opts),
    Template =
        hb_ao:get(
            <<"template">>,
            XM,
            #{},
            Opts#{ hashpath => ignore }
        ),
    case template_matches(ToMatch, Template) of
        true -> XM;
        false -> match_routes(ToMatch, Routes, Keys, Opts)
    end.

-doc """
Check if a message matches a message template or path regex.
""".
template_matches(ToMatch, Template) when is_map(Template) ->
    hb_message:match(Template, ToMatch, primary);
template_matches(ToMatch, Regex) when is_binary(Regex) ->
    MsgPath = (hb_path:from_message(request, ToMatch)),
    hb_path:regex_matches(MsgPath, Regex).

-doc """
Implements the load distribution strategies if given a cluster.
""".
choose(0, _, _, _, _) -> [];
choose(N, <<"Random">>, _, Nodes, _Opts) ->
    Node = lists:nth(rand:uniform(length(Nodes)), Nodes),
    [Node | choose(N - 1, <<"Random">>, nop, lists:delete(Node, Nodes), _Opts)];
choose(N, <<"By-Base">>, Hashpath, Nodes, Opts) when is_binary(Hashpath) ->
    choose(N, <<"By-Base">>, binary_to_bignum(Hashpath), Nodes, Opts);
choose(N, <<"By-Base">>, HashInt, Nodes, Opts) ->
    Node = lists:nth((HashInt rem length(Nodes)) + 1, Nodes),
    [
        Node
    |
        choose(
            N - 1,
            <<"By-Base">>,
            HashInt,
            lists:delete(Node, Nodes),
            Opts
        )
    ];
choose(N, <<"Nearest">>, HashPath, Nodes, Opts) ->
    BareHashPath = hb_util:native_id(HashPath),
    NodesWithDistances =
        lists:map(
            fun(Node) ->
                Wallet = hb_ao:get(<<"wallet">>, Node, Opts),
                DistanceScore =
                    field_distance(
                        hb_util:native_id(Wallet),
                        BareHashPath
                    ),
                {Node, DistanceScore}
            end,
            Nodes
        ),
    lists:reverse(
        element(1,
            lists:foldl(
                fun(_, {Current, Remaining}) ->
                    Res = {Lowest, _} = lowest_distance(Remaining),
                    {[Lowest|Current], lists:delete(Res, Remaining)}
                end,
                {[], NodesWithDistances},
                lists:seq(1, N)
            )
        )
    ).

-doc """
Calculate the minimum distance between two numbers
(either progressing backwards or forwards), assuming a
256-bit field.
""".
field_distance(A, B) when is_binary(A) ->
    field_distance(binary_to_bignum(A), B);
field_distance(A, B) when is_binary(B) ->
    field_distance(A, binary_to_bignum(B));
field_distance(A, B) ->
    AbsDiff = abs(A - B),
    min(AbsDiff, (1 bsl 256) - AbsDiff).

-doc """
Find the node with the lowest distance to the given hashpath.
""".
lowest_distance(Nodes) -> lowest_distance(Nodes, {undefined, infinity}).
lowest_distance([], X) -> X;
lowest_distance([{Node, Distance}|Nodes], {CurrentNode, CurrentDistance}) ->
    case Distance of
        infinity -> lowest_distance(Nodes, {Node, Distance});
        _ when Distance < CurrentDistance ->
            lowest_distance(Nodes, {Node, Distance});
        _ -> lowest_distance(Nodes, {CurrentNode, CurrentDistance})
    end.

-doc """
Cast a human-readable or native-encoded ID to a big integer.
""".
binary_to_bignum(Bin) when ?IS_ID(Bin) ->
    << Num:256/unsigned-integer >> = hb_util:native_id(Bin),
    Num.

%%% Tests

strategy_suite_test_() ->
    lists:map(
        fun(Strategy) ->
            {foreach,
                fun() -> ok end,
                fun(_) -> ok end,
                [
                    {
                        binary_to_list(Strategy) ++ ": " ++ Desc,
                        fun() -> Test(Strategy) end
                    }
                ||
                    {Desc, Test} <- [
                        {"unique", fun unique_test/1},
                        {"choose 1", fun choose_1_test/1},
                        {"choose n", fun choose_n_test/1}
                    ]
                ]
            }
        end,
        [<<"Random">>, <<"By-Base">>, <<"Nearest">>]
    ).

-doc """
Ensure that `By-Base' always chooses the same node for the same
hashpath.
""".
by_base_determinism_test() ->
    FirstN = 5,
    Nodes = generate_nodes(5),
    HashPaths = generate_hashpaths(100),
    Simulation = simulate(HashPaths, FirstN, Nodes, <<"By-Base">>),
    Simulation2 = simulate(HashPaths, FirstN, Nodes, <<"By-Base">>),
    ?assertEqual(Simulation, Simulation2).

unique_test(Strategy) ->
    TestSize = 1,
    FirstN = 5,
    Nodes = generate_nodes(5),
    Simulation = simulate(TestSize, FirstN, Nodes, Strategy),
    unique_nodes(Simulation).

choose_1_test(Strategy) ->
    TestSize = 1500,
    Nodes = generate_nodes(20),
    Simulation = simulate(TestSize, 1, Nodes, Strategy),
    within_norms(Simulation, Nodes, TestSize).

choose_n_test(Strategy) ->
    TestSize = 1500,
    FirstN = 5,
    Nodes = generate_nodes(20),
    Simulation = simulate(TestSize, FirstN, Nodes, Strategy),
    within_norms(Simulation, Nodes, TestSize * 5),
    unique_nodes(Simulation).

unique_nodes(Simulation) ->
    lists:foreach(
        fun(SelectedNodes) ->
            lists:foreach(
                fun(Node) ->
                    ?assertEqual(1, hb_util:count(Node, SelectedNodes))
                end,
                SelectedNodes
            )
        end,
        Simulation
    ).

route_template_message_matches_test() ->
    Routes = [
        #{
            <<"template">> => #{ <<"other-key">> => <<"other-value">> },
            <<"node">> => <<"incorrect">>
        },
        #{
            <<"template">> => #{ <<"special-key">> => <<"special-value">> },
            <<"node">> => <<"correct">>
        }
    ],
    ?assertEqual(
        {ok, <<"correct">>},
        route(
            #{ <<"path">> => <<"/">>, <<"special-key">> => <<"special-value">> },
            #{ routes => Routes }
        )
    ),
    ?assertEqual(
        {error, no_matches},
        route(
            #{ <<"path">> => <<"/">>, <<"special-key">> => <<"special-value2">> },
            #{ routes => Routes }
        )
    ),
    ?assertEqual(
        {ok, <<"fallback">>},
        route(
            #{ <<"path">> => <<"/">> },
            #{ routes => Routes ++ [#{ <<"node">> => <<"fallback">> }] }
        )
    ).

route_regex_matches_test() ->
    Routes = [
        #{
            <<"template">> => <<"/.*/compute">>,
            <<"node">> => <<"incorrect">>
        },
        #{
            <<"template">> => <<"/.*/schedule">>,
            <<"node">> => <<"correct">>
        }
    ],
    ?assertEqual(
        {ok, <<"correct">>},
        route(#{ <<"path">> => <<"/abc/schedule">> }, #{ routes => Routes })
    ),
    ?assertEqual(
        {ok, <<"correct">>},
        route(#{ <<"path">> => <<"/a/b/c/schedule">> }, #{ routes => Routes })
    ),
    ?assertEqual(
        {error, no_matches},
        route(#{ <<"path">> => <<"/a/b/c/bad-key">> }, #{ routes => Routes })
    ).

explicit_route_test() ->
    Routes = [
        #{
            <<"template">> => <<"*">>,
            <<"node">> => <<"unimportant">>
        }
    ],
    ?assertEqual(
        {ok, <<"https://google.com">>},
        route(
            #{ <<"path">> => <<"https://google.com">> },
            #{ routes => Routes }
        )
    ),
    ?assertEqual(
        {ok, <<"http://google.com">>},
        route(
            #{ <<"path">> => <<"http://google.com">> },
            #{ routes => Routes }
        )
    ).

device_call_from_singleton_test() ->
    % Try with a real-world example, taken from a GET request to the router.
    NodeOpts = #{ routes => Routes = [#{
        <<"template">> => <<"/some/path">>,
        <<"node">> => <<"old">>,
        <<"priority">> => 10
    }]},
    Msgs = hb_singleton:from(#{ <<"path">> => <<"~router@1.0/routes">> }),
    ?event({msgs, Msgs}),
    ?assertEqual(
        {ok, Routes},
        hb_ao:resolve_many(Msgs, NodeOpts)
    ).
    

get_routes_test() ->
    Node = hb_http_server:start_node(
        #{
            force_signed => false,
            routes => [
                #{
                    <<"template">> => <<"*">>,
                    <<"node">> => <<"our_node">>,
                    <<"priority">> => 10
                }
            ]
        }
    ),
    Res = hb_http:get(Node, <<"/~router@1.0/routes/1/node">>, #{}),
    ?event({get_routes_test, Res}),
    {ok, Recvd} = Res,
    ?assertMatch(<<"our_node">>, Recvd).

add_route_test() ->
    Owner = ar_wallet:new(),
    Node = hb_http_server:start_node(
        #{
            force_signed => false,
            routes => [
                #{
                    <<"template">> => <<"/some/path">>,
                    <<"node">> => <<"old">>,
                    <<"priority">> => 10
                }
            ],
            operator => hb_util:encode(ar_wallet:to_address(Owner))
        }
    ),
    Res =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~router@1.0/routes">>,
                    <<"template">> => <<"/some/new/path">>,
                    <<"node">> => <<"new">>,
                    <<"priority">> => 15
                },
                Owner
            ),
            #{}
        ),
    ?event({post_res, Res}),
    ?assertMatch({ok, <<"Route added.">>}, Res),
    GetRes = hb_http:get(Node, <<"/~router@1.0/routes/2/node">>, #{}),
    ?event({get_res, GetRes}),
    {ok, Recvd} = GetRes,
    ?assertMatch(<<"new">>, Recvd).

relay_nearest_test() ->
    Peer1 = <<"https://compute-1.forward.computer">>,
    Peer2 = <<"https://compute-2.forward.computer">>,
    HTTPSOpts = #{ http_client => httpc },
    {ok, Address1} = hb_http:get(Peer1, <<"/~meta@1.0/info/address">>, HTTPSOpts),
    {ok, Address2} = hb_http:get(Peer2, <<"/~meta@1.0/info/address">>, HTTPSOpts),
    Peers = [Address1, Address2],
    Node =
        hb_http_server:start_node(#{
            priv_wallet => ar_wallet:new(),
            routes => [
                #{
                    <<"template">> => <<"/.*~process@1.0/.*">>,
                    <<"strategy">> => <<"Nearest">>,
                    <<"nodes">> => [
                        #{
                            <<"prefix">> => Peer1,
                            <<"wallet">> => Address1
                        },
                        #{
                            <<"prefix">> => Peer2,
                            <<"wallet">> => Address2
                        }    
                    ]
                }
            ]
        }),
    {ok, RelayRes} =
        hb_http:get(
            Node,
            <<
                "/~relay@1.0/call?relay-path=",
                    "/CtOVB2dBtyN_vw3BdzCOrvcQvd9Y1oUGT-zLit8E3qM~process@1.0",
                    "/slot"
            >>,
            #{}
        ),
    HasValidSigner =
        lists:any(
            fun(Peer) -> lists:member(Peer, hb_message:signers(RelayRes)) end,
            Peers
        ),
    ?assert(HasValidSigner).

%%% Statistical test utilities

generate_nodes(N) ->
    [
        #{
            <<"host">> =>
                <<"http://localhost:", (integer_to_binary(Port))/binary>>,
            <<"wallet">> => hb_util:encode(crypto:strong_rand_bytes(32))
        }
    ||
        Port <- lists:seq(1, N)
    ].

generate_hashpaths(Runs) ->
    [
        hb_util:encode(crypto:strong_rand_bytes(32))
    ||
        _ <- lists:seq(1, Runs)
    ].

simulate(Runs, ChooseN, Nodes, Strategy) when is_integer(Runs) ->
    simulate(
        generate_hashpaths(Runs),
        ChooseN,
        Nodes,
        Strategy
    );
simulate(HashPaths, ChooseN, Nodes, Strategy) ->
    [
        choose(ChooseN, Strategy, HashPath, Nodes, #{})
    ||
        HashPath <- HashPaths
    ].

simulation_occurences(SimRes, Nodes) ->
    lists:foldl(
        fun(NearestNodes, Acc) ->
            lists:foldl(
                fun(Node, Acc2) ->
                    Acc2#{ Node => maps:get(Node, Acc2) + 1 }
                end,
                Acc,
                NearestNodes
            )
        end,
        #{ Node => 0 || Node <- Nodes },
        SimRes
    ).

simulation_distribution(SimRes, Nodes) ->
    maps:values(simulation_occurences(SimRes, Nodes)).

within_norms(SimRes, Nodes, TestSize) ->
    Distribution = simulation_distribution(SimRes, Nodes),
    % Check that the mean is `TestSize/length(Nodes)'
    Mean = hb_util:mean(Distribution),
    ?assert(Mean == (TestSize / length(Nodes))),
    % Check that the highest count is not more than 3 standard deviations
    % away from the mean.
    StdDev3 = Mean + 3 * hb_util:stddev(Distribution),
    ?assert(lists:max(Distribution) < StdDev3).