%%% @doc A device that routes outbound messages from the node to their
%%% appropriate network recipients via HTTP. All messages are initially
%%% routed to a single process per node, which then load-balances them
%%% between downstream workers that perform the actual requests.
%%% 
%%% The routes for the router are defined in the `routes' key of the `Opts',
%%% as a precidence-ordered list of maps. The first map that matches the
%%% message will be used to determine the route.
%%% 
%%% Multiple nodes can be specified as viable for a single route, with the
%%% `Choose' key determining how many nodes to choose from the list (defaulting
%%% to 1). The `Strategy' key determines the load distribution strategy,
%%% which can be one of `Random', `By-Base', or `Nearest'. The route may also 
%%% define additional parallel execution parameters, which are used by the
%%% `hb_http' module to manage control of requests.
%%% 
%%% The structure of the routes should be as follows:
%%% <pre>
%%%     Node?: The node to route the message to.
%%%     Nodes?: A list of nodes to route the message to.
%%%     Strategy?: The load distribution strategy to use.
%%%     Choose?: The number of nodes to choose from the list.
%%%     Template?: A message template to match the message against, either as a
%%%                map or a path regex.
%%% </pre>
-module(dev_router).
-export([routes/3, route/2, route/3, preprocess/3]).
-export([match/3, register/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% A exposed register function that allows telling the current node to register
%% a new route with a remote router node. This function should also be itempotent
%% so that it can be called only once.
register(_M1, _M2, Opts) ->
    Registered = hb_opts:get(router_registered, false, Opts),
    % Check if the route is already registered
    case Registered of
        true ->
            {error, <<"Route already registered.">>};
        false ->
            % Validate node history
            case hb_opts:validate_node_history(Opts) of
                {ok, _} ->
                    RouterNode = hb_opts:get(<<"router_peer_location">>, not_found, Opts),
                    Prefix = hb_opts:get(<<"router_prefix">>, not_found, Opts),
                    Price = hb_opts:get(<<"router_price">>, not_found, Opts),
                    Template = hb_opts:get(<<"router_template">>, not_found, Opts),
                    % Check if any required parameters are missing
                    Missing = [
                        {<<"router_peer_location">>, RouterNode},
                        {<<"router_prefix">>, Prefix},
                        {<<"router_price">>, Price},
                        {<<"router_template">>, Template}
                    ],
                    MissingParams = 
                        [Param || {Param, Value} <- Missing, Value =:= not_found],
                    {ok, Attestion} = dev_snp:generate(
                        #{}, 
                        #{}, 
                        #{ 
                            priv_wallet => hb:wallet(), 
                            snp_trusted => hb_opts:get(snp_trusted, [#{}], Opts)
                        }
                    ),
                    ?event(debug_register, {attestion, Attestion}),
                    case MissingParams of
                        [] ->
                            case hb_http:post(RouterNode, #{
                                <<"path">> => <<"/router~node-process@1.0/schedule">>,
                                <<"method">> => <<"POST">>,
                                <<"body">> =>
                                    hb_message:commit(
                                        #{
                                            <<"path">> => <<"register">>,
                                            <<"route">> =>
                                                #{
                                                    <<"prefix">> => Prefix,
                                                    <<"template">> => Template,
                                                    <<"price">> => Price
                                                },
                                            <<"body">> => Attestion
                                        },
                                        Opts
                                    )
                            }, Opts) of
                                {ok, _} ->
                                    hb_http_server:set_opts(
                                        Opts#{ router_registered => true }
                                    ),
                                    {ok, <<"Route registered.">>};
                                {error, _} ->
                                    {error, <<"Failed to register route.">>}
                            end;
                        _ ->
                            % Some parameters are missing, return error message
                            ParamList = lists:foldl(
                                fun(Param, Acc) ->
                                    case Acc of
                                        <<>> -> Param;
                                        _ -> <<Acc/binary, ", ", Param/binary>>
                                    end
                                end,
                                <<>>,
                                MissingParams
                            ),
                            ErrorMsg = <<"Missing required parameters: ", ParamList/binary>>,
                            {error, ErrorMsg}
                    end;
                {error, Reason} ->
                    % Node history validation failed
                    {error, Reason}
            end
    end.

%% @doc Device function that returns all known routes.
routes(M1, M2, Opts) ->
    ?event({routes_msg, M1, M2}),
    Routes = load_routes(Opts),
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

%% @doc Find the appropriate route for the given message. If we are able to 
%% resolve to a single host+path, we return that directly. Otherwise, we return
%% the matching route (including a list of nodes under `nodes') from the list of
%% routes.
%% 
%% If we have a route that has multiple resolving nodes, check
%% the load distribution strategy and choose a node. Supported strategies:
%% <pre>
%%           All: Return all nodes (default).
%%        Random: Distribute load evenly across all nodes, non-deterministically.
%%       By-Base: According to the base message's hashpath.
%%     By-Weight: According to the node's `weight' key.
%%       Nearest: According to the distance of the node's wallet address to the
%%                base message's hashpath.
%% </pre>
%% `By-Base' will ensure that all traffic for the same hashpath is routed to the
%% same node, minimizing work duplication, while `Random' ensures a more even
%% distribution of the requests.
%% 
%% Can operate as a `~router@1.0' device, which will ignore the base message,
%% routing based on the Opts and request message provided, or as a standalone
%% function, taking only the request message and the `Opts' map.
route(Msg, Opts) -> route(undefined, Msg, Opts).
route(_, Msg, Opts) ->
    Routes = load_routes(Opts),
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
                    ?event({choose,
                        {strategy, Strategy},
                        {choose_n, ChooseN},
                        {base, Base},
                        {nodes, Nodes},
                        {chosen, Chosen}
                    }),
                    case Chosen of
                        [Node] when is_map(Node) ->
                            apply_route(Msg, Node);
                        [NodeURI] -> {ok, NodeURI};
                        _ChosenNodes ->
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

%% @doc Find the target path to route for a request message.
find_target_path(Msg, Opts) ->
    case hb_ao:get(<<"route-path">>, Msg, not_found, Opts) of
        not_found ->
            ?event({find_target_path, {msg, Msg}, {opts, Opts}, not_found}),
            hb_ao:get(<<"path">>, Msg, no_path, Opts);
        RoutePath -> RoutePath
    end.

%% @doc Load the current routes for the node. Allows either explicit routes from
%% the node message's `routes' key, or dynamic routes generated by resolving the
%% `route_provider' message.
load_routes(Opts) ->
    case hb_opts:get(route_provider, not_found, Opts) of
        not_found -> hb_opts:get(routes, [], Opts);
        RoutesProvider ->
            ProviderMsgs = hb_singleton:from(RoutesProvider),
            ?event({route_provider, ProviderMsgs}),
            case hb_ao:resolve_many(ProviderMsgs, Opts) of
                {ok, Routes} -> Routes;
                {error, Error} -> throw({routes, routes_provider_failed, Error})
            end
    end.

%% @doc Extract the base message ID from a request message. Produces a single
%% binary ID that can be used for routing decisions.
extract_base(#{ <<"path">> := Path }, Opts) ->
    extract_base(Path, Opts);
extract_base(RawPath, Opts) when is_binary(RawPath) ->
    BasePath = hb_path:hd(#{ <<"path">> => RawPath }, Opts),
    case ?IS_ID(BasePath) of
        true -> BasePath;
        false ->
            case binary:split(BasePath, [<<"\~">>, <<"?">>, <<"&">>], [global]) of
                [BaseMsgID|_] when ?IS_ID(BaseMsgID) -> BaseMsgID;
                _ -> hb_crypto:sha256(BasePath)
            end
    end.

%% @doc Generate a `uri' key for each node in a route.
apply_routes(Msg, R, Opts) ->
    Nodes = hb_ao:get(<<"nodes">>, R, Opts),
    NodesWithRouteApplied =
        lists:map(
            fun(N) ->
                ?event(debug, {apply_route, {msg, Msg}, {node, N}}),
                case apply_route(Msg, N) of
                    {ok, URI} when is_binary(URI) -> N#{ <<"uri">> => URI };
                    {ok, RMsg} -> maps:merge(N, RMsg);
                    {error, _} -> N
                end
            end,
            hb_util:message_to_ordered_list(Nodes)
        ),
    ?event(debug, {nodes_after_apply, NodesWithRouteApplied}),
    R#{ <<"nodes">> => NodesWithRouteApplied }.

%% @doc Apply a node map's rules for transforming the path of the message.
%% Supports the following keys:
%% - `opts': A map of options to pass to the request.
%% - `prefix': The prefix to add to the path.
%% - `suffix': The suffix to add to the path.
%% - `replace': A regex to replace in the path.
apply_route(Msg, Route = #{ <<"opts">> := Opts }) ->
    {ok, #{
        <<"opts">> => Opts,
        <<"uri">> => hb_util:ok(apply_route(Msg, maps:without([<<"opts">>], Route)))
    }};
apply_route(#{ <<"route-path">> := Path }, R) ->
    apply_route(#{ <<"path">> => Path }, R);
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

%% @doc Find the first matching template in a list of known routes. Allows the
%% path to be specified by either the explicit `path' (for internal use by this
%% module), or `route-path' for use by external devices and users.
match(Base, Req, Opts) ->
    ?event(debug_preprocess, {routeReq, Req}),
    ?event(debug_preprocess,
        {routes,
            hb_ao:get(<<"routes">>, {as, <<"message@1.0">>, Base}, [], Opts)}
        ),
    Match =
        match_routes(
            Req#{ <<"path">> => find_target_path(Req, Opts) },
            hb_ao:get(<<"routes">>, {as, <<"message@1.0">>, Base}, [], Opts),
            Opts
        ),
    case Match of
        no_matches -> {error, no_matching_route};
        _ -> {ok, Match}
    end.

match_routes(ToMatch, Routes, Opts) ->
    match_routes(
        ToMatch,
        Routes,
        hb_ao:keys(hb_ao:normalize_keys(Routes)),
        Opts
    ).
match_routes(#{ <<"path">> := Explicit = <<"http://", _/binary>> }, _, _, _) ->
    % If the route is an explicit HTTP URL, we can match it directly.
    #{ <<"node">> => Explicit, <<"reference">> => <<"explicit">> };
match_routes(#{ <<"path">> := Explicit = <<"https://", _/binary>> }, _, _, _) ->
    #{ <<"node">> => Explicit, <<"reference">> => <<"explicit">> };
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
    case template_matches(ToMatch, Template, Opts) of
        true -> XM#{ <<"reference">> => hb_path:to_binary([<<"routes">>, XKey]) };
        false -> match_routes(ToMatch, Routes, Keys, Opts)
    end.

%% @doc Check if a message matches a message template or path regex.
template_matches(ToMatch, Template, _Opts) when is_map(Template) ->
    hb_message:match(Template, ToMatch, primary);
template_matches(ToMatch, Regex, Opts) when is_binary(Regex) ->
    MsgPath = find_target_path(ToMatch, Opts),
    Matches = hb_path:regex_matches(MsgPath, Regex),
	?event(debug_template_matches, {matches, Matches, msg_path, MsgPath, regex, Regex}),
	Matches.

%% @doc Implements the load distribution strategies if given a cluster.
choose(0, _, _, _, _) -> [];
choose(N, <<"Random">>, _, Nodes, _Opts) ->
    Node = lists:nth(rand:uniform(length(Nodes)), Nodes),
    [Node | choose(N - 1, <<"Random">>, nop, lists:delete(Node, Nodes), _Opts)];
choose(N, <<"By-Weight">>, _, Nodes, Opts) ->
    ?event(debug, {nodes, Nodes}),
    NodesWithWeight =
        [
            { Node, hb_util:float(hb_ao:get(<<"weight">>, Node, Opts)) }
        ||
            Node <- Nodes
        ],
    Node = hb_util:weighted_random(NodesWithWeight),
    [
        Node
    |
        choose(N - 1, <<"By-Weight">>, nop, lists:delete(Node, Nodes), Opts)
    ];
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

%% @doc Calculate the minimum distance between two numbers
%% (either progressing backwards or forwards), assuming a
%% 256-bit field.
field_distance(A, B) when is_binary(A) ->
    field_distance(binary_to_bignum(A), B);
field_distance(A, B) when is_binary(B) ->
    field_distance(A, binary_to_bignum(B));
field_distance(A, B) ->
    AbsDiff = abs(A - B),
    min(AbsDiff, (1 bsl 256) - AbsDiff).

%% @doc Find the node with the lowest distance to the given hashpath.
lowest_distance(Nodes) -> lowest_distance(Nodes, {undefined, infinity}).
lowest_distance([], X) -> X;
lowest_distance([{Node, Distance}|Nodes], {CurrentNode, CurrentDistance}) ->
    case Distance of
        infinity -> lowest_distance(Nodes, {Node, Distance});
        _ when Distance < CurrentDistance ->
            lowest_distance(Nodes, {Node, Distance});
        _ -> lowest_distance(Nodes, {CurrentNode, CurrentDistance})
    end.

%% @doc Cast a human-readable or native-encoded ID to a big integer.
binary_to_bignum(Bin) when ?IS_ID(Bin) ->
    << Num:256/unsigned-integer >> = hb_util:native_id(Bin),
    Num.

%% @doc Preprocess a request to check if it should be relayed to a different node.
preprocess(_Msg1, Msg2, Opts) ->
    Req = hb_ao:get(<<"request">>, Msg2, Opts),
    ?event(debug_preprocess, {called_preprocess,Req}),
    TemplateRoutes = load_routes(Opts),
    ?event(debug_preprocess, {template_routes, TemplateRoutes}),
    {_, Match} = match(#{ <<"routes">> => TemplateRoutes }, Req, Opts),
    ?event(debug_preprocess, {match, Match}),
    case Match of
        no_matching_route -> 
            ?event(debug_preprocess, preprocessor_did_not_match),
            case hb_opts:get(router_preprocess_default, <<"local">>, Opts) of
                <<"local">> ->
                    ?event(debug_preprocess, executing_locally),
                    {ok, #{
                        <<"body">> =>
                            hb_ao:get(<<"body">>, Msg2, Opts#{ hashpath => ignore })
                    }};
                <<"error">> ->
                    ?event(debug_preprocess, preprocessor_returning_error),
                    {ok, #{
                        <<"body">> =>
                            [#{
                                <<"status">> => 404,
                                <<"message">> =>
                                    <<"No matching template found in the given routes.">>
                            }]
                    }}
            end;
        _ -> 
            ?event(debug_preprocess, {matched_route, Match}),
            {ok,
                #{
                    <<"body">> =>
                        [
                            #{ <<"device">> => <<"relay@1.0">> },
                            #{
                                <<"path">> => <<"call">>,
                                <<"target">> => <<"body">>,
                                <<"body">> =>
                                    hb_ao:get(
                                        <<"request">>,
                                        Msg2,
                                        Opts#{ hashpath => ignore }
                                    )
                            }
                        ]
                }
            }
    end.

%%% Tests

route_provider_test() ->
    Node = hb_http_server:start_node(#{
        route_provider => #{
            <<"path">> => <<"/test-key/routes">>,
            <<"test-key">> => #{
                <<"routes">> => [
                    #{
                        <<"template">> => <<"*">>,
                        <<"node">> => <<"testnode">>
                    }
                ]
            }
        }
    }),
    ?assertEqual(
        {ok, <<"testnode">>},
        hb_http:get(Node, <<"/~router@1.0/routes/1/node">>, #{})
    ).

dynamic_route_provider_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Node = hb_http_server:start_node(#{
        route_provider => #{
            <<"device">> => <<"lua@5.3a">>,
            <<"path">> => <<"route_provider">>,
            <<"module">> => #{
                <<"content-type">> => <<"application/lua">>,
                <<"body">> => Script
            },
            <<"node">> => <<"test-dynamic-node">>
        },
        priv_wallet => ar_wallet:new()
    }),
    ?assertEqual(
        {ok, <<"test-dynamic-node">>},
        hb_http:get(Node, <<"/~router@1.0/routes/1/node">>, #{})
    ).

local_process_route_provider_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new(),
        route_provider => #{
            <<"path">> => <<"/router~node-process@1.0/now/known-routes">>
        },
        node_processes => #{
            <<"router">> => #{
                <<"device">> => <<"process@1.0">>,
                <<"execution-device">> => <<"lua@5.3a">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"module">> => #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"body">> => Script
                },
                <<"node">> => <<"router-node">>,
                <<"function">> => <<"compute_routes">>
            }
        }
    }),
    ?assertEqual(
        {ok, <<"test1">>},
        hb_http:get(Node, <<"/~router@1.0/routes/1/template">>, #{})
    ),
    % Query the route 10 times with the same path. This should yield 2 different
    % results, as the route provider should choose 1 node of a set of 2 at random.
    Responses =
        lists:map(
            fun(_) ->
                hb_util:ok(
                    hb_http:get(
                        Node,
                        <<"/~router@1.0/route?route-path=test2">>,
                        #{
                            <<"route-path">> => <<"test2">>
                        }
                    )
                )
            end,
            lists:seq(1, 10)
        ),
    ?event({responses, Responses}),
    ?assertEqual(2, sets:size(sets:from_list(Responses))).

%% @doc Example of a Lua module being used as the `route_provider' for a
%% HyperBEAM node. The module utilized in this example dynamically adjusts the
%% likelihood of routing to a given node, depending upon price and performance.
local_dynamic_router_test() ->
    BenchRoutes = 50,
    {ok, Module} = file:read_file(<<"scripts/dynamic-router.lua">>),
    Run = hb_util:bin(rand:uniform(1337)),
    Node = hb_http_server:start_node(Opts = #{
        store => [
            #{
                <<"store-module">> => hb_store_fs,
                <<"prefix">> => <<"cache-TEST/dynrouter-", Run/binary>>
            }
        ],
        priv_wallet => ar_wallet:new(),
        route_provider => #{
            <<"path">> =>
                RouteProvider =
                    <<"/router~node-process@1.0/compute/routes~message@1.0">>
        },
        node_processes => #{
            <<"router">> => #{
                <<"device">> => <<"process@1.0">>,
                <<"execution-device">> => <<"lua@5.3a">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"module">> => #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"module">> => <<"dynamic-router">>,
                    <<"body">> => Module
                },
                % Set module-specific factors for the test
                <<"pricing-weight">> => 9,
                <<"performance-weight">> => 1,
                <<"score-preference">> => 4
            }
        }
    }),
    Store = hb_opts:get(store, no_store, Opts),
    ?event(debug_dynrouter, {store, Store}),
    % Register workers with the dynamic router with varied prices.
    lists:foreach(fun(X) ->
        hb_http:post(
            Node,
            #{
                <<"path">> => <<"/router~node-process@1.0/schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(
                        #{
                            <<"path">> => <<"register">>,
                            <<"route">> =>
                                #{
                                    <<"prefix">> =>
                                        <<
                                            "https://test-node-",
                                                (hb_util:bin(X))/binary,
                                                ".com"
                                        >>,
                                    <<"template">> => <<"/.*~process@1.0/.*">>,
                                    <<"price">> => X * 250
                                }
                        },
                        Opts
                    )
            },
            Opts
        )
    end, lists:seq(1, 5)),
    % Force computation of the current state. This should be done with a 
    % background worker (ex: a `~cron@1.0/every' task).
    hb_http:get(Node, <<"/router~node-process@1.0/now">>, #{}),
    {ok, Routes} = hb_http:get(Node, RouteProvider, Opts),
    ?event(debug_dynrouter, {got_routes, Routes}),
    % Query the route 10 times with the same path. This should yield 2 different
    % results, as the route provider should choose 1 node of a set of 2 at random.
    BeforeExec = os:system_time(millisecond),
    Responses =
        lists:map(
            fun(_) ->
                hb_util:ok(
                    hb_http:get(
                        Node,
                        <<"/~router@1.0/route/uri?route-path=/procID~process@1.0/now">>,
                        #{}
                    )
                )
            end,
            lists:seq(1, BenchRoutes)
        ),
    AfterExec = os:system_time(millisecond),
    hb_util:eunit_print(
        "Calculated ~p routes in ~ps (~.2f routes/s)",
        [
            BenchRoutes,
            (AfterExec - BeforeExec) / 1000,
            BenchRoutes / ((AfterExec - BeforeExec) / 1000)
        ]
    ),
    % Calculate the distribution of the responses.
    UniqueResponses = sets:to_list(sets:from_list(Responses)),
    Dist =
        [
            {
                Resp,
                hb_util:count(Resp, Responses) / length(Responses)
            }
        ||
            Resp <- UniqueResponses
        ],
    ?event(debug_distribution, {distribution_of_responses, Dist}),
    ?assert(length(UniqueResponses) > 1).

%% @doc Example of a Lua module being used as the `route_provider' for a
%% HyperBEAM node. The module utilized in this example dynamically adjusts the
%% likelihood of routing to a given node, depending upon price and performance.
%% also include preprocessing support for routing
dynamic_router_test() ->
    {ok, Module} = file:read_file(<<"scripts/dynamic-router.lua">>),
    Run = hb_util:bin(rand:uniform(1337)),
	ExecWallet = hb:wallet(<<"test/admissible-report-wallet.json">>),
	ProxyWallet = ar_wallet:new(),
    ExecNode =
        hb_http_server:start_node(
        	ExecOpts = #{ priv_wallet => ExecWallet }
        ),
    Node = hb_http_server:start_node(ProxyOpts = #{
        snp_trusted => [
			#{
				<<"vcpus">> => 32,
				<<"vcpu_type">> => 5, 
				<<"vmm_type">> => 1,
				<<"guest_features">> => 1,
				<<"firmware">> =>
                    <<"b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510">>,
				<<"kernel">> =>
                    <<"69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576">>,
				<<"initrd">> =>
                    <<"544045560322dbcd2c454bdc50f35edf0147829ec440e6cb487b4a1503f923c1">>,
				<<"append">> =>
                    <<"95a34faced5e487991f9cc2253a41cbd26b708bf00328f98dddbbf6b3ea2892e">>
			}
		],
        store => [
            #{
                <<"store-module">> => hb_store_fs,
                <<"prefix">> => <<"cache-TEST/dynrouter-", Run/binary>>
            }
        ],
        priv_wallet => ProxyWallet,
        on => 
            #{
                <<"request">> => #{
                    <<"device">> => <<"router@1.0">>,
                    <<"path">> => <<"preprocess">>
                }
            },
        route_provider => #{
            <<"path">> => <<"/router~node-process@1.0/compute/routes~message@1.0">>
        },
        node_processes => #{
            <<"router">> => #{
                <<"type">> => <<"Process">>,
                <<"device">> => <<"process@1.0">>,
                <<"execution-device">> => <<"lua@5.3a">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"module">> => #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"module">> => <<"dynamic-router">>,
                    <<"body">> => Module
                },
                % Set module-specific factors for the test
                <<"pricing-weight">> => 9,
                <<"performance-weight">> => 1,
                <<"score-preference">> => 4,
                <<"is-admissible">> => #{ 
                  <<"device">> => <<"snp@1.0">>,
                  <<"path">> => <<"verify">>
                }
            }
        }
    }),    % mergeRight this takes our defined Opts and merges them into the
    % node opts configs.
    Store = hb_opts:get(store, no_store, ProxyOpts),
    ?event(debug_dynrouter, {store, Store}),
    % Register workers with the dynamic router with varied prices.
    {ok, [Req]} = file:consult(<<"test/admissible-report.eterm">>),
    lists:foreach(fun(X) ->
        {ok, Res} = 
            hb_http:post(
                Node,
                #{
                    <<"path">> => <<"/router~node-process@1.0/schedule">>,
                    <<"method">> => <<"POST">>,
                    <<"body">> =>
                        hb_message:commit(
                            #{
                                <<"path">> => <<"register">>,
                                <<"route">> =>
                                    #{
                                        <<"prefix">> => ExecNode,
                                        <<"template">> => <<"/c">>,
                                        <<"price">> => X * 250
                                    },
                                <<"body">> => hb_message:commit(Req, ExecOpts)
                            },
                            ExecOpts
                        )
                },
                ExecOpts
            ),
        Res
    end, lists:seq(1, 1)),
    % Force computation of the current state. This should be done with a 
    % background worker (ex: a `~cron@1.0/every' task).
    {Status, NodeRoutes} = hb_http:get(Node, <<"/router~node-process@1.0/now">>, #{}),
    ?event(debug_dynrouter, {got_node_routes, NodeRoutes}),
    ?assertEqual(ok, Status),
    ProxyWalletAddr = hb_util:human_id(ar_wallet:to_address(ProxyWallet)),
    ExecNodeAddr = hb_util:human_id(ar_wallet:to_address(ExecWallet)),
    % Ensure that the `~meta@1.0/info/address' response is produced by the
    % proxy wallet.
    ?event(debug_dynrouter,
        {addresses,
            {proxy_wallet_addr, ProxyWalletAddr},
            {exec_node_addr, ExecNodeAddr}
        }
    ),
    ?assertEqual(
        {ok, ProxyWalletAddr},
        hb_http:get(Node, <<"/~meta@1.0/info/address">>, ProxyOpts)
    ),
    % Ensure that computation is done by the exec node.
    {ok, ResMsg} = hb_http:get(Node, <<"/c?c+list=1">>, ExecOpts),
    ?assertEqual([ExecNodeAddr], hb_message:signers(ResMsg)).

%% @doc Demonstrates routing tables being dynamically created and adjusted
%% according to the real-time performance of nodes. This test utilizes the
%% `dynamic-router' script to manage routes and recalculate weights based on the
%% reported performance.
dynamic_routing_by_performance_test_() ->
    {timeout, 30, fun dynamic_routing_by_performance/0}.
dynamic_routing_by_performance() ->
    % Setup test parameters
    TestNodes = 4,
    BenchRoutes = 16,
    TestPath = <<"/worker">>,
    % Start the main node for the test, loading the `dynamic-router' script and
    % the http_monitor to generate performance messages.
    {ok, Script} = file:read_file(<<"scripts/dynamic-router.lua">>),
    Run = hb_util:bin(rand:uniform(1337_000)),
    Node = hb_http_server:start_node(Opts = #{
        relay_http_client => gun,
        store => [
            #{
                <<"store-module">> => hb_store_fs,
                <<"prefix">> => <<"cache-TEST/dynrouter-", Run/binary>>
            }
        ],
        priv_wallet => ar_wallet:new(),
        route_provider => #{
            <<"path">> =>
                <<"/perf-router~node-process@1.0/compute/routes~message@1.0">>
        },
        node_processes => #{
            <<"perf-router">> => #{
                <<"device">> => <<"process@1.0">>,
                <<"execution-device">> => <<"lua@5.3a">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"module">> => #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"name">> => <<"dynamic-router">>,
                    <<"body">> => Script
                },
                % Set module-specific factors for the test
                <<"pricing-weight">> => 1,
                <<"performance-weight">> => 99,
                <<"score-preference">> => 4,
                <<"performance-period">> => 2, % Adjust quickly
                <<"initial-performance">> => 1000
            }
        },
        % Define the request that should be called in order to record performance
        % information into the process. The `body' of the `http_monitor' message
        % is filled with the signed performance report.
        http_monitor => #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"/perf-router~node-process@1.0/schedule">>
        }
    }),
    % Start and add a series of nodes with decreasing performance, via lag 
    % introduced with a hook set to `~test@1.0/delay'.
    _XNodes =
        lists:map(
            fun(X) ->
                % Start the node, applying a delay that increases for each additional
                % node.
                XNode =
                    hb_http_server:start_node(
                        #{
                            on =>
                                #{
                                    <<"request">> => #{
                                        <<"device">> => <<"test-device@1.0">>,
                                        <<"path">> => <<"delay">>,
                                        <<"duration">> => (X - 1) * 100,
                                        <<"return">> => #{
                                            <<"body">> => [
                                                #{ <<"worker">> => X },
                                                <<"worker">>
                                            ]
                                        }
                                    }
                                }
                        }
                    ),
                % Register the node with the router.
                hb_http:post(
                    Node,
                    #{
                        <<"path">> => <<"/perf-router~node-process@1.0/schedule">>,
                        <<"method">> => <<"POST">>,
                        <<"body">> =>
                            hb_message:commit(
                                #{
                                    <<"path">> => <<"register">>,
                                    <<"route">> =>
                                        #{
                                            <<"prefix">> => XNode,
                                            <<"template">> => TestPath,
                                            <<"price">> => 1000 + X
                                        }
                                },
                                Opts
                            )
                    },
                    Opts
                ),
                XNode
            end,
            lists:seq(1, TestNodes)
        ),
    % Force calculation of the process state.
    {ok, ResBefore} =
        hb_http:get(
            Node,
            PerfPath =
                <<"/perf-router~node-process@1.0/now/routes~message@1.0/1/nodes">>,
            Opts
        ),
    ?event(debug_dynrouter, {nodes_before, ResBefore}),
    % Send `BenchRoutes' request messages to the nodes.
    lists:foreach(
        fun(_XNode) ->
            % We send the requests to the main node's `relay@1.0' device, which
            % will then apply the routes and the request to the test node set.
            Res = hb_http:get(
                Node,
                << "/~relay@1.0/call?relay-path=/worker" >>,
                Opts
            ),
            ?event(debug_dynrouter, {recvd, Res})
        end,
        lists:seq(1, BenchRoutes)
    ),
    % Call `recalculate' on the router process and get the resulting weight
    % table.
    hb_http:post(
        Node,
        #{
            <<"path">> => <<"/perf-router~node-process@1.0/schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                hb_message:commit(#{ <<"path">> => <<"recalculate">> }, Opts)
        },
        Opts
    ),
    % Get the new weights
    {ok, After} = hb_http:get(Node, PerfPath, Opts),
    WeightsByWorker =
        maps:from_list(
            lists:map(
                fun(N) ->
                    {
                        N,
                        hb_ao:get(
                            <<(integer_to_binary(N))/binary, "/weight">>,
                            After,
                            Opts
                        )
                    }
                end,
                lists:seq(1, TestNodes)
            )
        ),
    ?event(debug_dynrouter, {worker_weights, {explicit, WeightsByWorker}}),
    ?assert(maps:get(1, WeightsByWorker) > 0.4),
    ?assert(maps:get(TestNodes, WeightsByWorker) < 0.3),
    ok.

weighted_random_strategy_test() ->
    Nodes =
        [
            #{ <<"host">> => <<"1">>, <<"weight">> => 1 },
            #{ <<"host">> => <<"2">>, <<"weight">> => 99 }
        ],
    SimRes = simulate(1000, 1, Nodes, <<"By-Weight">>),
    [One, _] = simulation_distribution(SimRes, Nodes),
    ?assert(One < 25),
    ?assert(One > 4).

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

%% @doc Ensure that `By-Base' always chooses the same node for the same
%% hashpath.
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
    ),
    % Test that `route-path' can also be used to specify the path, via an AO
    % call.
    ?assertMatch(
        {ok, #{ <<"node">> := <<"http://google.com">> }},
        hb_ao:resolve(
            #{ <<"device">> => <<"router@1.0">>, routes => Routes },
            #{
                <<"path">> => <<"match">>,
                <<"route-path">> => <<"http://google.com">>
            },
            #{}
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