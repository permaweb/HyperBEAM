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
-export([info/1, info/3, routes/3, route/2, route/3, preprocess/3]).
-export([match/3, register/3]).
-export([field_distance/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Exported function for getting device info, controls which functions are
%% exposed via the device API.
info(_) -> 
    #{
        exports =>
            [
                <<"info">>,
                <<"routes">>,
                <<"route">>,
                <<"match">>,
                <<"register">>,
                <<"preprocess">>
            ]
    }.

%% @doc HTTP info response providing information about this device
info(_Base, _Req, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Router device for handling outbound message routing">>,
        <<"version">> => <<"1.0">>,
        <<"api">> => #{
            <<"info">> => #{
                <<"description">> => <<"Get device info">>
            },
            <<"routes">> => #{
                <<"description">> => <<"Get or add routes">>,
                <<"method">> => <<"GET or POST">>
            },
            <<"route">> => #{
                <<"description">> => <<"Find a route for a message">>,
                <<"required_params">> => #{
                    <<"route-path">> => <<"Path to route">>
                }
            },
            <<"match">> => #{
                <<"description">> => <<"Match a message against available routes">>
            },
            <<"register">> => #{
                <<"description">> => <<"Register a route with a remote router node">>,
                <<"node-message">> => #{
                    <<"routes">> => 
                        [
                            #{
                                <<"registration-peer">> => <<"Location of the router peer">>,
                                <<"prefix">> => <<"Prefix for the route">>,
                                <<"price">> => <<"Price for the route">>,
                                <<"template">> => <<"Template to match the route">>
                            }
                        ]
                }
            },
            <<"preprocess">> => #{
                <<"description">> => <<"Preprocess a request to check if it should be relayed">>
            }
        }
    },
    {ok, InfoBody}.

%% @doc Register function that allows telling the current node to register
%% a new route with a remote router node. This function should also be idempotent.
%% so that it can be called only once.
register(_M1, M2, Opts) ->
    %% Extract all required parameters from options
    %% These values will be used to construct the registration message
    RouterOpts = hb_opts:get(router_opts, #{}, Opts),
    RouterRegMsgs =
        case hb_maps:get(<<"offered">>, RouterOpts, #{}, Opts) of
            RegList when is_list(RegList) -> RegList;
            RegMsg when is_map(RegMsg) -> [RegMsg]
        end,
    lists:foreach(
        fun(RegMsg) ->
            RouterNode =
                hb_ao:get(
                    <<"registration-peer">>,
                    RegMsg,
                    not_found,
                    Opts
                ),
            {ok, SigOpts} =
                case hb_ao:get(<<"as">>, M2, not_found, Opts) of
                    not_found -> {ok, Opts};
                    AsID -> hb_opts:as(AsID, Opts)
                end,
            % Post registration request to the router node
            % The message includes our route details and attestation
            % for verification
            {ok, Res} =
                hb_http:post(
                    RouterNode,
                    <<"/~router@1.0/routes">>,
                    hb_message:commit(
                        #{
                            <<"subject">> => <<"self">>,
                            <<"action">> => <<"register">>,
                            <<"route">> => RegMsg
                        },
                        SigOpts
                    ),
                    Opts
                ),
            ?event({registered, {msg, M2}, {res, Res}}),
            {ok, <<"Route registered.">>}
        end,
        RouterRegMsgs
    ),
    {ok, <<"Routes registered.">>}.

%% @doc Device function that returns all known routes.
routes(M1, M2, Opts) ->
    ?event({routes_msg, M1, M2}),
    Routes = load_routes(Opts),
    ?event({routes, Routes}),
    case hb_ao:get(<<"method">>, M2, Opts) of
        <<"POST">> ->
            RouterOpts = hb_opts:get(router_opts, #{}, Opts),
            ?event(debug_route_reg, {router_opts, RouterOpts}),
            case hb_maps:get(<<"registrar">>, RouterOpts, not_found, Opts) of
                not_found ->
                    % There is no registrar; register if and only if the message
                    % is signed by an authorized operator.
                    ?event(debug_route_reg, no_registrar),
                    Owner = hb_opts:get(operator, undefined, Opts),
                    RouteOwners = hb_opts:get(route_owners, [Owner], Opts),
                    Signers = hb_message:signers(M2, Opts),
                    IsTrusted =
                        lists:any(
                            fun(Signer) -> lists:member(Signer, Signers) end,
                            RouteOwners
                        ),
                    case IsTrusted of
                        true ->
                            % Minimize the work performed by AO-Core to make the sort
                            % more efficient.
                            SortOpts = Opts#{ <<"hashpath">> => ignore },
                            NewRoutes =
                                lists:sort(
                                    fun(X, Y) ->
                                        hb_ao:get(<<"priority">>, X, SortOpts)
                                            < hb_ao:get(<<"priority">>, Y, SortOpts)
                                    end,
                                    [M2|Routes]
                                ),
                            ok = hb_http_server:set_opts(Opts#{ <<"routes">> => NewRoutes }),
                            {ok, <<"Route added.">>};
                        false -> {error, not_authorized}
                    end;
                Registrar ->
                    % Parse the registrar message and execute the route 
                    % registration against it.
                    RegistrarPath =
                        hb_maps:get(
                            <<"registrar-path">>,
                            RouterOpts,
                            not_found,
                            Opts
                        ),
                    ?event(debug_route_reg,
                        {registrar_found, {msg, Registrar}, {path, RegistrarPath}}
                    ),
                    RegReq =
                        case RegistrarPath of
                            not_found -> M2;
                            RegPath ->
                                M2#{ <<"path">> => RegPath }
                        end,
                    RegistrarMsgs = hb_singleton:from(Registrar, Opts) ++ [RegReq],
                    ?event(debug_route_reg, {registrar_msgs, RegistrarMsgs}),
                    case hb_ao:resolve_many(RegistrarMsgs, Opts) of
                        {ok, _} ->
                            {ok, <<"Route added.">>};
                        {error, Error} ->
                            {error, Error}
                    end
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
%%    Shuffled-X: A shuffling strategy is a variation of any other strategy in
%%                which the resulting nodes of the `X' strategy are randomly
%%                re-ordered before being returned.
%%        Random: Distribute load evenly across all nodes, non-deterministically.
%%       By-Base: According to the base message's hashpath.
%%     By-Weight: According to the node's `weight' key.
%%       Nearest: According to the distance of the node's wallet address to the
%%                base message's hashpath.
%%         Range: Determine a subset of nodes based on the `min' and `max' keys
%%                of the node, and the `Route-By` key in the request.
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
    MatchedRoute = match_routes(Msg, Routes, Opts),
    ?event({find_route, {msg, Msg}, {routes, Routes}, {res, MatchedRoute}}),
    case MatchedRoute of
        no_matches ->
            {error, no_matches};
        R ->
            case hb_ao:get(<<"node">>, R, Opts) of
                Node when is_binary(Node) ->
                    {ok, Node};
                Node when is_map(Node) ->
                    apply_route(Msg, Node, Opts);
                not_found ->
                    case hb_ao:get(<<"nodes">>, R, not_found, Opts) of
                        not_found ->
                            {error, no_matches};
                        _ ->
                            RouteWithAppliedNodes = apply_routes(Msg, R, Opts),
                            Strategy =
                                normalize_strategy(
                                    hb_ao:get(
                                        <<"strategy">>,
                                        RouteWithAppliedNodes,
                                        <<"All">>,
                                        Opts
                                    )
                                ),
                            case Strategy of
                                <<"All">> ->
                                    {ok, RouteWithAppliedNodes};
                                _ ->
                                    Nodes =
                                        hb_ao:get(
                                            <<"nodes">>,
                                            RouteWithAppliedNodes,
                                            [],
                                            Opts
                                        ),
                                    ChooseN =
                                        choose_count(
                                            hb_ao:get(
                                                <<"choose">>,
                                                RouteWithAppliedNodes,
                                                1,
                                                Opts
                                            ),
                                            Nodes
                                        ),
                                    Chosen = choose(ChooseN, Strategy, Msg, Nodes, Opts),
                                    ?event({choose,
                                        {strategy, Strategy},
                                        {choose_n, ChooseN},
                                        {nodes, Nodes},
                                        {msg, Msg},
                                        {chosen, Chosen}
                                    }),
                                    case Chosen of
                                        [] ->
                                            {error, no_matches};
                                        [Node] when is_map(Node) ->
                                            {ok, Node};
                                        [NodeURI] when is_binary(NodeURI) ->
                                            {ok, NodeURI};
                                        _ ->
                                            {
                                                ok,
                                                RouteWithAppliedNodes#{
                                                    <<"nodes">> => Chosen
                                                }
                                            }
                                    end
                            end
                    end
            end
    end.

%% @doc Load the current routes for the node. Allows either explicit routes from
%% the node message's `routes' key, or dynamic routes generated by resolving the
%% `<<"provider">>' message.
load_routes(Opts) ->
    RouterOpts = hb_opts:get(router_opts, #{}, Opts),
    case maps:find(<<"provider">>, RouterOpts) of
        error -> hb_opts:get(routes, [], Opts);
        {ok, RoutesProvider} ->
            ?event({<<"provider">>, RoutesProvider}),
            case provider_routes(RoutesProvider, Opts) of
                {ok, #{ <<"routes">> := Routes }} ->
                    hb_cache:ensure_all_loaded(Routes, Opts);
                {ok, Routes} ->
                    hb_cache:ensure_all_loaded(Routes, Opts);
                {error, Error} -> throw({routes, routes_provider_failed, Error})
            end
    end.

provider_routes(RoutesProvider, Opts) when is_list(RoutesProvider) ->
    hb_ao:resolve_many(RoutesProvider, Opts);
provider_routes(RoutesProvider, Opts) ->
    hb_ao:resolve(RoutesProvider, Opts).

%% @doc Generate a `uri' key for each node in a route.
apply_routes(Msg, R, Opts) ->
    Nodes = hb_ao:get(<<"nodes">>, R, Opts),
    NodesWithRouteApplied =
        lists:map(
            fun(N) ->
                ?event({apply_route, {msg, Msg}, {node, N}}),
                case apply_route(Msg, N, Opts) of
                    {ok, URI} when is_binary(URI) -> N#{ <<"uri">> => URI };
                    {ok, RMsg} -> hb_maps:merge(N, RMsg);
                    {error, _} -> N
                end
            end,
            hb_util:message_to_ordered_list(Nodes, Opts)
        ),
    ?event({nodes_after_apply, NodesWithRouteApplied}),
    R#{ <<"nodes">> => NodesWithRouteApplied }.

%% @doc Apply a node map's rules for transforming the path of the message.
%% Supports the following keys:
%% - `opts': A map of options to pass to the request.
%% - `prefix': The prefix to add to the path.
%% - `suffix': The suffix to add to the path.
%% - `match' and `with': A regex to replace in the path.
apply_route(Msg, Route, Opts) ->
    % LoadedRoute = hb_cache:ensure_all_loaded(Route, Opts),
    RouteOpts = hb_opts:mimic_default_types(
        hb_maps:get(<<"opts">>, Route, #{}), existing, Opts),
    {ok, #{
        <<"opts">> => RouteOpts,
        <<"uri">> =>
            hb_util:ok(
                do_apply_route(
                    Msg,
                    hb_maps:without([<<"opts">>], Route, Opts),
                    Opts
                )
            )
    }}.
do_apply_route(#{ <<"route-path">> := Path }, R, Opts) ->
    do_apply_route(#{ <<"path">> => Path }, R, Opts);
do_apply_route(_, #{ <<"uri">> := URI }, _Opts) ->
    {ok, URI};
do_apply_route(#{ <<"path">> := RawPath }, #{ <<"prefix">> := RawPrefix }, Opts) ->
    Path = hb_cache:ensure_loaded(RawPath, Opts),
    Prefix = hb_cache:ensure_loaded(RawPrefix, Opts),
    {ok, <<Prefix/binary, Path/binary>>};
do_apply_route(#{ <<"path">> := RawPath }, #{ <<"suffix">> := RawSuffix }, Opts) ->
    Path = hb_cache:ensure_loaded(RawPath, Opts),
    Suffix = hb_cache:ensure_loaded(RawSuffix, Opts),
    {ok, <<Path/binary, Suffix/binary>>};
do_apply_route(
        #{ <<"path">> := RawPath },
        #{ <<"match">> := RawMatch, <<"with">> := RawWith },
        Opts) ->
    Path = hb_cache:ensure_loaded(RawPath, Opts),
    Match = hb_cache:ensure_loaded(RawMatch, Opts),
    With = hb_cache:ensure_loaded(RawWith, Opts),
    % Apply the regex to the path and replace the first occurrence.
    case re:replace(Path, Match, With, [global, {return, binary}]) of
        NewPath when is_binary(NewPath) ->
            {ok, NewPath};
        _ ->
            {error, invalid_replace_args}
    end.

%% @doc Find the first matching template in a list of known routes. Allows the
%% path to be specified by either the explicit `path' (for internal use by this
%% module), or `route-path' for use by external devices and users.
match(Base, Req, Opts) ->
    ?event(debug_preprocess,
        {matching_routes,
            {base, Base},
            {req, Req}
        }
    ),
    TargetPath =
        case hb_util:find_target_path(Req, Opts) of
            no_path -> no_path;
            {_TargetKey, Path} -> Path
        end,
    Match =
        match_routes(
            Req#{ <<"path">> => TargetPath },
            hb_ao:get(<<"routes">>, {as, <<"message@1.0">>, Base}, [], Opts),
            Opts
        ),
    case Match of
        no_matches -> {error, no_matching_route};
        _ -> {ok, Match}
    end.

match_routes(ToMatch, Routes, Opts) ->
    Keys =
        case hb_util:is_ordered_list(Routes, Opts) of
            true ->
                lists:seq(1, length(hb_util:message_to_ordered_list(Routes, Opts)));
            false ->
                hb_ao:keys(hb_ao:normalize_keys(Routes, Opts))
        end,
    match_routes(
        hb_cache:ensure_all_loaded(ToMatch, Opts),
        hb_cache:ensure_all_loaded(Routes, Opts),
        Keys,
        Opts
    ).
match_routes(Req = #{ <<"route-path">> := Path }, Routes, Keys, Opts) ->
    match_routes(
        (maps:without([<<"route-path">>], Req))#{ <<"path">> => Path },
        Routes,
        Keys,
        Opts
    );
match_routes(#{ <<"path">> := Explicit = <<"http://", _/binary>> }, _, _, _) ->
    % If the route is an explicit HTTP URL, we can match it directly.
    #{ <<"node">> => Explicit, <<"reference">> => <<"explicit">> };
match_routes(#{ <<"path">> := Explicit = <<"https://", _/binary>> }, _, _, _) ->
    #{ <<"node">> => Explicit, <<"reference">> => <<"explicit">> };
match_routes(_, _, [], _) -> no_matches;
match_routes(ToMatch, Routes, [XKey|Keys], Opts) ->
    NormRoutes = hb_ao:normalize_keys(Routes, Opts),
    XM = hb_maps:get(hb_ao:normalize_key(XKey), NormRoutes, not_found, Opts),
    Template =
        hb_maps:get(
            <<"template">>,
            XM,
            #{},
            Opts#{ <<"hashpath">> => ignore }
        ),
    case hb_util:template_matches(ToMatch, Template, Opts) of
        true -> XM#{ <<"reference">> => hb_path:to_binary([<<"routes">>, XKey]) };
        false -> match_routes(ToMatch, Routes, Keys, Opts)
    end.

%% @doc Implements the load distribution strategies if given a cluster.
choose(0, _, _, _, _) -> [];
choose(_, _, _, [], _) -> [];
choose(N, <<"Shuffled-", NextStrategy/binary>>, Msg, Nodes, Opts) ->
    % A shuffling strategy is a variation of any other strategy in which the
    % resulting nodes of the `NextStrategy' are randomly re-ordered before being
    % returned.
    choose(N, <<"Random">>, Msg, choose(N, NextStrategy, Msg, Nodes, Opts), Opts);
choose(N, <<"Random">>, _, Nodes, _Opts) ->
    Node = lists:nth(rand:uniform(length(Nodes)), Nodes),
    [Node | choose(N - 1, <<"Random">>, nop, lists:delete(Node, Nodes), _Opts)];
choose(N, <<"By-Weight">>, _, Nodes, Opts) ->
    ?event({nodes, Nodes}),
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
choose(N, <<"By-Base">>, #{ <<"path">> := Path }, Nodes, Opts) when is_binary(Path) ->
    choose(N, <<"By-Base">>, route_hash_int(Path, Opts), Nodes, Opts);
choose(N, <<"By-Base">>, #{ <<"route-by">> := RouteBy }, Nodes, Opts) ->
    choose(N, <<"By-Base">>, route_hash_int(RouteBy, Opts), Nodes, Opts);
choose(N, <<"By-Base">>, Hashpath, Nodes, Opts) when is_binary(Hashpath) ->
    choose(N, <<"By-Base">>, route_hash_int(Hashpath, Opts), Nodes, Opts);
choose(N, <<"By-Base">>, HashInt, Nodes, Opts) when is_integer(HashInt) ->
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
choose(N, <<"Nearest-Integer">>, #{ <<"route-by">> := Int }, Nodes, Opts) ->
    RouteInt = route_integer(Int, Opts),
    NodesWithDistances =
        lists:map(
            fun(Node) ->
                %% Use 4-arity get with explicit default — the old
                %% 3-arity call returned the Opts map when `center'
                %% was missing, crashing field_distance with badarith.
                %% Centerless nodes get 2^256 (> max distance) so they
                %% are selected last.
                case hb_maps:get(<<"center">>, Node, not_found, Opts) of
                    not_found ->
                        {Node, 1 bsl 256};
                    Center ->
                        {Node, field_distance(RouteInt, Center)}
                end
            end,
            Nodes
        ),
    lists:reverse(
        element(
            1,
            lists:foldl(
                fun(_, {Current, Remaining}) ->
                    Res = {Lowest, _} = lowest_distance(Remaining),
                    {[Lowest|Current], lists:delete(Res, Remaining)}
                end,
                {[], NodesWithDistances},
                lists:seq(1, N)
            )
        )
    );
choose(N, <<"Nearest-Integer">>, #{ <<"path">> := Path }, Nodes, Opts)
        when is_binary(Path) ->
    choose(
        N,
        <<"Nearest-Integer">>,
        #{ <<"route-by">> => route_hash_int(Path, Opts) },
        Nodes,
        Opts
    );
choose(N, <<"Nearest-Integer">>, RouteBy, Nodes, Opts) ->
    choose(N, <<"Nearest-Integer">>, #{ <<"route-by">> => RouteBy }, Nodes, Opts);
choose(N, <<"Range">>, #{ <<"route-by">> := RouteBy }, Nodes, Opts) ->
    FilteredNodes =
        lists:filter(
            fun(Node) ->
                Min = hb_maps:get(<<"min">>, Node, undefined, Opts),
                Max = hb_maps:get(<<"max">>, Node, infinity, Opts),
                (Min == undefined orelse RouteBy >= hb_util:int(Min)) andalso
                    (Max == infinity orelse RouteBy =< hb_util:int(Max))
            end,
            Nodes
        ),
    lists:sublist(FilteredNodes, min(length(FilteredNodes), N));
choose(N, <<"Nearest">>, #{ <<"path">> := HashPath }, Nodes, Opts)
        when is_binary(HashPath) ->
    choose(N, <<"Nearest">>, normalize_hashpath(HashPath), Nodes, Opts);
choose(N, <<"Nearest">>, HashPath, Nodes, Opts) when is_binary(HashPath) ->
    BareHashPath = hb_util:native_id(HashPath),
    NodesWithDistances =
        lists:map(
            fun(Node) ->
                Wallet = 
                    case hb_maps:get(<<"wallet">>, Node, not_found, Opts) of
                        W when is_binary(W) -> W;
                        not_found -> throw({error, wallet_not_found});
                        _ -> throw({error, invalid_wallet})
                    end,
                Salt =
                    case hb_maps:find(<<"salt">>, Node, Opts) of
                        {ok, S} -> <<":", S/binary>>;
                        error -> <<>>
                    end,
                DistanceScore =
                    field_distance(
                        hb_crypto:sha256(
                            <<
                                HashPath/binary,
                                ":",
                                Wallet/binary,
                                Salt/binary
                            >>
                        ),
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

choose_count(RawChoose, Nodes) ->
    NormalizedChoose =
        case safe_to_integer(RawChoose) of
            {ok, X} when X > 0 -> X;
            _ -> 0
        end,
    min(NormalizedChoose, length(Nodes)).

normalize_strategy(RawStrategy) ->
    Lower = hb_util:to_lower(hb_util:bin(RawStrategy)),
    case Lower of
        <<"shuffled-", Rest/binary>> ->
            <<"Shuffled-", (normalize_strategy_base(Rest))/binary>>;
        <<"shuffled_", Rest/binary>> ->
            <<"Shuffled-", (normalize_strategy_base(Rest))/binary>>;
        _ ->
            normalize_strategy_base(Lower)
    end.

normalize_strategy_base(<<"all">>) -> <<"All">>;
normalize_strategy_base(<<"random">>) -> <<"Random">>;
normalize_strategy_base(<<"by-base">>) -> <<"By-Base">>;
normalize_strategy_base(<<"by_base">>) -> <<"By-Base">>;
normalize_strategy_base(<<"by-weight">>) -> <<"By-Weight">>;
normalize_strategy_base(<<"by_weight">>) -> <<"By-Weight">>;
normalize_strategy_base(<<"nearest">>) -> <<"Nearest">>;
normalize_strategy_base(<<"nearest-integer">>) -> <<"Nearest-Integer">>;
normalize_strategy_base(<<"nearest_integer">>) -> <<"Nearest-Integer">>;
normalize_strategy_base(<<"range">>) -> <<"Range">>;
normalize_strategy_base(_) -> <<"All">>.

route_integer(Int, _Opts) when is_integer(Int) ->
    Int;
route_integer(Bin, Opts) when is_binary(Bin) ->
    case safe_to_integer(Bin) of
        {ok, Int} -> Int;
        error -> route_hash_int(Bin, Opts)
    end;
route_integer(Value, Opts) ->
    route_hash_int(Value, Opts).

route_hash_int(Int, _Opts) when is_integer(Int) ->
    Int;
route_hash_int(Bin, _Opts) when is_binary(Bin), ?IS_ID(Bin) ->
    binary_to_bignum(Bin);
route_hash_int(Bin, _Opts) when is_binary(Bin), byte_size(Bin) == 32 ->
    <<Int:256/unsigned-integer>> = Bin,
    Int;
route_hash_int(Bin, Opts) when is_binary(Bin) ->
    route_hash_int(hb_crypto:sha256(Bin), Opts);
route_hash_int(#{ <<"path">> := Path }, Opts) when is_binary(Path) ->
    route_hash_int(Path, Opts);
route_hash_int(Value, Opts) ->
    route_hash_int(hb_util:bin(Value), Opts).

normalize_hashpath(Bin) when is_binary(Bin), ?IS_ID(Bin) ->
    Bin;
normalize_hashpath(Bin) when is_binary(Bin), byte_size(Bin) == 32 ->
    Bin;
normalize_hashpath(Bin) when is_binary(Bin) ->
    hb_crypto:sha256(Bin).

safe_to_integer(Value) when is_integer(Value) ->
    {ok, Value};
safe_to_integer(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Int -> {ok, Int}
    catch
        _:_ -> error
    end;
safe_to_integer(Value) when is_list(Value) ->
    try list_to_integer(Value) of
        Int -> {ok, Int}
    catch
        _:_ -> error
    end;
safe_to_integer(_) ->
    error.

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
preprocess(Base, RawReq, Opts) ->
    Req = hb_ao:get(<<"request">>, RawReq, Opts#{ <<"hashpath">> => ignore }),
    ?event(debug_preprocess, {called_preprocess,Req}),
    TemplateRoutes = load_routes(Opts),
    ?event(debug_preprocess, {template_routes, TemplateRoutes}),
    Res = hb_http:message_to_request(Req, Opts),
    ?event(debug_preprocess, {match, Res}),
    case Res of
        {error, _} -> 
            ?event(debug_preprocess, preprocessor_did_not_match),
            case hb_opts:get(router_preprocess_default, <<"local">>, Opts) of
                <<"local">> ->
                    ?event(debug_preprocess, executing_locally),
                    {ok, #{
                        <<"body">> =>
                            hb_ao:get(
                                <<"body">>,
                                RawReq,
                                Opts#{ <<"hashpath">> => ignore }
                            )
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
        {ok, _Method, Node, _Path, _MsgWithoutMeta, _ReqOpts} ->
            ?event(debug_preprocess, {matched_route, {explicit, Res}}),
            CommitRequest =
                hb_util:atom(
                    hb_ao:get_first(
                        [
                            {Base, <<"commit-request">>}
                        ],
                        false,
                        Opts
                    )
                ),
            MaybeCommit =
                case CommitRequest of
                    true -> #{ <<"commit-request">> => true };
                    false -> #{}
                end,
            % Construct a request to `relay@1.0/call' which will proxy a request
            % to `apply@1.0/body' with the original request body as the argument.
            % This allows us to potentially sign the request before sending it,
            % letting the recipient node charge/verify us as necessary, without
            % explicitly signing the user's request itself.
            % 
            % We additionally ensure that the request itself has a commitment,
            % such that headers added by the relaying node are not added to the
            % user's request.
            UserReqWithCommit =
                case hb_message:signers(Req, Opts) of
                    [] ->
                        hb_message:commit(
                            Req,
                            Opts,
                            #{
                                <<"commitment-device">> => <<"httpsig@1.0">>,
                                <<"type">> => <<"unsigned">>
                            }
                        );
                    _ ->
                        Req
                end,
            UserPath =
                case hb_maps:get(<<"path">>, Req, not_found, Opts) of
                    P when is_binary(P), byte_size(P) > 0 ->
                        P;
                    not_found ->
                        throw({error, missing_user_path});
                    _ ->
                        throw({error, invalid_user_path})
                end,
            RelayReq =
                #{
                    <<"device">> => <<"apply@1.0">>,
                    <<"path">> => <<"user-path">>,
                    <<"source">> => <<"user-message">>,
                    <<"user-path">> => UserPath,
                    <<"user-message">> => UserReqWithCommit
                },
            ?event(debug_preprocess, {prepared_relay_req, RelayReq}),
            {
                ok,
                #{
                    <<"body">> =>
                        [
                            MaybeCommit#{
                                <<"device">> => <<"relay@1.0">>,
                                <<"relay-device">> => <<"apply@1.0">>,
                                <<"method">> => <<"POST">>,
                                <<"peer">> => Node
                            },
                            #{
                                <<"path">> => <<"call">>,
                                <<"target">> => <<"proxy-message">>,
                                <<"proxy-message">> => RelayReq
                            }
                        ]
                }
            }
    end.

%%% Tests

test_provider_test_parallel() ->
    Node =
        hb_http_server:start_node(Opts =
            #{
                <<"store">> => hb_test_utils:test_store(),
                <<"router-opts">> => #{
                    <<"provider">> => #{
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
                }
            }
        ),
    ?assertEqual(
        {ok, <<"testnode">>},
        hb_http:get(Node, <<"/~router@1.0/routes/1/node">>, Opts)
    ).

dynamic_provider_test_parallel() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Node = hb_http_server:start_node(#{
        <<"store">> => hb_test_utils:test_store(),
        <<"router-opts">> => #{
            <<"provider">> => #{
                <<"device">> => <<"lua@5.3a">>,
                <<"path">> => <<"provider">>,
                <<"module">> => #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"body">> => Script
                },
                <<"node">> => <<"test-dynamic-node">>
            }
        },
        <<"priv-wallet">> => ar_wallet:new()
    }),
    ?assertEqual(
        {ok, <<"test-dynamic-node">>},
        hb_http:get(Node, <<"/~router@1.0/routes/1/node">>, #{})
    ).

local_process_provider_test_parallel_() ->
    {timeout, 30, fun local_process_provider/0}.
local_process_provider() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Node = hb_http_server:start_node(#{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"router-opts">> => #{
            <<"provider">> => #{
                <<"path">> => <<"/router~node-process@1.0/now/known-routes">>
            }
        },
        <<"node-processes">> => #{
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
                        <<"/~router@1.0/route&route-path=test2/uri">>,
                        #{}
                    )
                )
            end,
            lists:seq(1, 10)
        ),
    ?event({responses, Responses}),
    ?assertEqual(2, length(hb_util:unique(Responses))).

%% @doc Example of a Lua module being used as the `<<"provider">>' for a
%% HyperBEAM node. The module utilized in this example dynamically adjusts the
%% likelihood of routing to a given node, depending upon price and performance.
local_dynamic_router_test_parallel_() ->
    {timeout, 60, fun local_dynamic_router/0}.
local_dynamic_router() ->
    BenchRoutes = 50,
    TestNodes = 5,
    {ok, Module} = file:read_file(<<"scripts/dynamic-router.lua">>),
    Node = hb_http_server:start_node(Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new(),
        <<"router-opts">> => #{
            <<"registrar">> => #{
                <<"device">> => <<"router@1.0">>,
                <<"path">> => <<"/router1~node-process@1.0/schedule">>
            },
            <<"provider">> => #{
                <<"path">> =>
                    RouteProvider =
                        <<"/router1~node-process@1.0/compute/routes~message@1.0">>
            }
        },
        <<"node-processes">> => #{
            <<"router1">> => #{
                <<"device">> => <<"process@1.0">>,
                <<"execution-device">> => <<"lua@5.3a">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"module">> => #{
                    <<"content-type">> => <<"application/lua">>,
                    <<"name">> => <<"dynamic-router">>,
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
    lists:foreach(
        fun(X) ->
            hb_http:post(
                Node,
                #{
                    <<"path">> => <<"/router1~node-process@1.0/schedule">>,
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
        end,
        lists:seq(1, TestNodes)
    ),
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
                        Opts
                    )
                )
            end,
            lists:seq(1, BenchRoutes)
        ),
    AfterExec = os:system_time(millisecond),
    hb_format:eunit_print(
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

%% @doc Test that verifies dynamic router functionality and template-based pricing.
%% Sets up a two-node system: an execution node with p4@1.0 processing and a proxy
%% node with router@1.0 for dynamic routing. The test confirms that:
%% - dev_simple_pay correctly uses template matching via <<"router@1.0">> -> routes
%%   to determine pricing for different routes (e.g., "/c" route with price 0)
%% - Dynamic routing works with Lua-based route providers that adjust routing
%%   likelihood based on price and performance factors
%% - Request preprocessing and routing happens correctly between nodes
%% - Non-chargeable routes are properly handled via template patterns
dynamic_router_pricing_test_parallel_() ->
    {timeout, 30, fun dynamic_router_pricing/0}.
dynamic_router_pricing() ->
    {ok, Module} = file:read_file(<<"scripts/dynamic-router.lua">>),
    {ok, ClientScript} = file:read_file("scripts/hyper-token-p4-client.lua"),
    {ok, TokenScript} = file:read_file("scripts/hyper-token.lua"),
    {ok, ProcessScript} = file:read_file("scripts/hyper-token-p4.lua"),
    ExecWallet = hb:wallet(<<"test/admissible-report-wallet.json">>),
    ProxyWallet = ar_wallet:new(),
    ExecNodeAddr = hb_util:human_id(ar_wallet:to_address(ExecWallet)),
    Processor =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"lua@5.3a">>,
            <<"pricing-device">> => <<"simple-pay@1.0">>,
            <<"ledger-path">> => <<"/ledger2~node-process@1.0">>,
            <<"module">> => #{
                <<"content-type">> => <<"text/x-lua">>,
                <<"name">> => <<"scripts/hyper-token-p4-client.lua">>,
                <<"body">> => ClientScript
            }
        },
    ExecNode =
        hb_http_server:start_node(
            ExecOpts = #{
                <<"priv-wallet">> => ExecWallet, 
                <<"port">> => 10009,
                <<"store">> => hb_test_utils:test_store(),
                <<"node-processes">> => #{
                    <<"ledger2">> => #{
                        <<"device">> => <<"process@1.0">>,
                        <<"execution-device">> => <<"lua@5.3a">>,
                        <<"scheduler-device">> => <<"scheduler@1.0">>,
                        <<"authority-match">> => 1,
                        <<"admin">> => ExecNodeAddr,
                        <<"token">> =>
                            <<"iVplXcMZwiu5mn0EZxY-PxAkz_A9KOU0cmRE0rwej3E">>,                 
                        <<"module">> => [
                            #{
                                <<"content-type">> => <<"text/x-lua">>,
                                <<"name">> => <<"scripts/hyper-token.lua">>,
                                <<"body">> => TokenScript
                            },
                            #{
                                <<"content-type">> => <<"text/x-lua">>,
                                <<"name">> => <<"scripts/hyper-token-p4.lua">>,
                                <<"body">> => ProcessScript
                            }
                        ],              
                        <<"authority">> => ExecNodeAddr              
                    }
                },
                <<"p4-recipient">> => ExecNodeAddr, 
                <<"p4-non-chargable-routes">> => [
                    #{ <<"template">> => <<"/*~node-process@1.0/*">> },
                    #{ <<"template">> => <<"/*~router@1.0/*">> }
                ],
                <<"on">> => #{
                    <<"request">> => Processor,
                    <<"response">> => Processor
                },
                <<"node-process-spawn-codec">> => <<"ans104@1.0">>,
                <<"router-opts">> => #{
                    <<"offered">> => [
                        #{
                            <<"registration-peer">> => <<"http://localhost:10010">>,         
                            <<"template">> => <<"/c">>,  
                            <<"prefix">> => <<"http://localhost:10009">>,
                            <<"price">> => 0
                        },
                        #{
                            <<"registration-peer">> => <<"http://localhost:10010">>,         
                            <<"template">> => <<"/b">>,  
                            <<"prefix">> => <<"http://localhost:10009">>,                   
                            <<"price">> => 1
                        }
                    ]
                }
            }
        ),
    RouterNode = hb_http_server:start_node(#{
        <<"port">> => 10010,
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ProxyWallet,
        <<"on">> => 
            #{
                <<"request">> => #{
                    <<"device">> => <<"router@1.0">>,
                    <<"path">> => <<"preprocess">>,
                    <<"commit-request">> => true
                }
            },
        <<"router-opts">> => #{
            <<"provider">> => #{
                <<"path">> =>
                    <<"/router2~node-process@1.0/compute/routes~message@1.0">>
            },
            <<"registrar">> => #{
                <<"path">> => <<"/router2~node-process@1.0">>
            },
            <<"registrar-path">> => <<"schedule">>
        },
        <<"relay-allow-commit-request">> => true,
        <<"node-processes">> => #{
            <<"router2">> => #{
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
                    <<"path">> => <<"default">>,
                    <<"default">> => <<"false">>
                },
                <<"trusted-peer">> => ExecNodeAddr
            }
        }
    }),
    ?event(
        debug_load_routes,
        {node_message, hb_http:get(RouterNode, <<"/~meta@1.0/info">>, #{})}
    ),
    % Register workers with the dynamic router with varied prices.
    {ok, <<"Routes registered.">>} =
        hb_http:post(
            ExecNode,
            <<"/~router@1.0/register">>,
            #{}
        ),
    % Force computation of the current state.
    {Status, _NodeRoutes} =
        hb_http:get(
            RouterNode,
            <<"/router2~node-process@1.0/now/at-slot">>,
            #{}
        ),
    ?assertEqual(ok, Status),
    % Check that path /c is free
    {ok, CRes} = hb_http:get(RouterNode, <<"/c?c+list=1">>, #{}),
    ?event(debug_dynrouter, {res_msg, CRes}),
    ?assertEqual(1, hb_maps:get(<<"1">>, CRes, not_found)),
    % Check that path /b is not free and returns Insufficient funds
    {error, BRes} = hb_http:get(RouterNode, <<"/b?b+list=1">>, #{}),
    ?event(debug_dynrouter, {res_msg, BRes}),
    ?assertEqual(<<"Insufficient funds">>, hb_maps:get(<<"body">>, BRes, not_found)).


%% @doc Example of a Lua module being used as the `<<"provider">>' for a
%% HyperBEAM node. The module utilized in this example dynamically adjusts the
%% likelihood of routing to a given node, depending upon price and performance.
%% also include preprocessing support for routing
dynamic_router_test_parallel_() ->
    {timeout, 30, fun dynamic_router/0}.
dynamic_router() ->
    {ok, Module} = file:read_file(<<"scripts/dynamic-router.lua">>),
    ExecWallet = hb:wallet(<<"test/admissible-report-wallet.json">>),
    ProxyWallet = ar_wallet:new(),
    ExecNode =
        hb_http_server:start_node(
            ExecOpts = #{ <<"priv-wallet">> => ExecWallet, <<"store">> => hb_test_utils:test_store() }
        ),
    Node = hb_http_server:start_node(ProxyOpts = #{
        <<"snp-trusted">> => [
            #{
                <<"vcpus">> => 32,
                <<"vcpu-type">> => 5, 
                <<"vmm-type">> => 1,
                <<"guest-features">> => 1,
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
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ProxyWallet,
        <<"on">> => 
            #{
                <<"request">> => #{
                    <<"device">> => <<"router@1.0">>,
                    <<"path">> => <<"preprocess">>
                }
            },
        <<"router-opts">> => #{
            <<"provider">> => #{
                <<"path">> => <<"/router~node-process@1.0/compute/routes~message@1.0">>
            }
        },
        <<"node-processes">> => #{
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
    {Status, NodeRoutes} = hb_http:get(Node, <<"/router~node-process@1.0/now/at-slot">>, #{}),
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
    ?assertEqual([ExecNodeAddr], hb_message:signers(ResMsg, ExecOpts)).

%% @doc Demonstrates routing tables being dynamically created and adjusted
%% according to the real-time performance of nodes. This test utilizes the
%% `dynamic-router' script to manage routes and recalculate weights based on the
%% reported performance.
dynamic_routing_by_performance_test_parallel_() ->
    {timeout, 60, fun dynamic_routing_by_performance/0}.
dynamic_routing_by_performance() ->
    % Setup test parameters
    TestNodes = 4,
    BenchRoutes = 16,
    TestPath = <<"/worker">>,
    % Start the main node for the test, loading the `dynamic-router' script and
    % the http-monitor to generate performance messages.
    {ok, Script} = file:read_file(<<"scripts/dynamic-router.lua">>),
    Node = hb_http_server:start_node(Opts = #{
        <<"relay-http-client">> => gun,
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new(),
        <<"router-opts">> => #{
            <<"provider">> => #{
                <<"path">> =>
                    <<"/perf-router~node-process@1.0/compute/routes~message@1.0">>
            }
        },
        <<"node-processes">> => #{
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
        % information into the process. The `body' of the `http-monitor' message
        % is filled with the signed performance report.
        <<"http-monitor">> => #{
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
                            <<"store">> => hb_test_utils:test_store(),
                            <<"on">> =>
                                #{
                                    <<"request">> => #{
                                        <<"device">> => <<"test-device@1.0">>,
                                        <<"path">> => <<"delay">>,
                                        <<"duration">> => (X - 1) * 70,
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
    timer:sleep(150),
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
    ?assert(
        maps:get(1, WeightsByWorker) > maps:get(TestNodes, WeightsByWorker)
    ),
    ok.

weighted_random_strategy_test_parallel() ->
    Nodes =
        [
            #{ <<"host">> => <<"1">>, <<"weight">> => 1 },
            #{ <<"host">> => <<"2">>, <<"weight">> => 99 }
        ],
    SimRes = simulate(1000, 1, Nodes, <<"By-Weight">>),
    [HitsOnFirstHost, _] = simulation_distribution(SimRes, Nodes),
    ProportionOfFirstHost = HitsOnFirstHost / 1000,
    ?event(debug_weighted_random, {proportion_of_first_host, ProportionOfFirstHost}),
    ?assert(ProportionOfFirstHost < 0.05),
    ?assert(ProportionOfFirstHost >= 0.0001).

shuffled_strategy_test_parallel() ->
    Opts = #{},
    Nodes =
        [
            #{ <<"id">> => 1, <<"center">> => 100 },
            #{ <<"id">> => 2, <<"center">> => 200 },
            #{ <<"id">> => 3, <<"center">> => 300 },
            #{ <<"id">> => 4, <<"center">> => 400 }
        ],
    % First, test that without shuffling the nodes are in the `Nearest-Integer'.
    ?assertMatch(
        [#{ <<"id">> := 3 }, #{ <<"id">> := 2 }],
        choose(2, <<"Nearest-Integer">>, #{ <<"route-by">> => 251 }, Nodes, Opts)
    ),
    % Next, test that if we re-run the same strategy many times, we get at least
    % some results that break the non-shuffled order. We would always expect 
    % that the first node will be the one with the lowest center value, but
    % instead we get at least one result in 100 that returns the higher-center
    % value.
    ?assert(
        lists:member(
            2,
            [
                maps:get(
                    <<"id">>,
                    hd(choose(
                        2,
                        <<"Shuffled-Nearest-Integer">>,
                        #{ <<"route-by">> => 1 },
                        Nodes,
                        Opts
                    ))
                )
            ||
                _ <- lists:seq(1, 100)
            ]
        )
    ).
        
range_limited_route_filtering_test_parallel() ->
    Opts = #{},
    Nodes = [
        #{ <<"id">> => 0, <<"max">> => 20 },
        #{ <<"id">> => 1, <<"min">> => 0, <<"max">> => 49 },
        #{ <<"id">> => 2, <<"min">> => 48, <<"max">> => 99 },
        #{ <<"id">> => 3, <<"min">> => 48 }
    ],
    AllPresent =
        fun(IDs, SelectedNodes) ->
            SelectedIDs = [ maps:get(<<"id">>, Node) || Node <- SelectedNodes ],
            ?event({selected_ids, SelectedIDs}),
            lists:all(
                fun(ID) -> lists:member(ID, SelectedIDs) end,
                IDs
            )
        end,
    ?assert(
        AllPresent(
            [0, 1],
            choose(2, <<"Range">>, #{ <<"route-by">> => 15 }, Nodes, Opts)
        )
    ),
    ?assert(
        AllPresent(
            [1, 2, 3],
            choose(4, <<"Range">>, #{ <<"route-by">> => 49 }, Nodes, Opts)
        )
    ),
    ?assert(
        AllPresent(
            [3],
            choose(2, <<"Range">>, #{ <<"route-by">> => 9001 }, Nodes, Opts)
        )
    ),
    lists:foreach(
        fun(_) ->
            ?assert(
                AllPresent(
                    [0, 1],
                    choose(
                        2,
                        <<"Shuffled-Range">>,
                        #{ <<"route-by">> => 10 },
                        Nodes,
                        Opts
                    )
                )
            )
        end,
        lists:seq(1, 10)
    ).

strategy_suite_test_parallel_() ->
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
by_base_determinism_test_parallel() ->
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

route_template_message_matches_test_parallel() ->
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
            #{ <<"routes">> => Routes }
        )
    ),
    ?assertEqual(
        {error, no_matches},
        route(
            #{ <<"path">> => <<"/">>, <<"special-key">> => <<"special-value2">> },
            #{ <<"routes">> => Routes }
        )
    ),
    ?assertEqual(
        {ok, <<"fallback">>},
        route(
            #{ <<"path">> => <<"/">> },
            #{ <<"routes">> => Routes ++ [#{ <<"node">> => <<"fallback">> }] }
        )
    ).

route_regex_matches_test_parallel() ->
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
        route(#{ <<"path">> => <<"/abc/schedule">> }, #{ <<"routes">> => Routes })
    ),
    ?assertEqual(
        {ok, <<"correct">>},
        route(#{ <<"path">> => <<"/a/b/c/schedule">> }, #{ <<"routes">> => Routes })
    ),
    ?assertEqual(
        {error, no_matches},
        route(#{ <<"path">> => <<"/a/b/c/bad-key">> }, #{ <<"routes">> => Routes })
    ).

explicit_route_test_parallel() ->
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
            #{ <<"routes">> => Routes }
        )
    ),
    ?assertEqual(
        {ok, <<"http://google.com">>},
        route(
            #{ <<"path">> => <<"http://google.com">> },
            #{ <<"routes">> => Routes }
        )
    ),
    % Test that `route-path' can also be used to specify the path, via an AO
    % call.
    ?assertMatch(
        {ok, #{ <<"node">> := <<"http://google.com">> }},
        hb_ao:resolve(
            #{ <<"device">> => <<"router@1.0">>, <<"routes">> => Routes },
            #{
                <<"path">> => <<"match">>,
                <<"route-path">> => <<"http://google.com">>
            },
            #{}
        )
    ).

device_call_from_singleton_test_parallel() ->
    % Try with a real-world example, taken from a GET request to the router.
    NodeOpts = #{ <<"routes">> => Routes = [#{
        <<"template">> => <<"/some/path">>,
        <<"node">> => <<"old">>,
        <<"priority">> => 10
    }]},
    Msgs = hb_singleton:from(#{ <<"path">> => <<"~router@1.0/routes">> }, NodeOpts),
    ?event({msgs, Msgs}),
    ?assertEqual(
        {ok, Routes},
        hb_ao:resolve_many(Msgs, NodeOpts)
    ).
    

get_routes_test_parallel() ->
    Node = hb_http_server:start_node(
        #{
            <<"force-signed">> => false,
            <<"routes">> => [
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

add_route_test_parallel() ->
    Owner = ar_wallet:new(),
    Node = hb_http_server:start_node(
        #{
            <<"force-signed">> => false,
            <<"routes">> => [
                #{
                    <<"template">> => <<"/some/path">>,
                    <<"node">> => <<"old">>,
                    <<"priority">> => 10
                }
            ],
            <<"operator">> => hb_util:encode(ar_wallet:to_address(Owner))
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
                #{ <<"priv-wallet">> => Owner }
            ),
            #{}
        ),
    ?event({post_res, Res}),
    ?assertMatch({ok, <<"Route added.">>}, Res),
    GetRes = hb_http:get(Node, <<"/~router@1.0/routes/2/node">>, #{}),
    ?event({get_res, GetRes}),
    {ok, Recvd} = GetRes,
    ?assertMatch(<<"new">>, Recvd).

%% @doc Test that the `preprocess/3' function re-routes a request to remote
%% peers via `~relay@1.0', according to the node's routing table.
request_hook_reroute_to_nearest_test_parallel() ->
    Peer1 = hb_http_server:start_node(#{ <<"priv-wallet">> => W1 = ar_wallet:new() }),
    Peer2 = hb_http_server:start_node(#{ <<"priv-wallet">> => W2 = ar_wallet:new() }),
    Address1 = hb_util:human_id(ar_wallet:to_address(W1)),
    Address2 = hb_util:human_id(ar_wallet:to_address(W2)),
    Peers = [Address1, Address2],
    Node =
        hb_http_server:start_node(Opts = #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"routes">> =>
                [
                    #{
                        <<"template">> => <<"/.*/.*/.*">>,
                        <<"strategy">> => <<"Nearest">>,
                        <<"nodes">> =>
                            lists:map(
                                fun({Address, Node}) ->
                                    #{
                                        <<"prefix">> => Node,
                                        <<"wallet">> => Address
                                    }
                                end,
                                [
                                    {Address1, Peer1},
                                    {Address2, Peer2}
                                ]
                            )
                    }
                ],
            <<"on">> => #{ <<"request">> => #{ <<"device">> => <<"relay@1.0">> } }
        }),
    Res =
        lists:map(
            fun(_) ->
                hb_util:ok(
                    hb_http:get(
                        Node,
                        <<"/~meta@1.0/info/address">>,
                        Opts#{ <<"http-only-result">> => true }
                    )
                )
            end,
            lists:seq(1, 3)
        ),
    ?event(debug_test,
        {res, {
            {response, Res},
            {signers, hb_message:signers(Res, Opts)}
        }}
    ),
    HasValidSigner = lists:any(
        fun(Peer) ->
            lists:member(Peer, Res)
        end,
        Peers
    ),
    ?assert(HasValidSigner).

route_nearest_integer_preserves_opts_test_parallel() ->
    Routes =
        [
            #{
                <<"template">> => <<"/chunk">>,
                <<"nodes">> =>
                    [
                        #{
                            <<"center">> => 100,
                            <<"prefix">> => <<"http://node-100">>,
                            <<"opts">> => #{ <<"protocol">> => http2 }
                        },
                        #{
                            <<"center">> => 200,
                            <<"prefix">> => <<"http://node-200">>,
                            <<"opts">> => #{ <<"protocol">> => http2 }
                        },
                        #{
                            <<"center">> => 400,
                            <<"prefix">> => <<"http://node-400">>,
                            <<"opts">> => #{ <<"protocol">> => http2 }
                        }
                    ],
                <<"strategy">> => <<"nearest-integer">>,
                <<"choose">> => 2,
                <<"parallel">> => 2,
                <<"responses">> => 2,
                <<"stop-after">> => false,
                <<"admissible-status">> => 200
            },
            #{
                <<"template">> => <<".*">>,
                <<"node">> => <<"fallback">>
            }
        ],
    {ok, Route} =
        route(
            #{ <<"path">> => <<"/chunk">>, <<"route-by">> => 210 },
            #{ <<"routes">> => Routes }
        ),
    ?assertEqual(2, hb_ao:get(<<"parallel">>, Route, #{})),
    ?assertEqual(2, hb_ao:get(<<"responses">>, Route, #{})),
    ?assertEqual(false, hb_ao:get(<<"stop-after">>, Route, #{})),
    SelectedNodes = hb_ao:get(<<"nodes">>, Route, #{}),
    ?assertEqual(2, length(SelectedNodes)),
    SelectedCenters =
        lists:sort(
            [
                hb_ao:get(<<"center">>, Node, #{})
            ||
                Node <- SelectedNodes
            ]
        ),
    ?assertEqual([100, 200], SelectedCenters),
    SelectedURIs =
        lists:sort(
            [
                hb_ao:get(<<"uri">>, Node, #{})
            ||
                Node <- SelectedNodes
            ]
        ),
    ?assertEqual(
        [<<"http://node-100/chunk">>, <<"http://node-200/chunk">>],
        SelectedURIs
    ).

route_multirequest_parallel_limit_test_parallel_() ->
    {timeout, 30, fun route_multirequest_parallel_limit/0}.
route_multirequest_parallel_limit() ->
    DelayMs = 300,
    WorkerNodes =
        lists:map(
            fun(N) ->
                hb_http_server:start_node(
                    #{
                        <<"store">> => hb_test_utils:test_store(),
                        <<"on">> =>
                            #{
                                <<"request">> =>
                                    #{
                                        <<"device">> => <<"test-device@1.0">>,
                                        <<"path">> => <<"delay">>,
                                        <<"duration">> => DelayMs,
                                        <<"return">> =>
                                            #{
                                                <<"body">> =>
                                                    [
                                                        #{ <<"worker">> => N },
                                                        <<"worker">>
                                                    ]
                                            }
                                    }
                            }
                    }
                )
            end,
            lists:seq(1, 3)
        ),
    Routes =
        [
            #{
                <<"template">> => <<"/worker">>,
                <<"nodes">> =>
                    lists:map(
                        fun(Node) -> #{ <<"prefix">> => Node } end,
                        WorkerNodes
                    ),
                <<"strategy">> => <<"all">>,
                <<"parallel">> => 2,
                <<"responses">> => 3,
                <<"stop-after">> => false
            }
        ],
    Start = os:system_time(millisecond),
    Results =
        hb_http:request(
            #{ <<"method">> => <<"GET">>, <<"path">> => <<"/worker">> },
            #{
                <<"routes">> => Routes,
                <<"http-only-result">> => false
            }
        ),
    Duration = os:system_time(millisecond) - Start,
    ?assertEqual(3, length(Results)),
    WorkerBodies =
        lists:sort(
            [
                hb_ao:get(<<"body">>, Res, #{})
            ||
                {ok, Res} <- Results
            ]
        ),
    ?assertEqual([1, 2, 3], WorkerBodies),
    % With 3 peers of 300ms each: `parallel = 2` should complete in about two
    % waves (~600ms), not one (~300ms) or fully serial (~900ms).
    ?event({duration, Duration}),
    ?assert(Duration >= 600),
    ?assert(Duration < 900).

%% @doc Test that a full production-style route configuration (matching a
%% typical config.json) resolves every request type correctly: single-node
%% prefix routes, multi-node All-strategy routes, Nearest-Integer chunk
%% routes, match/with regex routes, and fallback routes.
full_route_config_test_parallel() ->
    Routes =
        [
            #{
                <<"template">> => <<"^/arweave/chunk">>,
                <<"nodes">> =>
                    [
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"center">> => 3_600_000_000,
                            <<"with">> => <<"https://data-1.arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        },
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"center">> => 8_200_000_000,
                            <<"with">> => <<"https://data-2.arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        },
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"center">> => 12_200_000_000,
                            <<"with">> => <<"https://data-3.arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        },
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"with">> => <<"https://data-4.arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        },
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"center">> => 16_200_000_000,
                            <<"with">> => <<"https://data-5.arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        }
                    ],
                <<"strategy">> => <<"Nearest-Integer">>,
                <<"choose">> => 3,
                <<"parallel">> => 2
            },
            #{
                <<"template">> => <<"^/arweave">>,
                <<"nodes">> =>
                    [
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"with">> => <<"https://arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        }
                    ],
                <<"parallel">> => true,
                <<"stop-after">> => 1,
                <<"admissible-status">> => 200
            },
            #{
                <<"template">> => <<"/raw">>,
                <<"node">> =>
                    #{
                        <<"prefix">> => <<"https://arweave.net">>,
                        <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                    }
            }
        ],
    Opts = #{ <<"routes">> => Routes },

    %% --- Nearest-Integer strategy for /arweave/chunk ---

    %% A chunk request with route-by near center 8_200_000_000 should pick
    %% the 3 closest nodes out of the 5 available.
    {ok, ChunkRoute} =
        route(
            #{
                <<"path">> => <<"/arweave/chunk/8200000100">>,
                <<"route-by">> => 8_200_000_100
            },
            Opts
        ),
    ?event(router_test, {chunk_route, {route_by, 8_200_000_100}, {route, ChunkRoute}}),
    ?assertEqual(2, hb_ao:get(<<"parallel">>, ChunkRoute, #{})),
    ChunkNodes = hb_ao:get(<<"nodes">>, ChunkRoute, #{}),
    ?assertEqual(3, length(ChunkNodes)),
    %% The three nearest centers to 8_200_000_100 should be
    %% 8_200_000_000, 3_600_000_000, and 12_200_000_000.
    ChunkCenters =
        lists:sort(
            [hb_ao:get(<<"center">>, N, #{}) || N <- ChunkNodes]
        ),
    ?event(router_test, {chunk_centers, ChunkCenters}),
    ?assertEqual([3_600_000_000, 8_200_000_000, 12_200_000_000], ChunkCenters),
    %% Each selected node should have a URI with the match/with regex applied:
    %% /arweave/chunk/... -> https://data-N.arweave.net/chunk/...
    ChunkURIs =
        lists:sort(
            [hb_ao:get(<<"uri">>, N, #{}) || N <- ChunkNodes]
        ),
    ?event(router_test, {chunk_uris, ChunkURIs}),
    ?assertEqual(
        [
            <<"https://data-1.arweave.net/chunk/8200000100">>,
            <<"https://data-2.arweave.net/chunk/8200000100">>,
            <<"https://data-3.arweave.net/chunk/8200000100">>
        ],
        ChunkURIs
    ),

    %% A chunk request near the high end should select the 3 closest to
    %% 16_000_000_000: 16_200_000_000, 12_200_000_000, and 8_200_000_000.
    {ok, HighChunkRoute} =
        route(
            #{
                <<"path">> => <<"/arweave/chunk/16000000000">>,
                <<"route-by">> => 16_000_000_000
            },
            Opts
        ),
    ?event(router_test, {high_chunk_route, {route_by, 16_000_000_000}, {route, HighChunkRoute}}),
    HighChunkCenters =
        lists:sort(
            [
                hb_ao:get(<<"center">>, N, #{})
            ||
                N <- hb_ao:get(<<"nodes">>, HighChunkRoute, #{})
            ]
        ),
    ?event(router_test, {high_chunk_centers, HighChunkCenters}),
    ?assertEqual(
        [8_200_000_000, 12_200_000_000, 16_200_000_000],
        HighChunkCenters
    ),

    %% --- Fallback /arweave route (non-chunk) ---

    %% A non-chunk arweave request (e.g. /arweave/tx/...) should fall
    %% through the chunk template and match the general ^/arweave route.
    {ok, ArweaveRoute} =
        route(#{ <<"path">> => <<"/arweave/tx/RTvlIxbvDOpo7kPisnhnfz0BtgOZE4QlScBSRLEkky4">> }, Opts),
    ?event(router_test, {arweave_fallback_route, ArweaveRoute}),
    ?assertEqual(true, hb_ao:get(<<"parallel">>, ArweaveRoute, #{})),
    ?assertEqual(1, hb_ao:get(<<"stop-after">>, ArweaveRoute, #{})),
    ?assertEqual(200, hb_ao:get(<<"admissible-status">>, ArweaveRoute, #{})),
    ArweaveNodes = hb_ao:get(<<"nodes">>, ArweaveRoute, #{}),
    ?assertEqual(1, length(ArweaveNodes)),
    ArweaveURI = hb_ao:get(<<"uri">>, hd(ArweaveNodes), #{}),
    ?event(router_test, {arweave_fallback_uri, ArweaveURI}),
    ?assertEqual(<<"https://arweave.net/tx/RTvlIxbvDOpo7kPisnhnfz0BtgOZE4QlScBSRLEkky4">>, ArweaveURI),

    %% --- Single-node prefix route (/raw) ---

    {ok, RawRoute} =
        route(#{ <<"path">> => <<"/raw/RTvlIxbvDOpo7kPisnhnfz0BtgOZE4QlScBSRLEkky4">> }, Opts),
    ?event(router_test, {raw_route, RawRoute}),
    ?assertEqual(
        <<"https://arweave.net/raw/RTvlIxbvDOpo7kPisnhnfz0BtgOZE4QlScBSRLEkky4">>,
        hb_ao:get(<<"uri">>, RawRoute, #{})
    ),

    %% --- No match ---

    NoMatchResult = route(#{ <<"path">> => <<"/unknown/endpoint">> }, Opts),
    ?event(router_test, {no_match_result, NoMatchResult}),
    ?assertEqual({error, no_matches}, NoMatchResult),

    %% --- HTTP GETs through the routes ---
    %% Fire actual requests using hb_http:request/2, the same way the
    %% route_multirequest_parallel_limit test does it.
    HttpReqOpts = #{ <<"routes">> => Routes, <<"http-only-result">> => false },

    %% Chunk request via Nearest-Integer (parallel=2, choose=3).
    %% With 3 nodes and parallel=2, wave 1 sends to 2 nodes, wave 2 sends
    %% to the remaining 1. We time it to confirm parallelism.
    ChunkStart = os:system_time(millisecond),
    ChunkHttpRes =
        (catch hb_http:request(
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"/arweave/chunk/8200000100">>,
                <<"route-by">> => 8_200_000_100
            },
            HttpReqOpts
        )),
    ChunkDuration = os:system_time(millisecond) - ChunkStart,
    ?event(router_test, {chunk_http_result, ChunkHttpRes}),
    ?event(router_test, {chunk_http_duration_ms, ChunkDuration}),

    %% Now test with ALL 5 data nodes to really exercise parallel=2.
    %% choose=5 means all 5 nodes get hit, but only 2 at a time.
    %% With ~300-500ms per request, we expect ~3 waves (~900-1500ms)
    %% instead of fully serial (~1500-2500ms) or fully parallel (~300-500ms).
    AllChunkRoutes =
        [
            #{
                <<"template">> => <<"^/arweave/chunk">>,
                <<"nodes">> =>
                    [
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"center">> => 3_600_000_000,
                            <<"with">> => <<"https://data-1.arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        },
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"center">> => 8_200_000_000,
                            <<"with">> => <<"https://data-2.arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        },
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"center">> => 12_200_000_000,
                            <<"with">> => <<"https://data-3.arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        },
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"center">> => 14_000_000_000,
                            <<"with">> => <<"https://data-4.arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        },
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"center">> => 16_200_000_000,
                            <<"with">> => <<"https://data-5.arweave.net">>,
                            <<"opts">> => #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                        }
                    ],
                <<"strategy">> => <<"Nearest-Integer">>,
                <<"choose">> => 5,
                <<"parallel">> => 2,
                <<"responses">> => 5,
                <<"stop-after">> => false
            }
        ],
    AllChunkStart = os:system_time(millisecond),
    AllChunkHttpRes =
        (catch hb_http:request(
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"/arweave/chunk/8200000100">>,
                <<"route-by">> => 8_200_000_100
            },
            #{ <<"routes">> => AllChunkRoutes, <<"http-only-result">> => false }
        )),
    AllChunkDuration = os:system_time(millisecond) - AllChunkStart,
    ?event(router_test, {all_chunk_http_result, AllChunkHttpRes}),
    ?event(router_test, {all_chunk_http_duration_ms, AllChunkDuration}),
    ?event(router_test, {all_chunk_responses,
        case is_list(AllChunkHttpRes) of
            true -> length(AllChunkHttpRes);
            false -> not_a_list
        end
    }),

    %% Fallback /arweave route.
    ArweaveHttpRes =
        (catch hb_http:request(
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"/arweave/tx/RTvlIxbvDOpo7kPisnhnfz0BtgOZE4QlScBSRLEkky4">>
            },
            HttpReqOpts
        )),
    ?event(router_test, {arweave_http_result, ArweaveHttpRes}),

    %% /raw prefix route.
    RawHttpRes =
        (catch hb_http:request(
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"/raw/RTvlIxbvDOpo7kPisnhnfz0BtgOZE4QlScBSRLEkky4">>
            },
            HttpReqOpts
        )),
    ?event(router_test, {raw_http_result, RawHttpRes}).

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
                    Acc2#{ Node => hb_maps:get(Node, Acc2, 0, #{}) + 1 }
                end,
                Acc,
                NearestNodes
            )
        end,
        #{ Node => 0 || Node <- Nodes },
        SimRes
    ).

simulation_distribution(SimRes, Nodes) ->
    hb_maps:values(simulation_occurences(SimRes, Nodes), #{}).

within_norms(SimRes, Nodes, TestSize) ->
    Distribution = simulation_distribution(SimRes, Nodes),
    % Check that the mean is `TestSize/length(Nodes)'
    Mean = hb_util:mean(Distribution),
    ?assert(Mean == (TestSize / length(Nodes))),
    % Check that the highest count is not more than 4 standard deviations
    % away from the mean.
    StdDev3 = Mean + 4 * hb_util:stddev(Distribution),
    ?assert(lists:max(Distribution) < StdDev3).
