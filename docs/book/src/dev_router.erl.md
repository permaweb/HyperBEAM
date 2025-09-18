# dev_router

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_router.erl)

A device that routes outbound messages from the node to their
appropriate network recipients via HTTP. All messages are initially
routed to a single process per node, which then load-balances them
between downstream workers that perform the actual requests.
The routes for the router are defined in the `routes` key of the `Opts`,
as a precidence-ordered list of maps. The first map that matches the
message will be used to determine the route.
Multiple nodes can be specified as viable for a single route, with the
`Choose` key determining how many nodes to choose from the list (defaulting
to 1). The `Strategy` key determines the load distribution strategy,
which can be one of `Random`, `By-Base`, or `Nearest`. The route may also 
define additional parallel execution parameters, which are used by the
`hb_http` module to manage control of requests.
The structure of the routes should be as follows:
<pre>
    Node?: The node to route the message to.
    Nodes?: A list of nodes to route the message to.
    Strategy?: The load distribution strategy to use.
    Choose?: The number of nodes to choose from the list.
    Template?: A message template to match the message against, either as a
               map or a path regex.
</pre>

---

## Exported Functions

- `info/1`
- `info/3`
- `match/3`
- `preprocess/3`
- `register/3`
- `route/2`
- `route/3`
- `routes/3`

---

### info

A device that routes outbound messages from the node to their
Exported function for getting device info, controls which functions are

```erlang
info(_) -> 
    #{ exports => [info, routes, route, match, register, preprocess] }.
```

### info

HTTP info response providing information about this device

```erlang
info(_Msg1, _Msg2, _Opts) ->
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
```

### register

Register function that allows telling the current node to register
Device function that returns all known routes.

```erlang
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
```

### routes

Register function that allows telling the current node to register
Device function that returns all known routes.

```erlang
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
```

### route

Find the appropriate route for the given message. If we are able to 

```erlang
route(Msg, Opts) -> route(undefined, Msg, Opts).
```

### route

Find the appropriate route for the given message. If we are able to 

```erlang
route(_, Msg, Opts) ->
    Routes = load_routes(Opts),
    R = match_routes(Msg, Routes, Opts),
    ?event({find_route, {msg, Msg}, {routes, Routes}, {res, R}}),
    case (R =/= no_matches) andalso hb_ao:get(<<"node">>, R, Opts) of
        false -> {error, no_matches};
        Node when is_binary(Node) -> {ok, Node};
        Node when is_map(Node) -> apply_route(Msg, Node, Opts);
        not_found ->
            ModR = apply_routes(Msg, R, Opts),
            case hb_ao:get(<<"strategy">>, R, Opts) of
                not_found -> {ok, ModR};
                <<"All">> -> {ok, ModR};
                Strategy ->
                    ChooseN = hb_ao:get(<<"choose">>, R, 1, Opts),
                    % Get the first element of the path -- the `base' message
                    % of the request.
```

### load_routes

Load the current routes for the node. Allows either explicit routes from

```erlang
load_routes(Opts) ->
    RouterOpts = hb_opts:get(router_opts, #{}, Opts),
    case hb_maps:get(<<"provider">>, RouterOpts, not_found, Opts) of
        not_found -> hb_opts:get(routes, [], Opts);
        RoutesProvider ->
            ProviderMsgs = hb_singleton:from(RoutesProvider, Opts),
            ?event({<<"provider">>, ProviderMsgs}),
            case hb_ao:resolve_many(ProviderMsgs, Opts) of
                {ok, Routes} -> hb_cache:ensure_all_loaded(Routes, Opts);
                {error, Error} -> throw({routes, routes_provider_failed, Error})
            end
    end.
```

### extract_base

Extract the base message ID from a request message. Produces a single

```erlang
extract_base(#{ <<"path">> := Path }, Opts) ->
    extract_base(Path, Opts);
```

### extract_base

Extract the base message ID from a request message. Produces a single

```erlang
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
```

### apply_routes

Generate a `uri` key for each node in a route.
Apply a node map's rules for transforming the path of the message.

```erlang
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
```

### apply_route

Generate a `uri` key for each node in a route.
Apply a node map's rules for transforming the path of the message.

```erlang
apply_route(Msg, Route, Opts) ->
    % LoadedRoute = hb_cache:ensure_all_loaded(Route, Opts),
    RouteOpts = hb_maps:get(<<"opts">>, Route, #{}),
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
```

### do_apply_route

```erlang
do_apply_route(#{ <<"route-path">> := Path }, R, Opts) ->
    do_apply_route(#{ <<"path">> => Path }, R, Opts);
```

### do_apply_route

```erlang
do_apply_route(#{ <<"path">> := RawPath }, #{ <<"prefix">> := RawPrefix }, Opts) ->
    Path = hb_cache:ensure_loaded(RawPath, Opts),
    Prefix = hb_cache:ensure_loaded(RawPrefix, Opts),
    {ok, <<Prefix/binary, Path/binary>>};
```

### do_apply_route

```erlang
do_apply_route(#{ <<"path">> := RawPath }, #{ <<"suffix">> := RawSuffix }, Opts) ->
    Path = hb_cache:ensure_loaded(RawPath, Opts),
    Suffix = hb_cache:ensure_loaded(RawSuffix, Opts),
    {ok, <<Path/binary, Suffix/binary>>};
```

### do_apply_route

```erlang
do_apply_route(
        #{ <<"path">> := RawPath },
        #{ <<"match">> := RawMatch, <<"with">> := RawWith },
        Opts) ->
    Path = hb_cache:ensure_loaded(RawPath, Opts),
    Match = hb_cache:ensure_loaded(RawMatch, Opts),
    With = hb_cache:ensure_loaded(RawWith, Opts),
    % Apply the regex to the path and replace the first occurrence.
```

### match

Find the first matching template in a list of known routes. Allows the

```erlang
match(Base, Req, Opts) ->
    ?event(debug_preprocess,
        {matching_routes,
            {base, Base},
            {req, Req}
        }
    ),
    TargetPath = hb_util:find_target_path(Req, Opts),
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
```

### match_routes

```erlang
match_routes(ToMatch, Routes, Opts) ->
    match_routes(
        hb_cache:ensure_all_loaded(ToMatch, Opts),
        hb_cache:ensure_all_loaded(Routes, Opts),
        hb_ao:keys(hb_ao:normalize_keys(Routes, Opts)),
        Opts
    ).
```

### match_routes

```erlang
match_routes(#{ <<"path">> := Explicit = <<"http://", _/binary>> }, _, _, _) ->
    % If the route is an explicit HTTP URL, we can match it directly.
```

### match_routes

```erlang
match_routes(#{ <<"path">> := Explicit = <<"https://", _/binary>> }, _, _, _) ->
    #{ <<"node">> => Explicit, <<"reference">> => <<"explicit">> };
```

### match_routes

```erlang
match_routes(_, _, [], _) -> no_matches;
```

### match_routes

```erlang
match_routes(ToMatch, Routes, [XKey|Keys], Opts) ->
    XM = hb_ao:get(XKey, Routes, Opts),
    Template =
        hb_ao:get(
            <<"template">>,
            XM,
            #{},
            Opts#{ hashpath => ignore }
        ),
    case hb_util:template_matches(ToMatch, Template, Opts) of
        true -> XM#{ <<"reference">> => hb_path:to_binary([<<"routes">>, XKey]) };
        false -> match_routes(ToMatch, Routes, Keys, Opts)
    end.
```

### choose

Implements the load distribution strategies if given a cluster.

```erlang
choose(0, _, _, _, _) -> [];
```

### choose

Implements the load distribution strategies if given a cluster.

```erlang
choose(N, <<"Random">>, _, Nodes, _Opts) ->
    Node = lists:nth(rand:uniform(length(Nodes)), Nodes),
    [Node | choose(N - 1, <<"Random">>, nop, lists:delete(Node, Nodes), _Opts)];
```

### choose

Implements the load distribution strategies if given a cluster.

```erlang
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
```

### choose

Implements the load distribution strategies if given a cluster.

```erlang
choose(N, <<"By-Base">>, Hashpath, Nodes, Opts) when is_binary(Hashpath) ->
    choose(N, <<"By-Base">>, binary_to_bignum(Hashpath), Nodes, Opts);
```

### choose

Implements the load distribution strategies if given a cluster.

```erlang
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
```

### choose

Implements the load distribution strategies if given a cluster.

```erlang
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
```

### field_distance

Calculate the minimum distance between two numbers

```erlang
field_distance(A, B) when is_binary(A) ->
    field_distance(binary_to_bignum(A), B);
```

### field_distance

Calculate the minimum distance between two numbers

```erlang
field_distance(A, B) when is_binary(B) ->
    field_distance(A, binary_to_bignum(B));
```

### field_distance

Calculate the minimum distance between two numbers

```erlang
field_distance(A, B) ->
    AbsDiff = abs(A - B),
    min(AbsDiff, (1 bsl 256) - AbsDiff).
```

### lowest_distance

Find the node with the lowest distance to the given hashpath.

```erlang
lowest_distance(Nodes) -> lowest_distance(Nodes, {undefined, infinity}).
```

### lowest_distance

Find the node with the lowest distance to the given hashpath.

```erlang
lowest_distance([], X) -> X;
```

### lowest_distance

Find the node with the lowest distance to the given hashpath.

```erlang
lowest_distance([{Node, Distance}|Nodes], {CurrentNode, CurrentDistance}) ->
    case Distance of
        infinity -> lowest_distance(Nodes, {Node, Distance});
        _ when Distance < CurrentDistance ->
            lowest_distance(Nodes, {Node, Distance});
        _ -> lowest_distance(Nodes, {CurrentNode, CurrentDistance})
    end.
```

### binary_to_bignum

Cast a human-readable or native-encoded ID to a big integer.

```erlang
binary_to_bignum(Bin) when ?IS_ID(Bin) ->
    << Num:256/unsigned-integer >> = hb_util:native_id(Bin),
    Num.
```

### preprocess

Preprocess a request to check if it should be relayed to a different node.

```erlang
preprocess(Msg1, Msg2, Opts) ->
    Req = hb_ao:get(<<"request">>, Msg2, Opts#{ hashpath => ignore }),
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
        {ok, _Method, Node, _Path, _MsgWithoutMeta, _ReqOpts} ->
            ?event(debug_preprocess, {matched_route, {explicit, Res}}),
            CommitRequest =
                hb_util:atom(
                    hb_ao:get_first(
                        [
                            {Msg1, <<"commit-request">>}
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
```

### test_provider_test

```erlang
test_provider_test() ->
    Node =
        hb_http_server:start_node(Opts =
            #{
                router_opts => #{
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
                },
                store => #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-TEST">>
                }
            }
        ),
    ?assertEqual(
        {ok, <<"testnode">>},
        hb_http:get(Node, <<"/~router@1.0/routes/1/node">>, Opts)
    ).
```

### dynamic_provider_test

```erlang
dynamic_provider_test() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Node = hb_http_server:start_node(#{
        router_opts => #{
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
        priv_wallet => ar_wallet:new()
    }),
    ?assertEqual(
        {ok, <<"test-dynamic-node">>},
        hb_http:get(Node, <<"/~router@1.0/routes/1/node">>, #{})
    ).
```

### local_process_provider_test_

```erlang
local_process_provider_test_() ->
    {timeout, 30, fun local_process_provider/0}.
```

### local_process_provider

```erlang
local_process_provider() ->
    {ok, Script} = file:read_file("test/test.lua"),
    Node = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new(),
        router_opts => #{
            <<"provider">> => #{
                <<"path">> => <<"/router~node-process@1.0/now/known-routes">>
            }
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
```

### local_dynamic_router_test_

Example of a Lua module being used as the `<<"provider">>` for a

```erlang
local_dynamic_router_test_() ->
    {timeout, 60, fun local_dynamic_router/0}.
```

### local_dynamic_router

```erlang
local_dynamic_router() ->
    BenchRoutes = 50,
    TestNodes = 5,
    {ok, Module} = file:read_file(<<"scripts/dynamic-router.lua">>),
    Node = hb_http_server:start_node(Opts = #{
        store => hb_test_utils:test_store(),
        priv_wallet => ar_wallet:new(),
        router_opts => #{
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
        node_processes => #{
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
```

### dynamic_router_pricing_test_

Test that verifies dynamic router functionality and template-based pricing.

```erlang
dynamic_router_pricing_test_() ->
    {timeout, 30, fun dynamic_router_pricing/0}.
```

### dynamic_router_pricing

```erlang
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
                priv_wallet => ExecWallet, 
                port => 10009,
                store => hb_test_utils:test_store(),
                node_processes => #{
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
                p4_recipient => ExecNodeAddr, 
                p4_non_chargable_routes => [
                    #{ <<"template">> => <<"/*~node-process@1.0/*">> },
                    #{ <<"template">> => <<"/*~router@1.0/*">> }
                ],
                on => #{
                    <<"request">> => Processor,
                    <<"response">> => Processor
                },
                node_process_spawn_codec => <<"ans104@1.0">>,
                router_opts => #{
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
        port => 10010,
        store => hb_test_utils:test_store(),
        priv_wallet => ProxyWallet,
        on => 
            #{
                <<"request">> => #{
                    <<"device">> => <<"router@1.0">>,
                    <<"path">> => <<"preprocess">>,
                    <<"commit-request">> => true
                }
            },
        router_opts => #{
            <<"provider">> => #{
                <<"path">> =>
                    <<"/router2~node-process@1.0/compute/routes~message@1.0">>
            },
            <<"registrar">> => #{
                <<"path">> => <<"/router2~node-process@1.0">>
            },
            <<"registrar-path">> => <<"schedule">>
        },
        relay_allow_commit_request => true,
        node_processes => #{
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
```

### dynamic_router_test_

Example of a Lua module being used as the `<<"provider">>` for a

```erlang
dynamic_router_test_() ->
    {timeout, 30, fun dynamic_router/0}.
```

### dynamic_router

```erlang
dynamic_router() ->
    {ok, Module} = file:read_file(<<"scripts/dynamic-router.lua">>),
    ExecWallet = hb:wallet(<<"test/admissible-report-wallet.json">>),
    ProxyWallet = ar_wallet:new(),
    ExecNode =
        hb_http_server:start_node(
            ExecOpts = #{ priv_wallet => ExecWallet, store => hb_test_utils:test_store() }
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
        store => hb_test_utils:test_store(),
        priv_wallet => ProxyWallet,
        on => 
            #{
                <<"request">> => #{
                    <<"device">> => <<"router@1.0">>,
                    <<"path">> => <<"preprocess">>
                }
            },
        router_opts => #{
            <<"provider">> => #{
                <<"path">> => <<"/router~node-process@1.0/compute/routes~message@1.0">>
            }
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
```

### dynamic_routing_by_performance_test_

Demonstrates routing tables being dynamically created and adjusted

```erlang
dynamic_routing_by_performance_test_() ->
    {timeout, 60, fun dynamic_routing_by_performance/0}.
```

### dynamic_routing_by_performance

```erlang
dynamic_routing_by_performance() ->
    % Setup test parameters
    TestNodes = 4,
    BenchRoutes = 16,
    TestPath = <<"/worker">>,
    % Start the main node for the test, loading the `dynamic-router' script and
    % the http_monitor to generate performance messages.
```

### weighted_random_strategy_test

```erlang
weighted_random_strategy_test() ->
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
```

### strategy_suite_test_

```erlang
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
```

### by_base_determinism_test

Ensure that `By-Base` always chooses the same node for the same

```erlang
by_base_determinism_test() ->
    FirstN = 5,
    Nodes = generate_nodes(5),
    HashPaths = generate_hashpaths(100),
    Simulation = simulate(HashPaths, FirstN, Nodes, <<"By-Base">>),
    Simulation2 = simulate(HashPaths, FirstN, Nodes, <<"By-Base">>),
    ?assertEqual(Simulation, Simulation2).
```

### unique_test

```erlang
unique_test(Strategy) ->
    TestSize = 1,
    FirstN = 5,
    Nodes = generate_nodes(5),
    Simulation = simulate(TestSize, FirstN, Nodes, Strategy),
    unique_nodes(Simulation).
```

### choose_1_test

```erlang
choose_1_test(Strategy) ->
    TestSize = 1500,
    Nodes = generate_nodes(20),
    Simulation = simulate(TestSize, 1, Nodes, Strategy),
    within_norms(Simulation, Nodes, TestSize).
```

### choose_n_test

```erlang
choose_n_test(Strategy) ->
    TestSize = 1500,
    FirstN = 5,
    Nodes = generate_nodes(20),
    Simulation = simulate(TestSize, FirstN, Nodes, Strategy),
    within_norms(Simulation, Nodes, TestSize * 5),
    unique_nodes(Simulation).
```

### unique_nodes

```erlang
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
```

### route_template_message_matches_test

```erlang
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
```

### route_regex_matches_test

```erlang
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
```

### explicit_route_test

```erlang
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
```

### device_call_from_singleton_test

```erlang
device_call_from_singleton_test() ->
    % Try with a real-world example, taken from a GET request to the router.
```

### get_routes_test

```erlang
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
```

### add_route_test

Test that the `preprocess/3` function re-routes a request to remote

```erlang
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
```

### request_hook_reroute_to_nearest_test

Test that the `preprocess/3` function re-routes a request to remote

```erlang
request_hook_reroute_to_nearest_test() ->
    Peer1 = hb_http_server:start_node(#{ priv_wallet => W1 = ar_wallet:new() }),
    Peer2 = hb_http_server:start_node(#{ priv_wallet => W2 = ar_wallet:new() }),
    Address1 = hb_util:human_id(ar_wallet:to_address(W1)),
    Address2 = hb_util:human_id(ar_wallet:to_address(W2)),
    Peers = [Address1, Address2],
    Node =
        hb_http_server:start_node(Opts = #{
            priv_wallet => ar_wallet:new(),
            routes =>
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
            on => #{ <<"request">> => #{ <<"device">> => <<"relay@1.0">> } }
        }),
    Res =
        lists:map(
            fun(_) ->
                hb_util:ok(
                    hb_http:get(
                        Node,
                        <<"/~meta@1.0/info/address">>,
                        Opts#{ http_only_result => true }
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
```

### generate_nodes

```erlang
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
```

### generate_hashpaths

```erlang
generate_hashpaths(Runs) ->
    [
        hb_util:encode(crypto:strong_rand_bytes(32))
    ||
        _ <- lists:seq(1, Runs)
    ].
```

### simulate

```erlang
simulate(Runs, ChooseN, Nodes, Strategy) when is_integer(Runs) ->
    simulate(
        generate_hashpaths(Runs),
        ChooseN,
        Nodes,
        Strategy
    );
```

### simulate

```erlang
simulate(HashPaths, ChooseN, Nodes, Strategy) ->
    [
        choose(ChooseN, Strategy, HashPath, Nodes, #{})
    ||
        HashPath <- HashPaths
    ].
```

### simulation_occurences

```erlang
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
```

### simulation_distribution

```erlang
simulation_distribution(SimRes, Nodes) ->
    hb_maps:values(simulation_occurences(SimRes, Nodes), #{}).
```

### within_norms

```erlang
within_norms(SimRes, Nodes, TestSize) ->
    Distribution = simulation_distribution(SimRes, Nodes),
    % Check that the mean is `TestSize/length(Nodes)'
    Mean = hb_util:mean(Distribution),
    ?assert(Mean == (TestSize / length(Nodes))),
    % Check that the highest count is not more than 3 standard deviations
    % away from the mean.
```

---

*Generated from [dev_router.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_router.erl)*
