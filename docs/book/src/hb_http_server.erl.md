# hb_http_server

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_http_server.erl)

A router that attaches a HTTP server to the AO-Core resolver.
Because AO-Core is built to speak in HTTP semantics, this module
only has to marshal the HTTP request into a message, and then
pass it to the AO-Core resolver. 
`hb_http:reply/4` is used to respond to the client, handling the 
process of converting a message back into an HTTP response.
The router uses an `Opts` message as its Cowboy initial state, 
such that changing it on start of the router server allows for
the execution parameters of all downstream requests to be controlled.

---

## Exported Functions

- `allowed_methods/2`
- `get_opts/0`
- `get_opts/1`
- `init/2`
- `set_default_opts/1`
- `set_opts/1`
- `set_opts/2`
- `set_proc_server_id/1`
- `start_node/0`
- `start_node/1`
- `start/0`
- `start/1`

---

### start

A router that attaches a HTTP server to the AO-Core resolver.
Starts the HTTP server. Optionally accepts an `Opts` message, which

```erlang
start() ->
    ?event(http, {start_store, <<"cache-mainnet">>}),
    Loaded =
        case hb_opts:load(Loc = hb_opts:get(hb_config_location, <<"config.flat">>)) of
            {ok, Conf} ->
                ?event(boot, {loaded_config, Loc, Conf}),
                Conf;
            {error, Reason} ->
                ?event(boot, {failed_to_load_config, Loc, Reason}),
                #{}
        end,
    MergedConfig =
        hb_maps:merge(
            hb_opts:default_message_with_env(),
            Loaded
        ),
    %% Apply store defaults before starting store
    StoreOpts = hb_opts:get(store, no_store, MergedConfig),
    StoreDefaults = hb_opts:get(store_defaults, #{}, MergedConfig),
    UpdatedStoreOpts = 
        case StoreOpts of
            no_store -> no_store;
            _ when is_list(StoreOpts) -> hb_store_opts:apply(StoreOpts, StoreDefaults);
            _ -> StoreOpts
        end,
    hb_store:start(UpdatedStoreOpts),
    PrivWallet =
        hb:wallet(
            hb_opts:get(
                priv_key_location,
                <<"hyperbeam-key.json">>,
                Loaded
            )
        ),
    maybe_greeter(MergedConfig, PrivWallet),
    start(
        Loaded#{
            priv_wallet => PrivWallet,
            store => UpdatedStoreOpts,
            port => hb_opts:get(port, 8734, Loaded),
            cache_writers => [hb_util:human_id(ar_wallet:to_address(PrivWallet))]
        }
    ).
```

### start

```erlang
start(Opts) ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
        ranch,
        cowboy,
        gun,
        os_mon
    ]),
    hb:init(),
    BaseOpts = set_default_opts(Opts),
    {ok, Listener, _Port} = new_server(BaseOpts),
    {ok, Listener}.
```

### maybe_greeter

Print the greeter message to the console if we are not running tests.

```erlang
maybe_greeter(MergedConfig, PrivWallet) ->
    case hb_features:test() of
        false ->
            print_greeter(MergedConfig, PrivWallet);
        true ->
            ok
    end.
```

### print_greeter

Print the greeter message to the console. Includes the version, operator

```erlang
print_greeter(Config, PrivWallet) ->
    FormattedConfig = hb_format:term(Config, Config, 2),
    io:format("~n"
        "===========================================================~n"
        "==    ██╗  ██╗██╗   ██╗██████╗ ███████╗██████╗           ==~n"
        "==    ██║  ██║╚██╗ ██╔╝██╔══██╗██╔════╝██╔══██╗          ==~n"
        "==    ███████║ ╚████╔╝ ██████╔╝█████╗  ██████╔╝          ==~n"
        "==    ██╔══██║  ╚██╔╝  ██╔═══╝ ██╔══╝  ██╔══██╗          ==~n"
        "==    ██║  ██║   ██║   ██║     ███████╗██║  ██║          ==~n"
        "==    ╚═╝  ╚═╝   ╚═╝   ╚═╝     ╚══════╝╚═╝  ╚═╝          ==~n"
        "==                                                       ==~n"
        "==        ██████╗ ███████╗ █████╗ ███╗   ███╗ VERSION:   ==~n"
        "==        ██╔══██╗██╔════╝██╔══██╗████╗ ████║      v~p. ==~n"
        "==        ██████╔╝█████╗  ███████║██╔████╔██║            ==~n"
        "==        ██╔══██╗██╔══╝  ██╔══██║██║╚██╔╝██║ EAT GLASS, ==~n"
        "==        ██████╔╝███████╗██║  ██║██║ ╚═╝ ██║ BUILD THE  ==~n"
        "==        ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝    FUTURE. ==~n"
        "===========================================================~n"
        "== Node activate at: ~s ==~n"
        "== Operator: ~s ==~n"
        "===========================================================~n"
        "== Config:                                               ==~n"
        "===========================================================~n"
        "   ~s~n"
        "===========================================================~n",
        [
            ?HYPERBEAM_VERSION,
            string:pad(
                lists:flatten(
                    io_lib:format(
                        "http://~s:~p",
                        [
                            hb_opts:get(host, <<"localhost">>, Config),
                            hb_opts:get(port, 8734, Config)
                        ]
                    )
                ),
                35, leading, $ 
            ),
            hb_util:human_id(ar_wallet:to_address(PrivWallet)),
            FormattedConfig
        ]
    ).
```

### new_server

Trigger the creation of a new HTTP server node. Accepts a `NodeMsg`

```erlang
new_server(RawNodeMsg) ->
    RawNodeMsgWithDefaults =
        hb_maps:merge(
            hb_opts:default_message_with_env(),
            RawNodeMsg#{ only => local }
        ),
    HookMsg = #{ <<"body">> => RawNodeMsgWithDefaults },
    NodeMsg =
        case dev_hook:on(<<"start">>, HookMsg, RawNodeMsgWithDefaults) of
            {ok, #{ <<"body">> := NodeMsgAfterHook }} -> NodeMsgAfterHook;
            Unexpected ->
                ?event(http,
                    {failed_to_start_server,
                        {unexpected_hook_result, Unexpected}
                    }
                ),
                throw(
                    {failed_to_start_server,
                        {unexpected_hook_result, Unexpected}
                    }
                )
        end,
    % Put server ID into node message so it's possible to update current server
    hb_http:start(),
    ServerID =
        hb_util:human_id(
            ar_wallet:to_address(
                hb_opts:get(
                    priv_wallet,
                    no_wallet,
                    NodeMsg
                )
            )
        ),
    % Put server ID into node message so it's possible to update current server
    % params
    NodeMsgWithID = hb_maps:put(http_server, ServerID, NodeMsg),
    Dispatcher = cowboy_router:compile([{'_', [{'_', ?MODULE, ServerID}]}]),
    ProtoOpts = #{
        env => #{dispatch => Dispatcher, node_msg => NodeMsgWithID},
        stream_handlers => [cowboy_stream_h],
        max_connections => infinity,
        idle_timeout => hb_opts:get(idle_timeout, 300000, NodeMsg)
    },
    PrometheusOpts =
        case hb_opts:get(prometheus, not hb_features:test(), NodeMsg) of
            true ->
                ?event(prometheus,
                    {starting_prometheus, {test_mode, hb_features:test()}}
                ),
                % Attempt to start the prometheus application, if possible.
```

### start_http3

```erlang
start_http3(ServerID, ProtoOpts, NodeMsg) ->
    ?event(http, {start_http3, ServerID}),
    Parent = self(),
    ServerPID =
        spawn(fun() ->
            application:ensure_all_started(quicer),
            {ok, Listener} = cowboy:start_quic(
                ServerID, 
                TransOpts = #{
                    socket_opts => [
                        {certfile, "test/test-tls.pem"},
                        {keyfile, "test/test-tls.key"},
                        {port, Port = hb_opts:get(port, 8734, NodeMsg)}
                    ]
                },
                ProtoOpts
            ),
            ranch_server:set_new_listener_opts(
                ServerID,
                1024,
                ranch:normalize_opts(
                    hb_maps:to_list(TransOpts#{ port => Port })
                ),
                ProtoOpts,
                []
            ),
            ranch_server:set_addr(ServerID, {<<"localhost">>, Port}),
            % Bypass ranch's requirement to have a connection supervisor define
            % to support updating protocol opts.
```

### http3_conn_sup_loop

```erlang
http3_conn_sup_loop() ->
    receive
        _ -> 
            % Ignore any other messages
            http3_conn_sup_loop()
    end.
```

### start_http2

```erlang
start_http2(ServerID, ProtoOpts, NodeMsg) ->
    ?event(http, {start_http2, ServerID}),
    StartRes = cowboy:start_clear(
        ServerID,
        [
            {port, Port = hb_opts:get(port, 8734, NodeMsg)}
        ],
        ProtoOpts
    ),
    case StartRes of
        {ok, Listener} ->
            ?event(debug_router_info, {http2_started, {listener, Listener}, {port, Port}}),
            {ok, Port, Listener};
        {error, {already_started, Listener}} ->
            ?event(http, {http2_already_started, {listener, Listener}}),
            ?event(debug_router_info,
                {restarting,
                    {id, ServerID},
                    {node_msg, NodeMsg}
                }
            ),
            cowboy:set_env(ServerID, node_msg, #{}),
            % {ok, Port, Listener}
            cowboy:stop_listener(ServerID),
            start_http2(ServerID, ProtoOpts, NodeMsg)
    end.
```

### init

Entrypoint for all HTTP requests. Receives the Cowboy request option and

```erlang
init(Req, ServerID) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> -> cors_reply(Req, ServerID);
        _ ->
            {ok, Body} = read_body(Req),
            handle_request(Req, Body, ServerID)
    end.
```

### read_body

Helper to grab the full body of a HTTP request, even if it's chunked.

```erlang
read_body(Req) -> read_body(Req, <<>>).
```

### read_body

Helper to grab the full body of a HTTP request, even if it's chunked.

```erlang
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} -> {ok, << Acc/binary, Data/binary >>};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.
```

### cors_reply

Reply to CORS preflight requests.

```erlang
cors_reply(Req, _ServerID) ->
    Req2 = cowboy_req:reply(204, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"*">>,
        <<"access-control-allow-methods">> =>
            <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>
    }, Req),
    ?event(http_debug, {cors_reply, {req, Req}, {req2, Req2}}),
    {ok, Req2, no_state}.
```

### handle_request

Handle all non-CORS preflight requests as AO-Core requests. Execution 

```erlang
handle_request(RawReq, Body, ServerID) ->
    % Insert the start time into the request so that it can be used by the
    % `hb_http' module to calculate the duration of the request.
```

### handle_error

Return a 500 error response to the client.

```erlang
handle_error(Req, Singleton, Type, Details, Stacktrace, NodeMsg) ->
    DetailsStr = hb_util:bin(hb_format:message(Details, NodeMsg, 1)),
    StacktraceStr = hb_util:bin(hb_format:trace(Stacktrace)),
    ErrorMsg =
        #{
            <<"status">> => 500,
            <<"type">> => hb_util:bin(hb_format:message(Type)),
            <<"details">> => DetailsStr,
            <<"stacktrace">> => StacktraceStr
        },
    ErrorBin = hb_format:error(ErrorMsg, NodeMsg),
    ?event(
        http_error,
        {returning_500_error,
            {string,
                hb_format:indent_lines(
                    <<"\n", ErrorBin/binary, "\n">>,
                    1
                )
            }
        }
    ),
    % Remove leading and trailing noise from the stacktrace and details.
```

### allowed_methods

Return the list of allowed methods for the HTTP server.

```erlang
allowed_methods(Req, State) ->
    {
        [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>, <<"PATCH">>],
        Req,
        State
    }.
```

### set_opts

Merges the provided `Opts` with uncommitted values from `Request`,

```erlang
set_opts(Opts) ->
    case hb_opts:get(http_server, no_server_ref, Opts) of
        no_server_ref ->
            ok;
        ServerRef ->
            ok = cowboy:set_env(ServerRef, node_msg, Opts)
    end.
```

### set_opts

```erlang
set_opts(Request, Opts) ->
    PreparedOpts =
        hb_opts:mimic_default_types(
            Opts,
            false,
            Opts
        ),
    PreparedRequest =
        hb_opts:mimic_default_types(
            hb_message:uncommitted(Request),
            false,
            Opts
        ),
    MergedOpts =
        maps:merge(
            PreparedOpts,
            PreparedRequest
        ),
    ?event(set_opts, {merged_opts, {explicit, MergedOpts}}),
    History =
        hb_opts:get(node_history, [], Opts)
            ++ [ hb_private:reset(maps:without([node_history], PreparedRequest)) ],
    FinalOpts = MergedOpts#{
        http_server => hb_opts:get(http_server, no_server, Opts),
        node_history => History
    },
    {set_opts(FinalOpts), FinalOpts}.
```

### get_opts

Get the node message for the current process.

```erlang
get_opts() ->
    get_opts(#{ http_server => get(server_id) }).
```

### get_opts

```erlang
get_opts(NodeMsg) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, NodeMsg),
    cowboy:get_env(ServerRef, node_msg, no_node_msg).
```

### set_proc_server_id

Initialize the server ID for the current process.

```erlang
set_proc_server_id(ServerID) ->
    put(server_id, ServerID).
```

### set_default_opts

Apply the default node message to the given opts map.

```erlang
set_default_opts(Opts) ->
    % Create a temporary opts map that does not include the defaults.
```

### start_node

Test that we can start the server, send a message, and get a response.

```erlang
start_node() ->
    start_node(#{}).
```

### start_node

```erlang
start_node(Opts) ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
        ranch,
        cowboy,
        gun,
        os_mon
    ]),
    hb:init(),
    hb_sup:start_link(Opts),
    ServerOpts = set_default_opts(Opts),
    {ok, _Listener, Port} = new_server(ServerOpts),
    <<"http://localhost:", (integer_to_binary(Port))/binary, "/">>.
%%% Tests
%%% The following only covering the HTTP server initialization process. For tests
%%% of HTTP server requests/responses, see `hb_http.erl'.
```

### set_node_opts_test

Ensure that the `start` hook can be used to modify the node options. We
Test the set_opts/2 function that merges request with options,

```erlang
set_node_opts_test() ->
    Node =
        start_node(#{
            on => #{
                <<"start">> => #{
                    <<"device">> =>
                        #{
                            <<"start">> =>
                                fun(_, #{ <<"body">> := NodeMsg }, _) ->
                                    {ok, #{
                                        <<"body">> =>
                                            NodeMsg#{ <<"test-success">> => true }
                                    }}
                                end
                        }
                }
            }
        }),
    {ok, LiveOpts} = hb_http:get(Node, <<"/~meta@1.0/info">>, #{}),
    ?assert(hb_ao:get(<<"test-success">>, LiveOpts, false, #{})).
```

### set_opts_test

Ensure that the `start` hook can be used to modify the node options. We
Test the set_opts/2 function that merges request with options,

```erlang
set_opts_test() ->
    DefaultOpts = hb_opts:default_message_with_env(),
    start_node(DefaultOpts#{ 
        priv_wallet => Wallet = ar_wallet:new(), 
        port => rand:uniform(10000) + 10000 
    }),
    Opts = get_opts(#{ 
        http_server => hb_util:human_id(ar_wallet:to_address(Wallet))
    }),
    NodeHistory = hb_opts:get(node_history, [], Opts),
    ?event(debug_node_history, {node_history_length, length(NodeHistory)}),
    ?assert(length(NodeHistory) == 0),
    % Test case 1: Empty node_history case
    Request1 = #{
        <<"hello">> => <<"world">>
    },             
    {ok, UpdatedOpts1} = set_opts(Request1, Opts),
    NodeHistory1 = hb_opts:get(node_history, not_found, UpdatedOpts1),
    Key1 = hb_opts:get(<<"hello">>, not_found, UpdatedOpts1),
    ?event(debug_node_history, {node_history_length, length(NodeHistory1)}),
    ?assert(length(NodeHistory1) == 1),
    ?assert(Key1 == <<"world">>),
    % Test case 2: Non-empty node_history case
    Request2 = #{
        <<"hello2">> => <<"world2">>
    },
    {ok, UpdatedOpts2} = set_opts(Request2, UpdatedOpts1),
    NodeHistory2 = hb_opts:get(node_history, not_found, UpdatedOpts2),
    Key2 = hb_opts:get(<<"hello2">>, not_found, UpdatedOpts2),
    ?event(debug_node_history, {node_history_length, length(NodeHistory2)}),
    ?assert(length(NodeHistory2) == 2),
    ?assert(Key2 == <<"world2">>),
    % Test case 3: Non-empty node_history case
    {ok, UpdatedOpts3} = set_opts(#{}, UpdatedOpts2#{ <<"hello3">> => <<"world3">> }),
    NodeHistory3 = hb_opts:get(node_history, not_found, UpdatedOpts3),
    Key3 = hb_opts:get(<<"hello3">>, not_found, UpdatedOpts3),
    ?event(debug_node_history, {node_history_length, length(NodeHistory3)}),
    ?assert(length(NodeHistory3) == 3),
    ?assert(Key3 == <<"world3">>).
```

### restart_server_test

Ensure that the `start` hook can be used to modify the node options. We
Test the set_opts/2 function that merges request with options,

```erlang
restart_server_test() ->
    % We force HTTP2, overriding the HTTP3 feature, because HTTP3 restarts don't work yet.
```

---

*Generated from [hb_http_server.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_http_server.erl)*
