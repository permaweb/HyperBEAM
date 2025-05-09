%%% @doc A router that attaches a HTTP server to the AO-Core resolver.
%%% Because AO-Core is built to speak in HTTP semantics, this module
%%% only has to marshal the HTTP request into a message, and then
%%% pass it to the AO-Core resolver. 
%%% 
%%% `hb_http:reply/4' is used to respond to the client, handling the 
%%% process of converting a message back into an HTTP response.
%%% 
%%% The router uses an `Opts' message as its Cowboy initial state, 
%%% such that changing it on start of the router server allows for
%%% the execution parameters of all downstream requests to be controlled.
-module(hb_http_server).
-export([start/0, start/1, allowed_methods/2, init/2, set_opts/1, set_opts/2, get_opts/1]).
-export([start_node/0, start_node/1, set_default_opts/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Starts the HTTP server. Optionally accepts an `Opts' message, which
%% is used as the source for server configuration settings, as well as the
%% `Opts' argument to use for all AO-Core resolution requests downstream.
start() ->
    ?event(http, {start_store, <<"cache-mainnet">>}),
    Store = hb_opts:get(store, no_store, #{}),
    hb_store:start(Store),
    Loaded =
        case hb_opts:load(Loc = hb_opts:get(hb_config_location, <<"config.flat">>, #{})) of
            {ok, Conf} ->
                ?event(boot, {loaded_config, Loc, Conf}),
                Conf;
            {error, Reason} ->
                ?event(boot, {failed_to_load_config, Loc, Reason}),
                #{}
        end,
    MergedConfig =
        maps:merge(
            hb_opts:default_message(),
            Loaded
        ),
    PrivWallet =
        hb:wallet(
            hb_opts:get(
                priv_key_location,
                <<"hyperbeam-key.json">>,
                Loaded
            )
        ),
    FormattedConfig = hb_util:debug_fmt(MergedConfig, 2),
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
                            hb_opts:get(host, <<"localhost">>, Loaded),
                            hb_opts:get(port, 8734, Loaded)
                        ]
                    )
                ),
                35, leading, $ 
            ),
            hb_util:human_id(ar_wallet:to_address(PrivWallet)),
            FormattedConfig
        ]
    ),
    start(
        Loaded#{
            priv_wallet => PrivWallet,
            store => Store,
            port => hb_opts:get(port, 8734, Loaded),
            cache_writers => [hb_util:human_id(ar_wallet:to_address(PrivWallet))]
        }
    ).
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

%% @doc Trigger the creation of a new HTTP server node. Accepts a `NodeMsg'
%% message, which is used to configure the server. This function executed the
%% `start' hook on the node, giving it the opportunity to modify the `NodeMsg'
%% before it is used to configure the server. The `start' hook expects gives and
%% expects the node message to be in the `body' key.
new_server(RawNodeMsg) ->
    RawNodeMsgWithDefaults =
        maps:merge(
            hb_opts:default_message(),
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
    NodeMsgWithID = maps:put(http_server, ServerID, NodeMsg),
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
                try
                    application:ensure_all_started([prometheus, prometheus_cowboy]),
                    ProtoOpts#{
                        metrics_callback =>
                            fun prometheus_cowboy2_instrumenter:observe/1,
                        stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
                    }
                catch
                    Type:Reason ->
                        % If the prometheus application is not started, we can
                        % still start the HTTP server, but we won't have any
                        % metrics.
                        ?event(prometheus,
                            {prometheus_not_started, {type, Type}, {reason, Reason}}
                        ),
                        ProtoOpts
                end;
            false ->
                ?event(prometheus,
                    {prometheus_not_started, {test_mode, hb_features:test()}}
                ),
                ProtoOpts
        end,
    DefaultProto =
        case hb_features:http3() of
            true -> http3;
            false -> http2
        end,
    {ok, Port, Listener} =
        case Protocol = hb_opts:get(protocol, DefaultProto, NodeMsg) of
            http3 ->
                start_http3(ServerID, PrometheusOpts, NodeMsg);
            Pro when Pro =:= http2; Pro =:= http1 ->
                % The HTTP/2 server has fallback mode to 1.1 as necessary.
                start_http2(ServerID, PrometheusOpts, NodeMsg);
            _ -> {error, {unknown_protocol, Protocol}}
        end,
    ?event(http,
        {http_server_started,
            {listener, Listener},
            {server_id, ServerID},
            {port, Port},
            {protocol, Protocol},
            {store, hb_opts:get(store, no_store, NodeMsg)}
        }
    ),
    {ok, Listener, Port}.

start_http3(ServerID, ProtoOpts, _NodeMsg) ->
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
                        {keyfile, "test/test-tls.key"}
                    ]
                },
                ProtoOpts
            ),
            {ok, {_, GivenPort}} = quicer:sockname(Listener),
            ranch_server:set_new_listener_opts(
                ServerID,
                1024,
                ranch:normalize_opts(
                    maps:to_list(TransOpts#{ port => GivenPort })
                ),
                ProtoOpts,
                []
            ),
            ranch_server:set_addr(ServerID, {<<"localhost">>, GivenPort}),
            % Bypass ranch's requirement to have a connection supervisor define
            % to support updating protocol opts.
            % Quicer doesn't use a connection supervisor, so we just spawn one
            % that does nothing.
            ConnSup = spawn(fun() -> http3_conn_sup_loop() end),
            ranch_server:set_connections_sup(ServerID, ConnSup),
            Parent ! {ok, GivenPort},
            receive stop -> stopped end
        end),
    receive {ok, GivenPort} -> {ok, GivenPort, ServerPID}
    after 2000 ->
        {error, {timeout, staring_http3_server, ServerID}}
    end.

http3_conn_sup_loop() ->
    receive
        _ -> 
            % Ignore any other messages
            http3_conn_sup_loop()
    end.

start_http2(ServerID, ProtoOpts, NodeMsg) ->
    ?event(http, {start_http2, ServerID}),
    {ok, Listener} = cowboy:start_clear(
        ServerID,
        [
            {port, Port = hb_opts:get(port, 8734, NodeMsg)}
        ],
        ProtoOpts
    ),
    {ok, Port, Listener}.

%% @doc Entrypoint for all HTTP requests. Receives the Cowboy request option and
%% the server ID, which can be used to lookup the node message.
init(Req, ServerID) ->
    case cowboy_req:method(Req) of
        <<"OPTIONS">> -> cors_reply(Req, ServerID);
        _ ->
            {ok, Body} = read_body(Req),
            handle_request(Req, Body, ServerID)
    end.

%% @doc Helper to grab the full body of a HTTP request, even if it's chunked.
read_body(Req) -> read_body(Req, <<>>).
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} -> {ok, << Acc/binary, Data/binary >>};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

%% @doc Reply to CORS preflight requests.
cors_reply(Req, _ServerID) ->
    Req2 = cowboy_req:reply(204, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"*">>,
        <<"access-control-allow-methods">> =>
            <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>
    }, Req),
    ?event(http_debug, {cors_reply, {req, Req}, {req2, Req2}}),
    {ok, Req2, no_state}.

%% @doc Handle all non-CORS preflight requests as AO-Core requests. Execution 
%% starts by parsing the HTTP request into HyerBEAM's message format, then
%% passing the message directly to `meta@1.0' which handles calling AO-Core in
%% the appropriate way.
handle_request(RawReq, Body, ServerID) ->
    % Insert the start time into the request so that it can be used by the
    % `hb_http' module to calculate the duration of the request.
    StartTime = os:system_time(millisecond),
    Req = RawReq#{ start_time => StartTime },
    NodeMsg = get_opts(#{ http_server => ServerID }),
    case {cowboy_req:path(RawReq), cowboy_req:qs(RawReq)} of
        {<<"/">>, <<>>} ->
            % If the request is for the root path, serve a redirect to the default 
            % request of the node.
            cowboy_req:reply(
                302,
                #{
                    <<"location">> =>
                        hb_opts:get(
                            default_req,
                            <<"/~hyperbuddy@1.0/index">>,
                            NodeMsg
                        )
                },
                RawReq
            );
        _ ->
            % The request is of normal AO-Core form, so we parse it and invoke
            % the meta@1.0 device to handle it.
            ?event(http, {http_inbound, {cowboy_req, Req}, {body, {string, Body}}}),
            TracePID = hb_tracer:start_trace(),
            % Parse the HTTP request into HyerBEAM's message format.
            try 
                ReqSingleton = hb_http:req_to_tabm_singleton(Req, Body, NodeMsg),
                CommitmentCodec = hb_http:accept_to_codec(ReqSingleton, NodeMsg),
                ?event(http,
                    {parsed_singleton,
                        {req_singleton, ReqSingleton},
                        {accept_codec, CommitmentCodec}},
                    #{trace => TracePID}
                ),
                % hb_tracer:record_step(TracePID, request_parsing),
                % Invoke the meta@1.0 device to handle the request.
                {ok, Res} =
                    dev_meta:handle(
                        NodeMsg#{
                            commitment_device => CommitmentCodec,
                            trace => TracePID
                        },
                        ReqSingleton
                    ),
                hb_http:reply(Req, ReqSingleton, Res, NodeMsg)
            catch
                Type:Details:Stacktrace ->
                    Trace = hb_tracer:get_trace(TracePID),
                    TraceString = hb_tracer:format_error_trace(Trace),
                    ?event(
                        http_error,
                        {http_error,
                            {type, Type},
                            {details, Details},
                            {stacktrace, Stacktrace}
                        }
                    ),
                    hb_http:reply(
                        Req,
                        #{},
                        #{
                            <<"status">> => 500,
                            <<"body">> => TraceString
                        },
                        NodeMsg
                    )
            end
end.

%% @doc Return the list of allowed methods for the HTTP server.
allowed_methods(Req, State) ->
    {
        [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>, <<"PATCH">>],
        Req,
        State
    }.

%% @doc Merges the provided `Opts' with uncommitted values from `Request',
%% preserves the http_server value, and updates node_history by prepending
%% the `Request'. If a server reference exists, updates the Cowboy environment
%% variable 'node_msg' with the resulting options map.
set_opts(Opts) ->
    case hb_opts:get(http_server, no_server_ref, Opts) of
        no_server_ref ->
            ok;
        ServerRef ->
            ok = cowboy:set_env(ServerRef, node_msg, Opts)
    end.
set_opts(Request, Opts) ->
    MergedOpts =
        maps:merge(
            Opts,
            hb_opts:mimic_default_types(
                hb_message:uncommitted(Request),
                new_atoms
            )
        ),
    FinalOpts = MergedOpts#{
        http_server => hb_opts:get(http_server, no_server, Opts),
        node_history => [Request | hb_opts:get(node_history, [], Opts)]
    },
    {set_opts(FinalOpts), FinalOpts}.

get_opts(NodeMsg) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, NodeMsg),
    cowboy:get_env(ServerRef, node_msg, no_node_msg).

set_default_opts(Opts) ->
    % Create a temporary opts map that does not include the defaults.
    TempOpts = Opts#{ only => local },
    % Generate a random port number between 10000 and 30000 to use
    % for the server.
    Port =
        case hb_opts:get(port, no_port, TempOpts) of
            no_port ->
                rand:seed(exsplus, erlang:system_time(microsecond)),
                10000 + rand:uniform(50000);
            PassedPort -> PassedPort
        end,
    Wallet =
        case hb_opts:get(priv_wallet, no_viable_wallet, TempOpts) of
            no_viable_wallet -> ar_wallet:new();
            PassedWallet -> PassedWallet
        end,
    Store =
        case hb_opts:get(store, no_store, TempOpts) of
            no_store ->
                TestDir = <<"cache-TEST/run-fs-", (integer_to_binary(Port))/binary>>,
                filelib:ensure_dir(binary_to_list(TestDir)),
                #{ <<"store-module">> => hb_store_fs, <<"prefix">> => TestDir };
            PassedStore -> PassedStore
        end,
    ?event({set_default_opts,
        {given, TempOpts},
        {port, Port},
        {store, Store},
        {wallet, Wallet}
    }),
    Opts#{
        port => Port,
        store => Store,
        priv_wallet => Wallet,
        address => hb_util:human_id(ar_wallet:to_address(Wallet)),
        force_signed => true
    }.

%% @doc Test that we can start the server, send a message, and get a response.
start_node() ->
    start_node(#{}).
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

%% @doc Ensure that the `start' hook can be used to modify the node options. We
%% do this by creating a message with a device that has a `start' key. This 
%% key takes the message's body (the anticipated node options) and returns a
%% modified version of that body, which will be used to configure the node. We
%% then check that the node options were modified as we expected.
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
