%%% @doc A router that attaches a HTTP server to the Converge resolver.
%%% Because Converge is built to speak in HTTP semantics, this module
%%% only has to marshal the HTTP request into a message, and then
%%% pass it to the Converge resolver. 
%%% 
%%% `hb_http:reply/3' is used to respond to the client, handling the 
%%% process of converting a message back into an HTTP response.
%%% 
%%% The router uses an `Opts` message as its Cowboy initial state, 
%%% such that changing it on start of the router server allows for
%%% the execution parameters of all downstream requests to be controlled.
-module(hb_http_server).
-export([start/0, start/1, allowed_methods/2, init/2, set_opts/1]).
-export([start_test_node/0, start_test_node/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Starts the HTTP server. Optionally accepts an `Opts` message, which
%% is used as the source for server configuration settings, as well as the
%% `Opts` argument to use for all Converge resolution requests downstream.
start() ->
    start(#{ priv_wallet => hb:wallet(hb_opts:get(key_location)) }).
start(Opts) ->
    {ok, Listener, _Port} = new_server(Opts),
    {ok, Listener}.

new_server(RawNodeMsg) ->
    NodeMsg =
        maps:merge(
            hb_opts:default_message(),
            RawNodeMsg#{ only => local }
        ),
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
    Dispatcher =
        cowboy_router:compile(
            [
                % {HostMatch, list({PathMatch, Handler, InitialState})}
                {'_', [
                    {
                        "/metrics/[:registry]",
                        prometheus_cowboy2_handler,
                        #{}
                    },
                    {'_', ?MODULE, ServerID}
                ]}
            ]
        ),
    ProtoOpts = #{
        env => #{dispatch => Dispatcher, node_msg => NodeMsg},
        metrics_callback =>
            fun prometheus_cowboy2_instrumenter:observe/1,
        stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
    },
    {ok, Port, Listener} =
        case Protocol = hb_opts:get(protocol, no_proto, NodeMsg) of
            http3 ->
                start_http3(ServerID, ProtoOpts, NodeMsg);
            Pro when Pro =:= http2; Pro =:= http1 ->
        		% The HTTP/2 server has fallback mode to 1.1 as necessary.
                start_http2(ServerID, ProtoOpts, NodeMsg);
            _ -> {error, {unknown_protocol, Protocol}}
        end,
    ?event(debug,
        {http_server_started,
            {listener, Listener},
            {server_id, ServerID},
            {port, Port},
            {protocol, Protocol}
        }
    ),
    {ok, Listener, Port}.

start_http3(ServerID, ProtoOpts, _NodeMsg) ->
    ?event(debug, {start_http3, ServerID}),
    Parent = self(),
    ServerPID =
        spawn(fun() ->
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
            Parent ! {ok, GivenPort},
            receive stop -> stopped end
        end),
    receive {ok, GivenPort} -> {ok, GivenPort, ServerPID}
    after 2000 ->
        {error, {timeout, staring_http3_server, ServerID}}
    end.

start_http2(ServerID, ProtoOpts, NodeMsg) ->
    ?event(debug, {start_http2, ServerID}),
    {ok, Listener} = cowboy:start_clear(
        ServerID,
        [
            {port, Port = hb_opts:get(port, 8734, NodeMsg)}
        ],
        ProtoOpts
    ),
    {ok, Port, Listener}.

init(Req, ServerID) ->
    NodeMsg = cowboy:get_env(ServerID, node_msg, no_node_msg),
    % Parse the HTTP request into HyerBEAM's message format.
    ReqSingleton = hb_http:req_to_tabm_singleton(Req, NodeMsg),
    ?event(http, {http_inbound, ReqSingleton}),
    case dev_meta:handle(NodeMsg, ReqSingleton) of
        {ok, Res} -> hb_http:reply(Req, Res);
        {error, _} -> hb_http:reply(Req, 404, <<"Not Found">>)
    end.

%% @doc Return the complete Ranch ETS table for the node for debugging.
ranch_ets() ->
    case ets:info(ranch_server) of
        undefined -> [];
        _ -> ets:tab2list(ranch_server)
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

%% @doc Update the `Opts` map that the HTTP server uses for all future
%% requests.
set_opts(Opts) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, Opts),
    cowboy:set_env(ServerRef, opts, Opts).

%%% Tests

test_opts(Opts) ->
    rand:seed(default),
    % Generate a random port number between 42000 and 62000 to use
    % for the server.
    Port = 10000 + rand:uniform(20000),
    Wallet = ar_wallet:new(),
    Opts#{
        % Generate a random port number between 8000 and 9000.
        port => Port,
        store =>
            {hb_store_fs,
                #{
                    prefix =>
                        <<"TEST-cache-", (integer_to_binary(Port))/binary>>
                }
            },
        priv_wallet => Wallet
    }.

%% @doc Test that we can start the server, send a message, and get a response.
start_test_node() ->
    start_test_node(#{}).
start_test_node(Opts) ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
        debugger,
        ranch,
        cowboy,
        gun,
        prometheus,
        prometheus_cowboy,
        os_mon,
        rocksdb
    ]),
    hb:init(),
    hb_sup:start_link(Opts),
    ServerOpts = test_opts(Opts),
    {ok, _Listener, Port} = new_server(ServerOpts),
    <<"http://localhost:", (integer_to_binary(Port))/binary, "/">>.

raw_http_access_test() ->
    URL = start_test_node(#{ protocol => http1 }),
    TX =
        ar_bundles:serialize(
            hb_message:convert(
                #{
                    path => <<"Key1">>,
                    <<"Key1">> => #{ <<"Key2">> => <<"Value1">> }
                },
                tx,
                converge,
                #{}
            )
        ),
    {ok, {{_, 200, _}, _, Body}} =
        httpc:request(
            post,
            {iolist_to_binary(URL), [], "application/octet-stream", TX},
            [],
            [{body_format, binary}]
        ),
    Msg = hb_message:convert(ar_bundles:deserialize(Body), converge, tx, #{}),
    ?assertEqual(<<"Value1">>, hb_converge:get(<<"Key2">>, Msg, #{})).