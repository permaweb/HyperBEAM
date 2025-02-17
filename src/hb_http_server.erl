%%% @doc A router that attaches a HTTP server to the Converge resolver.
%%% Because Converge is built to speak in HTTP semantics, this module
%%% only has to marshal the HTTP request into a message, and then
%%% pass it to the Converge resolver. 
%%% 
%%% `hb_http:reply/4' is used to respond to the client, handling the 
%%% process of converting a message back into an HTTP response.
%%% 
%%% The router uses an `Opts' message as its Cowboy initial state, 
%%% such that changing it on start of the router server allows for
%%% the execution parameters of all downstream requests to be controlled.
-module(hb_http_server).
-export([start/0, start/1, allowed_methods/2, init/2, set_opts/1, get_opts/1]).
-export([start_node/0, start_node/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Starts the HTTP server. Optionally accepts an `Opts' message, which
%% is used as the source for server configuration settings, as well as the
%% `Opts' argument to use for all Converge resolution requests downstream.
start() ->
    ?event(http, {start_store, "main-cache"}),
    Store = [{hb_store_fs, #{ prefix => "main-cache" }}],
    hb_store:start(Store),
    start(
        #{
            priv_wallet => hb:wallet(hb_opts:get(key_location)),
            store => Store,
            port => hb_opts:get(http_default_remote_port, 8734)
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
        prometheus,
        prometheus_cowboy,
        os_mon,
        rocksdb
    ]),
    hb:init(),
    BaseOpts = set_base_opts(Opts),
    {ok, Listener, _Port} = new_server(BaseOpts),
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
    % Put server ID into node message so it's possible to update current server
    % params
    NodeMsgWithID = maps:put(http_server, ServerID, NodeMsg),
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
        env => #{dispatch => Dispatcher, node_msg => NodeMsgWithID},
        metrics_callback =>
            fun prometheus_cowboy2_instrumenter:observe/1,
        stream_handlers => [cowboy_metrics_h, cowboy_stream_h],
        max_connections => infinity,
        idle_timeout => hb_opts:get(idle_timeout, 300000, NodeMsg)
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
        <<"OPTIONS">> ->
            cors_reply(Req, ServerID);
        _ ->
            handle_request(Req, ServerID)
    end.

%% @doc Reply to CORS preflight requests.
cors_reply(Req, _ServerID) ->
    cowboy_req:reply(204, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
        <<"access-control-allow-headers">> =>
            cowboy_req:header(<<"access-control-request-headers">>, Req, <<"">>)
    }, Req).

%% @doc Handle all non-CORS preflight requests as Converge requests. Execution 
%% starts by parsing the HTTP request into HyerBEAM's message format, then
%% passing the message directly to `meta@1.0` which handles calling Converge in
%% the appropriate way.
handle_request(Req, ServerID) ->
    NodeMsg = get_opts(#{ http_server => ServerID }),
    ?event(http, {http_inbound, Req}),
    % Parse the HTTP request into HyerBEAM's message format.
    ReqSingleton = hb_http:req_to_tabm_singleton(Req, NodeMsg),
    ?event(http, {parsed_singleton, ReqSingleton}),
    {ok, Res} = dev_meta:handle(NodeMsg, ReqSingleton),
    hb_http:reply(Req, Res, NodeMsg).

%% @doc Return the list of allowed methods for the HTTP server.
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

%% @doc Update the `Opts' map that the HTTP server uses for all future
%% requests.
set_opts(Opts) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, Opts),
    ok = cowboy:set_env(ServerRef, node_msg, Opts).

get_opts(NodeMsg) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, NodeMsg),
    cowboy:get_env(ServerRef, node_msg, no_node_msg).

%%% Tests

set_base_opts(Opts) ->
    % Generate a random port number between 10000 and 30000 to use
    % for the server.
    Port =
        case hb_opts:get(port, no_port, Opts#{ only => local }) of
            no_port ->
                rand:seed(exsplus, erlang:timestamp()),
                10000 + rand:uniform(20000);
            PassedPort -> PassedPort
        end,
    Wallet =
        case hb_opts:get(priv_wallet, no_viable_wallet, Opts) of
            no_viable_wallet -> ar_wallet:new();
            PassedWallet -> PassedWallet
        end,
    Store =
        case hb_opts:get(store, no_store, Opts) of
            no_store ->
                {hb_store_fs,
                    #{
                        prefix =>
                            <<"TEST-cache-", (integer_to_binary(Port))/binary>>
                    }
                };
            PassedStore -> PassedStore
        end,
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
        prometheus,
        prometheus_cowboy,
        os_mon,
        rocksdb
    ]),
    hb:init(),
    hb_sup:start_link(Opts),
    ServerOpts = set_base_opts(Opts),
    {ok, _Listener, Port} = new_server(ServerOpts),
    <<"http://localhost:", (integer_to_binary(Port))/binary, "/">>.