%%% @doc HyperBEAM HTTP/HTTPS server with SSL certificate integration.
%%%
%%% This module provides a complete HTTP and HTTPS server implementation
%%% for HyperBEAM nodes, with automatic SSL certificate management and
%%% HTTP to HTTPS redirect capabilities.
%%%
%%% Key features:
%%% - HTTP server with AO-Core integration for message processing
%%% - HTTPS server with automatic SSL certificate deployment
%%% - HTTP to HTTPS redirect with 301 Moved Permanently responses
%%% - SSL certificate integration via dev_ssl_cert device
%%% - Configurable ports for development and production
%%% - Prometheus metrics integration (optional)
%%% - Complete application lifecycle management
%%%
%%% The module marshals HTTP requests into HyperBEAM message format,
%%% processes them through the AO-Core resolver, and converts responses
%%% back to HTTP format using `hb_http:reply/4'.
%%%
%%% Configuration is managed through an `Opts' message that serves as
%%% Cowboy's initial state, allowing dynamic control of execution
%%% parameters for all downstream requests.
-module(hb_http_server).

%% Public API exports
-export([
    start/0, start/1,
    start_node/0, start_node/1,
    start_https_node/5
]).

%% Request handling exports  
-export([
    init/2,
    allowed_methods/2
]).

%% HTTPS and redirect exports
-export([
    redirect_to_https/3
]).

%% Configuration and state management exports
-export([
    set_opts/1, set_opts/2,
    get_opts/0, get_opts/1,
    set_default_opts/1,
    set_proc_server_id/1
]).

%% Type specifications
-type server_opts() :: map().
-type server_id() :: binary().
-type listener_ref() :: pid().

%% Function specifications
-spec start() -> {ok, listener_ref()}.
-spec start(server_opts()) -> {ok, listener_ref()}.
-spec start_node() -> binary().
-spec start_node(server_opts()) -> binary().
-spec start_https_node(
    binary(), 
    binary(), 
    server_opts(), 
    server_id() | no_server,
    integer()
) -> binary().
-spec redirect_to_https(cowboy_req:req(), server_opts(), integer()) -> 
    {ok, cowboy_req:req(), server_opts()}.

-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Default configuration constants
-define(DEFAULT_HTTP_PORT, 8734).
-define(DEFAULT_IDLE_TIMEOUT, 300000).
-define(DEFAULT_CONFIG_FILE, <<"config.flat">>).
-define(DEFAULT_PRIV_KEY_FILE, <<"hyperbeam-key.json">>).
-define(DEFAULT_DASHBOARD_PATH, <<"/~hyperbuddy@1.0/dashboard">>).
-define(RANDOM_PORT_MIN, 10000).
-define(RANDOM_PORT_RANGE, 50000).

%% Test certificate paths
-define(TEST_CERT_FILE, "test/test-tls.pem").
-define(TEST_KEY_FILE, "test/test-tls.key").

%% HTTP/3 timeouts
-define(HTTP3_STARTUP_TIMEOUT, 2000).

%%% ===================================================================
%%% Public API & Main Entry Points
%%% ===================================================================

%% @doc Starts the HTTP server with configuration loading and setup.
%%
%% This function performs the complete HTTP server initialization including:
%% 1. Loading configuration from files
%% 2. Setting up store and wallet configuration
%% 3. Displaying the startup greeter message
%% 4. Starting the HTTP server with merged configuration
%%
%% The function loads configuration from the configured location, merges it
%% with environment defaults, and starts all necessary services.
%%
%% @returns {ok, Listener} where Listener is the Cowboy listener PID
start() ->
    ?event(http, {start_store, <<"cache-mainnet">>}),
    Loaded =
        case hb_opts:load(
            Loc = hb_opts:get(hb_config_location, ?DEFAULT_CONFIG_FILE)
        ) of
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
            _ when is_list(StoreOpts) -> 
                hb_store_opts:apply(StoreOpts, StoreDefaults);
            _ -> StoreOpts
        end,
    hb_store:start(UpdatedStoreOpts),
    PrivWallet =
        hb:wallet(
            hb_opts:get(
                priv_key_location,
                ?DEFAULT_PRIV_KEY_FILE,
                Loaded
            )
        ),
    print_greeter_if_not_test(MergedConfig, PrivWallet),
    start(
        Loaded#{
            priv_wallet => PrivWallet,
            store => UpdatedStoreOpts,
            port => hb_opts:get(port, ?DEFAULT_HTTP_PORT, Loaded),
            cache_writers => 
                [hb_util:human_id(ar_wallet:to_address(PrivWallet))]
        }
    ).

%% @doc Starts the HTTP server with provided options.
%%
%% This function starts the HTTP server using the provided configuration
%% options. It ensures all required applications are started, initializes
%% HyperBEAM, and creates the server with default option processing.
%%
%% @param Opts Configuration options map for the server
%% @returns {ok, Listener} where Listener is the Cowboy listener PID
start(Opts) ->
    start_required_applications(),
    hb:init(),
    BaseOpts = set_default_opts(Opts),
    {ok, Listener, _Port} = new_server(BaseOpts),
    {ok, Listener}.

%% @doc Start a test node with default configuration.
%%
%% This function starts a complete HyperBEAM node for testing purposes
%% using default configuration. It's a convenience wrapper around
%% start_node/1 with an empty options map.
%%
%% @returns Node URL binary for making HTTP requests
start_node() ->
    start_node(#{}).

%% @doc Start a complete HyperBEAM node with custom configuration.
%%
%% This function performs complete node startup including:
%% 1. Starting all required Erlang applications
%% 2. Initializing HyperBEAM core systems
%% 3. Starting the supervisor tree
%% 4. Creating and starting the HTTP server
%% 5. Returning the node URL for client connections
%%
%% @param Opts Configuration options map for the node
%% @returns Node URL binary like <<"http://localhost:8734/">>
start_node(Opts) ->
    start_required_applications(),
    hb:init(),
    hb_sup:start_link(Opts),
    ServerOpts = set_default_opts(Opts),
    {ok, _Listener, Port} = new_server(ServerOpts),
    <<"http://localhost:", (integer_to_binary(Port))/binary, "/">>.

%% @doc Start an HTTPS node with the given certificate and key.
%%
%% This function follows the same pattern as start_node() but creates an HTTPS
%% server instead of HTTP. It does complete application startup, supervisor
%% initialization, and proper node configuration.
%%
%% @param CertFile Path to certificate PEM file
%% @param KeyFile Path to private key PEM file
%% @param Opts Server configuration options (supports https_port)
%% @param RedirectTo HTTP server ID to configure for redirect
%% @param HttpsPort HTTPS port number for the server
%% @returns HTTPS node URL binary like <<"https://localhost:8443/">>
start_https_node(CertFile, KeyFile, Opts, RedirectTo, HttpsPort) ->
    ?event(https, {starting_https_node, {opts_keys, maps:keys(Opts)}}),
    % Ensure all required applications are started
    start_required_applications(),
    % Initialize HyperBEAM
    hb:init(),
    % Start supervisor with HTTPS-specific options
    StrippedOpts = maps:without([port], Opts),
    HttpsOpts = StrippedOpts#{
        port => HttpsPort
    },
    hb_sup:start_link(HttpsOpts),
    % Set up server options for HTTPS
    ServerOpts = set_default_opts(HttpsOpts),
    % Create the HTTPS server using new_server with TLS transport
    {ok, _Listener, Port} = 
        new_https_server(ServerOpts, CertFile, KeyFile, RedirectTo, HttpsPort),
    % Return HTTPS URL
    <<"https://localhost:", (integer_to_binary(Port))/binary, "/">>.

%%% ===================================================================
%%% Core Server Creation
%%% ===================================================================

%% @doc Create a new HTTP server with full configuration processing.
%%
%% This function handles the complete HTTP server creation workflow:
%% 1. Merging provided options with environment defaults
%% 2. Processing startup hooks for configuration modification
%% 3. Generating unique server identifiers
%% 4. Setting up Cowboy dispatchers and protocol options
%% 5. Configuring optional Prometheus metrics
%% 6. Starting the appropriate protocol listener (HTTP/2 or HTTP/3)
%%
%% @param RawNodeMsg Raw node message configuration
%% @returns {ok, Listener, Port} or {error, Reason}
new_server(RawNodeMsg) ->
    % Prepare node message with defaults
    RawNodeMsgWithDefaults =
        hb_maps:merge(
            hb_opts:default_message_with_env(),
            RawNodeMsg#{ only => local }
        ),
    % Process startup hooks using shared utility
    {ok, NodeMsg} = process_server_hooks(RawNodeMsgWithDefaults),
    % Initialize HTTP and create server ID
    hb_http:start(),
    ServerID = generate_server_id(NodeMsg),
    % Create protocol options with Prometheus support
    ProtoOpts = create_base_protocol_opts(ServerID, NodeMsg),
    PrometheusOpts = add_prometheus_if_enabled(ProtoOpts, NodeMsg),
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

%% @doc Create a new HTTPS server with TLS configuration.
%%
%% This function creates an HTTPS server using the provided SSL certificate
%% files. It handles the complete HTTPS server setup including:
%% 1. Processing server startup hooks
%% 2. Creating unique HTTPS server identifiers
%% 3. Setting up dispatchers and protocol options
%% 4. Configuring Prometheus metrics if enabled
%% 5. Starting the TLS listener with certificates
%% 6. Setting up HTTP to HTTPS redirect if requested
%%
%% @param Opts Server configuration options
%% @param CertFile Path to SSL certificate PEM file
%% @param KeyFile Path to SSL private key PEM file
%% @param RedirectTo HTTP server ID to configure for redirect (or no_server)
%% @param HttpsPort HTTPS port number for the server
%% @returns {ok, Listener, Port} or {error, Reason}
new_https_server(Opts, CertFile, KeyFile, RedirectTo, HttpsPort) ->
    ?event(https, {creating_new_https_server, {opts_keys, maps:keys(Opts)}}),
    try
        {ok, NodeMsg} = process_server_hooks(Opts),
        {_ServerID, HttpsServerID} = create_https_server_id(NodeMsg),
        {_Dispatcher, ProtoOpts} = 
            create_https_dispatcher(HttpsServerID, NodeMsg),
        FinalProtoOpts = add_prometheus_if_enabled(ProtoOpts, NodeMsg),
        {ok, Listener} = 
            start_tls_listener(
                HttpsServerID, 
                HttpsPort, 
                CertFile, 
                KeyFile, 
                FinalProtoOpts
            ),
        setup_redirect_if_needed(RedirectTo, NodeMsg, HttpsPort),
        {ok, Listener, HttpsPort}
    catch
        Error:Reason:Stacktrace ->
            ?event(
                https, 
                {
                    https_server_creation_failed, 
                    {error, Error}, 
                    {reason, Reason}, 
                    {stacktrace, Stacktrace}
                }
            ),
            {error, {Error, Reason}}
    end.

%%% ===================================================================
%%% Protocol-Specific Server Functions
%%% ===================================================================

%% @doc Start HTTP/3 server using QUIC transport.
%%
%% This function starts an HTTP/3 server using the QUIC protocol for
%% enhanced performance. It handles:
%% 1. Starting the QUICER application for QUIC support
%% 2. Creating a Cowboy QUIC listener with test certificates
%% 3. Configuring Ranch server options for QUIC transport
%% 4. Setting up connection supervision
%%
%% @param ServerID Unique server identifier
%% @param ProtoOpts Protocol options for Cowboy
%% @param NodeMsg Node configuration message
%% @returns {ok, Port, ServerPID} or {error, Reason}
start_http3(ServerID, ProtoOpts, NodeMsg) ->
    ?event(http, {start_http3, ServerID}),
    Parent = self(),
    ServerPID =
        spawn(fun() ->
            application:ensure_all_started(quicer),
            {ok, _Listener} = cowboy:start_quic(
                ServerID, 
                TransOpts = #{
                    socket_opts => [
                        {certfile, ?TEST_CERT_FILE},
                        {keyfile, ?TEST_KEY_FILE},
                        {port, Port = hb_opts:get(port, ?DEFAULT_HTTP_PORT, NodeMsg)}
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
            % Quicer doesn't use a connection supervisor, so we just spawn one
            % that does nothing.
            ConnSup = spawn(fun() -> http3_conn_sup_loop() end),
            ranch_server:set_connections_sup(ServerID, ConnSup),
            Parent ! {ok, Port},
            receive stop -> stopped end
        end),
    receive {ok, Port} -> {ok, Port, ServerPID}
    after ?HTTP3_STARTUP_TIMEOUT ->
        {error, {timeout, starting_http3_server, ServerID}}
    end.

%% @doc HTTP/3 connection supervisor loop.
%%
%% This function provides a minimal connection supervisor for HTTP/3
%% servers. QUIC doesn't use traditional connection supervisors, so
%% this is a placeholder that ignores all messages.
%%
%% @returns never returns (infinite loop)
http3_conn_sup_loop() ->
    receive
        _ -> 
            % Ignore any other messages
            http3_conn_sup_loop()
    end.

%% @doc Start HTTP/2 server using TCP transport.
%%
%% This function starts an HTTP/2 server with fallback to HTTP/1.1
%% using TCP transport. It handles:
%% 1. Starting a Cowboy clear (non-TLS) listener
%% 2. Port configuration and binding
%% 3. Restart handling for already-started listeners
%%
%% @param ServerID Unique server identifier
%% @param ProtoOpts Protocol options for Cowboy
%% @param NodeMsg Node configuration message
%% @returns {ok, Port, Listener} or {error, Reason}
start_http2(ServerID, ProtoOpts, NodeMsg) ->
    ?event(http, {start_http2, ServerID}),
    StartRes = cowboy:start_clear(
        ServerID,
        [
            {port, Port = hb_opts:get(port, ?DEFAULT_HTTP_PORT, NodeMsg)}
        ],
        ProtoOpts
    ),
    case StartRes of
        {ok, Listener} ->
            ?event(
                debug_router_info, 
                {http2_started, {listener, Listener}, {port, Port}}
            ),
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


%%% ===================================================================
%%% Request Handling
%%% ===================================================================

%% @doc Entrypoint for all HTTP requests.
%%
%% This function serves as the main entry point for all incoming HTTP
%% requests. It handles two types of requests:
%% 1. Redirect requests - configured to redirect HTTP to HTTPS
%% 2. Normal requests - standard HyperBEAM request processing
%%
%% The function routes requests based on the handler state type.
%%
%% @param Req Cowboy request object
%% @param State Either {redirect_https, Opts, HttpsPort} or ServerID
%% @returns {ok, UpdatedReq, State}
init(Req, {redirect_https, Opts, HttpsPort}) ->
    % Handle HTTPS redirect
    redirect_to_https(Req, Opts, HttpsPort);
init(Req, ServerID) ->
    % Handle normal requests
    case cowboy_req:method(Req) of
        <<"OPTIONS">> -> cors_reply(Req, ServerID);
        _ ->
            {ok, Body} = read_body(Req),
            handle_request(Req, Body, ServerID)
    end.

%% @doc Handle all non-CORS preflight requests as AO-Core requests.
%%
%% This function processes normal HTTP requests through the AO-Core system:
%% 1. Adding request timing information
%% 2. Retrieving server configuration options
%% 3. Handling root path redirects to default dashboard
%% 4. Parsing HTTP requests into HyperBEAM message format
%% 5. Invoking the meta@1.0 device for request processing
%% 6. Converting responses back to HTTP format
%%
%% @param RawReq Raw Cowboy request object
%% @param Body HTTP request body as binary
%% @param ServerID Server identifier for configuration lookup
%% @returns {ok, UpdatedReq, State}
handle_request(RawReq, Body, ServerID) ->
    % Insert the start time into the request so that it can be used by the
    % `hb_http' module to calculate the duration of the request.
    StartTime = os:system_time(millisecond),
    Req = RawReq#{ start_time => StartTime },
    NodeMsg = get_opts(#{ http_server => ServerID }),
    put(server_id, ServerID),
    case {cowboy_req:path(RawReq), cowboy_req:qs(RawReq)} of
        {<<"/">>, <<>>} ->
            % If the request is for the root path, serve a 
            % redirect to the default request of the node.
            Req2 = cowboy_req:reply(
                302,
                #{
                    <<"location">> =>
                        hb_opts:get(
                            default_request,
                            ?DEFAULT_DASHBOARD_PATH,
                            NodeMsg
                        )
                },
                RawReq
            ),
            {ok, Req2, no_state};
        _ ->
            % The request is of normal AO-Core form, so we parse it and invoke
            % the meta@1.0 device to handle it.
            ?event(http,
                {
                    http_inbound,
                    {cowboy_req, {explicit, Req}, {body, {string, Body}}}
                }
            ),
            % TracePID = hb_tracer:start_trace(),
            % Parse the HTTP request into HyerBEAM's message format.
            ReqSingleton =
                try hb_http:req_to_tabm_singleton(Req, Body, NodeMsg)
                catch ParseError:ParseDetails:ParseStacktrace ->
                    {parse_error, ParseError, ParseDetails, ParseStacktrace}
                end,
            try 
                case ReqSingleton of
                    {parse_error, PType, PDetails, PStacktrace} ->
                        erlang:raise(PType, PDetails, PStacktrace);
                    _ ->
                        ok
                end,
                CommitmentCodec = 
                    hb_http:accept_to_codec(ReqSingleton, NodeMsg),
                ?event(http,
                    {parsed_singleton,
                        {req_singleton, ReqSingleton},
                        {accept_codec, CommitmentCodec}}
                    % #{trace => TracePID}
                ),
                % hb_tracer:record_step(TracePID, request_parsing),
                % Invoke the meta@1.0 device to handle the request.
                {ok, Res} =
                    dev_meta:handle(
                        NodeMsg#{
                            commitment_device => CommitmentCodec
                            % trace => TracePID
                        },
                        ReqSingleton
                    ),
                hb_http:reply(Req, ReqSingleton, Res, NodeMsg)
            catch
                Type:Details:Stacktrace ->
                    handle_error(
                        Req,
                        ReqSingleton,
                        Type,
                        Details,
                        Stacktrace,
                        NodeMsg
                    )
            end
    end.

%% @doc Read the complete body of an HTTP request.
%%
%% This function handles reading HTTP request bodies that may be sent
%% in chunks. It accumulates all chunks into a single binary for
%% processing by the request handler.
%%
%% @param Req Cowboy request object
%% @returns {ok, Body} where Body is the complete request body
read_body(Req) -> read_body(Req, <<>>).

%% @doc Read HTTP request body with accumulator for chunked data.
%%
%% This is the internal implementation that handles chunked request
%% bodies by recursively reading chunks and accumulating them into
%% a single binary.
%%
%% @param Req0 Cowboy request object
%% @param Acc Accumulator binary for body chunks
%% @returns {ok, CompleteBody}
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _Req} -> {ok, << Acc/binary, Data/binary >>};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

%% @doc Reply to CORS preflight requests.
%%
%% This function handles HTTP OPTIONS requests for CORS (Cross-Origin
%% Resource Sharing) preflight checks. It returns appropriate CORS
%% headers allowing cross-origin requests from any domain with any
%% headers and standard HTTP methods.
%%
%% @param Req Cowboy request object
%% @param _ServerID Server identifier (unused)
%% @returns {ok, UpdatedReq, State}
cors_reply(Req, _ServerID) ->
    Req2 = cowboy_req:reply(204, #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"*">>,
        <<"access-control-allow-methods">> =>
            <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>
    }, Req),
    ?event(http_debug, {cors_reply, {req, Req}, {req2, Req2}}),
    {ok, Req2, no_state}.

%% @doc Return a 500 error response to the client.
%%
%% This function handles internal server errors by:
%% 1. Formatting error details and stacktrace for logging
%% 2. Creating a structured error message
%% 3. Logging the error with appropriate formatting
%% 4. Removing noise from stacktrace and details
%% 5. Sending the error response to the client
%%
%% @param Req Cowboy request object
%% @param Singleton Request singleton for response formatting
%% @param Type Error type
%% @param Details Error details
%% @param Stacktrace Error stacktrace
%% @param NodeMsg Node configuration for formatting
%% @returns {ok, UpdatedReq, State}
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
    FormattedErrorMsg =
        ErrorMsg#{
            <<"stacktrace">> => 
                hb_util:bin(hb_format:remove_noise(StacktraceStr)),
            <<"details">> => 
                hb_util:bin(hb_format:remove_noise(DetailsStr))
        },
    hb_http:reply(Req, Singleton, FormattedErrorMsg, NodeMsg).

%% @doc Return the list of allowed HTTP methods for the server.
%%
%% This function specifies which HTTP methods are supported by the
%% HyperBEAM HTTP server. It's used by Cowboy for method validation
%% and CORS preflight responses.
%%
%% @param Req Cowboy request object
%% @param State Handler state
%% @returns {MethodList, Req, State} where MethodList contains allowed methods
allowed_methods(Req, State) ->
    {
        [
            <<"GET">>, <<"POST">>, <<"PUT">>, 
            <<"DELETE">>, <<"OPTIONS">>, <<"PATCH">>
        ],
        Req,
        State
    }.

%%% ===================================================================
%%% HTTPS & Redirect Functions
%%% ===================================================================

%% @doc Set up HTTP to HTTPS redirect on the original server.
%%
%% This function modifies an existing HTTP server's dispatcher to redirect
%% all incoming traffic to the HTTPS equivalent. It:
%% 1. Creates a new Cowboy dispatcher with redirect handlers
%% 2. Updates the server's environment with the new dispatcher
%% 3. Logs the redirect configuration for debugging
%%
%% @param ServerID HTTP server identifier to configure for redirect
%% @param Opts Configuration options containing HTTPS port information
%% @param HttpsPort HTTPS port number for the server
%% @returns ok
setup_http_redirect(ServerID, Opts, HttpsPort) ->
    ?event(https, {setting_up_http_redirect, {server_id, ServerID}}),
    % Create a new dispatcher that redirects everything to HTTPS
    % We use a special redirect handler that will be handled by init/2
    RedirectDispatcher = cowboy_router:compile([
        {'_', [
            {'_', ?MODULE, {redirect_https, Opts, HttpsPort}}
        ]}
    ]),
    % Update the server's dispatcher
    cowboy:set_env(ServerID, dispatch, RedirectDispatcher),
    ?event(https, {http_redirect_configured, {server_id, ServerID}}).

%% @doc HTTP to HTTPS redirect handler.
%%
%% This handler processes HTTP requests and sends 301 Moved Permanently
%% responses to redirect clients to HTTPS. It:
%% 1. Extracts host, path, and query string from the request
%% 2. Determines the appropriate HTTPS port from configuration
%% 3. Constructs the HTTPS URL preserving path and query parameters
%% 4. Sends a 301 redirect with CORS headers
%%
%% @param Req0 Cowboy request object
%% @param State Handler state containing server options
%% @param HttpsPort HTTPS port number for the server
%% @returns {ok, UpdatedReq, State}
redirect_to_https(Req0, State, HttpsPort) ->
    Host = cowboy_req:host(Req0),
    Path = cowboy_req:path(Req0),
    Qs = cowboy_req:qs(Req0),
    % Get HTTPS port from state, default to 443
    % Build the HTTPS URL with port if not standard HTTPS port
    BaseUrl = case HttpsPort of
        443 -> <<"https://", Host/binary>>;
        _ -> 
            PortBin = integer_to_binary(HttpsPort),
            <<"https://", Host/binary, ":", PortBin/binary>>
    end,
    Location = case Qs of
        <<>> -> 
            <<BaseUrl/binary, Path/binary>>;
        _ -> 
            <<BaseUrl/binary, Path/binary, "?", Qs/binary>>
    end,
    ?event(
        https, 
        {
            redirecting_to_https, 
            {from, Path}, 
            {to, Location}, 
            {https_port, HttpsPort}
        }
    ),
    % Send 301 redirect
    Req = cowboy_req:reply(301, #{
        <<"location">> => Location,
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-headers">> => <<"*">>,
        <<"access-control-allow-methods">> => 
            <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>
    }, Req0),
    {ok, Req, State}.

%%% ===================================================================
%%% Configuration & State Management
%%% ===================================================================

%% @doc Set server options by updating Cowboy environment.
%%
%% This function updates the server's runtime configuration by setting
%% the 'node_msg' environment variable in the Cowboy listener. It's used
%% to dynamically update server behavior without restarting.
%%
%% @param Opts Options map containing http_server reference and new settings
%% @returns ok
set_opts(Opts) ->
    case hb_opts:get(http_server, no_server_ref, Opts) of
        no_server_ref ->
            ok;
        ServerRef ->
            ok = cowboy:set_env(ServerRef, node_msg, Opts)
    end.

%% @doc Merge request with server options and update node history.
%%
%% This function performs advanced options merging by:
%% 1. Preparing and normalizing both request and server options
%% 2. Merging uncommitted request values with server configuration
%% 3. Updating the node history with the new request
%% 4. Preserving the http_server reference for future updates
%% 5. Updating the live server configuration
%%
%% @param Request Request message with new configuration values
%% @param Opts Current server options
%% @returns {ok, MergedOpts} where MergedOpts contains the updated configuration
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
            ++ [ 
                hb_private:reset(
                    maps:without([node_history], PreparedRequest)
                )
            ],
    FinalOpts = MergedOpts#{
        http_server => hb_opts:get(http_server, no_server, Opts),
        node_history => History
    },
    {set_opts(FinalOpts), FinalOpts}.

%% @doc Get server options for the current process.
%%
%% This function retrieves the current server configuration for the
%% calling process by looking up the server ID from the process
%% dictionary and fetching the associated node message.
%%
%% @returns Server options map or no_node_msg if not found
get_opts() ->
    get_opts(#{ http_server => get(server_id) }).
%% @doc Get server options for a specific server.
%%
%% This function retrieves the server configuration for a specific
%% server by extracting the server reference and fetching the
%% 'node_msg' environment variable from Cowboy.
%%
%% @param NodeMsg Node message containing server reference
%% @returns Server options map or no_node_msg if not found
get_opts(NodeMsg) ->
    ServerRef = hb_opts:get(http_server, no_server_ref, NodeMsg),
    cowboy:get_env(ServerRef, node_msg, no_node_msg).

%% @doc Initialize the server ID for the current process.
%%
%% This function stores the server identifier in the process dictionary
%% so that other functions can retrieve server-specific configuration
%% without explicitly passing the server ID.
%%
%% @param ServerID Server identifier to store
%% @returns ok
set_proc_server_id(ServerID) ->
    put(server_id, ServerID).

%% @doc Apply default configuration to the provided options.
%%
%% This function enhances the provided options with system defaults:
%% 1. Generating a random port if none provided
%% 2. Creating a new wallet if none provided
%% 3. Setting up default store configuration
%% 4. Adding derived values like address and force_signed flag
%%
%% @param Opts Base options map to enhance with defaults
%% @returns Enhanced options map with all required defaults
set_default_opts(Opts) ->
    % Create a temporary opts map that does not include the defaults.
    TempOpts = Opts#{ only => local },
    % Generate a random port number between 10000 and 30000 to use
    % for the server.
    Port =
        case hb_opts:get(port, no_port, TempOpts) of
            no_port ->
                rand:seed(exsplus, erlang:system_time(microsecond)),
                ?RANDOM_PORT_MIN + rand:uniform(?RANDOM_PORT_RANGE);
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
                hb_store:start(Stores = [hb_test_utils:test_store()]),
                Stores;
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

%%% ===================================================================
%%% UI & Display Functions
%%% ===================================================================

%% @doc Conditionally print the startup greeter message.
%%
%% This function displays the HyperBEAM startup banner and configuration
%% information, but only when not running in test mode. It provides
%% visual feedback about successful server startup and configuration.
%%
%% @param MergedConfig Complete server configuration
%% @param PrivWallet Private wallet for operator address display
%% @returns ok
print_greeter_if_not_test(MergedConfig, PrivWallet) ->
    case hb_features:test() of
        false ->
            print_greeter(MergedConfig, PrivWallet);
        true ->
            ok
    end.

%% @doc Print the HyperBEAM startup banner and configuration.
%%
%% This function displays a detailed startup message including:
%% 1. ASCII art HyperBEAM logo
%% 2. Version information
%% 3. Server URL for access
%% 4. Operator wallet address
%% 5. Complete configuration details
%%
%% The output provides comprehensive information about the running
%% server instance for debugging and verification.
%%
%% @param Config Server configuration map
%% @param PrivWallet Private wallet for operator identification
%% @returns ok
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
                            hb_opts:get(port, ?DEFAULT_HTTP_PORT, Config)
                        ]
                    )
                ),
                35, leading, $ 
            ),
            hb_util:human_id(ar_wallet:to_address(PrivWallet)),
            FormattedConfig
        ]
    ).

%%% ===================================================================
%%% Shared Server Utilities
%%% ===================================================================

%% @doc Start all required applications for HyperBEAM servers.
%%
%% This function ensures all necessary Erlang applications are started
%% for both HTTP and HTTPS servers. The applications include:
%% 1. Core Erlang applications (kernel, stdlib)
%% 2. Network applications (inets, ssl)
%% 3. HTTP server applications (ranch, cowboy)
%% 4. HTTP client applications (gun)
%% 5. System monitoring (os_mon)
%%
%% @returns ok or {error, Reason}
start_required_applications() ->
    application:ensure_all_started([
        kernel,
        stdlib,
        inets,
        ssl,
        ranch,
        cowboy,
        gun,
        os_mon
    ]).

%% @doc Generate unique server ID from wallet address.
%%
%% This function creates a unique server identifier by:
%% 1. Extracting the private wallet from node configuration
%% 2. Converting the wallet to an Arweave address
%% 3. Creating a human-readable ID from the address
%%
%% The resulting ID is used for Cowboy listener registration and
%% server identification throughout the system.
%%
%% @param NodeMsg Node configuration containing wallet information
%% @returns ServerID binary for use as Cowboy listener name
generate_server_id(NodeMsg) ->
    hb_util:human_id(
        ar_wallet:to_address(
            hb_opts:get(priv_wallet, no_wallet, NodeMsg)
        )
    ).

%% @doc Create base protocol options for Cowboy servers.
%%
%% This function creates the standard protocol options used by both
%% HTTP and HTTPS servers. It configures:
%% 1. Cowboy dispatcher with the server module and ID
%% 2. Environment variables including node message
%% 3. Stream handlers for request processing
%% 4. Connection limits and timeout settings
%%
%% @param ServerID Server identifier for the dispatcher
%% @param NodeMsg Node configuration message
%% @returns Protocol options map for Cowboy listener
create_base_protocol_opts(ServerID, NodeMsg) ->
    NodeMsgWithID = hb_maps:put(http_server, ServerID, NodeMsg),
    Dispatcher = cowboy_router:compile([{'_', [{'_', ?MODULE, ServerID}]}]),
    #{
        env => #{dispatch => Dispatcher, node_msg => NodeMsgWithID},
        stream_handlers => [cowboy_stream_h],
        max_connections => infinity,
        idle_timeout => hb_opts:get(idle_timeout, ?DEFAULT_IDLE_TIMEOUT, NodeMsg)
    }.

%% @doc Add Prometheus metrics to protocol options if enabled.
%%
%% This function conditionally enhances protocol options with Prometheus
%% metrics collection. It:
%% 1. Checks if Prometheus is enabled in configuration
%% 2. Starts Prometheus applications if needed
%% 3. Adds metrics callback and enhanced stream handlers
%% 4. Handles graceful fallback if Prometheus is unavailable
%%
%% @param ProtoOpts Base protocol options to enhance
%% @param NodeMsg Node configuration message
%% @returns Enhanced protocol options with optional Prometheus support
add_prometheus_if_enabled(ProtoOpts, NodeMsg) ->
    case hb_opts:get(prometheus, not hb_features:test(), NodeMsg) of
        true ->
            ?event(prometheus,
                {starting_prometheus, {test_mode, hb_features:test()}}
            ),
            try
                application:ensure_all_started([prometheus, prometheus_cowboy]),
                ProtoOpts#{
                    metrics_callback => 
                        fun prometheus_cowboy2_instrumenter:observe/1,
                    stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
                }
            catch
                Type:Reason ->
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
    end.

%% @doc Process server startup hooks for configuration modification.
%%
%% This function executes the startup hook system, allowing external
%% devices and modules to modify server configuration before startup.
%% It:
%% 1. Wraps options in the expected hook message format
%% 2. Calls the startup hook with the configuration
%% 3. Extracts the modified configuration from the hook response
%% 4. Handles hook execution errors with appropriate logging
%%
%% @param Opts Initial server options to process through hooks
%% @returns {ok, ModifiedNodeMsg} or throws {failed_to_start_server, Reason}
process_server_hooks(Opts) ->
    HookMsg = #{ <<"body">> => Opts },
    case dev_hook:on(<<"start">>, HookMsg, Opts) of
        {ok, #{ <<"body">> := NodeMsgAfterHook }} -> 
            {ok, NodeMsgAfterHook};
        Unexpected ->
            ?event(server,
                {failed_to_start_server,
                    {unexpected_hook_result, Unexpected}
                }
            ),
            throw(
                {failed_to_start_server,
                    {unexpected_hook_result, Unexpected}
                }
            )
    end.

%%% ===================================================================
%%% HTTPS Server Helper Functions
%%% ===================================================================

%% @doc Create HTTPS server IDs from node configuration.
%%
%% This function generates unique server identifiers for HTTPS servers:
%% 1. Initializes the HTTP module for request handling
%% 2. Generates the base server ID using the shared utility
%% 3. Creates the HTTPS-specific server ID by appending '_https'
%%
%% The HTTPS server ID is used for Cowboy listener registration and
%% must be unique from the HTTP server ID.
%%
%% @param NodeMsg Node configuration message containing wallet
%% @returns {ServerID, HttpsServerID} tuple for server identification
create_https_server_id(NodeMsg) ->
    % Initialize HTTP module
    hb_http:start(),
    % Create server ID using shared utility
    ServerID = generate_server_id(NodeMsg),
    HttpsServerID = <<ServerID/binary, "_https">>,
    {ServerID, HttpsServerID}.

%% @doc Create HTTPS dispatcher and protocol options.
%%
%% This function sets up the Cowboy dispatcher and protocol options
%% for HTTPS servers by leveraging the shared utility functions.
%% It:
%% 1. Creates base protocol options using the shared utility
%% 2. Extracts the dispatcher for return compatibility
%% 3. Ensures consistent configuration between HTTP and HTTPS
%%
%% @param HttpsServerID Unique HTTPS server identifier
%% @param NodeMsg Node configuration message
%% @returns {Dispatcher, ProtoOpts} tuple for Cowboy configuration
create_https_dispatcher(HttpsServerID, NodeMsg) ->
    % Use shared utility for protocol options
    ProtoOpts = create_base_protocol_opts(HttpsServerID, NodeMsg),
    % Extract dispatcher for return (though not used in current flow)
    #{env := #{dispatch := Dispatcher}} = ProtoOpts,
    {Dispatcher, ProtoOpts}.

%% @doc Start TLS listener for HTTPS server.
%%
%% This function starts the actual Cowboy TLS listener with the
%% provided certificate files and protocol options. It handles
%% the low-level server startup.
%%
%% @param HttpsServerID Unique HTTPS server identifier
%% @param HttpsPort Port number for HTTPS server
%% @param CertFile Path to certificate PEM file
%% @param KeyFile Path to private key PEM file
%% @param ProtoOpts Protocol options for Cowboy
%% @returns {ok, Listener} or {error, Reason}
start_tls_listener(HttpsServerID, HttpsPort, CertFile, KeyFile, ProtoOpts) ->
    ?event(
        https, 
        {
            starting_tls_listener, 
            {server_id, HttpsServerID}, 
            {port, HttpsPort}, 
            {cert_file, CertFile}, 
            {key_file, KeyFile}
        }
    ),
    case cowboy:start_tls(
        HttpsServerID,
        [
            {port, HttpsPort},
            {certfile, CertFile},
            {keyfile, KeyFile}
        ],
        ProtoOpts
    ) of
        {ok, Listener} ->
            ?event(
                https, 
                {
                    https_server_started, 
                    {listener, Listener}, 
                    {server_id, HttpsServerID}, 
                    {port, HttpsPort}
                }
            ),
            {ok, Listener};
        {error, Reason} ->
            ?event(https, {tls_listener_start_failed, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Set up HTTP to HTTPS redirect if needed.
%%
%% This function conditionally configures an existing HTTP server
%% to redirect all traffic to HTTPS. It:
%% 1. Validates the redirect target server ID
%% 2. Configures HTTP server redirect if target is valid
%% 3. Logs redirect setup or skipping with reasons
%% 4. Handles invalid server IDs gracefully
%%
%% The redirect setup allows seamless HTTP to HTTPS migration.
%%
%% @param RedirectTo HTTP server ID to configure (or no_server to skip)
%% @param NodeMsg Node configuration message with HTTPS port
%% @param HttpsPort HTTPS port number for redirect URL construction
%% @returns ok
setup_redirect_if_needed(RedirectTo, NodeMsg, HttpsPort) ->
    ?event(
        https, 
        {
            checking_for_http_server_to_redirect, 
            {original_server_id, RedirectTo}
        }
    ),
    case RedirectTo of
        no_server ->
            ?event(https, {no_original_server_to_redirect}),
            ok;
        _ when is_binary(RedirectTo) ->
            ?event(
                https, 
                {
                    setting_up_redirect_from_http_to_https, 
                    {http_server, RedirectTo},
                    {https_port, HttpsPort}
                }
            ),
            setup_http_redirect(RedirectTo, NodeMsg, HttpsPort);
        _ ->
            ?event(https, {invalid_redirect_server_id, RedirectTo}),
            ok
    end.

%%% ===================================================================
%%% Tests
%%% ===================================================================

%% @doc Test server startup hook functionality.
%%
%% This test verifies that the startup hook system works correctly by:
%% 1. Creating a test device with a startup hook
%% 2. Starting a node with the hook configuration
%% 3. Verifying that the hook modified the server options
%% 4. Confirming the modified options are accessible via the API
%%
%% @returns ok (test assertion)
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

%% @doc Test the set_opts/2 function for options merging and history.
%%
%% This test validates the options merging functionality by:
%% 1. Starting a test node with a known wallet
%% 2. Testing empty node history initialization
%% 3. Testing single request option merging
%% 4. Testing multiple request history accumulation
%% 5. Verifying node history growth and option persistence
%%
%% @returns ok (test assertions)
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
    {ok, UpdatedOpts3} = 
        set_opts(#{}, UpdatedOpts2#{ <<"hello3">> => <<"world3">> }),
    NodeHistory3 = hb_opts:get(node_history, not_found, UpdatedOpts3),
    Key3 = hb_opts:get(<<"hello3">>, not_found, UpdatedOpts3),
    ?event(debug_node_history, {node_history_length, length(NodeHistory3)}),
    ?assert(length(NodeHistory3) == 3),
    ?assert(Key3 == <<"world3">>).

%% @doc Test server restart functionality.
%%
%% This test verifies that servers can be restarted with updated
%% configuration by:
%% 1. Starting a server with initial configuration
%% 2. Starting a second server with the same wallet but different config
%% 3. Verifying that the second server has the updated configuration
%% 4. Confirming that server restart preserves functionality
%%
%% @returns ok (test assertion)
restart_server_test() ->
    % We force HTTP2, overriding the HTTP3 feature, 
    % because HTTP3 restarts don't work yet.
    Wallet = ar_wallet:new(),
    BaseOpts = #{
        <<"test-key">> => <<"server-1">>,
        priv_wallet => Wallet,
        protocol => http2
    },
    _ = start_node(BaseOpts),
    N2 = start_node(BaseOpts#{ <<"test-key">> => <<"server-2">> }),
    ?assertEqual(
        {ok, <<"server-2">>},
        hb_http:get(N2, <<"/~meta@1.0/info/test-key">>, #{})
    ).