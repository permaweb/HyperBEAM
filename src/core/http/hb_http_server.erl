%%% @doc A router that attaches a HTTP server to the AO-Core resolver.
%%% Because AO-Core is built to speak in HTTP semantics, this module
%%% only has to marshal the HTTP request into a message, and then
%%% pass it to the AO-Core resolver. 
%%% 
%%% `req_to_tabm_singleton/3' converts the HTTP request into a message,
%%% `resolve_request/3' resolves it, and `reply/4' responds to the client,
%%% converting the result back into an HTTP response.
%%%
%%% We parse a request in five steps:
%%%
%%% 1. We merge the query parameters of the request over its headers. A query
%%%    parameter does not replace a header that the request's `signature-input'
%%%    names, as the signature covers the value of that header.
%%% 2. We find the codec of the request. A request with a `signature-input'
%%%    header is in `httpsig@1.0' (RFC 9421), whatever its `content-type'.
%%%    Otherwise, a `content-type' that gives a codec gives that codec (see
%%%    `hb_http'): `application/x.ao.ans104' and `application/ans104' give
%%%    `ans104@1.0', and `application/json' gives `json@1.0'. A `content-type'
%%%    that gives a device that is not a codec is refused with a 415. Any other
%%%    `content-type', or none, gives `httpsig@1.0'. Nothing else selects the
%%%    codec.
%%% 3. We decode the request with its codec. An `httpsig@1.0' request is
%%%    decoded from its headers and body. A request in any other codec is
%%%    decoded from its body alone: the codec deserializes it. If the request
%%%    has commitments, we keep only the keys that they cover. We then remove
%%%    the commitments of an unsigned request, as `hb_ao' does for every
%%%    execution input.
%%% 4. We add the details of the HTTP request to the message. The `method' and
%%%    `host' keys are always set from the HTTP request. The `path', `accept'
%%%    and `accept-bundle' keys are only added if the message does not have
%%%    them. The `path' is taken from a `path' header, or from the request
%%%    target if there is no such header. The `accept' and `accept-bundle'
%%%    keys are taken from the query or the headers, and default to `*/*' and
%%%    `false'. The cookies of the `cookie' header are placed in `priv/cookie',
%%%    and the caller's address in `priv/ip'. An `ao-peer-port' key is
%%%    replaced by an `ao-peer' key with the caller's address and that
%%%    port. We remove the `content-length' key and an empty `body', unless
%%%    they are signed.
%%% 5. We verify every commitment of the request, and of the messages that it
%%%    holds. If the node has `force-signed-requests' set, the request must
%%%    also have a signer. If either check fails, the node replies with a 400.
%%%
%%% `hb_singleton' then parses the `path' of the request into the messages to
%%% resolve. When the path is the request target, its query parameters are
%%% added to the request, replacing any keys of the same name, signed or not.
%%% Each of the messages keeps the commitments of the request. As a result, a
%%% device that authorizes a request by its signer must only read the keys
%%% that the signer committed, after verifying them. `lib_meta:authorized/3'
%%% returns these keys.
%%%
%%% We reply to the client with the result as follows:
%%%
%%% 1. The HTTP status of the reply is the result's `status' key, if it is an
%%%    integer from 100 to 599, or the binary encoding of one. Otherwise, it is
%%%    the code that `hb_http:status_code/2' gives for the result's Erlang
%%%    status. We do not add a `status' key to the reply.
%%% 2. If the result is a literal or a list, we send it as the `body' of a
%%%    message, with `ao-result' set to `body'.
%%% 3. We find the codec of the reply from the request's `accept', read as RFC
%%%    9110 gives: its media ranges in order of preference, by q-value and then
%%%    by their order. The first range that the node can serve gives the codec.
%%%    A range of the result's own `content-type' gives `httpsig@1.0', which
%%%    sends the result's body unchanged. A range that gives a codec gives that
%%%    codec. We pass over any other range, such as `*/*' or `text/html'. If no
%%%    range can be served, or the request has no valid `accept', the reply is
%%%    in `httpsig@1.0'. For example, `application/json, text/plain, */*' gives
%%%    `json@1.0' for a computed result, and the document itself for a stored
%%%    JSON document.
%%% 4. If the reply is in `httpsig@1.0' and the result has no commitments, we
%%%    send it with its unsigned commitment over its own keys, so that the
%%%    client can tell them from the headers that the reply adds. If the node
%%%    has `force-signed' set, we also sign the `hashpath' of the result,
%%%    committing only that key. We do this only if the message that we send is
%%%    the result of its hashpath, and the message has no `hashpath' key of its
%%%    own. We sign the `hashpath' in the same way for a request whose
%%%    `accept-signature' (RFC 9421) asks for it: a member with the inner list
%%%    `("hashpath")' and the URI of the node's committer as its `keyid'. The
%%%    field is a preference: we ignore any other member.
%%% 5. A reply in `httpsig@1.0' sends the keys of the result as its headers and
%%%    body, and the result's own `content-type' as its `content-type', so a
%%%    stored page or image is served as itself. A `content-type' of the result
%%%    of the form `application/x.ao.<name>' is sent in a body part instead. If
%%%    the request does not have `accept-bundle: true', we add the node's index
%%%    page to the reply as its `body' and `content-type'. We only do this if
%%%    the message has neither key, as a value or as a link.
%%% 6. A reply in any other codec sends the result as its body, encoded by the
%%%    codec, with the type and subtype of the range as its `content-type': a
%%%    request for `application/json' gets `application/json'. The codec
%%%    carries the commitments in its own form: `ans104@1.0' and `tx@1.0' carry
%%%    their own signature, and `json@1.0' and `flat@1.0' carry every
%%%    commitment in the message's `commitments' key.
%%% 
%%% The router uses an `Opts' message as its Cowboy initial state, 
%%% such that changing it on start of the router server allows for
%%% the execution parameters of all downstream requests to be controlled.
%%%
%%% A node with `http-proxy-protocol' set to `true' requires each connection to
%%% start with a PROXY protocol header, and takes the caller's address from it.
%%% Only a node that is reached through a trusted proxy should set it. Without
%%% it, the caller's address is the address of the socket's peer. A node behind
%%% an HTTP proxy, which sends the caller's address only as a header, lists the
%%% proxy's address in `http-trusted-proxies': a request from a listed address
%%% takes the caller's address from its `x-real-ip' header.
-module(hb_http_server).
-export([start/0, start/1, start_application/0, allowed_methods/2, init/2]).
-export([set_opts/1, set_opts/2, get_opts/0, get_opts/1, with_node_committer/1]).
-export([update_opts/2, set_default_opts/1, set_proc_server_id/1]).
-export([static/3, static/4]).
-export([start_node/0, start_node/1]).
-export([resolve_request/3, error_reply/4, commit_reply/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(DEFAULT_FILTER_KEYS, [<<"content-length">>]).
%% The HTTP methods that keep their own Prometheus label value.
-define(METRIC_METHODS, [
    <<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>,
    <<"DELETE">>, <<"OPTIONS">>, <<"PATCH">>
]).
%% Define the max size we can return in 500 error details field.
-define(DEFAULT_ERROR_DETAILS_MAX_SIZE, 32*1024).
%% At most this many media ranges in `accept' are considered.
-define(MAX_ACCEPT_RANGES, 100).

%%% HTTP server start and management functions.

%% @doc Starts the HTTP server. Optionally accepts an `Opts' message, which
%% is used as the source for server configuration settings, as well as the
%% `Opts' argument to use for all AO-Core resolution requests downstream.
start() ->
    {ok, Listener, _ServerID} = start_application(),
    {ok, Listener}.

%% @doc Start the application HTTP server and return its listener ID.
start_application() ->
    ?event(http, {start_store, <<"cache-mainnet/fs">>}),
    EnvConfig = hb_opts:default_message_with_env(),
    Loc = hb_opts:get(hb_config_location, <<"config.flat">>, EnvConfig),
    HBConfig = os:getenv("HB_CONFIG"),
    Loaded =
        case hb_opts:load(Loc, EnvConfig) of
            {ok, Conf} ->
                ?event(boot, {loaded_config, {path, Loc}, {config, Conf}}),
                Conf;
            % No `HB_CONFIG' and no `config.flat': the node uses its defaults.
            {error, enoent} when HBConfig =:= false -> #{};
            {error, Reason} ->
                throw({failed_to_load_config, Loc, Reason})
        end,
    MergedConfig = hb_maps:merge(EnvConfig, Loaded),
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
                <<"hyperbeam-key.json">>,
                Loaded
            )
        ),
    maybe_greeter(Loaded, PrivWallet),
    {ok, Listener} = start(
        Loaded#{
            <<"priv-wallet">> => PrivWallet,
            <<"store">> => UpdatedStoreOpts,
            <<"port">> => hb_opts:get(port, 8734, Loaded)
        }
    ),
    ServerID = hb_util:human_id(ar_wallet:to_address(PrivWallet)),
    {ok, Listener, ServerID}.
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
    ok = hb_process_sampler:ensure_started(BaseOpts),
    ok = hb_system_monitor:ensure_started(BaseOpts),
    {ok, Listener, _Port} = new_server(BaseOpts),
    {ok, Listener}.

%% @doc Print the greeter message to the console if we are not running tests.
maybe_greeter(MergedConfig, PrivWallet) ->
    case hb_features:test() of
        false ->
            print_greeter(MergedConfig, PrivWallet);
        true ->
            ok
    end.

%% @doc Read a static file from a device's HTML asset directory.
static(Device, Name, Opts) ->
    static(Device, Name, #{}, Opts).
static(Device, Name, Template, _Opts) ->
    Base = hb_util:bin(code:priv_dir(hb)),
    Filename = <<Base/binary, "/html/", Device/binary, "/", Name/binary>>,
    ?event({serving_static, Filename}),
    case file:read_file(Filename) of
        {ok, RawBody} ->
            Body = apply_static_template(RawBody, Template),
            {ok, #{
                <<"body">> => Body,
                <<"content-type">> => content_type(Filename)
            }};
        {error, _} ->
            {error, not_found}
    end.

%% @doc Return the content type for a static file.
content_type(Filename) ->
    case filename:extension(Filename) of
        <<".html">> -> <<"text/html">>;
        <<".js">> -> <<"text/javascript">>;
        <<".css">> -> <<"text/css">>;
        <<".png">> -> <<"image/png">>;
        <<".ico">> -> <<"image/x-icon">>;
        <<".ttf">> -> <<"font/ttf">>;
        <<".json">> -> <<"application/json">>;
        _ -> <<"text/plain">>
    end.

%% @doc Apply a simple binary replacement template to a static file. Values are
%% inserted as HTML text, and a list of `{Term, Description}' pairs as the
%% terms and descriptions of a definition list.
apply_static_template(Body, Template) when is_map(Template) ->
    apply_static_template(Body, maps:to_list(Template));
apply_static_template(Body, []) ->
    Body;
apply_static_template(Body, [{Key, Value} | Rest]) ->
    apply_static_template(
        binary:replace(
            Body,
            <<"{{", Key/binary, "}}">>,
            template_html(Value),
            [global]
        ),
        Rest
    ).

%% @doc Convert a template value to HTML text. A list of `{Term, Description}'
%% pairs gives the terms and descriptions of a definition list. A link gives
%% its URI. Any other value that is not a literal, such as a message, is
%% printed by `hb_format:term/1', which removes the private keys of a message.
template_html([]) ->
    <<>>;
template_html([{_, _} | _] = Pairs) ->
    <<
        <<"<dt>", (template_html(Term))/binary, "</dt>",
            "<dd>", (template_html(Description))/binary, "</dd>">>
    ||
        {Term, Description} <- Pairs
    >>;
template_html({link, URI, _}) ->
    template_html(URI);
template_html(Value) when is_binary(Value); is_number(Value); is_atom(Value) ->
    escape_html(hb_util:bin(Value));
template_html(Value) ->
    escape_html(hb_util:bin(hb_format:term(Value))).

%% @doc Escape the characters that HTML reads as markup.
escape_html(Bin) ->
    << <<(escape_html_char(Char))/binary>> || <<Char>> <= Bin >>.

escape_html_char($&) -> <<"&amp;">>;
escape_html_char($<) -> <<"&lt;">>;
escape_html_char($>) -> <<"&gt;">>;
escape_html_char($") -> <<"&quot;">>;
escape_html_char(Char) -> <<Char>>.

%% @doc Print the greeter message to the console. Includes the version, operator
%% address, URL to access the node, and the wider configuration (including the
%% keys inherited from the default configuration).
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
        "==        ██╔══██╗██╔════╝██╔══██╗████╗ ████║     v~s. ==~n"
        "==        ██████╔╝█████╗  ███████║██╔████╔██║            ==~n"
        "==        ██╔══██╗██╔══╝  ██╔══██║██║╚██╔╝██║ EAT GLASS, ==~n"
        "==        ██████╔╝███████╗██║  ██║██║ ╚═╝ ██║ BUILD THE  ==~n"
        "==        ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝    FUTURE. ==~n"
        "===========================================================~n"
        "== Node live at: ~s ==~n"
        "== Operator: ~s ==~n"
        "===========================================================~n"
        "== Config:                                               ==~n"
        "===========================================================~n"
        "   ~s~n~n"
        "===========================================================~n",
        [
            ?HYPERBEAM_VERSION,
            string:pad(
                lists:flatten(
                    io_lib:format(
                        "~s://~s:~p",
                        [
                            scheme(Config),
                            hb_opts:get(node_host, <<"localhost">>, Config),
                            hb_opts:get(port, 8734, Config)
                        ]
                    )
                ),
                39,
                leading,
                $ % Note: Space after `$` is functional, not garbage.
            ),
            hb_util:human_id(ar_wallet:to_address(PrivWallet)),
            FormattedConfig
        ]
    ).

%% @doc Trigger the creation of a new HTTP server node. Accepts a `NodeMsg'
%% message, which is used to configure the server. This function executed the
%% `start' hook on the node, giving it the opportunity to modify the `NodeMsg'
%% before it is used to configure the server. The `start' hook expects gives and
%% expects the node message to be in the `body' key.
new_server(RawNodeMsg) ->
    RawNodeMsgWithDefaults =
        hb_maps:merge(
            hb_opts:default_message_with_env(),
            RawNodeMsg#{ <<"only">> => local }
        ),
    HookMsg = #{ <<"body">> => RawNodeMsgWithDefaults },
    NodeMsg =
        case hb_hook:on(<<"start">>, HookMsg, RawNodeMsgWithDefaults) of
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
    init_prometheus(),
    Wallet = hb_opts:get(priv_wallet, no_wallet, NodeMsg),
    ServerID = hb_util:human_id(ar_wallet:to_address(Wallet)),
    TLS = hb_tls:config(NodeMsg),
    DefaultProtocol =
        case hb_features:http3() of
            true -> http3;
            false -> http2
        end,
    Protocol = hb_opts:get(protocol, DefaultProtocol, NodeMsg),
    case {Protocol, TLS} of
        {http3, TLSConfig} when is_map(TLSConfig) ->
            error('tls-not-supported-for-http3');
        {Supported, _} when Supported =:= http1; Supported =:= http2;
                Supported =:= http3 -> ok;
        _ -> error({'unknown-protocol', Protocol})
    end,
    % Put server ID into node message so it's possible to update current server
    % params.
    NodeMsgWithID = hb_maps:put(<<"http-server">>, ServerID, NodeMsg),
    ProtoOpts = listener_protocol_options(ServerID, NodeMsgWithID),
    PrometheusOpts =
        case hb_opts:get(prometheus, not hb_features:test(), NodeMsg) of
            true ->
                ?event(prometheus,
                    {starting_prometheus, {test_mode, hb_features:test()}}
                ),
                % Attempt to start the prometheus application, if possible.
                try
                    application:ensure_all_started([prometheus, prometheus_cowboy, prometheus_ranch]),
                    prometheus_registry:register_collectors([hb_metrics_collector]),
                    ProtoOpts#{
                        metrics_callback => fun observe_request_metrics/1,
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
    TLSOpts = prepare_tls(TLS, Wallet, ServerID, NodeMsg),
    {Port, Listener} = try
        case case {Protocol, TLSOpts} of
                {http3, []} -> start_http3(ServerID, PrometheusOpts, NodeMsg);
                {Pro, _} when Pro =:= http2; Pro =:= http1 ->
                    start_http2(ServerID, PrometheusOpts, NodeMsg, TLSOpts)
            end of
            {ok, StartedPort, StartedListener} ->
                {StartedPort, StartedListener};
            {error, ListenerReason} ->
                error({'http-server-start-failed', ListenerReason})
        end
    catch
        Class:StartReason:Stack ->
            case TLS of false -> ok; _ -> stop_tls(ServerID) end,
            erlang:raise(Class, StartReason, Stack)
    end,
    % Update the node message with the actual port that was used, in the event
    % that the OS assigned a different port. This happens, for example, when we
    % use port 0.
    set_opts(NodeMsg#{ <<"port">> => Port }),
    ?event(http,
        {http_server_started,
            {listener, Listener},
            {server_id, ServerID},
            {port, Port},
            {protocol, Protocol},
            {store, hb_opts:get(store, no_store, NodeMsg)}
        }
    ),
    set_proc_server_id(ServerID),
    {ok, Listener, Port}.

prepare_tls(false, _Wallet, ServerID, _NodeMsg) ->
    stop_tls(ServerID),
    [];
prepare_tls(TLS, Wallet, ServerID, NodeMsg) ->
    ACME = hb_maps:get(<<"acme">>, TLS, #{}, NodeMsg),
    stop_tls(ServerID),
    try
        case {
            hb_maps:is_key(<<"certificate-path">>, TLS, NodeMsg),
            hb_maps:get(<<"challenge-type">>, ACME, <<"http-01">>, NodeMsg)
        } of
            {true, _} -> ok;
            {false, <<"http-01">>} ->
                ChallengeRef = {tls_http_01, ServerID},
                ChallengeNode = challenge_node(ACME, ServerID, NodeMsg),
                {ok, _, _} = start_http2(
                    ChallengeRef,
                    listener_protocol_options(ChallengeRef, ChallengeNode),
                    ChallengeNode,
                    []
                );
            {false, <<"dns-01">>} -> ok;
            _ -> error('invalid-acme-challenge-type')
        end,
        PrivateTLS = #{
            <<"server-id">> => ServerID,
            <<"lifecycle-capability">> => make_ref()
        },
        Private = #{ <<"tls">> => PrivateTLS },
        Request = hb_private:set(
            #{ <<"path">> => <<"obtain">> }, Private, NodeMsg
        ),
        ResolveOpts = hb_private:set(NodeMsg, Private, NodeMsg),
        {ok, BootstrapResult} = hb_ao:resolve(
            #{ <<"device">> => <<"tls@1.0">> },
            Request,
            ResolveOpts#{
                <<"only">> => local,
                <<"hashpath">> => ignore,
                <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
            }
        ),
        Chain = hb_maps:get(
            <<"certificate-chain">>, BootstrapResult, not_found, NodeMsg
        ),
        {ok, TLSOpts} = hb_tls:socket_options(Wallet, Chain),
        TLSOpts
    catch
        Class:Reason:Stack ->
            stop_tls(ServerID),
            erlang:raise(Class, Reason, Stack)
    end.

challenge_node(ACME, ServerID, NodeMsg) ->
    ChallengeRef = {tls_http_01, ServerID},
    hb_private:set(
        (hb_private:reset(NodeMsg))#{
            <<"port">> => hb_maps:get(<<"http-port">>, ACME, 80, NodeMsg),
            <<"force-signed">> => false,
            <<"http-proxy-protocol">> => false,
            <<"http-server">> => ChallengeRef,
            <<"on">> => #{
                <<"request">> => #{ <<"device">> => <<"tls@1.0">> }
            }
        },
        #{ <<"tls">> => #{ <<"server-id">> => ServerID } },
        NodeMsg
    ).

listener_protocol_options(ServerID, NodeMsg) ->
    Dispatcher = cowboy_router:compile([{'_', [{'_', ?MODULE, ServerID}]}]),
    #{
        env => #{ dispatch => Dispatcher, node_msg => NodeMsg },
        stream_handlers => [cowboy_stream_h],
        max_connections => infinity,
        max_concurrent_streams =>
            hb_opts:get(max_concurrent_streams, 100, NodeMsg),
        idle_timeout => hb_opts:get(idle_timeout, 300000, NodeMsg),
        proxy_header => hb_opts:get(http_proxy_protocol, false, NodeMsg),
        % `httpsig@1.0' sends a header for each key of a message whose value
        % is at most 4096 bytes. Percent-encoding can triple the length of
        % its name, escaping can double the length of its value, and the
        % `signature-input' header lists every key of each commitment.
        max_headers => 1024,
        max_header_name_length => 4096,
        max_header_value_length => 16384
    }.

stop_tls(ServerID) ->
    case hb_name:lookup({<<"tls@1.0">>, ServerID}) of
        PID when is_pid(PID) ->
            PID ! {stop, self()},
            receive {stopped, PID} -> ok end;
        undefined -> ok
    end,
    cowboy:stop_listener({tls_http_01, ServerID}),
    ok.

start_http3(ServerID, ProtoOpts, NodeMsg) ->
    ?event(http, {start_http3, ServerID}),
    Parent = self(),
    ServerPID =
        spawn(fun() ->
            application:ensure_all_started(quicer),
            {ok, _Listener} =
                cowboy:start_quic(
                    ServerID, 
                    TransOpts = #{
                        socket_opts => [
                            {certfile, "test/test-tls.pem"},
                            {keyfile, "test/test-tls.key"},
                            {port, hb_opts:get(port, 0, NodeMsg)}
                        ]
                    },
                    ProtoOpts
                ),
            ActualPort = ranch:get_port(ServerID),
            ranch_server:set_new_listener_opts(
                ServerID,
                1024,
                ranch:normalize_opts(
                    hb_maps:to_list(TransOpts#{ port => ActualPort })
                ),
                ProtoOpts,
                []
            ),
            ranch_server:set_addr(ServerID, {<<"localhost">>, ActualPort}),
            % Bypass ranch's requirement to have a connection supervisor define
            % to support updating protocol opts.
            % Quicer doesn't use a connection supervisor, so we just spawn one
            % that does nothing.
            ConnSup = spawn(fun() -> http3_conn_sup_loop() end),
            ranch_server:set_connections_sup(ServerID, ConnSup),
            Parent ! {ok, ActualPort},
            receive stop -> stopped end
        end),
    receive {ok, Port} -> {ok, Port, ServerPID}
    after 2000 ->
        {error, {timeout, starting_http3_server, ServerID}}
    end.

http3_conn_sup_loop() ->
    receive
        _ -> 
            % Ignore any other messages
            http3_conn_sup_loop()
    end.

start_http2(ServerID, ProtoOpts, NodeMsg, TLSOpts) ->
    ?event(http, {start_http2, ServerID}),
    MaxConnections = maps:get(<<"max-connections">>, NodeMsg, 10000),
    NumAcceptors =
        maps:get(
            <<"num-acceptors">>,
            NodeMsg,
            erlang:system_info(schedulers) * 4
        ),
    RequestedPort = hb_opts:get(port, 0, NodeMsg),
    TransportOpts = #{
        socket_opts => [{port, RequestedPort} | TLSOpts],
        max_connections => MaxConnections,
        num_acceptors => NumAcceptors
    },
    StartFun = case TLSOpts of [] -> start_clear; _ -> start_tls end,
    StartRes = cowboy:StartFun(ServerID, TransportOpts, ProtoOpts),
    case StartRes of
        {ok, Listener} ->
            ActualPort = ranch:get_port(ServerID),
            ?event(
                debug_router_info,
                {http2_started,
                    {listener, Listener},
                    {requested_port, RequestedPort},
                    {actual_port, ActualPort}
                },
                NodeMsg
            ),
            {ok, ActualPort, Listener};
        {error, {already_started, Listener}} ->
            ?event(http, {http2_already_started, {listener, Listener}}),
            ?event(debug_router_info,
                {restarting,
                    {id, ServerID},
                    {node_msg, NodeMsg}
                }
            ),
            cowboy:set_env(ServerID, node_msg, #{}),
            cowboy:stop_listener(ServerID),
            start_http2(ServerID, ProtoOpts, NodeMsg, TLSOpts);
        {error, Reason} ->
            {error, Reason}
    end.

%%% Request/response lifecycle.

%% @doc Entrypoint for all HTTP requests. Receives the Cowboy request option and
%% the server ID, which can be used to lookup the node message.
init(Req, ServerID) ->
    case {cowboy_req:method(Req), ServerID} of
        {<<"OPTIONS">>, {tls_http_01, _}} ->
            {ok, Body} = read_body(Req),
            handle_request(Req, Body, ServerID);
        {<<"OPTIONS">>, _} -> cors_reply(Req, ServerID);
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
    ?event(debug_http, {cors_reply, {req, Req}, {req2, Req2}}),
    {ok, Req2, no_state}.

%% @doc Handle all non-CORS preflight requests as AO-Core requests. Execution 
%% starts by parsing the HTTP request into HyerBEAM's message format, then
%% resolving it with `resolve_request/3'.
handle_request(RawReq, Body, ServerID) ->
    % Insert the start time into the request so that it can be used by the
    % `hb_http' module to calculate the duration of the request.
    StartTime = os:system_time(millisecond),
    Req = RawReq#{ start_time => StartTime },
    NodeMsg = get_opts(#{ <<"http-server">> => ServerID }),
    put(server_id, ServerID),
    ?event(http,
        {
            http_inbound,
            {cowboy_req, {explicit, Req}, {body, {string, Body}}}
        }
    ),
    % Parse the HTTP request into HyerBEAM's message format.
    try req_to_tabm_singleton(Req, Body, NodeMsg) of
        ReqSingleton ->
            try
                ?event(http,
                    {parsed_singleton,
                        {req_singleton, ReqSingleton}},
                    #{}
                ),
                {Request, Res} =
                    resolve_request(
                        cowboy_req:path(Req),
                        ReqSingleton,
                        NodeMsg
                    ),
                reply(Req, Request, Res, NodeMsg)
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
    catch ParseError:ParseDetails:ParseStacktrace ->
        handle_error(
            Req,
            #{},
            ParseError,
            ParseDetails,
            ParseStacktrace,
            NodeMsg
        )
    end.

%% @doc Resolve a singleton request: parse it, apply the node's `request' hook
%% to its messages, resolve them, and apply the `response' hook to the result.
%% A node that is not initialized resolves only `/~meta@1.0/info', and an
%% empty sequence of messages redirects to the node's default request.
%% Returns the request that the `request' hook returns, with the result, so
%% that the reply is encoded for that request. Before the hook has run, the
%% request is the singleton.
%% `Path' is the path of the request target, as the client sent it. A caller
%% that does not receive the request through Cowboy must decode and verify the
%% singleton before it calls this function.
resolve_request(Path, Singleton, NodeMsg) ->
    maybe
        true ?=
            hb_opts:get(initialized, false, NodeMsg) =/= false orelse
                Path == <<"/~meta@1.0/info">> orelse
                {Singleton,
                    {error, <<"Node must be initialized before use.">>}},
        Msgs =
            hb_hashpath:from(
                hb_private:set(Singleton, <<"http-request">>, Singleton, NodeMsg),
                requests,
                NodeMsg
            ),
        {_, {ok, RequestRes = #{ <<"body">> := PreProcessedMsgs }}} ?=
            {Singleton, resolve_hook(<<"request">>, Singleton, Msgs, NodeMsg)},
        Request = hb_maps:get(<<"request">>, RequestRes, Singleton, NodeMsg),
        true ?=
            PreProcessedMsgs =/= [] orelse
                {Request, {ok,
                    #{
                        <<"status">> => 307,
                        <<"body">> => <<"Redirecting to default request.">>,
                        <<"location">> => hb_opts:get(
                            default_request,
                            <<"/~hyperbuddy@1.0/index">>,
                            NodeMsg
                        )
                    }
                }},
        % Resolve the request message without the node's key material, and
        % with `http_extra_opts' overriding the node defaults.
        HTTPOpts =
            maps:without(
                ?NODE_KEY_OPTS,
                hb_maps:merge(
                    get_opts(NodeMsg),
                    hb_opts:get(http_extra_opts, #{}, NodeMsg),
                    NodeMsg
                )
            ),
        {ErlStatus, Res} =
            hb_ao:resolve_many(
                PreProcessedMsgs,
                HTTPOpts#{ <<"force-message">> => true }
            ),
        % The result keeps its Erlang status unless the `response' hook returns
        % an error.
        {_, {ok, #{ <<"body">> := Body }}} ?=
            {Request,
                resolve_hook(
                    <<"response">>,
                    Request,
                    Res,
                    get_opts(NodeMsg)
                )},
        {Request, {ErlStatus, Body}}
    end.

%% @doc Execute a hook from the node message upon the user's request, and
%% return its result. The invocation of the hook provides a request of the
%% following form:
%% <pre>
%%      /path => request | response
%%      /request => the request singleton
%%      /body => parsed sequence of messages to process | the execution result
%% </pre>
resolve_hook(HookName, InitiatingRequest, Body, NodeMsg) ->
    HookReq =
        #{
            <<"request">> => InitiatingRequest,
            <<"body">> => Body,
            <<"priv">> => #{ <<"hook-caller">> => <<"kernel">> }
        },
    ?event(hook, {resolve_hook, HookName, HookReq}),
    % Hooks resolve caller content. Code that signs for the node gets its keys
    % back through `with_node_committer/1'.
    case hb_hook:on(HookName, HookReq, maps:without(?NODE_KEY_OPTS, NodeMsg)) of
        {ok, HookRes = #{ <<"body">> := ResponseBody }} ->
            ?event(hook,
                {resolve_hook_success,
                    {name, HookName},
                    {response_body, ResponseBody}
                }
            ),
            {ok, HookRes};
        {error, _} = Error ->
            ?event(hook,
                {resolve_hook_error,
                    {name, HookName},
                    {error, Error}
                }
            ),
            Error;
        {failure, _} = Failure -> Failure;
        % A hook that gives the reply, such as `tunnel@1.0', ends the request.
        {reply, _} = Reply -> Reply;
        Other ->
            {error, Other}
    end.

%% @doc Return an error response to the client: a message the node cannot
%% accept is the client's error, anything else is the server's.
handle_error(Req, Singleton, Type, Details, Stacktrace, NodeMsg) ->
    Reply = error_reply(Type, Details, Stacktrace, NodeMsg),
    ?event(
        http_error,
        {returning_error,
            {method, cowboy_req:method(Req)},
            {path, {string, cowboy_req:path(Req)}},
            {error, Reply}
        },
        NodeMsg
    ),
    reply(Req, Singleton, Reply, NodeMsg).

%% @doc Return the status and the message of the reply to an exception: the
%% error's `type', `details' and `stacktrace', with the keys that
%% `parse_error/1' gives.
error_reply(Type, Details, Stacktrace, NodeMsg) ->
    DetailsStr = hb_util:bin(hb_format:message(Details, NodeMsg, 1)),
    StacktraceStr = hb_util:bin(hb_format:trace(Stacktrace)),
    ErrorMsg =
        (parse_error(Details))#{
            <<"type">> => hb_util:bin(hb_format:message(Type)),
            <<"details">> => DetailsStr,
            <<"stacktrace">> => StacktraceStr
        },
    ErrorDetailsMaxSize = hb_opts:get(error_details_max_size, ?DEFAULT_ERROR_DETAILS_MAX_SIZE, NodeMsg),
    % Preserve indentation while removing trailing noise.
    FormattedErrorMsg =
        ErrorMsg#{
            <<"stacktrace">> => hb_util:bin(hb_format:remove_trailing_noise(StacktraceStr)),
            <<"details">> =>
                hb_format:truncate(
                    hb_util:bin(
                        hb_format:remove_trailing_noise(DetailsStr)),
                        ErrorDetailsMaxSize
                )
        },
    {error_status(Type, Details), FormattedErrorMsg}.

%% @doc The status of an error response. A request whose commitments do not
%% verify or that `hb_singleton' cannot parse is refused as the client's error.
%% A request that needs a message the node cannot load is not found.
error_status(throw, {invalid_commitments, _}) -> error;
error_status(throw, {invalid_singleton, _, _}) -> error;
error_status(throw, {necessary_message_not_found, _, _}) -> not_found;
error_status(_Type, _Details) -> failure.

%% @doc Return the keys that the reply to a request that the node cannot parse
%% carries with its error: the error type and offender of a path that
%% `hb_singleton' cannot parse, and the 415 status of a request whose
%% `content-type' gives a device that is not a codec.
parse_error({invalid_singleton, Error, Offender}) ->
    #{ <<"error">> => hb_util:bin(Error), <<"offender">> => Offender };
parse_error({'unsupported-media-type', _ContentType}) ->
    #{ <<"status">> => 415 };
parse_error(_Details) -> #{}.

%% @doc Return the list of allowed methods for the HTTP server.
allowed_methods(Req, State) ->
    {
        [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>, <<"PATCH">>],
        Req,
        State
    }.

%% @doc Merges the provided `Opts' with uncommitted values from `Request',
%% preserves the http-server value, and updates node-history by prepending
%% the `Request'. If a server reference exists, updates the Cowboy environment
%% variable 'node_msg' with the resulting node message.
set_opts(Opts) ->
    case hb_opts:get(http_server, no_server_ref, Opts) of
        no_server_ref ->
            ok;
        ServerRef ->
            % `Opts' from a request contain no key material: keep the node's.
            ok = cowboy:set_env(ServerRef, node_msg, with_node_committer(Opts))
    end.
set_opts(Request, Opts) ->
    PreparedRequest = hb_message:uncommitted(Request),
    case hb_maps:is_key(<<"tls">>, PreparedRequest, Opts) of
        true ->
            {error, <<"TLS configuration cannot be changed at runtime.">>};
        false ->
            MergedOpts = maps:merge(Opts, PreparedRequest),
            ?event(set_opts, {merged_opts, {explicit, MergedOpts}}),
            History = hb_opts:get(node_history, [], Opts) ++ [
                hb_private:reset(
                    maps:without([<<"node-history">>], PreparedRequest)
                )
            ],
            FinalOpts = MergedOpts#{
                <<"http-server">> => hb_opts:get(http_server, no_server, Opts),
                <<"node-history">> => History
            },
            {set_opts(FinalOpts), FinalOpts}
    end.

%% @doc Get the node message for the current process.
get_opts() ->
    get_opts(#{ <<"http-server">> => get(server_id) }).
get_opts(NodeMsg) ->
    case hb_opts:get(http_server, no_server_ref, NodeMsg) of
        no_server_ref -> NodeMsg;
        ServerRef -> cowboy:get_env(ServerRef, node_msg, no_node_msg)
    end.

%% @doc Call `Fun' with the node message of the server that `Opts' give, and
%% return its result. Calls for one server run one at a time, so a `Fun' that
%% sets the node message does not drop the change of another call. A `Fun'
%% that calls `update_opts/2' for its own server waits for itself.
update_opts(Fun, Opts) ->
    hb_name:atomic(
        {?MODULE, node_msg, hb_opts:get(http_server, no_server_ref, Opts)},
        fun() -> Fun(get_opts(Opts)) end
    ).

%% @doc Return `Opts' with the node message's key material if they contain no
%% wallet, as request execution `Opts' do not. The node's `priv-wallet' is its
%% committer. If `Opts' give `identities', keep them.
with_node_committer(Opts) ->
    case hb_opts:get(priv_wallet, undefined, Opts#{ <<"only">> => local }) of
        undefined ->
            maps:merge(maps:with(?NODE_KEY_OPTS, get_opts(Opts)), Opts);
        _ -> Opts
    end.

%% @doc Initialize the server ID for the current process.
set_proc_server_id(ServerID) ->
    put(server_id, ServerID).

%% @doc Apply the default node message to the given options message.
set_default_opts(Opts) ->
    % Create a temporary options message that does not include the defaults.
    TempOpts = Opts#{ <<"only">> => local },
    % Get the port to use for the server. If no port is provided, we use port 0
    % will the operating system assign a free port.
    Port = hb_opts:get(port, 0, TempOpts),
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
        {priv_wallet, Wallet}
    }),
    Opts#{
        <<"port">> => Port,
        <<"store">> => Store,
        <<"priv-wallet">> => Wallet,
        <<"address">> => hb_util:human_id(ar_wallet:to_address(Wallet)),
        <<"committer">> => hb_opts:committer(#{ <<"priv-wallet">> => Wallet }),
        <<"force-signed">> => true
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
    ok = hb_process_sampler:ensure_started(ServerOpts),
    ok = hb_system_monitor:ensure_started(ServerOpts),
    {ok, _Listener, Port} = new_server(ServerOpts),
    Scheme = scheme(get_opts()),
    % Return the URL with 127.0.0.1, as the node listens on IPv4 only.
    <<Scheme/binary, "://127.0.0.1:", (hb_util:bin(Port))/binary, "/">>.

scheme(NodeMsg) ->
    case hb_tls:config(NodeMsg) of
        TLS when is_map(TLS) -> <<"https">>;
        false -> <<"http">>
    end.

%% @doc Reply to the client's HTTP request with the result of resolving it. The
%% HTTP status comes from the result and is not added to the message, which is
%% sent with the node's signature on its hashpath (`maybe_sign/3'). A list is
%% the `body' of the reply, which its `ao-result' key selects. If a request hook
%% gives the reply (`{reply, Result}'), send `Result'.
reply(InitReq, TABMReq, {reply, Result}, Opts) ->
    reply(InitReq, TABMReq, Result, Opts);
reply(InitReq, TABMReq, {ErlStatus, List}, Opts) when is_list(List) ->
    Msg = #{ <<"ao-result">> => <<"body">>, <<"body">> => List },
    reply(InitReq, TABMReq, {ErlStatus, Msg}, Opts);
reply(InitReq, TABMReq, Result, Opts) ->
    ReplyStartTime = os:system_time(millisecond),
    {ErlStatus, RawMessage} = hb_ao:force_message(Result, Opts),
    KeyNormMessage = hb_ao:normalize_keys(RawMessage, Opts),
    {ok, Req, Message} =
        try reply_handle_cookies(InitReq, KeyNormMessage, Opts)
        catch _Type:Error:_Stacktrace ->
            ?event(warning, {reply_handle_cookies_error, {error, Error}}, Opts),
            {ok, InitReq, KeyNormMessage}
        end,
    {Status, HeadersBeforeCors, EncodedBody} =
        encode_reply(
            hb_http:status_code({ErlStatus, Message}, Opts),
            TABMReq,
            Message,
            Opts
        ),
    % Get the CORS request headers from the message, if they exist.
    ReqHdr = cowboy_req:header(<<"access-control-request-headers">>, Req, <<"">>),
    HeadersWithCors = add_cors_headers(HeadersBeforeCors, ReqHdr, Opts),
    % A bodyless reply carries no `content-digest' either. Every other reply
    % carries the `content-length' of its body: without it, hackney reads a
    % reply whose connection closes early as a whole reply.
    Bodyless = is_bodyless_reply(cowboy_req:method(Req), Status),
    ReplyHeaders =
        case Bodyless of
            true ->
                hb_maps:without([<<"content-digest">>], HeadersWithCors, Opts);
            false ->
                HeadersWithCors#{
                    <<"content-length">> => hb_util:bin(byte_size(EncodedBody))
                }
        end,
    EncodedHeaders = hb_private:reset(ReplyHeaders),
    ?event(debug_http,
        {http_replying,
            {status, {explicit, Status}},
            {path, hb_maps:get(<<"path">>, Req, undefined_path, Opts)},
            {raw_message, RawMessage},
            {enc_headers, {explicit, EncodedHeaders}},
            {enc_body, EncodedBody}
        }
    ),
    ReqBeforeStream = Req#{ resp_headers => EncodedHeaders },
    PostStreamReq = cowboy_req:stream_reply(Status, #{}, ReqBeforeStream),
    % Stream the body back to the caller if there is content, ending the
    % stream with it. If we already signal a bodyless reply, skip.
    case Bodyless of
        true -> skip;
        false -> cowboy_req:stream_body(EncodedBody, fin, PostStreamReq)
    end,
    EndTime = os:system_time(millisecond),
    ReqDuration = EndTime - hb_maps:get(start_time, Req, undefined, Opts),
    ReplyDuration = EndTime - ReplyStartTime,
    record_request_metric(
        ReqDuration * 1000000,
        ReplyDuration * 1000000,
        Status
    ),
    ?event(debug_http, {reply_headers, {explicit, PostStreamReq}}),
    ?event(http_server_short,
        {sent,
            {status, Status},
            {ip, {string, real_ip(Req, Opts)}},
            {duration, EndTime - hb_maps:get(start_time, Req, undefined, Opts)},
            {body_size, byte_size(EncodedBody)},
            {method, cowboy_req:method(Req)},
            {host, get_host(TABMReq, Opts)},
            {path,
                {string,
                    uri_string:percent_decode(
                        hb_maps:get(<<"path">>, TABMReq, <<"[NO PATH]">>, Opts)
                    )
                }
            }
        }
    ),
    {ok, PostStreamReq, no_state}.

%% @doc Whether a reply to this method and status carries no body. A `1xx'
%% reply is informational and a `205' resets content, so both end at headers.
is_bodyless_reply(<<"HEAD">>, _Status) -> true;
is_bodyless_reply(_Method, Status) when Status >= 100, Status < 200 -> true;
is_bodyless_reply(_Method, Status)
        when Status == 204; Status == 205; Status == 304 -> true;
is_bodyless_reply(_Method, _Status) -> false.

%% @doc Commit the result that a reply sends in the given codec. A result
%% without commitments that is sent in `httpsig@1.0' gets its unsigned
%% commitment, over its own keys before the reply adds any, so a receiver keeps
%% those keys. The other codecs send the result alone as the body, and add no
%% commitment. If the node signs its replies and the message that is sent is
%% the result of its hashpath, the node also signs the hashpath. A message that
%% carries a `hashpath' key, such as another node's reply that this node
%% relays, gets no signature from this node.
maybe_sign(RawMsg, Codec, Opts) ->
    Msg =
        case {Codec, hb_maps:is_key(<<"commitments">>, RawMsg, Opts)} of
            {<<"httpsig@1.0">>, false} ->
                hb_message:commit(
                    RawMsg,
                    Opts,
                    #{
                        <<"device">> => <<"httpsig@1.0">>,
                        <<"type">> => <<"unsigned">>
                    }
                );
            _ -> RawMsg
        end,
    case hb_private:from_message(Msg) of
        #{ <<"hashpath">> := Hashpath, <<"hashpath-result">> := ID } ->
            case hb_opts:get(force_signed, false, Opts) andalso
                    not hb_maps:is_key(<<"hashpath">>, Msg, Opts) andalso
                    hb_message:id(Msg, all, Opts) == ID of
                true ->
                    hb_message:commit(
                        Msg#{ <<"hashpath">> => Hashpath },
                        Opts,
                        #{
                            <<"device">> => <<"httpsig@1.0">>,
                            <<"committed">> => [<<"hashpath">>]
                        }
                    );
                false -> Msg
            end;
        _ -> Msg
    end.

%% @doc Commit a result as the reply to a request commits it (`maybe_sign/3'),
%% for a node that sends the reply for this node, such as a `tunnel@1.0'
%% broker: only this node can sign the hashpath of the result. The cookie keys
%% that the reply sends as headers stay in the result, uncommitted.
commit_reply(TABMReq, Msg, Opts) ->
    {ok, _, WithoutCookies} = reply_handle_cookies(#{}, Msg, Opts),
    {Codec, _} = accept_to_codec(TABMReq, WithoutCookies, Opts),
    maps:merge(
        maps:with([<<"cookie">>, <<"set-cookie">>], Msg),
        maybe_sign(WithoutCookies, Codec, accept_signature(TABMReq, Opts))
    ).

%% @doc Handle replying with cookies if the message contains them. Returns the
%% new Cowboy `Req` object, and the message without the cookie keys that no
%% signature covers. Both `set-cookie' and `cookie' fields are treated as
%% viable sources of cookies.
reply_handle_cookies(Req, Message, Opts) ->
    {ok, Cookies} = hb_http:cookie(<<"extract">>, Message, #{}, Opts),
    ?event(debug_cookie, {encoding_reply_cookies, {explicit, Cookies}}),
    case Cookies of
        NoCookies when map_size(NoCookies) == 0 -> {ok, Req, Message};
        _ ->
            % The internal values of the `cookie' field will be stored in the
            % `priv_store' by default, so we let `dev_codec_cookie:opts/1'
            % reset the options.
            {ok, #{ <<"set-cookie">> := SetCookieLines }} =
                hb_http:cookie(
                    <<"to">>,
                    Message,
                    #{ <<"format">> => <<"set-cookie">> },
                    Opts
                ),
            ?event(debug_cookie, {outbound_set_cookie_lines, SetCookieLines}),
            % Add the cookies to the response headers.
            FinalReq =
                lists:foldl(
                    fun(FullCookieLine, ReqAcc) ->
                        [CookieRef, _] = binary:split(FullCookieLine, <<"=">>),
                        RespCookies = maps:get(resp_cookies, ReqAcc, #{}),
                        % Note: Cowboy handles cookies peculiarly. The key
                        % given in the `resp_cookies' map is not used directly
                        % in the response headers. Nonetheless, we use the
                        % key parsed from the cookie line as the key, but do not
                        % be surprised if while debugging you see a different
                        % key created by Cowboy in the response headers.
                        ReqAcc#{
                            resp_cookies =>
                                RespCookies#{ CookieRef => FullCookieLine }
                        }
                    end,
                    Req,
                    SetCookieLines
                ),
            % If the message has no signers, remove its unsigned commitment
            % with its cookie keys: `maybe_sign/3' commits to the keys that
            % the reply sends.
            {
                ok,
                FinalReq,
                hb_message:without_unless_signed(
                    [<<"cookie">>, <<"set-cookie">>],
                    case hb_message:signers(Message, Opts) of
                        [] -> hb_message:uncommitted(Message, Opts);
                        _ -> Message
                    end,
                    Opts
                )
            }
    end.

%% @doc Add permissive CORS headers to a message, if the message has not already
%% specified CORS headers.
add_cors_headers(Msg, ReqHdr, Opts) ->
    CorHeaders = #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
        <<"access-control-expose-headers">> => <<"*">>
    },
     WithAllowHeaders = case ReqHdr of
        <<>> -> CorHeaders;
        _ -> CorHeaders#{
             <<"access-control-allow-headers">> =>
                hb_escape:encode_header(ReqHdr)
        }
    end,
    % Keys in the given message will overwrite the defaults listed below if 
    % included, due to `hb_maps:merge''s precidence order.
    hb_maps:merge(WithAllowHeaders, Msg, Opts).

%% @doc Generate the headers and body for a HTTP response message.
encode_reply(Status, TABMReq, RawMessage, Opts) ->
    {Codec, ContentType} = accept_to_codec(TABMReq, RawMessage, Opts),
    Message = maybe_sign(RawMessage, Codec, accept_signature(TABMReq, Opts)),
    % Verify the result while it is still in `structured@1.0' form, before it is
    % converted to the target wire codec, if paranoid mode enables http_result.
    hb_message:paranoid_verify(http_result, Message, Opts),
    ?event(debug_http, {encoding_reply, {codec, Codec}, {message, Message}}),
    AcceptBundle =
        hb_util:bool(
            hb_maps:get(<<"accept-bundle">>, TABMReq, false, Opts)
        ),
    ?event(debug_http,
        {encoding_reply,
            {status, Status},
            {codec, Codec},
            {should_bundle, AcceptBundle},
            {response_message, Message}
        }
    ),
    % Codecs generally do not need to specify headers outside of the content-type,
    % aside the default `httpsig@1.0' codec, which expresses its form in HTTP
    % documents, and subsequently must set its own headers.
    case {Status, Codec, AcceptBundle} of
        {500, <<"httpsig@1.0">>, false} ->
            ?event(debug_accept,
                {returning_500_error,
                    {status, Status},
                    {codec, Codec},
                    {bundle, AcceptBundle}
                }
            ),
            {ok, ErrMsg} =
                static(
                    <<"hyperbuddy@1.0">>,
                    <<"500.html">>,
                    #{ <<"error">> => hb_format:error(Message, Opts) },
                    Opts
                ),
            {Status,
                maps:without([<<"body">>], ErrMsg),
                maps:get(<<"body">>, ErrMsg, <<>>)
            };
        {Code, <<"httpsig@1.0">>, false}
                when Code == 403; Code == 404;
                     Code == 400, is_map_key(<<"offender">>, Message);
                     Code == 400, is_map_key(<<"resolving">>, Message) ->
            {ok, ErrMsg} =
                static(
                    <<"hyperbuddy@1.0">>,
                    <<"error.html">>,
                    hb_ao_errors:page(Code, Message, TABMReq),
                    Opts
                ),
            {Status,
                maps:without([<<"body">>], ErrMsg),
                maps:get(<<"body">>, ErrMsg, <<>>)
            };
        {_, <<"httpsig@1.0">>, _} ->
            % Encode without the node's key material, as for the resolution of
            % the request: the `index' of the result can run the client's code.
            EncMessage =
                hb_message:convert(
                    Message,
                    case AcceptBundle of
                        true ->
                            #{
                                <<"device">> => <<"httpsig@1.0">>,
                                <<"path">> => <<"to">>,
                                <<"bundle">> => true
                            };
                        false ->
                            TABMReq#{
                                <<"device">> => <<"httpsig@1.0">>,
                                <<"path">> => <<"to">>,
                                <<"index">> =>
                                    hb_opts:get(generate_index, true, Opts)
                            }
                    end,
                    <<"structured@1.0">>,
                    maps:without(
                        ?NODE_KEY_OPTS,
                        Opts#{ <<"topic">> => ao_internal }
                    )
                ),
            {
                Status,
                hb_maps:without([<<"body">>], EncMessage, Opts),
                hb_maps:get(<<"body">>, EncMessage, <<>>, Opts)
            };
        _ ->
            % A codec other than `httpsig@1.0' sends the message as the body,
            % with the type and subtype of the media range that asked for the
            % codec as its `content-type'.
            {
                Status,
                #{ <<"content-type">> => ContentType },
                hb_http:encode(
                    Codec,
                    Message,
                    AcceptBundle,
                    Opts#{ <<"topic">> => ao_internal }
                )
            }
    end.

%% @doc Find the codec of the reply to a request from its `accept', read as RFC
%% 9110 gives, and the `content-type' of a reply in that codec. The media ranges
%% are taken in order of preference, by q-value and then by their order, and
%% the first that the node can serve gives the codec:
%% <pre>
%%     the result's own `content-type'    httpsig@1.0: the body, unchanged
%%     a range that gives a codec          that codec, with the range's type
%%                                         and subtype as `content-type'
%% </pre>
%% We pass over any other range, such as `*/*' and `text/html'. If no range can
%% be served, or the `accept' is not a valid HTTP field, we use `httpsig@1.0'.
accept_to_codec(TABMReq, Msg, Opts) ->
    Accept = hb_maps:get(<<"accept">>, TABMReq, <<"*/*">>, Opts),
    ?event(debug_accept,
        {accept_to_codec,
            {original_req, TABMReq},
            {accept, Accept}
        }
    ),
    MediaRanges =
        try cow_http_hd:parse_accept(Accept)
        catch error:_ -> []
        end,
    Own =
        try
            {Type, SubType, _Params} =
                cow_http_hd:parse_content_type(
                    hb_maps:get(<<"content-type">>, Msg, <<>>, Opts)
                ),
            {Type, SubType}
        catch error:_ -> undefined
        end,
    Ranges = [Range || Range = {_, Quality, _} <- MediaRanges, Quality > 0],
    Sorted = lists:sort(
        fun({_, QualityA, _}, {_, QualityB, _}) ->
            QualityA >= QualityB
        end,
        Ranges
    ),
    preferred_codec(lists:sublist(Sorted, ?MAX_ACCEPT_RANGES), Own, Opts).

%% @doc Return the codec of the first media range that the node can serve, and
%% the `content-type' of a reply in it, or `httpsig@1.0' if there is none.
preferred_codec([], _Own, _Opts) -> {<<"httpsig@1.0">>, undefined};
preferred_codec([{{Type, SubType, _} = MediaRange, _, _} | Rest], Own, Opts) ->
    case {Type, SubType} == Own orelse hb_http:codec(MediaRange, Opts) of
        true -> {<<"httpsig@1.0">>, undefined};
        {ok, Codec} -> {Codec, <<Type/binary, "/", SubType/binary>>};
        {error, _} -> preferred_codec(Rest, Own, Opts)
    end.

%% @doc Return `Opts' with `force-signed' set if the request's
%% `accept-signature' (RFC 9421) asks for the node's signature over the
%% `hashpath' of the result, so that `maybe_sign/3' signs it. A member asks for
%% it with the inner list `("hashpath")' and the URI of the node's committer as
%% its `keyid'. The field is a preference: we ignore any other member, and a
%% field that does not parse.
accept_signature(#{ <<"accept-signature">> := Field }, Opts) ->
    Members =
        try hb_structured_fields:parse_dictionary(Field)
        catch error:_ -> []
        end,
    KeyID = {<<"keyid">>, {string, hb_opts:committer(Opts)}},
    case
        [
            Name
        ||
            {Name, {list, Keys, Params}} <- Members,
            Keys == [{item, {string, <<"hashpath">>}, []}],
            lists:member(KeyID, Params)
        ]
    of
        [] -> Opts;
        _ -> Opts#{ <<"force-signed">> => true }
    end;
accept_signature(_TABMReq, Opts) ->
    Opts.

%% @doc Convert a cowboy request to a normalized message. We first parse the
%% `primitive' message from the request: A message (represented as an Erlang
%% map) of binary keys and values for the request headers and query parameters.
%% We then find the codec of the request from its `signature-input' and
%% `content-type' headers, and decode the request with it.
req_to_tabm_singleton(Req, Body, Opts) ->
    FullPath =
        <<
            (cowboy_req:path(Req))/binary,
            "?",
            (cowboy_req:qs(Req))/binary
        >>,
    Headers = cowboy_req:headers(Req),
    {ok, _Path, QueryKeys} = hb_singleton:from_path(FullPath),
    % The query parameters override the request's headers, except for the
    % keys that its signatures cover: a signature is verified over the values
    % that were signed.
    SignedKeys = signed_keys(Headers, Opts),
    PrimitiveMsg = maps:merge(Headers, maps:without(SignedKeys, QueryKeys)),
    % The headers give the codec of the request, and the query does not. A
    % `content-type' of a device that is not a codec is refused with a 415.
    Codec =
        case hb_http:message_codec(Headers, Opts) of
            {ok, HeaderCodec} -> HeaderCodec;
            {error, _} ->
                ContentType = maps:get(<<"content-type">>, Headers),
                throw({'unsupported-media-type', ContentType})
        end,
    ?event(debug_http,
        {parsing_req,
            {path, FullPath},
            {query, QueryKeys},
            {headers, Headers},
            {primitive_message, PrimitiveMsg}
        }
    ),
    ?event({req_to_tabm_singleton, {codec, Codec}}),
    Decoded =
        case Codec of
            <<"httpsig@1.0">> ->
                % HTTPSig commitments share the transport headers. Retain only
                % their committed fields before restoring request metadata.
                hb_message:with_only_committed_deep(
                    hb_message:convert(
                        PrimitiveMsg#{ <<"body">> => Body },
                        <<"structured@1.0">>,
                        Codec,
                        Opts
                    ),
                    Opts
                );
            _ ->
                % The body is the whole message, encoded by the codec. If it
                % has commitments, we keep only the keys that they cover.
                hb_message:with_only_committed_deep(
                    hb_http:decode(Codec, Body, Opts),
                    Opts
                )
        end,
    % Reset the request's private keys, which a codec can restore from the
    % wire, before the node sets its own. Then remove the commitments of an
    % unsigned request, as `hb_ao' does for every execution input.
    Input = hb_ao:execution_input(hb_private:reset(Decoded), Opts),
    % Cookie-backed commitments need the request's cookie and peer context.
    Normalized = normalize_unsigned(PrimitiveMsg, Req, Input, Opts),
    maybe
        true ?= hb_message:deep_verify(Normalized, Opts),
        Signers = hb_message:signers(Normalized, Opts),
        true ?= not hb_opts:get(force_signed_requests, false, Opts)
            orelse Signers =/= [],
        case Signers =/= [] andalso hb_opts:get(store_all_signed, false, Opts) of
            true ->
                {ok, _} =
                    hb_cache:write(Decoded,
                        Opts#{
                            <<"store">> =>
                                #{
                                    <<"store-module">> => hb_store_fs,
                                    <<"name">> => <<"cache-mainnet/http">>
                                }
                        }
                    );
            false -> do_nothing
        end,
        Normalized
    else
        _ -> throw({invalid_commitments, Normalized})
    end.

%% @doc Return the message keys that the request's HTTPSig signatures cover,
%% from its `signature-input' headers. A signature covers its keys' values, so
%% the query parameters do not replace the signed header values of these keys.
%% A derived component, such as `@path', covers the key `path'.
%% HTTPSig is the only request form that carries signed content in headers:
%% the other codecs sign the body, which the query parameters do not reach.
signed_keys(Headers, Opts) ->
    case hb_maps:find(<<"signature-input">>, Headers, Opts) of
        {ok, SigInput} ->
            lists:usort(
                lists:append(
                    [
                        [
                            hb_escape:decode(string:trim(Key, leading, "@"))
                        ||
                            {item, {string, Key}, _} <- Fields
                        ]
                    ||
                        {_Name, {list, Fields, _Params}} <-
                            hb_structured_fields:parse_dictionary(SigInput)
                    ]
                )
            );
        error -> []
    end.

%% @doc Observe a request without letting any method token become a permanent
%% Prometheus label. Known methods keep their label; all other tokens share
%% the `OTHER' label.
observe_request_metrics(Metrics) ->
    prometheus_cowboy2_instrumenter:observe(
        case Metrics of
            #{ req := Req } ->
                Method = cowboy_req:method(Req),
                Metrics#{
                    req => Req#{ method => normalize_method(Method) }
                };
            _ -> Metrics
        end
    ).

normalize_method(Method) ->
    case lists:member(Method, ?METRIC_METHODS) of
        true -> Method;
        false -> <<"OTHER">>
    end.

%% @doc Add the method and path to a message, if they are not already present.
%% Remove browser-added fields that are unhelpful during processing (for example,
%% `content-length').
%% The precidence order for finding the path is:
%% 1. The path in the message
%% 2. The path and query of the request URI
normalize_unsigned(PrimMsg, Req = #{ headers := RawHeaders }, Msg, Opts) ->
    ?event(debug_http, {adding_method_and_path_from_request, {explicit, Req}}),
    Method = cowboy_req:method(Req),
    MsgPath =
        hb_maps:get(
            <<"path">>,
            Msg,
            hb_maps:get(
                <<"path">>, 
                RawHeaders,
                iolist_to_binary(
                    cowboy_req:uri(
                        Req,
                        #{
                            host => undefined,
                            port => undefined,
                            scheme => undefined
                        }
                    )
                ),
                Opts
            ),
            Opts
        ),
    FilterKeys = hb_opts:get(http_inbound_filter_keys, ?DEFAULT_FILTER_KEYS, Opts),
    FilteredMsg = hb_message:without_unless_signed(FilterKeys, Msg, Opts),
    BaseMsg =
        FilteredMsg#{
            <<"method">> => Method,
            <<"path">> => MsgPath,
            <<"accept-bundle">> =>
                maps:get(
                    <<"accept-bundle">>,
                    Msg,
                    maps:get(
                        <<"accept-bundle">>,
                        PrimMsg,
                        maps:get(<<"accept-bundle">>, RawHeaders, false)
                    )
                ),
            <<"accept">> =>
                Accept = maps:get(
                    <<"accept">>,
                    Msg,
                    maps:get(
                        <<"accept">>,
                        PrimMsg,
                        maps:get(<<"accept">>, RawHeaders, <<"*/*">>)
                    )
                )
        },
    ?event(debug_accept, {normalize_unsigned, {accept, Accept}}),
    % Parse and add the cookie from the request, if present. We reinstate the
    % `cookie' field in the message, as it is not typically signed, yet should
    % be honored by the node anyway.
    {ok, WithCookie} =
        case maps:get(<<"cookie">>, RawHeaders, undefined) of
            undefined -> {ok, BaseMsg};
            Cookie ->
                hb_http:cookie(
                    <<"from">>,
                    BaseMsg#{ <<"cookie">> => Cookie },
                    #{},
                    Opts
                )
        end,
    % If the body is empty and unsigned, we remove it.
    NormalBody =
        case hb_maps:get(<<"body">>, WithCookie, undefined, Opts) of
            <<"">> -> hb_message:without_unless_signed(<<"body">>, WithCookie, Opts);
            _ -> WithCookie
        end,
    RealIP = real_ip(Req, Opts),
    WithPeer = case hb_maps:get(<<"ao-peer-port">>, NormalBody, undefined, Opts) of
        undefined -> NormalBody;
        P2PPort ->
            Address = case binary:match(RealIP, <<":">>) of
                nomatch -> RealIP;
                _ -> <<"[", RealIP/binary, "]">>
            end,
            Peer = <<Address/binary, ":", (hb_util:bin(P2PPort))/binary>>,
            (hb_message:without_unless_signed(<<"ao-peer-port">>, NormalBody, Opts))#{
                <<"ao-peer">> => Peer
            }
    end,
    WithPrivIP = hb_private:set(WithPeer, <<"ip">>, RealIP, Opts),
    Host = cowboy_req:host(Req),
    WithPrivIP#{<<"host">> => Host}.


%% @doc Return the caller's IP address: the source address of the connection's
%% PROXY header, if the listener reads one; else the `x-real-ip' header, if the
%% socket's peer is in `http-trusted-proxies'; else the socket's peer address.
real_ip(Req, Opts) ->
    {PeerIP, _} = cowboy_req:peer(Req),
    Peer = hb_util:bin(inet:ntoa(PeerIP)),
    Trusted = hb_opts:get(http_trusted_proxies, [], Opts),
    case {maps:get(proxy_header, Req, #{}), lists:member(Peer, Trusted)} of
        {#{ src_address := Source }, _} -> hb_util:bin(inet:ntoa(Source));
        {_, true} -> cowboy_req:header(<<"x-real-ip">>, Req, Peer);
        {_, false} -> Peer
    end.

%% @doc Find the hostname from either the inbound TABM request form,
%% or set explicitly in the node message.
get_host(TABMReq, Opts) ->
    ReqHost = maps:get(<<"host">>, TABMReq, <<"no_host">>),
    case hb_opts:get(node_host, no_host, Opts) of
        no_host ->
            ReqHost;
        NodeHost ->
            % Replace suffix part
            filename:rootname(ReqHost, <<".", NodeHost/binary>>)
    end.

%%% Metrics

init_prometheus() ->
    hb_prometheus:declare(histogram, [
		{name, http_server_encoding_duration_seconds},
        {labels, [status_code]},
		{buckets, [0.001, 0.0025, 0.005,
                    0.01, 0.025, 0.05,
                    0.1, 0.25, 0.5,
                    1, 2.5, 5,
                    10, 30, 60]},
		{
			help,
			"The total duration of an hb_http_server:reply call. This starts when a response"
            "is ready to send back to the client and ends when the message is deliver"
            "to the client."
		}
	]),
    hb_prometheus:declare(histogram, [
		{name, http_server_duration_seconds},
        {labels, [status_code]},
		{buckets, [0.001, 0.0025, 0.005,
                    0.01, 0.025, 0.05,
                    0.1, 0.25, 0.5,
                    1, 2.5, 5,
                    10, 30, 60]},
		{
			help,
			"The total duration of an hb_http_server request call." 
		}
	]).

record_request_metric(TotalDuration, ReplyDuration, StatusCode) ->
    spawn(
        fun() ->
            hb_prometheus:observe(
                TotalDuration,
                http_server_duration_seconds,
                [StatusCode]
            ),
            hb_prometheus:observe(
                ReplyDuration,
                http_server_encoding_duration_seconds,
                [StatusCode]
            )
        end
    ).

%%% Tests

%% @doc Ensure that the `start' hook can be used to modify the node options. We
%% do this by creating a message with a device that has a `start' key. This 
%% key takes the message's body (the anticipated node options) and returns a
%% modified version of that body, which will be used to configure the node. We
%% then check that the node options were modified as we expected.
set_node_opts_test() ->
    Node =
        start_node(#{
            <<"on">> => #{
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

%% @doc Test the set_opts/2 function that merges request with options,
%% manages node history, and updates server state.
set_opts_test() ->
    DefaultOpts = hb_opts:default_message_with_env(),
    start_node(DefaultOpts#{ 
        <<"priv-wallet">> => Wallet = ar_wallet:new(), 
        <<"identities">> => #{},
        <<"port">> => rand:uniform(10000) + 10000 
    }),
    Opts = get_opts(#{ 
        <<"http-server">> => hb_util:human_id(ar_wallet:to_address(Wallet))
    }),
    NodeHistory = hb_opts:get(node_history, [], Opts),
    ?event(debug_node_history, {node_history_length, length(NodeHistory)}),
    ?assert(length(NodeHistory) == 0),
    % Test case 1: Empty node-history case
    Request1 = #{
        <<"hello">> => <<"world">>
    },             
    {ok, UpdatedOpts1} = set_opts(Request1, Opts),
    NodeHistory1 = hb_opts:get(node_history, not_found, UpdatedOpts1),
    Key1 = hb_opts:get(<<"hello">>, not_found, UpdatedOpts1),
    ?event(debug_node_history, {node_history_length, length(NodeHistory1)}),
    ?assert(length(NodeHistory1) == 1),
    ?assert(Key1 == <<"world">>),
    % Test case 2: Non-empty node-history case
    Request2 = #{
        <<"hello2">> => <<"world2">>
    },
    {ok, UpdatedOpts2} = set_opts(Request2, UpdatedOpts1),
    NodeHistory2 = hb_opts:get(node_history, not_found, UpdatedOpts2),
    Key2 = hb_opts:get(<<"hello2">>, not_found, UpdatedOpts2),
    ?event(debug_node_history, {node_history_length, length(NodeHistory2)}),
    ?assert(length(NodeHistory2) == 2),
    ?assert(Key2 == <<"world2">>),
    % Test case 3: Non-empty node-history case
    {ok, UpdatedOpts3} = set_opts(#{}, UpdatedOpts2#{ <<"hello3">> => <<"world3">> }),
    NodeHistory3 = hb_opts:get(node_history, not_found, UpdatedOpts3),
    Key3 = hb_opts:get(<<"hello3">>, not_found, UpdatedOpts3),
    ?event(debug_node_history, {node_history_length, length(NodeHistory3)}),
    ?assert(length(NodeHistory3) == 3),
    ?assert(Key3 == <<"world3">>),
    % If request `Opts' give `identities', the node message keeps them.
    Ids = #{ <<"zone">> => #{ <<"priv-wallet">> => ar_wallet:new() } },
    set_opts(maps:remove(<<"priv-wallet">>, Opts#{ <<"identities">> => Ids })),
    ?assertEqual(Ids, hb_opts:get(identities, not_found, get_opts(Opts))),
    % Updates at once keep the key that each of them sets.
    Keys = [ <<"update-", (hb_util:bin(N))/binary>> || N <- lists:seq(1, 10) ],
    hb_pmap:parallel_map(
        Keys,
        fun(Key) ->
            update_opts(
                fun(NodeMsg) ->
                    timer:sleep(1),
                    set_opts(NodeMsg#{ Key => true })
                end,
                Opts
            )
        end,
        10
    ),
    ?assertEqual([], Keys -- maps:keys(get_opts(Opts))).

set_tls_opts_rejected_test() ->
    ?assertEqual(
        {error, <<"TLS configuration cannot be changed at runtime.">>},
        set_opts(#{ <<"tls">> => false }, #{})
    ).

tls_http3_rejected_before_start_test() ->
    ?assertError(
        'tls-not-supported-for-http3',
        start_node(#{
            <<"priv-wallet">> => ar_wallet:load_keyfile("test/key-1.json"),
            <<"protocol">> => http3,
            <<"tls">> => #{}
        })
    ).

%% @doc Check that a listener reads a PROXY header only if the node sets
%% `http-proxy-protocol', and that the ACME challenge listener never does.
proxy_protocol_options_test() ->
    ?assertEqual(false,
        maps:get(proxy_header, listener_protocol_options(test, #{}))),
    Opts = #{ <<"http-proxy-protocol">> => true },
    ?assertEqual(true,
        maps:get(proxy_header, listener_protocol_options(test, Opts))),
    Challenge = challenge_node(#{}, test, Opts),
    ?assertEqual(false,
        maps:get(proxy_header, listener_protocol_options(test, Challenge))).

%% @doc An HTTP/2 listener bounds the concurrent streams of one connection and
%% lets the node lower or raise the bound.
http2_max_concurrent_streams_test() ->
    ?assertEqual(100,
        maps:get(max_concurrent_streams, listener_protocol_options(test, #{}))),
    ?assertEqual(25,
        maps:get(
            max_concurrent_streams,
            listener_protocol_options(
                test, #{ <<"max-concurrent-streams">> => 25 }
            )
        )).

%% @doc Check that the caller's IP is the PROXY header's source address or the
%% socket's peer address, never an `x-real-ip' header, and that `ao-peer'
%% gives an IPv6 address in brackets.
caller_address_test() ->
    Req = #{
        method => <<"GET">>, path => <<"/body">>, qs => <<>>,
        scheme => <<"http">>, host => <<"localhost">>, port => 80,
        peer => {{127, 0, 0, 1}, 1234},
        headers => #{
            <<"x-real-ip">> => <<"198.51.100.99">>,
            <<"ao-peer-port">> => <<"12345">>
        }
    },
    lists:foreach(
        fun({Request, IP, Peer}) ->
            Msg = req_to_tabm_singleton(Request, <<>>, #{}),
            ?assertEqual(IP, hb_private:get(<<"ip">>, Msg, #{})),
            ?assertEqual(Peer, hb_maps:get(<<"ao-peer">>, Msg))
        end,
        [
            {Req, <<"127.0.0.1">>, <<"127.0.0.1:12345">>},
            {Req#{ proxy_header => #{ command => local } },
                <<"127.0.0.1">>, <<"127.0.0.1:12345">>},
            {Req#{ proxy_header => #{ src_address => {192, 0, 2, 1} } },
                <<"192.0.2.1">>, <<"192.0.2.1:12345">>},
            {Req#{ proxy_header => #{
                src_address => {16#2001, 16#db8, 0, 0, 0, 0, 0, 1}
            } }, <<"2001:db8::1">>, <<"[2001:db8::1]:12345">>},
            {Req#{ peer => {{0, 0, 0, 0, 0, 0, 0, 1}, 1234} },
                <<"::1">>, <<"[::1]:12345">>}
        ]
    ),
    % The `x-real-ip' of a request from a listed proxy is the caller's address.
    Trusted = #{ <<"http-trusted-proxies">> => [<<"127.0.0.1">>] },
    ?assertEqual(
        <<"198.51.100.99">>,
        hb_private:get(
            <<"ip">>,
            req_to_tabm_singleton(Req, <<>>, Trusted),
            #{}
        )
    ).

restart_server_test() ->
    % We force HTTP2, overriding the HTTP3 feature, because HTTP3 restarts don't work yet.
    Wallet = ar_wallet:new(),
    BaseOpts = #{
        <<"test-key">> => <<"server-1">>,
        <<"priv-wallet">> => Wallet,
        <<"protocol">> => http2
    },
    _ = start_node(BaseOpts),
    N2 = start_node(BaseOpts#{ <<"test-key">> => <<"server-2">> }),
    ?assertEqual(
        {ok, <<"server-2">>},
        hb_http:get(N2, <<"/~meta@1.0/info/test-key">>, #{ <<"protocol">> => http2 })
    ).

%% @doc A node in `prod' mode serves requests, and does not profile or record
%% them unless its options enable it. `HB_MODE' gives the processes that serve
%% them the global mode `prod'.
prod_mode_test() ->
    Node = start_node(#{ <<"mode">> => prod }),
    os:putenv("HB_MODE", "prod"),
    try
        ?assertEqual(
            {ok, prod},
            hb_http:get(Node, <<"/~meta@1.0/info/mode">>, #{})
        ),
        ?assertEqual(
            {error, <<"Profiling is disabled.">>},
            hb_http:get(Node, <<"/~profile@1.0/run?run=/~meta@1.0/info">>, #{})
        ),
        ?assertEqual(
            {error, <<"Recording is disabled.">>},
            hb_http:get(
                Node,
                <<"/~recorder@1.0/record?record=/~meta@1.0/info">>,
                #{}
            )
        )
    after
        os:unsetenv("HB_MODE")
    end.

%% @doc A request whose path reads a link to a message the node does not hold
%% gets 404.
read_missing_link_test() ->
    Node = start_node(#{ <<"store">> => hb_test_utils:test_store() }),
    Missing = hb_util:human_id(crypto:strong_rand_bytes(32)),
    ?assertMatch(
        {error, #{ <<"status">> := 404 }},
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/x">>,
                <<"x">> => <<"ao:", Missing/binary>>
            },
            #{}
        )
    ).

test_opts() ->
    #{ <<"store">> => hb_test_utils:test_store(), <<"priv-wallet">> => hb:wallet() }.

isolated_test_opts() ->
    #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new()
    }.

%% @doc Verify that request hosts are formatted appropriately for logging.
get_host_test_parallel() ->
    Opts = #{ <<"node-host">> => <<"example.com">> },
    ?assertEqual(
        <<"abc">>,
        get_host(#{ <<"host">> => <<"abc.example.com">> }, Opts)
    ),
    ?assertEqual(
        <<"example.com">>,
        get_host(#{ <<"host">> => <<"example.com">> }, Opts)
    ).

%% @doc Test that forced response signing preserves signers and commits the
%% hashpath.
maybe_sign_hashpath_only_test() ->
    OldWallet = ar_wallet:new(),
    NewWallet = ar_wallet:new(),
    Hashpath = hb_path:hashpath(<<"test-hashpath">>, #{}),
    OldOpts =
        #{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"priv-wallet">> => OldWallet
        },
    Opts =
        #{
            <<"force-signed">> => true,
            <<"priv-wallet">> => NewWallet
        },
    OldSigner = hb_test_utils:committer(OldWallet),
    NewSigner = hb_test_utils:committer(NewWallet),
    AlreadySigned =
        hb_hashpath:attach(
            hb_message:commit(
                #{ <<"body">> => <<"test">> },
                OldOpts,
                #{ <<"committed">> => [<<"body">>] }
            ),
            Hashpath,
            Opts
        ),
    Signed = maybe_sign(AlreadySigned, <<"httpsig@1.0">>, Opts),
    ?assertEqual(Hashpath, maps:get(<<"hashpath">>, Signed)),
    ?assertEqual(
        lists:sort([OldSigner, NewSigner]),
        lists:sort(hb_message:signers(Signed, Opts))
    ),
    ?assert(hb_test_utils:has_committed_keys(Signed, [<<"body">>])),
    ?assert(hb_test_utils:has_committed_keys(Signed, [<<"hashpath">>])),
    ?assert(hb_message:verify(Signed, all, Opts)).

%% @doc Test that forced response signing gives a reply without a hashpath its
%% unsigned commitment alone in `httpsig@1.0', and no commitment in another
%% codec, and leaves a signed reply as it is.
maybe_sign_without_hashpath_test() ->
    Res = #{ <<"body">> => <<"test">> },
    Opts =
        #{
            <<"force-signed">> => true,
            <<"priv-wallet">> => ar_wallet:new()
        },
    ?assertEqual(
        [hb_message:id(Res, all, Opts)],
        maps:keys(
            maps:get(
                <<"commitments">>,
                maybe_sign(Res, <<"httpsig@1.0">>, Opts)
            )
        )
    ),
    ?assertEqual(Res, maybe_sign(Res, <<"json@1.0">>, Opts)),
    Signed = hb_message:commit(Res, Opts),
    ?assertNotEqual([], hb_message:signers(Signed, Opts)),
    ?assertEqual(Signed, maybe_sign(Signed, <<"httpsig@1.0">>, Opts)).

%% @doc The node signs the `hashpath' of a result alone: if it has
%% `force-signed' set, or for a request whose `accept-signature' asks for it. A
%% result changed after its hashpath was attached is not signed.
hashpath_signature_test() ->
    Wallet = ar_wallet:new(),
    Hashpath = hb_path:hashpath(<<"test-hashpath">>, #{}),
    Opts = #{ <<"force-signed">> => true, <<"priv-wallet">> => Wallet },
    Signer = hb_test_utils:committer(Wallet),
    Result =
        hb_hashpath:attach(
            #{ <<"body">> => <<"test">>, <<"status">> => 200 },
            Hashpath,
            Opts
        ),
    Signed = maybe_sign(Result, <<"httpsig@1.0">>, Opts),
    ?assertEqual(Hashpath, maps:get(<<"hashpath">>, Signed)),
    ?assertEqual([Signer], hb_message:signers(Signed, Opts)),
    ?assert(hb_test_utils:has_committed_keys(Signed, [<<"hashpath">>])),
    ?assert(hb_message:verify(Signed, all, Opts)),
    % A message changed after its hashpath was attached is not signed.
    Changed = Result#{ <<"body">> => <<"changed">> },
    ?assertEqual(
        [],
        hb_message:signers(
            maybe_sign(Changed, <<"httpsig@1.0">>, Opts),
            Opts
        )
    ),
    % A node without `force-signed' signs the `hashpath' of the reply to a
    % request whose `accept-signature' asks for it.
    {200, Headers, _} =
        encode_reply(
            200,
            #{
                <<"accept-signature">> =>
                    <<"sig1=(\"hashpath\");keyid=\"", Signer/binary, "\"">>
            },
            Result,
            Opts#{ <<"force-signed">> => false }
        ),
    ?assertNotEqual(
        nomatch,
        binary:match(maps:get(<<"signature-input">>, Headers), Signer)
    ).

%% @doc Every ingress codec verifies commitments and applies the node's
%% signature policy before executing the request.
ingress_commitments_test_() ->
    [
        {binary_to_list(Codec), fun() ->
            Opts = isolated_test_opts(),
            CommitCodec = case Codec of
                <<"json@1.0">> -> <<"httpsig@1.0">>;
                _ -> Codec
            end,
            Msg = #{ <<"body">> => <<"original">> },
            Signed = hb_message:commit(Msg, Opts, CommitCodec),
            Unsigned = hb_message:normalize_commitments(Msg, Opts),
            % A commitment with any constant HMAC key is unsigned.
            Constant = hb_message:commit(Msg, Opts,
                #{
                    <<"type">> => <<"hmac-sha256">>,
                    <<"committer">> => <<"hmac:constant:x">>
                }
            ),
            Invalid = Signed#{ <<"body">> => <<"modified">> },
            SignedJSON =
                hb_message:commit(
                    Msg#{ <<"content-type">> => <<"application/json">> },
                    Opts,
                    CommitCodec
                ),
            WrongMethod = hb_message:commit(
                Msg#{ <<"method">> => <<"GET">> }, Opts, CommitCodec),
            Child = case Codec of
                <<"tx@1.0">> -> hb_message:commit(Msg, Opts, <<"ans104@1.0">>);
                _ -> Signed
            end,
            lists:foreach(
                fun(Required) ->
                    Node = start_node(
                        (isolated_test_opts())#{
                            <<"force-signed-requests">> => Required
                        }
                    ),
                    UnsignedStatus = case Required of true -> 400; false -> 200 end,
                    ?assertMatch({ok, UnsignedStatus, _, _},
                        ingress_request(Node, Codec, Msg, Opts)),
                    ?assertMatch({ok, UnsignedStatus, _, _},
                        ingress_request(Node, Codec, Unsigned, Opts)),
                    ?assertMatch({ok, UnsignedStatus, _, _},
                        ingress_request(Node, Codec, Constant, Opts)),
                    ?assertMatch({ok, UnsignedStatus, _, _},
                        ingress_request(Node, Codec,
                            Msg#{ <<"child">> => Child }, Opts)),
                    ?assertMatch({ok, 200, _, <<"original">>},
                        ingress_request(Node, Codec, Signed, Opts)),
                    % A signed request is read in its codec, whatever its own
                    % `content-type'.
                    ?assertMatch({ok, 200, _, <<"original">>},
                        ingress_request(Node, Codec, SignedJSON, Opts)),
                    ?assertMatch({ok, 400, _, _},
                        ingress_request(Node, Codec, Invalid, Opts)),
                    ?assertMatch({ok, 400, _, _},
                        ingress_request(Node, Codec, WrongMethod, Opts)),
                    ?assertMatch({ok, 400, _, _},
                        ingress_request(Node, Codec,
                            Msg#{ <<"child">> =>
                                Child#{ <<"body">> => <<"modified">> } }, Opts))
                end,
                [false, true]
            )
        end}
    || Codec <- [<<"httpsig@1.0">>, <<"ans104@1.0">>, <<"tx@1.0">>, <<"json@1.0">>]
    ].

%% @doc JSON ingress preserves explicit links and verifies inline children.
ingress_json_links_test() ->
    Opts = isolated_test_opts(),
    Req = #{
        method => <<"POST">>, path => <<"/body">>, qs => <<>>,
        scheme => <<"http">>, host => <<"localhost">>, port => 80,
        peer => {{127, 0, 0, 1}, 1234},
        headers => #{ <<"content-type">> => <<"application/json">> }
    },
    ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Child = hb_message:commit(#{ <<"number">> => 42 }, Opts),
    Msg = #{
        <<"body">> => <<"original">>,
        <<"child">> => Child,
        <<"reference">> => <<"ao:", ID/binary>>
    },
    Decoded = req_to_tabm_singleton(Req, hb_json:encode(Msg), Opts),
    ?assertEqual(Child, maps:get(<<"child">>, Decoded)),
    ?assertMatch(
        {link, <<"ao:", ID/binary>>, _},
        maps:get(<<"reference">>, Decoded)
    ),
    % A request cannot give the node a private key: a crafted `priv/hook-caller'
    % does not reach the kernel-only gate that reads it.
    ?assertEqual(
        not_found,
        hb_private:get(
            <<"hook-caller">>,
            req_to_tabm_singleton(
                Req,
                hb_json:encode(
                    #{ <<"priv">> => #{ <<"hook-caller">> => <<"kernel">> } }
                ),
                Opts
            ),
            Opts
        )
    ),
    % An unsigned request carries no commitment once it is decoded.
    Unsigned = hb_message:normalize_commitments(#{ <<"a">> => <<"1">> }, Opts),
    ?assertNot(
        maps:is_key(
            <<"commitments">>,
            req_to_tabm_singleton(
                Req,
                hb_message:convert(Unsigned, <<"json@1.0">>, Opts),
                Opts
            )
        )
    ),
    ?assertThrow(
        {invalid_commitments, _},
        req_to_tabm_singleton(
            Req,
            hb_json:encode(Msg#{ <<"child">> => Child#{ <<"number">> => 43 } }),
            Opts
        )
    ),
    ?assertEqual({error, not_found}, hb_cache:read(ID, Opts)).

%% @doc Send the codec's wire representation directly to a real HTTP node.
ingress_request(Node, Codec, Msg, Opts) ->
    Wire = hb_message:convert(
        Msg, #{ <<"device">> => Codec, <<"bundle">> => true }, Opts),
    MediaType = #{ <<"content-type">> => hb_http:media_type(Codec) },
    {Headers, Body} = case Codec of
        <<"httpsig@1.0">> ->
            {maps:remove(<<"body">>, Wire), maps:get(<<"body">>, Wire, <<>>)};
        <<"ans104@1.0">> -> {MediaType, ar_bundles:serialize(Wire)};
        <<"tx@1.0">> ->
            {MediaType, hb_json:encode(ar_tx:tx_to_json_struct(Wire))};
        _ -> {MediaType, Wire}
    end,
    hb_http_client:request(
        #{
            peer => Node,
            path => <<"//body">>,
            method => <<"POST">>,
            headers => Headers,
            body => Body
        },
        Opts
    ).

%% @doc Browser requests receive friendly, templated authorization and missing
%% resource pages, with the corresponding HTTP status.
client_error_pages_test() ->
    Node = start_node(isolated_test_opts()),
    Link = <<"ao:", (hb_util:human_id(crypto:strong_rand_bytes(32)))/binary>>,
    lists:foreach(
        fun({Path, Status, Title}) ->
            {ok, {{_, Status, _}, Headers, Body}} = httpc:request(
                get,
                {binary_to_list(<<Node/binary, Path/binary>>), []},
                [],
                [{body_format, binary}]
            ),
            ?assertEqual("text/html", proplists:get_value("content-type", Headers)),
            ?assertNotEqual(nomatch, binary:match(Body, Title)),
            ?assertEqual(nomatch, binary:match(Body, <<"{{">>)),
            ?assertEqual(nomatch, binary:match(Body, <<"not_authorized">>))
        end,
        [
            {<<"/~cache@1.0/write">>, 403, <<"Access denied.">>},
            {<<"/~cache@1.0/read?read=missing">>, 404, <<"Page cannot be found.">>},
            % A link in the error is printed as its URI.
            {<<"/x?x=", Link/binary>>, 404, <<"<dd>", Link/binary, "</dd>">>}
        ]
    ).

%% @doc A request whose commitment carries the operator's address but was not
%% made with the operator's key is refused, and the node message stays as it
%% is.
forged_commitment_rejected_test() ->
    Opts = test_opts(),
    Node = start_node(Opts),
    Update =
        #{
            <<"path">> => <<"/~meta@1.0/info">>,
            <<"method">> => <<"POST">>,
            <<"short-trace-len">> => 7,
            <<"type">> => <<"node-message">>
        },
    Genuine = hb_message:commit(Update, Opts),
    [{_, GenuineComm}] =
        maps:to_list(hb_maps:get(<<"commitments">>, Genuine, Opts)),
    Attacker = isolated_test_opts(),
    Signed = hb_message:commit(Update, Attacker),
    [{ID, Comm}] = maps:to_list(hb_maps:get(<<"commitments">>, Signed, Opts)),
    Forged =
        Signed#{
            <<"commitments">> =>
                #{
                    ID =>
                        Comm#{
                            <<"keyid">> =>
                                hb_maps:get(<<"keyid">>, GenuineComm, Opts),
                            <<"committer">> =>
                                hb_maps:get(<<"committer">>, GenuineComm, Opts)
                        }
                }
        },
    ?assertEqual(
        hb_message:signers(Genuine, Opts),
        hb_message:signers(Forged, Opts)
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 400 }},
        hb_http:post(Node, Forged, Attacker)
    ),
    {ok, Unchanged} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    ?assertNotEqual(7, hb_ao:get(<<"short-trace-len">>, Unchanged, Opts)),
    ?assertMatch({ok, _}, hb_http:post(Node, Genuine, Opts)),
    {ok, Changed} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    ?assertEqual(7, hb_ao:get(<<"short-trace-len">>, Changed, Opts)).

paranoid_http_result_test() ->
    % The `http_result' topic verifies each response at the reply boundary (in
    % `encode_reply', before wire conversion): a validly committed result
    % encodes cleanly, while corrupting its committed body is caught.
    Opts =
        (test_opts())#{
            <<"paranoid-verify">> => [http_result],
            <<"debug-print">> => []
        },
    Valid = hb_message:commit(#{ <<"body">> => <<"ok">> }, Opts),
    ?assertMatch({_, _, _}, encode_reply(200, #{}, Valid, Opts)),
    ?assertThrow(
        {paranoid_verification_failure, http_result, _, _, _},
        encode_reply(200, #{}, Valid#{ <<"body">> => <<"mangled">> }, Opts)
    ).

binary_codec_reply_headers_test() ->
    Opts = test_opts(),
    Message = #{ <<"body">> => <<"value">>, <<"result">> => <<"ok">> },
    {200, Headers, Body} =
        encode_reply(
            200,
            #{ <<"accept">> => <<"application/json">> },
            Message,
            Opts
        ),
    ?assertEqual(#{ <<"content-type">> => <<"application/json">> }, Headers),
    ?assertEqual(
        Message,
        hb_message:convert(Body, <<"structured@1.0">>, <<"json@1.0">>, Opts)
    ),
    % A reply in `flat@1.0' has the keys of the result, without its `priv'.
    ?assertMatch(
        {200, _, <<"body: value\nresult: ok\n">>},
        encode_reply(
            200,
            #{ <<"accept">> => <<"application/x.ao.flat">> },
            Message#{ <<"priv">> => #{ <<"hashpath">> => <<"hidden">> } },
            Opts
        )
    ).

%% @doc The first media range of the request's `accept', by q-value and then by
%% order, that the node can serve gives the codec of the reply.
accept_precedence_test() ->
    Opts = test_opts(),
    Codec =
        fun(Accept, Msg) ->
            {Res, _} = accept_to_codec(#{ <<"accept">> => Accept }, Msg, Opts),
            Res
        end,
    ?assertEqual(<<"json@1.0">>, Codec(<<"application/x.ao.json">>, #{})),
    ?assertEqual(<<"json@1.0">>, Codec(<<"application/x.ao.json.1">>, #{})),
    ?assertEqual(<<"json@1.0">>, Codec(<<"application/x.ao.json.1.0">>, #{})),
    ?assertEqual(
        <<"ans104@1.0">>,
        Codec(<<"*/*;q=0.5, application/ans104">>, #{})
    ),
    ?assertEqual(
        <<"json@1.0">>,
        Codec(<<"text/plain, application/x.ao.json;q=0.5">>, #{})
    ),
    ?assertEqual(
        <<"json@1.0">>,
        Codec(<<"application/json, text/plain, */*">>, #{})
    ),
    ?assertEqual(
        <<"tx@1.0">>,
        Codec(<<"application/x.ao.none, application/x.ao.tx">>, #{})
    ),
    % A device that is not a codec is passed over.
    ?assertEqual(
        <<"httpsig@1.0">>,
        Codec(<<"application/x.ao.lua.5.3a">>, #{})
    ),
    % A range of the result's own `content-type' sends the result's body.
    ?assertEqual(
        <<"httpsig@1.0">>,
        Codec(
            <<"application/json, */*">>,
            #{ <<"content-type">> => <<"application/json">> }
        )
    ).

cors_get_test() ->
    % The node's request hook asks for the reply in `json@1.0'.
    Hook =
        fun(_, HookReq = #{ <<"request">> := Req }, _) ->
            {ok, HookReq#{
                <<"request">> => Req#{ <<"accept">> => <<"application/json">> }
            }}
        end,
    URL =
        start_node(#{
            <<"on">> => #{
                <<"request">> => #{ <<"device">> => #{ <<"request">> => Hook } }
            }
        }),
    % The CORS headers are headers of the HTTP reply, not keys of its message.
    {ok, 200, Headers, Body} =
        hb_http_client:request(
            #{
                peer => URL,
                path => <<"/~meta@1.0/info">>,
                method => <<"GET">>,
                headers => #{},
                body => <<>>
            },
            test_opts()
        ),
    ?assertEqual(
        <<"*">>,
        proplists:get_value(<<"access-control-allow-origin">>, Headers)
    ),
    % The reply is encoded for the request that the request hook returns.
    ?assertEqual(
        <<"application/json">>,
        proplists:get_value(<<"content-type">>, Headers)
    ),
    % The reply carries the length of its body, so a cut reply is an error.
    ?assertEqual(
        hb_util:bin(byte_size(Body)),
        proplists:get_value(<<"content-length">>, Headers)
    ).

index_test() ->
    NodeURL = start_node(),
    % The index page is the body of the HTTP reply, not a key of its message.
    {ok, 200, _, Body} =
        hb_http_client:request(
            #{
                peer => NodeURL,
                path => <<"/~test-device@1.0/load">>,
                method => <<"GET">>,
                headers => #{ <<"accept-bundle">> => <<"false">> },
                body => <<>>
            },
            #{}
        ),
    ?assertEqual(<<"i like turtles!">>, Body),
    % The `index' of a message does not commit with the node's key.
    {ok, ID} =
        hb_cache:write(
            #{
                <<"device">> => <<"lua@5.3a">>,
                <<"module">> =>
                    #{
                        <<"content-type">> => <<"application/lua">>,
                        <<"body">> =>
                            <<
                                "function index() return { body = "
                                "ao.resolve({ device = 'message@1.0' }, "
                                "{ path = 'commit' }) } end"
                            >>
                    }
            },
            get_opts()
        ),
    {ok, 200, _, CommitStatus} =
        hb_http_client:request(
            #{
                peer => NodeURL,
                path => <<"/", ID/binary>>,
                method => <<"GET">>,
                headers => #{ <<"accept-bundle">> => <<"false">> },
                body => <<>>
            },
            #{}
        ),
    ?assertNotEqual(<<"ok">>, CommitStatus).

index_request_test() ->
    URL = start_node(),
    {ok, 200, _, Body} =
        hb_http_client:request(
            #{
                peer => URL,
                path => <<"/~test-device@1.0/load?name=dogs">>,
                method => <<"GET">>,
                headers => #{ <<"accept-bundle">> => <<"false">> },
                body => <<>>
            },
            #{}
        ),
    ?assertEqual(<<"i like dogs!">>, Body).

%% @doc A query parameter overrides a request header of the same name, so the
%% keys that a request sets in its path keep working beside headers.
query_overrides_header_test() ->
    Node = start_node(),
    {ok, Res} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"//accept?accept=from-query">>,
                <<"accept">> => <<"from-header">>,
                <<"accept-bundle">> => false
            },
            #{}
        ),
    ?assertEqual(<<"from-query">>, Res).

%% @doc A signed request's query keys apply over its signed content after its
%% signature is verified. Its uncommitted headers do not reach its execution.
signed_request_query_riders_test() ->
    Opts = test_opts(),
    Node = start_node(),
    Signed =
        hb_message:commit(
            #{ <<"x">> => <<"original">> },
            Opts,
            #{ <<"committed">> => [<<"x">>] }
        ),
    % A query key that the request does not carry is added to it.
    ?assertEqual(
        {ok, <<"1">>},
        hb_http:request(<<"GET">>, Node, <<"//y?y=1">>, Signed, Opts)
    ),
    % The same key sent as an uncommitted header is dropped.
    ?assertMatch(
        {error, _},
        hb_http:request(
            <<"GET">>,
            Node,
            <<"//y">>,
            Signed#{ <<"y">> => <<"7">> },
            Opts
        )
    ),
    % A query key that shares a signed key's name replaces the signed value,
    % and the signature, verified over the signed value, holds.
    ?assertEqual(
        {ok, <<"test">>},
        hb_http:request(<<"GET">>, Node, <<"//x?x=test">>, Signed, Opts)
    ),
    % A query key is lowercase, so one that differs only in case replaces it
    % too.
    ?assertEqual(
        {ok, <<"test">>},
        hb_http:request(<<"GET">>, Node, <<"//x?X=test">>, Signed, Opts)
    ),
    % A `device' header does not replace the `device' of a signed message.
    SignedDevice =
        hb_message:commit(
            #{ <<"device">> => <<"message@1.0">>, <<"x">> => <<"original">> },
            Opts
        ),
    ?assertEqual(
        {ok, <<"original">>},
        hb_http:post(
            Node,
            #{
                <<"path">> => <<"//x">>,
                <<"device">> => <<"meta@1.0">>,
                <<"content-type">> => <<"application/json">>,
                <<"body">> =>
                    hb_message:convert(SignedDevice, <<"json@1.0">>, Opts)
            },
            Opts
        )
    ).
