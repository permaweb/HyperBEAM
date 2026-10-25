%%% @doc Reverse HTTP tunnel for nodes behind one-way networks.
%%%
%%% A node signs a long-poll `register' request with its wallet. The broker
%%% returns a request, which the node executes through its HTTP request hooks,
%%% then posts to `response'. Each request is dispatched once. A timeout means
%%% execution may have occurred; the tunnel never retries application work.
%%%
%%% Install `request' on `on/request' to route a wallet's DNS URI host or
%%% an explicit `tunnel-target'. `call' accepts the target and a nested request.
%%% Set `tunnel-host' to restrict host routing to that domain's direct children.
%%% Hosts without a connected target pass unchanged to the next request handler.
%%% `connect' starts, resizes or stops an outbound worker pool, for the node
%%% itself or with the operator's signature. `status' reports waiting streams,
%%% requests and workers for this node.
%%%
%%% Application messages are loaded at their source and carried unchanged
%%% inside signed envelopes. A response is the message that the origin's HTTP
%%% server replies with, committed as that server commits it. The origin posts
%%% it to `response' with its Erlang result tag.
-module(dev_tunnel).
-specification("../../../specs/tunnel@1.0.md").
-export([device_info/1, register/3, response/3, call/3, request/3]).
-export([status/3, connect/3]).
-include("include/hb.hrl").

%% @doc Export only the device's public interface.
device_info(_) ->
    #{
        exports =>
            [
                <<"register">>,
                <<"response">>,
                <<"call">>,
                <<"request">>,
                <<"status">>,
                <<"connect">>
            ]
    }.

%% @doc Wait with a signed registration until a request for its address
%% arrives, or until the registration expires.
register(_Base, Req, Opts) ->
    maybe
        {ok, Address, Control} ?= authenticate(Req, [<<"address">>], Opts),
        true ?=
            Address =:= hb_maps:get(<<"address">>, Control, none, Opts) orelse
                denied(),
        Start = erlang:monotonic_time(second),
        Result =
            dev_tunnel_server:register(
                broker(Opts),
                Address,
                timeout(tunnel_register_timeout, 45_000, #{}, Control, Opts)
            ),
        meter(Start, Opts),
        case Result of
            {ok, #{ <<"status">> := 204 }} -> Result;
            {ok, Work} -> {ok, sign(Work#{ <<"status">> => 200 }, Opts)};
            Error -> Error
        end
    end.

%% @doc Accept a response only from the node to which the request was sent, and
%% give the caller the origin's reply with its Erlang result tag
%% (`result-status').
response(_Base, Req, Opts) ->
    maybe
        {ok, Address, Control} ?=
            authenticate(
                Req,
                [<<"tunnel-request-id">>, <<"result-status">>, <<"response">>],
                Opts
            ),
        ID = hb_maps:get(<<"tunnel-request-id">>, Control, none, Opts),
        Response = hb_maps:get(<<"response">>, Control, none, Opts),
        {ok, accepted} ?=
            dev_tunnel_server:response(
                broker(Opts),
                ID,
                Address,
                {
                    hb_util:atom(
                        hb_maps:get(<<"result-status">>, Control, none, Opts)
                    ),
                    maybe_add_unsigned(Response, Opts)
                }
            ),
        {ok, #{ <<"status">> => 202, <<"tunnel-request-id">> => ID }}
    end.

%% @doc Queue an explicit request, including when its target is not connected.
call(Base, Req, Opts) ->
    maybe
        {ok, Address} ?=
            address(value(<<"tunnel-target">>, Base, Req, none, Opts)),
        Request = value(<<"request">>, Base, Req, none, Opts),
        true ?=
            Request =/= none orelse
                bad_request(<<"A tunnel request is required.">>),
        dev_tunnel_server:call(
            broker(Opts),
            Address,
            load(Request, Opts),
            response_timeout(Base, Req, Opts),
            explicit
        )
    end.

%% @doc Route a raw singleton, leaving unrelated hooks and payloads untouched.
request(Base, Hook, Opts) ->
    Raw = hb_maps:get(<<"request">>, Hook, #{}, Opts),
    {Path, _Query} = split_path(Raw, Opts),
    Target =
        case hb_path:term_to_path_parts(Path, Opts) of
            [<<"~tunnel@1.0">>, <<"call">>] -> call_here;
            _ -> target(Base, Raw, Opts)
        end,
    case Target of
        % Reply to an explicit call from the hook, as to a forwarded call, so
        % the HTTP path does not add the broker's signature to the origin's reply.
        call_here -> {reply, call(Base, Raw, Opts)};
        none -> {ok, Hook};
        {error, _} = Error -> Error;
        {Mode, Address} ->
            Broker = broker(Opts),
            maybe
                true ?=
                    Mode =:= explicit orelse
                        dev_tunnel_server:connected(Broker, Address) orelse
                        {ok, Hook},
                {ok, Forwarded} ?= forward(Mode, Raw, Opts),
                Reply =
                    dev_tunnel_server:call(
                        Broker,
                        Address,
                        Forwarded,
                        response_timeout(Base, Raw, Opts),
                        Mode
                    ),
                {reply, Reply}
            end
    end.

%% @doc Report only this HTTP node's broker state.
status(_Base, _Req, Opts) ->
    dev_tunnel_server:status(broker(Opts)).

%% @doc Start, resize or stop a pool. A pool of no workers stops once its
%% workers end their current registrations.
connect(Base, Req, Opts) ->
    maybe
        {ok, Control} ?= connect_control(Base, Req, Opts),
        Peer = hb_maps:get(<<"peer">>, Control, none, Opts),
        true ?=
            is_binary(Peer) orelse
                bad_request(<<"A tunnel peer is required.">>),
        Workers = integer(hb_maps:get(<<"workers">>, Control, 1, Opts)),
        Max = integer(hb_opts:get(tunnel_max_workers, 32, Opts)),
        true ?=
            is_integer(Workers) andalso Workers >= 0 andalso
                (Max =:= infinity orelse
                    (is_integer(Max) andalso Workers =< Max)) orelse
                bad_request(<<"Invalid tunnel worker count.">>),
        Address = hb_opts:address(Opts),
        Given = hb_maps:get(<<"address">>, Control, Address, Opts),
        true ?=
            Given =:= Address orelse
                bad_request(
                    <<"The tunnel address must be this node's wallet.">>
                ),
        dev_tunnel_server:connect(
            broker(Opts),
            Peer,
            Address,
            Workers,
            integer(hb_opts:get(tunnel_max_client_workers, 32, Opts)),
            integer(hb_opts:get(tunnel_client_retire_timeout, 300_000, Opts)),
            fun(Stop) -> client(Peer, Address, Stop, Opts) end
        )
    end.

%% @doc Return the controls of a `connect'. A caller whose options contain the
%% node's wallet is the node itself, such as an `on/start' hook, and gives its
%% controls unsigned. Any other caller, such as an HTTP request or code that a
%% request runs, must give controls that the node's operator signed.
connect_control(Base, Req, Opts) ->
    case
        hb_opts:get(priv_wallet, no_viable_wallet, Opts#{ <<"only">> => local })
    of
        no_viable_wallet ->
            maybe
                {ok, _Signer, Control} ?=
                    authenticate(Req, [<<"peer">>], Opts),
                {ok, IsOperator} =
                    hb_ao:resolve(
                        #{ <<"device">> => <<"meta@1.0">> },
                        #{
                            <<"path">> => <<"is-operator">>,
                            <<"body">> => Control
                        },
                        Opts#{ <<"hashpath">> => ignore }
                    ),
                true ?= IsOperator orelse denied(),
                {ok, Control}
            end;
        _ -> {ok, hb_maps:merge(Base, Req, Opts)}
    end.

%% @doc Select a signed envelope covering all required controls. Unsigned
%% additions cannot change its peer, worker count, request ID or payload.
authenticate(Req, Required, Opts) ->
    try
        Loaded = load(hb_private:get(<<"http-request">>, Req, Req, Opts), Opts),
        Candidates =
            [
                {Signer,
                    (hb_maps:with(Keys, Loaded, Opts))#{
                        <<"commitments">> => #{ ID => Commitment }
                    }}
            ||
                {ID, Commitment = #{ <<"committer">> := Committer }} <-
                    hb_maps:to_list(
                        hb_maps:get(<<"commitments">>, Loaded, #{}, Opts)
                    ),
                Signer <- [hb_util:committer_address(Committer)],
                ?IS_ID(Signer),
                Keys <- [hb_message:committed(Loaded, [ID], Opts)],
                Required -- Keys =:= []
            ],
        case Candidates of
            [{Signer, Control} | _] -> {ok, Signer, Control};
            [] -> denied()
        end
    catch Class:Reason ->
        ?event(tunnel, {control_rejected, Class, Reason}),
        denied()
    end.

%% @doc Load a message and its links, and check that its commitments, and those
%% of the messages that it contains, verify.
load(Message, Opts) ->
    Loaded = hb_cache:ensure_all_loaded(Message, Opts),
    true = hb_message:deep_verify(Loaded, Opts),
    Loaded.

%% @doc Sign a control message over all of its keys. It is signed as a bundle,
%% so that the messages that it contains are not written to the node's store.
sign(Message, Opts) ->
    Public = hb_private:reset(load(Message, Opts)),
    hb_message:commit(
        Public,
        Opts,
        #{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"committed">> => maps:keys(hb_message:uncommitted(Public, Opts)),
            <<"bundle">> => true
        }
    ).

%% @doc If the only commitments of a reply sign its hashpath, add its unsigned
%% commitment, over its keys other than `hashpath', as the origin's HTTP server
%% gives it: the response message carries the reply with only its signed
%% commitments (`dev_httpsig_conv:to/4').
maybe_add_unsigned(Reply, Opts) ->
    case hb_message:committed(Reply, #{ <<"ids">> => <<"any">> }, Opts) of
        [<<"hashpath">>] ->
            #{ <<"commitments">> := Unsigned } =
                hb_message:commit(
                    maps:without([<<"commitments">>, <<"hashpath">>], Reply),
                    Opts,
                    #{
                        <<"commitment-device">> => <<"httpsig@1.0">>,
                        <<"type">> => <<"unsigned">>
                    }
                ),
            Reply#{
                <<"commitments">> =>
                    maps:merge(maps:get(<<"commitments">>, Reply), Unsigned)
            };
        _ -> Reply
    end.

%% @doc Prefer the wallet-address host to an explicit target or query argument.
target(Base, Req, Opts) ->
    Host = hb_maps:get(<<"host">>, Req, <<>>, Opts),
    HostAddress =
        try
            #{host := Hostname} = uri_string:parse(<<"//", Host/binary>>),
            [Label | Suffix] =
                binary:split(
                    string:trim(string:lowercase(Hostname), trailing, "."),
                    <<".">>
                ),
            Parent =
                hb_maps:get(
                    <<"host">>,
                    Base,
                    hb_opts:get(tunnel_host, none, Opts),
                    Opts
                ),
            true = Parent =:= none orelse Suffix =:= [Parent],
            <<"uri-did-ar-", _/binary>> = Label,
            {ok, ID} = hb_committer:address(Label),
            ID
        catch _:_ -> none
        end,
    case HostAddress of
        none ->
            {_Path, Query} = split_path(Req, Opts),
            Target =
                case value(<<"tunnel-target">>, Base, Req, none, Opts) of
                    none ->
                        proplists:get_value(<<"tunnel-target">>, Query, none);
                    Explicit -> Explicit
                end,
            case Target of
                none -> none;
                _ ->
                    case address(Target) of
                        {ok, ID2} -> {explicit, ID2};
                        Error -> Error
                    end
            end;
        _ -> {host, HostAddress}
    end.

%% @doc Remove routing controls only when doing so preserves all commitments. A
%% `host' that routes a request is removed, as an origin with `name@1.0' would
%% read its first label as a name, unless a commitment of the request covers it.
forward(Mode, Raw, Opts) ->
    try
        Keys = [<<"tunnel-target">>, <<"tunnel-timeout">>],
        Loaded = load(Raw, Opts),
        Committed =
            hb_message:committed(Loaded, #{ <<"ids">> => <<"any">> }, Opts),
        Removed =
            case {Mode, lists:member(<<"host">>, Committed)} of
                {host, false} -> [<<"host">> | Keys];
                _ -> Keys
            end,
        Clean = maps:without(Removed, Loaded),
        {Path, Pairs} = split_path(Clean, Opts),
        Forwarded =
            case [{K, V} || {K, V} <- Pairs, not lists:member(K, Keys)] of
                Pairs -> Clean;
                [] -> Clean#{ <<"path">> => Path };
                Kept ->
                    Query = uri_string:compose_query(Kept),
                    Clean#{ <<"path">> => <<Path/binary, "?", Query/binary>> }
            end,
        true = hb_message:deep_verify(Forwarded, Opts),
        {ok, Forwarded}
    catch
        _:_ ->
            bad_request(
                <<"Tunnel routing must preserve the request commitments.">>
            )
    end.

%% @doc Split a request's path at its first `?', as the HTTP server does, into
%% the path and the arguments of its query. A query that does not parse gives
%% no arguments.
split_path(Req, Opts) ->
    case binary:split(hb_maps:get(<<"path">>, Req, <<>>, Opts), <<"?">>) of
        [Path, Query] ->
            case uri_string:dissect_query(Query) of
                Pairs when is_list(Pairs) -> {Path, Pairs};
                _ -> {Path, []}
            end;
        [Path] -> {Path, []}
    end.

%% @doc Require a real 256-bit wallet address.
address(Value) ->
    try
        Native = hb_util:native_id(Value),
        32 = byte_size(Native),
        {ok, hb_util:human_id(Native)}
    catch _:_ -> bad_request(<<"Invalid tunnel address.">>)
    end.

%% @doc Read a request override before its base-message default.
value(Key, Base, Req, Default, Opts) ->
    hb_maps:get_first([{Req, Key}, {Base, Key}], Default, Opts).

%% @doc Bound a request's response wait by the node's own
%% `tunnel-response-timeout', so a request cannot occupy a worker past the
%% node's limit, nor ask it to wait for `infinity'. A request's `infinity', or
%% a wait past the limit, becomes the limit.
response_timeout(Base, Req, Opts) ->
    Limit = integer(hb_opts:get(tunnel_response_timeout, 300_000, Opts)),
    shorter(timeout(tunnel_response_timeout, 300_000, Base, Req, Opts), Limit).

%% @doc Return the shorter of a request's wait and the node's limit, treating
%% `infinity' as the longer. An invalid wait passes through for the broker to
%% refuse.
shorter(infinity, Limit) -> Limit;
shorter(Wait, infinity) -> Wait;
shorter(Wait, Limit) when is_integer(Wait), is_integer(Limit) ->
    min(Wait, Limit);
shorter(Wait, _Limit) -> Wait.

%% @doc Let the broker validate timeouts before registering state.
timeout(Key, Default, Base, Req, Opts) ->
    {_Path, Query} = split_path(Req, Opts),
    integer(
        value(
            <<"tunnel-timeout">>,
            Base,
            Req,
            proplists:get_value(
                <<"tunnel-timeout">>,
                Query,
                hb_opts:get(Key, Default, Opts)
            ),
            Opts
        )
    ).

%% @doc Normalize numeric HTTP controls. The broker refuses an invalid value.
integer(infinity) -> infinity;
integer(<<"infinity">>) -> infinity;
integer(Value) ->
    try hb_util:int(Value)
    catch _:_ -> invalid
    end.

%% @doc Isolate brokers by HTTP node, including in a multi-server BEAM.
broker(Opts) ->
    case hb_opts:get(http_server, none, Opts) of
        none ->
            hb_util:human_id(
                ar_wallet:to_address(
                    hb_opts:get(priv_wallet, no_viable_wallet, Opts)
                )
            );
        Server -> Server
    end.

%% @doc Charge the seconds that a registration waited to `metering@1.0', as
%% `tunnel-seconds'. Outside a metering session this does nothing.
meter(Start, Opts) ->
    {ok, Metering} = hb_device_load:reference(<<"metering@1.0">>, Opts),
    Metering:consume(
        <<"tunnel-seconds">>,
        erlang:monotonic_time(second) - Start,
        Opts
    ).

%% @doc Stop between requests. Reconnection never replays application work.
client(Peer, Address, Stop, Opts) ->
    receive
        {tunnel_client_stop, Stop} -> ok
    after 0 ->
        try
            Current = node_opts(Opts),
            Timeout =
                integer(hb_opts:get(tunnel_register_timeout, 45_000, Current)),
            SendTimeout =
                case Timeout of
                    infinity -> infinity;
                    _ -> Timeout + 5_000
                end,
            HTTPOpts =
                Current#{
                    <<"http-only-result">> => false,
                    <<"http-client-send-timeout">> => SendTimeout,
                    <<"http-client-hackney-recv-timeout">> => SendTimeout
                },
            Registration =
                sign(
                    #{
                        <<"codec-device">> => <<"httpsig@1.0">>,
                        <<"address">> => Address,
                        <<"tunnel-timeout">> => Timeout
                    },
                    Current
                ),
            case
                hb_http:post(
                    Peer,
                    <<"/~tunnel@1.0/register">>,
                    Registration,
                    HTTPOpts
                )
            of
                {ok, #{ <<"status">> := 204 }} -> ok;
                {ok, Work} ->
                    {ok, _, Control} =
                        authenticate(
                            Work,
                            [
                                <<"tunnel-request-id">>,
                                <<"tunnel-target">>,
                                <<"request">>
                            ],
                            Current
                        ),
                    Address =
                        hb_maps:get(
                            <<"tunnel-target">>,
                            Control,
                            none,
                            Current
                        ),
                    ID =
                        hb_maps:get(
                            <<"tunnel-request-id">>,
                            Control,
                            none,
                            Current
                        ),
                    {Status, Response} =
                        execute(
                            hb_maps:get(<<"request">>, Control, none, Current),
                            node_opts(Current)
                        ),
                    Envelope =
                        sign(
                            #{
                                <<"codec-device">> => <<"httpsig@1.0">>,
                                <<"tunnel-request-id">> => ID,
                                <<"result-status">> => Status,
                                <<"response">> => Response
                            },
                            Current
                        ),
                    % A response is posted again after a server error or a
                    % failed connection, up to 3 times, waiting longer each
                    % time. A broker answers a response that it has delivered
                    % with 404, which is not posted again.
                    ResponseOpts =
                        HTTPOpts#{
                            <<"http-retry">> => 3,
                            <<"http-retry-response">> => [failure],
                            <<"http-retry-time">> =>
                                hb_opts:get(tunnel_retry_time, 1_000, Current)
                        },
                    case
                        hb_http:post(
                            Peer,
                            <<"/~tunnel@1.0/response">>,
                            Envelope,
                            ResponseOpts
                        )
                    of
                        {ok, #{ <<"status">> := 202 }} -> ok;
                        Error ->
                            ?event(
                                tunnel,
                                {response_rejected, ID, {explicit, Error}}
                            )
                    end;
                Error -> error({'registration-failed', Error})
            end
        catch
            Class:Reason ->
                ?event(tunnel, {client_failed, Class, Reason}),
                timer:sleep(hb_opts:get(tunnel_retry_time, 1_000, Opts))
        end,
        client(Peer, Address, Stop, Opts)
    end.

%% @doc Execute through the node's normal HTTP request and response hooks, and
%% return the Erlang result tag and the message of the reply. An exception gets
%% the reply that the HTTP server gives it (`hb_http_server:error_reply/4').
execute(Request, Opts) ->
    try
        Singleton = singleton(Request, Opts),
        [Path | _] =
            binary:split(
                hb_maps:get(<<"path">>, Singleton, <<"/">>, Opts),
                <<"?">>
            ),
        Result =
            maybe
                true ?=
                    not hb_opts:get(force_signed_requests, false, Opts) orelse
                        hb_message:signers(Singleton, Opts) =/= [] orelse
                        bad_request(
                            <<"A signed application request is required.">>
                        ),
                {_, HTTPResult} =
                    hb_http_server:resolve_request(Path, Singleton, Opts),
                HTTPResult
            end,
        reply(Singleton, Result, Opts)
    catch
        Class:Reason:Stacktrace ->
            ?event(tunnel, {execution_failed, Class, Reason}),
            hb_http_server:error_reply(Class, Reason, Stacktrace, Opts)
    end.

%% @doc Return the Erlang result tag and the message that the HTTP server
%% replies to a result with, committed as it commits its reply to the request
%% (`hb_http_server:commit_reply/3'). A list is the `body' of a message whose
%% `ao-result' is `body'. If a request hook gives the reply (`{reply, Result}'),
%% reply to `Result'. The message is loaded, as the caller cannot reach this
%% node to load its links.
reply(Request, {reply, Result}, Opts) -> reply(Request, Result, Opts);
reply(Request, {Status, List}, Opts) when is_list(List) ->
    reply(
        Request,
        {Status, #{ <<"ao-result">> => <<"body">>, <<"body">> => List }},
        Opts
    );
reply(Request, Result, Opts) ->
    {Status, Message} = hb_ao:force_message(Result, Opts),
    Committed =
        hb_http_server:commit_reply(
            Request,
            hb_ao:normalize_keys(Message, Opts),
            Opts
        ),
    {Status, load(hb_private:reset(Committed), Opts)}.

%% @doc Accept either a raw singleton or the canonical sequence form.
singleton(Request, Opts) when is_map(Request) ->
    case hb_util:is_ordered_list(Request, Opts) of
        true -> hb_singleton:to(hb_util:message_to_ordered_list(Request, Opts));
        false -> Request
    end;
singleton(Request, _Opts) -> hb_singleton:to(Request).

%% @doc Refresh boot-time options once the node's listener is available.
node_opts(Opts) ->
    Server = broker(Opts),
    hb_http_server:set_proc_server_id(Server),
    hb_http_server:get_opts(#{ <<"http-server">> => Server }).

%% @doc Report malformed controls without changing broker state.
bad_request(Body) -> {error, #{ <<"status">> => 400, <<"body">> => Body }}.

%% @doc Report a missing, invalid, or insufficiently scoped signature.
denied() ->
    {error, #{
        <<"status">> => 403,
        <<"body">> => <<"Tunnel control signature required.">>
    }}.
