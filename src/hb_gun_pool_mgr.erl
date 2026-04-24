%%% @doc Juggler pool manager — one gen_server per {Authority, Scope}.
%%%
%%% Owns up to max_sockets gun connections. Callers call request/6 and then
%%% receive demuxed gun messages in their own mailbox. No per-request
%%% worker process on our side; gun connections are the concurrency unit.
%%%
%%% Concurrency model:
%%%   h2: one connection serves ~100 concurrent streams (server-advertised).
%%%   h1: one connection serves one stream at a time; pool up to max_sockets.
%%%
%%% Circuit breaker: inc consec_connect_failures on connect timeout; at
%%% threshold, set cooldown_until and reject all requests immediately.
%%%
%%% Idle eviction: after two consecutive idle_check ticks with no traffic
%%% the manager terminates itself and removes its ETS entry.
%%%
-module(hb_gun_pool_mgr).
-behaviour(gen_server).

-export([start_link/6, request/6, cancel_stream/2, abandon_queued/2,
         normalize_tls_opts/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("include/hb.hrl").
-include("include/hb_http_client.hrl").

-define(DEFAULT_MAX_SOCKETS_H1, 16).
-define(DEFAULT_MAX_SOCKETS_H2, 2).
-define(DEFAULT_MAX_QUEUED_PER_CALLER, 8).
-define(MAX_CONNECT_FAILURES, 3).
-define(COOLDOWN_MS, 30_000).
-define(IDLE_POLL_MS, 30_000).
-define(CONNECT_TIMEOUT_TIMER_MSG, connect_timeout_check).

-record(conn, {
    pid              :: pid(),
    protocol         = connecting :: connecting | http | http2,
    streams_in_flight = 0         :: non_neg_integer(),
    max_streams       = 1         :: pos_integer(),
    opened_at         = 0         :: integer(),
    last_used         = 0         :: integer(),
    connect_timer               :: reference() | undefined
}).

-record(state, {
    authority          :: binary(),
    scope              :: term(),
    host               :: string(),
    port               :: inet:port_number(),
    transport          :: tcp | tls,
    opts               :: map(),
    protocol           :: http | http1 | http2 | http3,
    max_sockets        :: pos_integer(),
    max_queued         :: pos_integer(),
    idle_poll_ms       :: pos_integer(),
    gun_module         = gun :: module(),
    conns              = #{} :: #{pid() => #conn{}},
    streams            = #{} :: #{
        reference() => {pid(), pid()} | {abandoned, pid()}
    },
    caller_refs        = #{} :: #{pid() => reference()},
    %% per-caller queue of {ReqArgs, CallerPid, From} triples
    queue              = []  :: list(),
    consec_connect_failures = 0 :: non_neg_integer(),
    cooldown_until     = undefined :: integer() | undefined,
    was_idle           = false :: boolean()
}).

%% @doc Start and link a juggler manager. Spawned by hb_gun_pool supervisor.
start_link(Authority, Scope, Host, Port, Transport, Opts) ->
    gen_server:start_link(
        ?MODULE,
        {Authority, Scope, Host, Port, Transport, Opts},
        []
    ).

%% @doc Submit a request to the pool. Returns {ok, MgrPid, StreamRef} on
%% acceptance; caller then awaits demuxed gun messages in its mailbox.
%% Returns {error, Reason} immediately on backpressure or cooldown.
request(MgrPid, Method, Path, Headers, Body, Opts) ->
    %% Bound the call to a reasonable ceiling independent of connect_timeout.
    %% Orphaned workers (e.g. after an eunit test is killed) otherwise pin
    %% pool resources for the full connect_timeout (60s default), stalling
    %% subsequent tests. 10s is ample for local/internal peers and long
    %% enough for most real handshake paths.
    QueueCeiling = hb_opts:get(http_client_gun_call_timeout, 10_000, Opts),
    ReqRef = make_ref(),
    ReqArgs = #{ref => ReqRef, method => Method, path => Path,
                headers => Headers, body => Body},
    try gen_server:call(MgrPid, {request, ReqArgs, self()}, QueueCeiling) of
        {ok, StreamRef} -> {ok, MgrPid, StreamRef};
        {error, _} = Err -> Err
    catch
        exit:{timeout, _}  ->
            abandon_queued(MgrPid, ReqRef),
            {error, timeout};
        exit:{noproc, _}   -> {error, noproc}
    end.

%% @doc Ask the manager to stop forwarding a stream to its caller. Used when
%% the caller has given up on a response (e.g. send_timeout).
cancel_stream(MgrPid, StreamRef) ->
    gen_server:cast(MgrPid, {cancel_stream, StreamRef}).

%% @doc Ask the manager to drop any queued request matching ReqRef. Used
%% when a caller's gen_server:call timed out before the request was issued.
abandon_queued(MgrPid, ReqRef) ->
    gen_server:cast(MgrPid, {abandon_queued, ReqRef}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({Authority, Scope, Host, Port, Transport, RawOpts}) ->
    process_flag(trap_exit, true),
    %% Normalize atom/list keys to the canonical binary-dash form so reads
    %% via hb_opts:get find both node config and test-injected overrides.
    Opts = hb_opts:mimic_default_types(RawOpts, existing, RawOpts),
    Protocol = resolve_protocol(Transport, Opts),
    MaxSockets = max_sockets(Protocol, Opts),
    MaxQueued = hb_opts:get(
        http_client_gun_max_queued_per_caller, ?DEFAULT_MAX_QUEUED_PER_CALLER, Opts),
    IdlePollMs = hb_opts:get(gun_pool_idle_poll_ms, ?IDLE_POLL_MS, Opts),
    GunModule = hb_opts:get(gun_module, gun, Opts),
    erlang:send_after(IdlePollMs, self(), {?MODULE, idle_check}),
    {ok, #state{
        authority   = Authority,
        scope       = Scope,
        host        = hb_util:list(Host),
        port        = Port,
        transport   = Transport,
        opts        = Opts,
        protocol    = Protocol,
        max_sockets = MaxSockets,
        max_queued  = MaxQueued,
        idle_poll_ms = IdlePollMs,
        gun_module  = GunModule
    }}.

handle_call({request, ReqArgs, CallerPid}, From,
            State = #state{cooldown_until = Until}) ->
    Now = erlang:monotonic_time(millisecond),
    case is_integer(Until) andalso Until > Now of
        true ->
            {reply, {error, no_connection_available}, State};
        false ->
            handle_request(ReqArgs, CallerPid, From, State)
    end;

handle_call(pool_info, _From, State = #state{conns = Cs, queue = Q}) ->
    Up = maps:fold(
        fun(_, #conn{protocol = P}, A) when P =/= connecting -> A + 1;
           (_, _, A) -> A end,
        0, Cs),
    InFlight = maps:fold(
        fun(_, #conn{streams_in_flight = N}, A) -> A + N end, 0, Cs),
    {reply, #{workers_up => Up, inflight => InFlight, queued => length(Q)}, State};

handle_call(Req, _From, State) ->
    ?event(warning, {unhandled_call, {module, ?MODULE}, {request, Req}}),
    {reply, ok, State}.

handle_cast({cancel_stream, StreamRef},
            State = #state{streams = Streams}) ->
    %% Forget the stream so subsequent gun_data / gun_response / gun_error
    %% messages for it are dropped rather than forwarded. Do NOT send gun:cancel
    %% (RST_STREAM) here: under h2 the server may already have HEADERS in flight
    %% for this stream, and RSTing races with that delivery. Gun then emits
    %% stream_closed errors on the shared connection that can be mis-attributed
    %% to later, unrelated streams. Since we no longer forward data to the
    %% caller, letting the stream drain naturally is both correct and safe.
    %% The pool only frees the socket once gun reports the stream ended.
    case maps:find(StreamRef, Streams) of
        {ok, {CallerPid, ConnPid}} when is_pid(CallerPid) ->
            {noreply, abandon_stream(StreamRef, ConnPid, State)};
        {ok, {abandoned, _ConnPid}} ->
            {noreply, State};
        error ->
            {noreply, State}
    end;
handle_cast({abandon_queued, ReqRef},
            State = #state{queue = Q}) ->
    Q2 = [E || E = {#{ref := R}, _, _} <- Q, R =/= ReqRef],
    {noreply, State#state{queue = Q2}};
handle_cast(Cast, State) ->
    ?event(warning, {unhandled_cast, {module, ?MODULE}, {cast, Cast}}),
    {noreply, State}.

%% Connection came up.
handle_info({gun_up, ConnPid, Proto},
            State = #state{conns = Cs}) ->
    case maps:find(ConnPid, Cs) of
        error ->
            {noreply, State};
        {ok, Conn} ->
            cancel_connect_timer(Conn),
            MaxStreams = case Proto of
                http2 -> 100;
                _     -> 1
            end,
            Conn2 = Conn#conn{
                protocol          = Proto,
                max_streams       = MaxStreams,
                connect_timer     = undefined
            },
            State2 = State#state{
                conns                   = Cs#{ConnPid => Conn2},
                consec_connect_failures = 0
            },
            {noreply, drain_queue(State2)}
    end;

%% Server advertises new max_concurrent_streams for h2.
handle_info({gun_notify, ConnPid, settings_changed,
             #{max_concurrent_streams := MaxS}},
            State = #state{conns = Cs}) ->
    case maps:find(ConnPid, Cs) of
        error ->
            {noreply, State};
        {ok, Conn} ->
            {noreply, State#state{conns = Cs#{ConnPid => Conn#conn{max_streams = MaxS}}}}
    end;
handle_info({gun_notify, _, _, _}, State) ->
    {noreply, State};

%% Demux: stream-level response messages — forward to caller using self()
%% as the pid so the caller's receive pattern matches on the manager pid.
handle_info({gun_response, ConnPid, StreamRef, IsFin, Status, Headers},
            State = #state{streams = Streams}) ->
    case maps:find(StreamRef, Streams) of
        error ->
            {noreply, State};
        {ok, {CallerPid, ConnPid}} ->
            CallerPid ! {gun_response, self(), StreamRef, IsFin, Status, Headers},
            State2 = case IsFin of
                fin  -> stream_done(StreamRef, ConnPid, State);
                nofin -> State
            end,
            {noreply, State2};
        {ok, {abandoned, ConnPid}} ->
            State2 = case IsFin of
                fin  -> stream_done(StreamRef, ConnPid, State);
                nofin -> State
            end,
            {noreply, State2}
    end;

handle_info({gun_data, ConnPid, StreamRef, IsFin, Data},
            State = #state{streams = Streams}) ->
    case maps:find(StreamRef, Streams) of
        error ->
            {noreply, State};
        {ok, {CallerPid, ConnPid}} ->
            CallerPid ! {gun_data, self(), StreamRef, IsFin, Data},
            State2 = case IsFin of
                fin  -> stream_done(StreamRef, ConnPid, State);
                nofin -> State
            end,
            {noreply, State2};
        {ok, {abandoned, ConnPid}} ->
            State2 = case IsFin of
                fin  -> stream_done(StreamRef, ConnPid, State);
                nofin -> State
            end,
            {noreply, State2}
    end;

handle_info({gun_trailers, ConnPid, StreamRef, Trailers},
            State = #state{streams = Streams}) ->
    case maps:find(StreamRef, Streams) of
        error -> {noreply, State};
        {ok, {CallerPid, ConnPid}} ->
            CallerPid ! {gun_trailers, self(), StreamRef, Trailers},
            {noreply, stream_done(StreamRef, ConnPid, State)};
        {ok, {abandoned, ConnPid}} ->
            {noreply, stream_done(StreamRef, ConnPid, State)}
    end;

handle_info({gun_error, ConnPid, StreamRef, Reason},
            State = #state{streams = Streams}) when is_reference(StreamRef) ->
    case maps:find(StreamRef, Streams) of
        error -> {noreply, State};
        {ok, {CallerPid, ConnPid}} ->
            CallerPid ! {gun_error, self(), StreamRef, Reason},
            {noreply, stream_done(StreamRef, ConnPid, State)};
        {ok, {abandoned, ConnPid}} ->
            {noreply, stream_done(StreamRef, ConnPid, State)}
    end;

%% Connection-level error (not stream-specific).
handle_info({gun_error, ConnPid, Reason}, State) ->
    ?event(warning, {gun_connection_error, {pid, ConnPid}, {reason, Reason}}),
    {noreply, drop_conn(ConnPid, {error, Reason}, State)};

%% Connection went down.
handle_info({gun_down, ConnPid, _Proto, Reason, _KilledStreams}, State) ->
    ?event(debug_http_client, {gun_connection_down, {pid, ConnPid}, {reason, Reason}}),
    {noreply, drop_conn(ConnPid, {error, {down, Reason}}, State)};

%% gun process died (linked).
handle_info({'EXIT', ConnPid, Reason}, State = #state{conns = Cs}) ->
    case maps:is_key(ConnPid, Cs) of
        true ->
            {noreply, handle_conn_exit(ConnPid, Reason, State)};
        false ->
            {noreply, State}
    end;

%% Connect timeout: gun_up never arrived for this connection.
handle_info({?MODULE, connect_timeout, ConnPid},
            State = #state{conns = Cs, gun_module = GunMod}) ->
    case maps:find(ConnPid, Cs) of
        {ok, #conn{protocol = connecting}} ->
            GunMod:close(ConnPid),
            {noreply, handle_connect_timeout(ConnPid, State)};
        _ ->
            {noreply, State}
    end;

%% Caller died: cancel its in-flight streams.
handle_info({'DOWN', Ref, process, CallerPid, _Reason},
            State = #state{caller_refs = CRefs}) ->
    case maps:find(CallerPid, CRefs) of
        {ok, Ref} ->
            {noreply, cancel_caller_streams(CallerPid, State)};
        _ ->
            {noreply, State}
    end;

handle_info({?MODULE, idle_check},
            State = #state{idle_poll_ms = IdlePollMs}) ->
    Idle = is_idle(State),
    case {State#state.was_idle, Idle} of
        {true, true} ->
            {stop, normal, State};
        _ ->
            erlang:send_after(IdlePollMs, self(), {?MODULE, idle_check}),
            {noreply, State#state{was_idle = Idle}}
    end;

handle_info(Msg, State) ->
    ?event(warning, {unhandled_info, {module, ?MODULE}, {message, Msg}}),
    {noreply, State}.

terminate(_Reason, #state{authority = Authority, scope = Scope,
                          conns = Cs, streams = Streams, queue = Q,
                          caller_refs = CRefs, gun_module = GunMod}) ->
    %% Table may already be gone if the supervisor tore it down first.
    %% Verified: ets:delete/2 on a missing table raises error:badarg.
    try ets:delete(hb_gun_pool_registry, {Authority, Scope})
    catch error:badarg -> ok
    end,
    maps:foreach(
        fun(StreamRef, {CallerPid, _ConnPid}) when is_pid(CallerPid) ->
            CallerPid ! {gun_error, self(), StreamRef, shutdown};
           (_, _) ->
            ok
        end,
        Streams),
    lists:foreach(
        fun({_ReqArgs, _CallerPid, From}) ->
            gen_server:reply(From, {error, shutdown})
        end,
        Q),
    maps:foreach(
        fun(ConnPid, _) -> GunMod:close(ConnPid) end,
        Cs),
    maps:foreach(
        fun(_, Ref) -> erlang:demonitor(Ref, [flush]) end,
        CRefs),
    ok.

%%====================================================================
%% Internal — request handling
%%====================================================================

handle_request(ReqArgs, CallerPid, From, State) ->
    case pick_conn(State) of
        {ok, ConnPid} ->
            {StreamRef, State2} = issue_request(ConnPid, ReqArgs, CallerPid, State),
            {reply, {ok, StreamRef}, ensure_caller_monitored(CallerPid, State2)};
        none ->
            case can_open_conn(State) of
                true ->
                    case open_conn(State) of
                        {ok, State2} ->
                            enqueue(ReqArgs, CallerPid, From, State2);
                        {error, Reason, State2} ->
                            %% gun:open failed synchronously; do not strand the
                            %% current caller on the queue with no live timer.
                            ?event(warning,
                                   {gun_open_failed_immediate, {reason, Reason}}),
                            {reply, {error, no_connection_available}, State2}
                    end;
                false ->
                    enqueue(ReqArgs, CallerPid, From, State)
            end
    end.

enqueue(ReqArgs, CallerPid, From, State = #state{queue = Q, max_queued = Cap}) ->
    CallerQ = [E || E = {_, CPid, _} <- Q, CPid =:= CallerPid],
    case length(CallerQ) >= Cap of
        true ->
            {reply, {error, no_connection_available}, State};
        false ->
            State2 = ensure_caller_monitored(CallerPid, State),
            {noreply, State2#state{queue = Q ++ [{ReqArgs, CallerPid, From}]}}
    end.

drain_queue(State = #state{queue = []}) ->
    State;
drain_queue(State = #state{queue = [{ReqArgs, CallerPid, From} | Rest]}) ->
    case pick_conn(State) of
        {ok, ConnPid} ->
            {StreamRef, State2} = issue_request(ConnPid, ReqArgs, CallerPid,
                                                 State#state{queue = Rest}),
            gen_server:reply(From, {ok, StreamRef}),
            drain_queue(State2);
        none ->
            case can_open_conn(State) of
                true ->
                    case open_conn(State) of
                        {ok, State2}        -> State2;
                        {error, _, State2}  -> State2
                    end;
                false ->
                    State
            end
    end.

pick_conn(#state{conns = Cs}) ->
    Ready = maps:fold(
        fun(Pid, #conn{protocol = P, streams_in_flight = N, max_streams = Max}, Acc)
                when P =/= connecting, N < Max ->
            [{N, Pid} | Acc];
           (_, _, Acc) -> Acc
        end,
        [],
        Cs
    ),
    case lists:keysort(1, Ready) of
        []          -> none;
        [{_, Pid} | _] -> {ok, Pid}
    end.

can_open_conn(#state{conns = Cs, max_sockets = Max}) ->
    maps:size(Cs) < Max.

open_conn(State = #state{host = Host, port = Port, transport = Transport,
                          opts = Opts, protocol = Protocol, conns = Cs,
                          gun_module = GunMod}) ->
    ConnTimeout = hb_opts:get(
        http_client_connect_timeout, ?DEFAULT_CONNECT_TIMEOUT, Opts),
    GunOpts = build_gun_opts(Transport, Protocol, Opts),
    OpenFn = hb_opts:get(gun_open_fn, fun GunMod:open/3, Opts),
    case OpenFn(Host, Port, GunOpts) of
        {ok, ConnPid} ->
            TRef = erlang:send_after(ConnTimeout, self(),
                                     {?MODULE, connect_timeout, ConnPid}),
            Conn = #conn{
                pid          = ConnPid,
                protocol     = connecting,
                opened_at    = erlang:monotonic_time(millisecond),
                connect_timer = TRef
            },
            {ok, State#state{conns = Cs#{ConnPid => Conn}}};
        {error, Reason} ->
            %% Surface the error to the triggering caller without crashing
            %% the manager and without incrementing consec_connect_failures:
            %% a synchronous gun:open error is usually a config/DNS issue,
            %% not a load signal. Tripping the 30s cooldown here causes
            %% unrelated callers sharing this Authority to see spurious
            %% no_connection_available for the whole cooldown window.
            ?event(warning, {gun_open_failed, {host, Host}, {port, Port},
                             {reason, Reason}}),
            {error, Reason, State}
    end.

issue_request(ConnPid, ReqArgs, CallerPid,
              State = #state{conns = Cs, streams = Streams,
                             gun_module = GunMod}) ->
    #{method := Method, path := Path, headers := Headers, body := Body} = ReqArgs,
    HeaderList = case is_list(Headers) of
        true -> Headers;
        false -> maps:to_list(Headers)
    end,
    StreamRef = GunMod:request(ConnPid, Method, Path, HeaderList, Body),
    Conn = maps:get(ConnPid, Cs),
    Conn2 = Conn#conn{
        streams_in_flight = Conn#conn.streams_in_flight + 1,
        last_used = erlang:monotonic_time(millisecond)
    },
    State2 = State#state{
        conns   = Cs#{ConnPid => Conn2},
        streams = Streams#{StreamRef => {CallerPid, ConnPid}}
    },
    {StreamRef, State2}.

%%====================================================================
%% Internal — stream lifecycle
%%====================================================================

stream_done(StreamRef, ConnPid, State = #state{conns = Cs, streams = Streams}) ->
    State2 = State#state{streams = maps:remove(StreamRef, Streams)},
    case maps:find(ConnPid, Cs) of
        error ->
            drain_queue(State2);
        {ok, Conn} ->
            N = max(0, Conn#conn.streams_in_flight - 1),
            Conn2 = Conn#conn{streams_in_flight = N},
            drain_queue(State2#state{conns = Cs#{ConnPid => Conn2}})
    end.

drop_conn(ConnPid, ErrorReason, State = #state{conns = Cs, streams = Streams}) ->
    %% Notify callers whose streams were on this conn and drop those streams
    %% in a single pass. filter's predicate-with-side-effect is idiomatic
    %% when the predicate is the selector for whom to notify.
    Streams2 = maps:filter(
        fun(Ref, {CallerPid, CConn})
                when is_pid(CallerPid), CConn =:= ConnPid ->
                CallerPid ! {gun_error, self(), Ref, ErrorReason},
                false;
           (_, {abandoned, CConn}) when CConn =:= ConnPid ->
                false;
           (_, _) -> true
        end, Streams),
    drain_queue(State#state{conns = maps:remove(ConnPid, Cs),
                            streams = Streams2}).

handle_conn_exit(ConnPid, _Reason, State = #state{conns = Cs}) ->
    case maps:find(ConnPid, Cs) of
        {ok, #conn{protocol = connecting}} ->
            handle_connect_timeout(ConnPid, State);
        _ ->
            drop_conn(ConnPid, {error, shutdown}, State)
    end.

handle_connect_timeout(ConnPid, State = #state{conns = Cs,
                                                consec_connect_failures = NF}) ->
    case maps:find(ConnPid, Cs) of
        {ok, Conn} ->
            cancel_connect_timer(Conn),
            ok;
        error ->
            ok
    end,
    Cs2 = maps:remove(ConnPid, Cs),
    NF2 = NF + 1,
    State2 = State#state{conns = Cs2, consec_connect_failures = NF2},
    case NF2 >= ?MAX_CONNECT_FAILURES of
        true ->
            Until = erlang:monotonic_time(millisecond) + ?COOLDOWN_MS,
            fail_queued_requests(State2#state{cooldown_until = Until});
        false ->
            drain_queue(State2)
    end.

fail_queued_requests(State = #state{queue = Q}) ->
    lists:foreach(
        fun({_ReqArgs, _CallerPid, From}) ->
            gen_server:reply(From, {error, no_connection_available})
        end,
        Q),
    State#state{queue = []}.

cancel_caller_streams(CallerPid, State = #state{streams = Streams,
                                                caller_refs = CRefs,
                                                queue = Q}) ->
    %% Keep dead callers' h1 streams occupying their socket until gun
    %% naturally drains the response; HTTP/1 cancel only silences the
    %% stream to the caller and does not make the connection reusable.
    Streams2 = maps:map(
        fun(_Ref, {CPid, ConnPid}) when CPid =:= CallerPid ->
            {abandoned, ConnPid};
           (_Ref, StreamState) ->
            StreamState
        end,
        Streams),
    %% Drop queued (not yet dispatched) requests from this caller.
    Q2 = [E || E = {_, CPid, _} <- Q, CPid =/= CallerPid],
    case maps:take(CallerPid, CRefs) of
        {MonRef, CRefs2} ->
            erlang:demonitor(MonRef, [flush]),
            drain_queue(State#state{streams = Streams2,
                                    caller_refs = CRefs2, queue = Q2});
        error ->
            drain_queue(State#state{streams = Streams2, queue = Q2})
    end.

%%====================================================================
%% Internal — helpers
%%====================================================================

%% @doc Mark a stream abandoned without freeing its socket until gun ends it.
abandon_stream(StreamRef, ConnPid, State = #state{streams = Streams}) ->
    State#state{streams = Streams#{StreamRef => {abandoned, ConnPid}}}.

ensure_caller_monitored(CallerPid, State = #state{caller_refs = CRefs}) ->
    case maps:is_key(CallerPid, CRefs) of
        true ->
            State;
        false ->
            MonRef = erlang:monitor(process, CallerPid),
            State#state{caller_refs = CRefs#{CallerPid => MonRef}}
    end.

cancel_connect_timer(#conn{connect_timer = undefined}) -> ok;
cancel_connect_timer(#conn{connect_timer = TRef}) ->
    erlang:cancel_timer(TRef),
    ok.

is_idle(#state{conns = Cs, queue = Q, streams = Streams}) ->
    Q =:= [] andalso
    maps:size(Streams) =:= 0 andalso
    maps:fold(
        fun(_, #conn{streams_in_flight = N}, A) -> A + N end, 0, Cs) =:= 0.

%% @doc Normalize TLS opts for stable scope hashing.
%%
%% Fields with known semantics are extracted and sorted independently:
%%   verify, cacerts/cacertfile, cert/certfile, key/keyfile,
%%   server_name_indication, versions, alpn_advertised_protocols,
%%   customize_hostname_check.
%% ciphers list order is preserved (server-preference matters).
%% verify_fun closures are replaced by their erlang:fun_info uniq so
%% functionally equivalent funs from the same module/function hash equally
%% even if the closure captures different variables.
%% Unknown opts are passed through via lists:sort on {Key, Value} pairs.
normalize_tls_opts(Opts) when is_list(Opts) ->
    normalize_tls_opts(maps:from_list(Opts));
normalize_tls_opts(Opts) when is_map(Opts) ->
    Known = [verify, cacerts, cacertfile, cert, certfile, key, keyfile,
             server_name_indication, versions, alpn_advertised_protocols,
             customize_hostname_check, ciphers],
    KnownPairs = lists:filtermap(
        fun(K) ->
            case maps:find(K, Opts) of
                {ok, V} -> {true, {K, normalize_tls_value(K, V)}};
                error   -> false
            end
        end,
        Known),
    UnknownPairs = lists:sort(
        [{K, normalize_tls_value(K, V)} || {K, V} <- maps:to_list(Opts),
                                            not lists:member(K, Known)]),
    lists:sort(KnownPairs) ++ UnknownPairs.

normalize_tls_value(verify_fun, {Fun, _State}) ->
    {verify_fun, erlang:fun_info(Fun, uniq)};
normalize_tls_value(verify_fun, Fun) when is_function(Fun) ->
    {verify_fun, erlang:fun_info(Fun, uniq)};
normalize_tls_value(ciphers, Ciphers) ->
    Ciphers;
normalize_tls_value(_Key, Value) ->
    Value.

resolve_protocol(tls, Opts) ->
    hb_opts:get(protocol, http2, Opts);
resolve_protocol(tcp, Opts) ->
    case hb_opts:get(protocol, http2, Opts) of
        http3 -> http2;
        Other -> Other
    end.

max_sockets(http2, Opts) ->
    hb_opts:get(http_client_gun_max_sockets_h2, ?DEFAULT_MAX_SOCKETS_H2, Opts);
max_sockets(_, Opts) ->
    hb_opts:get(http_client_gun_max_sockets_h1, ?DEFAULT_MAX_SOCKETS_H1, Opts).

build_gun_opts(Transport, Protocol, Opts) ->
    Keepalive = hb_opts:get(http_client_keepalive, ?DEFAULT_KEEPALIVE_TIMEOUT, Opts),
    ConnTimeout = hb_opts:get(
        http_client_connect_timeout, ?DEFAULT_CONNECT_TIMEOUT, Opts),
    Base = #{
        http_opts       => #{keepalive => Keepalive},
        http2_opts      => #{keepalive => Keepalive,
                             notify_settings_changed => true},
        retry           => 0,
        connect_timeout => ConnTimeout
    },
    ProtoOpts = case Protocol of
        http3 -> Base#{protocols => [http3], transport => quic};
        http2 -> Base#{protocols => [http2]};
        _     -> Base#{protocols => [http]}
    end,
    TlsOpts = hb_opts:get(tls_opts, [], Opts),
    case {Protocol, Transport} of
        {http3, _} ->
            %% Keep transport => quic from ProtoOpts; gun still honors
            %% tls_opts for the underlying TLS/QUIC handshake.
            ProtoOpts#{tls_opts => TlsOpts};
        {_, tls} ->
            ProtoOpts#{transport => tls, tls_opts => TlsOpts};
        {_, tcp} ->
            ProtoOpts
    end.
