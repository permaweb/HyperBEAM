%%% @doc Top-level supervisor and public API for the gun connection pool.
%%% ETS registry keyed on {Authority, Scope} → manager pid; each manager is
%%% a temporary simple_one_for_one child (hb_gun_pool_mgr juggler).
-module(hb_gun_pool).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_or_get_pool/3, stop/1, stop_all/0]).
-export([flush_stream/1]).
-export([init/1]).

-include("include/hb.hrl").

-define(REGISTRY, hb_gun_pool_registry).

%% @doc Start the top-level pool supervisor. Called by hb_sup.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Find or start a pool. ConnInfo must contain host, port, and
%% transport (tcp | tls); opts is optional (hb_opts-style map).
start_or_get_pool(Authority, Scope, ConnInfo = #{host := Host, port := Port,
                                                 transport := Transport}) ->
    case ets:info(?REGISTRY) of
        undefined -> {error, pool_runtime_unavailable};
        _ ->
            Opts = maps:get(opts, ConnInfo, #{}),
            start_pool(Authority, Scope,
                       #{host => Host, port => Port,
                         transport => Transport, opts => Opts})
    end;
start_or_get_pool(Authority, Scope, ConnInfo = #{host := Host, port := Port}) ->
    case ets:info(?REGISTRY) of
        undefined -> {error, pool_runtime_unavailable};
        _ ->
            Opts = maps:get(opts, ConnInfo, #{}),
            start_pool(Authority, Scope,
                       #{host => Host, port => Port,
                         transport => tcp, opts => Opts})
    end.

stop(MgrPid) -> stop_pool(MgrPid).

%% @doc Stop every pool currently registered. Used by test teardown.
stop_all() ->
    case ets:info(?REGISTRY) of
        undefined ->
            ok;
        _ ->
            lists:foreach(
                fun({Key, MgrPid}) ->
                    ets:delete(?REGISTRY, Key),
                    _ = supervisor:terminate_child(?MODULE, MgrPid)
                end,
                ets:tab2list(?REGISTRY))
    end.

%% @doc Drain buffered messages for a timed-out StreamRef from the caller mailbox.
flush_stream(StreamRef) ->
    receive
        {gun_response, _, StreamRef, _, _, _} -> flush_stream(StreamRef);
        {gun_data, _, StreamRef, _, _}        -> flush_stream(StreamRef);
        {gun_trailers, _, StreamRef, _}       -> flush_stream(StreamRef);
        {gun_error, _, StreamRef, _}          -> flush_stream(StreamRef)
    after 0 -> ok
    end.

%%====================================================================
%% supervisor callback
%%====================================================================

init([]) ->
    case ets:info(?REGISTRY) of
        undefined ->
            ets:new(?REGISTRY,
                    [named_table, public, set, {read_concurrency, true}]);
        _ -> ok
    end,
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 60},
    ChildSpec = #{
        id      => hb_gun_pool_mgr,
        start   => {hb_gun_pool_mgr, start_link, []},
        restart => temporary,
        shutdown => 10000,
        type    => worker,
        modules => [hb_gun_pool_mgr]
    },
    {ok, {SupFlags, [ChildSpec]}}.

%%====================================================================
%% Private
%%====================================================================

%% Atomically claim {Authority, Scope} via ets:insert_new with a
%% {pending, Claimer} marker so concurrent cold-starts yield one manager.
start_pool(Authority, Scope, #{host := Host, port := Port,
                                transport := Transport, opts := Opts}) ->
    Key = {Authority, Scope},
    case ets:lookup(?REGISTRY, Key) of
        [{Key, {pending, Claimer}}] ->
            wait_for_claimer(Key, Claimer),
            %% If the claimer crashed before promoting the marker, clear it
            %% atomically on the exact tuple — otherwise the next start_pool
            %% pass sees the same dead marker and the pool is permanently
            %% wedged for this {Authority, Scope}.
            _ = maybe_clear_stale_pending(Key, Claimer),
            start_pool(Authority, Scope,
                       #{host => Host, port => Port,
                         transport => Transport, opts => Opts});
        [{Key, MgrPid}] when is_pid(MgrPid) ->
            case is_process_alive(MgrPid) of
                true  -> {ok, MgrPid};
                false ->
                    ets:delete(?REGISTRY, Key),
                    start_pool(Authority, Scope,
                               #{host => Host, port => Port,
                                 transport => Transport, opts => Opts})
            end;
        [] ->
            case ets:insert_new(?REGISTRY, {Key, {pending, self()}}) of
                false ->
                    start_pool(Authority, Scope,
                               #{host => Host, port => Port,
                                 transport => Transport, opts => Opts});
                true ->
                    do_start(Key, Host, Port, Transport, Opts)
            end
    end.

%% Poll the registry until the claimer either promotes the entry to a real
%% manager pid, swaps it out, or the claimer dies. Do not simply monitor
%% the claimer: in the normal success path it long-outlives the
%% pending->MgrPid transition, so a monitor would block the whole deadline
%% even after the pool is ready for use.
wait_for_claimer(Key, Claimer) ->
    Deadline = erlang:monotonic_time(millisecond) + 2_000,
    wait_for_claimer(Key, Claimer, Deadline).

wait_for_claimer(Key, Claimer, Deadline) ->
    case ets:lookup(?REGISTRY, Key) of
        [{Key, {pending, Claimer}}] ->
            case is_process_alive(Claimer)
                 andalso erlang:monotonic_time(millisecond) < Deadline of
                false -> ok;
                true  ->
                    receive after 5 -> ok end,
                    wait_for_claimer(Key, Claimer, Deadline)
            end;
        _ ->
            ok
    end.

maybe_clear_stale_pending(Key, Claimer) ->
    case ets:lookup(?REGISTRY, Key) of
        [{Key, {pending, Claimer}}] ->
            case is_process_alive(Claimer) of
                false ->
                    %% delete_object matches the exact tuple, so a concurrent
                    %% promotion to {Key, MgrPid} is not clobbered.
                    ets:delete_object(?REGISTRY, {Key, {pending, Claimer}});
                true  ->
                    ok
            end;
        _ ->
            ok
    end.

stop_pool(MgrPid) when is_pid(MgrPid) ->
    case ets:match(?REGISTRY, {'$1', MgrPid}) of
        [[Key]] ->
            ets:delete(?REGISTRY, Key),
            supervisor:terminate_child(?MODULE, MgrPid);
        [] ->
            ok
    end.

do_start({Authority, Scope} = Key, Host, Port, Transport, Opts) ->
    case supervisor:start_child(?MODULE,
                                [Authority, Scope, Host, Port, Transport, Opts]) of
        {ok, MgrPid} ->
            ets:insert(?REGISTRY, {Key, MgrPid}),
            {ok, MgrPid};
        Err ->
            ets:delete(?REGISTRY, Key),
            Err
    end.
