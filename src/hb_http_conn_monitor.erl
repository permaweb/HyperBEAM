%%% @doc A gen_server that monitors gun connection mailbox sizes and reports
%%% metrics to Prometheus. This provides visibility into connection health
%%% and potential backpressure issues.
-module(hb_http_conn_monitor).
-behaviour(gen_server).
-include("include/hb.hrl").
-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-define(CONNECTIONS_ETS, hb_http_client_connections).
-define(MONITORING_INTERVAL_MS, 5000).

-record(state, {
    opts = #{}
}).

%%% ==================================================================
%%% Public interface.
%%% ==================================================================

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%%% ==================================================================
%%% gen_server callbacks.
%%% ==================================================================

init(Opts) ->
    case hb_opts:get(prometheus, not hb_features:test(), Opts) of
        true ->
            init_prometheus(),
            erlang:send_after(?MONITORING_INTERVAL_MS, self(), conn_mailbox_monitoring),
            {ok, #state{opts = Opts}};
        false ->
            {ok, #state{opts = Opts}}
    end.

handle_call(Request, _From, State) ->
    ?event(warning, {unhandled_call, {module, ?MODULE}, {request, Request}}),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    ?event(warning, {unhandled_cast, {module, ?MODULE}, {cast, Cast}}),
    {noreply, State}.

handle_info(conn_mailbox_monitoring, State) ->
    spawn(fun() ->
        case ets:whereis(?CONNECTIONS_ETS) of
            undefined ->
                ok;
            _ ->
                ets:foldl(
                    fun({ConnKey, ConnPID}, Acc) ->
                        sample_conn_pid(ConnKey, ConnPID),
                        Acc
                    end,
                    ok,
                    ?CONNECTIONS_ETS
                )
        end
    end),
    erlang:send_after(?MONITORING_INTERVAL_MS, self(), conn_mailbox_monitoring),
    {noreply, State};

handle_info(Message, State) ->
    ?event(warning, {unhandled_info, {module, ?MODULE}, {message, Message}}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%% ==================================================================
%%% Private functions.
%%% ==================================================================

init_prometheus() ->
    case application:get_application(prometheus) of
        undefined ->
            ok;
        _ ->
            try
                prometheus_gauge:new([
                    {name, gun_mailbox_size},
                    {labels, [conn_id]},
                    {help, "Gun connection mailbox size"}
                ])
            catch
                error:{mf_already_exists, _, _} ->
                    %% Metric already registered, this is fine
                    ok
            end
    end.

sample_conn_pid(ConnKey, ConnPID) ->
    case process_info(ConnPID, message_queue_len) of
        {message_queue_len, Len} ->
            report(ConnKey, Len);
        undefined ->
            ok
    end.

report({Peer, ConnType, Index}, Value) ->
    ConnId = iolist_to_binary([hb_util:bin(Peer), "_", atom_to_binary(ConnType), "_", integer_to_binary(Index)]),
    prometheus_gauge:set(
        gun_mailbox_size,
        [ConnId],
        Value);
report({Peer, ConnType}, Value) ->
    ConnId = iolist_to_binary([hb_util:bin(Peer), "_", atom_to_binary(ConnType)]),
    prometheus_gauge:set(
        gun_mailbox_size,
        [ConnId],
        Value);
report(ConnKey, Value) ->
    prometheus_gauge:set(
        gun_mailbox_size,
        [hb_util:bin(ConnKey)],
        Value).