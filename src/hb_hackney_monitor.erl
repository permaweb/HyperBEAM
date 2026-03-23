%%% @doc A gen_server that periodically samples hackney pool statistics
%%% and reports them to Prometheus. Runs on a timer rather than per-request
%%% to avoid adding overhead to the HTTP hot path.
-module(hb_hackney_monitor).
-behaviour(gen_server).
-include("include/hb.hrl").
-include("include/hb_http_client.hrl").
-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-define(INTERVAL_MS, 5000).

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
            erlang:send_after(?INTERVAL_MS, self(), sample);
        false ->
            no_op
    end,
    {ok, #{}}.

handle_call(Request, _From, State) ->
    ?event(warning, {unhandled_call, {module, ?MODULE}, {request, Request}}),
    {reply, ok, State}.

handle_cast(Cast, State) ->
    ?event(warning, {unhandled_cast, {module, ?MODULE}, {cast, Cast}}),
    {noreply, State}.

handle_info(sample, State) ->
    sample_pool(),
    erlang:send_after(?INTERVAL_MS, self(), sample),
    {noreply, State};
handle_info(Message, State) ->
    ?event(warning, {unhandled_info, {module, ?MODULE}, {message, Message}}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%% ==================================================================
%%% Private functions.
%%% ==================================================================

%% @doc Register the hackney pool gauges with Prometheus.
init_prometheus() ->
    hb_prometheus:declare(gauge, [
        {name, hackney_pool_in_use},
        {help, "Hackney connections currently in use"}
    ]),
    hb_prometheus:declare(gauge, [
        {name, hackney_pool_queue},
        {help, "Requests waiting for a hackney connection"}
    ]).

%% @doc Read hackney pool stats and update the gauges.
sample_pool() ->
    try hackney_pool:get_stats(?HACKNEY_POOL) of
        Stats ->
            InUse = proplists:get_value(in_use_count, Stats, 0),
            Queue = proplists:get_value(queue_count, Stats, 0),
            prometheus_gauge:set(hackney_pool_in_use, InUse),
            prometheus_gauge:set(hackney_pool_queue, Queue)
    catch _:_ -> ok
    end.
