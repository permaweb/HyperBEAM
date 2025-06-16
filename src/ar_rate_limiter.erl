-module(ar_rate_limiter).
-behaviour(gen_server).
-export([start_link/1, throttle/3, off/0, on/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).
-include("include/hb.hrl").
-record(state, {
          traces,
          off,
          opts
         }).

%%%===================================================================
%%% Public interface.
%%%===================================================================


start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).


%% @doc Hang until it is safe to make another request to the given Peer with the
%% given Path. The limits are configured in include/ar_blacklist_middleware.hrl.
throttle(Peer, Path, Opts) ->
    case lists:member(Peer, hb_opts:get(throttle_exempt_peers, [], Opts)) of
        true ->
            ok;
        false ->
            throttle2(Peer, Path, Opts)
    end.


throttle2(Peer, Path, Opts) ->
    Routes = hb_opts:get(throttle_exempt_paths, [], Opts),
    IsExempt =
        lists:any(fun(Route) -> hb_path:regex_matches(Path, Route) end, Routes),
    case IsExempt of
        true -> ok;
        false ->
            Res = catch gen_server:call(?MODULE, {throttle, Peer, Path}, infinity),
            case Res of
                {'EXIT', {noproc, {gen_server, call, _}}} ->
                    ok;
                {'EXIT', Reason} ->
                    exit(Reason);
                _ ->
                    ok
            end
    end.


%% @doc Turn rate limiting off.
off() ->
    gen_server:cast(?MODULE, turn_off).


%% @doc Turn rate limiting on.
on() ->
    gen_server:cast(?MODULE, turn_on).


%%%===================================================================
%%% Generic server callbacks.
%%%===================================================================


init(Opts) ->
    process_flag(trap_exit, true),
    {ok, #state{traces = #{}, off = false, opts = Opts}}.


handle_call({throttle, _Peer, _Path}, _From, #state{off = true} = State) ->
    {reply, ok, State};
handle_call({throttle, Peer, Path}, From, State) ->
    gen_server:cast(?MODULE, {throttle, Peer, Path, From}),
    {noreply, State};

handle_call(Request, _From, State) ->
    ?event(warning, {unhandled_call, {module, ?MODULE}, {request, Request}}),
    {reply, ok, State}.


handle_cast({throttle, Peer, Path, From}, State) ->
    #state{traces = Traces, opts = Opts} = State,
    {Type, Limit} = hb_opts:get(throttle_rpm_by_path, Path, Opts),
    Now = os:system_time(millisecond),
    case hb_maps:get({Peer, Type}, Traces, not_found, Opts) of
        not_found ->
            gen_server:reply(From, ok),
            Traces2 = hb_maps:put({Peer, Type}, {1, queue:from_list([Now])}, Traces, Opts),
            {noreply, State#state{traces = Traces2}};
        {N, Trace} ->
            {N2, Trace2} = cut_trace(N, queue:in(Now, Trace), Now, Opts),
            %% The macro specifies requests per minute while the throttling window
            %% is 30 seconds.
            HalfLimit = Limit div 2,
            %% Try to approach but not hit the limit.
            case N2 + 1 > max(1, HalfLimit * 80 / 100) of
                true ->
                    ?event(
                      {approaching_peer_rpm_limit,
                       {peer, Peer},
                       {path, Path},
                       {minute_limit, Limit},
                       {caller, From}}),
                    erlang:send_after(
                      1000,
                      ?MODULE,
                      {'$gen_cast', {throttle, Peer, Path, From}}),
                    {noreply, State};
                false ->
                    gen_server:reply(From, ok),
                    Traces2 = hb_maps:put({Peer, Type}, {N2 + 1, Trace2}, Traces, Opts),
                    {noreply, State#state{traces = Traces2}}
            end
    end;

handle_cast(turn_off, State) ->
    {noreply, State#state{off = true}};

handle_cast(turn_on, State) ->
    {noreply, State#state{off = false}};

handle_cast(Cast, State) ->
    ?event(warning, {unhandled_cast, {module, ?MODULE}, {cast, Cast}}),
    {noreply, State}.


handle_info(Message, State) ->
    ?event(warning, {unhandled_info, {module, ?MODULE}, {message, Message}}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


%%%===================================================================
%%% Private functions.
%%%===================================================================


cut_trace(N, Trace, Now, Opts) ->
    {{value, Timestamp}, Trace2} = queue:out(Trace),
    case Timestamp < Now - hb_opts:get(throttle_period, 30000, Opts) of
        true ->
            cut_trace(N - 1, Trace2, Now, Opts);
        false ->
            {N, Trace}
    end.
