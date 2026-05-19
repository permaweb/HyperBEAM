%%% @doc Monitor BEAM system events that indicate scheduler starvation,
%%% long-running NIFs/drivers, GC pauses, and mailbox buildup. Uses
%%% erlang:system_monitor/2 to receive notifications when thresholds are
%%% breached and logs them through the ?event system.
%%%
%%% When a long_schedule event exceeds the deep inspection threshold,
%%% the monitor grabs process_info for the offending PID (stacktrace,
%%% current function, memory, message queue, reductions). This is
%%% rate-limited to avoid flooding.
-module(hb_system_monitor).

-export([ensure_started/1]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_LONG_SCHEDULE_MS, 40).
-define(DEFAULT_LONG_GC_MS, 50).
-define(DEFAULT_LARGE_HEAP_WORDS, 40 * 1024 * 1024).
-define(DEFAULT_LONG_MSG_QUEUE_ENABLE, 10_000).
-define(DEFAULT_LONG_MSG_QUEUE_DISABLE, 1_000).
-define(DEFAULT_DEEP_INSPECT_MS, 90).
-define(DEFAULT_DEEP_INSPECT_INTERVAL_MS, 1_000).

%% @doc Ensure the system monitor singleton is started if enabled.
ensure_started(Opts) ->
    Enabled = hb_opts:get(system_monitor, not hb_features:test(), Opts),
    ?event(system_monitor, {system_monitor_enabled, Enabled}),
    case Enabled of
        true ->
            _ = hb_name:singleton(?MODULE, fun() -> start(Opts) end),
            ok;
        false ->
            ok
    end.

%% @doc Start the system monitor process.
start(Opts) ->
    hb_prometheus:ensure_started(),
    init_prometheus(),
    MonitorOpts = build_monitor_opts(Opts),
    ?event(system_monitor, {starting_system_monitor, MonitorOpts}),
    erlang:system_monitor(self(), MonitorOpts),
    loop(#{
        opts => Opts,
        last_deep_inspect =>
            erlang:monotonic_time(millisecond)
                - ?DEFAULT_DEEP_INSPECT_INTERVAL_MS
    }).

%% @doc Build the erlang:system_monitor/2 option list from config.
build_monitor_opts(Opts) ->
    LongSchedule =
        hb_opts:get(long_schedule_ms, ?DEFAULT_LONG_SCHEDULE_MS, Opts),
    LongGC =
        hb_opts:get(long_gc_ms, ?DEFAULT_LONG_GC_MS, Opts),
    LargeHeap =
        hb_opts:get(large_heap_words, ?DEFAULT_LARGE_HEAP_WORDS, Opts),
    MsgQueueEnable =
        hb_opts:get(
            long_msg_queue_enable,
            ?DEFAULT_LONG_MSG_QUEUE_ENABLE,
            Opts
        ),
    MsgQueueDisable =
        hb_opts:get(
            long_msg_queue_disable,
            ?DEFAULT_LONG_MSG_QUEUE_DISABLE,
            Opts
        ),
    [
        {long_schedule, LongSchedule},
        {long_gc, LongGC},
        {large_heap, LargeHeap},
        {long_message_queue, {MsgQueueDisable, MsgQueueEnable}},
        busy_port,
        busy_dist_port
    ].

%% @doc Receive loop for system monitor messages.
loop(State) ->
    receive
        {monitor, PidOrPort, long_schedule, Info} ->
            ?event(system_monitor,
                {long_schedule, PidOrPort, Info}),
            InLoc = format_location(
                proplists:get_value(in, Info, undefined)),
            OutLoc = format_location(
                proplists:get_value(out, Info, undefined)),
            hb_prometheus:inc(counter,
                system_monitor_long_schedule_total,
                [InLoc, OutLoc]),
            State2 = maybe_deep_inspect(PidOrPort, Info, State),
            loop(State2);
        {monitor, Pid, long_gc, Info} ->
            ?event(system_monitor,
                {long_gc, Pid, Info}),
            hb_prometheus:inc(counter, system_monitor_events_total,
                [long_gc]),
            loop(State);
        {monitor, Pid, large_heap, Info} ->
            ?event(system_monitor,
                {large_heap, Pid, Info}),
            hb_prometheus:inc(counter, system_monitor_events_total,
                [large_heap]),
            loop(State);
        {monitor, Pid, long_message_queue, Long} ->
            ?event(system_monitor,
                {long_message_queue, Pid, Long}),
            hb_prometheus:inc(counter, system_monitor_events_total,
                [long_message_queue]),
            loop(State);
        {monitor, Pid, busy_port, Port} ->
            ?event(system_monitor,
                {busy_port, Pid, Port}),
            hb_prometheus:inc(counter, system_monitor_events_total,
                [busy_port]),
            loop(State);
        {monitor, Pid, busy_dist_port, Port} ->
            ?event(system_monitor,
                {busy_dist_port, Pid, Port}),
            hb_prometheus:inc(counter, system_monitor_events_total,
                [busy_dist_port]),
            loop(State);
        Message ->
            ?event(warning,
                {unhandled_info, {module, ?MODULE}, {message, Message}}),
            loop(State)
    end.

%% @doc Declare prometheus metrics for system monitor events.
init_prometheus() ->
    hb_prometheus:declare(counter, [
        {name, system_monitor_events_total},
        {labels, [event]},
        {help, "Count of erlang:system_monitor events by type"}
    ]),
    hb_prometheus:declare(counter, [
        {name, system_monitor_long_schedule_total},
        {labels, [scheduled_in, scheduled_out]},
        {help,
            "Count of long_schedule events"
            " labeled by in/out function"}
    ]),
    hb_prometheus:declare(counter, [
        {name, system_monitor_deep_inspect_total},
        {labels, [entry, location]},
        {help,
            "Count of deep inspections."
            " entry=outermost stack frame,"
            " location=mid/current frame"}
    ]).

%% @doc Format a schedule location for use as a prometheus label.
format_location(undefined) ->
    <<"undefined">>;
format_location({Mod, Func, Arity}) ->
    <<
        (atom_to_binary(Mod))/binary, ":",
        (atom_to_binary(Func))/binary, "/",
        (integer_to_binary(Arity))/binary
    >>;
format_location(_) ->
    <<"unknown">>.

%% @doc If the timeout exceeds the deep inspection threshold and
%% enough time has passed since the last inspection, grab detailed
%% process info for the offending PID.
maybe_deep_inspect(PidOrPort, _Info, State) when is_port(PidOrPort) ->
    State;
maybe_deep_inspect(Pid, Info, #{opts := Opts, last_deep_inspect := Last} = State) ->
    Threshold =
        hb_opts:get(deep_inspect_ms, ?DEFAULT_DEEP_INSPECT_MS, Opts),
    Cooldown =
        hb_opts:get(
            deep_inspect_interval_ms,
            ?DEFAULT_DEEP_INSPECT_INTERVAL_MS,
            Opts
        ),
    Timeout = proplists:get_value(timeout, Info, 0),
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - Last,
    case Timeout >= Threshold andalso Elapsed >= Cooldown of
        true ->
            try
                deep_inspect(Pid, Info)
            catch
                Class:Reason ->
                    ?event(system_monitor,
                        {deep_inspect_error, Pid, Class, Reason})
            end,
            State#{last_deep_inspect => Now};
        false ->
            State
    end.

%% @doc Grab detailed process info, log it, and record in prometheus.
%% The `entry` label is the outermost non-glue frame (why the process
%% exists). The `location` label is `mid/current` where mid is roughly
%% the middle of the stack and current is the innermost frame — enough
%% to tell what region of the codebase was active without exploding
%% prometheus cardinality.
deep_inspect(Pid, ScheduleInfo) ->
    ProcInfo = safe_process_info(Pid, [
        registered_name,
        current_function,
        current_stacktrace,
        initial_call,
        message_queue_len,
        memory,
        reductions,
        dictionary,
        status
    ]),
    ?event(system_monitor, {deep_inspect, Pid, ScheduleInfo, ProcInfo}),
    Stack = proplists:get_value(current_stacktrace, ProcInfo, []),
    Entry = stack_entry(Stack),
    Location = stack_location(Stack),
    hb_prometheus:inc(counter, system_monitor_deep_inspect_total,
        [Entry, Location]).

%% @doc Extract the outermost non-glue frame as the entry point.
stack_entry([]) ->
    <<"unknown">>;
stack_entry(Stack) ->
    hb_format:process_from_trace(Stack).

%% @doc Build a compact location label from the stack: `mid/current`.
%% Current is the innermost frame (head of stacktrace), mid is
%% roughly 1/3 from the bottom — a frame that gives codebase context
%% without being the generic entry or the leaf.
stack_location([]) ->
    <<"unknown">>;
stack_location([Only]) ->
    format_frame(Only);
stack_location(Stack) ->
    Current = hd(Stack),
    Len = length(Stack),
    MidIdx = max(1, Len - (Len div 3)),
    Mid = lists:nth(MidIdx, Stack),
    case Mid =:= Current of
        true ->
            format_frame(Current);
        false ->
            <<
                (format_frame(Mid))/binary, "/",
                (format_frame(Current))/binary
            >>
    end.

%% @doc Format a single stack frame as `mod:func/arity`.
format_frame({Mod, Func, Arity, _}) ->
    format_location({Mod, Func, Arity});
format_frame({Mod, Func, Arity}) ->
    format_location({Mod, Func, Arity});
format_frame(_) ->
    <<"unknown">>.

%% @doc Safely retrieve process info. The process may have died
%% between the monitor event and our inspection.
safe_process_info(Pid, Items) ->
    try erlang:process_info(Pid, Items) of
        undefined -> [{status, dead}];
        Info -> Info
    catch
        _:_ -> [{status, dead}]
    end.

%% =================================================================
%% Tests
%% =================================================================

%% @doc Test that deep_inspect captures process info for a living process.
deep_inspect_live_process_test() ->
    hb_prometheus:ensure_started(),
    init_prometheus(),
    Info = [{timeout, 100}, {in, undefined}, {out, undefined}],
    Pid = spawn(fun() -> receive stop -> ok end end),
    deep_inspect(Pid, Info),
    ProcInfo = safe_process_info(Pid, [status, memory]),
    ?assertMatch([{status, _}, {memory, _}], ProcInfo),
    Pid ! stop.

%% @doc Test that deep_inspect handles a dead process gracefully.
deep_inspect_dead_process_test() ->
    hb_prometheus:ensure_started(),
    init_prometheus(),
    Info = [{timeout, 100}, {in, undefined}, {out, undefined}],
    Pid = spawn(fun() -> ok end),
    timer:sleep(10),
    deep_inspect(Pid, Info),
    ProcInfo = safe_process_info(Pid, [status]),
    ?assertEqual([{status, dead}], ProcInfo).

%% @doc Test that maybe_deep_inspect fires when threshold is exceeded.
maybe_deep_inspect_fires_test() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    Info = [{timeout, 100}, {in, undefined}, {out, undefined}],
    Now = erlang:monotonic_time(millisecond),
    State = #{
        opts => #{deep_inspect_ms => 50, deep_inspect_interval_ms => 0},
        last_deep_inspect => Now - 1000
    },
    State2 = maybe_deep_inspect(Pid, Info, State),
    #{last_deep_inspect := LastTime} = State2,
    ?assert(LastTime >= Now),
    Pid ! stop.

%% @doc Test that maybe_deep_inspect respects cooldown.
maybe_deep_inspect_cooldown_test() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    Info = [{timeout, 100}, {in, undefined}, {out, undefined}],
    Now = erlang:monotonic_time(millisecond),
    State = #{
        opts => #{deep_inspect_ms => 50, deep_inspect_interval_ms => 60_000},
        last_deep_inspect => Now
    },
    State2 = maybe_deep_inspect(Pid, Info, State),
    ?assertEqual(Now, maps:get(last_deep_inspect, State2)),
    Pid ! stop.

%% @doc Test that maybe_deep_inspect skips when below threshold.
maybe_deep_inspect_below_threshold_test() ->
    Pid = spawn(fun() -> receive stop -> ok end end),
    Info = [{timeout, 10}, {in, undefined}, {out, undefined}],
    State = #{
        opts => #{deep_inspect_ms => 50, deep_inspect_interval_ms => 0},
        last_deep_inspect => 0
    },
    State2 = maybe_deep_inspect(Pid, Info, State),
    ?assertEqual(0, maps:get(last_deep_inspect, State2)),
    Pid ! stop.
