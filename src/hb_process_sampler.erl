%%% @doc Sample BEAM process state for diagnostics.
-module(hb_process_sampler).

-export([ensure_started/1]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_SAMPLE_PROCESSES_INTERVAL, 15000).

%% @doc Ensure the process sampler singleton is started if enabled.
ensure_started(Opts) ->
    ProcessSamplerEnabled =
        hb_opts:get(process_sampler, not hb_features:test(), Opts)
            andalso hb_opts:get(prometheus, not hb_features:test(), Opts),
    ?event(process_sampler, {process_sampler_enabled, ProcessSamplerEnabled}),
    case ProcessSamplerEnabled of
        true ->
            _ = hb_name:singleton(?MODULE, fun() -> start(Opts) end),
            ok;
        false ->
            ok
    end.

%% @doc Initialize the process sampler and enter its receive loop.
start(Opts) ->
    ?event(process_sampler, {starting_process_sampler,
        {interval, hb_opts:get(process_sampler_interval, ?DEFAULT_SAMPLE_PROCESSES_INTERVAL, Opts)}}),
    hb_prometheus:ensure_started(),
    schedule_process_sample(Opts),
    loop(
        #{
            opts => Opts
        }
    ).

%% @doc Receive loop for the process sampler.
loop(State = #{ opts := Opts }) ->
    receive
        sample_processes ->
            sample_processes(State),
            schedule_process_sample(Opts),
            loop(State);
        Message ->
            ?event(warning, {unhandled_info, {module, ?MODULE}, {message, Message}}),
            loop(State)
    end.

%% @doc Schedule the next process sample if enabled.
schedule_process_sample(Opts) ->
    case hb_opts:get(
        process_sampler_interval,
        ?DEFAULT_SAMPLE_PROCESSES_INTERVAL,
        Opts
    ) of
        Interval when is_integer(Interval) andalso Interval > 0 ->
            erlang:send_after(Interval, self(), sample_processes);
        _ ->
            ok
    end.

%% @doc Sample all BEAM processes and report aggregate metrics.
sample_processes(#{ opts := Opts }) ->
    StartTime = erlang:monotonic_time(),
    try
        Processes = erlang:processes(),
        ProcessData =
            lists:filtermap(
                fun(PID) -> process_function(PID, Opts) end,
                Processes
            ),
        ProcessMetrics = accumulate_process_metrics(ProcessData),
        report_process_metrics(ProcessMetrics),
        EndTime = erlang:monotonic_time(),
        ElapsedTime =
            erlang:convert_time_unit(
                EndTime - StartTime,
                native,
                microsecond
            ),
        ?event(
            process_sampler,
            {sample_processes,
                {processes, length(Processes)},
                {elapsed_ms, ElapsedTime / 1000}
            },
            Opts
        )
    catch
        Class:Reason:Stacktrace ->
            ?event(
                warning,
                {process_sampler_failed,
                    {class, Class},
                    {reason, Reason},
                    {stacktrace, {trace, Stacktrace}}
                },
                Opts
            )
    end.

%% @doc Sum process memory, reductions, and mailbox sizes by process name.
accumulate_process_metrics(ProcessData) ->
    lists:foldl(
        fun({_Status, ProcessName, Memory, Reductions, MsgQueueLen}, Acc) ->
            {MemoryTotal, ReductionsTotal, MsgQueueLenTotal} =
                maps:get(ProcessName, Acc, {0, 0, 0}),
            maps:put(
                ProcessName,
                {
                    MemoryTotal + Memory,
                    ReductionsTotal + Reductions,
                    MsgQueueLenTotal + MsgQueueLen
                },
                Acc
            )
        end,
        #{},
        ProcessData
    ).

%% @doc Report aggregate process metrics to Prometheus.
report_process_metrics(ProcessMetrics) ->
    reset_process_info_metric(),
    maps:foreach(
        fun(ProcessName, {Memory, Reductions, MsgQueueLen}) ->
            prometheus_gauge:set(process_info, [ProcessName, <<"memory">>], Memory),
            prometheus_gauge:set(
                process_info,
                [ProcessName, <<"reductions">>],
                Reductions
            ),
            prometheus_gauge:set(
                process_info,
                [ProcessName, <<"message_queue">>],
                MsgQueueLen
            )
        end,
        ProcessMetrics
    ),
    report_memory_metrics().

%% @doc Recreate the per-process metric family to clear exited-process labels.
reset_process_info_metric() ->
    _ = prometheus_gauge:deregister(process_info),
    ok =
        prometheus_gauge:new(
            [
                {name, process_info},
                {labels, [process, type]},
                {help,
                    "Sampling info about active processes."
                    " Only set when process_sampler is enabled."}
            ]
        ).

%% @doc Report BEAM memory totals through the process_info metric family.
report_memory_metrics() ->
    prometheus_gauge:set(
        process_info,
        [<<"total">>, <<"memory">>],
        erlang:memory(total)
    ),
    prometheus_gauge:set(
        process_info,
        [<<"processes">>, <<"memory">>],
        erlang:memory(processes)
    ),
    prometheus_gauge:set(
        process_info,
        [<<"processes_used">>, <<"memory">>],
        erlang:memory(processes_used)
    ),
    prometheus_gauge:set(
        process_info,
        [<<"system">>, <<"memory">>],
        erlang:memory(system)
    ),
    prometheus_gauge:set(
        process_info,
        [<<"atom">>, <<"memory">>],
        erlang:memory(atom)
    ),
    prometheus_gauge:set(
        process_info,
        [<<"atom_used">>, <<"memory">>],
        erlang:memory(atom_used)
    ),
    prometheus_gauge:set(
        process_info,
        [<<"binary">>, <<"memory">>],
        erlang:memory(binary)
    ),
    prometheus_gauge:set(
        process_info,
        [<<"code">>, <<"memory">>],
        erlang:memory(code)
    ),
    prometheus_gauge:set(
        process_info,
        [<<"ets">>, <<"memory">>],
        erlang:memory(ets)
    ).

%% @doc Sample a single process and return aggregate data for it.
process_function(PID, _Opts) ->
    case process_info(
        PID,
        [
            current_stacktrace,
            registered_name,
            status,
            memory,
            reductions,
            message_queue_len
        ]
    ) of
        [{current_stacktrace, Stack},
                {registered_name, Name},
                {status, Status},
                {memory, Memory},
                {reductions, Reductions},
                {message_queue_len, MsgQueueLen}] ->
            ProcessName = process_name(Name, Stack),
            {true, {Status, ProcessName, Memory, Reductions, MsgQueueLen}};
        _ ->
            false
    end.

%% @doc Resolve a readable process name from its registration or stack.
process_name([], []) ->
    <<"unknown">>;
process_name([], Stack) ->
    InitialCall = initial_call(lists:reverse(Stack)),
    M = element(1, InitialCall),
    F = element(2, InitialCall),
    A = element(3, InitialCall),
    <<
        (hb_util:bin(M))/binary,
        ":",
        (hb_util:bin(F))/binary,
        "/",
        (hb_util:bin(A))/binary
    >>;
process_name(Name, _Stack) ->
    hb_util:bin(Name).

%% @doc Determine the initial stack frame for an anonymous process.
initial_call([]) ->
    {unknown, unknown, 0};
initial_call([{proc_lib, init_p_do_apply, _A, _Location} | Stack]) ->
    initial_call(Stack);
initial_call([InitialCall | _Stack]) ->
    InitialCall.

%%% Tests

%% @doc Ensure anonymous process names fall back to their initial call.
process_name_from_stack_test() ->
    ?assertEqual(
        <<"hb_http_server:init/2">>,
        process_name(
            [],
            [
                {hb_http_server, init, 2, []},
                {proc_lib, init_p_do_apply, 3, []}
            ]
        )
    ).

%% @doc Ensure registered names are returned directly.
process_name_registered_test() ->
    ?assertEqual(<<"my_proc">>, process_name(my_proc, [])).

%% @doc Ensure aggregate process metrics are summed by process name.
accumulate_process_metrics_test() ->
    Metrics =
        accumulate_process_metrics(
            [
                {running, <<"worker">>, 10, 20, 1},
                {running, <<"worker">>, 5, 3, 2}
            ]
        ),
    ?assertEqual({15, 23, 3}, maps:get(<<"worker">>, Metrics)).
