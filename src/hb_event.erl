%%% @doc Wrapper for incrementing prometheus counters.
-module(hb_event).
-export([counters/0, diff/1, diff/2]).
-export([debug_print/4, debug_print/5, debug_print/6]).
-export([format_file_log/2]).
-export([log/1, log/2, log/3, log/4, log/5, log/6]).
-export([log_event/6]).
-export([setup_logger/0]).
-export([increment/3, increment/4, increment_callers/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OVERLOAD_QUEUE_LENGTH, 10_000).
-define(MAX_MEMORY, 50_000_000). % 50 MB
-define(MAX_EVENT_NAME_LENGTH, 100).
%% OTP handler for logging to disk.
-define(PRINT_LOGGER, hb_print_logger).
-define(PRINT_LOGGER_DOMAIN, [hb_print]).
-define(FILE_LOGGER, hb_file_logger).
%% OTP logger domain, logs sent with this domain are directed to hb_file_logger.
-define(FILE_LOGGER_DOMAIN, [hb_log]).
-define(DEFAULT_PRINT_HANDLER_FILTER, hb_drop_hb_print_logs).
-define(DEFAULT_FILE_HANDLER_FILTER, hb_drop_hb_file_logs).

-ifdef(NO_EVENTS).
debug_print(_X, _Mod, _Func, _Line) -> ok.
debug_print(_X, _Mod, _Func, _Line, _Opts) -> ok.
debug_print(_Topic, _X, _Mod, _Func, _Line, _Opts) -> ok.
log(_X) -> ok.
log(_Topic, _X) -> ok.
log(_Topic, _X, _Mod) -> ok.
log(_Topic, _X, _Mod, _Func) -> ok.
log(_Topic, _X, _Mod, _Func, _Line) -> ok.
log(_Topic, _X, _Mod, _Func, _Line, _Opts) -> ok.
-else.
%% @doc Debugging log logging function. For now, it just prints to standard
%% error.
log(X) -> log(global, X).
log(Topic, X) -> log(Topic, X, "").
log(Topic, X, Mod) -> log(Topic, X, Mod, undefined).
log(Topic, X, Mod, Func) -> log(Topic, X, Mod, Func, undefined).
log(Topic, X, Mod, Func, Line) -> log(Topic, X, Mod, Func, Line, #{}).
log(Topic, X, Mod, undefined, Line, Opts) -> log(Topic, X, Mod, "", Line, Opts);
log(Topic, X, Mod, Func, undefined, Opts) -> log(Topic, X, Mod, Func, "", Opts);
log(Topic, X, Mod, Func, Line, Opts) ->
    debug_print(Topic, X, Mod, Func, Line, Opts),
    try increment(Topic, X, Opts) catch _:_ -> ok end,
    % Return the logged value to the caller. This allows callers to insert 
    % `?event(...)' macros into the flow of other executions, without having to
    % break functional style.
    X.

debug_print(X, Mod, Func, Line) ->
    debug_print(X, Mod, Func, Line, #{}).
debug_print(X, Mod, Func, Line, Opts) ->
    debug_print(debug_print, X, Mod, Func, Line, Opts).
debug_print(Topic, X, Mod, Func, Line, Opts) ->
    case should_print(print, Topic, Opts)
        orelse should_print(print, Mod, Opts)
    of
        true -> print_event(Topic, X, Mod, Func, Line, Opts);
        false -> X
    end,
    case should_print(log, Topic, Opts)
        orelse should_print(log, Mod, Opts)
    of
        true -> log_event(Topic, X, Mod, Func, Line, Opts);
        false -> ok
    end,
    X.
-endif.

%% @doc Determine if the topic should be printed or logged. Uses a cache in the
%% process dictionary to avoid re-checking the same topic multiple times.
should_print(Type, Topic, Opts) ->
    case erlang:get({event_print, Type, Topic}) of
        {cached, X} -> X;
        undefined ->
            Result =
                case hb_opts:get(print_opt(Type), false, Opts) of
                    EventList when is_list(EventList) ->
                        lists:member(Topic, EventList);
                    true -> true;
                    false -> false
                end,
            erlang:put({event_print, Type, Topic}, {cached, Result}),
            Result
    end.

print_opt(print) -> debug_print;
print_opt(log) -> debug_log.

%% @doc Configure a rotating file logger for HyperBEAM events.
setup_logger() ->
    LogFile =
        filename:join(
            hb_util:list(hb_opts:get(log_dir)),
            "hyperbeam.log"
        ),
    ok = filelib:ensure_dir(LogFile),
    setup_handler(
        ?PRINT_LOGGER,
        ?DEFAULT_PRINT_HANDLER_FILTER,
        ?PRINT_LOGGER_DOMAIN,
        print_logger_config()
    ),
    setup_handler(
        ?FILE_LOGGER,
        ?DEFAULT_FILE_HANDLER_FILTER,
        ?FILE_LOGGER_DOMAIN,
        file_logger_config(LogFile)
    ).

setup_handler(Handler, DefaultFilter, Domain, Config) ->
    ensure_default_handler_filter(DefaultFilter, Domain),
    logger:remove_handler(Handler),
    case logger:add_handler(Handler, logger_std_h, Config) of
        ok -> ok;
        {error, {handler_not_added, {already_exist, _}}} -> ok;
        {error, {handler_not_added, {already_started, _}}} -> ok;
        {error, {already_exist, _}} -> ok;
        {error, {already_started, _}} -> ok;
        {error, HandlerReason} -> erlang:error(HandlerReason)
    end.

%% @doc Build the OTP logger configuration for the HyperBEAM file handler.
file_logger_config(LogFile) ->
    logger_handler_config(
        ?FILE_LOGGER_DOMAIN,
        hb_log_domain,
        #{
            report_cb => fun ?MODULE:format_file_log/2,
            template => [time, " ", msg, "\n"],
            single_line => false
        },
        #{
            file => LogFile,
            max_no_bytes => hb_opts:get(log_max_bytes),
            max_no_files => hb_opts:get(log_max_files)
        }
    ).

print_logger_config() ->
    logger_handler_config(
        ?PRINT_LOGGER_DOMAIN,
        hb_print_domain,
        #{
            report_cb => fun ?MODULE:format_file_log/2,
            template => [msg, "\n"],
            single_line => false
        },
        #{type => standard_error}
    ).

logger_handler_config(Domain, FilterId, FormatterConfig, HandlerConfig) ->
    #{
        level => all,
        sync_mode_qlen => 200,
        drop_mode_qlen => 200,
        flush_qlen => 1000,
        burst_limit_enable => true,
        burst_limit_max_count => 500,
        burst_limit_window_time => 1000,
        filter_default => stop,
        filters =>
            [
                {
                    FilterId,
                    {fun logger_filters:domain/2, {log, sub, Domain}}
                }
            ],
        formatter => {logger_formatter, FormatterConfig},
        config => HandlerConfig
    }.

ensure_default_handler_filter(FilterId, Domain) ->
    logger:remove_handler_filter(default, FilterId),
    case logger:add_handler_filter(
        default,
        FilterId,
        {fun logger_filters:domain/2, {stop, sub, Domain}}
    ) of
        ok -> ok;
        {error, {already_exist, _}} -> ok;
        {error, FilterReason} -> erlang:error(FilterReason)
    end.

print_event(Topic, X, Mod, Func, Line, Opts) ->
    logger:log(
        notice,
        event_report(X, Mod, Func, Line, Opts),
        (event_metadata(Topic, Mod, Func, Line))#{
            domain => ?PRINT_LOGGER_DOMAIN
        }
    ).

%% @doc Queue an event for asynchronous file logging via OTP logger.
log_event(Topic, X, Mod, Func, Line, Opts) ->
    logger:log(
        notice,
        event_report(X, Mod, Func, Line, Opts),
        (event_metadata(Topic, Mod, Func, Line))#{
            domain => ?FILE_LOGGER_DOMAIN
        }
    ).

event_report(X, Mod, Func, Line, Opts) ->
    #{
        event => X,
        line => Line,
        function => Func,
        module => Mod,
        opts => Opts
    }.

event_metadata(Topic, Mod, Func, Line) ->
    #{
        line => Line,
        function => Func,
        module => Mod,
        topic => Topic
    }.

%% @doc Render the event log entry in the logger handler process.
format_file_log(
    #{event := X, line := Line, function := Func, module := Mod, opts := Opts},
    _Config
) ->
    hb_format:format_debug(X, Mod, Func, Line, Opts).

%% @doc Increment the counter for the given topic and message. Registers the
%% counter if it doesn't exist. If the topic is `global', the message is ignored.
%% This means that events must specify a topic if they want to be counted,
%% filtering debug messages.
%% 
%% This function uses a series of hard-coded topics to ignore explicitly in
%% order to quickly filter events that are executed so frequently that they
%% would otherwise cause heavy performance costs.
increment(Topic, Message, Opts) ->
    increment(Topic, Message, Opts, 1).
increment(ids, _Message, _Opts, _Count) -> ignored;
increment(global, _Message, _Opts, _Count) -> ignored;
increment(linkify, _Message, _Opts, _Count) -> ignored;
increment(debug_linkify, _Message, _Opts, _Count) -> ignored;
increment(debug_id, _Message, _Opts, _Count) -> ignored;
increment(debug_enc, _Message, _Opts, _Count) -> ignored;
increment(debug_commitments, _Message, _Opts, _Count) -> ignored;
increment(message_set, _Message, _Opts, _Count) -> ignored;
increment(read_cached, _Message, _Opts, _Count) -> ignored;
increment(ao_core, _Message, _Opts, _Count) -> ignored;
increment(ao_internal, _Message, _Opts, _Count) -> ignored;
increment(ao_devices, _Message, _Opts, _Count) -> ignored;
increment(ao_subresolution, _Message, _Opts, _Count) -> ignored;
increment(signature_base, _Message, _Opts, _Count) -> ignored;
increment(id_base, _Message, _Opts, _Count) -> ignored;
increment(parsing, _Message, _Opts, _Count) -> ignored;
increment(Topic, Message, _Opts, Count) ->
    case parse_name(Topic) of
        no_event_name -> ignored;
        <<"debug", _/binary>> -> ignored;
        TopicBin ->
            find_event_server() ! {increment, TopicBin, parse_name(Message), Count}
    end.

%% @doc Increment the call paths and individual upstream calling functions of
%% the current execution. This function generates the stacktrace itself. It is
%% **extremely** expensive, so it should only be used in very specific cases.
%% Do not ship code that calls this function to prod.
increment_callers(Topic) ->
    increment_callers(Topic, erlang).
increment_callers(Topic, Type) ->
    BinTopic = hb_util:bin(Topic),
    increment(
        <<BinTopic/binary, "-call-paths">>,
        hb_format:trace_short(Type),
        #{}
    ),
    lists:foreach(
        fun(Caller) ->
            increment(<<BinTopic/binary, "-callers">>, Caller, #{})
        end,
        hb_format:trace_to_list(hb_format:get_trace(Type))
    ).

%% @doc Return a message containing the current counter values for all logged
%% HyperBEAM events. The result comes in a form as follows:
%%      /GroupName/EventName -> Count
%% Where the `EventName` is derived from the value of the first term sent to the
%% `?event(...)' macros.
counters() ->
    UnaggregatedCounts =
        [
            {Group, Name, Count}
        ||
            {{default, <<"event">>, [Group, Name], _}, Count, _} <- raw_counters()
        ],
    lists:foldl(
        fun({Group, Name, Count}, Acc) -> 
            Acc#{
                Group => (maps:get(Group, Acc, #{}))#{
                    Name => maps:get(Name, maps:get(Group, Acc, #{}), 0) + Count
                }
            }
        end,
        #{},
        UnaggregatedCounts
    ).

%% @doc Return the change in the event counters before and after executing the
%% given function.
diff(Fun) ->
    diff(Fun, #{}).
diff(Fun, Opts) ->
    EventsBefore = counters(),
    Res = Fun(),
    EventsAfter = counters(),
    {hb_message:diff(EventsBefore, EventsAfter, Opts), Res}.

-ifdef(NO_EVENTS).
raw_counters() ->
    [].
-else.
raw_counters() ->
    ets:match_object(
        prometheus_counter_table,
        {{default, <<"event">>, '_', '_'}, '_', '_'}
    ).
-endif.

%% @doc Find the event server, creating it if it doesn't exist. We cache the
%% result in the process dictionary to avoid looking it up multiple times.
find_event_server() ->
    hb_name:singleton(?MODULE, fun() -> server() end).

server() ->
    hb_prometheus:ensure_started(),
    ensure_event_counter(),
    handle_events().

ensure_event_counter() ->
    hb_prometheus:declare(
        counter,
        [
            {name, <<"event">>},
            {help, <<"AO-Core execution events">>},
            {labels, [topic, event]}
        ]).

handle_events() ->
    handle_events(0).
handle_events(N) ->
    receive
        {increment, Topic, Event, Count} ->
            BatchCount = 0,
            prometheus_counter:inc(<<"event">>, [Topic, Event], Count + BatchCount),
            check_overload({Topic, Event}, N),
            handle_events(N + 1)
    end.

check_overload(Last, N) ->
    case N rem 1000 of
        0 ->
            case erlang:process_info(self(), message_queue_len) of
                {message_queue_len, Len} when Len > ?OVERLOAD_QUEUE_LENGTH ->
                    {memory, MemorySize} = erlang:process_info(self(), memory),
                    case rand:uniform(max(1000, Len - ?OVERLOAD_QUEUE_LENGTH)) of
                        1 ->
                            ?debug_print(
                                {warning,
                                    prometheus_event_queue_overloading,
                                    {queue, Len},
                                    {last_event, Last},
                                    {memory_bytes, MemorySize}
                                }
                            );
                        _ -> ignored
                    end,
                    case MemorySize of
                        MemorySize when MemorySize > ?MAX_MEMORY ->
                            ?debug_print(
                                {error,
                                    prometheus_event_queue_terminating_on_memory_overload,
                                    {queue, Len},
                                    {memory_bytes, MemorySize},
                                    {last_event, Last}
                                }
                            ),
                            exit(memory_overload);
                        _ -> no_action
                    end;
                _ -> ignored
            end;
        _ -> ok
    end.

parse_name(Name) when is_tuple(Name) ->
    parse_name(element(1, Name));
parse_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
parse_name(Name)
        when is_binary(Name)
        andalso byte_size(Name) > ?MAX_EVENT_NAME_LENGTH ->
    no_event_name;
parse_name(Name) when is_list(Name) ->
    iolist_to_binary(Name);
parse_name(Name) when is_binary(Name) ->
    Name;
parse_name(_) -> no_event_name.

%%% Benchmark tests

-define(BENCHMARK_DURATION, 0.25).
%% @doc Benchmark the performance of a full log of an event.
benchmark_event_test() ->
    Iterations =
        hb_test_utils:benchmark(
            fun() ->
                log(test_module, {test, 1})
            end,
            ?BENCHMARK_DURATION
        ),
    hb_test_utils:benchmark_print(<<"Recorded">>, <<"events">>, Iterations, ?BENCHMARK_DURATION),
    ?assert(Iterations >= 1000),
    ok.

%% @doc Benchmark the performance of looking up whether a topic and module
%% should be printed.
benchmark_print_lookup_test() ->
    DefaultOpts = hb_opts:default_message_with_env(),
    Iterations =
        hb_test_utils:benchmark(
            fun() ->
                should_print(print, test_module, DefaultOpts)
                    orelse should_print(print, test_event, DefaultOpts)
            end,
            ?BENCHMARK_DURATION
        ),
    hb_test_utils:benchmark_print(<<"Looked-up">>, <<"topics">>, Iterations, ?BENCHMARK_DURATION),
    ?assert(Iterations >= 1000),
    ok.

%% @doc Benchmark the performance of incrementing an event.
benchmark_increment_test() ->
    Iterations =
        hb_test_utils:benchmark(
            fun() -> increment(test_module, {test, 1}, #{}) end,
            ?BENCHMARK_DURATION
        ),
    hb_test_utils:benchmark_print(<<"Incremented">>, <<"events">>, Iterations, ?BENCHMARK_DURATION),
    ?assert(Iterations >= 1000),
    ok.

should_log_test() ->
    ?assertEqual(true, should_print(log, topic_a, #{ debug_log => [topic_a] })),
    ?assertEqual(false, should_print(log, topic_b, #{ debug_log => [topic_a] })),
    ?assertEqual(true, should_print(log, topic_c, #{ debug_log => true })),
    ?assertEqual(false, should_print(log, topic_d, #{ debug_log => false })).

-ifdef(NO_EVENTS).
benchmark_drain_rate_test() -> ok.
batch_correctness_test() -> ok.
overload_checks_past_first_thousand_test() -> ok.
-else.
benchmark_drain_rate_test() ->
    NumKeys = 50,
    NumEvents = 100000,
    log(warmup, {warmup, 0}),
    timer:sleep(100),
    EventPid = hb_name:lookup(?MODULE),
    wait_drain(EventPid, 5000),
    erlang:suspend_process(EventPid),
    Keys =
        [
            {
                hb_util:bin([<<"corr-topic-">>, hb_util:int(K)]),
                hb_util:bin([<<"corr-event-">>, hb_util:int(K)])
            }
        ||
            K <- lists:seq(1, NumKeys)
        ],
    fill_mailbox(EventPid, NumEvents, Keys),
    erlang:resume_process(EventPid),
    {DrainTime, _} =
        timer:tc(
            fun() ->
                wait_drain(EventPid, 30000)
            end
        ),
    DrainRate = round(NumEvents / (max(1, DrainTime) / 1_000_000)),
    hb_test_utils:benchmark_print(
        <<"Drained">>,
        <<"events">>,
        DrainRate,
        1
    ),
    ?assert(DrainRate >= 10000),
    ok.

batch_correctness_test() ->
    log(warmup, {warmup, 0}),
    timer:sleep(100),
    EventPid = hb_name:lookup(?MODULE),
    wait_drain(EventPid, 5000),
    NumKeys = 5,
    N = 30_000,
    Keys = [{list_to_binary("corr_topic_" ++ integer_to_list(K)),
             list_to_binary("corr_event_" ++ integer_to_list(K))}
            || K <- lists:seq(1, NumKeys)],
    Before = counters(),
    BeforeCounts = [{T, E, deep_get([T, E], Before, 0)} || {T, E} <- Keys],
    erlang:suspend_process(EventPid),
    lists:foreach(fun(I) ->
        {T, E} = lists:nth((I rem NumKeys) + 1, Keys),
        EventPid ! {increment, T, E, 1}
    end, lists:seq(1, N)),
    erlang:resume_process(EventPid),
    wait_drain(EventPid, 30000),
    After = counters(),
    PerKey = N div NumKeys,
    lists:foreach(fun({T, E, BeforeVal}) ->
        AfterVal = deep_get([T, E], After, 0),
        ?assertEqual(PerKey, AfterVal - BeforeVal)
    end, BeforeCounts),
    ok.

overload_checks_past_first_thousand_test() ->
    {EventPid, Ref} =
        spawn_monitor(
            fun() ->
                hb_prometheus:ensure_started(),
                ensure_event_counter(),
                handle_events(1000)
            end
        ),
    erlang:suspend_process(EventPid),
    Topic = lists:duplicate(256, $a),
    Event = lists:duplicate(256, $b),
    lists:foreach(
        fun(_) ->
            EventPid ! {increment, Topic, Event, 1}
        end,
        lists:seq(1, ?OVERLOAD_QUEUE_LENGTH + 100)
    ),
    {message_queue_len, QueueLen} =
        erlang:process_info(EventPid, message_queue_len),
    {memory, MemorySize} = erlang:process_info(EventPid, memory),
    ?assert(QueueLen > ?OVERLOAD_QUEUE_LENGTH),
    ?assert(MemorySize > ?MAX_MEMORY),
    erlang:resume_process(EventPid),
    receive
        {'DOWN', Ref, process, EventPid, memory_overload} ->
            ok;
        {'DOWN', Ref, process, EventPid, Reason} ->
            ?assertEqual(memory_overload, Reason)
    after 5000 ->
        exit(EventPid, kill),
        error(memory_overload_not_triggered)
    end.

deep_get([Group, Name], Map, Default) ->
    case maps:get(Group, Map, undefined) of
        undefined -> Default;
        Inner -> maps:get(Name, Inner, Default)
    end.

%% @doc Fill the event server mailbox with a list of keys. Rotate the keys to
%% ensure that we are testing the event server's ability to handle many different
%% types of event.
fill_mailbox(_Pid, 0, _Keys) -> ok;
fill_mailbox(Pid, N, Keys = [{Topic, Event}|_]) ->
    Pid ! {increment, Topic, Event, 1},
    fill_mailbox(Pid, N - 1, hb_util:shuffle(Keys)).

wait_drain(Pid, Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    wait_drain_loop(Pid, Deadline).

wait_drain_loop(Pid, Deadline) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, 0} -> ok;
        {message_queue_len, _} ->
            case erlang:monotonic_time(millisecond) >= Deadline of
                true -> error(drain_timeout);
                false ->
                    timer:sleep(10),
                    wait_drain_loop(Pid, Deadline)
            end;
        undefined ->
            error(event_server_dead)
    end.
-endif.
