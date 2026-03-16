%%% @doc Wrapper for incrementing prometheus counters.
-module(hb_event).
-export([counters/0, diff/1, diff/2]).
-export([log/1, log/2, log/3, log/4, log/5, log/6]).
-export([increment/3, increment/4, increment_callers/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OVERLOAD_QUEUE_LENGTH, 10000).
-define(MAX_MEMORY, 1_000_000_000). % 1GB
-define(MAX_EVENT_NAME_LENGTH, 100).

-ifdef(NO_EVENTS).
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
    % Check if the debug_print option has the topic in it if set.
    case should_print(Topic, Opts) orelse should_print(Mod, Opts) of
        true -> hb_format:print(X, Mod, Func, Line, Opts);
        false -> X
    end,
    try increment(Topic, X, Opts) catch _:_ -> ok end,
    % Return the logged value to the caller. This allows callers to insert 
    % `?event(...)' macros into the flow of other executions, without having to
    % break functional style.
    X.
-endif.

%% @doc Determine if the topic should be printed. Uses a cache in the process
%% dictionary to avoid re-checking the same topic multiple times.
should_print(Topic, Opts) ->
    case erlang:get({event_print, Topic}) of
        {cached, X} -> X;
        undefined ->
            Result =
                case hb_opts:get(debug_print, false, Opts) of
                    EventList when is_list(EventList) ->
                        lists:member(Topic, EventList);
                    true -> true;
                    false -> false
                end,
            erlang:put({event_print, Topic}, {cached, Result}),
            Result
    end.

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
        {increment, TopicBin, EventName, Count} ->
            N2 = N + 1,
            case N2 rem 1000 of
                0 ->
                    check_overload(EventName);
                _ ->
                    ok
            end,
            prometheus_counter:inc(<<"event">>, [TopicBin, EventName], Count),
            handle_events(N2)
    end.

check_overload(EventName) ->
    case erlang:process_info(self(), message_queue_len) of
        {message_queue_len, Len} when Len > ?OVERLOAD_QUEUE_LENGTH ->
            {memory, MemorySize} = erlang:process_info(self(), memory),
            case rand:uniform(max(1000, Len - ?OVERLOAD_QUEUE_LENGTH)) of
                1 ->
                    ?debug_print(
                        {warning,
                            prometheus_event_queue_overloading,
                            {queue, Len},
                            {current_message, EventName},
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
                            {current_message, EventName}
                        }
                    ),
                    exit(memory_overload);
                _ -> no_action
            end;
        _ -> ignored
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
                should_print(test_module, DefaultOpts)
                    orelse should_print(test_event, DefaultOpts)
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

-ifdef(NO_EVENTS).
benchmark_drain_rate_test() -> ok.
-else.
benchmark_drain_rate_test() ->
    log(warmup, {warmup, 0}),
    timer:sleep(100),
    EventPid = hb_name:lookup(?MODULE),
    wait_drain(EventPid, 5000),
    N = 100000,
    fill_mailbox(EventPid, N),
    {DrainTime, _} = timer:tc(fun() ->
        wait_drain(EventPid, 30000)
    end),
    DrainRate = round(N / (DrainTime / 1_000_000)),
    hb_test_utils:benchmark_print(
        <<"Drained">>, <<"events">>, DrainRate, 1),
    ?assert(DrainRate >= 10000),
    ok.

fill_mailbox(_Pid, 0) -> ok;
fill_mailbox(Pid, N) ->
    Pid ! {increment, <<"bench">>, <<"drain">>, 1},
    fill_mailbox(Pid, N - 1).

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
