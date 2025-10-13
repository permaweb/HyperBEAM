%%% @doc Wrapper for incrementing prometheus counters.
-module(hb_event).
-export([counters/0, diff/1, diff/2]).
-export([log/1, log/2, log/3, log/4, log/5, log/6]).
-export([increment/3, increment/4, increment_callers/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OVERLOAD_QUEUE_LENGTH, 10000).

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
increment(global, _Message, _Opts, _Count) -> ignored;
increment(ao_core, _Message, _Opts, _Count) -> ignored;
increment(ao_internal, _Message, _Opts, _Count) -> ignored;
increment(ao_devices, _Message, _Opts, _Count) -> ignored;
increment(ao_subresolution, _Message, _Opts, _Count) -> ignored;
increment(signature_base, _Message, _Opts, _Count) -> ignored;
increment(id_base, _Message, _Opts, _Count) -> ignored;
increment(parsing, _Message, _Opts, _Count) -> ignored;
increment(Topic, Message, _Opts, Count) ->
    case parse_name(Message) of
        <<"debug", _/binary>> -> ignored;
        EventName ->
            TopicBin = parse_name(Topic),
            case find_event_server() of
                Pid when is_pid(Pid) ->
                    Pid ! {increment, TopicBin, EventName, Count};
                undefined ->
                    PID = spawn(fun() -> server() end),
                    hb_name:register(?MODULE, PID),
                    PID ! {increment, TopicBin, EventName, Count}
            end
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
    ets:tab2list(prometheus_counter_table).
-endif.

%% @doc Find the event server, creating it if it doesn't exist. We cache the
%% result in the process dictionary to avoid looking it up multiple times.
find_event_server() ->
    case erlang:get({event_server, ?MODULE}) of
        {cached, Pid} -> Pid;
        undefined ->
            PID =
                case hb_name:lookup(?MODULE) of
                    Pid when is_pid(Pid) -> Pid;
                    undefined ->
                        NewServer = spawn(fun() -> server() end),
                        hb_name:register(?MODULE, NewServer),
                        NewServer
                end,
            erlang:put({event_server, ?MODULE}, {cached, PID}),
            PID
    end.

server() ->
    await_prometheus_started(),
    prometheus_counter:declare(
        [
            {name, <<"event">>},
            {help, <<"AO-Core execution events">>},
            {labels, [topic, event]}
        ]),
    handle_events().
handle_events() ->
    receive
        {increment, TopicBin, EventName, Count} ->
            case erlang:process_info(self(), message_queue_len) of
                {message_queue_len, Len} when Len > ?OVERLOAD_QUEUE_LENGTH ->
                    % Print a warning, but do so less frequently the more 
                    % overloaded the system is.
                    case rand:uniform(max(1000, Len - ?OVERLOAD_QUEUE_LENGTH)) of
                        1 ->
                            ?debug_print(
                                {warning,
                                    prometheus_event_queue_overloading,
                                    {queue, Len},
                                    {current_message, EventName}
                                }
                            );
                        _ -> ignored
                    end;
                _ -> ignored
            end,
            prometheus_counter:inc(<<"event">>, [TopicBin, EventName], Count),
            handle_events()
    end.

%% @doc Delay the event server until prometheus is started.
await_prometheus_started() ->
    receive
        Msg ->
            case application:get_application(prometheus) of
                undefined -> await_prometheus_started();
                _ -> self() ! Msg, ok
            end
    end.

parse_name(Name) when is_tuple(Name) ->
    parse_name(element(1, Name));
parse_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
parse_name(Name) when is_binary(Name) ->
    Name;
parse_name(Name) when is_list(Name) ->
    iolist_to_binary(Name);
parse_name(_) -> no_event_name.

%%% Benchmark tests

%% @doc Benchmark the performance of a full log of an event.
benchmark_event_test() ->
    Iterations =
        hb_test_utils:benchmark(
            fun() ->
                log(test_module, {test, 1})
            end
        ),
    hb_test_utils:benchmark_print(<<"Recorded">>, <<"events">>, Iterations),
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
            end
        ),
    hb_test_utils:benchmark_print(<<"Looked-up">>, <<"topics">>, Iterations),
    ?assert(Iterations >= 1000),
    ok.

%% @doc Benchmark the performance of incrementing an event.
benchmark_increment_test() ->
    Iterations =
        hb_test_utils:benchmark(
            fun() -> increment(test_module, {test, 1}, #{}) end
        ),
    hb_test_utils:benchmark_print(<<"Incremented">>, <<"events">>, Iterations),
    ?assert(Iterations >= 1000),
    ok.