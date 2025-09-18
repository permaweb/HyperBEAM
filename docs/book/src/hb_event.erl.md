# hb_event

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_event.erl)

Wrapper for incrementing prometheus counters.

---

## Exported Functions

- `counters/0`
- `diff/1`
- `diff/2`
- `increment_callers/1`
- `increment/3`
- `increment/4`
- `log/1`
- `log/2`
- `log/3`
- `log/4`
- `log/5`
- `log/6`

---

### log

Wrapper for incrementing prometheus counters.

```erlang
log(_X) -> ok.
```

### log

Wrapper for incrementing prometheus counters.

```erlang
log(_Topic, _X) -> ok.
```

### log

Wrapper for incrementing prometheus counters.

```erlang
log(_Topic, _X, _Mod) -> ok.
```

### log

Wrapper for incrementing prometheus counters.

```erlang
log(_Topic, _X, _Mod, _Func) -> ok.
```

### log

Wrapper for incrementing prometheus counters.

```erlang
log(_Topic, _X, _Mod, _Func, _Line) -> ok.
```

### log

Wrapper for incrementing prometheus counters.

```erlang
log(_Topic, _X, _Mod, _Func, _Line, _Opts) -> ok.
-else.
```

### log

Debugging log logging function. For now, it just prints to standard

```erlang
log(X) -> log(global, X).
```

### log

Debugging log logging function. For now, it just prints to standard

```erlang
log(Topic, X) -> log(Topic, X, "").
```

### log

Debugging log logging function. For now, it just prints to standard

```erlang
log(Topic, X, Mod) -> log(Topic, X, Mod, undefined).
```

### log

Debugging log logging function. For now, it just prints to standard

```erlang
log(Topic, X, Mod, Func) -> log(Topic, X, Mod, Func, undefined).
```

### log

Debugging log logging function. For now, it just prints to standard

```erlang
log(Topic, X, Mod, Func, Line) -> log(Topic, X, Mod, Func, Line, #{}).
```

### log

Debugging log logging function. For now, it just prints to standard

```erlang
log(Topic, X, Mod, undefined, Line, Opts) -> log(Topic, X, Mod, "", Line, Opts);
```

### log

Debugging log logging function. For now, it just prints to standard

```erlang
log(Topic, X, Mod, Func, undefined, Opts) -> log(Topic, X, Mod, Func, "", Opts);
```

### log

Debugging log logging function. For now, it just prints to standard

```erlang
log(Topic, X, Mod, Func, Line, Opts) ->
    % Check if the debug_print option has the topic in it if set.
```

### should_print

Determine if the topic should be printed. Uses a cache in the process

```erlang
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
```

### handle_tracer

```erlang
handle_tracer(Topic, X, Opts) ->
	AllowedTopics = [http, ao_result],
	case lists:member(Topic, AllowedTopics) of
		true -> 
			case hb_opts:get(trace, undefined, Opts) of
				undefined -> 
					case tuple_to_list(X) of
						[_ | Rest] -> 
							try
								Map = maps:from_list(Rest),
								TopicOpts = hb_opts:get(opts, #{}, Map),
								case hb_opts:get(trace, undefined, TopicOpts) of
									undefined ->  ok;
									TracePID ->
                                        hb_tracer:record_step(TracePID, {Topic, X})
								end
							catch
								_:_ -> ok
							end;
						_ -> 
							ok
					end;
				TracePID -> hb_tracer:record_step(TracePID, {Topic, X})
			end;
		_ -> ok
	end.
```

### increment

Increment the counter for the given topic and message. Registers the

```erlang
increment(Topic, Message, Opts) ->
    increment(Topic, Message, Opts, 1).
```

### increment

```erlang
increment(global, _Message, _Opts, _Count) -> ignored;
```

### increment

```erlang
increment(ao_core, _Message, _Opts, _Count) -> ignored;
```

### increment

```erlang
increment(ao_internal, _Message, _Opts, _Count) -> ignored;
```

### increment

```erlang
increment(ao_devices, _Message, _Opts, _Count) -> ignored;
```

### increment

```erlang
increment(ao_subresolution, _Message, _Opts, _Count) -> ignored;
```

### increment

```erlang
increment(signature_base, _Message, _Opts, _Count) -> ignored;
```

### increment

```erlang
increment(id_base, _Message, _Opts, _Count) -> ignored;
```

### increment

```erlang
increment(parsing, _Message, _Opts, _Count) -> ignored;
```

### increment

```erlang
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
```

### increment_callers

Increment the call paths and individual upstream calling functions of

```erlang
increment_callers(Topic) ->
    increment_callers(Topic, erlang).
```

### increment_callers

```erlang
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
```

### counters

Return a message containing the current counter values for all logged

```erlang
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
```

### diff

Return the change in the event counters before and after executing the

```erlang
diff(Fun) ->
    diff(Fun, #{}).
```

### diff

```erlang
diff(Fun, Opts) ->
    EventsBefore = counters(),
    Res = Fun(),
    EventsAfter = counters(),
    {hb_message:diff(EventsBefore, EventsAfter, Opts), Res}.
```

### raw_counters

```erlang
raw_counters() ->
    [].
```

### raw_counters

```erlang
raw_counters() ->
    ets:tab2list(prometheus_counter_table).
```

### find_event_server

Find the event server, creating it if it doesn't exist. We cache the

```erlang
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
```

### server

```erlang
server() ->
    await_prometheus_started(),
    prometheus_counter:declare(
        [
            {name, <<"event">>},
            {help, <<"AO-Core execution events">>},
            {labels, [topic, event]}
        ]),
    handle_events().
```

### handle_events

```erlang
handle_events() ->
    receive
        {increment, TopicBin, EventName, Count} ->
            case erlang:process_info(self(), message_queue_len) of
                {message_queue_len, Len} when Len > ?OVERLOAD_QUEUE_LENGTH ->
                    % Print a warning, but do so less frequently the more 
                    % overloaded the system is.
```

### await_prometheus_started

Delay the event server until prometheus is started.

```erlang
await_prometheus_started() ->
    receive
        Msg ->
            case application:get_application(prometheus) of
                undefined -> await_prometheus_started();
                _ -> self() ! Msg, ok
            end
    end.
```

### parse_name

```erlang
parse_name(Name) when is_tuple(Name) ->
    parse_name(element(1, Name));
```

### parse_name

```erlang
parse_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8);
```

### parse_name

```erlang
parse_name(Name) when is_binary(Name) ->
    Name;
```

### parse_name

```erlang
parse_name(Name) when is_list(Name) ->
    iolist_to_binary(Name);
```

### parse_name

Benchmark the performance of a full log of an event.

```erlang
parse_name(_) -> no_event_name.
%%% Benchmark tests
```

### benchmark_event_test

Benchmark the performance of a full log of an event.

```erlang
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
```

### benchmark_print_lookup_test

Benchmark the performance of looking up whether a topic and module

```erlang
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
```

### benchmark_increment_test

Benchmark the performance of incrementing an event.

```erlang
benchmark_increment_test() ->
    Iterations =
        hb_test_utils:benchmark(
            fun() -> increment(test_module, {test, 1}, #{}) end
        ),
    hb_test_utils:benchmark_print(<<"Incremented">>, <<"events">>, Iterations),
    ?assert(Iterations >= 1000),
```

---

*Generated from [hb_event.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_event.erl)*
