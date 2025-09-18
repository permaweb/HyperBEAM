# hb_tracer

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_tracer.erl)

A module for tracing the flow of requests through the system.
This allows for tracking the lifecycle of a request from HTTP receipt through processing and response.

---

## Exported Functions

- `format_error_trace/1`
- `get_trace/1`
- `record_step/2`
- `start_trace/0`

---

### start_trace

A module for tracing the flow of requests through the system.
Start a new tracer acting as queue of events registered.

```erlang
start_trace() ->
    Trace = #{steps => queue:new()},
    TracePID = spawn(fun() -> trace_loop(Trace) end),
    ?event(trace, {trace_started, TracePID}),
    TracePID.
```

### trace_loop

```erlang
trace_loop(Trace) ->
    receive
        {record_step, Step} ->
            Steps = maps:get(steps, Trace),
            NewTrace = Trace#{steps => queue:in(Step, Steps)},
            ?event(trace, {step_recorded, Step}),
            trace_loop(NewTrace);
        {get_trace, From} ->
            % Convert queue to list for the response
            TraceWithList =
                Trace#{steps =>
                           queue:to_list(
                               maps:get(steps, Trace))},
            From ! {trace, TraceWithList},
            trace_loop(Trace)
    end.
```

### record_step

Register a new step into a tracer

```erlang
record_step(TracePID, Step) ->
    TracePID ! {record_step, Step}.
```

### get_trace

Exports the complete queue of events

```erlang
get_trace(TracePID) ->
    TracePID ! {get_trace, self()},
    receive
        {trace, Trace} ->
            Trace
    after 5000 ->
        ?event(trace, {trace_timeout, TracePID}),
        {trace, #{}}
    end.
```

### format_error_trace

Format a trace for error in a user-friendly emoji oriented output

```erlang
format_error_trace(Trace) ->
    Steps = maps:get(steps, Trace, []),
    TraceMap =
        lists:foldl(fun(TraceItem, Acc) ->
                       case TraceItem of
                           {http, {parsed_singleton, _ReqSingleton, _}} ->
                               maps:put(request_parsing, true, Acc);
                           {ao_core, {stage, Stage, _Task}} ->
                                maps:put(resolve_stage, Stage, Acc);
                           {ao_result,
                            {load_device_failed, _, _, _, _, {exec_exception, Exception}, _, _}} ->
                               maps:put(error, Exception, Acc);
                           {ao_result,
                            {exec_failed,
                             _,
                             _,
                             _,
                             {func, Fun},
                             _,
                             {exec_exception, Error},
                             _,
                             _}} ->
                               maps:put(error, {Fun, Error}, Acc);
                           _ -> Acc
                       end
                    end,
                    #{},
                    Steps),
    % Build the trace message
    TraceStrings = <<"Oops! Something went wrong. Here's the rundown:">>,
    % Add parsing status
    ParsingTrace =
        case maps:get(request_parsing, TraceMap, false) of
            false ->
                Emoji = failure_emoji(),
                <<TraceStrings/binary, "\n", Emoji/binary, "Parsing your request">>;
            true ->
                Emoji = checkmark_emoji(),
                <<TraceStrings/binary, "\n", Emoji/binary, "Parsing your request">>
        end,
    % Add stage information
    StageTrace =
        case maps:get(resolve_stage, TraceMap, undefined) of
            undefined ->
                ParsingTrace;
            Stage ->
                StageEmoji = stage_to_emoji(Stage),
                try << ParsingTrace/binary, "\n", StageEmoji/binary,
                        " Resolved steps of your execution" >>
                catch
                    error:badarg ->
                        iolist_to_binary(io_lib:format("~p", [ParsingTrace]))
                end
        end,
    % Add error information
    case maps:get(error, TraceMap, undefined) of
        undefined ->
            StageTrace;
        {Fun, Reason} ->
            FailureEmoji = failure_emoji(),
            ErrMsg = list_to_binary(io_lib:format("~p -> ~p", [Fun, Reason])),
            <<StageTrace/binary, "\n", FailureEmoji/binary, "Error ", ErrMsg/binary>>;
        Error ->
            FailureEmoji = failure_emoji(),
            <<StageTrace/binary, "\n", FailureEmoji/binary, "Error ", Error/binary>>
    end.
```

### checkmark_emoji

```erlang
checkmark_emoji() ->
    % Unicode for checkmark
    <<"\xE2\x9C\x85">>. % \xE2\x9C\x85 is the checkmark emoji in UTF-8
```

### failure_emoji

```erlang
failure_emoji() ->
    % Unicode for failure emoji
    <<"\xE2\x9D\x8C">>. % \xE2\x9D\x8C is the failure emoji in UTF-8
% Helper function to convert stage number to emoji
```

### stage_to_emoji

```erlang
stage_to_emoji(Stage) when Stage >= 1, Stage =< 9 ->
    % Unicode for circled numbers 1-9
    StageEmoji = Stage + 48,
    <<StageEmoji, 16#E2, 16#83, 16#A3>>;
```

### stage_to_emoji

```erlang
stage_to_emoji(_) ->
    "".
```

---

*Generated from [hb_tracer.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_tracer.erl)*
