# dev_profile

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_profile.erl)

A module for running different profiling tools upon HyperBEAM executions.
This device allows a variety of profiling tools to be used and for their
outputs to be returned as messages, or displayed locally on the console.
When called from an AO-Core request, the path at the given key is resolved.
If the `eval` function is instead directly invoked via Erlang, the first
argument may be a function to profile instead.

---

## Exported Functions

- `eval/1`
- `eval/2`
- `eval/3`
- `eval/4`
- `info/1`

---

### info

A module for running different profiling tools upon HyperBEAM executions.
Default to the `eval` function.

```erlang
info(_) ->
    #{
        excludes => [<<"keys">>, <<"set">>],
        default => fun eval/4
    }.
```

### eval

Invoke a profiling tool on a function or an AO-Core resolution. If a 

```erlang
eval(Fun) -> eval(Fun, #{}).
```

### eval

Invoke a profiling tool on a function or an AO-Core resolution. If a 

```erlang
eval(Fun, Opts) -> eval(Fun, #{}, Opts).
```

### eval

Invoke a profiling tool on a function or an AO-Core resolution. If a 

```erlang
eval(Fun, Req, Opts) when is_function(Fun) ->
    do_eval(
        Fun,
        case return_mode(Req, Opts, undefined) of
            undefined -> Req#{ <<"return-mode">> => <<"open">> };
            _ -> Req
        end,
        Opts
    );
```

### eval

Invoke a profiling tool on a function or an AO-Core resolution. If a 

```erlang
eval(Base, Request, Opts) ->
    eval(<<"eval">>, Base, Request, Opts).
```

### eval

Invoke a profiling tool on a function or an AO-Core resolution. If a 

```erlang
eval(PathKey, Base, Req, Opts) when not is_function(Base) ->
    case hb_ao:get(PathKey, Req, undefined, Opts) of
        undefined ->
            {
                error,
                <<
                    "Path key `",
                    (hb_util:bin(PathKey))/binary,
                    "` not found in request."
                >>
            };
        Path ->
            do_eval(
                fun() -> hb_ao:resolve(Req#{ <<"path">> => Path }, Opts) end,
                Req,
                Opts
            )
    end.
```

### do_eval

```erlang
do_eval(Fun, Req, Opts) ->
    % Validate the request and options, then invoke the engine-specific profile
    % function. We match the user-requested engine against the supported engines
    % on the node. Each engine takes three arguments:
    % 1. The function to profile.
```

### find_profiling_config

Find the profiling options. The supported options for `profiling` in the

```erlang
find_profiling_config(Opts) ->
    case hb_opts:get(profiling, not_found, Opts) of
        not_found ->
            case hb_opts:get(mode, prod, Opts) of
                prod -> false;
                _ -> hb_features:test()
            end;
        EnableProfiling -> EnableProfiling
    end.
```

### validate_enabled

Validate that profiling is enabled. 

```erlang
validate_enabled(Opts) ->
    case find_profiling_config(Opts) of
        false -> {validation_error, disabled};
        _ -> true
    end.
```

### validate_return_mode

Validate that the request return mode is acceptable. We only allow the

```erlang
validate_return_mode(Req, Opts) ->
    case return_mode(Req, Opts) of
        <<"open">> -> hb_opts:get(mode, prod, Opts) == debug;
        _ -> true
    end.
```

### validate_signer

Validate that the request is from a valid signer, if set by the node

```erlang
validate_signer(Req, Opts) ->
    case find_profiling_config(Opts) of
        ValidSigners when is_list(ValidSigners) ->
            lists:any(
                fun(Signer) -> lists:member(Signer, ValidSigners) end,
                hb_message:signers(Req, Opts)
            );
        EnableProfiling -> EnableProfiling
    end orelse {validation_error, invalid_signer}.
```

### engine

Return the profiling function for the given engine.

```erlang
engine(<<"eflame">>) -> {ok, fun eflame_profile/3};
```

### engine

Return the profiling function for the given engine.

```erlang
engine(<<"eprof">>) -> {ok, fun eprof_profile/3};
```

### engine

Return the profiling function for the given engine.

```erlang
engine(<<"event">>) -> {ok, fun event_profile/3};
```

### engine

Return the profiling function for the given engine.

```erlang
engine(default) -> {ok, default()};
```

### engine

Return the profiling function for the given engine.
Return the default profiling engine to use. `eflame` if preferred if

```erlang
engine(Unknown) -> {unknown_engine, Unknown}.
```

### default

Return the profiling function for the given engine.
Return the default profiling engine to use. `eflame` if preferred if

```erlang
default() ->
    case hb_features:eflame() of
        true -> fun eflame_profile/3;
        false -> fun eprof_profile/3
    end.
```

### eflame_profile

Profile a function using the `eflame` tool. This tool is only available

```erlang
eflame_profile(Fun, Req, Opts) ->
    File = temp_file(),
    Res = eflame:apply(normal, File, Fun, []),
    MergeStacks = hb_maps:get(<<"mode">>, Req, <<"merge">>, Opts),
    EflameDir = code:lib_dir(eflame),
    % Get the name of the function to profile. If the path in the request is
    % set, attempt to find it. If that is not found, we use the bare path.
```

### eflame_profile

```erlang
eflame_profile(_Fun, _Req, _Opts) ->
    {error, <<"eflame is not enabled.">>}.
-endif.
```

### eprof_profile

Profile a function using the `eprof` tool.

```erlang
eprof_profile(Fun, Req, Opts) ->
    File = temp_file(),
    % Attempt to profile the function, stopping the profiler afterwards.
```

### event_profile

Profile using HyperBEAM's events.

```erlang
event_profile(Fun, Req, Opts) ->
    Start = hb_event:counters(),
    Fun(),
    End = hb_event:counters(),
    Diff = hb_message:diff(Start, End, Opts),
    case return_mode(Req, Opts) of
        <<"message">> ->
            {ok, Diff};
        <<"console">> ->
            hb_format:print(Diff),
            {ok, Diff}
    end.
```

### return_mode

Get the return mode of a profiler run. The run mode is set to `console`

```erlang
return_mode(Req, Opts) ->
    return_mode(Req, Opts, <<"message">>).
```

### return_mode

Get the return mode of a profiler run. The run mode is set to `console`
Returns a temporary filename for use in a profiling run.

```erlang
return_mode(Req, Opts, Default) ->
    hb_ao:get(<<"return-mode">>, Req, Default, Opts).
```

### temp_file

Get the return mode of a profiler run. The run mode is set to `console`
Returns a temporary filename for use in a profiling run.

```erlang
temp_file() -> temp_file(<<"out">>).
```

### temp_file

Get the return mode of a profiler run. The run mode is set to `console`
Returns a temporary filename for use in a profiling run.

```erlang
temp_file(Ext) ->
    <<
        "profile-",
        (integer_to_binary(os:system_time(microsecond)))/binary,
        ".",
        Ext/binary
    >>.
```

### eprof_fun_test

```erlang
eprof_fun_test() -> test_engine(function, <<"eprof">>).
```

### eprof_resolution_test

```erlang
eprof_resolution_test() -> test_engine(resolution, <<"eprof">>).
-ifdef(ENABLE_EFLAME).
```

### eflame_fun_test

```erlang
eflame_fun_test() -> test_engine(function, <<"eflame">>).
```

### eflame_resolution_test

```erlang
eflame_resolution_test() -> test_engine(resolution, <<"eflame">>).
-endif.
```

### test_engine

Run a test and validate the output for a given engine.

```erlang
test_engine(Type, Engine) ->
    validate_profiler_output(Engine, test_profiler_exec(Type, Engine)).
```

### test_profiler_exec

Invoke an engine in either a function (as called from Erlang) or

```erlang
test_profiler_exec(function, Engine) ->
    eval(
        fun() -> dev_meta:build(#{}, #{}, #{}) end,
        #{ <<"engine">> => Engine, <<"return-mode">> => <<"message">> },
        #{}
    );
```

### test_profiler_exec

Invoke an engine in either a function (as called from Erlang) or

```erlang
test_profiler_exec(resolution, Engine) ->
    hb_ao:resolve(
        #{
            <<"path">> => <<"/~profile@1.0/run?run=/~meta@1.0/build">>,
            <<"engine">> => Engine, <<"return-mode">> => <<"message">> },
        #{}
    ).
```

### validate_profiler_output

Verify the expected type of output from a profiler.

```erlang
validate_profiler_output(<<"eprof">>, Res) ->
    ?assertMatch(
        {ok,
            #{
                <<"content-type">> := <<"text/plain">>,
                <<"body">> := Body
            }
        } when byte_size(Body) > 100,
        Res
    );
```

### validate_profiler_output

Verify the expected type of output from a profiler.

```erlang
validate_profiler_output(<<"eflame">>, Res) ->
    ?assertMatch(
        {ok,
            #{
                <<"content-type">> := <<"image/svg+xml">>,
                <<"body">> := Body
            }
        } when byte_size(Body) > 100,
        Res
```

---

*Generated from [dev_profile.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_profile.erl)*
