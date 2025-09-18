# dev_wasm

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_wasm.erl)

A device that executes a WASM image on messages using the Memory-64 
preview standard. In the backend, this device uses `beamr`: An Erlang wrapper 
for WAMR, the WebAssembly Micro Runtime.
The device has the following requirements and interface:
<pre>
    M1/Init ->
        Assumes:
            M1/process
            M1/[Prefix]/image
        Generates:
            /priv/[Prefix]/instance
            /priv/[Prefix]/import-resolver
        Side-effects:
            Creates a WASM executor loaded in memory of the HyperBEAM node.
    M1/Compute ->
        Assumes:
            M1/priv/[Prefix]/instance
            M1/priv/[Prefix]/import-resolver
            M1/process
            M2/message
            M2/message/function OR M1/function
            M2/message/parameters OR M1/parameters
        Generates:
            /results/[Prefix]/type
            /results/[Prefix]/output
        Side-effects:
            Calls the WASM executor with the message and process.
    M1/[Prefix]/state ->
        Assumes:
            M1/priv/[Prefix]/instance
        Generates:
            Raw binary WASM state
</pre>

---

## Exported Functions

- `cache_wasm_image/1`
- `cache_wasm_image/2`
- `compute/3`
- `import/3`
- `info/2`
- `init/3`
- `instance/3`
- `normalize/3`
- `snapshot/3`
- `terminate/3`

---

### info

A device that executes a WASM image on messages using the Memory-64 
Export all functions aside the `instance/3` function.

```erlang
info(_Msg1, _Opts) ->
    #{
        excludes => [instance]
    }.
```

### init

Boot a WASM image on the image stated in the `process/image` field of

```erlang
init(M1, M2, Opts) ->
    ?event(running_init),
    % Where we should read initial parameters from.
```

### default_import_resolver

Take a BEAMR import call and resolve it using `hb_ao`.

```erlang
default_import_resolver(Msg1, Msg2, Opts) ->
    #{
        instance := WASM,
        module := Module,
        func := Func,
        args := Args,
        func_sig := Signature
    } = Msg2,
    Prefix = dev_stack:prefix(Msg1, Msg2, Opts),
    {ok, Msg3} =
        hb_ao:resolve(
            hb_private:set(
                Msg1,
                #{ <<Prefix/binary, "/instance">> => WASM },
                Opts
            ),
            #{
                <<"path">> => <<"import">>,
                <<"module">> => list_to_binary(Module),
                <<"func">> => list_to_binary(Func),
                <<"args">> => Args,
                <<"func-sig">> => list_to_binary(Signature)
            },
            Opts
        ),
    NextState = hb_ao:get(state, Msg3, Opts),
    Response = hb_ao:get(results, Msg3, Opts),
    {ok, Response, NextState}.
```

### compute

Call the WASM executor with a message that has been prepared by a prior

```erlang
compute(RawM1, M2, Opts) ->
    % Normalize the message to have an open WASM instance, but no literal `State'.
```

### normalize

Normalize the message to have an open WASM instance, but no literal
Serialize the WASM state to a binary.

```erlang
normalize(RawM1, M2, Opts) ->
    ?event({normalize_raw_m1, RawM1}),
    M3 = 
        case instance(RawM1, M2, Opts) of
            not_found ->
                DeviceKey =
                    case hb_ao:get(<<"device-key">>, RawM1, Opts) of
                        not_found -> [];
                        Key -> [Key]
                    end,
                ?event(
                    {no_instance_attempting_to_get_snapshot,
                        {msg1, RawM1}, {device_key, DeviceKey}
                    }
                ),
                Memory = 
                    hb_ao:get(
                        [<<"snapshot">>] ++ DeviceKey ++ [<<"body">>],
                        {as, dev_message, RawM1},
                        Opts
                    ),
                case Memory of
                    not_found -> throw({error, no_wasm_instance_or_snapshot});
                    State ->
                        {ok, M1} = init(RawM1, State, Opts),
                        Res = hb_beamr:deserialize(instance(M1, M2, Opts), State),
                        ?event(snapshot, {wasm_deserialized, {result, Res}}),
                        M1
                end;
            _ ->
                ?event(wasm_instance_found_not_deserializing),
                RawM1
        end,
    dev_message:set(M3, #{ <<"snapshot">> => unset }, Opts).
```

### snapshot

Normalize the message to have an open WASM instance, but no literal
Serialize the WASM state to a binary.

```erlang
snapshot(M1, M2, Opts) ->
    ?event(snapshot, generating_snapshot),
    Instance = instance(M1, M2, Opts),
    {ok, Serialized} = hb_beamr:serialize(Instance),
    {ok,
        #{
            <<"body">> => Serialized
        }
    }.
```

### terminate

Tear down the WASM executor.

```erlang
terminate(M1, M2, Opts) ->
    ?event(terminate_called_on_dev_wasm),
    Prefix = dev_stack:prefix(M1, M2, Opts),
    Instance = instance(M1, M2, Opts),
    hb_beamr:stop(Instance),
    {ok, hb_private:set(M1,
        #{
            <<Prefix/binary, "/instance">> => unset
        },
        Opts
    )}.
```

### instance

Get the WASM instance from the message. Note that this function is exported

```erlang
instance(M1, M2, Opts) ->
    Prefix = dev_stack:prefix(M1, M2, Opts),
    Path = <<Prefix/binary, "/instance">>,
    ?event({searching_for_instance, Path, M1}),
    hb_private:get(Path, M1, Opts#{ hashpath => ignore }).
```

### import

Handle standard library calls by:

```erlang
import(Msg1, Msg2, Opts) ->
    % 1. Adjust the path to the stdlib.
```

### undefined_import_stub

Log the call to the standard library as an event, and write the

```erlang
undefined_import_stub(Msg1, Msg2, Opts) ->
    ?event({unimplemented_dev_wasm_call, {msg1, Msg1}, {msg2, Msg2}}),
    Prefix = dev_stack:prefix(Msg1, Msg2, Opts),
    UndefinedCallsPath =
        <<"state/results/", Prefix/binary, "/undefined-calls">>,
    Msg3 = hb_ao:set(
        Msg1,
        #{
            UndefinedCallsPath =>
                [
                    Msg2
                |
                    case hb_ao:get(UndefinedCallsPath, Msg1, Opts) of
                        not_found -> [];
                        X -> X
                    end
                ]
        },
        Opts
    ),
    {ok, #{ state => Msg3, results => [0] }}.
```

### init

```erlang
init() ->
    application:ensure_all_started(hb),
    hb:init().
```

### input_prefix_test

```erlang
input_prefix_test() ->
    init(),
    #{ <<"image">> := ImageID } = cache_wasm_image("test/test.wasm"),
    Msg1 =
        #{
            <<"device">> => <<"wasm-64@1.0">>,
            <<"input-prefix">> => <<"test-in">>,
            <<"test-in">> => #{ <<"image">> => ImageID }
        },
    {ok, Msg2} = hb_ao:resolve(Msg1, <<"init">>, #{}),
    ?event({after_init, Msg2}),
    Priv = hb_private:from_message(Msg2),
    ?assertMatch(
        {ok, Instance} when is_pid(Instance),
        hb_ao:resolve(Priv, <<"instance">>, #{})
    ),
    ?assertMatch(
        {ok, Fun} when is_function(Fun),
        hb_ao:resolve(Priv, <<"import-resolver">>, #{})
    ).
```

### process_prefixes_test

Test that realistic prefixing for a `dev_process` works --

```erlang
process_prefixes_test() ->
    init(),
    Msg1 =
        #{
            <<"device">> => <<"wasm-64@1.0">>,
            <<"output-prefix">> => <<"wasm">>,
            <<"input-prefix">> => <<"process">>,
            <<"process">> => cache_wasm_image("test/test.wasm")
        },
    {ok, Msg3} = hb_ao:resolve(Msg1, <<"init">>, #{}),
    ?event({after_init, Msg3}),
    Priv = hb_private:from_message(Msg3),
    ?assertMatch(
        {ok, Instance} when is_pid(Instance),
        hb_ao:resolve(Priv, <<"wasm/instance">>, #{})
    ),
    ?assertMatch(
        {ok, Fun} when is_function(Fun),
        hb_ao:resolve(Priv, <<"wasm/import-resolver">>, #{})
    ).
```

### init_test

```erlang
init_test() ->
    init(),
    Msg = cache_wasm_image("test/test.wasm"),
    {ok, Msg1} = hb_ao:resolve(Msg, <<"init">>, #{}),
    ?event({after_init, Msg1}),
    Priv = hb_private:from_message(Msg1),
    ?assertMatch(
        {ok, Instance} when is_pid(Instance),
        hb_ao:resolve(Priv, <<"instance">>, #{})
    ),
    ?assertMatch(
        {ok, Fun} when is_function(Fun),
        hb_ao:resolve(Priv, <<"import-resolver">>, #{})
    ).
```

### basic_execution_test

```erlang
basic_execution_test() ->
    ?assertEqual(
        {ok, [120.0]},
        test_run_wasm("test/test.wasm", <<"fac">>, [5.0], #{})
    ).
```

### basic_execution_64_test

```erlang
basic_execution_64_test() ->
    ?assertEqual(
        {ok, [120.0]},
        test_run_wasm("test/test-64.wasm", <<"fac">>, [5.0], #{})
    ).
```

### imported_function_test

```erlang
imported_function_test() ->
    ?assertEqual(
        {ok, [32]},
        test_run_wasm(
            "test/pow_calculator.wasm",
            <<"pow">>,
            [2, 5],
            #{
                <<"stdlib/my_lib">> =>
                    #{ <<"device">> => <<"test-device@1.0">> }
            }
        )
    ).
```

### benchmark_test

```erlang
benchmark_test() ->
    BenchTime = 0.5,
    init(),
    Msg0 = cache_wasm_image("test/test-64.wasm"),
    {ok, Msg1} = hb_ao:resolve(Msg0, <<"init">>, #{}),
    Msg2 =
        hb_maps:merge(
            Msg1,
            #{
                <<"function">> => <<"fac">>,
                <<"parameters">> => [5.0]
            },
			#{}
        ),
    Iterations =
        hb_test_utils:benchmark(
            fun() ->
                hb_ao:resolve(Msg2, <<"compute">>, #{})
            end,
            BenchTime
        ),
    ?event(benchmark, {scheduled, Iterations}),
    hb_test_utils:benchmark_print(
        <<"Through AO-Core:">>,
        <<"resolutions">>,
        Iterations,
        BenchTime
    ),
    ?assert(Iterations > 5),
    ok.
```

### state_export_and_restore_test

```erlang
state_export_and_restore_test() ->
    init(),
    % Generate a WASM message. We use the pow_calculator because it has a 
    % reasonable amount of memory to work with.
```

### cache_wasm_image

```erlang
cache_wasm_image(Image) ->
    cache_wasm_image(Image, #{}).
```

### cache_wasm_image

```erlang
cache_wasm_image(Image, Opts) ->
    {ok, Bin} = file:read_file(Image),
    Msg = #{ <<"body">> => Bin },
    {ok, ID} = hb_cache:write(Msg, Opts),
    #{
        <<"device">> => <<"wasm-64@1.0">>,
        <<"image">> => ID
    }.
```

### test_run_wasm

```erlang
test_run_wasm(File, Func, Params, AdditionalMsg) ->
    init(),
    Msg0 = cache_wasm_image(File),
    {ok, Msg1} = hb_ao:resolve(Msg0, <<"init">>, #{}),
    ?event({after_init, Msg1}),
    Msg2 =
        hb_maps:merge(
            Msg1,
            hb_ao:set(
                #{
                    <<"function">> => Func,
                    <<"parameters">> => Params
                },
                AdditionalMsg,
                #{ hashpath => ignore }
            ),
			#{}
        ),
    ?event({after_setup, Msg2}),
    {ok, StateRes} = hb_ao:resolve(Msg2, <<"compute">>, #{}),
    ?event({after_resolve, StateRes}),
    hb_ao:resolve(StateRes, <<"results/output">>, #{}).
```

---

*Generated from [dev_wasm.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_wasm.erl)*
