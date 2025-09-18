# hb_beamr

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_beamr.erl)

BEAMR: A WAMR wrapper for BEAM.
Beamr is a library that allows you to run WASM modules in BEAM, using the
Webassembly Micro Runtime (WAMR) as its engine. Each WASM module is 
executed using a Linked-In Driver (LID) that is loaded into BEAM. It is
designed with a focus on supporting long-running WASM executions that 
interact with Erlang functions and processes easily.
Because each WASM module runs as an independent async worker, if you plan
to run many instances in parallel, you should be sure to configure the 
BEAM to have enough async worker threads enabled (see `erl +A N` in the
Erlang manuals).
The core API is simple:
<pre>
    start(WasmBinary) -> {ok, Port, Imports, Exports}
        Where:
            WasmBinary is the WASM binary to load.
            Port is the port to the LID.
            Imports is a list of tuples of the form {Module, Function,
                Args, Signature}.
            Exports is a list of tuples of the form {Function, Args,
                Signature}.
    stop(Port) -> ok
    call(Port, FunctionName, Args) -> {ok, Result}
        Where:
            FunctionName is the name of the function to call.
            Args is a list of Erlang terms (converted to WASM values by
                BEAMR) that match the signature of the function.
            Result is a list of Erlang terms (converted from WASM values).
    call(Port, FunName, Args[, Import, State, Opts]) -> {ok, Res, NewState}
        Where:
            ImportFun is a function that will be called upon each import.
            ImportFun must have an arity of 2: Taking an arbitrary `state`
            term, and a map containing the `port`, `module`, `func`, `args`,
            `signature`, and the `options` map of the import.
            It must return a tuple of the form {ok, Response, NewState}.
    serialize(Port) -> {ok, Mem}
        Where:
            Port is the port to the LID.
            Mem is a binary representing the full WASM state.
    deserialize(Port, Mem) -> ok
        Where:
            Port is the port to the LID.
            Mem is a binary output of a previous `serialize/1` call.
</pre>
BEAMR was designed for use in the HyperBEAM project, but is suitable for
deployment in other Erlang applications that need to run WASM modules. PRs
are welcome.

---

## Exported Functions

- `call/3`
- `call/4`
- `call/5`
- `call/6`
- `deserialize/2`
- `serialize/1`
- `start/1`
- `start/2`
- `stop/1`
- `stub/3`
- `wasm_send/2`

---

### load_driver

BEAMR: A WAMR wrapper for BEAM.
Load the driver for the WASM executor.

```erlang
load_driver() ->
    case erl_ddll:load(code:priv_dir(hb), ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, Error} -> {error, Error}
    end.
```

### start

Start a WASM executor context. Yields a port to the LID, and the

```erlang
start(WasmBinary) when is_binary(WasmBinary) ->
    start(WasmBinary, wasm).
```

### start

```erlang
start(WasmBinary, Mode) when is_binary(WasmBinary) ->
    ?event({loading_module, {bytes, byte_size(WasmBinary)}, Mode}),
    Self = self(),
    WASM = spawn(
        fun() ->
            ok = load_driver(),
            Port = open_port({spawn, "hb_beamr"}, []),
            Port ! {self(), {command, term_to_binary({init, WasmBinary, Mode})}},
            ?event({waiting_for_init_from, Port}),
            worker(Port, Self)
        end
    ),
    receive
        {execution_result, Imports, Exports} ->
            ?event(
                {wasm_init_success,
                    {imports, Imports},
                    {exports, Exports}}),
            {ok, WASM, Imports, Exports};
        {error, Error} ->
            ?event({wasm_init_error, Error}),
            stop(WASM),
            {error, Error}
    end.
```

### worker

A worker process that is responsible for handling a WASM instance.

```erlang
worker(Port, Listener) ->
    receive
        stop ->
            ?event({stop_invoked_for_beamr, self()}),
            case erlang:port_info(Port, id) of
                undefined ->
                    ok;
                _ ->
                    port_close(Port),
                    ok
            end,
            ok;
        {wasm_send, NewListener, Message} ->
            ?event({wasm_send, {listener, NewListener}, {message, Message}}),
            Port ! {self(), Message},
            worker(Port, NewListener);
        WASMResult ->
            ?event({wasm_result, {listener, Listener}, {result, WASMResult}}),
            Listener ! WASMResult,
            worker(Port, Listener)
    end.
```

### wasm_send

```erlang
wasm_send(WASM, Message) when is_pid(WASM) ->
    WASM ! {wasm_send, self(), Message},
    ok.
```

### stop

Stop a WASM executor context.

```erlang
stop(WASM) when is_pid(WASM) ->
    WASM ! stop,
    ok.
```

### call

Call a function in the WASM executor (see moduledoc for more details).

```erlang
call(PID, FuncRef, Args) ->
    {ok, Res, _} = call(PID, FuncRef, Args, fun stub/3),
    {ok, Res}.
```

### call

```erlang
call(PID, FuncRef, Args, ImportFun) ->
    call(PID, FuncRef, Args, ImportFun, #{}).
```

### call

```erlang
call(PID, FuncRef, Args, ImportFun, StateMsg) ->
    call(PID, FuncRef, Args, ImportFun, StateMsg, #{}).
```

### call

```erlang
call(PID, FuncRef, Args, ImportFun, StateMsg, Opts)
        when is_binary(FuncRef) ->
    call(PID, binary_to_list(FuncRef), Args, ImportFun, StateMsg, Opts);
```

### call

```erlang
call(WASM, FuncRef, Args, ImportFun, StateMsg, Opts) 
        when is_pid(WASM)
        andalso (is_list(FuncRef) or is_integer(FuncRef))
        andalso is_list(Args)
        andalso is_function(ImportFun)
        andalso is_map(Opts) ->
    case is_valid_arg_list(Args) of
        true ->
            ?event(
                {call_started,
                    WASM,
                    FuncRef,
                    Args,
                    ImportFun,
                    StateMsg,
                    Opts}),
            wasm_send(WASM,
                {command,
                    term_to_binary(
                        case is_integer(FuncRef) of
                            true -> {indirect_call, FuncRef, Args};
                            false -> {call, FuncRef, Args}
                        end
                    )
                }
            ),
            ?event({waiting_for_call_result, self(), WASM}),
            monitor_call(WASM, ImportFun, StateMsg, Opts);
        false ->
            {error, {invalid_args, Args}}
    end.
```

### stub

Stub import function for the WASM executor.

```erlang
stub(Msg1, _Msg2, _Opts) ->
    ?event(stub_stdlib_called),
    {ok, [0], Msg1}.
```

### monitor_call

Synchonously monitor the WASM executor for a call result and any

```erlang
monitor_call(WASM, ImportFun, StateMsg, Opts) ->
    receive
        {execution_result, Result} ->
            ?event({call_result, Result}),
            {ok, Result, StateMsg};
        {import, Module, Func, Args, Signature} ->
            ?event({import_called, Module, Func, Args, Signature}),
            try
                {ok, Res, StateMsg2} =
                    ImportFun(StateMsg,
                        #{
                            instance => WASM,
                            module => Module,
                            func => Func,
                            args => Args,
                            func_sig => Signature
                        },
                        Opts
                    ),
                ?event({import_ret, Module, Func, {args, Args}, {res, Res}}),
                dispatch_response(WASM, Res),
                monitor_call(WASM, ImportFun, StateMsg2, Opts)
            catch
                Err:Reason:Stack ->
                    % Signal the WASM executor to stop.
```

### dispatch_response

Check the type of an import response and dispatch it to a Beamr port.

```erlang
dispatch_response(WASM, Term) when is_pid(WASM) ->
	case is_valid_arg_list(Term) of
		true ->
			wasm_send(WASM,
				{command, term_to_binary({import_response, Term})});
		false ->
			throw({error, {invalid_response, Term}})
	end;
```

### dispatch_response

Check the type of an import response and dispatch it to a Beamr port.

```erlang
dispatch_response(_WASM, Term) ->
	throw({error, {invalid_response, Term}}).
```

### is_valid_arg_list

Check that a list of arguments is valid for a WASM function call.

```erlang
is_valid_arg_list(Args) when is_list(Args) ->
    lists:all(fun(Arg) -> is_integer(Arg) or is_float(Arg) end, Args);
```

### is_valid_arg_list

Check that a list of arguments is valid for a WASM function call.

```erlang
is_valid_arg_list(_) ->
    false.
```

### serialize

Serialize the WASM state to a binary.

```erlang
serialize(WASM) when is_pid(WASM) ->
    ?event(starting_serialize),
    {ok, Size} = hb_beamr_io:size(WASM),
    ?event({image_size, Size}),
    {ok, Mem} = hb_beamr_io:read(WASM, 0, Size),
    ?event({finished_serialize, byte_size(Mem)}),
    {ok, Mem}.
```

### deserialize

Deserialize a WASM state from a binary.

```erlang
deserialize(WASM, Bin) when is_pid(WASM) andalso is_binary(Bin) ->
    ?event(starting_deserialize),
    Res = hb_beamr_io:write(WASM, 0, Bin),
    ?event({finished_deserialize, Res}),
    ok.
```

### driver_loads_test

```erlang
driver_loads_test() ->
    ?assertEqual(ok, load_driver()).
```

### simple_wasm_test

Test standalone `hb_beamr` correctly after loading a WASM module.

```erlang
simple_wasm_test() ->
    {ok, File} = file:read_file("test/test.wasm"),
    {ok, WASM, _Imports, _Exports} = start(File),
    {ok, [Result]} = call(WASM, "fac", [5.0]),
    ?assertEqual(120.0, Result).
```

### imported_function_test

Test that imported functions can be called from the WASM module.

```erlang
imported_function_test() ->
    {ok, File} = file:read_file("test/pow_calculator.wasm"),
    {ok, WASM, _Imports, _Exports} = start(File),
    {ok, [Result], _} =
        call(WASM, <<"pow">>, [2, 5],
            fun(Msg1, #{ args := [Arg1, Arg2] }, _Opts) ->
                {ok, [Arg1 * Arg2], Msg1}
            end),
    ?assertEqual(32, Result).
```

### wasm64_test

Test that WASM Memory64 modules load and execute correctly.

```erlang
wasm64_test() ->
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, WASM, _ImportMap, _Exports} = start(File),
    {ok, [Result]} = call(WASM, "fac", [5.0]),
    ?assertEqual(120.0, Result).
```

### multiclient_test

Ensure that processes outside of the initial one can interact with

```erlang
multiclient_test() ->
    Self = self(),
    ExecPID = spawn(fun() ->
        receive {wasm, WASM} ->
            {ok, [Result]} = call(WASM, "fac", [5.0]),
            Self ! {result, Result}
        end
    end),
    _StartPID = spawn(fun() ->
        {ok, File} = file:read_file("test/test.wasm"),
        {ok, WASM, _ImportMap, _Exports} = start(File),
        ExecPID ! {wasm, WASM}
    end),
    receive
        {result, Result} ->
            ?assertEqual(120.0, Result)
    end.
```

### benchmark_test

```erlang
benchmark_test() ->
    BenchTime = 1,
    {ok, File} = file:read_file("test/test-64.wasm"),
    {ok, WASM, _ImportMap, _Exports} = start(File),
    Iterations = hb_test_utils:benchmark(
        fun() ->
            {ok, [Result]} = call(WASM, "fac", [5.0]),
            ?assertEqual(120.0, Result)
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    ?assert(Iterations > 1000),
    hb_test_utils:benchmark_print(
        <<"Direct beamr: Executed">>,
        <<"calls">>,
        Iterations,
        BenchTime
    ),
```

---

*Generated from [hb_beamr.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_beamr.erl)*
