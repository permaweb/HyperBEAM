# [Module hb_beamr.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_beamr.erl)




BEAMR: A WAMR wrapper for BEAM.

<a name="description"></a>

## Description ##

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

```

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
               ImportFun must have an arity of 2: Taking an arbitrary <code>state</code>
               term, and a map containing the <code>port</code>, <code>module</code>, <code>func</code>, <code>args</code>,<code>signature</code>, and the <code>options</code> map of the import.
               It must return a tuple of the form {ok, Response, NewState}.
       serialize(Port) -> {ok, Mem}
           Where:
               Port is the port to the LID.
               Mem is a binary representing the full WASM state.
       deserialize(Port, Mem) -> ok
           Where:
               Port is the port to the LID.
               Mem is a binary output of a previous <code>serialize/1</code> call.
```

BEAMR was designed for use in the HyperBEAM project, but is suitable for
deployment in other Erlang applications that need to run WASM modules. PRs
are welcome.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#benchmark_test-0">benchmark_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#call-3">call/3</a></td><td>Call a function in the WASM executor (see moduledoc for more details).</td></tr><tr><td valign="top"><a href="#call-4">call/4</a></td><td></td></tr><tr><td valign="top"><a href="#call-5">call/5</a></td><td></td></tr><tr><td valign="top"><a href="#call-6">call/6</a></td><td></td></tr><tr><td valign="top"><a href="#deserialize-2">deserialize/2</a></td><td>Deserialize a WASM state from a binary.</td></tr><tr><td valign="top"><a href="#dispatch_response-2">dispatch_response/2*</a></td><td>Check the type of an import response and dispatch it to a Beamr port.</td></tr><tr><td valign="top"><a href="#driver_loads_test-0">driver_loads_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#imported_function_test-0">imported_function_test/0*</a></td><td>Test that imported functions can be called from the WASM module.</td></tr><tr><td valign="top"><a href="#is_valid_arg_list-1">is_valid_arg_list/1*</a></td><td>Check that a list of arguments is valid for a WASM function call.</td></tr><tr><td valign="top"><a href="#load_driver-0">load_driver/0*</a></td><td>Load the driver for the WASM executor.</td></tr><tr><td valign="top"><a href="#monitor_call-4">monitor_call/4*</a></td><td>Synchonously monitor the WASM executor for a call result and any
imports that need to be handled.</td></tr><tr><td valign="top"><a href="#multiclient_test-0">multiclient_test/0*</a></td><td>Ensure that processes outside of the initial one can interact with
the WASM executor.</td></tr><tr><td valign="top"><a href="#serialize-1">serialize/1</a></td><td>Serialize the WASM state to a binary.</td></tr><tr><td valign="top"><a href="#simple_wasm_test-0">simple_wasm_test/0*</a></td><td>Test standalone <code>hb_beamr</code> correctly after loading a WASM module.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start a WASM executor context.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop a WASM executor context.</td></tr><tr><td valign="top"><a href="#stub-3">stub/3</a></td><td>Stub import function for the WASM executor.</td></tr><tr><td valign="top"><a href="#wasm64_test-0">wasm64_test/0*</a></td><td>Test that WASM Memory64 modules load and execute correctly.</td></tr><tr><td valign="top"><a href="#wasm_send-2">wasm_send/2</a></td><td></td></tr><tr><td valign="top"><a href="#worker-2">worker/2*</a></td><td>A worker process that is responsible for handling a WASM instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="benchmark_test-0"></a>

### benchmark_test/0 * ###

`benchmark_test() -> any()`

<a name="call-3"></a>

### call/3 ###

`call(PID, FuncRef, Args) -> any()`

Call a function in the WASM executor (see moduledoc for more details).

<a name="call-4"></a>

### call/4 ###

`call(PID, FuncRef, Args, ImportFun) -> any()`

<a name="call-5"></a>

### call/5 ###

`call(PID, FuncRef, Args, ImportFun, StateMsg) -> any()`

<a name="call-6"></a>

### call/6 ###

`call(PID, FuncRef, Args, ImportFun, StateMsg, Opts) -> any()`

<a name="deserialize-2"></a>

### deserialize/2 ###

`deserialize(WASM, Bin) -> any()`

Deserialize a WASM state from a binary.

<a name="dispatch_response-2"></a>

### dispatch_response/2 * ###

`dispatch_response(WASM, Term) -> any()`

Check the type of an import response and dispatch it to a Beamr port.

<a name="driver_loads_test-0"></a>

### driver_loads_test/0 * ###

`driver_loads_test() -> any()`

<a name="imported_function_test-0"></a>

### imported_function_test/0 * ###

`imported_function_test() -> any()`

Test that imported functions can be called from the WASM module.

<a name="is_valid_arg_list-1"></a>

### is_valid_arg_list/1 * ###

`is_valid_arg_list(Args) -> any()`

Check that a list of arguments is valid for a WASM function call.

<a name="load_driver-0"></a>

### load_driver/0 * ###

`load_driver() -> any()`

Load the driver for the WASM executor.

<a name="monitor_call-4"></a>

### monitor_call/4 * ###

`monitor_call(WASM, ImportFun, StateMsg, Opts) -> any()`

Synchonously monitor the WASM executor for a call result and any
imports that need to be handled.

<a name="multiclient_test-0"></a>

### multiclient_test/0 * ###

`multiclient_test() -> any()`

Ensure that processes outside of the initial one can interact with
the WASM executor.

<a name="serialize-1"></a>

### serialize/1 ###

`serialize(WASM) -> any()`

Serialize the WASM state to a binary.

<a name="simple_wasm_test-0"></a>

### simple_wasm_test/0 * ###

`simple_wasm_test() -> any()`

Test standalone `hb_beamr` correctly after loading a WASM module.

<a name="start-1"></a>

### start/1 ###

`start(WasmBinary) -> any()`

Start a WASM executor context. Yields a port to the LID, and the
imports and exports of the WASM module. Optionally, specify a mode
(wasm or aot) to indicate the type of WASM module being loaded.

<a name="start-2"></a>

### start/2 ###

`start(WasmBinary, Mode) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(WASM) -> any()`

Stop a WASM executor context.

<a name="stub-3"></a>

### stub/3 ###

`stub(Msg1, Msg2, Opts) -> any()`

Stub import function for the WASM executor.

<a name="wasm64_test-0"></a>

### wasm64_test/0 * ###

`wasm64_test() -> any()`

Test that WASM Memory64 modules load and execute correctly.

<a name="wasm_send-2"></a>

### wasm_send/2 ###

`wasm_send(WASM, Message) -> any()`

<a name="worker-2"></a>

### worker/2 * ###

`worker(Port, Listener) -> any()`

A worker process that is responsible for handling a WASM instance.
It wraps the WASM port, handling inputs and outputs from the WASM module.
The last sender to the port is always the recipient of its messages, so
be careful to ensure that there is only one active sender to the port at
any time.

