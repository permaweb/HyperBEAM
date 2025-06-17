# [Module dev_wasm.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_wasm.erl)




A device that executes a WASM image on messages using the Memory-64
preview standard.

<a name="description"></a>

## Description ##

In the backend, this device uses `beamr`: An Erlang wrapper
for WAMR, the WebAssembly Micro Runtime.

The device has the following requirements and interface:

```

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
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#basic_execution_64_test-0">basic_execution_64_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#basic_execution_test-0">basic_execution_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#benchmark_test-0">benchmark_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#cache_wasm_image-1">cache_wasm_image/1</a></td><td></td></tr><tr><td valign="top"><a href="#cache_wasm_image-2">cache_wasm_image/2</a></td><td></td></tr><tr><td valign="top"><a href="#compute-3">compute/3</a></td><td>Call the WASM executor with a message that has been prepared by a prior
pass.</td></tr><tr><td valign="top"><a href="#default_import_resolver-3">default_import_resolver/3*</a></td><td>Take a BEAMR import call and resolve it using <code>hb_ao</code>.</td></tr><tr><td valign="top"><a href="#import-3">import/3</a></td><td>Handle standard library calls by:
1.</td></tr><tr><td valign="top"><a href="#imported_function_test-0">imported_function_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>Export all functions aside the <code>instance/3</code> function.</td></tr><tr><td valign="top"><a href="#init-0">init/0*</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>Boot a WASM image on the image stated in the <code>process/image</code> field of
the message.</td></tr><tr><td valign="top"><a href="#init_test-0">init_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#input_prefix_test-0">input_prefix_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#instance-3">instance/3</a></td><td>Get the WASM instance from the message.</td></tr><tr><td valign="top"><a href="#normalize-3">normalize/3</a></td><td>Normalize the message to have an open WASM instance, but no literal
<code>State</code> key.</td></tr><tr><td valign="top"><a href="#process_prefixes_test-0">process_prefixes_test/0*</a></td><td>Test that realistic prefixing for a <code>dev_process</code> works --
including both inputs (from <code>Process/</code>) and outputs (to the
Device-Key) work.</td></tr><tr><td valign="top"><a href="#snapshot-3">snapshot/3</a></td><td>Serialize the WASM state to a binary.</td></tr><tr><td valign="top"><a href="#state_export_and_restore_test-0">state_export_and_restore_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-3">terminate/3</a></td><td>Tear down the WASM executor.</td></tr><tr><td valign="top"><a href="#test_run_wasm-4">test_run_wasm/4*</a></td><td></td></tr><tr><td valign="top"><a href="#undefined_import_stub-3">undefined_import_stub/3*</a></td><td>Log the call to the standard library as an event, and write the
call details into the message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="basic_execution_64_test-0"></a>

### basic_execution_64_test/0 * ###

`basic_execution_64_test() -> any()`

<a name="basic_execution_test-0"></a>

### basic_execution_test/0 * ###

`basic_execution_test() -> any()`

<a name="benchmark_test-0"></a>

### benchmark_test/0 * ###

`benchmark_test() -> any()`

<a name="cache_wasm_image-1"></a>

### cache_wasm_image/1 ###

`cache_wasm_image(Image) -> any()`

<a name="cache_wasm_image-2"></a>

### cache_wasm_image/2 ###

`cache_wasm_image(Image, Opts) -> any()`

<a name="compute-3"></a>

### compute/3 ###

`compute(RawM1, M2, Opts) -> any()`

Call the WASM executor with a message that has been prepared by a prior
pass.

<a name="default_import_resolver-3"></a>

### default_import_resolver/3 * ###

`default_import_resolver(Msg1, Msg2, Opts) -> any()`

Take a BEAMR import call and resolve it using `hb_ao`.

<a name="import-3"></a>

### import/3 ###

`import(Msg1, Msg2, Opts) -> any()`

Handle standard library calls by:
1. Adding the right prefix to the path from BEAMR.
2. Adding the state to the message at the stdlib path.
3. Resolving the adjusted-path-Msg2 against the added-state-Msg1.
4. If it succeeds, return the new state from the message.
5. If it fails with `not_found`, call the stub handler.

<a name="imported_function_test-0"></a>

### imported_function_test/0 * ###

`imported_function_test() -> any()`

<a name="info-2"></a>

### info/2 ###

`info(Msg1, Opts) -> any()`

Export all functions aside the `instance/3` function.

<a name="init-0"></a>

### init/0 * ###

`init() -> any()`

<a name="init-3"></a>

### init/3 ###

`init(M1, M2, Opts) -> any()`

Boot a WASM image on the image stated in the `process/image` field of
the message.

<a name="init_test-0"></a>

### init_test/0 * ###

`init_test() -> any()`

<a name="input_prefix_test-0"></a>

### input_prefix_test/0 * ###

`input_prefix_test() -> any()`

<a name="instance-3"></a>

### instance/3 ###

`instance(M1, M2, Opts) -> any()`

Get the WASM instance from the message. Note that this function is exported
such that other devices can use it, but it is excluded from calls from AO-Core
resolution directly.

<a name="normalize-3"></a>

### normalize/3 ###

`normalize(RawM1, M2, Opts) -> any()`

Normalize the message to have an open WASM instance, but no literal
`State` key. Ensure that we do not change the hashpath during this process.

<a name="process_prefixes_test-0"></a>

### process_prefixes_test/0 * ###

`process_prefixes_test() -> any()`

Test that realistic prefixing for a `dev_process` works --
including both inputs (from `Process/`) and outputs (to the
Device-Key) work

<a name="snapshot-3"></a>

### snapshot/3 ###

`snapshot(M1, M2, Opts) -> any()`

Serialize the WASM state to a binary.

<a name="state_export_and_restore_test-0"></a>

### state_export_and_restore_test/0 * ###

`state_export_and_restore_test() -> any()`

<a name="terminate-3"></a>

### terminate/3 ###

`terminate(M1, M2, Opts) -> any()`

Tear down the WASM executor.

<a name="test_run_wasm-4"></a>

### test_run_wasm/4 * ###

`test_run_wasm(File, Func, Params, AdditionalMsg) -> any()`

<a name="undefined_import_stub-3"></a>

### undefined_import_stub/3 * ###

`undefined_import_stub(Msg1, Msg2, Opts) -> any()`

Log the call to the standard library as an event, and write the
call details into the message.

