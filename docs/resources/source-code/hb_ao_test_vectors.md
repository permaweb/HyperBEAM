# [Module hb_ao_test_vectors.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_ao_test_vectors.erl)




Uses a series of different `Opts` values to test the resolution engine's
execution under different circumstances.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#as_path_test-1">as_path_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#basic_get_test-1">basic_get_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#basic_set_test-1">basic_set_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#continue_as_test-1">continue_as_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#deep_recursive_get_test-1">deep_recursive_get_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#deep_set_new_messages_test-0">deep_set_new_messages_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#deep_set_test-1">deep_set_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#deep_set_with_device_test-1">deep_set_with_device_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#denormalized_device_key_test-1">denormalized_device_key_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#device_excludes_test-1">device_excludes_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#device_exports_test-1">device_exports_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#device_with_default_handler_function_test-1">device_with_default_handler_function_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#device_with_handler_function_test-1">device_with_handler_function_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#exec_dummy_device-2">exec_dummy_device/2*</a></td><td>Ensure that we can read a device from the cache then execute it.</td></tr><tr><td valign="top"><a href="#gen_default_device-0">gen_default_device/0*</a></td><td>Create a simple test device that implements the default handler.</td></tr><tr><td valign="top"><a href="#gen_handler_device-0">gen_handler_device/0*</a></td><td>Create a simple test device that implements the handler key.</td></tr><tr><td valign="top"><a href="#generate_device_with_keys_using_args-0">generate_device_with_keys_using_args/0*</a></td><td>Generates a test device with three keys, each of which uses
progressively more of the arguments that can be passed to a device key.</td></tr><tr><td valign="top"><a href="#get_as_with_device_test-1">get_as_with_device_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#get_with_device_test-1">get_with_device_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#key_from_id_device_with_args_test-1">key_from_id_device_with_args_test/1*</a></td><td>Test that arguments are passed to a device key as expected.</td></tr><tr><td valign="top"><a href="#key_to_binary_test-1">key_to_binary_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#list_transform_test-1">list_transform_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#load_as_test-1">load_as_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#load_device_test-0">load_device_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#recursive_get_test-1">recursive_get_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_binary_key_test-1">resolve_binary_key_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_from_multiple_keys_test-1">resolve_from_multiple_keys_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_id_test-1">resolve_id_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_key_twice_test-1">resolve_key_twice_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_path_element_test-1">resolve_path_element_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_simple_test-1">resolve_simple_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#run_all_test_-0">run_all_test_/0*</a></td><td>Run each test in the file with each set of options.</td></tr><tr><td valign="top"><a href="#run_test-0">run_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#set_with_device_test-1">set_with_device_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#start_as_test-1">start_as_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#start_as_with_parameters_test-1">start_as_with_parameters_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#step_hook_test-1">step_hook_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_opts-0">test_opts/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_suite-0">test_suite/0*</a></td><td></td></tr><tr><td valign="top"><a href="#untrusted_load_device_test-0">untrusted_load_device_test/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="as_path_test-1"></a>

### as_path_test/1 * ###

`as_path_test(Opts) -> any()`

<a name="basic_get_test-1"></a>

### basic_get_test/1 * ###

`basic_get_test(Opts) -> any()`

<a name="basic_set_test-1"></a>

### basic_set_test/1 * ###

`basic_set_test(Opts) -> any()`

<a name="continue_as_test-1"></a>

### continue_as_test/1 * ###

`continue_as_test(Opts) -> any()`

<a name="deep_recursive_get_test-1"></a>

### deep_recursive_get_test/1 * ###

`deep_recursive_get_test(Opts) -> any()`

<a name="deep_set_new_messages_test-0"></a>

### deep_set_new_messages_test/0 * ###

`deep_set_new_messages_test() -> any()`

<a name="deep_set_test-1"></a>

### deep_set_test/1 * ###

`deep_set_test(Opts) -> any()`

<a name="deep_set_with_device_test-1"></a>

### deep_set_with_device_test/1 * ###

`deep_set_with_device_test(Opts) -> any()`

<a name="denormalized_device_key_test-1"></a>

### denormalized_device_key_test/1 * ###

`denormalized_device_key_test(Opts) -> any()`

<a name="device_excludes_test-1"></a>

### device_excludes_test/1 * ###

`device_excludes_test(Opts) -> any()`

<a name="device_exports_test-1"></a>

### device_exports_test/1 * ###

`device_exports_test(Opts) -> any()`

<a name="device_with_default_handler_function_test-1"></a>

### device_with_default_handler_function_test/1 * ###

`device_with_default_handler_function_test(Opts) -> any()`

<a name="device_with_handler_function_test-1"></a>

### device_with_handler_function_test/1 * ###

`device_with_handler_function_test(Opts) -> any()`

<a name="exec_dummy_device-2"></a>

### exec_dummy_device/2 * ###

`exec_dummy_device(SigningWallet, Opts) -> any()`

Ensure that we can read a device from the cache then execute it. By
extension, this will also allow us to load a device from Arweave due to the
remote store implementations.

<a name="gen_default_device-0"></a>

### gen_default_device/0 * ###

`gen_default_device() -> any()`

Create a simple test device that implements the default handler.

<a name="gen_handler_device-0"></a>

### gen_handler_device/0 * ###

`gen_handler_device() -> any()`

Create a simple test device that implements the handler key.

<a name="generate_device_with_keys_using_args-0"></a>

### generate_device_with_keys_using_args/0 * ###

`generate_device_with_keys_using_args() -> any()`

Generates a test device with three keys, each of which uses
progressively more of the arguments that can be passed to a device key.

<a name="get_as_with_device_test-1"></a>

### get_as_with_device_test/1 * ###

`get_as_with_device_test(Opts) -> any()`

<a name="get_with_device_test-1"></a>

### get_with_device_test/1 * ###

`get_with_device_test(Opts) -> any()`

<a name="key_from_id_device_with_args_test-1"></a>

### key_from_id_device_with_args_test/1 * ###

`key_from_id_device_with_args_test(Opts) -> any()`

Test that arguments are passed to a device key as expected.
Particularly, we need to ensure that the key function in the device can
specify any arity (1 through 3) and the call is handled correctly.

<a name="key_to_binary_test-1"></a>

### key_to_binary_test/1 * ###

`key_to_binary_test(Opts) -> any()`

<a name="list_transform_test-1"></a>

### list_transform_test/1 * ###

`list_transform_test(Opts) -> any()`

<a name="load_as_test-1"></a>

### load_as_test/1 * ###

`load_as_test(Opts) -> any()`

<a name="load_device_test-0"></a>

### load_device_test/0 * ###

`load_device_test() -> any()`

<a name="recursive_get_test-1"></a>

### recursive_get_test/1 * ###

`recursive_get_test(Opts) -> any()`

<a name="resolve_binary_key_test-1"></a>

### resolve_binary_key_test/1 * ###

`resolve_binary_key_test(Opts) -> any()`

<a name="resolve_from_multiple_keys_test-1"></a>

### resolve_from_multiple_keys_test/1 * ###

`resolve_from_multiple_keys_test(Opts) -> any()`

<a name="resolve_id_test-1"></a>

### resolve_id_test/1 * ###

`resolve_id_test(Opts) -> any()`

<a name="resolve_key_twice_test-1"></a>

### resolve_key_twice_test/1 * ###

`resolve_key_twice_test(Opts) -> any()`

<a name="resolve_path_element_test-1"></a>

### resolve_path_element_test/1 * ###

`resolve_path_element_test(Opts) -> any()`

<a name="resolve_simple_test-1"></a>

### resolve_simple_test/1 * ###

`resolve_simple_test(Opts) -> any()`

<a name="run_all_test_-0"></a>

### run_all_test_/0 * ###

`run_all_test_() -> any()`

Run each test in the file with each set of options. Start and reset
the store for each test.

<a name="run_test-0"></a>

### run_test/0 * ###

`run_test() -> any()`

<a name="set_with_device_test-1"></a>

### set_with_device_test/1 * ###

`set_with_device_test(Opts) -> any()`

<a name="start_as_test-1"></a>

### start_as_test/1 * ###

`start_as_test(Opts) -> any()`

<a name="start_as_with_parameters_test-1"></a>

### start_as_with_parameters_test/1 * ###

`start_as_with_parameters_test(Opts) -> any()`

<a name="step_hook_test-1"></a>

### step_hook_test/1 * ###

`step_hook_test(InitOpts) -> any()`

<a name="test_opts-0"></a>

### test_opts/0 * ###

`test_opts() -> any()`

<a name="test_suite-0"></a>

### test_suite/0 * ###

`test_suite() -> any()`

<a name="untrusted_load_device_test-0"></a>

### untrusted_load_device_test/0 * ###

`untrusted_load_device_test() -> any()`

