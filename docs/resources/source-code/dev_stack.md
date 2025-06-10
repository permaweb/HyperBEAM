# [Module dev_stack.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_stack.erl)




A device that contains a stack of other devices, and manages their
execution.

<a name="description"></a>

## Description ##

It can run in two modes: fold (the default), and map.

In fold mode, it runs upon input messages in the order of their keys. A
stack maintains and passes forward a state (expressed as a message) as it
progresses through devices.

For example, a stack of devices as follows:

```

   Device -> Stack
   Device-Stack/1/Name -> Add-One-Device
   Device-Stack/2/Name -> Add-Two-Device
```

When called with the message:

```

   #{ Path = "FuncName", binary => <code><<"0">></code> }
```

Will produce the output:

```

   #{ Path = "FuncName", binary => <code><<"3">></code> }
   {ok, #{ bin => <code><<"3">></code> }}
```

In map mode, the stack will run over all the devices in the stack, and
combine their results into a single message. Each of the devices'
output values have a key that is the device's name in the `Device-Stack`
(its number if the stack is a list).

You can switch between fold and map modes by setting the `Mode` key in the
`Msg2` to either `Fold` or `Map`, or set it globally for the stack by
setting the `Mode` key in the `Msg1` message. The key in `Msg2` takes
precedence over the key in `Msg1`.

The key that is called upon the device stack is the same key that is used
upon the devices that are contained within it. For example, in the above
scenario we resolve FuncName on the stack, leading FuncName to be called on
Add-One-Device and Add-Two-Device.

A device stack responds to special statuses upon responses as follows:

`skip`: Skips the rest of the device stack for the current pass.

`pass`: Causes the stack to increment its pass number and re-execute
the stack from the first device, maintaining the state
accumulated so far. Only available in fold mode.

In all cases, the device stack will return the accumulated state to the
caller as the result of the call to the stack.

The dev_stack adds additional metadata to the message in order to track
the state of its execution as it progresses through devices. These keys
are as follows:

`Stack-Pass`: The number of times the stack has reset and re-executed
from the first device for the current message.

`Input-Prefix`: The prefix that the device should use for its outputs
and inputs.

`Output-Prefix`: The device that was previously executed.

All counters used by the stack are initialized to 1.

Additionally, as implemented in HyperBEAM, the device stack will honor a
number of options that are passed to it as keys in the message. Each of
these options is also passed through to the devices contained within the
stack during execution. These options include:

`Error-Strategy`: Determines how the stack handles errors from devices.
See `maybe_error/5` for more information.

`Allow-Multipass`: Determines whether the stack is allowed to automatically
re-execute from the first device when the `pass` tag is returned. See
`maybe_pass/3` for more information.

Under-the-hood, dev_stack uses a `default` handler to resolve all calls to
devices, aside `set/2` which it calls itself to mutate the message's `device`
key in order to change which device is currently being executed. This method
allows dev_stack to ensure that the message's HashPath is always correct,
even as it delegates calls to other devices. An example flow for a `dev_stack`
execution is as follows:

```

   	/Msg1/AlicesExcitingKey ->
   		dev_stack:execute ->
   			/Msg1/Set?device=/Device-Stack/1 ->
   			/Msg2/AlicesExcitingKey ->
   			/Msg3/Set?device=/Device-Stack/2 ->
   			/Msg4/AlicesExcitingKey
   			... ->
   			/MsgN/Set?device=[This-Device] ->
   		returns {ok, /MsgN+1} ->
   	/MsgN+1
```

In this example, the `device` key is mutated a number of times, but the
resulting HashPath remains correct and verifiable.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#benchmark_test-0">benchmark_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#example_device_for_stack_test-0">example_device_for_stack_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#generate_append_device-1">generate_append_device/1</a></td><td></td></tr><tr><td valign="top"><a href="#generate_append_device-2">generate_append_device/2*</a></td><td></td></tr><tr><td valign="top"><a href="#increment_pass-2">increment_pass/2*</a></td><td>Helper to increment the pass number.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td></td></tr><tr><td valign="top"><a href="#input_and_output_prefixes_test-0">input_and_output_prefixes_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#input_output_prefixes_passthrough_test-0">input_output_prefixes_passthrough_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#input_prefix-3">input_prefix/3</a></td><td>Return the input prefix for the stack.</td></tr><tr><td valign="top"><a href="#many_devices_test-0">many_devices_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_error-5">maybe_error/5*</a></td><td></td></tr><tr><td valign="top"><a href="#no_prefix_test-0">no_prefix_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#not_found_test-0">not_found_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#output_prefix-3">output_prefix/3</a></td><td>Return the output prefix for the stack.</td></tr><tr><td valign="top"><a href="#output_prefix_test-0">output_prefix_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#pass_test-0">pass_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#prefix-3">prefix/3</a></td><td>Return the default prefix for the stack.</td></tr><tr><td valign="top"><a href="#reinvocation_test-0">reinvocation_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_fold-3">resolve_fold/3*</a></td><td>The main device stack execution engine.</td></tr><tr><td valign="top"><a href="#resolve_fold-4">resolve_fold/4*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_map-3">resolve_map/3*</a></td><td>Map over the devices in the stack, accumulating the output in a single
message of keys and values, where keys are the same as the keys in the
original message (typically a number).</td></tr><tr><td valign="top"><a href="#router-3">router/3*</a></td><td></td></tr><tr><td valign="top"><a href="#router-4">router/4</a></td><td>The device stack key router.</td></tr><tr><td valign="top"><a href="#simple_map_test-0">simple_map_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#simple_stack_execute_test-0">simple_stack_execute_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#skip_test-0">skip_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_prefix_msg-0">test_prefix_msg/0*</a></td><td></td></tr><tr><td valign="top"><a href="#transform-3">transform/3*</a></td><td>Return Message1, transformed such that the device named <code>Key</code> from the
<code>Device-Stack</code> key in the message takes the place of the original <code>Device</code>
key.</td></tr><tr><td valign="top"><a href="#transform_external_call_device_test-0">transform_external_call_device_test/0*</a></td><td>Ensure we can generate a transformer message that can be called to
return a version of msg1 with only that device attached.</td></tr><tr><td valign="top"><a href="#transform_internal_call_device_test-0">transform_internal_call_device_test/0*</a></td><td>Test that the transform function can be called correctly internally
by other functions in the module.</td></tr><tr><td valign="top"><a href="#transformer_message-2">transformer_message/2*</a></td><td>Return a message which, when given a key, will transform the message
such that the device named <code>Key</code> from the <code>Device-Stack</code> key in the message
takes the place of the original <code>Device</code> key.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="benchmark_test-0"></a>

### benchmark_test/0 * ###

`benchmark_test() -> any()`

<a name="example_device_for_stack_test-0"></a>

### example_device_for_stack_test/0 * ###

`example_device_for_stack_test() -> any()`

<a name="generate_append_device-1"></a>

### generate_append_device/1 ###

`generate_append_device(Separator) -> any()`

<a name="generate_append_device-2"></a>

### generate_append_device/2 * ###

`generate_append_device(Separator, Status) -> any()`

<a name="increment_pass-2"></a>

### increment_pass/2 * ###

`increment_pass(Message, Opts) -> any()`

Helper to increment the pass number.

<a name="info-1"></a>

### info/1 ###

`info(Msg) -> any()`

<a name="input_and_output_prefixes_test-0"></a>

### input_and_output_prefixes_test/0 * ###

`input_and_output_prefixes_test() -> any()`

<a name="input_output_prefixes_passthrough_test-0"></a>

### input_output_prefixes_passthrough_test/0 * ###

`input_output_prefixes_passthrough_test() -> any()`

<a name="input_prefix-3"></a>

### input_prefix/3 ###

`input_prefix(Msg1, Msg2, Opts) -> any()`

Return the input prefix for the stack.

<a name="many_devices_test-0"></a>

### many_devices_test/0 * ###

`many_devices_test() -> any()`

<a name="maybe_error-5"></a>

### maybe_error/5 * ###

`maybe_error(Message1, Message2, DevNum, Info, Opts) -> any()`

<a name="no_prefix_test-0"></a>

### no_prefix_test/0 * ###

`no_prefix_test() -> any()`

<a name="not_found_test-0"></a>

### not_found_test/0 * ###

`not_found_test() -> any()`

<a name="output_prefix-3"></a>

### output_prefix/3 ###

`output_prefix(Msg1, Msg2, Opts) -> any()`

Return the output prefix for the stack.

<a name="output_prefix_test-0"></a>

### output_prefix_test/0 * ###

`output_prefix_test() -> any()`

<a name="pass_test-0"></a>

### pass_test/0 * ###

`pass_test() -> any()`

<a name="prefix-3"></a>

### prefix/3 ###

`prefix(Msg1, Msg2, Opts) -> any()`

Return the default prefix for the stack.

<a name="reinvocation_test-0"></a>

### reinvocation_test/0 * ###

`reinvocation_test() -> any()`

<a name="resolve_fold-3"></a>

### resolve_fold/3 * ###

`resolve_fold(Message1, Message2, Opts) -> any()`

The main device stack execution engine. See the moduledoc for more
information.

<a name="resolve_fold-4"></a>

### resolve_fold/4 * ###

`resolve_fold(Message1, Message2, DevNum, Opts) -> any()`

<a name="resolve_map-3"></a>

### resolve_map/3 * ###

`resolve_map(Message1, Message2, Opts) -> any()`

Map over the devices in the stack, accumulating the output in a single
message of keys and values, where keys are the same as the keys in the
original message (typically a number).

<a name="router-3"></a>

### router/3 * ###

`router(Message1, Message2, Opts) -> any()`

<a name="router-4"></a>

### router/4 ###

`router(Key, Message1, Message2, Opts) -> any()`

The device stack key router. Sends the request to `resolve_stack`,
except for `set/2` which is handled by the default implementation in
`dev_message`.

<a name="simple_map_test-0"></a>

### simple_map_test/0 * ###

`simple_map_test() -> any()`

<a name="simple_stack_execute_test-0"></a>

### simple_stack_execute_test/0 * ###

`simple_stack_execute_test() -> any()`

<a name="skip_test-0"></a>

### skip_test/0 * ###

`skip_test() -> any()`

<a name="test_prefix_msg-0"></a>

### test_prefix_msg/0 * ###

`test_prefix_msg() -> any()`

<a name="transform-3"></a>

### transform/3 * ###

`transform(Msg1, Key, Opts) -> any()`

Return Message1, transformed such that the device named `Key` from the
`Device-Stack` key in the message takes the place of the original `Device`
key. This transformation allows dev_stack to correctly track the HashPath
of the message as it delegates execution to devices contained within it.

<a name="transform_external_call_device_test-0"></a>

### transform_external_call_device_test/0 * ###

`transform_external_call_device_test() -> any()`

Ensure we can generate a transformer message that can be called to
return a version of msg1 with only that device attached.

<a name="transform_internal_call_device_test-0"></a>

### transform_internal_call_device_test/0 * ###

`transform_internal_call_device_test() -> any()`

Test that the transform function can be called correctly internally
by other functions in the module.

<a name="transformer_message-2"></a>

### transformer_message/2 * ###

`transformer_message(Msg1, Opts) -> any()`

Return a message which, when given a key, will transform the message
such that the device named `Key` from the `Device-Stack` key in the message
takes the place of the original `Device` key. This allows users to call
a single device from the stack:

/Msg1/Transform/DeviceName/keyInDevice ->
keyInDevice executed on DeviceName against Msg1.

