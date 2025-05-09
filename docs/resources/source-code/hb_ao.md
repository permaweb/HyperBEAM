# [Module hb_ao.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_ao.erl)




This module is the root of the device call logic of the
AO-Core protocol in HyperBEAM.

<a name="description"></a>

## Description ##

At the implementation level, every message is simply a collection of keys,
dictated by its `Device`, that can be resolved in order to yield their
values. Each key may return another message or a raw value:

`ao(Message1, Message2) -> {Status, Message3}`

Under-the-hood, `AO-Core(Message1, Message2)` leads to the evaluation of
`DeviceMod:PathPart(Message1, Message2)`, which defines the user compute
to be performed. If `Message1` does not specify a device, `dev_message` is
assumed. The key to resolve is specified by the `Path` field of the message.

After each output, the `HashPath` is updated to include the `Message2`
that was executed upon it.

Because each message implies a device that can resolve its keys, as well
as generating a merkle tree of the computation that led to the result,
you can see AO-Core protocol as a system for cryptographically chaining
the execution of `combinators`. See `docs/ao-core-protocol.md` for more
information about AO-Core.

The `Fun(Message1, Message2)` pattern is repeated throughout the HyperBEAM
codebase, sometimes with `MessageX` replaced with `MX` or `MsgX` for brevity.

Message3 can be either a new message or a raw output value (a binary, integer,
float, atom, or list of such values).

Devices can be expressed as either modules or maps. They can also be
referenced by an Arweave ID, which can be used to load a device from
the network (depending on the value of the `load_remote_devices` and
`trusted_device_signers` environment settings).

HyperBEAM device implementations are defined as follows:

```

       DevMod:ExportedFunc : Key resolution functions. All are assumed to be
                             device keys (thus, present in every message that
                             uses it) unless specified by <code>DevMod:info()</code>.
                             Each function takes a set of parameters
                             of the form <code>DevMod:KeyHandler(Msg1, Msg2, Opts)</code>.
                             Each of these arguments can be ommitted if not
                             needed. Non-exported functions are not assumed
                             to be device keys.
       DevMod:info : Optional. Returns a map of options for the device. All
                     options are optional and assumed to be the defaults if
                     not specified. This function can accept a <code>Message1</code> as
                     an argument, allowing it to specify its functionality
                     based on a specific message if appropriate.
       info/exports : Overrides the export list of the Erlang module, such that
                     only the functions in this list are assumed to be device
                     keys. Defaults to all of the functions that DevMod
                     exports in the Erlang environment.
       info/excludes : A list of keys that should not be resolved by the device,
                       despite being present in the Erlang module exports list.
       info/handler : A function that should be used to handle _all_ keys for
                      messages using the device.
       info/default : A function that should be used to handle all keys that
                      are not explicitly implemented by the device. Defaults to
                      the <code>dev_message</code> device, which contains general keys for
                      interacting with messages.
       info/default_mod : A different device module that should be used to
                      handle all keys that are not explicitly implemented
                      by the device. Defaults to the <code>dev_message</code> device.
       info/grouper : A function that returns the concurrency 'group' name for
                      an execution. Executions with the same group name will
                      be executed by sending a message to the associated process
                      and waiting for a response. This allows you to control
                      concurrency of execution and to allow executions to share
                      in-memory state as applicable. Default: A derivation of
                      Msg1+Msg2. This means that concurrent calls for the same
                      output will lead to only a single execution.
       info/worker : A function that should be run as the 'server' loop of
                     the executor for interactions using the device.
   The HyperBEAM resolver also takes a number of runtime options that change
   the way that the environment operates:<code>update_hashpath</code>:  Whether to add the <code>Msg2</code> to <code>HashPath</code> for the <code>Msg3</code>.
   					Default: true.<code>add_key</code>:          Whether to add the key to the start of the arguments.
   					Default: <code><not set></code>.
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#deep_set-4">deep_set/4</a></td><td>Recursively search a map, resolving keys, and set the value of the key
at the given path.</td></tr><tr><td valign="top"><a href="#default_module-0">default_module/0*</a></td><td>The default device is the identity device, which simply returns the
value associated with any key as it exists in its Erlang map.</td></tr><tr><td valign="top"><a href="#device_set-4">device_set/4*</a></td><td>Call the device's <code>set</code> function.</td></tr><tr><td valign="top"><a href="#device_set-5">device_set/5*</a></td><td></td></tr><tr><td valign="top"><a href="#do_resolve_many-2">do_resolve_many/2*</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_loaded-2">ensure_loaded/2*</a></td><td>Ensure that the message is loaded from the cache if it is an ID.</td></tr><tr><td valign="top"><a href="#error_execution-5">error_execution/5*</a></td><td>Handle an error in a device call.</td></tr><tr><td valign="top"><a href="#error_infinite-3">error_infinite/3*</a></td><td>Catch all return if we are in an infinite loop.</td></tr><tr><td valign="top"><a href="#error_invalid_intermediate_status-5">error_invalid_intermediate_status/5*</a></td><td></td></tr><tr><td valign="top"><a href="#error_invalid_message-3">error_invalid_message/3*</a></td><td>Catch all return if the message is invalid.</td></tr><tr><td valign="top"><a href="#find_exported_function-5">find_exported_function/5</a></td><td>Find the function with the highest arity that has the given name, if it
exists.</td></tr><tr><td valign="top"><a href="#force_message-2">force_message/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Shortcut for resolving a key in a message without its status if it is
<code>ok</code>.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#get-4">get/4</a></td><td></td></tr><tr><td valign="top"><a href="#get_first-2">get_first/2</a></td><td>take a sequence of base messages and paths, then return the value of the
first message that can be resolved using a path.</td></tr><tr><td valign="top"><a href="#get_first-3">get_first/3</a></td><td></td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>Get the info map for a device, optionally giving it a message if the
device's info function is parameterized by one.</td></tr><tr><td valign="top"><a href="#info-3">info/3*</a></td><td></td></tr><tr><td valign="top"><a href="#info_handler_to_fun-4">info_handler_to_fun/4*</a></td><td>Parse a handler key given by a device's <code>info</code>.</td></tr><tr><td valign="top"><a href="#internal_opts-1">internal_opts/1*</a></td><td>The execution options that are used internally by this module
when calling itself.</td></tr><tr><td valign="top"><a href="#is_exported-2">is_exported/2*</a></td><td></td></tr><tr><td valign="top"><a href="#is_exported-4">is_exported/4</a></td><td>Check if a device is guarding a key via its <code>exports</code> list.</td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>Shortcut to get the list of keys from a message.</td></tr><tr><td valign="top"><a href="#keys-2">keys/2</a></td><td></td></tr><tr><td valign="top"><a href="#keys-3">keys/3</a></td><td></td></tr><tr><td valign="top"><a href="#load_device-2">load_device/2</a></td><td>Load a device module from its name or a message ID.</td></tr><tr><td valign="top"><a href="#maybe_force_message-2">maybe_force_message/2*</a></td><td>Force the result of a device call into a message if the result is not
requested by the <code>Opts</code>.</td></tr><tr><td valign="top"><a href="#message_to_device-2">message_to_device/2</a></td><td>Extract the device module from a message.</td></tr><tr><td valign="top"><a href="#message_to_fun-3">message_to_fun/3</a></td><td>Calculate the Erlang function that should be called to get a value for
a given key from a device.</td></tr><tr><td valign="top"><a href="#normalize_key-1">normalize_key/1</a></td><td>Convert a key to a binary in normalized form.</td></tr><tr><td valign="top"><a href="#normalize_key-2">normalize_key/2</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_keys-1">normalize_keys/1</a></td><td>Ensure that a message is processable by the AO-Core resolver: No lists.</td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Remove a key from a message, using its underlying device.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td></td></tr><tr><td valign="top"><a href="#resolve-2">resolve/2</a></td><td>Get the value of a message's key by running its associated device
function.</td></tr><tr><td valign="top"><a href="#resolve-3">resolve/3</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_many-2">resolve_many/2</a></td><td>Resolve a list of messages in sequence.</td></tr><tr><td valign="top"><a href="#resolve_stage-4">resolve_stage/4*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_stage-5">resolve_stage/5*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_stage-6">resolve_stage/6*</a></td><td></td></tr><tr><td valign="top"><a href="#set-2">set/2</a></td><td>Shortcut for setting a key in the message using its underlying device.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td></td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td></td></tr><tr><td valign="top"><a href="#subresolve-4">subresolve/4*</a></td><td>Execute a sub-resolution.</td></tr><tr><td valign="top"><a href="#truncate_args-2">truncate_args/2</a></td><td>Truncate the arguments of a function to the number of arguments it
actually takes.</td></tr><tr><td valign="top"><a href="#verify_device_compatibility-2">verify_device_compatibility/2*</a></td><td>Verify that a device is compatible with the current machine.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="deep_set-4"></a>

### deep_set/4 ###

`deep_set(Msg, Rest, Value, Opts) -> any()`

Recursively search a map, resolving keys, and set the value of the key
at the given path. This function has special cases for handling `set` calls
where the path is an empty list (`/`). In this case, if the value is an
immediate, non-complex term, we can set it directly. Otherwise, we use the
device's `set` function to set the value.

<a name="default_module-0"></a>

### default_module/0 * ###

`default_module() -> any()`

The default device is the identity device, which simply returns the
value associated with any key as it exists in its Erlang map. It should also
implement the `set` key, which returns a `Message3` with the values changed
according to the `Message2` passed to it.

<a name="device_set-4"></a>

### device_set/4 * ###

`device_set(Msg, Key, Value, Opts) -> any()`

Call the device's `set` function.

<a name="device_set-5"></a>

### device_set/5 * ###

`device_set(Msg, Key, Value, Mode, Opts) -> any()`

<a name="do_resolve_many-2"></a>

### do_resolve_many/2 * ###

`do_resolve_many(MsgList, Opts) -> any()`

<a name="ensure_loaded-2"></a>

### ensure_loaded/2 * ###

`ensure_loaded(MsgID, Opts) -> any()`

Ensure that the message is loaded from the cache if it is an ID. If is
not loadable or already present, we raise an error.

<a name="error_execution-5"></a>

### error_execution/5 * ###

`error_execution(ExecGroup, Msg2, Whence, X4, Opts) -> any()`

Handle an error in a device call.

<a name="error_infinite-3"></a>

### error_infinite/3 * ###

`error_infinite(Msg1, Msg2, Opts) -> any()`

Catch all return if we are in an infinite loop.

<a name="error_invalid_intermediate_status-5"></a>

### error_invalid_intermediate_status/5 * ###

`error_invalid_intermediate_status(Msg1, Msg2, Msg3, RemainingPath, Opts) -> any()`

<a name="error_invalid_message-3"></a>

### error_invalid_message/3 * ###

`error_invalid_message(Msg1, Msg2, Opts) -> any()`

Catch all return if the message is invalid.

<a name="find_exported_function-5"></a>

### find_exported_function/5 ###

`find_exported_function(Msg, Dev, Key, MaxArity, Opts) -> any()`

Find the function with the highest arity that has the given name, if it
exists.

If the device is a module, we look for a function with the given name.

If the device is a map, we look for a key in the map. First we try to find
the key using its literal value. If that fails, we cast the key to an atom
and try again.

<a name="force_message-2"></a>

### force_message/2 ###

`force_message(X1, Opts) -> any()`

<a name="get-2"></a>

### get/2 ###

`get(Path, Msg) -> any()`

Shortcut for resolving a key in a message without its status if it is
`ok`. This makes it easier to write complex logic on top of messages while
maintaining a functional style.

Additionally, this function supports the `{as, Device, Msg}` syntax, which
allows the key to be resolved using another device to resolve the key,
while maintaining the tracability of the `HashPath` of the output message.

Returns the value of the key if it is found, otherwise returns the default
provided by the user, or `not_found` if no default is provided.

<a name="get-3"></a>

### get/3 ###

`get(Path, Msg, Opts) -> any()`

<a name="get-4"></a>

### get/4 ###

`get(Path, Msg, Default, Opts) -> any()`

<a name="get_first-2"></a>

### get_first/2 ###

`get_first(Paths, Opts) -> any()`

take a sequence of base messages and paths, then return the value of the
first message that can be resolved using a path.

<a name="get_first-3"></a>

### get_first/3 ###

`get_first(Msgs, Default, Opts) -> any()`

<a name="info-2"></a>

### info/2 ###

`info(Msg, Opts) -> any()`

Get the info map for a device, optionally giving it a message if the
device's info function is parameterized by one.

<a name="info-3"></a>

### info/3 * ###

`info(DevMod, Msg, Opts) -> any()`

<a name="info_handler_to_fun-4"></a>

### info_handler_to_fun/4 * ###

`info_handler_to_fun(Handler, Msg, Key, Opts) -> any()`

Parse a handler key given by a device's `info`.

<a name="internal_opts-1"></a>

### internal_opts/1 * ###

`internal_opts(Opts) -> any()`

The execution options that are used internally by this module
when calling itself.

<a name="is_exported-2"></a>

### is_exported/2 * ###

`is_exported(Info, Key) -> any()`

<a name="is_exported-4"></a>

### is_exported/4 ###

`is_exported(Msg, Dev, Key, Opts) -> any()`

Check if a device is guarding a key via its `exports` list. Defaults to
true if the device does not specify an `exports` list. The `info` function is
always exported, if it exists. Elements of the `exludes` list are not
exported. Note that we check for info _twice_ -- once when the device is
given but the info result is not, and once when the info result is given.
The reason for this is that `info/3` calls other functions that may need to
check if a key is exported, so we must avoid infinite loops. We must, however,
also return a consistent result in the case that only the info result is
given, so we check for it in both cases.

<a name="keys-1"></a>

### keys/1 ###

`keys(Msg) -> any()`

Shortcut to get the list of keys from a message.

<a name="keys-2"></a>

### keys/2 ###

`keys(Msg, Opts) -> any()`

<a name="keys-3"></a>

### keys/3 ###

`keys(Msg, Opts, X3) -> any()`

<a name="load_device-2"></a>

### load_device/2 ###

`load_device(Map, Opts) -> any()`

Load a device module from its name or a message ID.
Returns {ok, Executable} where Executable is the device module. On error,
a tuple of the form {error, Reason} is returned.

<a name="maybe_force_message-2"></a>

### maybe_force_message/2 * ###

`maybe_force_message(X1, Opts) -> any()`

Force the result of a device call into a message if the result is not
requested by the `Opts`. If the result is a literal, we wrap it in a message
and signal the location of the result inside. We also similarly handle ao-result
when the result is a single value and an explicit status code.

<a name="message_to_device-2"></a>

### message_to_device/2 ###

`message_to_device(Msg, Opts) -> any()`

Extract the device module from a message.

<a name="message_to_fun-3"></a>

### message_to_fun/3 ###

`message_to_fun(Msg, Key, Opts) -> any()`

Calculate the Erlang function that should be called to get a value for
a given key from a device.

This comes in 7 forms:
1. The message does not specify a device, so we use the default device.
2. The device has a `handler` key in its `Dev:info()` map, which is a
function that takes a key and returns a function to handle that key. We pass
the key as an additional argument to this function.
3. The device has a function of the name `Key`, which should be called
directly.
4. The device does not implement the key, but does have a default handler
for us to call. We pass it the key as an additional argument.
5. The device does not implement the key, and has no default handler. We use
the default device to handle the key.
Error: If the device is specified, but not loadable, we raise an error.

Returns {ok | add_key, Fun} where Fun is the function to call, and add_key
indicates that the key should be added to the start of the call's arguments.

<a name="normalize_key-1"></a>

### normalize_key/1 ###

`normalize_key(Key) -> any()`

Convert a key to a binary in normalized form.

<a name="normalize_key-2"></a>

### normalize_key/2 ###

`normalize_key(Key, Opts) -> any()`

<a name="normalize_keys-1"></a>

### normalize_keys/1 ###

`normalize_keys(Msg1) -> any()`

Ensure that a message is processable by the AO-Core resolver: No lists.

<a name="remove-2"></a>

### remove/2 ###

`remove(Msg, Key) -> any()`

Remove a key from a message, using its underlying device.

<a name="remove-3"></a>

### remove/3 ###

`remove(Msg, Key, Opts) -> any()`

<a name="resolve-2"></a>

### resolve/2 ###

`resolve(SingletonMsg, Opts) -> any()`

Get the value of a message's key by running its associated device
function. Optionally, takes options that control the runtime environment.
This function returns the raw result of the device function call:
`{ok | error, NewMessage}.`
The resolver is composed of a series of discrete phases:
1: Normalization.
2: Cache lookup.
3: Validation check.
4: Persistent-resolver lookup.
5: Device lookup.
6: Execution.
7: Execution of the `step` hook.
8: Subresolution.
9: Cryptographic linking.
10: Result caching.
11: Notify waiters.
12: Fork worker.
13: Recurse or terminate.

<a name="resolve-3"></a>

### resolve/3 ###

`resolve(Msg1, Path, Opts) -> any()`

<a name="resolve_many-2"></a>

### resolve_many/2 ###

`resolve_many(ListMsg, Opts) -> any()`

Resolve a list of messages in sequence. Take the output of the first
message as the input for the next message. Once the last message is resolved,
return the result.
A `resolve_many` call with only a single ID will attempt to read the message
directly from the store. No execution is performed.

<a name="resolve_stage-4"></a>

### resolve_stage/4 * ###

`resolve_stage(X1, Raw, Msg2, Opts) -> any()`

<a name="resolve_stage-5"></a>

### resolve_stage/5 * ###

`resolve_stage(X1, Msg1, Msg2, ExecName, Opts) -> any()`

<a name="resolve_stage-6"></a>

### resolve_stage/6 * ###

`resolve_stage(X1, Func, Msg1, Msg2, ExecName, Opts) -> any()`

<a name="set-2"></a>

### set/2 ###

`set(Msg1, Msg2) -> any()`

Shortcut for setting a key in the message using its underlying device.
Like the `get/3` function, this function honors the `error_strategy` option.
`set` works with maps and recursive paths while maintaining the appropriate
`HashPath` for each step.

<a name="set-3"></a>

### set/3 ###

`set(RawMsg1, RawMsg2, Opts) -> any()`

<a name="set-4"></a>

### set/4 ###

`set(Msg1, Key, Value, Opts) -> any()`

<a name="subresolve-4"></a>

### subresolve/4 * ###

`subresolve(RawMsg1, DevID, ReqPath, Opts) -> any()`

Execute a sub-resolution.

<a name="truncate_args-2"></a>

### truncate_args/2 ###

`truncate_args(Fun, Args) -> any()`

Truncate the arguments of a function to the number of arguments it
actually takes.

<a name="verify_device_compatibility-2"></a>

### verify_device_compatibility/2 * ###

`verify_device_compatibility(Msg, Opts) -> any()`

Verify that a device is compatible with the current machine.

