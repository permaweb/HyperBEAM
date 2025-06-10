# [Module hb_private.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_private.erl)




This module provides basic helper utilities for managing the
private element of a message, which can be used to store state that is
not included in serialized messages, or those granted to users via the
APIs.

<a name="description"></a>

## Description ##

Private elements of a message can be useful for storing state that
is only relevant temporarily. For example, a device might use the private
element to store a cache of values that are expensive to recompute. They
should _not_ be used for encoding state that makes the execution of a
device non-deterministic (unless you are sure you know what you are doing).

The `set` and `get` functions of this module allow you to run those keys
as AO-Core paths if you would like to have private `devices` in the
messages non-public zone.

See `hb_ao` for more information about the AO-Core protocol
and private elements of messages.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_message-1">from_message/1</a></td><td>Return the <code>private</code> key from a message.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>Helper for getting a value from the private element of a message.</td></tr><tr><td valign="top"><a href="#get-4">get/4</a></td><td></td></tr><tr><td valign="top"><a href="#get_private_key_test-0">get_private_key_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#is_private-1">is_private/1</a></td><td>Check if a key is private.</td></tr><tr><td valign="top"><a href="#priv_ao_opts-1">priv_ao_opts/1*</a></td><td>The opts map that should be used when resolving paths against the
private element of a message.</td></tr><tr><td valign="top"><a href="#remove_private_specifier-1">remove_private_specifier/1*</a></td><td>Remove the first key from the path if it is a private specifier.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Unset all of the private keys in a message.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td></td></tr><tr><td valign="top"><a href="#set-4">set/4</a></td><td>Helper function for setting a key in the private element of a message.</td></tr><tr><td valign="top"><a href="#set_priv-2">set_priv/2</a></td><td>Helper function for setting the complete private element of a message.</td></tr><tr><td valign="top"><a href="#set_private_test-0">set_private_test/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_message-1"></a>

### from_message/1 ###

`from_message(Msg) -> any()`

Return the `private` key from a message. If the key does not exist, an
empty map is returned.

<a name="get-3"></a>

### get/3 ###

`get(Key, Msg, Opts) -> any()`

Helper for getting a value from the private element of a message. Uses
AO-Core resolve under-the-hood, removing the private specifier from the
path if it exists.

<a name="get-4"></a>

### get/4 ###

`get(InputPath, Msg, Default, Opts) -> any()`

<a name="get_private_key_test-0"></a>

### get_private_key_test/0 * ###

`get_private_key_test() -> any()`

<a name="is_private-1"></a>

### is_private/1 ###

`is_private(Key) -> any()`

Check if a key is private.

<a name="priv_ao_opts-1"></a>

### priv_ao_opts/1 * ###

`priv_ao_opts(Opts) -> any()`

The opts map that should be used when resolving paths against the
private element of a message.

<a name="remove_private_specifier-1"></a>

### remove_private_specifier/1 * ###

`remove_private_specifier(InputPath) -> any()`

Remove the first key from the path if it is a private specifier.

<a name="reset-1"></a>

### reset/1 ###

`reset(Msg) -> any()`

Unset all of the private keys in a message.

<a name="set-3"></a>

### set/3 ###

`set(Msg, PrivMap, Opts) -> any()`

<a name="set-4"></a>

### set/4 ###

`set(Msg, InputPath, Value, Opts) -> any()`

Helper function for setting a key in the private element of a message.

<a name="set_priv-2"></a>

### set_priv/2 ###

`set_priv(Msg, PrivMap) -> any()`

Helper function for setting the complete private element of a message.

<a name="set_private_test-0"></a>

### set_private_test/0 * ###

`set_private_test() -> any()`

