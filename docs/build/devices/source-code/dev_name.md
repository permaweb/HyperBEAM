# [Module dev_name.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_name.erl)




A device for resolving names to their corresponding values, through the
use of a `resolver` interface.

<a name="description"></a>

## Description ##
Each `resolver` is a message that can be
given a `key` and returns an associated value. The device will attempt to
match the key against each resolver in turn, and return the value of the
first resolver that matches.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute_resolver-3">execute_resolver/3*</a></td><td>Execute a resolver with the given key and return its value.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Configure the <code>default</code> key to proxy to the <code>resolver/4</code> function.</td></tr><tr><td valign="top"><a href="#load_and_execute_test-0">load_and_execute_test/0*</a></td><td>Test that we can resolve messages from a name loaded with the device.</td></tr><tr><td valign="top"><a href="#match_resolver-3">match_resolver/3*</a></td><td>Find the first resolver that matches the key and return its value.</td></tr><tr><td valign="top"><a href="#message_lookup_device_resolver-1">message_lookup_device_resolver/1*</a></td><td></td></tr><tr><td valign="top"><a href="#multiple_resolvers_test-0">multiple_resolvers_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#no_resolvers_test-0">no_resolvers_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve-4">resolve/4*</a></td><td>Resolve a name to its corresponding value.</td></tr><tr><td valign="top"><a href="#single_resolver_test-0">single_resolver_test/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="execute_resolver-3"></a>

### execute_resolver/3 * ###

`execute_resolver(Key, Resolver, Opts) -> any()`

Execute a resolver with the given key and return its value.

<a name="info-1"></a>

### info/1 ###

`info(X1) -> any()`

Configure the `default` key to proxy to the `resolver/4` function.
Exclude the `keys` and `set` keys from being processed by this device, as
these are needed to modify the base message itself.

<a name="load_and_execute_test-0"></a>

### load_and_execute_test/0 * ###

`load_and_execute_test() -> any()`

Test that we can resolve messages from a name loaded with the device.

<a name="match_resolver-3"></a>

### match_resolver/3 * ###

`match_resolver(Key, Resolvers, Opts) -> any()`

Find the first resolver that matches the key and return its value.

<a name="message_lookup_device_resolver-1"></a>

### message_lookup_device_resolver/1 * ###

`message_lookup_device_resolver(Msg) -> any()`

<a name="multiple_resolvers_test-0"></a>

### multiple_resolvers_test/0 * ###

`multiple_resolvers_test() -> any()`

<a name="no_resolvers_test-0"></a>

### no_resolvers_test/0 * ###

`no_resolvers_test() -> any()`

<a name="resolve-4"></a>

### resolve/4 * ###

`resolve(Key, X2, Req, Opts) -> any()`

Resolve a name to its corresponding value. The name is given by the key
called. For example, `GET /~name@1.0/hello&load=false` grants the value of
`hello`. If the `load` key is set to `true`, the value is treated as a
pointer and its contents is loaded from the cache. For example,
`GET /~name@1.0/reference` yields the message at the path specified by the
`reference` key.

<a name="single_resolver_test-0"></a>

### single_resolver_test/0 * ###

`single_resolver_test() -> any()`

