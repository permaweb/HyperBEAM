# [Module dev_lookup.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_lookup.erl)




A device that looks up an ID from a local store and returns it, honoring
the `accept` key to return the correct format.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#aos2_message_lookup_test-0">aos2_message_lookup_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#binary_lookup_test-0">binary_lookup_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_lookup_test-0">http_lookup_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#message_lookup_test-0">message_lookup_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#read-3">read/3</a></td><td>Fetch a resource from the cache using "target" ID extracted from the message.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="aos2_message_lookup_test-0"></a>

### aos2_message_lookup_test/0 * ###

`aos2_message_lookup_test() -> any()`

<a name="binary_lookup_test-0"></a>

### binary_lookup_test/0 * ###

`binary_lookup_test() -> any()`

<a name="http_lookup_test-0"></a>

### http_lookup_test/0 * ###

`http_lookup_test() -> any()`

<a name="message_lookup_test-0"></a>

### message_lookup_test/0 * ###

`message_lookup_test() -> any()`

<a name="read-3"></a>

### read/3 ###

`read(M1, M2, Opts) -> any()`

Fetch a resource from the cache using "target" ID extracted from the message

