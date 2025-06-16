# [Module dev_dedup.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_dedup.erl)




A device that deduplicates messages in an evaluation stream, returning
status `skip` if the message has already been seen.

<a name="description"></a>

## Description ##

This device is typically used to ensure that a message is only executed
once, even if assigned multiple times, upon a `~process@1.0` evaluation.
It can, however, be used in many other contexts.

This device honors the `pass` key if it is present in the message. If so,
it will only run on the first pass. Additionally, the device supports
a `subject-key` key that allows the caller to specify the key whose ID
should be used for deduplication. If the `subject-key` key is not present,
the device will use the `body` of the request as the subject. If the key is
set to `request`, the device will use the entire request itself as the
subject.

This device runs on the first pass of the `compute` key call if executed
in a stack, and not in subsequent passes. Currently the device stores its
list of already seen items in memory, but at some point it will likely make
sense to drop them in the cache.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dedup_test-0">dedup_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#dedup_with_multipass_test-0">dedup_with_multipass_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#handle-4">handle/4*</a></td><td>Forward the keys and <code>set</code> functions to the message device, handle all
others with deduplication.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dedup_test-0"></a>

### dedup_test/0 * ###

`dedup_test() -> any()`

<a name="dedup_with_multipass_test-0"></a>

### dedup_with_multipass_test/0 * ###

`dedup_with_multipass_test() -> any()`

<a name="handle-4"></a>

### handle/4 * ###

`handle(Key, M1, M2, Opts) -> any()`

Forward the keys and `set` functions to the message device, handle all
others with deduplication. This allows the device to be used in any context
where a key is called. If the `dedup-key`

<a name="info-1"></a>

### info/1 ###

`info(M1) -> any()`

