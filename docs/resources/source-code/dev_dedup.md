# [Module dev_dedup.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_dedup.erl)




A device that deduplicates messages send to a process.

<a name="description"></a>

## Description ##
Only runs on the first pass of the `compute` key call if executed
in a stack. Currently the device stores its list of already seen
items in memory, but at some point it will likely make sense to
drop them in the cache.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dedup_test-0">dedup_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#dedup_with_multipass_test-0">dedup_with_multipass_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#handle-4">handle/4*</a></td><td>Forward the keys function to the message device, handle all others
with deduplication.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td></td></tr></table>


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

Forward the keys function to the message device, handle all others
with deduplication. We only act on the first pass.

<a name="info-1"></a>

### info/1 ###

`info(M1) -> any()`

