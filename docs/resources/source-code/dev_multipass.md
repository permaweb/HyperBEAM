# [Module dev_multipass.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_multipass.erl)




A device that triggers repass events until a certain counter has been
reached.

<a name="description"></a>

## Description ##
This is useful for certain types of stacks that need various
execution passes to be completed in sequence across devices.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#basic_multipass_test-0">basic_multipass_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#handle-4">handle/4*</a></td><td>Forward the keys function to the message device, handle all others
with deduplication.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="basic_multipass_test-0"></a>

### basic_multipass_test/0 * ###

`basic_multipass_test() -> any()`

<a name="handle-4"></a>

### handle/4 * ###

`handle(Key, M1, M2, Opts) -> any()`

Forward the keys function to the message device, handle all others
with deduplication. We only act on the first pass.

<a name="info-1"></a>

### info/1 ###

`info(M1) -> any()`

