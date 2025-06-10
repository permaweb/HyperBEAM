# [Module hb_features.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_features.erl)




A module that exports a list of feature flags that the node supports
using the `-ifdef` macro.

<a name="description"></a>

## Description ##
As a consequence, this module acts as a proxy of information between the
build system and the runtime execution environment.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td>Returns a list of all feature flags that the node supports.</td></tr><tr><td valign="top"><a href="#enabled-1">enabled/1</a></td><td>Returns true if the feature flag is enabled.</td></tr><tr><td valign="top"><a href="#genesis_wasm-0">genesis_wasm/0</a></td><td></td></tr><tr><td valign="top"><a href="#http3-0">http3/0</a></td><td></td></tr><tr><td valign="top"><a href="#rocksdb-0">rocksdb/0</a></td><td></td></tr><tr><td valign="top"><a href="#test-0">test/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-0"></a>

### all/0 ###

`all() -> any()`

Returns a list of all feature flags that the node supports.

<a name="enabled-1"></a>

### enabled/1 ###

`enabled(Feature) -> any()`

Returns true if the feature flag is enabled.

<a name="genesis_wasm-0"></a>

### genesis_wasm/0 ###

`genesis_wasm() -> any()`

<a name="http3-0"></a>

### http3/0 ###

`http3() -> any()`

<a name="rocksdb-0"></a>

### rocksdb/0 ###

`rocksdb() -> any()`

<a name="test-0"></a>

### test/0 ###

`test() -> any()`

