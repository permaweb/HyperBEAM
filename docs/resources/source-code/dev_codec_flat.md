# [Module dev_codec_flat.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_flat.erl)




A codec for turning TABMs into/from flat Erlang maps that have
(potentially multi-layer) paths as their keys, and a normal TABM binary as
their value.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#binary_passthrough_test-0">binary_passthrough_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#commit-3">commit/3</a></td><td></td></tr><tr><td valign="top"><a href="#committed-3">committed/3</a></td><td></td></tr><tr><td valign="top"><a href="#deep_nesting_test-0">deep_nesting_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#deserialize-1">deserialize/1</a></td><td></td></tr><tr><td valign="top"><a href="#empty_map_test-0">empty_map_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td>Convert a flat map to a TABM.</td></tr><tr><td valign="top"><a href="#inject_at_path-3">inject_at_path/3*</a></td><td></td></tr><tr><td valign="top"><a href="#multiple_paths_test-0">multiple_paths_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#nested_conversion_test-0">nested_conversion_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#path_list_test-0">path_list_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#serialize-1">serialize/1</a></td><td></td></tr><tr><td valign="top"><a href="#simple_conversion_test-0">simple_conversion_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td>Convert a TABM to a flat map.</td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="binary_passthrough_test-0"></a>

### binary_passthrough_test/0 * ###

`binary_passthrough_test() -> any()`

<a name="commit-3"></a>

### commit/3 ###

`commit(Msg, Req, Opts) -> any()`

<a name="committed-3"></a>

### committed/3 ###

`committed(Msg, Req, Opts) -> any()`

<a name="deep_nesting_test-0"></a>

### deep_nesting_test/0 * ###

`deep_nesting_test() -> any()`

<a name="deserialize-1"></a>

### deserialize/1 ###

`deserialize(Bin) -> any()`

<a name="empty_map_test-0"></a>

### empty_map_test/0 * ###

`empty_map_test() -> any()`

<a name="from-1"></a>

### from/1 ###

`from(Bin) -> any()`

Convert a flat map to a TABM.

<a name="inject_at_path-3"></a>

### inject_at_path/3 * ###

`inject_at_path(Rest, Value, Map) -> any()`

<a name="multiple_paths_test-0"></a>

### multiple_paths_test/0 * ###

`multiple_paths_test() -> any()`

<a name="nested_conversion_test-0"></a>

### nested_conversion_test/0 * ###

`nested_conversion_test() -> any()`

<a name="path_list_test-0"></a>

### path_list_test/0 * ###

`path_list_test() -> any()`

<a name="serialize-1"></a>

### serialize/1 ###

`serialize(Map) -> any()`

<a name="simple_conversion_test-0"></a>

### simple_conversion_test/0 * ###

`simple_conversion_test() -> any()`

<a name="to-1"></a>

### to/1 ###

`to(Bin) -> any()`

Convert a TABM to a flat map.

<a name="verify-3"></a>

### verify/3 ###

`verify(Msg, Req, Opts) -> any()`

