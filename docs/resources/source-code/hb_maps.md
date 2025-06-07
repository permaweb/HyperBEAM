# [Module hb_maps.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_maps.erl)




An abstraction for working with maps in HyperBEAM, matching the
generic `maps` module, but additionally supporting the resolution of
links as they are encountered.

<a name="description"></a>

## Description ##

These functions must be used extremely
carefully. In virtually all circumstances, the `hb_ao:resolve/3` or
`hb_ao:get/3` functions should be used instead, as they will execute the
full AO-Core protocol upon requests (normalizing keys, applying the
appropriate device's functions, as well as resolving links). By using this
module's functions, you are implicitly making the assumption that the message
in question is of the `~message@1.0` form, ignoring any other keys that its
actual device may present. This module is intended for the extremely rare
circumstances in which the additional overhead of the full AO-Core
execution cycle is not acceptable, and the data in question is known to
conform to the `~message@1.0` form.

If you do not understand any/all of the above, you are in the wrong place!
Utilise the `hb_ao` module and read the documentation therein, saving
yourself from the inevitable issues that will arise from using this
module without understanding the full implications. You have been warned.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#filter-2">filter/2</a></td><td></td></tr><tr><td valign="top"><a href="#filter-3">filter/3</a></td><td></td></tr><tr><td valign="top"><a href="#filter_passively_loads_test-0">filter_passively_loads_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#filter_with_link_test-0">filter_with_link_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#filtermap-2">filtermap/2</a></td><td></td></tr><tr><td valign="top"><a href="#filtermap-3">filtermap/3</a></td><td></td></tr><tr><td valign="top"><a href="#filtermap_passively_loads_test-0">filtermap_passively_loads_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#filtermap_with_link_test-0">filtermap_with_link_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td></td></tr><tr><td valign="top"><a href="#find-3">find/3</a></td><td></td></tr><tr><td valign="top"><a href="#fold-3">fold/3</a></td><td></td></tr><tr><td valign="top"><a href="#fold-4">fold/4</a></td><td></td></tr><tr><td valign="top"><a href="#fold_with_typed_link_test-0">fold_with_typed_link_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#get-4">get/4</a></td><td>Get a value from a map, resolving links as they are encountered in both
the TABM encoded link format, as well as the structured type.</td></tr><tr><td valign="top"><a href="#get_with_link_test-0">get_with_link_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#get_with_typed_link_test-0">get_with_typed_link_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#is_key-2">is_key/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_key-3">is_key/3</a></td><td></td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td></td></tr><tr><td valign="top"><a href="#keys-2">keys/2</a></td><td></td></tr><tr><td valign="top"><a href="#map-2">map/2</a></td><td></td></tr><tr><td valign="top"><a href="#map-3">map/3</a></td><td></td></tr><tr><td valign="top"><a href="#map_with_link_test-0">map_with_link_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#merge-2">merge/2</a></td><td></td></tr><tr><td valign="top"><a href="#merge-3">merge/3</a></td><td></td></tr><tr><td valign="top"><a href="#put-3">put/3</a></td><td></td></tr><tr><td valign="top"><a href="#put-4">put/4</a></td><td></td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td></td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_on_link_test-0">resolve_on_link_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td></td></tr><tr><td valign="top"><a href="#size-2">size/2</a></td><td></td></tr><tr><td valign="top"><a href="#take-2">take/2</a></td><td></td></tr><tr><td valign="top"><a href="#take-3">take/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-2">to_list/2</a></td><td></td></tr><tr><td valign="top"><a href="#update_with-3">update_with/3</a></td><td></td></tr><tr><td valign="top"><a href="#update_with-4">update_with/4</a></td><td></td></tr><tr><td valign="top"><a href="#values-1">values/1</a></td><td></td></tr><tr><td valign="top"><a href="#values-2">values/2</a></td><td></td></tr><tr><td valign="top"><a href="#with-2">with/2</a></td><td></td></tr><tr><td valign="top"><a href="#with-3">with/3</a></td><td></td></tr><tr><td valign="top"><a href="#without-2">without/2</a></td><td></td></tr><tr><td valign="top"><a href="#without-3">without/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="filter-2"></a>

### filter/2 ###

<pre><code>
filter(Fun::fun((Key::term(), Value::term()) -&gt; boolean()), Map::map()) -&gt; map()
</code></pre>
<br />

<a name="filter-3"></a>

### filter/3 ###

<pre><code>
filter(Fun::fun((Key::term(), Value::term()) -&gt; boolean()), Map::map(), Opts::map()) -&gt; map()
</code></pre>
<br />

<a name="filter_passively_loads_test-0"></a>

### filter_passively_loads_test/0 * ###

`filter_passively_loads_test() -> any()`

<a name="filter_with_link_test-0"></a>

### filter_with_link_test/0 * ###

`filter_with_link_test() -> any()`

<a name="filtermap-2"></a>

### filtermap/2 ###

<pre><code>
filtermap(Fun::fun((Key::term(), Value::term()) -&gt; {boolean(), term()}), Map::map()) -&gt; map()
</code></pre>
<br />

<a name="filtermap-3"></a>

### filtermap/3 ###

<pre><code>
filtermap(Fun::fun((Key::term(), Value::term()) -&gt; {boolean(), term()}), Map::map(), Opts::map()) -&gt; map()
</code></pre>
<br />

<a name="filtermap_passively_loads_test-0"></a>

### filtermap_passively_loads_test/0 * ###

`filtermap_passively_loads_test() -> any()`

<a name="filtermap_with_link_test-0"></a>

### filtermap_with_link_test/0 * ###

`filtermap_with_link_test() -> any()`

<a name="find-2"></a>

### find/2 ###

<pre><code>
find(Key::term(), Map::map()) -&gt; {ok, term()} | error
</code></pre>
<br />

<a name="find-3"></a>

### find/3 ###

<pre><code>
find(Key::term(), Map::map(), Opts::map()) -&gt; {ok, term()} | error
</code></pre>
<br />

<a name="fold-3"></a>

### fold/3 ###

<pre><code>
fold(Fun::fun((Key::term(), Value::term(), Acc::term()) -&gt; term()), Acc::term(), Map::map()) -&gt; term()
</code></pre>
<br />

<a name="fold-4"></a>

### fold/4 ###

<pre><code>
fold(Fun::fun((Key::term(), Value::term(), Acc::term()) -&gt; term()), Acc::term(), Map::map(), Opts::map()) -&gt; term()
</code></pre>
<br />

<a name="fold_with_typed_link_test-0"></a>

### fold_with_typed_link_test/0 * ###

`fold_with_typed_link_test() -> any()`

<a name="from_list-1"></a>

### from_list/1 ###

<pre><code>
from_list(List::[{Key::term(), Value::term()}]) -&gt; map()
</code></pre>
<br />

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Key::term(), Map::map()) -&gt; term()
</code></pre>
<br />

<a name="get-3"></a>

### get/3 ###

<pre><code>
get(Key::term(), Map::map(), Default::term()) -&gt; term()
</code></pre>
<br />

<a name="get-4"></a>

### get/4 ###

<pre><code>
get(Key::term(), Map::map(), Default::term(), Opts::map()) -&gt; term()
</code></pre>
<br />

Get a value from a map, resolving links as they are encountered in both
the TABM encoded link format, as well as the structured type.

<a name="get_with_link_test-0"></a>

### get_with_link_test/0 * ###

`get_with_link_test() -> any()`

<a name="get_with_typed_link_test-0"></a>

### get_with_typed_link_test/0 * ###

`get_with_typed_link_test() -> any()`

<a name="is_key-2"></a>

### is_key/2 ###

<pre><code>
is_key(Key::term(), Map::map()) -&gt; boolean()
</code></pre>
<br />

<a name="is_key-3"></a>

### is_key/3 ###

<pre><code>
is_key(Key::term(), Map::map(), Opts::map()) -&gt; boolean()
</code></pre>
<br />

<a name="keys-1"></a>

### keys/1 ###

<pre><code>
keys(Map::map()) -&gt; [term()]
</code></pre>
<br />

<a name="keys-2"></a>

### keys/2 ###

<pre><code>
keys(Map::map(), Opts::map()) -&gt; [term()]
</code></pre>
<br />

<a name="map-2"></a>

### map/2 ###

<pre><code>
map(Fun::fun((Key::term(), Value::term()) -&gt; term()), Map::map()) -&gt; map()
</code></pre>
<br />

<a name="map-3"></a>

### map/3 ###

<pre><code>
map(Fun::fun((Key::term(), Value::term()) -&gt; term()), Map::map(), Opts::map()) -&gt; map()
</code></pre>
<br />

<a name="map_with_link_test-0"></a>

### map_with_link_test/0 * ###

`map_with_link_test() -> any()`

<a name="merge-2"></a>

### merge/2 ###

<pre><code>
merge(Map1::map(), Map2::map()) -&gt; map()
</code></pre>
<br />

<a name="merge-3"></a>

### merge/3 ###

<pre><code>
merge(Map1::map(), Map2::map(), Opts::map()) -&gt; map()
</code></pre>
<br />

<a name="put-3"></a>

### put/3 ###

<pre><code>
put(Key::term(), Value::term(), Map::map()) -&gt; map()
</code></pre>
<br />

<a name="put-4"></a>

### put/4 ###

<pre><code>
put(Key::term(), Value::term(), Map::map(), Opts::map()) -&gt; map()
</code></pre>
<br />

<a name="remove-2"></a>

### remove/2 ###

<pre><code>
remove(Key::term(), Map::map()) -&gt; map()
</code></pre>
<br />

<a name="remove-3"></a>

### remove/3 ###

<pre><code>
remove(Key::term(), Map::map(), Opts::map()) -&gt; map()
</code></pre>
<br />

<a name="resolve_on_link_test-0"></a>

### resolve_on_link_test/0 * ###

`resolve_on_link_test() -> any()`

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Map::map()) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="size-2"></a>

### size/2 ###

<pre><code>
size(Map::map(), Opts::map()) -&gt; non_neg_integer()
</code></pre>
<br />

<a name="take-2"></a>

### take/2 ###

<pre><code>
take(N::non_neg_integer(), Map::map()) -&gt; map()
</code></pre>
<br />

<a name="take-3"></a>

### take/3 ###

<pre><code>
take(N::non_neg_integer(), Map::map(), Opts::map()) -&gt; map()
</code></pre>
<br />

<a name="to_list-1"></a>

### to_list/1 ###

<pre><code>
to_list(Map::map()) -&gt; [{Key::term(), Value::term()}]
</code></pre>
<br />

<a name="to_list-2"></a>

### to_list/2 ###

<pre><code>
to_list(Map::map(), Opts::map()) -&gt; [{Key::term(), Value::term()}]
</code></pre>
<br />

<a name="update_with-3"></a>

### update_with/3 ###

<pre><code>
update_with(Key::term(), Fun::fun((Value::term()) -&gt; term()), Map::map()) -&gt; map()
</code></pre>
<br />

<a name="update_with-4"></a>

### update_with/4 ###

<pre><code>
update_with(Key::term(), Fun::fun((Value::term()) -&gt; term()), Map::map(), Opts::map()) -&gt; map()
</code></pre>
<br />

<a name="values-1"></a>

### values/1 ###

<pre><code>
values(Map::map()) -&gt; [term()]
</code></pre>
<br />

<a name="values-2"></a>

### values/2 ###

<pre><code>
values(Map::map(), Opts::map()) -&gt; [term()]
</code></pre>
<br />

<a name="with-2"></a>

### with/2 ###

<pre><code>
with(Keys::[term()], Map::map()) -&gt; map()
</code></pre>
<br />

<a name="with-3"></a>

### with/3 ###

<pre><code>
with(Keys::[term()], Map::map(), Opts::map()) -&gt; map()
</code></pre>
<br />

<a name="without-2"></a>

### without/2 ###

<pre><code>
without(Keys::[term()], Map::map()) -&gt; map()
</code></pre>
<br />

<a name="without-3"></a>

### without/3 ###

<pre><code>
without(Keys::[term()], Map::map(), Opts::map()) -&gt; map()
</code></pre>
<br />

