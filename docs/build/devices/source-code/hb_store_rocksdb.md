# [Module hb_store_rocksdb.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store_rocksdb.erl)




A process wrapper over rocksdb storage.

__Behaviours:__ [`gen_server`](gen_server.md), [`hb_store`](hb_store.md).

<a name="description"></a>

## Description ##

Replicates functionality of the
hb_fs_store module.

Encodes the item types with the help of prefixes, see `encode_value/2`
and `decode_value/1`
<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = binary() | list()
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = binary() | list()
</code></pre>




### <a name="type-value_type">value_type()</a> ###


<pre><code>
value_type() = link | raw | group
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_path-3">add_path/3</a></td><td>Add two path components together.</td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#collect-1">collect/1*</a></td><td></td></tr><tr><td valign="top"><a href="#collect-2">collect/2*</a></td><td></td></tr><tr><td valign="top"><a href="#convert_if_list-1">convert_if_list/1*</a></td><td></td></tr><tr><td valign="top"><a href="#decode_value-1">decode_value/1*</a></td><td></td></tr><tr><td valign="top"><a href="#do_read-2">do_read/2*</a></td><td></td></tr><tr><td valign="top"><a href="#do_resolve-3">do_resolve/3*</a></td><td></td></tr><tr><td valign="top"><a href="#do_write-3">do_write/3*</a></td><td>Write given Key and Value to the database.</td></tr><tr><td valign="top"><a href="#enabled-0">enabled/0</a></td><td>Returns whether the RocksDB store is enabled.</td></tr><tr><td valign="top"><a href="#encode_value-2">encode_value/2*</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_dir-2">ensure_dir/2*</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_dir-3">ensure_dir/3*</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_list-1">ensure_list/1*</a></td><td>Ensure that the given filename is a list, not a binary.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#join-1">join/1*</a></td><td></td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td>List all items registered in rocksdb store.</td></tr><tr><td valign="top"><a href="#list-2">list/2</a></td><td>Returns the full list of items stored under the given path.</td></tr><tr><td valign="top"><a href="#make_group-2">make_group/2</a></td><td>Creates group under the given path.</td></tr><tr><td valign="top"><a href="#make_link-3">make_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_append_key_to_group-2">maybe_append_key_to_group/2*</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_convert_to_binary-1">maybe_convert_to_binary/1*</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_create_dir-3">maybe_create_dir/3*</a></td><td></td></tr><tr><td valign="top"><a href="#open_rockdb-1">open_rockdb/1*</a></td><td></td></tr><tr><td valign="top"><a href="#path-2">path/2</a></td><td>Return path.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read data by the key.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td></td></tr><tr><td valign="top"><a href="#resolve-2">resolve/2</a></td><td>Replace links in a path with the target of the link.</td></tr><tr><td valign="top"><a href="#scope-1">scope/1</a></td><td>Return scope (local).</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Start the RocksDB store.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#type-2">type/2</a></td><td>Get type of the current item.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>Write given Key and Value to the database.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_path-3"></a>

### add_path/3 ###

`add_path(Opts, Path1, Path2) -> any()`

Add two path components together. // is not used

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

<a name="collect-1"></a>

### collect/1 * ###

`collect(Iterator) -> any()`

<a name="collect-2"></a>

### collect/2 * ###

`collect(Iterator, Acc) -> any()`

<a name="convert_if_list-1"></a>

### convert_if_list/1 * ###

`convert_if_list(Value) -> any()`

<a name="decode_value-1"></a>

### decode_value/1 * ###

<pre><code>
decode_value(X1::binary()) -&gt; {<a href="#type-value_type">value_type()</a>, binary()}
</code></pre>
<br />

<a name="do_read-2"></a>

### do_read/2 * ###

`do_read(Opts, Key) -> any()`

<a name="do_resolve-3"></a>

### do_resolve/3 * ###

`do_resolve(Opts, FinalPath, Rest) -> any()`

<a name="do_write-3"></a>

### do_write/3 * ###

<pre><code>
do_write(Opts, Key, Value) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = map()</code></li><li><code>Key = <a href="#type-key">key()</a></code></li><li><code>Value = <a href="#type-value">value()</a></code></li><li><code>Result = ok | {error, any()}</code></li></ul>

Write given Key and Value to the database

<a name="enabled-0"></a>

### enabled/0 ###

`enabled() -> any()`

Returns whether the RocksDB store is enabled.

<a name="encode_value-2"></a>

### encode_value/2 * ###

<pre><code>
encode_value(X1::<a href="#type-value_type">value_type()</a>, Value::binary()) -&gt; binary()
</code></pre>
<br />

<a name="ensure_dir-2"></a>

### ensure_dir/2 * ###

`ensure_dir(DBHandle, BaseDir) -> any()`

<a name="ensure_dir-3"></a>

### ensure_dir/3 * ###

`ensure_dir(DBHandle, CurrentPath, Rest) -> any()`

<a name="ensure_list-1"></a>

### ensure_list/1 * ###

`ensure_list(Value) -> any()`

Ensure that the given filename is a list, not a binary.

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Request, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(Dir) -> any()`

<a name="join-1"></a>

### join/1 * ###

`join(Key) -> any()`

<a name="list-0"></a>

### list/0 ###

`list() -> any()`

List all items registered in rocksdb store. Should be used only
for testing/debugging, as the underlying operation is doing full traversal
on the KV storage, and is slow.

<a name="list-2"></a>

### list/2 ###

<pre><code>
list(Opts, Path) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = any()</code></li><li><code>Path = any()</code></li><li><code>Result = {ok, [string()]} | {error, term()}</code></li></ul>

Returns the full list of items stored under the given path. Where the path
child items is relevant to the path of parentItem. (Same as in `hb_store_fs`).

<a name="make_group-2"></a>

### make_group/2 ###

<pre><code>
make_group(Opts, Key) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = any()</code></li><li><code>Key = binary()</code></li><li><code>Result = ok | {error, already_added}</code></li></ul>

Creates group under the given path.

<a name="make_link-3"></a>

### make_link/3 ###

<pre><code>
make_link(Opts::any(), Key1::<a href="#type-key">key()</a>, New::<a href="#type-key">key()</a>) -&gt; ok
</code></pre>
<br />

<a name="maybe_append_key_to_group-2"></a>

### maybe_append_key_to_group/2 * ###

`maybe_append_key_to_group(Key, CurrentDirContents) -> any()`

<a name="maybe_convert_to_binary-1"></a>

### maybe_convert_to_binary/1 * ###

`maybe_convert_to_binary(Value) -> any()`

<a name="maybe_create_dir-3"></a>

### maybe_create_dir/3 * ###

`maybe_create_dir(DBHandle, DirPath, Value) -> any()`

<a name="open_rockdb-1"></a>

### open_rockdb/1 * ###

`open_rockdb(RawDir) -> any()`

<a name="path-2"></a>

### path/2 ###

`path(Opts, Path) -> any()`

Return path

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(Opts, Key) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = map()</code></li><li><code>Key = <a href="#type-key">key()</a> | list()</code></li><li><code>Result = {ok, <a href="#type-value">value()</a>} | not_found | {error, {corruption, string()}} | {error, any()}</code></li></ul>

Read data by the key.
Recursively follows link messages

<a name="reset-1"></a>

### reset/1 ###

<pre><code>
reset(Opts::[]) -&gt; ok | no_return()
</code></pre>
<br />

<a name="resolve-2"></a>

### resolve/2 ###

<pre><code>
resolve(Opts, Path) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = any()</code></li><li><code>Path = binary() | list()</code></li><li><code>Result = not_found | string()</code></li></ul>

Replace links in a path with the target of the link.

<a name="scope-1"></a>

### scope/1 ###

`scope(X1) -> any()`

Return scope (local)

<a name="start-1"></a>

### start/1 ###

`start(Opts) -> any()`

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Opts) -> any()`

Start the RocksDB store.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Opts::any()) -&gt; ok
</code></pre>
<br />

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="type-2"></a>

### type/2 ###

<pre><code>
type(Opts, Key) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = map()</code></li><li><code>Key = binary()</code></li><li><code>Result = composite | simple | not_found</code></li></ul>

Get type of the current item

<a name="write-3"></a>

### write/3 ###

<pre><code>
write(Opts, Key, Value) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = map()</code></li><li><code>Key = <a href="#type-key">key()</a></code></li><li><code>Value = <a href="#type-value">value()</a></code></li><li><code>Result = ok | {error, any()}</code></li></ul>

Write given Key and Value to the database

