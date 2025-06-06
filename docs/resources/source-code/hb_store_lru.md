# [Module hb_store_lru.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store_lru.erl)




An in-memory store implementation, following the `hb_store` behavior
and interface.

<a name="description"></a>

## Description ##

This implementation uses a least-recently-used cache first,
and offloads evicted data to a specified non-volatile store over time.

This cache is registered under `{in_memory, HTTPServerID}`, in `hb_name`
so that all processes that are executing using the HTTP server’s Opts
can find it quickly.

The least-recently-used strategy (first is the most recent used, last is the
least recently used) is implemented by keeping track of the order and bytes
on ets tables:
- A cache table containing all the entries along with the value size and
key index.
- A cache indexing table containing all the index pointing to the keys. The
IDs are then sorted to ease the eviction policy.
- A cache statistics table containing all the information about the cache
size, capacity, and indexing.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_cache_entry-3">add_cache_entry/3*</a></td><td></td></tr><tr><td valign="top"><a href="#add_cache_index-3">add_cache_index/3*</a></td><td></td></tr><tr><td valign="top"><a href="#append_key_to_group-2">append_key_to_group/2*</a></td><td></td></tr><tr><td valign="top"><a href="#assign_new_entry-7">assign_new_entry/7*</a></td><td></td></tr><tr><td valign="top"><a href="#cache_size-1">cache_size/1*</a></td><td></td></tr><tr><td valign="top"><a href="#cache_tail_key-1">cache_tail_key/1*</a></td><td></td></tr><tr><td valign="top"><a href="#cache_term_test-0">cache_term_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#clean_old_link-2">clean_old_link/2*</a></td><td>Remove the link association for the the old linked data to the given key.</td></tr><tr><td valign="top"><a href="#convert_if_list-1">convert_if_list/1*</a></td><td></td></tr><tr><td valign="top"><a href="#decrease_cache_size-2">decrease_cache_size/2*</a></td><td></td></tr><tr><td valign="top"><a href="#delete_cache_entry-2">delete_cache_entry/2*</a></td><td></td></tr><tr><td valign="top"><a href="#delete_cache_index-2">delete_cache_index/2*</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_dir-2">ensure_dir/2*</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_dir-3">ensure_dir/3*</a></td><td></td></tr><tr><td valign="top"><a href="#ets_keys-2">ets_keys/2*</a></td><td>List all of the keys in the store for a given path, supporting a special
case for the root.</td></tr><tr><td valign="top"><a href="#evict_all_entries-2">evict_all_entries/2*</a></td><td></td></tr><tr><td valign="top"><a href="#evict_but_able_to_read_from_fs_store_test-0">evict_but_able_to_read_from_fs_store_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#evict_items_with_insufficient_space_test-0">evict_items_with_insufficient_space_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#evict_oldest_entry-3">evict_oldest_entry/3*</a></td><td></td></tr><tr><td valign="top"><a href="#evict_oldest_entry-4">evict_oldest_entry/4*</a></td><td></td></tr><tr><td valign="top"><a href="#evict_oldest_items_test-0">evict_oldest_items_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_cache_with_retry-2">fetch_cache_with_retry/2*</a></td><td></td></tr><tr><td valign="top"><a href="#fetch_cache_with_retry-3">fetch_cache_with_retry/3*</a></td><td></td></tr><tr><td valign="top"><a href="#get_cache_entry-2">get_cache_entry/2*</a></td><td></td></tr><tr><td valign="top"><a href="#get_index_id-1">get_index_id/1*</a></td><td></td></tr><tr><td valign="top"><a href="#get_persistent_store-1">get_persistent_store/1*</a></td><td></td></tr><tr><td valign="top"><a href="#handle_group-3">handle_group/3*</a></td><td></td></tr><tr><td valign="top"><a href="#increase_cache_size-2">increase_cache_size/2*</a></td><td></td></tr><tr><td valign="top"><a href="#init-2">init/2*</a></td><td>Create the <code>ets</code> tables for the LRU cache:
- The cache of data itself (public, with read concurrency enabled)
- A set for the LRU's stats.</td></tr><tr><td valign="top"><a href="#join-1">join/1*</a></td><td></td></tr><tr><td valign="top"><a href="#link_cache_entry-4">link_cache_entry/4*</a></td><td></td></tr><tr><td valign="top"><a href="#list-2">list/2</a></td><td>List all the keys registered.</td></tr><tr><td valign="top"><a href="#list_test-0">list_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#make_group-2">make_group/2</a></td><td>Create a directory inside the store.</td></tr><tr><td valign="top"><a href="#make_link-3">make_link/3</a></td><td>Make a link from a key to another in the store.</td></tr><tr><td valign="top"><a href="#maybe_convert_to_binary-1">maybe_convert_to_binary/1*</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_create_dir-3">maybe_create_dir/3*</a></td><td></td></tr><tr><td valign="top"><a href="#offload_to_store-5">offload_to_store/5*</a></td><td></td></tr><tr><td valign="top"><a href="#put_cache_entry-4">put_cache_entry/4*</a></td><td></td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Retrieve value in the cache from the given key.</td></tr><tr><td valign="top"><a href="#replace_entry-5">replace_entry/5*</a></td><td></td></tr><tr><td valign="top"><a href="#replace_link_test-0">replace_link_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Reset the store by completely cleaning the ETS tables and
delegate the reset to the underlying offloading store.</td></tr><tr><td valign="top"><a href="#reset_test-0">reset_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve-2">resolve/2</a></td><td></td></tr><tr><td valign="top"><a href="#resolve-3">resolve/3*</a></td><td></td></tr><tr><td valign="top"><a href="#scope-1">scope/1</a></td><td>The LRU store is always local, for now.</td></tr><tr><td valign="top"><a href="#server_loop-2">server_loop/2*</a></td><td></td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>The default capacity is used when no capacity is provided in the store
options.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop the LRU in memory by offloading the keys in the ETS tables
before exiting the process.</td></tr><tr><td valign="top"><a href="#stop_test-0">stop_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#sync-1">sync/1*</a></td><td>Force the caller to wait until the server has fully processed all
messages in its mailbox, up to the initiation of the call.</td></tr><tr><td valign="top"><a href="#table_keys-1">table_keys/1*</a></td><td></td></tr><tr><td valign="top"><a href="#table_keys-2">table_keys/2*</a></td><td></td></tr><tr><td valign="top"><a href="#table_keys-4">table_keys/4*</a></td><td></td></tr><tr><td valign="top"><a href="#test_opts-1">test_opts/1*</a></td><td>Generate a set of options for testing.</td></tr><tr><td valign="top"><a href="#test_opts-2">test_opts/2*</a></td><td></td></tr><tr><td valign="top"><a href="#type-2">type/2</a></td><td>Determine the type of a key in the store.</td></tr><tr><td valign="top"><a href="#type_test-0">type_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#unknown_value_test-0">unknown_value_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#update_cache_size-3">update_cache_size/3*</a></td><td></td></tr><tr><td valign="top"><a href="#update_recently_used-3">update_recently_used/3*</a></td><td></td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>Write an entry in the cache.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_cache_entry-3"></a>

### add_cache_entry/3 * ###

`add_cache_entry(X1, Key, Value) -> any()`

<a name="add_cache_index-3"></a>

### add_cache_index/3 * ###

`add_cache_index(X1, ID, Key) -> any()`

<a name="append_key_to_group-2"></a>

### append_key_to_group/2 * ###

`append_key_to_group(Key, Group) -> any()`

<a name="assign_new_entry-7"></a>

### assign_new_entry/7 * ###

`assign_new_entry(State, Key, Value, ValueSize, Capacity, Group, Opts) -> any()`

<a name="cache_size-1"></a>

### cache_size/1 * ###

`cache_size(X1) -> any()`

<a name="cache_tail_key-1"></a>

### cache_tail_key/1 * ###

`cache_tail_key(X1) -> any()`

<a name="cache_term_test-0"></a>

### cache_term_test/0 * ###

`cache_term_test() -> any()`

<a name="clean_old_link-2"></a>

### clean_old_link/2 * ###

`clean_old_link(Table, Link) -> any()`

Remove the link association for the the old linked data to the given key

<a name="convert_if_list-1"></a>

### convert_if_list/1 * ###

`convert_if_list(Value) -> any()`

<a name="decrease_cache_size-2"></a>

### decrease_cache_size/2 * ###

`decrease_cache_size(X1, Size) -> any()`

<a name="delete_cache_entry-2"></a>

### delete_cache_entry/2 * ###

`delete_cache_entry(X1, Key) -> any()`

<a name="delete_cache_index-2"></a>

### delete_cache_index/2 * ###

`delete_cache_index(X1, ID) -> any()`

<a name="ensure_dir-2"></a>

### ensure_dir/2 * ###

`ensure_dir(State, Path) -> any()`

<a name="ensure_dir-3"></a>

### ensure_dir/3 * ###

`ensure_dir(State, CurrentPath, Rest) -> any()`

<a name="ets_keys-2"></a>

### ets_keys/2 * ###

`ets_keys(Opts, Path) -> any()`

List all of the keys in the store for a given path, supporting a special
case for the root.

<a name="evict_all_entries-2"></a>

### evict_all_entries/2 * ###

`evict_all_entries(X1, Opts) -> any()`

<a name="evict_but_able_to_read_from_fs_store_test-0"></a>

### evict_but_able_to_read_from_fs_store_test/0 * ###

`evict_but_able_to_read_from_fs_store_test() -> any()`

<a name="evict_items_with_insufficient_space_test-0"></a>

### evict_items_with_insufficient_space_test/0 * ###

`evict_items_with_insufficient_space_test() -> any()`

<a name="evict_oldest_entry-3"></a>

### evict_oldest_entry/3 * ###

`evict_oldest_entry(State, ValueSize, Opts) -> any()`

<a name="evict_oldest_entry-4"></a>

### evict_oldest_entry/4 * ###

`evict_oldest_entry(State, ValueSize, FreeSize, Opts) -> any()`

<a name="evict_oldest_items_test-0"></a>

### evict_oldest_items_test/0 * ###

`evict_oldest_items_test() -> any()`

<a name="fetch_cache_with_retry-2"></a>

### fetch_cache_with_retry/2 * ###

`fetch_cache_with_retry(Opts, Key) -> any()`

<a name="fetch_cache_with_retry-3"></a>

### fetch_cache_with_retry/3 * ###

`fetch_cache_with_retry(Opts, Key, Retries) -> any()`

<a name="get_cache_entry-2"></a>

### get_cache_entry/2 * ###

`get_cache_entry(Table, Key) -> any()`

<a name="get_index_id-1"></a>

### get_index_id/1 * ###

`get_index_id(X1) -> any()`

<a name="get_persistent_store-1"></a>

### get_persistent_store/1 * ###

`get_persistent_store(Opts) -> any()`

<a name="handle_group-3"></a>

### handle_group/3 * ###

`handle_group(State, Key, Opts) -> any()`

<a name="increase_cache_size-2"></a>

### increase_cache_size/2 * ###

`increase_cache_size(X1, ValueSize) -> any()`

<a name="init-2"></a>

### init/2 * ###

`init(From, StoreOpts) -> any()`

Create the `ets` tables for the LRU cache:
- The cache of data itself (public, with read concurrency enabled)
- A set for the LRU's stats.
- An ordered set for the cache's index.

<a name="join-1"></a>

### join/1 * ###

`join(Key) -> any()`

<a name="link_cache_entry-4"></a>

### link_cache_entry/4 * ###

`link_cache_entry(State, Existing, New, Opts) -> any()`

<a name="list-2"></a>

### list/2 ###

`list(Opts, Path) -> any()`

List all the keys registered.

<a name="list_test-0"></a>

### list_test/0 * ###

`list_test() -> any()`

<a name="make_group-2"></a>

### make_group/2 ###

`make_group(Opts, Key) -> any()`

Create a directory inside the store.

<a name="make_link-3"></a>

### make_link/3 ###

`make_link(Opts, Link, New) -> any()`

Make a link from a key to another in the store.

<a name="maybe_convert_to_binary-1"></a>

### maybe_convert_to_binary/1 * ###

`maybe_convert_to_binary(Value) -> any()`

<a name="maybe_create_dir-3"></a>

### maybe_create_dir/3 * ###

`maybe_create_dir(State, DirPath, Value) -> any()`

<a name="offload_to_store-5"></a>

### offload_to_store/5 * ###

`offload_to_store(TailKey, TailValue, Links, Group, Opts) -> any()`

<a name="put_cache_entry-4"></a>

### put_cache_entry/4 * ###

`put_cache_entry(State, Key, Value, Opts) -> any()`

<a name="read-2"></a>

### read/2 ###

`read(Opts, RawKey) -> any()`

Retrieve value in the cache from the given key.
Because the cache uses LRU, the key is moved on the most recent used key to
cycle and re-prioritize cache entry.

<a name="replace_entry-5"></a>

### replace_entry/5 * ###

`replace_entry(State, Key, Value, ValueSize, X5) -> any()`

<a name="replace_link_test-0"></a>

### replace_link_test/0 * ###

`replace_link_test() -> any()`

<a name="reset-1"></a>

### reset/1 ###

`reset(Opts) -> any()`

Reset the store by completely cleaning the ETS tables and
delegate the reset to the underlying offloading store.

<a name="reset_test-0"></a>

### reset_test/0 * ###

`reset_test() -> any()`

<a name="resolve-2"></a>

### resolve/2 ###

`resolve(Opts, Key) -> any()`

<a name="resolve-3"></a>

### resolve/3 * ###

`resolve(Opts, CurrPath, Rest) -> any()`

<a name="scope-1"></a>

### scope/1 ###

`scope(X1) -> any()`

The LRU store is always local, for now.

<a name="server_loop-2"></a>

### server_loop/2 * ###

`server_loop(State, Opts) -> any()`

<a name="start-1"></a>

### start/1 ###

`start(StoreOpts) -> any()`

The default capacity is used when no capacity is provided in the store
options. Maximum number of retries when fetching cache entries that aren't
immediately found due to timing issues in concurrent operations.
Start the LRU cache.

<a name="stop-1"></a>

### stop/1 ###

`stop(Opts) -> any()`

Stop the LRU in memory by offloading the keys in the ETS tables
before exiting the process.

<a name="stop_test-0"></a>

### stop_test/0 * ###

`stop_test() -> any()`

<a name="sync-1"></a>

### sync/1 * ###

`sync(Server) -> any()`

Force the caller to wait until the server has fully processed all
messages in its mailbox, up to the initiation of the call.

<a name="table_keys-1"></a>

### table_keys/1 * ###

`table_keys(TableName) -> any()`

<a name="table_keys-2"></a>

### table_keys/2 * ###

`table_keys(TableName, Prefix) -> any()`

<a name="table_keys-4"></a>

### table_keys/4 * ###

`table_keys(TableName, CurrentKey, Prefix, Acc) -> any()`

<a name="test_opts-1"></a>

### test_opts/1 * ###

`test_opts(PersistentStore) -> any()`

Generate a set of options for testing. The default is to use an `fs` store
as the persistent backing.

<a name="test_opts-2"></a>

### test_opts/2 * ###

`test_opts(PersistentStore, Capacity) -> any()`

<a name="type-2"></a>

### type/2 ###

`type(Opts, Key) -> any()`

Determine the type of a key in the store.

<a name="type_test-0"></a>

### type_test/0 * ###

`type_test() -> any()`

<a name="unknown_value_test-0"></a>

### unknown_value_test/0 * ###

`unknown_value_test() -> any()`

<a name="update_cache_size-3"></a>

### update_cache_size/3 * ###

`update_cache_size(X1, PreviousSize, NewSize) -> any()`

<a name="update_recently_used-3"></a>

### update_recently_used/3 * ###

`update_recently_used(State, Key, Entry) -> any()`

<a name="write-3"></a>

### write/3 ###

`write(Opts, RawKey, Value) -> any()`

Write an entry in the cache.

After writing, the LRU is updated by moving the key in the most-recently-used
key to cycle and re-prioritize cache entry.

