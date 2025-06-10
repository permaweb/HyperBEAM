# [Module hb_store.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_path-2">add_path/2</a></td><td>Add two path components together.</td></tr><tr><td valign="top"><a href="#add_path-3">add_path/3</a></td><td></td></tr><tr><td valign="top"><a href="#behavior_info-1">behavior_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#call_all-3">call_all/3*</a></td><td>Call a function on all modules in the store.</td></tr><tr><td valign="top"><a href="#call_function-3">call_function/3*</a></td><td>Call a function on the first store module that succeeds.</td></tr><tr><td valign="top"><a href="#filter-2">filter/2</a></td><td>Takes a store object and a filter function or match spec, returning a
new store object with only the modules that match the filter.</td></tr><tr><td valign="top"><a href="#generate_test_suite-1">generate_test_suite/1</a></td><td></td></tr><tr><td valign="top"><a href="#generate_test_suite-2">generate_test_suite/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_store_scope-1">get_store_scope/1*</a></td><td>Ask a store for its own scope.</td></tr><tr><td valign="top"><a href="#hierarchical_path_resolution_test-1">hierarchical_path_resolution_test/1*</a></td><td>Ensure that we can resolve links through a directory.</td></tr><tr><td valign="top"><a href="#join-1">join/1</a></td><td>Join a list of path components together.</td></tr><tr><td valign="top"><a href="#list-2">list/2</a></td><td>List the keys in a group in the store.</td></tr><tr><td valign="top"><a href="#make_group-2">make_group/2</a></td><td>Make a group in the store.</td></tr><tr><td valign="top"><a href="#make_link-3">make_link/3</a></td><td>Make a link from one path to another in the store.</td></tr><tr><td valign="top"><a href="#path-1">path/1</a></td><td>Create a path from a list of path components.</td></tr><tr><td valign="top"><a href="#path-2">path/2</a></td><td></td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read a key from the store.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Delete all of the keys in a store.</td></tr><tr><td valign="top"><a href="#resolve-2">resolve/2</a></td><td>Follow links through the store to resolve a path to its ultimate target.</td></tr><tr><td valign="top"><a href="#resursive_path_resolution_test-1">resursive_path_resolution_test/1*</a></td><td>Ensure that we can resolve links recursively.</td></tr><tr><td valign="top"><a href="#scope-2">scope/2</a></td><td>Limit the store scope to only a specific (set of) option(s).</td></tr><tr><td valign="top"><a href="#simple_path_resolution_test-1">simple_path_resolution_test/1*</a></td><td>Test path resolution dynamics.</td></tr><tr><td valign="top"><a href="#sort-2">sort/2</a></td><td>Order a store by a preference of its scopes.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#store_suite_test_-0">store_suite_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_stores-0">test_stores/0</a></td><td></td></tr><tr><td valign="top"><a href="#type-2">type/2</a></td><td>Get the type of element of a given path in the store.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>Write a key with a value to the store.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_path-2"></a>

### add_path/2 ###

`add_path(Path1, Path2) -> any()`

Add two path components together. If no store implements the add_path
function, we concatenate the paths.

<a name="add_path-3"></a>

### add_path/3 ###

`add_path(Store, Path1, Path2) -> any()`

<a name="behavior_info-1"></a>

### behavior_info/1 ###

`behavior_info(X1) -> any()`

<a name="call_all-3"></a>

### call_all/3 * ###

`call_all(X, Function, Args) -> any()`

Call a function on all modules in the store.

<a name="call_function-3"></a>

### call_function/3 * ###

`call_function(X, Function, Args) -> any()`

Call a function on the first store module that succeeds. Returns its
result, or no_viable_store if none of the stores succeed.

<a name="filter-2"></a>

### filter/2 ###

`filter(Module, Filter) -> any()`

Takes a store object and a filter function or match spec, returning a
new store object with only the modules that match the filter. The filter
function takes 2 arguments: the scope and the options. It calls the store's
scope function to get the scope of the module.

<a name="generate_test_suite-1"></a>

### generate_test_suite/1 ###

`generate_test_suite(Suite) -> any()`

<a name="generate_test_suite-2"></a>

### generate_test_suite/2 ###

`generate_test_suite(Suite, Stores) -> any()`

<a name="get_store_scope-1"></a>

### get_store_scope/1 * ###

`get_store_scope(Store) -> any()`

Ask a store for its own scope. If it doesn't have one, return the
default scope (local).

<a name="hierarchical_path_resolution_test-1"></a>

### hierarchical_path_resolution_test/1 * ###

`hierarchical_path_resolution_test(Opts) -> any()`

Ensure that we can resolve links through a directory.

<a name="join-1"></a>

### join/1 ###

`join(Path) -> any()`

Join a list of path components together.

<a name="list-2"></a>

### list/2 ###

`list(Modules, Path) -> any()`

List the keys in a group in the store. Use only in debugging.
The hyperbeam model assumes that stores are built as efficient hash-based
structures, so this is likely to be very slow for most stores.

<a name="make_group-2"></a>

### make_group/2 ###

`make_group(Modules, Path) -> any()`

Make a group in the store. A group can be seen as a namespace or
'directory' in a filesystem.

<a name="make_link-3"></a>

### make_link/3 ###

`make_link(Modules, Existing, New) -> any()`

Make a link from one path to another in the store.

<a name="path-1"></a>

### path/1 ###

`path(Path) -> any()`

Create a path from a list of path components. If no store implements
the path function, we return the path with the 'default' transformation (id).

<a name="path-2"></a>

### path/2 ###

`path(X1, Path) -> any()`

<a name="read-2"></a>

### read/2 ###

`read(Modules, Key) -> any()`

Read a key from the store.

<a name="reset-1"></a>

### reset/1 ###

`reset(Modules) -> any()`

Delete all of the keys in a store. Should be used with extreme
caution. Lost data can lose money in many/most of hyperbeam's use cases.

<a name="resolve-2"></a>

### resolve/2 ###

`resolve(Modules, Path) -> any()`

Follow links through the store to resolve a path to its ultimate target.

<a name="resursive_path_resolution_test-1"></a>

### resursive_path_resolution_test/1 * ###

`resursive_path_resolution_test(Opts) -> any()`

Ensure that we can resolve links recursively.

<a name="scope-2"></a>

### scope/2 ###

`scope(Scope, Opts) -> any()`

Limit the store scope to only a specific (set of) option(s).
Takes either an Opts message or store, and either a single scope or a list
of scopes.

<a name="simple_path_resolution_test-1"></a>

### simple_path_resolution_test/1 * ###

`simple_path_resolution_test(Opts) -> any()`

Test path resolution dynamics.

<a name="sort-2"></a>

### sort/2 ###

`sort(Stores, PreferenceOrder) -> any()`

Order a store by a preference of its scopes. This is useful for making
sure that faster (or perhaps cheaper) stores are used first. If a list is
provided, it will be used as a preference order. If a map is provided,
scopes will be ordered by the scores in the map. Any unknown scopes will
default to a score of 0.

<a name="start-1"></a>

### start/1 ###

`start(Modules) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(Modules) -> any()`

<a name="store_suite_test_-0"></a>

### store_suite_test_/0 * ###

`store_suite_test_() -> any()`

<a name="test_stores-0"></a>

### test_stores/0 ###

`test_stores() -> any()`

<a name="type-2"></a>

### type/2 ###

`type(Modules, Path) -> any()`

Get the type of element of a given path in the store. This can be
a performance killer if the store is remote etc. Use only when necessary.

<a name="write-3"></a>

### write/3 ###

`write(Modules, Key, Value) -> any()`

Write a key with a value to the store.

