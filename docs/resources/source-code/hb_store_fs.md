# [Module hb_store_fs.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store_fs.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_prefix-2">add_prefix/2*</a></td><td>Add the directory prefix to a path.</td></tr><tr><td valign="top"><a href="#list-2">list/2</a></td><td>List contents of a directory in the store.</td></tr><tr><td valign="top"><a href="#make_group-2">make_group/2</a></td><td>Create a directory (group) in the store.</td></tr><tr><td valign="top"><a href="#make_link-3">make_link/3</a></td><td>Create a symlink, handling the case where the link would point to itself.</td></tr><tr><td valign="top"><a href="#read-1">read/1*</a></td><td></td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read a key from the store, following symlinks as needed.</td></tr><tr><td valign="top"><a href="#remove_prefix-2">remove_prefix/2*</a></td><td>Remove the directory prefix from a path.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Reset the store by completely removing its directory and recreating it.</td></tr><tr><td valign="top"><a href="#resolve-2">resolve/2</a></td><td>Replace links in a path successively, returning the final path.</td></tr><tr><td valign="top"><a href="#resolve-3">resolve/3*</a></td><td></td></tr><tr><td valign="top"><a href="#scope-1">scope/1</a></td><td>The file-based store is always local, for now.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Initialize the file system store with the given data directory.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop the file system store.</td></tr><tr><td valign="top"><a href="#type-1">type/1*</a></td><td></td></tr><tr><td valign="top"><a href="#type-2">type/2</a></td><td>Determine the type of a key in the store.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>Write a value to the specified path in the store.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_prefix-2"></a>

### add_prefix/2 * ###

`add_prefix(X1, Path) -> any()`

Add the directory prefix to a path.

<a name="list-2"></a>

### list/2 ###

`list(Opts, Path) -> any()`

List contents of a directory in the store.

<a name="make_group-2"></a>

### make_group/2 ###

`make_group(Opts, Path) -> any()`

Create a directory (group) in the store.

<a name="make_link-3"></a>

### make_link/3 ###

`make_link(Opts, Link, New) -> any()`

Create a symlink, handling the case where the link would point to itself.

<a name="read-1"></a>

### read/1 * ###

`read(Path) -> any()`

<a name="read-2"></a>

### read/2 ###

`read(Opts, Key) -> any()`

Read a key from the store, following symlinks as needed.

<a name="remove_prefix-2"></a>

### remove_prefix/2 * ###

`remove_prefix(X1, Path) -> any()`

Remove the directory prefix from a path.

<a name="reset-1"></a>

### reset/1 ###

`reset(X1) -> any()`

Reset the store by completely removing its directory and recreating it.

<a name="resolve-2"></a>

### resolve/2 ###

`resolve(Opts, RawPath) -> any()`

Replace links in a path successively, returning the final path.
Each element of the path is resolved in turn, with the result of each
resolution becoming the prefix for the next resolution. This allows
paths to resolve across many links. For example, a structure as follows:

/a/b/c: "Not the right data"
/a/b -> /a/alt-b
/a/alt-b/c: "Correct data"

will resolve "a/b/c" to "Correct data".

<a name="resolve-3"></a>

### resolve/3 * ###

`resolve(Opts, CurrPath, Rest) -> any()`

<a name="scope-1"></a>

### scope/1 ###

`scope(X1) -> any()`

The file-based store is always local, for now. In the future, we may
want to allow that an FS store is shared across a cluster and thus remote.

<a name="start-1"></a>

### start/1 ###

`start(X1) -> any()`

Initialize the file system store with the given data directory.

<a name="stop-1"></a>

### stop/1 ###

`stop(X1) -> any()`

Stop the file system store. Currently a no-op.

<a name="type-1"></a>

### type/1 * ###

`type(Path) -> any()`

<a name="type-2"></a>

### type/2 ###

`type(Opts, Key) -> any()`

Determine the type of a key in the store.

<a name="write-3"></a>

### write/3 ###

`write(Opts, PathComponents, Value) -> any()`

Write a value to the specified path in the store.

