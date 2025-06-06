# [Module hb_store_remote_node.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store_remote_node.erl)




A store module that reads data from another AO node.

<a name="description"></a>

## Description ##
Notably, this store only provides the _read_ side of the store interface.
The write side could be added, returning an commitment that the data has
been written to the remote node. In that case, the node would probably want
to upload it to an Arweave bundler to ensure persistence, too.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#make_link-3">make_link/3</a></td><td>Link a source to a destination in the remote node.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read a key from the remote node.</td></tr><tr><td valign="top"><a href="#read_test-0">read_test/0*</a></td><td>Test that we can create a store, write a random message to it, then
start a remote node with that store, and read the message from it.</td></tr><tr><td valign="top"><a href="#resolve-2">resolve/2</a></td><td>Resolve a key path in the remote store.</td></tr><tr><td valign="top"><a href="#scope-1">scope/1</a></td><td>Return the scope of this store.</td></tr><tr><td valign="top"><a href="#type-2">type/2</a></td><td>Determine the type of value at a given key.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>Write a key to the remote node.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="make_link-3"></a>

### make_link/3 ###

`make_link(Opts, Source, Destination) -> any()`

Link a source to a destination in the remote node.

Constructs an HTTP POST link request. If a wallet is provided,
the message is signed. Returns {ok, Path} on HTTP 200, or
{error, Reason} on failure.

<a name="read-2"></a>

### read/2 ###

`read(Opts, Key) -> any()`

Read a key from the remote node.

Makes an HTTP GET request to the remote node and returns the
committed message.

<a name="read_test-0"></a>

### read_test/0 * ###

`read_test() -> any()`

Test that we can create a store, write a random message to it, then
start a remote node with that store, and read the message from it.

<a name="resolve-2"></a>

### resolve/2 ###

`resolve(X1, Key) -> any()`

Resolve a key path in the remote store.

For the remote node store, the key is returned as-is.

<a name="scope-1"></a>

### scope/1 ###

`scope(Arg) -> any()`

Return the scope of this store.

For the remote store, the scope is always `remote`.

<a name="type-2"></a>

### type/2 ###

`type(Opts, Key) -> any()`

Determine the type of value at a given key.

Remote nodes support only the `simple` type or `not_found`.

<a name="write-3"></a>

### write/3 ###

`write(Opts, Key, Value) -> any()`

Write a key to the remote node.

Constructs an HTTP POST write request. If a wallet is provided,
the message is signed. Returns {ok, Path} on HTTP 200, or
{error, Reason} on failure.

