# [Module hb_cache.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_cache.erl)




A cache of AO-Core protocol messages and compute results.

<a name="description"></a>

## Description ##

HyperBEAM stores all paths in key value stores, abstracted by the `hb_store`
module. Each store has its own storage backend, but each works with simple
key-value pairs. Each store can write binary keys at paths, and link between
paths.

There are three layers to HyperBEAMs internal data representation on-disk:

1. The raw binary data, written to the store at the hash of the content.
Storing binary paths in this way effectively deduplicates the data.
2. The hashpath-graph of all content, stored as a set of links between
hashpaths, their keys, and the data that underlies them. This allows
all messages to share the same hashpath space, such that all requests
from users additively fill-in the hashpath space, minimizing duplicated
compute.
3. Messages, referrable by their IDs (committed or uncommitted). These are
stored as a set of links commitment IDs and the uncommitted message.

Before writing a message to the store, we convert it to Type-Annotated
Binary Messages (TABMs), such that each of the keys in the message is
either a map or a direct binary.

Nested keys are lazily loaded from the stores, such that large deeply
nested messages where only a small part of the data is actually used are
not loaded into memory unnecessarily. In order to ensure that a message is
loaded from the cache after a `read`, we can use the `ensure_loaded/1` and
`ensure_all_loaded/1` functions. Ensure loaded will load the exact value
that has been requested, while ensure all loaded will load the entire
structure of the message into memory.

Lazily loadable `links` are expressed as a tuple of the following form:
`{link, ID, LinkOpts}`, where `ID` is the path to the data in the store,
and `LinkOpts` is a map of suggested options to use when loading the data.
In particular, this module ensures to stash the `store` option in `LinkOpts`,
such that the `read` function can use the correct store without having to
search unnecessarily. By providing an `Opts` argument to `ensure_loaded` or
`ensure_all_loaded`, the caller can specify additional options to use when
loading the data -- overriding the suggested options in the link.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cache_suite_test_-0">cache_suite_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#calculate_all_ids-2">calculate_all_ids/2*</a></td><td>Calculate the IDs for a message.</td></tr><tr><td valign="top"><a href="#commitment_path-2">commitment_path/2*</a></td><td>Generate the commitment path for a given base path.</td></tr><tr><td valign="top"><a href="#do_write_message-3">do_write_message/3*</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_all_loaded-1">ensure_all_loaded/1</a></td><td>Ensure that all of the components of a message (whether a map, list,
or immediate value) are recursively fully loaded from the stores into memory.</td></tr><tr><td valign="top"><a href="#ensure_all_loaded-2">ensure_all_loaded/2</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_loaded-1">ensure_loaded/1</a></td><td>Ensure that a value is loaded from the cache if it is an ID or a link.</td></tr><tr><td valign="top"><a href="#ensure_loaded-2">ensure_loaded/2</a></td><td></td></tr><tr><td valign="top"><a href="#link-3">link/3</a></td><td>Make a link from one path to another in the store.</td></tr><tr><td valign="top"><a href="#list-2">list/2</a></td><td>List all items under a given path.</td></tr><tr><td valign="top"><a href="#list_numbered-2">list_numbered/2</a></td><td>List all items in a directory, assuming they are numbered.</td></tr><tr><td valign="top"><a href="#prepare_commitments-2">prepare_commitments/2*</a></td><td>The <code>structured@1.0</code> encoder does not typically encode <code>commitments</code>,
subsequently, when we encounter a commitments message we prepare its contents
separately, then write each to the store.</td></tr><tr><td valign="top"><a href="#prepare_links-4">prepare_links/4*</a></td><td>Prepare a set of links from a listing of subpaths.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read the message at a path.</td></tr><tr><td valign="top"><a href="#read_ao_types-4">read_ao_types/4*</a></td><td>Read and parse the ao-types for a given path if it is in the supplied
list of subpaths, returning a map of keys and their types.</td></tr><tr><td valign="top"><a href="#read_resolved-3">read_resolved/3</a></td><td>Read the output of a prior computation, given Msg1, Msg2, and some
options.</td></tr><tr><td valign="top"><a href="#run_test-0">run_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#store_read-3">store_read/3*</a></td><td>List all of the subpaths of a given path and return a map of keys and
links to the subpaths, including their types.</td></tr><tr><td valign="top"><a href="#test_deeply_nested_complex_message-1">test_deeply_nested_complex_message/1*</a></td><td>Test deeply nested item storage and retrieval.</td></tr><tr><td valign="top"><a href="#test_device_map_cannot_be_written_test-0">test_device_map_cannot_be_written_test/0*</a></td><td>Test that message whose device is <code>#{}</code> cannot be written.</td></tr><tr><td valign="top"><a href="#test_message_with_list-1">test_message_with_list/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_signed-1">test_signed/1</a></td><td></td></tr><tr><td valign="top"><a href="#test_signed-2">test_signed/2*</a></td><td></td></tr><tr><td valign="top"><a href="#test_store_ans104_message-1">test_store_ans104_message/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_store_binary-1">test_store_binary/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_store_simple_signed_message-1">test_store_simple_signed_message/1*</a></td><td>Test storing and retrieving a simple unsigned item.</td></tr><tr><td valign="top"><a href="#test_store_simple_unsigned_message-1">test_store_simple_unsigned_message/1*</a></td><td>Test storing and retrieving a simple unsigned item.</td></tr><tr><td valign="top"><a href="#test_store_unsigned_empty_message-1">test_store_unsigned_empty_message/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_store_unsigned_nested_empty_message-1">test_store_unsigned_nested_empty_message/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_unsigned-1">test_unsigned/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_integer-1">to_integer/1*</a></td><td></td></tr><tr><td valign="top"><a href="#types_to_implicit-1">types_to_implicit/1*</a></td><td>Convert a map of ao-types to an implicit map of types.</td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td>Write a message to the cache.</td></tr><tr><td valign="top"><a href="#write_binary-3">write_binary/3</a></td><td>Write a raw binary keys into the store and link it at a given hashpath.</td></tr><tr><td valign="top"><a href="#write_binary-4">write_binary/4*</a></td><td></td></tr><tr><td valign="top"><a href="#write_hashpath-2">write_hashpath/2</a></td><td>Write a hashpath and its message to the store and link it.</td></tr><tr><td valign="top"><a href="#write_hashpath-3">write_hashpath/3*</a></td><td></td></tr><tr><td valign="top"><a href="#write_key-6">write_key/6*</a></td><td>Write a single key for a message into the store.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cache_suite_test_-0"></a>

### cache_suite_test_/0 * ###

`cache_suite_test_() -> any()`

<a name="calculate_all_ids-2"></a>

### calculate_all_ids/2 * ###

`calculate_all_ids(Bin, Opts) -> any()`

Calculate the IDs for a message.

<a name="commitment_path-2"></a>

### commitment_path/2 * ###

`commitment_path(Base, Opts) -> any()`

Generate the commitment path for a given base path.

<a name="do_write_message-3"></a>

### do_write_message/3 * ###

`do_write_message(Bin, Store, Opts) -> any()`

<a name="ensure_all_loaded-1"></a>

### ensure_all_loaded/1 ###

`ensure_all_loaded(Msg) -> any()`

Ensure that all of the components of a message (whether a map, list,
or immediate value) are recursively fully loaded from the stores into memory.
This is a catch-all function that is useful in situations where ensuring a
message contains no links is important, but it carries potentially extreme
performance costs.

<a name="ensure_all_loaded-2"></a>

### ensure_all_loaded/2 ###

`ensure_all_loaded(Link, Opts) -> any()`

<a name="ensure_loaded-1"></a>

### ensure_loaded/1 ###

`ensure_loaded(Msg) -> any()`

Ensure that a value is loaded from the cache if it is an ID or a link.
If it is not loadable we raise an error. If the value is a message, we will
load only the first `layer` of it: Representing all nested messages inside
the result as links. If the value has an associated `type` key in the extra
options, we apply it to the read value, 'lazily' recreating a `structured@1.0`
form.

<a name="ensure_loaded-2"></a>

### ensure_loaded/2 ###

`ensure_loaded(Lk, RawOpts) -> any()`

<a name="link-3"></a>

### link/3 ###

`link(Existing, New, Opts) -> any()`

Make a link from one path to another in the store.
Note: Argument order is `link(Src, Dst, Opts)`.

<a name="list-2"></a>

### list/2 ###

`list(Path, Opts) -> any()`

List all items under a given path.

<a name="list_numbered-2"></a>

### list_numbered/2 ###

`list_numbered(Path, Opts) -> any()`

List all items in a directory, assuming they are numbered.

<a name="prepare_commitments-2"></a>

### prepare_commitments/2 * ###

`prepare_commitments(RawCommitments, Opts) -> any()`

The `structured@1.0` encoder does not typically encode `commitments`,
subsequently, when we encounter a commitments message we prepare its contents
separately, then write each to the store.

<a name="prepare_links-4"></a>

### prepare_links/4 * ###

`prepare_links(RootPath, Subpaths, Store, Opts) -> any()`

Prepare a set of links from a listing of subpaths.

<a name="read-2"></a>

### read/2 ###

`read(Path, Opts) -> any()`

Read the message at a path. Returns in `structured@1.0` format: Either a
richly typed map or a direct binary.

<a name="read_ao_types-4"></a>

### read_ao_types/4 * ###

`read_ao_types(Path, Subpaths, Store, Opts) -> any()`

Read and parse the ao-types for a given path if it is in the supplied
list of subpaths, returning a map of keys and their types.

<a name="read_resolved-3"></a>

### read_resolved/3 ###

`read_resolved(MsgID1, MsgID2, Opts) -> any()`

Read the output of a prior computation, given Msg1, Msg2, and some
options.

<a name="run_test-0"></a>

### run_test/0 * ###

`run_test() -> any()`

<a name="store_read-3"></a>

### store_read/3 * ###

`store_read(Path, Store, Opts) -> any()`

List all of the subpaths of a given path and return a map of keys and
links to the subpaths, including their types.

<a name="test_deeply_nested_complex_message-1"></a>

### test_deeply_nested_complex_message/1 * ###

`test_deeply_nested_complex_message(Store) -> any()`

Test deeply nested item storage and retrieval

<a name="test_device_map_cannot_be_written_test-0"></a>

### test_device_map_cannot_be_written_test/0 * ###

`test_device_map_cannot_be_written_test() -> any()`

Test that message whose device is `#{}` cannot be written. If it were to
be written, it would cause an infinite loop.

<a name="test_message_with_list-1"></a>

### test_message_with_list/1 * ###

`test_message_with_list(Store) -> any()`

<a name="test_signed-1"></a>

### test_signed/1 ###

`test_signed(Data) -> any()`

<a name="test_signed-2"></a>

### test_signed/2 * ###

`test_signed(Data, Wallet) -> any()`

<a name="test_store_ans104_message-1"></a>

### test_store_ans104_message/1 * ###

`test_store_ans104_message(Store) -> any()`

<a name="test_store_binary-1"></a>

### test_store_binary/1 * ###

`test_store_binary(Store) -> any()`

<a name="test_store_simple_signed_message-1"></a>

### test_store_simple_signed_message/1 * ###

`test_store_simple_signed_message(Store) -> any()`

Test storing and retrieving a simple unsigned item

<a name="test_store_simple_unsigned_message-1"></a>

### test_store_simple_unsigned_message/1 * ###

`test_store_simple_unsigned_message(Store) -> any()`

Test storing and retrieving a simple unsigned item

<a name="test_store_unsigned_empty_message-1"></a>

### test_store_unsigned_empty_message/1 * ###

`test_store_unsigned_empty_message(Store) -> any()`

<a name="test_store_unsigned_nested_empty_message-1"></a>

### test_store_unsigned_nested_empty_message/1 * ###

`test_store_unsigned_nested_empty_message(Store) -> any()`

<a name="test_unsigned-1"></a>

### test_unsigned/1 ###

`test_unsigned(Data) -> any()`

<a name="to_integer-1"></a>

### to_integer/1 * ###

`to_integer(Value) -> any()`

<a name="types_to_implicit-1"></a>

### types_to_implicit/1 * ###

`types_to_implicit(Types) -> any()`

Convert a map of ao-types to an implicit map of types.

<a name="write-2"></a>

### write/2 ###

`write(RawMsg, Opts) -> any()`

Write a message to the cache. For raw binaries, we write the data at
the hashpath of the data (by default the SHA2-256 hash of the data). We link
the unattended ID's hashpath for the keys (including `/commitments`) on the
message to the underlying data and recurse. We then link each commitment ID
to the uncommitted message, such that any of the committed or uncommitted IDs
can be read, and once in memory all of the commitments are available. For
deep messages, the commitments will also be read, such that the ID of the
outer message (which does not include its commitments) will be built upon
the commitments of the inner messages. We do not, however, store the IDs from
commitments on signed _inner_ messages. We may wish to revisit this.

<a name="write_binary-3"></a>

### write_binary/3 ###

`write_binary(Hashpath, Bin, Opts) -> any()`

Write a raw binary keys into the store and link it at a given hashpath.

<a name="write_binary-4"></a>

### write_binary/4 * ###

`write_binary(Hashpath, Bin, Store, Opts) -> any()`

<a name="write_hashpath-2"></a>

### write_hashpath/2 ###

`write_hashpath(Msg, Opts) -> any()`

Write a hashpath and its message to the store and link it.

<a name="write_hashpath-3"></a>

### write_hashpath/3 * ###

`write_hashpath(HP, Msg, Opts) -> any()`

<a name="write_key-6"></a>

### write_key/6 * ###

`write_key(Base, Key, HPAlg, RawCommitments, Store, Opts) -> any()`

Write a single key for a message into the store.

