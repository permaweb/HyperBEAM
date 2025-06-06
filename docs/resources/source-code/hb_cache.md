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
either a map or a direct binary.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cache_suite_test_-0">cache_suite_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#calculate_all_ids-2">calculate_all_ids/2*</a></td><td>Calculate the IDs for a message.</td></tr><tr><td valign="top"><a href="#do_read-4">do_read/4*</a></td><td>Read a path from the store.</td></tr><tr><td valign="top"><a href="#do_write_message-4">do_write_message/4*</a></td><td></td></tr><tr><td valign="top"><a href="#link-3">link/3</a></td><td>Make a link from one path to another in the store.</td></tr><tr><td valign="top"><a href="#list-2">list/2</a></td><td>List all items under a given path.</td></tr><tr><td valign="top"><a href="#list_numbered-2">list_numbered/2</a></td><td>List all items in a directory, assuming they are numbered.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read the message at a path.</td></tr><tr><td valign="top"><a href="#read_resolved-3">read_resolved/3</a></td><td>Read the output of a prior computation, given Msg1, Msg2, and some
options.</td></tr><tr><td valign="top"><a href="#run_test-0">run_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#store_read-3">store_read/3*</a></td><td>List all of the subpaths of a given path, read each in turn, returning a
flat map.</td></tr><tr><td valign="top"><a href="#store_read-4">store_read/4*</a></td><td></td></tr><tr><td valign="top"><a href="#test_deeply_nested_complex_message-1">test_deeply_nested_complex_message/1*</a></td><td>Test deeply nested item storage and retrieval.</td></tr><tr><td valign="top"><a href="#test_device_map_cannot_be_written_test-0">test_device_map_cannot_be_written_test/0*</a></td><td>Test that message whose device is <code>#{}</code> cannot be written.</td></tr><tr><td valign="top"><a href="#test_message_with_message-1">test_message_with_message/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_signed-1">test_signed/1</a></td><td></td></tr><tr><td valign="top"><a href="#test_signed-2">test_signed/2*</a></td><td></td></tr><tr><td valign="top"><a href="#test_store_ans104_message-1">test_store_ans104_message/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_store_binary-1">test_store_binary/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_store_simple_signed_message-1">test_store_simple_signed_message/1*</a></td><td>Test storing and retrieving a simple unsigned item.</td></tr><tr><td valign="top"><a href="#test_store_simple_unsigned_message-1">test_store_simple_unsigned_message/1*</a></td><td>Test storing and retrieving a simple unsigned item.</td></tr><tr><td valign="top"><a href="#test_store_unsigned_empty_message-1">test_store_unsigned_empty_message/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_unsigned-1">test_unsigned/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_integer-1">to_integer/1*</a></td><td></td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td>Write a message to the cache.</td></tr><tr><td valign="top"><a href="#write_binary-3">write_binary/3</a></td><td>Write a raw binary keys into the store and link it at a given hashpath.</td></tr><tr><td valign="top"><a href="#write_binary-4">write_binary/4*</a></td><td></td></tr><tr><td valign="top"><a href="#write_hashpath-2">write_hashpath/2</a></td><td>Write a hashpath and its message to the store and link it.</td></tr><tr><td valign="top"><a href="#write_hashpath-3">write_hashpath/3*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cache_suite_test_-0"></a>

### cache_suite_test_/0 * ###

`cache_suite_test_() -> any()`

<a name="calculate_all_ids-2"></a>

### calculate_all_ids/2 * ###

`calculate_all_ids(Bin, Opts) -> any()`

Calculate the IDs for a message.

<a name="do_read-4"></a>

### do_read/4 * ###

`do_read(Path, Store, Opts, AlreadyRead) -> any()`

Read a path from the store. Unsafe: May recurse indefinitely if circular
links are present.

<a name="do_write_message-4"></a>

### do_write_message/4 * ###

`do_write_message(Bin, AllIDs, Store, Opts) -> any()`

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

<a name="read-2"></a>

### read/2 ###

`read(Path, Opts) -> any()`

Read the message at a path. Returns in `structured@1.0` format: Either a
richly typed map or a direct binary.

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

List all of the subpaths of a given path, read each in turn, returning a
flat map. We track the paths that we have already read to avoid circular
links.

<a name="store_read-4"></a>

### store_read/4 * ###

`store_read(Path, Store, Opts, AlreadyRead) -> any()`

<a name="test_deeply_nested_complex_message-1"></a>

### test_deeply_nested_complex_message/1 * ###

`test_deeply_nested_complex_message(Opts) -> any()`

Test deeply nested item storage and retrieval

<a name="test_device_map_cannot_be_written_test-0"></a>

### test_device_map_cannot_be_written_test/0 * ###

`test_device_map_cannot_be_written_test() -> any()`

Test that message whose device is `#{}` cannot be written. If it were to
be written, it would cause an infinite loop.

<a name="test_message_with_message-1"></a>

### test_message_with_message/1 * ###

`test_message_with_message(Opts) -> any()`

<a name="test_signed-1"></a>

### test_signed/1 ###

`test_signed(Data) -> any()`

<a name="test_signed-2"></a>

### test_signed/2 * ###

`test_signed(Data, Wallet) -> any()`

<a name="test_store_ans104_message-1"></a>

### test_store_ans104_message/1 * ###

`test_store_ans104_message(Opts) -> any()`

<a name="test_store_binary-1"></a>

### test_store_binary/1 * ###

`test_store_binary(Opts) -> any()`

<a name="test_store_simple_signed_message-1"></a>

### test_store_simple_signed_message/1 * ###

`test_store_simple_signed_message(Opts) -> any()`

Test storing and retrieving a simple unsigned item

<a name="test_store_simple_unsigned_message-1"></a>

### test_store_simple_unsigned_message/1 * ###

`test_store_simple_unsigned_message(Opts) -> any()`

Test storing and retrieving a simple unsigned item

<a name="test_store_unsigned_empty_message-1"></a>

### test_store_unsigned_empty_message/1 * ###

`test_store_unsigned_empty_message(Opts) -> any()`

<a name="test_unsigned-1"></a>

### test_unsigned/1 ###

`test_unsigned(Data) -> any()`

<a name="to_integer-1"></a>

### to_integer/1 * ###

`to_integer(Value) -> any()`

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

