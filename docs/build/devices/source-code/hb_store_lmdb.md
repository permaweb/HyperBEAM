# [Module hb_store_lmdb.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store_lmdb.erl)




An LMDB (Lightning Memory Database) implementation of the HyperBeam store interface.

<a name="description"></a>

## Description ##

This module provides a persistent key-value store backend using LMDB, which is a
high-performance embedded transactional database. The implementation follows a
singleton pattern where each database environment gets its own dedicated server
process to manage transactions and coordinate writes.

Key features include:

* Asynchronous writes with batched transactions for performance

* Automatic link resolution for creating symbolic references between keys

* Group support for organizing hierarchical data structures

* Prefix-based key listing for directory-like navigation

* Process-local caching of database handles for efficiency


The module implements a dual-flush strategy: writes are accumulated in memory
and flushed either after an idle timeout or when explicitly requested during
read operations that encounter cache misses.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_path-3">add_path/3</a></td><td>Add two path components together.</td></tr><tr><td valign="top"><a href="#basic_test-0">basic_test/0*</a></td><td>Test suite demonstrating basic store operations.</td></tr><tr><td valign="top"><a href="#cache_debug_test-0">cache_debug_test/0*</a></td><td>Debug test to understand cache linking behavior.</td></tr><tr><td valign="top"><a href="#cache_style_test-0">cache_style_test/0*</a></td><td>Test cache-style usage through hb_store interface.</td></tr><tr><td valign="top"><a href="#commit_manager-2">commit_manager/2*</a></td><td>Background process that enforces maximum flush intervals.</td></tr><tr><td valign="top"><a href="#create_parent_groups-3">create_parent_groups/3*</a></td><td>Helper function to recursively create parent groups.</td></tr><tr><td valign="top"><a href="#ensure_parent_groups-2">ensure_parent_groups/2*</a></td><td>Ensure all parent groups exist for a given path.</td></tr><tr><td valign="top"><a href="#ensure_transaction-1">ensure_transaction/1*</a></td><td>Ensure that the server has an active LMDB transaction for writes.</td></tr><tr><td valign="top"><a href="#exact_hb_store_test-0">exact_hb_store_test/0*</a></td><td>Test that matches the exact hb_store hierarchical test pattern.</td></tr><tr><td valign="top"><a href="#find_env-1">find_env/1*</a></td><td>Retrieve or create the LMDB environment handle for a database.</td></tr><tr><td valign="top"><a href="#find_pid-1">find_pid/1*</a></td><td>Locate an existing server process or spawn a new one if needed.</td></tr><tr><td valign="top"><a href="#fold_after-4">fold_after/4*</a></td><td>Fold over a database after a given path.</td></tr><tr><td valign="top"><a href="#fold_cursor-5">fold_cursor/5*</a></td><td></td></tr><tr><td valign="top"><a href="#group_test-0">group_test/0*</a></td><td>Group test - verifies group creation and type detection.</td></tr><tr><td valign="top"><a href="#is_link-1">is_link/1*</a></td><td>Helper function to check if a value is a link and extract the target.</td></tr><tr><td valign="top"><a href="#isolated_type_debug_test-0">isolated_type_debug_test/0*</a></td><td>Isolated test focusing on the exact cache issue.</td></tr><tr><td valign="top"><a href="#link_fragment_test-0">link_fragment_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#link_key_list_test-0">link_key_list_test/0*</a></td><td>Link key list test - verifies symbolic link creation using structured key paths.</td></tr><tr><td valign="top"><a href="#link_test-0">link_test/0*</a></td><td>Link test - verifies symbolic link creation and resolution.</td></tr><tr><td valign="top"><a href="#list-2">list/2</a></td><td>List all keys that start with a given prefix.</td></tr><tr><td valign="top"><a href="#list_test-0">list_test/0*</a></td><td>List test - verifies prefix-based key listing functionality.</td></tr><tr><td valign="top"><a href="#list_with_link_test-0">list_with_link_test/0*</a></td><td>Test that list function resolves links correctly.</td></tr><tr><td valign="top"><a href="#make_group-2">make_group/2</a></td><td>Create a group entry that can contain other keys hierarchically.</td></tr><tr><td valign="top"><a href="#make_link-3">make_link/3</a></td><td>Create a symbolic link from a new key to an existing key.</td></tr><tr><td valign="top"><a href="#nested_map_cache_test-0">nested_map_cache_test/0*</a></td><td>Test nested map storage with cache-like linking behavior.</td></tr><tr><td valign="top"><a href="#notify_flush-1">notify_flush/1*</a></td><td>Notify all processes waiting for a flush operation to complete.</td></tr><tr><td valign="top"><a href="#path-2">path/2</a></td><td>Transform a path into the store's canonical form.</td></tr><tr><td valign="top"><a href="#path_traversal_link_test-0">path_traversal_link_test/0*</a></td><td>Path traversal link test - verifies link resolution during path traversal.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read a value from the database by key, with automatic link resolution.</td></tr><tr><td valign="top"><a href="#read_direct-2">read_direct/2*</a></td><td>Read a value directly from the database with link resolution.</td></tr><tr><td valign="top"><a href="#read_with_flush-2">read_with_flush/2*</a></td><td>Read with immediate flush for cases where we need to see recent writes.</td></tr><tr><td valign="top"><a href="#read_with_retry-2">read_with_retry/2*</a></td><td>Unified read function that handles LMDB reads with retry logic.</td></tr><tr><td valign="top"><a href="#read_with_retry-3">read_with_retry/3*</a></td><td></td></tr><tr><td valign="top"><a href="#reconstruct_map-2">reconstruct_map/2*</a></td><td></td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Completely delete the database directory and all its contents.</td></tr><tr><td valign="top"><a href="#resolve-2">resolve/2</a></td><td>Resolve a path by following any symbolic links.</td></tr><tr><td valign="top"><a href="#resolve_path_links-2">resolve_path_links/2*</a></td><td>Resolve links in a path, checking each segment except the last.</td></tr><tr><td valign="top"><a href="#resolve_path_links-3">resolve_path_links/3*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_path_links_acc-4">resolve_path_links_acc/4*</a></td><td></td></tr><tr><td valign="top"><a href="#scope-0">scope/0</a></td><td>Return the scope of this storage backend.</td></tr><tr><td valign="top"><a href="#scope-1">scope/1</a></td><td>Return the scope of this storage backend (ignores parameters).</td></tr><tr><td valign="top"><a href="#server-1">server/1*</a></td><td>Main server loop that handles database operations and manages transactions.</td></tr><tr><td valign="top"><a href="#server_flush-1">server_flush/1*</a></td><td>Commit the current transaction to disk and clean up state.</td></tr><tr><td valign="top"><a href="#server_write-3">server_write/3*</a></td><td>Add a key-value pair to the current transaction, creating one if needed.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start the LMDB storage system for a given database configuration.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Gracefully shut down the database server and close the environment.</td></tr><tr><td valign="top"><a href="#sync-1">sync/1*</a></td><td>Force an immediate flush of all pending writes to disk.</td></tr><tr><td valign="top"><a href="#to_path-1">to_path/1*</a></td><td>Helper function to convert to a path.</td></tr><tr><td valign="top"><a href="#type-2">type/2</a></td><td>Determine whether a key represents a simple value or composite group.</td></tr><tr><td valign="top"><a href="#type_test-0">type_test/0*</a></td><td>Type test - verifies type detection for both simple and composite entries.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>Write a key-value pair to the database asynchronously.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_path-3"></a>

### add_path/3 ###

`add_path(Opts, Path1, Path2) -> any()`

Add two path components together.
For LMDB, this concatenates the path lists.

<a name="basic_test-0"></a>

### basic_test/0 * ###

`basic_test() -> any()`

Test suite demonstrating basic store operations.

The following functions implement unit tests using EUnit to verify that
the LMDB store implementation correctly handles various scenarios including
basic read/write operations, hierarchical listing, group creation, link
resolution, and type detection.

Basic store test - verifies fundamental read/write functionality.

This test creates a temporary database, writes a key-value pair, reads it
back to verify correctness, and cleans up by stopping the database. It
serves as a sanity check that the basic storage mechanism is working.

<a name="cache_debug_test-0"></a>

### cache_debug_test/0 * ###

`cache_debug_test() -> any()`

Debug test to understand cache linking behavior

<a name="cache_style_test-0"></a>

### cache_style_test/0 * ###

`cache_style_test() -> any()`

Test cache-style usage through hb_store interface

<a name="commit_manager-2"></a>

### commit_manager/2 * ###

`commit_manager(Opts, Server) -> any()`

Background process that enforces maximum flush intervals.

This function runs in a separate process linked to the main server and
ensures that transactions are committed within a reasonable time frame
even during periods of continuous write activity. It sends periodic
flush requests to the main server based on the configured maximum flush time.

The commit manager provides a safety net against data loss by preventing
transactions from remaining uncommitted indefinitely. It works in conjunction
with the idle timeout mechanism to provide comprehensive data safety guarantees.

The process runs in an infinite loop, coordinating with the main server
through message passing and restarting its timer after each successful flush.

<a name="create_parent_groups-3"></a>

### create_parent_groups/3 * ###

`create_parent_groups(Opts, Current, Rest) -> any()`

Helper function to recursively create parent groups.

<a name="ensure_parent_groups-2"></a>

### ensure_parent_groups/2 * ###

<pre><code>
ensure_parent_groups(Opts::map(), Path::binary()) -&gt; ok
</code></pre>
<br />

`Opts`: Database configuration map<br />`Path`: The path whose parents should exist<br />

returns: ok

Ensure all parent groups exist for a given path.

This function creates the necessary parent groups for a path, similar to
how filesystem stores use ensure_dir. For example, if the path is
"a/b/c/file", it will ensure groups "a", "a/b", and "a/b/c" exist.

<a name="ensure_transaction-1"></a>

### ensure_transaction/1 * ###

`ensure_transaction(State) -> any()`

Ensure that the server has an active LMDB transaction for writes.

This function implements lazy transaction creation by checking if a transaction
already exists in the server state. If not, it creates a new read-write
transaction and opens the default database within it.

The lazy approach improves efficiency by avoiding transaction overhead when
the server is idle, while ensuring that write operations always have a
transaction available when needed.

Transactions in LMDB are lightweight but still represent a commitment of
resources, so creating them only when needed helps optimize memory usage
and system performance.

<a name="exact_hb_store_test-0"></a>

### exact_hb_store_test/0 * ###

`exact_hb_store_test() -> any()`

Test that matches the exact hb_store hierarchical test pattern

<a name="find_env-1"></a>

### find_env/1 * ###

`find_env(Opts) -> any()`

Retrieve or create the LMDB environment handle for a database.

<a name="find_pid-1"></a>

### find_pid/1 * ###

`find_pid(StoreOpts) -> any()`

Locate an existing server process or spawn a new one if needed.

<a name="fold_after-4"></a>

### fold_after/4 * ###

`fold_after(Opts, Path, Fun, Acc) -> any()`

Fold over a database after a given path. The `Fun` is called with
the key and value, and the accumulator.

<a name="fold_cursor-5"></a>

### fold_cursor/5 * ###

`fold_cursor(X1, Txn, Cur, Fun, Acc) -> any()`

<a name="group_test-0"></a>

### group_test/0 * ###

`group_test() -> any()`

Group test - verifies group creation and type detection.

This test creates a group entry and verifies that it is correctly identified
as a composite type and cannot be read directly (like filesystem directories).

<a name="is_link-1"></a>

### is_link/1 * ###

`is_link(Value) -> any()`

Helper function to check if a value is a link and extract the target.

<a name="isolated_type_debug_test-0"></a>

### isolated_type_debug_test/0 * ###

`isolated_type_debug_test() -> any()`

Isolated test focusing on the exact cache issue

<a name="link_fragment_test-0"></a>

### link_fragment_test/0 * ###

`link_fragment_test() -> any()`

<a name="link_key_list_test-0"></a>

### link_key_list_test/0 * ###

`link_key_list_test() -> any()`

Link key list test - verifies symbolic link creation using structured key paths.

This test demonstrates the store's ability to handle complex key structures
represented as lists of binary segments, and verifies that symbolic links
work correctly when the target key is specified as a list rather than a
flat binary string.

The test creates a hierarchical key structure using a list format (which
presumably gets converted to a path-like binary internally), creates a
symbolic link pointing to that structured key, and verifies that link
resolution works transparently to return the original value.

This is particularly important for applications that organize data in
hierarchical structures where keys represent nested paths or categories,
and need to create shortcuts or aliases to deeply nested data.

<a name="link_test-0"></a>

### link_test/0 * ###

`link_test() -> any()`

Link test - verifies symbolic link creation and resolution.

This test creates a regular key-value pair, creates a link pointing to it,
and verifies that reading from the link location returns the original value.
This demonstrates the transparent link resolution mechanism.

<a name="list-2"></a>

### list/2 ###

<pre><code>
list(Opts::map(), Path::binary()) -&gt; {ok, [binary()]} | {error, term()}
</code></pre>
<br />

`Path`: Binary prefix to search for<br />

returns: {ok, [Key]} list of matching keys, {error, Reason} on failure

List all keys that start with a given prefix.

This function provides directory-like navigation by finding all keys that
begin with the specified path prefix. It uses LMDB's fold operation to
efficiently scan through the database and collect matching keys.

The implementation only returns keys that are longer than the prefix itself,
ensuring that the prefix acts like a directory separator. For example,
listing with prefix "colors" will return "colors/red" and "colors/blue"
but not "colors" itself.

If the Path points to a link, the function resolves the link and lists
the contents of the target directory instead.

This is particularly useful for implementing hierarchical data organization
and providing tree-like navigation interfaces in applications.

<a name="list_test-0"></a>

### list_test/0 * ###

`list_test() -> any()`

List test - verifies prefix-based key listing functionality.

This test creates several keys with hierarchical names and verifies that
the list operation correctly returns only keys matching a specific prefix.
It demonstrates the directory-like navigation capabilities of the store.

<a name="list_with_link_test-0"></a>

### list_with_link_test/0 * ###

`list_with_link_test() -> any()`

Test that list function resolves links correctly

<a name="make_group-2"></a>

### make_group/2 ###

<pre><code>
make_group(Opts::map(), GroupName::binary()) -&gt; ok | {error, term()}
</code></pre>
<br />

`Opts`: Database configuration map<br />`GroupName`: Binary name for the group<br />

returns: Result of the write operation

Create a group entry that can contain other keys hierarchically.

Groups in the HyperBeam system represent composite entries that can contain
child elements, similar to directories in a filesystem. This function creates
a group by storing the special value "group" at the specified key.

The group mechanism allows applications to organize data hierarchically and
provides semantic meaning that can be used by navigation and visualization
tools to present appropriate user interfaces.

Groups can be identified later using the type/2 function, which will return
'composite' for group entries versus 'simple' for regular key-value pairs.

<a name="make_link-3"></a>

### make_link/3 ###

<pre><code>
make_link(Opts::map(), Existing::binary() | list(), New::binary()) -&gt; ok
</code></pre>
<br />

`Existing`: The key that already exists and contains the target value<br />`New`: The new key that should link to the existing key<br />

returns: Result of the write operation

Create a symbolic link from a new key to an existing key.

This function implements a symbolic link mechanism by storing a special
"link:" prefixed value at the new key location. When the new key is read,
the system will automatically resolve the link and return the value from
the target key instead.

Links provide a way to create aliases, shortcuts, or alternative access
paths to the same underlying data without duplicating storage. They can
be chained together to create complex reference structures, though care
should be taken to avoid circular references.

The link resolution happens transparently during read operations, making
links invisible to most application code while providing powerful
organizational capabilities.

<a name="nested_map_cache_test-0"></a>

### nested_map_cache_test/0 * ###

`nested_map_cache_test() -> any()`

Test nested map storage with cache-like linking behavior

This test demonstrates how to store a nested map structure where:
1. Each value is stored at data/{hash_of_value}
2. Links are created to compose the values back into the original map structure
3. Reading the composed structure reconstructs the original nested map

<a name="notify_flush-1"></a>

### notify_flush/1 * ###

`notify_flush(State) -> any()`

Notify all processes waiting for a flush operation to complete.

This function handles the coordination between the server's flush operations
and client processes that may be blocked waiting for data to be committed.
It uses a non-blocking receive loop to collect all pending flush requests
and respond to them immediately.

The non-blocking nature (timeout of 0) ensures that the server doesn't get
stuck waiting for messages that may not exist, while still handling all
queued requests efficiently.

<a name="path-2"></a>

### path/2 ###

`path(Opts, PathParts) -> any()`

Transform a path into the store's canonical form.
For LMDB, paths are simply joined with "/" separators.

<a name="path_traversal_link_test-0"></a>

### path_traversal_link_test/0 * ###

`path_traversal_link_test() -> any()`

Path traversal link test - verifies link resolution during path traversal.

This test verifies that when reading a path as a list, intermediate path
segments that are links get resolved correctly. For example, if "link"
is a symbolic link to "group", then reading ["link", "key"] should
resolve to reading ["group", "key"].

This functionality enables transparent redirection at the directory level,
allowing reorganization of hierarchical data without breaking existing
access patterns.

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(Opts::map(), PathParts::binary() | list()) -&gt; {ok, binary()} | {error, term()}
</code></pre>
<br />

`Opts`: Database configuration map<br />

returns: {ok, Value} on success, {error, Reason} on failure

Read a value from the database by key, with automatic link resolution.

This function attempts to read a value directly from the committed database.
If the key is not found, it triggers a flush operation to ensure any pending
writes are committed before retrying the read.

The function automatically handles link resolution: if a stored value begins
with the "link:" prefix, it extracts the target key and recursively reads
from that location instead. This creates a symbolic link mechanism that
allows multiple keys to reference the same underlying data.

When given a list of path segments, the function first attempts a direct read
for optimal performance. Only if the direct read fails does it perform link
resolution at each level of the path except the final segment, allowing path
traversal through symbolic links to work transparently.

Link resolution is transparent to the caller and can chain through multiple
levels of indirection, though care should be taken to avoid circular references.

<a name="read_direct-2"></a>

### read_direct/2 * ###

`read_direct(Opts, Path) -> any()`

Read a value directly from the database with link resolution.
This is the internal implementation that handles actual database reads.

<a name="read_with_flush-2"></a>

### read_with_flush/2 * ###

`read_with_flush(Opts, Path) -> any()`

Read with immediate flush for cases where we need to see recent writes.
This is used when we expect the key to exist from a recent write operation.

<a name="read_with_retry-2"></a>

### read_with_retry/2 * ###

`read_with_retry(Opts, Path) -> any()`

Unified read function that handles LMDB reads with retry logic.
Returns {ok, Value}, not_found, or performs flush and retries.

<a name="read_with_retry-3"></a>

### read_with_retry/3 * ###

`read_with_retry(Opts, Path, RetriesRemaining) -> any()`

<a name="reconstruct_map-2"></a>

### reconstruct_map/2 * ###

`reconstruct_map(StoreOpts, Path) -> any()`

<a name="reset-1"></a>

### reset/1 ###

`reset(Opts) -> any()`

Completely delete the database directory and all its contents.

This is a destructive operation that removes all data from the specified
database. It first performs a graceful shutdown to ensure data consistency,
then uses the system shell to recursively delete the entire database
directory structure.

This function is primarily intended for testing and development scenarios
where you need to start with a completely clean database state. It should
be used with extreme caution in production environments.

<a name="resolve-2"></a>

### resolve/2 ###

<pre><code>
resolve(Opts::map(), Path::binary() | list()) -&gt; binary()
</code></pre>
<br />

`Path`: The path to resolve (binary or list)<br />

returns: The resolved path as a binary

Resolve a path by following any symbolic links.

For LMDB, we handle links through our own "link:" prefix mechanism.
This function resolves link chains in paths, similar to filesystem symlink resolution.
It's used by the cache to resolve paths before type checking and reading.

<a name="resolve_path_links-2"></a>

### resolve_path_links/2 * ###

`resolve_path_links(Opts, Path) -> any()`

Resolve links in a path, checking each segment except the last.
Returns the resolved path where any intermediate links have been followed.

<a name="resolve_path_links-3"></a>

### resolve_path_links/3 * ###

`resolve_path_links(Opts, Path, Depth) -> any()`

<a name="resolve_path_links_acc-4"></a>

### resolve_path_links_acc/4 * ###

`resolve_path_links_acc(Opts, Tail, AccPath, Depth) -> any()`

<a name="scope-0"></a>

### scope/0 ###

<pre><code>
scope() -&gt; local
</code></pre>
<br />

returns: 'local' always

Return the scope of this storage backend.

The LMDB implementation is always local-only and does not support distributed
operations. This function exists to satisfy the HyperBeam store interface
contract and inform the system about the storage backend's capabilities.

<a name="scope-1"></a>

### scope/1 ###

<pre><code>
scope(X1::term()) -&gt; local
</code></pre>
<br />

returns: 'local' always

Return the scope of this storage backend (ignores parameters).

This is an alternate form of scope/0 that ignores any parameters passed to it.
The LMDB backend is always local regardless of configuration.

<a name="server-1"></a>

### server/1 * ###

`server(State) -> any()`

Main server loop that handles database operations and manages transactions.

This function implements the core server logic using Erlang's selective receive
mechanism. It handles four types of messages: environment requests from readers,
write requests that accumulate in transactions, explicit flush requests that
commit pending data, and stop messages for graceful shutdown.

The server uses a timeout-based flush strategy where it automatically commits
transactions after a period of inactivity. This balances write performance
(by batching operations) with data safety (by limiting the window of potential
data loss).

The server maintains its state as a map containing the LMDB environment,
current transaction handle, and configuration parameters. State updates are
handled functionally by passing modified state maps through tail-recursive calls.

<a name="server_flush-1"></a>

### server_flush/1 * ###

`server_flush(RawState) -> any()`

Commit the current transaction to disk and clean up state.

This function handles the critical operation of persisting accumulated writes
to the database. If a transaction is active, it commits the transaction and
notifies any processes waiting for the flush to complete.

After committing, the server state is cleaned up by removing transaction
references, preparing for the next batch of operations. If no transaction
is active, the function is a no-op.

The notification mechanism ensures that read operations blocked on cache
misses can proceed once fresh data is available.

<a name="server_write-3"></a>

### server_write/3 * ###

`server_write(RawState, Key, Value) -> any()`

Add a key-value pair to the current transaction, creating one if needed.

This function handles write operations by ensuring a transaction is active
and then adding the key-value pair to it using LMDB's native interface.
If no transaction exists, it creates one automatically.

The function uses LMDB's direct NIF interface for maximum performance,
bypassing higher-level abstractions that might add overhead. The write
is added to the transaction but not committed until a flush occurs.

<a name="start-1"></a>

### start/1 ###

<pre><code>
start(Opts::map()) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

returns: {ok, ServerPid} on success, {error, Reason} on failure

Start the LMDB storage system for a given database configuration.

This function initializes or connects to an existing LMDB database instance.
It uses a singleton pattern, so multiple calls with the same configuration
will return the same server process. The server process manages the LMDB
environment and coordinates all database operations.

The StoreOpts map must contain a "prefix" key specifying the
database directory path. Also the required configuration includes "capacity" for
the maximum database size and flush timing parameters.

<a name="stop-1"></a>

### stop/1 ###

`stop(StoreOpts) -> any()`

Gracefully shut down the database server and close the environment.

This function performs an orderly shutdown of the database system by first
stopping the server process (which flushes any pending writes) and then
closing the LMDB environment to release system resources.

The shutdown process ensures that no data is lost and all file handles
are properly closed. After calling stop, the database can be restarted
by calling any other function that triggers server creation.

<a name="sync-1"></a>

### sync/1 * ###

<pre><code>
sync(Opts::map()) -&gt; ok | {error, term()}
</code></pre>
<br />

returns: 'ok' when flush is complete, {error, Reason} on failure

Force an immediate flush of all pending writes to disk.

This function synchronously forces the database server to commit any
pending writes in the current transaction. It blocks until the flush
operation is complete, ensuring that all previously written data is
durably stored before returning.

This is useful when you need to ensure data is persisted immediately,
rather than waiting for the automatic flush timers to trigger. Common
use cases include critical checkpoints, before system shutdown, or
when preparing for read operations that must see the latest writes.

<a name="to_path-1"></a>

### to_path/1 * ###

`to_path(PathParts) -> any()`

Helper function to convert to a path

<a name="type-2"></a>

### type/2 ###

<pre><code>
type(Opts::map(), Key::binary()) -&gt; composite | simple | not_found
</code></pre>
<br />

`Opts`: Database configuration map<br />`Key`: The key to examine<br />

returns: 'composite' for group entries, 'simple' for regular values

Determine whether a key represents a simple value or composite group.

This function reads the value associated with a key and examines its content
to classify the entry type. Keys storing the literal binary "group" are
considered composite (directory-like) entries, while all other values are
treated as simple key-value pairs.

This classification is used by higher-level HyperBeam components to understand
the structure of stored data and provide appropriate navigation interfaces.

<a name="type_test-0"></a>

### type_test/0 * ###

`type_test() -> any()`

Type test - verifies type detection for both simple and composite entries.

This test creates both a group (composite) entry and a regular (simple) entry,
then verifies that the type detection function correctly identifies each one.
This demonstrates the semantic classification system used by the store.

<a name="write-3"></a>

### write/3 ###

<pre><code>
write(Opts::map(), PathParts::binary() | list(), Value::binary()) -&gt; ok
</code></pre>
<br />

`Opts`: Database configuration map<br />`Value`: Binary value to store<br />

returns: 'ok' immediately (write happens asynchronously)

Write a key-value pair to the database asynchronously.

This function sends a write request to the database server process and returns
immediately without waiting for the write to be committed to disk. The server
accumulates writes in a transaction that is periodically flushed based on
timing constraints or explicit flush requests.

The asynchronous nature provides better performance for write-heavy workloads
while the batching strategy ensures data consistency and reduces I/O overhead.
However, recent writes may not be immediately visible to readers until the
next flush occurs.

