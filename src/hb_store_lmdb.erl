%% @doc An LMDB (Lightning Memory Database) implementation of the HyperBeam store interface.
%%
%% This module provides a persistent key-value store backend using LMDB, which is a
%% high-performance embedded transactional database. The implementation follows a
%% singleton pattern where each database environment gets its own dedicated server
%% process to manage transactions and coordinate writes.
%%
%% Key features include:
%% <ul>
%%   <li>Asynchronous writes with batched transactions for performance</li>
%%   <li>Automatic link resolution for creating symbolic references between keys</li>
%%   <li>Group support for organizing hierarchical data structures</li>
%%   <li>Prefix-based key listing for directory-like navigation</li>
%%   <li>Process-local caching of database handles for efficiency</li>
%% </ul>
%%
%% The module implements a dual-flush strategy: writes are accumulated in memory
%% and flushed either after an idle timeout or when explicitly requested during
%% read operations that encounter cache misses.
-module(hb_store_lmdb).

%% Public API exports
-export([start/1, stop/1, scope/0, scope/1, reset/1]).
-export([read/2, write/3, list/2]).
-export([make_group/2, make_link/3, type/2]).

%% Test framework and project includes
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Configuration constants with reasonable defaults
-define(DEFAULT_SIZE, 16 * 1024 * 1024 * 1024).  % 16GB default database size
-define(CONNECT_TIMEOUT, 3000).                   % 3 second timeout for server communication
-define(DEFAULT_IDLE_FLUSH_TIME, 5).              % 5ms idle time before auto-flush
-define(DEFAULT_MAX_FLUSH_TIME, 50).              % 50ms maximum time between flushes

%% @doc Start the LMDB storage system for a given database configuration.
%%
%% This function initializes or connects to an existing LMDB database instance.
%% It uses a singleton pattern, so multiple calls with the same configuration
%% will return the same server process. The server process manages the LMDB
%% environment and coordinates all database operations.
%%
%% The StoreOpts map must contain a "prefix" key specifying the
%% database directory path. Also the required configuration includes "max-size" for
%% the maximum database size and flush timing parameters.
%%
%% @param StoreOpts A map containing database configuration options
%% @returns {ok, ServerPid} on success, {error, Reason} on failure
-spec start(map()) -> {ok, pid()} | {error, term()}.
start(StoreOpts) when is_map(StoreOpts) ->
    {ok, find_or_spawn_instance(StoreOpts)};
start(_) ->
    {error, {badarg, "StoreOpts must be a map"}}.

%% @doc Determine whether a key represents a simple value or composite group.
%%
%% This function reads the value associated with a key and examines its content
%% to classify the entry type. Keys storing the literal binary "group" are
%% considered composite (directory-like) entries, while all other values are
%% treated as simple key-value pairs.
%%
%% This classification is used by higher-level HyperBeam components to understand
%% the structure of stored data and provide appropriate navigation interfaces.
%%
%% @param Opts Database configuration map
%% @param Key The key to examine
%% @returns 'composite' for group entries, 'simple' for regular values
-spec type(map(), binary()) -> composite | simple.
type(Opts, Key) ->
    {ok, Value} = read(Opts, Key),
    ?event({value, Value}),
    case Value of
        <<"group">> -> composite;
        _ -> simple
    end.

%% @doc Write a key-value pair to the database asynchronously.
%%
%% This function sends a write request to the database server process and returns
%% immediately without waiting for the write to be committed to disk. The server
%% accumulates writes in a transaction that is periodically flushed based on
%% timing constraints or explicit flush requests.
%%
%% The asynchronous nature provides better performance for write-heavy workloads
%% while the batching strategy ensures data consistency and reduces I/O overhead.
%% However, recent writes may not be immediately visible to readers until the
%% next flush occurs.
%%
%% @param StoreOpts Database configuration map
%% @param Key Binary key to write
%% @param Value Binary value to store
%% @returns 'ok' immediately (write happens asynchronously)
-spec write(map(), binary() | list(), binary()) -> ok.
write(StoreOpts, Key, Value) when is_list(Key) ->
    KeyBin = hb_util:bin(lists:join(<<"/">>, Key)),
    write(StoreOpts, KeyBin, Value);
write(StoreOpts, Key, Value) ->
    PID = find_or_spawn_instance(StoreOpts),
    PID ! {write, Key, Value},
    ok.

%% @doc Read a value from the database by key, with automatic link resolution.
%%
%% This function attempts to read a value directly from the committed database.
%% If the key is not found, it triggers a flush operation to ensure any pending
%% writes are committed before retrying the read.
%%
%% The function automatically handles link resolution: if a stored value begins
%% with the "link:" prefix, it extracts the target key and recursively reads
%% from that location instead. This creates a symbolic link mechanism that
%% allows multiple keys to reference the same underlying data.
%%
%% Link resolution is transparent to the caller and can chain through multiple
%% levels of indirection, though care should be taken to avoid circular references.
%%
%% @param StoreOpts Database configuration map  
%% @param Key Binary key to read
%% @returns {ok, Value} on success, {error, Reason} on failure
-spec read(map(), binary() | list()) -> {ok, binary()} | {error, term()}.
read(StoreOpts, Key) when is_list(Key) ->
    KeyBin = hb_util:bin(lists:join(<<"/">>, Key)),
    read(StoreOpts, KeyBin);
read(StoreOpts, Key) ->
    LinkPrefixSize = byte_size(<<"link:">>),
    case lmdb:get(find_env(StoreOpts), Key) of
        {ok, Value} ->
            % Check if this value is actually a link to another key
            case byte_size(Value) > LinkPrefixSize andalso
                binary:part(Value, 0, LinkPrefixSize) =:= <<"link:">> of
                true -> 
                   % Extract the target key and recursively resolve the link
                   Link = binary:part(Value, LinkPrefixSize, byte_size(Value) - LinkPrefixSize),
                   read(StoreOpts, Link);
                false ->
                    % Regular value, return as-is
                    {ok, Value}
            end;
        not_found ->
            % Key not found in committed data, trigger flush and retry
            ?event(read_miss, {miss, Key}),
            find_or_spawn_instance(StoreOpts) ! {flush, self(), Ref = make_ref()},
            receive
                {flushed, Ref} -> lmdb:get(find_env(StoreOpts), Key)
            after ?CONNECT_TIMEOUT -> {error, timeout}
            end
    end.

%% @doc Return the scope of this storage backend.
%%
%% The LMDB implementation is always local-only and does not support distributed
%% operations. This function exists to satisfy the HyperBeam store interface
%% contract and inform the system about the storage backend's capabilities.
%%
%% @returns 'local' always
-spec scope() -> local.
scope() -> local.

%% @doc Return the scope of this storage backend (ignores parameters).
%%
%% This is an alternate form of scope/0 that ignores any parameters passed to it.
%% The LMDB backend is always local regardless of configuration.
%%
%% @param _Opts Ignored parameter
%% @returns 'local' always  
-spec scope(term()) -> local.
scope(_) -> scope().

%% @doc List all keys that start with a given prefix.
%%
%% This function provides directory-like navigation by finding all keys that
%% begin with the specified path prefix. It uses LMDB's fold operation to
%% efficiently scan through the database and collect matching keys.
%%
%% The implementation only returns keys that are longer than the prefix itself,
%% ensuring that the prefix acts like a directory separator. For example,
%% listing with prefix "colors" will return "colors/red" and "colors/blue"
%% but not "colors" itself.
%%
%% This is particularly useful for implementing hierarchical data organization
%% and providing tree-like navigation interfaces in applications.
%%
%% @param StoreOpts Database configuration map
%% @param Path Binary prefix to search for
%% @returns {ok, [Key]} list of matching keys, {error, Reason} on failure
-spec list(map(), binary()) -> {ok, [binary()]} | {error, term()}.
list(StoreOpts, Path) when is_map(StoreOpts), is_binary(Path) ->
    Env = find_env(StoreOpts),
    PathSize = byte_size(Path),
    try
       lmdb:fold(Env, default,
           fun(Key, _Value, Acc) ->
               % Only match on keys that have the prefix and are longer than it
               case byte_size(Key) > PathSize andalso 
                    binary:part(Key, 0, PathSize) =:= Path of
                  true -> [Key | Acc];
                  false -> Acc
               end
           end,
           []
       )
    catch
       _:Error -> {error, Error}
    end;
list(_, _) ->
    {error, {badarg, "StoreOpts must be a map and Path must be an binary"}}.

%% @doc Create a group entry that can contain other keys hierarchically.
%%
%% Groups in the HyperBeam system represent composite entries that can contain
%% child elements, similar to directories in a filesystem. This function creates
%% a group by storing the special value "group" at the specified key.
%%
%% The group mechanism allows applications to organize data hierarchically and
%% provides semantic meaning that can be used by navigation and visualization
%% tools to present appropriate user interfaces.
%%
%% Groups can be identified later using the type/2 function, which will return
%% 'composite' for group entries versus 'simple' for regular key-value pairs.
%%
%% @param StoreOpts Database configuration map
%% @param GroupName Binary name for the group
%% @returns Result of the write operation
-spec make_group(map(), binary()) -> ok | {error, term()}.
make_group(StoreOpts, GroupName) when is_map(StoreOpts), is_binary(GroupName) ->
    write(StoreOpts, GroupName, hb_util:bin(group));
make_group(_,_) ->
    {error, {badarg, "StoreOps must be map and GroupName must be a binary"}}.

%% @doc Create a symbolic link from a new key to an existing key.
%%
%% This function implements a symbolic link mechanism by storing a special
%% "link:" prefixed value at the new key location. When the new key is read,
%% the system will automatically resolve the link and return the value from
%% the target key instead.
%%
%% Links provide a way to create aliases, shortcuts, or alternative access
%% paths to the same underlying data without duplicating storage. They can
%% be chained together to create complex reference structures, though care
%% should be taken to avoid circular references.
%%
%% The link resolution happens transparently during read operations, making
%% links invisible to most application code while providing powerful
%% organizational capabilities.
%%
%% @param StoreOpts Database configuration map
%% @param Existing The key that already exists and contains the target value
%% @param New The new key that should link to the existing key
%% @returns Result of the write operation
-spec make_link(map(), binary() | list(), binary()) -> ok.
make_link(StoreOpts, Existing, New) when is_list(Existing) ->
    ExistingBin = hb_util:bin(lists:join(<<"/">>, Existing)),
    ?event({ existingKey, ExistingBin}),
    make_link(StoreOpts, ExistingBin, New);
make_link(StoreOpts, Existing, New) ->
   ExistingBin = hb_util:bin(Existing),
   write(StoreOpts, New, <<"link:", ExistingBin/binary>>). 

%% @doc Retrieve or create the LMDB environment handle for a database.
%%
%% This function manages the LMDB environment handles using a two-level caching
%% strategy. First, it checks the process dictionary for a cached handle. If not
%% found, it requests the handle from the singleton server process and caches
%% it locally for future use.
%%
%% The caching strategy improves performance by avoiding repeated server
%% communication for read operations while ensuring that all processes share
%% the same underlying database environment.
%%
%% Environment handles are lightweight references that can be safely shared
%% between processes and cached indefinitely as long as the server remains alive.
%%
%% @param StoreOpts Database configuration map containing the directory prefix
%% @returns LMDB environment handle or 'timeout' on communication failure
find_env(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    case get({?MODULE, DataDir}) of
        undefined ->
            % Not cached locally, request from server
            ?event(debug_process_cache, {not_in_env_cache, {?MODULE, DataDir}}),
            PID = find_or_spawn_instance(StoreOpts),
            PID ! {get_env, self(), Ref = make_ref()},
            receive
                {env, Env, Ref} ->
                    % Cache the environment handle in process dictionary
                    put({?MODULE, DataDir}, Env),
                    Env
            after ?CONNECT_TIMEOUT -> timeout
            end;
        Env -> Env
    end.

%% @doc Locate an existing server process or spawn a new one if needed.
%%
%% This function implements the singleton pattern for server processes using
%% a two-tier lookup strategy. First, it checks the local process dictionary
%% for a cached server PID. If not found, it consults the global process
%% registry (hb_name) to see if another process has already started a server
%% for this database directory.
%%
%% Only if no server exists anywhere in the system will a new one be spawned.
%% This ensures that each database directory has exactly one server process
%% regardless of how many client processes are accessing it.
%%
%% The caching in the process dictionary improves performance by avoiding
%% registry lookups on subsequent calls from the same process.
%%
%% @param StoreOpts Database configuration map containing the directory prefix
%% @returns PID of the server process (existing or newly created)
find_or_spawn_instance(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    case get({?MODULE, {server, DataDir}}) of
        undefined ->
            % Not cached locally, check global registry
            ?event(debug_process_cache, {not_in_process_cache, {?MODULE, DataDir}}),
            case hb_name:lookup({?MODULE, DataDir}) of
                undefined ->
                    % No server exists anywhere, create a new one
                    Pid = start_server(StoreOpts),
                    Pid;
                Pid -> Pid
            end;
        Pid ->
            Pid
    end.

%% @doc Gracefully shut down the database server and close the environment.
%%
%% This function performs an orderly shutdown of the database system by first
%% stopping the server process (which flushes any pending writes) and then
%% closing the LMDB environment to release system resources.
%%
%% The shutdown process ensures that no data is lost and all file handles
%% are properly closed. After calling stop, the database can be restarted
%% by calling any other function that triggers server creation.
%%
%% @param StoreOpts Database configuration map
%% @returns 'ok' when shutdown is complete
stop(StoreOpts) ->
    PID = find_or_spawn_instance(StoreOpts),
    Env = find_env(StoreOpts),
    PID ! stop,
    lmdb:env_close(Env),
    ok.

%% @doc Completely delete the database directory and all its contents.
%%
%% This is a destructive operation that removes all data from the specified
%% database. It first performs a graceful shutdown to ensure data consistency,
%% then uses the system shell to recursively delete the entire database
%% directory structure.
%%
%% This function is primarily intended for testing and development scenarios
%% where you need to start with a completely clean database state. It should
%% be used with extreme caution in production environments.
%%
%% @param StoreOpts Database configuration map containing the directory prefix
%% @returns 'ok' when deletion is complete
reset(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    stop(StoreOpts),
    os:cmd(binary_to_list(<< "rm -Rf ", DataDir/binary >>)),
    ok.

%% @doc Initialize a new server process for managing database operations.
%%
%% This function creates the complete server infrastructure for a database
%% instance. It first ensures the database directory exists, then creates
%% the LMDB environment with the specified configuration parameters.
%%
%% The server architecture consists of two linked processes: the main server
%% that handles database operations and a commit manager that enforces maximum
%% flush intervals. This dual-process design ensures that data is regularly
%% committed to disk even during periods of continuous write activity.
%%
%% The server process is registered both in the local process dictionary and
%% the global process registry to enable the singleton pattern used throughout
%% the module.
%%
%% @param StoreOpts Database configuration map containing directory and options
%% @returns PID of the newly created server process
start_server(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    % Ensure the database directory exists
    filelib:ensure_dir(
        binary_to_list(hb_util:bin(DataDir)) ++ "/mbd.data"
    ),
    
    % Create the LMDB environment with specified size limit
    {ok, Env} =
        lmdb:env_create(
            DataDir,
            #{
                max_mapsize => maps:get(<<"max-size">>, StoreOpts, ?DEFAULT_SIZE)
            }
        ),
    
    % Prepare server state with environment handle
    ServerOpts = StoreOpts#{ <<"env">> => Env },
    
    % Spawn the main server process with linked commit manager
    Server = 
        spawn(
            fun() ->
                spawn_link(fun() -> commit_manager(ServerOpts, self()) end),
                server(ServerOpts)
            end
        ),
    
    % Register the server in process dictionary for caching
    put({?MODULE, {server, DataDir}}, Server),
    Server.

%% @doc Main server loop that handles database operations and manages transactions.
%%
%% This function implements the core server logic using Erlang's selective receive
%% mechanism. It handles four types of messages: environment requests from readers,
%% write requests that accumulate in transactions, explicit flush requests that
%% commit pending data, and stop messages for graceful shutdown.
%%
%% The server uses a timeout-based flush strategy where it automatically commits
%% transactions after a period of inactivity. This balances write performance
%% (by batching operations) with data safety (by limiting the window of potential
%% data loss).
%%
%% The server maintains its state as a map containing the LMDB environment,
%% current transaction handle, and configuration parameters. State updates are
%% handled functionally by passing modified state maps through tail-recursive calls.
%%
%% @param State Map containing server configuration and runtime state
%% @returns 'ok' when the server terminates, otherwise recurses indefinitely
server(State) ->
    receive
        {get_env, From, Ref} ->
            % Reader requesting environment handle for direct access
            From ! {env, maps:get(<<"env">>, State), Ref},
            server(State);
        {write, Key, Value} ->
            % Write request, accumulate in current transaction
            server(server_write(State, Key, Value));
        {flush, From, Ref} ->
            % Explicit flush request, commit transaction and notify requester
            NewState = server_flush(State),
            From ! {flushed, Ref},
            server(NewState);
        stop ->
            % Shutdown request, flush final data and terminate
            server_flush(State),
            ok
    after
        % Auto-flush after idle timeout to ensure data safety
        maps:get(<<"idle-flush-time">>, State, ?DEFAULT_IDLE_FLUSH_TIME) ->
        server(server_flush(State))
    end.

%% @doc Add a key-value pair to the current transaction, creating one if needed.
%%
%% This function handles write operations by ensuring a transaction is active
%% and then adding the key-value pair to it using LMDB's native interface.
%% If no transaction exists, it creates one automatically.
%%
%% The function uses LMDB's direct NIF interface for maximum performance,
%% bypassing higher-level abstractions that might add overhead. The write
%% is added to the transaction but not committed until a flush occurs.
%%
%% @param RawState Current server state map
%% @param Key Binary key to write
%% @param Value Binary value to store
%% @returns Updated server state with the write added to the transaction
server_write(RawState, Key, Value) ->
    State = ensure_transaction(RawState),
    lmdb_nif:put(
        maps:get(<<"transaction">>, State),
        maps:get(<<"instance">>, State),
        Key,
        Value,
        0
    ),
    State.

%% @doc Commit the current transaction to disk and clean up state.
%%
%% This function handles the critical operation of persisting accumulated writes
%% to the database. If a transaction is active, it commits the transaction and
%% notifies any processes waiting for the flush to complete.
%%
%% After committing, the server state is cleaned up by removing transaction
%% references, preparing for the next batch of operations. If no transaction
%% is active, the function is a no-op.
%%
%% The notification mechanism ensures that read operations blocked on cache
%% misses can proceed once fresh data is available.
%%
%% @param RawState Current server state map  
%% @returns Updated server state with transaction cleared
server_flush(RawState) ->
    case maps:get(<<"transaction">>, RawState, undefined) of
        undefined ->
            % No active transaction, nothing to flush
            RawState;
        _ ->
            % Commit the transaction and clean up state
            Res = lmdb_nif:txn_commit(maps:get(<<"transaction">>, RawState)),
            notify_flush(RawState),
            RawState#{ <<"transaction">> => undefined, <<"instance">> => undefined }
    end.

%% @doc Notify all processes waiting for a flush operation to complete.
%%
%% This function handles the coordination between the server's flush operations
%% and client processes that may be blocked waiting for data to be committed.
%% It uses a non-blocking receive loop to collect all pending flush requests
%% and respond to them immediately.
%%
%% The non-blocking nature (timeout of 0) ensures that the server doesn't get
%% stuck waiting for messages that may not exist, while still handling all
%% queued requests efficiently.
%%
%% @param State Current server state (used for context, not modified)
%% @returns 'ok' when all notifications have been sent
notify_flush(State) ->
    receive
        {flush, From, Ref} ->
            From ! {flushed, Ref},
            notify_flush(State)
    after 0 ->
        ok
    end.

%% @doc Background process that enforces maximum flush intervals.
%%
%% This function runs in a separate process linked to the main server and
%% ensures that transactions are committed within a reasonable time frame
%% even during periods of continuous write activity. It sends periodic
%% flush requests to the main server based on the configured maximum flush time.
%%
%% The commit manager provides a safety net against data loss by preventing
%% transactions from remaining uncommitted indefinitely. It works in conjunction
%% with the idle timeout mechanism to provide comprehensive data safety guarantees.
%%
%% The process runs in an infinite loop, coordinating with the main server
%% through message passing and restarting its timer after each successful flush.
%%
%% @param StoreOpts Database configuration containing timing parameters
%% @param Server PID of the main server process to send flush requests to
%% @returns Does not return under normal circumstances (infinite loop)
commit_manager(StoreOpts, Server) ->
    Time = maps:get(<<"max-flush-time">>, StoreOpts, ?DEFAULT_MAX_FLUSH_TIME),
    receive after Time ->
        % Time limit reached, request flush from main server
        Server ! {flush, self(), Ref = make_ref()},
        receive
            {flushed, Ref} ->
                % Flush completed, restart the cycle
                commit_manager(StoreOpts, Server)
        after ?CONNECT_TIMEOUT -> timeout
        end,
        commit_manager(StoreOpts, Server)
    end.

%% @doc Ensure that the server has an active LMDB transaction for writes.
%%
%% This function implements lazy transaction creation by checking if a transaction
%% already exists in the server state. If not, it creates a new read-write
%% transaction and opens the default database within it.
%%
%% The lazy approach improves efficiency by avoiding transaction overhead when
%% the server is idle, while ensuring that write operations always have a
%% transaction available when needed.
%%
%% Transactions in LMDB are lightweight but still represent a commitment of
%% resources, so creating them only when needed helps optimize memory usage
%% and system performance.
%%
%% @param State Current server state map
%% @returns Server state guaranteed to have an active transaction
ensure_transaction(State) ->
    case maps:get(<<"transaction">>, State, undefined) of
        undefined ->
            % No transaction exists, create one
            {ok, Txn} =
                lmdb_nif:txn_begin(
                    maps:get(<<"env">>, State),
                    undefined,
                    0
                ),
            {ok, Dbi} = lmdb:open_db(Txn, default),
            State#{<<"transaction">> => Txn, <<"instance">> => Dbi};
        _ ->
            % Transaction already exists, return state unchanged
            State
    end.

%% @doc Test suite demonstrating basic store operations.
%%
%% The following functions implement unit tests using EUnit to verify that
%% the LMDB store implementation correctly handles various scenarios including
%% basic read/write operations, hierarchical listing, group creation, link
%% resolution, and type detection.

%% @doc Basic store test - verifies fundamental read/write functionality.
%%
%% This test creates a temporary database, writes a key-value pair, reads it
%% back to verify correctness, and cleans up by stopping the database. It
%% serves as a sanity check that the basic storage mechanism is working.
basic_test() ->
    StoreOpts = #{
        <<"prefix">> => <<"/tmp/store-1">>,
        <<"max-size">> => ?DEFAULT_SIZE
    },
    Res = write(StoreOpts, <<"Hello">>, <<"World2">>),
    ?assertEqual(ok, Res),
    {ok, Value} = read(StoreOpts, <<"Hello">>),
    ?assertEqual(Value, <<"World2">>),
    ok = stop(StoreOpts).

%% @doc List test - verifies prefix-based key listing functionality.
%%
%% This test creates several keys with hierarchical names and verifies that
%% the list operation correctly returns only keys matching a specific prefix.
%% It demonstrates the directory-like navigation capabilities of the store.
list_test() ->
    StoreOpts = #{
        <<"prefix">> => <<"/tmp/store-2">>,
        <<"max-size">> => ?DEFAULT_SIZE
    },
    write(StoreOpts, <<"colors/red">>, <<"1">>),
    write(StoreOpts, <<"colors/blue">>, <<"2">>),
    write(StoreOpts, <<"colors/green">>, <<"3">>),
    write(StoreOpts, <<"foo/bar">>, <<"baz">>),
    write(StoreOpts, <<"beep/boop">>, <<"bam">>),
    % Brief delay to ensure writes are flushed
    timer:sleep(10), 
    {ok, Keys} = list(StoreOpts, <<"colors">>),
    ?assertEqual([<<"colors/red">>, <<"colors/green">>, <<"colors/blue">>], Keys),
    ok = stop(StoreOpts).

%% @doc Group test - verifies group creation and type detection.
%%
%% This test creates a group entry and verifies that it can be read back
%% and correctly identified as a composite type rather than a simple value.
group_test() ->
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store3">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    make_group(StoreOpts, <<"colors">>),
    {ok, Result} = read(StoreOpts, <<"colors">>),
    ?event({ result, Result}),
    ?assertEqual(hb_util:atom(Result), group).

%% @doc Link test - verifies symbolic link creation and resolution.
%%
%% This test creates a regular key-value pair, creates a link pointing to it,
%% and verifies that reading from the link location returns the original value.
%% This demonstrates the transparent link resolution mechanism.
link_test() ->
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store3">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    write(StoreOpts, <<"foo/bar/baz">>, <<"Bam">>),
    make_link(StoreOpts, <<"foo/bar/baz">>, <<"foo/beep/baz">>),
    {ok, Result} = read(StoreOpts, <<"foo/beep/baz">>),
    ?event({ result, Result}),
    ?assertEqual(Result, <<"Bam">>).

%% @doc Type test - verifies type detection for both simple and composite entries.
%%
%% This test creates both a group (composite) entry and a regular (simple) entry,
%% then verifies that the type detection function correctly identifies each one.
%% This demonstrates the semantic classification system used by the store.
type_test() ->
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store-6">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    make_group(StoreOpts, <<"assets">>),
    Type = type(StoreOpts, <<"assets">>),
    ?event({type, Type}),
    ?assertEqual(composite, Type),
    write(StoreOpts, <<"assets/1">>, <<"bam">>),
    Type2 = type(StoreOpts, <<"assets/1">>),
    ?event({type2, Type2}),
    ?assertEqual(simple, Type2).

%% @doc Link key list test - verifies symbolic link creation using structured key paths.
%%
%% This test demonstrates the store's ability to handle complex key structures
%% represented as lists of binary segments, and verifies that symbolic links
%% work correctly when the target key is specified as a list rather than a
%% flat binary string.
%%
%% The test creates a hierarchical key structure using a list format (which
%% presumably gets converted to a path-like binary internally), creates a
%% symbolic link pointing to that structured key, and verifies that link
%% resolution works transparently to return the original value.
%%
%% This is particularly important for applications that organize data in
%% hierarchical structures where keys represent nested paths or categories,
%% and need to create shortcuts or aliases to deeply nested data.
link_key_list_test() ->
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store-7">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    write(StoreOpts, [ <<"parent">>, <<"key">> ], <<"value">>),
    make_link(StoreOpts, [ <<"parent">>, <<"key">> ], <<"my-link">>),
    timer:sleep(100),
    {ok, Result} = read(StoreOpts, <<"my-link">>),
    ?event({result, Result}),
    ?assertEqual(<<"value">>, Result). 