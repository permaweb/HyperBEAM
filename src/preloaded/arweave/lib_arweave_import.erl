%%% @doc Read the metadata an Arweave node keeps in RocksDB into the store the
%%% storage module it belongs to uses here.
%%%
%%% An operator who already runs an Arweave node has the weave on disk, packed
%%% for their address, with the node's own indexes beside it. The chunk files
%%% are byte-compatible with the ones this node reads, so nothing about them
%%% moves: they are read where they lie. What moves is the metadata, which is
%%% the two things a miner cannot mine without -- the record of which bytes of
%%% the weave a module holds, and the index that places each chunk in it with
%%% the Merkle paths that prove where it sits.
%%%
%%% The source is opened read-only and nothing is ever written to it, so the
%%% databases stay exactly as the node that built them left them, that node can
%%% go on reading them, and an import can be run again. What an import writes is
%%% this node's own index, which is not in the module's directory at all.
%%%
%%% == What is read ==
%%%
%%% Per storage module, from the directory named by the same identifier
%%% `lib_arweave_storage:id/1' gives it:
%%%
%%% ```
%%% <DataDir>/storage_modules/<StoreID>/rocksdb/ar_sync_record_db
%%% <DataDir>/storage_modules/<StoreID>/rocksdb/ar_data_sync_db
%%% <DataDir>/storage_modules/<StoreID>/rocksdb/ar_data_sync_chunk_db
%%% '''
%%%
%%% Every import names that `rocksdb' directory as its source: `discover/2'
%%% reads the modules a data directory holds and `lib_arweave_storage' names
%%% the directory of each, so an import in place reads from
%%% `<module_path>/rocksdb' and writes into the store beside it.
%%%
%%% `ar_sync_record_db' holds the record set the node last wrote whole, under
%%% `sync_records', and a write-ahead log of every change since, numbered 1 to
%%% the count under `wal'. `ar_data_sync_db' is the one database of the node
%%% that has column families; `chunks_index' holds one row per chunk, keyed by
%%% a 256-bit absolute end offset. `ar_data_sync_chunk_db' holds the data path
%%% of each chunk -- and the chunk itself, for a chunk no chunk file can
%%% represent -- under the 64-byte key that `chunks_index' row points at.
%%%
%%% That key carries the wall-clock time its row was written, so it is not
%%% derivable and nothing may be built on reproducing it. It is not kept: the
%%% two rows are joined during the scan and written as the one entry
%%% `lib_arweave_chunk_index' keeps, under the chunk's own absolute end offset.
%%%
%%% Nothing else is read. The data root indexes, the transaction indexes and
%%% the disk pool answer for uploading and serving data rather than for mining:
%%% a proof carries the transaction path, the data path and the chunk, and all
%%% three come from the three databases above. A node that is asked to serve
%%% the rest syncs it as it would have anyway.
%%%
%%% == Bounded memory ==
%%%
%%% A partition holds some 13.7 million chunks and every row carries the paths
%%% that prove one, so nothing here collects rows. The scan is driven by
%%% `chunks_index', reads the data path of each row as it reaches it, writes
%%% the entry, and moves on, holding one row at a time whatever the module's
%%% size.
%%%
%%% == Reading a foreign database ==
%%%
%%% Every value is an Erlang term the other node encoded, read back with
%%% `binary_to_term/2' in `safe' mode. A value that does not decode, or that
%%% does not hold what its keyspace promises, is counted and passed over rather
%%% than allowed to end the import: a source that is one row short is a source
%%% one chunk poorer, not one this node should refuse to mine from.
%%%
%%% RocksDB is not in the default build. `available/0' answers whether this one
%%% has it, and without it every import answers with an error rather than
%%% failing on a module that is not there.
-module(lib_arweave_import).
-export([available/0, discover/2, import/3, import_sync_records/3]).
-export([import_index/3]).
-include("include/hb.hrl").

%%% The directory a data directory holds its storage modules under.
-define(MODULES_DIR, "storage_modules").

%%% The databases one storage module's metadata is read from.
-define(SYNC_RECORD_DB, "ar_sync_record_db").
-define(DATA_SYNC_DB, "ar_data_sync_db").
-define(CHUNK_DATA_DB, "ar_data_sync_chunk_db").

%%% The column family of `ar_data_sync_db' holding one row per chunk.
-define(CHUNKS_INDEX, "chunks_index").

%%% The keys the record snapshot and the length of the write-ahead log are
%%% stored under. Every other key of that database is one entry of the log,
%%% named by `binary:encode_unsigned/1' of its number.
-define(SYNC_RECORDS_KEY, <<"sync_records">>).
-define(WAL_KEY, <<"wal">>).

%%% The width of every offset key of the source, in bits.
-define(OFFSET_BITS, 256).

%%% The rows scanned between progress events.
-define(PROGRESS, 100000).

%%% The record ids an Arweave node writes.
%%%
%%% Naming them is what makes a source's records readable at all:
%%% `binary_to_term/2' in `safe' mode refuses a term carrying an atom this node
%%% has never seen, and a node that has never held a replica-2.9 partition has
%%% had no reason to name `ar_data_sync_footprints'. Listing them here puts
%%% every one of them in this module's own atom table.
-define(RECORD_IDS,
    [
        ar_data_sync,
        ar_data_sync_footprints,
        ar_chunk_storage,
        ar_chunk_storage_replica_2_9_1_unpacked,
        ar_chunk_storage_replica_2_9_5_entropy,
        ar_chunk_storage_replica_2_9_1_entropy,
        invalid_chunks
    ]
).

%%% Whether a logged operation names a range of the weave. Every offset in the
%%% source is an Erlang integer of whatever width it needs, and an interval
%%% with no bytes in it is one `ar_intervals' has no meaning for.
-define(RANGE(End, Start),
    (
        is_integer(End) andalso is_integer(Start)
            andalso End > Start andalso Start >= 0
    )
).

%% @doc Whether this node can read the databases an Arweave node keeps its
%% metadata in.
%%
%% Answered at run time rather than compiled in. The RocksDB library is a
%% dependency of one rebar3 profile and of nothing else, and the Forge compiles
%% a device with its own fixed options rather than a profile's -- so a
%% compile-time gate would leave this module inert in every build the device
%% tests run against, which is to say untested. The library is here or it is
%% not, and this asks.
available() ->
    code:ensure_loaded(rocksdb) == {module, rocksdb}.

%% @doc The storage modules a data directory holds.
%%
%% Each is read from the name of its own directory, which is the identifier
%% `lib_arweave_storage:id/1' gives it, so an import is told what a data
%% directory holds by the disk rather than by a configuration that could
%% disagree with it. Discovery needs no database and so answers in every build.
%%
%% A directory that names no module is reported rather than passed over: it is
%% either a module this node cannot read or something that does not belong
%% there, and an operator counting their partitions should hear about both.
discover(DataDir, Opts) ->
    Dir = filename:join(hb_util:list(DataDir), ?MODULES_DIR),
    case file:list_dir(Dir) of
        {ok, Names} ->
            {ok, discovered(lists:sort(Names), Dir, [], Opts)};
        {error, Reason} ->
            {error,
                error_message(
                    404,
                    <<"import-source-unreadable">>,
                    detail("~ts: ~p", [Dir, Reason])
                )
            }
    end.

%% @doc Import everything one storage module needs to be mined from, out of the
%% directory holding the source's databases for it.
%%
%% The sync records come first and the index second, which is the order a
%% reader depends on them in: a record claiming bytes whose entries are not
%% written yet is a claim to answer for chunks that cannot be found, where an
%% entry no record claims is one nothing looks at.
%%
%% The whole import is one mutation of the module, so it runs where every other
%% mutation of it runs. A pass writing chunks while an import wrote the records
%% for them would leave each holding half of what the other had done.
import(Module, Source, Opts) ->
    maybe
        ok ?= readable(),
        lib_arweave_storage:exclusive(
            Module,
            fun() -> imported(Module, Source, Opts) end,
            Opts
        )
    end.

%% @doc Import the record of which bytes of the weave one storage module holds.
%%
%% The source writes the record set whole once a minute and logs every change
%% in between, so the snapshot alone is up to a minute behind the disk. The log
%% is replayed from 1 to the count the source recorded, in order, stopping at
%% the first entry that is not there: a node that died between recording the
%% number and writing the entry leaves exactly that gap, and every entry past
%% it changes a record that was never made.
import_sync_records(Module, Source, Opts) ->
    maybe
        ok ?= readable(),
        with_database(
            database(Source, ?SYNC_RECORD_DB),
            fun(DB) -> sync_records(Module, DB, Opts) end
        )
    end.

%% @doc Import the index that places each of a storage module's chunks in the
%% weave, joining each row of the source's chunk index to the data path it
%% keeps for it in a database of its own.
import_index(Module, Source, Opts) ->
    maybe
        ok ?= readable(),
        with_database(
            database(Source, ?CHUNK_DATA_DB),
            fun(ChunkData) ->
                with_column_families(
                    database(Source, ?DATA_SYNC_DB),
                    fun(DB, Families) ->
                        chunks(Module, DB, Families, ChunkData, Opts)
                    end
                )
            end
        )
    end.

%% @doc Hold only for a node that can read the source. An operator is told what
%% their build lacks rather than shown the failure of a call into a library that
%% is not there.
readable() ->
    case available() of
        true ->
            ok;
        false ->
            {error,
                error_message(
                    501,
                    <<"rocksdb-unavailable">>,
                    <<"This node is not built with RocksDB support. Build it "
                        "with the `rocksdb' rebar3 profile.">>
                )
            }
    end.

%%% Internal functions.

%% @doc Read each listed directory as the module it names, reporting the ones
%% that name none.
discovered([], _Dir, Modules, _Opts) ->
    lists:reverse(Modules);
discovered([Name | Rest], Dir, Modules, Opts) ->
    case parsed(Name) of
        {ok, Module} ->
            discovered(Rest, Dir, [Module | Modules], Opts);
        not_found ->
            ?event(warning,
                {arweave_import_unrecognised_module,
                    {directory, hb_util:bin(Dir)},
                    {name, hb_util:bin(Name)}
                },
                Opts
            ),
            discovered(Rest, Dir, Modules, Opts)
    end.

%% @doc The module one directory name spells, or `not_found'. The name is
%% whatever an operator's disk holds rather than anything this node wrote, so
%% one that carries an address that is not base64url reads as no module at all
%% instead of as the failure of the reader.
parsed(Name) ->
    case catch lib_arweave_storage:parse_id(Name) of
        {BucketSize, Bucket, _Packing} = Module
                when is_integer(BucketSize), is_integer(Bucket) ->
            {ok, Module};
        _Other ->
            not_found
    end.

%% @doc Build the standard error body.
error_message(Status, Message, Detail) ->
    #{
        <<"status">> => Status,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.

%% @doc Describe what went wrong in the terms the layer that failed put it in.
detail(Format, Args) ->
    hb_util:bin(io_lib:format(Format, Args)).

%% @doc Import one module, reporting what each half of it moved.
imported(Module, Source, Opts) ->
    maybe
        {ok, Records} ?= import_sync_records(Module, Source, Opts),
        {ok, Chunks} ?= import_index(Module, Source, Opts),
        Report = maps:merge(Records, Chunks),
        {ok,
            Report#{
                <<"module">> => hb_util:bin(lib_arweave_storage:id(Module)),
                <<"source">> => hb_util:bin(Source)
            }
        }
    end.

%% @doc Read one module's records: the snapshot, then the log written over it,
%% then the save that makes them this node's own.
sync_records(Module, DB, Opts) ->
    maybe
        {ok, Snapshot} ?= snapshot(DB),
        Wal = wal(DB),
        {Replayed, Counts} =
            replay(DB, 1, Wal, Snapshot, record_counts(Wal), Opts),
        {Records, Counts2} = nameable(Replayed, Counts, Opts),
        ok ?= lib_arweave_sync_record:save(Module, Records, Opts),
        {ok, Counts2#{ <<"sync-records">> => summary(Records) }}
    end.

%% @doc The record set the source last wrote whole, as the one set this node
%% keeps: the records held against an id and the records held against an id in
%% one packing are one map here, keyed exactly as the source keys them.
%%
%% A source that has never written a snapshot holds no records, which is a
%% module with nothing synced rather than a failure. A snapshot that is there
%% but is not a snapshot is a failure: an import that read it as nothing would
%% quietly leave a full partition claiming to hold no byte of the weave.
snapshot(DB) ->
    case rocksdb:get(DB, ?SYNC_RECORDS_KEY, []) of
        not_found ->
            {ok, #{}};
        {ok, Value} ->
            decoded(Value);
        {error, Reason} ->
            {error,
                error_message(
                    500,
                    <<"import-snapshot-unreadable">>,
                    detail("~p", [Reason])
                )
            }
    end.

%% @doc Read the two maps a snapshot holds into one record set.
decoded(Value) ->
    case term(Value) of
        {ok, {ById, ByIdType}} when is_map(ById), is_map(ByIdType) ->
            validated(maps:to_list(maps:merge(ById, ByIdType)), #{});
        _Other ->
            {error,
                error_message(
                    422,
                    <<"import-snapshot-malformed">>,
                    <<"A source snapshot is not a pair of record maps.">>
                )
            }
    end.

%% @doc Rebuild every record of a snapshot, refusing one that is not a set of
%% intervals of the weave.
validated([], Records) ->
    {ok, Records};
validated([{Id, Intervals} | Rest], Records) ->
    maybe
        {ok, Rebuilt} ?= intervals(Intervals),
        validated(Rest, Records#{ Id => Rebuilt })
    end.

%% @doc Rebuild one record from the source's own set.
%%
%% The intervals are added one at a time rather than taken as they are, so the
%% record this node saves is the one its own algebra builds: two intervals that
%% meet are one interval here, whatever the source held them as.
intervals(Set) ->
    case listed(Set) of
        {ok, List} -> rebuilt(List, ar_intervals:new());
        not_found -> {error, malformed()}
    end.

%% @doc The intervals of one of the source's sets, or `not_found' if it holds
%% none: the set is a term another node encoded, so it is not a set until it
%% has been read as one.
listed(Set) ->
    try {ok, ar_intervals:to_list(Set)}
    catch _Class:_Reason -> not_found
    end.

rebuilt([], Intervals) ->
    {ok, Intervals};
rebuilt([{End, Start} | Rest], Intervals) when ?RANGE(End, Start) ->
    rebuilt(Rest, ar_intervals:add(Intervals, End, Start));
rebuilt(_List, _Intervals) ->
    {error, malformed()}.

%% @doc How many entries the source recorded in its write-ahead log. A log this
%% node cannot read the length of is a log it does not replay, which leaves the
%% snapshot, which is the state the source itself would recover to if the log
%% were gone.
wal(DB) ->
    case read(DB, ?WAL_KEY) of
        {ok, Value} -> binary:decode_unsigned(Value);
        not_found -> 0
    end.

%% @doc Replay the write-ahead log over the snapshot, in the order it was
%% written. The replay stops where the log stops being readable, because an
%% entry applied over a missing one is a change made to a record that never had
%% the change before it.
replay(_DB, N, Wal, Records, Counts, _Opts) when N > Wal ->
    {Records, Counts};
replay(DB, N, Wal, Records, Counts, Opts) ->
    case entry(DB, N, Records) of
        {ok, Applied} ->
            replay(
                DB,
                N + 1,
                Wal,
                Applied,
                count(<<"replayed">>, Counts),
                Opts
            );
        not_found ->
            ?event(arweave_import,
                {write_ahead_log_ends, {recorded, Wal}, {replayed, N - 1}},
                Opts
            ),
            {Records, Counts}
    end.

%% @doc Apply one numbered entry of the write-ahead log to the records.
entry(DB, N, Records) ->
    maybe
        {ok, Value} ?= read(DB, binary:encode_unsigned(N)),
        {ok, Operation} ?= term(Value),
        operation(Records, Operation)
    end.

%% @doc Apply one logged operation, in the four shapes an Arweave node logs.
%%
%% `add' records a range against an id alone and `{add, Packing}' against the
%% id and against the id in that packing; `delete' and `cut' take bytes away
%% from an id and from every packing of it, because bytes that are gone are
%% gone whatever they were packed as. These are the operations this node's own
%% record set is changed by, so the source's log is replayed by the code that
%% would have made the same changes here.
operation(Records, {add, {End, Start, Id}})
        when ?RANGE(End, Start), is_atom(Id) ->
    {ok, lib_arweave_sync_record:add(Records, Id, End, Start)};
operation(Records, {{add, Packing}, {End, Start, Id}})
        when ?RANGE(End, Start), is_atom(Id) ->
    {ok, lib_arweave_sync_record:add(Records, Id, Packing, End, Start)};
operation(Records, {delete, {End, Start, Id}})
        when ?RANGE(End, Start), is_atom(Id) ->
    {ok, lib_arweave_sync_record:delete(Records, Id, End, Start)};
operation(Records, {cut, {Offset, Id}})
        when is_integer(Offset), Offset >= 0, is_atom(Id) ->
    {ok, lib_arweave_sync_record:cut(Records, Id, Offset)};
operation(_Records, _Operation) ->
    not_found.

%% @doc Keep the records this node has a name to file under, counting the rest.
%%
%% A record is stored under the name `lib_arweave_sync_record:label/1' gives
%% its id, so an id or a packing that has no spelling there has nowhere to go.
%% Dropping one costs a re-sync of the bytes it claimed, which is the cost this
%% node pays for any record it loses; keeping one would cost a save that fails
%% and an import that moved nothing.
nameable(Records, Counts, Opts) ->
    maps:fold(
        fun(Id, Intervals, {Kept, Counted}) ->
            kept(Id, Intervals, Kept, Counted, Opts)
        end,
        {#{}, Counts},
        Records
    ).

%% @doc Keep one record, or count it out. A record whose id is not one an
%% Arweave node writes is imported all the same, and reported: it is a record
%% this node cannot say anything about, which an operator can.
kept(Id, Intervals, Kept, Counts, Opts) ->
    case named(Id) of
        {ok, _Label} ->
            unknown(Id, Opts),
            {Kept#{ Id => Intervals }, Counts};
        not_found ->
            ?event(warning,
                {arweave_import_unnameable_record, {id, Id}},
                Opts
            ),
            {Kept, count(<<"skipped-records">>, Counts)}
    end.

%% @doc The name one record is filed under, or `not_found' when this node has
%% none for it.
named(Id) when is_atom(Id) ->
    label(Id);
named(Id) when is_tuple(Id), tuple_size(Id) == 2, is_atom(element(1, Id)) ->
    label(Id);
named(_Id) ->
    not_found.

label(Id) ->
    case catch lib_arweave_sync_record:label(Id) of
        Label when is_binary(Label) -> {ok, Label};
        _Other -> not_found
    end.

%% @doc Report a record whose id is one no Arweave node this code knows writes.
unknown(Id, Opts) ->
    case known(Id) of
        true ->
            ok;
        false ->
            ?event(warning,
                {arweave_import_unknown_record, {id, Id}},
                Opts
            )
    end.

%% @doc Whether an id is one an Arweave node writes.
known(Id) when is_atom(Id) ->
    lists:member(Id, ?RECORD_IDS);
known({Id, _Packing}) ->
    known(Id);
known(_Id) ->
    false.

%% @doc What each imported record holds, by the name it is filed under.
summary(Records) ->
    maps:from_list(
        [
            {
                lib_arweave_sync_record:label(Id),
                #{
                    <<"intervals">> =>
                        lib_arweave_sync_record:count(Records, Id),
                    <<"size">> => lib_arweave_sync_record:size(Records, Id)
                }
            }
        ||
            Id <- lib_arweave_sync_record:ids(Records)
        ]
    ).

%% @doc Scan the source's chunk index, writing one entry of this node's index
%% per row.
chunks(Module, DB, Families, ChunkData, Opts) ->
    maybe
        {ok, Family} ?= family(Families, ?CHUNKS_INDEX),
        {ok, Iterator} ?= iterator(DB, Family),
        try
            {ok,
                rows(
                    Iterator,
                    first,
                    Module,
                    ChunkData,
                    index_counts(),
                    Opts
                )
            }
        after
            rocksdb:iterator_close(Iterator)
        end
    end.

%% @doc Walk the source's chunk index from its first row to its last. A scan
%% that cannot go on stops where it is: the rows behind it are imported, and
%% the count of them is what an operator compares with the module's size.
rows(Iterator, Move, Module, ChunkData, Counts, Opts) ->
    case rocksdb:iterator_move(Iterator, Move) of
        {ok, Key, Value} ->
            rows(
                Iterator,
                next,
                Module,
                ChunkData,
                row(Module, Key, Value, ChunkData, Counts, Opts),
                Opts
            );
        {error, invalid_iterator} ->
            Counts;
        {error, Reason} ->
            ?event(warning,
                {arweave_import_scan_failed,
                    {module, lib_arweave_storage:id(Module)},
                    {reason, Reason},
                    {imported, maps:get(<<"chunks">>, Counts, 0)}
                },
                Opts
            ),
            Counts
    end.

%% @doc Import one row of the source's chunk index.
row(Module, Key, Value, ChunkData, Counts, Opts) ->
    case metadata(Key, Value) of
        {ok, ChunkDataKey, Metadata} ->
            joined(Module, ChunkDataKey, Metadata, ChunkData, Counts, Opts);
        not_found ->
            ?event(arweave_import,
                {chunk_row_unreadable, {size, byte_size(Value)}},
                Opts
            ),
            count(<<"undecodable-metadata">>, Counts)
    end.

%% @doc Write one chunk's entry, with the data path the source keeps for it.
%%
%% A row whose data path is gone is left out of the index rather than written
%% without one: a proof carries the data path, so an entry missing it names a
%% chunk this node could not prove, and a miner reading it would build a
%% solution it cannot submit. The count of them is what an operator weighs
%% against the module.
joined(Module, ChunkDataKey, Metadata, ChunkData, Counts, Opts) ->
    case data_path(ChunkData, ChunkDataKey) of
        {ok, DataPath} ->
            written(Module, Metadata, DataPath, Counts, Opts);
        {ok, Chunk, DataPath} ->
            Written = written(Module, Metadata, DataPath, Counts, Opts),
            body(Module, Metadata, Chunk, Written, Opts);
        not_found ->
            ?event(arweave_import,
                {chunk_data_missing, {key, hb_util:encode(ChunkDataKey)}},
                Opts
            ),
            count(<<"missing-chunk-data">>, Counts)
    end.

%% @doc Write one chunk's entry into this node's index.
written(Module, Metadata, DataPath, Counts, Opts) ->
    Entry = Metadata#{ <<"data-path">> => hb_util:encode(DataPath) },
    case lib_arweave_chunk_index:put(Module, Entry, Opts) of
        ok ->
            progress(count(<<"chunks">>, Counts), Module, Opts);
        {error, Reason} ->
            ?event(warning,
                {arweave_import_chunk_unwritable, {reason, Reason}},
                Opts
            ),
            count(<<"unwritable-chunks">>, Counts)
    end.

%% @doc Write the bytes of a chunk the source holds in its database rather than
%% in a chunk file. These are the chunks `ar_chunk_storage' refuses -- an
%% unpacked chunk shorter than 256 KiB -- which no miner reads and which the
%% chunk file layout cannot hold, so they move with their metadata.
body(Module, Metadata, Chunk, Counts, Opts) ->
    Offset = hb_maps:get(<<"absolute-end-offset">>, Metadata, 0, Opts),
    case lib_arweave_chunk_index:put_chunk(Module, Offset, Chunk, Opts) of
        ok ->
            count(<<"chunk-bodies">>, Counts);
        {error, Reason} ->
            ?event(warning,
                {arweave_import_chunk_unwritable, {reason, Reason}},
                Opts
            ),
            count(<<"unwritable-chunks">>, Counts)
    end.

%% @doc Read one row of the source's chunk index: the key it points at for its
%% data path, and the fields this node's own entry holds.
%%
%% The offset an entry is keyed on here spans 64 bits, so a row naming a wider
%% one is not a row this node can file. The weave is many orders of magnitude
%% from that bound; a row past it did not come from one.
metadata(<< Offset:?OFFSET_BITS >>, Value)
        when Offset > 0, Offset < (1 bsl 64) ->
    fields(Offset, term(Value));
metadata(_Key, _Value) ->
    not_found.

fields(Offset,
        {ok,
            {ChunkDataKey, TXRoot, DataRoot, TXPath, RelativeOffset, ChunkSize}
        })
        when
        is_binary(ChunkDataKey),
        is_binary(TXRoot),
        is_binary(DataRoot),
        is_binary(TXPath),
        is_integer(RelativeOffset),
        is_integer(ChunkSize)
    ->
    {ok,
        ChunkDataKey,
        #{
            <<"absolute-end-offset">> => Offset,
            <<"chunk-size">> => ChunkSize,
            <<"relative-offset">> => RelativeOffset,
            <<"tx-root">> => hb_util:encode(TXRoot),
            <<"data-root">> => hb_util:encode(DataRoot),
            <<"tx-path">> => hb_util:encode(TXPath)
        }
    };
fields(_Offset, _Term) ->
    not_found.

%% @doc The data path of one chunk, and the chunk itself when the source holds
%% the bytes in its database instead of in a chunk file.
data_path(DB, ChunkDataKey) ->
    maybe
        {ok, Value} ?= read(DB, ChunkDataKey),
        stored(term(Value))
    end.

stored({ok, {Chunk, DataPath}}) when is_binary(Chunk), is_binary(DataPath) ->
    {ok, Chunk, DataPath};
stored({ok, DataPath}) when is_binary(DataPath) ->
    {ok, DataPath};
stored(_Other) ->
    not_found.

%% @doc Report how far a scan has come, once every so many rows. A partition
%% takes millions of them, so an import that said nothing until it finished
%% would be indistinguishable from one that had stopped.
progress(Counts, Module, Opts) ->
    Chunks = maps:get(<<"chunks">>, Counts, 0),
    case Chunks rem ?PROGRESS of
        0 ->
            ?event(arweave_import,
                {chunks_imported,
                    {module, lib_arweave_storage:id(Module)},
                    {chunks, Chunks}
                },
                Opts
            );
        _Between ->
            ok
    end,
    Counts.

%% @doc The path of one of a module's source databases.
database(Source, Name) ->
    filename:join(hb_util:list(Source), Name).

%% @doc Run a function against one plain source database.
%%
%% Read-only is what keeps a data directory readable by the node that built it:
%% the open takes no lock, writes no log and compacts nothing. The database is
%% closed however the run ends, because a handle left open holds the files of a
%% directory this node does not own.
with_database(Path, Fun) ->
    maybe
        {ok, DB} ?= open(Path),
        try Fun(DB)
        after
            rocksdb:close(DB)
        end
    end.

%% @doc Run a function against the one source database that has column
%% families. Every family on disk has to be named to open it at all, so they
%% are read from the database rather than named from a list here that a source
%% written by another version would not match.
with_column_families(Path, Fun) ->
    maybe
        {ok, Names} ?= families(Path),
        {ok, DB, Handles} ?= open(Path, Names),
        try Fun(DB, maps:from_list(lists:zip(Names, Handles)))
        after
            rocksdb:close(DB)
        end
    end.

open(Path) ->
    case rocksdb:open_readonly(Path, [{create_if_missing, false}]) of
        {ok, DB} -> {ok, DB};
        {error, Reason} -> {error, unopenable(Path, Reason)}
    end.

open(Path, Names) ->
    Descriptors = [ {Name, []} || Name <- Names ],
    case
        rocksdb:open_readonly(Path, [{create_if_missing, false}], Descriptors)
    of
        {ok, DB, Handles} -> {ok, DB, Handles};
        {error, Reason} -> {error, unopenable(Path, Reason)}
    end.

families(Path) ->
    case rocksdb:list_column_families(Path, [{create_if_missing, false}]) of
        {ok, Names} -> {ok, Names};
        {error, Reason} -> {error, unopenable(Path, Reason)}
    end.

%% @doc One named column family of the source's column-family database.
family(Families, Name) ->
    case maps:get(Name, Families, not_found) of
        not_found ->
            {error,
                error_message(
                    404,
                    <<"import-family-missing">>,
                    hb_util:bin(Name)
                )
            };
        Handle ->
            {ok, Handle}
    end.

iterator(DB, Family) ->
    case rocksdb:iterator(DB, Family, []) of
        {ok, Iterator} ->
            {ok, Iterator};
        {error, Reason} ->
            {error,
                error_message(
                    500,
                    <<"import-scan-unopenable">>,
                    detail("~p", [Reason])
                )
            }
    end.

%% @doc Read one key of a source database. A key that cannot be read is one
%% that is not there as far as an import goes: every reader of this answers for
%% what it did not find, and none of them can do more with the reason than the
%% source itself could.
read(DB, Key) ->
    case rocksdb:get(DB, Key, []) of
        {ok, Value} -> {ok, Value};
        _Absent -> not_found
    end.

%% @doc Read the term one stored value holds, or `not_found' when it holds none
%% this node can read. `safe' refuses a term naming an atom this node has never
%% seen, which is a value written by software this one does not know.
term(Value) ->
    try {ok, binary_to_term(Value, [safe])}
    catch _Class:_Reason -> not_found
    end.

%% @doc The counters an index import reports.
index_counts() ->
    #{
        <<"chunks">> => 0,
        <<"chunk-bodies">> => 0,
        <<"undecodable-metadata">> => 0,
        <<"missing-chunk-data">> => 0,
        <<"unwritable-chunks">> => 0
    }.

%% @doc The counters a sync record import reports, against the length of the
%% log the source recorded.
record_counts(Wal) ->
    #{
        <<"write-ahead-log">> => Wal,
        <<"replayed">> => 0,
        <<"skipped-records">> => 0
    }.

count(Key, Counts) ->
    maps:update_with(Key, fun(Count) -> Count + 1 end, 1, Counts).

%% @doc The answer to a source database that cannot be opened.
unopenable(Path, Reason) ->
    error_message(
        404,
        <<"import-source-unopenable">>,
        detail("~ts: ~p", [Path, Reason])
    ).

%% @doc The answer to a snapshot that holds something other than records.
malformed() ->
    error_message(
        422,
        <<"import-record-malformed">>,
        <<"A source record is not a set of intervals of the weave.">>
    ).
