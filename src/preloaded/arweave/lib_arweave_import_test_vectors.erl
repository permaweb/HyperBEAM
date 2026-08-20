%%% @doc Vectors for the import of an Arweave node's metadata.
%%%
%%% There is no Arweave node here, so every vector writes the source itself:
%%% real RocksDB databases, in the layout and with the key and value encodings
%%% an Arweave node writes them in. `ar_sync_record_db' holds a snapshot under
%%% `sync_records' and a write-ahead log numbered from 1 to the count under
%%% `wal'; `ar_data_sync_db' holds the eight column families a node opens it
%%% with, in the order it opens them, with `chunks_index' keyed by 256-bit
%%% absolute end offset; `ar_data_sync_chunk_db' holds the data paths under the
%%% 64-byte keys those rows point at.
%%%
%%% Writing the source with the same library that reads it is the test: a
%%% reader that disagrees with those encodings fails the round trip, and so
%%% does a source built to encodings the reader does not expect.
%%%
%%% Every vector that opens a source asserts it is untouched afterwards -- every
%%% key and value of every database, and the size and modification time of
%%% every file holding them. An operator's data directory is one their Arweave
%%% node goes on reading.
-module(lib_arweave_import_test_vectors).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The databases one storage module's metadata is read from.
-define(SYNC_RECORD_DB, "ar_sync_record_db").
-define(DATA_SYNC_DB, "ar_data_sync_db").
-define(CHUNK_DATA_DB, "ar_data_sync_chunk_db").

%%% The column families an Arweave node opens `ar_data_sync_db' with, in the
%%% order it opens them. The order is the layout: a family is found by the name
%%% it was created under, and one created out of order is a different family.
-define(COLUMN_FAMILIES,
    [
        "default",
        "chunks_index",
        "data_root_index",
        "data_root_offset_index",
        "tx_index",
        "tx_offset_index",
        "disk_pool_chunks_index",
        "migrations_index"
    ]
).

%%% Tests.

%% @doc Every vector but one writes a real RocksDB source and reads it back, so
%% the suite is generated rather than declared: a node whose build carries no
%% RocksDB library has no source to write, and answers that instead of failing
%% on a call into a library that is not there.
%%
%% `discover/2' needs no database and is exercised either way, from the two
%% vectors below that only read directory names.
import_test_() ->
    {timeout, 300, vectors(lib_arweave_import:available())}.

vectors(false) ->
    [
        fun unavailable_imports/0,
        fun discover_reads_every_module/0,
        fun discover_reports_a_missing_directory/0
    ];
vectors(true) ->
    [
        fun snapshot_without_a_log/0,
        fun write_ahead_log_replays_every_operation/0,
        fun a_truncated_log_stops_at_the_gap/0,
        fun an_empty_source_imports_nothing/0,
        fun chunk_index_rows_read_back/0,
        fun a_chunk_held_in_the_database_is_moved/0,
        fun a_row_without_its_chunk_data_is_counted/0,
        fun an_unreadable_row_is_counted/0,
        fun import_moves_both_halves/0,
        fun a_missing_source_is_reported/0,
        fun discover_reads_every_module/0,
        fun discover_reports_a_missing_directory/0
    ].

%% @doc A snapshot with no log imports exactly the intervals it holds, against
%% the plain ids and the packing-tagged ones alike.
snapshot_without_a_log() ->
    Opts = opts(<<"snapshot">>),
    Source = source(Opts),
    sync_record_db(
        Source,
        {
            #{
                ar_data_sync => intervals([{5, 2}, {10, 7}]),
                ar_chunk_storage => intervals([{?DATA_CHUNK_SIZE, 0}])
            },
            #{ {ar_data_sync, packing()} => intervals([{5, 2}]) }
        },
        [],
        0
    ),
    Before = state(Source),
    {ok, Report} =
        lib_arweave_import:import_sync_records(module(), Source, Opts),
    ?assertEqual(0, hb_maps:get(<<"replayed">>, Report, none)),
    ?assertEqual(0, hb_maps:get(<<"skipped-records">>, Report, none)),
    {ok, Records} = lib_arweave_sync_record:load(module(), Opts),
    ?assertEqual(
        [ar_chunk_storage, ar_data_sync, {ar_data_sync, packing()}],
        lib_arweave_sync_record:ids(Records)
    ),
    ?assertEqual([{5, 2}, {10, 7}], listed(Records, ar_data_sync)),
    ?assertEqual([{?DATA_CHUNK_SIZE, 0}], listed(Records, ar_chunk_storage)),
    ?assertEqual([{5, 2}], listed(Records, {ar_data_sync, packing()})),
    ?assertEqual(
        #{ <<"intervals">> => 2, <<"size">> => 6 },
        hb_maps:get(<<"ar_data_sync">>,
            hb_maps:get(<<"sync-records">>, Report, #{}), none)
    ),
    ?assertEqual(Before, state(Source)).

%% @doc A snapshot with a log of all four logged operations replays to what an
%% Arweave node's own replay would hold.
%%
%% The snapshot holds bytes 1 to 10 against `ar_data_sync' and 6 to 10 against
%% the same id in one packing. Then, in order:
%%
%% <ol>
%% <li>`add' records 16 to 20 against `ar_chunk_storage', which no other
%% operation here touches: (20, 15].</li>
%% <li>`{add, Packing}' records 21 to 30 against `ar_data_sync' and against
%% `ar_data_sync' in that packing: (10, 0] and (30, 20] for the first, (10, 5]
%% and (30, 20] for the second.</li>
%% <li>`delete' takes bytes 3 to 5 from `ar_data_sync' and from every packing
%% of it. It splits the first record's (10, 0] into (2, 0] and (10, 5]; the
%% second record starts at 5, so it loses nothing.</li>
%% <li>`cut' drops everything above 25 from `ar_data_sync' and from every
%% packing of it, which shortens (30, 20] to (25, 20] in both.</li>
%% </ol>
write_ahead_log_replays_every_operation() ->
    Opts = opts(<<"replay">>),
    Source = source(Opts),
    sync_record_db(
        Source,
        {
            #{ ar_data_sync => intervals([{10, 0}]) },
            #{ {ar_data_sync, packing()} => intervals([{10, 5}]) }
        },
        [
            {add, {20, 15, ar_chunk_storage}},
            {{add, packing()}, {30, 20, ar_data_sync}},
            {delete, {5, 2, ar_data_sync}},
            {cut, {25, ar_data_sync}}
        ],
        4
    ),
    Before = state(Source),
    {ok, Report} =
        lib_arweave_import:import_sync_records(module(), Source, Opts),
    ?assertEqual(4, hb_maps:get(<<"write-ahead-log">>, Report, none)),
    ?assertEqual(4, hb_maps:get(<<"replayed">>, Report, none)),
    {ok, Records} = lib_arweave_sync_record:load(module(), Opts),
    ?assertEqual(
        [{2, 0}, {10, 5}, {25, 20}],
        listed(Records, ar_data_sync)
    ),
    ?assertEqual(
        [{10, 5}, {25, 20}],
        listed(Records, {ar_data_sync, packing()})
    ),
    ?assertEqual([{20, 15}], listed(Records, ar_chunk_storage)),
    ?assertEqual(Before, state(Source)).

%% @doc A log whose entries stop before its count does replays what is there
%% and stops at the gap.
%%
%% A node that died between recording the number of an entry and writing the
%% entry itself leaves exactly this: a count of 4 over the entries 1, 2 and 4.
%% The two before the gap are applied -- (5, 0] and (10, 7] against
%% `ar_data_sync' -- and the one past it is not, so nothing records (20, 15].
a_truncated_log_stops_at_the_gap() ->
    Opts = opts(<<"truncated">>),
    Source = source(Opts),
    sync_record_db(
        Source,
        {#{}, #{}},
        [
            {1, {add, {5, 0, ar_data_sync}}},
            {2, {add, {10, 7, ar_data_sync}}},
            {4, {add, {20, 15, ar_data_sync}}}
        ],
        4
    ),
    Before = state(Source),
    {ok, Report} =
        lib_arweave_import:import_sync_records(module(), Source, Opts),
    ?assertEqual(4, hb_maps:get(<<"write-ahead-log">>, Report, none)),
    ?assertEqual(2, hb_maps:get(<<"replayed">>, Report, none)),
    {ok, Records} = lib_arweave_sync_record:load(module(), Opts),
    ?assertEqual([{5, 0}, {10, 7}], listed(Records, ar_data_sync)),
    ?assertEqual(Before, state(Source)).

%% @doc A source that has never written a snapshot imports as a module holding
%% nothing, rather than as a failure: it is a storage module with nothing
%% synced into it yet.
an_empty_source_imports_nothing() ->
    Opts = opts(<<"empty">>),
    Source = source(Opts),
    sync_record_db(Source, none, [], 0),
    Before = state(Source),
    {ok, Report} =
        lib_arweave_import:import_sync_records(module(), Source, Opts),
    ?assertEqual(#{}, hb_maps:get(<<"sync-records">>, Report, none)),
    {ok, Records} = lib_arweave_sync_record:load(module(), Opts),
    ?assertEqual([], lib_arweave_sync_record:ids(Records)),
    ?assertEqual(Before, state(Source)).

%% @doc The rows of the source's chunk index import into this node's index and
%% read back holding the same fields, with the data path the source keeps for
%% each in a database of its own folded in.
chunk_index_rows_read_back() ->
    Opts = opts(<<"index">>),
    Source = source(Opts),
    First = chunk(1, offset(1), ?DATA_CHUNK_SIZE),
    Second = chunk(2, offset(2), 100000),
    chunk_db(Source, [First, Second]),
    data_sync_db(Source, [First, Second]),
    Before = state(Source),
    {ok, Report} = lib_arweave_import:import_index(module(), Source, Opts),
    ?assertEqual(2, hb_maps:get(<<"chunks">>, Report, none)),
    ?assertEqual(0, hb_maps:get(<<"chunk-bodies">>, Report, none)),
    ?assertEqual(0, hb_maps:get(<<"missing-chunk-data">>, Report, none)),
    ?assertEqual(
        {ok, metadata(First)},
        lib_arweave_chunk_index:get(module(), offset(1), Opts)
    ),
    ?assertEqual(
        {ok, metadata(Second)},
        lib_arweave_chunk_index:get(module(), offset(2), Opts)
    ),
    ?assertEqual(
        {ok, metadata(First)},
        lib_arweave_chunk_index:get_by_byte(module(), offset(1) - 1, Opts)
    ),
    ?assertEqual(Before, state(Source)).

%% @doc A source row holding the chunk itself beside its data path moves the
%% chunk too. These are the chunks no chunk file can represent, which is why
%% the source keeps them in its database, and why they cannot be read from the
%% chunk files this node reads everything else from.
a_chunk_held_in_the_database_is_moved() ->
    Opts = opts(<<"chunk-body">>),
    Source = source(Opts),
    Chunk = crypto:strong_rand_bytes(1024),
    Held = held(chunk(1, offset(1), 1024), Chunk),
    Plain = chunk(2, offset(2), ?DATA_CHUNK_SIZE),
    chunk_db(Source, [Held, Plain]),
    data_sync_db(Source, [Held, Plain]),
    Before = state(Source),
    {ok, Report} = lib_arweave_import:import_index(module(), Source, Opts),
    ?assertEqual(2, hb_maps:get(<<"chunks">>, Report, none)),
    ?assertEqual(1, hb_maps:get(<<"chunk-bodies">>, Report, none)),
    ?assertEqual(
        {ok, metadata(Held)},
        lib_arweave_chunk_index:get(module(), offset(1), Opts)
    ),
    ?assertEqual(
        {ok, Chunk},
        lib_arweave_chunk_index:get_chunk(module(), offset(1), Opts)
    ),
    ?assertEqual(
        not_found,
        lib_arweave_chunk_index:get_chunk(module(), offset(2), Opts)
    ),
    ?assertEqual(Before, state(Source)).

%% @doc A row whose data path is not in the source is counted and left out of
%% the index, and the rows around it are imported.
%%
%% An entry carries the data path a proof is made of, so one written without it
%% would name a chunk this node could not prove and a miner would build a
%% solution it cannot submit. The count is what an operator weighs against the
%% module.
a_row_without_its_chunk_data_is_counted() ->
    Opts = opts(<<"orphan">>),
    Source = source(Opts),
    Orphan = chunk(1, offset(1), ?DATA_CHUNK_SIZE),
    Kept = chunk(2, offset(2), ?DATA_CHUNK_SIZE),
    chunk_db(Source, [Kept]),
    data_sync_db(Source, [Orphan, Kept]),
    Before = state(Source),
    {ok, Report} = lib_arweave_import:import_index(module(), Source, Opts),
    ?assertEqual(1, hb_maps:get(<<"chunks">>, Report, none)),
    ?assertEqual(1, hb_maps:get(<<"missing-chunk-data">>, Report, none)),
    ?assertEqual(
        not_found,
        lib_arweave_chunk_index:get(module(), offset(1), Opts)
    ),
    ?assertEqual(
        {ok, metadata(Kept)},
        lib_arweave_chunk_index:get(module(), offset(2), Opts)
    ),
    ?assertEqual(Before, state(Source)).

%% @doc A row whose value is not a row this node can read is counted and passed
%% over, and the rows around it are imported. A source one row short is a
%% source one chunk poorer, not one to refuse to mine from.
an_unreadable_row_is_counted() ->
    Opts = opts(<<"unreadable">>),
    Source = source(Opts),
    Kept = chunk(1, offset(1), ?DATA_CHUNK_SIZE),
    chunk_db(Source, [Kept]),
    data_sync_db(Source, [Kept]),
    put_rows(
        filename:join(Source, ?DATA_SYNC_DB),
        [
            {<< (offset(2)):256 >>, <<"not a term">>},
            {<< (offset(3)):256 >>, term_to_binary({too, few, fields})}
        ]
    ),
    Before = state(Source),
    {ok, Report} = lib_arweave_import:import_index(module(), Source, Opts),
    ?assertEqual(1, hb_maps:get(<<"chunks">>, Report, none)),
    ?assertEqual(2, hb_maps:get(<<"undecodable-metadata">>, Report, none)),
    ?assertEqual(
        {ok, metadata(Kept)},
        lib_arweave_chunk_index:get(module(), offset(1), Opts)
    ),
    ?assertEqual(Before, state(Source)).

%% @doc One import moves both halves of a module and reports what each moved.
import_moves_both_halves() ->
    Opts = opts(<<"whole">>),
    Source = source(Opts),
    Chunk = chunk(1, offset(1), ?DATA_CHUNK_SIZE),
    sync_record_db(
        Source,
        {
            #{ ar_data_sync => intervals([bucket(1)]) },
            #{}
        },
        [{add, {offset(1), offset(1) - ?DATA_CHUNK_SIZE, ar_chunk_storage}}],
        1
    ),
    chunk_db(Source, [Chunk]),
    data_sync_db(Source, [Chunk]),
    Before = state(Source),
    {ok, Report} = lib_arweave_import:import(module(), Source, Opts),
    ?assertEqual(
        hb_util:bin(lib_arweave_storage:id(module())),
        hb_maps:get(<<"module">>, Report, none)
    ),
    ?assertEqual(1, hb_maps:get(<<"replayed">>, Report, none)),
    ?assertEqual(1, hb_maps:get(<<"chunks">>, Report, none)),
    {ok, Records} = lib_arweave_sync_record:load(module(), Opts),
    ?assertEqual([bucket(1)], listed(Records, ar_data_sync)),
    ?assertEqual([bucket(1)], listed(Records, ar_chunk_storage)),
    ?assertEqual(
        {ok, metadata(Chunk)},
        lib_arweave_chunk_index:get(module(), offset(1), Opts)
    ),
    ?assertEqual(Before, state(Source)).

%% @doc A source directory that holds no database is reported rather than read
%% as a module with nothing in it.
a_missing_source_is_reported() ->
    Opts = opts(<<"missing">>),
    Source = source(Opts),
    ?assertMatch(
        {error, #{ <<"message">> := <<"import-source-unopenable">> }},
        lib_arweave_import:import_sync_records(module(), Source, Opts)
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"import-source-unopenable">> }},
        lib_arweave_import:import_index(module(), Source, Opts)
    ).

%% @doc The modules a data directory holds are read from the names of their own
%% directories, and a directory that names none is reported.
discover_reads_every_module() ->
    Opts = opts(<<"discover">>),
    Modules =
        [
            {ar_block:partition_size(), 3, {replica_2_9, address()}},
            {ar_block:partition_size(), 0, unpacked},
            {1000000, 5, {spora_2_6, address()}}
        ],
    lists:foreach(
        fun(Module) ->
            ok = made(module_dir(Opts, lib_arweave_storage:id(Module)))
        end,
        Modules
    ),
    ok = made(module_dir(Opts, "not-a-storage-module")),
    {ok, Discovered} =
        lib_arweave_import:discover(
            hb_maps:get(<<"arweave-data-dir">>, Opts, none), Opts),
    ?assertEqual(lists:sort(Modules), lists:sort(Discovered)).

%% @doc A data directory that holds no storage modules at all is reported,
%% because an operator pointing an import at the wrong directory would
%% otherwise be told they have nothing to mine.
discover_reports_a_missing_directory() ->
    Opts = opts(<<"discover-missing">>),
    ?assertMatch(
        {error, #{ <<"message">> := <<"import-source-unreadable">> }},
        lib_arweave_import:discover(
            hb_maps:get(<<"arweave-data-dir">>, Opts, none), Opts)
    ).

%%% Test helpers.

%% @doc Write the source's sync record database: the snapshot the node last
%% wrote whole, the entries of its write-ahead log, and the count of them it
%% recorded. `none' writes no snapshot at all, which is a node that has not
%% written one yet. An entry may name its own number, for a log that stops
%% before its count does.
sync_record_db(Source, Snapshot, Entries, Wal) ->
    put_rows(
        filename:join(Source, ?SYNC_RECORD_DB),
        snapshot(Snapshot)
            ++ [{<<"wal">>, binary:encode_unsigned(Wal)}]
            ++ log(Entries, 1)
    ).

snapshot(none) ->
    [];
snapshot(Snapshot) ->
    [{<<"sync_records">>, term_to_binary(Snapshot)}].

log([], _N) ->
    [];
log([{N, Operation} | Rest], _N) when is_integer(N) ->
    [
        {binary:encode_unsigned(N), term_to_binary(Operation)}
    |
        log(Rest, N + 1)
    ];
log([Operation | Rest], N) ->
    [
        {binary:encode_unsigned(N), term_to_binary(Operation)}
    |
        log(Rest, N + 1)
    ].

%% @doc Write the source's chunk data database: one row per chunk, holding
%% either the data path alone or the chunk and its data path, under the
%% 64-byte key the chunk index row points at.
chunk_db(Source, Chunks) ->
    put_rows(
        filename:join(Source, ?CHUNK_DATA_DB),
        [ {chunk_data_key(Chunk), stored(Chunk)} || Chunk <- Chunks ]
    ).

%% @doc Write the source's column family database, with the eight families a
%% node opens it with in the order it opens them, and one `chunks_index' row
%% per chunk keyed by its 256-bit absolute end offset.
data_sync_db(Source, Chunks) ->
    put_rows(
        filename:join(Source, ?DATA_SYNC_DB),
        [
            {
                << (maps:get(offset, Chunk)):256 >>,
                term_to_binary(
                    {
                        chunk_data_key(Chunk),
                        maps:get(tx_root, Chunk),
                        maps:get(data_root, Chunk),
                        maps:get(tx_path, Chunk),
                        maps:get(relative_offset, Chunk),
                        maps:get(size, Chunk)
                    }
                )
            }
        ||
            Chunk <- Chunks
        ]
    ).

%% @doc Write rows into one source database, creating it as an Arweave node
%% would. The column family database is the one with families; every other is
%% plain, and holds its rows in the default family.
put_rows(Path, Rows) ->
    ok = made(Path),
    case filename:basename(Path) == ?DATA_SYNC_DB of
        true -> put_family_rows(Path, Rows);
        false -> put_plain_rows(Path, Rows)
    end.

put_plain_rows(Path, Rows) ->
    {ok, DB} = rocksdb:open(Path, [{create_if_missing, true}]),
    lists:foreach(
        fun({Key, Value}) -> ok = rocksdb:put(DB, Key, Value, []) end,
        Rows
    ),
    ok = rocksdb:close(DB).

put_family_rows(Path, Rows) ->
    {ok, DB, Handles} =
        rocksdb:open(
            Path,
            [{create_if_missing, true}, {create_missing_column_families, true}],
            [ {Name, []} || Name <- ?COLUMN_FAMILIES ]
        ),
    Chunks = family(Handles, "chunks_index"),
    lists:foreach(
        fun({Key, Value}) -> ok = rocksdb:put(DB, Chunks, Key, Value, []) end,
        Rows
    ),
    ok = rocksdb:close(DB).

%% @doc The handle of one named column family. The families are created in the
%% order an Arweave node creates them, so each handle is the one its name holds
%% the position of in that order.
family(Handles, Name) ->
    maps:get(Name, maps:from_list(lists:zip(?COLUMN_FAMILIES, Handles))).

%% @doc One chunk of the source, as both databases hold it. The data path and
%% the paths above it are distinguishable by their contents, so a reader that
%% crossed two fields would be caught by the round trip.
chunk(N, Offset, Size) ->
    #{
        offset => Offset,
        size => Size,
        relative_offset => (N - 1) * ?DATA_CHUNK_SIZE,
        tx_root => crypto:hash(sha256, << "tx-root-", N:8 >>),
        data_root => crypto:hash(sha256, << "data-root-", N:8 >>),
        tx_path => << "tx-path-", N:8 >>,
        data_path => << "data-path-", N:8 >>
    }.

%% @doc A chunk the source holds the bytes of, rather than one whose bytes are
%% in a chunk file.
held(Chunk, Bytes) ->
    Chunk#{ chunk => Bytes }.

%% @doc The value the source's chunk data database holds for one chunk.
stored(#{ chunk := Bytes, data_path := DataPath }) ->
    term_to_binary({Bytes, DataPath});
stored(#{ data_path := DataPath }) ->
    term_to_binary(DataPath).

%% @doc The key the source stores one chunk's data path under: 64 bytes of a
%% 256-bit number and the hash of the path itself. An Arweave node puts the
%% microsecond the row was written in the first half, which is why nothing can
%% derive the key and why this node joins on it once, during the import, and
%% does not keep it. A vector puts the chunk's own offset there instead, so
%% that the two databases can be written from one description of a chunk.
chunk_data_key(#{ data_path := DataPath, offset := Offset }) ->
    << Offset:256, (crypto:hash(sha256, DataPath))/binary >>.

%% @doc The entry this node's index holds for one chunk of the source.
metadata(Chunk) ->
    #{
        <<"absolute-end-offset">> => maps:get(offset, Chunk),
        <<"chunk-size">> => maps:get(size, Chunk),
        <<"relative-offset">> => maps:get(relative_offset, Chunk),
        <<"tx-root">> => hb_util:encode(maps:get(tx_root, Chunk)),
        <<"data-root">> => hb_util:encode(maps:get(data_root, Chunk)),
        <<"tx-path">> => hb_util:encode(maps:get(tx_path, Chunk)),
        <<"data-path">> => hb_util:encode(maps:get(data_path, Chunk))
    }.

%% @doc The absolute end offset of one of a vector's chunks. They are above the
%% strict data split threshold, one 256 KiB bucket apart, which is where a
%% mined partition's chunks are and one to a bucket as they are there.
offset(N) ->
    ar_block:strict_data_split_threshold() + (N * ?DATA_CHUNK_SIZE).

%% @doc The interval one of a vector's chunks covers: the 256 KiB of the weave
%% below its absolute end offset, which is the bucket its chunk file holds it
%% in and the range a record claiming it holds.
bucket(N) ->
    {offset(N), offset(N) - ?DATA_CHUNK_SIZE}.

%% @doc Everything about a source an import must leave alone: every key and
%% value of every database it holds, and the name, size and modification time
%% of every file holding them.
%%
%% The files are listed both before and after the databases are read, so a read
%% that touched one is caught here as surely as an import that did.
state(Source) ->
    Files = files(Source),
    Dumped = [ {Name, dumped(Source, Name)} || Name <- databases() ],
    {Files, Dumped, files(Source)}.

databases() ->
    [?SYNC_RECORD_DB, ?DATA_SYNC_DB, ?CHUNK_DATA_DB].

%% @doc Every key and value of one source database, read the way an import
%% reads it. A database a vector did not write holds nothing.
dumped(Source, Name) ->
    Path = filename:join(Source, Name),
    case filelib:is_dir(Path) of
        false -> not_found;
        true -> opened(Path, Name)
    end.

opened(Path, ?DATA_SYNC_DB) ->
    {ok, Names} = rocksdb:list_column_families(Path, []),
    {ok, DB, Handles} =
        rocksdb:open_readonly(
            Path,
            [{create_if_missing, false}],
            [ {Name, []} || Name <- Names ]
        ),
    Dumped =
        [
            {Name, entries(rocksdb:iterator(DB, Handle, []))}
        ||
            {Name, Handle} <- lists:zip(Names, Handles)
        ],
    ok = rocksdb:close(DB),
    Dumped;
opened(Path, _Name) ->
    {ok, DB} = rocksdb:open_readonly(Path, [{create_if_missing, false}]),
    Entries = entries(rocksdb:iterator(DB, [])),
    ok = rocksdb:close(DB),
    Entries.

%% @doc Every key and value an iterator walks, in the order it walks them.
entries({ok, Iterator}) ->
    Entries = moved(Iterator, first, []),
    ok = rocksdb:iterator_close(Iterator),
    Entries.

moved(Iterator, Move, Entries) ->
    case rocksdb:iterator_move(Iterator, Move) of
        {ok, Key, Value} -> moved(Iterator, next, [{Key, Value} | Entries]);
        {error, invalid_iterator} -> lists:reverse(Entries)
    end.

%% @doc The name, size and modification time of every file under a directory.
files(Dir) ->
    lists:sort(
        filelib:fold_files(
            Dir,
            ".*",
            true,
            fun(Path, Files) ->
                [
                    {
                        Path,
                        filelib:file_size(Path),
                        filelib:last_modified(Path)
                    }
                |
                    Files
                ]
            end,
            []
        )
    ).

%% @doc The intervals of one record, built as the source's own algebra builds
%% them.
intervals(List) ->
    ar_intervals:from_list(List).

%% @doc The intervals one loaded record holds, ascending by end offset.
listed(Records, Id) ->
    ar_intervals:to_list(lib_arweave_sync_record:intervals(Records, Id)).

%% @doc Create one directory, and the directories above it.
made(Dir) ->
    filelib:ensure_dir(filename:join(Dir, "made")).

%% @doc The directory one storage module occupies in a data directory.
module_dir(Opts, StoreID) ->
    filename:join(
        [lib_arweave_storage:data_dir(Opts), "storage_modules", StoreID]).

%% @doc The directory an Arweave node keeps one module's databases in, which is
%% the directory this node's own index is written beside.
source(Opts) ->
    filename:join(lib_arweave_storage:module_path(module(), Opts), "rocksdb").

%% @doc The packing every packing-tagged record of these vectors is held in.
packing() ->
    {replica_2_9, address()}.

%% @doc An address whose base64url spelling carries no `_', which is the
%% separator a storage module's directory name is built from.
address() ->
    crypto:hash(sha256, <<"arweave-import">>).

%% @doc Point a vector at a data directory of its own under the system
%% temporary directory, so that each one gets a source and a store no other
%% vector shares.
opts(Tag) ->
    #{
        <<"arweave-data-dir">> =>
            hb_util:bin(
                filename:join(
                    os:getenv("TMPDIR", "/tmp"),
                    hb_util:list(
                        <<
                            "hb-arweave-import-",
                            Tag/binary,
                            "-",
                            (hb_util:bin(
                                erlang:system_time(microsecond)))/binary,
                            "-",
                            (hb_util:encode(crypto:strong_rand_bytes(6)))/binary
                        >>
                    )
                )
            )
    }.

%% @doc Without RocksDB there is no source to read, and every import says which
%% build would read one rather than failing on a library that is not there.
unavailable_imports() ->
    ?assertEqual(false, lib_arweave_import:available()),
    ?assertMatch(
        {error, #{ <<"message">> := <<"rocksdb-unavailable">> }},
        lib_arweave_import:import(module(), "rocksdb", #{})
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"rocksdb-unavailable">> }},
        lib_arweave_import:import_sync_records(module(), "rocksdb", #{})
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"rocksdb-unavailable">> }},
        lib_arweave_import:import_index(module(), "rocksdb", #{})
    ).

%% @doc The storage module every vector imports into. One bucket of the weave,
%% unpacked, so that the vectors turn on the metadata rather than on a packing.
module() ->
    {ar_block:partition_size(), 2, unpacked}.
