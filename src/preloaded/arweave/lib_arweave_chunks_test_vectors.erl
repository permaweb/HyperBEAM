%%% @doc Vectors for the chunk files a storage module holds the weave in.
%%%
%%% Every test here runs against a real data directory of its own under the
%%% system temporary directory, because the thing being tested is the bytes on
%%% disk: an Arweave node wrote these files, and this node has to read and write
%%% them the same way or it is mining someone else's data.
%%%
%%% `on_disk_layout_test/0' is the compatibility test. It writes one chunk and
%%% then checks the file with `file:read_file/1' against numbers derived from
%%% the format -- the slot width, the slot's index in the file, the value of the
%%% three byte prefix -- rather than against whatever the code happened to
%%% produce.
%%%
%%% The four `upstream_' tests are ports of `ar_chunk_storage''s own EUnit
%%% tests: `test_well_aligned/0', `test_not_aligned/0',
%%% `test_cross_file_aligned/0' and `test_cross_file_not_aligned/0'. Their
%%% offsets, their reads and their expected results are upstream's, with
%%% upstream's `put/4' and `delete/1' split into the write and the sync record
%%% entry that `ar_chunk_storage:record_chunk/5' makes of them. Upstream's
%%% `chunk_bucket_test/0' and `get_chunk_byte_from_bucket_end_test/0' are not
%%% here: both mock `ar_block:strict_data_split_threshold/0', and they test the
%%% vendored arithmetic rather than anything this module owns.
-module(lib_arweave_chunks_test_vectors).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include("include/ar_chunk_storage.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The width of one slot: the three byte offset prefix and the chunk beside it.
-define(SLOT_SIZE, (?OFFSET_SIZE + ?DATA_CHUNK_SIZE)).

%%% A chunk file of eight buckets, so that a test can reach the boundary
%%% between two of them without writing two gigabytes of sparse file.
-define(SMALL_GROUP_SIZE, (8 * ?DATA_CHUNK_SIZE)).

%% @doc A chunk written into the slot its padded end offset owns reads back as
%% itself, from any byte the chunk covers, and from no other byte.
write_and_read_test() ->
    Opts = test_opts(<<"write-and-read">>),
    Module = module(),
    Chunk = chunk(),
    Offset = 2 * ?DATA_CHUNK_SIZE,
    Records = store(Module, #{}, Offset, Chunk, Opts),
    ?assertEqual(
        {ok, {Offset, Chunk}},
        read_by_record(Module, Offset - 1, Records, Opts)
    ),
    ?assertEqual(
        {ok, {Offset, Chunk}},
        read_by_record(Module, Offset - ?DATA_CHUNK_SIZE, Records, Opts)
    ),
    assert_read(Chunk, Offset, Module, Records, Opts),
    ?assertEqual(not_found, read_by_record(Module, Offset, Records, Opts)),
    ?assertEqual(
        not_found,
        read_by_record(Module, Offset - ?DATA_CHUNK_SIZE - 1, Records, Opts)
    ),
    ?assertEqual(
        not_found,
        lib_arweave_chunks:read(Module, 20 * ?DATA_CHUNK_SIZE, Opts)
    ).

%% @doc A chunk that is not exactly one slot wide is refused. The layout keeps
%% no sizes, so there is nowhere to say how much of a slot is chunk.
write_size_test() ->
    Opts = test_opts(<<"write-size">>),
    Module = module(),
    ?assertMatch(
        {error, #{ <<"message">> := <<"chunk-size-invalid">> }},
        lib_arweave_chunks:write(
            Module,
            2 * ?DATA_CHUNK_SIZE,
            binary:part(chunk(), 0, ?DATA_CHUNK_SIZE - 1),
            Opts
        )
    ),
    ?assertEqual(
        not_found,
        lib_arweave_chunks:read_offset(Module, 2 * ?DATA_CHUNK_SIZE, Opts)
    ).

%% @doc The bytes on disk are the bytes an Arweave node would have written.
%%
%% Everything asserted here is derived from the format: a file named for the
%% offset of the first bucket it covers, holding slots of
%% `?OFFSET_SIZE + ?DATA_CHUNK_SIZE' bytes indexed by bucket, each carrying the
%% chunk's start offset within its bucket in three big-endian bytes followed by
%% the chunk itself.
on_disk_layout_test() ->
    Opts = test_opts(<<"on-disk-layout">>, ?SMALL_GROUP_SIZE),
    Module = module(),
    Chunk = chunk(),
    % A chunk in the fourth bucket of the second chunk file, beginning seven
    % bytes into that bucket.
    ChunkFileStart = ?SMALL_GROUP_SIZE,
    BucketIndex = 3,
    Prefix = 7,
    BucketStart = ChunkFileStart + BucketIndex * ?DATA_CHUNK_SIZE,
    Offset = BucketStart + Prefix + ?DATA_CHUNK_SIZE,
    Position = BucketIndex * ?SLOT_SIZE,
    ok = lib_arweave_chunks:write(Module, Offset, Chunk, Opts),
    % The file is named by the offset of the first bucket it covers, and sits
    % in the chunk storage directory of the module's own directory.
    Path = lib_arweave_chunks:file_path(Module, ChunkFileStart, Opts),
    ?assertEqual(hb_util:bin(ChunkFileStart), filename:basename(Path)),
    ?assertEqual(
        hb_util:bin(lib_arweave_storage:chunk_dir(Module, Opts)),
        hb_util:bin(filename:dirname(Path))
    ),
    ?assertEqual([Path], lib_arweave_chunks:files(Module, Opts)),
    % The file holds every slot up to and including the one written, and
    % nothing beyond it.
    {ok, Bytes} = file:read_file(Path),
    ?assertEqual((BucketIndex + 1) * ?SLOT_SIZE, byte_size(Bytes)),
    ?assertEqual(
        << 0:(Position * 8) >>,
        binary:part(Bytes, 0, Position)
    ),
    ?assertEqual(
        << Prefix:?OFFSET_BIT_SIZE >>,
        binary:part(Bytes, Position, ?OFFSET_SIZE)
    ),
    ?assertEqual(
        Chunk,
        binary:part(Bytes, Position + ?OFFSET_SIZE, ?DATA_CHUNK_SIZE)
    ),
    % The same numbers read back through this module.
    ?assertEqual(
        {ChunkFileStart, Path, Position, Prefix},
        lib_arweave_chunks:locate(Module, Offset, Opts)
    ),
    ?assertEqual(
        {ok, Prefix},
        lib_arweave_chunks:read_offset(Module, Offset, Opts)
    ),
    ?assertEqual(
        {ok, {Offset, Chunk}},
        lib_arweave_chunks:read(Module, Offset - 1, BucketStart + Prefix, Opts)
    ).

%% @doc A chunk beginning at its bucket's own start is stored with the prefix
%% 262144 rather than 0, because 0 is what an untouched slot carries and the
%% two would otherwise be the same bytes.
special_zero_offset_test() ->
    Opts = test_opts(<<"special-zero-offset">>, ?SMALL_GROUP_SIZE),
    Module = module(),
    Chunk = chunk(),
    Offset = 5 * ?DATA_CHUNK_SIZE,
    Position = 4 * ?SLOT_SIZE,
    ok = lib_arweave_chunks:write(Module, Offset, Chunk, Opts),
    ?assertEqual(
        {ok, ?DATA_CHUNK_SIZE},
        lib_arweave_chunks:read_offset(Module, Offset, Opts)
    ),
    {ok, Bytes} = file:read_file(lib_arweave_chunks:file_path(Module, 0, Opts)),
    ?assertEqual(
        << ?DATA_CHUNK_SIZE:?OFFSET_BIT_SIZE >>,
        binary:part(Bytes, Position, ?OFFSET_SIZE)
    ),
    ?assertEqual(
        {ok, {Offset, Chunk}},
        lib_arweave_chunks:read(
            Module, Offset - 1, Offset - ?DATA_CHUNK_SIZE, Opts)
    ),
    % A slot in the same file that was never written carries a zero prefix and
    % holds no chunk, and one past the end of the file is not there at all.
    ?assertEqual(
        {ok, 0},
        lib_arweave_chunks:read_offset(Module, 3 * ?DATA_CHUNK_SIZE, Opts)
    ),
    ?assertEqual(
        not_found,
        lib_arweave_chunks:read(
            Module,
            3 * ?DATA_CHUNK_SIZE - 1,
            2 * ?DATA_CHUNK_SIZE,
            Opts
        )
    ),
    ?assertEqual(
        not_found,
        lib_arweave_chunks:read_offset(Module, 8 * ?DATA_CHUNK_SIZE, Opts)
    ).

%% @doc A run of chunks reads back whole, with holes, and with the range
%% reaching past either end of what the module holds.
read_range_test_() ->
    {timeout, 30, fun test_read_range/0}.

test_read_range() ->
    Opts = test_opts(<<"read-range">>, ?SMALL_GROUP_SIZE),
    Module = module(),
    [C1, C2, C3, C4] = [chunk() || _ <- lists:seq(1, 4)],
    % Four consecutive chunks in buckets 0 to 3, the third of them missing.
    Records0 = store(Module, #{}, ?DATA_CHUNK_SIZE, C1, Opts),
    Records1 = store(Module, Records0, 2 * ?DATA_CHUNK_SIZE, C2, Opts),
    Records2 = store(Module, Records1, 4 * ?DATA_CHUNK_SIZE, C4, Opts),
    Records = store(Module, Records2, 3 * ?DATA_CHUNK_SIZE, C3, Opts),
    ?assertEqual(
        {ok, [
            {?DATA_CHUNK_SIZE, C1},
            {2 * ?DATA_CHUNK_SIZE, C2},
            {3 * ?DATA_CHUNK_SIZE, C3},
            {4 * ?DATA_CHUNK_SIZE, C4}
        ]},
        lib_arweave_chunks:read_range(
            Module, 0, 4 * ?DATA_CHUNK_SIZE, Records, Opts)
    ),
    % A hole in the middle of the run yields fewer pairs, not an error.
    Holed = remove(Module, Records, 3 * ?DATA_CHUNK_SIZE, Opts),
    ?assertEqual(
        {ok, [
            {?DATA_CHUNK_SIZE, C1},
            {2 * ?DATA_CHUNK_SIZE, C2},
            {4 * ?DATA_CHUNK_SIZE, C4}
        ]},
        lib_arweave_chunks:read_range(
            Module, 0, 4 * ?DATA_CHUNK_SIZE, Holed, Opts)
    ),
    % A range beginning before the first chunk the module holds starts where
    % the synced interval does.
    Late = store(Module, #{}, 3 * ?DATA_CHUNK_SIZE, C3, Opts),
    ?assertEqual(
        {ok, [{3 * ?DATA_CHUNK_SIZE, C3}]},
        lib_arweave_chunks:read_range(
            Module, 0, 4 * ?DATA_CHUNK_SIZE, Late, Opts)
    ),
    % A range ending after the last chunk runs off the end of the file, which
    % is a short read rather than a failure.
    ?assertEqual(
        {ok, [
            {?DATA_CHUNK_SIZE, C1},
            {2 * ?DATA_CHUNK_SIZE, C2},
            {3 * ?DATA_CHUNK_SIZE, C3},
            {4 * ?DATA_CHUNK_SIZE, C4}
        ]},
        lib_arweave_chunks:read_range(
            Module, 0, 7 * ?DATA_CHUNK_SIZE, Records, Opts)
    ),
    % A range wider than a whole chunk file is refused.
    ?assertMatch(
        {error, #{ <<"message">> := <<"range-too-large">> }},
        lib_arweave_chunks:read_range(
            Module, 0, ?SMALL_GROUP_SIZE, Records, Opts)
    ).

%% @doc A range straddling the boundary between two chunk files is read from
%% both of them, in one call, in order.
read_range_across_files_test_() ->
    {timeout, 30, fun test_read_range_across_files/0}.

test_read_range_across_files() ->
    Opts = test_opts(<<"read-range-across-files">>, ?SMALL_GROUP_SIZE),
    Module = module(),
    [C1, C2, C3, C4] = [chunk() || _ <- lists:seq(1, 4)],
    % Buckets 6 and 7 are the last of the first chunk file; 8 and 9 are the
    % first two of the second.
    Records0 = store(Module, #{}, 7 * ?DATA_CHUNK_SIZE, C1, Opts),
    Records1 = store(Module, Records0, 8 * ?DATA_CHUNK_SIZE, C2, Opts),
    Records2 = store(Module, Records1, 9 * ?DATA_CHUNK_SIZE, C3, Opts),
    Records = store(Module, Records2, 10 * ?DATA_CHUNK_SIZE, C4, Opts),
    ?assertEqual(
        [
            lib_arweave_chunks:file_path(Module, 0, Opts),
            lib_arweave_chunks:file_path(Module, ?SMALL_GROUP_SIZE, Opts)
        ],
        lists:sort(lib_arweave_chunks:files(Module, Opts))
    ),
    ?assertEqual(
        {ok, [
            {7 * ?DATA_CHUNK_SIZE, C1},
            {8 * ?DATA_CHUNK_SIZE, C2},
            {9 * ?DATA_CHUNK_SIZE, C3},
            {10 * ?DATA_CHUNK_SIZE, C4}
        ]},
        lib_arweave_chunks:read_range(
            Module, 6 * ?DATA_CHUNK_SIZE, 4 * ?DATA_CHUNK_SIZE, Records, Opts)
    ),
    ?assertEqual(
        {ok, [{8 * ?DATA_CHUNK_SIZE, C2}, {9 * ?DATA_CHUNK_SIZE, C3}]},
        lib_arweave_chunks:read_range(
            Module, 7 * ?DATA_CHUNK_SIZE, 2 * ?DATA_CHUNK_SIZE, Records, Opts)
    ).

%% @doc A slot the sync record does not cover is not returned by a range read,
%% however much data the file holds there.
%%
%% This is the entropy case: a `replica_2_9' module has entropy written into
%% the buckets it holds no data for, and entropy carries an offset prefix of
%% its own, so only the record distinguishes the two.
read_range_ignores_entropy_test() ->
    Opts = test_opts(<<"read-range-entropy">>, ?SMALL_GROUP_SIZE),
    Module = module(),
    [C1, Entropy, C3] = [chunk() || _ <- lists:seq(1, 3)],
    % Three occupied buckets, of which the middle one holds entropy: it is
    % written the same way and left out of the record.
    Records0 = store(Module, #{}, ?DATA_CHUNK_SIZE, C1, Opts),
    ok = lib_arweave_chunks:write(Module, 2 * ?DATA_CHUNK_SIZE, Entropy, Opts),
    Records = store(Module, Records0, 3 * ?DATA_CHUNK_SIZE, C3, Opts),
    ?assertEqual(
        {ok, {2 * ?DATA_CHUNK_SIZE, Entropy}},
        lib_arweave_chunks:read(
            Module, 2 * ?DATA_CHUNK_SIZE - 1, ?DATA_CHUNK_SIZE, Opts)
    ),
    ?assertEqual(
        {ok, [{?DATA_CHUNK_SIZE, C1}, {3 * ?DATA_CHUNK_SIZE, C3}]},
        lib_arweave_chunks:read_range(
            Module, 0, 3 * ?DATA_CHUNK_SIZE, Records, Opts)
    ).

%% @doc Deleting a chunk empties its slot and leaves its neighbours alone.
delete_test() ->
    Opts = test_opts(<<"delete">>, ?SMALL_GROUP_SIZE),
    Module = module(),
    [C1, C2, C3] = [chunk() || _ <- lists:seq(1, 3)],
    Records0 = store(Module, #{}, ?DATA_CHUNK_SIZE, C1, Opts),
    Records1 = store(Module, Records0, 2 * ?DATA_CHUNK_SIZE, C2, Opts),
    Records2 = store(Module, Records1, 3 * ?DATA_CHUNK_SIZE, C3, Opts),
    Records = remove(Module, Records2, 2 * ?DATA_CHUNK_SIZE, Opts),
    ?assertEqual(
        {ok, 0},
        lib_arweave_chunks:read_offset(Module, 2 * ?DATA_CHUNK_SIZE, Opts)
    ),
    assert_read(not_found, 2 * ?DATA_CHUNK_SIZE, Module, Records, Opts),
    assert_read(C1, ?DATA_CHUNK_SIZE, Module, Records, Opts),
    assert_read(C3, 3 * ?DATA_CHUNK_SIZE, Module, Records, Opts),
    % The slot keeps its place in the file, zeroed.
    {ok, Bytes} = file:read_file(lib_arweave_chunks:file_path(Module, 0, Opts)),
    ?assertEqual(3 * ?SLOT_SIZE, byte_size(Bytes)),
    ?assertEqual(
        << 0:(?SLOT_SIZE * 8) >>,
        binary:part(Bytes, ?SLOT_SIZE, ?SLOT_SIZE)
    ),
    % Deleting from a file that was never written is already true.
    ?assertEqual(
        ok,
        lib_arweave_chunks:delete(Module, 100 * ?SMALL_GROUP_SIZE, Opts)
    ).

%% @doc Only the files named by a decimal offset are chunk files. The cursors
%% an Arweave node leaves in the same directory are not, and neither is the
%% temporary copy a defragmentation makes.
files_test() ->
    Opts = test_opts(<<"files">>, ?SMALL_GROUP_SIZE),
    Module = module(),
    ok = lib_arweave_chunks:write(Module, ?DATA_CHUNK_SIZE, chunk(), Opts),
    ok =
        lib_arweave_chunks:write(
            Module, ?SMALL_GROUP_SIZE + ?DATA_CHUNK_SIZE, chunk(), Opts),
    Dir = lib_arweave_storage:chunk_dir(Module, Opts),
    lists:foreach(
        fun(Name) -> ok = file:write_file(filename:join(Dir, Name), <<>>) end,
        [
            "prepare_replica_2_9_cursor",
            "repack_in_place_cursor2",
            "0.tmp",
            "chunks_sizes"
        ]
    ),
    ?assertEqual(
        [
            lib_arweave_chunks:file_path(Module, 0, Opts),
            lib_arweave_chunks:file_path(Module, ?SMALL_GROUP_SIZE, Opts)
        ],
        lists:sort(lib_arweave_chunks:files(Module, Opts))
    ),
    % A module whose directory has not been created holds no files.
    ?assertEqual([], lib_arweave_chunks:files(module(1), Opts)).

%% @doc The offsets a miner actually reads: above the strict data split
%% threshold, where every chunk is padded onto the same lattice and the bucket
%% a byte belongs to follows from the byte alone.
mainnet_offsets_test_() ->
    {timeout, 30, fun test_mainnet_offsets/0}.

test_mainnet_offsets() ->
    Opts = test_opts(<<"mainnet-offsets">>, ?SMALL_GROUP_SIZE),
    Module = module(),
    Threshold = ar_block:strict_data_split_threshold(),
    [C1, C2, C3] = [chunk() || _ <- lists:seq(1, 3)],
    % Three consecutive padded end offsets. The first two share a chunk file
    % and the third begins the next one.
    O1 = Threshold + ?DATA_CHUNK_SIZE,
    O2 = Threshold + 2 * ?DATA_CHUNK_SIZE,
    O3 = Threshold + 3 * ?DATA_CHUNK_SIZE,
    Records0 = store(Module, #{}, O1, C1, Opts),
    Records1 = store(Module, Records0, O2, C2, Opts),
    Records = store(Module, Records1, O3, C3, Opts),
    ?assertEqual(2, length(lib_arweave_chunks:files(Module, Opts))),
    % Every byte of a chunk names the bucket its chunk sits in, without a sync
    % record to place it.
    lists:foreach(
        fun({Offset, Chunk}) ->
            lists:foreach(
                fun(Byte) ->
                    ?assertEqual(
                        {ok, {Offset, Chunk}},
                        lib_arweave_chunks:read(Module, Byte, Opts)
                    )
                end,
                [
                    Offset - ?DATA_CHUNK_SIZE,
                    Offset - ?DATA_CHUNK_SIZE + 1,
                    Offset - ?DATA_CHUNK_SIZE div 2,
                    Offset - 1
                ]
            )
        end,
        [{O1, C1}, {O2, C2}, {O3, C3}]
    ),
    % The prefix every chunk above the threshold carries is the threshold's own
    % offset within a bucket.
    ?assertEqual(
        {ok, Threshold rem ?DATA_CHUNK_SIZE},
        lib_arweave_chunks:read_offset(Module, O1, Opts)
    ),
    % One recall range read, across the boundary between the two files.
    ?assertEqual(
        {ok, [{O1, C1}, {O2, C2}, {O3, C3}]},
        lib_arweave_chunks:read_range(
            Module,
            O1 - ?DATA_CHUNK_SIZE,
            3 * ?DATA_CHUNK_SIZE,
            Records,
            Opts
        )
    ).

%% @doc Upstream `ar_chunk_storage:test_well_aligned/0'. The offsets, the reads
%% and the expected results are upstream's.
upstream_well_aligned_test_() ->
    {timeout, 60, fun test_upstream_well_aligned/0}.

test_upstream_well_aligned() ->
    Opts = test_opts(<<"upstream-well-aligned">>),
    Module = module(),
    [C1, C2, C3] = [chunk() || _ <- lists:seq(1, 3)],
    Records0 = store(Module, #{}, 2 * ?DATA_CHUNK_SIZE, C1, Opts),
    assert_read(C1, 2 * ?DATA_CHUNK_SIZE, Module, Records0, Opts),
    ?assertEqual(
        not_found,
        read_by_record(Module, 2 * ?DATA_CHUNK_SIZE, Records0, Opts)
    ),
    ?assertEqual(
        not_found,
        read_by_record(Module, 2 * ?DATA_CHUNK_SIZE + 1, Records0, Opts)
    ),
    Records1 = remove(Module, Records0, 2 * ?DATA_CHUNK_SIZE, Opts),
    assert_read(not_found, 2 * ?DATA_CHUNK_SIZE, Module, Records1, Opts),
    Records2 = store(Module, Records1, ?DATA_CHUNK_SIZE, C2, Opts),
    assert_read(C2, ?DATA_CHUNK_SIZE, Module, Records2, Opts),
    assert_read(not_found, 2 * ?DATA_CHUNK_SIZE, Module, Records2, Opts),
    Records3 = store(Module, Records2, 2 * ?DATA_CHUNK_SIZE, C1, Opts),
    assert_read(C1, 2 * ?DATA_CHUNK_SIZE, Module, Records3, Opts),
    assert_read(C2, ?DATA_CHUNK_SIZE, Module, Records3, Opts),
    Pair = [{?DATA_CHUNK_SIZE, C2}, {2 * ?DATA_CHUNK_SIZE, C1}],
    lists:foreach(
        fun({Start, Size}) ->
            ?assertEqual(
                {ok, Pair},
                lib_arweave_chunks:read_range(
                    Module, Start, Size, Records3, Opts)
            )
        end,
        [
            {0, 2 * ?DATA_CHUNK_SIZE},
            {1, 2 * ?DATA_CHUNK_SIZE},
            {1, 2 * ?DATA_CHUNK_SIZE - 1},
            {0, 3 * ?DATA_CHUNK_SIZE},
            {0, ?DATA_CHUNK_SIZE + 1}
        ]
    ),
    Records4 = store(Module, Records3, 3 * ?DATA_CHUNK_SIZE, C3, Opts),
    assert_read(C2, ?DATA_CHUNK_SIZE, Module, Records4, Opts),
    assert_read(C1, 2 * ?DATA_CHUNK_SIZE, Module, Records4, Opts),
    assert_read(C3, 3 * ?DATA_CHUNK_SIZE, Module, Records4, Opts),
    ?assertEqual(
        not_found,
        read_by_record(Module, 3 * ?DATA_CHUNK_SIZE, Records4, Opts)
    ),
    ?assertEqual(
        not_found,
        read_by_record(Module, 3 * ?DATA_CHUNK_SIZE + 1, Records4, Opts)
    ),
    Records5 = store(Module, Records4, 2 * ?DATA_CHUNK_SIZE, C2, Opts),
    assert_read(C2, ?DATA_CHUNK_SIZE, Module, Records5, Opts),
    assert_read(C2, 2 * ?DATA_CHUNK_SIZE, Module, Records5, Opts),
    assert_read(C3, 3 * ?DATA_CHUNK_SIZE, Module, Records5, Opts),
    Records6 = remove(Module, Records5, ?DATA_CHUNK_SIZE, Opts),
    assert_read(not_found, ?DATA_CHUNK_SIZE, Module, Records6, Opts),
    ?assertEqual(
        {ok, []},
        lib_arweave_chunks:read_range(
            Module, 0, ?DATA_CHUNK_SIZE, Records6, Opts)
    ),
    assert_read(C2, 2 * ?DATA_CHUNK_SIZE, Module, Records6, Opts),
    assert_read(C3, 3 * ?DATA_CHUNK_SIZE, Module, Records6, Opts),
    ?assertEqual(
        {ok, [{2 * ?DATA_CHUNK_SIZE, C2}, {3 * ?DATA_CHUNK_SIZE, C3}]},
        lib_arweave_chunks:read_range(
            Module, 0, 4 * ?DATA_CHUNK_SIZE, Records6, Opts)
    ),
    ?assertEqual(
        {ok, []},
        lib_arweave_chunks:read_range(
            Module, 7 * ?DATA_CHUNK_SIZE, 13 * ?DATA_CHUNK_SIZE, Records6, Opts)
    ).

%% @doc Upstream `ar_chunk_storage:test_not_aligned/0'. The offsets, the reads
%% and the expected results are upstream's.
upstream_not_aligned_test_() ->
    {timeout, 60, fun test_upstream_not_aligned/0}.

test_upstream_not_aligned() ->
    Opts = test_opts(<<"upstream-not-aligned">>),
    Module = module(),
    [C1, C2, C3] = [chunk() || _ <- lists:seq(1, 3)],
    Records0 = store(Module, #{}, 2 * ?DATA_CHUNK_SIZE + 7, C1, Opts),
    assert_read(C1, 2 * ?DATA_CHUNK_SIZE + 7, Module, Records0, Opts),
    Records1 = remove(Module, Records0, 2 * ?DATA_CHUNK_SIZE + 7, Opts),
    assert_read(not_found, 2 * ?DATA_CHUNK_SIZE + 7, Module, Records1, Opts),
    Records2 = store(Module, Records1, 2 * ?DATA_CHUNK_SIZE + 7, C1, Opts),
    assert_read(C1, 2 * ?DATA_CHUNK_SIZE + 7, Module, Records2, Opts),
    assert_absent(
        [
            2 * ?DATA_CHUNK_SIZE + 7,
            ?DATA_CHUNK_SIZE + 7 - 1,
            ?DATA_CHUNK_SIZE,
            ?DATA_CHUNK_SIZE - 1,
            0,
            1
        ],
        Module,
        Records2,
        Opts
    ),
    Records3 = store(Module, Records2, ?DATA_CHUNK_SIZE + 3, C2, Opts),
    assert_read(C2, ?DATA_CHUNK_SIZE + 3, Module, Records3, Opts),
    assert_absent([0, 1, 2], Module, Records3, Opts),
    Records4 = remove(Module, Records3, 2 * ?DATA_CHUNK_SIZE + 7, Opts),
    assert_read(C2, ?DATA_CHUNK_SIZE + 3, Module, Records4, Opts),
    assert_read(not_found, 2 * ?DATA_CHUNK_SIZE + 7, Module, Records4, Opts),
    Records5 = store(Module, Records4, 3 * ?DATA_CHUNK_SIZE + 7, C3, Opts),
    assert_read(C3, 3 * ?DATA_CHUNK_SIZE + 7, Module, Records5, Opts),
    Records6 = store(Module, Records5, 3 * ?DATA_CHUNK_SIZE + 7, C1, Opts),
    assert_read(C1, 3 * ?DATA_CHUNK_SIZE + 7, Module, Records6, Opts),
    Half = ?DATA_CHUNK_SIZE div 2,
    Records7 = store(Module, Records6, 4 * ?DATA_CHUNK_SIZE + Half, C2, Opts),
    assert_read(C2, 4 * ?DATA_CHUNK_SIZE + Half, Module, Records7, Opts),
    assert_absent(
        [
            4 * ?DATA_CHUNK_SIZE + Half,
            3 * ?DATA_CHUNK_SIZE + 7,
            3 * ?DATA_CHUNK_SIZE + 8
        ],
        Module,
        Records7,
        Opts
    ),
    Records8 =
        store(Module, Records7, 5 * ?DATA_CHUNK_SIZE + Half + 1, C2, Opts),
    assert_read(C2, 5 * ?DATA_CHUNK_SIZE + Half + 1, Module, Records8, Opts),
    assert_read(not_found, 2 * ?DATA_CHUNK_SIZE + 7, Module, Records8, Opts),
    Records = remove(Module, Records8, 4 * ?DATA_CHUNK_SIZE + Half, Opts),
    assert_read(not_found, 4 * ?DATA_CHUNK_SIZE + Half, Module, Records, Opts),
    assert_read(C2, 5 * ?DATA_CHUNK_SIZE + Half + 1, Module, Records, Opts),
    assert_read(C1, 3 * ?DATA_CHUNK_SIZE + 7, Module, Records, Opts),
    ?assertEqual(
        {ok, [{3 * ?DATA_CHUNK_SIZE + 7, C1}]},
        lib_arweave_chunks:read_range(
            Module,
            2 * ?DATA_CHUNK_SIZE + 7,
            2 * ?DATA_CHUNK_SIZE,
            Records,
            Opts
        )
    ),
    ?assertEqual(
        {ok, [{3 * ?DATA_CHUNK_SIZE + 7, C1}]},
        lib_arweave_chunks:read_range(
            Module,
            2 * ?DATA_CHUNK_SIZE + 6,
            2 * ?DATA_CHUNK_SIZE,
            Records,
            Opts
        )
    ),
    % The second chunk ends past the range because the range reaches into the
    % bucket it is placed in.
    Two = [
        {3 * ?DATA_CHUNK_SIZE + 7, C1},
        {5 * ?DATA_CHUNK_SIZE + Half + 1, C2}
    ],
    lists:foreach(
        fun({Start, Size}) ->
            ?assertEqual(
                {ok, Two},
                lib_arweave_chunks:read_range(
                    Module, Start, Size, Records, Opts)
            )
        end,
        [
            {2 * ?DATA_CHUNK_SIZE + 7, 2 * ?DATA_CHUNK_SIZE + 1},
            {2 * ?DATA_CHUNK_SIZE + 7, 3 * ?DATA_CHUNK_SIZE},
            {2 * ?DATA_CHUNK_SIZE + 7 - 1, 3 * ?DATA_CHUNK_SIZE},
            {2 * ?DATA_CHUNK_SIZE, 4 * ?DATA_CHUNK_SIZE}
        ]
    ).

%% @doc Upstream `ar_chunk_storage:test_cross_file_aligned/0', with a chunk file
%% of eight buckets in place of upstream's eight thousand so that the boundary
%% is reachable. The offsets, the reads and the expected results are upstream's,
%% read in units of the chunk file size as upstream writes them.
upstream_cross_file_aligned_test_() ->
    {timeout, 60, fun test_upstream_cross_file_aligned/0}.

test_upstream_cross_file_aligned() ->
    Opts = test_opts(<<"upstream-cross-file-aligned">>, ?SMALL_GROUP_SIZE),
    Module = module(),
    Group = ?SMALL_GROUP_SIZE,
    [C1, C2] = [chunk() || _ <- lists:seq(1, 2)],
    Records0 = store(Module, #{}, Group, C1, Opts),
    assert_read(C1, Group, Module, Records0, Opts),
    assert_absent(
        [Group, Group + 1, 0, Group - ?DATA_CHUNK_SIZE - 1],
        Module,
        Records0,
        Opts
    ),
    Records1 = store(Module, Records0, Group + ?DATA_CHUNK_SIZE, C2, Opts),
    assert_read(C2, Group + ?DATA_CHUNK_SIZE, Module, Records1, Opts),
    assert_read(C1, Group, Module, Records1, Opts),
    Pair = [{Group, C1}, {Group + ?DATA_CHUNK_SIZE, C2}],
    ?assertEqual(
        {ok, Pair},
        lib_arweave_chunks:read_range(
            Module,
            Group - ?DATA_CHUNK_SIZE,
            2 * ?DATA_CHUNK_SIZE,
            Records1,
            Opts
        )
    ),
    ?assertEqual(
        {ok, Pair},
        lib_arweave_chunks:read_range(
            Module,
            Group - 2 * ?DATA_CHUNK_SIZE - 1,
            4 * ?DATA_CHUNK_SIZE,
            Records1,
            Opts
        )
    ),
    assert_absent([0, Group - ?DATA_CHUNK_SIZE - 1], Module, Records1, Opts),
    Records2 = remove(Module, Records1, Group, Opts),
    assert_read(not_found, Group, Module, Records2, Opts),
    assert_read(C2, Group + ?DATA_CHUNK_SIZE, Module, Records2, Opts),
    Records = store(Module, Records2, Group, C2, Opts),
    assert_read(C2, Group, Module, Records, Opts).

%% @doc Upstream `ar_chunk_storage:test_cross_file_not_aligned/0', with a chunk
%% file of eight buckets in place of upstream's eight thousand. The offsets, the
%% reads and the expected results are upstream's.
upstream_cross_file_not_aligned_test_() ->
    {timeout, 60, fun test_upstream_cross_file_not_aligned/0}.

test_upstream_cross_file_not_aligned() ->
    Opts = test_opts(<<"upstream-cross-file-not-aligned">>, ?SMALL_GROUP_SIZE),
    Module = module(),
    Group = ?SMALL_GROUP_SIZE,
    Half = ?DATA_CHUNK_SIZE div 2,
    [C1, C2, C3, C4, C5] = [chunk() || _ <- lists:seq(1, 5)],
    Records0 = store(Module, #{}, Group + 1, C1, Opts),
    assert_read(C1, Group + 1, Module, Records0, Opts),
    assert_absent(
        [Group + 1, Group - ?DATA_CHUNK_SIZE], Module, Records0, Opts),
    Records1 = store(Module, Records0, 2 * Group + Half, C2, Opts),
    assert_read(C2, 2 * Group + Half, Module, Records1, Opts),
    assert_absent([Group + 1], Module, Records1, Opts),
    Records2 = store(Module, Records1, 2 * Group - Half, C3, Opts),
    assert_read(C2, 2 * Group + Half, Module, Records2, Opts),
    assert_read(C3, 2 * Group - Half, Module, Records2, Opts),
    Records3 = store(Module, Records2, 2 * Group + 3 * Half, C4, Opts),
    Records4 = store(Module, Records3, 2 * Group + 5 * Half, C5, Opts),
    ?assertEqual(
        {ok, [{2 * Group + Half, C2}, {2 * Group + 3 * Half, C4}]},
        lib_arweave_chunks:read_range(
            Module, 2 * Group - Half, 2 * ?DATA_CHUNK_SIZE, Records4, Opts)
    ),
    ?assertEqual(
        {ok, [
            {2 * Group + Half, C2},
            {2 * Group + 3 * Half, C4},
            {2 * Group + 5 * Half, C5}
        ]},
        lib_arweave_chunks:read_range(
            Module, 2 * Group - Half + 10, 2 * ?DATA_CHUNK_SIZE, Records4, Opts)
    ),
    ?assertEqual(
        {ok, [{2 * Group - Half, C3}, {2 * Group + Half, C2}]},
        lib_arweave_chunks:read_range(
            Module,
            2 * Group - Half - ?DATA_CHUNK_SIZE,
            2 * ?DATA_CHUNK_SIZE,
            Records4,
            Opts
        )
    ),
    ?assertEqual(
        {ok, [
            {2 * Group - Half, C3},
            {2 * Group + Half, C2},
            {2 * Group + 3 * Half, C4}
        ]},
        lib_arweave_chunks:read_range(
            Module,
            2 * Group - Half - ?DATA_CHUNK_SIZE + 10,
            2 * ?DATA_CHUNK_SIZE,
            Records4,
            Opts
        )
    ),
    assert_absent([Group + 1, Group + Half - 1], Module, Records4, Opts),
    Records5 = remove(Module, Records4, 2 * Group - Half, Opts),
    assert_read(not_found, 2 * Group - Half, Module, Records5, Opts),
    assert_read(C2, 2 * Group + Half, Module, Records5, Opts),
    assert_read(C1, Group + 1, Module, Records5, Opts),
    Records6 = remove(Module, Records5, Group + 1, Opts),
    assert_read(not_found, Group + 1, Module, Records6, Opts),
    assert_read(not_found, 2 * Group - Half, Module, Records6, Opts),
    assert_read(C2, 2 * Group + Half, Module, Records6, Opts),
    Records7 = remove(Module, Records6, 2 * Group + Half, Opts),
    assert_read(not_found, 2 * Group + Half, Module, Records7, Opts),
    Records8 = remove(Module, Records7, Group + 1, Opts),
    Records9 = remove(Module, Records8, 100 * Group + 1, Opts),
    Records = store(Module, Records9, 2 * Group - Half, C1, Opts),
    assert_read(C1, 2 * Group - Half, Module, Records, Opts),
    assert_absent([2 * Group - Half], Module, Records, Opts).

%%% Test helpers.

%% @doc A storage module to write into. It is named by the identifier an
%% Arweave node names the directory by, so the layout under test is the layout
%% on an Arweave node's disk.
module() ->
    module(0).

module(Bucket) ->
    {ar_block:partition_size(), Bucket, unpacked}.

%% @doc A data directory of this test's own, under the system temporary
%% directory, with chunk files of the size upstream writes by default.
test_opts(Tag) ->
    #{ <<"arweave-data-dir">> => data_dir(Tag) }.

%% @doc The same, with chunk files of the given size.
test_opts(Tag, GroupSize) ->
    #{
        <<"arweave-data-dir">> => data_dir(Tag),
        <<"arweave-chunk-group-size">> => GroupSize
    }.

%% @doc A directory no other test and no other run writes into.
data_dir(Tag) ->
    hb_util:bin(
        filename:join([
            os:getenv("TMPDIR", "/tmp"),
            "hb-arweave-chunks",
            <<
                Tag/binary,
                "-",
                (hb_util:encode(crypto:strong_rand_bytes(6)))/binary
            >>
        ])
    ).

%% @doc A chunk of the size a slot holds.
chunk() ->
    crypto:strong_rand_bytes(?DATA_CHUNK_SIZE).

%% @doc Write a chunk and record the bytes it covers, which is the pair of
%% steps upstream's `ar_chunk_storage:record_chunk/5' takes.
store(Module, Records, PaddedEndOffset, Chunk, Opts) ->
    ok = lib_arweave_chunks:write(Module, PaddedEndOffset, Chunk, Opts),
    lib_arweave_sync_record:add(
        Records,
        ar_chunk_storage,
        PaddedEndOffset,
        PaddedEndOffset - ?DATA_CHUNK_SIZE
    ).

%% @doc Delete a chunk and drop the bytes it covered, which is what upstream's
%% `ar_chunk_storage:delete/2' does in one step.
remove(Module, Records, PaddedEndOffset, Opts) ->
    ok = lib_arweave_chunks:delete(Module, PaddedEndOffset, Opts),
    lib_arweave_sync_record:delete(
        Records,
        ar_chunk_storage,
        PaddedEndOffset,
        PaddedEndOffset - ?DATA_CHUNK_SIZE
    ).

%% @doc Read as upstream's `ar_chunk_storage:get/2' does: find the interval the
%% byte falls in, and read the chunk that interval's lattice places there.
read_by_record(Module, Byte, Records, Opts) ->
    Interval =
        lib_arweave_sync_record:interval(Records, ar_chunk_storage, Byte + 1),
    case Interval of
        not_found ->
            not_found;
        {_End, IntervalStart} ->
            lib_arweave_chunks:read(Module, Byte, IntervalStart, Opts)
    end.

%% @doc Upstream's `assert_get/3': every byte a chunk covers finds the chunk.
assert_read(Expected, PaddedEndOffset, Module, Records, Opts) ->
    Result =
        case Expected of
            not_found -> not_found;
            Chunk -> {ok, {PaddedEndOffset, Chunk}}
        end,
    lists:foreach(
        fun(Byte) ->
            ?assertEqual(
                Result,
                read_by_record(Module, Byte, Records, Opts)
            )
        end,
        [
            PaddedEndOffset - 1,
            PaddedEndOffset - 2,
            PaddedEndOffset - ?DATA_CHUNK_SIZE,
            PaddedEndOffset - ?DATA_CHUNK_SIZE + 1,
            PaddedEndOffset - ?DATA_CHUNK_SIZE + 2,
            PaddedEndOffset - ?DATA_CHUNK_SIZE div 2,
            PaddedEndOffset - ?DATA_CHUNK_SIZE div 2 + 1,
            PaddedEndOffset - ?DATA_CHUNK_SIZE div 2 - 1,
            PaddedEndOffset - ?DATA_CHUNK_SIZE div 3
        ]
    ).

%% @doc Each of the given bytes belongs to no chunk this module holds.
assert_absent(Bytes, Module, Records, Opts) ->
    lists:foreach(
        fun(Byte) ->
            ?assertEqual(
                not_found,
                read_by_record(Module, Byte, Records, Opts)
            )
        end,
        Bytes
    ).
