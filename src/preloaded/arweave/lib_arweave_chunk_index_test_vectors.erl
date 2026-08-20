%%% @doc Vectors for the index that places a stored chunk in the weave: the
%%% binary an entry is stored as, what that encoding refuses, and which chunk a
%%% byte resolves to on either side of the strict data split threshold.
%%%
%%% Every vector runs against a real LMDB store, in a data directory of its
%%% own, reached the way a running node reaches it -- through
%%% `lib_arweave_storage:store/2' from the storage module's own identifier.
-module(lib_arweave_chunk_index_test_vectors).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The bytes an entry holds beside its two paths: a version byte, a 32-bit
%%% chunk size, a 64-bit relative offset, two 32-byte Merkle roots and a 32-bit
%%% size for each path.
-define(FRAME_SIZE, (1 + 4 + 8 + 32 + 32 + 4 + 4)).

%%% Tests.

%% @doc An entry with no paths at all round-trips, and is exactly the frame.
empty_paths_round_trip_test() ->
    Metadata = metadata(?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE, <<>>, <<>>),
    {ok, Encoded} = lib_arweave_chunk_index:encode(Metadata),
    ?assertEqual(?FRAME_SIZE, byte_size(Encoded)),
    ?assertEqual(
        {ok, value(Metadata)},
        lib_arweave_chunk_index:decode(Encoded)
    ).

%% @doc An entry carrying the longest paths the protocol admits round-trips,
%% and costs the frame plus the paths themselves.
maximal_paths_round_trip_test() ->
    TXPath = crypto:strong_rand_bytes(?MAX_TX_PATH_SIZE),
    DataPath = crypto:strong_rand_bytes(?MAX_DATA_PATH_SIZE),
    Metadata =
        metadata(?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE, TXPath, DataPath),
    {ok, Encoded} = lib_arweave_chunk_index:encode(Metadata),
    ?assertEqual(
        ?FRAME_SIZE + ?MAX_TX_PATH_SIZE + ?MAX_DATA_PATH_SIZE,
        byte_size(Encoded)
    ),
    ?assertEqual(
        {ok, value(Metadata)},
        lib_arweave_chunk_index:decode(Encoded)
    ).

%% @doc An entry carrying the paths a mainnet chunk actually proves itself with
%% round-trips: the data path of one chunk of a 256 MiB transaction, and the tx
%% path of that transaction in a 128-transaction block, both built by the same
%% Merkle code the protocol validates them with.
mainnet_shaped_paths_round_trip_test() ->
    {DataRoot, DataPath} = data_proof(),
    {TXRoot, TXPath} = tx_proof(DataRoot),
    Metadata =
        #{
            <<"absolute-end-offset">> => 512 * ?DATA_CHUNK_SIZE,
            <<"chunk-size">> => ?DATA_CHUNK_SIZE,
            <<"relative-offset">> => 511 * ?DATA_CHUNK_SIZE,
            <<"tx-root">> => hb_util:encode(TXRoot),
            <<"data-root">> => hb_util:encode(DataRoot),
            <<"tx-path">> => hb_util:encode(TXPath),
            <<"data-path">> => hb_util:encode(DataPath)
        },
    {ok, Encoded} = lib_arweave_chunk_index:encode(Metadata),
    ?assertEqual(
        ?FRAME_SIZE + byte_size(TXPath) + byte_size(DataPath),
        byte_size(Encoded)
    ),
    ?assertEqual(
        {ok, value(Metadata)},
        lib_arweave_chunk_index:decode(Encoded)
    ).

%% @doc Every numeric field round-trips at the top of its own range, and a
%% field one step past it is refused rather than truncated into the store.
field_boundaries_round_trip_test() ->
    Widest =
        (metadata(1, 1, <<>>, <<>>))#{
            <<"chunk-size">> => (1 bsl 32) - 1,
            <<"relative-offset">> => (1 bsl 64) - 1
        },
    {ok, Encoded} = lib_arweave_chunk_index:encode(Widest),
    ?assertEqual(?FRAME_SIZE, byte_size(Encoded)),
    ?assertEqual({ok, value(Widest)}, lib_arweave_chunk_index:decode(Encoded)),
    ?assertEqual(
        {ok, value(metadata(1, 0, <<>>, <<>>))},
        round_trip(metadata(1, 0, <<>>, <<>>))
    ),
    ?assertMatch(
        {error, _},
        lib_arweave_chunk_index:encode(
            Widest#{ <<"chunk-size">> => 1 bsl 32 })
    ),
    ?assertMatch(
        {error, _},
        lib_arweave_chunk_index:encode(
            Widest#{ <<"relative-offset">> => 1 bsl 64 })
    ),
    ?assertMatch(
        {error, _},
        lib_arweave_chunk_index:encode(
            Widest#{ <<"tx-root">> => hb_util:encode(<<0:248>>) })
    ).

%% @doc A value cut short of what it declares is refused. Every prefix of a
%% real entry is tried, so the refusal does not depend on where the cut lands.
decode_refuses_a_truncated_value_test() ->
    {ok, Encoded} =
        lib_arweave_chunk_index:encode(
            metadata(?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE, <<"tx">>, <<"data">>)),
    lists:foreach(
        fun(Size) ->
            ?assertMatch(
                {error, _},
                lib_arweave_chunk_index:decode(
                    binary:part(Encoded, 0, Size))
            )
        end,
        lists:seq(0, byte_size(Encoded) - 1)
    ).

%% @doc A value written by a format this one does not know is refused rather
%% than read as though the fields behind the version byte were still these.
decode_refuses_an_unknown_version_test() ->
    {ok, << _Version:8, Rest/binary >>} =
        lib_arweave_chunk_index:encode(
            metadata(?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE, <<"tx">>, <<"data">>)),
    lists:foreach(
        fun(Version) ->
            ?assertMatch(
                {error, _},
                lib_arweave_chunk_index:decode(<< Version:8, Rest/binary >>)
            )
        end,
        [0, 2, 255]
    ).

%% @doc A value whose declared path sizes do not account for exactly the bytes
%% behind them is refused: one that claims more than is there, and one that
%% claims less and leaves a tail nothing accounts for.
decode_refuses_a_path_size_that_does_not_fit_test() ->
    Frame =
        <<
            1:8,
            (?DATA_CHUNK_SIZE):32,
            0:64,
            0:256,
            0:256
        >>,
    ?assertMatch(
        {error, _},
        lib_arweave_chunk_index:decode(
            << Frame/binary, 1000:32, "tx", 4:32, "data" >>)
    ),
    ?assertMatch(
        {error, _},
        lib_arweave_chunk_index:decode(
            << Frame/binary, 2:32, "tx", 1000:32, "data" >>)
    ),
    ?assertMatch(
        {error, _},
        lib_arweave_chunk_index:decode(
            << Frame/binary, 1:32, "tx", 4:32, "data" >>)
    ),
    ?assertMatch(
        {error, _},
        lib_arweave_chunk_index:decode(
            << Frame/binary, 2:32, "tx", 4:32, "data", "trailing" >>)
    ),
    ?assertMatch(
        {ok, _},
        lib_arweave_chunk_index:decode(
            << Frame/binary, 2:32, "tx", 4:32, "data" >>)
    ).

%% @doc A stored entry reads back as the message it was written from.
put_reads_back_unchanged_test() ->
    Opts = test_opts(),
    {DataRoot, DataPath} = data_proof(),
    {TXRoot, TXPath} = tx_proof(DataRoot),
    Metadata =
        #{
            <<"absolute-end-offset">> => 7 * ?DATA_CHUNK_SIZE,
            <<"chunk-size">> => ?DATA_CHUNK_SIZE,
            <<"relative-offset">> => 6 * ?DATA_CHUNK_SIZE,
            <<"tx-root">> => hb_util:encode(TXRoot),
            <<"data-root">> => hb_util:encode(DataRoot),
            <<"tx-path">> => hb_util:encode(TXPath),
            <<"data-path">> => hb_util:encode(DataPath)
        },
    ok = lib_arweave_chunk_index:put(test_module(), Metadata, Opts),
    ?assertEqual(
        {ok, Metadata},
        lib_arweave_chunk_index:get(
            test_module(), 7 * ?DATA_CHUNK_SIZE, Opts)
    ).

%% @doc A deleted entry is gone from the point read and from its bucket, and
%% the bucket's other entries are untouched.
delete_removes_one_entry_test() ->
    Opts = test_opts(),
    store(below_threshold_chunks(), Opts),
    ok = lib_arweave_chunk_index:delete(test_module(), 300, Opts),
    ?assertEqual(
        not_found,
        lib_arweave_chunk_index:get(test_module(), 300, Opts)
    ),
    ?assertEqual(
        [100, 100300, 300300],
        offsets(lib_arweave_chunk_index:list_bucket(
            test_module(), ?DATA_CHUNK_SIZE, Opts))
    ).

%% @doc Above the strict data split threshold every byte of a chunk resolves to
%% that chunk.
%%
%% The threshold T is not a multiple of 256 KiB, so the buckets above it run
%% (T, T + 256 KiB], (T + 256 KiB, T + 512 KiB] and so on, one chunk each. The
%% first bucket here holds a full 256 KiB chunk ending at T + 256 KiB. Its
%% first byte is T + 1 and its last is T + 256 KiB, and both give the seek
%% offset T + 1, because the seek offset of any byte above the threshold is the
%% first byte of the bucket it falls in. The last offset of the chunk and the
%% first of the next one are one apart, so this also pins which side of a
%% bucket boundary a byte belongs to.
every_byte_of_a_chunk_resolves_to_it_test() ->
    Opts = test_opts(),
    T = ar_block:strict_data_split_threshold(),
    store(above_threshold_chunks(), Opts),
    ?assertEqual(T + ?DATA_CHUNK_SIZE, at(T + 1, Opts)),
    ?assertEqual(
        T + ?DATA_CHUNK_SIZE,
        at(T + (?DATA_CHUNK_SIZE div 2), Opts)
    ),
    ?assertEqual(T + ?DATA_CHUNK_SIZE, at(T + ?DATA_CHUNK_SIZE, Opts)),
    ?assertEqual(
        T + ?DATA_CHUNK_SIZE + 100000,
        at(T + ?DATA_CHUNK_SIZE + 1, Opts)
    ).

%% @doc A byte in the zero-padding tail of a short chunk above the threshold
%% resolves to that chunk.
%%
%% The second bucket above the threshold covers (T + 256 KiB, T + 512 KiB] and
%% holds a chunk of 100000 bytes ending at T + 256 KiB + 100000. The bytes
%% above that end offset up to T + 512 KiB are the zero padding the chunk file
%% holds and no transaction owns. Every one of them has the seek offset
%% T + 256 KiB + 1, which the chunk covers, so the chunk is the answer.
padding_of_a_short_chunk_resolves_to_it_test() ->
    Opts = test_opts(),
    T = ar_block:strict_data_split_threshold(),
    Short = T + ?DATA_CHUNK_SIZE + 100000,
    store(above_threshold_chunks(), Opts),
    ?assertEqual(Short, at(Short, Opts)),
    ?assertEqual(Short, at(Short + 1, Opts)),
    ?assertEqual(Short, at(Short + 50000, Opts)),
    ?assertEqual(Short, at(T + (2 * ?DATA_CHUNK_SIZE), Opts)).

%% @doc A byte no stored chunk covers is absent rather than answered with the
%% nearest chunk. The bucket four above the threshold holds nothing at all, and
%% the gap between the two ranges stored below it is covered by nothing either.
an_uncovered_byte_is_not_found_test() ->
    Opts = test_opts(),
    T = ar_block:strict_data_split_threshold(),
    store(above_threshold_chunks() ++ below_threshold_chunks(), Opts),
    ?assertEqual(not_found, absent(T + (4 * ?DATA_CHUNK_SIZE), Opts)),
    ?assertEqual(not_found, absent(400000, Opts)).

%% @doc Below the threshold several chunks share one bucket, and a byte
%% resolves to the one that holds it rather than to its neighbour.
%%
%% Below the threshold the seek offset is the byte itself and the bucket end is
%% the largest multiple of 256 KiB at or below the byte, floored at 256 KiB. So
%% the four chunks ending at 100, 300, 100300 and 300300 -- all below
%% 2 * 256 KiB = 524288 -- share the bucket ending at 256 KiB.
several_chunks_in_one_bucket_test() ->
    Opts = test_opts(),
    store(below_threshold_chunks(), Opts),
    ?assertEqual(
        [100, 300, 100300, 300300],
        offsets(lib_arweave_chunk_index:list_bucket(
            test_module(), ?DATA_CHUNK_SIZE, Opts))
    ),
    lists:foreach(
        fun({Offset, Expected}) ->
            ?assertEqual(Expected, at(Offset, Opts))
        end,
        [
            {1, 100}, {50, 100}, {100, 100},
            {101, 300}, {300, 300},
            {301, 100300}, {50000, 100300}, {100300, 100300},
            {100301, 300300}, {200000, 300300}, {300300, 300300}
        ]
    ).

%% @doc Below the threshold a chunk can straddle a 256 KiB boundary, so the
%% bucket a byte falls in is not always the bucket its chunk is filed in.
%%
%% The chunk here covers (500000, 550000]. Its bucket end is the largest
%% multiple of 256 KiB at or below 550000, which is 524288. The byte 510000 is
%% inside it, but the largest multiple of 256 KiB at or below 510000 is 262144
%% -- a different bucket, and one that is not empty: it holds the four chunks
%% below it, none of which reaches 510000. A chunk is at most 256 KiB, so its
%% bucket is at most one 256 KiB step above the byte's, and the lookup finds it
%% there.
a_chunk_straddling_a_bucket_boundary_is_found_test() ->
    Opts = test_opts(),
    store(below_threshold_chunks() ++ [{550000, 50000}], Opts),
    ?assertEqual(
        ar_chunk_storage:get_chunk_bucket_end(550000),
        ar_chunk_storage:get_chunk_bucket_end(510000) + ?DATA_CHUNK_SIZE
    ),
    ?assertEqual(
        [100, 300, 100300, 300300],
        offsets(lib_arweave_chunk_index:list_bucket(
            test_module(), ar_chunk_storage:get_chunk_bucket_end(510000), Opts))
    ),
    ?assertEqual(
        [550000],
        offsets(lib_arweave_chunk_index:list_bucket(
            test_module(), ar_chunk_storage:get_chunk_bucket_end(550000), Opts))
    ),
    ?assertEqual(550000, at(510000, Opts)),
    ?assertEqual(550000, at(500001, Opts)),
    ?assertEqual(550000, at(550000, Opts)),
    ?assertEqual(not_found, absent(500000, Opts)).

%% @doc Keys sort the way the offsets they name sort, across every order of
%% magnitude an offset can take, because every key is the same width.
key_order_is_offset_order_test() ->
    T = ar_block:strict_data_split_threshold(),
    Offsets =
        [
            1, 2, 9, 10, 11, 99, 100, 101, 12345, ?DATA_CHUNK_SIZE,
            ?DATA_CHUNK_SIZE + 1, 1_000_000, 999_999_999, 1_000_000_000,
            1_000_000_000_000, T, T + 1, T + ?DATA_CHUNK_SIZE,
            1_000_000_000_000_000_000, (1 bsl 64) - 1
        ],
    Keys =
        [
            lib_arweave_chunk_index:key(test_module(), Offset)
        ||
            Offset <- Offsets
        ],
    ?assertEqual(Offsets, lists:sort(Offsets)),
    ?assertEqual(Keys, lists:sort(Keys)),
    ?assertMatch([_], lists:usort([ byte_size(Key) || Key <- Keys ])).

%% @doc A whole chunk that has no chunk file round-trips through the store, and
%% is gone once deleted.
chunk_round_trips_test() ->
    Opts = test_opts(),
    Chunk = crypto:strong_rand_bytes(?DATA_CHUNK_SIZE),
    ok = lib_arweave_chunk_index:put_chunk(test_module(), 300300, Chunk, Opts),
    ?assertEqual(
        {ok, Chunk},
        lib_arweave_chunk_index:get_chunk(test_module(), 300300, Opts)
    ),
    ok = lib_arweave_chunk_index:delete_chunk(test_module(), 300300, Opts),
    ?assertEqual(
        not_found,
        lib_arweave_chunk_index:get_chunk(test_module(), 300300, Opts)
    ).

%% @doc A module whose store has never been written answers every read with
%% absence rather than with an error a caller would have to interpret.
an_unwritten_module_is_not_found_test() ->
    Opts = test_opts(),
    T = ar_block:strict_data_split_threshold(),
    ?assertEqual(
        not_found,
        lib_arweave_chunk_index:get(test_module(), ?DATA_CHUNK_SIZE, Opts)
    ),
    ?assertEqual(not_found, absent(1, Opts)),
    ?assertEqual(not_found, absent(T + 1, Opts)),
    ?assertEqual(
        not_found,
        lib_arweave_chunk_index:get_chunk(test_module(), ?DATA_CHUNK_SIZE, Opts)
    ),
    ?assertEqual(
        {ok, []},
        lib_arweave_chunk_index:list_bucket(
            test_module(), ?DATA_CHUNK_SIZE, Opts)
    ).

%%% Test helpers.

%% @doc Options naming a data directory of this vector's own, so that what one
%% vector writes cannot be read by another. The index store is derived from it
%% by `lib_arweave_storage:store/2', which is how a running node reaches it.
test_opts() ->
    Dir =
        filename:join(
            [
                "/tmp",
                "hb-arweave-chunk-index",
                hb_util:list(
                    <<
                        (hb_util:bin(erlang:system_time(microsecond)))/binary,
                        "-",
                        (hb_util:encode(crypto:strong_rand_bytes(6)))/binary
                    >>
                )
            ]
        ),
    ok = filelib:ensure_dir(filename:join([Dir, "index"])),
    #{ <<"arweave-data-dir">> => hb_util:bin(Dir) }.

%% @doc The storage module every vector indexes. Its identifier names the
%% directory the index is written beside, exactly as it does for the Arweave
%% node whose data directory this is.
test_module() ->
    {ar_block:partition_size(), 0, unpacked}.

%% @doc One chunk's metadata, with paths distinguishable by their sizes.
metadata(AbsoluteEndOffset, ChunkSize, TXPath, DataPath) ->
    #{
        <<"absolute-end-offset">> => AbsoluteEndOffset,
        <<"chunk-size">> => ChunkSize,
        <<"relative-offset">> => 0,
        <<"tx-root">> => hb_util:encode(<< 1:256 >>),
        <<"data-root">> => hb_util:encode(<< 2:256 >>),
        <<"tx-path">> => hb_util:encode(TXPath),
        <<"data-path">> => hb_util:encode(DataPath)
    }.

%% @doc The fields `decode/1' answers with: everything but the absolute end
%% offset, which is the key rather than part of the value.
value(Metadata) ->
    maps:remove(<<"absolute-end-offset">>, Metadata).

%% @doc Encode a metadata message and read it straight back.
round_trip(Metadata) ->
    {ok, Encoded} = lib_arweave_chunk_index:encode(Metadata),
    lib_arweave_chunk_index:decode(Encoded).

%% @doc Store one entry per `{AbsoluteEndOffset, ChunkSize}' pair.
store(Chunks, Opts) ->
    lists:foreach(
        fun({AbsoluteEndOffset, ChunkSize}) ->
            ok =
                lib_arweave_chunk_index:put(
                    test_module(),
                    metadata(
                        AbsoluteEndOffset,
                        ChunkSize,
                        <<"tx-path">>,
                        <<"data-path">>
                    ),
                    Opts
                )
        end,
        Chunks
    ).

%% @doc Two chunks above the strict data split threshold, one per bucket: a
%% full 256 KiB chunk filling the first bucket, and a 100000 byte chunk at the
%% start of the second whose remaining bytes are the zero padding the chunk
%% file holds.
above_threshold_chunks() ->
    T = ar_block:strict_data_split_threshold(),
    [
        {T + ?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE},
        {T + ?DATA_CHUNK_SIZE + 100000, 100000}
    ].

%% @doc Four chunks below the strict data split threshold, covering (0, 300300]
%% with no gap. All four end below 2 * 256 KiB, so all four share the bucket
%% ending at 256 KiB.
below_threshold_chunks() ->
    [{100, 100}, {300, 200}, {100300, 100000}, {300300, 200000}].

%% @doc The absolute end offset of the chunk holding a one-based weave offset.
%% `get_by_byte/3' counts bytes from zero, as a recall byte is counted, so the
%% offset a vector reasons in is one above the byte it asks about.
at(Offset, Opts) ->
    found(lib_arweave_chunk_index:get_by_byte(test_module(), Offset - 1, Opts)).

%% @doc A lookup of a one-based weave offset that is expected to find nothing.
absent(Offset, Opts) ->
    lib_arweave_chunk_index:get_by_byte(test_module(), Offset - 1, Opts).

%% @doc The absolute end offsets a bucket listing answers with.
offsets({ok, Entries}) ->
    [ hb_maps:get(<<"absolute-end-offset">>, Entry, 0) || Entry <- Entries ].

%% @doc The absolute end offset of the chunk a lookup found.
found({ok, Metadata}) ->
    hb_maps:get(<<"absolute-end-offset">>, Metadata, 0).

%% @doc The data root of a 256 MiB transaction and the path proving its 512th
%% chunk, built by the Merkle code the protocol validates the proof with.
data_proof() ->
    Leaves =
        [
            {crypto:strong_rand_bytes(32), Chunk * ?DATA_CHUNK_SIZE}
        ||
            Chunk <- lists:seq(1, 1024)
        ],
    {DataRoot, Tree} = ar_merkle:generate_tree(Leaves),
    {DataRoot,
        ar_merkle:generate_path(DataRoot, 511 * ?DATA_CHUNK_SIZE, Tree)}.

%% @doc The transaction root of a 128-transaction block holding the given data
%% root, and the path proving it.
tx_proof(DataRoot) ->
    Size = 1024 * ?DATA_CHUNK_SIZE,
    Leaves =
        [
            {
                case Position of
                    64 -> DataRoot;
                    _ -> crypto:strong_rand_bytes(32)
                end,
                Position * Size
            }
        ||
            Position <- lists:seq(1, 128)
        ],
    {TXRoot, Tree} = ar_merkle:generate_tree(Leaves),
    {TXRoot, ar_merkle:generate_path(TXRoot, 63 * Size, Tree)}.
