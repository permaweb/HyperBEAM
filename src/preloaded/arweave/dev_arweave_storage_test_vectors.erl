%%% @doc Vectors for the storage modules an Arweave node holds the weave in,
%%% driven through `~arweave-storage@2.9' rather than through the libraries
%%% beneath it.
%%%
%%% Every vector runs against a data directory of its own under the system
%%% temporary directory: real chunk files, a real index, a real sync record and,
%%% where the module is packed, real replica-2.9 entropy. The weave they store
%%% into is synthetic but not fabricated -- one transaction of eight chunks in
%%% the first block -- and the two Merkle paths every chunk is stored with are
%%% built by the same `ar_merkle' a validator checks a proof of access with, so
%%% a chunk really is placed where these vectors say it is.
%%%
%%% The modules are tiny, and deliberately so. A mainnet partition is 3.6 TB and
%%% one replica-2.9 entropy footprint is sliced across the 1024 chunks of a
%%% 3.27 GiB sector. A module whose bucket is four chunks wide has a range of
%%% fourteen chunks once its ten chunk overlap is added, which is far shorter
%%% than one sector, so `ar_entropy_gen:entropy_offsets/2' is bounded by the
%%% module's own end and one footprint covers exactly one bucket. That is what
%%% makes preparing a bucket here cost thirty-two RandomX runs rather than
%%% thirty-two thousand.
%%%
%%% `prepares_a_whole_module/0' is not a `_test': it prepares every bucket of a
%%% module, which is fourteen footprints, and runs under
%%% `rebar3 device test --devices dev_arweave_storage
%%% --test all:prepares_a_whole_module'.
-module(dev_arweave_storage_test_vectors).
-export([prepares_a_whole_module/0]).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include("include/ar_chunk_storage.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The weave every vector stores into: one transaction of eight chunks, in the
%%% first block, which is the whole of the weave.
-define(CHUNKS, 8).
-define(WEAVE_SIZE, (?CHUNKS * ?DATA_CHUNK_SIZE)).

%%% The bucket size every module here is configured with, in chunks.
-define(BUCKET_CHUNKS, 4).
-define(BUCKET_SIZE, (?BUCKET_CHUNKS * ?DATA_CHUNK_SIZE)).

%%% The overlap a module's range carries past its own bucket, which is what
%%% lets a whole recall range be read from one module: ten chunks for a
%%% replica-2.9 module, a legacy recall range for every other.
-define(REPLICA_OVERLAP, (10 * ?DATA_CHUNK_SIZE)).
-define(UNPACKED_OVERLAP, ?LEGACY_RECALL_RANGE_SIZE).

%%% The width of one slot of a chunk file: the three byte offset prefix and the
%%% chunk beside it.
-define(SLOT_SIZE, (?OFFSET_SIZE + ?DATA_CHUNK_SIZE)).

%%% A chunk file of eight buckets, so that a vector can check where in one a
%%% slot sits without writing a two gibibyte sparse file.
-define(SMALL_GROUP_SIZE, (8 * ?DATA_CHUNK_SIZE)).

%%% Entropies of one footprint generated at once. A footprint is thirty-two
%%% independent RandomX runs, so one worker each costs a vector one round of
%%% them rather than four.
-define(WORKERS, ?COMPOSITE_PACKING_SUB_CHUNK_COUNT).

%%% The size of the leaf a mis-sized proof gives the first chunk of the weave:
%%% a hundred bytes, which no chunk of the weave the vectors store is.
-define(MIS_SIZE, 100).

%%% The bytes the first chunk of the unaligned weave holds, which shift every
%%% chunk after it off the 256 KiB grid. Only a transaction below the strict
%%% data split threshold may be split like this, and that is the whole of the
%%% weave these vectors write.
-define(SHIFT, 100000).
-define(UNALIGNED_SIZE, (?SHIFT + (?CHUNKS - 1) * ?DATA_CHUNK_SIZE)).

%%% Vectors.

%% @doc `modules' names a configured module by the identifier an Arweave node
%% names its directory by, over the range that identifier implies, and a module
%% nothing has been written to holds nothing and has been prepared for nothing.
configured_modules_test() ->
    Opts = node_opts(<<"modules">>, [unpacked_module(1), replica_module(0)]),
    {ok, Result} = resolve(<<"modules">>, #{}, Opts),
    Unpacked = field(<<"1">>, Result, Opts),
    Replica = field(<<"2">>, Result, Opts),
    % The identifier, and the directory it names, are the ones an Arweave node
    % writes: `storage_module_<BucketSize>_<Bucket>_<Packing>' under
    % `storage_modules' of the data directory.
    ?assertEqual(unpacked_id(1), field(<<"id">>, Unpacked, Opts)),
    ?assertEqual(replica_id(0), field(<<"id">>, Replica, Opts)),
    ?assertEqual(
        hb_util:bin(module_path(unpacked_id(1), Opts)),
        field(<<"path">>, Unpacked, Opts)
    ),
    % The range is the module's own bucket plus the overlap its packing carries.
    ?assertEqual(?BUCKET_SIZE, field(<<"range-start">>, Unpacked, Opts)),
    ?assertEqual(
        2 * ?BUCKET_SIZE + ?UNPACKED_OVERLAP,
        field(<<"range-end">>, Unpacked, Opts)
    ),
    ?assertEqual(0, field(<<"range-start">>, Replica, Opts)),
    ?assertEqual(
        ?BUCKET_SIZE + ?REPLICA_OVERLAP,
        field(<<"range-end">>, Replica, Opts)
    ),
    ?assertEqual(?BUCKET_SIZE, field(<<"bucket-size">>, Unpacked, Opts)),
    ?assertEqual(1, field(<<"bucket">>, Unpacked, Opts)),
    ?assertEqual(<<"unpacked">>, field(<<"packing">>, Unpacked, Opts)),
    ?assertEqual(0, field(<<"packing-difficulty">>, Unpacked, Opts)),
    ?assertEqual(not_found, field(<<"address">>, Unpacked, Opts)),
    ?assertEqual(<<"replica-2-9">>, field(<<"packing">>, Replica, Opts)),
    ?assertEqual(
        ?REPLICA_2_9_PACKING_DIFFICULTY,
        field(<<"packing-difficulty">>, Replica, Opts)
    ),
    ?assertEqual(
        hb_util:encode(address()),
        field(<<"address">>, Replica, Opts)
    ),
    % A module nothing has been written to holds nothing, and its preparation
    % has reached no further than the first byte of its own range.
    ?assertEqual(0, field(<<"synced">>, Unpacked, Opts)),
    ?assertEqual(0, field(<<"stored">>, Unpacked, Opts)),
    ?assertEqual(false, field(<<"prepared">>, Replica, Opts)),
    ?assertEqual(1, field(<<"prepare-cursor">>, Replica, Opts)),
    ?assertEqual(
        ?BUCKET_SIZE + 1,
        field(<<"prepare-cursor">>, Unpacked, Opts)
    ).

%% @doc A chunk stored with the proof that places it in the weave reads back
%% byte for byte, from any byte of the weave it covers and from no other, with
%% the paths and the offsets it was placed by.
stores_and_reads_a_chunk_test() ->
    Opts = node_opts(<<"store-and-read">>, [unpacked_module(0)]),
    Weave = weave(),
    Request = store_request(2, Weave),
    {ok, Stored} = resolve(<<"store">>, Request, Opts),
    ?assertEqual(true, field(<<"stored">>, Stored, Opts)),
    ?assertEqual(
        3 * ?DATA_CHUNK_SIZE,
        field(<<"absolute-end-offset">>, Stored, Opts)
    ),
    ?assertEqual(
        3 * ?DATA_CHUNK_SIZE,
        field(<<"padded-end-offset">>, Stored, Opts)
    ),
    ?assertEqual(unpacked_id(0), field(<<"module">>, Stored, Opts)),
    % Every byte of the chunk finds it; the first byte of the chunk beside it
    % finds nothing, because nothing is there.
    ?assertEqual(
        {ok, chunk(2)},
        resolve(<<"chunk">>, #{ <<"offset">> => 2 * ?DATA_CHUNK_SIZE }, Opts)
    ),
    ?assertEqual(
        {ok, chunk(2)},
        resolve(
            <<"chunk">>,
            #{ <<"offset">> => 3 * ?DATA_CHUNK_SIZE - 1 },
            Opts
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"chunk-not-held">> }},
        resolve(<<"chunk">>, #{ <<"offset">> => 3 * ?DATA_CHUNK_SIZE }, Opts)
    ),
    % The proof carries the chunk, the form it unpacks to, and everything that
    % places it in the weave. An unpacked module holds the two forms alike.
    {ok, Proof} =
        resolve(
            <<"chunk-proof">>,
            #{ <<"offset">> => 2 * ?DATA_CHUNK_SIZE + 7 },
            Opts
        ),
    ?assertEqual(hb_util:encode(chunk(2)), field(<<"chunk">>, Proof, Opts)),
    ?assertEqual(
        hb_util:encode(chunk(2)),
        field(<<"unpacked-chunk">>, Proof, Opts)
    ),
    ?assertEqual(<<"unpacked">>, field(<<"packing">>, Proof, Opts)),
    ?assertEqual(?DATA_CHUNK_SIZE, field(<<"chunk-size">>, Proof, Opts)),
    ?assertEqual(
        3 * ?DATA_CHUNK_SIZE,
        field(<<"absolute-end-offset">>, Proof, Opts)
    ),
    ?assertEqual(
        3 * ?DATA_CHUNK_SIZE,
        field(<<"relative-offset">>, Proof, Opts)
    ),
    ?assertEqual(
        maps:get(<<"tx-path">>, Request),
        field(<<"tx-path">>, Proof, Opts)
    ),
    ?assertEqual(
        maps:get(<<"data-path">>, Request),
        field(<<"data-path">>, Proof, Opts)
    ),
    ?assertEqual(
        maps:get(<<"tx-root">>, Request),
        field(<<"tx-root">>, Proof, Opts)
    ),
    ?assertEqual(
        hb_util:encode(data_root(Weave)),
        field(<<"data-root">>, Proof, Opts)
    ).

%% @doc `store' refuses a chunk its proof does not place, by name: bytes that do
%% not hash to the Merkle leaf, a leaf giving the chunk a size the bytes do not
%% fill, and a byte the block the proof is checked against does not hold.
%% Nothing any of them supplied is written.
store_refuses_an_unplaced_chunk_test() ->
    Opts = node_opts(<<"store-refusals">>, [unpacked_module(0)]),
    Weave = weave(),
    Request = store_request(2, Weave),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-chunk-id">> }},
        resolve(
            <<"store">>,
            Request#{ <<"chunk">> => hb_util:encode(chunk(3)) },
            Opts
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-chunk-size">> }},
        resolve(<<"store">>, mis_sized_request(), Opts)
    ),
    % A byte past the end of the block is walked to the last chunk the block
    % holds, which this chunk's own path does not lead to.
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-data-path">> }},
        resolve(
            <<"store">>,
            Request#{ <<"offset">> => ?WEAVE_SIZE + 10 },
            Opts
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"chunk-not-held">> }},
        resolve(<<"chunk">>, #{ <<"offset">> => 2 * ?DATA_CHUNK_SIZE }, Opts)
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"chunk-not-held">> }},
        resolve(<<"chunk">>, #{ <<"offset">> => 0 }, Opts)
    ),
    ?assertEqual(0, synced(unpacked_id(0), Opts)).

%% @doc Where a chunk lands is read out of its proof, never out of the request.
%% A request naming a different absolute end offset stores the chunk where its
%% Merkle paths place it, and it is readable there and nowhere else.
the_offset_comes_from_the_proof_test() ->
    Opts = node_opts(<<"proof-offset">>, [unpacked_module(0)]),
    Weave = weave(),
    {ok, Stored} =
        resolve(
            <<"store">>,
            (store_request(1, Weave))#{
                <<"absolute-end-offset">> => 7 * ?DATA_CHUNK_SIZE,
                <<"padded-end-offset">> => 7 * ?DATA_CHUNK_SIZE,
                <<"chunk-size">> => ?MIS_SIZE
            },
            Opts
        ),
    ?assertEqual(
        2 * ?DATA_CHUNK_SIZE,
        field(<<"absolute-end-offset">>, Stored, Opts)
    ),
    ?assertEqual(
        {ok, chunk(1)},
        resolve(<<"chunk">>, #{ <<"offset">> => ?DATA_CHUNK_SIZE }, Opts)
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"chunk-not-held">> }},
        resolve(
            <<"chunk">>,
            #{ <<"offset">> => 7 * ?DATA_CHUNK_SIZE - 1 },
            Opts
        )
    ),
    % The size in the index is the one the Merkle leaf gives the chunk too.
    {ok, Proof} =
        resolve(<<"chunk-proof">>, #{ <<"offset">> => ?DATA_CHUNK_SIZE }, Opts),
    ?assertEqual(?DATA_CHUNK_SIZE, field(<<"chunk-size">>, Proof, Opts)),
    ?assertEqual(?DATA_CHUNK_SIZE, synced(unpacked_id(0), Opts)).

%% @doc `range' answers with the chunks of a span in offset order, each with the
%% absolute end offset it is held at, skipping the buckets the module holds no
%% chunk in. A span this node holds no module over is answered with no chunks
%% rather than refused.
range_reads_a_span_test() ->
    Opts = node_opts(<<"range">>, [unpacked_module(0)]),
    Weave = weave(),
    store_chunks([0, 1, 3], Weave, Opts),
    ?assertEqual(
        [
            {?DATA_CHUNK_SIZE, chunk(0)},
            {2 * ?DATA_CHUNK_SIZE, chunk(1)},
            {4 * ?DATA_CHUNK_SIZE, chunk(3)}
        ],
        chunks(span(0, 4 * ?DATA_CHUNK_SIZE, Opts), Opts)
    ),
    % A span of one chunk is one chunk, and a span of the hole is nothing.
    ?assertEqual(
        [{2 * ?DATA_CHUNK_SIZE, chunk(1)}],
        chunks(span(?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE, Opts), Opts)
    ),
    ?assertEqual(
        [],
        chunks(span(2 * ?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE, Opts), Opts)
    ),
    % A span beyond every module's range is a span this node holds nothing in.
    Beyond = 2 * ?BUCKET_SIZE + ?UNPACKED_OVERLAP,
    ?assertEqual([], chunks(span(Beyond, ?DATA_CHUNK_SIZE, Opts), Opts)),
    % The span a request does not size is the recall range the protocol defines
    % at the difficulty it names.
    {ok, Whole} =
        resolve(
            <<"range">>,
            #{
                <<"range-start">> => 0,
                <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY
            },
            Opts
        ),
    ?assertEqual(
        ?RECALL_RANGE_SIZE div ?REPLICA_2_9_PACKING_DIFFICULTY,
        field(<<"size">>, Whole, Opts)
    ),
    {ok, Legacy} = resolve(<<"range">>, #{ <<"range-start">> => 0 }, Opts),
    ?assertEqual(?LEGACY_RECALL_RANGE_SIZE, field(<<"size">>, Legacy, Opts)),
    ?assertEqual(3, length(chunks(Legacy, Opts))).

%% @doc A span is read from every module holding any of it, and a chunk two
%% modules both hold is answered once. A node holding the two halves of a recall
%% range in two modules holds the whole of it.
range_spans_two_modules_test() ->
    Opts =
        node_opts(
            <<"range-two-modules">>,
            [unpacked_module(0), unpacked_module(1)]
        ),
    Weave = weave(),
    store_chunks([0, 1, 2, 3, 4, 5], Weave, Opts),
    % The first bucket's chunks are in the first module and the second bucket's
    % in the second, because that is where the offsets in their proofs fall.
    ?assertEqual(?BUCKET_SIZE, synced(unpacked_id(0), Opts)),
    ?assertEqual(2 * ?DATA_CHUNK_SIZE, synced(unpacked_id(1), Opts)),
    ?assertEqual(
        [
            {?DATA_CHUNK_SIZE, chunk(0)},
            {2 * ?DATA_CHUNK_SIZE, chunk(1)},
            {3 * ?DATA_CHUNK_SIZE, chunk(2)},
            {4 * ?DATA_CHUNK_SIZE, chunk(3)},
            {5 * ?DATA_CHUNK_SIZE, chunk(4)},
            {6 * ?DATA_CHUNK_SIZE, chunk(5)}
        ],
        chunks(span(0, 6 * ?DATA_CHUNK_SIZE, Opts), Opts)
    ),
    % A span that meets only the second module is answered by it alone.
    ?assertEqual(
        [
            {5 * ?DATA_CHUNK_SIZE, chunk(4)},
            {6 * ?DATA_CHUNK_SIZE, chunk(5)}
        ],
        chunks(span(4 * ?DATA_CHUNK_SIZE, 2 * ?DATA_CHUNK_SIZE, Opts), Opts)
    ).

%% @doc A span beginning in a stretch of the weave no module of this node holds
%% still answers with the chunks it runs into. Upstream reads the module holding
%% the start of a range; a range may begin before every module it meets.
range_runs_into_a_module_test() ->
    Opts = node_opts(<<"range-runs-in">>, [unpacked_module(1)]),
    Weave = weave(),
    store_chunks([4, 5], Weave, Opts),
    % Nothing covers the first bucket of the weave at all.
    ?assertMatch(
        {error, #{ <<"message">> := <<"offset-not-covered">> }},
        resolve(<<"store">>, store_request(0, Weave), Opts)
    ),
    ?assertEqual(
        [
            {5 * ?DATA_CHUNK_SIZE, chunk(4)},
            {6 * ?DATA_CHUNK_SIZE, chunk(5)}
        ],
        chunks(span(0, 6 * ?DATA_CHUNK_SIZE, Opts), Opts)
    ).

%% @doc A span is answered in the packing it was asked for, or in the packing of
%% the modules it was read from, and never in another. A miner asked for a
%% replica-2.9 range and handed unpacked chunks would hash bytes that meet no
%% difficulty and read its own partition as empty.
range_answers_in_one_packing_test() ->
    Opts = node_opts(<<"range-packing">>, [unpacked_module(0)]),
    Weave = weave(),
    store_chunks([0], Weave, Opts),
    {ok, Replica} =
        resolve(
            <<"range">>,
            #{
                <<"range-start">> => 0,
                <<"size">> => ?DATA_CHUNK_SIZE,
                <<"packing">> => <<"replica-2-9">>,
                <<"address">> => hb_util:encode(address())
            },
            Opts
        ),
    ?assertEqual(<<"replica-2-9">>, field(<<"packing">>, Replica, Opts)),
    ?assertEqual([], chunks(Replica, Opts)),
    % A request naming no packing takes what the modules hold.
    {ok, Any} =
        resolve(
            <<"range">>,
            #{ <<"range-start">> => 0, <<"size">> => ?DATA_CHUNK_SIZE },
            Opts
        ),
    ?assertEqual(<<"unpacked">>, field(<<"packing">>, Any, Opts)),
    ?assertEqual([{?DATA_CHUNK_SIZE, chunk(0)}], chunks(Any, Opts)),
    % A span nothing was read from is answered in the packing that was asked
    % after, which a request naming none has not constrained at all.
    Beyond = 2 * ?BUCKET_SIZE + ?UNPACKED_OVERLAP,
    {ok, Nothing} =
        resolve(
            <<"range">>,
            #{ <<"range-start">> => Beyond, <<"size">> => ?DATA_CHUNK_SIZE },
            Opts
        ),
    ?assertEqual(<<"any">>, field(<<"packing">>, Nothing, Opts)),
    ?assertEqual([], chunks(Nothing, Opts)),
    ?assertMatch(
        {error, #{ <<"message">> := <<"unsupported-packing">> }},
        resolve(
            <<"range">>,
            #{ <<"range-start">> => 0, <<"packing">> => <<"spora-2-6">> },
            Opts
        )
    ).

%% @doc `sync-record' reports the intervals `store' created, under each record a
%% module keeps them in, with the byte counts of what is really there.
sync_record_reports_what_is_held_test() ->
    Opts = node_opts(<<"sync-record">>, [unpacked_module(0)]),
    Weave = weave(),
    ?assertEqual(#{}, records(unpacked_id(0), Opts)),
    store_chunks([0, 1], Weave, Opts),
    Held = #{ <<"intervals">> => 1, <<"size">> => 2 * ?DATA_CHUNK_SIZE },
    ?assertEqual(
        #{
            <<"ar_data_sync">> => Held,
            <<"ar_data_sync.unpacked">> => Held,
            <<"ar_chunk_storage">> => Held
        },
        records(unpacked_id(0), Opts)
    ),
    % A chunk stored across a hole is a second interval of the same records.
    store_chunks([3], Weave, Opts),
    Split = #{ <<"intervals">> => 2, <<"size">> => 3 * ?DATA_CHUNK_SIZE },
    ?assertEqual(
        #{
            <<"ar_data_sync">> => Split,
            <<"ar_data_sync.unpacked">> => Split,
            <<"ar_chunk_storage">> => Split
        },
        records(unpacked_id(0), Opts)
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"unknown-storage-module">> }},
        resolve(<<"sync-record">>, #{ <<"module">> => <<"nowhere">> }, Opts)
    ).

%% @doc `prepare' answers only for a module it can name, and only for one that
%% entropy is written for. A node holding several modules has to be told which
%% of them a pass is for, and a node holding none has nothing to prepare.
prepare_refuses_test() ->
    ?assertMatch(
        {error, #{ <<"message">> := <<"unsupported-packing">> }},
        resolve(
            <<"prepare">>,
            #{ <<"footprints">> => 1 },
            node_opts(<<"prepare-unpacked">>, [unpacked_module(0)])
        )
    ),
    Several =
        node_opts(
            <<"prepare-several">>,
            [replica_module(0), replica_module(1)]
        ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"ambiguous-storage-module">> }},
        resolve(<<"prepare">>, #{ <<"footprints">> => 1 }, Several)
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"unknown-storage-module">> }},
        resolve(
            <<"prepare">>,
            #{ <<"footprints">> => 1, <<"module">> => <<"nowhere">> },
            Several
        )
    ),
    None = node_opts(<<"prepare-none">>, []),
    ?assertMatch(
        {error, #{ <<"message">> := <<"no-storage-modules">> }},
        resolve(<<"prepare">>, #{ <<"footprints">> => 1 }, None)
    ),
    % A node holding no storage modules describes none and holds no record of
    % any, rather than failing to answer.
    {ok, NoModules} = resolve(<<"modules">>, #{}, None),
    ?assertEqual(not_found, field(<<"1">>, NoModules, None)),
    {ok, NoRecords} = resolve(<<"sync-record">>, #{}, None),
    ?assertEqual(not_found, field(<<"1">>, NoRecords, None)).

%% @doc A preparation pass that did not move the cursor does not write it
%% again.
%%
%% This one is the Arweave node's own file, in the Arweave node's own
%% directory, and it is the file that lets either node carry on where the other
%% stopped. A module whose range is prepared reaches the end of it on every
%% pass it is given, and a pass runs once a second.
prepare_cursor_is_not_rewritten_test() ->
    Opts = node_opts(<<"prepare-cursor">>, [replica_module(0)]),
    Module = hd(lib_arweave_storage:modules(Opts)),
    {Start, End} = lib_arweave_storage:range(Module),
    Path =
        filename:join(
            lib_arweave_storage:chunk_dir(Module, Opts),
            "prepare_replica_2_9_cursor"
        ),
    ok = lib_arweave_entropy:advance(Module, End, Opts),
    ?assert(filelib:is_regular(Path)),
    ?assertEqual(End, lib_arweave_entropy:cursor(Module, Opts)),
    % With the file gone the cursor reads as the first byte of the range, which
    % is where a pass over a module nothing has prepared begins: nothing to
    % record, and nothing recorded.
    ok = file:delete(Path),
    ok = lib_arweave_entropy:advance(Module, Start + 1, Opts),
    ?assertEqual(false, filelib:is_regular(Path)).

%% @doc `store' writes into the module a request names, which must be a module
%% this node holds and one covering the offset the proof placed the chunk at.
%% Without one the first covering module takes the chunk.
store_names_its_module_test() ->
    Opts =
        node_opts(
            <<"store-module">>,
            [unpacked_module(0), unpacked_module(1)]
        ),
    Weave = weave(),
    % The fifth chunk of the weave falls in the overlap both modules cover.
    {ok, Named} =
        resolve(
            <<"store">>,
            (store_request(4, Weave))#{ <<"module">> => unpacked_id(0) },
            Opts
        ),
    ?assertEqual(unpacked_id(0), field(<<"module">>, Named, Opts)),
    ?assertEqual(?DATA_CHUNK_SIZE, synced(unpacked_id(0), Opts)),
    ?assertEqual(0, synced(unpacked_id(1), Opts)),
    {ok, Default} = resolve(<<"store">>, store_request(5, Weave), Opts),
    ?assertEqual(unpacked_id(1), field(<<"module">>, Default, Opts)),
    % A module that does not cover the offset, and a module this node does not
    % hold, are each refused by name.
    ?assertMatch(
        {error, #{ <<"message">> := <<"module-not-covering">> }},
        resolve(
            <<"store">>,
            (store_request(0, Weave))#{ <<"module">> => unpacked_id(1) },
            Opts
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"unknown-storage-module">> }},
        resolve(
            <<"store">>,
            (store_request(0, Weave))#{ <<"module">> => <<"nowhere">> },
            Opts
        )
    ).

%% @doc A caller supplying no block bounds has them read from the block index
%% this node validated, at the byte the chunk was fetched for. The first chunk
%% of the weave is the case that has no byte below its own seek offset.
stores_against_the_block_index_test() ->
    Opts = node_opts(<<"indexed-bounds">>, [unpacked_module(0)]),
    Weave = weave(),
    ok = seed(Weave, Opts),
    lists:foreach(
        fun(Index) ->
            {ok, Stored} =
                resolve(<<"store">>, indexed_request(Index, Weave), Opts),
            ?assertEqual(
                (Index + 1) * ?DATA_CHUNK_SIZE,
                field(<<"absolute-end-offset">>, Stored, Opts)
            ),
            ?assertEqual(
                {ok, chunk(Index)},
                resolve(
                    <<"chunk">>,
                    #{ <<"offset">> => Index * ?DATA_CHUNK_SIZE },
                    Opts
                )
            )
        end,
        [0, 1, 3]
    ),
    ?assertEqual(3 * ?DATA_CHUNK_SIZE, synced(unpacked_id(0), Opts)).

%% @doc A caller naming block bounds this node's own index disagrees with is
%% refused.
%%
%% The bounds are what the two Merkle paths are walked against, so a caller free
%% to choose them is a caller free to walk any bytes to any offset of a
%% partition this node then mines. Naming them is allowed -- a node whose chain
%% does not reach an offset has nothing to read them from -- but naming them
%% where the chain does reach is checked rather than believed.
store_refuses_unindexed_bounds_test() ->
    Opts = node_opts(<<"unindexed-bounds">>, [unpacked_module(0)]),
    Weave = weave(),
    ok = seed(Weave, Opts),
    Named = bounded(indexed_request(0, Weave), ?WEAVE_SIZE, Weave),
    lists:foreach(
        fun(Wrong) ->
            ?assertMatch(
                {error, #{ <<"message">> := <<"bounds-not-indexed">> }},
                resolve(<<"store">>, maps:merge(Named, Wrong), Opts)
            )
        end,
        [
            #{ <<"tx-root">> => hb_util:encode(crypto:hash(sha256, <<"no">>)) },
            #{ <<"block-start-offset">> => ?DATA_CHUNK_SIZE },
            #{ <<"block-size">> => ?WEAVE_SIZE - 1 }
        ]
    ),
    ?assertEqual(0, synced(unpacked_id(0), Opts)),
    % The bounds the index does hold are the ones it was given.
    ?assertMatch({ok, _}, resolve(<<"store">>, Named, Opts)),
    ?assertEqual(?DATA_CHUNK_SIZE, synced(unpacked_id(0), Opts)).

%% @doc Storing the same chunk twice is not an error and leaves the module
%% holding exactly what one store left it holding.
storing_a_chunk_twice_is_idempotent_test() ->
    Opts = node_opts(<<"store-twice">>, [unpacked_module(0)]),
    Weave = weave(),
    {ok, First} = resolve(<<"store">>, store_request(2, Weave), Opts),
    Records = records(unpacked_id(0), Opts),
    {ok, Second} = resolve(<<"store">>, store_request(2, Weave), Opts),
    ?assertEqual(First, Second),
    ?assertEqual(Records, records(unpacked_id(0), Opts)),
    ?assertEqual(
        {ok, chunk(2)},
        resolve(<<"chunk">>, #{ <<"offset">> => 2 * ?DATA_CHUNK_SIZE }, Opts)
    ),
    ?assertEqual(
        [{3 * ?DATA_CHUNK_SIZE, chunk(2)}],
        chunks(span(2 * ?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE, Opts), Opts)
    ).

%% @doc The bytes a store leaves on disk are the bytes an Arweave node would
%% have written. Every number asserted here is derived from the format -- the
%% file named for the offset of the first bucket it covers, holding slots of a
%% three byte prefix and a chunk, indexed by bucket -- rather than from what the
%% device produced.
on_disk_layout_test() ->
    Opts =
        node_opts(
            <<"on-disk">>,
            [unpacked_module(0)],
            ?SMALL_GROUP_SIZE
        ),
    Weave = weave(),
    store_chunks([5], Weave, Opts),
    % The sixth chunk of the weave occupies the sixth bucket of the first chunk
    % file, and begins at that bucket's own start.
    BucketIndex = 5,
    Position = BucketIndex * ?SLOT_SIZE,
    Path = chunk_file(unpacked_id(0), 0, Opts),
    {ok, Bytes} = file:read_file(Path),
    ?assertEqual((BucketIndex + 1) * ?SLOT_SIZE, byte_size(Bytes)),
    ?assertEqual(<< 0:(Position * 8) >>, binary:part(Bytes, 0, Position)),
    % A chunk beginning at its bucket's own start carries the prefix 262144
    % rather than 0, because 0 is what an untouched slot carries.
    ?assertEqual(
        << ?DATA_CHUNK_SIZE:?OFFSET_BIT_SIZE >>,
        binary:part(Bytes, Position, ?OFFSET_SIZE)
    ),
    ?assertEqual(
        chunk(5),
        binary:part(Bytes, Position + ?OFFSET_SIZE, ?DATA_CHUNK_SIZE)
    ),
    ?assertEqual([<<"0">>], filenames(unpacked_id(0), Opts)).

%% @doc A chunk whose absolute end offset is not bucket aligned reads back from
%% every byte of the weave it covers, with the same metadata from each.
%%
%% The slot such a chunk was written into is named by the offset its
%% transaction's Merkle layout gave it, and by nothing any byte of it derives:
%% below the strict data split threshold a transaction may be split anywhere, so
%% a byte says which chunk holds it only through the index.
reads_an_unaligned_chunk_test() ->
    Opts = node_opts(<<"unaligned">>, [unpacked_module(0)]),
    Weave = unaligned_weave(),
    lists:foreach(
        fun(Index) ->
            {ok, Stored} =
                resolve(<<"store">>, unaligned_request(Index, Weave), Opts),
            ?assertEqual(
                ?SHIFT + Index * ?DATA_CHUNK_SIZE,
                field(<<"absolute-end-offset">>, Stored, Opts)
            ),
            ?assertEqual(
                ?SHIFT + Index * ?DATA_CHUNK_SIZE,
                field(<<"padded-end-offset">>, Stored, Opts)
            )
        end,
        [1, 2, 3]
    ),
    lists:foreach(
        fun(Index) -> assert_reads(Index, Weave, Opts) end,
        [1, 2, 3]
    ),
    % The byte below them all belongs to the short chunk the transaction begins
    % with, which this module was never given.
    ?assertMatch(
        {error, #{ <<"message">> := <<"chunk-not-held">> }},
        resolve(<<"chunk">>, #{ <<"offset">> => ?SHIFT - 1 }, Opts)
    ).

%% @doc The first, last and a middle byte of one chunk of the unaligned weave
%% all find that chunk, and all find the same proof of it.
assert_reads(Index, Weave, Opts) ->
    Start = ?SHIFT + (Index - 1) * ?DATA_CHUNK_SIZE,
    EndOffset = Start + ?DATA_CHUNK_SIZE,
    Request = unaligned_request(Index, Weave),
    lists:foreach(
        fun(Byte) ->
            ?assertEqual(
                {ok, chunk(Index)},
                resolve(<<"chunk">>, #{ <<"offset">> => Byte }, Opts)
            ),
            {ok, Proof} =
                resolve(<<"chunk-proof">>, #{ <<"offset">> => Byte }, Opts),
            ?assertEqual(
                hb_util:encode(chunk(Index)),
                field(<<"chunk">>, Proof, Opts)
            ),
            ?assertEqual(
                hb_util:encode(chunk(Index)),
                field(<<"unpacked-chunk">>, Proof, Opts)
            ),
            ?assertEqual(
                EndOffset,
                field(<<"absolute-end-offset">>, Proof, Opts)
            ),
            ?assertEqual(
                ?DATA_CHUNK_SIZE,
                field(<<"chunk-size">>, Proof, Opts)
            ),
            ?assertEqual(
                maps:get(<<"data-path">>, Request),
                field(<<"data-path">>, Proof, Opts)
            )
        end,
        [Start, Start + ?DATA_CHUNK_SIZE div 2, EndOffset - 1]
    ).

%% @doc A chunk shorter than a slot is held in the index alone, over the bytes
%% of the weave it really covers.
%%
%% The chunk file layout is one 256 KiB slot per bucket with no size beside it,
%% so a shorter chunk has no slot -- which below the strict data split threshold
%% is any chunk a transaction ends on. A record covering the whole bucket would
%% claim the bytes of the chunks before it, and at the start of the weave it
%% would claim bytes below zero, which is not a range a record can hold at all.
stores_a_short_chunk_test() ->
    Opts = node_opts(<<"short-chunk">>, [unpacked_module(0)]),
    Weave = unaligned_weave(),
    Short = binary:part(chunk(0), 0, ?SHIFT),
    {ok, Stored} =
        resolve(
            <<"store">>,
            bounded(proof_request(Short, 0, Weave), ?UNALIGNED_SIZE, Weave),
            Opts
        ),
    ?assertEqual(?SHIFT, field(<<"absolute-end-offset">>, Stored, Opts)),
    % The record claims the hundred thousand bytes the chunk holds, and reads
    % back as the range it was written as.
    ?assertEqual(?SHIFT, synced(unpacked_id(0), Opts)),
    ?assertEqual(
        not_found,
        field(<<"ar_chunk_storage">>, records(unpacked_id(0), Opts), Opts)
    ),
    % The chunk comes back from the index, and the proof pads it to the size a
    % proof of access is taken at.
    ?assertEqual(
        {ok, Short},
        resolve(<<"chunk">>, #{ <<"offset">> => ?SHIFT - 1 }, Opts)
    ),
    {ok, Proof} = resolve(<<"chunk-proof">>, #{ <<"offset">> => 0 }, Opts),
    ?assertEqual(hb_util:encode(Short), field(<<"chunk">>, Proof, Opts)),
    ?assertEqual(?SHIFT, field(<<"chunk-size">>, Proof, Opts)),
    ?assertEqual(
        hb_util:encode(
            << Short/binary, 0:((?DATA_CHUNK_SIZE - ?SHIFT) * 8) >>),
        field(<<"unpacked-chunk">>, Proof, Opts)
    ),
    % No chunk file was written for it, and a pass over the range reads nothing.
    ?assertEqual([], filenames(unpacked_id(0), Opts)),
    ?assertEqual([], chunks(span(0, ?DATA_CHUNK_SIZE, Opts), Opts)).

%% @doc A replica-2.9 module, end to end: prepared with the entropy its chunks
%% are enciphered with, stored into, read back packed, and proved.
%%
%% This is what the whole subsystem exists for. The entropy is generated twice
%% -- once by the preparation pass and once here, from the protocol's own
%% derivation -- so what the packed bytes are checked against is the protocol
%% rather than the code that wrote them.
replica_module_test_() ->
    {timeout, 300, fun test_replica_module/0}.

test_replica_module() ->
    Opts = node_opts(<<"replica">>, [replica_module(0)]),
    Weave = weave(),
    {ok, Prepared} = resolve(<<"prepare">>, #{ <<"footprints">> => 1 }, Opts),
    ?assertEqual(1, field(<<"footprints">>, Prepared, Opts)),
    ?assertEqual(2 * ?DATA_CHUNK_SIZE, field(<<"cursor">>, Prepared, Opts)),
    ?assertEqual(false, field(<<"complete">>, Prepared, Opts)),
    % The entropy record covers the bucket that was prepared, under the packing
    % the module holds and under the record's own name.
    Bucket = #{ <<"intervals">> => 1, <<"size">> => ?DATA_CHUNK_SIZE },
    Records = records(replica_id(0), Opts),
    ?assertEqual(Bucket, field(<<"ar_chunk_storage_replica_2_9_5_entropy">>,
        Records, Opts)),
    ?assertEqual(Bucket, field(entropy_label(), Records, Opts)),
    % The slot itself now holds the entropy the protocol says that bucket's
    % chunk is enciphered with, and the module still holds no data.
    Entropy = entropy(?DATA_CHUNK_SIZE, Opts),
    ?assertEqual(Entropy, slot_bytes(replica_id(0), 0, Opts)),
    ?assertEqual(0, synced(replica_id(0), Opts)),
    ?assertEqual([], chunks(span(0, ?DATA_CHUNK_SIZE, Opts), Opts)),
    % A chunk stored into the prepared bucket comes back enciphered with it.
    {ok, Stored} = resolve(<<"store">>, store_request(0, Weave), Opts),
    ?assertEqual(replica_id(0), field(<<"module">>, Stored, Opts)),
    {ok, Packed} = resolve(<<"chunk">>, #{ <<"offset">> => 0 }, Opts),
    ?assertEqual(?DATA_CHUNK_SIZE, byte_size(Packed)),
    ?assertNotEqual(chunk(0), Packed),
    ?assertEqual(crypto:exor(chunk(0), Entropy), Packed),
    ?assertEqual(?DATA_CHUNK_SIZE, synced(replica_id(0), Opts)),
    % A pass reads the packed bytes, which is what a nonce hashes.
    ?assertEqual(
        [{?DATA_CHUNK_SIZE, Packed}],
        chunks(span(0, ?DATA_CHUNK_SIZE, Opts), Opts)
    ),
    % The proof carries both forms of the chunk, and a validator accepts it for
    % a sub-chunk of it exactly as it accepts the proof of a block header.
    {ok, Proof} = resolve(<<"chunk-proof">>, #{ <<"offset">> => 7 }, Opts),
    ?assertEqual(hb_util:encode(Packed), field(<<"chunk">>, Proof, Opts)),
    ?assertEqual(
        hb_util:encode(chunk(0)),
        field(<<"unpacked-chunk">>, Proof, Opts)
    ),
    ?assertEqual(<<"replica-2-9">>, field(<<"packing">>, Proof, Opts)),
    ?assertMatch(
        {ok, #{ <<"valid">> := true, <<"chunk-verified">> := true }},
        validated(Proof, 3, 0, Weave, Opts)
    ).

%% @doc The other order the two halves of a packed chunk may arrive in: the
%% chunk into a bucket whose entropy has not been generated, and the entropy
%% after it. The chunk waits in the slot unenciphered under the record that says
%% so, nothing serves it, and the preparation pass enciphers it in place.
store_before_prepare_test_() ->
    {timeout, 300, fun test_store_before_prepare/0}.

test_store_before_prepare() ->
    Opts = node_opts(<<"store-first">>, [replica_module(0)]),
    Weave = weave(),
    {ok, Stored} = resolve(<<"store">>, store_request(0, Weave), Opts),
    ?assertEqual(
        ?DATA_CHUNK_SIZE,
        field(<<"absolute-end-offset">>, Stored, Opts)
    ),
    ?assertEqual(chunk(0), slot_bytes(replica_id(0), 0, Opts)),
    Waiting = records(replica_id(0), Opts),
    ?assertEqual(
        #{ <<"intervals">> => 1, <<"size">> => ?DATA_CHUNK_SIZE },
        field(<<"ar_chunk_storage_replica_2_9_1_unpacked">>, Waiting, Opts)
    ),
    ?assertEqual(not_found, field(<<"ar_chunk_storage">>, Waiting, Opts)),
    ?assertEqual(0, synced(replica_id(0), Opts)),
    % Bytes no entropy has reached are not a chunk of the weave in the form this
    % module holds one, so nothing answers with them.
    ?assertMatch(
        {error, #{ <<"message">> := <<"chunk-not-held">> }},
        resolve(<<"chunk">>, #{ <<"offset">> => 0 }, Opts)
    ),
    ?assertEqual([], chunks(span(0, ?DATA_CHUNK_SIZE, Opts), Opts)),
    % The pass that reaches the bucket enciphers what is waiting in it, and
    % everything that holds for a chunk stored into a prepared bucket then
    % holds for this one.
    {ok, Prepared} = resolve(<<"prepare">>, #{ <<"footprints">> => 1 }, Opts),
    ?assertEqual(1, field(<<"footprints">>, Prepared, Opts)),
    Entropy = entropy(?DATA_CHUNK_SIZE, Opts),
    {ok, Packed} = resolve(<<"chunk">>, #{ <<"offset">> => 0 }, Opts),
    ?assertEqual(crypto:exor(chunk(0), Entropy), Packed),
    ?assertEqual(?DATA_CHUNK_SIZE, synced(replica_id(0), Opts)),
    ?assertEqual(
        [{?DATA_CHUNK_SIZE, Packed}],
        chunks(span(0, ?DATA_CHUNK_SIZE, Opts), Opts)
    ),
    {ok, Proof} = resolve(<<"chunk-proof">>, #{ <<"offset">> => 0 }, Opts),
    ?assertEqual(hb_util:encode(Packed), field(<<"chunk">>, Proof, Opts)),
    ?assertEqual(
        hb_util:encode(chunk(0)),
        field(<<"unpacked-chunk">>, Proof, Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"valid">> := true, <<"chunk-verified">> := true }},
        validated(Proof, 0, 0, Weave, Opts)
    ).

%%% Test helpers.

%% @doc Ask this node's storage device for one of its keys.
resolve(Path, Request, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave-storage@2.9">> },
        Request#{ <<"path">> => Path },
        Opts
    ).

%% @doc The weave reads the same over HTTP as it does in process.
%%
%% Every other vector resolves the device directly, which is how a miner on the
%% same node reaches it. An operator reaching it from anywhere else goes through
%% the HTTP layer and its codecs, and the two answers this device gives are
%% shaped very differently: `chunk-proof' is a message of base64url fields, and
%% `range' is a message carrying whole 256 KiB chunks as raw bytes. A codec that
%% mangled either would be invisible to a suite that never left the process.
http_round_trip_test() ->
    Opts = node_opts(<<"http">>, [unpacked_module(0)]),
    Weave = weave(),
    store_chunks([0, 1], Weave, Opts),
    Node =
        hb_http_server:start_node(
            Opts#{
                <<"force-signed">> => false,
                <<"prometheus">> => false
            }
        ),
    % The proof of one chunk, field by field against the answer in process.
    {ok, Direct} =
        resolve(<<"chunk-proof">>, #{ <<"offset">> => ?DATA_CHUNK_SIZE }, Opts),
    {ok, Served} =
        hb_http:get(
            Node,
            <<"/~arweave-storage@2.9/chunk-proof?offset=",
                (hb_util:bin(?DATA_CHUNK_SIZE))/binary>>,
            Opts
        ),
    lists:foreach(
        fun(Key) ->
            ?assertEqual(
                {Key, field(Key, Direct, Opts)},
                {Key, field(Key, Served, Opts)}
            )
        end,
        [
            <<"chunk">>,
            <<"unpacked-chunk">>,
            <<"tx-path">>,
            <<"data-path">>,
            <<"tx-root">>,
            <<"data-root">>,
            <<"absolute-end-offset">>,
            <<"chunk-size">>,
            <<"packing">>
        ]
    ),
    % A whole span, whose chunks are raw bytes rather than text.
    {ok, Range} =
        hb_http:get(
            Node,
            <<"/~arweave-storage@2.9/range?range-start=0&size=",
                (hb_util:bin(2 * ?DATA_CHUNK_SIZE))/binary>>,
            Opts
        ),
    ?assertEqual(<<"unpacked">>, field(<<"packing">>, Range, Opts)),
    ?assertEqual(
        [
            {?DATA_CHUNK_SIZE, chunk(0)},
            {2 * ?DATA_CHUNK_SIZE, chunk(1)}
        ],
        chunks(Range, Opts)
    ),
    % What the node says it holds.
    {ok, Modules} =
        hb_http:get(Node, <<"/~arweave-storage@2.9/modules">>, Opts),
    ?assertEqual(
        2 * ?DATA_CHUNK_SIZE,
        hb_util:int(hb_ao:get(<<"1/synced">>, Modules, Opts))
    ).

%% @doc Read a field of a result, so that a key a device did not answer with is
%% `not_found' in the assertion rather than a badkey in the vector.
field(Key, Message, Opts) ->
    hb_maps:get(Key, Message, not_found, Opts).

%% @doc The node message a vector runs against: a store of its own, an Arweave
%% data directory of its own, and the storage modules it names.
node_opts(Tag, Modules) ->
    node_opts(Tag, Modules, ?CHUNK_GROUP_SIZE).
node_opts(Tag, Modules, GroupSize) ->
    #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"arweave-data-dir">> => data_dir(Tag),
        <<"arweave-storage-modules">> => Modules,
        <<"arweave-chunk-group-size">> => GroupSize,
        <<"arweave-packing-workers">> => ?WORKERS
    }.

%% @doc A data directory no other vector and no other run writes into.
data_dir(Tag) ->
    hb_util:bin(
        filename:join([
            os:getenv("TMPDIR", "/tmp"),
            "hb-arweave-storage",
            <<
                Tag/binary,
                "-",
                (hb_util:encode(crypto:strong_rand_bytes(6)))/binary
            >>
        ])
    ).

%% @doc An unpacked storage module holding one bucket of the weave.
unpacked_module(Bucket) ->
    #{
        <<"bucket-size">> => ?BUCKET_SIZE,
        <<"bucket">> => Bucket,
        <<"packing">> => <<"unpacked">>
    }.

%% @doc A replica-2.9 storage module packed for the address of these vectors.
replica_module(Bucket) ->
    #{
        <<"bucket-size">> => ?BUCKET_SIZE,
        <<"bucket">> => Bucket,
        <<"packing">> => <<"replica-2-9">>,
        <<"address">> => hb_util:encode(address())
    }.

%% @doc The identifier an Arweave node names an unpacked module's directory by.
unpacked_id(Bucket) ->
    <<
        "storage_module_",
        (hb_util:bin(?BUCKET_SIZE))/binary,
        "_",
        (hb_util:bin(Bucket))/binary,
        "_unpacked"
    >>.

%% @doc The same for a replica-2.9 module, whose packing names the address it
%% is packed for in base64url.
replica_id(Bucket) ->
    <<
        "storage_module_",
        (hb_util:bin(?BUCKET_SIZE))/binary,
        "_",
        (hb_util:bin(Bucket))/binary,
        "_",
        (hb_util:encode(address()))/binary,
        ".replica.2.9"
    >>.

%% @doc The directory one module occupies, in the layout an Arweave node writes.
module_path(Id, Opts) ->
    filename:join([
        hb_util:list(hb_maps:get(<<"arweave-data-dir">>, Opts, <<>>, Opts)),
        "storage_modules",
        hb_util:list(Id)
    ]).

%% @doc The path of one chunk file of a module: a file named for the offset of
%% the first bucket it covers, under the module's own chunk directory.
chunk_file(Id, ChunkFileStart, Opts) ->
    filename:join([
        module_path(Id, Opts),
        "chunk_storage",
        hb_util:list(hb_util:bin(ChunkFileStart))
    ]).

%% @doc The names in a module's chunk directory. A module nothing has been
%% written to has no directory, and holds no names.
filenames(Id, Opts) ->
    Dir = filename:join([module_path(Id, Opts), "chunk_storage"]),
    lists:sort(
        [hb_util:bin(Name) || Name <- hb_util:ok_or(file:list_dir(Dir), [])]).

%% @doc The address the packed modules of these vectors hold their chunks for.
address() ->
    crypto:hash(sha256, <<"dev_arweave_storage address">>).

%% @doc The bytes of one chunk of the weave. No thirty-two bytes of it are the
%% same as any other, so a read at the wrong place in a chunk file cannot come
%% back looking right.
chunk(Index) ->
    <<
        <<
            (crypto:hash(sha256,
                << "dev_arweave_storage chunk ", Index:8, Part:16 >>))/binary
        >>
    ||
        Part <- lists:seq(0, (?DATA_CHUNK_SIZE div 32) - 1)
    >>.

%% @doc The Merkle trees of the synthetic weave: the tree over the chunks of its
%% only transaction, and the tree over the transactions of its only block.
weave() ->
    {DataRoot, DataTree} =
        ar_merkle:generate_tree(
            [
                {
                    ar_tx:generate_chunk_id(chunk(Index)),
                    (Index + 1) * ?DATA_CHUNK_SIZE
                }
            ||
                Index <- lists:seq(0, ?CHUNKS - 1)
            ]
        ),
    {TXRoot, TXTree} = ar_merkle:generate_tree([{DataRoot, ?WEAVE_SIZE}]),
    {DataRoot, DataTree, TXRoot, TXTree}.

%% @doc The root of the tree over the chunks of the weave's only transaction.
data_root({DataRoot, _DataTree, _TXRoot, _TXTree}) ->
    DataRoot.

%% @doc The root of the tree over the transactions of the weave's only block.
tx_root({_DataRoot, _DataTree, TXRoot, _TXTree}) ->
    TXRoot.

%% @doc The Merkle trees of a weave whose transaction begins with a chunk of a
%% hundred thousand bytes, so that no chunk after it ends on a bucket boundary.
unaligned_weave() ->
    Whole =
        [
            {
                ar_tx:generate_chunk_id(chunk(Index)),
                ?SHIFT + Index * ?DATA_CHUNK_SIZE
            }
        ||
            Index <- lists:seq(1, ?CHUNKS - 1)
        ],
    {DataRoot, DataTree} =
        ar_merkle:generate_tree(
            [
                {
                    ar_tx:generate_chunk_id(binary:part(chunk(0), 0, ?SHIFT)),
                    ?SHIFT
                }
            |
                Whole
            ]
        ),
    {TXRoot, TXTree} = ar_merkle:generate_tree([{DataRoot, ?UNALIGNED_SIZE}]),
    {DataRoot, DataTree, TXRoot, TXTree}.

%% @doc The request one chunk of the weave is stored with: the bytes, the byte
%% of the weave they were fetched for, the two paths that place them, and the
%% bounds of the block that wrote them.
store_request(Index, Weave) ->
    bounded(indexed_request(Index, Weave), ?WEAVE_SIZE, Weave).

%% @doc The same without the block bounds, which a caller may leave to this
%% node's own block index.
indexed_request(Index, Weave) ->
    proof_request(chunk(Index), Index * ?DATA_CHUNK_SIZE, Weave).

%% @doc The request one chunk of the unaligned weave is stored with. Its first
%% chunk is short, so every chunk after it begins where that one ended.
unaligned_request(Index, Weave) ->
    bounded(
        proof_request(
            chunk(Index),
            ?SHIFT + (Index - 1) * ?DATA_CHUNK_SIZE,
            Weave
        ),
        ?UNALIGNED_SIZE,
        Weave
    ).

%% @doc The bytes of one chunk, the byte of the weave they were fetched for, and
%% the two paths that place them.
proof_request(Bytes, Byte, {DataRoot, DataTree, TXRoot, TXTree}) ->
    #{
        <<"chunk">> => hb_util:encode(Bytes),
        <<"offset">> => Byte,
        <<"tx-path">> =>
            hb_util:encode(ar_merkle:generate_path(TXRoot, Byte, TXTree)),
        <<"data-path">> =>
            hb_util:encode(ar_merkle:generate_path(DataRoot, Byte, DataTree))
    }.

%% @doc Add the bounds of the block that wrote a transaction of the given size,
%% which is the whole of the weave in every vector here.
bounded(Request, TXSize, Weave) ->
    Request#{
        <<"tx-root">> => hb_util:encode(tx_root(Weave)),
        <<"block-start-offset">> => 0,
        <<"block-size">> => TXSize
    }.

%% @doc A request whose proof places the first chunk of the weave over a hundred
%% bytes, which is not the size of the chunk that hashes to its Merkle leaf.
mis_sized_request() ->
    {DataRoot, DataTree} =
        ar_merkle:generate_tree(
            [
                {ar_tx:generate_chunk_id(chunk(0)), ?MIS_SIZE},
                {
                    ar_tx:generate_chunk_id(chunk(1)),
                    ?MIS_SIZE + ?DATA_CHUNK_SIZE
                }
            ]
        ),
    {TXRoot, TXTree} =
        ar_merkle:generate_tree([{DataRoot, ?MIS_SIZE + ?DATA_CHUNK_SIZE}]),
    bounded(
        proof_request(chunk(0), 0, {DataRoot, DataTree, TXRoot, TXTree}),
        ?MIS_SIZE + ?DATA_CHUNK_SIZE,
        {DataRoot, DataTree, TXRoot, TXTree}
    ).

%% @doc Store the given chunks of the weave, asserting each landed where its own
%% Merkle paths place it.
store_chunks(Indices, Weave, Opts) ->
    lists:foreach(
        fun(Index) ->
            {ok, Result} =
                resolve(<<"store">>, store_request(Index, Weave), Opts),
            ?assertEqual(true, field(<<"stored">>, Result, Opts)),
            ?assertEqual(
                (Index + 1) * ?DATA_CHUNK_SIZE,
                field(<<"absolute-end-offset">>, Result, Opts)
            )
        end,
        Indices
    ).

%% @doc Read a span of the weave, in the packing its modules hold it in.
span(Start, Size, Opts) ->
    {ok, Result} =
        resolve(
            <<"range">>,
            #{ <<"range-start">> => Start, <<"size">> => Size },
            Opts
        ),
    Result.

%% @doc Read a span's answer as the pairs a miner walks: each chunk's absolute
%% end offset and its bytes, in the order they were answered.
chunks(Range, Opts) ->
    [
        {
            hb_util:int(field(<<"absolute-end-offset">>, Chunk, Opts)),
            field(<<"chunk">>, Chunk, Opts)
        }
    ||
        Chunk <-
            hb_util:message_to_ordered_list(
                field(<<"chunks">>, Range, Opts), Opts)
    ].

%% @doc The records one module keeps, by the name it keeps each under.
records(Id, Opts) ->
    {ok, Result} = resolve(<<"sync-record">>, #{ <<"module">> => Id }, Opts),
    field(<<"records">>, field(<<"1">>, Result, Opts), Opts).

%% @doc How many bytes of the weave one module holds. A module with no records
%% at all holds none, which is what a module nothing was written to has.
synced(Id, Opts) ->
    hb_maps:get(
        <<"size">>,
        hb_maps:get(<<"ar_data_sync">>, records(Id, Opts), #{}, Opts),
        0,
        Opts
    ).

%% @doc The name the entropy record of a replica-2.9 module is filed under: the
%% record's own name, the packing, and the address it is packed for, in
%% base64url.
entropy_label() ->
    <<
        "ar_chunk_storage_replica_2_9_5_entropy.replica_2_9_",
        (hb_util:encode(address()))/binary
    >>.

%% @doc The entropy one bucket's chunk is enciphered with, derived here from the
%% protocol's own generation rather than from anything the device wrote: the
%% thirty-two 8 MiB blobs of that bucket's footprint, and the first slice of
%% each of them combined into one chunk's worth.
entropy(BucketEndOffset, Opts) ->
    {ChunkEntropy, _Rest} =
        ar_entropy_gen:take_and_combine_entropy_slices(
            lib_arweave_packing:entropies(address(), BucketEndOffset, Opts)),
    ChunkEntropy.

%% @doc The bytes one slot of a module's first chunk file holds, read off the
%% disk rather than through the device.
slot_bytes(Id, BucketIndex, Opts) ->
    {ok, Bytes} = file:read_file(chunk_file(Id, 0, Opts)),
    binary:part(
        Bytes,
        BucketIndex * ?SLOT_SIZE + ?OFFSET_SIZE,
        ?DATA_CHUNK_SIZE
    ).

%% @doc Check a proof the device answered with the way a block validator checks
%% the proof of a block header: the packed sub-chunk a nonce addressed, the
%% unpacked chunk beside it, and the two paths, against the block that wrote the
%% chunk and the packing the partition is held in.
validated(Proof, Index, RecallOffset, Weave, Opts) ->
    Packed = hb_util:decode(field(<<"chunk">>, Proof, Opts)),
    hb_ao:resolve(
        #{
            <<"device">> => <<"arweave-spora@2.9">>,
            <<"block-start-offset">> => 0,
            <<"block-size">> => ?WEAVE_SIZE,
            <<"recall-offset">> => RecallOffset,
            <<"tx-root">> => hb_util:encode(tx_root(Weave)),
            <<"sub-chunk-index">> => Index,
            <<"packing">> =>
                #{
                    <<"format">> => <<"replica-2-9">>,
                    <<"reward-addr">> => hb_util:encode(address()),
                    <<"packing-difficulty">> =>
                        ?REPLICA_2_9_PACKING_DIFFICULTY
                },
            <<"poa">> =>
                #{
                    <<"tx-path">> => field(<<"tx-path">>, Proof, Opts),
                    <<"data-path">> => field(<<"data-path">>, Proof, Opts),
                    <<"chunk">> =>
                        hb_util:encode(
                            binary:part(
                                Packed,
                                Index * ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
                                ?COMPOSITE_PACKING_SUB_CHUNK_SIZE
                            )
                        ),
                    <<"unpacked-chunk">> =>
                        field(<<"unpacked-chunk">>, Proof, Opts)
                }
        },
        <<"validate">>,
        Opts
    ).

%% @doc Write the block that wrote the synthetic weave into this node's cache
%% and select it as the tip, which is where `store' reads a chunk's block bounds
%% from when the caller supplies none.
seed(Weave, Opts) ->
    Hash = hb_util:encode(crypto:hash(sha384, <<"dev_arweave_storage block">>)),
    {ok, IndexID} = hb_cache:write(index(Hash, Weave, Opts), Opts),
    {ok, ID} =
        hb_cache:write(
            #{
                <<"indep-hash">> => Hash,
                <<"height">> => 0,
                <<"block-index">> =>
                    {link, IndexID,
                        #{ <<"type">> => <<"link">>, <<"lazy">> => false }}
            },
            Opts
        ),
    ok = hb_cache:link(ID, Hash, Opts),
    hb_cache:link(Hash, <<"~arweave@2.9/tip">>, Opts).

%% @doc The block index of the synthetic weave: the one block that wrote it.
index(Hash, Weave, Opts) ->
    hb_util:ok(
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-block-index@2.9">> },
            #{
                <<"path">> => <<"append">>,
                <<"start-height">> => 0,
                <<"indep-hash">> => Hash,
                <<"weave-size">> => ?WEAVE_SIZE,
                <<"tx-root">> => hb_util:encode(tx_root(Weave))
            },
            Opts
        )
    ).

%% @doc Prepare every bucket of a replica-2.9 module, and then prepare it again.
%%
%% Fourteen footprints of thirty-two RandomX runs each, which is why this is not
%% a `_test'. What it establishes is that a pass ends: the cursor runs past the
%% module's range, the module reports itself prepared, and the pass that follows
%% writes nothing and says the module is complete.
prepares_a_whole_module() ->
    Opts = node_opts(<<"prepare-whole">>, [replica_module(0)]),
    Buckets = (?BUCKET_SIZE + ?REPLICA_OVERLAP) div ?DATA_CHUNK_SIZE,
    {ok, First} =
        resolve(<<"prepare">>, #{ <<"footprints">> => Buckets + 1 }, Opts),
    ?assertEqual(Buckets, field(<<"footprints">>, First, Opts)),
    ?assertEqual(true, field(<<"complete">>, First, Opts)),
    {ok, Modules} = resolve(<<"modules">>, #{}, Opts),
    ?assertEqual(
        true,
        field(<<"prepared">>, field(<<"1">>, Modules, Opts), Opts)
    ),
    Before = [file:read_file(Path) || Path <- chunk_files(replica_id(0), Opts)],
    {ok, Again} =
        resolve(<<"prepare">>, #{ <<"footprints">> => Buckets + 1 }, Opts),
    ?assertEqual(0, field(<<"footprints">>, Again, Opts)),
    ?assertEqual(true, field(<<"complete">>, Again, Opts)),
    ?assertEqual(
        Before,
        [file:read_file(Path) || Path <- chunk_files(replica_id(0), Opts)]
    ).

%% @doc Every chunk file one module holds, by ascending name. A chunk file is
%% named by the decimal offset of the first bucket it covers, so every other
%% name in the directory -- the preparation cursor beside them -- is not one.
chunk_files(Id, Opts) ->
    [
        filename:join([module_path(Id, Opts), "chunk_storage", Name])
    ||
        Name <- filenames(Id, Opts),
        numeric(Name)
    ].

%% @doc Whether a name in a chunk directory is a chunk file's, which is to say
%% a decimal integer and nothing else.
numeric(Name) ->
    case string:to_integer(hb_util:list(Name)) of
        {ChunkFileStart, []} when is_integer(ChunkFileStart) -> true;
        _ -> false
    end.
