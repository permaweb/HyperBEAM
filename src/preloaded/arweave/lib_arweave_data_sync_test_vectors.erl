%%% @doc Vectors for the bounded pass that fills a storage module from peers.
%%%
%%% Every vector runs a real pass over a real storage module on a temporary
%%% directory: real chunk files, a real index, a real sync record and a real
%%% block index the proofs are checked against. Nothing is stubbed but the
%%% network, and the network is replaced by a weave rather than by a recording:
%%% one transaction of eight chunks in the first block of a synthetic weave,
%%% answering `chunk-proof' with the two Merkle paths that really do place each
%%% chunk where it says. The vectors are told which of those chunks the network
%%% will answer for, which is how a hole in the weave is expressed.
%%%
%%% The live vector at the bottom does the same against the real network, into
%%% a module cut out of one real mainnet block.
-module(lib_arweave_data_sync_test_vectors).
-export([live_syncs_mainnet_weave/0]).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The weave the vectors sync from: one transaction of eight chunks, in the
%%% first block.
-define(CHUNKS, 8).
-define(WEAVE_SIZE, (?CHUNKS * ?DATA_CHUNK_SIZE)).

%%% The bucket of the weave the module under test holds, at one chunk to the
%%% bucket. A bucket in the middle of the weave has chunks on both sides of it,
%%% so a pass that wandered past either end of its module would be visible.
-define(BUCKET, 1).

%%% The file this node keeps a module's syncing place in.
-define(CURSOR, "hyperbeam_sync_cursor").

%%% The block of the real weave the live vector syncs out of. Fixed, so the
%%% vector is a fixture rather than a moving target, and large enough that a
%%% module cut two chunks into it still lies within it.
-define(LIVE_HEIGHT, 1982894).

%% @doc A pass over a module holding nothing fetches the chunks its range
%% begins with and writes them, and the module's record grows by exactly the
%% bytes of the chunks that were written.
first_pass_fills_the_module_test() ->
    {Module, Opts} = setup(<<"data-sync-first">>),
    {ok, Report} = lib_arweave_data_sync:sync(Module, 3, Opts),
    ?assertEqual(3, field(<<"chunks">>, Report, Opts)),
    ?assertEqual(3, field(<<"attempted">>, Report, Opts)),
    ?assertEqual(false, field(<<"wrapped">>, Report, Opts)),
    ?assertEqual(3 * ?DATA_CHUNK_SIZE, field(<<"synced">>, Report, Opts)),
    ?assertEqual(3 * ?DATA_CHUNK_SIZE, synced(Module, Opts)),
    % The bytes the record claims are the bytes of the three chunks the module
    % begins with, and no others.
    ?assert(recorded(Module, byte(?BUCKET) + 1, Opts)),
    ?assert(recorded(Module, byte(?BUCKET + 3), Opts)),
    ?assertNot(recorded(Module, byte(?BUCKET + 3) + 1, Opts)),
    ?assertEqual(byte(?BUCKET + 3), field(<<"cursor">>, Report, Opts)),
    ?assertEqual(byte(?BUCKET + 3), lib_arweave_data_sync:cursor(Module, Opts)).

%% @doc A second pass begins where the first stopped. It attempts only the
%% chunks it was given, and each of them is a chunk the module did not hold --
%% so the record grows by the whole of the second pass as well as the first.
second_pass_continues_test() ->
    {Module, Opts} = setup(<<"data-sync-second">>),
    {ok, First} = lib_arweave_data_sync:sync(Module, 3, Opts),
    ?assertEqual(3, field(<<"chunks">>, First, Opts)),
    {ok, Second} = lib_arweave_data_sync:sync(Module, 2, Opts),
    ?assertEqual(2, field(<<"chunks">>, Second, Opts)),
    ?assertEqual(2, field(<<"attempted">>, Second, Opts)),
    ?assertEqual(5 * ?DATA_CHUNK_SIZE, field(<<"synced">>, Second, Opts)),
    ?assertEqual(byte(?BUCKET + 5), field(<<"cursor">>, Second, Opts)),
    % A pass that had begun again would have found the first three chunks
    % already recorded and skipped past them, reaching the same place; it is
    % the record of the fifth chunk that says the second pass took only two
    % more.
    ?assert(recorded(Module, byte(?BUCKET + 5), Opts)),
    ?assertNot(recorded(Module, byte(?BUCKET + 5) + 1, Opts)).

%% @doc A byte no peer answers for is a hole the pass steps over. It is
%% attempted and not stored, the cursor moves past it, and the record does not
%% claim its bytes -- so a pass that comes back round asks for it again.
missing_chunk_is_stepped_over_test() ->
    {Module, Opts} = setup(<<"data-sync-hole">>, held_without(?BUCKET + 2)),
    {ok, Report} = lib_arweave_data_sync:sync(Module, 3, Opts),
    ?assertEqual(2, field(<<"chunks">>, Report, Opts)),
    ?assertEqual(3, field(<<"attempted">>, Report, Opts)),
    ?assertEqual(2 * ?DATA_CHUNK_SIZE, field(<<"synced">>, Report, Opts)),
    ?assertEqual(byte(?BUCKET + 3), field(<<"cursor">>, Report, Opts)),
    % The hole is the third chunk of the module, and every byte of it is
    % unclaimed.
    ?assertNot(recorded(Module, byte(?BUCKET + 2) + 1, Opts)),
    ?assertNot(recorded(Module, byte(?BUCKET + 3), Opts)),
    ?assert(recorded(Module, byte(?BUCKET + 2), Opts)).

%% @doc A cursor at the end of a module's range wraps to its start, and the
%% pass says so. The pass that follows the wrap asks again for the bytes the
%% earlier passes stepped over, and nothing for the bytes already held.
cursor_wraps_at_the_range_end_test() ->
    {Module, Opts} = setup(<<"data-sync-wrap">>, held_without(?BUCKET + 2)),
    {ok, First} = lib_arweave_data_sync:sync(Module, 3, Opts),
    ?assertEqual(2, field(<<"chunks">>, First, Opts)),
    {_Start, End} = lib_arweave_storage:range(Module),
    ok = lib_arweave_data_sync:advance(Module, End, Opts),
    {ok, Wrapped} = lib_arweave_data_sync:sync(Module, 3, Opts),
    ?assertEqual(true, field(<<"wrapped">>, Wrapped, Opts)),
    ?assertEqual(0, field(<<"chunks">>, Wrapped, Opts)),
    ?assertEqual(0, field(<<"attempted">>, Wrapped, Opts)),
    ?assertEqual(byte(?BUCKET), field(<<"cursor">>, Wrapped, Opts)),
    ?assertEqual(byte(?BUCKET), lib_arweave_data_sync:cursor(Module, Opts)),
    % Come back round with the hole filled: the pass skips the two chunks the
    % record already holds and asks for the one it does not.
    {ok, Retry} = lib_arweave_data_sync:sync(Module, 1, whole(Opts)),
    ?assertEqual(1, field(<<"chunks">>, Retry, Opts)),
    ?assertEqual(byte(?BUCKET + 3), field(<<"cursor">>, Retry, Opts)),
    ?assert(recorded(Module, byte(?BUCKET + 3), whole(Opts))).

%% @doc The cursor is this node's own file, kept where this node's own files
%% go, and a pass that did not move it does not write it again.
%%
%% Two properties an operator feels rather than a caller does. A data directory
%% an Arweave node filled is not a thing to leave our files inside: a name in a
%% module's chunk directory is one `ar_chunk_storage' reads as a chunk file, so
%% the cursor goes in the module's own directory beside it. And a module
%% holding the whole of its range wraps to the start of it on every pass, while
%% a pass runs once a second -- so a place that has not moved is not rewritten.
cursor_is_written_outside_the_chunk_directory_test() ->
    {Module, Opts} = setup(<<"data-sync-cursor">>),
    {Start, End} = lib_arweave_storage:range(Module),
    Path =
        filename:join(
            lib_arweave_storage:module_path(Module, Opts), ?CURSOR),
    ok = lib_arweave_data_sync:advance(Module, End, Opts),
    ?assert(filelib:is_regular(Path)),
    ?assertEqual(End, lib_arweave_data_sync:cursor(Module, Opts)),
    ?assertNot(
        lists:member(
            ?CURSOR,
            hb_util:ok_or(
                file:list_dir(lib_arweave_storage:chunk_dir(Module, Opts)),
                []
            )
        )
    ),
    % With the file gone the cursor reads as the start of the range, which is
    % where a wrapped pass ends: nothing to record, and nothing recorded.
    ok = file:delete(Path),
    ok = lib_arweave_data_sync:advance(Module, Start, Opts),
    ?assertEqual(false, filelib:is_regular(Path)).

%% @doc A peer answering in a packing this node did not ask for is refused by
%% name. Nothing is written and nothing is coerced: bytes in a packing a module
%% does not hold would be bytes nothing on the weave accepts.
unsupported_packing_is_refused_test() ->
    {Module, Opts} = setup(<<"data-sync-packing">>, held(), <<"replica-2-9">>),
    {error, Error} = lib_arweave_data_sync:sync(Module, 1, Opts),
    ?assertEqual(<<"unsupported-packing">>, field(<<"message">>, Error, Opts)),
    ?assertNotEqual(
        nomatch,
        binary:match(field(<<"detail">>, Error, Opts), <<"replica-2-9">>)
    ),
    ?assertEqual(0, synced(Module, Opts)).

%% @doc `missing' reports the spans of a module's range it does not hold. Before
%% a pass that is the whole range; after one that stepped over a hole it is the
%% hole and the tail beyond what was synced.
missing_reports_the_gaps_test() ->
    {Module, Opts} = setup(<<"data-sync-missing">>, held_without(?BUCKET + 2)),
    {Start, End} = lib_arweave_storage:range(Module),
    ?assertEqual(
        {ok, [#{ <<"start">> => Start, <<"end">> => End }]},
        lib_arweave_data_sync:missing(Module, 3, Opts)
    ),
    {ok, Report} = lib_arweave_data_sync:sync(Module, 4, Opts),
    ?assertEqual(3, field(<<"chunks">>, Report, Opts)),
    ?assertEqual(4, field(<<"attempted">>, Report, Opts)),
    ?assertEqual(
        {ok,
            [
                #{
                    <<"start">> => byte(?BUCKET + 2),
                    <<"end">> => byte(?BUCKET + 3)
                },
                #{ <<"start">> => byte(?BUCKET + 4), <<"end">> => End }
            ]
        },
        lib_arweave_data_sync:missing(Module, 3, Opts)
    ),
    % The limit is a limit: a caller asking for one gap is told about the first.
    ?assertEqual(
        {ok,
            [
                #{
                    <<"start">> => byte(?BUCKET + 2),
                    <<"end">> => byte(?BUCKET + 3)
                }
            ]
        },
        lib_arweave_data_sync:missing(Module, 1, Opts)
    ).

%%% The weave the vectors sync from.

%% @doc Build a node holding one storage module over a synthetic weave, with
%% the block that wrote that weave in its own validated block index.
setup(Tag) ->
    setup(Tag, held()).
setup(Tag, Held) ->
    setup(Tag, Held, <<"unpacked">>).
setup(Tag, Held, Format) ->
    Opts = (node_opts(Tag))#{ <<"arweave-weave">> => weave(Held, Format) },
    ok = seed(Opts),
    {module(Opts), Opts}.

%% @doc The node message a vector runs against: an isolated store, an isolated
%% Arweave data directory, and one unpacked storage module of one chunk.
node_opts(Tag) ->
    Store = hb_test_utils:test_store(hb_store_lmdb, Tag),
    Name =
        hb_util:bin(
            filename:absname(hb_util:list(maps:get(<<"name">>, Store)))),
    #{
        <<"store">> => [Store#{ <<"name">> => Name }],
        <<"arweave-data-dir">> => << Name/binary, "-arweave" >>,
        <<"arweave-storage-modules">> =>
            [
                #{
                    <<"bucket-size">> => ?DATA_CHUNK_SIZE,
                    <<"bucket">> => ?BUCKET,
                    <<"packing">> => <<"unpacked">>
                }
            ]
    }.

%% @doc The one storage module a vector's node holds.
module(Opts) ->
    hd(lib_arweave_storage:modules(Opts)).

%% @doc The same node with a weave holding every chunk, for the pass that comes
%% back round to a hole the network has since filled.
whole(Opts) ->
    Opts#{ <<"arweave-weave">> => weave(held(), <<"unpacked">>) }.

%% @doc Write the block that wrote the synthetic weave, and select it as this
%% node's tip. `~arweave-storage@2.9/store' reads a chunk's block bounds from
%% the index the tip carries, so without this there is nothing to check a
%% peer's proof against.
seed(Opts) ->
    {ok, IndexID} = hb_cache:write(index(Opts), Opts),
    Hash = hb_util:encode(crypto:hash(sha384, <<"data-sync block">>)),
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
index(Opts) ->
    hb_util:ok(
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-block-index@2.9">> },
            #{
                <<"path">> => <<"append">>,
                <<"start-height">> => 0,
                <<"indep-hash">> =>
                    hb_util:encode(crypto:hash(sha384, <<"data-sync block">>)),
                <<"weave-size">> => ?WEAVE_SIZE,
                <<"tx-root">> => hb_util:encode(element(1, tx_tree()))
            },
            Opts
        )
    ).

%% @doc A weave source: one transaction of eight chunks in the first block,
%% answering `chunk-proof' for any byte of a chunk it holds exactly as a
%% gateway answers `GET /chunk/<offset>'. A byte of any other chunk is answered
%% as a hole, which is what a peer that has not synced that part of the weave
%% returns.
weave(Held, Format) ->
    #{
        <<"device">> =>
            #{
                chunk_proof =>
                    fun(_Base, Req, Opts) ->
                        chunk_proof(
                            Held,
                            Format,
                            hb_util:int(
                                hb_maps:get(<<"offset">>, Req, 0, Opts))
                        )
                    end
            }
    }.

chunk_proof(Held, Format, Offset) ->
    Chunk = Offset div ?DATA_CHUNK_SIZE,
    chunk_proof(lists:member(Chunk, Held), Format, Chunk, Offset).
chunk_proof(false, _Format, _Chunk, _Offset) ->
    {error,
        #{
            <<"status">> => 404,
            <<"message">> => <<"chunk-not-found">>,
            <<"detail">> => <<"This peer holds no chunk at that offset.">>
        }
    };
chunk_proof(true, Format, Chunk, Offset) ->
    {DataRoot, DataTree} = data_tree(),
    {TXRoot, TXTree} = tx_tree(),
    {ok,
        #{
            <<"chunk">> => hb_util:encode(chunk(Chunk)),
            <<"chunk-size">> => ?DATA_CHUNK_SIZE,
            <<"absolute-end-offset">> => (Chunk + 1) * ?DATA_CHUNK_SIZE,
            <<"packing">> => Format,
            <<"tx-path">> =>
                hb_util:encode(ar_merkle:generate_path(TXRoot, Offset, TXTree)),
            <<"data-path">> =>
                hb_util:encode(
                    ar_merkle:generate_path(DataRoot, Offset, DataTree))
        }
    }.

%% @doc Every chunk of the weave, and every chunk but one.
held() ->
    lists:seq(0, ?CHUNKS - 1).
held_without(Chunk) ->
    lists:delete(Chunk, held()).

%% @doc The Merkle tree over the chunks of the weave's only transaction.
data_tree() ->
    ar_merkle:generate_tree(
        [
            {
                ar_tx:generate_chunk_id(chunk(Chunk)),
                (Chunk + 1) * ?DATA_CHUNK_SIZE
            }
        ||
            Chunk <- lists:seq(0, ?CHUNKS - 1)
        ]
    ).

%% @doc The Merkle tree over the transactions of the weave's only block.
tx_tree() ->
    {DataRoot, _DataTree} = data_tree(),
    ar_merkle:generate_tree([{DataRoot, ?WEAVE_SIZE}]).

%% @doc The bytes of one chunk of the weave.
chunk(Chunk) ->
    binary:copy(
        crypto:hash(sha256, << "data-sync chunk ", Chunk:8 >>),
        ?DATA_CHUNK_SIZE div 32
    ).

%%% Reading what a pass did.

%% @doc The first byte of a bucket of the weave, in the numbering a chunk is
%% asked for by: the byte a chunk begins at is the end offset of the one below.
byte(Bucket) ->
    Bucket * ?DATA_CHUNK_SIZE.

%% @doc How many bytes a module's synced record claims.
synced(Module, Opts) ->
    {ok, Records} = lib_arweave_sync_record:load(Module, Opts),
    lib_arweave_sync_record:size(Records, ar_data_sync).

%% @doc Whether a module's synced record claims one offset. Offsets are
%% 1-based here, as they are in the record itself.
recorded(Module, Offset, Opts) ->
    {ok, Records} = lib_arweave_sync_record:load(Module, Opts),
    lib_arweave_sync_record:is_recorded(Records, ar_data_sync, Offset).

%% @doc Read a field of a result, so that a key a pass did not answer with is
%% `not_found' in the assertion rather than a badkey in the vector.
field(Key, Message, Opts) ->
    hb_maps:get(Key, Message, not_found, Opts).

%%% Live probe.

%% @doc Sync real chunks out of the real weave, and prove what was written.
%%
%% The synthetic vectors hold the pass against a weave this file built, so they
%% cannot see a peer protocol detail this node has wrong: the offset a chunk is
%% asked for by, the bucket a proof is seeked at, or the shape of the answer.
%% This one runs the same pass against the network, into a storage module cut
%% out of one real mainnet block, and then reads back what it wrote and checks
%% the proof of it the way a block validator would.
%%
%% The module is cut two chunks into the block so that every byte of its range
%% lies inside the block, which is what makes the two-entry index below a true
%% statement about the weave rather than a convenient one.
live_syncs_mainnet_weave() ->
    Opts = live_opts(),
    Module = module(Opts),
    {ok, Report} = lib_arweave_data_sync:sync(Module, 2, Opts),
    ?assertEqual(2, field(<<"attempted">>, Report, Opts)),
    ?assertEqual(2, field(<<"chunks">>, Report, Opts)),
    ?assert(field(<<"synced">>, Report, Opts) > 0),
    live_proves(Module, live_start(Opts), Opts).

%% @doc A node holding one unpacked storage module over a bucket of the real
%% weave, with the two blocks that bound it in its block index.
live_opts() ->
    Base = node_opts(<<"data-sync-live">>),
    Bucket = live_bucket(Base),
    Opts =
        Base#{
            <<"arweave-storage-modules">> =>
                [
                    #{
                        <<"bucket-size">> => ?DATA_CHUNK_SIZE,
                        <<"bucket">> => Bucket,
                        <<"packing">> => <<"unpacked">>
                    }
                ]
        },
    ok = live_seed(Opts),
    Opts.

%% @doc The bucket the live module holds: two chunks into the block, so that
%% the first byte the pass asks for is one the block really wrote.
live_bucket(Opts) ->
    (live_int(<<"weave_size">>, live_block(?LIVE_HEIGHT - 1, Opts), Opts)
        div ?DATA_CHUNK_SIZE) + 2.

%% @doc The first byte of the live module's range.
live_start(Opts) ->
    {Start, _End} = lib_arweave_storage:range(module(Opts)),
    Start.

%% @doc Write the two real blocks bounding the live module's range into a block
%% index, and select the newer of them as this node's tip. Two entries, because
%% one block's bounds are the weave size of the block below it and the weave
%% size of the block itself.
live_seed(Opts) ->
    Below = live_block(?LIVE_HEIGHT - 1, Opts),
    Block = live_block(?LIVE_HEIGHT, Opts),
    {ok, IndexID} = hb_cache:write(live_index(Below, Block, Opts), Opts),
    Hash = live_field(<<"indep_hash">>, Block, Opts),
    {ok, ID} =
        hb_cache:write(
            #{
                <<"indep-hash">> => Hash,
                <<"height">> => ?LIVE_HEIGHT,
                <<"block-index">> =>
                    {link, IndexID,
                        #{ <<"type">> => <<"link">>, <<"lazy">> => false }}
            },
            Opts
        ),
    ok = hb_cache:link(ID, Hash, Opts),
    hb_cache:link(Hash, <<"~arweave@2.9/tip">>, Opts).

live_index(Below, Block, Opts) ->
    hb_util:ok(
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-block-index@2.9">> },
            #{
                <<"path">> => <<"append">>,
                <<"start-height">> => 0,
                <<"entries">> =>
                    hb_util:list_to_numbered_message(
                        [live_entry(Below, Opts), live_entry(Block, Opts)])
            },
            Opts
        )
    ).

live_entry(Block, Opts) ->
    #{
        <<"indep-hash">> => live_field(<<"indep_hash">>, Block, Opts),
        <<"weave-size">> => live_int(<<"weave_size">>, Block, Opts),
        <<"tx-root">> => live_field(<<"tx_root">>, Block, Opts)
    }.

%% @doc Read back a chunk the pass wrote and check the proof of it the way a
%% block validator checks the proof a solution carries: the two paths against
%% the tx root of the block that wrote the byte, and the bytes on this node's
%% disk against the chunk identifier the data path resolves to.
%%
%% The identifier comes from the stored bytes, so the two halves of the check
%% meet: the paths say which chunk of the weave that byte belongs to, and the
%% hash says the module is holding that chunk and not another.
live_proves(Module, Byte, Opts) ->
    {ok, Proof} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave-storage@2.9">> },
            #{ <<"path">> => <<"chunk-proof">>, <<"offset">> => Byte },
            Opts
        ),
    Unpacked = hb_util:decode(field(<<"unpacked-chunk">>, Proof, Opts)),
    Size = hb_util:int(field(<<"chunk-size">>, Proof, Opts)),
    Block = live_block(?LIVE_HEIGHT, Opts),
    BlockStart =
        live_int(<<"weave_size">>, live_block(?LIVE_HEIGHT - 1, Opts), Opts),
    ?assertMatch(
        {ok, #{ <<"valid">> := true }},
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"block-start-offset">> => BlockStart,
                <<"block-size">> =>
                    live_int(<<"weave_size">>, Block, Opts) - BlockStart,
                <<"recall-offset">> =>
                    ar_chunk_storage:get_chunk_seek_offset(Byte + 1) - 1,
                <<"tx-root">> => live_field(<<"tx_root">>, Block, Opts),
                <<"sub-chunk-index">> => 0,
                <<"expected-chunk-id">> =>
                    hb_util:encode(
                        ar_tx:generate_chunk_id(
                            binary:part(Unpacked, 0, Size))),
                <<"poa">> =>
                    #{
                        <<"tx-path">> => field(<<"tx-path">>, Proof, Opts),
                        <<"data-path">> => field(<<"data-path">>, Proof, Opts),
                        <<"chunk">> => <<>>
                    }
            },
            <<"validate">>,
            Opts
        )
    ),
    % The chunk was placed inside the block whose bounds proved it, at the
    % offset the transaction's own Merkle layout puts it at.
    EndOffset = hb_util:int(field(<<"absolute-end-offset">>, Proof, Opts)),
    ?assert(EndOffset > BlockStart),
    ?assert(EndOffset =< live_int(<<"weave_size">>, Block, Opts)),
    ?assertEqual(2 * ?DATA_CHUNK_SIZE, synced(Module, Opts)).

%% @doc Read a block from the network by height.
live_block(Height, Opts) ->
    {ok, Block} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"block">>, <<"block">> => Height },
            Opts
        ),
    Block.

live_field(Key, Block, Opts) ->
    hb_maps:get(Key, Block, not_found, Opts).

live_int(Key, Block, Opts) ->
    hb_util:int(live_field(Key, Block, Opts)).
