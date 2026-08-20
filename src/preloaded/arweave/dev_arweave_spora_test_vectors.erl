%%% @doc Proof-of-access boundary vectors for Arweave 2.9, and a live probe that
%%% holds the device against the network it implements.
-module(dev_arweave_spora_test_vectors).
-export([live_reproduces_mainnet/0]).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc A rebased data path is legal only after rebase support activates.
select_rebase_ruleset_test() ->
    Opts = opts(),
    Request = rebased_request(?MERKLE_REBASE_SUPPORT_THRESHOLD),
    ?assertMatch({ok, _}, hb_ao:resolve(Request, <<"validate">>, Opts)),
    ?assertEqual(
        <<"invalid-data-path">>,
        rejection(
            rebased_request(
                ?MERKLE_REBASE_SUPPORT_THRESHOLD - ?DATA_CHUNK_SIZE
            ),
            Opts
        )
    ).

%% @doc A non-border chunk is legal only under the pre-rebase strict-borders rules.
select_strict_borders_ruleset_test() ->
    Opts = opts(),
    Start = ?STRICT_DATA_SPLIT_THRESHOLD - 150000,
    ?assertMatch(
        {ok, _},
        hb_ao:resolve(split_request(Start), <<"validate">>, Opts)
    ),
    ?assertEqual(
        <<"invalid-data-path">>,
        rejection(split_request(Start + ?DATA_CHUNK_SIZE), Opts)
    ).

%% @doc A chunk packed for a mining address is the chunk the proof of access
%% binds to that address at that offset. This is the producer side of the
%% packing: the entropy key, the slice the sub-chunk is enciphered with, and the
%% padding a short chunk is packed under all have to be the ones the validator
%% recomputes, or nothing a miner solved from would prove.
pack_binds_a_proof_test() ->
    Opts = opts(),
    Chunk = crypto:strong_rand_bytes(?DATA_CHUNK_SIZE),
    {Packed, Unpacked} = pack(Chunk, 3, Opts),
    ?assertNotEqual(
        binary:part(Unpacked, 3 * ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
            ?COMPOSITE_PACKING_SUB_CHUNK_SIZE),
        Packed
    ),
    ?assertMatch(
        {ok, #{ <<"valid">> := true, <<"chunk-verified">> := true }},
        hb_ao:resolve(
            packed_request(Chunk, Packed, Unpacked, 3), <<"validate">>, Opts)
    ).

%% @doc The tail of a short chunk is packed as zeroes, and the proof binds to
%% the chunk's own length rather than the padded one.
pack_pads_a_short_chunk_test() ->
    Opts = opts(),
    Chunk = crypto:strong_rand_bytes(100),
    {Packed, Unpacked} = pack(Chunk, 31, Opts),
    ?assertEqual(?DATA_CHUNK_SIZE, byte_size(Unpacked)),
    ?assertEqual(<< 0:((?DATA_CHUNK_SIZE - 100) * 8) >>,
        binary:part(Unpacked, 100, ?DATA_CHUNK_SIZE - 100)),
    ?assertMatch(
        {ok, #{ <<"valid">> := true, <<"chunk-verified">> := true }},
        hb_ao:resolve(
            packed_request(Chunk, Packed, Unpacked, 31), <<"validate">>, Opts)
    ).

%% @doc Only the format whose cipher is its own inverse may be packed. The
%% others would silently decipher, yielding chunks nothing on the weave accepts.
pack_refuses_a_cipher_test() ->
    ?assertEqual(
        <<"unsupported-packing">>,
        rejection(
            (pack_request(<<>>, 0))#{
                <<"packing">> =>
                    #{
                        <<"format">> => <<"composite">>,
                        <<"reward-addr">> => hb_util:encode(leaf(0)),
                        <<"packing-difficulty">> => 1
                    }
            },
            <<"pack-sub-chunk">>,
            opts()
        )
    ).

%% @doc A sub-chunk index beyond the sub-chunks of a chunk is refused rather
%% than read past the chunk.
pack_refuses_an_out_of_range_index_test() ->
    ?assertEqual(
        <<"invalid-sub-chunk-index">>,
        rejection(
            pack_request(<<>>, ?COMPOSITE_PACKING_SUB_CHUNK_COUNT),
            <<"pack-sub-chunk">>,
            opts()
        )
    ).

%% @doc Pack one sub-chunk of the first chunk of the weave, returning the packed
%% sub-chunk and the padded chunk as bytes.
pack(Chunk, Index, Opts) ->
    {ok, Result} =
        hb_ao:resolve(pack_request(Chunk, Index), <<"pack-sub-chunk">>, Opts),
    {
        hb_maps:get(<<"chunk">>, Result, not_found, Opts),
        hb_maps:get(<<"unpacked-chunk">>, Result, not_found, Opts)
    }.

pack_request(Chunk, Index) ->
    #{
        <<"device">> => <<"arweave-spora@2.9">>,
        <<"chunk">> => hb_util:encode(Chunk),
        <<"sub-chunk-index">> => Index,
        <<"absolute-end-offset">> => byte_size(Chunk),
        <<"packing">> => packing()
    }.

%% @doc A proof of the single chunk of the single transaction of the first
%% block of the weave, carrying the packed sub-chunk under test.
packed_request(Chunk, Packed, Unpacked, Index) ->
    ChunkID = ar_tx:generate_chunk_id(Chunk),
    Size = byte_size(Chunk),
    {DataRoot, DataTree} = ar_merkle:generate_tree([{ChunkID, Size}]),
    {TXRoot, TXTree} = ar_merkle:generate_tree([{DataRoot, Size}]),
    #{
        <<"device">> => <<"arweave-spora@2.9">>,
        <<"block-start-offset">> => 0,
        <<"block-size">> => Size,
        <<"recall-offset">> => 0,
        <<"tx-root">> => hb_util:encode(TXRoot),
        <<"sub-chunk-index">> => Index,
        <<"packing">> => packing(),
        <<"poa">> =>
            #{
                <<"tx-path">> =>
                    hb_util:encode(ar_merkle:generate_path(TXRoot, 0, TXTree)),
                <<"data-path">> =>
                    hb_util:encode(
                        ar_merkle:generate_path(DataRoot, 0, DataTree)),
                <<"chunk">> => hb_util:encode(Packed),
                <<"unpacked-chunk">> => hb_util:encode(Unpacked)
            }
    }.

packing() ->
    packing(hb_util:encode(leaf(0))).
packing(Address) ->
    #{
        <<"format">> => <<"replica-2-9">>,
        <<"reward-addr">> => Address,
        <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY
    }.

rebased_request(BlockStartOffset) ->
    Leaves =
        [
            [{leaf(1), ?DATA_CHUNK_SIZE}, {leaf(2), 2 * ?DATA_CHUNK_SIZE}],
            {leaf(3), ?DATA_CHUNK_SIZE}
        ],
    synthetic_request(
        BlockStartOffset,
        BlockStartOffset,
        Leaves,
        3 * ?DATA_CHUNK_SIZE,
        0,
        leaf(1)
    ).

split_request(BlockStartOffset) ->
    Leaves = [{leaf(1), 100000}, {leaf(2), 200000}, {leaf(3), 300000}],
    synthetic_request(
        BlockStartOffset,
        ?STRICT_DATA_SPLIT_THRESHOLD +
            (BlockStartOffset - (?STRICT_DATA_SPLIT_THRESHOLD - 150000)),
        Leaves,
        300000,
        150000,
        leaf(2)
    ).

synthetic_request(BlockStartOffset, RecallOffset, Leaves, TXSize, Dest, ChunkID) ->
    {DataRoot, DataTree} = ar_merkle:generate_tree(Leaves),
    {TXRoot, TXTree} = ar_merkle:generate_tree([{DataRoot, TXSize}]),
    #{
        <<"device">> => <<"arweave-spora@2.9">>,
        <<"block-start-offset">> => BlockStartOffset,
        <<"block-size">> => TXSize,
        <<"recall-offset">> => RecallOffset,
        <<"tx-root">> => hb_util:encode(TXRoot),
        <<"sub-chunk-index">> => 0,
        <<"expected-chunk-id">> => hb_util:encode(ChunkID),
        <<"packing">> =>
            #{
                <<"format">> => <<"replica-2-9">>,
                <<"reward-addr">> => hb_util:encode(leaf(0)),
                <<"packing-difficulty">> => 10
            },
        <<"poa">> =>
            #{
                <<"tx-path">> =>
                    hb_util:encode(
                        ar_merkle:generate_path(TXRoot, Dest, TXTree)
                    ),
                <<"data-path">> =>
                    hb_util:encode(
                        ar_merkle:generate_path(DataRoot, Dest, DataTree)
                    ),
                <<"chunk">> => <<>>,
                <<"unpacked-chunk">> => <<>>
            }
    }.

leaf(N) -> crypto:hash(sha256, <<"dev_arweave_spora leaf ", N:8>>).

opts() -> #{ <<"store">> => [hb_test_utils:test_store()] }.

rejection(Request, Opts) ->
    rejection(Request, <<"validate">>, Opts).
rejection(Request, Path, Opts) ->
    {error, Error} = hb_ao:resolve(Request, Path, Opts),
    hb_maps:get(<<"message">>, Error, not_found, Opts).

%% @doc The entropy this node generates for one bucket of the mainnet weave is
%% the entropy a real Arweave node enciphered that bucket's chunk with.
%%
%% This is the strongest statement the packing can make without a network. The
%% expected hash is not this node's own: it is the exclusive-or of two slots an
%% upstream Arweave node (2.9.6-alpha1) wrote to disk for the same bucket, one
%% storage module packed `replica_2_9' for the address below and one `unpacked',
%% read straight out of its `chunk_storage' files. The packing is a cipher whose
%% key is the entropy, so their difference is the entropy the network used, and
%% nothing this node computes is involved in producing it.
%%
%% The bucket is at 380 TB, above both the strict data split threshold and the
%% merkle rebase threshold, on entropy partition 105 at slice index 569 -- so
%% every term the key is derived from is a real value rather than a boundary
%% case that happens to be zero.
%%
%% A packing that differs from the network's by one byte produces blocks nothing
%% accepts, and no test that checks this node against itself can see it.
mainnet_entropy_test() ->
    Opts = opts(),
    RewardAddr =
        hb_util:native_id(<<"uaV-x-DGYWgKxGs5AsbOY9GDPWTFOz63Q9kPTal0ePA">>),
    BucketEndOffset = 380002107654144,
    ?assertEqual(
        BucketEndOffset,
        ar_chunk_storage:get_chunk_bucket_end(BucketEndOffset)
    ),
    ?assertEqual(105, ar_replica_2_9:get_entropy_partition(BucketEndOffset)),
    SliceIndex = ar_replica_2_9:get_slice_index(BucketEndOffset),
    ?assertEqual(569, SliceIndex),
    Entropies =
        [
            entropy(RewardAddr, BucketEndOffset, SubChunkStartOffset, Opts)
        ||
            SubChunkStartOffset <-
                lists:seq(
                    0,
                    ?DATA_CHUNK_SIZE - ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
                    ?COMPOSITE_PACKING_SUB_CHUNK_SIZE
                )
        ],
    ?assertEqual(?COMPOSITE_PACKING_SUB_CHUNK_COUNT, length(Entropies)),
    ?assertEqual(?REPLICA_2_9_ENTROPY_SIZE, byte_size(hd(Entropies))),
    % One chunk's entropy is the same slice of each of the thirty-two, in the
    % order the sub-chunks sit in the chunk.
    Combined =
        iolist_to_binary(
            [
                binary:part(
                    Entropy,
                    SliceIndex * ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
                    ?COMPOSITE_PACKING_SUB_CHUNK_SIZE
                )
            ||
                Entropy <- Entropies
            ]
        ),
    ?assertEqual(?DATA_CHUNK_SIZE, byte_size(Combined)),
    ?assertEqual(
        <<"Qq8U1Sp1YvZvbeD48fTrwXvGv1_T0kDB5TNL2hu9wnI">>,
        hb_util:encode(crypto:hash(sha256, Combined))
    ).

%% @doc Generate the entropy one sub-chunk of a bucket is enciphered with.
entropy(RewardAddr, BucketEndOffset, SubChunkStartOffset, Opts) ->
    {ok, Result} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"reward-addr">> => hb_util:encode(RewardAddr),
                <<"absolute-end-offset">> => BucketEndOffset,
                <<"sub-chunk-start-offset">> => SubChunkStartOffset
            },
            <<"entropy">>,
            Opts
        ),
    hb_maps:get(<<"entropy">>, Result, not_found, Opts).

%%% Live probe.

%% @doc Every rule a miner applies, held against two blocks the network already
%% accepted: one solved from two chunks, and one -- a hundred times rarer --
%% solved from a single chunk at the difficulty that rarity buys.
%%
%% This is the strongest statement the device can make about itself. A packing
%% that differs from the network's by one byte, or an entropy keyed on an offset
%% derived rather than read, produces blocks nothing accepts, and neither
%% failure is visible to a test that only checks the device against itself.
%%
%% The heights are fixed, so the probe is a fixture rather than a moving target.
live_reproduces_mainnet() ->
    Opts = opts(),
    live_two_chunk_solution(1982900, Opts),
    live_one_chunk_solution(1982682, Opts).

%% @doc A block solved from both recall ranges. Its own first hash does not meet
%% the one-chunk difficulty -- which is why it went on to read a second chunk --
%% and the hash of the two together is the solution the header declares. Its two
%% recall bytes fall on opposite sides of the strict data split threshold, so
%% both weave layouts are packed.
live_two_chunk_solution(Height, Opts) ->
    {Block, H0, Nonce, Address} = live_session(Height, Opts),
    Byte1 = live_recall_byte(Block, H0, <<"range1-start">>, Nonce, Opts),
    Byte2 = live_recall_byte(Block, H0, <<"range2-start">>, Nonce, Opts),
    ?assertEqual(live_int(<<"recall_byte">>, Block, Opts), Byte1),
    ?assertEqual(live_int(<<"recall_byte2">>, Block, Opts), Byte2),
    PoA = hb_maps:get(<<"poa">>, Block, not_found, Opts),
    PoA2 = hb_maps:get(<<"poa2">>, Block, not_found, Opts),
    {H1, _Preimage1} = live_hash(<<"h1">>, H0, Nonce, PoA, Opts),
    {H2, Preimage2} = live_hash(<<"h2">>, H0, H1, PoA2, Opts),
    ?assertEqual(hb_maps:get(<<"hash">>, Block, not_found, Opts), H2),
    ?assertEqual(hb_maps:get(<<"hash_preimage">>, Block, not_found, Opts),
        Preimage2),
    ?assertNot(live_passes(<<"h1">>, H1, Block, Opts)),
    ?assert(live_passes(<<"h2">>, H2, Block, Opts)),
    live_packs(Byte1, PoA, Address, Nonce, Opts),
    live_packs(Byte2, PoA2, Address, Nonce, Opts).

%% @doc A block solved from one chunk. Its first hash is the solution hash
%% itself, it meets the hundredfold difficulty a single chunk is held to, and
%% the header names no second recall byte at all.
live_one_chunk_solution(Height, Opts) ->
    {Block, H0, Nonce, Address} = live_session(Height, Opts),
    Byte1 = live_recall_byte(Block, H0, <<"range1-start">>, Nonce, Opts),
    ?assertEqual(live_int(<<"recall_byte">>, Block, Opts), Byte1),
    PoA = hb_maps:get(<<"poa">>, Block, not_found, Opts),
    {H1, Preimage1} = live_hash(<<"h1">>, H0, Nonce, PoA, Opts),
    ?assertEqual(hb_maps:get(<<"hash">>, Block, not_found, Opts), H1),
    ?assertEqual(hb_maps:get(<<"hash_preimage">>, Block, not_found, Opts),
        Preimage1),
    ?assert(live_passes(<<"h1">>, H1, Block, Opts)),
    live_packs(Byte1, PoA, Address, Nonce, Opts).

%% @doc The mining entropy of a block, recomputed from the step it was mined at
%% and the seed of the block below it, with the nonce and address it was mined
%% by.
live_session(Height, Opts) ->
    Block = live_block(Height, Opts),
    Parent = live_block(Height - 1, Opts),
    Info = hb_maps:get(<<"nonce_limiter_info">>, Block, not_found, Opts),
    Address = hb_maps:get(<<"reward_addr">>, Block, not_found, Opts),
    {ok, Entropy} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"nonce-limiter-output">> =>
                    hb_maps:get(<<"output">>, Info, not_found, Opts),
                <<"partition-number">> =>
                    hb_maps:get(<<"partition_number">>, Block, not_found, Opts),
                <<"seed">> =>
                    hb_maps:get(
                        <<"seed">>,
                        hb_maps:get(
                            <<"nonce_limiter_info">>, Parent, not_found, Opts),
                        not_found,
                        Opts
                    ),
                <<"reward-addr">> => Address,
                <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY
            },
            <<"h0">>,
            Opts
        ),
    {
        Block,
        hb_maps:get(<<"h0">>, Entropy, not_found, Opts),
        binary:decode_unsigned(
            hb_util:decode(hb_maps:get(<<"nonce">>, Block, not_found, Opts))),
        Address
    }.

%% @doc The byte a block's nonce recalls from one of its two ranges.
live_recall_byte(Block, H0, Range, Nonce, Opts) ->
    Info = hb_maps:get(<<"nonce_limiter_info">>, Block, not_found, Opts),
    {ok, Ranges} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"h0">> => H0,
                <<"partition-number">> =>
                    hb_maps:get(<<"partition_number">>, Block, not_found, Opts),
                <<"partition-upper-bound">> =>
                    hb_maps:get(<<"zone_upper_bound">>, Info, not_found, Opts)
            },
            <<"recall-range">>,
            Opts
        ),
    {ok, Recall} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"range-start">> =>
                    hb_maps:get(Range, Ranges, not_found, Opts),
                <<"nonce">> => Nonce,
                <<"packing-difficulty">> => ?REPLICA_2_9_PACKING_DIFFICULTY
            },
            <<"recall-byte">>,
            Opts
        ),
    hb_util:int(hb_maps:get(<<"recall-byte">>, Recall, not_found, Opts)).

%% @doc One of the two solution hashes of a block, over the packed sub-chunk the
%% proof carries, with the preimage it was taken over.
live_hash(<<"h1">> = Key, H0, Nonce, PoA, Opts) ->
    live_hash(
        Key,
        #{ <<"h0">> => H0, <<"nonce">> => Nonce },
        PoA,
        Opts
    );
live_hash(<<"h2">> = Key, H0, H1, PoA, Opts) ->
    live_hash(Key, #{ <<"h0">> => H0, <<"h1">> => H1 }, PoA, Opts).
live_hash(Key, Request, PoA, Opts) ->
    {ok, Result} =
        hb_ao:resolve(
            Request#{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"chunk">> => hb_maps:get(<<"chunk">>, PoA, not_found, Opts)
            },
            Key,
            Opts
        ),
    {
        hb_maps:get(<<"hash">>, Result, not_found, Opts),
        hb_maps:get(<<"preimage">>, Result, not_found, Opts)
    }.

%% @doc Whether a solution hash meets the difficulty its kind is held to.
live_passes(Kind, Hash, Block, Opts) ->
    Diff = live_int(<<"diff">>, Block, Opts),
    Height = live_int(<<"height">>, Block, Opts),
    Pair = {ar_difficulty:poa1_diff(Diff, Height), Diff},
    live_passes(Kind, hb_util:decode(Hash), Pair).
live_passes(<<"h1">>, Hash, Pair) ->
    ar_node_utils:h1_passes_diff_check(
        Hash, Pair, ?REPLICA_2_9_PACKING_DIFFICULTY);
live_passes(<<"h2">>, Hash, Pair) ->
    ar_node_utils:h2_passes_diff_check(
        Hash, Pair, ?REPLICA_2_9_PACKING_DIFFICULTY).

%% @doc Pack the unpacked chunk a proof carries and require the result to be the
%% packed sub-chunk it carries beside it, at the offset the weave reports for
%% the chunk holding the recall byte.
live_packs(Byte, PoA, Address, Nonce, Opts) ->
    {ok, Proof} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"chunk-proof">>, <<"offset">> => Byte },
            Opts
        ),
    {ok, Packed} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-spora@2.9">>,
                <<"chunk">> =>
                    hb_maps:get(<<"unpacked_chunk">>, PoA, not_found, Opts),
                <<"sub-chunk-index">> =>
                    ar_block:get_sub_chunk_index(
                        ?REPLICA_2_9_PACKING_DIFFICULTY, Nonce),
                <<"absolute-end-offset">> =>
                    hb_maps:get(
                        <<"absolute-end-offset">>, Proof, not_found, Opts),
                <<"packing">> => packing(Address)
            },
            <<"pack-sub-chunk">>,
            Opts
        ),
    ?assertEqual(
        hb_util:decode(hb_maps:get(<<"chunk">>, PoA, not_found, Opts)),
        hb_maps:get(<<"chunk">>, Packed, not_found, Opts)).

%% @doc Read an integer field of a block message.
live_int(Key, Block, Opts) ->
    hb_util:int(hb_maps:get(Key, Block, not_found, Opts)).

%% @doc Read a block from the network by height.
live_block(Height, Opts) ->
    {ok, Block} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{ <<"path">> => <<"block">>, <<"block">> => Height },
            Opts
        ),
    Block.
