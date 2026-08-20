%%% @doc An AO-Core interface to Arweave's proof of access: the storage proof a
%%% block carries to show that whoever mined it held the chunk the protocol
%%% recalled, in the packed form their mining address obliges them to hold.
%%%
%%% The device owns proof-of-access semantics and packing. It delegates every
%%% Merkle walk to `~arweave-merkle@2.9', which knows nothing of blocks, and is
%%% in turn driven by `~arweave-block@2.9', which derives the recall byte from
%%% the block header and the block bounds from the weave index. Nothing here
%%% reads a block field: a proof is checked against offsets and roots the caller
%%% supplies, and the caller is responsible for having derived them honestly.
%%%
%%% Two costs shape the design. A replica-2.9 proof needs a whole 8 MiB entropy
%%% blob to consume 8 KiB of it, and every recall byte keys a different blob --
%%% so entropy is generated on demand and never cached. The RandomX state the
%%% blob is generated from costs a second to build and holds over a gibibyte, so
%%% it is built once per variant, owned by an `hb_name' singleton, and shared.
-module(dev_arweave_spora).
-implements(<<"arweave-spora@2.9">>).
-export([info/1, validate/3, recall_range/3, recall_byte/3, h0/3, h1/3, h2/3]).
-export([unpack/3, unpack_sub_chunk/3, entropy/3]).
-include("include/hb.hrl").
-include("include/ar_consensus.hrl").

%% @doc Export only the proof-of-access operations, leaving message manipulation
%% to `message@1.0'.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Validate a proof of access. `poa' must prove that the chunk containing
%% `recall-offset' belongs to the block spanning `block-size' bytes from
%% `block-start-offset' under `tx-root', and the packed chunk it carries must
%% unpack to that chunk under `packing'.
%%
%% Returns the id of the chunk the proofs resolved to, so that a caller holding
%% several proofs of the same solution can bind them to one another.
%%
%% An `expected-chunk-id' short-circuits the unpacking: the proofs are still
%% walked, but the packed chunk is not touched. It exists for a caller that has
%% already unpacked this chunk under this packing and is re-checking the proofs
%% against a different block header. A caller that has not done so must not
%% supply it.
validate(Base, Req, Opts) ->
    PoA = required(<<"poa">>, Base, Req, Opts),
    TXPath = field(<<"tx-path">>, PoA, Opts),
    DataPath = field(<<"data-path">>, PoA, Opts),
    % The chunks are decoded once, here: they are the two large elements of a
    % proof, and both the size check and the packing need their bytes.
    Chunk = hb_util:decode(field(<<"chunk">>, PoA, Opts)),
    UnpackedChunk = hb_util:decode(optional_field(<<"unpacked-chunk">>, PoA, Opts)),
    TXRoot = required(<<"tx-root">>, Base, Req, Opts),
    BlockStartOffset = hb_util:int(required(<<"block-start-offset">>, Base, Req, Opts)),
    BlockSize = hb_util:int(required(<<"block-size">>, Base, Req, Opts)),
    RecallOffset = hb_util:int(required(<<"recall-offset">>, Base, Req, Opts)),
    SubChunkIndex = hb_util:int(required(<<"sub-chunk-index">>, Base, Req, Opts)),
    SeekOffset = recall_bucket_offset(RecallOffset, BlockStartOffset),
    maybe
        ok ?= proof_size(TXPath, DataPath, Chunk, UnpackedChunk),
        {ok, Packing} ?= packing(required(<<"packing">>, Base, Req, Opts), Opts),
        {ok, DataRoot, TXStartOffset, TXEndOffset} ?=
            tx_path(TXRoot, TXPath, SeekOffset, BlockSize, Opts),
        {ok, ChunkID, ChunkStartOffset, ChunkEndOffset} ?=
            data_path(
                DataRoot,
                DataPath,
                SeekOffset - TXStartOffset,
                TXEndOffset - TXStartOffset,
                data_path_ruleset(BlockStartOffset),
                Opts
            ),
        Leaf =
            #{
                <<"chunk-id">> => ChunkID,
                <<"chunk-size">> => ChunkEndOffset - ChunkStartOffset,
                <<"absolute-end-offset">> =>
                    BlockStartOffset + TXStartOffset + ChunkEndOffset,
                <<"sub-chunk-start-offset">> =>
                    SubChunkIndex * ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
                <<"tx-root">> => hb_util:decode(TXRoot)
            },
        Expected = get_first(<<"expected-chunk-id">>, Base, Req, [], Opts),
        ok ?= chunk(Expected, Packing, Leaf, Chunk, UnpackedChunk, Opts),
        {ok,
            #{
                <<"valid">> => true,
                <<"chunk-id">> => ChunkID,
                % Whether the packed chunk itself was unpacked and bound to the
                % Merkle leaf, or the caller's own `expected-chunk-id' was
                % compared against it instead. The second is a strictly weaker
                % answer -- the proof's chunk bytes are never looked at -- and a
                % caller that cannot tell the two apart has no way to know it
                % asserted the thing it was asking about.
                <<"chunk-verified">> => Expected == []
            }
        }
    end.

%% @doc Return the start offsets of the two recall ranges a solution may draw
%% from: the first inside the miner's own partition, the second anywhere in the
%% weave below the partition upper bound.
recall_range(Base, Req, Opts) ->
    {Range1Start, Range2Start} =
        ar_block:get_recall_range(
            hb_util:decode(required(<<"h0">>, Base, Req, Opts)),
            hb_util:int(required(<<"partition-number">>, Base, Req, Opts)),
            hb_util:int(required(<<"partition-upper-bound">>, Base, Req, Opts))
        ),
    {ok,
        #{
            <<"range1-start">> => Range1Start,
            <<"range2-start">> => Range2Start
        }
    }.

%% @doc Return the byte a nonce recalls from a range, and the sub-chunk of that
%% chunk it points at. At packing difficulty 0 each nonce steps a whole chunk
%% and there is no sub-chunk, which the protocol denotes with `-1'.
recall_byte(Base, Req, Opts) ->
    RangeStart = hb_util:int(required(<<"range-start">>, Base, Req, Opts)),
    Nonce = hb_util:int(required(<<"nonce">>, Base, Req, Opts)),
    PackingDifficulty = hb_util:int(required(<<"packing-difficulty">>, Base, Req, Opts)),
    {ok,
        #{
            <<"recall-byte">> =>
                ar_block:get_recall_byte(RangeStart, Nonce, PackingDifficulty),
            <<"sub-chunk-index">> =>
                ar_block:get_sub_chunk_index(PackingDifficulty, Nonce)
        }
    }.

%% @doc Compute H0, the entropy the two recall ranges are chosen from. One
%% RandomX hash: rx512 at packing difficulty 0, rx4096 at every difficulty above
%% it -- which is every post-2.9 block.
h0(Base, Req, Opts) ->
    PackingDifficulty = hb_util:int(required(<<"packing-difficulty">>, Base, Req, Opts)),
    {ok,
        #{
            <<"h0">> =>
                hb_util:encode(
                    ar_block:compute_h0(
                        hb_util:decode(required(<<"nonce-limiter-output">>, Base, Req, Opts)),
                        hb_util:int(required(<<"partition-number">>, Base, Req, Opts)),
                        hb_util:decode(required(<<"seed">>, Base, Req, Opts)),
                        hb_util:decode(required(<<"reward-addr">>, Base, Req, Opts)),
                        PackingDifficulty,
                        packing_state(h0_variant(PackingDifficulty), Opts)
                    )
                )
        }
    }.

%% @doc Compute H1: the hash of a solution found in the first recall range, and
%% the carrier of that chunk when the solution needs the second range too. Pure
%% SHA-256 over the *packed* chunk -- no RandomX.
h1(Base, Req, Opts) ->
    hash(
        ar_block:compute_h1(
            hb_util:decode(required(<<"h0">>, Base, Req, Opts)),
            hb_util:int(required(<<"nonce">>, Base, Req, Opts)),
            hb_util:decode(required(<<"chunk">>, Base, Req, Opts))
        )
    ).

%% @doc Compute H2: the hash of a solution involving the second chunk. Also pure
%% SHA-256 over the packed chunk.
h2(Base, Req, Opts) ->
    hash(
        ar_block:compute_h2(
            hb_util:decode(required(<<"h1">>, Base, Req, Opts)),
            hb_util:decode(required(<<"chunk">>, Base, Req, Opts)),
            hb_util:decode(required(<<"h0">>, Base, Req, Opts))
        )
    ).

%% @doc Unpack a whole packed chunk. `chunk-size' is the unpadded size the
%% Merkle leaf claims, and defaults to the packed size -- which is the same for
%% every chunk but the last of its transaction.
unpack(Base, Req, Opts) ->
    Chunk = hb_util:decode(required(<<"chunk">>, Base, Req, Opts)),
    maybe
        {ok, Packing} ?= packing(required(<<"packing">>, Base, Req, Opts), Opts),
        {ok, Unpacked} ?=
            unpack(
                Packing,
                hb_util:int(required(<<"absolute-end-offset">>, Base, Req, Opts)),
                hb_util:decode(get_first(<<"tx-root">>, Base, Req, <<>>, Opts)),
                Chunk,
                hb_util:int(get_first(<<"chunk-size">>, Base, Req, byte_size(Chunk), Opts)),
                Opts
            ),
        {ok, #{ <<"chunk">> => cache_link(Unpacked, Opts) }}
    end.

%% @doc Unpack one 8 KiB packed sub-chunk of a composite or replica-2.9 chunk.
%% `sub-chunk-start-offset' is the sub-chunk's byte offset within its chunk, not
%% its index.
unpack_sub_chunk(Base, Req, Opts) ->
    maybe
        {ok, Packing} ?= packing(required(<<"packing">>, Base, Req, Opts), Opts),
        {ok, Unpacked} ?=
            unpack_sub_chunk(
                Packing,
                hb_util:int(required(<<"absolute-end-offset">>, Base, Req, Opts)),
                hb_util:decode(get_first(<<"tx-root">>, Base, Req, <<>>, Opts)),
                hb_util:decode(required(<<"chunk">>, Base, Req, Opts)),
                hb_util:int(required(<<"sub-chunk-start-offset">>, Base, Req, Opts)),
                Opts
            ),
        {ok, #{ <<"chunk">> => cache_link(Unpacked, Opts) }}
    end.

%% @doc Generate the 8 MiB replica-2.9 entropy a sub-chunk is enciphered with.
%% This is the dominant cost of validating a post-2.9 block: one blob per proof,
%% two proofs for the two-chunk solutions that are the economic norm.
entropy(Base, Req, Opts) ->
    RewardAddr = hb_util:decode(required(<<"reward-addr">>, Base, Req, Opts)),
    Packing = {replica_2_9, RewardAddr},
    Entropy =
        ar_packing_server:generate_replica_2_9_entropy(
            RewardAddr,
            hb_util:int(required(<<"absolute-end-offset">>, Base, Req, Opts)),
            hb_util:int(required(<<"sub-chunk-start-offset">>, Base, Req, Opts)),
            ar_packing_server:get_randomx_state_by_packing(
                Packing,
                packing_state(packing_variant(Packing), Opts)
            )
        ),
    {ok, #{ <<"entropy">> => cache_link(Entropy, Opts) }}.

%%% Internal functions.

%% @doc Return the block-relative offset the two proofs are checked at. Above
%% the strict data split threshold the recall byte snaps to the start of the
%% 256 KiB bucket that holds it, which may land before the block's own start;
%% `ar_merkle' clamps a negative target to zero, and that clamping is part of
%% consensus.
recall_bucket_offset(RecallOffset, BlockStartOffset)
        when RecallOffset >= ?STRICT_DATA_SPLIT_THRESHOLD ->
    ar_poa:get_padded_offset(RecallOffset + 1, ?STRICT_DATA_SPLIT_THRESHOLD)
        - ?DATA_CHUNK_SIZE
        - BlockStartOffset;
recall_bucket_offset(RecallOffset, BlockStartOffset) ->
    RecallOffset - BlockStartOffset.

%% @doc Choose the ruleset the data path is validated under. The choice is made
%% from the start offset of the block that *holds* the recalled chunk, against
%% the two protocol thresholds -- never from a field of the block being
%% validated. A post-2.9 block may recall a byte from any era of the weave, so
%% all three rulesets are live and all three must be right.
data_path_ruleset(BlockStartOffset)
        when BlockStartOffset >= ?MERKLE_REBASE_SUPPORT_THRESHOLD ->
    <<"offset-rebase-support">>;
data_path_ruleset(BlockStartOffset)
        when BlockStartOffset >= ?STRICT_DATA_SPLIT_THRESHOLD ->
    <<"strict-data-split">>;
data_path_ruleset(_BlockStartOffset) ->
    <<"strict-borders">>.

%% @doc Enforce the size bounds a proof must satisfy before any of it is
%% interpreted. A proof that exceeds them cannot be valid, and refusing it here
%% keeps an unbounded path out of the Merkle walk. The paths are still in their
%% wire form, since that is what the Merkle device takes; the chunks are not.
proof_size(TXPath, DataPath, Chunk, UnpackedChunk) ->
    case
        byte_size(hb_util:decode(TXPath)) =< ?MAX_TX_PATH_SIZE
            andalso byte_size(hb_util:decode(DataPath)) =< ?MAX_DATA_PATH_SIZE
            andalso byte_size(Chunk) =< ?DATA_CHUNK_SIZE
            andalso byte_size(UnpackedChunk) =< ?DATA_CHUNK_SIZE
    of
        true -> ok;
        false ->
            {error, error_message(<<"invalid-proof-size">>,
                <<"A proof element exceeds the size the protocol permits it.">>)}
    end.

%% @doc Check the transaction path: the proof that the recalled byte falls
%% inside a transaction of the block under `tx-root'. Always the `basic'
%% ruleset -- the border, split and rebase rules govern the data path alone.
tx_path(TXRoot, TXPath, Offset, BlockSize, Opts) ->
    case merkle(TXRoot, TXPath, Offset, BlockSize, <<"basic">>, Opts) of
        {ok, DataRoot, StartOffset, EndOffset} ->
            {ok, DataRoot, StartOffset, EndOffset};
        {error, _} ->
            {error, error_message(<<"invalid-tx-path">>,
                <<"The tx-path does not resolve to a data-root under the tx-root.">>)}
    end.

%% @doc Check the data path: the proof that the recalled byte falls inside a
%% chunk of the transaction the tx-path resolved to.
data_path(DataRoot, DataPath, Offset, TXSize, Ruleset, Opts) ->
    case merkle(DataRoot, DataPath, Offset, TXSize, Ruleset, Opts) of
        {ok, ChunkID, StartOffset, EndOffset} ->
            {ok, ChunkID, StartOffset, EndOffset};
        {error, _} ->
            {error, error_message(<<"invalid-data-path">>,
                <<"The data-path does not resolve to a chunk under the data-root.">>)}
    end.

%% @doc Resolve a Merkle proof through `~arweave-merkle@2.9'. The composition is
%% the point of the device split: proof of access owns the offsets, the packing
%% and the thresholds, the Merkle device owns tree walking, and neither reaches
%% into the other's vendored code. Roots, proofs and leaves all stay in their
%% wire form: the leaf of a transaction path is the root of the data path that
%% follows it, so decoding either here would only mean re-encoding it.
merkle(Root, Proof, Offset, Size, Ruleset, Opts) ->
    case
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"root">> => Root,
                <<"proof">> => Proof,
                <<"offset">> => Offset,
                <<"size">> => Size,
                <<"ruleset">> => Ruleset
            },
            <<"validate">>,
            Opts
        )
    of
        {ok, Result} ->
            {ok,
                hb_maps:get(<<"leaf">>, Result, not_found, Opts),
                hb_util:int(hb_maps:get(<<"start-offset">>, Result, not_found, Opts)),
                hb_util:int(hb_maps:get(<<"end-offset">>, Result, not_found, Opts))
            };
        {error, Error} ->
            {error, Error}
    end.

%% @doc Bind the proof's packed chunk to the Merkle leaf the two paths resolved
%% to. An `expected-chunk-id' replaces the binding with a comparison against a
%% chunk id the caller has already established -- the proof's own chunk bytes
%% are then never examined, which the result reports as
%% `chunk-verified => false' rather than leaving the caller to infer it from
%% what they passed in.
chunk([], Packing, Leaf, Chunk, UnpackedChunk, Opts) ->
    unpack_and_bind(Packing, Leaf, Chunk, UnpackedChunk, Opts);
chunk(Expected, _Packing, Leaf, _Chunk, _UnpackedChunk, Opts) ->
    chunk_id(hb_maps:get(<<"chunk-id">>, Leaf, not_found, Opts), Expected).

%% @doc Unpack the proof's chunk and require it to be the chunk the Merkle leaf
%% names.
%%
%% A spora-2.6 proof carries a whole 256 KiB packed chunk: unpack it and hash
%% it. Every packing difficulty at or above 1 carries one 8 KiB packed sub-chunk
%% plus the whole 0-padded unpacked chunk, and binds two things -- the
%% deciphered sub-chunk must equal the corresponding slice of the supplied
%% unpacked chunk, and the unpadded unpacked chunk must hash to the leaf.
%% Neither check implies the other: the first ties the packing to the address,
%% the second ties the data to the weave.
unpack_and_bind({spora_2_6, _} = Packing, Leaf, Chunk, _UnpackedChunk, Opts) ->
    maybe
        {ok, Unpacked} ?=
            unpack(
                Packing,
                hb_maps:get(<<"absolute-end-offset">>, Leaf, not_found, Opts),
                hb_maps:get(<<"tx-root">>, Leaf, not_found, Opts),
                Chunk,
                hb_maps:get(<<"chunk-size">>, Leaf, not_found, Opts),
                Opts
            ),
        chunk_id(
            hb_maps:get(<<"chunk-id">>, Leaf, not_found, Opts),
            hb_util:encode(ar_tx:generate_chunk_id(Unpacked))
        )
    end;
unpack_and_bind(Packing, Leaf, Chunk, UnpackedChunk, Opts) ->
    ChunkSize = hb_maps:get(<<"chunk-size">>, Leaf, not_found, Opts),
    maybe
        ok ?= chunk_padding(UnpackedChunk, ChunkSize),
        ok ?= sub_chunk(Packing, Leaf, Chunk, UnpackedChunk, Opts),
        chunk_id(
            hb_maps:get(<<"chunk-id">>, Leaf, not_found, Opts),
            hb_util:encode(
                ar_tx:generate_chunk_id(binary:part(UnpackedChunk, 0, ChunkSize))
            )
        )
    end.

%% @doc Confirm the unpacked chunk is a full 256 KiB whose tail beyond the
%% chunk's own size is zero. The size is checked here rather than left to the
%% slice: a short chunk would raise a `badarg' where an invalid proof is meant.
%%
%% It names itself apart from `proof_size/4', which bounds the same field on
%% the way in. Two size rules answering with one message would make either
%% deletable without a mutant noticing: an oversized unpacked chunk that got
%% past the bound would simply be refused here, under the name the test was
%% asserting.
chunk_padding(UnpackedChunk, ChunkSize)
        when ChunkSize > ?DATA_CHUNK_SIZE;
             byte_size(UnpackedChunk) /= ?DATA_CHUNK_SIZE ->
    {error, error_message(<<"invalid-unpacked-chunk-size">>,
        <<"The unpacked chunk is not a 256 KiB chunk padded to its full size.">>)};
chunk_padding(UnpackedChunk, ChunkSize) ->
    PaddingSize = ?DATA_CHUNK_SIZE - ChunkSize,
    case binary:part(UnpackedChunk, ChunkSize, PaddingSize) of
        << 0:(PaddingSize * 8) >> -> ok;
        _ ->
            {error, error_message(<<"invalid-chunk-padding">>,
                <<"The unpacked chunk is padded with something other than zeroes.">>)}
    end.

%% @doc Decipher the proof's packed sub-chunk and require it to equal the
%% corresponding slice of the supplied unpacked chunk. This is a match, not a
%% comparison: an unpacking that yields any other well-formed sub-chunk -- one
%% for a different offset, or for a different address -- fails here.
sub_chunk(Packing, Leaf, Chunk, UnpackedChunk, Opts) ->
    SubChunkStartOffset = hb_maps:get(<<"sub-chunk-start-offset">>, Leaf, not_found, Opts),
    Expected =
        binary:part(
            UnpackedChunk,
            SubChunkStartOffset,
            ?COMPOSITE_PACKING_SUB_CHUNK_SIZE
        ),
    case
        unpack_sub_chunk(
            Packing,
            hb_maps:get(<<"absolute-end-offset">>, Leaf, not_found, Opts),
            hb_maps:get(<<"tx-root">>, Leaf, not_found, Opts),
            Chunk,
            SubChunkStartOffset,
            Opts
        )
    of
        {ok, Expected} -> ok;
        _ ->
            {error, error_message(<<"invalid-sub-chunk">>,
                <<"The packed sub-chunk does not decipher to the unpacked chunk.">>)}
    end.

%% @doc Require a chunk id to be the one the Merkle leaf names.
chunk_id(ChunkID, ChunkID) ->
    ok;
chunk_id(_ChunkID, _Other) ->
    {error, error_message(<<"invalid-chunk-id">>,
        <<"The chunk does not hash to the id the data-path resolved to.">>)}.

%% @doc Unpack a whole packed chunk, normalising the vendored failure modes into
%% the error convention.
unpack(Packing, AbsoluteEndOffset, TXRoot, Chunk, ChunkSize, Opts) ->
    unpack_result(
        ar_packing_server:unpack(
            Packing,
            AbsoluteEndOffset,
            TXRoot,
            Chunk,
            ChunkSize,
            packing_state(packing_variant(Packing), Opts)
        )
    ).

%% @doc Unpack one packed sub-chunk, normalising the vendored failure modes.
unpack_sub_chunk(Packing, AbsoluteEndOffset, TXRoot, Chunk, SubChunkStartOffset, Opts) ->
    unpack_result(
        ar_packing_server:unpack_sub_chunk(
            Packing,
            AbsoluteEndOffset,
            TXRoot,
            Chunk,
            SubChunkStartOffset,
            packing_state(packing_variant(Packing), Opts)
        )
    ).

%% @doc Map an unpacking result onto the error convention. A padding or size
%% failure is a property of the proof and is reported as one. Anything else --
%% an uninitialised RandomX state, an exception out of the NIF -- is a property
%% of this node, not of the proof, so it is raised rather than reported as an
%% invalid proof.
unpack_result({ok, Unpacked}) ->
    {ok, Unpacked};
unpack_result({error, invalid_padding}) ->
    {error, error_message(<<"invalid-chunk-padding">>,
        <<"The chunk unpacks to something other than zeroes past its size.">>)};
unpack_result({error, invalid_packed_size}) ->
    {error, error_message(<<"invalid-packed-chunk-size">>,
        <<"The packed chunk is not the size its packing requires.">>)};
unpack_result({error, invalid_chunk_size}) ->
    {error, error_message(<<"invalid-chunk-size">>,
        <<"The chunk size exceeds the size of the packed chunk carrying it.">>)};
unpack_result(Error) ->
    throw({unpacking_failed, Error}).

%% @doc Map a `packing' message onto the term the vendored packing code takes.
%% The mapping is explicit rather than derived, both because the wire names are
%% dashed where the vendored atoms are not, and because an unrecognised format
%% must be an error the caller can branch on rather than a coerced atom.
packing(Packing, Opts) ->
    packing(
        field(<<"format">>, Packing, Opts),
        hb_util:decode(field(<<"reward-addr">>, Packing, Opts)),
        hb_util:int(hb_maps:get(<<"packing-difficulty">>, Packing, 0, Opts))
    ).
packing(<<"spora-2-6">>, RewardAddr, _PackingDifficulty) ->
    {ok, {spora_2_6, RewardAddr}};
packing(<<"composite">>, RewardAddr, PackingDifficulty) ->
    {ok, {composite, RewardAddr, PackingDifficulty}};
packing(<<"replica-2-9">>, RewardAddr, _PackingDifficulty) ->
    {ok, {replica_2_9, RewardAddr}};
packing(Format, _RewardAddr, _PackingDifficulty) ->
    {error, error_message(<<"unsupported-packing">>,
        <<"This node cannot unpack the format `", Format/binary, "'.">>)}.

%% @doc Return the RandomX variant a packing format is built from.
packing_variant({spora_2_6, _RewardAddr}) -> rx512;
packing_variant({composite, _RewardAddr, _PackingDifficulty}) -> rx4096;
packing_variant({replica_2_9, _RewardAddr}) -> rxsquared.

%% @doc Return the RandomX variant H0 is computed with. Packing difficulty 0
%% predates the 2.8 fork; every block since uses rx4096.
h0_variant(0) -> rx512;
h0_variant(_PackingDifficulty) -> rx4096.

%% @doc Return an `ar_packing_server' packing state carrying an initialised
%% RandomX state for `Variant', starting the singleton that owns it if this is
%% its first use.
%%
%% RandomX state is the one genuinely process-bound resource in the subsystem: a
%% NIF resource costing a second to build in light mode and minutes in fast
%% mode, holding a gibibyte or more. It is keyed on `?RANDOMX_PACKING_KEY', a
%% fixed protocol constant, so a state built once stays valid for the life of
%% the network -- which is why it is a singleton rather than a cache. The three
%% variants are independent, so a node pays only for the ones it uses.
%%
%% `hb_name' is BEAM-global, so the name is scoped by the node's address: nodes
%% share a BEAM throughout the test suite, and two of them wanting different
%% modes must not be handed each other's state.
packing_state(Variant, Opts) ->
    Mode = randomx_mode(hb_opts:get(<<"arweave-randomx-mode">>, <<"light">>, Opts)),
    Name =
        {
            arweave_randomx,
            Variant,
            Mode,
            node_scope(hb_opts:get(priv_wallet, [], Opts))
        },
    % Named for what it is: every caller asking for the state, not the state
    % being built. The build happens once, inside the singleton below. Read as
    % a creation, a run of these looks like a singleton dying and respawning --
    % which is a diagnosis this event has already caused once.
    ?event(arweave_spora,
        {randomx_state_requested, {variant, Variant}, {mode, Mode}},
        Opts
    ),
    PID = hb_name:singleton(Name, fun() -> randomx_owner(Variant, Mode) end),
    Ref = monitor(process, PID),
    PID ! {randomx_state, self(), Ref},
    receive
        {randomx_state, Ref, PackingState} ->
            demonitor(Ref, [flush]),
            PackingState;
        {'DOWN', Ref, process, PID, Reason} ->
            throw({randomx_state_unavailable, Variant, Reason})
    end.

%% @doc Own a RandomX state for the life of the node. The state is built before
%% the first request is served, and then handed out unchanged: NIF resources are
%% reference-counted, so every caller may hold one, while the owner keeps the
%% underlying allocation alive.
randomx_owner(Variant, Mode) ->
    randomx_owner(ar_packing_server:init_packing_state(Mode, [Variant])).
randomx_owner(PackingState) ->
    receive
        {randomx_state, From, Ref} ->
            From ! {randomx_state, Ref, PackingState},
            randomx_owner(PackingState)
    end.


%% @doc Map the node's configured RandomX mode onto the atom the vendored
%% packing code takes. Mapped explicitly rather than coerced: the two atoms are
%% a closed vocabulary, and neither is guaranteed to be interned before the
%% packing code is first loaded -- which is exactly when this runs.
randomx_mode(<<"light">>) -> light;
randomx_mode(<<"fast">>) -> fast.

%% @doc Name the node a RandomX singleton belongs to. A node with no wallet
%% configured owns the `[]' scope, which is a scope like any other.
node_scope([]) -> [];
node_scope(Wallet) -> hb_util:human_id(ar_wallet:to_address(Wallet)).

%% @doc Return the standard shape of a hash key's result.
hash({Hash, Preimage}) ->
    {ok,
        #{
            <<"hash">> => hb_util:encode(Hash),
            <<"preimage">> => hb_util:encode(Preimage)
        }
    }.

%% @doc Write a large value to the cache and return a link to it. Chunks are
%% 256 KiB and entropy is 8 MiB; returning either inline would copy it through
%% every message the result passes through, for a caller that usually wants
%% only its hash.
cache_link(Binary, Opts) ->
    {ok, Location} = hb_cache:write(Binary, Opts),
    {link, Location, #{}}.

%% @doc Read a field from the request, falling back to the base message.
%%
%% These are data fields, not keys to resolve, so they are read with
%% `hb_maps:get/4' rather than `hb_ao:get/4'. Resolving a key against a message
%% that names this device dispatches back into the device -- so reading the
%% `h0' field of an `h1' request with `hb_ao:get' would compute an H0 rather
%% than return the one supplied. `hb_maps:get/4' reads the value directly while
%% still loading it if it is a link.
get_first(Key, Base, Req, Default, Opts) ->
    case hb_maps:get(Key, Req, not_found, Opts) of
        not_found -> hb_maps:get(Key, Base, Default, Opts);
        Value -> Value
    end.

%% @doc Read a key that has no meaningful default.
required(Key, Base, Req, Opts) ->
    case get_first(Key, Base, Req, not_found, Opts) of
        not_found -> throw({missing_key, Key});
        Value -> Value
    end.

%% @doc Read a required field of a nested message -- the proof's own elements,
%% each of which may be a link to a value far larger than the message holding
%% it.
field(Key, Message, Opts) ->
    case hb_maps:get(Key, Message, not_found, Opts) of
        not_found -> throw({missing_key, Key});
        Value -> Value
    end.

%% @doc Read a field that the wire form omits when it is empty. A proof at
%% packing difficulty 0 carries no unpacked chunk at all.
optional_field(Key, Message, Opts) ->
    hb_maps:get(Key, Message, <<>>, Opts).

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.
