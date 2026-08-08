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
-include_lib("eunit/include/eunit.hrl").

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

%%% Tests.

%% @doc The golden vectors: real mainnet post-2.9 proofs of access, paired with
%% the bounds of the block that actually holds the byte each one recalls.
%%
%%     {Height, Proof, RecallOffset, BlockStartOffset, BlockEndOffset, TXRoot}
%%
%% The proofs come from `test/fixtures/arweave'; the bounds and the transaction root
%% come from the mainnet block index, which `~arweave-block-index@2.9/bounds'
%% will produce at run time and which are frozen here so that the test needs
%% neither an index nor a network. Every recall byte lands in a block mined
%% years before the one carrying the proof, so the three eras of the weave --
%% and therefore all three data-path rulesets -- are all exercised.
golden_vectors() ->
    [
        % Block start below the strict data split threshold: strict-borders.
        {1974239, <<"poa2">>, 14792735383554, 14790855690314, 14806590655801,
            <<"ozgHTwQPj5FNwBd0Tr5RoUZICfBi0UVFtL1mdDyXbEQ">>},
        % Block start between the two thresholds: strict-data-split.
        {1974849, <<"poa2">>, 102828462797357, 102828084601078, 102830857560310,
            <<"UM4d9hHjFw9QqYxFi7aOP_3BzLTpIKFpI-jo4LfyLYw">>},
        {1974850, <<"poa">>, 83521370300568, 83520184688886, 83522914132214,
            <<"tzgTRtbKPvoebYv0x-HA32LUCFDqYYBt3nh9yxGr8iY">>},
        {1974870, <<"poa">>, 43220778741187, 43220293951734, 43221043421430,
            <<"RlrP-NySxRkfHNRLcLhE19H2I7uu8rfzAfCSH7gnHB8">>},
        {1974872, <<"poa">>, 115246454124947, 115244903211254, 115248319996150,
            <<"dkbnSYJI0CDaQug-n0jaNHLr0v88Ag5C1yR9YXPamaw">>},
        % Block start above the merkle rebase support threshold:
        % offset-rebase-support.
        {1974239, <<"poa">>, 335954811379147, 335954602664182, 335956384194806,
            <<"vimHm-9boeBBUGyVuf__ldtrFF5PI2LfRnqi9NaozMw">>},
        % The one-chunk solution in the fixture set: no `recall-byte2', so no
        % second proof.
        {1974860, <<"poa">>, 218816484901751, 218816326443254, 218816578101494,
            <<"tQDuLxUQstSJemM08IMithuHh36LU9gfwKBPs9qn_qQ">>},
        {1974871, <<"poa">>, 352102932374496, 352098788090102, 352103433281782,
            <<"KulCVl8Ce5NyXKFQO38-tPdDzW-7Tal_7w2L6S0U-g0">>},
        {1974871, <<"poa2">>, 238377158114703, 238376099029238, 238377381437686,
            <<"kHujf2BFScybMVql8xi51AF27UCgD6cNcNNVxHt5tQo">>},
        {1974880, <<"poa">>, 359713235289064, 359712680354038, 359713375297782,
            <<"CdaY6ryyyG8nE91vozcwwJNyn60mbCT2twyMvicCBCI">>}
    ].

%% @doc The vector every mutation test starts from: the first proof of block
%% 1,974,871's two-chunk solution, which recalls a byte from the rebase era of
%% the weave and resolves to a full 256 KiB chunk.
mutation_base() ->
    hd([ Vector || Vector = {1974871, <<"poa">>, _, _, _, _} <- golden_vectors() ]).

%% @doc Every mainnet proof validates, and yields the chunk id the block header
%% itself commits to. Each one generates an 8 MiB entropy blob, so the whole set
%% is a second or two of RandomX.
golden_vectors_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        lists:foreach(
            fun(Vector) ->
                {Height, Proof, _RecallOffset, _Start, _End, _TXRoot} = Vector,
                ?assertEqual(
                    chunk_hash(Height, Proof),
                    validated(request(Vector), Opts)
                )
            end,
            golden_vectors()
        )
    end}.

%% @doc H0, the two recall ranges, the recall bytes and the solution hash of a
%% real block are all reproduced from the block and its parent alone. This is
%% the whole of the proof-of-work side of the device against mainnet: a mistake
%% anywhere in the chain moves the final hash.
solution_chain_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        lists:foreach(
            fun(Height) -> solution_chain(Height, Opts) end,
            [1974240, 1974860, 1974871, 1974880]
        )
    end}.

%% @doc `unpack-sub-chunk' deciphers a mainnet packed sub-chunk into exactly the
%% slice of the unpacked chunk the proof carries, and returns it as a link.
unpack_sub_chunk_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        Vector = mutation_base(),
        Request = request(Vector),
        PoA = maps:get(<<"poa">>, Request),
        {ok, Result} =
            hb_ao:resolve(
                Request#{
                    <<"absolute-end-offset">> => absolute_end_offset(Vector),
                    <<"chunk">> => maps:get(<<"chunk">>, PoA),
                    <<"sub-chunk-start-offset">> => sub_chunk_start_offset(Vector)
                },
                <<"unpack-sub-chunk">>,
                Opts
            ),
        ?assertEqual(
            binary:part(
                hb_util:decode(maps:get(<<"unpacked-chunk">>, PoA)),
                sub_chunk_start_offset(Vector),
                ?COMPOSITE_PACKING_SUB_CHUNK_SIZE
            ),
            hb_maps:get(<<"chunk">>, Result, not_found, Opts)
        )
    end}.

%% @doc `entropy' yields the 8 MiB blob the sub-chunk was enciphered with:
%% XORing the packed sub-chunk against the slice of it the chunk's offset
%% selects reproduces the unpacked sub-chunk. Unpacking a replica-2.9 chunk is
%% nothing more than this.
entropy_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        Vector = mutation_base(),
        Request = request(Vector),
        PoA = maps:get(<<"poa">>, Request),
        AbsoluteEndOffset = absolute_end_offset(Vector),
        SubChunkStartOffset = sub_chunk_start_offset(Vector),
        {ok, Result} =
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-spora@2.9">>,
                    <<"reward-addr">> => reward_addr(Vector),
                    <<"absolute-end-offset">> => AbsoluteEndOffset,
                    <<"sub-chunk-start-offset">> => SubChunkStartOffset
                },
                <<"entropy">>,
                Opts
            ),
        Entropy = hb_maps:get(<<"entropy">>, Result, not_found, Opts),
        ?assertEqual(?REPLICA_2_9_ENTROPY_SIZE, byte_size(Entropy)),
        Slice =
            binary:part(
                Entropy,
                ar_replica_2_9:get_slice_index(AbsoluteEndOffset)
                    * ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
                ?COMPOSITE_PACKING_SUB_CHUNK_SIZE
            ),
        ?assertEqual(
            binary:part(
                hb_util:decode(maps:get(<<"unpacked-chunk">>, PoA)),
                SubChunkStartOffset,
                ?COMPOSITE_PACKING_SUB_CHUNK_SIZE
            ),
            crypto:exor(hb_util:decode(maps:get(<<"chunk">>, PoA)), Slice)
        )
    end}.

%%% Mutation tests. Each changes exactly one guarded value of a proof that is
%%% otherwise known to validate, and asserts the error the guard must raise. A
%%% mutant that validates instead reports `valid', which is the signal that the
%%% check it targets is dead code.

%% @doc A transaction path longer than the protocol permits is refused before it
%% is walked.
reject_oversized_tx_path_test() ->
    ?assertEqual(
        <<"invalid-proof-size">>,
        rejection(mutate_poa(<<"tx-path">>, oversized(?MAX_TX_PATH_SIZE)))
    ).

%% @doc The same for a data path.
reject_oversized_data_path_test() ->
    ?assertEqual(
        <<"invalid-proof-size">>,
        rejection(mutate_poa(<<"data-path">>, oversized(?MAX_DATA_PATH_SIZE)))
    ).

%% @doc A packed chunk cannot exceed one chunk.
reject_oversized_chunk_test() ->
    ?assertEqual(
        <<"invalid-proof-size">>,
        rejection(mutate_poa(<<"chunk">>, oversized(?DATA_CHUNK_SIZE)))
    ).

%% @doc Nor can an unpacked chunk.
reject_oversized_unpacked_chunk_test() ->
    ?assertEqual(
        <<"invalid-proof-size">>,
        rejection(mutate_poa(<<"unpacked-chunk">>, oversized(?DATA_CHUNK_SIZE)))
    ).

%% @doc An unpacked chunk that is not padded to a full chunk is refused rather
%% than sliced.
reject_truncated_unpacked_chunk_test_() ->
    {timeout, 300, fun() ->
        ?assertEqual(
            <<"invalid-unpacked-chunk-size">>,
            rejection(
                mutate_poa(
                    <<"unpacked-chunk">>,
                    hb_util:encode(<< 0:(?COMPOSITE_PACKING_SUB_CHUNK_SIZE * 8) >>)
                )
            )
        )
    end}.

%% @doc The transaction path must resolve to the transaction root the caller
%% supplies. Flipping one byte of the root breaks it.
reject_foreign_tx_root_test() ->
    {_Height, _Proof, _RecallOffset, _Start, _End, TXRoot} = mutation_base(),
    ?assertEqual(
        <<"invalid-tx-path">>,
        rejection(mutate(<<"tx-root">>, flip(TXRoot)))
    ).

%% @doc A corrupted transaction path does not resolve to its root either.
reject_corrupt_tx_path_test() ->
    ?assertEqual(
        <<"invalid-tx-path">>,
        rejection(mutate_poa(<<"tx-path">>, flip(poa_field(<<"tx-path">>))))
    ).

%% @doc The block size bounds the transaction path's walk, so a proof does not
%% carry over into a block of a different size.
reject_wrong_block_size_test() ->
    {_Height, _Proof, _RecallOffset, Start, End, _TXRoot} = mutation_base(),
    ?assertEqual(
        <<"invalid-tx-path">>,
        rejection(mutate(<<"block-size">>, (End - Start) div 2))
    ).

%% @doc The recall offset selects which chunk the paths must prove. Moving it by
%% a whole bucket selects a different one.
reject_wrong_recall_offset_test() ->
    {_Height, _Proof, RecallOffset, _Start, _End, _TXRoot} = mutation_base(),
    ?assertEqual(
        <<"invalid-data-path">>,
        rejection(mutate(<<"recall-offset">>, RecallOffset + ?DATA_CHUNK_SIZE))
    ).

%% @doc A corrupted data path does not resolve to the data root the transaction
%% path produced.
reject_corrupt_data_path_test() ->
    ?assertEqual(
        <<"invalid-data-path">>,
        rejection(mutate_poa(<<"data-path">>, flip(poa_field(<<"data-path">>))))
    ).

%% @doc The deciphered sub-chunk must equal the slice of the unpacked chunk it
%% covers. Flipping a byte inside that slice breaks the binding between the
%% packed and unpacked forms.
reject_mismatched_sub_chunk_test_() ->
    {timeout, 300, fun() ->
        Vector = mutation_base(),
        ?assertEqual(
            <<"invalid-sub-chunk">>,
            rejection(
                mutate_poa(
                    <<"unpacked-chunk">>,
                    flip_at(poa_field(<<"unpacked-chunk">>), sub_chunk_start_offset(Vector))
                )
            )
        )
    end}.

%% @doc The sub-chunk index selects both the slice of the unpacked chunk and the
%% entropy the packed sub-chunk is deciphered with, so a proof cannot be
%% replayed at another index.
reject_wrong_sub_chunk_index_test_() ->
    {timeout, 300, fun() ->
        Request = request(mutation_base()),
        Index = maps:get(<<"sub-chunk-index">>, Request),
        ?assertEqual(
            <<"invalid-sub-chunk">>,
            rejection(mutate(<<"sub-chunk-index">>, (Index + 1) rem ?COMPOSITE_PACKING_SUB_CHUNK_COUNT))
        )
    end}.

%% @doc The replica-2.9 entropy is keyed on the miner's address, so a proof
%% packed for one address does not decipher for another.
reject_wrong_reward_addr_test_() ->
    {timeout, 300, fun() ->
        Request = request(mutation_base()),
        Packing = maps:get(<<"packing">>, Request),
        ?assertEqual(
            <<"invalid-sub-chunk">>,
            rejection(
                mutate(
                    <<"packing">>,
                    Packing#{
                        <<"reward-addr">> => flip(maps:get(<<"reward-addr">>, Packing))
                    }
                )
            )
        )
    end}.

%% @doc The unpacked chunk must hash to the id the data path resolved to.
%% Flipping a byte outside the sub-chunk the proof deciphers leaves the packing
%% intact and breaks only that hash -- so the two checks are independent, and
%% neither subsumes the other.
reject_mismatched_chunk_id_test_() ->
    {timeout, 300, fun() ->
        Vector = mutation_base(),
        Outside =
            (sub_chunk_start_offset(Vector) + ?COMPOSITE_PACKING_SUB_CHUNK_SIZE)
                rem ?DATA_CHUNK_SIZE,
        ?assertEqual(
            <<"invalid-chunk-id">>,
            rejection(
                mutate_poa(
                    <<"unpacked-chunk">>,
                    flip_at(poa_field(<<"unpacked-chunk">>), Outside)
                )
            )
        )
    end}.

%% @doc An `expected-chunk-id' that the paths do not resolve to is refused, even
%% though supplying one skips the unpacking entirely.
reject_wrong_expected_chunk_id_test() ->
    {Height, Proof, _RecallOffset, _Start, _End, _TXRoot} = mutation_base(),
    {ok, ChunkID} = chunk_hash(Height, Proof),
    ?assertEqual(
        <<"invalid-chunk-id">>,
        rejection(
            mutate(
                <<"expected-chunk-id">>,
                flip(hb_maps:get(<<"chunk-id">>, ChunkID, not_found, test_opts()))
            )
        )
    ).

%% @doc The result says whether the proof's own chunk was verified.
%%
%% Supplying `expected-chunk-id' skips the unpacking entirely: the proof's chunk
%% bytes are never examined, and the answer rests on a chunk id the caller
%% asserted. That is a strictly weaker answer than the default, so it is
%% reported as one. Were both modes to answer `valid => true' alike, a caller
%% could not tell that the thing they asked about had not been looked at.
reports_whether_the_chunk_was_verified_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        {Height, Proof, _RecallOffset, _Start, _End, _TXRoot} = mutation_base(),
        {ok, ChunkID} = chunk_hash(Height, Proof),
        Id = hb_maps:get(<<"chunk-id">>, ChunkID, not_found, Opts),
        % The default path unpacks and binds the proof's own chunk.
        {ok, Full} = hb_ao:resolve(request(mutation_base()), <<"validate">>, Opts),
        ?assertEqual(true, hb_maps:get(<<"valid">>, Full, false, Opts)),
        ?assertEqual(
            true,
            hb_util:atom(hb_maps:get(<<"chunk-verified">>, Full, missing, Opts))
        ),
        % Asserting the id short-circuits it, and the answer says so.
        Short =
            hb_maps:put(<<"expected-chunk-id">>, Id, request(mutation_base()), Opts),
        {ok, Skipped} = hb_ao:resolve(Short, <<"validate">>, Opts),
        ?assertEqual(true, hb_maps:get(<<"valid">>, Skipped, false, Opts)),
        ?assertEqual(
            false,
            hb_util:atom(
                hb_maps:get(<<"chunk-verified">>, Skipped, missing, Opts))
        )
    end}.

%% @doc A packing format this node cannot unpack is reported as such, rather
%% than as an invalid proof.
reject_unknown_packing_test() ->
    Request = request(mutation_base()),
    Packing = maps:get(<<"packing">>, Request),
    ?assertEqual(
        <<"unsupported-packing">>,
        rejection(mutate(<<"packing">>, Packing#{ <<"format">> => <<"spora-2-5">> }))
    ).

%% @doc The unpacked chunk of a partial chunk must be padded to a full chunk
%% with zeroes. The proof below resolves to a 100,000-byte chunk; with correct
%% padding it survives as far as the sub-chunk check, and a single non-zero
%% padding byte stops it before that.
reject_nonzero_chunk_padding_test_() ->
    {timeout, 300, fun() ->
        Opts = test_opts(),
        Request = partial_chunk_request(),
        PoA = maps:get(<<"poa">>, Request),
        ?assertEqual(<<"invalid-sub-chunk">>, rejection(Request, Opts)),
        ?assertEqual(
            <<"invalid-chunk-padding">>,
            rejection(
                Request#{
                    <<"poa">> =>
                        PoA#{
                            <<"unpacked-chunk">> =>
                                flip_at(maps:get(<<"unpacked-chunk">>, PoA), 200000)
                        }
                },
                Opts
            )
        )
    end}.

%% @doc The data path ruleset is chosen from the start offset of the block that
%% holds the recalled chunk, and the choice is load-bearing. The proof below
%% carries a rebase marker, which only `offset-rebase-support' accepts. Moving
%% the block -- and the byte it recalls -- down by exactly one bucket leaves the
%% offset the proof is checked at untouched and drops the block below the rebase
%% threshold, and the proof is then refused.
select_rebase_ruleset_test() ->
    Opts = test_opts(),
    ?assertEqual(
        0,
        (?MERKLE_REBASE_SUPPORT_THRESHOLD - ?STRICT_DATA_SPLIT_THRESHOLD)
            rem ?DATA_CHUNK_SIZE
    ),
    Request = rebased_request(?MERKLE_REBASE_SUPPORT_THRESHOLD),
    ?assertMatch({ok, _}, hb_ao:resolve(Request, <<"validate">>, Opts)),
    ?assertEqual(
        <<"invalid-data-path">>,
        rejection(rebased_request(?MERKLE_REBASE_SUPPORT_THRESHOLD - ?DATA_CHUNK_SIZE), Opts)
    ).

%% @doc The other boundary. The proof below splits its data into three
%% 100,000-byte chunks, which only `strict-borders' accepts -- the strict split
%% rule requires every chunk but the last to start on a bucket border. Moving
%% the block up by one bucket, across the strict data split threshold, refuses
%% it.
select_strict_borders_ruleset_test() ->
    Opts = test_opts(),
    Request = split_request(?STRICT_DATA_SPLIT_THRESHOLD - 150000),
    ?assertMatch({ok, _}, hb_ao:resolve(Request, <<"validate">>, Opts)),
    ?assertEqual(
        <<"invalid-data-path">>,
        rejection(split_request(?STRICT_DATA_SPLIT_THRESHOLD - 150000 + ?DATA_CHUNK_SIZE), Opts)
    ).

%%% Test helpers.

test_opts() ->
    #{ <<"store">> => [hb_test_utils:test_store()] }.

%% @doc Read one of the mainnet block fixtures. They are Arweave's own JSON, so
%% their keys are the wire's `snake_case' and their values are plain -- no links
%% and no `structured@1.0' -- which is why they are read with `maps:get'.
fixture(Height) ->
    {ok, Body} =
        file:read_file(
            "test/fixtures/arweave/block-" ++ integer_to_list(Height) ++ ".json"
        ),
    hb_json:decode(Body).

%% @doc Build a `validate' request from a golden vector.
request({Height, Proof, RecallOffset, BlockStartOffset, BlockEndOffset, TXRoot}) ->
    Block = fixture(Height),
    PoA = maps:get(Proof, Block),
    PackingDifficulty = maps:get(<<"packing_difficulty">>, Block),
    Nonce = binary:decode_unsigned(hb_util:decode(maps:get(<<"nonce">>, Block))),
    #{
        <<"device">> => <<"arweave-spora@2.9">>,
        <<"block-start-offset">> => BlockStartOffset,
        <<"block-size">> => BlockEndOffset - BlockStartOffset,
        <<"recall-offset">> => RecallOffset,
        <<"tx-root">> => TXRoot,
        <<"sub-chunk-index">> => ar_block:get_sub_chunk_index(PackingDifficulty, Nonce),
        <<"packing">> =>
            #{
                <<"format">> => <<"replica-2-9">>,
                <<"reward-addr">> => maps:get(<<"reward_addr">>, Block),
                <<"packing-difficulty">> => PackingDifficulty
            },
        <<"poa">> =>
            #{
                <<"tx-path">> => maps:get(<<"tx_path">>, PoA),
                <<"data-path">> => maps:get(<<"data_path">>, PoA),
                <<"chunk">> => maps:get(<<"chunk">>, PoA),
                <<"unpacked-chunk">> => maps:get(<<"unpacked_chunk">>, PoA)
            }
    }.

%% @doc The chunk id a block header commits to for one of its proofs. A block
%% carries the hash of each unpacked chunk, and that hash is the Merkle leaf the
%% proof must resolve to -- so it is an independent statement of the answer.
chunk_hash(Height, <<"poa">>) ->
    {ok, #{ <<"chunk-id">> => maps:get(<<"unpacked_chunk_hash">>, fixture(Height)) }};
chunk_hash(Height, <<"poa2">>) ->
    {ok, #{ <<"chunk-id">> => maps:get(<<"unpacked_chunk2_hash">>, fixture(Height)) }}.

%% @doc Resolve a request and return only the fields `validate' promises, so
%% that the assertion is not coupled to the resolver's own bookkeeping.
validated(Request, Opts) ->
    {ok, Result} = hb_ao:resolve(Request, <<"validate">>, Opts),
    ?assert(hb_maps:get(<<"valid">>, Result, false, Opts)),
    {ok, #{ <<"chunk-id">> => hb_maps:get(<<"chunk-id">>, Result, not_found, Opts) }}.

%% @doc Resolve a request expected to be refused, returning the `message' of the
%% error. A request that validates returns `valid' instead, which is how a
%% mutation test reports that the check it targets did not fire.
rejection(Request) ->
    rejection(Request, test_opts()).
rejection(Request, Opts) ->
    case hb_ao:resolve(Request, <<"validate">>, Opts) of
        {ok, _Result} -> valid;
        {error, Error} -> hb_maps:get(<<"message">>, Error, not_found, Opts)
    end.

%% @doc Replace one field of the mutation base request.
mutate(Key, Value) ->
    (request(mutation_base()))#{ Key => Value }.

%% @doc Replace one field of the mutation base request's proof.
mutate_poa(Key, Value) ->
    Request = request(mutation_base()),
    Request#{ <<"poa">> => (maps:get(<<"poa">>, Request))#{ Key => Value } }.

%% @doc Read one field of the mutation base request's proof.
poa_field(Key) ->
    maps:get(Key, maps:get(<<"poa">>, request(mutation_base()))).

%% @doc The absolute end offset in the weave of the chunk a golden vector
%% recalls -- the value the device derives internally, and which the
%% `unpack-sub-chunk' and `entropy' keys take as an argument. Walked with
%% `ar_merkle' directly: a test helper that composed the Merkle device would be
%% asserting against the same code path it is meant to corroborate.
absolute_end_offset(Vector) ->
    {_Height, _Proof, RecallOffset, BlockStartOffset, BlockEndOffset, TXRoot} = Vector,
    PoA = maps:get(<<"poa">>, request(Vector)),
    SeekOffset = recall_bucket_offset(RecallOffset, BlockStartOffset),
    {DataRoot, TXStartOffset, TXEndOffset} =
        ar_merkle:validate_path(
            hb_util:decode(TXRoot),
            SeekOffset,
            BlockEndOffset - BlockStartOffset,
            hb_util:decode(maps:get(<<"tx-path">>, PoA))
        ),
    {_ChunkID, _ChunkStartOffset, ChunkEndOffset} =
        ar_merkle:validate_path(
            DataRoot,
            SeekOffset - TXStartOffset,
            TXEndOffset - TXStartOffset,
            hb_util:decode(maps:get(<<"data-path">>, PoA)),
            ruleset_atom(data_path_ruleset(BlockStartOffset))
        ),
    BlockStartOffset + TXStartOffset + ChunkEndOffset.

%% @doc The byte offset within its chunk of the sub-chunk a golden vector's
%% proof carries.
sub_chunk_start_offset(Vector) ->
    maps:get(<<"sub-chunk-index">>, request(Vector)) * ?COMPOSITE_PACKING_SUB_CHUNK_SIZE.

%% @doc The mining address a golden vector's chunk is packed for.
reward_addr(Vector) ->
    maps:get(<<"reward-addr">>, maps:get(<<"packing">>, request(Vector))).

%% @doc Check that a block's H0, recall ranges, recall bytes and solution hash
%% are all reproduced by this device from the block and its parent.
solution_chain(Height, Opts) ->
    Block = fixture(Height),
    Parent = fixture(Height - 1),
    Info = maps:get(<<"nonce_limiter_info">>, Block),
    PackingDifficulty = maps:get(<<"packing_difficulty">>, Block),
    Nonce = binary:decode_unsigned(hb_util:decode(maps:get(<<"nonce">>, Block))),
    Device = #{ <<"device">> => <<"arweave-spora@2.9">> },
    {ok, H0Result} =
        hb_ao:resolve(
            Device#{
                <<"nonce-limiter-output">> => maps:get(<<"output">>, Info),
                <<"partition-number">> => maps:get(<<"partition_number">>, Block),
                <<"seed">> =>
                    maps:get(<<"seed">>, maps:get(<<"nonce_limiter_info">>, Parent)),
                <<"reward-addr">> => maps:get(<<"reward_addr">>, Block),
                <<"packing-difficulty">> => PackingDifficulty
            },
            <<"h0">>,
            Opts
        ),
    H0 = hb_maps:get(<<"h0">>, H0Result, not_found, Opts),
    {ok, Ranges} =
        hb_ao:resolve(
            Device#{
                <<"h0">> => H0,
                <<"partition-number">> => maps:get(<<"partition_number">>, Block),
                <<"partition-upper-bound">> =>
                    hb_util:int(maps:get(<<"zone_upper_bound">>, Info))
            },
            <<"recall-range">>,
            Opts
        ),
    RecallByte =
        fun(RangeKey) ->
            {ok, Result} =
                hb_ao:resolve(
                    Device#{
                        <<"range-start">> => hb_maps:get(RangeKey, Ranges, not_found, Opts),
                        <<"nonce">> => Nonce,
                        <<"packing-difficulty">> => PackingDifficulty
                    },
                    <<"recall-byte">>,
                    Opts
                ),
            Result
        end,
    First = RecallByte(<<"range1-start">>),
    ?assertEqual(
        hb_util:int(maps:get(<<"recall_byte">>, Block)),
        hb_maps:get(<<"recall-byte">>, First, not_found, Opts)
    ),
    ?assertEqual(
        ar_block:get_sub_chunk_index(PackingDifficulty, Nonce),
        hb_maps:get(<<"sub-chunk-index">>, First, not_found, Opts)
    ),
    {ok, H1} =
        hb_ao:resolve(
            Device#{
                <<"h0">> => H0,
                <<"nonce">> => Nonce,
                <<"chunk">> =>
                    maps:get(<<"chunk">>, maps:get(<<"poa">>, Block))
            },
            <<"h1">>,
            Opts
        ),
    solution_hash(Block, Device, H0, H1, RecallByte, Opts).

%% @doc A one-chunk solution hashes to H1; a two-chunk solution carries a
%% `recall-byte2' that must also be reproduced, and hashes to H2.
solution_hash(Block, Device, H0, H1, RecallByte, Opts) ->
    case maps:get(<<"recall_byte2">>, Block, null) of
        null ->
            ?assertEqual(maps:get(<<"hash">>, Block), hb_maps:get(<<"hash">>, H1, not_found, Opts)),
            ?assertEqual(
                maps:get(<<"hash_preimage">>, Block),
                hb_maps:get(<<"preimage">>, H1, not_found, Opts)
            );
        RecallByte2 ->
            ?assertEqual(
                hb_util:int(RecallByte2),
                hb_maps:get(<<"recall-byte">>, RecallByte(<<"range2-start">>), not_found, Opts)
            ),
            {ok, H2} =
                hb_ao:resolve(
                    Device#{
                        <<"h1">> => hb_maps:get(<<"hash">>, H1, not_found, Opts),
                        <<"h0">> => H0,
                        <<"chunk">> =>
                            maps:get(<<"chunk">>, maps:get(<<"poa2">>, Block))
                    },
                    <<"h2">>,
                    Opts
                ),
            ?assertEqual(maps:get(<<"hash">>, Block), hb_maps:get(<<"hash">>, H2, not_found, Opts)),
            ?assertEqual(
                maps:get(<<"hash_preimage">>, Block),
                hb_maps:get(<<"preimage">>, H2, not_found, Opts)
            )
    end.

%% @doc A proof whose data path carries a rebase marker, in a block starting at
%% `BlockStartOffset' and recalling its first byte. Only the
%% `offset-rebase-support' ruleset accepts a rebased path, so the request
%% validates exactly when the block start selects that ruleset.
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

%% @doc A proof whose data is split into three 100,000-byte chunks, recalled at
%% the middle one. Only `strict-borders' accepts a chunk that neither fills its
%% bucket nor ends its dataset, so the request validates exactly when the block
%% start selects that ruleset.
split_request(BlockStartOffset) ->
    Leaves = [{leaf(1), 100000}, {leaf(2), 200000}, {leaf(3), 300000}],
    synthetic_request(
        BlockStartOffset,
        ?STRICT_DATA_SPLIT_THRESHOLD + (BlockStartOffset - (?STRICT_DATA_SPLIT_THRESHOLD - 150000)),
        Leaves,
        300000,
        150000,
        leaf(2)
    ).

%% @doc A proof resolving to a 100,000-byte chunk -- the last of its dataset --
%% so that its unpacked chunk carries 162,144 bytes of padding to check. Sited
%% below the strict data split threshold, where the recall byte is not snapped
%% to a bucket border.
partial_chunk_request() ->
    Leaves = [{leaf(1), ?DATA_CHUNK_SIZE}, {leaf(2), ?DATA_CHUNK_SIZE + 100000}],
    Request =
        synthetic_request(
            0,
            300000,
            Leaves,
            ?DATA_CHUNK_SIZE + 100000,
            300000,
            leaf(2)
        ),
    PoA = maps:get(<<"poa">>, Request),
    maps:remove(
        <<"expected-chunk-id">>,
        Request#{
            <<"poa">> =>
                PoA#{
                    <<"chunk">> =>
                        hb_util:encode(
                            << 0:(?COMPOSITE_PACKING_SUB_CHUNK_SIZE * 8) >>
                        ),
                    <<"unpacked-chunk">> =>
                        hb_util:encode(
                            <<
                                (leaf(9))/binary,
                                0:((?DATA_CHUNK_SIZE - 32) * 8)
                            >>
                        )
                }
        }
    ).

%% @doc Build a request around a synthetic data tree: wrap it in a single-
%% transaction block, prove the byte at `Dest', and supply `ChunkID' -- the leaf
%% the proof resolves to -- as the chunk id the caller already knows, so that no
%% packed chunk is needed. Given as an argument rather than walked out of the
%% tree, because these requests are built at placements where the proof is meant
%% not to validate.
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
                    hb_util:encode(ar_merkle:generate_path(TXRoot, Dest, TXTree)),
                <<"data-path">> =>
                    hb_util:encode(ar_merkle:generate_path(DataRoot, Dest, DataTree)),
                <<"chunk">> => <<>>,
                <<"unpacked-chunk">> => <<>>
            }
    }.

%% @doc The `ar_merkle' atom for a ruleset this device names. Used only to build
%% the synthetic proofs, which must know which ruleset they are being built for.
ruleset_atom(<<"strict-borders">>) -> strict_borders_ruleset;
ruleset_atom(<<"strict-data-split">>) -> strict_data_split_ruleset;
ruleset_atom(<<"offset-rebase-support">>) -> offset_rebase_support_ruleset.

%% @doc A stable, distinct 32-byte leaf value.
leaf(N) ->
    crypto:hash(sha256, << "dev_arweave_spora leaf ", N:8 >>).

%% @doc Grow a base64url value past a byte limit, so that only its size is
%% wrong.
oversized(Limit) ->
    hb_util:encode(<< 0:((Limit + 1) * 8) >>).

%% @doc Flip the last byte of a base64url value, leaving its length alone.
flip(Encoded) ->
    flip_at(Encoded, byte_size(hb_util:decode(Encoded)) - 1).

%% @doc Flip one byte of a base64url value, at an offset into the value it
%% encodes rather than into its encoding -- a flip in the encoding lands on a
%% byte the caller did not choose.
flip_at(Encoded, Offset) ->
    Decoded = hb_util:decode(Encoded),
    << Head:Offset/binary, Byte:8, Tail/binary >> = Decoded,
    hb_util:encode(<< Head/binary, (Byte bxor 1):8, Tail/binary >>).
