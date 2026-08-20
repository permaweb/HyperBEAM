%%% @doc An AO-Core interface to the storage modules an Arweave node holds the
%%% weave in: the chunk files on disk, the entropy they are packed with, the
%%% index that places each chunk in the weave, and the record of which offsets
%%% are held.
%%%
%%% This is what makes a miner a miner. A partition is 3.6 TB of chunks packed
%%% for one address, and a pass over one VDF step reads two 2.5 MiB recall
%%% ranges of it and hashes them. Nothing about that is affordable from a peer:
%%% the chunks a peer serves are unpacked, and packing one sub-chunk costs an 8
%%% MiB RandomX run. A node mines from what it holds.
%%%
%%% The layout on disk is the Arweave node's own, derived by the vendored
%%% `ar_chunk_storage' and `ar_storage_module', so an operator may point
%%% `arweave-data-dir' at a data directory an Arweave node filled and read what
%%% is there. What that node keeps in RocksDB beside the chunk files -- the
%%% Merkle paths and the sync records -- this node keeps in a store of its own
%%% under `index' in the data directory, and `import' builds one from the other.
%%%
%%% Two keys carry the weave to a miner, and they are the contract any chunk
%%% source answers. `range' returns the packed chunks of a span, which is what a
%%% pass hashes; it carries no proofs, because the paths are needed only for the
%%% one nonce that meets the difficulty. `chunk-proof' returns everything that
%%% nonce's proof of access needs, and is asked once per solution.
-module(dev_arweave_storage).
-implements(<<"arweave-storage@2.9">>).
-device_libraries([
    lib_arweave_chunk_index,
    lib_arweave_chunk_index_test_vectors,
    lib_arweave_chunks,
    lib_arweave_chunks_test_vectors,
    lib_arweave_entropy,
    lib_arweave_packing,
    lib_arweave_state,
    lib_arweave_storage,
    lib_arweave_sync_record,
    lib_arweave_sync_record_test_vectors
]).
-export([info/1, modules/3, range/3, chunk/3, chunk_proof/3]).
-export([sync_record/3, prepare/3, store/3]).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include("include/ar_chunk_storage.hrl").
-include("include/ar_consensus.hrl").

%% @doc Export only the storage operations, leaving message manipulation to
%% `message@1.0'.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Describe every storage module this node is configured to hold: where it
%% is, what it holds, how much of it is synced, and whether its entropy has been
%% written.
modules(_Base, _Req, Opts) ->
    describe(lib_arweave_storage:modules(Opts), fun described/3, [], Opts).

%% @doc Return the packed chunks of a span of the weave, in offset order.
%%
%% One call reads one recall range. The chunks are returned inline rather than
%% as cache links: a pass hashes them and drops them, and writing the span to
%% the cache to read it back would cost more than the read of the range itself.
%%
%% `range-start' names where the span begins. Its length is `size', or the
%% recall range the protocol defines at the request's `packing-difficulty' --
%% which is 2.5 MiB at replica-2.9's difficulty of ten, not the 25 MiB of the
%% unscaled constant. `packing' with `address' name the form the span must be
%% held in: a chunk packed for another address is one this node cannot mine
%% from.
%%
%% A span no configured module covers is not an error: it is a range this node
%% holds nothing in, which is the answer a miner missing part of its own
%% partition gets too.
range(Base, Req, Opts) ->
    Start = hb_util:int(required(<<"range-start">>, Base, Req, Opts)),
    Difficulty =
        hb_util:int(
            get_first(<<"packing-difficulty">>, Base, Req, 0, Opts)),
    Size =
        hb_util:int(
            get_first(
                <<"size">>,
                Base,
                Req,
                ar_block:get_recall_range_size(Difficulty),
                Opts
            )
        ),
    maybe
        {ok, Packing} ?= requested_packing(Base, Req, Opts),
        {ok, Held, Chunks} ?= read_range(Start, Size, Packing, Opts),
        {ok,
            #{
                <<"range-start">> => Start,
                <<"size">> => Size,
                <<"packing">> => span_packing(Held),
                <<"chunks">> => hb_util:list_to_numbered_message(Chunks)
            }
        }
    end.

%% @doc Return the packed bytes of the chunk of the weave holding a byte.
chunk(Base, Req, Opts) ->
    maybe
        {ok, _Module, _Metadata, Chunk} ?= locate(Base, Req, Opts),
        {ok, Chunk}
    end.

%% @doc Return the chunk of the weave holding a byte with everything a proof of
%% access built from it needs: the packed chunk, the unpacked chunk it deciphers
%% to, and the two Merkle paths that place it in the weave.
%%
%% The unpacking is the expensive half -- thirty-two 8 MiB RandomX runs for one
%% chunk -- and is why this is a separate key from `range'. A pass asks for it
%% once, for the nonce whose hash already met the difficulty.
chunk_proof(Base, Req, Opts) ->
    maybe
        {ok, Module, Metadata, Chunk} ?= locate(Base, Req, Opts),
        Packing = lib_arweave_storage:packing(Module),
        {ok, Unpacked} ?= unpacked(Packing, Metadata, Chunk, Opts),
        {ok,
            Metadata#{
                <<"chunk">> => hb_util:encode(Chunk),
                <<"unpacked-chunk">> => hb_util:encode(Unpacked),
                <<"packing">> => lib_arweave_storage:packing_label(Packing)
            }
        }
    end.

%% @doc Return the intervals a storage module holds, under each record it keeps
%% them in. `module' names one; without it, every configured module answers.
sync_record(Base, Req, Opts) ->
    maybe
        {ok, Modules} ?= requested_modules(Base, Req, Opts),
        describe(Modules, fun records/3, [], Opts)
    end.

%% @doc Generate and store the entropy a module's chunks are packed with, for up
%% to `footprints' more of its range.
%%
%% Bounded and idempotent, and intended to be driven by `~cron@1.0/every'. A
%% pass that finds the module already prepared answers saying so and writes
%% nothing.
prepare(Base, Req, Opts) ->
    Footprints =
        hb_util:int(
            get_first(
                <<"footprints">>,
                Base,
                Req,
                hb_opts:get(<<"arweave-prepare-footprints">>, 1, Opts),
                Opts
            )
        ),
    maybe
        {ok, Module} ?= requested_module(Base, Req, Opts),
        lib_arweave_storage:exclusive(
            Module,
            fun() -> lib_arweave_entropy:prepare(Module, Footprints, Opts) end,
            Opts
        )
    end.

%% @doc Store one chunk of the weave, with the proof that places it there, in
%% whichever configured module covers the offset the proof puts it at.
%%
%% Where the chunk goes is read out of the proof, never out of the request. A
%% chunk's absolute end offset follows the Merkle layout of the transaction
%% holding it, so below the strict data split threshold no arithmetic over the
%% byte it was fetched for recovers it -- and a caller that could name the
%% offset could put any bytes in any slot of a partition this node then mines.
%%
%% `chunk', `data-path', `tx-path' and `offset' are the answer a peer gives to
%% `~arweave@2.9/chunk-proof', with `offset' the byte the chunk was asked for.
%% The block bounds come from this node's own validated block index unless the
%% caller supplies them, because they are what the proof is checked against.
%% `module' names which of several modules covering the offset is to hold the
%% chunk; without it the first covering module takes it.
store(Base, Req, Opts) ->
    Byte = hb_util:int(required(<<"offset">>, Base, Req, Opts)),
    SeekByte = ar_chunk_storage:get_chunk_seek_offset(Byte + 1) - 1,
    maybe
        {ok, Chunk} ?= supplied_chunk(Base, Req, Opts),
        {ok, Bounds} ?= bounds(SeekByte, Base, Req, Opts),
        {ok, Placement} ?= placement(Chunk, SeekByte, Bounds, Base, Req, Opts),
        EndOffset =
            hb_util:int(
                hb_maps:get(<<"absolute-end-offset">>, Placement, 0, Opts)),
        {ok, Module} ?= covering_module(EndOffset, Base, Req, Opts),
        lib_arweave_storage:exclusive(
            Module,
            fun() -> write(Module, Chunk, Placement, Base, Req, Opts) end,
            Opts
        )
    end.

%%% Internal functions.

%% @doc Answer with one message per storage module, stopping at the first whose
%% records cannot be read. A module whose records are unreadable is one this
%% node cannot say what it holds of, and saying nothing of it would read as
%% holding nothing.
describe([], _Fun, Described, _Opts) ->
    {ok, hb_util:list_to_numbered_message(lists:reverse(Described))};
describe([Module | Modules], Fun, Described, Opts) ->
    maybe
        {ok, Records} ?= lib_arweave_sync_record:load(Module, Opts),
        describe(Modules, Fun, [Fun(Module, Records, Opts) | Described], Opts)
    end.

%% @doc Describe one storage module and what it currently holds.
described(Module, Records, Opts) ->
    (lib_arweave_storage:to_message(Module))#{
        <<"path">> => hb_util:bin(lib_arweave_storage:module_path(Module, Opts)),
        <<"synced">> => lib_arweave_sync_record:size(Records, ar_data_sync),
        <<"stored">> => lib_arweave_sync_record:size(Records, ar_chunk_storage),
        <<"prepared">> => lib_arweave_entropy:prepared(Module, Opts),
        <<"prepare-cursor">> => lib_arweave_entropy:cursor(Module, Opts)
    }.

%% @doc Return every record a module keeps, by the name it keeps it under.
records(Module, Records, _Opts) ->
    #{
        <<"module">> => hb_util:bin(lib_arweave_storage:id(Module)),
        <<"records">> =>
            maps:from_list(
                [
                    {
                        lib_arweave_sync_record:label(Id),
                        #{
                            <<"intervals">> =>
                                lib_arweave_sync_record:count(Records, Id),
                            <<"size">> =>
                                lib_arweave_sync_record:size(Records, Id)
                        }
                    }
                ||
                    Id <- lib_arweave_sync_record:ids(Records)
                ]
            )
    }.

%% @doc Read a span of the weave from every module that holds any of it.
%%
%% Upstream reads one module -- `ar_mining_io:find_thread/3' takes the one whose
%% range meets the span most -- because it is choosing which of a pool of I/O
%% threads to send the read to. Nothing here has that concern, so every module
%% meeting the span answers and the answers are merged: a span may begin in a
%% stretch of the weave no module holds and run into one, and a node holding the
%% two halves of a recall range in two modules holds the whole of it.
%%
%% One span is answered in one packing, because a nonce hashes whatever bytes it
%% is given and bytes of two packings are not one range. Two modules in the same
%% packing holding one offset hold the same bytes there, so a chunk read from
%% both is one chunk rather than a conflict.
read_range(Start, Size, Packing, Opts) ->
    case intersecting(Start, Size, Packing, Opts) of
        [] ->
            {ok, Packing, []};
        [First | _Rest] = Modules ->
            Held = lib_arweave_storage:packing(First),
            maybe
                {ok, Pairs} ?=
                    spanned(
                        [
                            Module
                        ||
                            Module <- Modules,
                            lib_arweave_storage:packing(Module) == Held
                        ],
                        Start,
                        Size,
                        [],
                        Opts
                    ),
                {ok, Held,
                    [
                        #{
                            <<"absolute-end-offset">> => EndOffset,
                            <<"chunk">> => Chunk
                        }
                    ||
                        {EndOffset, Chunk} <- Pairs
                    ]
                }
            end
    end.

%% @doc Read the span from each of the given modules, stopping at the first
%% whose records or files cannot be read, and merge what they hold into one
%% ascending run. The merge is by absolute end offset, which is where a chunk
%% two of them hold sits in both.
spanned([], _Start, _Size, Pairs, _Opts) ->
    {ok, lists:ukeysort(1, Pairs)};
spanned([Module | Modules], Start, Size, Pairs, Opts) ->
    maybe
        {ok, Records} ?= lib_arweave_sync_record:load(Module, Opts),
        {ok, Read} ?=
            lib_arweave_chunks:read_range(Module, Start, Size, Records, Opts),
        spanned(Modules, Start, Size, Read ++ Pairs, Opts)
    end.

%% @doc Every configured module in the given packing whose own range shares a
%% byte with the span. `any' meets every packing, which is what a request naming
%% none asks after.
intersecting(Start, Size, Packing, Opts) ->
    [
        Module
    ||
        Module <- lib_arweave_storage:modules(Opts),
        Packing == any orelse lib_arweave_storage:packing(Module) == Packing,
        meets(lib_arweave_storage:range(Module), Start, Start + Size)
    ].

%% @doc Hold when a module's range and a span of the weave share a byte. A
%% module's range excludes its own left bound: the byte at it is the last byte
%% of the module below.
meets({RangeStart, RangeEnd}, Start, End) ->
    Start < RangeEnd andalso End > RangeStart.

%% @doc Name the packing a span was read in. A span this node holds nothing in
%% is answered in the packing the request asked after, which may be any packing
%% at all -- and never in a packing the request excluded.
span_packing(any) ->
    <<"any">>;
span_packing(Packing) ->
    lib_arweave_storage:packing_label(Packing).

%% @doc Find the chunk of the weave holding a byte, in the module that holds it,
%% with the metadata that places it. A byte no module holds a chunk for is a
%% 404: this node cannot answer for it, which is different from the byte being
%% outside the weave.
locate(Base, Req, Opts) ->
    Byte = hb_util:int(required(<<"offset">>, Base, Req, Opts)),
    maybe
        {ok, Packing} ?= requested_packing(Base, Req, Opts),
        located(lib_arweave_storage:covering(Byte + 1, Packing, Opts), Byte, Opts)
    end.
located([], _Byte, _Opts) ->
    {error, error_message(404, <<"chunk-not-held">>,
        <<"No storage module of this node holds a chunk at that offset.">>)};
located([Module | Rest], Byte, Opts) ->
    case lib_arweave_chunk_index:get_by_byte(Module, Byte, Opts) of
        {ok, Metadata} -> chunk_bytes(Module, Metadata, Byte, Rest, Opts);
        not_found -> located(Rest, Byte, Opts)
    end.

%% @doc Read the bytes of a chunk the index placed, out of the slot the index's
%% own offset names.
%%
%% The byte is what a miner recalled; the absolute end offset is where the
%% Merkle layout of the transaction holding the chunk put it, and that offset is
%% what the chunk file was keyed on. Above the strict data split threshold the
%% two derive the same slot, because every chunk fills its own bucket. Below it
%% nothing is bucket aligned and only the offset says which slot was written.
chunk_bytes(Module, Metadata, Byte, Rest, Opts) ->
    PaddedEndOffset =
        ar_block:get_chunk_padded_offset(
            hb_util:int(
                hb_maps:get(<<"absolute-end-offset">>, Metadata, 0, Opts))
        ),
    maybe
        {ok, Records} ?= lib_arweave_sync_record:load(Module, Opts),
        held(
            lib_arweave_sync_record:is_recorded(
                Records, ar_chunk_storage, PaddedEndOffset),
            {Module, Metadata, PaddedEndOffset},
            Byte,
            Rest,
            Opts
        )
    end.

%% @doc Read the slot a module's record says holds a chunk.
%%
%% A slot the record does not cover is not a chunk of the weave in the form this
%% module holds one: a replica-2.9 module keeps raw entropy in the buckets it
%% has no data for, and keeps a chunk that arrived before that entropy
%% unenciphered until the preparation pass reaches it, and the bytes alone do
%% not say which of the three a slot holds. What the record does not cover the
%% index may still hold outright -- a chunk shorter than a slot has no slot of
%% its own, and lives in the index as it does on an Arweave node. A slot the
%% record covers that the files do not hold is an index ahead of its data, which
%% is the state a crash between the two writes leaves, and is answered by trying
%% the next module.
held(true, {Module, Metadata, PaddedEndOffset}, Byte, Rest, Opts) ->
    case
        lib_arweave_chunks:read(
            Module,
            PaddedEndOffset - 1,
            PaddedEndOffset - ?DATA_CHUNK_SIZE,
            Opts
        )
    of
        {ok, {_EndOffset, Chunk}} -> {ok, Module, Metadata, Chunk};
        _Other -> located(Rest, Byte, Opts)
    end;
held(false, {Module, Metadata, PaddedEndOffset}, Byte, Rest, Opts) ->
    case lib_arweave_chunk_index:get_chunk(Module, PaddedEndOffset, Opts) of
        {ok, Chunk} -> {ok, Module, Metadata, Chunk};
        _Absent -> located(Rest, Byte, Opts)
    end.

%% @doc Return the unpacked form of a stored chunk, padded to the size a proof
%% is taken at. An unpacked module holds it already.
unpacked(unpacked, _Metadata, Chunk, _Opts) ->
    {ok, ar_packing_server:pad_chunk(Chunk)};
unpacked({replica_2_9, RewardAddr}, Metadata, Chunk, Opts) ->
    maybe
        {ok, Result} ?=
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-spora@2.9">>,
                    <<"chunk">> => hb_util:encode(Chunk),
                    <<"chunk-size">> =>
                        hb_maps:get(<<"chunk-size">>, Metadata, 0, Opts),
                    <<"absolute-end-offset">> =>
                        hb_maps:get(
                            <<"absolute-end-offset">>, Metadata, 0, Opts),
                    <<"packing">> =>
                        #{
                            <<"format">> => <<"replica-2-9">>,
                            <<"reward-addr">> => hb_util:encode(RewardAddr)
                        }
                },
                <<"unpack">>,
                Opts
            ),
        {ok,
            ar_packing_server:pad_chunk(
                hb_maps:get(<<"chunk">>, Result, <<>>, Opts))
        }
    end;
unpacked(Packing, _Metadata, _Chunk, _Opts) ->
    {error, error_message(422, <<"unsupported-packing">>,
        <<"This node cannot unpack a `",
            (lib_arweave_storage:packing_label(Packing))/binary,
            "' chunk.">>)}.

%% @doc The packing a request is asking after: the one it names, or the one this
%% node's own modules are held in.
requested_packing(Base, Req, Opts) ->
    case get_first(<<"packing">>, Base, Req, not_found, Opts) of
        not_found -> {ok, any};
        <<"any">> -> {ok, any};
        <<"unpacked">> -> {ok, unpacked};
        <<"replica-2-9">> -> replica_packing(Base, Req, Opts);
        Format ->
            {error, error_message(422, <<"unsupported-packing">>,
                <<"This node holds no `", Format/binary, "' storage module.">>)}
    end.

%% @doc Build the replica-2.9 packing term for the address a request names, or
%% for the one this node mines to.
replica_packing(Base, Req, Opts) ->
    case get_first(<<"address">>, Base, Req, not_found, Opts) of
        not_found ->
            {ok,
                {replica_2_9,
                    ar_wallet:to_address(
                        hb_opts:get(priv_wallet, hb:wallet(), Opts))}
            };
        Address ->
            {ok, {replica_2_9, hb_util:native_id(Address)}}
    end.

%% @doc The modules a request names: the one it named, or all of them.
requested_modules(Base, Req, Opts) ->
    case get_first(<<"module">>, Base, Req, not_found, Opts) of
        not_found -> {ok, lib_arweave_storage:modules(Opts)};
        _Named -> named_module(Base, Req, Opts)
    end.

named_module(Base, Req, Opts) ->
    maybe
        {ok, Module} ?= requested_module(Base, Req, Opts),
        {ok, [Module]}
    end.

%% @doc The single module a request names. A request naming none takes the only
%% configured module, because a node with one module has no ambiguity to
%% resolve; a node with several must say which.
requested_module(Base, Req, Opts) ->
    Modules = lib_arweave_storage:modules(Opts),
    case get_first(<<"module">>, Base, Req, not_found, Opts) of
        not_found -> only_module(Modules);
        Named ->
            found_module(
                lib_arweave_storage:find(hb_util:list(Named), Modules),
                Named
            )
    end.

only_module([Module]) ->
    {ok, Module};
only_module([]) ->
    {error, error_message(422, <<"no-storage-modules">>,
        <<"This node is configured with no Arweave storage modules.">>)};
only_module(Modules) ->
    {error, error_message(422, <<"ambiguous-storage-module">>,
        <<"This node holds several storage modules; name the one to use: ",
            (named(Modules))/binary, ".">>)}.

%% @doc Name every configured module, so that a request that had to choose one
%% is told what there was to choose from.
named(Modules) ->
    hb_util:bin(
        lists:join(
            ", ",
            [ lib_arweave_storage:id(Module) || Module <- Modules ]
        )
    ).

found_module(not_found, Named) ->
    {error, error_message(404, <<"unknown-storage-module">>,
        <<"This node holds no storage module named `",
            (hb_util:bin(Named))/binary, "'.">>)};
found_module(Module, _Named) ->
    {ok, Module}.

%% @doc The module a chunk at an offset belongs in: the one the request names,
%% or the first configured module covering the offset.
%%
%% A node holding several modules over one offset has no other way to say which
%% of them a caller meant. A named module still has to cover the offset itself:
%% a chunk written into a module the proof does not place it in would leave that
%% module's record claiming bytes of the weave it does not hold.
covering_module(EndOffset, Base, Req, Opts) ->
    maybe
        {ok, Packing} ?= requested_packing(Base, Req, Opts),
        Covering = lib_arweave_storage:covering(EndOffset, Packing, Opts),
        case get_first(<<"module">>, Base, Req, not_found, Opts) of
            not_found -> covering_module(Covering, EndOffset);
            Named -> named_covering(Covering, Named, EndOffset, Opts)
        end
    end.
covering_module([], EndOffset) ->
    {error, error_message(422, <<"offset-not-covered">>,
        <<"No storage module of this node covers offset ",
            (hb_util:bin(EndOffset))/binary, ".">>)};
covering_module([Module | _Rest], _EndOffset) ->
    {ok, Module}.

%% @doc The module a request names, refused when this node holds no such module
%% and refused again when the one it holds does not cover the offset.
named_covering(Covering, Named, EndOffset, Opts) ->
    Modules = lib_arweave_storage:modules(Opts),
    case lib_arweave_storage:find(hb_util:list(Named), Modules) of
        not_found ->
            found_module(not_found, Named);
        Module ->
            covering_named(
                lists:member(Module, Covering), Module, Named, EndOffset)
    end.

covering_named(true, Module, _Named, _EndOffset) ->
    {ok, Module};
covering_named(false, _Module, Named, EndOffset) ->
    {error, error_message(422, <<"module-not-covering">>,
        <<"The storage module `", (hb_util:bin(Named))/binary,
            "' does not cover offset ",
            (hb_util:bin(EndOffset))/binary, ".">>)}.

%% @doc The bytes of a chunk a request supplies, refused if they are more than a
%% chunk of the weave holds.
supplied_chunk(Base, Req, Opts) ->
    Chunk = hb_util:decode(required(<<"chunk">>, Base, Req, Opts)),
    case byte_size(Chunk) > ?DATA_CHUNK_SIZE of
        true ->
            {error, error_message(422, <<"invalid-chunk-size">>,
                <<"The chunk is larger than a chunk of the weave.">>)};
        false ->
            {ok, Chunk}
    end.

%% @doc The bounds of the block that wrote the chunk at an offset, read from
%% the block index of this node's own tip.
%%
%% A caller may name them, and a caller that names them is checked against the
%% index rather than believed: the bounds are what the two Merkle paths are
%% walked against, so a caller free to choose them is a caller free to walk any
%% bytes to any offset of a partition this node then mines. A node whose chain
%% does not reach the offset has nothing to check against and takes what it was
%% given -- which is a node that cannot mine that offset either, because a
%% solution there has no block to prove against.
bounds(SeekByte, Base, Req, Opts) ->
    case get_first(<<"tx-root">>, Base, Req, not_found, Opts) of
        not_found ->
            indexed_bounds(SeekByte, Opts);
        TXRoot ->
            BlockStart =
                hb_util:int(
                    required(<<"block-start-offset">>, Base, Req, Opts)),
            BlockSize =
                hb_util:int(required(<<"block-size">>, Base, Req, Opts)),
            agreed(
                #{
                    <<"tx-root">> => TXRoot,
                    <<"block-start">> => BlockStart,
                    <<"block-end">> => BlockStart + BlockSize
                },
                indexed_bounds(SeekByte, Opts),
                Opts
            )
    end.

%% @doc Hold a caller's bounds only where this node's own block index agrees
%% with them, field for field. An index that does not reach the offset answers
%% for no block, and there is nothing to disagree with.
agreed(Named, {ok, Indexed}, Opts) ->
    case bound_fields(Named, Opts) == bound_fields(Indexed, Opts) of
        true ->
            {ok, Indexed};
        false ->
            {error, error_message(422, <<"bounds-not-indexed">>,
                <<"The block bounds named for this chunk are not the bounds "
                    "this node's block index has at that offset.">>)}
    end;
agreed(Named, _Unindexed, _Opts) ->
    {ok, Named}.

%% @doc The three fields a chunk's block bounds are, spelled the one way.
bound_fields(Bounds, Opts) ->
    {
        hb_util:bin(hb_maps:get(<<"tx-root">>, Bounds, <<>>, Opts)),
        hb_util:int(hb_maps:get(<<"block-start">>, Bounds, 0, Opts)),
        hb_util:int(hb_maps:get(<<"block-end">>, Bounds, 0, Opts))
    }.

%% @doc Read a chunk's block bounds from the block index this node validated.
%% The seek byte is counted from zero, which is the offset
%% `~arweave-block-index@2.9/bounds' answers for and the one upstream's
%% `ar_block_index:get_block_bounds/1' is called with.
indexed_bounds(SeekByte, Opts) ->
    maybe
        {ok, Tip} ?= hb_ao:resolve(#{ <<"device">> => <<"arweave@2.9">> },
            <<"tip">>, Opts),
        Index = lib_arweave_state:block_index(Tip, Opts),
        hb_ao:resolve(
            Index#{ <<"device">> => <<"arweave-block-index@2.9">> },
            #{ <<"path">> => <<"bounds">>, <<"offset">> => SeekByte },
            Opts
        )
    end.

%% @doc Walk the two paths and answer with where they place the chunk: its
%% absolute end offset, its size, the data root of the transaction holding it,
%% and its offset within that transaction.
%%
%% The chunk itself is bound by its identifier rather than by unpacking: a chunk
%% arriving from a peer is unpacked, and what has to hold is that these bytes
%% are the bytes the Merkle leaf names, at the size it names them at.
placement(Chunk, SeekByte, Bounds, Base, Req, Opts) ->
    BlockStart = hb_util:int(hb_maps:get(<<"block-start">>, Bounds, 0, Opts)),
    BlockEnd = hb_util:int(hb_maps:get(<<"block-end">>, Bounds, 0, Opts)),
    maybe
        {ok, Result} ?=
            hb_ao:resolve(
                #{
                    <<"device">> => <<"arweave-spora@2.9">>,
                    <<"poa">> =>
                        #{
                            <<"tx-path">> =>
                                required(<<"tx-path">>, Base, Req, Opts),
                            <<"data-path">> =>
                                required(<<"data-path">>, Base, Req, Opts),
                            <<"chunk">> => <<>>
                        },
                    <<"tx-root">> =>
                        hb_maps:get(<<"tx-root">>, Bounds, <<>>, Opts),
                    <<"block-start-offset">> => BlockStart,
                    <<"block-size">> => BlockEnd - BlockStart,
                    <<"recall-offset">> => SeekByte,
                    <<"sub-chunk-index">> => 0,
                    <<"expected-chunk-id">> =>
                        hb_util:encode(ar_tx:generate_chunk_id(Chunk))
                },
                <<"validate">>,
                Opts
            ),
        ok ?=
            sized(
                hb_util:int(hb_maps:get(<<"chunk-size">>, Result, 0, Opts)),
                byte_size(Chunk)
            ),
        {ok,
            Result#{
                <<"tx-root">> => hb_maps:get(<<"tx-root">>, Bounds, <<>>, Opts)
            }
        }
    end.

%% @doc Require the chunk to be the size the Merkle leaf gives it. The
%% identifier already binds the bytes, but a leaf naming a different size for
%% the same bytes would place the chunk over a span it does not fill.
sized(ChunkSize, ChunkSize) ->
    ok;
sized(_ChunkSize, _Supplied) ->
    {error, error_message(422, <<"invalid-chunk-size">>,
        <<"The chunk is not the size the proof gives it.">>)}.

%% @doc Write a proven chunk into a module: its bytes into the chunk file, its
%% Merkle paths into the index, and the record of what is now held.
%%
%% The records are saved last. A crash before them leaves a chunk on disk that
%% no record claims, which the next pass writes again; a crash after them would
%% leave a record claiming bytes that are not there, which a miner would hash.
write(Module, Chunk, Placement, Base, Req, Opts) ->
    Packing = lib_arweave_storage:packing(Module),
    EndOffset =
        hb_util:int(hb_maps:get(<<"absolute-end-offset">>, Placement, 0, Opts)),
    PaddedEndOffset = ar_block:get_chunk_padded_offset(EndOffset),
    maybe
        {ok, Records} ?= lib_arweave_sync_record:load(Module, Opts),
        {ok, Stored} ?=
            written(
                ar_chunk_storage:is_storage_supported(
                    EndOffset, byte_size(Chunk), Packing),
                Module,
                PaddedEndOffset,
                Chunk,
                Records,
                Opts
            ),
        ok ?=
            lib_arweave_chunk_index:put(
                Module, metadata(Placement, Base, Req, Opts), Opts),
        ok ?= lib_arweave_sync_record:save(Module, Stored, Opts),
        {ok,
            #{
                <<"stored">> => true,
                <<"absolute-end-offset">> => EndOffset,
                <<"padded-end-offset">> => PaddedEndOffset,
                <<"module">> => hb_util:bin(lib_arweave_storage:id(Module))
            }
        }
    end.

%% @doc Write a chunk's bytes wherever its module holds them.
%%
%% A chunk a module's files cannot hold -- one shorter than a chunk, in a module
%% that keeps no chunk sizes -- lives in the index alone, which is where an
%% Arweave node keeps it too.
written(false, Module, PaddedEndOffset, Chunk, Records, Opts) ->
    maybe
        ok ?=
            lib_arweave_chunk_index:put_chunk(
                Module, PaddedEndOffset, Chunk, Opts),
        {ok,
            synced(
                Records,
                lib_arweave_storage:packing(Module),
                PaddedEndOffset,
                byte_size(Chunk)
            )
        }
    end;
written(true, Module, PaddedEndOffset, Chunk, Records, Opts) ->
    packed(
        lib_arweave_storage:packing(Module),
        Module,
        PaddedEndOffset,
        Chunk,
        Records,
        Opts
    ).

%% @doc Store a chunk in the form its module holds chunks in. A replica-2.9
%% module holds it enciphered with the entropy of its bucket, which may not have
%% been written yet -- in which case the chunk waits in the slot, unenciphered,
%% under the record that says so, and the preparation pass enciphers it when it
%% reaches that bucket.
packed(unpacked, Module, PaddedEndOffset, Chunk, Records, Opts) ->
    maybe
        ok ?=
            lib_arweave_chunks:write(
                Module,
                PaddedEndOffset,
                ar_packing_server:pad_chunk(Chunk),
                Opts
            ),
        {ok,
            stored(
                synced(Records, unpacked, PaddedEndOffset, ?DATA_CHUNK_SIZE),
                PaddedEndOffset
            )
        }
    end;
packed({replica_2_9, _Addr}, Module, PaddedEndOffset, Chunk, Records, Opts) ->
    case
        lib_arweave_entropy:encipher_stored(
            Module, PaddedEndOffset, Chunk, Records, Opts)
    of
        {ok, Stored} -> {ok, Stored};
        not_prepared -> waiting(Module, PaddedEndOffset, Chunk, Records, Opts);
        {error, Error} -> {error, Error}
    end;
packed(Packing, _Module, _PaddedEndOffset, _Chunk, _Records, _Opts) ->
    {error, error_message(422, <<"unsupported-packing">>,
        <<"This node cannot pack chunks for a `",
            (lib_arweave_storage:packing_label(Packing))/binary,
            "' storage module.">>)}.

%% @doc Store a chunk in a bucket whose entropy has not been generated yet.
waiting(Module, PaddedEndOffset, Chunk, Records, Opts) ->
    maybe
        ok ?=
            lib_arweave_chunks:write(
                Module,
                PaddedEndOffset,
                ar_packing_server:pad_chunk(Chunk),
                Opts
            ),
        {ok,
            lib_arweave_sync_record:add(
                Records,
                ar_chunk_storage:sync_record_id(unpacked_padded),
                PaddedEndOffset,
                PaddedEndOffset - ?DATA_CHUNK_SIZE
            )
        }
    end.

%% @doc Record that a module now holds data at an offset, in its own packing.
synced(Records, Packing, PaddedEndOffset, ChunkSize) ->
    lib_arweave_sync_record:add(
        Records,
        ar_data_sync,
        Packing,
        PaddedEndOffset,
        start_offset(PaddedEndOffset, ChunkSize)
    ).

%% @doc The first byte of the weave a stored chunk holds.
%%
%% Above the strict data split threshold a chunk owns its whole bucket, padding
%% included, and the record covers the bucket. Below it a chunk may be any size
%% and holds only the bytes it has: a record covering the bucket would claim
%% bytes of the chunks before it, which this node would then answer for and
%% never fetch. At the very start of the weave it would claim bytes below zero,
%% which is not a range this record can hold at all.
start_offset(PaddedEndOffset, _ChunkSize)
        when PaddedEndOffset > ?STRICT_DATA_SPLIT_THRESHOLD ->
    PaddedEndOffset - ?DATA_CHUNK_SIZE;
start_offset(PaddedEndOffset, ChunkSize) ->
    PaddedEndOffset - ChunkSize.

%% @doc Record that a module's chunk files now hold a chunk at an offset.
stored(Records, PaddedEndOffset) ->
    lib_arweave_sync_record:add(
        Records,
        ar_chunk_storage,
        PaddedEndOffset,
        PaddedEndOffset - ?DATA_CHUNK_SIZE
    ).

%% @doc The metadata that places a chunk in the weave. Every offset and root in
%% it is what the proof walk resolved to, not what the request asserted.
metadata(Placement, Base, Req, Opts) ->
    #{
        <<"absolute-end-offset">> =>
            hb_maps:get(<<"absolute-end-offset">>, Placement, 0, Opts),
        <<"chunk-size">> => hb_maps:get(<<"chunk-size">>, Placement, 0, Opts),
        <<"relative-offset">> =>
            hb_maps:get(<<"relative-offset">>, Placement, 0, Opts),
        <<"tx-root">> => hb_maps:get(<<"tx-root">>, Placement, <<>>, Opts),
        <<"data-root">> => hb_maps:get(<<"data-root">>, Placement, <<>>, Opts),
        <<"tx-path">> => required(<<"tx-path">>, Base, Req, Opts),
        <<"data-path">> => required(<<"data-path">>, Base, Req, Opts)
    }.

%% @doc Read a field from the request, falling back to the base message.
%%
%% These are data fields, not keys to resolve, so they are read with
%% `hb_maps:get/4' rather than `hb_ao:get/4'. Resolving a key against a message
%% that names this device dispatches back into the device -- so reading the
%% `range' key of a `chunk' request with `hb_ao:get' would read a span of the
%% weave rather than return what was supplied.
get_first(Key, Base, Req, Default, Opts) ->
    case hb_maps:get(Key, Req, not_found, Opts) of
        not_found -> hb_maps:get(Key, Base, Default, Opts);
        Value -> Value
    end.

%% @doc Read a key that has no meaningful default.
required(Key, Base, Req, Opts) ->
    case get_first(Key, Base, Req, not_found, Opts) of
        not_found -> throw({'missing-key', Key});
        Value -> Value
    end.

%% @doc Build the standard error body.
error_message(Status, Message, Detail) ->
    #{
        <<"status">> => Status,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.
