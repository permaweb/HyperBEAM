%%% @doc Preparing a storage module for replica-2.9: writing the entropy each
%%% of its chunks is enciphered with, and combining that entropy with the chunk
%%% data whichever of the two arrives second.
%%%
%%% A replica-2.9 chunk is enciphered by exclusive-or with 256 KiB of entropy
%%% assembled from thirty-two separate 8 MiB blobs, one per sub-chunk. Each blob
%%% is a RandomX run, and each is sliced across a thousand and twenty-four
%%% chunks spread through the partition. That is the whole reason a node
%%% `prepare's a partition rather than packing a chunk when it arrives:
%%% generating the entropy for one chunk alone costs the same thirty-two runs as
%%% generating it for the thousand and twenty-four that share those blobs.
%%%
%%% So the entropy is written first, into the chunk file slots themselves. A
%%% slot then holds either raw entropy waiting for data, or data already
%%% enciphered with it, and the bytes alone do not say which -- the sync records
%%% do. A chunk arriving before the entropy is stored unenciphered under the
%%% `unpacked_padded' record, and this module enciphers it in place when the
%%% entropy for its bucket is generated.
%%%
%%% The cursor is the file an Arweave node keeps, in the format it keeps it in,
%%% so a partition this node prepared is one that node can carry on preparing,
%%% and the other way round.
-module(lib_arweave_entropy).
-export([prepare/3, recorded/3, prepared/2, cursor/2, advance/3]).
-export([encipher_stored/5, offsets/3]).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include("include/ar_chunk_storage.hrl").
-include("include/ar_consensus.hrl").

%%% The file an Arweave node records its preparation progress in.
-define(CURSOR, "prepare_replica_2_9_cursor").

%%% The record naming the buckets entropy has been written for.
-define(ENTROPY_RECORD, ar_chunk_storage_replica_2_9_5_entropy).

%%% The record an Arweave node wrote entropy under before 2.9.5. Read, never
%%% written: a partition prepared by such a node is still prepared.
-define(LEGACY_ENTROPY_RECORD, ar_chunk_storage_replica_2_9_1_entropy).

%% @doc Generate and store the entropy for up to `Footprints' more of a
%% module's partition, starting where the last pass stopped.
%%
%% One footprint is the unit the protocol makes cheap: thirty-two blobs, and the
%% thousand and twenty-four buckets they are sliced across. A pass that wrote
%% part of a footprint would have to generate those blobs again to write the
%% rest, so a pass is bounded in footprints rather than in buckets.
%%
%% Answers with what it did and where it stopped, which is what tells a caller
%% driving this from `~cron@1.0' whether there is more to do.
prepare(Module, Footprints, Opts) ->
    {Start, End} = lib_arweave_storage:range(Module),
    Packing = lib_arweave_storage:packing(Module),
    maybe
        {ok, RewardAddr} ?= entropy_packing(Packing),
        {ok, Records} ?= lib_arweave_sync_record:load(Module, Opts),
        State =
            #{
                <<"module">> => Module,
                <<"reward-addr">> => RewardAddr,
                <<"module-start">> => Start,
                <<"module-end">> => ar_chunk_storage:get_chunk_bucket_end(End),
                <<"records">> => Records,
                <<"loaded">> => Records
            },
        footprints(Footprints, cursor(Module, Opts), 0, State, Opts)
    end.

%% @doc Hold for a bucket whose entropy this module has already written. The
%% pre-2.9.5 record is consulted too, because a partition an older Arweave node
%% prepared is prepared.
recorded(Records, PaddedEndOffset, Packing) ->
    BucketStart = ar_chunk_storage:get_chunk_bucket_start(PaddedEndOffset),
    lib_arweave_sync_record:is_recorded(
        Records, ?ENTROPY_RECORD, Packing, BucketStart + 1)
        orelse
    lib_arweave_sync_record:is_recorded(
        Records, ?LEGACY_ENTROPY_RECORD, BucketStart + 1).

%% @doc Hold for a module whose whole range has been prepared. Derived from the
%% cursor, exactly as an Arweave node derives it.
prepared(Module, Opts) ->
    {_Start, End} = lib_arweave_storage:range(Module),
    cursor(Module, Opts) > End.

%% @doc The offset a module's preparation has reached. A module that has never
%% been prepared starts one byte into its own range, which is the first byte any
%% bucket of it can be reached by.
cursor(Module, Opts) ->
    {Start, _End} = lib_arweave_storage:range(Module),
    Path =
        filename:join(
            lib_arweave_storage:chunk_dir(Module, Opts), ?CURSOR),
    case file:read_file(Path) of
        {ok, Bin} -> stored_cursor(catch binary_to_term(Bin, [safe]), Start + 1);
        {error, _} -> Start + 1
    end.

%% @doc Record that preparation has reached an offset. The directory is ensured
%% first: a pass that wrote no entropy still saves its place, and on a module
%% nothing has ever been written to there is no directory yet.
%%
%% A pass that ends where the file already stands writes nothing. This is the
%% Arweave node's own cursor file, in the Arweave node's own directory, and a
%% prepared module reaches the end of its range on every pass it is given.
advance(Module, Cursor, Opts) ->
    advance(Module, Cursor, cursor(Module, Opts), Opts).
advance(_Module, Cursor, Cursor, _Opts) ->
    ok;
advance(Module, Cursor, _Reached, Opts) ->
    Path =
        filename:join(
            lib_arweave_storage:chunk_dir(Module, Opts), ?CURSOR),
    maybe
        ok ?= lib_arweave_chunks:ensure_dir(Module, Opts),
        file:write_file(Path, term_to_binary(Cursor))
    end.

%% @doc Encipher a chunk that arrived after the entropy for its bucket was
%% written, and store the packed result in the slot the entropy occupies.
%%
%% Returns the updated records; the caller saves them once its own writes are
%% durable. A bucket with no entropy yet is `not_prepared', which is a state of
%% the module rather than a failure of the chunk: the caller stores the chunk
%% unenciphered under the `unpacked_padded' record and this module enciphers it
%% when the pass reaches its bucket.
encipher_stored(Module, PaddedEndOffset, Chunk, Records, Opts) ->
    Packing = lib_arweave_storage:packing(Module),
    StartOffset = PaddedEndOffset - ?DATA_CHUNK_SIZE,
    maybe
        true ?= recorded(Records, PaddedEndOffset, Packing) orelse not_prepared,
        {ok, {_EndOffset, Entropy}} ?=
            lib_arweave_chunks:read(Module, StartOffset, StartOffset, Opts),
        ok ?=
            lib_arweave_chunks:write(
                Module,
                PaddedEndOffset,
                lib_arweave_packing:encipher(
                    ar_packing_server:pad_chunk(Chunk), Entropy),
                Opts
            ),
        {ok, stored(Records, PaddedEndOffset, Packing)}
    end.

%% @doc The buckets one footprint of entropy covers, oldest first. Exposed for
%% the vectors and for an operator asking what a pass would touch.
offsets(Module, BucketEndOffset, _Opts) ->
    {_Start, End} = lib_arweave_storage:range(Module),
    ar_entropy_gen:entropy_offsets(
        BucketEndOffset, ar_chunk_storage:get_chunk_bucket_end(End)).

%%% Internal functions.

%% @doc Hold only for a module whose packing entropy is written for.
entropy_packing({replica_2_9, RewardAddr}) ->
    {ok, RewardAddr};
entropy_packing(_Packing) ->
    {error,
        #{
            <<"status">> => 422,
            <<"message">> => <<"unsupported-packing">>,
            <<"detail">> =>
                <<"Only a `replica-2-9' storage module is prepared with "
                    "entropy.">>
        }
    }.

%% @doc Write one footprint at a time until the pass has written the number it
%% was given, or the module's range ends.
footprints(0, Cursor, Written, State, Opts) ->
    done(Cursor, Written, false, State, Opts);
footprints(Footprints, Cursor, Written, State, Opts) ->
    BucketEndOffset = ar_chunk_storage:get_chunk_bucket_end(Cursor),
    case BucketEndOffset > field(<<"module-end">>, State) of
        true ->
            done(Cursor, Written, true, State, Opts);
        false ->
            footprint(Footprints, BucketEndOffset, Written, State, Opts)
    end.

%% @doc Write the footprint anchored on one bucket, unless its entropy is
%% already there, and carry on from the next bucket that needs one.
footprint(Footprints, BucketEndOffset, Written, State, Opts) ->
    Records = field(<<"records">>, State),
    RewardAddr = field(<<"reward-addr">>, State),
    Packing = {replica_2_9, RewardAddr},
    case recorded(Records, BucketEndOffset, Packing) of
        true ->
            footprints(
                Footprints,
                next(BucketEndOffset, Records, Packing),
                Written,
                State,
                Opts
            );
        false ->
            written(
                Footprints,
                BucketEndOffset,
                Written,
                store(BucketEndOffset, State, Opts),
                Opts
            )
    end.

%% @doc Carry on from the bucket after the footprint just written.
written(Footprints, BucketEndOffset, Written, State, Opts) ->
    Records = field(<<"records">>, State),
    Packing = {replica_2_9, field(<<"reward-addr">>, State)},
    footprints(
        Footprints - 1,
        next(BucketEndOffset, Records, Packing),
        Written + 1,
        State,
        Opts
    ).

%% @doc Generate one footprint's thirty-two entropies and place every slice of
%% them. The offsets, the slicing and the keys are the vendored derivation, so
%% the entropy this node writes is the entropy the protocol says a chunk at that
%% offset is enciphered with.
store(BucketEndOffset, State, Opts) ->
    RewardAddr = field(<<"reward-addr">>, State),
    Module = field(<<"module">>, State),
    Entropies = lib_arweave_packing:entropies(RewardAddr, BucketEndOffset, Opts),
    Records =
        ar_entropy_gen:map_entropies(
            Entropies,
            ar_entropy_gen:entropy_offsets(
                BucketEndOffset, field(<<"module-end">>, State)),
            field(<<"module-start">>, State),
            ar_entropy_gen:generate_entropy_keys(RewardAddr, BucketEndOffset),
            RewardAddr,
            fun slice/6,
            [Module, Opts],
            field(<<"records">>, State)
        ),
    State#{ <<"records">> => Records }.

%% @doc Place one bucket's worth of entropy: enciphering a chunk that is already
%% waiting in the slot, or occupying the slot until one arrives.
%%
%% A slot holding raw entropy carries the bucket's own relative offset, which is
%% zero and is therefore written as the special zero offset. That value has no
%% meaning of its own here; it is what makes the slot read back as written
%% rather than as empty.
slice(ChunkEntropy, BucketEndOffset, RewardAddr, Module, Opts, Records) ->
    Packing = {replica_2_9, RewardAddr},
    Byte = ar_chunk_storage:get_chunk_byte_from_bucket_end(BucketEndOffset),
    {Waiting, PaddedEndOffset} = waiting(Records, Byte, BucketEndOffset),
    case chunk(Waiting, Module, Byte, PaddedEndOffset, ChunkEntropy, Opts) of
        {ok, Chunk} ->
            placed(
                lib_arweave_chunks:write(Module, PaddedEndOffset, Chunk, Opts),
                Waiting,
                PaddedEndOffset,
                Packing,
                Records,
                Opts
            );
        {error, Error} ->
            ?event(warning,
                {arweave_entropy_slice_failed,
                    {bucket_end_offset, BucketEndOffset},
                    {error, Error}
                },
                Opts
            ),
            Records
    end.

%% @doc Return the chunk waiting in a bucket's slot, if one is, and the offset
%% it was stored at. A chunk stored before the entropy arrived sits at its own
%% padded end offset rather than at the bucket's, and near the strict data split
%% threshold the two need not name the same bucket at all -- in which case the
%% slot belongs to neither and the entropy takes it.
%%
%% A bucket the chunk-storage record already covers holds a chunk that has been
%% enciphered, so it is not waiting for anything. The record that says a chunk
%% is waiting outlives the enciphering -- an Arweave node clears it only when
%% the chunk is deleted -- so without this a bucket whose entropy record was
%% lost and rebuilt would have its chunk enciphered a second time, which
%% destroys it.
waiting(Records, Byte, BucketEndOffset) ->
    case lib_arweave_sync_record:is_recorded(Records, ar_chunk_storage, Byte + 1) of
        true -> {false, BucketEndOffset};
        false -> unenciphered(Records, Byte, BucketEndOffset)
    end.

unenciphered(Records, Byte, BucketEndOffset) ->
    case
        lib_arweave_sync_record:interval(
            Records, ar_chunk_storage:sync_record_id(unpacked_padded), Byte + 1)
    of
        not_found ->
            {false, BucketEndOffset};
        {_IntervalEnd, IntervalStart} ->
            EndOffset =
                IntervalStart
                    + hb_util:floor_int(Byte - IntervalStart, ?DATA_CHUNK_SIZE)
                    + ?DATA_CHUNK_SIZE,
            same_bucket(
                ar_chunk_storage:get_chunk_bucket_end(EndOffset),
                BucketEndOffset,
                EndOffset
            )
    end.

same_bucket(BucketEndOffset, BucketEndOffset, EndOffset) ->
    {true, EndOffset};
same_bucket(_Other, BucketEndOffset, _EndOffset) ->
    {false, BucketEndOffset}.

%% @doc The bytes to write into a bucket's slot: the entropy itself, or the
%% chunk that was waiting there enciphered with it.
chunk(false, _Module, _Byte, _PaddedEndOffset, ChunkEntropy, _Opts) ->
    {ok, ChunkEntropy};
chunk(true, Module, Byte, PaddedEndOffset, ChunkEntropy, Opts) ->
    maybe
        {ok, {_EndOffset, Unpacked}} ?=
            lib_arweave_chunks:read(
                Module, Byte, PaddedEndOffset - ?DATA_CHUNK_SIZE, Opts),
        {ok, lib_arweave_packing:encipher(Unpacked, ChunkEntropy)}
    end.

%% @doc Record what a written slot now holds. The entropy record covers the
%% bucket either way; the chunk records cover it only when the slot holds a
%% chunk, because a slot holding entropy alone holds no data to answer with.
placed(ok, false, PaddedEndOffset, Packing, Records, _Opts) ->
    entropy_record(Records, PaddedEndOffset, Packing);
placed(ok, true, PaddedEndOffset, Packing, Records, _Opts) ->
    stored(entropy_record(Records, PaddedEndOffset, Packing),
        PaddedEndOffset, Packing);
placed({error, Error}, _Waiting, PaddedEndOffset, _Packing, Records, Opts) ->
    ?event(warning,
        {arweave_entropy_write_failed,
            {padded_end_offset, PaddedEndOffset},
            {error, Error}
        },
        Opts
    ),
    Records.

%% @doc Record that a bucket's entropy has been written. Entropy intervals are
%% always the whole bucket, never the chunk's own extent.
entropy_record(Records, PaddedEndOffset, Packing) ->
    BucketEnd = ar_chunk_storage:get_chunk_bucket_end(PaddedEndOffset),
    lib_arweave_sync_record:add(
        Records, ?ENTROPY_RECORD, Packing, BucketEnd, BucketEnd - ?DATA_CHUNK_SIZE).

%% @doc Record that a slot now holds a packed chunk: present in the chunk file,
%% and synced at this module's packing.
stored(Records, PaddedEndOffset, Packing) ->
    StartOffset = PaddedEndOffset - ?DATA_CHUNK_SIZE,
    lib_arweave_sync_record:add(
        lib_arweave_sync_record:add(
            lib_arweave_sync_record:delete(
                Records, ar_data_sync, PaddedEndOffset, StartOffset),
            ar_chunk_storage,
            PaddedEndOffset,
            StartOffset
        ),
        ar_data_sync,
        Packing,
        PaddedEndOffset,
        StartOffset
    ).

%% @doc The next bucket a pass considers: the start of the next range with no
%% entropy, or simply the bucket after this one.
next(BucketEndOffset, Records, Packing) ->
    case
        lib_arweave_sync_record:next_unsynced(
            Records, {?ENTROPY_RECORD, Packing}, BucketEndOffset, infinity)
    of
        not_found -> BucketEndOffset + ?DATA_CHUNK_SIZE;
        {_End, Start} -> Start + ?DATA_CHUNK_SIZE
    end.

%% @doc Save what the pass wrote and answer with what it did. The records are
%% written after the chunk files, so a crash between them loses a record for
%% bytes that are on disk -- which the next pass rewrites -- rather than keeping
%% one for bytes that are not.
done(Cursor, Written, Complete, State, Opts) ->
    Module = field(<<"module">>, State),
    Records = field(<<"records">>, State),
    maybe
        ok ?= saved(Module, Records, field(<<"loaded">>, State), Opts),
        ok ?= advance(Module, Cursor, Opts),
        {ok,
            #{
                <<"footprints">> => Written,
                <<"cursor">> => Cursor,
                <<"complete">> => Complete
            }
        }
    end.

%% @doc Write the records the pass leaves, unless it changed none of them.
%%
%% A module whose range is prepared reaches the end of it without touching a
%% record, and a partition's record set runs to thousands of intervals. Writing
%% them back once a second for a module that is done is work with no product.
saved(_Module, Records, Records, _Opts) ->
    ok;
saved(Module, Records, _Loaded, Opts) ->
    lib_arweave_sync_record:save(Module, Records, Opts).

%% @doc Read a stored cursor, falling back to the start of the module for a file
%% that holds anything else.
stored_cursor(Cursor, _Default) when is_integer(Cursor) -> Cursor;
stored_cursor(_Other, Default) -> Default.

%% @doc Read a field of the pass state.
field(Key, State) ->
    maps:get(Key, State).
