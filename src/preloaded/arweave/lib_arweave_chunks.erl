%%% @doc The chunk files a storage module holds the weave in.
%%%
%%% A chunk file is a flat array of fixed slots, one per 256 KiB bucket of the
%%% weave, and nothing else: no header, no index, no free list. The slot a
%%% bucket owns is `<< ChunkOffset:24, Chunk:262144/binary >>' at
%%% `RelativeOffset + 3 * (RelativeOffset div 262144)' bytes into the file
%%% named for the offset its first bucket begins at. That is the layout an
%%% Arweave node wrote, so it is the layout this node reads and writes; the
%%% arithmetic behind it is the vendored `ar_chunk_storage', called here rather
%%% than restated.
%%%
%%% The three-byte prefix says where inside its bucket the chunk begins, and a
%%% prefix of zero says the slot holds nothing at all. Zero is therefore not
%%% available to mean `at the bucket's own start', which is why that one case
%%% is written as 262144 instead.
%%%
%%% A slot holding bytes is not the same thing as a slot holding a chunk. A
%%% module packed for `replica_2_9' has entropy written into the buckets it has
%%% no data for yet, and entropy carries a prefix like anything else, so the
%%% file cannot tell you which of the two you are looking at. The sync record
%%% can. `read_range/5' therefore takes the record and drops what is not in it,
%%% while a read of a single slot returns whatever occupies it: the caller of a
%%% single read has already decided which slot it wants, and the caller that
%%% wants the entropy back out of a bucket is one of them.
%%%
%%% Files are opened for the operation and closed after it. There is no handle
%%% cache and no file index: every mutation of a module already runs inside
%%% `lib_arweave_storage:exclusive/3', and a cached index of the chunk
%%% directory would be a second answer to a question the directory itself
%%% answers. Files are sparse, never preallocated and never truncated, so a
%%% read that runs off the end of one is an ordinary short read rather than a
%%% failure.
-module(lib_arweave_chunks).
-export([write/4, read/3, read/4, read_range/5, read_offset/3, delete/3]).
-export([files/2, file_path/3, locate/3, ensure_dir/2]).
-include("include/hb.hrl").
-include("include/ar.hrl").
-include("include/ar_chunk_storage.hrl").

%%% The sync record naming the offsets at which a module's chunk files hold
%%% data rather than entropy. It is the value `sync_record_id/1' of the
%%% vendored `ar_chunk_storage' gives for every packing a module can hold.
-define(RECORD, ar_chunk_storage).

%% @doc Write one chunk into the slot its padded end offset owns.
%%
%% A slot holds exactly one chunk's worth of bytes and the module keeps no
%% sizes, so a shorter chunk is one this layout cannot represent rather than
%% one it pads. Creates the module's chunk directory and the file if they are
%% not there; a second write to the same bucket replaces the first.
write(Module, PaddedEndOffset, Chunk, Opts)
        when byte_size(Chunk) == ?DATA_CHUNK_SIZE ->
    maybe
        ok ?= ensure_dir(Module, Opts),
        {_ChunkFileStart, Filepath, Position, ChunkOffset} =
            locate(Module, PaddedEndOffset, Opts),
        written(
            file:open(Filepath, [read, write, raw, binary]),
            Position,
            [<< ChunkOffset:?OFFSET_BIT_SIZE >> | Chunk]
        )
    end;
write(_Module, _PaddedEndOffset, _Chunk, _Opts) ->
    {error,
        #{
            <<"status">> => 400,
            <<"message">> => <<"chunk-size-invalid">>,
            <<"detail">> => <<"a chunk file slot holds exactly 262144 bytes">>
        }
    }.

%% @doc Read the chunk occupying the bucket the given byte belongs to.
%%
%% The bucket is derived from the byte alone, which every offset above the
%% strict data split threshold allows: there the padded end offset of the chunk
%% covering a byte is the only end offset its bucket can carry, so the byte
%% names the bucket exactly. That is the whole of a `replica_2_9' module and
%% everything a miner reads. Below the threshold a chunk may be any size and no
%% offset says where one begins, so a caller reading that part of the weave has
%% to bring the synced interval and use `read/4'.
%%
%% The result is whatever occupies the slot, unfiltered by the sync record: the
%% caller of a single read has already chosen the slot, and the caller reading
%% a `replica_2_9' bucket back before enciphering it wants precisely the
%% entropy the record excludes.
read(Module, Byte, Opts) ->
    slot(Module, Byte, ar_chunk_storage:get_chunk_bucket_start(Byte + 1), Opts).

%% @doc Read the chunk containing the given byte, given the start of the synced
%% interval the byte falls in.
%%
%% The interval's start is the lattice the chunks sit on -- they follow it in
%% 256 KiB steps -- and stepping to the one holding the byte is what turns a
%% byte into a bucket where the offset alone cannot. As with `read/3', the slot
%% is returned as it is found, without consulting the record it came from.
read(Module, Byte, IntervalStart, Opts) ->
    slot(
        Module,
        Byte,
        Byte - (Byte - IntervalStart) rem ?DATA_CHUNK_SIZE,
        Opts
    ).

%% @doc Read every chunk a module holds inside a range of the weave, as
%% `{PaddedEndOffset, Chunk}' pairs in ascending order.
%%
%% This is the read a miner makes: one recall range, one pass, one `pread' per
%% chunk file the range touches. The range need not cover the chunks it
%% intersects completely, and the last pair may extend past its end -- up to
%% `Start + Size + 262144 - 1' -- because a bucket the range reaches into is a
%% bucket whose whole chunk is read.
%%
%% The sync record decides two things. Where the chunks begin: they are a
%% lattice of 256 KiB steps from the start of the synced interval, which is the
%% only thing that places chunks below the strict data split threshold. And
%% which slots count: the holes between a module's chunks may be filled with
%% entropy, so a slot the record does not cover is dropped, exactly as upstream
%% does and for the same reason. A range with no synced interval, a file that
%% is not there and a hole in the middle of one all yield fewer pairs, never an
%% error.
read_range(Module, Start, Size, Records, Opts) ->
    GroupSize = lib_arweave_storage:chunk_group_size(Opts),
    maybe
        true ?= Size < GroupSize orelse oversized(Size, GroupSize),
        Synced =
            lib_arweave_sync_record:next_synced(
                Records, ?RECORD, Start, infinity),
        case Synced of
            {_End, IntervalStart} when Start + Size > IntervalStart ->
                spanned(Module, Start, Size, IntervalStart, Records, Opts);
            _ ->
                {ok, []}
        end
    end.

%% @doc Read the three-byte prefix of the slot a padded end offset owns,
%% without the chunk beside it.
%%
%% The prefix is where in its bucket the slot's chunk begins, `262144' when it
%% begins at the bucket's own start, and zero when the slot holds nothing.
read_offset(Module, PaddedEndOffset, Opts) ->
    {_ChunkFileStart, Filepath, Position, _ChunkOffset} =
        locate(Module, PaddedEndOffset, Opts),
    prefix(file:open(Filepath, [read, raw, binary]), Position).

%% @doc Empty the slot a padded end offset owns, by zeroing it.
%%
%% The slot keeps its place in the file -- the layout has no way to remove one
%% -- and a prefix of zero is what says it holds nothing. Deleting from a file
%% that was never written is already true.
delete(Module, PaddedEndOffset, Opts) ->
    {_ChunkFileStart, Filepath, Position, _ChunkOffset} =
        locate(Module, PaddedEndOffset, Opts),
    deleted(file:open(Filepath, [read, write, raw, binary]), Position).

%% @doc Every chunk file a module holds, as paths.
%%
%% A chunk file is named by the decimal offset of the first bucket it covers,
%% so every other name in the directory is something else: the cursors an
%% Arweave node leaves beside its chunk files while it prepares or repacks a
%% module, and the temporary copies a defragmentation makes.
files(Module, Opts) ->
    Dir = lib_arweave_storage:chunk_dir(Module, Opts),
    [
        filename:join(Dir, hb_util:bin(Name))
    ||
        Name <- listed(Dir),
        numeric(Name)
    ].

%% @doc The path of the chunk file covering the buckets from the given offset.
file_path(Module, ChunkFileStart, Opts) ->
    ar_chunk_storage:get_filepath(
        lib_arweave_storage:data_dir(Opts),
        hb_util:bin(ChunkFileStart),
        lib_arweave_storage:id(Module)
    ).

%% @doc Where the chunk with the given padded end offset lives: the offset its
%% chunk file starts at, that file's path, the byte position of its slot, and
%% the prefix the slot carries.
locate(Module, PaddedEndOffset, Opts) ->
    ChunkFileStart =
        ar_chunk_storage:get_chunk_file_start(
            PaddedEndOffset,
            lib_arweave_storage:chunk_group_size(Opts)
        ),
    {Position, ChunkOffset} =
        ar_chunk_storage:get_position_and_relative_chunk_offset(
            ChunkFileStart,
            PaddedEndOffset
        ),
    {
        ChunkFileStart,
        file_path(Module, ChunkFileStart, Opts),
        Position,
        ChunkOffset
    }.

%% @doc Create the directory a module's chunk files live in.
ensure_dir(Module, Opts) ->
    checked(
        filelib:ensure_path(lib_arweave_storage:chunk_dir(Module, Opts)),
        <<"chunk-dir-unavailable">>
    ).

%%% Internal functions.

%% @doc Read the one slot whose chunk begins at the given offset. The chunk's
%% prefix has to place the byte the read was made for inside it; a slot that
%% does not is one the caller's arithmetic did not describe, and it reads as
%% absent rather than as the wrong chunk.
slot(Module, Byte, ChunkStart, Opts) ->
    ChunkFileStart =
        ar_chunk_storage:get_chunk_file_start_by_start_offset(
            ChunkStart,
            lib_arweave_storage:chunk_group_size(Opts)
        ),
    maybe
        {ok, Chunks} ?=
            slots(
                Byte,
                ChunkStart,
                ChunkFileStart,
                file_path(Module, ChunkFileStart, Opts),
                1
            ),
        found(Chunks)
    end.

%% @doc Read the chunks of a range that a synced interval intersects.
%%
%% The interval's start is the lattice: the first chunk begins at the step at
%% or below the range's start, the last at the step at or below its final byte,
%% and every step between them is a bucket to read. A range that reaches into
%% the next chunk file is read as far as this file goes and then continued as a
%% range of its own, which resolves the record again from where it resumes.
spanned(Module, Start, Size, IntervalStart, Records, Opts) ->
    GroupSize = lib_arweave_storage:chunk_group_size(Opts),
    Start2 = max(Start, IntervalStart),
    Size2 = Start + Size - Start2,
    ChunkStart = Start2 - (Start2 - IntervalStart) rem ?DATA_CHUNK_SIZE,
    ChunkFileStart =
        ar_chunk_storage:get_chunk_file_start_by_start_offset(
            ChunkStart, GroupSize),
    End = Start2 + Size2,
    LastChunkStart =
        (End - 1) - ((End - 1) - IntervalStart) rem ?DATA_CHUNK_SIZE,
    LastChunkFileStart =
        ar_chunk_storage:get_chunk_file_start_by_start_offset(
            LastChunkStart, GroupSize),
    ChunkCount = (LastChunkStart - ChunkStart) div ?DATA_CHUNK_SIZE + 1,
    case ChunkFileStart == LastChunkFileStart of
        true ->
            chunks(
                Module, Start2, ChunkStart, ChunkFileStart, ChunkCount,
                Records, Opts
            );
        false ->
            % The range crosses a chunk file boundary. The count before it
            % floors, so an unaligned first chunk leaves the boundary's own
            % bucket to the range that continues after it.
            SizeBefore = ChunkFileStart + GroupSize - ChunkStart,
            CountBefore =
                max(SizeBefore, ?DATA_CHUNK_SIZE) div ?DATA_CHUNK_SIZE,
            StartAfter = ChunkStart + CountBefore * ?DATA_CHUNK_SIZE,
            SizeAfter =
                Size2 - CountBefore * ?DATA_CHUNK_SIZE + (Start2 - ChunkStart),
            maybe
                {ok, Before} ?=
                    chunks(
                        Module, Start2, ChunkStart, ChunkFileStart, CountBefore,
                        Records, Opts
                    ),
                {ok, After} ?=
                    read_range(Module, StartAfter, SizeAfter, Records, Opts),
                {ok, Before ++ After}
            end
    end.

%% @doc Read consecutive slots of one chunk file and keep the ones the sync
%% record holds.
chunks(Module, Byte, Start, ChunkFileStart, ChunkCount, Records, Opts) ->
    maybe
        {ok, Read} ?=
            slots(
                Byte,
                Start,
                ChunkFileStart,
                file_path(Module, ChunkFileStart, Opts),
                ChunkCount
            ),
        {ok, synced(Read, Records, ChunkCount)}
    end.

%% @doc Read `ChunkCount' consecutive slots of one chunk file as a single
%% `pread', starting at the slot the given offset's bucket owns.
slots(Byte, Start, ChunkFileStart, Filepath, ChunkCount) ->
    read_slots(
        file:open(Filepath, [read, raw, binary]),
        Byte,
        Start,
        ChunkFileStart,
        ChunkCount
    ).

%% @doc Read the slots through an opened file and close it.
read_slots({error, enoent}, _Byte, _Start, _ChunkFileStart, _ChunkCount) ->
    % A chunk file a module never wrote is a stretch of the weave it holds
    % nothing in, which is a hole rather than a failure.
    {ok, []};
read_slots({error, Reason}, _Byte, _Start, _ChunkFileStart, _ChunkCount) ->
    failed(<<"chunk-file-unreadable">>, Reason);
read_slots({ok, File}, Byte, Start, ChunkFileStart, ChunkCount) ->
    {Position, _ChunkOffset} =
        ar_chunk_storage:get_position_and_relative_chunk_offset_by_start_offset(
            ChunkFileStart,
            Start
        ),
    Read =
        file:pread(
            File,
            Position,
            (?DATA_CHUNK_SIZE + ?OFFSET_SIZE) * ChunkCount
        ),
    file:close(File),
    decoded(Read, Byte, hb_util:floor_int(Start, ?DATA_CHUNK_SIZE)).

%% @doc Decode a read of consecutive slots into `{PaddedEndOffset, Chunk}'
%% pairs. The first slot's prefix has to place the byte the read was made for
%% inside its chunk; a slot that fails that describes something other than what
%% was asked for, and the whole read goes with it.
decoded({ok, << ChunkOffset:?OFFSET_BIT_SIZE, _Rest/binary >> = Bin},
        Byte, BucketStart) ->
    case ar_chunk_storage:is_offset_valid(Byte, BucketStart, ChunkOffset) of
        true ->
            {ok,
                ar_chunk_storage:extract_end_offset_chunk_pairs(
                    Bin, BucketStart, 1)
            };
        false ->
            {ok, []}
    end;
decoded({ok, _Partial}, _Byte, _BucketStart) ->
    {ok, []};
decoded(eof, _Byte, _BucketStart) ->
    {ok, []};
decoded({error, Reason}, _Byte, _BucketStart) ->
    failed(<<"chunk-read-failed">>, Reason).

%% @doc Drop the pairs the sync record does not hold, which are the buckets
%% that hold entropy or nothing. A read of a single slot is returned as it is:
%% its caller chose the slot, so the record has already had its say, or was
%% never the question.
synced(Chunks, _Records, 1) ->
    Chunks;
synced(Chunks, Records, _ChunkCount) ->
    [
        {PaddedEndOffset, Chunk}
    ||
        {PaddedEndOffset, Chunk} <- Chunks,
        lib_arweave_sync_record:is_recorded(Records, ?RECORD, PaddedEndOffset)
    ].

%% @doc The result of a read of one slot.
found([Pair]) -> {ok, Pair};
found([]) -> not_found.

%% @doc Read one slot's prefix from an opened file and close it.
prefix({error, enoent}, _Position) ->
    not_found;
prefix({error, Reason}, _Position) ->
    failed(<<"chunk-file-unreadable">>, Reason);
prefix({ok, File}, Position) ->
    Read = file:pread(File, Position, ?OFFSET_SIZE),
    file:close(File),
    read_prefix(Read).

%% @doc The prefix a read of three bytes found, if it found three bytes.
read_prefix({ok, << ChunkOffset:?OFFSET_BIT_SIZE >>}) ->
    {ok, ChunkOffset};
read_prefix({ok, _Partial}) ->
    not_found;
read_prefix(eof) ->
    not_found;
read_prefix({error, Reason}) ->
    failed(<<"chunk-read-failed">>, Reason).

%% @doc Write one slot through an opened file and close it.
written({error, Reason}, _Position, _Data) ->
    failed(<<"chunk-file-unwritable">>, Reason);
written({ok, File}, Position, Data) ->
    Result = file:pwrite(File, Position, Data),
    file:close(File),
    checked(Result, <<"chunk-write-failed">>).

%% @doc Zero one slot through an opened file and close it.
deleted({error, enoent}, _Position) ->
    ok;
deleted({error, Reason}, _Position) ->
    failed(<<"chunk-file-unwritable">>, Reason);
deleted({ok, File}, Position) ->
    Result =
        file:pwrite(
            File,
            Position,
            << 0:((?OFFSET_SIZE + ?DATA_CHUNK_SIZE) * 8) >>
        ),
    file:close(File),
    checked(Result, <<"chunk-delete-failed">>).

%% @doc The names a module's chunk directory holds. A module whose directory
%% has not been created holds no files.
listed(Dir) ->
    case file:list_dir(Dir) of
        {ok, Names} -> Names;
        {error, _Reason} -> []
    end.

%% @doc Whether a name in a chunk directory is a chunk file's, which is to say
%% a decimal integer and nothing else.
numeric(Name) ->
    case string:to_integer(Name) of
        {ChunkFileStart, []} when is_integer(ChunkFileStart) -> true;
        _ -> false
    end.

%% @doc Give a file system result this subsystem's error shape.
checked(ok, _Message) ->
    ok;
checked({error, Reason}, Message) ->
    failed(Message, Reason).

%% @doc The error a range wider than a whole chunk file is refused with. No
%% recall range is anywhere near this size, and one that was could not be
%% served by the two files a boundary splits a range across.
oversized(Size, GroupSize) ->
    {error,
        #{
            <<"status">> => 400,
            <<"message">> => <<"range-too-large">>,
            <<"detail">> =>
                hb_util:bin(
                    io_lib:format(
                        "~B bytes asked of chunk files holding ~B",
                        [Size, GroupSize]
                    )
                )
        }
    }.

%% @doc The error a failed file operation is reported as.
failed(Message, Reason) ->
    {error,
        #{
            <<"status">> => 500,
            <<"message">> => Message,
            <<"detail">> => hb_util:bin(io_lib:format("~p", [Reason]))
        }
    }.
