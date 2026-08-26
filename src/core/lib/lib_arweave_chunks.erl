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
%%% Files are opened for the operation and closed after it. Files are sparse,
%%% never preallocated and never truncated, so a read that runs off the end of
%%% one is an ordinary short read rather than a failure. Bulk sequential
%%% consumption of a module's files belongs to `lib_arweave_index_scan', which
%%% streams whole files; this module answers for single slots and the layout.
-module(lib_arweave_chunks).
-export([write/4, read/3, read/4, read_offset/3]).
-export([files/2, file_path/3, locate/3, ensure_dir/2]).
-include("include/hb.hrl").
-include("include/ar_chunk_storage.hrl").

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
%% names the bucket exactly. Below the threshold a chunk may be any size and no
%% offset says where one begins, so a caller reading that part of the weave has
%% to bring the synced interval and use `read/4'.
read(Module, Byte, Opts) ->
    slot(Module, Byte, ar_chunk_storage:get_chunk_bucket_start(Byte + 1), Opts).

%% @doc Read the chunk containing the given byte, given the start of the synced
%% interval the byte falls in.
%%
%% The interval's start is the lattice the chunks sit on -- they follow it in
%% 256 KiB steps -- and stepping to the one holding the byte is what turns a
%% byte into a bucket where the offset alone cannot.
read(Module, Byte, IntervalStart, Opts) ->
    slot(
        Module,
        Byte,
        Byte - (Byte - IntervalStart) rem ?DATA_CHUNK_SIZE,
        Opts
    ).

%% @doc Read the three-byte prefix of the slot a padded end offset owns,
%% without the chunk beside it.
%%
%% The prefix is where in its bucket the slot's chunk begins, `262144' when it
%% begins at the bucket's own start, and zero when the slot holds nothing.
read_offset(Module, PaddedEndOffset, Opts) ->
    {_ChunkFileStart, Filepath, Position, _ChunkOffset} =
        locate(Module, PaddedEndOffset, Opts),
    prefix(file:open(Filepath, [read, raw, binary]), Position).

%% @doc Every chunk file a module holds, as paths in ascending offset order.
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
        Name <- lists:sort(fun by_offset/2, listed(Dir)),
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
                file_path(Module, ChunkFileStart, Opts)
            ),
        found(Chunks)
    end.

%% @doc Read one slot of one chunk file as a single `pread'.
slots(Byte, Start, ChunkFileStart, Filepath) ->
    read_slots(
        file:open(Filepath, [read, raw, binary]),
        Byte,
        Start,
        ChunkFileStart
    ).

%% @doc Read the slot through an opened file and close it.
read_slots({error, enoent}, _Byte, _Start, _ChunkFileStart) ->
    % A chunk file a module never wrote is a stretch of the weave it holds
    % nothing in, which is a hole rather than a failure.
    {ok, []};
read_slots({error, Reason}, _Byte, _Start, _ChunkFileStart) ->
    failed(<<"chunk-file-unreadable">>, Reason);
read_slots({ok, File}, Byte, Start, ChunkFileStart) ->
    {Position, _ChunkOffset} =
        ar_chunk_storage:get_position_and_relative_chunk_offset_by_start_offset(
            ChunkFileStart,
            Start
        ),
    Read = file:pread(File, Position, ?DATA_CHUNK_SIZE + ?OFFSET_SIZE),
    file:close(File),
    decoded(Read, Byte, hb_util:floor_int(Start, ?DATA_CHUNK_SIZE)).

%% @doc Decode a read of one slot into a `{PaddedEndOffset, Chunk}' pair. The
%% slot's prefix has to place the byte the read was made for inside its chunk;
%% a slot that fails that describes something other than what was asked for,
%% and the whole read goes with it.
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

%% @doc Order two chunk file names by the offsets they cover. Names that are
%% not offsets sort among themselves, and are dropped by the caller.
by_offset(A, B) ->
    case {string:to_integer(A), string:to_integer(B)} of
        {{OffsetA, []}, {OffsetB, []}} -> OffsetA =< OffsetB;
        _ -> A =< B
    end.

%% @doc Give a file system result this subsystem's error shape.
checked(ok, _Message) ->
    ok;
checked({error, Reason}, Message) ->
    failed(Message, Reason).

%% @doc The error a failed file operation is reported as.
failed(Message, Reason) ->
    {error,
        #{
            <<"status">> => 500,
            <<"message">> => Message,
            <<"detail">> => hb_util:bin(io_lib:format("~p", [Reason]))
        }
    }.
