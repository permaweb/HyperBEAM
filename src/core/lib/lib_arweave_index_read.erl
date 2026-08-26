%%% @doc Buffered sequential reads of an unpacked storage module, in weave
%%% offsets.
%%%
%%% The scanner asks for byte ranges of the weave; this module answers them
%%% from large sequential reads of the module's chunk files. A request inside
%%% the current buffer is a sub-binary and costs nothing; a request beyond it
%%% drops the buffer and reads a new batch of whole slots -- one `pread' per
%%% chunk file touched -- from the bucket the request begins in. Reads are
%%% therefore sequential per consumer whenever the consumer's requests ascend,
%%% which the scanner's do, and a consumer that skips a stretch of the weave
%%% (an item's data, a transaction that is not a bundle) skips the disk reads
%%% with it.
%%%
%%% Above the strict data split threshold every transaction's data begins on
%%% the padded lattice `Threshold + k * 262144' -- anchored at the threshold,
%%% not at zero, so every chunk's start is `Threshold rem 262144' (41,206 on
%%% mainnet) bytes into the absolute bucket its slot is keyed by, and every
%%% live slot's prefix carries that same residue. A contiguous weave range is
%%% therefore still a contiguous run of slots, shifted by the residue; this
%%% module serves that regime, which is everything an unpacked mainnet
%%% partition of the present era holds. Offsets at or below the threshold
%%% are served on the absolute lattice, which is only meaningful for data
%%% whose sync intervals happen to be aligned -- real below-threshold chunks
%%% live in an Arweave node's RocksDB, not in chunk files, and are not this
%%% scanner's input.
%%%
%%% A slot whose three-byte prefix is zero was never written, and one whose
%%% prefix disagrees with the lattice residue describes a chunk this reader's
%%% arithmetic did not place. The buffer ends at either, so a request
%%% reaching past one comes back `short' rather than as the wrong bytes; the
%%% scanner counts the loss and carries on at the next transaction.
-module(lib_arweave_index_read).
-export([open/2, read/3, close/1, stats/1]).
-include("include/hb.hrl").
-include("include/ar_chunk_storage.hrl").

%%% The number of payload bytes one batch read covers by default. Batches are
%%% whole slots, so the bytes moved per `pread' are this plus three per chunk.
-define(BATCH_SIZE, (256 * ?DATA_CHUNK_SIZE)). % 64 MiB.

%% @doc Open a reader over one storage module. The batch size is read from
%% `arweave-index-read-size' (bytes, rounded up to whole chunks).
open(Module, Opts) ->
    #{
        <<"module">> => Module,
        <<"opts">> => Opts,
        <<"group-size">> => lib_arweave_storage:chunk_group_size(Opts),
        <<"batch-chunks">> =>
            hb_util:ceil_int(
                hb_util:int(
                    hb_opts:get(
                        <<"arweave-index-read-size">>, ?BATCH_SIZE, Opts)),
                ?DATA_CHUNK_SIZE
            ) div ?DATA_CHUNK_SIZE,
        <<"file">> => undefined,
        <<"file-start">> => -1,
        <<"buffer">> => <<>>,
        <<"base">> => 0,
        <<"bytes-read">> => 0,
        <<"reads">> => 0
    }.

%% @doc Read `Len' bytes of the weave beginning at `Offset', as
%% `{ok, Binary, Reader}'. Bytes the module does not hold -- an unwritten
%% slot, a chunk file that is not there -- end the answer early: the caller
%% receives `{short, Reader}' and decides what the loss means.
read(Offset, Len, Reader = #{ <<"buffer">> := Buffer, <<"base">> := Base })
        when Offset >= Base
        andalso Offset + Len =< Base + byte_size(Buffer) ->
    {ok, binary:part(Buffer, Offset - Base, Len), Reader};
read(Offset, Len, Reader) ->
    maybe
        {ok, Refilled} ?= refill(Offset, Len, Reader),
        #{ <<"buffer">> := Buffer, <<"base">> := Base } = Refilled,
        case Offset + Len =< Base + byte_size(Buffer) of
            true -> {ok, binary:part(Buffer, Offset - Base, Len), Refilled};
            false -> {short, Refilled}
        end
    end.

%% @doc Close the reader's open chunk file, if any.
close(#{ <<"file">> := undefined }) -> ok;
close(#{ <<"file">> := File }) -> file:close(File).

%% @doc How much the reader moved: bytes read from disk and preads issued.
stats(#{ <<"bytes-read">> := Bytes, <<"reads">> := Reads }) ->
    #{ <<"bytes-read">> => Bytes, <<"reads">> => Reads }.

%%% Internal functions.

%% @doc Replace the buffer with a batch of whole slots covering the request.
%% The batch begins at the lattice point at or before the request and spans
%% the configured batch size or the request's length, whichever is larger,
%% split across chunk files as the layout dictates. The buffer's base is the
%% lattice point itself; the buckets its slots are keyed by sit one residue
%% below it.
refill(Offset, Len, Reader = #{ <<"batch-chunks">> := BatchChunks }) ->
    Residue = residue(Offset),
    Base = Offset - ((Offset - Residue) rem ?DATA_CHUNK_SIZE),
    Chunks =
        max(
            BatchChunks,
            hb_util:ceil_int(
                Offset + Len - Base, ?DATA_CHUNK_SIZE) div ?DATA_CHUNK_SIZE
        ),
    gathered(
        Base - Residue,
        Residue,
        Chunks,
        [],
        Reader#{ <<"buffer">> => <<>>, <<"base">> => Base }
    ).

%% @doc Where the chunk lattice sits within the absolute buckets: at the
%% strict data split threshold's own offset above it, at zero below.
residue(Offset) ->
    Threshold = ar_block:strict_data_split_threshold(),
    case Offset > Threshold of
        true -> Threshold rem ?DATA_CHUNK_SIZE;
        false -> 0
    end.

%% @doc Read slot runs file by file until the batch is assembled, a hole ends
%% it, or a file is missing. Whatever was assembled before the stop is the
%% buffer; the stop itself surfaces on the read that overruns it.
gathered(_BucketStart, _Residue, 0, Acc, Reader) ->
    {ok, assembled(Acc, Reader)};
gathered(BucketStart, Residue, Chunks, Acc,
        Reader = #{ <<"group-size">> := GroupSize }) ->
    FileStart =
        ar_chunk_storage:get_chunk_file_start_by_start_offset(
            BucketStart, GroupSize),
    InFile =
        min(Chunks, (FileStart + GroupSize - BucketStart) div ?DATA_CHUNK_SIZE),
    case opened(FileStart, Reader) of
        % A missing chunk file is a hole spanning the file: the batch ends
        % where the file would have begun.
        missing ->
            {ok, assembled(Acc, Reader)};
        {error, Reason} ->
            {error, Reason};
        {ok, File, Reader2} ->
            {Position, _ChunkOffset} =
                ar_chunk_storage:get_position_and_relative_chunk_offset_by_start_offset(
                    FileStart,
                    BucketStart
                ),
            Wanted = InFile * (?DATA_CHUNK_SIZE + ?OFFSET_SIZE),
            Read = file:pread(File, Position, Wanted),
            counted(Read, BucketStart, Residue, Chunks, InFile, Acc, Reader2)
    end.

%% @doc Account a `pread''s result and split its slots from the prefixes.
counted(eof, _BucketStart, _Residue, _Chunks, _InFile, Acc, Reader) ->
    {ok, assembled(Acc, Reader)};
counted({error, Reason}, _BucketStart, _Residue, _Chunks, _InFile, _Acc, _Reader) ->
    {error, Reason};
counted({ok, Bin}, BucketStart, Residue, Chunks, InFile, Acc,
        Reader = #{ <<"bytes-read">> := Bytes, <<"reads">> := Reads }) ->
    Reader2 =
        Reader#{
            <<"bytes-read">> => Bytes + byte_size(Bin),
            <<"reads">> => Reads + 1
        },
    {Slots, Whole} = slots(Bin, Residue, []),
    Acc2 = lists:reverse(Slots, Acc),
    Got = length(Slots),
    case Whole andalso Got == InFile of
        true ->
            gathered(
                BucketStart + Got * ?DATA_CHUNK_SIZE,
                Residue,
                Chunks - Got,
                Acc2,
                Reader2
            );
        false ->
            {ok, assembled(Acc2, Reader2)}
    end.

%% @doc The chunks of a run of slots, in order, stopping at the first slot
%% whose prefix marks it unwritten or off the lattice, or whose bytes are not
%% all there. Returns whether the run was consumed whole. A prefix of 262144
%% is how a chunk beginning at its bucket's own start is spelled, which the
%% `rem' folds back onto a residue of zero.
slots(<< 0:?OFFSET_BIT_SIZE, _:?DATA_CHUNK_SIZE/binary, _/binary >>,
        _Residue, Acc) ->
    {lists:reverse(Acc), false};
slots(<< Prefix:?OFFSET_BIT_SIZE, Chunk:?DATA_CHUNK_SIZE/binary, Rest/binary >>,
        Residue, Acc) when Prefix rem ?DATA_CHUNK_SIZE == Residue ->
    slots(Rest, Residue, [Chunk | Acc]);
slots(<< _OffLattice:?OFFSET_BIT_SIZE, _:?DATA_CHUNK_SIZE/binary, _/binary >>,
        _Residue, Acc) ->
    {lists:reverse(Acc), false};
slots(<<>>, _Residue, Acc) ->
    {lists:reverse(Acc), true};
slots(_Partial, _Residue, Acc) ->
    {lists:reverse(Acc), false}.

%% @doc One contiguous buffer from the collected chunks, newest last.
assembled(Acc, Reader) ->
    Reader#{ <<"buffer">> => iolist_to_binary(lists:reverse(Acc)) }.

%% @doc The open handle of the chunk file starting at the given offset,
%% opening it -- and closing its predecessor -- when the batch walks into it.
opened(FileStart, Reader = #{ <<"file-start">> := FileStart,
        <<"file">> := File }) when File /= undefined ->
    {ok, File, Reader};
opened(FileStart, Reader = #{ <<"module">> := Module, <<"opts">> := Opts }) ->
    ok = close(Reader),
    Path = lib_arweave_chunks:file_path(Module, FileStart, Opts),
    case file:open(Path, [read, raw, binary]) of
        {ok, File} ->
            {ok, File,
                Reader#{ <<"file">> => File, <<"file-start">> => FileStart }};
        {error, enoent} ->
            missing;
        {error, Reason} ->
            {error, Reason}
    end.
