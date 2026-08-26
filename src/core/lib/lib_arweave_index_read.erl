%%% @doc Buffered sequential reads of an unpacked storage module, in weave
%%% offsets, with the disk kept one batch ahead of the parse.
%%%
%%% The scanner asks for byte ranges of the weave; this module answers them
%%% from large sequential reads of the module's chunk files. A request inside
%%% the current buffer is a sub-binary and costs nothing; a request beyond it
%%% replaces the buffer with a batch of whole slots -- one `pread' per chunk
%%% file touched -- from the bucket the request begins in.
%%%
%%% The reads run in a fetcher process the reader owns. When a served batch
%%% is consumed sequentially -- the next miss continues exactly where the
%%% batch ended -- the fetcher reads the following batch on its own time, so
%%% the disk works while the scanner parses and a dense module streams at
%%% the slower of the two rates rather than their sum. A miss that jumps --
%%% the scanner skipping an item's data or a transaction -- is served fresh
%%% and disarms the read-ahead, so sparse scanning never pays for bytes it
%%% will not use. Batch binaries pass between the processes by reference.
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
%%% arithmetic did not place. A batch ends at either, so a request reaching
%%% past one comes back `short' rather than as the wrong bytes; the scanner
%%% counts the loss and carries on at the next transaction.
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
    BatchChunks =
        hb_util:ceil_int(
            hb_util:int(
                hb_opts:get(<<"arweave-index-read-size">>, ?BATCH_SIZE, Opts)),
            ?DATA_CHUNK_SIZE
        ) div ?DATA_CHUNK_SIZE,
    IO =
        #{
            <<"module">> => Module,
            <<"opts">> => Opts,
            <<"group-size">> => lib_arweave_storage:chunk_group_size(Opts),
            <<"batch-chunks">> => BatchChunks,
            <<"file">> => undefined,
            <<"file-start">> => -1,
            <<"bytes-read">> => 0,
            <<"reads">> => 0,
            <<"prefetches">> => 0
        },
    #{
        <<"fetcher">> => spawn_link(fun() -> fetcher(IO, none) end),
        <<"batch-chunks">> => BatchChunks,
        <<"buffer">> => <<>>,
        <<"base">> => 0
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
    #{ <<"fetcher">> := Fetcher, <<"batch-chunks">> := BatchChunks } = Reader,
    Residue = residue(Offset),
    Base = Offset - ((Offset - Residue) rem ?DATA_CHUNK_SIZE),
    Chunks =
        max(
            BatchChunks,
            hb_util:ceil_int(
                Offset + Len - Base, ?DATA_CHUNK_SIZE) div ?DATA_CHUNK_SIZE
        ),
    case call(Fetcher, {fetch, Base - Residue, Residue, Chunks}) of
        {ok, Buffer} ->
            Refilled = Reader#{ <<"buffer">> => Buffer, <<"base">> => Base },
            case Offset + Len =< Base + byte_size(Buffer) of
                true ->
                    {ok, binary:part(Buffer, Offset - Base, Len), Refilled};
                false ->
                    {short, Refilled}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Stop the reader's fetcher, closing its open chunk file.
close(#{ <<"fetcher">> := Fetcher }) ->
    call(Fetcher, close).

%% @doc How much the reader moved: bytes read from disk, preads issued, and
%% how many batches the read-ahead supplied.
stats(#{ <<"fetcher">> := Fetcher }) ->
    call(Fetcher, stats).

%%% Internal functions.

%% @doc One synchronous request to the fetcher. Monitored, so a fetcher
%% that died surfaces as this reader's failure rather than as a wait.
call(Fetcher, Request) ->
    Ref = erlang:monitor(process, Fetcher),
    Fetcher ! {Request, self(), Ref},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Fetcher, Reason} ->
            erlang:error({'fetcher-down', Reason})
    end.

%% @doc The fetcher: every `pread' of the reader happens here. After serving
%% a whole batch it reads the next one before waiting again, and a request
%% continuing exactly there is answered without touching the disk. `Ahead'
%% is `none', or `{Params, Result}' for the batch read in advance.
fetcher(IO, Ahead) ->
    receive
        {{fetch, BucketStart, Residue, Chunks}, From, Ref} ->
            Params = {BucketStart, Residue, Chunks},
            {Result, IO2} =
                case Ahead of
                    {Params, Ready} ->
                        {Ready, counted_prefetch(IO)};
                    _MissOrNone ->
                        batch(BucketStart, Residue, Chunks, IO)
                end,
            From ! {Ref, result(Result)},
            fetcher_ahead(IO2, Params, Result);
        {stats, From, Ref} ->
            From !
                {Ref,
                    maps:with(
                        [<<"bytes-read">>, <<"reads">>, <<"prefetches">>],
                        IO
                    )
                },
            fetcher(IO, Ahead);
        {close, From, Ref} ->
            ok = close_file(IO),
            From ! {Ref, ok}
    end.

%% @doc Arm the read-ahead after a served batch. Only an ordinary batch
%% consumed whole predicts its continuation: one that ended early hit a hole
%% the scanner will now step over, and an oversized one -- a bundle table
%% wider than a batch -- is followed by requests inside it, not after it.
fetcher_ahead(IO = #{ <<"batch-chunks">> := Chunks },
        {BucketStart, Residue, Chunks}, {ok, Buffer})
        when byte_size(Buffer) == Chunks * ?DATA_CHUNK_SIZE ->
    NextBucketStart = BucketStart + Chunks * ?DATA_CHUNK_SIZE,
    {Result, IO2} = batch(NextBucketStart, Residue, Chunks, IO),
    fetcher(IO2, {{NextBucketStart, Residue, Chunks}, Result});
fetcher_ahead(IO, _Params, _Result) ->
    fetcher(IO, none).

%% @doc Shape a batch result for the reply.
result({ok, Buffer}) -> {ok, Buffer};
result({error, Reason}) -> {error, Reason}.

%% @doc Account a batch served from the read-ahead.
counted_prefetch(IO = #{ <<"prefetches">> := N }) ->
    IO#{ <<"prefetches">> => N + 1 }.

%% @doc Read one batch of whole slots into a contiguous buffer.
batch(BucketStart, Residue, Chunks, IO) ->
    case gathered(BucketStart, Residue, Chunks, [], IO) of
        {ok, Acc, IO2} ->
            {{ok, iolist_to_binary(lists:reverse(Acc))}, IO2};
        {error, Reason} ->
            {{error, Reason}, IO}
    end.

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
%% batch; the stop itself surfaces on the read that overruns it.
gathered(_BucketStart, _Residue, 0, Acc, IO) ->
    {ok, Acc, IO};
gathered(BucketStart, Residue, Chunks, Acc,
        IO = #{ <<"group-size">> := GroupSize }) ->
    FileStart =
        ar_chunk_storage:get_chunk_file_start_by_start_offset(
            BucketStart, GroupSize),
    InFile =
        min(Chunks, (FileStart + GroupSize - BucketStart) div ?DATA_CHUNK_SIZE),
    case opened(FileStart, IO) of
        % A missing chunk file is a hole spanning the file: the batch ends
        % where the file would have begun.
        missing ->
            {ok, Acc, IO};
        {error, Reason} ->
            {error, Reason};
        {ok, File, IO2} ->
            {Position, _ChunkOffset} =
                ar_chunk_storage:get_position_and_relative_chunk_offset_by_start_offset(
                    FileStart,
                    BucketStart
                ),
            Wanted = InFile * (?DATA_CHUNK_SIZE + ?OFFSET_SIZE),
            Read = file:pread(File, Position, Wanted),
            counted(Read, BucketStart, Residue, Chunks, InFile, Acc, IO2)
    end.

%% @doc Account a `pread''s result and split its slots from the prefixes.
counted(eof, _BucketStart, _Residue, _Chunks, _InFile, Acc, IO) ->
    {ok, Acc, IO};
counted({error, Reason}, _BucketStart, _Residue, _Chunks, _InFile, _Acc, _IO) ->
    {error, Reason};
counted({ok, Bin}, BucketStart, Residue, Chunks, InFile, Acc,
        IO = #{ <<"bytes-read">> := Bytes, <<"reads">> := Reads }) ->
    IO2 =
        IO#{
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
                IO2
            );
        false ->
            {ok, Acc2, IO2}
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

%% @doc The open handle of the chunk file starting at the given offset,
%% opening it -- and closing its predecessor -- when the batch walks into it.
opened(FileStart, IO = #{ <<"file-start">> := FileStart, <<"file">> := File })
        when File /= undefined ->
    {ok, File, IO};
opened(FileStart, IO = #{ <<"module">> := Module, <<"opts">> := Opts }) ->
    ok = close_file(IO),
    Path = lib_arweave_chunks:file_path(Module, FileStart, Opts),
    case file:open(Path, [read, raw, binary]) of
        {ok, File} ->
            {ok, File, IO#{ <<"file">> => File, <<"file-start">> => FileStart }};
        {error, enoent} ->
            missing;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Close the fetcher's open chunk file, if any.
close_file(#{ <<"file">> := undefined }) -> ok;
close_file(#{ <<"file">> := File }) -> file:close(File).
