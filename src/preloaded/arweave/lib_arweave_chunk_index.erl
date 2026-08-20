%%% @doc The index that places a stored chunk in the weave.
%%%
%%% A chunk file holds packed bytes and nothing else. What says which
%%% transaction those bytes came from, where in the weave they sit and which
%%% Merkle paths prove it is this index. A mining solution carries exactly
%%% those paths beside the chunk, so a miner that cannot answer from here has
%%% no solution to submit at all.
%%%
%%% An Arweave node keeps the same facts in two RocksDB column families.
%%% `chunks_index' is keyed by absolute end offset and its value points at a
%%% `chunk_data_db' row holding the data path and -- for a chunk
%%% `ar_chunk_storage:is_storage_supported/3' refuses -- the chunk bytes. That
%%% pointer is a 64-byte key carrying a wall-clock timestamp, so it is not
%%% derivable and nothing may depend on reproducing it. This index drops it:
%%% the data path is stored inline with the rest of the metadata, and a chunk
%%% that has no chunk file is stored under its own absolute end offset.
%%%
%%% == The keys ==
%%%
%%% ```
%%% ~arweave@2.9/storage/<StoreID>/chunks/<BucketEnd>/<AbsoluteEndOffset>
%%% ~arweave@2.9/storage/<StoreID>/chunk-data/<AbsoluteEndOffset>
%%% '''
%%%
%%% Every offset component is a zero-padded decimal of exactly 20 digits, which
%%% spans every offset representable in 64 bits. Fixed width buys two things:
%%% the lexicographic order of the keys is the numeric order of the offsets,
%%% and no key is a byte prefix of another. `hb_store_lmdb' reads a key by
%%% scanning its raw byte prefix, so a variable-width name would make a read of
%%% offset 1 scan and copy offsets 10, 100 and 1000 alongside it.
%%%
%%% `BucketEnd' is `ar_chunk_storage:get_chunk_bucket_end/1' of the chunk's
%%% absolute end offset -- the 256 KiB bucket the chunk file itself places the
%%% chunk in, so the index and the chunk files agree by construction.
%%%
%%% == The value ==
%%%
%%% ```
%%% << ?FORMAT_VERSION:8, ChunkSize:32, RelativeOffset:64, TXRoot:32/binary,
%%%    DataRoot:32/binary, TXPathSize:32, TXPath/binary, DataPathSize:32,
%%%    DataPath/binary >>
%%% '''
%%%
%%% 85 bytes of frame plus the two paths. The absolute end offset the entry is
%%% keyed on is not repeated in the value. `decode/1' refuses a value it does
%%% not recognise rather than guessing at one, so a truncated write, a value
%%% written by another format version, and a value whose declared path sizes do
%%% not account for exactly its remaining bytes are all errors.
%%%
%%% == Finding the chunk that holds a byte ==
%%%
%%% Above the strict data split threshold a chunk's absolute end offset is not
%%% derivable from a byte inside it, because the padded offset the chunk file
%%% is keyed on differs from the transaction's own end offset. So the index
%%% answers by range: `get_by_byte/3' takes the seek offset that
%%% `ar_chunk_storage:get_chunk_seek_offset/1' derives from the byte's own
%%% one-based weave offset, exactly as `ar_data_sync:get_chunk/2' is called
%%% with `RecallByte + 1', and returns the entry whose
%%% `(AbsoluteEndOffset - ChunkSize, AbsoluteEndOffset]' holds it.
%%%
%%% An Arweave node finds that entry with an ordered seek over the whole
%%% keyspace -- the first key at or above the seek offset, refused when the
%%% chunk it names starts after the seek offset. `hb_store' offers point reads
%%% and group listings rather than a seek, so the bucketing bounds the search
%%% instead. A chunk is at most 256 KiB, so a chunk holding the seek offset
%%% ends less than 256 KiB above it, and `get_chunk_bucket_end/1' rises in
%%% 256 KiB steps: the chunk's bucket is therefore either the seek offset's own
%%% bucket or the one 256 KiB above it. Reading those two groups in that order
%%% and taking the first entry at or above the seek offset finds precisely what
%%% the ordered seek would have found. Above the threshold the first bucket
%%% always holds it, because the padding puts a chunk and every byte of its
%%% bucket in the same bucket. Below it, where a chunk of up to 256 KiB
%%% straddles the 256 KiB grid, the second bucket is what makes the answer
%%% right.
%%%
%%% == Deletion ==
%%%
%%% `hb_store' writes and reads; it does not delete. A deleted key therefore
%%% holds a marker that no encoding produces, and reads as absent. The marker
%%% is what a reorganisation leaves behind at an offset the weave no longer has
%%% a chunk for, which is the same claim the absent key makes.
-module(lib_arweave_chunk_index).
-export([put/3, get/3, get_by_byte/3, delete/3, list_bucket/3]).
-export([put_chunk/4, get_chunk/3, delete_chunk/3]).
-export([encode/1, decode/1, key/2, bucket_key/2]).
-include("include/hb.hrl").
-include("include/ar.hrl").

%%% The paths one module's chunk metadata and loose chunks occupy.
-define(STORAGE, <<"~arweave@2.9/storage">>).
-define(CHUNKS, <<"chunks">>).
-define(CHUNK_DATA, <<"chunk-data">>).

%%% The digits an offset is named by. 20 spans every offset representable in
%%% 64 bits, so every key of this index is the same width.
-define(OFFSET_DIGITS, 20).

%%% The version byte every stored value opens with. It is also what keeps a
%%% value out of `hb_store_lmdb''s own marker space: a stored value that began
%%% `link:' or was exactly `group' would be read back as a link or a group.
-define(FORMAT_VERSION, 1).

%%% The value a deleted key holds. No encoding of this module produces it,
%%% because every encoding opens with the format version.
-define(DELETED, <<0>>).

%%% Whether a value is a non-negative integer the encoding holds in the given
%%% number of bits.
-define(FITS(X, Bits), (is_integer(X) andalso X >= 0 andalso X < (1 bsl Bits))).

%% @doc Record where one chunk sits in the weave.
put(Module, Metadata, Opts) ->
    maybe
        {ok, Offset} ?= offset(Metadata),
        {ok, Value} ?= encode(Metadata),
        write(key(Module, Offset), Value, Opts)
    end.

%% @doc Read the metadata of the chunk ending at an offset.
get(Module, AbsoluteEndOffset, Opts) ->
    maybe
        {ok, Value} ?= read(key(Module, AbsoluteEndOffset), Opts),
        entry(hb_util:int(AbsoluteEndOffset), Value, Opts)
    end.

%% @doc Read the metadata of the chunk holding a byte of the weave.
%%
%% `Byte' is counted from zero, as a recall byte is and as
%% `lib_arweave_chunks:read/3' counts one. Arweave's own offsets are one-based
%% inclusive ends, so the chunk holding the byte is the chunk holding the
%% offset one above it -- at a bucket boundary the raw byte belongs to the
%% chunk below, which is the answer a peer gives for it too.
%%
%% A byte in the zero-padding tail of a short chunk above the strict data split
%% threshold resolves to that chunk rather than to nothing, because the seek
%% offset the whole bucket shares is inside the chunk's own bytes.
get_by_byte(Module, Byte, Opts) ->
    Seek = ar_chunk_storage:get_chunk_seek_offset(hb_util:int(Byte) + 1),
    maybe
        {ok, Entries} ?=
            candidates(
                Module,
                ar_chunk_storage:get_chunk_bucket_end(Seek),
                Seek,
                Opts
            ),
        covering(Entries, Seek)
    end.

%% @doc Forget the chunk ending at an offset.
delete(Module, AbsoluteEndOffset, Opts) ->
    write(key(Module, AbsoluteEndOffset), ?DELETED, Opts).

%% @doc Every chunk indexed in one 256 KiB bucket, by ascending absolute end
%% offset. A bucket holds one chunk above the strict data split threshold and
%% may hold several below it.
list_bucket(Module, BucketEnd, Opts) ->
    collect(
        [
            entry(Offset, Value, Opts)
        ||
            {Offset, Value} <-
                children(bucket_key(Module, BucketEnd), Opts)
        ],
        []
    ).

%% @doc Store the bytes of a chunk that has no chunk file. These are the chunks
%% `ar_chunk_storage:is_storage_supported/3' refuses: an unpacked chunk shorter
%% than 256 KiB, which no miner reads and which the chunk file layout -- one
%% 256 KiB slot per bucket, with no length beside it -- cannot represent.
put_chunk(Module, AbsoluteEndOffset, Chunk, Opts) ->
    write(
        chunk_key(Module, AbsoluteEndOffset),
        << ?FORMAT_VERSION:8, Chunk/binary >>,
        Opts
    ).

%% @doc Read the bytes of a chunk that has no chunk file.
get_chunk(Module, AbsoluteEndOffset, Opts) ->
    maybe
        {ok, Value} ?= read(chunk_key(Module, AbsoluteEndOffset), Opts),
        chunk(Value)
    end.

%% @doc Forget the bytes of a chunk that has no chunk file.
delete_chunk(Module, AbsoluteEndOffset, Opts) ->
    write(chunk_key(Module, AbsoluteEndOffset), ?DELETED, Opts).

%% @doc Encode a metadata message into the binary one entry holds. See the
%% module documentation for the layout.
encode(Metadata) ->
    maybe
        {ok, TXRoot} ?= binary_field(<<"tx-root">>, Metadata),
        {ok, DataRoot} ?= binary_field(<<"data-root">>, Metadata),
        {ok, TXPath} ?= binary_field(<<"tx-path">>, Metadata),
        {ok, DataPath} ?= binary_field(<<"data-path">>, Metadata),
        encoded(
            field(<<"chunk-size">>, Metadata),
            field(<<"relative-offset">>, Metadata),
            TXRoot,
            DataRoot,
            TXPath,
            DataPath
        )
    end.

%% @doc Read the binary one entry holds. The absolute end offset is the key
%% rather than part of the value, so it is not among the fields returned; the
%% readers of this index add it from the key they read.
decode(<< ?FORMAT_VERSION:8, ChunkSize:32, RelativeOffset:64,
        TXRoot:32/binary, DataRoot:32/binary, TXPathSize:32,
        TXPath:TXPathSize/binary, DataPathSize:32,
        DataPath:DataPathSize/binary >>) ->
    {ok,
        #{
            <<"chunk-size">> => ChunkSize,
            <<"relative-offset">> => RelativeOffset,
            <<"tx-root">> => hb_util:encode(TXRoot),
            <<"data-root">> => hb_util:encode(DataRoot),
            <<"tx-path">> => hb_util:encode(TXPath),
            <<"data-path">> => hb_util:encode(DataPath)
        }
    };
decode(_Value) ->
    {error,
        error_message(
            <<"invalid-chunk-metadata">>,
            <<"A stored chunk index entry is not in the stored format.">>
        )
    }.

%% @doc The key one chunk's metadata is stored under.
key(Module, AbsoluteEndOffset) ->
    Offset = hb_util:int(AbsoluteEndOffset),
    hb_path:to_binary(
        [
            group(Module, ?CHUNKS),
            name(ar_chunk_storage:get_chunk_bucket_end(Offset)),
            name(Offset)
        ]
    ).

%% @doc The group every chunk of one 256 KiB bucket is stored in.
bucket_key(Module, BucketEnd) ->
    hb_path:to_binary(
        [group(Module, ?CHUNKS), name(hb_util:int(BucketEnd))]).

%%% Internal functions.

%% @doc The key the bytes of one chunk without a chunk file are stored under.
chunk_key(Module, AbsoluteEndOffset) ->
    hb_path:to_binary(
        [
            group(Module, ?CHUNK_DATA),
            name(hb_util:int(AbsoluteEndOffset))
        ]
    ).

%% @doc The group one module's entries of a kind are filed under. The module
%% names itself in the key rather than only in the store it is written to,
%% because `arweave-storage-index' lets an operator keep every module's index in
%% one store -- and two modules hold chunks at the same offsets wherever their
%% ranges overlap.
group(Module, Kind) ->
    hb_path:to_binary(
        [?STORAGE, hb_util:bin(lib_arweave_storage:id(Module)), Kind]).

%% @doc Name an offset in the fixed width every key of this index uses.
name(Offset) ->
    hb_util:bin(io_lib:format("~*..0B", [?OFFSET_DIGITS, Offset])).

%% @doc The absolute end offset a metadata message names. Every key of this
%% index is derived from it, so a message without one is not an entry and is
%% refused rather than filed at zero.
offset(Metadata) ->
    case hb_maps:get(<<"absolute-end-offset">>, Metadata, not_found) of
        not_found ->
            {error,
                error_message(
                    <<"invalid-chunk-metadata">>,
                    <<"A chunk index entry names no absolute end offset.">>
                )
            };
        Value ->
            {ok, hb_util:int(Value)}
    end.

%% @doc Read an integer field of a metadata message.
field(Key, Metadata) ->
    hb_util:int(hb_maps:get(Key, Metadata, 0)).

%% @doc Read a base64URL field of a metadata message. An absent field is the
%% empty binary, which is what an empty path is on the wire.
binary_field(Key, Metadata) ->
    case hb_util:safe_decode(hb_maps:get(Key, Metadata, <<>>)) of
        {ok, Decoded} ->
            {ok, Decoded};
        {error, _Reason} ->
            {error,
                error_message(
                    <<"invalid-chunk-metadata">>,
                    << "A chunk index field is not base64URL: ", Key/binary >>
                )
            }
    end.

%% @doc Build the stored binary, refusing a field the format cannot hold. A
%% field wider than the bits it is written into is truncated by the binary
%% construction rather than reported, and a truncated offset in an index is a
%% wrong answer rather than a missing one.
encoded(ChunkSize, RelativeOffset, TXRoot, DataRoot, TXPath, DataPath)
        when
        ?FITS(ChunkSize, 32),
        ?FITS(RelativeOffset, 64),
        byte_size(TXRoot) == 32,
        byte_size(DataRoot) == 32,
        ?FITS(byte_size(TXPath), 32),
        ?FITS(byte_size(DataPath), 32)
    ->
    {ok,
        <<
            ?FORMAT_VERSION:8,
            ChunkSize:32,
            RelativeOffset:64,
            TXRoot:32/binary,
            DataRoot:32/binary,
            (byte_size(TXPath)):32,
            TXPath/binary,
            (byte_size(DataPath)):32,
            DataPath/binary
        >>
    };
encoded(_ChunkSize, _RelativeOffset, _TXRoot, _DataRoot, _TXPath, _DataPath) ->
    {error,
        error_message(
            <<"invalid-chunk-metadata">>,
            <<"A chunk index field is outside the stored format's range.">>
        )
    }.

%% @doc Read the bytes of a chunk out of the value they are stored in.
chunk(<< ?FORMAT_VERSION:8, Chunk/binary >>) ->
    {ok, Chunk};
chunk(_Value) ->
    {error,
        error_message(
            <<"invalid-chunk-data">>,
            <<"A stored chunk is not in the stored format.">>
        )
    }.

%% @doc Build the metadata message of one entry, from the offset its key names
%% and the value it holds.
entry(AbsoluteEndOffset, Value, Opts) ->
    case decode(Value) of
        {ok, Metadata} ->
            {ok, Metadata#{ <<"absolute-end-offset">> => AbsoluteEndOffset }};
        {error, _Reason} = Error ->
            ?event(warning,
                {arweave_chunk_index_entry_undecodable,
                    {absolute_end_offset, AbsoluteEndOffset},
                    {size, byte_size(Value)}
                },
                Opts
            ),
            Error
    end.

%% @doc The entries that could hold a seek offset, nearest bucket first.
%%
%% A chunk is at most 256 KiB and ends at or after any byte it holds, so a
%% chunk holding the seek offset ends in `[Seek, Seek + 256 KiB)'. Bucket ends
%% rise in 256 KiB steps, so that range meets exactly two of them: the seek
%% offset's own bucket and the one above it. The second is only read when the
%% first holds nothing that could answer, which above the strict data split
%% threshold is never.
candidates(Module, Bucket, Seek, Opts) ->
    maybe
        {ok, []} ?= above(Module, Bucket, Seek, Opts),
        above(Module, Bucket + ?DATA_CHUNK_SIZE, Seek, Opts)
    end.

%% @doc The entries of one bucket whose chunks end at or after a seek offset,
%% by ascending absolute end offset. A chunk ending before the seek offset
%% cannot hold it.
above(Module, Bucket, Seek, Opts) ->
    maybe
        {ok, Entries} ?= list_bucket(Module, Bucket, Opts),
        {ok,
            [
                Entry
            ||
                Entry <- Entries,
                field(<<"absolute-end-offset">>, Entry) >= Seek
            ]
        }
    end.

%% @doc Select the chunk a seek offset falls in, as an Arweave node's ordered
%% seek does: the nearest chunk ending at or after the seek offset, refused
%% when that chunk starts after it. The candidates are ordered, so the first is
%% the nearest, and a further one could only be further away still.
covering([], _Seek) ->
    not_found;
covering([Metadata | _Rest], Seek) ->
    Distance = field(<<"absolute-end-offset">>, Metadata) - Seek,
    case Distance < field(<<"chunk-size">>, Metadata) of
        true -> {ok, Metadata};
        false -> not_found
    end.

%% @doc The offset and stored value of every entry of one bucket, by ascending
%% offset. A store that returns each child's value alongside its name -- which
%% `hb_store_lmdb' does, from the one scan that listed them -- is taken at its
%% word; one that returns names alone is read again for each. A bucket that
%% holds nothing is empty rather than an error, because an index is asked about
%% offsets it does not hold on every recall range that misses.
children(Key, Opts) ->
    case hb_store:read(lib_arweave_storage:store(Opts), Key, Opts) of
        {composite, Children} ->
            lists:sort(
                lists:filtermap(
                    fun(Child) -> child(Key, Child, Opts) end,
                    Children
                )
            );
        _Absent ->
            []
    end.

%% @doc Read one listed child of a bucket. A deleted entry holds a marker
%% rather than a value, and is not one.
child(_Key, {_Name, ?DELETED}, _Opts) ->
    false;
child(_Key, {Name, Value}, _Opts) ->
    {true, {hb_util:int(Name), Value}};
child(Key, Name, Opts) ->
    case read(hb_path:to_binary([Key, Name]), Opts) of
        {ok, Value} -> {true, {hb_util:int(Name), Value}};
        _Absent -> false
    end.

%% @doc Read one key of a module's index. A key that was deleted holds a marker
%% rather than an entry and reads as absent, as does a key a store cannot
%% answer for -- which is what an Arweave node does with a failed index read
%% too, because a chunk it cannot read is a chunk it cannot prove.
read(Key, Opts) ->
    case hb_store:read(lib_arweave_storage:store(Opts), Key, Opts) of
        {ok, ?DELETED} -> not_found;
        {ok, Value} -> {ok, Value};
        _Absent -> not_found
    end.

%% @doc Write one key of a module's index.
write(Key, Value, Opts) ->
    hb_store:write(lib_arweave_storage:store(Opts), #{ Key => Value }, Opts).

%% @doc Reduce a list of per-entry results to one result over the list. An
%% entry that cannot be read makes the bucket unanswerable, because a bucket
%% missing one of its chunks is what a lookup would silently take for an offset
%% this node does not hold.
collect([], Entries) ->
    {ok, lists:reverse(Entries)};
collect([{ok, Entry} | Rest], Entries) ->
    collect(Rest, [Entry | Entries]);
collect([Error | _Rest], _Entries) ->
    Error.

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.
