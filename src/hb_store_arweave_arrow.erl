%%% @doc A store that maintains an append-only index chain on Arweave.
%%% Each index slab references the previous slab, allowing key lookups by
%%% seeking through only the required bytes on Arweave's flat address space.
-module(hb_store_arweave_arrow).
%%% Store API:
-export([scope/0, scope/1, type/2, read/2, write/3, start/1]).
%%% Unused Store API:
-export([resolve/2, make_link/3, make_group/2]).
%%% Index API:
-export([read_head/1, write_head/3, read_location/2]).
-export([write_update/2, write_update_with_locations/2, read_range/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SLAB_MAGIC, <<"HBAA">>).
-define(SLAB_VERSION, 1).
-define(LOCATION_VERSION, 1).
-define(HEADER_SIZE, 40).
-define(DIRECTORY_ENTRY_SIZE, 16).
-define(NO_PREVIOUS, 16#FFFFFFFFFFFFFFFF).
-define(DEFAULT_BUCKET_COUNT, 128).
-define(DEFAULT_ARWEAVE_DEVICE, <<"arweave@1.0">>).
-define(DEFAULT_CHUNK_PATH, <<"chunk">>).
-define(DEFAULT_APPEND_PATH, <<"append">>).

%% @doc Start the local head store if configured.
start(StoreOpts) ->
    case head_store(StoreOpts) of
        no_store -> ok;
        HeadStore -> hb_store:start(HeadStore)
    end.

%% @doc This store reads from remote Arweave offsets.
scope() -> remote.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().

%% @doc Resolve a key path to a binary key.
resolve(_, Key) when is_binary(Key) -> Key;
resolve(_, Key) when is_list(Key) -> hb_store:join(Key);
resolve(_, _) -> not_found.

%% @doc Unsupported.
make_link(_, _, _) -> not_found.

%% @doc Unsupported.
make_group(_, _) -> not_found.

%% @doc Return `simple` when a key is indexed.
type(StoreOpts, Key) ->
    case read_location(StoreOpts, Key) of
        {ok, _} -> simple;
        _ -> not_found
    end.

%% @doc Read a value by following the index chain to its Arweave byte range.
read(StoreOpts, Key) ->
    case read_location(StoreOpts, Key) of
        {ok, #{ <<"start-offset">> := Start, <<"length">> := Length }} ->
            read_range(Start, Length, StoreOpts);
        _ ->
            not_found
    end.

%% @doc Write a single key-value update.
write(StoreOpts, Key, Value) when is_binary(Value) ->
    case write_update(StoreOpts, [{Key, Value}]) of
        {ok, _Head} -> ok;
        Other -> Other
    end;
write(_, _, _) -> not_found.

%% @doc Write a batch update by uploading values, then uploading a new slab.
write_update(StoreOpts, Updates) ->
    case normalize_value_updates(Updates) of
        {error, _} = Error ->
            Error;
        Normalized ->
            case upload_values(Normalized, StoreOpts, []) of
                {error, _} = Error ->
                    Error;
                {ok, Locations} ->
                    write_update_with_locations(StoreOpts, Locations)
            end
    end.

%% @doc Upload a slab with explicit key->location mappings and advance head.
write_update_with_locations(StoreOpts, Updates) ->
    case normalize_location_updates(Updates) of
        {error, _} = Error ->
            Error;
        Normalized ->
            Previous =
                case read_head(StoreOpts) of
                    {ok, Head} -> Head;
                    _ -> none
                end,
            BucketCount = bucket_count(StoreOpts),
            Slab =
                encode_slab(
                    Normalized,
                    Previous,
                    BucketCount
                ),
            case append_binary(StoreOpts, Slab) of
                {ok,
                    #{
                        <<"start-offset">> := Start,
                        <<"length">> := Length
                    } = NewHead} ->
                    case write_head(StoreOpts, Start, Length) of
                        ok -> {ok, NewHead};
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc Read the current slab head from local storage.
read_head(StoreOpts) ->
    case head_store(StoreOpts) of
        no_store ->
            not_found;
        HeadStore ->
            case hb_store:read(HeadStore, head_key(StoreOpts)) of
                {ok, Encoded} ->
                    case decode_location(Encoded) of
                        {ok, {Start, Length}} ->
                            {ok, location(Start, Length)};
                        {error, _} = Error ->
                            Error
                    end;
                _ ->
                    not_found
            end
    end.

%% @doc Persist the current slab head into local storage.
write_head(StoreOpts, Start, Length)
        when is_integer(Start), Start >= 0, is_integer(Length), Length >= 0 ->
    case head_store(StoreOpts) of
        no_store ->
            {error, no_head_store};
        HeadStore ->
            hb_store:write(HeadStore, head_key(StoreOpts), encode_location(Start, Length))
    end;
write_head(_, _, _) ->
    {error, invalid_head}.

%% @doc Resolve a key to its Arweave byte range by traversing the slab chain.
read_location(StoreOpts, Key) ->
    try normalize_key(Key) of
        KeyBin ->
            case read_head(StoreOpts) of
                {ok, Head} ->
                    lookup_chain(KeyBin, Head, sets:new(), StoreOpts);
                not_found ->
                    not_found;
                {error, _} = Error ->
                    Error
            end
    catch
        throw:{invalid_arrow_key, _} -> not_found
    end.

%% @doc Read a byte range from Arweave by offset and length.
read_range(StartOffset, Length, _StoreOpts)
        when
        not is_integer(StartOffset) orelse StartOffset < 0
        orelse not is_integer(Length) orelse Length < 0 ->
    {error, invalid_range};
read_range(_StartOffset, 0, _StoreOpts) ->
    {ok, <<>>};
read_range(StartOffset, Length, StoreOpts) ->
    Device = arweave_device(StoreOpts),
    Request = #{
        <<"path">> => chunk_path(StoreOpts),
        <<"offset">> => StartOffset + 1,
        <<"length">> => Length
    },
    case hb_ao:resolve(#{ <<"device">> => Device }, Request, StoreOpts) of
        {ok, Data} when is_binary(Data) ->
            {ok, Data};
        {ok, #{ <<"body">> := Data }} when is_binary(Data) ->
            {ok, Data};
        not_found ->
            not_found;
        {error, _} = Error ->
            Error;
        Other ->
            {error, {invalid_chunk_response, Other}}
    end.

%% @doc Traverse slabs from newest to oldest until a key is found.
lookup_chain(
        Key,
        #{ <<"start-offset">> := Start },
        Visited,
        StoreOpts
    ) ->
    case sets:is_element(Start, Visited) of
        true ->
            {error, cycle_detected};
        false ->
            case read_slab_header(Start, StoreOpts) of
                {ok, Header} ->
                    BucketIndex = bucket_index(Key, maps:get(bucket_count, Header)),
                    case read_bucket_entry(Key, Start, Header, BucketIndex, StoreOpts) of
                        {ok, Location} ->
                            {ok, Location};
                        not_found ->
                            case maps:get(previous, Header, none) of
                                none ->
                                    not_found;
                                Previous ->
                                    lookup_chain(
                                        Key,
                                        Previous,
                                        sets:add_element(Start, Visited),
                                        StoreOpts
                                    )
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error;
                not_found ->
                    not_found
            end
    end.

%% @doc Read and decode a slab header.
read_slab_header(StartOffset, StoreOpts) ->
    case read_range(StartOffset, ?HEADER_SIZE, StoreOpts) of
        {ok, HeaderBinary} ->
            decode_slab_header(HeaderBinary);
        Other ->
            Other
    end.

%% @doc Read a specific directory entry and scan the bucket for a key.
read_bucket_entry(Key, SlabStart, Header, BucketIndex, StoreOpts) ->
    DirectoryOffset = maps:get(directory_offset, Header),
    EntryOffset =
        SlabStart
            + DirectoryOffset
            + (BucketIndex * ?DIRECTORY_ENTRY_SIZE),
    case read_range(EntryOffset, ?DIRECTORY_ENTRY_SIZE, StoreOpts) of
        {ok, <<_BucketOffset:64, BucketLength:64>>} when BucketLength =:= 0 ->
            not_found;
        {ok, <<BucketOffset:64, BucketLength:64>>} ->
            case read_range(SlabStart + BucketOffset, BucketLength, StoreOpts) of
                {ok, BucketBinary} ->
                    find_in_bucket(Key, BucketBinary);
                Other ->
                    Other
            end;
        {ok, _Other} ->
            {error, malformed_directory_entry};
        Other ->
            Other
    end.

%% @doc Parse a bucket and return the matching location if present.
find_in_bucket(_Key, <<Count:32>>) when Count =:= 0 ->
    not_found;
find_in_bucket(Key, <<Count:32, Rest/binary>>) ->
    find_in_bucket(Key, Count, Rest);
find_in_bucket(_Key, _BucketBinary) ->
    {error, malformed_bucket}.

%% @doc Walk bucket entries.
find_in_bucket(_Key, 0, _Rest) ->
    not_found;
find_in_bucket(
        Key,
        Count,
        <<KeyLength:16, EntryKey:KeyLength/binary, Start:64, Length:64, Rest/binary>>
    ) ->
    case EntryKey =:= Key of
        true -> {ok, location(Start, Length)};
        false -> find_in_bucket(Key, Count - 1, Rest)
    end;
find_in_bucket(_Key, _Count, _Rest) ->
    {error, malformed_bucket_entry}.

%% @doc Upload a list of values and return key->location pairs.
upload_values([], _StoreOpts, Acc) ->
    {ok, lists:reverse(Acc)};
upload_values([{Key, Value} | Rest], StoreOpts, Acc) ->
    case append_binary(StoreOpts, Value) of
        {ok, Location} ->
            upload_values(Rest, StoreOpts, [{Key, Location} | Acc]);
        {error, _} = Error ->
            Error
    end.

%% @doc Upload bytes using the configured append device path.
append_binary(StoreOpts, Data) when is_binary(Data) ->
    Device = append_device(StoreOpts),
    Request = #{
        <<"path">> => append_path(StoreOpts),
        <<"body">> => Data
    },
    case hb_ao:resolve(#{ <<"device">> => Device }, Request, StoreOpts) of
        {ok, #{ <<"start-offset">> := Start, <<"length">> := Length }} ->
            {ok, location(Start, Length)};
        {ok, #{ <<"offset">> := Start, <<"length">> := Length }} ->
            {ok, location(Start, Length)};
        {error, _} = Error ->
            Error;
        Other ->
            {error, {invalid_append_response, Other}}
    end.

%% @doc Encode a slab with directory and bucket sections.
encode_slab(Pairs, Previous, BucketCount) ->
    BucketMap = bucket_map(Pairs, BucketCount),
    DirectoryStart = ?HEADER_SIZE,
    DirectorySize = BucketCount * ?DIRECTORY_ENTRY_SIZE,
    BucketsStart = DirectoryStart + DirectorySize,
    {DirectoryEntries, BucketBins, EntryCount} =
        build_buckets(
            BucketMap,
            BucketCount,
            BucketsStart,
            0,
            [],
            [],
            0
        ),
    Directory = encode_directory(DirectoryEntries),
    Header = encode_slab_header(BucketCount, Previous, EntryCount),
    iolist_to_binary([Header, Directory, BucketBins]).

%% @doc Build map of bucket index -> list of entries.
bucket_map(Pairs, BucketCount) ->
    lists:foldl(
        fun({Key, Location}, Acc) ->
            Index = bucket_index(Key, BucketCount),
            Existing = maps:get(Index, Acc, []),
            Acc#{ Index => [{Key, Location} | Existing] }
        end,
        #{},
        Pairs
    ).

%% @doc Build bucket binaries and their directory entries.
build_buckets(
        _BucketMap,
        BucketCount,
        _CurrentOffset,
        BucketCount,
        DirectoryAcc,
        BucketAcc,
        EntryCount
    ) ->
    {lists:reverse(DirectoryAcc), lists:reverse(BucketAcc), EntryCount};
build_buckets(
        BucketMap,
        BucketCount,
        CurrentOffset,
        Index,
        DirectoryAcc,
        BucketAcc,
        EntryCount
    ) ->
    Entries = sort_entries(maps:get(Index, BucketMap, [])),
    case Entries of
        [] ->
            build_buckets(
                BucketMap,
                BucketCount,
                CurrentOffset,
                Index + 1,
                [{0, 0} | DirectoryAcc],
                BucketAcc,
                EntryCount
            );
        _ ->
            Bucket = encode_bucket(Entries),
            BucketLength = byte_size(Bucket),
            build_buckets(
                BucketMap,
                BucketCount,
                CurrentOffset + BucketLength,
                Index + 1,
                [{CurrentOffset, BucketLength} | DirectoryAcc],
                [Bucket | BucketAcc],
                EntryCount + length(Entries)
            )
    end.

%% @doc Sort entries by key.
sort_entries(Entries) ->
    lists:sort(
        fun({KeyA, _}, {KeyB, _}) ->
            KeyA =< KeyB
        end,
        Entries
    ).

%% @doc Encode a bucket.
encode_bucket(Entries) ->
    EncodedEntries =
        lists:map(
            fun({Key, #{ <<"start-offset">> := Start, <<"length">> := Length }}) ->
                KeyLength = byte_size(Key),
                if
                    KeyLength > 16#FFFF ->
                        throw({key_too_large, KeyLength});
                    true ->
                        <<
                            KeyLength:16,
                            Key/binary,
                            Start:64,
                            Length:64
                        >>
                end
            end,
            Entries
        ),
    iolist_to_binary([<< (length(Entries)):32 >>, EncodedEntries]).

%% @doc Encode directory entries.
encode_directory(Entries) ->
    iolist_to_binary(
        [
            <<Offset:64, Length:64>>
        ||
            {Offset, Length} <- Entries
        ]
    ).

%% @doc Encode slab header.
encode_slab_header(BucketCount, Previous, EntryCount) ->
    {PrevStart, PrevLength} = encode_previous(Previous),
    <<
        ?SLAB_MAGIC/binary,
        ?SLAB_VERSION:8,
        0:8,
        BucketCount:16,
        PrevStart:64,
        PrevLength:64,
        ?HEADER_SIZE:64,
        EntryCount:64
    >>.

%% @doc Decode slab header.
decode_slab_header(
        <<
            "HBAA",
            ?SLAB_VERSION:8,
            _Flags:8,
            BucketCount:16,
            PrevStart:64,
            PrevLength:64,
            DirectoryOffset:64,
            EntryCount:64
        >>
    ) ->
    {ok, #{
        bucket_count => BucketCount,
        previous => decode_previous(PrevStart, PrevLength),
        directory_offset => DirectoryOffset,
        entry_count => EntryCount
    }};
decode_slab_header(<< "HBAA", Version:8, _/binary >>) ->
    {error, {unsupported_slab_version, Version}};
decode_slab_header(_) ->
    {error, malformed_slab_header}.

%% @doc Encode the previous slab pointer.
encode_previous(none) ->
    {?NO_PREVIOUS, 0};
encode_previous(#{ <<"start-offset">> := Start, <<"length">> := Length }) ->
    {Start, Length}.

%% @doc Decode the previous slab pointer.
decode_previous(?NO_PREVIOUS, _Length) ->
    none;
decode_previous(Start, Length) ->
    location(Start, Length).

%% @doc Normalize update keys and binary values.
normalize_value_updates(Updates) ->
    normalize_updates(
        Updates,
        fun(Value) when is_binary(Value) ->
            {ok, Value};
           (_Value) ->
            {error, invalid_update_value}
        end
    ).

%% @doc Normalize update keys and explicit locations.
normalize_location_updates(Updates) ->
    normalize_updates(
        Updates,
        fun(Value) ->
            normalize_location_value(Value)
        end
    ).

%% @doc Normalize update list/map and keep the latest value per key.
normalize_updates(Updates, ValueNormalizer) when is_map(Updates) ->
    normalize_updates(maps:to_list(Updates), ValueNormalizer);
normalize_updates(Updates, ValueNormalizer) when is_list(Updates) ->
    try
        KeyValueMap =
            lists:foldl(
                fun({RawKey, RawValue}, Acc) ->
                    Key = normalize_key(RawKey),
                    case ValueNormalizer(RawValue) of
                        {ok, Value} ->
                            Acc#{ Key => Value };
                        {error, _} = Error ->
                            throw(Error)
                    end
                end,
                #{},
                Updates
            ),
        sort_entries(maps:to_list(KeyValueMap))
    catch
        throw:{error, _} = Error -> Error;
        throw:{invalid_arrow_key, _} -> {error, invalid_update_key}
    end;
normalize_updates(_Updates, _ValueNormalizer) ->
    {error, invalid_updates}.

%% @doc Normalize a location value.
normalize_location_value(#{ <<"start-offset">> := Start, <<"length">> := Length }) ->
    normalize_location_value({Start, Length});
normalize_location_value({Start, Length})
        when
        is_integer(Start),
        Start >= 0,
        is_integer(Length),
        Length >= 0 ->
    {ok, location(Start, Length)};
normalize_location_value(_) ->
    {error, invalid_location_value}.

%% @doc Normalize a key.
normalize_key(Key) when is_binary(Key) ->
    Key;
normalize_key(Key) when is_list(Key) ->
    hb_store:join(Key);
normalize_key(Key) ->
    throw({invalid_arrow_key, Key}).

%% @doc Encode a location for persistent local head storage.
encode_location(Start, Length) ->
    <<?LOCATION_VERSION:8, Start:64, Length:64>>.

%% @doc Decode a location from local head storage.
decode_location(<<?LOCATION_VERSION:8, Start:64, Length:64>>) ->
    {ok, {Start, Length}};
decode_location(_) ->
    {error, malformed_location}.

%% @doc Build a location message.
location(Start, Length) ->
    #{
        <<"start-offset">> => Start,
        <<"length">> => Length
    }.

%% @doc Compute bucket index for a key.
bucket_index(Key, BucketCount) ->
    erlang:phash2(Key, BucketCount).

%% @doc Read head store option.
head_store(StoreOpts) ->
    opt(<<"head-store">>, arweave_arrow_head_store, no_store, StoreOpts).

%% @doc Read head key option.
head_key(StoreOpts) ->
    opt(
        <<"head-key">>,
        arweave_arrow_head_key,
        default_head_key(StoreOpts),
        StoreOpts
    ).

%% @doc Read the configured Arweave device name.
arweave_device(StoreOpts) ->
    opt(
        <<"arweave-device">>,
        arweave_arrow_device,
        ?DEFAULT_ARWEAVE_DEVICE,
        StoreOpts
    ).

%% @doc Read the configured append device name.
append_device(StoreOpts) ->
    opt(
        <<"append-device">>,
        arweave_arrow_append_device,
        arweave_device(StoreOpts),
        StoreOpts
    ).

%% @doc Read the configured chunk path.
chunk_path(StoreOpts) ->
    opt(
        <<"chunk-path">>,
        arweave_arrow_chunk_path,
        ?DEFAULT_CHUNK_PATH,
        StoreOpts
    ).

%% @doc Read the configured append path.
append_path(StoreOpts) ->
    opt(
        <<"append-path">>,
        arweave_arrow_append_path,
        ?DEFAULT_APPEND_PATH,
        StoreOpts
    ).

%% @doc Read and clamp bucket count.
bucket_count(StoreOpts) ->
    Requested = hb_util:int(
        opt(
            <<"bucket-count">>,
            arweave_arrow_bucket_count,
            ?DEFAULT_BUCKET_COUNT,
            StoreOpts
        )
    ),
    erlang:min(16#FFFF, erlang:max(1, Requested)).

%% @doc Generate default local head key for the store.
default_head_key(StoreOpts) ->
    StoreName = hb_util:bin(opt(<<"name">>, name, <<"default">>, StoreOpts)),
    <<StoreName/binary, "/arweave-arrow/head">>.

%% @doc Read an option with local key override support.
opt(BinaryKey, AtomKey, Default, StoreOpts) ->
    case maps:find(BinaryKey, StoreOpts) of
        {ok, Value} ->
            Value;
        error ->
            case maps:find(AtomKey, StoreOpts) of
                {ok, Value} ->
                    Value;
                error ->
                    hb_opts:get(AtomKey, Default, StoreOpts)
            end
    end.

%%% Tests

write_read_roundtrip_test() ->
    with_test_store(
        fun(Store) ->
            ok = write(Store, <<"alpha">>, <<"A">>),
            ok = write(Store, <<"beta">>, <<"B">>),
            ?assertEqual(simple, type(Store, <<"alpha">>)),
            ?assertEqual({ok, <<"A">>}, read(Store, <<"alpha">>)),
            ?assertEqual({ok, <<"B">>}, read(Store, <<"beta">>)),
            ?assertEqual(not_found, read(Store, <<"missing">>))
        end
    ).

chained_updates_allow_interspersed_offsets_test() ->
    with_test_store(
        fun(Store) ->
            ok = write(Store, <<"first">>, <<"FIRST">>),
            {ok, Head1} = read_head(Store),
            {ok, _Noise} = append_binary(Store, <<"NOISE">>),
            ok = write(Store, <<"second">>, <<"SECOND">>),
            {ok, Head2} = read_head(Store),
            Head1End =
                maps:get(<<"start-offset">>, Head1)
                    + maps:get(<<"length">>, Head1),
            ?assert(maps:get(<<"start-offset">>, Head2) > Head1End),
            ?assertEqual({ok, <<"FIRST">>}, read(Store, <<"first">>)),
            ?assertEqual({ok, <<"SECOND">>}, read(Store, <<"second">>))
        end
    ).

lookup_reads_only_needed_segments_test() ->
    with_test_store(
        fun(Store) ->
            ok = write(Store, <<"older">>, <<"OLD-DATA">>),
            {ok, _Noise} = append_binary(Store, <<"INTERSPERSED">>),
            ok = write(Store, <<"newer">>, <<"NEW-DATA">>),
            {ok, Head} = read_head(Store),
            ok = dev_arweave_flat:clear_reads(#{}, #{}, Store),
            ?assertEqual({ok, <<"OLD-DATA">>}, read(Store, <<"older">>)),
            {ok, Stats} = dev_arweave_flat:stats(#{}, #{}, Store),
            Reads = maps:get(<<"reads">>, Stats),
            ?assert(length(Reads) >= 4),
            ?assert(
                lists:member(
                    {
                        maps:get(<<"start-offset">>, Head),
                        ?HEADER_SIZE
                    },
                    Reads
                )
            ),
            ?assert(
                lists:any(
                    fun({_Offset, Length}) -> Length =:= ?DIRECTORY_ENTRY_SIZE end,
                    Reads
                )
            ),
            ?assert(
                not lists:member(
                    {
                        maps:get(<<"start-offset">>, Head),
                        maps:get(<<"length">>, Head)
                    },
                    Reads
                )
            )
        end
    ).

%% @doc Execute a test with isolated mock Arweave state.
with_test_store(Fun) ->
    {Store, Table} = new_test_store(),
    try Fun(Store)
    after
        catch hb_store:reset(head_store(Store)),
        catch ets:delete(Table)
    end.

%% @doc Construct a store with a mock flat-address Arweave device.
new_test_store() ->
    Table = ets:new(?MODULE, [set, public]),
    HeadStore = [hb_test_utils:test_store(hb_store_volatile, <<"arrow-head">>)],
    hb_store:reset(HeadStore),
    hb_store:start(HeadStore),
    DefaultDevices = hb_opts:get(preloaded_devices, []),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"arweave-arrow-test">>,
        <<"head-store">> => HeadStore,
        <<"bucket-count">> => 8,
        <<"arweave-device">> => <<"arweave-flat@1.0">>,
        <<"append-device">> => <<"arweave-flat@1.0">>,
        arweave_flat_table => Table,
        preloaded_devices =>
            DefaultDevices ++
                [
                    #{
                        <<"name">> => <<"arweave-flat@1.0">>,
                        <<"module">> => dev_arweave_flat
                    }
                ]
    },
    ok = dev_arweave_flat:reset(#{}, #{}, Store),
    {Store, Table}.
