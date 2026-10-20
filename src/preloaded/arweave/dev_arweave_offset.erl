%%% @doc A module for the Arweave device that implements the default key 
%%% resolution logic. The default key returns slices of bytes inside Arweave as
%%% message representations.
-module(dev_arweave_offset).
-export([get/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Resolve either a message at an Arweave offset, or a direct key from the
%% base message if the key is not an integer.
-spec get(binary(), #{ _ => _ }, #{ _ => _ }, map()) -> term().
get(Key, Base, _Request, Opts) ->
    case parse(Key) of
        {ok, StartOffset, Length} ->
            load_item_at_offset(StartOffset, Length, Opts);
        error ->
            case hb_maps:get(Key, Base, not_found, Opts) of
                not_found -> {error, not_found};
                Value -> {ok, Value}
            end
    end.

%% @doc Parse a path key as a global Arweave start offset. The supported syntax
%% is as follows:
%% ```
%% Reference :: Offset-Length
%% Offset :: <integer>[Unit]
%% Length :: <integer>
%% Unit ::
%%     b        : The global Arweave offset in absolute bytes (default).
%%     k[i][b]  : The global Arweave offset in absolute kilobytes or kibibytes.
%%     m[i][b]  : The global Arweave offset in absolute megabytes or mebibytes.
%%     g[i][b]  : The global Arweave offset in absolute gigabytes or gibibytes.
%%     t[i][b]  : The global Arweave offset in absolute terabytes or tebibytes.
%%     p[i][b]  : The global Arweave offset in absolute petabytes or pebibytes.
%%     e[i][b]  : The global Arweave offset in absolute exabytes or exbibytes.
%%     z[i][b]  : The global Arweave offset in absolute zettabytes or zebibytes.
%%     y[i][b]  : The global Arweave offset in absolute yottabytes or yobibytes.
%% ```
%% In the scheme above, the `i` modifier in units indicates that the unit is in
%% binary multiples of the base unit. For example, `kib` is 1024 bytes, `mib` is
%% 1024 * 1024 bytes, etc. By contrast, the `kb` unit is decimal-oriented: `kb`
%% 1000 bytes, `mb` is 1000 * 1000 bytes, etc. To aid minimization of the bytes
%% required for the references, the `b` is always implied and need not be
%% specified.
parse(Key) ->
    try
        {OffsetBin, Length} =
            case binary:split(Key, <<"-">>) of
                [Start, LengthBin] -> {Start, hb_util:int(LengthBin)};
                [Start] -> {Start, undefined}
            end,
        {ok, unit(OffsetBin), Length}
    catch
        _Class:_Error:_StackTrace -> error
    end.

%% @doc Parses and applies a unit modifier to a base value, supporting both
%% the `kb` and `kib` unit formats.
unit(Binary) ->
    UnitWithoutOptionalB =
        case Binary of
            <<Prefix:(byte_size(Binary) - 1)/binary, "b">> -> Prefix;
            _ -> Binary
        end,
    unit(0, UnitWithoutOptionalB).
unit(Complete, <<>>) -> Complete;
unit(Base, <<Int:8/integer, Rest/binary>>) when Int >= $0 andalso Int =< $9 ->
    unit(Base * 10 + (Int - $0), Rest);
unit(Base, <<"ki">>) when Base > 0 -> unit(Base * 1024, <<>>);
unit(Base, <<"mi">>) when Base > 0 -> unit(Base * 1024, <<"ki">>);
unit(Base, <<"gi">>) when Base > 0 -> unit(Base * 1024, <<"mi">>);
unit(Base, <<"ti">>) when Base > 0 -> unit(Base * 1024, <<"gi">>);
unit(Base, <<"pi">>) when Base > 0 -> unit(Base * 1024, <<"ti">>);
unit(Base, <<"ei">>) when Base > 0 -> unit(Base * 1024, <<"pi">>);
unit(Base, <<"zi">>) when Base > 0 -> unit(Base * 1024, <<"ei">>);
unit(Base, <<"yi">>) when Base > 0 -> unit(Base * 1024, <<"zi">>);
unit(Base, <<"k">>) when Base > 0 -> unit(Base * 1000, <<>>);
unit(Base, <<"m">>) when Base > 0 -> unit(Base * 1000, <<"k">>);
unit(Base, <<"g">>) when Base > 0 -> unit(Base * 1000, <<"m">>);
unit(Base, <<"t">>) when Base > 0 -> unit(Base * 1000, <<"g">>);
unit(Base, <<"p">>) when Base > 0 -> unit(Base * 1000, <<"t">>);
unit(Base, <<"e">>) when Base > 0 -> unit(Base * 1000, <<"p">>);
unit(Base, <<"z">>) when Base > 0 -> unit(Base * 1000, <<"e">>);
unit(Base, <<"y">>) when Base > 0 -> unit(Base * 1000, <<"z">>).

%% @doc Load an ANS-104 item whose header begins at the given global offset.
%% When a length is supplied it is treated as the exact ANS-104 data length, so
%% we can skip bundle index discovery and read only the remaining payload bytes.
load_item_at_offset(ExplicitOffset, Length, Opts) when is_integer(Length) ->
    maybe
        {ok, _ChunkJSON, FirstChunk} ?= chunk_from_offset(ExplicitOffset, Opts),
        {ok, HeaderChunk} ?=
            header_chunk(ExplicitOffset, infinity, FirstChunk, Opts),
        ?event(
            arweave_offset_lookup,
            {loaded_explicit_offset,
                {explicit_offset, ExplicitOffset},
                {length, Length}
            },
            Opts
        ),
        load_item_from_data_size(ExplicitOffset, Length, HeaderChunk, Opts)
    end;
load_item_at_offset(TargetOffset, undefined, Opts) ->
    maybe
        {ok, StartOffset, ItemSize, FirstChunk} ?=
            message_from_offset(TargetOffset, Opts),
        load_item_from_serialized_size(StartOffset, ItemSize, FirstChunk, Opts)
    else
        false -> {error, invalid_item_size};
        Error -> Error
    end.

%% @doc Load an item when the exact ANS-104 data length is already known.
load_item_from_data_size(StartOffset, DataSize, FirstChunk, Opts) ->
    maybe
        {ok, HeaderSize, HeaderTX} ?= deserialize_header(FirstChunk),
        load_item_from_header(StartOffset, HeaderSize, HeaderTX, DataSize, Opts)
    end.

%% @doc Load an item when its serialized size is known from the containing
%% bundle index.
load_item_from_serialized_size(StartOffset, ItemSize, FirstChunk, Opts) ->
    maybe
        {ok, HeaderSize, HeaderTX} ?= deserialize_header(FirstChunk),
        true ?= HeaderSize =< ItemSize,
        load_item_from_header(
            StartOffset,
            HeaderSize,
            HeaderTX,
            ItemSize - HeaderSize,
            Opts
        )
    else
        false -> {error, invalid_item_size};
        Error -> Error
    end.

%% @doc Complete an item load once the header has been decoded, using any data
%% bytes that were already present after the header before reading the tail.
load_item_from_header(StartOffset, HeaderSize, HeaderTX, DataSize, Opts) ->
    {HeaderData, RemainingLength} =
        split_header_data(HeaderTX#tx.data, DataSize),
    ?event(
        arweave_offset_lookup,
        {calculating_message_from_offset,
            {start_offset, StartOffset},
            {header_size, HeaderSize},
            {data_size, DataSize},
            {header_data, HeaderData},
            {remaining_length, RemainingLength}
        },
        Opts
    ),
    maybe
        {ok, RemainingData} ?=
            read_remaining_data(
                StartOffset,
                HeaderSize,
                byte_size(HeaderData),
                RemainingLength,
                Opts
            ),
        FullTX =
            HeaderTX#tx{
                data = << HeaderData/binary, RemainingData/binary >>,
                data_size = DataSize
            },
        {ok,
            hb_message:convert(
                FullTX,
                <<"structured@1.0">>,
                <<"ans104@1.0">>,
                Opts
            )
        }
    end.

%% @doc Read the chunk containing the given offset and trim it to begin at the
%% first byte of the requested item.
chunk_from_offset(StartOffset, Opts) ->
    case dev_arweave:get_chunk(StartOffset + 1, Opts) of
        {ok, ChunkJSON} ->
            AbsEnd = hb_util:int(maps:get(<<"absolute_end_offset">>, ChunkJSON)),
            Chunk = hb_util:decode(maps:get(<<"chunk">>, ChunkJSON)),
            ChunkStart = AbsEnd - byte_size(Chunk) + 1,
            Skip = (StartOffset + 1) - ChunkStart,
            {ok, ChunkJSON, binary:part(Chunk, Skip, byte_size(Chunk) - Skip)};
        Error ->
            Error
    end.

%% @doc Safe wraper for ANS-104 header deserialization.
deserialize_header(Binary) ->
    try ar_bundles:deserialize_header(Binary)
    catch _:_ -> {error, <<"Invalid message header">>}
    end.

%% @doc Split the bytes already present after a decoded header from those that
%% still need to be read from Arweave.
split_header_data(HeaderData, DataSize) ->
    PrefixSize = min(byte_size(HeaderData), DataSize),
    {
        binary:part(HeaderData, 0, PrefixSize),
        DataSize - PrefixSize
    }.

%% @doc Read any bytes of the data segment that were not present in the first
%% header chunk.
read_remaining_data(_StartOffset, _HeaderSize, _PrefixSize, 0, _Opts) ->
    {ok, <<>>};
read_remaining_data(StartOffset, HeaderSize, PrefixSize, Length, Opts) ->
    hb_store_arweave:read_chunks(StartOffset + HeaderSize + PrefixSize, Length, Opts).

%% @doc Locate the deepest bundled item that contains the given global offset.
message_from_offset(TargetOffset, Opts) ->
    maybe
        {ok, ChunkJSON, FirstChunk} ?= chunk_from_offset(TargetOffset, Opts),
        message_from_offset(
            TargetOffset,
            bundle_start_offset(ChunkJSON),
            TargetOffset,
            FirstChunk,
            Opts
        )
    end.

%% @doc Recover the global start offset of the containing bundle from the end
%% offset of the chunk in global space and its end offset inside the bundle.
bundle_start_offset(ChunkJSON) ->
    AbsEnd = hb_util:int(maps:get(<<"absolute_end_offset">>, ChunkJSON)),
    ChunkEndInBundle =
        ar_merkle:extract_note(
            hb_util:decode(maps:get(<<"data_path">>, ChunkJSON))
        ),
    AbsEnd - ChunkEndInBundle.

message_from_offset(TargetOffset, BundleStartOffset, KnownOffset, KnownChunk, Opts) ->
    maybe
        {ok, _ChunkJSON, BundleChunk} ?=
            chunk_from_offset(BundleStartOffset, Opts),
        % A member contains the target only if the header ends at or before the
        % target, so a longer header gives `not_found` without reading the rest
        % of it. `bundle_header/4` refuses a chunk too short to contain the item
        % count.
        ClaimedSize = ar_bundles:bundle_header_size(BundleChunk),
        true ?=
            ClaimedSize =:= invalid_bundle_header
                orelse ClaimedSize =< TargetOffset - BundleStartOffset
                orelse {error, not_found},
        {ok, HeaderSize, BundleIndex} ?=
            lib_arweave_common:bundle_header(
                BundleStartOffset,
                infinity,
                BundleChunk,
                Opts
            ),
        {ok, ItemStartOffset, ItemSize} ?=
            find_bundle_member(
                TargetOffset,
                BundleStartOffset + HeaderSize,
                BundleIndex,
                Opts
            ),
        maybe_nested_item(
            TargetOffset,
            ItemStartOffset,
            ItemSize,
            KnownOffset,
            KnownChunk,
            Opts
        )
    end.

%% @doc If the containing item is itself a bundle and the offset lies in its
%% data payload, recurse into its bundle header. Otherwise return the item.
maybe_nested_item(
        TargetOffset,
        ItemStartOffset,
        ItemSize,
        KnownOffset,
        KnownChunk,
        Opts
    ) ->
    maybe
        {ok, FirstChunk} ?=
            item_chunk(ItemStartOffset, KnownOffset, KnownChunk, Opts),
        {ok, HeaderChunk} ?=
            header_chunk(ItemStartOffset, ItemSize, FirstChunk, Opts),
        maybe_nested_item(
            TargetOffset,
            ItemStartOffset,
            ItemSize,
            HeaderChunk,
            KnownOffset,
            KnownChunk,
            Opts
        )
    end.

maybe_nested_item(
        TargetOffset,
        ItemStartOffset,
        ItemSize,
        FirstChunk,
        KnownOffset,
        KnownChunk,
        Opts
    ) ->
    maybe
        {ok, HeaderSize, HeaderTX} ?= deserialize_header(FirstChunk),
        true ?= TargetOffset >= ItemStartOffset + HeaderSize,
        true ?= ar_tx:type(HeaderTX) =/= binary,
        {ok, _, _, _} ?=
            message_from_offset(
                TargetOffset,
                ItemStartOffset + HeaderSize,
                KnownOffset,
                KnownChunk,
                Opts
            )
    else
        false -> {ok, ItemStartOffset, ItemSize, FirstChunk};
        {error, not_found} -> {ok, ItemStartOffset, ItemSize, FirstChunk};
        Error -> Error
    end.

%% @doc Reuse the first chunk we already have when the located item starts at the
%% same offset as the original request, otherwise fetch the item's first chunk.
item_chunk(ItemStartOffset, ItemStartOffset, FirstChunk, _Opts) ->
    {ok, FirstChunk};
item_chunk(ItemStartOffset, _KnownOffset, _KnownChunk, Opts) ->
    case chunk_from_offset(ItemStartOffset, Opts) of
        {ok, _ChunkJSON, FirstChunk} -> {ok, FirstChunk};
        Error -> Error
    end.

%% @doc Read on into the next chunk when an item runs past its first chunk and
%% its header does not decode from that chunk alone.
header_chunk(ItemStartOffset, ItemSize, FirstChunk, Opts)
        when byte_size(FirstChunk) < ItemSize ->
    case deserialize_header(FirstChunk) of
        {ok, _HeaderSize, _HeaderTX} ->
            {ok, FirstChunk};
        {error, _} ->
            maybe
                {ok, _ChunkJSON, NextChunk} ?=
                    chunk_from_offset(
                        ItemStartOffset + byte_size(FirstChunk),
                        Opts
                    ),
                {ok, <<FirstChunk/binary, NextChunk/binary>>}
            end
    end;
header_chunk(_ItemStartOffset, _ItemSize, FirstChunk, _Opts) ->
    {ok, FirstChunk}.

%% @doc Locate the bundle member containing the given offset.
find_bundle_member(TargetOffset, ItemStartOffset, _BundleIndex, Opts)
        when TargetOffset < ItemStartOffset ->
    ?event(
        arweave_offset_lookup,
        {bundle_offset_search_exceeded_bounds,
            {target_offset, TargetOffset},
            {item_start_offset, ItemStartOffset}
        },
        Opts
    ),
    {error, not_found};
find_bundle_member(TargetOffset, ItemStartOffset, [{ID, Size} | _], Opts)
        when TargetOffset < ItemStartOffset + Size ->
    % The target offset is within the current bundle member.
    ?event(
        arweave_offset_lookup,
        {resolved_bundle_member, {id, ID}, {size, Size}},
        Opts
    ),
    {ok, ItemStartOffset, Size};
find_bundle_member(TargetOffset, ItemStartOffset, [{_ID, Size} | Rest], Opts) ->
    find_bundle_member(TargetOffset, ItemStartOffset + Size, Rest, Opts);
find_bundle_member(_TargetOffset, _ItemStartOffset, [], _Opts) ->
    {error, not_found}.

%%% Tests

parse_offset_test() ->
    ?assertEqual({ok, 160399272861859, undefined}, parse(<<"160399272861859">>)),
    ?assertEqual({ok, 160399272861859, 498852}, parse(<<"160399272861859-498852">>)),
    ?assertEqual({ok, 160399273000000, undefined}, parse(<<"160399273000000">>)),
    ?assertEqual({ok, 160399273000000, 498852}, parse(<<"160399273000000-498852">>)),
    ?assertEqual({ok, 160399273000000, undefined}, parse(<<"160399273m">>)),
    ?assertEqual({ok, 160399273000000, 498852}, parse(<<"160399273m-498852">>)),
    ?assertEqual(
        {ok, 1337 * 1024 * 1024 * 1024 * 1024, undefined},
        parse(<<"1337tib">>)
    ),
    ?assertEqual(error, parse(<<"65t3aueqg2rkyk7c7csh7g3262xwkz5v5d7xtzit3rlpxjaa6ezq">>)),
    ok.

%% @doc A 1 KB item whose first 32 bytes claim a huge bundle item count must
%% not drive an unbounded header read: the bytes before the target offset
%% bound the header, so the lookup gives `not_found` without fetching any
%% chunk past it.
offset_header_bound_test() ->
    ItemSize = 1000,
    AbsEnd = 1000000,
    % The first 32 bytes claim 50,000 items, a 3 MB header, from a 1 KB item.
    Item = <<50000:256/little, 0:((ItemSize - 32) * 8)>>,
    % The chunk has the proofs that an Arweave node gives: the item is the
    % data of a transaction, the only one in its block.
    {Root, Tree} =
        ar_merkle:generate_tree([{ar_tx:generate_chunk_id(Item), ItemSize}]),
    {TXRoot, TXTree} = ar_merkle:generate_tree([{Root, ItemSize}]),
    Chunk =
        hb_json:encode(
            #{
                <<"chunk">> => hb_util:encode(Item),
                <<"chunk_size">> => ItemSize,
                <<"absolute_end_offset">> => AbsEnd,
                <<"data_path">> =>
                    hb_util:encode(ar_merkle:generate_path(Root, 0, Tree)),
                <<"tx_path">> =>
                    hb_util:encode(ar_merkle:generate_path(TXRoot, 0, TXTree))
            }
        ),
    {ok, Gateway, Handle} =
        hb_mock_server:start(
            [{"/chunk/[...]", chunk, fun(_Req) -> {200, Chunk} end}]
        ),
    Opts =
        #{
            <<"routes">> =>
                [
                    #{
                        <<"template">> => <<"/arweave">>,
                        <<"node">> =>
                            #{
                                <<"match">> => <<"^/arweave">>,
                                <<"with">> => Gateway,
                                <<"opts">> =>
                                    #{
                                        <<"http-client">> => httpc,
                                        <<"protocol">> => http2
                                    }
                            }
                    }
                ]
        },
    Res =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            hb_util:bin(AbsEnd - 1),
            Opts
        ),
    Fetches = length(hb_mock_server:get_requests(Handle, chunk)),
    hb_mock_server:stop(Handle),
    % The item's chunk is read to find its start and its header; a bounded
    % read stops there, while an unbounded one fetches the claimed 3 MB.
    ?assert(Fetches =< 2),
    ?assertEqual({error, not_found}, Res).

offset_item_cases_test() ->
    Opts = #{},
    Png = #{ <<"content-type">> => <<"image/png">> },
    Jpeg = #{ <<"content-type">> => <<"image/jpeg">> },
    %% Each case fetches a live item from arweave.net; running the six
    %% cases in parallel cuts the wall time to roughly the slowest fetch.
    Cases =
        [
            %% A simple message.
            {<<"160399272861859">>, 498852, Png},
            %% A reference with a given length.
            {<<"160399272861859-498852">>, 498852, Png},
            %% A reference to a byte in the middle of the test message.
            {<<"160399273000000">>, 498852, Png},
            %% A megabyte reference to the item, occurring in the middle.
            {<<"160399273m">>, 498852, Png},
            {<<"384600234780716">>, 856691, Jpeg},
            %% An item whose header runs past the end of its first chunk.
            {<<"376836294221800">>, 480, #{}}
        ],
    hb_pmap:parallel_map(
        Cases,
        fun({Path, DataSize, Tags}) ->
            assert_offset_item(Path, DataSize, Tags, Opts)
        end,
        length(Cases)
    ),
    ok.

offset_nested_item_test() ->
    Opts = #{},
    TXID = <<"bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    Node = hb_http_server:start_node(),
    {ok, Expected} =
        hb_http:get(
            Node,
            <<"/~arweave@2.9/tx=", TXID/binary, "/1/2">>,
            Opts
        ),
    {ItemStartOffset, _ItemSize} =
        bundle_message_offset_from_tx(TXID, [1, 2], Opts),
    assert_offset_matches(hb_util:bin(ItemStartOffset + 1), Expected, Opts),
    % The last byte of item 1's bundle index gives item 1.
    {FirstMemberOffset, _} = bundle_message_offset_from_tx(TXID, [1, 1], Opts),
    {ok, Bundle} =
        hb_http:get(Node, <<"/~arweave@2.9/tx=", TXID/binary, "/1">>, Opts),
    assert_offset_matches(hb_util:bin(FirstMemberOffset - 1), Bundle, Opts).

assert_offset_item(Path, DataSize, Tags, Opts) ->
    {ok, Item} = hb_ao:resolve(#{ <<"device">> => <<"arweave@2.9">> }, Path, Opts),
    TX = hb_message:convert(Item, <<"ans104@1.0">>, <<"structured@1.0">>, Opts),
    ?assert(hb_message:verify(Item, all, Opts)),
    ?assertEqual(DataSize, TX#tx.data_size),
    ?assertEqual(DataSize, byte_size(TX#tx.data)),
    maps:foreach(
        fun(Key, Value) ->
            ?assertEqual({ok, Value}, hb_maps:find(Key, Item, Opts))
        end,
        Tags
    ),
    ok.

assert_offset_matches(Path, Expected, Opts) ->
    {ok, Item} = hb_ao:resolve(#{ <<"device">> => <<"arweave@2.9">> }, Path, Opts),
    ExpectedTX =
        hb_message:convert(
            Expected,
            <<"ans104@1.0">>,
            <<"structured@1.0">>,
            Opts
        ),
    TX = hb_message:convert(Item, <<"ans104@1.0">>, <<"structured@1.0">>, Opts),
    ?assert(hb_message:verify(Item, all, Opts)),
    ?assertEqual(
        hb_message:id(Expected, hb_message:signers(Item, Opts), Opts),
        hb_message:id(Item, signed, Opts)
    ),
    ?assertEqual(ExpectedTX#tx.data_size, TX#tx.data_size),
    ok.

bundle_message_offset_from_tx(TXID, Path, Opts) ->
    {ok, #{ <<"body">> := OffsetBody }} =
        hb_http:request(
            #{
                <<"path">> => <<"/arweave/tx/", TXID/binary, "/offset">>,
                <<"method">> => <<"GET">>
            },
            Opts
        ),
    OffsetMsg = hb_json:decode(OffsetBody),
    EndOffset = hb_util:int(maps:get(<<"offset">>, OffsetMsg)),
    Size = hb_util:int(maps:get(<<"size">>, OffsetMsg)),
    bundled_index_offset(EndOffset - Size, Path, Opts).

bundled_index_offset(BundleStartOffset, [Index], Opts) ->
    {ok, HeaderSize, BundleIndex} =
        lib_arweave_common:bundle_header(
            BundleStartOffset,
            Opts
        ),
    nth_bundle_item(Index, BundleStartOffset + HeaderSize, BundleIndex);
bundled_index_offset(BundleStartOffset, [Index | Rest], Opts) ->
    {ItemStartOffset, _ItemSize} =
        bundled_index_offset(BundleStartOffset, [Index], Opts),
    {ok, _ChunkJSON, FirstChunk} = chunk_from_offset(ItemStartOffset, Opts),
    {ok, HeaderSize, _HeaderTX} = deserialize_header(FirstChunk),
    bundled_index_offset(ItemStartOffset + HeaderSize, Rest, Opts).

nth_bundle_item(1, ItemStartOffset, [{_ID, Size} | _]) ->
    {ItemStartOffset, Size};
nth_bundle_item(Index, ItemStartOffset, [{_ID, Size} | Rest]) when Index > 1 ->
    nth_bundle_item(Index - 1, ItemStartOffset + Size, Rest).

offset_as_name_resolver_lookup_test() ->
    Opts = #{
        <<"name-resolvers">> => [#{ <<"device">> => <<"arweave@2.9">> }],
        <<"on">> =>
            #{
                <<"request">> => [#{ <<"device">> => <<"name@1.0">> }]
            }
    },
    Node = hb_http_server:start_node(Opts),
    {ok, Item} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/">>,
                <<"host">> => <<"152974576623958.localhost">>
            },
            Opts
        ),
    ?assertEqual(<<"application/json">>, hb_ao:get(<<"content-type">>, Item, Opts)).
