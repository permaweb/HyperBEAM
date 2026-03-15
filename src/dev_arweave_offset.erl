%%% @doc A module for the Arweave device that implements the default key 
%%% resolution logic. The default key returns slices of bytes inside Arweave as
%%% message representations.
-module(dev_arweave_offset).
-export([get/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Resolve either a message at an Arweave offset, or a direct key from the
%% base message if the key is not an integer.
get(Key, Base, Request, Opts) ->
    case parse(Key) of
        {ok, StartOffset, Length} ->
            load_item_at_offset(StartOffset, Length, Opts);
        error ->
            dev_message:get(Key, Base, Request, Opts)
    end.

%% @doc Parse a path key as a global Arweave start offset.
parse(Key) ->
    try
        case binary:split(Key, <<"-">>) of
            [Start, Length] ->
                {ok, hb_util:int(Start), hb_util:int(Length)};
            [Start] ->
                {ok, hb_util:int(Start), undefined}
        end
    catch
        _:_ -> error
    end.

%% @doc Load an ANS-104 item whose header begins at the given global offset.
load_item_at_offset(StartOffset, Length, Opts) ->
    maybe
        {ok, ChunkJSON, FirstChunk} ?= item_chunk_from_offset(StartOffset, Opts),
        {ok, HeaderSize, HeaderTX} ?= 
            try ar_bundles:deserialize_header(FirstChunk)
            catch _:_ -> {error, invalid_ans104_header}
            end,
        {ok, DataSize} ?=
            if Length =/= undefined -> {ok, Length};
            true ->
                case item_size_from_offset(StartOffset, ChunkJSON, Opts) of
                    {ok, ItemSize} when HeaderSize =< ItemSize ->
                        {ok, ItemSize - HeaderSize};
                    {ok, _ItemSize} -> false;
                    ItemSizeError -> ItemSizeError
                end
            end,
        {HeaderData, RemainingLength} =
            split_header_data(HeaderTX#tx.data, DataSize),
        {ok, RemainingData} ?=
            read_remaining_item_data(
                StartOffset,
                HeaderSize,
                byte_size(HeaderData),
                RemainingLength,
                Opts
            ),
        FullTX =
            HeaderTX#tx{
                data = <<HeaderData/binary, RemainingData/binary>>,
                data_size = DataSize
            },
        {ok,
            hb_message:convert(
                FullTX,
                <<"structured@1.0">>,
                <<"ans104@1.0">>,
                Opts
            )}
    else
        false -> {error, invalid_item_size};
        Error -> Error
    end.

%% @doc Read the chunk containing the given offset and trim it to begin at the
%% first byte of the requested item.
item_chunk_from_offset(StartOffset, Opts) ->
    case dev_arweave:get_chunk(StartOffset + 1, Opts) of
        {ok, ChunkJSON} ->
            ChunkSize = hb_util:int(maps:get(<<"chunk_size">>, ChunkJSON)),
            AbsEnd = hb_util:int(maps:get(<<"absolute_end_offset">>, ChunkJSON)),
            Chunk = hb_util:decode(maps:get(<<"chunk">>, ChunkJSON)),
            ChunkStart = AbsEnd - ChunkSize + 1,
            Skip = (StartOffset + 1) - ChunkStart,
            {ok, ChunkJSON, binary:part(Chunk, Skip, byte_size(Chunk) - Skip)};
        Error ->
            Error
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
read_remaining_item_data(_StartOffset, _HeaderSize, _PrefixSize, 0, _Opts) ->
    {ok, <<>>};
read_remaining_item_data(StartOffset, HeaderSize, PrefixSize, Length, Opts) ->
    hb_store_arweave:read_chunks(StartOffset + HeaderSize + PrefixSize, Length, Opts).

%% @doc Determine the size of the item at an offset by locating it in the parent
%% Arweave transaction's bundle header. In order to do this we must:
%% 1. Find the global offset of the data root of the chunk.
%% 2. Jump to that location and read the header chunks until we find our item.
%% 3. Extract the item's size from the bundle header and return it.
%% We achieve objective (1) by extracting the `absolute_end_offset` from the
%% chunk JSON and subtracting the `data_path`'s note from it. The `data_path`
%% is the Merkle path of the chunk that contains the item, and its note is the
%% offset of the end of the chunk inside the bundle. The `absolute_end_offset`
%% is the global offset of the end of the chunk, so to calculate the bundle's
%% start offset we can simply perform `absolute_end_offset - data_path_note`.
item_size_from_offset(StartOffset, ChunkJSON, Opts) ->
    AbsEnd = hb_util:int(maps:get(<<"absolute_end_offset">>, ChunkJSON)),
    ChunkEndInBundle =
        ar_merkle:extract_note(
            hb_util:decode(maps:get(<<"data_path">>, ChunkJSON))
        ),
    BundleStartOffset = AbsEnd - ChunkEndInBundle,
    case dev_arweave:bundle_header(BundleStartOffset, Opts) of
        {ok, HeaderSize, BundleIndex} ->
            locate_bundle_item(
                StartOffset,
                BundleStartOffset + HeaderSize,
                BundleIndex
            );
        Error ->
            Error
    end.

%% @doc Locate the item that starts at the given offset in a bundle header
%% index and return its serialized size.
locate_bundle_item(StartOffset, ItemStartOffset, [{_ID, Size} | _]) 
        when StartOffset =:= ItemStartOffset ->
    {ok, Size};
locate_bundle_item(StartOffset, ItemStartOffset, [{_ID, Size} | Rest])
        when StartOffset > ItemStartOffset ->
    locate_bundle_item(StartOffset, ItemStartOffset + Size, Rest);
locate_bundle_item(_StartOffset, _ItemStartOffset, _BundleIndex) ->
    {error, not_found}.

%%% Tests

offset_item_cases_test() ->
    Opts = #{},
    assert_offset_item(
        <<"160399272861859">>,
        498852,
        #{ <<"content-type">> => <<"image/png">> },
        Opts
    ),
    assert_offset_item(
        <<"160399272861859-498852">>,
        498852,
        #{ <<"content-type">> => <<"image/png">> },
        Opts
    ),
    assert_offset_item(
        <<"384600234780716">>,
        856691,
        #{ <<"content-type">> => <<"image/jpeg">> },
        Opts
    ),
    ok.

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

offset_as_name_resolver_lookup_test() ->
    Opts = #{
        name_resolvers => [#{ <<"device">> => <<"arweave@2.9">> }],
        on =>
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
