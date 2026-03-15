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
        {ok, ItemSize} ?=
            if Length =:= undefined ->
                item_size_from_offset(StartOffset, ChunkJSON, Opts);
            true ->
                {ok, Length}
            end,
        true ?= HeaderSize =< ItemSize,
        DataSize = ItemSize - HeaderSize,
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

%% @doc Resolve the size of the item at the given offset by locating it in the
%% containing bundle header. We use the `note` attached to the Merkle leaf of
%% the `tx_path` for the chunk to find the size of the bundle that contains the
%% item. We then use the `note` attached to the Merkle leaf of the `data_path`
%% for the chunk to find the offset of the end of the chunk inside the bundle.
item_size_from_offset(StartOffset, ChunkJSON, Opts) ->
    AbsEnd = hb_util:int(maps:get(<<"absolute_end_offset">>, ChunkJSON)),
    BundleSize =
        ar_merkle:extract_note(
            hb_util:decode(maps:get(<<"tx_path">>, ChunkJSON))
        ),
    ChunkEndInBundle =
        ar_merkle:extract_note(
            hb_util:decode(maps:get(<<"data_path">>, ChunkJSON))
        ),
    BundleStartOffset = AbsEnd - ChunkEndInBundle,
    case bundle_header(BundleStartOffset, BundleSize, Opts) of
        {ok, HeaderSize, BundleIndex} ->
            locate_bundle_item(
                StartOffset,
                BundleStartOffset + HeaderSize,
                BundleIndex
            );
        Error ->
            Error
    end.

%% @doc Read and decode the containing bundle header for an item.
bundle_header(BundleStartOffset, _BundleSize, Opts) ->
    case hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => BundleStartOffset + 1
        },
        Opts
    ) of
        {ok, FirstChunk} ->
            case ar_bundles:bundle_header_size(FirstChunk) of
                invalid_bundle_header ->
                    {error, invalid_bundle_header};
                HeaderSize ->
                    case read_bundle_header(BundleStartOffset, HeaderSize, FirstChunk, Opts) of
                        {ok, HeaderBin} ->
                            case ar_bundles:decode_bundle_header(HeaderBin) of
                                {_Items, BundleIndex} ->
                                    {ok, HeaderSize, BundleIndex};
                                invalid_bundle_header ->
                                    {error, invalid_bundle_header}
                            end;
                        Error ->
                            Error
                    end
            end;
        Error ->
            Error
    end.

%% @doc Read exactly the bytes needed to decode a bundle header.
read_bundle_header(_BundleStartOffset, HeaderSize, FirstChunk, _Opts)
        when HeaderSize =< byte_size(FirstChunk) ->
    {ok, binary:part(FirstChunk, 0, HeaderSize)};
read_bundle_header(BundleStartOffset, HeaderSize, _FirstChunk, Opts) ->
    hb_store_arweave:read_chunks(BundleStartOffset, HeaderSize, Opts).

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


resolve_item_at_offset_test() ->
    StartOffset = 384600234780716,
    ExpectedID = <<"cTI07T1OrF0KZEqPmZji1VTdbeKJG7kMAVlLu7KQvyw">>,
    {ok, Item} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            hb_util:bin(StartOffset),
            #{}
        ),
    ?assert(hb_message:verify(Item, all, #{})),
    ?assertEqual(ExpectedID, hb_message:id(Item, signed, #{})).

offset_as_name_resolver_lookup_test() ->
    Opts = #{
        name_resolvers => [#{ <<"device">> => <<"arweave@2.9">> }],
        on =>
            #{
                <<"request">> => [#{ <<"device">> => <<"name@2.9">> }]
            }
    },
    Node = hb_http_server:start_node(Opts),
    {ok, Item} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/">>,
                <<"host">> => <<"384600234780716.localhost">>
            },
            Opts
        ),
    ?assertEqual(<<"image/jpeg">>, hb_ao:get(<<"content-type">>, Item, Opts)).