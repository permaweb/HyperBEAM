%%% @doc A device that provides access to Arweave network information, relayed
%%% from a designated node.
%%%
%%% The node(s) that are used to query data may be configured by altering the
%%% `/arweave` route in the node's configuration message.
%%%
%%% `blocks&weave-size=END&height=HEIGHT' seeks the compact local block index
%%% in `arweave-block-store', inclusively, with `limit=1&direction=asc' by
%%% default. A height alone starts at that height's canonical entry. Results
%%% carry `height', `weave-size', `hash' (the independent hash), and `tx-root'.
%%% `block-index&from=HEIGHT&to=HEIGHT' imports native compact ranges in
%%% descending order; `~copycat@1.0/arweave&mode=block-index' also resolves
%%% tip-relative heights. A block request with `include-block-index=true'
%%% records its compact entry alongside the fetched or cached header.
-module(dev_arweave).
-implements(<<"arweave@2.9">>).
-device_libraries([lib_arweave_common]).
-export([device_info/0]).
-export([tx/3, raw/3, chunk/3, block/3, current/3, status/3, price/3, tx_anchor/3]).
-export([pending/3, block_heights/3, blocks/3, block_index/3]).
-export([post_tx_header/2, post_tx/3, post_tx/4, post_chunk/2]).
%%% Helper functions
-export([get_chunk/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(IS_BLOCK_ID(X), (is_binary(X) andalso byte_size(X) == 64)).

%% @doc Route unknown keys through offset resolution first, then fall back to
%% the message device for direct key access. The exported helper functions are
%% not keys of the device.
device_info() ->
    #{
        excludes =>
            [
                <<"keys">>,
                <<"set">>,
                <<"set-path">>,
                <<"remove">>,
                <<"post-tx-header">>,
                <<"post-chunk">>,
                <<"get-chunk">>
            ],
        default => fun dev_arweave_offset:get/4
    }.

%% @doc Proxy the `/info' endpoint from the Arweave node.
-spec status(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {error, _}.
status(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/info">>, Opts).

%% @doc Returns the given transaction as an AO-Core message. By default, this
%% embeds the `/raw` payload. Set `exclude-data` to true to return just the
%% header.
-spec tx(
    #{ _ => _ },
    #{
        method => binary(),
        tx => binary(),
        'arweave-subject' => binary(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _}.
tx(Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"POST">> -> post_tx(Base, Request, Opts);
        <<"GET">> -> get_tx(Base, Request, Opts)
    end.

%% @doc Upload either an ans104 or an L1 transaction to Arweave.
%% Ensures that uploaded transactions are stored in the local cache after a
%% successful response has been received.
%% 
%% Note: When uploading ans104 transactions, this function will use the
%% node's default bundler. If instead you want to use this node as a bundler
%% you should use the ~bundler@1.0 device.
-spec post_tx(
    #{ _ => _ },
    #{ 'arweave-subject' => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _}.
post_tx(Base, RawRequest, Opts) ->
    {ok, Request} = extract_subject(Base, RawRequest, Opts),
    case hb_maps:find(<<"commitment-device">>, Request, Opts) of
        {ok, Device} ->
            post_tx(Base, Request, Opts, Device);
        error ->
            post_tx_detect_device(Base, Request, Opts)
    end.

%% @doc Detect the commitment device to use when posting a transaction.
post_tx_detect_device(Base, Request, Opts) ->
    case hb_message:commitment_devices(Request, Opts) of
        [Device] -> post_tx(Base, Request, Opts, Device);
        [] -> 
            ?event(warning,
                {no_commitment_devices,
                    {request, Request},
                    {base, Base}
                }
            ),
            {error, <<"No commitment found on `POST tx` request.">>};
        Devices ->
            ?event(error, {too_many_commitment_devices, Devices}),
            {error, too_many_commitment_devices}
    end.

%% @doc Extract the message to post from the request or base message, by the
%% `arweave-subject' key of the request.
extract_subject(Base, Request, Opts) ->
    case hb_maps:get(<<"arweave-subject">>, Request, <<"request">>, Opts) of
        <<"request">> ->
            {ok, Request};
        <<"base">> ->
            {ok, Base};
        <<"base:", BaseTarget/binary>> ->
            hb_maps:find(BaseTarget, Base, Opts);
        <<"request:", RequestTarget/binary>> ->
            hb_maps:find(RequestTarget, Request, Opts);
        _ ->
            not_found
    end.

%% @doc Handle dispatch of Arweave base-layer TX records or ANS-104 nested 
%% transactions. Both are expected in their `structured@1.0` forms as input and
%% converted to their commitment codecs during their dispatch flows.
post_tx(_Base, Request, Opts, <<"tx@1.0">>) ->
    TX = hb_message:convert(Request, <<"tx@1.0">>, Opts),
    Res = post_tx_header(TX, Opts),
    case Res of
        {ok, _} ->
            CacheRes = hb_cache:write(Request, Opts),
            case CacheRes of
                {ok, _} ->
                    ?event(debug_arweave, {tx_cached, {msg, Request}, {status, ok}});
                _ ->
                    ?event(error, {tx_failed_to_cache, {msg, Request}, CacheRes})
            end;
        _ ->
            ok
    end,
    Res;
post_tx(_Base, Request, Opts, <<"ans104@1.0">>) ->
    hb_http:post(
        hb_opts:get(bundler_ans104, not_found, Opts),
        #{
            <<"path">> => <<"/~bundler@1.0/tx">>,
            <<"bundler-subject">> => <<"body">>,
            <<"body">> => Request
        },
        Opts
    );
post_tx(_Base, _Request, _Opts, Device) ->
    ?event(error, {unsupported_commitment_device, Device}),
    {error, <<"Unsupported commitment device on `POST tx` request.">>}.

post_tx_header(TX, Opts) ->
    JSON = ar_tx:tx_to_json_struct(TX#tx{ data = <<>> }),
    Serialized = hb_json:encode(JSON),
    LogExtra = [
        {codec, <<"tx@1.0">>},
        {id, {explicit, hb_util:human_id(TX#tx.id)}}
    ],
    request(
        <<"POST">>,
        <<"/tx">>,
        #{ <<"body">> => Serialized },
        LogExtra,
        Opts
    ).

%% @doc Get a transaction from the Arweave node, as indicated by the
%% `tx` key in the request or base message. By default, this embeds the data
%% payload. Set `exclude_data` to true to return just the header.
get_tx(Base, Request, Opts) ->
    case find_key(<<"tx">>, Base, Request, Opts) of
        not_found -> {error, not_found};
        TXID ->
            request(
                <<"GET">>,
                <<"/tx/", TXID/binary>>,
                #{},
                [],
                #{ <<"exclude-data">> => hb_util:bool(
                    find_key(<<"exclude-data">>, Base, Request, Opts)) },
                Opts
            )
    end.

%% @doc A router for range requests by method. Both `HEAD` and `GET` requests
%% are supported.
-spec raw(
    #{ raw => binary(), _ => _ },
    #{ method => binary(), raw => binary(), range => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, binary() | #{ _ => _ }} | {error, _}.
raw(Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"HEAD">> -> head_raw(Base, Request, Opts);
        <<"GET">> -> get_raw(Base, Request, Opts)
    end.

%% @doc Handle `HEAD /raw=ID` requests by reading the header chunk and
%% returning the `content-type` of the item, if found.
head_raw(Base, Request, Opts) ->
    ?event(debug_raw, {raw, {base, Base}, {request, Request}}),
    case find_key(<<"raw">>, Base, Request, Opts) of
        TXID when ?IS_ID(TXID) ->
            % Locate the item with the offsets this node holds.
            IndexStore = hb_store_arweave:store_from_opts(Opts),
            case hb_store_arweave:read_index_offset(IndexStore, TXID) of
                {ok,
                    #{
                        <<"codec">> := Codec,
                        <<"start">> := StartOffset,
                        <<"length">> := Length
                    }} ->
                        CodecFun =
                            case Codec of
                                <<"ans104@1.0">> -> fun head_raw_ans104/4;
                                <<"tx@1.0">> -> fun head_raw_tx/4;
                                _ -> throw({'invalid-codec', Codec})
                            end,
                        CodecFun(TXID, StartOffset, Length, Opts);
                not_found ->
                    ?event(
                        arweave,
                        {raw_head_offset_failed, {id, TXID}},
                        Opts
                    ),
                    {error, not_found}
            end;
        _ -> 
            {error, not_found}
    end.

%% @doc Arweave transaction headers are not part of the Arweave data tree, and
%% thus we do not add their header bytes to the offset in order to read their
%% data.
head_raw_tx(TXID, relative, Length, Opts) ->
    case pending(
        #{ <<"pending">> => TXID },
        #{ <<"exclude-data">> => true },
        Opts
    ) of
        {ok, StructuredTXHeader} ->
            head_raw_tx_response(
                TXID,
                relative,
                hb_util:int(
                    hb_maps:get(
                        <<"data-size">>,
                        StructuredTXHeader,
                        Length,
                        Opts
                    )
                ),
                StructuredTXHeader,
                Opts
            );
        _ ->
            head_raw_confirmed_tx(TXID, relative, Length, Opts)
    end;
head_raw_tx(TXID, StartOffset, Length, Opts) ->
    head_raw_confirmed_tx(TXID, StartOffset, Length, Opts).

head_raw_confirmed_tx(TXID, StartOffset, Length, Opts) ->
    {ok, StructuredTXHeader} =
        get_tx(
            #{ <<"tx">> => TXID },
            #{ <<"exclude-data">> => true },
            Opts
        ),
    head_raw_tx_response(TXID, StartOffset, Length, StructuredTXHeader, Opts).

head_raw_tx_response(TXID, StartOffset, Length, StructuredTXHeader, Opts) ->
    ContentType =
        hb_maps:get(
            <<"content-type">>,
            StructuredTXHeader,
            <<"application/octet-stream">>,
            Opts
        ),
    {ok,
        #{
            <<"raw-id">> => TXID,
            <<"offset">> => StartOffset,
            <<"data-offset">> => pending_root_offset(TXID, StartOffset),
            <<"content-type">> => ContentType,
            <<"header-length">> => 0,
            <<"data-length">> => Length,
            <<"accept-ranges">> => <<"bytes">>
        }
    }.

%% @doc ANS-104 headers are stored as part of the global Arweave data tree, so
%% so to read the data associated with their IDs, we must first read the header
%% chunk, deserialize it, and offset our data read from its starting offset.
head_raw_ans104(TXID, ArweaveOffset, Length, Opts) ->
    ?event(debug_raw, {head_raw_ans104, {txid, TXID}, {arweave_offset, ArweaveOffset}, {length, Length}}),
    case hb_store_arweave:read_chunks(
        ArweaveOffset, min(Length, ?DATA_CHUNK_SIZE), Opts) of
        {ok, HeaderChunk} ->
            do_head_raw_ans104(TXID, ArweaveOffset, Length, HeaderChunk, Opts);
        {error, Error} -> {error, Error}
    end.
do_head_raw_ans104(TXID, ArweaveOffset, Length, Data, _Opts) ->
    maybe
        {ok, HeaderSize, HeaderTX} ?= deserialize_ans104_header(Data),
        % Published indexes may match only a prefix of the requested ID.
        true ?=
            ar_tx:generate_id(HeaderTX, signed) =:= hb_util:native_id(TXID)
                orelse {error, not_found},
        ContentType =
            list_find(
                <<"content-type">>,
                HeaderTX#tx.tags,
                <<"application/octet-stream">>
            ),
        {ok,
            #{
                <<"raw-id">> => TXID,
                <<"offset">> => ArweaveOffset,
                <<"data-offset">> =>
                    add_data_offset(ArweaveOffset, HeaderSize),
                <<"content-type">> => ContentType,
                <<"header-length">> => HeaderSize,
                <<"data-length">> => Length - HeaderSize,
                <<"accept-ranges">> => <<"bytes">>
            }
        }
    end.

deserialize_ans104_header(Data) ->
    try ar_bundles:deserialize_header(Data)
    catch
        throw:{invalid_ans104_tags, _Reason} ->
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Invalid ANS-104 tag encoding.">>
                }
            }
    end.

pending_root_offset(TXID, relative) ->
    #{ <<"relative">> => TXID, <<"offset">> => 0 };
pending_root_offset(_TXID, Offset) ->
    Offset.

add_data_offset(#{ <<"relative">> := TXID, <<"offset">> := Offset }, Add) ->
    #{ <<"relative">> => TXID, <<"offset">> => Offset + Add };
add_data_offset(Offset, Add) ->
    Offset + Add.

%% @doc Get raw transaction *data* and `content-type` of an Arweave message.
%% Does not deserialize the message, nor return signature information. Included
%% only for compatibility with the legacy Arweave gateway `/raw` endpoint.
get_raw(Base, Request, Opts) ->
    ?event(debug_raw, {raw, {base, Base}, {request, Request}}),
    case head_raw(Base, Request, Opts) of
        not_found -> {error, not_found};
        Err = {error, _} -> Err;
        {ok,
            Header = #{
                <<"raw-id">> := TXID,
                <<"data-offset">> := ArweaveDataOffset,
                <<"content-type">> := ContentType,
                <<"data-length">> := FullContentLength
            }
        } ->
        ?event(debug_raw, {raw_header,
            {header, Header}}),
        case parse_range_params(Request, FullContentLength, Opts) of
            Err = {error, _} -> Err;
            {ok, StartRange, EndRange} ->
                RangeLength = (EndRange - StartRange) + 1,
                {ok, Data} =
                    hb_store_arweave:read_chunks(
                        add_data_offset(ArweaveDataOffset, StartRange),
                        RangeLength,
                        Opts
                    ),
                {
                    ok,
                    Header#{
                        <<"status">> => 206,
                        <<"content-type">> => ContentType,
                        <<"content-range">> =>
                            <<
                                "bytes ",
                                (hb_util:bin(StartRange))/binary,
                                "-",
                                (hb_util:bin(EndRange))/binary,
                                "/",
                                (hb_util:bin(FullContentLength))/binary
                            >>,
                        <<"body">> => Data
                    }
                };
            false ->
                case hb_store_arweave:read_chunks(ArweaveDataOffset, FullContentLength, Opts) of
                    {ok, Data} ->
                        {ok, Header#{
                            <<"content-type">> => ContentType,
                            <<"body">> => Data
                        }};
                    Error ->
                        ?event(
                            arweave,
                            {raw_read_chunks_failed, {id, TXID}, {error, Error}},
                            Opts
                        ),
                        Error
                end
            end
    end.

%% @doc Extract the start and end of the range a request asks for from an item
%% of `Length' bytes: `bytes=A-B', `bytes=A-' (to the end) or `bytes=-N' (the
%% last `N' bytes), as RFC 9110 (section 14.1.2) defines them. The end is at
%% most the item's last byte. A range that does not parse, or that contains no
%% byte of the item, returns a 416 error.
parse_range_params(<<"bytes=", ByteDescriptor/binary>>, Length, Opts) ->
    parse_range_params(<<"bytes ", ByteDescriptor/binary>>, Length, Opts);
parse_range_params(<<"bytes ", ByteDescriptor/binary>>, Length, _Opts) ->
    [ByteRange|_] = binary:split(ByteDescriptor, <<"/">>),
    Match =
        re:run(
            ByteRange,
            <<"\\A([0-9]*)-([0-9]*)\\z">>,
            [{capture, all_but_first, binary}]
        ),
    Range =
        case Match of
            {match, [<<>>, Suffix]} when Suffix =/= <<>> ->
                {max(Length - hb_util:int(Suffix), 0), Length - 1};
            {match, [Start, <<>>]} when Start =/= <<>> ->
                {hb_util:int(Start), Length - 1};
            {match, [Start, End]} when Start =/= <<>> ->
                {hb_util:int(Start), min(hb_util:int(End), Length - 1)};
            _ ->
                invalid
        end,
    case Range of
        {First, Last} when First =< Last -> {ok, First, Last};
        _ ->
            {error,
                #{
                    <<"status">> => 416,
                    <<"content-range">> =>
                        <<"bytes */", (hb_util:bin(Length))/binary>>,
                    <<"body">> => <<"Range not satisfiable.">>
                }
            }
    end;
parse_range_params(Msg, Length, Opts) ->
    case hb_ao:resolve(Msg, <<"range">>, Opts#{ <<"hashpath">> => ignore }) of
        {ok, Str} -> parse_range_params(Str, Length, Opts);
        _ -> false
    end.

%% @doc Case-insensitively find a key in a list and return its value.
list_find(_Key, [], Default) -> Default;
list_find(Key, [{XKey, Value} | Rest], Default) ->
    NormalizedKey = hb_util:to_lower(hb_ao:normalize_key(XKey)),
    if NormalizedKey =:= Key -> Value;
    true -> list_find(Key, Rest, Default)
    end.

%% @doc Retrieve a chunk or range of bytes from an Arweave node, or post a 
%% single chunk. Notably, as well as wrapping the Arweave node's normal
%% `GET /chunk' API, it also supports additional facilities:
%% - `GET` with `length`: Returns precisely the range of bytes specified by the
%%   offset and length.
%% - `GET` with `txid`: `GET`s a chunk or range of bytes from the given offset,
%%   relative to the given transaction's data root.
-spec chunk(
    #{ _ => _ },
    #{
        method => binary(),
        offset => binary() | integer(),
        length => binary() | integer(),
        pending => binary(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, binary() | #{ _ => _ }} | {error, _}.
chunk(Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"POST">> -> post_chunk(Base, Request, Opts);
        <<"GET">> -> get_chunk(Base, Request, Opts)
    end.

%% @doc Post a single chunk to the Arweave node, via its JSON encoding
post_chunk(_Base, Request, Opts) ->
    post_chunk(Request, Opts).
post_chunk(Request, Opts) ->
    hb_http:post(
        hb_opts:get(gateway, not_found, Opts),
        #{
            <<"path">> => <<"/chunk">>,
            <<"body">> => hb_json:encode(Request)
        },
        Opts
    ).

%% @doc Retrieve a chunk or slice of bytes by its offset, either relative to the
%% global Arweave data tree, or relative to the start of a specific pending
%% transaction.
get_chunk(_Base, Request, Opts) ->
    Offset = hb_util:int(hb_maps:get(<<"offset">>, Request, 0, Opts)),
    Length = hb_util:int(hb_maps:get(<<"length">>, Request, 1, Opts)),
    MaybeRelativeTXID = hb_maps:get(<<"pending">>, Request, undefined, Opts),
    case fetch_chunk_range(Offset, Length, MaybeRelativeTXID, Opts) of
        {ok, Chunks} ->
            Data = iolist_to_binary(Chunks),
            case hb_maps:is_key(<<"length">>, Request, Opts) of
                true ->
                    {ok, binary:part(Data, 0, min(Length, byte_size(Data)))};
                false ->
                    {ok, Data}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

fetch_chunk_range(Offset, Length, undefined, Opts)
        when (Offset >= ?STRICT_DATA_SPLIT_THRESHOLD) andalso
        ((Offset + Length - 1) >= ?STRICT_DATA_SPLIT_THRESHOLD) ->
    get_chunk_range_fixed_size(Offset, (Offset + Length - 1), Opts);
fetch_chunk_range(Offset, Length, undefined, Opts)
        when (Offset < ?STRICT_DATA_SPLIT_THRESHOLD) andalso
        ((Offset + Length - 1) < ?STRICT_DATA_SPLIT_THRESHOLD) ->
    get_chunk_range_variable_size(Offset, (Offset + Length - 1), Opts);
fetch_chunk_range(_Offset, _Length, undefined, _Opts) ->
    {error, chunk_range_spans_strict_data_split_threshold};
fetch_chunk_range(Offset, Length, RelativeTXID, Opts)
        when is_binary(RelativeTXID) ->
    get_chunk_range_relative(Offset, Length, RelativeTXID, Opts).

%% @doc Post-threshold: chunks occupy fixed 256KiB buckets aligned to
%% absolute weave offsets, which need not coincide with Offset. Query at
%% DATA_CHUNK_SIZE increments and let fill_gaps iteratively cover any
%% remaining holes until the range is contiguous.
get_chunk_range_fixed_size(Offset, EndOffset, Opts) ->
    hb_prometheus:observe(
        EndOffset - Offset,
        arweave_chunk_load_requested_bytes,
        []),
    Offsets = generate_offsets(Offset, EndOffset, ?DATA_CHUNK_SIZE),
    case fetch_and_collect(Offsets, Opts) of
        {ok, ChunkInfos} -> fill_gaps(ChunkInfos, Offset, EndOffset, Opts);
        Error -> Error
    end.

%% @doc Pre-threshold: chunks can be any size <= 256KiB. First pass at
%% DATA_CHUNK_SIZE increments plus one extra candidate chunk, then
%% iteratively fill gaps until contiguous.
get_chunk_range_variable_size(Offset, EndOffset, Opts) ->
    hb_prometheus:observe(
        EndOffset - Offset,
        arweave_chunk_load_requested_bytes,
        []),
    Offsets = generate_offsets(Offset, EndOffset, ?DATA_CHUNK_SIZE),
    case fetch_and_collect(Offsets, Opts) of
        {ok, ChunkInfos} -> fill_gaps(ChunkInfos, Offset, EndOffset, Opts);
        Error -> Error
    end.

%% @doc Return a chunk or range of bytes relative to a specific, unconfirmed,
%% transaction's data root.
get_chunk_range_relative(Offset, Length, RelativeTXID, Opts) ->
    case pending(
        #{},
        #{ <<"pending">> => RelativeTXID, <<"exclude-data">> => true },
        Opts
    ) of
        {ok, PendingTX} ->
            DataSize = hb_util:int(maps:get(<<"data-size">>, PendingTX)),
            DataRoot = hb_util:decode(maps:get(<<"data-root">>, PendingTX)),
            hb_prometheus:observe(
                Length,
                arweave_chunk_load_requested_bytes,
                []
            ),
            Offsets = pending_relative_chunk_offsets(Offset, Length, DataSize),
            GETFun =
                fun(XOffset) ->
                    decode_relative_chunk(
                        XOffset,
                        DataRoot,
                        DataSize,
                        get_pending_chunk(RelativeTXID, XOffset, Opts)
                    )
                end,
            % A peer answers 404 for an offset that is not the end of a chunk,
            % so a chunk that does not end at one of `Offsets' is missing here.
            Concurrency =
                hb_opts:get(arweave_chunk_fetch_concurrency, 10, Opts),
            Found =
                [
                    ChunkInfo
                ||
                    {ok, ChunkInfo} <-
                        hb_pmap:parallel_map(Offsets, GETFun, Concurrency)
                ],
            case fill_relative_gaps(
                Found,
                max(1, Offset + 1),
                min(Offset + Length, DataSize),
                DataSize,
                GETFun,
                Opts
            ) of
                {ok, ChunkInfos} ->
                    {ok, Binaries} = assemble_chunks(ChunkInfos, Offset + 1),
                    {ok, iolist_to_binary(Binaries)};
                Error -> Error
            end;
        Error ->
            Error
    end.

%% @doc Decode a pending chunk response into a {Start, End, Binary} tuple,
%% taking the start and end from the chunk's proof. The proof must verify
%% against the data root under either rule set by which a node admits a
%% pending chunk, and the chunk must contain the byte at the offset it was
%% fetched for.
decode_relative_chunk(Offset, DataRoot, DataSize, {ok, JSON}) ->
    Chunk = hb_util:decode(maps:get(<<"chunk">>, JSON)),
    ChunkID = ar_tx:generate_chunk_id(Chunk),
    DataPath = hb_util:decode(maps:get(<<"data_path">>, JSON)),
    Validate =
        fun(Ruleset) ->
            ar_merkle:validate_path(
                DataRoot,
                Offset - 1,
                DataSize,
                DataPath,
                Ruleset
            )
        end,
    % A proof with rebased subtrees verifies only under the rebase rule set.
    Bounds =
        case Validate(strict_borders_ruleset) of
            false -> Validate(offset_rebase_support_ruleset);
            StrictBounds -> StrictBounds
        end,
    case Bounds of
        {ChunkID, Start, End}
                when End - Start =:= byte_size(Chunk),
                    Start < Offset, Offset =< End ->
            {ok, {Start + 1, End, Chunk}};
        _ ->
            {error, 'invalid-chunk-proof'}
    end;
decode_relative_chunk(_Offset, _DataRoot, _DataSize, {error, _} = Err) ->
    Err.

%% @doc Fetch pending chunks until they cover bytes `RangeStart' to `RangeEnd'.
%% A peer serves a pending chunk only at its end offset, which is the start of
%% the chunk after it. So the chunk that contains the last byte of a gap is
%% asked for at the start of the first chunk after the gap, or at the data
%% size if no chunk after the gap is known.
fill_relative_gaps(ChunkInfos, RangeStart, RangeEnd, DataSize, GETFun, Opts) ->
    Sorted = sort_chunks(ChunkInfos),
    case find_gaps(Sorted, RangeStart, RangeEnd) of
        [] ->
            {ok, [Info || Info = {Start, _, _} <- Sorted, Start =< RangeEnd]};
        Gaps ->
            Offsets =
                [
                    hd(
                        [Start - 1 || {Start, _, _} <- Sorted, Start > GapEnd]
                            ++ [DataSize]
                    )
                ||
                    {_GapStart, GapEnd} <- Gaps
                ],
            case fetch_and_collect(Offsets, GETFun, Opts) of
                {ok, NewInfos} ->
                    fill_relative_gaps(
                        Sorted ++ NewInfos,
                        RangeStart,
                        RangeEnd,
                        DataSize,
                        GETFun,
                        Opts
                    );
                Error -> Error
            end
    end.

%% @doc Iteratively detect gaps in coverage and fetch the chunk at the start
%% of each gap until the entire range [Offset, EndOffset] is covered.
fill_gaps(ChunkInfos, Offset, EndOffset, Opts) ->
    Sorted = sort_chunks(ChunkInfos),
    case find_gaps(Sorted, Offset, EndOffset) of
        [] ->
            assemble_chunks(Sorted, Offset);
        Gaps ->
            GapOffsets = [Start || {Start, _End} <- Gaps],
            ?event(debug_arweave,
                {fill_gaps, 
                    {offset, Offset},
                    {end_offset, EndOffset},
                    {chunks,
                        [
                            {Start, End, byte_size(Chunk)}
                        ||
                            {Start, End, Chunk} <- Sorted
                        ]
                    },
                    {gap_offsets, GapOffsets}
                }
            ),
            case fetch_and_collect(GapOffsets, Opts) of
                {ok, NewInfos} ->
                    ?event(debug_arweave, {fill_gaps, NewInfos}),
                    fill_gaps(
                        Sorted ++ NewInfos,
                        Offset, EndOffset, Opts
                    );
                Error -> Error
            end
    end.

%% @doc Fetch chunks at the given offsets in parallel and parse the responses
%% into {AbsoluteStartOffset, AbsoluteEndOffset, ChunkBinary} tuples.
fetch_and_collect(Offsets, Opts) ->
    fetch_and_collect(
        Offsets,
        fun(Offset) -> decode_chunk(Offset, get_chunk(Offset, Opts)) end,
        Opts
    ).
fetch_and_collect(Offsets, GETFun, Opts) ->
    Concurrency = hb_opts:get(arweave_chunk_fetch_concurrency, 10, Opts),
    collect_chunks(hb_pmap:parallel_map(Offsets, GETFun, Concurrency)).

%% @doc Generate a list of offsets from Start to End (inclusive) stepping by
%% Step bytes. Used to produce candidate query offsets at 256KiB increments.
generate_offsets(Start, End, Step) ->
    generate_offsets(Start, End, Step, []).
generate_offsets(Current, End, _Step, Acc) when Current > End ->
    Offsets = lists:reverse(Acc),
    ?event(debug_arweave, {fetch_chunk_offsets, {offsets, Offsets}}),
    Offsets;
generate_offsets(Current, End, Step, Acc) ->
    generate_offsets(Current + Step, End, Step, [Current | Acc]).

pending_relative_chunk_offsets(Offset, Length, DataSize) ->
    RangeStart = max(1, Offset + 1),
    RangeEnd = min(Offset + Length, DataSize),
    case RangeStart > RangeEnd of
        true ->
            [];
        false ->
            FirstChunk = ((RangeStart - 1) div ?DATA_CHUNK_SIZE) + 1,
            LastChunk = ((RangeEnd - 1) div ?DATA_CHUNK_SIZE) + 1,
            [min(Chunk * ?DATA_CHUNK_SIZE, DataSize) ||
                Chunk <- lists:seq(FirstChunk, LastChunk)]
    end.

%% @doc Decode a chunk response into a {Start, End, Binary} tuple, if the chunk
%% contains the byte at the offset it was fetched for.
%% Runs inside the pmap worker so raw JSON is GC'd per-worker.
decode_chunk(Offset, {ok, JSON}) ->
    AbsEnd = hb_util:int(maps:get(<<"absolute_end_offset">>, JSON)),
    {AbsStart, _AbsEnd, Chunk} = ChunkTuple = decode_chunk_tuple(JSON, AbsEnd),
    ?event(debug_arweave,
        {decode_chunk,
            {abs_start, AbsStart},
            {abs_end, AbsEnd},
            {size, byte_size(Chunk)}}),
    case AbsStart =< Offset andalso Offset =< AbsEnd of
        true -> {ok, ChunkTuple};
        false -> {error, 'chunk-not-at-offset'}
    end;
decode_chunk(_Offset, {error, _} = Err) ->
    Err.

decode_chunk_tuple(JSON, ChunkEnd) ->
    Chunk = hb_util:decode(maps:get(<<"chunk">>, JSON)),
    {ChunkEnd - byte_size(Chunk) + 1, ChunkEnd, Chunk}.

%% @doc Collect decoded chunk results. Fails fast on the first error.
collect_chunks(Results) ->
    collect_chunks(Results, []).

collect_chunks([], Acc) ->
    {ok, lists:reverse(Acc)};
collect_chunks([{ok, ChunkInfo} | Rest], Acc) ->
    collect_chunks(Rest, [ChunkInfo | Acc]);
collect_chunks([{error, Reason} | _], _Acc) ->
    {error, Reason}.

%% @doc Sort chunk infos by start offset. If duplicate starts appear, log a
%% warning since this should not happen.
sort_chunks(ChunkInfos) ->
    Sorted = lists:sort(
        fun({StartA, EndA, _}, {StartB, EndB, _}) ->
            case StartA =:= StartB of
                true ->
                    % This should never happen. Logging rather than ignoring
                    % "just in case".
                    ?event(
                        warning,
                        {duplicate_chunk_start_offset,
                            {start, StartA},
                            {left_end, EndA},
                            {right_end, EndB}
                        }
                    );
                false ->
                    ok
            end,
            StartA =< StartB
        end,
        ChunkInfos
    ),
    Sorted.

%% @doc Find byte ranges within [RangeStart, RangeEnd] not covered by any
%% chunk. Returns a list of {GapStart, GapEnd} tuples.
%% WARNING: the find_gaps logic is untested in production and may not be 
%%          needed. We have yet to find an L1 TX that is chunked in such
%%          a way as to create gaps when using our naive 256KiB chunking.
find_gaps(SortedChunks, RangeStart, RangeEnd) ->
    find_gaps(SortedChunks, RangeStart, RangeEnd, []).

find_gaps([], Pos, RangeEnd, Gaps) when Pos =< RangeEnd ->
    lists:reverse([{Pos, RangeEnd} | Gaps]);
find_gaps([], _Pos, _RangeEnd, Gaps) ->
    lists:reverse(Gaps);
find_gaps([{ChunkStart, ChunkEnd, _} | Rest], Pos, RangeEnd, Gaps) ->
    NewGaps = case ChunkStart > Pos of
        true -> [{Pos, ChunkStart - 1} | Gaps];
        false -> Gaps
    end,
    find_gaps(Rest, max(Pos, ChunkEnd + 1), RangeEnd, NewGaps).

%% @doc Assemble chunk infos into a list of contiguous binaries suitable for
%% iolist_to_binary. The first chunk is sliced if it starts before Offset.
assemble_chunks(ChunkInfos, Offset) ->
    Sorted = sort_chunks(ChunkInfos),
    Binaries = lists:map(
        fun({ChunkStart, _ChunkEnd, Data}) ->
            case ChunkStart < Offset of
                true ->
                    % The first chunk may start before the requested offset;
                    % trim the leading bytes to start exactly at Offset.
                    Skip = Offset - ChunkStart,
                    ?event(debug_arweave, {assemble_chunks,
                        {skip, Skip},
                        {chunk_start, ChunkStart},
                        {offset, Offset},
                        {byte_size, byte_size(Data)},
                        {length, byte_size(Data) - Skip}
                    }),
                    binary:part(Data, Skip, byte_size(Data) - Skip);
                false ->
                    ?event(debug_arweave, {assemble_chunks,
                        {chunk_start, ChunkStart},
                        {offset, Offset},
                        {byte_size, byte_size(Data)}
                    }),
                    Data
            end
        end,
        Sorted
    ),
    {ok, Binaries}.

get_chunk(Offset, Opts) ->
    % Note: it's possible that we will need to add the x-bucket-based-offset
    % header to *some* queries. When querying L1 TX chunks from after the
    % strict data split threshold, in theory that header is needed. But I
    % haven't found a TX which requires it. However, including the header
    % when querying some *dataitems* does cause an error. So for now we will
    % leaeve the header out and continue to search for a case where it is
    % needed.
    Path = <<"/chunk/", (hb_util:bin(Offset))/binary>>,
    maybe
        {ok, JSON} ?=
            request(<<"GET">>, Path, #{ <<"route-by">> => Offset }, Opts),
        TXPath = hb_util:decode(maps:get(<<"tx_path">>, JSON)),
        TXEnd = ar_merkle:extract_note(TXPath),
        % The transaction proof supplies the data root and transaction bounds.
        % Its block root is not available on this read path.
        {ok, TXRoot} ?= ar_merkle:extract_root(TXPath),
        {DataRoot, TXStart, TXEnd} =
            ar_merkle:validate_path(TXRoot, TXEnd - 1, TXEnd, TXPath),
        validate_chunk(JSON, DataRoot, TXEnd - TXStart)
    end.

%% @doc Check the data path, chunk hash and size against the data root.
validate_chunk(JSON, DataRoot, DataSize) ->
    Chunk = hb_util:decode(maps:get(<<"chunk">>, JSON)),
    ChunkID = ar_tx:generate_chunk_id(Chunk),
    DataPath = hb_util:decode(maps:get(<<"data_path">>, JSON)),
    {ChunkEnd, Ruleset} = chunk_position(DataPath, 0, 0, basic_ruleset),
    case ar_merkle:validate_path(
        DataRoot,
        ChunkEnd - 1,
        DataSize,
        DataPath,
        Ruleset
    ) of
        {ChunkID, Start, End} when End - Start =:= byte_size(Chunk) ->
            {ok, JSON};
        _ -> {error, 'invalid-chunk-proof'}
    end.

%% @doc Recover the transaction-relative end offset, including subtree rebases.
chunk_position(<<_:32/binary, End:256>>, _Left, Shift, Ruleset) ->
    {Shift + End, Ruleset};
chunk_position(Path, Left, Shift, Ruleset) ->
    {Rebased, Branch} =
        case Path of
            <<0:256, Rest/binary>> -> {true, Rest};
            _ -> {false, Path}
        end,
    <<L:32/binary, _:32/binary, Note:256, Child/binary>> = Branch,
    {ok, ChildRoot} =
        ar_merkle:extract_root(
            case Child of
                <<0:256, ChildBranch/binary>> -> ChildBranch;
                _ -> Child
            end
        ),
    NextLeft =
        case ChildRoot of
            L -> Left;
            _ -> max(Left, Note)
        end,
    case Rebased of
        true ->
            chunk_position(
                Child,
                0,
                Shift + NextLeft,
                offset_rebase_support_ruleset
            );
        false -> chunk_position(Child, NextLeft, Shift, Ruleset)
    end.

%% @doc Retrieve (and cache) block information from Arweave. If the `block' key
%% is present, it is used to look up the associated block. If it is of Arweave
%% block hash length (43 characters), it is used as an ID. If it is parsable as
%% an integer, it is used as a block height. If it is not present, the current
%% block is used. `include-proofs' defaults to true; false omits `poa' and
%% `poa2' from the returned and cached header after fetching from Arweave.
%% A full request refetches a cached header whose proofs are absent.
-spec block(
    #{ block => binary(), _ => _ } | {id, binary()} | {height, integer()},
    #{ block => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _}.
block(Base, RawRequest, Opts) when is_map(Base) ->
    Request = RawRequest#{ <<"include-proofs">> =>
        hb_util:bool(
            hb_maps:get_first(
                [{RawRequest, <<"include-proofs">>}, {Base, <<"include-proofs">>}],
                true,
                Opts
            )
        ) },
    Block =
        hb_maps:get_first(
            [
                {Request, <<"block">>},
                {Base, <<"block">>}
            ],
            not_found,
            Opts
        ),
    Result = case Block of
        <<"current">> -> request(<<"GET">>, <<"/block/current">>, #{}, [], Request, Opts);
        not_found -> request(<<"GET">>, <<"/block/current">>, #{}, [], Request, Opts);
        ID when ?IS_BLOCK_ID(ID) -> block({id, ID}, Request, Opts);
        MaybeHeight ->
            try hb_util:int(MaybeHeight) of
                Int -> block({height, Int}, Request, Opts)
            catch
                _:_ ->
                    {
                        error,
                        <<"Invalid block reference `", MaybeHeight/binary, "`">>
                    }
            end
    end,
    case {Result, hb_util:bool(find_key(<<"include-block-index">>, Base, Request, Opts))} of
        {{ok, Header}, true} ->
            ok = dev_arweave_block_cache:index(#{
                <<"height">> => hb_maps:get(<<"height">>, Header, not_found, Opts),
                <<"weave-size">> => hb_maps:get(<<"weave-size">>, Header, not_found, Opts),
                <<"hash">> => hb_maps:get(<<"indep-hash">>, Header, not_found, Opts),
                <<"tx-root">> => hb_maps:get(<<"tx-root">>, Header, not_found, Opts)
            }, Opts);
        _ -> ok
    end,
    Result;
block({id, ID}, Req, Opts) ->
    case read_cached_block(ID, Req, Opts) of
        {ok, Block} ->
            ?event(arweave_short, {read_block_from_cache,
                {id, {explicit, ID}}
            }),
            {ok, Block};
        {error, not_found} when is_map(Req) ->
            case only_if_cached(Req, Opts) of
                true -> {error, not_found};
                false -> request(<<"GET">>, <<"/block/hash/", ID/binary>>, #{}, [], Req, Opts)
            end;
        {error, not_found} ->
            request(<<"GET">>, <<"/block/hash/", ID/binary>>, #{}, [], Req, Opts)
    end;
block({height, Height}, Req, Opts) ->
    case read_cached_block(Height, Req, Opts) of
        {ok, Block} ->
            ?event(arweave_short, {read_block_from_cache,
                {height, Height}
            }),
            {ok, Block};
        {error, not_found} when is_map(Req) ->
            case only_if_cached(Req, Opts) of
                true -> {error, not_found};
                false ->
                    request(
                        <<"GET">>,
                        <<"/block/height/",
                            (hb_util:bin(Height))/binary>>,
                        #{ <<"route-by">> => Height },
                        [],
                        Req,
                        Opts
                    )
            end;
        {error, not_found} ->
            request(
                <<"GET">>,
                <<"/block/height/",
                    (hb_util:bin(Height))/binary>>,
                #{ <<"route-by">> => Height },
                [],
                Req,
                Opts
            )
    end.

%% @doc List the block heights available in the block cache.
block_heights(_Base, _Request, Opts) ->
    dev_arweave_block_cache:heights(Opts).

%% @doc Seek the local compact block index by `weave-size' and `height'.
%% Bounds are inclusive; `limit' defaults to one and `direction' to ascending.
%% Omitting `weave-size' starts at the given height's canonical row.
blocks(_Base, Request, Opts) ->
    dev_arweave_block_cache:blocks(Request, Opts).

%% @doc Import native compact entries in descending height order. An omitted
%% `to' stops at the first indexed height; bounded runs honor `reindex'.
block_index(_Base, Request, Opts) ->
    From = hb_util:int(hb_maps:get(<<"from">>, Request, 0, Opts)),
    To = case hb_maps:get(<<"to">>, Request, undefined, Opts) of
        undefined -> undefined;
        Height -> hb_util:int(Height)
    end,
    import_block_index(Request, From, To, Opts).

%% @doc Arweave serves at most 10,000 compact entries per native range.
import_block_index(_Req, Current, To, _Opts) when is_integer(To), Current < To ->
    {ok, To};
import_block_index(_Req, Current, undefined, _Opts) when Current < 0 ->
    {ok, 0};
import_block_index(Req, Current, To, Opts) ->
    Reindex = hb_util:bool(hb_maps:get(<<"reindex">>, Req, true, Opts)),
    case {dev_arweave_block_cache:indexed(Current, Opts), To, Reindex} of
        {{ok, _}, undefined, _} -> {ok, Current};
        {{ok, _}, _, false} -> import_block_index(Req, Current - 1, To, Opts);
        _ ->
            Lower = max(max(0, Current - 9999),
                case To of undefined -> 0; _ -> To end),
            maybe
                {ok, Entries} ?= request(<<"GET">>,
                    <<"/block_index/", (hb_util:bin(Lower))/binary,
                        "/", (hb_util:bin(Current))/binary>>,
                    #{ <<"x-block-format">> => <<"1">>, <<"route-by">> => Current }, Opts),
                true ?= length(Entries) =:= Current - Lower + 1 orelse
                    {error, 'invalid-block-index-range'},
                store_block_index(Entries, Req, Current, To, Opts)
            end
    end.

%% @doc Height is inferred from the native index's descending range order.
store_block_index([], Req, Current, To, Opts) ->
    import_block_index(Req, Current, To, Opts);
store_block_index([Entry | Rest], Req, Height, To, Opts) ->
    Reindex = hb_util:bool(hb_maps:get(<<"reindex">>, Req, true, Opts)),
    case {dev_arweave_block_cache:indexed(Height, Opts), To, Reindex} of
        {{ok, _}, undefined, _} -> {ok, Height};
        {{ok, _}, _, false} -> store_block_index(Rest, Req, Height - 1, To, Opts);
        _ ->
            maybe
                ok ?= dev_arweave_block_cache:index(#{
                    <<"height">> => Height,
                    <<"weave-size">> => hb_maps:get(<<"weave_size">>, Entry, not_found, Opts),
                    <<"hash">> => hb_maps:get(<<"hash">>, Entry, not_found, Opts),
                    <<"tx-root">> => hb_maps:get(<<"tx_root">>, Entry, not_found, Opts)
                }, Opts),
                store_block_index(Rest, Req, Height - 1, To, Opts)
            end
    end.

%% @doc Bypass stored headers for refreshes or when required proofs are absent.
%% Only the request's `cache-control' applies, read as `hb_cache_control' reads
%% it: `no-cache' in the options skips the resolver's cache, not this one.
read_cached_block(Block, Req, Opts) ->
    maybe
        #{ <<"lookup">> := true } ?=
            hb_cache_control:derive_cache_settings(
                [Req],
                Opts#{ <<"cache-control">> => [] }
            ),
        {ok, Cached} ?= dev_arweave_block_cache:read(Block, Opts),
        false ?= hb_maps:get(<<"include-proofs">>, Req, true, Opts)
            andalso not hb_maps:is_key(<<"poa">>, Cached, Opts),
        {ok, block_proofs(Cached, Req, Opts)}
    else
        #{ <<"lookup">> := false } -> {error, not_found};
        true -> {error, not_found};
        Error -> Error
    end.

%% @doc Remove proof payloads without loading their cached links.
block_proofs(Block, Req, Opts) ->
    case hb_maps:get(<<"include-proofs">>, Req, true, Opts) of
        true -> Block;
        false ->
            hb_maps:without(
                [<<"poa">>, <<"poa2">>], hb_message:uncommitted(Block, Opts), Opts)
    end.

%% @doc Name the fields of a block as message keys: `-' in place of `_', as in
%% `indep-hash', in the block and in the messages nested in it, and
%% `block-signature' and `block-nonce' for the block's `signature' and `nonce'.
%% In `httpsig@1.0', `signature' is the header that carries the signatures
%% of a message and `nonce' one of their parameters.
block_keys(Block) ->
    maps:from_list(
        lists:map(
            fun({<<"signature">>, Value}) -> {<<"block-signature">>, Value};
               ({<<"nonce">>, Value}) -> {<<"block-nonce">>, Value};
               ({Key, Value}) ->
                    {hb_util_string:dash_chars(Key), dash_keys(Value)}
            end,
            maps:to_list(Block)
        )
    ).

%% @doc Put `-' in place of `_' in the keys of a message nested in a block.
dash_keys(Msg) when is_map(Msg) ->
    maps:from_list(
        [
            {hb_util_string:dash_chars(Key), dash_keys(Value)}
        ||
            {Key, Value} <- maps:to_list(Msg)
        ]
    );
dash_keys(Value) -> Value.

%% @doc Whether a peer's block is the block its path asked for, or an error:
%% `/block/height/<n>' asks for `height' `n', `/block/hash/<id>' for
%% `indep-hash' `id', and `/block/current' for any block.
requested_block(<<"height/", Height/binary>>, Block, Opts) ->
    hb_maps:get(<<"height">>, Block, not_found, Opts) =:= hb_util:int(Height)
        orelse {error, 'invalid-block-height'};
requested_block(<<"hash/", ID/binary>>, Block, Opts) ->
    hb_maps:get(<<"indep-hash">>, Block, not_found, Opts) =:= ID
        orelse {error, 'invalid-indep-hash'};
requested_block(<<"current">>, _Block, _Opts) ->
    true.

%% @doc Return whether the request only permits cached values. Only the
%% request's `cache-control' applies, as in `read_cached_block/3'.
only_if_cached(Req, Opts) ->
    hb_maps:get(
        <<"only-if-cached">>,
        hb_cache_control:derive_cache_settings(
            [Req],
            Opts#{ <<"cache-control">> => [] }
        ),
        false,
        Opts
    ).

%% @doc Retrieve the current block information from Arweave.
-spec current(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {error, _}.
current(Base, Request, Opts) ->
    block(Base, Request#{ <<"block">> => <<"current">> }, Opts).

-spec price(
    #{ size => integer(), _ => _ },
    #{ size => integer(), _ => _ },
    #{ _ => _ }
) -> {ok, integer() | binary() | #{ _ => _ }} | {error, _}.
price(Base, Request, Opts) ->
    Size =
        hb_ao:get_first(
            [
                {Request, <<"size">>},
                {Base, <<"size">>}
            ],
            not_found,
            Opts
        ),
    case Size of
        not_found ->
            {error, not_found};
        _ ->
            request(<<"GET">>, <<"/price/", (hb_util:bin(Size))/binary>>, Opts)
    end.

-spec tx_anchor(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, binary() | #{ _ => _ }} | {error, _}.
tx_anchor(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/tx_anchor">>, Opts).

%% @doc Retrieve either a list of the pending TXIDs on the configured Arweave
%% nodes, or a specific unconfirmed transaction header by its TXID.
-spec pending(
    #{ pending => binary(), _ => _ },
    #{ pending => binary(), offset => integer(), _ => _ },
    #{ _ => _ }
) -> {ok, binary() | [binary()] | #{ _ => _ }} | {error, _}.
pending(Base, Request, Opts) ->
    case find_key(<<"pending">>, Base, Request, Opts) of
        not_found -> request(<<"GET">>, <<"/tx/pending">>, Opts);
        TXID ->
            case hb_maps:find(<<"offset">>, Request, Opts) of
                error ->
                    % Retreive a bare TX header by its TXID
                    ExcludeData =
                        hb_util:bool(
                            find_key(<<"exclude-data">>, Base, Request, Opts)),
                    request(
                        <<"GET">>,
                        <<"/unconfirmed_tx/", TXID/binary>>,
                        #{}, [], #{ <<"exclude-data">> => ExcludeData }, Opts
                    );
                {ok, RawOffset} ->
                    Offset = hb_util:int(RawOffset),
                    % Check the chunk against the verified transaction header.
                    maybe
                        {ok, TX} ?=
                            pending(
                                #{},
                                #{
                                    <<"pending">> => TXID,
                                    <<"exclude-data">> => true
                                },
                                Opts
                            ),
                        {ok, JSON} ?= get_pending_chunk(TXID, Offset, Opts),
                        {ok, _} ?=
                            decode_relative_chunk(
                                Offset,
                                hb_util:decode(maps:get(<<"data-root">>, TX)),
                                hb_util:int(maps:get(<<"data-size">>, TX)),
                                {ok, JSON}
                            ),
                        {ok, JSON}
                    end
            end
    end.

%% @doc Download an unconfirmed chunk by its offset.
get_pending_chunk(TXID, Offset, Opts) ->
    request(
        <<"GET">>,
        <<
            "/unconfirmed_chunk/",
            TXID/binary,
            "/",
            (hb_util:bin(Offset))/binary
        >>,
        Opts
    ).

%%% Internal Functions

%% @doc Find the transaction ID to retrieve from Arweave based on the request or
%% base message.
find_key(Key, Base, Request, Opts) ->
    hb_maps:get(
        Key,
        Request,
        hb_maps:get(Key, Base, not_found, Opts),
        Opts
    ).

%% @doc Make a request to the Arweave node and parse the response into an
%% AO-Core message. Most Arweave API responses are in JSON format, but without
%% a `content-type' header. Subsequently, we parse the response manually and
%% pass it back as a message. `Req' controls response conversion only; `Extra'
%% supplies the upstream HTTP fields.
request(Method, Path, Opts) ->
    request(Method, Path, #{}, [], Opts).
request(Method, Path, Extra, Opts) ->
    request(Method, Path, Extra, [], Opts).
request(Method, Path, Extra, LogExtra, Opts) ->
    request(Method, Path, Extra, LogExtra, #{}, Opts).
request(Method, Path, Extra, LogExtra, Req, Opts) ->
    ?event(debug_arweave, {request,
        {method, Method}, {path, {explicit, Path}}, {log_extra, LogExtra}}),
    Res =
        hb_http:request(
            Extra#{
                <<"path">> => <<"/arweave", Path/binary>>,
                <<"method">> => Method
            },
            Opts#{
                <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
            }
        ),
    to_message(Path, Method, best_response(Res), LogExtra, Req, Opts).

%% @doc Select the best response from a list of responses by sorting them
%% ascending by HTTP status code. Returns the first (best) response tuple.
best_response({error, {no_viable_responses, Responses}}) ->
    best_response(Responses);
best_response([]) ->
    {error, no_viable_responses};
best_response(Responses) when is_list(Responses) ->
    Sorted = lists:sort(
        fun({_, ResponseA}, {_, ResponseB}) ->
            StatusA = response_status(ResponseA),
            StatusB = response_status(ResponseB),
            StatusA =< StatusB
        end,
        Responses
    ),
    hd(Sorted);
best_response(Response) ->
    Response.

response_status(Response) when is_map(Response) ->
    maps:get(<<"status">>, Response, 999);
response_status(_Response) ->
    999.

%% @doc Transform a response from the Arweave node into an AO-Core message.
to_message(Path, Method, {error, #{ <<"status">> := 404 }}, LogExtra, _Req, _Opts) ->
    event_request(Path, Method, 404, LogExtra),
    {error, not_found};
to_message(Path = <<"/tx/", _/binary>>, <<"GET">>, {error, #{ <<"status">> := 422 }}, LogExtra, _Req, _Opts) ->
    event_request(Path, <<"GET">>, 422, LogExtra),
    {failure, <<"Arweave peer could not process the request.">>};
to_message(Path, Method, {error, Response}, LogExtra, _Req, _Opts) when is_map(Response) ->
    Status = maps:get(<<"status">>, Response, client_error),
    event_request(Path, Method, Status, LogExtra),
    {error, Response};
to_message(Path, Method, {error, Response}, LogExtra, _Req, _Opts) ->
    event_request(Path, Method, client_error, LogExtra),
    {error, Response};
to_message(Path, Method, {failure, Response}, LogExtra, _Req, _Opts) when is_map(Response) ->
    Status = maps:get(<<"status">>, Response, server_error),
    event_request(Path, Method, Status, LogExtra),
    {error, server_error};
to_message(Path, Method, {failure, _Response}, LogExtra, _Req, _Opts) ->
    event_request(Path, Method, server_error, LogExtra),
    {error, server_error};
to_message(Path = <<"/tx">>, <<"POST">>, {ok, Response}, LogExtra, _Req, _Opts) ->
    Status = maps:get(<<"status">>, Response, 200),
    event_request(Path, <<"POST">>, Status, LogExtra),
    {ok, Response};
to_message(Path = <<"/tx/pending">>, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, _Req, _Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    {ok, hb_json:decode(Body)};
to_message(Path = <<"/block_index/", _/binary>>, <<"GET">>,
        {ok, #{ <<"body">> := Body }}, LogExtra, _Req, _Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    {ok, hb_json:decode(Body)};
to_message(Path = <<"/unconfirmed_tx/", ID/binary>>, <<"GET">>, Result, LogExtra, Req, Opts) ->
    to_tx_message(pending, ID, Path, Result, LogExtra, Req, Opts);
to_message(Path = <<"/tx/", TXID/binary>>, <<"GET">>,
        {ok, #{ <<"status">> := 202 }}, LogExtra, Req, Opts) ->
    % A node answers 202, with no header, for a TX in its mempool. The TX is
    % read as `pending' reads it, and carries status 202.
    event_request(Path, <<"GET">>, 202, LogExtra),
    maybe
        {ok, TX} ?= pending(#{ <<"pending">> => TXID }, Req, Opts),
        {ok, TX#{ <<"status">> => 202 }}
    end;
to_message(Path = <<"/tx/", TXID/binary>>, <<"GET">>, Result, LogExtra, Req, Opts) ->
    to_tx_message(tx, TXID, Path, Result, LogExtra, Req, Opts);
to_message(Path = <<"/raw/", _/binary>>, <<"GET">>, {ok, Response}, LogExtra, _Req, Opts) ->
    % A reply without a body carries empty data.
    event_request(Path, <<"GET">>, 200, LogExtra),
    {ok, hb_maps:get(<<"body">>, Response, <<>>, Opts)};
to_message(Path = <<"/block/", Ref/binary>>, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, Req, Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    Decoded =
        hb_message:convert(
            Body,
            <<"structured@1.0">>,
            % `convert/4' asks the source codec to produce TABM first; put
            % `bundle' on json@1.0 so that step does not linkify block fields.
            #{
                <<"device">> => <<"json@1.0">>,
                <<"bundle">> => true
            },
            Opts
        ),
    Block = block_proofs(block_keys(Decoded), Req, Opts),
    maybe
        true ?= requested_block(Ref, Block, Opts),
        CacheRes =
            case hb_opts:get(arweave_index_blocks, true, Opts) of
                true -> dev_arweave_block_cache:write(Block, Opts);
                false -> skipped
            end,
        ?event(
            debug_arweave_index,
            {
                if CacheRes == skipped -> skipped_caching_arweave_block;
                true -> cached_arweave_block
                end,
                {path, Path},
                {result, CacheRes}
            }
        ),
        {ok, Block}
    end;
to_message(Path = <<"/price/", _/binary>>, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, _Req, _Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    {ok, hb_util:int(Body)};
to_message(Path = <<"/tx_anchor">>, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, _Req, _Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    {ok, hb_util:decode(Body)};
to_message(Path, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, _Req, Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    % All other responses that are `OK' status are converted from JSON to an
    % AO-Core message.
    ?event(
        {arweave_json_response,
            {path, Path},
            {body_size, byte_size(Body)}
        }
    ),
    {
        ok,
        hb_message:convert(
            Body,
            <<"structured@1.0">>,
            #{
                <<"device">> => <<"json@1.0">>,
                <<"bundle">> => true
            },
            Opts
        )
    }.

%% @doc Generic handler for parsing a TX response from the Arweave node,
%% including optionally adding the data payload if appropriate.
to_tx_message(Type, ID, Path, {ok, #{ <<"body">> := Body }}, LogExtra, Req, Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    TXHeader = ar_tx:json_struct_to_tx(hb_json:decode(Body)),
    ?event(debug_arweave,
        {arweave_tx_response,
            {type, Type},
            {id, {string, ID}},
            {path, {string, Path}},
            {raw_body, {explicit, Body}},
            {body, {explicit, hb_json:decode(Body)}},
            {tx, TXHeader}
        }
    ),
    Loaded =
        case hb_maps:get(<<"exclude-data">>, Req, false, Opts) of
            true when TXHeader#tx.format =:= 1 ->
                {ok, TXHeader#tx.data};
            true -> {ok, ?DEFAULT_DATA};
            false ->
                DataRes =
                    case Type of
                        tx when TXHeader#tx.format =:= 1 ->
                            {ok, TXHeader#tx.data};
                        tx ->
                            request(<<"GET">>, <<"/raw/", ID/binary>>, Opts);
                        pending ->
                            get_chunk_range_relative(
                                0,
                                TXHeader#tx.data_size,
                                ID,
                                Opts
                            )
                    end,
                % A transaction that has data is returned only with that data:
                % a read that gives no data answers `not_found'.
                case DataRes of
                    {ok, RawData} when RawData =/= ?DEFAULT_DATA ->
                        {ok, RawData};
                    _ when TXHeader#tx.data_size =:= 0 -> {ok, ?DEFAULT_DATA};
                    {ok, _} -> {error, not_found};
                    Error -> Error
                end
        end,
    case Loaded of
        {ok, Data} -> verified_tx_message(ID, TXHeader#tx{ data = Data }, Opts);
        Other -> Other
    end.

%% @doc Return the transaction only if it has the requested ID, its signature
%% verifies, and any data it carries is the data that the signature covers.
verified_tx_message(ID, TX, Opts) ->
    case TX#tx.id =:= hb_util:native_id(ID) andalso ar_tx:verify(TX) of
        true ->
            try
                {
                    ok,
                    hb_message:convert(
                        TX,
                        <<"structured@1.0">>,
                        <<"tx@1.0">>,
                        Opts
                    )
                }
            catch
                _:{necessary_message_not_found, _, _}:_ ->
                    {error, not_found};
                _:_:_ ->
                    {
                        error,
                        #{
                            <<"status">> => 422,
                            <<"body">> =>
                                <<
                                    "Required transaction available and valid, ",
                                    "but not deserializable."
                                >>
                        }
                    }
            end;
        false ->
            {error, <<"Received invalid transaction.">>}
    end.

event_request(Path, Method, Status, Extra) ->
    BaseList = [{request, {explicit, Path}}, {method, Method}, {status, Status}],
    MergedTuple = erlang:list_to_tuple(BaseList ++ Extra),
    ?event(arweave_short, MergedTuple).

%%% Tests

%% @doc Reindexing selects the current hash and end at a height, without
%% promoting a compact entry into a complete cached header.
block_index_reindex_test() ->
    Store = hb_test_utils:test_store(hb_store_lmdb),
    Opts = #{ <<"store">> => [hb_test_utils:test_store(hb_store_volatile)],
        <<"arweave-block-store">> => Store },
    Hash = hb_util:encode(crypto:strong_rand_bytes(48)),
    Entry = #{ <<"hash">> => Hash, <<"weave_size">> => <<"100">>, <<"tx_root">> => <<>> },
    Header = #{ <<"indep-hash">> => Hash, <<"height">> => 4,
        <<"weave-size">> => 100, <<"block-size">> => 100 },
    {ok, ID} = hb_cache:write(Header, #{ <<"store">> => Store }),
    hb_cache:link(ID, Hash, #{ <<"store">> => Store }),
    hb_cache:link(ID, <<"~arweave@2.9/block/height/4">>, #{ <<"store">> => Store }),
    ?assertEqual({ok, 4}, store_block_index([Entry], #{}, 4, 4, Opts)),
    % The node does not read its block index from a store of remote scope.
    ?assertEqual({ok, []}, hb_ao:resolve(<<"~arweave@2.9/blocks&height=4">>,
        #{ <<"store">> => [Store#{ <<"scope">> => remote }] })),
    NewHash = hb_util:encode(crypto:strong_rand_bytes(48)),
    New = Entry#{ <<"hash">> := NewHash, <<"weave_size">> := <<"101">> },
    ?assertEqual({ok, 4}, store_block_index([New], #{ <<"reindex">> => false }, 4, 4, Opts)),
    {ok, [Old]} = hb_ao:resolve(<<"~arweave@2.9/blocks&height=4">>, Opts),
    ?assertEqual(Hash, hb_maps:get(<<"hash">>, Old)),
    ?assertEqual({ok, 4}, store_block_index([New], #{}, 4, 4, Opts)),
    {ok, [Updated]} = hb_ao:resolve(<<"~arweave@2.9/blocks&weave-size=0&limit=10">>, Opts),
    ?assertEqual(NewHash, hb_maps:get(<<"hash">>, Updated)),
    ?assertEqual(101, hb_maps:get(<<"weave-size">>, Updated)),
    ?assertMatch({error, #{ <<"status">> := 504 }}, hb_ao:resolve(#{
        <<"path">> => <<"~arweave@2.9/block">>, <<"block">> => 4,
        <<"include-proofs">> => false,
        <<"cache-control">> => [<<"only-if-cached">>] }, Opts)),
    % An unbounded run stops at the first completed height inside its batch.
    ?assertEqual({ok, 4}, store_block_index([New, New, Entry], #{}, 5, undefined, Opts)),
    ?assertEqual({ok, []}, hb_ao:resolve(<<"~arweave@2.9/blocks&height=3">>, Opts)),
    ?assertEqual({ok, 3}, store_block_index([New, New, Entry],
        #{ <<"reindex">> => false }, 5, 3, Opts)),
    {ok, [Resumed]} = hb_ao:resolve(<<"~arweave@2.9/blocks&height=3">>, Opts),
    ?assertEqual(Hash, hb_maps:get(<<"hash">>, Resumed)).

%% @doc Proof-free headers stay isolated, satisfy metadata queries offline,
%% and cannot satisfy a request for proofs until the full block is fetched.
include_proofs_test_() ->
    [{timeout, 60, fun() -> include_proofs(Height) end}
        || Height <- [0, 1000000, 2003806]].

%% @doc Exercise both proof representations across historical block formats.
include_proofs(Height) ->
    Stores = [hb_test_utils:test_store(hb_store_volatile)
        || _ <- lists:seq(1, 2)],
    lists:foreach(fun hb_store:start/1, Stores),
    [Ambient, Blocks] = Stores,
    Opts = #{
        <<"store">> => [Ambient],
        <<"arweave-block-store">> => [Blocks],
        <<"include-proofs">> => false,
        <<"gateway">> => <<"http://chain-3.arweave.xyz:1984">>,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
    },
    Req = #{ <<"path">> => <<"~arweave@2.9/block">>,
        <<"block">> => Height },
    CachePath = <<"~arweave@2.9/block/height/", (hb_util:bin(Height))/binary>>,
    Offline = Opts#{ <<"gateway">> => <<"http://127.0.0.1:1">>,
        <<"routes">> => [] },
    CachedReq = Req#{ <<"cache-control">> => [<<"only-if-cached">>] },
    Public = fun(Msg) ->
        hb_private:reset(hb_cache:ensure_all_loaded(Msg, Opts))
    end,
    try
        {ok, RawFull} = hb_ao:resolve(Req,
            Opts#{ <<"arweave-index-blocks">> => false }),
        Full = Public(RawFull),
        ?assert(hb_maps:is_key(<<"poa">>, Full)),
        ?assertEqual(Height =:= 2003806, hb_maps:is_key(<<"poa2">>, Full)),
        % The unsigned commitment of a header covers the keys it carries, so
        % a header with its proofs and one without differ in that commitment
        % alone.
        Expected =
            hb_message:uncommitted(
                hb_maps:without([<<"poa">>, <<"poa2">>], Full), Opts),
        Lean0 = fun(Msg) -> hb_message:uncommitted(Public(Msg), Opts) end,
        {ok, Lean} = hb_ao:resolve(
            Req#{ <<"include-proofs">> => false }, Opts),
        ?assertEqual(Expected, Lean0(Lean)),
        {ok, Stored} = hb_cache:read(
            CachePath,
            Opts#{ <<"store">> => [Blocks] }),
        ?assertEqual(Expected, Lean0(Stored)),
        ?assertEqual({error, not_found}, hb_cache:read(
            CachePath, Opts)),
        lists:foreach(fun(Reference) ->
            Read = CachedReq#{ <<"block">> => Reference },
            ?assertEqual({error, not_found}, hb_ao:resolve(Read, Offline)),
            {ok, Header} = hb_ao:resolve(
                Read#{ <<"include-proofs">> => <<"false">> }, Offline),
            ?assertEqual(Expected, Lean0(Header))
        end, [Height, hb_maps:get(<<"indep-hash">>, Full)]),
        % Explicit cache controls still apply to proof-free reads.
        ?assertEqual({error, not_found}, hb_ao:resolve(
            CachedReq#{ <<"include-proofs">> => false,
                <<"cache-control">> => [<<"no-cache">>, <<"only-if-cached">>] },
            Offline)),
        {ok, Restored} = hb_ao:resolve(Req, Opts),
        ?assertEqual(Full, Public(Restored)),
        {ok, Warm} = hb_ao:resolve(CachedReq, Offline),
        ?assertEqual(Full, Public(Warm)),
        {ok, WarmLean} = hb_ao:resolve(
            CachedReq#{ <<"include-proofs">> => false }, Offline),
        ?assertEqual(Expected, Lean0(WarmLean))
    after
        lists:foreach(fun hb_store:stop/1, Stores)
    end.

unprocessable_transaction_test() ->
    Wallet = ar_wallet:new(),
    Req = #{ <<"exclude-data">> => true },
    Opts = #{ <<"store">> => [] },
    lists:foreach(
        fun(Value) ->
            TX = ar_tx:sign(#tx{
                format = 2,
                tags = [
                    {<<"from-process">>, Value},
                    {<<"ao-types">>, <<"integer=(\"from-process\")">>}
                ]
            }, Wallet),
            ID = hb_util:human_id(TX#tx.id),
            Path = <<"/tx/", ID/binary>>,
            Response = {ok, #{ <<"body">> =>
                hb_json:encode(ar_tx:tx_to_json_struct(TX)) }},
            Result = to_tx_message(tx, ID, Path, Response, [], Req, Opts),
            case Value of
                <<"12345">> ->
                    {ok, Message} = Result,
                    ?assertEqual(12345, hb_maps:get(<<"from-process">>, Message));
                _ ->
                    ?assertMatch({error, #{ <<"status">> := 422 }}, Result),
                    ?assertEqual(
                        {error, <<"Received invalid transaction.">>},
                        to_tx_message(tx, hb_util:human_id(<<0:256>>),
                            Path, Response, [], Req, Opts)
                    )
            end
        end,
        [<<"[object Object]">>, <<"a">>, <<"1.5">>, <<"12345">>]
    ),
    ?assertMatch(
        {failure, _},
        to_message(<<"/tx/test">>, <<"GET">>,
            {error, #{ <<"status">> => 422 }}, [], Req, Opts)
    ),
    % A 202 to `GET /tx/<ID>' gives the TX as `pending' reads it, with
    % status 202.
    {ID, _Data, PeerOpts, Handle} = start_pending_peer([10], 0, Opts),
    try
        {ok, Pending} = pending(#{ <<"pending">> => ID }, Req, PeerOpts),
        ?assertEqual(
            {ok, Pending#{ <<"status">> => 202 }},
            to_message(<<"/tx/", ID/binary>>, <<"GET">>,
                {ok, #{ <<"status">> => 202, <<"body">> => <<"Pending">> }},
                [], Req, PeerOpts)
        )
    after
        hb_mock_server:stop(Handle)
    end.

%% @doc A fixed bad interior offset from a live TX is rejected by
%% bundle_header/3 as invalid_bundle_header.
bundle_header_garbage_guard_test_parallel() ->
    ServerOpts = #{ <<"store">> => [hb_test_utils:test_store()] },
    _Server = hb_http_server:start_node(ServerOpts),
    ProbeOffset = 376836336327208,
    Size = 121798901,
    ?assertEqual(
        {error, invalid_bundle_header},
        lib_arweave_common:bundle_header(ProbeOffset - 1, Size, ServerOpts)
    ).


post_ans104_message_test_parallel() ->
    Port = rand:uniform(10000) + 10000,
    ServerOpts = #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"port">> => Port,
        <<"bundler-ans104">> => iolist_to_binary(
            io_lib:format("http://localhost:~p/", [Port])
        )
    },
    Server = hb_http_server:start_node(ServerOpts),
    %% For some reason if we wait 1500ms before the request this test doesn't fail
    %% with connect_timeout when runnning in parallel.
    timer:sleep(1500),
    ClientOpts =
        #{
            <<"store">> => [hb_test_utils:test_store()],
            <<"priv-wallet">> => hb:wallet()
        },
    Msg =
        hb_message:commit(
            #{
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Process">>,
                % The item has a `target', as a message to a process does.
                <<"target">> => hb_util:human_id(crypto:strong_rand_bytes(32)),
                <<"data">> => <<"test-data">>
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"ans104@1.0">> }
        ),
    % A mistyped `arweave-subject' fails.
    ?assertMatch(
        {failure, _},
        hb_http:post(
            Server,
            Msg#{ <<"path">> => <<"/~arweave@2.9/tx?arweave-subject=requst">> },
            ClientOpts
        )
    ),
    {ok, PostRes} =
        hb_http:post(
            Server,
            Msg#{
                <<"path">> => <<"/~arweave@2.9/tx">>
            },
            ClientOpts
        ),
    ?assertMatch(#{ <<"status">> := 200 }, PostRes),
    ?event(debug_test, {post_res, PostRes}),
    SignedID = hb_message:id(Msg, signed, ClientOpts),
    {ok, GetRes} =
        hb_http:get(
            Server, <<"/", SignedID/binary>>,
            ClientOpts
        ),
    ?assertMatch(
        #{
            <<"status">> := 200,
            <<"variant">> := <<"ao.N.1">>,
            <<"type">> := <<"Process">>,
            <<"data">> := <<"test-data">>
        },
        GetRes
    ),
    ok.

post_tx_json_success_test_parallel() ->
    {Response, Node1Posts, Node2Posts} =
        post_tx_json_two_node_test({200, <<"OK-1">>}, {200, <<"OK-2">>}),
    ?assertMatch({ok, #{ <<"status">> := 200 }}, Response),
    ?assertEqual(1, length(Node1Posts)),
    ?assertEqual(1, length(Node2Posts)),
    ok.

post_tx_json_mixed_status_prefers_success_test_parallel() ->
    {Response, Node1Posts, Node2Posts} =
        post_tx_json_two_node_test(
            {400, <<"Transaction verification failed.">>},
            {200, <<"OK-2">>}
        ),
    ?assertMatch({ok, #{ <<"status">> := 200 }}, Response),
    ?assertEqual(1, length(Node1Posts)),
    ?assertEqual(1, length(Node2Posts)),
    ok.

best_response_handles_failed_connect_entries_test_parallel() ->
    FailedConnect =
        {failed_connect,
            [
                {to_address, {"tip-4.arweave.xyz", 1984}},
                {inet, [inet], etimedout}
            ]
        },
    Responses = [
        {error, FailedConnect},
        {ok, #{ <<"status">> => 200, <<"body">> => <<"OK-2">> }}
    ],
    ?assertEqual(
        {ok, #{ <<"status">> => 200, <<"body">> => <<"OK-2">> }},
        best_response(Responses)
    ).

best_response_non_map_error_round_trips_test_parallel() ->
    FailedConnect =
        {failed_connect,
            [
                {to_address, {"tip-4.arweave.xyz", 1984}},
                {inet, [inet], etimedout}
            ]
        },
    ?assertEqual(
        {error, FailedConnect},
        to_message(<<"/tx">>, <<"GET">>, {error, FailedConnect}, [], #{}, #{})
    ).

empty_raw_response_test() ->
    ?assertEqual(
        {ok, <<>>},
        to_message(
            <<"/raw/empty">>,
            <<"GET">>,
            {ok, #{ <<"status">> => 200 }},
            [],
            #{},
            #{}
        )
    ).

post_tx_json_two_node_test(Node1TxResponse, Node2TxResponse) ->
    {ok, MockNode1, MockHandle1} = hb_mock_server:start([
        {"/tx", tx, Node1TxResponse}
    ]),
    {ok, MockNode2, MockHandle2} = hb_mock_server:start([
        {"/tx", tx, Node2TxResponse}
    ]),
    Server = hb_http_server:start_node(
        post_tx_json_two_node_server_opts(MockNode1, MockNode2)
    ),
    ClientOpts = post_tx_json_client_opts(),
    try
        Response = post_tx_json_request(Server, ClientOpts),
        Node1Posts = hb_mock_server:get_requests(tx, 1, MockHandle1),
        Node2Posts = hb_mock_server:get_requests(tx, 1, MockHandle2),
        {Response, Node1Posts, Node2Posts}
    after
        hb_mock_server:stop(MockHandle1),
        hb_mock_server:stop(MockHandle2)
    end.

post_tx_json_two_node_server_opts(MockNode1, MockNode2) ->
    Routes =
        [
            #{
                <<"template">> =>
                    #{
                        <<"path">> => <<"^/arweave/tx">>,
                        <<"method">> => <<"POST">>
                    },
                <<"nodes">> =>
                    [
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"with">> => MockNode1,
                            <<"opts">> => #{ <<"http-client">> => httpc }
                        },
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"with">> => MockNode2,
                            <<"opts">> => #{ <<"http-client">> => httpc }
                        }
                    ],
                <<"parallel">> => true,
                <<"responses">> => 2,
                <<"stop-after">> => false,
                <<"admissible-status">> => 200
            }
        ],
    #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"routes">> => Routes
    }.

post_tx_json_client_opts() ->
    #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"priv-wallet">> => hb:wallet()
    }.

post_tx_json_payload(ClientOpts) ->
    Msg =
        hb_message:commit(
            #{
                <<"tag">> => <<"value">>,
                <<"data">> => <<"test-data">>
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    TX = hb_message:convert(Msg, <<"tx@1.0">>, <<"structured@1.0">>, ClientOpts),
    JSON = ar_tx:tx_to_json_struct(TX#tx{ data = <<>> }),
    hb_json:encode(JSON).

post_tx_json_request(Server, ClientOpts) ->
    Serialized = post_tx_json_payload(ClientOpts),
    hb_http:post(
        Server,
        #{
            <<"path">> => <<"/~arweave@2.9/tx">>,
            <<"content-type">> => <<"application/tx">>,
            <<"body">> => Serialized
        },
        ClientOpts
    ).

%% @doc Build isolated test opts and pre-index the blocks for the given TXIDs.
setup_arweave_index_opts(TXIDs) ->
    TestStore = hb_test_utils:test_store(hb_store_volatile, <<"arweave-index">>),
    IndexStore = #{ <<"module">> => hb_store_arweave, <<"index-store">> => [TestStore] },
    Opts = #{
        <<"store">> => [TestStore],
        <<"arweave-index-ids">> => true,
        <<"arweave-index-store">> => IndexStore
    },
    % Either: Index the blocks containing the TXs...
    % lists:foreach(
    %     fun(Block) -> ok = index_test_block(Block, Opts) end,
    %     lists:usort([tx_index_block(TXID) || TXID <- TXIDs])
    % ),
    % ...or: Index the TXs directly. This depends on the `/tx/<TXID>/offset`
    % endpoint being available in the `/arweave` routes.
    lists:foreach(
        fun(TXID) -> ok = index_test_tx(TXID, IndexStore, Opts) end,
        TXIDs
    ),
    Opts.

index_test_block(Block, Opts) ->
    BlockBin = hb_util:bin(Block),
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&from=",
                BlockBin/binary,
                "&to=",
                BlockBin/binary
            >>,
            Opts#{ <<"arweave-index-ids">> => true }
        ),
    ok.

index_test_tx(TXID, IndexStore, Opts) ->
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
    StartOffset = EndOffset - Size,
    ok =
        hb_store_arweave:write_offset(
            IndexStore,
            TXID,
            <<"tx@1.0">>,
            StartOffset,
            Size
        ),
    ?assertMatch({ok, _}, hb_store_arweave:read_offset(IndexStore, TXID, Opts)),
    ok.

tx_index_block(<<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>) -> 1749502;
tx_index_block(<<"jI0A4BASHaUdCCsdv249BxDX6IlE0Ko391TuI6REATw">>) -> 1289677;
tx_index_block(<<"4FnBmvgWmqXWEEprjVqBsV5aRpAgF6_yJX_GTGsSZjY">>) -> 753012;
tx_index_block(<<"YR9m4c3CrlljCRYEWBLeoKekbAyYZRMo2Kpz61IeNp8">>) -> 1233918.

get_tx_basic_data_test_parallel() ->
    {ok, Structured} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => <<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>,
            <<"exclude-data">> => false
        },
        #{}
    ),
    ?event(debug_test, {structured_tx, Structured}),
    ?assert(hb_message:verify(Structured, all, #{})),
    % Hash the data to make it easier to match
    StructuredWithHash = Structured#{
        <<"data">> => hb_util:encode(
            crypto:hash(sha256, (maps:get(<<"data">>, Structured)))
        )
    },
    ExpectedMsg = #{
        <<"data">> => <<"PEShWA1ER2jq7CatAPpOZ30TeLrjOSpaf_Po7_hKPo4">>,
        <<"reward">> => <<"482143296">>,
        <<"anchor">> => <<"XTzaU2_m_hRYDLiXkcleOC4zf5MVTXIeFWBOsJSRrtEZ8kM6Oz7EKLhZY7fTAvKq">>,
        <<"content-type">> => <<"application/json">>
    },
    ?assert(hb_message:match(ExpectedMsg, StructuredWithHash, only_present)),
    ok.

get_tx_format_one_data_test_parallel() ->
    {ok, TX} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => <<"U-rx7euDqM6GPl9fLTGrirZxLIihy-ZsfuIZOYHJjPk">>,
            <<"exclude-data">> => false
        },
        #{}
    ),
    ?assertEqual(17967, byte_size(hb_ao:get(<<"data">>, TX))),
    ?assert(hb_message:verify(TX, all, #{})).

%% @doc The data for this transaction ends with two smaller chunks.
get_tx_split_chunk_test_parallel() ->
    {ok, Structured} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
            <<"exclude-data">> => false
        },
        #{}
    ),
    ?assert(hb_message:verify(Structured, all, #{})),
    ?assertEqual(
        <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
        hb_message:id(Structured, signed)),
    ExpectedMsg = #{
        <<"reward">> => <<"6035386935">>,
        <<"anchor">> => <<"PX16-598IrIMvLxFkvfNTWLVKXqXSmArOdW3o7X8jWMCH1fiNOjBZ2XjQlw0FOme">>,
        <<"Contract">> => <<"KTzTXT_ANmF84fWEKHzWURD1LWd9QaFR9yfYUwH2Lxw">>
    },
    ?assert(hb_message:match(ExpectedMsg, Structured, only_present)),

    Child = hb_ao:get(<<"1/2">>, Structured),
    ?assert(hb_message:verify(Child, all, #{})),
    ?event(debug_test, {child, {explicit, hb_message:id(Child, signed)}}),
    ?assertEqual(
        <<"8aJrRWtHcJvJ61qsH6agGkemzrtLw3W22xFrpCGAnTM">>,
        hb_message:id(Child, signed)),
    % A TX whose data root is the root of chunks of 262,144 and 11,406 bytes:
    % its read keeps the TX's signed commitment.
    {ok, FixedSizeChunks} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"tx">>,
                <<"tx">> => <<"z0BVqo6FJpyWVzPLwaOYfB4TshMH4EUsC2V4ytouQc4">>,
                <<"exclude-data">> => false
            },
            #{}
        ),
    ?assertEqual(
        <<"z0BVqo6FJpyWVzPLwaOYfB4TshMH4EUsC2V4ytouQc4">>,
        hb_message:id(FixedSizeChunks, signed)
    ),
    ok.

get_tx_basic_data_exclude_data_test_parallel() ->
    TXID = <<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>,
    Opts = setup_arweave_index_opts([TXID]),
    {ok, Structured} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => TXID,
            <<"exclude-data">> => true
        },
        Opts
    ),
    ?event(debug_test, {structured_tx, Structured}),
    ?assert(hb_message:verify(Structured, all, Opts)),
    ?assertEqual(false, maps:is_key(<<"data">>, Structured)),
    ExpectedMsg = #{
        <<"reward">> => <<"482143296">>,
        <<"anchor">> => <<"XTzaU2_m_hRYDLiXkcleOC4zf5MVTXIeFWBOsJSRrtEZ8kM6Oz7EKLhZY7fTAvKq">>,
        <<"content-type">> => <<"application/json">>
    },
    ?assert(hb_message:match(ExpectedMsg, Structured, only_present)),
    {ok, RawData} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"raw">>,
            <<"raw">> => TXID
        },
        Opts
    ),
    ?event(debug_test, {raw_data, RawData}),
    Data = hb_ao:get(<<"body">>, RawData, Opts),
    StructuredWithData = Structured#{ <<"data">> => Data },
    ?assert(hb_message:verify(StructuredWithData, all, Opts)),
    DataHash = hb_util:encode(crypto:hash(sha256, Data)),
    ?assertEqual(<<"PEShWA1ER2jq7CatAPpOZ30TeLrjOSpaf_Po7_hKPo4">>, DataHash),
    ok.

get_tx_data_tag_exclude_data_test_parallel() ->
    TXID = <<"jI0A4BASHaUdCCsdv249BxDX6IlE0Ko391TuI6REATw">>,
    Opts = setup_arweave_index_opts([TXID]),
    {ok, Structured} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => TXID,
            <<"exclude-data">> => true
        },
        Opts
    ),
    ?event(debug_test, {structured_tx, Structured}),
    ?assert(hb_message:verify(Structured, all, Opts)),
    ?assertEqual(false, maps:is_key(<<"data">>, Structured)),
    ExpectedMsg = #{
        <<"reward">> => <<"630923958">>,
        <<"anchor">> => <<"CWJKkpdXEQO9sCWLFg8Cqby0d7wY0Gez5H95YG15g8pAYaXVatF9Ms1QBUpvZ-Ll">>,
        <<"content-type">> => <<"image/png">>
    },
    ?assert(hb_message:match(ExpectedMsg, Structured, only_present)),
    {ok, RawData} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"raw">>,
            <<"raw">> => TXID
        },
        Opts
    ),
    Data = hb_ao:get(<<"body">>, RawData, Opts),
    StructuredWithData = Structured#{ <<"data">> => Data },
    ?assert(hb_message:verify(StructuredWithData, all, Opts)),
    DataHash = hb_util:encode(crypto:hash(sha256, Data)),
    ?assertEqual(<<"IHyJ9BlQaHLWVwwklMwV1XEYXGjwx2B6HXNJZ4yJXeQ">>, DataHash),
    ok.

head_raw_tx_test_parallel() ->
    TXID = <<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>,
    % A TX with the tag `device: manifest@1.0'.
    DeviceTXID = <<"fQXYPE9MAcfI1wV2CwJ3sJIhgT9btBOlYFOKFDGhAs0">>,
    Opts = setup_arweave_index_opts([TXID, DeviceTXID]),
    {ok, Result} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"raw">>,
                <<"raw">> => TXID,
                <<"method">> => <<"HEAD">>
            },
            Opts
        ),
    ?event({result, Result}),
    ?assertEqual(
        {ok, <<"application/json">>},
        hb_maps:find(<<"content-type">>, Result, Opts)
    ),
    ?assertEqual(
        {ok, 774},
        hb_maps:find(<<"data-length">>, Result, Opts)
    ),
    ?assertEqual(
        {ok, 0},
        hb_maps:find(<<"header-length">>, Result, Opts)
    ),
    % The HEAD of a TX with a `device' tag gives the TX's `content-type': the
    % device does not run.
    ?assertMatch(
        {ok,
            #{
                <<"content-type">> := <<"application/x.arweave-manifest+json">>,
                <<"data-length">> := 1221326
            }
        },
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"raw">>,
                <<"raw">> => DeviceTXID,
                <<"method">> => <<"HEAD">>
            },
            Opts
        )
    ).

head_raw_ans104_test_parallel() ->
    Opts = setup_arweave_index_opts([]),
    DataItemID = <<"0vy2Ey8bWkSDcRIvWQJjxDeVGYOrTSmYIIhBILJntY8">>,
    BlockBin = hb_util:bin(1_827_942),
    hb_ao:resolve(
        <<"~copycat@1.0/arweave&from=", BlockBin/binary, "&to=", BlockBin/binary>>,
        Opts
    ),
    {ok, Result} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"raw">>,
                <<"raw">> => DataItemID,
                <<"method">> => <<"HEAD">>
            },
            Opts
        ),
    ?assertEqual(
        {ok, <<"application/json">>},
        hb_maps:find(<<"content-type">>, Result, Opts)
    ),
    ?assertEqual(
        {ok, 575},
        hb_maps:find(<<"data-length">>, Result, Opts)
    ).

%% @doc Raw reads must check the full ID of a published-index candidate.
raw_published_index_id_test_parallel() ->
    Store = hb_store_arweave:store_from_opts(hb_opts:default_message()),
    [_Primary | Published] = maps:get(<<"index-store">>, Store),
    Local = hb_test_utils:test_store(),
    IndexStore = Store#{
        <<"index-store">> => [Local | Published],
        <<"local-store">> => [Local]
    },
    Opts = #{
        <<"store">> => [Local],
        <<"arweave-index-store">> => IndexStore
    },
    ok = hb_store:start(IndexStore),
    ID = <<"AAAAhyV8_NwududSxuraAj7DLWiZHDTqVKWrZglpNok">>,
    <<Prefix:255, Last:1>> = hb_util:native_id(ID),
    WrongID = hb_util:encode(<<Prefix:255, (Last bxor 1):1>>),
    {ok, Candidate} = hb_store_arweave:read_index_offset(IndexStore, WrongID),
    ?assertEqual(381852134215637, maps:get(<<"start">>, Candidate)),
    lists:foreach(
        fun({Request, Length}) ->
            Base = #{ <<"device">> => <<"arweave@2.9">> },
            Req = Request#{ <<"path">> => <<"raw">> },
            {ok, Result} =
                hb_ao:resolve(Base, Req#{ <<"raw">> => ID }, Opts),
            ?assertEqual(ID, hb_maps:get(<<"raw-id">>, Result, Opts)),
            ?assertEqual(2827, hb_maps:get(<<"data-length">>, Result, Opts)),
            ?assertEqual(Length,
                byte_size(hb_maps:get(<<"body">>, Result, <<>>, Opts))),
            ?assertEqual(
                {error, not_found},
                hb_ao:resolve(Base, Req#{ <<"raw">> => WrongID }, Opts)
            )
        end,
        [
            {#{ <<"method">> => <<"HEAD">> }, 0},
            {#{ <<"method">> => <<"GET">> }, 2827},
            {#{
                <<"method">> => <<"GET">>,
                <<"range">> => <<"bytes 0-2/2827">>
            }, 3}
        ]
    ).

head_raw_ans104_invalid_tags_test() ->
    Tags = [{<<"Content-Type">>, <<"application/json">>}],
    EncodedTags = ar_bundles_avro:encode(Tags),
    TagsWithoutTerminator = binary:part(EncodedTags, 0, byte_size(EncodedTags) - 1),
    Body = <<"{\"$schema\":\"https://example.invalid/schema\"}">>,
    DataItem =
        <<
            1, 0,
            0:4096,
            0:4096,
            0,
            0,
            (length(Tags)):64/little,
            (byte_size(TagsWithoutTerminator)):64/little,
            TagsWithoutTerminator/binary,
            Body/binary
        >>,
    ?assertMatch(
        {error, #{ <<"status">> := 400 }},
        do_head_raw_ans104(<<0:256>>, 0, byte_size(DataItem), DataItem, #{})
    ).

get_raw_range_tx_test_parallel() ->
    DataItemID = <<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>,
    Opts = setup_arweave_index_opts([DataItemID]),
    {ok, Result} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"raw">>,
                <<"raw">> => DataItemID,
                <<"method">> => <<"GET">>,
                <<"range">> => <<"bytes 0-2/774">>
            },
            Opts
        ),
    ?event(debug_test, {result, Result}),
    ?assertEqual(
        {ok, <<"{\"d">>},
        hb_maps:find(<<"body">>, Result, Opts)
    ),
    {ok, Result2} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"raw">>,
                <<"raw">> => DataItemID,
                <<"method">> => <<"GET">>,
                <<"range">> => <<"bytes 100-105/774">>
            },
            Opts
        ),
    ?event(debug_test, {result2, Result2}),
    ?assertEqual(
        {ok, <<"application/json">>},
        hb_maps:find(<<"content-type">>, Result2, Opts)
    ),
    ?assertEqual(
        {ok, <<"ame Cr">>},
        hb_maps:find(<<"body">>, Result2, Opts)
    ).

get_raw_range_ans104_test_parallel() ->
    Opts = setup_arweave_index_opts([]),
    DataItemID = <<"0vy2Ey8bWkSDcRIvWQJjxDeVGYOrTSmYIIhBILJntY8">>,
    BlockBin = hb_util:bin(1_827_942),
    hb_ao:resolve(
        <<"~copycat@1.0/arweave&from=", BlockBin/binary, "&to=", BlockBin/binary>>,
        Opts
    ),
    {ok, Result} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"raw">>,
                <<"raw">> => DataItemID,
                <<"method">> => <<"GET">>,
                <<"range">> => <<"bytes 0-1/575">>
            },
            Opts
        ),
    ?event(debug_test, {result, Result}),
    ?assertEqual(
        {ok, <<"{\n">>},
        hb_maps:find(<<"body">>, Result, Opts)
    ),
    {ok, Result2} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"raw">>,
                <<"raw">> => DataItemID,
                <<"method">> => <<"GET">>,
                <<"range">> => <<"bytes 100-105/575">>
            },
            Opts
        ),
    ?event(debug_test, {result2, Result2}),
    ?assertEqual(
        {ok, <<"application/json">>},
        hb_maps:find(<<"content-type">>, Result2, Opts)
    ),
    ?assertEqual(
        {ok, <<"t #972">>},
        hb_maps:find(<<"body">>, Result2, Opts)
    ).

get_tx_rsa_nested_bundle_test_parallel() ->
    Node = hb_http_server:start_node(),
    Path = <<"/~arweave@2.9/tx=bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    {ok, Root} = hb_http:get(Node, Path, #{}),
    ?event(debug_test, {root, Root}),
    ?assert(hb_message:verify(Root, all, #{})),
    ChildPath = <<Path/binary, "/1/2">>,
    {ok, Child} = hb_http:get(Node, ChildPath, #{}),
    ?event(debug_test, {child, Child}),
    ?assert(hb_message:verify(Child, all, #{})),
    {ok, ExpectedChild} =
        hb_ao:resolve(
            Root,
            <<"1/2">>,
            #{}
        ),
    ?assert(hb_message:match(ExpectedChild, Child, only_present)),
    ManualChild = #{
        <<"data">> => <<"{\"totalTickedRewardsDistributed\":0,\"distributedEpochIndexes\":[],\"newDemandFactors\":[],\"newEpochIndexes\":[],\"tickedRewardDistributions\":[],\"newPruneGatewaysResults\":[{\"delegateStakeReturned\":0,\"stakeSlashed\":0,\"gatewayStakeReturned\":0,\"delegateStakeWithdrawing\":0,\"prunedGateways\":[],\"slashedGateways\":[],\"gatewayStakeWithdrawing\":0}]}">>,
        <<"data-protocol">> => <<"ao">>,
        <<"from-module">> => <<"cbn0KKrBZH7hdNkNokuXLtGryrWM--PjSTBqIzw9Kkk">>,
        <<"from-process">> => <<"agYcCFJtrMG6cqMuZfskIkFTGvUPddICmtQSBIoPdiA">>,
        <<"anchor">> => <<"MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAyODAxODg">>,
        <<"reference">> => <<"280188">>,
        <<"target">> => <<"1R5QEtX53Z_RRQJwzFWf40oXiPW2FibErT_h02pu8MU">>,
        <<"type">> => <<"Message">>,
        <<"variant">> => <<"ao.TN.1">>
    },
    ?assert(hb_message:match(ManualChild, Child, only_present)),
    ok.

%% @TODO: This test is disabled because it takes too long to run. Re-enable
%% once some performance optimizations are implemented.
get_tx_rsa_large_bundle_test_disabled() ->
    {timeout, 300, fun() ->
        Node = hb_http_server:start_node(),
        Path = <<"/~arweave@2.9/tx=VifINXnMxLwJXOjHG5uM0JssiylR8qvajjj7HlzQvZA">>,
        {ok, Root} = hb_http:get(Node, Path, #{}),
        ?event(debug_test, {root, Root}),
        ?assert(hb_message:verify(Root, all, #{})),
        ok
    end}.

get_bad_tx_test_parallel() ->
    Node = hb_http_server:start_node(),
    Path = <<"/~arweave@2.9/tx=INVALID-ID">>,
    Res = hb_http:get(Node, Path, #{}),
    ?assertEqual({error, not_found}, Res),
    % The exported helper functions are not keys of the device.
    lists:foreach(
        fun(Key) ->
            ?assertEqual(
                {error, not_found},
                hb_http:get(Node, <<"/~arweave@2.9/", Key/binary>>, #{})
            )
        end,
        [<<"post-tx-header">>, <<"get-chunk">>, <<"post-chunk">>]
    ).

%% @doc Chunk reads check the received bytes and proof before returning a range.
chunk_proof_test_() ->
    [
        {
            hb_util:list(Mode) ++ " " ++ hb_util:list(Change),
            fun() -> chunk_proof(Mode, Change) end
        }
    ||
        Mode <- [confirmed, split, rebased, nested, pending, pending_json],
        Change <- [valid, data, path]
    ].

chunk_proof(Mode, Change) ->
    Data = <<"test-data">>,
    Leaf = {ar_tx:generate_chunk_id(Data), byte_size(Data)},
    Filler =
        {
            ar_tx:generate_chunk_id(binary:copy(<<0>>, ?DATA_CHUNK_SIZE)),
            ?DATA_CHUNK_SIZE
        },
    {DataRoot, DataTree} =
        ar_merkle:generate_tree(
            case Mode of
                split ->
                    [
                        {ar_tx:generate_chunk_id(<<"other">>), 5},
                        {element(1, Leaf), 14}
                    ];
                rebased -> [[Filler], [Leaf]];
                nested -> [[Filler], [[Filler], [Leaf]]];
                _ -> [Leaf]
            end
        ),
    DataSize =
        case Mode of
            split -> 14;
            rebased -> ?DATA_CHUNK_SIZE + 9;
            nested -> 2 * ?DATA_CHUNK_SIZE + 9;
            _ -> 9
        end,
    DataPath = ar_merkle:generate_path(DataRoot, DataSize - 1, DataTree),
    {TXRoot, TXTree} =
        ar_merkle:generate_tree(
            [
                {<<0:256>>, 100},
                {DataRoot, 100 + DataSize},
                {<<1:256>>, 125 + DataSize}
            ]
        ),
    JSON = #{
        <<"chunk">> => hb_util:encode(Data),
        <<"data_path">> => hb_util:encode(DataPath),
        <<"tx_path">> =>
            hb_util:encode(ar_merkle:generate_path(TXRoot, 100, TXTree)),
        <<"absolute_end_offset">> => hb_util:bin(1000 + DataSize),
        <<"chunk_size">> => <<"9">>
    },
    Changed =
        case Change of
            valid -> JSON;
            data -> JSON#{ <<"chunk">> := hb_util:encode(<<"bad!-data">>) };
            path ->
                PrefixSize = byte_size(DataPath) - 64,
                <<Prefix:PrefixSize/binary, Byte, Rest/binary>> = DataPath,
                JSON#{
                    <<"data_path">> :=
                        hb_util:encode(
                            <<Prefix/binary, (Byte bxor 1), Rest/binary>>
                        )
                }
        end,
    Header = hb_json:decode(post_tx_json_payload(post_tx_json_client_opts())),
    {ok, Node, Handle} =
        hb_mock_server:start(
            [
                {"/chunk/:offset", chunk, {200, hb_json:encode(Changed)}},
                {
                    "/unconfirmed_chunk/:id/:offset",
                    chunk,
                    {200, hb_json:encode(Changed)}
                },
                {"/unconfirmed_tx/:id", tx, {200, hb_json:encode(Header)}}
            ]
        ),
    Opts = #{
        <<"store">> => [hb_test_utils:test_store(hb_store_volatile)],
        <<"routes">> =>
            [
                #{
                    <<"template">> => <<"^/arweave">>,
                    <<"node">> =>
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"with">> => Node,
                            <<"opts">> => #{ <<"http-client">> => httpc }
                        }
                }
            ]
    },
    Offset =
        case Mode of
            pending ->
                #{
                    <<"relative">> => maps:get(<<"id">>, Header),
                    <<"offset">> => 0
                };
            _ -> 1000 + DataSize - byte_size(Data)
        end,
    try
        Result =
            case Mode of
                pending_json ->
                    hb_ao:resolve(
                        #{ <<"device">> => <<"arweave@2.9">> },
                        #{
                            <<"path">> => <<"pending">>,
                            <<"pending">> => maps:get(<<"id">>, Header),
                            <<"offset">> => 9
                        },
                        Opts
                    );
                _ -> hb_store_arweave:read_chunks(Offset, 4, Opts)
            end,
        case {Change, Mode} of
            {valid, pending_json} ->
                {ok, Received} = Result,
                ?assertEqual(Changed, maps:with(maps:keys(Changed), Received));
            {valid, _} -> ?assertEqual({ok, <<"test">>}, Result);
            _ -> ?assertEqual({error, 'invalid-chunk-proof'}, Result)
        end
    after
        hb_mock_server:stop(Handle)
    end.

pending_offset_handling_test() ->
    ClientOpts = post_tx_json_client_opts(),
    PendingData = <<"test-data">>,
    PendingLength = byte_size(PendingData),
    #tx{ data_root = DataRoot, data_tree = DataTree } =
        ar_tx:generate_chunk_tree(
            #tx{
                data = PendingData,
                data_size = PendingLength,
                format = 2
            }
    ),
    DataPath = ar_merkle:generate_path(DataRoot, 0, DataTree),
    HeaderJSON = hb_json:decode(post_tx_json_payload(ClientOpts)),
    TXID = maps:get(<<"id">>, HeaderJSON),
    ChunkBody =
        hb_json:encode(
            #{
                <<"chunk">> => hb_util:encode(PendingData),
                <<"data_path">> => hb_util:encode(DataPath)
            }
        ),
    {ok, MockNode, MockHandle} = hb_mock_server:start([
        {"/unconfirmed_chunk/:id/:offset", pending_chunk, {200, ChunkBody}},
        {"/unconfirmed_tx/:id", pending_tx, {200, hb_json:encode(HeaderJSON)}}
    ]),
    Routes = [
        #{
            <<"template">> => <<"^/arweave">>,
            <<"nodes">> => [
                #{
                    <<"match">> => <<"^/arweave">>,
                    <<"with">> => MockNode,
                    <<"opts">> => #{ <<"http-client">> => httpc }
                }
            ],
            <<"stop-after">> => true
        }
    ],
    IndexStore =
        #{
            <<"index-store">> => [hb_test_utils:test_store()],
            <<"routes">> => Routes
        },
    try
        ok = hb_store_arweave:write_offset(
            IndexStore,
            TXID,
            <<"tx@1.0">>,
            relative,
            PendingLength
        ),
        {ok, StoreTX} =
            hb_store_arweave:read(IndexStore, #{ <<"read">> => TXID }, ClientOpts),
        ?assertEqual(TXID, hb_message:id(StoreTX, signed, ClientOpts)),
        ?assertEqual(PendingData, maps:get(<<"data">>, StoreTX))
    after
        hb_mock_server:stop(MockHandle)
    end,
    % Pending transactions whose chunks end off multiples of 256 KiB read in
    % full: arweave-js's even split, an uneven split, four 128 KiB chunks, and
    % two chunks in a rebased subtree before a third.
    lists:foreach(
        fun({Sizes, Rebased}) ->
            {ID, Data, PeerOpts, PeerHandle} =
                start_pending_peer(Sizes, Rebased, ClientOpts),
            try
                ?assertEqual(
                    {ok, Data},
                    hb_store_arweave:read_chunks(
                        #{ <<"relative">> => ID, <<"offset">> => 0 },
                        byte_size(Data),
                        PeerOpts
                    )
                ),
                % A read with no length returns only the first chunk.
                ?assertEqual(
                    {ok, binary:part(Data, 0, hd(Sizes))},
                    hb_ao:resolve(
                        #{ <<"device">> => <<"arweave@2.9">> },
                        #{
                            <<"path">> => <<"chunk">>,
                            <<"pending">> => ID,
                            <<"offset">> => 0
                        },
                        PeerOpts
                    )
                )
            after
                hb_mock_server:stop(PeerHandle)
            end
        end,
        [
            {[136192, 136192], 0},
            {[200000, 100000], 0},
            {[131072, 131072, 131072, 131072], 0},
            {[200000, 200000, 134528], 2}
        ]
    ).

%% @doc Start a peer that serves a pending transaction of random chunks of the
%% given sizes, each chunk only at its end offset. The first `Rebased' chunks
%% are a rebased subtree of the transaction's data tree. Returns the
%% transaction's ID and data, the options that route to the peer, and the
%% peer's handle.
start_pending_peer(Sizes, Rebased, Opts) ->
    Chunks =
        ar_tx:chunks_to_size_tagged_chunks(
            [crypto:strong_rand_bytes(Size) || Size <- Sizes]
        ),
    Leaves = ar_tx:sized_chunks_to_sized_chunk_ids(Chunks),
    {Root, Tree} =
        ar_merkle:generate_tree(
            case lists:split(Rebased, Leaves) of
                {[], _} -> Leaves;
                {Subtree, Rest} -> [Subtree | Rest]
            end
        ),
    TX =
        ar_tx:sign(
            #tx{ format = 2, data_root = Root, data_size = lists:sum(Sizes) },
            hb:wallet()
        ),
    {ok, Node, Handle} = hb_mock_server:start([
        {"/unconfirmed_chunk/:id/:offset", pending_chunk,
            fun(#{ <<"bindings">> := #{ offset := Offset } }) ->
                End = hb_util:int(Offset),
                case lists:keyfind(End, 2, Chunks) of
                    {Chunk, End} ->
                        Path = ar_merkle:generate_path(Root, End - 1, Tree),
                        {
                            200,
                            hb_json:encode(
                                #{
                                    <<"chunk">> => hb_util:encode(Chunk),
                                    <<"data_path">> => hb_util:encode(Path)
                                }
                            )
                        };
                    false ->
                        {404, <<>>}
                end
            end},
        {"/unconfirmed_tx/:id", pending_tx,
            {200, hb_json:encode(ar_tx:tx_to_json_struct(TX))}}
    ]),
    Route =
        #{
            <<"template">> => <<"^/arweave">>,
            <<"node">> =>
                #{
                    <<"match">> => <<"^/arweave">>,
                    <<"with">> => Node,
                    <<"opts">> => #{ <<"http-client">> => httpc }
                }
        },
    {
        hb_util:human_id(TX#tx.id),
        iolist_to_binary([Chunk || {Chunk, _} <- Chunks]),
        Opts#{ <<"routes">> => [Route] },
        Handle
    }.

%% @doc: helper test to generate and write a dataitem to disk so that we
%% can validate it using 3rd-party js libraries and gateways.
serialize_data_item_test_disabled() ->
    DataItem = ar_bundles:sign_item(
        #tx{
            data = <<"Hello from HyperBEAM test!">>,
            tags = [
                {<<"content-type">>, <<"text/plain">>},
                {<<"test-tag">>, <<"test-value">>},
                {<<"app-name">>, <<"HyperBEAM">>}
            ]
        },
        hb:wallet()
    ),
    SerializedItem = ar_bundles:serialize(DataItem),
    % Write to disk in the test directory
    OutputPath = filename:join([
        "test",
        "arbundles.js",
        "hyperbeam-test-item.bin"
    ]),
    ok = filelib:ensure_dir(OutputPath),
    ok = file:write_file(OutputPath, SerializedItem),
    ?event({wrote_data_item, {path, OutputPath}, {size, byte_size(SerializedItem)}}),
    ?assert(filelib:is_file(OutputPath)),
    % Read it back and verify it deserializes correctly
    {ok, ReadData} = file:read_file(OutputPath),
    VerifiedItem = ar_bundles:deserialize(ReadData),
    ?assertEqual(DataItem#tx.data, VerifiedItem#tx.data),
    ?assertEqual(length(DataItem#tx.tags), length(VerifiedItem#tx.tags)),
    ?assert(ar_bundles:verify_item(VerifiedItem)),
    ok.

pending_relative_chunk_helpers_test_parallel() ->
    DataSize = 315127,
    ?assertEqual([?DATA_CHUNK_SIZE], pending_relative_chunk_offsets(0, 1, DataSize)),
    ?assertEqual([DataSize], pending_relative_chunk_offsets(?DATA_CHUNK_SIZE, 1, DataSize)),
    ?assertEqual([?DATA_CHUNK_SIZE, DataSize], pending_relative_chunk_offsets(0, DataSize, DataSize)).

get_partial_chunk_post_split_test_parallel() ->
    %% https://arweave.net/tx/QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4/offset
    %% 
    Offset = 378092137521399,
    ExpectedLength = 1000,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"G62E7qonT1RBmkC6e3pNJz_thpS9xkVD3qTJAk6o3Uc">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_full_chunk_post_split_test_parallel() ->
    %% https://arweave.net/tx/QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4/offset
    %% 
    Offset = 378092137521399,
    ExpectedLength = ?DATA_CHUNK_SIZE,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"LyTBdUe0rNmpqt8C-p7HksdiredXaa0wCBAPt3504W0">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_multi_chunk_post_split_test_parallel() ->
    %% https://arweave.net/tx/QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4/offset
    %% 
    Offset = 378092137521399,
    ExpectedLength = ?DATA_CHUNK_SIZE * 3,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"4Cb_N0z0tMDwCiWrUbuzktfn-H6NLHT1btXGDo3CByI">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.


%% @doc Query a chunk range that starts and ends in the middle of a chunk.
get_mid_chunk_post_split_test_parallel() ->
    %% https://arweave.net/tx/QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4/offset
    %% 
    Offset = 378092137521399 + 200_000,
    ExpectedLength = ?DATA_CHUNK_SIZE + 300_000,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"xkEZpGqDiCVuVZfGVyscmfYNZqYmgBLjOrMD2P_SfWs">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_partial_chunk_pre_split_test_parallel() ->
    %% https://arweave.net/tx/v4ophPvV-cNp5gkpkjMuUZ-lf-fBfm1Wk-pB4vJb00E/offset
    %% 
    Offset = 30575701172109,
    ExpectedLength = 1000,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"yU5tZyDCTZ4MFcT6lng74tvx1oIbPkpCw1VAJsSqeuo">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_full_chunk_pre_split_test_parallel() ->
    %% https://arweave.net/tx/v4ophPvV-cNp5gkpkjMuUZ-lf-fBfm1Wk-pB4vJb00E/offset
    %% 
    Offset = 30575701172109,
    ExpectedLength = ?DATA_CHUNK_SIZE,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"nVCvjEq9T5nxIR6jvglNbX1_CYCg0WifxfQoXhS4gik">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_multi_chunk_pre_split_test_parallel() ->
    %% https://arweave.net/tx/v4ophPvV-cNp5gkpkjMuUZ-lf-fBfm1Wk-pB4vJb00E/offset
    %% 
    Offset = 30575701172109,
    ExpectedLength = ?DATA_CHUNK_SIZE * 3,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"DfS3jtLXqG3zO_IFA3P-r55SUBoeJmeIh4Eim2Rldeo">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_mid_chunk_pre_split_test_parallel() ->
    %% https://arweave.net/tx/v4ophPvV-cNp5gkpkjMuUZ-lf-fBfm1Wk-pB4vJb00E/offset
    %% 
    Offset = 30575701172109 + 200_000,
    ExpectedLength = ?DATA_CHUNK_SIZE + 300_000,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"mgSfqsNapn_BXpbnIHtdeu3rQyvrjBaS0c7rEbUbtBU">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_pre_split_small_chunks_test_parallel() ->
    TXID = <<"4FnBmvgWmqXWEEprjVqBsV5aRpAgF6_yJX_GTGsSZjY">>,
    Opts = setup_arweave_index_opts([TXID]),
    assert_chunk_range(
        <<"tx@1.0">>,
        TXID,
        11_741_031_646_397 - 810774,
        810774,
        <<"LJbiKv5gT2Y5XKFFPF6WqYAdOtaZAvHmtCkfCTbP43g">>,
        Opts
    ).

get_post_split_small_chunks_test_parallel() ->
    TXID = <<"YR9m4c3CrlljCRYEWBLeoKekbAyYZRMo2Kpz61IeNp8">>,
    Opts = setup_arweave_index_opts([TXID]),
    assert_chunk_range(
        <<"tx@1.0">>,
        TXID,
        146_563_435_390_439 - 541937,
        541937,
        <<"cR2HRQRfZP_MiC1egrdc8y8j4SAF9-ppvaIaXDq5i7s">>,
        Opts
    ).

get_pre_split_gap_test_parallel() ->
    TXID = <<"VexuG68KCNpw21fGZw1ycRCYBtQMHhl274zGDBh3kQE">>,
    Opts = setup_arweave_index_opts([TXID]),
    assert_chunk_range(
        <<"tx@1.0">>,
        TXID,
        13308109889261 - 8789723,
        8789723,
        <<"X6sbQdUyKTQ8LGzmleWU_jxO8Oda7S_bshDDKP_Mnqs">>,
        Opts
    ).

get_pre_split_small_tx_test_parallel() ->
    TXID = <<"K4C4dLZ7V4ffYJcR9JtVQwIXCTLD1mMCUaPbHuUdFgw">>,
    Opts = setup_arweave_index_opts([TXID]),
    assert_chunk_range(
        <<"tx@1.0">>,
        TXID,
        12778619748052 - 1444,
        1444,
        <<"o7gJm-FgmWcIvbDiFxDaL56WkJIWQCwsN95Z8zNjEO8">>,
        Opts
    ).

%% @doc Checks an item that begins in the middle of a chunk - without
%% special handling get_chunk_range() used to leave off the last few bytes
get_ed25519_item_test_parallel() ->
    TXID = <<"jTFA8XDI_rqmUB6-hhoJF4Yi7p6ZpS_0AByFLU1OPrU">>,
    DataItemID = <<"1rTy7gQuK9lJydlKqCEhtGLp2WWG-GOrVo5JdiCmaxs">>,
    Opts = setup_arweave_index_opts([TXID]),
    assert_chunk_range(
        <<"ans104@1.0">>,
        DataItemID,
        160399272861859,
        499025,
        <<"PQ5sHoQYSdi1unjHjsfNS_ZXdMvmznEvIkBTvToqVbU">>,
        Opts
    ).

%% @doc this test fails if the chunks are queried with
%% the `x-bucket-based-offset' header set.
bucket_based_offset_fail_test_parallel() ->
    TXID = <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
    DataItemID = <<"z-oKJfhMq5qoVFrljEfiBKgumaJmCWVxNJaavR5aPE8">>,
    Opts = setup_arweave_index_opts([TXID]),
    assert_chunk_range(
        <<"ans104@1.0">>,
        DataItemID,
        376836461101675,
        116247,
        <<"4BN8AQEQLpTjresTntyrjJ94eFS2TaMM21MnuHGXtJc">>,
        Opts
    ).

%% @doc this dataitem needs the 'x-bucket-based-offset' header set OR
%% special handling.
bucket_based_offset_pass_test_parallel() ->
    DataItemID = <<"cTI07T1OrF0KZEqPmZji1VTdbeKJG7kMAVlLu7KQvyw">>,
    Opts = setup_arweave_index_opts([]),
    assert_chunk_range(
        <<"ans104@1.0">>,
        DataItemID,
        384600234780716,
        856885,
        <<"EVLmVPkpWZjcDtw_zX2r18O7GC85P8VmuaKNy-sDRrw">>,
        Opts
    ).

reassemble_bundle1_test_parallel() ->
    assert_bundle_tx(<<"c1-FkhQd-Ul-VpIMR5Vs77lK__BlzHzena2zgNh_hME">>).

reassemble_bundle2_test_parallel() ->
    assert_bundle_tx(<<"OVjj52NvyIys7u84Rv1uqRG2vswlF95QDVPSmsmlwLk">>).

%% @doc This asserts that a bundle is correctly represented in the weave.
%% It queries the L1 TX chunk range, reads the chunks, and then
%% reassembles the bundle and nested items. This is also useful tool 
%% debugging tool to check that a bundle is present in the weave.
assert_bundle_tx(TXID) ->
    Opts = #{},
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
    StartOffset = EndOffset - Size,
    ?event(debug_test, {offset_info,
        {tx, TXID}, {start_offset, StartOffset}, {size, Size}}),
    assert_bundle_items(TXID, StartOffset, Size, Opts).

%% @doc Download, decode, and verify all items in a bundle TX. Fetches the
%% chunk range and TX header from the arweave@2.9 device, parses the bundle
%% header, then verifies and logs each L1 item. Recurses into nested bundles.
assert_bundle_items(TXID, StartOffset, Size, Opts) ->
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => StartOffset + 1,
            <<"length">> => Size
        },
        Opts
    ),
    ?event(debug_test, {chunk_data_size, byte_size(Data)}),
    {ok, TXHeader} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => TXID,
            <<"exclude-data">> => true
        },
        Opts
    ),
    ?event(debug_test, {l1_tx_header, TXHeader}),
    {ItemsBin, BundleHeader} = ar_bundles:decode_bundle_header(Data),
    lists:foldl(
        fun({ID, ItemSize}, Offset) ->
            ItemBin = binary:part(ItemsBin, Offset, ItemSize),
            Item = ar_bundles:deserialize(ItemBin),
            ?assert(ar_bundles:verify_item(Item)),
            ?event(debug_test, {l2_bundle,
                {id, {explicit, hb_util:encode(ID)}},
                {size, ItemSize},
                {tags, Item#tx.tags},
                {data_size, Item#tx.data_size},
                {format, Item#tx.format},
                {signature_type, Item#tx.signature_type}
            }),
            case ar_tx:type(Item) of
                list -> print_nested_items(Item#tx.data);
                _ -> ok
            end,
            Offset + ItemSize
        end,
        0,
        BundleHeader
    ),
    ok.

print_nested_items(DataMap) when is_map(DataMap) ->
    maps:foreach(
        fun(Key, Child) ->
            ?assert(ar_bundles:verify_item(Child)),
            ?event(debug_test, {l3_nested_item,
                {key, Key},
                {id, {explicit, hb_util:encode(ar_bundles:id(Child, unsigned))}},
                {tags, Child#tx.tags},
                {data_size, Child#tx.data_size},
                {format, Child#tx.format},
                {signature_type, Child#tx.signature_type}
            })
        end,
        DataMap
    );
print_nested_items(Items) when is_list(Items) ->
    lists:foreach(
        fun(Child) ->
            ?assert(ar_bundles:verify_item(Child)),
            ?event(debug_test, {l3_nested_item,
                {id, {explicit, hb_util:encode(ar_bundles:id(Child, unsigned))}},
                {tags, Child#tx.tags},
                {data_size, Child#tx.data_size},
                {format, Child#tx.format},
                {signature_type, Child#tx.signature_type}
            })
        end,
        Items
    ).

% large_tx_test() ->
%     assert_chunk_range(
%         <<"GX2bvdo736wJPR1GmIkyW9GRk3JdXQ_aAd1ozX1d450">>,
%         378161418083672,
%         42040418,
%         <<"wmDVKM6nYRvqre2DdxmX_mhJ6u8unwmTD4YdmzERcZs">>
%     ).

assert_chunk_range(Type, ID, StartOffset, ExpectedLength, ExpectedHash, Opts) ->
    T1 = erlang:monotonic_time(millisecond),
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => StartOffset+1,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    T2 = erlang:monotonic_time(millisecond),
    ?event(debug_performance, {chunk_range_resolve,
        {elapsed_ms, T2 - T1},
        {id, {explicit, ID}},
        {offset, StartOffset + 1},
        {length, ExpectedLength}
    }),
    % {ok, RawDataMsg} = hb_ao:resolve(
    %     #{ <<"device">> => <<"arweave@2.9">> },
    %     #{
    %         <<"path">> => <<"raw">>,
    %         <<"raw">> => ID
    %     },
    %     Opts
    % ),
    % RawData = hb_ao:get(<<"data">>, RawDataMsg, Opts),
    % ?event(debug_test, {chunk_vs_raw_comparison,
    %     {id, {explicit, ID}},
    %     {type, Type},
    %     {start_offset, StartOffset},
    %     {expected_length, ExpectedLength},
    %     {chunk_size, byte_size(Data)},
    %     {raw_size, byte_size(RawData)},
    %     {match, Data =:= RawData},
    %     {hash, {explicit, hb_util:encode(crypto:hash(sha256, Data))}}
    % }),
    case Type of
        <<"ans104@1.0">> ->
            Item = ar_bundles:deserialize(Data),
            ?event(debug_test, {item, Item}),
            ?assert(ar_bundles:verify_item(Item));
            % ?assertEqual(RawData, Item#tx.data);
        <<"tx@1.0">> ->
            {ok, TXHeader} = hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                #{
                    <<"path">> => <<"tx">>,
                    <<"tx">> => ID,
                    <<"exclude-data">> => true
                },
                Opts
            ),
            ?assertEqual(false, maps:is_key(<<"data">>, TXHeader)),
            ?event(debug_test, {tx_header, TXHeader}),
            ?assert(hb_message:verify(TXHeader, all, Opts)),
            TXWithData = TXHeader#{ <<"data">> => Data },
            ?event(debug_test, {tx_with_data, TXWithData}),
            ?assert(hb_message:verify(TXWithData, all, Opts))
            % ?assertEqual(RawData, Data)
    end,
    ?event(debug_test, {data, {explicit,  hb_util:encode(crypto:hash(sha256, Data))}}),
    ?assertEqual(ExpectedHash, hb_util:encode(crypto:hash(sha256, Data))),
    ok.

get_post_split_mid_chunk_large_module_test_parallel() ->
    Offset = 194_794_421_495_003,
    ExpectedLength = 732_228,
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset + 1,
            <<"length">> => ExpectedLength
        },
        #{}
    ),
    ?assertEqual(ExpectedLength, byte_size(Data)).
