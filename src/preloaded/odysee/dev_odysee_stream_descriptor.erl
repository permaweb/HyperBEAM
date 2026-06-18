%%% @doc Odysee stream descriptor compatibility device.
%%%
%%% This device preserves and validates legacy LBRY stream descriptor JSON,
%%% verifies encrypted blobs by their SHA-384 hashes, and reconstructs the
%%% original media bytes from supplied or fetched encrypted blob bytes.
-module(dev_odysee_stream_descriptor).
-implements(<<"odysee-stream-descriptor@1.0">>).
-export([info/1, decode/3, fetch/3, verify/3, reconstruct/3, media/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"odysee-stream-descriptor@1.0">>).
-define(DEFAULT_BLOB_BASE_URLS, [
    <<"https://blobcache-eu.odycdn.com">>,
    <<"https://blobcache-us.odycdn.com">>,
    <<"https://blobcache.lbry.com">>
]).
-define(DEFAULT_RANGE_CHUNK_SIZE, 1048576).
-define(DEFAULT_FULL_RESPONSE_LIMIT, 8388608).
-define(DEFAULT_BLOB_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_BLOB_RECV_TIMEOUT, 30000).
-define(DEFAULT_BLOB_CHECKOUT_TIMEOUT, 5000).
-define(DEFAULT_LBRYNET_TIMEOUT, 120).
-define(SHA384_HEX_SIZE, 96).
-define(AES_BLOCK_SIZE, 16).

%% @doc Return the public device API.
info(_Opts) ->
    #{
        exports => [
            <<"decode">>,
            <<"fetch">>,
            <<"verify">>,
            <<"reconstruct">>,
            <<"media">>
        ]
    }.

%% @doc Decode and validate stream descriptor JSON.
decode(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, Raw} ?= descriptor_bytes(Base, Req, Opts),
            {ok, decode_descriptor(Raw, provided_sd_hash(Base, Req, Opts), Opts)}
        else
            Error -> Error
        end
    end).

%% @doc Fetch a stream descriptor by `sd-hash' and decode it.
fetch(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, SDHash} ?= sd_hash(Base, Req, Opts),
            {ok, Raw} ?= fetch_blob(SDHash, Base, Req, Opts),
            {ok, decode_descriptor(Raw, SDHash, Opts)}
        else
            Error -> Error
        end
    end).

%% @doc Verify any supplied encrypted blob bytes against the descriptor.
verify(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, Desc} ?= ensure_descriptor(Base, Req, Opts),
            verify_descriptor_blobs(Desc, Base, Req, Opts)
        else
            Error -> Error
        end
    end).

%% @doc Verify, decrypt, and concatenate stream blobs.
reconstruct(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, Desc} ?= ensure_descriptor(Base, Req, Opts),
            {ok, Key} ?=
                decode_hex(
                    <<"key">>,
                    hb_maps:get(<<"key">>, Desc, not_found, Opts),
                    16
                ),
            {ok, Parts} ?= decrypt_blobs(data_blobs(Desc), Key, Base, Req, Opts),
            {ok,
                Desc#{
                    <<"verified">> => true,
                    <<"body">> => iolist_to_binary(Parts),
                    <<"content-type">> =>
                        hb_maps:get(
                            <<"media-type">>,
                            Req,
                            <<"application/octet-stream">>,
                            Opts
                        )
                }
            }
        else
            Error -> Error
        end
    end).

%% @doc Serve reconstructed media bytes with browser-compatible range headers.
media(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, Desc} ?= ensure_descriptor(Base, Req, Opts),
            {ok, Size} ?= media_size(Base, Req, Opts),
            media_response(Desc, Size, Base, Req, Opts)
        else
            Error -> Error
        end
    end).

safe(Fun) ->
    try Fun() of
        Res -> Res
    catch
        _:{error, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

ensure_descriptor(Base, _Req, _Opts)
        when is_map(Base),
             is_map_key(<<"stream-hash">>, Base),
             is_map_key(<<"blobs">>, Base) ->
    {ok, Base};
ensure_descriptor(Base, Req, Opts) ->
    case descriptor_bytes(Base, Req, Opts) of
        {ok, _Raw} -> decode(Base, Req, Opts);
        {error, descriptor_not_found} -> fetch(Base, Req, Opts);
        Error -> Error
    end.

decode_descriptor(Raw, ProvidedSDHash, Opts) ->
    case try_decode_json(Raw) of
        {ok, Source} -> normalize_descriptor(Source, Raw, ProvidedSDHash, Opts);
        Error -> throw(Error)
    end.

try_decode_json(Raw) ->
    try {ok, hb_json:decode(Raw)}
    catch
        _:_ -> {error, invalid_json}
    end.

normalize_descriptor(Source, Raw, ProvidedSDHash, Opts) when is_map(Source) ->
    maybe
        {ok, StreamType} ?= required(<<"stream_type">>, Source, Opts),
        {ok, StreamNameHex0} ?= required(<<"stream_name">>, Source, Opts),
        {ok, KeyHex0} ?= required(<<"key">>, Source, Opts),
        {ok, SuggestedHex0} ?= required(<<"suggested_file_name">>, Source, Opts),
        {ok, StreamHash0} ?= required(<<"stream_hash">>, Source, Opts),
        {ok, RawBlobs} ?= required(<<"blobs">>, Source, Opts),
        {ok, StreamNameHex, StreamName} ?=
            decode_hex_value(<<"stream_name">>, StreamNameHex0),
        {ok, KeyHex, _Key} ?= decode_hex_value(<<"key">>, KeyHex0, 16),
        {ok, SuggestedHex, SuggestedFileName} ?=
            decode_hex_value(<<"suggested_file_name">>, SuggestedHex0),
        {ok, Blobs} ?= normalize_blobs(RawBlobs),
        StreamHash = normalize_hex(StreamHash0),
        ok ?= require_sha384_hex(StreamHash, <<"stream_hash">>),
        ok ?= validate_stream_hash(StreamHash, StreamNameHex, KeyHex, SuggestedHex, Blobs),
        SDHash = sha384_hex(Raw),
        ok ?= validate_provided_sd_hash(ProvidedSDHash, SDHash),
        #{
            <<"device">> => ?DEVICE,
            <<"content-type">> => <<"application/json">>,
            <<"body">> => Raw,
            <<"source">> => Source,
            <<"stream-type">> => StreamType,
            <<"stream-name">> => StreamName,
            <<"stream-name-hex">> => StreamNameHex,
            <<"key">> => KeyHex,
            <<"suggested-file-name">> => SuggestedFileName,
            <<"suggested-file-name-hex">> => SuggestedHex,
            <<"stream-hash">> => StreamHash,
            <<"sd-hash">> => SDHash,
            <<"descriptor-store-path">> => <<"odysee/descriptor/", SDHash/binary>>,
            <<"blob-store-paths">> => blob_store_paths(Blobs),
            <<"blobs">> => Blobs
        }
    else
        Error -> throw(Error)
    end;
normalize_descriptor(_Source, _Raw, _ProvidedSDHash, _Opts) ->
    throw(invalid_descriptor_json).

required(Key, Map, Opts) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> {error, {missing, Key}};
        Value -> {ok, Value}
    end.

normalize_blobs(Blobs) when is_list(Blobs), Blobs =/= [] ->
    normalize_blobs(Blobs, 0, []);
normalize_blobs(_) ->
    {error, invalid_blobs}.

normalize_blobs([], _Expected, _Acc) ->
    {error, missing_terminator};
normalize_blobs([Blob], Expected, Acc) ->
    maybe
        {ok, Norm} ?= normalize_blob(Blob, Expected),
        ok ?= require_terminator(Norm),
        {ok, lists:reverse([Norm | Acc])}
    end;
normalize_blobs([Blob | Rest], Expected, Acc) ->
    maybe
        {ok, Norm} ?= normalize_blob(Blob, Expected),
        ok ?= require_data_blob(Norm),
        normalize_blobs(Rest, Expected + 1, [Norm | Acc])
    end.

normalize_blob(Blob, Expected) when is_map(Blob) ->
    maybe
        {ok, BlobNum} ?= required(<<"blob_num">>, Blob, #{}),
        ok ?= require_blob_num(BlobNum, Expected),
        {ok, Length} ?= required(<<"length">>, Blob, #{}),
        ok ?= require_valid_length(Length, Expected),
        {ok, IVHex, _IV} ?= decode_hex_value(
            <<"iv">>,
            hb_maps:get(<<"iv">>, Blob, not_found, #{}),
            16
        ),
        normalize_blob_hash(Blob, Expected, Length, IVHex)
    end;
normalize_blob(_Blob, _Expected) ->
    {error, invalid_blob}.

normalize_blob_hash(Blob, _Expected, 0, _IVHex)
        when is_map_key(<<"blob_hash">>, Blob) ->
    {error, terminator_has_hash};
normalize_blob_hash(_Blob, Expected, 0, IVHex) ->
    {ok, #{
        <<"blob-num">> => Expected,
        <<"length">> => 0,
        <<"iv">> => IVHex
    }};
normalize_blob_hash(Blob, Expected, Length, IVHex) ->
    maybe
        {ok, BlobHash0} ?= required(<<"blob_hash">>, Blob, #{}),
        BlobHash = normalize_hex(BlobHash0),
        ok ?= require_sha384_hex(BlobHash, <<"blob_hash">>),
        {ok, #{
            <<"blob-num">> => Expected,
            <<"length">> => Length,
            <<"iv">> => IVHex,
            <<"blob-hash">> => BlobHash,
            <<"blob-store-path">> => <<"odysee/blob/", BlobHash/binary>>
        }}
    end.

blob_store_paths(Blobs) ->
    hb_util:list_to_numbered_message(
        [
            hb_maps:get(<<"blob-store-path">>, Blob, #{})
        ||
            Blob <- Blobs,
            not is_terminator(Blob)
        ]
    ).

is_terminator(#{ <<"length">> := 0 }) -> true;
is_terminator(_) -> false.

require_terminator(Blob) ->
    case is_terminator(Blob) of
        true -> ok;
        false -> {error, missing_terminator}
    end.

require_data_blob(Blob) ->
    case is_terminator(Blob) of
        true -> {error, terminator_not_final};
        false -> ok
    end.

require_valid_length(Length, _Expected)
        when is_integer(Length), Length >= 0 ->
    ok;
require_valid_length(_Length, Expected) ->
    {error, {invalid_length, Expected}}.

require_blob_num(Expected, Expected) ->
    ok;
require_blob_num(BlobNum, Expected) ->
    {error, {unexpected_blob_num, BlobNum, Expected}}.

require_sha384_hex(Hex, Name) when byte_size(Hex) =:= ?SHA384_HEX_SIZE ->
    case decode_hex(Name, Hex) of
        {ok, _} -> ok;
        Error -> Error
    end;
require_sha384_hex(_Hex, Name) ->
    {error, {invalid_hex_size, Name}}.

validate_stream_hash(StreamHash, StreamNameHex, KeyHex, SuggestedHex, Blobs) ->
    case calculate_stream_hash(StreamNameHex, KeyHex, SuggestedHex, Blobs) of
        StreamHash -> ok;
        _ -> {error, stream_hash_mismatch}
    end.

verify_descriptor_blobs(Desc, Base, Req, Opts) ->
    {Verified, Missing} =
        lists:foldl(
            fun(Blob, {VerifiedAcc, MissingAcc}) ->
                Hash = hb_maps:get(<<"blob-hash">>, Blob, not_found, Opts),
                case get_blob_bytes(Hash, Base, Req, Opts) of
                    {ok, Bytes} ->
                        case verify_blob_bytes(Blob, Bytes) of
                            ok -> {[Hash | VerifiedAcc], MissingAcc};
                            Error -> throw(Error)
                        end;
                    {error, not_found} ->
                        {VerifiedAcc, [Hash | MissingAcc]};
                    Error ->
                        throw(Error)
                end
            end,
            {[], []},
            data_blobs(Desc)
        ),
    {ok,
        Desc#{
            <<"verified">> => Missing =:= [],
            <<"verified-blobs">> => lists:reverse(Verified),
            <<"missing-blobs">> => lists:reverse(Missing)
        }
    }.

decrypt_blobs([], _Key, _Base, _Req, _Opts) ->
    {ok, []};
decrypt_blobs([Blob | Rest], Key, Base, Req, Opts) ->
    maybe
        {ok, Plain} ?= get_plain_blob(Blob, Key, Base, Req, Opts),
        {ok, Tail} ?= decrypt_blobs(Rest, Key, Base, Req, Opts),
        {ok, [Plain | Tail]}
    end.

verify_blob_bytes(Blob, Bytes) ->
    Hash = hb_maps:get(<<"blob-hash">>, Blob, #{}),
    Length = hb_maps:get(<<"length">>, Blob, #{}),
    case {byte_size(Bytes), sha384_hex(Bytes)} of
        {Length, Hash} -> ok;
        {OtherLength, _} when OtherLength =/= Length ->
            {error, {blob_length_mismatch, Hash}};
        {_, _} ->
            {error, {blob_hash_mismatch, Hash}}
    end.

decrypt_blob(Bytes, Key, IV) ->
    try crypto:crypto_one_time(aes_128_cbc, Key, IV, Bytes, false) of
        Padded -> remove_pkcs7(Padded)
    catch
        _:_ -> {error, decrypt_failed}
    end.

remove_pkcs7(Bin) when byte_size(Bin) > 0 ->
    Size = byte_size(Bin),
    PadLen = binary:at(Bin, Size - 1),
    case PadLen >= 1 andalso PadLen =< ?AES_BLOCK_SIZE andalso Size >= PadLen of
        true ->
            DataLen = Size - PadLen,
            Padding = binary:part(Bin, DataLen, PadLen),
            case Padding =:= binary:copy(<<PadLen>>, PadLen) of
                true -> {ok, binary:part(Bin, 0, DataLen)};
                false -> {error, invalid_padding}
            end;
        false ->
            {error, invalid_padding}
    end;
remove_pkcs7(_) ->
    {error, invalid_padding}.

data_blobs(Desc) ->
    lists:filter(
        fun(Blob) -> not is_terminator(Blob) end,
        hb_maps:get(<<"blobs">>, Desc, [])
    ).

media_response(Desc, Size, Base, Req, Opts) ->
    Headers = media_headers(Desc, Size, Base, Req, Opts),
    case method(Req, Opts) of
        <<"head">> -> {ok, Headers#{ <<"body">> => <<>> }};
        <<"options">> -> {ok, cors_preflight_response()};
        _ -> media_body_response(Desc, Size, Headers, Base, Req, Opts)
    end.

media_headers(Desc, Size, Base, Req, Opts) ->
    (cors_headers())#{
        <<"device">> => ?DEVICE,
        <<"content-type">> => media_type(Base, Req, Opts),
        <<"content-length">> => Size,
        <<"accept-ranges">> => <<"bytes">>,
        <<"sd-hash">> => hb_maps:get(<<"sd-hash">>, Desc, not_found, Opts)
    }.

media_body_response(Desc, Size, Headers, Base, Req, Opts) ->
    case parse_media_range(Base, Req, Size, Opts) of
        {ok, Start, End} ->
            maybe
                {ok, Body} ?= read_media_range(Desc, Start, End, Size, Base, Req, Opts),
                {ok, range_response(Headers, Start, End, Size, Body)}
            end;
        no_range ->
            case allow_full_response(Size, Base, Req, Opts) of
                true ->
                    maybe
                        {ok, Body} ?= read_media_range(Desc, 0, Size - 1, Size, Base, Req, Opts),
                        {ok, Headers#{
                            <<"status">> => 200,
                            <<"content-length">> => byte_size(Body),
                            <<"body">> => Body
                        }}
                    end;
                false ->
                    {ok, range_required_response(Size)}
            end;
        Error ->
            Error
    end.

range_response(Headers, Start, End, Size, Body) ->
    Headers#{
        <<"status">> => 206,
        <<"content-length">> => byte_size(Body),
        <<"content-range">> =>
            <<
                "bytes ",
                (integer_to_binary(Start))/binary,
                "-",
                (integer_to_binary(End))/binary,
                "/",
                (integer_to_binary(Size))/binary
            >>,
        <<"body">> => Body
    }.

range_required_response(Size) ->
    Body = <<"Range header required for large LBRY media responses.">>,
    (cors_headers())#{
        <<"status">> => 416,
        <<"content-type">> => <<"text/plain">>,
        <<"content-length">> => byte_size(Body),
        <<"content-range">> => <<"bytes */", (integer_to_binary(Size))/binary>>,
        <<"accept-ranges">> => <<"bytes">>,
        <<"body">> => Body
    }.

cors_preflight_response() ->
    (cors_headers())#{
        <<"status">> => 204,
        <<"content-type">> => <<"text/plain">>,
        <<"content-length">> => 0,
        <<"body">> => <<>>
    }.

cors_headers() ->
    #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET,HEAD,OPTIONS">>,
        <<"access-control-allow-headers">> =>
            <<"Range,Content-Type,Accept,Authorization">>,
        <<"access-control-expose-headers">> =>
            <<"Content-Length,Content-Range,Accept-Ranges,Location,Content-Digest">>
    }.

read_media_range(Desc, Start, End, Size, Base, Req, Opts) ->
    maybe
        {ok, Key} ?=
            decode_hex(
                <<"key">>,
                hb_maps:get(<<"key">>, Desc, not_found, Opts),
                16
            ),
        Blobs = data_blobs(Desc),
        Lengths = plain_blob_lengths(Blobs, Size, Opts),
        read_media_range(Blobs, Lengths, Key, Start, End, 0, [], Base, Req, Opts)
    end.

read_media_range(Blobs, unknown, Key, Start, End, Offset, Acc, Base, Req, Opts) ->
    read_media_range(Blobs, Key, Start, End, Offset, Acc, Base, Req, Opts);
read_media_range(_Blobs, _Lengths, _Key, _Start, End, Offset, Acc, _Base, _Req, _Opts)
        when Offset > End ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
read_media_range([], _Lengths, _Key, _Start, _End, _Offset, Acc, _Base, _Req, _Opts)
        when Acc =/= [] ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
read_media_range([], _Lengths, _Key, _Start, _End, _Offset, _Acc, _Base, _Req, _Opts) ->
    {error, range_not_satisfiable};
read_media_range([_Blob | Rest], [PlainSize | RestLengths], Key, Start, End, Offset, Acc, Base, Req, Opts)
        when is_integer(PlainSize), Offset + PlainSize =< Start ->
    read_media_range(
        Rest,
        RestLengths,
        Key,
        Start,
        End,
        Offset + PlainSize,
        Acc,
        Base,
        Req,
        Opts
    );
read_media_range([Blob | Rest], [_PlainSize | RestLengths], Key, Start, End, Offset, Acc, Base, Req, Opts) ->
    maybe
        {ok, Plain} ?= get_plain_blob(Blob, Key, Offset, Base, Req, Opts),
        PlainSize = byte_size(Plain),
        NextOffset = Offset + PlainSize,
        NextAcc = append_range_part(Plain, Start, End, Offset, NextOffset, Acc),
        read_media_range(Rest, RestLengths, Key, Start, End, NextOffset, NextAcc, Base, Req, Opts)
    end.

read_media_range(_Blobs, _Key, _Start, End, Offset, Acc, _Base, _Req, _Opts)
        when Offset > End ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
read_media_range([], _Key, _Start, _End, _Offset, Acc, _Base, _Req, _Opts)
        when Acc =/= [] ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
read_media_range([], _Key, _Start, _End, _Offset, _Acc, _Base, _Req, _Opts) ->
    {error, range_not_satisfiable};
read_media_range([Blob | Rest], Key, Start, End, Offset, Acc, Base, Req, Opts) ->
    maybe
        {ok, Plain} ?= get_plain_blob(Blob, Key, Offset, Base, Req, Opts),
        PlainSize = byte_size(Plain),
        NextOffset = Offset + PlainSize,
        NextAcc = append_range_part(Plain, Start, End, Offset, NextOffset, Acc),
        read_media_range(Rest, Key, Start, End, NextOffset, NextAcc, Base, Req, Opts)
    end.

plain_blob_lengths([], _MediaSize, _Opts) ->
    [];
plain_blob_lengths(Blobs, MediaSize, Opts) ->
    Lengths = [hb_maps:get(<<"length">>, Blob, not_found, Opts) || Blob <- Blobs],
    case lists:all(fun(Length) -> is_integer(Length) andalso Length > 0 end, Lengths) of
        true -> plain_blob_lengths_from_encrypted(Lengths, MediaSize);
        false -> unknown
    end.

plain_blob_lengths_from_encrypted(Lengths, MediaSize) ->
    Count = length(Lengths),
    Padding = lists:sum(Lengths) - MediaSize,
    LastPadding = Padding - (Count - 1),
    case Padding >= Count andalso LastPadding >= 1 andalso LastPadding =< ?AES_BLOCK_SIZE of
        true ->
            {Front, [Last]} = lists:split(Count - 1, Lengths),
            [Length - 1 || Length <- Front] ++ [Last - LastPadding];
        false ->
            unknown
    end.

append_range_part(_Plain, Start, _End, _Offset, NextOffset, Acc)
        when NextOffset =< Start ->
    Acc;
append_range_part(_Plain, _Start, End, Offset, _NextOffset, Acc)
        when Offset > End ->
    Acc;
append_range_part(Plain, Start, End, Offset, _NextOffset, Acc) ->
    PlainSize = byte_size(Plain),
    SliceStart = max(0, Start - Offset),
    SliceEnd = min(PlainSize - 1, End - Offset),
    case SliceEnd >= SliceStart of
        true ->
            SliceSize = SliceEnd - SliceStart + 1,
            [binary:part(Plain, SliceStart, SliceSize) | Acc];
        false ->
            Acc
    end.

parse_media_range(Base, Req, Size, Opts) ->
    case first_found(
        [
            {Req, <<"range">>},
            {Req, <<"Range">>},
            {Base, <<"range">>},
            {Base, <<"Range">>}
        ],
        Opts
    ) of
        not_found -> no_range;
        Range when is_binary(Range) ->
            parse_range_header(Range, Size, range_chunk_size(Base, Req, Opts));
        _ ->
            {error, invalid_range}
    end.

parse_range_header(<<"bytes=", Descriptor/binary>>, Size, ChunkSize) ->
    parse_range_descriptor(Descriptor, Size, ChunkSize);
parse_range_header(<<"bytes ", Descriptor/binary>>, Size, ChunkSize) ->
    parse_range_descriptor(Descriptor, Size, ChunkSize);
parse_range_header(_Range, _Size, _ChunkSize) ->
    {error, invalid_range}.

parse_range_descriptor(Descriptor, Size, ChunkSize) ->
    [FirstRange | _] = binary:split(Descriptor, <<",">>),
    [ByteRange | _] = binary:split(FirstRange, <<"/">>),
    case binary:split(ByteRange, <<"-">>) of
        [<<>>, SuffixBin] ->
            maybe
                {ok, Suffix} ?= non_negative_int(SuffixBin),
                ok ?= require_positive(Suffix),
                Start = max(0, Size - Suffix),
                validate_range(Start, Size - 1, Size)
            end;
        [StartBin, <<>>] ->
            maybe
                {ok, Start} ?= non_negative_int(StartBin),
                End = min(Size - 1, Start + ChunkSize - 1),
                validate_range(Start, End, Size)
            end;
        [StartBin, EndBin] ->
            maybe
                {ok, Start} ?= non_negative_int(StartBin),
                {ok, End0} ?= non_negative_int(EndBin),
                End = min(Size - 1, End0),
                validate_range(Start, End, Size)
            end;
        _ ->
            {error, invalid_range}
    end.

validate_range(Start, End, Size)
        when Start >= 0, Start < Size, End >= Start ->
    {ok, Start, End};
validate_range(_Start, _End, _Size) ->
    {error, range_not_satisfiable}.

media_size(Base, Req, Opts) ->
    case first_found(
        [
            {Req, <<"media-size">>},
            {Req, <<"media_size">>},
            {Req, <<"source-size">>},
            {Req, <<"source_size">>},
            {Base, <<"media-size">>},
            {Base, <<"media_size">>},
            {Base, <<"source-size">>},
            {Base, <<"source_size">>}
        ],
        Opts
    ) of
        not_found -> {error, media_size_not_found};
        Value ->
            maybe
                {ok, Size} ?= positive_int(Value),
                {ok, Size}
            end
    end.

media_type(Base, Req, Opts) ->
    case first_found(
        [
            {Req, <<"media-type">>},
            {Req, <<"media_type">>},
            {Req, <<"content-type">>},
            {Base, <<"media-type">>},
            {Base, <<"media_type">>},
            {Base, <<"content-type">>}
        ],
        Opts
    ) of
        not_found -> <<"application/octet-stream">>;
        Type -> Type
    end.

method(Req, Opts) ->
    hb_util:to_lower(hb_util:bin(hb_maps:get(<<"method">>, Req, <<"GET">>, Opts))).

range_chunk_size(Base, Req, Opts) ->
    case first_found(
        [
            {Req, <<"range-chunk-size">>},
            {Req, <<"chunk-size">>},
            {Base, <<"range-chunk-size">>},
            {Base, <<"chunk-size">>}
        ],
        Opts
    ) of
        not_found -> hb_opts:get(<<"lbry-range-chunk-size">>, ?DEFAULT_RANGE_CHUNK_SIZE, Opts);
        Value -> hb_util:int(Value)
    end.

allow_full_response(Size, Base, Req, Opts) ->
    truthy(first_found([{Req, <<"allow-full">>}, {Base, <<"allow-full">>}], Opts))
        orelse Size =< hb_opts:get(
            <<"lbry-full-response-limit">>,
            ?DEFAULT_FULL_RESPONSE_LIMIT,
            Opts
        ).

truthy(true) -> true;
truthy(1) -> true;
truthy(<<"1">>) -> true;
truthy(<<"true">>) -> true;
truthy(<<"yes">>) -> true;
truthy(_) -> false.

falsy(false) -> true;
falsy(0) -> true;
falsy(<<"0">>) -> true;
falsy(<<"false">>) -> true;
falsy(<<"no">>) -> true;
falsy(_) -> false.

positive_int(Value) ->
    maybe
        {ok, Int} ?= non_negative_int(Value),
        ok ?= require_positive(Int),
        {ok, Int}
    end.

non_negative_int(Value) when is_integer(Value), Value >= 0 ->
    {ok, Value};
non_negative_int(Value) ->
    try hb_util:int(Value) of
        Int when is_integer(Int), Int >= 0 -> {ok, Int};
        _ -> {error, invalid_integer}
    catch
        _:_ -> {error, invalid_integer}
    end.

require_positive(Int) when Int > 0 -> ok;
require_positive(_Int) -> {error, invalid_integer}.

descriptor_bytes(Base, _Req, _Opts) when is_binary(Base) ->
    {ok, Base};
descriptor_bytes(Base, Req, Opts) ->
    case first_found(
        [
            {Req, <<"descriptor">>},
            {Req, <<"body">>},
            {Base, <<"descriptor">>},
            {Base, <<"body">>}
        ],
        Opts
    ) of
        Raw when is_binary(Raw) -> {ok, Raw};
        _ -> {error, descriptor_not_found}
    end.

sd_hash(Base, Req, Opts) ->
    case provided_sd_hash(Base, Req, Opts) of
        not_found -> {error, sd_hash_not_found};
        Hash ->
            Norm = normalize_hex(Hash),
            case require_sha384_hex(Norm, <<"sd-hash">>) of
                ok -> {ok, Norm};
                Error -> Error
            end
    end.

provided_sd_hash(Base, Req, Opts) ->
    first_found(
        [
            {Req, <<"sd-hash">>},
            {Req, <<"sd_hash">>},
            {Base, <<"sd-hash">>},
            {Base, <<"sd_hash">>}
        ],
        Opts
    ).

first_found([], _Opts) ->
    not_found;
first_found([{Msg, Key} | Rest], Opts) when is_map(Msg) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> first_found(Rest, Opts);
        Value -> Value
    end;
first_found([_ | Rest], Opts) ->
    first_found(Rest, Opts).

get_blob_bytes(Hash, Base, Req, Opts) ->
    NormHash = normalize_hex(Hash),
    Blobs =
        first_found(
            [
                {Req, <<"encrypted-blobs">>},
                {Req, <<"blobs">>},
                {Base, <<"encrypted-blobs">>},
                {Base, <<"blobs">>}
            ],
            Opts
        ),
    case Blobs of
        Map when is_map(Map) ->
            case hb_maps:get(NormHash, Map, not_found, Opts) of
                Bytes when is_binary(Bytes) -> {ok, Bytes};
                _ -> maybe_fetch_blob(NormHash, Base, Req, Opts)
            end;
        _ ->
            maybe_fetch_blob(NormHash, Base, Req, Opts)
    end.

maybe_fetch_blob(Hash, Base, Req, Opts) ->
    case read_cached_blob(Hash, Base, Req, Opts) of
        {ok, _Bytes} = Cached ->
            Cached;
        _ ->
            case hb_maps:get(<<"fetch-blobs">>, Req, false, Opts) of
                true -> fetch_blob(Hash, Base, Req, Opts);
                _ -> {error, not_found}
            end
    end.

fetch_blob(Hash, Base, Req, Opts) ->
    NormHash = normalize_hex(Hash),
    case read_cached_blob(NormHash, Base, Req, Opts) of
        {ok, _Bytes} = Cached ->
            Cached;
        _ ->
            case read_store_blob(NormHash, Base, Req, Opts) of
                {ok, Bytes} ->
                    write_cached_blob(NormHash, Bytes, Base, Req, Opts),
                    {ok, Bytes};
                _ ->
                    case read_local_blob(NormHash, Base, Req, Opts) of
                        {ok, Bytes} ->
                            write_cached_blob(NormHash, Bytes, Base, Req, Opts),
                            {ok, Bytes};
                        _ ->
                            fetch_missing_blob(NormHash, Base, Req, Opts)
                    end
            end
    end.

read_store_blob(Hash, Base, Req, Opts) ->
    case store_blob_enabled(Base, Req, Opts) of
        true ->
            case hb_cache:read(<<"odysee/blob/", Hash/binary>>, Opts) of
                {ok, Msg} when is_map(Msg) ->
                    case stored_blob_bytes(Msg, Opts) of
                        Body when is_binary(Body) -> verify_fetched_blob(Hash, Body);
                        _ -> {error, not_found}
                    end;
                {ok, Body} when is_binary(Body) ->
                    verify_fetched_blob(Hash, Body);
                _ ->
                    {error, not_found}
            end;
        false ->
            {error, not_found}
    end.

stored_blob_bytes(Msg, Opts) ->
    case hb_maps:get(<<"body">>, Msg, not_found, Opts) of
        Body when is_binary(Body) -> Body;
        _ -> hb_maps:get(<<"data">>, Msg, not_found, Opts)
    end.

store_blob_enabled(Base, Req, Opts) ->
    case first_found(
        [
            {Req, <<"use-store-blobs">>},
            {Req, <<"use_store_blobs">>},
            {Base, <<"use-store-blobs">>},
            {Base, <<"use_store_blobs">>}
        ],
        Opts
    ) of
        not_found ->
            hb_opts:get(store, no_viable_store, Opts) =/= no_viable_store;
        Value ->
            truthy(Value)
    end.

fetch_missing_blob(Hash, Base, Req, Opts) ->
    case fetch_lbrynet_blob(Hash, Base, Req, Opts) of
        {ok, _Bytes} = OK ->
            OK;
        _ ->
            fetch_remote_blob(
                Hash,
                blob_urls(Hash, Base, Req, Opts),
                Base,
                Req,
                Opts,
                []
            )
    end.

fetch_lbrynet_blob(Hash, Base, Req, Opts) ->
    fetch_lbrynet_blob(Hash, lbrynet_api_urls(Base, Req, Opts), Base, Req, Opts, []).

fetch_lbrynet_blob(_Hash, [], _Base, _Req, _Opts, Errors) ->
    {error, {lbrynet_fetch_failed, lists:reverse(Errors)}};
fetch_lbrynet_blob(Hash, [URL | Rest], Base, Req, Opts, Errors) ->
    case fetch_lbrynet_blob_url(Hash, URL, Base, Req, Opts) of
        {ok, Body} ->
            write_cached_blob(Hash, Body, Base, Req, Opts),
            {ok, Body};
        Error ->
            fetch_lbrynet_blob(Hash, Rest, Base, Req, Opts, [{URL, Error} | Errors])
    end.

fetch_lbrynet_blob_url(Hash, URL, Base, Req, Opts) ->
    Body = hb_json:encode(#{
        <<"method">> => <<"blob_get">>,
        <<"params">> => #{
            <<"blob_hash">> => Hash,
            <<"timeout">> => lbrynet_timeout(Base, Req, Opts)
        }
    }),
    Msg = #{
        <<"method">> => <<"POST">>,
        <<"path">> => URL,
        <<"content-type">> => <<"application/json">>,
        <<"body">> => Body
    },
    case hb_http:request(Msg, lbrynet_http_opts(Base, Req, Opts)) of
        {ok, #{ <<"status">> := Status, <<"body">> := ResBody }}
                when is_integer(Status), Status >= 200, Status < 300, is_binary(ResBody) ->
            read_lbrynet_blob_response(Hash, ResBody, Base, Req, Opts);
        {ok, #{ <<"status">> := Status }} when is_integer(Status) ->
            {error, {http_status, Status}};
        Error ->
            Error
    end.

read_lbrynet_blob_response(Hash, Body, Base, Req, Opts) ->
    maybe
        {ok, Decoded} ?= try_decode_json(Body),
        ok ?= lbrynet_response_ok(Decoded, Opts),
        read_local_blob(Hash, Base, Req, Opts)
    end.

lbrynet_response_ok(Decoded, Opts) when is_map(Decoded) ->
    case hb_maps:get(<<"error">>, Decoded, not_found, Opts) of
        not_found -> ok;
        null -> ok;
        Error -> {error, {lbrynet_error, Error}}
    end;
lbrynet_response_ok(_Decoded, _Opts) ->
    {error, invalid_lbrynet_response}.

get_plain_blob(Blob, Key, Base, Req, Opts) ->
    get_plain_blob(Blob, Key, not_found, Base, Req, Opts).

get_plain_blob(Blob, Key, MediaOffset, Base, Req, Opts) ->
    Hash = hb_maps:get(<<"blob-hash">>, Blob, not_found, Opts),
    Path = plain_blob_cache_path(Key, Hash),
    case read_cached_bytes(Path, Base, Req, Opts, plain) of
        {ok, _Plain} = Cached ->
            Cached;
        _ ->
            maybe_warmup_missing_blob(Hash, MediaOffset, Base, Req, Opts),
            decrypt_plain_blob(Blob, Key, Path, Base, Req, Opts)
    end.

decrypt_plain_blob(Blob, Key, Path, Base, Req, Opts) ->
    maybe
        Hash = hb_maps:get(<<"blob-hash">>, Blob, not_found, Opts),
        {ok, Bytes} ?= get_blob_bytes(Hash, Base, Req, Opts),
        ok ?= verify_blob_bytes(Blob, Bytes),
        {ok, IV} ?=
            decode_hex(
                <<"iv">>,
                hb_maps:get(<<"iv">>, Blob, not_found, Opts),
                16
            ),
        {ok, Plain} ?= decrypt_blob(Bytes, Key, IV),
        write_cached_bytes(Path, Plain, Base, Req, Opts, plain),
        {ok, Plain}
    end.

maybe_warmup_missing_blob(_Hash, not_found, _Base, _Req, _Opts) ->
    ok;
maybe_warmup_missing_blob(Hash, MediaOffset, Base, Req, Opts)
        when is_integer(MediaOffset) ->
    case encrypted_blob_available(Hash, Base, Req, Opts) of
        true -> ok;
        false -> warmup_lbrynet_stream(Hash, MediaOffset, Base, Req, Opts)
    end;
maybe_warmup_missing_blob(_Hash, _MediaOffset, _Base, _Req, _Opts) ->
    ok.

encrypted_blob_available(Hash, Base, Req, Opts) ->
    NormHash = normalize_hex(Hash),
    Blobs =
        first_found(
            [
                {Req, <<"encrypted-blobs">>},
                {Req, <<"blobs">>},
                {Base, <<"encrypted-blobs">>},
                {Base, <<"blobs">>}
            ],
            Opts
        ),
    case Blobs of
        Map when is_map(Map) ->
            case hb_maps:get(NormHash, Map, not_found, Opts) of
                Bytes when is_binary(Bytes) -> true;
                _ -> encrypted_blob_file_available(NormHash, Base, Req, Opts)
            end;
        _ ->
            encrypted_blob_file_available(NormHash, Base, Req, Opts)
    end.

encrypted_blob_file_available(Hash, Base, Req, Opts) ->
    case read_cached_blob(Hash, Base, Req, Opts) of
        {ok, _Bytes} -> true;
        _ ->
            case read_local_blob(Hash, Base, Req, Opts) of
                {ok, _Bytes} -> true;
                _ -> false
            end
    end.

warmup_lbrynet_stream(Hash, MediaOffset, Base, Req, Opts) ->
    warmup_lbrynet_stream(Hash, MediaOffset, lbrynet_stream_urls(Base, Req, Opts), Base, Req, Opts).

warmup_lbrynet_stream(_Hash, _MediaOffset, [], _Base, _Req, _Opts) ->
    ok;
warmup_lbrynet_stream(Hash, MediaOffset, [URL | Rest], Base, Req, Opts) ->
    Range =
        <<
            "bytes=",
            (integer_to_binary(MediaOffset))/binary,
            "-",
            (integer_to_binary(MediaOffset))/binary
        >>,
    Msg = #{
        <<"method">> => <<"GET">>,
        <<"path">> => URL,
        <<"range">> => Range,
        <<"accept">> => <<"video/*,*/*">>
    },
    case hb_http:request(Msg, lbrynet_http_opts(Base, Req, Opts)) of
        {ok, #{ <<"status">> := Status }} when is_integer(Status), Status >= 200, Status < 300 ->
            ok;
        _ ->
            warmup_lbrynet_stream(Hash, MediaOffset, Rest, Base, Req, Opts)
    end.

read_local_blob(Hash, Base, Req, Opts) ->
    read_local_blob(Hash, blob_dirs(Base, Req, Opts), []).

read_local_blob(_Hash, [], Errors) ->
    {error, {local_blob_not_found, lists:reverse(Errors)}};
read_local_blob(Hash, [Dir | Rest], Errors) when is_binary(Dir) ->
    Path = filename:join(binary_to_list(Dir), binary_to_list(Hash)),
    case file:read_file(Path) of
        {ok, Body} ->
            verify_fetched_blob(Hash, Body);
        {error, Reason} ->
            read_local_blob(Hash, Rest, [{Dir, Reason} | Errors])
    end;
read_local_blob(Hash, [_Invalid | Rest], Errors) ->
    read_local_blob(Hash, Rest, [invalid_blob_dir | Errors]).

fetch_remote_blob(Hash, [], _Base, _Req, _Opts, Errors) ->
    {error, {blob_fetch_failed, Hash, lists:reverse(Errors)}};
fetch_remote_blob(Hash, [URL | Rest], Base, Req, Opts, Errors) ->
    case fetch_blob_url(Hash, URL, Base, Req, Opts) of
        {ok, Body} ->
            write_cached_blob(Hash, Body, Base, Req, Opts),
            {ok, Body};
        Error ->
            fetch_remote_blob(Hash, Rest, Base, Req, Opts, [{URL, Error} | Errors])
    end.

fetch_blob_url(Hash, URL, Base, Req, Opts) ->
    case
        hb_http:request(
            #{ <<"path">> => URL, <<"method">> => <<"GET">> },
            lbry_http_opts(Base, Req, Opts)
        )
    of
        {ok, #{ <<"status">> := Status, <<"body">> := Body }}
                when is_integer(Status), Status >= 200, Status < 300, is_binary(Body) ->
            verify_fetched_blob(Hash, Body);
        {ok, #{ <<"status">> := Status }} when is_integer(Status) ->
            {error, {http_status, Status}};
        {ok, #{ <<"body">> := Body }} when is_binary(Body) ->
            verify_fetched_blob(Hash, Body);
        {ok, Body} when is_binary(Body) ->
            verify_fetched_blob(Hash, Body);
        Error ->
            Error
    end.

verify_fetched_blob(Hash, Body) ->
    case sha384_hex(Body) of
        Hash -> {ok, Body};
        Other -> {error, {fetched_blob_hash_mismatch, Hash, Other}}
    end.

blob_urls(Hash, Base, Req, Opts) ->
    TemplateURLs =
        [
            expand_blob_template(Template, Hash)
        ||
            Template <- blob_url_templates(Base, Req, Opts),
            is_binary(Template)
        ],
    BaseURLs = [
        blob_url(URL, Hash)
    ||
        URL <- blob_base_urls(Base, Req, Opts),
        is_binary(URL),
        byte_size(URL) > 0
    ],
    TemplateURLs ++ BaseURLs.

blob_base_urls(Base, Req, Opts) ->
    Values =
        values_from(
            [
                {Req, <<"blob-base-url">>},
                {Req, <<"blob_base_url">>},
                {Req, <<"blob-base-urls">>},
                {Req, <<"blob_base_urls">>},
                {Req, <<"reflector-url">>},
                {Req, <<"reflector_url">>},
                {Req, <<"reflector-urls">>},
                {Req, <<"reflector_urls">>},
                {Base, <<"blob-base-url">>},
                {Base, <<"blob_base_url">>},
                {Base, <<"blob-base-urls">>},
                {Base, <<"blob_base_urls">>},
                {Base, <<"reflector-url">>},
                {Base, <<"reflector_url">>},
                {Base, <<"reflector-urls">>},
                {Base, <<"reflector_urls">>}
            ],
            Opts
        ),
    case Values of
        [] ->
            opt_values(
                [<<"lbry-blob-base-urls">>, <<"lbry-blob-base-url">>],
                ?DEFAULT_BLOB_BASE_URLS,
                Opts
            );
        _ ->
            Values
    end.

blob_url_templates(Base, Req, Opts) ->
    values_from(
        [
            {Req, <<"blob-url-template">>},
            {Req, <<"blob_url_template">>},
            {Req, <<"blob-url-templates">>},
            {Req, <<"blob_url_templates">>},
            {Base, <<"blob-url-template">>},
            {Base, <<"blob_url_template">>},
            {Base, <<"blob-url-templates">>},
            {Base, <<"blob_url_templates">>}
        ],
        Opts
    )
        ++ opt_values(
            [<<"lbry-blob-url-templates">>, <<"lbry-blob-url-template">>],
            [],
            Opts
        ).

lbrynet_api_urls(Base, Req, Opts) ->
    values_from(
        [
            {Req, <<"lbrynet-api-url">>},
            {Req, <<"lbrynet_api_url">>},
            {Req, <<"lbrynet-api-urls">>},
            {Req, <<"lbrynet_api_urls">>},
            {Base, <<"lbrynet-api-url">>},
            {Base, <<"lbrynet_api_url">>},
            {Base, <<"lbrynet-api-urls">>},
            {Base, <<"lbrynet_api_urls">>}
        ],
        Opts
    )
        ++ opt_values(
            [<<"lbry-lbrynet-api-urls">>, <<"lbry-lbrynet-api-url">>],
            [],
            Opts
        ).

lbrynet_stream_urls(Base, Req, Opts) ->
    Explicit =
        values_from(
            [
                {Req, <<"lbrynet-stream-url">>},
                {Req, <<"lbrynet_stream_url">>},
                {Req, <<"lbrynet-media-url">>},
                {Req, <<"lbrynet_media_url">>},
                {Base, <<"lbrynet-stream-url">>},
                {Base, <<"lbrynet_stream_url">>},
                {Base, <<"lbrynet-media-url">>},
                {Base, <<"lbrynet_media_url">>}
            ],
            Opts
        )
            ++ opt_values(
                [<<"lbry-lbrynet-stream-urls">>, <<"lbry-lbrynet-stream-url">>],
                [],
                Opts
            ),
    BaseURLs =
        values_from(
            [
                {Req, <<"lbrynet-stream-base-url">>},
                {Req, <<"lbrynet_stream_base_url">>},
                {Req, <<"lbrynet-media-base-url">>},
                {Req, <<"lbrynet_media_base_url">>},
                {Base, <<"lbrynet-stream-base-url">>},
                {Base, <<"lbrynet_stream_base_url">>},
                {Base, <<"lbrynet-media-base-url">>},
                {Base, <<"lbrynet_media_base_url">>}
            ],
            Opts
        )
            ++ opt_values(
                [
                    <<"lbry-lbrynet-stream-base-urls">>,
                    <<"lbry-lbrynet-stream-base-url">>
                ],
                [],
                Opts
            ),
    Explicit ++
        [
            lbrynet_stream_url(URL, Base, Req, Opts)
        ||
            URL <- BaseURLs,
            is_binary(URL),
            byte_size(URL) > 0
        ].

lbrynet_stream_url(BaseURL, Base, Req, Opts) ->
    {ok, SDHash} = sd_hash(Base, Req, Opts),
    CleanBaseURL =
        case binary:at(BaseURL, byte_size(BaseURL) - 1) of
            $/ -> binary:part(BaseURL, 0, byte_size(BaseURL) - 1);
            _ -> BaseURL
        end,
    <<CleanBaseURL/binary, "/", SDHash/binary>>.

blob_dirs(Base, Req, Opts) ->
    values_from(
        [
            {Req, <<"blob-dir">>},
            {Req, <<"blob_dir">>},
            {Req, <<"blob-dirs">>},
            {Req, <<"blob_dirs">>},
            {Req, <<"blob-directory">>},
            {Req, <<"blob_directory">>},
            {Base, <<"blob-dir">>},
            {Base, <<"blob_dir">>},
            {Base, <<"blob-dirs">>},
            {Base, <<"blob_dirs">>},
            {Base, <<"blob-directory">>},
            {Base, <<"blob_directory">>}
        ],
        Opts
    )
        ++ opt_values(
            [<<"lbry-blob-dirs">>, <<"lbry-blob-dir">>],
            [],
            Opts
        ).

blob_url(BaseURL, Hash) when is_binary(BaseURL), byte_size(BaseURL) > 0 ->
    CleanBaseURL =
        case binary:at(BaseURL, byte_size(BaseURL) - 1) of
            $/ -> binary:part(BaseURL, 0, byte_size(BaseURL) - 1);
            _ -> BaseURL
        end,
    <<CleanBaseURL/binary, "/blob?hash=", Hash/binary>>;
blob_url(_BaseURL, _Hash) ->
    throw({error, invalid_blob_base_url}).

expand_blob_template(Template, Hash) ->
    binary:replace(Template, <<"{hash}">>, Hash, [global]).

read_cached_blob(Hash, Base, Req, Opts) ->
    read_cached_bytes(blob_cache_path(Hash), Base, Req, Opts, encrypted).

write_cached_blob(Hash, Bytes, Base, Req, Opts) ->
    write_cached_bytes(blob_cache_path(Hash), Bytes, Base, Req, Opts, encrypted).

read_cached_bytes(Path, Base, Req, Opts, Kind) ->
    case cache_enabled(Kind, Base, Req, Opts) of
        true ->
            try hb_cache:read(Path, cache_opts(Opts)) of
                {ok, Bytes} when is_binary(Bytes) -> {ok, Bytes};
                _ -> {error, not_found}
            catch
                _:_ -> {error, not_found}
            end;
        false ->
            {error, not_found}
    end.

write_cached_bytes(Path, Bytes, Base, Req, Opts, Kind) ->
    case cache_enabled(Kind, Base, Req, Opts) of
        true ->
            try hb_cache:write_binary(Path, Bytes, cache_opts(Opts)) of
                _ -> ok
            catch
                _:_ -> ok
            end;
        false ->
            ok
    end.

cache_enabled(plain, Base, Req, Opts) ->
    cache_flag(
        [
            {Req, <<"plain-cache-blobs">>},
            {Req, <<"plain_cache_blobs">>},
            {Base, <<"plain-cache-blobs">>},
            {Base, <<"plain_cache_blobs">>}
        ],
        <<"lbry-plain-blob-cache">>,
        true,
        Opts
    );
cache_enabled(_Kind, Base, Req, Opts) ->
    cache_flag(
        [
            {Req, <<"cache-blobs">>},
            {Req, <<"cache_blobs">>},
            {Req, <<"blob-cache">>},
            {Req, <<"blob_cache">>},
            {Base, <<"cache-blobs">>},
            {Base, <<"cache_blobs">>},
            {Base, <<"blob-cache">>},
            {Base, <<"blob_cache">>}
        ],
        <<"lbry-blob-cache">>,
        true,
        Opts
    ).

cache_flag(Sources, OptKey, Default, Opts) ->
    Value =
        case first_found(Sources, Opts) of
            not_found -> hb_opts:get(OptKey, Default, Opts);
            Found -> Found
        end,
    not falsy(Value).

cache_opts(Opts) ->
    Opts#{ <<"cache-read-mode">> => raw }.

blob_cache_path(Hash) ->
    <<"lbry/blob/", Hash/binary>>.

plain_blob_cache_path(Key, Hash) ->
    <<"lbry/plain/", (sha384_hex(<<Key/binary, Hash/binary>>))/binary>>.

lbry_http_opts(Base, Req, Opts) ->
    Opts#{
        <<"http-client-connect-timeout">> =>
            timeout_value(
                <<"blob-connect-timeout">>,
                <<"lbry-blob-connect-timeout">>,
                ?DEFAULT_BLOB_CONNECT_TIMEOUT,
                Base,
                Req,
                Opts
            ),
        <<"http-client-hackney-recv-timeout">> =>
            timeout_value(
                <<"blob-recv-timeout">>,
                <<"lbry-blob-recv-timeout">>,
                ?DEFAULT_BLOB_RECV_TIMEOUT,
                Base,
                Req,
                Opts
            ),
        <<"http-client-hackney-checkout-timeout">> =>
            timeout_value(
                <<"blob-checkout-timeout">>,
                <<"lbry-blob-checkout-timeout">>,
                ?DEFAULT_BLOB_CHECKOUT_TIMEOUT,
                Base,
                Req,
                Opts
            )
    }.

lbrynet_http_opts(Base, Req, Opts) ->
    Opts#{
        <<"http-client-connect-timeout">> =>
            timeout_value(
                <<"lbrynet-connect-timeout">>,
                <<"lbry-lbrynet-connect-timeout">>,
                ?DEFAULT_BLOB_CONNECT_TIMEOUT,
                Base,
                Req,
                Opts
            ),
        <<"http-client-hackney-recv-timeout">> =>
            timeout_value(
                <<"lbrynet-recv-timeout">>,
                <<"lbry-lbrynet-recv-timeout">>,
                lbrynet_timeout(Base, Req, Opts) * 1000 + 5000,
                Base,
                Req,
                Opts
            ),
        <<"http-client-hackney-checkout-timeout">> =>
            timeout_value(
                <<"lbrynet-checkout-timeout">>,
                <<"lbry-lbrynet-checkout-timeout">>,
                ?DEFAULT_BLOB_CHECKOUT_TIMEOUT,
                Base,
                Req,
                Opts
            )
    }.

lbrynet_timeout(Base, Req, Opts) ->
    timeout_value(
        <<"lbrynet-timeout">>,
        <<"lbry-lbrynet-timeout">>,
        ?DEFAULT_LBRYNET_TIMEOUT,
        Base,
        Req,
        Opts
    ).

timeout_value(Key, OptKey, Default, Base, Req, Opts) ->
    case first_found([{Req, Key}, {Base, Key}], Opts) of
        not_found -> hb_util:int(hb_opts:get(OptKey, Default, Opts));
        Value -> hb_util:int(Value)
    end.

values_from(Sources, Opts) ->
    lists:append(
        [
            value_list(Value)
        ||
            {Msg, Key} <- Sources,
            is_map(Msg),
            Value <- [hb_maps:get(Key, Msg, not_found, Opts)],
            Value =/= not_found
        ]
    ).

opt_values([], Default, _Opts) ->
    Default;
opt_values([Key | Rest], Default, Opts) ->
    case hb_opts:get(Key, not_found, Opts) of
        not_found -> opt_values(Rest, Default, Opts);
        Value -> value_list(Value)
    end.

value_list(not_found) ->
    [];
value_list(Value) when is_binary(Value) ->
    case binary:split(Value, <<",">>, [global]) of
        [Value] -> [Value];
        Parts -> [Part || Part <- Parts, Part =/= <<>>]
    end;
value_list(Values) when is_list(Values) ->
    Values;
value_list(Value) ->
    [Value].

decode_hex_value(Name, Hex) ->
    maybe
        Norm = normalize_hex(Hex),
        {ok, Bin} ?= decode_hex(Name, Norm),
        {ok, Norm, Bin}
    end.

decode_hex_value(Name, Hex, Bytes) ->
    maybe
        Norm = normalize_hex(Hex),
        {ok, Bin} ?= decode_hex(Name, Norm, Bytes),
        {ok, Norm, Bin}
    end.

decode_hex(Name, Hex) when is_binary(Hex) ->
    try {ok, binary:decode_hex(Hex)}
    catch _:_ -> {error, {invalid_hex, Name}}
    end;
decode_hex(Name, _Hex) ->
    {error, {invalid_hex, Name}}.

decode_hex(Name, Hex, Bytes) ->
    case byte_size(Hex) of
        Size when Size =:= Bytes * 2 ->
            decode_hex(Name, Hex);
        _ ->
            {error, {invalid_hex_size, Name}}
    end.

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:to_lower(Hex);
normalize_hex(Hex) ->
    throw({invalid_hex, Hex}).

validate_provided_sd_hash(not_found, _SDHash) -> ok;
validate_provided_sd_hash(Provided, SDHash) ->
    case normalize_hex(Provided) of
        SDHash -> ok;
        _ -> {error, sd_hash_mismatch}
    end.

calculate_stream_hash(StreamNameHex, KeyHex, SuggestedHex, Blobs) ->
    BlobSums =
        iolist_to_binary(
            lists:map(fun blob_hashsum/1, Blobs)
        ),
    BlobDigest = crypto:hash(sha384, BlobSums),
    sha384_hex(
        <<
            StreamNameHex/binary,
            KeyHex/binary,
            SuggestedHex/binary,
            BlobDigest/binary
        >>
    ).

blob_hashsum(Blob) ->
    Length = hb_maps:get(<<"length">>, Blob, #{}),
    BlobNum = hb_maps:get(<<"blob-num">>, Blob, #{}),
    IV = hb_maps:get(<<"iv">>, Blob, #{}),
    HashPrefix =
        case hb_maps:get(<<"blob-hash">>, Blob, not_found, #{}) of
            not_found -> <<>>;
            Hash -> Hash
        end,
    crypto:hash(
        sha384,
        <<
            HashPrefix/binary,
            (integer_to_binary(BlobNum))/binary,
            IV/binary,
            (integer_to_binary(Length))/binary
        >>
    ).

sha384_hex(Bin) ->
    hb_util:to_hex(crypto:hash(sha384, Bin)).

-ifdef(TEST).

decode_and_reconstruct_test() ->
    {JSON, Encrypted, Plain, BlobHash} = test_descriptor(),
    {ok, Desc} = decode(#{}, #{ <<"body">> => JSON }, #{}),
    ?assertEqual(JSON, hb_maps:get(<<"body">>, Desc, #{})),
    ?assertEqual(
        <<"odysee/descriptor/", (hb_maps:get(<<"sd-hash">>, Desc, #{}))/binary>>,
        hb_maps:get(<<"descriptor-store-path">>, Desc, #{})
    ),
    ?assertEqual(
        [<<"odysee/blob/", BlobHash/binary>>],
        hb_util:message_to_ordered_list(hb_maps:get(<<"blob-store-paths">>, Desc, #{}), #{})
    ),
    Verified =
        hb_util:ok(verify(
            Desc,
            #{ <<"blobs">> => #{ BlobHash => Encrypted } },
            #{}
        )),
    ?assertEqual(BlobHash, hd(hb_maps:get(<<"verified-blobs">>, Verified, #{}))),
    {ok, Reconstructed} =
        reconstruct(
            Desc,
            #{ <<"blobs">> => #{ BlobHash => Encrypted } },
            #{}
        ),
    ?assertEqual(Plain, hb_maps:get(<<"body">>, Reconstructed, #{})).

decode_rejects_out_of_order_blobs_test() ->
    {JSON, _Encrypted, _Plain, _BlobHash} = test_descriptor(1),
    ?assertMatch(
        {error, _},
        decode(#{}, #{ <<"body">> => JSON }, #{})
    ).

verify_reports_missing_blobs_test() ->
    {JSON, _Encrypted, _Plain, BlobHash} = test_descriptor(),
    {ok, Desc} = decode(#{}, #{ <<"body">> => JSON }, #{}),
    {ok, Verified} = verify(Desc, #{}, #{}),
    ?assertEqual(false, hb_maps:get(<<"verified">>, Verified, #{})),
    ?assertEqual([BlobHash], hb_maps:get(<<"missing-blobs">>, Verified, #{})).

verify_reads_encrypted_blob_from_store_test() ->
    {JSON, Encrypted, _Plain, BlobHash} = test_descriptor(),
    {ok, Desc} = decode(#{}, #{ <<"body">> => JSON }, #{}),
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/blob/", BlobHash/binary>> => #{
                <<"device">> => <<"odysee-blob@1.0">>,
                <<"content-type">> => <<"application/octet-stream">>,
                <<"body">> => Encrypted,
                <<"blob-hash">> => BlobHash,
                <<"blob-store-path">> => <<"odysee/blob/", BlobHash/binary>>
            }
        }
    },
    {ok, Verified} =
        verify(
            Desc,
            #{ <<"fetch-blobs">> => true, <<"cache-blobs">> => false },
            #{ <<"store">> => Store }
        ),
    ?assertEqual(true, hb_maps:get(<<"verified">>, Verified, #{})),
    ?assertEqual([BlobHash], hb_maps:get(<<"verified-blobs">>, Verified, #{})).

media_head_returns_range_metadata_test() ->
    {JSON, Encrypted, Plain, BlobHash} = test_descriptor(),
    {ok, Head} =
        media(
            #{},
            #{
                <<"body">> => JSON,
                <<"blobs">> => #{ BlobHash => Encrypted },
                <<"media-size">> => byte_size(Plain),
                <<"media-type">> => <<"video/mp4">>,
                <<"method">> => <<"HEAD">>
            },
            #{}
        ),
    ?assertEqual(<<"video/mp4">>, hb_maps:get(<<"content-type">>, Head, #{})),
    ?assertEqual(byte_size(Plain), hb_maps:get(<<"content-length">>, Head, #{})),
    ?assertEqual(<<"bytes">>, hb_maps:get(<<"accept-ranges">>, Head, #{})),
    ?assertEqual(<<>>, hb_maps:get(<<"body">>, Head, #{})).

media_get_returns_explicit_range_test() ->
    {JSON, Encrypted, Plain, BlobHash} = test_descriptor(),
    {ok, Range} =
        media(
            #{},
            #{
                <<"body">> => JSON,
                <<"blobs">> => #{ BlobHash => Encrypted },
                <<"media-size">> => byte_size(Plain),
                <<"range">> => <<"bytes=6-9">>
            },
            #{}
        ),
    ?assertEqual(206, hb_maps:get(<<"status">>, Range, #{})),
    ?assertEqual(<<"bytes 6-9/31">>, hb_maps:get(<<"content-range">>, Range, #{})),
    ?assertEqual(binary:part(Plain, 6, 4), hb_maps:get(<<"body">>, Range, #{})).

media_get_accepts_capital_range_test() ->
    {JSON, Encrypted, Plain, BlobHash} = test_descriptor(),
    {ok, Range} =
        media(
            #{},
            #{
                <<"body">> => JSON,
                <<"blobs">> => #{ BlobHash => Encrypted },
                <<"media-size">> => byte_size(Plain),
                <<"Range">> => <<"bytes=6-9">>
            },
            #{}
        ),
    ?assertEqual(206, hb_maps:get(<<"status">>, Range, #{})),
    ?assertEqual(binary:part(Plain, 6, 4), hb_maps:get(<<"body">>, Range, #{})).

media_options_preflight_test() ->
    {JSON, _Encrypted, Plain, _BlobHash} = test_descriptor(),
    {ok, Res} =
        media(
            #{},
            #{
                <<"body">> => JSON,
                <<"media-size">> => byte_size(Plain),
                <<"method">> => <<"OPTIONS">>
            },
            #{}
        ),
    ?assertEqual(204, hb_maps:get(<<"status">>, Res, #{})),
    ?assertEqual(<<"*">>, hb_maps:get(<<"access-control-allow-origin">>, Res, #{})),
    ?assertEqual(<<>>, hb_maps:get(<<"body">>, Res, #{})).

media_get_caps_open_ended_range_test() ->
    {JSON, Encrypted, Plain, BlobHash} = test_descriptor(),
    {ok, Range} =
        media(
            #{},
            #{
                <<"body">> => JSON,
                <<"blobs">> => #{ BlobHash => Encrypted },
                <<"media-size">> => byte_size(Plain),
                <<"range">> => <<"bytes=0-">>,
                <<"chunk-size">> => 5
            },
            #{}
        ),
    ?assertEqual(206, hb_maps:get(<<"status">>, Range, #{})),
    ?assertEqual(<<"bytes 0-4/31">>, hb_maps:get(<<"content-range">>, Range, #{})),
    ?assertEqual(binary:part(Plain, 0, 5), hb_maps:get(<<"body">>, Range, #{})).

media_get_requires_range_for_large_media_test() ->
    {JSON, Encrypted, _Plain, BlobHash} = test_descriptor(),
    {ok, Res} =
        media(
            #{},
            #{
                <<"body">> => JSON,
                <<"blobs">> => #{ BlobHash => Encrypted },
                <<"media-size">> => ?DEFAULT_FULL_RESPONSE_LIMIT + 1
            },
            #{}
        ),
    ?assertEqual(416, hb_maps:get(<<"status">>, Res, #{})),
    ?assertEqual(
        <<"bytes */8388609">>,
        hb_maps:get(<<"content-range">>, Res, #{})
    ).

media_get_reads_local_blob_dir_test() ->
    {JSON, Encrypted, Plain, BlobHash} = test_descriptor(),
    Dir = test_tmp_dir(),
    BlobPath = filename:join(Dir, binary_to_list(BlobHash)),
    try
        ok = file:make_dir(Dir),
        ok = file:write_file(BlobPath, Encrypted),
        {ok, Range} =
            media(
                #{},
                #{
                    <<"body">> => JSON,
                    <<"blob-dir">> => list_to_binary(Dir),
                    <<"fetch-blobs">> => true,
                    <<"media-size">> => byte_size(Plain),
                    <<"range">> => <<"bytes=0-4">>
                },
                #{}
            ),
        ?assertEqual(206, hb_maps:get(<<"status">>, Range, #{})),
        ?assertEqual(binary:part(Plain, 0, 5), hb_maps:get(<<"body">>, Range, #{}))
    after
        file:delete(BlobPath),
        file:del_dir(Dir)
    end.

media_get_skips_blobs_before_range_test() ->
    {JSON, _Encrypted1, Encrypted2, Plain1, Plain2, _Hash1, Hash2} =
        two_blob_test_descriptor(),
    Start = byte_size(Plain1),
    End = Start + 4,
    Range = <<"bytes=", (integer_to_binary(Start))/binary, "-", (integer_to_binary(End))/binary>>,
    {ok, Res} =
        media(
            #{},
            #{
                <<"body">> => JSON,
                <<"blobs">> => #{ Hash2 => Encrypted2 },
                <<"media-size">> => byte_size(Plain1) + byte_size(Plain2),
                <<"range">> => Range
            },
            #{}
        ),
    ?assertEqual(206, hb_maps:get(<<"status">>, Res, #{})),
    ?assertEqual(binary:part(Plain2, 0, 5), hb_maps:get(<<"body">>, Res, #{})).

media_fetches_missing_blob_from_lbrynet_api_test() ->
    {JSON, Encrypted, Plain, BlobHash} = test_descriptor(),
    Dir = test_tmp_dir(),
    BlobPath = filename:join(Dir, binary_to_list(BlobHash)),
    Response = hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"error">> => null,
        <<"result">> => <<"Downloaded blob">>,
        <<"id">> => 1
    }),
    try
        ok = file:make_dir(Dir),
        {ok, MockServer, ServerHandle} =
            hb_mock_server:start([
                {"/", lbrynet, fun(_Msg) ->
                    ok = file:write_file(BlobPath, Encrypted),
                    {200, Response}
                end}
            ]),
        try
            {ok, Range} =
                media(
                    #{},
                    #{
                        <<"body">> => JSON,
                        <<"blob-dir">> => list_to_binary(Dir),
                        <<"fetch-blobs">> => true,
                        <<"cache-blobs">> => false,
                        <<"plain-cache-blobs">> => false,
                        <<"lbrynet-api-url">> => MockServer,
                        <<"lbrynet-timeout">> => 5,
                        <<"media-size">> => byte_size(Plain),
                        <<"range">> => <<"bytes=0-4">>
                    },
                    #{}
                ),
            ?assertEqual(206, hb_maps:get(<<"status">>, Range, #{})),
            ?assertEqual(binary:part(Plain, 0, 5), hb_maps:get(<<"body">>, Range, #{})),
            [Request] = hb_mock_server:get_requests(lbrynet, 1, ServerHandle),
            Body = hb_json:decode(hb_maps:get(<<"body">>, Request, #{})),
            Params = hb_maps:get(<<"params">>, Body, #{}),
            ?assertEqual(<<"blob_get">>, hb_maps:get(<<"method">>, Body, #{})),
            ?assertEqual(not_found, hb_maps:get(<<"jsonrpc">>, Body, not_found, #{})),
            ?assertEqual(not_found, hb_maps:get(<<"id">>, Body, not_found, #{})),
            ?assertEqual(BlobHash, hb_maps:get(<<"blob_hash">>, Params, #{}))
        after
            hb_mock_server:stop(ServerHandle)
        end
    after
        file:delete(BlobPath),
        file:del_dir(Dir)
    end.

media_warms_missing_blob_from_lbrynet_stream_test() ->
    {JSON, Encrypted, Plain, BlobHash} = test_descriptor(),
    Dir = test_tmp_dir(),
    BlobPath = filename:join(Dir, binary_to_list(BlobHash)),
    try
        ok = file:make_dir(Dir),
        {ok, MockServer, ServerHandle} =
            hb_mock_server:start([
                {"/", lbrynet_stream, fun(_Msg) ->
                    ok = file:write_file(BlobPath, Encrypted),
                    {206, <<"warm">>}
                end}
            ]),
        try
            {ok, Range} =
                media(
                    #{},
                    #{
                        <<"body">> => JSON,
                        <<"blob-dir">> => list_to_binary(Dir),
                        <<"fetch-blobs">> => true,
                        <<"cache-blobs">> => false,
                        <<"plain-cache-blobs">> => false,
                        <<"lbrynet-stream-url">> => MockServer,
                        <<"media-size">> => byte_size(Plain),
                        <<"range">> => <<"bytes=0-4">>
                    },
                    #{}
                ),
            ?assertEqual(206, hb_maps:get(<<"status">>, Range, #{})),
            ?assertEqual(binary:part(Plain, 0, 5), hb_maps:get(<<"body">>, Range, #{})),
            [Request] = hb_mock_server:get_requests(lbrynet_stream, 1, ServerHandle),
            Headers = hb_maps:get(<<"headers">>, Request, #{}, #{}),
            ?assertEqual(<<"GET">>, hb_maps:get(<<"method">>, Request, #{})),
            ?assertEqual(<<"bytes=0-0">>, hb_maps:get(<<"range">>, Headers, #{}, #{}))
        after
            hb_mock_server:stop(ServerHandle)
        end
    after
        file:delete(BlobPath),
        file:del_dir(Dir)
    end.

verify_uses_cached_encrypted_blob_after_local_read_test() ->
    {JSON, Encrypted, _Plain, BlobHash} = test_descriptor(),
    {ok, Desc} = decode(#{}, #{ <<"body">> => JSON }, #{}),
    Dir = test_tmp_dir(),
    BlobPath = filename:join(Dir, binary_to_list(BlobHash)),
    try
        ok = file:make_dir(Dir),
        ok = file:write_file(BlobPath, Encrypted),
        {ok, Local} =
            verify(
                Desc,
                #{
                    <<"blob-dir">> => list_to_binary(Dir),
                    <<"fetch-blobs">> => true,
                    <<"cache-blobs">> => true
                },
                #{}
            ),
        ?assertEqual(true, hb_maps:get(<<"verified">>, Local, #{})),
        {ok, Cached} =
            verify(
                Desc,
                #{
                    <<"fetch-blobs">> => false,
                    <<"cache-blobs">> => true
                },
                #{}
            ),
        ?assertEqual(true, hb_maps:get(<<"verified">>, Cached, #{}))
    after
        file:delete(BlobPath),
        file:del_dir(Dir)
    end.

media_uses_plain_cache_without_encrypted_blob_test() ->
    {JSON, Encrypted, Plain, BlobHash} = test_descriptor(),
    Req = #{
        <<"body">> => JSON,
        <<"blobs">> => #{ BlobHash => Encrypted },
        <<"media-size">> => byte_size(Plain),
        <<"range">> => <<"bytes=0-4">>,
        <<"plain-cache-blobs">> => true
    },
    {ok, _Warm} = media(#{}, Req, #{}),
    {ok, Cached} =
        media(
            #{},
            (maps:remove(<<"blobs">>, Req))#{ <<"range">> => <<"bytes=6-9">> },
            #{}
        ),
    ?assertEqual(206, hb_maps:get(<<"status">>, Cached, #{})),
    ?assertEqual(binary:part(Plain, 6, 4), hb_maps:get(<<"body">>, Cached, #{})).

test_descriptor() ->
    test_descriptor(0).

test_tmp_dir() ->
    Root =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            Dir -> Dir
        end,
    filename:join(
        Root,
        "hb-lbry-" ++ integer_to_list(erlang:unique_integer([positive]))
    ).

test_descriptor(BlobNum) ->
    Plain = <<"hello from a legacy LBRY stream">>,
    Key = <<0:128>>,
    IV = <<1:128>>,
    TerminatorIV = <<2:128>>,
    Encrypted = encrypt_blob(Plain, Key, IV),
    BlobHash = sha384_hex(Encrypted),
    StreamNameHex = hb_util:to_hex(<<"test.mp4">>),
    SuggestedHex = hb_util:to_hex(<<"test.mp4">>),
    KeyHex = hb_util:to_hex(Key),
    IVHex = hb_util:to_hex(IV),
    TerminatorIVHex = hb_util:to_hex(TerminatorIV),
    Blobs = [
        #{
            <<"blob-num">> => BlobNum,
            <<"length">> => byte_size(Encrypted),
            <<"iv">> => IVHex,
            <<"blob-hash">> => BlobHash
        },
        #{
            <<"blob-num">> => 1,
            <<"length">> => 0,
            <<"iv">> => TerminatorIVHex
        }
    ],
    StreamHash = calculate_stream_hash(StreamNameHex, KeyHex, SuggestedHex, Blobs),
    JSON = hb_json:encode(#{
        <<"stream_type">> => <<"lbryfile">>,
        <<"stream_name">> => StreamNameHex,
        <<"key">> => KeyHex,
        <<"suggested_file_name">> => SuggestedHex,
        <<"stream_hash">> => StreamHash,
        <<"blobs">> => [
            #{
                <<"blob_num">> => BlobNum,
                <<"length">> => byte_size(Encrypted),
                <<"iv">> => IVHex,
                <<"blob_hash">> => BlobHash
            },
            #{
                <<"blob_num">> => 1,
                <<"length">> => 0,
                <<"iv">> => TerminatorIVHex
            }
        ]
    }),
    {JSON, Encrypted, Plain, BlobHash}.

two_blob_test_descriptor() ->
    Plain1 = <<"1234567890123456789012345678901">>,
    Plain2 = <<"second legacy LBRY stream blob">>,
    Key = <<0:128>>,
    IV1 = <<1:128>>,
    IV2 = <<2:128>>,
    TerminatorIV = <<3:128>>,
    Encrypted1 = encrypt_blob(Plain1, Key, IV1),
    Encrypted2 = encrypt_blob(Plain2, Key, IV2),
    Hash1 = sha384_hex(Encrypted1),
    Hash2 = sha384_hex(Encrypted2),
    StreamNameHex = hb_util:to_hex(<<"test.mp4">>),
    SuggestedHex = hb_util:to_hex(<<"test.mp4">>),
    KeyHex = hb_util:to_hex(Key),
    IVHex1 = hb_util:to_hex(IV1),
    IVHex2 = hb_util:to_hex(IV2),
    TerminatorIVHex = hb_util:to_hex(TerminatorIV),
    Blobs = [
        #{
            <<"blob-num">> => 0,
            <<"length">> => byte_size(Encrypted1),
            <<"iv">> => IVHex1,
            <<"blob-hash">> => Hash1
        },
        #{
            <<"blob-num">> => 1,
            <<"length">> => byte_size(Encrypted2),
            <<"iv">> => IVHex2,
            <<"blob-hash">> => Hash2
        },
        #{
            <<"blob-num">> => 2,
            <<"length">> => 0,
            <<"iv">> => TerminatorIVHex
        }
    ],
    StreamHash = calculate_stream_hash(StreamNameHex, KeyHex, SuggestedHex, Blobs),
    JSON = hb_json:encode(#{
        <<"stream_type">> => <<"lbryfile">>,
        <<"stream_name">> => StreamNameHex,
        <<"key">> => KeyHex,
        <<"suggested_file_name">> => SuggestedHex,
        <<"stream_hash">> => StreamHash,
        <<"blobs">> => [
            #{
                <<"blob_num">> => 0,
                <<"length">> => byte_size(Encrypted1),
                <<"iv">> => IVHex1,
                <<"blob_hash">> => Hash1
            },
            #{
                <<"blob_num">> => 1,
                <<"length">> => byte_size(Encrypted2),
                <<"iv">> => IVHex2,
                <<"blob_hash">> => Hash2
            },
            #{
                <<"blob_num">> => 2,
                <<"length">> => 0,
                <<"iv">> => TerminatorIVHex
            }
        ]
    }),
    {JSON, Encrypted1, Encrypted2, Plain1, Plain2, Hash1, Hash2}.

encrypt_blob(Plain, Key, IV) ->
    crypto:crypto_one_time(aes_128_cbc, Key, IV, add_pkcs7(Plain), true).

add_pkcs7(Bin) ->
    PadLen = ?AES_BLOCK_SIZE - (byte_size(Bin) rem ?AES_BLOCK_SIZE),
    <<Bin/binary, (binary:copy(<<PadLen>>, PadLen))/binary>>.

-endif.
