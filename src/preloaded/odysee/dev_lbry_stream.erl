%%% @doc LBRY/Odysee stream playback compatibility device.
%%%
%%% This device derives a browser/player friendly playback contract from a
%%% resolved LBRY stream claim.
-module(dev_lbry_stream).
-implements(<<"lbry-stream@1.0">>).
-export([info/1, stream/3, from_claim/3, playback/3, media/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"lbry-stream@1.0">>).
-define(DEFAULT_PLAYER_SERVER, <<"https://player.odycdn.com">>).

%% @doc Return the public device API.
info(_Opts) ->
    #{ exports => [<<"stream">>, <<"from-claim">>, <<"playback">>, <<"media">>] }.

%% @doc Resolve/derive stream metadata from a claim.
stream(Base, Req, Opts) ->
    from_claim(Base, Req, Opts).

%% @doc Derive stream metadata from a resolved or raw claim.
from_claim(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, ClaimMsg} ?= ensure_claim(Base, Req, Opts),
            ok_message(derive_stream(ClaimMsg, Base, Req, Opts))
        else
            Error -> Error
        end
    end).

%% @doc Return the playback contract, or a 307 redirect when requested.
playback(Base, Req, Opts) ->
    safe(fun() ->
        case method(Req, Opts) of
            <<"options">> ->
                {ok, cors_preflight_response()};
            _ ->
                maybe
                    {ok, Stream} ?= from_claim(Base, Req, Opts),
                    {ok, playback_response(Stream, Base, Req, Opts)}
                else
                    Error -> Error
                end
        end
    end).

%% @doc Serve media bytes through descriptor blobs or a bounded player proxy.
media(Base, Req, Opts) ->
    safe(fun() ->
        case method(Req, Opts) of
            <<"options">> ->
                {ok, cors_preflight_response()};
            _ ->
                maybe
                    {ok, Stream} ?= from_claim(Base, Req, Opts),
                    media_response(Stream, Base, Req, Opts)
                else
                    Error -> Error
                end
        end
    end).

safe(Fun) ->
    try Fun() of
        Res -> Res
    catch
        _:{error, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

ok_message(Msg) when is_map(Msg) -> {ok, Msg};
ok_message(Error) -> Error.

ensure_claim(Base = #{ <<"claim">> := Claim }, _Req, Opts) when is_map(Claim) ->
    case hb_maps:get(<<"claim-id">>, Base, not_found, Opts) of
        not_found ->
            hb_ao:raw(
                <<"lbry-claim@1.0">>,
                <<"resolve">>,
                #{},
                #{ <<"claim">> => Claim },
                Opts
            );
        _ -> {ok, Base}
    end;
ensure_claim(Base, Req, Opts) ->
    hb_ao:raw(<<"lbry-claim@1.0">>, <<"resolve">>, Base, Req, Opts).

derive_stream(ClaimMsg, Base, Req, Opts) ->
    maybe
        Claim = hb_maps:get(<<"claim">>, ClaimMsg, ClaimMsg, Opts),
        {ok, Value} ?= required(<<"value">>, ClaimMsg, Opts),
        {ok, Source} ?= required(<<"source">>, Value, Opts),
        {ok, SDHash} ?= required_first([<<"sd_hash">>, <<"sd-hash">>], Source, Opts),
        {ok, MediaType} ?=
            required_first([<<"media_type">>, <<"media-type">>], Source, Opts),
        {ok, ClaimID} ?= required(<<"claim-id">>, ClaimMsg, Opts),
        {ok, ClaimName} ?= required(<<"claim-name">>, ClaimMsg, Opts),
        Ext = file_extension(MediaType, Source, Opts),
        PlayerServer = player_server(Base, Req, Opts),
        StreamingURL = streaming_url(PlayerServer, ClaimName, ClaimID, SDHash, Ext),
        DownloadURL = download_url(PlayerServer, ClaimID, SDHash, Ext),
        Stream = stream_message(
            Claim,
            ClaimMsg,
            Value,
            Source,
            ClaimID,
            ClaimName,
            SDHash,
            MediaType,
            Ext,
            StreamingURL,
            DownloadURL,
            Opts
        ),
        Stream#{ <<"body">> => hb_json:encode(playback_payload(Stream, Opts)) }
    end.

stream_message(
    Claim,
    ClaimMsg,
    Value,
    Source,
    ClaimID,
    ClaimName,
    SDHash,
    MediaType,
    Ext,
    StreamingURL,
    DownloadURL,
    Opts
) ->
    Msg0 = #{
        <<"device">> => ?DEVICE,
        <<"content-type">> => <<"application/json">>,
        <<"claim">> => Claim,
        <<"claim-message">> => ClaimMsg,
        <<"value">> => Value,
        <<"source">> => Source,
        <<"claim-id">> => ClaimID,
        <<"claim-name">> => ClaimName,
        <<"sd-hash">> => SDHash,
        <<"media-type">> => MediaType,
        <<"file-extension">> => Ext,
        <<"streaming-url">> => StreamingURL,
        <<"download-url">> => DownloadURL
    },
    Optional = [
        {<<"title">>, first_value([<<"title">>], Value, Opts)},
        {<<"description">>, first_value([<<"description">>], Value, Opts)},
        {<<"stream-type">>, first_value([<<"stream_type">>, <<"stream-type">>], Value, Opts)},
        {<<"source-name">>, first_value([<<"name">>], Source, Opts)},
        {<<"source-hash">>, first_value([<<"hash">>], Source, Opts)},
        {<<"source-size">>, first_value([<<"size">>], Source, Opts)},
        {<<"thumbnail">>, thumbnail_url(Value, Opts)},
        {<<"duration">>, video_field(<<"duration">>, Value, Opts)},
        {<<"height">>, video_field(<<"height">>, Value, Opts)},
        {<<"width">>, video_field(<<"width">>, Value, Opts)}
    ],
    lists:foldl(fun put_optional/2, Msg0, Optional).

playback_response(Stream, Base, Req, Opts) ->
    URL =
        case bytes_playback_requested(Base, Req, Opts) of
            true -> media_url(Stream, Base, Req, Opts);
            false -> hb_maps:get(<<"streaming-url">>, Stream, Opts)
        end,
    case redirect_requested(Base, Req, Opts) of
        true ->
            (cors_headers())#{
                <<"status">> => 307,
                <<"location">> => URL,
                <<"content-type">> => <<"text/plain">>,
                <<"body">> => <<>>
            };
        false ->
            Payload = (playback_payload(Stream, Opts))#{ <<"streaming_url">> => URL },
            Body = hb_json:encode(Payload),
            (cors_headers())#{
                <<"status">> => 200,
                <<"content-type">> => <<"application/json">>,
                <<"content-length">> => byte_size(Body),
                <<"body">> => Body
            }
    end.

playback_payload(Stream, Opts) ->
    Pairs = [
        {<<"streaming_url">>, hb_maps:get(<<"streaming-url">>, Stream, Opts)},
        {<<"download_url">>, hb_maps:get(<<"download-url">>, Stream, Opts)},
        {<<"sd_hash">>, hb_maps:get(<<"sd-hash">>, Stream, Opts)},
        {<<"media_type">>, hb_maps:get(<<"media-type">>, Stream, Opts)},
        {<<"claim_id">>, hb_maps:get(<<"claim-id">>, Stream, Opts)},
        {<<"claim_name">>, hb_maps:get(<<"claim-name">>, Stream, Opts)},
        {<<"title">>, hb_maps:get(<<"title">>, Stream, not_found, Opts)},
        {<<"description">>, hb_maps:get(<<"description">>, Stream, not_found, Opts)},
        {<<"stream_type">>, hb_maps:get(<<"stream-type">>, Stream, not_found, Opts)},
        {<<"source_name">>, hb_maps:get(<<"source-name">>, Stream, not_found, Opts)},
        {<<"source_hash">>, hb_maps:get(<<"source-hash">>, Stream, not_found, Opts)},
        {<<"source_size">>, hb_maps:get(<<"source-size">>, Stream, not_found, Opts)},
        {<<"thumbnail_url">>, hb_maps:get(<<"thumbnail">>, Stream, not_found, Opts)},
        {<<"duration">>, hb_maps:get(<<"duration">>, Stream, not_found, Opts)},
        {<<"height">>, hb_maps:get(<<"height">>, Stream, not_found, Opts)},
        {<<"width">>, hb_maps:get(<<"width">>, Stream, not_found, Opts)}
    ],
    lists:foldl(fun put_optional/2, #{}, Pairs).

descriptor_media_request(Stream, Base, Req, Opts) ->
    Msg0 = #{
        <<"sd-hash">> => hb_maps:get(<<"sd-hash">>, Stream, Opts),
        <<"media-type">> => hb_maps:get(<<"media-type">>, Stream, Opts),
        <<"fetch-blobs">> => true
    },
    Msg1 =
        put_optional(
            {<<"media-size">>, hb_maps:get(<<"source-size">>, Stream, not_found, Opts)},
            Msg0
        ),
    lists:foldl(
        fun(Key, Msg) -> copy_first(Key, Base, Req, Msg, Opts) end,
        Msg1,
        [
            <<"method">>,
            <<"range">>,
            <<"Range">>,
            <<"encrypted-blobs">>,
            <<"blobs">>,
            <<"descriptor">>,
            <<"blob-base-url">>,
            <<"blob_base_url">>,
            <<"blob-base-urls">>,
            <<"blob_base_urls">>,
            <<"reflector-url">>,
            <<"reflector_url">>,
            <<"reflector-urls">>,
            <<"reflector_urls">>,
            <<"blob-url-template">>,
            <<"blob_url_template">>,
            <<"blob-url-templates">>,
            <<"blob_url_templates">>,
            <<"blob-dir">>,
            <<"blob_dir">>,
            <<"blob-dirs">>,
            <<"blob_dirs">>,
            <<"blob-directory">>,
            <<"blob_directory">>,
            <<"blob-cache">>,
            <<"blob_cache">>,
            <<"cache-blobs">>,
            <<"cache_blobs">>,
            <<"plain-cache-blobs">>,
            <<"plain_cache_blobs">>,
            <<"blob-connect-timeout">>,
            <<"blob-recv-timeout">>,
            <<"blob-checkout-timeout">>,
            <<"range-chunk-size">>,
            <<"chunk-size">>,
            <<"allow-full">>
        ]
    ).

media_response(Stream, Base, Req, Opts) ->
    case prefer_player_proxy(Base, Req, Opts) of
        true ->
            player_media_response(Stream, Base, Req, Opts);
        false ->
            descriptor_or_player_media(Stream, Base, Req, Opts)
    end.

descriptor_or_player_media(Stream, Base, Req, Opts) ->
    DescriptorRes =
        hb_ao:raw(
            <<"lbry-stream-descriptor@1.0">>,
            <<"media">>,
            #{},
            descriptor_media_request(Stream, Base, Req, Opts),
            Opts
        ),
    case DescriptorRes of
        {ok, _} -> DescriptorRes;
        Error ->
            case player_proxy_enabled(Base, Req, Opts) of
                true -> player_media_response(Stream, Base, Req, Opts);
                false -> Error
            end
    end.

prefer_player_proxy(Base, Req, Opts) ->
    player_proxy_enabled(Base, Req, Opts) andalso not descriptor_media_config_present(Base, Req, Opts).

player_proxy_enabled(Base, Req, Opts) ->
    Value =
        case first_found(
            [
                {Req, <<"player-proxy">>},
                {Req, <<"player_proxy">>},
                {Base, <<"player-proxy">>},
                {Base, <<"player_proxy">>}
            ],
            Opts
        ) of
            not_found -> hb_opts:get(<<"lbry-player-proxy">>, true, Opts);
            Found -> Found
        end,
    not falsy(Value).

descriptor_media_config_present(Base, Req, Opts) ->
    first_found(
        [
            {Req, <<"encrypted-blobs">>},
            {Req, <<"blobs">>},
            {Req, <<"descriptor">>},
            {Req, <<"blob-base-url">>},
            {Req, <<"blob_base_url">>},
            {Req, <<"blob-base-urls">>},
            {Req, <<"blob_base_urls">>},
            {Req, <<"reflector-url">>},
            {Req, <<"reflector_url">>},
            {Req, <<"reflector-urls">>},
            {Req, <<"reflector_urls">>},
            {Req, <<"blob-url-template">>},
            {Req, <<"blob_url_template">>},
            {Req, <<"blob-url-templates">>},
            {Req, <<"blob_url_templates">>},
            {Req, <<"blob-dir">>},
            {Req, <<"blob_dir">>},
            {Req, <<"blob-dirs">>},
            {Req, <<"blob_dirs">>},
            {Req, <<"blob-directory">>},
            {Req, <<"blob_directory">>},
            {Base, <<"encrypted-blobs">>},
            {Base, <<"blobs">>},
            {Base, <<"descriptor">>},
            {Base, <<"blob-base-url">>},
            {Base, <<"blob_base_url">>},
            {Base, <<"blob-base-urls">>},
            {Base, <<"blob_base_urls">>},
            {Base, <<"reflector-url">>},
            {Base, <<"reflector_url">>},
            {Base, <<"reflector-urls">>},
            {Base, <<"reflector_urls">>},
            {Base, <<"blob-url-template">>},
            {Base, <<"blob_url_template">>},
            {Base, <<"blob-url-templates">>},
            {Base, <<"blob_url_templates">>},
            {Base, <<"blob-dir">>},
            {Base, <<"blob_dir">>},
            {Base, <<"blob-dirs">>},
            {Base, <<"blob_dirs">>},
            {Base, <<"blob-directory">>},
            {Base, <<"blob_directory">>}
        ],
        Opts
    ) =/= not_found.

player_media_response(Stream, Base, Req, Opts) ->
    case method(Req, Opts) of
        <<"head">> ->
            {ok, player_head_response(Stream, Opts)};
        _ ->
            fetch_player_media(player_media_urls(Stream, Opts), Stream, Base, Req, Opts, [])
    end.

player_head_response(Stream, Opts) ->
    Msg0 =
        (cors_headers())#{
            <<"status">> => 200,
            <<"content-type">> => hb_maps:get(<<"media-type">>, Stream, <<"application/octet-stream">>, Opts),
            <<"accept-ranges">> => <<"bytes">>,
            <<"body">> => <<>>
        },
    put_optional({<<"content-length">>, stream_size(Stream, Opts)}, Msg0).

fetch_player_media([], _Stream, _Base, _Req, _Opts, Errors) ->
    {error, {player_media_fetch_failed, lists:reverse(Errors)}};
fetch_player_media([URL | Rest], Stream, Base, Req, Opts, Errors) ->
    Range = player_proxy_range(Stream, Base, Req, Opts),
    Msg = #{
        <<"method">> => <<"GET">>,
        <<"path">> => URL,
        <<"range">> => Range,
        <<"accept">> => <<"video/*,*/*">>
    },
    case hb_http:request(Msg, Opts) of
        {ok, Res = #{ <<"status">> := Status }}
                when is_integer(Status), Status >= 200, Status < 300 ->
            {ok, player_proxy_response(Res, Stream, Opts)};
        {ok, #{ <<"status">> := Status }} when is_integer(Status) ->
            fetch_player_media(Rest, Stream, Base, Req, Opts, [{URL, Status} | Errors]);
        Error ->
            fetch_player_media(Rest, Stream, Base, Req, Opts, [{URL, Error} | Errors])
    end.

player_proxy_response(Res, Stream, Opts) ->
    Body = hb_maps:get(<<"body">>, Res, <<>>, Opts),
    Msg0 =
        (cors_headers())#{
            <<"status">> => hb_maps:get(<<"status">>, Res, 206, Opts),
            <<"content-type">> =>
                response_header(
                    [<<"content-type">>, <<"Content-Type">>],
                    Res,
                    hb_maps:get(<<"media-type">>, Stream, <<"application/octet-stream">>, Opts),
                    Opts
                ),
            <<"content-length">> => byte_size(Body),
            <<"accept-ranges">> => <<"bytes">>,
            <<"body">> => Body
        },
    put_optional(
        {<<"content-range">>,
            response_header([<<"content-range">>, <<"Content-Range">>], Res, not_found, Opts)},
        Msg0
    ).

response_header([], _Res, Default, _Opts) ->
    Default;
response_header([Key | Rest], Res, Default, Opts) ->
    case hb_maps:get(Key, Res, not_found, Opts) of
        not_found -> response_header(Rest, Res, Default, Opts);
        Value -> Value
    end.

player_media_urls(Stream, Opts) ->
    [
        URL
    ||
        Key <- [<<"download-url">>, <<"streaming-url">>],
        URL <- [hb_maps:get(Key, Stream, not_found, Opts)],
        URL =/= not_found
    ].

player_proxy_range(Stream, Base, Req, Opts) ->
    Range =
        case first_found(
            [
                {Req, <<"range">>},
                {Req, <<"Range">>},
                {Base, <<"range">>},
                {Base, <<"Range">>}
            ],
            Opts
        ) of
            not_found -> <<"bytes=0-">>;
            Found -> hb_util:bin(Found)
        end,
    cap_player_range(Range, Stream, Base, Req, Opts).

cap_player_range(<<"bytes=", Descriptor/binary>>, Stream, Base, Req, Opts) ->
    cap_range_descriptor(Descriptor, Stream, Base, Req, Opts);
cap_player_range(<<"bytes ", Descriptor/binary>>, Stream, Base, Req, Opts) ->
    cap_range_descriptor(Descriptor, Stream, Base, Req, Opts);
cap_player_range(_Range, Stream, Base, Req, Opts) ->
    capped_range_from_start(0, Stream, Base, Req, Opts).

cap_range_descriptor(Descriptor, Stream, Base, Req, Opts) ->
    [FirstRange | _] = binary:split(Descriptor, <<",">>),
    [ByteRange | _] = binary:split(FirstRange, <<"/">>),
    case binary:split(ByteRange, <<"-">>) of
        [<<>>, SuffixBin] ->
            capped_suffix_range(SuffixBin, Stream, Base, Req, Opts);
        [StartBin, <<>>] ->
            case safe_int(StartBin) of
                {ok, Start} -> capped_range_from_start(Start, Stream, Base, Req, Opts);
                error -> capped_range_from_start(0, Stream, Base, Req, Opts)
            end;
        [StartBin, EndBin] ->
            case {safe_int(StartBin), safe_int(EndBin)} of
                {{ok, Start}, {ok, End}} ->
                    capped_range(Start, End, Stream, Base, Req, Opts);
                _ ->
                    capped_range_from_start(0, Stream, Base, Req, Opts)
            end;
        _ ->
            capped_range_from_start(0, Stream, Base, Req, Opts)
    end.

capped_suffix_range(SuffixBin, Stream, Base, Req, Opts) ->
    case {safe_int(SuffixBin), stream_size(Stream, Opts)} of
        {{ok, Suffix}, Size} when is_integer(Size), Suffix > 0 ->
            Capped = min(Suffix, player_proxy_chunk_size(Base, Req, Opts)),
            capped_range(max(0, Size - Capped), Size - 1, Stream, Base, Req, Opts);
        _ ->
            capped_range_from_start(0, Stream, Base, Req, Opts)
    end.

capped_range_from_start(Start, Stream, Base, Req, Opts) ->
    capped_range(
        Start,
        Start + player_proxy_chunk_size(Base, Req, Opts) - 1,
        Stream,
        Base,
        Req,
        Opts
    ).

capped_range(Start, End, Stream, Base, Req, Opts) ->
    ChunkEnd = Start + player_proxy_chunk_size(Base, Req, Opts) - 1,
    MediaEnd =
        case stream_size(Stream, Opts) of
            Size when is_integer(Size), Size > 0 -> Size - 1;
            _ -> ChunkEnd
        end,
    FinalEnd = max(Start, min(min(End, ChunkEnd), MediaEnd)),
    <<"bytes=", (integer_to_binary(Start))/binary, "-", (integer_to_binary(FinalEnd))/binary>>.

stream_size(Stream, Opts) ->
    case hb_maps:get(<<"source-size">>, Stream, not_found, Opts) of
        not_found -> not_found;
        Value ->
            case safe_int(Value) of
                {ok, Size} -> Size;
                error -> not_found
            end
    end.

player_proxy_chunk_size(Base, Req, Opts) ->
    Value =
        case first_found(
            [
                {Req, <<"range-chunk-size">>},
                {Req, <<"chunk-size">>},
                {Base, <<"range-chunk-size">>},
                {Base, <<"chunk-size">>}
            ],
            Opts
        ) of
            not_found -> hb_opts:get(<<"lbry-player-proxy-chunk-size">>, 1048576, Opts);
            Found -> Found
        end,
    case safe_int(Value) of
        {ok, Size} when Size > 0 -> Size;
        _ -> 1048576
    end.

safe_int(Value) when is_integer(Value), Value >= 0 ->
    {ok, Value};
safe_int(Value) ->
    try {ok, hb_util:int(Value)}
    catch _:_ -> error
    end.

copy_first(Key, Base, Req, Msg, Opts) ->
    case first_found([{Req, Key}, {Base, Key}], Opts) of
        not_found -> Msg;
        Value -> Msg#{ Key => Value }
    end.

bytes_playback_requested(Base, Req, Opts) ->
    Mode =
        first_found(
            [
                {Req, <<"mode">>},
                {Req, <<"playback-mode">>},
                {Req, <<"format">>},
                {Base, <<"mode">>},
                {Base, <<"playback-mode">>},
                {Base, <<"format">>}
            ],
            Opts
        ),
    truthy(first_found([{Req, <<"bytes">>}, {Req, <<"serve">>}, {Base, <<"bytes">>}, {Base, <<"serve">>}], Opts))
        orelse Mode =:= <<"bytes">>
        orelse Mode =:= <<"media">>
        orelse Mode =:= <<"hyperbeam">>.

media_url(Stream, Base, Req, Opts) ->
    Origin = trim_trailing_slash(media_base_url(Base, Req, Opts)),
    ClaimName = hb_maps:get(<<"claim-name">>, Stream, Opts),
    ClaimID = hb_maps:get(<<"claim-id">>, Stream, Opts),
    Query =
        encode_query(
            [
                {<<"claim-name">>, ClaimName},
                {<<"claim-id">>, ClaimID}
            ]
                ++ media_query_params(Base, Req, Opts)
        ),
    <<Origin/binary, "/~lbry-stream@1.0/media?", Query/binary>>.

media_query_params(Base, Req, Opts) ->
    lists:filtermap(
        fun(Key) ->
            case first_found([{Req, Key}, {Base, Key}], Opts) of
                not_found -> false;
                Value -> {true, {Key, Value}}
            end
        end,
        media_query_keys()
    ).

media_query_keys() ->
    [
        <<"descriptor">>,
        <<"blob-base-url">>,
        <<"blob_base_url">>,
        <<"blob-base-urls">>,
        <<"blob_base_urls">>,
        <<"reflector-url">>,
        <<"reflector_url">>,
        <<"reflector-urls">>,
        <<"reflector_urls">>,
        <<"blob-url-template">>,
        <<"blob_url_template">>,
        <<"blob-url-templates">>,
        <<"blob_url_templates">>,
        <<"blob-dir">>,
        <<"blob_dir">>,
        <<"blob-dirs">>,
        <<"blob_dirs">>,
        <<"blob-directory">>,
        <<"blob_directory">>,
        <<"blob-cache">>,
        <<"blob_cache">>,
        <<"cache-blobs">>,
        <<"cache_blobs">>,
        <<"plain-cache-blobs">>,
        <<"plain_cache_blobs">>,
        <<"blob-connect-timeout">>,
        <<"blob-recv-timeout">>,
        <<"blob-checkout-timeout">>,
        <<"range-chunk-size">>,
        <<"chunk-size">>,
        <<"allow-full">>
    ].

encode_query(Pairs) ->
    iolist_to_binary(
        lists:join(
            <<"&">>,
            [
                <<
                    (url_encode(Key))/binary,
                    "=",
                    (url_encode(query_value(Value)))/binary
                >>
            ||
                {Key, Value} <- Pairs
            ]
        )
    ).

query_value(Value) when is_binary(Value) ->
    Value;
query_value(Value) when is_integer(Value) ->
    integer_to_binary(Value);
query_value(true) ->
    <<"true">>;
query_value(false) ->
    <<"false">>;
query_value(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
query_value(Values) when is_list(Values) ->
    case io_lib:printable_list(Values) of
        true ->
            unicode:characters_to_binary(Values);
        false ->
            iolist_to_binary(lists:join(<<",">>, [query_value(Value) || Value <- Values]))
    end;
query_value(Value) ->
    hb_util:bin(Value).

method(Req, Opts) ->
    hb_util:to_lower(hb_util:bin(hb_maps:get(<<"method">>, Req, <<"GET">>, Opts))).

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
            <<"Content-Length,Content-Range,Accept-Ranges,Location">>
    }.

media_base_url(Base, Req, Opts) ->
    case first_found(
        [
            {Req, <<"media-base-url">>},
            {Req, <<"media_base_url">>},
            {Base, <<"media-base-url">>},
            {Base, <<"media_base_url">>}
        ],
        Opts
    ) of
        not_found ->
            request_origin(Req, Opts);
        URL ->
            URL
    end.

request_origin(Req, Opts) ->
    case first_found([{Req, <<"x-forwarded-host">>}, {Req, <<"host">>}], Opts) of
        not_found ->
            hb_opts:get(<<"lbry-media-origin">>, <<"http://localhost:8734">>, Opts);
        Host ->
            Scheme =
                case first_found([{Req, <<"x-forwarded-proto">>}, {Req, <<"scheme">>}], Opts) of
                    not_found -> <<"http">>;
                    Proto -> Proto
                end,
            <<Scheme/binary, "://", (ensure_host_port(Host, Scheme, Opts))/binary>>
    end.

ensure_host_port(Host, _Scheme, _Opts) ->
    case binary:match(Host, <<":">>) of
        nomatch -> append_node_port(Host);
        _ -> Host
    end.

append_node_port(Host) ->
    <<Host/binary, ":", (integer_to_binary(hb_opts:get(port, 8734)))/binary>>.

url_encode(Bin) when is_binary(Bin) ->
    iolist_to_binary([url_encode_byte(Byte) || <<Byte>> <= Bin]).

url_encode_byte(Byte)
        when Byte >= $a, Byte =< $z;
             Byte >= $A, Byte =< $Z;
             Byte >= $0, Byte =< $9;
             Byte =:= $-;
             Byte =:= $.;
             Byte =:= $_;
             Byte =:= $~ ->
    <<Byte>>;
url_encode_byte(Byte) ->
    <<$%, (hex_digit(Byte bsr 4)), (hex_digit(Byte band 15))>>.

hex_digit(N) when N >= 0, N =< 9 -> $0 + N;
hex_digit(N) -> $A + N - 10.

streaming_url(PlayerServer, ClaimName, ClaimID, SDHash, Ext) ->
    Base = trim_trailing_slash(PlayerServer),
    ShortSDHash = sd_hash_prefix(SDHash),
    <<
        Base/binary,
        "/api/v3/streams/free/",
        ClaimName/binary,
        "/",
        ClaimID/binary,
        "/",
        ShortSDHash/binary,
        ".",
        Ext/binary
    >>.

download_url(PlayerServer, ClaimID, SDHash, Ext) ->
    Base = trim_trailing_slash(PlayerServer),
    ShortSDHash = sd_hash_prefix(SDHash),
    <<Base/binary, "/v6/streams/", ClaimID/binary, "/", ShortSDHash/binary, ".", Ext/binary>>.

sd_hash_prefix(SDHash) when byte_size(SDHash) >= 6 ->
    binary:part(SDHash, 0, 6);
sd_hash_prefix(SDHash) ->
    SDHash.

player_server(Base, Req, Opts) ->
    case first_found(
        [
            {Req, <<"player-server">>},
            {Req, <<"player_server">>},
            {Base, <<"player-server">>},
            {Base, <<"player_server">>}
        ],
        Opts
    ) of
        not_found ->
            hb_opts:get(<<"lbry-player-server">>, ?DEFAULT_PLAYER_SERVER, Opts);
        Server ->
            Server
    end.

file_extension(MediaType, Source, Opts) ->
    case media_extension(MediaType) of
        not_found -> source_extension(Source, Opts);
        Ext -> Ext
    end.

media_extension(<<"video/mp4">>) -> <<"mp4">>;
media_extension(<<"video/webm">>) -> <<"webm">>;
media_extension(<<"audio/mpeg">>) -> <<"mp3">>;
media_extension(<<"audio/mp4">>) -> <<"m4a">>;
media_extension(<<"audio/ogg">>) -> <<"ogg">>;
media_extension(<<"image/jpeg">>) -> <<"jpg">>;
media_extension(<<"image/png">>) -> <<"png">>;
media_extension(<<"image/gif">>) -> <<"gif">>;
media_extension(_MediaType) -> not_found.

source_extension(Source, Opts) ->
    case first_value([<<"name">>], Source, Opts) of
        Name when is_binary(Name) ->
            case lists:reverse(binary:split(Name, <<".">>, [global])) of
                [Ext | [_ | _]] -> hb_util:to_lower(Ext);
                _ -> <<"bin">>
            end;
        _ ->
            <<"bin">>
    end.

thumbnail_url(Value, Opts) ->
    case first_value([<<"thumbnail">>], Value, Opts) of
        Thumbnail when is_map(Thumbnail) -> first_value([<<"url">>], Thumbnail, Opts);
        Other -> Other
    end.

video_field(Key, Value, Opts) ->
    case first_value([<<"video">>], Value, Opts) of
        Video when is_map(Video) -> first_value([Key], Video, Opts);
        _ -> not_found
    end.

redirect_requested(Base, Req, Opts) ->
    Format =
        first_found(
            [
                {Req, <<"format">>},
                {Req, <<"response">>},
                {Base, <<"format">>},
                {Base, <<"response">>}
            ],
            Opts
        ),
    Redirect =
        first_found(
            [
                {Req, <<"redirect">>},
                {Base, <<"redirect">>}
            ],
            Opts
        ),
    truthy(Redirect) orelse Format =:= <<"redirect">>.

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

required(Key, Map, Opts) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> {error, {missing, Key}};
        Value -> {ok, Value}
    end.

required_first(Keys, Map, Opts) ->
    case first_value(Keys, Map, Opts) of
        not_found -> {error, {missing, hd(Keys)}};
        Value -> {ok, Value}
    end.

first_value([], _Map, _Opts) ->
    not_found;
first_value([Key | Rest], Map, Opts) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> first_value(Rest, Map, Opts);
        Value -> Value
    end.

first_found([], _Opts) ->
    not_found;
first_found([{Msg, Key} | Rest], Opts) when is_map(Msg) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> first_found(Rest, Opts);
        Value -> Value
    end;
first_found([_ | Rest], Opts) ->
    first_found(Rest, Opts).

put_optional({_Key, not_found}, Msg) -> Msg;
put_optional({Key, Value}, Msg) -> Msg#{ Key => Value }.

trim_trailing_slash(URL) when is_binary(URL), byte_size(URL) > 0 ->
    case binary:at(URL, byte_size(URL) - 1) of
        $/ -> binary:part(URL, 0, byte_size(URL) - 1);
        _ -> URL
    end;
trim_trailing_slash(URL) ->
    URL.

-ifdef(TEST).

stream_from_claim_builds_playback_url_test() ->
    {ok, Stream} = stream(#{}, #{ <<"claim">> => target_claim() }, #{}),
    ?assertEqual(<<"video/mp4">>, hb_maps:get(<<"media-type">>, Stream, #{})),
    ?assertEqual(expected_streaming_url(), hb_maps:get(<<"streaming-url">>, Stream, #{})),
    Body = hb_json:decode(hb_maps:get(<<"body">>, Stream, #{})),
    ?assertEqual(expected_streaming_url(), hb_maps:get(<<"streaming_url">>, Body, #{})).

playback_redirect_test() ->
    {ok, Redirect} =
        playback(
            #{},
            #{ <<"claim">> => target_claim(), <<"redirect">> => true },
            #{}
        ),
    ?assertEqual(307, hb_maps:get(<<"status">>, Redirect, #{})),
    ?assertEqual(expected_streaming_url(), hb_maps:get(<<"location">>, Redirect, #{})).

playback_bytes_redirect_points_to_media_test() ->
    {ok, Redirect} =
        playback(
            #{},
            #{
                <<"claim">> => target_claim(),
                <<"redirect">> => true,
                <<"mode">> => <<"bytes">>,
                <<"media-base-url">> => <<"http://127.0.0.1:8734">>
            },
            #{}
        ),
    ?assertEqual(307, hb_maps:get(<<"status">>, Redirect, #{})),
    ?assertEqual(expected_media_url(), hb_maps:get(<<"location">>, Redirect, #{})).

playback_bytes_redirect_preserves_blob_config_test() ->
    {ok, Redirect} =
        playback(
            #{},
            #{
                <<"claim">> => target_claim(),
                <<"redirect">> => true,
                <<"mode">> => <<"bytes">>,
                <<"media-base-url">> => <<"http://127.0.0.1:8734">>,
                <<"blob-base-url">> => <<"http://127.0.0.1:9090">>,
                <<"blob-dir">> => <<"/tmp/lbry blobs">>,
                <<"cache-blobs">> => false,
                <<"blob-connect-timeout">> => 2500
            },
            #{}
        ),
    ?assertEqual(307, hb_maps:get(<<"status">>, Redirect, #{})),
    ?assertEqual(expected_media_url_with_config(), hb_maps:get(<<"location">>, Redirect, #{})).

playback_bytes_redirect_adds_node_port_to_host_test() ->
    {ok, Redirect} =
        playback(
            #{},
            #{
                <<"claim">> => target_claim(),
                <<"redirect">> => true,
                <<"mode">> => <<"bytes">>,
                <<"host">> => <<"127.0.0.1">>
            },
            #{}
    ),
    ?assertEqual(expected_media_url(), hb_maps:get(<<"location">>, Redirect, #{})).

playback_bytes_json_returns_media_url_in_body_test() ->
    {ok, Res} =
        playback(
            #{},
            #{
                <<"claim">> => target_claim(),
                <<"mode">> => <<"bytes">>,
                <<"media-base-url">> => <<"http://127.0.0.1:8734">>
            },
            #{}
        ),
    Body = hb_json:decode(hb_maps:get(<<"body">>, Res, #{})),
    ?assertEqual(200, hb_maps:get(<<"status">>, Res, #{})),
    ?assertEqual(<<"application/json">>, hb_maps:get(<<"content-type">>, Res, #{})),
    ?assertEqual(expected_media_url(), hb_maps:get(<<"streaming_url">>, Body, #{})),
    ?assertEqual(not_found, hb_maps:get(<<"description">>, Res, not_found, #{})).

playback_options_preflight_test() ->
    {ok, Res} = playback(#{}, #{ <<"method">> => <<"OPTIONS">> }, #{}),
    ?assertEqual(204, hb_maps:get(<<"status">>, Res, #{})),
    ?assertEqual(<<"*">>, hb_maps:get(<<"access-control-allow-origin">>, Res, #{})),
    ?assertEqual(<<>>, hb_maps:get(<<"body">>, Res, #{})).

media_options_preflight_test() ->
    {ok, Res} = media(#{}, #{ <<"method">> => <<"OPTIONS">> }, #{}),
    ?assertEqual(204, hb_maps:get(<<"status">>, Res, #{})),
    ?assertEqual(<<"*">>, hb_maps:get(<<"access-control-allow-origin">>, Res, #{})),
    ?assertEqual(<<>>, hb_maps:get(<<"body">>, Res, #{})).

media_player_proxy_head_uses_claim_metadata_test() ->
    {ok, Res} =
        media(
            #{},
            #{ <<"claim">> => target_claim(), <<"method">> => <<"HEAD">> },
            #{}
        ),
    ?assertEqual(200, hb_maps:get(<<"status">>, Res, #{})),
    ?assertEqual(<<"video/mp4">>, hb_maps:get(<<"content-type">>, Res, #{})),
    ?assertEqual(653610679, hb_maps:get(<<"content-length">>, Res, #{})),
    ?assertEqual(<<"bytes">>, hb_maps:get(<<"accept-ranges">>, Res, #{})),
    ?assertEqual(<<>>, hb_maps:get(<<"body">>, Res, #{})).

media_player_proxy_caps_open_range_test() ->
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/v6/streams/[...]", player, {206, <<"abcde">>}}
        ]),
    try
        {ok, Stream} =
            stream(
                #{},
                #{ <<"claim">> => target_claim(), <<"player-server">> => MockServer },
                #{}
            ),
        {ok, Res} =
            player_media_response(
                Stream,
                #{},
                #{ <<"range">> => <<"bytes=0-">>, <<"chunk-size">> => 5 },
                #{}
            ),
        ?assertEqual(206, hb_maps:get(<<"status">>, Res, #{})),
        ?assertEqual(<<"abcde">>, hb_maps:get(<<"body">>, Res, #{})),
        [Request] = hb_mock_server:get_requests(player, 1, ServerHandle),
        Headers = hb_maps:get(<<"headers">>, Request, #{}, #{}),
        ?assertEqual(<<"bytes=0-4">>, hb_maps:get(<<"range">>, Headers, #{}, #{}))
    after
        hb_mock_server:stop(ServerHandle)
    end.

stream_rejects_non_stream_claim_test() ->
    Claim = target_claim(),
    BadValue = maps:remove(<<"source">>, hb_maps:get(<<"value">>, Claim, #{})),
    ?assertMatch(
        {error, {missing, <<"source">>}},
        stream(#{}, #{ <<"claim">> => Claim#{ <<"value">> => BadValue } }, #{})
    ).

expected_streaming_url() ->
    <<
        "https://player.odycdn.com/api/v3/streams/free/",
        "why-is-it-so-easy-to-disrupt-gps/",
        "346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169/",
        "6ee8f7.mp4"
    >>.

expected_media_url() ->
    <<
        "http://127.0.0.1:8734/~lbry-stream@1.0/media?",
        "claim-name=why-is-it-so-easy-to-disrupt-gps&",
        "claim-id=346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169"
    >>.

expected_media_url_with_config() ->
    <<
        (expected_media_url())/binary,
        "&blob-base-url=http%3A%2F%2F127.0.0.1%3A9090",
        "&blob-dir=%2Ftmp%2Flbry%20blobs",
        "&cache-blobs=false",
        "&blob-connect-timeout=2500"
    >>.

target_claim() ->
    #{
        <<"claim_id">> => <<"346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169">>,
        <<"canonical_url">> =>
            <<"lbry://@veritasium#f/why-is-it-so-easy-to-disrupt-gps#3">>,
        <<"name">> => <<"why-is-it-so-easy-to-disrupt-gps">>,
        <<"type">> => <<"claim">>,
        <<"value_type">> => <<"stream">>,
        <<"value">> => #{
            <<"title">> => <<"Why Is It So Easy To Disrupt GPS?">>,
            <<"description">> => <<"Something is disrupting GPS signals across Europe.">>,
            <<"source">> => #{
                <<"hash">> =>
                    <<"81a1fc78a95489d499214616773505d4ca78bb49279a7dafc6aa1b0a546b2eeb6253db951d1d5514388a3c7b57bea647">>,
                <<"media_type">> => <<"video/mp4">>,
                <<"name">> => <<"why-is-it-so-easy-to-disrupt.mp4">>,
                <<"sd_hash">> =>
                    <<"6ee8f762a2eedbd2b5eeade82ca4d0a6287f55db4195563cc52fc004701b7d55edcfad277a5141084bdf5fca3adb403a">>,
                <<"size">> => <<"653610679">>
            },
            <<"stream_type">> => <<"video">>,
            <<"thumbnail">> => #{ <<"url">> => <<"https://thumbnails.lbry.com/tz23G_UXCGA">> },
            <<"video">> => #{
                <<"duration">> => 2056,
                <<"height">> => 1080,
                <<"width">> => 1920
            }
        }
    }.

-endif.
