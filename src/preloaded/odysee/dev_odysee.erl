-module(dev_odysee).
-implements(<<"odysee@1.0">>).
-export([
    info/1,
    index/3,
    resolve/3,
    claim/3,
    transaction/3,
    descriptor/3,
    blob/3,
    verify_blobs/3,
    stream_graph/3,
    verified_stream/3,
    range/3,
    media/3,
    bytes/3
]).
-include("include/hb.hrl").

-define(DEFAULT_RANGE_SIZE, 1048576).

info(_) ->
    #{
        exports => [
            <<"index">>,
            <<"resolve">>,
            <<"claim">>,
            <<"transaction">>,
            <<"descriptor">>,
            <<"blob">>,
            <<"verify-blobs">>,
            <<"stream-graph">>,
            <<"verified-stream">>,
            <<"range">>,
            <<"media">>,
            <<"bytes">>
        ]
    }.

index(_Base, _Req, _Opts) ->
    {ok, #{
        <<"device">> => <<"odysee@1.0">>,
        <<"paths">> => #{
            <<"resolve">> => [<<"claim-id">>, <<"name">>, <<"url">>],
            <<"claim">> => [<<"claim-id">>, <<"name">>, <<"url">>],
            <<"transaction">> => [<<"txid">>],
            <<"descriptor">> => [<<"sd-hash">>],
            <<"blob">> => [<<"hash">>],
            <<"verify-blobs">> => [<<"sd-hash">>, <<"limit">>],
            <<"stream-graph">> => [<<"claim-id">>, <<"name">>, <<"url">>],
            <<"verified-stream">> => [<<"claim-id">>, <<"name">>, <<"url">>],
            <<"range">> => [<<"sd-hash">>, <<"start">>, <<"end">>],
            <<"media">> => [<<"sd-hash">>, <<"claim-id">>, <<"name">>, <<"url">>, <<"range">>],
            <<"bytes">> => [<<"sd-hash">>]
        }
    }}.

resolve(Base, Req, Opts) ->
    with_target(Base, Req, Opts, fun(Target) ->
        map_result(
            hb_lbry_proxy:claim(Target, Opts),
            fun(Claim) -> stream_claim(Claim, Opts) end
        )
    end).

claim(Base, Req, Opts) ->
    resolve(Base, Req, Opts).

transaction(Base, Req, Opts) ->
    with_txid(Base, Req, Opts, fun(TxID) ->
        map_result(
            hb_lbry_proxy:transaction_show(TxID, Opts),
            fun(TxResult) ->
                Hex = hb_maps:get(<<"hex">>, TxResult, undefined, Opts),
                case Hex of
                    undefined -> error_map(missing_raw_tx_hex);
                    _ ->
                        Tx =
                            codec(
                                <<"lbry-transaction@1.0">>,
                                Hex,
                                #{ <<"encoding">> => <<"hex">> },
                                Opts
                            ),
                        hex_binaries(Tx#{ <<"raw-hex">> => hb_util:to_lower(Hex) })
                end
            end
        )
    end).

descriptor(Base, Req, Opts) ->
    with_sd_hash(Base, Req, Opts, fun(SDHash) ->
        map_result(
            hb_lbry_bridge:descriptor(SDHash, Opts),
            fun(Descriptor) ->
                codec(<<"lbry-stream-descriptor@1.0">>, Descriptor, #{}, Opts)
            end
        )
    end).

blob(Base, Req, Opts) ->
    with_blob_hash(Base, Req, Opts, fun(Hash) ->
        map_result(
            hb_lbry_bridge:blob(Hash, Opts),
            fun(Bytes) ->
                #{
                    <<"status">> => 200,
                    <<"content-type">> => <<"application/octet-stream">>,
                    <<"content-length">> => byte_size(Bytes),
                    <<"blob-hash">> => hb_util:to_lower(Hash),
                    <<"body">> => Bytes
                }
            end
        )
    end).

verify_blobs(Base, Req, Opts) ->
    with_sd_hash(Base, Req, Opts, fun(SDHash) ->
        Limit = integer_param(Base, Req, <<"limit">>, 1, Opts),
        map_result(
            hb_lbry_bridge:verify_blobs(SDHash, Limit, Opts),
            fun(Result) -> normalize_verify_blobs(Result, Opts) end
        )
    end).

stream_graph(Base, Req, Opts) ->
    with_target(Base, Req, Opts, fun(Target) ->
        map_result(
            hb_lbry_bridge:stream_graph(Target, Opts),
            fun(StreamGraph) ->
                hex_binaries(normalize_stream_graph(StreamGraph, Target, Opts))
            end
        )
    end).

verified_stream(Base, Req, Opts) ->
    with_target(Base, Req, Opts, fun(Target) ->
        map_result(
            hb_lbry_bridge:verified_stream(Target, Opts),
            fun(VerifiedStream) ->
                hex_binaries(normalize_verified_stream(VerifiedStream, Target, Opts))
            end
        )
    end).

range(Base, Req, Opts) ->
    with_sd_hash(Base, Req, Opts, fun(SDHash) ->
        case explicit_range(Base, Req, Opts) of
            {ok, Start, End} ->
                range_response(SDHash, Start, End, Opts);
            Error ->
                error_map(Error)
        end
    end).

media(Base, Req, Opts) ->
    with_media_source(Base, Req, Opts, fun(Source) ->
        case request_range(Base, Req, Opts) of
            {ok, Start, End} ->
                case bounded_range(Source, Start, End) of
                    {ok, BoundedStart, BoundedEnd} ->
                        range_response(Source, BoundedStart, BoundedEnd, Opts);
                    Error ->
                        error_map(Error)
                end;
            Error ->
                error_map(Error)
        end
    end).

bytes(Base, Req, Opts) ->
    with_sd_hash(Base, Req, Opts, fun(SDHash) ->
        map_result(
            hb_lbry_bridge:reassemble_stream(SDHash, Opts),
            fun(Result) ->
                #{
                    <<"status">> => 200,
                    <<"content-type">> => <<"application/octet-stream">>,
                    <<"accept-ranges">> => <<"bytes">>,
                    <<"sd-hash">> => hb_util:to_lower(SDHash),
                    <<"byte-size">> => maps:get(<<"byte-size">>, Result),
                    <<"descriptor">> =>
                        codec(
                            <<"lbry-stream-descriptor@1.0">>,
                            maps:get(<<"descriptor">>, Result),
                            #{},
                            Opts
                        ),
                    <<"body">> => maps:get(<<"bytes">>, Result)
                }
            end
        )
    end).

range_response(SDHash, Start, End, Opts) when is_binary(SDHash) ->
    range_response(#{ <<"sd-hash">> => SDHash }, Start, End, Opts);
range_response(Source, Start, End, Opts) ->
    SDHash = maps:get(<<"sd-hash">>, Source),
    map_result(
        hb_lbry_bridge:stream_range(SDHash, Start, End, Opts),
        fun(Result) ->
            Body = maps:get(<<"bytes">>, Result),
            ActualEnd = maps:get(<<"end">>, Result),
            Total = maps:get(<<"byte-size">>, Source, undefined),
            ?event(odysee_device,
                {media_slice,
                    {sd_hash, hb_util:to_lower(SDHash)},
                    {start, Start},
                    {actual_end, ActualEnd},
                    {requested_end, maps:get(<<"requested-end">>, Result)},
                    {size, byte_size(Body)}},
                Opts
            ),
            maps:merge(
                #{
                    <<"status">> => 206,
                    <<"content-type">> =>
                        maps:get(<<"content-type">>, Source, <<"application/octet-stream">>),
                    <<"content-length">> => byte_size(Body),
                    <<"accept-ranges">> => <<"bytes">>,
                    <<"content-range">> => content_range(Start, ActualEnd, Total),
                    <<"sd-hash">> => hb_util:to_lower(SDHash),
                    <<"start">> => Start,
                    <<"end">> => ActualEnd,
                    <<"requested-end">> => maps:get(<<"requested-end">>, Result),
                    <<"body">> => Body
                },
                maps:from_list(response_metadata(Source, Total))
            )
        end
    ).

response_metadata(Source, Total) ->
    [
        {K, V}
     ||
        {K, V} <- [
            {<<"byte-size">>, Total},
            {<<"byte-size-source">>, maps:get(<<"byte-size-source">>, Source, undefined)},
            {<<"claim-id">>, maps:get(<<"claim-id">>, Source, undefined)},
            {<<"filename">>, maps:get(<<"filename">>, Source, undefined)}
        ],
        V =/= undefined
    ].

normalize_stream_graph(StreamGraph, Target, Opts) ->
    Claim = maps:get(<<"claim">>, StreamGraph),
    ParsedTx = maps:get(<<"parsed-tx">>, StreamGraph),
    ClaimOutput = claim_output(ParsedTx, claim_nout(Claim)),
    #{
        <<"device">> => <<"odysee@1.0">>,
        <<"view">> => <<"stream-graph">>,
        <<"target">> => Target,
        <<"stream">> => stream_claim(stream_claim_source(Claim, ClaimOutput), Opts),
        <<"descriptor">> =>
            codec(
                <<"lbry-stream-descriptor@1.0">>,
                maps:get(<<"descriptor">>, StreamGraph),
                #{},
                Opts
            ),
        <<"transaction">> =>
            codec(
                <<"lbry-transaction@1.0">>,
                maps:get(<<"raw">>, ParsedTx),
                #{},
                Opts
            ),
        <<"sd-hash">> => maps:get(<<"sd-hash">>, StreamGraph),
        <<"txid">> => maps:get(<<"txid">>, StreamGraph)
    }.

normalize_verified_stream(VerifiedStream, Target, Opts) ->
    StreamGraph = normalize_stream_graph(VerifiedStream, Target, Opts),
    Claim = maps:get(<<"claim">>, VerifiedStream),
    ParsedTx = maps:get(<<"parsed-tx">>, VerifiedStream),
    ClaimOutput = claim_output(ParsedTx, claim_nout(Claim)),
    StreamGraph#{
        <<"view">> => <<"verified-stream">>,
        <<"signed-sd-hash">> => maps:get(<<"signed-sd-hash">>, VerifiedStream),
        <<"claim-envelope">> =>
            case ClaimOutput of
                not_found -> not_found;
                Output ->
                    codec(
                        <<"lbry-claim@1.0">>,
                        maps:get(<<"claim">>, Output),
                        #{},
                        Opts
                    )
            end,
        <<"channel">> =>
            codec(
                <<"lbry-channel@1.0">>,
                maps:get(<<"signing_channel">>, Claim, #{}),
                #{},
                Opts
            ),
        <<"attestation">> =>
            codec(
                <<"lbry-channel-attestation@1.0">>,
                maps:get(<<"attestation">>, VerifiedStream),
                #{},
                Opts
            )
    }.

normalize_verify_blobs(Result, Opts) ->
    Result#{
        <<"device">> => <<"odysee@1.0">>,
        <<"descriptor">> =>
            codec(
                <<"lbry-stream-descriptor@1.0">>,
                maps:get(<<"descriptor">>, Result),
                #{},
                Opts
            )
    }.

%% Device responses must survive JSON serialization over HTTP, so raw
%% non-UTF8 binaries (scripts, claim envelopes, hashes) are hex-encoded at
%% this boundary. The bridge and codec layers keep the raw bytes.
hex_binaries(Map) when is_map(Map) ->
    maps:map(fun(_Key, Value) -> hex_binaries(Value) end, Map);
hex_binaries(List) when is_list(List) ->
    [hex_binaries(Value) || Value <- List];
hex_binaries(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin, utf8) of
        Bin -> Bin;
        _ -> hb_util:to_hex(Bin)
    end;
hex_binaries(Other) ->
    Other.

stream_claim(Claim, Opts) ->
    codec(<<"lbry-stream@1.0">>, Claim, #{}, Opts).

stream_claim_source(Claim, #{ <<"claim-envelope">> := Envelope }) ->
    Claim#{ <<"claim-envelope">> => Envelope };
stream_claim_source(Claim, _ClaimOutput) ->
    Claim.

codec(Device, Msg, Req, Opts) ->
    hb_message:convert(Msg, <<"structured@1.0">>, Req#{ <<"device">> => Device }, Opts).

with_target(Base, Req, Opts, Fun) ->
    case param(Base, Req, target_keys(), Opts) of
        {ok, Target} -> {ok, Fun(Target)};
        Error -> error_response(Error)
    end.

with_txid(Base, Req, Opts, Fun) ->
    case param(Base, Req, [<<"txid">>, <<"tx-id">>, <<"tx_id">>], Opts) of
        {ok, TxID} -> {ok, Fun(TxID)};
        Error -> error_response(Error)
    end.

with_sd_hash(Base, Req, Opts, Fun) ->
    case param(Base, Req, [<<"sd-hash">>, <<"sd_hash">>, <<"sdhash">>], Opts) of
        {ok, SDHash} -> {ok, Fun(SDHash)};
        Error -> error_response(Error)
    end.

with_blob_hash(Base, Req, Opts, Fun) ->
    case param(Base, Req, [<<"hash">>, <<"blob-hash">>, <<"blob_hash">>], Opts) of
        {ok, Hash} -> {ok, Fun(Hash)};
        Error -> error_response(Error)
    end.

with_media_source(Base, Req, Opts, Fun) ->
    case param(Base, Req, [<<"sd-hash">>, <<"sd_hash">>, <<"sdhash">>], Opts) of
        {ok, SDHash} ->
            {Size, SizeSource} = exact_stream_size(SDHash, undefined, Opts),
            {ok, Fun(optional_source_fields(#{
                <<"sd-hash">> => SDHash,
                <<"byte-size">> => Size,
                <<"byte-size-source">> => SizeSource
            }))};
        _ ->
            with_target(Base, Req, Opts, fun(Target) ->
                map_result(
                    hb_lbry_proxy:claim(Target, Opts),
                    fun(Claim) ->
                        case media_source_from_claim(Claim, Opts) of
                            {ok, Source} -> Fun(Source#{ <<"target">> => Target });
                            Error -> error_map(Error)
                        end
                    end
                )
            end)
    end.

target_keys() ->
    [
        <<"claim-id">>,
        <<"claim_id">>,
        <<"claim">>,
        <<"url">>,
        <<"name">>,
        <<"target">>
    ].

param(Base, Req, Keys, Opts) ->
    case hb_maps:get_first(param_paths(Base, Req, Keys), not_found, Opts) of
        Value when is_binary(Value), byte_size(Value) > 0 -> {ok, Value};
        _ -> {error, {missing_required, hd(Keys)}}
    end.

param_paths(Base, Req, Keys) ->
    lists:flatmap(
        fun(Key) ->
            [{Req, Key}, {Base, Key}]
        end,
        Keys
    ).

integer_param(Base, Req, Key, Default, Opts) ->
    case hb_maps:get_first([{Req, Key}, {Base, Key}], Default, Opts) of
        Int when is_integer(Int) -> Int;
        Bin when is_binary(Bin) ->
            try binary_to_integer(Bin) of
                Parsed -> Parsed
            catch
                _:_ -> Default
            end;
        _ -> Default
    end.

explicit_range(Base, Req, Opts) ->
    case {integer_param(Base, Req, <<"start">>, undefined, Opts),
          integer_param(Base, Req, <<"end">>, undefined, Opts)} of
        {Start, End} when is_integer(Start), is_integer(End), End >= Start ->
            {ok, Start, End};
        _ ->
            {error, missing_range}
    end.

request_range(Base, Req, Opts) ->
    case explicit_range(Base, Req, Opts) of
        {ok, _, _} = Explicit ->
            Explicit;
        _ ->
            case param(Base, Req, [<<"range">>], Opts) of
                {ok, Range} -> parse_range(Range, Opts);
                _ -> {ok, 0, default_range_size(Opts) - 1}
            end
    end.

parse_range(<<"bytes=", Spec/binary>>, Opts) ->
    case binary:split(Spec, <<"-">>) of
        [StartBin, EndBin] when byte_size(StartBin) > 0 ->
            maybe
                {ok, Start} ?= parse_nonnegative_integer(StartBin),
                {ok, End} ?= range_end(Start, EndBin, Opts),
                true ?= End >= Start orelse {error, invalid_range},
                {ok, Start, End}
            end;
        _ ->
            {error, invalid_range}
    end;
parse_range(_, _Opts) ->
    {error, invalid_range}.

range_end(Start, <<>>, Opts) ->
    {ok, Start + default_range_size(Opts) - 1};
range_end(_Start, EndBin, _Opts) ->
    parse_nonnegative_integer(EndBin).

default_range_size(Opts) ->
    hb_maps:get(<<"odysee-default-range-size">>, Opts, ?DEFAULT_RANGE_SIZE, Opts).

parse_nonnegative_integer(Bin) ->
    try binary_to_integer(Bin) of
        Int when Int >= 0 -> {ok, Int};
        _ -> {error, invalid_integer}
    catch
        _:_ -> {error, invalid_integer}
    end.

bounded_range(Source, Start, End) ->
    case maps:get(<<"byte-size">>, Source, undefined) of
        undefined ->
            {ok, Start, End};
        Size when Start < Size ->
            {ok, Start, min(End, Size - 1)};
        _ ->
            {error, invalid_range}
    end.

content_range(Start, End, undefined) ->
    content_range(Start, End, <<"*">>);
content_range(Start, End, Total) when is_integer(Total) ->
    content_range(Start, End, integer_to_binary(Total));
content_range(Start, End, Total) ->
    iolist_to_binary([
        <<"bytes ">>,
        integer_to_binary(Start),
        <<"-">>,
        integer_to_binary(End),
        <<"/">>,
        Total
    ]).

media_source_from_claim(Claim, Opts) ->
    case hb_util:deep_get([<<"value">>, <<"source">>], Claim, #{}) of
        Source when is_map(Source) ->
            case maps:get(<<"sd_hash">>, Source, undefined) of
                undefined ->
                    {error, missing_sd_hash};
                SDHash ->
                    ClaimSize = integer_value(maps:get(<<"size">>, Source, undefined)),
                    {Size, SizeSource} = exact_stream_size(SDHash, ClaimSize, Opts),
                    {ok, optional_source_fields(#{
                        <<"sd-hash">> => SDHash,
                        <<"claim-id">> => maps:get(<<"claim_id">>, Claim, undefined),
                        <<"byte-size">> => Size,
                        <<"byte-size-source">> => SizeSource,
                        <<"content-type">> => maps:get(<<"media_type">>, Source, undefined),
                        <<"filename">> => maps:get(<<"name">>, Source, undefined)
                    })}
            end;
        _ ->
            {error, missing_source}
    end.

optional_source_fields(Source) ->
    maps:filter(
        fun(_Key, Value) ->
            Value =/= undefined
        end,
        Source
    ).

integer_value(Value) when is_integer(Value) ->
    Value;
integer_value(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Int -> Int
    catch
        _:_ -> undefined
    end;
integer_value(_) ->
    undefined.

exact_stream_size(SDHash, Fallback, Opts) ->
    case hb_lbry_bridge:stream_size(SDHash, Opts) of
        {ok, #{ <<"byte-size">> := Size }} -> {Size, <<"descriptor-last-blob">>};
        _ when Fallback =/= undefined -> {Fallback, <<"claim-source-size">>};
        _ -> {undefined, undefined}
    end.

claim_nout(Claim) ->
    case maps:get(<<"nout">>, Claim, undefined) of
        Nout when is_integer(Nout) -> Nout;
        Nout when is_binary(Nout) ->
            try binary_to_integer(Nout) of
                Int -> Int
            catch
                _:_ -> undefined
            end;
        _ -> undefined
    end.

claim_output(_Tx, undefined) ->
    not_found;
claim_output(Tx, Nout) ->
    case [
        Output
     ||
        Output <- maps:get(<<"outputs">>, Tx, []),
        maps:get(<<"nout">>, Output, undefined) == Nout,
        maps:is_key(<<"claim">>, Output)
    ] of
        [Output | _] -> Output;
        [] -> not_found
    end.

map_result({ok, Value}, Fun) ->
    Fun(Value);
map_result({error, Reason}, _Fun) ->
    error_map(Reason);
map_result({failure, Reason}, _Fun) ->
    error_map({failure, Reason}).

error_response({error, Reason}) ->
    {ok, error_map(Reason)};
error_response(Reason) ->
    {ok, error_map(Reason)}.

error_map(Reason) ->
    #{
        <<"status">> => status_for(Reason),
        <<"content-type">> => <<"text/plain">>,
        <<"error">> => error_term(Reason),
        <<"body">> => hb_util:bin(io_lib:format("~p", [Reason]))
    }.

status_for({missing_required, _}) -> 400;
status_for({error, Reason}) -> status_for(Reason);
status_for(missing_range) -> 416;
status_for(invalid_range) -> 416;
status_for(invalid_integer) -> 400;
status_for(not_found) -> 404;
status_for({http_status, 403, _}) -> 403;
status_for(protected) -> 403;
status_for(protected_content) -> 403;
status_for({hash_mismatch, _, _}) -> 502;
status_for({failure, _}) -> 502;
status_for({invalid_attestation, _, _}) -> 502;
status_for(_) -> 500.

error_term(Reason) when is_atom(Reason) ->
    hb_util:bin(Reason);
error_term({error, Reason}) ->
    error_term(Reason);
error_term({Reason, _}) when is_atom(Reason) ->
    hb_util:bin(Reason);
error_term({Reason, _, _}) when is_atom(Reason) ->
    hb_util:bin(Reason);
error_term(_) ->
    <<"error">>.
