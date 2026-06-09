-module(hb_lbry_stream_descriptor).
-export([
    parse/1, parse/2,
    descriptor_hash/1,
    blob_hash/1,
    verify_blob_hash/2,
    reassemble/2,
    stream_size/2,
    decrypt_blob/3,
    pkcs7_unpad/1
]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MAX_BLOB_SIZE, 2_097_152).
-define(MAX_PLAIN_BLOB_SIZE, 2_097_151).

parse(Raw) when is_binary(Raw) ->
    try hb_json:decode(Raw) of
        JSON when is_map(JSON) ->
            parse_json(Raw, JSON, undefined);
        _ ->
            {error, invalid_descriptor_json}
    catch
        _:_ ->
            {error, invalid_descriptor_json}
    end.

parse(Raw, ExpectedSDHash) ->
    case parse(Raw) of
        {ok, Descriptor} ->
            case verify_blob_hash(ExpectedSDHash, Raw) of
                ok ->
                    {ok, Descriptor#{ <<"sd-hash">> => normalize_hex(ExpectedSDHash) }};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

descriptor_hash(Raw) ->
    blob_hash(Raw).

blob_hash(Bytes) when is_binary(Bytes) ->
    hb_util:to_hex(crypto:hash(sha384, Bytes)).

verify_blob_hash(ExpectedHash, Bytes) when is_binary(Bytes) ->
    ActualHash = blob_hash(Bytes),
    case normalize_hex(ExpectedHash) of
        ActualHash -> ok;
        Normalized -> {error, {hash_mismatch, Normalized, ActualHash}}
    end.

reassemble(RawDescriptor, Fetch) when is_binary(RawDescriptor) ->
    case parse(RawDescriptor) of
        {ok, Descriptor} -> reassemble(Descriptor, Fetch);
        Error -> Error
    end;
reassemble(#{ <<"key">> := KeyHex, <<"blobs">> := Blobs }, Fetch)
        when is_function(Fetch, 1) ->
    DataBlobs =
        lists:filter(
            fun(Blob) ->
                maps:get(<<"terminator">>, Blob, false) =/= true
            end,
            Blobs
        ),
    reassemble_blobs(DataBlobs, KeyHex, Fetch, []).

stream_size(RawDescriptor, Fetch) when is_binary(RawDescriptor) ->
    case parse(RawDescriptor) of
        {ok, Descriptor} -> stream_size(Descriptor, Fetch);
        Error -> Error
    end;
stream_size(#{ <<"key">> := KeyHex, <<"blobs">> := Blobs } = Descriptor, Fetch)
        when is_function(Fetch, 1) ->
    DataBlobs =
        lists:filter(
            fun(Blob) ->
                maps:get(<<"terminator">>, Blob, false) =/= true
            end,
            Blobs
        ),
    case DataBlobs of
        [] ->
            {ok, 0};
        _ ->
            LastBlob = lists:last(DataBlobs),
            FullBlobCount = length(DataBlobs) - 1,
            Stride = maps:get(<<"plain-blob-stride">>, Descriptor, ?MAX_PLAIN_BLOB_SIZE),
            maybe
                {ok, LastPlaintext} ?= fetch_and_decrypt_blob(LastBlob, KeyHex, Fetch),
                {ok, (Stride * FullBlobCount) + byte_size(LastPlaintext)}
            end
    end.

decrypt_blob(KeyHex, Blob, Ciphertext) ->
    with_hex(KeyHex, 16, key, fun(Key) ->
        with_hex(maps:get(<<"iv">>, Blob), 16, iv, fun(IV) ->
            try crypto:crypto_one_time(aes_128_cbc, Key, IV, Ciphertext, false) of
                Padded ->
                    pkcs7_unpad(Padded)
            catch
                _:_ ->
                    {error, invalid_ciphertext}
            end
        end)
    end).

pkcs7_unpad(<<>>) ->
    {error, invalid_padding};
pkcs7_unpad(Padded) when is_binary(Padded) ->
    Size = byte_size(Padded),
    PadLen = binary:last(Padded),
    case PadLen >= 1 andalso PadLen =< 16 andalso PadLen =< Size of
        true ->
            Padding = binary:part(Padded, Size - PadLen, PadLen),
            case Padding == binary:copy(<<PadLen>>, PadLen) of
                true -> {ok, binary:part(Padded, 0, Size - PadLen)};
                false -> {error, invalid_padding}
            end;
        false ->
            {error, invalid_padding}
    end.

parse_json(Raw, JSON, ExpectedSDHash) ->
    maybe
        {ok, StreamType} ?= require_binary(JSON, <<"stream_type">>),
        {ok, StreamName} ?= require_hex(JSON, <<"stream_name">>, any),
        {ok, Key} ?= require_hex(JSON, <<"key">>, 16),
        {ok, SuggestedFileName} ?= require_hex(JSON, <<"suggested_file_name">>, any),
        {ok, StreamHash} ?= require_hex(JSON, <<"stream_hash">>, 48),
        {ok, RawBlobs} ?= require_list(JSON, <<"blobs">>),
        {ok, Blobs} ?= parse_blobs(RawBlobs),
        Descriptor =
            #{
                <<"device">> => <<"lbry-stream-descriptor@1.0">>,
                <<"raw">> => Raw,
                <<"computed-sd-hash">> => descriptor_hash(Raw),
                <<"stream-type">> => StreamType,
                <<"stream-name">> => StreamName,
                <<"key">> => Key,
                <<"suggested-file-name">> => SuggestedFileName,
                <<"stream-hash">> => StreamHash,
                <<"blobs">> => Blobs,
                <<"data-blob-count">> => length(Blobs) - 1,
                <<"plain-blob-stride">> => ?MAX_PLAIN_BLOB_SIZE
            },
        case ExpectedSDHash of
            undefined -> {ok, Descriptor};
            _ -> {ok, Descriptor#{ <<"sd-hash">> => normalize_hex(ExpectedSDHash) }}
        end
    else
        Error -> Error
    end.

require_binary(JSON, Key) ->
    case maps:get(Key, JSON, undefined) of
        Value when is_binary(Value) -> {ok, Value};
        _ -> {error, {missing_or_invalid, Key}}
    end.

require_hex(JSON, Key, Bytes) ->
    maybe
        {ok, Value} ?= require_binary(JSON, Key),
        ok ?= validate_hex(Value, Bytes),
        {ok, normalize_hex(Value)}
    end.

require_list(JSON, Key) ->
    case maps:get(Key, JSON, undefined) of
        Value when is_list(Value) -> {ok, Value};
        _ -> {error, {missing_or_invalid, Key}}
    end.

parse_blobs([]) ->
    {error, missing_blobs};
parse_blobs(Blobs) ->
    parse_blobs(Blobs, 0, length(Blobs) - 1, []).

parse_blobs([], _Index, _Last, Acc) ->
    {ok, lists:reverse(Acc)};
parse_blobs([Blob | Rest], Index, Last, Acc) when is_map(Blob) ->
    IsLast = Index == Last,
    IsFinalDataBlob = Index == Last - 1,
    case parse_blob(Blob, Index, IsLast, IsFinalDataBlob) of
        {ok, Parsed} -> parse_blobs(Rest, Index + 1, Last, [Parsed | Acc]);
        Error -> Error
    end;
parse_blobs([_ | _], _Index, _Last, _Acc) ->
    {error, invalid_blob_entry}.

parse_blob(Blob, Index, IsLast, IsFinalDataBlob) ->
    maybe
        {ok, Index} ?= require_blob_num(Blob, Index),
        {ok, Length} ?= require_length(Blob, IsLast, IsFinalDataBlob),
        {ok, IV} ?= require_hex(Blob, <<"iv">>, 16),
        Parsed0 =
            #{
                <<"blob-num">> => Index,
                <<"length">> => Length,
                <<"iv">> => IV
            },
        parse_blob_hash(Blob, Parsed0, IsLast)
    end.

require_blob_num(Blob, Index) ->
    case maps:get(<<"blob_num">>, Blob, undefined) of
        Index -> {ok, Index};
        Other -> {error, {invalid_blob_num, Index, Other}}
    end.

require_length(Blob, true, _IsFinalDataBlob) ->
    case maps:get(<<"length">>, Blob, undefined) of
        0 -> {ok, 0};
        Other -> {error, {invalid_terminator_length, Other}}
    end;
require_length(Blob, false, true) ->
    case maps:get(<<"length">>, Blob, undefined) of
        Length when is_integer(Length),
                Length > 0,
                Length =< ?MAX_BLOB_SIZE,
                Length rem 16 == 0 ->
            {ok, Length};
        Other ->
            {error, {invalid_blob_length, Other}}
    end;
require_length(Blob, false, false) ->
    case maps:get(<<"length">>, Blob, undefined) of
        ?MAX_BLOB_SIZE -> {ok, ?MAX_BLOB_SIZE};
        Other -> {error, {invalid_full_blob_length, Other}}
    end.

parse_blob_hash(Blob, Parsed, true) ->
    case maps:is_key(<<"blob_hash">>, Blob) of
        false -> {ok, Parsed#{ <<"terminator">> => true }};
        true -> {error, terminator_has_blob_hash}
    end;
parse_blob_hash(Blob, Parsed, false) ->
    maybe
        {ok, Hash} ?= require_hex(Blob, <<"blob_hash">>, 48),
        {ok, Parsed#{ <<"blob-hash">> => Hash }}
    end.

validate_hex(Value, any) ->
    try binary:decode_hex(Value) of
        _ -> ok
    catch
        _:_ -> {error, invalid_hex}
    end;
validate_hex(Value, Bytes) ->
    try binary:decode_hex(Value) of
        Decoded when byte_size(Decoded) == Bytes -> ok;
        Decoded -> {error, {invalid_hex_size, byte_size(Decoded), Bytes}}
    catch
        _:_ -> {error, invalid_hex}
    end.

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:to_lower(Hex).

with_hex(Hex, Bytes, Name, Fun) ->
    case validate_hex(Hex, Bytes) of
        ok -> Fun(binary:decode_hex(Hex));
        Error -> {error, {invalid_hex_field, Name, Error}}
    end.

reassemble_blobs([], _KeyHex, _Fetch, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc))};
reassemble_blobs([Blob | Rest], KeyHex, Fetch, Acc) ->
    maybe
        {ok, Plaintext} ?= fetch_and_decrypt_blob(Blob, KeyHex, Fetch),
        reassemble_blobs(Rest, KeyHex, Fetch, [Plaintext | Acc])
    end.

fetch_and_decrypt_blob(Blob, KeyHex, Fetch) ->
    Hash = maps:get(<<"blob-hash">>, Blob),
    case Fetch(Hash) of
        {ok, Ciphertext} when is_binary(Ciphertext) ->
            maybe
                ok ?= verify_ciphertext(Blob, Ciphertext),
                decrypt_blob(KeyHex, Blob, Ciphertext)
            end;
        {ok, _} ->
            {error, {invalid_blob_bytes, Hash}};
        Error ->
            Error
    end.

verify_ciphertext(Blob, Ciphertext) ->
    Hash = maps:get(<<"blob-hash">>, Blob),
    ExpectedLength = maps:get(<<"length">>, Blob),
    case byte_size(Ciphertext) of
        ExpectedLength -> verify_blob_hash(Hash, Ciphertext);
        ActualLength -> {error, {length_mismatch, Hash, ExpectedLength, ActualLength}}
    end.

parse_and_reassemble_test() ->
    {Raw, Plaintext, Blobs} = sample_descriptor(),
    {ok, Descriptor} = parse(Raw),
    ?assertEqual(
        descriptor_hash(Raw),
        maps:get(<<"computed-sd-hash">>, Descriptor)
    ),
    Fetch =
        fun(Hash) ->
            {ok, maps:get(Hash, Blobs)}
        end,
    ?assertEqual({ok, Plaintext}, reassemble(Descriptor, Fetch)).

stream_size_fetches_only_last_blob_test() ->
    {Raw, Plaintext, Blobs} = sample_descriptor(),
    {ok, Descriptor} = parse(Raw),
    DataBlobs =
        lists:filter(
            fun(Blob) ->
                maps:get(<<"terminator">>, Blob, false) =/= true
            end,
            maps:get(<<"blobs">>, Descriptor)
        ),
    LastHash = maps:get(<<"blob-hash">>, lists:last(DataBlobs)),
    Fetch =
        fun(Hash) ->
            ?assertEqual(LastHash, Hash),
            {ok, maps:get(Hash, Blobs)}
        end,
    ?assertEqual({ok, byte_size(Plaintext)}, stream_size(Descriptor, Fetch)).

parse_with_sd_hash_test() ->
    {Raw, _Plaintext, _Blobs} = sample_descriptor(),
    SDHash = descriptor_hash(Raw),
    {ok, Descriptor} = parse(Raw, SDHash),
    ?assertEqual(SDHash, maps:get(<<"sd-hash">>, Descriptor)).

reassemble_rejects_hash_mismatch_test() ->
    {Raw, _Plaintext, Blobs} = sample_descriptor(),
    {ok, Descriptor} = parse(Raw),
    [First | _] =
        lists:filter(
            fun(Blob) ->
                maps:get(<<"terminator">>, Blob, false) =/= true
            end,
            maps:get(<<"blobs">>, Descriptor)
        ),
    FirstHash = maps:get(<<"blob-hash">>, First),
    BadBlobs = Blobs#{ FirstHash := <<"not the encrypted bytes">> },
    Fetch =
        fun(Hash) ->
            {ok, maps:get(Hash, BadBlobs)}
        end,
    ?assertMatch(
        {error, {length_mismatch, FirstHash, _, _}},
        reassemble(Descriptor, Fetch)
    ).

parse_rejects_bad_terminator_test() ->
    {Raw, _Plaintext, _Blobs} = sample_descriptor(),
    JSON = hb_json:decode(Raw),
    [Data, Terminator] = maps:get(<<"blobs">>, JSON),
    BadTerminator = Terminator#{ <<"blob_hash">> => blob_hash(<<"bad">>) },
    BadJSON = JSON#{ <<"blobs">> => [Data, BadTerminator] },
    ?assertEqual(
        {error, terminator_has_blob_hash},
        parse(hb_json:encode(BadJSON))
    ).

parse_rejects_short_non_final_data_blob_test() ->
    {Raw, _Plaintext, _Blobs} = sample_descriptor(),
    JSON = hb_json:decode(Raw),
    [Data, Terminator] = maps:get(<<"blobs">>, JSON),
    SecondData = Data#{
        <<"blob_num">> => 1,
        <<"blob_hash">> => blob_hash(<<"second">>)
    },
    BadJSON = JSON#{ <<"blobs">> => [Data, SecondData, Terminator#{ <<"blob_num">> => 2 }] },
    ?assertMatch(
        {error, {invalid_full_blob_length, _}},
        parse(hb_json:encode(BadJSON))
    ).

parse_old_sort_descriptor_test() ->
    Raw = old_sort_descriptor(),
    SDHash = <<"9313d1807551186126acc3662e74d9de29cede78d4f133349ace846273ef116b9bb86be86c54509eb84840e4b032f6b2">>,
    {ok, Descriptor} = parse(Raw, SDHash),
    ?assertEqual(SDHash, maps:get(<<"sd-hash">>, Descriptor)),
    ?assertEqual(SDHash, maps:get(<<"computed-sd-hash">>, Descriptor)),
    ?assertEqual(1, maps:get(<<"data-blob-count">>, Descriptor)).

sample_descriptor() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31>>,
    Plaintext = <<"hello verified legacy stream">>,
    Ciphertext = encrypt_blob(Key, IV, Plaintext),
    BlobHash = blob_hash(Ciphertext),
    Descriptor =
        #{
            <<"stream_type">> => <<"lbryfile">>,
            <<"stream_name">> => hb_util:to_hex(<<"sample.mp4">>),
            <<"key">> => hb_util:to_hex(Key),
            <<"suggested_file_name">> => hb_util:to_hex(<<"sample.mp4">>),
            <<"stream_hash">> => blob_hash(<<"stream hash test">>),
            <<"blobs">> => [
                #{
                    <<"length">> => byte_size(Ciphertext),
                    <<"blob_num">> => 0,
                    <<"iv">> => hb_util:to_hex(IV),
                    <<"blob_hash">> => BlobHash
                },
                #{
                    <<"length">> => 0,
                    <<"blob_num">> => 1,
                    <<"iv">> => hb_util:to_hex(<<0:128>>)
                }
            ]
        },
    {hb_json:encode(Descriptor), Plaintext, #{ BlobHash => Ciphertext }}.

encrypt_blob(Key, IV, Plaintext) ->
    crypto:crypto_one_time(aes_128_cbc, Key, IV, pkcs7_pad(Plaintext), true).

old_sort_descriptor() ->
    iolist_to_binary([
        <<"{\"stream_name\": \"4f62616d6120446f6e6b65792d322e73746c\", ">>,
        <<"\"blobs\": [{\"length\": 1153488, \"blob_num\": 0, ">>,
        <<"\"blob_hash\": \"9fa32a249ce3f2d4e46b78599800f368b72f2a7f22b81df443c7f6bdbef496bd61b4c0079c73d79c8bb9be9a6bf86592\", ">>,
        <<"\"iv\": \"0bf348867244019c9e22196339016ea6\"}, ">>,
        <<"{\"length\": 0, \"blob_num\": 1, ">>,
        <<"\"iv\": \"9f36abae16955463919b07ed530a3d18\"}], ">>,
        <<"\"stream_type\": \"lbryfile\", ">>,
        <<"\"key\": \"a03742b87628aa7228e48f1dcd207e48\", ">>,
        <<"\"suggested_file_name\": \"4f62616d6120446f6e6b65792d322e73746c\", ">>,
        <<"\"stream_hash\": \"b43f4b1379780caf60d20aa06ac38fb144df61e514ebfa97537018ba73bce8fe37ae712f473ff0ba0be0eef44e160207\"}">>
    ]).

pkcs7_pad(Plaintext) ->
    PadLen = 16 - (byte_size(Plaintext) rem 16),
    <<Plaintext/binary, (binary:copy(<<PadLen>>, PadLen))/binary>>.
