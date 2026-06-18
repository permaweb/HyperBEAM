%%% @doc A read-only store sourcing LBRY stream descriptors by `sd_hash'.
%%% Stream descriptors are stored as encrypted LBRY blobs, so this store first
%%% verifies the requested SHA-384 bytes through `hb_store_lbry_blob', then
%%% parses them as a descriptor and returns a native
%%% `lbry-stream-descriptor@1.0' committed message.
-module(hb_store_lbry_stream_descriptor).
-export([scope/0, scope/1, type/3, read/3, resolve/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

scope() -> remote.
scope(_) -> scope().

resolve(_StoreOpts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    case normalize_key(Key) of
        {ok, Hash, _Mode} -> {ok, Hash};
        error -> {error, not_found}
    end.

type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, _} -> {ok, simple};
        Error -> Error
    end.

read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    case normalize_key(Key) of
        {ok, Hash, Mode} ->
            case descriptor_bytes(StoreOpts, Hash, NodeOpts) of
                {ok, Bytes} -> descriptor_message(Hash, Bytes, Mode);
                Error -> Error
            end;
        error ->
            {error, not_found}
    end.

descriptor_message(Hash, Bytes, Mode) ->
    case hb_lbry_commitment:descriptor_message(Bytes, Hash) of
        {ok, Msg} ->
            {ok, Msg};
        {error, _} = Error ->
            case Mode of
                fallback -> {error, not_found};
                strict -> Error
            end
    end.

descriptor_bytes(StoreOpts, Hash, Opts) ->
    case descriptor_fixture(StoreOpts, Hash, Opts) of
        not_found ->
            blob_store_bytes(StoreOpts, Hash, Opts);
        Result ->
            Result
    end.

descriptor_fixture(StoreOpts, Hash, Opts) ->
    Fixtures = hb_maps:get(<<"fixtures">>, StoreOpts, #{}, Opts),
    Keys = [
        <<"lbry/descriptor/", Hash/binary>>,
        <<"lbry/stream-descriptor/", Hash/binary>>,
        <<"odysee/descriptor/", Hash/binary>>,
        <<"odysee/descriptor-id/", Hash/binary>>,
        <<"odysee/stream-descriptor/", Hash/binary>>,
        Hash
    ],
    case first_fixture(Keys, Fixtures, Opts) of
        not_found -> not_found;
        Msg -> descriptor_bytes_from_fixture(Msg, Opts)
    end.

first_fixture([], _Fixtures, _Opts) ->
    not_found;
first_fixture([Key | Rest], Fixtures, Opts) ->
    case hb_maps:get(Key, Fixtures, not_found, Opts) of
        not_found -> first_fixture(Rest, Fixtures, Opts);
        Msg -> Msg
    end.

descriptor_bytes_from_fixture(Msg0, Opts) ->
    Msg =
        case is_map(Msg0) of
            true -> Msg0;
            false -> Msg0
        end,
    case Msg of
        Bytes when is_binary(Bytes) ->
            {ok, Bytes};
        Map when is_map(Map) ->
            Loaded = hb_cache:ensure_all_loaded(Map, Opts),
            case hb_maps:get(<<"raw">>, Loaded, not_found, Opts) of
                Bytes when is_binary(Bytes) -> {ok, Bytes};
                _ ->
                    case hb_maps:get(<<"body">>, Loaded, not_found, Opts) of
                        Bytes when is_binary(Bytes) -> {ok, Bytes};
                        _ -> {error, missing_descriptor_bytes}
                    end
            end;
        _ ->
            {error, missing_descriptor_bytes}
    end.

blob_store_bytes(StoreOpts, Hash, Opts) ->
    BlobStore = StoreOpts#{ <<"store-module">> => hb_store_lbry_blob },
    case hb_store_lbry_blob:read(BlobStore, #{ <<"read">> => Hash }, Opts) of
        {ok, Msg0} ->
            Msg = hb_cache:ensure_all_loaded(Msg0, Opts),
            case hb_maps:get(<<"data">>, Msg, not_found, Opts) of
                Bytes when is_binary(Bytes) -> {ok, Bytes};
                _ -> {error, missing_blob_data}
            end;
        Error ->
            Error
    end.

normalize_key(Key0) ->
    Key = strip_slash(hb_path:to_binary(Key0)),
    case descriptor_path_hash(Key) of
        {ok, Hash} ->
            {ok, Hash, strict};
        error ->
            case valid_hash(Key) of
                true -> {ok, hb_util:to_lower(Key), fallback};
                false -> error
            end
    end.

descriptor_path_hash(<<"lbry/descriptor/", Hash/binary>>) ->
    explicit_hash(Hash);
descriptor_path_hash(<<"lbry/stream-descriptor/", Hash/binary>>) ->
    explicit_hash(Hash);
descriptor_path_hash(<<"odysee/descriptor/", Hash/binary>>) ->
    explicit_hash(Hash);
descriptor_path_hash(<<"odysee/descriptor-id/", Hash/binary>>) ->
    explicit_hash(Hash);
descriptor_path_hash(<<"odysee/stream-descriptor/", Hash/binary>>) ->
    explicit_hash(Hash);
descriptor_path_hash(_Key) ->
    error.

explicit_hash(Hash) ->
    case valid_hash(Hash) of
        true -> {ok, hb_util:to_lower(Hash)};
        false -> error
    end.

strip_slash(<<"/", Rest/binary>>) ->
    strip_slash(Rest);
strip_slash(Key) ->
    Key.

valid_hash(Hash) when is_binary(Hash), byte_size(Hash) == 96 ->
    try binary:decode_hex(Hash) of
        Decoded -> byte_size(Decoded) == 48
    catch
        _:_ -> false
    end;
valid_hash(_) ->
    false.

%%% Tests

read_returns_committed_descriptor_message_test() ->
    {Raw, SDHash} = sample_descriptor(),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{
            <<"lbry/descriptor/", SDHash/binary>> => Raw
        }
    },
    {ok, Msg} = read(Store, #{ <<"read">> => <<"lbry/descriptor/", SDHash/binary>> }, #{}),
    ?assertEqual(<<"lbry-stream-descriptor@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(SDHash, maps:get(<<"sd-hash">>, Msg)),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

direct_hash_http_get_exposes_native_signature_input_test() ->
    application:ensure_all_started(inets),
    {Raw, SDHash} = sample_descriptor(),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{ SDHash => Raw }
    },
    Node = hb_http_server:start_node(#{ <<"store">> => [Store] }),
    URL = binary_to_list(<<Node/binary, SDHash/binary>>),
    {ok, {{_, 200, _}, Headers, _Body}} =
        httpc:request(get, {URL, []}, [], [{body_format, binary}]),
    SignatureInput = http_header(<<"signature-input">>, Headers),
    ?assertNotEqual(not_found, SignatureInput),
    ?assertNotEqual(
        nomatch,
        binary:match(
            SignatureInput,
            <<"alg=\"lbry-stream-descriptor@1.0/sha-384\"">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(SignatureInput, <<"native-id=\"", SDHash/binary, "\"">>)
    ).

bare_non_descriptor_hash_returns_not_found_test() ->
    Bytes = <<"encrypted blob, not descriptor json">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{ Hash => Bytes }
    },
    ?assertEqual({error, not_found}, read(Store, #{ <<"read">> => Hash }, #{})).

explicit_non_descriptor_hash_fails_test() ->
    Bytes = <<"encrypted blob, not descriptor json">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Store = #{
        <<"store-module">> => ?MODULE,
        <<"fixtures">> => #{ <<"lbry/descriptor/", Hash/binary>> => Bytes }
    },
    ?assertMatch(
        {error, _},
        read(Store, #{ <<"read">> => <<"lbry/descriptor/", Hash/binary>> }, #{})
    ).

store_stack_falls_back_to_blob_for_non_descriptor_hash_test() ->
    Bytes = <<"encrypted blob, not descriptor json">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Store = [
        #{
            <<"store-module">> => ?MODULE,
            <<"fixtures">> => #{ Hash => Bytes }
        },
        #{
            <<"store-module">> => hb_store_lbry_blob,
            <<"fixtures">> => #{ Hash => Bytes }
        }
    ],
    {ok, Msg} = hb_store:read(Store, Hash, #{}),
    ?assertEqual(<<"lbry-blob@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(Hash, maps:get(<<"blob-hash">>, Msg)).

sample_descriptor() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31>>,
    Ciphertext = crypto:crypto_one_time(
        aes_128_cbc,
        Key,
        IV,
        pkcs7_pad(<<"hello verified legacy stream">>),
        true
    ),
    BlobHash = hb_lbry_stream_descriptor:blob_hash(Ciphertext),
    Descriptor =
        #{
            <<"stream_type">> => <<"lbryfile">>,
            <<"stream_name">> => hb_util:to_hex(<<"sample.mp4">>),
            <<"key">> => hb_util:to_hex(Key),
            <<"suggested_file_name">> => hb_util:to_hex(<<"sample.mp4">>),
            <<"stream_hash">> => hb_lbry_stream_descriptor:blob_hash(<<"stream hash test">>),
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
    Raw = hb_json:encode(Descriptor),
    {Raw, hb_lbry_stream_descriptor:descriptor_hash(Raw)}.

pkcs7_pad(Data) ->
    PadLen = 16 - (byte_size(Data) rem 16),
    <<Data/binary, (binary:copy(<<PadLen>>, PadLen))/binary>>.

http_header(Name, Headers) ->
    LowerName = hb_util:bin(string:lowercase(hb_util:bin(Name))),
    case [
        hb_util:bin(Value)
     ||
        {Key, Value} <- Headers,
        hb_util:bin(string:lowercase(hb_util:bin(Key))) == LowerName
    ] of
        [Value | _] -> Value;
        [] -> not_found
    end.
