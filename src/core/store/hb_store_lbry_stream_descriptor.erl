%%% @doc Read-only LBRY stream descriptor source store.
%%%
%%% The native descriptor key is the descriptor blob SHA-384, commonly exposed
%%% as `sd_hash'. This store maps native descriptor keys onto `hb_store_odysee'
%%% fetch mechanics and returns `~lbry-stream-descriptor@1.0' commitments.
-module(hb_store_lbry_stream_descriptor).
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([read/3, type/3, resolve/3, list/3]).
-export([write/3, group/3, link/3]).
-include_lib("eunit/include/eunit.hrl").

-define(SHA384_HEX_SIZE, 96).

start(_StoreOpts, _Req, _NodeOpts) ->
    ok.

stop(_StoreOpts, _Req, _NodeOpts) ->
    ok.

reset(_StoreOpts, _Req, _NodeOpts) ->
    ok.

scope() ->
    remote.

scope(#{ <<"scope">> := Scope }) ->
    Scope;
scope(_StoreOpts) ->
    scope().

resolve(_StoreOpts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    case descriptor_path(Key) of
        {ok, Path} -> {ok, Path};
        Error -> Error
    end.

type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, Msg} when is_map(Msg) -> {ok, composite};
        {ok, _Bin} -> {ok, simple};
        Error -> Error
    end.

read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    case descriptor_path(Key) of
        {ok, Path} ->
            hb_store_odysee:read(StoreOpts, #{ <<"read">> => Path }, NodeOpts);
        Error ->
            Error
    end.

list(_StoreOpts, _Req, _NodeOpts) ->
    {error, not_found}.

write(_StoreOpts, _Req, _NodeOpts) ->
    {error, read_only}.

group(_StoreOpts, _Req, _NodeOpts) ->
    {error, read_only}.

link(_StoreOpts, _Req, _NodeOpts) ->
    {error, read_only}.

descriptor_path(Key0) ->
    Key = normalize_key(Key0),
    case Key of
        <<"lbry/descriptor/", Hash/binary>> -> hash_path(Hash);
        <<"lbry/descriptor-id/", Hash/binary>> -> hash_path(Hash);
        <<"lbry/stream-descriptor/", Hash/binary>> -> hash_path(Hash);
        <<"odysee/descriptor/", _Hash/binary>> -> {ok, Key};
        <<"odysee/descriptor-id/", Hash/binary>> -> hash_path(Hash);
        <<"odysee/stream-descriptor/", Hash/binary>> -> hash_path(Hash);
        Hash when byte_size(Hash) =:= ?SHA384_HEX_SIZE -> hash_path(Hash);
        _ -> {error, not_found}
    end.

hash_path(Hash0) ->
    Hash = normalize_hex(Hash0),
    case byte_size(Hash) of
        ?SHA384_HEX_SIZE -> {ok, <<"odysee/descriptor/", Hash/binary>>};
        _ -> {error, invalid_descriptor_hash}
    end.

normalize_key(<<"/", Rest/binary>>) ->
    normalize_key(Rest);
normalize_key(Key) when is_binary(Key) ->
    Key;
normalize_key(Key) ->
    hb_path:to_binary(Key).

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:bin(string:lowercase(binary_to_list(Hex))).

-ifdef(TEST).

read_native_sd_hash_fixture_test() ->
    {Descriptor, SDHash, Desc} = descriptor_fixture(),
    Store = #{
        <<"store-module">> => hb_store_lbry_stream_descriptor,
        <<"fixtures">> => #{
            <<"odysee/descriptor/", SDHash/binary>> => Desc
        }
    },
    {ok, Msg} = hb_store:read(Store, SDHash, #{}),
    ?assertEqual(<<"lbry-stream-descriptor@1.0">>, hb_maps:get(<<"device">>, Msg, #{})),
    ?assertEqual(Descriptor, hb_maps:get(<<"body">>, Msg, #{})),
    ?assert(lists:member(
        <<"lbry-stream-descriptor@1.0">>,
        hb_message:commitment_devices(Msg, #{})
    )),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})).

source_verify_req(Msg) ->
    #{
        <<"commitment-ids">> =>
            [
                ID
            ||
                {ID, Commitment} <- maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, #{})),
                hb_maps:get(<<"commitment-device">>, Commitment, not_found, #{})
                    =:= <<"lbry-stream-descriptor@1.0">>
            ]
    }.

descriptor_fixture() ->
    StreamNameHex = hb_util:to_hex(<<"verified.mp4">>),
    KeyHex = <<"000102030405060708090a0b0c0d0e0f">>,
    SuggestedHex = StreamNameHex,
    BlobHash = hb_util:to_hex(crypto:hash(sha384, <<"encrypted blob">>)),
    Blob = #{
        <<"blob_num">> => 0,
        <<"blob_hash">> => BlobHash,
        <<"iv">> => <<"00112233445566778899aabbccddeeff">>,
        <<"length">> => 16
    },
    Terminator = #{
        <<"blob_num">> => 1,
        <<"iv">> => <<"ffeeddccbbaa99887766554433221100">>,
        <<"length">> => 0
    },
    StreamHash = descriptor_stream_hash(StreamNameHex, KeyHex, SuggestedHex, [Blob, Terminator]),
    JSON =
        hb_json:encode(#{
            <<"stream_type">> => <<"lbryfile">>,
            <<"stream_name">> => StreamNameHex,
            <<"key">> => KeyHex,
            <<"suggested_file_name">> => SuggestedHex,
            <<"stream_hash">> => StreamHash,
            <<"blobs">> => [Blob, Terminator]
        }),
    SDHash = hb_util:to_hex(crypto:hash(sha384, JSON)),
    {ok, Desc} =
        hb_ao:raw(
            <<"odysee-stream-descriptor@1.0">>,
            <<"decode">>,
            #{},
            #{ <<"body">> => JSON, <<"sd-hash">> => SDHash },
            #{}
        ),
    {JSON, SDHash, Desc}.

descriptor_stream_hash(StreamNameHex, KeyHex, SuggestedHex, Blobs) ->
    BlobSums =
        iolist_to_binary([
            descriptor_blob_hashsum(hb_ao:normalize_keys(Blob, #{}))
        ||
            Blob <- Blobs
        ]),
    BlobDigest = crypto:hash(sha384, BlobSums),
    hb_util:to_hex(
        crypto:hash(
            sha384,
            <<StreamNameHex/binary, KeyHex/binary, SuggestedHex/binary, BlobDigest/binary>>
        )
    ).

descriptor_blob_hashsum(Blob) ->
    Length = hb_maps:get(<<"length">>, Blob, #{}),
    BlobNum = first_value([<<"blob-num">>, <<"blob_num">>], Blob, #{}),
    IV = hb_maps:get(<<"iv">>, Blob, #{}),
    HashPrefix =
        case first_value([<<"blob-hash">>, <<"blob_hash">>], Blob, #{}) of
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

first_value([], _Map, _Opts) ->
    not_found;
first_value([Key | Rest], Map, Opts) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> first_value(Rest, Map, Opts);
        Value -> Value
    end.

-endif.
