%%% @doc LBRY stream descriptor source commitment device.
%%%
%%% Descriptor decoding and media reconstruction are delegated to the Odysee
%%% compatibility device, while commitments bind the native descriptor bytes,
%%% `sd_hash', stream hash, and blob store paths.
-module(dev_lbry_stream_descriptor).
-implements(<<"lbry-stream-descriptor@1.0">>).
-export([info/1, commit/3, decode/3, fetch/3, verify/3, reconstruct/3, media/3, to_hint/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"lbry-stream-descriptor@1.0">>).

info(_Opts) ->
    #{
        exports => [
            <<"commit">>,
            <<"decode">>,
            <<"fetch">>,
            <<"verify">>,
            <<"reconstruct">>,
            <<"media">>,
            <<"to-hint">>
        ]
    }.

commit(Base, _Req, Opts) ->
    safe(fun() ->
        Msg = remove_matching_commitments(Base, Opts),
        ok = require_descriptor_source(Msg, Opts),
        {ok, SDHash} = descriptor_hash(Msg, Opts),
        CommittedKeys = committed_keys(Msg, Opts),
        Digest = source_digest(Msg, CommittedKeys, Opts),
        {ok, NativeFields} =
            hb_lbry_commitment:native_id_fields(<<"sd-hash">>, SDHash),
        Commitment0 = maps:merge(NativeFields, #{
            <<"commitment-device">> => ?DEVICE,
            <<"type">> => <<"stream-descriptor">>,
            <<"committed">> => hb_util:list_to_numbered_message(CommittedKeys),
            <<"hash-algorithm">> => <<"sha384">>,
            <<"source-digest">> => Digest
        }),
        Commitment = add_evidence(Msg, Commitment0, Opts),
        ID = commitment_id(Commitment),
        Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
        {ok, Msg#{ <<"commitments">> => Commitments#{ ID => Commitment } }}
    end).

decode(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"decode">>, Base, Req, Opts).

fetch(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"fetch">>, Base, Req, Opts).

verify(Base, Req, Opts) ->
    case hb_maps:get(<<"source-digest">>, Req, not_found, Opts) of
        not_found ->
            hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"verify">>, Base, Req, Opts);
        _ ->
            verify_commitment(Base, Req, Opts)
    end.

reconstruct(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"reconstruct">>, Base, Req, Opts).

media(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"media">>, Base, Req, Opts).

to_hint(_Base, Req, _Opts) ->
    {ok, Req#{ <<"bundle">> => true }}.

verify_commitment(Base, Req, Opts) ->
    safe(fun() ->
        CommittedKeys =
            hb_util:message_to_ordered_list(
                hb_maps:get(<<"committed">>, Req, #{}, Opts),
                Opts
            ),
        ExpectedDigest = hb_maps:get(<<"source-digest">>, Req, not_found, Opts),
        ActualDigest = source_digest(Base, CommittedKeys, Opts),
        DescriptorValid = require_descriptor_source(Base, Opts) =:= ok,
        NativeValid =
            case descriptor_hash(Base, Opts) of
                {ok, SDHash} -> native_id_valid(Req, SDHash, Opts);
                _ -> false
            end,
        {ok,
            ExpectedDigest =/= not_found
                andalso ExpectedDigest =:= ActualDigest
                andalso DescriptorValid
                andalso NativeValid
        }
    end).

safe(Fun) ->
    try Fun() of
        Res -> Res
    catch
        _:{error, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

remove_matching_commitments(Msg, Opts) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    Filtered =
        hb_maps:filter(
            fun(_ID, Commitment) ->
                hb_maps:get(<<"commitment-device">>, Commitment, not_found, Opts)
                    =/= ?DEVICE
            end,
            Commitments,
            Opts
        ),
    case map_size(Filtered) of
        0 -> hb_maps:without([<<"commitments">>], Msg, Opts);
        _ -> Msg#{ <<"commitments">> => Filtered }
    end.

committed_keys(Msg, Opts) ->
    Candidates = [
        <<"device">>,
        <<"content-type">>,
        <<"body">>,
        <<"sd-hash">>,
        <<"stream-hash">>,
        <<"stream-name">>,
        <<"key">>,
        <<"suggested-file-name">>,
        <<"blob-store-paths">>,
        <<"descriptor-store-path">>
    ],
    lists:sort(
        [
            Key
        ||
            Key0 <- Candidates,
            Key <- [hb_ao:normalize_key(Key0)],
            hb_maps:is_key(Key, Msg, Opts)
        ]
    ).

source_digest(Msg, Keys, Opts) ->
    hb_util:human_id(
        crypto:hash(
            sha256,
            term_to_binary([
                {Key, canonical(committed_value(Key, Msg, Opts), Opts)}
            ||
                Key <- Keys
            ])
        )
    ).

committed_value(<<"device">>, Msg, Opts) ->
    hb_maps:get(<<"device">>, Msg, ?DEVICE, Opts);
committed_value(Key, Msg, Opts) ->
    hb_maps:get(Key, Msg, not_found, Opts).

canonical(Map, Opts) when is_map(Map) ->
    [
        {Key, canonical(hb_maps:get(Key, Map, not_found, Opts), Opts)}
    ||
        Key <- lists:sort(hb_maps:keys(Map, Opts))
    ];
canonical(List, Opts) when is_list(List) ->
    [canonical(Value, Opts) || Value <- List];
canonical(Value, _Opts) when is_integer(Value) ->
    integer_to_binary(Value);
canonical(true, _Opts) ->
    <<"true">>;
canonical(false, _Opts) ->
    <<"false">>;
canonical(Value, _Opts) ->
    Value.

commitment_id(Commitment) ->
    case hb_lbry_commitment:native_id(Commitment, #{}) of
        {ok, _Hex, Bytes} ->
            hb_lbry_commitment:commitment_id(Bytes);
        _ ->
            hb_util:human_id(crypto:hash(sha256, term_to_binary({?DEVICE, Commitment})))
    end.

add_evidence(Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"sd-hash">>, hb_maps:get(<<"sd-hash">>, Msg, not_found, Opts)},
            {<<"stream-hash">>, hb_maps:get(<<"stream-hash">>, Msg, not_found, Opts)},
            {<<"descriptor-store-path">>, hb_maps:get(<<"descriptor-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    ).

put_optionals([], Msg) ->
    Msg;
put_optionals([{_Key, not_found} | Rest], Msg) ->
    put_optionals(Rest, Msg);
put_optionals([{Key, Value} | Rest], Msg) ->
    put_optionals(Rest, Msg#{ Key => Value }).

require_descriptor_source(Msg, Opts) ->
    case hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"decode">>, #{}, Msg, Opts) of
        {ok, Desc} ->
            case same_descriptor_id(Desc, Msg, Opts) of
                true -> ok;
                false -> {error, descriptor_identity_mismatch}
            end;
        Error ->
            Error
    end.

same_descriptor_id(Desc, Msg, Opts) ->
    same_field(<<"sd-hash">>, Desc, Msg, Opts)
        andalso same_field(<<"stream-hash">>, Desc, Msg, Opts).

same_field(Key, Desc, Msg, Opts) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> hb_maps:get(Key, Desc, not_found, Opts) =/= not_found;
        Value -> hb_maps:get(Key, Desc, not_found, Opts) =:= Value
    end.

descriptor_hash(Msg, Opts) ->
    case hb_maps:get(<<"sd-hash">>, Msg, not_found, Opts) of
        Hash when is_binary(Hash) -> {ok, hb_util:to_lower(Hash)};
        _ -> {error, sd_hash_not_found}
    end.

native_id_valid(Commitment, SDHash, Opts) ->
    case hb_lbry_commitment:native_id(Commitment, Opts) of
        {ok, SDHash, _Bytes} -> true;
        _ -> false
    end.

-ifdef(TEST).

descriptor_commitment_verifies_test() ->
    {Descriptor, SDHash} = descriptor_fixture(),
    {ok, Desc0} =
        decode(
            #{},
            #{ <<"body">> => Descriptor, <<"sd-hash">> => SDHash },
            #{}
        ),
    Desc = Desc0#{ <<"device">> => ?DEVICE },
    {ok, Committed} = commit(Desc, #{}, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"sd-hash">> => <<"bad">> },
            source_verify_req(Committed),
            #{}
        )
    ).

source_verify_req(Msg) ->
    #{
        <<"commitment-ids">> => lbry_descriptor_commitment_ids(Msg)
    }.

lbry_descriptor_commitment_ids(Msg) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, #{}),
    [
        ID
    ||
        {ID, Commitment} <- maps:to_list(Commitments),
        hb_maps:get(<<"commitment-device">>, Commitment, not_found, #{}) =:= ?DEVICE
    ].

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
    StreamHash =
        descriptor_stream_hash(
            StreamNameHex,
            KeyHex,
            SuggestedHex,
            [Blob, Terminator]
        ),
    JSON =
        hb_json:encode(#{
            <<"stream_type">> => <<"lbryfile">>,
            <<"stream_name">> => StreamNameHex,
            <<"key">> => KeyHex,
            <<"suggested_file_name">> => SuggestedHex,
            <<"stream_hash">> => StreamHash,
            <<"blobs">> => [Blob, Terminator]
        }),
    {JSON, hb_util:to_hex(crypto:hash(sha384, JSON))}.

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
