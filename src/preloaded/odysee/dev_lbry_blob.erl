%%% @doc LBRY blob source commitment device.
%%%
%%% A blob's native source identifier is the SHA-384 digest of its encrypted
%%% body. This device commits to the normalized HyperBEAM blob message and
%%% verifies that the body still matches the LBRY blob hash it claims.
-module(dev_lbry_blob).
-implements(<<"lbry-blob@1.0">>).
-export([info/1, commit/3, verify/3, to_hint/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"lbry-blob@1.0">>).

%% @doc Return the public device API.
info(_Opts) ->
    #{ exports => [<<"commit">>, <<"verify">>, <<"to-hint">>] }.

%% @doc Preserve blob source messages in bundle form when verifying.
to_hint(_Base, Req, _Opts) ->
    {ok, Req#{ <<"bundle">> => true }}.

%% @doc Add a LBRY blob source commitment to a normalized blob message.
commit(Base, _Req, Opts) ->
    safe(fun() ->
        Msg = remove_matching_commitments(Base, Opts),
        {ok, Body} = blob_body(Msg, Opts),
        {ok, BlobHash} = blob_hash(Msg, Opts),
        ok = require_blob_match(BlobHash, Body),
        CommittedKeys = committed_keys(Msg, Opts),
        Digest = source_digest(Msg, CommittedKeys, Opts),
        {ok, NativeFields} =
            hb_lbry_commitment:native_id_fields(<<"blob-hash">>, BlobHash),
        Commitment0 = maps:merge(NativeFields, #{
            <<"commitment-device">> => ?DEVICE,
            <<"type">> => <<"blob">>,
            <<"committed">> => hb_util:list_to_numbered_message(CommittedKeys),
            <<"hash-algorithm">> => <<"sha384">>,
            <<"blob-hash">> => BlobHash,
            <<"blob-size">> => integer_to_binary(byte_size(Body)),
            <<"source-digest">> => Digest
        }),
        Commitment = put_optional(
            <<"blob-store-path">>,
            hb_maps:get(<<"blob-store-path">>, Msg, not_found, Opts),
            Commitment0
        ),
        ID = commitment_id(Commitment),
        Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
        {ok, Msg#{ <<"commitments">> => Commitments#{ ID => Commitment } }}
    end).

%% @doc Verify a LBRY blob source commitment.
verify(Base, Req, Opts) ->
    safe(fun() ->
        CommittedKeys =
            hb_util:message_to_ordered_list(
                hb_maps:get(<<"committed">>, Req, #{}, Opts),
                Opts
            ),
        ExpectedDigest = hb_maps:get(<<"source-digest">>, Req, not_found, Opts),
        ActualDigest = source_digest(Base, CommittedKeys, Opts),
        {ok, Body} = blob_body(Base, Opts),
        {ok, BlobHash} = blob_hash(Base, Opts),
        SizeValid = valid_blob_size(Base, Body, Opts),
        BlobMatch = require_blob_match(BlobHash, Body),
        NativeValid = native_id_valid(Req, BlobHash, Opts),
        {ok,
            ExpectedDigest =/= not_found
                andalso ExpectedDigest =:= ActualDigest
                andalso SizeValid
                andalso BlobMatch =:= ok
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
        <<"blob-hash">>,
        <<"blob-size">>,
        <<"blob-store-path">>
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

put_optional(_Key, not_found, Msg) ->
    Msg;
put_optional(Key, Value, Msg) ->
    Msg#{ Key => Value }.

blob_body(Msg, Opts) ->
    case hb_maps:get(<<"body">>, Msg, not_found, Opts) of
        Body when is_binary(Body) -> {ok, Body};
        _ -> {error, body_not_found}
    end.

blob_hash(Msg, Opts) ->
    case hb_maps:get(<<"blob-hash">>, Msg, not_found, Opts) of
        Hash when is_binary(Hash) -> {ok, normalize_hex(Hash)};
        _ -> {error, blob_hash_not_found}
    end.

valid_blob_size(Msg, Body, Opts) ->
    case hb_maps:get(<<"blob-size">>, Msg, byte_size(Body), Opts) of
        Size when is_integer(Size) -> Size =:= byte_size(Body);
        Size when is_binary(Size) -> valid_blob_size(Size, byte_size(Body));
        _ -> false
    end.

valid_blob_size(Size, Expected) ->
    try binary_to_integer(Size) =:= Expected
    catch _:_ -> false
    end.

require_blob_match(BlobHash, Body) ->
    case sha384_hex(Body) of
        BlobHash -> ok;
        Other -> {error, {blob_hash_mismatch, BlobHash, Other}}
    end.

native_id_valid(Commitment, BlobHash, Opts) ->
    case hb_lbry_commitment:native_id(Commitment, Opts) of
        {ok, BlobHash, _Bytes} -> true;
        _ -> false
    end.

sha384_hex(Bin) ->
    hb_util:to_hex(crypto:hash(sha384, Bin)).

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:bin(string:lowercase(binary_to_list(Hex))).

-ifdef(TEST).

blob_commitment_verifies_test() ->
    {Msg, Body, _Hash} = blob_fixture(),
    {ok, Committed} = commit(Msg, #{}, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"body">> => <<Body/binary, "!">> },
            source_verify_req(Committed),
            #{}
        )
    ).

blob_size_is_committed_test() ->
    {Msg, _Body, _Hash} = blob_fixture(),
    {ok, Committed} = commit(Msg, #{}, #{}),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"blob-size">> => 1 },
            source_verify_req(Committed),
            #{}
        )
    ).

source_verify_req(Msg) ->
    #{
        <<"commitment-ids">> => lbry_blob_commitment_ids(Msg)
    }.

lbry_blob_commitment_ids(Msg) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, #{}),
    [
        ID
    ||
        {ID, Commitment} <- maps:to_list(Commitments),
        hb_maps:get(<<"commitment-device">>, Commitment, not_found, #{}) =:= ?DEVICE
    ].

blob_fixture() ->
    Body = <<"encrypted blob">>,
    BlobHash = sha384_hex(Body),
    {
        #{
            <<"device">> => ?DEVICE,
            <<"content-type">> => <<"application/octet-stream">>,
            <<"body">> => Body,
            <<"blob-hash">> => BlobHash,
            <<"blob-store-path">> => <<"odysee/blob/", BlobHash/binary>>,
            <<"blob-size">> => byte_size(Body)
        },
        Body,
        BlobHash
    }.

-endif.
