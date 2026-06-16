%%% @doc Odysee source commitment device.
%%%
%%% This device is the first native-commitment layer for the Odysee bridge. It
%%% does not replace the specialized Odysee compatibility devices; it commits to
%%% the normalized public messages they produce and verifies the source-specific
%%% invariants that are available for each message type.
-module(dev_odysee).
-implements(<<"odysee@1.0">>).
-export([info/1, index/3, source/3, commit/3, verify/3, to_hint/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"odysee@1.0">>).
-define(LBRY_BLOB_COMMITMENT_DEVICE, <<"lbry-blob@1.0">>).
-define(LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE, <<"lbry-stream-descriptor@1.0">>).
-define(LBRY_CLAIM_OUTPUT_COMMITMENT_DEVICE, <<"lbry-claim-output@1.0">>).
-define(LBRY_TRANSACTION_COMMITMENT_DEVICE, <<"lbry-transaction@1.0">>).

%% @doc Return the public device API.
info(_Opts) ->
    #{ exports => [<<"index">>, <<"source">>, <<"commit">>, <<"verify">>, <<"to-hint">>] }.

index(_Base, _Req, _Opts) ->
    {ok, #{
        <<"device">> => ?DEVICE,
        <<"paths">> => #{
            <<"source">> => [<<"id">>, <<"native-id">>, <<"kind">>],
            <<"commit">> => [<<"type">>],
            <<"verify">> => [<<"commitment-ids">>]
        },
        <<"source-kinds">> =>
            [
                <<"blob">>,
                <<"stream-descriptor">>,
                <<"transaction">>,
                <<"claim-output">>,
                <<"claim">>
            ]
    }}.

%% @doc Preserve nested source messages in bundle form when verifying.
to_hint(_Base, Req, _Opts) ->
    {ok, Req#{ <<"bundle">> => true }}.

%% @doc Read a committed public Odysee/LBRY source object by native identifier.
source(Base, Req, Opts) ->
    safe(fun() ->
        {ok, _Kind, Path} = native_source_path(Base, Req, Opts),
        case hb_ao:raw(
            <<"cache@1.0">>,
            <<"read">>,
            Base,
            #{ <<"read">> => Path },
            Opts
        ) of
            {ok, Msg} -> {ok, Msg};
            Error -> Error
        end
    end).

%% @doc Add an Odysee source commitment to a normalized Odysee message.
commit(Base, Req, Opts) ->
    safe(fun() ->
        Type = commitment_type(Base, Req, Opts),
        Msg = remove_matching_commitments(Base, Type, Opts),
        CommittedKeys = committed_keys(Type, Msg, Opts),
        Digest = source_digest(Msg, CommittedKeys, Opts),
        Commitment0 = #{
            <<"commitment-device">> => ?DEVICE,
            <<"type">> => Type,
            <<"committed">> => hb_util:list_to_numbered_message(CommittedKeys),
            <<"source-digest">> => Digest,
            <<"verification-tier">> => verification_tier(Type, Msg, Opts),
            <<"verification-limitations">> => verification_limitations(Type, Msg, Opts)
        },
        Commitment = add_evidence(Type, Msg, Commitment0, Opts),
        ID = commitment_id(Type, Commitment),
        Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
        {ok, Msg#{ <<"commitments">> => Commitments#{ ID => Commitment } }}
    end).

%% @doc Verify an Odysee source commitment.
verify(Base, Req, Opts) ->
    safe(fun() ->
        Type = hb_maps:get(<<"type">>, Req, source, Opts),
        CommittedKeys =
            hb_util:message_to_ordered_list(
                hb_maps:get(<<"committed">>, Req, #{}, Opts),
                Opts
            ),
        ExpectedDigest = hb_maps:get(<<"source-digest">>, Req, not_found, Opts),
        ActualDigest = source_digest(Base, CommittedKeys, Opts),
        DigestValid =
            ExpectedDigest =/= not_found
                andalso ExpectedDigest =:= ActualDigest,
        {ok, DigestValid andalso verify_type(Type, Base, Req, Opts)}
    end).

safe(Fun) ->
    try Fun() of
        Res -> Res
    catch
        _:{error, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

native_source_path(Base, Req, Opts) ->
    case first_message_value(
        [<<"id">>, <<"native-id">>, <<"native_id">>, <<"source-id">>, <<"source_id">>, <<"read">>],
        [Req, Base],
        Opts
    ) of
        ID when is_binary(ID) ->
            classify_native_source_id(ID, source_kind(Base, Req, Opts));
        _ ->
            {error, source_id_not_found}
    end.

source_kind(Base, Req, Opts) ->
    case first_message_value([<<"kind">>, <<"source-kind">>, <<"type">>], [Req, Base], Opts) of
        Kind when is_binary(Kind) -> hb_ao:normalize_key(Kind);
        _ -> not_found
    end.

first_message_value(_Keys, [], _Opts) ->
    not_found;
first_message_value(Keys, [Msg | Rest], Opts) when is_map(Msg) ->
    case first_value(Keys, Msg, Opts) of
        not_found -> first_message_value(Keys, Rest, Opts);
        Value -> Value
    end;
first_message_value(Keys, [_ | Rest], Opts) ->
    first_message_value(Keys, Rest, Opts).

classify_native_source_id(ID0, Kind) ->
    ID = normalize_source_id(ID0),
    case ID of
        <<"odysee/", _/binary>> ->
            {ok, <<"path">>, ID};
        <<"lbry/blob/", Hash/binary>> ->
            blob_source_path(Hash);
        <<"lbry/blob-id/", Hash/binary>> ->
            blob_source_path(Hash);
        <<"lbry/descriptor/", Hash/binary>> ->
            descriptor_source_path(Hash);
        <<"lbry/descriptor-id/", Hash/binary>> ->
            descriptor_source_path(Hash);
        <<"lbry/stream-descriptor/", Hash/binary>> ->
            descriptor_source_path(Hash);
        <<"lbry/transaction/", TxID/binary>> ->
            transaction_source_path(TxID);
        <<"lbry/tx/", TxID/binary>> ->
            transaction_source_path(TxID);
        <<"lbry/claim-output/", Rest/binary>> ->
            claim_output_source_path(Rest);
        <<"lbry/claim-proof/", Rest/binary>> ->
            claim_output_source_path(Rest);
        _ ->
            classify_bare_native_source_id(ID, Kind)
    end.

classify_bare_native_source_id(ID, <<"blob">>) ->
    blob_source_path(ID);
classify_bare_native_source_id(ID, <<"stream-descriptor">>) ->
    descriptor_source_path(ID);
classify_bare_native_source_id(ID, <<"descriptor">>) ->
    descriptor_source_path(ID);
classify_bare_native_source_id(ID, <<"transaction">>) ->
    transaction_source_path(ID);
classify_bare_native_source_id(ID, <<"tx">>) ->
    transaction_source_path(ID);
classify_bare_native_source_id(ID, Kind)
        when Kind =:= <<"claim-output">>; Kind =:= <<"claim-proof">>; Kind =:= <<"outpoint">> ->
    claim_output_source_path(ID);
classify_bare_native_source_id(ID, _Kind) ->
    case {byte_size(ID), binary:match(ID, <<":">>)} of
        {96, nomatch} -> blob_source_path(ID);
        {64, nomatch} -> transaction_source_path(ID);
        {40, nomatch} -> claim_source_path(ID);
        {_Size, _Colon} -> claim_output_source_path(ID)
    end.

blob_source_path(Hash0) ->
    Hash = normalize_hex(Hash0),
    case valid_hex(Hash, 96) of
        true -> {ok, <<"blob">>, <<"odysee/blob/", Hash/binary>>};
        false -> {error, invalid_blob_hash}
    end.

descriptor_source_path(Hash0) ->
    Hash = normalize_hex(Hash0),
    case valid_hex(Hash, 96) of
        true -> {ok, <<"stream-descriptor">>, <<"odysee/descriptor/", Hash/binary>>};
        false -> {error, invalid_descriptor_hash}
    end.

transaction_source_path(TxID0) ->
    TxID = normalize_hex(TxID0),
    case valid_hex(TxID, 64) of
        true -> {ok, <<"transaction">>, <<"odysee/transaction/", TxID/binary>>};
        false -> {error, invalid_txid}
    end.

claim_source_path(ClaimID0) ->
    ClaimID = normalize_hex(ClaimID0),
    case valid_hex(ClaimID, 40) of
        true -> {ok, <<"claim">>, <<"odysee/claim-id/", ClaimID/binary>>};
        false -> {error, invalid_claim_id}
    end.

claim_output_source_path(Rest0) ->
    Rest = normalize_source_id(Rest0),
    Parts =
        case binary:split(Rest, <<"/">>) of
            [PathTxID, PathNOut] -> [PathTxID, PathNOut];
            _ -> binary:split(Rest, <<":">>)
        end,
    case Parts of
        [TxID0, NOut0] ->
            TxID = normalize_hex(TxID0),
            case {valid_hex(TxID, 64), non_negative_integer(NOut0)} of
                {true, {ok, NOut}} ->
                    {ok,
                        <<"claim-output">>,
                        <<"odysee/claim-proof/", TxID/binary, "/", (integer_to_binary(NOut))/binary>>};
                {false, _} ->
                    {error, invalid_txid};
                {_, Error} ->
                    Error
            end;
        _ ->
            {error, invalid_claim_output_id}
    end.

valid_hex(Hex, Size) when is_binary(Hex), byte_size(Hex) =:= Size ->
    try byte_size(binary:decode_hex(Hex)) =:= Size div 2
    catch _:_ -> false
    end;
valid_hex(_Hex, _Size) ->
    false.

non_negative_integer(Bin) when is_binary(Bin) ->
    try
        Int = binary_to_integer(Bin),
        case Int >= 0 of
            true -> {ok, Int};
            false -> {error, invalid_nout}
        end
    catch _:_ ->
        {error, invalid_nout}
    end;
non_negative_integer(Int) when is_integer(Int), Int >= 0 ->
    {ok, Int};
non_negative_integer(_Value) ->
    {error, invalid_nout}.

normalize_source_id(<<"/", Rest/binary>>) ->
    normalize_source_id(Rest);
normalize_source_id(ID) when is_binary(ID) ->
    ID.

commitment_type(Base, Req, Opts) ->
    case hb_maps:get(<<"type">>, Req, not_found, Opts) of
        not_found -> infer_type(Base, Opts);
        Type -> Type
    end.

infer_type(Base, Opts) ->
    case hb_maps:get(<<"device">>, Base, not_found, Opts) of
        <<"odysee-claim@1.0">> -> <<"claim">>;
        <<"odysee-stream-descriptor@1.0">> -> <<"stream-descriptor">>;
        <<"lbry-stream-descriptor@1.0">> -> <<"stream-descriptor">>;
        <<"odysee-channel@1.0">> -> <<"channel">>;
        <<"odysee-comment@1.0">> -> <<"comment">>;
        <<"odysee-reaction@1.0">> -> <<"comment-reaction">>;
        <<"odysee-file@1.0">> -> <<"file-view-count">>;
        <<"odysee-file-reaction@1.0">> -> <<"file-reaction">>;
        <<"odysee-subscription@1.0">> -> <<"subscription-count">>;
        <<"odysee-blob@1.0">> -> <<"blob">>;
        <<"lbry-blob@1.0">> -> <<"blob">>;
        <<"odysee-claim-proof@1.0">> -> <<"claim-proof">>;
        <<"lbry-claim-output@1.0">> -> <<"claim-proof">>;
        <<"odysee-stream@1.0">> ->
            case hb_maps:get(<<"view">>, Base, not_found, Opts) of
                <<"verified-stream">> -> <<"stream-attestation">>;
                _ -> <<"stream">>
            end;
        _ -> <<"source">>
    end.

remove_matching_commitments(Msg, Type, Opts) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    Filtered =
        hb_maps:filter(
            fun(_ID, Commitment) ->
                not (
                    hb_maps:get(<<"commitment-device">>, Commitment, not_found, Opts) =:= ?DEVICE
                        andalso hb_maps:get(<<"type">>, Commitment, not_found, Opts) =:= Type
                )
            end,
            Commitments,
            Opts
        ),
    case map_size(Filtered) of
        0 -> hb_maps:without([<<"commitments">>], Msg, Opts);
        _ -> Msg#{ <<"commitments">> => Filtered }
    end.

committed_keys(Type, Msg, Opts) ->
    Candidates =
        case Type of
            <<"claim">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"claim-id">>,
                    <<"claim-name">>,
                    <<"canonical-url">>,
                    <<"value-type">>,
                    <<"claim-store-path">>,
                    <<"claim-proof-store-path">>,
                    <<"txid">>,
                    <<"nout">>,
                    <<"height">>,
                    <<"claim-op">>
                ];
            <<"stream-descriptor">> ->
                [
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
                ];
            <<"channel">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"claim-id">>,
                    <<"claim-name">>,
                    <<"channel-id">>,
                    <<"channel-name">>,
                    <<"identity-type">>,
                    <<"value-type">>,
                    <<"canonical-url">>,
                    <<"permanent-url">>,
                    <<"short-url">>,
                    <<"public-key">>,
                    <<"public-key-id">>,
                    <<"signature-valid">>,
                    <<"committer-format">>,
                    <<"ao-committer">>,
                    <<"claim-store-path">>,
                    <<"channel-store-path">>,
                    <<"claim-proof-store-path">>,
                    <<"txid">>,
                    <<"nout">>,
                    <<"height">>,
                    <<"claim-op">>
                ];
            <<"claim-proof">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"txid">>,
                    <<"nout">>,
                    <<"claim-id">>,
                    <<"claim-name">>,
                    <<"claim-op">>,
                    <<"claim-value-size">>,
                    <<"claim-value-hash">>,
                    <<"claim-proof-store-path">>,
                    <<"claim-script-valid">>,
                    <<"txid-valid">>,
                    <<"nout-valid">>,
                    <<"claim-id-valid">>,
                    <<"claim-name-valid">>,
                    <<"claim-value-hash-valid">>,
                    <<"valid">>,
                    <<"proof-tier">>
                ];
            <<"comment">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"comment-id">>,
                    <<"claim-id">>,
                    <<"channel-id">>,
                    <<"channel-name">>,
                    <<"comment">>,
                    <<"ancestors">>,
                    <<"comment-store-path">>,
                    <<"claim-store-path">>,
                    <<"channel-store-path">>,
                    <<"signature">>,
                    <<"signing-ts">>,
                    <<"public-key">>
                ];
            <<"comment-reaction">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"comment-id">>,
                    <<"comment-ids">>,
                    <<"my_reactions">>,
                    <<"others_reactions">>,
                    <<"comment-reaction-store-path">>
                ];
            <<"stream-attestation">> ->
                [
                    <<"device">>,
                    <<"view">>,
                    <<"claim-id">>,
                    <<"claim-name">>,
                    <<"target">>,
                    <<"sd-hash">>,
                    <<"signed-sd-hash">>,
                    <<"signature-verification">>,
                    <<"channel-verification">>,
                    <<"descriptor-verification">>
                ];
            <<"stream">> ->
                [
                    <<"device">>,
                    <<"claim-id">>,
                    <<"claim-name">>,
                    <<"sd-hash">>,
                    <<"media-type">>,
                    <<"source-hash">>,
                    <<"source-size">>,
                    <<"stream-store-path">>,
                    <<"claim-store-path">>,
                    <<"descriptor-store-path">>,
                    <<"channel-store-path">>,
                    <<"claim-proof-store-path">>,
                    <<"txid">>,
                    <<"nout">>,
                    <<"claim-height">>,
                    <<"claim-op">>,
                    <<"streaming-url">>,
                    <<"download-url">>
                ];
            <<"blob">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"blob-hash">>,
                    <<"blob-size">>,
                    <<"blob-store-path">>
                ];
            <<"file-view-count">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"claim-id">>,
                    <<"claim-ids">>,
                    <<"view-counts">>,
                    <<"by-claim-id">>,
                    <<"file-view-count-store-path">>
                ];
            <<"file-reaction">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"claim-id">>,
                    <<"claim-ids">>,
                    <<"my_reactions">>,
                    <<"others_reactions">>,
                    <<"file-reaction-store-path">>
                ];
            <<"subscription-count">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"claim-id">>,
                    <<"claim-ids">>,
                    <<"sub-counts">>,
                    <<"by-claim-id">>,
                    <<"subscription-count-store-path">>
                ];
            _ ->
                hb_maps:keys(Msg, Opts)
        end,
    lists:sort(
        [
            Key
        ||
            Key0 <- Candidates,
            Key <- [hb_ao:normalize_key(Key0)],
            not lists:member(Key, [<<"commitments">>, <<"priv">>]),
            hb_maps:is_key(Key, Msg, Opts)
        ]
    ).

source_digest(Msg, Keys, Opts) ->
    hb_util:human_id(
        crypto:hash(
            sha256,
            term_to_binary([
                {Key, canonical(hb_maps:get(Key, Msg, not_found, Opts), Opts)}
            ||
                Key <- Keys
            ])
        )
    ).

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

commitment_id(Type, Commitment) ->
    hb_util:human_id(crypto:hash(sha256, term_to_binary({?DEVICE, Type, Commitment}))).

verification_tier(<<"stream-descriptor">>, _Msg, _Opts) -> 2;
verification_tier(<<"blob">>, _Msg, _Opts) -> 2;
verification_tier(<<"comment">>, _Msg, _Opts) -> 2;
verification_tier(<<"claim-proof">>, _Msg, _Opts) -> 2;
verification_tier(<<"stream-attestation">>, _Msg, _Opts) -> 1;
verification_tier(_Type, _Msg, _Opts) -> 1.

verification_limitations(<<"claim">>, _Msg, _Opts) ->
    [<<"raw LBRY transaction proof is not included in this commitment">>];
verification_limitations(<<"channel">>, _Msg, _Opts) ->
    [<<"raw LBRY transaction proof is not included in this commitment">>];
verification_limitations(<<"stream">>, _Msg, _Opts) ->
    [<<"stream claim signature validity must be verified by a stream attestation commitment">>];
verification_limitations(<<"stream-attestation">>, _Msg, _Opts) ->
    [<<"stream claim signature validity is currently SDK/resolve-attested">>];
verification_limitations(<<"claim-proof">>, _Msg, _Opts) ->
    [<<"block inclusion proof is not included in this transaction-output proof">>];
verification_limitations(_Type, _Msg, _Opts) ->
    [].

add_evidence(<<"claim">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"claim-name">>, hb_maps:get(<<"claim-name">>, Msg, not_found, Opts)},
            {<<"value-type">>, hb_maps:get(<<"value-type">>, Msg, not_found, Opts)},
            {<<"claim-store-path">>, hb_maps:get(<<"claim-store-path">>, Msg, not_found, Opts)},
            {<<"claim-proof-store-path">>, hb_maps:get(<<"claim-proof-store-path">>, Msg, not_found, Opts)},
            {<<"txid">>, hb_maps:get(<<"txid">>, Msg, not_found, Opts)},
            {<<"nout">>, hb_maps:get(<<"nout">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"stream-descriptor">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"sd-hash">>, hb_maps:get(<<"sd-hash">>, Msg, not_found, Opts)},
            {<<"stream-hash">>, hb_maps:get(<<"stream-hash">>, Msg, not_found, Opts)},
            {<<"descriptor-store-path">>, hb_maps:get(<<"descriptor-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"channel">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"channel-id">>, hb_maps:get(<<"channel-id">>, Msg, not_found, Opts)},
            {<<"channel-name">>, hb_maps:get(<<"channel-name">>, Msg, not_found, Opts)},
            {<<"claim-store-path">>, hb_maps:get(<<"claim-store-path">>, Msg, not_found, Opts)},
            {<<"channel-store-path">>, hb_maps:get(<<"channel-store-path">>, Msg, not_found, Opts)},
            {<<"claim-proof-store-path">>, hb_maps:get(<<"claim-proof-store-path">>, Msg, not_found, Opts)},
            {<<"txid">>, hb_maps:get(<<"txid">>, Msg, not_found, Opts)},
            {<<"nout">>, hb_maps:get(<<"nout">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"claim-proof">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"txid">>, hb_maps:get(<<"txid">>, Msg, not_found, Opts)},
            {<<"nout">>, hb_maps:get(<<"nout">>, Msg, not_found, Opts)},
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"claim-proof-store-path">>, hb_maps:get(<<"claim-proof-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"stream">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"sd-hash">>, hb_maps:get(<<"sd-hash">>, Msg, not_found, Opts)},
            {<<"stream-store-path">>, hb_maps:get(<<"stream-store-path">>, Msg, not_found, Opts)},
            {<<"claim-store-path">>, hb_maps:get(<<"claim-store-path">>, Msg, not_found, Opts)},
            {<<"descriptor-store-path">>, hb_maps:get(<<"descriptor-store-path">>, Msg, not_found, Opts)},
            {<<"channel-store-path">>, hb_maps:get(<<"channel-store-path">>, Msg, not_found, Opts)},
            {<<"claim-proof-store-path">>, hb_maps:get(<<"claim-proof-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"comment">>, Msg, Commitment, Opts) ->
    Comment = comment_source(Msg, Opts),
    put_optionals(
        [
            {<<"comment-id">>, first_value([<<"comment-id">>], Comment, Opts)},
            {<<"channel-id">>, first_value([<<"channel-id">>], Comment, Opts)},
            {<<"comment-store-path">>, first_value([<<"comment-store-path">>], Comment, Opts)},
            {<<"claim-store-path">>, first_value([<<"claim-store-path">>], Comment, Opts)},
            {<<"channel-store-path">>, first_value([<<"channel-store-path">>], Comment, Opts)}
        ],
        Commitment
    );
add_evidence(<<"comment-reaction">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"comment-id">>, hb_maps:get(<<"comment-id">>, Msg, not_found, Opts)},
            {<<"comment-reaction-store-path">>,
                hb_maps:get(<<"comment-reaction-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"blob">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"blob-hash">>, hb_maps:get(<<"blob-hash">>, Msg, not_found, Opts)},
            {<<"blob-store-path">>, hb_maps:get(<<"blob-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"file-view-count">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"file-view-count-store-path">>,
                hb_maps:get(<<"file-view-count-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"file-reaction">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"file-reaction-store-path">>,
                hb_maps:get(<<"file-reaction-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"subscription-count">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"subscription-count-store-path">>,
                hb_maps:get(<<"subscription-count-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(_Type, _Msg, Commitment, _Opts) ->
    Commitment.

put_optionals([], Msg) ->
    Msg;
put_optionals([{_Key, not_found} | Rest], Msg) ->
    put_optionals(Rest, Msg);
put_optionals([{Key, Value} | Rest], Msg) ->
    put_optionals(Rest, Msg#{ Key => Value }).

verify_type(<<"claim">>, Base, _Req, Opts) ->
    ClaimID = hb_maps:get(<<"claim-id">>, Base, not_found, Opts),
    BodyValid = case decode_body(Base, Opts) of
        {ok, Source} ->
            ClaimID =/= not_found
                andalso contains_claim_id(Source, ClaimID, Opts);
        _ ->
            ClaimID =/= not_found
    end,
    BodyValid;
verify_type(<<"stream-descriptor">>, Base, _Req, Opts) ->
    case hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"decode">>, #{}, Base, Opts) of
        {ok, Desc} ->
            hb_maps:get(<<"sd-hash">>, Desc, not_found, Opts)
                =:= hb_maps:get(<<"sd-hash">>, Base, not_found, Opts);
        _ ->
            false
    end;
verify_type(<<"channel">>, Base, _Req, Opts) ->
    ChannelID = hb_maps:get(<<"channel-id">>, Base, not_found, Opts),
    case decode_body(Base, Opts) of
        {ok, Claim} ->
            ChannelID =/= not_found
                andalso ChannelID =:= first_value([<<"claim_id">>, <<"claim-id">>], Claim, Opts)
                andalso first_value([<<"value_type">>, <<"value-type">>], Claim, Opts) =:= <<"channel">>
                andalso hb_maps:get(<<"public-key">>, Base, not_found, Opts) =/= not_found;
        _ ->
            false
    end;
verify_type(<<"claim-proof">>, Base, _Req, Opts) ->
    case hb_ao:raw(<<"odysee-claim-proof@1.0">>, <<"verify">>, Base, #{}, Opts) of
        {ok, #{ <<"valid">> := true }} -> true;
        _ -> false
    end;
verify_type(<<"stream-attestation">>, Base, _Req, Opts) ->
    hb_maps:get(<<"valid">>, Base, false, Opts) =:= true;
verify_type(<<"stream">>, Base, _Req, Opts) ->
    hb_maps:get(<<"claim-id">>, Base, not_found, Opts) =/= not_found
        andalso hb_maps:get(<<"sd-hash">>, Base, not_found, Opts) =/= not_found;
verify_type(<<"comment">>, Base, _Req, Opts) ->
    Comment = comment_source(Base, Opts),
    case hb_maps:get(<<"public-key">>, Comment, not_found, Opts) of
        not_found ->
            false;
        PublicKey ->
            case hb_ao:raw(
                <<"odysee-comment@1.0">>,
                <<"verify-signature">>,
                Comment,
                #{ <<"public-key">> => PublicKey },
                Opts
            ) of
                {ok, #{ <<"is-valid">> := true }} -> true;
                _ -> false
            end
    end;
verify_type(<<"comment-reaction">>, Base, _Req, Opts) ->
    summary_store_path_valid(
        Base,
        <<"comment-id">>,
        <<"comment-ids">>,
        <<"comment-reaction-store-path">>,
        <<"odysee/comment-reaction/">>,
        Opts
    )
        andalso normalized_surface_matches(
            <<"odysee-reaction@1.0">>,
            Base,
            [<<"comment-ids">>, <<"my_reactions">>, <<"others_reactions">>],
            Opts
        );
verify_type(<<"blob">>, Base, _Req, Opts) ->
    case {hb_maps:get(<<"blob-hash">>, Base, not_found, Opts), hb_maps:get(<<"body">>, Base, not_found, Opts)} of
        {Hash, Body} when is_binary(Hash), is_binary(Body) ->
            normalize_hex(Hash) =:= sha384_hex(Body);
        _ ->
            false
    end;
verify_type(<<"file-view-count">>, Base, _Req, Opts) ->
    summary_store_path_valid(
        Base,
        <<"claim-id">>,
        <<"claim-ids">>,
        <<"file-view-count-store-path">>,
        <<"odysee/file-view-count/">>,
        Opts
    )
        andalso normalized_surface_matches(
            <<"odysee-file@1.0">>,
            Base,
            [<<"claim-ids">>, <<"view-counts">>, <<"by-claim-id">>],
            Opts
        );
verify_type(<<"file-reaction">>, Base, _Req, Opts) ->
    summary_store_path_valid(
        Base,
        <<"claim-id">>,
        <<"claim-ids">>,
        <<"file-reaction-store-path">>,
        <<"odysee/file-reaction/">>,
        Opts
    )
        andalso normalized_surface_matches(
            <<"odysee-file-reaction@1.0">>,
            Base,
            [<<"claim-ids">>, <<"my_reactions">>, <<"others_reactions">>],
            Opts
        );
verify_type(<<"subscription-count">>, Base, _Req, Opts) ->
    summary_store_path_valid(
        Base,
        <<"claim-id">>,
        <<"claim-ids">>,
        <<"subscription-count-store-path">>,
        <<"odysee/subscription-count/">>,
        Opts
    )
        andalso normalized_surface_matches(
            <<"odysee-subscription@1.0">>,
            Base,
            [<<"claim-ids">>, <<"sub-counts">>, <<"by-claim-id">>],
            Opts
        );
verify_type(_Type, _Base, _Req, _Opts) ->
    true.

comment_source(Base, Opts) ->
    case hb_maps:get(<<"comment">>, Base, not_found, Opts) of
        Comment when is_map(Comment) -> Comment;
        _ -> Base
    end.

first_value([], _Map, _Opts) ->
    not_found;
first_value([Key | Rest], Map, Opts) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> first_value(Rest, Map, Opts);
        Value -> Value
    end.

decode_body(Msg, Opts) ->
    case hb_maps:get(<<"body">>, Msg, not_found, Opts) of
        Body when is_binary(Body) ->
            try {ok, hb_json:decode(Body)}
            catch _:_ -> {error, invalid_json}
            end;
        _ ->
            {error, body_not_found}
    end.

normalized_surface_matches(Device, Base, Keys, Opts) ->
    case hb_ao:raw(Device, <<"normalize">>, Base, #{}, Opts) of
        {ok, Normalized} ->
            lists:all(
                fun(Key) ->
                    hb_maps:get(Key, Base, not_found, Opts)
                        =:= hb_maps:get(Key, Normalized, not_found, Opts)
                end,
                Keys
            );
        _ ->
            false
    end.

summary_store_path_valid(Base, IDKey, IDsKey, PathKey, Prefix, Opts) ->
    case {
        hb_maps:get(IDKey, Base, not_found, Opts),
        hb_maps:get(IDsKey, Base, not_found, Opts),
        hb_maps:get(PathKey, Base, not_found, Opts)
    } of
        {ID, IDs, Path} when is_binary(ID), is_list(IDs), is_binary(Path) ->
            lists:member(ID, IDs) andalso Path =:= <<Prefix/binary, ID/binary>>;
        _ ->
            false
    end.

sha384_hex(Bin) ->
    hb_util:to_hex(crypto:hash(sha384, Bin)).

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:bin(string:lowercase(binary_to_list(Hex))).

contains_claim_id(Source, ClaimID, Opts) when is_map(Source) ->
    first_value([<<"claim_id">>, <<"claim-id">>], Source, Opts) =:= ClaimID
        orelse lists:any(
            fun(Value) -> contains_claim_id(Value, ClaimID, Opts) end,
            maps:values(Source)
        );
contains_claim_id(Source, ClaimID, Opts) when is_list(Source) ->
    lists:any(fun(Value) -> contains_claim_id(Value, ClaimID, Opts) end, Source);
contains_claim_id(_Source, _ClaimID, _Opts) ->
    false.

-ifdef(TEST).

claim_commitment_verifies_test() ->
    Claim = #{
        <<"claim_id">> => <<"abc123">>,
        <<"name">> => <<"example">>,
        <<"value">> => #{ <<"title">> => <<"Example">> }
    },
    Msg = #{
        <<"device">> => <<"odysee-claim@1.0">>,
        <<"claim-id">> => <<"abc123">>,
        <<"claim-name">> => <<"example">>,
        <<"claim">> => Claim,
        <<"value">> => hb_maps:get(<<"value">>, Claim, #{})
    },
    {ok, Committed} = commit(Msg, #{ <<"type">> => <<"claim">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    Tampered = Committed#{ <<"claim-id">> => <<"bad">> },
    ?assertEqual(false, hb_message:verify(Tampered, source_verify_req(Tampered), #{})).

descriptor_commitment_verifies_test() ->
    {Descriptor, SDHash} = descriptor_fixture(),
    {ok, Desc} =
        hb_ao:raw(
            <<"odysee-stream-descriptor@1.0">>,
            <<"decode">>,
            #{},
            #{ <<"body">> => Descriptor, <<"sd-hash">> => SDHash },
            #{}
    ),
    {ok, Committed} = commit(Desc, #{ <<"type">> => <<"stream-descriptor">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"sd-hash">> => <<"bad">> },
            source_verify_req(Committed),
            #{}
        )
    ).

channel_commitment_verifies_test() ->
    Channel = channel_fixture(),
    {ok, Committed} = commit(Channel, #{ <<"type">> => <<"channel">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"channel-id">> => <<"bad">> },
            source_verify_req(Committed),
            #{}
        )
    ).

comment_commitment_verifies_test() ->
    Comment = comment_fixture(),
    {ok, Committed} = commit(Comment, #{ <<"type">> => <<"comment">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    Nested = hb_maps:get(<<"comment">>, Committed, #{}),
    Tampered = Committed#{ <<"comment">> => Nested#{ <<"comment">> => <<"tampered">> } },
    ?assertEqual(false, hb_message:verify(Tampered, source_verify_req(Committed), #{})).

comment_reaction_commitment_verifies_test() ->
    Reaction = comment_reaction_fixture(),
    {ok, Committed} = commit(Reaction, #{ <<"type">> => <<"comment-reaction">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"comment-id">> => <<"wrong">> },
            source_verify_req(Committed),
            #{}
        )
    ).

blob_commitment_verifies_test() ->
    {Blob, Body, _Hash} = blob_fixture(),
    {ok, Committed} = commit(Blob, #{ <<"type">> => <<"blob">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"body">> => <<Body/binary, "!">> },
            source_verify_req(Committed),
            #{}
        )
    ).

file_view_count_commitment_verifies_test() ->
    Counts = file_view_count_fixture(),
    {ok, Committed} = commit(Counts, #{ <<"type">> => <<"file-view-count">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"view-counts">> => [0] },
            source_verify_req(Committed),
            #{}
        )
    ).

file_reaction_commitment_verifies_test() ->
    Reaction = file_reaction_fixture(),
    {ok, Committed} = commit(Reaction, #{ <<"type">> => <<"file-reaction">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"claim-id">> => <<"wrong">> },
            source_verify_req(Committed),
            #{}
        )
    ).

subscription_count_commitment_verifies_test() ->
    Counts = subscription_count_fixture(),
    {ok, Committed} = commit(Counts, #{ <<"type">> => <<"subscription-count">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"claim-id">> => <<"wrong">> },
            source_verify_req(Committed),
            #{}
        )
    ).

store_fixture_read_attaches_source_commitment_test() ->
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/claim/test">> => claim_fixture()
        }
    },
    {ok, Msg} = hb_store:read(Store, <<"odysee/claim/test">>, #{}),
    ?assert(hb_maps:is_key(<<"commitments">>, Msg, #{})),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})).

store_fixture_read_keeps_verifiable_commitment_test() ->
    {ok, Committed} = commit(claim_fixture(), #{ <<"type">> => <<"claim">> }, #{}),
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/claim/test">> => Committed
        }
    },
    {ok, Msg} = hb_store:read(Store, <<"/odysee/claim/test">>, #{}),
    ?assertEqual(false, hb_maps:is_key(<<"claim">>, Msg, #{})),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})).

store_fixture_read_commits_channel_comment_blob_and_descriptor_test() ->
    {Blob, _Body, BlobHash} = blob_fixture(),
    {Descriptor, SDHash} = descriptor_fixture(),
    {ok, Desc} =
        hb_ao:raw(
            <<"odysee-stream-descriptor@1.0">>,
            <<"decode">>,
            #{},
            #{ <<"body">> => Descriptor, <<"sd-hash">> => SDHash },
            #{}
        ),
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/channel/channel-1">> => channel_fixture(),
            <<"odysee/comment/vector-1">> => comment_fixture(),
            <<"odysee/blob/", BlobHash/binary>> => Blob,
            <<"odysee/descriptor/", SDHash/binary>> => Desc
        }
    },
    {ok, Channel} = hb_store:read(Store, <<"odysee/channel/channel-1">>, #{}),
    ?assert(hb_message:verify(Channel, source_verify_req(Channel), #{})),
    {ok, Comment} = hb_store:read(Store, <<"odysee/comment/vector-1">>, #{}),
    ?assert(hb_message:verify(Comment, source_verify_req(Comment), #{})),
    {ok, BlobMsg} = hb_store:read(Store, <<"odysee/blob/", BlobHash/binary>>, #{}),
    ?assert(has_commitment_device(BlobMsg, ?LBRY_BLOB_COMMITMENT_DEVICE)),
    ?assertEqual(?LBRY_BLOB_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, BlobMsg, #{})),
    ?assert(hb_message:verify(BlobMsg, source_verify_req(BlobMsg), #{})),
    {ok, DescMsg} = hb_store:read(Store, <<"odysee/descriptor/", SDHash/binary>>, #{}),
    ?assert(has_commitment_device(DescMsg, ?LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE)),
    ?assertEqual(
        ?LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE,
        hb_maps:get(<<"device">>, DescMsg, #{})
    ),
    ?assertEqual(SDHash, hb_maps:get(<<"sd-hash">>, DescMsg, #{})),
    ?assertEqual(
        <<"odysee/blob/", BlobHash/binary>>,
        hb_maps:get(<<"1">>, hb_maps:get(<<"blob-store-paths">>, DescMsg, #{}), #{})
    ),
    ?assert(hb_message:verify(DescMsg, source_verify_req(DescMsg), #{})).

store_fixture_read_commits_surface_summaries_test() ->
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/comment-reaction/vector-1">> => comment_reaction_fixture(),
            <<"odysee/file-view-count/claim-1">> => file_view_count_fixture(),
            <<"odysee/file-reaction/claim-1">> => file_reaction_fixture(),
            <<"odysee/subscription-count/channel-1">> => subscription_count_fixture()
        }
    },
    {ok, CommentReaction} = hb_store:read(Store, <<"odysee/comment-reaction/vector-1">>, #{}),
    ?assertEqual(<<"vector-1">>, hb_maps:get(<<"comment-id">>, CommentReaction, #{})),
    ?assert(hb_message:verify(CommentReaction, source_verify_req(CommentReaction), #{})),
    {ok, ViewCounts} = hb_store:read(Store, <<"odysee/file-view-count/claim-1">>, #{}),
    ?assertEqual(<<"claim-1">>, hb_maps:get(<<"claim-id">>, ViewCounts, #{})),
    ?assert(hb_message:verify(ViewCounts, source_verify_req(ViewCounts), #{})),
    {ok, FileReaction} = hb_store:read(Store, <<"odysee/file-reaction/claim-1">>, #{}),
    ?assertEqual(<<"claim-1">>, hb_maps:get(<<"claim-id">>, FileReaction, #{})),
    ?assert(hb_message:verify(FileReaction, source_verify_req(FileReaction), #{})),
    {ok, SubscriptionCounts} =
        hb_store:read(Store, <<"odysee/subscription-count/channel-1">>, #{}),
    ?assertEqual(<<"channel-1">>, hb_maps:get(<<"claim-id">>, SubscriptionCounts, #{})),
    ?assert(hb_message:verify(SubscriptionCounts, source_verify_req(SubscriptionCounts), #{})).

store_fixture_read_commits_transaction_test() ->
    {Raw, TxID} = transaction_fixture(),
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/transaction/", TxID/binary>> => #{
                <<"device">> => <<"lbry-transaction@1.0">>,
                <<"content-type">> => <<"application/vnd.lbry.transaction">>,
                <<"body">> => Raw,
                <<"txid">> => TxID,
                <<"tx-size">> => byte_size(Raw)
            }
        }
    },
    {ok, Msg} = hb_store:read(Store, <<"odysee/transaction/", TxID/binary>>, #{}),
    ?assert(has_commitment_device(Msg, ?LBRY_TRANSACTION_COMMITMENT_DEVICE)),
    ?assertEqual(?LBRY_TRANSACTION_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, Msg, #{})),
    ?assertEqual(TxID, hb_maps:get(<<"txid">>, Msg, #{})),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})).

source_reads_native_blob_and_transaction_ids_test() ->
    {Blob, _Body, BlobHash} = blob_fixture(),
    {Raw, TxID} = transaction_fixture(),
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/blob/", BlobHash/binary>> => Blob,
            <<"odysee/transaction/", TxID/binary>> => #{
                <<"device">> => <<"lbry-transaction@1.0">>,
                <<"content-type">> => <<"application/vnd.lbry.transaction">>,
                <<"body">> => Raw,
                <<"txid">> => TxID,
                <<"tx-size">> => byte_size(Raw)
            }
        }
    },
    {ok, BlobMsg} = source(#{}, #{ <<"id">> => BlobHash }, #{ <<"store">> => Store }),
    ?assertEqual(?LBRY_BLOB_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, BlobMsg, #{})),
    ?assert(hb_message:verify(BlobMsg, source_verify_req(BlobMsg), #{})),
    {ok, TxMsg} = source(#{}, #{ <<"id">> => TxID }, #{ <<"store">> => Store }),
    ?assertEqual(?LBRY_TRANSACTION_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, TxMsg, #{})),
    ?assertEqual(TxID, hb_maps:get(<<"txid">>, TxMsg, #{})),
    ?assert(hb_message:verify(TxMsg, source_verify_req(TxMsg), #{})).

store_live_blob_read_fetches_and_commits_hash_test() ->
    Body = <<"encrypted blob">>,
    BlobHash = sha384_hex(Body),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/blob", blob, {200, Body}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"blob-url-template">> => <<MockServer/binary, "/blob?hash={hash}">>
        },
        {ok, Msg} = hb_store:read(Store, <<"odysee/blob-id/", BlobHash/binary>>, #{}),
        ?assertEqual(BlobHash, hb_maps:get(<<"blob-hash">>, Msg, #{})),
        ?assert(has_commitment_device(Msg, ?LBRY_BLOB_COMMITMENT_DEVICE)),
        ?assertEqual(?LBRY_BLOB_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, Msg, #{})),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        [_Request] = hb_mock_server:get_requests(blob, 1, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

store_live_claim_proof_read_fetches_and_commits_test() ->
    {TxHex, TxID, ClaimID} = proof_tx_fixture(<<"example">>, <<"raw claim">>),
    Raw = hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{
            <<"txid">> => TxID,
            <<"hex">> => TxHex,
            <<"outputs">> => []
        },
        <<"id">> => 1
    }),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/", transaction_show, {200, Raw}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"lbry-proxy-url">> => MockServer
        },
        Path = <<"odysee/claim-proof/", TxID/binary, "/0">>,
        {ok, Msg} = hb_store:read(Store, Path, #{}),
        ?assertEqual(ClaimID, hb_maps:get(<<"claim-id">>, Msg, #{})),
        ?assertEqual(true, hb_maps:get(<<"valid">>, Msg, #{})),
        ?assert(has_commitment_device(Msg, ?LBRY_CLAIM_OUTPUT_COMMITMENT_DEVICE)),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        LbryStore = Store#{ <<"store-module">> => hb_store_lbry_claim_output },
        {ok, LbryMsg} = hb_store:read(LbryStore, <<TxID/binary, ":0">>, #{}),
        ?assertEqual(ClaimID, hb_maps:get(<<"claim-id">>, LbryMsg, #{})),
        ?assert(has_commitment_device(LbryMsg, ?LBRY_CLAIM_OUTPUT_COMMITMENT_DEVICE)),
        ?assert(hb_message:verify(LbryMsg, source_verify_req(LbryMsg), #{})),
        [_Request1, _Request2] = hb_mock_server:get_requests(transaction_show, 2, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

store_live_claim_id_read_searches_and_commits_test() ->
    Claim = claim_source_fixture(),
    Raw = search_response([Claim]),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/", claim_search, {200, Raw}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"lbry-proxy-url">> => MockServer
        },
        {ok, Msg} = hb_store:read(Store, <<"odysee/claim-id/abc123">>, #{}),
        ?assertEqual(<<"abc123">>, hb_maps:get(<<"claim-id">>, Msg, #{})),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        [_Request] = hb_mock_server:get_requests(claim_search, 1, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

store_live_stream_id_read_searches_and_commits_test() ->
    Claim = stream_source_fixture(),
    Raw = search_response([Claim]),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/", stream_search, {200, Raw}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"lbry-proxy-url">> => MockServer
        },
        {ok, Msg} = hb_store:read(Store, <<"odysee/stream-id/stream-1">>, #{}),
        ?assertEqual(<<"stream-1">>, hb_maps:get(<<"claim-id">>, Msg, #{})),
        ?assertEqual(<<"odysee/claim-id/stream-1">>, hb_maps:get(<<"claim-store-path">>, Msg, #{})),
        ?assertEqual(<<"odysee/descriptor/", (hb_maps:get(<<"sd-hash">>, Msg, #{}))/binary>>, hb_maps:get(<<"descriptor-store-path">>, Msg, #{})),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        [_Request] = hb_mock_server:get_requests(stream_search, 1, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

store_live_channel_read_searches_and_commits_test() ->
    Claim = channel_source_fixture(),
    Raw = search_response([Claim]),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/", channel_search, {200, Raw}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"lbry-proxy-url">> => MockServer
        },
        {ok, Msg} = hb_store:read(Store, <<"odysee/channel-id/channel-1">>, #{}),
        ?assertEqual(<<"channel-1">>, hb_maps:get(<<"channel-id">>, Msg, #{})),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        [_Request] = hb_mock_server:get_requests(channel_search, 1, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

store_live_comment_read_fetches_and_commits_test() ->
    Comment = comment_source_fixture(),
    Raw = hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => Comment,
        <<"id">> => 1
    }),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/", comment_by_id, {200, Raw}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"odysee-comment-url">> => MockServer
        },
        {ok, Msg} = hb_store:read(Store, <<"odysee/comment-id/vector-1">>, #{}),
        ?assertEqual(<<"vector-1">>, hb_maps:get(<<"comment-id">>, Msg, #{})),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        [_Request] = hb_mock_server:get_requests(comment_by_id, 1, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

remote_store_read_verifies_and_caches_source_commitment_test() ->
    Key = <<"odysee/claim/test">>,
    SourceStore = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            Key => claim_fixture()
        }
    },
    SourceNode = hb_http_server:start_node(#{ <<"store">> => SourceStore }),
    ClientStore = hb_test_utils:test_store(),
    RemoteStore = [
        #{
            <<"store-module">> => hb_store_remote_node,
            <<"node">> => SourceNode,
            <<"require-codec">> => <<"json@1.0">>,
            <<"local-store">> => [ClientStore]
        }
    ],
    {ok, Msg} = hb_cache:read(Key, #{ <<"store">> => RemoteStore }),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
    {ok, Cached} = hb_cache:read(Key, #{ <<"store">> => [ClientStore] }),
    ?assert(hb_message:verify(Cached, source_verify_req(Cached), #{})).

remote_store_read_verifies_and_caches_surface_summary_commitment_test() ->
    Key = <<"odysee/file-view-count/claim-1">>,
    SourceStore = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            Key => file_view_count_fixture()
        }
    },
    SourceNode = hb_http_server:start_node(#{ <<"store">> => SourceStore }),
    ClientStore = hb_test_utils:test_store(),
    RemoteStore = [
        #{
            <<"store-module">> => hb_store_remote_node,
            <<"node">> => SourceNode,
            <<"require-codec">> => <<"json@1.0">>,
            <<"verify-remote-read">> => true,
            <<"local-store">> => [ClientStore]
        }
    ],
    {ok, Msg} = hb_cache:read(Key, #{ <<"store">> => RemoteStore }),
    ?assertEqual(<<"claim-1">>, hb_maps:get(<<"claim-id">>, Msg, #{})),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
    {ok, Cached} = hb_cache:read(Key, #{ <<"store">> => [ClientStore] }),
    ?assert(hb_message:verify(Cached, source_verify_req(Cached), #{})).

remote_store_verifies_native_lbry_blob_and_caches_commitment_id_test() ->
    Body = <<"encrypted blob">>,
    BlobHash = sha384_hex(Body),
    Key = <<"lbry/blob/", BlobHash/binary>>,
    SourceStore = #{
        <<"store-module">> => hb_store_lbry_blob,
        <<"fixtures">> => #{
            <<"odysee/blob/", BlobHash/binary>> => #{
                <<"device">> => <<"lbry-blob@1.0">>,
                <<"content-type">> => <<"application/octet-stream">>,
                <<"body">> => Body,
                <<"blob-hash">> => BlobHash,
                <<"blob-size">> => byte_size(Body)
            }
        }
    },
    SourceNode = hb_http_server:start_node(#{ <<"store">> => SourceStore }),
    ClientStore = hb_test_utils:test_store(),
    RemoteStore = [
        #{
            <<"store-module">> => hb_store_remote_node,
            <<"node">> => SourceNode,
            <<"require-codec">> => <<"json@1.0">>,
            <<"verify-remote-read">> => true,
            <<"local-store">> => [ClientStore]
        }
    ],
    {ok, Msg} = hb_cache:read(Key, #{ <<"store">> => RemoteStore }),
    ?assert(has_commitment_device(Msg, ?LBRY_BLOB_COMMITMENT_DEVICE)),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
    [CommitmentID] = commitment_ids_by_device(Msg, ?LBRY_BLOB_COMMITMENT_DEVICE),
    {ok, CachedByKey} = hb_cache:read(Key, #{ <<"store">> => [ClientStore] }),
    ?assert(hb_message:verify(CachedByKey, source_verify_req(CachedByKey), #{})),
    {ok, CachedByCommitmentID} =
        hb_cache:read(CommitmentID, #{ <<"store">> => [ClientStore] }),
    ?assert(hb_message:verify(
        CachedByCommitmentID,
        source_verify_req(CachedByCommitmentID),
        #{}
    )).

remote_store_rejects_substituted_native_blob_test() ->
    Body = <<"the real blob">>,
    BlobHash = sha384_hex(Body),
    RequestedHash = sha384_hex(<<"a different blob">>),
    Key = <<"lbry/blob/", RequestedHash/binary>>,
    SourceStore = #{
        <<"store-module">> => hb_store_lbry_blob,
        <<"fixtures">> => #{
            <<"odysee/blob/", RequestedHash/binary>> => #{
                <<"device">> => <<"lbry-blob@1.0">>,
                <<"content-type">> => <<"application/octet-stream">>,
                <<"body">> => Body,
                <<"blob-hash">> => BlobHash,
                <<"blob-size">> => byte_size(Body)
            }
        }
    },
    SourceNode = hb_http_server:start_node(#{ <<"store">> => SourceStore }),
    ClientStore = hb_test_utils:test_store(),
    RemoteStore = [
        #{
            <<"store-module">> => hb_store_remote_node,
            <<"node">> => SourceNode,
            <<"require-codec">> => <<"json@1.0">>,
            <<"verify-remote-read">> => true,
            <<"local-store">> => [ClientStore]
        }
    ],
    ?assertMatch({error, _}, hb_cache:read(Key, #{ <<"store">> => RemoteStore })),
    ?assertEqual({error, not_found}, hb_cache:read(Key, #{ <<"store">> => [ClientStore] })).

source_verify_req(Msg) ->
    #{
        <<"commitment-ids">> => odysee_commitment_ids(Msg)
    }.

odysee_commitment_ids(Msg) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, #{}),
    [
        ID
    ||
        {ID, Commitment} <- maps:to_list(Commitments),
        lists:member(
            hb_maps:get(<<"commitment-device">>, Commitment, not_found, #{}),
            [
                ?DEVICE,
                ?LBRY_BLOB_COMMITMENT_DEVICE,
                ?LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE,
                ?LBRY_CLAIM_OUTPUT_COMMITMENT_DEVICE,
                ?LBRY_TRANSACTION_COMMITMENT_DEVICE
            ]
        )
    ].

has_commitment_device(Msg, Device) ->
    lists:member(Device, hb_message:commitment_devices(Msg, #{})).

commitment_ids_by_device(Msg, Device) ->
    [
        ID
    ||
        {ID, Commitment} <- maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, #{})),
        hb_maps:get(<<"commitment-device">>, Commitment, not_found, #{}) =:= Device
    ].

claim_fixture() ->
    Claim = claim_source_fixture(),
    #{
        <<"device">> => <<"odysee-claim@1.0">>,
        <<"content-type">> => <<"application/json">>,
        <<"body">> => hb_json:encode(Claim),
        <<"claim">> => Claim,
        <<"claim-id">> => <<"abc123">>,
        <<"claim-name">> => <<"example">>,
        <<"value">> => hb_maps:get(<<"value">>, Claim, #{})
    }.

claim_source_fixture() ->
    #{
        <<"claim_id">> => <<"abc123">>,
        <<"name">> => <<"example">>,
        <<"value">> => #{ <<"title">> => <<"Example">> }
    }.

stream_source_fixture() ->
    #{
        <<"claim_id">> => <<"stream-1">>,
        <<"canonical_url">> => <<"lbry://example#1">>,
        <<"name">> => <<"example">>,
        <<"value_type">> => <<"stream">>,
        <<"value">> => #{
            <<"source">> => #{
                <<"media_type">> => <<"video/mp4">>,
                <<"name">> => <<"example.mp4">>,
                <<"sd_hash">> =>
                    <<"6ee8f762a2eedbd2b5eeade82ca4d0a6287f55db4195563cc52fc004701b7d55edcfad277a5141084bdf5fca3adb403a">>,
                <<"size">> => 42
            },
            <<"stream_type">> => <<"video">>
        }
    }.

channel_fixture() ->
    Claim = channel_source_fixture(),
    hb_util:ok(
        hb_ao:raw(
            <<"odysee-channel@1.0">>,
            <<"channel">>,
            #{},
            #{ <<"claim">> => Claim },
            #{}
        )
    ).

channel_source_fixture() ->
    #{
        <<"claim_id">> => <<"channel-1">>,
        <<"canonical_url">> => <<"lbry://@example#1">>,
        <<"name">> => <<"@example">>,
        <<"value_type">> => <<"channel">>,
        <<"value">> => #{
            <<"title">> => <<"Example">>,
            <<"public_key">> => <<"3082010a0282010100">>,
            <<"public_key_id">> => <<"bLGr4w">>
        }
    }.

comment_fixture() ->
    Comment = comment_source_fixture(),
    hb_util:ok(
        hb_ao:raw(
            <<"odysee-comment@1.0">>,
            <<"normalize">>,
            #{},
            #{ <<"comment">> => Comment },
            #{}
        )
    ).

comment_reaction_fixture() ->
    Msg = hb_util:ok(
        hb_ao:raw(
            <<"odysee-reaction@1.0">>,
            <<"normalize">>,
            #{},
            #{ <<"result">> => comment_reaction_result() },
            #{}
        )
    ),
    Msg#{
        <<"comment-id">> => <<"vector-1">>,
        <<"comment-reaction-store-path">> => <<"odysee/comment-reaction/vector-1">>
    }.

comment_source_fixture() ->
    (commentron_vector())#{
        <<"comment_id">> => <<"vector-1">>,
        <<"comment">> => <<"nicee">>
    }.

comment_reaction_result() ->
    #{
        <<"my_reactions">> => #{
            <<"vector-1">> => [<<"like">>]
        },
        <<"others_reactions">> => #{
            <<"vector-1">> => #{ <<"like">> => 53 }
        }
    }.

file_view_count_fixture() ->
    Msg = hb_util:ok(
        hb_ao:raw(
            <<"odysee-file@1.0">>,
            <<"normalize">>,
            #{ <<"claim-ids">> => <<"claim-1">> },
            #{ <<"counts">> => [1504] },
            #{}
        )
    ),
    Msg#{
        <<"claim-id">> => <<"claim-1">>,
        <<"file-view-count-store-path">> => <<"odysee/file-view-count/claim-1">>
    }.

file_reaction_fixture() ->
    Msg = hb_util:ok(
        hb_ao:raw(
            <<"odysee-file-reaction@1.0">>,
            <<"normalize">>,
            #{},
            #{ <<"result">> => file_reaction_result() },
            #{}
        )
    ),
    Msg#{
        <<"claim-id">> => <<"claim-1">>,
        <<"file-reaction-store-path">> => <<"odysee/file-reaction/claim-1">>
    }.

file_reaction_result() ->
    #{
        <<"my_reactions">> => #{
            <<"claim-1">> => [<<"like">>]
        },
        <<"others_reactions">> => #{
            <<"claim-1">> => #{ <<"like">> => 59 }
        }
    }.

subscription_count_fixture() ->
    Msg = hb_util:ok(
        hb_ao:raw(
            <<"odysee-subscription@1.0">>,
            <<"normalize">>,
            #{ <<"claim-ids">> => <<"channel-1">> },
            #{ <<"counts">> => [169000] },
            #{}
        )
    ),
    Msg#{
        <<"claim-id">> => <<"channel-1">>,
        <<"subscription-count-store-path">> => <<"odysee/subscription-count/channel-1">>
    }.

search_response(Claims) ->
    hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{
            <<"items">> => Claims,
            <<"page">> => 1,
            <<"page_size">> => length(Claims),
            <<"total_items">> => length(Claims),
            <<"total_pages">> => 1
        },
        <<"id">> => 1
    }).

blob_fixture() ->
    Body = <<"encrypted blob">>,
    BlobHash = sha384_hex(Body),
    {
        #{
            <<"device">> => <<"odysee-blob@1.0">>,
            <<"content-type">> => <<"application/octet-stream">>,
            <<"body">> => Body,
            <<"blob-hash">> => BlobHash,
            <<"blob-size">> => byte_size(Body)
        },
        Body,
        BlobHash
    }.

transaction_fixture() ->
    Raw =
        <<
            1:32/little,
            1,
            0:256,
            16#ffffffff:32/little,
            0,
            16#ffffffff:32/little,
            1,
            0:64/little,
            0,
            0:32/little
        >>,
    {Raw, hb_lbry_tx:txid(Raw)}.

proof_tx_fixture(Name, Value) ->
    Script = proof_claim_script(Name, Value),
    RawTx = proof_tx_with_script(Script),
    TxHash = crypto:hash(sha256, crypto:hash(sha256, RawTx)),
    TxID = hb_util:to_hex(reverse_binary(TxHash)),
    ClaimID =
        hb_util:to_hex(
            reverse_binary(
                crypto:hash(
                    ripemd160,
                    crypto:hash(sha256, <<TxHash/binary, 0:32/big>>)
                )
            )
        ),
    {hb_util:to_hex(RawTx), TxID, ClaimID}.

proof_tx_with_script(Script) ->
    ScriptSize = byte_size(Script),
    <<
        1:32/little,
        1,
        0:256,
        16#ffffffff:32/little,
        0,
        16#ffffffff:32/little,
        1,
        1000:64/little,
        ScriptSize,
        Script/binary,
        0:32/little
    >>.

proof_claim_script(Name, Value) ->
    AddressScript = <<16#76, 16#a9, 20, 0:160, 16#88, 16#ac>>,
    <<
        16#b5,
        (proof_push(Name))/binary,
        (proof_push(Value))/binary,
        16#6d,
        16#75,
        AddressScript/binary
    >>.

proof_push(Bin) when byte_size(Bin) < 16#4c ->
    <<(byte_size(Bin)), Bin/binary>>.

reverse_binary(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

commentron_vector() ->
    #{
        <<"channel-id">> => <<"7fadfe1d0dce928350137a13497b6fc36627cf45">>,
        <<"channel_id">> => <<"7fadfe1d0dce928350137a13497b6fc36627cf45">>,
        <<"public-key">> =>
            <<"3056301006072a8648ce3d020106052b8104000a03420004e0743cfa62857d1d7bda9ca6ba0ec3325902866e6442f51a9da2b143bc0ba40cda532e483e1a8a48c84b4b9dc16a117b2f9763d518db50d8fed2b818937ef8b1">>,
        <<"signature">> =>
            <<"fe35046bd949fc89037d64ac3558fea859022a166558b459b6883acafa15ca9ec567ca23e7b4ae19e4dbc3f92aac30a132315db7abcb03c15c61662fb9f49458">>,
        <<"signing-ts">> => <<"1582846386">>,
        <<"signing_ts">> => <<"1582846386">>,
        <<"data">> => <<"nicee">>
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
    {JSON, hb_util:to_hex(crypto:hash(sha384, JSON))}.

descriptor_stream_hash(StreamNameHex, KeyHex, SuggestedHex, Blobs) ->
    BlobSums =
        iolist_to_binary([
            descriptor_blob_hashsum(
                hb_ao:normalize_keys(Blob, #{})
            )
        ||
            Blob <- Blobs
        ]),
    BlobDigest = crypto:hash(sha384, BlobSums),
    hb_util:to_hex(
        crypto:hash(sha384, <<StreamNameHex/binary, KeyHex/binary, SuggestedHex/binary, BlobDigest/binary>>)
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
        <<HashPrefix/binary, (integer_to_binary(BlobNum))/binary, IV/binary, (integer_to_binary(Length))/binary>>
    ).

-endif.
