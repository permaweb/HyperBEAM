%%% @doc LBRY claim-output source commitment device.
%%%
%%% This device commits to a verified raw transaction output proof. The raw
%%% transaction parsing remains in `~odysee-claim-proof@1.0`; this device gives
%%% the resulting source object its LBRY-native commitment boundary.
-module(dev_lbry_claim_output).
-implements(<<"lbry-claim-output@1.0">>).
-export([info/1, commit/3, verify/3, to_hint/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"lbry-claim-output@1.0">>).

info(_Opts) ->
    #{ exports => [<<"commit">>, <<"verify">>, <<"to-hint">>] }.

to_hint(_Base, Req, _Opts) ->
    {ok, Req#{ <<"bundle">> => true }}.

commit(Base, _Req, Opts) ->
    safe(fun() ->
        Msg = remove_matching_commitments(Base, Opts),
        ok = require_claim_output_source(Msg, Opts),
        {ok, NativeFields} = claim_output_native_fields(Msg, Opts),
        CommittedKeys = committed_keys(Msg, Opts),
        Digest = source_digest(Msg, CommittedKeys, Opts),
        Commitment0 = maps:merge(NativeFields, #{
            <<"commitment-device">> => ?DEVICE,
            <<"type">> => <<"claim-output">>,
            <<"committed">> => hb_util:list_to_numbered_message(CommittedKeys),
            <<"source-digest">> => Digest
        }),
        Commitment = add_evidence(Msg, Commitment0, Opts),
        ID = commitment_id(Commitment),
        Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
        {ok, Msg#{ <<"commitments">> => Commitments#{ ID => Commitment } }}
    end).

verify(Base, Req, Opts) ->
    safe(fun() ->
        CommittedKeys =
            hb_util:message_to_ordered_list(
                hb_maps:get(<<"committed">>, Req, #{}, Opts),
                Opts
            ),
        ExpectedDigest = hb_maps:get(<<"source-digest">>, Req, not_found, Opts),
        ActualDigest = source_digest(Base, CommittedKeys, Opts),
        SourceValid = require_claim_output_source(Base, Opts) =:= ok,
        NativeValid =
            case claim_output_native_id(Base, Opts) of
                {ok, NativeID} -> native_id_valid(Req, NativeID, Opts);
                _ -> false
            end,
        {ok,
            ExpectedDigest =/= not_found
                andalso ExpectedDigest =:= ActualDigest
                andalso SourceValid
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
        <<"txid">>,
        <<"txid-internal">>,
        <<"nout">>,
        <<"amount">>,
        <<"script-hex">>,
        <<"address-script-hex">>,
        <<"claim-op">>,
        <<"claim-id">>,
        <<"claim-name">>,
        <<"claim-value-size">>,
        <<"claim-value-hash">>,
        <<"claim-value-hex">>,
        <<"claim-proof-store-path">>,
        <<"claim-script-valid">>,
        <<"valid">>,
        <<"txid-valid">>,
        <<"nout-valid">>,
        <<"claim-id-valid">>,
        <<"claim-name-valid">>,
        <<"claim-value-hash-valid">>,
        <<"proof-tier">>
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
            {<<"txid">>, hb_maps:get(<<"txid">>, Msg, not_found, Opts)},
            {<<"nout">>, hb_maps:get(<<"nout">>, Msg, not_found, Opts)},
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"claim-proof-store-path">>, hb_maps:get(<<"claim-proof-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    ).

put_optionals([], Msg) ->
    Msg;
put_optionals([{_Key, not_found} | Rest], Msg) ->
    put_optionals(Rest, Msg);
put_optionals([{Key, Value} | Rest], Msg) ->
    put_optionals(Rest, Msg#{ Key => commitment_value(Value) }).

commitment_value(Value) when is_integer(Value) ->
    integer_to_binary(Value);
commitment_value(Value) ->
    Value.

claim_output_native_fields(Msg, Opts) ->
    case claim_output_native_id(Msg, Opts) of
        {ok, NativeID} ->
            hb_lbry_commitment:native_id_fields(<<"outpoint">>, NativeID);
        Error ->
            Error
    end.

claim_output_native_id(Msg, Opts) ->
    case {hb_maps:get(<<"txid">>, Msg, not_found, Opts), nout(Msg, Opts)} of
        {TxID, {ok, NOut}} when is_binary(TxID) ->
            try
                {ok,
                    hb_util:to_hex(
                        hb_lbry_commitment:outpoint_bytes(hb_util:to_lower(TxID), NOut)
                    )}
            catch
                _:_ -> {error, invalid_outpoint}
            end;
        _ ->
            {error, invalid_outpoint}
    end.

nout(Msg, Opts) ->
    case hb_maps:get(<<"nout">>, Msg, not_found, Opts) of
        NOut when is_integer(NOut), NOut >= 0 -> {ok, NOut};
        NOut when is_binary(NOut) ->
            try
                Int = binary_to_integer(NOut),
                case Int >= 0 of
                    true -> {ok, Int};
                    false -> {error, invalid_nout}
                end
            catch
                _:_ -> {error, invalid_nout}
            end;
        _ ->
            {error, invalid_nout}
    end.

native_id_valid(Commitment, NativeID, Opts) ->
    case hb_lbry_commitment:native_id(Commitment, Opts) of
        {ok, NativeID, _Bytes} -> true;
        _ -> false
    end.

require_claim_output_source(Msg, Opts) ->
    case hb_ao:raw(<<"odysee-claim-proof@1.0">>, <<"verify">>, Msg, #{}, Opts) of
        {ok, Proof} -> require_same_valid_proof(Proof, Msg, Opts);
        Error -> Error
    end.

require_same_valid_proof(Proof, Msg, Opts) ->
    case truthy(hb_maps:get(<<"valid">>, Proof, false, Opts))
        andalso truthy(hb_maps:get(<<"claim-script-valid">>, Proof, false, Opts))
        andalso same_field(<<"txid">>, Proof, Msg, Opts)
        andalso same_field(<<"nout">>, Proof, Msg, Opts)
        andalso same_field(<<"claim-id">>, Proof, Msg, Opts)
    of
        true -> ok;
        false -> {error, invalid_claim_output}
    end.

same_field(Key, Proof, Msg, Opts) ->
    hb_maps:get(Key, Proof, not_found, Opts)
        =:= hb_maps:get(Key, Msg, not_found, Opts).

truthy(true) -> true;
truthy(<<"true">>) -> true;
truthy(<<"1">>) -> true;
truthy(1) -> true;
truthy(_Value) -> false.

-ifdef(TEST).

claim_output_commitment_verifies_test() ->
    {TxHex, TxID, ClaimID} = claim_tx_fixture(<<"example">>, <<"raw claim">>),
    {ok, Proof0} =
        hb_ao:raw(
            <<"odysee-claim-proof@1.0">>,
            <<"verify">>,
            #{},
            #{
                <<"tx-hex">> => TxHex,
                <<"txid">> => TxID,
                <<"nout">> => 0,
                <<"claim-id">> => ClaimID,
                <<"claim-name">> => <<"example">>
            },
            #{}
        ),
    Proof = Proof0#{ <<"device">> => ?DEVICE },
    {ok, Committed} = commit(Proof, #{}, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"claim-id">> => <<"bad">> },
            source_verify_req(Committed),
            #{}
        )
    ).

source_verify_req(Msg) ->
    #{
        <<"commitment-ids">> =>
            [
                ID
            ||
                {ID, Commitment} <- maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, #{})),
                hb_maps:get(<<"commitment-device">>, Commitment, not_found, #{}) =:= ?DEVICE
            ]
    }.

claim_tx_fixture(Name, Value) ->
    Script = claim_script(Name, Value),
    RawTx = tx_with_script(Script),
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

tx_with_script(Script) ->
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

claim_script(Name, Value) ->
    AddressScript = <<16#76, 16#a9, 20, 0:160, 16#88, 16#ac>>,
    <<
        16#b5,
        (push(Name))/binary,
        (push(Value))/binary,
        16#6d,
        16#75,
        AddressScript/binary
    >>.

push(Bin) when byte_size(Bin) < 16#4c ->
    <<(byte_size(Bin)), Bin/binary>>.

reverse_binary(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

-endif.
