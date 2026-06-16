%%% @doc LBRY transaction source commitment device.
%%%
%%% A transaction's native source identifier is its display-order txid, derived
%%% by double-SHA256 hashing the raw transaction bytes and reversing the digest.
-module(dev_lbry_transaction).
-implements(<<"lbry-transaction@1.0">>).
-export([info/1, commit/3, verify/3, to_hint/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"lbry-transaction@1.0">>).

info(_Opts) ->
    #{ exports => [<<"commit">>, <<"verify">>, <<"to-hint">>] }.

to_hint(_Base, Req, _Opts) ->
    {ok, Req#{ <<"bundle">> => true }}.

commit(Base, _Req, Opts) ->
    safe(fun() ->
        Msg0 = remove_matching_commitments(Base, Opts),
        {ok, Raw} = raw_tx(Msg0, Opts),
        {ok, Tx} = hb_lbry_tx:parse(Raw),
        TxID = hb_maps:get(<<"txid">>, Tx, Opts),
        ok = require_ok(require_txid_match(Msg0, TxID, Opts)),
        Msg = ensure_transaction_message(Msg0, Raw, TxID),
        CommittedKeys = committed_keys(Msg, Opts),
        Digest = source_digest(Msg, CommittedKeys, Opts),
        {ok, NativeFields} =
            hb_lbry_commitment:native_id_fields(<<"txid">>, TxID),
        Commitment0 = maps:merge(NativeFields, #{
            <<"commitment-device">> => ?DEVICE,
            <<"type">> => <<"transaction">>,
            <<"committed">> => hb_util:list_to_numbered_message(CommittedKeys),
            <<"hash-algorithm">> => <<"sha256d">>,
            <<"txid">> => TxID,
            <<"tx-size">> => integer_to_binary(byte_size(Raw)),
            <<"source-digest">> => Digest
        }),
        Commitment = put_optional(
            <<"tx-store-path">>,
            hb_maps:get(<<"tx-store-path">>, Msg, not_found, Opts),
            Commitment0
        ),
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
        {ok, Raw} = raw_tx(Base, Opts),
        TxID = hb_lbry_tx:txid(Raw),
        SizeValid = valid_tx_size(Base, Raw, Opts),
        NativeValid = native_id_valid(Req, TxID, Opts),
        {ok,
            ExpectedDigest =/= not_found
                andalso ExpectedDigest =:= ActualDigest
                andalso SizeValid
                andalso expected_txid(Req, Opts) =:= TxID
                andalso txid_field(Base, Opts) =:= TxID
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

ensure_transaction_message(Msg, Raw, TxID) ->
    Msg#{
        <<"device">> => ?DEVICE,
        <<"content-type">> => <<"application/vnd.lbry.transaction">>,
        <<"body">> => Raw,
        <<"txid">> => TxID,
        <<"tx-size">> => byte_size(Raw),
        <<"tx-store-path">> => <<"odysee/transaction/", TxID/binary>>
    }.

committed_keys(Msg, Opts) ->
    Candidates = [
        <<"device">>,
        <<"content-type">>,
        <<"body">>,
        <<"txid">>,
        <<"tx-size">>,
        <<"tx-store-path">>
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

raw_tx(Msg, Opts) ->
    case hb_maps:get(<<"body">>, Msg, not_found, Opts) of
        Raw when is_binary(Raw) ->
            case hb_maps:get(<<"encoding">>, Msg, not_found, Opts) of
                <<"hex">> -> decode_tx_hex(Raw);
                _ -> {ok, Raw}
            end;
        _ ->
            case first_value([<<"tx-hex">>, <<"tx_hex">>, <<"hex">>], Msg, Opts) of
                Hex when is_binary(Hex) -> decode_tx_hex(Hex);
                _ -> {error, raw_tx_not_found}
            end
    end.

decode_tx_hex(Hex) ->
    try {ok, binary:decode_hex(hb_util:to_lower(Hex))}
    catch _:_ -> {error, invalid_tx_hex}
    end.

require_txid_match(Msg, TxID, Opts) ->
    case txid_field(Msg, Opts) of
        not_found -> ok;
        TxID -> ok;
        Other -> {error, {txid_mismatch, Other, TxID}}
    end.

require_ok(ok) ->
    ok;
require_ok({error, Reason}) ->
    throw({error, Reason}).

txid_field(Msg, Opts) ->
    case hb_maps:get(<<"txid">>, Msg, not_found, Opts) of
        TxID when is_binary(TxID) -> normalize_hex(TxID);
        _ -> not_found
    end.

expected_txid(Commitment, Opts) ->
    case hb_maps:get(<<"txid">>, Commitment, not_found, Opts) of
        TxID when is_binary(TxID) -> normalize_hex(TxID);
        _ -> not_found
    end.

native_id_valid(Commitment, TxID, Opts) ->
    case hb_lbry_commitment:native_id(Commitment, Opts) of
        {ok, TxID, _Bytes} -> true;
        _ -> false
    end.

valid_tx_size(Msg, Raw, Opts) ->
    case hb_maps:get(<<"tx-size">>, Msg, byte_size(Raw), Opts) of
        Size when is_integer(Size) -> Size =:= byte_size(Raw);
        Size when is_binary(Size) -> valid_tx_size(Size, byte_size(Raw));
        _ -> false
    end.

valid_tx_size(Size, Expected) ->
    try binary_to_integer(Size) =:= Expected
    catch _:_ -> false
    end.

first_value([], _Msg, _Opts) ->
    not_found;
first_value([Key | Rest], Msg, Opts) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> first_value(Rest, Msg, Opts);
        Value -> Value
    end.

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:bin(string:lowercase(binary_to_list(Hex))).

-ifdef(TEST).

transaction_commitment_verifies_test() ->
    Raw = test_tx_raw(),
    TxID = hb_lbry_tx:txid(Raw),
    {ok, Committed} = commit(#{ <<"body">> => Raw, <<"txid">> => TxID }, #{}, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"body">> => <<Raw/binary, 0>> },
            source_verify_req(Committed),
            #{}
        )
    ).

transaction_rejects_txid_mismatch_test() ->
    Raw = test_tx_raw(),
    ?assertMatch(
        {error, {txid_mismatch, _, _}},
        commit(#{ <<"body">> => Raw, <<"txid">> => <<"00">> }, #{}, #{})
    ).

test_tx_raw() ->
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
    >>.

source_verify_req(Msg) ->
    #{
        <<"commitment-ids">> =>
            [
                ID
            ||
                {ID, Commitment} <- maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, #{})),
                hb_maps:get(<<"commitment-device">>, Commitment, not_found, #{})
                    =:= ?DEVICE
            ]
    }.

-endif.
