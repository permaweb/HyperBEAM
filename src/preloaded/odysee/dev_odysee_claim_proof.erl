%%% @doc Raw LBRY claim transaction proof device.
%%%
%%% This device verifies the part of a claim proof that can be checked from raw
%%% transaction bytes alone: txid, output index, LBRY claim script shape, claim
%%% name, and claim ID derivation. Block inclusion can be layered on top by
%%% adding a block header and merkle branch proof to this message family later.
-module(dev_odysee_claim_proof).
-implements(<<"odysee-claim-proof@1.0">>).
-export([info/1, decode/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"odysee-claim-proof@1.0">>).
-define(OP_PUSHDATA1, 16#4c).
-define(OP_PUSHDATA2, 16#4d).
-define(OP_PUSHDATA4, 16#4e).
-define(OP_2DROP, 16#6d).
-define(OP_DROP, 16#75).
-define(OP_CLAIM_NAME, 16#b5).
-define(OP_SUPPORT_CLAIM, 16#b6).
-define(OP_UPDATE_CLAIM, 16#b7).

%% @doc Return the public device API.
info(_Opts) ->
    #{ exports => [<<"decode">>, <<"verify">>] }.

%% @doc Decode raw transaction evidence into a claim proof message.
decode(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, TxHex} ?= tx_hex(Base, Req, Opts),
            {ok, RawTx} ?= decode_hex(<<"tx-hex">>, TxHex),
            {ok, Tx} ?= parse_tx(RawTx),
            {ok, NOut} ?= nout(Base, Req, Opts),
            {ok, Output} ?= output_at(NOut, hb_maps:get(<<"outputs">>, Tx, Opts)),
            {ok, ClaimScript} ?= decode_claim_script(hb_maps:get(<<"script">>, Output, Opts)),
            {ok, proof_message(TxHex, Tx, NOut, Output, ClaimScript)}
        else
            Error -> Error
        end
    end).

%% @doc Verify raw transaction evidence against optional expected claim fields.
verify(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, Proof0} ?= ensure_proof(Base, Req, Opts),
            Expected = expected_fields(Base, Req, Opts),
            Proof = add_validity(Proof0, Expected, Opts),
            {ok, Proof}
        else
            Error -> Error
        end
    end).

safe(Fun) ->
    try Fun() of
        Res -> Res
    catch
        _:{error, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

ensure_proof(Base, _Req, Opts)
        when is_map(Base),
             is_map_key(<<"claim-script-valid">>, Base),
             is_map_key(<<"txid">>, Base),
             is_map_key(<<"claim-id">>, Base) ->
    {ok, hb_cache:ensure_all_loaded(Base, Opts)};
ensure_proof(Base, Req, Opts) ->
    decode(Base, Req, Opts).

proof_message(TxHex, Tx, NOut, Output, ClaimScript) ->
    TxID = maps:get(<<"txid">>, Tx),
    TxHash = maps:get(<<"tx-hash">>, Tx),
    ClaimID = claim_id(ClaimScript, TxHash, NOut),
    ClaimValue = maps:get(<<"claim-value">>, ClaimScript, <<>>),
    Msg0 = #{
        <<"device">> => ?DEVICE,
        <<"content-type">> => <<"application/json">>,
        <<"body">> => normalize_hex(TxHex),
        <<"txid">> => TxID,
        <<"txid-internal">> => hb_util:to_hex(TxHash),
        <<"nout">> => NOut,
        <<"amount">> => maps:get(<<"amount">>, Output),
        <<"script-hex">> => hb_util:to_hex(maps:get(<<"script">>, Output)),
        <<"address-script-hex">> => hb_util:to_hex(maps:get(<<"address-script">>, ClaimScript)),
        <<"claim-op">> => maps:get(<<"claim-op">>, ClaimScript),
        <<"claim-id">> => ClaimID,
        <<"claim-name">> => maps:get(<<"claim-name">>, ClaimScript),
        <<"claim-value-size">> => byte_size(ClaimValue),
        <<"claim-value-hash">> => sha256_hex(ClaimValue),
        <<"claim-proof-store-path">> =>
            <<"odysee/claim-proof/", TxID/binary, "/", (integer_to_binary(NOut))/binary>>,
        <<"claim-script-valid">> => true,
        <<"proof-tier">> => <<"raw-transaction-output">>
    },
    put_optional({<<"claim-value-hex">>, claim_value_hex(ClaimValue)}, Msg0).

claim_value_hex(<<>>) -> not_found;
claim_value_hex(Value) -> hb_util:to_hex(Value).

add_validity(Proof, Expected, Opts) ->
    TxIDValid = expected_equal(<<"txid">>, Proof, Expected, Opts),
    NOutValid = expected_equal(<<"nout">>, Proof, Expected, Opts),
    ClaimIDValid = expected_equal(<<"claim-id">>, Proof, Expected, Opts),
    ClaimNameValid = expected_equal(<<"claim-name">>, Proof, Expected, Opts),
    ValueHashValid = expected_equal(<<"claim-value-hash">>, Proof, Expected, Opts),
    ScriptValid = truthy(hb_maps:get(<<"claim-script-valid">>, Proof, false, Opts)),
    Valid =
        ScriptValid
            andalso TxIDValid
            andalso NOutValid
            andalso ClaimIDValid
            andalso ClaimNameValid
            andalso ValueHashValid,
    Proof#{
        <<"valid">> => Valid,
        <<"txid-valid">> => TxIDValid,
        <<"nout-valid">> => NOutValid,
        <<"claim-id-valid">> => ClaimIDValid,
        <<"claim-name-valid">> => ClaimNameValid,
        <<"claim-value-hash-valid">> => ValueHashValid
    }.

expected_fields(Base, Req, Opts) ->
    Claim = first_map([<<"claim">>, <<"output">>], [Req, Base], Opts),
    maps:from_list(
        lists:filtermap(
            fun({Key, Keys}) ->
                case first_value(Keys, [Req, Base, Claim], Opts) of
                    not_found -> false;
                    Value -> {true, {Key, normalize_expected(Key, Value)}}
                end
            end,
            [
                {<<"txid">>, [<<"txid">>]},
                {<<"nout">>, [<<"nout">>, <<"output-index">>, <<"output_index">>]},
                {<<"claim-id">>, [<<"claim-id">>, <<"claim_id">>]},
                {<<"claim-name">>, [<<"claim-name">>, <<"name">>]},
                {<<"claim-value-hash">>, [<<"claim-value-hash">>, <<"claim_value_hash">>]}
            ]
        )
    ).

expected_equal(Key, Proof, Expected, Opts) ->
    case hb_maps:get(Key, Expected, not_found, Opts) of
        not_found ->
            true;
        ExpectedValue ->
            normalize_expected(Key, hb_maps:get(Key, Proof, not_found, Opts)) =:= ExpectedValue
    end.

normalize_expected(<<"nout">>, Value) when is_binary(Value) ->
    binary_to_integer(Value);
normalize_expected(Key, Value) when Key =:= <<"txid">>; Key =:= <<"claim-id">>; Key =:= <<"claim-value-hash">> ->
    normalize_hex(Value);
normalize_expected(_Key, Value) ->
    Value.

tx_hex(Base, Req, Opts) ->
    case first_value(
        [
            <<"tx-hex">>,
            <<"tx_hex">>,
            <<"raw-transaction">>,
            <<"raw_transaction">>,
            <<"hex">>,
            <<"body">>
        ],
        [Req, Base],
        Opts
    ) of
        Hex when is_binary(Hex) -> {ok, normalize_hex(Hex)};
        _ -> {error, tx_hex_not_found}
    end.

nout(Base, Req, Opts) ->
    case first_value([<<"nout">>, <<"output-index">>, <<"output_index">>], [Req, Base], Opts) of
        NOut when is_integer(NOut), NOut >= 0 -> {ok, NOut};
        NOut when is_binary(NOut) ->
            try
                Int = binary_to_integer(NOut),
                case Int >= 0 of
                    true -> {ok, Int};
                    false -> {error, invalid_nout}
                end
            catch _:_ ->
                {error, invalid_nout}
            end;
        _ ->
            {error, nout_not_found}
    end.

parse_tx(RawTx) ->
    maybe
        {ok, VersionBin, R0} ?= take(4, RawTx),
        {HasWitness, R1} = witness_prefix(R0),
        {ok, InputCount, R2, InputCountBin} ?= read_varint(R1),
        {ok, Inputs, R3, InputsBin} ?= parse_inputs(InputCount, R2),
        {ok, OutputCount, R4, OutputCountBin} ?= read_varint(R3),
        {ok, Outputs, R5, OutputsBin} ?= parse_outputs(OutputCount, R4),
        {ok, R6} ?= skip_witnesses(HasWitness, InputCount, R5),
        {ok, LockTimeBin, <<>>} ?= take(4, R6),
        NonWitness =
            <<
                VersionBin/binary,
                InputCountBin/binary,
                InputsBin/binary,
                OutputCountBin/binary,
                OutputsBin/binary,
                LockTimeBin/binary
            >>,
        TxHash = hash256(NonWitness),
        {ok, #{
            <<"version">> => VersionBin,
            <<"has-witness">> => HasWitness,
            <<"inputs">> => Inputs,
            <<"outputs">> => Outputs,
            <<"lock-time">> => LockTimeBin,
            <<"tx-hash">> => TxHash,
            <<"txid">> => hb_util:to_hex(reverse_binary(TxHash))
        }}
    end.

witness_prefix(<<0, Flag, Rest/binary>>) when Flag =/= 0 ->
    {true, Rest};
witness_prefix(Bin) ->
    {false, Bin}.

parse_inputs(Count, Bin) ->
    parse_inputs(Count, Bin, [], []).

parse_inputs(0, Bin, Inputs, Parts) ->
    {ok, lists:reverse(Inputs), Bin, iolist_to_binary(lists:reverse(Parts))};
parse_inputs(Count, Bin, Inputs, Parts) ->
    maybe
        {ok, PrevHash, R0} ?= take(32, Bin),
        {ok, IndexBin, R1} ?= take(4, R0),
        Index = little_unsigned(IndexBin),
        {ok, ScriptLen, R2, ScriptLenBin} ?= read_varint(R1),
        {ok, Script, R3} ?= take(ScriptLen, R2),
        {ok, Sequence, R4} ?= take(4, R3),
        Part = <<PrevHash/binary, IndexBin/binary, ScriptLenBin/binary, Script/binary, Sequence/binary>>,
        Input = #{
            <<"previous-txid">> => hb_util:to_hex(reverse_binary(PrevHash)),
            <<"previous-nout">> => Index,
            <<"script">> => Script,
            <<"sequence">> => Sequence
        },
        parse_inputs(Count - 1, R4, [Input | Inputs], [Part | Parts])
    end.

parse_outputs(Count, Bin) ->
    parse_outputs(Count, Bin, [], []).

parse_outputs(0, Bin, Outputs, Parts) ->
    {ok, lists:reverse(Outputs), Bin, iolist_to_binary(lists:reverse(Parts))};
parse_outputs(Count, Bin, Outputs, Parts) ->
    maybe
        {ok, AmountBin, R0} ?= take(8, Bin),
        Amount = little_unsigned(AmountBin),
        {ok, ScriptLen, R1, ScriptLenBin} ?= read_varint(R0),
        {ok, Script, R2} ?= take(ScriptLen, R1),
        Part = <<AmountBin/binary, ScriptLenBin/binary, Script/binary>>,
        Output = #{ <<"amount">> => Amount, <<"script">> => Script },
        parse_outputs(Count - 1, R2, [Output | Outputs], [Part | Parts])
    end.

skip_witnesses(false, _InputCount, Bin) ->
    {ok, Bin};
skip_witnesses(true, InputCount, Bin) ->
    skip_witnesses(InputCount, Bin).

skip_witnesses(0, Bin) ->
    {ok, Bin};
skip_witnesses(Count, Bin) ->
    maybe
        {ok, ItemCount, R0, _Enc} ?= read_varint(Bin),
        {ok, R1} ?= skip_witness_items(ItemCount, R0),
        skip_witnesses(Count - 1, R1)
    end.

skip_witness_items(0, Bin) ->
    {ok, Bin};
skip_witness_items(Count, Bin) ->
    maybe
        {ok, Size, R0, _Enc} ?= read_varint(Bin),
        {ok, _Item, R1} ?= take(Size, R0),
        skip_witness_items(Count - 1, R1)
    end.

output_at(NOut, Outputs) when NOut < length(Outputs) ->
    {ok, lists:nth(NOut + 1, Outputs)};
output_at(_NOut, _Outputs) ->
    {error, output_not_found}.

decode_claim_script(Script) ->
    maybe
        {ok, {op, Op}, R0} ?= script_op(Script),
        ok ?= require_claim_op(Op),
        {ok, Name, R1} ?= script_push(R0),
        {ok, Param2, R2} ?= script_push(R1),
        decode_claim_script(Op, Name, Param2, R2)
    end.

decode_claim_script(?OP_CLAIM_NAME, Name, Value, Rest0) ->
    maybe
        {ok, {op, ?OP_2DROP}, Rest1} ?= script_op(Rest0),
        {ok, {op, ?OP_DROP}, AddressScript} ?= script_op(Rest1),
        {ok, #{
            <<"claim-op">> => <<"create">>,
            <<"claim-name">> => Name,
            <<"claim-value">> => Value,
            <<"address-script">> => AddressScript
        }}
    end;
decode_claim_script(Op, Name, ClaimIDBytes, Rest0)
        when Op =:= ?OP_UPDATE_CLAIM; Op =:= ?OP_SUPPORT_CLAIM ->
    maybe
        ok ?= require_claim_id_bytes(ClaimIDBytes),
        {ok, Next, Rest1} ?= script_op(Rest0),
        decode_update_or_support(Op, Name, ClaimIDBytes, Next, Rest1)
    end.

decode_update_or_support(?OP_UPDATE_CLAIM, Name, ClaimIDBytes, {push, Value}, Rest0) ->
    maybe
        {ok, {op, ?OP_2DROP}, Rest1} ?= script_op(Rest0),
        {ok, {op, ?OP_2DROP}, AddressScript} ?= script_op(Rest1),
        {ok, #{
            <<"claim-op">> => <<"update">>,
            <<"claim-name">> => Name,
            <<"claim-id-bytes">> => ClaimIDBytes,
            <<"claim-value">> => Value,
            <<"address-script">> => AddressScript
        }}
    end;
decode_update_or_support(?OP_UPDATE_CLAIM, _Name, _ClaimIDBytes, _Next, _Rest) ->
    {error, invalid_claim_script};
decode_update_or_support(?OP_SUPPORT_CLAIM, Name, ClaimIDBytes, {push, Value}, Rest0) ->
    maybe
        {ok, {op, ?OP_2DROP}, Rest1} ?= script_op(Rest0),
        {ok, {op, ?OP_2DROP}, AddressScript} ?= script_op(Rest1),
        {ok, #{
            <<"claim-op">> => <<"support">>,
            <<"claim-name">> => Name,
            <<"claim-id-bytes">> => ClaimIDBytes,
            <<"claim-value">> => Value,
            <<"address-script">> => AddressScript
        }}
    end;
decode_update_or_support(?OP_SUPPORT_CLAIM, Name, ClaimIDBytes, {op, ?OP_2DROP}, Rest0) ->
    maybe
        {ok, {op, ?OP_DROP}, AddressScript} ?= script_op(Rest0),
        {ok, #{
            <<"claim-op">> => <<"support">>,
            <<"claim-name">> => Name,
            <<"claim-id-bytes">> => ClaimIDBytes,
            <<"claim-value">> => <<>>,
            <<"address-script">> => AddressScript
        }}
    end;
decode_update_or_support(?OP_SUPPORT_CLAIM, _Name, _ClaimIDBytes, _Next, _Rest) ->
    {error, invalid_claim_script}.

claim_id(#{ <<"claim-op">> := <<"create">> }, TxHash, NOut) ->
    ClaimID = hash160(<<TxHash/binary, NOut:32/big-unsigned-integer>>),
    hb_util:to_hex(reverse_binary(ClaimID));
claim_id(ClaimScript, _TxHash, _NOut) ->
    hb_util:to_hex(reverse_binary(hb_maps:get(<<"claim-id-bytes">>, ClaimScript, #{}))).

script_push(Bin) ->
    case script_op(Bin) of
        {ok, {push, Data}, Rest} -> {ok, Data, Rest};
        {ok, {op, _Op}, _Rest} -> {error, expected_push};
        Error -> Error
    end.

script_op(<<Op, Rest/binary>>) when Op < ?OP_PUSHDATA1 ->
    maybe
        {ok, Data, R0} ?= take(Op, Rest),
        {ok, {push, Data}, R0}
    end;
script_op(<<?OP_PUSHDATA1, Len, Rest/binary>>) ->
    maybe
        {ok, Data, R0} ?= take(Len, Rest),
        {ok, {push, Data}, R0}
    end;
script_op(<<?OP_PUSHDATA2, Len:16/little-unsigned-integer, Rest/binary>>) ->
    maybe
        {ok, Data, R0} ?= take(Len, Rest),
        {ok, {push, Data}, R0}
    end;
script_op(<<?OP_PUSHDATA4, Len:32/little-unsigned-integer, Rest/binary>>) ->
    maybe
        {ok, Data, R0} ?= take(Len, Rest),
        {ok, {push, Data}, R0}
    end;
script_op(<<Op, Rest/binary>>) ->
    {ok, {op, Op}, Rest};
script_op(<<>>) ->
    {error, truncated_script}.

read_varint(<<N, Rest/binary>>) when N < 16#fd ->
    {ok, N, Rest, <<N>>};
read_varint(<<16#fd, N:16/little-unsigned-integer, Rest/binary>>) ->
    {ok, N, Rest, <<16#fd, N:16/little-unsigned-integer>>};
read_varint(<<16#fe, N:32/little-unsigned-integer, Rest/binary>>) ->
    {ok, N, Rest, <<16#fe, N:32/little-unsigned-integer>>};
read_varint(<<16#ff, N:64/little-unsigned-integer, Rest/binary>>) ->
    {ok, N, Rest, <<16#ff, N:64/little-unsigned-integer>>};
read_varint(_Bin) ->
    {error, truncated_varint}.

take(Size, Bin) when byte_size(Bin) >= Size ->
    {Head, Tail} = split_binary(Bin, Size),
    {ok, Head, Tail};
take(_Size, _Bin) ->
    {error, truncated_binary}.

require_claim_op(?OP_CLAIM_NAME) -> ok;
require_claim_op(?OP_SUPPORT_CLAIM) -> ok;
require_claim_op(?OP_UPDATE_CLAIM) -> ok;
require_claim_op(_Op) -> {error, not_claim_script}.

require_claim_id_bytes(ClaimIDBytes) when byte_size(ClaimIDBytes) =:= 20 ->
    ok;
require_claim_id_bytes(_ClaimIDBytes) ->
    {error, invalid_claim_id_bytes}.

decode_hex(Name, Hex) ->
    try {ok, binary:decode_hex(normalize_hex(Hex))}
    catch _:_ -> {error, {invalid_hex, Name}}
    end.

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:bin(string:lowercase(binary_to_list(Hex))).

little_unsigned(Bin) ->
    binary:decode_unsigned(Bin, little).

hash256(Bin) ->
    crypto:hash(sha256, crypto:hash(sha256, Bin)).

hash160(Bin) ->
    crypto:hash(ripemd160, crypto:hash(sha256, Bin)).

sha256_hex(Bin) ->
    hb_util:to_hex(crypto:hash(sha256, Bin)).

reverse_binary(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

first_map(_Keys, [], _Opts) ->
    #{};
first_map(Keys, [Msg | Rest], Opts) when is_map(Msg) ->
    case first_value(Keys, Msg, Opts) of
        Value when is_map(Value) -> Value;
        _ -> first_map(Keys, Rest, Opts)
    end;
first_map(Keys, [_Msg | Rest], Opts) ->
    first_map(Keys, Rest, Opts).

first_value(Keys, Msgs, Opts) when is_list(Msgs), not is_binary(Msgs) ->
    first_value_from_msgs(Keys, Msgs, Opts);
first_value([], _Map, _Opts) ->
    not_found;
first_value([Key | Rest], Map, Opts) when is_map(Map) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> first_value(Rest, Map, Opts);
        Value -> Value
    end;
first_value(_Keys, _Map, _Opts) ->
    not_found.

first_value_from_msgs(_Keys, [], _Opts) ->
    not_found;
first_value_from_msgs(Keys, [Msg | Rest], Opts) ->
    case first_value(Keys, Msg, Opts) of
        not_found -> first_value_from_msgs(Keys, Rest, Opts);
        Value -> Value
    end.

put_optional({_Key, not_found}, Msg) -> Msg;
put_optional({Key, Value}, Msg) -> Msg#{ Key => Value }.

truthy(true) -> true;
truthy(<<"true">>) -> true;
truthy(<<"1">>) -> true;
truthy(1) -> true;
truthy(_Value) -> false.

-ifdef(TEST).

verify_create_claim_script_test() ->
    {TxHex, TxID, ClaimID} = claim_tx_fixture(<<"example">>, <<"raw claim">>),
    {ok, Proof} =
        verify(
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
    ?assertEqual(true, hb_maps:get(<<"valid">>, Proof, #{})),
    ?assertEqual(<<"create">>, hb_maps:get(<<"claim-op">>, Proof, #{})),
    ?assertEqual(<<"example">>, hb_maps:get(<<"claim-name">>, Proof, #{})),
    ?assertEqual(ClaimID, hb_maps:get(<<"claim-id">>, Proof, #{})).

verify_rejects_wrong_claim_id_test() ->
    {TxHex, TxID, _ClaimID} = claim_tx_fixture(<<"example">>, <<"raw claim">>),
    {ok, Proof} =
        verify(
            #{},
            #{
                <<"tx-hex">> => TxHex,
                <<"txid">> => TxID,
                <<"nout">> => 0,
                <<"claim-id">> => <<"bad">>
            },
            #{}
        ),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Proof, #{})),
    ?assertEqual(false, hb_maps:get(<<"claim-id-valid">>, Proof, #{})).

verify_update_claim_script_test() ->
    ExistingClaimID = <<"346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169">>,
    {TxHex, TxID} = update_tx_fixture(<<"example">>, ExistingClaimID, <<"updated">>),
    {ok, Proof} =
        verify(
            #{},
            #{
                <<"tx-hex">> => TxHex,
                <<"txid">> => TxID,
                <<"nout">> => 0,
                <<"claim-id">> => ExistingClaimID,
                <<"claim-name">> => <<"example">>
            },
            #{}
        ),
    ?assertEqual(true, hb_maps:get(<<"valid">>, Proof, #{})),
    ?assertEqual(<<"update">>, hb_maps:get(<<"claim-op">>, Proof, #{})),
    ?assertEqual(ExistingClaimID, hb_maps:get(<<"claim-id">>, Proof, #{})).

verify_rejects_txid_mismatch_test() ->
    {TxHex, _TxID, ClaimID} = claim_tx_fixture(<<"example">>, <<"raw claim">>),
    {ok, Proof} =
        verify(
            #{},
            #{
                <<"tx-hex">> => TxHex,
                <<"txid">> => <<"00">>,
                <<"nout">> => 0,
                <<"claim-id">> => ClaimID
            },
            #{}
        ),
    ?assertEqual(false, hb_maps:get(<<"valid">>, Proof, #{})),
    ?assertEqual(false, hb_maps:get(<<"txid-valid">>, Proof, #{})).

claim_tx_fixture(Name, Value) ->
    Script = claim_script(Name, Value),
    RawTx = tx_with_script(Script),
    {ok, Tx} = parse_tx(RawTx),
    TxHash = hb_maps:get(<<"tx-hash">>, Tx, #{}),
    TxID = hb_maps:get(<<"txid">>, Tx, #{}),
    ClaimID = hb_util:to_hex(reverse_binary(hash160(<<TxHash/binary, 0:32/big>>))),
    {hb_util:to_hex(RawTx), TxID, ClaimID}.

update_tx_fixture(Name, ClaimID, Value) ->
    {ok, ClaimIDBytes} = decode_hex(<<"claim-id">>, ClaimID),
    Script = update_script(Name, reverse_binary(ClaimIDBytes), Value),
    RawTx = tx_with_script(Script),
    {ok, Tx} = parse_tx(RawTx),
    {hb_util:to_hex(RawTx), hb_maps:get(<<"txid">>, Tx, #{})}.

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
    AddressScript = p2pkh_script(),
    <<
        ?OP_CLAIM_NAME,
        (push(Name))/binary,
        (push(Value))/binary,
        ?OP_2DROP,
        ?OP_DROP,
        AddressScript/binary
    >>.

update_script(Name, ClaimIDBytes, Value) ->
    AddressScript = p2pkh_script(),
    <<
        ?OP_UPDATE_CLAIM,
        (push(Name))/binary,
        (push(ClaimIDBytes))/binary,
        (push(Value))/binary,
        ?OP_2DROP,
        ?OP_2DROP,
        AddressScript/binary
    >>.

p2pkh_script() ->
    <<16#76, 16#a9, 20, 0:160, 16#88, 16#ac>>.

push(Bin) when byte_size(Bin) < ?OP_PUSHDATA1 ->
    <<(byte_size(Bin)), Bin/binary>>.

-endif.
