-module(hb_lbry_tx).
-export([
    parse/1,
    parse_hex/1,
    txid/1,
    double_sha256/1,
    hash160/1,
    parse_claim_envelope/1,
    signature_hash/3
]).
-ifdef(TEST).
-export([task0_tx_hex/0]).
-endif.
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(OP_CLAIM_NAME, 16#b5).
-define(OP_UPDATE_CLAIM, 16#b7).
-define(OP_2DROP, 16#6d).
-define(OP_DROP, 16#75).
-define(OP_PUSHDATA1, 16#4c).
-define(OP_PUSHDATA2, 16#4d).
-define(OP_PUSHDATA4, 16#4e).

parse_hex(Hex) when is_binary(Hex) ->
    try parse(binary:decode_hex(Hex)) of
        Result -> Result
    catch
        _:_ -> {error, invalid_tx_hex}
    end.

parse(Raw) when is_binary(Raw) ->
    maybe
        {ok, Version, Rest1} ?= read_int32_le(Raw),
        {ok, InputCount, Rest2} ?= read_varint(Rest1),
        {ok, Inputs, Rest3} ?= read_inputs(InputCount, Rest2, []),
        {ok, OutputCount, Rest4} ?= read_varint(Rest3),
        TxHash = double_sha256(Raw),
        {ok, Outputs, Rest5} ?= read_outputs(OutputCount, Rest4, TxHash, 0, []),
        {ok, LockTime, <<>>} ?= read_uint32_le(Rest5),
        {ok, #{
            <<"raw">> => Raw,
            <<"version">> => Version,
            <<"txid">> => txid(Raw),
            <<"inputs">> => Inputs,
            <<"outputs">> => Outputs,
            <<"lock-time">> => LockTime
        }}
    end.

txid(Raw) ->
    hb_util:to_hex(reverse(double_sha256(Raw))).

%% @doc Compute the legacy Bitcoin/LBRY `SIGHASH_ALL' digest for one input of
%% a parsed transaction. The scriptCode is the spent output's full
%% scriptPubKey -- for claim outputs that includes the claim prefix, not only
%% the trailing payment script: lbcd never strips the prefix during script
%% execution, and `opcodeCheckSig' hashes the whole executing pkScript. The
%% spending input's script is replaced with the scriptCode, every other input
%% script is emptied, outputs and sequences stay untouched, and the hash type
%% is appended as a 4-byte little-endian value before double-SHA256. The
%% caller must reject scriptCodes containing `OP_CODESEPARATOR' outside push
%% data; the standard claim and payment script shapes accepted by the
%% ancestry verifier cannot contain one.
signature_hash(Tx, InputIndex, ScriptCode) ->
    Inputs = maps:get(<<"inputs">>, Tx),
    SerializedInputs =
        [
            serialize_input(Input, Position, InputIndex, ScriptCode)
         ||
            {Input, Position} <-
                lists:zip(Inputs, lists:seq(0, length(Inputs) - 1))
        ],
    Outputs = maps:get(<<"outputs">>, Tx),
    Preimage = <<
        (maps:get(<<"version">>, Tx)):32/little-signed,
        (varint(length(Inputs)))/binary,
        (iolist_to_binary(SerializedInputs))/binary,
        (varint(length(Outputs)))/binary,
        (iolist_to_binary([serialize_output(Output) || Output <- Outputs]))/binary,
        (maps:get(<<"lock-time">>, Tx)):32/little,
        1:32/little
    >>,
    double_sha256(Preimage).

serialize_input(Input, Position, Position, ScriptCode) ->
    serialize_input_with_script(Input, ScriptCode);
serialize_input(Input, _Position, _InputIndex, _ScriptCode) ->
    serialize_input_with_script(Input, <<>>).

serialize_input_with_script(Input, Script) ->
    <<
        (maps:get(<<"prev-tx-hash">>, Input))/binary,
        (maps:get(<<"prev-nout">>, Input)):32/little,
        (varbytes(Script))/binary,
        (maps:get(<<"sequence">>, Input)):32/little
    >>.

serialize_output(Output) ->
    <<
        (maps:get(<<"amount">>, Output)):64/little,
        (varbytes(maps:get(<<"script">>, Output)))/binary
    >>.

varbytes(Bytes) ->
    <<(varint(byte_size(Bytes)))/binary, Bytes/binary>>.

varint(Value) when Value < 16#fd ->
    <<Value>>;
varint(Value) when Value =< 16#ffff ->
    <<16#fd, Value:16/little>>;
varint(Value) when Value =< 16#ffffffff ->
    <<16#fe, Value:32/little>>;
varint(Value) ->
    <<16#ff, Value:64/little>>.

double_sha256(Raw) ->
    crypto:hash(sha256, crypto:hash(sha256, Raw)).

hash160(Raw) ->
    crypto:hash(ripemd160, crypto:hash(sha256, Raw)).

read_inputs(0, Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest};
read_inputs(Count, Raw, Acc) ->
    maybe
        {ok, PrevHash, Rest1} ?= take(32, Raw),
        {ok, Vout, Rest2} ?= read_uint32_le(Rest1),
        {ok, Script, Rest3} ?= read_varbytes(Rest2),
        {ok, Sequence, Rest4} ?= read_uint32_le(Rest3),
        Input = #{
            <<"prev-tx-hash">> => PrevHash,
            <<"prev-txid">> => hb_util:to_hex(reverse(PrevHash)),
            <<"prev-nout">> => Vout,
            <<"script">> => Script,
            <<"sequence">> => Sequence,
            <<"signature-digest-piece">> => <<PrevHash/binary, Vout:32/little>>
        },
        read_inputs(Count - 1, Rest4, [Input | Acc])
    end.

read_outputs(0, Rest, _TxHash, _Position, Acc) ->
    {ok, lists:reverse(Acc), Rest};
read_outputs(Count, Raw, TxHash, Position, Acc) ->
    maybe
        {ok, Amount, Rest1} ?= read_uint64_le(Raw),
        {ok, Script, Rest2} ?= read_varbytes(Rest1),
        Output0 = #{
            <<"amount">> => Amount,
            <<"nout">> => Position,
            <<"script">> => Script
        },
        Output = maybe_add_claim(Output0, TxHash, Position),
        read_outputs(Count - 1, Rest2, TxHash, Position + 1, [Output | Acc])
    end.

maybe_add_claim(Output = #{ <<"script">> := Script }, TxHash, Position) ->
    case parse_claim_script(Script, TxHash, Position) of
        {ok, Claim} -> maps:merge(Output, Claim);
        {error, _} -> Output
    end.

parse_claim_script(<<?OP_CLAIM_NAME, Rest0/binary>>, TxHash, Position) ->
    maybe
        {ok, Name, Rest1} ?= read_push(Rest0),
        {ok, ClaimBytes, <<?OP_2DROP, ?OP_DROP, PaymentScript/binary>>} ?=
            read_push(Rest1),
        ClaimHash = hash160(<<TxHash/binary, Position:32/big>>),
        {ok, Envelope} ?= parse_claim_envelope(ClaimBytes),
        {ok, #{
            <<"claim-op">> => <<"create">>,
            <<"claim-name">> => Name,
            <<"claim">> => ClaimBytes,
            <<"claim-id">> => hb_util:to_hex(reverse(ClaimHash)),
            <<"claim-hash">> => ClaimHash,
            <<"claim-envelope">> => Envelope,
            <<"payment-script">> => PaymentScript
        }}
    end;
parse_claim_script(<<?OP_UPDATE_CLAIM, Rest0/binary>>, _TxHash, _Position) ->
    maybe
        {ok, Name, Rest1} ?= read_push(Rest0),
        {ok, ClaimHash, Rest2} ?= read_push(Rest1),
        {ok, ClaimBytes, <<?OP_2DROP, ?OP_2DROP, PaymentScript/binary>>} ?=
            read_push(Rest2),
        {ok, Envelope} ?= parse_claim_envelope(ClaimBytes),
        {ok, #{
            <<"claim-op">> => <<"update">>,
            <<"claim-name">> => Name,
            <<"claim">> => ClaimBytes,
            <<"claim-id">> => hb_util:to_hex(reverse(ClaimHash)),
            <<"claim-hash">> => ClaimHash,
            <<"claim-envelope">> => Envelope,
            <<"payment-script">> => PaymentScript
        }}
    end;
parse_claim_script(_Script, _TxHash, _Position) ->
    {error, not_claim}.

parse_claim_envelope(<<0, Message/binary>> = Raw) ->
    {ok, #{
        <<"raw">> => Raw,
        <<"encoding">> => <<"v2-protobuf">>,
        <<"signed">> => false,
        <<"message">> => Message
    }};
parse_claim_envelope(
    <<1, SigningChannelHash:20/binary, Signature:64/binary, Message/binary>> = Raw
) ->
    {ok, #{
        <<"raw">> => Raw,
        <<"encoding">> => <<"v2-protobuf">>,
        <<"signed">> => true,
        <<"signing-channel-hash">> => SigningChannelHash,
        <<"signing-channel-id">> => hb_util:to_hex(reverse(SigningChannelHash)),
        <<"claim-signature">> => Signature,
        <<"message">> => Message
    }};
parse_claim_envelope(<<"{", _/binary>> = Raw) ->
    {ok, #{
        <<"raw">> => Raw,
        <<"encoding">> => <<"v0-json">>,
        <<"signed">> => false,
        <<"message">> => Raw
    }};
parse_claim_envelope(Raw) when is_binary(Raw), byte_size(Raw) > 0 ->
    {ok, #{
        <<"raw">> => Raw,
        <<"encoding">> => <<"v1-protobuf">>,
        <<"signed">> => false,
        <<"message">> => Raw
    }};
parse_claim_envelope(_) ->
    {error, invalid_claim_envelope}.

read_push(<<Len:8, Rest/binary>>) when Len > 0, Len < ?OP_PUSHDATA1 ->
    take(Len, Rest);
read_push(<<?OP_PUSHDATA1, Len:8, Rest/binary>>) ->
    take(Len, Rest);
read_push(<<?OP_PUSHDATA2, Len:16/little, Rest/binary>>) ->
    take(Len, Rest);
read_push(<<?OP_PUSHDATA4, Len:32/little, Rest/binary>>) ->
    take(Len, Rest);
read_push(<<0, Rest/binary>>) ->
    {ok, <<>>, Rest};
read_push(_) ->
    {error, invalid_pushdata}.

read_varbytes(Raw) ->
    maybe
        {ok, Size, Rest} ?= read_varint(Raw),
        take(Size, Rest)
    end.

read_varint(<<Value:8, Rest/binary>>) when Value < 16#fd ->
    {ok, Value, Rest};
read_varint(<<16#fd, Value:16/little, Rest/binary>>) ->
    {ok, Value, Rest};
read_varint(<<16#fe, Value:32/little, Rest/binary>>) ->
    {ok, Value, Rest};
read_varint(<<16#ff, Value:64/little, Rest/binary>>) ->
    {ok, Value, Rest};
read_varint(_) ->
    {error, invalid_varint}.

read_int32_le(<<Value:32/little-signed, Rest/binary>>) ->
    {ok, Value, Rest};
read_int32_le(_) ->
    {error, truncated_int32}.

read_uint32_le(<<Value:32/little, Rest/binary>>) ->
    {ok, Value, Rest};
read_uint32_le(_) ->
    {error, truncated_uint32}.

read_uint64_le(<<Value:64/little, Rest/binary>>) ->
    {ok, Value, Rest};
read_uint64_le(_) ->
    {error, truncated_uint64}.

take(Size, Raw) when is_integer(Size), Size >= 0, byte_size(Raw) >= Size ->
    <<Value:Size/binary, Rest/binary>> = Raw,
    {ok, Value, Rest};
take(_, _) ->
    {error, truncated_binary}.

reverse(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

parse_task0_transaction_test() ->
    {ok, Tx} = parse_hex(task0_tx_hex()),
    ?assertEqual(
        <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
        maps:get(<<"txid">>, Tx)
    ),
    [Input] = maps:get(<<"inputs">>, Tx),
    ?assertEqual(
        <<"54e8da1d574ff931ebbf136da148c50d7643433ac57c0aee9fa60a8e3a846e2f">>,
        maps:get(<<"prev-txid">>, Input)
    ),
    [ClaimOutput, PaymentOutput] = maps:get(<<"outputs">>, Tx),
    ?assertEqual(100000, maps:get(<<"amount">>, ClaimOutput)),
    ?assertEqual(344174550, maps:get(<<"amount">>, PaymentOutput)),
    ?assertEqual(<<"create">>, maps:get(<<"claim-op">>, ClaimOutput)),
    ?assertEqual(
        <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
        maps:get(<<"claim-id">>, ClaimOutput)
    ),
    Envelope = maps:get(<<"claim-envelope">>, ClaimOutput),
    ?assertEqual(<<"v2-protobuf">>, maps:get(<<"encoding">>, Envelope)),
    ?assertEqual(true, maps:get(<<"signed">>, Envelope)),
    ?assertEqual(
        <<"585d54c7b82fd92043ed583c5aea18a9547028aa">>,
        maps:get(<<"signing-channel-id">>, Envelope)
    ),
    ?assertEqual(64, byte_size(maps:get(<<"claim-signature">>, Envelope))),
    ?assert(byte_size(maps:get(<<"message">>, Envelope)) > 0).

hash160_claim_id_derivation_test() ->
    {ok, Tx} = parse_hex(task0_tx_hex()),
    [ClaimOutput | _] = maps:get(<<"outputs">>, Tx),
    ?assertEqual(
        <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
        maps:get(<<"claim-id">>, ClaimOutput)
    ).

task0_tx_hex() ->
    <<
        "01000000012f6e843a8e0aa69fee0a7cc53a4343760dc548a16d13bfeb31f94f571ddae854010000006a47304402202ee7491d13424d2d06ae2407d48d3280223140dfe19e6d14ceedd2609d19e92b0220069a68ed6cd682ee442d8e39ce7f72f5e772b12614a0ab7796c42d817de25ce301210378ff344cc1f8a5451e7b8f348670b20c44ae44704ac05c59fb936ac1a4f26769ffffffff02a086010000000000fde801b531416666616972652d42726967697474652d5f2dc3a7612d64c3a97261696c6c652d656e2d706c65696e2d6469726563742d4d970101aa287054a918ea5a3c58ed4320d92fb8c7545d58e2bd32941d8256b818f5a72bc5ab16bcecff961261e5ea0036a0d7e26aa24738010ef602e0683690d7601cfe3df9268e46dfbb925a70cd16216e046ed17f3da60ac5010aab010a30cb215d05f21823b1208313edeaf8d7af4b2d2d00acc58fac1a1cf40427351b3b79a636b70f5844f6c691330955a53b18123541666661697265204272696769747465205f20c3a7612064c3a97261696c6c6520656e20706c65696e20646972656374202e6d7034188bcb861e2209766964656f2f6d703432303da16b833f169c21caeb62ca66111227413f30f63c9d2f52f2a787643e086c334ee6949e05875cfe94a816aba02e492e1a044e6f6e6528faa2a0d1065a0908800510e802188a08423141666661697265204272696769747465205f20c3a7612064c3a97261696c6c6520656e20706c65696e206469726563742052412a3f68747470733a2f2f7468756d62732e6f647963646e2e636f6d2f62353765383966656131653333636136623761616536386638363735623235622e77656270620208016d7576a914b462dfca8f203323f9c4375e4160e257f61aca7888acd6af8314000000001976a914b462dfca8f203323f9c4375e4160e257f61aca7888ac00000000"
    >>.
