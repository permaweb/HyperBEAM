-module(dev_lbry_transaction).
-implements(<<"lbry-transaction@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([from/3, to/3, to_hint/3, verify/3, content_type/1]).
-include("include/hb.hrl").

content_type(_) ->
    {ok, <<"application/vnd.lbry.transaction">>}.

from(Map, Req, Opts) when is_map(Map) ->
    lib_lbry_codec:from_structured(ensure_device(Map), Req, Opts);
from(Raw, Req, Opts) when is_binary(Raw) ->
    case parse_input(Raw, Req, Opts) of
        {ok, Tx} ->
            lib_lbry_codec:from_structured(ensure_device(Tx), Req, Opts);
        Error ->
            Error
    end.

to(Bin, _Req, _Opts) when is_binary(Bin) ->
    {ok, Bin};
to(TABM, Req, Opts) ->
    {ok, Structured} = lib_lbry_codec:to_structured(TABM, Req, Opts),
    lib_lbry_codec:raw_hex_or_structured(ensure_device(Structured), Req, Opts).

to_hint(_Msg, Req, _Opts) ->
    lib_lbry_codec:to_hint(Req).

%% @doc Verify a transaction commitment: the raw transaction bytes must
%% recompute to the commitment's native display-order txid and parse as a
%% valid transaction. The message's `txid' and `device' keys must agree with
%% the commitment. Any missing or mismatching input fails closed.
verify(Base, Req, Opts) ->
    Valid =
        maybe
            <<"sha-256d">> ?= hb_maps:get(<<"type">>, Req, undefined, Opts),
            <<"lbry-transaction@1.0">> ?=
                hb_maps:get(<<"device">>, Base, undefined, Opts),
            ok ?=
                hb_lbry_commitment:committed_subset(
                    Req,
                    [<<"device">>, <<"raw">>, <<"txid">>],
                    Opts
                ),
            {ok, Hex, Bytes} ?= hb_lbry_commitment:native_id(Req, Opts),
            32 ?= byte_size(Bytes),
            Raw = hb_maps:get(<<"raw">>, Base, undefined, Opts),
            true ?= is_binary(Raw),
            Hex ?= hb_lbry_tx:txid(Raw),
            {ok, _Tx} ?= hb_lbry_tx:parse(Raw),
            Hex == txid_field(Base, Opts)
        else
            _ -> false
        end,
    ?event(lbry_commitment, {transaction_verify, {valid, Valid}}),
    {ok, Valid}.

parse_input(Raw, Req, Opts) ->
    Decoded =
        case hb_maps:get(<<"encoding">>, Req, undefined, Opts) of
            <<"hex">> ->
                decode_hex(Raw);
            _ ->
                {ok, Raw}
        end,
    case Decoded of
        {ok, Bytes} ->
            case hb_lbry_commitment:transaction_message(Bytes) of
                {ok, _} = Ok -> Ok;
                Error when Raw == Bytes -> retry_as_hex(Raw, Error);
                Error -> Error
            end;
        Error ->
            Error
    end.

%% Bare binary inputs may be raw bytes or hex without an `encoding' hint;
%% retry the hex interpretation before failing, matching the previous
%% auto-detection behavior.
retry_as_hex(Raw, ParseError) ->
    case decode_hex(Raw) of
        {ok, Bytes} ->
            case hb_lbry_commitment:transaction_message(Bytes) of
                {ok, _} = Ok -> Ok;
                _ -> ParseError
            end;
        _ ->
            ParseError
    end.

decode_hex(Raw) ->
    case lib_lbry_codec:hex_to_binary(Raw) of
        {ok, Bytes} -> {ok, Bytes};
        _ -> {error, invalid_tx_hex}
    end.

txid_field(Base, Opts) ->
    case hb_maps:get(<<"txid">>, Base, undefined, Opts) of
        TxID when is_binary(TxID) -> hb_util:to_lower(TxID);
        _ -> undefined
    end.

ensure_device(Msg) ->
    Msg#{ <<"device">> => <<"lbry-transaction@1.0">> }.
