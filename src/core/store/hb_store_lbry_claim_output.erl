%%% @doc Read-only LBRY claim-family source store.
%%%
%%% Immutable outpoints remain the native source boundary. The legacy
%%% `claim-output' view still returns the normalized
%%% `~lbry-claim-output@1.0' proof object. The broader native claim-family
%%% views return `~lbry-claim@1.0', `~lbry-channel@1.0', or
%%% `~lbry-stream@1.0' evidence messages synthesized from the raw
%%% transaction bytes at the requested outpoint.
-module(hb_store_lbry_claim_output).
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([read/3, type/3, resolve/3, list/3]).
-export([write/3, group/3, link/3]).

-define(TXID_HEX_SIZE, 64).

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

resolve(StoreOpts, #{ <<"resolve">> := Key }, NodeOpts) ->
    case parse_key(StoreOpts, Key, NodeOpts) of
        {ok, TxID, NOut, Kind} -> {ok, canonical_path(TxID, NOut, Kind)};
        Error -> Error
    end.

type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, Msg} when is_map(Msg) -> {ok, composite};
        {ok, _Bin} -> {ok, simple};
        Error -> Error
    end.

read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    case parse_key(StoreOpts, Key, NodeOpts) of
        {ok, TxID, NOut, <<"claim-output">>} ->
            hb_store_odysee:read(
                StoreOpts,
                #{ <<"read">> => claim_output_path(TxID, NOut) },
                NodeOpts
            );
        {ok, TxID, NOut, Kind} ->
            read_native(StoreOpts, TxID, NOut, Kind, NodeOpts);
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

read_native(StoreOpts, TxID, NOut, Kind, NodeOpts) ->
    maybe
        {ok, TxMsg} =
            hb_store_lbry_transaction:read(
                StoreOpts,
                #{ <<"read">> => TxID },
                NodeOpts
            ),
        {ok, Raw} ?= transaction_raw(TxMsg, NodeOpts),
        case Kind of
            <<"claim">> -> hb_lbry_commitment:claim_output_message(Raw, NOut);
            <<"channel">> -> hb_lbry_commitment:channel_output_message(Raw, NOut);
            <<"stream">> -> hb_lbry_commitment:stream_claim_message(Raw, NOut)
        end
    end.

transaction_raw(TxMsg, Opts) ->
    case hb_maps:get(<<"body">>, TxMsg, undefined, Opts) of
        Raw when is_binary(Raw) -> {ok, Raw};
        _ ->
            case hb_maps:get(<<"raw">>, TxMsg, undefined, Opts) of
                Raw when is_binary(Raw) -> {ok, Raw};
                _ -> {error, raw_transaction_not_found}
            end
    end.

parse_key(StoreOpts, Key0, NodeOpts) ->
    Key = normalize_key(Key0),
    case Key of
        <<"lbry/claim-output/", Rest/binary>> ->
            parse_rest(Rest, <<"claim-output">>);
        <<"lbry/claim-proof/", Rest/binary>> ->
            parse_rest(Rest, <<"claim-output">>);
        <<"odysee/claim-proof/", Rest/binary>> ->
            parse_rest(Rest, <<"claim-output">>);
        <<"lbry/claim/", Rest/binary>> ->
            parse_rest(Rest, <<"claim">>);
        <<"lbry/channel/", Rest/binary>> ->
            parse_rest(Rest, <<"channel">>);
        <<"lbry/stream/", Rest/binary>> ->
            parse_rest(Rest, <<"stream">>);
        Rest ->
            parse_rest(Rest, store_kind(StoreOpts, NodeOpts))
    end.

parse_rest(Rest, Kind) ->
    case binary:split(Rest, <<"/">>) of
        [TxID, NOut] -> parse_outpoint(TxID, NOut, Kind);
        _ ->
            case binary:split(Rest, <<":">>) of
                [TxID, NOut] -> parse_outpoint(TxID, NOut, Kind);
                _ -> {error, not_found}
            end
    end.

parse_outpoint(TxID0, NOut0, Kind) ->
    TxID = normalize_hex(TxID0),
    case {byte_size(TxID), nout(NOut0), valid_kind(Kind)} of
        {?TXID_HEX_SIZE, {ok, NOut}, true} ->
            {ok, TxID, binary_to_integer(NOut), Kind};
        {?TXID_HEX_SIZE, Error, _} ->
            Error;
        _ ->
            {error, invalid_txid}
    end.

store_kind(StoreOpts, NodeOpts) ->
    case hb_maps:get(<<"kind">>, StoreOpts, <<"claim-output">>, NodeOpts) of
        <<"claim">> -> <<"claim">>;
        <<"channel">> -> <<"channel">>;
        <<"stream">> -> <<"stream">>;
        <<"claim-proof">> -> <<"claim-output">>;
        <<"claim-output">> -> <<"claim-output">>;
        _ -> <<"claim-output">>
    end.

valid_kind(<<"claim-output">>) -> true;
valid_kind(<<"claim">>) -> true;
valid_kind(<<"channel">>) -> true;
valid_kind(<<"stream">>) -> true;
valid_kind(_) -> false.

canonical_path(TxID, NOut, <<"claim-output">>) ->
    <<"lbry/claim-output/", TxID/binary, "/", (integer_to_binary(NOut))/binary>>;
canonical_path(TxID, NOut, <<"claim">>) ->
    <<"lbry/claim/", TxID/binary, "/", (integer_to_binary(NOut))/binary>>;
canonical_path(TxID, NOut, <<"channel">>) ->
    <<"lbry/channel/", TxID/binary, "/", (integer_to_binary(NOut))/binary>>;
canonical_path(TxID, NOut, <<"stream">>) ->
    <<"lbry/stream/", TxID/binary, "/", (integer_to_binary(NOut))/binary>>.

claim_output_path(TxID, NOut) ->
    <<"odysee/claim-proof/", TxID/binary, "/", (integer_to_binary(NOut))/binary>>.

nout(NOut) when is_binary(NOut) ->
    try
        Int = binary_to_integer(NOut),
        case Int >= 0 of
            true -> {ok, integer_to_binary(Int)};
            false -> {error, invalid_nout}
        end
    catch _:_ ->
        {error, invalid_nout}
    end;
nout(NOut) when is_integer(NOut), NOut >= 0 ->
    {ok, integer_to_binary(NOut)};
nout(_NOut) ->
    {error, invalid_nout}.

normalize_key(<<"/", Rest/binary>>) ->
    normalize_key(Rest);
normalize_key(Key) when is_binary(Key) ->
    Key;
normalize_key(Key) ->
    hb_path:to_binary(Key).

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:bin(string:lowercase(binary_to_list(Hex))).
