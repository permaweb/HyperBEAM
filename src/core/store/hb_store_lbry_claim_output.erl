%%% @doc A read-only store sourcing immutable LBRY claim-output evidence by
%%% display-order outpoint (`txid:nout'). The raw transaction is fetched and
%%% txid-verified through `hb_store_lbry_transaction', then the requested
%%% output is parsed into a claim-output message with a native
%%% `lbry-claim@1.0' commitment. With the `kind' store option set to
%%% `channel', the output must be a channel claim and the returned message
%%% carries the normalized channel public key under a `lbry-channel@1.0'
%%% commitment instead. With `kind' set to `stream', the output must be a
%%% stream claim and the returned message additionally carries the
%%% descriptor `sd_hash' under a `lbry-stream@1.0' commitment.
%%%
%%% Claim-ID and name lookup is a locator concern and deliberately not part
%%% of this store: outpoints are immutable evidence, claim IDs are not
%%% provably current without a ClaimTrie proof.
-module(hb_store_lbry_claim_output).
-export([scope/0, scope/1, type/3, read/3, resolve/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

scope() -> remote.
scope(_) -> scope().

resolve(_StoreOpts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    case parse_outpoint(Key) of
        {ok, TxID, Nout} ->
            {ok, <<TxID/binary, ":", (integer_to_binary(Nout))/binary>>};
        _ ->
            {error, not_found}
    end.

type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, _} -> {ok, simple};
        Error -> Error
    end.

read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    case parse_outpoint(Key) of
        {ok, TxID, Nout} ->
            Result = fetch_output(StoreOpts, TxID, Nout, NodeOpts),
            ?event(lbry_claim_output,
                {claim_output_read,
                    {txid, TxID},
                    {nout, Nout},
                    {kind, kind(StoreOpts, NodeOpts)},
                    {result, result_class(Result)}},
                NodeOpts
            ),
            Result;
        _ ->
            {error, not_found}
    end.

fetch_output(StoreOpts, TxID, Nout, NodeOpts) ->
    maybe
        {ok, TxMsg} ?=
            hb_store_lbry_transaction:read(
                StoreOpts,
                #{ <<"read">> => TxID },
                NodeOpts
            ),
        Raw = maps:get(<<"raw">>, TxMsg),
        case kind(StoreOpts, NodeOpts) of
            <<"channel">> -> hb_lbry_commitment:channel_output_message(Raw, Nout);
            <<"stream">> -> hb_lbry_commitment:stream_claim_message(Raw, Nout);
            _ -> hb_lbry_commitment:claim_output_message(Raw, Nout)
        end
    end.

kind(StoreOpts, NodeOpts) ->
    hb_maps:get(<<"kind">>, StoreOpts, <<"claim">>, NodeOpts).

parse_outpoint(Key) when is_binary(Key) ->
    case binary:split(Key, <<":">>) of
        [TxID, NoutBin] ->
            case {valid_txid(TxID), parse_nout(NoutBin)} of
                {true, {ok, Nout}} -> {ok, hb_util:to_lower(TxID), Nout};
                _ -> {error, invalid_outpoint}
            end;
        _ ->
            {error, invalid_outpoint}
    end;
parse_outpoint(_) ->
    {error, invalid_outpoint}.

parse_nout(NoutBin) ->
    try binary_to_integer(NoutBin) of
        Nout when Nout >= 0 -> {ok, Nout};
        _ -> {error, invalid_nout}
    catch
        _:_ -> {error, invalid_nout}
    end.

valid_txid(TxID) when is_binary(TxID), byte_size(TxID) == 64 ->
    try binary:decode_hex(TxID) of
        Decoded -> byte_size(Decoded) == 32
    catch
        _:_ -> false
    end;
valid_txid(_) ->
    false.

result_class({ok, _}) -> ok;
result_class({error, _}) -> error;
result_class({failure, _}) -> failure.

%%% Tests

read_returns_committed_claim_output_test() ->
    application:ensure_all_started(inets),
    TxID = <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
    {ok, Server, Handle} = proxy_server(hb_lbry_tx:task0_tx_hex()),
    try
        Store = store(Server),
        {ok, Msg} =
            read(
                Store,
                #{ <<"read">> => <<TxID/binary, ":0">> },
                #{ <<"http-client">> => httpc }
            ),
        ?assertEqual(<<"lbry-claim@1.0">>, maps:get(<<"device">>, Msg)),
        ?assertEqual(
            <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
            maps:get(<<"claim-id">>, Msg)
        ),
        ?assertEqual(<<"create">>, maps:get(<<"claim-op">>, Msg)),
        ?assertEqual(TxID, maps:get(<<"txid">>, Msg)),
        ?assertEqual(0, maps:get(<<"nout">>, Msg)),
        ?assertEqual(
            true,
            hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
        )
    after
        hb_mock_server:stop(Handle)
    end.

read_rejects_non_claim_output_test() ->
    application:ensure_all_started(inets),
    TxID = <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
    {ok, Server, Handle} = proxy_server(hb_lbry_tx:task0_tx_hex()),
    try
        Store = store(Server),
        ?assertEqual(
            {error, missing_claim_output},
            read(
                Store,
                #{ <<"read">> => <<TxID/binary, ":1">> },
                #{ <<"http-client">> => httpc }
            )
        )
    after
        hb_mock_server:stop(Handle)
    end.

read_rejects_channel_kind_for_stream_claim_test() ->
    application:ensure_all_started(inets),
    TxID = <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
    {ok, Server, Handle} = proxy_server(hb_lbry_tx:task0_tx_hex()),
    try
        Store = (store(Server))#{ <<"kind">> => <<"channel">> },
        ?assertEqual(
            {error, {missing_field, 2}},
            read(
                Store,
                #{ <<"read">> => <<TxID/binary, ":0">> },
                #{ <<"http-client">> => httpc }
            )
        )
    after
        hb_mock_server:stop(Handle)
    end.

read_rejects_invalid_outpoint_test() ->
    ?assertEqual(
        {error, not_found},
        read(#{}, #{ <<"read">> => <<"not-an-outpoint">> }, #{})
    ),
    ?assertEqual(
        {error, not_found},
        read(#{}, #{ <<"read">> => <<"aabb:0">> }, #{})
    ).

proxy_server(Hex) ->
    Response =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ <<"hex">> => Hex },
            <<"id">> => 1
        }),
    hb_mock_server:start([{"/api/v1/proxy", proxy, {200, Response}}]).

store(Server) ->
    #{
        <<"store-module">> => ?MODULE,
        <<"lbry-proxy-node">> => Server,
        <<"http-client">> => httpc
    }.
