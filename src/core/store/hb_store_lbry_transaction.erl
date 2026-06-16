%%% @doc Read-only LBRY transaction source store.
%%%
%%% The native transaction key is the 64-character display-order txid. Reads are
%%% delegated to `hb_store_odysee' fetch mechanics and returned as
%%% `~lbry-transaction@1.0' source-committed messages.
-module(hb_store_lbry_transaction).
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([read/3, type/3, resolve/3, list/3]).
-export([write/3, group/3, link/3]).
-include_lib("eunit/include/eunit.hrl").

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

resolve(_StoreOpts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    case transaction_path(Key) of
        {ok, Path} -> {ok, Path};
        Error -> Error
    end.

type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, Msg} when is_map(Msg) -> {ok, composite};
        {ok, _Bin} -> {ok, simple};
        Error -> Error
    end.

read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    case transaction_path(Key) of
        {ok, Path} ->
            hb_store_odysee:read(StoreOpts, #{ <<"read">> => Path }, NodeOpts);
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

transaction_path(Key0) ->
    Key = normalize_key(Key0),
    case Key of
        <<"lbry/transaction/", TxID/binary>> -> txid_path(TxID);
        <<"lbry/tx/", TxID/binary>> -> txid_path(TxID);
        <<"odysee/transaction/", _TxID/binary>> -> {ok, Key};
        <<"odysee/tx/", TxID/binary>> -> txid_path(TxID);
        TxID when byte_size(TxID) =:= ?TXID_HEX_SIZE -> txid_path(TxID);
        _ -> {error, not_found}
    end.

txid_path(TxID0) ->
    TxID = normalize_hex(TxID0),
    case byte_size(TxID) of
        ?TXID_HEX_SIZE -> {ok, <<"odysee/transaction/", TxID/binary>>};
        _ -> {error, invalid_txid}
    end.

normalize_key(<<"/", Rest/binary>>) ->
    normalize_key(Rest);
normalize_key(Key) when is_binary(Key) ->
    Key;
normalize_key(Key) ->
    hb_path:to_binary(Key).

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:bin(string:lowercase(binary_to_list(Hex))).

-ifdef(TEST).

read_native_txid_fixture_test() ->
    Raw = test_tx_raw(),
    TxID = hb_lbry_tx:txid(Raw),
    Store = #{
        <<"store-module">> => hb_store_lbry_transaction,
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
    {ok, Msg} = hb_store:read(Store, TxID, #{}),
    ?assertEqual(<<"lbry-transaction@1.0">>, hb_maps:get(<<"device">>, Msg, #{})),
    ?assert(lists:member(
        <<"lbry-transaction@1.0">>,
        hb_message:commitment_devices(Msg, #{})
    )),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})).

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
                    =:= <<"lbry-transaction@1.0">>
            ]
    }.

-endif.
