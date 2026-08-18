%%% @doc Decode Arweave peer response bodies for the sync application.
%%%
%%% The consensus devices operate only on AO-Core messages. This module is the
%%% boundary that decodes the peer wire formats sync consumes; the owning
%%% semantic libraries retain conversions between consensus values and
%%% AO-Core messages.
-module(lib_arweave_sync_codec).
-export([decode_block/2, decode_history/4, decode_transaction/2,
    decode_wallet_page/1]).

%% @doc Decode an Arweave binary block into its canonical AO-Core message.
decode_block(Body, Opts) ->
    case ar_serialize:binary_to_block(Body) of
        {ok, Block} -> {ok, lib_arweave_block:from(Block, Opts)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Decode a carried history and persist its bounded AO-Core structure.
decode_history(Kind, Body, Height, Opts) ->
    case history_values(Kind, Body) of
        {ok, Values} ->
            {ok,
                lib_arweave_history:from_values(
                    Kind, Values, Height, Opts
                )
            };
        {error, _Reason} ->
            {error, << "invalid-", Kind/binary >>}
    end.

%% @doc Decode an Arweave JSON transaction into its committed AO-Core message.
decode_transaction(Body, Opts) ->
    try
        {ok,
            lib_arweave_tx:from_tx(
                ar_tx:json_struct_to_tx(hb_json:decode(Body)),
                Opts
            )
        }
    catch
        _Class:_Reason ->
            {error, <<"invalid-transaction-encoding">>}
    end.

%% @doc Decode one `GET /wallet_list/<root>[/<cursor>]' response body.
decode_wallet_page(<<131, 80, _/binary>>) ->
    % The compressed external term format declares its inflated size in a
    % header that `binary_to_term/2' allocates against before inspecting the
    % body. Legitimate Arweave wallet pages use plain external term format.
    {error, <<"invalid-wallet-list-page">>};
decode_wallet_page(Body) ->
    try binary_to_term(Body, [safe]) of
        #{ next_cursor := Cursor, wallets := Wallets } when is_list(Wallets) ->
            decode_wallet_page(Cursor, Wallets);
        _ ->
            {error, <<"invalid-wallet-list-page">>}
    catch
        error:badarg ->
            {error, <<"invalid-wallet-list-page">>}
    end.

%% @doc Validate a decoded wallet page and its continuation cursor.
decode_wallet_page(Cursor, Wallets) when is_binary(Cursor); Cursor == last ->
    case lists:all(fun is_valid_account/1, Wallets) of
        true -> {ok, Cursor, Wallets};
        false -> {error, <<"invalid-account">>}
    end;
decode_wallet_page(_Cursor, _Wallets) ->
    {error, <<"invalid-wallet-list-cursor">>}.

%% @doc Return whether a decoded term is a canonical Arweave account entry.
is_valid_account({Addr, {Balance, LastTX}}) ->
    is_binary(Addr) andalso is_binary(LastTX)
        andalso is_integer(Balance) andalso Balance >= 0;
is_valid_account({Addr, {Balance, LastTX, Denomination, MiningPermission}}) ->
    is_binary(Addr) andalso is_binary(LastTX)
        andalso is_integer(Balance) andalso Balance >= 0
        andalso is_integer(Denomination) andalso Denomination > 0
        andalso is_boolean(MiningPermission);
is_valid_account(_) ->
    false.

%% @doc Decode a peer history into the newest-first values consensus consumes.
history_values(<<"reward-history">>, Body) ->
    ar_serialize:binary_to_reward_history(Body);
history_values(<<"block-time-history">>, Body) ->
    ar_serialize:binary_to_block_time_history(Body).
