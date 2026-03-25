%%% @doc A module that performs caching operations for Arweave transaction
%%% headers, storing them under a pseudo-path without linking the signed TXID.
%%% This avoids colliding with full TX cache entries that may share the same
%%% commitment ID.
-module(dev_arweave_tx_cache).
-export([read/2, write/2]).
-export([path/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The pseudo-path prefix which the Arweave TX cache should use.
-define(ARWEAVE_TX_CACHE_PREFIX, <<"~arweave@2.9">>).

%% @doc Read a TX header from the cache.
read(TXID, Opts) when ?IS_ID(TXID) ->
    Store = cache_store(Opts),
    Res =
        case hb_store:read(Store, path(TXID, Opts)) of
            {ok, Bin} when is_binary(Bin) ->
                case catch binary_to_term(Bin, [safe]) of
                    Msg when is_map(Msg) -> {ok, Msg};
                    _ -> not_found
                end;
            Other ->
                Other
        end,
    ?event(arweave_cache, {read_tx_header, {id, {explicit, TXID}}, {result, Res}}),
    Res;
read(_, _) ->
    not_found.

%% @doc Return the path of a TX header that will be used in the cache.
path(TXID, Opts) when ?IS_ID(TXID) ->
    hb_store:path(cache_store(Opts), [
        ?ARWEAVE_TX_CACHE_PREFIX,
        <<"tx">>,
        <<"header">>,
        hb_util:native_id(TXID)
    ]).

%% @doc Write a TX header to the cache.
write(TXHeader, Opts) when is_map(TXHeader) ->
    Store = cache_store(Opts),
    TXID = hb_message:id(TXHeader, signed, Opts),
    Path = path(TXID, Opts),
    Res = hb_store:write(Store, Path, term_to_binary(TXHeader)),
    ?event(
        arweave_cache,
        {wrote_tx_header,
            {id, {explicit, TXID}},
            {path, Path},
            {result, Res}
        }
    ),
    Res.

%% @doc Determine the store to use for the TX header cache.
cache_store(#{ <<"index-store">> := Store }) ->
    Store;
cache_store(Opts) ->
    case hb_store_arweave:store_from_opts(Opts) of
        no_store ->
            hb_opts:get(store, no_viable_store, Opts);
        #{ <<"index-store">> := Store } ->
            Store;
        _ ->
            hb_opts:get(store, no_viable_store, Opts)
    end.

%%% Tests

write_read_test() ->
    Store = [hb_test_utils:test_store()],
    Opts = #{
        store => Store,
        priv_wallet => hb:wallet()
    },
    Header = test_tx_header(Opts),
    TXID = hb_message:id(Header, signed, Opts),
    ok = write(Header, Opts),
    ?assertEqual({ok, Header}, read(TXID, Opts)).

test_tx_header(Opts) ->
    Msg =
        hb_message:commit(
            #{
                <<"content-type">> => <<"text/plain">>,
                <<"data">> => <<"test-data">>,
                <<"test-key">> => <<"test-value">>
            },
            Opts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    TX = hb_message:convert(Msg, <<"tx@1.0">>, <<"structured@1.0">>, Opts),
    hb_message:convert(
        TX#tx{ data = <<>> },
        <<"structured@1.0">>,
        <<"tx@1.0">>,
        Opts
    ).
