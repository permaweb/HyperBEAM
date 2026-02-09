%%% @doc AO-Core device for Arweave tx/block gossip replication.
-module(dev_arweave_gossip).

-export([info/1, info/3, default/4]).
-export([tx/3, block/3, peers/3, gossip_tx/3, gossip_block/3]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(GOSSIP_PREFIX, <<"~arweave-gossip@2.9.5">>).

info(_Opts) ->
    #{
        default => fun default/4
    }.

info(_Base, _Req, _Opts) ->
    {ok,
        #{
            <<"name">> => <<"arweave-gossip@2.9.5">>,
            <<"description">> => <<"Arweave tx/block gossip and replication device">>,
            <<"exports">> =>
                [
                    <<"tx">>,
                    <<"block">>,
                    <<"peers">>,
                    <<"gossip-tx">>,
                    <<"gossip-block">>
                ]
        }
    }.

default(<<"set">>, Base, Req, Opts) ->
    dev_message:set(Base, Req, Opts);
default(<<"keys">>, Base, _Req, _Opts) ->
    dev_message:keys(Base);
default(<<"tx">>, Base, Req, Opts) ->
    tx(Base, Req, Opts);
default(<<"block">>, Base, Req, Opts) ->
    block(Base, Req, Opts);
default(<<"peers">>, Base, Req, Opts) ->
    peers(Base, Req, Opts);
default(<<"gossip-tx">>, Base, Req, Opts) ->
    gossip_tx(Base, Req, Opts);
default(<<"gossip-block">>, Base, Req, Opts) ->
    gossip_block(Base, Req, Opts);
default(_, Base, Req, Opts) ->
    tx(Base, Req, Opts).

tx(Base, Req, Opts) ->
    case method(Base, Req, Opts) of
        <<"POST">> ->
            TX = read_any([<<"tx">>], Base, Req, Req, Opts),
            Type = <<"tx">>,
            {ok, Stored} = store_item(Type, TX, Opts),
            {ok, Stored};
        _ ->
            {ok, list_items(<<"tx">>, Opts)}
    end.

block(Base, Req, Opts) ->
    case method(Base, Req, Opts) of
        <<"POST">> ->
            Block = read_any([<<"block">>], Base, Req, Req, Opts),
            {ok, Stored} = store_item(<<"block">>, Block, Opts),
            maybe_cache_block(Block, Opts),
            {ok, Stored};
        _ ->
            {ok, list_items(<<"block">>, Opts)}
    end.

peers(Base, Req, Opts) ->
    case method(Base, Req, Opts) of
        <<"POST">> ->
            Given = read_any([<<"peers">>], Base, Req, [], Opts),
            {ok, #{<<"peers">> => Given}};
        _ ->
            {ok, #{<<"peers">> => read_peers(Base, Req, Opts)}}
    end.

gossip_tx(Base, Req, Opts) ->
    TX = read_any([<<"tx">>], Base, Req, Req, Opts),
    Results =
        lists:map(
            fun(Peer) ->
                {
                    Peer,
                    hb_http:post(
                        Peer,
                        #{
                            <<"path">> => <<"/~arweave-gossip@2.9.5/tx">>,
                            <<"tx">> => TX
                        },
                        Opts
                    )
                }
            end,
            read_peers(Base, Req, Opts)
        ),
    {ok, #{<<"results">> => Results}}.

gossip_block(Base, Req, Opts) ->
    Block = read_any([<<"block">>], Base, Req, Req, Opts),
    Results =
        lists:map(
            fun(Peer) ->
                {
                    Peer,
                    hb_http:post(
                        Peer,
                        #{
                            <<"path">> => <<"/~arweave-gossip@2.9.5/block">>,
                            <<"block">> => Block
                        },
                        Opts
                    )
                }
            end,
            read_peers(Base, Req, Opts)
        ),
    {ok, #{<<"results">> => Results}}.

store_item(Type, Item, Opts) ->
    {ok, ID} = hb_cache:write(Item, Opts),
    Slot = next_slot(Type, Opts),
    ok = hb_cache:link(ID, slot_path(Type, Slot, Opts), Opts),
    {ok,
        #{
            <<"accepted">> => true,
            <<"id">> => ID,
            <<"slot">> => Slot
        }
    }.

list_items(Type, Opts) ->
    Slots = lists:sort(hb_cache:list_numbered(pool_dir(Type), Opts)),
    Items =
        lists:filtermap(
            fun(Slot) ->
                case hb_cache:read(slot_path(Type, Slot, Opts), Opts) of
                    {ok, Msg} -> {true, Msg};
                    _ -> false
                end
            end,
            Slots
        ),
    #{
        <<"count">> => length(Items),
        <<"items">> => Items
    }.

next_slot(Type, Opts) ->
    case hb_cache:list_numbered(pool_dir(Type), Opts) of
        [] -> 1;
        Slots -> lists:max(Slots) + 1
    end.

pool_dir(Type) ->
    [?GOSSIP_PREFIX, Type].

slot_path(Type, Slot, Opts) ->
    hb_store:path(hb_opts:get(store, no_viable_store, Opts), pool_dir(Type) ++ [hb_util:bin(Slot)]).

maybe_cache_block(Block, Opts) ->
    try
        dev_arweave_block_cache:write(Block, Opts)
    catch
        _:_ -> ok
    end.

read_peers(Base, Req, Opts) ->
    read_any([<<"peers">>, arweave_gossip_peers], Base, Req, hb_opts:get(arweave_gossip_peers, [], Opts), Opts).

method(Base, Req, Opts) ->
    hb_ao:get_first([{Req, <<"method">>}, {Base, <<"method">>}], <<"GET">>, Opts).

read_any(Keys, Base, Req, Default, Opts) ->
    Candidates = [{Req, Key} || Key <- Keys] ++ [{Base, Key} || Key <- Keys],
    hb_ao:get_first(Candidates, Default, Opts).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

tx_pool_roundtrip_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    TX = #{<<"hello">> => <<"world">>},
    {ok, #{<<"accepted">> := true}} =
        tx(#{}, #{<<"method">> => <<"POST">>, <<"tx">> => TX}, Opts),
    {ok, Listed} = tx(#{}, #{<<"method">> => <<"GET">>}, Opts),
    ?assertEqual(1, hb_maps:get(<<"count">>, Listed, 0, #{})).

block_pool_roundtrip_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    Block =
        #{
            <<"height">> => 1,
            <<"indep_hash">> => <<"hash-1">>,
            <<"hash">> => <<"hash-2">>
        },
    {ok, #{<<"accepted">> := true}} =
        block(#{}, #{<<"method">> => <<"POST">>, <<"block">> => Block}, Opts),
    {ok, Listed} = block(#{}, #{<<"method">> => <<"GET">>}, Opts),
    ?assertEqual(1, hb_maps:get(<<"count">>, Listed, 0, #{})).

peers_read_test() ->
    {ok, #{<<"peers">> := [<<"http://example.com">>]}} =
        peers(
            #{},
            #{<<"method">> => <<"GET">>, <<"peers">> => [<<"http://example.com">>]},
            #{}
        ).
