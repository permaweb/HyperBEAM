%%% @doc Logic for handling bundler recocery on node restart.
%%% 
%%% When a bundler is running it will cache the state of each uploaded item
%%% or bundle as it move through the bundling and upload process. If the node
%%% is restarted before it can finish including all uploaded items in a bundle,
%%% or finish seeding all bundles in process, the recovery process will ensure
%%% that the data in process is recovered and resumed.
-module(dev_bundler_recovery).
-export([
    recover_unbundled_items/2,
    recover_bundles/2
]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Spawn a process to recover unbundled items.
recover_unbundled_items(ServerPID, Opts) ->
    spawn(fun() -> do_recover_unbundled_items(ServerPID, Opts) end).

%% @doc Spawn a process to recover in-progress bundles.
recover_bundles(ServerPID, Opts) ->
    spawn(fun() -> do_recover_bundles(ServerPID, Opts) end).

do_recover_unbundled_items(ServerPID, Opts) ->
    try
        ?event(bundler_short, {recover_unbundled_items_start}),
        UnbundledItems = dev_bundler_cache:load_items(
            <<>>,
            Opts,
            fun(ItemID, Item) ->
                ?event(
                    bundler_short,
                    {recovered_unbundled_item,
                        {id, {string, ItemID}}
                    }
                ),
                ServerPID ! {enqueue_item, Item}
            end,
            fun(ItemID) ->
                ?event(
                    bundler_short,
                    {failed_to_recover_unbundled_item,
                        {id, {string, ItemID}}
                    }
                )
            end
        ),
        ?event(bundler_short, {recover_unbundled_items_complete,
            {count, length(UnbundledItems)}}),
        ok
    catch
        _:Error:Stack ->
            ?event(
                error,
                {recover_unbundled_items_failed,
                    {error, Error},
                    {stack, Stack}
                },
                Opts
            )
    end.

do_recover_bundles(ServerPID, Opts) ->
    try
        BundleStates = dev_bundler_cache:load_bundle_states(Opts),
        ?event(bundler_short, {recover_bundles_start,
            {count, length(BundleStates)}}),
        lists:foreach(
            fun({TXID, Status}) ->
                recover_bundle(ServerPID, TXID, Status, Opts)
            end,
            BundleStates
        ),
        ?event(bundler_short, {recover_bundles_complete,
            {count, length(BundleStates)}}),
        ok
    catch
        _:Error:Stack ->
            ?event(
                error,
                {recover_bundles_failed,
                    {error, Error},
                    {stack, Stack}
                },
                Opts
            )
    end.

recover_bundle(ServerPID, TXID, Status, Opts) ->
    ?event(
        bundler_short,
        {recovering_bundle,
            {tx_id, {explicit, TXID}},
            {status, Status}
        }
    ),
    try
        CommittedTX = dev_bundler_cache:load_tx(TXID, Opts),
        case CommittedTX of
            not_found ->
                throw(tx_not_found);
            _ ->
                Items = dev_bundler_cache:load_items(
                    TXID,
                    Opts,
                    fun(ItemID, _Item) ->
                        ?event(
                            debug_bundler,
                            {loaded_bundle_item,
                                {tx_id, {explicit, TXID}},
                                {item_id, {explicit, ItemID}}
                            }
                        )
                    end,
                    fun(ItemID) ->
                        ?event(
                            error,
                            {failed_to_load_bundle_item,
                                {tx_id, {explicit, TXID}},
                                {item_id, {explicit, ItemID}}
                            },
                            Opts
                        ),
                        throw({failed_to_load_bundle_item, ItemID})
                    end
                ),
                ServerPID ! {recover_bundle, CommittedTX, Items}
        end
    catch
        _:Error:Stack ->
            ?event(
                error,
                {failed_to_recover_bundle,
                    {tx_id, {explicit, TXID}},
                    {error, Error},
                    {stack, Stack}
                },
                Opts
            )
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

recover_unbundled_items_test() ->
    Opts = #{<<"store">> => hb_test_utils:test_store()},
    Item1 = new_data_item(1, 10, Opts),
    Item2 = new_data_item(2, 10, Opts),
    Item3 = new_data_item(3, 10, Opts),
    ok = dev_bundler_cache:write_item(Item1, Opts),
    ok = dev_bundler_cache:write_item(Item2, Opts),
    ok = dev_bundler_cache:write_item(Item3, Opts),
    FakeTX = new_bundle_tx([Item2], Opts),
    ok = dev_bundler_cache:write_tx(FakeTX, [Item2], Opts),
    recover_unbundled_items(self(), Opts),
    RecoveredItems = receive_enqueue_items(2),
    RecoveredItems1 = normalize_items(RecoveredItems, Opts),
    ?assertEqual(
        lists:sort([Item1, Item3]),
        lists:sort(RecoveredItems1)
    ).

recover_bundles_skips_complete_test() ->
    Opts = #{<<"store">> => hb_test_utils:test_store()},
    Item1 = new_data_item(1, 10, Opts),
    Item2 = new_data_item(2, 10, Opts),
    Item3 = new_data_item(3, 10, Opts),
    ok = dev_bundler_cache:write_item(Item1, Opts),
    ok = dev_bundler_cache:write_item(Item2, Opts),
    ok = dev_bundler_cache:write_item(Item3, Opts),
    PostedTX = new_bundle_tx([Item1, Item2], Opts),
    CompletedTX = new_bundle_tx([Item3], Opts),
    ok = dev_bundler_cache:write_tx(PostedTX, [Item1, Item2], Opts),
    ok = dev_bundler_cache:write_tx(CompletedTX, [Item3], Opts),
    ok = dev_bundler_cache:complete_tx(CompletedTX, Opts),
    recover_bundles(self(), Opts),
    {RecoveredTX, RecoveredItems} = receive_recovered_bundle(),
    RecoveredItems1 = normalize_items(RecoveredItems, Opts),
    ?assertEqual(
        hb_message:id(PostedTX, signed, Opts),
        hb_message:id(RecoveredTX, signed, Opts)
    ),
    ?assertEqual(
        lists:sort([Item1, Item2]),
        lists:sort(RecoveredItems1)
    ),
    receive
        {recover_bundle, _, _} ->
            erlang:error(unexpected_second_recovered_bundle)
    after 200 ->
        ok
    end.

recover_bundles_failed_bundle_items_continue_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"debug-print">> => false
    },
    ValidItem = new_data_item(1, 10, Opts),
    ok = dev_bundler_cache:write_item(ValidItem, Opts),
    ValidTX = new_bundle_tx([ValidItem], Opts),
    ok = dev_bundler_cache:write_tx(ValidTX, [ValidItem], Opts),
    BrokenTX = new_bundle_tx([], Opts),
    ok = dev_bundler_cache:write_tx(BrokenTX, [], Opts),
    MissingItemID = <<"missing-item">>,
    ok = write_missing_item_bundle(MissingItemID, BrokenTX, Opts),
    recover_bundles(self(), Opts),
    {RecoveredTX, RecoveredItems} = receive_recovered_bundle(),
    RecoveredItems1 = normalize_items(RecoveredItems, Opts),
    ?assertEqual(
        hb_message:id(ValidTX, signed, Opts),
        hb_message:id(RecoveredTX, signed, Opts)
    ),
    ?assertEqual([ValidItem], RecoveredItems1),
    receive
        {recover_bundle, _, _} ->
            erlang:error(unexpected_broken_bundle_recovered)
    after 200 ->
        ok
    end.

receive_enqueue_items(Count) ->
    receive_enqueue_items(Count, []).

receive_enqueue_items(0, Items) ->
    lists:reverse(Items);
receive_enqueue_items(Count, Items) ->
    receive
        {enqueue_item, Item} ->
            receive_enqueue_items(Count - 1, [Item | Items])
    after 1000 ->
        erlang:error({missing_enqueue_items, Count})
    end.

receive_recovered_bundle() ->
    receive
        {recover_bundle, CommittedTX, Items} ->
            {CommittedTX, Items}
    after 1000 ->
        erlang:error(missing_recovered_bundle)
    end.

normalize_items(Items, Opts) ->
    [
        hb_message:with_commitments(
            #{ <<"commitment-device">> => <<"ans104@1.0">> },
            Item,
            Opts
        )
        || Item <- Items
    ].

write_missing_item_bundle(ItemID, TX, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    Path = hb_path:to_binary([
        <<"~bundler@1.0">>,
        <<"item">>,
        ItemID,
        <<"bundle">>
    ]),
    hb_store:write(Store, #{ Path => hb_message:id(TX, signed, Opts) }, Opts).

new_data_item(Index, Size, Opts) ->
    Tag = <<"tag", (integer_to_binary(Index))/binary>>,
    Value = <<"value", (integer_to_binary(Index))/binary>>,
    Item = ar_bundles:sign_item(
        #tx{
            data = rand:bytes(Size),
            tags = [{Tag, Value}]
        },
        hb:wallet()
    ),
    hb_message:convert(Item, <<"structured@1.0">>, <<"ans104@1.0">>, Opts).

new_bundle_tx(Items, Opts) ->
    TX = dev_bundler_task:data_items_to_tx(lists:reverse(Items), Opts),
    hb_message:convert(TX, <<"structured@1.0">>, <<"tx@1.0">>, Opts).
