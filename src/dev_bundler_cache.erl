%%% @doc Cache management for the bundler device. This module handles caching
%%% of data items and bundle state for crash recovery.
%%%
%%% Pseudopath structure:
%%%   ~bundler@1.0/item/{DataItemID}/bundle -> TXID | <<>>
%%%   ~bundler@1.0/tx/{TXID}/status -> <<"posted">> | <<"complete">>
%%%
%%% Recovery flow:
%%%   1. Load unbundled items (where bundle = <<>>) back into dev_bundler queue
%%%   2. Load TX states and reconstruct dev_bundler_dispatch bundles
%%%   3. Enqueue appropriate tasks based on status
-module(dev_bundler_cache).
-export([
    write_item/2,
    write_tx/3,
    complete_tx/2,
    load_unbundled_items/1,
    load_bundle_states/1,
    load_bundled_items/2,
    load_tx/2
]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BUNDLER_PREFIX, <<"~bundler@1.0">>).

%%% Data Item operations

item_id(Item, Opts) when is_map(Item) ->
    hb_message:id(Item, signed, Opts).

%% @doc Write a data item to cache and create its bundler pseudopath.
write_item(Item, Opts) when is_map(Item) ->
    % Write the actual item to cache
    {ok, _} = hb_cache:write(Item, Opts),
    % Use the committed (structured) item for path generation
    Path = item_path(Item, Opts),
    % Create pseudopath with empty bundle reference
    write_pseudopath(Path, <<>>, Opts).

%% @doc Link a data item to a bundle TX.
link_item_to_tx(Item, TX, Opts) when is_map(Item) and is_map(TX) ->
    Path = item_path(Item, Opts),
    TXID = tx_id(TX, Opts),
    write_pseudopath(Path, TXID, Opts).

%% @doc Get the bundle TXID for a data item, or <<>> if not bundled.
get_item_bundle(Item, Opts) when is_map(Item) ->
    Path = item_path(Item, Opts),
    case read_pseudopath(Path, Opts) of
        {ok, Value} -> Value;
        not_found -> not_found
    end.

%% @doc Construct the pseudopath for an item's bundle reference.
%% Item should be a structured message.
item_path(Item, Opts) when is_map(Item) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:path(Store, [
        ?BUNDLER_PREFIX,
        <<"item">>,
        item_id(Item, Opts),
        <<"bundle">>
    ]).

%%% TX/Bundle operations

tx_id(TX, _Opts) when is_binary(TX) ->
    TX;
tx_id(TX, Opts) when is_map(TX) ->
    hb_message:id(TX, signed, Opts).

write_tx(TX, Items, Opts) when is_map(TX) ->
    {ok, _} = hb_cache:write(TX, Opts),
    set_tx_status(TX, <<"posted">>, Opts),
    lists:foreach(
        fun(Item) ->
            ok = link_item_to_tx(Item, TX, Opts)
        end,
        Items
    ).

complete_tx(TX, Opts) ->
    set_tx_status(TX, <<"complete">>, Opts).

%% @doc Set the status of a bundle TX.
set_tx_status(TX, Status, Opts) ->
    Path = tx_path(TX, Opts),
    ?event(bundler_debug, {set_tx_status, {path, Path}, {status, Status}}),
    write_pseudopath(Path, Status, Opts).

%% @doc Get the status of a bundle TX.
get_tx_status(TX, Opts) ->
    Path = tx_path(TX, Opts),
    case read_pseudopath(Path, Opts) of
        {ok, Value} -> Value;
        not_found -> not_found
    end.

%% @doc Construct the pseudopath for a TX's status.
%% TXID should already be encoded (base64 string).
tx_path(TX, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:path(Store, [
        ?BUNDLER_PREFIX,
        <<"tx">>,
        tx_id(TX, Opts),
        <<"status">>
    ]).

%%% Recovery operations

%% @doc Load all unbundled items (where bundle = <<>>) from cache.
%% Returns list of actual Item messages for re-queuing.
load_unbundled_items(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ItemsPath = hb_store:path(Store, [?BUNDLER_PREFIX, <<"item">>]),
    % List all item IDs
    ItemIDs = case hb_cache:list(ItemsPath, Opts) of
        [] -> [];
        List -> List
    end,
    ?event(bundler_short,
        {recovering_all_unbundled_items, length(ItemIDs)}
    ),
    % Filter for unbundled items and load them
    lists:filtermap(
        fun(ItemIDStr) ->
            % Read the bundle pseudopath directly
            BundlePath = hb_store:path(Store, [
                ?BUNDLER_PREFIX,
                <<"item">>,
                ItemIDStr,
                <<"bundle">>
            ]),
            case read_pseudopath(BundlePath, Opts) of
                {ok, <<>>} ->
                    % Unbundled item - load it fully (resolve all links)
                    case hb_cache:read(ItemIDStr, Opts) of
                        {ok, Item} ->
                            FullyLoadedItem = hb_cache:ensure_all_loaded(Item, Opts),
                            ?event(bundler_short,
                                {recovered_unbundled_item,
                                    {id, {string, ItemIDStr}}
                                }
                            ),
                            {true, FullyLoadedItem};
                        _ ->
                            ?event(bundler_short,
                                {failed_to_recover_unbundled_item,
                                    {id, {string, ItemIDStr}}
                                }
                            ),
                            false
                    end;
                _ ->
                    % Already bundled or not found
                    false
            end
        end,
        ItemIDs
    ).

%% @doc Load all bundle TX states from cache.
%% Returns list of {TXID, Status} tuples.
load_bundle_states(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    TXRootPath = hb_store:path(Store, [?BUNDLER_PREFIX, <<"tx">>]),
    % List all TX IDs
    TXIDs = case hb_cache:list(TXRootPath, Opts) of
        [] -> [];
        List -> List
    end,
    % Load status for each TX
    lists:filtermap(
        fun(TXID) ->
            % TXIDStr is already the base64-encoded ID we need
            case get_tx_status(TXID, Opts) of
                not_found -> false;
                <<>> -> false; % Empty status, ignore
                <<"complete">> -> false; % Skip completed bundles
                Status ->
                    ?event(
                        bundler_debug,
                        {loaded_tx_state,
                            {id, {string, TXID}},
                            {status, Status}
                        }
                    ),
                    {true, {TXID, Status}}
            end
        end,
        TXIDs
    ).

%% @doc Load all data items associated with a bundle TX.
%% Uses the item pseudopaths to find items with matching tx-id.
load_bundled_items(TXID, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ItemsPath = hb_store:path(Store, [?BUNDLER_PREFIX, <<"item">>]),
    % List all item IDs
    ItemIDs = case hb_cache:list(ItemsPath, Opts) of
        [] -> [];
        List -> List
    end,
    ?event(bundler_short, {recovering_bundled_items,
        {count, length(ItemIDs)}}),
    % Filter for items belonging to this TX and load them
    lists:filtermap(
        fun(ItemIDStr) ->
            % Read the bundle pseudopath directly
            BundlePath = hb_store:path(Store, [
                ?BUNDLER_PREFIX,
                <<"item">>,
                ItemIDStr,
                <<"bundle">>
            ]),
            case read_pseudopath(BundlePath, Opts) of
                {ok, BundleTXID} when BundleTXID =:= TXID ->
                    % This item belongs to our bundle - load it fully (resolve all links)
                    case hb_cache:read(ItemIDStr, Opts) of
                        {ok, Item} ->
                            FullyLoadedItem = hb_cache:ensure_all_loaded(Item, Opts),
                            ?event(
                                bundler_debug,
                                {loaded_tx_item,
                                    {tx_id, {explicit, TXID}},
                                    {item_id, {explicit, ItemIDStr}}
                                }
                            ),
                            {true, FullyLoadedItem};
                        _ ->
                            ?event(
                                error,
                                {failed_to_load_tx_item,
                                    {tx_id, {explicit, TXID}},
                                    {item_id, {explicit, ItemIDStr}}
                                }
                            ),
                            false
                    end;
                _ ->
                    % Doesn't belong to this bundle or not found
                    false
            end
        end,
        ItemIDs
    ).

%% @doc Load a TX from cache by its ID.
load_tx(TXID, Opts) ->
    ?event(bundler_debug, {load_tx, {tx_id, {explicit, TXID}}}),
    case hb_cache:read(TXID, Opts) of
        {ok, TX} ->
            ?event(bundler_debug, {loaded_tx, {tx_id, {explicit, TXID}}}),
            hb_cache:ensure_all_loaded(TX, Opts);
        _ ->
            ?event(error, {failed_to_load_tx, {tx_id, {explicit, TXID}}}),
            not_found
    end.


%%% Helper functions

%% @doc Write a value to a pseudopath.
write_pseudopath(Path, Value, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:write(Store, Path, Value).

%% @doc Read a value from a pseudopath.
read_pseudopath(Path, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    case hb_store:read(Store, Path) of
        {ok, Value} -> {ok, Value};
        _ -> not_found
    end.

%%% Tests

basic_cache_test() ->
    Opts = #{store => hb_test_utils:test_store()},
    Item = new_data_item(1, 10, Opts),
    ok = write_item(Item, Opts),
    ItemID = item_id(Item, Opts),
    ?assertEqual(<<>>, get_item_bundle(Item, Opts)),
    TX = new_tx(1, Opts),
    ok = write_tx(TX, [Item], Opts),
    TXID = tx_id(TX, Opts),
    ?assertEqual(TXID, get_item_bundle(Item, Opts)),
    ?assertEqual(<<"posted">>, get_tx_status(TX, Opts)),
    ok = complete_tx(TX, Opts),
    ?assertEqual(<<"complete">>, get_tx_status(TX, Opts)),
    ?assertEqual(TX, read_cache(TXID, <<"tx@1.0">>, Opts)),
    ?assertEqual(Item, read_cache(ItemID, <<"ans104@1.0">>, Opts)),
    ok.

load_unbundled_items_test() ->
    Opts = #{store => hb_test_utils:test_store()},
    Item1 = new_data_item(1, <<"data1">>, Opts),
    Item2 = new_data_item(2, <<"data2">>, Opts),
    Item3 = new_data_item(3, <<"data3">>, Opts),
    ok = write_item(Item1, Opts),
    ok = write_item(Item2, Opts),
    ok = write_item(Item3, Opts),
    TX = new_tx(1, Opts),
    % Link item2 to a bundle, leave others unbundled
    ok = write_tx(TX, [Item2], Opts),
    % Load unbundled items
    UnbundledItems1 = load_unbundled_items(Opts),
    UnbundledItems2 = [
        hb_message:with_commitments(
            #{ <<"commitment-device">> => <<"ans104@1.0">> },
            Item, Opts) || Item <- UnbundledItems1
        ],
    UnbundledItems3 = lists:sort(UnbundledItems2),
    ?event(debug_test, {unbundled_items, UnbundledItems3}),
    ?assertEqual(lists:sort([Item1, Item3]), UnbundledItems3),
    ok.

load_bundle_states_test() ->
    Opts = #{store => hb_test_utils:test_store()},
    TX1 = new_tx(1, Opts),
    TX2 = new_tx(2, Opts),
    TX3 = new_tx(3, Opts),    
    ok = set_tx_status(TX1, <<"posted">>, Opts),
    ok = set_tx_status(TX2, <<"complete">>, Opts),
    ok = set_tx_status(TX3, <<"posted">>, Opts),
    States = load_bundle_states(Opts),
    ?event(debug_test, {bundle_states, States}),
    % Only non-complete states are loaded
    ?assertEqual(2, length(States)),
    % Verify content
    StatesMap = maps:from_list(States),
    ?assertEqual(<<"posted">>, maps:get(tx_id(TX1, Opts), StatesMap)),
    ?assertEqual(<<"posted">>, maps:get(tx_id(TX3, Opts), StatesMap)),
    ok.

load_bundled_items_test() ->
    Opts = #{store => hb_test_utils:test_store()},
    Item1 = new_data_item(1, <<"data1">>, Opts),
    Item2 = new_data_item(2, <<"data2">>, Opts),
    Item3 = new_data_item(3, <<"data3">>, Opts),
    ok = write_item(Item1, Opts),
    ok = write_item(Item2, Opts),
    ok = write_item(Item3, Opts),
    TX1 = new_tx(1, Opts),
    TX2 = new_tx(2, Opts),
    ok = write_tx(TX1, [Item1, Item2], Opts),
    ok = write_tx(TX2, [Item3], Opts),
    % Load items for bundle 1
    Bundle1Items1 = load_bundled_items(tx_id(TX1, Opts), Opts),
    Bundle1Items2 = [
        hb_message:with_commitments(
            #{ <<"commitment-device">> => <<"ans104@1.0">> },
            Item, Opts) || Item <- Bundle1Items1
        ],
    Bundle1Items3 = lists:sort(Bundle1Items2),
    ?assertEqual(lists:sort([Item1, Item2]), Bundle1Items3),
    % Load items for bundle 2
    Bundle2Items1 = load_bundled_items(tx_id(TX2, Opts), Opts),
    Bundle2Items2 = [
        hb_message:with_commitments(
            #{ <<"commitment-device">> => <<"ans104@1.0">> },
            Item, Opts) || Item <- Bundle2Items1
        ],
    Bundle2Items3 = lists:sort(Bundle2Items2),
    ?assertEqual(lists:sort([Item3]), Bundle2Items3),
    ok.

%% @doc That when posting a bundle to the bundler all items in the bundle
%% are accessible via optimistic cache. The bundle has the following structure:
%%   L2Bundle (bundle)
%%     L3Item  (leaf)
%%     L3Bundle  (nested bundle)
%%       L4IItem1 (leaf)
%%       L4IItem2 (leaf)
bundler_optimistic_cache_test() ->
    L3Item = ar_bundles:sign_item(
        #tx{ data = <<"l3item">>, tags = [{<<"idx">>, <<"1">>}] },
        hb:wallet()
    ),
    L4Item1 = ar_bundles:sign_item(
        #tx{ data = <<"l4item1">>, tags = [{<<"idx">>, <<"2.1">>}] },
        hb:wallet()
    ),
    L4Item2 = ar_bundles:sign_item(
        #tx{ data = <<"l4item2">>, tags = [{<<"idx">>, <<"2.2">>}] },
        hb:wallet()
    ),
    % L3Bundle is itself a bundle wrapping the two L4 leaves.
    {undefined, L3BundlePayload} = ar_bundles:serialize_bundle(
        list, [L4Item1, L4Item2], false),
    L3Bundle = ar_bundles:sign_item(
        #tx{
            data = L3BundlePayload,
            tags = [
                {<<"Bundle-Format">>, <<"binary">>},
                {<<"Bundle-Version">>, <<"2.0.0">>},
                {<<"idx">>, <<"2">>}
            ]
        },
        hb:wallet()
    ),
    {undefined, L2BundlePayload} = ar_bundles:serialize_bundle(
        list, [L3Item, L3Bundle], false),
    L2Bundle = ar_bundles:sign_item(
        #tx{
            data = L2BundlePayload,
            tags = [
                {<<"Bundle-Format">>, <<"binary">>},
                {<<"Bundle-Version">>, <<"2.0.0">>}
            ]
        },
        hb:wallet()
    ),
    % Compute signed IDs for all items before posting.
    L2BundleID = hb_util:encode(ar_bundles:id(L2Bundle, signed)),
    L3ItemID   = hb_util:encode(ar_bundles:id(L3Item,   signed)),
    L3BundleID = hb_util:encode(ar_bundles:id(L3Bundle, signed)),
    L4Item1ID  = hb_util:encode(ar_bundles:id(L4Item1,  signed)),
    L4Item2ID  = hb_util:encode(ar_bundles:id(L4Item2,  signed)),
    % Start a real node with LMDB and POST the serialized bundle wrapper over HTTP.
    Node = hb_http_server:start_node(#{
        priv_wallet => hb:wallet(),
        store => hb_test_utils:test_store(hb_store_lmdb)
    }),
    try
        Serialized = ar_bundles:serialize(L2Bundle),
        ?assertMatch({ok, _}, hb_http:post(
            Node,
            #{
                <<"device">> => <<"bundler@1.0">>,
                <<"path">> => <<"/tx?codec-device=ans104@1.0">>,
                <<"content-type">> => <<"application/octet-stream">>,
                <<"body">> => Serialized
            },
            #{}
        )),
        % Every item at every nesting level must be independently readable
        % via a bare GET /ID — the real user-facing access pattern.
        AllItems = [
            {l2bundle, L2BundleID},
            {l3item,   L3ItemID},
            {l3bundle, L3BundleID},
            {l4item1,  L4Item1ID},
            {l4item2,  L4Item2ID}
        ],
        lists:foreach(
            fun({Label, ExpectedID}) ->
                {ok, Msg} = hb_http:get(
                    Node, #{ <<"path">> => <<"/", ExpectedID/binary>> }, #{}),
                ?event(debug_test, {item_result,
                    {label, Label}, {expected_id, ExpectedID}, {msg, Msg}}),
                ?assert(hb_message:verify(Msg)),
                ?assertEqual(ExpectedID, hb_message:id(Msg, signed))
            end,
            AllItems
        ),
        ok
    after
        dev_bundler:stop_server()
    end.

new_data_item(Index, SizeOrData, Opts) ->
    Data = case is_binary(SizeOrData) of
        true -> SizeOrData;
        false -> rand:bytes(SizeOrData)
    end,
    Tag = <<"tag", (integer_to_binary(Index))/binary>>,
    Value = <<"value", (integer_to_binary(Index))/binary>>,
    Item = ar_bundles:sign_item(
        #tx{
            data = Data,
            tags = [{Tag, Value}]
        },
        hb:wallet()
    ),
    hb_message:convert(Item, <<"structured@1.0">>, <<"ans104@1.0">>, Opts).

new_tx(Index, Opts) ->
    Tag = <<"tag", (integer_to_binary(Index))/binary>>,
    Value = <<"value", (integer_to_binary(Index))/binary>>,
    TX = ar_tx:sign(#tx{
        format = 2,
        tags = [{Tag, Value} ]
    }, hb:wallet()),
    hb_message:convert(TX, <<"structured@1.0">>, <<"tx@1.0">>, Opts).

read_cache(ID, Device, Opts) ->
    {ok, Resolved} = hb_ao:resolve(#{ <<"path">> => ID }, Opts),
    Loaded = hb_cache:ensure_all_loaded(Resolved, Opts),
    hb_message:with_commitments(
        #{ <<"commitment-device">> => Device }, Loaded, Opts).