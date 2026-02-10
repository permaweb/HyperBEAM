%%% @doc AO-Core device for Arweave tx/block gossip replication.
-module(dev_arweave_gossip).

-export([info/1, info/3, default/4]).
-export([tx/3, block/3, peers/3, gossip_tx/3, gossip_block/3]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(GOSSIP_PREFIX, <<"~arweave-gossip@2.9.5">>).
-define(TX_GOSSIP_PATH, <<"/~arweave-gossip@2.9.5/tx">>).
-define(BLOCK_GOSSIP_PATH, <<"/~arweave-gossip@2.9.5/block">>).

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
default(<<"peer">>, Base, Req, Opts) ->
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
            {ok, Stored} = store_item(<<"tx">>, TX, Opts),
            {ok, Stored};
        <<"GET">> ->
            case resource_suffix(<<"tx">>, Base, Req, Opts) of
                <<"pending">> ->
                    {ok, #{<<"txids">> => pending_ids(<<"tx">>, Opts)}};
                <<"ready_for_mining">> ->
                    {ok, #{<<"txids">> => pending_ids(<<"tx">>, Opts)}};
                <<>> ->
                    {ok, list_items(<<"tx">>, Opts)};
                TXID ->
                    get_item(<<"tx">>, TXID, Opts)
            end;
        _ ->
            {error, method_not_allowed}
    end.

block(Base, Req, Opts) ->
    case method(Base, Req, Opts) of
        <<"POST">> ->
            Block = read_any([<<"block">>], Base, Req, Req, Opts),
            {ok, Stored} = store_item(<<"block">>, Block, Opts),
            maybe_cache_block(Block, Opts),
            {ok, Stored};
        <<"GET">> ->
            case resource_suffix(<<"block">>, Base, Req, Opts) of
                <<>> ->
                    {ok, list_items(<<"block">>, Opts)};
                ID ->
                    get_item(<<"block">>, ID, Opts)
            end;
        _ ->
            {error, method_not_allowed}
    end.

peers(Base, Req, Opts) ->
    case method(Base, Req, Opts) of
        <<"POST">> ->
            ToAdd = normalize_peer_list(read_any([<<"peer">>, <<"peers">>], Base, Req, [], Opts)),
            Existing = persisted_peers(Opts),
            Updated = unique_peers(Existing ++ ToAdd),
            ok = write_persisted_peers(Updated, Opts),
            {ok, #{<<"peers">> => Updated, <<"added">> => ToAdd}};
        <<"DELETE">> ->
            ToRemove = normalize_peer_list(read_any([<<"peer">>, <<"peers">>], Base, Req, [], Opts)),
            Existing = persisted_peers(Opts),
            Updated = [Peer || Peer <- Existing, not lists:member(Peer, ToRemove)],
            ok = write_persisted_peers(Updated, Opts),
            {ok, #{<<"peers">> => Updated, <<"removed">> => ToRemove}};
        _ ->
            {ok, #{<<"peers">> => read_peers(Base, Req, Opts)}}
    end.

gossip_tx(Base, Req, Opts) ->
    TX = read_any([<<"tx">>], Base, Req, Req, Opts),
    _ = store_item(<<"tx">>, TX, Opts),
    {ok, broadcast(<<"tx">>, TX, Base, Req, Opts)}.

gossip_block(Base, Req, Opts) ->
    Block = read_any([<<"block">>], Base, Req, Req, Opts),
    _ = store_item(<<"block">>, Block, Opts),
    maybe_cache_block(Block, Opts),
    {ok, broadcast(<<"block">>, Block, Base, Req, Opts)}.

broadcast(Type, Item, Base, Req, Opts) ->
    Path =
        read_any(
            [<<"broadcast-path">>],
            Base,
            Req,
            default_broadcast_path(Type),
            Opts
        ),
    Peers = read_peers(Base, Req, Opts),
    Results =
        lists:map(
            fun(Peer) ->
                Payload =
                    case Type of
                        <<"tx">> -> #{<<"path">> => Path, <<"tx">> => Item};
                        _ -> #{<<"path">> => Path, <<"block">> => Item}
                    end,
                {Peer, catch hb_http:post(Peer, Payload, Opts)}
            end,
            Peers
        ),
    SuccessCount =
        length(
            [
                ok
             || {_Peer, Res} <- Results,
                is_successful_broadcast(Res)
            ]
        ),
    #{
        <<"results">> => Results,
        <<"success-count">> => SuccessCount,
        <<"failure-count">> => length(Results) - SuccessCount
    }.

is_successful_broadcast({ok, _}) ->
    true;
is_successful_broadcast(#{<<"status">> := Status}) when is_integer(Status) ->
    Status >= 200 andalso Status < 300;
is_successful_broadcast(_) ->
    false.

default_broadcast_path(<<"tx">>) ->
    ?TX_GOSSIP_PATH;
default_broadcast_path(<<"block">>) ->
    ?BLOCK_GOSSIP_PATH.

store_item(Type, Item, Opts) ->
    LogicalID = item_id(Type, Item, Opts),
    case find_item(Type, LogicalID, Opts) of
        {ok, ExistingSlot, _Entry} ->
            {ok,
                #{
                    <<"accepted">> => true,
                    <<"known">> => true,
                    <<"id">> => LogicalID,
                    <<"slot">> => ExistingSlot
                }
            };
        not_found ->
            {ok, ItemID} = hb_cache:write(Item, Opts),
            Slot = next_slot(Type, Opts),
            Entry =
                #{
                    <<"id">> => LogicalID,
                    <<"item-id">> => ItemID,
                    <<"received-at">> => os:system_time(second),
                    <<"item">> => Item
                },
            {ok, EntryID} = hb_cache:write(Entry, Opts),
            ok = hb_cache:link(EntryID, slot_path(Type, Slot, Opts), Opts),
            {ok,
                #{
                    <<"accepted">> => true,
                    <<"known">> => false,
                    <<"id">> => LogicalID,
                    <<"slot">> => Slot
                }
            }
    end.

get_item(Type, ID, Opts) ->
    case find_item(Type, ID, Opts) of
        {ok, _Slot, Entry} ->
            {ok, materialize(entry_item(Entry, Opts), Opts)};
        not_found ->
            {error, not_found}
    end.

find_item(Type, ID, Opts) ->
    lists:foldl(
        fun({Slot, Entry}, not_found) ->
                case entry_id(Entry, Opts) of
                    ID -> {ok, Slot, Entry};
                    _ -> not_found
                end;
           (_, Found) ->
                Found
        end,
        not_found,
        pool_entries(Type, Opts)
    ).

list_items(Type, Opts) ->
    Entries = pool_entries(Type, Opts),
    Items = [materialize(entry_item(Entry, Opts), Opts) || {_Slot, Entry} <- Entries],
    IDs = [entry_id(Entry, Opts) || {_Slot, Entry} <- Entries],
    #{
        <<"count">> => length(Items),
        <<"ids">> => IDs,
        <<"items">> => Items
    }.

pending_ids(Type, Opts) ->
    [entry_id(Entry, Opts) || {_Slot, Entry} <- pool_entries(Type, Opts)].

pool_entries(Type, Opts) ->
    Slots = lists:sort(hb_cache:list_numbered(pool_dir(Type), Opts)),
    lists:filtermap(
        fun(Slot) ->
            case hb_cache:read(slot_path(Type, Slot, Opts), Opts) of
                {ok, Entry} when is_map(Entry) -> {true, {Slot, Entry}};
                _ -> false
            end
        end,
        Slots
    ).

entry_id(Entry, Opts) ->
    hb_util:bin(hb_maps:get(<<"id">>, Entry, <<>>, Opts)).

entry_item(Entry, Opts) ->
    hb_maps:get(<<"item">>, Entry, Entry, Opts).

materialize(Value, Opts) ->
    try
        hb_cache:ensure_all_loaded(Value, Opts)
    catch
        _:_ -> Value
    end.

next_slot(Type, Opts) ->
    case hb_cache:list_numbered(pool_dir(Type), Opts) of
        [] -> 1;
        Slots -> lists:max(Slots) + 1
    end.

pool_dir(Type) ->
    [?GOSSIP_PREFIX, Type].

slot_path(Type, Slot, Opts) ->
    hb_store:path(
        hb_opts:get(store, no_viable_store, Opts),
        pool_dir(Type) ++ [hb_util:bin(Slot)]
    ).

item_id(_Type, Item, Opts) when is_map(Item) ->
    MaybeID =
        first_present_key(
            Item,
            [
                <<"id">>,
                <<"tx-id">>,
                <<"txid">>,
                <<"hash">>,
                <<"indep-hash">>,
                <<"indep_hash">>,
                <<"height">>
            ],
            not_found,
            Opts
        ),
    case MaybeID of
        not_found ->
            hb_util:encode(crypto:hash(sha256, term_to_binary(Item)));
        ID ->
            hb_util:bin(ID)
    end;
item_id(_Type, Item, _Opts) ->
    hb_util:encode(crypto:hash(sha256, term_to_binary(Item))).

first_present_key(_Map, [], Default, _Opts) ->
    Default;
first_present_key(Map, [Key | Rest], Default, Opts) ->
    case hb_maps:find(Key, Map, Opts) of
        {ok, Value} -> Value;
        error -> first_present_key(Map, Rest, Default, Opts)
    end.

maybe_cache_block(Block, Opts) ->
    try
        dev_arweave_block_cache:write(Block, Opts)
    catch
        _:_ -> ok
    end.

persisted_peers(Opts) ->
    case hb_cache:read(peer_state_path(Opts), Opts) of
        {ok, Stored} ->
            normalize_peer_list(hb_maps:get(<<"peers">>, Stored, [], Opts));
        _ ->
            []
    end.

write_persisted_peers(Peers, Opts) ->
    {ok, MsgID} = hb_cache:write(#{<<"peers">> => unique_peers(Peers)}, Opts),
    ok = hb_cache:link(MsgID, peer_state_path(Opts), Opts),
    ok.

peer_state_path(Opts) ->
    hb_store:path(
        hb_opts:get(store, no_viable_store, Opts),
        [?GOSSIP_PREFIX, <<"state">>, <<"peers">>]
    ).

read_peers(Base, Req, Opts) ->
    Configured =
        read_any(
            [<<"peers">>, arweave_gossip_peers],
            Base,
            Req,
            hb_opts:get(arweave_gossip_peers, [], Opts),
            Opts
        ),
    unique_peers(persisted_peers(Opts) ++ normalize_peer_list(Configured)).

normalize_peer_list(Peers) when is_list(Peers) ->
    [hb_util:bin(Peer) || Peer <- Peers];
normalize_peer_list(Peer) when is_binary(Peer) ->
    [Peer];
normalize_peer_list(Peer) when is_atom(Peer) ->
    [hb_util:bin(Peer)];
normalize_peer_list(_) ->
    [].

unique_peers(Peers) ->
    lists:foldl(
        fun(Peer, Acc) ->
            case lists:member(Peer, Acc) of
                true -> Acc;
                false -> Acc ++ [Peer]
            end
        end,
        [],
        Peers
    ).

method(Base, Req, Opts) ->
    read_any([<<"method">>], Base, Req, <<"GET">>, Opts).

resource_suffix(Resource, Base, Req, Opts) ->
    Action = hb_util:bin(read_any([<<"action">>], Base, Req, <<>>, Opts)),
    Path = hb_util:bin(read_any([<<"path">>], Base, Req, <<>>, Opts)),
    Candidate =
        case Action of
            <<>> -> Path;
            _ -> Action
        end,
    trim_resource_prefix(Resource, trim_leading_slash(Candidate)).

trim_resource_prefix(Resource, Resource) ->
    <<>>;
trim_resource_prefix(Resource, Candidate) ->
    Prefix = <<Resource/binary, "/">>,
    PrefixSize = byte_size(Prefix),
    case Candidate of
        <<Prefix:PrefixSize/binary, Rest/binary>> -> Rest;
        _ -> Candidate
    end.

trim_leading_slash(<<"/", Rest/binary>>) ->
    trim_leading_slash(Rest);
trim_leading_slash(Bin) ->
    Bin.

read_any(Keys, Base, Req, Default, Opts) ->
    read_any_local(Keys, Req, Base, Default, Opts).

read_any_local([], _Req, _Base, Default, _Opts) ->
    Default;
read_any_local([Key | Rest], Req, Base, Default, Opts) ->
    case hb_maps:find(Key, Req, Opts) of
        {ok, Value} ->
            Value;
        error ->
            case is_map(Base) of
                true ->
                    case hb_maps:find(Key, Base, Opts) of
                        {ok, Value2} -> Value2;
                        error -> read_any_local(Rest, Req, Base, Default, Opts)
                    end;
                false ->
                    read_any_local(Rest, Req, Base, Default, Opts)
            end
    end.

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

resolve_gossip(Path, Req, Opts) ->
    hb_ao:resolve(
        #{<<"device">> => dev_arweave_gossip},
        Req#{<<"path">> => Path},
        test_opts(Opts)
    ).

test_opts(Opts) ->
    case maps:is_key(store, Opts) of
        true -> Opts;
        false -> Opts#{store => [hb_test_utils:test_store()]}
    end.

tx_pool_roundtrip_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    TX = #{<<"id">> => <<"tx-1">>, <<"hello">> => <<"world">>},
    {ok, #{<<"accepted">> := true, <<"known">> := false}} =
        resolve_gossip(<<"tx">>, #{<<"method">> => <<"POST">>, <<"tx">> => TX}, Opts),
    {ok, #{<<"accepted">> := true, <<"known">> := true}} =
        resolve_gossip(<<"tx">>, #{<<"method">> => <<"POST">>, <<"tx">> => TX}, Opts),
    {ok, Listed} = resolve_gossip(<<"tx">>, #{<<"method">> => <<"GET">>}, Opts),
    ?assertEqual(1, hb_maps:get(<<"count">>, Listed, 0, #{})).

pending_ids_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    TX1 = #{<<"id">> => <<"tx-a">>},
    TX2 = #{<<"id">> => <<"tx-b">>},
    {ok, _} = resolve_gossip(<<"tx">>, #{<<"method">> => <<"POST">>, <<"tx">> => TX1}, Opts),
    {ok, _} = resolve_gossip(<<"tx">>, #{<<"method">> => <<"POST">>, <<"tx">> => TX2}, Opts),
    {ok, #{<<"txids">> := IDs}} =
        resolve_gossip(
            <<"tx">>,
            #{<<"method">> => <<"GET">>, <<"action">> => <<"pending">>},
            Opts
        ),
    ?assertEqual(2, length(IDs)).

get_tx_by_id_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    TX = #{<<"id">> => <<"tx-42">>, <<"a">> => 1},
    {ok, _} = resolve_gossip(<<"tx">>, #{<<"method">> => <<"POST">>, <<"tx">> => TX}, Opts),
    {ok, Loaded} =
        resolve_gossip(
            <<"tx">>,
            #{<<"method">> => <<"GET">>, <<"action">> => <<"tx-42">>},
            Opts
        ),
    ?assertEqual(<<"tx-42">>, hb_maps:get(<<"id">>, Loaded, <<>>, #{})),
    ?assertEqual(1, hb_maps:get(<<"a">>, Loaded, 0, #{})).

block_pool_roundtrip_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    Block =
        #{
            <<"height">> => 1,
            <<"indep_hash">> => <<"hash-1">>,
            <<"hash">> => <<"hash-2">>
        },
    {ok, #{<<"accepted">> := true}} =
        resolve_gossip(<<"block">>, #{<<"method">> => <<"POST">>, <<"block">> => Block}, Opts),
    {ok, Listed} = resolve_gossip(<<"block">>, #{<<"method">> => <<"GET">>}, Opts),
    ?assertEqual(1, hb_maps:get(<<"count">>, Listed, 0, #{})).

peers_read_test() ->
    {ok, #{<<"peers">> := [<<"http://example.com">>]}} =
        resolve_gossip(
            <<"peers">>,
            #{<<"method">> => <<"GET">>, <<"peers">> => [<<"http://example.com">>]},
            #{}
        ).

peers_add_remove_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    {ok, #{<<"peers">> := [<<"http://a">>]}} =
        resolve_gossip(
            <<"peers">>,
            #{<<"method">> => <<"POST">>, <<"peer">> => <<"http://a">>},
            Opts
        ),
    {ok, #{<<"peers">> := [<<"http://a">>]}} =
        resolve_gossip(<<"peers">>, #{<<"method">> => <<"GET">>}, Opts),
    {ok, #{<<"peers">> := []}} =
        resolve_gossip(
            <<"peers">>,
            #{<<"method">> => <<"DELETE">>, <<"peer">> => <<"http://a">>},
            Opts
        ).

gossip_tx_no_peers_test() ->
    Opts = #{store => [hb_test_utils:test_store()]},
    {ok, Res} =
        resolve_gossip(
            <<"gossip-tx">>,
            #{<<"tx">> => #{<<"id">> => <<"tx-x">>}},
            Opts
        ),
    ?assertEqual(0, hb_maps:get(<<"success-count">>, Res, -1, #{})),
    ?assertEqual(0, hb_maps:get(<<"failure-count">>, Res, -1, #{})).
