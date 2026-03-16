%%% @doc A request hook device for content moderation by blacklist.
%%%
%%% The node operator configures blacklist providers via the
%%% `blacklist-providers` key (a list) in the node message options. Each provider
%%% can be a message or a path that returns a message or binary. If a binary is
%%% returned from a provider, it is parsed as a newline-delimited list of IDs.
%%% Multiple providers are merged into a single cache (union of all IDs).
%%% 
%%% The device is intended for use as a `~hook@1.0` `on/request` handler. It
%%% blocks requests when any ID present in the hook payload matches the active
%%% blacklist. The device also implements a `refresh` key that can be used to
%%% force a reload of the blacklist cache, potentially on node startup or on a 
%%% `~cron@1.0/every` trigger.
%%% 
%%% The principle of this device is the same as the content policies utilized in
%%% the Arweave network: No central enforcement, but each node is capable of
%%% enforcing its own content policies based on its own free choice and
%%% configuration.
-module(dev_blacklist).
-export([request/3, refresh/3]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_MIN_WAIT, 60).

%% @doc Hook handler: block requests that involve blacklisted IDs.
request(_Base, HookReq, Opts) ->
    ?event({hook_req, HookReq}),
    case hb_opts:get(blacklist_providers, false, Opts) of
        false -> {ok, HookReq};
        _ ->
            case is_match(HookReq, Opts) of
                false ->
                    ?event(blacklist, {allowed, HookReq}, Opts),
                    {ok, HookReq};
                ID ->
                    ?event(blacklist, {blocked, ID}, Opts),
                    {
                        ok,
                        HookReq#{
                            <<"body">> =>
                                [#{
                                    <<"status">> => 451,
                                    <<"reason">> => <<"content-policy">>,
                                    <<"blocked-id">> => ID,
                                    <<"body">> =>
                                        <<
                                            "Requested message blocked by this node's ",
                                            "content policy. Blocked ID: ", ID/binary
                                        >>
                                }]
                        }
                    }
            end
    end.

%% @doc Check if the message contains any blacklisted IDs.
is_match(Msg, Opts) ->
    maybe_refresh(Opts),
    IDs = collect_ids(Msg, Opts),
    MatchesFromIDs = fun(ID) -> ets:lookup(cache_table_name(Opts), ID) =/= [] end,
    case lists:filter(MatchesFromIDs, IDs) of
        [] -> false;
        [ID|_] -> ID
    end.

%% @doc Force a reload of the blacklist cache. Returns the number of newly 
%% inserted IDs.
refresh(Base, Req, Opts) ->
    ?event({refresh_called, {base, Base}, {req, Req}}),
    maybe_refresh(Opts).

%%% Internal

%% @doc Check if a refresh is due and, if so, atomically claim the refresh so
%% that only one process fetches at a time (no thundering herd).
maybe_refresh(Opts) ->
    Table = ensure_cache_table(Opts),
    MinWait =
        hb_util:int(
            hb_opts:get(
                blacklist_refresh_frequency,
                ?DEFAULT_MIN_WAIT,
                Opts
            )
        ),
    Time = erlang:system_time(second),
    LastRefresh =
        case ets:lookup(Table, {meta, last_refresh}) of
            [{{meta, last_refresh}, T}] -> T;
            [] -> 0
        end,
    case (Time - LastRefresh) > MinWait of
        false ->
            skip_update;
        true ->
            try_claim_refresh(Table, Opts)
    end.

%% @doc Attempt to atomically claim the refresh sentinel. Only the winner
%% proceeds with the actual fetch; losers return skip_update. During cold
%% start (no prior refresh completed), losers wait for the initial refresh
%% rather than skipping, to avoid evaluating against an empty blacklist.
try_claim_refresh(Table, Opts) ->
    case ets:insert_new(Table, {{meta, refreshing}, self()}) of
        true ->
            do_refresh(Table, Opts);
        false ->
            case ets:lookup(Table, {meta, refreshing}) of
                [{{meta, refreshing}, PID}] ->
                    case is_process_alive(PID) of
                        true ->
                            wait_if_cold_start(Table);
                        false ->
                            ets:select_delete(Table, [
                                {{{meta, refreshing}, PID}, [], [true]}
                            ]),
                            try_claim_refresh(Table, Opts)
                    end;
                [] ->
                    try_claim_refresh(Table, Opts)
            end
    end.

%% @doc During cold start (no {meta, last_refresh} yet), wait for the
%% in-progress refresh to complete so callers don't see an empty blacklist.
%% In steady state, skip immediately since we already have data.
wait_if_cold_start(Table) ->
    case ets:lookup(Table, {meta, last_refresh}) of
        [] ->
            hb_util:until(
                fun() ->
                    ets:lookup(Table, {meta, last_refresh}) =/= []
                end,
                100
            ),
            skip_update;
        _ ->
            skip_update
    end.

%% @doc Perform the actual refresh: fetch all providers, then update the
%% last-refresh timestamp and release the sentinel.
do_refresh(Table, Opts) ->
    try
        ets:update_counter(
            Table, {meta, refresh_count}, 1, {{meta, refresh_count}, 0}
        ),
        fetch_and_insert_ids(Opts)
    after
        ets:insert(Table, {{meta, last_refresh}, erlang:system_time(second)}),
        ets:delete(Table, {meta, refreshing})
    end.

%% @doc Fetch blacklists from all configured providers and insert IDs into the
%% cache table. The caller must ensure the table exists before calling this.
fetch_and_insert_ids(Opts) ->
    Providers = resolve_providers(Opts),
    Total = lists:foldl(
        fun(Provider, Acc) ->
            case fetch_single_provider(Provider, Opts) of
                {ok, Count} -> Acc + Count;
                {error, _} -> Acc
            end
        end,
        0,
        Providers
    ),
    ?event(blacklist_short, {fetched_and_inserted_ids, Total}, Opts),
    {ok, Total}.

%% @doc Resolve the configured providers into a list.
resolve_providers(Opts) ->
    case hb_opts:get(blacklist_providers, [], Opts) of
        Providers when is_list(Providers) -> Providers;
        _ -> []
    end.

%% @doc Fetch a single provider's blacklist and insert its IDs into the cache.
%% Handles 304 Not Modified responses when an ETag was sent.
fetch_single_provider(Provider, Opts) ->
    try
        case execute_provider(Provider, Opts) of
            {ok, Blacklist} ->
                Status = hb_maps:get(
                    <<"status">>, Blacklist, 200, Opts
                ),
                case hb_util:int(Status) of
                    304 ->
                        Table = cache_table_name(Opts),
                        ets:update_counter(
                            Table,
                            {meta, not_modified_count},
                            1,
                            {{meta, not_modified_count}, 0}
                        ),
                        ?event(blacklist_short,
                            {provider_not_modified, Provider}, Opts),
                        {ok, 0};
                    _ ->
                        maybe_store_etag(Provider, Blacklist, Opts),
                        {ok, IDs} = parse_blacklist(Blacklist, Opts),
                        ?event({parsed_blacklist, {ids, IDs}}),
                        BlacklistID =
                            hb_message:id(Blacklist, all, Opts),
                        ?event({update_blacklist_cache,
                            {ids, IDs}, {blacklist_id, BlacklistID}}),
                        Table = cache_table_name(Opts),
                        {ok, insert_ids(IDs, BlacklistID, Table, Opts)}
                end;
            {error, _} = Error ->
                ?event({execute_provider_error, Error}),
                Error
        end
    catch
        Type:Reason ->
            ?event({provider_fetch_error,
                {type, Type}, {reason, Reason}, {provider, Provider}}),
            {error, {Type, Reason}}
    end.

%% @doc Execute the blacklist provider, returning the result. For binary
%% providers (HTTP via relay), inject If-None-Match when a stored ETag exists.
execute_provider(Provider, Opts) ->
    ?event({execute_provider, {provider, Provider}}),
    case hb_cache:ensure_loaded(Provider, Opts) of
        Bin when is_binary(Bin) ->
            Table = cache_table_name(Opts),
            Msg =
                case ets:lookup(Table, {etag, Provider}) of
                    [{{etag, _}, StoredETag}] ->
                        #{
                            <<"path">> => Bin,
                            <<"if-none-match">> => StoredETag
                        };
                    [] ->
                        #{ <<"path">> => Bin }
                end,
            hb_ao:resolve(Msg, Opts);
        Msgs when is_list(Msgs) ->
            hb_ao:resolve_many(Msgs, Opts)
    end.

%% @doc Store the ETag from a provider response, or clear a stale one.
maybe_store_etag(Provider, Response, Opts) ->
    Table = cache_table_name(Opts),
    case hb_maps:get(<<"etag">>, Response, not_found, Opts) of
        not_found ->
            ets:delete(Table, {etag, Provider});
        ETag ->
            ets:insert(Table, {{etag, Provider}, ETag})
    end.

%% @doc Parse the blacklist body, returning a list of IDs.
parse_blacklist(Link, Opts) when ?IS_LINK(Link) ->
    parse_blacklist(hb_cache:ensure_loaded(Link, Opts), Opts);
parse_blacklist(Body, _Opts) when is_list(Body) ->
    {ok, lists:filtermap(fun parse_blacklist_line/1, Body)};
parse_blacklist(Msg, Opts) when is_map(Msg) ->
    maybe
        {ok, Body} = hb_maps:find(<<"body">>, Msg, Opts),
        parse_blacklist(Body, Opts)
    end;
parse_blacklist(Body, _Opts) when is_binary(Body) ->
    Lines = binary:split(Body, <<"\n">>, [global]),
    {ok, lists:filtermap(fun parse_blacklist_line/1, Lines)}.

%% @doc Parse a single line of the blacklist body, returning the ID if it is valid,
%% and `false' otherwise.
parse_blacklist_line(Line) ->
    Trimmed = string:trim(Line, both),
    case Trimmed of
        <<>> -> false;
        <<"#", _/binary>> -> false;
        ID when ?IS_ID(ID) -> {true, hb_util:human_id(ID)};
        _ -> false
    end.

%% @doc Collect all IDs found as elements of a given message.
collect_ids(Msg, Opts) -> lists:usort(collect_ids(Msg, [], Opts)).
collect_ids(Bin, Acc, _Opts) when ?IS_ID(Bin) -> [hb_util:human_id(Bin) | Acc];
collect_ids(Bin, Acc, _Opts) when is_binary(Bin) -> Acc;
collect_ids({as, _, Msg}, Acc, Opts) -> collect_ids(Msg, Acc, Opts);
collect_ids({link, ID, _}, Acc, _Opts) when ?IS_ID(ID) ->
    [hb_util:human_id(ID) | Acc];
collect_ids(Msg, Acc, Opts) when is_map(Msg) ->
    case hb_maps:get(<<"path">>, Msg, undefined, Opts) of
        Path when ?IS_ID(Path) -> [hb_util:human_id(Path)];
        _ -> []
    end ++
    hb_maps:keys(hb_maps:get(<<"commitments">>, Msg, #{}, Opts), Opts) ++
    hb_maps:fold(
        fun(_Key, Value, AccIn) -> collect_ids(Value, AccIn, Opts) end,
        Acc,
        Msg
    );
collect_ids(List, Acc, Opts) when is_list(List) ->
    lists:foldl(
        fun(Elem, AccIn) -> collect_ids(Elem, AccIn, Opts) end,
        Acc,
        List
    );
collect_ids(_Other, Acc, _Opts) -> Acc.

%% @doc Insert a list of IDs into the cache table, returning the number of new IDs
%% inserted. Each ID is inserted as a key with the current timestamp as the value.
insert_ids([], _Value, _Table, _Opts) -> 0;
insert_ids([ID | IDs], Value, Table, Opts) when ?IS_ID(ID) ->
    case ets:lookup(Table, ID) of
        [] ->
            ets:insert(Table, {ID, Value}),
            1 + insert_ids(IDs, Value, Table, Opts);
        _ -> insert_ids(IDs, Value, Table, Opts)
    end.

%% @doc Ensure the cache table exists.
ensure_cache_table(Opts) ->
    TableName = cache_table_name(Opts),
    case ets:info(TableName) of
        undefined ->
            hb_name:singleton(
                TableName,
                fun() ->
                    ?event({creating_table, TableName}),
                    ets:new(
                        TableName,
                        [
                            named_table,
                            set,
                            public,
                            {read_concurrency, true},
                            {write_concurrency, true}
                        ]
                    ),
                    try_claim_refresh(TableName, Opts),
                    receive kill -> ok end
                end
            ),
            hb_util:until(
                fun() -> ets:info(TableName) =/= undefined end,
                100
            ),
            TableName;
        _ ->
            TableName
    end.

%% @doc Calculate the name of the cache table given the `Opts`.
cache_table_name(Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(Wallet),
    binary_to_atom(<<"~blacklist@1.0/cache/", Address/binary>>).

%%% Tests

setup_test_env() ->
    %% We need to create a new priv_wallet to avoid conflift when starting a
    %% new node from an existing priv_wallet address.
    Opts0 = #{ store => hb_test_utils:test_store(), priv_wallet => ar_wallet:new() },
    Msg1 = hb_message:commit(#{ <<"body">> => <<"test-1">> }, Opts0),
    Msg2 = hb_message:commit(#{ <<"body">> => <<"test-2">> }, Opts0),
    Msg3 = hb_message:commit(#{ <<"body">> => <<"test-3">> }, Opts0),
    SignedID1 = hb_message:id(Msg1, signed, Opts0),
    {ok, _UnsignedID1} = hb_cache:write(Msg1, Opts0),
    {ok, UnsignedID2} = hb_cache:write(Msg2, Opts0),
    {ok, UnsignedID3} = hb_cache:write(Msg3, Opts0),
    Blacklist =
        #{
            <<"data-protocol">> => <<"content-policy">>,
            <<"body">> => <<SignedID1/binary, "\n", UnsignedID2/binary, "\n">>
        },
    BlacklistMsg = hb_message:commit(Blacklist, Opts0),
    {ok, BlacklistID} = hb_cache:write(BlacklistMsg, Opts0),
    ?event(
        {test_env_setup,
            {opts, Opts0},
            {signed_id1, SignedID1},
            {unsigned_id2, UnsignedID2},
            {unsigned_id3, UnsignedID3},
            {blocked, [SignedID1, UnsignedID2]}
        }
    ),
    {ok, #{
        opts => Opts0,
        signed1=> SignedID1,
        unsigned2=> UnsignedID2,
        unsigned3 => UnsignedID3,
        blacklist => BlacklistID
    }}.

%% @doc Test the blacklist device with a static blacklist that is in the local
%% store.
basic_test() ->
    {ok, #{
        opts := Opts0,
        signed1 := SignedID1,
        unsigned3 := UnsignedID3,
        blacklist := BlacklistID
    }} = setup_test_env(),
    Opts1 =
        Opts0#{
            blacklist_providers => [BlacklistID],
            on => #{
                <<"request">> => #{ <<"device">> => <<"blacklist@1.0">> }
            }
        },
    Node = hb_http_server:start_node(Opts1),
    ?assertMatch(
        {ok, <<"test-3">>},
        hb_http:get(Node, <<"/", UnsignedID3/binary, "/body">>, Opts1)
    ),
    ?assertMatch(
        {error,
            #{
                <<"status">> := 451,
                <<"reason">> := <<"content-policy">>
            }},
        hb_http:get(Node, SignedID1, Opts1)
    ),
    ok.

%% @doc Ensure that the default provider does not block any requests.
default_provider_test() ->
    {ok, #{
        opts := Opts0,
        signed1 := SignedID1,
        unsigned3 := UnsignedID3
    }} = setup_test_env(),
    Opts1 = Opts0#{ blacklist_providers => [] },
    Node = hb_http_server:start_node(Opts1),
    ?assertMatch(
        {ok, <<"test-3">>},
        hb_http:get(Node, <<"/", UnsignedID3/binary, "/body">>, Opts1)
    ),
    ?assertMatch(
        {ok, <<"test-1">>},
        hb_http:get(Node, <<SignedID1/binary, "/body">>, Opts1)
    ),
    ok.

%% @doc Test the blacklist device with a blacklist that is provided via HTTP.
blacklist_from_external_http_test() ->
    {ok, #{
        opts := RemoteOpts = #{ store := RootStore },
        signed1 := SignedID1,
        unsigned3 := UnsignedID3,
        blacklist := BlacklistID
    }} = setup_test_env(),
    % Start a node that we will ask to provide the blacklist via HTTP.
    BlacklistHostNode = hb_http_server:start_node(RemoteOpts),
    % Start a node that will use the blacklist host node to provide the blacklist
    % via HTTP.
    NodeOpts = 
        #{
            store => RootStore,
            priv_wallet => ar_wallet:new(),
            blacklist_providers =>
                [<<
                    "/~relay@1.0/call?relay-method=GET&relay-path=",
                        BlacklistHostNode/binary, BlacklistID/binary
                >>],
            on => #{
                <<"request">> => #{ <<"device">> => <<"blacklist@1.0">> }
            }
        },
    Node = hb_http_server:start_node(NodeOpts),
    ?assertMatch(
        {ok, <<"test-3">>},
        hb_http:get(Node, <<"/", UnsignedID3/binary, "/body">>, NodeOpts)
    ),
    ?assertMatch(
        {error,
            #{
                <<"status">> := 451,
                <<"reason">> := <<"content-policy">>
            }},
        hb_http:get(Node, SignedID1, NodeOpts)
    ).

%% @doc Test that multiple providers merge their blacklists.
multiple_providers_test() ->
    {ok, #{
        opts := Opts0,
        signed1 := SignedID1,
        unsigned2 := UnsignedID2,
        unsigned3 := UnsignedID3
    }} = setup_test_env(),
    Blacklist1 = #{
        <<"data-protocol">> => <<"content-policy">>,
        <<"body">> => <<SignedID1/binary, "\n">>
    },
    Blacklist2 = #{
        <<"data-protocol">> => <<"content-policy">>,
        <<"body">> => <<UnsignedID2/binary, "\n">>
    },
    BlacklistMsg1 = hb_message:commit(Blacklist1, Opts0),
    BlacklistMsg2 = hb_message:commit(Blacklist2, Opts0),
    {ok, BlacklistID1} = hb_cache:write(BlacklistMsg1, Opts0),
    {ok, BlacklistID2} = hb_cache:write(BlacklistMsg2, Opts0),
    Opts1 = Opts0#{
        blacklist_providers => [BlacklistID1, BlacklistID2],
        on => #{
            <<"request">> => #{ <<"device">> => <<"blacklist@1.0">> }
        }
    },
    Node = hb_http_server:start_node(Opts1),
    ?assertMatch(
        {error, #{ <<"status">> := 451 }},
        hb_http:get(Node, SignedID1, Opts1)
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 451 }},
        hb_http:get(Node, <<"/", UnsignedID2/binary>>, Opts1)
    ),
    ?assertMatch(
        {ok, <<"test-3">>},
        hb_http:get(Node, <<"/", UnsignedID3/binary, "/body">>, Opts1)
    ),
    ok.

%% @doc Test that a failing provider does not prevent other providers from
%% contributing entries.
provider_failure_resilience_test() ->
    {ok, #{
        opts := Opts0,
        signed1 := SignedID1,
        unsigned3 := UnsignedID3,
        blacklist := BlacklistID
    }} = setup_test_env(),
    BadProvider = <<"aaaabbbbccccddddeeeeffffgggghhhhiiiijjjjkkkk">>,
    Opts1 = Opts0#{
        blacklist_providers => [BadProvider, BlacklistID],
        on => #{
            <<"request">> => #{ <<"device">> => <<"blacklist@1.0">> }
        }
    },
    Node = hb_http_server:start_node(Opts1),
    ?assertMatch(
        {error, #{ <<"status">> := 451 }},
        hb_http:get(Node, SignedID1, Opts1)
    ),
    ?assertMatch(
        {ok, <<"test-3">>},
        hb_http:get(Node, <<"/", UnsignedID3/binary, "/body">>, Opts1)
    ),
    ok.

%% @doc Test that concurrent requests on a fresh node only trigger one provider
%% fetch (cold-start herd prevention).
cold_start_herd_test() ->
    {ok, #{
        opts := Opts0,
        signed1 := SignedID1,
        blacklist := BlacklistID
    }} = setup_test_env(),
    BlacklistHostNode = hb_http_server:start_node(Opts0),
    ProviderPath =
        <<
            "/~relay@1.0/call?relay-method=GET&relay-path=",
            BlacklistHostNode/binary, BlacklistID/binary
        >>,
    NodeOpts =
        #{
            store => maps:get(store, Opts0),
            priv_wallet => ar_wallet:new(),
            blacklist_providers => [ProviderPath],
            on => #{
                <<"request">> => #{ <<"device">> => <<"blacklist@1.0">> }
            }
        },
    Node = hb_http_server:start_node(NodeOpts),
    N = 20,
    Parent = self(),
    Ref = make_ref(),
    _Workers = [
        spawn(fun() ->
            hb_http:get(Node, <<"/some-path">>, NodeOpts),
            Parent ! {Ref, done}
        end)
    || _ <- lists:seq(1, N)],
    lists:foreach(fun(_) -> receive {Ref, done} -> ok end end, lists:seq(1, N)),
    Table = cache_table_name(NodeOpts),
    [{{meta, refresh_count}, Count}] =
        ets:lookup(Table, {meta, refresh_count}),
    ?assertEqual(1, Count),
    ?assertEqual([], ets:lookup(Table, {meta, refreshing})),
    ?assertNotEqual(
        [],
        ets:lookup(Table, hb_util:human_id(SignedID1))
    ),
    ok.

%% @doc Test that concurrent requests with a stale timestamp only trigger one
%% provider fetch (steady-state herd prevention).
steady_state_herd_test() ->
    {ok, #{
        opts := Opts0,
        signed1 := SignedID1,
        blacklist := BlacklistID
    }} = setup_test_env(),
    Opts1 =
        Opts0#{
            blacklist_providers => [BlacklistID],
            blacklist_refresh_frequency => 0,
            on => #{
                <<"request">> => #{ <<"device">> => <<"blacklist@1.0">> }
            }
        },
    Node = hb_http_server:start_node(Opts1),
    hb_http:get(Node, <<"/warmup">>, Opts1),
    Table = cache_table_name(Opts1),
    CountBefore =
        case ets:lookup(Table, {meta, refresh_count}) of
            [{{meta, refresh_count}, C}] -> C;
            [] -> 0
        end,
    ets:insert(Table, {{meta, last_refresh}, 0}),
    timer:sleep(1100),
    N = 20,
    Parent = self(),
    Ref = make_ref(),
    _Workers = [
        spawn(fun() ->
            hb_http:get(Node, <<"/some-path">>, Opts1),
            Parent ! {Ref, done}
        end)
    || _ <- lists:seq(1, N)],
    lists:foreach(fun(_) -> receive {Ref, done} -> ok end end, lists:seq(1, N)),
    ?assertEqual([], ets:lookup(Table, {meta, refreshing})),
    [{{meta, refresh_count}, CountAfter}] =
        ets:lookup(Table, {meta, refresh_count}),
    ?assertEqual(1, CountAfter - CountBefore),
    ?assertNotEqual(
        [],
        ets:lookup(Table, hb_util:human_id(SignedID1))
    ),
    ok.

%% @doc Test that a stale sentinel (dead PID) is reclaimed and refresh proceeds.
crash_recovery_test() ->
    {ok, #{
        opts := Opts0,
        signed1 := SignedID1,
        blacklist := BlacklistID
    }} = setup_test_env(),
    Opts1 =
        Opts0#{
            blacklist_providers => [BlacklistID],
            blacklist_refresh_frequency => 0
        },
    Table = ensure_cache_table(Opts1),
    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),
    ?assertNot(is_process_alive(DeadPid)),
    ets:insert(Table, {{meta, refreshing}, DeadPid}),
    ets:insert(Table, {{meta, last_refresh}, 0}),
    timer:sleep(1100),
    maybe_refresh(Opts1),
    ?assertEqual([], ets:lookup(Table, {meta, refreshing})),
    [{{meta, last_refresh}, Ts}] =
        ets:lookup(Table, {meta, last_refresh}),
    ?assert(Ts > 0),
    ?assertNotEqual([], ets:lookup(Table, hb_util:human_id(SignedID1))),
    ok.

%% @doc Directly test the 304 Not Modified code path in fetch_single_provider.
%% Creates a provider whose resolved message has status 304, verifying that
%% fetch_single_provider returns {ok, 0} and increments the not_modified_count.
not_modified_provider_test() ->
    Opts0 = #{ store => hb_test_utils:test_store(), priv_wallet => ar_wallet:new() },
    Table = ensure_cache_table(Opts0),
    Response304 = hb_message:commit(
        #{ <<"status">> => 304, <<"body">> => <<>> },
        Opts0
    ),
    {ok, Response304ID} = hb_cache:write(Response304, Opts0),
    CountBefore =
        case ets:lookup(Table, {meta, not_modified_count}) of
            [{{meta, not_modified_count}, N}] -> N;
            [] -> 0
        end,
    Result = fetch_single_provider(Response304ID, Opts0),
    ?assertEqual({ok, 0}, Result),
    [{{meta, not_modified_count}, CountAfter}] =
        ets:lookup(Table, {meta, not_modified_count}),
    ?assertEqual(CountBefore + 1, CountAfter),
    ok.
