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

%% @doc Fetch the blacklist and store the results in the cache table.
maybe_refresh(Opts) ->
    ensure_cache_table(Opts),
    MinWait =
        hb_util:int(
            hb_opts:get(
                blacklist_refresh_frequency,
                ?DEFAULT_MIN_WAIT,
                Opts
            )
        ),
    Time = erlang:system_time(second),
    case hb_opts:get(blacklist_last_refresh, 0, Opts) of
        LastRefresh when (Time - LastRefresh) > MinWait ->
            fetch_and_insert_ids(Opts),
            hb_http_server:set_opts(Opts#{ blacklist_last_refresh => Time });
        _ ->
            skip_update
    end.

%% @doc Fetch blacklists from all configured providers and insert IDs into the
%% cache table.
fetch_and_insert_ids(Opts) ->
    ensure_cache_table(Opts),
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
fetch_single_provider(Provider, Opts) ->
    try
        case execute_provider(Provider, Opts) of
            {ok, Blacklist} ->
                {ok, IDs} = parse_blacklist(Blacklist, Opts),
                ?event({parsed_blacklist, {ids, IDs}}),
                BlacklistID = hb_message:id(Blacklist, all, Opts),
                ?event({update_blacklist_cache,
                    {ids, IDs}, {blacklist_id, BlacklistID}}),
                Table = cache_table_name(Opts),
                {ok, insert_ids(IDs, BlacklistID, Table, Opts)};
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

%% @doc Execute the blacklist provider, returning the result.
execute_provider(Provider, Opts) ->
    ?event({execute_provider, {provider, Provider}}),
    case hb_cache:ensure_loaded(Provider, Opts) of
        Bin when is_binary(Bin) ->
            hb_ao:resolve(#{ <<"path">> => Bin }, Opts);
        Msgs when is_list(Msgs) ->
            hb_ao:resolve_many(Msgs, Opts)
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
                    fetch_and_insert_ids(Opts),
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
