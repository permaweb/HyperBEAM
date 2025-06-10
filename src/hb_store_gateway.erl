%%% @doc A store module that reads data from the nodes Arweave gateway and 
%%% GraphQL routes, additionally including additional store-specific routes.
-module(hb_store_gateway).
-export([scope/1, type/2, read/2, resolve/2, list/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The scope of a GraphQL store is always remote, due to performance.
scope(_) -> remote.
resolve(_, Key) -> Key.

list(StoreOpts, Key) ->
    case read(StoreOpts, Key) of
        not_found -> not_found;
        {ok, Message} -> {ok, maps:keys(Message)}
    end.

%% @doc Get the type of the data at the given key. We potentially cache the
%% result, so that we don't have to read the data from the GraphQL route
%% multiple times.
type(StoreOpts, Key) ->
    ?event({type, StoreOpts, Key}),
    case read(StoreOpts, Key) of
        not_found -> not_found;
        {ok, Data} ->
            ?event({type, hb_private:reset(hb_message:uncommitted(Data))}),
            IsFlat = lists:all(
                fun({_, Value}) -> not is_map(Value) end,
                maps:to_list(hb_private:reset(hb_message:uncommitted(Data)))
            ),
            if
                IsFlat -> simple;
                true -> composite
            end
    end.

%% @doc Read the data at the given key from the GraphQL route. Will only attempt
%% to read the data if the key is an ID.
read(StoreOpts, Key) ->
    case hb_path:term_to_path_parts(Key) of
        [ID] when ?IS_ID(ID) ->
            ?event({read, StoreOpts, Key}),
            case hb_gateway_client:read(Key, StoreOpts) of
                {error, _} -> not_found;
                {ok, Message} ->
                    ?event(remote_read, {got_message_from_gateway, Message}),
                    maybe_cache(StoreOpts, Message),
                    {ok, Message}
            end;
        _ ->
            ?event({ignoring_non_id, Key}),
            not_found
    end.

%% @doc Cache the data if the cache is enabled. The `store' option may either
%% be `false' to disable local caching, or a store definition to use as the
%% cache.
maybe_cache(StoreOpts, Data) ->
    ?event({maybe_cache, StoreOpts, Data}),
    % Check for store in both the direct map and the legacy opts map
    Store = case maps:get(<<"store">>, StoreOpts, not_found) of
        not_found -> 
            % Check in legacy opts format
            NestedOpts = maps:get(<<"opts">>, StoreOpts, #{}),
            hb_opts:get(store, false, NestedOpts);
        FoundStore -> 
            FoundStore
    end,
    case Store of
        false -> do_nothing;
        Store ->
            ?event({writing_message_to_local_cache, Data}),
            case hb_cache:write(Data, #{ store => Store}) of
                {ok, _} -> Data;
                {error, Err} ->
                    ?event(warning, {error_writing_to_local_gateway_cache, Err}),
                    Data
            end
    end.

%%% Tests

%% @doc Store is accessible via the default options.
graphql_as_store_test_() ->
	{timeout, 10, fun() ->
		hb_http_server:start_node(#{}),
		?assertMatch(
			{ok, #{ <<"type">> := <<"Assignment">> }},
			hb_store:read(
				[#{ <<"store-module">> => hb_store_gateway, <<"opts">> => #{} }],
				<<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>
			)
		)
	end}.

%% @doc Stored messages are accessible via `hb_cache' accesses.
graphql_from_cache_test() ->
    hb_http_server:start_node(#{}),
    Opts = #{ store => [#{ <<"store-module">> => hb_store_gateway, <<"opts">> => #{} }] },
    ?assertMatch(
        {ok, #{ <<"type">> := <<"Assignment">> }},
        hb_cache:read(
            <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
            Opts
        )
    ).

manual_local_cache_test() ->
    hb_http_server:start_node(#{}),
    Local = #{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST">> },
    hb_store:reset(Local),
    Gateway = #{ <<"store-module">> => hb_store_gateway, <<"store">> => false },
    {ok, FromRemote} =
        hb_cache:read(
            <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
            #{ store => Gateway }
        ),
    ?event({writing_recvd_to_local, FromRemote}),
    {ok, _} = hb_cache:write(FromRemote, #{ store => Local }),
    {ok, Read} =
        hb_cache:read(
            <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
            #{ store => Local }
        ),
    ?event({read_from_local, Read}),
    ?assert(hb_message:match(Read, FromRemote)).

%% @doc Ensure that saving to the gateway store works.
cache_read_message_test() ->
    hb_http_server:start_node(#{}),
    Local = #{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST">> },
    hb_store:reset(Local),
    WriteOpts = #{ store =>
        [
            #{ <<"store-module">> => hb_store_gateway,
                <<"store">> => [Local]
            }
        ]
    },
    {ok, Written} =
        hb_cache:read(
            <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
            WriteOpts
        ),
    {ok, Read} =
        hb_cache:read(
            <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
            #{ store => [Local] }
        ),
    ?assert(hb_message:match(Read, Written)).

%% @doc Routes can be specified in the options, overriding the default routes.
%% We test this by inversion: If the above cache read test works, then we know 
%% that the default routes allow access to the item. If the test below were to
%% produce the same result, despite an empty 'only' route list, then we would
%% know that the module is not respecting the route list.
specific_route_test() ->
    hb_http_server:start_node(#{}),
    Opts = #{
        store =>
            [
                #{ <<"store-module">> => hb_store_gateway, 
                   <<"routes">> => [],
                   <<"only">> => local
                }
            ]
    },
    ?assertMatch(
        not_found,
        hb_cache:read(
            <<"0Tb9mULcx8MjYVgXleWMVvqo1_jaw_P6AO_CJMTj0XE">>,
            Opts
        )
    ).

%% @doc Test that the default node config allows for data to be accessed.
external_http_access_test() ->
    Node = hb_http_server:start_node(
        #{
            cache_control => <<"cache">>,
            store =>
                [
                    #{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST">> },
                    #{ <<"store-module">> => hb_store_gateway, <<"store">> => false }
                ]
        }
    ),
    ?assertMatch(
        {ok, #{ <<"data-protocol">> := <<"ao">> }},
        hb_http:get(
            Node,
            <<"p45HPD-ENkLS7Ykqrx6p_DYGbmeHDeeF8LJ09N2K53g">>,
            #{}
        )
    ).

%% Ensure that we can get data from the gateway and execute upon it.
resolve_on_gateway_test_() ->
    {timeout, 10, fun() ->
        TestProc = <<"p45HPD-ENkLS7Ykqrx6p_DYGbmeHDeeF8LJ09N2K53g">>,
        EmptyStore = #{
            <<"store-module">> => hb_store_fs,
            <<"prefix">> => <<"cache-TEST">>
        },
        hb_store:reset(EmptyStore),
        hb_http_server:start_node(#{}),
        Opts = #{
            store =>
                [
                    #{
                        <<"store-module">> => hb_store_gateway,
                        <<"store">> => false
                    },
                    EmptyStore
                ],
            cache_control => <<"cache">>
        },
        ?assertMatch(
            {ok, #{ <<"type">> := <<"Process">> }},
            hb_cache:read(TestProc, Opts)
        ),
        % TestProc is an AO Legacynet process: No device tag, so we start by resolving
        % only an explicit key.
        ?assertMatch(
            {ok, <<"Process">>},
            hb_ao:resolve(TestProc, <<"type">>, Opts)
        ),
        % Next, we resolve the schedule key on the message, as a `process@1.0'
        % message.
        {ok, X} =
            hb_ao:resolve(
                {as, <<"process@1.0">>, TestProc},
                <<"schedule">>,
                Opts
            ),
        ?assertMatch(#{ <<"assignments">> := _ }, X)
    end}.

%% @doc Test to verify store opts is being set for Data-Protocol ao
store_opts_test() ->
    Opts = #{
        cache_control => <<"cache">>,
        store =>
            [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"prefix">> => <<"cache-TEST">>
                },
                #{ <<"store-module">> => hb_store_gateway, 
                        <<"store">> => false,
                        <<"subindex">> => [
                        #{
                            <<"name">> => <<"Data-Protocol">>,
                            <<"value">> => <<"ao">>
                        }
                    ]
                }
            ]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, Res} = 
        hb_http:get(
            Node,
            <<"myb2p8_TSM0KSgBMoG-nu6TLuqWwPmdZM5V2QSUeNmM">>,
            #{}
        ),
    ?event(debug_gateway, {res, Res}),
    ?assertEqual(<<"Hello World">>,hb_ao:get(<<"data">>, Res)).