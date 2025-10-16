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
    ?event(store_gateway, executing_list),
    case read(StoreOpts, Key) of
        not_found -> not_found;
        {ok, Message} -> {ok, hb_maps:keys(Message, StoreOpts)}
    end.

%% @doc Get the type of the data at the given key. We potentially cache the
%% result, so that we don't have to read the data from the GraphQL route
%% multiple times.
type(StoreOpts, Key) ->
    ?event(store_gateway, executing_type),
    case read(StoreOpts, Key) of
        not_found -> not_found;
        {ok, Data} ->
            ?event({type, hb_private:reset(hb_message:uncommitted(Data, StoreOpts))}),
            IsFlat = lists:all(
                fun({_, Value}) -> not is_map(Value) end,
                hb_maps:to_list(
                    hb_private:reset(
                        hb_message:uncommitted(Data, StoreOpts)
                    ),
                    StoreOpts
                )
            ),
            if
                IsFlat -> simple;
                true -> composite
            end
    end.

%% @doc Read the data at the given key from the GraphQL route. Will only attempt
%% to read the data if the key is an ID.
read(BaseStoreOpts, Key) ->
    StoreOpts = opts(BaseStoreOpts),
    case hb_path:term_to_path_parts(Key, StoreOpts) of
        [ID|Rest] when ?IS_ID(ID) ->
            ?event({gateway_read, {opts, StoreOpts}, {id, ID}, {subpath, Rest}}),
            case hb_gateway_client:read(ID, StoreOpts) of
                {error, _} ->
                    ?event({read_not_found, {key, ID}}),
                    not_found;
                {ok, Message} ->
                    ?event({read_found, {key, ID}}),
                    try hb_store_remote_node:maybe_cache(StoreOpts, Message)
                    catch _:_ -> ignored end,
                    case Rest of
                        [] -> {ok, Message};
                        _ ->
                            case hb_util:deep_get(Rest, Message, StoreOpts) of
                                not_found -> not_found;
                                Value -> {ok, Value}
                            end
                    end
            end;
        _ ->
            ?event({ignoring_non_id, Key}),
            not_found
    end.

%% @doc Normalize the routes in the given `Opts`.
opts(Opts) ->
    case hb_maps:find(<<"node">>, Opts) of
        error -> Opts;
        {ok, Node} ->
            case hb_maps:get(<<"node-type">>, Opts, <<"arweave">>, Opts) of
                <<"arweave">> ->
                    Opts#{
                        routes => [
                            #{
                                % Routes for GraphQL requests to use the remote
                                % server's GraphQL API.
                                <<"template">> => <<"/graphql">>,
                                <<"nodes">> => [#{ <<"prefix">> => Node }]
                            },
                            #{
                                <<"template">> => <<"/raw">>,
                                <<"nodes">> => [#{ <<"prefix">> => Node }]
                            }
                        ]
                    };
                <<"ao">> ->
                    Opts#{
                        routes => [
                            #{
                                <<"template">> => <<"/graphql">>,
                                <<"nodes">> =>
                                    [
                                        #{
                                            <<"prefix">> =>
                                                <<Node/binary, "/~query@1.0">>
                                        }
                                    ]
                            },
                            #{
                                <<"template">> => <<"/raw">>,
                                <<"nodes">> =>
                                [
                                    #{
                                        <<"match">> => <<"^/raw">>,
                                        <<"with">> => Node
                                    }
                                ]
                            }
                        ]
                    }
            end
    end.

%%% Tests

%% @doc Store is accessible via the default options.
graphql_as_store_test_() ->
    hb_http_server:start_node(#{}),
	{timeout, 10, fun() ->
		hb_http_server:start_node(#{}),
		?assertMatch(
			{ok, #{ <<"app-name">> := <<"aos">> }},
			hb_store:read(
				[#{ <<"store-module">> => hb_store_gateway }],
				<<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>
			)
		)
	end}.

%% @doc Stored messages are accessible via `hb_cache' accesses.
graphql_from_cache_test() ->
    hb_http_server:start_node(#{}),
    Opts =
        #{
            store =>
                [
                    #{
                        <<"store-module">> => hb_store_gateway
                    }
                ]
        },
    ?assertMatch(
        {ok, #{ <<"app-name">> := <<"aos">> }},
        hb_cache:read(
            <<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>,
            Opts
        )
    ).

manual_local_cache_test() ->
    hb_http_server:start_node(#{}),
    Local = #{
        <<"store-module">> => hb_store_fs,
        <<"name">> => <<"cache-TEST/gw-local-cache">>
    },
    hb_store:reset(Local),
    Gateway = #{
        <<"store-module">> => hb_store_gateway,
        <<"local-store">> => Local
    },
    {ok, FromRemote} =
        hb_cache:read(
            <<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>,
            #{ store => [Gateway] }
        ),
    ?event({writing_recvd_to_local, FromRemote}),
    {ok, _} = hb_cache:write(FromRemote, #{ store => [Local] }),
    {ok, Read} =
        hb_cache:read(
            <<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>,
            #{ store => [Local] }
        ),
    ?event({read_from_local, Read}),
    ?assert(hb_message:match(Read, FromRemote)).

%% @doc Ensure that saving to the gateway store works.
cache_read_message_test() ->
    hb_http_server:start_node(#{}),
    Local = #{
        <<"store-module">> => hb_store_fs,
        <<"name">> => <<"cache-TEST/1">>
    },
    hb_store:reset(Local),
    WriteOpts = #{
        store =>
            [
                #{ <<"store-module">> => hb_store_gateway,
                    <<"local-store">> => [Local]
                }
            ]
    },
    {ok, Written} =
        hb_cache:read(
            <<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>,
            WriteOpts
        ),
    {ok, Read} =
        hb_cache:read(
            <<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>,
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
            <<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>,
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
                    #{
                        <<"store-module">> => hb_store_fs,
                        <<"name">> => <<"cache-TEST">>
                    },
                    #{ <<"store-module">> => hb_store_gateway }
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
% resolve_on_gateway_test_() ->
%     {timeout, 10, fun() ->
%         TestProc = <<"p45HPD-ENkLS7Ykqrx6p_DYGbmeHDeeF8LJ09N2K53g">>,
%         EmptyStore = #{
%             <<"store-module">> => hb_store_fs,
%             <<"name">> => <<"cache-TEST">>
%         },
%         hb_store:reset(EmptyStore),
%         hb_http_server:start_node(#{}),
%         Opts = #{
%             store =>
%                 [
%                     #{
%                         <<"store-module">> => hb_store_gateway,
%                         <<"store">> => false
%                     },
%                     EmptyStore
%                 ],
%             cache_control => <<"cache">>
%         },
%         ?assertMatch(
%             {ok, #{ <<"type">> := <<"Process">> }},
%             hb_cache:read(TestProc, Opts)
%         ),
%         % TestProc is an AO Legacynet process: No device tag, so we start by resolving
%         % only an explicit key.
%         ?assertMatch(
%             {ok, <<"Process">>},
%             hb_ao:resolve(TestProc, <<"type">>, Opts)
%         ),
%         % Next, we resolve the schedule key on the message, as a `process@1.0'
%         % message.
%         {ok, X} =
%             hb_ao:resolve(
%                 {as, <<"process@1.0">>, TestProc},
%                 <<"schedule">>,
%                 Opts
%             ),
%         ?assertMatch(#{ <<"assignments">> := _ }, X)
%     end}.

%% @doc Test to verify store opts is being set for Data-Protocol ao
store_opts_test() ->
    Opts = #{
        cache_control => <<"cache">>,
        store =>
            [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-TEST">>
                },
                #{
                    <<"store-module">> => hb_store_gateway, 
                    <<"local-store">> => false,
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
    ?assertEqual(<<"Hello World">>, hb_ao:get(<<"data">>, Res)).

%% @doc Test that items retreived from the gateway store are verifiable.
verifiability_test() ->
    hb_http_server:start_node(#{}),
    {ok, Message} =
        hb_cache:read(
            <<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>,
            #{
                store =>
                    [
                        #{
                            <<"store-module">> => hb_store_gateway
                        }
                    ]
            }
        ),
    % Ensure that the message is verifiable after being converted to 
    % httpsig@1.0 and back to structured@1.0.
    HTTPSig =
        hb_message:convert(
            Message,
            <<"httpsig@1.0">>,
            <<"structured@1.0">>,
            #{}
        ),
    ?assert(hb_message:verify(HTTPSig)),
    Structured =
        hb_message:convert(
            HTTPSig,
            <<"structured@1.0">>,
            <<"httpsig@1.0">>,
            #{}
        ),
    ?event({verifying, {structured, Structured}, {original, Message}}),
    ?assert(hb_message:verify(Structured)).

%% @doc Test that another HyperBEAM node offering the `~query@1.0' device can
%% be used as a store.
remote_hyperbeam_node_ans104_test() ->
    ServerOpts =
        #{
            priv_wallet => ar_wallet:new(),
            store => hb_test_utils:test_store()
        },
    Server = hb_http_server:start_node(ServerOpts),
    Msg =
        hb_message:commit(
            #{
                <<"hello">> => <<"world">>
            },
            ServerOpts,
            #{ <<"commitment-device">> => <<"ans104@1.0">> }
        ),
    {ok, ID} = hb_cache:write(Msg, ServerOpts),
    {ok, ReadMsg} = hb_cache:read(ID, ServerOpts),
    ?assert(hb_message:verify(ReadMsg)),
    LocalStore = hb_test_utils:test_store(),
    ClientOpts =
        #{
            store =>
                [
                    #{
                        <<"store-module">> => hb_store_gateway,
                        <<"node">> => Server,
                        <<"node-type">> => <<"ao">>,
                        <<"local-store">> => [LocalStore]
                    }
                ]
        },
    {ok, Req} = hb_cache:read(ID, ClientOpts),
    ?assert(hb_message:verify(Req)),
    ?assert(hb_message:match(Msg, Req)).