# hb_store_gateway

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store_gateway.erl)

A store module that reads data from the nodes Arweave gateway and 
GraphQL routes, additionally including additional store-specific routes.

---

## Exported Functions

- `list/2`
- `read/2`
- `resolve/2`
- `scope/1`
- `type/2`

---

### scope

A store module that reads data from the nodes Arweave gateway and 
The scope of a GraphQL store is always remote, due to performance.

```erlang
scope(_) -> remote.
```

### resolve

A store module that reads data from the nodes Arweave gateway and 
The scope of a GraphQL store is always remote, due to performance.

```erlang
resolve(_, Key) -> Key.
```

### list

A store module that reads data from the nodes Arweave gateway and 
The scope of a GraphQL store is always remote, due to performance.

```erlang
list(StoreOpts, Key) ->
    ?event(store_gateway, executing_list),
    case read(StoreOpts, Key) of
        not_found -> not_found;
        {ok, Message} -> {ok, hb_maps:keys(Message, StoreOpts)}
    end.
```

### type

Get the type of the data at the given key. We potentially cache the

```erlang
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
```

### read

Read the data at the given key from the GraphQL route. Will only attempt

```erlang
read(BaseStoreOpts, Key) ->
    StoreOpts = opts(BaseStoreOpts),
    case hb_path:term_to_path_parts(Key, StoreOpts) of
        [ID] when ?IS_ID(ID) ->
            ?event({read, StoreOpts, Key}),
            case hb_gateway_client:read(Key, StoreOpts) of
                {error, _} ->
                    ?event(store_gateway, {read_not_found, {key, ID}}),
                    not_found;
                {ok, Message} ->
                    ?event(store_gateway, {read_found, {key, ID}}),
                    try hb_store_remote_node:maybe_cache(StoreOpts, Message)
                    catch _:_ -> ignored end,
                    {ok, Message}
            end;
        _ ->
            ?event({ignoring_non_id, Key}),
            not_found
    end.
```

### opts

Normalize the routes in the given `Opts`.

```erlang
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
```

### graphql_as_store_test_

Store is accessible via the default options.

```erlang
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
```

### graphql_from_cache_test

Stored messages are accessible via `hb_cache` accesses.

```erlang
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
```

### manual_local_cache_test

```erlang
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
```

### cache_read_message_test

Ensure that saving to the gateway store works.

```erlang
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
```

### specific_route_test

Routes can be specified in the options, overriding the default routes.

```erlang
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
```

### external_http_access_test

Test that the default node config allows for data to be accessed.

```erlang
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
```

### store_opts_test

Test to verify store opts is being set for Data-Protocol ao
Test that items retreived from the gateway store are verifiable.

```erlang
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
```

### verifiability_test

Test to verify store opts is being set for Data-Protocol ao
Test that items retreived from the gateway store are verifiable.

```erlang
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
```

### remote_hyperbeam_node_ans104_test

Test that another HyperBEAM node offering the `~query@1.0` device can

```erlang
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
    {ok, Msg2} = hb_cache:read(ID, ClientOpts),
    ?assert(hb_message:verify(Msg2)),
```

---

*Generated from [hb_store_gateway.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store_gateway.erl)*
