# dev_cache

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_cache.erl)

A device that looks up an ID from a local store and returns it,
honoring the `accept` key to return the correct format. The cache also
supports writing messages to the store, if the node message has the
writer's address in its `cache_writers` key.

---

## Exported Functions

- `link/3`
- `read/3`
- `write/3`

---

### read

A device that looks up an ID from a local store and returns it,
Read data from the cache.

```erlang
read(_M1, M2, Opts) ->
    Location = hb_ao:get(<<"target">>, M2, Opts),
    ?event({read, {key_extracted, Location}}),
    ?event(debug_gateway, cache_read),
    case hb_cache:read(Location, Opts) of
        {ok, Res} ->
            ?event({read, {cache_result, ok, Res}}),
            case hb_ao:get(<<"accept">>, M2, Opts) of
                <<"application/aos-2">> ->
                    ?event(dev_cache, 
						{read, 
							{accept_header, <<"application/aos-2">>}
						}
					),
                    JSONMsg = dev_json_iface:message_to_json_struct(Res, Opts),
                    ?event(dev_cache, {read, {json_message, JSONMsg}}),
                    {ok,
                        #{
                            <<"body">> => hb_json:encode(JSONMsg),
                            <<"content-type">> => <<"application/aos-2">>
                        }
					};
                _ ->
                    {ok, Res}
            end;
        not_found ->
            % The cache does not have this ID,but it may still be an explicit
            % `data/' path.
```

### write

Write data to the cache.

```erlang
write(_M1, M2, Opts) ->
    case is_trusted_writer(M2, Opts) of
        true ->
            ?event(dev_cache, {write, {trusted_writer, true}}),
            Type = hb_ao:get(<<"type">>, M2, <<"single">>, Opts),
            ?event(dev_cache, {write, {write_type, Type}}),
            case Type of
                <<"single">> ->
                    ?event(dev_cache, {write, {write_single_called}}),
                    write_single(M2, Opts);
                <<"batch">> ->
                    ?event(dev_cache, {write, {write_batch_called}}),
                    hb_maps:map(
                        fun(_, Value) ->
                            ?event(dev_cache, {write, {batch_item, Value}}),
                            write_single(Value, Opts)
                        end,
                        hb_ao:get(<<"body">>, M2, Opts),
                        Opts
                    );
                _ ->
                    ?event(dev_cache, {write, {invalid_write_type, Type}}),
                    {error,
                        #{
                            <<"status">> => 400,
                            <<"body">> => <<"Invalid write type.">>
                        }
                    }
            end;
        false ->
            ?event(dev_cache, {write, {trusted_writer, false}}),
            {error,
                #{
                    <<"status">> => 403,
                    <<"body">> => <<"Not authorized to write to the cache.">>
                }
            }
    end.
```

### link

Link a source to a destination in the cache.

```erlang
link(_Base, Req, Opts) ->
    case is_trusted_writer(Req, Opts) of
        true ->
            Source = hb_ao:get(<<"source">>, Req, Opts),
            Destination = hb_ao:get(<<"destination">>, Req, Opts),
            write_single(#{
                <<"operation">> => <<"link">>,
                <<"source">> => Source,
                <<"destination">> => Destination
            }, Opts);
        false ->
            {error, not_authorized}
    end.
```

### write_single

Helper function to write a single data item to the cache.

```erlang
write_single(Msg, Opts) ->
    Body = hb_ao:get(<<"body">>, Msg, Opts),
    ?event(dev_cache, {write_single, {body_extracted, Body}}),
    Location = hb_ao:get(<<"location">>, Msg, Opts),
    ?event(dev_cache, {write_single, {location_extracted, Location}}),
    Operation = hb_ao:get(<<"operation">>, Msg, <<"write">>, Opts),
    ?event(dev_cache, {write_single, {operation, Operation}}),
    case {Operation, Body, Location} of
        {<<"write">>, not_found, _} ->
            ?event(dev_cache, {write_single, {error, "No body to write"}}),
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"No body to write.">>
                }
            };
        {<<"write">>, Binary, not_found} when is_binary(Binary) ->
            % When asked to write only a binary, we do not calculate any
            % alternative IDs.
```

### is_trusted_writer

Verify that the request originates from a trusted writer.

```erlang
is_trusted_writer(Req, Opts) ->
    Signers = hb_message:signers(Req, Opts),
    ?event(dev_cache, {is_trusted_writer, {signers, Signers}, {req, Req}}),
    CacheWriters = hb_opts:get(cache_writers, [], Opts),
    ?event(dev_cache, {is_trusted_writer, {cache_writers, CacheWriters}}),
    AnyTrusted = lists:any(fun(Signer) -> lists:member(Signer, CacheWriters) end, Signers),
    case AnyTrusted of
        true ->
            ?event(dev_cache, {is_trusted_writer, {trusted, true}}),
            true;
        _ ->
            ?event(dev_cache, {is_trusted_writer, {trusted, false}}),
            false
    end.
```

### setup_test_env

Create a test environment with a local store and node.

```erlang
setup_test_env() ->
    Timestamp = integer_to_binary(os:system_time(millisecond)),
    StorePrefix = <<"cache-TEST/remote-", Timestamp/binary>>,
    ?event(dev_cache, {setup_test_env, {start, StorePrefix}}),
    application:ensure_all_started(hb),
    ?event(dev_cache, {setup_test_env, {hb_started}}),
    LocalStore = 
		#{ <<"store-module">> => hb_store_fs, <<"name">> => StorePrefix },
    ?event(dev_cache, {setup_test_env, {local_store_configured, LocalStore}}),
    hb_store:reset(LocalStore),
    ?event(dev_cache, {setup_test_env, {store_reset}}),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    ?event(dev_cache, {setup_test_env, {address, Address}}),
    Node = hb_http_server:start_node(#{ 
        cache_control => [<<"no-cache">>, <<"no-store">>],
        store => LocalStore,
        cache_writers => [
			Address,
			hb_util:human_id(ar_wallet:to_address(hb:wallet()))
		],
        store_all_signed => false
    }),
    ?event(dev_cache, {setup_test_env, {node_started, Node}}),
    TestOpts = #{
        cache_control => [<<"no-cache">>, <<"no-store">>],
        store_all_signed => false,
        store => [
            #{
                <<"store-module">> => hb_store_remote_node,
                <<"node">> => Node,
                priv_wallet => Wallet
            }
	    ]
    },
    {ok, TestOpts, [LocalStore, Wallet, Address, Node]}.
```

### write_to_cache

Write data to the cache via HTTP.

```erlang
write_to_cache(Node, Data, Wallet) ->
    ?event(dev_cache, {write_to_cache, {start, Node}}),
    WriteMsg = #{
        <<"path">> => <<"/~cache@1.0/write">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Data
    },
    ?event(dev_cache, {write_to_cache, {message_created, WriteMsg}}),
    SignedMsg = hb_message:commit(WriteMsg, Wallet),
    ?event(dev_cache, {write_to_cache, {message_signed}}),
    WriteResult = hb_http:post(Node, SignedMsg, #{}),
    ?event(dev_cache, {write_to_cache, {http_post, WriteResult}}),
    {ok, WriteResponse} = WriteResult,
    ?event(dev_cache, {write_to_cache, {response_received, WriteResponse}}),
    Status = hb_ao:get(<<"status">>, WriteResponse, 0, #{}),
    ?assertEqual(200, Status),
    Path = hb_ao:get(<<"path">>, WriteResponse, not_found, #{}),
    ?assertNotEqual(not_found, Path),
    ?event(dev_cache, {write_to_cache, {write_success, Path}}),
    {WriteResponse, Path}.
```

### read_from_cache

Read data from the cache via HTTP.

```erlang
read_from_cache(Node, Path) ->
    ?event(dev_cache, {read_from_cache, {start, Node, Path}}),
    ReadMsg = #{
        <<"path">> => <<"/~cache@1.0/read">>,
        <<"method">> => <<"GET">>,
        <<"target">> => Path
    },
    ?event(dev_cache, {read_from_cache, {request_created, ReadMsg}}),
    ?event({test_read, request, ReadMsg}),
    ReadResult = hb_http:get(Node, ReadMsg, #{}),
    ?event(dev_cache, {read_from_cache, {http_get, ReadResult}}),
    case ReadResult of
        ReadResponse when is_binary(ReadResponse) ->
            ?event(dev_cache, 
				{read_from_cache, 
					{response_binary, ReadResponse}
				}
			),
            ReadResponse;
        {ok, ReadResponse} ->
            ?event(dev_cache, {read_from_cache, {response_ok, ReadResponse}}),
            ReadResponse;
        {error, Reason} ->
            ?event(dev_cache, {read_from_cache, {response_error, Reason}}),
            {error, Reason}
    end.
```

### cache_write_message_test

Test that the cache can be written to and read from using the hb_cache

```erlang
cache_write_message_test() ->
    ?event(dev_cache, {cache_api_test, {start}}),
    {ok, Opts, _} = setup_test_env(),
    TestData = #{
        <<"test_key">> => <<"test_value">>
    },
    ?event(dev_cache, {cache_api_test, {opts, Opts}}),
    {ok, Path} = hb_cache:write(TestData, Opts),
    ?event(dev_cache, {cache_api_test, {data_written, Path}}),
    {ok, ReadData} = hb_cache:read(Path, Opts),
    ?event(dev_cache, {cache_api_test, {data_read, ReadData}}),
    ?assert(hb_message:match(TestData, ReadData, only_present, Opts)),
    ?event(dev_cache, {cache_api_test}),
    ok.
```

### cache_write_binary_test

Ensure that we can write direct binaries to the cache.

```erlang
cache_write_binary_test() ->
    ?event(dev_cache, {cache_api_test, {start}}),
    {ok, Opts, _} = setup_test_env(),
    TestData = <<"test_binary">>,
    {ok, Path} = hb_cache:write(TestData, Opts),
    {ok, ReadData} = hb_cache:read(Path, Opts),
    ?event(dev_cache, {cache_api_test, {data_read, ReadData}}),
    ?assertEqual(TestData, ReadData),
    ?event(dev_cache, {cache_api_test}),
```

---

*Generated from [dev_cache.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_cache.erl)*
