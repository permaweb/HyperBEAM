%%% @doc A device that looks up an ID from a local store and returns it,
%%% honoring the `accept' key to return the correct format. The cache also
%%% supports writing messages to the store, if the node message has the
%%% writer's address in its `cache_writers' key.
-module(dev_cache).
-export([read/3, write/3, link/3, group/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Read data from the cache.
%% Retrieves data corresponding to a key from a local store.
%% The key is extracted from the incoming message under &lt;&lt;"read"&gt;&gt;.
%% The options map may include store configuration.
%% If the "accept" header is set to &lt;&lt;"application/aos-2"&gt;&gt;, the result is 
%% converted to a JSON structure and encoded.
%%
%% @param M1 Ignored parameter.
%% @param M2 The request message containing the key and an optional "accept"
%%            header.
%% @param Opts A map of configuration options.
%% @returns {ok, Data} on success,
%%          {error, not_found} if the key does not exist,
%%          {error, Reason} or {failure, Reason} on failure.
read(_M1, M2, Opts) ->
    Location = hb_ao:get(<<"read">>, M2, Opts),
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
                    {ok, JSONMsg} =
                        hb_ao:resolve(
                            #{ <<"device">> => <<"json-iface@1.0">> },
                            #{
                                <<"path">> => <<"to">>,
                                <<"message">> => Res
                            },
                            Opts
                        ),
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
        {error, not_found} ->
            % The cache does not have this ID,but it may still be an explicit
            % `data/' path.
            % Store = hb_opts:get(store, [], Opts),
            Store = hb_opts:get(store, no_viable_store, Opts),
            ?event(dev_cache, {read, {location, Location}, {store, Store}}),
            hb_store:read(Store, Location, Opts);
        {error, _} = Error ->
            Error;
        {failure, _} = Failure ->
            Failure
    end.

%% @doc Write data to the cache.
%% Processes a write request by first verifying that the request comes from a
%% trusted writer (as defined by the `cache_writers' configuration in the
%% options). Single writes accept a binary body for direct cache insertion or a
%% map body for direct store writes. Batch writes iterate over the items in the
%% body and apply the same transformation to each value.
%%
%% @param M1 Ignored parameter.
%% @param M2 The request message containing the data to write, the write type,
%%            and any additional parameters.
%% @param Opts A map of configuration options.
%% @returns {ok, Path} on success, where Path indicates where the data was
%%          stored, {error, Reason} or {failure, Reason} on failure.
write(_M1, M2, Opts) ->
    case is_trusted_writer(M2, Opts) of
        true ->
            ?event(dev_cache, {write, {trusted_writer, true}}),
            Body = hb_ao:get(<<"body">>, M2, not_found, Opts),
            Type =
                case hb_maps:get(<<"type">>, M2, <<"single">>, Opts) of
                    <<"batch">> -> <<"batch">>;
                    _ -> <<"single">>
                end,
            ?event(dev_cache, {write, {write_type, Type}}),
            case Type of
                <<"single">> ->
                    ?event(dev_cache, {write, {write_single_called}}),
                    write_single(Body, Opts);
                <<"batch">> ->
                    ?event(dev_cache, {write, {write_batch_called}}),
                    case Body of
                        Batch when is_map(Batch) ->
                            hb_maps:map(
                                fun(_, Value) ->
                                    ?event(dev_cache, {write, {batch_item, Value}}),
                                    write_single(Value, Opts)
                                end,
                                Batch,
                                Opts
                            );
                        _ ->
                            {error,
                                #{
                                    <<"status">> => 400,
                                    <<"body">> => <<"Invalid write type.">>
                                }
                            }
                    end;
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

%% @doc Link a source to a destination in the cache.
link(_Base, Req, Opts) ->
    case is_trusted_writer(Req, Opts) of
        true ->
            Destination = hb_ao:get(<<"destination">>, Req, Opts),
            Source = hb_ao:get(<<"source">>, Req, Opts),
            wrap_store_result(hb_store:link(#{ Destination => Source }, Opts));
        false ->
            {error, not_authorized}
    end.

group(_Base, Req, Opts) ->
    case is_trusted_writer(Req, Opts) of
        true ->
            wrap_store_result(
                hb_store:group(
                    #{ <<"group">> => hb_ao:get(<<"group">>, Req, Opts) },
                    Opts
                )
            );
        false ->
            {error, not_authorized}
    end.

%% @doc Helper function to write a single data item to the cache.
%% Writes store-shaped request maps directly to the store layer, or stores
%% direct binaries in the cache and returns their derived path.
%%
%% @param Body The data to be written.
%% @param Opts A map of configuration options.
%% @returns {ok, #{status := 200, path := Path}} on success,
%%          {error, Reason} on failure.
write_single(Body, Opts) ->
    ?event(dev_cache, {write_single, {body, Body}}),
    case Body of
        not_found ->
            ?event(dev_cache, {write_single, {error, "No body to write"}}),
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"No body to write.">>
                }
            };
        Binary when is_binary(Binary) ->
            ?event(dev_cache, {write_single, {processing_binary, Binary}}),
            {ok, Path} = hb_cache:write(Binary, Opts),
            ?event(dev_cache, {write_single, {binary_written, Path}}),
            {ok, #{ <<"status">> => 200, <<"path">> => Path }};
        Req when is_map(Req) ->
            wrap_store_result(hb_store:write(Req, Opts));
        _Other ->
            ?event(dev_cache, {write_single, {error, <<"Invalid write type">>}}),
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Invalid write type.">>
                }
            }
    end.

wrap_store_result(ok) ->
    {ok, #{ <<"status">> => 200 }};
wrap_store_result(OtherResult) ->
    OtherResult.

%% @doc Verify that the request originates from a trusted writer.
%% Checks that the single signer of the request is present in the list
%% of trusted cache writer addresses specified in the options.
%%
%% @param Req The request message.
%% @param Opts A map of configuration options.
%% @returns true if the request is from an authorized writer, false
%%          otherwise.
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

%%%--------------------------------------------------------------------
%%% Test Helpers
%%%--------------------------------------------------------------------

%% @doc Create a test environment with a local store and node.
%% Ensures that the required application is started, configures a local
%% file-system store, resets the store for a clean state, creates a wallet
%% for signing requests, and starts a node with the store and trusted cache
%% writer configuration.
%%
%% @param StorePrefix A binary specifying the prefix for the local store.
%% @returns {ok, TestOpts, [LocalStore, Wallet, Address, Node]}
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
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"store">> => LocalStore,
        <<"cache-writers">> => [
			Address,
			hb_util:human_id(ar_wallet:to_address(hb:wallet()))
		],
        <<"store-all-signed">> => false
    }),
    ?event(dev_cache, {setup_test_env, {node_started, Node}}),
    TestOpts = #{
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"store-all-signed">> => false,
        <<"store">> => [
            #{
                <<"store-module">> => hb_store_remote_node,
                <<"node">> => Node,
                <<"priv-wallet">> => Wallet
            }
	    ]
    },
    {ok, TestOpts, [LocalStore, Wallet, Address, Node]}.

%%%--------------------------------------------------------------------
%%% Tests
%%%--------------------------------------------------------------------

%% @doc Test that the cache can be written to and read from using the hb_cache
%% API.
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

%% @doc Ensure that we can write direct binaries to the cache.
cache_write_binary_test() ->
    ?event(dev_cache, {cache_api_test, {start}}),
    {ok, Opts, _} = setup_test_env(),
    TestData = <<"test_binary">>,
    {ok, Path} = hb_cache:write(TestData, Opts),
    {ok, ReadData} = hb_cache:read(Path, Opts),
    ?event(dev_cache, {cache_api_test, {data_read, ReadData}}),
    ?assertEqual(TestData, ReadData),
    ?event(dev_cache, {cache_api_test}),
    ok.
