%%% @doc A device that looks up an ID from a local store and returns it,
%%% honoring the `accept' key to return the correct format. The cache also
%%% supports writing messages to the store when the node operator has signed
%%% the corresponding cache operation type.
-module(dev_cache).
-device_libraries([lib_meta, lib_process]).
-export([read/3, write/3]).
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
-spec read(
    #{ _ => _ },
    #{ read := binary(), accept => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, _} | {error, _} | {failure, _}.
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
                _ when is_list(Res) ->
                    % The reply's `ao-result' is the list, as it was written.
                    {ok, #{ <<"ao-result">> => <<"body">>, <<"body">> => Res }};
                _ ->
                    {ok, Res}
            end;
        {error, not_found} ->
            % The cache does not have this ID, but the store may have a value at
            % the path.
            % Store = hb_opts:get(store, [], Opts),
            Store = hb_opts:get(store, no_viable_store, Opts),
            ?event(dev_cache, {read, {location, Location}, {store, Store}}),
            hb_store:read(Store, Location, Opts);
        {error, _} = Error ->
            Error;
        {failure, _} = Failure ->
            Failure
    end.

%% @doc Write a binary or a message on behalf of the node operator.
%% The request must sign `type: cache-write'. Set `write-type: batch' to write
%% each value in a map body separately.
write(_Base, Req, Opts) ->
    maybe
        {ok, Authorized} ?=
            lib_meta:is_authorized(<<"cache-write">>, operator, Req, Opts),
        Body = hb_maps:get(<<"body">>, Authorized, not_found, Opts),
        case hb_maps:get(<<"write-type">>, Authorized, <<"single">>, Opts) of
            <<"single">> -> write_single(Body, Opts);
            <<"batch">> when is_map(Body) ->
                hb_maps:map(
                    fun(_, Value) -> write_single(Value, Opts) end,
                    Body,
                    Opts
                );
            _ ->
                {error,
                    #{ <<"status">> => 400, <<"body">> => <<"Invalid write type.">> }
                }
        end
    end.

%% @doc Helper function to write a single data item to the cache.
%% Writes a binary or a message to the cache, and returns its path: the `ni:'
%% name of a binary, or the unsigned ID of a message.
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
        Value when is_binary(Value); is_map(Value); is_list(Value) ->
            ?event(dev_cache, {write_single, {processing_value, Value}}),
            {ok, Path} = hb_cache:write(Value, Opts),
            ?event(dev_cache, {write_single, {value_written, Path}}),
            {ok, #{ <<"status">> => 200, <<"path">> => Path }};
        _Other ->
            ?event(dev_cache, {write_single, {error, <<"Invalid write type">>}}),
            {error,
                #{
                    <<"status">> => 400,
                    <<"body">> => <<"Invalid write type.">>
                }
            }
    end.

%%%--------------------------------------------------------------------
%%% Test Helpers
%%%--------------------------------------------------------------------

%% @doc Create a test environment with a local store and node.
%% Ensures that the required application is started, configures a local
%% file-system store, resets the store for a clean state, creates a wallet
%% for signing requests, and starts a node with the store and operator settings.
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
    Address = hb_test_utils:committer(Wallet),
    ?event(dev_cache, {setup_test_env, {address, Address}}),
    Node = hb_http_server:start_node(#{ 
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"store">> => LocalStore,
        <<"operator">> => Address,
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
                <<"priv-wallet">> => Wallet,
                % The tests read back the `ni:' path of a binary they write.
                <<"trusted">> => true
            }
	    ]
    },
    {ok, TestOpts, [LocalStore, Wallet, Address, Node]}.

%%%--------------------------------------------------------------------
%%% Tests
%%%--------------------------------------------------------------------

%% @doc Ensure that a cache write requires its own signed type.
typed_cache_mutations_test() ->
    Wallet = ar_wallet:new(),
    Store = hb_test_utils:test_store(),
    Opts = #{ <<"priv-wallet">> => Wallet, <<"store">> => Store,
        <<"http-only-result">> => false },
    Node = hb_http_server:start_node(Opts),
    Signed =
        hb_message:commit(
            #{
                <<"type">> => <<"node-message">>,
                <<"body">> => <<"wrong-type">>
            },
            Opts
        ),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        hb_http:post(
            Node,
            Signed#{ <<"path">> => <<"/~cache@1.0/write">> },
            Opts
        )
    ).

%% @doc Ensure that the operator can write direct binaries to the cache.
cache_write_binary_test() ->
    ?event(dev_cache, {cache_api_test, {start}}),
    {ok, Opts, [_LocalStore, Wallet, _Address, Node]} = setup_test_env(),
    TestData = <<"test_binary">>,
    Write =
        hb_message:commit(
            #{
                <<"path">> => <<"/~cache@1.0/write">>,
                <<"type">> => <<"cache-write">>,
                <<"method">> => <<"POST">>,
                <<"body">> => TestData
            },
            #{ <<"priv-wallet">> => Wallet }
        ),
    {ok, Written} = hb_http:post(Node, Write, Opts),
    Path = hb_ao:get(<<"path">>, Written, Opts),
    {ok, ReadData} = hb_cache:read(Path, Opts),
    ?event(dev_cache, {cache_api_test, {data_read, ReadData}}),
    ?assertEqual(TestData, ReadData),
    % A message with a nested message reads back by the ID its write returns.
    Msg = #{ <<"child">> => #{ <<"key">> => <<"value">> } },
    {ok, #{ <<"path">> := ID }} =
        hb_http:post(
            Node,
            hb_message:commit(
                (hb_message:uncommitted(Write))#{ <<"body">> => Msg },
                #{ <<"priv-wallet">> => Wallet }
            ),
            Opts
        ),
    ?assertEqual(
        {ok, <<"value">>},
        hb_http:get(Node, <<"/", ID/binary, "/child/key">>, Opts)
    ),
    ?event(dev_cache, {cache_api_test}),
    ok.
