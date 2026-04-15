%%% @doc A store module that reads data from another AO node.
%%% Notably, this store only provides the _read_ side of the store interface.
%%% The write side could be added, returning an commitment that the data has
%%% been written to the remote node. In that case, the node would probably want
%%% to upload it to an Arweave bundler to ensure persistence, too.
-module(hb_store_remote_node).
-export([scope/1, type/2, read/2, write/3, make_link/3, make_group/2, resolve/2]).
%%% Public utilities.
-export([maybe_cache/2, maybe_cache/3, read_local_cache/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Return the scope of this store.
%%
%% For the remote store, the scope is always `remote'.
%%
%% @param StoreOpts A message with the store options (ignored).
%% @returns remote.
scope(_StoreOpts) ->
    remote.

%% @doc Resolve a key path in the remote store.
%%
%% For the remote node store, the key is returned as-is.
%%
%% @param Data A map containing node configuration.
%% @param Key The key to resolve.
%% @returns The resolved key.
resolve(#{ <<"node">> := Node }, Key) ->
    ?event({remote_resolve, {node, Node}, {key, Key}}),
    Key;
resolve(#{ <<"nodes">> := Nodes }, Key) ->
    ?event({remote_resolve, {nodes, Nodes}, {key, Key}}),
    Key.

%% @doc Determine the type of value at a given key.
%%
%% Remote nodes support only the `simple' type or `not_found'.
%%
%% @param Opts A map of options (including node configuration).
%% @param Key The key whose value type is determined.
%% @returns simple if found, or not_found otherwise.
type(Opts, Key) when is_map_key(<<"node">>, Opts); is_map_key(<<"nodes">>, Opts) ->
    ?event({remote_type, {opts, Opts}, {key, Key}}),
    case read(Opts, Key) of
        not_found -> not_found;
        _ -> simple
    end.

%% @doc Read a key from the remote node.
%%
%% Makes an HTTP GET request to the remote node and returns the
%% committed message.
%%
%% @param Opts A map of options (including node configuration).
%% @param Key The key to read.
%% @returns {ok, Msg} on success or not_found if the key is missing.
read(#{ <<"only-ids">> := true }, Key) when not ?IS_ID(Key) ->
    not_found;
read(Opts = #{ <<"node">> := Node }, Key) ->
    OptsWithoutNode = maps:remove(<<"node">>, Opts),
    read(OptsWithoutNode#{ <<"nodes">> => [#{ <<"prefix">> => Node }] }, Key);
read(StoreOpts = #{ <<"nodes">> := Nodes }, Key) ->
    MultirequestDirectives =
        maps:filter(
            fun(<<"multirequest-", _/binary>>, _) -> true; (_, _) -> false end,
            StoreOpts
        ),
    ?event(
        {read,
            {nodes, Nodes},
            {key, Key},
            {multirequest_directives, MultirequestDirectives}
        }
    ),
    HTTPReq =
        maps:merge(
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"/~cache@1.0/read?target=", Key/binary>>,
                <<"multirequest-responses">> => 1,
                <<"multirequest-stop-after">> => true,
                <<"multirequest-admissible">> => #{
                    <<"device">> => <<"cache@1.0">>,
                    <<"path">> => <<"expected-response">>,
                    <<"expected">> => Key
                }
            },
            MultirequestDirectives
        ),
    % TODO: When `opts` key normalization lands, we should re-work this.
    MaybeHooks =
        case maps:find(<<"on">>, StoreOpts) of
            {ok, Hooks} -> #{ on => Hooks };
            error -> #{}
        end,
    ?event({remote_read, {request, HTTPReq}, {hooks, MaybeHooks}}),
    HTTPRes =
        hb_http:request(
            HTTPReq,
            MaybeHooks#{
                cache_control => [<<"no-cache">>, <<"no-store">>],
                routes =>
                    [
                        #{
                            <<"template">> => <<"/~cache@1.0/read">>,
                            <<"nodes">> => Nodes,
                            <<"parallel">> => true
                        }
                    ]
            }
        ),
    handle_read_response(Key, HTTPRes, StoreOpts).

%% @doc Handle a read response from the remote node, filtering the raw response
%% and invoking a possible local cache write operation.
handle_read_response(Key, {ok, Res}, StoreOpts) ->
    {ok, Msg} = hb_message:with_only_committed(Res, StoreOpts),
    ?event(
        debug_admissible,
        {remote_read, {only_committed, Msg}, {raw_response, Res}}
    ),
    maybe_cache(StoreOpts, Msg, [Key]),
    {ok, Msg};
handle_read_response(Key, UnexpectedRes, _StoreOpts) ->
    ?event(
        debug_admissible,
        {read_failed, {key, Key}, {unexpected_response, UnexpectedRes}}
    ),
    not_found.

%% @doc Cache the data if the cache is enabled. The `local-store' option may
%% either be `false' or a store definition to use as the local cache. Additional
%% paths may be provided that should be linked to the data.
maybe_cache(StoreOpts, Data) ->
    maybe_cache(StoreOpts, Data, []).
maybe_cache(StoreOpts, Data, Links) ->
    ?event({maybe_cache, StoreOpts, Data}),
    try
        % Check if the local store is in our store options.
        case hb_maps:get(<<"local-store">>, StoreOpts, false, StoreOpts) of
            false ->
                skipped;
            Store ->
                case hb_cache:write(Data, #{ store => Store }) of
                    {ok, RootPath} ->
                        % Remove the base path from the links.
                        LinksWithoutRootPath =
                            lists:filter(
                                fun(Link) -> Link /= RootPath end,
                                Links
                            ),
                        ?event(store_remote_node, cached_received),
                        LinkResults =
                            lists:filter(
                                fun(Link) ->
                                    hb_store:make_link(Store, RootPath, Link) == false
                                end,
                                LinksWithoutRootPath
                            ),
                        ?event(store_remote_node,
                            {linked_cached,
                                {failed_links, LinkResults}
                            }
                        ),
                        case LinkResults of
                            [] -> ok;
                            _ -> {failed_links, LinkResults}
                        end;
                    {error, Err} ->
                        ?event(store_remote_node, error_on_local_cache_write),
                        ?event(warning, {error_caching_remote_node_data, Err}),
                        {error, Err}
                end
        end
    catch _:_ ->
        ignored
    end.

%% @doc Read local store cached value.
read_local_cache(StoreOpts, ID) ->
    ?event({read_local_cache, StoreOpts, ID}),
    case hb_maps:get(<<"local-store">>, StoreOpts, false, StoreOpts) of
        false -> not_found;
        Store -> hb_cache:read(ID, #{ store => Store })
    end.

%% @doc Write a key to the remote node.
%%
%% Constructs an HTTP POST write request. If a wallet is provided,
%% the message is signed. Returns {ok, Path} on HTTP 200, or
%% {error, Reason} on failure.
%%
%% @param Opts A map of options (including node configuration).
%% @param Key The key to write.
%% @param Value The value to store.
%% @returns {ok, Path} on success or {error, Reason} on failure.
write(#{ <<"read-only">> := true }, _Key, _Value) ->
    not_found;
write(Opts = #{ <<"node">> := Node }, Key, Value) ->
    ?event({write, {node, Node}, {key, Key}, {value, Value}}),
    WriteMsg = #{
        <<"path">> => <<"/~cache@1.0/write">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Value
    },
    SignedMsg = hb_message:commit(WriteMsg, Opts),
    ?event({write, {signed, SignedMsg}}),
    case hb_http:post(Node, SignedMsg, Opts) of
        {ok, Response} ->
            Status = hb_ao:get(<<"status">>, Response, 0, #{}),
            ?event(store_remote_node, {write_completed, {response, Response}}),
            case Status of
                200 -> ok;
                _ -> {error, {unexpected_status, Status}}
            end;
        {error, Err} ->
            ?event({write, {error, Err}}),
            {error, Err}
    end.

%% @doc Link a source to a destination in the remote node.
%%
%% Constructs an HTTP POST link request. If a wallet is provided,
%% the message is signed. Returns {ok, Path} on HTTP 200, or
%% {error, Reason} on failure.
make_link(#{ <<"read-only">> := true }, _Source, _Destination) ->
    not_found;
make_link(Opts = #{ <<"node">> := Node }, Source, Destination) ->
    ?event({make_remote_link, {node, Node}, {source, Source},
                                  {destination, Destination}}),
    LinkMsg = #{
        <<"path">> => <<"/~cache@1.0/link">>,
        <<"method">> => <<"POST">>,
        <<"source">> => Source,
        <<"destination">> => Destination
    },
    SignedMsg = hb_message:commit(LinkMsg, Opts),
    ?event({make_remote_link, {signed, SignedMsg}}),
    case hb_http:post(Node, SignedMsg, Opts) of
        {ok, Response} ->
            Status = hb_ao:get(<<"status">>, Response, 0, #{}),
            ?event(store_remote_node, {make_link_completed, {response, Response}}),
            case Status of
                200 -> ok;
                _ -> {error, {unexpected_status, Status}}
            end;
        {error, Err} ->
            ?event(store_remote_node, {make_link_error, {error, Err}}),
            {error, Err}
    end.

%% @doc Remote store `make_group/2' is a no-op.
make_group(_StoreOpts, _Path) -> not_found.

%%%--------------------------------------------------------------------
%%% Tests
%%%--------------------------------------------------------------------

multinode_env() ->
    Node1Store = [hb_test_utils:test_store()],                                                            
    Node2Store = [hb_test_utils:test_store()],
    Wallet1 = ar_wallet:new(),
    Wallet2 = ar_wallet:new(),
    Opts1 = #{ priv_wallet => Wallet1, store => Node1Store },
    Opts2 = #{ priv_wallet => Wallet2, store => Node2Store },
    Msg1 = hb_message:commit(#{ <<"key1">> => <<"message1">>, <<"num1">> => 1 }, Opts1),
    Msg2 = hb_message:commit(#{ <<"key2">> => <<"message2">> }, Opts2),
    BothMsg =
        hb_message:commit(
            #{ <<"key-both">> => <<"value-both">> },
            Opts1
        ),
    {ok, ID1} = hb_cache:write(Msg1, Opts1),
    {ok, ID2} = hb_cache:write(Msg2, Opts2),
    {ok, IDBoth} = hb_cache:write(BothMsg, Opts1),
    {ok, IDBoth} = hb_cache:write(BothMsg, Opts2),
    Node1 = hb_http_server:start_node(Opts1),
    Node2 = hb_http_server:start_node(Opts2),
    RemoteStore =
        #{
            <<"store-module">> => hb_store_remote_node,
            <<"max-retries">> => 0,
            <<"nodes">> => [
                #{ 
                    <<"prefix">> => Node1, 
                    <<"opts">> => #{ <<"http-reference">> => <<"node1">> }
                }, 
                #{
                    <<"prefix">> => Node2, 
                    <<"opts">> => #{ <<"http-reference">> => <<"node2">> }
                }
            ],
            <<"parallel">> => 1
        },
    #{
        ids_single => [ID1, ID2],
        id_both => [IDBoth],
        nodes => [Node1, Node2],
        stores => [Node1Store, Node2Store],
        opts => [Opts1, Opts2],
        remote_store => RemoteStore
    }.

%% @doc Test that we can create a store, write a random message to it, then
%% start a remote node with that store, and read the message from it.
read_test() ->
    rand:seed(default),
    LocalStore = #{ 
		<<"store-module">> => hb_store_fs,
		<<"name">> => <<"cache-mainnet">>
	},
    hb_store:reset(LocalStore),
    M = #{ <<"test-key">> => Rand = rand:uniform(1337) },
    ID = hb_message:id(M),
    {ok, ID} =
        hb_cache:write(
			M, 
			#{ store => LocalStore }
		),
    ?event({wrote, ID}),
    Node =
        hb_http_server:start_node(
            #{
                store => LocalStore
            }
        ),
    RemoteStore = [
		#{ <<"store-module">> => hb_store_remote_node, <<"node">> => Node }
	],
    {ok, RetrievedMsg} = hb_cache:read(ID, #{ store => RemoteStore }),
    ?assertMatch(#{ <<"test-key">> := Rand }, hb_cache:ensure_all_loaded(RetrievedMsg)).

read_only_ids_test() ->
    LocalStore = hb_test_utils:test_store(),
    hb_store:reset(LocalStore),
    {ok, ID} =
        hb_cache:write(
			<<"message">>, 
			#{ store => LocalStore }
		),
    Node = hb_http_server:start_node(#{ store => LocalStore }),
    RemoteStore =
		#{
            <<"store-module">> => hb_store_remote_node,
            <<"node">> => Node,
            <<"only-ids">> => true
        },
    ?assertEqual(not_found, hb_cache:read(ID, #{ store => [RemoteStore] })).

multiread_test() ->
    #{ ids_single := [ID1, ID2], remote_store := RemoteStore } = multinode_env(),
    ?assertMatch(
        {ok, #{ <<"key1">> := <<"message1">>}},
        hb_cache:read(ID1, #{ store => RemoteStore })
    ),
    ?assertMatch(
        {ok, #{ <<"key2">> := <<"message2">>}},
        hb_cache:read(ID2, #{ store => RemoteStore })
    ).

corrupted_id_test() ->
    #{
        ids_single := [ID1|_],
        stores := [Store1|_],
        remote_store := RemoteStore
    } = multinode_env(),
    % Start by reading the message back and checking that it is accessible
    % (and valid) to start with.
    ?assertMatch(
        {ok, #{ <<"key1">> :=  _ }},
        hb_cache:read(ID1, #{ store => RemoteStore })
    ),
    {ok, Msg} = hb_cache:read(ID1, #{ store => Store1 }),
    % Corrupt the value of `key1`, but keep the commitments. These commitments
    % will now be invalid. A local store will return this invalid value, but
    % a remote store will not.
    hb_cache:write(Msg#{ <<"key1">> => <<"corrupt-value">> }, #{ store => Store1 }),
    {ok, ReadCorruptMsg} = hb_cache:read(ID1, #{ store => Store1 }),
    ?assertMatch(
        #{ <<"key1">> := <<"corrupt-value">> },
        hb_cache:ensure_all_loaded(ReadCorruptMsg, #{ store => Store1 })
    ),
    ?assertMatch(not_found, hb_cache:read(ID1, #{ store => RemoteStore })).

multiread_corrupted_id_test() ->
    #{
        id_both := IDBoth,
        stores := [Store1, Store2],
        remote_store := RemoteStore
    } = multinode_env(),
    % Force an invalid link on one node to the nessage stored in both nodes.
    FakeID = hb_util:human_id(<<0:256>>),
    ok = hb_store:make_link(Store1, IDBoth, FakeID),
    % Check we can read the message back from the store locally. This would be
    % a local node store integrity failure if it were to happen in the wild, but
    % our security model assumes that the local store is trustworthy for local
    % computation.
    {ok, RetrievedMsg} = hb_cache:read(FakeID, #{ store => Store1 }),
    ?assertMatch(
        #{ <<"key-both">> := <<"value-both">> },
        hb_cache:ensure_all_loaded(RetrievedMsg)
    ),
    % Ensure that we _cannot_ read the message back from the remote node. This
    % should fail despite the remote peer returning a valid message (with the 
    % wrong message) because the `multirequest-admissible' directive will fail.
    ?assertMatch(
        not_found,
        hb_cache:read(FakeID, #{ store => RemoteStore })
    ).

multiread_swapped_id_test() ->
    #{
        ids_single := [ID1, ID2],
        nodes := [Node1, Node2],
        stores := [Store1, Store2],
        remote_store := RemoteStore
    } = multinode_env(),
    % Link ID2 to ID1 on the first node and store. The first node will
    % return ID2 but with the wrong message. It should fail and trigger a
    % call to the second node, which should return it correctly.
    ok = hb_store:make_link(Store1, ID1, ID2),
    ?assertMatch(
        {ok, #{ <<"key2">> := _ }},
        hb_cache:read(ID2, #{ store => Store2 })
    ),
    ?assertMatch(
        {ok, #{ <<"key1">> := _ }},
        hb_cache:read(ID2, #{ store => Store1 })
    ),
    % Verify that a remote store with only the corrupt node will not return ID2,
    % but a store with both the corrupt and correct nodes will.
    ?assertMatch(
        not_found,
        hb_cache:read(
            ID2,
            #{ store => RemoteStore#{ <<"nodes">> => [#{ <<"prefix">> => Node1 }] } }
        )
    ),
    ?assertMatch(
        {ok, #{ <<"key2">> := _ }},
        hb_cache:read(ID2, #{ store => Store2 })
    ),
    ?assertMatch(
        {ok, #{ <<"key2">> := _ }},
        hb_cache:read(ID2, #{ store => RemoteStore })
    ).

multiread_admissible_response_hook_test() ->
    #{
        ids_single := [ID1|_],
        remote_store := BaseRemoteStore
    } = multinode_env(),
    % Ensure that we can execute a hook on valid read responses.
    LogStore = [hb_test_utils:test_store()],
    RemoteStore =
        BaseRemoteStore#{
            <<"on">> => #{
                <<"~cache@1.0">> =>
                    #{
                        <<"admissible-response">> => #{
                            <<"device">> => <<"test-device@1.0">>,
                            <<"store">> => LogStore,
                            <<"path">> => <<"log-request">>
                        }
                    }
            }
        },
    Opts = #{ store => RemoteStore },
    ?assertMatch(
        {ok, #{ <<"key1">> :=  _ }},
        hb_cache:read(ID1, #{ store => RemoteStore })
    ),
    ?assertMatch(
        {ok, Logs} when is_map(Logs) andalso map_size(Logs) > 1,
        hb_ao:resolve(
            #{ <<"device">> => <<"test-device@1.0">> },
            <<"logs">>,
            Opts#{ store => LogStore }
        )
    ).

arweave_dot_net_as_remote_node_test() ->
    TestIDs =
        [
            <<"93Ui7nOLDNVCVMLeFkVeeOCVkm5Jy-kf6FNatW3q2TI">>,
            <<"VuhnX2G8qVAb6kwHOiCQKl2c-42uoMKSIpHgKc0Pnzg">>
        ],
    Opts =
        #{
            store =>
                [
                    #{
                        <<"store-module">> => hb_store_remote_node,
                        <<"name">> => <<"cache-arweave">>,
                        <<"node">> => <<"https://arweave.net">>
                    }
                ]
        },
    % Recent bundled AO messages -- no `signature` tag collision.
    lists:foreach(
        fun(ID) ->
            {ok, M} = hb_cache:read(ID, Opts),
            ?assert(hb_message:verify(M, all, Opts))
        end,
        TestIDs
    ).