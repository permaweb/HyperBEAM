%%% @doc A store module that reads data from another AO node.
%%% Notably, this store only provides the _read_ side of the store interface.
%%% The write side could be added, returning an commitment that the data has
%%% been written to the remote node. In that case, the node would probably want
%%% to upload it to an Arweave bundler to ensure persistence, too.
-module(hb_store_remote_node).
-export([scope/1, read/3]).
%%% Public utilities.
-export([maybe_cache/2, read_local_cache/3]).
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

%% @doc Read a key from the remote node. A key that is not an ID refers to the
%% remote node's own state, which its reply cannot be checked against, so it
%% is read only if the store sets `trusted' to `true'.
%%
%% @param Opts A map of options (including node configuration).
%% @param Key The key to read.
%% @returns `{ok, Msg}' on success or `{error, not_found}' if the key is missing.
read_request(Opts, Key) when ?IS_ID(Key) -> remote_read(Opts, Key);
read_request(Opts = #{ <<"trusted">> := true }, Key) -> remote_read(Opts, Key);
read_request(_, _) -> {error, not_found}.

%% @doc Make an HTTP GET request for a key to the remote node and return the
%% committed message. Unless the key is a `ni:' name, a binary is encoded with
%% `hb_cache:encode_value/1', so that the cache reads it back as the same
%% binary. A `node' that starts with `/' is a path: the request is sent to the
%% peer that the node's `routes' give for it.
remote_read(Opts = #{ <<"node">> := Node }, Key) ->
    ?event(store_remote_node, {executing_read, {node, Node}, {key, Key}}),
    Path =
        case ?IS_ID(Key) of
            true -> <<"/~cache@1.0/read=", (hb_util:human_id(Key))/binary>>;
            false -> <<"/~cache@1.0/read">>
        end,
    Request = #{ <<"path">> => Path, <<"read">> => Key },
    HTTPOpts = Opts#{ <<"http-only-result">> => true },
    HTTPRes =
        case Node of
            <<"/", _/binary>> ->
                Prefix = string:trim(Node, trailing, "/"),
                hb_http:request(
                    Request#{
                        <<"method">> => <<"GET">>,
                        <<"path">> => <<Prefix/binary, Path/binary>>
                    },
                    HTTPOpts
                );
            _ -> hb_http:get(Node, Request, HTTPOpts)
        end,
    case HTTPRes of
        {ok, Res} ->
            % returning the whole response to get the test-key
            Msg = without_transport_commitment(Res, Key, Opts),
            ?event(store_remote_node, {read_found, {result, Msg, response, Res}}),
            case not ?IS_ID(Key) orelse has_id(Msg, Key, Opts) of
                true ->
                    maybe_cache(Opts, Msg),
                    case Key of
                        <<"ni:", _/binary>> -> {ok, Msg};
                        _ -> {ok, hb_cache:encode_value(Msg)}
                    end;
                false ->
                    ?event(store_remote_node, {read_other_id, {key, Key}}),
                    {error, not_found}
            end;
        {error, _Err} ->
            ?event(store_remote_node, {read_not_found, {key, Key}}),
            {error, not_found}
    end;
remote_read(_, _) -> {error, not_found}.

%% @doc Read a key with the node's options, overridden by the options in the
%% store's message. `force-message' is removed, as with it `hb_ao' returns
%% messages in place of values, and `store-module', as `hb_cache:list/2' takes
%% options that contain it for a store.
read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    read_request(
        maps:without(
            [<<"store-module">>, <<"force-message">>],
            hb_util:deep_merge(NodeOpts, StoreOpts, NodeOpts)
        ),
        Key
    ).

%% @doc Return whether a message read by an ID has that ID: its uncommitted ID,
%% the ID of one of its commitments, or the combined ID of its commitments, as
%% `hb_cache:write/2' links it under. Its commitments must verify, as a reply
%% gives each one's ID as the key it is under. A binary has the ID of its data.
has_id(Msg, ID, Opts) when is_map(Msg) ->
    lists:member(
        hb_util:human_id(ID),
        [hb_message:id(Msg, none, Opts), hb_message:id(Msg, all, Opts)] ++
            hb_maps:keys(hb_maps:get(<<"commitments">>, Msg, #{}, Opts), Opts)
    ) andalso hb_message:verify(Msg, all, Opts);
has_id(Bin, ID, Opts) ->
    hb_util:human_id(ID) == hb_message:id(Bin, none, Opts).

%% @doc Remove the transport commitments from the response to a read of `Key':
%% the replying node's signature over `hashpath', and its unsigned commitment
%% over the keys of its reply, once the keys that it does not cover are
%% removed. If `Key' is the ID of a commitment, keep that commitment, even if
%% it covers only `hashpath'. Remove the reply's `hashpath' with the transport
%% commitments, unless a signed commitment covers it, or the commitment with
%% the ID `Key' does. If no signed commitment remains, remove the
%% `commitments' key. A reply without commitments loses its `hashpath' and
%% `status'.
without_transport_commitment(Msg = #{ <<"commitments">> := _ }, Key, Opts) ->
    ReadIDs = [hb_util:human_id(Key) || ?IS_ID(Key)],
    OwnKeys =
        hb_message:committed(
            hb_message:with_commitments(ReadIDs, Msg, Opts),
            #{ <<"ids">> => <<"all">> },
            Opts
        ),
    {ok, Committed} =
        hb_message:with_only_committed(
            hb_message:without_commitments(
                hb_maps:keys(
                    hb_message:commitments(
                        #{ <<"committed">> => [<<"hashpath">>] },
                        Msg,
                        Opts
                    ),
                    Opts
                ) -- ReadIDs,
                Msg,
                Opts
            ),
            Opts
        ),
    Signed =
        hb_message:commitments(#{ <<"committer">> => '_' }, Committed, Opts),
    hb_message:without_unless_signed(
        [<<"hashpath">>] -- OwnKeys,
        case hb_maps:size(Signed, Opts) of
            0 -> hb_message:uncommitted(Committed, Opts);
            _ -> Committed#{ <<"commitments">> => Signed }
        end,
        Opts
    );
without_transport_commitment(Msg, _Key, _Opts) when is_map(Msg) ->
    maps:without([<<"hashpath">>, <<"status">>], Msg);
without_transport_commitment(Res, _Key, _Opts) ->
    Res.

%% @doc Cache the data if the cache is enabled. The `local-store' option may
%% either be `false' or a store definition to use as the local cache. The
%% cache links the data to its own IDs alone: its content hash, and the ID of
%% each commitment that verifies. The key the data was requested by is not
%% linked to it, as the data need not be the message with that ID.
maybe_cache(StoreOpts, Data) ->
    ?event({maybe_cache, StoreOpts, Data}),
    try
        % Check if the local store is in our store options.
        case hb_maps:get(<<"local-store">>, StoreOpts, false, StoreOpts) of
            false ->
                skipped;
            Store ->
                case hb_cache:write(Data, #{ <<"store">> => Store }) of
                    {ok, _RootPath} ->
                        ?event(store_remote_node, cached_received),
                        ok;
                    {error, Err} ->
                        ?event(store_remote_node, error_on_local_cache_write),
                        ?event(warning, {error_caching_remote_node_data, Err}),
                        {error, Err}
                end
        end
    catch _:_ ->
        ignored
    end.

%% @doc Read local store cached value. Maintains the `Opts` for the recursive
%% `hb_cache:read` call, but uses the `StoreOpts` as the source of the
%% `local-store` value.
read_local_cache(StoreOpts, ID, Opts) ->
    ?event({read_local_cache, StoreOpts, ID}),
    case hb_maps:get(<<"local-store">>, StoreOpts, false, StoreOpts) of
        false -> {error, not_found};
        Store ->
            CacheOpts = hb_util:deep_merge(Opts, StoreOpts, Opts),
            hb_cache:read(
                ID,
                CacheOpts#{ <<"store">> => Store }
            )
    end.

%%%--------------------------------------------------------------------
%%% Tests
%%%--------------------------------------------------------------------

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
			#{ <<"store">> => LocalStore }
		),
    ?event({wrote, ID}),
    Node =
        hb_http_server:start_node(
            #{
                <<"store">> => LocalStore
            }
        ),
    RemoteStore = [
		#{
			<<"store-module">> => hb_store_remote_node,
			<<"node">> => Node,
			<<"http-only-result">> => false
		}
	],
    {ok, RetrievedMsg} = hb_cache:read(ID, #{ <<"store">> => RemoteStore }),
    % An unsigned message reads back as it was written, without commitments.
    ?assertEqual(#{ <<"test-key">> => Rand }, hb_cache:ensure_all_loaded(RetrievedMsg)),
    % A read with the `force-message' of an HTTP request runs the node's
    % `http-client-receipt' hook.
    Self = self(),
    Receipt =
        fun(_, Req, _) ->
            Self ! receipt,
            {ok, Req}
        end,
    {ok, _} =
        hb_cache:read(
            ID,
            #{
                <<"store">> => RemoteStore,
                <<"force-message">> => true,
                <<"priv-wallet">> => ar_wallet:new(),
                <<"on">> => #{
                    <<"http-client-receipt">> =>
                        #{ <<"device">> => #{ http_client_receipt => Receipt } }
                }
            }
        ),
    ?assertEqual(
        receipt,
        receive receipt -> receipt after 5000 -> 'missing-receipt' end
    ),
    % The store reads the list regardless of the HTTP result option.
    ?assertEqual(
        {ok, []},
        hb_cache:read(
            hb_util:ok(hb_cache:write([], #{ <<"store">> => LocalStore })),
            #{ <<"store">> => RemoteStore }
        )
    ),
    % A message keeps a `hashpath' key of its own.
    ?assertMatch(
        {ok, #{ <<"hashpath">> := <<"x">> }},
        hb_cache:read(
            hb_util:ok(
                hb_cache:write(
                    #{ <<"hashpath">> => <<"x">> },
                    #{ <<"store">> => LocalStore }
                )
            ),
            #{ <<"store">> => RemoteStore }
        )
    ),
    % A message keeps the `status' key that it holds.
    ?assertMatch(
        {ok, #{ <<"status">> := <<"pending">> }},
        hb_cache:read(
            hb_util:ok(
                hb_cache:write(
                    #{ <<"status">> => <<"pending">> },
                    #{ <<"store">> => LocalStore }
                )
            ),
            #{ <<"store">> => RemoteStore }
        )
    ).

read_only_ids_test() ->
    LocalStore = hb_test_utils:test_store(),
    hb_store:reset(LocalStore),
    {ok, ID} =
        hb_cache:write(
			<<"message">>, 
			#{ <<"store">> => LocalStore }
		),
    Node =
        hb_http_server:start_node(
            #{
                <<"store">> => LocalStore
            }
        ),
    RemoteStore = [
		#{ <<"store-module">> => hb_store_remote_node,
           <<"node">> => Node,
           <<"trusted">> => false }
	],
    ?assertEqual({error, not_found}, hb_cache:read(ID, #{ <<"store">> => RemoteStore })).

%% @doc Read a message through a remote store whose `node' is a path, which the
%% node's `routes' send to the remote node.
routed_read_test() ->
    LocalStore = hb_test_utils:test_store(),
    {ok, ID} =
        hb_cache:write(
            #{ <<"test-key">> => <<"routed">> },
            #{ <<"store">> => LocalStore }
        ),
    Node = hb_http_server:start_node(#{ <<"store">> => LocalStore }),
    Opts = #{
        <<"store">> => [
            #{
                <<"store-module">> => hb_store_remote_node,
                <<"node">> => <<"/peer">>
            }
        ],
        <<"force-message">> => true,
        <<"routes">> => [
            #{
                <<"template">> => <<"^/peer/">>,
                <<"nodes">> => [
                    #{
                        <<"match">> => <<"^/peer">>,
                        <<"with">> => string:trim(Node, trailing, "/")
                    }
                ]
            }
        ]
    },
    ?assertMatch(
        {ok, #{ <<"test-key">> := <<"routed">> }},
        hb_cache:read(ID, Opts)
    ).
