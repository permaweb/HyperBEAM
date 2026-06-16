%%% @doc A store module that reads data from another AO node.
%%% Notably, this store only provides the _read_ side of the store interface.
%%% The write side could be added, returning an commitment that the data has
%%% been written to the remote node. In that case, the node would probably want
%%% to upload it to an Arweave bundler to ensure persistence, too.
-module(hb_store_remote_node).
-export([scope/1, type/3, read/3, write/3, link/3, group/3, resolve/3]).
%%% Public utilities.
-export([maybe_cache/2, maybe_cache/3, read_local_cache/3]).
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
resolve(#{ <<"node">> := Node }, #{ <<"resolve">> := Key }, _NodeOpts) ->
    ?event({remote_resolve, {node, Node}, {key, Key}}),
    {ok, Key}.

%% @doc Determine the type of value at a given key.
%%
%% Remote nodes support `simple', `composite', or `{error, not_found}'.
%%
%% @param Opts A map of options (including node configuration).
%% @param Key The key whose value type is determined.
%% @returns `{ok, simple}' or `{ok, composite}' if found, or
%%          `{error, not_found}' otherwise.
type(Opts = #{ <<"node">> := Node }, #{ <<"type">> := Key }, _NodeOpts) ->
    ?event({remote_type, {node, Node}, {key, Key}}),
    case read_request(Opts, Key) of
        {composite, _} -> {ok, composite};
        {ok, _} -> {ok, simple};
        Other -> Other
    end.

%% @doc Read a key from the remote node.
%%
%% Makes an HTTP GET request to the remote node and returns the
%% committed message.
%%
%% @param Opts A map of options (including node configuration).
%% @param Key The key to read.
%% @returns `{ok, Msg}' on success or `{error, not_found}' if the key is missing.
read_request(#{ <<"only-ids">> := true }, Key) when not ?IS_ID(Key) ->
    {error, not_found};
read_request(Opts = #{ <<"node">> := Node }, Key) ->
    ?event(store_remote_node, {executing_read, {node, Node}, {key, Key}}),
    ReadReq0 = #{ <<"path">> => <<"/~cache@1.0/read">>, <<"read">> => Key },
    ReadReq =
        case hb_maps:get(<<"require-codec">>, Opts, not_found, Opts) of
            not_found -> ReadReq0;
            Codec -> ReadReq0#{ <<"require-codec">> => Codec }
        end,
    HTTPRes =
        hb_http:get(
            Node,
            ReadReq,
            Opts
        ),
    case HTTPRes of
        {ok, Res} ->
            % returning the whole response to get the test-key
            case hb_message:with_only_committed(Res, Opts) of
                {ok, Msg0} ->
                    case verify_remote_read(Opts, Key, Msg0) of
                        {ok, Msg} ->
                            ?event(store_remote_node, {read_found, {result, Msg, response, Res}}),
                            maybe_cache(Opts, Msg, cache_links(Opts, Key, Msg)),
                            {ok, Msg};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        {error, _Err} ->
            ?event(store_remote_node, {read_not_found, {key, Key}}),
            {error, not_found}
    end;
read_request(_, _) -> {error, not_found}.
read(Opts, #{ <<"read">> := Key }, _NodeOpts) ->
    read_request(Opts, Key).

verify_remote_read(Opts, Key, Msg) ->
    case truthy(hb_maps:get(<<"verify-remote-read">>, Opts, false, Opts)) of
        false ->
            {ok, Msg};
        true ->
            verify_remote_read(Opts, Key, Msg, expected_commitment_devices(Opts, Key, Msg))
    end.

verify_remote_read(_Opts, Key, _Msg, []) ->
    {error, {remote_read_verification_failed, {missing_expected_device, Key}}};
verify_remote_read(Opts, Key, Msg, Devices) ->
    IDs = commitment_ids(Msg, Devices, Opts),
    case IDs of
        [] ->
            {error, {remote_read_verification_failed, {missing_commitment, Key, Devices}}};
        _ ->
            case native_key_bound(Key, Msg, IDs, Opts) of
                ok ->
                    case hb_message:verify(Msg, #{ <<"commitment-ids">> => IDs }, Opts) of
                        true -> {ok, Msg};
                        false ->
                            {error,
                                {remote_read_verification_failed,
                                    {invalid_commitment, Key, Devices}}}
                    end;
                Error ->
                    Error
            end
    end.

expected_commitment_devices(Opts, Key, Msg) ->
    case explicit_commitment_devices(Opts) of
        [] -> inferred_commitment_devices(Key, Msg, Opts);
        Devices -> Devices
    end.

explicit_commitment_devices(Opts) ->
    case hb_maps:get(<<"expected-commitment-devices">>, Opts, not_found, Opts) of
        not_found ->
            case hb_maps:get(<<"expected-commitment-device">>, Opts, not_found, Opts) of
                Device when is_binary(Device) -> [Device];
                _ -> []
            end;
        Devices when is_list(Devices) ->
            Devices;
        Devices when is_map(Devices) ->
            hb_util:message_to_ordered_list(Devices, Opts);
        Device when is_binary(Device) ->
            [Device];
        _ ->
            []
    end.

inferred_commitment_devices(Key, Msg, Opts) when is_binary(Key) ->
    case Key of
        <<"lbry/blob/", _/binary>> -> [<<"lbry-blob@1.0">>];
        <<"lbry/blob-id/", _/binary>> -> [<<"lbry-blob@1.0">>];
        <<"odysee/blob/", _/binary>> -> [<<"lbry-blob@1.0">>];
        <<"odysee/blob-id/", _/binary>> -> [<<"lbry-blob@1.0">>];
        <<"lbry/descriptor/", _/binary>> -> [<<"lbry-stream-descriptor@1.0">>];
        <<"lbry/descriptor-id/", _/binary>> -> [<<"lbry-stream-descriptor@1.0">>];
        <<"lbry/stream-descriptor/", _/binary>> -> [<<"lbry-stream-descriptor@1.0">>];
        <<"odysee/descriptor/", _/binary>> -> [<<"lbry-stream-descriptor@1.0">>];
        <<"odysee/descriptor-id/", _/binary>> -> [<<"lbry-stream-descriptor@1.0">>];
        <<"odysee/stream-descriptor/", _/binary>> -> [<<"lbry-stream-descriptor@1.0">>];
        <<"lbry/claim-output/", _/binary>> -> [<<"lbry-claim-output@1.0">>];
        <<"lbry/claim-proof/", _/binary>> -> [<<"lbry-claim-output@1.0">>];
        <<"odysee/claim-proof/", _/binary>> -> [<<"lbry-claim-output@1.0">>];
        <<"lbry/claim/", _/binary>> -> [<<"lbry-claim@1.0">>];
        <<"lbry/channel/", _/binary>> -> [<<"lbry-channel@1.0">>];
        <<"lbry/stream/", _/binary>> -> [<<"lbry-stream@1.0">>];
        <<"lbry/transaction/", _/binary>> -> [<<"lbry-transaction@1.0">>];
        <<"lbry/tx/", _/binary>> -> [<<"lbry-transaction@1.0">>];
        <<"odysee/transaction/", _/binary>> -> [<<"lbry-transaction@1.0">>];
        <<"odysee/tx/", _/binary>> -> [<<"lbry-transaction@1.0">>];
        <<"odysee/claim/", _/binary>> -> [<<"odysee@1.0">>];
        <<"odysee/claim-id/", _/binary>> -> [<<"odysee@1.0">>];
        <<"odysee/stream/", _/binary>> -> [<<"odysee@1.0">>];
        <<"odysee/stream-id/", _/binary>> -> [<<"odysee@1.0">>];
        <<"odysee/channel/", _/binary>> -> [<<"odysee@1.0">>];
        <<"odysee/channel-id/", _/binary>> -> [<<"odysee@1.0">>];
        <<"odysee/comment/", _/binary>> -> [<<"odysee@1.0">>];
        <<"odysee/comment-id/", _/binary>> -> [<<"odysee@1.0">>];
        _ -> inferred_commitment_devices(not_found, Msg, Opts)
    end;
inferred_commitment_devices(_Key, Msg, Opts) when is_map(Msg) ->
    case hb_maps:get(<<"device">>, Msg, not_found, Opts) of
        <<"lbry-blob@1.0">> -> [<<"lbry-blob@1.0">>];
        <<"lbry-stream-descriptor@1.0">> -> [<<"lbry-stream-descriptor@1.0">>];
        <<"lbry-claim-output@1.0">> -> [<<"lbry-claim-output@1.0">>];
        <<"lbry-claim@1.0">> -> [<<"lbry-claim@1.0">>];
        <<"lbry-channel@1.0">> -> [<<"lbry-channel@1.0">>];
        <<"lbry-stream@1.0">> -> [<<"lbry-stream@1.0">>];
        <<"lbry-transaction@1.0">> -> [<<"lbry-transaction@1.0">>];
        <<"odysee-claim-proof@1.0">> -> [<<"lbry-claim-output@1.0">>];
        <<"odysee-claim@1.0">> -> [<<"odysee@1.0">>];
        <<"odysee-stream@1.0">> -> [<<"odysee@1.0">>];
        <<"odysee-channel@1.0">> -> [<<"odysee@1.0">>];
        <<"odysee-comment@1.0">> -> [<<"odysee@1.0">>];
        _ -> []
    end;
inferred_commitment_devices(_Key, _Msg, _Opts) ->
    [].

commitment_ids(Msg, Devices, Opts) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    [
        ID
    ||
        {ID, Commitment} <- maps:to_list(Commitments),
        lists:member(
            hb_maps:get(<<"commitment-device">>, Commitment, not_found, Opts),
            Devices
        )
    ].

native_key_bound(Key, Msg, IDs, Opts) ->
    case expected_native_id(Key) of
        not_found ->
            ok;
        {ok, NativeID} ->
            Commitments =
                hb_cache:ensure_all_loaded(
                    hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
                    Opts
                ),
            case
                lists:any(
                    fun(ID) ->
                        case maps:get(ID, Commitments, undefined) of
                            undefined -> false;
                            Commitment -> commitment_bound_to_native_id(Commitment, NativeID, Opts)
                        end
                    end,
                    IDs
                )
            of
                true -> ok;
                false ->
                    {error,
                        {remote_read_verification_failed,
                            {native_id_mismatch, Key, NativeID}}}
            end
    end.

commitment_bound_to_native_id(Commitment, NativeID, Opts) ->
    case hb_lbry_commitment:native_id(Commitment, Opts) of
        {ok, NativeID, _Bytes} -> true;
        _ ->
            case hb_maps:get(<<"outpoint">>, Commitment, undefined, Opts) of
                Outpoint when is_binary(Outpoint) ->
                    hb_util:to_lower(Outpoint) =:= NativeID;
                _ ->
                    false
            end
    end.

expected_native_id(Key0) when is_binary(Key0) ->
    Key =
        case hb_path:to_binary(Key0) of
            <<"/", Normalized/binary>> -> Normalized;
            Other -> Other
        end,
    case Key of
        <<"lbry/blob/", Hash/binary>> -> native_hex(Hash, 48);
        <<"lbry/blob-id/", Hash/binary>> -> native_hex(Hash, 48);
        <<"odysee/blob/", Hash/binary>> -> native_hex(Hash, 48);
        <<"odysee/blob-id/", Hash/binary>> -> native_hex(Hash, 48);
        <<"lbry/descriptor/", Hash/binary>> -> native_hex(Hash, 48);
        <<"lbry/descriptor-id/", Hash/binary>> -> native_hex(Hash, 48);
        <<"lbry/stream-descriptor/", Hash/binary>> -> native_hex(Hash, 48);
        <<"odysee/descriptor/", Hash/binary>> -> native_hex(Hash, 48);
        <<"odysee/descriptor-id/", Hash/binary>> -> native_hex(Hash, 48);
        <<"odysee/stream-descriptor/", Hash/binary>> -> native_hex(Hash, 48);
        <<"lbry/transaction/", TxID/binary>> -> native_hex(TxID, 32);
        <<"lbry/tx/", TxID/binary>> -> native_hex(TxID, 32);
        <<"odysee/transaction/", TxID/binary>> -> native_hex(TxID, 32);
        <<"odysee/tx/", TxID/binary>> -> native_hex(TxID, 32);
        <<"lbry/claim-output/", Outpoint/binary>> -> outpoint_native_id(Outpoint);
        <<"lbry/claim-proof/", Outpoint/binary>> -> outpoint_native_id(Outpoint);
        <<"odysee/claim-proof/", Outpoint/binary>> -> outpoint_native_id(Outpoint);
        <<"lbry/claim/", Outpoint/binary>> -> outpoint_native_id(Outpoint);
        <<"lbry/channel/", Outpoint/binary>> -> outpoint_native_id(Outpoint);
        <<"lbry/stream/", Outpoint/binary>> -> outpoint_native_id(Outpoint);
        _ -> outpoint_native_id(Key)
    end;
expected_native_id(_Key) ->
    not_found.

native_hex(Hex, Bytes) ->
    case hb_lbry_commitment:native_id_bytes(Hex) of
        {ok, Normalized, NativeBytes} when byte_size(NativeBytes) =:= Bytes ->
            {ok, Normalized};
        _ ->
            not_found
    end.

outpoint_native_id(Rest) ->
    case binary:split(Rest, <<"/">>) of
        [TxID, NOut] -> outpoint_native_id(TxID, NOut);
        _ ->
            case binary:split(Rest, <<":">>) of
                [TxID, NOut] -> outpoint_native_id(TxID, NOut);
                _ -> not_found
            end
    end.

outpoint_native_id(TxID, NOutBin) ->
    case {native_hex(TxID, 32), non_negative_integer(NOutBin)} of
        {{ok, NormalizedTxID}, {ok, NOut}} ->
            try
                {ok, hb_util:to_hex(hb_lbry_commitment:outpoint_bytes(NormalizedTxID, NOut))}
            catch
                _:_ -> not_found
            end;
        _ ->
            not_found
    end.

non_negative_integer(Bin) ->
    try
        Int = binary_to_integer(Bin),
        case Int >= 0 of
            true -> {ok, Int};
            false -> not_found
        end
    catch
        _:_ -> not_found
    end.

cache_links(Opts, Key, Msg) ->
    Links = [Key],
    case truthy(hb_maps:get(<<"cache-commitment-ids">>, Opts, false, Opts))
        orelse truthy(hb_maps:get(<<"verify-remote-read">>, Opts, false, Opts))
    of
        true -> Links ++ commitment_ids(Msg, commitment_devices(Msg, Opts), Opts);
        false -> Links
    end.

commitment_devices(Msg, Opts) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    lists:usort([
        hb_maps:get(<<"commitment-device">>, Commitment, not_found, Opts)
    ||
        {_ID, Commitment} <- maps:to_list(Commitments),
        hb_maps:get(<<"commitment-device">>, Commitment, not_found, Opts) =/= not_found
    ]).

truthy(true) -> true;
truthy(<<"true">>) -> true;
truthy(<<"1">>) -> true;
truthy(1) -> true;
truthy(_Value) -> false.

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
                case hb_cache:write(Data, #{ <<"store">> => Store }) of
                    {ok, RootPath} ->
                        % Remove the base path from the links.
                        LinksWithoutRootPath =
                            lists:filter(
                                fun(Link) -> Link /= RootPath end,
                                Links
                            ),
                        ?event(store_remote_node, cached_received),
                        LinkResults =
                            lists:filtermap(
                                fun(Link) ->
                                    case hb_store:link(Store, #{ Link => RootPath }, #{}) of
                                        ok ->
                                            false;
                                        Result ->
                                            {true, {Link, Result}}
                                    end
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

%% @doc Read local store cached value. Maintains the `Opts` for the recursive
%% `hb_cache:read` call, but uses the `StoreOpts` as the source of the
%% `local-store` value.
read_local_cache(StoreOpts, ID, Opts) ->
    ?event({read_local_cache, StoreOpts, ID}),
    case hb_maps:get(<<"local-store">>, StoreOpts, false, StoreOpts) of
        false -> {error, not_found};
        Store -> hb_cache:read(ID, maps:merge(Opts, StoreOpts#{ <<"store">> => Store }))
    end.

%% @doc Write a key to the remote node.
%%
%% Uploads each value to the remote cache and then links each requested
%% destination to the uploaded path.
%%
%% @param Opts A map of options (including node configuration).
%% @param Req Map of destination paths to values.
%% @returns `ok' on success or `{error, Reason}' on failure.
write(#{ <<"read-only">> := true }, _Req, _NodeOpts) ->
    {error, not_found};
write(Opts = #{ <<"node">> := Node }, Req, _NodeOpts) when is_map(Req) ->
    ?event({write, {node, Node}, {request, Req}}),
    maps:fold(
        fun(Destination, Value, ok) ->
            case remote_write_value(Opts, Value) of
                {ok, SourcePath} ->
                    remote_link(Opts, hb_path:to_binary(SourcePath), hb_path:to_binary(Destination));
                {error, _} = Error ->
                    Error
            end;
           (_Destination, _Value, Error) ->
            Error
        end,
        ok,
        Req
    ).

%% @doc Link a source to a destination in the remote node.
%%
%% Constructs an HTTP POST link request for the given source and destination,
%% signing the request when a wallet is available.
%%
%% @returns `ok' on success or `{error, Reason}' on failure.
link(#{ <<"read-only">> := true }, _Req, _NodeOpts) ->
    {error, not_found};
link(Opts = #{ <<"node">> := _Node }, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(Destination, Source, ok) ->
            remote_link(Opts, hb_path:to_binary(Source), hb_path:to_binary(Destination));
           (_Destination, _Source, Error) ->
            Error
        end,
        ok,
        Req
    ).

%% @doc Create a group in the remote node cache.
group(#{ <<"read-only">> := true }, _Req, _NodeOpts) ->
    {error, not_found};
group(Opts = #{ <<"node">> := _Node }, #{ <<"group">> := Path }, _NodeOpts) ->
    remote_group(Opts, hb_path:to_binary(Path)).

remote_write_value(Opts = #{ <<"node">> := Node }, Value) ->
    Msg = #{
        <<"path">> => <<"/~cache@1.0/write">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Value
    },
    SignedMsg = hb_message:commit(Msg, Opts),
    case hb_http:post(Node, SignedMsg, Opts) of
        {ok, Response} ->
            case hb_ao:get(<<"status">>, Response, 0, #{}) of
                200 ->
                    case hb_ao:get(<<"path">>, Response, not_found, #{}) of
                        not_found -> {error, missing_path};
                        Path -> {ok, Path}
                    end;
                Status ->
                    {error, {unexpected_status, Status}}
            end;
        {error, Err} ->
            {error, Err}
    end.

remote_link(Opts = #{ <<"node">> := Node }, Source, Destination) ->
    Msg = #{
        <<"path">> => <<"/~cache@1.0/link">>,
        <<"method">> => <<"POST">>,
        <<"source">> => Source,
        <<"destination">> => Destination
    },
    SignedMsg = hb_message:commit(Msg, Opts),
    case hb_http:post(Node, SignedMsg, Opts) of
        {ok, Response} ->
            case hb_ao:get(<<"status">>, Response, 0, #{}) of
                200 -> ok;
                Status -> {error, {unexpected_status, Status}}
            end;
        {error, Err} ->
            {error, Err}
    end.

remote_group(Opts = #{ <<"node">> := Node }, Path) ->
    Msg = #{
        <<"path">> => <<"/~cache@1.0/group">>,
        <<"method">> => <<"POST">>,
        <<"group">> => Path
    },
    SignedMsg = hb_message:commit(Msg, Opts),
    case hb_http:post(Node, SignedMsg, Opts) of
        {ok, Response} ->
            case hb_ao:get(<<"status">>, Response, 0, #{}) of
                200 -> ok;
                Status -> {error, {unexpected_status, Status}}
            end;
        {error, Err} ->
            {error, Err}
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
		#{ <<"store-module">> => hb_store_remote_node, <<"node">> => Node }
	],
    {ok, RetrievedMsg} = hb_cache:read(ID, #{ <<"store">> => RemoteStore }),
    ?assertMatch(#{ <<"test-key">> := Rand }, hb_cache:ensure_all_loaded(RetrievedMsg)).

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
           <<"only-ids">> => true }
	],
    ?assertEqual({error, not_found}, hb_cache:read(ID, #{ <<"store">> => RemoteStore })).
