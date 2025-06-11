%%% @doc A store module that reads data from another AO node.
%%% Notably, this store only provides the _read_ side of the store interface.
%%% The write side could be added, returning an commitment that the data has
%%% been written to the remote node. In that case, the node would probably want
%%% to upload it to an Arweave bundler to ensure persistence, too.
-module(hb_store_remote_node).
-export([scope/1, type/2, read/2, write/3, make_link/3, resolve/2]).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").


%% @doc Return the scope of this store.
%%
%% For the remote store, the scope is always `remote'.
%%
%% @param Arg An arbitrary parameter (ignored).
%% @returns remote.
scope(Arg) ->
    ?event({remote_scope, {arg, Arg}}),
    remote.


%% @doc Resolve a key path in the remote store.
%%
%% For the remote node store, the key is returned as-is.
%%
%% @param Data A map containing node configuration.
%% @param Key The key to resolve.
%% @returns The resolved key.
resolve(#{<<"node">> := Node}, Key) ->
    ?event({remote_resolve, {node, Node}, {key, Key}}),
    Key.


%% @doc Determine the type of value at a given key.
%%
%% Remote nodes support only the `simple' type or `not_found'.
%%
%% @param Opts A map of options (including node configuration).
%% @param Key The key whose value type is determined.
%% @returns simple if found, or not_found otherwise.
type(Opts = #{<<"node">> := Node}, Key) ->
    ?event({remote_type, {node, Node}, {key, Key}}),
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
read(Opts = #{<<"node">> := Node}, Key) ->
    ?event({remote_read, {node, Node}, {key, Key}}),
    case hb_http:get(Node, #{<<"path">> => <<"/~cache@1.0/read">>, <<"target">> => Key}, Opts) of
        {ok, Res} ->
            % returning the whole response to get the test-key
            % {ok, Msg} = hb_message:with_only_committed(Res),
            Msg = hb_message:uncommitted(Res, Opts),
            % ?event({read, {result, Msg}}),
            {ok, Msg};
        {error, _Err} ->
            ?event({read, {result, not_found}}),
            not_found
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
write(Opts = #{<<"node">> := Node}, Key, Value) ->
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
            ?event({write, {response, Response}}),
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
make_link(Opts = #{<<"node">> := Node}, Source, Destination) ->
    ?event({make_remote_link, {node, Node},
                              {source, Source},
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
            ?event({make_remote_link, {response, Response}}),
            case Status of
                200 -> ok;
                _ -> {error, {unexpected_status, Status}}
            end;
        {error, Err} ->
            ?event({make_remote_link, {error, Err}}),
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
    M = #{<<"test-key">> => Rand = rand:uniform(1337)},
    ID = hb_message:id(M),
    {ok, ID} =
        hb_cache:write(
          M,
          #{store => LocalStore}),
    ?event({wrote, ID}),
    Node =
        hb_http_server:start_node(
          #{
            store => LocalStore
           }),
    RemoteStore = [#{<<"store-module">> => hb_store_remote_node, <<"node">> => Node}],
    {ok, RetrievedMsg} = hb_cache:read(ID, #{store => RemoteStore}),
    ?assertMatch(#{<<"test-key">> := Rand}, hb_cache:ensure_all_loaded(RetrievedMsg)).
