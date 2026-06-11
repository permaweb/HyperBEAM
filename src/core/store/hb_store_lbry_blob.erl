-module(hb_store_lbry_blob).
-export([scope/0, scope/1, type/3, read/3, resolve/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_NODE, <<"http://blobcache-eu.odycdn.com:5569">>).

scope() -> remote.
scope(_) -> scope().

resolve(_StoreOpts, #{ <<"resolve">> := Hash }, _NodeOpts) ->
    case valid_hash(Hash) of
        true -> {ok, hb_util:to_lower(Hash)};
        false -> {error, not_found}
    end.

type(StoreOpts, #{ <<"type">> := Hash }, NodeOpts) ->
    case valid_hash(Hash) of
        true ->
            case request(<<"HEAD">>, StoreOpts, hb_util:to_lower(Hash), NodeOpts) of
                {ok, Status, _Headers, _Body} when Status == 200; Status == 204 ->
                    {ok, simple};
                {ok, 403, _Headers, _Body} ->
                    {error, protected};
                {ok, 404, _Headers, _Body} ->
                    {error, not_found};
                {ok, Status, _Headers, _Body} when Status >= 500 ->
                    {failure, {http_status, Status}};
                {ok, Status, _Headers, _Body} ->
                    {error, {http_status, Status}};
                {error, Reason} ->
                    {failure, Reason}
            end;
        false ->
            {error, not_found}
    end.

%% @doc Read a blob by SHA-384 hash and return it as a HyperBEAM message
%% carrying the encrypted bytes under `data' and a native `lbry-blob@1.0'
%% commitment. The hash of the returned bytes is verified before the message
%% is constructed; mismatching bytes never leave the store.
read(StoreOpts, #{ <<"read">> := Hash }, NodeOpts) ->
    case valid_hash(Hash) of
        true ->
            NormalizedHash = hb_util:to_lower(Hash),
            case request(<<"GET">>, StoreOpts, NormalizedHash, NodeOpts) of
                {ok, 200, _Headers, Body} ->
                    case hb_lbry_stream_descriptor:verify_blob_hash(NormalizedHash, Body) of
                        ok ->
                            ?event(lbry_blob,
                                {blob_hash_verified,
                                    {hash, NormalizedHash},
                                    {size, byte_size(Body)}},
                                NodeOpts
                            ),
                            {ok, hb_lbry_commitment:blob_message(NormalizedHash, Body)};
                        Error ->
                            ?event(lbry_blob,
                                {blob_hash_rejected, {hash, NormalizedHash}, {error, Error}},
                                NodeOpts
                            ),
                            Error
                    end;
                {ok, 403, _Headers, _Body} ->
                    {error, protected};
                {ok, 404, _Headers, _Body} ->
                    {error, not_found};
                {ok, Status, _Headers, _Body} when Status >= 500 ->
                    {failure, {http_status, Status}};
                {ok, Status, _Headers, _Body} ->
                    {error, {http_status, Status}};
                {error, Reason} ->
                    {failure, Reason}
            end;
        false ->
            {error, not_found}
    end.

request(Method, StoreOpts, Hash, NodeOpts) ->
    Node = hb_maps:get(<<"node">>, StoreOpts, ?DEFAULT_NODE, NodeOpts),
    Path = blob_path(Hash, StoreOpts, NodeOpts),
    HTTPOpts =
        case hb_maps:get(<<"http-client">>, StoreOpts, not_found, NodeOpts) of
            not_found -> NodeOpts;
            Client -> NodeOpts#{ <<"http-client">> => Client }
        end,
    ?event(lbry_blob,
        {blob_request, {method, Method}, {hash, Hash}, {node, Node}},
        NodeOpts
    ),
    Result =
        hb_http_client:request(
            #{
                peer => Node,
                path => Path,
                method => Method,
                headers => #{},
                body => <<>>
            },
            HTTPOpts
        ),
    case Result of
        {ok, Status, _Headers, Body} ->
            ?event(lbry_blob,
                {blob_response,
                    {method, Method},
                    {hash, Hash},
                    {status, Status},
                    {size, byte_size(Body)}},
                NodeOpts
            );
        {error, Reason} ->
            ?event(lbry_blob,
                {blob_request_failed, {method, Method}, {hash, Hash}, {error, Reason}},
                NodeOpts
            )
    end,
    Result.

blob_path(Hash, StoreOpts, Opts) ->
    case hb_maps:get(<<"edge-token">>, StoreOpts, not_found, Opts) of
        not_found ->
            <<"/blob?hash=", Hash/binary>>;
        Token ->
            Query =
                unicode:characters_to_binary(
                    uri_string:compose_query([
                        {<<"hash">>, Hash},
                        {<<"edge_token">>, hb_util:bin(Token)}
                    ])
                ),
            <<"/blob?", Query/binary>>
    end.

valid_hash(Hash) when is_binary(Hash), byte_size(Hash) == 96 ->
    try binary:decode_hex(Hash) of
        Decoded -> byte_size(Decoded) == 48
    catch
        _:_ -> false
    end;
valid_hash(_) ->
    false.

read_returns_committed_blob_message_test() ->
    application:ensure_all_started(inets),
    Body = <<"encrypted bytes">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Body),
    {ok, Server, Handle} = hb_mock_server:start([{"/blob", blob, {200, Body}}]),
    try
        Store = #{ <<"store-module">> => ?MODULE, <<"node">> => Server },
        {ok, Msg} =
            read(Store, #{ <<"read">> => Hash }, #{ <<"http-client">> => httpc }),
        ?assertEqual(Body, maps:get(<<"data">>, Msg)),
        ?assertEqual(Hash, maps:get(<<"blob-hash">>, Msg)),
        ?assertEqual(<<"lbry-blob@1.0">>, maps:get(<<"device">>, Msg)),
        ?assertEqual(
            true,
            hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
        )
    after
        hb_mock_server:stop(Handle)
    end.

cache_read_returns_blob_message_test() ->
    application:ensure_all_started(inets),
    Body = <<"encrypted bytes">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Body),
    {ok, Server, Handle} = hb_mock_server:start([{"/blob", blob, {200, Body}}]),
    try
        Store = #{
            <<"store-module">> => ?MODULE,
            <<"node">> => Server,
            <<"http-client">> => httpc
        },
        {ok, Msg} = hb_cache:read(Hash, #{ <<"store">> => [Store] }),
        ?assertEqual(Body, maps:get(<<"data">>, Msg)),
        ?assertEqual(
            true,
            hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
        )
    after
        hb_mock_server:stop(Handle)
    end.

read_rejects_hash_mismatch_test() ->
    application:ensure_all_started(inets),
    Body = <<"encrypted bytes">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(<<"expected bytes">>),
    {ok, Server, Handle} = hb_mock_server:start([{"/blob", blob, {200, Body}}]),
    try
        Store = #{ <<"store-module">> => ?MODULE, <<"node">> => Server },
        ?assertMatch(
            {error, {hash_mismatch, Hash, _}},
            read(Store, #{ <<"read">> => Hash }, #{ <<"http-client">> => httpc })
        )
    after
        hb_mock_server:stop(Handle)
    end.

edge_token_is_query_encoded_test() ->
    Body = <<"encrypted bytes">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Body),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/blob", blob, {200, Body}}
    ]),
    try
        Store = #{
            <<"store-module">> => ?MODULE,
            <<"node">> => Server,
            <<"edge-token">> => <<"a+b&c">>
        },
        {ok, #{ <<"data">> := Body }} =
            read(Store, #{ <<"read">> => Hash }, #{ <<"http-client">> => httpc }),
        [Req] = hb_mock_server:get_requests(Handle, blob),
        ?assertEqual(<<"hash=", Hash/binary, "&edge_token=a%2Bb%26c">>, maps:get(<<"qs">>, Req))
    after
        hb_mock_server:stop(Handle)
    end.

resolve_rejects_non_hash_test() ->
    ?assertEqual(
        {error, not_found},
        resolve(#{}, #{ <<"resolve">> => <<"not-a-hash">> }, #{})
    ).
