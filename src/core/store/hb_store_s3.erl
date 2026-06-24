%%% @doc An S3-compatible object store. Reads and writes opaque byte values to
%%% an S3-style HTTP endpoint using the `<endpoint>/<bucket>/<key>' addressing
%%% convention. Outbound requests are made through `hb_http_client', following
%%% the same pattern as the other HTTP-backed stores (`hb_store_gateway',
%%% `hb_store_lbry_blob').
%%%
%%% The store is configured via its `StoreOpts' message:
%%% ```
%%%     #{
%%%         <<"store-module">> => hb_store_s3,
%%%         <<"endpoint">>      => <<"https://s3.example.com">>,
%%%         <<"bucket">>        => <<"my-bucket">>,
%%%         %% Optional pluggable auth header (see AUTH below).
%%%         <<"authorization">> => <<"AWS4-HMAC-SHA256 ...">>,
%%%         %% Optional client selector, threaded into NodeOpts.
%%%         <<"http-client">>   => httpc
%%%     }
%%% '''
%%%
%%% AUTH: This pass supports a pluggable authorization header only. If the
%%% `<<"authorization">>' key is present in `StoreOpts' its value is sent
%%% verbatim as the HTTP `Authorization' header on every request, which also
%%% covers presigned-URL style deployments (where the credentials are baked
%%% into the endpoint/key and no header is needed). Full AWS Signature
%%% Version 4 request signing (canonical request construction, scoped signing
%%% keys, `x-amz-*' headers) is intentionally NOT implemented here and is a
%%% deferred follow-up — this module never fabricates a SigV4 signature.
%%%
%%% WRITE ROUTING (not implemented here): the intended deployment routes writes
%%% by size. A multi-store rule sends values whose byte size exceeds a
%%% configured threshold to this remote S3 store, while smaller values stay in a
%%% fast local store. That routing belongs to the multi-store layer (a list of
%%% store messages in the node config), not to this module; this module only
%%% implements the single-store read/write/type/resolve behaviour.
-module(hb_store_s3).
-export([scope/0, scope/1, read/3, write/3, type/3, resolve/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CONTENT_TYPE, <<"application/octet-stream">>).

%% @doc S3 is a remote store; reads and writes traverse the network.
scope() -> remote.
scope(_) -> scope().

%% @doc Resolve is an identity for S3: keys map directly to object names. There
%% are no links to follow in an object store.
resolve(_StoreOpts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    {ok, Key}.

%% @doc Report the type of the value at a key. Objects are always `simple'
%% binaries in an object store, so the result is `{ok, simple}' when the object
%% exists and `{error, not_found}' when it is absent. Presence is probed with a
%% GET, mirroring how `hb_store_gateway' derives a type from a read; an object
%% store offers no cheaper metadata probe through `hb_http_client' (`httpc'
%% rejects a HEAD that carries a body).
type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, _Bytes} -> {ok, simple};
        Other -> Other
    end.

%% @doc Read the bytes stored at `Key' via `GET <endpoint>/<bucket>/<key>'.
%% Returns `{ok, Bytes}' on success, `{error, not_found}' when the object is
%% absent, and `{failure, Reason}' for server-side or transport errors so the
%% store manager can retry or fall through to the next store.
read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    case request(<<"GET">>, StoreOpts, Key, <<>>, NodeOpts) of
        {ok, 200, _Headers, Body} ->
            {ok, Body};
        {ok, 404, _Headers, _Body} ->
            {error, not_found};
        {ok, Status, _Headers, _Body} when Status >= 500 ->
            {failure, {http_status, Status}};
        {ok, Status, _Headers, _Body} ->
            {error, {http_status, Status}};
        {error, Reason} ->
            {failure, Reason}
    end.

%% @doc Write each `Key => Value' pair in the request map via
%% `PUT <endpoint>/<bucket>/<key>'. Returns `ok' once every object has been
%% stored, or the first `{error, _}'/`{failure, _}' encountered.
write(StoreOpts, Req, NodeOpts) ->
    write_pairs(StoreOpts, hb_maps:to_list(Req, NodeOpts), NodeOpts).

write_pairs(_StoreOpts, [], _NodeOpts) ->
    ok;
write_pairs(StoreOpts, [{Key, Value} | Rest], NodeOpts) ->
    case request(<<"PUT">>, StoreOpts, Key, Value, NodeOpts) of
        {ok, Status, _Headers, _Body} when Status >= 200, Status < 300 ->
            write_pairs(StoreOpts, Rest, NodeOpts);
        {ok, Status, _Headers, _Body} when Status >= 500 ->
            {failure, {http_status, Status}};
        {ok, Status, _Headers, _Body} ->
            {error, {http_status, Status}};
        {error, Reason} ->
            {failure, Reason}
    end.

%% @doc Issue an HTTP request to `<endpoint>/<bucket>/<key>' through
%% `hb_http_client', threading the optional per-store `http-client' selector
%% into the node options and attaching the pluggable `Authorization' header
%% when configured.
request(Method, StoreOpts, Key, Body, NodeOpts) ->
    Endpoint = hb_maps:get(<<"endpoint">>, StoreOpts, not_found, NodeOpts),
    Bucket = hb_maps:get(<<"bucket">>, StoreOpts, not_found, NodeOpts),
    Path = object_path(Bucket, Key),
    Headers = auth_headers(StoreOpts, NodeOpts),
    HTTPOpts =
        case hb_maps:get(<<"http-client">>, StoreOpts, not_found, NodeOpts) of
            not_found -> NodeOpts;
            Client -> NodeOpts#{ <<"http-client">> => Client }
        end,
    ?event(store_s3,
        {s3_request, {method, Method}, {endpoint, Endpoint}, {path, Path}},
        NodeOpts
    ),
    hb_http_client:request(
        #{
            peer => Endpoint,
            path => Path,
            method => Method,
            headers => Headers,
            body => Body
        },
        HTTPOpts
    ).

%% @doc Build the request path as `/<bucket>/<key>'. The key is percent-encoded
%% so that nested paths and reserved characters are transmitted safely.
object_path(Bucket, Key) ->
    EncodedKey = uri_string:quote(hb_util:bin(Key)),
    <<"/", (hb_util:bin(Bucket))/binary, "/", EncodedKey/binary>>.

auth_headers(StoreOpts, NodeOpts) ->
    Base = #{ <<"content-type">> => ?CONTENT_TYPE },
    case hb_maps:get(<<"authorization">>, StoreOpts, not_found, NodeOpts) of
        not_found -> Base;
        Auth -> Base#{ <<"authorization">> => hb_util:bin(Auth) }
    end.
