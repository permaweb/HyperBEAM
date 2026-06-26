%%% @doc An S3-compatible object store. Reads and writes opaque byte values to
%%% an S3-style HTTP endpoint using the `<endpoint>/<bucket>/<key>' addressing
%%% convention, with a symbolic-link layer compatible with the rest of
%%% HyperBEAM's stores. Outbound requests are made through `hb_http_client',
%%% following the same pattern as the other HTTP-backed stores
%%% (`hb_store_gateway', `hb_store_lbry_blob').
%%%
%%% The store is configured via its `StoreOpts' message:
%%% ```
%%%     #{
%%%         <<"store-module">> => hb_store_s3,
%%%         <<"endpoint">>      => <<"https://s3.example.com">>,
%%%         <<"bucket">>        => <<"my-bucket">>,
%%%         <<"prefix">>        => <<"optional/key/prefix">>,
%%%         <<"authorization">> => <<"AWS4-HMAC-SHA256 ...">>,  %% see AUTH
%%%         <<"http-client">>   => httpc                       %% optional
%%%     }
%%% '''
%%%
%%% LINKS: links are stored as a small object at `lnk/<key>' whose body is
%%% `link:<target>', matching the convention used by the LMDB store. `read' and
%%% `resolve' follow a link chain (bounded by `?MAX_LINK_DEPTH'); `link' writes
%%% the marker. `group' is a no-op: in an object store, composite groups are an
%%% emergent property of the key prefix, so no marker object is written.
%%%
%%% AUTH: this module supports a pluggable `authorization' header only (sent
%%% verbatim, which also covers presigned-URL deployments). Full AWS Signature
%%% Version 4 signing and object `list'ing (S3 `ListObjectsV2') are intentionally
%%% NOT implemented here: the canonical production S3 store is the `erlcloud'-
%%% backed `hb_store_s3' on the `newest/hb_store_s3' branch, which already does
%%% SigV4 and listing. The convergence path is to adopt that transport under this
%%% same current store contract (see aidocs/017); this module never fabricates a
%%% SigV4 signature, and `list' is therefore not offered here.
%%%
%%% WRITE ROUTING (not implemented here): routing writes above a size threshold
%%% to this remote store belongs to the multi-store layer (a list of store
%%% messages in node config), not to this module.
-module(hb_store_s3).
-export([start/1, stop/1, reset/1, scope/0, scope/1]).
-export([read/3, write/3, type/3, resolve/3, link/3, group/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CONTENT_TYPE, <<"application/octet-stream">>).
-define(LINK_MARKER, <<"link:">>).
-define(MAX_LINK_DEPTH, 1000).

%% @doc S3 is a remote store with no local lifecycle to manage.
start(_StoreOpts) -> ok.
stop(_StoreOpts) -> ok.
%% Reset is a deliberate no-op: a remote object store is not mass-deleted on
%% node reset.
reset(_StoreOpts) -> ok.

%% @doc S3 is a remote store; reads and writes traverse the network.
scope() -> remote.
scope(_) -> scope().

%% @doc Resolve a key by following its link chain to the terminal target. With
%% no endpoint configured (or no link present) this is the identity.
resolve(StoreOpts, #{ <<"resolve">> := Key }, NodeOpts) ->
    {ok, follow_links(StoreOpts, hb_util:bin(Key), 0, NodeOpts)}.

%% @doc Report the type of the value at a key: `{ok, simple}' when the (resolved)
%% object exists, otherwise the read's error. Listing-based `composite'
%% detection is deferred with `list' (see the module doc).
type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, _Bytes} -> {ok, simple};
        Other -> Other
    end.

%% @doc Read the bytes stored at `Key', first following any link chain, via
%% `GET <endpoint>/<bucket>/<key>'. Returns `{ok, Bytes}', `{error, not_found}',
%% or `{failure, Reason}' (server/transport errors) so the store manager can
%% fall through to the next store.
read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    Resolved = follow_links(StoreOpts, hb_util:bin(Key), 0, NodeOpts),
    get_object(StoreOpts, Resolved, NodeOpts).

get_object(StoreOpts, Key, NodeOpts) ->
    case request(<<"GET">>, StoreOpts, Key, <<>>, NodeOpts) of
        {ok, 200, _Headers, Body} -> {ok, Body};
        {ok, 404, _Headers, _Body} -> {error, not_found};
        {ok, Status, _Headers, _Body} when Status >= 500 ->
            {failure, {http_status, Status}};
        {ok, Status, _Headers, _Body} -> {error, {http_status, Status}};
        {error, Reason} -> {failure, Reason}
    end.

%% @doc Write each `Key => Value' pair in the request map via
%% `PUT <endpoint>/<bucket>/<key>'.
write(StoreOpts, Req, NodeOpts) ->
    maps:fold(
        fun(_Key, _Value, {Bad, _} = Error) when Bad == error; Bad == failure ->
                Error;
           (Key, Value, ok) ->
                put_object(StoreOpts, hb_util:bin(Key), Value, NodeOpts)
        end,
        ok,
        Req
    ).

put_object(StoreOpts, Key, Value, NodeOpts) ->
    case request(<<"PUT">>, StoreOpts, Key, Value, NodeOpts) of
        {ok, Status, _Headers, _Body} when Status >= 200, Status < 300 -> ok;
        {ok, Status, _Headers, _Body} when Status >= 500 ->
            {failure, {http_status, Status}};
        {ok, Status, _Headers, _Body} -> {error, {http_status, Status}};
        {error, Reason} -> {failure, Reason}
    end.

%% @doc Create symbolic links. The request maps each new path to the existing
%% target it should resolve to. A link is a small object at `lnk/<new>' whose
%% body is `link:<existing>'.
link(StoreOpts, Req, NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(_New, _Existing, {Bad, _} = Error) when Bad == error; Bad == failure ->
                Error;
           (New, Existing, ok) ->
                put_object(
                    StoreOpts,
                    link_key(hb_util:bin(New)),
                    <<?LINK_MARKER/binary, (hb_util:bin(Existing))/binary>>,
                    NodeOpts
                )
        end,
        ok,
        Req
    ).

%% @doc Groups are implicit in the key prefix for an object store; there is no
%% marker object to write.
group(_StoreOpts, #{ <<"group">> := _Path }, _NodeOpts) ->
    ok.

%% @doc Follow a link chain to its terminal key. Reads `lnk/<key>'; if it holds a
%% `link:<target>' marker, recurse on the target, bounded by `?MAX_LINK_DEPTH'.
%% Without a configured endpoint (or any link), the key is returned unchanged.
follow_links(StoreOpts, Key, Depth, NodeOpts) ->
    case hb_maps:get(<<"endpoint">>, StoreOpts, not_found, NodeOpts) of
        not_found -> Key;
        _ when Depth >= ?MAX_LINK_DEPTH -> Key;
        _ ->
            case request(<<"GET">>, StoreOpts, link_key(Key), <<>>, NodeOpts) of
                {ok, 200, _Headers, Body} ->
                    case parse_link(Body) of
                        {true, Target} ->
                            follow_links(StoreOpts, Target, Depth + 1, NodeOpts);
                        false ->
                            Key
                    end;
                _ ->
                    Key
            end
    end.

link_key(Key) -> <<"lnk/", Key/binary>>.

parse_link(<<"link:", Target/binary>>) -> {true, Target};
parse_link(_) -> false.

%% @doc Issue an HTTP request to `<endpoint>/<bucket>/<key>' through
%% `hb_http_client', threading the optional per-store `http-client' selector and
%% attaching the pluggable `Authorization' header when configured.
request(Method, StoreOpts, Key, Body, NodeOpts) ->
    Endpoint = hb_maps:get(<<"endpoint">>, StoreOpts, not_found, NodeOpts),
    Bucket = hb_maps:get(<<"bucket">>, StoreOpts, not_found, NodeOpts),
    Path = object_path(StoreOpts, Bucket, Key, NodeOpts),
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

%% @doc Build the request path as `/<bucket>/<prefix>/<key>', percent-encoding
%% the key so nested paths and reserved characters transmit safely.
object_path(StoreOpts, Bucket, Key, NodeOpts) ->
    Prefixed =
        case hb_maps:get(<<"prefix">>, StoreOpts, <<>>, NodeOpts) of
            <<>> -> hb_util:bin(Key);
            Prefix -> <<(hb_util:bin(Prefix))/binary, "/", (hb_util:bin(Key))/binary>>
        end,
    EncodedKey = uri_string:quote(Prefixed),
    <<"/", (hb_util:bin(Bucket))/binary, "/", EncodedKey/binary>>.

auth_headers(StoreOpts, NodeOpts) ->
    Base = #{ <<"content-type">> => ?CONTENT_TYPE },
    case hb_maps:get(<<"authorization">>, StoreOpts, not_found, NodeOpts) of
        not_found -> Base;
        Auth -> Base#{ <<"authorization">> => hb_util:bin(Auth) }
    end.
