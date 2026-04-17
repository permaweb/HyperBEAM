%%% @doc A read-only store backend that fetches IPFS CIDs from a configured
%%% set of HTTP gateways. This is how a HyperBEAM node becomes able to serve
%%% *external* IPFS content — content it did not itself commit locally.
%%%
%%% Crucially, this module does NOT trust the gateways. Every fetched body
%%% is hashed and compared to the requested CID before it is returned; a
%%% mismatched gateway response is treated as `not_found' and the next
%%% gateway is tried. The CID is the authority, not the HTTPS certificate.
%%%
%%% Shape of a config entry:
%%% ```
%%%   #{
%%%       <<"store-module">> => hb_store_ipfs_gateway,
%%%       <<"gateways">>     => [<<"https://w3s.link">>, <<"https://ipfs.io">>],
%%%       <<"timeout">>      => 15000  %% ms, optional, default 15_000
%%%   }
%%% '''
%%% Put this after your local stores so it acts as a read-through fallback.
%%% No `write/3' is exposed: this is a consumer-only view of IPFS.
%%%
%%% Keys that do not parse as CIDv1 are ignored quickly and return `not_found'
%%% so that this module can live safely in a chain alongside Arweave-addressed
%%% stores without stepping on their toes.
-module(hb_store_ipfs_gateway).
-export([scope/1, type/2, read/2, resolve/2, list/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_GATEWAYS, [
    <<"https://w3s.link">>,
    <<"https://ipfs.io">>
]).
-define(DEFAULT_TIMEOUT_MS, 15000).

%% @doc Gateway scope is always remote; prefer local stores in the chain.
scope(_) -> remote.

%% @doc Keys are returned as-is. We never alias CIDs to anything else.
resolve(_, Key) -> Key.

%% @doc A CID resolves to a single-binary `body' — IPFS has no composite
%% structure at this edge of the spec.
type(_, Key) ->
    case cid_of_key(Key) of
        {ok, _, _} -> simple;
        error -> not_found
    end.

%% @doc `list/2' on a CID returns the keys of the one-field message we wrap
%% the body in — conforming to the general store contract.
list(StoreOpts, Key) ->
    case read(StoreOpts, Key) of
        {ok, Message} when is_map(Message) ->
            {ok, hb_maps:keys(Message, StoreOpts)};
        Other -> Other
    end.

%% @doc Fetch the CID from one of the configured gateways. Tries each in
%% order. Returns `not_found' if every gateway misses; `failure' only if
%% something systemic broke. A digest mismatch is a miss, not a failure —
%% that is how we stop malicious gateways from poisoning the cache.
read(StoreOpts, Key) ->
    case cid_of_key(Key) of
        error ->
            ?event(ipfs_gateway, {ignoring_non_cid, Key}),
            not_found;
        {ok, CID, Parts} ->
            Gateways = hb_maps:get(<<"gateways">>, StoreOpts,
                ?DEFAULT_GATEWAYS, StoreOpts),
            Timeout = hb_maps:get(<<"timeout">>, StoreOpts,
                ?DEFAULT_TIMEOUT_MS, StoreOpts),
            try_gateways(Gateways, CID, Parts, Timeout, StoreOpts)
    end.

%%%====================================================================
%%% Internals
%%%====================================================================

%% @doc Parse a store key into a CID (binary) and its pre-decoded parts.
%% Accepts: a 59-ish-char CIDv1 binary, or a `[CID]' single-element path
%% list. Longer paths are rejected in phase 1 — we have no UnixFS/IPLD path
%% resolver yet, and silently returning the root would be misleading.
cid_of_key(Key) when is_binary(Key) ->
    try_parse_cid(Key);
cid_of_key([Single]) ->
    try_parse_cid(Single);
cid_of_key(_) ->
    error.

try_parse_cid(CID) when is_binary(CID) ->
    case dev_codec_ipfs_cid:decode(CID) of
        {ok, Parts} -> {ok, CID, Parts};
        {error, _} -> error
    end;
try_parse_cid(_) ->
    error.

try_gateways([], CID, _Parts, _Timeout, _Opts) ->
    ?event(ipfs_gateway, {all_gateways_missed, {cid, CID}}),
    not_found;
try_gateways([Gateway|Rest], CID, Parts, Timeout, Opts) ->
    case fetch_and_verify(Gateway, CID, Parts, Timeout, Opts) of
        {ok, Body} ->
            ?event(ipfs_gateway, {fetched, {cid, CID}, {gateway, Gateway},
                {bytes, byte_size(Body)}}),
            {ok, #{ <<"body">> => Body }};
        digest_mismatch ->
            %% Try the next gateway — this one lied.
            ?event(warning, {ipfs_gateway_digest_mismatch,
                {cid, CID}, {gateway, Gateway}}),
            try_gateways(Rest, CID, Parts, Timeout, Opts);
        not_found ->
            try_gateways(Rest, CID, Parts, Timeout, Opts);
        {error, Reason} ->
            ?event(ipfs_gateway, {gateway_error,
                {cid, CID}, {gateway, Gateway}, {reason, Reason}}),
            try_gateways(Rest, CID, Parts, Timeout, Opts)
    end.

%% @doc Single-gateway fetch. Uses OTP's `httpc' — no new dependency — and
%% verifies the body hash against the requested CID before returning.
fetch_and_verify(Gateway, CID, Parts, Timeout, _Opts) ->
    URL = binary_to_list(<<Gateway/binary, "/ipfs/", CID/binary>>),
    Headers = [
        {"accept", "application/vnd.ipld.raw, application/octet-stream"},
        {"user-agent", "hyperbeam-ipfs/1.0"}
    ],
    Request = {URL, Headers},
    HTTPOpts = [{timeout, Timeout}, {connect_timeout, Timeout}],
    Opts = [{body_format, binary}, {full_result, true}],
    case httpc:request(get, Request, HTTPOpts, Opts) of
        {ok, {{_, 200, _}, _RespHeaders, Body}} when is_binary(Body) ->
            case verify_digest(Parts, Body) of
                true -> {ok, Body};
                false -> digest_mismatch
            end;
        {ok, {{_, 404, _}, _, _}} -> not_found;
        {ok, {{_, Status, _}, _, _}} -> {error, {http_status, Status}};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Compare a gateway-returned body against the digest embedded in the
%% CID. Only sha2-256 is in scope for phase 1, which matches what every
%% current public gateway returns for a `bafk...' / `bafy...' v1 CID.
verify_digest(#{ <<"hash-alg">> := <<"sha2-256">>, <<"digest">> := Expected },
              Body) ->
    Expected =:= crypto:hash(sha256, Body);
verify_digest(_, _) ->
    false.

%%%====================================================================
%%% Tests
%%%====================================================================
%%% See `hb_store_ipfs_gateway_test' for end-to-end stubs using cowboy.

cid_of_key_test() ->
    CID = <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
    ?assertMatch({ok, CID, #{}}, cid_of_key(CID)),
    ?assertMatch({ok, CID, #{}}, cid_of_key([CID])),
    ?assertEqual(error, cid_of_key(<<"not-a-cid">>)),
    %% Arweave-style IDs (43-char base64url) must NOT be claimed by us.
    ?assertEqual(error,
        cid_of_key(<<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>)),
    %% Multi-part paths are out of scope in phase 1.
    ?assertEqual(error, cid_of_key([CID, <<"sub">>])).

verify_digest_accepts_correct_body_test() ->
    Body = <<"hello world">>,
    Parts = #{
        <<"hash-alg">> => <<"sha2-256">>,
        <<"digest">>   => crypto:hash(sha256, Body)
    },
    ?assert(verify_digest(Parts, Body)).

verify_digest_rejects_tampered_body_test() ->
    Parts = #{
        <<"hash-alg">> => <<"sha2-256">>,
        <<"digest">>   => crypto:hash(sha256, <<"hello world">>)
    },
    ?assertNot(verify_digest(Parts, <<"hello earth">>)).

scope_is_remote_test() ->
    ?assertEqual(remote, scope(#{})).

read_ignores_non_cid_test() ->
    ?assertEqual(not_found,
        read(#{}, <<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>)).

%% End-to-end with a cowboy stub: a well-behaved gateway returns the body,
%% digest matches, and `read/2' returns the wrapped message.
gateway_happy_path_test() ->
    application:ensure_all_started(inets),
    CID = <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
    Body = <<"hello world">>,
    {ok, URL, Handle} = hb_mock_server:start([
        {<<"/ipfs/", CID/binary>>, ipfs, {200, Body}}
    ]),
    try
        Store = #{
            <<"store-module">> => hb_store_ipfs_gateway,
            <<"gateways">>     => [URL]
        },
        ?assertEqual({ok, #{ <<"body">> => Body }}, read(Store, CID))
    after
        hb_mock_server:stop(Handle)
    end.

%% A lying gateway: returns bytes that don't hash to the requested CID.
%% The store must refuse (digest_mismatch) and ultimately `not_found'
%% because there are no other gateways to try.
gateway_digest_mismatch_test() ->
    application:ensure_all_started(inets),
    CID = <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
    {ok, URL, Handle} = hb_mock_server:start([
        {<<"/ipfs/", CID/binary>>, ipfs, {200, <<"hello earth">>}}
    ]),
    try
        Store = #{
            <<"store-module">> => hb_store_ipfs_gateway,
            <<"gateways">>     => [URL]
        },
        ?assertEqual(not_found, read(Store, CID))
    after
        hb_mock_server:stop(Handle)
    end.

%% Two gateways: the first returns tampered bytes, the second returns the
%% correct body. The store must fall through to the honest one.
gateway_fallthrough_test() ->
    application:ensure_all_started(inets),
    CID = <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
    Body = <<"hello world">>,
    {ok, BadURL, BadH} = hb_mock_server:start([
        {<<"/ipfs/", CID/binary>>, ipfs, {200, <<"lies">>}}
    ]),
    {ok, GoodURL, GoodH} = hb_mock_server:start([
        {<<"/ipfs/", CID/binary>>, ipfs, {200, Body}}
    ]),
    try
        Store = #{
            <<"store-module">> => hb_store_ipfs_gateway,
            <<"gateways">>     => [BadURL, GoodURL]
        },
        ?assertEqual({ok, #{ <<"body">> => Body }}, read(Store, CID))
    after
        hb_mock_server:stop(BadH),
        hb_mock_server:stop(GoodH)
    end.

gateway_404_falls_through_test() ->
    application:ensure_all_started(inets),
    CID = <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
    Body = <<"hello world">>,
    {ok, URL404, H404} = hb_mock_server:start([
        {<<"/ipfs/", CID/binary>>, ipfs, {404, <<"missing">>}}
    ]),
    {ok, GoodURL, GoodH} = hb_mock_server:start([
        {<<"/ipfs/", CID/binary>>, ipfs, {200, Body}}
    ]),
    try
        Store = #{
            <<"store-module">> => hb_store_ipfs_gateway,
            <<"gateways">>     => [URL404, GoodURL]
        },
        ?assertEqual({ok, #{ <<"body">> => Body }}, read(Store, CID))
    after
        hb_mock_server:stop(H404),
        hb_mock_server:stop(GoodH)
    end.

%% Integration with `hb_cache' — a CID missing from the local store falls
%% through to the gateway chain. This is how a production node actually
%% serves external IPFS content.
hb_cache_reads_from_gateway_test() ->
    application:ensure_all_started(inets),
    CID = <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
    Body = <<"hello world">>,
    {ok, URL, Handle} = hb_mock_server:start([
        {<<"/ipfs/", CID/binary>>, ipfs, {200, Body}}
    ]),
    try
        Opts = #{
            store => [
                hb_test_utils:test_store(),  %% local, empty
                #{
                    <<"store-module">> => hb_store_ipfs_gateway,
                    <<"gateways">>     => [URL]
                }
            ]
        },
        {ok, Msg} = hb_cache:read(CID, Opts),
        ?assertEqual(Body,
            hb_cache:ensure_loaded(maps:get(<<"body">>, Msg), Opts))
    after
        hb_mock_server:stop(Handle)
    end.
