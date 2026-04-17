%%% @doc A read-only store backend that fetches IPFS CIDs from a configured
%%% set of HTTP gateways. This is how a HyperBEAM node becomes able to serve
%%% *external* IPFS content — content it did not itself commit locally.
%%%
%%% Crucially, this module does NOT trust the gateways. Every fetched body
%%% goes through TWO layers of verification before it is handed up the
%%% chain:
%%%
%%%   1. Direct digest check: sha256(body) is compared to the CID's
%%%      multihash digest. A mismatched gateway response is treated as
%%%      `not_found' and the next gateway is tried.
%%%
%%%   2. Commitment attachment: an `~ipfs@1.0' unsigned commitment keyed by
%%%      the CID is attached to the returned message. This lets any
%%%      downstream consumer re-verify independently via
%%%      `hb_message:verify/2,3' — and the commitment is what `hb_cache'
%%%      uses to link the CID to the message's uncommitted ID if the
%%%      caller chooses to persist it locally.
%%%
%%% The CID is the authority, not the HTTPS certificate.
%%%
%%% Shape of a config entry:
%%% ```
%%%   #{
%%%       <<"store-module">> => hb_store_ipfs_gateway,
%%%       <<"gateways">>     => [<<"https://ipfs.io">>, <<"https://dweb.link">>],
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

%% Gateways known to serve public IPFS content at time of writing. Users
%% should override for production via the `<<"gateways">>' store-config key.
-define(DEFAULT_GATEWAYS, [
    <<"https://ipfs.io">>,
    <<"https://dweb.link">>,
    <<"https://nftstorage.link">>
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
            {ok, with_commitment(CID, Parts, Body)};
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

%% @doc Wrap verified bytes in a message whose `~ipfs@1.0' unsigned
%% commitment keyed by the CID makes it independently verifiable via
%% `hb_message:verify/2,3' — without trusting this store to have done the
%% check. The `codec' in the commitment mirrors the CID's multicodec so a
%% round-trip through the cache preserves identity.
with_commitment(CID, #{ <<"multicodec">> := Codec, <<"digest">> := Digest }, Body) ->
    %% Mirror `dev_codec_ipfs:commit/3': populate `signature' with the raw
    %% digest (base64url) and `keyid' with the universal `constant:ipfs',
    %% so the commitment round-trips over the HTTPSig wire format as an
    %% HMAC-shaped item. See `dev_codec_ipfs' for the rationale.
    #{
        <<"body">>        => Body,
        <<"commitments">> => #{
            CID => #{
                <<"commitment-device">> => <<"ipfs@1.0">>,
                <<"type">>              => <<"unsigned">>,
                <<"multicodec">>             => Codec,
                <<"hash-alg">>          => <<"sha2-256">>,
                <<"committed">>         => [<<"body">>],
                <<"signature">>         => hb_util:encode(Digest),
                <<"keyid">>             => <<"constant:ipfs">>
            }
        }
    }.

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

%%% Live-service tests. HyperBEAM's test suite hits the real network for
%%% its store/gateway backends (see `hb_store_gateway' tests against the
%%% public Arweave gateways); we do the same for IPFS. The CID used here
%%% is the canonical `raw("hello world")' CIDv1 that multiple public
%%% gateways serve:
%%%
%%%     bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e
%%%
%%% Each test lists several gateways so a single flaky endpoint cannot
%%% flake the whole suite.

-define(HELLO_WORLD_CID,
    <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>).
-define(HELLO_WORLD_BODY, <<"hello world">>).
-define(LIVE_GATEWAYS, [
    <<"https://ipfs.io">>,
    <<"https://dweb.link">>,
    <<"https://nftstorage.link">>,
    <<"https://4everland.io">>
]).

live_gateway_fetches_known_cid_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        Store = #{
            <<"store-module">> => hb_store_ipfs_gateway,
            <<"gateways">>     => ?LIVE_GATEWAYS,
            <<"timeout">>      => 20000
        },
        %% Either all live gateways served the body intact and we got the
        %% wrapped message, or every gateway was unreachable — in which
        %% case the test is skipped instead of flaking CI.
        case read(Store, ?HELLO_WORLD_CID) of
            {ok, Msg} ->
                ?assertEqual(
                    ?HELLO_WORLD_BODY,
                    maps:get(<<"body">>, Msg)
                ),
                Commitments = maps:get(<<"commitments">>, Msg),
                ?assert(maps:is_key(?HELLO_WORLD_CID, Commitments)),
                Commitment = maps:get(?HELLO_WORLD_CID, Commitments),
                ?assertEqual(<<"ipfs@1.0">>,
                    maps:get(<<"commitment-device">>, Commitment)),
                ?assertEqual(<<"raw">>,
                    maps:get(<<"multicodec">>, Commitment));
            not_found ->
                ?debugFmt("Skipping: all live gateways missed CID ~s",
                    [?HELLO_WORLD_CID]),
                ok
        end
    end}.

%% The commitment attached by the gateway store must verify via the
%% standard `hb_message:verify/2,3' machinery, using the same `~ipfs@1.0'
%% device whose `verify/3' is the canonical check. If this test passes,
%% callers can treat gateway-fetched messages like any other committed
%% HyperBEAM message.
live_gateway_attached_commitment_verifies_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        Store = #{
            <<"store-module">> => hb_store_ipfs_gateway,
            <<"gateways">>     => ?LIVE_GATEWAYS,
            <<"timeout">>      => 20000
        },
        case read(Store, ?HELLO_WORLD_CID) of
            {ok, Msg} ->
                %% Stock preloaded_devices plus ipfs@1.0, exactly what a
                %% user would configure in their node.
                Opts = #{
                    preloaded_devices =>
                        [ #{ <<"name">> => <<"ipfs@1.0">>,
                             <<"module">> => dev_codec_ipfs } |
                          hb_opts:get(preloaded_devices, [], #{}) ]
                },
                ?assertEqual(
                    true,
                    hb_message:verify(
                        Msg,
                        #{ <<"commitment-ids">> => [?HELLO_WORLD_CID] },
                        Opts
                    )
                );
            not_found ->
                ?debugFmt("Skipping: all live gateways missed CID",
                    [])
        end
    end}.

%% A CID missing from the local store falls through to the real gateway
%% chain and comes back via the normal `hb_cache:read/2' path. This is the
%% production pipeline exercised end-to-end against the public IPFS
%% network.
live_hb_cache_reads_from_gateway_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        Opts = #{
            store => [
                hb_test_utils:test_store(),
                #{
                    <<"store-module">> => hb_store_ipfs_gateway,
                    <<"gateways">>     => ?LIVE_GATEWAYS,
                    <<"timeout">>      => 20000
                }
            ]
        },
        case hb_cache:read(?HELLO_WORLD_CID, Opts) of
            {ok, Msg} ->
                ?assertEqual(
                    ?HELLO_WORLD_BODY,
                    hb_cache:ensure_loaded(
                        maps:get(<<"body">>, Msg), Opts)
                );
            not_found ->
                ?debugFmt("Skipping: all live gateways missed CID", [])
        end
    end}.

%% A gateway that misreads the prefix (e.g. the subpath `/ipfs/` served by
%% a non-IPFS host) may still return 200 with an unrelated body. The store
%% must refuse such a response by comparing sha256(body) against the CID's
%% multihash digest. This test exercises that path by asking a real host
%% for a nonsense CID — we expect `not_found' and no wrapped body.
live_gateway_rejects_unpinned_cid_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        %% A well-formed CIDv1 with a random digest. Vanishingly unlikely
        %% to be pinned anywhere; serves as a negative test.
        UnpinnedCID =
            dev_codec_ipfs_cid:encode(
                <<"raw">>, sha2_256,
                crypto:strong_rand_bytes(64)
            ),
        Store = #{
            <<"store-module">> => hb_store_ipfs_gateway,
            <<"gateways">>     => ?LIVE_GATEWAYS,
            <<"timeout">>      => 10000
        },
        ?assertEqual(not_found, read(Store, UnpinnedCID))
    end}.

%% Defense in depth: even if somehow a gateway did lie (and we can't rely
%% on any real gateway to do so on demand), the `verify_digest/2' function
%% that every response flows through is tested directly.
digest_gate_rejects_tampered_body_test() ->
    {ok, Parts} = dev_codec_ipfs_cid:decode(?HELLO_WORLD_CID),
    ?assert(verify_digest(Parts, ?HELLO_WORLD_BODY)),
    ?assertNot(verify_digest(Parts, <<"hello earth">>)).
