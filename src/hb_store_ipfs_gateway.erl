%%% @doc Read-only store backend that fetches IPFS CIDs from a configured
%%% set of HTTP gateways, verifies the body hashes to the requested CID,
%%% and attaches an `~ipfs@1.0' unsigned commitment so the message remains
%%% independently verifiable via `hb_message:verify/2,3'. The CID is the
%%% authority, not the HTTPS certificate.
%%%
%%% Config entry:
%%% ```
%%%   #{
%%%       <<"store-module">> => hb_store_ipfs_gateway,
%%%       <<"gateways">>     => [<<"https://ipfs.io">>, ...],
%%%       <<"timeout">>      => 15000
%%%   }
%%% '''
%%% Place after local stores for read-through semantics. Non-CIDv1 keys are
%%% ignored so the module is safe alongside Arweave-addressed stores.
-module(hb_store_ipfs_gateway).
-export([scope/1, type/2, read/2, resolve/2, list/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_GATEWAYS, [
    <<"https://ipfs.io">>,
    <<"https://dweb.link">>,
    <<"https://nftstorage.link">>
]).
-define(DEFAULT_TIMEOUT_MS, 15000).

%% @doc Always remote — prefer local stores in the chain.
scope(_) -> remote.

%% @doc CIDs are never aliased.
resolve(_, Key) -> Key.

%% @doc IPFS at this edge of the spec has no composite structure.
type(_, Key) ->
    case cid_of_key(Key) of
        {ok, _, _} -> simple;
        error      -> not_found
    end.

%% @doc Return the keys of the wrapping message for a fetched CID.
list(StoreOpts, Key) ->
    case read(StoreOpts, Key) of
        {ok, Message} when is_map(Message) ->
            {ok, hb_maps:keys(Message, StoreOpts)};
        Other -> Other
    end.

%% @doc Fetch the CID from one of the configured gateways, in order. A
%% digest mismatch is treated as a miss (the gateway lied) and the next
%% gateway is tried. Returns `not_found' if every gateway misses.
read(StoreOpts, Key) ->
    case cid_of_key(Key) of
        error ->
            ?event(ipfs_gateway, {ignoring_non_cid, Key}),
            not_found;
        {ok, CID, Parts} ->
            Gateways =
                hb_maps:get(<<"gateways">>, StoreOpts,
                            ?DEFAULT_GATEWAYS, StoreOpts),
            Timeout =
                hb_maps:get(<<"timeout">>, StoreOpts,
                            ?DEFAULT_TIMEOUT_MS, StoreOpts),
            try_gateways(Gateways, CID, Parts, Timeout)
    end.

%% @doc Parse a key into a CID and its pre-decoded parts. Accepts a bare
%% CIDv1 binary or a single-element path list; longer paths are rejected
%% (no UnixFS/IPLD path resolver yet).
cid_of_key(Key) when is_binary(Key) ->
    try_parse_cid(Key);
cid_of_key([Single]) ->
    try_parse_cid(Single);
cid_of_key(_) ->
    error.

try_parse_cid(CID) when is_binary(CID) ->
    case dev_codec_ipfs_cid:decode(CID) of
        {ok, Parts} -> {ok, CID, Parts};
        {error, _}  -> error
    end;
try_parse_cid(_) ->
    error.

try_gateways([], CID, _Parts, _Timeout) ->
    ?event(ipfs_gateway, {all_gateways_missed, {cid, CID}}),
    not_found;
try_gateways([Gateway|Rest], CID, Parts, Timeout) ->
    case fetch_and_verify(Gateway, CID, Parts, Timeout) of
        {ok, Body} ->
            ?event(ipfs_gateway,
                {fetched, {cid, CID}, {gateway, Gateway},
                 {bytes, byte_size(Body)}}),
            {ok, with_commitment(CID, Parts, Body)};
        digest_mismatch ->
            ?event(warning,
                {ipfs_gateway_digest_mismatch,
                 {cid, CID}, {gateway, Gateway}}),
            try_gateways(Rest, CID, Parts, Timeout);
        Other ->
            ?event(ipfs_gateway,
                {gateway_miss, {cid, CID},
                 {gateway, Gateway}, {reason, Other}}),
            try_gateways(Rest, CID, Parts, Timeout)
    end.

%% @doc Wrap verified bytes in a message whose `~ipfs@1.0' unsigned
%% commitment is keyed by the CID, so any downstream consumer can
%% re-verify independently. Mirrors `dev_codec_ipfs:commit/3' — signature =
%% raw digest (keeps the commitment on the httpsig wire), no keyid (no
%% key material needed for content-addressed commitments).
with_commitment(CID,
                #{ <<"hash-alg">> := HashAlg, <<"digest">> := Digest },
                Body) ->
    #{
        <<"body">>        => Body,
        <<"commitments">> => #{
            CID => #{
                <<"commitment-device">> => <<"ipfs@1.0">>,
                <<"type">>              => HashAlg,
                <<"committed">>         => [<<"body">>],
                <<"signature">>         => hb_util:encode(Digest)
            }
        }
    }.

%% @doc Fetch a single gateway; verify the body against the CID digest
%% before returning. Uses OTP `httpc' — no new dependency.
fetch_and_verify(Gateway, CID, Parts, Timeout) ->
    URL = binary_to_list(<<Gateway/binary, "/ipfs/", CID/binary>>),
    Headers = [
        {"accept", "application/vnd.ipld.raw, application/octet-stream"},
        {"user-agent", "hyperbeam-ipfs/1.0"}
    ],
    HTTPOpts = [{timeout, Timeout}, {connect_timeout, Timeout}],
    Opts = [{body_format, binary}, {full_result, true}],
    case httpc:request(get, {URL, Headers}, HTTPOpts, Opts) of
        {ok, {{_, 200, _}, _, Body}} when is_binary(Body) ->
            case verify_digest(Parts, Body) of
                true  -> {ok, Body};
                false -> digest_mismatch
            end;
        {ok, {{_, 404, _}, _, _}} -> not_found;
        {ok, {{_, Status, _}, _, _}} -> {error, {http_status, Status}};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Compare a fetched body against the digest embedded in the CID.
%% All `sha2-256-*' hash-algs share the same underlying digest function.
verify_digest(#{ <<"hash-alg">> := <<"sha2-256-", _/binary>>,
                 <<"digest">>   := Expected }, Body) ->
    Expected =:= crypto:hash(sha256, Body);
verify_digest(_, _) ->
    false.

%%% Tests. See `dev_codec_ipfs_live_test' for broader end-to-end coverage.

-define(HELLO_WORLD_CID,
    <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>).
-define(HELLO_WORLD_BODY, <<"hello world">>).
-define(LIVE_GATEWAYS, [
    <<"https://ipfs.io">>,
    <<"https://dweb.link">>,
    <<"https://nftstorage.link">>,
    <<"https://4everland.io">>
]).

live_store() ->
    #{
        <<"store-module">> => hb_store_ipfs_gateway,
        <<"gateways">>     => ?LIVE_GATEWAYS,
        <<"timeout">>      => 20000
    }.

ensure_inets() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl).

cid_of_key_test() ->
    CID = ?HELLO_WORLD_CID,
    ?assertMatch({ok, CID, #{}}, cid_of_key(CID)),
    ?assertMatch({ok, CID, #{}}, cid_of_key([CID])),
    ?assertEqual(error, cid_of_key(<<"not-a-cid">>)),
    %% Arweave IDs (43-char base64url) are not claimed here.
    ?assertEqual(error,
        cid_of_key(<<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>)),
    %% Multi-part paths are out of scope.
    ?assertEqual(error, cid_of_key([CID, <<"sub">>])).

verify_digest_accepts_correct_body_test() ->
    Body = <<"hello world">>,
    ?assert(verify_digest(#{
        <<"hash-alg">> => <<"sha2-256-raw">>,
        <<"digest">>   => crypto:hash(sha256, Body) }, Body)).

verify_digest_rejects_tampered_body_test() ->
    ?assertNot(verify_digest(#{
        <<"hash-alg">> => <<"sha2-256-raw">>,
        <<"digest">>   => crypto:hash(sha256, <<"hello world">>)
    }, <<"hello earth">>)).

verify_digest_accepts_dag_cbor_hash_alg_test() ->
    Body = <<16#a0>>,
    ?assert(verify_digest(#{
        <<"hash-alg">> => <<"sha2-256-dag-cbor">>,
        <<"digest">>   => crypto:hash(sha256, Body) }, Body)).

scope_is_remote_test() ->
    ?assertEqual(remote, scope(#{})).

read_ignores_non_cid_test() ->
    ?assertEqual(not_found,
        read(#{}, <<"BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>)).

digest_gate_rejects_tampered_body_test() ->
    {ok, Parts} = dev_codec_ipfs_cid:decode(?HELLO_WORLD_CID),
    ?assert(verify_digest(Parts, ?HELLO_WORLD_BODY)),
    ?assertNot(verify_digest(Parts, <<"hello earth">>)).

%%% Live-service tests. The canonical `hello world' CID is pinned on every
%%% public gateway; listing several avoids flaking on one being down.

live_gateway_fetches_known_cid_test_() ->
    {timeout, 60, fun() ->
        ensure_inets(),
        case read(live_store(), ?HELLO_WORLD_CID) of
            {ok, Msg} ->
                ?assertEqual(?HELLO_WORLD_BODY, maps:get(<<"body">>, Msg)),
                Comms = maps:get(<<"commitments">>, Msg),
                ?assert(maps:is_key(?HELLO_WORLD_CID, Comms)),
                C = maps:get(?HELLO_WORLD_CID, Comms),
                ?assertEqual(<<"ipfs@1.0">>,
                    maps:get(<<"commitment-device">>, C)),
                ?assertEqual(<<"sha2-256-raw">>, maps:get(<<"type">>, C));
            not_found ->
                ?debugFmt("Skipping: all gateways missed ~s",
                    [?HELLO_WORLD_CID])
        end
    end}.

%% The commitment attached by the gateway store must verify via the
%% standard `hb_message:verify/2,3' machinery.
live_gateway_attached_commitment_verifies_test_() ->
    {timeout, 60, fun() ->
        ensure_inets(),
        case read(live_store(), ?HELLO_WORLD_CID) of
            {ok, Msg} ->
                ?assert(hb_message:verify(
                    Msg,
                    #{ <<"commitment-ids">> => [?HELLO_WORLD_CID] },
                    #{}));
            not_found ->
                ?debugFmt("Skipping: all gateways missed ~s",
                    [?HELLO_WORLD_CID])
        end
    end}.

%% A CID missing from the local store must fall through to the gateway
%% chain and return via the standard `hb_cache:read/2' path.
live_hb_cache_reads_from_gateway_test_() ->
    {timeout, 60, fun() ->
        ensure_inets(),
        Opts = #{ store => [hb_test_utils:test_store(), live_store()] },
        case hb_cache:read(?HELLO_WORLD_CID, Opts) of
            {ok, Msg} ->
                ?assertEqual(?HELLO_WORLD_BODY,
                    hb_cache:ensure_loaded(
                        maps:get(<<"body">>, Msg), Opts));
            not_found ->
                ?debugFmt("Skipping: all gateways missed CID", [])
        end
    end}.

%% A fake CIDv1 with random digest must not resolve anywhere — the digest
%% gate refuses any body a gateway might return for this path.
live_gateway_rejects_unpinned_cid_test_() ->
    {timeout, 60, fun() ->
        ensure_inets(),
        UnpinnedCID = dev_codec_ipfs_cid:encode(
            <<"raw">>, sha2_256, crypto:strong_rand_bytes(64)),
        Store = (live_store())#{ <<"timeout">> => 10000 },
        ?assertEqual(not_found, read(Store, UnpinnedCID))
    end}.
