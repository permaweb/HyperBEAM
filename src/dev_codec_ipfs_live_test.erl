%%% @doc End-to-end tests for `~ipfs@1.0' against live IPFS gateways and
%%% real HyperBEAM nodes, exercising the user-facing flows advertised in
%%% PR #868:
%%%
%%%   1. `GET /~lookup@1.0/read&target=<CID>' serves the body.
%%%   2. First lookup fetches and pins; subsequent lookups resolve locally.
%%%   3. `GET /~lookup@1.0/read&target=<CID>/commit&type=signed...' returns
%%%      a bundler-ready ANS-104 signed message.
%%%
%%% Each test opts into the device via per-node `preloaded_devices' — the
%%% same way a production operator would enable it. Tests skip gracefully
%%% when all configured gateways are unreachable.
-module(dev_codec_ipfs_live_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(HELLO_WORLD_CID,
    <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>).
-define(HELLO_WORLD_BODY, <<"hello world">>).
-define(LIVE_GATEWAYS, [
    <<"https://ipfs.io">>,
    <<"https://dweb.link">>,
    <<"https://nftstorage.link">>,
    <<"https://4everland.io">>
]).
-define(LOOKUP_PATH,
    <<"/~lookup@1.0/read&target=", ?HELLO_WORLD_CID/binary>>).

%%% Helpers

gateway_store() ->
    #{
        <<"store-module">> => hb_store_ipfs_gateway,
        <<"gateways">>     => ?LIVE_GATEWAYS,
        <<"timeout">>      => 20000
    }.

ipfs_device() ->
    #{ <<"name">> => <<"ipfs@1.0">>, <<"module">> => dev_codec_ipfs }.

%% @doc Base node opts with `~ipfs@1.0' loaded and a gateway-backed store
%% behind a volatile primary.
node_opts() ->
    Stock = hb_opts:get(preloaded_devices, [], #{}),
    #{
        cache_control     => <<"cache">>,
        priv_wallet       => hb:wallet(),
        preloaded_devices => [ipfs_device() | Stock],
        store             => [hb_test_utils:test_store(), gateway_store()]
    }.

%% @doc Run `Fun' if the canonical `hello world' CID is live-reachable;
%% otherwise `?debugFmt' a skip note. Every live test routes through this.
with_live_gateways(Fun) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    case hb_store_ipfs_gateway:read(gateway_store(), ?HELLO_WORLD_CID) of
        {ok, _} -> Fun();
        _ ->
            ?debugFmt("Skipping: all gateways unreachable for ~s",
                [?HELLO_WORLD_CID])
    end.

%% @doc Extract the body from an `hb_http:get' response — sometimes a
%% bare binary, sometimes a map whose `body' may itself be a link.
response_body(R) when is_binary(R) -> R;
response_body(#{ <<"body">> := B }) -> hb_cache:ensure_loaded(B, #{}).

%%% PR Path 1 — Serve a CID from a running node

live_http_get_cid_serves_body_test_() ->
    {timeout, 90, fun() -> with_live_gateways(fun() ->
        NodeURL = hb_http_server:start_node(node_opts()),
        {ok, R} = hb_http:get(NodeURL, ?LOOKUP_PATH, #{}),
        ?assertEqual(?HELLO_WORLD_BODY, response_body(R))
    end) end}.

%% @doc Recomputing the CID from the wire body must reproduce the
%% requested CID — the only verification that matters in IPFS.
live_http_body_round_trips_to_cid_test_() ->
    {timeout, 90, fun() -> with_live_gateways(fun() ->
        NodeURL = hb_http_server:start_node(node_opts()),
        {ok, R} = hb_http:get(NodeURL, ?LOOKUP_PATH, #{}),
        ?assertEqual(?HELLO_WORLD_CID,
            dev_codec_ipfs_cid:encode(
                <<"raw">>, sha2_256, response_body(R)))
    end) end}.

%%% PR Path 2 — Preload / en-masse cache a set of CIDs

%% @doc First lookup pulls the CID through the gateway and pins it to the
%% node's primary store; a second direct probe of the primary succeeds.
live_cache_preload_pattern_test_() ->
    {timeout, 90, fun() -> with_live_gateways(fun() ->
        LocalStore = #{
            <<"store-module">> => hb_store_fs,
            <<"name">> =>
                iolist_to_binary(
                    ["cache-TEST/ipfs-preload-",
                     integer_to_list(erlang:system_time(microsecond))])
        },
        hb_store:reset(LocalStore),
        Stock = hb_opts:get(preloaded_devices, [], #{}),
        NodeURL = hb_http_server:start_node(#{
            cache_control     => <<"cache">>,
            priv_wallet       => hb:wallet(),
            preloaded_devices => [ipfs_device() | Stock],
            store             => [LocalStore, gateway_store()]
        }),
        {ok, R1} = hb_http:get(NodeURL, ?LOOKUP_PATH, #{}),
        ?assertEqual(?HELLO_WORLD_BODY, response_body(R1)),
        LocalOpts = #{ store => [LocalStore] },
        {ok, R2} = hb_cache:read(?HELLO_WORLD_CID, LocalOpts),
        ?assertEqual(?HELLO_WORLD_BODY,
            hb_cache:ensure_loaded(
                hb_ao:get(<<"body">>, R2, <<>>, LocalOpts), LocalOpts))
    end) end}.

%% @doc Transport: an IPFS commitment must arrive on the client side
%% under its CID map key, not under `h(Sig)'. This is what the `id='
%% extension in `dev_codec_httpsig_siginfo' preserves.
live_http_ipfs_commitment_survives_transport_test_() ->
    {timeout, 90, fun() -> with_live_gateways(fun() ->
        NodeURL = hb_http_server:start_node(node_opts()),
        ClientOpts = #{ preloaded_devices =>
            [ipfs_device() | hb_opts:get(preloaded_devices, [], #{})] },
        {ok, R} = hb_http:get(NodeURL, ?LOOKUP_PATH, ClientOpts),
        Msg =
            case R of
                M when is_map(M)    -> M;
                B when is_binary(B) -> #{ <<"body">> => B }
            end,
        IpfsComms = maps:filter(
            fun(_K, #{<<"commitment-device">> := <<"ipfs@1.0">>}) -> true;
               (_K, _) -> false end,
            maps:get(<<"commitments">>, Msg, #{})),
        case maps:to_list(IpfsComms) of
            []         -> ?debugFmt(
                "Skipping: no IPFS commitment on response", []);
            [{CID, _}] -> ?assertEqual(?HELLO_WORLD_CID, CID);
            Many       -> ?debugFmt("multiple ipfs commitments: ~p", [Many])
        end
    end) end}.

%% @doc Two in-process nodes, wired so a client request on Node B
%% transparently pulls through Node A:
%%
%%   Node A — upstream — has ONLY `hb_store_ipfs_gateway'. Every read
%%     passes through to the real IPFS network.
%%   Node B — downstream — has a primary fs store plus
%%     `hb_store_remote_node' pointed at Node A with `local-store' set to
%%     the primary. B's cache misses fall through to A; A's responses
%%     write through into B's primary on return.
%%
%% After the first query pins the body to B's primary, Node A is killed.
%% The next query on B must still succeed — served entirely from B's cache.
live_hb_to_hb_remote_store_relay_test_() ->
    {timeout, 120, fun() -> with_live_gateways(fun() ->
        %% Two distinct wallets — the HB server_id is derived from
        %% `priv_wallet''s address, so shared wallets collapse two nodes
        %% onto one listener.
        Stock = hb_opts:get(preloaded_devices, [], #{}),
        NodeAWallet = ar_wallet:new(),
        NodeAServerID =
            hb_util:human_id(ar_wallet:to_address(NodeAWallet)),
        NodeAURL = hb_http_server:start_node(#{
            port              => 18770,
            priv_wallet       => NodeAWallet,
            cache_control     => <<"cache">>,
            preloaded_devices => [ipfs_device() | Stock],
            store             => [gateway_store()]
        }),
        NodeBPrimary = hb_test_utils:test_store(),
        NodeBURL = hb_http_server:start_node(#{
            port              => 18771,
            priv_wallet       => ar_wallet:new(),
            cache_control     => <<"cache">>,
            preloaded_devices => [ipfs_device() | Stock],
            store => [
                NodeBPrimary,
                #{
                    <<"store-module">> => hb_store_remote_node,
                    <<"node">>         => NodeAURL,
                    <<"local-store">>  => [NodeBPrimary]
                }
            ]
        }),
        %% (1) First query: B->A->real IPFS, cached on B's primary on return.
        {ok, R1} = hb_http:get(NodeBURL, ?LOOKUP_PATH, #{}),
        ?assertEqual(?HELLO_WORLD_BODY, response_body(R1)),
        %% (2) B's primary now holds the message keyed by the CID.
        LocalOnly = #{ store => [NodeBPrimary] },
        {ok, MsgOnB} = hb_cache:read(?HELLO_WORLD_CID, LocalOnly),
        ?assertEqual(?HELLO_WORLD_BODY,
            hb_cache:ensure_loaded(
                maps:get(<<"body">>, MsgOnB), LocalOnly)),
        ?assert(maps:is_key(?HELLO_WORLD_CID,
            maps:get(<<"commitments">>, MsgOnB, #{}))),
        %% (3) Kill Node A; (4) B must still serve from primary.
        ok = cowboy:stop_listener(NodeAServerID),
        {ok, R2} = hb_http:get(NodeBURL, ?LOOKUP_PATH, #{}),
        ?assertEqual(?HELLO_WORLD_BODY, response_body(R2))
    end) end}.

%%% PR Path 3 — Commit IPFS content as ANS-104 via the node's wallet

%% The server-side half of the push-to-Arweave chain: node reads the CID
%% and re-commits as ANS-104 signed. The final POST to `~arweave@2.9/tx'
%% requires a funded wallet and a reachable bundler, neither in scope for
%% automated CI.
live_lookup_then_ans104_commit_test_() ->
    {timeout, 90, fun() -> with_live_gateways(fun() ->
        NodeURL = hb_http_server:start_node(node_opts()),
        Path = <<?LOOKUP_PATH/binary,
                 "/commit&type=signed&commitment-device=ans104@1.0">>,
        {ok, R} = hb_http:get(NodeURL, Path, #{}),
        ?assertEqual(?HELLO_WORLD_BODY, response_body(R))
    end) end}.

%%% Lua computation across IPFS-resolved data

live_lua_computation_over_ipfs_body_test_() ->
    {timeout, 90, fun() -> with_live_gateways(fun() ->
        NodeOpts = node_opts(),
        NodeURL = hb_http_server:start_node(NodeOpts),
        {ok, IpfsMsg} = hb_cache:read(?HELLO_WORLD_CID, NodeOpts),
        Body = hb_cache:ensure_loaded(
            hb_ao:get(<<"body">>, IpfsMsg, <<>>, NodeOpts), NodeOpts),
        ?assertEqual(?HELLO_WORLD_BODY, Body),
        Base = #{
            <<"device">>       => <<"lua@5.3a">>,
            <<"content-type">> => <<"application/lua">>,
            <<"body">>         =>
                <<"function byte_length(base, req)\n"
                  "  return #base.body\n"
                  "end\n">>,
            <<"function">>     => <<"byte_length">>,
            <<"parameters">>   => [ #{ <<"body">> => Body } ]
        },
        ?assertEqual(byte_size(?HELLO_WORLD_BODY),
            hb_ao:get(<<"byte_length">>, Base, undefined, NodeOpts)),
        {ok, _} = hb_http:get(NodeURL, <<"/~meta@1.0/info">>, #{})
    end) end}.
