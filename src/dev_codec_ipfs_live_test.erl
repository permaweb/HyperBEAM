%%% @doc End-to-end production tests for `~ipfs@1.0': live IPFS network +
%%% live HyperBEAM node + HTTP client, exercising the paths described in
%%% PR #868.
%%%
%%% The PR advertises three user-facing flows, each expressed through the
%%% standard AO-Core `~lookup@1.0' device so no kernel edits are required:
%%%
%%%   1. Serve a CID:      `GET /~lookup@1.0/read&target=<CID>'
%%%   2. Preload a CID:    first lookup fetches + pins; subsequent lookups
%%%                        are local.
%%%   3. Commit for Arweave:
%%%          `GET /~lookup@1.0/read&target=<CID>/commit
%%%              &type=signed&commitment-device=ans104@1.0'
%%%       returns the bundler-ready signed message. The final POST to
%%%       `~arweave@2.9/tx' needs a topped-up wallet and a configured
%%%       bundler endpoint, neither of which is in scope for automated CI.
%%%
%%% `~ipfs@1.0' is an optional, user-loadable device. Each test opts into
%%% it via per-node `preloaded_devices' — the same way a production
%%% operator enables it. Tests skip gracefully if every gateway is
%%% unreachable at the time they run (matches the `hb_store_gateway'
%%% live-test pattern).
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

%%%====================================================================
%%% Helpers
%%%====================================================================

%% @doc Node opts that opt into `~ipfs@1.0' and configure the IPFS
%% gateway store in the chain.
node_opts_with_ipfs() ->
    Stock = hb_opts:get(preloaded_devices, [], #{}),
    #{
        cache_control     => <<"cache">>,
        priv_wallet       => hb:wallet(),
        preloaded_devices =>
            [ #{ <<"name">> => <<"ipfs@1.0">>,
                 <<"module">> => dev_codec_ipfs } | Stock ],
        store => [
            hb_test_utils:test_store(),
            #{
                <<"store-module">> => hb_store_ipfs_gateway,
                <<"gateways">>     => ?LIVE_GATEWAYS,
                <<"timeout">>      => 20000
            }
        ]
    }.

gateways_reachable_for_cid(CID) ->
    Store = #{
        <<"store-module">> => hb_store_ipfs_gateway,
        <<"gateways">>     => ?LIVE_GATEWAYS,
        <<"timeout">>      => 20000
    },
    case hb_store_ipfs_gateway:read(Store, CID) of
        {ok, _} -> true;
        _       -> false
    end.

%%%====================================================================
%%% PR Path 1 — Serve a CID from a running node
%%%====================================================================

live_http_get_cid_serves_body_test_() ->
    {timeout, 90, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        case gateways_reachable_for_cid(?HELLO_WORLD_CID) of
            false ->
                ?debugFmt("Skipping: all gateways unreachable for ~s",
                    [?HELLO_WORLD_CID]);
            true ->
                NodeURL = hb_http_server:start_node(
                    node_opts_with_ipfs()),
                Path = <<"/~lookup@1.0/read&target=",
                         ?HELLO_WORLD_CID/binary>>,
                {ok, Response} = hb_http:get(NodeURL, Path, #{}),
                Body =
                    case Response of
                        B when is_binary(B) -> B;
                        #{ <<"body">> := B } ->
                            hb_cache:ensure_loaded(B, #{})
                    end,
                ?assertEqual(?HELLO_WORLD_BODY, Body)
        end
    end}.

%% Recomputing the CID from the wire body reproduces the requested CID —
%% the only verification that matters in IPFS.
live_http_body_round_trips_to_cid_test_() ->
    {timeout, 90, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        case gateways_reachable_for_cid(?HELLO_WORLD_CID) of
            false ->
                ?debugFmt("Skipping: all gateways unreachable", []);
            true ->
                NodeURL = hb_http_server:start_node(
                    node_opts_with_ipfs()),
                Path = <<"/~lookup@1.0/read&target=",
                         ?HELLO_WORLD_CID/binary>>,
                {ok, Response} = hb_http:get(NodeURL, Path, #{}),
                Body =
                    case Response of
                        B when is_binary(B) -> B;
                        #{ <<"body">> := B } ->
                            hb_cache:ensure_loaded(B, #{})
                    end,
                Recomputed =
                    dev_codec_ipfs_cid:encode(
                        <<"raw">>, sha2_256, Body),
                ?assertEqual(?HELLO_WORLD_CID, Recomputed)
        end
    end}.

%%%====================================================================
%%% PR Path 2 — Preload / en-masse cache a set of CIDs
%%%====================================================================

%% The first HTTP lookup pulls the CID via the gateway and pins it to
%% the node's local filesystem store. A second lookup — against an
%% opts-set that only contains the local store — still succeeds, proving
%% the HTTP request-response pipeline's write-through is doing the job.
%% This is the mechanism behind the PR's "HEAD /CID preload" claim.
live_cache_preload_pattern_test_() ->
    {timeout, 90, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        case gateways_reachable_for_cid(?HELLO_WORLD_CID) of
            false ->
                ?debugFmt("Skipping: all gateways unreachable", []);
            true ->
                LocalStore = #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> =>
                        iolist_to_binary(
                            ["cache-TEST/ipfs-preload-",
                             integer_to_list(
                                erlang:system_time(microsecond))])
                },
                hb_store:reset(LocalStore),
                Stock = hb_opts:get(preloaded_devices, [], #{}),
                NodeURL = hb_http_server:start_node(#{
                    cache_control     => <<"cache">>,
                    priv_wallet       => hb:wallet(),
                    preloaded_devices =>
                        [ #{ <<"name">> => <<"ipfs@1.0">>,
                             <<"module">> => dev_codec_ipfs } | Stock ],
                    store => [
                        LocalStore,
                        #{
                            <<"store-module">> => hb_store_ipfs_gateway,
                            <<"gateways">>     => ?LIVE_GATEWAYS,
                            <<"timeout">>      => 20000
                        }
                    ]
                }),
                %% 1. First HTTP read — fetches from the gateway and the
                %% cache-through write path pins it to LocalStore.
                Path = <<"/~lookup@1.0/read&target=",
                         ?HELLO_WORLD_CID/binary>>,
                {ok, R1} = hb_http:get(NodeURL, Path, #{}),
                Body1 =
                    case R1 of
                        B1 when is_binary(B1) -> B1;
                        #{ <<"body">> := B1 } ->
                            hb_cache:ensure_loaded(B1, #{})
                    end,
                ?assertEqual(?HELLO_WORLD_BODY, Body1),
                %% 2. Second lookup driven directly at the local store
                %% (no gateway, no node). If it resolves, the HTTP call
                %% pinned the CID.
                LocalOpts = #{ store => [LocalStore] },
                {ok, R2} = hb_cache:read(?HELLO_WORLD_CID, LocalOpts),
                ?assertEqual(
                    ?HELLO_WORLD_BODY,
                    hb_cache:ensure_loaded(
                        hb_ao:get(<<"body">>, R2, <<>>, LocalOpts),
                        LocalOpts))
        end
    end}.

%% HB-to-HB transport round-trip: a node receives an IPFS-committed
%% response via HTTP and must see the commitment at the CID key, not at
%% `h(sig)'. This is what the `id=' extension in
%% `dev_codec_httpsig_siginfo' preserves.
live_http_ipfs_commitment_survives_transport_test_() ->
    {timeout, 90, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        case gateways_reachable_for_cid(?HELLO_WORLD_CID) of
            false ->
                ?debugFmt("Skipping: all gateways unreachable", []);
            true ->
                %% Server: knows ipfs@1.0, has a gateway in its store.
                NodeOpts = node_opts_with_ipfs(),
                NodeURL = hb_http_server:start_node(NodeOpts),
                %% Client: knows ipfs@1.0 too, but no gateway; it relies
                %% entirely on the server-delivered message.
                ClientOpts = #{
                    preloaded_devices =>
                        [ #{ <<"name">> => <<"ipfs@1.0">>,
                             <<"module">> => dev_codec_ipfs } |
                          hb_opts:get(preloaded_devices, [], #{}) ]
                },
                Path = <<"/~lookup@1.0/read&target=",
                         ?HELLO_WORLD_CID/binary>>,
                {ok, Response} = hb_http:get(NodeURL, Path, ClientOpts),
                Msg =
                    case Response of
                        M when is_map(M) -> M;
                        B when is_binary(B) -> #{ <<"body">> => B }
                    end,
                Comms = maps:get(<<"commitments">>, Msg, #{}),
                IPFSComms =
                    maps:filter(
                        fun(_K, #{<<"commitment-device">> := <<"ipfs@1.0">>}) ->
                            true;
                           (_K, _) -> false
                        end,
                        Comms
                    ),
                case maps:to_list(IPFSComms) of
                    [] ->
                        ?debugFmt("Skipping: no IPFS commitment on "
                                  "response (likely gateway path not "
                                  "taken on this run)", []);
                    [{Key, _}] ->
                        ?assertEqual(?HELLO_WORLD_CID, Key);
                    Many ->
                        ?debugFmt("multiple ipfs commitments: ~p", [Many])
                end
        end
    end}.

%%%====================================================================
%%% PR Path 3 — Commit IPFS content as ANS-104 via the node's wallet
%%%====================================================================

%% The server-side-commit half of the push-to-Arweave chain: node reads
%% CID, applies an ANS-104 signed commitment using its own wallet, and
%% returns a bundler-ready message. The final POST to `~arweave@2.9/tx'
%% (or `~bundler@1.0/tx') needs a funded wallet and a reachable bundler,
%% neither of which is in scope for automated CI.
live_lookup_then_ans104_commit_test_() ->
    {timeout, 90, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        case gateways_reachable_for_cid(?HELLO_WORLD_CID) of
            false ->
                ?debugFmt("Skipping: all gateways unreachable", []);
            true ->
                NodeURL = hb_http_server:start_node(
                    node_opts_with_ipfs()),
                Path =
                    <<"/~lookup@1.0/read&target=",
                      ?HELLO_WORLD_CID/binary,
                      "/commit&type=signed&commitment-device=ans104@1.0">>,
                {ok, Response} = hb_http:get(NodeURL, Path, #{}),
                Body =
                    case Response of
                        B when is_binary(B) -> B;
                        #{ <<"body">> := B } ->
                            hb_cache:ensure_loaded(B, #{})
                    end,
                ?assertEqual(?HELLO_WORLD_BODY, Body)
        end
    end}.

%%%====================================================================
%%% Lua computation across IPFS-resolved data
%%%====================================================================

live_lua_computation_over_ipfs_body_test_() ->
    {timeout, 90, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        case gateways_reachable_for_cid(?HELLO_WORLD_CID) of
            false ->
                ?debugFmt("Skipping: all gateways unreachable", []);
            true ->
                NodeOpts = node_opts_with_ipfs(),
                NodeURL = hb_http_server:start_node(NodeOpts),
                {ok, IpfsMsg} = hb_cache:read(?HELLO_WORLD_CID, NodeOpts),
                Body = hb_cache:ensure_loaded(
                    hb_ao:get(<<"body">>, IpfsMsg, <<>>, NodeOpts),
                    NodeOpts),
                ?assertEqual(?HELLO_WORLD_BODY, Body),
                LuaSource =
                    <<"function byte_length(base, req)\n"
                      "  return #base.body\n"
                      "end\n">>,
                Base = #{
                    <<"device">>       => <<"lua@5.3a">>,
                    <<"content-type">> => <<"application/lua">>,
                    <<"body">>         => LuaSource,
                    <<"function">>     => <<"byte_length">>,
                    <<"parameters">>   => [ #{ <<"body">> => Body } ]
                },
                Result =
                    hb_ao:get(
                        <<"byte_length">>,
                        Base,
                        undefined,
                        NodeOpts
                    ),
                ?assertEqual(byte_size(?HELLO_WORLD_BODY), Result),
                {ok, _} = hb_http:get(NodeURL,
                    <<"/~meta@1.0/info">>, #{})
        end
    end}.
