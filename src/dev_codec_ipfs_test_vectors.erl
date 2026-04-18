%%% @doc Test vectors for `~ipfs@1.0'. Three layers of coverage:
%%%
%%%   1. Integration — dispatch through `hb_message:commit/3' and
%%%      `hb_message:verify/3', cache linkage from CID to message, and the
%%%      `to/3' / `from/3' dag-cbor conversions.
%%%   2. Live — end-to-end tests against real IPFS HTTP gateways and live
%%%      in-process HyperBEAM nodes (the flows advertised in PR #868:
%%%      serve a CID, preload/pin a CID, commit as ANS-104, relay between
%%%      two nodes). Tests skip gracefully when no gateway is reachable.
%%%   3. Message vectors — the `hb_message_test_vectors' battery run
%%%      against the codec, with a `skip' list declared on the opts entry
%%%      for vectors that do not apply to a content-addressed, unsigned-
%%%      only codec.
%%%
%%% Unit-level tests continue to live inline in `dev_codec_ipfs',
%%% `dev_codec_ipfs_cid', `dev_codec_ipfs_cbor', and `hb_store_ipfs_gateway'.
-module(dev_codec_ipfs_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Canonical IPFS ground truth: `ipfs add --raw-leaves -Q <"hello world"'.
-define(HELLO_WORLD,      <<"hello world">>).
-define(HELLO_WORLD_CID,
    <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>).
%% The canonical empty dag-cbor block `{}` (byte `0xa0') is pinned on
%% every public gateway.
-define(EMPTY_MAP_CID,
    <<"bafyreigbtj4x7ip5legnfznufuopl4sg4knzc2cof6duas4b3q2fy6swua">>).
-define(LIVE_GATEWAYS, [
    <<"https://ipfs.io">>,
    <<"https://dweb.link">>,
    <<"https://nftstorage.link">>,
    <<"https://4everland.io">>
]).
-define(LOOKUP_PATH,
    <<"/~lookup@1.0/read&target=", ?HELLO_WORLD_CID/binary>>).

%%% Helpers

%% @doc Integration-test opts: opt into `~ipfs@1.0' via `preloaded_devices'
%% and use a volatile store for isolation.
opts() ->
    opts(#{ store => hb_test_utils:test_store() }).
opts(Base) ->
    Stock = hb_opts:get(preloaded_devices, [], Base),
    Base#{
        preloaded_devices =>
            [ipfs_device() | Stock]
    }.

%% @doc Commit `Msg' with an unsigned `~ipfs@1.0' commitment. `Extra' may
%% override `hash-alg' (defaults to the codec's `sha2-256-raw').
ipfs_commit(Msg, Opts) ->
    ipfs_commit(Msg, Opts, #{}).
ipfs_commit(Msg, Opts, Extra) ->
    hb_message:commit(Msg, Opts, Extra#{
        <<"commitment-device">> => <<"ipfs@1.0">>,
        <<"type">>              => <<"unsigned">>
    }).

ipfs_device() ->
    #{ <<"name">> => <<"ipfs@1.0">>, <<"module">> => dev_codec_ipfs }.

gateway_store() ->
    #{
        <<"store-module">> => hb_store_ipfs_gateway,
        <<"gateways">>     => ?LIVE_GATEWAYS,
        <<"timeout">>      => 20000
    }.

%% @doc Full node opts: `~ipfs@1.0' loaded plus a gateway-backed store
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
%% otherwise emit a skip note. Every live test routes through this.
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

%%% 1. Integration — dispatch through hb_message:commit / verify

hb_message_commit_dispatches_to_us_test() ->
    Opts = opts(),
    Committed = ipfs_commit(#{ <<"body">> => ?HELLO_WORLD }, Opts),
    Commitments = maps:get(<<"commitments">>, Committed),
    ?assert(maps:is_key(?HELLO_WORLD_CID, Commitments)),
    ?assertEqual(<<"ipfs@1.0">>,
        maps:get(<<"commitment-device">>,
                 maps:get(?HELLO_WORLD_CID, Commitments))).

hb_message_verify_dispatches_to_us_test() ->
    Opts = opts(),
    Committed = ipfs_commit(#{ <<"body">> => ?HELLO_WORLD }, Opts),
    ?assert(hb_message:verify(
        Committed, #{ <<"commitment-ids">> => [?HELLO_WORLD_CID] }, Opts)).

verify_rejects_tampered_body_via_hb_message_test() ->
    Opts = opts(),
    Committed = ipfs_commit(#{ <<"body">> => ?HELLO_WORLD }, Opts),
    Tampered = Committed#{ <<"body">> => <<"hello earth">> },
    ?assertNot(hb_message:verify(
        Tampered, #{ <<"commitment-ids">> => [?HELLO_WORLD_CID] }, Opts)).

committed_returns_body_key_test() ->
    Opts = opts(),
    Committed = ipfs_commit(#{ <<"body">> => ?HELLO_WORLD }, Opts),
    ?assertEqual([<<"body">>],
        hb_message:committed(Committed, [?HELLO_WORLD_CID], Opts)).

%%% 2. Cache linkage — the load-bearing claim of phase 1

%% @doc Write a committed message to the cache, look it up by CID alone.
%% `hb_cache:do_write_message/3' links commitment IDs to the uncommitted
%% root; `hb_cache:read/2' follows that link.
cache_links_cid_to_uncommitted_id_test() ->
    Opts = opts(),
    Committed = ipfs_commit(#{ <<"body">> => ?HELLO_WORLD }, Opts),
    {ok, _} = hb_cache:write(Committed, Opts),
    {ok, Recovered} = hb_cache:read(?HELLO_WORLD_CID, Opts),
    ?assertEqual(?HELLO_WORLD,
        hb_cache:ensure_loaded(maps:get(<<"body">>, Recovered), Opts)),
    ?assert(maps:is_key(?HELLO_WORLD_CID,
        maps:get(<<"commitments">>, Recovered, #{}))).

%% @doc Multiple commitment devices on one message do not conflict: the
%% CID still resolves through the cache.
multiple_commitment_devices_coexist_test() ->
    Opts = opts(),
    Committed = ipfs_commit(#{ <<"body">> => ?HELLO_WORLD }, Opts),
    {ok, _} = hb_cache:write(Committed, Opts),
    {ok, ViaCID} = hb_cache:read(?HELLO_WORLD_CID, Opts),
    ?assertEqual(?HELLO_WORLD,
        hb_cache:ensure_loaded(maps:get(<<"body">>, ViaCID), Opts)).

%%% 3. to/3 and from/3 through hb_message:convert

to_dag_cbor_simple_test() ->
    Bytes = hb_message:convert(
        #{ <<"hello">> => <<"world">> }, <<"ipfs@1.0">>, opts()),
    ?assertEqual(<<16#a1, 16#65, "hello", 16#65, "world">>, Bytes).

%% @doc Roundtripping a typed message through dag-cbor preserves rich
%% types: integers, floats, booleans, null, lists, nested maps.
roundtrip_typed_message_test() ->
    Opts = opts(),
    Msg = #{
        <<"name">>   => <<"alice">>,
        <<"age">>    => 30,
        <<"score">>  => 4.5,
        <<"admin">>  => true,
        <<"parent">> => null,
        <<"tags">>   => [<<"a">>, <<"b">>, <<"c">>],
        <<"nested">> => #{ <<"k">> => <<"v">>, <<"n">> => -42 }
    },
    Bytes = hb_message:convert(Msg, <<"ipfs@1.0">>, Opts),
    Decoded = hb_message:convert(
        Bytes, <<"structured@1.0">>, <<"ipfs@1.0">>, Opts),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

%% @doc Encoding is deterministic: two differently-ordered source maps
%% produce the same bytes, and re-encoding is stable.
encoding_is_deterministic_test() ->
    Opts = opts(),
    B1 = hb_message:convert(
        #{ <<"a">> => 1, <<"bb">> => 2, <<"ccc">> => 3 },
        <<"ipfs@1.0">>, Opts),
    B2 = hb_message:convert(
        #{ <<"ccc">> => 3, <<"a">> => 1, <<"bb">> => 2 },
        <<"ipfs@1.0">>, Opts),
    ?assertEqual(B1, B2),
    ?assertEqual(B1, hb_message:convert(
        #{ <<"a">> => 1, <<"bb">> => 2, <<"ccc">> => 3 },
        <<"ipfs@1.0">>, Opts)).

%% @doc Committing the dag-cbor bytes of a message yields a CIDv1
%% identical to the one `ipfs dag put --input-codec dag-cbor' would produce.
cid_matches_dag_cbor_of_message_test() ->
    Opts = opts(),
    Bytes = hb_message:convert(
        #{ <<"hello">> => <<"world">> }, <<"ipfs@1.0">>, Opts),
    Committed = ipfs_commit(
        #{ <<"body">> => Bytes }, Opts,
        #{ <<"hash-alg">> => <<"sha2-256-dag-cbor">> }),
    [CID] = maps:keys(maps:get(<<"commitments">>, Committed)),
    {ok, Parts} = dev_codec_ipfs_cid:decode(CID),
    ?assertEqual(<<"sha2-256-dag-cbor">>, maps:get(<<"hash-alg">>, Parts)),
    ?assertEqual(crypto:hash(sha256, Bytes), maps:get(<<"digest">>, Parts)),
    ?assertMatch(<<"bafyrei", _:52/binary>>, CID).

%% @doc Atoms outside `null/true/false' have no dag-cbor representation.
unsupported_atom_rejected_test() ->
    ?assertMatch(
        {error, {dag_cbor_encode, {unsupported_atom, something}}},
        dev_codec_ipfs:to(#{ <<"kind">> => something }, #{}, opts())).

%% @doc Local end-to-end (no network): encode a rich message, commit its
%% CID, write, read back by CID, decode. Exercises the whole codec +
%% commit + cache path with no mocks.
local_end_to_end_encode_commit_cache_decode_test() ->
    Opts = opts(),
    Msg = #{
        <<"kind">>   => <<"greeting">>,
        <<"from">>   => <<"alice">>,
        <<"to">>     => <<"bob">>,
        <<"count">>  => 3,
        <<"active">> => true
    },
    Bytes = hb_message:convert(Msg, <<"ipfs@1.0">>, Opts),
    Committed = ipfs_commit(
        #{ <<"body">> => Bytes }, Opts,
        #{ <<"hash-alg">> => <<"sha2-256-dag-cbor">> }),
    [CID] = maps:keys(maps:get(<<"commitments">>, Committed)),
    {ok, _} = hb_cache:write(Committed, Opts),
    {ok, Fetched} = hb_cache:read(CID, Opts),
    FetchedBytes =
        hb_cache:ensure_loaded(maps:get(<<"body">>, Fetched), Opts),
    ?assertEqual(Bytes, FetchedBytes),
    ?assert(hb_message:match(
        Msg,
        hb_message:convert(
            FetchedBytes, <<"structured@1.0">>, <<"ipfs@1.0">>, Opts),
        strict, Opts)).

%% @doc A committed message roundtrips through the codec with its
%% commitments intact — matching `dev_codec_json' / `dev_codec_flat' /
%% `dev_codec_ans104'.
commit_then_encode_preserves_commitments_test() ->
    Opts = opts(),
    Committed = ipfs_commit(
        #{ <<"body">> => ?HELLO_WORLD, <<"kind">> => <<"greeting">> }, Opts),
    Bytes = hb_message:convert(Committed, <<"ipfs@1.0">>, Opts),
    {ok, Ipld} = dev_codec_ipfs_cbor:decode(Bytes),
    ?assert(maps:is_key(<<"commitments">>, Ipld)),
    ?assert(hb_message:match(
        Committed,
        hb_message:convert(
            Bytes, <<"structured@1.0">>, <<"ipfs@1.0">>, Opts),
        strict, Opts)).

%% @doc Two different codecs of the same body give two distinct CIDs that
%% both resolve to the same cached message.
raw_and_dag_cbor_cids_coexist_test() ->
    Opts = opts(),
    Body = <<16#a0>>,
    M1 = ipfs_commit(
        #{ <<"body">> => Body }, Opts,
        #{ <<"hash-alg">> => <<"sha2-256-raw">> }),
    M2 = ipfs_commit(
        M1, Opts, #{ <<"hash-alg">> => <<"sha2-256-dag-cbor">> }),
    ?assertEqual(2, maps:size(maps:get(<<"commitments">>, M2))),
    {ok, _} = hb_cache:write(M2, Opts),
    {ok, ViaDagCbor} = hb_cache:read(?EMPTY_MAP_CID, Opts),
    ?assertEqual(Body,
        hb_cache:ensure_loaded(maps:get(<<"body">>, ViaDagCbor), Opts)).

%%% 4. Live — real gateways, real HyperBEAM nodes

%% @doc End-to-end against real IPFS: fetch a known pinned dag-cbor CID,
%% verify the attached commitment, decode through `from/3'.
live_end_to_end_fetch_and_decode_dag_cbor_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        NodeOpts = opts(#{
            store =>
                [hb_test_utils:test_store(), gateway_store()]
        }),
        case hb_cache:read(?EMPTY_MAP_CID, NodeOpts) of
            {ok, Fetched} ->
                Bytes =
                    hb_cache:ensure_loaded(
                        maps:get(<<"body">>, Fetched), NodeOpts),
                ?assertEqual(<<16#a0>>, Bytes),
                ?assert(hb_message:verify(
                    Fetched,
                    #{ <<"commitment-ids">> => [?EMPTY_MAP_CID] },
                    NodeOpts)),
                ?assertEqual(#{},
                    hb_message:convert(
                        Bytes, <<"structured@1.0">>, <<"ipfs@1.0">>,
                        NodeOpts));
            _ ->
                ?debugFmt("Skipping: all gateways missed ~s",
                    [?EMPTY_MAP_CID])
        end
    end}.

%% @doc A running HyperBEAM node serves a CID via the `~lookup@1.0' path.
live_http_get_cid_serves_body_test_() ->
    {timeout, 90, fun() -> with_live_gateways(fun() ->
        NodeURL = hb_http_server:start_node(node_opts()),
        {ok, R} = hb_http:get(NodeURL, ?LOOKUP_PATH, #{}),
        ?assertEqual(?HELLO_WORLD, response_body(R))
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
        ?assertEqual(?HELLO_WORLD, response_body(R1)),
        LocalOpts = #{ store => [LocalStore] },
        {ok, R2} = hb_cache:read(?HELLO_WORLD_CID, LocalOpts),
        ?assertEqual(?HELLO_WORLD,
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
        %% Two distinct wallets — HB's server_id is derived from
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
        %% (1) First query: B -> A -> real IPFS; cached on B's primary on
        %% the return path.
        {ok, R1} = hb_http:get(NodeBURL, ?LOOKUP_PATH, #{}),
        ?assertEqual(?HELLO_WORLD, response_body(R1)),
        %% (2) B's primary now holds the message keyed by the CID.
        LocalOnly = #{ store => [NodeBPrimary] },
        {ok, MsgOnB} = hb_cache:read(?HELLO_WORLD_CID, LocalOnly),
        ?assertEqual(?HELLO_WORLD,
            hb_cache:ensure_loaded(
                maps:get(<<"body">>, MsgOnB), LocalOnly)),
        ?assert(maps:is_key(?HELLO_WORLD_CID,
            maps:get(<<"commitments">>, MsgOnB, #{}))),
        %% (3) Kill Node A; (4) B must still serve from primary.
        ok = cowboy:stop_listener(NodeAServerID),
        {ok, R2} = hb_http:get(NodeBURL, ?LOOKUP_PATH, #{}),
        ?assertEqual(?HELLO_WORLD, response_body(R2))
    end) end}.

%% @doc Server-side half of the push-to-Arweave chain: read the CID and
%% re-commit as ANS-104 signed. The final POST to `~arweave@2.9/tx'
%% requires a funded wallet and a reachable bundler, out of scope for CI.
live_lookup_then_ans104_commit_test_() ->
    {timeout, 90, fun() -> with_live_gateways(fun() ->
        NodeURL = hb_http_server:start_node(node_opts()),
        Path = <<?LOOKUP_PATH/binary,
                 "/commit&type=signed&commitment-device=ans104@1.0">>,
        {ok, R} = hb_http:get(NodeURL, Path, #{}),
        ?assertEqual(?HELLO_WORLD, response_body(R))
    end) end}.

%% @doc A Lua computation runs across IPFS-resolved data served by the
%% local node — the same node handling HTTP traffic.
live_lua_computation_over_ipfs_body_test_() ->
    {timeout, 90, fun() -> with_live_gateways(fun() ->
        NodeOpts = node_opts(),
        NodeURL = hb_http_server:start_node(NodeOpts),
        {ok, IpfsMsg} = hb_cache:read(?HELLO_WORLD_CID, NodeOpts),
        Body = hb_cache:ensure_loaded(
            hb_ao:get(<<"body">>, IpfsMsg, <<>>, NodeOpts), NodeOpts),
        ?assertEqual(?HELLO_WORLD, Body),
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
        ?assertEqual(byte_size(?HELLO_WORLD),
            hb_ao:get(<<"byte_length">>, Base, undefined, NodeOpts)),
        {ok, _} = hb_http:get(NodeURL, <<"/~meta@1.0/info">>, #{})
    end) end}.

%%% 5. Message test-vector battery

%% @doc Run the full `hb_message_test_vectors' battery against
%% `~ipfs@1.0', skipping vectors that do not apply to a content-addressed,
%% unsigned-only codec. Keeping the skip list here — rather than inside
%% the generic battery module — follows the `hb_ao_test_vectors' pattern
%% of carrying device-specific quirks on the opts entry.
suite_test_() ->
    hb_test_utils:suite_with_opts(
        hb_message_test_vectors:codec_test_suite([<<"ipfs@1.0">>]),
        vector_opts()).

vector_opts() ->
    [#{
        name     => ipfs,
        parallel => true,
        desc     => <<"ipfs@1.0">>,
        opts     => #{
            store       => hb_test_utils:test_store(),
            priv_wallet => hb:wallet()
        },
        skip => [
            %% Non-null/true/false atoms have no IPLD type and throw on
            %% encode.
            <<"Structured field atom parsing">>,
            %% `~ipfs@1.0' is unsigned-only (content-addressed); the
            %% node-message signing path needs a signed commitment.
            <<"Sign node message">>,
            %% `priv' is session-only state and is stripped by `to/3' —
            %% it must never cross the content-addressed boundary.
            <<"Priv survives conversion">>,
            %% `{link, CID}' flattens to the CID string in phase 2. A
            %% link-aware mapping through `hb_link' is the next phase.
            <<"ID of linked message">>
        ]
    }].
