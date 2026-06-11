%%% @doc Two-node remote-store proof for native LBRY commitments. Node B
%%% sources Odysee/LBRY objects through its stores; node A reads them
%%% through a remote-node store with `verify-remote-read' enabled. Node A
%%% verifies the returned native commitments locally, caches only valid
%%% results, and serves later reads from its own store -- without trusting
%%% node B's HTTP signature.
-module(hb_lbry_remote_store_test).
-include_lib("eunit/include/eunit.hrl").

two_node_blob_read_verifies_and_caches_test() ->
    application:ensure_all_started(inets),
    Bytes = <<"two-node encrypted blob">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    {ok, BlobServer, Handle} = hb_mock_server:start([
        {"/blob", blob, {200, Bytes}}
    ]),
    try
        NodeB =
            hb_http_server:start_node(#{
                <<"store">> => [
                    #{
                        <<"store-module">> => hb_store_lbry_blob,
                        <<"node">> => BlobServer,
                        <<"http-client">> => httpc
                    }
                ]
            }),
        LocalStore = hb_test_utils:test_store(),
        hb_store:reset(LocalStore),
        RemoteStore = #{
            <<"store-module">> => hb_store_remote_node,
            <<"node">> => NodeB,
            <<"verify-remote-read">> => true,
            <<"local-store">> => [LocalStore]
        },
        {ok, Msg} =
            hb_cache:read(Hash, #{ <<"store">> => [LocalStore, RemoteStore] }),
        Loaded = hb_cache:ensure_all_loaded(Msg),
        ?assertEqual(Bytes, maps:get(<<"data">>, Loaded)),
        ?assertEqual(Hash, maps:get(<<"blob-hash">>, Loaded)),
        % The boundary keeps only the native LBRY commitments: node B's own
        % HTTP response signatures are neither required nor trusted.
        ?assertEqual(
            true,
            hb_message:verify(Loaded, #{ <<"commitment-ids">> => <<"all">> }, #{})
        ),
        % The verified result was cached: a second read must succeed from the
        % local store alone. Reads by the native hash alias return the data
        % view; the cache loads commitments target-scoped, so the committed
        % view lives at the canonical commitment-ID alias.
        LocalOpts = #{ <<"store">> => [LocalStore] },
        {ok, Cached} = hb_cache:read(Hash, LocalOpts),
        CachedLoaded = hb_cache:ensure_all_loaded(Cached, LocalOpts),
        ?assertEqual(Bytes, maps:get(<<"data">>, CachedLoaded)),
        % An explicit re-verification of the cached evidence reads the
        % message through its commitment-ID alias and verifies natively.
        CommitmentID =
            hb_lbry_commitment:commitment_id(binary:decode_hex(Hash)),
        {ok, Committed} = hb_cache:read(CommitmentID, LocalOpts),
        CommittedLoaded = hb_cache:ensure_all_loaded(Committed, LocalOpts),
        ?assertEqual(Bytes, maps:get(<<"data">>, CommittedLoaded)),
        ?assertEqual(
            true,
            hb_message:verify(
                CommittedLoaded,
                #{ <<"commitment-ids">> => <<"all">> },
                LocalOpts
            )
        ),
        ?assertMatch(
            {ok, _},
            hb_lbry_commitment:verify_remote_read(
                Hash,
                CommittedLoaded,
                LocalOpts
            )
        )
    after
        hb_mock_server:stop(Handle)
    end.

two_node_rejects_substituted_blob_test() ->
    % Node B serves a perfectly valid blob message, but for a different hash
    % than the one node A requested. The native-id binding must reject it
    % and nothing may be cached.
    RealBytes = <<"the real blob">>,
    RealHash = hb_lbry_stream_descriptor:blob_hash(RealBytes),
    RequestedHash = hb_lbry_stream_descriptor:blob_hash(<<"a different blob">>),
    StoreB = hb_test_utils:test_store(),
    hb_store:reset(StoreB),
    Msg = hb_lbry_commitment:blob_message(RealHash, RealBytes),
    {ok, MsgID} = hb_cache:write(Msg, #{ <<"store">> => [StoreB] }),
    ok = hb_store:link(StoreB, #{ RequestedHash => MsgID }, #{}),
    NodeB = hb_http_server:start_node(#{ <<"store">> => [StoreB] }),
    LocalStore = hb_test_utils:test_store(),
    hb_store:reset(LocalStore),
    RemoteStore = #{
        <<"store-module">> => hb_store_remote_node,
        <<"node">> => NodeB,
        <<"verify-remote-read">> => true,
        <<"local-store">> => [LocalStore]
    },
    ?assertMatch(
        {error, _},
        hb_cache:read(RequestedHash, #{ <<"store">> => [LocalStore, RemoteStore] })
    ),
    ?assertMatch(
        {error, _},
        hb_cache:read(RequestedHash, #{ <<"store">> => [LocalStore] })
    ).

two_node_rejects_tampered_blob_test() ->
    % Node B serves a message whose commitment does not match its bytes.
    % Verification must fail and nothing may be cached.
    Bytes = <<"the real blob">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Tampered =
        (hb_lbry_commitment:blob_message(Hash, Bytes))#{
            <<"data">> => <<"tampered bytes!!!">>
        },
    StoreB = hb_test_utils:test_store(),
    hb_store:reset(StoreB),
    {ok, MsgID} = hb_cache:write(Tampered, #{ <<"store">> => [StoreB] }),
    ok = hb_store:link(StoreB, #{ Hash => MsgID }, #{}),
    NodeB = hb_http_server:start_node(#{ <<"store">> => [StoreB] }),
    LocalStore = hb_test_utils:test_store(),
    hb_store:reset(LocalStore),
    RemoteStore = #{
        <<"store-module">> => hb_store_remote_node,
        <<"node">> => NodeB,
        <<"verify-remote-read">> => true,
        <<"local-store">> => [LocalStore]
    },
    ?assertMatch(
        {error, _},
        hb_cache:read(Hash, #{ <<"store">> => [LocalStore, RemoteStore] })
    ),
    ?assertMatch(
        {error, _},
        hb_cache:read(Hash, #{ <<"store">> => [LocalStore] })
    ).

two_node_claim_output_read_verifies_test() ->
    application:ensure_all_started(inets),
    TxID = <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
    Outpoint = <<TxID/binary, ":0">>,
    TxResponse =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ <<"hex">> => hb_lbry_tx:task0_tx_hex() },
            <<"id">> => 1
        }),
    {ok, ProxyServer, Handle} = hb_mock_server:start([
        {"/api/v1/proxy", proxy, {200, TxResponse}}
    ]),
    try
        % Node B needs a writable local store: serving composite messages
        % offloads submessages into the node's cache during reply encoding.
        StoreB = hb_test_utils:test_store(),
        hb_store:reset(StoreB),
        NodeB =
            hb_http_server:start_node(#{
                <<"store">> => [
                    StoreB,
                    #{
                        <<"store-module">> => hb_store_lbry_claim_output,
                        <<"lbry-proxy-node">> => ProxyServer,
                        <<"http-client">> => httpc
                    }
                ]
            }),
        LocalStore = hb_test_utils:test_store(),
        hb_store:reset(LocalStore),
        RemoteStore = #{
            <<"store-module">> => hb_store_remote_node,
            <<"node">> => NodeB,
            <<"verify-remote-read">> => true,
            <<"local-store">> => [LocalStore]
        },
        {ok, Msg} =
            hb_cache:read(Outpoint, #{ <<"store">> => [LocalStore, RemoteStore] }),
        Loaded = hb_cache:ensure_all_loaded(Msg),
        ?assertEqual(
            <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
            maps:get(<<"claim-id">>, Loaded)
        ),
        ?assertEqual(
            true,
            hb_message:verify(Loaded, #{ <<"commitment-ids">> => <<"all">> }, #{})
        ),
        % A reader expecting channel evidence narrows the acceptable devices
        % and must reject claim evidence served for the same outpoint.
        NarrowedStore =
            RemoteStore#{
                <<"verify-remote-devices">> => [<<"lbry-channel@1.0">>]
            },
        hb_store:reset(LocalStore),
        ?assertMatch(
            {error, _},
            hb_cache:read(Outpoint, #{ <<"store">> => [LocalStore, NarrowedStore] })
        )
    after
        hb_mock_server:stop(Handle)
    end.
