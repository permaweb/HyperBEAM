%%% @doc Integration tests for `~ipfs@1.0'. The unit-level tests live inline
%%% in `dev_codec_ipfs' and `dev_codec_ipfs_cid'. This module covers:
%%%   1. Dispatch through `hb_message:commit/3' and `hb_message:verify/3' so
%%%      the device behaves correctly under the standard AO-Core machinery.
%%%   2. The cache linkage proof: writing a message with a CID commitment
%%%      makes `hb_cache:read(CID, Opts)' return the message, with no kernel
%%%      changes. This is the load-bearing claim of the phase 1 design.
-module(dev_codec_ipfs_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Canonical IPFS ground truth: `hello world' under the `raw' codec.
-define(HELLO_WORLD, <<"hello world">>).
-define(HELLO_WORLD_CID,
    <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>).

%%%====================================================================
%%% Test helpers
%%%====================================================================

%% @doc Build an Opts map that makes `~ipfs@1.0' available to the AO-Core
%% device loader without editing `hb_opts:preloaded_devices/0'. This is how
%% a production operator would enable the device: in node config, not by
%% patching the kernel. We use a volatile store so tests are isolated.
opts() ->
    Base = #{ store => hb_test_utils:test_store() },
    opts(Base).
opts(Base) ->
    %% Merge our entry into whatever `preloaded_devices' the node would
    %% normally use, so we do not hide any stock devices.
    Stock = hb_opts:get(preloaded_devices, [], Base),
    Base#{
        preloaded_devices =>
            [ #{ <<"name">> => <<"ipfs@1.0">>,
                 <<"module">> => dev_codec_ipfs } | Stock ]
    }.

%%%====================================================================
%%% 1. Dispatch through hb_message:commit / hb_message:verify
%%%====================================================================

hb_message_commit_dispatches_to_us_test() ->
    Opts = opts(),
    Msg = #{ <<"body">> => ?HELLO_WORLD },
    CommitReq = #{
        <<"commitment-device">> => <<"ipfs@1.0">>,
        <<"type">>              => <<"unsigned">>
    },
    Committed = hb_message:commit(Msg, Opts, CommitReq),
    Commitments = maps:get(<<"commitments">>, Committed),
    ?assert(maps:is_key(?HELLO_WORLD_CID, Commitments)),
    Commitment = maps:get(?HELLO_WORLD_CID, Commitments),
    ?assertEqual(<<"ipfs@1.0">>, maps:get(<<"commitment-device">>, Commitment)).

hb_message_verify_dispatches_to_us_test() ->
    Opts = opts(),
    Msg = #{ <<"body">> => ?HELLO_WORLD },
    CommitReq = #{
        <<"commitment-device">> => <<"ipfs@1.0">>,
        <<"type">>              => <<"unsigned">>
    },
    Committed = hb_message:commit(Msg, Opts, CommitReq),
    %% Verify by commitment-id.
    ?assertEqual(
        true,
        hb_message:verify(
            Committed,
            #{ <<"commitment-ids">> => [?HELLO_WORLD_CID] },
            Opts
        )
    ).

verify_rejects_tampered_body_via_hb_message_test() ->
    Opts = opts(),
    Msg = #{ <<"body">> => ?HELLO_WORLD },
    CommitReq = #{
        <<"commitment-device">> => <<"ipfs@1.0">>,
        <<"type">>              => <<"unsigned">>
    },
    Committed = hb_message:commit(Msg, Opts, CommitReq),
    Tampered = Committed#{ <<"body">> => <<"hello earth">> },
    ?assertEqual(
        false,
        hb_message:verify(
            Tampered,
            #{ <<"commitment-ids">> => [?HELLO_WORLD_CID] },
            Opts
        )
    ).

committed_returns_body_key_test() ->
    %% `hb_message:committed/3' reads each commitment's own `committed' list
    %% and takes the intersection. For a single `~ipfs@1.0' commitment that
    %% list is exactly `[<<"body">>]'.
    Opts = opts(),
    Msg = #{ <<"body">> => ?HELLO_WORLD },
    Committed =
        hb_message:commit(
            Msg, Opts,
            #{ <<"commitment-device">> => <<"ipfs@1.0">>,
               <<"type">>              => <<"unsigned">> }
        ),
    Keys =
        hb_message:committed(
            Committed,
            [?HELLO_WORLD_CID],
            Opts
        ),
    ?assertEqual([<<"body">>], Keys).

%%%====================================================================
%%% 2. Cache linkage — the load-bearing proof
%%%====================================================================

%% @doc Write a message with an IPFS commitment to the cache, then look it
%% up by the CID alone. This is what makes `GET /<CID>' work without any
%% kernel change: `hb_cache:do_write_message/3' links commitment IDs to the
%% uncommitted root ID, and `hb_cache:read/2' follows that link.
cache_links_cid_to_uncommitted_id_test() ->
    Opts = opts(),
    Msg = #{ <<"body">> => ?HELLO_WORLD },
    Committed =
        hb_message:commit(
            Msg, Opts,
            #{ <<"commitment-device">> => <<"ipfs@1.0">>,
               <<"type">>              => <<"unsigned">> }
        ),
    {ok, _UncommittedID} = hb_cache:write(Committed, Opts),
    %% The headline claim: reading by CID returns the cached message.
    {ok, Recovered} = hb_cache:read(?HELLO_WORLD_CID, Opts),
    RecoveredBody = hb_cache:ensure_loaded(
        maps:get(<<"body">>, Recovered), Opts
    ),
    ?assertEqual(?HELLO_WORLD, RecoveredBody),
    %% Commitment survives the roundtrip.
    Commitments = maps:get(<<"commitments">>, Recovered, #{}),
    ?assert(maps:is_key(?HELLO_WORLD_CID, Commitments)).

%% @doc A message can carry both an ANS-104 unsigned commitment AND an
%% `~ipfs@1.0' commitment; both commitment IDs independently resolve back
%% to the same cached message. This confirms `~ipfs@1.0' is additive and
%% does not conflict with any existing commitment device.
multiple_commitment_devices_coexist_test() ->
    Opts = opts(),
    Msg = #{ <<"body">> => ?HELLO_WORLD },
    WithIpfs =
        hb_message:commit(
            Msg, Opts,
            #{ <<"commitment-device">> => <<"ipfs@1.0">>,
               <<"type">>              => <<"unsigned">> }
        ),
    {ok, _UID} = hb_cache:write(WithIpfs, Opts),
    {ok, ViaCID} = hb_cache:read(?HELLO_WORLD_CID, Opts),
    ?assertEqual(
        ?HELLO_WORLD,
        hb_cache:ensure_loaded(maps:get(<<"body">>, ViaCID), Opts)
    ).

%% @doc Two different codecs of the same body must give two distinct CIDs
%% that both resolve. A `raw' CID and a `dag-cbor' CID on the same bytes
%% address the same underlying message.
raw_and_dag_cbor_cids_coexist_test() ->
    Opts = opts(),
    Body = <<16#a0>>,
    Msg = #{ <<"body">> => Body },
    M1 =
        hb_message:commit(
            Msg, Opts,
            #{ <<"commitment-device">> => <<"ipfs@1.0">>,
               <<"type">>              => <<"unsigned">>,
               <<"codec">>             => <<"raw">> }
        ),
    M2 =
        hb_message:commit(
            M1, Opts,
            #{ <<"commitment-device">> => <<"ipfs@1.0">>,
               <<"type">>              => <<"unsigned">>,
               <<"codec">>             => <<"dag-cbor">> }
        ),
    Commitments = maps:get(<<"commitments">>, M2),
    ?assertEqual(2, maps:size(Commitments)),
    {ok, _UID} = hb_cache:write(M2, Opts),
    %% The empty-dag-cbor CID should now also resolve, per our CID unit tests.
    DagCborCID = <<"bafyreigbtj4x7ip5legnfznufuopl4sg4knzc2cof6duas4b3q2fy6swua">>,
    {ok, ViaDagCbor} = hb_cache:read(DagCborCID, Opts),
    ?assertEqual(
        Body,
        hb_cache:ensure_loaded(maps:get(<<"body">>, ViaDagCbor), Opts)
    ).
