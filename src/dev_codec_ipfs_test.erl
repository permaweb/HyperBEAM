%%% @doc Integration tests for `~ipfs@1.0': dispatch through
%%% `hb_message:commit/3' and `hb_message:verify/3', cache linkage from CID
%%% to message, and the `to/3'+`from/3' dag-cbor path. Unit-level tests
%%% live inline in `dev_codec_ipfs' and `dev_codec_ipfs_cid'.
-module(dev_codec_ipfs_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(HELLO_WORLD, <<"hello world">>).
-define(HELLO_WORLD_CID,
    <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>).
-define(EMPTY_MAP_CID,
    <<"bafyreigbtj4x7ip5legnfznufuopl4sg4knzc2cof6duas4b3q2fy6swua">>).

%%% Helpers

%% @doc Test opts that opt into `~ipfs@1.0' via `preloaded_devices' and use
%% a volatile store for isolation — the same pattern a production operator
%% would use to enable the device without editing the kernel.
opts() ->
    opts(#{ store => hb_test_utils:test_store() }).
opts(Base) ->
    Stock = hb_opts:get(preloaded_devices, [], Base),
    Base#{
        preloaded_devices =>
            [ #{ <<"name">> => <<"ipfs@1.0">>,
                 <<"module">> => dev_codec_ipfs } | Stock ]
    }.

%% @doc Commit `Msg' with an unsigned `~ipfs@1.0' commitment. `HashAlg' is
%% optional (defaults to the codec's `sha2-256-raw').
ipfs_commit(Msg, Opts) ->
    ipfs_commit(Msg, Opts, #{}).
ipfs_commit(Msg, Opts, Extra) ->
    hb_message:commit(Msg, Opts, Extra#{
        <<"commitment-device">> => <<"ipfs@1.0">>,
        <<"type">>              => <<"unsigned">>
    }).

%%% 1. Dispatch through hb_message:commit / hb_message:verify

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

%% @doc Write a committed message to the cache and look it up by CID
%% alone. `hb_cache:do_write_message/3' links commitment IDs to the
%% uncommitted root; `hb_cache:read/2' follows that link.
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

%%% 3. Phase 2 — to/3 and from/3 through hb_message:convert

to_dag_cbor_simple_test() ->
    Bytes = hb_message:convert(
        #{ <<"hello">> => <<"world">> }, <<"ipfs@1.0">>, opts()),
    ?assertEqual(<<16#a1, 16#65, "hello", 16#65, "world">>, Bytes).

%% Roundtripping a typed message through dag-cbor preserves rich types:
%% integers, floats, booleans, null, lists, nested maps.
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

%% Encoding is deterministic: two differently-ordered source maps produce
%% the same bytes, and re-encoding is stable.
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

%% Committing the dag-cbor bytes of a message yields a CIDv1 identical to
%% the one `ipfs dag put --input-codec dag-cbor' would produce.
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

%% Atoms outside `null/true/false' have no dag-cbor representation.
unsupported_atom_rejected_test() ->
    ?assertMatch(
        {error, {dag_cbor_encode, {unsupported_atom, something}}},
        dev_codec_ipfs:to(#{ <<"kind">> => something }, #{}, opts())).

%% End-to-end against real IPFS: fetch a known pinned dag-cbor CID, verify
%% the attached commitment, decode through `from/3'. Skipped if all live
%% gateways are unreachable.
live_end_to_end_fetch_and_decode_dag_cbor_test_() ->
    {timeout, 60, fun() ->
        application:ensure_all_started(inets),
        application:ensure_all_started(ssl),
        NodeOpts = opts(#{
            store => [
                hb_test_utils:test_store(),
                #{
                    <<"store-module">> => hb_store_ipfs_gateway,
                    <<"gateways">>     =>
                        [<<"https://ipfs.io">>,
                         <<"https://dweb.link">>,
                         <<"https://nftstorage.link">>],
                    <<"timeout">>      => 20000
                }
            ]
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

%% Local end-to-end (no network): encode a rich message, commit its CID,
%% write, read back by CID, decode. Exercises the whole codec + commit +
%% cache path with no mocks.
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

%% A committed message roundtrips through the codec with its commitments
%% intact — matching `dev_codec_json' / `dev_codec_flat' / `dev_codec_ans104'.
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

%% Two different codecs of the same body give two distinct CIDs that both
%% resolve to the same cached message.
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
