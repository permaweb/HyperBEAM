%%% @doc `~ipfs@1.0' — a commitment device whose IDs are IPFS CIDv1s over a
%%% message's `body'.
%%%
%%% Phase 1 surface: `commit/3' (type `unsigned' only), `verify/3',
%%% `content_type/1', and `info/1'. No `to/3' or `from/3' yet — the
%%% `<<"body">>' blob is treated as opaque bytes for hashing. Phase 2 adds a
%%% full dag-cbor `to'/`from' pair, routed through `~structured@1.0'.
%%%
%%% How this fits AO-Core: a commitment whose ID is a CID gives the cache
%%% everything it already needs to serve the message under that CID. When a
%%% message with an `~ipfs@1.0' commitment is written via `hb_cache:write/2',
%%% the commitment ID is linked to the uncommitted ID of the message
%%% (see `hb_cache:do_write_message/3'). A subsequent `hb_cache:read(CID, _)'
%%% then returns the full message — no new routing, no kernel changes.
%%%
%%% Verification is the same deterministic function as commit: hash the body
%%% with the declared codec + hash algorithm and check that the resulting CID
%%% is present in the message's `commitments' map.
%%%
%%% This device is optional and user-loadable. It is not in
%%% `hb_opts:preloaded_devices/0' by default.
-module(dev_codec_ipfs).
-export([info/1, commit/3, verify/3, content_type/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE_NAME, <<"ipfs@1.0">>).
-define(DEFAULT_CODEC, <<"raw">>).
-define(DEFAULT_HASH_ALG, <<"sha2-256">>).
-define(COMMITTED_KEYS, [<<"body">>]).

%%%====================================================================
%%% AO-Core device surface
%%%====================================================================

%% @doc Restrict what AO-Core will resolve against this module. We are a
%% commitment device, not a general key resolver. `committed/3' is handled
%% by `dev_message' from the `<<"committed">>' field of each commitment, so
%% we do not export it here.
info(_) ->
    #{ exports => [commit, verify, content_type] }.

%% @doc Report the appropriate IPLD MIME type for a given codec.
content_type(#{ <<"codec">> := <<"dag-cbor">> }) ->
    {ok, <<"application/vnd.ipld.dag-cbor">>};
content_type(#{ <<"codec">> := <<"raw">> }) ->
    {ok, <<"application/vnd.ipld.raw">>};
content_type(_) ->
    {ok, <<"application/vnd.ipld.raw">>}.

%%%====================================================================
%%% commit/3
%%%====================================================================

%% @doc Compute a CIDv1 over the `body' of `Msg' and attach it as an
%% unsigned commitment.
%%
%% The `Req' may set:
%%   - `<<"codec">>'    — `<<"raw">>' (default, multicodec 0x55) or
%%                        `<<"dag-cbor">>' (multicodec 0x71).
%%   - `<<"hash-alg">>' — only `<<"sha2-256">>' is supported in phase 1.
%%
%% Only `type = unsigned' is supported; signed CIDs are not a thing in IPFS.
%% Anything else returns an error tuple so AO-Core's dispatcher surfaces a
%% clear failure instead of silently hashing.
commit(Msg, #{ <<"type">> := Type } = Req, Opts)
        when Type =:= <<"unsigned">>;
             Type =:= <<"unsigned-sha256">> ->
    Codec = hb_maps:get(<<"codec">>, Req, ?DEFAULT_CODEC, Opts),
    HashAlg = hb_maps:get(<<"hash-alg">>, Req, ?DEFAULT_HASH_ALG, Opts),
    Body = hb_maps:get(<<"body">>, Msg, <<>>, Opts),
    case {Codec, HashAlg} of
        {C, <<"sha2-256">>} when C =:= <<"raw">>; C =:= <<"dag-cbor">> ->
            CID = dev_codec_ipfs_cid:encode(C, sha2_256, Body),
            Commitment =
                #{
                    <<"commitment-device">> => ?DEVICE_NAME,
                    <<"type">>              => <<"unsigned">>,
                    <<"codec">>             => C,
                    <<"hash-alg">>          => <<"sha2-256">>,
                    <<"committed">>         => ?COMMITTED_KEYS
                },
            Existing = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
            ?event(ipfs,
                {commit,
                    {cid, CID},
                    {codec, C},
                    {body_size, byte_size(Body)}
                }
            ),
            {ok, Msg#{ <<"commitments">> => Existing#{ CID => Commitment } }};
        {_, <<"sha2-256">>} ->
            {error, {unsupported_codec, Codec}};
        {_, _} ->
            {error, {unsupported_hash_alg, HashAlg}}
    end;
commit(_Msg, #{ <<"type">> := Type }, _Opts) ->
    {error, {unsupported_type, Type}}.

%%%====================================================================
%%% verify/3
%%%====================================================================

%% @doc Verify an `~ipfs@1.0' commitment. `Req' carries the merged fields of
%% the commitment being verified (codec, hash-alg, etc.); `Base' is the full
%% message including its `commitments' map.
%%
%% The verification is the commitment function run in reverse: recompute the
%% CID from the body using the declared codec + hash-alg. The commitment is
%% valid iff that CID is a key in `Base''s commitments map — which it must
%% be, exactly when the body has not been tampered with.
verify(Base, Req, Opts) ->
    Codec = hb_maps:get(<<"codec">>, Req, ?DEFAULT_CODEC, Opts),
    HashAlg = hb_maps:get(<<"hash-alg">>, Req, ?DEFAULT_HASH_ALG, Opts),
    Body = hb_maps:get(<<"body">>, Base, <<>>, Opts),
    Commitments = hb_maps:get(<<"commitments">>, Base, #{}, Opts),
    case {Codec, HashAlg} of
        {C, <<"sha2-256">>} when C =:= <<"raw">>; C =:= <<"dag-cbor">> ->
            ExpectedCID = dev_codec_ipfs_cid:encode(C, sha2_256, Body),
            Res = hb_maps:is_key(ExpectedCID, Commitments, Opts),
            ?event(ipfs,
                {verify,
                    {codec, C},
                    {expected_cid, ExpectedCID},
                    {result, Res}
                }
            ),
            {ok, Res};
        _ ->
            ?event(warning,
                {ipfs_verify_unsupported, {codec, Codec}, {hash_alg, HashAlg}}),
            {ok, false}
    end.

%%%====================================================================
%%% Tests
%%%====================================================================
%%% Integration-level tests live in `dev_codec_ipfs_test'.

content_type_raw_test() ->
    ?assertEqual(
        {ok, <<"application/vnd.ipld.raw">>},
        content_type(#{ <<"codec">> => <<"raw">> })
    ).

content_type_dag_cbor_test() ->
    ?assertEqual(
        {ok, <<"application/vnd.ipld.dag-cbor">>},
        content_type(#{ <<"codec">> => <<"dag-cbor">> })
    ).

content_type_default_test() ->
    ?assertEqual(
        {ok, <<"application/vnd.ipld.raw">>},
        content_type(#{})
    ).

commit_unsigned_raw_attaches_cid_test() ->
    Msg = #{ <<"body">> => <<"hello world">> },
    Req = #{ <<"type">> => <<"unsigned">> },
    {ok, Committed} = commit(Msg, Req, #{}),
    Commitments = maps:get(<<"commitments">>, Committed),
    [CID] = maps:keys(Commitments),
    ?assertEqual(
        <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
        CID
    ),
    Commitment = maps:get(CID, Commitments),
    ?assertEqual(?DEVICE_NAME, maps:get(<<"commitment-device">>, Commitment)),
    ?assertEqual(<<"raw">>, maps:get(<<"codec">>, Commitment)),
    ?assertEqual(<<"sha2-256">>, maps:get(<<"hash-alg">>, Commitment)),
    ?assertEqual([<<"body">>], maps:get(<<"committed">>, Commitment)),
    ?assertNot(maps:is_key(<<"committer">>, Commitment)).

commit_unsigned_dag_cbor_test() ->
    Msg = #{ <<"body">> => <<16#a0>> },  %% empty dag-cbor map `{}`
    Req = #{ <<"type">> => <<"unsigned">>, <<"codec">> => <<"dag-cbor">> },
    {ok, Committed} = commit(Msg, Req, #{}),
    Commitments = maps:get(<<"commitments">>, Committed),
    [CID] = maps:keys(Commitments),
    ?assertEqual(
        <<"bafyreigbtj4x7ip5legnfznufuopl4sg4knzc2cof6duas4b3q2fy6swua">>,
        CID
    ).

commit_preserves_existing_commitments_test() ->
    Msg = #{
        <<"body">> => <<"hello world">>,
        <<"commitments">> => #{ <<"other">> => #{ <<"kind">> => <<"placeholder">> } }
    },
    {ok, Committed} = commit(Msg, #{ <<"type">> => <<"unsigned">> }, #{}),
    Commitments = maps:get(<<"commitments">>, Committed),
    ?assert(maps:is_key(<<"other">>, Commitments)),
    ?assertEqual(2, maps:size(Commitments)).

commit_rejects_signed_test() ->
    Msg = #{ <<"body">> => <<"x">> },
    ?assertMatch({error, {unsupported_type, _}},
        commit(Msg, #{ <<"type">> => <<"signed">> }, #{})).

commit_rejects_unknown_codec_test() ->
    Msg = #{ <<"body">> => <<"x">> },
    Req = #{ <<"type">> => <<"unsigned">>, <<"codec">> => <<"dag-pb">> },
    ?assertMatch({error, {unsupported_codec, <<"dag-pb">>}}, commit(Msg, Req, #{})).

verify_ok_for_intact_body_test() ->
    Msg = #{ <<"body">> => <<"hello world">> },
    {ok, Committed} = commit(Msg, #{ <<"type">> => <<"unsigned">> }, #{}),
    Commitments = maps:get(<<"commitments">>, Committed),
    [{_CID, Commitment}] = maps:to_list(Commitments),
    ?assertEqual({ok, true}, verify(Committed, Commitment, #{})).

verify_fails_for_tampered_body_test() ->
    Msg = #{ <<"body">> => <<"hello world">> },
    {ok, Committed} = commit(Msg, #{ <<"type">> => <<"unsigned">> }, #{}),
    Commitments = maps:get(<<"commitments">>, Committed),
    [{_CID, Commitment}] = maps:to_list(Commitments),
    Tampered = Committed#{ <<"body">> => <<"hello earth">> },
    ?assertEqual({ok, false}, verify(Tampered, Commitment, #{})).

verify_fails_when_codec_mismatches_test() ->
    %% A message whose commitment declares dag-cbor but whose body is a raw
    %% blob that does not hash to the stored CID under dag-cbor rules.
    Msg = #{ <<"body">> => <<"hello world">> },
    {ok, Committed} = commit(Msg, #{ <<"type">> => <<"unsigned">> }, #{}),
    Commitments = maps:get(<<"commitments">>, Committed),
    [{_CID, Commitment}] = maps:to_list(Commitments),
    %% Caller asserts dag-cbor; the computed CID will differ and not be present.
    DagCborReq = Commitment#{ <<"codec">> => <<"dag-cbor">> },
    ?assertEqual({ok, false}, verify(Committed, DagCborReq, #{})).
