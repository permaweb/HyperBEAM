%%% @doc `~ipfs@1.0' — a commitment device whose IDs are IPFS CIDv1s over a
%%% message's `body', and (in phase 2) a codec that serializes HyperBEAM
%%% messages to deterministic dag-cbor and back.
%%%
%%% Phase 1 surface: `commit/3' (type `unsigned' only), `verify/3',
%%% `content_type/1', and `info/1'. The `<<"body">>' blob is treated as
%%% opaque bytes for hashing.
%%%
%%% Phase 2 adds `to/3' and `from/3'. These route through `~structured@1.0'
%%% exactly like `dev_codec_json' — no changes to the structured codec, the
%%% cache, or the kernel. The pipeline is:
%%%
%%%   TABM <-> structured@1.0 (native types) <-> IPLD intermediate <-> dag-cbor bytes
%%%
%%% Atoms other than `null', `true', `false' are not representable in IPLD
%%% and cause `to/3' to throw — that matches the spec. Commitments are
%%% stripped before encoding (IPFS blocks are content; signatures are carried
%%% out-of-band by the HyperBEAM `commitments' machinery).
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
-export([to/3, from/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE_NAME, <<"ipfs@1.0">>).
%% The `type' of an IPFS commitment is the single coordinate that names
%% the CID's construction — both the multihash function and the CID's
%% multicodec in one string, matching IPFS tooling conventions:
%%
%%   sha2-256-raw      — `bafk...' CIDs
%%   sha2-256-dag-cbor — `bafy...' CIDs
%%
%% These slot directly into the wire `alg' as `ipfs@1.0/<type>' through
%% `dev_codec_httpsig_siginfo:commitment_to_alg/2', which is why we keep
%% the hash-alg inside the commitment's `type' field rather than a
%% separate (non-IANA-registered) metadata parameter.
-define(DEFAULT_TYPE, <<"sha2-256-raw">>).
-define(COMMITTED_KEYS, [<<"body">>]).

%%%====================================================================
%%% AO-Core device surface
%%%====================================================================

%% @doc Restrict what AO-Core will resolve against this module. We are a
%% commitment device and a codec, not a general key resolver. `committed/3'
%% is handled by `dev_message' from the `<<"committed">>' field of each
%% commitment, so we do not export it here.
info(_) ->
    #{ exports => [commit, verify, content_type, to, from] }.

%% @doc Report the appropriate IPLD MIME type for a given commitment
%% `type'. Matches the native-type names stored by `commit/3'.
content_type(#{ <<"type">> := <<"sha2-256-dag-cbor">> }) ->
    {ok, <<"application/vnd.ipld.dag-cbor">>};
content_type(#{ <<"type">> := <<"sha2-256-raw">> }) ->
    {ok, <<"application/vnd.ipld.raw">>};
content_type(_) ->
    {ok, <<"application/vnd.ipld.raw">>}.

%%%====================================================================
%%% commit/3
%%%====================================================================

%% @doc Compute a CIDv1 over the `body' of `Msg' and attach it as an
%% unsigned commitment.
%%
%% The caller-facing API mirrors `dev_codec_ans104' and `dev_codec_httpsig':
%% the generic `type: unsigned' is translated into the codec's native type
%% string, which is what ends up in the commitment and on the wire alg.
%% IPFS's native types are:
%%
%%   <<"sha2-256-raw">>      — default. Produces `bafk...' CIDs (multicodec 0x55).
%%   <<"sha2-256-dag-cbor">> — for dag-cbor blocks. Produces `bafy...' CIDs (0x71).
%%
%% An optional `hash-alg' request knob lets the caller pick between them
%% when passing `type: unsigned'; otherwise the raw form is the default.
%% Non-unsigned types delegate to `~httpsig@1.0' — the same composition
%% pattern as `dev_codec_flat' / `dev_codec_json'.

%% Generic caller knob: translate to the native type name.
commit(Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    Native = hb_maps:get(<<"hash-alg">>, Req, ?DEFAULT_TYPE, Opts),
    Req1 = hb_maps:without([<<"hash-alg">>], Req, Opts),
    commit(Msg, Req1#{ <<"type">> => Native }, Opts);

%% Native types: do the real work. Storing the hash-alg in `type' (rather
%% than a separate custom metadata parameter) means the wire alg reads as
%% `ipfs@1.0/sha2-256-raw' — an extension of the HyperBEAM `<device>/<type>'
%% alg convention that requires no custom RFC 9421 metadata parameters.
commit(Msg, #{ <<"type">> := Type }, Opts)
        when Type =:= <<"sha2-256-raw">>;
             Type =:= <<"sha2-256-dag-cbor">> ->
    Body = hb_maps:get(<<"body">>, Msg, <<>>, Opts),
    {ok, Multicodec} = type_to_multicodec(Type),
    CID = dev_codec_ipfs_cid:encode(Multicodec, sha2_256, Body),
    %% The `signature' field carries the raw sha-256 digest of the body
    %% (base64url). That's what an IPFS CID commits to by construction —
    %% there is no separate cryptographic signing step, and no key is
    %% needed to reverify (a receiver just recomputes from `body'). We do
    %% not set `keyid': RFC 9421 §1.4.2.3 permits its absence ("other
    %% means" of key-material retrieval, where here "other means" is "no
    %% retrieval at all"). The httpsig encoder drops absent `keyid' from
    %% the wire.
    %%
    %% The `signature' field also keeps the commitment on the httpsig
    %% wire (it is otherwise filtered out by `commitments_to_siginfo').
    %% Combined with the `id=' parameter that `dev_codec_httpsig_siginfo'
    %% emits when the map key differs from `h(Sig)', this lets remote
    %% nodes decode an IPFS commitment back under its CID key.
    Signature = hb_util:encode(crypto:hash(sha256, Body)),
    Commitment =
        #{
            <<"commitment-device">> => ?DEVICE_NAME,
            <<"type">>              => Type,
            <<"committed">>         => ?COMMITTED_KEYS,
            <<"signature">>         => Signature
        },
    Existing = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    ?event(ipfs,
        {commit,
            {cid, CID},
            {type, Type},
            {body_size, byte_size(Body)}
        }
    ),
    {ok, Msg#{ <<"commitments">> => Existing#{ CID => Commitment } }};

%% Caller asked for an unsigned commit with an unrecognised hash-alg.
commit(_Msg, #{ <<"type">> := Type }, _Opts)
        when Type =:= <<"sha2-256-dag-json">>;
             Type =:= <<"sha2-256-dag-pb">> ->
    {error, {unsupported_type, Type}};

commit(Msg, Req, Opts) ->
    %% Any other commit type — signed, rsa-pss, hmac, etc. — is outside the
    %% IPFS CID envelope. We delegate to `~httpsig@1.0' the same way
    %% `dev_codec_flat', `dev_codec_json', and other codec-only devices do.
    %% Users who want a pure IPFS CID commitment specify `type: unsigned';
    %% everything else gets a proper cryptographic commitment attached.
    dev_codec_httpsig:commit(Msg, Req, Opts).

%% @doc Resolve a native `type' to the underlying CID multicodec name.
type_to_multicodec(<<"sha2-256-raw">>)      -> {ok, <<"raw">>};
type_to_multicodec(<<"sha2-256-dag-cbor">>) -> {ok, <<"dag-cbor">>};
type_to_multicodec(_)                       -> error.

%%%====================================================================
%%% verify/3
%%%====================================================================

%% @doc Verify an `~ipfs@1.0' commitment. `Req' carries the merged fields
%% of the commitment being verified; `Base' is the full message including
%% its `commitments' map.
%%
%% The verification is the commitment function in reverse: recompute the
%% CID from `body' using the commitment's declared native `type'. The
%% commitment is valid iff that CID is a key in `Base''s commitments map
%% — exactly when the body has not been tampered with.
verify(Base, #{ <<"type">> := Type } = _Req, Opts)
        when Type =:= <<"sha2-256-raw">>;
             Type =:= <<"sha2-256-dag-cbor">> ->
    verify_native(Base, Type, Opts);
verify(Base, Req, Opts) ->
    %% Non-native commitments on an IPFS-device message are
    %% httpsig-shaped (see `commit/3'). Delegate.
    dev_codec_httpsig:verify(Base, Req, Opts).

verify_native(Base, Type, Opts) ->
    Body = hb_maps:get(<<"body">>, Base, <<>>, Opts),
    Commitments = hb_maps:get(<<"commitments">>, Base, #{}, Opts),
    {ok, Multicodec} = type_to_multicodec(Type),
    ExpectedCID = dev_codec_ipfs_cid:encode(Multicodec, sha2_256, Body),
    Res = hb_maps:is_key(ExpectedCID, Commitments, Opts),
    ?event(ipfs,
        {verify,
            {type, Type},
            {expected_cid, ExpectedCID},
            {result, Res}
        }
    ),
    {ok, Res}.

%%%====================================================================
%%% to/3 — TABM -> dag-cbor bytes (phase 2)
%%%====================================================================

%% @doc Serialize a HyperBEAM TABM message to deterministic dag-cbor bytes.
%% Routes through `~structured@1.0' to recover native types from the TABM,
%% then walks the rich message into the IPLD intermediate form and hands it
%% to the dag-cbor encoder. Commitments are stripped before encoding — they
%% do not belong in the content-addressed bytes.
to(Bin, _Req, _Opts) when is_binary(Bin) ->
    %% Encode a bare binary as a dag-cbor text string (or byte string if not
    %% UTF-8). Passing it through untouched would leave us unable to
    %% `from/3' the result — the roundtrip contract the codec test vectors
    %% rely on.
    try
        {ok, dev_codec_ipfs_cbor:encode(Bin)}
    catch
        throw:{dag_cbor_encode, {invalid_utf8, _}} ->
            {ok, dev_codec_ipfs_cbor:encode({bytes, Bin})}
    end;
to(Msg, _Req, Opts) when is_map(Msg) ->
    try
        %% Step 1: TABM -> structured form with native types.
        Structured =
            hb_message:convert(
                hb_private:reset(Msg),
                <<"structured@1.0">>,
                tabm,
                Opts
            ),
        %% Step 2: resolve all links. Dag-cbor encodes self-contained content
        %% — partial messages carrying `link'-ref placeholders would not
        %% roundtrip through the IPLD data model. An IPLD-link-aware mapping
        %% through `hb_link' is a future phase.
        Loaded = hb_cache:ensure_all_loaded(Structured, Opts),
        %% Step 3: strip only `priv' — it is per-session state and must
        %% never cross the codec boundary. Commitments *do* cross so that
        %% `from(to(X)) = X' over the full HyperBEAM message; peer codecs
        %% (json, flat, ans104) all behave this way. A pure IPFS consumer
        %% sees `commitments' as just another map field — completely valid
        %% IPLD, and no harm done.
        Clean = hb_maps:without([<<"priv">>], Loaded, Opts),
        %% Step 4: walk into the IPLD intermediate form, then encode.
        Ipld = structured_to_ipld(Clean),
        {ok, dev_codec_ipfs_cbor:encode(Ipld)}
    catch
        throw:{dag_cbor_encode, Reason} ->
            ?event(warning, {ipfs_to_failed, Reason}),
            {error, {dag_cbor_encode, Reason}}
    end.

%% @doc Walk a structured (rich-typed) HyperBEAM value into the IPLD
%% intermediate form understood by `dev_codec_ipfs_cbor:encode/1'.
%%
%% Mappings:
%%   - `null' / `true' / `false'      -> kept as IPLD native.
%%   - integer / float / binary       -> passed through as-is.
%%   - list                           -> list, recursively converted.
%%   - map                            -> map, with binary keys; values
%%                                       recursively converted.
%%   - other atoms                    -> throw; dag-cbor has no atom type.
%%
%% Any value the walker cannot map raises an error the caller surfaces as
%% `{error, {dag_cbor_encode, _}}'.
structured_to_ipld(null)  -> null;
structured_to_ipld(true)  -> true;
structured_to_ipld(false) -> false;
structured_to_ipld(A) when is_atom(A) ->
    throw({dag_cbor_encode, {unsupported_atom, A}});
structured_to_ipld(N) when is_integer(N); is_float(N) -> N;
structured_to_ipld(B) when is_binary(B) -> B;
structured_to_ipld(L) when is_list(L) ->
    [ structured_to_ipld(V) || V <- L ];
structured_to_ipld(M) when is_map(M) ->
    maps:from_list(
        [ {assert_binary_key(K), structured_to_ipld(V)}
            || {K, V} <- maps:to_list(M) ]
    );
structured_to_ipld(Other) ->
    throw({dag_cbor_encode, {unsupported_value, Other}}).

assert_binary_key(K) when is_binary(K) -> K;
assert_binary_key(K) ->
    throw({dag_cbor_encode, {non_binary_map_key, K}}).

%%%====================================================================
%%% from/3 — dag-cbor bytes -> TABM (phase 2)
%%%====================================================================

%% @doc Parse dag-cbor bytes into a TABM message. Decodes to the IPLD
%% intermediate form, normalizes into a rich structured message, then hands
%% to `~structured@1.0' to produce the TABM.
from(Map, _Req, _Opts) when is_map(Map) ->
    %% Passthrough for already-decoded messages, same discipline as json/flat.
    {ok, Map};
from(Bin, Req, Opts) when is_binary(Bin) ->
    case dev_codec_ipfs_cbor:decode(Bin) of
        {ok, Ipld} ->
            Structured = ipld_to_structured(Ipld),
            case Structured of
                S when is_map(S) ->
                    dev_codec_structured:from(S, Req, Opts);
                Other ->
                    {ok, Other}
            end;
        {error, Reason} ->
            ?event(warning, {ipfs_from_failed, Reason}),
            {error, {dag_cbor_decode, Reason}}
    end.

%% @doc Walk the IPLD intermediate form into a HyperBEAM structured form
%% (the rich, native-typed representation that `dev_codec_structured:from/3'
%% consumes).
%%
%% Decisions made for phase 2 minimum:
%%   - `{bytes, B}' and plain binary both flatten to a binary. HyperBEAM
%%     messages rarely need the bytes/text distinction, and re-inferring it
%%     via `ao-types' is out of scope for the first cut.
%%   - `{link, CID}' flattens to the CID string. This is lossy against
%%     IPLD's link semantics, but keeps v1 simple; a link-aware mapping
%%     through `hb_link' is the natural phase 3 step.
ipld_to_structured(null)  -> null;
ipld_to_structured(true)  -> true;
ipld_to_structured(false) -> false;
ipld_to_structured(N) when is_integer(N); is_float(N) -> N;
ipld_to_structured(B) when is_binary(B) -> B;
ipld_to_structured({bytes, B})            -> B;
ipld_to_structured({link, CID})           -> CID;
ipld_to_structured(L) when is_list(L) ->
    [ ipld_to_structured(V) || V <- L ];
ipld_to_structured(M) when is_map(M) ->
    maps:map(fun(_K, V) -> ipld_to_structured(V) end, M).

%%%====================================================================
%%% Tests
%%%====================================================================
%%% Integration-level tests live in `dev_codec_ipfs_test'.

content_type_raw_test() ->
    ?assertEqual(
        {ok, <<"application/vnd.ipld.raw">>},
        content_type(#{ <<"type">> => <<"sha2-256-raw">> })
    ).

content_type_dag_cbor_test() ->
    ?assertEqual(
        {ok, <<"application/vnd.ipld.dag-cbor">>},
        content_type(#{ <<"type">> => <<"sha2-256-dag-cbor">> })
    ).

content_type_default_test() ->
    ?assertEqual(
        {ok, <<"application/vnd.ipld.raw">>},
        content_type(#{})
    ).

%% The caller passes the generic `type: unsigned'; `commit/3' translates
%% into the codec's native type name (`sha2-256-raw') — same translation
%% pattern as `dev_codec_ans104' (unsigned -> unsigned-sha256) and
%% `dev_codec_httpsig' (unsigned -> hmac-sha256).
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
    ?assertEqual(<<"sha2-256-raw">>, maps:get(<<"type">>, Commitment)),
    ?assertEqual([<<"body">>], maps:get(<<"committed">>, Commitment)),
    %% The commitment carries a signature (= raw digest) so it stays on
    %% the httpsig wire via `dev_codec_httpsig_siginfo''s signature
    %% filter. It deliberately does NOT carry a `keyid' — there is no
    %% key to look up for a content-addressed commitment — nor any
    %% `committer', `hash-alg', or `multicodec' fields (the multihash
    %% and multicodec are both encoded in `type').
    ?assertMatch(#{<<"signature">> := _}, Commitment),
    ?assertNot(maps:is_key(<<"keyid">>, Commitment)),
    ?assertNot(maps:is_key(<<"committer">>, Commitment)),
    ?assertNot(maps:is_key(<<"hash-alg">>, Commitment)),
    ?assertNot(maps:is_key(<<"multicodec">>, Commitment)).

commit_unsigned_dag_cbor_test() ->
    Msg = #{ <<"body">> => <<16#a0>> },  %% empty dag-cbor map `{}`
    Req = #{ <<"type">> => <<"unsigned">>,
             <<"hash-alg">> => <<"sha2-256-dag-cbor">> },
    {ok, Committed} = commit(Msg, Req, #{}),
    Commitments = maps:get(<<"commitments">>, Committed),
    [CID] = maps:keys(Commitments),
    ?assertEqual(
        <<"bafyreigbtj4x7ip5legnfznufuopl4sg4knzc2cof6duas4b3q2fy6swua">>,
        CID
    ),
    Commitment = maps:get(CID, Commitments),
    ?assertEqual(<<"sha2-256-dag-cbor">>, maps:get(<<"type">>, Commitment)).

%% Callers that already know the native type can pass it directly.
commit_native_type_test() ->
    Msg = #{ <<"body">> => <<"hello world">> },
    {ok, Committed} = commit(Msg, #{ <<"type">> => <<"sha2-256-raw">> }, #{}),
    [CID] = maps:keys(maps:get(<<"commitments">>, Committed)),
    ?assertEqual(
        <<"bafkreifzjut3te2nhyekklss27nh3k72ysco7y32koao5eei66wof36n5e">>,
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

%% Non-unsigned commit types delegate to `~httpsig@1.0', matching the
%% composition pattern used by `dev_codec_flat', `dev_codec_json', and
%% other codec-only devices. A user who wants a pure IPFS CID passes
%% `type: unsigned'; everything else gets a proper signed commitment.
commit_signed_delegates_to_httpsig_test() ->
    Msg = #{ <<"body">> => <<"x">> },
    Wallet = ar_wallet:new(),
    Opts = #{ priv_wallet => Wallet },
    {ok, Signed} = commit(Msg, #{ <<"type">> => <<"signed">> }, Opts),
    Commitments = maps:get(<<"commitments">>, Signed),
    [{_CID, Commitment}|_] = maps:to_list(Commitments),
    ?assertEqual(<<"httpsig@1.0">>,
        maps:get(<<"commitment-device">>, Commitment)).

commit_rejects_unknown_hash_alg_test() ->
    Msg = #{ <<"body">> => <<"x">> },
    Req = #{ <<"type">> => <<"unsigned">>,
             <<"hash-alg">> => <<"sha2-256-dag-pb">> },
    ?assertMatch({error, {unsupported_type, <<"sha2-256-dag-pb">>}},
        commit(Msg, Req, #{})).

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

verify_fails_when_hash_alg_mismatches_test() ->
    %% A commitment written as raw; caller asserts dag-cbor on verify.
    %% Recomputing under dag-cbor yields a different CID that is not in the
    %% commitments map, so verify must return `{ok, false}'.
    Msg = #{ <<"body">> => <<"hello world">> },
    {ok, Committed} = commit(Msg, #{ <<"type">> => <<"unsigned">> }, #{}),
    Commitments = maps:get(<<"commitments">>, Committed),
    [{_CID, Commitment}] = maps:to_list(Commitments),
    DagCborReq = Commitment#{ <<"type">> => <<"sha2-256-dag-cbor">> },
    ?assertEqual({ok, false}, verify(Committed, DagCborReq, #{})).
