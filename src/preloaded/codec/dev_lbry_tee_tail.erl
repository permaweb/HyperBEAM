%%% @doc The `lbry-tee-tail@1.0' codec: the TEE-anchored header commitment
%%% classes that compose an `snp@1.0' attestation with the trustless MMR layer.
%%%
%%% Where `lbry-header@1.0' proves trustless facts about the buried chain (a
%%% leaf's MMR membership, a chunk's internal structure, a roll-forward's
%%% consistency), `lbry-tee-tail@1.0' covers what the trustless layer cannot
%%% cheaply prove on its own: that the LIVE TAIL of headers extending the pinned
%%% snapshot root was validated (PoW + linkage) by a measured enclave, and that
%%% the network's genesis -> snapshot provenance was established in-enclave.
%%%
%%% `verify/3' dispatches on the COMMITMENT's `type' (read from the request, not
%%% the base) so this commitment can sit beside an `lbry-header@1.0' commitment
%%% on a single evidence message whose base `type' is, e.g., `mmr-consistency':
%%%
%%%   `tee-tail'    - (a) the attested tail is a valid MMR consistency extension
%%%                   of the pinned snapshot root `R_S' to a new root `R_tip'
%%%                   (trustless structure, via `hb_lbry_mmr:verify_consistency');
%%%                   (b) an attested enclave node signed an evidence message
%%%                   binding `R_tip' (the attestation vouches the tail's PoW
%%%                   validity); (c) the attested `tee-to-root' equals `R_tip'.
%%%   `mmr-genesis' - an attested enclave node signed evidence binding
%%%                   `(genesis_hash, R_S)', so a fresh verifier bootstraps the
%%%                   provenance of the 32-byte snapshot root from one attestation
%%%                   instead of re-validating the whole genesis->snapshot chain.
%%%
%%% Capability switch (see `aidocs/012'): the attestation backend is chosen by
%%% capability, never by a hand-set demo flag. `verify_backend/1' uses the real
%%% `snp@1.0' device by default (verification is hardware-free; it needs only the
%%% loaded NIF and, for the signature leaf, the AMD KDS); it degrades to a
%%% loudly-labelled mock ONLY when `snp-mode' is explicitly `mock' (offline/CI).
%%% `generate_backend/1' (producer side) uses the real hardware report when
%%% `hb_snp_nif:check_snp_support' reports SNP support, else the canned mock
%%% report. The same node image therefore runs real on an SNP host and degrades
%%% cleanly elsewhere, with no code change.
-module(dev_lbry_tee_tail).
-implements(<<"lbry-tee-tail@1.0">>).
-export([info/0, verify/3]).
-export([tee_tail_evidence/2, mmr_genesis_evidence/2]).
-export([layered_evidence/2, verify_layered/2]).
-export([generate_backend/1, verify_backend/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Codec device: `verify' is the only resolved key.
info() ->
    #{ excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>] }.

%% @doc Verify a TEE-anchored header commitment, dispatching on the commitment's
%% `type' (request first, base second) so the base `type' is free for a
%% co-resident `lbry-header@1.0' commitment.
verify(Base, Req, Opts) ->
    Type =
        case hb_maps:get(<<"type">>, Req, undefined, Opts) of
            undefined -> hb_maps:get(<<"type">>, Base, undefined, Opts);
            T -> T
        end,
    case Type of
        <<"tee-tail">>    -> verify_tee_tail(Base, Opts);
        <<"mmr-genesis">> -> verify_mmr_genesis(Base, Opts);
        _                 -> {ok, false}
    end.

%%% --------------------------------------------------------------------------
%%% tee-tail
%%% --------------------------------------------------------------------------

%% @doc A `tee-tail' commitment holds when all three checks pass:
%%   1. linkage  - the tail `delta-leaves' append to the pinned snapshot root
%%                 `R_S' (node opts) and re-bag to the committed `to-root';
%%   2. attest   - the embedded `tee-evidence' is a valid `snp@1.0' attestation
%%                 (an attested enclave node signed it);
%%   3. binding  - the attested `tee-to-root' equals the committed `to-root', so
%%                 the attestation vouches exactly this tail.
verify_tee_tail(Base, Opts) ->
    OldPeaks    = hb_maps:get(<<"old-peaks">>,    Base, undefined, Opts),
    DeltaLeaves = hb_maps:get(<<"delta-leaves">>, Base, undefined, Opts),
    ToRoot      = hb_maps:get(<<"to-root">>,      Base, undefined, Opts),
    FromRoot    = hb_maps:get(<<"lbry-header-root">>, Opts, undefined, Opts),
    Evidence    = tee_evidence(Base, Opts),
    case lists:member(undefined, [OldPeaks, DeltaLeaves, ToRoot, FromRoot, Evidence]) of
        true -> {ok, false};
        false ->
            LinkageOK =
                hb_lbry_mmr:verify_consistency(
                    normalize_hash(FromRoot),
                    [normalize_peak(P) || P <- OldPeaks],
                    [normalize_hash(L) || L <- DeltaLeaves],
                    normalize_hash(ToRoot)
                ),
            AttestOK = verify_attestation(Evidence, Opts),
            BindingOK =
                normalize_hash(field(Evidence, <<"tee-to-root">>, Opts))
                    =:= normalize_hash(ToRoot),
            ?event(lbry_commitment,
                {tee_tail_verify,
                    {linkage, LinkageOK}, {attest, AttestOK}, {binding, BindingOK}}),
            {ok, LinkageOK andalso AttestOK andalso BindingOK}
    end.

%%% --------------------------------------------------------------------------
%%% mmr-genesis
%%% --------------------------------------------------------------------------

%% @doc An `mmr-genesis' commitment holds when an attested enclave node signed
%% evidence binding the network genesis hash and the snapshot root, and both
%% match the verifier's pinned values. It lets a fresh verifier accept the
%% PROVENANCE of the 32-byte snapshot root from one attestation rather than
%% re-validating genesis->snapshot. It is optional: a message without it still
%% verifies against the pinned root through the trustless layer.
verify_mmr_genesis(Base, Opts) ->
    Evidence    = tee_evidence(Base, Opts),
    PinnedRoot  = hb_maps:get(<<"lbry-header-root">>,    Opts, undefined, Opts),
    GenesisHash = hb_maps:get(<<"lbry-genesis-hash">>,   Opts, undefined, Opts),
    case lists:member(undefined, [Evidence, PinnedRoot, GenesisHash]) of
        true -> {ok, false};
        false ->
            AttestOK = verify_attestation(Evidence, Opts),
            RootOK =
                normalize_hash(field(Evidence, <<"tee-genesis-root">>, Opts))
                    =:= normalize_hash(PinnedRoot),
            GenesisOK =
                normalize_hash(field(Evidence, <<"tee-genesis-hash">>, Opts))
                    =:= normalize_hash(GenesisHash),
            ?event(lbry_commitment,
                {mmr_genesis_verify,
                    {attest, AttestOK}, {root, RootOK}, {genesis, GenesisOK}}),
            {ok, AttestOK andalso RootOK andalso GenesisOK}
    end.

%%% --------------------------------------------------------------------------
%%% Attestation (capability-switched)
%%% --------------------------------------------------------------------------

%% @doc Verify the embedded `snp@1.0' attestation evidence. The real backend
%% delegates to the packaged `snp@1.0' device exactly as `dev_green_zone' does
%% (`hb_ao:resolve({as, snp@1.0, Evidence}, verify, Opts)'), so the six-check
%% attestation pipeline (nonce binding, signer==address, debug-off,
%% trusted-measurement, measurement digest, AMD report signature) runs for real.
%% The mock backend is offline/CI only and is announced loudly; it skips just the
%% hardware/network crypto leaf while the linkage and binding checks above stay
%% real.
verify_attestation(Evidence, Opts) ->
    case verify_backend(Opts) of
        mock ->
            ?event(lbry_commitment,
                {tee_attestation,
                    <<"⚠ SNP NIF MOCKED - L2 attestation NOT "
                      "cryptographically validated; CI/dev only">>}),
            true;
        real ->
            case hb_ao:resolve({as, <<"snp@1.0">>, Evidence}, <<"verify">>, Opts) of
                {ok, <<"true">>} -> true;
                {ok, true}       -> true;
                _                -> false
            end
    end.

%% @doc Producer-side backend: a fresh hardware report needs real SNP support,
%% so probe for it; without it, fall back to the canned mock report. An explicit
%% `snp-mode' overrides the probe.
generate_backend(Opts) ->
    case hb_maps:get(<<"snp-mode">>, Opts, <<"auto">>, Opts) of
        <<"real">> -> real;
        <<"mock">> -> mock;
        _ ->
            case catch hb_snp_nif:check_snp_support() of
                {ok, true} -> real;
                _          -> mock
            end
    end.

%% @doc Verifier-side backend: verification is hardware-free, so default to the
%% real `snp@1.0' device; only an explicit `snp-mode => mock' engages the
%% offline shim.
verify_backend(Opts) ->
    case hb_maps:get(<<"snp-mode">>, Opts, <<"auto">>, Opts) of
        <<"mock">> -> mock;
        _          -> real
    end.

%%% --------------------------------------------------------------------------
%%% Producers
%%% --------------------------------------------------------------------------

%% @doc Build a `tee-tail' evidence message. `TailProof' is the trustless
%% consistency proof of the tail `{OldPeaks, DeltaLeaves, ToRoot}' rooted at the
%% pinned snapshot root; the attester additionally vouches the tail's PoW
%% validity via the attestation. Returns a message carrying the proof, the
%% node-signed attestation evidence (binding `tee-to-root => ToRoot'), and a
%% `lbry-tee-tail@1.0/tee-tail' commitment. `Opts' must carry `priv-wallet' and
%% `snp-trusted'.
tee_tail_evidence({OldPeaks, DeltaLeaves, ToRoot}, Opts) ->
    Evidence =
        attestation_evidence(
            #{ <<"tee-to-root">> => to_hex(ToRoot) },
            Opts
        ),
    Base = #{
        <<"device">>       => <<"lbry-tee-tail@1.0">>,
        <<"type">>         => <<"tee-tail">>,
        <<"old-peaks">>    => OldPeaks,
        <<"delta-leaves">> => DeltaLeaves,
        <<"to-root">>      => to_hex(ToRoot),
        <<"tee-evidence">> => Evidence
    },
    hb_lbry_commitment:with_commitment(
        Base,
        <<"lbry-tee-tail@1.0">>,
        <<"tee-tail">>,
        {<<"to-root">>, normalize_hash(ToRoot)},
        [<<"device">>, <<"old-peaks">>, <<"delta-leaves">>,
            <<"to-root">>, <<"tee-evidence">>],
        #{}
    ).

%% @doc Build an `mmr-genesis' evidence message binding the network genesis hash
%% and the snapshot root into a node-signed attestation.
mmr_genesis_evidence({GenesisHash, SnapshotRoot}, Opts) ->
    Evidence =
        attestation_evidence(
            #{
                <<"tee-genesis-hash">> => to_hex(GenesisHash),
                <<"tee-genesis-root">> => to_hex(SnapshotRoot)
            },
            Opts
        ),
    Base = #{
        <<"device">>       => <<"lbry-tee-tail@1.0">>,
        <<"type">>         => <<"mmr-genesis">>,
        <<"tee-evidence">> => Evidence
    },
    hb_lbry_commitment:with_commitment(
        Base,
        <<"lbry-tee-tail@1.0">>,
        <<"mmr-genesis">>,
        {<<"genesis-root">>, normalize_hash(SnapshotRoot)},
        [<<"device">>, <<"tee-evidence">>],
        #{}
    ).

%% Build a node-signed `snp@1.0' attestation report carrying the supplied
%% binding fields. The report is generated through the packaged `snp@1.0' device
%% (capability-switched: real hardware report when supported, else the canned
%% mock report), the binding fields are merged in, and the whole is committed
%% with the node wallet so the attestation's signer==address check holds and the
%% node signature covers the binding.
attestation_evidence(BindingFields, Opts) ->
    {ok, Report} = generate_report(Opts),
    Bound = maps:merge(Report, BindingFields),
    hb_message:commit(Bound, Opts).

%% Generate the raw `snp@1.0' report message, capability-switched. The mock
%% backend primes the process-dictionary NIF mock (read by `dev_snp:generate' in
%% this same resolve process) with the canned report before dispatching.
generate_report(Opts) ->
    GenBase = #{ <<"device">> => <<"snp@1.0">> },
    case generate_backend(Opts) of
        real ->
            hb_ao:resolve(GenBase, <<"generate">>, Opts);
        mock ->
            ?event(lbry_commitment,
                {tee_generate,
                    <<"⚠ SNP report MOCKED - no SEV-SNP hardware; "
                      "canned report (verification still real)">>}),
            {ok, Report} = file:read_file(<<"test/admissible-report.json">>),
            put(mock_snp_nif_response, Report),
            put(mock_snp_nif_enabled, true),
            try hb_ao:resolve(GenBase, <<"generate">>, Opts)
            after
                erase(mock_snp_nif_response),
                erase(mock_snp_nif_enabled)
            end
    end.

%%% --------------------------------------------------------------------------
%%% Layering (L0 trustless + L1 node signature + L2 TEE)
%%% --------------------------------------------------------------------------

%% @doc Build a layered roll-forward evidence message. The node signs a small,
%% commit-safe SUMMARY that binds the snapshot root `from-root' and the rolled
%% forward `to-root' -- this is L1, the node-operator signature, ALWAYS present
%% (the user's "otherwise sig"). When the producer can attest, the embedded
%% `tee-evidence' (L2, an `snp@1.0' attestation binding the same `to-root',
%% signed by the SAME node wallet) is added (the "if snp, add tee+sig"). The
%% bulky trustless MMR proof (L0) is verified separately in its raw form by the
%% `lbry-header@1.0' codec against the pinned root; it is the verifiable payload
%% the node vouches for here, not something re-signed under L1 (the existing
%% mmr-* commitments verify on the raw store surface, where their tuple/list
%% proof structures are intact). The `to-root' binding ties all three layers to
%% one root.
layered_evidence({_OldPeaks, _DeltaLeaves, ToRoot}, Opts) ->
    FromRoot = hb_maps:get(<<"lbry-header-root">>, Opts, undefined, Opts),
    Summary = #{
        <<"device">>    => <<"lbry-header@1.0">>,
        <<"layer">>     => <<"lbry-mmr-rollforward">>,
        <<"from-root">> => to_hex(FromRoot),
        <<"to-root">>   => to_hex(ToRoot)
    },
    Signed = hb_message:commit(Summary, Opts),
    case attest_capable(Opts) of
        true ->
            Signed#{
                <<"tee-evidence">> =>
                    attestation_evidence(#{ <<"tee-to-root">> => to_hex(ToRoot) }, Opts)
            };
        false ->
            Signed
    end.

%% L2 attaches when the host can attest: real SNP hardware (auto), or an
%% explicit mock backend (so the offline shim path is exercised in CI). Auto on
%% a non-SNP host yields the honest L0+L1 fallback with no L2.
attest_capable(Opts) ->
    case hb_maps:get(<<"snp-mode">>, Opts, <<"auto">>, Opts) of
        <<"mock">> -> true;
        <<"real">> -> true;
        _          -> generate_backend(Opts) =:= real
    end.

%% @doc Verify a layered evidence message. L1 (the node signature over the
%% root-binding summary) is always checked. The TEE layer is additive on the
%% producer; REQUIRING it is the verifier's policy: with `snp-required', a
%% message lacking `tee-evidence' is rejected. When present, L2 must (a) verify
%% as an `snp@1.0' attestation and (b) bind the SAME `to-root' as the signed
%% summary. A plain L0+L1 message (no SNP host) verifies fully and for real.
verify_layered(Msg, Opts) ->
    HasTee = hb_maps:get(<<"tee-evidence">>, Msg, undefined, Opts) =/= undefined,
    case snp_required(Opts) andalso not HasTee of
        true ->
            ?event(lbry_commitment, {verify_layered, snp_required_but_absent}),
            false;
        false ->
            L1OK =
                hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, Opts)
                    =:= true,
            L2OK = verify_tee_layer(HasTee, Msg, Opts),
            ?event(lbry_commitment, {verify_layered, {sig, L1OK}, {tee, L2OK}}),
            L1OK andalso L2OK
    end.

verify_tee_layer(false, _Msg, _Opts) ->
    true;
verify_tee_layer(true, Msg, Opts) ->
    Evidence = tee_evidence(Msg, Opts),
    ToRoot = hb_maps:get(<<"to-root">>, Msg, undefined, Opts),
    verify_attestation(Evidence, Opts)
        andalso normalize_hash(field(Evidence, <<"tee-to-root">>, Opts))
            =:= normalize_hash(ToRoot).

snp_required(Opts) ->
    case hb_maps:get(<<"snp-required">>, Opts, false, Opts) of
        true        -> true;
        <<"true">>  -> true;
        _           -> false
    end.

%%% --------------------------------------------------------------------------
%%% Helpers
%%% --------------------------------------------------------------------------

tee_evidence(Base, Opts) ->
    case hb_maps:get(<<"tee-evidence">>, Base, undefined, Opts) of
        undefined -> undefined;
        Evidence  -> hb_cache:ensure_all_loaded(Evidence, Opts)
    end.

field(Msg, Key, Opts) ->
    hb_maps:get(Key, Msg, undefined, Opts).

%% Accept either raw 32-byte hashes or 64-char display hex.
normalize_hash(H) when is_binary(H), byte_size(H) =:= 32 -> H;
normalize_hash(H) when is_binary(H), byte_size(H) =:= 64 -> binary:decode_hex(H);
normalize_hash(_) -> undefined.

normalize_peak({H, Hash}) -> {hb_util:int(H), normalize_hash(Hash)};
normalize_peak([H, Hash]) -> {hb_util:int(H), normalize_hash(Hash)}.

to_hex(H) when is_binary(H), byte_size(H) =:= 32 -> hb_util:to_hex(H);
to_hex(H) when is_binary(H), byte_size(H) =:= 64 -> hb_util:to_lower(H).

%%% --------------------------------------------------------------------------
%%% Tests
%%% --------------------------------------------------------------------------
-ifdef(TEST).

%% A small REAL consistency proof built from mainnet header chunk 0: the first 7
%% block hashes, split into a 4-leaf snapshot and a 3-leaf tail. `FromRoot' is
%% the snapshot root; `ToRoot' is the root after appending the tail. This is the
%% same construction the `lbry-header@1.0' consistency test validates.
chunk_proof() ->
    {ok, Chunk0} = file:read_file("test/fixtures/lbry/chunk0.bin"),
    Leaves = [hb_lbry_mmr:sha256d(binary:part(Chunk0, I * 112, 112))
                || I <- lists:seq(0, 6)],
    {Old, Delta} = lists:split(4, Leaves),
    OldPeaks = lists:foldl(fun(L, A) -> hb_lbry_mmr:mmr_append(A, L) end, [], Old),
    FromRoot = hb_lbry_mmr:bag_peaks([Pk || {_, Pk} <- OldPeaks]),
    ToRoot = hb_lbry_mmr:mmr_root(Leaves),
    {FromRoot, {OldPeaks, Delta, ToRoot}}.

%% The trusted launch configuration matching `test/admissible-report.json'
%% (the canned mock report), mirroring `dev_snp''s round-trip test config.
trusted_config() ->
    #{
        <<"vcpus">> => 32,
        <<"vcpu-type">> => 5,
        <<"vmm-type">> => 1,
        <<"guest-features">> => 1,
        <<"firmware">> =>
            <<"b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a"
              "62244b788e7e408c582ee48a74b289f3acec78510">>,
        <<"kernel">> =>
            <<"69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576">>,
        <<"initrd">> =>
            <<"544045560322dbcd2c454bdc50f35edf0147829ec440e6cb487b4a1503f923c1">>,
        <<"append">> =>
            <<"95a34faced5e487991f9cc2253a41cbd26b708bf00328f98dddbbf6b3ea2892e">>
    }.

opts(FromRoot, Mode) ->
    #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"snp-trusted">> => [trusted_config()],
        <<"snp-enforced-keys">> =>
            [vcpu_type, vmm_type, guest_features, firmware, kernel, initrd, append],
        <<"lbry-header-root">> => FromRoot,
        <<"snp-mode">> => Mode
    }.

%% --- tee-tail ---

tee_tail_mock_positive_test() ->
    {FromRoot, Proof} = chunk_proof(),
    Opts = opts(FromRoot, <<"mock">>),
    Evidence = tee_tail_evidence(Proof, Opts),
    ?assertEqual({ok, true}, verify(Evidence, #{}, Opts)).

tee_tail_tampered_tail_test() ->
    {FromRoot, {OldPeaks, [D0 | Rest], ToRoot}} = chunk_proof(),
    Opts = opts(FromRoot, <<"mock">>),
    Evidence = tee_tail_evidence({OldPeaks, [D0 | Rest], ToRoot}, Opts),
    %% Flip a byte in a tail delta leaf: the consistency linkage no longer
    %% re-bags to `to-root', so the commitment must fail.
    <<B, Tail/binary>> = D0,
    Tampered = Evidence#{ <<"delta-leaves">> => [<<(B bxor 1), Tail/binary>> | Rest] },
    ?assertEqual({ok, false}, verify(Tampered, #{}, Opts)).

tee_tail_binding_mismatch_test() ->
    {FromRoot, Proof} = chunk_proof(),
    Opts = opts(FromRoot, <<"mock">>),
    Evidence = tee_tail_evidence(Proof, Opts),
    %% Point the attestation binding at a different root: linkage still holds,
    %% but the attestation no longer vouches THIS tail, so binding fails.
    TeeEv = maps:get(<<"tee-evidence">>, Evidence),
    Wrong = TeeEv#{ <<"tee-to-root">> => hb_util:to_hex(hb_lbry_mmr:sha256d(<<"x">>)) },
    ?assertEqual({ok, false}, verify(Evidence#{ <<"tee-evidence">> => Wrong }, #{}, Opts)).

tee_tail_real_test_() ->
    { timeout, 60, fun tee_tail_real/0 }.
tee_tail_real() ->
    %% Capability default (auto): generate mocks (no SEV-SNP hardware here) but
    %% the attestation is verified for REAL through the snp@1.0 device, including
    %% the AMD KDS certificate fetch. That fetch is an external dependency whose
    %% Rust verifier `.unwrap()'s a failed/garbled response into a NIF panic; when
    %% KDS is unreachable or returns an unparsable cert, skip rather than fail
    %% (the deterministic mock-mode tests cover the linkage/binding/composition).
    {FromRoot, Proof} = chunk_proof(),
    Opts = opts(FromRoot, <<"auto">>),
    Evidence = tee_tail_evidence(Proof, Opts),
    try verify(Evidence, #{}, Opts) of
        Res -> ?assertEqual({ok, true}, Res)
    catch
        error:nif_panicked ->
            io:format(user,
                "tee_tail_real SKIPPED: AMD KDS VCEK fetch unavailable~n", []),
            ok
    end.

tee_tail_wrong_trusted_test_() ->
    { timeout, 60, fun tee_tail_wrong_trusted/0 }.
tee_tail_wrong_trusted() ->
    {FromRoot, Proof} = chunk_proof(),
    GenOpts = opts(FromRoot, <<"auto">>),
    Evidence = tee_tail_evidence(Proof, GenOpts),
    %% Verify against a trusted config whose ENFORCED firmware hash does not
    %% match the report: the real attestation's trusted-software check rejects
    %% (before any AMD KDS fetch), so the commitment fails.
    BadFw = binary:copy(<<$a>>, 96),
    BadOpts = GenOpts#{ <<"snp-trusted">> => [ (trusted_config())#{ <<"firmware">> => BadFw } ] },
    ?assertEqual({ok, false}, verify(Evidence, #{}, BadOpts)).

%% --- mmr-genesis ---

mmr_genesis_mock_positive_test() ->
    {FromRoot, _Proof} = chunk_proof(),
    {ok, Chunk0} = file:read_file("test/fixtures/lbry/chunk0.bin"),
    GenesisHash = hb_lbry_mmr:sha256d(binary:part(Chunk0, 0, 112)),
    Opts0 = opts(FromRoot, <<"mock">>),
    Opts = Opts0#{ <<"lbry-genesis-hash">> => GenesisHash },
    Evidence = mmr_genesis_evidence({GenesisHash, FromRoot}, Opts),
    ?assertEqual({ok, true}, verify(Evidence, #{}, Opts)).

mmr_genesis_wrong_genesis_test() ->
    {FromRoot, _Proof} = chunk_proof(),
    Opts0 = opts(FromRoot, <<"mock">>),
    GenesisHash = hb_lbry_mmr:sha256d(binary:part(element(2, file:read_file(
        "test/fixtures/lbry/chunk0.bin")), 0, 112)),
    Opts = Opts0#{ <<"lbry-genesis-hash">> => GenesisHash },
    %% Attestation binds a DIFFERENT genesis hash than the verifier pins.
    Evidence = mmr_genesis_evidence({hb_lbry_mmr:sha256d(<<"nope">>), FromRoot}, Opts),
    ?assertEqual({ok, false}, verify(Evidence, #{}, Opts)).

%% --- layering (L0 + L1 + L2) + snp-required policy ---

has_tee(Msg) -> maps:is_key(<<"tee-evidence">>, Msg).

layered_mock_positive_test() ->
    {FromRoot, Proof} = chunk_proof(),
    Opts = opts(FromRoot, <<"mock">>),
    %% Explicit mock attaches L2; L1 node sig + L2 attestation both verify.
    Msg = layered_evidence(Proof, Opts),
    ?assert(has_tee(Msg)),
    ?assert(verify_layered(Msg, Opts)).

layered_fallback_no_snp_test() ->
    %% auto on a non-SNP host: L1 (node sig over the root binding) only, no L2.
    {FromRoot, Proof} = chunk_proof(),
    Opts = opts(FromRoot, <<"auto">>),
    Msg = layered_evidence(Proof, Opts),
    ?assertNot(has_tee(Msg)),
    %% The fallback verifies fully and for real (no mock, no shim).
    ?assert(verify_layered(Msg, Opts)),
    %% A strict verifier that REQUIRES the TEE layer rejects the L1-only message.
    ?assertNot(verify_layered(Msg, Opts#{ <<"snp-required">> => true })).

layered_snp_required_present_test() ->
    {FromRoot, Proof} = chunk_proof(),
    Opts = (opts(FromRoot, <<"mock">>))#{ <<"snp-required">> => true },
    Msg = layered_evidence(Proof, Opts),
    ?assert(has_tee(Msg)),
    ?assert(verify_layered(Msg, Opts)).

%% Capability-aware demonstration: the SAME builder yields L1-only on this
%% non-SNP host and L1+L2 where SNP is present, with one verify call accepting
%% both. Prints the attached layer set so the demo self-describes.
layer_demo_test_() ->
    { timeout, 60, fun layer_demo/0 }.
layer_demo() ->
    {FromRoot, Proof} = chunk_proof(),
    Support = catch hb_snp_nif:check_snp_support(),
    AutoMsg = layered_evidence(Proof, opts(FromRoot, <<"auto">>)),
    MockMsg = layered_evidence(Proof, opts(FromRoot, <<"mock">>)),
    io:format(user,
        "~n=== Odysee MMR roll-forward: capability-switched layered commitment ==="
        "~n host hb_snp_nif:check_snp_support => ~p"
        "~n auto (this host):     ~s"
        "~n snp-host (simulated): ~s"
        "~n one verify_layered/2 accepts both; snp-required upgrades the floor.~n",
        [Support, describe_layers(AutoMsg), describe_layers(MockMsg)]),
    ?assert(verify_layered(AutoMsg, opts(FromRoot, <<"auto">>))),
    ?assert(verify_layered(MockMsg, opts(FromRoot, <<"mock">>))).

describe_layers(Msg) ->
    Base = "L0 trustless-MMR (verified separately) + L1 node-signature",
    case maps:is_key(<<"tee-evidence">>, Msg) of
        true  -> Base ++ " + L2 TEE-attestation";
        false -> Base
    end.

-endif.
