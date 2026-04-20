%%% @doc The TPM 2.0 device — binds HyperBEAM's identity to a real
%%% hardware TPM via `libtss2-esys'.
%%%
%%% This device is the software-layer of the LapEE (Laptop Execution
%%% Environment) appliance architecture. At node startup, the `on.start'
%%% hook invokes `extend' with the running node message as its subject.
%%% The digest of the subject is fed to `TPM2_PCR_Extend' (PCR 15 by
%%% default), binding this specific boot of the node — with this wallet,
%%% this `trusted_signers' set, this device map — to a measurement the
%%% TPM can later quote.
%%%
%%% Any party can then request `attestation', which returns a signed
%%% envelope containing:
%%%   1. EK certificate (chains to TPM vendor root CA)
%%%   2. Attestation Key public key
%%%   3. TPM2_Quote over a PCR set, signed by the AK
%%%   4. The full runtime event log so a verifier can replay the PCR 15
%%%      extend and confirm it matches the quoted value
%%%   5. The node message itself, so the verifier can recompute
%%%      `hb_message:id(NodeMsg, all, Opts)' and confirm equality with
%%%      the extend digest — closing the loop from quote back to the
%%%      specific software stack running.
%%%
%%% The device delegates all TPM operations to the `lapee_tpm_nif' NIF
%%% (a small C layer over libtss2-esys). This module is the HyperBEAM-
%%% shaped interface over that NIF: HB device conventions (`info',
%%% `(Base, Req, Opts)', exports map), standard error returns, and
%%% integration with AO-Core hook dispatch.
-module(dev_tpm2).
-export([info/1, info/3, extend/3, quote/3, pcr_read/3, attestation/3]).
-export([verify/3]).
-export([event_log/1]).
-include("include/hb.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Default PCR that HyperBEAM extends with the node-message identity.
-define(NODE_IDENTITY_PCR, 15).
%% Default PCR selection the quote covers.
-define(DEFAULT_QUOTE_PCRS, [0, 1, 7, 10, 11, 14, 15]).

%%%============================================================================
%%% Device API information
%%%============================================================================

%% @doc Declare the device's public surface.
info(_) ->
    #{
        exports =>
            [
                <<"info">>,
                <<"extend">>,
                <<"quote">>,
                <<"pcr-read">>,
                <<"attestation">>,
                <<"verify">>
            ]
    }.

%% @doc Human-readable documentation for the TPM 2.0 device.
info(_Base, _Req, _Opts) ->
    InfoBody = #{
        <<"description">> =>
            <<"TPM 2.0 device: bind a HyperBEAM node's identity to a real "
              "hardware TPM via libtss2-esys, and produce signed attestations "
              "that chain through quote → PCR extend → event log → node message, "
              "linking a running node's software state to TPM-rooted hardware "
              "attestation.">>,
        <<"version">> => <<"0.1">>,
        <<"specification">> => <<"TPM 2.0 (TCG)">>,
        <<"api">> => #{
            <<"info">> => #{
                <<"description">> => <<"This message.">>
            },
            <<"extend">> => #{
                <<"description">> =>
                    <<"Extend a PCR with the hash of a subject message. "
                      "Default PCR is 15 (LapEE node-identity binding).">>,
                <<"request">> => #{
                    <<"subject">> =>
                        <<"The message (or binary) whose identity should be "
                          "bound to the PCR. If absent, falls back to the "
                          "hook's `body' key, and then to the Base message.">>,
                    <<"pcr">> =>
                        <<"Integer PCR index (0–23). Defaults to 15.">>
                },
                <<"response">> =>
                    <<"`#{<<\"status\">> => 200, <<\"body\">> => "
                      "#{<<\"pcr\">> => N, "
                      "<<\"digest\">>    => base64url(bytes), "
                      "<<\"pcr_after\">> => base64url(bytes)}}'">>
            },
            <<"quote">> => #{
                <<"description">> =>
                    <<"Produce a TPM2_Quote signed by the node's Attestation "
                      "Key over the selected PCR set. Nonce comes from "
                      "`Req/nonce' if present.">>,
                <<"request">> => #{
                    <<"pcrs">> =>
                        <<"List of PCR indices to include (defaults to "
                          "[0, 1, 7, 10, 11, 14, 15]).">>,
                    <<"nonce">> =>
                        <<"base64url-encoded binary nonce (any length). If "
                          "absent, a fresh random 32-byte value is generated. "
                          "Hex input is NOT accepted — HyperBEAM wire is "
                          "base64url everywhere.">>
                }
            },
            <<"pcr-read">> => #{
                <<"description">> =>
                    <<"Read the current value of a PCR via `Esys_PCR_Read'.">>,
                <<"request">> => #{
                    <<"pcr">> => <<"Integer PCR index (required).">>
                }
            },
            <<"attestation">> => #{
                <<"description">> =>
                    <<"Produce a complete LapEE attestation envelope. Contains "
                      "EK cert chain, AK pubkey, TPM2_Quote, runtime event "
                      "log, node message, and the attested chain of trust the "
                      "LapEE verifier checks.">>,
                <<"request">> => #{
                    <<"pcrs">> => <<"Optional PCR selection.">>,
                    <<"nonce">> =>
                        <<"Optional nonce. Typical usage: consumer provides "
                          "a random nonce to prove freshness.">>
                }
            }
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%%%============================================================================
%%% extend/3 — the load-bearing hook entry point
%%%============================================================================

%% @doc Extend a PCR with the hash of a subject.
%%
%% Subject resolution order (highest precedence first):
%%   1. `Req/subject' — if set, use that value.
%%   2. `Req/body'   — the standard hook-payload location.
%%   3. `Base'       — fallback when neither is set.
%%
%% Digest derivation:
%%   * If the resolved subject is a binary of exactly 32 bytes, it is
%%     used as the SHA-256 digest directly.
%%   * If it is any other binary, SHA-256 is applied.
%%   * If it is a map (HyperBEAM message), `hb_message:id(Subject, all, Opts)'
%%     is used — this commits to every committed and uncommitted field in
%%     the message, which is exactly the "bind this specific node identity"
%%     semantic the LapEE paper requires.
%%
%% The PCR is taken from `Req/pcr' (integer or integer-binary), defaulting
%% to 15 — the LapEE node-identity PCR.
%%
%% On success, also records a named event in the runtime event log via
%% `lapee_tpm_nif:append_event/2'. The event log is flushed into every
%% subsequent attestation envelope so a verifier can replay the chain.
extend(Base, Req, Opts) ->
    Subject = resolve_subject(Base, Req, Opts),
    Pcr = resolve_pcr(Req, ?NODE_IDENTITY_PCR, Opts),
    Digest = digest_of(Subject, Opts),
    case nif_pcr_extend(Pcr, Digest) of
        ok ->
            %% Remember the subject (and its id) so that a later
            %% `attestation' call can embed the same node message the
            %% TPM committed to. The hook-dispatch path does not thread
            %% the extended subject through `Opts', so we use
            %% `persistent_term' — same pattern as the event log.
            case Subject of
                S when is_map(S), Pcr =:= ?NODE_IDENTITY_PCR ->
                    persistent_term:put(
                        {dev_tpm2, attested_node_msg}, S);
                _ -> ok
            end,
            EventDescription =
                case Subject of
                    S0 when is_map(S0) ->
                        iolist_to_binary(
                            io_lib:format(
                                "hb_message:id(Subject, all) over "
                                "~B-key message",
                                [maps:size(S0)]));
                    _ -> <<"binary subject (non-message)">>
                end,
            _ = append_event(Pcr,
                #{
                    <<"event_type">> =>
                        <<"EV_HYPERBEAM_NODE_IDENTITY_EXTEND">>,
                    <<"description">> => EventDescription,
                    <<"digest">> => hb_util:encode(Digest),
                    <<"subject_is_message">> =>
                        is_map(Subject)
                }
            ),
            After = case nif_pcr_read(Pcr) of
                {ok, V} -> hb_util:encode(V);
                _ -> <<"?">>
            end,
            {ok, #{
                <<"status">> => 200,
                <<"body">> => #{
                    <<"pcr">> => Pcr,
                    <<"digest">> => hb_util:encode(Digest),
                    <<"pcr_after">> => After
                }
            }};
        {error, Reason} ->
            {error, #{
                <<"status">> => 500,
                <<"body">> => #{
                    <<"error">> => <<"pcr_extend_failed">>,
                    <<"reason">> => hb_util:bin(Reason)
                }
            }}
    end.

%%%============================================================================
%%% quote/3
%%%============================================================================

%% @doc Request a TPM2_Quote over the given PCR selection.
%%
%% Returns the raw TPMS_ATTEST bytes (`quoted'), the AK signature,
%% the current PCR values, and the AK public key. All binary-valued
%% fields are base64url-encoded per AO-Core convention
%% (`hb_util:encode/1' / `hb_util:human_id/1').
quote(_Base, Req, Opts) ->
    Pcrs = resolve_pcr_list(Req, ?DEFAULT_QUOTE_PCRS, Opts),
    Nonce = resolve_nonce(Req),
    case ensure_ak(Opts) of
        {ok, AkTr} ->
            case nif_quote(AkTr, Pcrs, Nonce) of
                {ok, #{quoted := Q, signature := Sig, pcr_values := PcrMap}} ->
                    {ok, #{
                        <<"status">> => 200,
                        <<"body">> => #{
                            <<"pcr_selection">> => Pcrs,
                            <<"nonce">> => hb_util:encode(Nonce),
                            <<"quoted">> => hb_util:encode(Q),
                            <<"signature">> => hb_util:encode(Sig),
                            <<"pcr_values">> =>
                                maps:from_list(
                                    [{integer_to_binary(I),
                                      hb_util:encode(V)}
                                     || {I, V} <- maps:to_list(PcrMap)]),
                            <<"ak_pub_pem">> => ak_pub_pem(Opts)
                        }
                    }};
                {error, Reason} ->
                    error_resp(500, <<"quote_failed">>, Reason)
            end;
        {error, Reason} ->
            error_resp(500, <<"ak_unavailable">>, Reason)
    end.

%%%============================================================================
%%% pcr-read/3
%%%============================================================================

pcr_read(_Base, Req, Opts) ->
    Pcr = resolve_pcr(Req, 0, Opts),
    case nif_pcr_read(Pcr) of
        {ok, V} ->
            {ok, #{
                <<"status">> => 200,
                <<"body">> => #{
                    <<"pcr">> => Pcr,
                    <<"value">> => hb_util:encode(V)
                }
            }};
        {error, Reason} ->
            error_resp(500, <<"pcr_read_failed">>, Reason)
    end.

%%%============================================================================
%%% verify/3 — HB-side attestation verifier
%%%============================================================================

%% @doc Verify an attestation envelope end-to-end in-process. This is
%% what one HyperBEAM node uses to verify a peer, intended to be
%% reached via:
%%
%%   ~relay@1.0/call&relay-path="http://PEER:PORT/~tpm2@2.0a/attestation"
%%       /verify~tpm2@2.0a
%%
%% `Base' is the attestation envelope (same shape emitted by
%% `attestation/3'). Options in `Req':
%%   trusted-ca-pem : PEM bytes of the TPM vendor root CA to trust
%%                    for the EK cert chain. Defaults to the value of
%%                    `lapee_tpm_ca_cert' in `Opts' (a file path).
%%
%% Return shape (always 200 — the `verified' bool is the real verdict):
%%   verified : boolean
%%   verdict  : "accepted" | "rejected"
%%   checks   : list of per-check reports in stable order
%%   Each check: #{ name, ok, detail }
verify(Base, Req, Opts) ->
    Envelope = resolve_envelope(Base, Req, Opts),
    TrustedCaPem = resolve_trusted_ca(Req, Opts),
    Checks = [
        safely_run(fun() -> chk_ek_chain(Envelope, TrustedCaPem) end,
                   <<"EK certificate chains to trusted TPM vendor root CA">>),
        safely_run(fun() -> chk_quote(Envelope) end,
                   <<"TPM2_Quote signature + pcrDigest + nonce all valid">>),
        safely_run(fun() -> chk_event_log_replay(Envelope) end,
                   <<"Runtime event log replay of PCR 15 matches quoted value">>),
        safely_run(fun() -> chk_binding(Envelope) end,
                   <<"PCR 15 extension commits to node_message_id">>),
        safely_run(fun() -> chk_node_msg_shape(Envelope) end,
                   <<"Embedded node_message + id present and correct shape">>)
    ],
    AllOk = lists:all(fun(#{<<"ok">> := Ok}) -> Ok end, Checks),
    Verdict = case AllOk of
        true  -> <<"accepted">>;
        false -> <<"rejected">>
    end,
    {ok, #{
        <<"status">> => 200,
        <<"body">> => #{
            <<"verified">> => AllOk,
            <<"verdict">> => Verdict,
            <<"checks">> => Checks
        }
    }}.

%% Wrap any check in a try/catch so one misformed field doesn't take
%% down the whole verifier — the relevant check just becomes `ok=false,
%% detail=<exception info>'.
safely_run(F, Name) ->
    try F() of
        {ok, Detail}    -> #{ <<"name">> => Name,
                              <<"ok">> => true,
                              <<"detail">> => Detail };
        {error, Detail} -> #{ <<"name">> => Name,
                              <<"ok">> => false,
                              <<"detail">> => Detail }
    catch
        Class:Reason:Stack ->
            #{ <<"name">> => Name,
               <<"ok">> => false,
               <<"detail">> =>
                    iolist_to_binary(io_lib:format(
                        "exception ~p:~p at ~p", [Class, Reason, Stack])) }
    end.

%% Find the attestation envelope in the resolution chain we were
%% handed. In order:
%%   1. Req/envelope, if explicitly provided by the caller
%%   2. If Base itself carries `lapee_attestation_version', it IS the
%%      envelope (direct call)
%%   3. If Base has a `body' key whose value has
%%      `lapee_attestation_version', unwrap it (the common case:
%%      verify is invoked as the second segment of
%%      `.../attestation/verify~tpm2@2.0a' and Base is the response
%%      message produced by `attestation/3').
resolve_envelope(Base, Req, Opts) ->
    case hb_maps:get(<<"envelope">>, Req, undefined, Opts) of
        E when is_map(E) -> E;
        _ ->
            case is_envelope(Base) of
                true -> Base;
                false ->
                    case hb_maps:get(<<"body">>, Base, undefined, Opts) of
                        Inner when is_map(Inner) -> Inner;
                        _ -> Base
                    end
            end
    end.

is_envelope(M) when is_map(M) ->
    hb_maps:get(<<"lapee_attestation_version">>, M, undefined, #{}) /=
        undefined;
is_envelope(_) ->
    false.

resolve_trusted_ca(Req, Opts) ->
    case hb_maps:get(<<"trusted-ca-pem">>, Req, undefined, Opts) of
        Pem when is_binary(Pem), byte_size(Pem) > 0 -> Pem;
        _ ->
            Path = hb_opts:get(lapee_tpm_ca_cert,
                               <<"/etc/lapee/tpm-ca.crt">>, Opts),
            case file:read_file(binary_to_list(Path)) of
                {ok, Pem}  -> Pem;
                {error, _} -> <<>>
            end
    end.

%%---- check 1: EK cert chain --------------------------------------------
%%
%% pkix_path_validation drives a verify_fun when it encounters events
%% it can't resolve unilaterally — most legitimately, unknown TCG
%% extensions on EK certs (tpmManufacturer / tpmModel / tpmVersion /
%% tpmSpecification OIDs, which stock OTP doesn't know). We allow
%% ONLY those extension events through; every {bad_cert, _} event
%% (unknown_ca, self-signed, expired, name-mismatch, etc.) is a hard
%% reject. Returning {valid, State} for everything — the original
%% implementation — was a rubber stamp: pkix would surface
%% `{bad_cert, selfsigned_peer}` for a rogue EK and the callback
%% would tell it "that's fine", defeating the whole chain check.
chk_ek_chain(Envelope, TrustedCaPem) ->
    EkPem = hb_maps:get(<<"ek_cert_pem">>, Envelope, <<>>, #{}),
    case {decode_pem_cert(EkPem), decode_pem_cert(TrustedCaPem)} of
        {{ok, EkDer}, {ok, CaDer}} ->
            CaOtp = public_key:pkix_decode_cert(CaDer, otp),
            case public_key:pkix_path_validation(CaOtp, [EkDer],
                                                 [{verify_fun,
                                                   ek_chain_verify_fun()}]) of
                {ok, _} -> {ok, <<"OpenSSL pkix_path_validation ok">>};
                {error, Why} ->
                    {error,
                        iolist_to_binary(io_lib:format("chain invalid: ~p",
                                                       [Why]))}
            end;
        {_, {error, _}} ->
            {error, <<"trusted-ca-pem missing or unparseable">>};
        {{error, Why}, _} ->
            {error, iolist_to_binary(io_lib:format("ek_cert_pem invalid: ~p",
                                                    [Why]))}
    end.

%% Verify-fun for the EK cert chain validation. Pulled out so it can
%% be unit-tested in isolation — the previous implementation
%% returned `{valid, State}' for every event and that rubber-stamped
%% `{bad_cert, selfsigned_peer}', `{bad_cert, unknown_ca}' et al.
%% Here, only `{extension, _}' events (unknown TCG TPM OIDs) are
%% silently accepted; every `{bad_cert, _}' is a hard reject.
ek_chain_verify_fun() ->
    {fun
        (_, {bad_cert, _} = Reason, _) -> {fail, Reason};
        (_, {extension, _}, State)     -> {unknown, State};
        (_, valid, State)              -> {valid, State};
        (_, valid_peer, State)         -> {valid, State};
        (_, _Other, State)             -> {unknown, State}
     end, []}.

%%---- check 2: quote signature + extraData + pcrDigest -----------------
chk_quote(Envelope) ->
    Q = hb_maps:get(<<"tpm_quote">>, Envelope, #{}, #{}),
    AkPem = hb_maps:get(<<"ak_pub_pem">>, Envelope, <<>>, #{}),
    Quoted = hb_util:decode(hb_maps:get(<<"quoted">>, Q, <<>>, #{})),
    Sig    = hb_util:decode(hb_maps:get(<<"signature">>, Q, <<>>, #{})),
    Nonce  = hb_util:decode(hb_maps:get(<<"nonce">>, Q, <<>>, #{})),
    Sel    = hb_maps:get(<<"pcr_selection">>, Q, [], #{}),
    PcrMap = hb_maps:get(<<"pcr_values">>, Q, #{}, #{}),

    %% Signature: RSA-PSS with SHA-256, salt 32 (matches the NIF).
    case decode_pem_rsa_pub(AkPem) of
        {ok, RSAPub} ->
            case rsa_pss:verify(Quoted, sha256, Sig, RSAPub) of
                true ->
                    chk_tpms_attest(Quoted, Nonce, Sel, PcrMap);
                false ->
                    {error, <<"RSA-PSS(SHA256) verify of TPMS_ATTEST failed">>}
            end;
        {error, Why} ->
            {error, iolist_to_binary(io_lib:format("ak_pub_pem invalid: ~p",
                                                    [Why]))}
    end.

%% Parse TPMS_ATTEST: magic(4) + type(2) + qualifiedSigner(TPM2B) +
%% extraData(TPM2B) + clockInfo(17) + firmwareVersion(8) +
%% attested(TPMS_QUOTE_INFO = TPML_PCR_SELECTION + TPM2B_DIGEST).
chk_tpms_attest(Quoted, ExpectedNonce, SelIndices, PcrMap) ->
    try
        <<_Magic:4/binary, _Type:2/binary, Rest0/binary>> = Quoted,
        {QualifiedSigner, Rest1} = tpm2b(Rest0),
        {ExtraData, Rest2}       = tpm2b(Rest1),
        _ = QualifiedSigner,
        %% clockInfo (17) + firmwareVersion (8) = 25 bytes
        <<_ClockFwInfo:25/binary, NSel:32/unsigned-big,
          SelAndDigest/binary>> = Rest2,
        RestAfterSel = skip_pcr_selections(NSel, SelAndDigest),
        {PcrDigest, _} = tpm2b(RestAfterSel),
        case ExtraData of
            ExpectedNonce ->
                %% Verify pcrDigest = sha256(pcr_values concatenated in
                %% selection order).
                Computed = compute_pcr_digest(SelIndices, PcrMap),
                case Computed of
                    PcrDigest ->
                        {ok,
                            iolist_to_binary(io_lib:format(
                                "sig ok; extraData matches nonce (~B bytes); "
                                "pcrDigest matches ~B reported PCRs",
                                [byte_size(ExtraData), length(SelIndices)]))};
                    _ ->
                        {error, <<"quote pcrDigest does not match "
                                  "sha256(pcr_values)">>}
                end;
            _ ->
                {error,
                    iolist_to_binary(io_lib:format(
                        "extraData != nonce (got ~B bytes, expected ~B)",
                        [byte_size(ExtraData), byte_size(ExpectedNonce)]))}
        end
    catch
        error:{badmatch, _} ->
            {error, <<"TPMS_ATTEST parse error (truncated or wrong shape)">>}
    end.

tpm2b(<<Size:16/unsigned-big, Payload:Size/binary, Rest/binary>>) ->
    {Payload, Rest}.

skip_pcr_selections(0, Rest) -> Rest;
skip_pcr_selections(N, <<_Hash:16/unsigned-big, SizeSelect:8/unsigned-big,
                         _Selection:SizeSelect/binary, Rest/binary>>) ->
    skip_pcr_selections(N - 1, Rest).

compute_pcr_digest(Indices, PcrMap) ->
    Concat =
        lists:foldl(
            fun(I, Acc) ->
                Key = integer_to_binary(I),
                B64 = hb_maps:get(Key, PcrMap, undefined, #{}),
                case B64 of
                    undefined -> throw({missing_pcr, I});
                    _ -> <<Acc/binary, (hb_util:decode(B64))/binary>>
                end
            end,
            <<>>, Indices),
    crypto:hash(sha256, Concat).

%%---- check 3: event-log replay matches quoted PCR 15 ------------------
%%
%% Require at least one PCR-15 event. With zero events, `Replayed'
%% would be the all-zero sentinel; if an attestation also reported
%% PCR 15 as all-zero, the check would vacuously pass. `chk_binding'
%% separately catches that shape, but we make the intent explicit
%% here too: a LapEE node MUST have extended PCR 15 at least once
%% (via the enforced `on.start' hook), so an envelope with zero
%% PCR-15 events is not a valid LapEE attestation regardless of the
%% quoted PCR value.
chk_event_log_replay(Envelope) ->
    Events = [E || E <- hb_maps:get(<<"runtime_event_log">>, Envelope, [],
                                    #{}),
                   int_pcr(hb_maps:get(<<"pcr">>, E, 0, #{})) =:=
                       ?NODE_IDENTITY_PCR],
    Quoted15 =
        hb_maps:get(<<"15">>,
            hb_maps:get(<<"pcr_values">>,
                hb_maps:get(<<"tpm_quote">>, Envelope, #{}, #{}), #{}, #{}),
            undefined, #{}),
    case {Events, Quoted15} of
        {[], _} ->
            {error, <<"no PCR-15 events in runtime_event_log "
                      "(LapEE guest must extend PCR 15 via on.start)">>};
        {_, undefined} ->
            {error, <<"envelope has no tpm_quote.pcr_values[15]">>};
        _ ->
            Replayed =
                lists:foldl(
                    fun(E, Acc) ->
                        Dig = hb_util:decode(
                                hb_maps:get(<<"digest">>, E, <<>>, #{})),
                        crypto:hash(sha256, <<Acc/binary, Dig/binary>>)
                    end,
                    <<0:256>>, Events),
            case hb_util:decode(Quoted15) of
                Replayed ->
                    {ok,
                        iolist_to_binary(io_lib:format(
                            "~B PCR-15 event(s) replay to ~s",
                            [length(Events),
                             binary:part(hb_util:encode(Replayed), 0, 16)]))};
                _ ->
                    {error, <<"replay != quoted pcr_values[15]">>}
            end
    end.

int_pcr(V) when is_integer(V) -> V;
int_pcr(V) when is_binary(V)  -> binary_to_integer(V).

%%---- check 4: PCR 15 event commits to node_message_id ----------------
chk_binding(Envelope) ->
    ExpectedId =
        hb_maps:get(<<"node_message_id">>, Envelope, undefined, #{}),
    Events = [E || E <- hb_maps:get(<<"runtime_event_log">>, Envelope, [],
                                    #{}),
                   int_pcr(hb_maps:get(<<"pcr">>, E, 0, #{})) =:=
                       ?NODE_IDENTITY_PCR],
    case {ExpectedId, Events} of
        {undefined, _} -> {error, <<"no node_message_id in envelope">>};
        {_, []}        -> {error, <<"no PCR-15 events">>};
        {Id, _} ->
            %% node_message_id is a base64url human_id (43 chars).
            %% Each event digest is also base64url. Compare the decoded
            %% raw bytes so encoding quirks don't matter.
            IdRaw =
                try hb_util:decode(Id)
                catch _:_ -> <<>>
                end,
            case byte_size(IdRaw) of
                32 ->
                    %% Real 32-byte id; look for an event whose raw
                    %% digest matches byte-for-byte.
                    Match = [E || E <- Events,
                                  hb_util:decode(
                                    hb_maps:get(<<"digest">>, E, <<>>, #{}))
                                      =:= IdRaw],
                    case Match of
                        [] ->
                            {error, iolist_to_binary(io_lib:format(
                                "no PCR-15 event matches node_message_id ~s",
                                [binary:part(Id, 0,
                                             min(16, byte_size(Id)))]))};
                        [E|_] ->
                            Seq = hb_maps:get(<<"seq">>, E, <<>>, #{}),
                            {ok, iolist_to_binary(io_lib:format(
                                "match at seq=~p", [Seq]))}
                    end;
                Size ->
                    %% Empty / short / unparseable id. Refuse to
                    %% consider any event a match — otherwise an
                    %% envelope with `node_message_id = ""' and an
                    %% event with `digest = ""' would match the empty
                    %% binary trivially.
                    {error, iolist_to_binary(io_lib:format(
                        "node_message_id decodes to ~B bytes, expected 32",
                        [Size]))}
            end
    end.

%%---- check 5: node_message is present + id shape is right ------------
chk_node_msg_shape(Envelope) ->
    Nm = hb_maps:get(<<"node_message">>, Envelope, undefined, #{}),
    Id = hb_maps:get(<<"node_message_id">>, Envelope, undefined, #{}),
    case {Nm, Id} of
        {undefined, _} -> {error, <<"missing node_message">>};
        {_, undefined} -> {error, <<"missing node_message_id">>};
        {M, B} when is_map(M), is_binary(B), byte_size(B) =:= 43 ->
            {ok, iolist_to_binary(io_lib:format(
                "node_message is ~B-key map; id is 43-char base64url",
                [maps:size(M)]))};
        {_, B} when is_binary(B) ->
            {error, iolist_to_binary(io_lib:format(
                "node_message_id wrong size (~B, expected 43)",
                [byte_size(B)]))};
        _ ->
            {error, <<"node_message/_id have unexpected shape">>}
    end.

decode_pem_cert(<<>>) -> {error, empty};
decode_pem_cert(Pem) when is_binary(Pem) ->
    case public_key:pem_decode(Pem) of
        [{'Certificate', Der, not_encrypted} | _] -> {ok, Der};
        Other -> {error, {unexpected_pem_content, Other}}
    end.

decode_pem_rsa_pub(<<>>) -> {error, empty};
decode_pem_rsa_pub(Pem) when is_binary(Pem) ->
    case public_key:pem_decode(Pem) of
        [Entry | _] ->
            try
                case public_key:pem_entry_decode(Entry) of
                    #'RSAPublicKey'{} = Rsa -> {ok, Rsa};
                    #'SubjectPublicKeyInfo'{} = Spki ->
                        {ok, public_key:pkix_decode_cert(Spki, otp)};
                    Other -> {error, {unsupported_pub_key_type, Other}}
                end
            catch
                Cls:R -> {error, {Cls, R}}
            end;
        _ -> {error, no_pem_entries}
    end.

%%%============================================================================
%%% attestation/3 — the full envelope
%%%============================================================================

%% @doc Produce a full LapEE attestation envelope.
%%
%% The envelope is a plain AO-Core message. Binary-like fields are
%% base64url-encoded via `hb_util:encode/1' (same convention as every
%% other hash/id in AO-Core — `hb_message:id/3' returns a base64url
%% binary, `hb_util:human_id/1' does the same, etc.). To receive the
%% envelope inline over HTTP, pass `accept: application/json@1.0' +
%% `accept-bundle: true' (or the equivalent content-negotiation via
%% the `accept' query-string key); the normal codec dispatch in
%% `hb_http' then uses `dev_codec_json' with `bundle => true' and
%% the entire envelope arrives as one JSON body.
%%
%% Envelope shape (v0.3, base64url convention):
%%   lapee_attestation_version : <<"0.3">>
%%   issued_at_unix            : integer
%%   ek_cert_pem               : binary (PEM text)
%%   ak_pub_pem                : binary (PEM text)
%%   tpm_quote                 :
%%     pcr_selection  : [integer]         % PCR indices the quote covers
%%     nonce          : base64url(raw_nonce_bytes)
%%     quoted         : base64url(TPMS_ATTEST bytes)
%%     signature      : base64url(TPMT_SIGNATURE bytes)
%%     pcr_values     : #{ integer_pcr_as_binary => base64url(raw_pcr) }
%%   runtime_event_log         : [ #{ pcr :: integer,
%%                                    digest :: base64url(raw_hash),
%%                                    event_type :: binary, ... } ]
%%   node_message              : the AO-Core message that was extended
%%                               into PCR 15 at boot
%%   node_message_id           : base64url(hb_util:native_id/1 of
%%                               hb_message:id(node_message, all, Opts))
%%   wallet_address            : base64url human id of the operator
attestation(_Base, Req, Opts) ->
    Pcrs = resolve_pcr_list(Req, ?DEFAULT_QUOTE_PCRS, Opts),
    Nonce = resolve_nonce(Req),
    case ensure_ak(Opts) of
        {ok, AkTr} ->
            case nif_quote(AkTr, Pcrs, Nonce) of
                {ok, #{quoted := Q, signature := Sig, pcr_values := PcrMap}} ->
                    {EKCertPem, AKPubPem} =
                        {ek_cert_pem(Opts), ak_pub_pem(Opts)},
                    EventLog = event_log(Opts),
                    NodeMsg = get_node_msg(Opts),
                    NodeMsgId =
                        case NodeMsg of
                            undefined -> null;
                            _ ->
                                hb_util:human_id(
                                    hb_util:native_id(
                                        hb_message:id(NodeMsg, all, Opts)))
                        end,
                    Envelope = #{
                        <<"lapee_attestation_version">> => <<"0.3">>,
                        <<"issued_at_unix">> =>
                            erlang:system_time(second),
                        <<"ek_cert_pem">> => EKCertPem,
                        <<"ak_pub_pem">> => AKPubPem,
                        <<"tpm_quote">> => #{
                            <<"pcr_selection">> => Pcrs,
                            <<"nonce">> => hb_util:encode(Nonce),
                            <<"quoted">> => hb_util:encode(Q),
                            <<"signature">> => hb_util:encode(Sig),
                            <<"pcr_values">> =>
                                maps:from_list(
                                    [{integer_to_binary(I),
                                      hb_util:encode(V)}
                                     || {I, V} <- maps:to_list(PcrMap)])
                        },
                        <<"runtime_event_log">> => EventLog,
                        <<"node_message">> => NodeMsg,
                        <<"node_message_id">> => NodeMsgId,
                        <<"wallet_address">> =>
                            case hb_opts:get(priv_wallet, undefined, Opts) of
                                undefined -> null;
                                W ->
                                    hb_util:human_id(
                                        ar_wallet:to_address(W))
                            end
                    },
                    {ok, #{<<"status">> => 200, <<"body">> => Envelope}};
                {error, Reason} ->
                    error_resp(500, <<"quote_failed">>, Reason)
            end;
        {error, Reason} ->
            error_resp(500, <<"ak_unavailable">>, Reason)
    end.

%%%============================================================================
%%% Runtime event log
%%%============================================================================

%% @doc Return the in-memory event log accumulated since boot.
event_log(_Opts) ->
    case persistent_term:get({dev_tpm2, event_log}, undefined) of
        undefined -> [];
        L -> L
    end.

append_event(Pcr, Payload) ->
    Seq = case persistent_term:get({dev_tpm2, event_seq}, 0) of
        N -> N
    end,
    NewSeq = Seq + 1,
    Entry = Payload#{
        <<"seq">> => Seq,
        <<"pcr">> => Pcr,
        <<"emitted_at_unix">> => erlang:system_time(second)
    },
    Old = case persistent_term:get({dev_tpm2, event_log}, []) of
        L when is_list(L) -> L
    end,
    persistent_term:put({dev_tpm2, event_log}, Old ++ [Entry]),
    persistent_term:put({dev_tpm2, event_seq}, NewSeq),
    ok.

%%%============================================================================
%%% Subject / PCR / nonce resolution helpers
%%%============================================================================

resolve_subject(Base, Req, Opts) ->
    case hb_maps:get(<<"subject">>, Req, undefined, Opts) of
        undefined ->
            case hb_maps:get(<<"body">>, Req, undefined, Opts) of
                undefined -> Base;
                Body -> Body
            end;
        Subject -> Subject
    end.

resolve_pcr(Req, Default, Opts) ->
    case hb_maps:get(<<"pcr">>, Req, undefined, Opts) of
        undefined -> Default;
        I when is_integer(I) -> I;
        B when is_binary(B) ->
            try binary_to_integer(B)
            catch _:_ -> Default end
    end.

resolve_pcr_list(Req, Default, Opts) ->
    case hb_maps:get(<<"pcrs">>, Req, undefined, Opts) of
        undefined -> Default;
        L when is_list(L) ->
            [pcr_int(I) || I <- L];
        B when is_binary(B) ->
            [pcr_int(X) || X <- binary:split(B, <<",">>, [global]), X =/= <<>>];
        _ -> Default
    end.

pcr_int(I) when is_integer(I) -> I;
pcr_int(B) when is_binary(B) ->
    try binary_to_integer(B)
    catch _:_ -> 0
    end.

%% Nonce convention: base64url-encoded bytes. If the caller passes a
%% binary that decodes cleanly as base64url we hand the bytes to the
%% TPM; otherwise we treat the input as the raw bytes directly. Hex
%% is not supported (HyperBEAM wire convention is base64url
%% everywhere).
resolve_nonce(Req) when is_map(Req) ->
    case maps:get(<<"nonce">>, Req, undefined) of
        undefined ->
            crypto:strong_rand_bytes(32);
        B when is_binary(B) ->
            try hb_util:decode(B)
            catch _:_ -> B
            end
    end;
resolve_nonce(_) -> crypto:strong_rand_bytes(32).

%% @doc Produce a 32-byte SHA-256 digest for a subject.
%%
%% For HyperBEAM messages, `hb_message:id(Subject, all, Opts)' returns
%% a human-encoded (base64url, 43 chars) ID; we decode it back to the
%% raw 32-byte hash via `hb_util:native_id/1'. For binaries that are
%% already 32 bytes we use them as-is; for other binaries we hash with
%% SHA-256; for anything else we serialise and hash.
digest_of(Subject, Opts) when is_map(Subject) ->
    HumanId = hb_message:id(Subject, all, Opts),
    hb_util:native_id(HumanId);
digest_of(B, _Opts) when is_binary(B), byte_size(B) =:= 32 ->
    B;
digest_of(B, _Opts) when is_binary(B) ->
    crypto:hash(sha256, B);
digest_of(Other, _Opts) ->
    crypto:hash(sha256,
        iolist_to_binary(io_lib:format("~0p", [Other]))).

error_resp(Status, Err, Reason) ->
    {error, #{
        <<"status">> => Status,
        <<"body">> => #{
            <<"error">> => Err,
            <<"reason">> => hb_util:bin(Reason)
        }
    }}.

get_node_msg(Opts) ->
    %% Two lookups, in order: (1) the node message remembered by the
    %% last PCR-15 extend on this boot (populated by `extend/3'); this
    %% is the ONE the TPM state actually commits to. (2) An explicit
    %% `lapee_attested_node_msg' in Opts, for callers that already know
    %% what was extended (tests, or a caller priming the persistent_term
    %% outside of the normal hook path).
    case persistent_term:get({dev_tpm2, attested_node_msg}, undefined) of
        undefined -> hb_opts:get(lapee_attested_node_msg, undefined, Opts);
        Msg -> Msg
    end.

%%%============================================================================
%%% NIF wrappers + AK caching
%%%============================================================================

ensure_ak(Opts) ->
    case persistent_term:get({dev_tpm2, ak_tr}, undefined) of
        undefined ->
            case init_chain(Opts) of
                ok ->
                    {ok, persistent_term:get({dev_tpm2, ak_tr})};
                {error, _} = E -> E
            end;
        Tr -> {ok, Tr}
    end.

init_chain(Opts) ->
    case nif_startup() of
        ok ->
            case nif_create_ek() of
                {ok, #{esys_tr := EKTr, public_pem := EKPem}} ->
                    persistent_term:put({dev_tpm2, ek_tr}, EKTr),
                    persistent_term:put({dev_tpm2, ek_pub_pem}, EKPem),
                    ensure_ek_cert(EKPem, Opts),
                    case nif_create_signing_key(EKTr) of
                        {ok, #{esys_tr := AKTr, public_pem := AKPem}} ->
                            persistent_term:put({dev_tpm2, ak_tr}, AKTr),
                            persistent_term:put({dev_tpm2, ak_pub_pem}, AKPem),
                            ok;
                        {error, _} = E -> E
                    end;
                {error, _} = E -> E
            end;
        {error, _} = E -> E
    end.

ek_cert_pem(Opts) ->
    case persistent_term:get({dev_tpm2, ek_cert_pem}, undefined) of
        undefined ->
            _ = ensure_ak(Opts),
            persistent_term:get({dev_tpm2, ek_cert_pem}, <<>>);
        P -> P
    end.

ak_pub_pem(Opts) ->
    case persistent_term:get({dev_tpm2, ak_pub_pem}, undefined) of
        undefined ->
            _ = ensure_ak(Opts),
            persistent_term:get({dev_tpm2, ak_pub_pem}, <<>>);
        P -> P
    end.

%% Issue a test-CA-signed cert over the EK's public key. Matches the
%% existing lapee verifier contract.
ensure_ek_cert(EKPem, Opts) ->
    CaCert = hb_opts:get(lapee_tpm_ca_cert,
                         <<"/etc/lapee/tpm-ca.crt">>, Opts),
    CaKey = hb_opts:get(lapee_tpm_ca_key,
                         <<"/etc/lapee/tpm-ca.key">>, Opts),
    Out = hb_opts:get(lapee_tpm_ek_cert,
                         <<"/run/lapee/ek.crt">>, Opts),
    _ = filelib:ensure_dir(binary_to_list(Out)),
    PubPath = <<Out/binary, ".pub.pem">>,
    ok = file:write_file(PubPath, EKPem),
    TmpKey = <<Out/binary, ".tmp.key">>,
    TmpCsr = <<Out/binary, ".csr">>,
    TmpCnf = <<Out/binary, ".cnf">>,
    ok = file:write_file(TmpCnf,
        <<"[req]\ndistinguished_name=dn\nprompt=no\n[dn]\nCN=LapEE Test EK\n">>),
    _ = os:cmd(io_lib:format("openssl genrsa -out ~s 2048 2>/dev/null",
                             [TmpKey])),
    _ = os:cmd(io_lib:format("openssl req -new -key ~s -out ~s -config ~s 2>&1",
                             [TmpKey, TmpCsr, TmpCnf])),
    _ = os:cmd(io_lib:format(
        "openssl x509 -req -in ~s -CA ~s -CAkey ~s -CAcreateserial "
        "-out ~s -days 3650 -force_pubkey ~s 2>&1",
        [TmpCsr, CaCert, CaKey, Out, PubPath])),
    case file:read_file(Out) of
        {ok, Pem} ->
            persistent_term:put({dev_tpm2, ek_cert_pem}, Pem);
        _ -> ok
    end,
    _ = file:delete(TmpKey),
    _ = file:delete(TmpCsr),
    _ = file:delete(TmpCnf),
    _ = file:delete(PubPath),
    ok.

%%----------------------------------------------------------------------------
%% NIF-facing wrappers. We resolve the NIF lazily: first a runtime module
%% `lapee_tpm_nif' (if HB is built with the NIF linked in via its rebar
%% port_specs), falling back to dlopening a .so at well-known paths.
%%----------------------------------------------------------------------------

nif_module() ->
    case code:is_loaded(lapee_tpm_nif) of
        {file, _} -> lapee_tpm_nif;
        false ->
            case code:ensure_loaded(lapee_tpm_nif) of
                {module, _} -> lapee_tpm_nif;
                _ -> not_loaded
            end
    end.

nif_startup() ->
    case nif_module() of
        not_loaded -> {error, nif_not_loaded};
        M -> catch M:startup()
    end.

nif_pcr_extend(Pcr, Digest) ->
    case nif_module() of
        not_loaded -> {error, nif_not_loaded};
        M -> catch M:pcr_extend(Pcr, Digest)
    end.

nif_pcr_read(Pcr) ->
    case nif_module() of
        not_loaded -> {error, nif_not_loaded};
        M -> catch M:pcr_read(Pcr)
    end.

nif_create_ek() ->
    case nif_module() of
        not_loaded -> {error, nif_not_loaded};
        M -> catch M:create_primary_ek()
    end.

nif_create_signing_key(EKTr) ->
    case nif_module() of
        not_loaded -> {error, nif_not_loaded};
        M -> catch M:create_signing_key(EKTr)
    end.

nif_quote(AKTr, Pcrs, Nonce) ->
    case nif_module() of
        not_loaded -> {error, nif_not_loaded};
        M -> catch M:quote(AKTr, Pcrs, Nonce)
    end.

%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).

info_shape_test() ->
    Info = info(ignored),
    ?assert(maps:is_key(exports, Info)),
    Exports = maps:get(exports, Info),
    ?assert(lists:member(<<"extend">>, Exports)),
    ?assert(lists:member(<<"quote">>, Exports)),
    ?assert(lists:member(<<"pcr-read">>, Exports)),
    ?assert(lists:member(<<"attestation">>, Exports)).

info_docs_test() ->
    {ok, #{<<"status">> := 200, <<"body">> := Body}} = info(#{}, #{}, #{}),
    ?assert(maps:is_key(<<"description">>, Body)),
    ?assert(maps:is_key(<<"api">>, Body)),
    Api = maps:get(<<"api">>, Body),
    ?assert(maps:is_key(<<"extend">>, Api)),
    ?assert(maps:is_key(<<"attestation">>, Api)).

digest_of_32_byte_binary_test() ->
    B32 = <<0:256>>,
    ?assertEqual(B32, digest_of(B32, #{})).

digest_of_arbitrary_binary_test() ->
    Bin = <<"hello">>,
    ?assertEqual(crypto:hash(sha256, Bin), digest_of(Bin, #{})).

digest_of_message_uses_hb_message_id_test() ->
    %% Placeholder: would require hb_message loaded; sanity-check the
    %% code path at least.
    Msg = #{<<"a">> => 1, <<"b">> => 2},
    D = digest_of(Msg, #{}),
    ?assert(byte_size(D) =:= 32).

resolve_subject_test() ->
    %% Req/subject wins over body which wins over Base.
    ?assertEqual(<<"subj">>,
        resolve_subject(<<"base">>, #{<<"subject">> => <<"subj">>}, #{})),
    ?assertEqual(<<"body">>,
        resolve_subject(<<"base">>, #{<<"body">> => <<"body">>}, #{})),
    ?assertEqual(<<"base">>,
        resolve_subject(<<"base">>, #{}, #{})).

resolve_pcr_default_test() ->
    ?assertEqual(15, resolve_pcr(#{}, 15, #{})),
    ?assertEqual(10, resolve_pcr(#{<<"pcr">> => 10}, 15, #{})),
    ?assertEqual(7, resolve_pcr(#{<<"pcr">> => <<"7">>}, 15, #{})).

resolve_pcr_list_test() ->
    ?assertEqual([0, 1, 7],
        resolve_pcr_list(#{<<"pcrs">> => [0, 1, 7]},
                         ?DEFAULT_QUOTE_PCRS, #{})),
    ?assertEqual([0, 7, 15],
        resolve_pcr_list(#{<<"pcrs">> => <<"0,7,15">>},
                         ?DEFAULT_QUOTE_PCRS, #{})),
    ?assertEqual(?DEFAULT_QUOTE_PCRS,
        resolve_pcr_list(#{}, ?DEFAULT_QUOTE_PCRS, #{})).

%% Regression test: `chk_event_log_replay' must refuse to
%% "replay" zero events into a zero PCR and call it valid. Even
%% though `chk_binding' catches the same shape, we want the replay
%% check to be explicit about non-emptiness too — defence in depth.
chk_event_log_replay_rejects_empty_events_test() ->
    Zero43 = hb_util:encode(<<0:256>>),
    Envelope = #{
        <<"runtime_event_log">> => [],
        <<"tpm_quote">> => #{
            <<"pcr_values">> => #{<<"15">> => Zero43}
        }
    },
    ?assertMatch({error, _}, chk_event_log_replay(Envelope)).

%% Regression test: `chk_binding' must refuse to treat an empty /
%% malformed node_message_id as matching an empty event digest
%% (both would trivially `hb_util:decode' to `<<>>'). Real ids
%% decode to 32 bytes; anything else is a hard reject.
chk_binding_rejects_empty_id_test() ->
    %% Event whose digest decodes to <<>>.
    EmptyDigestEvent = #{<<"pcr">> => 15,
                         <<"digest">> => <<"">>,
                         <<"seq">> => 0},
    EnvelopeEmptyId = #{
        <<"node_message_id">> => <<"">>,
        <<"runtime_event_log">> => [EmptyDigestEvent]
    },
    ?assertMatch({error, _}, chk_binding(EnvelopeEmptyId)),
    %% Also: id that decodes to fewer than 32 bytes (shorter base64url).
    EnvelopeShortId = #{
        <<"node_message_id">> => <<"AAAA">>,   %% 3 bytes
        <<"runtime_event_log">> =>
            [EmptyDigestEvent#{<<"digest">> => <<"AAAA">>}]
    },
    ?assertMatch({error, _}, chk_binding(EnvelopeShortId)).

%% Regression test: the verify_fun used in chk_ek_chain must reject
%% every structural / trust failure pkix can report, and only let
%% through unknown-extension events. A previous implementation
%% returned {valid, _} for every event, which rubber-stamped rogue
%% self-signed EK certs as "OpenSSL pkix_path_validation ok".
ek_chain_verify_fun_rejects_bad_certs_test() ->
    {F, []} = ek_chain_verify_fun(),
    %% Any {bad_cert, _} must fail hard.
    ?assertMatch({fail, {bad_cert, unknown_ca}},
                 F(ignored, {bad_cert, unknown_ca},    state)),
    ?assertMatch({fail, {bad_cert, selfsigned_peer}},
                 F(ignored, {bad_cert, selfsigned_peer}, state)),
    ?assertMatch({fail, {bad_cert, invalid_issuer}},
                 F(ignored, {bad_cert, invalid_issuer}, state)),
    ?assertMatch({fail, {bad_cert, invalid_signature}},
                 F(ignored, {bad_cert, invalid_signature}, state)),
    ?assertMatch({fail, {bad_cert, cert_expired}},
                 F(ignored, {bad_cert, cert_expired},   state)),
    %% Unknown TCG TPM extensions (tpmManufacturer /
    %% tpmModel / tpmVersion / tpmSpecification OIDs) are
    %% informational; let pkix decide.
    ?assertMatch({unknown, state},
                 F(ignored, {extension, some_tcg_ext}, state)),
    %% Valid events pass through.
    ?assertMatch({valid, state}, F(ignored, valid, state)),
    ?assertMatch({valid, state}, F(ignored, valid_peer, state)),
    ok.

event_log_append_test() ->
    %% Reset state for the test.
    persistent_term:erase({dev_tpm2, event_log}),
    persistent_term:erase({dev_tpm2, event_seq}),
    ?assertEqual([], event_log(#{})),
    ok = append_event(15, #{<<"event_type">> => <<"T">>}),
    Log = event_log(#{}),
    ?assertEqual(1, length(Log)),
    [E1] = Log,
    ?assertEqual(15, maps:get(<<"pcr">>, E1)),
    ?assertEqual(0, maps:get(<<"seq">>, E1)),
    ok = append_event(15, #{<<"event_type">> => <<"U">>}),
    ?assertEqual(2, length(event_log(#{}))).

-endif.
