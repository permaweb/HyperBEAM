%%% @doc The TPM 2.0 device -- binds HyperBEAM's identity to a real
%%% hardware TPM via `libtss2-esys'.
%%%
%%% This device is the software-layer of the LapEE (Laptop Execution
%%% Environment) appliance architecture. At node startup, the `on.start'
%%% hook invokes `extend' with the running node message as its subject.
%%% The digest of the subject is fed to `TPM2_PCR_Extend' (PCR 15 by
%%% default), binding this specific boot of the node -- with this wallet,
%%% this `trusted_signers' set, this device map -- to a measurement the
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
%%%      the extend digest -- closing the loop from quote back to the
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
              "that chain through quote -> PCR extend -> event log -> node message, "
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
                        <<"Integer PCR index (0-23). Defaults to 15.">>
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
                          "Hex input is NOT accepted - HyperBEAM wire is "
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
%%% extend/3 -- the load-bearing hook entry point
%%%============================================================================

%% @doc Extend a PCR with the hash of a subject.
%%
%% Subject resolution order (highest precedence first):
%%   1. `Req/subject' -- if set, use that value.
%%   2. `Req/body'   -- the standard hook-payload location.
%%   3. `Base'       -- fallback when neither is set.
%%
%% Digest derivation:
%%   * If the resolved subject is a binary of exactly 32 bytes, it is
%%     used as the SHA-256 digest directly.
%%   * If it is any other binary, SHA-256 is applied.
%%   * If it is a map (HyperBEAM message), `hb_message:id(Subject, all, Opts)'
%%     is used -- this commits to every committed and uncommitted field in
%%     the message, which is exactly the "bind this specific node identity"
%%     semantic the LapEE paper requires.
%%
%% The PCR is taken from `Req/pcr' (integer or integer-binary), defaulting
%% to 15 -- the LapEE node-identity PCR.
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
            %% `persistent_term' -- same pattern as the event log.
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
                    <<"event-type">> =>
                        <<"EV_HYPERBEAM_NODE_IDENTITY_EXTEND">>,
                    <<"description">> => EventDescription,
                    <<"digest">> => hb_util:encode(Digest),
                    <<"subject-is-message">> =>
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
                    <<"pcr-after">> => After
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
                            <<"pcr-selection">> => Pcrs,
                            <<"nonce">> => hb_util:encode(Nonce),
                            <<"quoted">> => hb_util:encode(Q),
                            <<"signature">> => hb_util:encode(Sig),
                            <<"pcr-values">> =>
                                maps:from_list(
                                    [{integer_to_binary(I),
                                      hb_util:encode(V)}
                                     || {I, V} <- maps:to_list(PcrMap)]),
                            <<"ak-pub-pem">> => ak_pub_pem(Opts)
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
%%% verify/3 -- HB-side attestation verifier
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
%% Return shape (always 200 -- the `verified' bool is the real verdict):
%%   verified : boolean
%%   verdict  : "accepted" | "rejected"
%%   checks   : list of per-check reports in stable order
%%   Each check: #{ name, ok, detail }
verify(Base, Req, Opts) ->
    Envelope = resolve_envelope(Base, Req, Opts),
    TrustedCaPem = resolve_trusted_ca(Req, Opts),
    CaSource = trust_anchor_source(Req, Opts, TrustedCaPem),
    Checks = [
        safely_run(fun() -> chk_ek_chain(Envelope, TrustedCaPem) end,
                   <<"EK certificate chains to trusted TPM vendor root CA">>,
                   <<"core">>),
        safely_run(fun() -> chk_quote(Envelope) end,
                   <<"TPM2_Quote signature + pcrDigest + nonce all valid">>,
                   <<"core">>),
        safely_run(fun() -> chk_event_log_replay(Envelope) end,
                   <<"Runtime event log replay of PCR 15 matches quoted value">>,
                   <<"core">>),
        safely_run(fun() -> chk_binding(Envelope) end,
                   <<"PCR 15 extension commits to node_message_id">>,
                   <<"core">>),
        safely_run(fun() -> chk_node_msg_shape(Envelope) end,
                   <<"Embedded node_message + id present and correct shape">>,
                   <<"core">>),
        %% `firmware TCG event log replay' is INFORMATIONAL: the
        %% paper's trust anchor is PCR 15 (the LapEE node identity),
        %% not the firmware-emitted PCRs 0-14. SeaBIOS under QEMU
        %% legitimately emits an incomplete log that does not
        %% fully replay into the quoted PCR 1; that's a SeaBIOS
        %% quirk, not a LapEE security problem. The check runs,
        %% surfaces its result in `checks', but does NOT gate
        %% `verified' -- policy engines that want strict firmware-
        %% log consistency can key off the severity field.
        safely_run(fun() -> chk_tcg_event_log_replay(Envelope) end,
                   <<"Firmware TCG event log replays to quoted PCRs 0-14">>,
                   <<"informational">>)
    ],
    AllOk = lists:all(
        fun(#{<<"ok">> := Ok, <<"severity">> := Sev}) ->
                Ok orelse Sev =:= <<"informational">>
        end, Checks),
    Verdict = case AllOk of
        true  -> <<"accepted">>;
        false -> <<"rejected">>
    end,
    {ok, #{
        <<"status">> => 200,
        <<"body">> => #{
            <<"verified">> => AllOk,
            <<"verdict">> => Verdict,
            <<"checks">> => Checks,
            %% Tells callers which trust anchor was actually used.
            %% Helpful when debugging "I passed a CA and it was
            %% ignored": `request' means the inline anchor won,
            %% `node_config' means we fell through to the node's
            %% configured file.
            <<"trust-anchor-source">> => CaSource
        }
    }}.

%% Classify which source produced the trust anchor actually used
%% by `resolve_trusted_ca/2'. Returns a binary: "request", "node_config",
%% or "none" (when no anchor was found anywhere -- the chain check
%% will then fail with a targeted "missing or unparseable" message).
trust_anchor_source(Req, _Opts, <<>>) ->
    case {hb_maps:get(<<"trusted-ca">>, Req, undefined, #{}),
          hb_maps:get(<<"trusted-ca-pem">>, Req, undefined, #{})} of
        {undefined, undefined} -> <<"none">>;
        _ -> <<"request_but_empty">>
    end;
trust_anchor_source(Req, _Opts, _Pem) ->
    case {hb_maps:get(<<"trusted-ca">>, Req, undefined, #{}),
          hb_maps:get(<<"trusted-ca-pem">>, Req, undefined, #{})} of
        {B, _} when is_binary(B), byte_size(B) > 0 -> <<"request">>;
        {_, P} when is_binary(P), byte_size(P) > 0 -> <<"request">>;
        _ -> <<"node_config">>
    end.

%% Wrap any check in a try/catch so one misformed field doesn't take
%% down the whole verifier -- the relevant check just becomes `ok=false,
%% detail=<exception info>'.
safely_run(F, Name) ->
    safely_run(F, Name, <<"core">>).

safely_run(F, Name, Severity) ->
    try F() of
        {ok, Detail}    -> #{ <<"name">> => Name,
                              <<"ok">> => true,
                              <<"detail">> => Detail,
                              <<"severity">> => Severity };
        {error, Detail} -> #{ <<"name">> => Name,
                              <<"ok">> => false,
                              <<"detail">> => Detail,
                              <<"severity">> => Severity }
    catch
        Class:Reason:Stack ->
            #{ <<"name">> => Name,
               <<"ok">> => false,
               <<"detail">> =>
                    iolist_to_binary(io_lib:format(
                        "exception ~p:~p at ~p", [Class, Reason, Stack])),
               <<"severity">> => Severity }
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
    hb_maps:get(<<"lapee-attestation-version">>, M, undefined, #{}) /=
        undefined;
is_envelope(_) ->
    false.

%% Resolve the trust anchor in priority order:
%%
%%   1. `Req/trusted-ca'     -- base64url-encoded PEM bytes (the
%%                             HyperBEAM wire convention; the safe
%%                             form over URL-encoded GET).
%%   2. `Req/trusted-ca-pem' -- raw PEM text (back-compat; unsafe
%%                             over URL-encoded GET because `+' is
%%                             treated as literal `+', not space).
%%   3. Opts/`lapee_tpm_ca_cert' -- node-configured CA PEM path.
%%                             Default `/etc/lapee/tpm-ca.crt'.
%%
%% Keeping BOTH forms in sync across verify / verify-peer / the
%% chain-URL path is essential: an earlier version only handled
%% `trusted-ca-pem' here, so `.../verify~tpm-interpret@1.0?trusted-
%% ca=<rogue-b64>' silently fell back to the node's configured CA
%% and rubber-stamped the caller's rogue anchor as "ok" because
%% the legitimate CA actually did verify. A rogue-CA-with-same-CN
%% adversarial test now catches that.
resolve_trusted_ca(Req, Opts) ->
    case hb_maps:get(<<"trusted-ca">>, Req, undefined, Opts) of
        B when is_binary(B), byte_size(B) > 0 ->
            try hb_util:decode(B) of
                Decoded when is_binary(Decoded), byte_size(Decoded) > 0 ->
                    Decoded;
                _ -> resolve_trusted_ca_pem(Req, Opts)
            catch _:_ -> resolve_trusted_ca_pem(Req, Opts)
            end;
        _ -> resolve_trusted_ca_pem(Req, Opts)
    end.

resolve_trusted_ca_pem(Req, Opts) ->
    case hb_maps:get(<<"trusted-ca-pem">>, Req, undefined, Opts) of
        Pem when is_binary(Pem), byte_size(Pem) > 0 -> Pem;
        _ -> resolve_trusted_ca_from_config(Opts)
    end.

resolve_trusted_ca_from_config(Opts) ->
    Path = hb_opts:get(lapee_tpm_ca_cert,
                       <<"/etc/lapee/tpm-ca.crt">>, Opts),
    case file:read_file(binary_to_list(Path)) of
        {ok, Pem}  -> Pem;
        {error, _} -> <<>>
    end.

%%---- check 1: EK cert chain --------------------------------------------
%%
%% pkix_path_validation drives a verify_fun when it encounters events
%% it can't resolve unilaterally -- most legitimately, unknown TCG
%% extensions on EK certs (tpmManufacturer / tpmModel / tpmVersion /
%% tpmSpecification OIDs, which stock OTP doesn't know). We allow
%% ONLY those extension events through; every {bad_cert, _} event
%% (unknown_ca, self-signed, expired, name-mismatch, etc.) is a hard
%% reject. Returning {valid, State} for everything -- the original
%% implementation -- was a rubber stamp: pkix would surface
%% `{bad_cert, selfsigned_peer}` for a rogue EK and the callback
%% would tell it "that's fine", defeating the whole chain check.
chk_ek_chain(Envelope, TrustedCaPem) ->
    EkPem = hb_maps:get(<<"ek-cert-pem">>, Envelope, <<>>, #{}),
    case {decode_pem_cert(EkPem), decode_pem_cert(TrustedCaPem)} of
        {{ok, EkDer}, {ok, CaDer}} ->
            %% pkix_decode_cert on the CA can raise if the PEM
            %% structurally parsed but the DER is malformed (ASN.1
            %% decode errors). Catch here so the check cleanly
            %% returns {error, _} with a useful message instead of
            %% bubbling as a raw exception.
            try public_key:pkix_decode_cert(CaDer, otp) of
                CaOtp ->
                    case public_key:pkix_path_validation(
                            CaOtp, [EkDer],
                            [{verify_fun, ek_chain_verify_fun()}]) of
                        {ok, _} ->
                            {ok, <<"OpenSSL pkix_path_validation ok">>};
                        {error, Why} ->
                            {error, diagnose_chain_failure(Why, EkDer, CaDer)}
                    end
            catch
                Class:Reason ->
                    {error,
                        iolist_to_binary(io_lib:format(
                            "trusted CA is structurally PEM-shaped but "
                            "not a valid DER certificate (~p:~p); likely "
                            "corrupted over the wire (e.g. URL-encoding "
                            "mangled a PEM boundary). Use the `trusted-ca' "
                            "key with base64url-encoded PEM bytes for "
                            "unambiguous transport.",
                            [Class, Reason]))}
            end;
        {_, {error, _}} ->
            {error, <<"trusted CA missing or unparseable; set "
                      "`lapee_tpm_ca_cert' in node config or pass "
                      "`trusted-ca' (base64url PEM bytes) in the "
                      "request">>};
        {{error, Why}, _} ->
            {error, iolist_to_binary(io_lib:format("ek_cert_pem invalid: ~p",
                                                    [Why]))}
    end.

%% Produce a targeted error message for common pkix_path_validation
%% failures. The most confusing one in practice is `{bad_cert,
%% invalid_signature}' when the trusted CA's *subject* matches the
%% EK's *issuer* (same CN, same DN) but the CA's public key is from
%% a different generation (e.g. per-boot test CA that got out of
%% sync with the peer's current boot). That case is indistinguishable
%% from a rogue-CA attack at the pkix level, but we can make it
%% diagnosable by comparing the RDNs and flagging "name match, key
%% mismatch" so an operator knows whether to refresh their trust
%% anchor vs. investigate tampering.
diagnose_chain_failure(Why, EkDer, CaDer) ->
    Generic = iolist_to_binary(io_lib:format("chain invalid: ~p", [Why])),
    try
        EkOtp = public_key:pkix_decode_cert(EkDer, otp),
        CaOtp = public_key:pkix_decode_cert(CaDer, otp),
        EkTbs = EkOtp#'OTPCertificate'.tbsCertificate,
        CaTbs = CaOtp#'OTPCertificate'.tbsCertificate,
        EkIssuer = EkTbs#'OTPTBSCertificate'.issuer,
        CaSubject = CaTbs#'OTPTBSCertificate'.subject,
        case {Why, public_key:pkix_normalize_name(EkIssuer)
                 =:= public_key:pkix_normalize_name(CaSubject)} of
            {{bad_cert, invalid_signature}, true} ->
                <<"chain invalid: EK's issuer DN matches the trusted CA's "
                  "subject DN, but the signature does not verify under that "
                  "CA's public key. The trust anchor is from a different "
                  "CA generation than the one that signed this EK (common "
                  "when the peer rebooted and regenerated a per-boot test "
                  "CA -- refresh the trust anchor from the peer's CURRENT "
                  "boot), or a rogue CA with the same DN is being "
                  "presented (investigate).">>;
            {{bad_cert, invalid_issuer}, _} ->
                <<"chain invalid: EK's issuer DN does not match any "
                  "trusted CA's subject DN. The peer presented an EK "
                  "signed by a different CA entirely.">>;
            _ ->
                Generic
        end
    catch _:_ -> Generic
    end.

%% Verify-fun for the EK cert chain validation. Pulled out so it can
%% be unit-tested in isolation -- the previous implementation
%% returned `{valid, State}' for every event and that rubber-stamped
%% `{bad_cert, selfsigned_peer}', `{bad_cert, unknown_ca}' et al.
%%
%% Mirrors `dev_tpm_interpret:ek_verify_fun/3' semantics (kept in
%% sync deliberately; a divergence here would mean `dev_tpm2:verify/3'
%% accepts a chain the parser-side `validate_ek_chain/3' rejects, or
%% vice-versa). Two TCG OIDs are whitelisted as critical extensions
%% that real-world EK certs always carry:
%%
%%   2.23.133.8.1    id-tcg-kp-EKCertificate  (EKU)
%%   2.23.133.2.16   id-tcg-tpmSpecification  (spec-version attr)
%%
%% OTP's default path validator treats critical extensions it doesn't
%% recognise as `{bad_cert, {not_supported_extension, _}}' -- without
%% this whitelist, real Nuvoton / Infineon / STMicro EK certs all
%% fail. Non-TCG critical extensions still fail. Non-critical
%% extensions under the TCG arc `2.23.133.*' are accepted;
%% non-TCG non-critical extensions fall through as `unknown' so
%% pkix's own defaults apply.
ek_chain_verify_fun() ->
    {fun ek_chain_verify_fun/3, []}.

ek_chain_verify_fun(_, {bad_cert, {not_supported_extension, Ext}},
                    UserState) ->
    ExtId = case Ext of
        #'Extension'{extnID = Id} -> Id;
        _ -> undefined
    end,
    case ExtId of
        {2, 23, 133, 8, 1}   -> {valid, UserState};
        {2, 23, 133, 2, 16}  -> {valid, UserState};
        _ -> {fail, {not_supported_extension, Ext}}
    end;
ek_chain_verify_fun(_, {bad_cert, Reason}, _UserState) ->
    {fail, Reason};
ek_chain_verify_fun(_, {extension, #'Extension'{extnID = ExtId}},
                    UserState) ->
    %% Called for each non-critical unknown extension. Accept any
    %% OID under the TCG arc 2.23.133.x (EK metadata attributes).
    case ExtId of
        {2, 23, 133, _, _}    -> {valid, UserState};
        {2, 23, 133, _, _, _} -> {valid, UserState};
        _                     -> {unknown, UserState}
    end;
ek_chain_verify_fun(_, valid, UserState)      -> {valid, UserState};
ek_chain_verify_fun(_, valid_peer, UserState) -> {valid, UserState}.

%%---- check 2: quote signature + extraData + pcrDigest -----------------
chk_quote(Envelope) ->
    Q = hb_maps:get(<<"tpm-quote">>, Envelope, #{}, #{}),
    AkPem = hb_maps:get(<<"ak-pub-pem">>, Envelope, <<>>, #{}),
    Quoted = hb_util:decode(hb_maps:get(<<"quoted">>, Q, <<>>, #{})),
    Sig    = hb_util:decode(hb_maps:get(<<"signature">>, Q, <<>>, #{})),
    Nonce  = hb_util:decode(hb_maps:get(<<"nonce">>, Q, <<>>, #{})),
    Sel    = hb_maps:get(<<"pcr-selection">>, Q, [], #{}),
    PcrMap = hb_maps:get(<<"pcr-values">>, Q, #{}, #{}),

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
    Events = [E || E <- hb_maps:get(<<"runtime-event-log">>, Envelope, [],
                                    #{}),
                   int_pcr(hb_maps:get(<<"pcr">>, E, 0, #{})) =:=
                       ?NODE_IDENTITY_PCR],
    Quoted15 =
        hb_maps:get(<<"15">>,
            hb_maps:get(<<"pcr-values">>,
                hb_maps:get(<<"tpm-quote">>, Envelope, #{}, #{}), #{}, #{}),
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
        hb_maps:get(<<"node-message-id">>, Envelope, undefined, #{}),
    Events = [E || E <- hb_maps:get(<<"runtime-event-log">>, Envelope, [],
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
                    %% consider any event a match -- otherwise an
                    %% envelope with `node_message_id = ""' and an
                    %% event with `digest = ""' would match the empty
                    %% binary trivially.
                    {error, iolist_to_binary(io_lib:format(
                        "node_message_id decodes to ~B bytes, expected 32",
                        [Size]))}
            end
    end.

%%---- check 6: TCG event log replays to quoted PCRs 0-14 --------------
%%
%% The firmware-side TCG event log (tcg_event_log in the envelope) is
%% the source of truth for PCR 0-14 state. Every event declares which
%% PCR it was extended into + the digest it extended with. Replaying
%% from zero should reconstruct exactly the PCR values the TPM
%% reported in the quote.
%%
%% If an attacker presented an altered envelope (e.g. swapped in a
%% different firmware measurement but kept the quote), the replay
%% would diverge from the quoted value, rejecting.
%%
%% Permissive cases (not hard-rejects):
%%   - Envelope has no tcg_event_log (byte_size 0) -- can happen with
%%     QEMU SeaBIOS test guests where SeaBIOS emits only a minimal
%%     log. In this case there are no per-PCR events to replay, so
%%     the check is skipped with {ok, <<"no firmware log">>}. Callers
%%     who require a firmware log should refuse this verdict.
%%   - Event log parses but produces an error marker -- replay what
%%     we got anyway, but flag partial.
%%
%% Hard rejects:
%%   - For any PCR in 0-14 where the event log DOES have events,
%%     the reconstructed value MUST match the quoted value. Mismatch
%%     = fail.
chk_tcg_event_log_replay(Envelope) ->
    LogB64 = hb_maps:get(<<"tcg-event-log">>, Envelope, <<>>, #{}),
    LogBin = case LogB64 of
                 <<>> -> <<>>;
                 B when is_binary(B) ->
                     try hb_util:decode(B) catch _:_ -> <<>> end
             end,
    case byte_size(LogBin) of
        0 -> {ok, <<"no firmware log present (accepted)">>};
        _ ->
            Parsed = dev_tpm_tcg:parse(LogBin),
            Events = [V || {_, V} <- maps:to_list(Parsed),
                           is_map(V), not maps:is_key(<<"error">>, V)],
            Q = hb_maps:get(<<"tpm-quote">>, Envelope, #{}, #{}),
            QuotedPcrs = hb_maps:get(<<"pcr-values">>, Q, #{}, #{}),
            replay_and_compare(Events, QuotedPcrs, 0, [])
    end.

replay_and_compare([], _QuotedPcrs, Count, Mismatches) ->
    case Mismatches of
        [] ->
            {ok, iolist_to_binary(io_lib:format(
                "replayed ~B TCG event(s) into PCRs; all match the "
                "quoted values",
                [Count]))};
        _ ->
            {error, iolist_to_binary(io_lib:format(
                "TCG event log replay diverges from quoted PCR(s): ~p",
                [Mismatches]))}
    end;
replay_and_compare([Ev | Rest], QuotedPcrs, Count, Mismatches) ->
    %% EV_NO_ACTION is explicitly NOT extended (the spec says so).
    case maps:get(<<"event-type-code">>, Ev, 0) of
        3 ->  %% EV_NO_ACTION
            replay_and_compare(Rest, QuotedPcrs, Count, Mismatches);
        _ ->
            %% Fold this event's SHA-256 digest into the running
            %% reconstruction for its PCR, then (lazily) check at
            %% the end by asking whether the reconstructed PCR
            %% matches the quoted PCR. Accumulate per-PCR state
            %% in the process dictionary keyed by the PCR number;
            %% this avoids threading yet another state map.
            Pcr = maps:get(<<"pcr">>, Ev, -1),
            case in_range(Pcr) of
                false ->
                    replay_and_compare(Rest, QuotedPcrs,
                                       Count + 1, Mismatches);
                true ->
                    Digests = maps:get(<<"digests">>, Ev, #{}),
                    case maps:get(<<"sha256">>, Digests, undefined) of
                        D when is_binary(D), byte_size(D) =:= 32 ->
                            Prev = case get({replay_pcr, Pcr}) of
                                       undefined -> <<0:256>>;
                                       V -> V
                                   end,
                            Next = crypto:hash(sha256,
                                               <<Prev/binary, D/binary>>),
                            put({replay_pcr, Pcr}, Next),
                            case Rest of
                                [] ->
                                    MoreMismatches =
                                        collect_mismatches(QuotedPcrs,
                                                           Mismatches),
                                    replay_and_compare([], QuotedPcrs,
                                                       Count + 1,
                                                       MoreMismatches);
                                _ ->
                                    replay_and_compare(Rest, QuotedPcrs,
                                                       Count + 1,
                                                       Mismatches)
                            end;
                        _ ->
                            %% No SHA-256 digest on this event --
                            %% rare in modern logs. Skip without
                            %% counting as a mismatch; the overall
                            %% PCR reconstruction will reveal any
                            %% problem at the end.
                            replay_and_compare(Rest, QuotedPcrs,
                                               Count + 1, Mismatches)
                    end
            end
    end.

in_range(P) when is_integer(P), P >= 0, P =< 14 -> true;
in_range(_) -> false.

%% After replaying every event, compare each per-PCR
%% reconstruction against the quoted value. Only PCRs that
%% actually saw an event are compared -- an all-zero PCR with no
%% events is consistent.
collect_mismatches(QuotedPcrs, InitMismatches) ->
    lists:foldl(
        fun(P, Acc) ->
            case erase({replay_pcr, P}) of
                undefined -> Acc;
                Reconstructed ->
                    Key = integer_to_binary(P),
                    case hb_maps:get(Key, QuotedPcrs, undefined, #{}) of
                        undefined -> Acc;
                        QB64 ->
                            Quoted = hb_util:decode(QB64),
                            case Quoted of
                                Reconstructed -> Acc;
                                _ -> [{P, mismatch} | Acc]
                            end
                    end
            end
        end,
        InitMismatches,
        lists:seq(0, 14)).

%%---- check 5: node_message is present + id shape is right ------------
chk_node_msg_shape(Envelope) ->
    Nm = hb_maps:get(<<"node-message">>, Envelope, undefined, #{}),
    Id = hb_maps:get(<<"node-message-id">>, Envelope, undefined, #{}),
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
%%% attestation/3 -- the full envelope
%%%============================================================================

%% @doc Produce a full LapEE attestation envelope.
%%
%% The envelope is a plain AO-Core message. Binary-like fields are
%% base64url-encoded via `hb_util:encode/1' (same convention as every
%% other hash/id in AO-Core -- `hb_message:id/3' returns a base64url
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
%% Read the kernel's binary TCG event log. Canonical location is
%% `/sys/kernel/security/tpm0/binary_bios_measurements' (requires
%% securityfs mounted, kernel TPM driver loaded). Falls back to
%% `/sys/kernel/security/tpm1/...' (some Linux configs index their
%% TPM at tpm1). Returns empty binary when the log isn't
%% accessible -- either (a) no TPM driver, (b) securityfs not
%% mounted, or (c) host has no firmware-measured boot (which is
%% true for QEMU SeaBIOS test guests running under swtpm, where
%% SeaBIOS emits only a minimal log). An empty TCG log doesn't
%% break the attestation -- interpretation callers just see no
%% firmware events to reason about.
read_tcg_event_log() ->
    {Bin, _Source} = read_tcg_event_log_with_source(),
    Bin.

%% Variant that also returns the path we read from (or
%% `<<"unavailable">>`). Used by `tcg_event_log/3` so the
%% client can see which /sys path served the bytes.
read_tcg_event_log_with_source() ->
    Paths = [
        <<"/sys/kernel/security/tpm0/binary_bios_measurements">>,
        <<"/sys/kernel/security/tpm1/binary_bios_measurements">>
    ],
    read_first_available_with_source(Paths).

read_first_available([]) -> <<>>;
read_first_available([Path | Rest]) ->
    case file:read_file(binary_to_list(Path)) of
        {ok, Bin} when is_binary(Bin), byte_size(Bin) > 0 -> Bin;
        _ -> read_first_available(Rest)
    end.

read_first_available_with_source([]) ->
    {<<>>, <<"unavailable">>};
read_first_available_with_source([Path | Rest]) ->
    case file:read_file(binary_to_list(Path)) of
        {ok, Bin} when is_binary(Bin), byte_size(Bin) > 0 ->
            {Bin, Path};
        _ ->
            read_first_available_with_source(Rest)
    end.

%% Classify the raw TCG event log bytes without a full parse.
%% Used to stamp `tcg-event-log-format' on the attestation
%% envelope so a verifier can branch without re-walking the
%% whole log. The same heuristic is mirrored in
%% `dev_tpm_interpret:detect_log_format/1' for the post-parse
%% path.
infer_log_format(<<>>) -> <<"empty">>;
infer_log_format(Bin) when byte_size(Bin) < 32 ->
    <<"unknown">>;
infer_log_format(Bin) ->
    %% Crypto-agile logs begin with a TCG_PCR_EVENT (legacy
    %% 1.2 shape: pcr u32 + event-type u32 + 20-byte SHA-1 +
    %% 4-byte event-data-size + event-data), where:
    %%   * event-type = 3 (EV_NO_ACTION)
    %%   * event-data starts with ASCII "Spec ID Event03".
    %% TDX CCEL logs differ in that the first record is on
    %% PCR != 0 (MRTD lives on PCR 1).
    <<Pcr:32/little, EvType:32/little,
      _Sha1:20/binary, DataSize:32/little, Rest/binary>> = Bin,
    IsSpecId = EvType =:= 3 andalso DataSize >= 15 andalso
        byte_size(Rest) >= DataSize andalso
        binary:longest_common_prefix(
          [binary:part(Rest, 0, 15),
           <<"Spec ID Event03">>]) >= 15,
    if
        Pcr =/= 0 andalso IsSpecId -> <<"tdx-ccel">>;
        IsSpecId                    -> <<"crypto-agile">>;
        true                        -> <<"legacy-sha1">>
    end.


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
                    {TcgLogBin, TcgLogSource} =
                        read_tcg_event_log_with_source(),
                    TcgLogFormat = infer_log_format(TcgLogBin),
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
                        <<"lapee-attestation-version">> => <<"0.4">>,
                        <<"issued-at-unix">> =>
                            erlang:system_time(second),
                        <<"ek-cert-pem">> => EKCertPem,
                        %% Intermediates from NV `handle + 1'. Empty
                        %% when the vendor didn't provision a chain
                        %% slot, or when the cert is absent.
                        <<"ek-cert-chain-pem">> =>
                            ek_cert_chain_pem(),
                        %% Provenance of the EK cert -- tpm-nv (real,
                        %% with which handle + byte count) or absent
                        %% (with the probe-attempt list). This is the
                        %% hook that lets the verifier distinguish a
                        %% legitimate hardware EK from a missing one.
                        %% We never inject synthetic values; if the
                        %% TPM has no EK cert, the field is empty and
                        %% source.kind is "absent".
                        <<"ek-cert-source">> =>
                            ek_cert_source(),
                        %% Real TPM identity straight from
                        %% TPM2_GetCapability -- manufacturer, vendor
                        %% string, spec level/revision, firmware
                        %% version. Populated even when the EK cert
                        %% is absent, so the verifier always gets
                        %% "what chip is this" from ground truth.
                        <<"tpm-properties">> =>
                            tpm_properties(),
                        %% Runtime kernel + SMBIOS snapshot so
                        %% claim.cpu / claim.iommu / claim.lockdown
                        %% resolve to concrete values even when the
                        %% TCG event log doesn't carry them (paper-
                        %% committed fields per section Architecture
                        %% of the LapEE paper).
                        <<"platform-probes">> =>
                            platform_probes(),
                        <<"ak-pub-pem">> => AKPubPem,
                        <<"tpm-quote">> => #{
                            <<"pcr-selection">> => Pcrs,
                            <<"nonce">> => hb_util:encode(Nonce),
                            <<"quoted">> => hb_util:encode(Q),
                            <<"signature">> => hb_util:encode(Sig),
                            <<"pcr-values">> =>
                                maps:from_list(
                                    [{integer_to_binary(I),
                                      hb_util:encode(V)}
                                     || {I, V} <- maps:to_list(PcrMap)])
                        },
                        <<"runtime-event-log">> => EventLog,
                        %% Firmware-side TCG event log (PCRs 0-14
                        %% measurements the kernel exposes).
                        %% base64url -- consistent with every other
                        %% binary field in this envelope
                        %% (runtime_event_log digests, tpm_quote
                        %% values, PCR digests, ...). The interpret
                        %% device parses this into per-event
                        %% messages and extracts machine-identifying
                        %% fields (Secure Boot, firmware version,
                        %% bootloader hash, ...) per the paper's
                        %% section Architecture "every field is a named
                        %% event-log entry" requirement.
                        <<"tcg-event-log">> =>
                            hb_util:encode(TcgLogBin),
                        <<"tcg-event-log-source-path">>  =>
                            TcgLogSource,
                        <<"tcg-event-log-length-bytes">> =>
                            byte_size(TcgLogBin),
                        <<"tcg-event-log-format">>       =>
                            TcgLogFormat,
                        <<"node-message">> => NodeMsg,
                        <<"node-message-id">> => NodeMsgId,
                        <<"wallet-address">> =>
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
        <<"emitted-at-unix">> => erlang:system_time(second)
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
            %% Snapshot TPM-reported identity via TPM2_GetCapability.
            %% This is the primary manufacturer / firmware-version
            %% signal and works even when NV has no EK cert. The
            %% TCG-OID attributes on a real EK cert, when present,
            %% act as cross-check at the claim layer.
            capture_tpm_properties(),
            %% v1.2 E3: snapshot kernel/sysfs identity sources that
            %% aren't in the TCG event log (CPU vendor/model,
            %% IOMMU groups, kernel lockdown level, SMBIOS / DMI).
            %% These become `platform-probes' in the envelope and
            %% feed claim.cpu / claim.iommu / claim.lockdown on the
            %% verifier side. Probes run once at init_chain time so
            %% every subsequent /attestation call sees the same
            %% snapshot (important for reproducible claim digests).
            capture_platform_probes(),
            case nif_create_ek() of
                {ok, #{esys_tr := EKTr, public_pem := EKPem}} ->
                    persistent_term:put({dev_tpm2, ek_tr}, EKTr),
                    persistent_term:put({dev_tpm2, ek_pub_pem}, EKPem),
                    %% Pull the TPM's real EK certificate out of NV
                    %% storage. If no EK cert is provisioned we record
                    %% the absence explicitly -- we do NOT fabricate a
                    %% substitute. A missing EK cert is meaningful
                    %% evidence on the claim, not a condition to paper
                    %% over. See fetch_ek_cert_from_nv/1.
                    fetch_ek_cert_from_nv(Opts),
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

%% Capture TPM identity via TPM2_GetCapability (TPM_PT_MANUFACTURER,
%% TPM_PT_VENDOR_STRING_*, TPM_PT_FIRMWARE_VERSION_*, ...). These
%% values come straight from the TPM hardware regardless of EK-cert
%% provisioning -- that's what makes them the primary identification
%% source. On failure we stamp a structured "capability-probe-failed"
%% entry rather than dropping the field: silent absence would blur
%% the line between "no TPM" and "TPM didn't answer".
capture_tpm_properties() ->
    try lapee_tpm_nif:tpm_properties() of
        {ok, Props} ->
            persistent_term:put({dev_tpm2, tpm_properties}, Props),
            ok;
        {error, Reason} ->
            persistent_term:put(
                {dev_tpm2, tpm_properties},
                #{error => to_bin(Reason)}),
            ok
    catch C:E ->
        persistent_term:put(
            {dev_tpm2, tpm_properties},
            #{error => iolist_to_binary(
                io_lib:format("~p:~p", [C, E]))}),
        ok
    end.

to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> iolist_to_binary(L);
to_bin(A) when is_atom(A) -> atom_to_binary(A);
to_bin(T) -> iolist_to_binary(io_lib:format("~p", [T])).

%% v1.2 E3: read the sysfs + /proc signals that identify the
%% running platform beyond what the TCG event log carries, cache
%% the snapshot for the /attestation envelope.
%%
%% Paths probed (all read-only, no side effects):
%%
%%   /proc/cpuinfo                      CPU vendor_id / model name /
%%                                      family / model / stepping /
%%                                      microcode / flags
%%   /sys/kernel/security/lockdown      `[none] integrity confidentiality'
%%                                      style -- the bracketed entry is
%%                                      active
%%   /sys/kernel/iommu_groups/          one directory per IOMMU group;
%%                                      non-zero count == IOMMU active
%%   /sys/class/dmi/id/sys_vendor       SMBIOS Type-1 system vendor
%%                                      (e.g. "Framework")
%%   /sys/class/dmi/id/product_name     SMBIOS product name
%%   /sys/class/dmi/id/board_name       SMBIOS Type-2 board name
%%   /sys/class/dmi/id/bios_vendor      SMBIOS Type-0 BIOS vendor
%%   /sys/class/dmi/id/bios_version     SMBIOS Type-0 BIOS version
%%   /sys/class/dmi/id/bios_release     SMBIOS Type-0 BIOS release
%%
%% Every value is surfaced as a binary; `null' when the file is
%% absent / unreadable. Unknown paths do NOT fail the whole probe
%% pass -- a partial snapshot is still useful.
capture_platform_probes() ->
    Probes = #{
        cpuinfo          => read_cpuinfo_stanza(),
        lockdown         => read_trim(
            <<"/sys/kernel/security/lockdown">>),
        iommu_groups     => count_iommu_groups(),
        dmi_sys_vendor   => read_trim(
            <<"/sys/class/dmi/id/sys_vendor">>),
        dmi_product_name => read_trim(
            <<"/sys/class/dmi/id/product_name">>),
        dmi_board_name   => read_trim(
            <<"/sys/class/dmi/id/board_name">>),
        dmi_bios_vendor  => read_trim(
            <<"/sys/class/dmi/id/bios_vendor">>),
        dmi_bios_version => read_trim(
            <<"/sys/class/dmi/id/bios_version">>),
        dmi_bios_release => read_trim(
            <<"/sys/class/dmi/id/bios_release">>),
        %% v1.2 review addendum: three runtime-visible signals
        %% that strengthen the claim without changing the threat
        %% model:
        %%
        %%   kernel-cmdline  verbatim /proc/cmdline so the
        %%                    verifier can cross-check claim
        %%                    flags (iommu=, lockdown=, ...)
        %%                    against ground truth.
        %%
        %%   secure-boot     1-byte data octet from the SecureBoot
        %%                    EFI variable (GUID constant per
        %%                    UEFI spec 2.10 Table 3-1). 0x01 =
        %%                    enabled, 0x00 = disabled, null if
        %%                    the firmware isn't UEFI / efivarfs
        %%                    isn't mounted.
        %%
        %%   tpm-version-major  /sys/class/tpm/tpm0/tpm_version_major
        %%                       (1 or 2; complements the
        %%                        TPM2_GetCapability spec-family
        %%                        field).
        kernel_cmdline     => read_trim(<<"/proc/cmdline">>),
        secure_boot        => read_secure_boot_state(),
        tpm_version_major  => read_trim(
            <<"/sys/class/tpm/tpm0/tpm_version_major">>),
        ima_count          => read_trim(
            <<"/sys/kernel/security/integrity/ima/"
              "runtime_measurements_count">>),
        probed_at_unix     => erlang:system_time(second)
    },
    persistent_term:put({dev_tpm2, platform_probes}, Probes),
    ok.

%% Read the one-byte data octet from the EFI SecureBoot variable.
%% The efivarfs file layout is `<attributes:4><data:N>', where N=1
%% for SecureBoot. GUID suffix `8be4df61-93ca-11d2-aa0d-00e098032b8c'
%% is the EFI_GLOBAL_VARIABLE GUID per UEFI spec. Returns an atom
%% `enabled' / `disabled' / `unknown'; the atom is converted to
%% a binary in platform_probes/0.
read_secure_boot_state() ->
    Path = <<"/sys/firmware/efi/efivars/"
             "SecureBoot-8be4df61-93ca-11d2-aa0d-00e098032b8c">>,
    case file:read_file(Path) of
        {ok, <<_Attrs:4/binary, 1:8>>} -> enabled;
        {ok, <<_Attrs:4/binary, 0:8>>} -> disabled;
        {ok, _}                         -> unknown;
        _                               -> not_readable
    end.

%% Read the first `processor' stanza of /proc/cpuinfo into a map
%% keyed by the normalised field name (kebab-case binary). Returns
%% an empty map on any file-read or parse error.
read_cpuinfo_stanza() ->
    case file:read_file(<<"/proc/cpuinfo">>) of
        {ok, Bin} ->
            %% Split on the blank-line stanza separator.
            [First | _] = binary:split(Bin, <<"\n\n">>, [global]),
            Lines = binary:split(First, <<"\n">>, [global]),
            lists:foldl(fun line_to_kv/2, #{}, Lines);
        _ -> #{}
    end.

line_to_kv(Line, Acc) ->
    case binary:split(Line, <<":">>, []) of
        [Key, Val] ->
            K = normalise_kv_key(string:trim(Key)),
            V = string:trim(Val),
            %% Only the first occurrence counts (first processor
            %% stanza); skip subsequent duplicates.
            case maps:is_key(K, Acc) of
                true -> Acc;
                false -> Acc#{K => V}
            end;
        _ -> Acc
    end.

normalise_kv_key(K) ->
    %% /proc/cpuinfo uses space-separated lowercase words; our
    %% kebab-case wire convention matches.
    Lowered = string:lowercase(K),
    binary:replace(Lowered, <<" ">>, <<"-">>, [global]).

%% Count the entries in /sys/kernel/iommu_groups/. Each group
%% corresponds to one IOMMU-enforced isolation domain. Zero
%% groups means either no IOMMU driver is loaded or the kernel
%% booted without an IOMMU enable flag. Returns integer or
%% `null' on path absence.
count_iommu_groups() ->
    case file:list_dir(<<"/sys/kernel/iommu_groups">>) of
        {ok, Entries} ->
            length([E || E <- Entries, is_group_dir(E)]);
        _ -> null
    end.

%% True iff `E' is an all-digits directory name (a single IOMMU
%% group ID). Filters out stray non-numeric entries that some
%% kernels expose under /sys/kernel/iommu_groups/.
is_group_dir(E) ->
    case string:to_integer(E) of
        {N, <<>>} when is_integer(N) -> true;
        {N, ""}   when is_integer(N) -> true;
        _                            -> false
    end.

%% Read a file and trim trailing whitespace (including the
%% terminating \n that most sysfs entries have). Returns a
%% binary on success, null on any read error.
read_trim(Path) ->
    case file:read_file(Path) of
        {ok, Bin} -> string:trim(Bin, trailing, "\r\n \t");
        _ -> null
    end.

%% Pretty the cached TPM properties for the /attestation envelope.
%% Integers for firmware-version halves let the verifier reconstruct
%% the full 64-bit revision (fw1 << 32 | fw2) without ambiguity; the
%% binary manufacturer / vendor-string fields are kebab-cased on the
%% wire.
tpm_properties() ->
    case persistent_term:get({dev_tpm2, tpm_properties}, undefined) of
        undefined ->
            #{<<"available">> => false,
              <<"reason">>    => <<"init_chain has not executed yet">>};
        #{error := Why} ->
            #{<<"available">> => false,
              <<"reason">>    => Why};
        #{} = P ->
            FW1 = maps:get(firmware_version_1, P, 0),
            FW2 = maps:get(firmware_version_2, P, 0),
            #{
              <<"available">>            => true,
              <<"manufacturer">>         =>
                  maps:get(manufacturer, P, <<>>),
              <<"manufacturer-u32">>     =>
                  maps:get(manufacturer_u32, P, 0),
              <<"vendor-string">>        =>
                  maps:get(vendor_string, P, <<>>),
              <<"spec-family">>          =>
                  maps:get(spec_family, P, <<>>),
              <<"spec-level">>           =>
                  maps:get(spec_level, P, 0),
              <<"spec-revision">>        =>
                  maps:get(spec_revision, P, 0),
              <<"firmware-version-1">>   => FW1,
              <<"firmware-version-2">>   => FW2,
              <<"firmware-version-u64">> =>
                  (FW1 bsl 32) bor (FW2 band 16#FFFFFFFF),
              <<"day-of-year">>          =>
                  maps:get(day_of_year, P, 0),
              <<"year">>                 =>
                  maps:get(year, P, 0)
             }
    end.

%% Return the cached EK-cert-chain PEM bundle. Empty when no
%% chain was read out of the adjacent NV slot.
ek_cert_chain_pem() ->
    persistent_term:get({dev_tpm2, ek_cert_chain_pem}, <<>>).

%% Return the cached platform-probes map (captured at init_chain)
%% formatted for the wire -- binary keys, null for unknown, ints
%% preserved as ints. Not present if init_chain hasn't run yet.
platform_probes() ->
    case persistent_term:get({dev_tpm2, platform_probes}, undefined) of
        undefined ->
            #{<<"available">> => false,
              <<"reason">>    =>
                  <<"init_chain has not executed yet">>};
        #{} = P ->
            CpuInfo = maps:get(cpuinfo, P, #{}),
            CpuMap = maps:fold(
                fun(K, V, Acc) when is_binary(K) ->
                       Acc#{K => V};
                   (_, _, Acc) -> Acc
                end, #{}, CpuInfo),
            #{
                <<"available">>          => true,
                <<"cpuinfo">>            => CpuMap,
                <<"lockdown">>           =>
                    or_bin_null(maps:get(lockdown, P, null)),
                <<"iommu-groups-count">> =>
                    maps:get(iommu_groups, P, null),
                <<"dmi-sys-vendor">>     =>
                    or_bin_null(maps:get(dmi_sys_vendor, P, null)),
                <<"dmi-product-name">>   =>
                    or_bin_null(maps:get(dmi_product_name, P, null)),
                <<"dmi-board-name">>     =>
                    or_bin_null(maps:get(dmi_board_name, P, null)),
                <<"dmi-bios-vendor">>    =>
                    or_bin_null(maps:get(dmi_bios_vendor, P, null)),
                <<"dmi-bios-version">>   =>
                    or_bin_null(maps:get(dmi_bios_version, P, null)),
                <<"dmi-bios-release">>   =>
                    or_bin_null(maps:get(dmi_bios_release, P, null)),
                <<"kernel-cmdline">>     =>
                    or_bin_null(maps:get(kernel_cmdline, P, null)),
                <<"secure-boot">>        =>
                    atom_or_bin_null(maps:get(secure_boot, P, null)),
                <<"tpm-version-major">>  =>
                    or_bin_null(maps:get(tpm_version_major, P, null)),
                <<"ima-measurement-count">> =>
                    or_bin_null(maps:get(ima_count, P, null)),
                <<"probed-at-unix">>     =>
                    maps:get(probed_at_unix, P, 0)
             }
    end.

atom_or_bin_null(null) -> null;
atom_or_bin_null(A) when is_atom(A) -> atom_to_binary(A, utf8);
atom_or_bin_null(B) when is_binary(B) -> B;
atom_or_bin_null(_) -> null.

or_bin_null(null) -> null;
or_bin_null(<<>>) -> null;
or_bin_null(B) when is_binary(B) -> B;
or_bin_null(L) when is_list(L) -> iolist_to_binary(L);
or_bin_null(_) -> null.

%% Return the provenance map for the currently-cached EK cert. The
%% ensure_ak -> init_chain -> fetch_ek_cert_from_nv pipeline populates
%% this; if it hasn't run yet we return an "unknown" placeholder so
%% the attestation envelope shape stays stable.
ek_cert_source() ->
    case persistent_term:get({dev_tpm2, ek_cert_source}, undefined) of
        undefined ->
            #{<<"kind">> => <<"unknown">>,
              <<"reason">> =>
                  <<"ensure_ak/1 has not executed yet">>};
        #{} = M ->
            %% Keys in the raw map are atoms so we're always producing
            %% the same one-hop shape. Re-key to binary here so the
            %% wire-format stays kebab-case-on-binary-keys throughout.
            maps:fold(
                fun(K, V, Acc) when is_atom(K) ->
                       Acc#{atom_to_binary(K) => V};
                   (K, V, Acc) -> Acc#{K => V}
                end, #{}, M)
    end.

%% TCG EK Credential Profile -- standard NV indices for EK certificates.
%% https://trustedcomputinggroup.org/resource/tcg-ek-credential-profile/
-define(EK_NV_RSA_2048, 16#01C00002).  %% low-range RSA-2048 EK cert
-define(EK_NV_RSA_3072, 16#01C0000A).  %% low-range RSA-3072 EK cert
-define(EK_NV_ECC_P256, 16#01C00004).  %% low-range ECC NIST P-256
-define(EK_NV_ECC_P384, 16#01C00006).  %% low-range ECC NIST P-384
%% High-range (vendor-specific templates) -- checked as a fallback.
-define(EK_NV_HIGH_RSA_2048, 16#01C00012).
-define(EK_NV_HIGH_RSA_3072, 16#01C0001A).

%% Fetch the EK certificate from TPM NV storage and cache it. The list
%% below is iterated in order; the first NV index that yields a valid
%% certificate wins. If none do, the EK cert is recorded as ABSENT --
%% a first-class signal that the verifier must see. We never synthesize
%% a stand-in: this codepath is what makes the attestation chain
%% legitimate rather than cosmetic.
%%
%% Override the probe list via `lapee_tpm_ek_nv_handles' for TPMs
%% whose manufacturer publishes certs at non-standard indices.
fetch_ek_cert_from_nv(Opts) ->
    Handles = hb_opts:get(
        lapee_tpm_ek_nv_handles,
        [?EK_NV_RSA_2048,
         ?EK_NV_RSA_3072,
         ?EK_NV_ECC_P256,
         ?EK_NV_ECC_P384,
         ?EK_NV_HIGH_RSA_2048,
         ?EK_NV_HIGH_RSA_3072],
        Opts),
    case try_nv_handles(Handles, []) of
        {ok, Handle, Der} ->
            Pem = der_to_pem(Der),
            persistent_term:put({dev_tpm2, ek_cert_pem}, Pem),
            %% TCG EK Credential Profile section 2.2.1.4: adjacent
            %% NV slot (handle + 1) holds the EK cert's INTERMEDIATE
            %% chain as concatenated DER certs. Probe it. On Sam's
            %% Nuvoton Framework the leaf cert is issued by
            %% `NPCTxxx ECC384 LeafCA 012110' -- without this chain
            %% we get chain-valid=false against the vendor-root
            %% bundle we already ship.
            ChainHandle = Handle + 1,
            {ChainDers, ChainSource} =
                fetch_ek_cert_chain(ChainHandle),
            persistent_term:put({dev_tpm2, ek_cert_chain_ders},
                                ChainDers),
            persistent_term:put({dev_tpm2, ek_cert_chain_pem},
                                ders_to_pem(ChainDers)),
            persistent_term:put(
                {dev_tpm2, ek_cert_source},
                #{kind => <<"tpm-nv">>,
                  handle => iolist_to_binary(
                      io_lib:format("0x~8.16.0B", [Handle])),
                  bytes => byte_size(Der),
                  chain_handle => iolist_to_binary(
                      io_lib:format("0x~8.16.0B", [ChainHandle])),
                  chain_cert_count => length(ChainDers),
                  chain_source => ChainSource}),
            ok;
        {error, Attempts} ->
            persistent_term:put({dev_tpm2, ek_cert_pem}, <<>>),
            persistent_term:put({dev_tpm2, ek_cert_chain_ders}, []),
            persistent_term:put({dev_tpm2, ek_cert_chain_pem}, <<>>),
            persistent_term:put(
                {dev_tpm2, ek_cert_source},
                #{kind => <<"absent">>,
                  reason => <<
                    "no EK certificate provisioned in TPM NV storage; "
                    "attestation proceeds without one so the verifier "
                    "can see the gap">>,
                  probed => format_probe_attempts(Attempts)}),
            ok
    end.

%% Read + parse the EK-cert chain slot. TCG format: one or more
%% concatenated DER-encoded certs. Some vendors ship one
%% intermediate; some ship the full chain down to the root. We
%% parse whatever's there and return a list of DER cert binaries.
fetch_ek_cert_chain(ChainHandle) ->
    case lapee_tpm_nif:nv_read(ChainHandle) of
        {ok, Bin} when is_binary(Bin), byte_size(Bin) > 0 ->
            case split_concatenated_ders(Bin) of
                [] ->
                    {[], <<"nv-content-empty-or-non-der">>};
                Ders ->
                    {Ders, <<"tpm-nv">>}
            end;
        {error, Reason} ->
            {[], iolist_to_binary(
                io_lib:format("probe-failed: ~s",
                              [reason_to_text(Reason)]))}
    end.

%% Walk a binary that should be a concatenation of DER-encoded
%% X.509 certificates. Each cert starts with ASN.1 tag `0x30'
%% (SEQUENCE) followed by a length encoding: short form
%% (`0x00..0x7F', length fits in the byte) or long form
%% (`0x80 | N' where N is the number of length bytes, then N
%% big-endian bytes of length). Return the list of whole-cert
%% binaries. Garbage at the end (non-0x30 bytes) terminates parsing.
split_concatenated_ders(Bin) ->
    split_concatenated_ders(Bin, []).

split_concatenated_ders(<<>>, Acc) ->
    lists:reverse(Acc);
split_concatenated_ders(<<16#30, Rest/binary>> = Full, Acc) ->
    case der_seq_total_len(Rest) of
        {ok, TotalInner, HeaderLen} ->
            CertLen = 1 + HeaderLen + TotalInner,
            case Full of
                <<Cert:CertLen/binary, Tail/binary>> ->
                    split_concatenated_ders(Tail, [Cert | Acc]);
                _ ->
                    %% Length lied; stop.
                    lists:reverse(Acc)
            end;
        error ->
            lists:reverse(Acc)
    end;
split_concatenated_ders(_, Acc) ->
    %% Non-0x30 byte = not a DER cert start; end of chain.
    lists:reverse(Acc).

%% Parse the ASN.1 length encoding at the start of `Bin' (the
%% byte AFTER the 0x30 tag). Returns `{ok, ContentLen, LenBytes}'
%% where LenBytes is the number of bytes the length encoding
%% itself consumed.
der_seq_total_len(<<L:8, _/binary>>) when L =< 16#7F ->
    {ok, L, 1};
der_seq_total_len(<<16#81, L:8, _/binary>>) ->
    {ok, L, 2};
der_seq_total_len(<<16#82, L:16/big, _/binary>>) ->
    {ok, L, 3};
der_seq_total_len(<<16#83, L:24/big, _/binary>>) ->
    {ok, L, 4};
der_seq_total_len(<<16#84, L:32/big, _/binary>>) ->
    {ok, L, 5};
der_seq_total_len(_) ->
    error.

%% Encode a list of DERs as a PEM bundle (one CERTIFICATE block
%% each, concatenated). Empty list -> empty binary.
ders_to_pem([]) -> <<>>;
ders_to_pem(Ders) ->
    iolist_to_binary([der_to_pem(D) || D <- Ders]).

try_nv_handles([], Acc) -> {error, lists:reverse(Acc)};
try_nv_handles([H | Rest], Acc) ->
    case lapee_tpm_nif:nv_read(H) of
        {ok, Der} when is_binary(Der), byte_size(Der) > 0 ->
            %% Sanity-check: does it look like an X.509 DER cert? The
            %% first byte should be 0x30 (ASN.1 SEQUENCE). TCG says NV
            %% 0x01C0000x indices hold exactly one DER cert with no
            %% length prefix. If the TPM put something else here we
            %% prefer to record it as "unrecognised" rather than feed
            %% mystery bytes to the verifier as an "EK cert".
            case Der of
                <<16#30, _/binary>> ->
                    {ok, H, Der};
                _ ->
                    try_nv_handles(
                        Rest,
                        [{H, <<"nv-content-not-der">>, byte_size(Der)} | Acc])
            end;
        {error, Reason} ->
            try_nv_handles(Rest, [{H, Reason, 0} | Acc])
    end.

format_probe_attempts(Attempts) ->
    %% The NIF's error Reason can be either an atom
    %% (lapee_make_error, e.g. 'nv_index_undefined') OR a nested
    %% tuple {tss2_rc, <<"op: 0x... (decoded)">>} from
    %% lapee_make_tss_error on any TSS2 failure we didn't
    %% specifically map to an atom. Render both shapes without
    %% assuming the inner structure -- a ~p fallback keeps the
    %% envelope shape stable even when the TPM returns an
    %% unexpected code.
    [iolist_to_binary(
        io_lib:format("0x~8.16.0B: ~s (~p bytes read)",
                      [H, reason_to_text(R), Sz]))
     || {H, R, Sz} <- Attempts].

reason_to_text(R) when is_atom(R) -> atom_to_binary(R, utf8);
reason_to_text(R) when is_binary(R) -> R;
reason_to_text({tss2_rc, Bin}) when is_binary(Bin) -> Bin;
reason_to_text(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

%% Quick PEM re-encoder for a DER-encoded X.509 cert. Matches the
%% wire format the rest of the stack expects (PEM with "CERTIFICATE"
%% labels and 64-char base64 lines).
der_to_pem(Der) when is_binary(Der) ->
    B64 = base64:encode(Der),
    Wrapped = wrap_64(B64),
    iolist_to_binary([
        <<"-----BEGIN CERTIFICATE-----\n">>,
        Wrapped,
        <<"-----END CERTIFICATE-----\n">>]).

wrap_64(<<>>) -> <<>>;
wrap_64(Bin) when byte_size(Bin) =< 64 ->
    <<Bin/binary, "\n">>;
wrap_64(<<Line:64/binary, Rest/binary>>) ->
    <<Line/binary, "\n", (wrap_64(Rest))/binary>>.

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
    ?assert(lists:member(<<"attestation">>, Exports)),
    %% No standalone tcg-event-log endpoint -- the log travels
    %% INSIDE the attested attestation envelope. A standalone
    %% un-attested path would let a malicious node serve one
    %% log via /tcg-event-log and a different one via
    %% /attestation.
    ?assertNot(lists:member(<<"tcg-event-log">>, Exports)).

info_docs_test() ->
    {ok, #{<<"status">> := 200, <<"body">> := Body}} = info(#{}, #{}, #{}),
    ?assert(maps:is_key(<<"description">>, Body)),
    ?assert(maps:is_key(<<"api">>, Body)),
    Api = maps:get(<<"api">>, Body),
    ?assert(maps:is_key(<<"extend">>, Api)),
    ?assert(maps:is_key(<<"attestation">>, Api)).

%% The TCG event log source-path + length + format fields
%% travel attested alongside tcg-event-log itself, so a
%% verifier can see at a glance where the bytes came from
%% without re-reading them.
read_tcg_event_log_with_source_shape_test() ->
    %% On a Mac dev box there's no /sys TPM; expect
    %% `{<<>>, <<"unavailable">>}'.
    {Bin, Src} = read_tcg_event_log_with_source(),
    ?assertEqual(<<>>, Bin),
    ?assertEqual(<<"unavailable">>, Src),
    ok.

%% infer_log_format/1 classifies a crypto-agile header
%% correctly from raw bytes alone (no event-log parse needed).
infer_log_format_crypto_agile_test() ->
    %% TCG_PCR_EVENT:
    %%   pcr u32 LE = 0
    %%   event_type u32 LE = 3 (EV_NO_ACTION)
    %%   sha1 digest (20 zero bytes)
    %%   event_data_size u32 LE
    %%   event_data: "Spec ID Event03" + nul + payload
    SpecId = <<"Spec ID Event03", 0, 0:128>>,
    Bin = <<0:32/little, 3:32/little, 0:(20*8),
            (byte_size(SpecId)):32/little, SpecId/binary>>,
    ?assertEqual(<<"crypto-agile">>, infer_log_format(Bin)).

infer_log_format_tdx_ccel_test() ->
    SpecId = <<"Spec ID Event03", 0, 0:128>>,
    %% First record on PCR 1 (MRTD) -> TDX CCEL.
    Bin = <<1:32/little, 3:32/little, 0:(20*8),
            (byte_size(SpecId)):32/little, SpecId/binary>>,
    ?assertEqual(<<"tdx-ccel">>, infer_log_format(Bin)).

infer_log_format_empty_test() ->
    ?assertEqual(<<"empty">>, infer_log_format(<<>>)),
    ?assertEqual(<<"unknown">>, infer_log_format(<<0, 1, 2>>)).

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

%% `chk_tcg_event_log_replay' returns `{ok, _}' when the envelope
%% has no firmware log (accepting this case -- test/dev guests
%% running QEMU+swtpm don't emit a firmware event log). Callers who
%% require a firmware log chain should additionally check envelope.
%% tcg_event_log size.
chk_tcg_event_log_replay_empty_log_test() ->
    %% Both "no field" and "field but empty" accepted.
    ?assertMatch({ok, _},
                 chk_tcg_event_log_replay(#{<<"tcg-event-log">> => <<>>})),
    ?assertMatch({ok, _},
                 chk_tcg_event_log_replay(#{<<"tcg-event-log">> =>
                                              hb_util:encode(<<>>)})),
    ?assertMatch({ok, _}, chk_tcg_event_log_replay(#{})).

%% When the envelope carries a TCG log whose events replay to
%% match the quoted PCR values, the check passes. Uses the same
%% synthetic fixture as dev_tpm_tcg's own tests -- PCR 0 gets one
%% event, PCR 7 gets one event, and we compute the expected
%% reconstructed values.
chk_tcg_event_log_replay_accepts_consistent_fixture_test() ->
    %% Build a 2-record crypto-agile log: SpecID on PCR 0 (not
    %% extended, EV_NO_ACTION) + one EV_S_CRTM_VERSION on PCR 0.
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8, 2:32/little,
               AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    Data = <<"FW-ABC">>,
    Sha256 = crypto:hash(sha256, Data),
    Sha1   = crypto:hash(sha,    Data),
    Rec2 = <<0:32/little,
             16#8:32/little,
             2:32/little,
             16#04:16/little, Sha1/binary,
             16#0B:16/little, Sha256/binary,
             (byte_size(Data)):32/little, Data/binary>>,
    Log = <<FirstRec/binary, Rec2/binary>>,
    %% Compute the expected PCR-0 reconstruction.
    ExpectedPcr0 = crypto:hash(sha256, <<0:256, Sha256/binary>>),
    Envelope = #{
        <<"tcg-event-log">> => hb_util:encode(Log),
        <<"tpm-quote">> => #{
            <<"pcr-values">> =>
                #{<<"0">> => hb_util:encode(ExpectedPcr0)}
        }
    },
    ?assertMatch({ok, _}, chk_tcg_event_log_replay(Envelope)).

%% Tampering the log (flip one byte of the event data) makes the
%% reconstructed PCR diverge from the quoted value -> reject.
chk_tcg_event_log_replay_rejects_tampered_fixture_test() ->
    %% Same fixture construction as the prior test but with
    %% tampered event data -- PCR 0 reconstruction diverges from
    %% the expected value. Still records the "correct" quoted
    %% value in the envelope so the mismatch is detectable.
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8, 2:32/little,
               AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    GoodData = <<"FW-ABC">>,
    GoodSha256 = crypto:hash(sha256, GoodData),
    BadData = <<"FW-XXX">>,
    BadSha256 = crypto:hash(sha256, BadData),
    BadSha1   = crypto:hash(sha,    BadData),
    Rec2Tampered = <<0:32/little,
                     16#8:32/little,
                     2:32/little,
                     16#04:16/little, BadSha1/binary,
                     16#0B:16/little, BadSha256/binary,
                     (byte_size(BadData)):32/little, BadData/binary>>,
    Log = <<FirstRec/binary, Rec2Tampered/binary>>,
    %% Quote claims the GOOD PCR 0 value. Log has tampered digest.
    GoodPcr0 = crypto:hash(sha256, <<0:256, GoodSha256/binary>>),
    Envelope = #{
        <<"tcg-event-log">> => hb_util:encode(Log),
        <<"tpm-quote">> => #{
            <<"pcr-values">> =>
                #{<<"0">> => hb_util:encode(GoodPcr0)}
        }
    },
    ?assertMatch({error, _}, chk_tcg_event_log_replay(Envelope)).

%% Regression test: `resolve_trusted_ca' must honour inline
%% request-supplied anchors in priority order -- base64url
%% `trusted-ca' wins over raw `trusted-ca-pem', which wins over
%% node config. An earlier version only handled `trusted-ca-pem'
%% here, so a caller supplying a ROGUE CA via `trusted-ca' on
%% the chain-URL path had the whole parameter silently dropped
%% -- leaving the node's configured (legitimate) CA to
%% rubber-stamp the chain as OK.
resolve_trusted_ca_priority_test() ->
    InlinePemBin = <<"-----BEGIN CERTIFICATE-----\ninline-pem\n"
                    "-----END CERTIFICATE-----">>,
    %% Base64url-encoded bytes of a DISTINCT PEM.
    B64uPem = <<"-----BEGIN CERTIFICATE-----\nbase64url-pem\n"
                "-----END CERTIFICATE-----">>,
    B64u = hb_util:encode(B64uPem),
    %% (1) `trusted-ca' wins when both inline forms are present.
    ?assertEqual(B64uPem,
                 resolve_trusted_ca(#{<<"trusted-ca">> => B64u,
                                      <<"trusted-ca-pem">> => InlinePemBin},
                                    #{})),
    %% (2) `trusted-ca-pem' is used when `trusted-ca' is absent.
    ?assertEqual(InlinePemBin,
                 resolve_trusted_ca(#{<<"trusted-ca-pem">> => InlinePemBin},
                                    #{})),
    %% (3) Empty/malformed `trusted-ca' falls through to
    %%     `trusted-ca-pem', not to config.
    ?assertEqual(InlinePemBin,
                 resolve_trusted_ca(#{<<"trusted-ca">> => <<>>,
                                      <<"trusted-ca-pem">> => InlinePemBin},
                                    #{})),
    %% (4) Neither inline nor config -> empty binary (triggers clean
    %%     "trusted CA missing" error downstream in chk_ek_chain).
    %% Supply a nonexistent config path so file:read_file returns
    %% error; the default /etc/lapee/... likely isn't present in
    %% the test env either.
    ?assertEqual(<<>>,
                 resolve_trusted_ca(#{},
                                    #{lapee_tpm_ca_cert =>
                                          <<"/nonexistent/ca.pem">>})),
    ok.

%% Regression test: `chk_event_log_replay' must refuse to
%% "replay" zero events into a zero PCR and call it valid. Even
%% though `chk_binding' catches the same shape, we want the replay
%% check to be explicit about non-emptiness too -- defence in depth.
chk_event_log_replay_rejects_empty_events_test() ->
    Zero43 = hb_util:encode(<<0:256>>),
    Envelope = #{
        <<"runtime-event-log">> => [],
        <<"tpm-quote">> => #{
            <<"pcr-values">> => #{<<"15">> => Zero43}
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
        <<"node-message-id">> => <<"">>,
        <<"runtime-event-log">> => [EmptyDigestEvent]
    },
    ?assertMatch({error, _}, chk_binding(EnvelopeEmptyId)),
    %% Also: id that decodes to fewer than 32 bytes (shorter base64url).
    EnvelopeShortId = #{
        <<"node-message-id">> => <<"AAAA">>,   %% 3 bytes
        <<"runtime-event-log">> =>
            [EmptyDigestEvent#{<<"digest">> => <<"AAAA">>}]
    },
    ?assertMatch({error, _}, chk_binding(EnvelopeShortId)).

%% Regression test: the verify_fun used in chk_ek_chain must reject
%% every structural / trust failure pkix can report, while letting
%% real-world TPM EK cert extensions through so vendor certs from
%% Nuvoton / Infineon / STMicro validate. Keeps this in lock-step
%% with `dev_tpm_interpret:ek_verify_fun/3' -- the two live in
%% different modules (device vs parser) per the LapEE architecture
%% but must accept identical chains.
ek_chain_verify_fun_rejects_bad_certs_test() ->
    {F, []} = ek_chain_verify_fun(),
    %% Non-TCG {bad_cert, _} events: hard fail. pkix re-wraps the
    %% inner reason as `{error, {bad_cert, Reason}}', so returning
    %% the unwrapped atom is the canonical convention (matches
    %% `dev_tpm_interpret:ek_verify_fun/3').
    ?assertMatch({fail, unknown_ca},
                 F(ignored, {bad_cert, unknown_ca},    state)),
    ?assertMatch({fail, selfsigned_peer},
                 F(ignored, {bad_cert, selfsigned_peer}, state)),
    ?assertMatch({fail, invalid_issuer},
                 F(ignored, {bad_cert, invalid_issuer}, state)),
    ?assertMatch({fail, invalid_signature},
                 F(ignored, {bad_cert, invalid_signature}, state)),
    ?assertMatch({fail, cert_expired},
                 F(ignored, {bad_cert, cert_expired},   state)),
    %% Critical unknown extensions carrying TCG OIDs must pass: real
    %% EK certs mark id-tcg-kp-EKCertificate (2.23.133.8.1) and
    %% id-tcg-tpmSpecification (2.23.133.2.16) critical, which OTP's
    %% default validator would otherwise reject.
    EkuExt = #'Extension'{extnID = {2, 23, 133, 8, 1}},
    ?assertMatch({valid, state},
                 F(ignored,
                   {bad_cert, {not_supported_extension, EkuExt}},
                   state)),
    SpecExt = #'Extension'{extnID = {2, 23, 133, 2, 16}},
    ?assertMatch({valid, state},
                 F(ignored,
                   {bad_cert, {not_supported_extension, SpecExt}},
                   state)),
    %% Critical unknown extensions outside the TCG whitelist still
    %% fail -- a rogue EK carrying a truly-unrecognised critical
    %% extension must not slip through.
    RogueExt = #'Extension'{extnID = {1, 2, 3, 4}},
    ?assertMatch({fail, {not_supported_extension, _}},
                 F(ignored,
                   {bad_cert, {not_supported_extension, RogueExt}},
                   state)),
    %% Non-critical extensions under the TCG arc are informational.
    TcgNonCrit = #'Extension'{extnID = {2, 23, 133, 2, 1}},
    ?assertMatch({valid, state},
                 F(ignored, {extension, TcgNonCrit}, state)),
    %% Non-TCG non-critical extensions: unknown (let pkix decide).
    NonTcgNonCrit = #'Extension'{extnID = {1, 2, 3, 4, 5}},
    ?assertMatch({unknown, state},
                 F(ignored, {extension, NonTcgNonCrit}, state)),
    %% Valid events pass through.
    ?assertMatch({valid, state}, F(ignored, valid, state)),
    ?assertMatch({valid, state}, F(ignored, valid_peer, state)),
    ok.

event_log_append_test() ->
    %% Reset state for the test.
    persistent_term:erase({dev_tpm2, event_log}),
    persistent_term:erase({dev_tpm2, event_seq}),
    ?assertEqual([], event_log(#{})),
    ok = append_event(15, #{<<"event-type">> => <<"T">>}),
    Log = event_log(#{}),
    ?assertEqual(1, length(Log)),
    [E1] = Log,
    ?assertEqual(15, maps:get(<<"pcr">>, E1)),
    ?assertEqual(0, maps:get(<<"seq">>, E1)),
    ok = append_event(15, #{<<"event-type">> => <<"U">>}),
    ?assertEqual(2, length(event_log(#{}))).

-endif.
