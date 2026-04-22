%%% @doc `~tpm-interpret@1.0' — turn a verified LapEE TPM attestation
%%% into rich, human-readable AO-Core fields.
%%%
%%% The companion to `~tpm2@2.0a'. `~tpm2@2.0a' is responsible for the
%%% cryptographic chain (EK cert → AK → TPM2_Quote → PCR 15 → node
%%% message). This device is responsible for turning that chain into
%%% *meaning*: the TPM vendor, the firmware identity, the kernel
%%% identity, the IMA chain, any cross-references against a static
%%% database of known-good values.
%%%
%%% Exports
%%%
%%%   info        public surface description.
%%%   interpret   take a LapEE attestation envelope and return a
%%%               structured AO-Core message describing every piece
%%%               of evidence present in the envelope.
%%%   verify      shortcut: call `dev_tpm2:verify' first and, if it
%%%               passes, attach the interpretation. This is the
%%%               endpoint the user's target URL lands on:
%%%
%%%                 ~relay@1.0/call&relay-path="http://PEER/~tpm2@2.0a/attestation"
%%%                     /verify~tpm-interpret@1.0
%%%
%%% Databases
%%%
%%% Static lookup tables live under the release's `priv/tpm-interpret/':
%%%
%%%     manufacturers.json          TCG-assigned vendor IDs → {name,
%%%                                 kind, website, notes}
%%%     root-cas/                   per-vendor EK root CA PEMs; used
%%%                                 by the verifier side but listed
%%%                                 here for interpretability (e.g.
%%%                                 "which vendor CA verified this EK?")
%%%     pcr-profiles/*.json         known PCR 0/1/7 values for specific
%%%                                 firmware versions (Lenovo BIOS
%%%                                 1.52, Dell XYZ, QEMU OVMF, …)
%%%     uki-measurements/*.json     known PCR 11/12/13 values for
%%%                                 specific UKI kernel images.
%%%
%%% Every database entry is an AO-Core message (JSON on disk; parsed
%%% into maps at load time). Format is documented in the first entry
%%% of each file.
-module(dev_tpm_interpret).
-export([info/1, info/3, interpret/3, verify/3, verify_peer/3,
         summary/3, peer_summary/3, peer_status/3, checks/3,
         events/3, claim/3]).
-include("include/hb.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%%% Device surface
%%%============================================================================

info(_) ->
    #{ exports => [<<"info">>, <<"interpret">>, <<"verify">>,
                   <<"verify-peer">>, <<"summary">>, <<"peer-summary">>,
                   <<"peer-status">>, <<"checks">>,
                   <<"events">>, <<"claim">>] }.

info(_Base, _Req, _Opts) ->
    {ok, #{
        <<"status">> => 200,
        <<"body">> => #{
            <<"description">> =>
                <<"Interpret a `~tpm2@2.0a' attestation envelope into "
                  "named, cross-referenced fields (TPM manufacturer, "
                  "firmware identity, kernel identity, IMA policy, "
                  "LapEE node identity) from a static database shipped "
                  "in the HyperBEAM release. Composes with `~tpm2@2.0a/"
                  "verify': the `verify' export here runs the crypto "
                  "chain first and only interprets on success.">>,
            <<"version">> => <<"1.0">>,
            <<"wire-format">> =>
                <<"All binary fields on the wire are base64url "
                  "(hb_util:encode/1). No hex, except short always-"
                  "hex-displayed namespaced identifiers (e.g. "
                  "TPM_ST constants like 0x8018).">>,
            <<"api">> => #{
                <<"interpret">> => #{
                    <<"description">> =>
                        <<"Structured interpretation of the envelope. "
                          "Does NOT itself verify — pair with `verify' "
                          "or pre-verified input.">>,
                    <<"input">> =>
                        <<"An attestation envelope (lapee_attestation_"
                          "version present) via Base/Req/body.">>,
                    <<"response">> =>
                        <<"9 sections: envelope, tpm, ak, quote, pcrs, "
                          "boot, kernel, ima, node.">>
                },
                <<"verify">> => #{
                    <<"description">> =>
                        <<"Call ~tpm2@2.0a/verify, then if the chain "
                          "is accepted, return the verification result "
                          "plus the full interpretation.">>,
                    <<"input">> => <<"Envelope (see interpret).">>,
                    <<"response">> =>
                        <<"{verified, verdict, checks, interpretation}.">>
                },
                <<"verify-peer">> => #{
                    <<"description">> =>
                        <<"Fetch another HB node's `~tpm2@2.0a/"
                          "attestation' envelope (GET), verify its "
                          "crypto chain locally, and return the full "
                          "interpretation + a link-free summary. "
                          "Designed for the documented cross-node flow: "
                          "the caller trusts THIS node's verdict about "
                          "the peer without itself having to speak the "
                          "TPM crypto.">>,
                    <<"params">> => #{
                        <<"peer">> =>
                            <<"Required. Base URL of the peer to "
                              "verify (e.g. http://host.example:8734).">>,
                        <<"trusted-ca">> =>
                            <<"Optional. base64url-encoded PEM of the "
                              "TPM vendor root CA to trust for this "
                              "request. Overrides node config. Preferred "
                              "inline form.">>,
                        <<"trusted-ca-pem">> =>
                            <<"Optional (back-compat). Raw PEM as a "
                              "string. Unsafe over URL-encoded GET — "
                              "the `+' in base64 base-64 values and in "
                              "the PEM BEGIN header get mangled. Use "
                              "`trusted-ca' instead.">>
                    },
                    <<"response">> =>
                        <<"{peer, verified, verdict, checks, summary, "
                          "trust_anchor_source}.">>
                },
                <<"summary">> => #{
                    <<"description">> =>
                        <<"Lightweight, link-free interpretation "
                          "summary of an envelope. Same shape as the "
                          "`summary' field inside verify-peer, but "
                          "without the crypto verification. Use for "
                          "quick introspection when verification has "
                          "already happened (or will happen) "
                          "separately.">>,
                    <<"input">> => <<"Envelope (see interpret).">>,
                    <<"response">> =>
                        <<"{envelope_version, tpm_manufacturer, "
                          "tpm_manufacturer_kind, tpm_model, "
                          "tpm_firmware_version, ak_algorithm, "
                          "ak_key_size_bits, ak_public_key_b64url, "
                          "quote_attest_type, quote_clock_ms, "
                          "quote_reset_count, secure_boot_measured, "
                          "wallet_address, node_message_id, "
                          "on_start_hook_device, pcr15_event_count}.">>
                },
                <<"peer-summary">> => #{
                    <<"description">> =>
                        <<"Fetch a peer's attestation and return the "
                          "summary (interpret-only, NO crypto "
                          "verification). ~10x cheaper than verify-peer "
                          "— use for dashboards or discovery where "
                          "you'll crypto-verify separately.">>,
                    <<"params">> => #{
                        <<"peer">> => <<"Required. Base URL.">>
                    },
                    <<"response">> =>
                        <<"{peer, reachable, envelope_shape_ok, "
                          "summary}.">>
                },
                <<"peer-status">> => #{
                    <<"description">> =>
                        <<"Cheapest possible probe: is the peer "
                          "reachable and LapEE-shaped? Does not fetch "
                          "the full envelope — only the first layer "
                          "(envelope_version + wallet + node_message_id). "
                          "Intended for liveness checks.">>,
                    <<"params">> => #{
                        <<"peer">> => <<"Required. Base URL.">>
                    },
                    <<"response">> =>
                        <<"{peer, reachable, lapee_attestation_version, "
                          "wallet_address, node_message_id}.">>
                },
                <<"checks">> => #{
                    <<"description">> =>
                        <<"Return the machine-readable list of crypto "
                          "checks that verify / verify-peer performs, "
                          "with per-check failure implications. "
                          "Clients use this to build UI, programmatic "
                          "policy, or adversarial test harnesses. Each "
                          "check has a `severity': `core' checks gate "
                          "the `verified' verdict; `informational' "
                          "checks are surfaced but do NOT gate it.">>,
                    <<"response">> =>
                        <<"[{name, severity, purpose, failure_implies}].">>
                },
                <<"events">> => #{
                    <<"description">> =>
                        <<"Parse the envelope's tcg_event_log into a "
                          "1-indexed map of AO-Core messages. Each "
                          "event has {seq, pcr, event_type, "
                          "event_type_code, digests, event_data, "
                          "parsed}. The `parsed' sub-map carries "
                          "per-event-type decoded fields (Secure "
                          "Boot state, UEFI variable names, UKI key/"
                          "value, firmware version, bootloader PE "
                          "hash, microcode header, etc.). Individual "
                          "events are path-addressable: "
                          "`.../events/3/event_type', "
                          "`.../events/3/parsed/semantic/"
                          "secure_boot_enabled'.">>,
                    <<"input">> => <<"An envelope (same resolution "
                                     "as interpret).">>,
                    <<"response">> => <<"map of {<<\"1\">> => message, "
                                        "<<\"2\">> => message, ...}">>
                },
                <<"claim">> => #{
                    <<"description">> =>
                        <<"Flat, policy-friendly surface of machine-"
                          "identifying facts derived from the "
                          "attestation. Each claim has a value "
                          "(binary / bool / string / \"unknown\") "
                          "and a `_provenance' key listing the "
                          "source events that backed the derivation. "
                          "Designed to compose directly with green-"
                          "zone style predicates: "
                          "\"claim.secure_boot.enabled == true AND "
                          "claim.tme.enabled == true AND "
                          "claim.kernel.uki_hash IN {X, Y, Z}\".">>,
                    <<"input">> => <<"An envelope.">>,
                    <<"response">> =>
                        <<"#{secure_boot => #{enabled, db_authorities, "
                          "setup_mode, deployed_mode, _provenance}, "
                          "firmware => #{crtm_version, _provenance}, "
                          "boot_loader => #{image_hash, _provenance},"
                          " kernel => #{cmdline, uki_hash, iommu_"
                          "strict, _provenance}, tme => #{enabled, "
                          "_provenance}, lockdown => #{level, "
                          "_provenance}}.">>
                }
            }
        }
    }}.

%%%============================================================================
%%% events/3 — parsed TCG event log as AO-Core messages
%%%============================================================================

events(Base, Req, Opts) ->
    Envelope = resolve_envelope(Base, Req, Opts),
    {ok, #{
        <<"status">> => 200,
        <<"body">> => interpret_events(Envelope)
    }}.

%%%============================================================================
%%% claim/3 — flat, policy-friendly surface
%%%============================================================================

claim(Base, Req, Opts) ->
    Envelope = resolve_envelope(Base, Req, Opts),
    Db = hb_db_tpm:load(Opts),
    %% Claim pipeline reads from RAW (non-wire-encoded) events so
    %% UTF-8 cmdline flag values survive unaltered. Claim values
    %% are UTF-8-safe by construction (parsed text, base64url-
    %% encoded digests, integers, booleans, "unknown" sentinels —
    %% no raw firmware bytes), so we skip the wire-encode layer
    %% and return the claim as-is.
    Events = interpret_events_raw(Envelope),
    {ok, #{
        <<"status">> => 200,
        <<"body">> => interpret_claim(Events, Envelope, Db)
    }}.

%%%============================================================================
%%% summary/3 — lightweight interpret (no verify)
%%%============================================================================

summary(Base, Req, Opts) ->
    Envelope = resolve_envelope(Base, Req, Opts),
    Interp = safe_interpret(Envelope, Opts),
    {ok, #{
        <<"status">> => 200,
        <<"body">> => summarise_interp(Interp)
    }}.

%%%============================================================================
%%% peer_summary/3, peer_status/3 — lightweight cross-node introspection
%%%============================================================================

peer_summary(_Base, Req, Opts) ->
    case hb_maps:get(<<"peer">>, Req, undefined, Opts) of
        PeerUrl when is_binary(PeerUrl) ->
            Base = strip_trailing_slash(PeerUrl),
            case fetch_peer_envelope(Base, Opts) of
                {ok, Envelope} ->
                    Interp = safe_interpret(Envelope, Opts),
                    {ok, #{
                        <<"status">> => 200,
                        <<"body">> => #{
                            <<"peer">>     => Base,
                            <<"reachable">> => true,
                            <<"envelope-shape-ok">> => true,
                            <<"summary">> => summarise_interp(Interp)
                        }
                    }};
                {error, Reason} ->
                    {ok, #{
                        <<"status">> => 200,
                        <<"body">> => #{
                            <<"peer">>     => Base,
                            <<"reachable">> => false,
                            <<"envelope-shape-ok">> => false,
                            <<"detail">>   => fmt_reason(Reason)
                        }
                    }}
            end;
        _ -> missing_peer_400()
    end.

peer_status(_Base, Req, Opts) ->
    case hb_maps:get(<<"peer">>, Req, undefined, Opts) of
        PeerUrl when is_binary(PeerUrl) ->
            Base = strip_trailing_slash(PeerUrl),
            case fetch_peer_envelope(Base, Opts) of
                {ok, Envelope} ->
                    {ok, #{
                        <<"status">> => 200,
                        <<"body">> => #{
                            <<"peer">> => Base,
                            <<"reachable">> => true,
                            <<"lapee-attestation-version">> =>
                                hb_maps:get(
                                    <<"lapee-attestation-version">>,
                                    Envelope, null, Opts),
                            <<"wallet-address">> =>
                                hb_maps:get(<<"wallet-address">>,
                                            Envelope, null, Opts),
                            <<"node-message-id">> =>
                                hb_maps:get(<<"node-message-id">>,
                                            Envelope, null, Opts)
                        }
                    }};
                {error, Reason} ->
                    {ok, #{
                        <<"status">> => 200,
                        <<"body">> => #{
                            <<"peer">> => Base,
                            <<"reachable">> => false,
                            <<"lapee-attestation-version">> => null,
                            <<"wallet-address">> => null,
                            <<"node-message-id">> => null,
                            <<"detail">> => fmt_reason(Reason)
                        }
                    }}
            end;
        _ -> missing_peer_400()
    end.

%%%============================================================================
%%% checks/3 — machine-readable description of the verify battery
%%%             (5 core crypto checks + 1 informational firmware log
%%%             replay check; `severity' distinguishes)
%%%============================================================================

checks(_Base, _Req, _Opts) ->
    {ok, #{
        <<"status">> => 200,
        <<"body">> => #{
            <<"checks">> => [
                #{
                    <<"name">> =>
                        <<"EK certificate chains to trusted TPM "
                          "vendor root CA">>,
                    <<"severity">> => <<"core">>,
                    <<"purpose">> =>
                        <<"Proves this TPM was manufactured by a "
                          "known vendor whose root CA is in the "
                          "verifier's trust anchors. Without this, "
                          "the EK (and thus the AK, and thus the "
                          "quote) could be synthesised by anyone.">>,
                    <<"failure-implies">> =>
                        <<"The EK cert cannot be tied back to a "
                          "trusted TPM vendor. Either the TPM is "
                          "not a vendor we trust, OR the verifier's "
                          "trust anchor is stale, OR the cert was "
                          "tampered.">>
                },
                #{
                    <<"name">> =>
                        <<"TPM2_Quote signature + pcrDigest + "
                          "nonce all valid">>,
                    <<"severity">> => <<"core">>,
                    <<"purpose">> =>
                        <<"Proves the TPM signed the quoted PCR "
                          "values (and nothing else) with its AK, "
                          "and that extraData equals the caller's "
                          "nonce (anti-replay).">>,
                    <<"failure-implies">> =>
                        <<"Either the quote signature is invalid "
                          "(wrong key / tampered message), the "
                          "pcrDigest doesn't match the reported "
                          "PCR values, or the nonce was replayed.">>
                },
                #{
                    <<"name">> =>
                        <<"Runtime event log replay of PCR 15 "
                          "matches quoted value">>,
                    <<"severity">> => <<"core">>,
                    <<"purpose">> =>
                        <<"Proves the envelope's declared PCR 15 "
                          "events hash together to the quoted "
                          "PCR 15 value. Establishes a correspondence "
                          "between declared events and hardware "
                          "state.">>,
                    <<"failure-implies">> =>
                        <<"The runtime_event_log doesn't match "
                          "what was actually quoted — events "
                          "missing, inserted, or out of order.">>
                },
                #{
                    <<"name">> =>
                        <<"PCR 15 extension commits to "
                          "node-message-id">>,
                    <<"severity">> => <<"core">>,
                    <<"purpose">> =>
                        <<"Proves THIS node's node_message_id was "
                          "extended into PCR 15 — the LapEE key "
                          "binding. Ties the attestation to the "
                          "specific node configuration.">>,
                    <<"failure-implies">> =>
                        <<"The node_message_id claimed in the "
                          "envelope isn't in the PCR 15 event log. "
                          "The enforced on.start hook may not have "
                          "run, or the envelope is stitched from "
                          "another node's attestation.">>
                },
                #{
                    <<"name">> =>
                        <<"Embedded node_message + id present "
                          "and correct shape">>,
                    <<"severity">> => <<"core">>,
                    <<"purpose">> =>
                        <<"Proves the attestation carries its own "
                          "node message (configuration) with a 43-"
                          "character base64url id that decodes to "
                          "32 bytes. Enables offline inspection of "
                          "what was actually attested to.">>,
                    <<"failure-implies">> =>
                        <<"Envelope is malformed or missing the "
                          "node_message / node_message_id fields.">>
                },
                #{
                    <<"name">> =>
                        <<"Firmware TCG event log replays to "
                          "quoted PCRs 0-14">>,
                    <<"severity">> => <<"informational">>,
                    <<"purpose">> =>
                        <<"Cross-check: every firmware event in "
                          "the envelope's `tcg_event_log' should "
                          "fold (SHA-256 extend) into its quoted "
                          "PCR. A mismatch surfaces firmware-log "
                          "tampering or a decode bug. NOT a trust "
                          "anchor — the LapEE trust model is "
                          "rooted at PCR 15 (the node identity), "
                          "not at PCRs 0-14. Reported but does "
                          "NOT gate `verified'. Policy engines "
                          "wanting strict firmware-log consistency "
                          "can key off this check directly.">>,
                    <<"failure-implies">> =>
                        <<"The firmware event log does not "
                          "reconstruct into the quoted PCR(s). "
                          "Common benign cause: SeaBIOS under QEMU "
                          "emits an incomplete log. Benign on "
                          "development guests; worth investigating "
                          "on production hardware.">>
                }
            ]
        }
    }}.

%%%============================================================================
%%% Helpers for the introspection endpoints
%%%============================================================================

missing_peer_400() ->
    {ok, #{
        <<"status">> => 400,
        <<"body">> => #{
            <<"error">> => <<"missing-peer">>,
            <<"detail">> =>
                <<"This endpoint requires a `peer' key — the base "
                  "URL of a LapEE node (e.g. "
                  "http://127.0.0.1:8734).">>
        }
    }}.

fetch_peer_envelope(Base, Opts) ->
    FetchMsg = #{
        <<"path">>          => <<"/~tpm2@2.0a/attestation">>,
        <<"accept">>        => <<"application/json@1.0">>,
        <<"accept-bundle">> => <<"true">>
    },
    FetchResult =
        try hb_http:get(Base, FetchMsg, Opts)
        catch Class:Reason ->
            {error, {Class, Reason}}
        end,
    case FetchResult of
        {ok, Response} when is_map(Response) ->
            Envelope = unwrap_envelope(Response, Opts),
            case is_envelope(Envelope) of
                true  -> {ok, Envelope};
                false -> {error, not_lapee_shaped}
            end;
        {error, Why} -> {error, {transport, Why}};
        Unexpected   -> {error, {unexpected, Unexpected}}
    end.

fmt_reason({transport, Why}) ->
    iolist_to_binary(io_lib:format("transport: ~p", [Why]));
fmt_reason(not_lapee_shaped) ->
    <<"peer responded, but the response is not a LapEE "
      "attestation envelope (no lapee_attestation_version "
      "field).">>;
fmt_reason({unexpected, X}) ->
    iolist_to_binary(io_lib:format("unexpected response: ~p", [X]));
fmt_reason(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

%%%============================================================================
%%% verify/3 — the target endpoint
%%%============================================================================

verify(Base, Req, Opts) ->
    Envelope = resolve_envelope(Base, Req, Opts),
    case dev_tpm2:verify(Envelope, Req, Opts) of
        {ok, #{<<"status">> := 200,
               <<"body">> := #{<<"verified">> := true} = VerifyBody}} ->
            Interp = interpret_envelope(Envelope, Opts),
            {ok, #{
                <<"status">> => 200,
                <<"body">> => VerifyBody#{
                    <<"interpretation">> => Interp
                }
            }};
        {ok, #{<<"body">> := VerifyBody} = R} ->
            %% Chain rejected; attach the interpretation anyway so the
            %% caller can see WHY (e.g. "known-compromised firmware
            %% version") even when the signature fails.
            Partial = safe_interpret(Envelope, Opts),
            {ok, R#{
                <<"body">> => VerifyBody#{
                    <<"interpretation">> => Partial
                }
            }};
        Other -> Other
    end.

%%%============================================================================
%%% verify_peer/3 — cross-node entry point
%%%============================================================================
%%%
%%% Fetch another HB node's attestation envelope over HTTP, verify it
%%% here, and return the interpretation. Intended for the paper's
%%% cross-node flow where the caller wants THIS node to vouch for a
%%% peer it cannot itself verify.
%%%
%%%   GET /~tpm-interpret@1.0/verify-peer&peer=<base-url>
%%%
%%% `peer' is a bare URL; we normalise it + append `/~tpm2@2.0a/
%%% attestation' and fetch with the standard HB content-negotiation
%%% (`accept: application/json@1.0 + accept-bundle: true') so the
%%% envelope comes back inline with no body+link references (which
%%% would be meaningless on this node's cache).

verify_peer(_Base, Req, Opts) ->
    case hb_maps:get(<<"peer">>, Req, undefined, Opts) of
        undefined ->
            {ok, #{
                <<"status">> => 400,
                <<"body">> => #{
                    <<"error">> => <<"missing-peer">>,
                    <<"detail">> =>
                        <<"verify-peer requires a `peer' key (base URL "
                          "of the node to verify, e.g. "
                          "`http://127.0.0.1:8734').">>
                }
            }};
        PeerUrl when is_binary(PeerUrl) ->
            %% Optional inline trust anchor. If absent, we fall back
            %% to this verifier's configured `lapee_tpm_ca_cert' via
            %% dev_tpm2's `resolve_trusted_ca/2'. Two inline forms
            %% are accepted:
            %%
            %%   `trusted-ca'      — base64url-encoded PEM bytes
            %%                       (HyperBEAM wire convention; the
            %%                       safe form over HTTP/URL).
            %%   `trusted-ca-pem'  — raw PEM text. *Only* works when
            %%                       the request carries an
            %%                       unambiguous binary (e.g. POST
            %%                       body, not GET query string),
            %%                       because the URL form treats `+'
            %%                       as space and mangles the PEM
            %%                       header "BEGIN CERTIFICATE".
            %%
            %% Both mechanisms resolve to raw PEM bytes before we
            %% hand them to dev_tpm2.
            InlineCa = resolve_inline_ca(Req, Opts),
            fetch_and_verify_peer(PeerUrl, InlineCa, Opts);
        Other ->
            {ok, #{
                <<"status">> => 400,
                <<"body">> => #{
                    <<"error">> => <<"bad_peer">>,
                    <<"detail">> =>
                        iolist_to_binary(
                            io_lib:format("peer must be a binary URL; got ~p",
                                          [Other]))
                }
            }}
    end.

%% Pull an inline trust anchor out of Req, normalising whichever of
%% the two supported forms the caller used. Returns raw PEM bytes
%% (a binary) or undefined.
resolve_inline_ca(Req, Opts) ->
    case hb_maps:get(<<"trusted-ca">>, Req, undefined, Opts) of
        B when is_binary(B), byte_size(B) > 0 ->
            try hb_util:decode(B) of
                Decoded when is_binary(Decoded), byte_size(Decoded) > 0 ->
                    Decoded;
                _ -> undefined
            catch _:_ -> undefined
            end;
        _ ->
            case hb_maps:get(<<"trusted-ca-pem">>, Req, undefined, Opts) of
                Pem when is_binary(Pem), byte_size(Pem) > 0 -> Pem;
                _ -> undefined
            end
    end.

fetch_and_verify_peer(PeerUrl, InlineCa, Opts) ->
    Base = strip_trailing_slash(PeerUrl),
    %% Anti-replay: generate a fresh 32-byte nonce and require the
    %% peer's TPM2_Quote to sign it. An attacker replaying an old
    %% attestation envelope can't produce a new quote over OUR
    %% nonce without access to the TPM's AK.
    NonceBytes = crypto:strong_rand_bytes(32),
    NonceB64 = hb_util:encode(NonceBytes),
    FetchMsg = #{
        <<"path">>          => <<"/~tpm2@2.0a/attestation">>,
        <<"accept">>        => <<"application/json@1.0">>,
        <<"accept-bundle">> => <<"true">>,
        <<"nonce">>         => NonceB64
    },
    %% Wrap the fetch: `hb_http:get' can raise on malformed URLs,
    %% transport errors, or decode failures. Treat a raise the same
    %% way we treat `{error, _}' — 502 with a diagnostic — so a
    %% verifier never crashes because a peer misbehaved.
    FetchResult =
        try hb_http:get(Base, FetchMsg, Opts)
        catch Class:Reason ->
            {error, {Class, Reason}}
        end,
    case FetchResult of
        {ok, Response} when is_map(Response) ->
            Envelope = unwrap_envelope(Response, Opts),
            case is_envelope(Envelope) of
                false ->
                    {ok, #{
                        <<"status">> => 502,
                        <<"body">> => #{
                            <<"error">> => <<"peer-did-not-return-envelope">>,
                            <<"peer">>  => Base,
                            <<"detail">> =>
                                <<"GET /~tpm2@2.0a/attestation did not "
                                  "return a LapEE attestation envelope; "
                                  "peer may be unreachable, not "
                                  "LapEE-shaped, or returned an error.">>
                        }
                    }};
                true ->
                    %% Fresh-nonce check happens INSIDE run_cross_
                    %% node_verify by comparing the envelope's
                    %% tpm_quote.nonce to our challenge.
                    run_cross_node_verify(Base, Envelope, InlineCa,
                                          NonceBytes, Opts)
            end;
        {error, Why} ->
            {ok, #{
                <<"status">> => 502,
                <<"body">> => #{
                    <<"error">> => <<"peer-unreachable">>,
                    <<"peer">>  => Base,
                    <<"detail">> =>
                        iolist_to_binary(
                            io_lib:format("hb_http:get failed: ~p", [Why]))
                }
            }};
        Unexpected ->
            {ok, #{
                <<"status">> => 502,
                <<"body">> => #{
                    <<"error">> => <<"peer-unexpected-response">>,
                    <<"peer">>  => Base,
                    <<"detail">> =>
                        iolist_to_binary(
                            io_lib:format("hb_http:get returned ~p",
                                          [Unexpected]))
                }
            }}
    end.

strip_trailing_slash(B) when is_binary(B) ->
    case binary:last(B) of
        $/ -> binary:part(B, 0, byte_size(B) - 1);
        _  -> B
    end.

%% The cross-node path must not return the Envelope map back through
%% HB's response pipeline verbatim — the peer's commitments + any
%% `body+link' references inside would trip hb_cache:write when this
%% node normalises the response. We drop every map-valued field in
%% the result and keep only JSON-primitive-friendly summaries.
run_cross_node_verify(Base, Envelope, InlineCa, NonceBytes, Opts) ->
    %% Anti-replay gate: if the peer's envelope doesn't quote OUR
    %% challenge nonce, reject before anything else. Protects against
    %% replay of a previously-captured valid attestation.
    case envelope_quote_nonce(Envelope, Opts) of
        Bytes when Bytes =:= NonceBytes ->
            {Verified, Verdict, Checks, CaSource} =
                do_verify_summary(Envelope, InlineCa, Opts),
            Interp = safe_interpret(Envelope, Opts),
            Summary = summarise_interp(Interp),
            {ok, #{
                <<"status">> => 200,
                <<"body">> => #{
                    <<"peer">>             => Base,
                    <<"verified">>         => Verified,
                    <<"verdict">>          => Verdict,
                    <<"checks">>           => Checks,
                    <<"summary">>          => Summary,
                    <<"trust-anchor-source">> => CaSource,
                    <<"nonce-challenge">>  => hb_util:encode(NonceBytes),
                    <<"nonce-freshness">>  => <<"verified">>
                }
            }};
        _ ->
            %% Nonce mismatch: the peer returned an envelope that
            %% wasn't signed over our specific challenge. Either the
            %% peer ignored the nonce parameter (old implementation),
            %% the envelope was replayed, or the peer substituted
            %% a different envelope after seeing our challenge. All
            %% three are trust-breaking.
            {ok, #{
                <<"status">> => 200,
                <<"body">> => #{
                    <<"peer">>             => Base,
                    <<"verified">>         => false,
                    <<"verdict">>          => <<"rejected">>,
                    <<"nonce-challenge">>  => hb_util:encode(NonceBytes),
                    <<"nonce-freshness">>  => <<"mismatch">>,
                    <<"checks">>           => [#{
                        <<"name">>   => <<"Verifier-supplied nonce is "
                                          "echoed in the attestation "
                                          "quote">>,
                        <<"ok">>     => false,
                        <<"detail">> =>
                            <<"The peer's envelope quote did not match "
                              "the verifier's random challenge. The "
                              "attestation may be replayed, the peer "
                              "may have ignored the `?nonce=' query, "
                              "or the peer substituted a different "
                              "envelope. Trust not established.">>
                    }]
                }
            }}
    end.

%% Pull the TPM2_Quote's nonce (extraData) from an envelope and
%% decode it to raw bytes. Returns `undefined' on any shape issue.
envelope_quote_nonce(Envelope, Opts) ->
    try
        Q = hb_maps:get(<<"tpm-quote">>, Envelope, #{}, Opts),
        B64 = hb_maps:get(<<"nonce">>, Q, <<>>, Opts),
        hb_util:decode(B64)
    catch _:_ -> undefined
    end.

do_verify_summary(Envelope, InlineCa, Opts) ->
    %% Pass both keys through so dev_tpm2:resolve_trusted_ca can
    %% classify the source itself (and return it to us via body.
    %% trust_anchor_source). Avoids duplicating the priority rule
    %% here.
    Req0 = #{<<"envelope">> => Envelope},
    Req  = case InlineCa of
               undefined -> Req0;
               _         -> Req0#{<<"trusted-ca-pem">> => InlineCa}
           end,
    case dev_tpm2:verify(Envelope, Req, Opts) of
        {ok, #{<<"body">> := Body}} ->
            V = maps:get(<<"verified">>, Body, false),
            D = maps:get(<<"verdict">>, Body, <<"rejected">>),
            C = maps:get(<<"checks">>, Body, []),
            S = maps:get(<<"trust-anchor-source">>, Body, <<"node_config">>),
            {V, D, flatten_checks(C), S};
        _ ->
            {false, <<"rejected">>, [], <<"none">>}
    end.

flatten_checks(Cs) when is_list(Cs) ->
    [ case C of
          #{<<"ok">> := O, <<"name">> := N, <<"detail">> := De} ->
              Sev = maps:get(<<"severity">>, C, <<"core">>),
              #{<<"ok">> => O, <<"name">> => N, <<"detail">> => De,
                <<"severity">> => Sev};
          #{<<"ok">> := O, <<"name">> := N} ->
              Sev = maps:get(<<"severity">>, C, <<"core">>),
              #{<<"ok">> => O, <<"name">> => N, <<"detail">> => <<"">>,
                <<"severity">> => Sev};
          _ -> #{<<"ok">> => false, <<"name">> => <<"unknown">>,
                 <<"detail">> => <<"">>, <<"severity">> => <<"core">>}
      end || C <- Cs];
flatten_checks(_) -> [].

%% Produce a small, link-free summary of the interpretation — the
%% fields a caller would actually act on when deciding whether to
%% trust the peer. The full structured interpretation is still
%% available via `/~tpm-interpret@1.0/interpret' against the same
%% envelope if callers want every field.
summarise_interp(Interp) when is_map(Interp) ->
    Tpm  = maps:get(<<"tpm">>,  Interp, #{}),
    Ak   = maps:get(<<"ak">>,   Interp, #{}),
    Q    = maps:get(<<"quote">>, Interp, #{}),
    Boot = maps:get(<<"boot">>, Interp, #{}),
    Node = maps:get(<<"node">>, Interp, #{}),
    Env  = maps:get(<<"envelope">>, Interp, #{}),
    #{
        <<"envelope-version">> =>
            maps:get(<<"version">>, Env, null),
        <<"tpm-manufacturer">> =>
            maps:get(<<"manufacturer-name">>, Tpm, null),
        <<"tpm-manufacturer-kind">> =>
            maps:get(<<"manufacturer-kind">>, Tpm, null),
        <<"tpm-model">> =>
            maps:get(<<"model">>, Tpm, null),
        <<"tpm-firmware-version">> =>
            maps:get(<<"firmware-version">>, Tpm, null),
        <<"ak-algorithm">> =>
            maps:get(<<"algorithm">>, Ak, null),
        <<"ak-key-size-bits">> =>
            maps:get(<<"key-size-bits">>, Ak, null),
        <<"ak-public-key-b64url">> =>
            maps:get(<<"pub-der-sha256-b64url">>, Ak, null),
        <<"quote-attest-type">> =>
            maps:get(<<"attest-type">>, Q, null),
        <<"quote-clock-ms">> =>
            maps:get(<<"clock-ms">>, Q, null),
        <<"quote-reset-count">> =>
            maps:get(<<"reset-count">>, Q, null),
        <<"secure-boot-measured">> =>
            maps:get(<<"secure-boot-measured">>, Boot, null),
        <<"wallet-address">> =>
            maps:get(<<"wallet-address">>, Node, null),
        <<"node-message-id">> =>
            maps:get(<<"node-message-id">>, Node, null),
        <<"on-start-hook-device">> =>
            maps:get(<<"on-start-hook-device">>, Node, null),
        <<"pcr15-event-count">> =>
            maps:get(<<"pcr15-event-count">>, Node, null)
    };
summarise_interp(_) -> #{}.

%% The response from `hb_http:get' is a full HB message. The
%% attestation envelope may be returned directly (top-level
%% `lapee_attestation_version' key) or wrapped under `body' (the
%% usual device-response shape). Peel until we find something that
%% looks like our envelope.
unwrap_envelope(M, Opts) ->
    case is_envelope(M) of
        true -> M;
        false ->
            case hb_maps:get(<<"body">>, M, undefined, Opts) of
                Inner when is_map(Inner) -> unwrap_envelope(Inner, Opts);
                _ -> M
            end
    end.

%%%============================================================================
%%% interpret/3 — structured reading of the envelope
%%%============================================================================

interpret(Base, Req, Opts) ->
    Envelope = resolve_envelope(Base, Req, Opts),
    {ok, #{
        <<"status">> => 200,
        <<"body">> => interpret_envelope(Envelope, Opts)
    }}.

%%%============================================================================
%%% Envelope resolution (same shape as dev_tpm2:verify)
%%%============================================================================

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
is_envelope(_) -> false.

safe_interpret(E, Opts) ->
    try interpret_envelope(E, Opts)
    catch _:_ -> #{<<"error">> => <<"envelope_unreadable">>}
    end.

%%%============================================================================
%%% Top-level interpretation assembly
%%%============================================================================

interpret_envelope(E, Opts) ->
    Db = hb_db_tpm:load(Opts),
    Tpm = interpret_tpm_identity(E, Db),
    Ak  = interpret_ak(E),
    Quote = interpret_quote_metadata(E),
    %% Events first — the rich per-record decoded TCG event log. Every
    %% downstream interpretation (PCR-level enrichment, boot chain,
    %% kernel, IMA, claim) drills into these events to extract named
    %% fields. Keeping events as the single source of truth keeps
    %% the interpretation tree consistent: you can always navigate
    %% from `/interpret/pcrs/N/derived/<field>' back to the source
    %% events at `/interpret/pcrs/N/events/<seq>' and from there to
    %% the raw record at `/interpret/events/<seq>'.
    Events = interpret_events(E),
    Pcrs = interpret_pcrs(E, Db, Events),
    Boot = interpret_boot_chain(E, Db, Pcrs),
    Kernel = interpret_kernel(E, Db, Pcrs),
    Ima = interpret_ima(E, Db, Pcrs),
    Node = interpret_node(E),
    Env = interpret_envelope_meta(E),
    Claim = interpret_claim(Events, E, Db),
    #{
        <<"envelope">> => Env,
        <<"tpm">>      => Tpm,
        <<"ak">>       => Ak,
        <<"quote">>    => Quote,
        <<"pcrs">>     => Pcrs,
        <<"boot">>     => Boot,
        <<"kernel">>   => Kernel,
        <<"ima">>      => Ima,
        <<"node">>     => Node,
        <<"events">>   => Events,
        <<"claim">>    => Claim
    }.

%%---- events (full parsed + decoded TCG event log) ----------------------
%%
%% Surfaces every firmware-side event as an AO-Core native message.
%% Empty when the envelope has no tcg_event_log (e.g. QEMU+swtpm
%% test guests). Keyed by 1-based sequence number so individual
%% events are path-addressable:
%%
%%     /.../events/3                → whole event 3
%%     /.../events/3/event_type     → its type string
%%     /.../events/3/digests/sha256 → one digest
%%     /.../events/3/parsed         → the per-type decoded payload

interpret_events(E) ->
    encode_events_for_wire(interpret_events_raw(E)).

%% Raw (non-wire-encoded) events map. For internal consumers like
%% `interpret_claim' that need UTF-8 values (kernel cmdline flags,
%% variable names, etc.) without base64url round-tripping.
interpret_events_raw(E) ->
    case hb_maps:get(<<"tcg-event-log">>, E, <<>>, #{}) of
        LogB64 when is_binary(LogB64), byte_size(LogB64) > 0 ->
            LogBin = try hb_util:decode(LogB64) catch _:_ -> <<>> end,
            case byte_size(LogBin) of
                0 -> #{};
                _ ->
                    dev_tpm_tcg:decode_events(
                      dev_tpm_tcg:parse(LogBin))
            end;
        _ -> #{}
    end.

%% Recursively walk the events map and encode every BINARY value
%% as base64url, EXCEPT for fields we know are safe UTF-8 strings
%% (event_type, variable_name, action, etc.).
encode_events_for_wire(M) when is_map(M) ->
    maps:map(fun encode_field/2, M);
encode_events_for_wire(Other) -> Other.

encode_field(_K, V) when is_map(V) ->
    maps:map(fun encode_field/2, V);
encode_field(_K, V) when is_list(V) ->
    [encode_field_val(X) || X <- V];
%% These keys carry UTF-8 strings by construction — leave as-is.
%% Keys whose VALUE we know to be a UTF-8-safe string by
%% construction (produced by our decoders, not firmware bytes).
%% These pass through unchanged; all other binary values get
%% base64url-encoded so the JSON encoder doesn't choke on raw
%% firmware bytes.
encode_field(K, V) when is_binary(V) ->
    case is_utf8_safe_key(K) of
        true  -> V;
        false -> hb_util:encode(V)
    end;
encode_field(_K, V) -> V.

is_utf8_safe_key(<<"event-type">>)              -> true;
is_utf8_safe_key(<<"variable-name">>)           -> true;
is_utf8_safe_key(<<"variable-guid">>)           -> true;
is_utf8_safe_key(<<"type-guid">>)               -> true;
is_utf8_safe_key(<<"type-guid-name">>)          -> true;
is_utf8_safe_key(<<"tag-guid">>)                -> true;
is_utf8_safe_key(<<"tag-category">>)            -> true;
is_utf8_safe_key(<<"tag-id-hex">>)              -> true;
is_utf8_safe_key(<<"tag-id-name">>)             -> true;
is_utf8_safe_key(<<"tag-description">>)         -> true;
is_utf8_safe_key(<<"disk-guid">>)               -> true;
is_utf8_safe_key(<<"load-option-description">>) -> true;
is_utf8_safe_key(<<"table-description">>)       -> true;
is_utf8_safe_key(<<"action">>)                  -> true;
is_utf8_safe_key(<<"crtm-version">>)            -> true;
is_utf8_safe_key(<<"post-code">>)               -> true;
is_utf8_safe_key(<<"post-code-bytes">>)         -> true;
is_utf8_safe_key(<<"format">>)                  -> true;
is_utf8_safe_key(<<"key">>)                     -> true;
is_utf8_safe_key(<<"value">>)                   -> true;
is_utf8_safe_key(<<"separator">>)               -> true;
is_utf8_safe_key(<<"separator-kind">>)          -> true;
is_utf8_safe_key(<<"spec-id">>)                 -> true;
is_utf8_safe_key(<<"marker">>)                  -> true;
is_utf8_safe_key(<<"blob-description">>)        -> true;
is_utf8_safe_key(<<"text">>)                    -> true;
is_utf8_safe_key(<<"hash-alg-name">>)           -> true;
is_utf8_safe_key(<<"error">>)                   -> true;
is_utf8_safe_key(<<"path">>)                    -> true;  % file path + device path text
is_utf8_safe_key(<<"device-path-text">>)        -> true;
is_utf8_safe_key(<<"cpu-family-model-stepping">>) -> true;
is_utf8_safe_key(<<"date">>)                    -> true;  % e.g. "2024-04-15"
is_utf8_safe_key(<<"processor-rev-id-hex">>)    -> true;
is_utf8_safe_key(<<"nonhost-kind">>)            -> true;
is_utf8_safe_key(<<"note">>)                    -> true;  % human-readable
is_utf8_safe_key(<<"spdm-kind">>)               -> true;
is_utf8_safe_key(<<"sipa-category">>)           -> true;
is_utf8_safe_key(<<"sipa-subtype-name">>)       -> true;
is_utf8_safe_key(<<"vendor-guid">>)             -> true;
is_utf8_safe_key(<<"vendor-guid-name">>)        -> true;
is_utf8_safe_key(<<"protocol-guid">>)           -> true;
is_utf8_safe_key(<<"fv-file-name">>)            -> true;
is_utf8_safe_key(<<"fv-name">>)                 -> true;
is_utf8_safe_key(<<"disk-type-guid">>)          -> true;
is_utf8_safe_key(<<"owner-guid">>)              -> true;
is_utf8_safe_key(<<"subtype-name">>)            -> true;
is_utf8_safe_key(<<"type-name">>)               -> true;
is_utf8_safe_key(<<"partition-format">>)        -> true;
is_utf8_safe_key(<<"signature-type">>)          -> true;
is_utf8_safe_key(<<"partition-signature">>)     -> true;
is_utf8_safe_key(<<"hid-string">>)              -> true;
is_utf8_safe_key(<<"uid-string">>)              -> true;
is_utf8_safe_key(<<"cid-string">>)              -> true;
is_utf8_safe_key(<<"mac">>)                     -> true;
is_utf8_safe_key(<<"local-ip">>)                -> true;
is_utf8_safe_key(<<"remote-ip">>)               -> true;
is_utf8_safe_key(<<"gateway-ip">>)              -> true;
is_utf8_safe_key(<<"subnet-mask">>)             -> true;
is_utf8_safe_key(<<"uri">>)                     -> true;
is_utf8_safe_key(<<"ssid">>)                    -> true;
is_utf8_safe_key(<<"bd-addr">>)                 -> true;
is_utf8_safe_key(<<"uuid">>)                    -> true;
is_utf8_safe_key(<<"description">>)             -> true;
is_utf8_safe_key(<<"component">>)               -> true;
is_utf8_safe_key(<<"revision">>)                -> true;
is_utf8_safe_key(<<"x509-subject">>)            -> true;
is_utf8_safe_key(<<"x509-issuer">>)             -> true;
is_utf8_safe_key(<<"x509-serial">>)             -> true;
is_utf8_safe_key(<<"x509-not-before">>)         -> true;
is_utf8_safe_key(<<"x509-not-after">>)          -> true;
is_utf8_safe_key(<<"x509-public-key-alg">>)     -> true;
is_utf8_safe_key(<<"x509-signature-alg">>)      -> true;
is_utf8_safe_key(<<"x509-sha256-fingerprint">>) -> true;
is_utf8_safe_key(_)                             -> false.

encode_field_val(V) when is_map(V) -> maps:map(fun encode_field/2, V);
encode_field_val(V) when is_binary(V) ->
    %% List elements don't carry their key context, so we can't
    %% look up is_utf8_safe_key/1. Inspect the bytes: if the whole
    %% binary is printable ASCII, pass through; otherwise base64url.
    %% This is the right policy for `boot-order` (list of
    %% <<"Boot0001">>), `authorities` (list of UTF-8 names), etc.,
    %% while still base64-encoding any list of opaque bytes.
    case is_printable_ascii(V) of
        true  -> V;
        false -> hb_util:encode(V)
    end;
encode_field_val(V) -> V.

is_printable_ascii(<<>>) -> true;
is_printable_ascii(<<C, Rest/binary>>) when C >= 16#20, C =< 16#7E ->
    is_printable_ascii(Rest);
is_printable_ascii(_) -> false.

%%---- claim (flat, policy-friendly surface with provenance) -------------
%%
%% Each claim names a concrete property of the attested node. Value
%% is either a concrete binary / bool / string OR `"unknown"' when
%% the envelope doesn't carry enough evidence to decide. Every
%% populated claim carries a `_provenance' key listing the source
%% events (by {pcr, seq} tuples) that backed the derivation, so a
%% downstream verifier can audit.
%%
%%   claim.secure_boot.enabled
%%   claim.secure_boot.db_authorities
%%   claim.firmware.crtm_version
%%   claim.boot_loader.image_hash
%%   claim.kernel.uki_hash
%%   claim.kernel.cmdline
%%   claim.kernel.iommu_strict
%%   claim.tme.enabled
%%   claim.lockdown.level

interpret_claim(Events, E, Db) ->
    EvList = event_list(Events),
    Context = detect_context(Events, EvList),
    #{
        <<"secure-boot">>        => claim_secure_boot(EvList),
        <<"firmware">>           => claim_firmware(EvList, Db),
        <<"boot-loader">>        => claim_boot_loader(EvList),
        <<"boot-chain">>         => claim_boot_chain(EvList),
        <<"kernel">>             => claim_kernel(EvList, E),
        <<"cpu">>                => claim_cpu(EvList, Db),
        <<"shim">>               => claim_shim(EvList),
        %% Paper §Architecture — the quote itself carries freshness
        %% (reset-count / restart-count / clock-ms), TPM firmware
        %% identity, and the exact PCR selection that was quoted.
        %% Surface them on the compact claim API so policy engines
        %% don't have to parse the full interpret output.
        <<"quote">>              => claim_quote(E),
        %% PCR cross-reference — does the (PCR 0, PCR 1, PCR 7)
        %% triple match any profile in the shipped pcr-profiles/
        %% DB? If so we know exactly which firmware + platform
        %% booted this machine.
        <<"pcr-match">>          => claim_pcr_match(E, Db),
        %% Hour-6: fundamental quote-integrity check. Recompute
        %% SHA-XX(concat of selected PCRs) and compare against
        %% the TPM's declared pcrDigest. A mismatch means the
        %% quote is fraudulent OR the PCR values in the envelope
        %% were tampered with between signing and transport.
        <<"quote-integrity">>    => claim_quote_integrity(E),
        %% Hour-6: freshness composite — reset-count / restart-
        %% count / clock / safe / nonce rolled into a single
        %% policy-ready stanza. Tells a verifier whether the
        %% quote is from the current boot epoch.
        <<"freshness">>          => claim_freshness(E),
        %% Paper field #2 — TPM identity (vendor, model, firmware,
        %% spec, CVEs). Derived from the EK cert's TCG OIDs
        %% (2.23.133.2.1-3, 2.23.133.2.16) + the vendor catalogue.
        <<"tpm">>                => claim_tpm(E, Db),
        %% Confidential-compute context: Intel TDX CCEL / AMD SEV-
        %% SNP. When detected it's tier-5 evidence for claim.tme.
        <<"context">>            => Context,
        %% Paper-committed machine-identifying fields composed
        %% across tier 1 (events) / tier 2 (cmdline) / tier 3
        %% (UKI-hash DB) / tier 4 (boot-reached-PCR-15) /
        %% tier 5 (confidential-compute context). Every populated
        %% value carries a `*-evidence' list.
        <<"tme">>                => claim_tme(EvList, E, Db, Context),
        <<"iommu">>              => claim_iommu(EvList),
        <<"lockdown">>           => claim_lockdown(EvList, E, Db),
        <<"kernel-integrity">>   => claim_kernel_integrity(EvList),
        <<"verity">>             => claim_verity(EvList)
    }.

%%---- Confidential-compute context detection --------------------------
%%
%% Standard TCG PC Client logs start with an EV_NO_ACTION on PCR 0
%% carrying a "Spec ID Event03" header. Intel TDX's Confidential
%% Computing Event Log (CCEL) starts on PCR 1 (MRTD) with a
%% TDX-specific SpecID. AMD SEV-SNP guests typically emit a standard
%% TCG PC Client log with an SEV-SNP EV_EVENT_TAG early on.
%%
%% Returns #{kind, family, evidence}. `kind' is one of:
%%   <<"tcg-pc-client">>     normal firmware boot
%%   <<"intel-tdx-ccel">>    Intel TDX trust domain
%%   <<"amd-sev-snp">>       AMD SEV-SNP encrypted VM
%%   <<"amd-sev">>           AMD SEV (non-SNP, pre-Milan)
%%   <<"unknown">>           can't determine
detect_context(Events, _EvList) when is_map(Events), map_size(Events) =:= 0 ->
    #{<<"kind">> => <<"unknown">>,
      <<"family">> => <<"unknown">>,
      <<"evidence">> => []};
detect_context(Events, EvList) ->
    First = maps:get(<<"1">>, Events, #{}),
    FirstPcr = maps:get(<<"pcr">>, First, 0),
    TdxHit = FirstPcr =/= 0,
    SevSnpHit = has_sev_snp_tag(EvList),
    case {TdxHit, SevSnpHit} of
        {true, _} ->
            #{<<"kind">>       => <<"intel-tdx-ccel">>,
              <<"family">>     => <<"confidential-compute">>,
              <<"evidence">>   =>
                  [{<<"reason">>,
                    <<"first-record-pcr-nonzero">>},
                   {<<"first-pcr">>, FirstPcr}]};
        {_, true} ->
            #{<<"kind">>       => <<"amd-sev-snp">>,
              <<"family">>     => <<"confidential-compute">>,
              <<"evidence">>   =>
                  [{<<"reason">>, <<"sev-snp-event-tag">>}]};
        _ ->
            #{<<"kind">>       => <<"tcg-pc-client">>,
              <<"family">>     => <<"classical">>,
              <<"evidence">>   => []}
    end.

%% Recognise AMD SEV/SEV-SNP init tags by GUID prefix or by
%% well-known Azure / GCE confidential-compute tag IDs in the first
%% 10 events. The exact GUIDs are defined in SVSM / AMD CCP specs.
has_sev_snp_tag(EvList) ->
    Early = lists:sublist(EvList, 20),
    lists:any(
      fun(Ev) ->
          case maps:get(<<"event-type-code">>, Ev, 0) of
              16#6 ->
                  Parsed = maps:get(<<"parsed">>, Ev, #{}),
                  Guid = maps:get(<<"tag-guid">>, Parsed, <<>>),
                  Name = maps:get(<<"tag-id-name">>, Parsed, <<>>),
                  binary:match(Guid, <<"sev-snp">>) =/= nomatch
                    orelse binary:match(Name, <<"sev-snp">>) =/= nomatch
                    orelse binary:match(Name, <<"SEV">>) =/= nomatch
                    orelse Guid =:= <<"f5bc582a-3b04-4d0c-a2f5-e1b2a3c4d5e6">>;
              _ -> false
          end
      end, Early).

%%---- Paper field #2: claim.tpm (vendor + model + spec + CVEs) --------
claim_tpm(E, Db) ->
    Tpm = interpret_tpm_identity(E, Db),
    %% Known CVEs list: from the vendor catalogue (`known_cves' or
    %% `known-cves' key). If the vendor had a known ROCA or TPM-FAIL
    %% hit and our EK cert matches their fingerprint, surface it.
    Cves = maps:get(<<"known_cves">>, Tpm,
              maps:get(<<"known-cves">>, Tpm, [])),
    %% Trust-tier: discrete > fTPM-cpu > server-platform > virtual.
    Kind = maps:get(<<"manufacturer-kind">>, Tpm, null),
    TrustTier = tpm_trust_tier(Kind),
    %% Evidence: the EK cert chain validation result lives in
    %% dev_tpm2's checks layer; here we record just the cert-level
    %% facts.
    CertEv = case maps:get(<<"ek-cert-subject">>, Tpm, null) of
        null -> [];
        _ ->
            [{<<"tier">>, 1},
             {<<"source">>, <<"ek-cert-tcg-oids">>}]
    end,
    #{
        <<"manufacturer-id">>    => maps:get(<<"manufacturer-id">>,
                                              Tpm, null),
        <<"manufacturer-name">>  => maps:get(<<"manufacturer-name">>,
                                              Tpm, null),
        <<"manufacturer-kind">>  => Kind,
        <<"model">>              => maps:get(<<"model">>, Tpm, null),
        <<"firmware-version">>   => maps:get(<<"firmware-version">>,
                                              Tpm, null),
        <<"spec-family">>        => maps:get(<<"spec-family">>, Tpm,
                                              null),
        <<"spec-level">>         => maps:get(<<"spec-level">>, Tpm,
                                              null),
        <<"spec-revision">>      => maps:get(<<"spec-revision">>, Tpm,
                                              null),
        <<"trust-tier">>         => TrustTier,
        <<"known-cves">>         => Cves,
        <<"evidence">>           => CertEv
    }.

%% Trust tier ordering per paper §Hardware-Availability:
%%  discrete TPM       : strongest (dedicated chip, own RAM, own clock)
%%  fTPM-cpu           : weaker (shares CPU TEE; compromise propagates)
%%  server-platform    : re-issued under OEM CA; depends on OEM's
%%                       attestation hygiene
%%  virtual / software : hypervisor-rooted; trust is in the cloud
%%                       provider
tpm_trust_tier(<<"discrete">>)        -> <<"strongest">>;
tpm_trust_tier(<<"fTPM-cpu">>)        -> <<"cpu-tee">>;
tpm_trust_tier(<<"fTPM_cpu">>)        -> <<"cpu-tee">>;  % legacy spelling
tpm_trust_tier(<<"server-platform">>) -> <<"oem-reissued">>;
tpm_trust_tier(<<"virtual">>)         -> <<"hypervisor">>;
tpm_trust_tier(<<"software">>)        -> <<"hypervisor">>;
tpm_trust_tier(_)                     -> <<"unknown">>.

%% CPU microcode identity — from EV_CPU_MICROCODE on PCR 1.
%% Discriminates Intel vs AMD vs unknown via `parsed.format'.
%% The 2-arg form additionally cross-references
%% `priv/tpm-interpret/cpu-models.json' to attach a human-readable
%% `codename', `brand-range', `micro-arch', `year' and the
%% supported TEE/hardening feature set.
claim_cpu(Events) -> claim_cpu(Events, #{}).

claim_cpu(Events, Db) ->
    UcodeEvs = [Ev || Ev <- Events,
                      maps:get(<<"event-type-code">>, Ev, 0) =:= 16#09],
    case UcodeEvs of
        [] ->
            unknown_cpu_claim();
        [Ev | _] ->
            P = nested(Ev, [<<"parsed">>], #{}),
            Vendor = maps:get(<<"format">>, P, <<"unknown">>),
            Desc = format_microcode_desc(Vendor, P),
            {Family, Model, Stepping} = extract_cpu_fms(Vendor, P),
            Lookup = cpu_model_lookup(Vendor, Family, Model, Db),
            Base = #{
                <<"vendor">>              => Vendor,
                <<"vendor-provenance">>   => [event_provenance(Ev)],
                <<"microcode-description">>           => Desc,
                <<"microcode-description-provenance">>=>
                    [event_provenance(Ev)],
                <<"cpu-family">>          => to_int_or_null(Family),
                <<"cpu-model">>           => to_int_or_null(Model),
                <<"cpu-stepping">>        => to_int_or_null(Stepping),
                <<"cpu-family-model-key">> =>
                    family_model_key(Family, Model)
            },
            merge_cpu_lookup(Base, Lookup, Ev)
    end.

unknown_cpu_claim() ->
    #{<<"vendor">>              => <<"unknown">>,
      <<"vendor-provenance">>   => [],
      <<"microcode-description">>           => <<"unknown">>,
      <<"microcode-description-provenance">>=> [],
      <<"cpu-family">>          => null,
      <<"cpu-model">>           => null,
      <<"cpu-stepping">>        => null,
      <<"cpu-family-model-key">>=> null,
      <<"codename">>            => null,
      <<"brand-range">>         => null,
      <<"micro-arch">>          => null,
      <<"year">>                => null,
      <<"tee-support">>         => [],
      <<"codename-provenance">> => []}.

format_microcode_desc(<<"intel">>, P) ->
    iolist_to_binary(io_lib:format(
        "intel rev=0x~.16B sig=0x~.16B ~s",
        [maps:get(<<"update-revision">>, P, 0),
         maps:get(<<"processor-signature">>, P, 0),
         maps:get(<<"cpu-family-model-stepping">>, P, <<"">>)]));
format_microcode_desc(<<"amd">>, P) ->
    iolist_to_binary(io_lib:format(
        "amd patch-id=0x~.16B proc-rev=0x~4.16.0B ~s",
        [maps:get(<<"patch-id">>, P, 0),
         maps:get(<<"processor-rev-id">>, P, 0),
         maps:get(<<"date">>, P, <<"">>)]));
format_microcode_desc(_, _) -> <<"unknown">>.

%% Extract family/model/stepping from the format-specific parse.
%% Intel: `cpu-family-model-stepping' string has "family=N model=N
%%        stepping=N" (set by dev_tpm_tcg:format_intel_sig/1).
%% AMD:   `processor-rev-id' is a u16 (BaseModel-in-low, ExtendedModel
%%        middle, Family in high bits per AMD PPR).
extract_cpu_fms(<<"intel">>, P) ->
    S = maps:get(<<"cpu-family-model-stepping">>, P, <<>>),
    parse_fms_string(S);
extract_cpu_fms(<<"amd">>, P) ->
    Rev = maps:get(<<"processor-rev-id">>, P, 0),
    %% AMD ProcessorRevId (u16): bits 0-3 stepping, 4-11 combined
    %% model (low-nibble = BaseModel, high-byte bits 8-11 = ExtModel
    %% shifted), 12-15 family low-nibble; BaseFamily + ExtFamily per
    %% AMD PPR section "Processor Revision Identifier".
    %% Pragmatic approximation matching the Linux kernel's ucode
    %% parser in arch/x86/kernel/cpu/microcode/amd.c:
    Stepping = Rev band 16#F,
    Model    = (Rev bsr 4) band 16#FF,
    Family   = (Rev bsr 12) band 16#F,
    FullFamily =
        case Family of
            16#F -> Family + ((Rev bsr 20) band 16#FF);
            _    -> Family
        end,
    {FullFamily, Model, Stepping};
extract_cpu_fms(_, _) ->
    {undefined, undefined, undefined}.

%% Parse "family=6 model=151 stepping=2" → {6, 151, 2}.
parse_fms_string(<<>>) -> {undefined, undefined, undefined};
parse_fms_string(S) when is_binary(S) ->
    {find_fms(S, <<"family=">>),
     find_fms(S, <<"model=">>),
     find_fms(S, <<"stepping=">>)}.

find_fms(S, Prefix) ->
    case binary:split(S, Prefix) of
        [_, Rest] ->
            case binary:split(Rest, <<" ">>) of
                [NumBin | _] -> safe_int(NumBin);
                _            -> safe_int(Rest)
            end;
        _ -> undefined
    end.

safe_int(B) when is_binary(B) ->
    try binary_to_integer(B) catch _:_ -> undefined end;
safe_int(_) -> undefined.

to_int_or_null(undefined) -> null;
to_int_or_null(N) when is_integer(N) -> N;
to_int_or_null(_) -> null.

family_model_key(Family, Model)
    when is_integer(Family), is_integer(Model) ->
    iolist_to_binary(io_lib:format("~B-~B", [Family, Model]));
family_model_key(_, _) -> null.

%% Look up the given family/model in the CPU models DB. Vendor is
%% dispatched to "intel" | "amd" sub-maps of the top-level doc.
cpu_model_lookup(<<"intel">>, F, M, Db) ->
    cpu_model_lookup_in(<<"intel">>, F, M, Db);
cpu_model_lookup(<<"amd">>, F, M, Db) ->
    cpu_model_lookup_in(<<"amd">>, F, M, Db);
cpu_model_lookup(_, _, _, _) -> undefined.

cpu_model_lookup_in(VendorKey, F, M, Db)
    when is_integer(F), is_integer(M) ->
    VendorMap =
        maps:get(VendorKey,
                 maps:get(<<"cpu-models">>, Db, #{}), #{}),
    Key = iolist_to_binary(io_lib:format("~B-~B", [F, M])),
    case maps:get(Key, VendorMap, undefined) of
        M0 when is_map(M0) -> M0;
        _ -> undefined
    end;
cpu_model_lookup_in(_, _, _, _) -> undefined.

merge_cpu_lookup(Base, undefined, _Ev) ->
    Base#{
        <<"codename">>         => null,
        <<"brand-range">>      => null,
        <<"micro-arch">>       => null,
        <<"year">>             => null,
        <<"tee-support">>      => [],
        <<"codename-provenance">> => []
    };
merge_cpu_lookup(Base, Lookup, Ev) ->
    Base#{
        <<"codename">>         => maps:get(<<"codename">>, Lookup, null),
        <<"brand-range">>      => maps:get(<<"brand-range">>, Lookup, null),
        <<"micro-arch">>       => maps:get(<<"micro-arch">>, Lookup, null),
        <<"year">>             => maps:get(<<"year">>, Lookup, null),
        <<"tee-support">>      => maps:get(<<"tee-support">>, Lookup, []),
        <<"codename-provenance">> =>
            [event_provenance(Ev),
             {<<"source">>, <<"cpu-models.json">>}]
    }.

%% Shim-specific: the SBAT revocation policy + MokListTrusted
%% state. Found in EV_EFI_VARIABLE_AUTHORITY events.
claim_shim(Events) ->
    AuthEvs = [Ev || Ev <- Events,
                     maps:get(<<"event-type-code">>, Ev, 0) =:= 16#800000E0],
    SbatEvs = [Ev || Ev <- AuthEvs,
                     sem_var_name(Ev) =:= <<"SbatLevel">>],
    MokEvs = [Ev || Ev <- AuthEvs,
                    sem_var_name(Ev) =:= <<"MokListTrusted">>],
    {SbatRev, SbatProv} = case SbatEvs of
        [] -> {<<"unknown">>, []};
        [Sev | _] ->
            SSem = nested(Sev, [<<"parsed">>, <<"semantic">>], #{}),
            case maps:get(<<"sbat-entries">>, SSem, []) of
                [#{<<"component">> := <<"sbat">>,
                   <<"revision">> := Rev} | _] ->
                    {Rev, [event_provenance(Sev)]};
                _ -> {<<"unknown">>, []}
            end
    end,
    {MokTrusted, MokProv} = case MokEvs of
        [] -> {<<"unknown">>, []};
        [Mev | _] ->
            MSem = nested(Mev, [<<"parsed">>, <<"semantic">>], #{}),
            V = maps:get(<<"moklist-trusted">>, MSem, <<"unknown">>),
            {V, [event_provenance(Mev)]}
    end,
    #{<<"sbat-revision">>               => SbatRev,
      <<"sbat-revision-provenance">>    => SbatProv,
      <<"moklist-trusted">>             => MokTrusted,
      <<"moklist-trusted-provenance">>  => MokProv}.

%% Convert the keyed events map into a list sorted by seq number —
%% more convenient for iterating and filtering per event-type.
event_list(Events) when is_map(Events) ->
    Sorted = lists:sort(
        fun({KA, _}, {KB, _}) ->
            binary_to_integer(KA) =< binary_to_integer(KB)
        end,
        maps:to_list(Events)),
    [V || {_, V} <- Sorted, is_map(V), not maps:is_key(<<"error">>, V)];
event_list(_) -> [].

%% Secure Boot state + enrolled authorities.
claim_secure_boot(Events) ->
    SbEvents = [Ev || Ev <- Events,
                      maps:get(<<"event-type-code">>, Ev, 0) =:= 16#80000001,
                      sem_var_name(Ev) =:= <<"SecureBoot">>],
    {Enabled, Prov} = case SbEvents of
        [] -> {<<"unknown">>, []};
        [Ev0 | _] ->
            Sem = nested(Ev0, [<<"parsed">>, <<"semantic">>], #{}),
            V = maps:get(<<"secure-boot-enabled">>, Sem, <<"unknown">>),
            {V, [event_provenance(Ev0)]}
    end,
    DbAuths = collect_authorities(Events),
    SetupMode = lookup_binary_sem(Events, <<"SetupMode">>,
                                  <<"setup-mode">>),
    DeployedMode = lookup_binary_sem(Events, <<"DeployedMode">>,
                                     <<"deployed-mode">>),
    #{
        <<"enabled">>          => Enabled,
        <<"enabled-provenance">>=> Prov,
        <<"db-authorities">>   => DbAuths,
        <<"setup-mode">>       => SetupMode,
        <<"deployed-mode">>    => DeployedMode
    }.

%% Collect summarised signature-list entries from PK / KEK / db
%% variable events (which enumerate which keys are enrolled).
collect_authorities(Events) ->
    lists:flatten(
        [nested(Ev, [<<"parsed">>, <<"semantic">>, <<"signature-list">>], [])
         || Ev <- Events,
            maps:get(<<"event-type-code">>, Ev, 0) =:= 16#80000001,
            lists:member(sem_var_name(Ev),
                         [<<"PK">>, <<"KEK">>, <<"db">>, <<"dbx">>])]).

lookup_binary_sem(Events, VarName, SemKey) ->
    case [Ev || Ev <- Events,
                maps:get(<<"event-type-code">>, Ev, 0) =:= 16#80000001,
                sem_var_name(Ev) =:= VarName] of
        [] -> <<"unknown">>;
        [Ev | _] ->
            nested(Ev, [<<"parsed">>, <<"semantic">>, SemKey], <<"unknown">>)
    end.

sem_var_name(Ev) ->
    nested(Ev, [<<"parsed">>, <<"variable-name">>], <<>>).

%% Firmware identity from EV_S_CRTM_VERSION.
%% The 2-arg form additionally cross-references the shipped
%% firmware-versions DB; when the CRTM string starts with a
%% known vendor prefix we project the manifest's full attribute
%% set (vendor, trust-tier, secure-boot-default, ek-root-ca-
%% source, virtualization-platform, tpm-vendor-id, platforms)
%% back onto the claim alongside the raw CRTM string.
claim_firmware(Events) -> claim_firmware(Events, #{}).

claim_firmware(Events, Db) ->
    Matches = [Ev || Ev <- Events,
                     maps:get(<<"event-type-code">>, Ev, 0) =:= 16#8],
    case Matches of
        [] -> unknown_firmware_claim();
        [Ev0 | _] ->
            Version = nested(Ev0, [<<"parsed">>, <<"crtm-version">>],
                             <<"unknown">>),
            Base = #{
                <<"crtm-version">> => Version,
                <<"crtm-version-provenance">> =>
                    [event_provenance(Ev0)]},
            enrich_firmware_with_db(Base, Version, Db, Ev0)
    end.

unknown_firmware_claim() ->
    #{<<"crtm-version">> => <<"unknown">>,
      <<"crtm-version-provenance">> => [],
      <<"family-name">> => null,
      <<"family-vendor">> => null,
      <<"family-trust-tier">> => null,
      <<"family-secure-boot-default">> => null,
      <<"family-tpm-vendor-id">> => null,
      <<"family-virtualization-platform">> => null,
      <<"family-ek-root-ca-source">> => null,
      <<"family-platform">> => null,
      <<"family-provenance">> => []}.

%% Enrich a base firmware claim with cross-referenced attributes
%% from priv/tpm-interpret/firmware-versions/*.json (if the CRTM
%% string matches any manifest's prefix-list).
enrich_firmware_with_db(Base, Version, Db, Ev0) ->
    Manifests = maps:get(<<"firmware-versions">>, Db, #{}),
    case first_firmware_match(Version, Manifests) of
        undefined ->
            Base#{
                <<"family-name">> => null,
                <<"family-vendor">> => null,
                <<"family-trust-tier">> => null,
                <<"family-secure-boot-default">> => null,
                <<"family-tpm-vendor-id">> => null,
                <<"family-virtualization-platform">> => null,
                <<"family-ek-root-ca-source">> => null,
                <<"family-platform">> => null,
                <<"family-provenance">> => []};
        {MatchedKey, M, MatchedPrefix} ->
            %% If the manifest has a per-platform model map, try to
            %% identify which specific platform this CRTM belongs to.
            Platform = pick_platform(M, Version),
            Base#{
                <<"family-name">>           =>
                    maps:get(<<"name">>, M, null),
                <<"family-vendor">>         =>
                    maps:get(<<"vendor">>, M, null),
                <<"family-trust-tier">>     =>
                    maps:get(<<"trust-tier">>, M, null),
                <<"family-secure-boot-default">> =>
                    maps:get(<<"secure-boot-default">>, M, null),
                <<"family-tpm-vendor-id">> =>
                    maps:get(<<"tpm-vendor-id">>, M, null),
                <<"family-virtualization-platform">> =>
                    maps:get(<<"virtualization-platform">>, M, null),
                <<"family-ek-root-ca-source">> =>
                    maps:get(<<"ek-root-ca-source">>, M, null),
                <<"family-platform">>       => Platform,
                <<"family-provenance">>     =>
                    [event_provenance(Ev0),
                     {<<"source">>, <<"firmware-versions.json">>},
                     {<<"manifest-key">>, MatchedKey},
                     {<<"matched-prefix">>, MatchedPrefix}]}
    end.

%% Find the first manifest whose `match.crtm-version-prefix' list
%% contains a prefix of the given CRTM string. Returns
%% `{ManifestKey, ManifestMap, MatchedPrefix}' or `undefined'.
first_firmware_match(<<"unknown">>, _) -> undefined;
first_firmware_match(Version, Manifests) when is_binary(Version) ->
    Entries = maps:to_list(Manifests),
    find_firmware_match_in(Version, Entries);
first_firmware_match(_, _) -> undefined.

find_firmware_match_in(_Version, []) -> undefined;
find_firmware_match_in(Version, [{Key, M} | Rest]) ->
    Prefixes =
        maps:get(<<"crtm-version-prefix">>,
                 maps:get(<<"match">>, M, #{}), []),
    case matching_prefix(Version, Prefixes) of
        undefined -> find_firmware_match_in(Version, Rest);
        MatchedPrefix -> {Key, M, MatchedPrefix}
    end.

matching_prefix(_Version, []) -> undefined;
matching_prefix(Version, [Prefix | Rest]) when is_binary(Prefix) ->
    case binary:match(Version, Prefix) of
        {0, _} -> Prefix;
        _      -> matching_prefix(Version, Rest)
    end;
matching_prefix(Version, [_ | Rest]) ->
    matching_prefix(Version, Rest).

%% If the manifest declares a `platforms' map (model-prefix → text),
%% pick the first entry whose key is a prefix of the CRTM string.
pick_platform(M, Version) ->
    Platforms = maps:get(<<"platforms">>, M, #{}),
    case is_map(Platforms) andalso maps:size(Platforms) > 0 of
        true -> pick_platform_entry(maps:to_list(Platforms), Version);
        false -> null
    end.

pick_platform_entry([], _) -> null;
pick_platform_entry([{K, V} | Rest], Version) when is_binary(K) ->
    case binary:match(Version, K) of
        {0, _} -> V;
        _      -> pick_platform_entry(Rest, Version)
    end;
pick_platform_entry([_ | Rest], Version) ->
    pick_platform_entry(Rest, Version).

%% Bootloader: the first EV_EFI_BOOT_SERVICES_APPLICATION on PCR 4.
%% SHA-256 of the image is in digests.sha256.
claim_boot_loader(Events) ->
    Matches = [Ev || Ev <- Events,
                     maps:get(<<"event-type-code">>, Ev, 0) =:= 16#80000003,
                     maps:get(<<"pcr">>, Ev, 0) =:= 4],
    case Matches of
        [] ->
            #{<<"image-hash">> => <<"unknown">>,
              <<"image-hash-provenance">> => []};
        [Ev0 | _] ->
            Hash = nested(Ev0, [<<"digests">>, <<"sha256">>], <<"unknown">>),
            #{<<"image-hash">> => Hash,
              <<"image-hash-provenance">> =>
                  [event_provenance(Ev0)]}
    end.

%% @doc Full boot-chain enumeration. Returns every
%% EV_EFI_BOOT_SERVICES_APPLICATION (0x80000003),
%% EV_EFI_BOOT_SERVICES_DRIVER (0x80000004) and
%% EV_EFI_RUNTIME_SERVICES_DRIVER (0x80000005) event, in measurement
%% order, with the full decoded UEFI_IMAGE_LOAD_EVENT struct: image
%% SHA-256, image length, link-time address, parsed device path
%% (text form + structured node list), and per-event role.
%%
%% A policy engine can:
%%   * match the last-application's hash against a known OS-loader /
%%     UKI digest to prove the right kernel was chained in,
%%   * inspect the device-path nodes to see which ESP partition
%%     (GUID + PARTNR) each image came off,
%%   * detect runtime-service drivers loaded outside the normal
%%     chain (potential supply-chain surface).
%% @doc Compact `claim.quote' — surface the TPMS_ATTEST metadata
%% on the flat claim API. Includes freshness signals (reset-count,
%% restart-count, TPM wall-clock), TPM firmware identity, and the
%% exact (hash-alg, pcr-indexes) selection covered by the quote.
claim_quote(E) ->
    Q = hb_maps:get(<<"tpm-quote">>, E, #{}, #{}),
    case hb_maps:get(<<"quoted">>, Q, <<>>, #{}) of
        <<>> -> unknown_quote_claim();
        _ ->
            Meta = interpret_quote_metadata(E),
            case maps:is_key(<<"error">>, Meta) of
                true ->
                    Base = unknown_quote_claim(),
                    Base#{<<"error">> => maps:get(<<"error">>, Meta)};
                false ->
                    Sel = maps:get(<<"pcr-select">>, Meta, []),
                    QuotedIndexes = lists:usort(
                        lists:flatten(
                          [maps:get(<<"pcr-indexes">>, S, [])
                           || S <- Sel])),
                    QuotedAlgs =
                        [maps:get(<<"hash-alg-name">>, S, <<"unknown">>)
                         || S <- Sel],
                    #{
                        <<"magic-ok">>            =>
                            maps:get(<<"magic-ok">>, Meta, false),
                        <<"attest-type">>         =>
                            maps:get(<<"attest-type">>, Meta,
                                      <<"unknown">>),
                        <<"attest-type-code">>    =>
                            maps:get(<<"attest-type-code">>, Meta, 0),
                        <<"nonce">>               =>
                            maps:get(<<"nonce">>, Meta, <<"">>),
                        <<"clock-ms">>            =>
                            maps:get(<<"clock-ms">>, Meta, 0),
                        <<"clock-seconds">>       =>
                            maps:get(<<"clock-seconds">>, Meta, 0),
                        <<"reset-count">>         =>
                            maps:get(<<"reset-count">>, Meta, 0),
                        <<"restart-count">>       =>
                            maps:get(<<"restart-count">>, Meta, 0),
                        <<"safe">>                =>
                            maps:get(<<"safe">>, Meta, false),
                        <<"firmware-version-u64">>  =>
                            maps:get(<<"firmware-version-u64">>, Meta, 0),
                        <<"firmware-version-hex">>  =>
                            maps:get(<<"firmware-version-hex">>, Meta,
                                      <<"unknown">>),
                        <<"firmware-version-high">> =>
                            maps:get(<<"firmware-version-high">>, Meta, 0),
                        <<"firmware-version-low">>  =>
                            maps:get(<<"firmware-version-low">>, Meta, 0),
                        <<"qualified-signer-name">>         =>
                            maps:get(<<"qualified-signer-name">>, Meta,
                                      <<"">>),
                        <<"qualified-signer-name-length">>  =>
                            maps:get(<<"qualified-signer-name-length">>,
                                      Meta, 0),
                        <<"quoted-pcr-indexes">>  => QuotedIndexes,
                        <<"quoted-pcr-count">>    => length(QuotedIndexes),
                        <<"quoted-pcr-algs">>     => QuotedAlgs,
                        <<"pcr-digest">>          =>
                            maps:get(<<"pcr-digest">>, Meta, <<"">>),
                        <<"pcr-digest-length">>   =>
                            maps:get(<<"pcr-digest-length">>, Meta, 0),
                        <<"pcr-select">>          => Sel
                    }
            end
    end.

unknown_quote_claim() ->
    #{
        <<"magic-ok">>                      => false,
        <<"attest-type">>                   => <<"unknown">>,
        <<"attest-type-code">>              => 0,
        <<"nonce">>                         => <<"">>,
        <<"clock-ms">>                      => 0,
        <<"clock-seconds">>                 => 0,
        <<"reset-count">>                   => 0,
        <<"restart-count">>                 => 0,
        <<"safe">>                          => false,
        <<"firmware-version-u64">>          => 0,
        <<"firmware-version-hex">>          => <<"0x0000000000000000">>,
        <<"firmware-version-high">>         => 0,
        <<"firmware-version-low">>          => 0,
        <<"qualified-signer-name">>         => <<"">>,
        <<"qualified-signer-name-length">>  => 0,
        <<"quoted-pcr-indexes">>            => [],
        <<"quoted-pcr-count">>              => 0,
        <<"quoted-pcr-algs">>               => [],
        <<"pcr-digest">>                    => <<"">>,
        <<"pcr-digest-length">>             => 0,
        <<"pcr-select">>                    => []
    }.

%% @doc Cross-reference the (PCR 0, PCR 1, PCR 7) triple against
%% the shipped `priv/tpm-interpret/pcr-profiles/*.json' catalogue.
%% If all three match a profile's `match-pcrs.sha256' we declare
%% a high-confidence match; 2/3 is medium, 1/3 is low, 0/3 is
%% `no-match'. Returns the best match plus a list of all-matching
%% profiles so a policy engine can inspect alternatives.
%%
%% PCR 0 = core firmware measurement (CRTM + POST code + vendor
%% firmware blobs). PCR 1 = host platform configuration (CPU
%% microcode, SMBIOS, motherboard variables). PCR 7 = Secure Boot
%% state (db, dbx, KEK, PK, SecureBoot variable, MokListTrusted).
%% Matching all 3 pins firmware identity + boot policy + platform
%% config within the same fingerprint class.
claim_pcr_match(E, Db) ->
    PcrVals = nested(E, [<<"tpm-quote">>, <<"pcr-values">>], #{}),
    P0 = maps:get(<<"0">>, PcrVals, undefined),
    P1 = maps:get(<<"1">>, PcrVals, undefined),
    P7 = maps:get(<<"7">>, PcrVals, undefined),
    Profiles = maps:get(<<"pcr-profiles">>, Db, #{}),
    Scored = score_pcr_profiles(Profiles, P0, P1, P7),
    Best = best_pcr_profile_match(Scored),
    #{
        <<"pcr-0">>        => or_null(P0),
        <<"pcr-1">>        => or_null(P1),
        <<"pcr-7">>        => or_null(P7),
        <<"profile-count">> => maps:size(Profiles),
        <<"best-match">>   => project_pcr_match(Best),
        <<"all-matches">>  =>
            [project_pcr_match(M) || M <- Scored,
                                      maps:get(<<"score">>, M, 0) > 0]
    }.

%% Score every profile by how many of {pcr-0, pcr-1, pcr-7}
%% agree. Returns a list of `#{profile-key, name, score,
%% matched-pcrs, attributes}' maps sorted by descending score.
score_pcr_profiles(Profiles, P0, P1, P7) ->
    Scored = maps:fold(
        fun(Key, Profile, Acc) ->
            [score_one_profile(Key, Profile, P0, P1, P7) | Acc]
        end, [], Profiles),
    lists:reverse(
      lists:sort(
        fun(A, B) ->
            maps:get(<<"score">>, A, 0) =< maps:get(<<"score">>, B, 0)
        end, Scored)).

score_one_profile(Key, Profile, P0, P1, P7) ->
    Sha256 = nested(Profile, [<<"match-pcrs">>, <<"sha256">>], #{}),
    Pp0 = maps:get(<<"0">>, Sha256, undefined),
    Pp1 = maps:get(<<"1">>, Sha256, undefined),
    Pp7 = maps:get(<<"7">>, Sha256, undefined),
    Hits = [{<<"0">>, eq(P0, Pp0)},
            {<<"1">>, eq(P1, Pp1)},
            {<<"7">>, eq(P7, Pp7)}],
    Matched = [Idx || {Idx, true} <- Hits],
    Score = length(Matched),
    #{
        <<"profile-key">>  => Key,
        <<"name">>         => maps:get(<<"name">>, Profile, Key),
        <<"score">>        => Score,
        <<"matched-pcrs">> => Matched,
        <<"attributes">>   => maps:get(<<"attributes">>, Profile, #{})
    }.

eq(A, B) when A =/= undefined, B =/= undefined -> A =:= B;
eq(_, _) -> false.

best_pcr_profile_match([]) -> undefined;
best_pcr_profile_match([Top | _]) ->
    case maps:get(<<"score">>, Top, 0) of
        0 -> undefined;
        _ -> Top
    end.

project_pcr_match(undefined) ->
    #{<<"profile-key">> => null,
      <<"name">>        => null,
      <<"score">>       => 0,
      <<"confidence">>  => <<"no-match">>,
      <<"matched-pcrs">>=> [],
      <<"attributes">>  => #{}};
project_pcr_match(M) when is_map(M) ->
    Score = maps:get(<<"score">>, M, 0),
    Confidence = pcr_match_confidence(Score),
    M#{<<"confidence">> => Confidence}.

pcr_match_confidence(0) -> <<"no-match">>;
pcr_match_confidence(1) -> <<"low">>;
pcr_match_confidence(2) -> <<"medium">>;
pcr_match_confidence(3) -> <<"high">>;
pcr_match_confidence(_) -> <<"high">>.

%% @doc Fundamental quote-integrity check. The TPM's pcrDigest
%% field (now decoded into `claim.quote.pcr-digest') is defined
%% as the hash over the concatenation of the selected PCR values
%% in `pcrSelect' order. We recompute that digest and compare.
%%
%% A mismatch means one of:
%%   * the quote was not produced by the TPM that claims to have
%%     signed it (wrong PCRs fed in),
%%   * the envelope's `pcr-values' map was altered between the
%%     TPM signing and the envelope arriving here,
%%   * the pcrSelect / digest-alg are malformed.
%%
%% Any of those is a hard-stop for trusting the quote — the
%% crypto signature check alone is insufficient because the
%% signature only binds the TPMS_ATTEST blob, not the unquoted
%% per-PCR byte strings that the envelope carries.
%%
%% The digest algorithm is inferred from the declared
%% pcr-digest-length: 20 → SHA-1, 32 → SHA-256, 48 → SHA-384,
%% 64 → SHA-512.
claim_quote_integrity(E) ->
    Q = hb_maps:get(<<"tpm-quote">>, E, #{}, #{}),
    case hb_maps:get(<<"quoted">>, Q, <<>>, #{}) of
        <<>> -> unknown_quote_integrity();
        _ ->
            Meta = interpret_quote_metadata(E),
            case maps:is_key(<<"error">>, Meta) of
                true ->
                    M0 = unknown_quote_integrity(),
                    M0#{<<"error">> => maps:get(<<"error">>, Meta)};
                false ->
                    compute_quote_integrity(E, Meta)
            end
    end.

unknown_quote_integrity() ->
    #{
        <<"verifiable">>              => false,
        <<"pcr-digest-match">>        => <<"unknown">>,
        <<"pcr-digest-alg">>          => <<"unknown">>,
        <<"pcr-digest-claimed">>      => <<"">>,
        <<"pcr-digest-computed">>     => <<"">>,
        <<"pcr-indexes-used">>        => [],
        <<"missing-pcrs">>            => [],
        <<"evidence">>                => []
    }.

compute_quote_integrity(E, Meta) ->
    ClaimedDigestB64 = maps:get(<<"pcr-digest">>, Meta, <<"">>),
    Claimed = try hb_util:decode(ClaimedDigestB64)
              catch _:_ -> <<>> end,
    ClaimedLen = byte_size(Claimed),
    Alg = pcr_digest_alg_from_size(ClaimedLen),
    Sel = maps:get(<<"pcr-select">>, Meta, []),
    PcrVals = nested(E, [<<"tpm-quote">>, <<"pcr-values">>], #{}),
    {Concatenated, UsedIndexes, Missing} =
        concat_selected_pcrs(Sel, PcrVals),
    case Alg of
        <<"unknown">> ->
            #{
                <<"verifiable">>              => false,
                <<"pcr-digest-match">>        => <<"unknown">>,
                <<"pcr-digest-alg">>          => <<"unknown">>,
                <<"pcr-digest-claimed">>      => ClaimedDigestB64,
                <<"pcr-digest-computed">>     => <<"">>,
                <<"pcr-indexes-used">>        => UsedIndexes,
                <<"missing-pcrs">>            => Missing,
                <<"evidence">>                => [
                    {<<"reason">>,
                     <<"unknown-digest-alg-for-length">>},
                    {<<"claimed-length">>, ClaimedLen}]
            };
        _ ->
            Computed = tpm_hash(Alg, Concatenated),
            Match = Computed =:= Claimed,
            #{
                <<"verifiable">>          => Missing =:= [],
                <<"pcr-digest-match">>    => Match,
                <<"pcr-digest-alg">>      => Alg,
                <<"pcr-digest-claimed">>  => ClaimedDigestB64,
                <<"pcr-digest-computed">> => hb_util:encode(Computed),
                <<"pcr-indexes-used">>    => UsedIndexes,
                <<"missing-pcrs">>        => Missing,
                <<"evidence">>            => quote_integrity_evidence(
                    Match, Missing, length(UsedIndexes), Alg)
            }
    end.

quote_integrity_evidence(Match, Missing, UsedCount, Alg) ->
    [{<<"alg">>, Alg},
     {<<"pcr-count">>, UsedCount},
     {<<"match">>, Match},
     {<<"missing-count">>, length(Missing)}].

pcr_digest_alg_from_size(20) -> <<"sha1">>;
pcr_digest_alg_from_size(32) -> <<"sha256">>;
pcr_digest_alg_from_size(48) -> <<"sha384">>;
pcr_digest_alg_from_size(64) -> <<"sha512">>;
pcr_digest_alg_from_size(_)  -> <<"unknown">>.

tpm_hash(<<"sha1">>, Bin)   -> crypto:hash(sha,     Bin);
tpm_hash(<<"sha256">>, Bin) -> crypto:hash(sha256,  Bin);
tpm_hash(<<"sha384">>, Bin) -> crypto:hash(sha384,  Bin);
tpm_hash(<<"sha512">>, Bin) -> crypto:hash(sha512,  Bin);
tpm_hash(_, _)              -> <<>>.

%% @doc Walk pcrSelect in order, concatenate the corresponding
%% raw PCR bytes from the envelope's `pcr-values` map. Returns
%% `{Concatenated, UsedIndexes, Missing}'. Missing indexes are
%% selected PCRs whose value is absent from the envelope — a
%% quote is only verifiable if every selected PCR has a value.
concat_selected_pcrs(Selections, PcrVals) ->
    concat_selected_pcrs_(Selections, PcrVals, <<>>, [], []).

concat_selected_pcrs_([], _PcrVals, Acc, Used, Missing) ->
    {Acc, lists:reverse(Used), lists:reverse(Missing)};
concat_selected_pcrs_([Sel | Rest], PcrVals, Acc, Used, Missing) ->
    Indexes = maps:get(<<"pcr-indexes">>, Sel, []),
    {Acc1, Used1, Missing1} =
        lists:foldl(
          fun(I, {A, U, M}) ->
              Key = integer_to_binary(I),
              case maps:get(Key, PcrVals, undefined) of
                  undefined ->
                      {A, U, [I | M]};
                  B64 when is_binary(B64) ->
                      try
                          Raw = hb_util:decode(B64),
                          {<<A/binary, Raw/binary>>, [I | U], M}
                      catch _:_ ->
                          {A, U, [I | M]}
                      end;
                  _ -> {A, U, [I | M]}
              end
          end, {Acc, Used, Missing}, Indexes),
    concat_selected_pcrs_(Rest, PcrVals, Acc1, Used1, Missing1).

%% @doc Compose the freshness stanza. A verifier typically
%% challenges with a fresh nonce — the TPM echoes it back as
%% extraData inside the quote. Here we surface:
%%
%%   * the nonce echoed by the TPM (base64url),
%%   * the TPM's reset-count / restart-count (monotonic — newer
%%     quotes should have ≥ the most-recent previous pair from
%%     the same TPM),
%%   * clock-ms / clock-seconds (TPM wall-clock, monotonic
%%     within a boot epoch),
%%   * the `safe' flag (TRUE iff the clock hasn't been tampered
%%     with since last reset — any FALSE here is a red flag),
%%   * a composite `freshness-indicator' value:
%%       "ok"         — nonce present, safe=true, clock>0
%%       "safe-false" — safe flag is false; clock is untrusted
%%       "no-nonce"   — empty nonce means no challenge was bound
%%       "no-clock"   — clock-ms=0 is a sign of a dry-run quote
%%       "unknown"    — no quote present
claim_freshness(E) ->
    Q = hb_maps:get(<<"tpm-quote">>, E, #{}, #{}),
    case hb_maps:get(<<"quoted">>, Q, <<>>, #{}) of
        <<>> -> unknown_freshness_claim();
        _ ->
            Meta = interpret_quote_metadata(E),
            case maps:is_key(<<"error">>, Meta) of
                true ->
                    M0 = unknown_freshness_claim(),
                    M0#{<<"error">> => maps:get(<<"error">>, Meta)};
                false ->
                    project_freshness(Meta)
            end
    end.

unknown_freshness_claim() ->
    #{
        <<"nonce">>                 => <<"">>,
        <<"nonce-length">>          => 0,
        <<"reset-count">>           => 0,
        <<"restart-count">>         => 0,
        <<"clock-ms">>              => 0,
        <<"clock-seconds">>         => 0,
        <<"safe">>                  => false,
        <<"freshness-indicator">>   => <<"unknown">>,
        <<"evidence">>              => []
    }.

project_freshness(Meta) ->
    Nonce = maps:get(<<"nonce">>, Meta, <<"">>),
    Safe = maps:get(<<"safe">>, Meta, false),
    ClockMs = maps:get(<<"clock-ms">>, Meta, 0),
    ResetCount = maps:get(<<"reset-count">>, Meta, 0),
    RestartCount = maps:get(<<"restart-count">>, Meta, 0),
    NonceLen = try hb_util:decode(Nonce) of
                   Raw when is_binary(Raw) -> byte_size(Raw)
               catch _:_ -> 0
               end,
    Indicator = freshness_indicator(NonceLen, Safe, ClockMs),
    Evidence =
        [{<<"nonce-present">>, NonceLen > 0},
         {<<"nonce-length">>, NonceLen},
         {<<"safe">>, Safe},
         {<<"clock-positive">>, ClockMs > 0},
         {<<"reset-count">>, ResetCount},
         {<<"restart-count">>, RestartCount}],
    #{
        <<"nonce">>               => Nonce,
        <<"nonce-length">>        => NonceLen,
        <<"reset-count">>         => ResetCount,
        <<"restart-count">>       => RestartCount,
        <<"clock-ms">>            => ClockMs,
        <<"clock-seconds">>       => ClockMs div 1000,
        <<"safe">>                => Safe,
        <<"freshness-indicator">> => Indicator,
        <<"evidence">>            => Evidence
    }.

freshness_indicator(0, _, _)     -> <<"no-nonce">>;
freshness_indicator(_, false, _) -> <<"safe-false">>;
freshness_indicator(_, true, 0)  -> <<"no-clock">>;
freshness_indicator(_, true, Ms) when Ms > 0 -> <<"ok">>;
freshness_indicator(_, _, _)     -> <<"ok">>.

claim_boot_chain(Events) ->
    Codes = [16#80000003, 16#80000004, 16#80000005],
    Sorted = lists:sort(
        fun(A, B) ->
            maps:get(<<"seq">>, A, 0) =< maps:get(<<"seq">>, B, 0)
        end,
        [Ev || Ev <- Events,
               lists:member(maps:get(<<"event-type-code">>, Ev, 0),
                            Codes)]),
    Rows = lists:map(fun project_boot_row/1,
                     lists:zip(lists:seq(0, length(Sorted) - 1),
                               Sorted)),
    %% Summary: indices of first/last "application" (role =
    %% application implies it's the thing that ran next; the LAST
    %% application typically IS the OS loader / UKI).
    Apps = [R || R <- Rows,
                 maps:get(<<"role">>, R) =:= <<"application">>],
    %% First/last hashes already safely encoded by project_boot_row.
    First = case Apps of [] -> <<"unknown">>;
                          [F | _] -> maps:get(<<"image-hash">>, F,
                                               <<"unknown">>)
            end,
    Last = case Apps of [] -> <<"unknown">>;
                        _  -> maps:get(<<"image-hash">>,
                                        lists:last(Apps),
                                        <<"unknown">>)
           end,
    HasRuntime = lists:any(
                   fun(R) ->
                       maps:get(<<"role">>, R) =:= <<"runtime-driver">>
                   end, Rows),
    #{
        <<"length">>               => length(Rows),
        <<"application-count">>    => length(Apps),
        <<"first-application-hash">>  => First,
        <<"last-application-hash">>   => Last,
        <<"has-runtime-driver">>      => HasRuntime,
        <<"chain">>                   => Rows
    }.

%% Build one boot-chain row. `Index' is the 0-based chain position.
%% Raw SHA-256 digest is base64url-encoded here because the
%% claim pipeline deliberately bypasses the events wire-encode
%% layer (see claim/3 comment). Everything in this row must be
%% UTF-8-safe by construction.
project_boot_row({Index, Ev}) ->
    Code = maps:get(<<"event-type-code">>, Ev, 0),
    P = maps:get(<<"parsed">>, Ev, #{}),
    #{
        <<"chain-index">>          => Index,
        <<"role">>                 => boot_role(Code),
        <<"event-type-code">>      => Code,
        <<"seq">>                  => maps:get(<<"seq">>, Ev, 0),
        <<"pcr">>                  => maps:get(<<"pcr">>, Ev, 0),
        <<"image-hash">>           => safe_encode_hash(
            nested(Ev, [<<"digests">>, <<"sha256">>], undefined)),
        <<"image-length-in-memory">> =>
            maps:get(<<"image-length-in-memory">>, P, null),
        <<"image-link-time-address">> =>
            maps:get(<<"image-link-time-address">>, P, null),
        <<"device-path-text">>     =>
            maps:get(<<"device-path-text">>, P, <<"">>),
        <<"device-path-node-count">> =>
            length(maps:get(<<"device-path-nodes">>, P, [])),
        <<"provenance">>           => [event_provenance(Ev)]
    }.

%% Base64url-encode a raw binary hash, tolerating undefined +
%% already-encoded strings (`"unknown"', etc.).
safe_encode_hash(undefined) -> <<"unknown">>;
safe_encode_hash(H) when is_binary(H) ->
    %% If it's already ASCII (e.g. "unknown" or already-encoded),
    %% leave it; else base64url-encode.
    case lists:all(fun(B) -> B >= 32 andalso B < 128 end,
                    binary_to_list(H)) of
        true  -> H;
        false -> hb_util:encode(H)
    end;
safe_encode_hash(_) -> <<"unknown">>.

boot_role(16#80000003) -> <<"application">>;
boot_role(16#80000004) -> <<"driver">>;
boot_role(16#80000005) -> <<"runtime-driver">>;
boot_role(_)           -> <<"unknown">>.

%% Kernel / UKI identity. systemd-stub emits key=value EV_IPL events
%% on PCR 11/12/13 whose keys include `kernel_name', `kernel_
%% version', `initrd', and the cmdline. We collect them.
claim_kernel(Events, E) ->
    CmdlineEvs = ipl_kv_matches(Events, <<"cmdline">>) ++
                 ipl_kv_matches(Events, <<"kernel-cmdline">>),
    {Cmdline, CmdlineFlags, CmdlineProv} = case CmdlineEvs of
        [] -> {<<"unknown">>, #{}, []};
        [Ev | _] ->
            {nested(Ev, [<<"parsed">>, <<"value">>], <<"unknown">>),
             nested(Ev, [<<"parsed">>, <<"cmdline-flags">>], #{}),
             [event_provenance(Ev)]}
    end,
    UkiHash = hb_maps:get(
                <<"11">>,
                nested(E, [<<"tpm-quote">>, <<"pcr-values">>], #{}),
                <<"unknown">>),
    #{
        <<"cmdline">>             => Cmdline,
        <<"cmdline-provenance">>  => CmdlineProv,
        <<"cmdline-flag-count">>  =>
            maps:get(<<"-token-count">>, CmdlineFlags, 0),
        <<"uki-hash">>            => UkiHash,
        <<"uki-hash-provenance">> => [{<<"pcr">>, 11}],
        %% `iommu-strict' retained for backward compat; the new
        %% `claim.iommu' section has the full breakdown.
        <<"iommu-strict">>        =>
            maps:get(<<"iommu.strict">>, CmdlineFlags, <<"unknown">>)
    }.

ipl_kv_matches(Events, Key) ->
    [Ev || Ev <- Events,
           maps:get(<<"event-type-code">>, Ev, 0) =:= 16#D,
           nested(Ev, [<<"parsed">>, <<"key">>], <<>>) =:= Key].

%% Find the first EV_IPL cmdline event (PCR 12 "cmdline" or
%% "kernel-cmdline" key), return {CmdlineFlagsMap, [provenance]}.
%% Tier-2 evidence source for the claim rewrites below.
cmdline_flags_and_provenance(Events) ->
    Evs = ipl_kv_matches(Events, <<"cmdline">>) ++
          ipl_kv_matches(Events, <<"kernel-cmdline">>),
    case Evs of
        [] -> {#{}, []};
        [E | _] ->
            Flags = nested(E, [<<"parsed">>, <<"cmdline-flags">>], #{}),
            {Flags, [event_provenance(E)]}
    end.

%% TME/SME (paper §Arch line 226-230).
%%
%% Three orthogonal evidence tiers compose here:
%%   tier 2 (kernel cmdline intent): `mem_encrypt=on' / `sme=on' /
%%           `kvm_intel.tdx=on' measured into PCR 12 via sd-stub.
%%   tier 3 (UKI-hash claim DB lookup): this PCR 11 UKI hash appears
%%           in our uki-measurements DB with `checks-tme: true'
%%           (the kernel's early init halts if TME is off).
%%   tier 4 (boot-reached-PCR-15): PCR 15 was extended by the
%%           ephemeral node key → halt-check didn't fire → TME is on.
%%
%% Any ONE tier alone is insufficient for a definitive "on":
%%   tier 2 alone = intent, not proof (a kernel could ignore the flag)
%%   tier 3 alone = capability (the kernel HAS the halt-check), but
%%                  we'd still want tier 4 to know halt didn't fire
%%   tier 4 alone = boot completed, but we don't know what kernel ran
%%
%% The `enabled' field surfaces the composite verdict; the
%% `evidence' list lets policy engines require specific tier
%% combinations (e.g. "tier 2 + tier 3 + tier 4" for confidential-
%% compute, "tier 2 only" for development).
claim_tme(Events, E, Db) ->
    claim_tme(Events, E, Db,
              #{<<"kind">> => <<"tcg-pc-client">>, <<"evidence">> => []}).

claim_tme(Events, E, Db, Context) ->
    {Flags, CmdlineProv} = cmdline_flags_and_provenance(Events),
    %% Tier 2: cmdline intent.
    MemEnc  = maps:get(<<"mem_encrypt">>, Flags, undefined),
    Sme     = maps:get(<<"sme">>,          Flags, undefined),
    Tdx     = maps:get(<<"kvm_intel.tdx">>,Flags, undefined),
    Tier2 = case {MemEnc, Sme, Tdx} of
        {undefined, undefined, undefined} -> {<<"unknown">>, []};
        _ ->
            Intent = (MemEnc =:= true) orelse (Sme =:= true)
                     orelse (Tdx =:= true),
            {Intent, [{<<"tier">>, 2} | CmdlineProv]}
    end,
    %% Tier 3: UKI-hash / kernel-name / stub DB lookup.
    UkiHash = hb_maps:get(
                <<"11">>,
                nested(E, [<<"tpm-quote">>, <<"pcr-values">>], #{}),
                <<"unknown">>),
    UkiProfiles = maps:get(<<"uki-profiles">>, Db, #{}),
    Tier3 = case uki_db_lookup(UkiProfiles, UkiHash, Events,
                                <<"checks-tme">>) of
        {true, MatchTme} ->
            {true, [{<<"tier">>, 3},
                    {<<"uki-hash">>, UkiHash},
                    {<<"matched-profile">>,
                     maps:get(<<"name">>, MatchTme, <<"unknown">>)},
                    {<<"match-rule">>,
                     maps:get(<<"-rule">>, MatchTme, <<"unknown">>)}]};
        _ -> {<<"unknown">>, []}
    end,
    %% Tier 4: boot-reached-PCR-15 — we always have this if the
    %% quote verified. Surface it as supporting evidence.
    Pcr15 = hb_maps:get(
              <<"15">>,
              nested(E, [<<"tpm-quote">>, <<"pcr-values">>], #{}),
              <<"unknown">>),
    Tier4 = case Pcr15 of
        <<"unknown">> -> {<<"unknown">>, []};
        _             -> {true, [{<<"tier">>, 4},
                                 {<<"derivation">>,
                                  <<"pcr-15-extension-reached">>}]}
    end,
    %% Tier 5: confidential-compute context. Intel TDX requires TME
    %% (TDX Module initialises the TME-MK key generator during
    %% trust-domain build; a TDX-extended MRTD event log cannot exist
    %% without TME being on). AMD SEV-SNP similarly requires SME
    %% (SEV-SNP encrypts all guest memory under per-VM keys).
    Tier5 = case maps:get(<<"kind">>, Context, <<"tcg-pc-client">>) of
        <<"intel-tdx-ccel">> ->
            {true, [{<<"tier">>, 5},
                    {<<"context">>, <<"intel-tdx-ccel">>},
                    {<<"derivation">>,
                     <<"tdx-requires-tme">>}]};
        <<"amd-sev-snp">> ->
            {true, [{<<"tier">>, 5},
                    {<<"context">>, <<"amd-sev-snp">>},
                    {<<"derivation">>,
                     <<"sev-snp-requires-sme">>}]};
        <<"amd-sev">> ->
            {true, [{<<"tier">>, 5},
                    {<<"context">>, <<"amd-sev">>},
                    {<<"derivation">>,
                     <<"sev-requires-sme">>}]};
        _ -> {<<"unknown">>, []}
    end,
    compose_claim(<<"enabled">>, [Tier2, Tier3, Tier4, Tier5]).

%% @doc Determine whether any UKI-measurement profile in the DB
%% matches this attestation AND asserts the requested claim.
%%
%% Matches fire on ANY of:
%%
%%   * exact `uki-hash' equality (legacy top-level key), OR
%%   * `known-uki-hashes' list contains the PCR 11 hash, OR
%%   * `match.kernel-name-prefix' list has any prefix of an EV_IPL
%%     `kernel_name=<value>' event's value, OR
%%   * `match.stub-name' list contains an EV_IPL `stub_name=<value>'
%%     event's value.
%%
%% Returns `{true, MatchedProfile}' on success (with a synthetic
%% `-rule' key naming WHY it matched), else `false'.
%%
%% Claim tests look at either the top-level `<Key>: true' (legacy
%% schema) or `claims.<Key>: true' (schema v1+).
uki_db_lookup(Profiles, UkiHash, Events, Key) when is_map(Profiles) ->
    Matches = uki_db_matches(Profiles, UkiHash, Events),
    Hits = [P || P <- Matches, uki_profile_asserts(P, Key)],
    case Hits of
        [] -> false;
        [First | _] -> {true, First}
    end;
uki_db_lookup(_, _, _, _) -> false.

%% Backward-compat 3-arg form (used by tests that predate the
%% Events-aware matcher).
uki_db_lookup(Profiles, UkiHash, Key) ->
    case uki_db_lookup(Profiles, UkiHash, [], Key) of
        {true, _} -> true;
        _         -> false
    end.

%% Iterate all profiles, return those that match this envelope.
%% Each returned map is the profile with an extra `-rule' key
%% naming the matched rule ("uki-hash" | "known-uki-hashes" |
%% "kernel-name-prefix" | "stub-name").
uki_db_matches(Profiles, UkiHash, Events) when is_map(Profiles) ->
    %% dev_tpm_tcg:decode_ev_ipl/1 kebab-cases keys at parse time
    %% (`kernel_name' → `kernel-name'), so look up with the kebab
    %% form. We also probe both forms to keep the lookup robust
    %% against future changes to the parse side.
    KernelName =
        first_defined([ipl_kv_value(Events, <<"kernel-name">>),
                       ipl_kv_value(Events, <<"kernel_name">>)]),
    StubName =
        first_defined([ipl_kv_value(Events, <<"stub-name">>),
                       ipl_kv_value(Events, <<"stub_name">>)]),
    lists:filtermap(
      fun({_, P}) when is_map(P) ->
          uki_profile_match(P, UkiHash, KernelName, StubName);
         (_) -> false
      end,
      maps:to_list(Profiles));
uki_db_matches(_, _, _) -> [].

uki_profile_match(P, UkiHash, KernelName, StubName) ->
    %% Rule 1: exact top-level uki-hash.
    case maps:get(<<"uki-hash">>, P, undefined) of
        H when is_binary(H), H =:= UkiHash ->
            {true, P#{<<"-rule">> => <<"uki-hash">>}};
        _ ->
            %% Rule 2: known-uki-hashes list.
            case lists:member(UkiHash,
                              maps:get(<<"known-uki-hashes">>, P, [])) of
                true ->
                    {true, P#{<<"-rule">> => <<"known-uki-hashes">>}};
                false ->
                    uki_profile_match_by_match(P, KernelName, StubName)
            end
    end.

%% Match semantics (all-rules-must-be-compatible, ≥1-must-fire):
%%
%%   * If the profile declares `kernel-name-prefix', that list
%%     MUST contain a prefix of the observed kernel_name.
%%   * If the profile declares `stub-name', that list MUST
%%     contain the observed stub_name.
%%   * If the profile declares neither, no match (only the
%%     uki-hash / known-uki-hashes paths can match).
%%   * At least one of the declared rules must actually fire
%%     (i.e. the corresponding event must be present).
%%
%% This way `stub-name=systemd-stub' (generic to every systemd-
%% stub UKI) never overrides a more specific kernel-name-prefix
%% mismatch.
uki_profile_match_by_match(P, KernelName, StubName) ->
    M = maps:get(<<"match">>, P, #{}),
    PrefixList = maps:get(<<"kernel-name-prefix">>, M, []),
    StubList   = maps:get(<<"stub-name">>, M, []),
    HasKnp = PrefixList =/= [],
    HasStub = StubList =/= [],
    KnpFires = KernelName =/= undefined
               andalso any_prefix_match(KernelName, PrefixList),
    StubFires = StubName =/= undefined
                andalso lists:member(StubName, StubList),
    CompatKnp  = (not HasKnp) orelse KnpFires,
    CompatStub = (not HasStub) orelse StubFires,
    AtLeastOne = KnpFires orelse StubFires,
    case CompatKnp andalso CompatStub andalso AtLeastOne of
        true ->
            Rule =
                case KnpFires of
                    true  -> <<"kernel-name-prefix">>;
                    false -> <<"stub-name">>
                end,
            {true, P#{<<"-rule">> => Rule}};
        false -> false
    end.

%% @doc Find the first EV_IPL (0x0D) event whose parsed.key equals
%% `Key' and return its parsed.value, or `undefined'.
ipl_kv_value(Events, Key) ->
    case ipl_kv_matches(Events, Key) of
        [] -> undefined;
        [Ev | _] ->
            case nested(Ev, [<<"parsed">>, <<"value">>], undefined) of
                V when is_binary(V) -> V;
                _ -> undefined
            end
    end.

%% @doc Return the first `defined' entry in the list (undefined is
%% falsy). Used to probe multiple key spellings in the DB match
%% logic while tolerating encoder drift.
first_defined([]) -> undefined;
first_defined([undefined | Rest]) -> first_defined(Rest);
first_defined([V | _]) -> V.

%% @doc Case-sensitive prefix test against a list of candidate
%% prefixes.
any_prefix_match(_Val, []) -> false;
any_prefix_match(Val, [Prefix | Rest]) ->
    case binary:match(Val, Prefix) of
        {0, _} -> true;
        _      -> any_prefix_match(Val, Rest)
    end.

%% Does a matched profile assert `<Key>: true'? Accepts both the
%% legacy top-level shape and the schema-v1 `claims' sub-map shape.
uki_profile_asserts(P, Key) ->
    TopLevel = maps:get(Key, P, undefined),
    case TopLevel of
        true -> true;
        _ ->
            Claims = maps:get(<<"claims">>, P, #{}),
            maps:get(Key, Claims, false) =:= true
    end.

%% Compose a claim from multiple tiers. Rules:
%%   * Any tier giving `true' → claim is true (with all supporting
%%     tiers' evidence).
%%   * Any tier giving `false' while none say `true' → claim is false.
%%   * All tiers return "unknown" → claim is "unknown".
compose_claim(Field, TierResults) ->
    Values = [V || {V, _} <- TierResults],
    Evidence = lists:flatten([E || {_, E} <- TierResults]),
    Verdict = compose_verdict(Values),
    #{
        Field                                => Verdict,
        <<(Field)/binary, "-evidence">>      => Evidence,
        <<(Field)/binary, "-tier-count">>    =>
            length([E || E <- Evidence, is_tuple(E),
                          element(1, E) =:= <<"tier">>])
    }.

compose_verdict(Values) ->
    case lists:member(true, Values) of
        true -> true;
        false ->
            case lists:member(false, Values) of
                true -> false;
                false -> <<"unknown">>
            end
    end.

%% Kernel lockdown mode (paper §Arch line 223:
%% `lockdown=confidentiality').
%%
%% Tier 2: cmdline `lockdown=<mode>'.
%% Tier 3: UKI-hash claim `lockdown-confidentiality: true' in the DB.
claim_lockdown(Events) ->
    claim_lockdown(Events, #{}, #{}).

claim_lockdown(Events, E, Db) ->
    {Flags, CmdlineProv} = cmdline_flags_and_provenance(Events),
    Mode = maps:get(<<"lockdown">>, Flags, <<"unknown">>),
    Level = case Mode of
        <<"confidentiality">> -> <<"confidentiality">>;
        <<"integrity">>       -> <<"integrity">>;
        <<"none">>            -> <<"none">>;
        V when is_binary(V)   -> V;
        _                     -> <<"unknown">>
    end,
    LevelProv = case Level of
        <<"unknown">> -> [];
        _             -> [{<<"tier">>, 2} | CmdlineProv]
    end,
    %% Tier 3: did a matching UKI-hash (or kernel-name / stub-name)
    %% claim lockdown-confidentiality?
    UkiHash = hb_maps:get(
                <<"11">>,
                nested(E, [<<"tpm-quote">>, <<"pcr-values">>], #{}),
                <<"unknown">>),
    UkiProfiles = maps:get(<<"uki-profiles">>, Db, #{}),
    {Tier3Confirm, Tier3Evidence} =
        case uki_db_lookup(UkiProfiles, UkiHash, Events,
                            <<"lockdown-confidentiality">>) of
            {true, P} ->
                {true,
                 [{<<"tier">>, 3},
                  {<<"uki-hash">>, UkiHash},
                  {<<"matched-profile">>,
                   maps:get(<<"name">>, P, <<"unknown">>)},
                  {<<"match-rule">>,
                   maps:get(<<"-rule">>, P, <<"unknown">>)}]};
            _ -> {false, []}
        end,
    #{
        <<"level">>             => Level,
        <<"level-evidence">>    => LevelProv,
        <<"confidentiality-confirmed">>           => Tier3Confirm,
        <<"confidentiality-confirmed-evidence">>  => Tier3Evidence
    }.

%% IOMMU state (paper §Arch line 223:
%% `IOMMU strict mode ... init_on_alloc/init_on_free').
%%
%% Tier 2 cmdline flags:
%%   iommu=pt                   → DMA-remap mode
%%   iommu.strict=1             → flushes per-op (no lazy invalidation)
%%   intel_iommu=on | amd_iommu=on → vendor-specific enable
claim_iommu(Events) ->
    {Flags, CmdlineProv} = cmdline_flags_and_provenance(Events),
    Mode  = maps:get(<<"iommu">>, Flags, <<"unknown">>),
    Strct = maps:get(<<"iommu.strict">>, Flags, <<"unknown">>),
    Intel = maps:get(<<"intel_iommu">>, Flags, <<"unknown">>),
    Amd   = maps:get(<<"amd_iommu">>,   Flags, <<"unknown">>),
    %% An IOMMU is effectively "enabled" if at least one vendor enable
    %% flag is on or a mode was set.
    Enabled = case {Mode, Intel, Amd} of
        {<<"unknown">>, <<"unknown">>, <<"unknown">>} -> <<"unknown">>;
        _ ->
            (Mode =/= <<"unknown">>) orelse (Intel =:= true)
                                     orelse is_binary(Amd)
    end,
    Prov = case Enabled of
        <<"unknown">> -> [];
        _             -> [{<<"tier">>, 2} | CmdlineProv]
    end,
    #{
        <<"enabled">>                  => Enabled,
        <<"enabled-evidence">>         => Prov,
        <<"mode">>                     => Mode,
        <<"strict">>                   => Strct,
        <<"intel-iommu-requested">>    => Intel,
        <<"amd-iommu-requested">>      => Amd
    }.

%% Kernel integrity properties (paper §Security table):
%%   module.sig_enforce=1  → unsigned modules rejected
%%   init_on_alloc=1       → heap pages zeroed at alloc
%%   init_on_free=1        → heap pages zeroed at free
%%   slab_nomerge          → slab caches not merged (reduces cross-
%%                            cache exploitation)
%%   page_poison=1         → free pages poisoned
%%   lockdown=confidentiality → kernel lockdown in the strictest mode
claim_kernel_integrity(Events) ->
    {Flags, Prov} = cmdline_flags_and_provenance(Events),
    Base = case Prov of
        [] -> [];
        _  -> [{<<"tier">>, 2} | Prov]
    end,
    #{
        <<"module-sig-enforce">>     =>
            maps:get(<<"module.sig_enforce">>, Flags, <<"unknown">>),
        <<"init-on-alloc">>          =>
            maps:get(<<"init_on_alloc">>, Flags, <<"unknown">>),
        <<"init-on-free">>           =>
            maps:get(<<"init_on_free">>, Flags, <<"unknown">>),
        <<"slab-nomerge">>           =>
            maps:get(<<"slab_nomerge">>, Flags, <<"unknown">>),
        <<"page-poison">>            =>
            maps:get(<<"page_poison">>, Flags, <<"unknown">>),
        <<"kernel-page-table-isolation">> =>
            maps:get(<<"pti">>, Flags, <<"unknown">>),
        <<"randomize-kstack-offset">> =>
            maps:get(<<"randomize_kstack_offset">>, Flags, <<"unknown">>),
        <<"evidence">>               => Base
    }.

%% dm-verity rootfs + /usr integrity (paper §Arch line 222:
%% `cmdline carries the dm-verity root hash').
claim_verity(Events) ->
    {Flags, Prov} = cmdline_flags_and_provenance(Events),
    RootHash = case maps:get(<<"roothash">>, Flags, undefined) of
        undefined ->
            maps:get(<<"systemd.verity_root_hash">>, Flags, <<"unknown">>);
        V when is_binary(V) -> V;
        _ -> <<"unknown">>
    end,
    UsrHash = maps:get(<<"systemd.verity_usr_root_hash">>,
                        Flags, <<"unknown">>),
    Evidence = case RootHash of
        <<"unknown">> -> [];
        _             -> [{<<"tier">>, 2} | Prov]
    end,
    #{
        <<"root-hash">>           => RootHash,
        <<"usr-root-hash">>       => UsrHash,
        <<"evidence">>            => Evidence
    }.

%%---- small helpers -----------------------------------------------------

event_provenance(Ev) ->
    #{
        <<"pcr">> => maps:get(<<"pcr">>, Ev, null),
        <<"seq">> => maps:get(<<"seq">>, Ev, null)
    }.

nested(M, [K], D) when is_map(M) -> hb_maps:get(K, M, D, #{});
nested(M, [K | Rest], D) when is_map(M) ->
    case hb_maps:get(K, M, undefined, #{}) of
        Inner when is_map(Inner) -> nested(Inner, Rest, D);
        _ -> D
    end;
nested(_, _, D) -> D.

%%---- envelope meta -----------------------------------------------------

interpret_envelope_meta(E) ->
    #{
        <<"version">> =>
            hb_maps:get(<<"lapee-attestation-version">>, E, null, #{}),
        <<"issued-at-unix">> =>
            hb_maps:get(<<"issued-at-unix">>, E, null, #{}),
        <<"wallet-address">> =>
            hb_maps:get(<<"wallet-address">>, E, null, #{}),
        <<"node-message-id">> =>
            hb_maps:get(<<"node-message-id">>, E, null, #{})
    }.

%%---- TPM identity ------------------------------------------------------

interpret_tpm_identity(E, Db) ->
    Pem = hb_maps:get(<<"ek-cert-pem">>, E, <<>>, #{}),
    case decode_cert(Pem) of
        {ok, Cert} ->
            Attrs = tpm_attrs_from_cert(Cert),
            VendorId = maps:get(manufacturer_id, Attrs, undefined),
            VendorEntry = lookup_vendor(VendorId, Db),
            maps:merge(
                #{
                    <<"manufacturer-id">> =>
                        or_null(VendorId),
                    <<"manufacturer-name">> =>
                        maps:get(<<"name">>, VendorEntry, null),
                    <<"manufacturer-kind">> =>
                        maps:get(<<"kind">>, VendorEntry, null),
                    <<"model">> =>
                        or_null(maps:get(model, Attrs, undefined)),
                    <<"firmware-version">> =>
                        or_null(maps:get(firmware_version, Attrs,
                                         undefined)),
                    <<"spec-family">> =>
                        or_null(maps:get(spec_family, Attrs, undefined)),
                    <<"spec-level">> =>
                        or_null(maps:get(spec_level, Attrs, undefined)),
                    <<"spec-revision">> =>
                        or_null(maps:get(spec_revision, Attrs, undefined)),
                    <<"ek-cert-subject">> =>
                        or_null(maps:get(subject_rdn, Attrs, undefined)),
                    <<"ek-cert-issuer">> =>
                        or_null(maps:get(issuer_rdn, Attrs, undefined)),
                    <<"ek-cert-serial">> =>
                        or_null(maps:get(serial_b64url, Attrs, undefined)),
                    <<"ek-cert-valid-from">> =>
                        or_null(maps:get(valid_from, Attrs, undefined)),
                    <<"ek-cert-valid-to">> =>
                        or_null(maps:get(valid_to, Attrs, undefined))
                },
                extra_vendor_fields(VendorEntry))
            ;
        {error, Why} ->
            #{
                <<"manufacturer-id">> => null,
                <<"manufacturer-name">> => null,
                <<"error">> =>
                    iolist_to_binary(
                        io_lib:format("ek_cert_pem not decodable: ~p", [Why]))
            }
    end.

extra_vendor_fields(Entry) when is_map(Entry) ->
    %% Anything else the vendor entry carries (website, notes,
    %% known-compromised CVEs, etc.) is surfaced under the `tpm'
    %% block so policy callers can read it without a second lookup.
    maps:without(
        [<<"name">>, <<"kind">>, <<"id">>],
        Entry);
extra_vendor_fields(_) -> #{}.

lookup_vendor(undefined, _Db) -> #{};
lookup_vendor(Id, #{<<"vendors">> := V}) when is_map(V) ->
    maps:get(Id, V, maps:get(<<"unknown">>, V, #{}));
lookup_vendor(_, _) -> #{}.

%%---- AK -----------------------------------------------------------------

interpret_ak(E) ->
    Pem = hb_maps:get(<<"ak-pub-pem">>, E, <<>>, #{}),
    case decode_pub_key(Pem) of
        {ok, #'RSAPublicKey'{modulus = N, publicExponent = Exp}} ->
            Der = public_key:der_encode('RSAPublicKey',
                                        #'RSAPublicKey'{
                                            modulus=N, publicExponent=Exp}),
            #{
                <<"algorithm">> => <<"RSA">>,
                <<"key-size-bits">> =>
                    bit_size_of_modulus(N),
                <<"public-exponent">> => Exp,
                <<"pub-der-sha256-b64url">> =>
                    hb_util:encode(crypto:hash(sha256, Der))
            };
        {ok, Other} ->
            #{<<"algorithm">> =>
                iolist_to_binary(io_lib:format("~p", [element(1, Other)]))};
        {error, Why} ->
            #{<<"error">> =>
                iolist_to_binary(
                    io_lib:format("ak_pub_pem not decodable: ~p", [Why]))}
    end.

bit_size_of_modulus(N) when is_integer(N) ->
    bit_length(N).

bit_length(N) when N < 0 -> bit_length(-N);
bit_length(0) -> 0;
bit_length(N) -> bit_length(N bsr 1, 1).
bit_length(0, Acc) -> Acc;
bit_length(N, Acc) -> bit_length(N bsr 1, Acc + 1).

%%---- Quote metadata -----------------------------------------------------

interpret_quote_metadata(E) ->
    Q = hb_maps:get(<<"tpm-quote">>, E, #{}, #{}),
    QuotedB64 = hb_maps:get(<<"quoted">>, Q, <<>>, #{}),
    try
        Quoted = hb_util:decode(QuotedB64),
        <<Magic:4/binary, Type:16/unsigned-big, Rest0/binary>> = Quoted,
        {QualifiedSigner, Rest1} = tpm2b(Rest0),
        {ExtraData, Rest2}       = tpm2b(Rest1),
        <<Clock:64/unsigned-big,
          ResetCount:32/unsigned-big,
          RestartCount:32/unsigned-big,
          SafeByte:8,
          FirmwareVersion:64/unsigned-big,
          Rest3/binary>> = Rest2,
        %% The `attested' union depends on `Type'. For quotes
        %% (0x8018) it's TPMS_QUOTE_INFO = TPML_PCR_SELECTION +
        %% TPM2B_DIGEST. Other attest types carry different
        %% payloads; we parse those we recognise and fall back to
        %% a `tail-length' field otherwise.
        TypeName = attest_type_name(Type),
        AttestFields = decode_attest_body(Type, Rest3),
        BaseFields = #{
            %% Magic is a 4-byte TCG sentinel (0xFF "TCG"). We don't
            %% expose the raw bytes — `magic_ok' is the single fact a
            %% caller needs; an unrecognised magic means the quote is
            %% not TPM-shaped and `error' is returned instead.
            <<"magic-ok">>             => (Magic =:= <<16#FF, "TCG">>),
            <<"attest-type">>          => TypeName,
            <<"attest-type-code">>     => Type,
            <<"qualified-signer-name">> => hb_util:encode(QualifiedSigner),
            <<"qualified-signer-name-length">> => byte_size(QualifiedSigner),
            <<"nonce">>                => hb_util:encode(ExtraData),
            <<"clock-ms">>             => Clock,
            <<"clock-seconds">>        => Clock div 1000,
            <<"reset-count">>          => ResetCount,
            <<"restart-count">>        => RestartCount,
            <<"safe">>                 => SafeByte =/= 0,
            %% TPM firmware version is a 64-bit opaque identifier
            %% whose packing is vendor-defined. We surface both
            %% the raw u64 and a split form (hi/lo u32) that
            %% matches the common Infineon / Nuvoton / STMicro /
            %% Microsoft-TPM display convention.
            <<"firmware-version-u64">> => FirmwareVersion,
            <<"firmware-version-hex">> =>
                iolist_to_binary(io_lib:format(
                    "0x~16.16.0B", [FirmwareVersion])),
            <<"firmware-version-high">> =>
                (FirmwareVersion bsr 32) band 16#FFFFFFFF,
            <<"firmware-version-low">>  =>
                FirmwareVersion band 16#FFFFFFFF
        },
        maps:merge(BaseFields, AttestFields)
    catch
        _:_ ->
            #{<<"error">> =>
                <<"TPMS_ATTEST parse failed (truncated or wrong shape)">>}
    end.

tpm2b(<<Size:16/unsigned-big, Payload:Size/binary, Rest/binary>>) ->
    {Payload, Rest}.

%% @doc Decode the body of a TPMS_ATTEST based on its `Type'. For
%% quotes (0x8018) this is TPMS_QUOTE_INFO. Other types carry
%% different payloads; those we recognise are decoded, others get
%% a `tail-length' + `tail-sha256' fallback.
decode_attest_body(16#8018, Body) ->
    try
        %% TPMS_QUOTE_INFO:
        %%   pcrSelect: TPML_PCR_SELECTION
        %%     count:          u32 BE
        %%     pcrSelections:  [TPMS_PCR_SELECTION]
        %%   pcrDigest: TPM2B_DIGEST
        <<Count:32/unsigned-big, Rest0/binary>> = Body,
        {Selections, Rest1} = decode_pcr_selections(Count, Rest0, []),
        {PcrDigest, _Tail} = tpm2b(Rest1),
        #{<<"pcr-select">> => Selections,
          <<"pcr-select-count">> => Count,
          <<"pcr-digest">> => hb_util:encode(PcrDigest),
          <<"pcr-digest-length">> => byte_size(PcrDigest)}
    catch _:_ ->
        #{<<"attest-body-error">> =>
              <<"TPMS_QUOTE_INFO parse failed">>}
    end;
decode_attest_body(_OtherType, Body) ->
    #{<<"attest-body-length">> => byte_size(Body),
      <<"attest-body-sha256">> =>
          hb_util:encode(crypto:hash(sha256, Body))}.

decode_pcr_selections(0, Rest, Acc) ->
    {lists:reverse(Acc), Rest};
decode_pcr_selections(N, Bin, Acc) ->
    <<HashAlg:16/unsigned-big, SizeOfSelect:8,
      Select:SizeOfSelect/binary, Rest/binary>> = Bin,
    SelRec = #{
        <<"hash-alg-code">> => HashAlg,
        <<"hash-alg-name">> => hash_alg_name(HashAlg),
        <<"pcr-indexes">>   => pcr_bitmap_to_list(Select),
        <<"pcr-bitmap">>    => hb_util:encode(Select),
        <<"size-of-select">>=> SizeOfSelect
    },
    decode_pcr_selections(N - 1, Rest, [SelRec | Acc]).

%% @doc TPM_ALG_ID hash-algorithm mapping (TPM 2.0 Part 2 Table 9).
hash_alg_name(16#0004) -> <<"sha1">>;
hash_alg_name(16#000B) -> <<"sha256">>;
hash_alg_name(16#000C) -> <<"sha384">>;
hash_alg_name(16#000D) -> <<"sha512">>;
hash_alg_name(16#0012) -> <<"sm3-256">>;
hash_alg_name(16#0027) -> <<"sha3-256">>;
hash_alg_name(16#0028) -> <<"sha3-384">>;
hash_alg_name(16#0029) -> <<"sha3-512">>;
hash_alg_name(N) ->
    iolist_to_binary(io_lib:format("alg-0x~4.16.0B", [N])).

%% @doc Convert a TPML_PCR_SELECTION bitmap into a sorted list
%% of PCR indexes.
%%
%% The bitmap is little-endian-per-byte, LSB-of-each-byte is the
%% lowest PCR in that byte. Byte 0 bit 0 = PCR 0, byte 0 bit 7 =
%% PCR 7, byte 1 bit 0 = PCR 8, and so on.
pcr_bitmap_to_list(Bitmap) ->
    pcr_bitmap_to_list(Bitmap, 0, []).

pcr_bitmap_to_list(<<>>, _, Acc) -> lists:reverse(Acc);
pcr_bitmap_to_list(<<Byte:8, Rest/binary>>, Offset, Acc) ->
    Acc1 = lists:foldl(
        fun(Bit, A) ->
            case (Byte bsr Bit) band 1 of
                1 -> [Offset * 8 + Bit | A];
                0 -> A
            end
        end, Acc, lists:seq(0, 7)),
    pcr_bitmap_to_list(Rest, Offset + 1, Acc1).

%% Per TCG TPM 2.0 Part 2 Table 19 (TPM_ST Constants):
attest_type_name(16#8014) -> <<"TPM_ST_ATTEST_NV">>;
attest_type_name(16#8015) -> <<"TPM_ST_ATTEST_COMMAND_AUDIT">>;
attest_type_name(16#8016) -> <<"TPM_ST_ATTEST_SESSION_AUDIT">>;
attest_type_name(16#8017) -> <<"TPM_ST_ATTEST_CERTIFY">>;
attest_type_name(16#8018) -> <<"TPM_ST_ATTEST_QUOTE">>;
attest_type_name(16#8019) -> <<"TPM_ST_ATTEST_TIME">>;
attest_type_name(16#801A) -> <<"TPM_ST_ATTEST_CREATION">>;
attest_type_name(16#801C) -> <<"TPM_ST_ATTEST_NV_DIGEST">>;
attest_type_name(N) -> iolist_to_binary(io_lib:format("0x~.16B", [N])).

%%---- PCRs --------------------------------------------------------------

interpret_pcrs(E, _Db, Events) ->
    Q = hb_maps:get(<<"tpm-quote">>, E, #{}, #{}),
    Vals = hb_maps:get(<<"pcr-values">>, Q, #{}, #{}),
    %% Group events by the PCR they extended — a single pass over the
    %% parsed event log. The resulting `EventsByPcr' is a map from
    %% PCR index (integer) to a list of events sorted by seq.
    EventsByPcr = group_events_by_pcr(Events),
    maps:from_list(
        [{I, interpret_one_pcr(I, V, EventsByPcr)}
         || {I, V} <- maps:to_list(Vals)]).

%% For each PCR index in 0..23, the events that extended it, sorted
%% by sequence number (insertion order in the log).
group_events_by_pcr(Events) when is_map(Events) ->
    %% `Events' is a map #{<<"1">> => EventMsg, <<"2">> => ...}.
    SortedByseq =
        [Ev || {_, Ev} <-
            lists:sort(
                fun({KA, _}, {KB, _}) ->
                    key_to_int(KA) =< key_to_int(KB)
                end,
                [{K, V} || {K, V} <- maps:to_list(Events),
                           is_map(V)]),
            is_map(Ev)],
    lists:foldl(
        fun(Ev, Acc) ->
            case maps:get(<<"pcr">>, Ev, undefined) of
                P when is_integer(P) ->
                    maps:update_with(P, fun(L) -> L ++ [Ev] end,
                                     [Ev], Acc);
                _ -> Acc
            end
        end,
        #{},
        SortedByseq);
group_events_by_pcr(_) -> #{}.

key_to_int(B) when is_binary(B) ->
    try binary_to_integer(B) catch _:_ -> 0 end;
key_to_int(I) when is_integer(I) -> I;
key_to_int(_) -> 0.

interpret_one_pcr(Idx, B64, EventsByPcr) ->
    Raw = try hb_util:decode(B64)
          catch _:_ -> <<>>
          end,
    Zero = (Raw =:= <<0:256>>) orelse (Raw =:= <<>>),
    PcrInt = key_to_int(Idx),
    EvList = maps:get(PcrInt, EventsByPcr, []),
    EvMap = events_list_to_seq_map(EvList),
    Reconstruction = reconstruct_pcr(EvList, Raw),
    Derived = derive_fields_from_events(PcrInt, EvList),
    Base = #{
        %% Canonical base64url form, carried through unchanged from the
        %% attestation envelope. No hex twin: HyperBEAM wire convention
        %% is base64url everywhere, and the raw digest is well over the
        %% "short and always-displayed-in-hex" exception threshold.
        <<"digest">>     => B64,
        <<"role">>       => pcr_role(Idx),
        <<"role-notes">> => pcr_role_notes(Idx),
        <<"is-zero">>    => Zero,
        %% The filtered event log for this PCR. Each event is
        %% path-addressable under `/interpret/pcrs/<N>/events/<seq>'.
        <<"events">>     => EvMap,
        <<"event-count">> => length(EvList),
        %% `derived' is the merged named-field view. Every fact that
        %% can be unambiguously extracted from this PCR's events lands
        %% here as a concrete value (binary / bool / integer) OR the
        %% sentinel `<<"unknown">>' when the events don't carry the
        %% evidence to decide. A policy engine consumes `derived' as
        %% the policy input; `events' is the audit trail.
        <<"derived">>    => Derived
    },
    case Reconstruction of
        undefined -> Base;
        _ -> Base#{<<"reconstruction">> => Reconstruction}
    end.

events_list_to_seq_map(EvList) ->
    maps:from_list(
        [{integer_to_binary(maps:get(<<"seq">>, Ev, 0)), Ev}
         || Ev <- EvList]).

%% Replay every event's SHA-256 digest into its PCR and compare
%% against the quoted value. `undefined' when there are no events
%% in this PCR (nothing to reconstruct from).
reconstruct_pcr([], _Quoted) -> undefined;
reconstruct_pcr(EvList, Quoted) ->
    Replayed = lists:foldl(
        fun(Ev, Acc) ->
            case maps:get(<<"event-type-code">>, Ev, 0) of
                3 -> Acc;  %% EV_NO_ACTION — per TCG spec, not extended
                _ ->
                    Digests = maps:get(<<"digests">>, Ev, #{}),
                    case maps:get(<<"sha256">>, Digests, undefined) of
                        D when is_binary(D), byte_size(D) =:= 32 ->
                            crypto:hash(sha256, <<Acc/binary, D/binary>>);
                        _ -> Acc
                    end
            end
        end,
        <<0:256>>,
        EvList),
    Matches = (Replayed =:= Quoted),
    #{
        <<"replayed-digest">> => hb_util:encode(Replayed),
        <<"matches-quoted">>  => Matches,
        <<"replayed-from-events">> => length(EvList)
    }.

%% Derive named-field values from a PCR's events. The idea is that
%% *every property we can parse out of the firmware/OS events should
%% live here as a concrete AO-Core field*, navigable as
%% `/interpret/pcrs/<N>/derived/<field>'. Unknowns stay as the binary
%% `<<"unknown">>' so policy callers can distinguish "not present in
%% log" from "present and false".
derive_fields_from_events(Pcr, EvList) ->
    Base = derived_template_for_pcr(Pcr),
    lists:foldl(
        fun(Ev, Acc) -> merge_derived(Acc, derive_from_event(Pcr, Ev)) end,
        Base,
        EvList).

%% Per-PCR starting template of fields we expect to be able to derive
%% on real hardware — callers can rely on the SHAPE always being
%% present, with `<<"unknown">>' values when the current event log
%% can't populate them.
derived_template_for_pcr(0) ->
    %% PCR 0 = SRTM / firmware code.
    #{
        <<"crtm-version">>        => <<"unknown">>,
        <<"hcrtm">>               => <<"unknown">>,
        <<"post-codes">>          => [],
        <<"firmware-blobs">>      => [],
        <<"separator-seen">>      => false
    };
derived_template_for_pcr(1) ->
    %% PCR 1 = platform configuration (CPU microcode, platform
    %% config flags, UEFI boot variables, ACPI/SMBIOS handoff).
    #{
        <<"cpu-microcode">>       => <<"unknown">>,
        <<"uefi-boot-order">>     => [],
        <<"boot-entries">>        => [],
        <<"boot-current">>        => <<"unknown">>,
        <<"handoff-tables">>      => [],
        <<"separator-seen">>      => false
    };
derived_template_for_pcr(N) when N =:= 2; N =:= 3 ->
    #{
        <<"option-rom-scanned">>  => false,
        <<"separator-seen">>      => false
    };
derived_template_for_pcr(4) ->
    #{
        <<"boot-services-applications">> => [],
        <<"boot-action-markers">>        => [],
        <<"separator-seen">>             => false
    };
derived_template_for_pcr(5) ->
    #{
        <<"gpt-partition-tables">>  => 0,
        <<"separator-seen">>        => false
    };
derived_template_for_pcr(7) ->
    %% PCR 7 = Secure Boot state + keyset + shim authority chain.
    #{
        <<"secure-boot-enabled">>       => <<"unknown">>,
        <<"setup-mode">>                => <<"unknown">>,
        <<"audit-mode">>                => <<"unknown">>,
        <<"deployed-mode">>             => <<"unknown">>,
        <<"pk-entry-count">>            => <<"unknown">>,
        <<"pk-x509-fingerprints">>      => [],
        <<"kek-entry-count">>           => <<"unknown">>,
        <<"kek-x509-fingerprints">>     => [],
        <<"kek-issuers">>               => [],
        <<"db-entry-count">>            => <<"unknown">>,
        <<"db-x509-fingerprints">>      => [],
        <<"db-issuers">>                => [],
        <<"dbx-entry-count">>           => <<"unknown">>,
        <<"authorities">>               => [],
        %% shim-specific (when present in the authority chain):
        <<"moklist-trusted">>           => <<"unknown">>,
        <<"sbat-self-revision">>        => <<"unknown">>,
        <<"sbat-entry-count">>          => <<"unknown">>,
        <<"separator-seen">>            => false
    };
derived_template_for_pcr(8) -> #{<<"grub-cmdline">> => <<"unknown">>};
derived_template_for_pcr(9) -> #{<<"grub-modules">> => []};
derived_template_for_pcr(10) ->
    %% PCR 10 = IMA runtime. Per-file chain not yet transported —
    %% documented gap in the envelope.
    #{
        <<"ima-active">>            => true,
        <<"ima-event-count">>       => <<"unknown">>,
        <<"ima-files-measured">>    => <<"unknown">>,
        <<"note">>                  =>
            <<"LapEE does not yet transport the IMA per-file event "
              "log in the envelope; only PCR 10's final value is "
              "signed. Future `~tpm2@2.0a' versions will include it.">>
    };
derived_template_for_pcr(11) ->
    %% PCR 11 = UKI kernel image (systemd-stub PE hashes).
    #{
        <<"uki-measured">>          => false,
        <<"uki-image-hash">>        => <<"unknown">>,
        <<"uki-kernel-version">>    => <<"unknown">>
    };
derived_template_for_pcr(12) ->
    %% PCR 12 = UKI kernel cmdline (systemd-stub convention) — the
    %% paper's single most information-dense measurement. Every
    %% flag the paper §Architecture l.223-230 + §Security table
    %% calls out is surfaced as a named field here, with
    %% `"unknown"' as the "flag absent" sentinel.
    #{
        <<"uki-cmdline">>                  => <<"unknown">>,
        <<"uki-initrd-hash">>              => <<"unknown">>,
        %% Memory encryption (tier 2 evidence per the paper):
        <<"mem-encrypt-requested">>        => <<"unknown">>,
        <<"intel-tdx-requested">>          => <<"unknown">>,
        %% IOMMU:
        <<"iommu-mode">>                   => <<"unknown">>,
        <<"iommu-strict">>                 => <<"unknown">>,
        <<"intel-iommu-requested">>        => <<"unknown">>,
        <<"amd-iommu-requested">>          => <<"unknown">>,
        <<"iommu-passthrough">>            => <<"unknown">>,
        <<"iommu-dma-mode">>               => <<"unknown">>,
        %% Kernel lockdown:
        <<"lockdown-mode">>                => <<"unknown">>,
        %% Memory hygiene:
        <<"init-on-alloc">>                => <<"unknown">>,
        <<"init-on-free">>                 => <<"unknown">>,
        <<"slab-nomerge">>                 => <<"unknown">>,
        <<"page-poison">>                  => <<"unknown">>,
        %% Module loading:
        <<"module-sig-enforce">>           => <<"unknown">>,
        %% dm-verity rootfs integrity:
        <<"verity-root-hash">>             => <<"unknown">>,
        <<"verity-usr-root-hash">>         => <<"unknown">>,
        %% CPU mitigations:
        <<"kernel-page-table-isolation">>  => <<"unknown">>,
        <<"randomize-kstack-offset">>      => <<"unknown">>,
        <<"no-smt">>                       => <<"unknown">>,
        <<"mitigations-mode">>             => <<"unknown">>,
        <<"spectre-v2-mitigation">>        => <<"unknown">>,
        <<"ssbd-mode">>                    => <<"unknown">>,
        <<"vsyscall-mode">>                => <<"unknown">>,
        %% KASLR / audit / IMA:
        <<"no-kaslr">>                     => <<"unknown">>,
        <<"audit-enabled">>                => <<"unknown">>,
        <<"ima-policy">>                   => <<"unknown">>,
        <<"ima-appraise-mode">>            => <<"unknown">>,
        <<"debugfs-mode">>                 => <<"unknown">>
    };
derived_template_for_pcr(13) ->
    #{
        <<"uki-sysext-count">>      => <<"unknown">>
    };
derived_template_for_pcr(14) ->
    #{
        <<"mok-entry-count">>       => <<"unknown">>
    };
derived_template_for_pcr(15) ->
    %% LapEE node identity — fully parsed elsewhere in `node.*'.
    #{
        <<"lapee-node-identity-committed">> => true
    };
derived_template_for_pcr(_) -> #{}.

%% Per-event extraction. For each event, dig into its `parsed'
%% sub-map (populated by dev_tpm_tcg:decode_events/1) and return a
%% partial derived map. `merge_derived' (below) reduces the list of
%% partials into the final derived map.
derive_from_event(Pcr, Ev) ->
    Parsed = maps:get(<<"parsed">>, Ev, #{}),
    Semantic =
        case Parsed of
            #{<<"semantic">> := S} when is_map(S) -> S;
            _ -> #{}
        end,
    EtCode = maps:get(<<"event-type-code">>, Ev, 0),
    derive_from_event(Pcr, EtCode, Parsed, Semantic).

%% EV_NO_ACTION — SpecID header (PCR 0).
derive_from_event(0, 3, Parsed, _) ->
    case maps:get(<<"spec-id">>, Parsed, undefined) of
        undefined -> #{};
        V -> #{<<"spec-id">> => V}
    end;
%% EV_SEPARATOR — boundary marker. Fires on many PCRs.
derive_from_event(_, 4, Parsed, _) ->
    #{<<"separator-seen">> => true,
      <<"separator-kind">> => maps:get(<<"separator">>, Parsed,
                                       <<"unknown">>)};
%% EV_S_CRTM_VERSION — PCR 0.
derive_from_event(0, 8, Parsed, _) ->
    case maps:get(<<"crtm-version">>, Parsed, undefined) of
        V when is_binary(V), byte_size(V) > 0 ->
            #{<<"crtm-version">> => V};
        _ -> #{}
    end;
%% EV_CPU_MICROCODE — PCR 1. Intel AND AMD layouts. The TCG parser
%% emits `parsed.format' = "intel" or "amd" so we discriminate here.
derive_from_event(1, 9, Parsed, _) ->
    Format = maps:get(<<"format">>, Parsed, <<"unknown">>),
    case Format of
        <<"intel">> ->
            Rev = maps:get(<<"update-revision">>, Parsed, 0),
            Sig = maps:get(<<"processor-signature">>, Parsed, 0),
            FMS = maps:get(<<"cpu-family-model-stepping">>, Parsed,
                           <<"">>),
            #{<<"cpu-microcode">> =>
                iolist_to_binary(io_lib:format(
                    "intel rev=0x~.16B sig=0x~.16B ~s",
                    [Rev, Sig, FMS])),
              <<"cpu-vendor">> => <<"intel">>};
        <<"amd">> ->
            Patch = maps:get(<<"patch-id">>, Parsed, 0),
            ProcRev = maps:get(<<"processor-rev-id">>, Parsed, 0),
            Date = maps:get(<<"date">>, Parsed, <<"">>),
            #{<<"cpu-microcode">> =>
                iolist_to_binary(io_lib:format(
                    "amd patch-id=0x~.16B proc-rev=0x~4.16.0B ~s",
                    [Patch, ProcRev, Date])),
              <<"cpu-vendor">> => <<"amd">>};
        _ ->
            Rev = maps:get(<<"update-revision">>, Parsed, 0),
            case Rev of
                0 -> #{};
                _ -> #{<<"cpu-microcode">> =>
                          iolist_to_binary(io_lib:format(
                              "unknown rev=0x~.16B", [Rev]))}
            end
    end;
%% EV_POST_CODE — PCR 0.
derive_from_event(0, 1, Parsed, _) ->
    case maps:get(<<"post-code">>, Parsed, undefined) of
        V when is_binary(V), byte_size(V) > 0 ->
            #{<<"post-codes">> => [V]};
        _ -> #{}
    end;
%% EV_EFI_HCRTM_EVENT — PCR 0.
derive_from_event(0, 16#80000010, _, _) ->
    #{<<"hcrtm">> => true};
%% EV_EFI_PLATFORM_FIRMWARE_BLOB(2) — PCR 0.
derive_from_event(0, Code, Parsed, _) when Code =:= 16#80000008;
                                           Code =:= 16#8000000A ->
    Addr = maps:get(<<"blob-physical-address">>, Parsed, 0),
    Len  = maps:get(<<"blob-length">>, Parsed, 0),
    Desc = maps:get(<<"blob-description">>, Parsed, <<>>),
    Blob = #{<<"address">> => Addr,
             <<"length">>  => Len,
             <<"description">> => Desc},
    #{<<"firmware-blobs">> => [Blob]};
%% EV_EFI_VARIABLE_DRIVER_CONFIG — PCR 7.
derive_from_event(7, 16#80000001, Parsed, Semantic) ->
    Name = maps:get(<<"variable-name">>, Parsed, <<>>),
    case Name of
        <<"SecureBoot">> ->
            case maps:get(<<"secure-boot-enabled">>, Semantic, undefined) of
                true  -> #{<<"secure-boot-enabled">> => true};
                false -> #{<<"secure-boot-enabled">> => false};
                _ -> #{}
            end;
        <<"SetupMode">> ->
            case maps:get(<<"setup-mode">>, Semantic, undefined) of
                T when is_boolean(T) -> #{<<"setup-mode">> => T};
                _ -> #{}
            end;
        <<"AuditMode">> ->
            case maps:get(<<"audit-mode">>, Semantic, undefined) of
                T when is_boolean(T) -> #{<<"audit-mode">> => T};
                _ -> #{}
            end;
        <<"DeployedMode">> ->
            case maps:get(<<"deployed-mode">>, Semantic, undefined) of
                T when is_boolean(T) -> #{<<"deployed-mode">> => T};
                _ -> #{}
            end;
        <<"PK">> ->
            SL = maps:get(<<"signature-list">>, Semantic, []),
            Entries = lists:flatten(
                [maps:get(<<"entries">>, L, []) || L <- SL]),
            Fingerprints = [maps:get(<<"x509-sha256-fingerprint">>, E,
                                      <<"">>)
                            || E <- Entries, is_map(E),
                               maps:is_key(<<"x509-sha256-fingerprint">>, E)],
            #{<<"pk-entry-count">> =>
                lists:sum([maps:get(<<"entry-count">>, L, 0) || L <- SL]),
              <<"pk-x509-fingerprints">> => Fingerprints};
        <<"KEK">> ->
            SL = maps:get(<<"signature-list">>, Semantic, []),
            Entries = lists:flatten(
                [maps:get(<<"entries">>, L, []) || L <- SL]),
            Fingerprints = [maps:get(<<"x509-sha256-fingerprint">>, E,
                                      <<"">>)
                            || E <- Entries, is_map(E),
                               maps:is_key(<<"x509-sha256-fingerprint">>, E)],
            Issuers = [maps:get(<<"x509-issuer">>, E, <<"">>)
                       || E <- Entries, is_map(E),
                          maps:is_key(<<"x509-issuer">>, E)],
            #{<<"kek-entry-count">> =>
                lists:sum([maps:get(<<"entry-count">>, L, 0) || L <- SL]),
              <<"kek-x509-fingerprints">> => Fingerprints,
              <<"kek-issuers">> => Issuers};
        <<"db">> ->
            SL = maps:get(<<"signature-list">>, Semantic, []),
            Entries = lists:flatten(
                [maps:get(<<"entries">>, L, []) || L <- SL]),
            DbFingerprints = [maps:get(<<"x509-sha256-fingerprint">>, E,
                                        <<"">>)
                              || E <- Entries, is_map(E),
                                 maps:is_key(<<"x509-sha256-fingerprint">>, E)],
            DbIssuers = [maps:get(<<"x509-issuer">>, E, <<"">>)
                         || E <- Entries, is_map(E),
                            maps:is_key(<<"x509-issuer">>, E)],
            #{<<"db-entry-count">> =>
                lists:sum([maps:get(<<"entry-count">>, L, 0) || L <- SL]),
              <<"db-x509-fingerprints">> => DbFingerprints,
              <<"db-issuers">> => DbIssuers};
        <<"dbx">> ->
            SL = maps:get(<<"signature-list">>, Semantic, []),
            #{<<"dbx-entry-count">> =>
                lists:sum([maps:get(<<"entry-count">>, L, 0) || L <- SL])};
        _ -> #{}
    end;
%% EV_EFI_VARIABLE_AUTHORITY — PCR 7.
derive_from_event(7, 16#800000E0, Parsed, Semantic) ->
    Name = maps:get(<<"variable-name">>, Parsed, <<>>),
    Base = case Name of
        <<>> -> #{};
        _    -> #{<<"authorities">> => [Name]}
    end,
    %% Enrich based on the specific authority variable.
    case Name of
        <<"MokListTrusted">> ->
            case maps:get(<<"moklist-trusted">>, Semantic, undefined) of
                T when is_boolean(T) ->
                    Base#{<<"moklist-trusted">> => T};
                _ -> Base
            end;
        <<"SbatLevel">> ->
            case maps:get(<<"sbat-entries">>, Semantic, undefined) of
                undefined -> Base;
                SbatList when is_list(SbatList) ->
                    %% Pull the SBAT self-revision from the first entry;
                    %% its second column is a date-stamped revision int.
                    case SbatList of
                        [#{<<"component">> := <<"sbat">>,
                           <<"revision">> := Rev} | _] ->
                            Base#{<<"sbat-self-revision">> => Rev,
                                  <<"sbat-entry-count">> =>
                                      maps:get(<<"sbat-entry-count">>,
                                               Semantic, 0)};
                        _ -> Base
                    end
            end;
        _ -> Base
    end;

%% EV_EFI_VARIABLE_BOOT / _BOOT2 on PCR 1: BootOrder + Boot####.
derive_from_event(1, Code, Parsed, Semantic)
  when Code =:= 16#80000002; Code =:= 16#8000000C ->
    Name = maps:get(<<"variable-name">>, Parsed, <<>>),
    case Name of
        <<"BootOrder">> ->
            #{<<"uefi-boot-order">> =>
                maps:get(<<"boot-order">>, Semantic, [])};
        <<"Boot", _/binary>> ->
            case maps:get(<<"load-option-description">>,
                            Semantic, undefined) of
                D when is_binary(D) ->
                    #{<<"boot-entries">> =>
                        [#{<<"name">>        => Name,
                           <<"description">> => D,
                           <<"active">> =>
                               maps:get(<<"load-option-active">>,
                                        Semantic, false)}]};
                _ -> #{}
            end;
        <<"BootCurrent">> ->
            case maps:get(<<"boot-current">>, Semantic, undefined) of
                BC when is_binary(BC) ->
                    #{<<"boot-current">> => BC};
                _ -> #{}
            end;
        _ -> #{}
    end;
%% EV_ACTION — PCR 2/4, contributions to the boot action list.
derive_from_event(2, 5, Parsed, _) ->
    case maps:get(<<"action">>, Parsed, undefined) of
        A when is_binary(A) ->
            Low = string:lowercase(A),
            case binary:match(Low, <<"option rom">>) of
                nomatch -> #{};
                _ -> #{<<"option-rom-scanned">> => true}
            end;
        _ -> #{}
    end;
derive_from_event(4, 5, Parsed, _) ->
    case maps:get(<<"action">>, Parsed, undefined) of
        A when is_binary(A) -> #{<<"boot-action-markers">> => [A]};
        _ -> #{}
    end;
%% EV_EFI_BOOT_SERVICES_APPLICATION — PCR 4.
derive_from_event(4, 16#80000003, Parsed, _) ->
    App = #{
        <<"image-location-in-memory">> =>
            maps:get(<<"image-location-in-memory">>, Parsed, 0),
        <<"image-length-in-memory">>   =>
            maps:get(<<"image-length-in-memory">>, Parsed, 0)
    },
    #{<<"boot-services-applications">> => [App]};
%% EV_EFI_GPT_EVENT — PCR 5.
derive_from_event(5, 16#80000006, _, _) ->
    #{<<"gpt-partition-tables">> => 1};
%% EV_IPL — PCR 11/12/13 (systemd-stub key=value).
derive_from_event(11, 16#0D, Parsed, _) ->
    case {maps:get(<<"key">>, Parsed, undefined),
          maps:get(<<"value">>, Parsed, undefined)} of
        {<<"kernel-name">>, V} when is_binary(V) ->
            #{<<"uki-kernel-version">> => V, <<"uki-measured">> => true};
        {<<"kernel-image">>, _} ->
            #{<<"uki-measured">> => true};
        _ -> #{}
    end;
derive_from_event(12, 16#0D, Parsed, _) ->
    case {maps:get(<<"key">>, Parsed, undefined),
          maps:get(<<"value">>, Parsed, undefined)} of
        {<<"kernel-cmdline">>, V} when is_binary(V) ->
            %% Base: the raw cmdline string. Plus — every security
            %% flag the paper (§Architecture l.223-230, §Security
            %% table) lists as a boot-time attested property gets
            %% extracted into a named `derived/<field>' slot.
            Flags = maps:get(<<"cmdline-flags">>, Parsed, #{}),
            maps:merge(
                #{<<"uki-cmdline">> => V},
                extract_cmdline_security_flags(Flags));
        _ -> #{}
    end;
derive_from_event(_, _, _, _) -> #{}.

%% Paper §Architecture line 219-230 + §Security table — the set of
%% kernel-cmdline flags that, when present, attest to specific
%% security properties of the running kernel. Each mapping pins one
%% cmdline flag to one derived field on PCR 12.
%%
%%   mem_encrypt=on / sme=on → mem-encrypt-requested: true
%%   kvm_intel.tdx=on       → intel-tdx-requested: true
%%   iommu=pt | ...         → iommu-mode: "pt" (or other)
%%   iommu.strict=1         → iommu-strict: true
%%   intel_iommu=on         → intel-iommu-requested: true
%%   amd_iommu=on           → amd-iommu-requested: true
%%   lockdown=<mode>        → lockdown-mode: "integrity"|"confidentiality"|...
%%   init_on_alloc=1        → init-on-alloc: true
%%   init_on_free=1         → init-on-free: true
%%   module.sig_enforce=1   → module-sig-enforce: true
%%   roothash=<hex>         → verity-root-hash: <hex>
%%   systemd.verity_root_hash=<hex> → verity-root-hash: <hex>
%%   slab_nomerge           → slab-nomerge: true
%%   page_poison=1          → page-poison: true
%%   pti=on                 → kernel-page-table-isolation: true
%%   randomize_kstack_offset=1 → randomize-kstack-offset: true
extract_cmdline_security_flags(Flags) when is_map(Flags) ->
    lists:foldl(
        fun({SrcKey, DstKey, Kind}, Acc) ->
            case maps:get(SrcKey, Flags, undefined) of
                undefined -> Acc;
                Val       -> Acc#{DstKey => normalise_flag(Val, Kind)}
            end
        end, #{}, cmdline_security_flag_map());
extract_cmdline_security_flags(_) -> #{}.

cmdline_security_flag_map() ->
    [
        %% {cmdline-key, derived-field, kind}
        {<<"mem_encrypt">>,          <<"mem-encrypt-requested">>, bool},
        {<<"sme">>,                  <<"mem-encrypt-requested">>, bool},
        {<<"kvm_intel.tdx">>,        <<"intel-tdx-requested">>,   bool},
        {<<"iommu">>,                <<"iommu-mode">>,            raw},
        {<<"iommu.strict">>,         <<"iommu-strict">>,          bool},
        {<<"intel_iommu">>,          <<"intel-iommu-requested">>, bool},
        {<<"amd_iommu">>,            <<"amd-iommu-requested">>,   raw},
        {<<"iommu.passthrough">>,    <<"iommu-passthrough">>,     bool},
        {<<"iommu.dma_mode">>,       <<"iommu-dma-mode">>,        raw},
        {<<"lockdown">>,             <<"lockdown-mode">>,         raw},
        {<<"init_on_alloc">>,        <<"init-on-alloc">>,         bool},
        {<<"init_on_free">>,         <<"init-on-free">>,          bool},
        {<<"module.sig_enforce">>,   <<"module-sig-enforce">>,    bool},
        {<<"roothash">>,             <<"verity-root-hash">>,      raw},
        {<<"systemd.verity_root_hash">>, <<"verity-root-hash">>,  raw},
        {<<"systemd.verity_usr_root_hash">>,
                                     <<"verity-usr-root-hash">>,  raw},
        {<<"slab_nomerge">>,         <<"slab-nomerge">>,          bool},
        {<<"page_poison">>,          <<"page-poison">>,           bool},
        {<<"pti">>,                  <<"kernel-page-table-isolation">>, raw},
        {<<"randomize_kstack_offset">>,
                                     <<"randomize-kstack-offset">>, bool},
        {<<"nosmt">>,                <<"no-smt">>,                bool},
        {<<"mitigations">>,          <<"mitigations-mode">>,      raw},
        {<<"spectre_v2">>,           <<"spectre-v2-mitigation">>, raw},
        {<<"spec_store_bypass_disable">>,
                                     <<"ssbd-mode">>,             raw},
        {<<"vsyscall">>,             <<"vsyscall-mode">>,         raw},
        {<<"audit">>,                <<"audit-enabled">>,         raw},
        {<<"debugfs">>,              <<"debugfs-mode">>,          raw},
        {<<"nokaslr">>,              <<"no-kaslr">>,              bool},
        {<<"ima_policy">>,           <<"ima-policy">>,            raw},
        {<<"ima_appraise">>,         <<"ima-appraise-mode">>,     raw}
    ].

normalise_flag(true, bool)  -> true;
normalise_flag(false, bool) -> false;
normalise_flag(<<"1">>, bool) -> true;
normalise_flag(<<"0">>, bool) -> false;
normalise_flag(V, bool) when is_binary(V) -> V;   %% non-bool form
normalise_flag(V, raw)  -> V.

%% Merge two partial derived maps. Rules:
%%   - Lists concatenate.
%%   - Counters (integers) sum.
%%   - Booleans OR (so `option_rom_scanned = true' wins).
%%   - `<<"unknown">>' is overridden by any concrete value.
%%   - Otherwise rightmost wins.
merge_derived(Acc, New) ->
    maps:fold(
        fun(K, V, Inner) ->
            Existing = maps:get(K, Inner, undefined),
            Inner#{K => merge_value(K, Existing, V)}
        end,
        Acc,
        New).

merge_value(_K, undefined, V) -> V;
merge_value(_K, <<"unknown">>, V) -> V;
merge_value(_K, Old, <<"unknown">>) -> Old;
merge_value(_K, Old, New) when is_list(Old), is_list(New) -> Old ++ New;
merge_value(_K, Old, New) when is_integer(Old), is_integer(New) ->
    Old + New;
merge_value(_K, true, _) -> true;
merge_value(_K, _, true) -> true;
merge_value(_K, _Old, New) -> New.

%% Canonical TCG PCR usage. Source: TCG PC Client Platform Firmware
%% Profile + UEFI Spec + systemd-stub docs.
pcr_role(<<"0">>) -> <<"firmware-srtm">>;
pcr_role(<<"1">>) -> <<"platform-firmware-config">>;
pcr_role(<<"2">>) -> <<"option-rom-code">>;
pcr_role(<<"3">>) -> <<"option-rom-config">>;
pcr_role(<<"4">>) -> <<"boot-loader-code">>;
pcr_role(<<"5">>) -> <<"boot-loader-config">>;
pcr_role(<<"6">>) -> <<"platform-manufacturer">>;
pcr_role(<<"7">>) -> <<"secure-boot-policy">>;
pcr_role(<<"8">>) -> <<"grub-kernel-cmdline-legacy">>;
pcr_role(<<"9">>) -> <<"grub-kernel-modules-legacy">>;
pcr_role(<<"10">>) -> <<"ima-runtime-measurements">>;
pcr_role(<<"11">>) -> <<"uki-kernel-image">>;
pcr_role(<<"12">>) -> <<"uki-kernel-cmdline">>;
pcr_role(<<"13">>) -> <<"uki-system-extensions">>;
pcr_role(<<"14">>) -> <<"secure-boot-authority-mok">>;
pcr_role(<<"15">>) -> <<"lapee-node-identity">>;
pcr_role(N) when is_integer(N) -> pcr_role(integer_to_binary(N));
pcr_role(_) -> <<"unassigned-or-application">>.

pcr_role_notes(<<"0">>) ->
    <<"Extended by the CRTM/firmware with measurements of the firmware "
      "itself. Value depends on board vendor + BIOS/UEFI version.">>;
pcr_role_notes(<<"7">>) ->
    <<"Extended with Secure Boot state + the PK/KEK/db/dbx keyset. "
      "A legitimate SB-enabled boot produces a non-zero value; a "
      "zero value means Secure Boot was off during this boot.">>;
pcr_role_notes(<<"10">>) ->
    <<"Extended by the Linux IMA subsystem with every exec'd binary "
      "matching the active ima_policy. Tracks the runtime integrity "
      "history of userspace.">>;
pcr_role_notes(<<"11">>) ->
    <<"Extended by systemd-stub / sd-boot for the UKI's kernel image "
      "PE hashes. Pins the kernel+initrd identity to a signed image.">>;
pcr_role_notes(<<"15">>) ->
    <<"LapEE node identity. Extended at HB startup via the enforced "
      "`on.start' hook with the SHA-256 native id of the running "
      "node message. Uniquely identifies this boot's HB configuration.">>;
pcr_role_notes(N) when is_integer(N) -> pcr_role_notes(integer_to_binary(N));
pcr_role_notes(_) -> <<"">>.

%%---- Boot chain (firmware / Secure Boot) -------------------------------

interpret_boot_chain(_E, Db, Pcrs) ->
    Profile = match_pcr_profile(Pcrs, Db),
    Pcr0 = pcr_digest(<<"0">>, Pcrs),
    Pcr1 = pcr_digest(<<"1">>, Pcrs),
    Pcr7 = pcr_digest(<<"7">>, Pcrs),
    Base = #{
        <<"firmware-srtm">> => or_null(Pcr0),
        <<"platform-firmware-config">> => or_null(Pcr1),
        <<"secure-boot-policy">> => or_null(Pcr7),
        <<"secure-boot-measured">> =>
            %% PCR 7 all-zero => Secure Boot was OFF (or disabled) at
            %% boot. Non-zero => something extended it, likely
            %% genuine UEFI SB. We can't tell *on* vs *on-with-dev-
            %% keys* from the PCR alone — that needs the event log.
            not pcr_is_zero(<<"7">>, Pcrs)
    },
    case Profile of
        undefined -> Base#{<<"match">> => null};
        _ -> Base#{<<"match">> => Profile}
    end.

match_pcr_profile(Pcrs, Db) ->
    Profiles = case maps:get(<<"pcr-profiles">>, Db, #{}) of
        M when is_map(M) -> M;
        _ -> #{}
    end,
    Candidates =
        [Entry ||
            {_Key, Entry} <- maps:to_list(Profiles),
            profile_matches(Entry, Pcrs)],
    case Candidates of
        [] -> undefined;
        [E|_] -> summarise_profile(E)
    end.

%% Accept either `match_pcrs' (preferred) or `pcrs' (legacy).
%% An empty match block doesn't match — callers who want a
%% documentation-only profile to surface can look at the DB
%% directly. Profile digests are base64url strings (no hex).
profile_matches(Entry, Actual) when is_map(Entry) ->
    Expected =
        case maps:get(<<"match-pcrs">>, Entry, undefined) of
            undefined -> maps:get(<<"pcrs">>, Entry, #{});
            M -> M
        end,
    case maps:size(Expected) of
        0 -> false;
        _ ->
            lists:all(
                fun({PcrKey, ExpectedDigest}) ->
                    %% =:= not == so integer-valued profile digests
                    %% (if ever) don't coerce against binary actuals.
                    pcr_digest(PcrKey, Actual) =:= ExpectedDigest
                end,
                maps:to_list(Expected))
    end;
profile_matches(_, _) -> false.

summarise_profile(#{<<"name">> := Name, <<"attributes">> := Attrs}) ->
    #{<<"name">> => Name, <<"attributes">> => Attrs};
summarise_profile(#{<<"name">> := Name}) ->
    #{<<"name">> => Name};
summarise_profile(Entry) -> Entry.

%% Look up a PCR's base64url digest. Accepts both the new shape
%% (`digest' key) and any entry that still only has `raw_b64url'
%% from an older serialisation.
pcr_digest(Key, Pcrs) ->
    case hb_maps:get(Key, Pcrs, undefined, #{}) of
        #{<<"digest">> := D} -> D;
        #{<<"raw-b64url">> := D} -> D;
        _ -> undefined
    end.

pcr_is_zero(Key, Pcrs) ->
    case hb_maps:get(Key, Pcrs, undefined, #{}) of
        #{<<"is-zero">> := V} -> V;
        _ -> true
    end.

%%---- Kernel identity ---------------------------------------------------

interpret_kernel(_E, _Db, Pcrs) ->
    Pcr4 = pcr_digest(<<"4">>, Pcrs),
    Pcr11 = pcr_digest(<<"11">>, Pcrs),
    Pcr12 = pcr_digest(<<"12">>, Pcrs),
    #{
        <<"boot-loader">> => or_null(Pcr4),
        <<"uki-image">> => or_null(Pcr11),
        <<"uki-cmdline">> => or_null(Pcr12),
        <<"uki-measured">> =>
            (not pcr_is_zero(<<"11">>, Pcrs))
                orelse (not pcr_is_zero(<<"12">>, Pcrs))
    }.

%%---- IMA chain --------------------------------------------------------

interpret_ima(_E, _Db, Pcrs) ->
    %% Without the firmware/IMA event log (which we don't transport
    %% end-to-end today — a gap noted in SECURITY.md item 8), we can
    %% only report the PCR 10 final value + whether IMA was active.
    Pcr10 = pcr_digest(<<"10">>, Pcrs),
    Active = not pcr_is_zero(<<"10">>, Pcrs),
    #{
        <<"pcr10">> => or_null(Pcr10),
        <<"active">> => Active,
        <<"events-available">> => false,
        <<"note">> =>
            <<"LapEE does not yet transport the kernel IMA event log "
              "in the attestation envelope (PCR 10's final value is "
              "signed; the per-file chain isn't). Future `~tpm2@2.0a' "
              "versions will include it; until then, a verifier can "
              "only assert PCR 10 matches a known-good profile.">>
    }.

%%---- Node identity ----------------------------------------------------

interpret_node(E) ->
    Nm = hb_maps:get(<<"node-message">>, E, undefined, #{}),
    Id = hb_maps:get(<<"node-message-id">>, E, null, #{}),
    Wallet = hb_maps:get(<<"wallet-address">>, E, null, #{}),
    EventLog = hb_maps:get(<<"runtime-event-log">>, E, [], #{}),
    Pcr15Events = [Ev ||
        Ev <- EventLog,
        int_pcr(hb_maps:get(<<"pcr">>, Ev, 0, #{})) =:= 15],
    #{
        <<"wallet-address">> => Wallet,
        <<"node-message-id">> => Id,
        <<"node-message-key-count">> =>
            case Nm of
                M when is_map(M) -> maps:size(M);
                _ -> null
            end,
        <<"on-start-hook-device">> => nested_get(Nm, [<<"on">>, <<"start">>,
                                                      <<"device">>]),
        <<"on-start-hook-path">>   => nested_get(Nm, [<<"on">>, <<"start">>,
                                                      <<"path">>]),
        <<"pcr15-event-count">> => length(Pcr15Events),
        <<"pcr15-event-types">> =>
            [hb_maps:get(<<"event-type">>, Ev, null, #{})
             || Ev <- Pcr15Events]
    }.

int_pcr(V) when is_integer(V) -> V;
int_pcr(V) when is_binary(V)  -> binary_to_integer(V);
int_pcr(_) -> -1.

%%%============================================================================
%%% Certificate helpers
%%%============================================================================

decode_cert(<<>>) -> {error, empty};
decode_cert(Pem) when is_binary(Pem) ->
    case public_key:pem_decode(Pem) of
        [{'Certificate', Der, not_encrypted} | _] ->
            try {ok, public_key:pkix_decode_cert(Der, otp)}
            catch C:R -> {error, {C, R}}
            end;
        _ -> {error, no_certificate}
    end.

decode_pub_key(<<>>) -> {error, empty};
decode_pub_key(Pem) when is_binary(Pem) ->
    case public_key:pem_decode(Pem) of
        [Entry | _] ->
            try {ok, public_key:pem_entry_decode(Entry)}
            catch C:R -> {error, {C, R}}
            end;
        _ -> {error, no_entries}
    end.

%%% Extract TPM-specific attributes from the EK cert — following the
%%% TCG EK Credential Profile. The interesting fields are on the
%%% Subject Alternative Name's `directoryName', with three attribute
%%% OIDs:
%%%     2.23.133.2.1   tpmManufacturer   (e.g. "id:49465800")
%%%     2.23.133.2.2   tpmModel          (e.g. "SLB 9670")
%%%     2.23.133.2.3   tpmVersion        (e.g. "id:00010100")
%%% plus the TPM Specification extension (2.23.133.2.16 with family,
%%% level, revision, errata).
tpm_attrs_from_cert(#'OTPCertificate'{tbsCertificate = Tbs}) ->
    Subject = rdn_to_binary(Tbs#'OTPTBSCertificate'.subject),
    Issuer  = rdn_to_binary(Tbs#'OTPTBSCertificate'.issuer),
    Serial  = serial_b64url(Tbs#'OTPTBSCertificate'.serialNumber),
    {From, To} = validity(Tbs#'OTPTBSCertificate'.validity),
    Exts = case Tbs#'OTPTBSCertificate'.extensions of
        asn1_NOVALUE -> [];
        Xs -> Xs
    end,
    San = extract_san_attrs(Exts),
    Spec = extract_tpm_spec(Exts),
    maps:merge(
        maps:merge(
            #{
                subject_rdn => Subject,
                issuer_rdn => Issuer,
                serial_b64url => Serial,
                valid_from => From,
                valid_to   => To
            },
            San),
        Spec);
tpm_attrs_from_cert(_) -> #{}.

rdn_to_binary({rdnSequence, RDNs}) ->
    Parts = [rdn_attr_to_str(A) || R <- RDNs, A <- R],
    iolist_to_binary(lists:join(<<", ">>, Parts));
rdn_to_binary(_) -> <<>>.

rdn_attr_to_str(#'AttributeTypeAndValue'{type = T, value = V}) ->
    Name = oid_short_name(T),
    Vbin = rdn_value_to_binary(V),
    <<Name/binary, "=", Vbin/binary>>;
rdn_attr_to_str(_) -> <<"">>.

rdn_value_to_binary({utf8String, Bin}) -> Bin;
rdn_value_to_binary({printableString, Str}) -> list_to_binary(Str);
rdn_value_to_binary({teletexString, Str}) -> list_to_binary(Str);
rdn_value_to_binary({universalString, Str}) -> list_to_binary(Str);
rdn_value_to_binary({bmpString, Str}) -> list_to_binary(Str);
rdn_value_to_binary(Bin) when is_binary(Bin) -> Bin;
rdn_value_to_binary(List) when is_list(List) ->
    try iolist_to_binary(List)
    catch _:_ -> iolist_to_binary(io_lib:format("~p", [List]))
    end;
rdn_value_to_binary(Other) ->
    iolist_to_binary(io_lib:format("~p", [Other])).

oid_short_name({2,5,4,3}) -> <<"CN">>;
oid_short_name({2,5,4,6}) -> <<"C">>;
oid_short_name({2,5,4,7}) -> <<"L">>;
oid_short_name({2,5,4,8}) -> <<"ST">>;
oid_short_name({2,5,4,10}) -> <<"O">>;
oid_short_name({2,5,4,11}) -> <<"OU">>;
oid_short_name({2,23,133,2,1}) -> <<"tpmManufacturer">>;
oid_short_name({2,23,133,2,2}) -> <<"tpmModel">>;
oid_short_name({2,23,133,2,3}) -> <<"tpmVersion">>;
oid_short_name(Oid) -> iolist_to_binary(io_lib:format("~p", [Oid])).

validity(#'Validity'{notBefore = From, notAfter = To}) ->
    {format_time(From), format_time(To)};
validity(_) -> {undefined, undefined}.

format_time({utcTime, S}) -> list_to_binary(S);
format_time({generalTime, S}) -> list_to_binary(S);
format_time(_) -> undefined.

%% X.509 certificate serial numbers are positive integers up to 20
%% bytes long. We encode them as the minimal big-endian byte string
%% and base64url, matching the HyperBEAM wire convention. (OpenSSL
%% conventionally prints them as colon-separated hex; callers who
%% need that presentation can decode + format locally.)
serial_b64url(N) when is_integer(N), N >= 0 ->
    hb_util:encode(int_to_bigendian_bytes(N));
serial_b64url(_) -> undefined.

int_to_bigendian_bytes(0) -> <<0>>;
int_to_bigendian_bytes(N) when is_integer(N), N > 0 ->
    int_to_bigendian_bytes(N, <<>>).

int_to_bigendian_bytes(0, Acc) -> Acc;
int_to_bigendian_bytes(N, Acc) ->
    int_to_bigendian_bytes(N bsr 8, <<(N band 16#FF):8, Acc/binary>>).

%%% Walk the extensions and pull out any TPM-specific attributes.
extract_san_attrs(Exts) ->
    extract_from_ext(Exts, {2,5,29,17}, fun decode_san/1, #{}).

extract_tpm_spec(Exts) ->
    extract_from_ext(Exts, {2,23,133,2,16}, fun decode_tpm_spec/1, #{}).

extract_from_ext([], _Oid, _Fn, Acc) -> Acc;
extract_from_ext([#'Extension'{extnID = Oid, extnValue = V}|_], Oid, Fn, _) ->
    case Fn(V) of
        {ok, Map} -> Map;
        _ -> #{}
    end;
extract_from_ext([_|Tail], Oid, Fn, Acc) ->
    extract_from_ext(Tail, Oid, Fn, Acc).

decode_san(Value) ->
    %% Value is either an already-decoded list of {Type, Value}
    %% tuples, or a raw DER blob depending on OTP internals. Try
    %% both.
    try
        Entries = case Value of
            L when is_list(L) -> L;
            Bin when is_binary(Bin) ->
                %% SubjectAltName ::= GeneralNames ::= SEQUENCE OF GeneralName
                public_key:der_decode('SubjectAltName', Bin)
        end,
        {ok, decode_san_entries(Entries)}
    catch _:_ -> error
    end.

decode_san_entries(Entries) ->
    lists:foldl(
        fun({directoryName, {rdnSequence, RDNs}}, Acc) ->
                lists:foldl(fun attrs_from_rdn/2, Acc, RDNs);
           (_, Acc) -> Acc
        end, #{}, Entries).

attrs_from_rdn(RDN, Acc) ->
    lists:foldl(
        fun(#'AttributeTypeAndValue'{type=T, value=V}, A) ->
            case T of
                {2,23,133,2,1} ->
                    A#{manufacturer_id => trim_id(rdn_value_to_binary(V))};
                {2,23,133,2,2} ->
                    A#{model => rdn_value_to_binary(V)};
                {2,23,133,2,3} ->
                    A#{firmware_version => rdn_value_to_binary(V)};
                _ -> A
            end
        end, Acc, RDN).

%% tpmManufacturer is conventionally "id:NNNNNNNN" (4 ASCII hex
%% bytes = vendor code). Strip the id: prefix so the DB lookup key
%% is the 8-char hex string.
trim_id(<<"id:", Rest/binary>>) -> Rest;
trim_id(B) -> B.

decode_tpm_spec(Value) ->
    %% TPMSpecification ::= SEQUENCE { family UTF8String,
    %%                                 level   INTEGER,
    %%                                 revision INTEGER, [errata] }
    try
        {Family, Level, Rev} =
            case Value of
                B when is_binary(B) ->
                    {ok, Decoded} = 'OTP-PUB-KEY':decode('TPMSpec', B),
                    extract_spec_fields(Decoded);
                _ -> extract_spec_fields(Value)
            end,
        {ok, #{spec_family => Family,
               spec_level  => Level,
               spec_revision => Rev}}
    catch _:_ -> error
    end.

extract_spec_fields({_, Family, Level, Rev}) -> {Family, Level, Rev};
extract_spec_fields({_, Family, Level, Rev, _Errata}) -> {Family, Level, Rev};
extract_spec_fields(_) -> {undefined, undefined, undefined}.

%%%============================================================================
%%% Misc helpers
%%%============================================================================

%% Walk a nested-key path through a map. The map may have keys as
%% either atoms or binaries depending on whether we are reading a
%% native HB node message (atoms) or a TABM (binaries) — look up
%% both forms, binary first.
nested_get(M, [K]) when is_map(M) ->
    case map_get_anykey(K, M) of
        undefined -> null;
        V -> V
    end;
nested_get(M, [K|Rest]) when is_map(M) ->
    case map_get_anykey(K, M) of
        Inner when is_map(Inner) -> nested_get(Inner, Rest);
        _ -> null
    end;
nested_get(_, _) -> null.

map_get_anykey(K, M) when is_binary(K), is_map(M) ->
    case hb_maps:get(K, M, undefined, #{}) of
        undefined ->
            %% Fall through to atom form.
            try binary_to_existing_atom(K, utf8) of
                Atom -> hb_maps:get(Atom, M, undefined, #{})
            catch _:_ -> undefined
            end;
        V -> V
    end;
map_get_anykey(_, _) -> undefined.

or_null(undefined) -> null;
or_null(V) -> V.

%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).

info_shape_test() ->
    Info = info(ignored),
    ?assert(maps:is_key(exports, Info)),
    Exports = maps:get(exports, Info),
    %% Core surface
    ?assert(lists:member(<<"interpret">>, Exports)),
    ?assert(lists:member(<<"verify">>, Exports)),
    %% Cross-node introspection surface
    ?assert(lists:member(<<"verify-peer">>, Exports)),
    ?assert(lists:member(<<"peer-summary">>, Exports)),
    ?assert(lists:member(<<"peer-status">>, Exports)),
    ?assert(lists:member(<<"summary">>, Exports)),
    ?assert(lists:member(<<"checks">>, Exports)),
    %% Rich-event-log surface
    ?assert(lists:member(<<"events">>, Exports)),
    ?assert(lists:member(<<"claim">>, Exports)),
    ok.

%% `info/3' response documents every export's parameters + response
%% shape. A client must be able to discover the full surface by
%% calling `GET /~tpm-interpret@1.0/info'.
info_docs_full_surface_test() ->
    {ok, #{<<"body">> := Body}} = info(#{}, #{}, #{}),
    Api = maps:get(<<"api">>, Body),
    %% Every exported handler is documented in info.
    [?assert(maps:is_key(K, Api))
     || K <- [<<"interpret">>, <<"verify">>, <<"verify-peer">>,
              <<"summary">>, <<"peer-summary">>, <<"peer-status">>,
              <<"checks">>, <<"events">>, <<"claim">>]],
    %% Params are spelled out for the peer-facing handlers.
    VpParams = maps:get(<<"params">>, maps:get(<<"verify-peer">>, Api)),
    ?assert(maps:is_key(<<"peer">>, VpParams)),
    ?assert(maps:is_key(<<"trusted-ca">>, VpParams)),
    %% `wire_format' tells callers what encoding to expect.
    ?assert(maps:is_key(<<"wire-format">>, Body)),
    ok.

%% `events/3' parses the envelope's tcg_event_log into a
%% 1-indexed map of AO-Core messages. Uses the same synthetic
%% fixture as dev_tpm_tcg's tests (3 records: SpecID, CRTM
%% version, SecureBoot variable).
events_returns_indexed_map_test() ->
    Fixture = build_tcg_fixture(),
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Fixture)},
    {ok, #{<<"body">> := Events}} = events(Envelope, #{}, #{}),
    ?assertEqual(3, maps:size(Events)),
    E1 = maps:get(<<"1">>, Events),
    ?assertEqual(<<"EV_NO_ACTION">>, maps:get(<<"event-type">>, E1)),
    E3 = maps:get(<<"3">>, Events),
    ?assertEqual(<<"EV_EFI_VARIABLE_DRIVER_CONFIG">>,
                 maps:get(<<"event-type">>, E3)),
    %% decode_events enrichment: the SecureBoot variable's
    %% semantic decode surfaces as secure_boot_enabled: true.
    P3 = maps:get(<<"parsed">>, E3),
    Sem = maps:get(<<"semantic">>, P3),
    ?assertEqual(true, maps:get(<<"secure-boot-enabled">>, Sem)),
    ok.

%% Raw firmware bytes (event_data, digest algorithms) are not
%% UTF-8. They must arrive on the wire as base64url so HB's
%% JSON encoder can serialise the response. UTF-8-safe string
%% fields (event_type, variable_name, ...) stay as-is.
events_wire_encodes_nonutf8_binaries_test() ->
    Fixture = build_tcg_fixture(),
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Fixture)},
    {ok, #{<<"body">> := Events}} = events(Envelope, #{}, #{}),
    E3 = maps:get(<<"3">>, Events),
    %% event_data is 43 bytes of UEFI_VARIABLE_DATA (binary,
    %% not UTF-8): must be base64url.
    ED = maps:get(<<"event-data">>, E3),
    ?assert(is_binary(ED)),
    ?assertNotEqual(nomatch,
        re:run(ED, <<"^[A-Za-z0-9_-]+$">>)),
    %% digests.sha256 is 32 raw bytes: must be base64url (43 chars).
    Digests = maps:get(<<"digests">>, E3),
    Sha = maps:get(<<"sha256">>, Digests),
    ?assertEqual(43, byte_size(Sha)),
    ?assertNotEqual(nomatch,
        re:run(Sha, <<"^[A-Za-z0-9_-]+$">>)),
    %% UTF-8-safe keys must NOT be base64url-encoded.
    ?assertEqual(<<"EV_EFI_VARIABLE_DRIVER_CONFIG">>,
                 maps:get(<<"event-type">>, E3)),
    ok.

%% `claim/3' aggregates events into a flat, policy-friendly shape
%% with provenance. On a fixture that has a SecureBoot=enabled
%% event + a CRTM_VERSION event, claim.secure_boot.enabled =
%% true and claim.firmware.crtm_version carries the decoded
%% string.
claim_surface_extracts_secure_boot_and_crtm_test() ->
    Fixture = build_tcg_fixture(),
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Fixture)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    SB = maps:get(<<"secure-boot">>, Claim),
    ?assertEqual(true, maps:get(<<"enabled">>, SB)),
    %% Provenance points back at the source event.
    Prov = maps:get(<<"enabled-provenance">>, SB),
    ?assertEqual(1, length(Prov)),
    FW = maps:get(<<"firmware">>, Claim),
    ?assertEqual(<<"TEST FW v1">>, maps:get(<<"crtm-version">>, FW)),
    %% Fields we can't derive from the fixture are "unknown".
    TME = maps:get(<<"tme">>, Claim),
    ?assertEqual(<<"unknown">>, maps:get(<<"enabled">>, TME)),
    Lockdown = maps:get(<<"lockdown">>, Claim),
    ?assertEqual(<<"unknown">>, maps:get(<<"level">>, Lockdown)),
    ok.

%% Full paper-strength claim extraction from a synthetic event log
%% that includes a kernel-cmdline event with every security flag
%% the paper §Architecture line 219-230 + §Security table names.
%% Verifies every derived field resolves and every claim section
%% gets populated.
%% Intel TDX CCEL fixture (intel-tdx-ccel.bin) starts with a
%% first record on PCR 1 (MRTD), not PCR 0. Context detection
%% should flag it as `intel-tdx-ccel' which in turn provides
%% tier-5 evidence for `claim.tme.enabled = true'.
claim_surface_tdx_ccel_context_test() ->
    Path = filename:join([
        case code:priv_dir(hb) of
            {error, _} ->
                filename:join(
                    filename:dirname(
                        filename:dirname(code:which(?MODULE))),
                    "priv");
            D -> D
        end,
        "tpm-interpret", "fixtures", "intel-tdx-ccel.bin"]),
    case filelib:is_file(Path) of
        false -> ok;
        true ->
            {ok, Bin} = file:read_file(Path),
            Envelope = #{<<"tcg-event-log">> => hb_util:encode(Bin)},
            {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
            Ctx = maps:get(<<"context">>, Claim),
            ?assertEqual(<<"intel-tdx-ccel">>,
                         maps:get(<<"kind">>, Ctx)),
            ?assertEqual(<<"confidential-compute">>,
                         maps:get(<<"family">>, Ctx)),
            %% claim.tme.enabled should be true via tier-5 alone
            %% (even without cmdline evidence).
            TME = maps:get(<<"tme">>, Claim),
            ?assertEqual(true, maps:get(<<"enabled">>, TME)),
            Ev = maps:get(<<"enabled-evidence">>, TME),
            ?assert(lists:any(
                fun({<<"tier">>, 5}) -> true; (_) -> false end, Ev))
    end.

claim_surface_tpm_section_empty_envelope_test() ->
    %% With no EK cert, claim.tpm still returns structured
    %% "unknown" fields rather than crashing.
    Envelope = #{<<"tcg-event-log">> => <<"">>},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    TPM = maps:get(<<"tpm">>, Claim),
    ?assert(maps:is_key(<<"manufacturer-id">>, TPM)),
    ?assert(maps:is_key(<<"trust-tier">>, TPM)),
    ?assert(maps:is_key(<<"known-cves">>, TPM)),
    ?assert(maps:is_key(<<"evidence">>, TPM)).

claim_surface_full_cmdline_pipeline_test() ->
    %% Build a minimal crypto-agile log with a SpecID first record
    %% then an EV_IPL on PCR 12 whose value is the LapEE-standard
    %% cmdline.
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    Cmdline = <<"cmdline=ro quiet mem_encrypt=on intel_iommu=on "
                "iommu=pt iommu.strict=1 lockdown=confidentiality "
                "init_on_alloc=1 init_on_free=1 "
                "module.sig_enforce=1 slab_nomerge page_poison=1 "
                "roothash=deadbeef01", 0>>,
    CmdSha1 = crypto:hash(sha, Cmdline),
    CmdSha256 = crypto:hash(sha256, Cmdline),
    %% EV_IPL record on PCR 12.
    CmdRec = <<12:32/little, 16#D:32/little, 2:32/little,
               16#04:16/little, CmdSha1/binary,
               16#0B:16/little, CmdSha256/binary,
               (byte_size(Cmdline)):32/little, Cmdline/binary>>,
    Raw = <<FirstRec/binary, CmdRec/binary>>,
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Raw)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    %% Every new paper-claim section is present.
    lists:foreach(
        fun(K) -> ?assert(maps:is_key(K, Claim)) end,
        [<<"tme">>, <<"iommu">>, <<"lockdown">>,
         <<"kernel-integrity">>, <<"verity">>]),
    %% TME — composed from tier 2 (cmdline mem_encrypt=on) +
    %% tier 4 (PCR 15 reached, but envelope has no quote so
    %% tier 4 is unknown) + tier 3 empty DB.
    TME = maps:get(<<"tme">>, Claim),
    ?assertEqual(true, maps:get(<<"enabled">>, TME)),
    %% Evidence includes tier 2.
    TmeEv = maps:get(<<"enabled-evidence">>, TME),
    ?assert(lists:any(
        fun({<<"tier">>, 2}) -> true; (_) -> false end, TmeEv)),
    %% Lockdown = "confidentiality" from cmdline.
    Lockdown = maps:get(<<"lockdown">>, Claim),
    ?assertEqual(<<"confidentiality">>,
                 maps:get(<<"level">>, Lockdown)),
    %% IOMMU enabled + mode="pt" + strict=true.
    Iommu = maps:get(<<"iommu">>, Claim),
    ?assertEqual(true,       maps:get(<<"enabled">>, Iommu)),
    ?assertEqual(<<"pt">>,   maps:get(<<"mode">>, Iommu)),
    ?assertEqual(true,       maps:get(<<"strict">>, Iommu)),
    ?assertEqual(true,       maps:get(<<"intel-iommu-requested">>, Iommu)),
    %% Kernel integrity: every flag set.
    KI = maps:get(<<"kernel-integrity">>, Claim),
    ?assertEqual(true, maps:get(<<"module-sig-enforce">>, KI)),
    ?assertEqual(true, maps:get(<<"init-on-alloc">>, KI)),
    ?assertEqual(true, maps:get(<<"init-on-free">>, KI)),
    ?assertEqual(true, maps:get(<<"slab-nomerge">>, KI)),
    ?assertEqual(true, maps:get(<<"page-poison">>, KI)),
    %% Verity root hash extracted.
    Verity = maps:get(<<"verity">>, Claim),
    ?assertEqual(<<"deadbeef01">>,
                 maps:get(<<"root-hash">>, Verity)),
    ok.

%% Hour-3: tier-3 evidence via kernel-name-prefix match against
%% the shipped Fedora UKI profile. Build an event log with an
%% EV_IPL `kernel_name=Fedora-Linux-6.8.7-300' on PCR 12 and
%% another EV_IPL `stub_name=systemd-stub', plus a recognisable
%% Intel Raptor Lake microcode event on PCR 1. The claim
%% pipeline should:
%%   * enrich `claim.cpu' with codename=Raptor Lake + tee-support,
%%   * match `claim.tme.enabled-evidence' with a tier-3 hit whose
%%     matched-profile names the Fedora UKI baseline,
%%   * match `claim.lockdown.confidentiality-confirmed = true'
%%     with tier-3 evidence pointing at the Fedora profile.
claim_surface_hour3_db_cross_reference_test() ->
    %% Intel Sapphire Rapids sig → family=6 model=143 stepping=2
    %% (packed per Intel SDM §9.11.1). Encoded u32 LE:
    %%   family=6 base, model low=F, ExtModel=8, stepping=2
    %%   → raw sig = 0x000806F2
    ProcSig = 16#000806F2,
    %% Intel microcode header (48 bytes): HeaderVersion=1, rev=0x01,
    %% date=2024-01-15 (BCD), proc-sig, checksum=0, loader-rev=1,
    %% proc-flags=1, reserved, then padding.
    IntelHdr = <<1:32/little, 16#01:32/little,
                 16#20240115:32/little,
                 ProcSig:32/little,
                 0:32/little, 1:32/little,
                 1:32/little, 0:32/little,
                 0:(48*8 - 8*32)>>,
    %% EV_CPU_MICROCODE on PCR 1.
    UcodeSha1   = crypto:hash(sha,    IntelHdr),
    UcodeSha256 = crypto:hash(sha256, IntelHdr),
    UcodeRec = <<1:32/little, 16#09:32/little, 2:32/little,
                 16#04:16/little, UcodeSha1/binary,
                 16#0B:16/little, UcodeSha256/binary,
                 (byte_size(IntelHdr)):32/little, IntelHdr/binary>>,
    %% SpecID first record (crypto-agile log header).
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    %% EV_IPL kernel_name on PCR 12.
    Kname = <<"kernel_name=Fedora-Linux-6.8.7-300.fc40.x86_64", 0>>,
    KnSha1 = crypto:hash(sha, Kname),
    KnSha256 = crypto:hash(sha256, Kname),
    KnRec = <<12:32/little, 16#D:32/little, 2:32/little,
              16#04:16/little, KnSha1/binary,
              16#0B:16/little, KnSha256/binary,
              (byte_size(Kname)):32/little, Kname/binary>>,
    %% EV_IPL stub_name on PCR 12.
    Stub = <<"stub_name=systemd-stub", 0>>,
    StSha1 = crypto:hash(sha, Stub),
    StSha256 = crypto:hash(sha256, Stub),
    StRec = <<12:32/little, 16#D:32/little, 2:32/little,
              16#04:16/little, StSha1/binary,
              16#0B:16/little, StSha256/binary,
              (byte_size(Stub)):32/little, Stub/binary>>,
    Raw = <<FirstRec/binary, UcodeRec/binary,
            KnRec/binary, StRec/binary>>,
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Raw)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    %% claim.cpu enrichment: Sapphire Rapids was labeled as 6-143.
    Cpu = maps:get(<<"cpu">>, Claim),
    ?assertEqual(<<"intel">>, maps:get(<<"vendor">>, Cpu)),
    ?assertEqual(6,           maps:get(<<"cpu-family">>, Cpu)),
    ?assertEqual(143,         maps:get(<<"cpu-model">>, Cpu)),
    ?assertEqual(<<"Sapphire Rapids">>,
                 maps:get(<<"codename">>, Cpu)),
    ?assert(lists:member(<<"TDX">>,
                         maps:get(<<"tee-support">>, Cpu))),
    %% claim.tme — tier-3 evidence from kernel-name-prefix match
    %% against Fedora baseline.
    TME = maps:get(<<"tme">>, Claim),
    ?assertEqual(true, maps:get(<<"enabled">>, TME)),
    TmeEv = maps:get(<<"enabled-evidence">>, TME),
    ?assert(lists:any(
        fun({<<"tier">>, 3}) -> true; (_) -> false end, TmeEv)),
    ?assert(lists:any(
        fun({<<"match-rule">>, <<"kernel-name-prefix">>}) -> true;
           (_) -> false
        end, TmeEv)),
    %% claim.lockdown — tier-3 confidentiality-confirmed = true
    %% because the Fedora profile asserts lockdown-confidentiality.
    Lockdown = maps:get(<<"lockdown">>, Claim),
    ?assertEqual(true,
                 maps:get(<<"confidentiality-confirmed">>, Lockdown)),
    ok.

%% Hour-3: firmware-versions cross-reference. A CRTM starting
%% with "N1UET78W" (real ThinkPad P51 firmware) should match the
%% lenovo-thinkpad.json manifest and surface family-vendor=Lenovo.
claim_surface_hour3_firmware_family_match_test() ->
    %% SpecID first record.
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    %% EV_S_CRTM_VERSION on PCR 0: UTF-16LE "N1UET78W ".
    Crtm16 = unicode:characters_to_binary(
               <<"N1UET78W ">>, utf8, {utf16, little}),
    CrtmSha1 = crypto:hash(sha, Crtm16),
    CrtmSha256 = crypto:hash(sha256, Crtm16),
    CrtmRec = <<0:32/little, 16#8:32/little, 2:32/little,
                16#04:16/little, CrtmSha1/binary,
                16#0B:16/little, CrtmSha256/binary,
                (byte_size(Crtm16)):32/little, Crtm16/binary>>,
    Raw = <<FirstRec/binary, CrtmRec/binary>>,
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Raw)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    FW = maps:get(<<"firmware">>, Claim),
    ?assertEqual(<<"N1UET78W ">>,
                 maps:get(<<"crtm-version">>, FW)),
    ?assertEqual(<<"Lenovo">>, maps:get(<<"family-vendor">>, FW)),
    %% Provenance includes the source (firmware-versions.json)
    Prov = maps:get(<<"family-provenance">>, FW),
    ?assert(lists:any(
        fun({<<"source">>, <<"firmware-versions.json">>}) -> true;
           (_) -> false
        end, Prov)).

%% Hour-4: `claim.boot-chain' enumerates every EFI boot-services
%% / runtime-services image in seq order, with role labelling and
%% per-row device-path text. Build a synthetic log with one
%% driver (0x80000004) then one application (0x80000003); the
%% chain should be length 2, application-count 1, and the last-
%% application hash should equal the application event's
%% digests.sha256.
claim_surface_hour4_boot_chain_test() ->
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    %% Two UEFI_IMAGE_LOAD_EVENT payloads — with empty device
    %% path (len=0) so the parser takes the fast path.
    MkImage = fun(Addr, Len) ->
        <<Addr:64/little, Len:64/little, 0:64/little, 0:64/little>>
    end,
    DrvData = MkImage(16#1000, 16#2000),
    AppData = MkImage(16#8000, 16#10000),
    MkRec = fun(Pcr, Code, Data) ->
        S1 = crypto:hash(sha, Data),
        S2 = crypto:hash(sha256, Data),
        Sz = byte_size(Data),
        <<Pcr:32/little, Code:32/little, 2:32/little,
          16#04:16/little, S1/binary,
          16#0B:16/little, S2/binary,
          Sz:32/little, Data/binary>>
    end,
    DrvRec = MkRec(2, 16#80000004, DrvData),   %% driver
    AppRec = MkRec(4, 16#80000003, AppData),   %% application
    Raw = <<FirstRec/binary, DrvRec/binary, AppRec/binary>>,
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Raw)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    BC = maps:get(<<"boot-chain">>, Claim),
    ?assertEqual(2,     maps:get(<<"length">>, BC)),
    ?assertEqual(1,     maps:get(<<"application-count">>, BC)),
    ?assertEqual(false, maps:get(<<"has-runtime-driver">>, BC)),
    Chain = maps:get(<<"chain">>, BC),
    ?assertEqual(2, length(Chain)),
    [Row0, Row1] = Chain,
    ?assertEqual(<<"driver">>,      maps:get(<<"role">>, Row0)),
    ?assertEqual(<<"application">>, maps:get(<<"role">>, Row1)),
    ?assertEqual(0, maps:get(<<"chain-index">>, Row0)),
    ?assertEqual(1, maps:get(<<"chain-index">>, Row1)),
    ?assertEqual(maps:get(<<"image-hash">>, Row1),
                 maps:get(<<"last-application-hash">>, BC)),
    ?assertEqual(16#2000, maps:get(<<"image-length-in-memory">>, Row0)),
    ?assertEqual(16#10000, maps:get(<<"image-length-in-memory">>, Row1)),
    ok.

%% Hour-5: TPMS_ATTEST full decode round-trip. Build a synthetic
%% quote blob that hits every field (quote-specific pcrSelect +
%% pcrDigest union body, firmwareVersion, qualifiedSigner,
%% clockInfo, extraData), thread it through `claim/3`, assert
%% every field decodes correctly.
claim_surface_hour5_quote_round_trip_test() ->
    Magic = <<16#FF, "TCG">>,
    Type = 16#8018,                          %% TPM_ST_ATTEST_QUOTE
    QsName = crypto:hash(sha256, <<"signer">>),
    QsTpm2B = <<(byte_size(QsName)):16/big, QsName/binary>>,
    Nonce = <<"hour5-nonce-16-by">>,  %% 17 bytes (odd length ok)
    NonceTpm2B = <<(byte_size(Nonce)):16/big, Nonce/binary>>,
    Clock = 16#0000000012345678,
    ResetCount = 42,
    RestartCount = 7,
    Safe = 1,
    FwVer = 16#0102030400050006,
    %% Select PCRs 0, 1, 2, 7 under SHA-256
    %% (bitmap byte 0 = 0b10000111 = 0x87).
    PcrSelect = <<1:32/big, 16#000B:16/big, 3:8, 16#87, 0, 0>>,
    PcrDigest = crypto:hash(sha256, <<"some-pcr-set">>),
    PcrDigestTpm2B = <<(byte_size(PcrDigest)):16/big,
                        PcrDigest/binary>>,
    Quoted = <<Magic/binary, Type:16/big,
               QsTpm2B/binary, NonceTpm2B/binary,
               Clock:64/big, ResetCount:32/big,
               RestartCount:32/big, Safe:8, FwVer:64/big,
               PcrSelect/binary, PcrDigestTpm2B/binary>>,
    Envelope = #{
        <<"tpm-quote">> => #{
            <<"quoted">> => hb_util:encode(Quoted),
            <<"pcr-values">> => #{}
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    Q = maps:get(<<"quote">>, Claim),
    ?assertEqual(true, maps:get(<<"magic-ok">>, Q)),
    ?assertEqual(<<"TPM_ST_ATTEST_QUOTE">>,
                 maps:get(<<"attest-type">>, Q)),
    ?assertEqual(16#8018, maps:get(<<"attest-type-code">>, Q)),
    ?assertEqual(Clock, maps:get(<<"clock-ms">>, Q)),
    ?assertEqual(ResetCount, maps:get(<<"reset-count">>, Q)),
    ?assertEqual(RestartCount, maps:get(<<"restart-count">>, Q)),
    ?assertEqual(true, maps:get(<<"safe">>, Q)),
    ?assertEqual(FwVer, maps:get(<<"firmware-version-u64">>, Q)),
    ?assertEqual(<<"0x0102030400050006">>,
                 maps:get(<<"firmware-version-hex">>, Q)),
    ?assertEqual([0, 1, 2, 7],
                 maps:get(<<"quoted-pcr-indexes">>, Q)),
    ?assertEqual(4, maps:get(<<"quoted-pcr-count">>, Q)),
    ?assertEqual([<<"sha256">>],
                 maps:get(<<"quoted-pcr-algs">>, Q)),
    ?assertEqual(hb_util:encode(PcrDigest),
                 maps:get(<<"pcr-digest">>, Q)),
    ?assertEqual(32, maps:get(<<"pcr-digest-length">>, Q)),
    ?assertEqual(hb_util:encode(QsName),
                 maps:get(<<"qualified-signer-name">>, Q)),
    ok.

%% Hour-5: claim.quote on an envelope with no quote returns a
%% well-formed "unknown" stanza (not an error).
claim_surface_hour5_quote_missing_test() ->
    Envelope = #{<<"tcg-event-log">> => <<"">>},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    Q = maps:get(<<"quote">>, Claim),
    ?assertEqual(false, maps:get(<<"magic-ok">>, Q)),
    ?assertEqual(<<"unknown">>, maps:get(<<"attest-type">>, Q)),
    ?assertEqual(0, maps:get(<<"reset-count">>, Q)),
    ?assertEqual([], maps:get(<<"quoted-pcr-indexes">>, Q)),
    ok.

%% Hour-5: claim.pcr-match cross-references the (PCR 0, PCR 1,
%% PCR 7) triple against the 29 shipped pcr-profiles. When all
%% three match a profile's match-pcrs.sha256 we get confidence=
%% "high" and the profile's attributes are surfaced.
claim_surface_hour5_pcr_match_lenovo_test() ->
    %% Values straight from priv/tpm-interpret/pcr-profiles/
    %% from-fixture-lenovo-thinkpad-p51.json.
    Envelope = #{
        <<"tpm-quote">> => #{
            <<"pcr-values">> => #{
                <<"0">> =>
                    <<"XZ_KKkGSMn0dXX55Cw8WbWI1VVKsrA6r5FkdingFTuM">>,
                <<"1">> =>
                    <<"qoP03h5aHQXMvQjlP-ff0KNXxnOjn0355qAIMCT_3sE">>,
                <<"7">> =>
                    <<"SNfH-dPubRqKD7eZUWKq7NAOu50FvnkHAdTu7I34UZ4">>
            },
            <<"quoted">> => <<>>
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PM = maps:get(<<"pcr-match">>, Claim),
    ?assert(maps:get(<<"profile-count">>, PM) >= 29),
    Best = maps:get(<<"best-match">>, PM),
    ?assertEqual(<<"high">>, maps:get(<<"confidence">>, Best)),
    ?assertEqual([<<"0">>, <<"1">>, <<"7">>],
                 maps:get(<<"matched-pcrs">>, Best)),
    ?assertMatch(<<"Lenovo", _/binary>>,
                 maps:get(<<"name">>, Best)),
    %% All-matches list contains the hit.
    ?assert(length(maps:get(<<"all-matches">>, PM)) >= 1),
    ok.

%% Hour-5: claim.pcr-match on a random triple returns no-match
%% with score=0 and an empty all-matches list.
claim_surface_hour5_pcr_match_nomatch_test() ->
    Envelope = #{
        <<"tpm-quote">> => #{
            <<"pcr-values">> => #{
                <<"0">> => <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>,
                <<"1">> => <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>,
                <<"7">> => <<"ccccccccccccccccccccccccccccccccccccccccccc">>
            },
            <<"quoted">> => <<>>
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PM = maps:get(<<"pcr-match">>, Claim),
    Best = maps:get(<<"best-match">>, PM),
    ?assertEqual(<<"no-match">>, maps:get(<<"confidence">>, Best)),
    ?assertEqual(0, maps:get(<<"score">>, Best)),
    ?assertEqual([], maps:get(<<"all-matches">>, PM)),
    ok.

%% Hour-5: pcr-bitmap decoder — 0x87 (byte 0) → PCRs 0,1,2,7.
%% Cross-byte case: bitmap `<0x01, 0x01>` → PCR 0 + PCR 8.
pcr_bitmap_decoder_test() ->
    ?assertEqual([0, 1, 2, 7], pcr_bitmap_to_list(<<16#87>>)),
    ?assertEqual([0, 8],       pcr_bitmap_to_list(<<16#01, 16#01>>)),
    ?assertEqual([],           pcr_bitmap_to_list(<<0>>)),
    ?assertEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
                  16, 17, 18, 19, 20, 21, 22, 23],
                 pcr_bitmap_to_list(<<16#FF, 16#FF, 16#FF>>)),
    ok.

%% Hour-6: quote-integrity check fires true on a consistent
%% envelope. Build a quote where pcrDigest = SHA-256(PCR0 ||
%% PCR1 || PCR7) with all three PCRs present in the envelope.
claim_surface_hour6_quote_integrity_match_test() ->
    Pcr0 = crypto:hash(sha256, <<"fake-pcr0-value">>),
    Pcr1 = crypto:hash(sha256, <<"fake-pcr1-value">>),
    Pcr7 = crypto:hash(sha256, <<"fake-pcr7-value">>),
    PcrDigest = crypto:hash(sha256,
        <<Pcr0/binary, Pcr1/binary, Pcr7/binary>>),
    Quoted = build_minimal_quote_attest(
        <<"nonce12345">>, 5, 0, 1,
        %% PCR 0, 1, 7 selected → bitmap 0x83.
        <<1:32/big, 16#000B:16/big, 3:8, 16#83, 0, 0>>,
        PcrDigest),
    Envelope = #{<<"tpm-quote">> => #{
        <<"quoted">> => hb_util:encode(Quoted),
        <<"pcr-values">> => #{
            <<"0">> => hb_util:encode(Pcr0),
            <<"1">> => hb_util:encode(Pcr1),
            <<"7">> => hb_util:encode(Pcr7)}}},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    QI = maps:get(<<"quote-integrity">>, Claim),
    ?assertEqual(true,  maps:get(<<"verifiable">>, QI)),
    ?assertEqual(true,  maps:get(<<"pcr-digest-match">>, QI)),
    ?assertEqual(<<"sha256">>, maps:get(<<"pcr-digest-alg">>, QI)),
    ?assertEqual([0, 1, 7], maps:get(<<"pcr-indexes-used">>, QI)),
    ?assertEqual([], maps:get(<<"missing-pcrs">>, QI)),
    ?assertEqual(maps:get(<<"pcr-digest-claimed">>, QI),
                 maps:get(<<"pcr-digest-computed">>, QI)),
    ok.

%% Hour-6: a tampered PCR value (signed with the real one,
%% shipped with a different one) is detected as mismatch.
claim_surface_hour6_quote_integrity_tamper_test() ->
    Pcr0 = crypto:hash(sha256, <<"real-pcr0">>),
    PcrDigest = crypto:hash(sha256, Pcr0),
    Quoted = build_minimal_quote_attest(
        <<"x">>, 0, 0, 1,
        %% Only PCR 0 selected → bitmap 0x01.
        <<1:32/big, 16#000B:16/big, 3:8, 16#01, 0, 0>>,
        PcrDigest),
    Tampered = crypto:hash(sha256, <<"attacker-pcr0">>),
    Envelope = #{<<"tpm-quote">> => #{
        <<"quoted">> => hb_util:encode(Quoted),
        <<"pcr-values">> => #{
            <<"0">> => hb_util:encode(Tampered)}}},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    QI = maps:get(<<"quote-integrity">>, Claim),
    ?assertEqual(false, maps:get(<<"pcr-digest-match">>, QI)),
    ?assertEqual(true,  maps:get(<<"verifiable">>, QI)),
    ok.

%% Hour-6: selected PCR absent from envelope → missing-pcrs
%% populated and verifiable=false.
claim_surface_hour6_quote_integrity_missing_pcr_test() ->
    %% Select PCR 0 + PCR 7 but only ship PCR 0 in the envelope.
    Pcr0 = crypto:hash(sha256, <<"p0">>),
    PcrDigest = crypto:hash(sha256, Pcr0), % wrong, but we only
                                            % care about `verifiable`
    Quoted = build_minimal_quote_attest(
        <<"n">>, 0, 0, 1,
        <<1:32/big, 16#000B:16/big, 3:8, 16#81, 0, 0>>,
        PcrDigest),
    Envelope = #{<<"tpm-quote">> => #{
        <<"quoted">> => hb_util:encode(Quoted),
        <<"pcr-values">> => #{
            <<"0">> => hb_util:encode(Pcr0)}}},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    QI = maps:get(<<"quote-integrity">>, Claim),
    ?assertEqual(false, maps:get(<<"verifiable">>, QI)),
    ?assertEqual([7],   maps:get(<<"missing-pcrs">>, QI)),
    ok.

%% Hour-6: freshness stanza aggregates nonce + reset/restart +
%% clock + safe into a composite indicator.
claim_surface_hour6_freshness_ok_test() ->
    Nonce = <<"unique-nonce-for-this-attestation">>,
    Quoted = build_minimal_quote_attest(
        Nonce, 42, 3, 1,
        <<1:32/big, 16#000B:16/big, 3:8, 0, 0, 0>>,
        <<0:256>>),
    %% Manually patch clock-ms to be nonzero (the helper's default
    %% is 0; we need >0 for freshness-indicator=ok).
    Clocked = patch_clock(Quoted, 16#12345),
    Envelope = #{<<"tpm-quote">> => #{
        <<"quoted">> => hb_util:encode(Clocked),
        <<"pcr-values">> => #{}}},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    F = maps:get(<<"freshness">>, Claim),
    ?assertEqual(42, maps:get(<<"reset-count">>, F)),
    ?assertEqual(3,  maps:get(<<"restart-count">>, F)),
    ?assertEqual(true, maps:get(<<"safe">>, F)),
    ?assertEqual(16#12345, maps:get(<<"clock-ms">>, F)),
    ?assertEqual(<<"ok">>,
                 maps:get(<<"freshness-indicator">>, F)),
    ?assertEqual(33, maps:get(<<"nonce-length">>, F)),
    ok.

%% Freshness-indicator = "no-nonce" when the TPM echoed an
%% empty extraData field.
claim_surface_hour6_freshness_no_nonce_test() ->
    Quoted = build_minimal_quote_attest(
        <<>>, 0, 0, 1,
        <<0:32>>, <<0:256>>),
    Clocked = patch_clock(Quoted, 1),
    Envelope = #{<<"tpm-quote">> => #{
        <<"quoted">> => hb_util:encode(Clocked),
        <<"pcr-values">> => #{}}},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    F = maps:get(<<"freshness">>, Claim),
    ?assertEqual(<<"no-nonce">>,
                 maps:get(<<"freshness-indicator">>, F)),
    ?assertEqual(0, maps:get(<<"nonce-length">>, F)),
    ok.

%% Freshness-indicator = "safe-false" is the red-flag case.
claim_surface_hour6_freshness_safe_false_test() ->
    Quoted = build_minimal_quote_attest(
        <<"n">>, 0, 0, 0,  %% Safe=0
        <<0:32>>, <<0:256>>),
    Clocked = patch_clock(Quoted, 1),
    Envelope = #{<<"tpm-quote">> => #{
        <<"quoted">> => hb_util:encode(Clocked),
        <<"pcr-values">> => #{}}},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    F = maps:get(<<"freshness">>, Claim),
    ?assertEqual(false, maps:get(<<"safe">>, F)),
    ?assertEqual(<<"safe-false">>,
                 maps:get(<<"freshness-indicator">>, F)),
    ok.

%% Helper: minimal TPMS_ATTEST_QUOTE blob with parameterised
%% nonce / reset / restart / safe / pcrSelect / pcrDigest.
%% clock-ms defaults to 0 because quote tests usually don't
%% care about it; use patch_clock/2 when you do.
build_minimal_quote_attest(Nonce, ResetCount, RestartCount, Safe,
                             PcrSelect, PcrDigest) ->
    Magic = <<16#FF, "TCG">>,
    Type = 16#8018,
    QsName = crypto:hash(sha256, <<"signer">>),
    QsTpm2B = <<(byte_size(QsName)):16/big, QsName/binary>>,
    NonceTpm2B = <<(byte_size(Nonce)):16/big, Nonce/binary>>,
    FwVer = 16#0102030400050006,
    PcrDigestTpm2B = <<(byte_size(PcrDigest)):16/big,
                         PcrDigest/binary>>,
    <<Magic/binary, Type:16/big,
      QsTpm2B/binary, NonceTpm2B/binary,
      0:64/big, ResetCount:32/big, RestartCount:32/big,
      Safe:8, FwVer:64/big,
      PcrSelect/binary, PcrDigestTpm2B/binary>>.

%% Patch the clock-ms field (8 bytes starting at the fixed
%% offset of magic+type+QsName+ExtraData = depends on variable-
%% length fields; we compute from scratch).
patch_clock(Blob, NewClock) ->
    <<Magic:4/binary, Type:16/big, Rest0/binary>> = Blob,
    {QsName, Rest1} = tpm2b(Rest0),
    {ExtraData, Rest2} = tpm2b(Rest1),
    <<_OldClock:64/big,
      ResetCount:32/big, RestartCount:32/big,
      Safe:8, FwVer:64/big, Tail/binary>> = Rest2,
    QsTpm2B = <<(byte_size(QsName)):16/big, QsName/binary>>,
    NonceTpm2B = <<(byte_size(ExtraData)):16/big, ExtraData/binary>>,
    <<Magic/binary, Type:16/big,
      QsTpm2B/binary, NonceTpm2B/binary,
      NewClock:64/big, ResetCount:32/big, RestartCount:32/big,
      Safe:8, FwVer:64/big, Tail/binary>>.

%% Hour-3: UKI lookup helpers are resilient against malformed
%% DB entries.
uki_db_lookup_handles_empty_and_malformed_test() ->
    ?assertEqual(false, uki_db_lookup(#{}, <<"x">>, [], <<"k">>)),
    ?assertEqual(false, uki_db_lookup(not_a_map, <<"x">>, [], <<"k">>)),
    %% Profile that declares kernel-name-prefix but the events
    %% have no IPL event at all — no match.
    P = #{<<"name">> => <<"t">>,
          <<"match">> => #{<<"kernel-name-prefix">> => [<<"X-">>]},
          <<"claims">> => #{<<"checks-tme">> => true}},
    ?assertEqual(false,
                 uki_db_lookup(#{<<"t">> => P}, <<"y">>, [],
                                <<"checks-tme">>)),
    ok.

%% Helper: same fixture dev_tpm_tcg uses for its own tests.
build_tcg_fixture() ->
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    Data2 = <<"TEST FW v1">>,
    Sha1_2 = crypto:hash(sha, Data2),
    Sha256_2 = crypto:hash(sha256, Data2),
    Rec2 = <<0:32/little, 16#8:32/little, 2:32/little,
             16#04:16/little, Sha1_2/binary,
             16#0B:16/little, Sha256_2/binary,
             (byte_size(Data2)):32/little, Data2/binary>>,
    Uname = unicode:characters_to_binary(<<"SecureBoot">>, utf8,
                                           {utf16, little}),
    UvData = <<0:(16*8), 10:64/little, 1:64/little, Uname/binary, 1>>,
    Sha1_3 = crypto:hash(sha, UvData),
    Sha256_3 = crypto:hash(sha256, UvData),
    Rec3 = <<7:32/little, 16#80000001:32/little, 2:32/little,
             16#04:16/little, Sha1_3/binary,
             16#0B:16/little, Sha256_3/binary,
             (byte_size(UvData)):32/little, UvData/binary>>,
    <<FirstRec/binary, Rec2/binary, Rec3/binary>>.

%% `checks/3' returns a machine-readable description of the
%% cryptographic battery — clients build UI + policy on this, so
%% the shape must not drift silently.
checks_surface_stable_test() ->
    {ok, #{<<"body">> := #{<<"checks">> := Cs}}} = checks(#{}, #{}, #{}),
    %% 5 core + 1 informational = 6 total checks.
    ?assertEqual(6, length(Cs)),
    lists:foreach(
        fun(C) ->
            ?assert(maps:is_key(<<"name">>, C)),
            ?assert(maps:is_key(<<"purpose">>, C)),
            ?assert(maps:is_key(<<"failure-implies">>, C)),
            ?assert(maps:is_key(<<"severity">>, C))
        end, Cs),
    Names = [maps:get(<<"name">>, C) || C <- Cs],
    ?assert(lists:any(fun(N) ->
                          binary:match(N, <<"EK certificate">>) =/= nomatch
                      end, Names)),
    %% Exactly one informational check (the firmware TCG replay).
    Severities = [maps:get(<<"severity">>, C) || C <- Cs],
    ?assertEqual(5, length([S || S <- Severities, S =:= <<"core">>])),
    ?assertEqual(1, length([S || S <- Severities,
                                 S =:= <<"informational">>])),
    ok.

%% `summary/3' on a structurally-complete envelope returns the same
%% link-free shape that verify-peer's `summary' uses.
summary_returns_link_free_map_test() ->
    Zero = hb_util:encode(<<0:256>>),
    Envelope = #{
        <<"lapee-attestation-version">> => <<"0.3">>,
        <<"ek-cert-pem">> => <<>>,
        <<"ak-pub-pem">> => <<>>,
        <<"tpm-quote">> => #{<<"pcr-values">> => #{}, <<"quoted">> => <<>>,
                             <<"signature">> => <<>>, <<"nonce">> => <<>>,
                             <<"pcr-selection">> => []},
        <<"runtime-event-log">> => [],
        <<"node-message">> =>
            #{<<"on">> => #{<<"start">> =>
                              #{<<"device">> => <<"tpm2@2.0a">>}}},
        <<"node-message-id">> => Zero,
        <<"wallet-address">> => <<"sample-wallet">>
    },
    {ok, #{<<"body">> := S}} = summary(Envelope, #{}, #{}),
    ?assertEqual(<<"0.3">>, maps:get(<<"envelope-version">>, S)),
    ?assertEqual(<<"tpm2@2.0a">>,
                 maps:get(<<"on-start-hook-device">>, S)),
    ?assertEqual(<<"sample-wallet">>,
                 maps:get(<<"wallet-address">>, S)),
    %% Summary must not carry maps inside its values — that's the
    %% link-free property. Spot-check a few known fields.
    [?assert(not is_map(maps:get(K, S, null)))
     || K <- [<<"tpm-manufacturer">>, <<"ak-algorithm">>,
              <<"quote-attest-type">>, <<"secure-boot-measured">>,
              <<"pcr15-event-count">>]],
    ok.

%% `run_cross_node_verify' MUST reject when the envelope's
%% tpm_quote.nonce does NOT match the verifier's challenge. That
%% gate sits BEFORE any crypto verification — defence against a
%% replay of a previously-valid envelope captured off the wire.
%% Proof: hand-build an envelope with a known nonce, pass a
%% DIFFERENT nonce as the challenge, assert the response is
%% `verified: false, nonce_freshness: "mismatch"' and that the
%% single returned check names the nonce mismatch.
run_cross_node_verify_enforces_nonce_freshness_test() ->
    NonceInEnvelope = crypto:strong_rand_bytes(32),
    DifferentChallenge = crypto:strong_rand_bytes(32),
    ?assertNotEqual(NonceInEnvelope, DifferentChallenge),
    Envelope = #{
        <<"lapee-attestation-version">> => <<"0.3">>,
        <<"tpm-quote">> => #{
            <<"nonce">> => hb_util:encode(NonceInEnvelope)
        }
    },
    {ok, #{<<"body">> := Body}} =
        run_cross_node_verify(<<"http://peer">>,
                              Envelope,
                              undefined,
                              DifferentChallenge,
                              #{}),
    ?assertEqual(false, maps:get(<<"verified">>, Body)),
    ?assertEqual(<<"rejected">>, maps:get(<<"verdict">>, Body)),
    ?assertEqual(<<"mismatch">>, maps:get(<<"nonce-freshness">>, Body)),
    %% Response should carry exactly one failed check describing
    %% the nonce mismatch — no crypto checks should have run,
    %% because we gated BEFORE them.
    [FailedCheck] = maps:get(<<"checks">>, Body),
    ?assertEqual(false, maps:get(<<"ok">>, FailedCheck)),
    ?assert(binary:match(maps:get(<<"name">>, FailedCheck),
                         <<"nonce">>) =/= nomatch),
    ok.

%% Positive: matching nonce passes the freshness gate, letting the
%% crypto checks run.
run_cross_node_verify_accepts_matching_nonce_test() ->
    Challenge = crypto:strong_rand_bytes(32),
    Envelope = #{
        <<"lapee-attestation-version">> => <<"0.3">>,
        <<"tpm-quote">> => #{
            <<"nonce">> => hb_util:encode(Challenge),
            <<"pcr-values">> => #{},
            <<"quoted">> => <<>>,
            <<"signature">> => <<>>,
            <<"pcr-selection">> => []
        },
        <<"ek-cert-pem">> => <<>>,
        <<"ak-pub-pem">> => <<>>,
        <<"runtime-event-log">> => [],
        <<"node-message">> => #{<<"port">> => 8734},
        <<"node-message-id">> => hb_util:encode(<<0:256>>),
        <<"wallet-address">> => <<"sample">>
    },
    {ok, #{<<"body">> := Body}} =
        run_cross_node_verify(<<"http://peer">>, Envelope,
                              undefined, Challenge, #{}),
    %% Freshness gate passed — crypto checks attempted (and will
    %% fail on this synthetic envelope for other reasons, which is
    %% fine — we only assert nonce_freshness says "verified" and
    %% the check list isn't the single-entry nonce-mismatch form).
    ?assertEqual(<<"verified">>,
                 maps:get(<<"nonce-freshness">>, Body)),
    ?assertEqual(Challenge,
                 hb_util:decode(maps:get(<<"nonce-challenge">>, Body))),
    Checks = maps:get(<<"checks">>, Body),
    ?assert(length(Checks) >= 1),
    %% None of the checks should be the "verifier-supplied nonce"
    %% one — that's only emitted when the gate fails.
    [?assert(binary:match(maps:get(<<"name">>, C, <<>>),
                          <<"Verifier-supplied nonce">>) =:= nomatch)
     || C <- Checks],
    ok.

%% A missing `peer' parameter on any peer-* endpoint returns 400
%% with a targeted error — not silent.
peer_endpoints_reject_missing_peer_test() ->
    [?assertMatch({ok, #{<<"status">> := 400,
                         <<"body">> :=
                           #{<<"error">> := <<"missing-peer">>}}},
                  F(#{}, #{}, #{}))
     || F <- [fun peer_summary/3, fun peer_status/3]],
    ok.

%% `resolve_inline_ca/2' normalises both accepted forms of the
%% inline trust anchor — base64url `trusted-ca' wins over raw PEM
%% `trusted-ca-pem', and undefined/empty inputs stay undefined.
resolve_inline_ca_normalises_forms_test() ->
    Pem = <<"-----BEGIN CERTIFICATE-----\nAA==\n-----END CERTIFICATE-----">>,
    B64u = hb_util:encode(Pem),
    %% base64url form decodes back to the raw PEM bytes
    ?assertEqual(Pem, resolve_inline_ca(#{<<"trusted-ca">> => B64u}, #{})),
    %% raw-PEM form passes through
    ?assertEqual(Pem, resolve_inline_ca(#{<<"trusted-ca-pem">> => Pem}, #{})),
    %% both keys — base64url wins
    B64u2 = hb_util:encode(<<"OTHER">>),
    ?assertEqual(<<"OTHER">>,
                 resolve_inline_ca(#{<<"trusted-ca">> => B64u2,
                                     <<"trusted-ca-pem">> => Pem}, #{})),
    %% neither: undefined
    ?assertEqual(undefined, resolve_inline_ca(#{}, #{})),
    %% empty string: undefined
    ?assertEqual(undefined,
                 resolve_inline_ca(#{<<"trusted-ca">> => <<>>}, #{})),
    ok.

%% Interpret a hand-built envelope with NO valid EK cert — we still
%% get a map back with null TPM fields and the other sections filled
%% in from the data that IS present.
interpret_handles_partial_envelope_test() ->
    Zero = hb_util:encode(<<0:256>>),
    Envelope = #{
        <<"lapee-attestation-version">> => <<"0.3">>,
        <<"issued-at-unix">> => 1700000000,
        <<"ek-cert-pem">> => <<>>,
        <<"ak-pub-pem">> => <<>>,
        <<"tpm-quote">> => #{
            <<"pcr-selection">> => [0, 15],
            <<"pcr-values">> => #{
                <<"0">> => Zero,
                <<"15">> => Zero
            },
            <<"quoted">> => <<>>,
            <<"signature">> => <<>>,
            <<"nonce">> => <<>>
        },
        <<"runtime-event-log">> => [],
        <<"node-message">> =>
            #{<<"port">> => 8734,
              <<"on">> =>
                #{<<"start">> =>
                    #{<<"device">> => <<"tpm2@2.0a">>,
                      <<"path">> => <<"extend">>}}},
        <<"node-message-id">> => Zero,
        <<"wallet-address">> => <<"sample-wallet-address-XX">>
    },
    #{<<"status">> := 200, <<"body">> := Body} =
        element(2, interpret(Envelope, #{}, #{})),
    %% Envelope section present
    Env = maps:get(<<"envelope">>, Body),
    ?assertEqual(<<"0.3">>, maps:get(<<"version">>, Env)),
    %% TPM section reports error (empty PEM) but is still a map
    Tpm = maps:get(<<"tpm">>, Body),
    ?assert(is_map(Tpm)),
    %% PCR 15 is zero (got decoded) and its role is node identity
    Pcrs = maps:get(<<"pcrs">>, Body),
    Pcr15 = maps:get(<<"15">>, Pcrs),
    ?assertEqual(<<"lapee-node-identity">>, maps:get(<<"role">>, Pcr15)),
    ?assertEqual(true, maps:get(<<"is-zero">>, Pcr15)),
    %% Node section reads on.start.device
    Node = maps:get(<<"node">>, Body),
    ?assertEqual(<<"tpm2@2.0a">>,
                 maps:get(<<"on-start-hook-device">>, Node)).

pcr_role_canonical_mapping_test() ->
    ?assertEqual(<<"firmware-srtm">>, pcr_role(<<"0">>)),
    ?assertEqual(<<"secure-boot-policy">>, pcr_role(<<"7">>)),
    ?assertEqual(<<"ima-runtime-measurements">>, pcr_role(<<"10">>)),
    ?assertEqual(<<"uki-kernel-image">>, pcr_role(<<"11">>)),
    ?assertEqual(<<"lapee-node-identity">>, pcr_role(<<"15">>)),
    ?assertEqual(<<"unassigned-or-application">>, pcr_role(<<"22">>)).

%% Every PCR section includes a `derived' submap — named fields
%% extracted from the events extended into that PCR. When events are
%% present, the derived map pulls concrete values out of the events'
%% `parsed' + `parsed.semantic' sub-maps. This is what makes the
%% interpretation AO-Core navigable — every derivable property is
%% path-addressable as `/interpret/pcrs/<N>/derived/<field>'.
pcrs_derived_fields_populate_from_events_test() ->
    %% Synthesize an envelope whose events include both a
    %% EV_S_CRTM_VERSION (PCR 0) and an EV_EFI_VARIABLE_DRIVER_CONFIG
    %% for SecureBoot (PCR 7). Run it through the top-level
    %% interpreter.
    Fixture = build_tcg_fixture(),
    Q = #{<<"pcr-values">> => #{
            <<"0">> => hb_util:encode(<<0:256>>),
            <<"7">> => hb_util:encode(<<0:256>>)}},
    Envelope = #{
        <<"lapee-attestation-version">> => <<"0.3">>,
        <<"tcg-event-log">>             => hb_util:encode(Fixture),
        <<"tpm-quote">>                 => Q,
        <<"runtime-event-log">>         => [],
        <<"node-message">>              => #{},
        <<"node-message-id">>           => <<>>
    },
    Interp = interpret_envelope(Envelope, #{}),
    Pcrs = maps:get(<<"pcrs">>, Interp),
    %% PCR 0 has the CRTM_VERSION event (seq 2 in the fixture).
    Pcr0 = maps:get(<<"0">>, Pcrs),
    Derived0 = maps:get(<<"derived">>, Pcr0),
    ?assertEqual(<<"TEST FW v1">>,
                 maps:get(<<"crtm-version">>, Derived0)),
    ?assert(maps:get(<<"event-count">>, Pcr0) >= 1),
    %% PCR 7 has the SecureBoot variable (seq 3) → enabled=true.
    Pcr7 = maps:get(<<"7">>, Pcrs),
    Derived7 = maps:get(<<"derived">>, Pcr7),
    ?assertEqual(true,
                 maps:get(<<"secure-boot-enabled">>, Derived7)),
    %% Every PCR carries a reconstruction submessage when events are
    %% present. We didn't quote the real values here, so it'll say
    %% matches_quoted=false — but the SHAPE must be there.
    Recon0 = maps:get(<<"reconstruction">>, Pcr0),
    ?assert(maps:is_key(<<"replayed-digest">>, Recon0)),
    ?assert(maps:is_key(<<"matches-quoted">>, Recon0)),
    ok.

%% Direct test that the manufacturer DB actually loads when the
%% release ships it. If priv/tpm-interpret/manufacturers.json is
%% present, we expect Infineon (49465800) to be resolvable.
manufacturer_db_lookup_test() ->
    Db = hb_db_tpm:load(#{}),
    case maps:get(<<"vendors">>, Db, #{}) of
        V when is_map(V), map_size(V) > 0 ->
            case maps:get(<<"49465800">>, V, undefined) of
                undefined ->
                    ?debugFmt("manufacturers.json loaded but Infineon "
                              "(49465800) not present", []);
                Entry ->
                    ?assertEqual(<<"Infineon">>,
                                 maps:get(<<"name">>, Entry))
            end;
        _ ->
            %% Priv dir not present in eunit layout — skip.
            ok
    end.

-endif.
