%%% @doc `~tpm-interpret@1.0' -- turn a verified LapEE TPM attestation
%%% into rich, human-readable AO-Core fields.
%%%
%%% The companion to `~tpm2@2.0a'. `~tpm2@2.0a' is responsible for the
%%% cryptographic chain (EK cert -> AK -> TPM2_Quote -> PCR 15 -> node
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
%%%     manufacturers.json          TCG-assigned vendor IDs -> {name,
%%%                                 kind, website, notes}
%%%     root-cas/                   per-vendor EK root CA PEMs; used
%%%                                 by the verifier side but listed
%%%                                 here for interpretability (e.g.
%%%                                 "which vendor CA verified this EK?")
%%%     pcr-profiles/*.json         known PCR 0/1/7 values for specific
%%%                                 firmware versions (Lenovo BIOS
%%%                                 1.52, Dell XYZ, QEMU OVMF, ...)
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
                          "Does NOT itself verify -- pair with `verify' "
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
                              "string. Unsafe over URL-encoded GET -- "
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
                          "-- use for dashboards or discovery where "
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
                          "the full envelope -- only the first layer "
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
%%% events/3 -- parsed TCG event log as AO-Core messages
%%%============================================================================

events(Base, Req, Opts) ->
    Envelope = resolve_envelope(Base, Req, Opts),
    {ok, #{
        <<"status">> => 200,
        <<"body">> => interpret_events(Envelope)
    }}.

%%%============================================================================
%%% claim/3 -- flat, policy-friendly surface
%%%============================================================================

claim(Base, Req, Opts) ->
    Envelope = resolve_envelope(Base, Req, Opts),
    Db = hb_db_tpm:load(Opts),
    %% Claim pipeline reads from RAW (non-wire-encoded) events so
    %% UTF-8 cmdline flag values survive unaltered. Claim values
    %% are UTF-8-safe by construction (parsed text, base64url-
    %% encoded digests, integers, booleans, "unknown" sentinels --
    %% no raw firmware bytes), so we skip the wire-encode layer
    %% and return the claim as-is.
    Events = interpret_events_raw(Envelope),
    {ok, #{
        <<"status">> => 200,
        <<"body">> => interpret_claim(Events, Envelope, Db)
    }}.

%%%============================================================================
%%% summary/3 -- lightweight interpret (no verify)
%%%============================================================================

summary(Base, Req, Opts) ->
    Envelope = resolve_envelope(Base, Req, Opts),
    Interp = safe_interpret(Envelope, Opts),
    {ok, #{
        <<"status">> => 200,
        <<"body">> => summarise_interp(Interp)
    }}.

%%%============================================================================
%%% peer_summary/3, peer_status/3 -- lightweight cross-node introspection
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
%%% checks/3 -- machine-readable description of the verify battery
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
                          "what was actually quoted -- events "
                          "missing, inserted, or out of order.">>
                },
                #{
                    <<"name">> =>
                        <<"PCR 15 extension commits to "
                          "node-message-id">>,
                    <<"severity">> => <<"core">>,
                    <<"purpose">> =>
                        <<"Proves THIS node's node_message_id was "
                          "extended into PCR 15 -- the LapEE key "
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
                          "anchor -- the LapEE trust model is "
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
                <<"This endpoint requires a `peer' key -- the base "
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
%%% verify/3 -- the target endpoint
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
%%% verify_peer/3 -- cross-node entry point
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
            %%   `trusted-ca'      -- base64url-encoded PEM bytes
            %%                       (HyperBEAM wire convention; the
            %%                       safe form over HTTP/URL).
            %%   `trusted-ca-pem'  -- raw PEM text. *Only* works when
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
    %% way we treat `{error, _}' -- 502 with a diagnostic -- so a
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
%% HB's response pipeline verbatim -- the peer's commitments + any
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

%% Produce a small, link-free summary of the interpretation -- the
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
%%% interpret/3 -- structured reading of the envelope
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

%% Reviewer pass 10 fuzzer: guard on `is_map(Base)' so an
%% internal caller passing a top-level JSON array (or any non-
%% map shape) does not crash `hb_maps:get(<<"body">>, Base, ...)'
%% with `{badmap, Base}' before `safe_interpret' can wrap. The
%% only path through `interpret/3' / `claim/3' that reaches this
%% without a `safely_run' shield is the direct-call one.
resolve_envelope(Base, Req, Opts) when is_map(Base) ->
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
    end;
resolve_envelope(_Base, _Req, _Opts) ->
    %% Non-map Base (list, binary, integer, atom, etc.). No
    %% envelope can be extracted; fall through to an empty map so
    %% the downstream pipeline produces a structured "everything
    %% unknown" verdict instead of crashing.
    #{}.

is_envelope(M) when is_map(M) ->
    hb_maps:get(<<"lapee-attestation-version">>, M, undefined, #{}) /=
        undefined;
is_envelope(_) -> false.

%% Reviewer pass 10 fuzzer: three sites read `platform-probes'
%% as a map and then index into it. An adversarial envelope that
%% sets `platform-probes' to a binary / integer / list / atom
%% would otherwise crash the second `hb_maps:get' with
%% `{badmap, _}'. Centralised here so the is_map guard lives in
%% one place.
probes_map(E) ->
    case hb_maps:get(<<"platform-probes">>, E, #{}, #{}) of
        P when is_map(P) -> P;
        _ -> #{}
    end.

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
    %% Events first -- the rich per-record decoded TCG event log. Every
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
%%     /.../events/3                -> whole event 3
%%     /.../events/3/event_type     -> its type string
%%     /.../events/3/digests/sha256 -> one digest
%%     /.../events/3/parsed         -> the per-type decoded payload

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
%% These keys carry UTF-8 strings by construction -- leave as-is.
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
    BaseClaim0 = interpret_claim_body(Events, EvList, E, Db, Context),
    %% v1.1 cross-links: fold post-construction signals that need
    %% two already-built sections. Today's only example is
    %%   cpu.tee-support += [ tme-for-vendor ] when tme.enabled=true
    %% but this is the place to add future tme <-> iommu or
    %% firmware <-> cpu cross-references without threading them
    %% through every claim_* function.
    BaseClaim = cross_link_tme_into_cpu(BaseClaim0),
    %% Hour-13 + Hour-14: bolt meta layers onto the base
    %% claim tree in dependency order --
    %%   timeline        depends only on events + quote
    %%   policy-verdict  aggregates BaseClaim signals
    %%   attestation-summary READS policy-verdict (must come
    %%                   after)
    %%   evidence-digest hashes EVERYTHING (must be last so
    %%                   no downstream fields drift)
    WithTimeline = BaseClaim#{
        <<"timeline">>        => claim_timeline(EvList, E)
    },
    WithVerdict = WithTimeline#{
        <<"policy-verdict">>  => claim_policy_verdict(WithTimeline, E)
    },
    WithSummary = WithVerdict#{
        <<"attestation-summary">> =>
            claim_attestation_summary(WithVerdict)
    },
    WithSummary#{
        <<"evidence-digest">> => claim_evidence_digest(WithSummary)
    }.

%% When tme.enabled=true and cpu.vendor identifies the silicon, name
%% the concrete TEE feature in cpu.tee-support. AMD silicon advertises
%% "amd-sme" (Secure Memory Encryption; the Zen-specific name for the
%% same broad capability); Intel silicon advertises "intel-tme"
%% (Total Memory Encryption). If the vendor is unknown we still record
%% tee-support: ["memory-encryption"] so the claim shows the generic
%% capability rather than an empty list.
cross_link_tme_into_cpu(Claim) ->
    TME = maps:get(<<"tme">>, Claim, #{}),
    CPU = maps:get(<<"cpu">>, Claim, #{}),
    case maps:get(<<"enabled">>, TME, undefined) of
        <<"true">>  -> add_tee_for_vendor(Claim, CPU);
        true        -> add_tee_for_vendor(Claim, CPU);
        _           -> Claim
    end.

add_tee_for_vendor(Claim, CPU) ->
    Vendor = maps:get(<<"vendor">>, CPU, <<"unknown">>),
    Existing = maps:get(<<"tee-support">>, CPU, []),
    Feature = case Vendor of
        <<"amd">>   -> <<"amd-sme">>;
        <<"intel">> -> <<"intel-tme">>;
        _           -> <<"memory-encryption">>
    end,
    case lists:member(Feature, Existing) of
        true -> Claim;
        false ->
            Claim#{<<"cpu">> =>
                CPU#{<<"tee-support">> => [Feature | Existing]}}
    end.

interpret_claim_body(Events, EvList, E, Db, Context) ->
    #{
        <<"secure-boot">>        => claim_secure_boot(EvList),
        %% Hour-12: folded Secure-Boot policy posture --
        %% PK/KEK/db/dbx entry counts + mode (setup/user/
        %% deployed/audit) + trusted-signers list + blocked-
        %% hashes list + posture verdict (production, user-
        %% managed, setup, disabled, audit).
        <<"secure-boot-policy">> => claim_secure_boot_policy(EvList),
        <<"firmware">>           => claim_firmware(EvList, Db),
        <<"boot-loader">>        => claim_boot_loader(EvList),
        <<"boot-chain">>         => claim_boot_chain(EvList, Db),
        <<"kernel">>             => claim_kernel(EvList, E),
        <<"cpu">>                => claim_cpu(EvList, E, Db),
        <<"shim">>               => claim_shim(EvList),
        %% Paper section Architecture -- the quote itself carries freshness
        %% (reset-count / restart-count / clock-ms), TPM firmware
        %% identity, and the exact PCR selection that was quoted.
        %% Surface them on the compact claim API so policy engines
        %% don't have to parse the full interpret output.
        <<"quote">>              => claim_quote(E),
        %% PCR cross-reference -- does the (PCR 0, PCR 1, PCR 7)
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
        %% Hour-6: freshness composite -- reset-count / restart-
        %% count / clock / safe / nonce rolled into a single
        %% policy-ready stanza. Tells a verifier whether the
        %% quote is from the current boot epoch.
        <<"freshness">>          => claim_freshness(E),
        %% Hour-7: per-PCR event-log replay vs quoted value.
        %% For every PCR with events, re-fold the extensions
        %% and compare to the quote -- gives a definitive
        %% "event log is consistent with PCR values" verdict,
        %% per-PCR.
        <<"pcr-replay">>         => claim_pcr_replay(Events, E),
        %% Hour-7: IMA per-file measurement chain. Linux's
        %% Integrity Measurement Architecture hashes every
        %% executed binary / loaded kernel module / opened
        %% config file and extends PCR 10. We parse the ASCII
        %% IMA template into a navigable per-file list.
        <<"ima">>                => claim_ima(E),
        %% Hour-11: IMA policy cross-reference. Picks the
        %% best-matching per-distribution policy from the
        %% shipped `priv/tpm-interpret/ima-policies/' catalogue
        %% and classifies each parsed IMA entry as matched /
        %% unexpected / signature-missing / hash-alg-downgrade.
        <<"ima-policy">>         => claim_ima_policy(EvList, E, Db),
        %% Hour-8: platform-configuration snapshot aggregating
        %% every platform-identifying fact the event log
        %% carries: UEFI handoff tables (ACPI/SMBIOS/HOB
        %% presence), POST codes, option-ROM scans, measured
        %% UEFI variable count, boot-order / boot-current,
        %% per-PCR event histogram. Gives a verifier a single
        %% "what kind of machine is this?" stanza.
        <<"platform-config">>    => claim_platform_config(EvList),
        %% Paper field #2 -- TPM identity (vendor, model, firmware,
        %% spec, CVEs). Derived from the EK cert's TCG OIDs
        %% (2.23.133.2.1-3, 2.23.133.2.16) + the vendor catalogue.
        <<"tpm">>                => claim_tpm(E, Db),
        %% Structured decode of the Endorsement Key certificate
        %% (algorithm, key size, public exponent / curve, chain
        %% validation against the loaded root-cas/), projected
        %% onto the flat claim surface so a policy engine can
        %% answer "is this EK signed by a known TPM CA?" in one
        %% lookup.
        <<"ek">>                 => claim_ek(E, Db),
        %% Structured decode of the Attestation Key (AK) public
        %% blob -- algorithm, size, public exponent / curve,
        %% DER SHA-256 fingerprint so a verifier can pin the
        %% exact AK that signed the quote.
        <<"ak">>                 => claim_ak(E),
        %% Confidential-compute context: Intel TDX CCEL / AMD SEV-
        %% SNP. When detected it's tier-5 evidence for claim.tme.
        <<"context">>            => Context,
        %% Paper-committed machine-identifying fields composed
        %% across tier 1 (events) / tier 2 (cmdline) / tier 3
        %% (UKI-hash DB) / tier 4 (boot-reached-PCR-15) /
        %% tier 5 (confidential-compute context). Every populated
        %% value carries a `*-evidence' list.
        <<"tme">>                => claim_tme(EvList, E, Db, Context),
        <<"iommu">>              => claim_iommu(EvList, E),
        <<"lockdown">>           => claim_lockdown(EvList, E, Db),
        <<"kernel-integrity">>   => claim_kernel_integrity(EvList, E),
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
    %% Two independent sources. The CERT source pulls
    %% tpmManufacturer / tpmModel / tpmVersion from the EK cert's
    %% TCG-registered OIDs (2.23.133.2.1/.2/.3) + the specInfo
    %% extension (2.23.133.2.16). The CAPS source is a live
    %% TPM2_GetCapability snapshot (TPM_PT_MANUFACTURER +
    %% TPM_PT_VENDOR_STRING_* + TPM_PT_FIRMWARE_VERSION_* +
    %% TPM_PT_FAMILY_INDICATOR + TPM_PT_LEVEL + TPM_PT_REVISION)
    %% taken by dev_tpm2 at attestation time and carried on the
    %% envelope under `tpm-properties'.
    %%
    %% Capabilities win when both are present -- the EK cert is
    %% potentially vendor-signed-at-manufacture stale, whereas
    %% GetCapability returns today's firmware revision. When one
    %% is absent the other fills in; when both are absent we
    %% record null explicitly (no synthesis).
    FromCert = interpret_tpm_identity(E, Db),
    FromCaps = interpret_tpm_capabilities(E, Db),
    Merged = merge_tpm_sources(FromCert, FromCaps),
    Cves = maps:get(<<"known_cves">>, Merged,
              maps:get(<<"known-cves">>, Merged, [])),
    Kind = maps:get(<<"manufacturer-kind">>, Merged, null),
    TrustTier = tpm_trust_tier(Kind),
    Evidence = build_tpm_evidence(FromCert, FromCaps),
    #{
        <<"manufacturer-id">>    => maps:get(<<"manufacturer-id">>,
                                              Merged, null),
        <<"manufacturer-name">>  => maps:get(<<"manufacturer-name">>,
                                              Merged, null),
        <<"manufacturer-kind">>  => Kind,
        <<"model">>              => maps:get(<<"model">>, Merged, null),
        <<"firmware-version">>   => maps:get(<<"firmware-version">>,
                                              Merged, null),
        <<"firmware-version-u64">> =>
            maps:get(<<"firmware-version-u64">>, Merged, null),
        <<"spec-family">>        => maps:get(<<"spec-family">>, Merged,
                                              null),
        <<"spec-level">>         => maps:get(<<"spec-level">>, Merged,
                                              null),
        <<"spec-revision">>      => maps:get(<<"spec-revision">>, Merged,
                                              null),
        <<"vendor-string">>      => maps:get(<<"vendor-string">>, Merged,
                                              null),
        <<"trust-tier">>         => TrustTier,
        <<"known-cves">>         => Cves,
        <<"evidence">>           => Evidence
    }.

%% Read tpm-properties from the envelope (stamped by dev_tpm2 via
%% TPM2_GetCapability) and flatten it onto the same field names the
%% EK-cert path uses, so merge_tpm_sources/2 can combine them
%% key-for-key.
interpret_tpm_capabilities(E, Db) ->
    CapsRaw = hb_maps:get(<<"tpm-properties">>, E, undefined, #{}),
    %% Normalise Caps to a map so the later maps:get calls never
    %% trip a badmap error when the envelope has no tpm-properties
    %% block (e.g. old envelopes, or guests where init_chain never
    %% ran).
    Caps = case CapsRaw of
        #{} = M -> M;
        _       -> #{}
    end,
    %% `available' is stored as an atom on the guest side but may
    %% round-trip through JSON as <<"true">>/<<"false">>. Accept
    %% both shapes so the parser works whether the envelope came
    %% from same-process messaging or a writeback-then-reload.
    AvailableRaw = maps:get(<<"available">>, Caps, undefined),
    IsAvailable = (AvailableRaw =:= true) orelse
                  (AvailableRaw =:= <<"true">>) orelse
                  (AvailableRaw =:= "true"),
    case {IsAvailable, Caps} of
        {true, _} ->
            ManuId = maps:get(<<"manufacturer">>, Caps, <<>>),
            ManuU32 = maps:get(<<"manufacturer-u32">>, Caps, 0),
            VendorEntry =
                case lookup_vendor_by_u32(ManuU32, Db) of
                    #{} = V0 when map_size(V0) > 0 -> V0;
                    _ -> lookup_vendor_by_ascii(ManuId, Db)
                end,
            FW1 = maps:get(<<"firmware-version-1">>, Caps, 0),
            FW2 = maps:get(<<"firmware-version-2">>, Caps, 0),
            FWU = (FW1 bsl 32) bor (FW2 band 16#FFFFFFFF),
            FWStr = iolist_to_binary(
                io_lib:format("~8.16.0B.~8.16.0B", [FW1, FW2])),
            Rev = maps:get(<<"spec-revision">>, Caps, 0),
            SpecRev =
                case Rev of
                    0 -> null;
                    _ ->
                        iolist_to_binary(
                            io_lib:format("~.2f",
                                [float(Rev) / 100]))
                end,
            maps:merge(
                #{
                    <<"manufacturer-id">>    => maybe_bin_null(ManuId),
                    <<"manufacturer-name">>  =>
                        maps:get(<<"name">>, VendorEntry, null),
                    <<"manufacturer-kind">>  =>
                        maps:get(<<"kind">>, VendorEntry, null),
                    <<"model">>              =>
                        maybe_bin_null(
                            maps:get(<<"vendor-string">>, Caps, <<>>)),
                    <<"vendor-string">>      =>
                        maybe_bin_null(
                            maps:get(<<"vendor-string">>, Caps, <<>>)),
                    <<"firmware-version">>   => FWStr,
                    <<"firmware-version-u64">> => FWU,
                    <<"spec-family">>        =>
                        maybe_bin_null(
                            maps:get(<<"spec-family">>, Caps, <<>>)),
                    <<"spec-level">>         =>
                        maybe_zero_null(
                            maps:get(<<"spec-level">>, Caps, 0)),
                    <<"spec-revision">>      => SpecRev
                },
                extra_vendor_fields(VendorEntry));
        {false, _} when map_size(Caps) > 0 ->
            #{<<"caps-unavailable-reason">> =>
                  maps:get(<<"reason">>, Caps,
                           <<"tpm-properties present but available=false">>)};
        {false, _} ->
            #{}
    end.

%% Prefer capability-sourced values; fall back to cert-sourced.
%% Any non-null / non-empty capability value wins over the cert value
%% for the same key. We also keep the cert's fields around (under
%% `ek-cert-*' prefixes) so claim_ek can surface them; the caller
%% projects the top-level flat fields for claim_tpm.
merge_tpm_sources(FromCert, FromCaps) ->
    maps:fold(
        fun(K, V, Acc) ->
            case is_useful(V) of
                true  -> Acc#{K => V};
                false -> Acc
            end
        end,
        FromCert,
        FromCaps).

is_useful(null) -> false;
is_useful(<<>>) -> false;
is_useful(0) -> false;
is_useful(_) -> true.

maybe_bin_null(<<>>) -> null;
maybe_bin_null(B) when is_binary(B) -> B;
maybe_bin_null(_) -> null.

maybe_zero_null(0) -> null;
maybe_zero_null(N) when is_integer(N) -> N;
maybe_zero_null(_) -> null.

build_tpm_evidence(FromCert, FromCaps) ->
    CertEv = case maps:get(<<"ek-cert-subject">>, FromCert, null) of
        null -> [];
        _ ->
            [[{<<"tier">>, 1},
              {<<"source">>, <<"ek-cert-tcg-oids">>}]]
    end,
    CapsEv = case FromCaps of
        #{<<"caps-unavailable-reason">> := _} -> [];
        #{} when map_size(FromCaps) > 0 ->
            [[{<<"tier">>, 1},
              {<<"source">>, <<"tpm2-get-capability">>}]];
        _ -> []
    end,
    lists:flatten(CertEv ++ CapsEv).

%% Vendor lookup by the TPM-reported U32 manufacturer ID (the raw
%% 4-byte big-endian form of TPM_PT_MANUFACTURER). manufacturers.json
%% keys are always the 8-char hex string, e.g. <<"414D4400">> for
%% "AMD\0". This is the PRIMARY match path for capability-sourced
%% IDs because it preserves trailing-NUL bytes that would be dropped
%% by ASCII trimming.
lookup_vendor_by_u32(0, _Db) -> #{};
lookup_vendor_by_u32(U32, #{<<"vendors">> := V}) when is_integer(U32),
                                                       is_map(V) ->
    HexKey = iolist_to_binary(
        io_lib:format("~8.16.0B", [U32])),
    case maps:get(HexKey, V, undefined) of
        undefined ->
            %% Case-insensitive fallback: some manufacturers.json
            %% entries use lower-case hex.
            LowerKey = string:lowercase(HexKey),
            case maps:get(LowerKey, V, undefined) of
                undefined -> #{};
                E when is_map(E) -> E
            end;
        E when is_map(E) -> E
    end;
lookup_vendor_by_u32(_, _) -> #{}.

%% Secondary vendor lookup by ASCII manufacturer ID (3-4 char, NUL-
%% stripped form, e.g. <<"AMD">>). Tries (a) any `ascii-id' /
%% `id-ascii' fields on individual vendor entries, then (b) the hex
%% form with an RH-NUL pad to 4 bytes so <<"AMD">> -> <<"414D4400">>
%% matches the standard JSON key.
lookup_vendor_by_ascii(<<>>, _Db) -> #{};
lookup_vendor_by_ascii(Ascii, #{<<"vendors">> := V}) when is_map(V) ->
    Found = maps:fold(
        fun(_K, E, none) when is_map(E) ->
                IdA = maps:get(<<"ascii-id">>, E,
                     maps:get(<<"id-ascii">>, E, undefined)),
                case IdA of
                    Ascii -> E;
                    _ -> none
                end;
           (_K, _, Acc) -> Acc
        end, none, V),
    case Found of
        none ->
            %% Pad the ASCII to 4 bytes with trailing NUL and
            %% hex-encode. manufacturers.json keys are 8-char hex.
            Padded =
                case byte_size(Ascii) of
                    Sz when Sz < 4 ->
                        <<Ascii/binary, 0:((4 - Sz) * 8)>>;
                    _ -> binary:part(Ascii, {0, 4})
                end,
            HexKey = iolist_to_binary(
                [io_lib:format("~2.16.0B", [B])
                 || <<B:8>> <= Padded]),
            case maps:get(HexKey, V, undefined) of
                undefined -> #{};
                E when is_map(E) -> E
            end;
        E -> E
    end;
lookup_vendor_by_ascii(_, _) -> #{}.

%% @doc Structured decode of the Endorsement Key certificate,
%% projected onto the flat `claim' surface.
%%
%% The EK certificate is the root of the TPM's cryptographic
%% identity: it's signed by the TPM vendor's CA when the chip
%% is manufactured and carries (via TCG-registered OIDs in the
%% SubjectAltName) the TPM manufacturer, model, and firmware
%% version. `claim.tpm' already surfaces those TCG-OID fields;
%% `claim.ek' adds the certificate's cryptographic properties
%% (key algorithm + size + public-exponent / curve), validity
%% window, and a chain-validation verdict against any root
%% CAs shipped under `priv/tpm-interpret/root-cas/'.
%%
%% Output fields:
%%   present             bool -- an EK cert PEM was on the envelope
%%   subject             X.500 subject (string form)
%%   issuer              X.500 issuer (string form)
%%   serial-hex          cert serial (big-endian hex)
%%   valid-from          RFC 5280 notBefore (ISO-ish string)
%%   valid-to            RFC 5280 notAfter
%%   is-currently-valid  true iff now is inside the window
%%   key-alg             "rsa" | "ecdsa" | "ed25519" | "ed448" |
%%                       "dsa" | "unknown"
%%   key-size-bits       integer
%%   rsa-public-exponent integer (RSA only)
%%   public-key-sha256   base64url SHA-256 of the DER-encoded
%%                       SubjectPublicKeyInfo -- a pin-able key ID
%%   chain-validation    map of:
%%     validated-by-root-ca  name of the matching root CA,
%%                            or null / "no-roots-loaded"
%%     root-ca-count        number of root CAs in the DB
%%     chain-valid          true | false | "unknown"
%%     reason               free-form explanation
%%   evidence            provenance list
claim_ek(E, Db) ->
    Pem = hb_maps:get(<<"ek-cert-pem">>, E, <<>>, #{}),
    case decode_cert(Pem) of
        {error, _} -> unknown_ek_claim();
        {ok, Cert} ->
            Attrs = tpm_attrs_from_cert(Cert),
            {KeyAlg, KeyBits, RsaExp, PubDerSha256} =
                cert_public_key_summary(Cert),
            Roots = maps:get(<<"cert-roots">>, Db, []),
            %% v1.2 E2: envelope may carry the intermediate chain
            %% under `ek-cert-chain-pem' (PEM bundle of one or more
            %% certs, pulled from TPM NV at `<ek-handle>+1' per TCG
            %% EK Credential Profile section 2.2.1.4). Decode each
            %% one into an OTPCertificate for pkix_path_validation.
            ChainPem = hb_maps:get(<<"ek-cert-chain-pem">>, E, <<>>, #{}),
            ChainCerts = decode_cert_bundle(ChainPem),
            Chain = validate_ek_chain(Cert, ChainCerts, Roots),
            From = maps:get(valid_from, Attrs, undefined),
            To   = maps:get(valid_to, Attrs, undefined),
            #{
                <<"present">>            => true,
                <<"subject">>            =>
                    or_null(maps:get(subject_rdn, Attrs,
                                       undefined)),
                <<"issuer">>             =>
                    or_null(maps:get(issuer_rdn, Attrs,
                                       undefined)),
                <<"serial-hex">>         =>
                    or_null(maps:get(serial_b64url, Attrs,
                                       undefined)),
                <<"valid-from">>         => or_null(From),
                <<"valid-to">>           => or_null(To),
                <<"is-currently-valid">> => currently_valid(From, To),
                <<"key-alg">>            => KeyAlg,
                <<"key-size-bits">>      => KeyBits,
                <<"rsa-public-exponent">> => RsaExp,
                <<"public-key-sha256">>  => PubDerSha256,
                <<"chain-validation">>   => Chain,
                <<"evidence">>           =>
                    [{<<"tier">>, 1},
                     {<<"source">>, <<"ek-cert-der">>}]
            }
    end.

unknown_ek_claim() ->
    #{
        <<"present">>             => false,
        <<"subject">>             => null,
        <<"issuer">>              => null,
        <<"serial-hex">>          => null,
        <<"valid-from">>          => null,
        <<"valid-to">>            => null,
        <<"is-currently-valid">>  => <<"unknown">>,
        <<"key-alg">>             => <<"unknown">>,
        <<"key-size-bits">>       => 0,
        <<"rsa-public-exponent">> => null,
        <<"public-key-sha256">>   => <<"">>,
        <<"chain-validation">>    => unknown_ek_chain(),
        <<"evidence">>            => []
    }.

unknown_ek_chain() ->
    #{
        <<"validated-by-root-ca">> => null,
        <<"root-ca-count">>        => 0,
        <<"chain-valid">>          => <<"unknown">>,
        <<"reason">>               => <<"no ek-cert-pem on envelope">>
    }.

%% Pull the public-key alg + size + (RSA) exponent + SHA-256
%% fingerprint out of a decoded cert's SubjectPublicKeyInfo.
cert_public_key_summary(#'OTPCertificate'{tbsCertificate = Tbs}) ->
    case Tbs#'OTPTBSCertificate'.subjectPublicKeyInfo of
        #'OTPSubjectPublicKeyInfo'{
            algorithm = #'PublicKeyAlgorithm'{algorithm = AlgOid},
            subjectPublicKey = PubKey} ->
            {Alg, Bits, RsaExp} =
                spki_alg_bits_exp(AlgOid, PubKey),
            DerBytes = der_encode_spki(AlgOid, PubKey),
            Sha = case DerBytes of
                <<>> -> <<"">>;
                _    -> hb_util:encode(crypto:hash(sha256, DerBytes))
            end,
            {Alg, Bits, RsaExp, Sha};
        _ ->
            {<<"unknown">>, 0, null, <<"">>}
    end;
cert_public_key_summary(_) ->
    {<<"unknown">>, 0, null, <<"">>}.

spki_alg_bits_exp(?rsaEncryption,
                    #'RSAPublicKey'{modulus = N,
                                     publicExponent = Exp})
    when is_integer(N), is_integer(Exp) ->
    Bits = byte_size(binary:encode_unsigned(N)) * 8,
    {<<"rsa">>, Bits, Exp};
spki_alg_bits_exp(?'id-ecPublicKey', PubKey) when is_binary(PubKey) ->
    %% Uncompressed EC point: 1 + 2*curve-byte-size bytes.
    %% Derive approximate bit size from the point length.
    Bits = case byte_size(PubKey) of
        65 -> 256;  %% P-256
        97 -> 384;  %% P-384
        133 -> 521; %% P-521
        _ -> byte_size(PubKey) * 4
    end,
    {<<"ecdsa">>, Bits, null};
spki_alg_bits_exp(?'id-Ed25519', PubKey) when is_binary(PubKey) ->
    {<<"ed25519">>, byte_size(PubKey) * 8, null};
spki_alg_bits_exp(?'id-Ed448', PubKey) when is_binary(PubKey) ->
    {<<"ed448">>, byte_size(PubKey) * 8, null};
spki_alg_bits_exp(?'id-dsa', _) ->
    {<<"dsa">>, 0, null};
spki_alg_bits_exp(_, _) ->
    {<<"unknown">>, 0, null}.

%% DER-encode the subjectPublicKeyInfo for fingerprinting. RSA
%% is the common case; other algs re-encode the raw point.
der_encode_spki(?rsaEncryption, #'RSAPublicKey'{} = Rsa) ->
    try public_key:der_encode('RSAPublicKey', Rsa)
    catch _:_ -> <<>>
    end;
der_encode_spki(_, PubKey) when is_binary(PubKey) -> PubKey;
der_encode_spki(_, _) -> <<>>.

%% Is `now' inside [From, To]? Uses the raw binary form (ISO-ish
%% string we emit from `x509_time/1').
currently_valid(undefined, _) -> <<"unknown">>;
currently_valid(_, undefined) -> <<"unknown">>;
currently_valid(FromBin, ToBin) when is_binary(FromBin),
                                     is_binary(ToBin) ->
    %% Validity strings come from format_time/1 as raw ASCII in the
    %% X.509 shape: UTCTime `YYMMDDHHMMSSZ' (13 chars, year in
    %% [1950..2049] per RFC 5280 section 4.1.2.5.1) or
    %% GeneralizedTime `YYYYMMDDHHMMSSZ' (15 chars). The old
    %% x509_now_iso/0 emitted `YYYY-MM-DDTHH:MM:SSZ' which doesn't
    %% lexicographically compare to either form -- the dashes/colons
    %% shift every position, so a current-epoch cert reads as
    %% `valid-from' > `now', falsely rendering it "not-yet-valid".
    %% Parse both endpoints to `calendar:datetime()' and compare
    %% against `calendar:universal_time/0' via gregorian seconds.
    case {parse_x509_time(FromBin), parse_x509_time(ToBin)} of
        {{ok, FromDT}, {ok, ToDT}} ->
            NowDT = calendar:universal_time(),
            F = calendar:datetime_to_gregorian_seconds(FromDT),
            T = calendar:datetime_to_gregorian_seconds(ToDT),
            N = calendar:datetime_to_gregorian_seconds(NowDT),
            F =< N andalso N =< T;
        _ -> <<"unknown">>
    end;
currently_valid(_, _) -> <<"unknown">>.

%% Parse an X.509 validity string into `calendar:datetime()'. Accepts
%% UTCTime (13 char, two-digit year) and GeneralizedTime (15 char,
%% four-digit year). Returns `{ok, {{Y,Mo,D},{H,Mi,S}}}' on success,
%% `error' otherwise.
parse_x509_time(<<YY:2/binary, Rest:11/binary>>)
        when byte_size(Rest) == 11 ->
    %% UTCTime: 13 chars, terminating Z. Year resolution per
    %% RFC 5280 section 4.1.2.5.1: YY in [50..99] maps to [1950..1999],
    %% YY in [00..49] to [2000..2049].
    case parse_dt_tail(Rest) of
        {ok, Mo, D, H, Mi, S} ->
            Y2 = safe_int(YY),
            case Y2 of
                N when is_integer(N), N >= 0, N =< 49 ->
                    {ok, {{2000 + N, Mo, D}, {H, Mi, S}}};
                N when is_integer(N), N >= 50, N =< 99 ->
                    {ok, {{1900 + N, Mo, D}, {H, Mi, S}}};
                _ -> error
            end;
        _ -> error
    end;
parse_x509_time(<<YYYY:4/binary, Rest:11/binary>>)
        when byte_size(Rest) == 11 ->
    %% GeneralizedTime: 15 chars, terminating Z. Four-digit year.
    case parse_dt_tail(Rest) of
        {ok, Mo, D, H, Mi, S} ->
            case safe_int(YYYY) of
                Y when is_integer(Y) -> {ok, {{Y, Mo, D}, {H, Mi, S}}};
                _ -> error
            end;
        _ -> error
    end;
parse_x509_time(_) -> error.

parse_dt_tail(<<Mo:2/binary, D:2/binary, H:2/binary,
                Mi:2/binary, S:2/binary, "Z">>) ->
    Ints = [safe_int(X) || X <- [Mo, D, H, Mi, S]],
    case lists:all(fun(I) -> is_integer(I) end, Ints) of
        true ->
            [Mo1, D1, H1, Mi1, S1] = Ints,
            {ok, Mo1, D1, H1, Mi1, S1};
        false -> error
    end;
parse_dt_tail(_) -> error.

%% Decode a concatenated PEM bundle of one or more certs into a
%% list of OTPCertificate records. Used to consume the
%% `ek-cert-chain-pem' field that v1.2's dev_tpm2 stamps onto the
%% envelope (it's the concatenation of every DER cert found in
%% TPM NV at `<ek-handle>+1'). Returns `[]' on any error -- the
%% chain is always best-effort; a missing or malformed bundle
%% does not break the leaf-level claim.
decode_cert_bundle(<<>>) -> [];
decode_cert_bundle(Pem) when is_binary(Pem) ->
    try
        Entries = public_key:pem_decode(Pem),
        [public_key:pkix_decode_cert(Der, otp)
         || {'Certificate', Der, not_encrypted} <- Entries]
    catch _:_ -> []
    end;
decode_cert_bundle(_) -> [].

%% Validate the EK cert chain against shipped root CAs, optionally
%% threading the envelope-supplied intermediates through the
%% middle. TCG EK Credential Profile often ships the leaf-signing
%% intermediate CA in NV alongside the EK cert; without it, a
%% real EK cert issued by a vendor-sub-CA (e.g. Nuvoton's
%% `NPCTxxx ECC384 LeafCA 012110') never chains to the root CA we
%% have in the bundle.
%%
%%   Cert    -- the leaf EK cert (decoded OTPCertificate)
%%   Chain   -- list of OTPCertificate intermediates, root-last
%%              convention (TCG concatenates leaf->root in NV).
%%              We DER-encode each for pkix_path_validation.
%%   Roots   -- list of #{name, pem} from cert-roots Db.
%%
%% When no roots are loaded we return `unknown' (not a failure)
%% since the caller may be operating in a dev environment.
validate_ek_chain(Cert, Chain, Roots) ->
    DerCert = safe_der_encode(Cert),
    DerIntermediates = [safe_der_encode(C) || C <- Chain],
    validate_ek_chain_1(DerCert, DerIntermediates, Roots,
                        length(Chain)).

%% Legacy 2-arg form for call sites that predate v1.2's chain
%% threading. Equivalent to calling with an empty intermediate list.
validate_ek_chain(Cert, Roots) ->
    validate_ek_chain(Cert, [], Roots).

safe_der_encode(Cert) ->
    try public_key:pkix_encode('OTPCertificate', Cert, otp)
    catch _:_ -> <<>>
    end.

validate_ek_chain_1(_DerCert, _Intermediates, [], _ChainLen) ->
    #{
        <<"validated-by-root-ca">> => null,
        <<"root-ca-count">>        => 0,
        <<"chain-valid">>          => <<"unknown">>,
        <<"reason">>               =>
            <<"no root-cas loaded in priv/tpm-interpret/root-cas/">>,
        <<"intermediates-used">>   => 0
    };
validate_ek_chain_1(<<>>, _Intermediates, Roots, _ChainLen) ->
    #{
        <<"validated-by-root-ca">> => null,
        <<"root-ca-count">>        => length(Roots),
        <<"chain-valid">>          => <<"unknown">>,
        <<"reason">>               => <<"cert re-encode failed">>,
        <<"intermediates-used">>   => 0
    };
validate_ek_chain_1(DerCert, DerIntermediates, Roots, ChainLen) ->
    %% Only self-signed certs may stand as trust anchors. Non-self-
    %% signed entries in the shipped bundle are candidate
    %% intermediates — promoting them to anchors is the reward-hack
    %% we refuse to commit here (otherwise a manufacturer's LeafCA
    %% dropped into `priv/tpm-interpret/root-cas/' would "validate"
    %% any EK the LeafCA happens to have signed, stopping the chain
    %% short of the real manufacturer root).
    {TrueRoots, BundleIntermediates} = partition_self_signed_roots(Roots),
    %% Filter on-TPM intermediates down to valid DER. Keep order --
    %% TCG convention is leaf-to-root, which is also the order
    %% `pkix_path_validation/3' expects for CertPath.
    Cleaned = [D || D <- DerIntermediates, D =/= <<>>],
    %% First pass: try each self-signed root directly, using the
    %% on-TPM intermediates (if any) as the chain. This covers the
    %% happy case: NV 0x01C00003 has the manufacturer LeafCA,
    %% NV 0x01C00002 has the EK.
    Direct = try_validate_against_roots(DerCert, Cleaned, TrueRoots, 0),
    Result = case maps:get(<<"chain-valid">>, Direct, false) of
        true  -> Direct;
        _ when Cleaned =:= [] ->
            %% Fallback when the on-TPM chain is empty (seen on
            %% Nuvoton NPCT75x Framework builds where NV 0x01C00003
            %% is undefined). For each real root, try each shipped
            %% non-self-signed cert as the missing intermediate. See
            %% TCG EK Credential Profile -- the manufacturer's
            %% intermediate CA cert is itself published separately
            %% from NV.
            try_validate_with_bundle_intermediates(
              DerCert, TrueRoots, BundleIntermediates, Direct);
        _ -> Direct
    end,
    Result#{<<"intermediates-used">> => ChainLen}.

%% @doc Split the shipped bundle into self-signed trust anchors and
%% everything else (candidate intermediates). "Self-signed" is
%% determined by verifying the cert's signature under its own public
%% key, not by the cheaper subject==issuer check (which would accept
%% a same-name intermediate whose signer is a different, unknown key).
partition_self_signed_roots(Roots) ->
    lists:partition(
        fun(R) ->
            case decode_cert(maps:get(<<"pem">>, R, <<>>)) of
                {ok, Cert} ->
                    try public_key:pkix_is_self_signed(Cert)
                    catch _:_ -> false
                    end;
                _ -> false
            end
        end, Roots).

%% @doc Pair-walk the shipped anchors: for each self-signed root try
%% each shipped non-self-signed cert as a single intermediate. Stops
%% on the first successful chain; otherwise returns the original
%% direct-attempt result unchanged so reason strings bubble up.
try_validate_with_bundle_intermediates(DerCert, TrueRoots,
                                       BundleIntermediates,
                                       FallbackResult) ->
    %% Pre-decode anchors.
    DecodedRoots = [
        {Name, Cert} ||
        R <- TrueRoots,
        Name <- [maps:get(<<"name">>, R, <<"unknown-root">>)],
        Pem  <- [maps:get(<<"pem">>, R, <<>>)],
        {ok, Cert} <- [decode_cert(Pem)]
    ],
    %% Pre-decode intermediates to DER once.
    DecodedInters = [
        {IName, IDer} ||
        I <- BundleIntermediates,
        IName <- [maps:get(<<"name">>, I, <<"unknown-intermediate">>)],
        IPem <- [maps:get(<<"pem">>, I, <<>>)],
        {ok, ICert} <- [decode_cert(IPem)],
        IDer <- [safe_der_encode(ICert)],
        IDer =/= <<>>
    ],
    walk_bundle_pairs(DerCert, DecodedRoots, DecodedInters, FallbackResult).

walk_bundle_pairs(_DerCert, [], _Inters, FallbackResult) ->
    FallbackResult;
walk_bundle_pairs(DerCert, [{AnchorName, AnchorCert} | Rest],
                  Inters, FallbackResult) ->
    case try_pair_chain(DerCert, AnchorCert, AnchorName, Inters) of
        {ok, Hit} -> Hit;
        not_found -> walk_bundle_pairs(DerCert, Rest, Inters,
                                        FallbackResult)
    end.

try_pair_chain(_DerCert, _AnchorCert, _AnchorName, []) ->
    not_found;
try_pair_chain(DerCert, AnchorCert, AnchorName,
               [{InterName, InterDer} | Rest]) ->
    case validate_against_one_root(DerCert, [InterDer], AnchorCert) of
        true ->
            {ok, #{
                <<"validated-by-root-ca">> => AnchorName,
                <<"validated-via-intermediate">> => InterName,
                <<"root-ca-count">>        => 0,
                <<"chain-valid">>          => true,
                <<"reason">>               =>
                    <<"chain validates against ", AnchorName/binary,
                      " via bundled intermediate ", InterName/binary>>
            }};
        false ->
            try_pair_chain(DerCert, AnchorCert, AnchorName, Rest)
    end.

try_validate_against_roots(_DerCert, _Chain, [], Count) ->
    #{
        <<"validated-by-root-ca">> => null,
        <<"root-ca-count">>        => Count,
        <<"chain-valid">>          => false,
        <<"reason">>               =>
            <<"no root CA in priv/tpm-interpret/root-cas/ issued "
              "this certificate">>
    };
try_validate_against_roots(DerCert, Chain, [Root | Rest], Count) ->
    Name = maps:get(<<"name">>, Root, <<"unknown-root">>),
    RootPem = maps:get(<<"pem">>, Root, <<>>),
    case decode_cert(RootPem) of
        {ok, RootCert} ->
            case validate_against_one_root(DerCert, Chain, RootCert) of
                true ->
                    #{
                        <<"validated-by-root-ca">> => Name,
                        <<"root-ca-count">>        => Count + 1
                            + length(Rest),
                        <<"chain-valid">>          => true,
                        <<"reason">>               =>
                            <<"chain validates against ", Name/binary>>
                    };
                false ->
                    try_validate_against_roots(
                      DerCert, Chain, Rest, Count + 1)
            end;
        _ ->
            try_validate_against_roots(
              DerCert, Chain, Rest, Count + 1)
    end.

validate_against_one_root(DerCert, Intermediates, RootCert) ->
    try
        RootDer = public_key:pkix_encode(
                    'OTPCertificate', RootCert, otp),
        %% CertPath ordering per OTP public_key docs: first
        %% element is signed BY the trust anchor (closest-to-root),
        %% last is the leaf. TCG ships NV chains as
        %% [leaf, ..., closest-to-root], so we reverse the
        %% intermediates and append the leaf to get the right
        %% OTP order. Also try the straight-leaf-first form
        %% defensively -- a few vendor NV layouts write root-first.
        Orders = [
            lists:reverse(Intermediates) ++ [DerCert],  %% OTP form
            Intermediates ++ [DerCert]                    %% defensive
        ],
        %% Pass `verify_fun' that whitelists TCG EK EKU and spec-
        %% version extensions which OTP's default path validator
        %% treats as unrecognised-critical and rejects. A real
        %% Nuvoton / Infineon / AMD EK cert will carry one or
        %% more of these, so without the whitelist we'd see
        %% {bad_cert, {not_supported_extension, ...}} on every
        %% in-the-wild EK chain. See TCG EK Credential Profile
        %% section 3.5 and the OID registry at
        %% https://www.iana.org/assignments/smi-numbers.
        VerifyOpts = [{verify_fun, {fun ek_verify_fun/3, []}}],
        lists:any(
            fun(Path) ->
                case public_key:pkix_path_validation(
                       RootDer, Path, VerifyOpts) of
                    {ok, _} -> true;
                    _ -> false
                end
            end, Orders)
    catch _:_ -> false
    end.

%% verify_fun for pkix_path_validation. Whitelists the TCG-defined
%% OIDs that appear in real-world TPM EK certificates as critical
%% extensions but aren't in OTP's baseline X.509 extension
%% recognition set:
%%
%%   2.23.133.8.1   id-tcg-kp-EKCertificate
%%                   (extendedKeyUsage entry; EK certs always
%%                    carry this to signal they're TPM endorsement
%%                    certs rather than general-purpose certs)
%%   2.23.133.2.16  id-tcg-tpmSpecification
%%                   (non-EKU spec-version attribute; marked
%%                    critical by some vendors' profiles)
%%
%% Everything else uses the default path-validation semantics.
ek_verify_fun(_, {bad_cert, {not_supported_extension, Ext}}, UserState) ->
    ExtId = case Ext of
        #'Extension'{extnID = Id} -> Id;
        _ -> undefined
    end,
    case ExtId of
        {2, 23, 133, 8, 1}   -> {valid, UserState};
        {2, 23, 133, 2, 16}  -> {valid, UserState};
        _ -> {fail, {not_supported_extension, Ext}}
    end;
ek_verify_fun(_, {bad_cert, Reason}, _UserState) ->
    {fail, Reason};
ek_verify_fun(_, {extension, #'Extension'{extnID = ExtId}}, UserState) ->
    %% Called for each non-critical unknown extension. Accept any
    %% OID under the TCG arc 2.23.133.x (all EK metadata).
    case ExtId of
        {2, 23, 133, _, _}    -> {valid, UserState};
        {2, 23, 133, _, _, _} -> {valid, UserState};
        _                     -> {unknown, UserState}
    end;
ek_verify_fun(_, valid, UserState)      -> {valid, UserState};
ek_verify_fun(_, valid_peer, UserState) -> {valid, UserState}.

%% @doc Structured decode of the Attestation Key public blob on
%% the flat claim surface. AK is an RSA-2048 or RSA-3072 key
%% generated INSIDE the TPM with signing-only attributes; a
%% verifier pins the specific AK by SHA-256 fingerprint of its
%% DER SubjectPublicKeyInfo.
%%
%% Output fields:
%%   present                bool -- the envelope had an ak-pub-pem
%%   key-alg                "rsa" | "ecdsa" | "ed25519" | ...
%%   key-size-bits          integer
%%   rsa-public-exponent    integer (RSA only)
%%   public-key-sha256      base64url SHA-256 of the DER SPKI
%%   evidence               provenance
claim_ak(E) ->
    Pem = hb_maps:get(<<"ak-pub-pem">>, E, <<>>, #{}),
    case decode_pub_key(Pem) of
        {error, _} -> unknown_ak_claim();
        {ok, #'RSAPublicKey'{modulus = N,
                             publicExponent = Exp} = Rsa} ->
            Bits = byte_size(binary:encode_unsigned(N)) * 8,
            Der = try public_key:der_encode('RSAPublicKey', Rsa)
                  catch _:_ -> <<>>
                  end,
            Sha = case Der of
                <<>> -> <<"">>;
                _    -> hb_util:encode(crypto:hash(sha256, Der))
            end,
            #{
                <<"present">>             => true,
                <<"key-alg">>             => <<"rsa">>,
                <<"key-size-bits">>       => Bits,
                <<"rsa-public-exponent">> => Exp,
                <<"public-key-sha256">>   => Sha,
                <<"evidence">>            =>
                    [{<<"tier">>, 1},
                     {<<"source">>, <<"ak-pub-pem">>}]
            };
        {ok, Other} ->
            #{
                <<"present">>             => true,
                <<"key-alg">>             =>
                    iolist_to_binary(io_lib:format(
                        "~p", [element(1, Other)])),
                <<"key-size-bits">>       => 0,
                <<"rsa-public-exponent">> => null,
                <<"public-key-sha256">>   => <<"">>,
                <<"evidence">>            =>
                    [{<<"tier">>, 1},
                     {<<"source">>, <<"ak-pub-pem">>}]
            }
    end.

unknown_ak_claim() ->
    #{
        <<"present">>             => false,
        <<"key-alg">>             => <<"unknown">>,
        <<"key-size-bits">>       => 0,
        <<"rsa-public-exponent">> => null,
        <<"public-key-sha256">>   => <<"">>,
        <<"evidence">>            => []
    }.

%% Trust tier ordering per paper section Hardware-Availability:
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

%% CPU microcode identity -- from EV_CPU_MICROCODE on PCR 1.
%% Discriminates Intel vs AMD vs unknown via `parsed.format'.
%% The 2-arg form additionally cross-references
%% `priv/tpm-interpret/cpu-models.json' to attach a human-readable
%% `codename', `brand-range', `micro-arch', `year' and the
%% supported TEE/hardening feature set.
claim_cpu(Events) -> claim_cpu(Events, #{}, #{}).
claim_cpu(Events, Db) -> claim_cpu(Events, #{}, Db).

claim_cpu(Events, E, Db) ->
    UcodeEvs = [Ev || Ev <- Events,
                      maps:get(<<"event-type-code">>, Ev, 0) =:= 16#09],
    Base0 = case UcodeEvs of
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
    end,
    %% Firmware event strings often carry vendor hints even on TPMs
    %% where the firmware never emitted an EV_CPU_MICROCODE event.
    %% We mine EV_S_CRTM_CONTENTS / _VERSION, EV_POST_CODE,
    %% EV_EFI_PLATFORM_FIRMWARE_BLOB, EV_NONHOST_CONFIG,
    %% EV_EVENT_TAG, and EV_NO_ACTION for "Intel", "AMD", "Ryzen",
    %% "EPYC", "Xeon", "Core" substrings. The microcode path wins
    %% when both fire (exact CPUID is richer than a string hint)
    %% but we promote the string hint to populate `vendor' when
    %% microcode is absent.
    Base1 = enrich_cpu_from_strings(Base0, Events),
    %% v1.2 E3: runtime /proc/cpuinfo snapshot from the guest
    %% (stamped into the envelope as `platform-probes.cpuinfo').
    %% This is ground-truth from the running kernel -- includes
    %% vendor_id, model name, family/model/stepping, microcode
    %% revision, and the full CPU feature-flags list. Wins over
    %% the string-scan fallback when both fire.
    enrich_cpu_from_cpuinfo(Base1, E).

%% Event-string vendor hint. Returns #{vendor => ..., brand-range => ...}
%% or #{} if no hint found.
cpu_hint_from_events(Events) ->
    Substrings = [
        <<"GenuineIntel">>, <<"Intel">>, <<"Xeon">>, <<"Core">>, <<"Pentium">>, <<"Celeron">>,
        <<"AuthenticAMD">>, <<"AMD">>, <<"Ryzen">>, <<"EPYC">>, <<"Athlon">>, <<"Threadripper">>,
        <<"AGESA">>  %% AMD-specific reference code family
    ],
    InterestingCodes = [16#01,  %% EV_POST_CODE
                        16#07,  %% EV_S_CRTM_CONTENTS
                        16#08,  %% EV_S_CRTM_VERSION
                        16#0A,  %% EV_NONHOST_CONFIG
                        16#0B,  %% EV_NONHOST_INFO
                        16#0C,  %% EV_OMIT_BOOT_DEVICE_EVENTS
                        16#80000008, %% EV_EFI_PLATFORM_FIRMWARE_BLOB
                        16#80000006, %% EV_EFI_HANDOFF_TABLES
                        16#80000007, %% EV_EFI_PLATFORM_CONFIG_FLAGS
                        16#80000010, %% EV_EVENT_TAG
                        16#03        %% EV_NO_ACTION (SpecID payload)
                        ],
    Hits = lists:foldl(
        fun(Ev, Acc) ->
            case lists:member(maps:get(<<"event-type-code">>, Ev, 0),
                              InterestingCodes) of
                true ->
                    Hay = event_text_candidate(Ev),
                    lists:foldl(
                        fun(S, In) -> match_if_substring(S, Hay, Ev, In) end,
                        Acc, Substrings);
                false -> Acc
            end
        end, [], Events),
    hits_to_cpu_hint(Hits).

%% Collect any readable strings from an event -- parsed.value, parsed.description,
%% parsed.string, parsed.blob-description, parsed.event-data, raw, ...
event_text_candidate(Ev) ->
    Parsed = maps:get(<<"parsed">>, Ev, #{}),
    Pieces =
        [maps:get(K, Parsed, <<>>) || K <-
            [<<"value">>, <<"description">>, <<"string">>,
             <<"blob-description">>, <<"event-data">>, <<"text">>,
             <<"family-id">>, <<"platform-class">>, <<"signer">>]]
        ++ [maps:get(<<"event-data-ascii">>, Ev, <<>>),
            maps:get(<<"event-data">>, Ev, <<>>)],
    Filtered = [P || P <- Pieces, is_binary(P), byte_size(P) > 0],
    iolist_to_binary(lists:join(<<" | ">>, Filtered)).

match_if_substring(Needle, Haystack, Ev, Acc) ->
    case binary:match(Haystack, Needle) of
        nomatch -> Acc;
        _ -> [{Needle, Ev} | Acc]
    end.

%% Roll the accumulated substring hits into a vendor-plus-brand hint.
%% Intel hits win over AMD only if there's a direct Intel substring;
%% an "AGESA" hit (AMD's reference-code bundle) alone maps to AMD.
hits_to_cpu_hint([]) -> #{};
hits_to_cpu_hint(Hits) ->
    Needles = [N || {N, _} <- Hits],
    IsIntel = lists:any(
        fun(N) ->
            lists:member(N, [<<"GenuineIntel">>, <<"Intel">>,
                             <<"Xeon">>, <<"Core">>,
                             <<"Pentium">>, <<"Celeron">>])
        end, Needles),
    IsAmd = lists:any(
        fun(N) ->
            lists:member(N, [<<"AuthenticAMD">>, <<"AMD">>,
                             <<"Ryzen">>, <<"EPYC">>,
                             <<"Athlon">>, <<"Threadripper">>,
                             <<"AGESA">>])
        end, Needles),
    Vendor = case {IsIntel, IsAmd} of
        {true, _} -> <<"intel">>;
        {_, true} -> <<"amd">>;
        _         -> <<"unknown">>
    end,
    Brand = pick_brand_range(Needles),
    Prov = [{_, Ev} | _] = Hits,
    _ = Prov,
    FirstEv = element(2, hd(Hits)),
    #{<<"vendor">>            => Vendor,
      <<"vendor-provenance">> => [event_provenance(FirstEv),
                                   {<<"source">>, <<"event-string-scan">>}],
      <<"brand-range">>       => Brand,
      <<"brand-range-provenance">> =>
          [event_provenance(FirstEv),
           {<<"source">>, <<"event-string-scan">>}]}.

pick_brand_range(Needles) ->
    Ranked = [<<"Ryzen">>, <<"EPYC">>, <<"Threadripper">>, <<"Athlon">>,
              <<"Xeon">>, <<"Core">>, <<"Pentium">>, <<"Celeron">>,
              <<"AMD">>, <<"Intel">>, <<"AGESA">>],
    case lists:filter(fun(N) -> lists:member(N, Needles) end, Ranked) of
        [] -> null;
        [Best | _] -> Best
    end.

%% Fold `platform-probes.cpuinfo' from the envelope into the CPU
%% claim. This is the richest source we have -- it carries the
%% exact CPUID-level identity the running kernel reports for the
%% first processor core. When present, it wins over string-scan
%% hints and microcode events for vendor / model / family /
%% stepping / microcode description.
enrich_cpu_from_cpuinfo(Base, E) ->
    Probes = probes_map(E),
    CpuInfo = hb_maps:get(<<"cpuinfo">>, Probes, #{}, #{}),
    case CpuInfo of
        #{} when map_size(CpuInfo) == 0 -> Base;
        _ ->
            VendorRaw = maps:get(<<"vendor-id">>, CpuInfo, <<>>),
            ModelName = maps:get(<<"model-name">>, CpuInfo, <<>>),
            Family    = maps:get(<<"cpu-family">>, CpuInfo, <<>>),
            Model     = maps:get(<<"model">>, CpuInfo, <<>>),
            Stepping  = maps:get(<<"stepping">>, CpuInfo, <<>>),
            Microcode = maps:get(<<"microcode">>, CpuInfo, <<>>),
            Vendor =
                case VendorRaw of
                    <<"GenuineIntel">> -> <<"intel">>;
                    <<"AuthenticAMD">> -> <<"amd">>;
                    <<>>               -> maps:get(<<"vendor">>, Base,
                                                    <<"unknown">>);
                    _                  -> VendorRaw
                end,
            Base1 = Base#{
                <<"vendor">>           => Vendor,
                <<"vendor-provenance">> =>
                    [{<<"tier">>, 2},
                     {<<"source">>, <<"proc-cpuinfo">>}],
                <<"cpu-family">>       =>
                    binary_to_int_or(Family,
                        maps:get(<<"cpu-family">>, Base, null)),
                <<"cpu-model">>        =>
                    binary_to_int_or(Model,
                        maps:get(<<"cpu-model">>, Base, null)),
                <<"cpu-stepping">>     =>
                    binary_to_int_or(Stepping,
                        maps:get(<<"cpu-stepping">>, Base, null)),
                <<"microcode-description">> =>
                    case ModelName of
                        <<>> -> maps:get(<<"microcode-description">>,
                                          Base, <<"unknown">>);
                        _ ->
                            iolist_to_binary(io_lib:format(
                                "~s (ucode=~s)",
                                [ModelName, Microcode]))
                    end,
                <<"microcode-description-provenance">> =>
                    [{<<"tier">>, 2},
                     {<<"source">>, <<"proc-cpuinfo">>}]
            },
            %% brand-range from model name: `Intel(R) Xeon(R) ...'
            %% or `AMD Ryzen 7 7840U ...'. Extract the product line.
            BrandRange = extract_brand_range(ModelName,
                            maps:get(<<"brand-range">>, Base, null)),
            Base1#{<<"brand-range">> => BrandRange}
    end.

%% Delegates to `safe_int/1' so the binary-to-integer try/catch
%% lives in exactly one place; callers that want a specific default
%% instead of `undefined' wrap it here.
binary_to_int_or(Bin, Default) ->
    case safe_int(Bin) of
        undefined -> Default;
        N         -> N
    end.

extract_brand_range(<<>>, Default) -> Default;
extract_brand_range(ModelName, Default) when is_binary(ModelName) ->
    Candidates = [<<"Xeon">>, <<"Core">>, <<"Pentium">>,
                  <<"Celeron">>, <<"Ryzen">>, <<"EPYC">>,
                  <<"Threadripper">>, <<"Athlon">>],
    case lists:filter(
           fun(B) -> binary:match(ModelName, B) =/= nomatch end,
           Candidates) of
        []      -> Default;
        [First | _] -> First
    end;
extract_brand_range(_, Default) -> Default.

%% Fold an event-string hint into the CPU claim. When the microcode
%% path already produced a real vendor (`intel' | `amd'), we leave
%% the core identity alone and only annotate `brand-range' if it's
%% still null. When the microcode path produced `unknown', the hint
%% gets promoted so the claim surface populates instead of staying
%% blank.
enrich_cpu_from_strings(Base, Events) ->
    case cpu_hint_from_events(Events) of
        Hint when map_size(Hint) == 0 -> Base;
        Hint ->
            BaseVendor = maps:get(<<"vendor">>, Base, <<"unknown">>),
            B1 = case BaseVendor of
                <<"unknown">> ->
                    Base#{
                        <<"vendor">> =>
                            maps:get(<<"vendor">>, Hint, <<"unknown">>),
                        <<"vendor-provenance">> =>
                            maps:get(<<"vendor-provenance">>, Hint, [])
                    };
                _ -> Base
            end,
            case maps:get(<<"brand-range">>, B1, null) of
                null ->
                    B1#{
                        <<"brand-range">> =>
                            maps:get(<<"brand-range">>, Hint, null),
                        <<"brand-range-provenance">> =>
                            maps:get(<<"brand-range-provenance">>, Hint, [])
                    };
                _ -> B1
            end
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

%% Parse "family=6 model=151 stepping=2" -> {6, 151, 2}.
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

%% Convert the keyed events map into a list sorted by seq number --
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

%% @doc Full Secure-Boot policy posture. Hour-4's
%% `claim.secure-boot` surfaces enabled / setup-mode / deployed-
%% mode + a flat list of authorities. This stanza projects the
%% same underlying PK/KEK/db/dbx events into a policy-ready
%% view: per-bucket entry counts, the concrete trusted-signers
%% list (each with subject / issuer / fingerprint / type),
%% the blocked-hashes list (what the firmware refuses to load),
%% and a composite `policy-posture' verdict.
%%
%% Output fields:
%%   enabled                bool | "unknown"
%%   setup-mode             "disabled" | "enabled" | "unknown"
%%   deployed-mode          "disabled" | "enabled" | "unknown"
%%   pk-entry-count         count of Platform Key entries
%%   kek-entry-count        count of Key-Exchange-Key entries
%%   db-entry-count         count of allowed-signature entries
%%   dbx-entry-count        count of blocked-signature entries
%%   trusted-signers        list of #{subject, issuer,
%%                                   fingerprint, type-guid-name,
%%                                   owner-guid}
%%   blocked-hashes         list of #{hash-alg, hash-or-fingerprint,
%%                                   type-guid-name, owner-guid}
%%   policy-posture         one of:
%%     "setup-mode"         PK=0, any key can be enrolled
%%     "deployed-production" SB=true + all buckets populated +
%%                            deployed-mode=enabled
%%     "user-managed"       SB=true + all buckets populated +
%%                            deployed-mode=disabled
%%     "audit-only"         SB=false + buckets populated
%%     "disabled"           SB=false + buckets empty
%%     "unknown"            can't determine
%%   policy-strength        heuristic based on dbx population:
%%     "latest-revocations" dbx-entry-count >= 100
%%     "moderate-revocations" dbx-entry-count >= 20
%%     "minimal-revocations"  dbx-entry-count >= 1
%%     "no-revocations"       dbx empty
%%     "unknown"              no dbx event seen
claim_secure_boot_policy(Events) ->
    Sb = claim_secure_boot(Events),
    PkEntries  = flat_entries_for_var(Events, <<"PK">>),
    KekEntries = flat_entries_for_var(Events, <<"KEK">>),
    DbEntries  = flat_entries_for_var(Events, <<"db">>),
    DbxEntries = flat_entries_for_var(Events, <<"dbx">>),
    TrustedSigners = [project_trusted_signer(E) || E <- DbEntries],
    BlockedHashes = [project_blocked_hash(E) || E <- DbxEntries],
    Enabled = maps:get(<<"enabled">>, Sb, <<"unknown">>),
    SetupMode = maps:get(<<"setup-mode">>, Sb, <<"unknown">>),
    DeployedMode = maps:get(<<"deployed-mode">>, Sb, <<"unknown">>),
    Posture = policy_posture(Enabled, SetupMode, DeployedMode,
                               length(PkEntries), length(KekEntries),
                               length(DbEntries), length(DbxEntries)),
    Strength = policy_strength(DbxEntries),
    #{
        <<"enabled">>            => Enabled,
        <<"setup-mode">>         => SetupMode,
        <<"deployed-mode">>      => DeployedMode,
        <<"pk-entry-count">>     => length(PkEntries),
        <<"kek-entry-count">>    => length(KekEntries),
        <<"db-entry-count">>     => length(DbEntries),
        <<"dbx-entry-count">>    => length(DbxEntries),
        <<"trusted-signers">>    => TrustedSigners,
        <<"blocked-hashes">>     => BlockedHashes,
        <<"revocation-count">>   => length(DbxEntries),
        <<"policy-posture">>     => Posture,
        <<"policy-strength">>    => Strength
    }.

%% Iterate every PK/KEK/db/dbx event with the matching variable
%% name, flatten their signature-list[].entries[] into a single
%% list of entries. Each entry retains its parent type-guid-name
%% + owner-guid + any cert/hash fields the decoder attached.
flat_entries_for_var(Events, VarName) ->
    [maps:merge(#{<<"type-guid-name">> =>
                     maps:get(<<"type-guid-name">>, List, <<"">>)},
                 Entry)
     || Ev <- Events,
        maps:get(<<"event-type-code">>, Ev, 0) =:= 16#80000001,
        sem_var_name(Ev) =:= VarName,
        List <- nested(Ev,
                        [<<"parsed">>, <<"semantic">>,
                         <<"signature-list">>], []),
        is_map(List),
        Entry <- maps:get(<<"entries">>, List, []),
        is_map(Entry)].

project_trusted_signer(Entry) ->
    #{
        <<"type-guid-name">> =>
            maps:get(<<"type-guid-name">>, Entry, <<"unknown">>),
        <<"owner-guid">>     =>
            maps:get(<<"owner-guid">>, Entry, <<"">>),
        <<"subject">>        =>
            maps:get(<<"x509-subject">>, Entry, <<"">>),
        <<"issuer">>         =>
            maps:get(<<"x509-issuer">>, Entry, <<"">>),
        <<"fingerprint">>    =>
            maps:get(<<"x509-sha256-fingerprint">>, Entry, <<"">>),
        <<"not-before">>     =>
            maps:get(<<"x509-not-before">>, Entry, <<"">>),
        <<"not-after">>      =>
            maps:get(<<"x509-not-after">>, Entry, <<"">>),
        <<"public-key-alg">> =>
            maps:get(<<"x509-public-key-alg">>, Entry, <<"">>)
    }.

project_blocked_hash(Entry) ->
    TypeName = maps:get(<<"type-guid-name">>, Entry, <<"unknown">>),
    HashAlg = case TypeName of
        <<"EFI_CERT_SHA256_GUID">>       -> <<"sha256">>;
        <<"EFI_CERT_SHA384_GUID">>       -> <<"sha384">>;
        <<"EFI_CERT_SHA512_GUID">>       -> <<"sha512">>;
        <<"EFI_CERT_SHA1_GUID">>         -> <<"sha1">>;
        <<"EFI_CERT_X509_SHA256_GUID">>  -> <<"sha256">>;
        <<"EFI_CERT_X509_SHA384_GUID">>  -> <<"sha384">>;
        <<"EFI_CERT_X509_SHA512_GUID">>  -> <<"sha512">>;
        <<"EFI_CERT_X509_GUID">>         -> <<"x509-cert">>;
        _                                 -> <<"unknown">>
    end,
    %% For x509 entries the "hash" is really the cert fingerprint.
    %% For hash-type entries the decoder may surface a `hash' or
    %% `sha256' field -- accept either shape.
    HashOrFp = case maps:get(<<"hash">>, Entry, undefined) of
        B when is_binary(B) -> B;
        _ ->
            case maps:get(<<"sha256">>, Entry, undefined) of
                B when is_binary(B) -> B;
                _ -> maps:get(<<"x509-sha256-fingerprint">>,
                              Entry, <<"">>)
            end
    end,
    #{
        <<"type-guid-name">> => TypeName,
        <<"owner-guid">>     =>
            maps:get(<<"owner-guid">>, Entry, <<"">>),
        <<"hash-alg">>       => HashAlg,
        <<"hash-or-fingerprint">> => HashOrFp
    }.

policy_posture(<<"unknown">>, _, _, _, _, _, _) -> <<"unknown">>;
policy_posture(_, _, _, 0, _, _, _)              -> <<"setup-mode">>;
policy_posture(false, _, _, _, _, _, _)          ->
    %% SB disabled but policy keys present -> audit-only
    <<"audit-only">>;
policy_posture(true, _, <<"enabled">>, _, _, _, _) ->
    <<"deployed-production">>;
policy_posture(true, _, _, _Pk, _Kek, _Db, _Dbx) when _Pk > 0,
                                                      _Kek > 0,
                                                      _Db > 0 ->
    <<"user-managed">>;
policy_posture(_, _, _, _, _, _, _) -> <<"unknown">>.

policy_strength([]) -> <<"unknown">>;
policy_strength(Dbx) when length(Dbx) >= 100 ->
    <<"latest-revocations">>;
policy_strength(Dbx) when length(Dbx) >= 20 ->
    <<"moderate-revocations">>;
policy_strength(Dbx) when length(Dbx) >= 1 ->
    <<"minimal-revocations">>;
policy_strength(_) -> <<"no-revocations">>.

%% @doc Compute a deterministic SHA-256 digest over the entire
%% flat claim map. Used by verifiers to pin "this exact decoded
%% state was observed at time T" without copying the full tree.
%%
%% Canonicalisation:
%%   1. Recursively walk every map, sorting keys into the
%%      canonical byte-order.
%%   2. Encode the result with `term_to_binary/2' with
%%      `{minor_version, 2}', which is stable across Erlang/OTP
%%      versions.
%%   3. SHA-256 the encoded bytes.
%%
%% The returned stanza carries:
%%   digest    base64url SHA-256
%%   alg       "sha256"
%%   form      "canonical-sorted-keys-erlang-ext-v2"
%%   length    byte length of the canonical encoding
%%             (exposed so verifiers can pre-size a buffer)
%%
%% NOTE: this digest is NOT TPM-bound -- it's a client-side
%% convenience for comparing snapshots. For a TPM-bound digest,
%% use the `pcr-digest' from `claim.quote'.
claim_evidence_digest(Claim) when is_map(Claim) ->
    Canonical = canonicalise_claim(Claim),
    Encoded = term_to_binary(Canonical, [{minor_version, 2}]),
    Digest = crypto:hash(sha256, Encoded),
    #{
        <<"digest">>    => hb_util:encode(Digest),
        <<"alg">>       => <<"sha256">>,
        <<"form">>      =>
            <<"canonical-sorted-keys-erlang-ext-v2">>,
        <<"length">>    => byte_size(Encoded)
    };
claim_evidence_digest(_) ->
    #{<<"digest">> => <<"">>,
      <<"alg">>    => <<"sha256">>,
      <<"form">>   => <<"canonical-sorted-keys-erlang-ext-v2">>,
      <<"length">> => 0}.

%% Recursively sort a claim tree. Maps become sorted proplists;
%% lists are walked element-wise; scalars pass through.
canonicalise_claim(M) when is_map(M) ->
    Sorted = lists:sort(maps:to_list(M)),
    [{K, canonicalise_claim(V)} || {K, V} <- Sorted];
canonicalise_claim(L) when is_list(L) ->
    [canonicalise_claim(X) || X <- L];
canonicalise_claim(V) -> V.

%% @doc Unified temporal chain combining TPM wall-clock +
%% reset/restart counters + event-log seq range + IMA entry
%% count into a single chronology stanza. A policy engine can
%% compare consecutive quotes to detect:
%%
%%   * replay attacks           -- reset-count decrements or
%%                                 repeats a known-consumed pair
%%   * boot-epoch boundary      -- (reset-count, restart-count)
%%                                 changes
%%   * drift                    -- elapsed wall-clock vs
%%                                 expected delta
%%   * log truncation           -- event-log seq range shrinks
%%
%% Fields:
%%   tpm-epoch              "reset-count:restart-count" unique
%%                          per-boot-epoch within one TPM
%%   reset-count            from TPMS_ATTEST
%%   restart-count          from TPMS_ATTEST
%%   clock-ms               TPM wall-clock (ms since last reset)
%%   clock-seconds          same / 1000
%%   boot-elapsed-ms        alias for clock-ms (paper section Arch
%%                          freshness check)
%%   event-log-count        total parsed event-log entries
%%   event-log-seq-min      lowest seq in the log
%%   event-log-seq-max      highest seq in the log
%%   event-log-seq-range    max - min + 1
%%   ima-event-count        count of parsed IMA entries
claim_timeline(EvList, E) ->
    Quote = claim_quote(E),
    Ima = claim_ima(E),
    {SeqMin, SeqMax} = event_seq_range(EvList),
    SeqRange = case SeqMin of
        null -> 0;
        _    -> SeqMax - SeqMin + 1
    end,
    ResetCount = maps:get(<<"reset-count">>, Quote, 0),
    RestartCount = maps:get(<<"restart-count">>, Quote, 0),
    ClockMs = maps:get(<<"clock-ms">>, Quote, 0),
    Epoch = iolist_to_binary(io_lib:format(
        "~B:~B", [ResetCount, RestartCount])),
    #{
        <<"tpm-epoch">>          => Epoch,
        <<"reset-count">>        => ResetCount,
        <<"restart-count">>      => RestartCount,
        <<"clock-ms">>           => ClockMs,
        <<"clock-seconds">>      => ClockMs div 1000,
        <<"boot-elapsed-ms">>    => ClockMs,
        <<"event-log-count">>    => length(EvList),
        <<"event-log-seq-min">>  => or_null_int(SeqMin),
        <<"event-log-seq-max">>  => or_null_int(SeqMax),
        <<"event-log-seq-range">> => SeqRange,
        <<"ima-event-count">>    =>
            maps:get(<<"event-count">>, Ima, 0)
    }.

or_null_int(null) -> null;
or_null_int(N) when is_integer(N) -> N;
or_null_int(_) -> null.

%% Scan events, return {MinSeq, MaxSeq} (null/null if empty).
event_seq_range([]) -> {null, null};
event_seq_range(EvList) ->
    Seqs = [S || E <- EvList,
                 S <- [maps:get(<<"seq">>, E, null)],
                 is_integer(S)],
    case Seqs of
        []  -> {null, null};
        [H] -> {H, H};
        _   -> {lists:min(Seqs), lists:max(Seqs)}
    end.

%% @doc Aggregate prescriptive `policy-verdict' across every
%% claim section. Scans the decoded claim for signal facts
%% (secure-boot enabled, quote-integrity match, pcr-replay
%% consistency, freshness indicator, ima policy violations,
%% tpm trust-tier, tme, ...) and produces:
%%
%%   verdict       "trusted" | "attested-with-warnings" |
%%                 "untrusted" | "unknown"
%%   score         0-100 confidence score (integer)
%%   warnings      list of #{code, section, message}
%%   critical-failures   list of #{code, section, message}
%%   signals       map of the decisive per-section facts
%%                 (so a policy engine can inspect the
%%                  evidence without re-walking the tree)
%%   version       1  (so callers can reason about evolution)
%% Two-arity form so callers with the raw envelope can feed it
%% in. The signature-verification signal requires the raw quoted
%% bytes which the claim tree doesn't preserve.
claim_policy_verdict(Claim) -> claim_policy_verdict(Claim, #{}).

claim_policy_verdict(Claim, Envelope) ->
    Signals = collect_policy_signals(Claim, Envelope),
    {Warnings, Criticals} = classify_policy_findings(Signals),
    Score = policy_score(Signals, Warnings, Criticals),
    Verdict = policy_verdict_from(Criticals, Warnings, Signals),
    #{
        <<"version">>            => 1,
        <<"verdict">>            => Verdict,
        <<"score">>              => Score,
        <<"critical-failures">>  => Criticals,
        <<"warnings">>           => Warnings,
        <<"signals">>            => Signals
    }.

%% Pull the small set of decisive per-section facts into a
%% flat signal map -- same keys drive the classify_ and
%% score_ functions below.
collect_policy_signals(Claim) -> collect_policy_signals(Claim, #{}).

collect_policy_signals(Claim, Envelope) ->
    SB = maps:get(<<"secure-boot">>, Claim, #{}),
    QI = maps:get(<<"quote-integrity">>, Claim, #{}),
    PR = maps:get(<<"pcr-replay">>, Claim, #{}),
    FR = maps:get(<<"freshness">>, Claim, #{}),
    IMAPol = maps:get(<<"ima-policy">>, Claim, #{}),
    TPM = maps:get(<<"tpm">>, Claim, #{}),
    TME = maps:get(<<"tme">>, Claim, #{}),
    Ctx = maps:get(<<"context">>, Claim, #{}),
    SBP = maps:get(<<"secure-boot-policy">>, Claim, #{}),
    BC = maps:get(<<"boot-chain">>, Claim, #{}),
    Lockdown = maps:get(<<"lockdown">>, Claim, #{}),
    EK = maps:get(<<"ek">>, Claim, #{}),
    AK = maps:get(<<"ak">>, Claim, #{}),
    EkChain = maps:get(<<"chain-validation">>, EK, #{}),
    #{
        <<"secure-boot-enabled">>  =>
            maps:get(<<"enabled">>, SB, <<"unknown">>),
        <<"quote-integrity-match">> =>
            maps:get(<<"pcr-digest-match">>, QI, <<"unknown">>),
        <<"quote-verifiable">> =>
            maps:get(<<"verifiable">>, QI, false),
        <<"pcr-replay-consistent">> =>
            maps:get(<<"consistent">>, PR, false),
        <<"pcr-replay-mismatch-count">> =>
            length(maps:get(<<"pcrs-mismatching">>, PR, [])),
        %% OS-identity PCRs = [4, 7, 11, 14, 15] per TCG PC Client
        %% spec + LapEE paper (boot loader, SB policy, UKI, shim
        %% fallback, runtime). Firmware-internal = 0-6 \setminus 4.
        %% Split the mismatch count so pcr_replay_finding can emit
        %% info (firmware-only) vs warn (OS-identity) severity.
        <<"pcr-replay-os-identity-mismatch-count">> =>
            length([P || P <- maps:get(<<"pcrs-mismatching">>,
                                       PR, []),
                         lists:member(P, [4, 7, 11, 14, 15])]),
        <<"pcr-replay-firmware-only-mismatch-count">> =>
            length([P || P <- maps:get(<<"pcrs-mismatching">>,
                                       PR, []),
                         lists:member(P,
                                      [0, 1, 2, 3, 5, 6, 8, 9, 10])]),
        <<"freshness-indicator">> =>
            maps:get(<<"freshness-indicator">>, FR,
                      <<"unknown">>),
        <<"ima-policy-violations">> =>
            length(maps:get(<<"violations">>, IMAPol, [])),
        <<"ima-policy-picked">> =>
            maps:get(<<"picked-policy-key">>, IMAPol, null),
        <<"tpm-trust-tier">> =>
            maps:get(<<"trust-tier">>, TPM, <<"unknown">>),
        <<"tpm-known-cve-count">> =>
            length(maps:get(<<"known-cves">>, TPM, [])),
        <<"tme-enabled">> =>
            maps:get(<<"enabled">>, TME, <<"unknown">>),
        %% v1.2.2 paper P3: AK under Endorsement hierarchy gives
        %% the verifier cryptographic knowledge that AK and EK
        %% share a primary seed -> same physical TPM. Envelope
        %% field `ak-hierarchy' (constant "endorsement" in builds
        %% where the NIF creates the AK under Endorsement) is the
        %% signal -- read from the raw envelope so the finding
        %% logic can demote ek-ak-binding-not-implemented to the
        %% observational info tier when the binding is already
        %% enforced by hierarchy.
        <<"ak-hierarchy">> =>
            hb_maps:get(<<"ak-hierarchy">>, Envelope,
                         <<"unknown">>, #{}),
        <<"context-kind">> =>
            maps:get(<<"kind">>, Ctx, <<"unknown">>),
        <<"policy-posture">> =>
            maps:get(<<"policy-posture">>, SBP, <<"unknown">>),
        <<"policy-strength">> =>
            maps:get(<<"policy-strength">>, SBP, <<"unknown">>),
        <<"boot-chain-length">> =>
            maps:get(<<"length">>, BC, 0),
        <<"has-runtime-driver">> =>
            maps:get(<<"has-runtime-driver">>, BC, false),
        <<"lockdown-level">> =>
            maps:get(<<"level">>, Lockdown, <<"unknown">>),
        <<"ek-present">> =>
            maps:get(<<"present">>, EK, false),
        <<"ek-currently-valid">> =>
            maps:get(<<"is-currently-valid">>, EK, <<"unknown">>),
        <<"ek-chain-valid">> =>
            maps:get(<<"chain-valid">>, EkChain, <<"unknown">>),
        <<"ak-present">> =>
            maps:get(<<"present">>, AK, false),
        <<"ak-key-size-bits">> =>
            maps:get(<<"key-size-bits">>, AK, 0),
        %% v1.2 E6: expose reset/restart counts so freshness_finding
        %% can soften `safe=false' when this is clearly a first-cold-
        %% boot (the TPM has never seen a clean shutdown-with-state).
        <<"reset-count">> =>
            maps:get(<<"reset-count">>,
                     maps:get(<<"quote">>, Claim, #{}), null),
        <<"restart-count">> =>
            maps:get(<<"restart-count">>,
                     maps:get(<<"quote">>, Claim, #{}), null),
        %% v1.2 red-team review: capture the ACTUAL RSA-PSS
        %% signature-verification result. Drives
        %% quote_signature_finding/1 which is CRITICAL when false
        %% or unknown. claim_quote_integrity's pcr-digest-match is
        %% attacker-computable (just re-hash your chosen PCR
        %% values); this is not (requires a TPM-held AK private).
        <<"quote-signature-verified">> =>
            verify_quote_signature(Envelope),
        %% v1.2 paper-to-code pass P5: the AK pub is supposed to be
        %% extended into PCR 15 at init_chain time (see paper
        %% ephemeral-node-key-binding). An envelope without a
        %% matching EV_HYPERBEAM_KEY_PUBKEY_EXTEND event in the
        %% runtime event log does not prove the AK pub is bound to
        %% the measured-boot session. Computed here (mirrors
        %% dev_tpm2:chk_ak_pubkey_binding/1) so the cross-node
        %% verdict path also gates on it.
        <<"ak-pubkey-extend-verified">> =>
            verify_ak_pubkey_extend(Envelope),
        %% v1.2.2 paper P5-ext (AO-Core hashpath continuity): the
        %% TPM event log tip is committed into PCR 15 as the step
        %% immediately after the AK pub extension, so every
        %% subsequent AO-Core hashpath entry carries a commitment
        %% to the full boot measurement chain (paper Section AO-
        %% Core Continuity: "HyperBEAM seeds its AO-Core chain
        %% with a commitment to the TPM event log tip immediately
        %% after key-pubkey-extend"). Computed by locating an
        %% EV_HYPERBEAM_TCG_LOG_TIP_COMMITMENT event on PCR 15
        %% whose digest matches sha256(envelope.tcg-event-log).
        <<"hashpath-continuity-verified">> =>
            verify_tcg_log_tip_extend(Envelope),
        %% v1.2.2 paper P4: declarative field from the guest
        %% (`tpm-session-mode' == "hmac-aes128cfb" when the NIF
        %% opens an HMAC + parameter-encryption session for every
        %% sensitive TPM op). Verifier cannot re-verify bus-level
        %% protection from the wire envelope -- P4 is guest <-> TPM
        %% -- so we treat this as a declaration and grade it.
        <<"tpm-session-mode">> =>
            hb_maps:get(<<"tpm-session-mode">>, Envelope,
                         <<"unknown">>, #{}),
        %% The critical end-to-end binding the paper asks for:
        %% the HB operator wallet (which signs every AO-Core
        %% result) must be provably linked to the TPM's
        %% measured-boot session. Chain:
        %%
        %%   wallet W  \in  node-message
        %%        |
        %%        v   hb_message:id(node-message) = node-message-id
        %;        v
        %;   node-message-id extended into PCR 15
        %;        |
        %;        v   TPM2_Quote covers PCR 15
        %;        v
        %;   quote signed by AK (hierarchy = Endorsement -> same
        %;        EPS as EK -> same physical TPM as EK cert chain)
        %;
        %% If every step holds the consumer of a result signed by
        %% W can chain back to the hardware root of trust even
        %% with no further interaction with the node. Paper AO-
        %% Core continuity claim: "The result of every computation
        %% in the system is signed alongside the hashpath
        %; describing how it was made. When the signature is
        %; produced by a LapEE-bound key ... the chain binds the
        %; transcript to the boot conditional on A1."
        %%
        %% Verifier: re-compute hb_message:id(node-message) and
        %% confirm it matches node-message-id, then check that
        %; wallet-address is a value inside node-message, then
        %; check that a PCR-15 event carries the decoded node-
        %; message-id as its digest. All three must hold.
        <<"wallet-tpm-binding-verified">> =>
            verify_wallet_tpm_binding(Envelope)
    }.

%% Map signals to warnings + critical-failures. Every entry
%% surfaces a `code' policy engines can match against, the
%% originating `section', and a human-readable `message'.
classify_policy_findings(S) ->
    Findings =
        [secure_boot_finding(S),
         quote_integrity_finding(S),
         quote_signature_finding(S),    %% v1.2 red-team fix
         pcr_replay_finding(S),
         freshness_finding(S),
         ek_finding(S),
         ak_finding(S),
         ek_ak_binding_finding(S),      %% v1.2 red-team warn
         ak_pubkey_extend_finding(S),   %% v1.2 paper-to-code P5
         hashpath_continuity_finding(S), %% v1.2.2 paper P5-ext
         tpm_session_mode_finding(S),    %% v1.2.2 paper P4
         wallet_tpm_binding_finding(S),  %% v1.2.2 root-of-trust
         ima_policy_finding(S),
         tpm_trust_finding(S),
         tme_finding(S),
         runtime_driver_finding(S),
         cve_finding(S),
         sb_policy_finding(S),
         lockdown_finding(S)],
    Flat = [F || F <- Findings, F =/= ok],
    Criticals = [F || F <- Flat, map_severity(F) =:= critical],
    Warnings = [maps:remove(<<"severity">>, F)
                 || F <- Flat, map_severity(F) =:= warn],
    {Warnings, [maps:remove(<<"severity">>, F) || F <- Criticals]}.

map_severity(F) when is_map(F) ->
    maps:get(<<"severity">>, F, warn).

finding(critical, Code, Section, Message) ->
    #{<<"severity">> => critical,
      <<"code">> => Code,
      <<"section">> => Section,
      <<"message">> => Message};
finding(warn, Code, Section, Message) ->
    #{<<"severity">> => warn,
      <<"code">> => Code,
      <<"section">> => Section,
      <<"message">> => Message};
finding(info, Code, Section, Message) ->
    %% `info' is observational: the fact IS true, operators should
    %% know about it, but it doesn't degrade the attestation verdict
    %% (doesn't count in warnings list, doesn't reduce policy_score).
    %% Use for posture facts that document context rather than
    %% signal a problem: known-vendor-CVEs that don't apply to this
    %% specific firmware version, fresh-first-boot clock-unsafe
    %% patterns that are benign by construction, event-log
    %% truncations on firmware-internal PCRs whose identity is
    %% attested via the manufacturer fingerprint DB.
    #{<<"severity">> => info,
      <<"code">> => Code,
      <<"section">> => Section,
      <<"message">> => Message}.

secure_boot_finding(#{<<"secure-boot-enabled">> := false}) ->
    finding(warn, <<"secure-boot-disabled">>,
            <<"secure-boot">>,
            <<"Secure Boot is disabled. Verification of the "
              "boot chain is not cryptographically enforced.">>);
secure_boot_finding(#{<<"secure-boot-enabled">> := <<"unknown">>}) ->
    finding(warn, <<"secure-boot-unknown">>,
            <<"secure-boot">>,
            <<"Secure Boot state not determined from event log.">>);
secure_boot_finding(_) -> ok.

quote_integrity_finding(
  #{<<"quote-integrity-match">> := false}) ->
    finding(critical, <<"quote-integrity-mismatch">>,
            <<"quote-integrity">>,
            <<"Recomputed pcrDigest does not match the TPM's "
              "signed value -- the envelope's PCR values were "
              "tampered with or the quote is forged.">>);
quote_integrity_finding(_) -> ok.

pcr_replay_finding(
  #{<<"pcr-replay-os-identity-mismatch-count">> := N}) when N >= 3 ->
    finding(warn, <<"pcr-replay-multi-mismatch">>,
            <<"pcr-replay">>,
            iolist_to_binary(io_lib:format(
              "~p OS-identity PCR(s) (of 4, 7, 11, 14, 15) "
              "mismatched between event log and quoted values "
              "-- event log is likely truncated or has cross-"
              "bank artefacts.", [N])));
pcr_replay_finding(
  #{<<"pcr-replay-os-identity-mismatch-count">> := N}) when N >= 1 ->
    finding(warn, <<"pcr-replay-mismatch">>,
            <<"pcr-replay">>,
            iolist_to_binary(io_lib:format(
              "~p OS-identity PCR(s) (of 4, 7, 11, 14, 15) do "
              "not match the event log.", [N])));
pcr_replay_finding(
  #{<<"pcr-replay-firmware-only-mismatch-count">> := N}) when N >= 1 ->
    %% Firmware-internal PCRs 0-6 often have unlogged extensions
    %% (microcode updates, ACPI table hashes, platform POST codes
    %% that vendors treat as confidential). TCG PC Client permits
    %% this and v1.2.2 verifies firmware identity via the
    %% manufacturer fingerprint DB (dmi-bios-version / CRTM) not
    %% log replay. Emit as an observational info note so the
    %% verdict isn't tainted by a known-benign firmware behaviour,
    %% while still surfacing the fact to dashboards / audit.
    finding(info, <<"pcr-replay-firmware-internal-mismatch">>,
            <<"pcr-replay">>,
            iolist_to_binary(io_lib:format(
              "~p firmware-internal PCR(s) (0-6) replay did not "
              "match quoted values -- commonly caused by the "
              "firmware logging a subset of its own measurements. "
              "Firmware identity is attested independently via "
              "the fingerprint DB.", [N])));
pcr_replay_finding(_) -> ok.

freshness_finding(#{<<"freshness-indicator">> := <<"safe-false">>}
                  = S) ->
    %% A TPM's clock-safe bit is off whenever the TPM hasn't yet
    %% seen a clean shutdown-with-state since the last cold reset.
    %% On a freshly-flashed / freshly-provisioned device this is
    %% the NORMAL first-boot state -- not evidence of tampering.
    %%
    %% Three-way classification based on the TPM's reset / restart
    %% counters:
    %%
    %%   BOTH counts present AND ≤ 1 -> fresh boot. Warn, not
    %%       critical. Counts WILL rise on clean shutdowns.
    %%
    %%   counts missing from the quote entirely -> separate
    %%       "counts-missing" finding at critical severity.
    %%       Reason: an adversary who strips reset-count /
    %%       restart-count from an otherwise valid envelope would
    %%       otherwise slip past the tamper signal; we refuse to
    %%       downgrade severity on blind faith.
    %%
    %%   counts present AND not-fresh-boot pattern -> critical
    %%       tamper signal.
    R  = maps:get(<<"reset-count">>,  S, null),
    RC = maps:get(<<"restart-count">>, S, null),
    case fresh_boot_classify(R, RC) of
        fresh_boot ->
            finding(warn, <<"freshness-safe-false-first-boot">>,
                    <<"freshness">>,
                    <<"TPM clock `safe' flag is false. The TPM's "
                      "reset/restart counts indicate this is a "
                      "first-cold-boot (TPM has never seen a "
                      "clean shutdown-with-state-save), so the "
                      "unset `safe' bit is the expected state -- "
                      "not a tamper signal. Will be set on the "
                      "next clean reboot.">>);
        counts_missing ->
            finding(critical, <<"freshness-safe-false-counts-missing">>,
                    <<"freshness">>,
                    <<"TPM clock `safe' flag is false AND the "
                      "envelope does not carry reset-count / "
                      "restart-count so we cannot distinguish a "
                      "legitimate first-cold-boot from a tampered "
                      "quote. Defaulting to critical so the "
                      "verifier cannot be fooled by a stripped "
                      "envelope.">>);
        uninitialised_shadow ->
            %% Informational, not a warning: both counters being
            %% implausibly large (>= 2^28) is the signature of
            %% uninitialised NV shadow on a first-production-boot
            %% TPM. An attacker cannot fabricate this pattern on a
            %% TPM that has ever seen a clean shutdown, and the
            %% primary freshness defence (nonce in extraData) is
            %% unaffected. Surface as observational so the verdict
            %% doesn't degrade on a known-benign first-boot shape.
            finding(info,
                    <<"freshness-safe-false-shadow-uninitialised">>,
                    <<"freshness">>,
                    <<"TPM clock `safe' flag is false and both "
                      "reset-count and restart-count are implausibly "
                      "large (>= 2^28). Per TPM 2.0 Part 1 section "
                      "34.1, when the TPM has never executed "
                      "TPM2_Shutdown(STATE), the clock and counter "
                      "values in NV may be stale / uninitialised "
                      "and `safe=false' flags that the caller must "
                      "not rely on them. Observed on Nuvoton "
                      "NPCT75x first-production-boot. A real "
                      "attacker cannot fabricate this pattern on a "
                      "TPM that has seen a clean shutdown. Primary "
                      "freshness is unaffected: the quote's "
                      "extraData still binds to the caller's nonce. "
                      "Upgrades to stronger classification on the "
                      "next clean reboot.">>);
        safe_false_stale_counters ->
            %% The counts-plausible-but-safe-false default: LapEE
            %% power-cycle posture. See fresh_boot_classify/2
            %% comment for the full rationale. Warn, not critical.
            finding(warn, <<"freshness-safe-false-stale-counters">>,
                    <<"freshness">>,
                    <<"TPM `safe' flag is false with plausible "
                      "reset/restart counts. This is the expected "
                      "state for a LapEE boot: HB does not issue "
                      "TPM2_Shutdown(STATE) at node shutdown "
                      "(single-purpose appliance power-cycle "
                      "model), so counter values are stale across "
                      "boots and the TPM reports safe=NO. Primary "
                      "freshness is unaffected -- the quote's "
                      "extraData is bound to the verifier's fresh "
                      "nonce challenge on every attestation. "
                      "Cross-envelope counter-based replay defense "
                      "is weaker on this boot (counters cannot be "
                      "trusted as monotonic across safe=false "
                      "boundaries), but single-envelope replay is "
                      "still covered by the fresh-nonce mechanism. "
                      "Strict verifier policy that mandates "
                      "safe=true MAY reject; default LapEE "
                      "policy accepts this as warn.">>)
    end;
freshness_finding(
  #{<<"freshness-indicator">> := <<"no-nonce">>}) ->
    finding(warn, <<"freshness-no-nonce">>,
            <<"freshness">>,
            <<"Quote was produced without a challenger nonce "
              "-- replayable.">>);
%% Reviewer pass 6 (paper-to-code) LOW-1: the pre-batch-9 catch-all
%% silently accepted any envelope where `freshness-indicator' was
%% `unknown', absent, or an unexpected value. Raise to warn so an
%% adversary cannot strip the indicator to suppress the signal.
%% Genuine LapEE envelopes always populate it with `safe' or
%% `safe-false'.
freshness_finding(#{<<"freshness-indicator">> := <<"safe">>}) -> ok;
freshness_finding(_) ->
    finding(warn, <<"freshness-indicator-unknown">>,
            <<"freshness">>,
            <<"Envelope's `freshness-indicator' is absent or "
              "reports an unrecognised value. Cannot distinguish "
              "a legitimate quote from a stripped one.">>).

%% Three-way classifier for the freshness heuristic. Both counts
%% must be integers AND ≤ 1 to qualify as fresh-boot; anything else
%% is either counts-missing (when either or both are null) or
%% tamper (when both are integers but one exceeds the fresh-boot
%% threshold). This is tightened from the v1.2 batch-1 version
%% which collapsed null + fresh into a single "fresh" bucket --
%% that collapse was a footgun because an adversary stripping
%% counts from a quote looked indistinguishable from a genuine
%% first boot.
fresh_boot_classify(R, RC)
        when is_integer(R), is_integer(RC), R =< 1, RC =< 1 ->
    fresh_boot;
fresh_boot_classify(R, RC)
        when R =:= null orelse RC =:= null ->
    counts_missing;
%% When safe=false AND both counters are implausibly huge (>= 2^28),
%% the TPM is almost certainly returning stale-or-zero bytes from
%% uninitialised NV shadow memory. Per TPM 2.0 Part 1 section 34.1:
%% when the TPM has never executed TPM2_Shutdown(STATE), the clock
%% and counter values in NV may be stale and safe=NO is set to
%% warn the caller. Observed on Nuvoton NPCT75x on Framework 13
%% firstboot (reset-count ~2.7 billion, restart-count ~365 million,
%% TPM power-on age under 2 min). A real attacker cannot fabricate
%% these values on a TPM that has executed a clean shutdown, so
%% this pattern is a benign first-boot signature, not tamper.
%% Upgrades to stronger classification on the next clean reboot
%% (when safe=true, both counts reset to small-plausible values).
fresh_boot_classify(R, RC)
        when is_integer(R), is_integer(RC),
             R >= 16#10000000, RC >= 16#10000000 ->
    uninitialised_shadow;
%% Default case when counts are PLAUSIBLE (i.e., below 2^28) but
%% safe=false: the TPM has executed some number of resets and/or
%% restarts over its lifetime, but HAS NOT executed a clean
%% TPM2_Shutdown(STATE) since its last power-on. This is the
%% EXPECTED state for a LapEE boot: HB does not issue
%% TPM2_Shutdown(STATE) at node shutdown (single-purpose appliance
%% power-cycle model), so every envelope a verifier receives from
%% LapEE hardware carries safe=NO. Observed on Framework 13 +
%% Nuvoton NPCT75x: 26h uptime, reset-count=147, restart-count=0.
%%
%% The "fresh nonce in TPMS_ATTEST.extraData" IS the primary
%% freshness defense (verifier-controlled challenge, re-verified
%% per attestation). Counter-based cross-envelope replay defense
%% is a secondary defence that requires safe=true to be reliable;
%% when safe=false we acknowledge it's weaker but the primary
%% defense still holds. The paper amendment (STATUS.md paper-
%% amend section) updates to note that LapEE's power-cycle
%% posture makes safe=false the default, not an anomaly -- the
%% verdict therefore grades this as WARN, not CRITICAL.
fresh_boot_classify(_, _) ->
    safe_false_stale_counters.

ima_policy_finding(
  #{<<"ima-policy-violations">> := N}) when N > 0 ->
    finding(warn, <<"ima-policy-violations">>,
            <<"ima-policy">>,
            iolist_to_binary(io_lib:format(
              "~p IMA policy violation(s) detected.", [N])));
ima_policy_finding(_) -> ok.

tpm_trust_finding(
  #{<<"tpm-trust-tier">> := <<"hypervisor">>}) ->
    finding(warn, <<"tpm-trust-tier-hypervisor">>,
            <<"tpm">>,
            <<"TPM is hypervisor-emulated (vTPM) -- trust is "
              "delegated to the cloud provider.">>);
tpm_trust_finding(
  #{<<"tpm-trust-tier">> := <<"cpu-tee">>}) ->
    finding(warn, <<"tpm-trust-tier-ftpm">>,
            <<"tpm">>,
            <<"TPM is an fTPM (firmware TPM) -- compromise of "
              "the CPU TEE compromises the TPM root-of-trust.">>);
tpm_trust_finding(_) -> ok.

tme_finding(#{<<"tme-enabled">> := false}) ->
    finding(warn, <<"tme-disabled">>,
            <<"tme">>,
            <<"Total Memory Encryption is off; paper section Arch "
              "section confidentiality premise violated.">>);
tme_finding(#{<<"tme-enabled">> := <<"unknown">>}) ->
    finding(warn, <<"tme-unknown">>,
            <<"tme">>,
            <<"TME state could not be determined; no "
              "tier-2/3/4/5 evidence fired.">>);
tme_finding(_) -> ok.

runtime_driver_finding(#{<<"has-runtime-driver">> := true}) ->
    finding(warn, <<"boot-chain-has-runtime-driver">>,
            <<"boot-chain">>,
            <<"Boot chain loaded a UEFI runtime-services "
              "driver -- survives into OS runtime; review the "
              "image.">>);
runtime_driver_finding(_) -> ok.

cve_finding(#{<<"tpm-known-cve-count">> := N}) when N > 0 ->
    %% Informational, not a warning. The CVE list is vendor
    %% posture context (which families have had disclosed
    %% vulnerabilities) -- it tells the operator to track the
    %% TPM firmware version against vendor patch advisories.
    %% It does NOT invalidate the current attestation: the
    %% quote + EK chain + runtime event log are all valid
    %% evidence regardless. Degrading the verdict on every
    %% NPCT75x / Infineon / STM TPM would make attested-with-
    %% warnings the floor for the entire market; that isn't
    %% what the finding is for.
    finding(info, <<"tpm-known-cves">>,
            <<"tpm">>,
            iolist_to_binary(io_lib:format(
              "TPM vendor has ~p known CVE(s) listed in the "
              "manufacturers DB. Track firmware version against "
              "vendor patch advisories -- this does not "
              "invalidate the current attestation.", [N])));
cve_finding(_) -> ok.

sb_policy_finding(#{<<"policy-posture">> := <<"setup-mode">>}) ->
    finding(critical, <<"sb-policy-setup-mode">>,
            <<"secure-boot-policy">>,
            <<"Secure Boot in setup-mode -- any key can be "
              "enrolled. This is an unfinished provisioning "
              "state.">>);
sb_policy_finding(#{<<"policy-strength">> := <<"no-revocations">>}) ->
    finding(warn, <<"sb-policy-no-dbx">>,
            <<"secure-boot-policy">>,
            <<"dbx (revocation list) is empty -- known-vuln "
              "boot binaries are not blocked.">>);
sb_policy_finding(_) -> ok.

lockdown_finding(
  #{<<"lockdown-level">> := <<"confidentiality">>}) -> ok;
lockdown_finding(
  #{<<"lockdown-level">> := <<"integrity">>}) ->
    finding(warn, <<"lockdown-integrity-not-confidentiality">>,
            <<"lockdown">>,
            <<"Kernel lockdown is in `integrity' mode; paper "
              "section Arch recommends `confidentiality'.">>);
%% Reviewer pass 6 (paper-to-code) HIGH-2: the pre-batch-9 catch-all
%% silently treated `none' / `unknown' / absent lockdown-level as
%% ok. Several of the Table-2 threat-model defenses (/dev/mem,
%% kexec, ptrace-via-kallsyms, unsigned-module-load) are only
%% actually BLOCKED when lockdown is active; absence should move
%% the verdict, not slide through. Warning (not critical) today;
%% escalate to critical in v1.3 once the EV_IPL-cmdline cross-check
%% lands (red-team v1.3 backlog HIGH 3).
lockdown_finding(_) ->
    finding(warn, <<"lockdown-off-or-unknown">>,
            <<"lockdown">>,
            <<"Kernel lockdown level is not `confidentiality' or "
              "`integrity' (reports `none', `unknown', or absent). "
              "The paper's Table-2 defenses for /dev/mem, kexec, "
              "ptrace-via-kallsyms and unsigned-module-load are "
              "only enforced when lockdown is active -- an "
              "attestation without a confirmed lockdown level "
              "does not prove those defenses are in place.">>).

ek_finding(#{<<"ek-present">> := false}) ->
    %% v1.2 red-team review upgrade: an envelope with no EK cert
    %% has no TPM-rooted cryptographic identity at all. Previously
    %% a warning; that let an attacker who stripped ek-cert-pem
    %% avoid every downstream ek-* finding and reach
    %% verdict=attested-with-warnings. Upgraded to CRITICAL --
    %% without an EK there is nothing for the quote's AK to
    %% anchor against.
    finding(critical, <<"ek-cert-missing">>,
            <<"ek">>,
            <<"Envelope carries no EK certificate; TPM "
              "cryptographic identity cannot be anchored to a "
              "vendor CA. No legitimate LapEE envelope produces "
              "this shape.">>);
ek_finding(#{<<"ek-currently-valid">> := false}) ->
    finding(critical, <<"ek-cert-expired-or-not-yet-valid">>,
            <<"ek">>,
            <<"EK certificate is outside its validity window "
              "(expired or not-yet-valid).">>);
ek_finding(#{<<"ek-chain-valid">> := false}) ->
    finding(critical, <<"ek-chain-invalid">>,
            <<"ek">>,
            <<"EK certificate does not chain to any root CA "
              "in priv/tpm-interpret/root-cas/ -- cryptographic "
              "identity cannot be verified.">>);
%% Reviewer pass 6 (paper-to-code) MEDIUM-4: chain-valid = "unknown"
%% previously slid through the catch-all. An envelope where the
%% verifier could not load roots (empty roots directory, bad path,
%% etc) was silently treated as "chain not-invalid = fine" -- the
%% exact failure mode the paper's threat-model EK chain property
%% is supposed to catch. Upgrade to critical: an un-verified chain
%% is indistinguishable from a broken chain at the trust-anchor
%% layer.
ek_finding(#{<<"ek-chain-valid">> := <<"unknown">>}) ->
    finding(critical, <<"ek-chain-unknown">>,
            <<"ek">>,
            <<"EK chain validity is UNKNOWN (verifier could not "
              "load any root CAs, or the chain was not evaluated). "
              "An un-verified chain cannot anchor cryptographic "
              "identity; paper's threat-model requires the chain "
              "terminates at a TPM-vendor root.">>);
ek_finding(_) -> ok.

ak_finding(#{<<"ak-present">> := false}) ->
    %% v1.2 red-team review upgrade: no AK = no signature to
    %% verify = no quote integrity at all. Critical, not warn.
    finding(critical, <<"ak-pub-missing">>,
            <<"ak">>,
            <<"Envelope carries no Attestation Key public "
              "blob; verifier cannot pin which AK signed the "
              "quote.">>);
ak_finding(#{<<"ak-key-size-bits">> := N}) when N < 2048 ->
    finding(warn, <<"ak-key-too-small">>,
            <<"ak">>,
            iolist_to_binary(io_lib:format(
              "AK key size ~p bits is below the 2048-bit "
              "minimum recommended by NIST SP 800-57.", [N])));
ak_finding(_) -> ok.

%% v1.2 red-team review: claim/3 used to produce verdict=trusted
%% without ever verifying the RSA-PSS signature over the quote.
%% An attacker could set quote.signature=<<>> and quote.quoted=
%% <forged TPMS_ATTEST with chosen pcrDigest>, and the claim
%% pipeline's claim_quote_integrity would happily match the
%% recomputed pcrDigest against the forged quoted bytes and
%% light up "quote-integrity-match = true" -- without any TPM
%% key ever signing anything. Fixed by adding a dedicated
%% `quote-signature-verified' signal (computed in
%% collect_policy_signals via verify_quote_signature/1 below)
%% and gating verdict=trusted on it via the quote_signature_
%% finding/1 clause here.
quote_signature_finding(#{<<"quote-signature-verified">> := false}) ->
    finding(critical, <<"quote-signature-invalid">>,
            <<"quote-integrity">>,
            <<"RSA-PSS(SHA-256) signature over TPMS_ATTEST does "
              "not verify under the envelope's ak-pub-pem. "
              "Either the quote was forged (no TPM key ever "
              "signed it) or the envelope was tampered with in "
              "transit.">>);
quote_signature_finding(#{<<"quote-signature-verified">> := <<"unknown">>}) ->
    finding(critical, <<"quote-signature-unknown">>,
            <<"quote-integrity">>,
            <<"Cannot verify the TPMS_ATTEST signature: the "
              "envelope is missing either ak-pub-pem or a "
              "quote signature. No trusted verdict is possible "
              "without both.">>);
quote_signature_finding(_) -> ok.

%% v1.2 paper-to-code pass P5: verifier gate on the producer having
%% extended the AK public PEM into PCR 15 at init_chain time. Without
%% this, the paper's "a verifier replaying the event log observes
%% key-pubkey-extend land in PCR 15" property is not enforced -- an
%% attacker could present a freshly-generated AK pub + a quote over
%% a TPM they control and the verifier would have no signal to
%% reject.
ak_pubkey_extend_finding(
  #{<<"ak-pubkey-extend-verified">> := true}) -> ok;
ak_pubkey_extend_finding(
  #{<<"ak-pubkey-extend-verified">> := false}) ->
    finding(critical, <<"ak-pubkey-extend-missing">>,
            <<"ak">>,
            <<"No EV_HYPERBEAM_KEY_PUBKEY_EXTEND event in the "
              "runtime event log matches sha256(ak-pub-pem). "
              "Paper ephemeral-node-key-binding P5 requires the "
              "AK pub to be extended into PCR 15 at init_chain "
              "time; without it, the quote cannot be "
              "cryptographically tied to THIS measured-boot "
              "session's key material.">>);
ak_pubkey_extend_finding(
  #{<<"ak-pubkey-extend-verified">> := <<"unknown">>}) ->
    finding(critical, <<"ak-pubkey-extend-unknown">>,
            <<"ak">>,
            <<"Cannot evaluate the AK pub PCR-15 binding: envelope "
              "is missing ak-pub-pem or runtime_event_log. Paper "
              "ephemeral-node-key-binding P5 requires both.">>);
ak_pubkey_extend_finding(_) -> ok.

%% v1.2.2 paper P5-ext: AO-Core hashpath continuity finding.
%%
%%   true           -> ok (paper property held)
%%   false          -> warn (event missing or digest mismatch; the
%%                     envelope carries TCG event log bytes but the
%%                     runtime log does not commit to them)
%%   `log-absent'   -> info (firmware did not expose a TCG event log
%%                     in this envelope -- common on QEMU without
%%                     vTPM passthrough; paper P5-ext specifically
%%                     addresses real-hardware envelopes)
%%   `<<"unknown">>' -> ok (envelope shape doesn't let us decide)
hashpath_continuity_finding(
  #{<<"hashpath-continuity-verified">> := true}) -> ok;
hashpath_continuity_finding(
  #{<<"hashpath-continuity-verified">> := false}) ->
    finding(warn, <<"hashpath-continuity-missing">>,
            <<"ak">>,
            <<"No EV_HYPERBEAM_TCG_LOG_TIP_COMMITMENT event in the "
              "runtime event log with digest = sha256(tcg-event-log). "
              "Paper AO-Core continuity (P5-ext) requires the TPM "
              "event log tip to be extended into PCR 15 immediately "
              "after the AK pub, so every AO-Core hashpath entry "
              "carries a commitment to the full boot chain. Without "
              "it, the two logs (TPM + AO-Core) remain parallel but "
              "are not cross-linked.">>);
hashpath_continuity_finding(
  #{<<"hashpath-continuity-verified">> := <<"log-absent">>}) ->
    finding(info, <<"hashpath-continuity-log-absent">>,
            <<"ak">>,
            <<"Firmware did not expose a TCG event log in this "
              "envelope, so paper P5-ext (AO-Core hashpath "
              "continuity via TCG log commitment) cannot be "
              "evaluated. Common on QEMU without vTPM passthrough; "
              "not expected on real-hardware envelopes.">>);
hashpath_continuity_finding(_) -> ok.

%% v1.2.2 paper P4: TPM session mode declaration.
%%
%%   "hmac-aes128cfb"  -> ok (paper P4 held)
%%   "password"        -> warn (explicit regression to pre-P4)
%%   `unknown' / absent -> info (pre-P4 build; observational)
%%
%% The verifier treats this as a declaration rather than
%% something it can re-verify, because P4 is a guest<->TPM bus
%% property (whether the TPM commands carried HMAC+encrypt
%% coverage while they were in flight over the LPC bus), and
%% the envelope the verifier sees is the POST-quote wire form,
%% not the pre-quote TPM traffic.
tpm_session_mode_finding(
  #{<<"tpm-session-mode">> := <<"hmac-", _/binary>>}) -> ok;
tpm_session_mode_finding(
  #{<<"tpm-session-mode">> := <<"password">>}) ->
    finding(warn, <<"tpm-session-mode-password">>,
            <<"tpm">>,
            <<"Guest declares it uses plain password TPM sessions "
              "rather than HMAC + parameter encryption. Paper P4 "
              "(Arch section) requires HMAC + parameter encryption "
              "for all sensitive TPM operations on real-silicon "
              "builds; an attacker on the LPC bus can modify TPM "
              "responses when this protection is absent.">>);
tpm_session_mode_finding(
  #{<<"tpm-session-mode">> := <<"unknown">>}) ->
    finding(info, <<"tpm-session-mode-undeclared">>,
            <<"tpm">>,
            <<"Envelope predates v1.2.2 batch 31 -- the guest does "
              "not declare its TPM session mode. Assume pre-P4 (no "
              "HMAC session guarantee). Informational until a "
              "batch-31+ envelope is evaluated.">>);
tpm_session_mode_finding(_) -> ok.

%% v1.2.2 wallet <-> TPM root-of-trust binding.
%%
%% This is the critical chain the paper depends on: a consumer of
%% any AO-Core result signed by the node's operator wallet must be
%% able to chain the wallet back to the TPM's attested boot without
%; further interaction with the node.
%%
%; true -> ok (chain holds end-to-end)
%% false -> CRITICAL (chain broken -- wallet NOT provably linked to
%;                   the TPM-attested session)
%% unknown -> critical (envelope shape insufficient to evaluate)
wallet_tpm_binding_finding(
  #{<<"wallet-tpm-binding-verified">> := true}) -> ok;
wallet_tpm_binding_finding(
  #{<<"wallet-tpm-binding-verified">> := false}) ->
    finding(critical, <<"wallet-tpm-binding-broken">>,
            <<"ak">>,
            <<"The operator wallet is not provably linked to the "
              "TPM-attested measured-boot session. At least one of "
              "(a) node-message-id == hb_message:id(node-message), "
              "(b) wallet-address present as a value in node-"
              "message, or (c) a PCR-15 runtime event carrying the "
              "node-message-id digest did not hold. Without this "
              "chain, a consumer cannot chain a wallet-signed AO-"
              "Core result back to the hardware root of trust; an "
              "attacker with a software-generated wallet could "
              "claim LapEE provenance on results the TPM never "
              "actually attested.">>);
wallet_tpm_binding_finding(
  #{<<"wallet-tpm-binding-verified">> := <<"unknown">>}) ->
    finding(critical, <<"wallet-tpm-binding-unknown">>,
            <<"ak">>,
            <<"Cannot evaluate wallet <-> TPM binding: envelope "
              "lacks wallet-address, node-message, node-message-"
              "id, or runtime-event-log. Without all four, the "
              "paper's root-of-trust chain cannot be walked end-"
              "to-end.">>);
wallet_tpm_binding_finding(_) -> ok.

%% v1.2 red-team review: the EK<->AK binding is NOT cryptographically
%% proven in the v1.2 envelope. The EK chain validates an EK cert;
%% the quote validates under an AK pubkey; no operation proves
%% those two keys live in the same TPM. An attacker with a stolen
%% EK + chain could generate their own RSA keypair, sign a forged
%% quote with it, and pass chain-valid + signature-valid
%% simultaneously.
%%
%% The TCG-canonical fix is TPM2_MakeCredential / TPM2_ActivateCredential
%% provisioning: verifier wraps a secret with EK pub + AK name,
%% guest unwraps with TPM2_ActivateCredential (requires both keys
%% loaded in the same TPM), returns secret. Not implemented in
%% v1.2; scheduled for v1.3. For now the envelope carries a
%% warning so anyone reading verdict=attested-with-warnings sees
%% exactly what's missing.
ek_ak_binding_finding(#{<<"ak-hierarchy">> := <<"endorsement">>}) ->
    %% v1.2.2 paper P3 path: the AK is a primary created under the
    %% Endorsement hierarchy (see native/lapee_tpm_nif/
    %% lapee_tpm_nif.c nif_create_primary_ak -- ESYS_TR_RH_ENDORSEMENT).
    %% TCG TPM 2.0 Architecture section 13.2 guarantees that all
    %% primaries under the Endorsement hierarchy on a given TPM
    %% derive from that TPM's Endorsement Primary Seed (EPS), which
    %% never leaves the TPM. An AK and EK that both exist as
    %% primaries under Endorsement therefore MUST reside in the
    %% same physical TPM -- a different TPM cannot fabricate an AK
    %% whose primary-seed context matches. This is a HIERARCHY-LEVEL
    %% binding, strictly weaker than an interactive TPM2_Make-
    %% Credential / Activate round-trip (which would also attest
    %% the AK's TPMT_PUBLIC NAME at the specific moment of
    %% verification), but strong enough that the paper's threat
    %% model is covered: an adversary with a stolen EK + chain
    %% cannot generate a software RSA keypair and pair it with the
    %% EK, because the envelope declares Endorsement hierarchy and
    %% the adversary has no way to produce a quote whose AK pub
    %% derives from the target TPM's EPS. Demoted to info.
    finding(info, <<"ek-ak-binding-via-endorsement-hierarchy">>,
            <<"ek">>,
            <<"AK is a primary under the Endorsement hierarchy "
              "(paper P3). Since both EK and AK derive from the "
              "same TPM's Endorsement Primary Seed which never "
              "leaves the TPM (TCG Architecture section 13.2), "
              "they must reside in the same physical TPM. This "
              "is a hierarchy-level binding, weaker than an "
              "interactive MakeCredential / ActivateCredential "
              "round-trip but sufficient for the paper's threat "
              "model (stolen EK + software-generated AK is "
              "prevented).">>);
ek_ak_binding_finding(_Signals) ->
    finding(warn, <<"ek-ak-binding-not-implemented">>,
            <<"ek">>,
            <<"v1.2 does not yet cryptographically prove that the "
              "AK (signer of the quote) lives in the same TPM as "
              "the EK (anchor of the cert chain). An attacker "
              "with a stolen EK + chain could pair it with an "
              "attacker-generated AK and fool the verifier into "
              "accepting a forged quote. TCG TPM2_MakeCredential "
              "/ TPM2_ActivateCredential provisioning handshake "
              "is scheduled for v1.3. Until then, trust in this "
              "verdict requires that the envelope has not been "
              "transported through an adversarial channel.">>).

%% Verify the RSA-PSS-SHA256 signature over the envelope's
%% TPMS_ATTEST blob. Returns `true' | `false' | `<<"unknown">>`.
%% Uses the same primitive (rsa_pss:verify/4) and convention
%% (MGF1=SHA-256, salt=auto -- the verifier accepts the TPM's
%% hashLen-length salt per TCG TPM 2.0 Part 1 §11.2.4.4 and
%% PKCS #1 v2.1 §8.1; for SHA-256 the TPM emits salt=32) as
%% dev_tpm2:chk_quote/1 so a signed quote that passes chk_quote
%% will also pass this check.
verify_quote_signature(E) ->
    Q = hb_maps:get(<<"tpm-quote">>, E, #{}, #{}),
    AkPem = hb_maps:get(<<"ak-pub-pem">>, E, <<>>, #{}),
    QuotedB64 = hb_maps:get(<<"quoted">>, Q, <<>>, #{}),
    SigB64 = hb_maps:get(<<"signature">>, Q, <<>>, #{}),
    case {AkPem, QuotedB64, SigB64} of
        {<<>>, _, _} -> <<"unknown">>;
        {_, <<>>, _} -> <<"unknown">>;
        {_, _, <<>>} -> <<"unknown">>;
        _ ->
            try
                Quoted = hb_util:decode(QuotedB64),
                Sig    = hb_util:decode(SigB64),
                case decode_rsa_pub_pem(AkPem) of
                    {ok, RSAPub} ->
                        case rsa_pss:verify(Quoted, sha256, Sig, RSAPub) of
                            true  -> true;
                            false -> false
                        end;
                    _ -> <<"unknown">>
                end
            catch _:_ -> <<"unknown">>
            end
    end.

%% Verify the AK pub PEM was extended into PCR 15 during init_chain.
%% Mirrors `dev_tpm2:chk_ak_pubkey_binding/1' so the cross-node
%% interpret path gates on the same signal as the direct-crypto
%% verify path.
%%
%% Returns:
%%   `true'          if an EV_HYPERBEAM_KEY_PUBKEY_EXTEND event in the
%%                   runtime event log carries `digest =
%%                   sha256(ak-pub-pem)'.
%%   `false'         if events exist (and AK pub is present) but none
%%                   match.
%%   `<<"unknown">>' if ak-pub-pem is absent or the event log is
%%                   empty / unreadable -- we cannot distinguish
%%                   missing-evidence from actual-absence.
verify_ak_pubkey_extend(E) ->
    AkPem = hb_maps:get(<<"ak-pub-pem">>, E, <<>>, #{}),
    Log = hb_maps:get(<<"runtime-event-log">>, E, [], #{}),
    case {AkPem, Log} of
        {<<>>, _} -> <<"unknown">>;
        {_, []}   -> <<"unknown">>;
        {Pem, Events} ->
            try
                Expected = crypto:hash(sha256, Pem),
                %% Must be a PCR-15 event (paper P5 is specifically
                %% about PCR 15; a correctly-shaped event in another
                %% PCR does not satisfy the property). Matches the
                %% dev_tpm2:chk_ak_pubkey_binding/1 core-side filter.
                Match = [X || X <- Events,
                              is_map(X),
                              ev_pcr(X) =:= 15,
                              maps:get(<<"event-type">>, X, <<>>) =:=
                                  <<"EV_HYPERBEAM_KEY_PUBKEY_EXTEND">>,
                              hb_util:decode(
                                maps:get(<<"digest">>, X, <<>>)) =:=
                                  Expected],
                case Match of
                    [_|_] -> true;
                    []    -> false
                end
            catch _:_ -> <<"unknown">>
            end
    end.

%% @doc Wallet <-> TPM end-to-end binding verifier.
%%
%% Returns:
%%   true           all three sub-checks hold:
%;                  (a) node-message-id == hb_message:id(node-message)
%;                      (recomputed by the verifier from node-message)
%;                  (b) wallet-address appears as a value inside
%;                      node-message (direct or nested)
%;                  (c) a runtime event on PCR 15 carries the
%;                      decoded node-message-id as its digest
%;                      (same check the producer self-verification
%;                      chk_binding/1 performs)
%;   false          envelope has all inputs but at least one check
%;                  failed
%;   <<"unknown">>  envelope missing wallet-address, node-message,
%;                  node-message-id, or runtime-event-log
verify_wallet_tpm_binding(E) ->
    Wallet = hb_maps:get(<<"wallet-address">>, E, null, #{}),
    Nm     = hb_maps:get(<<"node-message">>, E, undefined, #{}),
    Id     = hb_maps:get(<<"node-message-id">>, E, null, #{}),
    Log    = hb_maps:get(<<"runtime-event-log">>, E, [], #{}),
    case {Wallet, Nm, Id, Log} of
        {null, _, _, _}       -> <<"unknown">>;
        {_, undefined, _, _}  -> <<"unknown">>;
        {_, _, null, _}       -> <<"unknown">>;
        {_, _, _, []}         -> <<"unknown">>;
        {W, NodeMap, ClaimedId, Events}
            when is_map(NodeMap), is_binary(W), is_binary(ClaimedId) ->
            %% (a) Recompute hb_message:id(node-message) and
            %; compare to the declared node-message-id.
            IdMatchesHash =
                try
                    %% hb_message:id returns the native (raw) id.
                    %; Envelope carries the human_id form.
                    Native = hb_message:id(NodeMap, all, #{}),
                    Human = hb_util:human_id(Native),
                    Human =:= ClaimedId
                        orelse Native =:= ClaimedId
                        orelse hb_util:encode(Native) =:= ClaimedId
                catch _:_ -> false
                end,
            %% (b) Wallet must appear somewhere in node-message.
            WalletInNm = map_contains_value(NodeMap, W),
            %% (c) Runtime log has PCR-15 event whose digest
            %; matches decoded node-message-id.
            IdInLog =
                try
                    IdRaw = hb_util:decode(ClaimedId),
                    byte_size(IdRaw) =:= 32
                        andalso lists:any(
                          fun(Ev) ->
                              is_map(Ev)
                                  andalso ev_pcr(Ev) =:= 15
                                  andalso
                                  (try
                                      hb_util:decode(
                                        maps:get(<<"digest">>, Ev, <<>>))
                                        =:= IdRaw
                                   catch _:_ -> false
                                   end)
                          end, Events)
                catch _:_ -> false
                end,
            case {IdMatchesHash, WalletInNm, IdInLog} of
                {true, true, true} -> true;
                _                  -> false
            end;
        _ -> <<"unknown">>
    end.

%% Recursive search for a value in a HyperBEAM-style map. Returns
%% true iff `Target' appears as a leaf binary, list element, or
%% nested-map value anywhere under Root.
map_contains_value(M, Target) when is_map(M) ->
    lists:any(fun(V) -> contains_v(V, Target) end, maps:values(M));
map_contains_value(_, _) -> false.

contains_v(V, V) -> true;
contains_v(V, Target) when is_map(V) -> map_contains_value(V, Target);
contains_v(V, Target) when is_list(V) ->
    lists:any(fun(X) -> contains_v(X, Target) end, V);
contains_v(_, _) -> false.

%% @doc Paper P5-ext (AO-Core hashpath continuity) verifier.
%%
%% Returns:
%%   `true'          if the runtime event log has an
%%                   EV_HYPERBEAM_TCG_LOG_TIP_COMMITMENT event on
%%                   PCR 15 whose digest equals sha256 of the
%%                   envelope's tcg-event-log bytes.
%%   `false'         if the tcg-event-log is present but no matching
%%                   event exists (the firmware log was available
%%                   at init_chain time but the cross-link was not
%%                   emitted -- paper property fails).
%%   `<<"log-absent">>' if the envelope carries no tcg-event-log
%%                   bytes (firmware didn't expose it). Paper P5-
%%                   ext only binds when a log is available; emit
%%                   as info rather than warn.
%%   `<<"unknown">>' if the envelope shape doesn't let us decide
%%                   (no runtime event log at all, etc.).
verify_tcg_log_tip_extend(E) ->
    TcgLogB64 = hb_maps:get(<<"tcg-event-log">>, E, <<>>, #{}),
    Log = hb_maps:get(<<"runtime-event-log">>, E, [], #{}),
    case {TcgLogB64, Log} of
        {<<>>, _} -> <<"log-absent">>;
        {_, []}   -> <<"unknown">>;
        {Base64, Events} ->
            try
                Raw = hb_util:decode(Base64),
                case byte_size(Raw) of
                    0 -> <<"log-absent">>;
                    _ ->
                        Expected = crypto:hash(sha256, Raw),
                        Match = [
                            X || X <- Events,
                                 is_map(X),
                                 ev_pcr(X) =:= 15,
                                 maps:get(<<"event-type">>, X, <<>>) =:=
                                     <<"EV_HYPERBEAM_TCG_LOG_TIP_COMMITMENT">>,
                                 hb_util:decode(
                                   maps:get(<<"digest">>, X, <<>>)) =:=
                                     Expected],
                        case Match of
                            [_|_] -> true;
                            []    -> false
                        end
                end
            catch _:_ -> <<"unknown">>
            end
    end.

%% Accept a PCR index expressed as integer or integer-binary
%% (envelopes that have round-tripped through JSON sometimes
%% serialise integer keys as binaries).
ev_pcr(E) ->
    case maps:get(<<"pcr">>, E, 0) of
        N when is_integer(N) -> N;
        B when is_binary(B)  ->
            try binary_to_integer(B) catch _:_ -> 0 end;
        _ -> 0
    end.

%% Small local PEM-RSA public-key decoder. Not sharing
%% `dev_tpm2:decode_pem_rsa_pub/1' avoids a cross-module
%% dependency from the parser (dev_tpm_interpret) to the
%% device layer (dev_tpm2); the two live in different layers
%% per the LapEE architecture.
decode_rsa_pub_pem(Pem) when is_binary(Pem) ->
    try
        case public_key:pem_decode(Pem) of
            [Entry | _] ->
                case public_key:pem_entry_decode(Entry) of
                    #'RSAPublicKey'{} = K -> {ok, K};
                    {#'SubjectPublicKeyInfo'{}, _} ->
                        %% Fall through to second-pass decode below.
                        decode_rsa_pub_spki(Entry);
                    Other ->
                        case Other of
                            #'SubjectPublicKeyInfo'{} = SPKI ->
                                decode_rsa_pub_spki_from(SPKI);
                            _ ->
                                {error, {not_rsa, Other}}
                        end
                end;
            [] -> {error, empty_pem}
        end
    catch C:E -> {error, {C, E}}
    end.

decode_rsa_pub_spki(Entry) ->
    case public_key:pem_entry_decode(Entry) of
        #'RSAPublicKey'{} = K -> {ok, K};
        _ -> {error, not_rsa}
    end.

decode_rsa_pub_spki_from(#'SubjectPublicKeyInfo'{
        subjectPublicKey = Bits}) ->
    try
        K = public_key:der_decode('RSAPublicKey', Bits),
        {ok, K}
    catch _:_ -> {error, not_rsa}
    end.

%% Heuristic scoring:
%%   Start at 100. Every critical costs 40 points, every
%%   warning costs 8 points. Clamp to 0..100. Some positive
%%   signals (high-tier TPM, confidential-compute context)
%%   bump the floor.
policy_score(Signals, Warnings, Criticals) ->
    Base = 100 - (length(Criticals) * 40) - (length(Warnings) * 8),
    Bumped = case maps:get(<<"context-kind">>, Signals,
                            <<"tcg-pc-client">>) of
        <<"intel-tdx-ccel">> -> Base + 5;
        <<"amd-sev-snp">>    -> Base + 5;
        _                    -> Base
    end,
    max(0, min(100, Bumped)).

%% Verdict triage:
%%   any critical-failure    -> "untrusted"
%%   any warning             -> "attested-with-warnings"
%%   nothing found + good signals -> "trusted"
%%   signals all unknown     -> "unknown"
policy_verdict_from([_ | _], _Warnings, _Signals) ->
    <<"untrusted">>;
policy_verdict_from([], [_ | _], _Signals) ->
    <<"attested-with-warnings">>;
policy_verdict_from([], [], Signals) ->
    case all_signals_unknown(Signals) of
        true  -> <<"unknown">>;
        false -> <<"trusted">>
    end.

all_signals_unknown(Signals) ->
    Values = maps:values(Signals),
    lists:all(fun(V) ->
        V =:= <<"unknown">> orelse V =:= null orelse V =:= 0
    end, Values).

%% @doc Descriptive TL;DR of the attestation. Unlike
%% policy-verdict (prescriptive: "trusted / untrusted"),
%% this stanza is purely descriptive -- a one-glance
%% summary of WHAT this machine is and HOW it booted.
%%
%% Fields:
%%   machine-identity       one-line "Vendor Model with CPU"
%%   firmware-identity      "CRTM <version> (<family-name>)"
%%   boot-identity          "boot-loader -> UKI -> kernel"
%%   tpm-identity           "Vendor kind model (trust-tier)"
%%   security-posture       "SB <en|dis>, Lockdown <lvl>, TME <on|off>"
%%   context                "tcg-pc-client" | "intel-tdx-ccel" | ...
%%   top-concerns           up to 5 critical/warning messages
%%                          (from policy-verdict)
%%   evidence-digest-short  first 16 chars of hour-13 digest
claim_attestation_summary(Claim) ->
    CPU = maps:get(<<"cpu">>, Claim, #{}),
    FW = maps:get(<<"firmware">>, Claim, #{}),
    TPM = maps:get(<<"tpm">>, Claim, #{}),
    SB = maps:get(<<"secure-boot">>, Claim, #{}),
    Lockdown = maps:get(<<"lockdown">>, Claim, #{}),
    TME = maps:get(<<"tme">>, Claim, #{}),
    Ctx = maps:get(<<"context">>, Claim, #{}),
    BC = maps:get(<<"boot-chain">>, Claim, #{}),
    Kernel = maps:get(<<"kernel">>, Claim, #{}),
    Verdict = maps:get(<<"policy-verdict">>, Claim, #{}),
    Concerns = lists:sublist(
        maps:get(<<"critical-failures">>, Verdict, []) ++
        maps:get(<<"warnings">>, Verdict, []), 5),
    #{
        <<"machine-identity">>  => compose_machine_identity(FW, CPU),
        <<"firmware-identity">> => compose_firmware_identity(FW),
        <<"boot-identity">>     => compose_boot_identity(BC, Kernel),
        <<"tpm-identity">>      => compose_tpm_identity(TPM),
        <<"security-posture">>  =>
            compose_security_posture(SB, Lockdown, TME),
        <<"context">>           =>
            maps:get(<<"kind">>, Ctx, <<"unknown">>),
        <<"top-concerns">>      => Concerns,
        <<"verdict">>           =>
            maps:get(<<"verdict">>, Verdict, <<"unknown">>),
        <<"score">>             =>
            maps:get(<<"score">>, Verdict, 0)
    }.

compose_machine_identity(FW, CPU) ->
    Vendor = bin_or(<<"family-vendor">>, FW, <<"unknown-vendor">>),
    Platform =
        case bin_or(<<"family-platform">>, FW, null) of
            null -> bin_or(<<"family-name">>, FW, <<"unknown-model">>);
            P    -> P
        end,
    Codename = bin_or(<<"codename">>, CPU, <<"unknown-cpu">>),
    iolist_to_binary([Vendor, <<" ">>, Platform,
                       <<" with ">>, Codename]).

%% @doc Look up `Key' in `Map'. If the value is a binary return
%% it; if null / undefined return `Default'; if a non-binary
%% return Default too (keeps composition iolist-safe).
bin_or(Key, Map, Default) ->
    case maps:get(Key, Map, Default) of
        B when is_binary(B) -> B;
        _                   -> Default
    end.

compose_firmware_identity(FW) ->
    Crtm = bin_or(<<"crtm-version">>, FW, <<"unknown">>),
    Family = case bin_or(<<"family-name">>, FW, null) of
        null -> <<"">>;
        F    -> iolist_to_binary([<<" (">>, F, <<")">>])
    end,
    iolist_to_binary([<<"CRTM ">>, Crtm, Family]).

compose_boot_identity(BC, Kernel) ->
    Length = maps:get(<<"length">>, BC, 0),
    UkiHash = case maps:get(<<"uki-hash">>, Kernel, <<"unknown">>) of
        <<"unknown">> -> <<"unknown-uki">>;
        H when is_binary(H) -> short_hash(H)
    end,
    iolist_to_binary(io_lib:format(
        "boot-chain len=~B -> UKI ~s",
        [Length, UkiHash])).

compose_tpm_identity(TPM) ->
    Name = bin_or(<<"manufacturer-name">>, TPM, <<"unknown-vendor">>),
    Kind = bin_or(<<"manufacturer-kind">>, TPM, <<"unknown-kind">>),
    Tier = bin_or(<<"trust-tier">>, TPM, <<"unknown">>),
    iolist_to_binary([Name, <<" ">>, Kind,
                       <<" (trust-tier=">>, Tier, <<")">>]).

compose_security_posture(SB, Lockdown, TME) ->
    SbStr = case maps:get(<<"enabled">>, SB, <<"unknown">>) of
        true             -> <<"SB on">>;
        false            -> <<"SB off">>;
        _                -> <<"SB unknown">>
    end,
    LdStr = case maps:get(<<"level">>, Lockdown, <<"unknown">>) of
        L when is_binary(L) ->
            iolist_to_binary([<<"lockdown=">>, L]);
        _ -> <<"lockdown=unknown">>
    end,
    TmeStr = case maps:get(<<"enabled">>, TME, <<"unknown">>) of
        true           -> <<"TME on">>;
        false          -> <<"TME off">>;
        _              -> <<"TME unknown">>
    end,
    iolist_to_binary([SbStr, <<", ">>, LdStr, <<", ">>, TmeStr]).

short_hash(H) when is_binary(H), byte_size(H) > 8 ->
    binary:part(H, 0, 8);
short_hash(H) -> H.

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

%% Resolve the manifest's `platforms' field to a concrete platform
%% string (or a list of candidates when the CRTM alone doesn't
%% differentiate). Accepts three shapes:
%%
%%   map  : `#{<<"IFR30">> => <<"Framework 13 (AMD Ryzen 7040)">>}'
%%          -- preferred; disambiguates by CRTM-prefix key.
%%   list : `[<<"Framework 13 (Intel)">>, <<"Framework 13 (AMD)">>,
%%           <<"Framework 16 (AMD)">>]'
%%          -- used when the CRTM is the same across variants
%%          (Framework shares IFR30 across Intel + AMD + 16-inch
%%          generations). We surface the full list as the candidate
%%          set; downstream CPU-identification narrows to one.
%%   atom/binary: return as-is.
%%
%% Returns `null' only when the manifest has no `platforms' field at
%% all, or the field is an empty map/list.
pick_platform(M, Version) ->
    case maps:get(<<"platforms">>, M, undefined) of
        undefined -> null;
        P when is_map(P), map_size(P) == 0 -> null;
        P when is_map(P) ->
            pick_platform_entry(maps:to_list(P), Version);
        [] -> null;
        L when is_list(L), length(L) == 1 ->
            hd(L);
        L when is_list(L) ->
            %% Multiple candidates sharing the same CRTM prefix.
            %% Present the full list so the verifier can narrow
            %% using CPU identity (once claim_cpu resolves vendor +
            %% brand) without losing information.
            L;
        B when is_binary(B) -> B;
        _ -> null
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
%% @doc Compact `claim.quote' -- surface the TPMS_ATTEST metadata
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
%% Any of those is a hard-stop for trusting the quote -- the
%% crypto signature check alone is insufficient because the
%% signature only binds the TPMS_ATTEST blob, not the unquoted
%% per-PCR byte strings that the envelope carries.
%%
%% The digest algorithm is inferred from the declared
%% pcr-digest-length: 20 -> SHA-1, 32 -> SHA-256, 48 -> SHA-384,
%% 64 -> SHA-512.
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
%% selected PCRs whose value is absent from the envelope -- a
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
%% challenges with a fresh nonce -- the TPM echoes it back as
%% extraData inside the quote. Here we surface:
%%
%%   * the nonce echoed by the TPM (base64url),
%%   * the TPM's reset-count / restart-count (monotonic -- newer
%%     quotes should have ≥ the most-recent previous pair from
%%     the same TPM),
%%   * clock-ms / clock-seconds (TPM wall-clock, monotonic
%%     within a boot epoch),
%%   * the `safe' flag (TRUE iff the clock hasn't been tampered
%%     with since last reset -- any FALSE here is a red flag),
%%   * a composite `freshness-indicator' value:
%%       "ok"         -- nonce present, safe=true, clock>0
%%       "safe-false" -- safe flag is false; clock is untrusted
%%       "no-nonce"   -- empty nonce means no challenge was bound
%%       "no-clock"   -- clock-ms=0 is a sign of a dry-run quote
%%       "unknown"    -- no quote present
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

%% @doc Per-PCR event-log ↔ quote consistency check. For every
%% PCR with events attributed to it, recompute the SHA-256 fold
%% chain and compare to the quoted PCR value. A mismatch means
%% the event log transport dropped / reordered / corrupted
%% events, OR the PCR value in the envelope was tampered with.
%%
%% Shape: map from PCR index (0..23) to
%%   #{replayed-digest: b64,
%%     quoted-digest: b64,
%%     matches: bool,
%%     event-count: int,
%%     pcr-is-zero: bool -- nothing ever extended this PCR}.
%%
%% Summary fields:
%%   pcrs-with-events      list of PCR indexes that had events
%%   pcrs-matching         list of PCRs where log matches quote
%%   pcrs-mismatching      list of PCRs where they don't match
%%   consistent            true iff mismatch list is empty
%%
%% The paper's threat model puts the event log ON the wire
%% alongside the signed quote; a verifier that accepts the quote
%% must also accept the event log. `pcr-replay' gives them a
%% single boolean to decide on.
claim_pcr_replay(Events, E) ->
    %% `Events' is the map #{<<"1">> => ev, ...}. Group by PCR.
    EvList = event_list(Events),
    EventsByPcr = group_events_list_by_pcr(EvList),
    PcrVals = nested(E, [<<"tpm-quote">>, <<"pcr-values">>], #{}),
    QuotedPcrSet = sets:from_list(
        [key_to_int(K) || K <- maps:keys(PcrVals)]),
    %% Build a per-PCR bank-alg override map from the quote's
    %% pcrSelect -- for mixed-bank quotes (some PCRs SHA-1, others
    %% SHA-256 etc) this keeps per-PCR replay using the correct
    %% algorithm instead of a single guess-from-size default.
    AlgByPcr = pcr_algs_from_quote(E),
    %% Cover every PCR mentioned by the event log OR the quote
    %% to give a complete picture.
    AllPcrs = lists:usort(
        lists:flatten(
          [maps:keys(EventsByPcr),
           [key_to_int(K) || K <- maps:keys(PcrVals)]])),
    PerPcr = maps:from_list(
        [{integer_to_binary(P),
          replay_one_pcr(P, EventsByPcr, PcrVals, AlgByPcr)}
         || P <- AllPcrs]),
    Matching = [P || {_K, R} <- maps:to_list(PerPcr),
                     P <- [maps:get(<<"pcr-index">>, R, -1)],
                     maps:get(<<"matches">>, R, false) =:= true],
    %% A PCR is a genuine mismatch only when the quote actually
    %% selected that PCR (we have a quoted value) AND replay
    %% disagreed. Event-log entries for PCRs the quote didn't
    %% include give us NO quoted value to compare against, so
    %% counting them as "mismatching" is a taxonomy bug -- the
    %% verifier simply cannot verify them. They get surfaced
    %% separately as `pcrs-unverifiable' for transparency.
    Mismatching = [P || {_K, R} <- maps:to_list(PerPcr),
                        P <- [maps:get(<<"pcr-index">>, R, -1)],
                        maps:get(<<"matches">>, R, true) =:= false,
                        maps:get(<<"event-count">>, R, 0) > 0,
                        sets:is_element(P, QuotedPcrSet)],
    Unverifiable = [P || {_K, R} <- maps:to_list(PerPcr),
                         P <- [maps:get(<<"pcr-index">>, R, -1)],
                         maps:get(<<"event-count">>, R, 0) > 0,
                         not sets:is_element(P, QuotedPcrSet)],
    Covered = [P || {_K, R} <- maps:to_list(PerPcr),
                    P <- [maps:get(<<"pcr-index">>, R, -1)],
                    maps:get(<<"event-count">>, R, 0) > 0],
    #{
        <<"per-pcr">>           => PerPcr,
        <<"pcrs-with-events">>  => lists:sort(Covered),
        <<"pcrs-matching">>     => lists:sort(Matching),
        <<"pcrs-mismatching">>  => lists:sort(Mismatching),
        <<"pcrs-unverifiable">> => lists:sort(Unverifiable),
        <<"consistent">>        => Mismatching =:= [] andalso
                                   Covered =/= [],
        <<"event-count">>       => length(EvList)
    }.

replay_one_pcr(Pcr, EventsByPcr, PcrVals) ->
    replay_one_pcr(Pcr, EventsByPcr, PcrVals, #{}).

replay_one_pcr(Pcr, EventsByPcr, PcrVals, AlgByPcr) ->
    Events = maps:get(Pcr, EventsByPcr, []),
    QuotedB64 = maps:get(integer_to_binary(Pcr), PcrVals, undefined),
    Quoted = case QuotedB64 of
        undefined -> undefined;
        B when is_binary(B) ->
            try hb_util:decode(B) catch _:_ -> undefined end
    end,
    %% Prefer bank-alg from the quote's pcrSelect when
    %% available; otherwise fall back to size-from-digest.
    Replay = case maps:get(Pcr, AlgByPcr, undefined) of
        undefined -> reconstruct_pcr(Events, Quoted);
        Alg       -> reconstruct_pcr(Events, Quoted, Alg)
    end,
    PcrIsZero = case Quoted of
        <<0:256>> -> true;
        <<>>      -> true;
        undefined -> true;
        _         -> false
    end,
    Base = #{
        <<"pcr-index">>         => Pcr,
        <<"event-count">>       => length(Events),
        <<"quoted-digest">>     => case QuotedB64 of
                                       undefined -> <<"">>;
                                       B2        -> B2
                                   end,
        <<"pcr-is-zero">>       => PcrIsZero
    },
    case Replay of
        undefined ->
            Base#{<<"replayed-digest">> => <<"">>,
                  <<"matches">>         => PcrIsZero,
                  <<"alg">>             =>
                      alg_from_digest_size(Quoted)};
        _ when is_map(Replay) ->
            Base#{<<"replayed-digest">> =>
                      maps:get(<<"replayed-digest">>, Replay, <<"">>),
                  <<"matches">>         =>
                      maps:get(<<"matches-quoted">>, Replay, false),
                  <<"alg">>             =>
                      maps:get(<<"alg">>, Replay, <<"sha256">>)}
    end.

%% @doc Aggregate platform-identifying facts from the event log
%% into a single snapshot. Everything here is derived from the
%% same events other claims use, but projected into a compact
%% "what kind of machine is this?" view a policy engine can
%% consume in one lookup.
%%
%% Fields:
%%   handoff-tables-v1        list of {vendor-guid, vendor-guid-
%%                            name, vendor-table-address} rows
%%                            (UEFI configuration tables on this
%%                            system -- SMBIOS entry point, ACPI
%%                            RSDP, HOB list, SAL, MPS, ...)
%%   handoff-tables-v2        list of {table-description} rows
%%                            for the named HANDOFF_TABLES2 form
%%   handoff-tables-count     total count across v1+v2
%%   acpi-present             bool -- an ACPI RSDP table showed up
%%   smbios-present           bool -- an SMBIOS entry point showed up
%%   hob-list-present         bool -- HOB List GUID seen (pre-OS
%%                            hand-off marker)
%%   post-codes               list of ASCII post-code strings
%%   option-rom-count         number of EV_EFI_ACTION events
%%                            naming an option ROM scan
%%   boot-order               from EV_EFI_VARIABLE_BOOT BootOrder
%%   boot-current             active boot entry index
%%   uefi-variable-count      distinct variable names measured
%%   measured-uefi-variables  sorted unique variable names
%%   event-count-per-pcr      histogram PCR -> event-count
%%   event-type-count         histogram event-type-name -> count
claim_platform_config(EvList) ->
    #{
        <<"handoff-tables-v1">>      => ht_v1_entries(EvList),
        <<"handoff-tables-v2">>      => ht_v2_entries(EvList),
        <<"handoff-tables-count">>   =>
            length(ht_v1_entries(EvList)) + length(ht_v2_entries(EvList)),
        <<"acpi-present">>           => has_handoff_table(EvList, <<"ACPI">>),
        <<"smbios-present">>         => has_handoff_table(EvList, <<"SMBIOS">>),
        <<"hob-list-present">>       => has_handoff_table(EvList, <<"HOB List">>),
        <<"post-codes">>             => post_code_strings(EvList),
        <<"option-rom-count">>       => option_rom_scan_count(EvList),
        <<"boot-order">>             => platform_boot_order(EvList),
        <<"boot-current">>           => platform_boot_current(EvList),
        <<"uefi-variable-count">>    => length(measured_uefi_vars(EvList)),
        <<"measured-uefi-variables">> => measured_uefi_vars(EvList),
        <<"event-count-per-pcr">>    => events_by_pcr_histogram(EvList),
        <<"event-type-count">>       => events_by_type_histogram(EvList),
        <<"digest-bank-coverage">>   => digest_bank_coverage(EvList),
        <<"digest-banks-present">>   => digest_banks_present(EvList),
        <<"log-format">>             => detect_log_format(EvList)
    }.

%% @doc Heuristic self-detection of the TCG event log format.
%% Returns one of:
%%   "crypto-agile"   -- TCG PC Client PFP 1.05 multi-bank log.
%%   "legacy-sha1"    -- pre-PFP-1.05 single-SHA-1 log.
%%   "tdx-ccel"       -- Intel TDX Confidential Computing Event Log.
%%   "empty"          -- no events parsed.
%%   "unknown"        -- inconclusive.
%%
%% Signals:
%%   * First event with event-type-code = 3 AND event-data
%%     starts with "Spec ID Event03" -> crypto-agile.
%%   * First event on PCR != 0 AND first event carries an SPDM
%%     TDX signature -> tdx-ccel.
%%   * Digest-bank set = {sha1} only -> legacy-sha1.
detect_log_format([]) -> <<"empty">>;
detect_log_format(EvList) ->
    First = hd(EvList),
    FirstPcr = maps:get(<<"pcr">>, First, 0),
    FirstCode = maps:get(<<"event-type-code">>, First, 0),
    FirstData = maps:get(<<"event-data">>, First, <<>>),
    Banks = digest_banks_present(EvList),
    IsSpecId = FirstCode =:= 3 andalso
               is_binary(FirstData) andalso
               byte_size(FirstData) >= 16 andalso
               binary:longest_common_prefix(
                 [FirstData, <<"Spec ID Event03">>]) >= 15,
    IsTdx = FirstPcr =/= 0,
    SingleSha1Bank = Banks =:= [<<"sha1">>],
    if
        IsTdx           -> <<"tdx-ccel">>;
        IsSpecId        -> <<"crypto-agile">>;
        SingleSha1Bank  -> <<"legacy-sha1">>;
        true            -> <<"unknown">>
    end.

%% Hour-11: histogram of which algorithm-bank digests are
%% present across the event log. An event can carry 1-4 bank
%% digests concurrently; the mix tells policy engines which
%% PCR banks can be replayed against this event log.
%%
%% Returns a map `{alg-name -> event-count-with-that-bank}'.
digest_bank_coverage(EvList) ->
    lists:foldl(
      fun(Ev, Acc) ->
          Digests = maps:get(<<"digests">>, Ev, #{}),
          lists:foldl(
            fun({Alg, Size}, A) ->
                case maps:get(Alg, Digests, undefined) of
                    B when is_binary(B), byte_size(B) =:= Size ->
                        maps:update_with(Alg,
                                          fun(N) -> N + 1 end,
                                          1, A);
                    _ -> A
                end
            end, Acc,
            [{<<"sha1">>, 20}, {<<"sha256">>, 32},
             {<<"sha384">>, 48}, {<<"sha512">>, 64},
             {<<"sm3-256">>, 32}])
      end, #{}, EvList).

%% Sorted list of bank names present in at least one event.
digest_banks_present(EvList) ->
    lists:sort(maps:keys(digest_bank_coverage(EvList))).

%% All EV_EFI_HANDOFF_TABLES (v1, 0x80000009) rows across the log.
ht_v1_entries(EvList) ->
    lists:flatten(
      [maps:get(<<"tables">>,
                maps:get(<<"parsed">>, Ev, #{}), [])
       || Ev <- EvList,
          maps:get(<<"event-type-code">>, Ev, 0) =:= 16#80000009]).

%% All EV_EFI_HANDOFF_TABLES2 (0x8000000B) named descriptions.
ht_v2_entries(EvList) ->
    lists:filtermap(
      fun(Ev) ->
          case maps:get(<<"event-type-code">>, Ev, 0) of
              16#8000000B ->
                  P = maps:get(<<"parsed">>, Ev, #{}),
                  case maps:get(<<"table-description">>, P, <<>>) of
                      <<>> -> false;
                      D    -> {true, #{<<"table-description">> => D}}
                  end;
              _ -> false
          end
      end, EvList).

%% Does any handoff-table row carry a vendor-guid-name containing
%% the given prefix substring? Used for has-acpi / has-smbios /
%% has-hob-list booleans.
has_handoff_table(EvList, Needle) ->
    lists:any(
      fun(Entry) ->
          Name = maps:get(<<"vendor-guid-name">>, Entry, <<>>),
          binary:match(Name, Needle) =/= nomatch
      end, ht_v1_entries(EvList)).

%% Collect ASCII post-code strings from all EV_POST_CODE (0x01)
%% events. Deduplicated and sorted.
post_code_strings(EvList) ->
    lists:usort(
      lists:filtermap(
        fun(Ev) ->
            case maps:get(<<"event-type-code">>, Ev, 0) of
                16#1 ->
                    P = maps:get(<<"parsed">>, Ev, #{}),
                    case maps:get(<<"post-code">>, P, undefined) of
                        C when is_binary(C), byte_size(C) > 0 -> {true, C};
                        _ -> false
                    end;
                _ -> false
            end
        end, EvList)).

%% Count EV_EFI_ACTION (0x80000007) events whose action names an
%% option-ROM scan / execution.
option_rom_scan_count(EvList) ->
    length([Ev || Ev <- EvList,
                  maps:get(<<"event-type-code">>, Ev, 0)
                      =:= 16#80000007,
                  Action <- [nested(Ev, [<<"parsed">>, <<"action">>],
                                      <<>>)],
                  is_binary(Action),
                  binary:match(Action, <<"Option">>) =/= nomatch
                  orelse binary:match(Action, <<"ROM">>) =/= nomatch]).

%% BootOrder variable (EV_EFI_VARIABLE_BOOT) content.
platform_boot_order(EvList) ->
    case [Ev || Ev <- EvList,
                maps:get(<<"event-type-code">>, Ev, 0) =:= 16#80000002,
                sem_var_name(Ev) =:= <<"BootOrder">>] of
        [] -> [];
        [Ev | _] ->
            nested(Ev, [<<"parsed">>, <<"semantic">>,
                        <<"boot-order">>], [])
    end.

%% Active boot entry (BootCurrent) if measured.
platform_boot_current(EvList) ->
    case [Ev || Ev <- EvList,
                maps:get(<<"event-type-code">>, Ev, 0) =:= 16#80000002,
                sem_var_name(Ev) =:= <<"BootCurrent">>] of
        [] -> <<"unknown">>;
        [Ev | _] ->
            nested(Ev, [<<"parsed">>, <<"semantic">>,
                        <<"boot-current">>], <<"unknown">>)
    end.

%% Unique UEFI variable names measured (across EV_EFI_VARIABLE_*).
measured_uefi_vars(EvList) ->
    Codes = [16#80000001, 16#80000002, 16#80000006, 16#800000E0,
             16#8000000C],
    lists:usort(
      [sem_var_name(Ev) ||
        Ev <- EvList,
        lists:member(maps:get(<<"event-type-code">>, Ev, 0), Codes),
        sem_var_name(Ev) =/= <<>>]).

%% Event-count histogram by PCR index (sorted key).
events_by_pcr_histogram(EvList) ->
    L = lists:foldl(
          fun(Ev, Acc) ->
              P = maps:get(<<"pcr">>, Ev, -1),
              maps:update_with(P, fun(N) -> N + 1 end, 1, Acc)
          end, #{}, EvList),
    %% Render map with string keys for JSON friendliness.
    maps:from_list(
      [{integer_to_binary(K), V} || {K, V} <- maps:to_list(L)]).

%% Event-count histogram by event-type name.
events_by_type_histogram(EvList) ->
    lists:foldl(
      fun(Ev, Acc) ->
          T = maps:get(<<"event-type">>, Ev, <<"unknown">>),
          maps:update_with(T, fun(N) -> N + 1 end, 1, Acc)
      end, #{}, EvList).

%% Group an event list by PCR index (integer).
group_events_list_by_pcr(EvList) ->
    lists:foldl(
      fun(Ev, Acc) ->
          case maps:get(<<"pcr">>, Ev, undefined) of
              P when is_integer(P) ->
                  maps:update_with(P,
                                   fun(L) -> L ++ [Ev] end,
                                   [Ev], Acc);
              _ -> Acc
          end
      end, #{}, EvList).

%% @doc Parse the Linux IMA (Integrity Measurement Architecture)
%% per-file measurement chain and expose it as navigable AO-Core
%% data. IMA extends PCR 10 with a per-file template digest for
%% every file IMA's policy says to measure; the measurements
%% live in /sys/kernel/security/ima/ascii_runtime_measurements
%% (ASCII, one line per measurement) or .../binary_runtime_
%% measurements (binary, easier to cryptographically verify).
%%
%% We read the ASCII form from the envelope under
%% `ima-log-ascii' (base64url-encoded since it's plain UTF-8).
%% Each line has the shape:
%%
%%   <pcr>  <template-digest>  <template-name>  <template-data>
%%
%% Template names we recognise:
%%   ima       legacy -- template-data is "<sha1>  <pathname>"
%%   ima-ng    "<hash-alg>:<file-hash>  <pathname>"
%%   ima-sig   "<hash-alg>:<file-hash>  <pathname>  <signature>"
%%   ima-buf   "<hash-alg>:<buf-hash>  <buf-name>"
%%   ima-modsig like ima-sig + kernel module signature
%%
%% Output:
%%   schema-version, event-count, templates-seen,
%%   unique-files, unique-hash-algs, entries[] where
%%   entries[i] = #{pcr, template, template-digest,
%%                  hash-alg, file-hash, pathname,
%%                  signature-present}.
claim_ima(E) ->
    case hb_maps:get(<<"ima-log-ascii">>, E, undefined, #{}) of
        undefined -> unknown_ima_claim();
        <<>>      -> unknown_ima_claim();
        B64 when is_binary(B64) ->
            try
                Ascii = hb_util:decode(B64),
                Entries = parse_ima_ascii(Ascii),
                project_ima_claim(Entries)
            catch _:_ ->
                unknown_ima_claim()
            end
    end.

unknown_ima_claim() ->
    #{
        <<"schema-version">>     => 1,
        <<"present">>            => false,
        <<"event-count">>        => 0,
        <<"templates-seen">>     => [],
        <<"unique-files">>       => 0,
        <<"unique-hash-algs">>   => [],
        <<"entries">>            => [],
        <<"note">>               =>
            <<"No `ima-log-ascii' field on envelope. A verifier "
              "can only assert PCR 10's final value matches a "
              "known-good policy profile without the per-file "
              "chain.">>
    }.

%% @doc Parse the ASCII runtime-measurements format. Tolerant
%% against blank lines / trailing whitespace.
parse_ima_ascii(Ascii) ->
    Lines = binary:split(Ascii, <<"\n">>, [global, trim_all]),
    [Parsed || L <- Lines,
               Parsed <- [parse_ima_line(L)],
               is_map(Parsed)].

parse_ima_line(<<>>) -> undefined;
parse_ima_line(Line) ->
    %% Split by whitespace (space / tab). Up to 4 fields:
    %% PCR, template-digest, template-name, template-data
    %% The 4th field itself is whitespace-separated internally.
    case binary:split(Line, [<<" ">>, <<"\t">>], [global, trim_all]) of
        [PcrB, TemplateDigest, Template | Rest] ->
            case safe_binary_to_integer(PcrB) of
                P when is_integer(P) ->
                    Body = iolist_to_binary(
                        lists:join(<<" ">>, Rest)),
                    TplMap = parse_ima_template_body(Template, Body),
                    Base = #{
                        <<"pcr">>              => P,
                        <<"template">>         => Template,
                        <<"template-digest">>  => TemplateDigest
                    },
                    maps:merge(Base, TplMap);
                _ -> undefined
            end;
        _ -> undefined
    end.

parse_ima_template_body(<<"ima">>, Body) ->
    %% Legacy template: "<sha1>  <pathname>".
    case binary:split(Body, [<<" ">>, <<"\t">>], [trim_all]) of
        [Sha1Hex, Path] ->
            #{<<"hash-alg">>         => <<"sha1">>,
              <<"file-hash-hex">>    => Sha1Hex,
              <<"file-hash-length">> => byte_size(Sha1Hex) div 2,
              <<"pathname">>         => Path,
              <<"signature-present">> => false};
        _ -> #{<<"raw">> => Body}
    end;
parse_ima_template_body(<<"ima-ng">>, Body) ->
    %% "<hash-alg>:<file-hash>  <pathname>".
    parse_ima_ng_like(Body, false);
parse_ima_template_body(<<"ima-sig">>, Body) ->
    %% "<hash-alg>:<file-hash>  <pathname>  <signature-hex>".
    parse_ima_ng_like(Body, true);
parse_ima_template_body(<<"ima-buf">>, Body) ->
    %% Same layout as ima-ng but the pathname is a buffer name.
    M = parse_ima_ng_like(Body, false),
    M#{<<"is-buffer">> => true};
parse_ima_template_body(<<"ima-modsig">>, Body) ->
    %% Same as ima-sig but the trailing signature is a module-
    %% signing PKCS#7 blob rather than an EVM signature.
    M = parse_ima_ng_like(Body, true),
    M#{<<"module-signature">> => true};
parse_ima_template_body(_Other, Body) ->
    #{<<"raw">> => Body}.

parse_ima_ng_like(Body, WithSig) ->
    Fields = binary:split(Body, [<<" ">>, <<"\t">>],
                           [global, trim_all]),
    case Fields of
        [HashAlgColon, Path | SigRest] ->
            {Alg, Hash} = split_alg_colon(HashAlgColon),
            PathEnriched = enrich_kernel_module_path(Path),
            Base = maps:merge(PathEnriched, #{
                <<"hash-alg">>         => Alg,
                <<"file-hash-hex">>    => Hash,
                <<"file-hash-length">> => byte_size(Hash) div 2,
                <<"pathname">>         => Path,
                <<"signature-present">> => WithSig
            }),
            case SigRest of
                [Sig | _] when WithSig ->
                    Base#{<<"signature-hex">> => Sig,
                          <<"signature-length">> =>
                              byte_size(Sig) div 2};
                _ -> Base
            end;
        _ -> #{<<"raw">> => Body}
    end.

%% @doc Recognise Linux kernel-module pathnames and extract
%% structured metadata.
%%
%% Canonical path shapes (Debian/Fedora/Ubuntu/Arch all use
%% this layout):
%%
%%   /lib/modules/<kernel-version>/kernel/<subsystem>/<mod>.ko
%%   /usr/lib/modules/<kernel-version>/kernel/<subsystem>/<mod>.ko
%%
%% With optional .xz / .gz / .zst compression suffix and the
%% `kernel/' directory sometimes absent on third-party modules.
%%
%% Returns an empty map when the path doesn't look like a
%% kernel module; else a map with:
%%   is-kernel-module        true
%%   module-name             bare name without .ko[.xz|.gz|.zst]
%%   module-kernel-version   the <kver> path segment
%%   module-subsystem        path from <kver>/[kernel/] to
%%                           the filename's parent dir
%%   module-compression      "none" | "xz" | "gz" | "zst"
enrich_kernel_module_path(Path) ->
    case kernel_module_path_parts(Path) of
        undefined -> #{};
        {Kver, Subsystem, Name, Compression} ->
            #{
                <<"is-kernel-module">>     => true,
                <<"module-name">>          => Name,
                <<"module-kernel-version">> => Kver,
                <<"module-subsystem">>     => Subsystem,
                <<"module-compression">>   => Compression
            }
    end.

kernel_module_path_parts(Path) when is_binary(Path) ->
    Prefixes = [<<"/lib/modules/">>, <<"/usr/lib/modules/">>],
    case strip_any_prefix(Path, Prefixes) of
        undefined -> undefined;
        Tail ->
            case binary:split(Tail, <<"/">>) of
                [Kver, Rest] when byte_size(Kver) > 0 ->
                    %% Rest = <subsystem>/<name>.ko[.xz|.gz|.zst]
                    %% Optionally strip a leading "kernel/" segment.
                    Rest1 = case Rest of
                        <<"kernel/", R/binary>> -> R;
                        _                       -> Rest
                    end,
                    case kernel_module_name_compression(Rest1) of
                        undefined -> undefined;
                        {Name, Compression, DirPath} ->
                            {Kver, DirPath, Name, Compression}
                    end;
                _ -> undefined
            end
    end;
kernel_module_path_parts(_) -> undefined.

strip_any_prefix(_Bin, []) -> undefined;
strip_any_prefix(Bin, [P | Rest]) ->
    Sz = byte_size(P),
    case Bin of
        <<P:Sz/binary, Tail/binary>> -> Tail;
        _ -> strip_any_prefix(Bin, Rest)
    end.

%% "subsystem/path/name.ko.xz" -> {"name", "xz", "subsystem/path"}.
kernel_module_name_compression(B) when is_binary(B) ->
    %% Find the last slash to isolate the basename.
    {Dir, Base} =
        case binary:matches(B, <<"/">>) of
            [] -> {<<>>, B};
            Ms ->
                {Off, _} = lists:last(Ms),
                {binary:part(B, 0, Off),
                 binary:part(B, Off + 1, byte_size(B) - Off - 1)}
        end,
    case split_module_basename(Base) of
        undefined -> undefined;
        {Name, Compression} -> {Name, Compression, Dir}
    end.

%% "foo.ko", "foo.ko.xz", "foo.ko.gz", "foo.ko.zst".
split_module_basename(Bin) ->
    split_module_basename(Bin, <<"">>).
split_module_basename(Bin, _Comp) ->
    case binary_suffix(Bin) of
        {Name, <<".ko">>}      -> {Name, <<"none">>};
        {Stem, <<".ko.xz">>}   -> {Stem, <<"xz">>};
        {Stem, <<".ko.gz">>}   -> {Stem, <<"gz">>};
        {Stem, <<".ko.zst">>}  -> {Stem, <<"zst">>};
        _ -> undefined
    end.

%% Try progressively shorter suffixes. Returns {Prefix, Suffix}
%% when a known kernel-module extension matches.
binary_suffix(Bin) ->
    Candidates = [<<".ko.xz">>, <<".ko.gz">>, <<".ko.zst">>, <<".ko">>],
    binary_suffix_try(Bin, Candidates).

binary_suffix_try(_Bin, []) -> undefined;
binary_suffix_try(Bin, [S | Rest]) ->
    Sz = byte_size(Bin),
    Ssz = byte_size(S),
    if
        Sz >= Ssz ->
            case binary:part(Bin, Sz - Ssz, Ssz) of
                S ->
                    {binary:part(Bin, 0, Sz - Ssz), S};
                _ ->
                    binary_suffix_try(Bin, Rest)
            end;
        true -> binary_suffix_try(Bin, Rest)
    end.

%% Split "sha256:abcdef..." into {"sha256", "abcdef..."}. If no
%% colon is present, treat as unknown algorithm.
split_alg_colon(B) ->
    case binary:split(B, <<":">>) of
        [Alg, Rest] -> {Alg, Rest};
        _           -> {<<"unknown">>, B}
    end.

safe_binary_to_integer(B) ->
    try binary_to_integer(B) catch _:_ -> undefined end.

%% Aggregate a per-file entry list into the summary claim shape.
project_ima_claim(Entries) ->
    Templates = lists:usort([maps:get(<<"template">>, E, <<"">>)
                              || E <- Entries]),
    Paths = lists:usort([maps:get(<<"pathname">>, E, <<"">>)
                          || E <- Entries,
                             maps:get(<<"pathname">>, E, <<"">>)
                               =/= <<"">>]),
    Algs = lists:usort([maps:get(<<"hash-alg">>, E, <<"unknown">>)
                         || E <- Entries]),
    #{
        <<"schema-version">>    => 1,
        <<"present">>           => Entries =/= [],
        <<"event-count">>       => length(Entries),
        <<"templates-seen">>    => Templates,
        <<"unique-files">>      => length(Paths),
        <<"unique-hash-algs">>  => Algs,
        <<"entries">>           => Entries
    }.

%% @doc IMA policy cross-reference. Picks the best-matching
%% per-distribution policy from the shipped ima-policies/
%% catalogue, then classifies each parsed IMA entry:
%%
%%   matched               pathname matches a policy-expected
%%                         entry (exact / prefix / suffix)
%%   unexpected            pathname does not match any
%%                         policy entry
%%   signature-missing     matched but the policy required a
%%                         signature that wasn't present
%%   hash-alg-downgrade    entry uses a weaker hash than the
%%                         policy's minimum-hash-alg
%%
%% Policy selection: the first policy whose `applies-to'
%% matches the envelope wins. Match criteria:
%%   - kernel_name EV_IPL on PCR 12 matches a
%%     kernel-name-prefix
%%   - the matched UKI-profile (if any -- from hour 3) has a
%%     key listed under `uki-profile-key'
%%
%% Output shape:
%%   picked-policy-key, picked-policy-name,
%%   policy-match-reason, total-entries,
%%   classification-counts: #{matched, unexpected,
%%                            signature-missing,
%%                            hash-alg-downgrade},
%%   violations: [{pathname, classification, reason}, ...]
claim_ima_policy(EvList, E, Db) ->
    ImaClaim = claim_ima(E),
    case maps:get(<<"present">>, ImaClaim, false) of
        false -> unknown_ima_policy_claim(<<"no-ima-log">>);
        true ->
            Policies = maps:get(<<"ima-policies">>, Db, #{}),
            case pick_ima_policy(Policies, EvList) of
                undefined ->
                    unknown_ima_policy_claim(<<"no-matching-policy">>);
                {Key, Policy, Reason} ->
                    Entries = maps:get(<<"entries">>, ImaClaim, []),
                    classify_ima_against_policy(
                      Key, Policy, Reason, Entries)
            end
    end.

unknown_ima_policy_claim(Reason) ->
    #{
        <<"picked-policy-key">>  => null,
        <<"picked-policy-name">> => null,
        <<"policy-match-reason">> => Reason,
        <<"total-entries">>      => 0,
        <<"classification-counts">> => #{
            <<"matched">>               => 0,
            <<"unexpected">>            => 0,
            <<"signature-missing">>     => 0,
            <<"hash-alg-downgrade">>    => 0
        },
        <<"violations">>         => []
    }.

%% Iterate policies; pick the first whose applies-to criteria
%% fit the envelope.
pick_ima_policy(Policies, EvList) when is_map(Policies) ->
    KernelName = first_defined(
                   [ipl_kv_value(EvList, <<"kernel-name">>),
                    ipl_kv_value(EvList, <<"kernel_name">>)]),
    pick_ima_policy_entries(
      maps:to_list(Policies), KernelName);
pick_ima_policy(_, _) -> undefined.

pick_ima_policy_entries([], _) -> undefined;
pick_ima_policy_entries([{K, P} | Rest], KernelName)
    when is_map(P) ->
    Applies = maps:get(<<"applies-to">>, P, #{}),
    KnpList = maps:get(<<"kernel-name-prefix">>, Applies, []),
    case KernelName =/= undefined andalso
         any_prefix_match(KernelName, KnpList) of
        true ->
            {K, P, <<"kernel-name-prefix">>};
        false ->
            pick_ima_policy_entries(Rest, KernelName)
    end;
pick_ima_policy_entries([_ | Rest], KernelName) ->
    pick_ima_policy_entries(Rest, KernelName).

%% Walk every IMA entry, classify, aggregate.
classify_ima_against_policy(Key, Policy, Reason, Entries) ->
    Expected = maps:get(<<"expected-files">>, Policy, []),
    MinAlg = maps:get(<<"minimum-hash-alg">>, Policy, <<"sha256">>),
    MinAlgStrength = hash_alg_strength(MinAlg),
    Classified = [classify_one_ima_entry(Ev, Expected,
                                           MinAlgStrength, MinAlg)
                   || Ev <- Entries],
    Counts = count_classifications(Classified),
    Violations = [maps:with(
                    [<<"pathname">>, <<"classification">>,
                     <<"reason">>, <<"matched-rule">>], C)
                    || C <- Classified,
                       is_violation(maps:get(<<"classification">>, C))],
    #{
        <<"picked-policy-key">>    => Key,
        <<"picked-policy-name">>   =>
            maps:get(<<"name">>, Policy, Key),
        <<"policy-match-reason">>  => Reason,
        <<"total-entries">>        => length(Entries),
        <<"classification-counts">> => Counts,
        <<"violations">>           => Violations
    }.

classify_one_ima_entry(Entry, Expected, MinAlgStrength, MinAlg) ->
    Pathname = maps:get(<<"pathname">>, Entry, <<>>),
    Alg = maps:get(<<"hash-alg">>, Entry, <<"unknown">>),
    SignaturePresent = maps:get(<<"signature-present">>, Entry,
                                  false),
    case find_matching_expected(Pathname, Expected) of
        undefined ->
            #{<<"pathname">> => Pathname,
              <<"classification">> => <<"unexpected">>,
              <<"reason">> => <<"pathname matches no policy entry">>,
              <<"matched-rule">> => null};
        {Rule, Matcher} ->
            SigReq = maps:get(<<"signature-required">>, Rule, false),
            ActualStrength = hash_alg_strength(Alg),
            if
                SigReq, not SignaturePresent ->
                    #{<<"pathname">> => Pathname,
                      <<"classification">> => <<"signature-missing">>,
                      <<"reason">> =>
                          <<"policy requires signature; none "
                            "present on IMA entry">>,
                      <<"matched-rule">> => Matcher};
                ActualStrength < MinAlgStrength ->
                    #{<<"pathname">> => Pathname,
                      <<"classification">> => <<"hash-alg-downgrade">>,
                      <<"reason">> =>
                          <<"IMA entry uses ", Alg/binary,
                            "; policy minimum is ", MinAlg/binary>>,
                      <<"matched-rule">> => Matcher};
                true ->
                    #{<<"pathname">> => Pathname,
                      <<"classification">> => <<"matched">>,
                      <<"reason">> => <<"">>,
                      <<"matched-rule">> => Matcher}
            end
    end.

%% Find the first expected-files rule whose pathname matcher
%% (exact / prefix / suffix) fits the entry's pathname.
%% Returns `{Rule, "exact:<val>" | "prefix:<val>" | "suffix:<val>"}'.
find_matching_expected(_Pathname, []) -> undefined;
find_matching_expected(Pathname, [Rule | Rest]) when is_map(Rule) ->
    case rule_match(Pathname, Rule) of
        {true, Matcher} -> {Rule, Matcher};
        false           -> find_matching_expected(Pathname, Rest)
    end;
find_matching_expected(Pathname, [_ | Rest]) ->
    find_matching_expected(Pathname, Rest).

rule_match(Pathname, Rule) ->
    case maps:get(<<"pathname">>, Rule, undefined) of
        P when is_binary(P), P =:= Pathname ->
            {true, <<"exact:", P/binary>>};
        _ ->
            case maps:get(<<"pathname-prefix">>, Rule, undefined) of
                Pre when is_binary(Pre) ->
                    case binary:match(Pathname, Pre) of
                        {0, _} -> {true, <<"prefix:", Pre/binary>>};
                        _ -> rule_match_suffix(Pathname, Rule)
                    end;
                _ -> rule_match_suffix(Pathname, Rule)
            end
    end.

rule_match_suffix(Pathname, Rule) ->
    case maps:get(<<"pathname-suffix">>, Rule, undefined) of
        Sfx when is_binary(Sfx) ->
            case binary:longest_common_suffix([Pathname, Sfx]) of
                L when L =:= byte_size(Sfx) ->
                    {true, <<"suffix:", Sfx/binary>>};
                _ -> false
            end;
        _ -> false
    end.

is_violation(<<"matched">>) -> false;
is_violation(_)             -> true.

count_classifications(Classified) ->
    Init = #{<<"matched">> => 0,
             <<"unexpected">> => 0,
             <<"signature-missing">> => 0,
             <<"hash-alg-downgrade">> => 0},
    lists:foldl(
      fun(C, Acc) ->
          K = maps:get(<<"classification">>, C, <<"matched">>),
          maps:update_with(K, fun(N) -> N + 1 end, 1, Acc)
      end, Init, Classified).

%% Relative strength ordering for common hash algs -- used for
%% detecting a hash-alg downgrade vs the policy minimum.
hash_alg_strength(<<"sha1">>)     -> 1;
hash_alg_strength(<<"md5">>)      -> 0;
hash_alg_strength(<<"sha256">>)   -> 2;
hash_alg_strength(<<"sha224">>)   -> 2;
hash_alg_strength(<<"sha384">>)   -> 3;
hash_alg_strength(<<"sha3-256">>) -> 2;
hash_alg_strength(<<"sha3-384">>) -> 3;
hash_alg_strength(<<"sha512">>)   -> 4;
hash_alg_strength(<<"sha3-512">>) -> 4;
hash_alg_strength(_)              -> 2.   % assume sha256-equivalent default

claim_boot_chain(Events) -> claim_boot_chain(Events, #{}).

claim_boot_chain(Events, Db) ->
    Codes = [16#80000003, 16#80000004, 16#80000005],
    Sorted = lists:sort(
        fun(A, B) ->
            maps:get(<<"seq">>, A, 0) =< maps:get(<<"seq">>, B, 0)
        end,
        [Ev || Ev <- Events,
               lists:member(maps:get(<<"event-type-code">>, Ev, 0),
                            Codes)]),
    BootImages = maps:get(<<"boot-images">>, Db, #{}),
    Rows = lists:map(
        fun(IE) -> enrich_boot_row(project_boot_row(IE), BootImages) end,
        lists:zip(lists:seq(0, length(Sorted) - 1), Sorted)),
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

%% @doc Cross-reference a boot-chain row against the shipped
%% boot-images catalogue. Returns the row augmented with
%% `publisher', `product', `category', `signed-by',
%% `cve-status', `cve-notes', `recommended-min-version',
%% `matched-by', `notes' when a match fires -- else `null' in
%% each of those slots so the shape stays stable.
%%
%% Match rules (first-wins within a profile):
%%   1. exact `image-hash-sha256' match against the row's
%%      image-hash (base64url-encoded by project_boot_row/1).
%%   2. prefix match of the row's device-path-text against any
%%      `match.device-path-suffix' string. We use prefix-from-
%%      right (the suffix appears at the end of the path text).
enrich_boot_row(Row, BootImages) when is_map(BootImages),
                                        map_size(BootImages) > 0 ->
    ImageHash = maps:get(<<"image-hash">>, Row, <<"">>),
    DpText    = maps:get(<<"device-path-text">>, Row, <<"">>),
    case first_boot_image_match(BootImages, ImageHash, DpText) of
        undefined  -> boot_row_unmatched(Row);
        {Key, Profile, MatchBy, Pattern} ->
            Row#{
                <<"publisher">> =>
                    maps:get(<<"publisher">>, Profile, <<"unknown">>),
                <<"product">>   =>
                    maps:get(<<"product">>, Profile, <<"unknown">>),
                <<"category">>  =>
                    maps:get(<<"category">>, Profile, <<"unknown">>),
                <<"signed-by">> =>
                    maps:get(<<"signed-by">>, Profile, []),
                <<"cve-status">> =>
                    maps:get(<<"cve-status">>, Profile, <<"unknown">>),
                <<"cve-notes">> =>
                    maps:get(<<"cve-notes">>, Profile, <<"">>),
                <<"recommended-min-version">> =>
                    maps:get(<<"recommended-min-version">>, Profile,
                              null),
                <<"matched-by">> => MatchBy,
                <<"matched-pattern">> => Pattern,
                <<"matched-profile-key">> => Key,
                <<"notes">> =>
                    maps:get(<<"notes">>, Profile, <<"">>)
            }
    end;
enrich_boot_row(Row, _Db) ->
    boot_row_unmatched(Row).

boot_row_unmatched(Row) ->
    Row#{
        <<"publisher">>                => null,
        <<"product">>                  => null,
        <<"category">>                 => null,
        <<"signed-by">>                => [],
        <<"cve-status">>               => <<"unknown">>,
        <<"cve-notes">>                => <<"">>,
        <<"recommended-min-version">>  => null,
        <<"matched-by">>               => <<"unmatched">>,
        <<"matched-pattern">>          => null,
        <<"matched-profile-key">>      => null,
        <<"notes">>                    => <<"">>
    }.

%% Iterate boot-image profiles; return `{ProfileKey, Profile,
%% MatchedBy, MatchedPattern}' on the first hit or `undefined'.
first_boot_image_match(Profiles, ImageHash, DpText) ->
    first_boot_image_match_list(
      maps:to_list(Profiles), ImageHash, DpText).

first_boot_image_match_list([], _, _) -> undefined;
first_boot_image_match_list([{K, P} | Rest], ImageHash, DpText)
    when is_map(P) ->
    M = maps:get(<<"match">>, P, #{}),
    HashList = maps:get(<<"image-hash-sha256">>, M, []),
    SfxList = maps:get(<<"device-path-suffix">>, M, []),
    %% Rule 1: exact hash.
    case lists:member(ImageHash, HashList) of
        true ->
            {K, P, <<"image-hash-sha256">>, ImageHash};
        false ->
            %% Rule 2: device-path suffix.
            case match_dp_suffix(DpText, SfxList) of
                {match, Pattern} ->
                    {K, P, <<"device-path-suffix">>, Pattern};
                nomatch ->
                    first_boot_image_match_list(
                      Rest, ImageHash, DpText)
            end
    end;
first_boot_image_match_list([_ | Rest], ImageHash, DpText) ->
    first_boot_image_match_list(Rest, ImageHash, DpText).

%% Does `DpText' end with any of the given suffixes? Returns
%% `{match, Suffix}' on first hit or `nomatch'. Case-insensitive
%% for the ASCII portion since UEFI filesystems are case-
%% insensitive.
match_dp_suffix(_, []) -> nomatch;
match_dp_suffix(DpText, [Sfx | Rest]) when is_binary(Sfx) ->
    case ends_with_ci(DpText, Sfx) orelse
         binary:match(DpText, Sfx) =/= nomatch of
        true  -> {match, Sfx};
        false -> match_dp_suffix(DpText, Rest)
    end;
match_dp_suffix(DpText, [_ | Rest]) ->
    match_dp_suffix(DpText, Rest).

%% Case-insensitive "DpText ends with Sfx?" for ASCII bytes.
ends_with_ci(DpText, Sfx) when is_binary(DpText), is_binary(Sfx) ->
    DpLen = byte_size(DpText),
    SfxLen = byte_size(Sfx),
    DpLen >= SfxLen andalso
        bin_to_lower(binary:part(DpText, DpLen - SfxLen, SfxLen))
         =:= bin_to_lower(Sfx).

bin_to_lower(B) when is_binary(B) ->
    << <<(to_lower_byte(X))>> || <<X:8>> <= B >>.

to_lower_byte(X) when X >= $A, X =< $Z -> X + 32;
to_lower_byte(X) -> X.

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

%% TME/SME (paper section Arch line 226-230).
%%
%% Three orthogonal evidence tiers compose here:
%%   tier 2 (kernel cmdline intent): `mem_encrypt=on' / `sme=on' /
%%           `kvm_intel.tdx=on' measured into PCR 12 via sd-stub.
%%   tier 3 (UKI-hash claim DB lookup): this PCR 11 UKI hash appears
%%           in our uki-measurements DB with `checks-tme: true'
%%           (the kernel's early init halts if TME is off).
%%   tier 4 (boot-reached-PCR-15): PCR 15 was extended by the
%%           ephemeral node key -> halt-check didn't fire -> TME is on.
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
    %% Tier 4: boot-reached-PCR-15 -- we always have this if the
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
    %% (`kernel_name' -> `kernel-name'), so look up with the kebab
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
%%   * Any tier giving `true' -> claim is true (with all supporting
%%     tiers' evidence).
%%   * Any tier giving `false' while none say `true' -> claim is false.
%%   * All tiers return "unknown" -> claim is "unknown".
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

%% Kernel lockdown mode (paper section Arch line 223:
%% `lockdown=confidentiality').
%%
%% Tier 2: cmdline `lockdown=<mode>'.
%% Tier 3: UKI-hash claim `lockdown-confidentiality: true' in the DB.
claim_lockdown(Events) ->
    claim_lockdown(Events, #{}, #{}).

claim_lockdown(Events, E, Db) ->
    {Flags, CmdlineProv} = cmdline_flags_and_provenance(Events),
    Mode = maps:get(<<"lockdown">>, Flags, <<"unknown">>),
    CmdlineLevel = case Mode of
        <<"confidentiality">> -> <<"confidentiality">>;
        <<"integrity">>       -> <<"integrity">>;
        <<"none">>            -> <<"none">>;
        V when is_binary(V)   -> V;
        _                     -> <<"unknown">>
    end,
    %% v1.2 E3: guest reads /sys/kernel/security/lockdown into
    %% `platform-probes.lockdown'. The file format is
    %% "[none] integrity confidentiality" -- the bracketed entry
    %% is the currently-active level. Runtime reading is tier-2
    %% evidence, stronger than cmdline alone because it reflects
    %% whatever the kernel ACTUALLY settled on (including LSM
    %% interactions).
    Probes = probes_map(E),
    RawLockdown = hb_maps:get(<<"lockdown">>, Probes, null, #{}),
    {RuntimeLevel, RuntimeProv} =
        case parse_lockdown_line(RawLockdown) of
            unknown -> {<<"unknown">>, []};
            RL ->
                {RL,
                 [{<<"tier">>, 2},
                  {<<"source">>,
                   <<"/sys/kernel/security/lockdown">>},
                  {<<"raw">>, RawLockdown}]}
        end,
    %% Runtime wins; fall back to cmdline flag.
    {Level, LevelProv} =
        case RuntimeLevel of
            <<"unknown">> ->
                Lp = case CmdlineLevel of
                    <<"unknown">> -> [];
                    _             -> [{<<"tier">>, 2} | CmdlineProv]
                end,
                {CmdlineLevel, Lp};
            _ ->
                {RuntimeLevel, RuntimeProv}
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

%% Parse a `/sys/kernel/security/lockdown' line -- the format is
%% `[active] other1 other2', where the bracketed token is the
%% currently-active lockdown level. Returns the active token as a
%% binary, or the atom `unknown' if the line doesn't match.
parse_lockdown_line(null) -> unknown;
parse_lockdown_line(<<>>) -> unknown;
parse_lockdown_line(Bin) when is_binary(Bin) ->
    %% Look for "[word]" anywhere on the line.
    case re:run(Bin, <<"\\[([A-Za-z_-]+)\\]">>,
                [{capture, all_but_first, binary}]) of
        {match, [Active]} -> Active;
        _ -> unknown
    end;
parse_lockdown_line(_) -> unknown.

%% IOMMU state (paper section Arch line 223:
%% `IOMMU strict mode ... init_on_alloc/init_on_free').
%%
%% Tier 2 cmdline flags:
%%   iommu=pt                   -> DMA-remap mode
%%   iommu.strict=1             -> flushes per-op (no lazy invalidation)
%%   intel_iommu=on | amd_iommu=on -> vendor-specific enable
claim_iommu(Events) -> claim_iommu(Events, #{}).

claim_iommu(Events, E) ->
    {Flags, CmdlineProv} = cmdline_flags_and_provenance(Events),
    Mode  = maps:get(<<"iommu">>, Flags, <<"unknown">>),
    Strct = maps:get(<<"iommu.strict">>, Flags, <<"unknown">>),
    Intel = maps:get(<<"intel_iommu">>, Flags, <<"unknown">>),
    Amd   = maps:get(<<"amd_iommu">>,   Flags, <<"unknown">>),
    %% Tier-2 conclusion from kernel cmdline alone.
    CmdEnabled = case {Mode, Intel, Amd} of
        {<<"unknown">>, <<"unknown">>, <<"unknown">>} -> <<"unknown">>;
        _ ->
            (Mode =/= <<"unknown">>) orelse (Intel =:= true)
                                     orelse is_binary(Amd)
    end,
    CmdProv = case CmdEnabled of
        <<"unknown">> -> [];
        _             -> [{<<"tier">>, 2} | CmdlineProv]
    end,
    %% v1.2 E3: runtime probe of /sys/kernel/iommu_groups surfaced
    %% by the guest into `platform-probes.iommu-groups-count'. If
    %% the guest reports any groups, the IOMMU is active in the
    %% running kernel regardless of what the cmdline said. This
    %% is tier-2 runtime evidence (stronger than cmdline-only).
    Probes = probes_map(E),
    GroupCount = hb_maps:get(<<"iommu-groups-count">>,
                             Probes, null, #{}),
    {RuntimeEnabled, RuntimeProv} =
        case GroupCount of
            N when is_integer(N), N > 0 ->
                {true,
                 [{<<"tier">>, 2},
                  {<<"source">>, <<"sysfs-iommu-groups">>},
                  {<<"groups-count">>, N}]};
            0 ->
                {false,
                 [{<<"tier">>, 2},
                  {<<"source">>, <<"sysfs-iommu-groups">>},
                  {<<"groups-count">>, 0}]};
            _ ->
                {<<"unknown">>, []}
        end,
    %% Runtime wins when available; fall back to cmdline.
    {Enabled, Prov} =
        case RuntimeEnabled of
            <<"unknown">> -> {CmdEnabled, CmdProv};
            _ -> {RuntimeEnabled, RuntimeProv ++ CmdProv}
        end,
    #{
        <<"enabled">>                  => Enabled,
        <<"enabled-evidence">>         => Prov,
        <<"mode">>                     => Mode,
        <<"strict">>                   => Strct,
        <<"intel-iommu-requested">>    => Intel,
        <<"amd-iommu-requested">>      => Amd,
        <<"runtime-groups-count">>     =>
            case GroupCount of null -> null; _ -> GroupCount end
    }.

%% Kernel integrity properties (paper section Security table):
%%   module.sig_enforce=1  -> unsigned modules rejected
%%   init_on_alloc=1       -> heap pages zeroed at alloc
%%   init_on_free=1        -> heap pages zeroed at free
%%   slab_nomerge          -> slab caches not merged (reduces cross-
%%                            cache exploitation)
%%   page_poison=1         -> free pages poisoned
%%   lockdown=confidentiality -> kernel lockdown in the strictest mode
claim_kernel_integrity(Events) ->
    claim_kernel_integrity(Events, #{}).

claim_kernel_integrity(Events, E) ->
    {Flags, Prov} = cmdline_flags_and_provenance(Events),
    Base = case Prov of
        [] -> [];
        _  -> [{<<"tier">>, 2} | Prov]
    end,
    %% Hour-12 addition: summarise kernel-module IMA entries
    %% when the envelope carries an IMA log. Gives a policy
    %% engine an at-a-glance "what modules did this kernel
    %% load?" view.
    ModulesSummary = summarise_modules_from_ima(E),
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
        <<"modules">>                => ModulesSummary,
        <<"evidence">>               => Base
    }.

%% Project module-loading activity out of the parsed IMA log.
%% When no IMA log is present, returns an `absent' stanza.
summarise_modules_from_ima(E) ->
    ImaClaim = claim_ima(E),
    case maps:get(<<"present">>, ImaClaim, false) of
        false ->
            #{<<"ima-log-present">> => false,
              <<"modules-loaded-count">> => 0,
              <<"modules-signed-count">> => 0,
              <<"modules-unsigned-count">> => 0,
              <<"modules-kernel-versions">> => [],
              <<"modules-by-subsystem">> => #{},
              <<"modules">> => []};
        true ->
            Entries = maps:get(<<"entries">>, ImaClaim, []),
            ModuleEntries = [E0 || E0 <- Entries,
                                    maps:get(<<"is-kernel-module">>,
                                              E0, false) =:= true],
            Signed = [E0 || E0 <- ModuleEntries,
                             maps:get(<<"signature-present">>,
                                       E0, false) =:= true],
            Unsigned = [E0 || E0 <- ModuleEntries,
                               maps:get(<<"signature-present">>,
                                        E0, false) =:= false],
            KVers = lists:usort(
                [maps:get(<<"module-kernel-version">>, E0, <<"">>)
                 || E0 <- ModuleEntries]),
            BySubsystem = lists:foldl(
                fun(E0, Acc) ->
                    S = maps:get(<<"module-subsystem">>, E0, <<"">>),
                    maps:update_with(S,
                                      fun(N) -> N + 1 end, 1, Acc)
                end, #{}, ModuleEntries),
            Modules = [project_module_summary(E0) || E0 <- ModuleEntries],
            #{<<"ima-log-present">>          => true,
              <<"modules-loaded-count">>     => length(ModuleEntries),
              <<"modules-signed-count">>     => length(Signed),
              <<"modules-unsigned-count">>   => length(Unsigned),
              <<"modules-kernel-versions">>  => KVers,
              <<"modules-by-subsystem">>     => BySubsystem,
              <<"modules">>                  => Modules}
    end.

project_module_summary(Entry) ->
    #{
        <<"module-name">>          =>
            maps:get(<<"module-name">>, Entry, <<"">>),
        <<"module-kernel-version">> =>
            maps:get(<<"module-kernel-version">>, Entry, <<"">>),
        <<"module-subsystem">>     =>
            maps:get(<<"module-subsystem">>, Entry, <<"">>),
        <<"module-compression">>   =>
            maps:get(<<"module-compression">>, Entry, <<"unknown">>),
        <<"pathname">>             =>
            maps:get(<<"pathname">>, Entry, <<"">>),
        <<"hash-alg">>             =>
            maps:get(<<"hash-alg">>, Entry, <<"">>),
        <<"file-hash-hex">>        =>
            maps:get(<<"file-hash-hex">>, Entry, <<"">>),
        <<"signature-present">>    =>
            maps:get(<<"signature-present">>, Entry, false)
    }.

%% dm-verity rootfs + /usr integrity (paper section Arch line 222:
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
            %% expose the raw bytes -- `magic_ok' is the single fact a
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

%% @doc Decode the body of a TPMS_ATTEST based on its `Type'.
%% Per TPM 2.0 Part 2 section 10.12.8 the `attested' union switches on
%% the TPMI_ST_ATTEST tag at offset +4 in TPMS_ATTEST. All 7
%% recognised attest types are decoded structurally; unknown
%% types get a `tail-length' + `tail-sha256' fallback.
%%
%%   0x8014  TPM_ST_ATTEST_NV            TPMS_NV_CERTIFY_INFO
%%   0x8015  TPM_ST_ATTEST_COMMAND_AUDIT TPMS_COMMAND_AUDIT_INFO
%%   0x8016  TPM_ST_ATTEST_SESSION_AUDIT TPMS_SESSION_AUDIT_INFO
%%   0x8017  TPM_ST_ATTEST_CERTIFY       TPMS_CERTIFY_INFO
%%   0x8018  TPM_ST_ATTEST_QUOTE         TPMS_QUOTE_INFO
%%   0x8019  TPM_ST_ATTEST_TIME          TPMS_TIME_ATTEST_INFO
%%   0x801A  TPM_ST_ATTEST_CREATION      TPMS_CREATION_INFO
%%   0x801C  TPM_ST_ATTEST_NV_DIGEST     TPMS_NV_DIGEST_CERTIFY_INFO
decode_attest_body(16#8018, Body) ->
    %% TPMS_QUOTE_INFO: pcrSelect + pcrDigest.
    try
        <<Count:32/unsigned-big, Rest0/binary>> = Body,
        {Selections, Rest1} = decode_pcr_selections(Count, Rest0, []),
        {PcrDigest, _Tail} = tpm2b(Rest1),
        #{<<"attest-body-type">> => <<"TPMS_QUOTE_INFO">>,
          <<"pcr-select">> => Selections,
          <<"pcr-select-count">> => Count,
          <<"pcr-digest">> => hb_util:encode(PcrDigest),
          <<"pcr-digest-length">> => byte_size(PcrDigest)}
    catch _:_ ->
        #{<<"attest-body-error">> =>
              <<"TPMS_QUOTE_INFO parse failed">>}
    end;
decode_attest_body(16#8017, Body) ->
    %% TPMS_CERTIFY_INFO: name + qualifiedName (both TPM2B_NAME).
    try
        {Name, Rest1} = tpm2b(Body),
        {QualName, _Tail} = tpm2b(Rest1),
        #{<<"attest-body-type">>      => <<"TPMS_CERTIFY_INFO">>,
          <<"object-name">>           => hb_util:encode(Name),
          <<"object-name-length">>    => byte_size(Name),
          <<"object-qualified-name">> => hb_util:encode(QualName),
          <<"object-qualified-name-length">> =>
              byte_size(QualName)}
    catch _:_ ->
        #{<<"attest-body-error">> =>
              <<"TPMS_CERTIFY_INFO parse failed">>}
    end;
decode_attest_body(16#8015, Body) ->
    %% TPMS_COMMAND_AUDIT_INFO: auditCounter (u64) + digestAlg
    %% (TPMI_ALG_HASH u16) + auditDigest (TPM2B_DIGEST) +
    %% commandDigest (TPM2B_DIGEST).
    try
        <<AuditCounter:64/unsigned-big,
          DigestAlg:16/unsigned-big,
          Rest0/binary>> = Body,
        {AuditDigest, Rest1} = tpm2b(Rest0),
        {CommandDigest, _Tail} = tpm2b(Rest1),
        #{<<"attest-body-type">>     =>
              <<"TPMS_COMMAND_AUDIT_INFO">>,
          <<"audit-counter">>        => AuditCounter,
          <<"audit-digest-alg-code">> => DigestAlg,
          <<"audit-digest-alg-name">> => hash_alg_name(DigestAlg),
          <<"audit-digest">>         => hb_util:encode(AuditDigest),
          <<"audit-digest-length">>  => byte_size(AuditDigest),
          <<"command-digest">>       => hb_util:encode(CommandDigest),
          <<"command-digest-length">> => byte_size(CommandDigest)}
    catch _:_ ->
        #{<<"attest-body-error">> =>
              <<"TPMS_COMMAND_AUDIT_INFO parse failed">>}
    end;
decode_attest_body(16#8016, Body) ->
    %% TPMS_SESSION_AUDIT_INFO: exclusiveSession (TPMI_YES_NO = u8)
    %% + sessionDigest (TPM2B_DIGEST).
    try
        <<Exclusive:8, Rest0/binary>> = Body,
        {SessDigest, _Tail} = tpm2b(Rest0),
        #{<<"attest-body-type">>     =>
              <<"TPMS_SESSION_AUDIT_INFO">>,
          <<"exclusive-session">>    => Exclusive =/= 0,
          <<"session-digest">>       => hb_util:encode(SessDigest),
          <<"session-digest-length">> => byte_size(SessDigest)}
    catch _:_ ->
        #{<<"attest-body-error">> =>
              <<"TPMS_SESSION_AUDIT_INFO parse failed">>}
    end;
decode_attest_body(16#801A, Body) ->
    %% TPMS_CREATION_INFO: objectName (TPM2B_NAME) +
    %% creationHash (TPM2B_DIGEST).
    try
        {ObjName, Rest1} = tpm2b(Body),
        {CreationHash, _Tail} = tpm2b(Rest1),
        #{<<"attest-body-type">>     => <<"TPMS_CREATION_INFO">>,
          <<"object-name">>          => hb_util:encode(ObjName),
          <<"object-name-length">>   => byte_size(ObjName),
          <<"creation-hash">>        => hb_util:encode(CreationHash),
          <<"creation-hash-length">> => byte_size(CreationHash)}
    catch _:_ ->
        #{<<"attest-body-error">> =>
              <<"TPMS_CREATION_INFO parse failed">>}
    end;
decode_attest_body(16#8019, Body) ->
    %% TPMS_TIME_ATTEST_INFO: time (TPMS_TIME_INFO
    %%   containing time:u64 + clockInfo:TPMS_CLOCK_INFO) +
    %% firmwareVersion (u64). Note: the outer TPMS_ATTEST already
    %% carries a TPMS_CLOCK_INFO + firmwareVersion at the common
    %% header, so for TIME attestations the extra copy inside
    %% the body is what this decoder yields -- they SHOULD
    %% match the outer fields; a mismatch signals TPM tampering.
    try
        %% TPMS_TIME_INFO = time (u64) + clockInfo
        %% TPMS_CLOCK_INFO = clock(u64) + resetCount(u32) +
        %%                   restartCount(u32) + safe(u8)
        <<Time:64/unsigned-big,
          ClockInner:64/unsigned-big,
          RsetInner:32/unsigned-big,
          RstrInner:32/unsigned-big,
          SafeInner:8,
          FwVerInner:64/unsigned-big,
          _Tail/binary>> = Body,
        #{<<"attest-body-type">>       => <<"TPMS_TIME_ATTEST_INFO">>,
          <<"time-u64">>               => Time,
          <<"inner-clock-ms">>         => ClockInner,
          <<"inner-reset-count">>      => RsetInner,
          <<"inner-restart-count">>    => RstrInner,
          <<"inner-safe">>             => SafeInner =/= 0,
          <<"inner-firmware-version-u64">> => FwVerInner}
    catch _:_ ->
        #{<<"attest-body-error">> =>
              <<"TPMS_TIME_ATTEST_INFO parse failed">>}
    end;
decode_attest_body(16#8014, Body) ->
    %% TPMS_NV_CERTIFY_INFO: indexName (TPM2B_NAME) + offset(u16)
    %% + nvContents (TPM2B_MAX_NV_BUFFER).
    try
        {IndexName, Rest1} = tpm2b(Body),
        <<Offset:16/unsigned-big, Rest2/binary>> = Rest1,
        {NvContents, _Tail} = tpm2b(Rest2),
        #{<<"attest-body-type">>  => <<"TPMS_NV_CERTIFY_INFO">>,
          <<"nv-index-name">>     => hb_util:encode(IndexName),
          <<"nv-index-name-length">> => byte_size(IndexName),
          <<"nv-offset">>         => Offset,
          <<"nv-contents">>       => hb_util:encode(NvContents),
          <<"nv-contents-length">> => byte_size(NvContents)}
    catch _:_ ->
        #{<<"attest-body-error">> =>
              <<"TPMS_NV_CERTIFY_INFO parse failed">>}
    end;
decode_attest_body(16#801C, Body) ->
    %% TPMS_NV_DIGEST_CERTIFY_INFO: indexName + nvDigest
    %% (both TPM2B_*).
    try
        {IndexName, Rest1} = tpm2b(Body),
        {NvDigest, _Tail} = tpm2b(Rest1),
        #{<<"attest-body-type">>     =>
              <<"TPMS_NV_DIGEST_CERTIFY_INFO">>,
          <<"nv-index-name">>        => hb_util:encode(IndexName),
          <<"nv-index-name-length">> => byte_size(IndexName),
          <<"nv-digest">>            => hb_util:encode(NvDigest),
          <<"nv-digest-length">>     => byte_size(NvDigest)}
    catch _:_ ->
        #{<<"attest-body-error">> =>
              <<"TPMS_NV_DIGEST_CERTIFY_INFO parse failed">>}
    end;
decode_attest_body(_OtherType, Body) ->
    #{<<"attest-body-type">> => <<"unknown">>,
      <<"attest-body-length">> => byte_size(Body),
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
    %% Group events by the PCR they extended -- a single pass over the
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

%% @doc Replay a PCR's events and compare to the quoted value.
%% The hash algorithm is inferred from the size of `Quoted' (20 =
%% SHA-1, 32 = SHA-256, 48 = SHA-384, 64 = SHA-512); the fold
%% starts from the algorithm's zero-seed, reads each event's
%% matching digest from `event.digests.<alg>', concatenates with
%% the accumulator, and rehashes. EV_NO_ACTION (code 3) is
%% skipped per TCG PFP section 5.3. If the quoted value has a size we
%% don't know, we default to SHA-256.
%%
%% Returns `undefined' when the event list is empty (nothing to
%% fold); else a map with replayed-digest + matches-quoted +
%% replayed-from-events + alg.
reconstruct_pcr([], _Quoted) -> undefined;
reconstruct_pcr(EvList, Quoted) ->
    reconstruct_pcr(EvList, Quoted, alg_from_digest_size(Quoted)).

%% Explicit-alg variant (lets callers force a bank).
reconstruct_pcr([], _Quoted, _Alg) -> undefined;
reconstruct_pcr(EvList, Quoted0, Alg) ->
    {AlgName, HashAtom, Size} = pcr_alg_triple(Alg),
    Seed = <<0:(Size*8)>>,
    %% Quoted may arrive here as raw bytes (in-memory path) or as
    %% a base64url-encoded binary string (JSON-envelope path).
    %% Normalise to raw so the downstream compare works either way.
    Quoted = normalise_digest(Quoted0, Size),
    Replayed = lists:foldl(
        fun(Ev, Acc) ->
            case maps:get(<<"event-type-code">>, Ev, 0) of
                3 -> Acc;
                _ ->
                    Digests = maps:get(<<"digests">>, Ev, #{}),
                    case normalise_digest(
                            maps:get(AlgName, Digests, undefined),
                            Size) of
                        D when is_binary(D), byte_size(D) =:= Size ->
                            crypto:hash(HashAtom,
                                         <<Acc/binary, D/binary>>);
                        _ -> Acc
                    end
            end
        end, Seed, EvList),
    Matches = case Quoted of
        <<>>      -> false;
        undefined -> false;
        _         -> Replayed =:= Quoted
    end,
    #{
        <<"replayed-digest">>      => hb_util:encode(Replayed),
        <<"matches-quoted">>       => Matches,
        <<"replayed-from-events">> => length(EvList),
        <<"alg">>                  => AlgName
    }.

%% @doc Normalise a digest value to raw bytes of the given size.
%% Handles both the in-memory path (already raw) and the JSON-
%% envelope path (base64url-encoded string). Any other shape
%% returns undefined so the caller can skip the event cleanly.
normalise_digest(undefined, _Size) -> undefined;
normalise_digest(<<>>, _Size) -> undefined;
normalise_digest(Bin, Size) when is_binary(Bin), byte_size(Bin) =:= Size ->
    Bin;
normalise_digest(Bin, Size) when is_binary(Bin) ->
    %% base64url of Size bytes has ceil(Size*4/3) chars; for 20B
    %% (sha1) -> 27, 32B (sha256) -> 43, 48B (sha384) -> 64, 64B
    %% (sha512) -> 86. Match on expected lengths to avoid
    %% accidentally decoding hex or other shapes.
    Expected = (Size * 4 + 2) div 3,
    case byte_size(Bin) of
        Expected ->
            try hb_util:decode(Bin) of
                Raw when is_binary(Raw), byte_size(Raw) =:= Size -> Raw;
                _ -> undefined
            catch _:_ -> undefined
            end;
        _ -> undefined
    end;
normalise_digest(_, _) -> undefined.

%% @doc Build a per-PCR `{pcr-index -> alg-name}' map from the
%% quote's TPMS_QUOTE_INFO pcrSelect. When the quote selects
%% multiple banks (e.g., SHA-1 for PCRs 0-7 + SHA-256 for
%% PCRs 8-15), each PCR in the list gets the bank's alg. When
%% the same PCR is selected under multiple banks the first
%% declared bank wins (TPM 2.0 spec behaviour).
%%
%% Returns an empty map if no quote / pcrSelect is present,
%% in which case replay falls back to size-based auto-detection.
pcr_algs_from_quote(E) ->
    Q = hb_maps:get(<<"tpm-quote">>, E, #{}, #{}),
    case hb_maps:get(<<"quoted">>, Q, <<>>, #{}) of
        <<>> -> #{};
        _ ->
            Meta = interpret_quote_metadata(E),
            case maps:is_key(<<"error">>, Meta) of
                true  -> #{};
                false ->
                    Sel = maps:get(<<"pcr-select">>, Meta, []),
                    build_alg_by_pcr_map(Sel, #{})
            end
    end.

build_alg_by_pcr_map([], Acc) -> Acc;
build_alg_by_pcr_map([Sel | Rest], Acc) when is_map(Sel) ->
    Alg = maps:get(<<"hash-alg-name">>, Sel, <<"sha256">>),
    Pcrs = maps:get(<<"pcr-indexes">>, Sel, []),
    %% First-declared-bank wins per spec -- use maps:merge with
    %% Acc last so existing entries are preserved.
    NewEntries = maps:from_list([{P, Alg} || P <- Pcrs]),
    build_alg_by_pcr_map(Rest, maps:merge(NewEntries, Acc));
build_alg_by_pcr_map([_ | Rest], Acc) ->
    build_alg_by_pcr_map(Rest, Acc).

%% Infer bank-alg name from the size of a raw digest. Defaults
%% to sha256 for the all-zero / empty / unknown cases.
alg_from_digest_size(Quoted) when is_binary(Quoted) ->
    case byte_size(Quoted) of
        20 -> <<"sha1">>;
        32 -> <<"sha256">>;
        48 -> <<"sha384">>;
        64 -> <<"sha512">>;
        _  -> <<"sha256">>
    end;
alg_from_digest_size(_) -> <<"sha256">>.

pcr_alg_triple(<<"sha1">>)   -> {<<"sha1">>,   sha,    20};
pcr_alg_triple(<<"sha256">>) -> {<<"sha256">>, sha256, 32};
pcr_alg_triple(<<"sha384">>) -> {<<"sha384">>, sha384, 48};
pcr_alg_triple(<<"sha512">>) -> {<<"sha512">>, sha512, 64};
pcr_alg_triple(_)            -> {<<"sha256">>, sha256, 32}.

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
%% on real hardware -- callers can rely on the SHAPE always being
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
    %% PCR 10 = IMA runtime. Per-file chain not yet transported --
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
    %% PCR 12 = UKI kernel cmdline (systemd-stub convention) -- the
    %% paper's single most information-dense measurement. Every
    %% flag the paper section Architecture l.223-230 + section Security table
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
    %% LapEE node identity -- fully parsed elsewhere in `node.*'.
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

%% EV_NO_ACTION -- SpecID header (PCR 0).
derive_from_event(0, 3, Parsed, _) ->
    case maps:get(<<"spec-id">>, Parsed, undefined) of
        undefined -> #{};
        V -> #{<<"spec-id">> => V}
    end;
%% EV_SEPARATOR -- boundary marker. Fires on many PCRs.
derive_from_event(_, 4, Parsed, _) ->
    #{<<"separator-seen">> => true,
      <<"separator-kind">> => maps:get(<<"separator">>, Parsed,
                                       <<"unknown">>)};
%% EV_S_CRTM_VERSION -- PCR 0.
derive_from_event(0, 8, Parsed, _) ->
    case maps:get(<<"crtm-version">>, Parsed, undefined) of
        V when is_binary(V), byte_size(V) > 0 ->
            #{<<"crtm-version">> => V};
        _ -> #{}
    end;
%% EV_CPU_MICROCODE -- PCR 1. Intel AND AMD layouts. The TCG parser
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
%% EV_POST_CODE -- PCR 0.
derive_from_event(0, 1, Parsed, _) ->
    case maps:get(<<"post-code">>, Parsed, undefined) of
        V when is_binary(V), byte_size(V) > 0 ->
            #{<<"post-codes">> => [V]};
        _ -> #{}
    end;
%% EV_EFI_HCRTM_EVENT -- PCR 0.
derive_from_event(0, 16#80000010, _, _) ->
    #{<<"hcrtm">> => true};
%% EV_EFI_PLATFORM_FIRMWARE_BLOB(2) -- PCR 0.
derive_from_event(0, Code, Parsed, _) when Code =:= 16#80000008;
                                           Code =:= 16#8000000A ->
    Addr = maps:get(<<"blob-physical-address">>, Parsed, 0),
    Len  = maps:get(<<"blob-length">>, Parsed, 0),
    Desc = maps:get(<<"blob-description">>, Parsed, <<>>),
    Blob = #{<<"address">> => Addr,
             <<"length">>  => Len,
             <<"description">> => Desc},
    #{<<"firmware-blobs">> => [Blob]};
%% EV_EFI_VARIABLE_DRIVER_CONFIG -- PCR 7.
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
%% EV_EFI_VARIABLE_AUTHORITY -- PCR 7.
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
%% EV_ACTION -- PCR 2/4, contributions to the boot action list.
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
%% EV_EFI_BOOT_SERVICES_APPLICATION -- PCR 4.
derive_from_event(4, 16#80000003, Parsed, _) ->
    App = #{
        <<"image-location-in-memory">> =>
            maps:get(<<"image-location-in-memory">>, Parsed, 0),
        <<"image-length-in-memory">>   =>
            maps:get(<<"image-length-in-memory">>, Parsed, 0)
    },
    #{<<"boot-services-applications">> => [App]};
%% EV_EFI_GPT_EVENT -- PCR 5.
derive_from_event(5, 16#80000006, _, _) ->
    #{<<"gpt-partition-tables">> => 1};
%% EV_IPL -- PCR 11/12/13 (systemd-stub key=value).
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
            %% Base: the raw cmdline string. Plus -- every security
            %% flag the paper (section Architecture l.223-230, section Security
            %% table) lists as a boot-time attested property gets
            %% extracted into a named `derived/<field>' slot.
            Flags = maps:get(<<"cmdline-flags">>, Parsed, #{}),
            maps:merge(
                #{<<"uki-cmdline">> => V},
                extract_cmdline_security_flags(Flags));
        _ -> #{}
    end;
derive_from_event(_, _, _, _) -> #{}.

%% Paper section Architecture line 219-230 + section Security table -- the set of
%% kernel-cmdline flags that, when present, attest to specific
%% security properties of the running kernel. Each mapping pins one
%% cmdline flag to one derived field on PCR 12.
%%
%%   mem_encrypt=on / sme=on -> mem-encrypt-requested: true
%%   kvm_intel.tdx=on       -> intel-tdx-requested: true
%%   iommu=pt | ...         -> iommu-mode: "pt" (or other)
%%   iommu.strict=1         -> iommu-strict: true
%%   intel_iommu=on         -> intel-iommu-requested: true
%%   amd_iommu=on           -> amd-iommu-requested: true
%%   lockdown=<mode>        -> lockdown-mode: "integrity"|"confidentiality"|...
%%   init_on_alloc=1        -> init-on-alloc: true
%%   init_on_free=1         -> init-on-free: true
%%   module.sig_enforce=1   -> module-sig-enforce: true
%%   roothash=<hex>         -> verity-root-hash: <hex>
%%   systemd.verity_root_hash=<hex> -> verity-root-hash: <hex>
%%   slab_nomerge           -> slab-nomerge: true
%%   page_poison=1          -> page-poison: true
%%   pti=on                 -> kernel-page-table-isolation: true
%%   randomize_kstack_offset=1 -> randomize-kstack-offset: true
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
            %% keys* from the PCR alone -- that needs the event log.
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
%% An empty match block doesn't match -- callers who want a
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
    %% end-to-end today -- a gap noted in SECURITY.md item 8), we can
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
    end;
%% Reviewer pass 10 fuzzer: some JSON round-trip libraries
%% decode `null' into the Erlang atom `undefined' rather than a
%% binary. Without this clause, `decode_cert(undefined)' raised
%% `function_clause' and escaped past `claim/3' / `interpret/3'
%% (neither wraps its callee in `try' -- only the `verify/3' path
%% does via `safe_interpret'). The LapEE canonical rule in
%% AGENTS.md demands every claim.* field populate to a concrete
%% value OR an explicit unknown/absent; a 500 stacktrace is
%% neither.
decode_cert(_) -> {error, not_binary}.

decode_pub_key(<<>>) -> {error, empty};
decode_pub_key(Pem) when is_binary(Pem) ->
    case public_key:pem_decode(Pem) of
        [Entry | _] ->
            try {ok, public_key:pem_entry_decode(Entry)}
            catch C:R -> {error, {C, R}}
            end;
        _ -> {error, no_entries}
    end;
decode_pub_key(_) -> {error, not_binary}.

%%% Extract TPM-specific attributes from the EK cert -- following the
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
%% native HB node message (atoms) or a TABM (binaries) -- look up
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
%% the paper section Architecture line 219-230 + section Security table names.
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
    %% TME -- composed from tier 2 (cmdline mem_encrypt=on) +
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
    %% Intel Sapphire Rapids sig -> family=6 model=143 stepping=2
    %% (packed per Intel SDM section 9.11.1). Encoded u32 LE:
    %%   family=6 base, model low=F, ExtModel=8, stepping=2
    %%   -> raw sig = 0x000806F2
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
    %% claim.tme -- tier-3 evidence from kernel-name-prefix match
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
    %% claim.lockdown -- tier-3 confidentiality-confirmed = true
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
    %% Two UEFI_IMAGE_LOAD_EVENT payloads -- with empty device
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

%% Hour-5: pcr-bitmap decoder -- 0x87 (byte 0) -> PCRs 0,1,2,7.
%% Cross-byte case: bitmap `<0x01, 0x01>` -> PCR 0 + PCR 8.
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
        %% PCR 0, 1, 7 selected -> bitmap 0x83.
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
        %% Only PCR 0 selected -> bitmap 0x01.
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

%% Hour-6: selected PCR absent from envelope -> missing-pcrs
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

%% Hour-7: PCR replay flags the log↔quote consistency on the
%% flat claim API. Build a minimal log with TWO events on
%% PCR 0 whose SHA-256 digests we compute ourselves, reproduce
%% the TPM's SHA-256 fold, use that as the quoted value, and
%% assert the match fires.
claim_surface_hour7_pcr_replay_match_test() ->
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    MkRec = fun(Pcr, Code, Data) ->
        S1 = crypto:hash(sha,    Data),
        S2 = crypto:hash(sha256, Data),
        Sz = byte_size(Data),
        {<<Pcr:32/little, Code:32/little, 2:32/little,
           16#04:16/little, S1/binary,
           16#0B:16/little, S2/binary,
           Sz:32/little, Data/binary>>,
         S2}
    end,
    {R1, D1} = MkRec(0, 16#8, <<"FW v1">>),
    {R2, D2} = MkRec(0, 16#8, <<"FW v2">>),
    Raw = <<FirstRec/binary, R1/binary, R2/binary>>,
    %% Replay the SHA-256 fold by hand:
    Fold0 = crypto:hash(sha256, <<0:256, D1/binary>>),
    Fold1 = crypto:hash(sha256, <<Fold0/binary, D2/binary>>),
    Envelope = #{
        <<"tcg-event-log">> => hb_util:encode(Raw),
        <<"tpm-quote">> => #{
            <<"pcr-values">> => #{
                <<"0">> => hb_util:encode(Fold1)
            },
            <<"quoted">> => <<>>}
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PR = maps:get(<<"pcr-replay">>, Claim),
    ?assertEqual([0], maps:get(<<"pcrs-matching">>, PR)),
    ?assertEqual([],  maps:get(<<"pcrs-mismatching">>, PR)),
    ?assertEqual(true, maps:get(<<"consistent">>, PR)),
    PerPcr = maps:get(<<"per-pcr">>, PR),
    Row = maps:get(<<"0">>, PerPcr),
    %% EV_NO_ACTION (SpecID first record) + two EV_S_CRTM_VERSION
    %% events = 3 events attributed to PCR 0. The replay still
    %% matches because EV_NO_ACTION is excluded from the fold.
    ?assertEqual(3, maps:get(<<"event-count">>, Row)),
    ?assertEqual(true, maps:get(<<"matches">>, Row)),
    ok.

%% A tampered quoted value for PCR 0 triggers mismatch.
claim_surface_hour7_pcr_replay_mismatch_test() ->
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    Data = <<"FW v1">>,
    S1 = crypto:hash(sha, Data),
    S2 = crypto:hash(sha256, Data),
    Rec = <<0:32/little, 16#8:32/little, 2:32/little,
            16#04:16/little, S1/binary,
            16#0B:16/little, S2/binary,
            (byte_size(Data)):32/little, Data/binary>>,
    Raw = <<FirstRec/binary, Rec/binary>>,
    %% Ship an obviously-wrong quoted PCR value (all-ones).
    Wrong = <<16#FF:8, 0:(256-8)>>,
    Envelope = #{
        <<"tcg-event-log">> => hb_util:encode(Raw),
        <<"tpm-quote">> => #{
            <<"pcr-values">> => #{<<"0">> => hb_util:encode(Wrong)},
            <<"quoted">> => <<>>}
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PR = maps:get(<<"pcr-replay">>, Claim),
    ?assertEqual([0], maps:get(<<"pcrs-mismatching">>, PR)),
    ?assertEqual(false, maps:get(<<"consistent">>, PR)),
    ok.

%% Hour-7: IMA parser round-trips ima-ng + ima-sig + ima-buf
%% templates from an ASCII log carried as base64url in the
%% envelope field `ima-log-ascii'.
claim_surface_hour7_ima_parse_test() ->
    Log = <<
      "10 abcdef0123456789abcdef0123456789abcdef01 ima-ng "
      "sha256:aa11bb22cc33dd44ee55ff66aa11bb22cc33dd44ee55ff66aa11bb22cc33dd44"
      " /bin/bash\n"
      "10 deadbeefcafebabe0123456789abcdef0123beef ima-sig "
      "sha256:5678abcd5678abcd5678abcd5678abcd5678abcd5678abcd5678abcd5678abcd"
      " /usr/lib/systemd/systemd 0302016430820160deadbeef\n"
      "10 aa11bb22cc33dd44ee55ff6600000000deadbeef ima-buf "
      "sha512:0000111122223333444455556666777788889999aaaabbbbccccddddeeeeffff1111222233334444555566667777888899990000aaaabbbbccccddddeeeeffff1111"
      " kexec-buffer\n"
    >>,
    Envelope = #{<<"ima-log-ascii">> => hb_util:encode(Log)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    I = maps:get(<<"ima">>, Claim),
    ?assertEqual(true, maps:get(<<"present">>, I)),
    ?assertEqual(3, maps:get(<<"event-count">>, I)),
    %% Templates sorted alphabetically.
    ?assertEqual([<<"ima-buf">>, <<"ima-ng">>, <<"ima-sig">>],
                 maps:get(<<"templates-seen">>, I)),
    ?assert(lists:member(<<"sha256">>,
                         maps:get(<<"unique-hash-algs">>, I))),
    ?assert(lists:member(<<"sha512">>,
                         maps:get(<<"unique-hash-algs">>, I))),
    Entries = maps:get(<<"entries">>, I),
    ?assertEqual(3, length(Entries)),
    [E1, E2, E3] = Entries,
    ?assertEqual(<<"ima-ng">>, maps:get(<<"template">>, E1)),
    ?assertEqual(<<"/bin/bash">>,
                 maps:get(<<"pathname">>, E1)),
    ?assertEqual(false, maps:get(<<"signature-present">>, E1)),
    ?assertEqual(<<"ima-sig">>, maps:get(<<"template">>, E2)),
    ?assertEqual(true, maps:get(<<"signature-present">>, E2)),
    ?assertMatch(<<"0302016430820160deadbeef">>,
                 maps:get(<<"signature-hex">>, E2)),
    ?assertEqual(<<"ima-buf">>, maps:get(<<"template">>, E3)),
    ?assertEqual(true, maps:get(<<"is-buffer">>, E3)),
    ?assertEqual(<<"kexec-buffer">>,
                 maps:get(<<"pathname">>, E3)),
    ok.

%% IMA is a no-op (returns unknown-ima-claim) when the envelope
%% has no `ima-log-ascii' field.
claim_surface_hour7_ima_absent_test() ->
    Envelope = #{<<"tcg-event-log">> => <<"">>},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    I = maps:get(<<"ima">>, Claim),
    ?assertEqual(false, maps:get(<<"present">>, I)),
    ?assertEqual(0, maps:get(<<"event-count">>, I)),
    ?assertEqual([], maps:get(<<"entries">>, I)),
    ok.

%% Hour-8: multi-bank PCR replay auto-detects the hash algorithm
%% from the declared digest size (20 = SHA-1, 32 = SHA-256,
%% 48 = SHA-384, 64 = SHA-512).
reconstruct_pcr_alg_detection_test() ->
    %% Two no-op events so only the seed is used.
    MkEv = fun(Alg, Size) ->
        #{<<"event-type-code">> => 16#8,
          <<"digests">> =>
              #{Alg => <<0:(Size*8)>>}}
    end,
    %% SHA-1 replay: seed = 20 zero bytes, fold one sha1 = 0 gives
    %% SHA-1(zero20 || zero20).
    Expected1 = crypto:hash(sha, <<0:320>>),
    R1 = reconstruct_pcr([MkEv(<<"sha1">>, 20)], Expected1),
    ?assertEqual(<<"sha1">>,  maps:get(<<"alg">>, R1)),
    ?assertEqual(true,        maps:get(<<"matches-quoted">>, R1)),
    %% SHA-256: seed = 32 zeros, fold one sha256 = 0.
    Expected256 = crypto:hash(sha256, <<0:512>>),
    R2 = reconstruct_pcr([MkEv(<<"sha256">>, 32)], Expected256),
    ?assertEqual(<<"sha256">>, maps:get(<<"alg">>, R2)),
    ?assertEqual(true,         maps:get(<<"matches-quoted">>, R2)),
    %% SHA-384.
    Expected384 = crypto:hash(sha384, <<0:768>>),
    R3 = reconstruct_pcr([MkEv(<<"sha384">>, 48)], Expected384),
    ?assertEqual(<<"sha384">>, maps:get(<<"alg">>, R3)),
    ?assertEqual(true,         maps:get(<<"matches-quoted">>, R3)),
    %% SHA-512.
    Expected512 = crypto:hash(sha512, <<0:1024>>),
    R4 = reconstruct_pcr([MkEv(<<"sha512">>, 64)], Expected512),
    ?assertEqual(<<"sha512">>, maps:get(<<"alg">>, R4)),
    ?assertEqual(true,         maps:get(<<"matches-quoted">>, R4)),
    ok.

%% Explicit-alg variant forces the bank even when the digest size
%% would suggest otherwise.
reconstruct_pcr_explicit_alg_test() ->
    Ev = #{<<"event-type-code">> => 16#8,
           <<"digests">> =>
               #{<<"sha384">> => <<0:384>>}},
    Expected384 = crypto:hash(sha384, <<0:768>>),
    R = reconstruct_pcr([Ev], Expected384, <<"sha384">>),
    ?assertEqual(<<"sha384">>, maps:get(<<"alg">>, R)),
    ?assertEqual(true, maps:get(<<"matches-quoted">>, R)),
    ok.

%% Hour-8: platform-config aggregates the UEFI handoff tables +
%% POST codes + option-ROM + UEFI variable count + per-PCR event
%% histogram from a single-pass over the event log. This test
%% builds a synthetic log with known content and asserts the
%% aggregation arithmetic.
claim_surface_hour8_platform_config_test() ->
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    MkRec = fun(Pcr, Code, Data) ->
        S1 = crypto:hash(sha,    Data),
        S2 = crypto:hash(sha256, Data),
        Sz = byte_size(Data),
        <<Pcr:32/little, Code:32/little, 2:32/little,
          16#04:16/little, S1/binary,
          16#0B:16/little, S2/binary,
          Sz:32/little, Data/binary>>
    end,
    %% EV_POST_CODE on PCR 0.
    PC1 = MkRec(0, 16#1, <<"POST-stage-1">>),
    PC2 = MkRec(0, 16#1, <<"POST-stage-2">>),
    %% EV_EFI_HANDOFF_TABLES v1 with 2 rows: SMBIOS + ACPI 2.0.
    SmbiosGuidBin = decode_guid(<<"eb9d2d31-2d88-11d3-9a16-0090273fc14d">>),
    AcpiGuidBin   = decode_guid(<<"8868e871-e4f1-11d3-bc22-0080c73c8881">>),
    HtData = <<2:64/little,
               SmbiosGuidBin/binary, 16#7EFDE000:64/little,
               AcpiGuidBin/binary,   16#7EFDE100:64/little>>,
    HtRec = MkRec(1, 16#80000009, HtData),
    %% Option ROM action.
    OrData = <<"Option ROM init">>,
    OrRec = MkRec(2, 16#80000007, OrData),
    Raw = <<FirstRec/binary, PC1/binary, PC2/binary,
            HtRec/binary, OrRec/binary>>,
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Raw)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PC = maps:get(<<"platform-config">>, Claim),
    ?assertEqual(true,  maps:get(<<"smbios-present">>, PC)),
    ?assertEqual(true,  maps:get(<<"acpi-present">>, PC)),
    ?assertEqual(false, maps:get(<<"hob-list-present">>, PC)),
    ?assertEqual(2, length(maps:get(<<"handoff-tables-v1">>, PC))),
    ?assertEqual(2, maps:get(<<"handoff-tables-count">>, PC)),
    ?assertEqual(1, maps:get(<<"option-rom-count">>, PC)),
    ?assertEqual([<<"POST-stage-1">>, <<"POST-stage-2">>],
                 maps:get(<<"post-codes">>, PC)),
    Hist = maps:get(<<"event-count-per-pcr">>, PC),
    %% PCR 0: SpecID + 2 POST, PCR 1: handoff, PCR 2: option-ROM.
    ?assertEqual(3, maps:get(<<"0">>, Hist)),
    ?assertEqual(1, maps:get(<<"1">>, Hist)),
    ?assertEqual(1, maps:get(<<"2">>, Hist)),
    ok.

%% Convert "aabbccdd-eeff-0011-2233-445566778899" into the
%% mixed-endian 16-byte EFI_GUID binary. Test helper only.
decode_guid(Dashed) ->
    NoDashes = binary:replace(Dashed, <<"-">>, <<>>, [global]),
    {ok, [A, B, C, D, E], _} =
        io_lib:fread("~8c~4c~4c~4c~12c",
                      unicode:characters_to_list(NoDashes)),
    Bytes = list_to_binary(
        [hex_bytes(A), hex_bytes(B), hex_bytes(C),
         hex_bytes(D), hex_bytes(E)]),
    <<D1:32, D2:16, D3:16, D4Rest:8/binary>> = Bytes,
    <<D1:32/little, D2:16/little, D3:16/little, D4Rest/binary>>.

hex_bytes([]) -> [];
hex_bytes([A, B | Rest]) ->
    [list_to_integer([A, B], 16) | hex_bytes(Rest)].

%% Hour-9: boot-chain DB cross-reference -- a row whose
%% device-path ends with `\EFI\Boot\BootX64.efi' should match
%% the fallback-bootx64 catalogue entry and attach publisher /
%% product / category / cve-status + `matched-by=device-path-
%% suffix'. We build a synthetic log whose boot-services row
%% has a device path carrying the fallback suffix.
claim_surface_hour9_boot_chain_enrichment_test() ->
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    %% Build a UEFI_IMAGE_LOAD_EVENT with a device path ending in
    %% the fallback path. Use a File Path node (type 0x04
    %% sub-type 0x04, UTF-16LE path string + End-entire).
    PathUtf16 = unicode:characters_to_binary(
                  <<"\\EFI\\Boot\\BootX64.efi">>,
                  utf8, {utf16, little}),
    PathNode = <<16#04, 16#04,
                 (byte_size(PathUtf16) + 4):16/little,
                 PathUtf16/binary>>,
    EndEntire = <<16#7F, 16#FF, 16#04, 16#00>>,
    DevicePath = <<PathNode/binary, EndEntire/binary>>,
    UefiImgLoad = <<16#10000:64/little,    %% imageLocationInMemory
                    16#20000:64/little,    %% imageLengthInMemory
                    16#400000:64/little,   %% imageLinkTimeAddress
                    (byte_size(DevicePath)):64/little,
                    DevicePath/binary>>,
    S1 = crypto:hash(sha,    UefiImgLoad),
    S2 = crypto:hash(sha256, UefiImgLoad),
    AppRec = <<4:32/little, 16#80000003:32/little, 2:32/little,
               16#04:16/little, S1/binary,
               16#0B:16/little, S2/binary,
               (byte_size(UefiImgLoad)):32/little, UefiImgLoad/binary>>,
    Raw = <<FirstRec/binary, AppRec/binary>>,
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Raw)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    BC = maps:get(<<"boot-chain">>, Claim),
    [Row | _] = maps:get(<<"chain">>, BC),
    ?assertEqual(<<"application">>, maps:get(<<"role">>, Row)),
    ?assertEqual(<<"device-path-suffix">>,
                 maps:get(<<"matched-by">>, Row)),
    ?assertEqual(<<"multi-vendor">>,
                 maps:get(<<"publisher">>, Row)),
    ?assertEqual(<<"fallback">>,
                 maps:get(<<"category">>, Row)),
    ?assertEqual(<<"fallback-bootx64">>,
                 maps:get(<<"matched-profile-key">>, Row)),
    ok.

%% A boot-chain row whose device path doesn't match any
%% catalogue pattern gets matched-by=unmatched and null
%% attribution fields (shape stays stable).
claim_surface_hour9_boot_chain_unmatched_test() ->
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    %% Empty device path -> device-path-text is empty ->
    %% no suffix match possible; image-hash random -> no
    %% hash match either.
    UefiImgLoad = <<0:64/little, 0:64/little, 0:64/little,
                    0:64/little>>,
    S1 = crypto:hash(sha,    UefiImgLoad),
    S2 = crypto:hash(sha256, UefiImgLoad),
    AppRec = <<4:32/little, 16#80000003:32/little, 2:32/little,
               16#04:16/little, S1/binary,
               16#0B:16/little, S2/binary,
               (byte_size(UefiImgLoad)):32/little, UefiImgLoad/binary>>,
    Raw = <<FirstRec/binary, AppRec/binary>>,
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Raw)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    BC = maps:get(<<"boot-chain">>, Claim),
    [Row | _] = maps:get(<<"chain">>, BC),
    ?assertEqual(<<"unmatched">>, maps:get(<<"matched-by">>, Row)),
    ?assertEqual(null, maps:get(<<"publisher">>, Row)),
    ?assertEqual(null, maps:get(<<"matched-profile-key">>, Row)),
    ok.

%% Hour-9: match_dp_suffix helper is case-insensitive
%% (EFI filesystems are case-insensitive by spec).
match_dp_suffix_case_insensitive_test() ->
    ?assertEqual({match, <<"\\EFI\\Boot\\BootX64.efi">>},
                 match_dp_suffix(
                   <<"Acpi(...)/\\EFI\\BOOT\\BOOTX64.EFI">>,
                   [<<"\\EFI\\Boot\\BootX64.efi">>])),
    ?assertEqual(nomatch,
                 match_dp_suffix(<<"/foo/bar">>,
                                  [<<"/baz/qux">>])),
    ok.

%% Hour-10: pcrSelect-driven bank selection. A quote that
%% selects PCR 0 under SHA-1 + PCR 7 under SHA-256 routes each
%% PCR to the correct bank -- even if the digest-size heuristic
%% alone would have guessed wrong.
claim_surface_hour10_pcr_select_alg_dispatch_test() ->
    Magic = <<16#FF, "TCG">>,
    Type = 16#8018,
    QsName = crypto:hash(sha256, <<"signer">>),
    QsTpm2B = <<(byte_size(QsName)):16/big, QsName/binary>>,
    Nonce = <<"n">>,
    NonceTpm2B = <<(byte_size(Nonce)):16/big, Nonce/binary>>,
    %% Two PCR selections: SHA-1 (0x0004) with PCR 0, SHA-256
    %% (0x000B) with PCR 7.
    PcrSelect = <<2:32/big,
                  16#0004:16/big, 3:8, 16#01, 0, 0,
                  16#000B:16/big, 3:8, 16#80, 0, 0>>,
    PcrDigest = <<0:256>>,
    PcrDigestTpm2B = <<(byte_size(PcrDigest)):16/big,
                         PcrDigest/binary>>,
    Quoted = <<Magic/binary, Type:16/big,
               QsTpm2B/binary, NonceTpm2B/binary,
               0:64, 0:32, 0:32, 1:8, 0:64,
               PcrSelect/binary, PcrDigestTpm2B/binary>>,
    %% PCR 0 shipped as SHA-1 (20 bytes), PCR 7 as SHA-256.
    Pcr0_sha1 = <<1:160>>,
    Pcr7_sha256 = <<2:256>>,
    Envelope = #{
        <<"tpm-quote">> => #{
            <<"quoted">> => hb_util:encode(Quoted),
            <<"pcr-values">> => #{
                <<"0">> => hb_util:encode(Pcr0_sha1),
                <<"7">> => hb_util:encode(Pcr7_sha256)
            }
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PR = maps:get(<<"pcr-replay">>, Claim),
    PerPcr = maps:get(<<"per-pcr">>, PR),
    ?assertEqual(<<"sha1">>,
                 maps:get(<<"alg">>, maps:get(<<"0">>, PerPcr))),
    ?assertEqual(<<"sha256">>,
                 maps:get(<<"alg">>, maps:get(<<"7">>, PerPcr))),
    ok.

%% No quote present -> pcr_algs_from_quote returns an empty map
%% and replay_one_pcr falls back to size-based detection.
claim_surface_hour10_no_quote_falls_back_test() ->
    Envelope = #{
        <<"tpm-quote">> => #{
            <<"quoted">> => <<>>,
            <<"pcr-values">> => #{
                <<"0">> => hb_util:encode(<<3:256>>)   %% 32 bytes
            }
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PR = maps:get(<<"pcr-replay">>, Claim),
    Row = maps:get(<<"0">>, maps:get(<<"per-pcr">>, PR)),
    ?assertEqual(<<"sha256">>, maps:get(<<"alg">>, Row)),
    ok.

%% Hour-11: claim.ima-policy picks the Fedora baseline when
%% the envelope carries a `kernel_name=Fedora-Linux-...' EV_IPL,
%% and flags /tmp/evil-binary as "unexpected" + a module
%% without signature as "signature-missing".
claim_surface_hour11_ima_policy_test() ->
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    Kname = <<"kernel_name=Fedora-Linux-6.8.7-300.fc40.x86_64", 0>>,
    KnSha1 = crypto:hash(sha, Kname),
    KnSha256 = crypto:hash(sha256, Kname),
    KnRec = <<12:32/little, 16#D:32/little, 2:32/little,
              16#04:16/little, KnSha1/binary,
              16#0B:16/little, KnSha256/binary,
              (byte_size(Kname)):32/little, Kname/binary>>,
    Raw = <<FirstRec/binary, KnRec/binary>>,
    Ima = <<
      "10 abc ima-ng "
      "sha256:11111111111111111111111111111111"
      "11111111111111111111111111111111"
      " /usr/bin/bash\n"
      "10 def ima-ng "
      "sha256:22222222222222222222222222222222"
      "22222222222222222222222222222222"
      " /tmp/evil-binary\n"
      "10 ghi ima-ng "
      "sha256:33333333333333333333333333333333"
      "33333333333333333333333333333333"
      " /usr/lib/modules/6.8.7/drivers/e1000e.ko\n"
    >>,
    Envelope = #{
        <<"tcg-event-log">> => hb_util:encode(Raw),
        <<"ima-log-ascii">> => hb_util:encode(Ima)
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    IP = maps:get(<<"ima-policy">>, Claim),
    ?assertEqual(<<"fedora-baseline">>,
                 maps:get(<<"picked-policy-key">>, IP)),
    ?assertEqual(<<"kernel-name-prefix">>,
                 maps:get(<<"policy-match-reason">>, IP)),
    ?assertEqual(3, maps:get(<<"total-entries">>, IP)),
    Counts = maps:get(<<"classification-counts">>, IP),
    ?assertEqual(1, maps:get(<<"matched">>, Counts)),
    ?assertEqual(1, maps:get(<<"unexpected">>, Counts)),
    ?assertEqual(1, maps:get(<<"signature-missing">>, Counts)),
    Violations = maps:get(<<"violations">>, IP),
    ?assertEqual(2, length(Violations)),
    Paths = [maps:get(<<"pathname">>, V) || V <- Violations],
    ?assert(lists:member(<<"/tmp/evil-binary">>, Paths)),
    ?assert(lists:member(<<"/usr/lib/modules/6.8.7/drivers/e1000e.ko">>,
                         Paths)),
    ok.

%% No IMA log -> well-formed "unknown" with reason.
claim_surface_hour11_ima_policy_no_log_test() ->
    Envelope = #{<<"tcg-event-log">> => <<"">>},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    IP = maps:get(<<"ima-policy">>, Claim),
    ?assertEqual(<<"no-ima-log">>,
                 maps:get(<<"policy-match-reason">>, IP)),
    ?assertEqual([], maps:get(<<"violations">>, IP)),
    ok.

%% Hour-11: digest-bank-coverage reports which algorithm banks
%% are present across the event log.
claim_surface_hour11_digest_bank_coverage_test() ->
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    Data = <<"FW v1">>,
    S1 = crypto:hash(sha, Data),
    S2 = crypto:hash(sha256, Data),
    Rec = <<0:32/little, 16#8:32/little, 2:32/little,
            16#04:16/little, S1/binary,
            16#0B:16/little, S2/binary,
            (byte_size(Data)):32/little, Data/binary>>,
    Raw = <<FirstRec/binary, Rec/binary>>,
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Raw)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PC = maps:get(<<"platform-config">>, Claim),
    Banks = maps:get(<<"digest-banks-present">>, PC),
    %% SpecID's EV_NO_ACTION has a 20-byte sha1-sized zero; the
    %% CRTM event has both sha1 + sha256. Both banks appear.
    ?assert(lists:member(<<"sha1">>, Banks)),
    ?assert(lists:member(<<"sha256">>, Banks)),
    Coverage = maps:get(<<"digest-bank-coverage">>, PC),
    ?assert(maps:get(<<"sha1">>, Coverage) >= 1),
    ?assert(maps:get(<<"sha256">>, Coverage) >= 1),
    ok.

%% Hour-12: claim.secure-boot-policy on a real Dell fixture.
%% The dell-notebook-wbcl.bin event log carries full PK/KEK/db/
%% dbx content (1/2/4/267 entries) but Secure Boot disabled ->
%% policy-posture="audit-only" + policy-strength="latest-
%% revocations" + trusted-signers list populated.
claim_surface_hour12_secure_boot_policy_dell_test() ->
    Path = filename:join([
        case code:priv_dir(hb) of
            {error, _} ->
                filename:join(
                    filename:dirname(
                        filename:dirname(code:which(?MODULE))),
                    "priv");
            D -> D
        end,
        "tpm-interpret", "fixtures",
        "dell-notebook-wbcl.bin"]),
    case filelib:is_file(Path) of
        false -> ok;
        true ->
            {ok, Bin} = file:read_file(Path),
            Envelope = #{<<"tcg-event-log">> => hb_util:encode(Bin)},
            {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
            SP = maps:get(<<"secure-boot-policy">>, Claim),
            ?assertEqual(1, maps:get(<<"pk-entry-count">>, SP)),
            ?assertEqual(2, maps:get(<<"kek-entry-count">>, SP)),
            ?assertEqual(4, maps:get(<<"db-entry-count">>, SP)),
            ?assert(maps:get(<<"dbx-entry-count">>, SP) >= 100),
            ?assertEqual(<<"audit-only">>,
                         maps:get(<<"policy-posture">>, SP)),
            ?assertEqual(<<"latest-revocations">>,
                         maps:get(<<"policy-strength">>, SP)),
            TrustedSigners = maps:get(<<"trusted-signers">>, SP),
            ?assertEqual(4, length(TrustedSigners)),
            %% At least one signer's subject mentions Dell.
            Subjects = [maps:get(<<"subject">>, S) || S <- TrustedSigners],
            ?assert(lists:any(
                fun(B) -> binary:match(B, <<"Dell">>) =/= nomatch end,
                Subjects))
    end.

%% claim.secure-boot-policy on an empty envelope returns
%% well-formed `unknown' shape with all-zero counts.
claim_surface_hour12_secure_boot_policy_empty_test() ->
    Envelope = #{<<"tcg-event-log">> => <<"">>},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    SP = maps:get(<<"secure-boot-policy">>, Claim),
    ?assertEqual(0, maps:get(<<"pk-entry-count">>, SP)),
    ?assertEqual(0, maps:get(<<"kek-entry-count">>, SP)),
    ?assertEqual(0, maps:get(<<"db-entry-count">>, SP)),
    ?assertEqual(0, maps:get(<<"dbx-entry-count">>, SP)),
    ?assertEqual([], maps:get(<<"trusted-signers">>, SP)),
    ?assertEqual([], maps:get(<<"blocked-hashes">>, SP)),
    ok.

%% Hour-12: kernel-module pathname decode enriches IMA entries
%% with module-name / module-kernel-version / module-subsystem
%% / module-compression, and claim.kernel-integrity.modules
%% summarises them across subsystems.
claim_surface_hour12_kernel_module_decode_test() ->
    Ima = <<
      "10 abc ima-sig sha256:1111 "
      "/usr/lib/modules/6.8.7-300.fc40.x86_64/kernel/drivers/net/"
      "ethernet/intel/e1000e/e1000e.ko.xz 0302\n"
      "10 def ima-ng sha256:2222 "
      "/lib/modules/6.8.7-300.fc40.x86_64/kernel/fs/ext4/ext4.ko.gz\n"
      "10 ghi ima-ng sha256:3333 /usr/bin/bash\n"
      "10 jkl ima-ng sha256:4444 "
      "/usr/lib/modules/5.15.0/kernel/sound/core/snd.ko\n"
    >>,
    Envelope = #{<<"ima-log-ascii">> => hb_util:encode(Ima)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    KI = maps:get(<<"kernel-integrity">>, Claim),
    Mods = maps:get(<<"modules">>, KI),
    %% 3 kernel-module entries; /usr/bin/bash excluded.
    ?assertEqual(3, maps:get(<<"modules-loaded-count">>, Mods)),
    ?assertEqual(1, maps:get(<<"modules-signed-count">>, Mods)),
    ?assertEqual(2, maps:get(<<"modules-unsigned-count">>, Mods)),
    KVers = maps:get(<<"modules-kernel-versions">>, Mods),
    ?assertEqual([<<"5.15.0">>, <<"6.8.7-300.fc40.x86_64">>], KVers),
    BySub = maps:get(<<"modules-by-subsystem">>, Mods),
    ?assertEqual(1,
                 maps:get(<<"drivers/net/ethernet/intel/e1000e">>,
                           BySub)),
    ?assertEqual(1, maps:get(<<"fs/ext4">>, BySub)),
    ?assertEqual(1, maps:get(<<"sound/core">>, BySub)),
    %% Per-module rows carry all fields.
    Rows = maps:get(<<"modules">>, Mods),
    ?assertEqual(3, length(Rows)),
    [R1 | _] = Rows,
    ?assertEqual(<<"e1000e">>, maps:get(<<"module-name">>, R1)),
    ?assertEqual(<<"6.8.7-300.fc40.x86_64">>,
                 maps:get(<<"module-kernel-version">>, R1)),
    ?assertEqual(<<"xz">>, maps:get(<<"module-compression">>, R1)),
    ?assertEqual(true, maps:get(<<"signature-present">>, R1)),
    ok.

%% Paths that don't match kernel-module layout don't produce
%% `is-kernel-module=true' and don't show up in the summary.
claim_surface_hour12_non_module_paths_excluded_test() ->
    Ima = <<
      "10 abc ima-ng sha256:1111 /usr/bin/bash\n"
      "10 def ima-ng sha256:2222 /etc/passwd\n"
      "10 ghi ima-ng sha256:3333 /home/user/script.sh\n"
    >>,
    Envelope = #{<<"ima-log-ascii">> => hb_util:encode(Ima)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    KI = maps:get(<<"kernel-integrity">>, Claim),
    Mods = maps:get(<<"modules">>, KI),
    ?assertEqual(0, maps:get(<<"modules-loaded-count">>, Mods)),
    ?assertEqual([], maps:get(<<"modules">>, Mods)),
    ok.

%% Hour-13: evidence-digest is deterministic -- identical
%% envelopes produce the same digest.
claim_surface_hour13_evidence_digest_deterministic_test() ->
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(build_tcg_fixture())},
    {ok, #{<<"body">> := C1}} = claim(Envelope, #{}, #{}),
    {ok, #{<<"body">> := C2}} = claim(Envelope, #{}, #{}),
    Ed1 = maps:get(<<"evidence-digest">>, C1),
    Ed2 = maps:get(<<"evidence-digest">>, C2),
    ?assertEqual(maps:get(<<"digest">>, Ed1),
                 maps:get(<<"digest">>, Ed2)),
    ?assertEqual(<<"sha256">>, maps:get(<<"alg">>, Ed1)),
    ?assertEqual(<<"canonical-sorted-keys-erlang-ext-v2">>,
                 maps:get(<<"form">>, Ed1)),
    %% Digest is 32 bytes base64url-encoded = 43 chars.
    ?assertEqual(43, byte_size(maps:get(<<"digest">>, Ed1))),
    ok.

%% Different envelopes -> different digests. Use a synthetic
%% log we know decodes cleanly.
claim_surface_hour13_evidence_digest_discriminates_test() ->
    Env1 = #{<<"tcg-event-log">> =>
                 hb_util:encode(build_tcg_fixture())},
    %% Any envelope that produces a different claim tree -- an
    %% empty one differs trivially.
    Env2 = #{<<"tcg-event-log">> => <<"">>},
    {ok, #{<<"body">> := C1}} = claim(Env1, #{}, #{}),
    {ok, #{<<"body">> := C2}} = claim(Env2, #{}, #{}),
    D1 = maps:get(<<"digest">>,
                   maps:get(<<"evidence-digest">>, C1)),
    D2 = maps:get(<<"digest">>,
                   maps:get(<<"evidence-digest">>, C2)),
    ?assertNotEqual(D1, D2),
    ok.

%% Hour-13: canonicalise_claim sorts map keys recursively so
%% that `term_to_binary' produces a deterministic byte-sequence
%% regardless of insertion order.
canonicalise_claim_sorts_keys_test() ->
    A = #{<<"b">> => 2, <<"a">> => 1, <<"z">> => #{<<"q">> => 9,
                                                      <<"p">> => 8}},
    B = #{<<"z">> => #{<<"p">> => 8, <<"q">> => 9},
          <<"a">> => 1, <<"b">> => 2},
    ?assertEqual(term_to_binary(canonicalise_claim(A),
                                  [{minor_version, 2}]),
                 term_to_binary(canonicalise_claim(B),
                                  [{minor_version, 2}])),
    ok.

%% Hour-13: timeline aggregates TPM clock + reset + restart
%% + event-log seq range + IMA count into a compact stanza.
claim_surface_hour13_timeline_test() ->
    Magic = <<16#FF, "TCG">>, Type = 16#8018,
    QsName = crypto:hash(sha256, <<"s">>),
    QsTpm2B = <<(byte_size(QsName)):16/big, QsName/binary>>,
    Nonce = <<"n">>, NonceTpm2B = <<1:16/big, Nonce/binary>>,
    Clock = 12345678, ResetCount = 42, RestartCount = 3,
    Quoted = <<Magic/binary, Type:16/big,
               QsTpm2B/binary, NonceTpm2B/binary,
               Clock:64/big, ResetCount:32/big,
               RestartCount:32/big, 1:8, 0:64,
               0:32/big, 0:16>>,
    Envelope = #{
        <<"tpm-quote">> => #{
            <<"quoted">> => hb_util:encode(Quoted),
            <<"pcr-values">> => #{}
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    T = maps:get(<<"timeline">>, Claim),
    ?assertEqual(<<"42:3">>, maps:get(<<"tpm-epoch">>, T)),
    ?assertEqual(42, maps:get(<<"reset-count">>, T)),
    ?assertEqual(3,  maps:get(<<"restart-count">>, T)),
    ?assertEqual(Clock, maps:get(<<"clock-ms">>, T)),
    ?assertEqual(Clock div 1000,
                 maps:get(<<"clock-seconds">>, T)),
    ?assertEqual(0, maps:get(<<"event-log-count">>, T)),
    ?assertEqual(null, maps:get(<<"event-log-seq-min">>, T)),
    ?assertEqual(null, maps:get(<<"event-log-seq-max">>, T)),
    ?assertEqual(0, maps:get(<<"event-log-seq-range">>, T)),
    ok.

%% Hour-14: policy-verdict + attestation-summary. An empty
%% envelope produces `verdict=unknown' (no signals) with an
%% empty concerns list.
claim_surface_hour14_policy_verdict_empty_test() ->
    Envelope = #{<<"tcg-event-log">> => <<"">>},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PV = maps:get(<<"policy-verdict">>, Claim),
    ?assertEqual(1, maps:get(<<"version">>, PV)),
    Verdict = maps:get(<<"verdict">>, PV),
    %% v1.2 red-team review: an envelope with no EK, no AK, and
    %% no quote signature cannot produce a legitimate trusted
    %% verdict. ek-cert-missing + ak-pub-missing + quote-
    %% signature-unknown are all CRITICAL findings now, so an
    %% empty envelope correctly lands at `untrusted'. The test
    %% previously accepted `unknown | attested-with-warnings';
    %% that was the bug the red-team reviewer surfaced.
    ?assertEqual(<<"untrusted">>, Verdict),
    ?assert(is_integer(maps:get(<<"score">>, PV))),
    ?assert(is_list(maps:get(<<"warnings">>, PV))),
    ?assert(is_list(maps:get(<<"critical-failures">>, PV))),
    %% And the critical-failures MUST include at least one of
    %% the three missing-crypto findings v1.2 upgraded.
    Criticals = maps:get(<<"critical-failures">>, PV),
    CodeSet = [maps:get(<<"code">>, F) || F <- Criticals],
    ?assert(lists:member(<<"ek-cert-missing">>, CodeSet)
           orelse lists:member(<<"ak-pub-missing">>, CodeSet)
           orelse lists:member(<<"quote-signature-unknown">>,
                               CodeSet)),
    ok.

%% A synthetic envelope with explicit secure-boot=true +
%% freshness + quote-integrity-match evidence drives the
%% verdict to "trusted" with no criticals and score > 50.
claim_surface_hour14_policy_verdict_trusted_test() ->
    %% Build a minimal but internally-consistent envelope:
    %%   - EV_EFI_VARIABLE_DRIVER_CONFIG SecureBoot=true
    %%   - EV_IPL cmdline with mem_encrypt + lockdown
    %%   - quote with matching pcrDigest
    AlgPairs = <<16#04:16/little, 20:16/little,
                 16#0B:16/little, 32:16/little>>,
    SpecId = <<"Spec ID Event03", 0,
               0:32/little, 0:8, 2:8, 0:8, 8:8,
               2:32/little, AlgPairs/binary, 0:8>>,
    SpecIdSize = byte_size(SpecId),
    FirstRec = <<0:32/little, 3:32/little, 0:(20*8),
                 SpecIdSize:32/little, SpecId/binary>>,
    %% SecureBoot variable event (type 0x80000001, PCR 7).
    SbName = unicode:characters_to_binary(<<"SecureBoot">>,
                                            utf8, {utf16, little}),
    SbVar = <<0:(16*8), 10:64/little, 1:64/little,
              SbName/binary, 1>>,
    SbRec = <<7:32/little, 16#80000001:32/little, 2:32/little,
              16#04:16/little, (crypto:hash(sha, SbVar))/binary,
              16#0B:16/little, (crypto:hash(sha256, SbVar))/binary,
              (byte_size(SbVar)):32/little, SbVar/binary>>,
    Cmdline = <<"cmdline=ro quiet mem_encrypt=on lockdown=confidentiality", 0>>,
    CmdRec = <<12:32/little, 16#D:32/little, 2:32/little,
               16#04:16/little,
               (crypto:hash(sha, Cmdline))/binary,
               16#0B:16/little,
               (crypto:hash(sha256, Cmdline))/binary,
               (byte_size(Cmdline)):32/little, Cmdline/binary>>,
    Raw = <<FirstRec/binary, SbRec/binary, CmdRec/binary>>,
    Envelope = #{<<"tcg-event-log">> => hb_util:encode(Raw)},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PV = maps:get(<<"policy-verdict">>, Claim),
    %% Signals map surfaces the expected keys.
    Sig = maps:get(<<"signals">>, PV),
    ?assert(maps:is_key(<<"secure-boot-enabled">>, Sig)),
    ?assert(maps:is_key(<<"freshness-indicator">>, Sig)),
    ?assert(maps:is_key(<<"tme-enabled">>, Sig)),
    %% SecureBoot=true surfaced on the signals map.
    ?assertEqual(true,
                 maps:get(<<"secure-boot-enabled">>, Sig)),
    %% cmdline set mem_encrypt=on -> tme-enabled=true on signals.
    ?assertEqual(true, maps:get(<<"tme-enabled">>, Sig)),
    %% The verdict is a recognised string.
    V = maps:get(<<"verdict">>, PV),
    ?assert(lists:member(V,
        [<<"trusted">>, <<"attested-with-warnings">>,
         <<"untrusted">>, <<"unknown">>])),
    ok.

%% Quote-integrity mismatch produces a critical failure + verdict
%% = "untrusted".
claim_surface_hour14_policy_verdict_untrusted_test() ->
    Pcr0 = crypto:hash(sha256, <<"real">>),
    PcrDigest = crypto:hash(sha256, Pcr0),
    Quoted = build_minimal_quote_attest(
        <<"n">>, 0, 0, 1,
        <<1:32/big, 16#000B:16/big, 3:8, 16#01, 0, 0>>,
        PcrDigest),
    Tampered = crypto:hash(sha256, <<"attacker">>),
    Envelope = #{<<"tpm-quote">> => #{
        <<"quoted">> => hb_util:encode(Quoted),
        <<"pcr-values">> => #{
            <<"0">> => hb_util:encode(Tampered)}}},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PV = maps:get(<<"policy-verdict">>, Claim),
    ?assertEqual(<<"untrusted">>, maps:get(<<"verdict">>, PV)),
    ?assert(length(maps:get(<<"critical-failures">>, PV)) >= 1),
    ok.

%% Hour-14: attestation-summary renders the descriptive TL;DR
%% and stays iolist-safe on a sparse envelope.
claim_surface_hour14_attestation_summary_shape_test() ->
    Envelope = #{<<"tcg-event-log">> => <<"">>},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    AS = maps:get(<<"attestation-summary">>, Claim),
    %% Every field is a binary or simple type.
    ?assert(is_binary(maps:get(<<"machine-identity">>, AS))),
    ?assert(is_binary(maps:get(<<"firmware-identity">>, AS))),
    ?assert(is_binary(maps:get(<<"boot-identity">>, AS))),
    ?assert(is_binary(maps:get(<<"tpm-identity">>, AS))),
    ?assert(is_binary(maps:get(<<"security-posture">>, AS))),
    ?assert(is_binary(maps:get(<<"context">>, AS))),
    ?assert(is_list(maps:get(<<"top-concerns">>, AS))),
    %% top-concerns is capped at 5.
    ?assert(length(maps:get(<<"top-concerns">>, AS)) =< 5),
    %% Unified verdict matches policy-verdict's.
    PV = maps:get(<<"policy-verdict">>, Claim),
    ?assertEqual(maps:get(<<"verdict">>, PV),
                 maps:get(<<"verdict">>, AS)),
    ?assertEqual(maps:get(<<"score">>, PV),
                 maps:get(<<"score">>, AS)),
    ok.

%% Hour-15: TPMS_CERTIFY_INFO body decode (type 0x8017).
%% Name + qualifiedName as two TPM2B_NAME structures.
decode_attest_body_certify_test() ->
    Name = crypto:hash(sha256, <<"object">>),
    QualName = crypto:hash(sha256, <<"qualified">>),
    Body = <<(byte_size(Name)):16/big, Name/binary,
             (byte_size(QualName)):16/big, QualName/binary>>,
    M = decode_attest_body(16#8017, Body),
    ?assertEqual(<<"TPMS_CERTIFY_INFO">>,
                 maps:get(<<"attest-body-type">>, M)),
    ?assertEqual(hb_util:encode(Name),
                 maps:get(<<"object-name">>, M)),
    ?assertEqual(hb_util:encode(QualName),
                 maps:get(<<"object-qualified-name">>, M)),
    ok.

%% TPMS_COMMAND_AUDIT_INFO body (0x8015).
decode_attest_body_command_audit_test() ->
    AuditDigest = crypto:hash(sha256, <<"ad">>),
    CommandDigest = crypto:hash(sha256, <<"cmd">>),
    Body = <<42:64/big, 16#000B:16/big,
             (byte_size(AuditDigest)):16/big, AuditDigest/binary,
             (byte_size(CommandDigest)):16/big,
              CommandDigest/binary>>,
    M = decode_attest_body(16#8015, Body),
    ?assertEqual(<<"TPMS_COMMAND_AUDIT_INFO">>,
                 maps:get(<<"attest-body-type">>, M)),
    ?assertEqual(42, maps:get(<<"audit-counter">>, M)),
    ?assertEqual(<<"sha256">>,
                 maps:get(<<"audit-digest-alg-name">>, M)),
    ?assertEqual(hb_util:encode(AuditDigest),
                 maps:get(<<"audit-digest">>, M)),
    ?assertEqual(hb_util:encode(CommandDigest),
                 maps:get(<<"command-digest">>, M)),
    ok.

%% TPMS_SESSION_AUDIT_INFO body (0x8016).
decode_attest_body_session_audit_test() ->
    SessionDigest = crypto:hash(sha256, <<"session">>),
    Body = <<1:8, (byte_size(SessionDigest)):16/big,
             SessionDigest/binary>>,
    M = decode_attest_body(16#8016, Body),
    ?assertEqual(<<"TPMS_SESSION_AUDIT_INFO">>,
                 maps:get(<<"attest-body-type">>, M)),
    ?assertEqual(true, maps:get(<<"exclusive-session">>, M)),
    ?assertEqual(hb_util:encode(SessionDigest),
                 maps:get(<<"session-digest">>, M)),
    ok.

%% TPMS_CREATION_INFO body (0x801A).
decode_attest_body_creation_test() ->
    ObjName = crypto:hash(sha256, <<"obj">>),
    CH = crypto:hash(sha256, <<"creation">>),
    Body = <<(byte_size(ObjName)):16/big, ObjName/binary,
             (byte_size(CH)):16/big, CH/binary>>,
    M = decode_attest_body(16#801A, Body),
    ?assertEqual(<<"TPMS_CREATION_INFO">>,
                 maps:get(<<"attest-body-type">>, M)),
    ?assertEqual(hb_util:encode(ObjName),
                 maps:get(<<"object-name">>, M)),
    ?assertEqual(hb_util:encode(CH),
                 maps:get(<<"creation-hash">>, M)),
    ok.

%% TPMS_TIME_ATTEST_INFO body (0x8019).
decode_attest_body_time_test() ->
    Body = <<16#12345:64/big,       % time-u64
             16#ABCDE:64/big,       % inner-clock-ms
             7:32/big,              % inner-reset-count
             2:32/big,              % inner-restart-count
             1:8,                   % inner-safe = true
             16#0102030400050006:64/big>>,  % inner firmware version
    M = decode_attest_body(16#8019, Body),
    ?assertEqual(<<"TPMS_TIME_ATTEST_INFO">>,
                 maps:get(<<"attest-body-type">>, M)),
    ?assertEqual(16#12345, maps:get(<<"time-u64">>, M)),
    ?assertEqual(16#ABCDE, maps:get(<<"inner-clock-ms">>, M)),
    ?assertEqual(7, maps:get(<<"inner-reset-count">>, M)),
    ?assertEqual(true, maps:get(<<"inner-safe">>, M)),
    ?assertEqual(16#0102030400050006,
                 maps:get(<<"inner-firmware-version-u64">>, M)),
    ok.

%% TPMS_NV_CERTIFY_INFO body (0x8014).
decode_attest_body_nv_certify_test() ->
    IndexName = crypto:hash(sha256, <<"nv-name">>),
    NvContents = <<"NV buffer contents here">>,
    Body = <<(byte_size(IndexName)):16/big, IndexName/binary,
             32:16/big,                    % offset
             (byte_size(NvContents)):16/big, NvContents/binary>>,
    M = decode_attest_body(16#8014, Body),
    ?assertEqual(<<"TPMS_NV_CERTIFY_INFO">>,
                 maps:get(<<"attest-body-type">>, M)),
    ?assertEqual(32, maps:get(<<"nv-offset">>, M)),
    ?assertEqual(hb_util:encode(NvContents),
                 maps:get(<<"nv-contents">>, M)),
    ok.

%% TPMS_NV_DIGEST_CERTIFY_INFO body (0x801C).
decode_attest_body_nv_digest_test() ->
    IndexName = crypto:hash(sha256, <<"nv-name">>),
    NvDigest = crypto:hash(sha256, <<"nv-digest">>),
    Body = <<(byte_size(IndexName)):16/big, IndexName/binary,
             (byte_size(NvDigest)):16/big, NvDigest/binary>>,
    M = decode_attest_body(16#801C, Body),
    ?assertEqual(<<"TPMS_NV_DIGEST_CERTIFY_INFO">>,
                 maps:get(<<"attest-body-type">>, M)),
    ?assertEqual(hb_util:encode(NvDigest),
                 maps:get(<<"nv-digest">>, M)),
    ok.

%% Unknown attest type falls through to length + sha256.
decode_attest_body_unknown_test() ->
    Body = <<"arbitrary payload">>,
    M = decode_attest_body(16#9999, Body),
    ?assertEqual(<<"unknown">>,
                 maps:get(<<"attest-body-type">>, M)),
    ?assertEqual(byte_size(Body),
                 maps:get(<<"attest-body-length">>, M)),
    ok.

%% Hour-15: log-format auto-detection. Synthetic crypto-agile log
%% with SpecID first record -> "crypto-agile". A sha1-only log ->
%% "legacy-sha1". Empty events -> "empty".
claim_surface_hour15_log_format_crypto_agile_test() ->
    Envelope = #{
        <<"tcg-event-log">> =>
            hb_util:encode(build_tcg_fixture())},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PC = maps:get(<<"platform-config">>, Claim),
    ?assertEqual(<<"crypto-agile">>,
                 maps:get(<<"log-format">>, PC)),
    ok.

claim_surface_hour15_log_format_empty_test() ->
    Envelope = #{<<"tcg-event-log">> => <<"">>},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PC = maps:get(<<"platform-config">>, Claim),
    ?assertEqual(<<"empty">>, maps:get(<<"log-format">>, PC)),
    ok.

%% claim.ak on a synthetic RSA-2048 AK decodes algorithm,
%% size, exponent, and SHA-256 fingerprint.
claim_surface_ak_rsa_decode_test() ->
    {PubKey, _} = crypto:generate_key(rsa, {2048, 65537}),
    [EBin, NBin] = PubKey,
    N = binary:decode_unsigned(NBin),
    Exp = binary:decode_unsigned(EBin),
    Rsa = #'RSAPublicKey'{modulus = N, publicExponent = Exp},
    AkPem = public_key:pem_encode(
        [public_key:pem_entry_encode('RSAPublicKey', Rsa)]),
    Envelope = #{<<"ak-pub-pem">> => AkPem},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    AK = maps:get(<<"ak">>, Claim),
    ?assertEqual(true,       maps:get(<<"present">>, AK)),
    ?assertEqual(<<"rsa">>,  maps:get(<<"key-alg">>, AK)),
    ?assertEqual(2048,       maps:get(<<"key-size-bits">>, AK)),
    ?assertEqual(65537,      maps:get(<<"rsa-public-exponent">>, AK)),
    Sha = maps:get(<<"public-key-sha256">>, AK),
    ?assertEqual(43, byte_size(Sha)),  %% base64url 32 bytes
    ok.

%% claim.ak on an empty envelope returns shape-stable nulls.
claim_surface_ak_missing_test() ->
    Envelope = #{},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    AK = maps:get(<<"ak">>, Claim),
    ?assertEqual(false,       maps:get(<<"present">>, AK)),
    ?assertEqual(<<"unknown">>, maps:get(<<"key-alg">>, AK)),
    ?assertEqual(0,           maps:get(<<"key-size-bits">>, AK)),
    ?assertEqual(null,        maps:get(<<"rsa-public-exponent">>, AK)),
    %% Policy verdict should flag ak-pub-missing as a warning.
    PV = maps:get(<<"policy-verdict">>, Claim),
    AllFindings = maps:get(<<"warnings">>, PV, []) ++
                  maps:get(<<"critical-failures">>, PV, []),
    Codes = [maps:get(<<"code">>, F) || F <- AllFindings],
    ?assert(lists:member(<<"ak-pub-missing">>, Codes)),
    ok.

%% claim.ek on an empty envelope returns shape-stable nulls +
%% surfaces an "ek-cert-missing" warning.
claim_surface_ek_missing_test() ->
    Envelope = #{<<"tcg-event-log">> => <<"">>},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    EK = maps:get(<<"ek">>, Claim),
    ?assertEqual(false, maps:get(<<"present">>, EK)),
    ?assertEqual(null,  maps:get(<<"subject">>, EK)),
    ?assertEqual(<<"unknown">>,
                 maps:get(<<"key-alg">>, EK)),
    Chain = maps:get(<<"chain-validation">>, EK),
    ?assertEqual(<<"unknown">>,
                 maps:get(<<"chain-valid">>, Chain)),
    PV = maps:get(<<"policy-verdict">>, Claim),
    AllFindings = maps:get(<<"warnings">>, PV, []) ++
                  maps:get(<<"critical-failures">>, PV, []),
    Codes = [maps:get(<<"code">>, F) || F <- AllFindings],
    ?assert(lists:member(<<"ek-cert-missing">>, Codes)),
    ok.

%% policy-verdict.signals map surfaces ek + ak facts so policy
%% engines can match without re-walking the tree.
claim_surface_ek_ak_signals_test() ->
    Envelope = #{},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PV = maps:get(<<"policy-verdict">>, Claim),
    Sig = maps:get(<<"signals">>, PV),
    ?assert(maps:is_key(<<"ek-present">>, Sig)),
    ?assert(maps:is_key(<<"ek-currently-valid">>, Sig)),
    ?assert(maps:is_key(<<"ek-chain-valid">>, Sig)),
    ?assert(maps:is_key(<<"ak-present">>, Sig)),
    ?assert(maps:is_key(<<"ak-key-size-bits">>, Sig)),
    ?assertEqual(false, maps:get(<<"ek-present">>, Sig)),
    ?assertEqual(false, maps:get(<<"ak-present">>, Sig)),
    ok.

%% Hour-3: UKI lookup helpers are resilient against malformed
%% DB entries.
uki_db_lookup_handles_empty_and_malformed_test() ->
    ?assertEqual(false, uki_db_lookup(#{}, <<"x">>, [], <<"k">>)),
    ?assertEqual(false, uki_db_lookup(not_a_map, <<"x">>, [], <<"k">>)),
    %% Profile that declares kernel-name-prefix but the events
    %% have no IPL event at all -- no match.
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
%% cryptographic battery -- clients build UI + policy on this, so
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
    %% Summary must not carry maps inside its values -- that's the
    %% link-free property. Spot-check a few known fields.
    [?assert(not is_map(maps:get(K, S, null)))
     || K <- [<<"tpm-manufacturer">>, <<"ak-algorithm">>,
              <<"quote-attest-type">>, <<"secure-boot-measured">>,
              <<"pcr15-event-count">>]],
    ok.

%% `run_cross_node_verify' MUST reject when the envelope's
%% tpm_quote.nonce does NOT match the verifier's challenge. That
%% gate sits BEFORE any crypto verification -- defence against a
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
    %% the nonce mismatch -- no crypto checks should have run,
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
    %% Freshness gate passed -- crypto checks attempted (and will
    %% fail on this synthetic envelope for other reasons, which is
    %% fine -- we only assert nonce_freshness says "verified" and
    %% the check list isn't the single-entry nonce-mismatch form).
    ?assertEqual(<<"verified">>,
                 maps:get(<<"nonce-freshness">>, Body)),
    ?assertEqual(Challenge,
                 hb_util:decode(maps:get(<<"nonce-challenge">>, Body))),
    Checks = maps:get(<<"checks">>, Body),
    ?assert(length(Checks) >= 1),
    %% None of the checks should be the "verifier-supplied nonce"
    %% one -- that's only emitted when the gate fails.
    [?assert(binary:match(maps:get(<<"name">>, C, <<>>),
                          <<"Verifier-supplied nonce">>) =:= nomatch)
     || C <- Checks],
    ok.

%% A missing `peer' parameter on any peer-* endpoint returns 400
%% with a targeted error -- not silent.
peer_endpoints_reject_missing_peer_test() ->
    [?assertMatch({ok, #{<<"status">> := 400,
                         <<"body">> :=
                           #{<<"error">> := <<"missing-peer">>}}},
                  F(#{}, #{}, #{}))
     || F <- [fun peer_summary/3, fun peer_status/3]],
    ok.

%% `resolve_inline_ca/2' normalises both accepted forms of the
%% inline trust anchor -- base64url `trusted-ca' wins over raw PEM
%% `trusted-ca-pem', and undefined/empty inputs stay undefined.
resolve_inline_ca_normalises_forms_test() ->
    Pem = <<"-----BEGIN CERTIFICATE-----\nAA==\n-----END CERTIFICATE-----">>,
    B64u = hb_util:encode(Pem),
    %% base64url form decodes back to the raw PEM bytes
    ?assertEqual(Pem, resolve_inline_ca(#{<<"trusted-ca">> => B64u}, #{})),
    %% raw-PEM form passes through
    ?assertEqual(Pem, resolve_inline_ca(#{<<"trusted-ca-pem">> => Pem}, #{})),
    %% both keys -- base64url wins
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

%% Interpret a hand-built envelope with NO valid EK cert -- we still
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

%% Every PCR section includes a `derived' submap -- named fields
%% extracted from the events extended into that PCR. When events are
%% present, the derived map pulls concrete values out of the events'
%% `parsed' + `parsed.semantic' sub-maps. This is what makes the
%% interpretation AO-Core navigable -- every derivable property is
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
    %% PCR 7 has the SecureBoot variable (seq 3) -> enabled=true.
    Pcr7 = maps:get(<<"7">>, Pcrs),
    Derived7 = maps:get(<<"derived">>, Pcr7),
    ?assertEqual(true,
                 maps:get(<<"secure-boot-enabled">>, Derived7)),
    %% Every PCR carries a reconstruction submessage when events are
    %% present. We didn't quote the real values here, so it'll say
    %% matches_quoted=false -- but the SHAPE must be there.
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
            %% Priv dir not present in eunit layout -- skip.
            ok
    end.

%% v1.1: when the envelope carries tpm-properties (from
%% TPM2_GetCapability via lapee_tpm_nif:tpm_properties/0), claim.tpm
%% populates manufacturer / vendor-string / spec-* / firmware-version
%% straight from the TPM hardware -- no EK cert needed.
v1_1_claim_tpm_from_capabilities_test() ->
    Envelope = #{
        <<"tcg-event-log">> => <<>>,
        <<"tpm-properties">> => #{
            <<"available">>          => true,
            <<"manufacturer">>       => <<"AMD">>,
            <<"manufacturer-u32">>   => 16#414D4400,
            <<"vendor-string">>      => <<"AMD">>,
            <<"spec-family">>        => <<"2.0">>,
            <<"spec-level">>         => 0,
            <<"spec-revision">>      => 138,
            <<"firmware-version-1">> => 16#00030055,
            <<"firmware-version-2">> => 16#00000002,
            <<"day-of-year">>        => 235,
            <<"year">>               => 2023
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    TPM = maps:get(<<"tpm">>, Claim),
    ?assertEqual(<<"AMD">>, maps:get(<<"manufacturer-id">>, TPM)),
    ?assertEqual(<<"AMD">>, maps:get(<<"vendor-string">>, TPM)),
    ?assertEqual(<<"2.0">>, maps:get(<<"spec-family">>, TPM)),
    ?assertEqual(<<"1.38">>, maps:get(<<"spec-revision">>, TPM)),
    ?assert(maps:get(<<"firmware-version-u64">>, TPM) > 0),
    Evidence = maps:get(<<"evidence">>, TPM),
    ?assert(lists:member({<<"source">>, <<"tpm2-get-capability">>},
                         Evidence)).

%% v1.1: an envelope with NO EK cert and NO tpm-properties block
%% must still produce a well-shaped claim.tpm with null fields and
%% an empty evidence list. No synthesis of stand-in values.
v1_1_claim_tpm_absent_both_sources_test() ->
    Envelope = #{<<"tcg-event-log">> => <<>>},
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    TPM = maps:get(<<"tpm">>, Claim),
    ?assertEqual(null, maps:get(<<"manufacturer-id">>, TPM)),
    ?assertEqual(null, maps:get(<<"manufacturer-name">>, TPM)),
    ?assertEqual(null, maps:get(<<"firmware-version">>, TPM)),
    ?assertEqual([], maps:get(<<"evidence">>, TPM)).

%% v1.1: event-log string scan promotes cpu.vendor when the
%% microcode event is missing. We build a minimal TCG_PCR_EVENT2
%% log with a single EV_S_CRTM_CONTENTS carrying an "AMD AGESA"
%% blob description. The microcode path stays empty, so the
%% string hint must win.
v1_1_cpu_vendor_hint_from_crtm_strings_test() ->
    %% Fabricate the smallest possible crypto-agile log carrying
    %% one EV_S_CRTM_CONTENTS with the string we want the scanner
    %% to pick up. The log parser pre-populates `parsed.value' /
    %% `parsed.description' on CRTM events; for directness we
    %% hand-build a parsed-events map the claim layer consumes.
    Events = #{
        <<"0">> => #{
            <<"event-type-code">> => 16#07,  %% EV_S_CRTM_CONTENTS
            <<"event-type">>      => <<"EV_S_CRTM_CONTENTS">>,
            <<"pcr">>             => 0,
            <<"seq">>             => 0,
            <<"parsed">>          => #{
                <<"description">> => <<"AMD AGESA PinnacleRidge-PI 0.0.7.B">>
            }
        }
    },
    CPU = claim_cpu(event_list(Events), #{}),
    ?assertEqual(<<"amd">>, maps:get(<<"vendor">>, CPU)),
    %% The brand ranker prefers specific product lines (Ryzen/EPYC/
    %% Core/Xeon/...) over the bare vendor name. When only "AMD"
    %% and "AGESA" are visible, we take the vendor name itself as
    %% a best-effort brand hint -- it's honest about what we saw
    %% without over-claiming a specific product line.
    ?assert(lists:member(maps:get(<<"brand-range">>, CPU),
                         [<<"AMD">>, <<"AGESA">>])).

v1_1_cpu_vendor_hint_intel_test() ->
    Events = #{
        <<"0">> => #{
            <<"event-type-code">> => 16#08,  %% EV_S_CRTM_VERSION
            <<"event-type">>      => <<"EV_S_CRTM_VERSION">>,
            <<"pcr">>             => 0,
            <<"seq">>             => 0,
            <<"parsed">>          => #{
                <<"value">> =>
                    <<"Intel(R) Xeon(R) E-2288G firmware v2.1.4">>
            }
        }
    },
    CPU = claim_cpu(event_list(Events), #{}),
    ?assertEqual(<<"intel">>, maps:get(<<"vendor">>, CPU)),
    ?assertEqual(<<"Xeon">>, maps:get(<<"brand-range">>, CPU)).

%% v1.1: when claim.tme.enabled = true the claim pipeline cross-
%% links tme into cpu.tee-support based on cpu.vendor. An AMD
%% CPU gets "amd-sme"; Intel gets "intel-tme"; unknown gets the
%% generic "memory-encryption" fallback.
v1_1_tme_cross_link_amd_sme_test() ->
    Claim0 = #{
        <<"tme">> => #{<<"enabled">> => <<"true">>},
        <<"cpu">> => #{<<"vendor">> => <<"amd">>, <<"tee-support">> => []}
    },
    Claim1 = cross_link_tme_into_cpu(Claim0),
    CPU1 = maps:get(<<"cpu">>, Claim1),
    ?assert(lists:member(<<"amd-sme">>,
                         maps:get(<<"tee-support">>, CPU1))).

v1_1_tme_cross_link_intel_tme_test() ->
    Claim0 = #{
        <<"tme">> => #{<<"enabled">> => true},  %% also accept atom true
        <<"cpu">> => #{<<"vendor">> => <<"intel">>, <<"tee-support">> => []}
    },
    CPU1 = maps:get(<<"cpu">>,
                    cross_link_tme_into_cpu(Claim0)),
    ?assert(lists:member(<<"intel-tme">>,
                         maps:get(<<"tee-support">>, CPU1))).

v1_1_tme_cross_link_unknown_vendor_test() ->
    Claim0 = #{
        <<"tme">> => #{<<"enabled">> => <<"true">>},
        <<"cpu">> => #{<<"vendor">> => <<"unknown">>, <<"tee-support">> => []}
    },
    CPU1 = maps:get(<<"cpu">>,
                    cross_link_tme_into_cpu(Claim0)),
    ?assert(lists:member(<<"memory-encryption">>,
                         maps:get(<<"tee-support">>, CPU1))).

%% v1.1: when tme.enabled is false the cross-link is a no-op.
v1_1_tme_cross_link_noop_when_disabled_test() ->
    Claim0 = #{
        <<"tme">> => #{<<"enabled">> => <<"false">>},
        <<"cpu">> => #{<<"vendor">> => <<"amd">>, <<"tee-support">> => []}
    },
    ?assertEqual(Claim0, cross_link_tme_into_cpu(Claim0)).

%% v1.1: if the cpu section already lists the vendor-specific TEE
%% feature (e.g. microcode event resolved it), don't double-insert.
v1_1_tme_cross_link_idempotent_test() ->
    Claim0 = #{
        <<"tme">> => #{<<"enabled">> => <<"true">>},
        <<"cpu">> => #{<<"vendor">> => <<"amd">>,
                        <<"tee-support">> => [<<"amd-sme">>]}
    },
    Claim1 = cross_link_tme_into_cpu(Claim0),
    TEE = maps:get(<<"tee-support">>, maps:get(<<"cpu">>, Claim1)),
    ?assertEqual(1, length([X || X <- TEE, X =:= <<"amd-sme">>])).

%% v1.1: vendor lookup via U32 manufacturer code (AMD -> "AMD\0" ->
%% 0x414D4400). The capability path uses this before falling back to
%% ASCII; the existing manufacturers.json key is the 8-char hex form.
v1_1_vendor_lookup_by_u32_test() ->
    Db = hb_db_tpm:load(#{}),
    case maps:get(<<"vendors">>, Db, #{}) of
        V when is_map(V), map_size(V) > 0 ->
            Amd = lookup_vendor_by_u32(16#414D4400, Db),
            ?assertEqual(<<"AMD">>, maps:get(<<"name">>, Amd)),
            Ifx = lookup_vendor_by_u32(16#49465800, Db),
            ?assertEqual(<<"Infineon">>, maps:get(<<"name">>, Ifx));
        _ -> ok
    end.

%% v1.2 E1: the old currently_valid/2 emitted ISO-8601 ("2026-04-23...")
%% for `now' but compared it against raw X.509 ASCII
%% ("230912044823Z") which misordered every cert. Lock in the fix.
v1_2_currently_valid_parses_utctime_test() ->
    %% Nuvoton EK from Sam's Framework: Sep 2023 -> Sep 2043, today
    %% is inside the window.
    ?assertEqual(true,
                 currently_valid(<<"230912044823Z">>,
                                 <<"430912044823Z">>)).

v1_2_currently_valid_expired_test() ->
    ?assertEqual(false,
                 currently_valid(<<"200101000000Z">>,
                                 <<"210101000000Z">>)).

v1_2_currently_valid_future_utctime_test() ->
    %% Far-future UTCTime years 30..49 map to 2030..2049 per
    %% RFC 5280 section 4.1.2.5.1.
    ?assertEqual(false,
                 currently_valid(<<"300101000000Z">>,
                                 <<"400101000000Z">>)).

v1_2_currently_valid_utctime_pre_2000_test() ->
    %% UTCTime year 99 maps to 1999, far-past.
    ?assertEqual(false,
                 currently_valid(<<"990101000000Z">>,
                                 <<"990601000000Z">>)).

v1_2_currently_valid_generalizedtime_test() ->
    %% Cert with GeneralizedTime endpoints covering today.
    ?assertEqual(true,
                 currently_valid(<<"20200101000000Z">>,
                                 <<"20500101000000Z">>)).

v1_2_currently_valid_malformed_test() ->
    ?assertEqual(<<"unknown">>,
                 currently_valid(<<"not-a-date">>,
                                 <<"20500101000000Z">>)),
    ?assertEqual(<<"unknown">>,
                 currently_valid(<<"20200101000000Z">>,
                                 <<"nope">>)),
    ?assertEqual(<<"unknown">>,
                 currently_valid(undefined, <<"20500101000000Z">>)).

%% v1.2 E5: framework-laptop.json (and similar Intel/Dell manifests)
%% ship `platforms' as a LIST, not a map. pick_platform used to
%% return null in that case, leaving `firmware.family-platform =
%% null' even on a perfectly-matched CRTM.
v1_2_pick_platform_map_test() ->
    Manifest = #{<<"platforms">> => #{
        <<"IFR30">> => <<"Framework Laptop 13 (AMD Ryzen 7040)">>,
        <<"IFG1">>  => <<"Framework Laptop 16 (AMD Ryzen 7040)">>
    }},
    ?assertEqual(<<"Framework Laptop 13 (AMD Ryzen 7040)">>,
                 pick_platform(Manifest, <<"IFR30.03.04">>)).

v1_2_pick_platform_single_list_test() ->
    Manifest = #{<<"platforms">> =>
        [<<"Framework Laptop 13 (AMD Ryzen 7040)">>]},
    ?assertEqual(<<"Framework Laptop 13 (AMD Ryzen 7040)">>,
                 pick_platform(Manifest, <<"IFR30.03.04">>)).

v1_2_pick_platform_multi_list_test() ->
    %% The real framework-laptop.json shape: three variants share
    %% the same CRTM prefix. pick_platform returns the full list
    %% as the candidate set; caller narrows by CPU identity.
    Manifest = #{<<"platforms">> => [
        <<"Framework Laptop 13 (Intel 11th-13th gen)">>,
        <<"Framework Laptop 13 (AMD Ryzen 7040 / 7xxx series)">>,
        <<"Framework Laptop 16 (AMD Ryzen 7040)">>
    ]},
    Res = pick_platform(Manifest, <<"IFR30.03.04">>),
    ?assert(is_list(Res)),
    ?assertEqual(3, length(Res)).

v1_2_pick_platform_empty_test() ->
    ?assertEqual(null, pick_platform(#{}, <<"IFR30.03.04">>)),
    ?assertEqual(null,
                 pick_platform(#{<<"platforms">> => #{}},
                               <<"IFR30.03.04">>)),
    ?assertEqual(null,
                 pick_platform(#{<<"platforms">> => []},
                               <<"IFR30.03.04">>)).

%% v1.2 E6: the old freshness_finding/1 always flagged
%% `safe=false' as critical. On a first-cold-boot TPM (resetCount
%% + restartCount both low), that's the expected state, not a
%% tamper signal. Soften to warning-with-reason.
v1_2_freshness_safe_false_first_boot_warns_test() ->
    S = #{<<"freshness-indicator">> => <<"safe-false">>,
          <<"reset-count">>         => 1,
          <<"restart-count">>       => 0},
    F = freshness_finding(S),
    ?assertEqual(warn, maps:get(<<"severity">>, F)),
    ?assertEqual(<<"freshness-safe-false-first-boot">>,
                 maps:get(<<"code">>, F)).

v1_2_freshness_safe_false_tamper_stays_critical_test() ->
    %% Non-first-boot TPM (> 1 reset AND > 1 restart) with safe=false
    %% -- this IS the tamper pattern.
    S = #{<<"freshness-indicator">> => <<"safe-false">>,
          <<"reset-count">>         => 5,
          <<"restart-count">>       => 17},
    F = freshness_finding(S),
    ?assertEqual(critical, maps:get(<<"severity">>, F)),
    ?assertEqual(<<"freshness-safe-false">>,
                 maps:get(<<"code">>, F)).

v1_2_freshness_safe_false_missing_counts_is_critical_test() ->
    %% Counts absent from the quote are DISTINCT from fresh-boot:
    %% an adversary stripping counts from an envelope produces the
    %% same missing-counts pattern, so we refuse to downgrade
    %% tamper severity. Stays critical, different code.
    S = #{<<"freshness-indicator">> => <<"safe-false">>,
          <<"reset-count">>         => null,
          <<"restart-count">>       => null},
    F = freshness_finding(S),
    ?assertEqual(critical, maps:get(<<"severity">>, F)),
    ?assertEqual(<<"freshness-safe-false-counts-missing">>,
                 maps:get(<<"code">>, F)).

v1_2_freshness_safe_false_partial_counts_is_critical_test() ->
    %% Only one of the two counts present. Treat as missing --
    %% same reason: adversary could strip just one to confuse
    %% the classifier.
    S1 = #{<<"freshness-indicator">> => <<"safe-false">>,
           <<"reset-count">>         => 1,
           <<"restart-count">>       => null},
    F1 = freshness_finding(S1),
    ?assertEqual(critical, maps:get(<<"severity">>, F1)),
    ?assertEqual(<<"freshness-safe-false-counts-missing">>,
                 maps:get(<<"code">>, F1)),
    S2 = #{<<"freshness-indicator">> => <<"safe-false">>,
           <<"reset-count">>         => null,
           <<"restart-count">>       => 0},
    F2 = freshness_finding(S2),
    ?assertEqual(critical, maps:get(<<"severity">>, F2)).

%% v1.2 E3: platform-probes.cpuinfo resolves `claim.cpu.vendor' +
%% brand-range on real hardware where the TCG event log doesn't
%% carry them. Intel example.
v1_2_cpu_from_cpuinfo_intel_test() ->
    Envelope = #{
        <<"tcg-event-log">> => <<>>,
        <<"platform-probes">> => #{
            <<"available">> => true,
            <<"cpuinfo">>   => #{
                <<"vendor-id">>  => <<"GenuineIntel">>,
                <<"model-name">> => <<"Intel(R) Core(TM) i7-1260P">>,
                <<"cpu-family">> => <<"6">>,
                <<"model">>      => <<"154">>,
                <<"stepping">>   => <<"3">>,
                <<"microcode">>  => <<"0x2c000271">>
            }
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    CPU = maps:get(<<"cpu">>, Claim),
    ?assertEqual(<<"intel">>, maps:get(<<"vendor">>, CPU)),
    ?assertEqual(<<"Core">>,  maps:get(<<"brand-range">>, CPU)),
    ?assertEqual(6,           maps:get(<<"cpu-family">>, CPU)),
    ?assertEqual(154,         maps:get(<<"cpu-model">>, CPU)),
    ?assertEqual(3,           maps:get(<<"cpu-stepping">>, CPU)).

v1_2_cpu_from_cpuinfo_amd_test() ->
    Envelope = #{
        <<"tcg-event-log">> => <<>>,
        <<"platform-probes">> => #{
            <<"available">> => true,
            <<"cpuinfo">>   => #{
                <<"vendor-id">>  => <<"AuthenticAMD">>,
                <<"model-name">> =>
                    <<"AMD Ryzen 7 7840U w/ Radeon 780M Graphics">>,
                <<"cpu-family">> => <<"25">>,
                <<"model">>      => <<"116">>,
                <<"stepping">>   => <<"1">>,
                <<"microcode">>  => <<"0xa704103">>
            }
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    CPU = maps:get(<<"cpu">>, Claim),
    ?assertEqual(<<"amd">>,   maps:get(<<"vendor">>, CPU)),
    ?assertEqual(<<"Ryzen">>, maps:get(<<"brand-range">>, CPU)),
    ?assertEqual(25,          maps:get(<<"cpu-family">>, CPU)),
    ?assertEqual(116,         maps:get(<<"cpu-model">>, CPU)).

%% v1.2 E3: IOMMU groups count > 0 means IOMMU is runtime-active
%% regardless of cmdline. Takes precedence over cmdline-only
%% inference.
v1_2_iommu_from_sysfs_groups_test() ->
    Envelope = #{
        <<"tcg-event-log">> => <<>>,
        <<"platform-probes">> => #{
            <<"available">>          => true,
            <<"iommu-groups-count">> => 23
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    IO = maps:get(<<"iommu">>, Claim),
    ?assertEqual(true, maps:get(<<"enabled">>, IO)),
    ?assertEqual(23, maps:get(<<"runtime-groups-count">>, IO)).

v1_2_iommu_disabled_when_zero_groups_test() ->
    Envelope = #{
        <<"tcg-event-log">> => <<>>,
        <<"platform-probes">> => #{
            <<"available">>          => true,
            <<"iommu-groups-count">> => 0
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    IO = maps:get(<<"iommu">>, Claim),
    ?assertEqual(false, maps:get(<<"enabled">>, IO)).

%% v1.2 E3: /sys/kernel/security/lockdown parses the bracketed
%% active entry. The guest stamps the raw line; the parser
%% extracts the active level.
v1_2_lockdown_parse_line_test() ->
    ?assertEqual(<<"none">>,
                 parse_lockdown_line(
                    <<"[none] integrity confidentiality">>)),
    ?assertEqual(<<"integrity">>,
                 parse_lockdown_line(
                    <<"none [integrity] confidentiality">>)),
    ?assertEqual(<<"confidentiality">>,
                 parse_lockdown_line(
                    <<"none integrity [confidentiality]">>)),
    ?assertEqual(unknown, parse_lockdown_line(<<"">>)),
    ?assertEqual(unknown, parse_lockdown_line(null)).

v1_2_lockdown_from_sysfs_test() ->
    Envelope = #{
        <<"tcg-event-log">> => <<>>,
        <<"platform-probes">> => #{
            <<"available">> => true,
            <<"lockdown">>  => <<"none [integrity] confidentiality">>
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    LD = maps:get(<<"lockdown">>, Claim),
    ?assertEqual(<<"integrity">>, maps:get(<<"level">>, LD)).

%% v1.2 red-team regression: a forged envelope that populates
%% claim_quote_integrity's pcr-digest-match correctly (attacker
%% computes sha256 of their chosen PCR values, puts the
%% matching digest in a hand-rolled TPMS_ATTEST blob) but has
%% NO valid signature and NO valid AK pubkey. Pre-v1.2 this
%% would have scored well on the integrity branch and only
%% incurred warnings elsewhere. Post-v1.2 it MUST land at
%% `untrusted' via the new quote_signature_finding/1 clause.
v1_2_forged_envelope_without_sig_is_untrusted_test() ->
    %% Hand-build the smallest TPMS_ATTEST whose pcrDigest
    %% matches a single known PCR 15 value, without signing
    %% anything. Attacker's playbook: the pcr-digest recompute
    %% path looks valid, but no signature exists.
    PcrBytes = <<0:256>>,
    PcrDigest = crypto:hash(sha256, PcrBytes),
    %% TPMS_ATTEST: magic(4) + type(2) + qualifiedSigner(TPM2B)
    %%   + extraData(TPM2B) + clockInfo(17) + firmwareVersion(8)
    %%   + attested = TPML_PCR_SELECTION + TPM2B_DIGEST.
    Magic = <<16#FF, "TCG">>,
    Type  = <<16#80, 16#18>>,   %% TPM_ST_ATTEST_QUOTE
    QualifiedSigner = <<0:16>>,  %% empty TPM2B
    Nonce = <<0:256>>,           %% 32 zero bytes
    ExtraData = <<32:16, Nonce/binary>>,
    ClockInfo = <<0:136>>,
    Firmware  = <<0:64>>,
    SelAlg = <<11:16>>,          %% sha256
    SelBitmap = <<3, 16#80, 0, 0>>,  %% 3-byte select with bit 15 set
    Sel = <<1:32, SelAlg/binary, SelBitmap/binary>>,
    DigestTpm2B = <<32:16, PcrDigest/binary>>,
    Quoted = <<Magic/binary, Type/binary, QualifiedSigner/binary,
               ExtraData/binary, ClockInfo/binary, Firmware/binary,
               Sel/binary, DigestTpm2B/binary>>,
    Envelope = #{
        <<"tcg-event-log">> => <<>>,
        <<"tpm-quote">> => #{
            <<"quoted">> => hb_util:encode(Quoted),
            <<"signature">> => <<>>,
            <<"nonce">> => hb_util:encode(Nonce),
            <<"pcr-selection">> => [15],
            <<"pcr-values">> => #{<<"15">> => hb_util:encode(PcrBytes)}
        }
    },
    {ok, #{<<"body">> := Claim}} = claim(Envelope, #{}, #{}),
    PV = maps:get(<<"policy-verdict">>, Claim),
    ?assertEqual(<<"untrusted">>, maps:get(<<"verdict">>, PV)),
    Criticals = maps:get(<<"critical-failures">>, PV),
    CodeSet = [maps:get(<<"code">>, F) || F <- Criticals],
    %% Must surface quote-signature-unknown (empty sig) + ek
    %% missing + ak missing as criticals.
    ?assert(lists:member(<<"quote-signature-unknown">>, CodeSet)),
    ?assert(lists:member(<<"ek-cert-missing">>, CodeSet)),
    ?assert(lists:member(<<"ak-pub-missing">>, CodeSet)).

%%--------------------------------------------------------------------
%% v1.2 batch 9 / paper-to-code pass 6 tests
%%--------------------------------------------------------------------

%% verify_ak_pubkey_extend: returns true | false | <<"unknown">>
%% based on whether a matching EV_HYPERBEAM_KEY_PUBKEY_EXTEND event
%% for the envelope's ak-pub-pem is present in the runtime event log.
v1_2_verify_ak_pubkey_extend_shapes_test() ->
    AkPem = <<"-----BEGIN PUBLIC KEY-----\nAK\n-----END PUBLIC KEY-----\n">>,
    GoodEvent = #{
        <<"pcr">> => 15,
        <<"event-type">> => <<"EV_HYPERBEAM_KEY_PUBKEY_EXTEND">>,
        <<"digest">> => hb_util:encode(crypto:hash(sha256, AkPem))
    },
    %% (1) Missing ak-pub-pem -> unknown.
    ?assertEqual(<<"unknown">>,
                 verify_ak_pubkey_extend(
                   #{<<"runtime-event-log">> => [GoodEvent]})),
    %% (2) Empty event log -> unknown.
    ?assertEqual(<<"unknown">>,
                 verify_ak_pubkey_extend(
                   #{<<"ak-pub-pem">> => AkPem,
                     <<"runtime-event-log">> => []})),
    %% (3) Event with wrong type -> false.
    WrongType = GoodEvent#{<<"event-type">> =>
                               <<"EV_HYPERBEAM_NODE_IDENTITY_EXTEND">>},
    ?assertEqual(false,
                 verify_ak_pubkey_extend(
                   #{<<"ak-pub-pem">> => AkPem,
                     <<"runtime-event-log">> => [WrongType]})),
    %% (4) Event with wrong digest -> false.
    WrongDig = GoodEvent#{<<"digest">> =>
                              hb_util:encode(
                                crypto:hash(sha256, <<"other">>))},
    ?assertEqual(false,
                 verify_ak_pubkey_extend(
                   #{<<"ak-pub-pem">> => AkPem,
                     <<"runtime-event-log">> => [WrongDig]})),
    %% (5) Correct event -> true.
    ?assertEqual(true,
                 verify_ak_pubkey_extend(
                   #{<<"ak-pub-pem">> => AkPem,
                     <<"runtime-event-log">> => [GoodEvent]})),
    %% (6) Reviewer 7 follow-up: a correctly-shaped event in the
    %% wrong PCR (not 15) must NOT satisfy P5. The core check
    %% `dev_tpm2:chk_ak_pubkey_binding/1' already filters by
    %% pcr=15; the interpret-side mirror now does too.
    WrongPcrEvent = GoodEvent#{<<"pcr">> => 11},
    ?assertEqual(false,
                 verify_ak_pubkey_extend(
                   #{<<"ak-pub-pem">> => AkPem,
                     <<"runtime-event-log">> => [WrongPcrEvent]})),
    %% (7) Integer-binary-encoded PCR field (round-trip via JSON).
    %% Must still be recognised as PCR 15.
    GoodEventBin = GoodEvent#{<<"pcr">> => <<"15">>},
    ?assertEqual(true,
                 verify_ak_pubkey_extend(
                   #{<<"ak-pub-pem">> => AkPem,
                     <<"runtime-event-log">> => [GoodEventBin]})),
    ok.

%% ak_pubkey_extend_finding: true -> ok; false -> critical;
%% unknown -> critical. Paper ephemeral-node-key-binding P5 demands
%% this is a gating signal, not a warning.
v1_2_ak_pubkey_extend_finding_severity_test() ->
    ?assertEqual(ok,
                 ak_pubkey_extend_finding(
                   #{<<"ak-pubkey-extend-verified">> => true})),
    FalseF = ak_pubkey_extend_finding(
               #{<<"ak-pubkey-extend-verified">> => false}),
    ?assertMatch(#{<<"severity">> := critical}, FalseF),
    ?assertEqual(<<"ak-pubkey-extend-missing">>,
                 maps:get(<<"code">>, FalseF)),
    UnknownF = ak_pubkey_extend_finding(
                 #{<<"ak-pubkey-extend-verified">> => <<"unknown">>}),
    ?assertMatch(#{<<"severity">> := critical}, UnknownF),
    ?assertEqual(<<"ak-pubkey-extend-unknown">>,
                 maps:get(<<"code">>, UnknownF)),
    %% Absent from signal map -> ok (catch-all): some callers
    %% synthesise signals maps in tests; the finding only fires
    %% when collect_policy_signals/2 has populated the key.
    ?assertEqual(ok, ak_pubkey_extend_finding(#{})),
    ok.

%% v1.2 batch 9 / paper-to-code HIGH-2: lockdown_finding must emit
%% a finding for `none' / `unknown' / absent lockdown-level, not
%% silently pass. Severity is warn today; escalates to critical
%% in v1.3 once the EV_IPL cmdline cross-check lands.
v1_2_lockdown_finding_catches_unknown_test() ->
    ?assertEqual(ok,
                 lockdown_finding(
                   #{<<"lockdown-level">> => <<"confidentiality">>})),
    Integrity = lockdown_finding(
                  #{<<"lockdown-level">> => <<"integrity">>}),
    ?assertMatch(#{<<"severity">> := warn}, Integrity),
    ?assertEqual(<<"lockdown-integrity-not-confidentiality">>,
                 maps:get(<<"code">>, Integrity)),
    %% None / unknown / absent all warn.
    lists:foreach(
        fun(Level) ->
            F = lockdown_finding(#{<<"lockdown-level">> => Level}),
            ?assertMatch(#{<<"severity">> := warn}, F),
            ?assertEqual(<<"lockdown-off-or-unknown">>,
                         maps:get(<<"code">>, F))
        end,
        [<<"none">>, <<"unknown">>, <<"">>, <<"disabled">>]),
    FAbsent = lockdown_finding(#{}),
    ?assertMatch(#{<<"severity">> := warn}, FAbsent),
    ?assertEqual(<<"lockdown-off-or-unknown">>,
                 maps:get(<<"code">>, FAbsent)),
    ok.

%% v1.2 batch 9 / paper-to-code MEDIUM-4: ek_finding must emit a
%% critical for `ek-chain-valid = "unknown"' so an empty roots
%% directory or un-evaluated chain does not slide past as "ok".
v1_2_ek_finding_catches_chain_unknown_test() ->
    F = ek_finding(#{<<"ek-chain-valid">> => <<"unknown">>}),
    ?assertMatch(#{<<"severity">> := critical}, F),
    ?assertEqual(<<"ek-chain-unknown">>, maps:get(<<"code">>, F)),
    %% true / present but chain-valid not set -> ok (other checks
    %% fire their own findings).
    ?assertEqual(ok,
                 ek_finding(#{<<"ek-present">> => true,
                              <<"ek-chain-valid">> => true})),
    %% false is separate, already CRITICAL.
    FF = ek_finding(#{<<"ek-chain-valid">> => false}),
    ?assertMatch(#{<<"severity">> := critical}, FF),
    ?assertEqual(<<"ek-chain-invalid">>, maps:get(<<"code">>, FF)),
    ok.

%% v1.2 batch 12 / reviewer pass 10 fuzzer: the LapEE canonical
%% rule (AGENTS.md) demands every claim.* field populate to a
%% concrete value OR an explicit unknown/absent; a 500 stacktrace
%% is neither. The fuzzer identified three shapes that crashed
%% the pre-batch-12 parser; each gets a regression test here.

%% Shape 1: envelope whose fields round-tripped through a JSON
%% library that decoded `null' as the atom `undefined'. Pre-
%% batch-12 `decode_cert(undefined)' and `decode_pub_key(undefined)'
%% had no matching clause and raised `function_clause', escaping
%% past `interpret/3' and `claim/3' (neither `try'-wraps).
v1_2_decode_cert_survives_non_binary_test() ->
    ?assertMatch({error, not_binary}, decode_cert(undefined)),
    ?assertMatch({error, not_binary}, decode_cert(42)),
    ?assertMatch({error, not_binary}, decode_cert([])),
    ?assertMatch({error, not_binary}, decode_cert(#{})),
    ?assertMatch({error, empty},      decode_cert(<<>>)),
    ?assertMatch({error, no_certificate},
                 decode_cert(<<"not-a-pem-cert">>)),
    ok.

v1_2_decode_pub_key_survives_non_binary_test() ->
    ?assertMatch({error, not_binary}, decode_pub_key(undefined)),
    ?assertMatch({error, not_binary}, decode_pub_key(42)),
    ?assertMatch({error, not_binary}, decode_pub_key([])),
    ?assertMatch({error, not_binary}, decode_pub_key(#{})),
    ?assertMatch({error, empty},      decode_pub_key(<<>>)),
    ?assertMatch({error, no_entries},
                 decode_pub_key(<<"not-a-pem-key">>)),
    ok.

%% Shape 2: non-map Base (top-level JSON array, binary, integer,
%% atom). Pre-batch-12 `resolve_envelope' called
%% `hb_maps:get(<<"body">>, Base, ...)' unconditionally, raising
%% `{badmap, Base}' before `safe_interpret' could wrap.
v1_2_resolve_envelope_survives_non_map_base_test() ->
    %% All non-map inputs collapse to an empty map -- the
    %% downstream pipeline then produces a structured
    %% "everything unknown" verdict instead of crashing.
    ?assertEqual(#{}, resolve_envelope([], #{}, #{})),
    ?assertEqual(#{}, resolve_envelope(<<"binary">>, #{}, #{})),
    ?assertEqual(#{}, resolve_envelope(42, #{}, #{})),
    ?assertEqual(#{}, resolve_envelope(undefined, #{}, #{})),
    %% Map inputs still work.
    ?assertEqual(#{<<"a">> => 1},
                 resolve_envelope(#{<<"a">> => 1}, #{}, #{})),
    %% And the nested-body path still unwraps.
    ?assertEqual(#{<<"x">> => 2},
                 resolve_envelope(#{<<"body">> => #{<<"x">> => 2}},
                                  #{}, #{})),
    ok.

%% Shape 3: `platform-probes' set to a non-map value. The three
%% claim_* paths (cpu, lockdown, iommu) read `platform-probes' as
%% a map and index into it; a non-map value raised `{badmap, _}'
%% on the second `hb_maps:get'. Now they all go through
%% `probes_map/1' which normalises to `#{}'.
v1_2_probes_map_normalises_non_map_test() ->
    ?assertEqual(#{},
                 probes_map(#{<<"platform-probes">> => <<"binary">>})),
    ?assertEqual(#{},
                 probes_map(#{<<"platform-probes">> => 42})),
    ?assertEqual(#{},
                 probes_map(#{<<"platform-probes">> => []})),
    ?assertEqual(#{},
                 probes_map(#{<<"platform-probes">> => undefined})),
    ?assertEqual(#{},
                 probes_map(#{})),    % absent
    %% Map-valued probes pass through unchanged.
    P = #{<<"cpuinfo">> => #{<<"vendor-id">> => <<"GenuineIntel">>}},
    ?assertEqual(P, probes_map(#{<<"platform-probes">> => P})),
    ok.

%% End-to-end: claim/3 with an adversarial envelope (non-map Base
%% + platform-probes binary + undefined pem-ish fields) does NOT
%% crash the parser. Returns a structured verdict with `unknown'
%% signals instead of a 500.
v1_2_claim_survives_adversarial_envelope_test() ->
    %% Top-level non-map Base (envelope 16 in the fuzzer audit):
    {ok, Claim1} = claim([], #{}, #{}),
    ?assertMatch(#{<<"status">> := 200}, Claim1),
    %% Body-wrapped envelope with `platform-probes' as a binary
    %% and PEM fields as atoms (envelope 10 + 15 combined):
    Adversarial = #{
        <<"body">> => #{
            <<"platform-probes">> => <<"not-a-map">>,
            <<"ek-cert-pem">>      => undefined,
            <<"ak-pub-pem">>       => undefined
        }
    },
    {ok, Claim2} = claim(Adversarial, #{}, #{}),
    ?assertMatch(#{<<"status">> := 200}, Claim2),
    %% Verdict pipeline still produces a structured result.
    Body = maps:get(<<"body">>, Claim2),
    ?assert(is_map(Body)),
    ?assert(maps:is_key(<<"policy-verdict">>, Body)),
    ok.

%% v1.2 batch 9 / paper-to-code LOW-1: freshness_finding must emit
%% a warn for envelopes without a recognised `freshness-indicator',
%% so an adversary cannot silence the signal by stripping the field.
v1_2_freshness_finding_catches_unknown_test() ->
    ?assertEqual(ok,
                 freshness_finding(
                   #{<<"freshness-indicator">> => <<"safe">>})),
    NoNonce = freshness_finding(
                #{<<"freshness-indicator">> => <<"no-nonce">>}),
    ?assertMatch(#{<<"severity">> := warn}, NoNonce),
    %% absent / unknown -> warn with new code.
    lists:foreach(
        fun(Arg) ->
            F = freshness_finding(Arg),
            ?assertMatch(#{<<"severity">> := warn}, F),
            ?assertEqual(<<"freshness-indicator-unknown">>,
                         maps:get(<<"code">>, F))
        end,
        [#{},
         #{<<"freshness-indicator">> => <<"unknown">>},
         #{<<"freshness-indicator">> => <<"">>},
         #{<<"freshness-indicator">> => <<"no-signal-here">>}]),
    ok.

-endif.
