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
         summary/3, peer_summary/3, peer_status/3, checks/3]).
-include("include/hb.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%%% Device surface
%%%============================================================================

info(_) ->
    #{ exports => [<<"info">>, <<"interpret">>, <<"verify">>,
                   <<"verify-peer">>, <<"summary">>, <<"peer-summary">>,
                   <<"peer-status">>, <<"checks">>] }.

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
            <<"wire_format">> =>
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
                          "policy, or adversarial test harnesses.">>,
                    <<"response">> =>
                        <<"[{name, purpose, failure_implies}].">>
                }
            }
        }
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
                            <<"envelope_shape_ok">> => true,
                            <<"summary">> => summarise_interp(Interp)
                        }
                    }};
                {error, Reason} ->
                    {ok, #{
                        <<"status">> => 200,
                        <<"body">> => #{
                            <<"peer">>     => Base,
                            <<"reachable">> => false,
                            <<"envelope_shape_ok">> => false,
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
                            <<"lapee_attestation_version">> =>
                                hb_maps:get(
                                    <<"lapee_attestation_version">>,
                                    Envelope, null, Opts),
                            <<"wallet_address">> =>
                                hb_maps:get(<<"wallet_address">>,
                                            Envelope, null, Opts),
                            <<"node_message_id">> =>
                                hb_maps:get(<<"node_message_id">>,
                                            Envelope, null, Opts)
                        }
                    }};
                {error, Reason} ->
                    {ok, #{
                        <<"status">> => 200,
                        <<"body">> => #{
                            <<"peer">> => Base,
                            <<"reachable">> => false,
                            <<"lapee_attestation_version">> => null,
                            <<"wallet_address">> => null,
                            <<"node_message_id">> => null,
                            <<"detail">> => fmt_reason(Reason)
                        }
                    }}
            end;
        _ -> missing_peer_400()
    end.

%%%============================================================================
%%% checks/3 — machine-readable description of the five-check battery
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
                    <<"purpose">> =>
                        <<"Proves this TPM was manufactured by a "
                          "known vendor whose root CA is in the "
                          "verifier's trust anchors. Without this, "
                          "the EK (and thus the AK, and thus the "
                          "quote) could be synthesised by anyone.">>,
                    <<"failure_implies">> =>
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
                    <<"purpose">> =>
                        <<"Proves the TPM signed the quoted PCR "
                          "values (and nothing else) with its AK, "
                          "and that extraData equals the caller's "
                          "nonce (anti-replay).">>,
                    <<"failure_implies">> =>
                        <<"Either the quote signature is invalid "
                          "(wrong key / tampered message), the "
                          "pcrDigest doesn't match the reported "
                          "PCR values, or the nonce was replayed.">>
                },
                #{
                    <<"name">> =>
                        <<"Runtime event log replay of PCR 15 "
                          "matches quoted value">>,
                    <<"purpose">> =>
                        <<"Proves the envelope's declared PCR 15 "
                          "events hash together to the quoted "
                          "PCR 15 value. Establishes a correspondence "
                          "between declared events and hardware "
                          "state.">>,
                    <<"failure_implies">> =>
                        <<"The runtime_event_log doesn't match "
                          "what was actually quoted — events "
                          "missing, inserted, or out of order.">>
                },
                #{
                    <<"name">> =>
                        <<"PCR 15 extension commits to "
                          "node_message_id">>,
                    <<"purpose">> =>
                        <<"Proves THIS node's node_message_id was "
                          "extended into PCR 15 — the LapEE key "
                          "binding. Ties the attestation to the "
                          "specific node configuration.">>,
                    <<"failure_implies">> =>
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
                    <<"purpose">> =>
                        <<"Proves the attestation carries its own "
                          "node message (configuration) with a 43-"
                          "character base64url id that decodes to "
                          "32 bytes. Enables offline inspection of "
                          "what was actually attested to.">>,
                    <<"failure_implies">> =>
                        <<"Envelope is malformed or missing the "
                          "node_message / node_message_id fields.">>
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
            <<"error">> => <<"missing_peer">>,
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
                    <<"error">> => <<"missing_peer">>,
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
                            <<"error">> => <<"peer_did_not_return_envelope">>,
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
                    <<"error">> => <<"peer_unreachable">>,
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
                    <<"error">> => <<"peer_unexpected_response">>,
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
                    <<"trust_anchor_source">> => CaSource,
                    <<"nonce_challenge">>  => hb_util:encode(NonceBytes),
                    <<"nonce_freshness">>  => <<"verified">>
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
                    <<"nonce_challenge">>  => hb_util:encode(NonceBytes),
                    <<"nonce_freshness">>  => <<"mismatch">>,
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
        Q = hb_maps:get(<<"tpm_quote">>, Envelope, #{}, Opts),
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
            S = maps:get(<<"trust_anchor_source">>, Body, <<"node_config">>),
            {V, D, flatten_checks(C), S};
        _ ->
            {false, <<"rejected">>, [], <<"none">>}
    end.

flatten_checks(Cs) when is_list(Cs) ->
    [ case C of
          #{<<"ok">> := O, <<"name">> := N, <<"detail">> := De} ->
              #{<<"ok">> => O, <<"name">> => N, <<"detail">> => De};
          #{<<"ok">> := O, <<"name">> := N} ->
              #{<<"ok">> => O, <<"name">> => N, <<"detail">> => <<"">>};
          _ -> #{<<"ok">> => false, <<"name">> => <<"unknown">>,
                 <<"detail">> => <<"">>}
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
        <<"envelope_version">> =>
            maps:get(<<"version">>, Env, null),
        <<"tpm_manufacturer">> =>
            maps:get(<<"manufacturer_name">>, Tpm, null),
        <<"tpm_manufacturer_kind">> =>
            maps:get(<<"manufacturer_kind">>, Tpm, null),
        <<"tpm_model">> =>
            maps:get(<<"model">>, Tpm, null),
        <<"tpm_firmware_version">> =>
            maps:get(<<"firmware_version">>, Tpm, null),
        <<"ak_algorithm">> =>
            maps:get(<<"algorithm">>, Ak, null),
        <<"ak_key_size_bits">> =>
            maps:get(<<"key_size_bits">>, Ak, null),
        <<"ak_public_key_b64url">> =>
            maps:get(<<"pub_der_sha256_b64url">>, Ak, null),
        <<"quote_attest_type">> =>
            maps:get(<<"attest_type">>, Q, null),
        <<"quote_clock_ms">> =>
            maps:get(<<"clock_ms">>, Q, null),
        <<"quote_reset_count">> =>
            maps:get(<<"reset_count">>, Q, null),
        <<"secure_boot_measured">> =>
            maps:get(<<"secure_boot_measured">>, Boot, null),
        <<"wallet_address">> =>
            maps:get(<<"wallet_address">>, Node, null),
        <<"node_message_id">> =>
            maps:get(<<"node_message_id">>, Node, null),
        <<"on_start_hook_device">> =>
            maps:get(<<"on_start_hook_device">>, Node, null),
        <<"pcr15_event_count">> =>
            maps:get(<<"pcr15_event_count">>, Node, null)
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
    hb_maps:get(<<"lapee_attestation_version">>, M, undefined, #{}) /=
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
    Pcrs = interpret_pcrs(E, Db),
    Boot = interpret_boot_chain(E, Db, Pcrs),
    Kernel = interpret_kernel(E, Db, Pcrs),
    Ima = interpret_ima(E, Db, Pcrs),
    Node = interpret_node(E),
    Env = interpret_envelope_meta(E),
    #{
        <<"envelope">> => Env,
        <<"tpm">>      => Tpm,
        <<"ak">>       => Ak,
        <<"quote">>    => Quote,
        <<"pcrs">>     => Pcrs,
        <<"boot">>     => Boot,
        <<"kernel">>   => Kernel,
        <<"ima">>      => Ima,
        <<"node">>     => Node
    }.

%%---- envelope meta -----------------------------------------------------

interpret_envelope_meta(E) ->
    #{
        <<"version">> =>
            hb_maps:get(<<"lapee_attestation_version">>, E, null, #{}),
        <<"issued_at_unix">> =>
            hb_maps:get(<<"issued_at_unix">>, E, null, #{}),
        <<"wallet_address">> =>
            hb_maps:get(<<"wallet_address">>, E, null, #{}),
        <<"node_message_id">> =>
            hb_maps:get(<<"node_message_id">>, E, null, #{})
    }.

%%---- TPM identity ------------------------------------------------------

interpret_tpm_identity(E, Db) ->
    Pem = hb_maps:get(<<"ek_cert_pem">>, E, <<>>, #{}),
    case decode_cert(Pem) of
        {ok, Cert} ->
            Attrs = tpm_attrs_from_cert(Cert),
            VendorId = maps:get(manufacturer_id, Attrs, undefined),
            VendorEntry = lookup_vendor(VendorId, Db),
            maps:merge(
                #{
                    <<"manufacturer_id">> =>
                        or_null(VendorId),
                    <<"manufacturer_name">> =>
                        maps:get(<<"name">>, VendorEntry, null),
                    <<"manufacturer_kind">> =>
                        maps:get(<<"kind">>, VendorEntry, null),
                    <<"model">> =>
                        or_null(maps:get(model, Attrs, undefined)),
                    <<"firmware_version">> =>
                        or_null(maps:get(firmware_version, Attrs,
                                         undefined)),
                    <<"spec_family">> =>
                        or_null(maps:get(spec_family, Attrs, undefined)),
                    <<"spec_level">> =>
                        or_null(maps:get(spec_level, Attrs, undefined)),
                    <<"spec_revision">> =>
                        or_null(maps:get(spec_revision, Attrs, undefined)),
                    <<"ek_cert_subject">> =>
                        or_null(maps:get(subject_rdn, Attrs, undefined)),
                    <<"ek_cert_issuer">> =>
                        or_null(maps:get(issuer_rdn, Attrs, undefined)),
                    <<"ek_cert_serial">> =>
                        or_null(maps:get(serial_b64url, Attrs, undefined)),
                    <<"ek_cert_valid_from">> =>
                        or_null(maps:get(valid_from, Attrs, undefined)),
                    <<"ek_cert_valid_to">> =>
                        or_null(maps:get(valid_to, Attrs, undefined))
                },
                extra_vendor_fields(VendorEntry))
            ;
        {error, Why} ->
            #{
                <<"manufacturer_id">> => null,
                <<"manufacturer_name">> => null,
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
    Pem = hb_maps:get(<<"ak_pub_pem">>, E, <<>>, #{}),
    case decode_pub_key(Pem) of
        {ok, #'RSAPublicKey'{modulus = N, publicExponent = Exp}} ->
            Der = public_key:der_encode('RSAPublicKey',
                                        #'RSAPublicKey'{
                                            modulus=N, publicExponent=Exp}),
            #{
                <<"algorithm">> => <<"RSA">>,
                <<"key_size_bits">> =>
                    bit_size_of_modulus(N),
                <<"public_exponent">> => Exp,
                <<"pub_der_sha256_b64url">> =>
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
    Q = hb_maps:get(<<"tpm_quote">>, E, #{}, #{}),
    QuotedB64 = hb_maps:get(<<"quoted">>, Q, <<>>, #{}),
    try
        Quoted = hb_util:decode(QuotedB64),
        <<Magic:4/binary, Type:16/unsigned-big, Rest0/binary>> = Quoted,
        {_QualifiedSigner, Rest1} = tpm2b(Rest0),
        {ExtraData, Rest2}        = tpm2b(Rest1),
        <<Clock:64/unsigned-big,
          ResetCount:32/unsigned-big,
          RestartCount:32/unsigned-big,
          SafeByte:8, _Rest3/binary>> = Rest2,
        #{
            %% Magic is a 4-byte TCG sentinel (0xFF "TCG"). We don't
            %% expose the raw bytes — `magic_ok' is the single fact a
            %% caller needs; an unrecognised magic means the quote is
            %% not TPM-shaped and `error' is returned instead.
            <<"magic_ok">> => (Magic =:= <<16#FF, "TCG">>),
            <<"attest_type">> => attest_type_name(Type),
            <<"nonce">> =>
                hb_util:encode(ExtraData),
            <<"clock_ms">> => Clock,
            <<"reset_count">> => ResetCount,
            <<"restart_count">> => RestartCount,
            <<"safe">> => SafeByte =/= 0
        }
    catch
        _:_ ->
            #{<<"error">> =>
                <<"TPMS_ATTEST parse failed (truncated or wrong shape)">>}
    end.

tpm2b(<<Size:16/unsigned-big, Payload:Size/binary, Rest/binary>>) ->
    {Payload, Rest}.

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

interpret_pcrs(E, _Db) ->
    Q = hb_maps:get(<<"tpm_quote">>, E, #{}, #{}),
    Vals = hb_maps:get(<<"pcr_values">>, Q, #{}, #{}),
    maps:from_list(
        [{I, interpret_one_pcr(I, V)}
         || {I, V} <- maps:to_list(Vals)]).

interpret_one_pcr(Idx, B64) ->
    Raw = try hb_util:decode(B64)
          catch _:_ -> <<>>
          end,
    Zero = (Raw =:= <<0:256>>) orelse (Raw =:= <<>>),
    #{
        %% Canonical base64url form, carried through unchanged from the
        %% attestation envelope. No hex twin: HyperBEAM wire convention
        %% is base64url everywhere, and the raw digest is well over the
        %% "short and always-displayed-in-hex" exception threshold.
        <<"digest">>     => B64,
        <<"role">>       => pcr_role(Idx),
        <<"role_notes">> => pcr_role_notes(Idx),
        <<"is_zero">>    => Zero
    }.

%% Canonical TCG PCR usage. Source: TCG PC Client Platform Firmware
%% Profile + UEFI Spec + systemd-stub docs.
pcr_role(<<"0">>) -> <<"firmware_srtm">>;
pcr_role(<<"1">>) -> <<"platform_firmware_config">>;
pcr_role(<<"2">>) -> <<"option_rom_code">>;
pcr_role(<<"3">>) -> <<"option_rom_config">>;
pcr_role(<<"4">>) -> <<"boot_loader_code">>;
pcr_role(<<"5">>) -> <<"boot_loader_config">>;
pcr_role(<<"6">>) -> <<"platform_manufacturer">>;
pcr_role(<<"7">>) -> <<"secure_boot_policy">>;
pcr_role(<<"8">>) -> <<"grub_kernel_cmdline_legacy">>;
pcr_role(<<"9">>) -> <<"grub_kernel_modules_legacy">>;
pcr_role(<<"10">>) -> <<"ima_runtime_measurements">>;
pcr_role(<<"11">>) -> <<"uki_kernel_image">>;
pcr_role(<<"12">>) -> <<"uki_kernel_cmdline">>;
pcr_role(<<"13">>) -> <<"uki_system_extensions">>;
pcr_role(<<"14">>) -> <<"secure_boot_authority_mok">>;
pcr_role(<<"15">>) -> <<"lapee_node_identity">>;
pcr_role(N) when is_integer(N) -> pcr_role(integer_to_binary(N));
pcr_role(_) -> <<"unassigned_or_application">>.

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
        <<"firmware_srtm">> => or_null(Pcr0),
        <<"platform_firmware_config">> => or_null(Pcr1),
        <<"secure_boot_policy">> => or_null(Pcr7),
        <<"secure_boot_measured">> =>
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
    Profiles = case maps:get(<<"pcr_profiles">>, Db, #{}) of
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
        case maps:get(<<"match_pcrs">>, Entry, undefined) of
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
        #{<<"raw_b64url">> := D} -> D;
        _ -> undefined
    end.

pcr_is_zero(Key, Pcrs) ->
    case hb_maps:get(Key, Pcrs, undefined, #{}) of
        #{<<"is_zero">> := V} -> V;
        _ -> true
    end.

%%---- Kernel identity ---------------------------------------------------

interpret_kernel(_E, _Db, Pcrs) ->
    Pcr4 = pcr_digest(<<"4">>, Pcrs),
    Pcr11 = pcr_digest(<<"11">>, Pcrs),
    Pcr12 = pcr_digest(<<"12">>, Pcrs),
    #{
        <<"boot_loader">> => or_null(Pcr4),
        <<"uki_image">> => or_null(Pcr11),
        <<"uki_cmdline">> => or_null(Pcr12),
        <<"uki_measured">> =>
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
        <<"events_available">> => false,
        <<"note">> =>
            <<"LapEE does not yet transport the kernel IMA event log "
              "in the attestation envelope (PCR 10's final value is "
              "signed; the per-file chain isn't). Future `~tpm2@2.0a' "
              "versions will include it; until then, a verifier can "
              "only assert PCR 10 matches a known-good profile.">>
    }.

%%---- Node identity ----------------------------------------------------

interpret_node(E) ->
    Nm = hb_maps:get(<<"node_message">>, E, undefined, #{}),
    Id = hb_maps:get(<<"node_message_id">>, E, null, #{}),
    Wallet = hb_maps:get(<<"wallet_address">>, E, null, #{}),
    EventLog = hb_maps:get(<<"runtime_event_log">>, E, [], #{}),
    Pcr15Events = [Ev ||
        Ev <- EventLog,
        int_pcr(hb_maps:get(<<"pcr">>, Ev, 0, #{})) =:= 15],
    #{
        <<"wallet_address">> => Wallet,
        <<"node_message_id">> => Id,
        <<"node_message_key_count">> =>
            case Nm of
                M when is_map(M) -> maps:size(M);
                _ -> null
            end,
        <<"on_start_hook_device">> => nested_get(Nm, [<<"on">>, <<"start">>,
                                                      <<"device">>]),
        <<"on_start_hook_path">>   => nested_get(Nm, [<<"on">>, <<"start">>,
                                                      <<"path">>]),
        <<"pcr15_event_count">> => length(Pcr15Events),
        <<"pcr15_event_types">> =>
            [hb_maps:get(<<"event_type">>, Ev, null, #{})
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
              <<"checks">>]],
    %% Params are spelled out for the peer-facing handlers.
    VpParams = maps:get(<<"params">>, maps:get(<<"verify-peer">>, Api)),
    ?assert(maps:is_key(<<"peer">>, VpParams)),
    ?assert(maps:is_key(<<"trusted-ca">>, VpParams)),
    %% `wire_format' tells callers what encoding to expect.
    ?assert(maps:is_key(<<"wire_format">>, Body)),
    ok.

%% `checks/3' returns a machine-readable description of the
%% cryptographic battery — clients build UI + policy on this, so
%% the shape must not drift silently.
checks_surface_stable_test() ->
    {ok, #{<<"body">> := #{<<"checks">> := Cs}}} = checks(#{}, #{}, #{}),
    ?assertEqual(5, length(Cs)),
    lists:foreach(
        fun(C) ->
            ?assert(maps:is_key(<<"name">>, C)),
            ?assert(maps:is_key(<<"purpose">>, C)),
            ?assert(maps:is_key(<<"failure_implies">>, C))
        end, Cs),
    Names = [maps:get(<<"name">>, C) || C <- Cs],
    ?assert(lists:any(fun(N) ->
                          binary:match(N, <<"EK certificate">>) =/= nomatch
                      end, Names)),
    ok.

%% `summary/3' on a structurally-complete envelope returns the same
%% link-free shape that verify-peer's `summary' uses.
summary_returns_link_free_map_test() ->
    Zero = hb_util:encode(<<0:256>>),
    Envelope = #{
        <<"lapee_attestation_version">> => <<"0.3">>,
        <<"ek_cert_pem">> => <<>>,
        <<"ak_pub_pem">> => <<>>,
        <<"tpm_quote">> => #{<<"pcr_values">> => #{}, <<"quoted">> => <<>>,
                             <<"signature">> => <<>>, <<"nonce">> => <<>>,
                             <<"pcr_selection">> => []},
        <<"runtime_event_log">> => [],
        <<"node_message">> =>
            #{<<"on">> => #{<<"start">> =>
                              #{<<"device">> => <<"tpm2@2.0a">>}}},
        <<"node_message_id">> => Zero,
        <<"wallet_address">> => <<"sample-wallet">>
    },
    {ok, #{<<"body">> := S}} = summary(Envelope, #{}, #{}),
    ?assertEqual(<<"0.3">>, maps:get(<<"envelope_version">>, S)),
    ?assertEqual(<<"tpm2@2.0a">>,
                 maps:get(<<"on_start_hook_device">>, S)),
    ?assertEqual(<<"sample-wallet">>,
                 maps:get(<<"wallet_address">>, S)),
    %% Summary must not carry maps inside its values — that's the
    %% link-free property. Spot-check a few known fields.
    [?assert(not is_map(maps:get(K, S, null)))
     || K <- [<<"tpm_manufacturer">>, <<"ak_algorithm">>,
              <<"quote_attest_type">>, <<"secure_boot_measured">>,
              <<"pcr15_event_count">>]],
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
        <<"lapee_attestation_version">> => <<"0.3">>,
        <<"tpm_quote">> => #{
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
    ?assertEqual(<<"mismatch">>, maps:get(<<"nonce_freshness">>, Body)),
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
        <<"lapee_attestation_version">> => <<"0.3">>,
        <<"tpm_quote">> => #{
            <<"nonce">> => hb_util:encode(Challenge),
            <<"pcr_values">> => #{},
            <<"quoted">> => <<>>,
            <<"signature">> => <<>>,
            <<"pcr_selection">> => []
        },
        <<"ek_cert_pem">> => <<>>,
        <<"ak_pub_pem">> => <<>>,
        <<"runtime_event_log">> => [],
        <<"node_message">> => #{<<"port">> => 8734},
        <<"node_message_id">> => hb_util:encode(<<0:256>>),
        <<"wallet_address">> => <<"sample">>
    },
    {ok, #{<<"body">> := Body}} =
        run_cross_node_verify(<<"http://peer">>, Envelope,
                              undefined, Challenge, #{}),
    %% Freshness gate passed — crypto checks attempted (and will
    %% fail on this synthetic envelope for other reasons, which is
    %% fine — we only assert nonce_freshness says "verified" and
    %% the check list isn't the single-entry nonce-mismatch form).
    ?assertEqual(<<"verified">>,
                 maps:get(<<"nonce_freshness">>, Body)),
    ?assertEqual(Challenge,
                 hb_util:decode(maps:get(<<"nonce_challenge">>, Body))),
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
                           #{<<"error">> := <<"missing_peer">>}}},
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
        <<"lapee_attestation_version">> => <<"0.3">>,
        <<"issued_at_unix">> => 1700000000,
        <<"ek_cert_pem">> => <<>>,
        <<"ak_pub_pem">> => <<>>,
        <<"tpm_quote">> => #{
            <<"pcr_selection">> => [0, 15],
            <<"pcr_values">> => #{
                <<"0">> => Zero,
                <<"15">> => Zero
            },
            <<"quoted">> => <<>>,
            <<"signature">> => <<>>,
            <<"nonce">> => <<>>
        },
        <<"runtime_event_log">> => [],
        <<"node_message">> =>
            #{<<"port">> => 8734,
              <<"on">> =>
                #{<<"start">> =>
                    #{<<"device">> => <<"tpm2@2.0a">>,
                      <<"path">> => <<"extend">>}}},
        <<"node_message_id">> => Zero,
        <<"wallet_address">> => <<"sample-wallet-address-XX">>
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
    ?assertEqual(<<"lapee_node_identity">>, maps:get(<<"role">>, Pcr15)),
    ?assertEqual(true, maps:get(<<"is_zero">>, Pcr15)),
    %% Node section reads on.start.device
    Node = maps:get(<<"node">>, Body),
    ?assertEqual(<<"tpm2@2.0a">>,
                 maps:get(<<"on_start_hook_device">>, Node)).

pcr_role_canonical_mapping_test() ->
    ?assertEqual(<<"firmware_srtm">>, pcr_role(<<"0">>)),
    ?assertEqual(<<"secure_boot_policy">>, pcr_role(<<"7">>)),
    ?assertEqual(<<"ima_runtime_measurements">>, pcr_role(<<"10">>)),
    ?assertEqual(<<"uki_kernel_image">>, pcr_role(<<"11">>)),
    ?assertEqual(<<"lapee_node_identity">>, pcr_role(<<"15">>)),
    ?assertEqual(<<"unassigned_or_application">>, pcr_role(<<"22">>)).

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
