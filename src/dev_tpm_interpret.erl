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
-export([info/1, info/3, interpret/3, verify/3]).
-include("include/hb.hrl").
-include_lib("public_key/include/public_key.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%%% Device surface
%%%============================================================================

info(_) ->
    #{ exports => [<<"info">>, <<"interpret">>, <<"verify">>] }.

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
            <<"api">> => #{
                <<"interpret">> => #{
                    <<"description">> =>
                        <<"Structured interpretation of the envelope. "
                          "Does NOT itself verify — pair with `verify' "
                          "or pre-verified input.">>
                },
                <<"verify">> => #{
                    <<"description">> =>
                        <<"Call ~tpm2@2.0a/verify, then if the chain "
                          "is accepted, return the verification result "
                          "plus the full interpretation.">>
                }
            }
        }
    }}.

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
                        or_null(maps:get(serial_hex, Attrs, undefined)),
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
            <<"magic_hex">> => hexenc(Magic),
            <<"magic_ok">> => (Magic =:= <<16#FF, "TCG">>),
            <<"attest_type">> => attest_type_name(Type),
            <<"nonce_b64url">> =>
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
        <<"raw_b64url">> => B64,
        <<"hex">>        => hexenc(Raw),
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
    Pcr0 = pcr_hex(<<"0">>, Pcrs),
    Pcr1 = pcr_hex(<<"1">>, Pcrs),
    Pcr7 = pcr_hex(<<"7">>, Pcrs),
    Base = #{
        <<"firmware_srtm_hex">> => or_null(Pcr0),
        <<"platform_firmware_config_hex">> => or_null(Pcr1),
        <<"secure_boot_policy_hex">> => or_null(Pcr7),
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
%% directly.
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
                fun({PcrKey, ExpHex}) ->
                    pcr_hex(PcrKey, Actual) == ExpHex
                end,
                maps:to_list(Expected))
    end;
profile_matches(_, _) -> false.

summarise_profile(#{<<"name">> := Name, <<"attributes">> := Attrs}) ->
    #{<<"name">> => Name, <<"attributes">> => Attrs};
summarise_profile(#{<<"name">> := Name}) ->
    #{<<"name">> => Name};
summarise_profile(Entry) -> Entry.

pcr_hex(Key, Pcrs) ->
    case hb_maps:get(Key, Pcrs, undefined, #{}) of
        #{<<"hex">> := H} -> H;
        _ -> undefined
    end.

pcr_is_zero(Key, Pcrs) ->
    case hb_maps:get(Key, Pcrs, undefined, #{}) of
        #{<<"is_zero">> := V} -> V;
        _ -> true
    end.

%%---- Kernel identity ---------------------------------------------------

interpret_kernel(_E, _Db, Pcrs) ->
    Pcr4 = pcr_hex(<<"4">>, Pcrs),
    Pcr11 = pcr_hex(<<"11">>, Pcrs),
    Pcr12 = pcr_hex(<<"12">>, Pcrs),
    #{
        <<"boot_loader_hex">> => or_null(Pcr4),
        <<"uki_image_hex">> => or_null(Pcr11),
        <<"uki_cmdline_hex">> => or_null(Pcr12),
        <<"uki_measured">> =>
            (not pcr_is_zero(<<"11">>, Pcrs))
                orelse (not pcr_is_zero(<<"12">>, Pcrs))
    }.

%%---- IMA chain --------------------------------------------------------

interpret_ima(_E, _Db, Pcrs) ->
    %% Without the firmware/IMA event log (which we don't transport
    %% end-to-end today — a gap noted in SECURITY.md item 8), we can
    %% only report the PCR 10 final value + whether IMA was active.
    Pcr10 = pcr_hex(<<"10">>, Pcrs),
    Active = not pcr_is_zero(<<"10">>, Pcrs),
    #{
        <<"pcr10_hex">> => or_null(Pcr10),
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
    Serial  = serial_hex(Tbs#'OTPTBSCertificate'.serialNumber),
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
                serial_hex => Serial,
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

serial_hex(N) when is_integer(N) ->
    iolist_to_binary(io_lib:format("~.16B", [N]));
serial_hex(_) -> undefined.

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

hexenc(B) when is_binary(B) ->
    string:lowercase(binary:encode_hex(B));
hexenc(_) -> <<>>.

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
    ?assert(lists:member(<<"interpret">>, Exports)),
    ?assert(lists:member(<<"verify">>, Exports)).

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
