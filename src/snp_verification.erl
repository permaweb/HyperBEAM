%%% @doc Verification functions for SNP commitment reports.
%%%
%%% This module handles verification of SNP attestation reports, including
%%% measurement verification, signature verification, and higher-level
%%% verification pipelines.
-module(snp_verification).
-export([verify_measurement/2, verify_signature/3, verify_signature_and_address/3,
         verify_debug_disabled/1, verify_measurement/3, verify_report_integrity/1,
         verify_nonce/4, verify_trusted_software/3, is_verification_failure/1,
         verify/3]).
-include("include/hb.hrl").
-include("include/snp_constants.hrl").
-include("include/snp_guids.hrl").

%% Type definitions
-type verification_result() :: {ok, true} | {ok, false} | {error, term()}.
-type trusted_software_config() :: map().  % Map containing trusted software hashes/config
-type trusted_software_list() :: [trusted_software_config()].

%% Helper function to validate verification configuration options
-spec validate_verify_config(NodeOpts :: map()) -> {ok, map()} | {error, term()}.
validate_verify_config(NodeOpts) ->
    maybe
        % Validate snp_trusted (required)
        {ok, _} ?= validate_snp_trusted_for_verify(NodeOpts),
        % Validate snp_enforced_keys (optional, but if present must be valid)
        {ok, _} ?= validate_snp_enforced_keys(NodeOpts),
        {ok, NodeOpts}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, {config_validation_error, Error}}
    end.

%% Helper function to validate snp_trusted for verification
-spec validate_snp_trusted_for_verify(NodeOpts :: map()) -> {ok, trusted_software_list()} | {error, term()}.
validate_snp_trusted_for_verify(NodeOpts) ->
    case hb_opts:get(snp_trusted, [#{}], NodeOpts) of
        [] -> 
            ?event(snp_error, {config_validation_failed, #{
                option => <<"snp_trusted">>,
                operation => <<"verify">>,
                reason => <<"empty_list">>,
                expected => <<"Non-empty list of trusted software configuration maps">>,
                suggestion => <<"snp_trusted must contain at least one trusted software configuration map for verification.">>
            }}),
            {error, {empty_trusted_configs, <<"snp_trusted cannot be empty for verification">>}};
        TrustedList when is_list(TrustedList) -> 
            % Validate each trusted config in the list
            validate_trusted_configs_list_for_verify(TrustedList, 0);
        InvalidTrusted -> 
            ?event(snp_error, {config_validation_failed, #{
                option => <<"snp_trusted">>,
                operation => <<"verify">>,
                actual_type => snp_util:get_type_name(InvalidTrusted),
                expected => <<"list of maps">>,
                suggestion => <<"snp_trusted must be a list of maps, each containing trusted software configuration.">>
            }}),
            {error, {invalid_trusted_type, <<"snp_trusted must be a list">>}}
    end.

%% Helper function to validate each trusted config in the list for verification
-spec validate_trusted_configs_list_for_verify(TrustedList :: [map()], Index :: non_neg_integer()) -> 
    {ok, trusted_software_list()} | {error, term()}.
validate_trusted_configs_list_for_verify([], _Index) ->
    {ok, []};
validate_trusted_configs_list_for_verify([Config | Rest], Index) ->
    case is_map(Config) of
        true -> 
            % Validate that config contains at least some expected keys
            ConfigKeys = maps:keys(Config),
            BinaryKeys = [K || K <- ConfigKeys, is_binary(K)],
            AtomKeys = [K || K <- ConfigKeys, is_atom(K)],
            AllKeys = BinaryKeys ++ AtomKeys,
            case length(AllKeys) > 0 of
                true -> 
                    validate_trusted_configs_list_for_verify(Rest, Index + 1);
                false -> 
                    ?event(snp_error, {config_validation_failed, #{
                        option => <<"snp_trusted">>,
                        operation => <<"verify">>,
                        index => Index,
                        reason => <<"empty_config_map">>,
                        expected => <<"Map with at least one configuration key">>,
                        suggestion => <<"Each trusted software configuration must contain at least one key (e.g., firmware, kernel, vcpus, etc.).">>
                    }}),
                    {error, {empty_trusted_config, Index, <<"Trusted config at index ", (hb_util:bin(integer_to_list(Index)))/binary, " is empty">>}}
            end;
        false -> 
            ?event(snp_error, {config_validation_failed, #{
                option => <<"snp_trusted">>,
                operation => <<"verify">>,
                index => Index,
                actual_type => snp_util:get_type_name(Config),
                expected => <<"map">>,
                suggestion => <<"Each element in snp_trusted must be a map containing trusted software configuration.">>
            }}),
            {error, {invalid_trusted_config_type, Index, <<"Config at index ", (hb_util:bin(integer_to_list(Index)))/binary, " must be a map">>}}
    end.

%% Helper function to validate snp_enforced_keys (optional)
-spec validate_snp_enforced_keys(NodeOpts :: map()) -> {ok, [atom()]} | {error, term()}.
validate_snp_enforced_keys(NodeOpts) ->
    case hb_opts:get(snp_enforced_keys, undefined, NodeOpts) of
        undefined -> 
            % Optional, use default
            {ok, ?COMMITTED_PARAMETERS};
        [] -> 
            % Empty list means use default
            {ok, ?COMMITTED_PARAMETERS};
        EnforcedKeys when is_list(EnforcedKeys), length(EnforcedKeys) > 0 -> 
            % Validate that all keys are atoms and are valid committed parameters
            validate_enforced_keys_list(EnforcedKeys);
        InvalidEnforced -> 
            ?event(snp_error, {config_validation_failed, #{
                option => <<"snp_enforced_keys">>,
                operation => <<"verify">>,
                actual_type => snp_util:get_type_name(InvalidEnforced),
                expected => <<"list of atoms">>,
                suggestion => <<"snp_enforced_keys must be a list of atoms representing committed parameters (e.g., [vcpus, vcpu_type, firmware, kernel]).">>
            }}),
            {error, {invalid_enforced_keys_type, <<"snp_enforced_keys must be a list of atoms">>}}
    end.

%% Helper function to validate enforced keys list
%% Note: Empty lists are handled by validate_snp_enforced_keys before calling this function.
%% However, this function is called recursively, so it will eventually be called with []
%% when all keys have been validated. In that case, return {ok, []} to indicate success.
-spec validate_enforced_keys_list(EnforcedKeys :: [term()]) -> {ok, [atom()]} | {error, term()}.
validate_enforced_keys_list(EnforcedKeys) ->
    validate_enforced_keys_list(EnforcedKeys, []).

%% Internal helper that accumulates validated keys
-spec validate_enforced_keys_list(EnforcedKeys :: [term()], Acc :: [atom()]) -> {ok, [atom()]} | {error, term()}.
validate_enforced_keys_list([], Acc) ->
    % Base case: all keys have been validated successfully, return them in reverse order
    {ok, lists:reverse(Acc)};
validate_enforced_keys_list([Key | Rest], Acc) ->
    case is_atom(Key) of
        true -> 
            % Check if key is a valid committed parameter
            case lists:member(Key, ?COMMITTED_PARAMETERS) of
                true -> 
                    validate_enforced_keys_list(Rest, [Key | Acc]);
                false -> 
                    ?event(snp_error, {config_validation_failed, #{
                        option => <<"snp_enforced_keys">>,
                        operation => <<"verify">>,
                        invalid_key => Key,
                        valid_keys => ?COMMITTED_PARAMETERS,
                        suggestion => <<"snp_enforced_keys must only contain valid committed parameters: ", (hb_util:bin(io_lib:format("~p", [?COMMITTED_PARAMETERS])))/binary>>
                    }}),
                    {error, {invalid_enforced_key, Key, <<"Key must be one of: ", (hb_util:bin(io_lib:format("~p", [?COMMITTED_PARAMETERS])))/binary>>}}
            end;
        false -> 
            ?event(snp_error, {config_validation_failed, #{
                option => <<"snp_enforced_keys">>,
                operation => <<"verify">>,
                invalid_key => Key,
                actual_type => case Key of
                    L when is_list(L) -> <<"list">>;
                    B when is_binary(B) -> <<"binary">>;
                    M when is_map(M) -> <<"map">>;
                    _ -> <<"other">>
                end,
                expected => <<"atom">>,
                suggestion => <<"All keys in snp_enforced_keys must be atoms (e.g., vcpus, firmware, kernel).">>
            }}),
            {error, {invalid_enforced_key_type, Key, <<"All keys must be atoms">>}}
    end;
validate_enforced_keys_list(_, _Acc) ->
    {ok, []}.

%% @doc Verify that the measurement in the report matches the expected measurement.
%% This is a simple byte comparison, so it's done in Erlang.
%% @param ReportJSON Binary containing the JSON attestation report
%% @param ExpectedMeasurement Binary containing the expected measurement (?LAUNCH_DIGEST_SIZE bytes)
%% @returns {ok, true} if measurements match, {ok, false} if they don't match,
%%          {error, Reason} if JSON parsing fails or measurement field is missing
-spec verify_measurement(ReportJSON :: binary(), ExpectedMeasurement :: binary()) -> 
    verification_result().
verify_measurement(ReportJSON, ExpectedMeasurement) ->
    case snp_util:safe_json_decode(ReportJSON) of
        {ok, ReportMap} ->
            case maps:find(<<"measurement">>, ReportMap) of
                {ok, ActualMeasurement} when is_list(ActualMeasurement) ->
                    ActualBin = hb_util:bin(ActualMeasurement),
                    ExpectedHex = hb_util:to_hex(ExpectedMeasurement),
                    ActualHex = hb_util:to_hex(ActualBin),
                    ?event(snp_short, {verify_measurement_hex, #{expected => ExpectedHex, actual => ActualHex}}),
                    case ActualBin =:= ExpectedMeasurement of
                        true ->
                            ?event(snp_short, {verify_measurement_match, true}),
                            {ok, true};
                        false -> 
                            ?event(snp_short, {verify_measurement_mismatch, #{expected_hex => ExpectedHex, actual_hex => ActualHex}}),
                            {ok, false}  % Measurement mismatch, not an error
                    end;
                {ok, ActualMeasurement} when is_binary(ActualMeasurement) ->
                    ExpectedHex = hb_util:to_hex(ExpectedMeasurement),
                    ActualHex = hb_util:to_hex(ActualMeasurement),
                    ?event(snp_short, {verify_measurement_hex, #{expected => ExpectedHex, actual => ActualHex}}),
                    case ActualMeasurement =:= ExpectedMeasurement of
                        true -> 
                            ?event(snp_short, {verify_measurement_match, true}),
                            {ok, true};
                        false -> 
                            ?event(snp_short, {verify_measurement_mismatch, #{expected_hex => ExpectedHex, actual_hex => ActualHex}}),
                            {ok, false}  % Measurement mismatch, not an error
                    end;
                error ->
                    ?event(snp_error, {verify_measurement_missing_field, #{
                        operation => <<"verify_measurement">>,
                        report_keys => maps:keys(ReportMap),
                        expected_field => <<"measurement">>,
                        suggestion => <<"Ensure the report JSON contains a 'measurement' field with the launch digest value.">>
                    }}),
                    {error, <<"Measurement verification failed: 'measurement' field not found in report. Expected a field named 'measurement' containing the launch digest (", 
                        (hb_util:bin(integer_to_list(?LAUNCH_DIGEST_SIZE)))/binary, " bytes).">>}
            end;
        {error, Reason} ->
            ?event(snp_error, {verify_measurement_decode_error, #{
                operation => <<"verify_measurement">>,
                reason => Reason,
                suggestion => <<"JSON decode failed. Ensure the input is valid JSON format.">>
            }}),
            {error, Reason}
    end.

%% @doc Verify the signature of an attestation report.
%% Accepts binary report structure and DER-encoded certificates for better performance.
%% @param ReportBinary Binary containing the raw report structure (?REPORT_SIZE bytes) OR JSON binary
%% @param CertChainPEM Binary containing the PEM-encoded certificate chain (ARK + ASK) OR DER binary
%% @param VcekDER Binary containing the DER-encoded VCEK certificate
%% @returns {ok, true} if signature is valid, {error, {ErrorCode, ErrorMsg}} if verification fails
-spec verify_signature(ReportBinary :: binary(), CertChainPEM :: binary(), VcekDER :: binary()) ->
    {ok, true} | {error, binary() | {term(), binary()}}.
verify_signature(ReportBinary, CertChainPEM, VcekDER) ->
    % Convert JSON to binary if needed
    ReportBin = case snp_util:is_json_binary(ReportBinary) of
        true -> 
            ?event(snp, {verify_signature_converting_json}),
            case snp_report_format:report_json_to_binary(ReportBinary) of
                {error, Reason1} -> 
                    ?event(snp_error, {verify_signature_json_conversion_error, #{
                        operation => <<"verify_signature">>,
                        error => Reason1,
                        suggestion => <<"Ensure the report JSON is valid and contains all required fields.">>
                    }}),
                    {error, Reason1};
                Bin -> {ok, Bin}
            end;
        false -> 
            case is_binary(ReportBinary) andalso byte_size(ReportBinary) =:= ?REPORT_SIZE of
                true -> {ok, ReportBinary};
                false -> 
                    ReportSize = case is_binary(ReportBinary) of
                        true -> byte_size(ReportBinary);
                        false -> <<"not_a_binary">>
                    end,
                    ReportType = case is_binary(ReportBinary) of 
                        true -> <<"binary">>; 
                        false -> <<"not_binary">> 
                    end,
                    ?event(snp_error, {verify_signature_invalid_report, #{
                        operation => <<"verify_signature">>,
                        actual_size => ReportSize,
                        expected_size => ?REPORT_SIZE,
                        actual_type => ReportType,
                        suggestion => <<"Ensure the report is either a ", (hb_util:bin(integer_to_list(?REPORT_SIZE)))/binary, "-byte binary or valid JSON format.">>
                    }}),
                    SizeStr = case is_binary(ReportBinary) of 
                        true -> integer_to_list(byte_size(ReportBinary)); 
                        false -> "not a binary" 
                    end,
                    {error, <<"Report validation failed: expected ", 
                        (hb_util:bin(integer_to_list(?REPORT_SIZE)))/binary, 
                        "-byte binary or valid JSON, got ", 
                        (hb_util:bin(SizeStr))/binary,
                        " bytes.">>}
            end
    end,
    % Convert PEM to DER if needed
    CertChainDER = case snp_util:is_pem_binary(CertChainPEM) of
        true -> 
            ?event(snp, {verify_signature_converting_pem}),
            case snp_certificates:pem_to_der_chain(CertChainPEM) of
                {error, Reason2} -> 
                    ?event(snp_error, {verify_signature_pem_conversion_error, #{
                        operation => <<"verify_signature">>,
                        error => Reason2,
                        suggestion => <<"Ensure the certificate chain is valid PEM format containing ASK and ARK certificates.">>
                    }}),
                    {error, Reason2};
                DER -> {ok, DER}
            end;
        false -> 
            case is_binary(CertChainPEM) of
                true -> {ok, CertChainPEM};
                false -> 
                    ?event(snp_error, {verify_signature_invalid_cert_chain, #{
                        operation => <<"verify_signature">>,
                        actual_type => case is_binary(CertChainPEM) of true -> <<"binary">>; false -> <<"not_binary">> end,
                        expected => <<"PEM or DER binary">>,
                        suggestion => <<"Ensure the certificate chain is a valid PEM or DER-encoded binary.">>
                    }}),
                    {error, <<"Certificate chain validation failed: expected PEM or DER binary, got ", 
                        (hb_util:bin(case is_binary(CertChainPEM) of true -> <<"binary">>; false -> <<"not_binary">> end))/binary,
                        ". Provide a valid certificate chain in PEM or DER format.">>}
            end
    end,
    % Validate VCEK DER
    VcekDERValid = case is_binary(VcekDER) andalso byte_size(VcekDER) > 0 of
        true -> {ok, VcekDER};
        false -> 
            ActualSize = case is_binary(VcekDER) of
                true -> byte_size(VcekDER);
                false -> 0
            end,
            ?event(snp_error, {verify_signature_invalid_vcek, #{
                operation => <<"verify_signature">>,
                actual_size => ActualSize,
                actual_type => snp_util:get_type_name(VcekDER),
                expected => <<"non-empty DER-encoded binary">>,
                suggestion => <<"Ensure VCEK is a valid DER-encoded certificate binary fetched from AMD KDS.">>
            }}),
            {error, <<"VCEK validation failed: expected non-empty DER-encoded binary, got ", 
                (hb_util:bin(case is_binary(VcekDER) of true -> integer_to_list(byte_size(VcekDER)); false -> hb_util:list(snp_util:get_type_name(VcekDER)) end))/binary,
                " bytes. Ensure VCEK is fetched from AMD KDS and is in DER format.">>}
    end,
    case {ReportBin, CertChainDER, VcekDERValid} of
        {{ok, RB}, {ok, CCD}, {ok, VD}} ->
            ?event(snp_short, {verify_signature_start, #{
                report_size => byte_size(RB),
                cert_chain_size => byte_size(CCD),
                vcek_size => byte_size(VD)
            }}),
            % All NIF calls go through snp_nif.erl
            {NifTimeMicros, NifResult} = timer:tc(fun() -> snp_nif:verify_signature_nif(RB, CCD, VD) end),
            NifTimeMs = NifTimeMicros / 1000,
            Result = NifResult,
            case Result of
                {ok, true} -> 
                    ?event(snp_short, {verify_signature_success, #{
                        time_ms => NifTimeMs
                    }});
                {ok, false} -> 
                    ?event(snp_error, {verify_signature_failed, #{
                        operation => <<"verify_signature">>,
                        time_ms => NifTimeMs,
                        suggestion => <<"The report signature is invalid. This may indicate a compromised or tampered report. Verify the report source and certificates.">>
                    }});
                Error -> 
                    ?event(snp_error, {verify_signature_error, #{
                        operation => <<"verify_signature">>,
                        error => Error,
                        time_ms => NifTimeMs
                    }})
            end,
            Result;
        {{error, Error1}, _, _} -> {error, Error1};
        {_, {error, Error2}, _} -> {error, Error2};
        {_, _, {error, Error3}} -> {error, Error3}
    end.


%% @doc Verify message signature and address.
%% @param MsgWithJSONReport The message containing the JSON report
%% @param Address The expected address
%% @param NodeOpts Node options
%% @returns {ok, true} if signature and address are valid, {error, signature_or_address_invalid} otherwise
-spec verify_signature_and_address(term(), binary(), map()) ->
    {ok, true} | {error, signature_or_address_invalid}.
verify_signature_and_address(MsgWithJSONReport, Address, NodeOpts) ->
    Signers = hb_message:signers(MsgWithJSONReport, NodeOpts),
    ?event(snp, {verify_signature_and_address_signers, Signers}),
    SigIsValid = hb_message:verify(MsgWithJSONReport, Signers),
    ?event(snp, {verify_signature_and_address_sig_valid, SigIsValid}),
    AddressIsValid = lists:member(Address, Signers),
    ?event(snp, {verify_signature_and_address_check, #{
        address => Address,
        signers => Signers,
        address_is_valid => AddressIsValid
    }}),
    case SigIsValid andalso AddressIsValid of
        true -> 
            ?event(snp_short, {verify_signature_and_address_success, true}),
            {ok, true};
        false -> 
            ?event(snp_error, {verify_signature_and_address_failed, #{
                operation => <<"verify_signature_and_address">>,
                signature_valid => SigIsValid,
                address_valid => AddressIsValid,
                expected_address => Address,
                actual_signers => Signers,
                suggestion => case {SigIsValid, AddressIsValid} of
                    {false, _} -> <<"Message signature is invalid. Verify the message was signed correctly.">>;
                    {true, false} -> <<"Address mismatch: expected address not found in signers. Verify the message was signed by the expected address.">>
                end
            }}),
            {error, signature_or_address_invalid}
    end.

%% @doc Verify that the debug flag is disabled in the SNP policy.
%%
%% This function checks the SNP policy to ensure that debug mode is disabled,
%% which is required for production environments to maintain security guarantees.
%%
%% @param Msg The normalized SNP message containing the policy
%% @returns `{ok, true}' if debug is disabled, or `{error, debug_enabled}' if enabled
-spec verify_debug_disabled(Msg :: map()) -> {ok, true} | {error, debug_enabled}.
verify_debug_disabled(Msg) ->
    DebugDisabled = not is_debug(Msg),
    Policy = hb_ao:get(<<"policy">>, Msg, #{}),
    ?event(snp_short, {verify_debug_disabled_check, #{
        policy => Policy,
        debug_disabled => DebugDisabled
    }}),
    case DebugDisabled of
        true -> 
            ?event(snp_short, {verify_debug_disabled_success, true}),
            {ok, true};
        false -> 
            ?event(snp_error, {verify_debug_disabled_failed, #{
                operation => <<"verify_debug_disabled">>,
                policy => Policy,
                suggestion => <<"Debug mode is enabled in the SNP policy. This is not allowed in production. Disable debug mode by clearing bit ", 
                    (hb_util:bin(integer_to_list(?DEBUG_FLAG_BIT)))/binary, " in the policy field.">>
            }}),
            {error, debug_enabled}
    end.

%% Helper to check if debug is enabled in the report
-spec is_debug(Report :: map()) -> boolean().
is_debug(Report) ->
    (hb_ao:get(<<"policy">>, Report, #{}) band (1 bsl ?DEBUG_FLAG_BIT)) =/= 0.

%% @doc Verify that the measurement in the SNP report is valid.
%%
%% This function validates the SNP measurement by:
%% 1. Extracting committed parameters from the message
%% 2. Computing the expected launch digest using those parameters
%% 3. Comparing the computed digest with the measurement in the report
%%
%% @param Msg The normalized SNP message containing local hashes
%% @param ReportJSON The raw JSON report containing the measurement
%% @param NodeOpts A map of configuration options
%% @returns `{ok, true}' if the measurement is valid, or 
%% `{error, measurement_invalid}' on failure
-spec verify_measurement(Msg :: map(), ReportJSON :: binary(), 
    NodeOpts :: map()) -> {ok, true} | {error, measurement_invalid | {measurement_verification_failed, term()}}.
verify_measurement(Msg, ReportJSON, NodeOpts) ->
    Args = extract_measurement_args(Msg, NodeOpts),
    ?event(snp, {verify_measurement_args, Args}),  % Verbose: full args
    % Try to read OVMF file and extract SEV hashes table GPA
    ArgsWithGpa = case snp_ovmf:read_ovmf_gpa() of
        {ok, Gpa} -> 
            ?event(snp_short, {ovmf_gpa_found, Gpa}),
            Args#{sev_hashes_gpa => Gpa};
        {error, GpaReason} -> 
            ?event(snp, {ovmf_gpa_not_found, GpaReason}),
            Args  % Continue without GPA if file not found
    end,
    ?event(snp, {compute_launch_digest_args, ArgsWithGpa}),
    {ok, ExpectedBin} = snp_launch_digest:compute_launch_digest(ArgsWithGpa),
    ?event(snp, {expected_measurement, hb_util:to_hex(ExpectedBin)}),
    Measurement = hb_ao:get(<<"measurement">>, Msg, NodeOpts),
    ?event(snp, {actual_measurement, Measurement}),
    % verify_measurement is now implemented in Erlang
    % Returns {ok, true} on match, {ok, false} on mismatch, {error, Reason} on parse errors
    case verify_measurement(ReportJSON, ExpectedBin) of
        {ok, true} -> 
            ?event(snp_short, {verify_measurement_success, true}),
            {ok, true};
        {ok, false} -> 
            ?event(snp_error, {verify_measurement_mismatch, #{
                operation => <<"verify_measurement">>,
                expected_hex => hb_util:to_hex(ExpectedBin),
                actual_measurement => Measurement,
                suggestion => <<"Measurement mismatch indicates the launch digest does not match. Verify that all committed parameters (vcpus, vcpu_type, vmm_type, guest_features, firmware, kernel, initrd, append) match the expected values.">>
            }}),
            {error, measurement_invalid};
        {error, Reason} -> 
            % JSON parsing or other errors - distinguish from measurement mismatch
            ?event(snp_error, {measurement_verification_error, #{
                operation => <<"verify_measurement">>,
                error => Reason,
                suggestion => <<"Failed to parse or extract measurement from report. Ensure the report JSON is valid and contains a 'measurement' field.">>
            }}),
            {error, {measurement_verification_failed, Reason}}
    end.

%% @doc Extract measurement arguments from the SNP message.
%%
%% This function extracts and formats the committed parameters needed for
%% measurement computation from the local hashes in the message.
%%
%% @param Msg The normalized SNP message containing local hashes
%% @param NodeOpts A map of configuration options
%% @returns A map of measurement arguments with atom keys
-spec extract_measurement_args(Msg :: map(), NodeOpts :: map()) -> map().
extract_measurement_args(Msg, NodeOpts) ->
    maps:from_list(
        lists:map(
            fun({Key, Val}) -> {binary_to_existing_atom(Key), Val} end,
            maps:to_list(
                maps:with(
                    lists:map(fun atom_to_binary/1, ?COMMITTED_PARAMETERS),
                    hb_cache:ensure_all_loaded(
                        hb_ao:get(<<"local-hashes">>, Msg, NodeOpts),
                        NodeOpts
                    )
                )
            )
        )
    ).

%% Helper function to parse and validate report JSON
-spec parse_and_validate_report_json(ReportJSON :: binary()) -> map().
parse_and_validate_report_json(ReportJSON) ->
    Report = hb_json:decode(ReportJSON),
    ?event(snp, {report_json_decoded, #{
        is_map => is_map(Report),
        report_type => case Report of
            R when is_map(R) -> map;
            _ -> other
        end
    }}),
    case Report of
        ReportMap when is_map(ReportMap) -> 
            ?event(snp, {report_map_valid, map_size(ReportMap)}),
            ReportMap;
        Other -> 
            ReportTypeStr = case Other of
                R2 when is_map(R2) -> <<"map">>;
                L2 when is_list(L2) -> <<"list">>;
                B2 when is_binary(B2) -> <<"binary">>;
                _ -> <<"other">>
            end,
            ?event(snp_error, {report_map_invalid, #{
                operation => <<"verify_report_integrity">>,
                report_type => ReportTypeStr,
                expected => <<"map">>,
                suggestion => <<"The report JSON must decode to a map/object. Ensure the JSON is valid and properly formatted.">>
            }}),
            throw({error, invalid_report_format})
    end.

%% Helper function to extract and validate chip_id from report
-spec extract_and_validate_chip_id(ReportMap :: map()) -> binary().
extract_and_validate_chip_id(ReportMap) ->
    ChipIdRaw = hb_ao:get(<<"chip_id">>, ReportMap, undefined, #{}),
    ?event(snp, {chip_id_raw, #{
        is_list => is_list(ChipIdRaw),
        list_length => case ChipIdRaw of
            L0 when is_list(L0) -> length(L0);
            _ -> undefined
        end
    }}),
    % Use centralized ChipId validation
    ChipId = case ChipIdRaw of
        undefined -> 
            ?event(snp_error, {missing_chip_id, #{
                operation => <<"verify_report_integrity">>,
                expected_field => <<"chip_id">>,
                suggestion => <<"The report must contain a 'chip_id' field. Ensure the SNP report is complete and properly formatted.">>
            }}),
            throw({error, missing_chip_id});
        ChipIdRawValue ->
            case snp_validation:validate_chip_id(ChipIdRawValue) of
                {ok, ValidChipId} ->
                    ?event(snp_short, {chip_id_valid, byte_size(ValidChipId)}),
                    ValidChipId;
                {error, Reason} ->
                    ?event(snp_error, {invalid_chip_id_format, #{
                        operation => <<"verify_report_integrity">>,
                        error => Reason,
                        suggestion => <<"The 'chip_id' field must be a list or binary containing exactly ", 
                            (hb_util:bin(integer_to_list(?CHIP_ID_SIZE)))/binary, " bytes.">>
                    }}),
                    throw({error, {invalid_chip_id, Reason}})
            end
    end,
    ?event(snp_short, {chip_id_extracted, byte_size(ChipId)}),
    ChipId.

%% Helper function to extract and validate current_tcb map
-spec extract_and_validate_tcb(ReportMap :: map()) -> map().
extract_and_validate_tcb(ReportMap) ->
    CurrentTcbRaw = hb_ao:get(<<"current_tcb">>, ReportMap, undefined, #{}),
    ?event(snp, {current_tcb_raw, is_map(CurrentTcbRaw)}),
    case CurrentTcbRaw of
        undefined -> 
            ?event(snp_error, {missing_current_tcb, #{
                operation => <<"verify_report_integrity">>,
                expected_field => <<"current_tcb">>,
                suggestion => <<"The report must contain a 'current_tcb' field. Ensure the SNP report is complete and properly formatted.">>
            }}),
            throw({error, missing_current_tcb});
        TcbMap when is_map(TcbMap) -> 
            ?event(snp_short, {current_tcb_valid, map_size(TcbMap)}),
            TcbMap;
        InvalidTcb -> 
            ?event(snp_error, {invalid_current_tcb_format, #{
                operation => <<"verify_report_integrity">>,
                actual_type => case InvalidTcb of
                    TcbList when is_list(TcbList) -> <<"list">>;
                    TcbBin when is_binary(TcbBin) -> <<"binary">>;
                    _ -> <<"other">>
                end,
                expected => <<"map">>,
                suggestion => <<"The 'current_tcb' field must be a map/object containing bootloader, tee, snp, and microcode SPL values.">>
            }}),
            throw({error, invalid_current_tcb_format})
    end.

%% Helper function to extract SPL field from TCB map
-spec extract_spl_field(TCBMap :: map(), FieldName :: binary(), FieldLabel :: binary()) -> integer().
extract_spl_field(TCBMap, FieldName, FieldLabel) ->
    FieldRaw = hb_ao:get(FieldName, TCBMap, undefined, #{}),
    ?event(snp, {FieldLabel, is_integer(FieldRaw)}),
    case FieldRaw of
        undefined -> 
            ?event(snp_error, {missing_spl_field, #{
                operation => <<"verify_report_integrity">>,
                expected_field => <<"current_tcb.", FieldName/binary>>,
                suggestion => <<"The 'current_tcb' map must contain a '", FieldName/binary, "' field with an integer SPL value (0-255).">>
            }}),
            throw({error, {missing_spl_field, FieldName}});
        Val when is_integer(Val) -> 
            ?event(snp_short, {spl_field_valid, #{field => FieldLabel, value => Val}}),
            Val;
        Invalid -> 
            ?event(snp_error, {invalid_spl_field, #{
                operation => <<"verify_report_integrity">>,
                field => FieldLabel,
                actual_value => Invalid,
                actual_type => case Invalid of
                    I when is_integer(I) -> <<"integer">>;
                    B when is_binary(B) -> <<"binary">>;
                    L when is_list(L) -> <<"list">>;
                    _ -> <<"other">>
                end,
                expected => <<"integer in range 0-255">>,
                suggestion => <<"The '", FieldName/binary, "' SPL value must be an integer in the range 0-255.">>
            }}),
            throw({error, {invalid_spl_field, FieldName}})
    end.

%% Helper function to convert report to binary and verify signature
-spec convert_and_verify_signature(ReportJSON :: binary(), CertChainPEM :: binary(), 
    VcekDER :: binary()) -> boolean().
convert_and_verify_signature(ReportJSON, CertChainPEM, VcekDER) ->
    ?event(snp, {converting_report_json_to_binary}),  % Verbose: conversion step
    ReportBinary = case snp_report_format:report_json_to_binary(ReportJSON) of
        {error, Reason} = E -> 
            ?event(snp_error, {report_json_to_binary_error, #{
                operation => <<"verify_report_integrity">>,
                error => Reason,
                suggestion => <<"Ensure the report JSON contains all required fields and is properly formatted.">>
            }}),
            throw(E);
        Bin -> 
            ?event(snp_short, {report_json_to_binary_success, byte_size(Bin)}),
            Bin
    end,
    
    ?event(snp_short, {verifying_signature_start, #{
        report_binary_size => byte_size(ReportBinary),
        cert_chain_size => byte_size(CertChainPEM),
        vcek_size => byte_size(VcekDER)
    }}),
    {VerifyTimeMicros, VerifyResult} = timer:tc(fun() -> 
        verify_signature(ReportBinary, CertChainPEM, VcekDER) 
    end),
    VerifyTimeMs = VerifyTimeMicros / 1000,
    {ok, ReportIsValid} = VerifyResult,
    ?event(snp_short, {signature_verification_complete, #{
        is_valid => ReportIsValid,
        time_ms => VerifyTimeMs
    }}),
    ReportIsValid.

%% @doc Verify the integrity of the SNP report's digital signature.
%%
%% This function validates the cryptographic signature of the SNP report
%% against the hardware root of trust to ensure the report has not been
%% tampered with and originates from genuine AMD SEV-SNP hardware.
%%
%% The function:
%% 1. Parses the JSON report to extract chip ID and TCB version
%% 2. Fetches the certificate chain (ARK + ASK) from AMD KDS
%% 3. Fetches the VCEK certificate from AMD KDS
%% 4. Verifies the signature using the Rust NIF
%%
%% @param ReportJSON The raw JSON report to verify
%% @returns `{ok, true}' if the report signature is valid, or
%% `{error, report_signature_invalid}' on failure
-spec verify_report_integrity(ReportJSON :: binary()) ->
    {ok, true} | {error, report_signature_invalid | term()}.
verify_report_integrity(ReportJSON) ->
    ?event(snp_short, {verify_report_integrity_start, byte_size(ReportJSON)}),
    {IntegrityTimeMicros, Result} = timer:tc(fun() ->
        maybe
        % Parse and validate report JSON
        ReportMap = parse_and_validate_report_json(ReportJSON),
        
        % Extract and validate chip_id
        ChipId = extract_and_validate_chip_id(ReportMap),
        
        % Extract and validate TCB map
        CurrentTcb = extract_and_validate_tcb(ReportMap),
        
        % Extract all SPL fields
        BootloaderSPL = extract_spl_field(CurrentTcb, <<"bootloader">>, <<"bootloader_spl_raw">>),
        TeeSPL = extract_spl_field(CurrentTcb, <<"tee">>, <<"tee_spl_raw">>),
        SnpSPL = extract_spl_field(CurrentTcb, <<"snp">>, <<"snp_spl_raw">>),
        UcodeSPL = extract_spl_field(CurrentTcb, <<"microcode">>, <<"ucode_spl_raw">>),
        ?event(snp_short, {all_tcb_fields_extracted, #{
            bootloader => BootloaderSPL,
            tee => TeeSPL,
            snp => SnpSPL,
            microcode => UcodeSPL
        }}),
        
        % Fetch certificates
        {CertChainPEM, VcekDER} = snp_certificates:fetch_verification_certificates(
            ChipId, BootloaderSPL, TeeSPL, SnpSPL, UcodeSPL),
        
        % Convert and verify signature
        ReportIsValid = convert_and_verify_signature(ReportJSON, CertChainPEM, VcekDER),
        case ReportIsValid of
            true -> 
                ?event(snp_short, {verify_report_integrity_success}),
                {ok, true};
            false -> 
                ?event(snp_error, {signature_invalid, #{
                    operation => <<"verify_report_integrity">>,
                    suggestion => <<"The report signature is invalid. This may indicate a compromised or tampered report. Verify the report source and certificates.">>
                }}),
                {error, report_signature_invalid}
        end
    else
        {error, ErrorReason} -> 
            ?event(snp_error, {report_verification_error, #{
                operation => <<"verify_report_integrity">>,
                error => ErrorReason,
                suggestion => <<"Check the error details above for specific validation failures.">>
            }}),
            {error, ErrorReason}
    end
    end),
    IntegrityTimeMs = IntegrityTimeMicros / 1000,
    ?event(snp_short, {verify_report_integrity_time_ms, IntegrityTimeMs}),
    Result.

%% @doc Verify that the nonce in the report matches the expected value.
%%
%% This function validates that the nonce in the SNP report was generated
%% using the correct address and node message ID, ensuring the report
%% corresponds to the expected request.
%%
%% @param Address The node's address used in nonce generation
%% @param NodeMsgID The node message ID used in nonce generation
%% @param Msg The normalized SNP message containing the nonce
%% @param NodeOpts A map of configuration options
%% @returns `{ok, true}' if the nonce matches, or `{error, nonce_mismatch}' on failure
-spec verify_nonce(Address :: binary(), NodeMsgID :: binary(), 
    Msg :: map(), NodeOpts :: map()) -> {ok, true} | {error, nonce_mismatch}.
verify_nonce(Address, NodeMsgID, Msg, NodeOpts) ->
    Nonce = hb_util:decode(hb_ao:get(<<"nonce">>, Msg, NodeOpts)),
    ?event(snp, {snp_nonce, Nonce}),
    NonceMatches = snp_nonce:report_data_matches(Address, NodeMsgID, Nonce),
    ?event(snp, {nonce_matches, NonceMatches}),
    case NonceMatches of
        true -> 
            ?event(snp_short, {verify_nonce_success, true}),
            {ok, true};
        false -> 
            ?event(snp_error, {verify_nonce_mismatch, #{
                operation => <<"verify_nonce">>,
                address => Address,
                node_msg_id => NodeMsgID,
                nonce => Nonce,
                suggestion => <<"Nonce mismatch indicates the report was not generated for this specific address and message ID. Verify the report corresponds to the expected request.">>
            }}),
            {error, nonce_mismatch}
    end.

%% @doc Verify that the software configuration is trusted.
%%
%% This function validates that the firmware, kernel, and other system
%% components match approved configurations by delegating to the
%% software trust validation system.
%%
%% @param M1 The previous message in the verification chain
%% @param Msg The normalized SNP message containing software hashes
%% @param NodeOpts A map of configuration options including trusted software list
%% @returns `{ok, true}' if the software is trusted, or `{error, untrusted_software}' 
%% on failure
-spec verify_trusted_software(M1 :: term(), Msg :: map(), NodeOpts :: map()) ->
    verification_result().
verify_trusted_software(M1, Msg, NodeOpts) ->
    {ok, IsTrustedSoftware} = snp_trust:execute_is_trusted(M1, Msg, NodeOpts),
    ?event(snp_short, {trusted_software, IsTrustedSoftware}),
    case IsTrustedSoftware of
        true -> 
            ?event(snp_short, {verify_trusted_software_success, true}),
            {ok, true};
        false -> 
            ?event(snp_error, {verify_trusted_software_failed, #{
                operation => <<"verify_trusted_software">>,
                suggestion => <<"The software configuration (firmware, kernel, etc.) does not match the trusted software list. Ensure all software components are approved and match the expected hashes.">>
            }}),
            {error, untrusted_software}
    end.

%% @doc Determine if an error is a verification failure (report is invalid)
%% vs a system error (missing config, network failure, etc.)
%% Verification failures should return {ok, false}, system errors should propagate
-spec is_verification_failure(Reason :: term()) -> boolean().
is_verification_failure(Reason) ->
    case Reason of
        nonce_mismatch -> true;
        signature_or_address_invalid -> true;
        debug_enabled -> true;
        untrusted_software -> true;
        measurement_invalid -> true;
        report_signature_invalid -> true;
        {measurement_verification_failed, _} -> true;  % Measurement parse error treated as verification failure
        _ -> false  % All other errors are system errors
    end.

%% @doc Verify an AMD SEV-SNP commitment report message.
%%
%% This function validates the identity of a remote node, its ephemeral private
%% address, and the integrity of the hardware-backed attestation report.
%% The verification process performs the following checks:
%% 1. Verify the address and the node message ID are the same as the ones
%%    used to generate the nonce.
%% 2. Verify the address that signed the message is the same as the one used
%%    to generate the nonce.
%% 3. Verify that the debug flag is disabled.
%% 4. Verify that the firmware, kernel, and OS (VMSAs) hashes, part of the
%%    measurement, are trusted.
%% 5. Verify the measurement is valid.
%% 6. Verify the report's certificate chain to hardware root of trust.
%%
%% Required configuration in NodeOpts map:
%% - snp_trusted: List of trusted software configurations
%% - snp_enforced_keys: Keys to enforce during validation (optional)
%%
%% @param M1 The previous message in the verification chain
%% @param M2 The message containing the SNP commitment report
%% @param NodeOpts A map of configuration options for verification
%% @returns `{ok, true}' on successful verification, `{ok, false}' on verification
%% failure (report is invalid), or `{error, Reason}' on system errors
%% (missing config, network failures, etc.)
-spec verify(M1 :: term(), M2 :: term(), NodeOpts :: map()) ->
    {ok, boolean()} | {error, term()}.
verify(M1, M2, NodeOpts) ->
    ?event(snp_short, {verify_called}),
    {VerifyTimeMicros, Result} = timer:tc(fun() ->
        maybe
            % Validate configuration options
            {ok, _} ?= validate_verify_config(NodeOpts),
            {ok, {Msg, Address, NodeMsgID, ReportJSON, MsgWithJSONReport}} 
                ?= snp_message:extract_and_normalize_message(M2, NodeOpts),
            % Perform all validation steps
            {ok, NonceResult} ?= verify_nonce(Address, NodeMsgID, Msg, NodeOpts),
            {ok, SigResult} ?= 
                verify_signature_and_address(
                    MsgWithJSONReport, 
                    Address, 
                    NodeOpts
                ),
            {ok, DebugResult} ?= verify_debug_disabled(Msg),
            {ok, TrustedResult} ?= verify_trusted_software(M1, Msg, NodeOpts),
            {ok, MeasurementResult} ?= verify_measurement(Msg, ReportJSON, NodeOpts),
            {ok, ReportResult} ?= verify_report_integrity(ReportJSON),
            Valid = lists:all(
                fun(Bool) -> Bool end, 
                    [
                        NonceResult, 
                        SigResult, 
                        DebugResult, 
                        TrustedResult, 
                        MeasurementResult, 
                        ReportResult
                    ]
                ),
            ?event(snp_short, {final_validation_result, Valid}),
            % Return boolean value (not binary) for consistency with dev_message:verify expectations
            % dev_message:verify_commitment expects {ok, boolean()}, so we must return {ok, false}
            % for verification failures, not {error, ...}
            {ok, Valid}
        else
            % Distinguish between verification failures and system errors
            % Verification failures (report is invalid) should return {ok, false}
            % System errors (missing config, network failures, etc.) should return {error, Reason}
            % even if it crashes dev_message:verify_commitment, because these indicate
            % exceptional conditions that need to be handled differently
            {error, Reason} = ErrorTuple ->
                case is_verification_failure(Reason) of
                    true ->
                        % Verification failure: report is invalid
                        ?event(snp_error, {snp_verification_failed, #{
                            operation => <<"verify">>,
                            reason => Reason,
                            suggestion => <<"The SNP report failed verification. Check individual validation steps above for details.">>
                        }}),
                        {ok, false};
                    false ->
                        % System error: propagate to caller
                        ?event(snp_error, {snp_system_error, #{
                            operation => <<"verify">>,
                            reason => Reason,
                            suggestion => <<"System error during verification. Check network connectivity, configuration, and system resources.">>
                        }}),
                        ErrorTuple
                end;
            Error ->
                % Unexpected error (exception, etc.) - treat as system error
                ?event(snp_error, {snp_system_error, #{
                    operation => <<"verify">>,
                    error => Error,
                    suggestion => <<"Unexpected error during verification. Check system logs for details.">>
                }}),
                {error, Error}
        end
    end),
    VerifyTimeMs = VerifyTimeMicros / 1000,
    ?event(snp_short, {verify_total_time_ms, VerifyTimeMs}),
    Result.

