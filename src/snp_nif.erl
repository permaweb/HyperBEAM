%%% @doc Main NIF interface layer for SNP commitment reports.
%%%
%%% This module provides the main interface for SNP operations, delegating
%%% to specialized modules for different aspects of SNP functionality.
%%% It maintains backward compatibility with the original dev_snp_nif API.
-module(snp_nif).
-export([generate_attestation_report/2, compute_launch_digest/1, check_snp_support/0]).
-export([verify_measurement/2, verify_signature/3]).
-export([fetch_cert_chain/1, fetch_vcek/6]).
-export([report_binary_to_json/1, report_json_to_binary/1]).
-export([pem_to_der_chain/1, pem_cert_to_der/1]).
-export([parse_ovmf_sev_hashes_gpa/1]).
-export([verify_signature_nif/3, verify_report_signature/2]).

-include("include/hb.hrl").
-include_lib("public_key/include/public_key.hrl").

-on_load(init/0).

%% @doc Check if SEV-SNP is supported on the current system.
%% This function will be replaced by the C NIF when loaded.
-spec check_snp_support() -> {ok, boolean()} | {error, term()}.
check_snp_support() ->
    erlang:nif_error(not_loaded).

%% @doc Generate an attestation report from the SEV-SNP hardware.
%% This function will be replaced by the C NIF when loaded.
-spec generate_attestation_report(UniqueData :: binary(), VMPL :: 0..3) ->
    {ok, binary()} | {error, {integer(), binary()}}.
generate_attestation_report(_UniqueData, _VMPL) ->
    erlang:nif_error(not_loaded).

%% @doc Compute launch digest.
%% Delegates to snp_launch_digest module.
-spec compute_launch_digest(Args :: map()) -> {ok, binary()} | {error, term()}.
compute_launch_digest(Args) ->
    snp_launch_digest:compute_launch_digest(Args).

%% @doc Verify that the measurement in the report matches the expected measurement.
%% Delegates to snp_verification module.
-spec verify_measurement(ReportJSON :: binary(), ExpectedMeasurement :: binary()) ->
    {ok, true} | {ok, false} | {error, binary()}.
verify_measurement(ReportJSON, ExpectedMeasurement) ->
    snp_verification:verify_measurement(ReportJSON, ExpectedMeasurement).

%% @doc Verify the signature of an attestation report.
%% Delegates to snp_verification module.
-spec verify_signature(ReportBinary :: binary(), CertChainPEM :: binary(), VcekDER :: binary()) ->
    {ok, true} | {error, binary() | {term(), binary()}}.
verify_signature(ReportBinary, CertChainPEM, VcekDER) ->
    snp_verification:verify_signature(ReportBinary, CertChainPEM, VcekDER).

%% @doc Fetches the AMD certificate chain (ASK + ARK) for the given SEV product name.
%% Delegates to snp_certificates module.
-spec fetch_cert_chain(SevProdName :: undefined | binary() | string()) ->
    {ok, binary()} | {error, term()}.
fetch_cert_chain(SevProdName) ->
    snp_certificates:fetch_cert_chain(SevProdName).

%% @doc Fetches the VCEK certificate for the given chip ID and TCB version.
%% Delegates to snp_certificates module.
-spec fetch_vcek(ChipId :: binary(), BootloaderSPL :: integer(), TeeSPL :: integer(),
                 SnpSPL :: integer(), UcodeSPL :: integer(), SevProdName :: undefined | binary() | string()) ->
    {ok, binary()} | {error, term()}.
fetch_vcek(ChipId, BootloaderSPL, TeeSPL, SnpSPL, UcodeSPL, SevProdName) ->
    snp_certificates:fetch_vcek(ChipId, BootloaderSPL, TeeSPL, SnpSPL, UcodeSPL, SevProdName).

%% @doc Convert binary report structure (1184 bytes) to JSON map.
%% Delegates to snp_report_format module.
-spec report_binary_to_json(ReportBinary :: binary()) -> map() | {error, binary()}.
report_binary_to_json(ReportBinary) ->
    snp_report_format:report_binary_to_json(ReportBinary).

%% @doc Convert JSON report map to binary report structure (1184 bytes).
%% Delegates to snp_report_format module.
-spec report_json_to_binary(ReportJSON :: binary() | map()) -> binary() | {error, term()}.
report_json_to_binary(ReportJSON) ->
    snp_report_format:report_json_to_binary(ReportJSON).

%% @doc Convert PEM certificate chain to DER-encoded binary.
%% Delegates to snp_certificates module.
-spec pem_to_der_chain(CertChainPEM :: binary()) -> binary() | {error, term()}.
pem_to_der_chain(CertChainPEM) ->
    snp_certificates:pem_to_der_chain(CertChainPEM).

%% @doc Convert a single PEM certificate to DER.
%% Delegates to snp_certificates module.
-spec pem_cert_to_der(CertPEM :: binary()) -> binary() | {error, term()}.
pem_cert_to_der(CertPEM) ->
    snp_certificates:pem_cert_to_der(CertPEM).

%% @doc Parse OVMF file to extract SEV hashes table GPA.
%% Delegates to snp_ovmf module.
-spec parse_ovmf_sev_hashes_gpa(OvmfPath :: string() | binary()) ->
    {ok, non_neg_integer()} | {error, term()}.
parse_ovmf_sev_hashes_gpa(OvmfPath) ->
    snp_ovmf:parse_ovmf_sev_hashes_gpa(OvmfPath).

%% @doc Verify signature - calls C NIF for actual verification.
%% This function verifies both the certificate chain (ARK -> ASK -> VCEK) and
%% the report signature. The C NIF uses OpenSSL to perform full cryptographic
%% chain verification, including RSASSA-PSS signature support.
%%
%% The certificate chain verification ensures:
%% 1. VCEK is signed by ASK
%% 2. ASK is signed by ARK (root of trust)
%% 3. Report signature is valid using VCEK's public key
%%
%% This provides full cryptographic verification of the attestation report's
%% authenticity, rather than relying solely on fetching certificates from AMD's KDS.
-spec verify_signature_nif(ReportBinary :: binary(), CertChainDER :: binary(), VcekDER :: binary()) ->
    {ok, true} | {error, term()}.
verify_signature_nif(_ReportBinary, _CertChainDER, _VcekDER) ->
    % C NIF handles both certificate chain verification and report signature verification
    % This will be replaced by the C NIF when loaded
    erlang:nif_error(not_loaded).

%% @doc Verify report signature - calls C NIF for actual verification.
%% This function will be replaced by the C NIF when loaded.
-spec verify_report_signature(ReportBinary :: binary(), VcekDER :: binary()) ->
    {ok, true} | {error, term()}.
verify_report_signature(_ReportBinary, _VcekDER) ->
    erlang:nif_error(not_loaded).

init() ->
    SoName = filename:join([code:priv_dir(hb), "snp_nif"]),
    erlang:load_nif(SoName, 0).

