%%% @doc This device provides an interface for validating and generating AMD SEV-SNP 
%%% commitment reports.
%%%
%%% AMD SEV-SNP (Secure Encrypted Virtualization - Secure Nested Paging) is a 
%%% hardware-based security technology that provides confidential computing 
%%% capabilities. This module handles the cryptographic validation of attestation 
%%% reports and the generation of commitment reports for trusted execution environments.
%%%
%%% The device supports two main operations:
%%% 1. Verification of remote node attestation reports with comprehensive validation
%%% 2. Generation of local attestation reports for proving node identity and software integrity
-module(dev_snp).
-export([generate/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Configuration constants
-define(COMMITTED_PARAMETERS, [vcpus, vcpu_type, vmm_type, guest_features,
    firmware, kernel, initrd, append]).

%% SNP-specific constants
-define(DEBUG_FLAG_BIT, 19).
-define(REPORT_DATA_VERSION, 1).

%% Test configuration constants
-define(TEST_VCPUS_COUNT, 32).
-define(TEST_VCPU_TYPE, 5).
-define(TEST_VMM_TYPE, 1).
-define(TEST_GUEST_FEATURES, 1).
-define(TEST_FIRMWARE_HASH, <<"b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510">>).
-define(TEST_KERNEL_HASH, <<"69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576">>).
-define(TEST_INITRD_HASH, <<"544045560322dbcd2c454bdc50f35edf0147829ec440e6cb487b4a1503f923c1">>).
-define(TEST_APPEND_HASH, <<"95a34faced5e487991f9cc2253a41cbd26b708bf00328f98dddbbf6b3ea2892e">>).

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
%% @returns `{ok, Binary}' with "true" on successful verification, or
%% `{error, Reason}' on failure with specific error details
-spec verify(M1 :: term(), M2 :: term(), NodeOpts :: map()) ->
    {ok, binary()} | {error, term()}.
verify(M1, M2, NodeOpts) ->
    ?event(snp_verify, verify_called),
    maybe
        {ok, {Msg, Address, NodeMsgID, ReportJSON, MsgWithJSONReport}} 
            ?= extract_and_normalize_message(M2, NodeOpts),
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
        ?event({final_validation_result, Valid}),
        {ok, hb_util:bin(Valid)}
    else
        {error, Reason} -> {error, Reason}
    end.

%% @doc Generate an AMD SEV-SNP commitment report and emit it as a message.
%%
%% This function creates a hardware-backed attestation report containing all
%% necessary data to validate the node's identity and software configuration.
%% The generation process performs the following operations:
%% 1. Loads and validates the provided configuration options
%% 2. Retrieves or creates a cryptographic wallet for node identity
%% 3. Generates a unique nonce using the node's address and message ID
%% 4. Extracts trusted software configuration from local options
%% 5. Generates the hardware attestation report using the NIF interface
%% 6. Packages the report with all verification data into a message
%%
%% Required configuration in Opts map:
%% - priv_wallet: Node's cryptographic wallet (created if not provided)
%% - snp_trusted: List of trusted software configurations (represents the 
%% configuration of the local node generating the report)
%%
%% @param _M1 Ignored parameter
%% @param _M2 Ignored parameter  
%% @param Opts A map of configuration options for report generation
%% @returns `{ok, Map}' on success with the complete report message, or
%% `{error, Reason}' on failure with error details
-spec generate(M1 :: term(), M2 :: term(), Opts :: map()) ->
    {ok, map()} | {error, term()}.
generate(_M1, _M2, Opts) ->
    maybe
        LoadedOpts = hb_cache:ensure_all_loaded(Opts, Opts),
        ?event({generate_opts, {explicit, LoadedOpts}}),
        % Validate wallet availability
        {ok, ValidWallet} ?= 
            case hb_opts:get(priv_wallet, no_viable_wallet, LoadedOpts) of
                no_viable_wallet -> {error, no_wallet_available};
                Wallet -> {ok, Wallet}
            end,
        % Generate address and node message components
        Address = hb_util:human_id(ar_wallet:to_address(ValidWallet)),
        NodeMsg = hb_private:reset(LoadedOpts),
        {ok, PublicNodeMsgID} ?= dev_message:id(
            NodeMsg,
            #{ <<"committers">> => <<"none">> },
            LoadedOpts
        ),
        RawPublicNodeMsgID = hb_util:native_id(PublicNodeMsgID),
        ?event({snp_node_msg, NodeMsg}),
        % Generate the commitment report components
        ?event({snp_address, byte_size(Address)}),
        ReportData = generate_nonce(Address, RawPublicNodeMsgID),
        ?event({snp_report_data, byte_size(ReportData)}),
        % Extract local hashes
        {ok, ValidLocalHashes} ?= 
            case hb_opts:get(snp_trusted, [#{}], LoadedOpts) of
                [] -> {error, no_trusted_configs};
                [FirstConfig | _] -> {ok, FirstConfig};
                _ -> {error, invalid_trusted_configs_format}
            end,
        ?event(snp_local_hashes, {explicit, ValidLocalHashes}),
        % Generate the hardware attestation report
        {ok, ReportJSON} ?= case get(mock_snp_nif_enabled) of
            true ->
                % Return mocked response for testing
                MockResponse = get(mock_snp_nif_response),
                {ok, MockResponse};
            _ ->
                % Call actual NIF function
                dev_snp_nif:generate_attestation_report(
                    ReportData, 
                    ?REPORT_DATA_VERSION
                )
        end,
        ?event({snp_report_json, ReportJSON}),
        ?event({snp_report_generated, {nonce, ReportData}, {report, ReportJSON}}),
        % Package the complete report message
        ReportMsg = #{
            <<"local-hashes">> => ValidLocalHashes,
            <<"nonce">> => hb_util:encode(ReportData),
            <<"address">> => Address,
            <<"node-message">> => NodeMsg,
            <<"report">> => ReportJSON
        },
        ?event({snp_report_msg, ReportMsg}),
        {ok, ReportMsg}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, Error}
    end.

%% @doc Extract and normalize the SNP commitment message from the input.
%%
%% This function processes the raw message and extracts all necessary components
%% for verification:
%% 1. Searches for a `body' key in the message, using it as the report source
%% 2. Applies message commitment and signing filters
%% 3. Extracts and decodes the JSON report
%% 4. Normalizes the message structure by merging report data
%% 5. Extracts the node address and message ID
%%
%% @param M2 The input message containing the SNP report
%% @param NodeOpts A map of configuration options
%% @returns `{ok, {Msg, Address, NodeMsgID, ReportJSON, MsgWithJSONReport}}'
%% on success with all extracted components, or `{error, Reason}' on failure
-spec extract_and_normalize_message(M2 :: term(), NodeOpts :: map()) ->
    {ok, {map(), binary(), binary(), binary(), map()}} | {error, term()}.
extract_and_normalize_message(M2, NodeOpts) ->
    maybe
        % Search for a `body' key in the message, and if found use it as the source
        % of the report. If not found, use the message itself as the source.
        ?event({node_opts, {explicit, NodeOpts}}),
        RawMsg = hb_ao:get(<<"body">>, M2, M2, NodeOpts#{ hashpath => ignore }),
        ?event({msg, {explicit, RawMsg}}),
        MsgWithJSONReport =
            hb_util:ok(
                hb_message:with_only_committed(
                    hb_message:with_only_committers(
                        RawMsg,
                        hb_message:signers(
                    RawMsg,
                            NodeOpts
                        ),
                        NodeOpts
                    ),
                    NodeOpts
                )
            ),
        ?event({msg_with_json_report, {explicit, MsgWithJSONReport}}),
        % Normalize the request message
        ReportJSON = hb_ao:get(<<"report">>, MsgWithJSONReport, NodeOpts),
        Report = hb_json:decode(ReportJSON),
        Msg =
            maps:merge(
                maps:without([<<"report">>], MsgWithJSONReport),
                Report
            ),
        
        % Extract address and node message ID
        Address = hb_ao:get(<<"address">>, Msg, NodeOpts),
        ?event({snp_address, Address}),
        {ok, NodeMsgID} ?= extract_node_message_id(Msg, NodeOpts),
        ?event({snp_node_msg_id, NodeMsgID}),
        {ok, {Msg, Address, NodeMsgID, ReportJSON, MsgWithJSONReport}}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, Error}
    end.


%% @doc Extract the node message ID from the SNP message.
%%
%% This function handles the extraction of the node message ID, which can be
%% provided either directly as a field or embedded within a node message that
%% needs to be processed to generate the ID.
%%
%% @param Msg The normalized SNP message
%% @param NodeOpts A map of configuration options
%% @returns `{ok, NodeMsgID}' on success with the extracted ID, or
%% `{error, missing_node_msg_id}' if no ID can be found
-spec extract_node_message_id(Msg :: map(), NodeOpts :: map()) ->
    {ok, binary()} | {error, missing_node_msg_id}.
extract_node_message_id(Msg, NodeOpts) ->
    case {hb_ao:get(<<"node-message">>, Msg, NodeOpts#{ hashpath => ignore }),
          hb_ao:get(<<"node-message-id">>, Msg, NodeOpts)} of
        {undefined, undefined} ->
            {error, missing_node_msg_id};
        {undefined, ID} ->
            {ok, ID};
        {NodeMsg, _} ->
            dev_message:id(NodeMsg, #{}, NodeOpts)
    end.

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
    ?event({snp_nonce, Nonce}),
    NonceMatches = report_data_matches(Address, NodeMsgID, Nonce),
    ?event({nonce_matches, NonceMatches}),
    case NonceMatches of
        true -> {ok, true};
        false -> {error, nonce_mismatch}
    end.

%% @doc Verify that the message signature and signing address are valid.
%%
%% This function validates that:
%% 1. The message signature is cryptographically valid
%% 2. The address that signed the message matches the address in the report
%%
%% @param MsgWithJSONReport The message containing the JSON report and signatures
%% @param Address The expected signing address from the report
%% @param NodeOpts A map of configuration options
%% @returns `{ok, true}' if both signature and address are valid, or
%% `{error, signature_or_address_invalid}' on failure
-spec verify_signature_and_address(MsgWithJSONReport :: map(), 
    Address :: binary(), NodeOpts :: map()) ->
    {ok, true} | {error, signature_or_address_invalid}.
verify_signature_and_address(MsgWithJSONReport, Address, NodeOpts) ->
    Signers = hb_message:signers(MsgWithJSONReport, NodeOpts),
    ?event({snp_signers, {explicit, Signers}}),
    SigIsValid = hb_message:verify(MsgWithJSONReport, Signers),
    ?event({snp_sig_is_valid, SigIsValid}),
    AddressIsValid = lists:member(Address, Signers),
    ?event({address_is_valid, AddressIsValid, {signer, Signers}, {address, Address}}),
    case SigIsValid andalso AddressIsValid of
        true -> {ok, true};
        false -> {error, signature_or_address_invalid}
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
    ?event({debug_disabled, DebugDisabled}),
    case DebugDisabled of
        true -> {ok, true};
        false -> {error, debug_enabled}
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
    {ok, true} | {error, untrusted_software}.
verify_trusted_software(M1, Msg, NodeOpts) ->
    {ok, IsTrustedSoftware} = execute_is_trusted(M1, Msg, NodeOpts),
    ?event({trusted_software, IsTrustedSoftware}),
    case IsTrustedSoftware of
        true -> {ok, true};
        false -> {error, untrusted_software}
    end.

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
    NodeOpts :: map()) -> {ok, true} | {error, measurement_invalid}.
verify_measurement(Msg, ReportJSON, NodeOpts) ->
    Args = extract_measurement_args(Msg, NodeOpts),
    ?event({args, { explicit, Args}}),
    {ok, Expected} = dev_snp_nif:compute_launch_digest(Args),
    ExpectedBin = list_to_binary(Expected),
    ?event({expected_measurement, {explicit, Expected}}),
    Measurement = hb_ao:get(<<"measurement">>, Msg, NodeOpts),
    ?event({measurement, {explicit,Measurement}}),
    {Status, MeasurementIsValid} =
        dev_snp_nif:verify_measurement(
            ReportJSON,
            ExpectedBin
        ),
    ?event({status, Status}),
    ?event({measurement_is_valid, MeasurementIsValid}),
    case MeasurementIsValid of
        true -> {ok, true};
        false -> {error, measurement_invalid}
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

%% @doc Verify the integrity of the SNP report's digital signature.
%%
%% This function validates the cryptographic signature of the SNP report
%% against the hardware root of trust to ensure the report has not been
%% tampered with and originates from genuine AMD SEV-SNP hardware.
%%
%% @param ReportJSON The raw JSON report to verify
%% @returns `{ok, true}' if the report signature is valid, or
%% `{error, report_signature_invalid}' on failure
-spec verify_report_integrity(ReportJSON :: binary()) ->
    {ok, true} | {error, report_signature_invalid}.
verify_report_integrity(ReportJSON) ->
    {ok, ReportIsValid} = dev_snp_nif:verify_signature(ReportJSON),
    ?event({report_is_valid, ReportIsValid}),
    case ReportIsValid of
        true -> {ok, true};
        false -> {error, report_signature_invalid}
    end.

%% @doc Check if the node's debug policy is enabled.
%%
%% This function examines the SNP policy field to determine if debug mode
%% is enabled by checking the debug flag bit in the policy bitmask.
%%
%% @param Report The SNP report containing the policy field
%% @returns `true' if debug mode is enabled, `false' otherwise
-spec is_debug(Report :: map()) -> boolean().
is_debug(Report) ->
    (hb_ao:get(<<"policy">>, Report, #{}) band (1 bsl ?DEBUG_FLAG_BIT)) =/= 0.


%% @doc Validate that all software hashes match trusted configurations.
%%
%% This function ensures that the firmware, kernel, and other system components
%% in the SNP report match approved configurations. The validation process:
%% 1. Extracts local hashes from the message
%% 2. Filters hashes to only include enforced keys
%% 3. Compares filtered hashes against trusted software configurations
%% 4. Returns true only if the configuration matches a trusted entry
%%
%% Configuration options in NodeOpts map:
%% - snp_trusted: List of maps containing trusted software configurations
%% - snp_enforced_keys: Keys to enforce during validation (defaults to all 
%%   committed parameters)
%%
%% @param _M1 Ignored parameter
%% @param Msg The SNP message containing local software hashes
%% @param NodeOpts A map of configuration options including trusted software
%% @returns `{ok, true}' if software is trusted, `{ok, false}' otherwise
-spec execute_is_trusted(M1 :: term(), Msg :: map(), NodeOpts :: map()) ->
    {ok, boolean()}.
execute_is_trusted(_M1, Msg, NodeOpts) ->
    FilteredLocalHashes = get_filtered_local_hashes(Msg, NodeOpts),
    TrustedSoftware = hb_opts:get(snp_trusted, [#{}], NodeOpts),
    ?event({trusted_software, {explicit, TrustedSoftware}}),
    IsTrusted = 
        is_software_trusted(
            FilteredLocalHashes, 
            TrustedSoftware, 
            NodeOpts
        ),
    ?event({is_all_software_trusted, IsTrusted}),
    {ok, IsTrusted}.

%% @doc Extract local hashes filtered to only include enforced keys.
%%
%% This function retrieves the local software hashes from the message and
%% filters them to only include the keys that are configured for enforcement.
%%
%% @param Msg The SNP message containing local hashes
%% @param NodeOpts A map of configuration options
%% @returns A map of filtered local hashes with only enforced keys
-spec get_filtered_local_hashes(Msg :: map(), NodeOpts :: map()) -> map().
get_filtered_local_hashes(Msg, NodeOpts) ->
    LocalHashes = hb_ao:get(<<"local-hashes">>, Msg, NodeOpts),
    EnforcedKeys = get_enforced_keys(NodeOpts),
    ?event({enforced_keys, {explicit, EnforcedKeys}}),
    FilteredLocalHashes = hb_cache:ensure_all_loaded(
        maps:with(EnforcedKeys, LocalHashes),
        NodeOpts
    ),
    ?event({filtered_local_hashes, {explicit, FilteredLocalHashes}}),
    FilteredLocalHashes.

%% @doc Get the list of enforced keys for software validation.
%%
%% This function retrieves the configuration specifying which software
%% component keys should be enforced during trust validation.
%%
%% @param NodeOpts A map of configuration options
%% @returns A list of binary keys that should be enforced
-spec get_enforced_keys(NodeOpts :: map()) -> [binary()].
get_enforced_keys(NodeOpts) ->
    lists:map(
        fun atom_to_binary/1,
        hb_opts:get(snp_enforced_keys, ?COMMITTED_PARAMETERS, NodeOpts)
    ).

%% @doc Check if filtered local hashes match any trusted configurations.
%%
%% This function compares the filtered local hashes against a list of
%% trusted software configurations, returning true if any configuration
%% matches exactly. It handles three cases:
%% 1. Empty list of trusted configurations (returns false)
%% 2. Valid list of trusted configurations (performs matching)
%% 3. Invalid trusted software configuration (returns false)
%%
%% @param FilteredLocalHashes The software hashes to validate
%% @param TrustedSoftware List of trusted software configurations or invalid input
%% @param NodeOpts Configuration options for matching
%% @returns `true' if hashes match a trusted configuration, `false' otherwise
-spec is_software_trusted(map(), [] | [map()] | term(), map()) -> boolean().
is_software_trusted(_FilteredLocalHashes, [], _NodeOpts) ->
    false;
is_software_trusted(FilteredLocalHashes, TrustedSoftware, NodeOpts) 
    when is_list(TrustedSoftware) ->
    lists:any(
        fun(TrustedMap) ->
            Match = 
                hb_message:match(
                    FilteredLocalHashes,
                    TrustedMap,
                    primary, 
                    NodeOpts
                ),
            ?event({match, {explicit, Match}}),
            is_map(TrustedMap) andalso Match == true
        end,
        TrustedSoftware
    );
is_software_trusted(_FilteredLocalHashes, _TrustedSoftware, _NodeOpts) ->
    false.

%% @doc Validate that the report data matches the expected nonce.
%%
%% This function ensures that the nonce in the SNP report was generated
%% using the same address and node message ID that are expected for this
%% verification request.
%%
%% @param Address The node's address used in nonce generation
%% @param NodeMsgID The node message ID used in nonce generation  
%% @param ReportData The actual nonce data from the SNP report
%% @returns `true' if the report data matches the expected nonce, `false' otherwise
-spec report_data_matches(Address :: binary(), NodeMsgID :: binary(), 
    ReportData :: binary()) -> boolean().
report_data_matches(Address, NodeMsgID, ReportData) ->
    ?event({generated_nonce, {explicit, generate_nonce(Address, NodeMsgID)}}),
    ?event({expected_nonce, {explicit, ReportData}}),
    generate_nonce(Address, NodeMsgID) == ReportData.

%% @doc Generate the nonce to use in the SNP commitment report.
%%
%% This function creates a unique nonce by concatenating the node's native
%% address and message ID. This nonce is embedded in the hardware attestation
%% report to bind it to a specific verification request.
%%
%% @param RawAddress The node's raw address identifier
%% @param RawNodeMsgID The raw node message identifier
%% @returns A binary nonce formed by concatenating the native address and message ID
-spec generate_nonce(RawAddress :: binary(), RawNodeMsgID :: binary()) -> binary().
generate_nonce(RawAddress, RawNodeMsgID) ->
    Address = hb_util:native_id(RawAddress),
    NodeMsgID = hb_util:native_id(RawNodeMsgID),
    << Address/binary, NodeMsgID/binary >>.

%% Test helper functions and data
get_test_hashes() ->
    #{
        <<"vcpus">> => ?TEST_VCPUS_COUNT,
        <<"vcpu_type">> => ?TEST_VCPU_TYPE,
        <<"vmm_type">> => ?TEST_VMM_TYPE,
        <<"guest_features">> => ?TEST_GUEST_FEATURES,
        <<"firmware">> => ?TEST_FIRMWARE_HASH,
        <<"kernel">> => ?TEST_KERNEL_HASH,
        <<"initrd">> => ?TEST_INITRD_HASH,
        <<"append">> => ?TEST_APPEND_HASH
    }.

%% Verification test helpers
setup_test_nodes() ->
    ProxyWallet = hb:wallet(<<"test/admissible-report-wallet.json">>),
    ProxyOpts = #{
        store => hb_opts:get(store),
        priv_wallet => ProxyWallet
    },
    _ReportNode = hb_http_server:start_node(ProxyOpts),
    VerifyingNode = hb_http_server:start_node(#{
        priv_wallet => ar_wallet:new(),
        store => hb_opts:get(store),
        snp_trusted => [
            #{
                <<"vcpus">> => ?TEST_VCPUS_COUNT,
                <<"vcpu_type">> => ?TEST_VCPU_TYPE,
                <<"vmm_type">> => ?TEST_VMM_TYPE,
                <<"guest_features">> => ?TEST_GUEST_FEATURES,
                <<"firmware">> => ?TEST_FIRMWARE_HASH,
                <<"kernel">> => ?TEST_KERNEL_HASH,
                <<"initrd">> => ?TEST_INITRD_HASH,
                <<"append">> => ?TEST_APPEND_HASH
            }
        ],
        snp_enforced_keys => [
            vcpu_type, vmm_type, guest_features,
            firmware, kernel, initrd, append
        ]
    }),
    {ProxyOpts, VerifyingNode}.


%% @doc Load test SNP report data from file.
%%
%% This function loads a sample SNP attestation report from a test file.
%% The test will fail if the file doesn't exist, ensuring predictable test data.
%%
%% @returns Binary containing test SNP report JSON data
%% @throws {error, {file_not_found, Filename}} if test file doesn't exist
-spec load_test_report_data() -> binary().
load_test_report_data() ->
    TestFile = <<"test/admissible-report.json">>,
    case file:read_file(TestFile) of
        {ok, Data} -> 
            Data;
        {error, enoent} ->
            throw({error, {file_not_found, TestFile}});
        {error, Reason} ->
            throw({error, {file_read_error, TestFile, Reason}})
    end.


%% Individual test cases
execute_is_trusted_exact_match_should_fail_test() ->
    % Test case: Exact match with trusted software should fail when vcpus differ
    Msg = #{
        <<"local-hashes">> => (get_test_hashes())#{
            <<"vcpus">> => 16
        }
    },
    NodeOpts = #{
        snp_trusted => [get_test_hashes()],
        snp_enforced_keys => [
            vcpus, vcpu_type, vmm_type, guest_features,
            firmware, kernel, initrd, append
        ]
    },
    {ok, Result} = execute_is_trusted(#{}, Msg, NodeOpts),
    ?assertEqual(false, Result).

execute_is_trusted_subset_match_should_pass_test() ->
    % Test case: Match with subset of keys in trusted software should pass
    Msg = #{
        <<"local-hashes">> => (get_test_hashes())#{
            <<"vcpus">> => 16
        }
    },
    NodeOpts = #{
        snp_trusted => [get_test_hashes()],
        snp_enforced_keys => [
            vcpu_type, vmm_type, guest_features,
            firmware, kernel, initrd, append
        ]
    },
    {ok, Result} = execute_is_trusted(#{}, Msg, NodeOpts),
    ?assertEqual(true, Result).

verify_test() ->
    {ProxyOpts, VerifyingNode} = setup_test_nodes(),
    {ok, [Request]} = file:consult(<<"test/admissible-report.eterm">>),
    {ok, Result} = hb_http:post(
        VerifyingNode,
        <<"/~snp@1.0/verify">>,
        hb_message:commit(Request, ProxyOpts),
        ProxyOpts
    ),
    ?event({verify_test_result, Result}),
    ?assertEqual(true, hb_util:atom(Result)).


%% @doc Test successful report generation with valid configuration.
generate_success_test() ->
    % Set up test configuration
    TestWallet = ar_wallet:new(),
    TestOpts = #{
        priv_wallet => TestWallet,
        snp_trusted => [#{
            <<"vcpus">> => ?TEST_VCPUS_COUNT,
            <<"vcpu_type">> => ?TEST_VCPU_TYPE,
            <<"firmware">> => ?TEST_FIRMWARE_HASH,
            <<"kernel">> => ?TEST_KERNEL_HASH
        }]
    },
    % Load test report data from file
    TestReportJSON = load_test_report_data(),
    % Mock the NIF function to return test data
    ok = mock_snp_nif(TestReportJSON),
    try
        % Call generate function
        {ok, Result} = generate(#{}, #{}, TestOpts),
        % Verify the result structure
        ?assert(is_map(Result)),
        ?assert(maps:is_key(<<"local-hashes">>, Result)),
        ?assert(maps:is_key(<<"nonce">>, Result)),
        ?assert(maps:is_key(<<"address">>, Result)),
        ?assert(maps:is_key(<<"node-message">>, Result)),
        ?assert(maps:is_key(<<"report">>, Result)),
        % Verify the report content
        ?assertEqual(TestReportJSON, maps:get(<<"report">>, Result)),
        % Verify local hashes match the first trusted config
        ExpectedHashes = maps:get(<<"local-hashes">>, Result),
        ?assertEqual(?TEST_VCPUS_COUNT, maps:get(<<"vcpus">>, ExpectedHashes)),
        ?assertEqual(?TEST_VCPU_TYPE, maps:get(<<"vcpu_type">>, ExpectedHashes)),
        % Verify nonce is properly encoded
        Nonce = maps:get(<<"nonce">>, Result),
        ?assert(is_binary(Nonce)),
        ?assert(byte_size(Nonce) > 0),
        % Verify address is present and properly formatted
        Address = maps:get(<<"address">>, Result),
        ?assert(is_binary(Address)),
        ?assert(byte_size(Address) > 0)
    after
        % Clean up mock
        unmock_snp_nif()
    end.

%% @doc Test error handling when wallet is missing.
generate_missing_wallet_test() ->
    TestOpts = #{
        % No priv_wallet provided
        snp_trusted => [#{ <<"firmware">> => ?TEST_FIRMWARE_HASH }]
    },
    % Mock the NIF function (shouldn't be called)
    ok = mock_snp_nif(<<"dummy_report">>),
    try
        % Call generate function - should fail
        Result = generate(#{}, #{}, TestOpts),
        ?assertMatch({error, no_wallet_available}, Result)
    after
        unmock_snp_nif()
    end.

%% @doc Test error handling when trusted configurations are missing.
generate_missing_trusted_configs_test() ->
    TestWallet = ar_wallet:new(),
    TestOpts = #{
        priv_wallet => TestWallet,
        snp_trusted => [] % Empty trusted configs
    },
    
    % Mock the NIF function (shouldn't be called)
    ok = mock_snp_nif(<<"dummy_report">>),
    
    try
        % Call generate function - should fail
        Result = generate(#{}, #{}, TestOpts),
        ?assertMatch({error, no_trusted_configs}, Result)
    after
        unmock_snp_nif()
    end.

%% @doc Test successful round-trip: generate then verify with same configuration.
verify_mock_generate_success_test_() ->
    { timeout, 30, fun verify_mock_generate_success/0 }.
verify_mock_generate_success() ->
    % Set up test configuration
    TestWallet = ar_wallet:new(),
    TestTrustedConfig = #{
        <<"vcpus">> => 32,
        <<"vcpu_type">> => ?TEST_VCPU_TYPE,
        <<"vmm_type">> => ?TEST_VMM_TYPE,
        <<"guest_features">> => ?TEST_GUEST_FEATURES,
        <<"firmware">> => ?TEST_FIRMWARE_HASH,
        <<"kernel">> => ?TEST_KERNEL_HASH,
        <<"initrd">> => ?TEST_INITRD_HASH,
        <<"append">> => ?TEST_APPEND_HASH
    },
    GenerateOpts = #{
        priv_wallet => TestWallet,
        snp_trusted => [TestTrustedConfig]
    },
    % Load test report data and set up mock
    TestReportJSON = load_test_report_data(),
    ok = mock_snp_nif(TestReportJSON),
    try
        % Step 1: Generate a test report using mocked SNP
        {ok, GeneratedMsg} = generate(#{}, #{}, GenerateOpts),
        % Verify the generated message structure
        ?assert(is_map(GeneratedMsg)),
        ?assert(maps:is_key(<<"report">>, GeneratedMsg)),
        ?assert(maps:is_key(<<"address">>, GeneratedMsg)),
        ?assert(maps:is_key(<<"nonce">>, GeneratedMsg)),
        % Step 2: Set up verification options with the same trusted config
        VerifyOpts = #{
            snp_trusted => [TestTrustedConfig],
            snp_enforced_keys => [vcpu_type, vmm_type, guest_features,
                                 firmware, kernel, initrd, append]
        },
        % Step 3: Verify the generated report
        {ok, VerifyResult} = 
            verify(
                #{}, 
                hb_message:commit(GeneratedMsg, GenerateOpts), 
                VerifyOpts
            ),
        % Step 4: Assert that verification succeeds
        ?assertEqual(<<"true">>, VerifyResult),
        % Additional validation: verify specific fields
        ReportData = maps:get(<<"report">>, GeneratedMsg),
        ?assertEqual(TestReportJSON, ReportData),
        LocalHashes = maps:get(<<"local-hashes">>, GeneratedMsg),
        ?assertEqual(TestTrustedConfig, LocalHashes)
    after
        % Clean up mock
        unmock_snp_nif()
    end.

%% @doc Test verification failure when using wrong trusted configuration.
verify_mock_generate_wrong_config_test_() ->
    { timeout, 30, fun verify_mock_generate_wrong_config/0 }.
verify_mock_generate_wrong_config() ->
    % Set up test configuration for generation
    TestWallet = ar_wallet:new(),
    GenerateTrustedConfig = #{
        <<"vcpus">> => ?TEST_VCPUS_COUNT,
        <<"vcpu_type">> => ?TEST_VCPU_TYPE,
        <<"vmm_type">> => ?TEST_VMM_TYPE,
        <<"guest_features">> => ?TEST_GUEST_FEATURES,
        <<"firmware">> => ?TEST_FIRMWARE_HASH,
        <<"kernel">> => ?TEST_KERNEL_HASH,
        <<"initrd">> => ?TEST_INITRD_HASH,
        <<"append">> => ?TEST_APPEND_HASH
    },
    GenerateOpts = #{
        priv_wallet => TestWallet,
        snp_trusted => [GenerateTrustedConfig]
    },
    % Load test report data and set up mock
    TestReportJSON = load_test_report_data(),
    ok = mock_snp_nif(TestReportJSON),
    try
        % Step 1: Generate a test report
        {ok, GeneratedMsg} = generate(#{}, #{}, GenerateOpts),
        % Step 2: Set up verification with DIFFERENT trusted config
        WrongTrustedConfig = #{
            <<"vcpus">> => 32, % Different from generation config
            <<"vcpu_type">> => 3, % Different from generation config  
            <<"firmware">> => <<"different_firmware_hash">>,
            <<"kernel">> => <<"different_kernel_hash">>
        },
        VerifyOpts = #{
            snp_trusted => [WrongTrustedConfig],
            snp_enforced_keys => [vcpus, vcpu_type, firmware, kernel]
        },
        % Step 3: Verify the generated report with wrong config
        VerifyResult = 
            verify(
                #{}, 
                hb_message:commit(GeneratedMsg, GenerateOpts), 
                VerifyOpts
            ),
        ?event({verify_result, {explicit, VerifyResult}}),
        % Step 4: Assert that verification fails (either as error or false result)
        case VerifyResult of
            {ok, <<"false">>} ->
                % Verification completed but returned false (all validations ran)
                ok;
            {error, _Reason} ->
                % Verification failed early (expected for wrong config)
                ok;
            Other ->
                % Unexpected result - should fail the test
                ?assertEqual({ok, <<"false">>}, Other)
        end
    after
        % Clean up mock
        unmock_snp_nif()
    end.

%% @doc Mock the SNP NIF function to return test data.
%%
%% This function sets up a simple mock for dev_snp_nif:generate_attestation_report
%% to return predefined test data instead of calling actual hardware.
%% Uses process dictionary for simple mocking without external dependencies.
%%
%% @param TestReportJSON The test report data to return
%% @returns ok if mocking is successful
-spec mock_snp_nif(ReportJSON :: binary()) -> ok.
mock_snp_nif(TestReportJSON) ->
    % Use process dictionary for simple mocking
    put(mock_snp_nif_response, TestReportJSON),
    put(mock_snp_nif_enabled, true),
    ok.

%% @doc Clean up SNP NIF mocking.
%%
%% This function removes the mock setup and restores normal NIF behavior.
%%
%% @returns ok
-spec unmock_snp_nif() -> ok.
unmock_snp_nif() ->
    % Clean up process dictionary mock
    erase(mock_snp_nif_response),
    erase(mock_snp_nif_enabled),
    ok.