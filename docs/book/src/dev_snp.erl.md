# dev_snp

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_snp.erl)

This device provides an interface for validating and generating AMD SEV-SNP 
commitment reports.
AMD SEV-SNP (Secure Encrypted Virtualization - Secure Nested Paging) is a 
hardware-based security technology that provides confidential computing 
capabilities. This module handles the cryptographic validation of attestation 
reports and the generation of commitment reports for trusted execution environments.
The device supports two main operations:
1. Verification of remote node attestation reports with comprehensive validation
2. Generation of local attestation reports for proving node identity and software integrity

---

## Exported Functions

- `generate/3`
- `verify/3`

---

### verify

This device provides an interface for validating and generating AMD SEV-SNP 
Verify an AMD SEV-SNP commitment report message.

```erlang
-spec verify(M1 :: term(), M2 :: term(), NodeOpts :: map()) ->
    {ok, binary()} | {error, term()}.
```

```erlang
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
```

### generate

Generate an AMD SEV-SNP commitment report and emit it as a message.

```erlang
-spec generate(M1 :: term(), M2 :: term(), Opts :: map()) ->
    {ok, map()} | {error, term()}.
```

```erlang
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
```

### extract_and_normalize_message

Extract and normalize the SNP commitment message from the input.

```erlang
-spec extract_and_normalize_message(M2 :: term(), NodeOpts :: map()) ->
    {ok, {map(), binary(), binary(), binary(), map()}} | {error, term()}.
```

```erlang
extract_and_normalize_message(M2, NodeOpts) ->
    maybe
        % Search for a `body' key in the message, and if found use it as the source
        % of the report. If not found, use the message itself as the source.
```

### extract_node_message_id

Extract the node message ID from the SNP message.

```erlang
-spec extract_node_message_id(Msg :: map(), NodeOpts :: map()) ->
    {ok, binary()} | {error, missing_node_msg_id}.
```

```erlang
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
```

### verify_nonce

Verify that the nonce in the report matches the expected value.

```erlang
-spec verify_nonce(Address :: binary(), NodeMsgID :: binary(), 
    Msg :: map(), NodeOpts :: map()) -> {ok, true} | {error, nonce_mismatch}.
```

```erlang
verify_nonce(Address, NodeMsgID, Msg, NodeOpts) ->
    Nonce = hb_util:decode(hb_ao:get(<<"nonce">>, Msg, NodeOpts)),
    ?event({snp_nonce, Nonce}),
    NonceMatches = report_data_matches(Address, NodeMsgID, Nonce),
    ?event({nonce_matches, NonceMatches}),
    case NonceMatches of
        true -> {ok, true};
        false -> {error, nonce_mismatch}
    end.
```

### verify_signature_and_address

Verify that the message signature and signing address are valid.

```erlang
-spec verify_signature_and_address(MsgWithJSONReport :: map(), 
    Address :: binary(), NodeOpts :: map()) ->
    {ok, true} | {error, signature_or_address_invalid}.
```

```erlang
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
```

### verify_trusted_software

Verify that the software configuration is trusted.

```erlang
-spec verify_trusted_software(M1 :: term(), Msg :: map(), NodeOpts :: map()) ->
    {ok, true} | {error, untrusted_software}.
```

```erlang
verify_trusted_software(M1, Msg, NodeOpts) ->
    {ok, IsTrustedSoftware} = execute_is_trusted(M1, Msg, NodeOpts),
    ?event({trusted_software, IsTrustedSoftware}),
    case IsTrustedSoftware of
        true -> {ok, true};
        false -> {error, untrusted_software}
    end.
```

### verify_measurement

Verify that the measurement in the SNP report is valid.

```erlang
-spec verify_measurement(Msg :: map(), ReportJSON :: binary(), 
    NodeOpts :: map()) -> {ok, true} | {error, measurement_invalid}.
```

```erlang
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
```

### verify_report_integrity

Verify the integrity of the SNP report's digital signature.

```erlang
-spec verify_report_integrity(ReportJSON :: binary()) ->
    {ok, true} | {error, report_signature_invalid}.
```

```erlang
verify_report_integrity(ReportJSON) ->
    {ok, ReportIsValid} = dev_snp_nif:verify_signature(ReportJSON),
    ?event({report_is_valid, ReportIsValid}),
    case ReportIsValid of
        true -> {ok, true};
        false -> {error, report_signature_invalid}
    end.
```

### execute_is_trusted

Validate that all software hashes match trusted configurations.

```erlang
-spec execute_is_trusted(M1 :: term(), Msg :: map(), NodeOpts :: map()) ->
    {ok, boolean()}.
```

```erlang
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
```

### report_data_matches

Validate that the report data matches the expected nonce.

```erlang
-spec report_data_matches(Address :: binary(), NodeMsgID :: binary(), 
    ReportData :: binary()) -> boolean().
```

```erlang
report_data_matches(Address, NodeMsgID, ReportData) ->
    ?event({generated_nonce, {explicit, generate_nonce(Address, NodeMsgID)}}),
    ?event({expected_nonce, {explicit, ReportData}}),
    generate_nonce(Address, NodeMsgID) == ReportData.
```

### get_test_hashes

```erlang
-spec generate_nonce(RawAddress :: binary(), RawNodeMsgID :: binary()) -> binary().
generate_nonce(RawAddress, RawNodeMsgID) ->
    Address = hb_util:native_id(RawAddress),
    NodeMsgID = hb_util:native_id(RawNodeMsgID),
    << Address/binary, NodeMsgID/binary >>.
```

```erlang
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
```

### setup_test_nodes

```erlang
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
```

### execute_is_trusted_exact_match_should_fail_test

```erlang
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
```

```erlang
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
```

### execute_is_trusted_subset_match_should_pass_test

```erlang
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
```

### verify_test

```erlang
verify_test() ->
    % Note: If this test fails, it may be because the unsigned ID of the node
    % message in `test/admissible-report.eterm` has changed. If the format ever
    % changes, this value will need to be updated. Recalculate the unsigned ID
    % of the `Request/node-message' field, decode `Request/address', concatenate
    % the two, and encode. The result will be the new `Request/nonce' value.
```

### generate_success_test

Test successful report generation with valid configuration.

```erlang
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
```

### generate_missing_wallet_test

Test error handling when wallet is missing.

```erlang
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
```

### generate_missing_trusted_configs_test

Test error handling when trusted configurations are missing.

```erlang
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
```

### verify_mock_generate_success_test_

Test successful round-trip: generate then verify with same configuration.

```erlang
verify_mock_generate_success_test_() ->
    { timeout, 30, fun verify_mock_generate_success/0 }.
```

### verify_mock_generate_success

```erlang
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
```

### verify_mock_generate_wrong_config_test_

Test verification failure when using wrong trusted configuration.

```erlang
verify_mock_generate_wrong_config_test_() ->
    { timeout, 30, fun verify_mock_generate_wrong_config/0 }.
```

### verify_mock_generate_wrong_config

```erlang
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
```

---

*Generated from [dev_snp.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_snp.erl)*
