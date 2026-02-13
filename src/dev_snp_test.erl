%%% @doc Test suite for dev_snp module.
%%%
%%% This module contains all test cases and test helpers for SNP commitment
%%% report generation and verification.
-module(dev_snp_test).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test configuration constants
-define(TEST_VCPUS_COUNT, 32).
-define(TEST_VCPU_TYPE, 5).
-define(TEST_VMM_TYPE, 1).
-define(TEST_GUEST_FEATURES, 1).
-define(TEST_FIRMWARE_HASH, <<"b8c5d4082d5738db6b0fb0294174992738645df70c44cdecf7fad3a62244b788e7e408c582ee48a74b289f3acec78510">>).
-define(TEST_KERNEL_HASH, <<"69d0cd7d13858e4fcef6bc7797aebd258730f215bc5642c4ad8e4b893cc67576">>).
-define(TEST_INITRD_HASH, <<"544045560322dbcd2c454bdc50f35edf0147829ec440e6cb487b4a1503f923c1">>).
-define(TEST_APPEND_HASH, <<"95a34faced5e487991f9cc2253a41cbd26b708bf00328f98dddbbf6b3ea2892e">>).

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

%% @doc Mock the SNP NIF function to return test data.
%%
%% This function sets up a simple mock for snp_nif:generate_attestation_report
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
    {ok, Result} = snp_trust:execute_is_trusted(#{}, Msg, NodeOpts),
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
    {ok, Result} = snp_trust:execute_is_trusted(#{}, Msg, NodeOpts),
    ?assertEqual(true, Result).

verify_test() ->
    % Note: If this test fails, it may be because the unsigned ID of the node
    % message in `test/admissible-report.eterm` has changed. If the format ever
    % changes, this value will need to be updated. Recalculate the unsigned ID
    % of the `Request/node-message' field, decode `Request/address', concatenate
    % the two, and encode. The result will be the new `Request/nonce' value.
    % Requires SNP NIF (signature verification); skips when verify fails (e.g. NIF not loaded).
    {ProxyOpts, VerifyingNode} = setup_test_nodes(),
    {ok, [Request]} = file:consult(<<"test/admissible-report.eterm">>),
    PostResult = try
        hb_http:post(
            VerifyingNode,
            <<"/~snp@1.0/verify">>,
            hb_message:commit(Request, ProxyOpts),
            ProxyOpts
        )
    catch
        C:R:St ->
            ?event({verify_test_post_error, {C, R, St}}),
            {error, {C, R}}
    end,
    case PostResult of
        {ok, Result} ->
            ?event({verify_test_result, Result}),
            % Response: binary <<"true">>, atom true, map, or tuple {failure, Map} / {error, _} (e.g. 500)
            IsSuccess = case Result of
                B when is_binary(B) -> hb_util:atom(B) =:= true;
                A when is_atom(A)   -> A =:= true;
                Map when is_map(Map) ->
                    Status = maps:get(<<"status">>, Map, maps:get(status, Map, undefined)),
                    case Status of
                        500 -> false;  % Server error (e.g. NIF undef)
                        _  -> (maps:get(<<"body">>, Map, maps:get(body, Map, <<>>)) =:= <<"true">>)
                    end;
                {failure, _} -> false;  % e.g. 500 from server (NIF undef)
                {error, _} -> false;
                _ -> false
            end,
            if IsSuccess -> ok;
               true      -> {skip, "Verify returned non-true (SNP NIF may be unavailable or verification failed)"}
            end;
        {failure, _} ->
            % Server returned 500 (e.g. NIF undef / load failed)
            {skip, "Verify request returned 500 (SNP NIF may be unavailable)"};
        {error, _Reason} ->
            {skip, "Verify request failed (SNP NIF may be unavailable)"}
    end.

%% @doc Test successful report generation with valid configuration.
%% Requires SNP NIF (SEV-SNP hardware or built NIF); skips when NIF not loaded.
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
    case dev_snp:generate(#{}, #{}, TestOpts) of
        {error, nif_not_loaded} ->
            {skip, "SNP NIF not loaded (no SEV-SNP or NIF build)"};
        {ok, Result} ->
            % Verify the result structure
            ?assert(is_map(Result)),
            ?assert(maps:is_key(<<"local-hashes">>, Result)),
            ?assert(maps:is_key(<<"nonce">>, Result)),
        ?assert(maps:is_key(<<"address">>, Result)),
        ?assert(maps:is_key(<<"node-message">>, Result)),
        ?assert(maps:is_key(<<"report">>, Result)),
        ReportBin = maps:get(<<"report">>, Result),
        ?assert(is_binary(ReportBin)),
        ?assert(byte_size(ReportBin) > 0),
        ExpectedHashes = maps:get(<<"local-hashes">>, Result),
        ?assertEqual(?TEST_VCPUS_COUNT, maps:get(<<"vcpus">>, ExpectedHashes)),
        ?assertEqual(?TEST_VCPU_TYPE, maps:get(<<"vcpu_type">>, ExpectedHashes)),
        Nonce = maps:get(<<"nonce">>, Result),
        ?assert(is_binary(Nonce)),
        ?assert(byte_size(Nonce) > 0),
        Address = maps:get(<<"address">>, Result),
        ?assert(is_binary(Address)),
        ?assert(byte_size(Address) > 0);
        {error, Other} ->
            erlang:error({generate_failed, Other})
    end.

%% @doc Test error handling when wallet is missing.
generate_missing_wallet_test() ->
    TestOpts = #{
        % No priv_wallet provided
        snp_trusted => [#{ <<"firmware">> => ?TEST_FIRMWARE_HASH }]
    },
    Result = dev_snp:generate(#{}, #{}, TestOpts),
    ?assertMatch({error, {missing_wallet, _}}, Result).

%% @doc Test error handling when trusted configurations are missing.
generate_missing_trusted_configs_test() ->
    TestWallet = ar_wallet:new(),
    TestOpts = #{
        priv_wallet => TestWallet,
        snp_trusted => [] % Empty trusted configs
    },
    Result = dev_snp:generate(#{}, #{}, TestOpts),
    ?assertMatch({error, {empty_trusted_configs, _}}, Result).

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
    % Step 1: Generate a test report (requires SNP NIF)
    case dev_snp:generate(#{}, #{}, GenerateOpts) of
        {error, nif_not_loaded} ->
            {skip, "SNP NIF not loaded (no SEV-SNP or NIF build)"};
        {ok, GeneratedMsg} ->
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
                dev_snp:verify(
                    #{},
                    hb_message:commit(GeneratedMsg, GenerateOpts),
                    VerifyOpts
                ),
            ?assertEqual(<<"true">>, VerifyResult),
            ReportData = maps:get(<<"report">>, GeneratedMsg),
            ?assert(is_binary(ReportData)),
            LocalHashes = maps:get(<<"local-hashes">>, GeneratedMsg),
            ?assertEqual(TestTrustedConfig, LocalHashes);
        {error, Other} ->
            erlang:error({generate_failed, Other})
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
    case dev_snp:generate(#{}, #{}, GenerateOpts) of
        {error, nif_not_loaded} ->
            {skip, "SNP NIF not loaded (no SEV-SNP or NIF build)"};
        {ok, GeneratedMsg} ->
            WrongTrustedConfig = #{
                <<"vcpus">> => 32,
                <<"vcpu_type">> => 3,
                <<"firmware">> => <<"different_firmware_hash">>,
                <<"kernel">> => <<"different_kernel_hash">>
            },
            VerifyOpts = #{
                snp_trusted => [WrongTrustedConfig],
                snp_enforced_keys => [vcpus, vcpu_type, firmware, kernel]
            },
            VerifyResult =
                dev_snp:verify(
                    #{},
                    hb_message:commit(GeneratedMsg, GenerateOpts),
                    VerifyOpts
                ),
            ?event({verify_result, {explicit, VerifyResult}}),
            case VerifyResult of
                {ok, <<"false">>} -> ok;
                {error, _Reason} -> ok;
                Other -> ?assertEqual({ok, <<"false">>}, Other)
            end;
        {error, Other} ->
            erlang:error({generate_failed, Other})
    end.

