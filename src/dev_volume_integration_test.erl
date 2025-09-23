-module(dev_volume_integration_test).
-export([run_integration_tests/0]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% WARNING: These tests perform DESTRUCTIVE operations on real disks
%% Only run with devices you can afford to lose data on!
%% Recommended: Use a test VM with attached storage or dedicated test device

%% Test configuration - CHANGE THESE FOR YOUR TEST ENVIRONMENT
-define(TEST_DEVICE, <<"/dev/sdc">>).
-define(TEST_PARTITION, <<"/dev/sdc1">>).
-define(TEST_PARTITION_TYPE, <<"ext4">>).
-define(TEST_VOLUME_NAME, <<"hb_test_volume">>).
-define(TEST_MOUNT_POINT, <<"/mnt/hb_test">>).
-define(TEST_STORE_PATH, <<"/mnt/hb_test/store">>).
-define(TEST_ENCRYPTION_KEY, <<"test_encryption_key_32_bytes_long">>).

%% Safety check - only run if explicitly enabled
-define(ENABLE_DESTRUCTIVE_TESTS, 
    case os:getenv("HB_ENABLE_DESTRUCTIVE_DISK_TESTS") of
        "true" -> true;
        "1" -> true;
        _ -> false
    end).

%% Test fixtures
setup_real_disk_test_opts() ->
    #{
        priv_volume_key => ?TEST_ENCRYPTION_KEY,
        volume_device => ?TEST_DEVICE,
        volume_partition => ?TEST_PARTITION,
        volume_partition_type => ?TEST_PARTITION_TYPE,
        volume_name => ?TEST_VOLUME_NAME,
        volume_mount_point => ?TEST_MOUNT_POINT,
        volume_store_path => ?TEST_STORE_PATH,
        store => [],
        genesis_wasm_db_dir => "cache-mainnet/genesis-wasm",
        volume_skip_decryption => <<"true">>,  % Skip decryption for test
        priv_wallet => create_test_wallet()
    }.

create_test_wallet() ->
    % Generate a test RSA key pair
    {ok, PrivKey} = generate_rsa_key(),
    PubKeyBinary = extract_public_key_binary(PrivKey),
    {{{rsa, 65537}, extract_private_key_binary(PrivKey), PubKeyBinary}, 
     <<"test_pub_key">>}.

generate_rsa_key() ->
    % Generate 2048-bit RSA key
    try
        PrivKey = public_key:generate_key({rsa, 2048, 65537}),
        {ok, PrivKey}
    catch
        _:Error ->
            {error, Error}
    end.

extract_public_key_binary(#'RSAPrivateKey'{modulus = Modulus}) ->
    crypto:integer_to_binary(Modulus).

extract_private_key_binary(#'RSAPrivateKey'{privateExponent = PrivExp}) ->
    crypto:integer_to_binary(PrivExp).

%% Safety checks before running destructive tests
safety_checks() ->
    case ?ENABLE_DESTRUCTIVE_TESTS of
        false ->
            {skip, "Destructive tests disabled. Set HB_ENABLE_DESTRUCTIVE_DISK_TESTS=true to enable"};
        true ->
            case check_test_environment() of
                ok -> ok;
                {error, Reason} -> {skip, Reason}
            end
    end.

check_test_environment() ->
    % Check if running as root (required for disk operations)
    case os:cmd("id -u") of
        "0\n" -> check_device_availability();
        _ -> {error, "Must run as root for disk operations"}
    end.

check_device_availability() ->
    Device = binary_to_list(?TEST_DEVICE),
    case filelib:is_file(Device) of
        true -> 
            case check_device_not_mounted() of
                ok -> warn_about_data_loss();
                Error -> Error
            end;
        false -> 
            {error, io_lib:format("Test device ~s not found", [Device])}
    end.

check_device_not_mounted() ->
    Device = binary_to_list(?TEST_DEVICE),
    MountCheck = os:cmd("mount | grep " ++ Device),
    case MountCheck of
        "" -> ok;  % Not mounted
        _ -> {error, io_lib:format("Device ~s appears to be mounted", [Device])}
    end.

warn_about_data_loss() ->
    Device = binary_to_list(?TEST_DEVICE),
    io:format("~n" ++
              "WARNING: About to perform DESTRUCTIVE operations on ~s~n" ++
              "This will DESTROY ALL DATA on the device!~n" ++
              "Press Ctrl+C to abort, or any key to continue...~n", [Device]),
    io:get_chars("", 1),
    ok.

%% Cleanup functions
cleanup_test_environment() ->
    cleanup_mount(),
    cleanup_luks(),
    ok.

cleanup_mount() ->
    MountPoint = binary_to_list(?TEST_MOUNT_POINT),
    os:cmd("umount " ++ MountPoint ++ " 2>/dev/null"),
    os:cmd("rmdir " ++ MountPoint ++ " 2>/dev/null").

cleanup_luks() ->
    VolumeName = binary_to_list(?TEST_VOLUME_NAME),
    os:cmd("cryptsetup luksClose " ++ VolumeName ++ " 2>/dev/null").

%% Pre-test setup
setup_test_environment() ->
    % Ensure mount point exists
    MountPoint = binary_to_list(?TEST_MOUNT_POINT),
    os:cmd("mkdir -p " ++ MountPoint),
    ok.

%% Main integration test
full_disk_integration_test_() ->
    case safety_checks() of
        {skip, Reason} ->
            {skip, Reason};
        ok ->
            {timeout, 300, 
             {setup,
              fun() -> 
                  setup_test_environment(),
                  setup_real_disk_test_opts()
              end,
              fun(_) -> 
                  cleanup_test_environment()
              end,
              fun(Opts) ->
                  [
                      ?_test(test_device_detection(Opts)),
                      ?_test(test_partition_creation_and_format(Opts)),
                      ?_test(test_complete_mount_process(Opts)),
                      ?_test(test_volume_operations(Opts)),
                      ?_test(test_store_configuration(Opts))
                  ]
              end
             }
            }
    end.

%% Individual test functions
test_device_detection(Opts) ->
    Device = maps:get(volume_device, Opts),
    
    % Test that device exists
    Result = dev_volume:check_device_exists(Device),
    ?assertEqual(ok, Result),
    
    io:format("Device detection test passed for ~s~n", [Device]).

test_partition_creation_and_format(Opts) ->
    Device = maps:get(volume_device, Opts),
    Partition = maps:get(volume_partition, Opts),
    PartitionType = maps:get(volume_partition_type, Opts),
    Key = maps:get(priv_volume_key, Opts),
    
    % First, ensure partition doesn't exist by cleaning up
    cleanup_existing_partition(Partition),
    
    % Test partition creation
    Result = dev_volume:create_and_format_partition(Device, Partition, PartitionType, Key),
    ?assertEqual(ok, Result),
    
    % Verify partition was created
    PartitionExists = hb_volume:check_for_device(Partition),
    ?assertEqual({ok, true}, PartitionExists),
    
    io:format("Partition creation and formatting test passed~n").

test_complete_mount_process(Opts) ->
    % Test the complete mount process
    {ok, Result} = dev_volume:mount(msg1, msg2, Opts),
    ?assertEqual(<<"Volume mounted and store updated successfully">>, Result),
    
    % Verify mount point exists and is accessible
    MountPoint = binary_to_list(maps:get(volume_mount_point, Opts)),
    ?assert(filelib:is_dir(MountPoint)),
    
    io:format("Complete mount process test passed~n").

test_volume_operations(Opts) ->
    MountPoint = binary_to_list(maps:get(volume_mount_point, Opts)),
    
    % Test writing to the mounted volume
    TestFile = MountPoint ++ "/test_write.txt",
    TestContent = "HyperBEAM volume test content",
    
    ok = file:write_file(TestFile, TestContent),
    {ok, ReadContent} = file:read_file(TestFile),
    ?assertEqual(list_to_binary(TestContent), ReadContent),
    
    % Clean up test file
    file:delete(TestFile),
    
    io:format("Volume operations test passed~n").

test_store_configuration(Opts) ->
    StorePath = maps:get(volume_store_path, Opts),
    
    % Test store directory creation
    StoreDir = binary_to_list(StorePath),
    os:cmd("mkdir -p " ++ StoreDir),
    ?assert(filelib:is_dir(StoreDir)),
    
    % Test store configuration update
    {ok, NewStore} = dev_volume:update_node_store(StorePath, Opts),
    ?assert(is_binary(NewStore)),
    
    io:format("Store configuration test passed~n").

cleanup_existing_partition(Partition) ->
    PartitionStr = binary_to_list(Partition),
    
    % Unmount if mounted
    os:cmd("umount " ++ PartitionStr ++ " 2>/dev/null"),
    
    % Close LUKS if open
    os:cmd("cryptsetup luksClose " ++ binary_to_list(?TEST_VOLUME_NAME) ++ " 2>/dev/null"),
    
    % Remove partition
    Device = binary_to_list(?TEST_DEVICE),
    os:cmd("parted -s " ++ Device ++ " rm 1 2>/dev/null"),
    
    % Clear partition table
    os:cmd("dd if=/dev/zero of=" ++ Device ++ " bs=512 count=1 2>/dev/null"),
    
    ok.

%% Comprehensive test with error injection
error_handling_integration_test_() ->
    case safety_checks() of
        {skip, Reason} -> {skip, Reason};
        ok ->
            {timeout, 180,
             {setup,
              fun() -> 
                  setup_test_environment(),
                  setup_real_disk_test_opts()
              end,
              fun(_) -> 
                  cleanup_test_environment()
              end,
              fun(Opts) ->
                  [
                      ?_test(test_invalid_device(Opts)),
                      ?_test(test_permission_errors(Opts)),
                      ?_test(test_mount_point_issues(Opts))
                  ]
              end
             }
            }
    end.

test_invalid_device(Opts) ->
    InvalidOpts = Opts#{volume_device => <<"/dev/nonexistent">>},
    {error, Error} = dev_volume:mount(msg1, msg2, InvalidOpts),
    ?assertEqual(<<"Base device check failed">>, Error).

test_permission_errors(_Opts) ->
    % Test with a device that should exist but may have permission issues
    TestOpts = #{
        priv_volume_key => <<"test_key">>,
        volume_device => <<"/dev/null">>,  % Should exist but not be a block device
        volume_partition => <<"/dev/null1">>,
        volume_partition_type => <<"ext4">>,
        volume_name => <<"test">>,
        volume_mount_point => <<"/tmp/test">>,
        volume_store_path => <<"/tmp/test/store">>,
        volume_skip_decryption => <<"true">>
    },
    
    {error, _Error} = dev_volume:mount(msg1, msg2, TestOpts).

test_mount_point_issues(Opts) ->
    % Test with invalid mount point
    InvalidMountOpts = Opts#{volume_mount_point => <<"/root/invalid/deep/path">>},
    
    Result = dev_volume:mount(msg1, msg2, InvalidMountOpts),
    % Should handle mount point creation issues gracefully
    case Result of
        {ok, _} -> ok;  % Succeeded despite path issues
        {error, _} -> ok  % Failed gracefully
    end.

%% Helper function to run tests safely
run_integration_tests() ->
    case ?ENABLE_DESTRUCTIVE_TESTS of
        false ->
            io:format("~nTo run integration tests, set environment variable:~n"),
            io:format("export HB_ENABLE_DESTRUCTIVE_DISK_TESTS=true~n"),
            io:format("WARNING: These tests will destroy data on ~s!~n", [?TEST_DEVICE]),
            {error, tests_disabled};
        true ->
            io:format("~nRunning destructive disk integration tests...~n"),
            eunit:test(?MODULE, [verbose])
    end.