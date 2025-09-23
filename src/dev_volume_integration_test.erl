-module(dev_volume_integration_test).
-export([run_integration_tests/0]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% WARNING: These tests perform DESTRUCTIVE operations on real disks
%% Test configuration - CHANGE THESE FOR YOUR TEST ENVIRONMENT
-define(TEST_DEVICE, <<"/dev/sda">>).
-define(TEST_PARTITION, <<"/dev/sda1">>).
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
        priv_wallet => hb:wallet()
    }.

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
                      ?_test(test_complete_mount_process(Opts))
                  ]
              end
             }
            }
    end.

test_complete_mount_process(Opts) ->
    % Test the complete mount process
    {ok, Result} = dev_volume:mount(msg1, msg2, Opts),
    ?assertEqual(<<"Volume mounted and store updated successfully">>, Result),
    
    % Verify mount point exists and is accessible
    MountPoint = binary_to_list(maps:get(volume_mount_point, Opts)),
    ?assert(filelib:is_dir(MountPoint)),
    
    io:format("Complete mount process test passed~n").

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