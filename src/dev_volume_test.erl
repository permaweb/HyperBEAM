%%% @doc Test suite for dev_volume module
%%%
%%% This test suite covers the secure volume management functionality
%%% provided by the dev_volume module. Tests focus on basic functionality
%%% without external dependencies.
-module(dev_volume_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Test Setup

setup() ->
    TestOpts = #{
        priv_volume_key => base64:encode(crypto:strong_rand_bytes(32)),
        volume_device => <<"/dev/test-device">>,
        volume_partition => <<"/dev/test-device1">>,
        volume_partition_type => <<"ext4">>,
        volume_name => <<"test-volume">>,
        volume_mount_point => <<"/mnt/test">>,
        volume_store_path => <<"/mnt/test/store">>,
        volume_skip_decryption => <<"false">>,
        priv_wallet => {
            {{rsa, 65537}, 
             crypto:strong_rand_bytes(256), 
             crypto:strong_rand_bytes(256)}, 
            <<"test-pubkey">>
        },
        store => [#{<<"store-module">> => test_store, <<"name">> => <<"test">>}]
    },
    TestOpts.

teardown(_) ->
    ok.

%%% Test Fixtures

basic_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         {"info/1 should return exports map", fun test_info_1/0},
         {"info/3 should return device information", fun test_info_3/0}
     ]}.

mount_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         {"mount/3 should fail when required options are missing", fun test_mount_missing_options/0},
         {"mount/3 should fail when volume key is not found", fun test_mount_no_key/0},
         {"mount/3 should handle invalid encrypted key", fun test_mount_invalid_encrypted_key/0},
         {"mount/3 should fail when trying to mount non-existing device", fun test_failed_mount_non_existing_device/0},
         {"mount/3 should successfully mount existing partition", fun test_successful_mount_existing_partition/0},
         {"mount/3 should create and format new partition successfully", fun test_successful_mount_create_and_format_partition/0}
     ]}.

public_key_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         {"public_key/3 should fail when wallet not available", fun test_public_key_no_wallet/0},
         {"public_key/3 should return encoded public key when wallet available", fun test_public_key_success/0}
     ]}.

%%% Test Functions

test_info_1() ->
    Result = dev_volume:info(test_arg),
    ?assertMatch(#{exports := [info, mount, public_key]}, Result).

test_info_3() ->
    {ok, Result} = dev_volume:info(msg1, msg2, #{}),
    ?assertMatch(#{<<"status">> := 200, <<"body">> := _}, Result),
    Body = maps:get(<<"body">>, Result),
    ?assertMatch(#{<<"description">> := <<"Secure Volume Management for HyperBEAM Nodes">>}, Body),
    ?assertMatch(#{<<"version">> := <<"1.0">>}, Body),
    ?assertMatch(#{<<"api">> := _}, Body).

test_mount_missing_options() ->
    EmptyOpts = #{},
    Result = dev_volume:mount(msg1, msg2, EmptyOpts),
    ?assertMatch({error, _}, Result).

test_mount_no_key() ->
    OptsNoKey = #{
        volume_device => <<"/dev/test-device">>,
        volume_partition => <<"/dev/test-device1">>,
        volume_partition_type => <<"ext4">>,
        volume_name => <<"test-volume">>,
        volume_mount_point => <<"/mnt/test">>,
        volume_store_path => <<"/mnt/test/store">>
    },
    Result = dev_volume:mount(msg1, msg2, OptsNoKey),
    ?assertMatch({error, _}, Result).

test_mount_invalid_encrypted_key() ->
    OptsWithInvalidKey = #{
        priv_volume_key => <<"invalid_base64_key">>,
        volume_device => <<"/dev/test-device">>,
        volume_partition => <<"/dev/test-device1">>,
        volume_partition_type => <<"ext4">>,
        volume_name => <<"test-volume">>,
        volume_mount_point => <<"/mnt/test">>,
        volume_store_path => <<"/mnt/test/store">>,
        volume_skip_decryption => <<"false">>,
        priv_wallet => {
            {{rsa, 65537}, 
             crypto:strong_rand_bytes(256), 
             crypto:strong_rand_bytes(256)}, 
            <<"test-pubkey">>
        }
    },
    Result = dev_volume:mount(msg1, msg2, OptsWithInvalidKey),
    ?assertMatch({error, _}, Result).

test_failed_mount_non_existing_device() ->
    meck:new(hb_volume, [passthrough]),
    
    TestOpts = setup(),
    HappyPathOpts = TestOpts#{volume_skip_decryption => <<"true">>},
    
    % Mock scenario where device exists but partition doesn't, so it needs to be created
    meck:expect(hb_volume, check_for_device, fun
        (<<"/dev/test-device">>) -> false     % Base device non exists
    end),
    
    % Execute the mount operation
    Result = dev_volume:mount(msg1, msg2, HappyPathOpts),
    
    % Should succeed with successful mount message
    ?assertMatch({error,<<"Base device not found">>}, Result),
    
    % Verify that partition creation flow was called
    ?assert(meck:called(hb_volume, check_for_device, [<<"/dev/test-device">>])),
    
    % Clean up mocks
    meck:unload([hb_volume]).

test_successful_mount_existing_partition() ->
    meck:new(hb_opts, [passthrough]),
    meck:new(hb_volume, [passthrough]),
    meck:new(hb_http_server, [passthrough]),
    
    TestOpts = setup(),
    HappyPathOpts = TestOpts#{volume_skip_decryption => <<"true">>},
    
    % Mock successful device operations
    meck:expect(hb_volume, check_for_device, fun(_) -> true end),
    meck:expect(hb_volume, mount_disk, fun(_, _, _, _) -> {ok, <<"Successfully mounted">>} end),
    meck:expect(hb_volume, change_node_store, fun(_, Store) -> 
        {ok, #{<<"store">> => Store}}
    end),
    meck:expect(hb_http_server, set_opts, fun(_) -> ok end),
    
    % Execute the mount operation
    Result = dev_volume:mount(msg1, msg2, HappyPathOpts),
    
    % Should succeed with successful mount message
    ?assertMatch({ok, <<"Volume mounted and store updated successfully">>}, Result),
    
    % Verify that the expected functions were called
    ?assert(meck:called(hb_volume, check_for_device, '_')),
    ?assert(meck:called(hb_volume, mount_disk, '_')),
    ?assert(meck:called(hb_volume, change_node_store, '_')),
    ?assert(meck:called(hb_http_server, set_opts, '_')),
    
    % Clean up mocks
    meck:unload([hb_http_server, hb_volume, hb_opts]).

test_successful_mount_create_and_format_partition() ->
    meck:new(hb_opts, [passthrough]),
    meck:new(hb_volume, [passthrough]),
    meck:new(hb_http_server, [passthrough]),
    
    TestOpts = setup(),
    HappyPathOpts = TestOpts#{volume_skip_decryption => <<"true">>},
    
    % Mock scenario where device exists but partition doesn't, so it needs to be created
    meck:expect(hb_volume, check_for_device, fun
        (<<"/dev/test-device">>) -> true;      % Base device exists
        (<<"/dev/test-device1">>) -> false     % Partition doesn't exist yet
    end),
    meck:expect(hb_volume, create_partition, fun(_, _) -> {ok, <<"Partition created">>} end),
    meck:expect(hb_volume, format_disk, fun(_, _) -> {ok, <<"Disk formatted">>} end),
    meck:expect(hb_volume, mount_disk, fun(_, _, _, _) -> {ok, <<"Successfully mounted">>} end),
    meck:expect(hb_volume, change_node_store, fun(_, Store) -> 
        {ok, #{<<"store">> => Store}}
    end),
    meck:expect(hb_http_server, set_opts, fun(_) -> ok end),
    
    % Execute the mount operation
    Result = dev_volume:mount(msg1, msg2, HappyPathOpts),
    
    % Should succeed with successful mount message
    ?assertMatch({ok, <<"Volume mounted and store updated successfully">>}, Result),
    
    % Verify that partition creation flow was called
    ?assert(meck:called(hb_volume, check_for_device, [<<"/dev/test-device">>])),
    ?assert(meck:called(hb_volume, check_for_device, [<<"/dev/test-device1">>])),
    ?assert(meck:called(hb_volume, create_partition, '_')),
    ?assert(meck:called(hb_volume, format_disk, '_')),
    ?assert(meck:called(hb_volume, mount_disk, '_')),
    ?assert(meck:called(hb_volume, change_node_store, '_')),
    ?assert(meck:called(hb_http_server, set_opts, '_')),
    
    % Clean up mocks
    meck:unload([hb_http_server, hb_volume, hb_opts]).

test_public_key_no_wallet() ->
    EmptyOpts = #{},
    Result = dev_volume:public_key(msg1, msg2, EmptyOpts),
    ?assertMatch({error, <<"Node wallet not available">>}, Result).

test_public_key_success() ->
    TestOpts = setup(),
    Result = dev_volume:public_key(msg1, msg2, TestOpts),
    ?assertMatch({ok, #{<<"status">> := 200, <<"public_key">> := _, <<"message">> := _}}, Result),
    {ok, ResultMap} = Result,
    ?assert(is_binary(maps:get(<<"public_key">>, ResultMap))).