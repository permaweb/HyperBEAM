%%% @doc Secure Volume Management for HyperBEAM Nodes
%%%
%%% This module handles encrypted storage operations for HyperBEAM, 
%%% providing a robust and secure approach to data persistence. It manages 
%%% the complete lifecycle of encrypted volumes from detection to creation, 
%%% formatting, and mounting.
%%%
%%% Key responsibilities:
%%% - Volume detection and initialization
%%% - Encrypted partition creation and formatting
%%% - Secure mounting using cryptographic keys
%%% - Store path reconfiguration to use mounted volumes
%%% - Automatic handling of various system states 
%%%   (new device, existing partition, etc.)
%%%
%%% The primary entry point is the `mount/3' function, which orchestrates 
%%% the entire process based on the provided configuration parameters. This 
%%% module works alongside `hb_volume' which provides the low-level 
%%% operations for device manipulation.
%%%
%%% Security considerations:
%%% - Ensures data at rest is protected through LUKS encryption
%%% - Provides proper volume sanitization and secure mounting
%%% - IMPORTANT: This module only applies configuration set in node options 
%%%   and does NOT accept disk operations via HTTP requests. It cannot 
%%%   format arbitrary disks as all operations are safeguarded by host 
%%%   operating system permissions enforced upon the HyperBEAM environment.
-module(dev_volume).
-export([info/1, info/3, mount/3, public_key/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% @doc Exported function for getting device info, controls which functions 
%% are exposed via the device API.
info(_) -> 
    ?event(debug_volume, {info, entry, device_info_requested}),
    #{ exports => [info, mount, public_key] }.

%% @doc HTTP info response providing information about this device
info(_Msg1, _Msg2, _Opts) ->
    ?event(debug_volume, {info, http_request, starting}),
    InfoBody = #{
        <<"description">> => 
            <<"Secure Volume Management for HyperBEAM Nodes">>,
        <<"version">> => <<"1.0">>,
        <<"api">> => #{
            <<"info">> => #{
                <<"description">> => <<"Get device info">>
            },
            <<"mount">> => #{
                <<"description">> => <<"Mount an encrypted volume">>,
                <<"required_node_opts">> => #{
                    <<"priv_volume_key">> => <<"The encryption key">>,
                    <<"volume_device">> => <<"The base device path">>,
                    <<"volume_partition">> => <<"The partition path">>,
                    <<"volume_partition_type">> => <<"The partition type">>,
                    <<"volume_name">> => 
                        <<"The name for the encrypted volume">>,
                    <<"volume_mount_point">> => 
                        <<"Where to mount the volume">>,
                    <<"volume_store_path">> => 
                        <<"The store path on the volume">>
                }
            },
            <<"public_key">> => #{
                <<"description">> => 
                    <<"Get the node's public key for encrypted key exchange">>
            }
        }
    },
    ?event(debug_volume, {info, http_response, success}),
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc Handles the complete process of secure encrypted volume mounting.
%%
%% This function performs the following operations depending on the state:
%% 1. Validates the encryption key is present
%% 2. Checks if the base device exists
%% 3. Checks if the partition exists on the device
%% 4. If the partition exists, attempts to mount it
%% 5. If the partition doesn't exist, creates it, formats it with 
%%    encryption and mounts it
%% 6. Updates the node's store configuration to use the mounted volume
%%
%% Config options in Opts map:
%% - priv_volume_key: (Required) The encryption key
%% - volume_device: Base device path
%% - volume_partition: Partition path
%% - volume_partition_type: Filesystem type
%% - volume_name: Name for encrypted volume
%% - volume_mount_point: Where to mount
%% - volume_store_path: Store path on volume
%%
%% @param M1 Base message for context.
%% @param M2 Request message with operation details.
%% @param Opts A map of configuration options for volume operations.
%% @returns `{ok, Binary}' on success with operation result message, or
%% `{error, Binary}' on failure with error message.
-spec mount(term(), term(), map()) -> 
    {ok, binary()} | {error, binary()}.
mount(_M1, _M2, Opts) ->
    ?event(debug_volume, {mount, entry, starting}),
    % Check if an encrypted key was sent in the request
    EncryptedKey = hb_opts:get(priv_volume_key, not_found, Opts),
    % Determine if we need to decrypt a key or use one from config
    SkipDecryption = hb_opts:get(volume_skip_decryption, 
        <<"false">>, Opts),
    Key = case SkipDecryption of
        <<"true">> ->
            ?event(debug_mount, {mount, skip_decryption, true}),
            EncryptedKey;
        _ ->
            ?event(debug_volume, {decrypt_volume_key}),
            case decrypt_volume_key(EncryptedKey, Opts) of
                {ok, DecryptedKey} -> DecryptedKey;
                {error, DecryptError} ->
                    ?event(debug_mount, 
                        {mount, key_decrypt_error, DecryptError}
                    ),
                    not_found
            end
    end,
    Device = hb_opts:get(volume_device, not_found, Opts),
    Partition = hb_opts:get(volume_partition, not_found, Opts),
    PartitionType = hb_opts:get(volume_partition_type, not_found, Opts),
    VolumeName = hb_opts:get(volume_name, not_found, Opts),
    MountPoint = hb_opts:get(volume_mount_point, not_found, Opts),
    StorePath = hb_opts:get(volume_store_path, not_found, Opts),
    ?event(debug_volume, 
        {mount, options_extracted, 
            {
                device, Device, partition, Partition, 
                partition_type, PartitionType, volume_name, VolumeName, 
                mount_point, MountPoint, store_path, StorePath
            }
        }
    ),
    % Check for missing required node options
    case hb_opts:check_required_opts([
        {<<"priv_volume_key">>, Key},
        {<<"volume_device">>, Device},
        {<<"volume_partition">>, Partition},
        {<<"volume_partition_type">>, PartitionType},
        {<<"volume_name">>, VolumeName}, 
        {<<"volume_mount_point">>, MountPoint},
        {<<"volume_store_path">>, StorePath}
    ], Opts) of
        {ok, _} ->
            handle_mounting_partition(
                Device, Partition, PartitionType, VolumeName, 
                MountPoint, StorePath, Key, Opts
            );
        {error, ErrorMsg} ->
            ?event(debug_volume, {mount, required_opts_error, ErrorMsg}),
            {error, ErrorMsg}
    end.

%% @doc Check if the base device exists and if it does, check if the 
%% partition exists.
%% @param Device The base device to check.
%% @param Partition The partition to check.
%% @param PartitionType The type of partition to check.
%% @param VolumeName The name of the volume to check.
%% @param MountPoint The mount point to check.
%% @param StorePath The store path to check.
%% @param Key The key to check.
%% @param Opts The options to check.
%% @returns `{ok, Binary}' on success with operation result message, or
%% `{error, Binary}' on failure with error message.
-spec handle_mounting_partition(
    term(), term(), term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
handle_mounting_partition(
    Device, Partition, PartitionType, VolumeName, MountPoint, StorePath, 
    Key, Opts
) ->
    ?event(debug_volume, 
        {handle_mounting_partition, entry}
    ),

    maybe
        {_, ok} ?= {check_device_exists, check_device_exists(Device)},
        {_, ok} ?= {maybe_create_and_format_partition, maybe_create_and_format_partition(Device, Partition, PartitionType, Key)},
        {_, ok} ?= {mount_volume, mount_volume(Partition, Key, MountPoint, VolumeName)},
        {_, {ok, NewStore}} ?= {update_node_store, update_node_store(StorePath, Opts)},
        {_, {ok, Result}} ?= {update_application_config, update_application_config(StorePath, NewStore, Opts)},
        {ok, Result}
    else
        {check_device_exists, {error, Error}} ->
            ?event(debug_volume, 
                {handle_mounting_partition, device_check_error, Error}
            ),
            {error, <<"Base device check failed">>};
        {maybe_create_and_format_partition, {error, Error}} ->
            ?event(debug_volume, 
                {handle_mounting_partition, maybe_create_and_format_partition_error, Error}
            ),
            {error, <<"Checking/creating/formatting partition failed">>};
        {mount_volume, {error, Error}} ->
            ?event(debug_volume, 
                {handle_mounting_partition, mount_volume_error, Error}
            ),
            {error, <<"Mounting volume failed">>};
        {update_node_store, {error, Error}} ->
            ?event(debug_volume, 
                {handle_mounting_partition, update_node_store_error, Error}
            ),
            {error, <<"Updating node store failed">>};
        {update_application_config, {error, Error}} ->
            ?event(debug_volume, 
                {handle_mounting_partition, update_application_config_error, Error}
            ),
            {error, <<"Updating application config failed">>}
    end.

check_device_exists(Device) ->
    ?event(debug_volume, 
        {check_device_exists, checking_does_device_exist, Device}
    ),    
    case hb_volume:check_for_device(Device) of
        {ok, false} ->
            ?event(debug_volume, 
                {check_device_exists, device_not_found, Device}
            ),
            {error, <<"Base device not found">>};
        {ok, true} ->
            ?event(debug_volume, 
                {check_device_exists, device_found}
            ),
            ok;
        {error, Error} ->
            ?event(debug_volume, 
                {check_device_exists, failed_to_check_device_existance, Error}
            ),
            {error, <<"Failed to check device existence">>}
    end.

maybe_create_and_format_partition(Device, Partition, PartitionType, Key) ->
    ?event(debug_volume, 
        {maybe_create_and_format_partition, checking_does_partition_exist, Partition}
    ),
    case hb_volume:check_for_device(Partition) of
        {ok, true} ->
            ?event(debug_volume,
                {maybe_create_and_format_partition, partition_exists, Partition}
            ),
            ok;  % Partition ready, continue
        {ok, false} ->
            ?event(debug_volume,
                {maybe_create_and_format_partition, creating_partition, Partition}
            ),
            create_and_format_partition(Device, Partition, PartitionType, Key);
        {error, Error} ->
            ?event(debug_volume, 
                {check_device_exists, failed_to_check_device_existance, Error}
            ),
            {error, <<"Failed to check partition existence">>}
    end.

create_and_format_partition(Device, Partition, PartitionType, Key) ->
    ?event(debug_volume, 
        {create_and_format_partition, creating_new_partition, Partition}
    ),
    maybe
        {create_partition_table, {ok, _}} ?= {create_partition_table, hb_volume:create_partition_table(Device)},
        {create_partition, {ok, _}} ?= {create_partition, hb_volume:create_partition(Device, PartitionType)},
        {format_partition, {ok, _}} ?= {format_partition, hb_volume:format_partition(Partition, Key)},
        ok
    else
        {create_partition_table, {error, Error}} -> 
            ?event(debug_volume, 
                {create_and_format_partition, creating_partition_table_error, 
                    {error, Error}
                }
            ),            
            {error, <<"Failed to create partition table">>};
        {create_partition, {error, Error}} ->
            ?event(debug_volume, 
                {create_and_format_partition, creating_partition_error, 
                    {error, Error}
                }
            ),            
            {error, <<"Failed to create partition">>};
        {format_partition, {error, Error}} ->
            ?event(debug_volume, 
                {create_and_format_partition, format_partition_error, 
                    {error, Error}
                }
            ),            
            {error, <<"Failed to format partition">>}
    end.

%% Simplified mounting function - does one thing well
-spec mount_volume(term(), term(), term(), term()) -> ok | {error, binary()}.
mount_volume(Partition, Key, MountPoint, VolumeName) ->
    ?event(debug_volume, 
        {mount_volume, entry, 
            {attempting_mount, Partition, MountPoint}
        }
    ),
    case hb_volume:mount_disk(Partition, Key, MountPoint, VolumeName) of
        {ok, MountResult} ->
            ?event(debug_volume,
                {mount_volume, mount_success, MountResult}
            ),
            ok;
        {error, MountError} ->
            ?event(debug_volume, 
                {mount_volume, mount_error, 
                    {error, MountError}
                }
            ),
            {error, <<"Failed to mount volume">>}
    end.

-spec update_node_store(term(), map()) -> {ok, term()} | {error, binary()}.
update_node_store(StorePath, Opts) ->
    ?event(debug_volume, {update_node_store, entry, StorePath}),
    CurrentStore = hb_opts:get(store, [], Opts),
    
    case hb_volume:change_node_store(StorePath, CurrentStore) of
        {ok, #{<<"store">> := NewStore} = StoreResult} ->
            ?event(debug_volume, 
                {update_node_store, store_change_success, 
                    {result, StoreResult}
                }
            ), 
            {ok, NewStore};
        {error, StoreError} ->
            ?event(debug_volume, 
                {update_node_store, store_change_error, 
                    {error, StoreError}
                }
            ),
            {error, <<"Failed to update store">>}
    end.

-spec update_application_config(term(), term(), map()) -> {ok, binary()}.
update_application_config(StorePath, NewStore, Opts) ->
    ?event(debug_volume,
        {update_application_config, entry,
            {updating_node_config, StorePath, NewStore}
        }
    ),
    GenesisWasmDBDir =
        hb_opts:get(
            genesis_wasm_db_dir,
            "cache-mainnet/genesis-wasm",
            Opts
        ),
    ?event(debug_volume, 
        {update_application_config, genesis_dir, GenesisWasmDBDir}
    ),  
    BinaryGenesisWasmDBDir = list_to_binary(GenesisWasmDBDir),
    FullGenesisPath =
        <<StorePath/binary, "/", BinaryGenesisWasmDBDir/binary>>,
    ?event(debug_volume, 
        {update_application_config, full_path_created, FullGenesisPath}
    ),
    ok = hb_http_server:set_opts(
        Opts#{
            store => NewStore, 
            genesis_wasm_db_dir => FullGenesisPath
        }
    ),
    ?event(debug_volume, 
        {update_application_config, config_updated, success}
    ),
    {ok, <<"Volume mounted and store updated successfully">>}.

%% @doc Returns the node's public key for secure key exchange.
%%
%% This function retrieves the node's wallet and extracts the public key
%% for encryption purposes. It allows users to securely exchange 
%% encryption keys by first encrypting their volume key with the node's 
%% public key.
%%
%% The process ensures that sensitive keys are never transmitted in 
%% plaintext. The encrypted key can then be securely sent to the node, 
%% which will decrypt it using its private key before using it for volume 
%% encryption.
%%
%% @param _M1 Ignored parameter.
%% @param _M2 Ignored parameter.
%% @param Opts A map of configuration options.
%% @returns `{ok, Map}' containing the node's public key on success, or
%% `{error, Binary}' if the node's wallet is not available.
-spec public_key(term(), term(), map()) -> 
    {ok, map()} | {error, binary()}.
public_key(_M1, _M2, Opts) ->
    % Retrieve the node's wallet
    case hb_opts:get(priv_wallet, undefined, Opts) of
        undefined ->
            % Node doesn't have a wallet yet
            ?event(debug_volume, 
                {public_key, wallet_error, no_wallet_found}
            ),
            {error, <<"Node wallet not available">>};
        {{_KeyType, _Priv, Pub}, _PubKey} ->
            ?event(debug_volume, 
                {public_key, wallet_found, key_conversion_starting}
            ),
            % Convert to a standard RSA format (PKCS#1 or X.509)
            RsaPubKey = #'RSAPublicKey'{
                publicExponent = 65537,  % Common RSA exponent
                modulus = crypto:bytes_to_integer(Pub)
            },
            % Convert to DER format
            DerEncoded = public_key:der_encode('RSAPublicKey', RsaPubKey),
            % Base64 encode for transmission
            Base64Key = base64:encode(DerEncoded),
            ?event(debug_volume, {public_key, success, key_encoded}),
            {ok, #{
                <<"status">> => 200,
                <<"public_key">> => Base64Key,
                <<"message">> => 
                    <<"Use this public key to encrypt your volume key">>
            }}
    end.

%% @doc Decrypts an encrypted volume key using the node's private key.
%%
%% This function takes an encrypted key (typically sent by a client who 
%% encrypted it with the node's public key) and decrypts it using the 
%% node's private RSA key.
%%
%% @param EncryptedKey The encrypted volume key (Base64 encoded).
%% @param Opts A map of configuration options.
%% @returns `{ok, DecryptedKey}' on successful decryption, or
%% `{error, Binary}' if decryption fails.
-spec decrypt_volume_key(binary(), map()) -> 
    {ok, binary()} | {error, binary()}.
decrypt_volume_key(EncryptedKeyBase64, Opts) ->
    % Decode the encrypted key
    try
        EncryptedKey = base64:decode(EncryptedKeyBase64),
        ?event(debug_volume, 
            {decrypt_volume_key, base64_decoded, success}
        ),
        % Retrieve the node's wallet with private key
        case hb_opts:get(priv_wallet, undefined, Opts) of
            undefined ->
                ?event(debug_volume, 
                    {decrypt_volume_key, wallet_error, no_wallet}
                ),
                {error, <<"Node wallet not available for decryption">>};
            {{_KeyType = {rsa, E}, Priv, Pub}, _PubKey} ->
                ?event(debug_volume, 
                    {decrypt_volume_key, wallet_found, creating_private_key}
                ),
                % Create RSA private key record for decryption
                RsaPrivKey = #'RSAPrivateKey'{
                    publicExponent = E,
                    modulus = crypto:bytes_to_integer(Pub),
                    privateExponent = crypto:bytes_to_integer(Priv)
                },
                % Decrypt the key
                DecryptedKey = 
                    public_key:decrypt_private(
                        EncryptedKey, 
                        RsaPrivKey
                    ),
                ?event(debug_volume, 
                    {decrypt_volume_key, decryption_success, key_decrypted}
                ),
                {ok, DecryptedKey}
        end
    catch
        _:Error ->
            ?event(debug_volume, 
                {decrypt_volume_key, decryption_error, Error}
            ),
            {error, <<"Failed to decrypt volume key">>}
    end.
