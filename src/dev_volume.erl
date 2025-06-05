%%% @doc Secure Volume Management for HyperBEAM Nodes
%%%
%%% This module handles encrypted storage operations for HyperBEAM, providing 
%%% a robust and secure approach to data persistence. It manages the complete 
%%% lifecycle of encrypted volumes from detection to creation, formatting, and 
%%% mounting.
%%%
%%% Key responsibilities:
%%% - Volume detection and initialization
%%% - Encrypted partition creation and formatting
%%% - Secure mounting using cryptographic keys
%%% - Store path reconfiguration to use mounted volumes
%%% - Automatic handling of various system states 
%%%   (new device, existing partition, etc.)
%%%
%%% The primary entry point is the `mount/3' function, which orchestrates the 
%%% entire process based on the provided configuration parameters. This module 
%%% works alongside `hb_volume' which provides the low-level operations for 
%%% device manipulation.
%%%
%%% Security considerations:
%%% - Ensures data at rest is protected through LUKS encryption
%%% - Provides proper volume sanitization and secure mounting
%%% - IMPORTANT: This module only applies configuration set in node options and
%%%   does NOT accept disk operations via HTTP requests. It cannot format arbitrary
%%%   disks as all operations are safeguarded by host operating system permissions
%%%   enforced upon the HyperBEAM environment.
-module(dev_volume).
-export([info/1, info/3, mount/3, public_key/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%% @doc Exported function for getting device info, controls which functions are
%% exposed via the device API.
info(_) -> 
    #{ exports => [info, mount, public_key] }.

%% @doc HTTP info response providing information about this device
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Secure Volume Management for HyperBEAM Nodes">>,
        <<"version">> => <<"1.0">>,
        <<"api">> => #{
            <<"info">> => #{
                <<"description">> => <<"Get device info">>
            },
            <<"mount">> => #{
                <<"description">> => <<"Mount an encrypted volume">>,
                <<"required_node_opts">> => #{
                    <<"volume_key">> => <<"The encryption key">>,
                    <<"volume_device">> => <<"The base device path">>,
                    <<"volume_partition">> => <<"The partition path">>,
                    <<"volume_partition_type">> => <<"The partition type">>,
                    <<"volume_name">> => <<"The name for the encrypted volume">>,
                    <<"volume_mount_point">> => <<"Where to mount the volume">>,
                    <<"volume_store_path">> => <<"The store path on the volume">>
                }
            },
            <<"public_key">> => #{
                <<"description">> => <<"Get the node's public key for encrypted key exchange">>
            }
        }
    },
    {ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc Handles the complete process of secure encrypted volume mounting.
%%
%% This function performs the following operations depending on the state:
%% 1. Validates the encryption key is present
%% 2. Checks if the base device exists
%% 3. Checks if the partition exists on the device
%% 4. If the partition exists, attempts to mount it
%% 5. If the partition doesn't exist, creates it, formats it with encryption 
%%    and mounts it
%% 6. Updates the node's store configuration to use the mounted volume
%%
%% Config options in Opts map:
%% - volume_key: (Required) The encryption key
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
-spec mount(term(), term(), map()) -> {ok, binary()} | {error, binary()}.
mount(_M1, _M2, Opts) ->
    % Check if an encrypted key was sent in the request
    EncryptedKey = hb_opts:get(volume_key, not_found, Opts),
    % Determine if we need to decrypt a key or use one from config
    ?event(debug_mount, {mount, encrypted_key, EncryptedKey}),
    Key = case decrypt_volume_key(EncryptedKey, Opts) of
        {ok, DecryptedKey} ->
            ?event(debug_mount, {mount, decrypted_key, DecryptedKey}),
            DecryptedKey;
        {error, DecryptError} ->
            ?event(debug_mount, {mount, key_decrypt_error, DecryptError}),
            not_found
    end,
    Device = hb_opts:get(volume_device, not_found, Opts),
    Partition = hb_opts:get(volume_partition, not_found, Opts),
    PartitionType = hb_opts:get(volume_partition_type, not_found, Opts),
    VolumeName = hb_opts:get(volume_name, not_found, Opts),
    MountPoint = hb_opts:get(volume_mount_point, not_found, Opts),
    StorePath = hb_opts:get(volume_store_path, not_found, Opts),
    % Check for missing required node options
    case hb_opts:check_required_opts([
        {<<"volume_key">>, Key},
        {<<"volume_device">>, Device},
        {<<"volume_partition">>, Partition},
        {<<"volume_partition_type">>, PartitionType},
        {<<"volume_name">>, VolumeName}, 
        {<<"volume_mount_point">>, MountPoint},
        {<<"volume_store_path">>, StorePath}
    ], Opts) of
        {ok, _} ->
            ?event(debug_mount, {mount, device, Device}),
            ?event(debug_mount, {mount, partition, Partition}),
            ?event(debug_mount, {mount, partition_type, PartitionType}),
            ?event(debug_mount, {mount, mount_point, MountPoint}),	
            check_base_device(
                Device, Partition, PartitionType, VolumeName, 
                MountPoint, StorePath, Key, Opts
            );
        {error, ErrorMsg} ->
            ?event(mount, {error, ErrorMsg}),
            {error, ErrorMsg}
    end.

%% @doc Returns the node's public key for secure key exchange.
%%
%% This function retrieves the node's wallet and extracts the public key
%% for encryption purposes. It allows users to securely exchange encryption keys
%% by first encrypting their volume key with the node's public key.
%%
%% The process ensures that sensitive keys are never transmitted in plaintext.
%% The encrypted key can then be securely sent to the node, which will decrypt it
%% using its private key before using it for volume encryption.
%%
%% @param _M1 Ignored parameter.
%% @param _M2 Ignored parameter.
%% @param Opts A map of configuration options.
%% @returns `{ok, Map}' containing the node's public key on success, or
%% `{error, Binary}' if the node's wallet is not available.
-spec public_key(term(), term(), map()) -> {ok, map()} | {error, binary()}.
public_key(_M1, _M2, Opts) ->
    ?event(volume, {public_key, start}),
    % Retrieve the node's wallet
    case hb_opts:get(priv_wallet, undefined, Opts) of
        undefined ->
            % Node doesn't have a wallet yet
            ?event(volume, {public_key, error, <<"no wallet found">>}),
            {error, <<"Node wallet not available">>};
        {{_KeyType, _Priv, Pub}, _PubKey} ->
            % Convert to a standard RSA format (PKCS#1 or X.509)
            RsaPubKey = #'RSAPublicKey'{
                publicExponent = 65537,  % Common RSA exponent
                modulus = crypto:bytes_to_integer(Pub)
            },
            % Convert to DER format
            DerEncoded = public_key:der_encode('RSAPublicKey', RsaPubKey),
            % Base64 encode for transmission
            Base64Key = base64:encode(DerEncoded),
            {ok, #{
                <<"status">> => 200,
                <<"public_key">> => Base64Key,
                <<"message">> => <<"Use this public key to encrypt your volume key">>
            }}
    end.

%% @doc Decrypts an encrypted volume key using the node's private key.
%%
%% This function takes an encrypted key (typically sent by a client who encrypted
%% it with the node's public key) and decrypts it using the node's private RSA key.
%%
%% @param EncryptedKey The encrypted volume key (Base64 encoded).
%% @param Opts A map of configuration options.
%% @returns `{ok, DecryptedKey}' on successful decryption, or
%% `{error, Binary}' if decryption fails.
-spec decrypt_volume_key(binary(), map()) -> {ok, binary()} | {error, binary()}.
decrypt_volume_key(EncryptedKeyBase64, Opts) ->
    % Decode the encrypted key
    try
        EncryptedKey = base64:decode(EncryptedKeyBase64),
        ?event(debug_mount, {decrypt_volume_key, encrypted_key, EncryptedKey}),
        % Retrieve the node's wallet with private key
        case hb_opts:get(priv_wallet, undefined, Opts) of
            undefined ->
                {error, <<"Node wallet not available for decryption">>};
            {{_KeyType = {rsa, E}, Priv, Pub}, _PubKey} ->
                % Create RSA private key record for decryption
                RsaPrivKey = #'RSAPrivateKey'{
                    publicExponent = E,
                    modulus = crypto:bytes_to_integer(Pub),
                    privateExponent = crypto:bytes_to_integer(Priv)
                },
                % Decrypt the key
                DecryptedKey = public_key:decrypt_private(EncryptedKey, RsaPrivKey),
                {ok, DecryptedKey}
        end
    catch
        _:Error ->
            ?event(debug_mount, {decrypt_volume_key, error, Error}),
            {error, <<"Failed to decrypt volume key">>}
    end.

%% @doc Check if the base device exists and if it does, check if the partition exists.
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
-spec check_base_device(
    term(), term(), term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
check_base_device(
    Device, Partition, PartitionType, VolumeName, MountPoint, StorePath, 
    Key, Opts
) ->
    case hb_volume:check_for_device(Device) of
        false ->
            % Base device doesn't exist
            ?event(debug_mount, 
                {device_check, error, <<"Base device not found">>}
            ),
            {error, <<"Base device not found">>};
        true ->
            check_partition(
                Device, Partition, PartitionType, VolumeName, 
                MountPoint, StorePath, Key, Opts
            )
    end.

%% @doc Check if the partition exists. If it does, attempt to mount it.
%% If it doesn't exist, create it, format it with encryption and mount it.
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
-spec check_partition(
    term(), term(), term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
check_partition(
    Device, Partition, PartitionType, VolumeName, MountPoint, StorePath, 
    Key, Opts
) ->
    case hb_volume:check_for_device(Partition) of
        true ->
            % Partition exists, try mounting it
            mount_existing_partition(
                Partition, Key, MountPoint, VolumeName, StorePath, Opts
            );
        false ->
            % Partition doesn't exist, create it
            create_and_mount_partition(
                Device, Partition, PartitionType, Key, 
                MountPoint, VolumeName, StorePath, Opts
            )
    end.

%% @doc Mount an existing partition.
%% @param Partition The partition to mount.
%% @param Key The key to mount.
%% @param MountPoint The mount point to mount.
%% @param VolumeName The name of the volume to mount.
%% @param StorePath The store path to mount.
%% @param Opts The options to mount.
%% @returns `{ok, Binary}' on success with operation result message, or
%% `{error, Binary}' on failure with error message.
-spec mount_existing_partition(
    term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
mount_existing_partition(
    Partition, Key, MountPoint, VolumeName, StorePath, Opts
) ->
    ?event(debug_mount, {mount_volume, attempt, Partition}),
    case hb_volume:mount_disk(Partition, Key, MountPoint, VolumeName) of
        {ok, MountResult} ->
            ?event(debug_mount, {mount_volume, success, MountResult}),
            update_store_path(StorePath, Opts);
        {error, MountError} ->
            ?event(debug_mount, {mount_volume, error, MountError}),
            {error, <<"Failed to mount volume">>}
    end.

%% @doc Create, format and mount a new partition.
%% @param Device The device to create the partition on.
%% @param Partition The partition to create.
%% @param PartitionType The type of partition to create.
%% @param Key The key to create the partition with.
%% @param MountPoint The mount point to mount the partition to.
%% @param VolumeName The name of the volume to mount.
%% @param StorePath The store path to mount.
%% @param Opts The options to mount.
%% @returns `{ok, Binary}' on success with operation result message, or
%% `{error, Binary}' on failure with error message.
-spec create_and_mount_partition(
    term(), term(), term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
create_and_mount_partition(
    Device, Partition, PartitionType, Key, 
    MountPoint, VolumeName, StorePath, Opts
) ->
    ?event(debug_mount, {create_partition, attempt, Device}),
    case hb_volume:create_partition(Device, PartitionType) of
        {ok, PartitionResult} ->
            ?event(debug_mount, {partition_create, success, PartitionResult}),
            format_and_mount(
                Partition, Key, MountPoint, VolumeName, StorePath, Opts
            );
        {error, PartitionError} ->
            ?event(debug_mount, {partition_create, error, PartitionError}),
            {error, <<"Failed to create partition">>}
    end.

%% @doc Format and mount a newly created partition.
%% @param Partition The partition to format and mount.
%% @param Key The key to format and mount the partition with.
%% @param MountPoint The mount point to mount the partition to.
%% @param VolumeName The name of the volume to mount.
%% @param StorePath The store path to mount.
%% @param Opts The options to mount.
%% @returns `{ok, Binary}' on success with operation result message, or
%% `{error, Binary}' on failure with error message.
-spec format_and_mount(
    term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
format_and_mount(
    Partition, Key, MountPoint, VolumeName, StorePath, Opts
) ->
    case hb_volume:format_disk(Partition, Key) of
        {ok, FormatResult} ->
            ?event(debug_mount, {format_disk, success, FormatResult}),
            mount_formatted_partition(
                Partition, Key, MountPoint, VolumeName, StorePath, Opts
            );
        {error, FormatError} ->
            ?event(debug_mount, {format_disk, error, FormatError}),
            {error, <<"Failed to format disk">>}
    end.

%% @doc Mount a newly formatted partition.
%% @param Partition The partition to mount.
%% @param Key The key to mount the partition with.
%% @param MountPoint The mount point to mount the partition to.
%% @param VolumeName The name of the volume to mount.
%% @param StorePath The store path to mount.
%% @param Opts The options to mount.
%% @returns `{ok, Binary}' on success with operation result message, or
%% `{error, Binary}' on failure with error message.
-spec mount_formatted_partition(
    term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
mount_formatted_partition(
    Partition, Key, MountPoint, VolumeName, StorePath, Opts
) ->
    case hb_volume:mount_disk(Partition, Key, MountPoint, VolumeName) of
        {ok, RetryMountResult} ->
            ?event(debug_mount, {mount_volume, success, RetryMountResult}),
            update_store_path(StorePath, Opts);
        {error, RetryMountError} ->
            ?event(debug_mount, {mount_volume, error, RetryMountError}),
            {error, <<"Failed to mount newly formatted volume">>}
    end.

%% @doc Update the store path to use the mounted volume.
%% @param StorePath The store path to update.
%% @param Opts The options to update.
%% @returns `{ok, Binary}' on success with operation result message, or
%% `{error, Binary}' on failure with error message.
-spec update_store_path(term(), map()) -> {ok, binary()} | {error, binary()}.
update_store_path(StorePath, Opts) ->
    CurrentStore = hb_opts:get(store, [], Opts),
    case hb_volume:change_node_store(StorePath, CurrentStore) of
        {ok, #{<<"store">> := NewStore} = StoreResult} ->
            ?event(debug_mount, {store_update, success, StoreResult}),
            update_node_config(NewStore, Opts);
        {error, StoreError} ->
            ?event(debug_mount, {store_update, error, StoreError}),
            {error, <<"Failed to update store">>}
    end.

%% @doc Update the node's configuration with the new store.
%% @param NewStore The new store to update the node's configuration with.
%% @param Opts The options to update the node's configuration with.
%% @returns `{ok, Binary}' on success with operation result message, or
%% `{error, Binary}' on failure with error message.
-spec update_node_config(term(), map()) -> {ok, binary()} | {error, binary()}.
update_node_config(NewStore, Opts) ->
    ok = hb_http_server:set_opts(Opts#{store => NewStore}),
    ?event(debug_mount, {store_update, config_updated}),
    {ok, <<"Volume mounted and store updated successfully">>}.
