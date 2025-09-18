# dev_volume

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_volume.erl)

Secure Volume Management for HyperBEAM Nodes
This module handles encrypted storage operations for HyperBEAM, 
providing a robust and secure approach to data persistence. It manages 
the complete lifecycle of encrypted volumes from detection to creation, 
formatting, and mounting.
Key responsibilities:
- Volume detection and initialization
- Encrypted partition creation and formatting
- Secure mounting using cryptographic keys
- Store path reconfiguration to use mounted volumes
- Automatic handling of various system states 
  (new device, existing partition, etc.)
The primary entry point is the `mount/3` function, which orchestrates 
the entire process based on the provided configuration parameters. This 
module works alongside `hb_volume` which provides the low-level 
operations for device manipulation.
Security considerations:
- Ensures data at rest is protected through LUKS encryption
- Provides proper volume sanitization and secure mounting
- IMPORTANT: This module only applies configuration set in node options 
  and does NOT accept disk operations via HTTP requests. It cannot 
  format arbitrary disks as all operations are safeguarded by host 
  operating system permissions enforced upon the HyperBEAM environment.

---

## Exported Functions

- `info/1`
- `info/3`
- `mount/3`
- `public_key/3`

---

### info

Secure Volume Management for HyperBEAM Nodes
Exported function for getting device info, controls which functions 

```erlang
info(_) -> 
    ?event(debug_volume, {info, entry, device_info_requested}),
    #{ exports => [info, mount, public_key] }.
```

### info

HTTP info response providing information about this device
Handles the complete process of secure encrypted volume mounting.

```erlang
-spec mount(term(), term(), map()) -> 
    {ok, binary()} | {error, binary()}.
```

```erlang
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
%%
%%
%%
```

### mount

HTTP info response providing information about this device
Handles the complete process of secure encrypted volume mounting.

```erlang
-spec mount(term(), term(), map()) -> 
    {ok, binary()} | {error, binary()}.
```

```erlang
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
            check_base_device(
                Device, Partition, PartitionType, VolumeName, 
                MountPoint, StorePath, Key, Opts
            );
        {error, ErrorMsg} ->
            ?event(debug_volume, {mount, required_opts_error, ErrorMsg}),
            {error, ErrorMsg}
    end.
```

### public_key

Returns the node's public key for secure key exchange.

```erlang
-spec public_key(term(), term(), map()) -> 
    {ok, map()} | {error, binary()}.
```

```erlang
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
```

### decrypt_volume_key

Decrypts an encrypted volume key using the node's private key.

```erlang
-spec decrypt_volume_key(binary(), map()) -> 
    {ok, binary()} | {error, binary()}.
```

```erlang
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
```

### check_base_device

Check if the base device exists and if it does, check if the 

```erlang
-spec check_base_device(
    term(), term(), term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
```

```erlang
check_base_device(
    Device, Partition, PartitionType, VolumeName, MountPoint, StorePath, 
    Key, Opts
) ->
    ?event(debug_volume, 
        {check_base_device, entry, {checking_device, Device}}
    ),
    case hb_volume:check_for_device(Device) of
        false ->
            % Base device doesn't exist
            ?event(debug_volume, 
                {check_base_device, device_not_found, Device}
            ),
            {error, <<"Base device not found">>};
        true ->
            ?event(debug_volume, 
                {check_base_device, device_found, 
                    {proceeding_to_partition_check, Device}
                }
            ),
            check_partition(
                Device, Partition, PartitionType, VolumeName, 
                MountPoint, StorePath, Key, Opts
            )
    end.
```

### check_partition

Check if the partition exists. If it does, attempt to mount it.

```erlang
-spec check_partition(
    term(), term(), term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
```

```erlang
check_partition(
    Device, Partition, PartitionType, VolumeName, MountPoint, StorePath, 
    Key, Opts
) ->
    ?event(debug_volume, 
        {check_partition, entry, {checking_partition, Partition}}
    ),
    case hb_volume:check_for_device(Partition) of
        true ->
            ?event(debug_volume, 
                {check_partition, partition_exists, 
                    {mounting_existing, Partition}
                }
            ),
            % Partition exists, try mounting it
            mount_existing_partition(
                Partition, Key, MountPoint, VolumeName, StorePath, Opts
            );
        false ->
            ?event(debug_volume, 
                {check_partition, partition_not_exists, 
                    {creating_new, Partition}
                }
            ),
            % Partition doesn't exist, create it
            create_and_mount_partition(
                Device, Partition, PartitionType, Key, 
                MountPoint, VolumeName, StorePath, Opts
            )
    end.
```

### mount_existing_partition

Mount an existing partition.

```erlang
-spec mount_existing_partition(
    term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
```

```erlang
mount_existing_partition(
    Partition, Key, MountPoint, VolumeName, StorePath, Opts
) ->
    ?event(debug_volume, 
        {mount_existing_partition, entry, 
            {attempting_mount, Partition, MountPoint}
        }
    ),
    case hb_volume:mount_disk(Partition, Key, MountPoint, VolumeName) of
        {ok, MountResult} ->
            ?event(debug_volume, 
                {mount_existing_partition, mount_success, MountResult}
            ),
            update_store_path(StorePath, Opts);
        {error, MountError} ->
            ?event(debug_volume, 
                {mount_existing_partition, mount_error, 
                    {error, MountError}
                }
            ),
            {error, <<"Failed to mount volume">>}
    end.
```

### create_and_mount_partition

Create, format and mount a new partition.

```erlang
-spec create_and_mount_partition(
    term(), term(), term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
```

```erlang
create_and_mount_partition(
    Device, Partition, PartitionType, Key, 
    MountPoint, VolumeName, StorePath, Opts
) ->
    ?event(debug_volume, 
        {create_and_mount_partition, entry, 
            {creating_partition, Device, PartitionType}
        }
    ),
    case hb_volume:create_partition(Device, PartitionType) of
        {ok, PartitionResult} ->
            ?event(debug_volume, 
                {create_and_mount_partition, partition_created, 
                    PartitionResult
                }
            ),
            format_and_mount(
                Partition, Key, MountPoint, VolumeName, StorePath, Opts
            );
        {error, PartitionError} ->
            ?event(debug_volume, 
                {create_and_mount_partition, partition_error, 
                    {error, PartitionError}
                }
            ),
            {error, <<"Failed to create partition">>}
    end.
```

### format_and_mount

Format and mount a newly created partition.

```erlang
-spec format_and_mount(
    term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
```

```erlang
format_and_mount(
    Partition, Key, MountPoint, VolumeName, StorePath, Opts
) ->
    ?event(debug_volume, 
        {format_and_mount, entry, {formatting_partition, Partition}}
    ),
    case hb_volume:format_disk(Partition, Key) of
        {ok, FormatResult} ->
            ?event(debug_volume, 
                {format_and_mount, format_success, 
                    {result, FormatResult}
                }
            ),
            mount_formatted_partition(
                Partition, Key, MountPoint, VolumeName, StorePath, Opts
            );
        {error, FormatError} ->
            ?event(debug_volume, 
                {format_and_mount, format_error, 
                    {error, FormatError}
                }
            ),
            {error, <<"Failed to format disk">>}
    end.
```

### mount_formatted_partition

Mount a newly formatted partition.

```erlang
-spec mount_formatted_partition(
    term(), term(), term(), term(), term(), map()
) -> {ok, binary()} | {error, binary()}.
```

```erlang
mount_formatted_partition(
    Partition, Key, MountPoint, VolumeName, StorePath, Opts
) ->
    ?event(debug_volume, 
        {mount_formatted_partition, entry, 
            {mounting_formatted, Partition, MountPoint}
        }
    ),
    case hb_volume:mount_disk(Partition, Key, MountPoint, VolumeName) of
        {ok, RetryMountResult} ->
            ?event(debug_volume, 
                {mount_formatted_partition, mount_success, 
                    {result, RetryMountResult}
                }
            ),
            update_store_path(StorePath, Opts);
        {error, RetryMountError} ->
            ?event(debug_volume, 
                {mount_formatted_partition, mount_error, 
                    {error, RetryMountError}
                }
            ),
            {error, <<"Failed to mount newly formatted volume">>}
    end.
```

### update_store_path

Update the store path to use the mounted volume.

```erlang
-spec update_store_path(term(), map()) -> 
    {ok, binary()} | {error, binary()}.
```

```erlang
update_store_path(StorePath, Opts) ->
    ?event(debug_volume, 
        {update_store_path, entry, {updating_store, StorePath}}
    ),
    CurrentStore = hb_opts:get(store, [], Opts),
    ?event(debug_volume, 
        {update_store_path, current_store, CurrentStore}
    ),
    case hb_volume:change_node_store(StorePath, CurrentStore) of
        {ok, #{<<"store">> := NewStore} = StoreResult} ->
            ?event(debug_volume, 
                {update_store_path, store_change_success, 
                    {result, StoreResult}
                }
            ),
            update_node_config(StorePath, NewStore, Opts);
        {error, StoreError} ->
            ?event(debug_volume, 
                {update_store_path, store_change_error, 
                    {error, StoreError}
                }
            ),
            {error, <<"Failed to update store">>}
    end.
```

### update_node_config

Update the node's configuration with the new store.

```erlang
-spec update_node_config(term(), term(), map()) -> 
    {ok, binary()} | {error, binary()}.
```

```erlang
update_node_config(StorePath, NewStore, Opts) ->
    ?event(debug_volume, 
        {update_node_config, entry, 
            {updating_config, StorePath, NewStore}
        }
    ),
    GenesisWasmDBDir = 
        hb_opts:get(
            genesis_wasm_db_dir,
            "cache-mainnet/genesis-wasm", 
            Opts
        ),
    ?event(debug_volume, 
        {update_node_config, genesis_dir, GenesisWasmDBDir}
    ),
    BinaryGenesisWasmDBDir = list_to_binary(GenesisWasmDBDir),
    FullGenesisPath = 
        <<StorePath/binary, "/", BinaryGenesisWasmDBDir/binary>>,
    ?event(debug_volume, 
        {update_node_config, full_path_created, FullGenesisPath}
    ),
    ok = 
        hb_http_server:set_opts(
            Opts#{
                store => NewStore, 
                genesis_wasm_db_dir => FullGenesisPath
            }
        ),
    ?event(debug_volume, 
        {update_node_config, config_updated, success}
    ),
```

---

*Generated from [dev_volume.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_volume.erl)*
