# [Module dev_volume.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_volume.erl)




Secure Volume Management for HyperBEAM Nodes.

<a name="description"></a>

## Description ##

This module handles encrypted storage operations for HyperBEAM, providing
a robust and secure approach to data persistence. It manages the complete
lifecycle of encrypted volumes from detection to creation, formatting, and
mounting.

Key responsibilities:
- Volume detection and initialization
- Encrypted partition creation and formatting
- Secure mounting using cryptographic keys
- Store path reconfiguration to use mounted volumes
- Automatic handling of various system states
(new device, existing partition, etc.)

The primary entry point is the `mount/3` function, which orchestrates the
entire process based on the provided configuration parameters. This module
works alongside `hb_volume` which provides the low-level operations for
device manipulation.

Security considerations:
- Ensures data at rest is protected through LUKS encryption
- Provides proper volume sanitization and secure mounting
- IMPORTANT: This module only applies configuration set in node options and
does NOT accept disk operations via HTTP requests. It cannot format arbitrary
disks as all operations are safeguarded by host operating system permissions
enforced upon the HyperBEAM environment.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check_base_device-8">check_base_device/8*</a></td><td>Check if the base device exists and if it does, check if the partition exists.</td></tr><tr><td valign="top"><a href="#check_partition-8">check_partition/8*</a></td><td>Check if the partition exists.</td></tr><tr><td valign="top"><a href="#create_and_mount_partition-8">create_and_mount_partition/8*</a></td><td>Create, format and mount a new partition.</td></tr><tr><td valign="top"><a href="#decrypt_volume_key-2">decrypt_volume_key/2*</a></td><td>Decrypts an encrypted volume key using the node's private key.</td></tr><tr><td valign="top"><a href="#format_and_mount-6">format_and_mount/6*</a></td><td>Format and mount a newly created partition.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Exported function for getting device info, controls which functions are
exposed via the device API.</td></tr><tr><td valign="top"><a href="#info-3">info/3</a></td><td>HTTP info response providing information about this device.</td></tr><tr><td valign="top"><a href="#mount-3">mount/3</a></td><td>Handles the complete process of secure encrypted volume mounting.</td></tr><tr><td valign="top"><a href="#mount_existing_partition-6">mount_existing_partition/6*</a></td><td>Mount an existing partition.</td></tr><tr><td valign="top"><a href="#mount_formatted_partition-6">mount_formatted_partition/6*</a></td><td>Mount a newly formatted partition.</td></tr><tr><td valign="top"><a href="#public_key-3">public_key/3</a></td><td>Returns the node's public key for secure key exchange.</td></tr><tr><td valign="top"><a href="#update_node_config-2">update_node_config/2*</a></td><td>Update the node's configuration with the new store.</td></tr><tr><td valign="top"><a href="#update_store_path-2">update_store_path/2*</a></td><td>Update the store path to use the mounted volume.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check_base_device-8"></a>

### check_base_device/8 * ###

<pre><code>
check_base_device(Device::term(), Partition::term(), PartitionType::term(), VolumeName::term(), MountPoint::term(), StorePath::term(), Key::term(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`Device`: The base device to check.<br />`Partition`: The partition to check.<br />`PartitionType`: The type of partition to check.<br />`VolumeName`: The name of the volume to check.<br />`MountPoint`: The mount point to check.<br />`StorePath`: The store path to check.<br />`Key`: The key to check.<br />`Opts`: The options to check.<br />

returns: `{ok, Binary}` on success with operation result message, or
`{error, Binary}` on failure with error message.

Check if the base device exists and if it does, check if the partition exists.

<a name="check_partition-8"></a>

### check_partition/8 * ###

<pre><code>
check_partition(Device::term(), Partition::term(), PartitionType::term(), VolumeName::term(), MountPoint::term(), StorePath::term(), Key::term(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`Device`: The base device to check.<br />`Partition`: The partition to check.<br />`PartitionType`: The type of partition to check.<br />`VolumeName`: The name of the volume to check.<br />`MountPoint`: The mount point to check.<br />`StorePath`: The store path to check.<br />`Key`: The key to check.<br />`Opts`: The options to check.<br />

returns: `{ok, Binary}` on success with operation result message, or
`{error, Binary}` on failure with error message.

Check if the partition exists. If it does, attempt to mount it.
If it doesn't exist, create it, format it with encryption and mount it.

<a name="create_and_mount_partition-8"></a>

### create_and_mount_partition/8 * ###

<pre><code>
create_and_mount_partition(Device::term(), Partition::term(), PartitionType::term(), Key::term(), MountPoint::term(), VolumeName::term(), StorePath::term(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`Device`: The device to create the partition on.<br />`Partition`: The partition to create.<br />`PartitionType`: The type of partition to create.<br />`Key`: The key to create the partition with.<br />`MountPoint`: The mount point to mount the partition to.<br />`VolumeName`: The name of the volume to mount.<br />`StorePath`: The store path to mount.<br />`Opts`: The options to mount.<br />

returns: `{ok, Binary}` on success with operation result message, or
`{error, Binary}` on failure with error message.

Create, format and mount a new partition.

<a name="decrypt_volume_key-2"></a>

### decrypt_volume_key/2 * ###

<pre><code>
decrypt_volume_key(EncryptedKeyBase64::binary(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`Opts`: A map of configuration options.<br />

returns: `{ok, DecryptedKey}` on successful decryption, or
`{error, Binary}` if decryption fails.

Decrypts an encrypted volume key using the node's private key.

This function takes an encrypted key (typically sent by a client who encrypted
it with the node's public key) and decrypts it using the node's private RSA key.

<a name="format_and_mount-6"></a>

### format_and_mount/6 * ###

<pre><code>
format_and_mount(Partition::term(), Key::term(), MountPoint::term(), VolumeName::term(), StorePath::term(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`Partition`: The partition to format and mount.<br />`Key`: The key to format and mount the partition with.<br />`MountPoint`: The mount point to mount the partition to.<br />`VolumeName`: The name of the volume to mount.<br />`StorePath`: The store path to mount.<br />`Opts`: The options to mount.<br />

returns: `{ok, Binary}` on success with operation result message, or
`{error, Binary}` on failure with error message.

Format and mount a newly created partition.

<a name="info-1"></a>

### info/1 ###

`info(X1) -> any()`

Exported function for getting device info, controls which functions are
exposed via the device API.

<a name="info-3"></a>

### info/3 ###

`info(Msg1, Msg2, Opts) -> any()`

HTTP info response providing information about this device

<a name="mount-3"></a>

### mount/3 ###

<pre><code>
mount(M1::term(), M2::term(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`M1`: Base message for context.<br />`M2`: Request message with operation details.<br />`Opts`: A map of configuration options for volume operations.<br />

returns: `{ok, Binary}` on success with operation result message, or
`{error, Binary}` on failure with error message.

Handles the complete process of secure encrypted volume mounting.

This function performs the following operations depending on the state:
1. Validates the encryption key is present
2. Checks if the base device exists
3. Checks if the partition exists on the device
4. If the partition exists, attempts to mount it
5. If the partition doesn't exist, creates it, formats it with encryption
and mounts it
6. Updates the node's store configuration to use the mounted volume

Config options in Opts map:
- volume_key: (Required) The encryption key
- volume_device: Base device path
- volume_partition: Partition path
- volume_partition_type: Filesystem type
- volume_name: Name for encrypted volume
- volume_mount_point: Where to mount
- volume_store_path: Store path on volume

<a name="mount_existing_partition-6"></a>

### mount_existing_partition/6 * ###

<pre><code>
mount_existing_partition(Partition::term(), Key::term(), MountPoint::term(), VolumeName::term(), StorePath::term(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`Partition`: The partition to mount.<br />`Key`: The key to mount.<br />`MountPoint`: The mount point to mount.<br />`VolumeName`: The name of the volume to mount.<br />`StorePath`: The store path to mount.<br />`Opts`: The options to mount.<br />

returns: `{ok, Binary}` on success with operation result message, or
`{error, Binary}` on failure with error message.

Mount an existing partition.

<a name="mount_formatted_partition-6"></a>

### mount_formatted_partition/6 * ###

<pre><code>
mount_formatted_partition(Partition::term(), Key::term(), MountPoint::term(), VolumeName::term(), StorePath::term(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`Partition`: The partition to mount.<br />`Key`: The key to mount the partition with.<br />`MountPoint`: The mount point to mount the partition to.<br />`VolumeName`: The name of the volume to mount.<br />`StorePath`: The store path to mount.<br />`Opts`: The options to mount.<br />

returns: `{ok, Binary}` on success with operation result message, or
`{error, Binary}` on failure with error message.

Mount a newly formatted partition.

<a name="public_key-3"></a>

### public_key/3 ###

<pre><code>
public_key(M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`Opts`: A map of configuration options.<br />

returns: `{ok, Map}` containing the node's public key on success, or
`{error, Binary}` if the node's wallet is not available.

Returns the node's public key for secure key exchange.

This function retrieves the node's wallet and extracts the public key
for encryption purposes. It allows users to securely exchange encryption keys
by first encrypting their volume key with the node's public key.

The process ensures that sensitive keys are never transmitted in plaintext.
The encrypted key can then be securely sent to the node, which will decrypt it
using its private key before using it for volume encryption.

<a name="update_node_config-2"></a>

### update_node_config/2 * ###

<pre><code>
update_node_config(NewStore::term(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`NewStore`: The new store to update the node's configuration with.<br />`Opts`: The options to update the node's configuration with.<br />

returns: `{ok, Binary}` on success with operation result message, or
`{error, Binary}` on failure with error message.

Update the node's configuration with the new store.

<a name="update_store_path-2"></a>

### update_store_path/2 * ###

<pre><code>
update_store_path(StorePath::term(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`StorePath`: The store path to update.<br />`Opts`: The options to update.<br />

returns: `{ok, Binary}` on success with operation result message, or
`{error, Binary}` on failure with error message.

Update the store path to use the mounted volume.

