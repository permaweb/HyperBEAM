# [Module dev_green_zone.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_green_zone.erl)




The green zone device, which provides secure communication and identity
management between trusted nodes.

<a name="description"></a>

## Description ##
It handles node initialization, joining existing green zones, key exchange,
and node identity cloning. All operations are protected by hardware
commitment and encryption.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_trusted_node-4">add_trusted_node/4*</a></td><td>Adds a node to the trusted nodes list with its commitment report.</td></tr><tr><td valign="top"><a href="#become-3">become/3</a></td><td>Clones the identity of a target node in the green zone.</td></tr><tr><td valign="top"><a href="#calculate_node_message-3">calculate_node_message/3*</a></td><td>Generate the node message that should be set prior to joining
a green zone.</td></tr><tr><td valign="top"><a href="#decrypt_zone_key-2">decrypt_zone_key/2*</a></td><td>Decrypts an AES key using the node's RSA private key.</td></tr><tr><td valign="top"><a href="#default_zone_required_opts-1">default_zone_required_opts/1*</a></td><td>Provides the default required options for a green zone.</td></tr><tr><td valign="top"><a href="#encrypt_payload-2">encrypt_payload/2*</a></td><td>Encrypts an AES key with a node's RSA public key.</td></tr><tr><td valign="top"><a href="#finalize_become-5">finalize_become/5*</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Controls which functions are exposed via the device API.</td></tr><tr><td valign="top"><a href="#info-3">info/3</a></td><td>Provides information about the green zone device and its API.</td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>Initialize the green zone for a node.</td></tr><tr><td valign="top"><a href="#join-3">join/3</a></td><td>Initiates the join process for a node to enter an existing green zone.</td></tr><tr><td valign="top"><a href="#join_peer-5">join_peer/5*</a></td><td>Processes a join request to a specific peer node.</td></tr><tr><td valign="top"><a href="#key-3">key/3</a></td><td>Encrypts and provides the node's private key for secure sharing.</td></tr><tr><td valign="top"><a href="#maybe_set_zone_opts-4">maybe_set_zone_opts/4*</a></td><td>Adopts configuration from a peer when joining a green zone.</td></tr><tr><td valign="top"><a href="#rsa_wallet_integration_test-0">rsa_wallet_integration_test/0*</a></td><td>Test RSA operations with the existing wallet structure.</td></tr><tr><td valign="top"><a href="#try_mount_encrypted_volume-2">try_mount_encrypted_volume/2*</a></td><td>Attempts to mount an encrypted volume using the green zone AES key.</td></tr><tr><td valign="top"><a href="#validate_join-3">validate_join/3*</a></td><td>Validates an incoming join request from another node.</td></tr><tr><td valign="top"><a href="#validate_peer_opts-2">validate_peer_opts/2*</a></td><td>Validates that a peer's configuration matches required options.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_trusted_node-4"></a>

### add_trusted_node/4 * ###

<pre><code>
add_trusted_node(NodeAddr::binary(), Report::map(), RequesterPubKey::term(), Opts::map()) -&gt; ok
</code></pre>
<br />

`NodeAddr`: The joining node's address<br />`Report`: The commitment report provided by the joining node<br />`RequesterPubKey`: The joining node's public key<br />`Opts`: A map of configuration options<br />

returns: ok

Adds a node to the trusted nodes list with its commitment report.

This function updates the trusted nodes configuration:
1. Retrieves the current trusted nodes map
2. Adds the new node with its report and public key
3. Updates the node configuration with the new trusted nodes list

<a name="become-3"></a>

### become/3 ###

<pre><code>
become(M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`Opts`: A map of configuration options<br />

returns: `{ok, Map}` on success with confirmation details, or
`{error, Binary}` if the node is not part of a green zone or
identity adoption fails.

Clones the identity of a target node in the green zone.

This function performs the following operations:
1. Retrieves target node location and ID from the configuration
2. Verifies that the local node has a valid shared AES key
3. Requests the target node's encrypted key via its key endpoint
4. Verifies the response is from the expected peer
5. Decrypts the target node's private key using the shared AES key
6. Updates the local node's wallet with the target node's identity

Required configuration in Opts map:
- green_zone_peer_location: Target node's address
- green_zone_peer_id: Target node's unique identifier
- priv_green_zone_aes: The shared AES key for the green zone

<a name="calculate_node_message-3"></a>

### calculate_node_message/3 * ###

`calculate_node_message(RequiredOpts, Req, List) -> any()`

Generate the node message that should be set prior to joining
a green zone.

This function takes a required opts message, a request message, and an
`adopt-config` value. The `adopt-config` value can be a boolean, a list of
fields that should be included in the node message from the request, or a
binary string of fields to include, separated by commas.

<a name="decrypt_zone_key-2"></a>

### decrypt_zone_key/2 * ###

<pre><code>
decrypt_zone_key(EncZoneKey::binary(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`EncZoneKey`: The encrypted zone AES key (Base64 encoded or binary)<br />`Opts`: A map of configuration options<br />

returns: {ok, DecryptedKey} on success with the decrypted AES key

Decrypts an AES key using the node's RSA private key.

This function handles decryption of the zone key:
1. Decodes the encrypted key if it's in Base64 format
2. Extracts the RSA private key components from the wallet
3. Creates an RSA private key record
4. Performs private key decryption on the encrypted key

<a name="default_zone_required_opts-1"></a>

### default_zone_required_opts/1 * ###

<pre><code>
default_zone_required_opts(Opts::map()) -&gt; map()
</code></pre>
<br />

`Opts`: A map of configuration options from which to derive defaults<br />

returns: A map of required configuration options for the green zone

Provides the default required options for a green zone.

This function defines the baseline security requirements for nodes in a green zone:
1. Restricts loading of remote devices and only allows trusted signers
2. Limits to preloaded devices from the initiating machine
3. Enforces specific store configuration
4. Prevents route changes from the defaults
5. Requires matching hooks across all peers
6. Disables message scheduling to prevent conflicts
7. Enforces a permanent state to prevent further configuration changes

<a name="encrypt_payload-2"></a>

### encrypt_payload/2 * ###

<pre><code>
encrypt_payload(AESKey::binary(), RequesterPubKey::term()) -&gt; binary()
</code></pre>
<br />

`AESKey`: The shared AES key (256-bit binary)<br />`RequesterPubKey`: The node's public RSA key<br />

returns: The encrypted AES key

Encrypts an AES key with a node's RSA public key.

This function securely encrypts the shared key for transmission:
1. Extracts the RSA public key components
2. Creates an RSA public key record
3. Performs public key encryption on the AES key

<a name="finalize_become-5"></a>

### finalize_become/5 * ###

`finalize_become(KeyResp, NodeLocation, NodeID, GreenZoneAES, Opts) -> any()`

<a name="info-1"></a>

### info/1 ###

`info(X1) -> any()`

Controls which functions are exposed via the device API.

This function defines the security boundary for the green zone device by
explicitly listing which functions are available through the API.

<a name="info-3"></a>

### info/3 ###

`info(Msg1, Msg2, Opts) -> any()`

Provides information about the green zone device and its API.

This function returns detailed documentation about the device, including:
1. A high-level description of the device's purpose
2. Version information
3. Available API endpoints with their parameters and descriptions

<a name="init-3"></a>

### init/3 ###

<pre><code>
init(M1::term(), M2::term(), Opts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`Opts`: A map of configuration options<br />

returns: `{ok, Binary}` on success with confirmation message, or
`{error, Binary}` on failure with error message.

Initialize the green zone for a node.

This function performs the following operations:
1. Validates the node's history to ensure this is a valid initialization
2. Retrieves or creates a required configuration for the green zone
3. Ensures a wallet (keypair) exists or creates a new one
4. Generates a new 256-bit AES key for secure communication
5. Updates the node's configuration with these cryptographic identities

Config options in Opts map:
- green_zone_required_config: (Optional) Custom configuration requirements
- priv_wallet: (Optional) Existing wallet to use instead of creating a new one
- priv_green_zone_aes: (Optional) Existing AES key, if already part of a zone

<a name="join-3"></a>

### join/3 ###

<pre><code>
join(M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`M1`: The join request message with target peer information<br />`M2`: Additional request details, may include adoption preferences<br />`Opts`: A map of configuration options for join operations<br />

returns: `{ok, Map}` on success with join response details, or
`{error, Binary}` on failure with error message.

Initiates the join process for a node to enter an existing green zone.

This function performs the following operations depending on the state:
1. Validates the node's history to ensure proper initialization
2. Checks for target peer information (location and ID)
3. If target peer is specified:
a. Generates a commitment report for the peer
b. Prepares and sends a POST request to the target peer
c. Verifies the response and decrypts the returned zone key
d. Updates local configuration with the shared AES key
4. If no peer is specified, processes the join request locally

Config options in Opts map:
- green_zone_peer_location: Target peer's address
- green_zone_peer_id: Target peer's unique identifier
- green_zone_adopt_config:
(Optional) Whether to adopt peer's configuration (default: true)

<a name="join_peer-5"></a>

### join_peer/5 * ###

<pre><code>
join_peer(PeerLocation::binary(), PeerID::binary(), M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, map() | binary()}
</code></pre>
<br />

`PeerLocation`: The target peer's address<br />`PeerID`: The target peer's unique identifier<br />`M2`: May contain ShouldMount flag to enable encrypted volume mounting<br />

returns: `{ok, Map}` on success with confirmation message, or
`{error, Map|Binary}` on failure with error details

Processes a join request to a specific peer node.

This function handles the client-side join flow when connecting to a peer:
1. Verifies the node is not already in a green zone
2. Optionally adopts configuration from the target peer
3. Generates a hardware-backed commitment report
4. Sends a POST request to the peer's join endpoint
5. Verifies the response signature
6. Decrypts the returned AES key
7. Updates local configuration with the shared key
8. Optionally mounts an encrypted volume using the shared key

<a name="key-3"></a>

### key/3 ###

<pre><code>
key(M1::term(), M2::term(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`Opts`: A map of configuration options<br />

returns: `{ok, Map}` containing the encrypted key and IV on success, or
`{error, Binary}` if the node is not part of a green zone

Encrypts and provides the node's private key for secure sharing.

This function performs the following operations:
1. Retrieves the shared AES key and the node's wallet
2. Verifies that the node is part of a green zone (has a shared AES key)
3. Generates a random initialization vector (IV) for encryption
4. Encrypts the node's private key using AES-256-GCM with the shared key
5. Returns the encrypted key and IV for secure transmission

Required configuration in Opts map:
- priv_green_zone_aes: The shared AES key for the green zone
- priv_wallet: The node's wallet containing the private key to encrypt

<a name="maybe_set_zone_opts-4"></a>

### maybe_set_zone_opts/4 * ###

<pre><code>
maybe_set_zone_opts(PeerLocation::binary(), PeerID::binary(), Req::map(), InitOpts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`PeerLocation`: The location of the peer node to join<br />`PeerID`: The ID of the peer node to join<br />`Req`: The request message with adoption preferences<br />`InitOpts`: A map of initial configuration options<br />

returns: `{ok, Map}` with updated configuration on success, or
`{error, Binary}` if configuration retrieval fails

Adopts configuration from a peer when joining a green zone.

This function handles the conditional adoption of peer configuration:
1. Checks if adoption is enabled (default: true)
2. Requests required configuration from the peer
3. Verifies the authenticity of the configuration
4. Creates a node message with appropriate settings
5. Updates the local node configuration

Config options:
- green_zone_adopt_config: Controls configuration adoption (boolean, list, or binary)

<a name="rsa_wallet_integration_test-0"></a>

### rsa_wallet_integration_test/0 * ###

`rsa_wallet_integration_test() -> any()`

Test RSA operations with the existing wallet structure.

This test function verifies that encryption and decryption using the RSA keys
from the wallet work correctly. It creates a new wallet, encrypts a test
message with the RSA public key, and then decrypts it with the RSA private
key, asserting that the decrypted message matches the original.

<a name="try_mount_encrypted_volume-2"></a>

### try_mount_encrypted_volume/2 * ###

`try_mount_encrypted_volume(AESKey, Opts) -> any()`

Attempts to mount an encrypted volume using the green zone AES key.

This function handles the complete process of secure storage setup by
delegating to the dev_volume module, which provides a unified interface
for volume management.

The encryption key used for the volume is the same AES key used for green zone
communication, ensuring that only nodes in the green zone can access the data.

<a name="validate_join-3"></a>

### validate_join/3 * ###

<pre><code>
validate_join(M1::term(), Req::map(), Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`M1`: Ignored parameter<br />`Req`: The join request containing commitment report and public key<br />`Opts`: A map of configuration options<br />

returns: `{ok, Map}` on success with encrypted AES key, or
`{error, Binary}` on failure with error message

Validates an incoming join request from another node.

This function handles the server-side join flow when receiving a connection
request:
1. Validates the peer's configuration meets required standards
2. Extracts the commitment report and public key from the request
3. Verifies the hardware-backed commitment report
4. Adds the joining node to the trusted nodes list
5. Encrypts the shared AES key with the peer's public key
6. Returns the encrypted key to the requesting node

<a name="validate_peer_opts-2"></a>

### validate_peer_opts/2 * ###

<pre><code>
validate_peer_opts(Req::map(), Opts::map()) -&gt; boolean()
</code></pre>
<br />

`Req`: The request message containing the peer's configuration<br />`Opts`: A map of the local node's configuration options<br />

returns: true if the peer's configuration is valid, false otherwise

Validates that a peer's configuration matches required options.

This function ensures the peer node meets configuration requirements:
1. Retrieves the local node's required configuration
2. Gets the peer's options from its message
3. Adds required configuration to peer's required options list
4. Verifies the peer's node history is valid
5. Checks that the peer's options match the required configuration

