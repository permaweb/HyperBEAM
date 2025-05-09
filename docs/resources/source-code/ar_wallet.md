# [Module ar_wallet.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/ar_wallet.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compress_ecdsa_pubkey-1">compress_ecdsa_pubkey/1*</a></td><td></td></tr><tr><td valign="top"><a href="#hash_address-1">hash_address/1*</a></td><td></td></tr><tr><td valign="top"><a href="#hmac-1">hmac/1</a></td><td></td></tr><tr><td valign="top"><a href="#hmac-2">hmac/2</a></td><td></td></tr><tr><td valign="top"><a href="#load_key-1">load_key/1</a></td><td>Read the keyfile for the key with the given address from disk.</td></tr><tr><td valign="top"><a href="#load_keyfile-1">load_keyfile/1</a></td><td>Extract the public and private key from a keyfile.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td></td></tr><tr><td valign="top"><a href="#new_keyfile-2">new_keyfile/2</a></td><td>Generate a new wallet public and private key, with a corresponding keyfile.</td></tr><tr><td valign="top"><a href="#sign-2">sign/2</a></td><td>Sign some data with a private key.</td></tr><tr><td valign="top"><a href="#sign-3">sign/3</a></td><td>sign some data, hashed using the provided DigestType.</td></tr><tr><td valign="top"><a href="#to_address-1">to_address/1</a></td><td>Generate an address from a public key.</td></tr><tr><td valign="top"><a href="#to_address-2">to_address/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_ecdsa_address-1">to_ecdsa_address/1*</a></td><td></td></tr><tr><td valign="top"><a href="#to_rsa_address-1">to_rsa_address/1*</a></td><td></td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td>Verify that a signature is correct.</td></tr><tr><td valign="top"><a href="#verify-4">verify/4</a></td><td></td></tr><tr><td valign="top"><a href="#wallet_filepath-1">wallet_filepath/1*</a></td><td></td></tr><tr><td valign="top"><a href="#wallet_filepath-3">wallet_filepath/3*</a></td><td></td></tr><tr><td valign="top"><a href="#wallet_filepath2-1">wallet_filepath2/1*</a></td><td></td></tr><tr><td valign="top"><a href="#wallet_name-3">wallet_name/3*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compress_ecdsa_pubkey-1"></a>

### compress_ecdsa_pubkey/1 * ###

`compress_ecdsa_pubkey(X1) -> any()`

<a name="hash_address-1"></a>

### hash_address/1 * ###

`hash_address(PubKey) -> any()`

<a name="hmac-1"></a>

### hmac/1 ###

`hmac(Data) -> any()`

<a name="hmac-2"></a>

### hmac/2 ###

`hmac(Data, DigestType) -> any()`

<a name="load_key-1"></a>

### load_key/1 ###

`load_key(Addr) -> any()`

Read the keyfile for the key with the given address from disk.
Return not_found if arweave_keyfile_[addr].json or [addr].json is not found
in [data_dir]/?WALLET_DIR.

<a name="load_keyfile-1"></a>

### load_keyfile/1 ###

`load_keyfile(File) -> any()`

Extract the public and private key from a keyfile.

<a name="new-0"></a>

### new/0 ###

`new() -> any()`

<a name="new-1"></a>

### new/1 ###

`new(KeyType) -> any()`

<a name="new_keyfile-2"></a>

### new_keyfile/2 ###

`new_keyfile(KeyType, WalletName) -> any()`

Generate a new wallet public and private key, with a corresponding keyfile.
The provided key is used as part of the file name.

<a name="sign-2"></a>

### sign/2 ###

`sign(Key, Data) -> any()`

Sign some data with a private key.

<a name="sign-3"></a>

### sign/3 ###

`sign(X1, Data, DigestType) -> any()`

sign some data, hashed using the provided DigestType.

<a name="to_address-1"></a>

### to_address/1 ###

`to_address(Pubkey) -> any()`

Generate an address from a public key.

<a name="to_address-2"></a>

### to_address/2 ###

`to_address(PubKey, X2) -> any()`

<a name="to_ecdsa_address-1"></a>

### to_ecdsa_address/1 * ###

`to_ecdsa_address(PubKey) -> any()`

<a name="to_rsa_address-1"></a>

### to_rsa_address/1 * ###

`to_rsa_address(PubKey) -> any()`

<a name="verify-3"></a>

### verify/3 ###

`verify(Key, Data, Sig) -> any()`

Verify that a signature is correct.

<a name="verify-4"></a>

### verify/4 ###

`verify(X1, Data, Sig, DigestType) -> any()`

<a name="wallet_filepath-1"></a>

### wallet_filepath/1 * ###

`wallet_filepath(Wallet) -> any()`

<a name="wallet_filepath-3"></a>

### wallet_filepath/3 * ###

`wallet_filepath(WalletName, PubKey, KeyType) -> any()`

<a name="wallet_filepath2-1"></a>

### wallet_filepath2/1 * ###

`wallet_filepath2(Wallet) -> any()`

<a name="wallet_name-3"></a>

### wallet_name/3 * ###

`wallet_name(WalletName, PubKey, KeyType) -> any()`

