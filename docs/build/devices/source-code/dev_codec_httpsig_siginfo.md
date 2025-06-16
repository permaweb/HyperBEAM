# [Module dev_codec_httpsig_siginfo.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_httpsig_siginfo.erl)




A module for converting between commitments and their encoded `signature`
and `signature-input` keys.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_derived_specifiers-1">add_derived_specifiers/1</a></td><td>Normalize key parameters to ensure their names are correct for inclusion
in the <code>signature-input</code> and associated keys.</td></tr><tr><td valign="top"><a href="#commitment_to_alg-2">commitment_to_alg/2*</a></td><td>Calculate an <code>alg</code> string from a commitment message, using its
<code>commitment-device</code> and optionally, its <code>type</code> key.</td></tr><tr><td valign="top"><a href="#commitment_to_device_specifiers-2">commitment_to_device_specifiers/2*</a></td><td>Convert an <code>alg</code> to a commitment device.</td></tr><tr><td valign="top"><a href="#commitment_to_sf_siginfo-3">commitment_to_sf_siginfo/3*</a></td><td>Generate a <code>signature</code> and <code>signature-input</code> key pair from a given
commitment.</td></tr><tr><td valign="top"><a href="#commitment_to_sig_name-1">commitment_to_sig_name/1</a></td><td>Generate a signature name from a commitment.</td></tr><tr><td valign="top"><a href="#commitments_to_siginfo-3">commitments_to_siginfo/3</a></td><td>Generate a <code>signature</code> and <code>signature-input</code> key pair from a commitment
map.</td></tr><tr><td valign="top"><a href="#committed_keys_to_siginfo-1">committed_keys_to_siginfo/1</a></td><td>Convert committed keys to their siginfo format.</td></tr><tr><td valign="top"><a href="#decoding_nested_map_binary-1">decoding_nested_map_binary/1*</a></td><td></td></tr><tr><td valign="top"><a href="#from_siginfo_keys-3">from_siginfo_keys/3</a></td><td>Normalize a list of <code>httpsig@1.0</code> keys to their equivalents in AO-Core
format.</td></tr><tr><td valign="top"><a href="#get_additional_params-1">get_additional_params/1*</a></td><td></td></tr><tr><td valign="top"><a href="#nested_map_to_string-1">nested_map_to_string/1*</a></td><td></td></tr><tr><td valign="top"><a href="#remove_derived_specifiers-1">remove_derived_specifiers/1</a></td><td>Remove derived specifiers from a list of component identifiers.</td></tr><tr><td valign="top"><a href="#sf_siginfo_to_commitment-5">sf_siginfo_to_commitment/5*</a></td><td>Take a signature and signature-input as parsed structured-fields and
return a commitment.</td></tr><tr><td valign="top"><a href="#siginfo_to_commitments-3">siginfo_to_commitments/3</a></td><td>Take a message with a <code>signature</code> and <code>signature-input</code> key pair and
return a map of commitments.</td></tr><tr><td valign="top"><a href="#to_siginfo_keys-3">to_siginfo_keys/3</a></td><td>Normalize a list of AO-Core keys to their equivalents in <code>httpsig@1.0</code>
format.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_derived_specifiers-1"></a>

### add_derived_specifiers/1 ###

`add_derived_specifiers(ComponentIdentifiers) -> any()`

Normalize key parameters to ensure their names are correct for inclusion
in the `signature-input` and associated keys.

<a name="commitment_to_alg-2"></a>

### commitment_to_alg/2 * ###

`commitment_to_alg(Commitment, Opts) -> any()`

Calculate an `alg` string from a commitment message, using its
`commitment-device` and optionally, its `type` key.

<a name="commitment_to_device_specifiers-2"></a>

### commitment_to_device_specifiers/2 * ###

`commitment_to_device_specifiers(Commitment, Opts) -> any()`

Convert an `alg` to a commitment device. If the `alg` has the form of
a device specifier (`x@y.z...[/type]`), return the device. Otherwise, we
assume that the `alg` is a `type` of the `httpsig@1.0` algorithm.
`type` is an optional key that allows for subtyping of the algorithm. When
provided, in the `alg` it is parsed and returned as the `type` key in the
commitment message.

<a name="commitment_to_sf_siginfo-3"></a>

### commitment_to_sf_siginfo/3 * ###

`commitment_to_sf_siginfo(Msg, Commitment, Opts) -> any()`

Generate a `signature` and `signature-input` key pair from a given
commitment.

<a name="commitment_to_sig_name-1"></a>

### commitment_to_sig_name/1 ###

`commitment_to_sig_name(Commitment) -> any()`

Generate a signature name from a commitment. The commitment message is
not expected to be complete: Only the `commitment-device`, and the
`committer` or `keyid` keys are required.

<a name="commitments_to_siginfo-3"></a>

### commitments_to_siginfo/3 ###

`commitments_to_siginfo(Msg, Comms, Opts) -> any()`

Generate a `signature` and `signature-input` key pair from a commitment
map.

<a name="committed_keys_to_siginfo-1"></a>

### committed_keys_to_siginfo/1 ###

`committed_keys_to_siginfo(Msg) -> any()`

Convert committed keys to their siginfo format. This involves removing
the `body` key from the committed keys, if present, and replacing it with
the `content-digest` key.

<a name="decoding_nested_map_binary-1"></a>

### decoding_nested_map_binary/1 * ###

`decoding_nested_map_binary(Bin) -> any()`

<a name="from_siginfo_keys-3"></a>

### from_siginfo_keys/3 ###

`from_siginfo_keys(HTTPEncMsg, BodyKeys, SigInfoCommitted) -> any()`

Normalize a list of `httpsig@1.0` keys to their equivalents in AO-Core
format. There are three stages:
1. Remove the @ prefix from the component identifiers, if present.
2. Replace `content-digest` with the body keys, if present.
3. Replace the `body` key again with the value of the `ao-body-key` key, if
present. This is possible because the keys derived from the body often
contain the `body` key itself.
4. If the `content-type` starts with `multipart/`, we remove it.

<a name="get_additional_params-1"></a>

### get_additional_params/1 * ###

`get_additional_params(Commitment) -> any()`

<a name="nested_map_to_string-1"></a>

### nested_map_to_string/1 * ###

`nested_map_to_string(Map) -> any()`

<a name="remove_derived_specifiers-1"></a>

### remove_derived_specifiers/1 ###

`remove_derived_specifiers(ComponentIdentifiers) -> any()`

Remove derived specifiers from a list of component identifiers.

<a name="sf_siginfo_to_commitment-5"></a>

### sf_siginfo_to_commitment/5 * ###

`sf_siginfo_to_commitment(Msg, BodyKeys, SFSig, SFSigInput, Opts) -> any()`

Take a signature and signature-input as parsed structured-fields and
return a commitment.

<a name="siginfo_to_commitments-3"></a>

### siginfo_to_commitments/3 ###

`siginfo_to_commitments(Msg, BodyKeys, Opts) -> any()`

Take a message with a `signature` and `signature-input` key pair and
return a map of commitments.

<a name="to_siginfo_keys-3"></a>

### to_siginfo_keys/3 ###

`to_siginfo_keys(Msg, Commitment, Opts) -> any()`

Normalize a list of AO-Core keys to their equivalents in `httpsig@1.0`
format. This involves:
- If the HTTPSig message given has an `ao-body-key` key and the committed keys
list contains it, we replace it in the list with the `body` key and add the
`ao-body-key` key.
- If the list contains a `body` key, we replace it with the `content-digest`
key.
- Otherwise, we return the list unchanged.

