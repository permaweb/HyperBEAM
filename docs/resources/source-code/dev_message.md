# [Module dev_message.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_message.erl)




The identity device: For non-reserved keys, it simply returns a key
from the message as it is found in the message's underlying Erlang map.

<a name="description"></a>

## Description ##
Private keys (`priv[.*]`) are not included.
Reserved keys are: `id`, `commitments`, `committers`, `keys`, `path`,
`set`, `remove`, `get`, and `verify`. Their function comments describe the
behaviour of the device when these keys are set.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#calculate_id-3">calculate_id/3*</a></td><td></td></tr><tr><td valign="top"><a href="#cannot_get_private_keys_test-0">cannot_get_private_keys_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#case_insensitive_get-3">case_insensitive_get/3*</a></td><td>Key matching should be case insensitive, following RFC-9110, so we
implement a case-insensitive key lookup rather than delegating to
<code>hb_maps:get/2</code>.</td></tr><tr><td valign="top"><a href="#case_insensitive_get_test-0">case_insensitive_get_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#commit-3">commit/3</a></td><td>Commit to a message, using the <code>commitment-device</code> key to specify the
device that should be used to commit to the message.</td></tr><tr><td valign="top"><a href="#commitment_ids_from_committers-3">commitment_ids_from_committers/3*</a></td><td>Returns a list of commitment IDs in a commitments map that are relevant
for a list of given committer addresses.</td></tr><tr><td valign="top"><a href="#commitment_ids_from_request-3">commitment_ids_from_request/3*</a></td><td>Implements a standardized form of specifying commitment IDs for a
message request.</td></tr><tr><td valign="top"><a href="#committed-3">committed/3</a></td><td>Return the list of committed keys from a message.</td></tr><tr><td valign="top"><a href="#committers-1">committers/1</a></td><td>Return the committers of a message that are present in the given request.</td></tr><tr><td valign="top"><a href="#committers-2">committers/2</a></td><td></td></tr><tr><td valign="top"><a href="#committers-3">committers/3</a></td><td></td></tr><tr><td valign="top"><a href="#deep_unset_test-0">deep_unset_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_commitments_loaded-2">ensure_commitments_loaded/2*</a></td><td>Ensure that the <code>commitments</code> submessage of a base message is fully
loaded into local memory.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>Return the value associated with the key as it exists in the message's
underlying Erlang map.</td></tr><tr><td valign="top"><a href="#get-4">get/4</a></td><td></td></tr><tr><td valign="top"><a href="#get_keys_mod_test-0">get_keys_mod_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Return the ID of a message, using the <code>committers</code> list if it exists.</td></tr><tr><td valign="top"><a href="#id-2">id/2</a></td><td></td></tr><tr><td valign="top"><a href="#id-3">id/3</a></td><td></td></tr><tr><td valign="top"><a href="#id_device-2">id_device/2*</a></td><td>Locate the ID device of a message.</td></tr><tr><td valign="top"><a href="#index-3">index/3</a></td><td>Generate an index page for a message, in the event that the <code>body</code> and
<code>content-type</code> of a message returned to the client are both empty.</td></tr><tr><td valign="top"><a href="#info-0">info/0</a></td><td>Return the info for the identity device.</td></tr><tr><td valign="top"><a href="#is_private_mod_test-0">is_private_mod_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#key_from_device_test-0">key_from_device_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#keys-1">keys/1</a></td><td>Get the public keys of a message.</td></tr><tr><td valign="top"><a href="#keys-2">keys/2</a></td><td></td></tr><tr><td valign="top"><a href="#keys_from_device_test-0">keys_from_device_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#private_keys_are_filtered_test-0">private_keys_are_filtered_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#remove-2">remove/2</a></td><td>Remove a key or keys from a message.</td></tr><tr><td valign="top"><a href="#remove-3">remove/3</a></td><td></td></tr><tr><td valign="top"><a href="#remove_test-0">remove_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Deep merge keys in a message.</td></tr><tr><td valign="top"><a href="#set_conflicting_keys_test-0">set_conflicting_keys_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#set_ignore_undefined_test-0">set_ignore_undefined_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#set_path-3">set_path/3</a></td><td>Special case of <code>set/3</code> for setting the <code>path</code> key.</td></tr><tr><td valign="top"><a href="#unset_with_set_test-0">unset_with_set_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td>Verify a message.</td></tr><tr><td valign="top"><a href="#verify_commitment-3">verify_commitment/3*</a></td><td>Execute a function for a single commitment in the context of its
parent message.</td></tr><tr><td valign="top"><a href="#verify_test-0">verify_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#with_relevant_commitments-3">with_relevant_commitments/3*</a></td><td>Return a message with only the relevant commitments for a given request.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="calculate_id-3"></a>

### calculate_id/3 * ###

`calculate_id(Base, Req, NodeOpts) -> any()`

<a name="cannot_get_private_keys_test-0"></a>

### cannot_get_private_keys_test/0 * ###

`cannot_get_private_keys_test() -> any()`

<a name="case_insensitive_get-3"></a>

### case_insensitive_get/3 * ###

`case_insensitive_get(Key, Msg, Opts) -> any()`

Key matching should be case insensitive, following RFC-9110, so we
implement a case-insensitive key lookup rather than delegating to
`hb_maps:get/2`. Encode the key to a binary if it is not already.

<a name="case_insensitive_get_test-0"></a>

### case_insensitive_get_test/0 * ###

`case_insensitive_get_test() -> any()`

<a name="commit-3"></a>

### commit/3 ###

`commit(Self, Req, Opts) -> any()`

Commit to a message, using the `commitment-device` key to specify the
device that should be used to commit to the message. If the key is not set,
the default device (`httpsig@1.0`) is used.

<a name="commitment_ids_from_committers-3"></a>

### commitment_ids_from_committers/3 * ###

`commitment_ids_from_committers(CommitterAddrs, Commitments, Opts) -> any()`

Returns a list of commitment IDs in a commitments map that are relevant
for a list of given committer addresses.

<a name="commitment_ids_from_request-3"></a>

### commitment_ids_from_request/3 * ###

`commitment_ids_from_request(Base, Req, Opts) -> any()`

Implements a standardized form of specifying commitment IDs for a
message request. The caller may specify a list of committers (by address)
or a list of commitment IDs directly. They may specify both, in which case
the returned list will be the union of the two lists. In each case, they
may specify `all` or `none` for each group. If no specifiers are provided,
the default is `all` for commitments -- also implying `all` for committers.

<a name="committed-3"></a>

### committed/3 ###

`committed(Self, Req, Opts) -> any()`

Return the list of committed keys from a message.

<a name="committers-1"></a>

### committers/1 ###

`committers(Base) -> any()`

Return the committers of a message that are present in the given request.

<a name="committers-2"></a>

### committers/2 ###

`committers(Base, Req) -> any()`

<a name="committers-3"></a>

### committers/3 ###

`committers(X1, X2, NodeOpts) -> any()`

<a name="deep_unset_test-0"></a>

### deep_unset_test/0 * ###

`deep_unset_test() -> any()`

<a name="ensure_commitments_loaded-2"></a>

### ensure_commitments_loaded/2 * ###

`ensure_commitments_loaded(NonRelevant, Opts) -> any()`

Ensure that the `commitments` submessage of a base message is fully
loaded into local memory.

<a name="get-3"></a>

### get/3 ###

`get(Key, Msg, Opts) -> any()`

Return the value associated with the key as it exists in the message's
underlying Erlang map. First check the public keys, then check case-
insensitively if the key is a binary.

<a name="get-4"></a>

### get/4 ###

`get(Key, Msg, Msg2, Opts) -> any()`

<a name="get_keys_mod_test-0"></a>

### get_keys_mod_test/0 * ###

`get_keys_mod_test() -> any()`

<a name="id-1"></a>

### id/1 ###

`id(Base) -> any()`

Return the ID of a message, using the `committers` list if it exists.
If the `committers` key is `all`, return the ID including all known
commitments -- `none` yields the ID without any commitments. If the
`committers` key is a list/map, return the ID including only the specified
commitments.

The `id-device` key in the message can be used to specify the device that
should be used to calculate the ID. If it is not set, the default device
(`httpsig@1.0`) is used.

Note: This function _does not_ use AO-Core's `get/3` function, as it
as it would require significant computation. We may want to change this
if/when non-map message structures are created.

<a name="id-2"></a>

### id/2 ###

`id(Base, Req) -> any()`

<a name="id-3"></a>

### id/3 ###

`id(Base, Req, NodeOpts) -> any()`

<a name="id_device-2"></a>

### id_device/2 * ###

`id_device(X1, Opts) -> any()`

Locate the ID device of a message. The ID device is determined the
`device` set in _all_ of the commitments. If no commitments are present,
the default device (`httpsig@1.0`) is used.

<a name="index-3"></a>

### index/3 ###

`index(Msg, Req, Opts) -> any()`

Generate an index page for a message, in the event that the `body` and
`content-type` of a message returned to the client are both empty. We do this
as follows:
1. Find the `default_index` key of the node message. If it is a binary,
it is assumed to be the name of a device, and we execute the resolution
`as` that ID.
2. Merge the base message with the default index message, favoring the default
index message's keys over those in the base message, unless the default
was a device name.
3. Execute the `default_index_path` (base: `index`) upon the message,
giving the rest of the request unchanged.

<a name="info-0"></a>

### info/0 ###

`info() -> any()`

Return the info for the identity device.

<a name="is_private_mod_test-0"></a>

### is_private_mod_test/0 * ###

`is_private_mod_test() -> any()`

<a name="key_from_device_test-0"></a>

### key_from_device_test/0 * ###

`key_from_device_test() -> any()`

<a name="keys-1"></a>

### keys/1 ###

`keys(Msg) -> any()`

Get the public keys of a message.

<a name="keys-2"></a>

### keys/2 ###

`keys(Msg, Opts) -> any()`

<a name="keys_from_device_test-0"></a>

### keys_from_device_test/0 * ###

`keys_from_device_test() -> any()`

<a name="private_keys_are_filtered_test-0"></a>

### private_keys_are_filtered_test/0 * ###

`private_keys_are_filtered_test() -> any()`

<a name="remove-2"></a>

### remove/2 ###

`remove(Message1, Key) -> any()`

Remove a key or keys from a message.

<a name="remove-3"></a>

### remove/3 ###

`remove(Message1, X2, Opts) -> any()`

<a name="remove_test-0"></a>

### remove_test/0 * ###

`remove_test() -> any()`

<a name="set-3"></a>

### set/3 ###

`set(Message1, NewValuesMsg, Opts) -> any()`

Deep merge keys in a message. Takes a map of key-value pairs and sets
them in the message, overwriting any existing values.

<a name="set_conflicting_keys_test-0"></a>

### set_conflicting_keys_test/0 * ###

`set_conflicting_keys_test() -> any()`

<a name="set_ignore_undefined_test-0"></a>

### set_ignore_undefined_test/0 * ###

`set_ignore_undefined_test() -> any()`

<a name="set_path-3"></a>

### set_path/3 ###

`set_path(Message1, X2, Opts) -> any()`

Special case of `set/3` for setting the `path` key. This cannot be set
using the normal `set` function, as the `path` is a reserved key, necessary
for AO-Core to know the key to evaluate in requests.

<a name="unset_with_set_test-0"></a>

### unset_with_set_test/0 * ###

`unset_with_set_test() -> any()`

<a name="verify-3"></a>

### verify/3 ###

`verify(Self, Req, Opts) -> any()`

Verify a message. By default, all commitments are verified. The
`committers` key in the request can be used to specify that only the
commitments from specific committers should be verified. Similarly, specific
commitments can be specified using the `commitments` key.

<a name="verify_commitment-3"></a>

### verify_commitment/3 * ###

`verify_commitment(Base, Commitment, Opts) -> any()`

Execute a function for a single commitment in the context of its
parent message.
Note: Assumes that the `commitments` key has already been removed from the
message if applicable.

<a name="verify_test-0"></a>

### verify_test/0 * ###

`verify_test() -> any()`

<a name="with_relevant_commitments-3"></a>

### with_relevant_commitments/3 * ###

`with_relevant_commitments(Base, Req, Opts) -> any()`

Return a message with only the relevant commitments for a given request.
See `commitment_ids_from_request/3` for more information on the request format.

