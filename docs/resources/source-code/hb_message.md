# [Module hb_message.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_message.erl)




This module acts an adapter between messages, as modeled in the
AO-Core protocol, and their uderlying binary representations and formats.

<a name="description"></a>

## Description ##

Unless you are implementing a new message serialization codec, you should
not need to interact with this module directly. Instead, use the
`hb_ao` interfaces to interact with all messages. The `dev_message`
module implements a device interface for abstracting over the different
message formats.

`hb_message` and the HyperBEAM caches can interact with multiple different
types of message formats:

- Richly typed AO-Core structured messages.
- Arweave transations.
- ANS-104 data items.
- HTTP Signed Messages.
- Flat Maps.

This module is responsible for converting between these formats. It does so
by normalizing messages to a common format: `Type Annotated Binary Messages`
(TABM). TABMs are deep Erlang maps with keys than only contain either other
TABMs or binary values. By marshalling all messages into this format, they
can easily be coerced into other output formats. For example, generating a
`HTTP Signed Message` format output from an Arweave transaction. TABM is
also a simple format from a computational perspective (only binary literals
and O(1) access maps), such that operations upon them are efficient.

The structure of the conversions is as follows:

<pre>
Arweave TX/ANS-104 ==> dev_codec_ans104:from/1 ==> TABM
HTTP Signed Message ==> dev_codec_httpsig_conv:from/1 ==> TABM
Flat Maps ==> dev_codec_flat:from/1 ==> TABM

TABM ==> dev_codec_structured:to/1 ==> AO-Core Message
AO-Core Message ==> dev_codec_structured:from/1 ==> TABM

TABM ==> dev_codec_ans104:to/1 ==> Arweave TX/ANS-104
TABM ==> dev_codec_httpsig_conv:to/1 ==> HTTP Signed Message
TABM ==> dev_codec_flat:to/1 ==> Flat Maps
...
</pre>

Additionally, this module provides a number of utility functions for
manipulating messages. For example, `hb_message:sign/2` to sign a message of
arbitrary type, or `hb_message:format/1` to print an AO-Core/TABM message in
a human-readable format.

The `hb_cache` module is responsible for storing and retrieving messages in
the HyperBEAM stores configured on the node. Each store has its own storage
backend, but each works with simple key-value pairs. Subsequently, the
`hb_cache` module uses TABMs as the internal format for storing and
retrieving messages.

Test vectors to ensure the functioning of this module and the codecs that
interact with it are found in `hb_message_test_vectors.erl`.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#commit-2">commit/2</a></td><td>Sign a message with the given wallet.</td></tr><tr><td valign="top"><a href="#commit-3">commit/3</a></td><td></td></tr><tr><td valign="top"><a href="#commitment-2">commitment/2</a></td><td>Extract a commitment from a message given a <code>committer</code> ID, or a spec
message to match against.</td></tr><tr><td valign="top"><a href="#commitment-3">commitment/3</a></td><td></td></tr><tr><td valign="top"><a href="#commitment_devices-2">commitment_devices/2</a></td><td>Return the devices for which there are commitments on a message.</td></tr><tr><td valign="top"><a href="#committed-3">committed/3</a></td><td>Return the list of committed keys from a message.</td></tr><tr><td valign="top"><a href="#conversion_spec_to_req-2">conversion_spec_to_req/2*</a></td><td>Get a codec device and request params from the given conversion request.</td></tr><tr><td valign="top"><a href="#convert-3">convert/3</a></td><td>Convert a message from one format to another.</td></tr><tr><td valign="top"><a href="#convert-4">convert/4</a></td><td></td></tr><tr><td valign="top"><a href="#default_tx_list-0">default_tx_list/0</a></td><td>Get the ordered list of fields as AO-Core keys and default values of
the tx record.</td></tr><tr><td valign="top"><a href="#default_tx_message-0">default_tx_message/0*</a></td><td>Get the normalized fields and default values of the tx record.</td></tr><tr><td valign="top"><a href="#filter_default_keys-1">filter_default_keys/1</a></td><td>Remove keys from a map that have the default values found in the tx
record.</td></tr><tr><td valign="top"><a href="#find_target-3">find_target/3</a></td><td>Implements a standard pattern in which the target for an operation is
found by looking for a <code>target</code> key in the request.</td></tr><tr><td valign="top"><a href="#format-1">format/1</a></td><td>Format a message for printing, optionally taking an indentation level
to start from.</td></tr><tr><td valign="top"><a href="#format-2">format/2</a></td><td></td></tr><tr><td valign="top"><a href="#from_tabm-4">from_tabm/4*</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Return the ID of a message.</td></tr><tr><td valign="top"><a href="#id-2">id/2</a></td><td></td></tr><tr><td valign="top"><a href="#id-3">id/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_signed_key-3">is_signed_key/3</a></td><td>Determine whether a specific key is part of a message's commitments.</td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td>Check if two maps match, including recursively checking nested maps.</td></tr><tr><td valign="top"><a href="#match-3">match/3</a></td><td></td></tr><tr><td valign="top"><a href="#match-4">match/4</a></td><td></td></tr><tr><td valign="top"><a href="#matchable_keys-1">matchable_keys/1*</a></td><td></td></tr><tr><td valign="top"><a href="#minimize-1">minimize/1</a></td><td>Remove keys from the map that can be regenerated.</td></tr><tr><td valign="top"><a href="#minimize-2">minimize/2*</a></td><td></td></tr><tr><td valign="top"><a href="#normalize-2">normalize/2*</a></td><td>Return a map with only the keys that necessary, without those that can
be regenerated.</td></tr><tr><td valign="top"><a href="#print-1">print/1</a></td><td>Pretty-print a message.</td></tr><tr><td valign="top"><a href="#print-2">print/2*</a></td><td></td></tr><tr><td valign="top"><a href="#restore_priv-3">restore_priv/3*</a></td><td>Add the existing <code>priv</code> sub-map back to a converted message, honoring
any existing <code>priv</code> sub-map that may already be present.</td></tr><tr><td valign="top"><a href="#signers-2">signers/2</a></td><td>Return all of the committers on a message that have 'normal', 256 bit,
addresses.</td></tr><tr><td valign="top"><a href="#to_tabm-3">to_tabm/3*</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>Return the type of an encoded message.</td></tr><tr><td valign="top"><a href="#uncommitted-1">uncommitted/1</a></td><td>Return the unsigned version of a message in AO-Core format.</td></tr><tr><td valign="top"><a href="#uncommitted-2">uncommitted/2</a></td><td></td></tr><tr><td valign="top"><a href="#unsafe_match-5">unsafe_match/5*</a></td><td></td></tr><tr><td valign="top"><a href="#verify-1">verify/1</a></td><td>wrapper function to verify a message.</td></tr><tr><td valign="top"><a href="#verify-2">verify/2</a></td><td></td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td></td></tr><tr><td valign="top"><a href="#with_commitments-3">with_commitments/3</a></td><td>Filter messages that do not match the 'spec' given.</td></tr><tr><td valign="top"><a href="#with_only_committed-2">with_only_committed/2</a></td><td>Return a message with only the committed keys.</td></tr><tr><td valign="top"><a href="#with_only_committers-2">with_only_committers/2</a></td><td>Return the message with only the specified committers attached.</td></tr><tr><td valign="top"><a href="#with_only_committers-3">with_only_committers/3</a></td><td></td></tr><tr><td valign="top"><a href="#without_commitments-3">without_commitments/3</a></td><td>Filter messages that match the 'spec' given.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="commit-2"></a>

### commit/2 ###

`commit(Msg, WalletOrOpts) -> any()`

Sign a message with the given wallet.

<a name="commit-3"></a>

### commit/3 ###

`commit(Msg, Wallet, Format) -> any()`

<a name="commitment-2"></a>

### commitment/2 ###

`commitment(Committer, Msg) -> any()`

Extract a commitment from a message given a `committer` ID, or a spec
message to match against. Returns only the first matching commitment, or
`not_found`.

<a name="commitment-3"></a>

### commitment/3 ###

`commitment(CommitterID, Msg, Opts) -> any()`

<a name="commitment_devices-2"></a>

### commitment_devices/2 ###

`commitment_devices(Msg, Opts) -> any()`

Return the devices for which there are commitments on a message.

<a name="committed-3"></a>

### committed/3 ###

`committed(Msg, List, Opts) -> any()`

Return the list of committed keys from a message.

<a name="conversion_spec_to_req-2"></a>

### conversion_spec_to_req/2 * ###

`conversion_spec_to_req(Spec, Opts) -> any()`

Get a codec device and request params from the given conversion request.
Expects conversion spec to either be a binary codec name, or a map with a
`device` key and other parameters. Additionally honors the `always_bundle`
key in the node message if present.

<a name="convert-3"></a>

### convert/3 ###

`convert(Msg, TargetFormat, Opts) -> any()`

Convert a message from one format to another. Taking a message in the
source format, a target format, and a set of opts. If not given, the source
is assumed to be `structured@1.0`. Additional codecs can be added by ensuring they
are part of the `Opts` map -- either globally, or locally for a computation.

The encoding happens in two phases:
1. Convert the message to a TABM.
2. Convert the TABM to the target format.

The conversion to a TABM is done by the `structured@1.0` codec, which is always
available. The conversion from a TABM is done by the target codec.

<a name="convert-4"></a>

### convert/4 ###

`convert(Msg, TargetFormat, SourceFormat, Opts) -> any()`

<a name="default_tx_list-0"></a>

### default_tx_list/0 ###

`default_tx_list() -> any()`

Get the ordered list of fields as AO-Core keys and default values of
the tx record.

<a name="default_tx_message-0"></a>

### default_tx_message/0 * ###

`default_tx_message() -> any()`

Get the normalized fields and default values of the tx record.

<a name="filter_default_keys-1"></a>

### filter_default_keys/1 ###

`filter_default_keys(Map) -> any()`

Remove keys from a map that have the default values found in the tx
record.

<a name="find_target-3"></a>

### find_target/3 ###

`find_target(Self, Req, Opts) -> any()`

Implements a standard pattern in which the target for an operation is
found by looking for a `target` key in the request. If the target is `self`,
or not present, the operation is performed on the original message. Otherwise,
the target is expected to be a key in the message, and the operation is
performed on the value of that key.

<a name="format-1"></a>

### format/1 ###

`format(Item) -> any()`

Format a message for printing, optionally taking an indentation level
to start from.

<a name="format-2"></a>

### format/2 ###

`format(Bin, Indent) -> any()`

<a name="from_tabm-4"></a>

### from_tabm/4 * ###

`from_tabm(Msg, TargetFormat, OldPriv, Opts) -> any()`

<a name="id-1"></a>

### id/1 ###

`id(Msg) -> any()`

Return the ID of a message.

<a name="id-2"></a>

### id/2 ###

`id(Msg, Opts) -> any()`

<a name="id-3"></a>

### id/3 ###

`id(Msg, RawCommitters, Opts) -> any()`

<a name="is_signed_key-3"></a>

### is_signed_key/3 ###

`is_signed_key(Key, Msg, Opts) -> any()`

Determine whether a specific key is part of a message's commitments.

<a name="match-2"></a>

### match/2 ###

`match(Map1, Map2) -> any()`

Check if two maps match, including recursively checking nested maps.
Takes an optional mode argument to control the matching behavior:
`strict`: All keys in both maps be present and match.
`only_present`: Only present keys in both maps must match.
`primary`: Only the primary map's keys must be present.
Returns `true` or `{ErrType, Err}`.

<a name="match-3"></a>

### match/3 ###

`match(Map1, Map2, Mode) -> any()`

<a name="match-4"></a>

### match/4 ###

`match(Map1, Map2, Mode, Opts) -> any()`

<a name="matchable_keys-1"></a>

### matchable_keys/1 * ###

`matchable_keys(Map) -> any()`

<a name="minimize-1"></a>

### minimize/1 ###

`minimize(Msg) -> any()`

Remove keys from the map that can be regenerated. Optionally takes an
additional list of keys to include in the minimization.

<a name="minimize-2"></a>

### minimize/2 * ###

`minimize(RawVal, ExtraKeys) -> any()`

<a name="normalize-2"></a>

### normalize/2 * ###

`normalize(Map, Opts) -> any()`

Return a map with only the keys that necessary, without those that can
be regenerated.

<a name="print-1"></a>

### print/1 ###

`print(Msg) -> any()`

Pretty-print a message.

<a name="print-2"></a>

### print/2 * ###

`print(Msg, Indent) -> any()`

<a name="restore_priv-3"></a>

### restore_priv/3 * ###

`restore_priv(Msg, EmptyPriv, Opts) -> any()`

Add the existing `priv` sub-map back to a converted message, honoring
any existing `priv` sub-map that may already be present.

<a name="signers-2"></a>

### signers/2 ###

`signers(Msg, Opts) -> any()`

Return all of the committers on a message that have 'normal', 256 bit,
addresses.

<a name="to_tabm-3"></a>

### to_tabm/3 * ###

`to_tabm(Msg, SourceFormat, Opts) -> any()`

<a name="type-1"></a>

### type/1 ###

`type(TX) -> any()`

Return the type of an encoded message.

<a name="uncommitted-1"></a>

### uncommitted/1 ###

`uncommitted(Msg) -> any()`

Return the unsigned version of a message in AO-Core format.

<a name="uncommitted-2"></a>

### uncommitted/2 ###

`uncommitted(Bin, Opts) -> any()`

<a name="unsafe_match-5"></a>

### unsafe_match/5 * ###

`unsafe_match(Map1, Map2, Mode, Path, Opts) -> any()`

<a name="verify-1"></a>

### verify/1 ###

`verify(Msg) -> any()`

wrapper function to verify a message.

<a name="verify-2"></a>

### verify/2 ###

`verify(Msg, Committers) -> any()`

<a name="verify-3"></a>

### verify/3 ###

`verify(Msg, Committers, Opts) -> any()`

<a name="with_commitments-3"></a>

### with_commitments/3 ###

`with_commitments(Spec, Msg, Opts) -> any()`

Filter messages that do not match the 'spec' given. The underlying match
is performed in the `only_present` mode, such that match specifications only
need to specify the keys that must be present.

<a name="with_only_committed-2"></a>

### with_only_committed/2 ###

`with_only_committed(Msg, Opts) -> any()`

Return a message with only the committed keys. If no commitments are
present, the message is returned unchanged. This means that you need to
check if the message is:
- Committed
- Verifies
...before using the output of this function as the 'canonical' message. This
is such that expensive operations like signature verification are not
performed unless necessary.

<a name="with_only_committers-2"></a>

### with_only_committers/2 ###

`with_only_committers(Msg, Committers) -> any()`

Return the message with only the specified committers attached.

<a name="with_only_committers-3"></a>

### with_only_committers/3 ###

`with_only_committers(Msg, Committers, Opts) -> any()`

<a name="without_commitments-3"></a>

### without_commitments/3 ###

`without_commitments(Spec, Msg, Opts) -> any()`

Filter messages that match the 'spec' given. Inverts the `with_commitments/2`
function, such that only messages that do _not_ match the spec are returned.

