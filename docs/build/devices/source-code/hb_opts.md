# [Module hb_opts.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_opts.erl)




A module for interacting with local and global options inside
HyperBEAM.

<a name="description"></a>

## Description ##

Options are set globally, but can also be overridden using an
an optional local `Opts` map argument. Many functions across the HyperBEAM
environment accept an `Opts` argument, which can be used to customize
behavior.

Options set in an `Opts` map must _never_ change the behavior of a function
that should otherwise be deterministic. Doing so may lead to loss of funds
by the HyperBEAM node operator, as the results of their executions will be
different than those of other node operators. If they are economically
staked on the correctness of these results, they may experience punishments
for non-verifiable behavior. Instead, if a local node setting makes
deterministic behavior impossible, the caller should fail the execution
with a refusal to execute.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#as-2">as/2</a></td><td>Find a given identity from the <code>identities</code> map, and return the options
merged with the sub-options for that identity.</td></tr><tr><td valign="top"><a href="#cached_os_env-2">cached_os_env/2*</a></td><td>Cache the result of os:getenv/1 in the process dictionary, as it never
changes during the lifetime of a node.</td></tr><tr><td valign="top"><a href="#check_required_opts-2">check_required_opts/2</a></td><td>Utility function to check for required options in a list.</td></tr><tr><td valign="top"><a href="#config_lookup-3">config_lookup/3*</a></td><td>An abstraction for looking up configuration variables.</td></tr><tr><td valign="top"><a href="#default_message-0">default_message/0</a></td><td>The default configuration options of the hyperbeam node.</td></tr><tr><td valign="top"><a href="#ensure_node_history-2">ensure_node_history/2</a></td><td>Ensures all items in a node history meet required configuration options.</td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td>Get an option from the global options, optionally overriding with a
local <code>Opts</code> map if <code>prefer</code> or <code>only</code> is set to <code>local</code>.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#global_get-3">global_get/3*</a></td><td>Get an environment variable or configuration key.</td></tr><tr><td valign="top"><a href="#identities-1">identities/1</a></td><td>Find all known IDs and their sub-options from the <code>priv_ids</code> map.</td></tr><tr><td valign="top"><a href="#identities-2">identities/2*</a></td><td></td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td>Parse a <code>flat@1.0</code> encoded file into a map, matching the types of the
keys to those in the default message.</td></tr><tr><td valign="top"><a href="#load-2">load/2</a></td><td></td></tr><tr><td valign="top"><a href="#load_bin-2">load_bin/2</a></td><td></td></tr><tr><td valign="top"><a href="#mimic_default_types-3">mimic_default_types/3</a></td><td>Mimic the types of the default message for a given map.</td></tr><tr><td valign="top"><a href="#normalize_default-1">normalize_default/1*</a></td><td>Get an option from environment variables, optionally consulting the
<code>hb_features</code> of the node if a conditional default tuple is provided.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="as-2"></a>

### as/2 ###

`as(Identity, Opts) -> any()`

Find a given identity from the `identities` map, and return the options
merged with the sub-options for that identity.

<a name="cached_os_env-2"></a>

### cached_os_env/2 * ###

`cached_os_env(Key, DefaultValue) -> any()`

Cache the result of os:getenv/1 in the process dictionary, as it never
changes during the lifetime of a node.

<a name="check_required_opts-2"></a>

### check_required_opts/2 ###

<pre><code>
check_required_opts(KeyValuePairs::[{binary(), term()}], Opts::map()) -&gt; {ok, map()} | {error, binary()}
</code></pre>
<br />

`KeyValuePairs`: A list of {Name, Value} pairs to check.<br />`Opts`: The original options map to return if validation succeeds.<br />

returns: `{ok, Opts}` if all required options are present, or
`{error, <<"Missing required parameters: ", MissingOptsStr/binary>>}`
where `MissingOptsStr` is a comma-separated list of missing option names.

Utility function to check for required options in a list.
Takes a list of {Name, Value} pairs and returns:
- {ok, Opts} when all required options are present (Value =/= not_found)
- {error, ErrorMsg} with a message listing all missing options when any are not_found

<a name="config_lookup-3"></a>

### config_lookup/3 * ###

`config_lookup(Key, Default, Opts) -> any()`

An abstraction for looking up configuration variables. In the future,
this is the function that we will want to change to support a more dynamic
configuration system.

<a name="default_message-0"></a>

### default_message/0 ###

`default_message() -> any()`

The default configuration options of the hyperbeam node.

<a name="ensure_node_history-2"></a>

### ensure_node_history/2 ###

<pre><code>
ensure_node_history(NodeHistory::list() | term(), RequiredOpts::map()) -&gt; {ok, binary()} | {error, binary()}
</code></pre>
<br />

`RequiredOpts`: A map of options that must be present and unchanging<br />

returns: 
`{ok, <<"valid">>}` when validation passes
`{error, <<"missing_keys">>}` when required keys are missing from first item
`{error, <<"invalid_values">>}` when first item values don't match requirements
`{error, <<"modified_required_key">>}` when history items modify required keys
`{error, <<"validation_failed">>}` when other validation errors occur

Ensures all items in a node history meet required configuration options.

This function verifies that the first item (complete opts) contains all required
configuration options and that their values match the expected format. Then it
validates that subsequent history items (which represent differences) never
modify any of the required keys from the first item.

Validation is performed in two steps:
1. Checks that the first item has all required keys and valid values
2. Verifies that subsequent items don't modify any required keys from the first item

<a name="get-1"></a>

### get/1 ###

`get(Key) -> any()`

Get an option from the global options, optionally overriding with a
local `Opts` map if `prefer` or `only` is set to `local`. If the `only`
option is provided in the `local` map, only keys found in the corresponding
(`local` or `global`) map will be returned. This function also offers users
a way to specify a default value to return if the option is not set.

`prefer` defaults to `local`.

<a name="get-2"></a>

### get/2 ###

`get(Key, Default) -> any()`

<a name="get-3"></a>

### get/3 ###

`get(Key, Default, Opts) -> any()`

<a name="global_get-3"></a>

### global_get/3 * ###

`global_get(Key, Default, Opts) -> any()`

Get an environment variable or configuration key.

<a name="identities-1"></a>

### identities/1 ###

`identities(Opts) -> any()`

Find all known IDs and their sub-options from the `priv_ids` map. Allows
the identities to be named, or based on addresses. The results are normalized
such that the map returned by this function contains both mechanisms for
finding an identity and its sub-options. Additionally, sub-options are also
normalized such that the `address` property is present and accurate for all
given identities.

<a name="identities-2"></a>

### identities/2 * ###

`identities(Default, Opts) -> any()`

<a name="load-1"></a>

### load/1 ###

`load(Path) -> any()`

Parse a `flat@1.0` encoded file into a map, matching the types of the
keys to those in the default message.

<a name="load-2"></a>

### load/2 ###

`load(Path, Opts) -> any()`

<a name="load_bin-2"></a>

### load_bin/2 ###

`load_bin(Bin, Opts) -> any()`

<a name="mimic_default_types-3"></a>

### mimic_default_types/3 ###

`mimic_default_types(Map, Mode, Opts) -> any()`

Mimic the types of the default message for a given map.

<a name="normalize_default-1"></a>

### normalize_default/1 * ###

`normalize_default(Default) -> any()`

Get an option from environment variables, optionally consulting the
`hb_features` of the node if a conditional default tuple is provided.

