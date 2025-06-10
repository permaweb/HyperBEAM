# [Module dev_local_name.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_local_name.erl)




A device for registering and looking up local names.

<a name="description"></a>

## Description ##
This device uses
the node message to store a local cache of its known names, and the typical
non-volatile storage of the node message to store the names long-term.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#default_lookup-4">default_lookup/4*</a></td><td>Handle all other requests by delegating to the lookup function.</td></tr><tr><td valign="top"><a href="#direct_register-2">direct_register/2</a></td><td>Register a name without checking if the caller is an operator.</td></tr><tr><td valign="top"><a href="#find_names-1">find_names/1*</a></td><td>Returns a message containing all known names.</td></tr><tr><td valign="top"><a href="#generate_test_opts-0">generate_test_opts/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_test-0">http_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Export only the <code>lookup</code> and <code>register</code> functions.</td></tr><tr><td valign="top"><a href="#load_names-1">load_names/1*</a></td><td>Loads all known names from the cache and returns the new <code>node message</code>
with those names loaded into it.</td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3</a></td><td>Takes a <code>key</code> argument and returns the value of the name, if it exists.</td></tr><tr><td valign="top"><a href="#lookup_opts_name_test-0">lookup_opts_name_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#no_names_test-0">no_names_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#register-3">register/3</a></td><td>Takes a <code>key</code> and <code>value</code> argument and registers the name.</td></tr><tr><td valign="top"><a href="#register_test-0">register_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#unauthorized_test-0">unauthorized_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#update_names-2">update_names/2*</a></td><td>Updates the node message with the new names.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="default_lookup-4"></a>

### default_lookup/4 * ###

`default_lookup(Key, X2, Req, Opts) -> any()`

Handle all other requests by delegating to the lookup function.

<a name="direct_register-2"></a>

### direct_register/2 ###

`direct_register(Req, Opts) -> any()`

Register a name without checking if the caller is an operator. Exported
for use by other devices, but not publicly available.

<a name="find_names-1"></a>

### find_names/1 * ###

`find_names(Opts) -> any()`

Returns a message containing all known names.

<a name="generate_test_opts-0"></a>

### generate_test_opts/0 * ###

`generate_test_opts() -> any()`

<a name="http_test-0"></a>

### http_test/0 * ###

`http_test() -> any()`

<a name="info-1"></a>

### info/1 ###

`info(Opts) -> any()`

Export only the `lookup` and `register` functions.

<a name="load_names-1"></a>

### load_names/1 * ###

`load_names(Opts) -> any()`

Loads all known names from the cache and returns the new `node message`
with those names loaded into it.

<a name="lookup-3"></a>

### lookup/3 ###

`lookup(X1, Req, Opts) -> any()`

Takes a `key` argument and returns the value of the name, if it exists.

<a name="lookup_opts_name_test-0"></a>

### lookup_opts_name_test/0 * ###

`lookup_opts_name_test() -> any()`

<a name="no_names_test-0"></a>

### no_names_test/0 * ###

`no_names_test() -> any()`

<a name="register-3"></a>

### register/3 ###

`register(X1, Req, Opts) -> any()`

Takes a `key` and `value` argument and registers the name. The caller
must be the node operator in order to register a name.

<a name="register_test-0"></a>

### register_test/0 * ###

`register_test() -> any()`

<a name="unauthorized_test-0"></a>

### unauthorized_test/0 * ###

`unauthorized_test() -> any()`

<a name="update_names-2"></a>

### update_names/2 * ###

`update_names(LocalNames, Opts) -> any()`

Updates the node message with the new names. Further HTTP requests will
use this new message, removing the need to look up the names from non-volatile
storage.

