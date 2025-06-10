# [Module hb_cache_control.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_cache_control.erl)




Cache control logic for the AO-Core resolver.

<a name="description"></a>

## Description ##
It derives cache settings
from request, response, execution-local node Opts, as well as the global
node Opts. It applies these settings when asked to maybe store/lookup in
response to a request.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cache_binary_result_test-0">cache_binary_result_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#cache_message_result_test-0">cache_message_result_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#cache_source_to_cache_settings-1">cache_source_to_cache_settings/1*</a></td><td>Convert a cache source to a cache setting.</td></tr><tr><td valign="top"><a href="#derive_cache_settings-2">derive_cache_settings/2*</a></td><td>Derive cache settings from a series of option sources and the opts,
honoring precidence order.</td></tr><tr><td valign="top"><a href="#dispatch_cache_write-4">dispatch_cache_write/4*</a></td><td>Dispatch the cache write to a worker process if requested.</td></tr><tr><td valign="top"><a href="#empty_message_list_test-0">empty_message_list_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#exec_likely_faster_heuristic-3">exec_likely_faster_heuristic/3*</a></td><td>Determine whether we are likely to be faster looking up the result in
our cache (hoping we have it), or executing it directly.</td></tr><tr><td valign="top"><a href="#hashpath_ignore_prevents_storage_test-0">hashpath_ignore_prevents_storage_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#is_explicit_lookup-3">is_explicit_lookup/3*</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3*</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_lookup-3">maybe_lookup/3</a></td><td>Handles cache lookup, modulated by the caching options requested by
the user.</td></tr><tr><td valign="top"><a href="#maybe_set-2">maybe_set/2*</a></td><td>Takes a key and two maps, returning the first map with the key set to
the value of the second map _if_ the value is not undefined.</td></tr><tr><td valign="top"><a href="#maybe_store-4">maybe_store/4</a></td><td>Write a resulting M3 message to the cache if requested.</td></tr><tr><td valign="top"><a href="#message_source_cache_control_test-0">message_source_cache_control_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#message_without_cache_control_test-0">message_without_cache_control_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#msg_precidence_overrides_test-0">msg_precidence_overrides_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#msg_with_cc-1">msg_with_cc/1*</a></td><td></td></tr><tr><td valign="top"><a href="#multiple_directives_test-0">multiple_directives_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#necessary_messages_not_found_error-3">necessary_messages_not_found_error/3*</a></td><td>Generate a message to return when the necessary messages to execute a
cache lookup are not found in the cache.</td></tr><tr><td valign="top"><a href="#no_cache_directive_test-0">no_cache_directive_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#no_store_directive_test-0">no_store_directive_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#only_if_cached_directive_test-0">only_if_cached_directive_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#only_if_cached_not_found_error-3">only_if_cached_not_found_error/3*</a></td><td>Generate a message to return when <code>only_if_cached</code> was specified, and
we don't have a cached result.</td></tr><tr><td valign="top"><a href="#opts_override_message_settings_test-0">opts_override_message_settings_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#opts_source_cache_control_test-0">opts_source_cache_control_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#opts_with_cc-1">opts_with_cc/1*</a></td><td></td></tr><tr><td valign="top"><a href="#specifiers_to_cache_settings-1">specifiers_to_cache_settings/1*</a></td><td>Convert a cache control list as received via HTTP headers into a
normalized map of simply whether we should store and/or lookup the result.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cache_binary_result_test-0"></a>

### cache_binary_result_test/0 * ###

`cache_binary_result_test() -> any()`

<a name="cache_message_result_test-0"></a>

### cache_message_result_test/0 * ###

`cache_message_result_test() -> any()`

<a name="cache_source_to_cache_settings-1"></a>

### cache_source_to_cache_settings/1 * ###

`cache_source_to_cache_settings(Msg) -> any()`

Convert a cache source to a cache setting. The setting _must_ always be
directly in the source, not an AO-Core-derivable value. The
`to_cache_control_map` function is used as the source of settings in all
cases, except where an `Opts` specifies that hashpaths should not be updated,
which leads to the result not being cached (as it may be stored with an
incorrect hashpath).

<a name="derive_cache_settings-2"></a>

### derive_cache_settings/2 * ###

`derive_cache_settings(SourceList, Opts) -> any()`

Derive cache settings from a series of option sources and the opts,
honoring precidence order. The Opts is used as the first source. Returns a
map with `store` and `lookup` keys, each of which is a boolean.

For example, if the last source has a `no_store`, the first expresses no
preference, but the Opts has `cache_control => [always]`, then the result
will contain a `store => true` entry.

<a name="dispatch_cache_write-4"></a>

### dispatch_cache_write/4 * ###

`dispatch_cache_write(Msg1, Msg2, Msg3, Opts) -> any()`

Dispatch the cache write to a worker process if requested.
Invoke the appropriate cache write function based on the type of the message.

<a name="empty_message_list_test-0"></a>

### empty_message_list_test/0 * ###

`empty_message_list_test() -> any()`

<a name="exec_likely_faster_heuristic-3"></a>

### exec_likely_faster_heuristic/3 * ###

`exec_likely_faster_heuristic(Msg1, Msg2, Opts) -> any()`

Determine whether we are likely to be faster looking up the result in
our cache (hoping we have it), or executing it directly.

<a name="hashpath_ignore_prevents_storage_test-0"></a>

### hashpath_ignore_prevents_storage_test/0 * ###

`hashpath_ignore_prevents_storage_test() -> any()`

<a name="is_explicit_lookup-3"></a>

### is_explicit_lookup/3 * ###

`is_explicit_lookup(Msg1, X2, Opts) -> any()`

<a name="lookup-3"></a>

### lookup/3 * ###

`lookup(Msg1, Msg2, Opts) -> any()`

<a name="maybe_lookup-3"></a>

### maybe_lookup/3 ###

`maybe_lookup(Msg1, Msg2, Opts) -> any()`

Handles cache lookup, modulated by the caching options requested by
the user. Honors the following `Opts` cache keys:
`only_if_cached`: If set and we do not find a result in the cache,
return an error with a `Cache-Status` of `miss` and
a 504 `Status`.
`no_cache`:       If set, the cached values are never used. Returns
`continue` to the caller.

<a name="maybe_set-2"></a>

### maybe_set/2 * ###

`maybe_set(Map1, Map2) -> any()`

Takes a key and two maps, returning the first map with the key set to
the value of the second map _if_ the value is not undefined.

<a name="maybe_store-4"></a>

### maybe_store/4 ###

`maybe_store(Msg1, Msg2, Msg3, Opts) -> any()`

Write a resulting M3 message to the cache if requested. The precedence
order of cache control sources is as follows:
1. The `Opts` map (letting the node operator have the final say).
2. The `Msg3` results message (granted by Msg1's device).
3. The `Msg2` message (the user's request).
Msg1 is not used, such that it can specify cache control information about
itself, without affecting its outputs.

<a name="message_source_cache_control_test-0"></a>

### message_source_cache_control_test/0 * ###

`message_source_cache_control_test() -> any()`

<a name="message_without_cache_control_test-0"></a>

### message_without_cache_control_test/0 * ###

`message_without_cache_control_test() -> any()`

<a name="msg_precidence_overrides_test-0"></a>

### msg_precidence_overrides_test/0 * ###

`msg_precidence_overrides_test() -> any()`

<a name="msg_with_cc-1"></a>

### msg_with_cc/1 * ###

`msg_with_cc(CC) -> any()`

<a name="multiple_directives_test-0"></a>

### multiple_directives_test/0 * ###

`multiple_directives_test() -> any()`

<a name="necessary_messages_not_found_error-3"></a>

### necessary_messages_not_found_error/3 * ###

`necessary_messages_not_found_error(Msg1, Msg2, Opts) -> any()`

Generate a message to return when the necessary messages to execute a
cache lookup are not found in the cache.

<a name="no_cache_directive_test-0"></a>

### no_cache_directive_test/0 * ###

`no_cache_directive_test() -> any()`

<a name="no_store_directive_test-0"></a>

### no_store_directive_test/0 * ###

`no_store_directive_test() -> any()`

<a name="only_if_cached_directive_test-0"></a>

### only_if_cached_directive_test/0 * ###

`only_if_cached_directive_test() -> any()`

<a name="only_if_cached_not_found_error-3"></a>

### only_if_cached_not_found_error/3 * ###

`only_if_cached_not_found_error(Msg1, Msg2, Opts) -> any()`

Generate a message to return when `only_if_cached` was specified, and
we don't have a cached result.

<a name="opts_override_message_settings_test-0"></a>

### opts_override_message_settings_test/0 * ###

`opts_override_message_settings_test() -> any()`

<a name="opts_source_cache_control_test-0"></a>

### opts_source_cache_control_test/0 * ###

`opts_source_cache_control_test() -> any()`

<a name="opts_with_cc-1"></a>

### opts_with_cc/1 * ###

`opts_with_cc(CC) -> any()`

<a name="specifiers_to_cache_settings-1"></a>

### specifiers_to_cache_settings/1 * ###

`specifiers_to_cache_settings(CCSpecifier) -> any()`

Convert a cache control list as received via HTTP headers into a
normalized map of simply whether we should store and/or lookup the result.

