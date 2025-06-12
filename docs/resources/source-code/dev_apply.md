# [Module dev_apply.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_apply.erl)




A device that executes AO resolutions.

<a name="description"></a>

## Description ##

It can be passed either a key
that points to a singleton message or list of messages to resolve, or a
`base` and `request` pair to execute together via invoking the `pair` key.

When given a message with a `base` and `request` key, the default handler
will invoke `pair` upon it, setting the `path` in the resulting request to
the key that `apply` was invoked with.

If no `base` or `request` key is present, the default handler will invoke
`eval` upon the given message, using the given key as the `source` of the
message/list of messages to resolve.

Paths found in keys interpreted by this device can contain a `base:` or
`request:` prefix to indicate the message from which the path should be
retrieved. If no such prefix is present, the `Request` message is checked
first, and the `Base` message is checked second.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply_over_http_test-0">apply_over_http_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#default-4">default/4*</a></td><td>The default handler.</td></tr><tr><td valign="top"><a href="#error_to_message-1">error_to_message/1*</a></td><td>Convert an error to a message.</td></tr><tr><td valign="top"><a href="#eval-3">eval/3*</a></td><td>Apply the request's <code>source</code> key.</td></tr><tr><td valign="top"><a href="#find_message-4">find_message/4*</a></td><td>Find the value of the source key, supporting <code>base:</code> and <code>request:</code>
prefixes.</td></tr><tr><td valign="top"><a href="#find_path-4">find_path/4*</a></td><td>Resolve the given path on the message as <code>message@1.0</code>.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>The device info.</td></tr><tr><td valign="top"><a href="#normalize_path-1">normalize_path/1*</a></td><td>Normalize the path.</td></tr><tr><td valign="top"><a href="#pair-3">pair/3</a></td><td>Apply the message found at <code>request</code> to the message found at <code>base</code>.</td></tr><tr><td valign="top"><a href="#pair-4">pair/4*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_key_test-0">resolve_key_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_pair_test-0">resolve_pair_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_with_prefix_test-0">resolve_with_prefix_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#reverse_resolve_pair_test-0">reverse_resolve_pair_test/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply_over_http_test-0"></a>

### apply_over_http_test/0 * ###

`apply_over_http_test() -> any()`

<a name="default-4"></a>

### default/4 * ###

`default(Key, Base, Request, Opts) -> any()`

The default handler. If the `base` and `request` keys are present in
the given request, then the `pair` function is called. Otherwise, the `eval`
key is used to resolve the request.

<a name="error_to_message-1"></a>

### error_to_message/1 * ###

`error_to_message(Error) -> any()`

Convert an error to a message.

<a name="eval-3"></a>

### eval/3 * ###

`eval(Base, Request, Opts) -> any()`

Apply the request's `source` key. If this key is invoked as a result
of the default handler, the `source` key is set to the key of the request.

<a name="find_message-4"></a>

### find_message/4 * ###

`find_message(Path, Base, Request, Opts) -> any()`

Find the value of the source key, supporting `base:` and `request:`
prefixes.

<a name="find_path-4"></a>

### find_path/4 * ###

`find_path(Path, Base, Request, Opts) -> any()`

Resolve the given path on the message as `message@1.0`.

<a name="info-1"></a>

### info/1 ###

`info(X1) -> any()`

The device info. Forwards all keys aside `pair`, `keys` and `set` are
resolved with the `apply/4` function.

<a name="normalize_path-1"></a>

### normalize_path/1 * ###

`normalize_path(Path) -> any()`

Normalize the path.

<a name="pair-3"></a>

### pair/3 ###

`pair(Base, Request, Opts) -> any()`

Apply the message found at `request` to the message found at `base`.

<a name="pair-4"></a>

### pair/4 * ###

`pair(PathToSet, Base, Request, Opts) -> any()`

<a name="resolve_key_test-0"></a>

### resolve_key_test/0 * ###

`resolve_key_test() -> any()`

<a name="resolve_pair_test-0"></a>

### resolve_pair_test/0 * ###

`resolve_pair_test() -> any()`

<a name="resolve_with_prefix_test-0"></a>

### resolve_with_prefix_test/0 * ###

`resolve_with_prefix_test() -> any()`

<a name="reverse_resolve_pair_test-0"></a>

### reverse_resolve_pair_test/0 * ###

`reverse_resolve_pair_test() -> any()`

