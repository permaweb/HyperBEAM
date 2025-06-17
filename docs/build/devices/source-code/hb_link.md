# [Module hb_link.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_link.erl)




Utility functions for working with links.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode_all_links-1">decode_all_links/1</a></td><td>Decode links embedded in the headers of a message.</td></tr><tr><td valign="top"><a href="#format-1">format/1</a></td><td>Format a link as a short string suitable for printing.</td></tr><tr><td valign="top"><a href="#format-2">format/2</a></td><td></td></tr><tr><td valign="top"><a href="#format_unresolved-1">format_unresolved/1*</a></td><td>Format a link without resolving it.</td></tr><tr><td valign="top"><a href="#is_link_key-1">is_link_key/1</a></td><td>Determine if a key is an encoded link.</td></tr><tr><td valign="top"><a href="#normalize-2">normalize/2</a></td><td>Takes a message and ensures that it is normalized:.</td></tr><tr><td valign="top"><a href="#normalize-3">normalize/3</a></td><td></td></tr><tr><td valign="top"><a href="#offload_linked_message_test-0">offload_linked_message_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#offload_list_test-0">offload_list_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#read-1">read/1</a></td><td>Read a link into memory.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td></td></tr><tr><td valign="top"><a href="#remove_link_specifier-1">remove_link_specifier/1</a></td><td>Remove any <code>+link</code> suffixes from a key.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode_all_links-1"></a>

### decode_all_links/1 ###

`decode_all_links(Msg) -> any()`

Decode links embedded in the headers of a message.

<a name="format-1"></a>

### format/1 ###

`format(Link) -> any()`

Format a link as a short string suitable for printing. Checks the node
options (optionally) given, to see if it should resolve the link to a value
before printing.

<a name="format-2"></a>

### format/2 ###

`format(Link, Opts) -> any()`

<a name="format_unresolved-1"></a>

### format_unresolved/1 * ###

`format_unresolved(X1) -> any()`

Format a link without resolving it.

<a name="is_link_key-1"></a>

### is_link_key/1 ###

`is_link_key(Key) -> any()`

Determine if a key is an encoded link.

<a name="normalize-2"></a>

### normalize/2 ###

`normalize(Msg, Opts) -> any()`

Takes a message and ensures that it is normalized:

- All literal (binary) lazily-loadable values are in-memory.
- All submaps are represented as links, optionally offloading their local
values to the cache.
- All other values are left unchanged (including their potential types).

The response is a non-recursive, fully loaded message. It may still contain
types, but all submessages are guaranteed to be linkified. This stands in
contrast to `linkify`, which takes a structured message and returns a message
with structured links.

<a name="normalize-3"></a>

### normalize/3 ###

`normalize(Msg, Mode, Opts) -> any()`

<a name="offload_linked_message_test-0"></a>

### offload_linked_message_test/0 * ###

`offload_linked_message_test() -> any()`

<a name="offload_list_test-0"></a>

### offload_list_test/0 * ###

`offload_list_test() -> any()`

<a name="read-1"></a>

### read/1 ###

`read(Link) -> any()`

Read a link into memory. Uses `hb_cache:ensure_loaded/2` under-the-hood.

<a name="read-2"></a>

### read/2 ###

`read(Link, Opts) -> any()`

<a name="remove_link_specifier-1"></a>

### remove_link_specifier/1 ###

`remove_link_specifier(Key) -> any()`

Remove any `+link` suffixes from a key.

