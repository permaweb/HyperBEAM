# [Module hb_singleton.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_singleton.erl)




A parser that translates AO-Core HTTP API requests in TABM format
into an ordered list of messages to evaluate.

<a name="description"></a>

## Description ##

The details of this format
are described in `docs/ao-core-http-api.md`.

Syntax overview:

```

       Singleton: Message containing keys and a <code>path</code> field,
                  which may also contain a query string of key-value pairs.
       Path:
           - /Part1/Part2/.../PartN/ => [Part1, Part2, ..., PartN]
           - /ID/Part2/.../PartN => [ID, Part2, ..., PartN]
       Part: (Key + Resolution), Device?, #{ K => V}?
           - Part => #{ path => Part }
           - <code>Part&Key=Value => #{ path => Part, Key => Value }</code>
           - <code>Part&Key => #{ path => Part, Key => true }</code>
           - <code>Part&k1=v1&k2=v2 => #{ path => Part, k1 => `<<"v1">></code>, k2 => <code><<"v2">></code> }'
           - <code>Part~Device => {as, Device, #{ path => Part }}</code>
           - <code>Part~D&K1=V1 => {as, D, #{ path => Part, K1 => `<<"v1">></code> }}'
           - <code>pt&k1+int=1 => #{ path => pt, k1 => 1 }</code>
           - <code>pt~d&k1+int=1 => {as, d, #{ path => pt, k1 => 1 }}</code>
           - <code>(/nested/path) => Resolution of the path /nested/path</code>
           - <code>(/nested/path&k1=v1) => (resolve /nested/path)#{k1 => v1}</code>
           - <code>(/nested/path~D&K1=V1) => (resolve /nested/path)#{K1 => V1}</code>
           - <code>pt&k1+res=(/a/b/c) => #{ path => pt, k1 => (resolve /a/b/c) }</code>
       Key:
           - key: <code><<"value">></code> => #{ key => <code><<"value">></code>, ... } for all messages
           - n.key: <code><<"value">></code> => #{ key => <code><<"value">></code>, ... } for Nth message
           - key+Int: 1 => #{ key => 1, ... }
           - key+Res: /nested/path => #{ key => (resolve /nested/path), ... }
           - N.Key+Res=(/a/b/c) => #{ Key => (resolve /a/b/c), ... }
```

<a name="types"></a>

## Data Types ##




### <a name="type-ao_message">ao_message()</a> ###


<pre><code>
ao_message() = map() | binary()
</code></pre>




### <a name="type-tabm_message">tabm_message()</a> ###


<pre><code>
tabm_message() = map()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all_path_parts-2">all_path_parts/2*</a></td><td>Extract all of the parts from the binary, given (a list of) separators.</td></tr><tr><td valign="top"><a href="#append_path-2">append_path/2*</a></td><td></td></tr><tr><td valign="top"><a href="#apply_types-1">apply_types/1*</a></td><td>Step 3: Apply types to values and remove specifiers.</td></tr><tr><td valign="top"><a href="#basic_hashpath_test-0">basic_hashpath_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#basic_hashpath_to_test-0">basic_hashpath_to_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#build-3">build/3*</a></td><td></td></tr><tr><td valign="top"><a href="#build_messages-2">build_messages/2*</a></td><td>Step 5: Merge the base message with the scoped messages.</td></tr><tr><td valign="top"><a href="#decode_string-1">decode_string/1*</a></td><td>Attempt Cowboy URL decode, then sanitize the result.</td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td>Normalize a singleton TABM message into a list of executable AO-Core
messages.</td></tr><tr><td valign="top"><a href="#group_scoped-2">group_scoped/2*</a></td><td>Step 4: Group headers/query by N-scope.</td></tr><tr><td valign="top"><a href="#inlined_keys_test-0">inlined_keys_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#inlined_keys_to_test-0">inlined_keys_to_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_join-2">maybe_join/2*</a></td><td>Join a list of items with a separator, or return the first item if there
is only one item.</td></tr><tr><td valign="top"><a href="#maybe_subpath-1">maybe_subpath/1*</a></td><td>Check if the string is a subpath, returning it in parsed form,
or the original string with a specifier.</td></tr><tr><td valign="top"><a href="#maybe_typed-2">maybe_typed/2*</a></td><td>Parse a key's type (applying it to the value) and device name if present.</td></tr><tr><td valign="top"><a href="#multiple_inlined_keys_test-0">multiple_inlined_keys_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#multiple_inlined_keys_to_test-0">multiple_inlined_keys_to_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#multiple_messages_test-0">multiple_messages_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#multiple_messages_to_test-0">multiple_messages_to_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_base-1">normalize_base/1*</a></td><td>Normalize the base path.</td></tr><tr><td valign="top"><a href="#parse_explicit_message_test-0">parse_explicit_message_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_full_path-1">parse_full_path/1*</a></td><td>Parse the relative reference into path, query, and fragment.</td></tr><tr><td valign="top"><a href="#parse_inlined_key_val-1">parse_inlined_key_val/1*</a></td><td>Extrapolate the inlined key-value pair from a path segment.</td></tr><tr><td valign="top"><a href="#parse_inlined_keys-2">parse_inlined_keys/2*</a></td><td>Parse inlined key-value pairs from a path segment.</td></tr><tr><td valign="top"><a href="#parse_part-1">parse_part/1*</a></td><td>Parse a path part into a message or an ID.</td></tr><tr><td valign="top"><a href="#parse_part_mods-2">parse_part_mods/2*</a></td><td>Parse part modifiers:
1.</td></tr><tr><td valign="top"><a href="#parse_scope-1">parse_scope/1*</a></td><td>Get the scope of a key.</td></tr><tr><td valign="top"><a href="#part-2">part/2*</a></td><td>Extract the characters from the binary until a separator is found.</td></tr><tr><td valign="top"><a href="#part-4">part/4*</a></td><td></td></tr><tr><td valign="top"><a href="#path_messages-1">path_messages/1*</a></td><td>Step 2: Decode, split and sanitize the path.</td></tr><tr><td valign="top"><a href="#path_parts-2">path_parts/2*</a></td><td>Split the path into segments, filtering out empty segments and
segments that are too long.</td></tr><tr><td valign="top"><a href="#path_parts_test-0">path_parts_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#scoped_key_test-0">scoped_key_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#scoped_key_to_test-0">scoped_key_to_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#simple_to_test-0">simple_to_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#single_message_test-0">single_message_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#subpath_in_inlined_test-0">subpath_in_inlined_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#subpath_in_inlined_to_test-0">subpath_in_inlined_to_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#subpath_in_key_test-0">subpath_in_key_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#subpath_in_key_to_test-0">subpath_in_key_to_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#subpath_in_path_test-0">subpath_in_path_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#subpath_in_path_to_test-0">subpath_in_path_to_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td>Convert a list of AO-Core message into TABM message.</td></tr><tr><td valign="top"><a href="#to_suite_test_-0">to_suite_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1*</a></td><td></td></tr><tr><td valign="top"><a href="#typed_key_test-0">typed_key_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#typed_key_to_test-0">typed_key_to_test/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all_path_parts-2"></a>

### all_path_parts/2 * ###

`all_path_parts(Sep, Bin) -> any()`

Extract all of the parts from the binary, given (a list of) separators.

<a name="append_path-2"></a>

### append_path/2 * ###

`append_path(PathPart, Message) -> any()`

<a name="apply_types-1"></a>

### apply_types/1 * ###

`apply_types(Msg) -> any()`

Step 3: Apply types to values and remove specifiers.

<a name="basic_hashpath_test-0"></a>

### basic_hashpath_test/0 * ###

`basic_hashpath_test() -> any()`

<a name="basic_hashpath_to_test-0"></a>

### basic_hashpath_to_test/0 * ###

`basic_hashpath_to_test() -> any()`

<a name="build-3"></a>

### build/3 * ###

`build(I, Rest, ScopedKeys) -> any()`

<a name="build_messages-2"></a>

### build_messages/2 * ###

`build_messages(Msgs, ScopedModifications) -> any()`

Step 5: Merge the base message with the scoped messages.

<a name="decode_string-1"></a>

### decode_string/1 * ###

`decode_string(B) -> any()`

Attempt Cowboy URL decode, then sanitize the result.

<a name="from-1"></a>

### from/1 ###

`from(Path) -> any()`

Normalize a singleton TABM message into a list of executable AO-Core
messages.

<a name="group_scoped-2"></a>

### group_scoped/2 * ###

`group_scoped(Map, Msgs) -> any()`

Step 4: Group headers/query by N-scope.
`N.Key` => applies to Nth step. Otherwise => `global`

<a name="inlined_keys_test-0"></a>

### inlined_keys_test/0 * ###

`inlined_keys_test() -> any()`

<a name="inlined_keys_to_test-0"></a>

### inlined_keys_to_test/0 * ###

`inlined_keys_to_test() -> any()`

<a name="maybe_join-2"></a>

### maybe_join/2 * ###

`maybe_join(Items, Sep) -> any()`

Join a list of items with a separator, or return the first item if there
is only one item. If there are no items, return an empty binary.

<a name="maybe_subpath-1"></a>

### maybe_subpath/1 * ###

`maybe_subpath(Str) -> any()`

Check if the string is a subpath, returning it in parsed form,
or the original string with a specifier.

<a name="maybe_typed-2"></a>

### maybe_typed/2 * ###

`maybe_typed(Key, Value) -> any()`

Parse a key's type (applying it to the value) and device name if present.

<a name="multiple_inlined_keys_test-0"></a>

### multiple_inlined_keys_test/0 * ###

`multiple_inlined_keys_test() -> any()`

<a name="multiple_inlined_keys_to_test-0"></a>

### multiple_inlined_keys_to_test/0 * ###

`multiple_inlined_keys_to_test() -> any()`

<a name="multiple_messages_test-0"></a>

### multiple_messages_test/0 * ###

`multiple_messages_test() -> any()`

<a name="multiple_messages_to_test-0"></a>

### multiple_messages_to_test/0 * ###

`multiple_messages_to_test() -> any()`

<a name="normalize_base-1"></a>

### normalize_base/1 * ###

`normalize_base(Rest) -> any()`

Normalize the base path.

<a name="parse_explicit_message_test-0"></a>

### parse_explicit_message_test/0 * ###

`parse_explicit_message_test() -> any()`

<a name="parse_full_path-1"></a>

### parse_full_path/1 * ###

`parse_full_path(RelativeRef) -> any()`

Parse the relative reference into path, query, and fragment.

<a name="parse_inlined_key_val-1"></a>

### parse_inlined_key_val/1 * ###

`parse_inlined_key_val(Bin) -> any()`

Extrapolate the inlined key-value pair from a path segment. If the
key has a value, it may provide a type (as with typical keys), but if a
value is not provided, it is assumed to be a boolean `true`.

<a name="parse_inlined_keys-2"></a>

### parse_inlined_keys/2 * ###

`parse_inlined_keys(InlinedMsgBin, Msg) -> any()`

Parse inlined key-value pairs from a path segment. Each key-value pair
is separated by `&` and is of the form `K=V`.

<a name="parse_part-1"></a>

### parse_part/1 * ###

`parse_part(ID) -> any()`

Parse a path part into a message or an ID.
Applies the syntax rules outlined in the module doc, in the following order:
1. ID
2. Part subpath resolutions
3. Inlined key-value pairs
4. Device specifier

<a name="parse_part_mods-2"></a>

### parse_part_mods/2 * ###

`parse_part_mods(X1, Msg) -> any()`

Parse part modifiers:
1. `~Device` => `{as, Device, Msg}`
2. `&K=V` => `Msg#{ K => V }`

<a name="parse_scope-1"></a>

### parse_scope/1 * ###

`parse_scope(KeyBin) -> any()`

Get the scope of a key. Adds 1 to account for the base message.

<a name="part-2"></a>

### part/2 * ###

`part(Sep, Bin) -> any()`

Extract the characters from the binary until a separator is found.
The first argument of the function is an explicit separator character, or
a list of separator characters. Returns a tuple with the separator, the
accumulated characters, and the rest of the binary.

<a name="part-4"></a>

### part/4 * ###

`part(Seps, X2, Depth, CurrAcc) -> any()`

<a name="path_messages-1"></a>

### path_messages/1 * ###

`path_messages(RawBin) -> any()`

Step 2: Decode, split and sanitize the path. Split by `/` but avoid
subpath components, such that their own path parts are not dissociated from
their parent path.

<a name="path_parts-2"></a>

### path_parts/2 * ###

`path_parts(Sep, PathBin) -> any()`

Split the path into segments, filtering out empty segments and
segments that are too long.

<a name="path_parts_test-0"></a>

### path_parts_test/0 * ###

`path_parts_test() -> any()`

<a name="scoped_key_test-0"></a>

### scoped_key_test/0 * ###

`scoped_key_test() -> any()`

<a name="scoped_key_to_test-0"></a>

### scoped_key_to_test/0 * ###

`scoped_key_to_test() -> any()`

<a name="simple_to_test-0"></a>

### simple_to_test/0 * ###

`simple_to_test() -> any()`

<a name="single_message_test-0"></a>

### single_message_test/0 * ###

`single_message_test() -> any()`

<a name="subpath_in_inlined_test-0"></a>

### subpath_in_inlined_test/0 * ###

`subpath_in_inlined_test() -> any()`

<a name="subpath_in_inlined_to_test-0"></a>

### subpath_in_inlined_to_test/0 * ###

`subpath_in_inlined_to_test() -> any()`

<a name="subpath_in_key_test-0"></a>

### subpath_in_key_test/0 * ###

`subpath_in_key_test() -> any()`

<a name="subpath_in_key_to_test-0"></a>

### subpath_in_key_to_test/0 * ###

`subpath_in_key_to_test() -> any()`

<a name="subpath_in_path_test-0"></a>

### subpath_in_path_test/0 * ###

`subpath_in_path_test() -> any()`

<a name="subpath_in_path_to_test-0"></a>

### subpath_in_path_to_test/0 * ###

`subpath_in_path_to_test() -> any()`

<a name="to-1"></a>

### to/1 ###

<pre><code>
to(Messages::[<a href="#type-ao_message">ao_message()</a>]) -&gt; <a href="#type-tabm_message">tabm_message()</a>
</code></pre>
<br />

Convert a list of AO-Core message into TABM message.

<a name="to_suite_test_-0"></a>

### to_suite_test_/0 * ###

`to_suite_test_() -> any()`

<a name="type-1"></a>

### type/1 * ###

`type(Value) -> any()`

<a name="typed_key_test-0"></a>

### typed_key_test/0 * ###

`typed_key_test() -> any()`

<a name="typed_key_to_test-0"></a>

### typed_key_to_test/0 * ###

`typed_key_to_test() -> any()`

