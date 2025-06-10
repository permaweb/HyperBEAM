# [Module hb_structured_fields.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_structured_fields.erl)




A module for parsing and converting between Erlang and HTTP Structured
Fields, as described in RFC-9651.

<a name="description"></a>

## Description ##

The mapping between Erlang and structured headers types is as follow:

List: list()
Inner list: {list, [item()], params()}
Dictionary: [{binary(), item()}]
There is no distinction between empty list and empty dictionary.
Item with parameters: {item, bare_item(), params()}
Parameters: [{binary(), bare_item()}]
Bare item: one bare_item() that can be of type:
Integer: integer()
Decimal: {decimal, {integer(), integer()}}
String: {string, binary()}
Token: {token, binary()}
Byte sequence: {binary, binary()}
Boolean: boolean()
<a name="types"></a>

## Data Types ##




### <a name="type-sh_bare_item">sh_bare_item()</a> ###


<pre><code>
sh_bare_item() = integer() | <a href="#type-sh_decimal">sh_decimal()</a> | boolean() | {string | token | binary, binary()}
</code></pre>




### <a name="type-sh_decimal">sh_decimal()</a> ###


<pre><code>
sh_decimal() = {decimal, {integer(), integer()}}
</code></pre>




### <a name="type-sh_dictionary">sh_dictionary()</a> ###


<pre><code>
sh_dictionary() = [{binary(), <a href="#type-sh_item">sh_item()</a> | <a href="#type-sh_inner_list">sh_inner_list()</a>}]
</code></pre>




### <a name="type-sh_inner_list">sh_inner_list()</a> ###


<pre><code>
sh_inner_list() = {list, [<a href="#type-sh_item">sh_item()</a>], <a href="#type-sh_params">sh_params()</a>}
</code></pre>




### <a name="type-sh_item">sh_item()</a> ###


<pre><code>
sh_item() = {item, <a href="#type-sh_bare_item">sh_bare_item()</a>, <a href="#type-sh_params">sh_params()</a>}
</code></pre>




### <a name="type-sh_list">sh_list()</a> ###


<pre><code>
sh_list() = [<a href="#type-sh_item">sh_item()</a> | <a href="#type-sh_inner_list">sh_inner_list()</a>]
</code></pre>




### <a name="type-sh_params">sh_params()</a> ###


<pre><code>
sh_params() = [{binary(), <a href="#type-sh_bare_item">sh_bare_item()</a>}]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bare_item-1">bare_item/1</a></td><td></td></tr><tr><td valign="top"><a href="#dictionary-1">dictionary/1</a></td><td></td></tr><tr><td valign="top"><a href="#e2t-1">e2t/1*</a></td><td></td></tr><tr><td valign="top"><a href="#e2tb-1">e2tb/1*</a></td><td></td></tr><tr><td valign="top"><a href="#e2tp-1">e2tp/1*</a></td><td></td></tr><tr><td valign="top"><a href="#escape_string-2">escape_string/2*</a></td><td></td></tr><tr><td valign="top"><a href="#exp_div-1">exp_div/1*</a></td><td></td></tr><tr><td valign="top"><a href="#expected_to_term-1">expected_to_term/1*</a></td><td></td></tr><tr><td valign="top"><a href="#from_bare_item-1">from_bare_item/1</a></td><td>Convert an SF <code>bare_item</code> to an Erlang term.</td></tr><tr><td valign="top"><a href="#inner_list-1">inner_list/1*</a></td><td></td></tr><tr><td valign="top"><a href="#item-1">item/1</a></td><td></td></tr><tr><td valign="top"><a href="#item_or_inner_list-1">item_or_inner_list/1*</a></td><td></td></tr><tr><td valign="top"><a href="#key_to_binary-1">key_to_binary/1*</a></td><td>Convert an Erlang term to a binary key.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td></td></tr><tr><td valign="top"><a href="#params-1">params/1*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_bare_item-1">parse_bare_item/1</a></td><td>Parse an integer or decimal.</td></tr><tr><td valign="top"><a href="#parse_before_param-2">parse_before_param/2*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_binary-2">parse_binary/2*</a></td><td>Parse a byte sequence binary.</td></tr><tr><td valign="top"><a href="#parse_decimal-5">parse_decimal/5*</a></td><td>Parse a decimal binary.</td></tr><tr><td valign="top"><a href="#parse_dict_before_member-2">parse_dict_before_member/2*</a></td><td>Parse a binary SF dictionary before a member.</td></tr><tr><td valign="top"><a href="#parse_dict_before_sep-2">parse_dict_before_sep/2*</a></td><td>Parse a binary SF dictionary before a separator.</td></tr><tr><td valign="top"><a href="#parse_dict_key-3">parse_dict_key/3*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_dictionary-1">parse_dictionary/1</a></td><td>Parse a binary SF dictionary.</td></tr><tr><td valign="top"><a href="#parse_inner_list-2">parse_inner_list/2*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_item-1">parse_item/1</a></td><td>Parse a binary SF item to an SF <code>item</code>.</td></tr><tr><td valign="top"><a href="#parse_item1-1">parse_item1/1*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_list-1">parse_list/1</a></td><td>Parse a binary SF list.</td></tr><tr><td valign="top"><a href="#parse_list_before_member-2">parse_list_before_member/2*</a></td><td>Parse a binary SF list before a member.</td></tr><tr><td valign="top"><a href="#parse_list_before_sep-2">parse_list_before_sep/2*</a></td><td>Parse a binary SF list before a separator.</td></tr><tr><td valign="top"><a href="#parse_list_member-2">parse_list_member/2*</a></td><td>Parse a binary SF list before a member.</td></tr><tr><td valign="top"><a href="#parse_number-3">parse_number/3*</a></td><td>Parse an integer or decimal binary.</td></tr><tr><td valign="top"><a href="#parse_param-3">parse_param/3*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_string-2">parse_string/2*</a></td><td>Parse a string binary.</td></tr><tr><td valign="top"><a href="#parse_struct_hd_test_-0">parse_struct_hd_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_token-2">parse_token/2*</a></td><td>Parse a token binary.</td></tr><tr><td valign="top"><a href="#raw_to_binary-1">raw_to_binary/1*</a></td><td></td></tr><tr><td valign="top"><a href="#struct_hd_identity_test_-0">struct_hd_identity_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to_bare_item-1">to_bare_item/1*</a></td><td>Convert an Erlang term to an SF <code>bare_item</code>.</td></tr><tr><td valign="top"><a href="#to_dictionary-1">to_dictionary/1</a></td><td>Convert a map to a dictionary.</td></tr><tr><td valign="top"><a href="#to_dictionary-2">to_dictionary/2*</a></td><td></td></tr><tr><td valign="top"><a href="#to_dictionary_depth_test-0">to_dictionary_depth_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to_dictionary_test-0">to_dictionary_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to_inner_item-1">to_inner_item/1*</a></td><td>Convert an Erlang term to an SF <code>item</code>.</td></tr><tr><td valign="top"><a href="#to_inner_list-1">to_inner_list/1*</a></td><td>Convert an inner list to an SF term.</td></tr><tr><td valign="top"><a href="#to_inner_list-2">to_inner_list/2*</a></td><td></td></tr><tr><td valign="top"><a href="#to_inner_list-3">to_inner_list/3*</a></td><td></td></tr><tr><td valign="top"><a href="#to_item-1">to_item/1</a></td><td>Convert an item to a dictionary.</td></tr><tr><td valign="top"><a href="#to_item-2">to_item/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_item_or_inner_list-1">to_item_or_inner_list/1*</a></td><td>Convert an Erlang term to an SF <code>item</code> or <code>inner_list</code>.</td></tr><tr><td valign="top"><a href="#to_item_test-0">to_item_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td>Convert a list to an SF term.</td></tr><tr><td valign="top"><a href="#to_list-2">to_list/2*</a></td><td></td></tr><tr><td valign="top"><a href="#to_list_depth_test-0">to_list_depth_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to_list_test-0">to_list_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to_param-1">to_param/1*</a></td><td>Convert an Erlang term to an SF <code>parameter</code>.</td></tr><tr><td valign="top"><a href="#trim_ws-1">trim_ws/1*</a></td><td></td></tr><tr><td valign="top"><a href="#trim_ws_end-2">trim_ws_end/2*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bare_item-1"></a>

### bare_item/1 ###

`bare_item(Integer) -> any()`

<a name="dictionary-1"></a>

### dictionary/1 ###

<pre><code>
dictionary(Map::#{binary() =&gt; <a href="#type-sh_item">sh_item()</a> | <a href="#type-sh_inner_list">sh_inner_list()</a>} | <a href="#type-sh_dictionary">sh_dictionary()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="e2t-1"></a>

### e2t/1 * ###

`e2t(Dict) -> any()`

<a name="e2tb-1"></a>

### e2tb/1 * ###

`e2tb(V) -> any()`

<a name="e2tp-1"></a>

### e2tp/1 * ###

`e2tp(Params) -> any()`

<a name="escape_string-2"></a>

### escape_string/2 * ###

`escape_string(X1, Acc) -> any()`

<a name="exp_div-1"></a>

### exp_div/1 * ###

`exp_div(N) -> any()`

<a name="expected_to_term-1"></a>

### expected_to_term/1 * ###

`expected_to_term(Dict) -> any()`

<a name="from_bare_item-1"></a>

### from_bare_item/1 ###

`from_bare_item(BareItem) -> any()`

Convert an SF `bare_item` to an Erlang term.

<a name="inner_list-1"></a>

### inner_list/1 * ###

`inner_list(X1) -> any()`

<a name="item-1"></a>

### item/1 ###

<pre><code>
item(X1::<a href="#type-sh_item">sh_item()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="item_or_inner_list-1"></a>

### item_or_inner_list/1 * ###

`item_or_inner_list(Value) -> any()`

<a name="key_to_binary-1"></a>

### key_to_binary/1 * ###

`key_to_binary(Key) -> any()`

Convert an Erlang term to a binary key.

<a name="list-1"></a>

### list/1 ###

<pre><code>
list(List::<a href="#type-sh_list">sh_list()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="params-1"></a>

### params/1 * ###

`params(Params) -> any()`

<a name="parse_bare_item-1"></a>

### parse_bare_item/1 ###

`parse_bare_item(X1) -> any()`

Parse an integer or decimal.

<a name="parse_before_param-2"></a>

### parse_before_param/2 * ###

`parse_before_param(X1, Acc) -> any()`

<a name="parse_binary-2"></a>

### parse_binary/2 * ###

`parse_binary(X1, Acc) -> any()`

Parse a byte sequence binary.

<a name="parse_decimal-5"></a>

### parse_decimal/5 * ###

`parse_decimal(R, L1, L2, IntAcc, FracAcc) -> any()`

Parse a decimal binary.

<a name="parse_dict_before_member-2"></a>

### parse_dict_before_member/2 * ###

`parse_dict_before_member(X1, Acc) -> any()`

Parse a binary SF dictionary before a member.

<a name="parse_dict_before_sep-2"></a>

### parse_dict_before_sep/2 * ###

`parse_dict_before_sep(X1, Acc) -> any()`

Parse a binary SF dictionary before a separator.

<a name="parse_dict_key-3"></a>

### parse_dict_key/3 * ###

`parse_dict_key(R, Acc, K) -> any()`

<a name="parse_dictionary-1"></a>

### parse_dictionary/1 ###

<pre><code>
parse_dictionary(X1::binary()) -&gt; <a href="#type-sh_dictionary">sh_dictionary()</a>
</code></pre>
<br />

Parse a binary SF dictionary.

<a name="parse_inner_list-2"></a>

### parse_inner_list/2 * ###

`parse_inner_list(R0, Acc) -> any()`

<a name="parse_item-1"></a>

### parse_item/1 ###

<pre><code>
parse_item(Bin::binary()) -&gt; <a href="#type-sh_item">sh_item()</a>
</code></pre>
<br />

Parse a binary SF item to an SF `item`.

<a name="parse_item1-1"></a>

### parse_item1/1 * ###

`parse_item1(Bin) -> any()`

<a name="parse_list-1"></a>

### parse_list/1 ###

<pre><code>
parse_list(Bin::binary()) -&gt; <a href="#type-sh_list">sh_list()</a>
</code></pre>
<br />

Parse a binary SF list.

<a name="parse_list_before_member-2"></a>

### parse_list_before_member/2 * ###

`parse_list_before_member(R, Acc) -> any()`

Parse a binary SF list before a member.

<a name="parse_list_before_sep-2"></a>

### parse_list_before_sep/2 * ###

`parse_list_before_sep(X1, Acc) -> any()`

Parse a binary SF list before a separator.

<a name="parse_list_member-2"></a>

### parse_list_member/2 * ###

`parse_list_member(R0, Acc) -> any()`

Parse a binary SF list before a member.

<a name="parse_number-3"></a>

### parse_number/3 * ###

`parse_number(R, L, Acc) -> any()`

Parse an integer or decimal binary.

<a name="parse_param-3"></a>

### parse_param/3 * ###

`parse_param(R, Acc, K) -> any()`

<a name="parse_string-2"></a>

### parse_string/2 * ###

`parse_string(X1, Acc) -> any()`

Parse a string binary.

<a name="parse_struct_hd_test_-0"></a>

### parse_struct_hd_test_/0 * ###

`parse_struct_hd_test_() -> any()`

<a name="parse_token-2"></a>

### parse_token/2 * ###

`parse_token(R, Acc) -> any()`

Parse a token binary.

<a name="raw_to_binary-1"></a>

### raw_to_binary/1 * ###

`raw_to_binary(RawList) -> any()`

<a name="struct_hd_identity_test_-0"></a>

### struct_hd_identity_test_/0 * ###

`struct_hd_identity_test_() -> any()`

<a name="to_bare_item-1"></a>

### to_bare_item/1 * ###

`to_bare_item(BareItem) -> any()`

Convert an Erlang term to an SF `bare_item`.

<a name="to_dictionary-1"></a>

### to_dictionary/1 ###

`to_dictionary(Map) -> any()`

Convert a map to a dictionary.

<a name="to_dictionary-2"></a>

### to_dictionary/2 * ###

`to_dictionary(Dict, Rest) -> any()`

<a name="to_dictionary_depth_test-0"></a>

### to_dictionary_depth_test/0 * ###

`to_dictionary_depth_test() -> any()`

<a name="to_dictionary_test-0"></a>

### to_dictionary_test/0 * ###

`to_dictionary_test() -> any()`

<a name="to_inner_item-1"></a>

### to_inner_item/1 * ###

`to_inner_item(Item) -> any()`

Convert an Erlang term to an SF `item`.

<a name="to_inner_list-1"></a>

### to_inner_list/1 * ###

`to_inner_list(Inner) -> any()`

Convert an inner list to an SF term.

<a name="to_inner_list-2"></a>

### to_inner_list/2 * ###

`to_inner_list(Inner, Params) -> any()`

<a name="to_inner_list-3"></a>

### to_inner_list/3 * ###

`to_inner_list(Inner, Rest, Params) -> any()`

<a name="to_item-1"></a>

### to_item/1 ###

`to_item(Item) -> any()`

Convert an item to a dictionary.

<a name="to_item-2"></a>

### to_item/2 ###

`to_item(Item, Params) -> any()`

<a name="to_item_or_inner_list-1"></a>

### to_item_or_inner_list/1 * ###

`to_item_or_inner_list(ItemOrInner) -> any()`

Convert an Erlang term to an SF `item` or `inner_list`.

<a name="to_item_test-0"></a>

### to_item_test/0 * ###

`to_item_test() -> any()`

<a name="to_list-1"></a>

### to_list/1 ###

`to_list(List) -> any()`

Convert a list to an SF term.

<a name="to_list-2"></a>

### to_list/2 * ###

`to_list(Acc, Rest) -> any()`

<a name="to_list_depth_test-0"></a>

### to_list_depth_test/0 * ###

`to_list_depth_test() -> any()`

<a name="to_list_test-0"></a>

### to_list_test/0 * ###

`to_list_test() -> any()`

<a name="to_param-1"></a>

### to_param/1 * ###

`to_param(X1) -> any()`

Convert an Erlang term to an SF `parameter`.

<a name="trim_ws-1"></a>

### trim_ws/1 * ###

`trim_ws(R) -> any()`

<a name="trim_ws_end-2"></a>

### trim_ws_end/2 * ###

`trim_ws_end(Value, N) -> any()`

