# [Module hb_util.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_util.erl)




A collection of utility functions for building with HyperBEAM.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_commas-1">add_commas/1*</a></td><td></td></tr><tr><td valign="top"><a href="#all_hb_modules-0">all_hb_modules/0</a></td><td>Get all loaded modules that are loaded and are part of HyperBEAM.</td></tr><tr><td valign="top"><a href="#atom-1">atom/1</a></td><td>Coerce a string to an atom.</td></tr><tr><td valign="top"><a href="#bin-1">bin/1</a></td><td>Coerce a value to a binary.</td></tr><tr><td valign="top"><a href="#count-2">count/2</a></td><td></td></tr><tr><td valign="top"><a href="#debug_fmt-1">debug_fmt/1</a></td><td>Convert a term to a string for debugging print purposes.</td></tr><tr><td valign="top"><a href="#debug_fmt-2">debug_fmt/2</a></td><td></td></tr><tr><td valign="top"><a href="#debug_print-4">debug_print/4</a></td><td>Print a message to the standard error stream, prefixed by the amount
of time that has elapsed since the last call to this function.</td></tr><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Try to decode a URL safe base64 into a binary or throw an error when
invalid.</td></tr><tr><td valign="top"><a href="#deep_merge-2">deep_merge/2</a></td><td>Deep merge two maps, recursively merging nested maps.</td></tr><tr><td valign="top"><a href="#do_debug_fmt-2">do_debug_fmt/2*</a></td><td></td></tr><tr><td valign="top"><a href="#do_to_lines-1">do_to_lines/1*</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Encode a binary to URL safe base64 binary string.</td></tr><tr><td valign="top"><a href="#eunit_print-2">eunit_print/2</a></td><td>Format and print an indented string to standard error.</td></tr><tr><td valign="top"><a href="#find_value-2">find_value/2</a></td><td>Find the value associated with a key in parsed a JSON structure list.</td></tr><tr><td valign="top"><a href="#find_value-3">find_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#float-1">float/1</a></td><td>Coerce a string to a float.</td></tr><tr><td valign="top"><a href="#format_address-2">format_address/2*</a></td><td>If the user attempts to print a wallet, format it as an address.</td></tr><tr><td valign="top"><a href="#format_binary-1">format_binary/1</a></td><td>Format a binary as a short string suitable for printing.</td></tr><tr><td valign="top"><a href="#format_debug_trace-3">format_debug_trace/3*</a></td><td>Generate the appropriate level of trace for a given call.</td></tr><tr><td valign="top"><a href="#format_indented-2">format_indented/2</a></td><td>Format a string with an indentation level.</td></tr><tr><td valign="top"><a href="#format_indented-3">format_indented/3</a></td><td></td></tr><tr><td valign="top"><a href="#format_maybe_multiline-2">format_maybe_multiline/2</a></td><td>Format a map as either a single line or a multi-line string depending
on the value of the <code>debug_print_map_line_threshold</code> runtime option.</td></tr><tr><td valign="top"><a href="#format_trace-1">format_trace/1</a></td><td>Format a stack trace as a list of strings, one for each stack frame.</td></tr><tr><td valign="top"><a href="#format_trace-2">format_trace/2*</a></td><td></td></tr><tr><td valign="top"><a href="#format_trace_short-1">format_trace_short/1</a></td><td>Format a trace to a short string.</td></tr><tr><td valign="top"><a href="#format_trace_short-4">format_trace_short/4*</a></td><td></td></tr><tr><td valign="top"><a href="#format_tuple-2">format_tuple/2*</a></td><td>Helper function to format tuples with arity greater than 2.</td></tr><tr><td valign="top"><a href="#get_trace-0">get_trace/0*</a></td><td>Get the trace of the current process.</td></tr><tr><td valign="top"><a href="#hd-1">hd/1</a></td><td>Get the first element (the lowest integer key >= 1) of a numbered map.</td></tr><tr><td valign="top"><a href="#hd-2">hd/2</a></td><td></td></tr><tr><td valign="top"><a href="#hd-3">hd/3</a></td><td></td></tr><tr><td valign="top"><a href="#hd-5">hd/5*</a></td><td></td></tr><tr><td valign="top"><a href="#human_id-1">human_id/1</a></td><td>Convert a native binary ID to a human readable ID.</td></tr><tr><td valign="top"><a href="#human_int-1">human_int/1</a></td><td>Add <code>,</code> characters to a number every 3 digits to make it human readable.</td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Return the human-readable form of an ID of a message when given either
a message explicitly, raw encoded ID, or an Erlang Arweave <code>tx</code> record.</td></tr><tr><td valign="top"><a href="#id-2">id/2</a></td><td></td></tr><tr><td valign="top"><a href="#int-1">int/1</a></td><td>Coerce a string to an integer.</td></tr><tr><td valign="top"><a href="#is_hb_module-1">is_hb_module/1</a></td><td>Is the given module part of HyperBEAM?.</td></tr><tr><td valign="top"><a href="#is_hb_module-2">is_hb_module/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_human_binary-1">is_human_binary/1*</a></td><td>Determine whether a binary is human-readable.</td></tr><tr><td valign="top"><a href="#is_ordered_list-1">is_ordered_list/1</a></td><td>Determine if the message given is an ordered list, starting from 1.</td></tr><tr><td valign="top"><a href="#is_ordered_list-2">is_ordered_list/2*</a></td><td></td></tr><tr><td valign="top"><a href="#is_string_list-1">is_string_list/1</a></td><td>Is the given term a string list?.</td></tr><tr><td valign="top"><a href="#key_to_atom-2">key_to_atom/2</a></td><td>Convert keys in a map to atoms, lowering <code>-</code> to <code>_</code>.</td></tr><tr><td valign="top"><a href="#list-1">list/1</a></td><td>Coerce a value to a list.</td></tr><tr><td valign="top"><a href="#list_to_numbered_map-1">list_to_numbered_map/1</a></td><td>Convert a list of elements to a map with numbered keys.</td></tr><tr><td valign="top"><a href="#maybe_throw-2">maybe_throw/2</a></td><td>Throw an exception if the Opts map has an <code>error_strategy</code> key with the
value <code>throw</code>.</td></tr><tr><td valign="top"><a href="#mean-1">mean/1</a></td><td></td></tr><tr><td valign="top"><a href="#message_to_ordered_list-1">message_to_ordered_list/1</a></td><td>Take a message with numbered keys and convert it to a list of tuples
with the associated key as an integer and a value.</td></tr><tr><td valign="top"><a href="#message_to_ordered_list-2">message_to_ordered_list/2</a></td><td></td></tr><tr><td valign="top"><a href="#message_to_ordered_list-4">message_to_ordered_list/4*</a></td><td></td></tr><tr><td valign="top"><a href="#native_id-1">native_id/1</a></td><td>Convert a human readable ID to a native binary ID.</td></tr><tr><td valign="top"><a href="#normalize_trace-1">normalize_trace/1*</a></td><td>Remove all calls from this module from the top of a trace.</td></tr><tr><td valign="top"><a href="#number-1">number/1</a></td><td>Label a list of elements with a number.</td></tr><tr><td valign="top"><a href="#ok-1">ok/1</a></td><td>Unwrap a tuple of the form <code>{ok, Value}</code>, or throw/return, depending on
the value of the <code>error_strategy</code> option.</td></tr><tr><td valign="top"><a href="#ok-2">ok/2</a></td><td></td></tr><tr><td valign="top"><a href="#pick_weighted-2">pick_weighted/2*</a></td><td></td></tr><tr><td valign="top"><a href="#print_trace-3">print_trace/3*</a></td><td></td></tr><tr><td valign="top"><a href="#print_trace-4">print_trace/4</a></td><td>Print the trace of the current stack, up to the first non-hyperbeam
module.</td></tr><tr><td valign="top"><a href="#print_trace_short-4">print_trace_short/4</a></td><td>Print a trace to the standard error stream.</td></tr><tr><td valign="top"><a href="#remove_common-2">remove_common/2</a></td><td>Remove the common prefix from two strings, returning the remainder of the
first string.</td></tr><tr><td valign="top"><a href="#remove_trailing_noise-1">remove_trailing_noise/1*</a></td><td></td></tr><tr><td valign="top"><a href="#remove_trailing_noise-2">remove_trailing_noise/2</a></td><td></td></tr><tr><td valign="top"><a href="#safe_decode-1">safe_decode/1</a></td><td>Safely decode a URL safe base64 into a binary returning an ok or error
tuple.</td></tr><tr><td valign="top"><a href="#safe_encode-1">safe_encode/1</a></td><td>Safely encode a binary to URL safe base64.</td></tr><tr><td valign="top"><a href="#short_id-1">short_id/1</a></td><td>Return a short ID for the different types of IDs used in AO-Core.</td></tr><tr><td valign="top"><a href="#shuffle-1">shuffle/1*</a></td><td>Shuffle a list.</td></tr><tr><td valign="top"><a href="#stddev-1">stddev/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_hex-1">to_hex/1</a></td><td>Convert a binary to a hex string.</td></tr><tr><td valign="top"><a href="#to_lines-1">to_lines/1*</a></td><td></td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td>Convert a binary to a lowercase.</td></tr><tr><td valign="top"><a href="#to_sorted_keys-1">to_sorted_keys/1</a></td><td>Given a map or KVList, return a deterministically ordered list of its keys.</td></tr><tr><td valign="top"><a href="#to_sorted_list-1">to_sorted_list/1</a></td><td>Given a map or KVList, return a deterministically sorted list of its
key-value pairs.</td></tr><tr><td valign="top"><a href="#trace_macro_helper-5">trace_macro_helper/5</a></td><td>Utility function to help macro <code>?trace/0</code> remove the first frame of the
stack trace.</td></tr><tr><td valign="top"><a href="#until-1">until/1</a></td><td>Utility function to wait for a condition to be true.</td></tr><tr><td valign="top"><a href="#until-2">until/2</a></td><td></td></tr><tr><td valign="top"><a href="#until-3">until/3</a></td><td></td></tr><tr><td valign="top"><a href="#variance-1">variance/1</a></td><td></td></tr><tr><td valign="top"><a href="#weighted_random-1">weighted_random/1</a></td><td>Return a random element from a list, weighted by the values in the list.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_commas-1"></a>

### add_commas/1 * ###

`add_commas(Rest) -> any()`

<a name="all_hb_modules-0"></a>

### all_hb_modules/0 ###

`all_hb_modules() -> any()`

Get all loaded modules that are loaded and are part of HyperBEAM.

<a name="atom-1"></a>

### atom/1 ###

`atom(Str) -> any()`

Coerce a string to an atom.

<a name="bin-1"></a>

### bin/1 ###

`bin(Value) -> any()`

Coerce a value to a binary.

<a name="count-2"></a>

### count/2 ###

`count(Item, List) -> any()`

<a name="debug_fmt-1"></a>

### debug_fmt/1 ###

`debug_fmt(X) -> any()`

Convert a term to a string for debugging print purposes.

<a name="debug_fmt-2"></a>

### debug_fmt/2 ###

`debug_fmt(X, Indent) -> any()`

<a name="debug_print-4"></a>

### debug_print/4 ###

`debug_print(X, Mod, Func, LineNum) -> any()`

Print a message to the standard error stream, prefixed by the amount
of time that has elapsed since the last call to this function.

<a name="decode-1"></a>

### decode/1 ###

`decode(Input) -> any()`

Try to decode a URL safe base64 into a binary or throw an error when
invalid.

<a name="deep_merge-2"></a>

### deep_merge/2 ###

`deep_merge(Map1, Map2) -> any()`

Deep merge two maps, recursively merging nested maps.

<a name="do_debug_fmt-2"></a>

### do_debug_fmt/2 * ###

`do_debug_fmt(Wallet, Indent) -> any()`

<a name="do_to_lines-1"></a>

### do_to_lines/1 * ###

`do_to_lines(In) -> any()`

<a name="encode-1"></a>

### encode/1 ###

`encode(Bin) -> any()`

Encode a binary to URL safe base64 binary string.

<a name="eunit_print-2"></a>

### eunit_print/2 ###

`eunit_print(FmtStr, FmtArgs) -> any()`

Format and print an indented string to standard error.

<a name="find_value-2"></a>

### find_value/2 ###

`find_value(Key, List) -> any()`

Find the value associated with a key in parsed a JSON structure list.

<a name="find_value-3"></a>

### find_value/3 ###

`find_value(Key, Map, Default) -> any()`

<a name="float-1"></a>

### float/1 ###

`float(Str) -> any()`

Coerce a string to a float.

<a name="format_address-2"></a>

### format_address/2 * ###

`format_address(Wallet, Indent) -> any()`

If the user attempts to print a wallet, format it as an address.

<a name="format_binary-1"></a>

### format_binary/1 ###

`format_binary(Bin) -> any()`

Format a binary as a short string suitable for printing.

<a name="format_debug_trace-3"></a>

### format_debug_trace/3 * ###

`format_debug_trace(Mod, Func, Line) -> any()`

Generate the appropriate level of trace for a given call.

<a name="format_indented-2"></a>

### format_indented/2 ###

`format_indented(Str, Indent) -> any()`

Format a string with an indentation level.

<a name="format_indented-3"></a>

### format_indented/3 ###

`format_indented(RawStr, Fmt, Ind) -> any()`

<a name="format_maybe_multiline-2"></a>

### format_maybe_multiline/2 ###

`format_maybe_multiline(X, Indent) -> any()`

Format a map as either a single line or a multi-line string depending
on the value of the `debug_print_map_line_threshold` runtime option.

<a name="format_trace-1"></a>

### format_trace/1 ###

`format_trace(Stack) -> any()`

Format a stack trace as a list of strings, one for each stack frame.
Each stack frame is formatted if it matches the `stack_print_prefixes`
option. At the first frame that does not match a prefix in the
`stack_print_prefixes` option, the rest of the stack is not formatted.

<a name="format_trace-2"></a>

### format_trace/2 * ###

`format_trace(Rest, Prefixes) -> any()`

<a name="format_trace_short-1"></a>

### format_trace_short/1 ###

`format_trace_short(Trace) -> any()`

Format a trace to a short string.

<a name="format_trace_short-4"></a>

### format_trace_short/4 * ###

`format_trace_short(Max, Latch, Trace, Prefixes) -> any()`

<a name="format_tuple-2"></a>

### format_tuple/2 * ###

`format_tuple(Tuple, Indent) -> any()`

Helper function to format tuples with arity greater than 2.

<a name="get_trace-0"></a>

### get_trace/0 * ###

`get_trace() -> any()`

Get the trace of the current process.

<a name="hd-1"></a>

### hd/1 ###

`hd(Message) -> any()`

Get the first element (the lowest integer key >= 1) of a numbered map.
Optionally, it takes a specifier of whether to return the key or the value,
as well as a standard map of HyperBEAM runtime options.

<a name="hd-2"></a>

### hd/2 ###

`hd(Message, ReturnType) -> any()`

<a name="hd-3"></a>

### hd/3 ###

`hd(Message, ReturnType, Opts) -> any()`

<a name="hd-5"></a>

### hd/5 * ###

`hd(Map, Rest, Index, ReturnType, Opts) -> any()`

<a name="human_id-1"></a>

### human_id/1 ###

`human_id(Bin) -> any()`

Convert a native binary ID to a human readable ID. If the ID is already
a human readable ID, it is returned as is. If it is an ethereum address, it
is returned as is.

<a name="human_int-1"></a>

### human_int/1 ###

`human_int(Int) -> any()`

Add `,` characters to a number every 3 digits to make it human readable.

<a name="id-1"></a>

### id/1 ###

`id(Item) -> any()`

Return the human-readable form of an ID of a message when given either
a message explicitly, raw encoded ID, or an Erlang Arweave `tx` record.

<a name="id-2"></a>

### id/2 ###

`id(TX, Type) -> any()`

<a name="int-1"></a>

### int/1 ###

`int(Str) -> any()`

Coerce a string to an integer.

<a name="is_hb_module-1"></a>

### is_hb_module/1 ###

`is_hb_module(Atom) -> any()`

Is the given module part of HyperBEAM?

<a name="is_hb_module-2"></a>

### is_hb_module/2 ###

`is_hb_module(Atom, Prefixes) -> any()`

<a name="is_human_binary-1"></a>

### is_human_binary/1 * ###

`is_human_binary(Bin) -> any()`

Determine whether a binary is human-readable.

<a name="is_ordered_list-1"></a>

### is_ordered_list/1 ###

`is_ordered_list(Msg) -> any()`

Determine if the message given is an ordered list, starting from 1.

<a name="is_ordered_list-2"></a>

### is_ordered_list/2 * ###

`is_ordered_list(N, Msg) -> any()`

<a name="is_string_list-1"></a>

### is_string_list/1 ###

`is_string_list(MaybeString) -> any()`

Is the given term a string list?

<a name="key_to_atom-2"></a>

### key_to_atom/2 ###

`key_to_atom(Key, Mode) -> any()`

Convert keys in a map to atoms, lowering `-` to `_`.

<a name="list-1"></a>

### list/1 ###

`list(Value) -> any()`

Coerce a value to a list.

<a name="list_to_numbered_map-1"></a>

### list_to_numbered_map/1 ###

`list_to_numbered_map(List) -> any()`

Convert a list of elements to a map with numbered keys.

<a name="maybe_throw-2"></a>

### maybe_throw/2 ###

`maybe_throw(Val, Opts) -> any()`

Throw an exception if the Opts map has an `error_strategy` key with the
value `throw`. Otherwise, return the value.

<a name="mean-1"></a>

### mean/1 ###

`mean(List) -> any()`

<a name="message_to_ordered_list-1"></a>

### message_to_ordered_list/1 ###

`message_to_ordered_list(Message) -> any()`

Take a message with numbered keys and convert it to a list of tuples
with the associated key as an integer and a value. Optionally, it takes a
standard map of HyperBEAM runtime options.

<a name="message_to_ordered_list-2"></a>

### message_to_ordered_list/2 ###

`message_to_ordered_list(Message, Opts) -> any()`

<a name="message_to_ordered_list-4"></a>

### message_to_ordered_list/4 * ###

`message_to_ordered_list(Message, Keys, Key, Opts) -> any()`

<a name="native_id-1"></a>

### native_id/1 ###

`native_id(Bin) -> any()`

Convert a human readable ID to a native binary ID. If the ID is already
a native binary ID, it is returned as is.

<a name="normalize_trace-1"></a>

### normalize_trace/1 * ###

`normalize_trace(Rest) -> any()`

Remove all calls from this module from the top of a trace.

<a name="number-1"></a>

### number/1 ###

`number(List) -> any()`

Label a list of elements with a number.

<a name="ok-1"></a>

### ok/1 ###

`ok(Value) -> any()`

Unwrap a tuple of the form `{ok, Value}`, or throw/return, depending on
the value of the `error_strategy` option.

<a name="ok-2"></a>

### ok/2 ###

`ok(Other, Opts) -> any()`

<a name="pick_weighted-2"></a>

### pick_weighted/2 * ###

`pick_weighted(Rest, Remaining) -> any()`

<a name="print_trace-3"></a>

### print_trace/3 * ###

`print_trace(Stack, Label, CallerInfo) -> any()`

<a name="print_trace-4"></a>

### print_trace/4 ###

`print_trace(Stack, CallMod, CallFunc, CallLine) -> any()`

Print the trace of the current stack, up to the first non-hyperbeam
module. Prints each stack frame on a new line, until it finds a frame that
does not start with a prefix in the `stack_print_prefixes` hb_opts.
Optionally, you may call this function with a custom label and caller info,
which will be used instead of the default.

<a name="print_trace_short-4"></a>

### print_trace_short/4 ###

`print_trace_short(Trace, Mod, Func, Line) -> any()`

Print a trace to the standard error stream.

<a name="remove_common-2"></a>

### remove_common/2 ###

`remove_common(MainStr, SubStr) -> any()`

Remove the common prefix from two strings, returning the remainder of the
first string. This function also coerces lists to binaries where appropriate,
returning the type of the first argument.

<a name="remove_trailing_noise-1"></a>

### remove_trailing_noise/1 * ###

`remove_trailing_noise(Str) -> any()`

<a name="remove_trailing_noise-2"></a>

### remove_trailing_noise/2 ###

`remove_trailing_noise(Str, Noise) -> any()`

<a name="safe_decode-1"></a>

### safe_decode/1 ###

`safe_decode(E) -> any()`

Safely decode a URL safe base64 into a binary returning an ok or error
tuple.

<a name="safe_encode-1"></a>

### safe_encode/1 ###

`safe_encode(Bin) -> any()`

Safely encode a binary to URL safe base64.

<a name="short_id-1"></a>

### short_id/1 ###

`short_id(Bin) -> any()`

Return a short ID for the different types of IDs used in AO-Core.

<a name="shuffle-1"></a>

### shuffle/1 * ###

`shuffle(List) -> any()`

Shuffle a list.

<a name="stddev-1"></a>

### stddev/1 ###

`stddev(List) -> any()`

<a name="to_hex-1"></a>

### to_hex/1 ###

`to_hex(Bin) -> any()`

Convert a binary to a hex string. Do not use this for anything other than
generating a lower-case, non-special character id. It should not become part of
the core protocol. We use b64u for efficient encoding.

<a name="to_lines-1"></a>

### to_lines/1 * ###

`to_lines(Elems) -> any()`

<a name="to_lower-1"></a>

### to_lower/1 ###

`to_lower(Str) -> any()`

Convert a binary to a lowercase.

<a name="to_sorted_keys-1"></a>

### to_sorted_keys/1 ###

`to_sorted_keys(Msg) -> any()`

Given a map or KVList, return a deterministically ordered list of its keys.

<a name="to_sorted_list-1"></a>

### to_sorted_list/1 ###

`to_sorted_list(Msg) -> any()`

Given a map or KVList, return a deterministically sorted list of its
key-value pairs.

<a name="trace_macro_helper-5"></a>

### trace_macro_helper/5 ###

`trace_macro_helper(Fun, X2, Mod, Func, Line) -> any()`

Utility function to help macro `?trace/0` remove the first frame of the
stack trace.

<a name="until-1"></a>

### until/1 ###

`until(Condition) -> any()`

Utility function to wait for a condition to be true. Optionally,
you can pass a function that will be called with the current count of
iterations, returning an integer that will be added to the count. Once the
condition is true, the function will return the count.

<a name="until-2"></a>

### until/2 ###

`until(Condition, Count) -> any()`

<a name="until-3"></a>

### until/3 ###

`until(Condition, Fun, Count) -> any()`

<a name="variance-1"></a>

### variance/1 ###

`variance(List) -> any()`

<a name="weighted_random-1"></a>

### weighted_random/1 ###

`weighted_random(List) -> any()`

Return a random element from a list, weighted by the values in the list.

