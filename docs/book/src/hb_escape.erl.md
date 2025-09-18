# hb_escape

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_escape.erl)

Functions for escaping and unescaping mixed case values, for use in HTTP
headers. Both percent-encoding and escaping of double-quoted strings
(`"` => `\"`) are supported.
This is necessary for encodings of AO-Core messages for transmission in
HTTP/2 and HTTP/3, because uppercase header keys are explicitly disallowed.
While most map keys in HyperBEAM are normalized to lowercase, IDs are not.
Subsequently, we encode all header keys to lowercase %-encoded URI-style
strings because transmission.

---

## Exported Functions

- `decode_keys/2`
- `decode_quotes/1`
- `decode/1`
- `encode_keys/2`
- `encode_quotes/1`
- `encode/1`

---

### encode

Functions for escaping and unescaping mixed case values, for use in HTTP
Encode a binary as a URI-encoded string.

```erlang
encode(Bin) when is_binary(Bin) ->
    list_to_binary(percent_escape(binary_to_list(Bin))).
```

### decode

Decode a URI-encoded string back to a binary.

```erlang
decode(Bin) when is_binary(Bin) ->
    list_to_binary(percent_unescape(binary_to_list(Bin))).
```

### encode_quotes

Encode a string with escaped quotes.

```erlang
encode_quotes(String) when is_binary(String) ->
    list_to_binary(encode_quotes(binary_to_list(String)));
```

### encode_quotes

Encode a string with escaped quotes.

```erlang
encode_quotes([]) -> [];
```

### encode_quotes

Encode a string with escaped quotes.

```erlang
encode_quotes([$\" | Rest]) -> [$\\, $\" | encode_quotes(Rest)];
```

### encode_quotes

Encode a string with escaped quotes.
Decode a string with escaped quotes.

```erlang
encode_quotes([C | Rest]) -> [C | encode_quotes(Rest)].
```

### decode_quotes

Encode a string with escaped quotes.
Decode a string with escaped quotes.

```erlang
decode_quotes(String) when is_binary(String) ->
    list_to_binary(decode_quotes(binary_to_list(String)));
```

### decode_quotes

Encode a string with escaped quotes.
Decode a string with escaped quotes.

```erlang
decode_quotes([]) -> [];
```

### decode_quotes

Encode a string with escaped quotes.
Decode a string with escaped quotes.

```erlang
decode_quotes([$\\, $\" | Rest]) -> [$\" | decode_quotes(Rest)];
```

### decode_quotes

Encode a string with escaped quotes.
Decode a string with escaped quotes.

```erlang
decode_quotes([$\" | Rest]) -> decode_quotes(Rest);
```

### decode_quotes

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.

```erlang
decode_quotes([C | Rest]) -> [C | decode_quotes(Rest)].
```

### decode_keys

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.

```erlang
decode_keys(Msg, Opts) when is_map(Msg) ->
    hb_maps:from_list(
        lists:map(
            fun({Key, Value}) -> {decode(Key), Value} end,
            hb_maps:to_list(Msg, Opts)
        )
    );
```

### decode_keys

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.
URI encode keys in the base layer of a message. Does not recurse.

```erlang
decode_keys(Other, _Opts) -> Other.
```

### encode_keys

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.
URI encode keys in the base layer of a message. Does not recurse.

```erlang
encode_keys(Msg, Opts) when is_map(Msg) ->
    hb_maps:from_list(
        lists:map(
            fun({Key, Value}) -> {encode(Key), Value} end,
            hb_maps:to_list(Msg, Opts)
        )
    );
```

### encode_keys

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.
URI encode keys in the base layer of a message. Does not recurse.
Escape a list of characters as a URI-encoded string.

```erlang
encode_keys(Other, _Opts) -> Other.
```

### percent_escape

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.
URI encode keys in the base layer of a message. Does not recurse.
Escape a list of characters as a URI-encoded string.

```erlang
percent_escape([]) -> [];
```

### percent_escape

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.
URI encode keys in the base layer of a message. Does not recurse.
Escape a list of characters as a URI-encoded string.

```erlang
percent_escape([C | Cs]) when C >= $a, C =< $z -> [C | percent_escape(Cs)];
```

### percent_escape

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.
URI encode keys in the base layer of a message. Does not recurse.
Escape a list of characters as a URI-encoded string.

```erlang
percent_escape([C | Cs]) when C >= $0, C =< $9 -> [C | percent_escape(Cs)];
```

### percent_escape

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.
URI encode keys in the base layer of a message. Does not recurse.
Escape a list of characters as a URI-encoded string.

```erlang
percent_escape([C | Cs]) when
        C == $.; C == $-; C == $_; C == $/;
        C == $?; C == $& ->
    [C | percent_escape(Cs)];
```

### percent_escape

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.
URI encode keys in the base layer of a message. Does not recurse.
Escape a list of characters as a URI-encoded string.
Escape a single byte as a URI-encoded string.

```erlang
percent_escape([C | Cs]) -> [escape_byte(C) | percent_escape(Cs)].
```

### escape_byte

Encode a string with escaped quotes.
Decode a string with escaped quotes.
Return a message with all of its keys decoded.
URI encode keys in the base layer of a message. Does not recurse.
Escape a list of characters as a URI-encoded string.
Escape a single byte as a URI-encoded string.

```erlang
escape_byte(C) when C >= 0, C =< 255 ->
    [$%, hex_digit(C bsr 4), hex_digit(C band 15)].
```

### hex_digit

```erlang
hex_digit(N) when N >= 0, N =< 9 ->
    N + $0;
```

### hex_digit

```erlang
hex_digit(N) when N > 9, N =< 15 ->
    N + $a - 10.
```

### percent_unescape

Unescape a URI-encoded string.

```erlang
percent_unescape([$%, H1, H2 | Cs]) ->
    Byte = (hex_value(H1) bsl 4) + hex_value(H2),
    [Byte | percent_unescape(Cs)];
```

### percent_unescape

Unescape a URI-encoded string.

```erlang
percent_unescape([C | Cs]) ->
    [C | percent_unescape(Cs)];
```

### percent_unescape

Unescape a URI-encoded string.

```erlang
percent_unescape([]) ->
    [].
```

### hex_value

```erlang
hex_value(C) when C >= $0, C =< $9 ->
    C - $0;
```

### hex_value

```erlang
hex_value(C) when C >= $a, C =< $f ->
    C - $a + 10;
```

### hex_value

```erlang
hex_value(C) when C >= $A, C =< $F ->
    C - $A + 10.
```

### escape_unescape_identity_test

```erlang
escape_unescape_identity_test() ->
    % Test that unescape(escape(X)) == X for various inputs
    TestCases = [
        <<"hello">>,
        <<"hello, world!">>,
        <<"hello+list">>,
        <<"special@chars#here">>,
        <<"UPPERCASE">>,
        <<"MixedCASEstring">>,
        <<"12345">>,
        <<>> % Empty string
    ],
    ?event(parsing,
        {escape_unescape_identity_test,
            {test_cases,
                [
                        {Case, {explicit, encode(Case)}}
                    ||
                        Case <- TestCases
                ]
            }
        }
    ),
    lists:foreach(fun(TestCase) ->
        ?assertEqual(TestCase, decode(encode(TestCase)))
    end, TestCases).
```

### unescape_specific_test

```erlang
unescape_specific_test() ->
    % Test specific unescape cases
    ?assertEqual(<<"a">>, decode(<<"%61">>)),
    ?assertEqual(<<"A">>, decode(<<"%41">>)),
    ?assertEqual(<<"!">>, decode(<<"%21">>)),
    ?assertEqual(<<"hello, World!">>, decode(<<"hello%2c%20%57orld%21">>)),
    ?assertEqual(<<"/">>, decode(<<"%2f">>)),
    ?assertEqual(<<"?">>, decode(<<"%3f">>)).
```

### uppercase_test

```erlang
uppercase_test() ->
    % Test uppercase characters are properly escaped
    ?assertEqual(<<"%41">>, encode(<<"A">>)),
    ?assertEqual(<<"%42">>, encode(<<"B">>)),
    ?assertEqual(<<"%5a">>, encode(<<"Z">>)),
    ?assertEqual(<<"hello%20%57orld">>, encode(<<"hello World">>)),
    ?assertEqual(<<"test%41%42%43">>, encode(<<"testABC">>)).
```

### escape_unescape_special_chars_test

```erlang
escape_unescape_special_chars_test() ->
    % Test characters that should be escaped
    SpecialChars = [
        $@, $#, $", $$, $%, $&, $', $(, $), $*, $+, $,, $/, $:, $;, 
        $<, $=, $>, $?, $[, $\\, $], $^, $`, ${, $|, $}, $~, $\s
    ],
    TestString = list_to_binary(SpecialChars),
```

---

*Generated from [hb_escape.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_escape.erl)*
