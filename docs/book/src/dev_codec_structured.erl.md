# dev_codec_structured

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_structured.erl)

A device implementing the codec interface (to/1, from/1) for 
HyperBEAM's internal, richly typed message format. Supported rich types are:
- `integer`
- `float`
- `atom`
- `list`
Encoding to TABM can be limited to a subset of types (with other types
passing through in their rich representation) by specifying the types 
that should be encoded with the `encode-types` request key.
This format mirrors HTTP Structured Fields, aside from its limitations of 
compound type depths, as well as limited floating point representations.
As with all AO-Core codecs, its target format (the format it expects to 
receive in the `to/1` function, and give in `from/1`) is TABM.
For more details, see the HTTP Structured Fields (RFC-9651) specification.

---

## Exported Functions

- `commit/3`
- `decode_ao_types/2`
- `decode_value/2`
- `encode_ao_types/2`
- `encode_value/1`
- `from/3`
- `implicit_keys/2`
- `is_list_from_ao_types/2`
- `to/3`
- `verify/3`

---

### commit

A device implementing the codec interface (to/1, from/1) for 

```erlang
commit(Msg, Req, Opts) -> dev_codec_httpsig:commit(Msg, Req, Opts).
```

### verify

A device implementing the codec interface (to/1, from/1) for 
Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).

```erlang
verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).
```

### from

A device implementing the codec interface (to/1, from/1) for 
Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).

```erlang
from(Bin, _Req, _Opts) when is_binary(Bin) -> {ok, Bin};
```

### from

A device implementing the codec interface (to/1, from/1) for 
Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).

```erlang
from(List, Req, Opts) when is_list(List) ->
    % Encode the list as a map, then -- if our request indicates that we are
    % encoding lists -- add the `.' key to the `ao-types' field, indicating
    % that this message is a list and return. Otherwise, if the downstream
    % encoding did not set its own `ao-types' field, we convert the message
    % back to a list.
```

### from

```erlang
from(Msg, Req, Opts) when is_map(Msg) ->
    % Normalize the message, offloading links to the cache.
```

### from

Find the types that should be encoded from the request and options.

```erlang
from(Other, _Req, _Opts) -> {ok, hb_path:to_binary(Other)}.
```

### find_encode_types

Find the types that should be encoded from the request and options.
Determine the type for a value.

```erlang
find_encode_types(Req, Opts) ->
    hb_maps:get(<<"encode-types">>, Req, ?SUPPORTED_TYPES, Opts).
```

### type

Find the types that should be encoded from the request and options.
Determine the type for a value.

```erlang
type(Int) when is_integer(Int) -> <<"integer">>;
```

### type

Find the types that should be encoded from the request and options.
Determine the type for a value.

```erlang
type(Float) when is_float(Float) -> <<"float">>;
```

### type

Find the types that should be encoded from the request and options.
Determine the type for a value.

```erlang
type(Atom) when is_atom(Atom) -> <<"atom">>;
```

### type

Find the types that should be encoded from the request and options.
Determine the type for a value.

```erlang
type(List) when is_list(List) -> <<"list">>;
```

### type

Find the types that should be encoded from the request and options.
Determine the type for a value.
Discern the linkify mode from the request and the options.

```erlang
type(Other) -> Other.
```

### linkify_mode

Find the types that should be encoded from the request and options.
Determine the type for a value.
Discern the linkify mode from the request and the options.

```erlang
linkify_mode(Req, Opts) ->
    case hb_maps:get(<<"bundle">>, Req, not_found, Opts) of
        not_found -> hb_opts:get(linkify_mode, offload, Opts);
    	true ->
            % The request is asking for a bundle, so we should _not_ linkify.
```

### to

Convert a TABM into a native HyperBEAM message.

```erlang
to(Bin, _Req, _Opts) when is_binary(Bin) -> {ok, Bin};
```

### to

Convert a TABM into a native HyperBEAM message.

```erlang
to(TABM0, Req, Opts) when is_list(TABM0) ->
    % If we receive a list, we convert it to a message and run `to/3' on it. 
```

### to

```erlang
to(TABM0, Req, Opts) ->
    Types = decode_ao_types(TABM0, Opts),
    % Decode all links to their HyperBEAM-native, resolvable form.
```

### encode_ao_types

Generate an `ao-types` structured field from a map of keys and their

```erlang
encode_ao_types(Types, _Opts) ->
    iolist_to_binary(hb_structured_fields:dictionary(
        lists:map(
            fun(Key) ->
                {ok, Item} = hb_structured_fields:to_item(maps:get(Key, Types)),
                {hb_escape:encode(Key), Item}
            end,
            hb_util:to_sorted_keys(Types)
        )
    )).
```

### decode_ao_types

Parse the `ao-types` field of a TABM if present, and return a map of

```erlang
decode_ao_types(List, _Opts) when is_list(List) -> #{};
```

### decode_ao_types

Parse the `ao-types` field of a TABM if present, and return a map of

```erlang
decode_ao_types(Msg, Opts) when is_map(Msg) ->
    decode_ao_types(hb_maps:get(<<"ao-types">>, Msg, <<>>, Opts), Opts);
```

### decode_ao_types

Parse the `ao-types` field of a TABM if present, and return a map of

```erlang
decode_ao_types(Bin, _Opts) when is_binary(Bin) ->
    hb_maps:from_list(
        lists:map(
            fun({Key, {item, {_, Value}, _}}) ->
                {hb_escape:decode(Key), Value}
            end,
            hb_structured_fields:parse_dictionary(Bin)    
        )
    ).
```

### is_list_from_ao_types

Determine if the `ao-types` field of a TABM indicates that the message

```erlang
is_list_from_ao_types(Types, Opts) when is_binary(Types) ->
    is_list_from_ao_types(decode_ao_types(Types, Opts), Opts);
```

### is_list_from_ao_types

Determine if the `ao-types` field of a TABM indicates that the message

```erlang
is_list_from_ao_types(Types, _Opts) ->
    case maps:find(<<".">>, Types) of
        {ok, <<"list">>} -> true;
        _ -> false
    end.
```

### implicit_keys

Find the implicit keys of a TABM.

```erlang
implicit_keys(Req, Opts) ->
    hb_maps:keys(
        hb_maps:filtermap(
            fun(_Key, Val = <<"empty-", _/binary>>) -> {true, Val};
            (_Key, _Val) -> false
            end,
            decode_ao_types(Req, Opts),
            Opts
        ),
		Opts
    ).
```

### maybe_encode_value

Encode a value if it is in the list of supported types.

```erlang
maybe_encode_value(Value, EncodeTypes) ->
    case lists:member(type(Value), EncodeTypes) of
        true -> encode_value(Value);
        false -> skip
    end.
```

### encode_value

Convert a term to a binary representation, emitting its type for

```erlang
encode_value(Value) when is_integer(Value) ->
    [Encoded, _] = hb_structured_fields:item({item, Value, []}),
    {<<"integer">>, Encoded};
```

### encode_value

Convert a term to a binary representation, emitting its type for

```erlang
encode_value(Value) when is_float(Value) ->
    ?no_prod("Must use structured field representation for floats!"),
    {<<"float">>, float_to_binary(Value)};
```

### encode_value

Convert a term to a binary representation, emitting its type for

```erlang
encode_value(Value) when is_atom(Value) ->
    EncodedIOList =
        hb_structured_fields:item({item, {token, hb_util:bin(Value)}, []}),
    Encoded = hb_util:bin(EncodedIOList),
    {<<"atom">>, Encoded};
```

### encode_value

Convert a term to a binary representation, emitting its type for

```erlang
encode_value(Values) when is_list(Values) ->
    EncodedValues =
        lists:map(
            fun(Bin) when is_binary(Bin) -> {item, {string, Bin}, []};
               (Item) ->
                {RawType, Encoded} = encode_value(Item),
                Type = hb_ao:normalize_key(RawType),
                {
                    item,
                    {
                        string,
                        <<
                            "(ao-type-", Type/binary, ") ",
                            Encoded/binary
                        >>
                    },
                    []
                }
            end,
            Values
        ),
    EncodedList = hb_structured_fields:list(EncodedValues),
    {<<"list">>, iolist_to_binary(EncodedList)};
```

### encode_value

Convert a term to a binary representation, emitting its type for

```erlang
encode_value(Value) when is_binary(Value) ->
    {<<"binary">>, Value};
```

### encode_value

Convert a term to a binary representation, emitting its type for

```erlang
encode_value(Value) ->
    Value.
```

### decode_value

Convert non-binary values to binary for serialization.

```erlang
decode_value(Type, Value) when is_list(Type) ->
    decode_value(list_to_binary(Type), Value);
```

### decode_value

Convert non-binary values to binary for serialization.

```erlang
decode_value(Type, Value) when is_binary(Type) ->
    ?event({decoding, {type, Type}, {value, Value}}),
    decode_value(
        binary_to_existing_atom(
            list_to_binary(string:to_lower(binary_to_list(Type))),
            latin1
        ),
        Value
    );
```

### decode_value

Convert non-binary values to binary for serialization.

```erlang
decode_value(integer, Value) ->
    {item, Number, _} = hb_structured_fields:parse_item(Value),
    Number;
```

### decode_value

Convert non-binary values to binary for serialization.

```erlang
decode_value(float, Value) ->
    binary_to_float(Value);
```

### decode_value

Convert non-binary values to binary for serialization.

```erlang
decode_value(atom, Value) ->
    {item, {_, AtomString}, _} =
        hb_structured_fields:parse_item(Value),
    hb_util:atom(AtomString);
```

### decode_value

Convert non-binary values to binary for serialization.

```erlang
decode_value(list, Value) when is_binary(Value) ->
    lists:map(
        fun({item, {string, <<"(ao-type-", Rest/binary>>}, _}) ->
            [Type, Item] = binary:split(Rest, <<") ">>),
            decode_value(Type, Item);
           ({item, Item, _}) -> hb_structured_fields:from_bare_item(Item)
        end,
        hb_structured_fields:parse_list(iolist_to_binary(Value))
    );
```

### decode_value

Convert non-binary values to binary for serialization.

```erlang
decode_value(list, Value) when is_map(Value) ->
    hb_util:message_to_ordered_list(Value);
```

### decode_value

Convert non-binary values to binary for serialization.

```erlang
decode_value(map, Value) ->
    hb_maps:from_list(
        lists:map(
            fun({Key, {item, Item, _}}) ->
                ?event({decoded_item, {explicit, Key}, Item}),
                {Key, hb_structured_fields:from_bare_item(Item)}
            end,
            hb_structured_fields:parse_dictionary(iolist_to_binary(Value))
        )
    );
```

### decode_value

Convert non-binary values to binary for serialization.

```erlang
decode_value(BinType, Value) when is_binary(BinType) ->
    decode_value(
        list_to_existing_atom(
            string:to_lower(
                binary_to_list(BinType)
            )
        ),
        Value
    );
```

### decode_value

Convert non-binary values to binary for serialization.

```erlang
decode_value(OtherType, Value) ->
    ?event({unexpected_type, OtherType, Value}),
    throw({unexpected_type, OtherType, Value}).
```

### list_encoding_test

```erlang
list_encoding_test() ->
    % Test that we can encode and decode a list of integers.
```

---

*Generated from [dev_codec_structured.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_structured.erl)*
