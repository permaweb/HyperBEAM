# hb_json

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_json.erl)

Wrapper for encoding and decoding JSON. Supports maps and Jiffy's old 
`ejson` format. This module abstracts the underlying JSON library, allowing
us to switch between libraries as needed in the future.

---

## Exported Functions

- `decode/1`
- `decode/2`
- `encode/1`

---

### encode

Wrapper for encoding and decoding JSON. Supports maps and Jiffy's old 
Takes a term in Erlang's native form and encodes it as a JSON string.

```erlang
encode(Term) ->
    iolist_to_binary(json:encode(Term)).
```

### decode

Takes a JSON string and decodes it into an Erlang term.

```erlang
decode(Bin) -> json:decode(Bin).
```

---

*Generated from [hb_json.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_json.erl)*
