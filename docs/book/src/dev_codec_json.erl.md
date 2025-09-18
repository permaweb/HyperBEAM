# dev_codec_json

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_json.erl)

A simple JSON codec for HyperBEAM's message format. Takes a
message as TABM and returns an encoded JSON string representation.
This codec utilizes the httpsig@1.0 codec for signing and verifying.

---

## Exported Functions

- `commit/3`
- `committed/3`
- `content_type/1`
- `deserialize/3`
- `from/3`
- `serialize/3`
- `to/3`
- `verify/3`

---

### content_type

A simple JSON codec for HyperBEAM's message format. Takes a
Return the content type for the codec.
Encode a message to a JSON string, using JSON-native typing.

```erlang
content_type(_) -> {ok, <<"application/json">>}.
```

### to

A simple JSON codec for HyperBEAM's message format. Takes a
Return the content type for the codec.
Encode a message to a JSON string, using JSON-native typing.

```erlang
to(Msg, _Req, _Opts) when is_binary(Msg) ->
    {ok, hb_util:bin(json:encode(Msg))};
```

### to

A simple JSON codec for HyperBEAM's message format. Takes a
Return the content type for the codec.
Encode a message to a JSON string, using JSON-native typing.

```erlang
to(Msg, Req, Opts) ->
    % The input to this function will be a TABM message, so we:
    % 1. Convert it to a structured message.
```

### from

Decode a JSON string to a message.

```erlang
from(Map, _Req, _Opts) when is_map(Map) -> {ok, Map};
```

### from

Decode a JSON string to a message.

```erlang
from(JSON, _Req, Opts) ->
    % The JSON string will be a partially-TABM encoded message: Rich number
    % and list types, but no `atom's. Subsequently, we convert it to a fully
    % structured message after decoding, then turn the result back into a TABM.
```

### commit

```erlang
commit(Msg, Req, Opts) -> dev_codec_httpsig:commit(Msg, Req, Opts).
```

### verify

```erlang
verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).
```

### committed

```erlang
committed(Msg, Req, Opts) when is_binary(Msg) ->
    committed(hb_util:ok(from(Msg, Req, Opts)), Req, Opts);
```

### committed

```erlang
committed(Msg, _Req, Opts) ->
    hb_message:committed(Msg, all, Opts).
```

### deserialize

Deserialize the JSON string found at the given path.

```erlang
deserialize(Base, Req, Opts) ->
    Payload = 
        hb_ao:get(
            Target =
                hb_ao:get(
                    <<"target">>,
                    Req,
                    <<"body">>,
                    Opts
                ),
            Base,
            Opts
        ),
    case Payload of
        not_found -> {error, #{
            <<"status">> => 404,
            <<"body">> =>
                <<
                    "JSON payload not found in the base message.",
                    "Searched for: ", Target/binary
                >>
            }};
        _ ->
            from(Payload, Req, Opts)
    end.
```

### serialize

Serialize a message to a JSON string.

```erlang
serialize(Base, Msg, Opts) ->
    {ok,
        #{
            <<"content-type">> => <<"application/json">>,
            <<"body">> => hb_util:ok(to(Base, Msg, Opts))
        }
    }.
```

### decode_with_atom_test

```erlang
decode_with_atom_test() ->
    JSON =
        <<"""
        [
            {
                "store-module": "hb_store_fs",
                "name": "cache-TEST/json-test-store",
                "ao-types": "store-module=\"atom\""
            }
        ]
        """>>,
    Msg = hb_message:convert(JSON, <<"structured@1.0">>, <<"json@1.0">>, #{}),
    ?assertMatch(
        [#{ <<"store-module">> := hb_store_fs }|_],
        hb_cache:ensure_all_loaded(Msg, #{})
```

---

*Generated from [dev_codec_json.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_json.erl)*
