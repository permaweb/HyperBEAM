# hb_crypto

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_crypto.erl)

Implements the cryptographic functions and wraps the primitives
used in HyperBEAM. Abstracted such that this (extremely!) dangerous code 
can be carefully managed.
HyperBEAM currently implements two hashpath algorithms:
* `sha-256-chain`: A simple chained SHA-256 hash.
* `accumulate-256`: A SHA-256 hash that chains the given IDs and accumulates
  their values into a single commitment.
The accumulate algorithm is experimental and at this point only exists to
allow us to test multiple HashPath algorithms in HyperBEAM.

---

## Exported Functions

- `accumulate/1`
- `accumulate/2`
- `pbkdf2/5`
- `sha256_chain/2`
- `sha256/1`

---

### sha256_chain

Implements the cryptographic functions and wraps the primitives
Add a new ID to the end of a SHA-256 hash chain.

```erlang
sha256_chain(ID1, ID2) when ?IS_ID(ID1) ->
    sha256(<<ID1:32/binary, ID2/binary>>);
```

### sha256_chain

Implements the cryptographic functions and wraps the primitives
Add a new ID to the end of a SHA-256 hash chain.

```erlang
sha256_chain(ID1, ID2) ->
    throw({cannot_chain_bad_ids, ID1, ID2}).
```

### accumulate

Accumulate two IDs, or a list of IDs, into a single commitment. This 

```erlang
accumulate(IDs) when is_list(IDs) ->
    lists:foldl(fun accumulate/2, << 0:256 >>, IDs).
```

### accumulate

```erlang
accumulate(ID1 = << ID1Int:256 >>, ID2 = << ID2Int:256 >>)
        when (byte_size(ID1) =:= 32) and (byte_size(ID2) =:= 32) ->
    << (ID1Int + ID2Int):256 >>;
```

### accumulate

```erlang
accumulate(ID1, ID2) ->
    throw({cannot_accumulate_bad_ids, ID1, ID2}).
```

### sha256

Wrap Erlang's `crypto:hash/2` to provide a standard interface.

```erlang
sha256(Data) ->
    crypto:hash(sha256, Data).
```

### pbkdf2

Wrap Erlang's `crypto:pbkdf2_hmac/5` to provide a standard interface.

```erlang
pbkdf2(Alg, Password, Salt, Iterations, KeyLength) ->
    case crypto:pbkdf2_hmac(Alg, Password, Salt, Iterations, KeyLength) of
        Key when is_binary(Key) -> {ok, Key};
        {Tag, CFileInfo, Desc} ->
            ?event(
                {pbkdf2_error,
                    {tag, Tag},
                    {desc, Desc},
                    {c_file_info, CFileInfo}
                }
            ),
            {error, Desc}
    end.
```

### count_zeroes

Count the number of leading zeroes in a bitstring.

```erlang
count_zeroes(<<>>) ->
    0;
```

### count_zeroes

Count the number of leading zeroes in a bitstring.

```erlang
count_zeroes(<<0:1, Rest/bitstring>>) ->
    1 + count_zeroes(Rest);
```

### count_zeroes

Count the number of leading zeroes in a bitstring.

```erlang
count_zeroes(<<_:1, Rest/bitstring>>) ->
    count_zeroes(Rest).
```

### sha256_chain_test

Check that `sha-256-chain` correctly produces a hash matching

```erlang
sha256_chain_test() ->
    ID1 = <<1:256>>,
    ID2 = <<2:256>>,
    ID3 = sha256_chain(ID1, ID2),
    HashBase = << ID1/binary, ID2/binary >>,
    ?assertEqual(ID3, crypto:hash(sha256, HashBase)),
    % Basic entropy check.
```

---

*Generated from [hb_crypto.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_crypto.erl)*
