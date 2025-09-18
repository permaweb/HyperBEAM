# ar_tx

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/ar_tx.erl)

The module with utilities for transaction creation, signing, and verification.

---

## Exported Functions

- `json_struct_to_tx/1`
- `new/4`
- `new/5`
- `sign/2`
- `tx_to_json_struct/1`
- `verify_tx_id/2`
- `verify/1`

---

### new

The module with utilities for transaction creation, signing, and verification.
Create a new transaction.

```erlang
new(Dest, Reward, Qty, Last) ->
    #tx{
        id = crypto:strong_rand_bytes(32),
        anchor = Last,
        quantity = Qty,
        target = Dest,
        data = <<>>,
        data_size = 0,
        reward = Reward
    }.
```

### new

```erlang
new(Dest, Reward, Qty, Last, SigType) ->
    #tx{
        id = crypto:strong_rand_bytes(32),
        anchor = Last,
        quantity = Qty,
        target = Dest,
        data = <<>>,
        data_size = 0,
        reward = Reward,
        signature_type = SigType
    }.
```

### sign

Cryptographically sign (claim ownership of) a transaction.

```erlang
sign(TX, {PrivKey, {KeyType, Owner}}) ->
    NewTX = TX#tx{ owner = Owner, signature_type = KeyType },
    Sig = ar_wallet:sign(PrivKey, signature_data_segment(NewTX)),
    ID = crypto:hash(sha256, <<Sig/binary>>),
    NewTX#tx{ id = ID, signature = Sig }.
```

### verify

Verify whether a transaction is valid.

```erlang
verify(TX) ->
    do_verify(TX, verify_signature).
```

### verify_tx_id

Verify the given transaction actually has the given identifier.

```erlang
verify_tx_id(ExpectedID, #tx{ id = ID } = TX) ->
    ExpectedID == ID andalso verify_signature(TX, verify_signature) andalso verify_hash(TX).
```

### signature_data_segment

Generate the data segment to be signed for a given TX.

```erlang
signature_data_segment(TX) ->
    List = [
        << (integer_to_binary(TX#tx.format))/binary >>,
        << (TX#tx.owner)/binary >>,
        << (TX#tx.target)/binary >>,
        << (list_to_binary(integer_to_list(TX#tx.quantity)))/binary >>,
        << (list_to_binary(integer_to_list(TX#tx.reward)))/binary >>,
        << (TX#tx.anchor)/binary >>,
        << (integer_to_binary(TX#tx.data_size))/binary >>,
        << (TX#tx.data_root)/binary >>
    ],
    ar_deep_hash:hash(List).
```

### verify_signature

Verify the transaction's signature.

```erlang
verify_signature(TX = #tx{ signature_type = SigType }, verify_signature) ->
    SignatureDataSegment = signature_data_segment(TX),
    ar_wallet:verify({SigType, TX#tx.owner}, SignatureDataSegment, TX#tx.signature).
```

### verify_hash

Verify that the transaction's ID is a hash of its signature.

```erlang
verify_hash(#tx{ signature = Sig, id = ID }) ->
    ID == crypto:hash(sha256, << Sig/binary >>).
```

### do_verify

Verify transaction.

```erlang
do_verify(TX, VerifySignature) ->
    From = ar_wallet:to_address(TX#tx.owner, TX#tx.signature_type),
    Checks = [
        {"quantity_negative", TX#tx.quantity >= 0},
        {"same_owner_as_target", (From =/= TX#tx.target)},
        {"tx_id_not_valid", verify_hash(TX)},
        {"tx_signature_not_valid", verify_signature(TX, VerifySignature)},
        {"tx_data_size_negative", TX#tx.data_size >= 0},
        {"tx_data_size_data_root_mismatch", (TX#tx.data_size == 0) == (TX#tx.data_root == <<>>)}
    ],
    collect_validation_results(TX#tx.id, Checks).
```

### collect_validation_results

```erlang
collect_validation_results(_TXID, Checks) ->
    KeepFailed = fun
        ({_, true}) -> false;
        ({ErrorCode, false}) -> {true, ErrorCode}
    end,
    case lists:filtermap(KeepFailed, Checks) of
        [] -> true;
        _ -> false
    end.
```

### json_struct_to_tx

```erlang
json_struct_to_tx(TXStruct) ->
    Tags =
        case hb_util:find_value(<<"tags">>, TXStruct) of
            undefined ->
                [];
            Xs ->
                Xs
        end,
    Data = hb_util:decode(hb_util:find_value(<<"data">>, TXStruct)),
    Format =
        case hb_util:find_value(<<"format">>, TXStruct) of
            undefined ->
                1;
            N when is_integer(N) ->
                N;
            N when is_binary(N) ->
                binary_to_integer(N)
        end,
    Denomination =
        case hb_util:find_value(<<"denomination">>, TXStruct) of
            undefined ->
                0;
            EncodedDenomination ->
                MaybeDenomination = binary_to_integer(EncodedDenomination),
                true = MaybeDenomination > 0,
                MaybeDenomination
        end,
    TXID = hb_util:decode(hb_util:find_value(<<"id">>, TXStruct)),
    32 = byte_size(TXID),
    #tx{
        format = Format,
        id = TXID,
        anchor = hb_util:decode(hb_util:find_value(<<"anchor">>, TXStruct)),
        owner = hb_util:decode(hb_util:find_value(<<"owner">>, TXStruct)),
        tags = [{hb_util:decode(Name), hb_util:decode(Value)}
                %% Only the elements matching this pattern are included in the list.
```

### tx_to_json_struct

```erlang
tx_to_json_struct(
    #tx{
        id = ID,
        format = Format,
        anchor = Anchor,
        owner = Owner,
        tags = Tags,
        target = Target,
        quantity = Quantity,
        data = Data,
        reward = Reward,
        signature = Sig,
        data_size = DataSize,
        data_root = DataRoot,
        denomination = Denomination
    }) ->
    Fields = [
        {format,
            case Format of
                undefined ->
                    1;
                _ ->
                    Format
            end},
        {id, hb_util:encode(ID)},
        {anchor, hb_util:encode(Anchor)},
        {owner, hb_util:encode(Owner)},
        {tags,
            lists:map(
                fun({Name, Value}) ->
                    {
                        [
                            {name, hb_util:encode(Name)},
                            {value, hb_util:encode(Value)}
                        ]
                    }
                end,
                Tags
            )
        },
        {target, hb_util:encode(Target)},
        {quantity, integer_to_binary(Quantity)},
        {data, hb_util:encode(Data)},
        {data_size, integer_to_binary(DataSize)},
        {data_tree, []},
        {data_root, hb_util:encode(DataRoot)},
        {reward, integer_to_binary(Reward)},
        {signature, hb_util:encode(Sig)}
    ],
    Fields2 =
        case Denomination > 0 of
            true ->
                Fields ++ [{denomination, integer_to_binary(Denomination)}];
            false ->
                Fields
        end,
```

---

*Generated from [ar_tx.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/ar_tx.erl)*
