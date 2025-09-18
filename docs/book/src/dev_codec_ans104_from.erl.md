# dev_codec_ans104_from

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_ans104_from.erl)

Library functions for decoding ANS-104-style data items to TABM form.

---

## Exported Functions

- `base/5`
- `committed/5`
- `data/4`
- `fields/2`
- `tags/2`
- `with_commitments/5`

---

### fields

Library functions for decoding ANS-104-style data items to TABM form.
Return a TABM message containing the fields of the given decoded

```erlang
fields(Item, _Opts) ->
    case Item#tx.target of
        ?DEFAULT_TARGET -> #{};
        Target ->
            #{
                <<"target">> => hb_util:encode(Target)
            }
    end.
```

### tags

Return a TABM of the raw tags of the item, including all metadata

```erlang
tags(Item, Opts) ->
    Tags = hb_ao:normalize_keys(
        deduplicating_from_list(Item#tx.tags, Opts),
        Opts
    ),
    ao_types(Tags, Opts).
```

### ao_types

Ensure the encoded keys in the `ao-types` field are lowercased and

```erlang
ao_types(#{ <<"ao-types">> := AoTypes } = Tags, Opts) ->
    AOTypes = dev_codec_structured:decode_ao_types(AoTypes, Opts),
    % Normalize all keys in the ao-types map and re-encode
    NormAOTypes =
        maps:fold(
            fun(Key, Val, Acc) ->
                NormKey = hb_util:to_lower(hb_ao:normalize_key(Key)),
                Acc#{ NormKey => Val }
            end,
            #{},
            AOTypes
        ),
    EncodedAOTypes = dev_codec_structured:encode_ao_types(NormAOTypes, Opts),
    Tags#{ <<"ao-types">> := EncodedAOTypes };
```

### ao_types

Ensure the encoded keys in the `ao-types` field are lowercased and

```erlang
ao_types(Tags, _Opts) ->
    Tags.
```

### data

Return a TABM of the keys and values found in the data field of the item.

```erlang
data(Item, Req, Tags, Opts) ->
    % If the data field is empty, we return an empty map. If it is a map, we
    % return it as such. Otherwise, we return a map with the data key set to
    % the raw data value. This handles unbundling nested messages, as well as 
    % applying the `ao-data-key' tag if given.
```

### committed

Calculate the list of committed keys for an item, based on its 

```erlang
committed(Item, Fields, Tags, Data, Opts) ->
    hb_util:unique(
        data_keys(Data, Opts) ++
        tag_keys(Item, Opts) ++
        field_keys(Fields, Tags, Data, Opts)
    ).
```

### field_keys

Return the list of the keys from the fields TABM.

```erlang
field_keys(BaseFields, Tags, Data, Opts) ->
    HasTarget =
        hb_maps:is_key(<<"target">>, BaseFields, Opts) orelse
        hb_maps:is_key(<<"target">>, Tags, Opts) orelse
        hb_maps:is_key(<<"target">>, Data, Opts),
    case HasTarget of
        true -> [<<"target">>];
        false -> []
    end.
```

### data_keys

Return the list of the keys from the data TABM.

```erlang
data_keys(Data, Opts) ->
    hb_util:to_sorted_keys(Data, Opts).
```

### tag_keys

Return the list of the keys from the tags TABM. Filter all metadata

```erlang
tag_keys(Item, _Opts) ->
    MetaTags = [
        <<"bundle-format">>,
        <<"bundle-version">>,
        <<"bundle-map">>,
        <<"ao-data-key">>
    ],
    lists:filtermap(
        fun({Tag, _}) ->
            case lists:member(Tag, MetaTags) of
                true -> false;
                false -> {true, hb_util:to_lower(hb_ao:normalize_key(Tag))}
            end
        end,
        Item#tx.tags
    ).
```

### base

Return the complete message for an item, less its commitments. The

```erlang
base(CommittedKeys, Fields, Tags, Data, Opts) ->
    hb_maps:from_list(
        lists:map(
            fun(Key) ->
                case hb_maps:find(Key, Data, Opts) of
                    error ->
                        case hb_maps:find(Key, Fields, Opts) of
                            error ->
                                case hb_maps:find(Key, Tags, Opts) of
                                    error -> throw({missing_key, Key});
                                    {ok, Value} -> {Key, Value}
                                end;
                            {ok, Value} -> {Key, Value}
                        end;
                    {ok, Value} -> {Key, Value}
                end
            end,
            CommittedKeys
        )
    ).
```

### with_commitments

Return a message with the appropriate commitments added to it.

```erlang
with_commitments(Item, Tags, Base, CommittedKeys, Opts) ->
    case Item#tx.signature of
        ?DEFAULT_SIG ->
            case normal_tags(Item#tx.tags) of
                true -> Base;
                false ->
                    with_unsigned_commitment(Item, Tags, Base, CommittedKeys, Opts)
            end;
        _ -> with_signed_commitment(Item, Tags, Base, CommittedKeys, Opts)
    end.
```

### with_unsigned_commitment

Returns a commitments message for an item, containing an unsigned

```erlang
with_unsigned_commitment(Item, Tags, UncommittedMessage, CommittedKeys, Opts) ->
    ID = hb_util:human_id(Item#tx.unsigned_id),
    UncommittedMessage#{
        <<"commitments">> => #{
            ID =>
                filter_unset(
                    #{
                        <<"commitment-device">> => <<"ans104@1.0">>,
                        <<"committed">> => CommittedKeys,
                        <<"type">> => <<"unsigned-sha256">>,
                        <<"bundle">> => bundle_commitment_key(Tags, Opts),
                        <<"original-tags">> => original_tags(Item, Opts),
                        <<"field-target">> =>
                            case Item#tx.target of
                                ?DEFAULT_TARGET -> unset;
                                Target -> hb_util:encode(Target)
                            end,
                        <<"field-anchor">> =>
                            case Item#tx.anchor of
                                ?DEFAULT_LAST_TX -> unset;
                                LastTX -> LastTX
                            end
                    },
                    Opts
                )
        }
    }.
```

### with_signed_commitment

Returns a commitments message for an item, containing a signed

```erlang
with_signed_commitment(Item, Tags, UncommittedMessage, CommittedKeys, Opts) ->
    Address = hb_util:human_id(ar_wallet:to_address(Item#tx.owner)),
    ID = hb_util:human_id(Item#tx.id),
    Commitment =
        filter_unset(
            #{
                <<"commitment-device">> => <<"ans104@1.0">>,
                <<"committer">> => Address,
                <<"committed">> => CommittedKeys,
                <<"signature">> => hb_util:encode(Item#tx.signature),
                <<"keyid">> =>
                    <<"publickey:", (hb_util:encode(Item#tx.owner))/binary>>,
                <<"type">> => <<"rsa-pss-sha256">>,
                <<"bundle">> => bundle_commitment_key(Tags, Opts),
                <<"original-tags">> => original_tags(Item, Opts),
                <<"field-anchor">> =>
                    case Item#tx.anchor of
                        ?DEFAULT_LAST_TX -> unset;
                        LastTX -> LastTX
                    end,
                <<"field-target">> =>
                    case Item#tx.target of
                        ?DEFAULT_TARGET -> unset;
                        Target -> hb_util:encode(Target)
                    end
            },
            Opts
        ),
    UncommittedMessage#{
        <<"commitments">> => #{
            ID => Commitment
        }
    }.
```

### bundle_commitment_key

Return the bundle key for an item.
Check whether a list of key-value pairs contains only normalized keys.

```erlang
bundle_commitment_key(Tags, Opts) ->
    hb_util:bin(hb_maps:is_key(<<"bundle-format">>, Tags, Opts)).
```

### normal_tags

Return the bundle key for an item.
Check whether a list of key-value pairs contains only normalized keys.

```erlang
normal_tags(Tags) ->
    lists:all(
        fun({Key, _}) ->
            hb_util:to_lower(hb_ao:normalize_key(Key)) =:= Key
        end,
        Tags
    ).
```

### original_tags

Return the original tags of an item if it is applicable. Otherwise,

```erlang
original_tags(Item, _Opts) ->
    case normal_tags(Item#tx.tags) of
        true -> unset;
        false -> encoded_tags_to_map(Item#tx.tags)
    end.
```

### encoded_tags_to_map

Convert an ANS-104 encoded tag list into a HyperBEAM-compatible map.

```erlang
encoded_tags_to_map(Tags) ->
    hb_util:list_to_numbered_message(
        lists:map(
            fun({Key, Value}) ->
                #{
                    <<"name">> => Key,
                    <<"value">> => Value
                }
            end,
            Tags
        )
    ).
```

### filter_unset

Remove all undefined values from a map.

```erlang
filter_unset(Map, Opts) ->
    hb_maps:filter(
        fun(_, Value) ->
            case Value of
                unset -> false;
                _ -> true
            end
        end,
        Map,
        Opts
    ).
```

### deduplicating_from_list

Deduplicate a list of key-value pairs by key, generating a list of

```erlang
deduplicating_from_list(Tags, Opts) ->
    % Aggregate any duplicated tags into an ordered list of values.
```

---

*Generated from [dev_codec_ans104_from.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_ans104_from.erl)*
