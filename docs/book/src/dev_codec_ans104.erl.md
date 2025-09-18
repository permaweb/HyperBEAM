# dev_codec_ans104

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_ans104.erl)

Codec for managing transformations from `ar_bundles`-style Arweave TX
records to and from TABMs.

---

## Exported Functions

- `commit/3`
- `content_type/1`
- `deserialize/3`
- `from/3`
- `serialize/3`
- `to/3`
- `verify/3`

---

### content_type

Codec for managing transformations from `ar_bundles`-style Arweave TX
Return the content type for the codec.
Serialize a message or TX to a binary.

```erlang
content_type(_) -> {ok, <<"application/ans104">>}.
```

### serialize

Codec for managing transformations from `ar_bundles`-style Arweave TX
Return the content type for the codec.
Serialize a message or TX to a binary.

```erlang
serialize(Msg, Req, Opts) when is_map(Msg) ->
    serialize(to(Msg, Req, Opts), Req, Opts);
```

### serialize

Codec for managing transformations from `ar_bundles`-style Arweave TX
Return the content type for the codec.
Serialize a message or TX to a binary.

```erlang
serialize(TX, _Req, _Opts) when is_record(TX, tx) ->
    {ok, ar_bundles:serialize(TX)}.
```

### deserialize

Deserialize a binary ans104 message to a TABM.

```erlang
deserialize(#{ <<"body">> := Binary }, Req, Opts) ->
    deserialize(Binary, Req, Opts);
```

### deserialize

Deserialize a binary ans104 message to a TABM.

```erlang
deserialize(Binary, Req, Opts) when is_binary(Binary) ->
    deserialize(ar_bundles:deserialize(Binary), Req, Opts);
```

### deserialize

Deserialize a binary ans104 message to a TABM.

```erlang
deserialize(TX, Req, Opts) when is_record(TX, tx) ->
    from(TX, Req, Opts).
```

### commit

Sign a message using the `priv_wallet` key in the options. Supports both

```erlang
commit(Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"unsigned-sha256">> }, Opts);
```

### commit

Sign a message using the `priv_wallet` key in the options. Supports both

```erlang
commit(Msg, Req = #{ <<"type">> := <<"signed">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"rsa-pss-sha256">> }, Opts);
```

### commit

Sign a message using the `priv_wallet` key in the options. Supports both

```erlang
commit(Msg, Req = #{ <<"type">> := <<"rsa-pss-sha256">> }, Opts) ->
    % Convert the given message to an ANS-104 TX record, sign it, and convert
    % it back to a structured message.
```

### commit

```erlang
commit(Msg, #{ <<"type">> := <<"unsigned-sha256">> }, Opts) ->
    % Remove the commitments from the message, convert it to ANS-104, then back.
```

### verify

Verify an ANS-104 commitment.

```erlang
verify(Msg, Req, Opts) ->
    ?event({verify, {base, Msg}, {req, Req}}),
    OnlyWithCommitment =
        hb_private:reset(
            hb_message:with_commitments(
                Req,
                Msg,
                Opts
            )
        ),
    ?event({verify, {only_with_commitment, OnlyWithCommitment}}),
    {ok, TX} = to(OnlyWithCommitment, Req, Opts),
    ?event({verify, {encoded, TX}}),
    Res = ar_bundles:verify_item(TX),
    {ok, Res}.
```

### from

Convert a #tx record into a message map recursively.

```erlang
from(Binary, _Req, _Opts) when is_binary(Binary) -> {ok, Binary};
```

### from

Convert a #tx record into a message map recursively.

```erlang
from(TX, Req, Opts) when is_record(TX, tx) ->
    case lists:keyfind(<<"ao-type">>, 1, TX#tx.tags) of
        false ->
            do_from(TX, Req, Opts);
        {<<"ao-type">>, <<"binary">>} ->
            {ok, TX#tx.data}
    end.
```

### do_from

```erlang
do_from(RawTX, Req, Opts) ->
    % Ensure the TX is fully deserialized.
```

### to

Internal helper to translate a message to its #tx record representation,

```erlang
to(Binary, _Req, _Opts) when is_binary(Binary) ->
    % ar_bundles cannot serialize just a simple binary or get an ID for it, so
    % we turn it into a TX record with a special tag, tx_to_message will
    % identify this tag and extract just the binary.
```

### to

```erlang
to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
```

### to

```erlang
to(RawTABM, Req, Opts) when is_map(RawTABM) ->
    % Ensure that the TABM is fully loaded if the `bundle` key is set to true.
```

### to

```erlang
to(Other, _Req, _Opts) ->
    throw({invalid_tx, Other}).
```

### normal_tags_test

```erlang
normal_tags_test() ->
    Msg = #{
        <<"first-tag">> => <<"first-value">>,
        <<"second-tag">> => <<"second-value">>
    },
    {ok, Encoded} = to(Msg, #{}, #{}),
    ?event({encoded, Encoded}),
    {ok, Decoded} = from(Encoded, #{}, #{}),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, Decoded)).
```

### from_maintains_tag_name_case_test

```erlang
from_maintains_tag_name_case_test() ->
    TX = #tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>}
        ]
    },
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    ?event({signed_tx, SignedTX}),
    ?assert(ar_bundles:verify_item(SignedTX)),
    TABM = hb_util:ok(from(SignedTX, #{}, #{})),
    ?event({tabm, TABM}),
    ConvertedTX = hb_util:ok(to(TABM, #{}, #{})),
    ?event({converted_tx, ConvertedTX}),
    ?assert(ar_bundles:verify_item(ConvertedTX)),
    ?assertEqual(ConvertedTX, ar_bundles:normalize(SignedTX)).
```

### restore_tag_name_case_from_cache_test

```erlang
restore_tag_name_case_from_cache_test() ->
    Opts = #{ store => hb_test_utils:test_store() },
    TX = #tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag-2">>, <<"test-value-2">>}
        ]
    },
    SignedTX = ar_bundles:sign_item(TX, ar_wallet:new()),
    SignedMsg =
        hb_message:convert(
            SignedTX,
            <<"structured@1.0">>,
            <<"ans104@1.0">>,
            Opts
        ),
    SignedID = hb_message:id(SignedMsg, all),
    ?event({signed_msg, SignedMsg}),
    OnlyCommitted = hb_message:with_only_committed(SignedMsg, Opts),
    ?event({only_committed, OnlyCommitted}),
    {ok, ID} = hb_cache:write(SignedMsg, Opts),
    ?event({id, ID}),
    {ok, ReadMsg} = hb_cache:read(SignedID, Opts),
    ?event({restored_msg, ReadMsg}),
    {ok, ReadTX} = to(ReadMsg, #{}, Opts),
    ?event({restored_tx, ReadTX}),
    ?assert(hb_message:match(ReadMsg, SignedMsg)),
    ?assert(ar_bundles:verify_item(ReadTX)).
```

### unsigned_duplicated_tag_name_test

```erlang
unsigned_duplicated_tag_name_test() ->
    TX = ar_bundles:reset_ids(ar_bundles:normalize(#tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag">>, <<"test-value-2">>}
        ]
    })),
    Msg = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({msg, Msg}),
    TX2 = hb_message:convert(Msg, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({tx2, TX2}),
    ?assertEqual(TX, TX2).
```

### signed_duplicated_tag_name_test

```erlang
signed_duplicated_tag_name_test() ->
    TX = ar_bundles:sign_item(#tx {
        tags = [
            {<<"Test-Tag">>, <<"test-value">>},
            {<<"test-tag">>, <<"test-value-2">>}
        ]
    }, ar_wallet:new()),
    Msg = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({msg, Msg}),
    TX2 = hb_message:convert(Msg, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({tx2, TX2}),
    ?assertEqual(TX, TX2),
    ?assert(ar_bundles:verify_item(TX2)).
```

### simple_to_conversion_test

```erlang
simple_to_conversion_test() ->
    Msg = #{
        <<"first-tag">> => <<"first-value">>,
        <<"second-tag">> => <<"second-value">>
    },
    {ok, Encoded} = to(Msg, #{}, #{}),
    ?event({encoded, Encoded}),
    {ok, Decoded} = from(Encoded, #{}, #{}),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, hb_message:uncommitted(Decoded, #{}))).
```

### external_item_with_target_field_test

Ensure that items with an explicitly defined target field lead to:

```erlang
external_item_with_target_field_test() ->
    TX =
        ar_bundles:sign_item(
            #tx {
                target = crypto:strong_rand_bytes(32),
                tags = [
                    {<<"test-tag">>, <<"test-value">>},
                    {<<"test-tag-2">>, <<"test-value-2">>}
                ],
                data = <<"test-data">>
            },
            ar_wallet:new()
        ),
    EncodedTarget = hb_util:encode(TX#tx.target),
    ?event({tx, TX}),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, Decoded}),
    ?assertEqual(EncodedTarget, hb_maps:get(<<"target">>, Decoded, undefined, #{})),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, OnlyCommitted}),
    ?assertEqual(EncodedTarget, hb_maps:get(<<"target">>, OnlyCommitted, undefined, #{})),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?assertEqual(TX#tx.target, Encoded#tx.target),
    ?event({result, {initial, TX}, {result, Encoded}}),
    ?assertEqual(TX, Encoded).
```

### generate_item_with_target_tag_test

Ensure that items made inside HyperBEAM use the tags to encode `target`

```erlang
generate_item_with_target_tag_test() ->
    Msg =
        #{
            <<"target">> => Target = <<"NON-ID-TARGET">>,
            <<"other-key">> => <<"other-value">>
        },
    {ok, TX} = to(Msg, #{}, #{}),
    ?event({encoded_tx, TX}),
    % The encoded TX should have ignored the `target' field, setting a tag instead.
```

### generate_item_with_target_field_test

```erlang
generate_item_with_target_field_test() ->
    Msg =
        hb_message:commit(
            #{
                <<"target">> => Target = hb_util:encode(crypto:strong_rand_bytes(32)),
                <<"other-key">> => <<"other-value">>
            },
            #{ priv_wallet => hb:wallet() },
            <<"ans104@1.0">>
        ),
    {ok, TX} = to(Msg, #{}, #{}),
    ?event({encoded_tx, TX}),
    ?assertEqual(Target, hb_util:encode(TX#tx.target)),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, Decoded}),
    ?assertEqual(Target, hb_maps:get(<<"target">>, Decoded, undefined, #{})),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, OnlyCommitted}),
    ?assertEqual(Target, hb_maps:get(<<"target">>, OnlyCommitted, undefined, #{})),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({result, {initial, TX}, {result, Encoded}}),
    ?assertEqual(TX, Encoded).
```

### type_tag_test

```erlang
type_tag_test() ->
    TX =
        ar_bundles:sign_item(
            #tx {
                tags = [{<<"type">>, <<"test-value">>}]
            },
            ar_wallet:new()
        ),
    ?event({tx, TX}),
    Structured = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({structured, Structured}),
    TX2 = hb_message:convert(Structured, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({after_conversion, TX2}),
    ?assertEqual(TX, TX2).
```

### ao_data_key_test

```erlang
ao_data_key_test() ->
    Msg =
        hb_message:commit(
            #{
                <<"other-key">> => <<"Normal value">>,
                <<"body">> => <<"Body value">>
            },
            #{ priv_wallet => hb:wallet() },
            <<"ans104@1.0">>
        ),
    ?event({msg, Msg}),
    Enc = hb_message:convert(Msg, <<"ans104@1.0">>, #{}),
    ?event({enc, Enc}),
    ?assertEqual(<<"Body value">>, Enc#tx.data),
    Dec = hb_message:convert(Enc, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({dec, Dec}),
    ?assert(hb_message:verify(Dec, all, #{})).
```

### simple_signed_to_httpsig_test

```erlang
simple_signed_to_httpsig_test() ->
    Structured =
        hb_message:commit(
            #{ <<"test-tag">> => <<"test-value">> },
            #{ priv_wallet => ar_wallet:new() },
            #{
                <<"commitment-device">> => <<"ans104@1.0">>
            }
        ),
    ?event(debug_test, {msg, Structured}),
    HTTPSig =
        hb_message:convert(
            Structured,
            <<"httpsig@1.0">>,
            <<"structured@1.0">>,
            #{}
        ),
    ?event(debug_test, {httpsig, HTTPSig}),
    Structured2 =
        hb_message:convert(
            HTTPSig,
            <<"structured@1.0">>,
            <<"httpsig@1.0">>,
            #{}
        ),
    ?event(debug_test, {decoded, Structured2}),
	Match = hb_message:match(Structured, Structured2, #{}),
    ?assert(Match),
    ?assert(hb_message:verify(Structured2, all, #{})),
    HTTPSig2 = hb_message:convert(Structured2, <<"httpsig@1.0">>, <<"structured@1.0">>, #{}),
    ?event(debug_test, {httpsig2, HTTPSig2}),
    ?assert(hb_message:verify(HTTPSig2, all, #{})),
    ?assert(hb_message:match(HTTPSig, HTTPSig2)).
```

### unsorted_tag_map_test

```erlang
unsorted_tag_map_test() ->
    TX =
        ar_bundles:sign_item(
            #tx{
                format = ans104,
                tags = [
                    {<<"z">>, <<"position-1">>},
                    {<<"a">>, <<"position-2">>}
                ],
                data = <<"data">>
            },
            ar_wallet:new()
        ),
    ?assert(ar_bundles:verify_item(TX)),
    ?event(debug_test, {tx, TX}),
    {ok, TABM} = dev_codec_ans104:from(TX, #{}, #{}),
    ?event(debug_test, {tabm, TABM}),
    {ok, Decoded} = dev_codec_ans104:to(TABM, #{}, #{}),
    ?event(debug_test, {decoded, Decoded}),
    ?assert(ar_bundles:verify_item(Decoded)).
```

### field_and_tag_ordering_test

```erlang
field_and_tag_ordering_test() ->
    UnsignedTABM = #{
        <<"a">> => <<"value1">>,
        <<"z">> => <<"value2">>,
        <<"target">> => <<"NON-ID-TARGET">>
    },
    Wallet = hb:wallet(),
    SignedTABM = hb_message:commit(
        UnsignedTABM, #{priv_wallet => Wallet}, <<"ans104@1.0">>),
```

---

*Generated from [dev_codec_ans104.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_ans104.erl)*
