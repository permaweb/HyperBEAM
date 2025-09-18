# dev_codec_httpsig

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_httpsig.erl)

This module implements HTTP Message Signatures as described in RFC-9421
(https://datatracker.ietf.org/doc/html/rfc9421), as an AO-Core device.
It implements the codec standard (from/1, to/1), as well as the optional
commitment functions (id/3, sign/3, verify/3). The commitment functions
are found in this module, while the codec functions are relayed to the 
`dev_codec_httpsig_conv` module.

---

## Exported Functions

- `add_content_digest/2`
- `commit/3`
- `from/3`
- `normalize_for_encoding/3`
- `serialize/2`
- `serialize/3`
- `to/3`
- `verify/3`

---

### to

This module implements HTTP Message Signatures as described in RFC-9421

```erlang
to(Msg, Req, Opts) -> dev_codec_httpsig_conv:to(Msg, Req, Opts).
```

### from

This module implements HTTP Message Signatures as described in RFC-9421
Generate the `Opts` to use during AO-Core operations in the codec.

```erlang
from(Msg, Req, Opts) -> dev_codec_httpsig_conv:from(Msg, Req, Opts).
```

### opts

This module implements HTTP Message Signatures as described in RFC-9421
Generate the `Opts` to use during AO-Core operations in the codec.

```erlang
opts(RawOpts) ->
    RawOpts#{
        hashpath => ignore,
        cache_control => [<<"no-cache">>, <<"no-store">>],
        force_message => false
    }.
```

### serialize

A helper utility for creating a direct encoding of a HTTPSig message.

```erlang
serialize(Msg, Opts) -> serialize(Msg, #{}, Opts).
```

### serialize

A helper utility for creating a direct encoding of a HTTPSig message.

```erlang
serialize(Msg, #{ <<"format">> := <<"components">> }, Opts) ->
    % Convert to HTTPSig via TABM through calling `hb_message:convert` rather
    % than executing `to/3` directly. This ensures that our responses are 
    % normalized.
```

### serialize

```erlang
serialize(Msg, _Req, Opts) ->
    % We assume the default format of `binary` if none of the prior clauses
    % match.
```

### verify

```erlang
verify(Base, Req, RawOpts) ->
    % A rsa-pss-sha512 commitment is verified by regenerating the signature
    % base and validating against the signature.
```

### commit

Commit to a message using the HTTP-Signature format. We use the `type`

```erlang
commit(Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"hmac-sha256">> }, Opts);
```

### commit

Commit to a message using the HTTP-Signature format. We use the `type`

```erlang
commit(Msg, Req = #{ <<"type">> := <<"signed">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"rsa-pss-sha512">> }, Opts);
```

### commit

Commit to a message using the HTTP-Signature format. We use the `type`

```erlang
commit(MsgToSign, Req = #{ <<"type">> := <<"rsa-pss-sha512">> }, RawOpts) ->
    ?event(
        {generating_rsa_pss_sha512_commitment, {msg, MsgToSign}, {req, Req}}
    ),
    Opts = opts(RawOpts),
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    if Wallet =:= no_viable_wallet ->
        throw({cannot_commit, no_viable_wallet, MsgToSign});
    true ->
        ok
    end,
    % Utilize the hashpath, if present, as the tag for the commitment.
```

### commit

```erlang
commit(BaseMsg, Req = #{ <<"type">> := <<"hmac-sha256">> }, RawOpts) ->
    % Extract the key material from the request.
```

### maybe_bundle_tag_commitment

Annotate the commitment with the `bundle` key if the request contains

```erlang
maybe_bundle_tag_commitment(Commitment, Req, _Opts) ->
    case hb_util:atom(maps:get(<<"bundle">>, Req, false)) of
        true -> Commitment#{ <<"bundle">> => <<"true">> };
        false -> Commitment
    end.
```

### keys_to_commit

Derive the set of keys to commit to from a `commit` request and a 

```erlang
keys_to_commit(_Base, #{ <<"committed">> := Explicit}, _Opts) ->
    % Case 1: Explicitly provided keys to commit.
```

### keys_to_commit

```erlang
keys_to_commit(Base, _Req, Opts) ->
    % Extract the set of committed keys from the message.
```

### add_content_digest

If the `body` key is present and a binary, replace it with a

```erlang
add_content_digest(Msg, _Opts) ->
    case maps:get(<<"body">>, Msg, not_found) of
        Body when is_binary(Body) ->
            % Remove the body from the message and add the content-digest,
            % encoded as a structured field.
```

### normalize_for_encoding

Given a base message and a commitment, derive the message and commitment

```erlang
normalize_for_encoding(Msg, Commitment, Opts) ->
    % Extract the requested keys to include in the signature base.
```

### key_present

Calculate if a key or its `+link` TABM variant is present in a message.
create the signature base that will be signed in order to create the

```erlang
key_present(Key, Msg) ->
    NormalizedKey = hb_ao:normalize_key(Key),
    maps:is_key(NormalizedKey, Msg)
        orelse maps:is_key(<<NormalizedKey/binary, "+link">>, Msg).
%%
```

### signature_base

Calculate if a key or its `+link` TABM variant is present in a message.
create the signature base that will be signed in order to create the

```erlang
signature_base(EncodedMsg, Commitment, Opts) ->
	ComponentsLines =
        signature_components_line(
            EncodedMsg,
            Commitment,
            Opts
        ),
    ?event({component_identifiers_for_sig_base, ComponentsLines}),
	ParamsLine = signature_params_line(Commitment, Opts),
    SignatureBase = 
        <<
            ComponentsLines/binary, "\n",
            "\"@signature-params\": ", ParamsLine/binary
        >>,
    ?event(signature_base, {signature_base, {string, SignatureBase}}),
	SignatureBase.
```

### signature_components_line

Given a list of Component Identifiers and a Request/Response Message
construct the "signature-params-line" part of the signature base.

```erlang
signature_components_line(Req, Commitment, _Opts) ->
	ComponentsLines =
        lists:map(
            fun(Name) ->
                case maps:get(Name, Req, not_found) of
                    not_found ->
                        throw(
                            {
                                missing_key_for_signature_component_line,
                                Name,
                                {message, Req},
                                {commitment, Commitment}
                            }
                        );
                    Value ->
                        << <<"\"">>/binary, Name/binary, <<"\"">>/binary, <<": ">>/binary, Value/binary>>
                end
            end,
            maps:get(<<"committed">>, Commitment)
        ),
	iolist_to_binary(lists:join(<<"\n">>, ComponentsLines)).
%%
```

### signature_params_line

Given a list of Component Identifiers and a Request/Response Message
construct the "signature-params-line" part of the signature base.

```erlang
signature_params_line(RawCommitment, Opts) ->
    Commitment =
        maps:without(
            [<<"signature">>, <<"signature-input">>],
            RawCommitment
        ),
    ?event(debug_enc, {signature_params_line, {commitment, Commitment}}),
	hb_util:bin(
        hb_structured_fields:list(
            [
                {
                    list,
                    lists:map(
                        fun(Key) -> {item, {string, Key}, []} end,
                        dev_codec_httpsig_siginfo:add_derived_specifiers(
                            hb_util:message_to_ordered_list(
                                maps:get(<<"committed">>, Commitment),
                                Opts
                            )
                        )
                    ),
                    lists:map(
                        fun ({<<"alg">>, Param}) when is_binary(Param) ->
                            {<<"alg">>, {string, Param}};
                        ({Name, Param}) when is_binary(Param) ->
                            {Name, {string, Param}};
                        ({Name, Param}) when is_integer(Param) ->
                            {Name, Param}
                        end,
                        lists:sort(maps:to_list(
                            maps:with(
                                [
                                    <<"created">>,
                                    <<"expires">>,
                                    <<"nonce">>,
                                    <<"alg">>,
                                    <<"keyid">>,
                                    <<"tag">>,
                                    <<"bundle">>
                                ],
                                Commitment#{ <<"alg">> => maps:get(<<"type">>, Commitment) }
                            )
                        ))
                    )
                }
            ]
        )
    ).
```

### validate_large_message_from_http_test

Ensure that we can validate a signature on an extremely large and complex

```erlang
validate_large_message_from_http_test() ->
    Node = hb_http_server:start_node(Opts = #{
        force_signed => true,
        commitment_device => <<"httpsig@1.0">>,
        extra =>
            [
                [
                    [
                        #{
                            <<"n">> => N,
                            <<"m">> => M,
                            <<"o">> => O
                        }
                    ||
                        O <- lists:seq(1, 3)
                    ]
                ||
                    M <- lists:seq(1, 3)
                ]
            ||
                N <- lists:seq(1, 3)
            ]
    }),
    {ok, Res} = hb_http:get(Node, <<"/~meta@1.0/info">>, Opts),
    Signers = hb_message:signers(Res, Opts),
    ?event({received, {signers, Signers}, {res, Res}}),
    ?assert(length(Signers) == 1),
    ?assert(hb_message:verify(Res, Signers, Opts)),
    ?event({sig_verifies, Signers}),
    ?assert(hb_message:verify(Res, all, Opts)),
    ?event({hmac_verifies, <<"hmac-sha256">>}),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Res, Opts),
    ?event({msg_with_only_committed, OnlyCommitted}),
    ?assert(hb_message:verify(OnlyCommitted, Signers, Opts)),
    ?event({msg_with_only_committed_verifies, Signers}),
    ?assert(hb_message:verify(OnlyCommitted, all, Opts)),
    ?event({msg_with_only_committed_verifies_hmac, <<"hmac-sha256">>}).
```

### committed_id_test

Ensure that we can validate a signature on an extremely large and complex

```erlang
committed_id_test() ->
    Msg = #{ <<"basic">> => <<"value">> },
    Signed = hb_message:commit(Msg, hb:wallet()),
    ?assert(hb_message:verify(Signed, all, #{})),
    ?event({signed_msg, Signed}),
    UnsignedID = hb_message:id(Signed, none),
    SignedID = hb_message:id(Signed, all),
    ?event({ids, {unsigned_id, UnsignedID}, {signed_id, SignedID}}),
    ?assertNotEqual(UnsignedID, SignedID).
```

### commit_secret_key_test

```erlang
commit_secret_key_test() ->
    Msg = #{ <<"basic">> => <<"value">> },
    CommittedMsg =
        hb_message:commit(
            Msg,
            #{},
            #{
                <<"type">> => <<"hmac-sha256">>,
                <<"secret">> => <<"test-secret">>,
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"scheme">> => <<"secret">>
            }
        ),
    ?event({committed_msg, CommittedMsg}),
    Committers = hb_message:signers(CommittedMsg, #{}),
    ?assert(length(Committers) == 1),
    ?event({committers, Committers}),
    ?assert(
        hb_message:verify(
            CommittedMsg,
            #{ <<"committers">> => Committers, <<"secret">> => <<"test-secret">> },
            #{}
        )
    ),
    ?assertNot(
        hb_message:verify(
            CommittedMsg,
            #{ <<"committers">> => Committers, <<"secret">> => <<"bad-secret">> },
            #{}
        )
    ).
```

### multicommitted_id_test

```erlang
multicommitted_id_test() ->
    Msg = #{ <<"basic">> => <<"value">> },
    Signed1 = hb_message:commit(Msg, Wallet1 = ar_wallet:new()),
    Signed2 = hb_message:commit(Signed1, Wallet2 = ar_wallet:new()),
    Addr1 = hb_util:human_id(ar_wallet:to_address(Wallet1)),
    Addr2 = hb_util:human_id(ar_wallet:to_address(Wallet2)),
    ?event({signed_msg, Signed2}),
    UnsignedID = hb_message:id(Signed2, none),
    SignedID = hb_message:id(Signed2, all),
    ?event({ids, {unsigned_id, UnsignedID}, {signed_id, SignedID}}),
    ?assertNotEqual(UnsignedID, SignedID),
    ?assert(hb_message:verify(Signed2, [])),
    ?assert(hb_message:verify(Signed2, [Addr1])),
    ?assert(hb_message:verify(Signed2, [Addr2])),
    ?assert(hb_message:verify(Signed2, [Addr1, Addr2])),
    ?assert(hb_message:verify(Signed2, [Addr2, Addr1])),
    ?assert(hb_message:verify(Signed2, all)).
```

### sign_and_verify_link_test

Test that we can sign and verify a message with a link. We use 

```erlang
sign_and_verify_link_test() ->
    Msg = #{
        <<"normal">> => <<"typical-value">>,
        <<"untyped">> => #{ <<"inner-untyped">> => <<"inner-value">> },
        <<"typed">> => #{ <<"inner-typed">> => 123 }
    },
    NormMsg = hb_message:convert(Msg, <<"structured@1.0">>, #{}),
    ?event({msg, NormMsg}),
    Signed = hb_message:commit(NormMsg, hb:wallet()),
    ?event({signed_msg, Signed}),
    ?assert(hb_message:verify(Signed)).
```

---

*Generated from [dev_codec_httpsig.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_httpsig.erl)*
