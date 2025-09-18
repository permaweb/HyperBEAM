# dev_codec_httpsig_siginfo

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_httpsig_siginfo.erl)

A list of components that are `derived` in the context of RFC-9421 from the
request message.

---

## Exported Functions

- `add_derived_specifiers/1`
- `commitment_to_sig_name/1`
- `commitments_to_siginfo/3`
- `committed_keys_to_siginfo/1`
- `from_siginfo_keys/3`
- `remove_derived_specifiers/1`
- `siginfo_to_commitments/3`
- `to_siginfo_keys/3`

---

### commitments_to_siginfo

A module for converting between commitments and their encoded `signature`
Generate a `signature` and `signature-input` key pair from a commitment

```erlang
commitments_to_siginfo(_Msg, Comms, _Opts) when ?IS_EMPTY_MESSAGE(Comms) ->
    #{};
```

### commitments_to_siginfo

A module for converting between commitments and their encoded `signature`
Generate a `signature` and `signature-input` key pair from a commitment

```erlang
commitments_to_siginfo(Msg, Comms, Opts) ->
    % Generate a SF item for each commitment's signature and signature-input.
```

### commitment_to_sf_siginfo

Generate a `signature` and `signature-input` key pair from a given

```erlang
commitment_to_sf_siginfo(Msg, Commitment, Opts) ->
    % Generate the `alg' key from the commitment.
```

### get_additional_params

```erlang
get_additional_params(Commitment) ->
    AdditionalParams =
        sets:to_list(
            sets:subtract(
                sets:from_list(maps:keys(Commitment)), 
                sets:from_list(
                    [
                        <<"alg">>,
                        <<"keyid">>,
                        <<"tag">>,
                        <<"created">>,
                        <<"expires">>,
                        <<"nonce">>,
                        <<"committed">>,
                        <<"signature">>,
                        <<"type">>,
                        <<"commitment-device">>,
                        <<"committer">>
                    ]
                )
            )
        ),
    lists:map(fun(Param) ->
        ParamValue = maps:get(Param, Commitment),
        case ParamValue of
            Val when is_atom(Val) ->
                {Param, {string, atom_to_binary(Val, utf8)}};
            Val when is_binary(Val) ->
                {Param, {string, Val}};
            Val when is_list(Val) ->
                {Param, {string, list_to_binary(lists:join(<<", ">>, Val))}};
            Val when is_map(Val) ->
                Map = nested_map_to_string(Val),
                {Param, {string, list_to_binary(lists:join(<<", ">>, Map))} }
        end
    end, AdditionalParams).
```

### nested_map_to_string

```erlang
nested_map_to_string(Map) ->
    lists:map(fun(I) ->
        case maps:get(I, Map) of
            Val when is_map(Val) ->
                Name = maps:get(<<"name">>, Val),
                Value = hb_util:encode(maps:get(<<"value">>, Val)),
                <<I/binary, ":", Name/binary, ":", Value/binary>>;
            Val ->
                Val
        end
    end, maps:keys(Map)).
```

### siginfo_to_commitments

Take a message with a `signature` and `signature-input` key pair and

```erlang
siginfo_to_commitments(
        Msg =
            #{
                <<"signature">> := <<"comm-", SFSigBin/binary>>,
                <<"signature-input">> := <<"comm-", SFSigInputBin/binary>>
            },
        BodyKeys,
        Opts) ->
    % Parse the signature and signature-input structured-fields.
```

### siginfo_to_commitments

```erlang
siginfo_to_commitments(_Msg, _BodyKeys, _Opts) ->
    % If the message does not contain a `signature' or `signature-input' key,
    % we return an empty map.
```

### sf_siginfo_to_commitment

Take a signature and signature-input as parsed structured-fields and 

```erlang
sf_siginfo_to_commitment(Msg, BodyKeys, SFSig, SFSigInput, Opts) ->
    % Extract the signature and signature-input from the structured-fields.
```

### decoding_nested_map_binary

```erlang
decoding_nested_map_binary(Bin) ->
    MapBinary =
        lists:foldl(
            fun (X, Acc) ->
                case binary:split(X, <<":">>, [global]) of
                    [ID, Key, Value] ->
                        Acc#{
                            ID => #{ 
                                <<"name">> => Key,
                                <<"value">> => hb_util:decode(Value)
                            }
                        };
                    _ ->
                        X
                end
            end,
            #{},
            binary:split(Bin, <<", ">>, [global])
        ),
    case MapBinary of
        Res when is_map(Res) ->
            Res;
        Res ->
            Res
    end.
```

### to_siginfo_keys

Normalize a list of AO-Core keys to their equivalents in `httpsig@1.0`
Normalize a list of `httpsig@1.0` keys to their equivalents in AO-Core

```erlang
to_siginfo_keys(Msg, Commitment, Opts) ->
    {ok, _EncMsg, EncComm, _} =
        dev_codec_httpsig:normalize_for_encoding(Msg, Commitment, Opts),
    maps:get(<<"committed">>, EncComm).
```

### from_siginfo_keys

Normalize a list of AO-Core keys to their equivalents in `httpsig@1.0`
Normalize a list of `httpsig@1.0` keys to their equivalents in AO-Core

```erlang
from_siginfo_keys(HTTPEncMsg, BodyKeys, SigInfoCommitted) ->
    % 1. Remove specifiers from the list.
```

### committed_keys_to_siginfo

Convert committed keys to their siginfo format. This involves removing

```erlang
committed_keys_to_siginfo(Msg) when is_map(Msg) ->
    committed_keys_to_siginfo(hb_util:message_to_ordered_list(Msg));
```

### committed_keys_to_siginfo

Convert committed keys to their siginfo format. This involves removing

```erlang
committed_keys_to_siginfo([]) -> [];
```

### committed_keys_to_siginfo

Convert committed keys to their siginfo format. This involves removing

```erlang
committed_keys_to_siginfo([<<"body">> | Rest]) ->
    [<<"content-digest">> | Rest];
```

### committed_keys_to_siginfo

Convert committed keys to their siginfo format. This involves removing

```erlang
committed_keys_to_siginfo([Key | Rest]) ->
    [Key | committed_keys_to_siginfo(Rest)].
```

### commitment_to_device_specifiers

Convert an `alg` to a commitment device. If the `alg` has the form of

```erlang
commitment_to_device_specifiers(Commitment, Opts) when is_map(Commitment) ->
    commitment_to_device_specifiers(maps:get(<<"alg">>, Commitment), Opts);
```

### commitment_to_device_specifiers

Convert an `alg` to a commitment device. If the `alg` has the form of

```erlang
commitment_to_device_specifiers(Alg, _Opts) ->
    case binary:split(Alg, <<"@">>) of
        [Type] ->
            % The `alg' is not a device specifier, so we assume that it is a
            % type of the `httpsig@1.0' algorithm.
```

### commitment_to_alg

Calculate an `alg` string from a commitment message, using its 

```erlang
commitment_to_alg(#{ <<"commitment-device">> := <<"httpsig@1.0">>, <<"type">> := Type }, _Opts) ->
    Type;
```

### commitment_to_alg

Calculate an `alg` string from a commitment message, using its 

```erlang
commitment_to_alg(Commitment, _Opts) ->
    Type =
        case maps:get(<<"type">>, Commitment, undefined) of
            undefined -> <<>>;
            TypeSpecifier -> <<"/", TypeSpecifier/binary>>
        end,
    CommitmentDevice = maps:get(<<"commitment-device">>, Commitment),
    <<CommitmentDevice/binary, Type/binary>>.
```

### commitment_to_sig_name

Generate a signature name from a commitment. The commitment message is
Normalize key parameters to ensure their names are correct for inclusion

```erlang
commitment_to_sig_name(Commitment) ->
    BaseStr =
        case maps:get(<<"committer">>, Commitment, undefined) of
            undefined -> maps:get(<<"keyid">>, Commitment);
            Committer ->
                <<
                    (hb_util:to_hex(binary:part(hb_util:native_id(Committer), 1, 8)))
                        /binary
                >>
        end,
    DeviceStr =
        binary:replace(
            maps:get(
                <<"commitment-device">>,
                Commitment
            ),
            <<"@">>,
            <<"-">>
        ),
    <<DeviceStr/binary, ".", BaseStr/binary>>.
```

### add_derived_specifiers

Generate a signature name from a commitment. The commitment message is
Normalize key parameters to ensure their names are correct for inclusion

```erlang
add_derived_specifiers(ComponentIdentifiers) ->
    % Remove the @ prefix from the component identifiers, if present.
```

### remove_derived_specifiers

Remove derived specifiers from a list of component identifiers.

```erlang
remove_derived_specifiers(ComponentIdentifiers) ->
    lists:map(
        fun(<<"@", Key/binary>>) ->
            Key;
        (Key) ->
            Key
        end,
        ComponentIdentifiers
    ).
```

### parse_alg_test

```erlang
parse_alg_test() ->
    ?assertEqual(
        commitment_to_device_specifiers(#{ <<"alg">> => <<"rsa-pss-sha512">> }, #{}),
        #{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"type">> => <<"rsa-pss-sha512">>
        }
    ),
    ?assertEqual(
        commitment_to_device_specifiers(
            #{ <<"alg">> => <<"ans104@1.0/rsa-pss-sha256">> },
            #{}),
        #{
            <<"commitment-device">> => <<"ans104@1.0">>,
            <<"type">> => <<"rsa-pss-sha256">>
        }
    ).
```

### escaped_value_test

Test that tag values with special characters are correctly encoded and

```erlang
escaped_value_test() ->
    KeyID = crypto:strong_rand_bytes(32),
    Committer = hb_util:human_id(ar_wallet:to_address(KeyID)),
    Signature = crypto:strong_rand_bytes(512),
    ID = hb_util:human_id(crypto:hash(sha256, Signature)),
    Commitment = #{
        <<"committed">> => #{},
        <<"committer">> => Committer,
        <<"commitment-device">> => <<"tx@1.0">>,
        <<"keyid">> => <<"publickey:", (hb_util:encode(KeyID))/binary>>,
        <<"original-tags">> => #{
            <<"1">> => #{
                <<"name">> => <<"Key">>,
                <<"value">> => <<"value">>
            },
            <<"2">> => #{
                <<"name">> => <<"Quotes">>,
                <<"value">> => <<"{\"function\":\"mint\"}">>
            }
        },
        <<"signature">> => hb_util:encode(Signature),
        <<"type">> => <<"rsa-pss-sha256">>
    },
    SigInfo = commitments_to_siginfo(#{}, #{ ID => Commitment }, #{}),
    Commitments = siginfo_to_commitments(SigInfo, #{}, #{}),
    ?event(debug_test, {siginfo, {explicit, SigInfo}}),
    ?event(debug_test, {commitments, {explicit, Commitments}}),
    ?assertEqual(#{ ID => Commitment }, Commitments).
```

---

*Generated from [dev_codec_httpsig_siginfo.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_httpsig_siginfo.erl)*
