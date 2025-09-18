# hb_message

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_message.erl)

This module acts an adapter between messages, as modeled in the
AO-Core protocol, and their uderlying binary representations and formats.
Unless you are implementing a new message serialization codec, you should
not need to interact with this module directly. Instead, use the
`hb_ao` interfaces to interact with all messages. The `dev_message`
module implements a device interface for abstracting over the different
message formats.
`hb_message` and the HyperBEAM caches can interact with multiple different
types of message formats:
    - Richly typed AO-Core structured messages.
    - Arweave transations.
    - ANS-104 data items.
    - HTTP Signed Messages.
    - Flat Maps.
This module is responsible for converting between these formats. It does so
by normalizing messages to a common format: `Type Annotated Binary Messages`
(TABM). TABMs are deep Erlang maps with keys than only contain either other
TABMs or binary values. By marshalling all messages into this format, they
can easily be coerced into other output formats. For example, generating a
`HTTP Signed Message` format output from an Arweave transaction. TABM is
also a simple format from a computational perspective (only binary literals
and O(1) access maps), such that operations upon them are efficient.
The structure of the conversions is as follows:
<pre>
    Arweave TX/ANS-104 ==> dev_codec_ans104:from/1 ==> TABM
    HTTP Signed Message ==> dev_codec_httpsig_conv:from/1 ==> TABM
    Flat Maps ==> dev_codec_flat:from/1 ==> TABM
    TABM ==> dev_codec_structured:to/1 ==> AO-Core Message
    AO-Core Message ==> dev_codec_structured:from/1 ==> TABM
    TABM ==> dev_codec_ans104:to/1 ==> Arweave TX/ANS-104
    TABM ==> dev_codec_httpsig_conv:to/1 ==> HTTP Signed Message
    TABM ==> dev_codec_flat:to/1 ==> Flat Maps
    ...
</pre>
Additionally, this module provides a number of utility functions for
manipulating messages. For example, `hb_message:sign/2` to sign a message of
arbitrary type, or `hb_formatter:format_msg/1` to print an AO-Core/TABM message in
a human-readable format.
The `hb_cache` module is responsible for storing and retrieving messages in
the HyperBEAM stores configured on the node. Each store has its own storage
backend, but each works with simple key-value pairs. Subsequently, the 
`hb_cache` module uses TABMs as the internal format for storing and 
retrieving messages.
Test vectors to ensure the functioning of this module and the codecs that
interact with it are found in `hb_message_test_vectors.erl`.

---

## Exported Functions

- `commit/2`
- `commit/3`
- `commitment_devices/2`
- `commitment/2`
- `commitment/3`
- `commitments/3`
- `committed/3`
- `convert/3`
- `convert/4`
- `default_tx_list/0`
- `diff/3`
- `filter_default_keys/1`
- `find_target/3`
- `id/1`
- `id/2`
- `id/3`
- `is_signed_key/3`
- `match/2`
- `match/3`
- `match/4`
- `minimize/1`
- `normalize_commitments/2`
- `print/1`
- `signers/2`
- `type/1`
- `uncommitted/1`
- `uncommitted/2`
- `verify/1`
- `verify/2`
- `verify/3`
- `with_commitments/3`
- `with_only_committed/2`
- `with_only_committers/2`
- `with_only_committers/3`
- `without_commitments/3`
- `without_unless_signed/3`

---

### convert

This module acts an adapter between messages, as modeled in the
Convert a message from one format to another. Taking a message in the

```erlang
convert(Msg, TargetFormat, Opts) ->
    convert(Msg, TargetFormat, <<"structured@1.0">>, Opts).
```

### convert

This module acts an adapter between messages, as modeled in the
Convert a message from one format to another. Taking a message in the

```erlang
convert(Msg, TargetFormat, tabm, Opts) ->
    OldPriv =
        if is_map(Msg) -> maps:get(<<"priv">>, Msg, #{});
           true -> #{}
        end,
    from_tabm(Msg, TargetFormat, OldPriv, Opts);
```

### convert

This module acts an adapter between messages, as modeled in the
Convert a message from one format to another. Taking a message in the

```erlang
convert(Msg, TargetFormat, SourceFormat, Opts) ->
    OldPriv =
        if is_map(Msg) -> maps:get(<<"priv">>, Msg, #{});
           true -> #{}
        end,
    TABM =
        to_tabm(
            case is_map(Msg) of
                true -> hb_maps:without([<<"priv">>], Msg, Opts);
                false -> Msg
            end,
            SourceFormat,
            Opts
        ),
    case TargetFormat of
        tabm -> restore_priv(TABM, OldPriv, Opts);
        _ -> from_tabm(TABM, TargetFormat, OldPriv, Opts)
    end.
```

### to_tabm

```erlang
to_tabm(Msg, SourceFormat, Opts) ->
    {SourceCodecMod, Params} = conversion_spec_to_req(SourceFormat, Opts),
    % We use _from_ here because the codecs are labelled from the perspective
    % of their own format. `dev_codec_ans104:from/1' will convert _from_
    % an ANS-104 message _into_ a TABM.
```

### from_tabm

```erlang
from_tabm(Msg, TargetFormat, OldPriv, Opts) ->
    {TargetCodecMod, Params} = conversion_spec_to_req(TargetFormat, Opts),
    % We use the _to_ function here because each of the codecs we may call in
    % this step are labelled from the perspective of the target format. For 
    % example, `dev_codec_httpsig:to/1' will convert _from_ a TABM to an
    % HTTPSig message.
```

### restore_priv

Add the existing `priv` sub-map back to a converted message, honoring

```erlang
restore_priv(Msg, EmptyPriv, _Opts) when map_size(EmptyPriv) == 0 -> Msg;
```

### restore_priv

Add the existing `priv` sub-map back to a converted message, honoring
Get a codec device and request params from the given conversion request. 

```erlang
restore_priv(Msg, OldPriv, Opts) ->
    MsgPriv = hb_maps:get(<<"priv">>, Msg, #{}, Opts),
    ?event({restoring_priv, {msg_priv, MsgPriv}, {old_priv, OldPriv}}),
    NewPriv = hb_util:deep_merge(MsgPriv, OldPriv, Opts),
    ?event({new_priv, NewPriv}),
    Msg#{ <<"priv">> => NewPriv }.
```

### conversion_spec_to_req

Add the existing `priv` sub-map back to a converted message, honoring
Get a codec device and request params from the given conversion request. 

```erlang
conversion_spec_to_req(Spec, Opts) when is_binary(Spec) or (Spec == tabm) ->
    conversion_spec_to_req(#{ <<"device">> => Spec }, Opts);
```

### conversion_spec_to_req

Add the existing `priv` sub-map back to a converted message, honoring
Get a codec device and request params from the given conversion request. 

```erlang
conversion_spec_to_req(Spec, Opts) ->
    try
        Device =
            hb_maps:get(
                <<"device">>,
                Spec,
                no_codec_device_in_conversion_spec,
                Opts
            ),
        {
            case Device of
                tabm -> tabm;
                _ ->
                    hb_ao:message_to_device(
                        #{
                            <<"device">> => Device
                        },
                        Opts
                    )
            end,
            hb_maps:without([<<"device">>], Spec, Opts)
        }
    catch _:_ ->
        throw({message_codec_not_extractable, Spec})
    end.
```

### id

Return the ID of a message.

```erlang
id(Msg) -> id(Msg, uncommitted).
```

### id

Return the ID of a message.

```erlang
id(Msg, Opts) when is_map(Opts) -> id(Msg, uncommitted, Opts);
```

### id

Return the ID of a message.

```erlang
id(Msg, Committers) -> id(Msg, Committers, #{}).
```

### id

Return the ID of a message.

```erlang
id(Msg, RawCommitters, Opts) ->
    CommSpec =
        case RawCommitters of
            none -> #{ <<"committers">> => <<"none">> };
            uncommitted -> #{ <<"committers">> => <<"none">> };
            unsigned -> #{ <<"committers">> => <<"none">> };
            all -> #{ <<"committers">> => <<"all">> };
            signed -> #{ <<"committers">> => <<"all">> };
            List when is_list(List) -> #{ <<"committers">> => List }
        end,
    ?event({getting_id, {msg, Msg}, {spec, CommSpec}}),
    {ok, ID} =
        dev_message:id(
            Msg,
            CommSpec#{ <<"path">> => <<"id">> },
            Opts
        ),
    hb_util:human_id(ID).
```

### normalize_commitments

Normalize the IDs in a message, ensuring that there is at least one

```erlang
normalize_commitments(Msg, Opts) when is_map(Msg) ->
    NormMsg = 
        maps:map(
            fun(Key, Val) when Key == <<"commitments">> orelse Key == <<"priv">> ->
                Val;
               (_Key, Val) -> normalize_commitments(Val, Opts)
            end,
            Msg
        ),
    case hb_maps:get(<<"commitments">>, NormMsg, not_found, Opts) of
        not_found ->
            {ok, #{ <<"commitments">> := Commitments }} =
                dev_message:commit(
                    NormMsg,
                    #{ <<"type">> => <<"unsigned">> },
                    Opts
                ),
            NormMsg#{ <<"commitments">> => Commitments };
        _ -> NormMsg
    end;
```

### normalize_commitments

Normalize the IDs in a message, ensuring that there is at least one

```erlang
normalize_commitments(Msg, Opts) when is_list(Msg) ->
    lists:map(fun(X) -> normalize_commitments(X, Opts) end, Msg);
```

### normalize_commitments

Normalize the IDs in a message, ensuring that there is at least one

```erlang
normalize_commitments(Msg, _Opts) ->
    Msg.
```

### with_only_committed

Return a message with only the committed keys. If no commitments are

```erlang
with_only_committed(Msg, Opts) when is_map(Msg) ->
    ?event({with_only_committed, {msg, Msg}, {opts, Opts}}),
    Comms = hb_maps:get(<<"commitments">>, Msg, not_found, Opts),
    case is_map(Msg) andalso Comms /= not_found of
        true ->
            try
                CommittedKeys =
                    hb_message:committed(
                        Msg,
                        #{ <<"commitments">> => <<"all">> },
                        Opts
                    ),
                % Add the ao-body-key to the committed list if it is not
                % already present.
```

### with_only_committed

```erlang
with_only_committed(Msg, _) ->
    % If the message is not a map, it cannot be signed.
```

### with_links

Filter keys from a map that do not match either the list of keys or

```erlang
with_links(Keys, Map, Opts) ->
    hb_maps:with(
        Keys ++
            lists:map(
                fun(Key) ->
                    <<(hb_link:remove_link_specifier(Key))/binary, "+link">>
                end,
                Keys
            ),
        Map,
        Opts
    ).
```

### with_only_committers

Return the message with only the specified committers attached.

```erlang
with_only_committers(Msg, Committers) ->
    with_only_committers(Msg, Committers, #{}).
```

### with_only_committers

```erlang
with_only_committers(Msg, Committers, Opts) when is_map(Msg) ->
    NewCommitments =
        hb_maps:filter(
            fun(_, #{ <<"committer">> := Committer }) ->
                lists:member(Committer, Committers);
               (_, _) -> false
            end,
            hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
			Opts
        ),
    Msg#{ <<"commitments">> => NewCommitments };
```

### with_only_committers

```erlang
with_only_committers(Msg, _Committers, _Opts) ->
    throw({unsupported_message_type, Msg}).
```

### is_signed_key

Determine whether a specific key is part of a message's commitments.

```erlang
is_signed_key(Key, Msg, Opts) ->
    lists:member(Key, hb_message:committed(Msg, all, Opts)).
```

### without_unless_signed

Remove the any of the given keys that are not signed from a message.

```erlang
without_unless_signed(Key, Msg, Opts) when not is_list(Key) ->
    without_unless_signed([Key], Msg, Opts);
```

### without_unless_signed

Remove the any of the given keys that are not signed from a message.

```erlang
without_unless_signed(Keys, Msg, Opts) ->
    SignedKeys = hb_message:committed(Msg, all, Opts),
    maps:without(
        lists:filter(fun(K) -> not lists:member(K, SignedKeys) end, Keys),
        Msg
    ).
```

### commit

Sign a message with the given wallet.

```erlang
commit(Msg, WalletOrOpts) ->
    commit(
        Msg,
        WalletOrOpts,
        hb_opts:get(
            commitment_device,
            no_viable_commitment_device,
            case is_map(WalletOrOpts) of
                true -> WalletOrOpts;
                false -> #{ priv_wallet => WalletOrOpts }
            end
        )
    ).
```

### commit

```erlang
commit(Msg, Wallet, Format) when not is_map(Wallet) ->
    commit(Msg, #{ priv_wallet => Wallet }, Format);
```

### commit

```erlang
commit(Msg, Opts, CodecName) when is_binary(CodecName) ->
    commit(Msg, Opts, #{ <<"commitment-device">> => CodecName });
```

### commit

```erlang
commit(Msg, Opts, Spec) ->
    {ok, Signed} =
        dev_message:commit(
            Msg,
            Spec#{
                <<"commitment-device">> =>
                    case hb_maps:get(<<"commitment-device">>, Spec, none, Opts) of
                        none ->
                            case hb_maps:get(<<"device">>, Spec, none, Opts) of
                                none ->
                                    throw(
                                        {
                                            no_commitment_device_in_codec_spec,
                                            Spec
                                        }
                                    );
                                Device -> Device
                            end;
                        CommitmentDevice -> CommitmentDevice
                    end
            },
            Opts
        ),
    Signed.
```

### committed

Return the list of committed keys from a message.

```erlang
committed(Msg, all, Opts) ->
    committed(Msg, #{ <<"committers">> => <<"all">> }, Opts);
```

### committed

Return the list of committed keys from a message.

```erlang
committed(Msg, none, Opts) ->
    committed(Msg, #{ <<"committers">> => <<"none">> }, Opts);
```

### committed

Return the list of committed keys from a message.

```erlang
committed(Msg, List, Opts) when is_list(List) ->
    committed(Msg, #{ <<"commitments">> => List }, Opts);
```

### committed

Return the list of committed keys from a message.

```erlang
committed(Msg, CommittersMsg, Opts) ->
    ?event(
        {committed,
            {msg, {explicit, Msg}},
            {committers_msg, {explicit, CommittersMsg}},
            {opts, Opts}
        }
    ),
    {ok, CommittedKeys} = dev_message:committed(Msg, CommittersMsg, Opts),
    CommittedKeys.
```

### verify

wrapper function to verify a message.

```erlang
verify(Msg) -> verify(Msg, all).
```

### verify

wrapper function to verify a message.

```erlang
verify(Msg, Committers) ->
    verify(Msg, Committers, #{}).
```

### verify

```erlang
verify(Msg, all, Opts) ->
    verify(Msg, <<"all">>, Opts);
```

### verify

```erlang
verify(Msg, signers, Opts) ->
    verify(Msg, hb_message:signers(Msg, Opts), Opts);
```

### verify

```erlang
verify(Msg, Committers, Opts) when not is_map(Committers) ->
    verify(
        Msg,
        #{
            <<"committers">> =>
                case ?IS_ID(Committers) of
                    true -> [Committers];
                    false -> Committers
                end
        },
        Opts
    );
```

### verify

```erlang
verify(Msg, Spec, Opts) ->
    ?event(verify, {verify, {spec, Spec}}),
    {ok, Res} =
        dev_message:verify(
            Msg,
            Spec,
            Opts
        ),
    Res.
```

### uncommitted

Return the unsigned version of a message in AO-Core format.

```erlang
uncommitted(Msg) -> uncommitted(Msg, #{}).
```

### uncommitted

Return the unsigned version of a message in AO-Core format.

```erlang
uncommitted(Bin, _Opts) when is_binary(Bin) -> Bin;
```

### uncommitted

Return the unsigned version of a message in AO-Core format.
Return all of the committers on a message that have 'normal', 256 bit, 

```erlang
uncommitted(Msg, Opts) ->
    hb_maps:remove(<<"commitments">>, Msg, Opts).
```

### signers

Return the unsigned version of a message in AO-Core format.
Return all of the committers on a message that have 'normal', 256 bit, 

```erlang
signers(Msg, Opts) ->
    hb_util:ok(dev_message:committers(Msg, #{}, Opts)).
```

### print

Pretty-print a message.

```erlang
print(Msg) -> print(Msg, 0).
```

### print

Pretty-print a message.
Return the type of an encoded message.

```erlang
print(Msg, Indent) ->
    io:format(standard_error, "~s", [lists:flatten(hb_format:message(Msg, #{}, Indent))]).
```

### type

Pretty-print a message.
Return the type of an encoded message.

```erlang
type(TX) when is_record(TX, tx) -> tx;
```

### type

Pretty-print a message.
Return the type of an encoded message.

```erlang
type(Binary) when is_binary(Binary) -> binary;
```

### type

Pretty-print a message.
Return the type of an encoded message.

```erlang
type(Msg) when is_map(Msg) ->
    IsDeep = lists:any(
        fun({_, Value}) -> is_map(Value) end,
        lists:filter(
            fun({Key, _}) -> not hb_private:is_private(Key) end,
            hb_maps:to_list(Msg)
        )
    ),
    case IsDeep of
        true -> deep;
        false -> shallow
    end.
```

### match

Check if two maps match, including recursively checking nested maps.

```erlang
match(Map1, Map2) ->
    match(Map1, Map2, strict).
```

### match

```erlang
match(Map1, Map2, Mode) ->
    match(Map1, Map2, Mode, #{}).
```

### match

```erlang
match(Map1, Map2, Mode, Opts) ->
    try unsafe_match(Map1, Map2, Mode, [], Opts)
    catch _:Details -> Details
    end.
```

### unsafe_match

Match two maps, returning `true` if they match, or throwing an error

```erlang
unsafe_match(Map1, Map2, Mode, Path, Opts) ->
    Keys1 =
        hb_maps:keys(
            NormMap1 = hb_util:lower_case_key_map(minimize(
                normalize(hb_ao:normalize_keys(Map1, Opts), Opts),
                [<<"content-type">>, <<"ao-body-key">>]
            ), Opts)
        ),
    Keys2 =
        hb_maps:keys(
            NormMap2 = hb_util:lower_case_key_map(minimize(
                normalize(hb_ao:normalize_keys(Map2, Opts), Opts),
                [<<"content-type">>, <<"ao-body-key">>]
            ), Opts)
        ),
    PrimaryKeysPresent =
        (Mode == primary) andalso
            lists:all(
                fun(Key) -> lists:member(Key, Keys1) end,
                Keys1
            ),
    ?event(match,
        {match,
            {keys1, Keys1},
            {keys2, Keys2},
            {mode, Mode},
            {primary_keys_present, PrimaryKeysPresent},
            {msg1, Map1},
            {msg2, Map2}
        }
    ),
    case (Keys1 == Keys2) or (Mode == only_present) or PrimaryKeysPresent of
        true ->
            lists:all(
                fun(Key) ->
                    ?event(match, {matching_key, Key}),
                    Val1 =
                        hb_ao:normalize_keys(
                            hb_maps:get(Key, NormMap1, not_found, Opts),
                            Opts
                        ),
                    Val2 =
                        hb_ao:normalize_keys(
                            hb_maps:get(Key, NormMap2, not_found, Opts),
                            Opts
                        ),
                    BothPresent = (Val1 =/= not_found) and (Val2 =/= not_found),
                    case (not BothPresent) and (Mode == only_present) of
                        true -> true;
                        false ->
                            case is_map(Val1) andalso is_map(Val2) of
                                true ->
                                    unsafe_match(Val1, Val2, Mode, Path ++ [Key], Opts);
                                false ->
                                    case {Val1, Val2} of
                                        {V, V} -> true;
                                        {V, '_'} when V =/= not_found -> true;
                                        {'_', V} when V =/= not_found -> true;
                                        {'_', '_'} -> true;
                                        _ ->
                                            throw(
                                                {value_mismatch,
                                                    hb_format:short_id(
                                                        hb_path:to_binary(
                                                            Path ++ [Key]
                                                        )
                                                    ),
                                                    {val1, Val1},
                                                    {val2, Val2}
                                                }
                                            )
                                    end
                            end
                    end
                end,
                Keys1
            );
        false ->
            throw(
                {keys_mismatch,
                    {path, hb_format:short_id(hb_path:to_binary(Path))},
                    {keys1, Keys1},
                    {keys2, Keys2}
                }
            )
    end.
```

### matchable_keys

```erlang
matchable_keys(Map) ->
    lists:sort(lists:map(fun hb_ao:normalize_key/1, hb_maps:keys(Map))).
```

### diff

Return the numeric differences between two messages, matching deeply

```erlang
diff(Msg1, Msg2, Opts) when is_map(Msg1) andalso is_map(Msg2) ->
    maps:filtermap(
        fun(Key, Val2) ->
            case hb_maps:get(Key, Msg1, not_found, Opts) of
                Val2 ->
                    % The key is present in both maps, and the values match.
```

### diff

```erlang
diff(_Val1, _Val2, _Opts) ->
    not_found.
```

### with_commitments

Filter messages that do not match the 'spec' given. The underlying match

```erlang
with_commitments(ID, Msg, Opts) when ?IS_ID(ID) ->
    with_commitments([ID], Msg, Opts);
```

### with_commitments

Filter messages that do not match the 'spec' given. The underlying match

```erlang
with_commitments(Spec, Msg = #{ <<"commitments">> := Commitments }, Opts) ->
    ?event({with_commitments, {spec, Spec}, {commitments, Commitments}}),
    FilteredCommitments =
        hb_maps:filter(
            fun(ID, CommMsg) ->
                if is_list(Spec) ->
                    lists:member(ID, Spec);
                is_map(Spec) ->
                    match(Spec, CommMsg, primary, Opts) == true
                end
            end,
            Commitments,
            Opts
        ),
    ?event({with_commitments, {filtered_commitments, FilteredCommitments}}),
    Msg#{ <<"commitments">> => FilteredCommitments };
```

### with_commitments

Filter messages that do not match the 'spec' given. The underlying match

```erlang
with_commitments(_Spec, Msg, _Opts) ->
    Msg.
```

### without_commitments

Filter messages that match the 'spec' given. Inverts the `with_commitments/2`

```erlang
without_commitments(Spec, Msg = #{ <<"commitments">> := Commitments }, Opts) ->
    ?event({without_commitments, {spec, Spec}, {msg, Msg}, {commitments, Commitments}}),
    FilteredCommitments =
        hb_maps:without(
            hb_maps:keys(
                hb_maps:get(
                    <<"commitments">>,
                    with_commitments(Spec, Msg, Opts),
                    #{},
                    Opts
                )
            ),
            Commitments
        ),
    ?event({without_commitments, {filtered_commitments, FilteredCommitments}}),
    Msg#{ <<"commitments">> => FilteredCommitments };
```

### without_commitments

Filter messages that match the 'spec' given. Inverts the `with_commitments/2`

```erlang
without_commitments(_Spec, Msg, _Opts) ->
    Msg.
```

### commitment

Extract a commitment from a message given a `committer` or `commitment`

```erlang
commitment(ID, Msg) ->
    commitment(ID, Msg, #{}).
```

### commitment

```erlang
commitment(ID, Link, Opts) when ?IS_LINK(Link) ->
    commitment(ID, hb_cache:ensure_loaded(Link, Opts), Opts);
```

### commitment

```erlang
commitment(ID, #{ <<"commitments">> := Commitments }, Opts)
        when is_binary(ID), is_map_key(ID, Commitments) ->
    hb_maps:get(
        ID,
        Commitments,
        not_found,
        Opts
    );
```

### commitment

```erlang
commitment(Spec, Msg, Opts) ->
    Matches = commitments(Spec, Msg, Opts),
    ?event(debug_commitment, {commitment, {spec, Spec}, {matches, Matches}}),
    if
        map_size(Matches) == 0 -> not_found;
        map_size(Matches) == 1 ->
            CommID = hd(hb_maps:keys(Matches)),
            {ok, CommID, hb_util:ok(hb_maps:find(CommID, Matches, Opts))};
        true ->
            ?event(commitment, {multiple_matches, {matches, Matches}}),
            multiple_matches
    end;
```

### commitment

```erlang
commitment(_Spec, _Msg, _Opts) ->
    % The message has no commitments, so the spec can never match.
```

### commitments

Return a list of all commitments that match the spec.

```erlang
commitments(ID, Link, Opts) when ?IS_LINK(Link) ->
    commitments(ID, hb_cache:ensure_loaded(Link, Opts), Opts);
```

### commitments

Return a list of all commitments that match the spec.

```erlang
commitments(CommitterID, Msg, Opts) when is_binary(CommitterID) ->
    commitments(#{ <<"committer">> => CommitterID }, Msg, Opts);
```

### commitments

Return a list of all commitments that match the spec.

```erlang
commitments(Spec, #{ <<"commitments">> := Commitments }, Opts) ->
    hb_maps:filtermap(
        fun(_ID, CommMsg) ->
            case match(Spec, CommMsg, primary, Opts) of
                true -> {true, CommMsg};
                _ -> false
            end
        end,
        Commitments,
        Opts
    );
```

### commitments

Return a list of all commitments that match the spec.

```erlang
commitments(_Spec, _Msg, _Opts) ->
    #{}.
```

### commitment_devices

Return the devices for which there are commitments on a message.

```erlang
commitment_devices(#{ <<"commitments">> := Commitments }, Opts) ->
    lists:map(
        fun(CommMsg) ->
            hb_ao:get(<<"commitment-device">>, CommMsg, Opts)
        end,
        maps:values(Commitments)
    );
```

### commitment_devices

Return the devices for which there are commitments on a message.

```erlang
commitment_devices(_Msg, _Opts) ->
    [].
```

### find_target

Implements a standard pattern in which the target for an operation is

```erlang
find_target(Self, Req, Opts) ->
	GetOpts = Opts#{
        hashpath => ignore,
        cache_control => [<<"no-cache">>, <<"no-store">>]
    },
    {ok,
        case hb_maps:get(<<"target">>, Req, <<"self">>, GetOpts) of
            <<"self">> -> Self;
            Key ->
                hb_maps:get(
                    Key,
                    Req,
                    hb_maps:get(<<"body">>, Req, GetOpts),
                    GetOpts
                )
        end
    }.
```

### minimize

Remove keys from the map that can be regenerated. Optionally takes an

```erlang
minimize(Msg) -> minimize(Msg, []).
```

### minimize

Remove keys from the map that can be regenerated. Optionally takes an

```erlang
minimize(RawVal, _) when not is_map(RawVal) -> RawVal;
```

### minimize

Remove keys from the map that can be regenerated. Optionally takes an

```erlang
minimize(Map, ExtraKeys) ->
    NormKeys =
        lists:map(fun hb_ao:normalize_key/1, ?REGEN_KEYS)
            ++ lists:map(fun hb_ao:normalize_key/1, ExtraKeys),
    maps:filter(
        fun(Key, _) ->
            (not lists:member(hb_ao:normalize_key(Key), NormKeys))
                andalso (not hb_private:is_private(Key))
        end,
        maps:map(fun(_K, V) -> minimize(V) end, Map)
    ).
```

### normalize

Return a map with only the keys that necessary, without those that can

```erlang
normalize(Map, Opts) when is_map(Map) orelse is_list(Map) ->
    NormalizedMap = hb_ao:normalize_keys(Map, Opts),
    FilteredMap = filter_default_keys(NormalizedMap),
    hb_maps:with(matchable_keys(FilteredMap), FilteredMap);
```

### normalize

Return a map with only the keys that necessary, without those that can

```erlang
normalize(Other, _Opts) ->
    Other.
```

### filter_default_keys

Remove keys from a map that have the default values found in the tx

```erlang
filter_default_keys(Map) ->
    DefaultsMap = default_tx_message(),
    maps:filter(
        fun(Key, Value) ->
            case hb_maps:find(hb_ao:normalize_key(Key), DefaultsMap) of
                {ok, Value} -> false;
                _ -> true
            end
        end,
        Map
    ).
```

### default_tx_message

Get the normalized fields and default values of the tx record.

```erlang
default_tx_message() ->
    hb_maps:from_list(default_tx_list()).
```

### default_tx_list

Get the ordered list of fields as AO-Core keys and default values of

```erlang
default_tx_list() ->
    Keys = lists:map(fun hb_ao:normalize_key/1, record_info(fields, tx)),
```

---

*Generated from [hb_message.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_message.erl)*
