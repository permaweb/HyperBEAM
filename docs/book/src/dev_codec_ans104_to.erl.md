# dev_codec_ans104_to

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_ans104_to.erl)

Library functions for encoding messages to the ANS-104 format.

---

## Exported Functions

- `data/3`
- `maybe_load/3`
- `siginfo/2`
- `tags/4`

---

### maybe_load

Library functions for encoding messages to the ANS-104 format.
Determine if the message should be loaded from the cache and re-converted

```erlang
maybe_load(RawTABM, Req, Opts) ->
    case hb_util:atom(hb_ao:get(<<"bundle">>, Req, false, Opts)) of
        false -> RawTABM;
        true ->
            % Convert back to the fully loaded structured@1.0 message, then
            % convert to TABM with bundling enabled.
```

### siginfo

Calculate the fields for a message, returning an initial TX record.

```erlang
siginfo(Message, Opts) ->
    MaybeCommitment =
        hb_message:commitment(
            #{ <<"commitment-device">> => <<"ans104@1.0">> },
            Message,
            Opts
        ),
    case MaybeCommitment of
        {ok, _, Commitment} -> commitment_to_tx(Commitment, Opts);
        not_found ->
            case hb_maps:find(<<"target">>, Message, Opts) of
                {ok, EncodedTarget} ->
                    case hb_util:safe_decode(EncodedTarget) of
                        {ok, Target} when ?IS_ID(Target) ->
                            #tx{ target = Target };
                        _ -> #tx{}
                    end;
                error -> #tx{}
            end;
        multiple_matches ->
            throw({multiple_ans104_commitments_unsupported, Message})
    end.
```

### commitment_to_tx

Convert a commitment to a base TX record. Extracts the owner, signature,

```erlang
commitment_to_tx(Commitment, Opts) ->
    Signature =
        hb_util:decode(
            maps:get(<<"signature">>, Commitment, hb_util:encode(?DEFAULT_SIG))
        ),
    Owner =
        case hb_maps:find(<<"keyid">>, Commitment, Opts) of
            {ok, KeyID} ->
                hb_util:decode(
                    dev_codec_httpsig_keyid:remove_scheme_prefix(KeyID)
                );
            error -> ?DEFAULT_OWNER
        end,
    Tags =
        case hb_maps:find(<<"original-tags">>, Commitment, Opts) of
            {ok, OriginalTags} -> original_tags_to_tags(OriginalTags);
            error -> []
        end,
    LastTX =
        case hb_maps:find(<<"field-anchor">>, Commitment, Opts) of
            {ok, EncodedLastTX} -> hb_util:decode(EncodedLastTX);
            error -> ?DEFAULT_LAST_TX
        end,
    Target =
        case hb_maps:find(<<"field-target">>, Commitment, Opts) of
            {ok, EncodedTarget} -> hb_util:decode(EncodedTarget);
            error -> ?DEFAULT_TARGET
        end,
    ?event({commitment_owner, Owner}),
    ?event({commitment_signature, Signature}),
    ?event({commitment_tags, Tags}),
    ?event({commitment_last_tx, LastTX}),
    #tx{
        owner = Owner,
        signature = Signature,
        tags = Tags,
        anchor = LastTX,
        target = Target
    }.
```

### data

Calculate the data field for a message.

```erlang
data(TABM, Req, Opts) ->
    DataKey = inline_key(TABM),
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages.
```

### data_messages

Calculate the data value for a message. The rules are:

```erlang
data_messages(TABM, Opts) when is_map(TABM) ->
    UncommittedTABM =
        hb_maps:without(
            [<<"commitments">>, <<"data">>, <<"target">>],
            hb_private:reset(TABM),
            Opts
        ),
    % If there are too many keys in the TABM, throw an error.
```

### tags

Calculate the tags field for a data item. If the TX already has tags

```erlang
tags(#tx{ tags = ExistingTags }, _, _, _) when ExistingTags =/= [] ->
    ExistingTags;
```

### tags

Calculate the tags field for a data item. If the TX already has tags

```erlang
tags(TX, TABM, Data, Opts) ->
    DataKey = inline_key(TABM),
    MaybeCommitment =
        hb_message:commitment(
            #{ <<"commitment-device">> => <<"ans104@1.0">> },
            TABM,
            Opts
        ),
    CommittedTagKeys =
        case MaybeCommitment of
            {ok, _, Commitment} ->
                % There is already a commitment, so the tags and order are
                % pre-determined. However, if the message has been bundled,
                % any `+link`-suffixed keys in the committed list may need to
                % be resolved to their base keys (e.g., `output+link` -> `output`).
```

### include_target_tag

Return whether to include the `target` tag in the tags list.

```erlang
include_target_tag(TX, TABM, Opts) ->
    case {TX#tx.target, hb_maps:get(<<"target">>, TABM, undefined, Opts)} of
        {?DEFAULT_TARGET, _} -> true;
        {FieldTarget, TagTarget} when FieldTarget =/= TagTarget -> false;
        _ -> true
    end.
```

### committed_tag_keys_to_tags

Apply the `ao-data-key` to the committed keys to generate the list of

```erlang
committed_tag_keys_to_tags(TX, TABM, DataKey, Committed, Opts) ->
    DataKeysToExclude =
        case TX#tx.data of
            Data when is_map(Data)-> maps:keys(Data);
            _ -> []
        end,
    case DataKey of
        <<"data">> -> [];
        _ -> [{<<"ao-data-key">>, DataKey}]
    end ++
    lists:map(
        fun(Key) ->
            case hb_maps:find(Key, TABM, Opts) of
                error -> throw({missing_committed_key, Key});
                {ok, Value} -> {Key, Value}
            end
        end,
        hb_util:list_without(
            [DataKey | DataKeysToExclude],
            Committed
        )
    ).
```

### inline_key

Determine if an `ao-data-key` should be added to the message.

```erlang
inline_key(Msg) ->
    InlineKey = maps:get(<<"ao-data-key">>, Msg, undefined),
    case {
        InlineKey,
        maps:get(<<"data">>, Msg, ?DEFAULT_DATA) == ?DEFAULT_DATA,
        maps:is_key(<<"body">>, Msg)
            andalso not ?IS_LINK(maps:get(<<"body">>, Msg, undefined))
    } of
        {Explicit, _, _} when Explicit =/= undefined ->
            % ao-data-key already exists, so we honor it.
```

### original_tags_to_tags

Convert a HyperBEAM-compatible map into an ANS-104 encoded tag list,

```erlang
original_tags_to_tags(TagMap) ->
    OrderedList = hb_util:message_to_ordered_list(hb_private:reset(TagMap)),
    ?event({ordered_tagmap, {explicit, OrderedList}, {input, {explicit, TagMap}}}),
    lists:map(
        fun(#{ <<"name">> := Key, <<"value">> := Value }) ->
            {Key, Value}
        end,
        OrderedList
```

---

*Generated from [dev_codec_ans104_to.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_codec_ans104_to.erl)*
