%%% @doc Library functions for encoding messages to the ANS-104 format.
-module(dev_codec_ans104_to).
-export([is_bundle/3, maybe_load/3, data/3, tags/5, excluded_tags/3]).
-export([siginfo/4, fields_to_tx/4]).
-include("include/hb.hrl").

is_bundle({ok, _, Commitment}, _Req, Opts) ->
    hb_util:atom(hb_ao:get(<<"bundle">>, Commitment, false, Opts));
is_bundle(_, Req, Opts) ->
    case hb_maps:is_key(<<"bundle">>, Req, Opts) of
        true -> hb_util:atom(hb_ao:get(<<"bundle">>, Req, false, Opts));
        false -> hb_util:atom(hb_ao:get(<<"bundle">>, Opts, false, Opts))
    end.

%% @doc Determine if the message should be loaded from the cache and re-converted
%% to the TABM format. We do this if the `bundle' key is set to true.
maybe_load(RawTABM, true, Opts) ->
    % Convert back to the fully loaded structured@1.0 message, then
    % convert to TABM with bundling enabled.
    Structured = hb_message:convert(RawTABM, <<"structured@1.0">>, Opts),
    Loaded = hb_cache:ensure_all_loaded(Structured, Opts),
    % Convert to TABM with bundling enabled.
    LoadedTABM =
        hb_message:convert(
            Loaded,
            tabm,
            #{
                <<"device">> => <<"structured@1.0">>,
                <<"bundle">> => true
            },
            Opts
        ),
    % Ensure the commitments from the original message are the only
    % ones in the fully loaded message.
    LoadedComms = maps:get(<<"commitments">>, RawTABM, #{}),
    LoadedTABM#{ <<"commitments">> => LoadedComms };
maybe_load(RawTABM, false, _Opts) ->
    RawTABM.

%% @doc Calculate the fields for a message, returning an initial TX record.
%% One of the nuances here is that the `target' field must be set correctly.
%% If the message has a commitment, we extract the `field-target' if found and
%% place it in the `target' field. If the message does not have a commitment,
%% we check if the `target' field is set in the message. If it is encodable as
%% a valid 32-byte binary ID (assuming it is base64url encoded in the `to' call),
%% we place it in the `target' field. Otherwise, we leave it unset.
siginfo(_Message, {ok, _, Commitment}, FieldsFun, Opts) ->
    commitment_to_tx(Commitment, FieldsFun, Opts);
siginfo(Message, not_found, FieldsFun, Opts) ->
    FieldsFun(#tx{}, <<>>, Message, Opts);
siginfo(Message, multiple_matches, _FieldsFun, _Opts) ->
    throw({multiple_ans104_commitments_unsupported, Message}).

%% @doc Convert a commitment to a base TX record. Extracts the owner, signature,
%% tags, and last TX from the commitment. If the value is not present, the
%% default value is used.
commitment_to_tx(Commitment, FieldsFun, Opts) ->
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
    ?event({commitment_owner, Owner}),
    ?event({commitment_signature, Signature}),
    ?event({commitment_tags, Tags}),
    TX = #tx{
        owner = Owner,
        signature = Signature,
        tags = Tags
    },
    FieldsFun(TX, ?FIELD_PREFIX, Commitment, Opts).


%% @doc Convert a HyperBEAM-compatible map into an ANS-104 encoded tag list,
%% recreating the original order of the tags.
original_tags_to_tags(TagMap) ->
    OrderedList = hb_util:message_to_ordered_list(hb_private:reset(TagMap)),
    ?event({ordered_tagmap, {explicit, OrderedList}, {input, {explicit, TagMap}}}),
    lists:map(
        fun(#{ <<"name">> := Key, <<"value">> := Value }) ->
            {Key, Value}
        end,
        OrderedList
    ).

fields_to_tx(TX, Prefix, Map, Opts) ->
    Anchor =
        case hb_maps:find(<<Prefix/binary, "anchor">>, Map, Opts) of
            {ok, EncodedAnchor} ->
                case hb_util:safe_decode(EncodedAnchor) of
                    {ok, DecodedAnchor} when ?IS_ID(DecodedAnchor) ->
                        DecodedAnchor;
                    _ -> ?DEFAULT_ANCHOR
                end;
            error -> ?DEFAULT_ANCHOR
        end,
    Target =
        case hb_maps:find(<<Prefix/binary, "target">>, Map, Opts) of
            {ok, EncodedTarget} ->
                case hb_util:safe_decode(EncodedTarget) of
                    {ok, DecodedTarget} when ?IS_ID(DecodedTarget) -> 
                        DecodedTarget;
                    _ -> ?DEFAULT_TARGET
                end;
            error -> ?DEFAULT_TARGET
        end,
    ?event({fields_to_tx, {prefix, Prefix}, {anchor, Anchor}, {target, Target}}),
    TX#tx{
        anchor = Anchor,
        target = Target
    }.

%% @doc Calculate the data field for a message.
data(TABM, Req, Opts) ->
    DataKey = inline_key(TABM),
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages.
    UnencodedNestedMsgs = data_messages(TABM, Opts),
    NestedMsgs =
        hb_maps:map(
            fun(_, Msg) ->
                hb_util:ok(dev_codec_ans104:to(Msg, Req, Opts))
            end,
            UnencodedNestedMsgs,
            Opts
        ),
    DataVal = hb_maps:get(DataKey, TABM, ?DEFAULT_DATA),
    ?event(debug_data, {data_val, DataVal}),
    case {DataVal, hb_maps:size(NestedMsgs, Opts)} of
        {Binary, 0} when is_binary(Binary) ->
            % There are no nested messages, so we return the binary alone.
            Binary;
        {?DEFAULT_DATA, _} ->
            NestedMsgs;
        {DataVal, _} ->
            NestedMsgs#{
                DataKey => hb_util:ok(dev_codec_ans104:to(DataVal, Req, Opts))
            }
    end.

%% @doc Calculate the data value for a message. The rules are:
%% 1. There should be no more than 128 keys in the tags.
%% 2. Each key must be equal or less to 1024 bytes.
%% 3. Each value must be equal or less to 3072 bytes.
%% Presently, if we exceed these limits, we throw an error.
data_messages(TABM, Opts) when is_map(TABM) ->
    UncommittedTABM =
        hb_maps:without(
            [<<"commitments">>, <<"data">>, <<"target">>],
            hb_private:reset(TABM),
            Opts
        ),
    
    % Find keys that are too large or are nested messages, they will be
    % encoded as data messages.
    DataMessages = hb_maps:filter(
        fun(Key, Value) ->
            case is_map(Value) of
                true -> true;
                false -> byte_size(Value) > ?MAX_TAG_VALUE_SIZE orelse byte_size(Key) > ?MAX_TAG_NAME_SIZE
            end
        end,
        UncommittedTABM,
        Opts
    ),
    % If the remaining keys are too many to put in tags, throw an error.
    TagCount = map_size(UncommittedTABM) - map_size(DataMessages),
    if TagCount > ?MAX_TAG_COUNT ->
        throw({too_many_keys, UncommittedTABM});
    true ->
        DataMessages
    end.

%% @doc Calculate the tags field for a data item. If the TX already has tags
%% from the commitment decoding step, we use them. Otherwise we determine the
%% keys to use from the commitment.
tags(#tx{ tags = ExistingTags }, _, _, _, _) when ExistingTags =/= [] ->
    ExistingTags;
tags(TX, MaybeCommitment, TABM, ExcludedTagKeys, Opts) ->
    CommittedTagKeys = committed_tag_keys(MaybeCommitment, TABM, Opts),
    DataKeysToExclude =
        case TX#tx.data of
            Data when is_map(Data)-> maps:keys(Data);
            _ -> []
        end,
    TagKeys = hb_util:list_without(
        ExcludedTagKeys ++ DataKeysToExclude, 
        CommittedTagKeys
    ),
    Tags =
        bundle_tags_to_tags(MaybeCommitment) ++
        committed_tag_keys_to_tags(TABM, TagKeys, Opts),
    Tags.

committed_tag_keys({ok, _, Commitment}, TABM, Opts) ->
    % There is already a commitment, so the tags and order are
    % pre-determined. However, if the message has been bundled,
    % any `+link`-suffixed keys in the committed list may need to
    % be resolved to their base keys (e.g., `output+link` -> `output`).
    % We normalize each committed key to whichever form actually
    % exists in the current TABM to avoid missing keys.
    lists:map(
        fun(CommittedKey) ->
            NormalizedKey = hb_ao:normalize_key(CommittedKey),
            BaseKey = hb_link:remove_link_specifier(NormalizedKey),
            case dev_arweave_common:find_key(BaseKey, TABM, Opts) of
                error -> BaseKey;
                {FoundKey, _} -> FoundKey
            end
        end,
        hb_util:message_to_ordered_list(
            hb_util:ok(
                hb_maps:find(<<"committed">>, Commitment, Opts)
            )
        )
    );
committed_tag_keys(not_found, TABM, Opts) ->
    % There is no commitment, so we need to generate the tags. The
    % bundle-format and bundle-version tags are added by
    % `ar_bundles` so we do not add them here. The ao-data-key tag
    % is added if it is set to a non-default value, followed by the
    % keys from the TABM (less the data keys and target key -- see
    % `include_target_tag/3` for rationale).
    hb_util:list_without(
        [<<"commitments">>],
        hb_util:to_sorted_keys(hb_private:reset(TABM), Opts)
    );
committed_tag_keys(multiple_matches, TABM, _Opts) ->
    throw({multiple_ans104_commitments_unsupported, TABM}).

%% @doc Return a list of base fields that should be excluded from the tags
%% lists
excluded_tags(TX, TABM, Opts) ->
    exclude_target_tag(TX, TABM, Opts) ++
    exclude_anchor_tag(TX, TABM, Opts).

exclude_target_tag(TX, TABM, Opts) ->
    case {TX#tx.target, hb_maps:get(<<"target">>, TABM, undefined, Opts)} of
        {?DEFAULT_TARGET, _} -> [];
        {FieldTarget, TagTarget} when FieldTarget =/= TagTarget -> 
            [<<"target">>];
        _ -> []
    end.

exclude_anchor_tag(TX, TABM, Opts) ->
    case {TX#tx.anchor, hb_maps:get(<<"anchor">>, TABM, undefined, Opts)} of
        {?DEFAULT_ANCHOR, _} -> [];
        {FieldAnchor, TagAnchor} when FieldAnchor =/= TagAnchor -> 
            [<<"anchor">>];
        _ -> []
    end.

%% @doc Apply the `ao-data-key' to the committed keys to generate the list of
%% tags to include in the message.
committed_tag_keys_to_tags(TABM, Committed, Opts) ->
    DataKey = inline_key(TABM),
    ?event(
        {tags_before_data_key,
            {tag_keys, Committed},
            {data_key, DataKey},
            {tabm, TABM}
        }),
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
        hb_util:list_without([DataKey], Committed)
    ).

bundle_tags_to_tags({ok, _, Commitment}) ->
    lists:flatmap(
        fun(Key) ->
            case hb_maps:find(Key, Commitment) of
                {ok, Value} ->
                    [{Key, Value}];
                error ->
                    []
            end
        end,
        ?BUNDLE_KEYS
    );
bundle_tags_to_tags(_) ->
    [].

%%% Utility functions
    
%% @doc Determine if an `ao-data-key` should be added to the message.
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
            InlineKey;
        {_, true, true} -> 
            % There is no specific data field set, but there is a body, so we
            % use that as the `inline-key`.
            <<"body">>;
        _ ->
            % Default: `data' resolves to `data'.
            <<"data">>
    end.
