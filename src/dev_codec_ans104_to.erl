%%% @doc Library functions for encoding messages to the ANS-104 format.
-module(dev_codec_ans104_to).
-export([maybe_load/3, siginfo/2, data/3, tags/4]).
-include("include/hb.hrl").

%% @doc Determine if the message should be loaded from the cache and re-converted
%% to the TABM format. We do this if the `bundle' key is set to true.
maybe_load(RawTABM, Req, Opts) ->
    case hb_util:atom(hb_ao:get(<<"bundle">>, Req, false, Opts)) of
        false -> RawTABM;
        true ->
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
            LoadedTABM#{ <<"commitments">> => LoadedComms }
    end.

%% @doc Calculate the fields for a message, returning an initial TX record.
%% One of the nuances here is that the `target' field must be set correctly.
%% If the message has a commitment, we extract the `field-target' if found and
%% place it in the `target' field. If the message does not have a commitment,
%% we check if the `target' field is set in the message. If it is encodable as
%% a valid 32-byte binary ID (assuming it is base64url encoded in the `to' call),
%% we place it in the `target' field. Otherwise, we leave it unset.
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

%% @doc Convert a commitment to a base TX record. Extracts the owner, signature,
%% tags, and last TX from the commitment. If the value is not present, the
%% default value is used.
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
    % If there are too many keys in the TABM, throw an error.
    if map_size(UncommittedTABM) > ?MAX_TAG_COUNT ->
        throw({too_many_keys, UncommittedTABM});
    true ->
        % If there are less than 128 keys, we return those that are large, or
        % are nested messages.
        hb_maps:filter(
            fun(Key, Value) ->
                case is_map(Value) of
                    true -> true;
                    false -> byte_size(Value) > ?MAX_TAG_VALUE_SIZE orelse byte_size(Key) > ?MAX_TAG_NAME_SIZE
                end
            end,
            UncommittedTABM,
            Opts
        )
    end.

%% @doc Calculate the tags field for a data item. If the TX already has tags
%% from the commitment decoding step, we use them. Otherwise we determine the
%% keys to use from the `committed' field of the TABM.
tags(#tx{ tags = ExistingTags }, _, _, _) when ExistingTags =/= [] ->
    ExistingTags;
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
                % We normalize each committed key to whichever form actually
                % exists in the current TABM to avoid missing keys.
                lists:map(
                    fun(CommittedKey) ->
                        NormalizedKey = hb_ao:normalize_key(CommittedKey),
                        BaseKey = hb_link:remove_link_specifier(NormalizedKey),
                        case hb_maps:find(BaseKey, TABM, Opts) of
                            {ok, _} -> BaseKey;
                            error ->
                                BaseKeyLink = <<BaseKey/binary, "+link">>,
                                case hb_maps:find(BaseKeyLink, TABM, Opts) of
                                    {ok, _} -> BaseKeyLink;
                                    error -> BaseKey
                                end
                        end
                    end,
                    hb_util:message_to_ordered_list(
                        hb_util:ok(
                            hb_maps:find(<<"committed">>, Commitment, Opts)
                        )
                    )
                ) --
                    % If the target is set in the base TX from the
                    % commitment, we check if the TABM equals that value. If it does,
                    % we do not additionally add the target tag. If they differ, we
                    % include it.
                    case include_target_tag(TX, TABM, Opts) of
                        false -> [<<"target">>];
                        true -> []
                    end;
            not_found ->
                % There is no commitment, so we need to generate the tags. The
                % bundle-format and bundle-version tags are added by `ar_bundles`
                % so we do not add them here. The ao-data-key tag is added if it
                % is set to a non-default value, followed by the keys from the
                % TABM (less the data keys and target key -- see 
                % `include_target_tag/3` for rationale).
                hb_util:list_without(
                    [<<"commitments">>] ++
                        if is_map(Data) -> hb_maps:keys(Data, Opts);
                        true -> []
                        end ++
                        case include_target_tag(TX, TABM, Opts) of
                            false -> [<<"target">>];
                            true -> []
                        end,
                    hb_util:to_sorted_keys(hb_private:reset(TABM), Opts)
                );
            multiple_matches ->
                throw({multiple_ans104_commitments_unsupported, TABM})
        end,
    ?event(
        {tags_before_data_key,
            {committed_tag_keys, CommittedTagKeys},
            {data_key, DataKey},
            {data, Data},
            {tabm, TABM}
        }),
    committed_tag_keys_to_tags(TX, TABM, DataKey, CommittedTagKeys, Opts).

%% @doc Return whether to include the `target' tag in the tags list.
include_target_tag(TX, TABM, Opts) ->
    case {TX#tx.target, hb_maps:get(<<"target">>, TABM, undefined, Opts)} of
        {?DEFAULT_TARGET, _} -> true;
        {FieldTarget, TagTarget} when FieldTarget =/= TagTarget -> false;
        _ -> true
    end.

%% @doc Apply the `ao-data-key' to the committed keys to generate the list of
%% tags to include in the message.
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