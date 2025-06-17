%%% @doc Codec for managing transformations from `ar_bundles'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_ans104).
-export([to/3, from/3, commit/3, verify/3, content_type/1, normalize_data/1]).
-export([serialize/3, deserialize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 4096).
%% The list of TX fields that users can set directly. Data is excluded because
%% it may be set by the codec in order to support nested messages.
-define(TX_KEYS,
    [
        <<"last_tx">>,
        <<"owner">>,
        <<"target">>,
        <<"signature">>
    ]
).
%%% The list of keys that should be forced into the tag list, rather than being
%%% encoded as fields in the TX record.
-define(INVALID_FIELDS,
    [
        <<"id">>,
        <<"unsigned_id">>,
        <<"owner">>,
        <<"owner_address">>,
        <<"tags">>,
        <<"data_size">>,
        <<"data_tree">>,
        <<"signature">>,
        <<"signature_type">>
    ]
).
%%% The list of tags that a user is explicitly committing to when they sign an
%%% ANS-104 message.
-define(BASE_COMMITTED_TAGS, ?TX_KEYS ++ [<<"data">>]).
%% List of tags that should be removed during `to'. These relate to the nested
%% ar_bundles format that is used by the `ans104@1.0' codec.
-define(FILTERED_TAGS,
    [
        <<"bundle-format">>,
        <<"bundle-map">>,
        <<"bundle-version">>
    ]
).

%% @doc Return the content type for the codec.
content_type(_) -> {ok, <<"application/ans104">>}.

%% @doc Serialize a message or TX to a binary.
serialize(Msg, Req, Opts) when is_map(Msg) ->
    serialize(to(Msg, Req, Opts), Req, Opts);
serialize(TX, _Req, _Opts) when is_record(TX, tx) ->
    {ok, ar_bundles:serialize(TX)}.

%% @doc Deserialize a binary ans104 message to a TABM.
deserialize(#{ <<"body">> := Binary }, Req, Opts) ->
    deserialize(Binary, Req, Opts);
deserialize(Binary, Req, Opts) when is_binary(Binary) ->
    deserialize(ar_bundles:deserialize(Binary), Req, Opts);
deserialize(TX, Req, Opts) when is_record(TX, tx) ->
    from(TX, Req, Opts).

%% @doc Sign a message using the `priv_wallet' key in the options. Supports both
%% the `hmac-sha256' and `rsa-pss-sha256' algorithms, offering unsigned and
%% signed commitments.
commit(Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"unsigned-sha256">> }, Opts);
commit(Msg, Req = #{ <<"type">> := <<"signed">> }, Opts) ->
    commit(Msg, Req#{ <<"type">> => <<"rsa-pss-sha256">> }, Opts);
commit(Msg, Req = #{ <<"type">> := <<"rsa-pss-sha256">> }, Opts) ->
    % Convert the given message to an ANS-104 TX record, sign it, and convert
    % it back to a structured message.
    ?event({committing, {input, {explicit, Msg}}}),
    Signed =
        ar_bundles:sign_item(
            hb_util:ok(to(hb_private:reset(Msg), Req, Opts)),
            hb_opts:get(priv_wallet, no_viable_wallet, Opts)
        ),
    ?event({committing, {signed, {explicit, Signed}}}),
    SignedTABM =
        hb_message:convert(
            Signed,
            tabm,
            <<"ans104@1.0">>,
            Opts
        ),
    {ok, SignedTABM};
commit(Msg, #{ <<"type">> := <<"unsigned-sha256">> }, Opts) ->
    % Remove the commitments from the message, convert it to ANS-104, then back.
    % This forces the message to be normalized and the unsigned ID to be
    % recalculated.
    TX = hb_message:convert(
        hb_maps:without([<<"commitments">>], Msg, Opts),
        <<"ans104@1.0">>,
        tabm,
        Opts
    ),
    {
        ok,
        hb_message:convert(
            TX,
            tabm,
            <<"ans104@1.0">>,
            Opts
        )
    }.

%% @doc Verify an ANS-104 commitment.
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

%% @doc Convert a #tx record into a message map recursively.
from(Binary, _Req, _Opts) when is_binary(Binary) -> {ok, Binary};
from(TX, Req, Opts) when is_record(TX, tx) ->
    case lists:keyfind(<<"ao-type">>, 1, TX#tx.tags) of
        false ->
            do_from(TX, Req, Opts);
        {<<"ao-type">>, <<"binary">>} ->
            {ok, TX#tx.data}
    end.

%% @doc Translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
to(Binary, _Req, _Opts) when is_binary(Binary) ->
    % ar_bundles cannot serialize just a simple binary or get an ID for it, so
    % we turn it into a TX record with a special tag, tx_to_message will
    % identify this tag and extract just the binary.
    {ok,
        #tx{
            tags = [{<<"ao-type">>, <<"binary">>}],
            data = Binary
        }
    };
to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
to(InputTABM, Req, Opts) when is_map(InputTABM) ->
    NormalizedTABM = hb_ao:normalize_keys(normalize_data(InputTABM), Opts),
    ?event({to, {input_tabm, {explicit, NormalizedTABM}}}),

    TABM = maybe_bundle(NormalizedTABM, Req, Opts),

    % Check for invalid fields
    InvalidFields = [Field || Field <- ?INVALID_FIELDS, maps:is_key(Field, TABM)],
        case InvalidFields of
            [] -> ok;
            _ -> throw({invalid_fields, InvalidFields})
        end,
    
    % 1. Initialize the #tx record with the values from the TABM.
    {TX, RemainingTABM, OriginalTags} = apply_tabm_to_tx(#tx{}, TABM, Req, Opts),

    % 2. Ensure any links are loaded.
    LoadedTABM = hb_cache:ensure_all_loaded(RemainingTABM),
    ?event({to, {loaded_tabm, {explicit, LoadedTABM}}}),

    % 3. Extract tags and data items from the remaining TABM keys.
    {Tags, DataItems} = to_data_items(LoadedTABM, Req, Opts),

    % 4. Set the tags on the #tx record.
    TXWithoutData = set_tags(TX, Tags, OriginalTags, Opts),

    % 5. Set the data items on the #tx record.
    TXWithData = set_tx_data_or_throw(TXWithoutData, DataItems, Req, Opts),

    TXResult = ar_bundles:reset_ids(ar_bundles:normalize(TXWithData)),
    ?event({to, {result, TXResult}}),
    {ok, TXResult};
to(_Other, _Req, _Opts) ->
    throw(invalid_tx).

%%% ------------------------------------------------------------------------------------------
%%% from/3 helpers
%%% ------------------------------------------------------------------------------------------

do_from(RawTX, Req, Opts) ->
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(ar_bundles:normalize(RawTX)),
    
    % Initialize message from the transaction tags
    RawTags = deduplicating_from_list(TX#tx.tags, Opts),

    % 0. Add keys to the TABM for any non-default values in the #tx record.
    TABM0 = apply_tx_to_tabm(RawTags, TX, Opts),

    % 1. Strip out any fields that will be auto-computed
    TABM1 = maps:without(?INVALID_FIELDS, TABM0),

    % 2. Set the data field, if it's a map, we need to recursively turn its children
    %    into messages from their tx representations.
    TABM2 = set_map_data_or_throw(TABM1, TX, Req, Opts),

    % 3. Add the commitments to the message if the TX has a signature.
    TABM3 = hb_maps:without(?FILTERED_TAGS, add_commitments(TABM2, TX, Opts), Opts),

    ?event({do_from, {final_tabm, TABM3}}),
    {ok, TABM3}.


%% @doc Add keys to the TABM for any non-default values in the #tx record. Throws an error if
%% there's a value clash between the input TABM and #tx values.
apply_tx_to_tabm(InputTABM, TX, Opts) ->
    % We'll build up the output message first as a structured message, then we'll convert it
    % back to a TABM at the end. This is so that we can compare values to check for clashes - 
    % easier to do when the values are in their native form and not the aotypes/encoded form.
    Structured0 = hb_message:convert(
        hb_maps:with(hb_message:default_tx_keys(), InputTABM),
        <<"structured@1.0">>,
        Opts
    ),

    Structured1 = apply_to_structured(Structured0, TX),

    OutputTABM = hb_message:convert(
        Structured1,
        tabm,
        <<"structured@1.0">>,
        Opts
    ),

    hb_maps:merge(
        hb_maps:without(hb_message:default_tx_keys(), InputTABM, Opts),
        OutputTABM, Opts).

apply_to_structured(Structured, TX) ->
    % Process each field in the default TX list, excluding auto-computed fields and data.
    SkipFields = [<<"data">> | ?INVALID_FIELDS],
    lists:foldl(
        fun ({Field, DefaultValue}, AccStructured) ->
            case lists:member(Field, SkipFields) of
                true -> AccStructured;
                false ->
                    % Get the field value from TX
                    FieldIndex = field_index(hb_util:atom(Field)),
                    TXValue = element(FieldIndex, TX),
                    
                    % Only add to Structured if value is different from default
                    case TXValue of
                        DefaultValue -> AccStructured;
                        _ ->
                            % Try to coerce the value to match the expected type
                            case coerce_value(Field, TXValue, DefaultValue) of
                                {ok, CoercedValue} ->
                                    set_map_value_or_throw(
                                        hb_ao:normalize_key(Field), CoercedValue, AccStructured);
                                error ->
                                    AccStructured
                            end
                    end
            end
        end,
        Structured,
        hb_message:default_tx_list()
    ).
    
add_commitments(Structured, TX, Opts) ->
    OriginalTagMap = encoded_tags_to_map(TX#tx.tags),
    CommittedKeys =
        hb_ao:normalize_keys(
            hb_util:unique(
                ?BASE_COMMITTED_TAGS ++
                [
                    hb_ao:normalize_key(Tag)
                ||
                    {Tag, _} <- TX#tx.tags
                ] ++
                hb_util:to_sorted_keys(Structured)
            )
        ),
    WithCommitments =
        case TX#tx.signature of
            ?DEFAULT_SIG ->
                ?event({no_signature_detected, Structured}),
                case normal_tags(TX#tx.tags) of
                    true -> Structured;
                    false ->
                        ID = hb_util:human_id(TX#tx.unsigned_id),
                        Commitments = #{
                            ID => #{
                                <<"commitment-device">> => <<"ans104@1.0">>,
                                <<"type">> => <<"unsigned-sha256">>,
                                <<"original-tags">> => OriginalTagMap
                            }
                        },
                        set_map_value_or_throw( <<"commitments">>, Commitments, Structured)
                end;
            _ ->
                Address = hb_util:human_id(ar_wallet:to_address(TX#tx.owner)),
                WithoutBaseCommitment =
                    hb_maps:without(
                        [
                            <<"id">>,
                            <<"keyid">>,
                            <<"signature">>,
                            <<"commitment-device">>,
                            <<"original-tags">>
                        ],
                        Structured,
                        Opts
                    ),
                ID = hb_util:human_id(TX#tx.id),
                Commitment0 = #{
                    <<"commitment-device">> => <<"ans104@1.0">>,
                    <<"committer">> => Address,
                    <<"committed">> => CommittedKeys,
                    <<"keyid">> => hb_util:encode(TX#tx.owner),
                    <<"signature">> => hb_util:encode(TX#tx.signature),
                    <<"type">> => <<"rsa-pss-sha256">>
                },
                Commitment1 =
                    case lists:member(<<"bundle-format">>, maps:values(CommittedKeys)) of
                        true -> Commitment0#{ <<"bundle">> => true };
                        false -> Commitment0
                    end,
                Commitment2 = case normal_tags(TX#tx.tags) of
                    true -> Commitment1;
                    false ->
                        Commitment1#{
                            <<"original-tags">> => OriginalTagMap
                        }
                end,
                Commitments = #{ ID => Commitment2 },
                set_map_value_or_throw( <<"commitments">>, Commitments, WithoutBaseCommitment)
        end,
    WithCommitments.


set_map_value_or_throw(Key, NewValue, Map) ->
    case Map of
        #{Key := NewValue} -> Map;
        #{Key := ExistingValue} -> throw({invalid_map_value, Key, ExistingValue, NewValue});
        _ -> maps:put(Key, NewValue, Map)
    end.

set_map_data_or_throw(Map, TX, Req, Opts) ->
    DataMap = case TX#tx.data of
        Data when is_map(Data) ->
            % If the data is a map, we need to recursively turn its children
            % into messages from their tx representations.
            hb_maps:fold(
                fun(Key, InnerValue, Acc) ->
                    case from(InnerValue, Req, Opts) of
                        {ok, DecodedValue} ->
                            % throw if Key already exists in Map
                            set_map_value_or_throw(Key, DecodedValue, Acc);
                        _ -> Acc
                    end
                end,
                Map,
                Data
            );
        Data when Data == ?DEFAULT_DATA -> Map;
        Data when is_binary(Data) -> set_map_value_or_throw(<<"data">>, Data, Map);
        Data ->
            ?event({unexpected_data_type, {explicit, Data}}),
            ?event({was_processing, {explicit, TX}}),
            throw(invalid_tx)
    end,
    % Normalize the `data` field to the `ao-data-key's value, if set.
    case maps:get(<<"ao-data-key">>, DataMap, undefined) of
        undefined -> DataMap;
        DataKey ->
            % Remove the `data' and `ao-data-key' fields from the map, then
            % add the `data' field with the value of the `ao-data-key' field.
            NoDataMap = maps:without([<<"data">>, <<"ao-data-key">>], DataMap),
            NoDataMap#{
                DataKey => maps:get(<<"data">>, DataMap, ?DEFAULT_DATA)
            }
    end.
    

%% @doc Convert an ANS-104 encoded tag list into a HyperBEAM-compatible map.
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

%% @doc Check whether a list of key-value pairs contains only normalized keys.
normal_tags(Tags) ->
    lists:all(
        fun({Key, _}) ->
            hb_ao:normalize_key(Key) =:= Key
        end,
        Tags
    ).

%%% ------------------------------------------------------------------------------------------
%%% to/3 helpers
%%% ------------------------------------------------------------------------------------------

%% @doc Normalize the data field of a message to its appropriate value in a TABM.
normalize_data(Msg) ->
    case maps:is_key(<<"ao-data-key">>, Msg) of
        true -> Msg;
        false ->
            case inline_key(Msg) of
                <<"data">> -> Msg;
                InlineKey ->
                    (maps:without([InlineKey], Msg))#{
                        <<"ao-data-key">> => InlineKey,
                        <<"data">> => maps:get(InlineKey, Msg)
                    }
            end
    end.

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

maybe_bundle(TABM, Req, Opts) ->
    case hb_util:atom(hb_ao:get(<<"bundle">>, Req, false, Opts)) of
        false -> TABM;
        true ->
            % Convert back to the fully loaded structured@1.0 message, then
            % convert to TABM with bundling enabled.
            Structured = hb_message:convert(TABM, <<"structured@1.0">>, Opts),
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
            LoadedComms = maps:get(<<"commitments">>, TABM, #{}),
            LoadedTABM#{ <<"commitments">> => LoadedComms }
    end.

%% @doc Update a #tx record with the values of any TABM keys matching a #tx record field.
%% We convert the TABM to a structured message first so that any aotype'd fields are coerced
%% to their native type. This allows for easier value comparison with exiting #tx record
%% values.
%% 
%% Returns the updated #tx record, any remaining unapplied TABM keys, and the original tags
%% extracted from the TABM commitments.
apply_tabm_to_tx(TX, TABM, Req,  Opts) ->
    % There are 3 classes of values that can be applied directly to the #tx record:
    % 1. Simple fields (e.g. data_root, quantity, etc...)
    % 2. Commitments
    % 3. The data field
    
    % 1. Convert simple TABM fields to their native type, and apply to the #tx record.
    %    Simple fields are: the default #tx fields *excluding* data.
    DefaultTXTABM= hb_maps:with(hb_message:default_tx_keys(), TABM),
    SimpleTABM = hb_maps:without([<<"data">>], DefaultTXTABM),
    Structured = hb_message:convert(
        SimpleTABM,
        <<"structured@1.0">>,
        Opts
    ),
    {AppliedSimpleFields, TX1} = apply_to_tx(TX, Structured),
    
    % 2. Flatten the commitments into the 'signature' and 'owner' keys, and then apply those.
    {SignatureMap, OriginalTags} = flatten_commitments(TABM, Opts),
    {_, TX2} = apply_to_tx(TX1, SignatureMap),

    % 3. If the data field is a map, we recursively turn it into messages.
    %    Notably, we do not simply call message_to_tx/1 on the inner map
    %    because that would lead to adding an extra layer of nesting to the
    %    data. Apply the result.
    RawData = hb_maps:get(<<"data">>, TABM, ?DEFAULT_DATA, Opts),
    Data = case RawData of
        _ when is_map(RawData) -> hb_util:ok(to(RawData, Req, Opts));
        _ -> RawData
    end,
    {AppliedDataField, TX3} = apply_to_tx(TX2, #{ <<"data">> => Data }),

    % Return any remaining TABM keys that were not applied. These will be processed later.
    UnappliedTABM = hb_maps:without(
        AppliedSimpleFields ++ AppliedDataField ++ [<<"commitments">>],
        TABM
    ),
    {TX3, UnappliedTABM, OriginalTags}.

apply_to_tx(TX, Structured) ->
    % Process each field in the default TX list
    {AppliedFields, UpdatedTX} = lists:foldl(
        fun ({Field, DefaultValue}, {AccAppliedFields, AccTX}) ->
            % Get the normalized key for the field
            NormKey = hb_ao:normalize_key(Field),
            
            % Try to get the value from Structured
            case hb_maps:find(NormKey, Structured) of
                error ->
                    {AccAppliedFields, AccTX};                    
                {ok, Value} ->
                    % Try to coerce the value to match the default value's type
                    case coerce_value(Field, Value, DefaultValue) of
                        {ok, DefaultValue} ->
                            % Values are the same, keep in Structured
                            {AccAppliedFields, AccTX};
                        {ok, CoercedValue} ->
                            % Values are different, move to TX
                            {
                                [ Field | AccAppliedFields ],
                                set_tx_value_or_throw(Field, AccTX, DefaultValue, CoercedValue)
                            };
                        error ->
                            ?event(warning, {coercion_failed,
                                {field, Field}, {value, Value}, {default_value, DefaultValue}}),
                            % Coercion failed, keep in Structured
                            {AccAppliedFields, AccTX}
                    end
            end
        end,
        {[], TX},
        hb_message:default_tx_list()
    ),
    {AppliedFields, UpdatedTX}.

set_tx_data_or_throw(TX, DataItems, Req, Opts) ->
    case {TX#tx.data, hb_maps:size(DataItems, Opts)} of
        {Binary, 0} when is_binary(Binary) ->
            TX;
        {?DEFAULT_DATA, _} ->
            TX#tx{ data = DataItems };
        {Data, _} when is_map(Data) ->
            TX#tx{ data = hb_maps:merge(Data, DataItems, Opts) };
        {Data, _} when is_record(Data, tx) ->
            TX#tx{ data = DataItems#{ <<"data">> => Data } };
        {Data, _} when is_binary(Data) ->
            DataItem = hb_util:ok(to(Data, Req, Opts)),
            TX#tx{
                data = set_map_value_or_throw(<<"data">>, DataItem, DataItems)
            }
    end.

% Helper function to set a field value in a TX record, throwing if the field
% already has a non-default value
set_tx_value_or_throw(Field, TX, DefaultValue, NewValue) ->
    FieldIndex = field_index(hb_util:atom(Field)),
    case element(FieldIndex, TX) of 
        DefaultValue ->
            setelement(FieldIndex, TX, NewValue);
        ExistingValue ->
            throw({invalid_field_value, Field, ExistingValue, NewValue})
    end.

flatten_commitments(TABM, Opts) ->
    Commitments = hb_maps:get(<<"commitments">>, TABM, #{}, Opts),
    case hb_maps:keys(Commitments, Opts) of
        [] ->
            {#{}, []};
        [ID] ->
            Commitment = hb_maps:get(ID, Commitments),
            % Flatten the commitment into the 'signature' and 'owner' keys.
            Signature =
                #{
                    <<"signature">> =>
                        hb_util:decode(
                            maps:get(<<"signature">>, Commitment,
                                hb_util:encode(?DEFAULT_SIG)
                            )
                        ),
                    <<"owner">> =>
                        hb_util:decode(
                            maps:get(<<"keyid">>, Commitment,
                                hb_util:encode(?DEFAULT_OWNER)
                            )
                        )
                },
            % Flatten the original tags into the 'original-tags' key.
            OriginalTags = hb_maps:get(<<"original-tags">>, Commitment, #{}),
            {Signature, tag_map_to_encoded_tags(OriginalTags)};
        _ -> throw({multisignatures_not_supported_by_ans104, TABM})
    end.

%% @doc Convert a HyperBEAM-compatible map into an ANS-104 encoded tag list,
%% recreating the original order of the tags.
tag_map_to_encoded_tags(TagMap) ->
    OrderedList = hb_util:message_to_ordered_list(hb_private:reset(TagMap)),
    ?event({ordered_tagmap, {explicit, OrderedList}, {input, {explicit, TagMap}}}),
    lists:map(
        fun(#{ <<"name">> := Key, <<"value">> := Value }) ->
            {Key, Value}
        end,
        OrderedList
    ).

to_data_items(Structured, Req, Opts) ->
    % Process each key-value pair in Structured
    {Tags, DataItems} = maps:fold(
        fun(Key, Value, {AccTags, AccDataItems}) ->
            case Value of
                % Leave small binaries as tags
                Value when is_binary(Value), byte_size(Value) < ?MAX_TAG_VAL ->
                    {AccTags, AccDataItems};
                % All other values are converted to data items
                _ ->
                    {
                        maps:remove(Key, AccTags),
                        [
                            {hb_ao:normalize_key(Key), hb_util:ok(to(Value, Req, Opts))}
                            | AccDataItems
                        ]
                    }
            end
        end,
        {Structured, []},
        Structured
    ),
    {Tags, hb_maps:from_list(DataItems)}.

set_tags(TX, Tags, OriginalTags, Opts) ->
    % Check that the remaining keys are as we expect them to be, given the 
    % original tags. We do this by re-calculating the expected tags from the
    % original tags and comparing the result to the remaining keys.
    if length(OriginalTags) > 0 ->
        ExpectedTagsFromOriginal = deduplicating_from_list(OriginalTags, Opts),
        case Tags == ExpectedTagsFromOriginal of
            true -> ok;
            false ->
                ?event(warning,
                    {invalid_original_tags,
                        {expected, ExpectedTagsFromOriginal},
                        {given, Tags}
                    }
                ),
                throw({invalid_original_tags, OriginalTags, Tags})
        end;
    true -> ok
    end,
    % Restore the original tags, or the remaining keys if there are no original
    % tags.
    TX#tx{
        tags =
            case OriginalTags of
                [] -> hb_maps:to_list(Tags);
                _ -> OriginalTags
            end
    }.

%%% ------------------------------------------------------------------------------------------
%%% Generic helpers
%%% ------------------------------------------------------------------------------------------

%% @doc Deduplicate a list of key-value pairs by key, generating a list of
%% values for each normalized key if there are duplicates.
deduplicating_from_list(Tags, Opts) ->
    % Aggregate any duplicated tags into an ordered list of values.
    Aggregated =
        lists:foldl(
            fun({Key, Value}, Acc) ->
                NormKey = hb_ao:normalize_key(Key),
                case hb_maps:get(NormKey, Acc, undefined, Opts) of
                    undefined -> hb_maps:put(NormKey, Value, Acc, Opts);
                    Existing when is_list(Existing) ->
                        hb_maps:put(NormKey, Existing ++ [Value], Acc, Opts);
                    ExistingSingle ->
                        hb_maps:put(NormKey, [ExistingSingle, Value], Acc, Opts)
                end
            end,
            #{},
            Tags
        ),
    % Convert aggregated values into a structured-field list.
    Res =
        hb_maps:map(
            fun(_Key, Values) when is_list(Values) ->
                % Convert Erlang lists of binaries into a structured-field list.
                iolist_to_binary(
                    hb_structured_fields:list(
                        [
                            {item, {string, Value}, []}
                        ||
                            Value <- Values
                        ]
                    )
                );
            (_Key, Value) ->
                Value
            end,
            Aggregated,
            Opts
        ),
    Res.

% Helper function to coerce a value to match the type of the default value
coerce_value(<<"data">>, Value, _DefaultValue) ->
    {ok, Value};
coerce_value(<<"format">>, Value, _DefaultValue) ->
    case Value of
        <<"ans104">> -> {ok, ans104};
        <<"1">> -> {ok, 1};
        <<"2">> -> {ok, 2};
        _ -> {ok, Value}
    end;
coerce_value(_Field, Value, DefaultValue) ->
    case {Value, DefaultValue} of
        {V, D} when is_binary(V), ?IS_ID(V), is_binary(D) ->
            try {ok, hb_util:native_id(V)} catch _:_ -> error end;
        {V, D} when is_binary(V), is_binary(D) -> {ok, V};
        {V, D} when is_atom(V), is_atom(D) -> {ok, V};
        {V, D} when is_integer(V), is_integer(D) -> {ok, V};
        {V, D} when is_float(V), is_float(D) -> {ok, V};
        {V, D} when is_boolean(V), is_boolean(D) -> {ok, V};
        {V, D} when is_list(V), is_list(D) -> {ok, V};
        {V, D} when is_map(V), is_map(D) -> {ok, V};
        {V, D} when is_tuple(V), is_tuple(D) -> {ok, V};
        {V, D} when is_binary(V), is_atom(D) ->
            try {ok, hb_util:atom(V)} catch _:_ -> error end;
        {V, D} when is_binary(V), is_integer(D) ->
            try {ok, hb_util:int(V)} catch _:_ -> error end;
        {V, D} when is_binary(V), is_float(D) ->
            try {ok, hb_util:float(V)} catch _:_ -> error end;
        _ -> error
    end.

% Helper function to get the field index in the tx record
field_index(Field) ->
    Fields = record_info(fields, tx),
    IndexedFields = lists:zip(Fields, lists:seq(1, length(Fields))),
    case lists:keyfind(Field, 1, IndexedFields) of
        {Field, Index} -> Index + 1;
        false -> throw({invalid_field, Field})
    end.

%%% ------------------------------------------------------------------------------------------
%%% ANS-104-specific testing cases.
%%% ------------------------------------------------------------------------------------------

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
    ?event({restored_msg, {explicit, ReadMsg}}),
    {ok, ReadTX} = to(ReadMsg, #{}, Opts),
    ?event({restored_tx, {explicit, ReadTX}}),
    ?assert(hb_message:match(ReadMsg, SignedMsg)),
    ?assert(ar_bundles:verify_item(ReadTX)).

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

only_committed_maintains_target_test() ->
    TX = ar_bundles:sign_item(#tx {
        target = crypto:strong_rand_bytes(32),
        tags = [
            {<<"test-tag">>, <<"test-value">>},
            {<<"test-tag-2">>, <<"test-value-2">>}
        ],
        data = <<"test-data">>
    }, ar_wallet:new()),
    ?event({tx, {explicit, TX}}),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, {explicit, Decoded}}),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, {explicit, OnlyCommitted}}),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({encoded, {explicit, Encoded}}),
    ?event({tx, {explicit, TX}}),
    ?assertEqual(TX, Encoded).

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
        
simple_signed_to_httpsig_test_disabled() ->
    TX =
        ar_bundles:sign_item(
            #tx {
                tags = [
                    {<<"test-tag">>, <<"test-value">>},
                    {<<"test-tag-2">>, <<"test-value-2">>},
                    {<<"Capitalized-Tag">>, <<"test-value-3">>}
                ]
            },
            ar_wallet:new()
        ),
    Structured1 = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event(debug, {tx, TX}),
    TABM = hb_message:convert(TX, tabm, <<"ans104@1.0">>, #{}),
    ?event(debug, {tabm, TABM}),
    HTTPSig = hb_message:convert(TABM, <<"httpsig@1.0">>, tabm, #{}),
    ?event(debug, {httpsig, HTTPSig}),
    Structured2 = hb_message:convert(HTTPSig, <<"structured@1.0">>, <<"httpsig@1.0">>, #{}),
	Match = hb_message:match(Structured1, Structured2, #{}),
    ?event(debug, {match, Match}),
    ?assert(Match),
    ?assert(hb_message:verify(Structured2, all, #{})),
    HTTPSig2 = hb_message:convert(Structured2, <<"httpsig@1.0">>, <<"structured@1.0">>, #{}),
    ?event(debug, {httpsig2, HTTPSig2}),
    ?assert(hb_message:verify(HTTPSig2, all, #{})),
    ?assert(hb_message:match(HTTPSig, HTTPSig2)).


% aotypes_test() ->
%     Msg = #{
%         <<"binary-tag">> => <<"binary-value">>,
%         <<"atom-tag">> => atom_value,
%         <<"integer-tag">> => 123,
%         <<"float-tag">> => 123.456,
%         <<"boolean-tag">> => true,
%         <<"list-tag">> => [1, 2, 3],
%         <<"map-tag">> => #{<<"key">> => <<"value">>}
%     },
%     TABM0 = hb_message:convert(Msg, tabm, <<"structured@1.0">>, #{}),
%     Dataitem = hb_message:convert(TABM0, <<"ans104@1.0">>, tabm, #{}),
%     TABM1 = hb_message:convert(Dataitem, tabm, <<"ans104@1.0">>, #{}),
%     Structured = hb_message:convert(TABM1, <<"structured@1.0">>, tabm, #{}),
%     ?event({tabm, {explicit, TABM0}}),
%     ?event({dataitem, {explicit, Dataitem}}),
%     ?event({tabm, {explicit, TABM1}}),
%     ?event({structured, {explicit, Structured}}),
%     ?event({id, {explicit, Dataitem#tx.unsigned_id}}),
%     ExpectedTX = #tx{
%         unsigned_id = hb_util:decode(<<"MSWJEQCbH_mCmyEuPT45liJ4JSXAXAltYj7ZFGtypPY">>),
%         tags = [
%             {<<"ao-types">>, <<"atom-tag=\"atom\", boolean-tag=\"atom\", float-tag=\"float\", integer-tag=\"integer\"">>},
%             {<<"atom-tag">>, <<"atom_value">>},
%             {<<"binary-tag">>, <<"binary-value">>},
%             {<<"boolean-tag">>, <<"true">>},
%             {<<"float-tag">>, <<"1.23456000000000003070e+02">>},
%             {<<"integer-tag">>, <<"123">>},
%             {<<"list-tag+link">>, <<"LJNSyAg3udG_pxDcNGB0fdNZJ1GT49t7cydlGTRmZLc">>},
%             {<<"map-tag+link">>, <<"C2QtFNMLl1EqNMzRuenooVz-vpXuVDDOdCkiVjIiwSE">>}
%         ]
%     },
%     ?assertEqual(ExpectedTX, Dataitem),
%     ?assert(hb_message:match(Msg, Structured)),
%     ?assert(hb_message:match(TABM0, TABM1)),
%     ok.


set_defaults_test() ->
    UnsignedStructured = #{
        <<"format">> => ans104,
        <<"last_tx">> => <<>>,
        <<"target">> => <<>>,
        <<"quantity">> => 0,
        <<"data">> => ?DEFAULT_DATA,
        <<"manifest">> => undefined,
        <<"data_root">> => <<>>,
        <<"reward">> => 0,
        <<"denomination">> => 0
    },
    UnsignedTX = #tx{
        unsigned_id = hb_util:decode(<<"3eMto8z7IlnQgKPrHjmkrI2ohnrJhnCsss6wc4L86QQ">>),
        tags = [
            {<<"ao-types">>,
                <<
                    "denomination=\"integer\", ",
                    "format=\"atom\", ",
                    "manifest=\"atom\", ",
                    "quantity=\"integer\", ",
                    "reward=\"integer\""
                >>},
            {<<"data">>,?DEFAULT_DATA},
            {<<"data_root">>, <<>>},
            {<<"denomination">>,<<"0">>},
            {<<"format">>,<<"ans104">>},
            {<<"last_tx">>,<<>>},
            {<<"manifest">>,<<"undefined">>},
            {<<"quantity">>,<<"0">>},
            {<<"reward">>,<<"0">>},
            {<<"target">>,<<>>}
        ]
    },
    do_unsigned_roundtrip(UnsignedStructured, UnsignedTX),
    do_signed_roundtrip(UnsignedStructured, UnsignedTX).

invalid_fields_test() ->
    TestCases = [
        { <<"id">>, #{ <<"id">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"unsigned_id">>, #{ <<"unsigned_id">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"owner">>, #{ <<"owner">> => hb_util:encode(crypto:strong_rand_bytes(512)) } },
        { <<"owner_address">>, #{ <<"owner_address">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"tags">>, #{ <<"tags">> => <<"tags">> } },
        { <<"data_size">>, #{ <<"data_size">> => <<"100">> } },
        { <<"data_tree">>, #{ <<"data_tree">> => hb_util:encode(crypto:strong_rand_bytes(32)) } },
        { <<"signature">>, #{ <<"signature">> => hb_util:encode(crypto:strong_rand_bytes(512)) } }
    ],

    lists:foreach(
        fun({InvalidField, TestCase}) ->
            hb_test_utils:assert_throws(
                fun dev_codec_ans104:to/3,
                [TestCase, #{}, #{}],
                {invalid_fields, [InvalidField]},
                InvalidField
            )
        end,
        TestCases
    ).

do_unsigned_roundtrip(UnsignedStructured, UnsignedTX) ->
    StructuredCodec = #{<<"device">> => <<"structured@1.0">>, <<"bundle">> => true},
    TABM0 = hb_message:convert(UnsignedStructured, tabm, StructuredCodec, #{}),
    {ok, CommittedTABM0} =
        dev_codec_ans104:commit(TABM0, #{ <<"type">> => <<"unsigned">> }, #{}),
    {ok, DataItem} = dev_codec_ans104:to(TABM0, #{}, #{}),
    {ok, TABM1} = dev_codec_ans104:from(DataItem, #{}, #{}),
    Structured = hb_message:convert(TABM1, StructuredCodec, tabm, #{}),
    ?assertEqual(UnsignedTX, DataItem),
    ?assert(hb_message:match(UnsignedStructured, Structured)),
    ?assert(hb_message:match(TABM0, TABM1)),
    ?assert(hb_message:match(TABM0, CommittedTABM0)),
    ok.

do_signed_roundtrip(UnsignedStructured, UnsignedTX) ->
    {_, {_, Owner}} = Wallet = ar_wallet:new(),
    Opts = #{ priv_wallet => Wallet },
    StructuredCodec = #{<<"device">> => <<"structured@1.0">>, <<"bundle">> => true},

    TABM0 = hb_message:convert(UnsignedStructured, tabm, StructuredCodec, Opts),
    {ok, SignedTABM0} = 
        dev_codec_ans104:commit(TABM0, #{ <<"type">> => <<"signed">> }, Opts),
    ?assert(hb_util:ok(dev_codec_ans104:verify(SignedTABM0, #{}, Opts))),
    {ok, ID, Commitment} = hb_message:commitment(
        #{ <<"commitment-device">> => <<"ans104@1.0">> }, SignedTABM0, Opts),
    Signature = hb_util:decode(hb_ao:get(<<"signature">>, Commitment, <<>>, Opts)),
    SignedTX = UnsignedTX#tx{ id = hb_util:decode(ID), owner = Owner, signature = Signature },
    ?event({signed_id, {explicit, ID}}),
    {ok, DataItem} = dev_codec_ans104:to(SignedTABM0, #{}, Opts),
    {ok, SignedTABM1} = dev_codec_ans104:from(DataItem, #{}, Opts),

    {ok, UnsignedTABM0} =
        dev_codec_ans104:commit(SignedTABM0, #{ <<"type">> => <<"unsigned">> }, #{}),

    ?assert(hb_message:match(SignedTABM0, SignedTABM1)),
    ?assert(hb_message:match(TABM0, UnsignedTABM0)),
    ?assertEqual(SignedTX, DataItem),
    ok.