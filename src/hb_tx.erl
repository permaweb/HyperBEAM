-module(hb_tx).
-export([tx_to_tabm/5, tabm_to_tx/3, encoded_tags_to_map/1]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).

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

tx_to_tabm(TX, Device, CommittedTags, Req, Opts) ->
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
    add_commitments(TABM2, TX, Device, CommittedTags, Opts).

tabm_to_tx(InputTABM, Req, Opts) ->
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

   ar_bundles:reset_ids(ar_bundles:normalize(TXWithData)).

%%% ------------------------------------------------------------------------------------------
%%% tx_to_tabm/5 helpers
%%% ------------------------------------------------------------------------------------------


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
    
add_commitments(Structured, TX, Device, CommmittedTags, Opts) ->
    OriginalTagMap = encoded_tags_to_map(TX#tx.tags),
    CommittedKeys =
        hb_ao:normalize_keys(
            hb_util:unique(
                CommmittedTags ++
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
                                <<"commitment-device">> => Device,
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
                    <<"commitment-device">> => Device,
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
                    case dev_codec_ans104:from(InnerValue, Req, Opts) of
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
%%% tabm_to_tx/3 helpers
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
        _ when is_map(RawData) -> hb_util:ok(dev_codec_ans104:to(RawData, Req, Opts));
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
            DataItem = hb_util:ok(dev_codec_ans104:to(Data, Req, Opts)),
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
                            {hb_ao:normalize_key(Key), hb_util:ok(dev_codec_ans104:to(Value, Req, Opts))}
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
%%% Tests.
%%% ------------------------------------------------------------------------------------------

encoded_tags_to_map_test_() ->
    [
        fun test_encoded_tags_to_map_happy/0
    ].

%% @doc Test encoded_tags_to_map/1 with multiple test cases in a list.
test_encoded_tags_to_map_happy() ->
    TestCases = [
        {empty, [], #{}},
        {single, [
            {<<"test-key">>, <<"test-val">>}
        ], #{
            <<"1">> => #{<<"name">> => <<"test-key">>, <<"value">> => <<"test-val">>}
        }},
        {multiple, [
            {<<"alpha">>, <<"1">>},
            {<<"beta">>, <<"2">>},
            {<<"gamma">>, <<"3">>}
        ], #{
            <<"1">> => #{<<"name">> => <<"alpha">>, <<"value">> => <<"1">>},
            <<"2">> => #{<<"name">> => <<"beta">>, <<"value">> => <<"2">>},
            <<"3">> => #{<<"name">> => <<"gamma">>, <<"value">> => <<"3">>}
        }}
    ],
    lists:foreach(
        fun({Label, Input, Expected}) ->
            ?assertEqual(Expected, encoded_tags_to_map(Input), Label)
        end,
        TestCases
    ).