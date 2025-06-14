%%% @doc Utilities for HyperBEAM-specific handling of transactions. Handles both ANS104
%%% and Arewave L1 transactions.
-module(hb_tx).
-export([signer/1, is_signed/1]).
-export([tx_to_tabm/4, tabm_to_tx/3, binary_to_tx/1, encoded_tags_to_map/1]).
-export([id/1, id/2, reset_ids/1, update_ids/1]).
-export([normalize/1, normalize_data/1]).
-export([commit_message/4, verify_message/4, verify/1]).
-export([print/1, format/1, format/2]).

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

% How many bytes of a binary to print with `print/1'.
-define(BIN_PRINT, 20).
-define(INDENT_SPACES, 2).

%% List of tags that should be removed during `to'. These relate to the nested
%% ar_bundles format that is used by the `ans104@1.0' codec.
-define(FILTERED_TAGS,
    [
        <<"bundle-format">>,
        <<"bundle-map">>,
        <<"bundle-version">>
    ]
).

%% @doc Return the address of the signer of an item, if it is signed.
signer(#tx { owner = ?DEFAULT_OWNER }) -> undefined;
signer(Item) -> crypto:hash(sha256, Item#tx.owner).

%% @doc Check if an item is signed.
is_signed(Item) ->
    Item#tx.signature =/= ?DEFAULT_SIG.

%% @doc Return the ID of an item -- either signed or unsigned as specified.
%% If the item is unsigned and the user requests the signed ID, we return
%% the atom `not_signed'. In all other cases, we return the ID of the item.
id(Item) -> id(Item, unsigned).
id(Item, Type) when not is_record(Item, tx) ->
    id(normalize(Item), Type);
id(Item = #tx { unsigned_id = ?DEFAULT_ID }, unsigned) ->
    CorrectedItem = reset_ids(Item),
    CorrectedItem#tx.unsigned_id;
id(#tx { unsigned_id = UnsignedID }, unsigned) ->
    UnsignedID;
id(#tx { id = ?DEFAULT_ID }, signed) ->
    not_signed;
id(#tx { id = ID }, signed) ->
    ID.

%% @doc Re-calculate both of the IDs for an item. This is a wrapper
%% function around `update_id/1' that ensures both IDs are set from
%% scratch.
reset_ids(Item) ->
    update_ids(Item#tx { unsigned_id = ?DEFAULT_ID, id = ?DEFAULT_ID }).

%% @doc Take an item and ensure that both the unsigned and signed IDs are
%% appropriately set. This function is structured to fall through all cases
%% of poorly formed items, recursively ensuring its correctness for each case
%% until the item has a coherent set of IDs.
%% The cases in turn are:
%% - The item has no unsigned_id. This is never valid.
%% - The item has the default signature and ID. This is valid.
%% - The item has the default signature but a non-default ID. Reset the ID.
%% - The item has a signature. We calculate the ID from the signature.
%% - Valid: The item is fully formed and has both an unsigned and signed ID.
update_ids(Item = #tx { unsigned_id = ?DEFAULT_ID }) ->
    update_ids(Item#tx { unsigned_id = generate_id(Item, unsigned) });
update_ids(Item = #tx { id = ?DEFAULT_ID, signature = ?DEFAULT_SIG }) ->
    Item;
update_ids(Item = #tx { signature = ?DEFAULT_SIG }) ->
    Item#tx { id = ?DEFAULT_ID };
update_ids(Item = #tx { signature = Sig }) when Sig =/= ?DEFAULT_SIG ->
    Item#tx { id = generate_id(Item, signed) };
update_ids(TX) -> TX.

normalize(Item) -> reset_ids(normalize_data(Item)).

%% @doc Ensure that a data item (potentially containing a map or list) has a standard, serialized form.
normalize_data(not_found) -> throw(not_found);
normalize_data(Bundle) when is_list(Bundle); is_map(Bundle) ->
    normalize_data(#tx{ data = Bundle });
normalize_data(Item = #tx{data = Bin}) when is_binary(Bin) ->
    normalize_data_size(Item);
normalize_data(Item = #tx{data = Data}) ->
    normalize_data_size(ar_bundles:serialize_bundle_data(Data, Item)).

%% @doc Reset the data size of a data item. Assumes that the data is already normalized.
normalize_data_size(Item = #tx{data = Bin, format = 2}) when is_binary(Bin) ->
    normalize_data_root(Item);
normalize_data_size(Item = #tx{data = Bin}) when is_binary(Bin) ->
    Item#tx{data_size = byte_size(Bin)};
normalize_data_size(Item) -> Item.

%% @doc Calculate the data root of a data item. Assumes that the data is already normalized.
normalize_data_root(Item = #tx{data = Bin, format = 2})
        when is_binary(Bin) andalso Bin =/= ?DEFAULT_DATA ->
    Item#tx{data_root = ar_tx:data_root(Bin), data_size = byte_size(Bin)};
normalize_data_root(Item) -> Item.

%% @doc Sign a message using the `priv_wallet' key in the options. Supports both
%% the `hmac-sha256' and `rsa-pss-sha256' algorithms, offering unsigned and
%% signed commitments.
commit_message(Codec, Msg, Req = #{ <<"type">> := <<"unsigned">> }, Opts) ->
    commit_message(Codec, Msg, Req#{ <<"type">> => <<"unsigned-sha256">> }, Opts);
commit_message(Codec, Msg, Req = #{ <<"type">> := <<"signed">> }, Opts) ->
    commit_message(Codec, Msg, Req#{ <<"type">> => <<"rsa-pss-sha256">> }, Opts);
commit_message(Codec, Msg, Req = #{ <<"type">> := <<"rsa-pss-sha256">> }, Opts) ->
    % Convert the given message to an ANS-104 or Arweave TX record, sign it, and convert
    % it back to a structured message.
    ?event({committing, {input, Msg}}),
    TX = hb_util:ok(codec_to_tx(Codec, hb_private:reset(Msg), Req, Opts)),
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    Signed = sign(TX, Wallet),
    SignedStructured =
        hb_message:convert(
            Signed,
            <<"structured@1.0">>,
            Codec,
            Opts
        ),
    {ok, SignedStructured};
commit_message(Codec, Msg, #{ <<"type">> := <<"unsigned-sha256">> }, Opts) ->
    % Remove the commitments from the message, convert it to ANS-104 or Arweave, then back.
    % This forces the message to be normalized and the unsigned ID to be
    % recalculated.
    {
        ok,
        hb_message:convert(
            hb_maps:without([<<"commitments">>], Msg, Opts),
            Codec,
            <<"structured@1.0">>,
            Opts
        )
    }.

verify_message(Codec, Msg, Req, Opts) ->
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
    {ok, TX} = codec_to_tx(Codec, OnlyWithCommitment, Req, Opts),
    ?event({verify, {encoded, TX}}),
    Res = verify(TX),
    {ok, Res}.

sign(#tx{ format = ans104 } = TX, Wallet) ->
    ar_bundles:sign_item(TX, Wallet);
sign(TX, Wallet) ->
    ar_tx:sign(TX, Wallet).

verify(#tx{ format = ans104 } = TX) ->
    ar_bundles:verify_item(TX);
verify(TX) ->
    ar_tx:verify(TX). 

tx_to_tabm(RawTX, CommittedTags, Req, Opts) ->
    case lists:keyfind(<<"ao-type">>, 1, RawTX#tx.tags) of
        false ->
            tx_to_tabm2(RawTX, CommittedTags, Req, Opts);
        {<<"ao-type">>, <<"binary">>} ->
            RawTX#tx.data
    end.

tx_to_tabm2(RawTX, CommittedTags, Req, Opts) ->
    Device = case RawTX#tx.format of
        ans104 -> <<"ans104@1.0">>;
        _ -> <<"tx@1.0">>
    end,
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(normalize(RawTX)),
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
    TABM3 = add_commitments(TABM2, TX, Device, CommittedTags, Opts),

    hb_maps:without(?FILTERED_TAGS, TABM3, Opts).

tabm_to_tx(InputTABM, Req, Opts) ->
    NormalizedTABM = hb_ao:normalize_keys(normalize_data_field(InputTABM), Opts),
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

    hb_tx:reset_ids(hb_tx:normalize(TXWithData)).

binary_to_tx(Binary) ->
    % ans104 and Arweave cannot serialize just a simple binary or get an ID for it, so
    % we turn it into a TX record with a special tag, tx_to_message will
    % identify this tag and extract just the binary.
    #tx{
        tags = [{<<"ao-type">>, <<"binary">>}],
        data = Binary
    }.

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
normalize_data_field(Msg) ->
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

%% @doc Generate the ID for a given transaction.
generate_id(#tx{ format = ans104 } = TX, signed) ->
    ar_bundles:generate_id(TX, signed);
generate_id(TX, signed) ->
    ar_tx:generate_id(TX, signed);
generate_id(#tx{ format = ans104 } = TX, unsigned) ->
    ar_bundles:generate_id(TX, unsigned);
generate_id(TX, unsigned) ->
    ar_tx:generate_id(TX, unsigned).

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

print(Item) ->
    io:format(standard_error, "~s", [lists:flatten(format(Item))]).

format(Item) -> format(Item, 0).
format(Item, Indent) when is_list(Item); is_map(Item) ->
    format(hb_tx:normalize(Item), Indent);
format(Item, Indent) when is_record(Item, tx) ->
    Valid = verify(Item),
    format_line(
        "TX ( ~s: ~s ) {",
        [
            if
                Item#tx.signature =/= ?DEFAULT_SIG ->
                    lists:flatten(
                        io_lib:format(
                            "~s (signed) ~s (unsigned)",
                            [
                                hb_util:safe_encode(id(Item, signed)),
                                hb_util:safe_encode(id(Item, unsigned))
                            ]
                        )
                    );
                true -> hb_util:safe_encode(id(Item, unsigned))
            end,
            if
                Valid == true -> "[SIGNED+VALID]";
                true -> "[UNSIGNED/INVALID]"
            end
        ],
        Indent
    ) ++
    case (not Valid) andalso Item#tx.signature =/= ?DEFAULT_SIG of
        true ->
            format_line("!!! CAUTION: ITEM IS SIGNED BUT INVALID !!!", Indent + 1);
        false -> []
    end ++
    case is_signed(Item) of
        true ->
            format_line("Signer: ~s", [hb_util:encode(signer(Item))], Indent + 1);
        false -> []
    end ++
    format_line("Target: ~s", [
            case Item#tx.target of
                <<>> -> "[NONE]";
                Target -> hb_util:id(Target)
            end
        ], Indent + 1) ++
    format_line("Tags:", Indent + 1) ++
    lists:map(
        fun({Key, Val}) -> format_line("~s -> ~s", [Key, Val], Indent + 2) end,
        Item#tx.tags
    ) ++
    format_line("Data:", Indent + 1) ++ format_data(Item, Indent + 2) ++
    format_line("}", Indent);
format(Item, Indent) ->
    % Whatever we have, its not a tx...
    format_line("INCORRECT ITEM: ~p", [Item], Indent).

format_data(Item, Indent) when is_binary(Item#tx.data) ->
    case lists:keyfind(<<"bundle-format">>, 1, Item#tx.tags) of
        {_, _} ->
            format_data(ar_bundles:deserialize(ar_bundles:serialize(Item)), Indent);
        false ->
            format_line(
                "Binary: ~p... <~p bytes>",
                [format_binary(Item#tx.data), byte_size(Item#tx.data)],
                Indent
            )
    end;
format_data(Item, Indent) when is_map(Item#tx.data) ->
    format_line("Map:", Indent) ++
    lists:map(
        fun({Name, MapItem}) ->
            format_line("~s ->", [Name], Indent + 1) ++
            format(MapItem, Indent + 2)
        end,
        maps:to_list(Item#tx.data)
    );
format_data(Item, Indent) when is_list(Item#tx.data) ->
    format_line("List:", Indent) ++
    lists:map(
        fun(ListItem) ->
            format(ListItem, Indent + 1)
        end,
        Item#tx.data
    ).

format_binary(Bin) ->
    lists:flatten(
        io_lib:format(
            "~p",
            [
                binary:part(
                    Bin,
                    0,
                    case byte_size(Bin) of
                        X when X < ?BIN_PRINT -> X;
                        _ -> ?BIN_PRINT
                    end
                )
            ]
        )
    ).

format_line(Str, Indent) -> format_line(Str, "", Indent).
format_line(RawStr, Fmt, Ind) ->
    io_lib:format(
        [$\s || _ <- lists:seq(1, Ind * ?INDENT_SPACES)] ++
            lists:flatten(RawStr) ++ "\n",
        Fmt
    ).

codec_to_tx(<<"ans104@1.0">>, Msg, Req, Opts) ->
    dev_codec_ans104:to(Msg, Req, Opts);
codec_to_tx(<<"tx@1.0">>, Msg, Req, Opts) ->
    dev_codec_tx:to(Msg, Req, Opts);
codec_to_tx(Codec, Msg, Req, Opts) ->
    ?event({codec_to_tx, {codec, Codec}, {msg, Msg}, {req, Req}}),
    throw({invalid_codec, Codec}).
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