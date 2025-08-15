%%% @doc Utilities for HyperBEAM-specific handling of transactions. Handles both ANS104
%%% and Arewave L1 transactions.
-module(hb_tx).
-export([signer/1, is_signed/1]).
-export([tx_to_tabm/4, tabm_to_tx/4, binary_to_tx/1, encoded_tags_to_map/1]).
-export([id/1, id/2, reset_ids/1, update_ids/1]).
-export([normalize/1, normalize_data/1, normalize_data_field/1]).
-export([commit_message/4, verify_message/4, verify/1]).
-export([print/1, format/1, format/2]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 4096).

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


%%% The list of keys that should be forced into the tag list, rather than being
%%% encoded as fields in the TX record.
-define(BASE_INVALID_FIELDS,
    [
        <<"id">>,
        <<"unsigned_id">>,
        <<"owner">>,
        <<"owner_address">>,
        <<"tags">>,
        <<"data_tree">>,
        <<"signature">>,
        <<"signature_type">>
    ]
).

-define(ORIGINAL_FIELDS,
    [
        <<"target">>,
        <<"last_tx">>
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
    ?event({normalize_data, bundle, Bundle}),
    normalize_data(#tx{ data = Bundle });
normalize_data(Item = #tx { data = Data }) when is_list(Data) ->
    ?event({normalize_data, list, Item}),
    normalize_data(
        Item#tx{
            tags = ar_bundles:add_list_tags(Item#tx.tags),
            data =
                maps:from_list(
                    lists:zipwith(
                        fun(Index, MapItem) ->
                            {
                                integer_to_binary(Index),
                                update_ids(normalize_data(MapItem))
                            }
                        end,
                        lists:seq(1, length(Data)),
                        Data
                    )
                )
        }
    );
normalize_data(Item = #tx{data = Bin}) when is_binary(Bin) ->
    ?event({normalize_data, binary, Item}),
    normalize_data_size(Item);
normalize_data(Item = #tx{data = Data}) ->
    ?event({normalize_data, map, Item}),
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
    SignedTABM =
        hb_message:convert(
            Signed,
            tabm,
            Codec,
            Opts
        ),
    {ok, SignedTABM};
commit_message(Codec, Msg, #{ <<"type">> := <<"unsigned-sha256">> }, Opts) ->
    % Remove the commitments from the message, convert it to ANS-104 or Arweave, then back.
    % This forces the message to be normalized and the unsigned ID to be
    % recalculated.
    TX = hb_message:convert(
        hb_maps:without([<<"commitments">>], Msg, Opts),
        Codec,
        tabm,
        Opts
    ),
    {
        ok,
        hb_message:convert(
            TX,
            tabm,
            Codec,
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

get_device(#tx{ format = ans104 }) -> <<"ans104@1.0">>;
get_device(_) -> <<"tx@1.0">>.

tx_to_tabm(RawTX, CommittedTags, Req, Opts) ->
    case lists:keyfind(<<"ao-type">>, 1, RawTX#tx.tags) of
        false ->
            tx_to_tabm2(RawTX, CommittedTags, Req, Opts);
        {<<"ao-type">>, <<"binary">>} ->
            RawTX#tx.data
    end.

tx_to_tabm2(RawTX, CommittedTags, Req, Opts) ->
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(normalize(RawTX)),

    TABM0 = tx_to_tabm_tags(TX#tx.tags),

    TABM1 = tx_to_tabm_fields(TABM0,TX),

    NeedOriginalTags = not normal_tags(TX#tx.tags) orelse has_key_clashes(TABM1),

    TABM2 = tx_to_tabm_collapse_clashes(TABM1),

    TABM3 = tx_to_tabm_data(TABM2, TX, Req, Opts),

    TABM4 = tx_to_tabm_aotypes(TABM3, Opts),

    TABM5 = tx_to_tabm_commitments(TABM4, TX, CommittedTags, NeedOriginalTags, Opts),

    Result = hb_maps:without(?FILTERED_TAGS, TABM5, Opts),
    Result.

%% @doc Deduplicate a list of key-value pairs by key, generating a list of
%% values for each normalized key if there are duplicates.
tx_to_tabm_tags(Tags) ->
    Grouped = lists:foldl(
        fun({Key, Value}, Acc) ->
            NormKey = hb_util:to_lower(hb_ao:normalize_key(Key)),
            case maps:find(NormKey, Acc) of
                {ok, Pairs} -> Acc#{ NormKey => [{Key, Value} | Pairs] };
                error -> Acc#{ NormKey => [{Key, Value}] }
            end
        end,
        #{},
        Tags
    ),
    % sort the values for each key
    maps:map(
        fun(_NormKey, Pairs) ->
            lists:sort(
                fun({KeyA, _}, {KeyB, _}) ->
                    tag_sort(KeyA, KeyB)
                end,
                Pairs
            )
        end,
        Grouped
    ).

%% @doc Returns true if KeyA is less than KeyB according to the following rules:
%% - Assumes KeyA and KeyB differ only in casing
%% - Sort in the following order:
%%   1. lowercase
%%   2. Capitalized
%%   3. all other casing sorted by built-in alphanumeric sort
tag_sort(KeyA, KeyB) ->
    IsLowerA = hb_util:to_lower(KeyA) == KeyA,
    IsLowerB = hb_util:to_lower(KeyB) == KeyB,
    IsFirstCapA = is_first_char_uppercase(KeyA),
    IsFirstCapB = is_first_char_uppercase(KeyB),

    case {IsLowerA, IsLowerB} of
        {true, false} -> true;
        {false, true} -> false;
        {true, true} -> true;
        {false, false} ->
            case {IsFirstCapA, IsFirstCapB} of
                {true, false} -> true;
                {false, true} -> false;
                _ -> KeyA =< KeyB
            end
    end.

%% @doc Check if the first character of a binary is uppercase (A-Z)
is_first_char_uppercase(<<>>) -> false;
is_first_char_uppercase(<<FirstChar, _/binary>>) when FirstChar >= $A, FirstChar =< $Z -> true;
is_first_char_uppercase(_) -> false.

tx_to_tabm_fields(TABM, TX) ->
    process_original_fields(TX, TABM, fun add_field_to_tabm/3).

add_original_fields(Commitment, TX) ->
    process_original_fields(TX, Commitment, fun add_field_to_commitment/3).

%% @doc Common function to process original fields with different actions
process_original_fields(TX, Map, ActionFun) ->
    FieldDefaults = hb_message:default_tx_list(),
    
    lists:foldl(
        fun(NormalizedKey, Acc) ->
            % Get the current value from the TX record
            FieldIndex = field_index(hb_util:atom(NormalizedKey)),
            CurrentValue = element(FieldIndex, TX),
            
            % Get the default value for this field from hb_message:default_tx_list
            {_, DefaultValue} = lists:keyfind(NormalizedKey, 1, FieldDefaults),
            
            % Check if value is not default
            case CurrentValue =/= DefaultValue of
                true ->
                    ActionFun(NormalizedKey, CurrentValue, Acc);
                false ->
                    % Skip default values
                    Acc
            end
        end,
        Map,
        ?ORIGINAL_FIELDS
    ).

%% @doc Action function for fields_to_tabm - adds field to TABM if key doesn't exist
add_field_to_tabm(NormalizedKey, CurrentValue, TABM) ->
    % At this point the TABM stores a sorted list of {key, value} for each key, later
    % we will collapse this to a single value.
    Value = {original_key(NormalizedKey), hb_util:encode(CurrentValue)},
    case maps:find(NormalizedKey, TABM) of
        {ok, Values} ->
            maps:put(NormalizedKey, Values ++ [Value], TABM);
        error ->
            maps:put(NormalizedKey, [Value], TABM)
    end.

%% @doc Action function for add_original_fields - adds original-fieldname to commitment
add_field_to_commitment(NormalizedKey, CurrentValue, Commitment) ->
    maps:put(
        original_key(NormalizedKey),
        hb_util:encode(CurrentValue),
        Commitment).

original_key(<<"target">>) -> <<"original-target">>;
original_key(<<"last_tx">>) -> <<"original-last_tx">>.

tx_to_tabm_collapse_clashes(TABM) ->
    maps:map(
        fun(Key, Values) when is_list(Values) ->
            {_Key, FirstValue} = hd(Values),
            FirstValue;
        (Key, Value) ->
            Value
        end,
        TABM
    ).

tx_to_tabm_aotypes(TABM, Opts) ->
    AOTypes = dev_codec_structured:decode_ao_types(TABM, Opts),
    case maps:size(AOTypes) of
        0 -> TABM;
        _ ->
            LowercaseAOTypes = maps:fold(
                fun(Key, Value, Acc) ->
                    maps:put(hb_util:to_lower(Key), Value, Acc)
                end,
                #{},
                AOTypes
            ),
            maps:put(<<"ao-types">>,
                dev_codec_structured:encode_ao_types(
                    LowercaseAOTypes,
                    Opts
                ),
                TABM
            )
    end.

tabm_to_tx(BaseTX, InputTABM, Req, Opts) ->
    % Prepare the TABM and Commitments
    NormalizedTABM = hb_ao:normalize_keys(normalize_data_field(InputTABM), Opts),
    Commitments = hb_maps:get(<<"commitments">>, NormalizedTABM, #{}, Opts),
    BaseTABM = hb_maps:without([<<"commitments">>], NormalizedTABM),
    BundledTABM = maybe_bundle(BaseTABM, Req, Opts),
    LoadedTABM = hb_cache:ensure_all_loaded(BundledTABM),

    {TX0, TABM0} = tabm_to_tx_data(BaseTX, LoadedTABM, Req, Opts),
    {TX1, _TABM1} = tabm_to_tx_commitments(TX0, TABM0, Commitments, Opts),

    FinalTX = normalize(TX1),

    % Check for invalid fields. We do this at the end once tx.format and tx.data have been
    % set.
    HasInvalidFields = [
        Field || 
        Field <- invalid_fields(BaseTX), maps:is_key(Field, NormalizedTABM)],
    case HasInvalidFields of
        [] -> ok;
        _ -> throw({invalid_fields, HasInvalidFields})
    end,

    FinalTX.

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
    
tx_to_tabm_commitments(TABM, #tx{ signature = ?DEFAULT_SIG } = TX, _, NeedOriginalTags, Opts) ->
    ?event({no_signature_detected, TABM}),
    Commitment = initialize_commitment(TX, NeedOriginalTags),
    case maps:size(Commitment) of
        0 -> TABM;
        _ ->
            ID = hb_util:human_id(TX#tx.unsigned_id),
            Commitments = #{
                ID => Commitment#{
                    <<"commitment-device">> => get_device(TX),
                    <<"type">> => <<"unsigned-sha256">>
                }
            },
            set_map_value_or_throw( <<"commitments">>, Commitments, TABM)
    end;
tx_to_tabm_commitments(TABM, TX, CommittedTags, NeedOriginalTags, Opts) ->
    Address = hb_util:human_id(ar_wallet:to_address(TX#tx.owner)),
    ?event(debug_test, {inbound_committed_tags, CommittedTags}),
    CommittedKeys =
        hb_ao:normalize_keys(
            hb_util:unique(
                CommittedTags ++
                [
                    hb_util:to_lower(hb_ao:normalize_key(Tag))
                ||
                    {Tag, _} <- TX#tx.tags
                ] ++
                hb_util:to_sorted_keys(TABM)
            )
        ),
    ?event(debug_test, {committed_keys, CommittedKeys}),
    WithoutBaseCommitment =
        hb_maps:without(
            [
                <<"id">>,
                <<"keyid">>,
                <<"signature">>,
                <<"commitment-device">>,
                <<"original-tags">>
            ],
            TABM,
            Opts
        ),
    ID = hb_util:human_id(TX#tx.id),
    Commitment0 = initialize_commitment(TX, NeedOriginalTags),
    Commitment1 = Commitment0#{
        <<"commitment-device">> => get_device(TX),
        <<"committer">> => Address,
        <<"committed">> => CommittedKeys,
        <<"keyid">> =>
            <<
                "publickey:",
                (hb_util:encode(TX#tx.owner))/binary
            >>,
        <<"signature">> => hb_util:encode(TX#tx.signature),
        <<"type">> => <<"rsa-pss-sha256">>
    },
    Commitment2 =
        case lists:member(<<"bundle-format">>, maps:values(CommittedKeys)) of
            true -> Commitment1#{ <<"bundle">> => true };
            false -> Commitment1
        end,
    Commitments = #{ ID => Commitment2 },
    set_map_value_or_throw( <<"commitments">>, Commitments, WithoutBaseCommitment).

initialize_commitment(TX, NeedOriginalTags) ->
    Commitment0 = add_original_tags(#{}, TX, NeedOriginalTags),
    add_original_fields(Commitment0, TX).

add_original_tags(Commitment, TX, false) ->
    Commitment;
add_original_tags(Commitment, TX, true) ->
    Commitment#{ <<"original-tags">> => encoded_tags_to_map(TX#tx.tags) }.

has_key_clashes(TABM) ->
    lists:any(
        fun(Values) when is_list(Values) ->
            length(Values) > 1;
        (_Value) ->
            false
        end,
        maps:values(TABM)
    ).

set_map_value_or_throw(Key, NewValue, Map) ->
    case Map of
        #{Key := NewValue} -> Map;
        #{Key := ExistingValue} -> throw({invalid_map_value, Key, ExistingValue, NewValue});
        _ -> maps:put(Key, NewValue, Map)
    end.

tx_to_tabm_data(Map, TX, Req, Opts) ->
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
            hb_util:to_lower(hb_ao:normalize_key(Key)) =:= Key
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
            hb_message:convert(
                Loaded,
                tabm,
                #{
                    <<"device">> => <<"structured@1.0">>,
                    <<"bundle">> => true
                },
                Opts
            )
    end.

tabm_to_tx_data(TX, TABM, Req, Opts) ->
    % Extract the <<"data">> field from the TABM
    RawData = hb_maps:get(<<"data">>, TABM, ?DEFAULT_DATA, Opts),
    Data = case RawData of
        _ when is_map(RawData) -> hb_util:ok(dev_codec_ans104:to(RawData, Req, Opts));
        _ -> RawData
    end,
    TABM0 = hb_maps:without([<<"data">>], TABM),

    % Extract any large tags from the TABM
    {TABM1, LargeTagMap} = tabm_to_tx_large_tags(TABM0, Req, Opts),

    MergedData = case {Data, hb_maps:size(LargeTagMap, Opts)} of
        {?DEFAULT_DATA, 0} ->
            ?DEFAULT_DATA;
        {_, 0} when is_record(Data, tx) ->
            % Only binary, list, and map are allowd for the #tx.data field
            #{ <<"data">> => Data };
        {_, 0} ->
            Data;
        {?DEFAULT_DATA, _} ->
            LargeTagMap;
        _ ->
            case is_binary(Data) of 
                true ->
                    LargeTagMap#{ <<"data">> => hb_util:ok(dev_codec_ans104:to(Data, Req, Opts)) };
                false ->
                    LargeTagMap#{ <<"data">> => Data }
            end
    end,
    TX0 = set_tx_value_or_throw(data, TX, ?DEFAULT_DATA, MergedData),

    % If the TABM explicitly sets the data key to default, we need to preserve it in
    % the #tx tags in order to maintain idempotency when going TABM -> TX -> TABM.
    TABM2 = case maps:is_key(<<"data">>, TABM) andalso RawData == ?DEFAULT_DATA of
        true -> TABM1#{ <<"data">> => ?DEFAULT_DATA };
        false -> TABM1
    end,
    {TX0, TABM2}.

tabm_to_tx_large_tags(TABM, Req, Opts) ->
    % Process each key-value pair in Structured
    maps:fold(
        fun(Key, Value, {AccTABM, AccDataItems}) ->
            case Value of
                % Leave small binaries as tags
                Value when is_binary(Value), byte_size(Value) < ?MAX_TAG_VAL ->
                    {AccTABM, AccDataItems};
                % All other values are converted to data items, and the keys removed from
                % the TABM (as they won't be converted to tags on the #tx record)
                _ ->
                    {
                        maps:remove(Key, AccTABM),
                        AccDataItems#{
                            hb_ao:normalize_key(Key) =>
                                hb_util:ok(dev_codec_ans104:to(Value, Req, Opts))
                        }
                    }
            end
        end,
        {TABM, #{}},
        TABM
    ).

tabm_to_tx_commitments(TX, TABM, Commitments, Opts) ->
    Commitment = get_commitment(Commitments),
    TX0 = tabm_to_tx_signature(TX, Commitment),
    {TX1, TABM1} = tabm_to_tx_original_fields(TX0, TABM, Commitment),
    {TX2, TABM2} =
        case maps:is_key(<<"original-tags">>, Commitment) of
            true -> tabm_to_tx_original_tags(TX1, TABM1, Commitment, Opts);
            false -> {tabm_to_tx_tags(Commitment, TX1, TABM1), TABM1}
        end,
    
    {TX2, hb_maps:without([<<"commitments">>], TABM2)}.

get_commitment(Commitments) ->
    case hb_maps:keys(Commitments) of
        [] ->
            #{};
        [ID] ->
            hb_maps:get(ID, Commitments);
        _ -> throw({multisignatures_not_supported_by_ans104, Commitments})
    end.

tabm_to_tx_signature(TX, Commitment) ->
    Signature = hb_util:decode(
        maps:get(<<"signature">>, Commitment,
            hb_util:encode(?DEFAULT_SIG)
        )
    ),
    Owner = hb_util:decode(
        dev_codec_httpsig_keyid:remove_scheme_prefix(
            maps:get(
                <<"keyid">>,
                Commitment,
                hb_util:encode(?DEFAULT_OWNER)
            )
        )
    ),
    TX0 = set_tx_value_or_throw(signature, TX, ?DEFAULT_SIG, Signature),
    TX1 = set_tx_value_or_throw(owner, TX0, ?DEFAULT_OWNER, Owner),
    TX1.

tabm_to_tx_original_fields(TX, TABM, Commitment) ->
    FieldDefaults = hb_message:default_tx_list(),
    
    {TX0, TABM0} = lists:foldl(
        fun(NormalizedKey, {AccTX, AccTABM}) ->
            OriginalKey = original_key(NormalizedKey),
            case maps:find(OriginalKey, Commitment) of
                error ->
                    {AccTX, AccTABM};
                {ok, Value} ->
                    % Get the default value for this field
                    {_, DefaultValue} = lists:keyfind(NormalizedKey, 1, FieldDefaults),
                    
                    % Coerce the value
                    case coerce_value(NormalizedKey, Value, DefaultValue) of
                        {ok, CoercedValue} ->
                            % Set the value on TX
                            FieldAtom = hb_util:atom(NormalizedKey),
                            NewTX = set_tx_value_or_throw(FieldAtom, AccTX, DefaultValue, CoercedValue),
                            
                            % Remove the field from TABM
                            NewTABM = maps:remove(NormalizedKey, AccTABM),
                            
                            {NewTX, NewTABM};
                        error ->
                            ?event(warning, {coercion_failed, {field, NormalizedKey},
                                {value, Value}, {default_value, DefaultValue}}),
                            {AccTX, AccTABM}
                    end
            end
        end,
        {TX, TABM},
        ?ORIGINAL_FIELDS
    ),
    {TX0, TABM0}.

tabm_to_tx_original_tags(TX, TABM, Commitment, Opts) ->
    OriginalTags = hb_maps:get(<<"original-tags">>, Commitment, #{}),
    Tags = tag_map_to_encoded_tags(OriginalTags),

    % XXX TODO: re-add the check that compares original tags to the TABM
    % case maps:size(OriginalTags) > 0 of
    %     true ->
    %         ExpectedTags = hb_util:lower_case_key_map(deduplicating_from_list(TABM, Opts), Opts),
    %         case ExpectedTags == Tags of
    %             true -> ok;
    %             false ->
    %                 ?event(warning, {invalid_original_tags, {expected, ExpectedTags}, {given, Tags}}),
    %                 throw({invalid_original_tags, ExpectedTags, Tags})
    %         end;
    %     false ->
    %         ok
    % end,

    TABM0 = lists:foldl(
        fun({Key, Value}, Acc) ->
            NormalizedKey = hb_util:to_lower(hb_ao:normalize_key(Key)),
            maps:remove(NormalizedKey, Acc)
        end,
        TABM,
        Tags
    ),
    TX0 = set_tx_value_or_throw(tags, TX, [], Tags),
    {TX0, TABM0}.

tabm_to_tx_tags(Commitment, TX, TABM) ->
    Committed =
        hb_util:message_to_ordered_list(
            hb_maps:get(<<"committed">>, Commitment, #{})
        ),
    ?event(debug_test, {tabm_to_tx_tags, {explicit, TABM}}),
    Tags =
        lists:filtermap(
            fun(Key) ->
                case maps:find(Key, TABM) of
                    {ok, Value} ->
                        case lists:member({Key, Value}, TX#tx.tags) of
                            true -> false;
                            false -> {true, {Key, Value}}
                        end;
                    error -> false
                end
            end,
            Committed
        ),
    set_tx_value_or_throw(tags, TX, [], Tags).

%% @doc Helper function to set a field value in a TX record, throwing if the field
%% already has a non-default value
set_tx_value_or_throw(FieldAtom, TX, DefaultValue, NewValue) ->
    FieldIndex = field_index(FieldAtom),
    case element(FieldIndex, TX) of 
        DefaultValue ->
            setelement(FieldIndex, TX, NewValue);
        ExistingValue ->
            throw({invalid_field_value, FieldAtom, ExistingValue, NewValue})
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

%%% ------------------------------------------------------------------------------------------
%%% Generic helpers
%%% ------------------------------------------------------------------------------------------

invalid_fields(#tx{ format = ans104 }) ->
    ?BASE_INVALID_FIELDS ++ [<<"data_size">>];
invalid_fields(#tx{ format = 1 }) ->
    ?BASE_INVALID_FIELDS ++ [<<"data_size">>];
invalid_fields(#tx{ format = 2 } = TX) ->
    case TX#tx.data of
        ?DEFAULT_DATA -> ?BASE_INVALID_FIELDS;
        _ -> ?BASE_INVALID_FIELDS ++ [<<"data_size">>, <<"data_root">>]
    end.

%% @doc Deduplicate a list of key-value pairs by key, generating a list of
%% values for each normalized key if there are duplicates.
deduplicating_from_list(Tags, Opts) ->
    % Aggregate any duplicated tags into an ordered list of values.
    Aggregated =
        lists:foldl(
            fun({Key, Value}, Acc) ->
                NormKey = hb_util:to_lower(hb_ao:normalize_key(Key)),
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
coerce_value(<<"target">>, Value, _DefaultValue) ->
    {ok, hb_util:decode(Value)};
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
    format(normalize(Item), Indent);
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
%%% Tests
%%% ------------------------------------------------------------------------------------------

encoded_tags_to_map_test_() ->
    [
        fun test_encoded_tags_to_map_happy/0,
        fun test_tag_map_to_encoded_tags_happy/0
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
            ?assertEqual(Expected, encoded_tags_to_map(Input), Label),
            % Verify roundtrip
            ?assertEqual(Input, tag_map_to_encoded_tags(Expected), Label ++ " roundtrip")
        end,
        TestCases
    ).

%% @doc Test tag_map_to_encoded_tags/1 with multiple test cases in a list.
test_tag_map_to_encoded_tags_happy() ->
    TestCases = [
        {empty, #{}, []},
        {single, #{
            <<"1">> => #{<<"name">> => <<"test-key">>, <<"value">> => <<"test-val">>}
        }, [
            {<<"test-key">>, <<"test-val">>}
        ]},
        {multiple, #{
            <<"1">> => #{<<"name">> => <<"alpha">>, <<"value">> => <<"1">>},
            <<"2">> => #{<<"name">> => <<"beta">>, <<"value">> => <<"2">>},
            <<"3">> => #{<<"name">> => <<"gamma">>, <<"value">> => <<"3">>}
        }, [
            {<<"alpha">>, <<"1">>},
            {<<"beta">>, <<"2">>},
            {<<"gamma">>, <<"3">>}
        ]}
    ],
    lists:foreach(
        fun({Label, Input, Expected}) ->
            ?assertEqual(Expected, tag_map_to_encoded_tags(Input), Label),
            % Verify roundtrip
            ?assertEqual(Input, encoded_tags_to_map(Expected), Label ++ " roundtrip")
        end,
        TestCases
    ).

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