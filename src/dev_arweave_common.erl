%%% @doc Utility module for routing functionality to ar_bundles.erl or
%%% ar_tx.erl based off #tx.format.
-module(dev_arweave_common).
-export([is_signed/1, type/1, tagfind/3, find_key/3]).
-export([reset_ids/1, generate_id/2, normalize/1, serialize_data/1]).
-export([convert_bundle_list_to_map/1, convert_bundle_map_to_list/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Check if an item is signed.
is_signed(TX) ->
    TX#tx.signature =/= ?DEFAULT_SIG.

type(Item) ->
    Format = tagfind(<<"bundle-format">>, Item#tx.tags, <<>>),
    Version = tagfind(<<"bundle-version">>, Item#tx.tags, <<>>),
    MapTXID = tagfind(<<"bundle-map">>, Item#tx.tags, <<>>),
    case {hb_util:to_lower(Format), hb_util:to_lower(Version), MapTXID} of
        {<<"binary">>, <<"2.0.0">>, <<>>} ->
            list;
        {<<"binary">>, <<"2.0.0">>, _} ->
            map;
        _ ->
            binary
    end.

%% @doc Case-insensitively find a tag in a list and return its value.
tagfind(Key, Tags, Default) ->
    LowerCaseKey = hb_util:to_lower(Key),
    Found = lists:search(fun({TagName, _}) ->
        hb_util:to_lower(TagName) == LowerCaseKey
    end, Tags),
    case Found of
        {value, {_TagName, Value}} -> Value;
        false -> Default
    end.

%% @doc Find a key potentially with a +link specifier
find_key(Key, Map, Opts) ->
    case hb_maps:find(Key, Map, Opts) of
        {ok, Value} -> {Key, Value};
        error ->
            KeyLink = <<Key/binary, "+link">>,
            case hb_maps:find(KeyLink, Map, Opts) of
                {ok, Value} -> {KeyLink, Value};
                error -> error
            end
    end.

%% @doc Re-calculate both of the IDs for a #tx. This is a wrapper
%% function around `update_ids/1' that ensures both IDs are set from
%% scratch.
reset_ids(TX) ->
    update_ids(TX#tx{unsigned_id = ?DEFAULT_ID, id = ?DEFAULT_ID}).

%% @doc Take an #tx and ensure that both the unsigned and signed IDs are
%% appropriately set. This function is structured to fall through all cases
%% of poorly formed items, recursively ensuring its correctness for each case
%% until the item has a coherent set of IDs.
%% The cases in turn are:
%% - The item has no unsigned_id. This is never valid.
%% - The item has the default signature and ID. This is valid.
%% - The item has the default signature but a non-default ID. Reset the ID.
%% - The item has a signature. We calculate the ID from the signature.
%% - Valid: The item is fully formed and has both an unsigned and signed ID.
update_ids(TX = #tx{ unsigned_id = ?DEFAULT_ID }) ->
    update_ids(TX#tx{unsigned_id = generate_id(TX, unsigned)});
update_ids(TX = #tx{ id = ?DEFAULT_ID, signature = ?DEFAULT_SIG }) ->
    TX;
update_ids(TX = #tx{ signature = ?DEFAULT_SIG }) ->
    TX#tx{ id = ?DEFAULT_ID };
update_ids(TX = #tx{ signature = Sig }) when Sig =/= ?DEFAULT_SIG ->
    TX#tx{ id = generate_id(TX, signed) };
update_ids(TX) -> TX.

%% @doc Generate the ID for a given transaction.
generate_id(TX, signed) ->
    crypto:hash(sha256, TX#tx.signature);
generate_id(TX, unsigned) ->
    crypto:hash(sha256,
        generate_signature_data_segment(TX#tx{ owner = ?DEFAULT_OWNER })).

generate_signature_data_segment(TX = #tx{ format = ans104 }) ->
    ar_bundles:data_item_signature_data(TX);
generate_signature_data_segment(TX) ->
    ar_tx:generate_signature_data_segment(TX).

%% @doc Ensure that a data item (potentially containing a map or list) has a
%% standard, serialized form.
normalize(not_found) -> throw(not_found);
normalize(TX = #tx{data = Bin}) when is_binary(Bin) ->
    ?event({normalize, binary,
        hb_util:human_id(TX#tx.unsigned_id), hb_util:human_id(TX#tx.id)}),
    reset_ids(
        normalize_data_root(
            normalize_data_size(
                reset_owner_address(
                    TX))));
normalize(Bundle) when is_list(Bundle); is_map(Bundle) ->
    ?event({normalize, bundle}),
    normalize(#tx{ data = Bundle });
normalize(TX) ->
    ?event({normalize, TX}),
    {ItemType, SerializedTX} = serialize_data(TX, true),
    ?event({serialized_tx, ItemType, SerializedTX}),
    NormalizedTX = maybe_add_bundle_tags(ItemType, SerializedTX),
    ?event({normalized_tx, NormalizedTX}),
    normalize(NormalizedTX).

%%% XXX TODO: look at these two is_signed checks - might need to do
%%% has_manifest or similar
%%% and perhaps it's codec:to where we can check for has_manifest?

serialize_data(TX) -> serialize_data(TX, false).
serialize_data(Item = #tx{data = Data}, _) when is_binary(Data) ->
    {binary, Item};
serialize_data(Item = #tx{data = Data}, NormalizeChildren) ->
    {BundleType, ConvertedData} = 
        case {type(Item), is_list(Data), is_map(Data)} of
            {map, true, false} ->
                % Signed transaction with bundle-map tag and list data
                {map, convert_bundle_list_to_map(Data)};
            {list, false, true} ->
                % Signed transaction without bundle-map tag and map data
                {list, convert_bundle_map_to_list(Data)};
            {_, true, false} ->
                % Unsigned transaction with list data
                {list, convert_bundle_list_to_map(Data)};
            {_, false, true} ->
                {map, Data};
            _ ->
                {binary, Data}
        end,
    ?event({serialize_data,
        hb_util:human_id(Item#tx.unsigned_id), hb_util:human_id(Item#tx.id),
        {normalize_children, NormalizeChildren},
        {type, BundleType},
        {is_list, is_list(Data)},
        {is_map, is_map(Data)}}),
    {Manifest, SerializedData} =
        ar_bundles:serialize_bundle(BundleType, ConvertedData, NormalizeChildren),
    {BundleType, Item#tx{data = SerializedData, manifest = Manifest}}.

convert_bundle_list_to_map(Data) ->
    maps:from_list(
        lists:zipwith(
            fun(Index, MapItem) ->
                {
                    integer_to_binary(Index),
                    MapItem
                }
            end,
            lists:seq(1, length(Data)),
            Data
        )
    ).

convert_bundle_map_to_list(Data) ->
    lists:map(
        fun(Index) ->
            maps:get(list_to_binary(integer_to_list(Index)), Data)
        end,
        lists:seq(1, maps:size(Data))
    ).

maybe_add_bundle_tags(BundleType, TX) -> 
    BundleTags = case BundleType of
        binary ->
            % Item is either not a bundle, or if it is a bundle that has
            % been serialized to binary, it should already have bundle tags.
            [];
        list ->
            ?BUNDLE_TAGS;
        map ->
            ManifestID = ar_bundles:id(TX#tx.manifest, unsigned),
            ?BUNDLE_TAGS ++ [{<<"bundle-map">>, hb_util:encode(ManifestID)}]
    end,
    ExistingTagNames = [hb_util:to_lower(TagName) || {TagName, _} <- TX#tx.tags],
    FilteredBundleTags = lists:filter(
        fun({TagName, _}) ->
            not lists:member(hb_util:to_lower(TagName), ExistingTagNames)
        end,
        BundleTags
    ),
    TX#tx{tags = FilteredBundleTags ++ TX#tx.tags }.

%% @doc Reset the data size of a data item. Assumes that the data is already normalized.
normalize_data_size(Item = #tx{data = Bin}) when is_binary(Bin) ->
    Item#tx{data_size = byte_size(Bin)};
normalize_data_size(Item) -> Item.

reset_owner_address(TX = #tx{format = ans104}) ->
    TX;
reset_owner_address(TX) ->
    TX#tx{owner_address = ar_tx:get_owner_address(TX)}.


normalize_data_root(Item = #tx{data = Bin, format = 2})
        when is_binary(Bin) andalso Bin =/= ?DEFAULT_DATA ->
    Item#tx{data_root = ar_tx:data_root(Bin)};
normalize_data_root(Item) -> Item.


