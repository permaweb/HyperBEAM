%%% @doc Utility module for routing functionality to ar_bundles.erl or
%%% ar_tx.erl based off #tx.format.
-module(dev_arweave_common).
-export([reset_ids/1, generate_id/2, normalize/1]).
-export([convert_bundle_list_to_map/1, convert_bundle_map_to_list/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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
normalize(Item = #tx{data = Bin}) when is_binary(Bin) ->
    ?event({normalize, binary,
        hb_util:human_id(Item#tx.unsigned_id), hb_util:human_id(Item#tx.id)}),
    reset_ids(
        normalize_data_root(
            normalize_data_size(
                reset_owner_address(
                    Item))));
normalize(Bundle) when is_list(Bundle); is_map(Bundle) ->
    ?event({normalize, bundle}),
    normalize(#tx{ data = Bundle });
normalize(Item = #tx { data = Data }) when is_list(Data) ->
    ?event({normalize, list,
        hb_util:human_id(Item#tx.unsigned_id), hb_util:human_id(Item#tx.id)}),
    normalize(Item#tx{data = convert_bundle_list_to_map(Data)});
normalize(Item = #tx{data = Data}) when is_map(Data) ->
    {Manifest, Bin} = ar_bundles:serialize_bundle(Item#tx.data, true),
    SerializedItem =
        Item#tx{
            data = Bin,
            manifest = Manifest,
            tags = add_manifest_tags(
                add_bundle_tags(Item#tx.tags),
                ar_bundles:id(Manifest, unsigned)
            )
        },
    normalize(SerializedItem).

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

add_bundle_tags(Tags) -> ?BUNDLE_TAGS ++ (Tags -- ?BUNDLE_TAGS).

add_manifest_tags(Tags, ManifestID) ->
    lists:filter(
        fun({TagName, _}) ->
            hb_util:to_lower(TagName) =/= <<"bundle-map">>
        end,
        Tags
    ) ++ [{<<"bundle-map">>, hb_util:encode(ManifestID)}].

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


