%%% @doc Utility module for routing functionality to ar_bundles.erl or
%%% ar_tx.erl based off #tx.format.
-module(dev_arweave_common).
-export([is_signed/1, type/1, tagfind/3, find_key/3]).
-export([reset_ids/1, generate_id/2, normalize/1, serialize_data/1]).
-export([convert_bundle_list_to_map/1, convert_bundle_map_to_list/1]).
-export([serialize_sig_type/1, deserialize_sig_type/1]).
-export([log_conversion/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Check if an item is signed.
is_signed(TX) ->
    TX#tx.signature =/= ?DEFAULT_SIG.

type(Item = #tx{ format = ans104 }) ->
    % Always trust tags for ans104 items.
    type_from_tags(Item);
type(Item = #tx{ data = Data }) 
        when not is_binary(Data) orelse Data =:= ?DEFAULT_DATA ->
    % Trust tags for L1 TX without binary data
    type_from_tags(Item);
type(Item) ->
    % If an L1 TX has bundle tags but does not have a valid bundle header,
    % treat it as a binary. We have to do this since it may still be a valid
    % L1 TX even if the tags are sneaky.
    Result = case type_from_tags(Item) of
        binary ->
            binary;
        BundleType when is_binary(Item#tx.data) ->
            case ar_bundles:decode_bundle_header(Item#tx.data) of
                invalid_bundle_header ->
                    binary;
                {_Count, _Header} ->
                    BundleType
            end
    end,
    Result.
type_from_tags(Item) ->
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
normalize(TX) ->
    ?event({normalize, TX}),
    {ItemType, SerializedTX} = serialize_data(TX, true),
    ?event({serialized_tx, ItemType, SerializedTX}),
    NormalizedTX = maybe_add_bundle_tags(ItemType, SerializedTX),
    ?event({normalized_tx, NormalizedTX}),
    normalize(NormalizedTX).

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
                {list, Data};
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
normalize_data_size(Item = #tx{data = Bin})
        when is_binary(Bin) andalso Bin =/= ?DEFAULT_DATA ->
    Item#tx{data_size = byte_size(Bin)};
normalize_data_size(Item) -> Item.

reset_owner_address(TX = #tx{format = ans104}) ->
    TX;
reset_owner_address(TX) ->
    TX#tx{owner_address = ar_tx:get_owner_address(TX)}.


normalize_data_root(Item = #tx{data = Bin, format = 1})
        when is_binary(Bin) andalso Bin =/= ?DEFAULT_DATA ->
    Item#tx{data_root = ar_tx:data_root(legacy, Bin)};
normalize_data_root(Item = #tx{data = Bin, format = 2})
        when is_binary(Bin) andalso Bin =/= ?DEFAULT_DATA ->
    Item#tx{data_root = ar_tx:data_root(arweavejs, Bin)};
normalize_data_root(Item) -> Item.

serialize_sig_type({rsa, 65537}) -> <<"rsa-pss-sha256">>;
serialize_sig_type({ecdsa, secp256k1}) -> <<"ecdsa-secp256k1-sha256">>;
serialize_sig_type(Type) ->
    ?event(error, {signature_type, {type, Type}}),
    throw({invalid_signature_type, Type}).

deserialize_sig_type(<<"rsa-pss-sha256">>) -> {rsa, 65537};
deserialize_sig_type(<<"ecdsa-secp256k1-sha256">>) -> {ecdsa, secp256k1};
deserialize_sig_type(<<"unsigned-sha256">>) -> {rsa, 65537};
deserialize_sig_type(Type) ->
    ?event(error, {signature_type, {type, Type}}),
    throw({invalid_signature_type, Type}).

%% @doc Turn off debug_print_verify when logging within the to/from functions
%% to avoid infinite recursion.
log_conversion(Topic, X) ->
    ?event(Topic, X, #{debug_print_verify => false}).
%%%===================================================================
%%% Tests.
%%%===================================================================

tagfind_test() ->
    Default = <<"default">>,
    ?assertEqual(
        <<"v1">>,
        tagfind(<<"Foo">>, [{<<"fOo">>, <<"v1">>}], Default)
    ),
    ?assertEqual(
        Default,
        tagfind(<<"Missing">>, [{<<"foo">>, <<"v">>}], Default)
    ).


type_test() ->
    % Basic type from tags
    assert_type(binary, []),
    assert_type(binary, [{<<"tag">>, <<"value">>}]),
    assert_type(list, [
        {<<"bundle-format">>, <<"binary">>},
        {<<"tag">>, <<"value">>},
        {<<"bundle-version">>, <<"2.0.0">>}]),
    assert_type(map, [
        {<<"bundle-format">>, <<"binary">>},
        {<<"tag">>, <<"value">>},
        {<<"bundle-version">>, <<"2.0.0">>},
        {<<"bundle-map">>, <<"JmtD0fwFqJTK4P_XexVqBQdnDc0-C7FFIOge6GEOJE8">>}]),
    % L1 TX with bundle tags, but data is not a valid bundle.
    ?assertEqual(binary,
        type(#tx{
            format = 1,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}],
            data = <<"not a bundle">>
        })),
    ?assertEqual(binary,
        type(#tx{
            format = 2,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}],
            data = <<"not a bundle">>
        })),
    ?assertEqual(binary,
        type(#tx{
            format = 1,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}],
            data = <<1:256/little, <<"not a bundle">>/binary>>
        })),
    ?assertEqual(binary,
        type(#tx{
            format = 2,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}],
            data = <<1:256/little, <<"not a bundle">>/binary>>
        })),
    % L1 TX with bundle tags, and non-binary data
    ?assertEqual(list,
        type(#tx{
            format = 1,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}],
            data = []
        })),
    ?assertEqual(list,
        type(#tx{
            format = 2,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>}],
            data = []
        })),
    ?assertEqual(map,
        type(#tx{
            format = 1,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>},
                {<<"bundle-map">>, <<"JmtD0fwFqJTK4P_XexVqBQdnDc0-C7FFIOge6GEOJE8">>}],
            data = #{
                <<"1">> => <<"value1">>,
                <<"2">> => <<"value2">>
            }
        })),
    ?assertEqual(map,
        type(#tx{
            format = 2,
            tags = [
                {<<"bundle-format">>, <<"binary">>},
                {<<"bundle-version">>, <<"2.0.0">>},
                {<<"bundle-map">>, <<"JmtD0fwFqJTK4P_XexVqBQdnDc0-C7FFIOge6GEOJE8">>}],
            data = #{
                <<"1">> => <<"value1">>,
                <<"2">> => <<"value2">>
            }
        })),
    ok.

assert_type(ExpectedType, Tags) ->
    ?assertEqual(ExpectedType, type(#tx{format = 1, tags = Tags})),
    ?assertEqual(ExpectedType, type(#tx{format = 2, tags = Tags})),
    ?assertEqual(ExpectedType, type(#tx{format = ans104, tags = Tags})).