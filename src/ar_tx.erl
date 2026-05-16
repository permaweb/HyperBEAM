%%% @doc The module with utilities for transaction creation, signing, and verification.
-module(ar_tx).

-export([sign/2, verify/1, verify_tx_id/2]).
-export([id/1, id/2, get_owner_address/1, data_root/1, data_root/2]).
-export([is_signed/1, type/1, tagfind/3]).
-export([reset_ids/1, generate_id/2, normalize/1, serialize_data/1]).
-export([convert_bundle_list_to_map/1, convert_bundle_map_to_list/1]).
-export([serialize_sig_type/1, deserialize_sig_type/1]).
-export([generate_signature_data_segment/1, generate_chunk_id/1]).
-export([json_struct_to_tx/1, tx_to_json_struct/1]).
-export([generate_chunk_tree/1, generate_chunk_tree/2]).
-export([chunk_binary/2, chunk_binary/3, chunking_mode/1]).
-export([chunks_to_size_tagged_chunks/1, sized_chunks_to_sized_chunk_ids/1]).
-export([get_weave_size_increase/2]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Minimum chunk size targeted by the arweave-js chuking algorithm.
-define(MIN_CHUNK_SIZE, (32 * 1024)).

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Cryptographically sign (claim ownership of) a v2 transaction.
%% Used in tests and by the handler of the POST /unsigned_tx endpoint, which is
%% disabled by default.
sign(TX, {PrivKey, PubKey = {KeyType, Owner}}) ->
    TX2 = TX#tx{ owner = Owner, signature_type = KeyType },
    TX3 = TX2#tx{ owner_address = get_owner_address(TX2) },
    SignatureDataSegment = generate_signature_data_segment(TX3),
    sign(TX3, PrivKey, PubKey, SignatureDataSegment).

sign(TX, PrivKey, PubKey = {KeyType, Owner}) ->
    TX2 = TX#tx{ owner = Owner, signature_type = KeyType },
    TX3 = TX2#tx{ owner_address = get_owner_address(TX2) },
    SignatureDataSegment = generate_signature_data_segment(TX3),
    sign(TX3, PrivKey, PubKey, SignatureDataSegment).

%% @doc Cryptographically sign (claim ownership of) a v1 transaction.
%% Used in tests and by the handler of the POST /unsigned_tx endpoint, which is
%% disabled by default.
sign_v1(TX, {PrivKey, PubKey = {_, Owner}}) ->
    sign(TX, PrivKey, PubKey, signature_data_segment_v1(TX#tx{ owner = Owner })).

sign_v1(TX, PrivKey, PubKey = {_, Owner}) ->
    sign(TX, PrivKey, PubKey, signature_data_segment_v1(TX#tx{ owner = Owner })).

%% @doc Verify whether a transaction is valid.
%% 
%% Checks that are missing:
%% - format 2 unsupported pre-2.0
%% - verify_denomination
%% - is_tx_fee_sufficient
%% - tx_field_size_limit_v1/v2
%% - check_last_tx
%% - validate_overspend
%% - verify_malleabilitiy
%% - verify_target_length
verify(TX) ->
    From = ar_wallet:to_address(TX#tx.owner, TX#tx.signature_type),
    Checks = [
        {"tx_format_not_supported", TX#tx.format == 1 orelse TX#tx.format == 2},
        {"tx_signature_type_not_supported", verify_signature_type(TX)},
        {"quantity_negative", TX#tx.quantity >= 0},
        {"same_owner_as_target", (From =/= TX#tx.target)},
        {"tx_id_not_valid", verify_hash(TX)},
        {"tx_signature_not_valid", verify_signature(TX)}
    ] ++ verify_v1(TX) ++ verify_v2(TX),
    collect_validation_results(TX#tx.id, Checks).

%% @doc Verify the given transaction actually has the given identifier.
verify_tx_id(ExpectedID, #tx{ id = ID } = TX) ->
    ExpectedID == ID andalso verify_signature(TX) andalso verify_hash(TX).

%% @doc Return or generate the ID for a given transaction.
id(TX) -> id(TX, unsigned).
id(#tx{ id = ?DEFAULT_ID, signature = ?DEFAULT_SIG }, signed) ->
    not_signed;
id(TX = #tx{ id = ?DEFAULT_ID }, signed) ->
    generate_id(TX, signed);
id(#tx{ id = ID }, signed) ->
    ID;
id(TX = #tx{ unsigned_id = ?DEFAULT_ID }, unsigned) ->
    generate_id(TX, unsigned);
id(#tx{ unsigned_id = UnsignedID }, unsigned) ->
    UnsignedID.

%% @doc Check if an item is signed.
is_signed(TX) ->
    TX#tx.signature =/= ?DEFAULT_SIG.

type(Item = #tx{ format = ans104 }) ->
    % Always trust tags for ans104 items.
    type_from_tags(Item);
type(Item = #tx{ data = Data })
        when not is_binary(Data) orelse Data =:= ?DEFAULT_DATA ->
    % Trust tags for L1 TX without binary data.
    type_from_tags(Item);
type(Item) ->
    % If an L1 TX has bundle tags but does not have a valid bundle header,
    % treat it as a binary. It may still be a valid L1 TX even if the tags are
    % sneaky.
    case type_from_tags(Item) of
        binary ->
            binary;
        BundleType when is_binary(Item#tx.data) ->
            case ar_bundles:decode_bundle_header(Item#tx.data) of
                invalid_bundle_header -> binary;
                {_Count, _Header} -> BundleType
            end
    end.

type_from_tags(Item) ->
    Format = tagfind(<<"bundle-format">>, Item#tx.tags, <<>>),
    Version = tagfind(<<"bundle-version">>, Item#tx.tags, <<>>),
    MapTXID = tagfind(<<"bundle-map">>, Item#tx.tags, <<>>),
    case {hb_util:to_lower(Format), hb_util:to_lower(Version), MapTXID} of
        {<<"binary">>, <<"2.0.0">>, <<>>} -> list;
        {<<"binary">>, <<"2.0.0">>, _} -> map;
        _ -> binary
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

%% @doc Re-calculate both IDs for a #tx from scratch.
reset_ids(TX) ->
    update_ids(TX#tx{unsigned_id = ?DEFAULT_ID, id = ?DEFAULT_ID}).

%% @doc Ensure that both transaction IDs are coherent.
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
    crypto:hash(
        sha256,
        generate_signature_data_segment(TX#tx{ owner = ?DEFAULT_OWNER })
    ).

%% @doc Ensure that a data item has a standard, serialized form.
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
                {map, convert_bundle_list_to_map(Data)};
            {list, false, true} ->
                {list, convert_bundle_map_to_list(Data)};
            {_, true, false} ->
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
                {integer_to_binary(Index), MapItem}
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
        binary -> [];
        list -> ?BUNDLE_TAGS;
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

%% @doc Reset the data size of a data item.
normalize_data_size(Item = #tx{data = Bin})
        when is_binary(Bin) andalso Bin =/= ?DEFAULT_DATA ->
    Item#tx{data_size = byte_size(Bin)};
normalize_data_size(Item) -> Item.

reset_owner_address(TX = #tx{format = ans104}) ->
    TX;
reset_owner_address(TX) ->
    TX#tx{owner_address = get_owner_address(TX)}.

normalize_data_root(Item = #tx{data = Bin, format = 1})
        when is_binary(Bin) andalso Bin =/= ?DEFAULT_DATA ->
    Item#tx{data_root = data_root(legacy, Bin)};
normalize_data_root(Item = #tx{data = Bin, format = 2})
        when is_binary(Bin) andalso Bin =/= ?DEFAULT_DATA ->
    Item#tx{data_root = data_root(arweavejs, Bin)};
normalize_data_root(Item) -> Item.

serialize_sig_type({rsa, 65537}) -> ?RSA_SIGN_TYPE;
serialize_sig_type({ecdsa, secp256k1}) -> ?ECDSA_SIGN_TYPE;
serialize_sig_type(?EDDSA_KEY_TYPE) -> ?EDDSA_SIGN_TYPE;
serialize_sig_type(?SOLANA_KEY_TYPE) -> ?SOLANA_SIGN_TYPE;
serialize_sig_type(?ETHEREUM_KEY_TYPE) -> ?ETHEREUM_SIGN_TYPE;
serialize_sig_type(?TYPED_ETHEREUM_KEY_TYPE) -> ?TYPED_ETHEREUM_SIGN_TYPE;
serialize_sig_type(Type) ->
    ?event(error, {signature_type, {type, Type}}),
    throw({invalid_signature_type, Type}).

deserialize_sig_type(?RSA_SIGN_TYPE) -> {rsa, 65537};
deserialize_sig_type(?ECDSA_SIGN_TYPE) -> {ecdsa, secp256k1};
deserialize_sig_type(?EDDSA_SIGN_TYPE) -> ?EDDSA_KEY_TYPE;
deserialize_sig_type(?SOLANA_SIGN_TYPE) -> ?SOLANA_KEY_TYPE;
deserialize_sig_type(?ETHEREUM_SIGN_TYPE) -> ?ETHEREUM_KEY_TYPE;
deserialize_sig_type(?TYPED_ETHEREUM_SIGN_TYPE) -> ?TYPED_ETHEREUM_KEY_TYPE;
deserialize_sig_type(<<"unsigned-sha256">>) -> {rsa, 65537};
deserialize_sig_type(Type) ->
    ?event(error, {signature_type, {type, Type}}),
    throw({invalid_signature_type, Type}).

%% @doc Return the transaction's owner address. Take the cached value if available.
get_owner_address(#tx{ owner = ?DEFAULT_OWNER }) ->
    not_set;
get_owner_address(#tx{ owner = Owner, signature_type = KeyType, owner_address = not_set }) ->
    ar_wallet:to_address(Owner, KeyType);
get_owner_address(#tx{ owner_address = OwnerAddress }) ->
    OwnerAddress.

data_root(Bin) ->
    data_root(arweavejs, Bin).

data_root(Mode, Bin) ->
    Chunks = chunk_binary(Mode, ?DATA_CHUNK_SIZE, Bin),
    SizeTaggedChunks = chunks_to_size_tagged_chunks(Chunks),
    SizeTaggedChunkIDs = sized_chunks_to_sized_chunk_ids(SizeTaggedChunks),
    {Root, _} = ar_merkle:generate_tree(SizeTaggedChunkIDs),
    Root.

%%%===================================================================
%%% Private functions.
%%%===================================================================

%% @doc Verify the transaction's signature type is supported for the given format.
%% Format 1 transactions only support RSA with 65537.
%% Format 2 transactions support RSA with 65537 and ECDSA with secp256k1.
verify_signature_type(#tx{ format = 1 } = TX) ->
	case TX#tx.signature_type of
		{?RSA_SIGN_ALG, 65537} ->
			true;
		_ ->
			false
	end;
verify_signature_type(#tx{ format = 2 } = TX) ->
	case TX#tx.signature_type of
		{?RSA_SIGN_ALG, 65537} ->
			true;
		{?ECDSA_SIGN_ALG, secp256k1} ->
			true;
		_ ->
			false
	end;
verify_signature_type(_) ->
	false.

%% @doc Verify the transaction's signature.
verify_signature(TX = #tx{ signature_type = SigType }) ->
    case generate_signature_data_segment(TX) of
        error ->
            false;
        SignatureDataSegment ->
            ar_wallet:verify({SigType, TX#tx.owner}, SignatureDataSegment, TX#tx.signature)
    end.

%% @doc Generate the data segment to be signed for a given TX.
generate_signature_data_segment(TX = #tx{ format = ans104 }) ->
    ar_bundles:data_item_signature_data(TX);
generate_signature_data_segment(#tx{ format = 2 } = TX) ->
    case TX#tx.signature_type of
        {?ECDSA_SIGN_ALG, secp256k1} ->
            signature_data_segment_v2_no_public_key(TX);
        {?RSA_SIGN_ALG, 65537} ->
            signature_data_segment_v2(TX)
    end;
generate_signature_data_segment(#tx{ format = 1 } = TX) ->
    signature_data_segment_v1(TX);
generate_signature_data_segment(_) ->
    error.

%%% @doc Generate the data segment to be signed for a given v2 TX.
signature_data_segment_v2(TX) ->
    List = [
        << (integer_to_binary(TX#tx.format))/binary >>,
        << (TX#tx.owner)/binary >>,
        << (TX#tx.target)/binary >>,
        << (list_to_binary(integer_to_list(TX#tx.quantity)))/binary >>,
        << (list_to_binary(integer_to_list(TX#tx.reward)))/binary >>,
        << (TX#tx.anchor)/binary >>,
        tags_to_list(TX#tx.tags),
        << (integer_to_binary(TX#tx.data_size))/binary >>,
        << (TX#tx.data_root)/binary >>
    ],
    List2 =
        case TX#tx.denomination > 0 of
            true ->
                [<< (integer_to_binary(TX#tx.denomination))/binary >> | List];
            false ->
                List
        end,
    ar_deep_hash:hash(List2).

%% @doc Generate the data segment to be signed for a given v2 TX with ECDSA.
%% ECDSA signatures do not include the owner public key in the signature data segment.
signature_data_segment_v2_no_public_key(TX) ->
    List = [
        << (integer_to_binary(TX#tx.format))/binary >>,
        << (TX#tx.target)/binary >>,
        << (list_to_binary(integer_to_list(TX#tx.quantity)))/binary >>,
        << (list_to_binary(integer_to_list(TX#tx.reward)))/binary >>,
        << (TX#tx.anchor)/binary >>,
        tags_to_list(TX#tx.tags),
        << (integer_to_binary(TX#tx.data_size))/binary >>,
        << (TX#tx.data_root)/binary >>
    ],
    List2 =
        case TX#tx.denomination > 0 of
            true ->
                [<< (integer_to_binary(TX#tx.denomination))/binary >> | List];
            false ->
                List
        end,
    ar_deep_hash:hash(List2).

%% @doc Generate the data segment to be signed for a given v1 TX.
signature_data_segment_v1(TX) ->
    case TX#tx.denomination > 0 of
        true ->
            ar_deep_hash:hash([
                << (integer_to_binary(TX#tx.denomination))/binary >>,
                << (TX#tx.owner)/binary >>,
                << (TX#tx.target)/binary >>,
                << (list_to_binary(integer_to_list(TX#tx.quantity)))/binary >>,
                << (list_to_binary(integer_to_list(TX#tx.reward)))/binary >>,
                << (TX#tx.anchor)/binary >>,
                tags_to_list(TX#tx.tags)
            ]);
        false ->
            <<
                (TX#tx.owner)/binary,
                (TX#tx.target)/binary,
                (TX#tx.data)/binary,
                (list_to_binary(integer_to_list(TX#tx.quantity)))/binary,
                (list_to_binary(integer_to_list(TX#tx.reward)))/binary,
                (TX#tx.anchor)/binary,
                (tags_to_binary(TX#tx.tags))/binary
            >>
    end.

tags_to_list(Tags) ->
    [[Name, Value] || {Name, Value} <- Tags].

%% @doc Convert a transactions key-value tags to binary a format.
tags_to_binary(Tags) ->
    list_to_binary(
        lists:foldr(
            fun({Name, Value}, Acc) ->
                [Name, Value | Acc]
            end,
            [],
            Tags
        )
    ).

verify_v1(#tx{ format = 1 }) ->
    [];
verify_v1(_) ->
    [].

verify_v2(#tx{ format = 2 } = TX) ->
    [
        {"tx_data_size_negative", TX#tx.data_size >= 0},
        {"tx_data_size_data_root_mismatch", (TX#tx.data_size == 0) == (TX#tx.data_root == <<>>)},
        {"tx_data_mismatch", verify_v2_data(TX)}
    ];
verify_v2(_) ->
    [].

sign(TX, PrivKey, {KeyType, Owner}, SignatureDataSegment) ->
    NewTX = TX#tx{ 
        owner = Owner, 
        signature_type = KeyType,
        signature = ar_wallet:sign(PrivKey, SignatureDataSegment)
    },
    NewTX#tx{
        id = generate_id(NewTX, signed),
        owner_address = get_owner_address(NewTX) }.

%% @doc Verify that the transaction's ID is a hash of its signature.
verify_hash(#tx{ id = ID } = TX) ->
    ID == generate_id(TX, signed).

%% @doc On Arweave we don't have data on format=2 transactions, and so
%% traditionally just verify the transaction based on data_root and data_size.
%% However in HyperBEAM we will often populate the data field. Adding this
%% check to verify that `data_root`, `data_size`, and `data` are consistent.
verify_v2_data(#tx{ format = 2, data = ?DEFAULT_DATA }) ->
    true;
verify_v2_data(#tx{
        format = 2, data_root = DataRoot,
        data_size = DataSize, data = Data }) ->
    (DataSize == byte_size(Data)) andalso (DataRoot == data_root(Data));
verify_v2_data(_) ->
    true.

collect_validation_results(_TXID, Checks) ->
    KeepFailed = fun
        ({_, true}) -> false;
        ({ErrorCode, false}) -> {true, ErrorCode}
    end,
    case lists:filtermap(KeepFailed, Checks) of
        [] -> true;
        FailedChecks -> 
            ?event({tx_validation_failed, FailedChecks}),
            false
    end.

json_struct_to_tx(TXStruct) ->
    Tags =
        case hb_util:find_value(<<"tags">>, TXStruct) of
            undefined ->
                [];
            Xs ->
                Xs
        end,
    Data = hb_util:decode(hb_util:find_value(<<"data">>, TXStruct)),
    Format =
        case hb_util:find_value(<<"format">>, TXStruct) of
            undefined ->
                1;
            N when is_integer(N) ->
                N;
            N when is_binary(N) ->
                binary_to_integer(N)
        end,
    Denomination =
        case hb_util:find_value(<<"denomination">>, TXStruct) of
            undefined ->
                0;
            EncodedDenomination ->
                MaybeDenomination = binary_to_integer(EncodedDenomination),
                true = MaybeDenomination > 0,
                MaybeDenomination
        end,
    TXID = hb_util:decode(hb_util:find_value(<<"id">>, TXStruct)),
    32 = byte_size(TXID),
    Owner = hb_util:decode(hb_util:find_value(<<"owner">>, TXStruct)),
    Sig = hb_util:decode(hb_util:find_value(<<"signature">>, TXStruct)),
    SigType = set_sig_type_from_pub_key(Owner),
    TX = #tx{
        format = Format,
        id = TXID,
        anchor = hb_util:decode(hb_util:find_value(<<"last_tx">>, TXStruct)),
        owner = Owner,
        tags = [{hb_util:decode(Name), hb_util:decode(Value)}
                %% Only the elements matching this pattern are included in the list.
                || #{<<"name">> := Name, <<"value">> := Value} <- Tags],
        target = hb_util:decode(hb_util:find_value(<<"target">>, TXStruct)),
        quantity = binary_to_integer(hb_util:find_value(<<"quantity">>, TXStruct)),
        data = Data,
        reward = binary_to_integer(hb_util:find_value(<<"reward">>, TXStruct)),
        signature = Sig,
        signature_type = SigType,
        data_size = binary_to_integer(hb_util:find_value(<<"data_size">>, TXStruct)),
        data_root =
            case hb_util:find_value(<<"data_root">>, TXStruct) of
                undefined -> <<>>;
                DR -> hb_util:decode(DR)
            end,
        denomination = Denomination
    },
    %% For ECDSA transactions, recover the owner from the signature
    case SigType of
        ?ECDSA_KEY_TYPE ->
            DataSegment = generate_signature_data_segment(TX),
            Owner2 = ar_wallet:recover_key(DataSegment, Sig, SigType),
            TX#tx{ owner = Owner2, owner_address = ar_wallet:to_address(Owner2, SigType) };
        ?RSA_KEY_TYPE ->
            TX#tx{ owner_address = get_owner_address(TX) }
    end.

%% @doc Determine signature type from owner and signature.
%% For ECDSA transactions, the owner is empty in JSON (recovered from signature).
set_sig_type_from_pub_key(Owner) ->
    case Owner of
        <<>> ->
            %% Empty owner means ECDSA (public key is recovered from signature)
            ?ECDSA_KEY_TYPE;
        _ ->
            ?RSA_KEY_TYPE
    end.

tx_to_json_struct(
    #tx{
        id = ID,
        format = Format,
        anchor = Anchor,
        owner = Owner,
        tags = Tags,
        target = Target,
        quantity = Quantity,
        data = Data,
        reward = Reward,
        signature = Sig,
        data_size = DataSize,
        data_root = DataRoot,
        denomination = Denomination
    }) ->
    Fields = [
        {<<"format">>,
            case Format of
                undefined ->
                    1;
                _ ->
                    Format
            end},
        {<<"id">>, hb_util:encode(ID)},
        {<<"last_tx">>, hb_util:encode(Anchor)},
        {<<"owner">>, hb_util:encode(Owner)},
        {<<"tags">>,
            lists:map(
                fun({Name, Value}) ->
                    #{
                        <<"name">> => hb_util:encode(Name),
                        <<"value">> => hb_util:encode(Value)
                    }
                end,
                Tags
            )
        },
        {<<"target">>, hb_util:encode(Target)},
        {<<"quantity">>, integer_to_binary(Quantity)},
        {<<"data">>, hb_util:encode(Data)},
        {<<"data_size">>, integer_to_binary(DataSize)},
        {<<"data_tree">>, []},
        {<<"data_root">>, hb_util:encode(DataRoot)},
        {<<"reward">>, integer_to_binary(Reward)},
        {<<"signature">>, hb_util:encode(Sig)}
    ],
    Fields2 =
        case Denomination > 0 of
            true ->
                Fields ++ [{<<"denomination">>, integer_to_binary(Denomination)}];
            false ->
                Fields
        end,
    hb_maps:from_list(Fields2).

%% @doc Split the tx data into chunks and compute the Merkle tree from them.
%% Used to compute the Merkle roots of v1 transactions' data and to compute
%% Merkle proofs for v2 transactions when their data is uploaded without proofs.
generate_chunk_tree(TX) ->
    Mode = chunking_mode(TX#tx.format),
    generate_chunk_tree(TX,
        sized_chunks_to_sized_chunk_ids(
            chunks_to_size_tagged_chunks(
                chunk_binary(Mode, ?DATA_CHUNK_SIZE, TX#tx.data)
            )
        )
    ).

generate_chunk_tree(TX, ChunkIDSizes) ->
    {Root, Tree} = ar_merkle:generate_tree(ChunkIDSizes),
    TX#tx{ data_tree = Tree, data_root = Root }.

%% @doc Generate a chunk ID used to construct the Merkle tree from the tx data chunks.
generate_chunk_id(Chunk) ->
    crypto:hash(sha256, Chunk).

%% @doc Split the binary into chunks. Used for computing the Merkle roots of
%% v1 transactions' data and computing Merkle proofs for v2 transactions' when
%% their data is uploaded without proofs.
chunk_binary(ChunkSize, Bin) ->
    chunk_binary(arweavejs, ChunkSize, Bin).

chunking_mode(1) ->
    legacy;
chunking_mode(2) ->
    arweavejs;
chunking_mode(_) ->
    legacy.

%% @doc Split the binary into chunks using the requested mode.
%% legacy: fixed-size chunking with a smaller final chunk.
%% arweavejs: size-balanced chunking where the last two chunks may be small.
%%            This is the chunking logic used by the arweave-js library.
%%            Adapted from: https://github.com/ArweaveTeam/arweave-js/blob/39d8ef2799a2c555e6f9b0cc6adabd7cbc411bc8/src/common/lib/merkle.ts#L43
chunk_binary(legacy, ChunkSize, Bin) when byte_size(Bin) < ChunkSize ->
    [Bin];
chunk_binary(legacy, ChunkSize, Bin) ->
    <<ChunkBin:ChunkSize/binary, Rest/binary>> = Bin,
    [ChunkBin | chunk_binary(legacy, ChunkSize, Rest)];
chunk_binary(arweavejs, ChunkSize, Bin) ->
    chunk_binary_arweavejs(arweavejs, ChunkSize, Bin, []).

chunk_binary_arweavejs(arweavejs, ChunkSize, Bin, Acc)
        when byte_size(Bin) >= ChunkSize ->
    BinSize = byte_size(Bin),
    NextChunkSize = BinSize - ChunkSize,
    ChunkSize2 =
        case NextChunkSize > 0 andalso NextChunkSize < ?MIN_CHUNK_SIZE of
            true -> 
                (BinSize + 1) div 2;
            false -> ChunkSize
        end,
    <<Chunk:ChunkSize2/binary, Rest/binary>> = Bin,
    chunk_binary_arweavejs(arweavejs, ChunkSize, Rest, [Chunk | Acc]);
chunk_binary_arweavejs(arweavejs, _ChunkSize, Bin, Acc) ->
    lists:reverse([Bin | Acc]).

%% @doc Assign a byte offset to every chunk in the list.
chunks_to_size_tagged_chunks(Chunks) ->
    lists:reverse(
        element(
            2,
            lists:foldl(
                fun(Chunk, {Pos, List}) ->
                    End = Pos + byte_size(Chunk),
                    {End, [{Chunk, End} | List]}
                end,
                {0, []},
                Chunks
            )
        )
    ).

%% @doc Convert a list of chunk, byte offset tuples to
%% the list of chunk ID, byte offset tuples.
sized_chunks_to_sized_chunk_ids(SizedChunks) ->
    [{generate_chunk_id(Chunk), Size} || {Chunk, Size} <- SizedChunks].

%% @doc Return the number of bytes the weave is increased by when the given transaction
%% is included.
get_weave_size_increase(#tx{ data_size = DataSize }, Height) ->
	get_weave_size_increase(DataSize, Height);

get_weave_size_increase(0, _Height) ->
	0;
get_weave_size_increase(DataSize, Height) ->
	case Height >= ar_fork:height_2_5() of
		true ->
			%% The smallest multiple of ?DATA_CHUNK_SIZE larger than or equal to data_size.
			ar_poa:get_padded_offset(DataSize, 0);
		false ->
			DataSize
	end.

%%%===================================================================
%%% Tests.
%%%===================================================================

%% @doc A helper for preparing transactions for signing. Used in tests.
%% Should be moved to a testing module.
new(Data, Reward) ->
    #tx{
        format = 1,
        id = crypto:strong_rand_bytes(32),
        data = Data,
        reward = Reward,
        data_size = byte_size(Data)
    }.

chunk_binary_legacy_test() ->
    ChunkSize = 10,
    Data = binary:copy(<<"a">>, 25),
    ChunksLegacy = chunk_binary(legacy, ChunkSize, Data),
    ?assertEqual([10, 10, 5], [byte_size(Chunk) || Chunk <- ChunksLegacy]),
    ?assertEqual(ChunksLegacy, chunk_binary(legacy, ChunkSize, Data)).

chunk_binary_arweavejs_balanced_test() ->
    ChunkSize = ?DATA_CHUNK_SIZE,
    MinChunkSize = ?MIN_CHUNK_SIZE,
    DataSize = ChunkSize + MinChunkSize - 1,
    Data = binary:copy(<<"b">>, DataSize),
    Chunks = chunk_binary(arweavejs, ChunkSize, Data),
    ExpectedFirst = (DataSize + 1) div 2,
    ExpectedSecond = DataSize - ExpectedFirst,
    ?assertEqual([ExpectedFirst, ExpectedSecond], [byte_size(Chunk) || Chunk <- Chunks]).

chunk_binary_arweavejs_standard_test() ->
    ChunkSize = ?DATA_CHUNK_SIZE,
    MinChunkSize = ?MIN_CHUNK_SIZE,
    DataSize = ChunkSize + MinChunkSize,
    Data = binary:copy(<<"c">>, DataSize),
    Chunks = chunk_binary(arweavejs, ChunkSize, Data),
    ?assertEqual([ChunkSize, MinChunkSize], [byte_size(Chunk) || Chunk <- Chunks]).

sign_tx_test_() ->
    {timeout, 30, fun test_sign_tx/0}.

sign_tx_v1(TX, {Priv, Pub}) ->
    sign_v1(generate_chunk_tree(TX), Priv, Pub).

sign_tx_v2(TX, {Priv, Pub}) ->
    sign(generate_chunk_tree(TX), Priv, Pub).

test_sign_tx() ->
    NewTX = new(<<"TEST DATA">>, ?AR(1)),
    {Priv, Pub} = Wallet = ar_wallet:new(),

    ValidTXs = [
        sign_v1(NewTX, Priv, Pub),
        sign(generate_chunk_tree(NewTX#tx{ format = 2 }), Priv, Pub)
    ],
    lists:foreach(
        fun(TX) ->
            ?assert(verify(TX), TX#tx.format)
        end,
        ValidTXs
    ),
    InvalidTXs = [
        { "tx_format_not_supported",
            (sign_tx_v1(NewTX, Wallet))#tx{ format = 3 } },
        { "tx_format_not_supported",
            (sign_tx_v2(NewTX#tx{ format = 2 }, Wallet))#tx{ format = 3 } },
        { "quantity_negative",
            sign_tx_v1(NewTX#tx{ quantity = -1 }, Wallet) },
        { "quantity_negative",
            sign_tx_v2(NewTX#tx{ quantity = -1, format = 2 }, Wallet) },
        { "same_owner_as_target",
            sign_tx_v1(NewTX#tx{ target = ar_wallet:to_address(Wallet) }, Wallet) },
        { "same_owner_as_target",
            sign_tx_v2(NewTX#tx{ target = ar_wallet:to_address(Wallet), format = 2 }, Wallet) },
        { "tx_id_not_valid",
            (sign_tx_v1(NewTX, Wallet))#tx{ id = crypto:strong_rand_bytes(32) } },
        { "tx_id_not_valid",
            (sign_tx_v2(NewTX#tx{ format = 2 }, Wallet))#tx{ id = crypto:strong_rand_bytes(32) } },
        { "tx_signature_not_valid",
            (sign_tx_v1(NewTX, Wallet))#tx{ signature = crypto:strong_rand_bytes(64) } },
        { "tx_signature_not_valid",
            (sign_tx_v2(NewTX#tx{ format = 2 }, Wallet))#tx{ signature = crypto:strong_rand_bytes(64) } },
        { "tx_data_size_negative",
            sign_tx_v2(NewTX#tx{ data_size = -1, format = 2 }, Wallet) },
        { "tx_data_size_data_root_mismatch", 
            sign((generate_chunk_tree(NewTX#tx{ format = 2 }))#tx{ data_root = <<>> }, Priv, Pub) }
    ],
    lists:foreach(
        fun({Message, TX}) ->
            ?assert(not verify(TX), 
                lists:flatten(
                    io_lib:format("Format ~p: ~s", [TX#tx.format, Message])
                )
            )
        end,
        InvalidTXs
    ).

sign_and_verify_chunked_test_() ->
    {timeout, 60, fun test_sign_and_verify_chunked/0}.

test_sign_and_verify_chunked() ->
    TXData = crypto:strong_rand_bytes(trunc(?DATA_CHUNK_SIZE * 5.5)),
    {Priv, Pub} = ar_wallet:new(),
    UnsignedTX =
        generate_chunk_tree(
            #tx{
                format = 2,
                data = TXData,
                data_size = byte_size(TXData),
                reward = ?AR(100)
            }
        ),
    SignedTX = sign(UnsignedTX#tx{ data = <<>> }, Priv, Pub),
    ?assert(verify(SignedTX)).

%% Ensure that a forged transaction does not pass verification.

forge_test_() ->
    {timeout, 30, fun test_forge/0}.

test_forge() ->
    NewTX = new(<<"TEST DATA">>, ?AR(10)),
    {Priv, Pub} = ar_wallet:new(),
    InvalidSignTX = (sign_v1(NewTX, Priv, Pub))#tx{
        data = <<"FAKE DATA">>
    },
    ?assert(not verify(InvalidSignTX)).

generate_and_validate_even_chunk_tree_test() ->
    Data = crypto:strong_rand_bytes(?DATA_CHUNK_SIZE * 7),
    lists:map(
        fun(ChallengeLocation) ->
            test_generate_chunk_tree_and_validate_path(Data, ChallengeLocation)
        end,
        [
            0, 1, 10, ?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE + 1, 2 * ?DATA_CHUNK_SIZE - 1,
            7 * ?DATA_CHUNK_SIZE - 1
        ]
    ).

generate_and_validate_uneven_chunk_tree_test() ->
    Data = crypto:strong_rand_bytes(?DATA_CHUNK_SIZE * 4 + 10),
    lists:map(
        fun(ChallengeLocation) ->
            test_generate_chunk_tree_and_validate_path(Data, ChallengeLocation)
        end,
        [
            0, 1, 10, ?DATA_CHUNK_SIZE, ?DATA_CHUNK_SIZE + 1, 2 * ?DATA_CHUNK_SIZE - 1,
            4 * ?DATA_CHUNK_SIZE + 9
        ]
    ).

test_generate_chunk_tree_and_validate_path(Data, ChallengeLocation) ->
    ChunkStart = hb_util:floor_int(ChallengeLocation, ?DATA_CHUNK_SIZE),
    Chunk = binary:part(Data, ChunkStart, min(?DATA_CHUNK_SIZE, byte_size(Data) - ChunkStart)),
    #tx{ data_root = DataRoot, data_tree = DataTree } =
        generate_chunk_tree(
            #tx{
                data = Data,
                data_size = byte_size(Data)
            }
        ),
    DataPath =
        ar_merkle:generate_path(
            DataRoot,
            ChallengeLocation,
            DataTree
        ),
    RealChunkID = generate_chunk_id(Chunk),
    {PathChunkID, StartOffset, EndOffset} =
        ar_merkle:validate_path(DataRoot, ChallengeLocation, byte_size(Data), DataPath),
    {PathChunkID, StartOffset, EndOffset} =
        ar_merkle:validate_path(DataRoot, ChallengeLocation, byte_size(Data),
                DataPath, strict_data_split_ruleset),
    {PathChunkID, StartOffset, EndOffset} =
        ar_merkle:validate_path(DataRoot, ChallengeLocation, byte_size(Data),
                DataPath, strict_borders_ruleset),
    ?assertEqual(RealChunkID, PathChunkID),
    ?assert(ChallengeLocation >= StartOffset),
    ?assert(ChallengeLocation < EndOffset).


%%===================================================================
%% json_struct_to_tx tests
%%===================================================================

json_struct_to_tx_happy_test() ->
    ID32        = crypto:strong_rand_bytes(32),
    Owner32     = crypto:strong_rand_bytes(32),
    LastTx32    = crypto:strong_rand_bytes(32),
    SigBin      = <<>>,
    DataRoot32  = crypto:strong_rand_bytes(32),
    Sig64       = crypto:strong_rand_bytes(64),
    BaseStruct0 = #{
        <<"id">>          => hb_util:encode(ID32),
        <<"last_tx">>     => hb_util:encode(LastTx32),
        <<"owner">>       => hb_util:encode(Owner32),
        <<"signature">>   => hb_util:encode(SigBin),
        <<"quantity">>    => <<"100">>,
        <<"reward">>      => <<"10">>,
        <<"data_size">>   => <<"0">>,
        <<"data">>        => hb_util:encode(<<>>),
        <<"target">>      => <<>>,
        <<"tags">>        => [],
        <<"data_root">>   => hb_util:encode(DataRoot32)
    },

    TagFun = fun(Key, Val) ->
        #{ 
            <<"name">> =>  hb_util:encode(Key),
            <<"value">> => hb_util:encode(Val)
        }
    end,

    TargetBin = crypto:strong_rand_bytes(32),
    TargetB64 = hb_util:encode(TargetBin),

    Variants = [
        %% baseline – ensure empty tag list maintained
        {BaseStruct0, fun(TX) ->
            ?assertEqual(1, TX#tx.format),
            ?assertEqual([], TX#tx.tags)
        end},
        {BaseStruct0#{ <<"format">> => 2 }, fun(TX) -> ?assertEqual(2, TX#tx.format) end},
        {BaseStruct0#{ <<"format">> => <<"2">> }, fun(TX) -> ?assertEqual(2, TX#tx.format) end},
        {BaseStruct0#{ <<"quantity">> => <<"1234">> }, fun(TX) -> ?assertEqual(1234, TX#tx.quantity) end},
        {BaseStruct0#{ <<"reward">> => <<"567">> }, fun(TX) -> ?assertEqual(567, TX#tx.reward) end},
        {BaseStruct0#{ <<"tags">> => [TagFun(<<"tagname">>, <<"tagval">>)] },
            fun(TX) -> ?assertEqual([{<<"tagname">>, <<"tagval">>}], TX#tx.tags) end},
        {BaseStruct0#{ <<"denomination">> => <<"7">> }, fun(TX) -> ?assertEqual(7, TX#tx.denomination) end},
        {BaseStruct0#{ <<"target">> => TargetB64 },
            fun(TX) -> ?assertEqual(TargetBin, TX#tx.target)end},
        {BaseStruct0#{ <<"data_root">> => hb_util:encode(DataRoot32) },
            fun(TX) -> ?assertEqual(DataRoot32, TX#tx.data_root) end},
        {BaseStruct0#{ <<"data_size">> => <<"250">> },
            fun(TX) -> ?assertEqual(250, TX#tx.data_size) end},
        {BaseStruct0#{ <<"signature">> => hb_util:encode(Sig64) },
            fun(TX) -> ?assertEqual(Sig64, TX#tx.signature) end}
    ],

    lists:foreach(
        fun({Struct, AssertFun}) ->
            Parsed = json_struct_to_tx(Struct),
            %% constant fields always true across variants
            ?assert(is_record(Parsed, tx)),
            ?assertEqual(ID32, Parsed#tx.id),
            ?assertEqual(Owner32, Parsed#tx.owner),
            ?assertEqual(ar_wallet:to_address(Owner32), Parsed#tx.owner_address),
            ?assertEqual(LastTx32, Parsed#tx.anchor),
            ?assertEqual(?RSA_KEY_TYPE, Parsed#tx.signature_type),
            %% run variant-specific assertion
            AssertFun(Parsed)
        end,
        Variants
    ).

json_struct_to_tx_failure_test() ->
    ID32        = crypto:strong_rand_bytes(32),
    Owner32     = crypto:strong_rand_bytes(32),
    LastTx32    = crypto:strong_rand_bytes(32),

    BaseStruct = #{
        <<"id">>         => hb_util:encode(ID32),
        <<"last_tx">>     => hb_util:encode(LastTx32),
        <<"owner">>       => hb_util:encode(Owner32),
        <<"signature">>   => <<>>,
        <<"quantity">>    => <<"0">>,
        <<"reward">>      => <<"0">>,
        <<"data_size">>   => <<"0">>,
        <<"data">>       => <<>>,
        <<"target">>     => <<>>,
        <<"tags">>       => [],
        <<"data_root">>  => <<>>
    },

    BadIDBin  = crypto:strong_rand_bytes(31),
    InvalidB64 = <<"!!not_base64!!">>,

    %% invalid tag variants
    BadTagName   = [ #{ <<"name">> => InvalidB64, <<"value">> => hb_util:encode(<<"val">>)} ],
    BadTagValue  = [ #{ <<"name">> => hb_util:encode(<<"key">>), <<"value">> => InvalidB64} ],

    FailureCases = [
        {"invalid_format", BaseStruct#{ <<"format">> => <<"abc">> }, badarg},
        {"denomination_zero", BaseStruct#{ <<"denomination">> => <<"0">> }, {badmatch, false}},
        {"id_wrong_size", BaseStruct#{ <<"id">> => hb_util:encode(BadIDBin) }, {badmatch, 31}},
        {"quantity_nonnumeric", BaseStruct#{ <<"quantity">> => <<"notanumber">> }, badarg},
        {"reward_nonnumeric", BaseStruct#{ <<"reward">> => <<"xyz">> }, badarg},
        {"data_size_nonnumeric", BaseStruct#{ <<"data_size">> => <<"abc">> }, badarg},
        {"quantity_missing", maps:remove(<<"quantity">>, BaseStruct), badarg},
        {"id_invalid_b64", BaseStruct#{ <<"id">> => InvalidB64 }, badarg},
        {"owner_invalid_b64", BaseStruct#{ <<"owner">> => InvalidB64 }, badarg},
        {"last_tx_invalid_b64", BaseStruct#{ <<"last_tx">> => InvalidB64 }, badarg},
        {"signature_invalid_b64", BaseStruct#{ <<"signature">> => InvalidB64 }, badarg},
        {"data_invalid_b64", BaseStruct#{ <<"data">> => InvalidB64 }, badarg},
        {"data_root_invalid_b64", BaseStruct#{ <<"data_root">> => InvalidB64 }, badarg},
        {"tag_name_invalid_b64", BaseStruct#{ <<"tags">> => BadTagName }, badarg},
        {"tag_value_invalid_b64", BaseStruct#{ <<"tags">> => BadTagValue }, badarg},
        {"target_invalid_b64", BaseStruct#{ <<"target">> => InvalidB64 }, badarg}
        ],

    lists:foreach(
        fun({Label, Struct, Reason}) ->
            Error = try 
                json_struct_to_tx(Struct),
                {failed_to_throw, Label}
            catch
                error:Reason -> ok;
                error:Other -> {wrong_reason, Label, Other}
            end,
            ?assertEqual(ok, Error)
        end,
        FailureCases
    ).

%%===================================================================
%% tx_to_json_struct tests
%%===================================================================
tx_to_json_struct_happy_test() ->
    Owner = crypto:strong_rand_bytes(32),

    BaseTX = #tx{
        id             = crypto:strong_rand_bytes(32),
        anchor         = crypto:strong_rand_bytes(32),
        owner          = Owner,
        signature_type = ?RSA_KEY_TYPE, 
        owner_address  = ar_wallet:to_address(Owner, ?RSA_KEY_TYPE), %% Not in JSON
        tags           = [],
        target         = <<>>,
        quantity       = 0,
        data           = <<>>,
        data_size      = 0,
        data_root      = <<>>,
        reward         = 0,
        signature      = crypto:strong_rand_bytes(512), %% RSA signature size
        format         = 1,
        denomination   = 0
    },

    %% Helper to create the expected tag structure for JSON
    JsonTagFun = fun({Name, Value}) ->
        #{ 
            <<"name">> =>  hb_util:encode(Name),
            <<"value">> => hb_util:encode(Value)
        }
    end,

    SpecificTarget = crypto:strong_rand_bytes(32),
    SpecificDataRoot = crypto:strong_rand_bytes(32),
    SpecificData = crypto:strong_rand_bytes(64),

    Variants = [
        %% {Label, ModifiedTX, ExpectedCheckerFun}
        {"baseline", BaseTX},
        {"format_2", BaseTX#tx{format = 2}},
        {"format_undefined", BaseTX#tx{format = undefined}},
        {"positive_denomination", BaseTX#tx{denomination = 5}},
        {"with_tags", BaseTX#tx{tags = [{<<"tag1">>, <<"val1">>}, {<<"TagTwo">>, <<"ValueTwo">>}]}},
        {"with_target", BaseTX#tx{target = SpecificTarget}},
        {"with_data_root", BaseTX#tx{data_root = SpecificDataRoot}},
        {"with_data", BaseTX#tx{data = SpecificData, data_size = byte_size(SpecificData)}},
        {"with_quantity_and_reward", BaseTX#tx{quantity = 12345, reward = 6789}}
    ],

    lists:foreach(
        fun({Label, TXVariant}) ->
            JsonStruct = tx_to_json_struct(TXVariant),
            ?assert(is_map(JsonStruct)),
            %% Assert common fields
            ExpectedFormat = case TXVariant#tx.format of
                undefined -> 1;
                _ -> TXVariant#tx.format
            end,
            ?assertEqual(ExpectedFormat, maps:get(<<"format">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.id), maps:get(<<"id">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.anchor), maps:get(<<"last_tx">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.owner), maps:get(<<"owner">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.target), maps:get(<<"target">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.signature), maps:get(<<"signature">>, JsonStruct), Label),
            ?assertEqual(integer_to_binary(TXVariant#tx.quantity), maps:get(<<"quantity">>, JsonStruct), Label),
            ?assertEqual(integer_to_binary(TXVariant#tx.reward), maps:get(<<"reward">>, JsonStruct), Label),
            ?assertEqual(integer_to_binary(TXVariant#tx.data_size), maps:get(<<"data_size">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.data), maps:get(<<"data">>, JsonStruct), Label),
            ?assertEqual(hb_util:encode(TXVariant#tx.data_root), maps:get(<<"data_root">>, JsonStruct), Label),
            ?assertEqual(integer_to_binary(TXVariant#tx.denomination),
                maps:get(<<"denomination">>, JsonStruct, integer_to_binary(0)), Label),
            ?assertEqual([], maps:get(<<"data_tree">>, JsonStruct), Label), % Always empty list

            ExpectedJsonTags = lists:map(JsonTagFun, TXVariant#tx.tags),
            ?assertEqual(ExpectedJsonTags, maps:get(<<"tags">>, JsonStruct), 
                {tags_mismatch, Label}),

            %% Assert no extra keys are present
            BaseExpectedKeys = [
                <<"format">>, <<"id">>, <<"last_tx">>, <<"owner">>, <<"tags">>,
                 <<"target">>, <<"quantity">>, <<"data">>, <<"data_size">>,
                 <<"data_tree">>, <<"data_root">>, <<"reward">>,
                 <<"signature">>
            ],
            ExpectedKeys =
                case TXVariant#tx.denomination > 0 of
                    true  -> lists:sort([<<"denomination">> | BaseExpectedKeys]);
                    false -> lists:sort(BaseExpectedKeys)
                end,
            ActualKeys = lists:sort(maps:keys(JsonStruct)),
            ?assertEqual(ExpectedKeys, ActualKeys, 
                {extra_or_missing_keys, Label})
        end,
        Variants
    ).

tx_to_json_struct_failure_test() ->
    RandBin32 = crypto:strong_rand_bytes(32),
    BaseTX = #tx{
        id = RandBin32, anchor = RandBin32, owner = RandBin32,
        signature_type = ?RSA_KEY_TYPE, target = <<>>, quantity = 0,
        data = <<>>, data_size = 0, data_root = <<>>, reward = 0,
        signature = crypto:strong_rand_bytes(512), format = 1, tags = [],
        denomination = 0
    },

    FailureCases = [
        {"id_not_binary",        BaseTX#tx{id = not_a_binary}, badarg},
        {"anchor_not_binary",    BaseTX#tx{anchor = 123}, badarg},
        {"owner_not_binary",     BaseTX#tx{owner = an_atom}, badarg},
        {"target_not_binary",    BaseTX#tx{target = {oops}}, badarg},
        {"data_not_binary",      BaseTX#tx{data = 99.9}, badarg},
        {"data_root_not_binary", BaseTX#tx{data_root = not_a_binary}, badarg},
        {"signature_not_binary", BaseTX#tx{signature = make_ref()}, badarg},
        {"quantity_not_integer", BaseTX#tx{quantity = <<"100">>}, badarg},
        {"reward_not_integer",   BaseTX#tx{reward = 1.0}, badarg},
        {"data_size_not_integer",BaseTX#tx{data_size = an_atom}, badarg},
        {"denomination_not_integer_when_positive", BaseTX#tx{denomination = <<"5">>}, badarg},
        {"tag_name_not_binary",  BaseTX#tx{tags = [{not_binary, <<"val">>}]}, badarg},
        {"tag_value_not_binary", BaseTX#tx{tags = [{<<"key">>, not_binary}]}, badarg},
        {"tags_not_list", BaseTX#tx{tags = #{}}, {case_clause, #{}}}
    ],

    lists:foreach(
        fun({Label, TX, Reason}) ->
            Error = try
                tx_to_json_struct(TX),
                {failed_to_throw, Label}
            catch
                error:Reason -> ok;
                error:Other -> {wrong_reason, Label, Other}
            end,
            ?assertEqual(ok, Error)
        end,
        FailureCases
    ).
