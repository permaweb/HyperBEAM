%%% @doc Utilities for HyperBEAM-specific handling of transactions. Handles both ANS104
%%% and Arewave L1 transactions.
-module(hb_tx).
-export([signer/1, is_signed/1]).
-export([tx_to_tabm/5, tabm_to_tx/3, binary_to_tx/1, encoded_tags_to_map/1]).
-export([id/1, id/2, reset_ids/1, update_ids/1]).
-export([normalize/1, normalize_data/1]).
-export([commit_message/4, verify_message/4, verify/1]).
-export([print/1, format/1, format/2]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).

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
    ?event(xxx, {committing, {input, Msg}}),
    TX = hb_util:ok(codec_to_tx(Codec, hb_private:reset(Msg), Req, Opts)),
    ?event(xxx, {commit_message, {tx, {explicit, TX}}}),
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    Signed = sign(TX, Wallet),
    ?event(xxx, {commit_message, {signed, {explicit, Signed}}}),
    SignedStructured =
        hb_message:convert(
            Signed,
            <<"structured@1.0">>,
            Codec,
            Opts
        ),
    ?event(xxx, {commit_message, {signed_structured, {explicit, SignedStructured}}}),
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
    ?event({verify, {encoded, {explicit, TX}}}),
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

tx_to_tabm(RawTX, TXKeys, CommittedTags, Req, Opts) ->
    case lists:keyfind(<<"ao-type">>, 1, RawTX#tx.tags) of
        false ->
            tx_to_tabm2(RawTX, TXKeys, CommittedTags, Req, Opts);
        {<<"ao-type">>, <<"binary">>} ->
            RawTX#tx.data
    end.

tx_to_tabm2(RawTX, TXKeys, CommittedTags, Req, Opts) ->
    Device = case RawTX#tx.format of
        ans104 -> <<"ans104@1.0">>;
        _ -> <<"tx@1.0">>
    end,
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(normalize(RawTX)),
    OriginalTagMap = encoded_tags_to_map(TX#tx.tags),
    % Get the raw fields and values of the tx record and pair them. Then convert 
    % the list of key-value pairs into a map, removing irrelevant fields.
    RawTXKeysMap =
        hb_maps:with(TXKeys,
            hb_ao:normalize_keys(
                hb_maps:from_list(
                    lists:zip(
                        record_info(fields, tx),
                        tl(tuple_to_list(TX))
                    )
                ),
				Opts
            ),
			Opts
        ),
    % Normalize `owner' to `keyid', remove 'id', and remove 'signature'
    TXKeysMap =
        maps:without(
            [<<"owner">>, <<"signature">>],
            case maps:get(<<"owner">>, RawTXKeysMap, ?DEFAULT_OWNER) of
                ?DEFAULT_OWNER -> RawTXKeysMap;
                Owner -> RawTXKeysMap#{ <<"keyid">> => Owner }
            end
        ),
    % Generate a TABM from the tags.
    MapWithoutData =
        hb_maps:merge(
            TXKeysMap,
            deduplicating_from_list(TX#tx.tags, Opts),
			Opts
        ),
    ?event({tags_from_tx, {explicit, MapWithoutData}}),
    DataMap =
        case TX#tx.data of
            Data when is_map(Data) ->
                % If the data is a map, we need to recursively turn its children
                % into messages from their tx representations.
                hb_maps:merge(
                    MapWithoutData,
                    hb_maps:map(
                        fun(_, InnerValue) ->
                            hb_util:ok(dev_codec_ans104:from(InnerValue, Req, Opts))
                        end,
                        Data,
                        Opts
                    ),
					Opts
                );
            Data when Data == ?DEFAULT_DATA -> MapWithoutData;
            Data when is_binary(Data) -> MapWithoutData#{ <<"data">> => Data };
            Data ->
                ?event({unexpected_data_type, {explicit, Data}}),
                ?event({was_processing, {explicit, TX}}),
                throw(invalid_tx)
        end,
    % Merge the data map with the rest of the TX map and remove any keys that
    % are not part of the message.
    NormalizedDataMap =
        hb_ao:normalize_keys(hb_maps:merge(DataMap, MapWithoutData, Opts), Opts),
    %% Add the commitments to the message if the TX has a signature.
    ?event({message_before_commitments, NormalizedDataMap}),
    WithCommitments =
        case TX#tx.signature of
            ?DEFAULT_SIG ->
                ?event({no_signature_detected, NormalizedDataMap}),
                case normal_tags(TX#tx.tags) of
                    true -> NormalizedDataMap;
                    false ->
                        ID = hb_util:human_id(TX#tx.unsigned_id),
                        NormalizedDataMap#{
                            <<"commitments">> => #{
                                ID => #{
                                    <<"commitment-device">> => Device,
                                    <<"type">> => <<"unsigned-sha256">>,
                                    <<"original-tags">> => OriginalTagMap
                                }
                            }
                        }
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
                        NormalizedDataMap,
						Opts
                    ),
                ID = hb_util:human_id(TX#tx.id),
                ?event({raw_tx_id, {id, ID}, {explicit, WithoutBaseCommitment}}),
                Commitment = #{
                    <<"commitment-device">> => Device,
                    <<"committer">> => Address,
                    <<"committed">> =>
                        hb_util:unique(
                            CommittedTags
                                ++ [ hb_ao:normalize_key(Tag) || {Tag, _} <- TX#tx.tags ]
                        ),
                    <<"keyid">> => hb_util:encode(TX#tx.owner),
                    <<"signature">> => hb_util:encode(TX#tx.signature),
                    <<"type">> => <<"rsa-pss-sha256">>
                },
                WithoutBaseCommitment#{
                    <<"commitments">> => #{
                        ID =>
                            case normal_tags(TX#tx.tags) of
                                true -> Commitment;
                                false -> Commitment#{
                                    <<"original-tags">> => OriginalTagMap
                                }
                            end
                    }
                }
        end,
    Result = hb_maps:without(?FILTERED_TAGS, WithCommitments, Opts),
    ?event({from_result, {explicit, Result}}),
    Result.
      
tabm_to_tx(NormTABM, Req, Opts) ->
    % Ensure that the TABM is fully loaded, for now.
    ?event({to, {norm, NormTABM}}),
    TABM =
        hb_ao:normalize_keys(
            hb_maps:without([<<"commitments">>], NormTABM, Opts),
            Opts
        ),
    Commitments = hb_maps:get(<<"commitments">>, NormTABM, #{}, Opts),
    TABMWithComm =
        case hb_maps:keys(Commitments, Opts) of
            [] -> TABM;
            [ID] ->
                Commitment = hb_maps:get(ID, Commitments),
                TABMWithoutCommitmentKeys =
                    TABM#{
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
                WithOrigKeys =
                    case maps:get(<<"original-tags">>, Commitment, undefined) of
                        undefined -> TABMWithoutCommitmentKeys;
                        OrigKeys ->
                            TABMWithoutCommitmentKeys#{
                                <<"original-tags">> => OrigKeys
                            }
                    end,
                ?event({flattened_tabm, WithOrigKeys}),
                WithOrigKeys;
            _ -> throw({multisignatures_not_supported_by_ans104, NormTABM})
        end,
    OriginalTagMap = hb_maps:get(<<"original-tags">>, TABMWithComm, #{}, Opts),
    OriginalTags = tag_map_to_encoded_tags(OriginalTagMap),
    TABMNoOrigTags = hb_maps:without([<<"original-tags">>], TABMWithComm, Opts),
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages. Notably, we do not simply
    % call message_to_tx/1 on the inner map because that would lead to adding
    % an extra layer of nesting to the data.
    MsgKeyMap =
        hb_maps:map(
            fun(_Key, Msg) when is_map(Msg) -> hb_util:ok(dev_codec_ans104:to(Msg, Req, Opts));
            (_Key, Value) -> Value
            end,
            TABMNoOrigTags,
            Opts
        ),
    NormalizedMsgKeyMap = hb_ao:normalize_keys(MsgKeyMap, Opts),
    % Iterate through the default fields, replacing them with the values from
    % the message map if they are present.
    {RemainingMap, BaseTXList} =
        lists:foldl(
            fun({Field, Default}, {RemMap, Acc}) ->
                NormKey = hb_ao:normalize_key(Field),
                case maps:find(NormKey, NormalizedMsgKeyMap) of
                    error -> {RemMap, [Default | Acc]};
                    {ok, Value} when NormKey =:= <<"format">> ->
                        {
                            maps:remove(NormKey, RemMap),
                            [coerce_format(Value) | Acc]
                        };
                    {ok, Value} when is_integer(Default) ->
                        {
                            maps:remove(NormKey, RemMap),
                            [hb_util:int(Value) | Acc]
                        };
                    {ok, Value} when is_binary(Default) andalso ?IS_ID(Value) ->
                        % NOTE: Do we really want to do this type coercion?
                        {
                            maps:remove(NormKey, RemMap),
                            [
                                try hb_util:native_id(Value) catch _:_ -> Value end
                            |
                                Acc
                            ]
                        };
                    {ok, Value} ->
                        {
                            maps:remove(NormKey, RemMap),
                            [Value|Acc]
                        }
                end
            end,
            {NormalizedMsgKeyMap, []},
            hb_message:default_tx_list()
        ),
    % Rebuild the tx record from the new list of fields and values.
    TXWithoutTags = list_to_tuple([tx | lists:reverse(BaseTXList)]),
    % Calculate which set of the remaining keys will be used as tags.
    % Remaining: items where the value is less than or equal to ?MAX_TAG_VAL bytes will
    %            stay as tags.
    % RawDataItems: items where the value is greater than ?MAX_TAG_VAL bytes will get
    %               converted to data items.
    {Remaining, RawDataItems} =
        lists:partition(
            fun({_Key, Value}) when is_binary(Value) ->
                    case unicode:characters_to_binary(Value) of
                        {error, _, _} -> false;
                        _ -> byte_size(Value) =< ?MAX_TAG_VAL
                    end;
                (_) -> false
            end,
            hb_maps:to_list(RemainingMap, Opts)
        ),
    ?event({remaining_keys_to_convert_to_tags, {explicit, Remaining}}),
    ?event({original_tags, {explicit, OriginalTags}}),
    % Check that the remaining keys are as we expect them to be, given the 
    % original tags. We do this by re-calculating the expected tags from the
    % original tags and comparing the result to the remaining keys.
    if length(OriginalTags) > 0 ->
        ExpectedTagsFromOriginal = deduplicating_from_list(OriginalTags, Opts),
        NormRemaining = maps:from_list(Remaining),
        case NormRemaining == ExpectedTagsFromOriginal of
            true -> ok;
            false ->
                ?event(warning,
                    {invalid_original_tags,
                        {expected, ExpectedTagsFromOriginal},
                        {given, NormRemaining}
                    }
                ),
                throw({invalid_original_tags, OriginalTags, NormRemaining})
        end;
    true -> ok
    end,
    % Restore the original tags, or the remaining keys if there are no original
    % tags.
    TX =
        TXWithoutTags#tx {
            tags =
                case OriginalTags of
                    [] -> Remaining;
                    _ -> OriginalTags
                end
        },
    ?event({tx_before_data, TX}),
    % Recursively turn the remaining data items into tx records.
    DataItems = hb_maps:from_list(lists:map(
        fun({Key, Value}) ->
            ?event({data_item, {key, Key}, {value, Value}}),
            {hb_ao:normalize_key(Key), hb_util:ok(dev_codec_ans104:to(Value, Req, Opts))}
        end,
        RawDataItems
    )),
    % Set the data based on the remaining keys.
    TXWithData = 
        case {TX#tx.data, hb_maps:size(DataItems, Opts)} of
            {Binary, 0} when is_binary(Binary) ->
                TX;
            {?DEFAULT_DATA, _} ->
                TX#tx { data = DataItems };
            {Data, _} when is_map(Data) ->
                TX#tx { data = hb_maps:merge(Data, DataItems, Opts) };
            {Data, _} when is_record(Data, tx) ->
                TX#tx { data = DataItems#{ <<"data">> => Data } };
            {Data, _} when is_binary(Data) ->
                TX#tx {
                    data =
                        DataItems#{
                            <<"data">> => hb_util:ok(dev_codec_ans104:to(Data, Req, Opts))
                        }
                }
        end,
    ?event({tx_with_data, {explicit, TXWithData}}),
    % Regenerate the IDs
    TXWithIDs =
        try reset_ids(normalize(TXWithData))
        catch
            _:Error ->
                ?event({prepared_tx_before_ids,
                    {tags, {explicit, TXWithData#tx.tags}},
                    {data, TXWithData#tx.data}
                }),
                throw(Error)
        end,
    ?event({to_result, {explicit, TXWithIDs}}),
    TXWithIDs.

binary_to_tx(Binary) ->
    % ans104 and Arweave cannot serialize just a simple binary or get an ID for it, so
    % we turn it into a TX record with a special tag, tx_to_message will
    % identify this tag and extract just the binary.
    #tx{
        tags = [{<<"ao-type">>, <<"binary">>}],
        data = Binary
    }.

%% @doc Convert an ANS-104/Arweave encoded tag list into a HyperBEAM-compatible map.
encoded_tags_to_map(Tags) ->
    hb_util:list_to_numbered_map(
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

%% @doc Convert a HyperBEAM-compatible map into an ANS-104/Arweave encoded tag list,
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

%%%===================================================================
%%% Private functions.
%%%===================================================================

%% @doc Generate the ID for a given transaction.
generate_id(#tx{ format = ans104 } = TX, signed) ->
    ar_bundles:generate_id(TX, signed);
generate_id(TX, signed) ->
    ar_tx:generate_id(TX, signed);
generate_id(#tx{ format = ans104 } = TX, unsigned) ->
    ar_bundles:generate_id(TX, unsigned);
generate_id(TX, unsigned) ->
    ar_tx:generate_id(TX, unsigned).

%% @doc Check whether a list of key-value pairs contains only normalized keys.
normal_tags(Tags) ->
    lists:all(
        fun({Key, _}) ->
            hb_ao:normalize_key(Key) =:= Key
        end,
        Tags
    ).

%% @doc Deduplicate a list of key-value pairs by key, generating a list of
%% values for each normalized key if there are duplicates.
deduplicating_from_list(Tags, Opts) ->
    % Aggregate any duplicated tags into an ordered list of values.
    Aggregated =
        lists:foldl(
            fun({Key, Value}, Acc) ->
                NormKey = hb_ao:normalize_key(Key),
                ?event({deduplicating_from_list, {key, NormKey}, {value, Value}, {acc, Acc}}),
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
    ?event({deduplicating_from_list, {aggregated, Aggregated}}),
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
    ?event({deduplicating_from_list, {result, Res}}),
    Res.

codec_to_tx(<<"ans104@1.0">>, Msg, Req, Opts) ->
    dev_codec_ans104:to(Msg, Req, Opts);
codec_to_tx(<<"tx@1.0">>, Msg, Req, Opts) ->
    dev_codec_tx:to(Msg, Req, Opts);
codec_to_tx(Codec, Msg, Req, Opts) ->
    ?event({codec_to_tx, {codec, Codec}, {msg, Msg}, {req, Req}}),
    throw({invalid_codec, Codec}).

coerce_format(<<"ans104">>) -> ans104;
coerce_format(<<"1">>) -> 1;
coerce_format(<<"2">>) -> 2;
coerce_format(Format) -> Format.

%%%===================================================================
%%% Tests.
%%%===================================================================
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
