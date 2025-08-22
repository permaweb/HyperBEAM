%%% @doc Library functions for decoding ANS-104-style data items to TABM form.
-module(dev_codec_ans104_from).
-export([fields/2, tags/2, data/4, committed/5, base/5]).
-export([with_commitments/5]).
-include("include/hb.hrl").

%% @doc Return a TABM message containing the fields of the given decoded
%% ANS-104 data item that should be included in the base message.
fields(Item, _Opts) ->
    case Item#tx.target of
        ?DEFAULT_TARGET -> #{};
        Target ->
            #{
                <<"target">> => hb_util:encode(Target)
            }
    end.

%% @doc Return a TABM of the raw tags of the item, including all metadata
%% (e.g. `ao-type', `ao-data-key', etc.)
tags(Item, Opts) ->
    Tags = hb_ao:normalize_keys(
        deduplicating_from_list(Item#tx.tags, Opts),
        Opts
    ),
    ao_types(Tags, Opts).

%% @doc Ensure the encoded keys in the `ao-types' field are lowercased and
%% normalized like the other keys in the tags field.
ao_types(#{ <<"ao-types">> := AoTypes } = Tags, Opts) ->
    AOTypes = dev_codec_structured:decode_ao_types(AoTypes, Opts),
    % Normalize all keys in the ao-types map and re-encode
    NormAOTypes =
        maps:fold(
            fun(Key, Val, Acc) ->
                NormKey = hb_util:to_lower(hb_ao:normalize_key(Key)),
                Acc#{ NormKey => Val }
            end,
            #{},
            AOTypes
        ),
    EncodedAOTypes = dev_codec_structured:encode_ao_types(NormAOTypes, Opts),
    Tags#{ <<"ao-types">> := EncodedAOTypes };
ao_types(Tags, _Opts) ->
    Tags.

%% @doc Return a TABM of the keys and values found in the data field of the item.
data(Item, Req, Tags, Opts) ->
    % If the data field is empty, we return an empty map. If it is a map, we
    % return it as such. Otherwise, we return a map with the data key set to
    % the raw data value. This handles unbundling nested messages, as well as 
    % applying the `ao-data-key' tag if given.
    DataKey = maps:get(<<"ao-data-key">>, Tags, <<"data">>),
    case {DataKey, Item#tx.data} of
        {_, ?DEFAULT_DATA} -> #{};
        {DataKey, Map} when is_map(Map) ->
            % If the data is a map, we need to recursively turn its children
            % into messages from their tx representations.
            hb_ao:normalize_keys(
                hb_maps:map(
                    fun(_, InnerValue) ->
                        hb_util:ok(dev_codec_ans104:from(InnerValue, Req, Opts))
                    end,
                    Map,
                    Opts
                ),
                Opts
            );
        {DataKey, Data} -> #{ DataKey => Data }
    end.

%% @doc Calculate the list of committed keys for an item, based on its 
%% components (fields, tags, and data).
committed(Item, Fields, Tags, Data, Opts) ->
    hb_util:unique(
        data_keys(Data, Opts) ++
        tag_keys(Item, Opts) ++
        field_keys(Fields, Tags, Data, Opts)
    ).

%% @doc Return the list of the keys from the fields TABM.
field_keys(BaseFields, Tags, Data, Opts) ->
    HasTarget =
        hb_maps:is_key(<<"target">>, BaseFields, Opts) orelse
        hb_maps:is_key(<<"target">>, Tags, Opts) orelse
        hb_maps:is_key(<<"target">>, Data, Opts),
    case HasTarget of
        true -> [<<"target">>];
        false -> []
    end.

%% @doc Return the list of the keys from the data TABM.
data_keys(Data, Opts) ->
    hb_util:to_sorted_keys(Data, Opts).

%% @doc Return the list of the keys from the tags TABM. Filter all metadata
%% tags: `ao-data-key', `ao-types', `bundle-format', `bundle-version'.
tag_keys(Item, _Opts) ->
    MetaTags = [
        <<"bundle-format">>,
        <<"bundle-version">>,
        <<"bundle-map">>,
        <<"ao-data-key">>
    ],
    lists:filtermap(
        fun({Tag, _}) ->
            case lists:member(Tag, MetaTags) of
                true -> false;
                false -> {true, hb_util:to_lower(hb_ao:normalize_key(Tag))}
            end
        end,
        Item#tx.tags
    ).

%% @doc Return the complete message for an item, less its commitments. The
%% precidence order for choosing fields to place into the base message is:
%% 1. Data
%% 2. Tags
%% 3. Fields
base(CommittedKeys, Fields, Tags, Data, Opts) ->
    hb_maps:from_list(
        lists:map(
            fun(Key) ->
                case hb_maps:find(Key, Data, Opts) of
                    error ->
                        case hb_maps:find(Key, Fields, Opts) of
                            error ->
                                case hb_maps:find(Key, Tags, Opts) of
                                    error -> throw({missing_key, Key});
                                    {ok, Value} -> {Key, Value}
                                end;
                            {ok, Value} -> {Key, Value}
                        end;
                    {ok, Value} -> {Key, Value}
                end
            end,
            CommittedKeys
        )
    ).

%% @doc Return a message with the appropriate commitments added to it.
with_commitments(Item, Tags, Base, CommittedKeys, Opts) ->
    case Item#tx.signature of
        ?DEFAULT_SIG ->
            case normal_tags(Item#tx.tags) of
                true -> Base;
                false ->
                    with_unsigned_commitment(Item, Tags, Base, CommittedKeys, Opts)
            end;
        _ -> with_signed_commitment(Item, Tags, Base, CommittedKeys, Opts)
    end.

%% @doc Returns a commitments message for an item, containing an unsigned
%% commitment.
with_unsigned_commitment(Item, Tags, UncommittedMessage, CommittedKeys, Opts) ->
    ID = hb_util:human_id(Item#tx.unsigned_id),
    UncommittedMessage#{
        <<"commitments">> => #{
            ID =>
                filter_unset(
                    #{
                        <<"commitment-device">> => <<"ans104@1.0">>,
                        <<"committed">> => CommittedKeys,
                        <<"type">> => <<"unsigned-sha256">>,
                        <<"bundle">> => bundle_commitment_key(Tags, Opts),
                        <<"original-tags">> => original_tags(Item, Opts),
                        <<"field-target">> =>
                            case Item#tx.target of
                                ?DEFAULT_TARGET -> unset;
                                Target -> hb_util:encode(Target)
                            end,
                        <<"field-anchor">> =>
                            case Item#tx.anchor of
                                ?DEFAULT_ANCHOR -> unset;
                                LastTX -> LastTX
                            end
                    },
                    Opts
                )
        }
    }.

%% @doc Returns a commitments message for an item, containing a signed
%% commitment.
with_signed_commitment(Item, Tags, UncommittedMessage, CommittedKeys, Opts) ->
    Address = hb_util:human_id(ar_wallet:to_address(Item#tx.owner)),
    ID = hb_util:human_id(Item#tx.id),
    Commitment =
        filter_unset(
            #{
                <<"commitment-device">> => <<"ans104@1.0">>,
                <<"committer">> => Address,
                <<"committed">> => CommittedKeys,
                <<"signature">> => hb_util:encode(Item#tx.signature),
                <<"keyid">> =>
                    <<"publickey:", (hb_util:encode(Item#tx.owner))/binary>>,
                <<"type">> => <<"rsa-pss-sha256">>,
                <<"bundle">> => bundle_commitment_key(Tags, Opts),
                <<"original-tags">> => original_tags(Item, Opts),
                <<"field-anchor">> =>
                    case Item#tx.anchor of
                        ?DEFAULT_ANCHOR -> unset;
                        LastTX -> LastTX
                    end,
                <<"field-target">> =>
                    case Item#tx.target of
                        ?DEFAULT_TARGET -> unset;
                        Target -> hb_util:encode(Target)
                    end
            },
            Opts
        ),
    UncommittedMessage#{
        <<"commitments">> => #{
            ID => Commitment
        }
    }.

%% @doc Return the bundle key for an item.
bundle_commitment_key(Tags, Opts) ->
    hb_util:bin(hb_maps:is_key(<<"bundle-format">>, Tags, Opts)).

%% @doc Check whether a list of key-value pairs contains only normalized keys.
normal_tags(Tags) ->
    lists:all(
        fun({Key, _}) ->
            hb_util:to_lower(hb_ao:normalize_key(Key)) =:= Key
        end,
        Tags
    ).

%% @doc Return the original tags of an item if it is applicable. Otherwise,
%% return `undefined'.
original_tags(Item, _Opts) ->
    case normal_tags(Item#tx.tags) of
        true -> unset;
        false -> encoded_tags_to_map(Item#tx.tags)
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

%% @doc Remove all undefined values from a map.
filter_unset(Map, Opts) ->
    hb_maps:filter(
        fun(_, Value) ->
            case Value of
                unset -> false;
                _ -> true
            end
        end,
        Map,
        Opts
    ).

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