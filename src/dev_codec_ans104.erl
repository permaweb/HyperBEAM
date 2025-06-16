%%% @doc Codec for managing transformations from `ar_bundles'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_ans104).
-export([to/3, from/3, commit/3, verify/3, content_type/1]).
-export([serialize/3, deserialize/3]).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 4096).
%% The list of TX fields that users can set directly. Data is excluded because
%% it may be set by the codec in order to support nested messages.
-define(TX_KEYS,
        [<<"last_tx">>,
         <<"owner">>,
         <<"target">>,
         <<"signature">>]).
%%% The list of keys that should be forced into the tag list, rather than being
%%% encoded as fields in the TX record.
-define(FORCED_TAG_FIELDS,
        [<<"quantity">>,
         <<"manifest">>,
         <<"data_size">>,
         <<"data_tree">>,
         <<"data_root">>,
         <<"reward">>,
         <<"denomination">>,
         <<"signature_type">>]).
%%% The list of tags that a user is explicitly committing to when they sign an
%%% ANS-104 message.
-define(BASE_COMMITTED_TAGS, ?TX_KEYS ++ [<<"data">>]).
%% List of tags that should be removed during `to'. These relate to the nested
%% ar_bundles format that is used by the `ans104@1.0' codec.
-define(FILTERED_TAGS,
        [<<"bundle-format">>,
         <<"bundle-map">>,
         <<"bundle-version">>]).


%% @doc Return the content type for the codec.
content_type(_) -> {ok, <<"application/ans104">>}.


%% @doc Serialize a message or TX to a binary.
serialize(Msg, Req, Opts) when is_map(Msg) ->
    serialize(to(Msg, Req, Opts), Req, Opts);
serialize(TX, _Req, _Opts) when is_record(TX, tx) ->
    {ok, ar_bundles:serialize(TX)}.


%% @doc Deserialize a binary ans104 message to a TABM.
deserialize(#{<<"body">> := Binary}, Req, Opts) ->
    deserialize(Binary, Req, Opts);
deserialize(Binary, Req, Opts) when is_binary(Binary) ->
    deserialize(ar_bundles:deserialize(Binary), Req, Opts);
deserialize(TX, Req, Opts) when is_record(TX, tx) ->
    from(TX, Req, Opts).


%% @doc Sign a message using the `priv_wallet' key in the options. Supports both
%% the `hmac-sha256' and `rsa-pss-sha256' algorithms, offering unsigned and
%% signed commitments.
commit(Msg, Req = #{<<"type">> := <<"unsigned">>}, Opts) ->
    commit(Msg, Req#{<<"type">> => <<"unsigned-sha256">>}, Opts);
commit(Msg, Req = #{<<"type">> := <<"signed">>}, Opts) ->
    commit(Msg, Req#{<<"type">> => <<"rsa-pss-sha256">>}, Opts);
commit(Msg, Req = #{<<"type">> := <<"rsa-pss-sha256">>}, Opts) ->
    % Convert the given message to an ANS-104 TX record, sign it, and convert
    % it back to a structured message.
    ?event({committing, {input, Msg}}),
    Signed =
        ar_bundles:sign_item(
          hb_util:ok(to(hb_private:reset(Msg), Req, Opts)),
          hb_opts:get(priv_wallet, no_viable_wallet, Opts)),
    SignedStructured =
        hb_message:convert(
          Signed,
          <<"structured@1.0">>,
          <<"ans104@1.0">>,
          Opts),
    {ok, SignedStructured};
commit(Msg, #{<<"type">> := <<"unsigned-sha256">>}, Opts) ->
    % Remove the commitments from the message, convert it to ANS-104, then back.
    % This forces the message to be normalized and the unsigned ID to be
    % recalculated.
    {ok,
     hb_message:convert(
       hb_maps:without([<<"commitments">>], Msg, Opts),
       <<"ans104@1.0">>,
       <<"structured@1.0">>,
       Opts)}.


%% @doc Verify an ANS-104 commitment.
verify(Msg, Req, Opts) ->
    ?event({verify, {base, Msg}, {req, Req}}),
    OnlyWithCommitment =
        hb_private:reset(
          hb_message:with_commitments(
            Req,
            Msg,
            Opts)),
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


do_from(RawTX, Req, Opts) ->
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(ar_bundles:normalize(RawTX)),
    OriginalTagMap = encoded_tags_to_map(TX#tx.tags),
    % Get the raw fields and values of the tx record and pair them. Then convert
    % the list of key-value pairs into a map, removing irrelevant fields.
    RawTXKeysMap =
        hb_maps:with(?TX_KEYS,
                     hb_ao:normalize_keys(
                       hb_maps:from_list(
                         lists:zip(
                           record_info(fields, tx),
                           tl(tuple_to_list(TX)))),
                       Opts),
                     Opts),
    % Normalize `owner' to `keyid', remove 'id', and remove 'signature'
    TXKeysMap =
        hb_message:filter_default_keys(
          maps:without(
            [<<"owner">>, <<"signature">>],
            case maps:get(<<"owner">>, RawTXKeysMap, ?DEFAULT_OWNER) of
                ?DEFAULT_OWNER -> RawTXKeysMap;
                Owner -> RawTXKeysMap#{<<"keyid">> => Owner}
            end)),
    % Generate a TABM from the tags.
    MapWithoutData =
        hb_maps:merge(
          TXKeysMap,
          deduplicating_from_list(TX#tx.tags, Opts),
          Opts),
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
                            hb_util:ok(from(InnerValue, Req, Opts))
                    end,
                    Data,
                    Opts),
                  Opts);
            Data when Data == ?DEFAULT_DATA -> MapWithoutData;
            Data when is_binary(Data) -> MapWithoutData#{<<"data">> => Data};
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
    CommittedKeys =
        hb_ao:normalize_keys(
          hb_util:unique(
            ?BASE_COMMITTED_TAGS ++
            [ hb_ao:normalize_key(Tag)
              || {Tag, _} <- TX#tx.tags ] ++
            hb_util:to_sorted_keys(NormalizedDataMap))),
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
                                                         <<"commitment-device">> => <<"ans104@1.0">>,
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
                      [<<"id">>,
                       <<"keyid">>,
                       <<"signature">>,
                       <<"commitment-device">>,
                       <<"original-tags">>],
                      NormalizedDataMap,
                      Opts),
                ID = hb_util:human_id(TX#tx.id),
                ?event({raw_tx_id, {id, ID}, {explicit, WithoutBaseCommitment}}),
                Commitment = #{
                               <<"commitment-device">> => <<"ans104@1.0">>,
                               <<"committer">> => Address,
                               <<"committed">> => CommittedKeys,
                               <<"keyid">> => hb_util:encode(TX#tx.owner),
                               <<"signature">> => hb_util:encode(TX#tx.signature),
                               <<"type">> => <<"rsa-pss-sha256">>
                              },
                CommitmentWithBundle =
                    case lists:member(<<"bundle-format">>, maps:values(CommittedKeys)) of
                        true -> Commitment#{<<"bundle">> => true};
                        false -> Commitment
                    end,
                WithoutBaseCommitment#{
                  <<"commitments">> => #{
                                         ID =>
                                             case normal_tags(TX#tx.tags) of
                                                 true -> CommitmentWithBundle;
                                                 false ->
                                                     CommitmentWithBundle#{
                                                       <<"original-tags">> => OriginalTagMap
                                                      }
                                             end
                                        }
                 }
        end,
    Res = hb_maps:without(?FILTERED_TAGS, WithCommitments, Opts),
    ?event({message_after_commitments, Res}),
    {ok, Res}.


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
          Tags),
    ?event({deduplicating_from_list, {aggregated, Aggregated}}),
    % Convert aggregated values into a structured-field list.
    Res =
        hb_maps:map(
          fun(_Key, Values) when is_list(Values) ->
                  % Convert Erlang lists of binaries into a structured-field list.
                  iolist_to_binary(
                    hb_structured_fields:list(
                      [ {item, {string, Value}, []}
                        || Value <- Values ]));
             (_Key, Value) ->
                  Value
          end,
          Aggregated,
          Opts),
    ?event({deduplicating_from_list, {result, Res}}),
    Res.


%% @doc Check whether a list of key-value pairs contains only normalized keys.
normal_tags(Tags) ->
    lists:all(
      fun({Key, _}) ->
              hb_ao:normalize_key(Key) =:= Key
      end,
      Tags).


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
        Tags)).


%% @doc Convert a HyperBEAM-compatible map into an ANS-104 encoded tag list,
%% recreating the original order of the tags.
tag_map_to_encoded_tags(TagMap) ->
    OrderedList = hb_util:message_to_ordered_list(hb_private:reset(TagMap)),
    ?event({ordered_tagmap, {explicit, OrderedList}, {input, {explicit, TagMap}}}),
    lists:map(
      fun(#{<<"name">> := Key, <<"value">> := Value}) ->
              {Key, Value}
      end,
      OrderedList).


%% @doc Internal helper to translate a message to its #tx record representation,
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
      }};
to(TX, _Req, _Opts) when is_record(TX, tx) -> {ok, TX};
to(RawTABM, Req, Opts) when is_map(RawTABM) ->
    % Ensure that the TABM is fully loaded if the `bundle` key is set to true.
    NormTABM = hb_message:filter_default_keys(RawTABM),
    ?event({to, {inbound, NormTABM}, {req, Req}}),
    MaybeBundle =
        case hb_util:atom(hb_ao:get(<<"bundle">>, Req, false, Opts)) of
            false -> NormTABM;
            true ->
                % Convert back to the fully loaded structured@1.0 message, then
                % convert to TABM with bundling enabled.
                Structured = hb_message:convert(NormTABM, <<"structured@1.0">>, Opts),
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
                      Opts),
                % Ensure the commitments from the original message are the only
                % ones in the fully loaded message.
                LoadedComms = maps:get(<<"commitments">>, NormTABM, #{}),
                LoadedTABM#{<<"commitments">> => LoadedComms}
        end,
    ?event({to, {maybe_bundle, MaybeBundle}}),
    TABM =
        hb_ao:normalize_keys(
          hb_maps:without([<<"commitments">>], MaybeBundle, Opts),
          Opts),
    Commitments = hb_maps:get(<<"commitments">>, MaybeBundle, #{}, Opts),
    TABMWithComm =
        case hb_maps:keys(Commitments, Opts) of
            [] -> TABM;
            [ID] ->
                Commitment = hb_maps:get(ID, Commitments),
                TABMWithoutCommitmentKeys =
                    TABM#{
                      <<"signature">> =>
                          hb_util:decode(
                            maps:get(<<"signature">>,
                                     Commitment,
                                     hb_util:encode(?DEFAULT_SIG))),
                      <<"owner">> =>
                          hb_util:decode(
                            maps:get(<<"keyid">>,
                                     Commitment,
                                     hb_util:encode(?DEFAULT_OWNER)))
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
          fun(_Key, Msg) when is_map(Msg) -> hb_util:ok(to(Msg, Req, Opts));
             (_Key, Value) -> Value
          end,
          TABMNoOrigTags,
          Opts),
    NormalizedMsgKeyMap = hb_ao:normalize_keys(MsgKeyMap, Opts),
    % Iterate through the default fields, replacing them with the values from
    % the message map if they are present.
    ForcedTagFields =
        maps:with(?FORCED_TAG_FIELDS, NormalizedMsgKeyMap),
    NormalizedMsgKeyMap2 =
        maps:without(?FORCED_TAG_FIELDS, NormalizedMsgKeyMap),
    {RemainingMapWithoutForcedTags, BaseTXList} =
        lists:foldl(
          fun({Field, Default}, {RemMap, Acc}) ->
                  NormKey = hb_ao:normalize_key(Field),
                  case maps:find(NormKey, NormalizedMsgKeyMap2) of
                      error -> {RemMap, [Default | Acc]};
                      {ok, Value} when is_binary(Default) andalso ?IS_ID(Value) ->
                          % NOTE: Do we really want to do this type coercion?
                          {maps:remove(NormKey, RemMap),
                           [try hb_util:native_id(Value) catch _:_ -> Value end | Acc]};
                      {ok, Value} ->
                          {maps:remove(NormKey, RemMap),
                           [Value | Acc]}
                  end
          end,
          {NormalizedMsgKeyMap2, []},
          hb_message:default_tx_list()),
    RemainingMap = maps:merge(RemainingMapWithoutForcedTags, ForcedTagFields),
    % Rebuild the tx record from the new list of fields and values.
    TXWithoutTags = list_to_tuple([tx | lists:reverse(BaseTXList)]),
    % Calculate which set of the remaining keys will be used as tags.
    {Remaining, RawDataItems} =
        lists:partition(
          fun({_Key, Value}) when is_binary(Value) ->
                  case unicode:characters_to_binary(Value) of
                      {error, _, _} -> false;
                      _ -> byte_size(Value) =< ?MAX_TAG_VAL
                  end;
             (_) -> false
          end,
          hb_maps:to_list(RemainingMap, Opts)),
    ?event({remaining_keys_to_convert_to_tags, {explicit, Remaining}}),
    ?event({original_tags, {explicit, OriginalTags}}),
    % Check that the remaining keys are as we expect them to be, given the
    % original tags. We do this by re-calculating the expected tags from the
    % original tags and comparing the result to the remaining keys.
    if
        length(OriginalTags) > 0 ->
            ExpectedTagsFromOriginal = deduplicating_from_list(OriginalTags, Opts),
            NormRemaining = maps:from_list(Remaining),
            case NormRemaining == ExpectedTagsFromOriginal of
                true -> ok;
                false ->
                    ?event(warning,
                           {invalid_original_tags,
                            {expected, ExpectedTagsFromOriginal},
                            {given, NormRemaining}}),
                    throw({invalid_original_tags, OriginalTags, NormRemaining})
            end;
        true -> ok
    end,
    % Restore the original tags, or the remaining keys if there are no original
    % tags.
    TX =
        TXWithoutTags#tx{
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
                                            {hb_ao:normalize_key(Key), hb_util:ok(to(Value, Req, Opts))}
                                    end,
                                    RawDataItems)),
    % Set the data based on the remaining keys.
    TXWithData =
        case {TX#tx.data, hb_maps:size(DataItems, Opts)} of
            {Binary, 0} when is_binary(Binary) ->
                TX;
            {?DEFAULT_DATA, _} ->
                TX#tx{data = DataItems};
            {Data, _} when is_map(Data) ->
                TX#tx{data = hb_maps:merge(Data, DataItems, Opts)};
            {Data, _} when is_record(Data, tx) ->
                TX#tx{data = DataItems#{<<"data">> => Data}};
            {Data, _} when is_binary(Data) ->
                TX#tx{
                  data =
                      DataItems#{
                        <<"data">> => hb_util:ok(to(Data, Req, Opts))
                       }
                 }
        end,
    Res =
        try
            ar_bundles:reset_ids(ar_bundles:normalize(TXWithData))
        catch
            _:Error ->
                ?event({{reset_ids_error, Error}, {tx_without_data, TX}}),
                ?event({prepared_tx_before_ids,
                        {tags, {explicit, TXWithData#tx.tags}},
                        {data, TXWithData#tx.data}}),
                throw(Error)
        end,
    ?event({to_result, {explicit, Res}}),
    {ok, Res};
to(_Other, _Req, _Opts) ->
    throw(invalid_tx).


%%% ANS-104-specific testing cases.


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
    TX = #tx{
           tags = [{<<"Test-Tag">>, <<"test-value">>}]
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
    Opts = #{store => hb_test_utils:test_store()},
    TX = #tx{
           tags = [{<<"Test-Tag">>, <<"test-value">>},
                   {<<"test-tag-2">>, <<"test-value-2">>}]
          },
    SignedTX = ar_bundles:sign_item(TX, ar_wallet:new()),
    SignedMsg =
        hb_message:convert(
          SignedTX,
          <<"structured@1.0">>,
          <<"ans104@1.0">>,
          Opts),
    SignedID = hb_message:id(SignedMsg, all),
    ?event({signed_msg, SignedMsg}),
    OnlyCommitted = hb_message:with_only_committed(SignedMsg, Opts),
    ?event({only_committed, OnlyCommitted}),
    {ok, ID} = hb_cache:write(SignedMsg, Opts),
    ?event({id, ID}),
    {ok, ReadMsg} = hb_cache:read(SignedID, Opts),
    ?event({restored_msg, ReadMsg}),
    {ok, ReadTX} = to(ReadMsg, #{}, Opts),
    ?event({restored_tx, ReadTX}),
    ?assert(hb_message:match(ReadMsg, SignedMsg)),
    ?assert(ar_bundles:verify_item(ReadTX)).


unsigned_duplicated_tag_name_test() ->
    TX = ar_bundles:reset_ids(ar_bundles:normalize(#tx{
                                                     tags = [{<<"Test-Tag">>, <<"test-value">>},
                                                             {<<"test-tag">>, <<"test-value-2">>}]
                                                    })),
    Msg = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({msg, Msg}),
    TX2 = hb_message:convert(Msg, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({tx2, TX2}),
    ?assertEqual(TX, TX2).


signed_duplicated_tag_name_test() ->
    TX = ar_bundles:sign_item(#tx{
                                tags = [{<<"Test-Tag">>, <<"test-value">>},
                                        {<<"test-tag">>, <<"test-value-2">>}]
                               },
                              ar_wallet:new()),
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
    TX = ar_bundles:sign_item(#tx{
                                target = crypto:strong_rand_bytes(32),
                                tags = [{<<"test-tag">>, <<"test-value">>},
                                        {<<"test-tag-2">>, <<"test-value-2">>}],
                                data = <<"test-data">>
                               },
                              ar_wallet:new()),
    ?event({tx, TX}),
    Decoded = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({decoded, Decoded}),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Decoded, #{}),
    ?event({only_committed, OnlyCommitted}),
    Encoded = hb_message:convert(OnlyCommitted, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({encoded, Encoded}),
    ?assertEqual(TX, Encoded).


type_tag_test() ->
    TX =
        ar_bundles:sign_item(
          #tx{
            tags = [{<<"type">>, <<"test-value">>}]
           },
          ar_wallet:new()),
    ?event({tx, TX}),
    Structured = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, #{}),
    ?event({structured, Structured}),
    TX2 = hb_message:convert(Structured, <<"ans104@1.0">>, <<"structured@1.0">>, #{}),
    ?event({after_conversion, TX2}),
    ?assertEqual(TX, TX2).


simple_signed_to_httpsig_test_disabled() ->
    TX =
        ar_bundles:sign_item(
          #tx{
            tags = [{<<"test-tag">>, <<"test-value">>},
                    {<<"test-tag-2">>, <<"test-value-2">>},
                    {<<"Capitalized-Tag">>, <<"test-value-3">>}]
           },
          ar_wallet:new()),
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
