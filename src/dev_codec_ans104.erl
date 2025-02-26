%%% @doc Codec for managing transformations from `ar_bundles'-style Arweave TX
%%% records to and from TABMs.
-module(dev_codec_ans104).
-export([id/1, to/1, from/1, attest/3, verify/3, attested/3]).
-include("include/hb.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).
%% The list of TX fields that users can set directly.
-define(TX_KEYS,
    [
        <<"id">>,
        <<"last_tx">>,
        <<"owner">>,
        <<"target">>,
        <<"signature">>
    ]
).
-define(FILTERED_TAGS,
    [
        <<"bundle-format">>,
        <<"bundle-map">>,
        <<"bundle-version">>
    ]
).

%% @doc Return the ID of a message.
id(Msg) ->
    TABM = dev_codec_structured:from(Msg),
    {ok, (to(TABM))#tx.id}.

%% @doc Sign a message using the `priv_wallet' key in the options.
attest(Msg, _Req, Opts) ->
    Signed = ar_bundles:sign_item(
        to(Msg),
        Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts)
    ),
    ID = Signed#tx.id,
    Owner = Signed#tx.owner,
    Sig = Signed#tx.signature,
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    % Gather the prior attestations.
    PriorAttestations = maps:get(<<"attestations">>, Msg, #{}),
    Attestation =
        #{
            <<"attestation-device">> => <<"ans104@1.0">>,
            <<"id">> => ID,
            <<"owner">> => Owner,
            <<"signature">> => Sig
        },
    AttestationWithHP =
        case Msg of
            #{ <<"hashpath">> := Hashpath } ->
                Attestation#{ <<"hashpath">> => Hashpath };
            _ -> Attestation
        end,
    MsgWithoutHP = maps:without([<<"hashpath">>], Msg),
    {ok,
        MsgWithoutHP#{
            <<"attestations">> =>
                PriorAttestations#{
                    Address => AttestationWithHP
                }
        }
    }.

%% @doc Return an empty list if the message as given does not validate, as
%% ANS-104 messages do not keep a list of attested keys.
attested(Msg, Req, Opts) ->
    case verify(Msg, Req, Opts) of
        {ok, true} -> {ok, maps:keys(Msg)};
        _ -> {ok, []}
    end.

%% @doc Verify an ANS-104 attestation.
verify(Msg, _Req, _Opts) ->
    MsgWithoutPriv = maps:remove(<<"priv">>, Msg), % Remove "priv" (with original_tags)
    MsgWithoutAttestations = maps:without([<<"attestations">>], MsgWithoutPriv),
    TX = to(MsgWithoutAttestations),
    % Reinject "priv" before verifying
    PrivMap = maps:get(<<"priv">>, Msg, #{}),
    case maps:find(<<"original_tags">>, PrivMap) of
        {ok, OrigTags} ->
            TXFixed = TX#tx { tags = OrigTags };
        error ->
            TXFixed = TX
    end,
    ar_bundles:verify_item(TXFixed).

%% @doc Convert a #tx record into a message map recursively.
from(Binary) when is_binary(Binary) -> Binary;
from(TX) when is_record(TX, tx) ->
    case lists:keyfind(<<"converge-type">>, 1, TX#tx.tags) of
        false ->
            do_from(TX);
        {<<"converge-type">>, <<"binary">>} ->
            TX#tx.data
    end.
do_from(RawTX) ->
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(ar_bundles:normalize(RawTX)), % <- Is norm necessary?
    % Get the raw fields and values of the tx record and pair them. Then convert 
    % the list of key-value pairs into a map, removing irrelevant fields.
    TXKeysMap =
        maps:with(?TX_KEYS,
            hb_converge:normalize_keys(
                maps:from_list(
                    lists:zip(
                        record_info(fields, tx),
                        tl(tuple_to_list(TX))
                    )
                )
            )
        ),
    % Generate a TABM from the tags.
    MapWithoutData = maps:merge(TXKeysMap, maps:from_list(TX#tx.tags)),

    % Stash original tags in `priv.original_tags' - untouched by normalization/lowercasing/etc.
    % Necessary for reconstructing deephash material in ar_bundles that would match
    % the signature and make ar_bundle:verify_item pass
    OriginalTags = TX#tx.tags,
    OriginalPriv = maps:get(<<"priv">>, MapWithoutData, #{}),
    PrivWithOriginalTags = maps:put(<<"original_tags">>, OriginalTags, OriginalPriv),
    MapWithPreservedTags = MapWithoutData#{ <<"priv">> => PrivWithOriginalTags },

    DataMap =
        case TX#tx.data of
            Data when is_map(Data) ->
                % If the data is a map, we need to recursively turn its children
                % into messages from their tx representations.
                maps:merge(
                    MapWithPreservedTags,
                    maps:map(fun(_, InnerValue) -> from(InnerValue) end, Data)
                );
            Data when Data == ?DEFAULT_DATA -> MapWithPreservedTags;
            Data when is_binary(Data) -> MapWithPreservedTags#{ <<"data">> => Data };
            Data ->
                ?event({unexpected_data_type, {explicit, Data}}),
                ?event({was_processing, {explicit, TX}}),
                throw(invalid_tx)
        end,
    % Merge the data map with the rest of the TX map and remove any keys that
    % are not part of the message.
    NormalizedDataMap =
        hb_converge:normalize_keys(maps:merge(DataMap, MapWithoutData)),
    %% Add the attestations to the message if the TX has a signature.
    ?event({message_before_attestations, NormalizedDataMap}),
    WithAttestations =
        case TX#tx.signature of
            ?DEFAULT_SIG ->
                NormalizedDataMap;
            _ ->
                Address = hb_util:human_id(ar_wallet:to_address(TX#tx.owner)),
                WithoutBaseAttestation =
                    maps:without(
                        [<<"id">>, <<"owner">>, <<"signature">>, <<"attestation-device">>],
                        NormalizedDataMap
                    ),
                WithoutBaseAttestation#{
                    <<"attestations">> => #{
                        Address => #{
                            <<"attestation-device">> => <<"ans104@1.0">>,
                            <<"id">> => TX#tx.id,
                            <<"owner">> => TX#tx.owner,
                            <<"signature">> => TX#tx.signature
                        }
                    }
                }
        end,
    ResTmp = maps:without(?FILTERED_TAGS, WithAttestations),
    ?event({message_after_attestations, ResTmp}),

    % We lost `priv` after hb_converge:normalize_keys and re-serializing it via codec
    % so we need to re-inject it
    PrivMap = maps:get(<<"priv">>, WithAttestations, #{}),
    Res = ResTmp#{ <<"priv">> => PrivMap },

    Res.

%% @doc Internal helper to translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
to(Binary) when is_binary(Binary) ->
    % ar_bundles cannot serialize just a simple binary or get an ID for it, so
    % we turn it into a TX record with a special tag, tx_to_message will
    % identify this tag and extract just the binary.
    #tx{
        tags= [{<<"converge-type">>, <<"binary">>}],
        data = Binary
    };
to(TX) when is_record(TX, tx) -> TX;
to(RawTABM) when is_map(RawTABM) ->
    % The path is a special case so we normalized it first. It may have been
    % modified by `hb_converge' in order to set it to the current key that is
    % being executed. We should check whether the path is in the
    % `priv/Converge/Original-Path' field, and if so, use that instead of the
    % stated path. This normalizes the path, such that the signed message will
    % continue to validate correctly.
    TABM = hb_converge:normalize_keys(maps:without([<<"attestations">>], RawTABM)),
    Attestations = maps:get(<<"attestations">>, RawTABM, #{}),
    TABMWithAtt =
        case maps:keys(Attestations) of
            [] -> TABM;
            [Address] -> maps:merge(TABM, maps:get(Address, Attestations));
            _ -> throw({multisignatures_not_supported_by_ans104, RawTABM})
        end,
    M =
        case {maps:find(<<"path">>, TABMWithAtt), hb_private:from_message(TABMWithAtt)} of
            {{ok, _}, #{ <<"converge">> := #{ <<"original-path">> := Path } }} ->
                maps:put(<<"path">>, Path, TABMWithAtt);
            _ -> TABMWithAtt
        end,
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages. Notably, we do not simply
    % call message_to_tx/1 on the inner map because that would lead to adding
    % an extra layer of nesting to the data.
    %?event({message_to_tx, {keys, Keys}, {map, M}}),
    MsgKeyMap =
        maps:map(
            fun(_Key, Msg) when is_map(Msg) -> to(Msg);
            (_Key, Value) -> Value
            end,
            M
        ),
    NormalizedMsgKeyMap = hb_converge:normalize_keys(MsgKeyMap),
    % Iterate through the default fields, replacing them with the values from
    % the message map if they are present.
    {RemainingMap, BaseTXList} =
        lists:foldl(
            fun({Field, Default}, {RemMap, Acc}) ->
                NormKey = hb_converge:normalize_key(Field),
                case maps:find(NormKey, NormalizedMsgKeyMap) of
                    error -> {RemMap, [Default | Acc]};
                    {ok, Value} when is_binary(Default) andalso ?IS_ID(Value) ->
                        {
                            maps:remove(NormKey, RemMap),
                            [hb_util:native_id(Value)|Acc]
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
    {Tags, RawDataItems} =
        lists:partition(
            fun({_Key, Value}) when is_binary(Value) ->
                    case unicode:characters_to_binary(Value) of
                        {error, _, _} -> false;
                        _ -> byte_size(Value) =< ?MAX_TAG_VAL
                    end;
                (_) -> false
            end,
            [ 
                    {Key, maps:get(Key, RemainingMap)}
                ||
                    Key <- maps:keys(RemainingMap)
            ]
        ),
    % We don't let the user set the tags directly, but they can instead set any
    % number of keys to short binary values, which will be included as tags.
    TX = TXWithoutTags#tx { tags = Tags },
    % Recursively turn the remaining data items into tx records.
    DataItems = maps:from_list(lists:map(
        fun({Key, Value}) ->
            {hb_converge:normalize_key(Key), to(Value)}
        end,
        RawDataItems
    )),
    % Set the data based on the remaining keys.
    TXWithData = 
        case {TX#tx.data, maps:size(DataItems)} of
            {Binary, 0} when is_binary(Binary) ->
                TX;
            {?DEFAULT_DATA, _} ->
                TX#tx { data = DataItems };
            {Data, _} when is_map(Data) ->
                TX#tx { data = maps:merge(Data, DataItems) };
            {Data, _} when is_record(Data, tx) ->
                TX#tx { data = DataItems#{ <<"data">> => Data } };
            {Data, _} when is_binary(Data) ->
                TX#tx { data = DataItems#{ <<"data">> => to(Data) } }
        end,
    % ar_bundles:reset_ids(ar_bundles:normalize(TXWithData));
    Res = try ar_bundles:reset_ids(ar_bundles:normalize(TXWithData))
    catch
        _:Error ->
            ?event({{reset_ids_error, Error}, {tx_without_data, TX}}),
            ?event({prepared_tx_before_ids,
                {tags, {explicit, TXWithData#tx.tags}},
                {data, TXWithData#tx.data}
            }),
            throw(Error)
    end,
    %?event({result, {explicit, Res}}),
    Res;
to(Other) ->
    throw(invalid_tx).