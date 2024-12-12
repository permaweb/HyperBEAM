
%%% @doc This module acts an adapter between messages, as modeled in the
%%% Converge Protocol, and their uderlying binary representations.
%%% See `docs/converge-protocol.md' for details on Converge. Unless you are
%%% implementing a new message serialization format, you should not need to 
%%% interact with this module directly. Instead, use the `hb_converge'
%%% interfaces to interact with all messages. The `dev_message' module
%%% implements a device interface for handling messages as the default Converge
%%% device.
-module(hb_message).
-export([load/2, sign/2, verify/1]).
-export([serialize/1, serialize/2, deserialize/1, deserialize/2, signers/1]).
-export([message_to_tx/1, tx_to_message/1]).
-export([message_to_http/1]).
%%% Debugging tools:
-export([print/1, format/1, format/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The size at which a value should be made into a body item, instead of a
%% tag.
-define(MAX_TAG_VAL, 128).
%% The list of TX fields that users can set directly.
-define(TX_KEYS,
    [id, unsigned_id, last_tx, owner, target, signature]).
-define(FILTERED_TAGS,
    [
        <<"Converge-Large-Binary">>,
        <<"Bundle-Format">>,
        <<"Bundle-Map">>,
        <<"Bundle-Version">>,
        <<"Type:">>
    ]
).
-define(REGEN_KEYS, [id, unsigned_id]).

%% @doc Pretty-print a message.
print(Msg) -> print(Msg, 0).
print(Msg, Indent) ->
    io:format(standard_error, "~s", [lists:flatten(format(Msg, Indent))]).

%% @doc Format a message for printing, optionally taking an indentation level
%% to start from.
format(Item) -> format(Item, 0).
format(Bin, Indent) when is_binary(Bin) ->
    hb_util:format_indented(
        hb_util:format_binary(Bin),
        Indent
    );
format(Map, Indent) when is_map(Map) ->
    Header = hb_util:format_indented("Message {~n", Indent),
    Res = lists:map(
        fun({Key, Val}) ->
            NormKey = hb_converge:to_key(Key, #{ error_strategy => ignore }),
            KeyStr = 
                case NormKey of
                    Key ->
                        io_lib:format("~p", [NormKey]);
                    undefined ->
                        io_lib:format("~p [!!! INVALID KEY !!!]", [Key]);
                    _ ->
                        io_lib:format("~p [raw: ~p]", [NormKey, Key])
                end,
            hb_util:format_indented(
                "~s := ~s~n",
                [
                    lists:flatten(KeyStr),
                    case Val of
                        NextMap when is_map(NextMap) ->
                            hb_util:format_map(NextMap, Indent + 2);
                        Bin when is_binary(Bin) ->
                            hb_util:format_binary(Bin);
                        Other ->
                            io_lib:format("~p", [Other])
                    end
                ],
                Indent + 1
            )
        end,
        maps:to_list(Map)
    ),
    case Res of
        [] -> "[Empty map]";
        _ ->
            lists:flatten(
                Header ++ Res ++ hb_util:format_indented("}", Indent)
            )
    end;
format(Item, Indent) ->
    % Whatever we have is not a message map.
    hb_util:format_indented("[UNEXPECTED VALUE] ~p", [Item], Indent).

%% @doc Return the signers of a message. For now, this is just the signer
%% of the message itself. In the future, we will support multiple signers.
signers(Msg) ->
    [ar_bundles:signer(Msg)].

%% @doc Sign a message with the given wallet.
sign(Msg, Wallet) ->
    tx_to_message(ar_bundles:sign_item(message_to_tx(Msg), Wallet)).

%% @doc Verify a message.
verify(Msg) ->
    ar_bundles:verify_item(message_to_tx(Msg)).

%% @doc Load a message from the cache.
load(Store, ID) when is_binary(ID)
        andalso (byte_size(ID) == 43 orelse byte_size(ID) == 32) ->
    tx_to_message(hb_cache:read_message(Store, ID));
load(Store, Path) ->
    tx_to_message(hb_cache:read(Store, Path)).

%% @doc Check if two maps match, including recursively checking nested maps.
match(Map1, Map2) ->
    Keys1 = maps:keys(NormMap1 = minimize(normalize(Map1))),
    Keys2 = maps:keys(NormMap2 = minimize(normalize(Map2))),
    case Keys1 == Keys2 of
        true ->
            lists:all(
                fun(Key) ->
                    Val1 = maps:get(Key, NormMap1, not_found),
                    Val2 = maps:get(Key, NormMap2, not_found),
                    case is_map(Val1) andalso is_map(Val2) of
                        true -> match(Val1, Val2);
                        false ->
                            case Val1 == Val2 of
                                true -> true;
                                false ->
                                    ?event(
                                        {key_mismatch,
                                            {explicit, {Key, Val1, Val2}}
                                        }
                                    ),
                                    false
                            end
                    end
                end,
                Keys1
            );
        false ->
            ?event({keys_mismatch, Keys1, Keys2}),
            false
    end.
	
matchable_keys(Map) ->
    lists:sort(lists:map(fun hb_converge:key_to_binary/1, maps:keys(Map))).

%% @doc Normalize the keys in a map. Also takes a list of keys and returns a
%% sorted list of normalized keys if the input is a list.
normalize_keys(Keys) when is_list(Keys) ->
    lists:sort(lists:map(fun hb_converge:key_to_binary/1, Keys));
normalize_keys(Map) ->
    maps:from_list(
        lists:map(
            fun({Key, Value}) ->
                {hb_converge:key_to_binary(Key), Value}
            end,
            maps:to_list(Map)
        )
    ).

%% @doc Remove keys from the map that can be regenerated.
minimize(Map) ->
    NormRegenKeys = normalize_keys(?REGEN_KEYS),
    maps:filter(
        fun(Key, _) ->
            not lists:member(hb_converge:key_to_binary(Key), NormRegenKeys)
        end,
        Map
    ).

%% @doc Return a map with only the keys that necessary, without those that can
%% be regenerated.
normalize(Map) ->
    NormalizedMap = normalize_keys(Map),
    FilteredMap = filter_default_tx_keys(NormalizedMap),
    maps:with(matchable_keys(FilteredMap), FilteredMap).

%% @doc Remove keys from a map that have the default values found in the tx
%% record.
filter_default_tx_keys(Map) ->
    DefaultsMap = default_tx_message(),
    maps:filter(
        fun(Key, Value) ->
            case maps:find(hb_converge:key_to_binary(Key), DefaultsMap) of
                {ok, Value} -> false;
                _ -> true
            end
        end,
        Map
    ).

%% @doc Get the normalized fields and default values of the tx record.
default_tx_message() ->
    normalize_keys(maps:from_list(default_tx_list())).

%% @doc Get the ordered list of fields and default values of the tx record.
default_tx_list() ->
    lists:zip(record_info(fields, tx), tl(tuple_to_list(#tx{}))).

%% @doc Serialize a message to a binary representation, either as JSON or the
%% binary format native to the message/bundles spec in use.
serialize(M) -> serialize(M, binary).
serialize(M, json) ->
    jiffy:encode(ar_bundles:item_to_json_struct(M));
serialize(M, binary) ->
    ar_bundles:serialize(message_to_tx(M)).

%% @doc Deserialize a message from a binary representation.
deserialize(B) -> deserialize(B, binary).
deserialize(J, json) ->
    {JSONStruct} = jiffy:decode(J),
    ar_bundles:json_struct_to_item(JSONStruct);
deserialize(B, binary) ->
    tx_to_message(ar_bundles:deserialize(B)).

%% @doc Internal helper to translate a message to its #tx record representation,
%% which can then be used by ar_bundles to serialize the message. We call the 
%% message's device in order to get the keys that we will be checkpointing. We 
%% do this recursively to handle nested messages. The base case is that we hit
%% a binary, which we return as is.
message_to_tx(Binary) when is_binary(Binary) ->
    % ar_bundles cannot serialize just a simple binary or get an ID for it, so
    % so we turn it into a TX record with a special tag, tx_to_message will
    % identify this tag and extract just the binary.
    #tx{
        tags= [{<<"Converge-Large-Binary">>, integer_to_binary(byte_size(Binary))}],
        data = Binary
    };
message_to_tx(TX) when is_record(TX, tx) -> TX;
message_to_tx(M) when is_map(M) ->
    % Get the keys that will be serialized, excluding private keys. Note that
    % we do not call hb_converge:resolve here because we want to include all keys
    % in the underlying map, except the private ones.
    Keys = maps:keys(M),
    % Translate the keys into a binary map. If a key has a value that is a map,
    % we recursively turn its children into messages. Notably, we do not simply
    % call message_to_tx/1 on the inner map because that would lead to adding
    % an extra layer of nesting to the data.
    %?event({message_to_tx, {keys, Keys}, {map, M}}),
    MsgKeyMap =
        maps:from_list(lists:flatten(
            lists:map(
                fun(Key) ->
                    case maps:find(Key, M) of
                        {ok, Map} when is_map(Map) ->
                            {Key, message_to_tx(Map)};
                        {ok, Value} when is_binary(Value) ->
                            {Key, Value};
                        {ok, Value} when is_atom(Value) or is_integer(Value) ->
                            ItemKey = hb_converge:key_to_binary(Key),
                            {Type, BinaryValue} = encode_value(Value),
                            [
                                {<<"Type:", ItemKey/binary>>, Type},
                                {ItemKey, BinaryValue}
                            ];
                        {ok, _} ->
                            []
                    end
                end,
                lists:filter(
                    fun(Key) ->
                        % Filter keys that the user could set directly, but
                        % should be regenerated when moving msg -> TX, as well
                        % as private keys.
                        not lists:member(Key, ?REGEN_KEYS) andalso
                            not hb_private:is_private(Key)
                    end,
                    Keys
                )
            )
        )),
    NormalizedMsgKeyMap = normalize_keys(MsgKeyMap),
    % Iterate through the default fields, replacing them with the values from
    % the message map if they are present.
    {RemainingMap, BaseTXList} = lists:foldl(
        fun({Field, Default}, {RemMap, Acc}) ->
            NormKey = hb_converge:key_to_binary(Field),
            case maps:find(NormKey, NormalizedMsgKeyMap) of
                error -> {RemMap, [Default | Acc]};
                {ok, Value} -> {maps:remove(NormKey, RemMap), [Value | Acc]}
            end
        end,
        {NormalizedMsgKeyMap, []},
        default_tx_list()
    ),
    % Rebuild the tx record from the new list of fields and values.
    TXWithoutTags = list_to_tuple([tx | lists:reverse(BaseTXList)]),
    % Calculate which set of the remaining keys will be used as tags.
    {Tags, RawDataItems} =
        lists:partition(
            fun({_Key, Value}) when byte_size(Value) =< ?MAX_TAG_VAL -> true;
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
            {Key, message_to_tx(Value)}
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
                TX#tx { data = DataItems#{ data => Data } };
            {Data, _} when is_binary(Data) ->
                TX#tx { data = DataItems#{ data => message_to_tx(Data) } }
        end,
    % ?event({prepared_tx_before_ids,
    % 	{tags, {explicit, TXWithData#tx.tags}},
    % 	{data, TXWithData#tx.data}
    % }),
    ar_bundles:reset_ids(ar_bundles:normalize(TXWithData));
message_to_tx(Other) ->
    ?event({unexpected_message_form, {explicit, Other}}),
    throw(invalid_tx).

%%% @doc Maps the native HyperBEAM Message 
%%% to an "HTTP" message. An HTTP Message has the following shape:
%%% 
%%% #{ 
%%%     headers => [
%%%         {<<"Example-Header">>, <<"Value">>}
%%%     ],
%%%     body: <<"Some body">>
%%% }
%%% 
%%% 
%%% For each HyperBEAM Message Key:
%%% 
%%% The Key will be ignored if:
%%% - The field is private (according to hb_private:is_private/1)
%%% - The field is one of ?REGEN_KEYS
%%%
%%% The Key will be mapped according to the following rules:
%%%     signatures -> {SignatureInput, Signature} header Tuples, each encoded as a Structured Field Dictionary
%%%     body:
%%%         - If a map, then every value is assumed another Msg to recursively transform, then combine in
%%%           a multipart response sent as the body
%%%         - Otherwise, make this the body of the Http Message
%%%     _ -> {Name/binary, Value/binary} header Tuple
%%%         - If the header is a VALID list of dictionary, then attempt to encode as a structured field headers
%%%         - header is considered valid if:
%%%             - Header size is <2KB
%%%             - Only a single depth, as only a single depth is supported by structured fields
%%%     
message_to_http(Msg) ->
    PublicMsg = hb_private:reset(Msg),
    MinimizedMsg = minimize(PublicMsg),
    NormalizedMsg = normalize_keys(MinimizedMsg),
    Http = lists:foldl(
        fun
            ({<<"signatures">>, Signatures}, Http) -> signatures_to_http(Http, Signatures);
            ({<<"body">>, Body}, Http) -> body_to_http(Http, Body);
            ({Name, Value}, Http) -> field_to_http(Http, {Name, Value})
        end,
        #{ headers => [], body => <<>> },
        maps:to_list(NormalizedMsg)    
    ),
    Http.

signatures_to_http(Http, Signatures) when is_map(Signatures) ->
    signatures_to_http(Http, maps:to_list(Signatures));
signatures_to_http(Http, Signatures) when is_list(Signatures) ->
    {SfSigInputs, SfSigs} = lists:foldl(
        fun ({SigName, SignatureMap = #{ inputs := Inputs, signature := Signature }}, {SfSigInputs, SfSigs}) ->
            NextSigInput = hb_http_signature:sf_signature_params(Inputs, SignatureMap),
            NextSig = hb_http_signature:sf_signature(Signature),
            NextName = hb_converge:key_to_binary(SigName),
            {
                [{NextName, NextSigInput} | SfSigInputs],
                [{NextName, NextSig} | SfSigs] 
            }
        end,
        % Start with empty Structured Field Dictionaries
        {[], []},
        Signatures
    ),
    Headers = maps:get(headers, Http),
    % Upsert these headers to ensure they are not duplicated
    H1 = lists:keystore(<<"Signature">>, 1, Headers, {<<"Signature">>, hb_structured_fields:dictionary(SfSigs)}),
    NewHeaders = lists:keystore(<<"Signature-Input">>, 1, H1, {<<"Signature-Input">>, hb_http_structured_fields:dictionary(SfSigInputs)}),
    maps:put(headers, NewHeaders, Http).

field_to_http(Http, {Name, Map}) when is_map(Map) ->
    {not_implemented, Map};
field_to_http(Http, {Name, List}) when is_list(List) ->
    {not_implemented, List};
field_to_http(Http, {Name, Value}) ->
    NormalizedName = hb_converge:key_to_binary(Name),
    NormalizedValue = hb_converge:key_to_binary(Value),
    Headers = maps:get(headers, Http),
    NewHeaders = lists:append(Headers, [{NormalizedName, hb_structured_fields:dictionary(NormalizedValue)}]),
    maps:put(headers, NewHeaders, Http).

body_to_http(Http, Body) when is_map(Body)->
    % recursively call message_to_http for each Msg and
    % and then programattically join in the body using the Boundary,
    % according to multipart/form-data semantics
    ?no_prod("What should the Boundary be?"),
    Boundary = base64:encode(crypto:strong_rand_bytes(32)),
    Parts = maps:map(
        fun (Key, Msg) ->
            ?no_prod("What should the name be?"),
            NormalizedKey = hb_converge:key_to_binary(Key),
            #{ headers := SubHeaders, body := SubBody } = message_to_http(Msg),
            % Serialize the headers, to be included in the part of the multipart response
            SerializedHeaders = lists:foldl(
                fun ({Name, Value}, Acc) ->
                    <<Acc/binary, "\n", Name/binary, ": ", Value/binary>>
                end,
                <<"Content-Disposition: form-data; name=", NormalizedKey/binary>>,
                SubHeaders
            ),
            % Content-Disposition: form-data; name="fgserbvrebserfe"
            % Content-Type: image/png
            % 
            % <body>
            <<SerializedHeaders/binary, <<"\n\n">>, SubBody/binary>>
        end,
        Body 
    ),
    JoinedBody = lists:join(<<<<"--">>, Boundary/binary>>, Parts),
    Headers = maps:get(headers, Http),
    % Upsert a Content-Type header to make the Msg multipart
    NewHeaders = lists:keystore(<<"Content-Type">>, 1, Headers,
        {<<"Content-Type">>, <<"multipart/form-data; boundary=", Boundary/binary>>}
    ),
    body_to_http(
        maps:put(headers, NewHeaders, Http),
        iolist_to_binary(JoinedBody)    
    );
body_to_http(Http, Body) when is_binary(Body) ->
    maps:merge(Http, #{ body => Body }).

%% @doc Convert non-binary values to binary for serialization.
decode_value(decimal, Value) ->
    {item, Number, _} = hb_http_structured_fields:parse_item(Value),
    Number;
decode_value(atom, Value) ->
    {item, {string, AtomString}, _} =
        hb_http_structured_fields:parse_item(Value),
    binary_to_existing_atom(AtomString, latin1);
decode_value(OtherType, Value) ->
    ?event({unexpected_type, OtherType, Value}),
    throw({unexpected_type, OtherType, Value}).

%% @doc Convert a term to a typed key.
to_typed_keys({Key, Value}) ->
    to_typed_keys(Key, Value).
to_typed_keys(Key, Value) when is_binary(Value) ->
    [{Key, Value}];
to_typed_keys(Key, Value) when is_map(Value) ->
    [{Key, message_to_tx(Value)}];
to_typed_keys(Key, Value) ->
    ItemKey = hb_converge:key_to_binary(Key),
    {Type, BinaryValue} = encode_value(Value),
    [
        {<<"Type:", ItemKey/binary>>, Type},
        {ItemKey, BinaryValue}
    ].

%% @doc Convert a term to a binary representation, emitting its type for
%% serialization as a separate tag.
encode_value(Value) when is_integer(Value) ->
    ?no_prod("Non-standardized type conversion invoked."),
    [Encoded, _] = hb_http_structured_fields:item({item, Value, []}),
    {<<"decimal">>, Encoded};
encode_value(Value) when is_atom(Value) ->
    ?no_prod("Non-standardized type conversion invoked."),
    [EncodedIOList, _] =
        hb_http_structured_fields:item(
            {item, {string, atom_to_binary(Value, latin1)}, []}),
    Encoded = list_to_binary(EncodedIOList),
    {<<"atom">>, Encoded};
encode_value(Value) ->
    Value.

%% @doc Convert a #tx record into a message map recursively.
tx_to_message(Binary) when is_binary(Binary) -> Binary;
tx_to_message(TX) when is_record(TX, tx) ->
    case lists:keyfind(<<"Converge-Large-Binary">>, 1, TX#tx.tags) of
        false ->
            do_tx_to_message(TX);
        {_, _Size} ->
            TX#tx.data
    end.
do_tx_to_message(RawTX) ->
    % Ensure the TX is fully deserialized.
    TX = ar_bundles:deserialize(ar_bundles:normalize(RawTX)),
    % We need to generate a map from each field of the tx record.
    % Get the raw fields and values of the tx record and pair them.
    Fields = record_info(fields, tx),
    Values = tl(tuple_to_list(TX)),
    TXKeyVals = lists:zip(Fields, Values),
    % Convert the list of key-value pairs into a map.
    UnfilteredTXMap = maps:from_list(TXKeyVals),
    TXMap = maps:with(?TX_KEYS, UnfilteredTXMap),
    {TXTagsUnparsed, FilteredTags} = lists:partition(
        fun({Key, _}) ->
            not lists:any(
                fun(FilterKey) ->
                    case binary:longest_common_prefix([Key, FilterKey]) of
                        Length when Length == byte_size(FilterKey) ->
                            true;
                        _ ->
                            false
                    end
                end,
                ?FILTERED_TAGS
            );
            (_) -> true
        end,
        TX#tx.tags
    ),
    % Parse tags that have a "Type:" prefix.
    TagTypes =
        [
            {Name, binary_to_existing_atom(Value, latin1)}
        ||
            {<<"Type:", Name/binary>>, Value} <- FilteredTags
        ],
    TXTags =
        lists:map(
            fun({Name, BinaryValue}) ->
                case lists:keyfind(Name, 1, TagTypes) of
                    false -> {Name, BinaryValue};
                    {_, Type} -> {Name, decode_value(Type, BinaryValue)}
                end
            end,
            TXTagsUnparsed
        ),
    % Next, merge the tags into the map.
    MapWithoutData = maps:merge(TXMap, maps:from_list(TXTags)),
    % Finally, merge the data into the map.
    normalize(
        case TX#tx.data of
            Data when is_map(Data) ->
                % If the data is a map, we need to recursively turn its children
                % into messages from their tx representations.
                ?event({merging_map_and_data, MapWithoutData, Data}),
                maps:merge(
                    MapWithoutData,
                    maps:map(
                        fun(_, InnerValue) ->
                            tx_to_message(InnerValue)
                        end,
                        Data
                    )
                );
            Data when Data == ?DEFAULT_DATA ->
                MapWithoutData;
            Data when is_binary(Data) ->
                MapWithoutData#{ data => Data };
            Data ->
                ?event({unexpected_data_type, {explicit, Data}}),
                ?event({was_processing, {explicit, TX}}),
                throw(invalid_tx)
        end
    ).

%%% Tests

basic_map_to_tx_test() ->
    Msg = #{ normal_key => <<"NORMAL_VALUE">> },
    TX = message_to_tx(Msg),
    ?assertEqual([{<<"normal_key">>, <<"NORMAL_VALUE">>}], TX#tx.tags).

%% @doc Test that the filter_default_tx_keys/1 function removes TX fields
%% that have the default values found in the tx record, but not those that
%% have been set by the user.
default_tx_keys_removed_test() ->
    TX = #tx { unsigned_id = << 1:256 >>, last_tx = << 2:256 >> },
    TXMap = #{
        unsigned_id => TX#tx.unsigned_id,
        last_tx => TX#tx.last_tx,
        <<"owner">> => TX#tx.owner,
        <<"target">> => TX#tx.target,
        data => TX#tx.data
    },
    FilteredMap = filter_default_tx_keys(TXMap),
    ?assertEqual(<< 1:256 >>, maps:get(unsigned_id, FilteredMap)),
    ?assertEqual(<< 2:256 >>, maps:get(last_tx, FilteredMap, not_found)),
    ?assertEqual(not_found, maps:get(<<"owner">>, FilteredMap, not_found)),
    ?assertEqual(not_found, maps:get(<<"target">>, FilteredMap, not_found)).

minimization_test() ->
    Msg = #{
        unsigned_id => << 1:256 >>,
        <<"id">> => << 2:256 >>
    },
    MinimizedMsg = minimize(Msg),
    ?event({minimized, MinimizedMsg}),
    ?assertEqual(0, maps:size(MinimizedMsg)).

%% @doc Test that we can convert a message into a tx record and back.
single_layer_message_to_tx_test() ->
    Msg = #{
        last_tx => << 2:256 >>,
        owner => << 3:4096 >>,
        target => << 4:256 >>,
        data => <<"DATA">>,
        <<"Special-Key">> => <<"SPECIAL_VALUE">>
    },
    TX = message_to_tx(Msg),
    ?event({tx_to_message, {msg, Msg}, {tx, TX}}),
    ?assertEqual(maps:get(last_tx, Msg), TX#tx.last_tx),
    ?assertEqual(maps:get(owner, Msg), TX#tx.owner),
    ?assertEqual(maps:get(target, Msg), TX#tx.target),
    ?assertEqual(maps:get(data, Msg), TX#tx.data),
    ?assertEqual({<<"Special-Key">>, <<"SPECIAL_VALUE">>},
        lists:keyfind(<<"Special-Key">>, 1, TX#tx.tags)).

%% @doc Test that we can convert a #tx record into a message correctly.
single_layer_tx_to_message_test() ->
    TX = #tx {
        unsigned_id = << 1:256 >>,
        last_tx = << 2:256 >>,
        owner = << 3:256 >>,
        target = << 4:256 >>,
        data = <<"DATA">>,
        tags = [{<<"special_key">>, <<"SPECIAL_KEY">>}]
    },
    Msg = tx_to_message(TX),
    ?assertEqual(maps:get(<<"special_key">>, Msg), <<"SPECIAL_KEY">>),
    ?assertEqual(<< "DATA">>, maps:get(<<"data">>, Msg)),
    ?assertEqual(<< 2:256 >>, maps:get(<<"last_tx">>, Msg)),
    ?assertEqual(<< 3:256 >>, maps:get(<<"owner">>, Msg)),
    ?assertEqual(<< 4:256 >>, maps:get(<<"target">>, Msg)).

%% @doc Test that the message matching function works.
match_test() ->
    Msg = #{ a => 1, b => 2 },
    TX = message_to_tx(Msg),
    Msg2 = tx_to_message(TX),
    ?assert(match(Msg, Msg2)).

%% @doc Test that two txs match. Note: This function uses tx_to_message/1
%% underneath, which (depending on the test) could potentially lead to false
%% positives.
txs_match(TX1, TX2) ->
    match(tx_to_message(TX1), tx_to_message(TX2)).

%% @doc Structured field parsing tests.
structured_field_atom_parsing_test() ->
    Msg = #{ highly_unusual_http_header => highly_unusual_value },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).

structured_field_decimal_parsing_test() ->
    Msg = #{ integer_field => 1234567890 },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).

binary_to_binary_test() ->
    % Serialization must be able to turn a raw binary into a TX, then turn
    % that TX back into a binary and have the result match the original.
    Bin = <<"THIS IS A BINARY, NOT A NORMAL MESSAGE">>,
    Msg = message_to_tx(Bin),
    ?assertEqual(Bin, tx_to_message(Msg)).

%% @doc Test that the data field is correctly managed when we have multiple
%% uses for it (the 'data' key itself, as well as keys that cannot fit in
%% tags).
message_with_large_keys_test() ->
    Msg = #{
        <<"normal_key">> => <<"normal_value">>,
        <<"large_key">> => << 0:((1 + ?MAX_TAG_VAL) * 8) >>,
        <<"another_large_key">> => << 0:((1 + ?MAX_TAG_VAL) * 8) >>,
        <<"another_normal_key">> => <<"another_normal_value">>
    },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).

%% @doc Check that large keys and data fields are correctly handled together.
nested_message_with_large_keys_and_data_test() ->
    Msg = #{
        <<"normal_key">> => <<"normal_value">>,
        <<"large_key">> => << 0:(?MAX_TAG_VAL * 16) >>,
        <<"another_large_key">> => << 0:(?MAX_TAG_VAL * 16) >>,
        <<"another_normal_key">> => <<"another_normal_value">>,
        data => <<"Hey from the data field!">>
    },
    TX = message_to_tx(Msg),
    Msg2 = tx_to_message(TX),
    ?event({matching, {input, Msg}, {tx, TX}, {output, Msg2}}),
    ?assert(match(Msg, Msg2)).

simple_nested_message_test() ->
    Msg = #{
        a => <<"1">>,
        nested => #{ <<"b">> => <<"1">> },
        c => <<"3">>
    },
    TX = message_to_tx(Msg),
    Msg2 = tx_to_message(TX),
    ?event({matching, {input, Msg}, {output, Msg2}}),
    ?assert(
        match(
            Msg,
            Msg2
        )
    ).

%% @doc Test that the data field is correctly managed when we have multiple
%% uses for it (the 'data' key itself, as well as keys that cannot fit in
%% tags).
nested_message_with_large_data_test() ->
    Msg = #{
        <<"tx_depth">> => <<"outer">>,
        data => #{
            <<"tx_map_item">> =>
                #{
                    <<"tx_depth">> => <<"inner">>,
                    <<"large_data_inner">> => << 0:((1 + ?MAX_TAG_VAL) * 8) >>
                },
            <<"large_data_outer">> => << 0:((1 + ?MAX_TAG_VAL) * 8) >>
        }
    },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).

%% @doc Test that we can convert a 3 layer nested message into a tx record and back.
deeply_nested_message_with_data_test() ->
    Msg = #{
        <<"tx_depth">> => <<"outer">>,
        data => #{
            <<"tx_map_item">> =>
                #{
                    <<"tx_depth">> => <<"inner">>,
                    data => #{
                        <<"tx_depth">> => <<"innermost">>,
                        data => <<"DATA">>
                    }
                }
        }
    },
    ?assert(match(Msg, tx_to_message(message_to_tx(Msg)))).


nested_structured_fields_test() ->
    NestedMsg = #{ a => #{ b => 1 } },
    ?assert(
        match(
            NestedMsg,
            tx_to_message(message_to_tx(NestedMsg))
        )
    ).

nested_message_with_large_keys_test() ->
    Msg = #{
        a => <<"1">>,
        long_data => << 0:((1 + ?MAX_TAG_VAL) * 8) >>,
        nested => #{ <<"b">> => <<"1">> },
        c => <<"3">>
    },
    ResMsg = tx_to_message(message_to_tx(Msg)),
    ?event({matching, {input, Msg}, {output, ResMsg}}),
    ?assert(match(Msg, ResMsg)).

%% @doc Test that we can convert a signed tx into a message and back.
signed_tx_to_message_and_back_test() ->
    TX = #tx {
        data = <<"TEST_DATA">>,
        tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}]
    },
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    ?assert(ar_bundles:verify_item(SignedTX)),
    SignedMsg = tx_to_message(SignedTX),
    SignedTX2 = message_to_tx(SignedMsg),
    ?assert(ar_bundles:verify_item(SignedTX2)).

signed_deep_tx_serialize_and_deserialize_test() ->
    TX = #tx {
        tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}],
        data = #{
            <<"NESTED_TX">> =>
                #tx {
                    data = <<"NESTED_DATA">>,
                    tags = [{<<"NESTED_KEY">>, <<"NESTED_VALUE">>}]
                }
        }
    },
    SignedTX = ar_bundles:deserialize(
        ar_bundles:sign_item(TX, hb:wallet())
    ),
    ?assert(ar_bundles:verify_item(SignedTX)),
    SerializedTX = serialize(SignedTX),
    DeserializedTX = deserialize(SerializedTX),
    ?assert(
        match(
            tx_to_message(SignedTX),
            DeserializedTX
        )
    ).

calculate_unsigned_message_id_test() ->
    Msg = #{
        data => <<"DATA">>,
        <<"special_key">> => <<"SPECIAL_KEY">>
    },
    UnsignedTX = message_to_tx(Msg),
    UnsignedMessage = tx_to_message(UnsignedTX),
    ?assertEqual(
        hb_util:encode(ar_bundles:id(UnsignedTX, unsigned)),
        hb_util:id(UnsignedMessage, unsigned)
    ).

sign_serialize_deserialize_verify_test() ->
    Msg = #{
        data => <<"DATA">>,
        <<"special_key">> => <<"SPECIAL_KEY">>
    },
    TX = message_to_tx(Msg),
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    ?assert(ar_bundles:verify_item(SignedTX)),
    SerializedMsg = serialize(SignedTX),
    DeserializedMsg = deserialize(SerializedMsg),
    DeserializedTX = message_to_tx(DeserializedMsg),
    ?assert(ar_bundles:verify_item(DeserializedTX)).

unsigned_id_test() ->
    UnsignedTX = ar_bundles:normalize(#tx { data = <<"TEST_DATA">> }),
    UnsignedMessage = tx_to_message(UnsignedTX),
    ?assertEqual(
        hb_util:encode(ar_bundles:id(UnsignedTX, unsigned)),
        hb_util:id(UnsignedMessage, unsigned)
    ).

signed_id_test_disabled() ->
    TX = #tx {
        data = <<"TEST_DATA">>,
        tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}]
    },
    SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
    ?assert(ar_bundles:verify_item(SignedTX)),
    SignedMsg = tx_to_message(SignedTX),
    ?assertEqual(
        hb_util:encode(ar_bundles:id(SignedTX, signed)),
        hb_util:id(SignedMsg, signed)
    ).