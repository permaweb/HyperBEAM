-module(dev_codec_structured).
-moduledoc """
A device implementing the codec interface (to/1, from/1) for 
HyperBEAM's internal, richly typed message format.

This format mirrors HTTP Structured Fields, aside from its limitations of 
compound type depths, as well as limited floating point representations.

As with all AO-Core codecs, its target format (the format it expects to 
receive in the `to/1` function, and give in `from/1`) is TABM.

For more details, see the HTTP Structured Fields (RFC-9651) specification.
""".
-export([to/1, from/1, commit/3, committed/3, verify/3]).
-export([decode_value/2, encode_value/1, implicit_keys/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Route signature functions to the `dev_codec_httpsig` module
commit(Msg, Req, Opts) -> dev_codec_httpsig:commit(Msg, Req, Opts).
verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).
committed(Msg, Req, Opts) -> dev_codec_httpsig:committed(Msg, Req, Opts).

-doc """
Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).
""".
from(Bin) when is_binary(Bin) -> Bin;
from(Msg) when is_map(Msg) ->
    NormKeysMap = hb_ao:normalize_keys(Msg),
    {Types, Values} = lists:foldl(
        fun (Key, {Types, Values}) ->
            case maps:find(Key, NormKeysMap) of
                {ok, <<>>} ->
                    BinKey = hb_ao:normalize_key(Key),
                    {[{BinKey, <<"empty-binary">>} | Types], Values};
                {ok, []} ->
                    BinKey = hb_ao:normalize_key(Key),
                    {[{BinKey, <<"empty-list">>} | Types], Values};
                {ok, EmptyMap} when ?IS_EMPTY_MESSAGE(EmptyMap) ->
                    BinKey = hb_ao:normalize_key(Key),
                    {[{BinKey, <<"empty-message">>} | Types], Values};
                {ok, Value} when is_binary(Value) ->
                    {Types, [{Key, Value} | Values]};
                {ok, Map} when is_map(Map) ->
                    {Types, [{Key, from(Map)} | Values]};
                {ok, MsgList = [Msg1|_]} when is_map(Msg1) or is_list(Msg1) ->
                    % We have a list of maps. Convert to a numbered map and
                    % recurse.
                    BinKey = hb_ao:normalize_key(Key),
                    % Convert the list of maps into a numbered map and recurse
                    NumberedMap = from(hb_ao:normalize_keys(MsgList)),
                    {[{BinKey, <<"list">>} | Types], [{BinKey, NumberedMap} | Values]};
                {ok, Value} when
                        is_atom(Value) or is_integer(Value)
                        or is_list(Value) or is_float(Value) ->
                    BinKey = hb_ao:normalize_key(Key),
                    ?event({encode_value, Value}),
                    {Type, BinValue} = encode_value(Value),
                    {[{BinKey, Type} | Types], [{BinKey, BinValue} | Values]};
                {ok, {resolve, Operations}} when is_list(Operations) ->
                    {Types, [{Key, {resolve, Operations}} | Values]};
                {ok, Function} when is_function(Function) ->
                    % We have a function. Convert to a binary string representation.
                    % This value is unique to the specific byte code of the module
                    % that generated the function, so it is reproducible (assuming
                    % the same module is used) but cannot be used to resolve the
                    % function at runtime.
                    FuncRef = list_to_binary(erlang:fun_to_list(Function)),
                    {Types, [{Key, FuncRef} | Values]};
                {ok, _UnsupportedValue} ->
                    {Types, Values}
            end
        end,
        {[],[]},
        lists:filter(
            fun(Key) ->
                % Filter keys that the user could set directly, but
                % should be regenerated when moving msg -> TX, as well
                % as private keys.
                not lists:member(Key, ?REGEN_KEYS) andalso
                    not hb_private:is_private(Key)
            end,
            hb_util:to_sorted_keys(NormKeysMap)
        )
    ),
    % Encode the AoTypes as a structured dictionary
    % And include as a field on the produced TABM
    WithTypes =
        case Types of 
            [] -> Values;
            T ->
                AoTypes = iolist_to_binary(hb_structured_fields:dictionary(
                    lists:map(
                        fun({Key, Value}) ->
                            {ok, Item} = hb_structured_fields:to_item(Value),
                            {hb_escape:encode(Key), Item}
                        end,
                        lists:reverse(T)
                    )
                )),
                [{<<"ao-types">>, AoTypes} | Values]
        end,
    maps:from_list(lists:reverse(WithTypes));
from(Other) -> hb_path:to_binary(Other).

-doc """
Convert a TABM into a native HyperBEAM message.
""".
to(Bin) when is_binary(Bin) -> Bin;
to(TABM0) ->
    Types = case maps:get(<<"ao-types">>, TABM0, <<>>) of
        <<>> -> #{};
        Bin -> parse_ao_types(Bin)
    end,
    % "empty values" will each have a type, but no corresponding value
    % (because its empty)
    % 
    % So we first loop through Types and map over the each empty type to its
    % equivalent empty value
    TABM1 = maps:from_list(
        maps:fold(
            fun (Key, <<"empty-binary">>, Acc) -> [{Key, <<>>} | Acc];
                (Key, <<"empty-list">>, Acc) -> [{Key, []} | Acc];
                (Key, <<"empty-message">>, Acc) -> [{Key, #{}} | Acc];
                (_Key, _Value, Acc) -> Acc
            end,
            [],
            Types
        )
    ),
    % 1. Remove 'ao-types' field
    % 2. Decode any binary values that have a type;
    % 3. Recursively decode any maps that we encounter;
    % 4. Return the remaining keys and values as a map.
    hb_message:filter_default_keys(maps:fold(
        fun (<<"ao-types">>, _Value, Acc) -> Acc;
        (RawKey, BinValue, Acc) when is_binary(BinValue) ->
            case maps:find(hb_ao:normalize_key(RawKey), Types) of
                % The value is a binary, no parsing required
                error -> Acc#{ RawKey => BinValue };
                % Parse according to its type
                {ok, Type} ->
                    Decoded = decode_value(Type, BinValue),
                    Acc#{ RawKey => Decoded }
            end;
        (RawKey, ChildTABM, Acc) when is_map(ChildTABM) ->
            % Decode the child TABM
            ChildDecoded = to(ChildTABM),
            Acc#{
                RawKey =>
                    case maps:find(RawKey, Types) of
                        error ->
                            % The value is a map, so we return it as is
                            ChildDecoded;
                        {ok, <<"list">>} ->
                            % The child is a list of maps, so we need to convert the
                            % map into a list, while maintaining the correct order
                            % of the keys
                            hb_util:message_to_ordered_list(ChildDecoded)
                    end
            };
        (RawKey, Value, Acc) ->
            % We encountered a key that already has a converted type.
            % We can just return it as is.
            Acc#{ RawKey => Value }
        end,
        TABM1,
        TABM0
    )).

-doc """
Parse the `ao-types` field of a TABM and return a map of keys and their
types
""".
parse_ao_types(Msg) when is_map(Msg) ->
    parse_ao_types(maps:get(<<"ao-types">>, Msg, <<>>));
parse_ao_types(Bin) ->
    maps:from_list(
        lists:map(
            fun({Key, {item, {_, Value}, _}}) ->
                {hb_escape:decode(Key), Value}
            end,
            hb_structured_fields:parse_dictionary(Bin)    
        )
    ).

-doc """
Find the implicit keys of a TABM.
""".
implicit_keys(Req) ->
    maps:keys(
        maps:filtermap(
            fun(_Key, Val = <<"empty-", _/binary>>) -> {true, Val};
            (_Key, _Val) -> false
            end,
            parse_ao_types(Req)
        )
    ).

-doc """
Convert a term to a binary representation, emitting its type for
serialization as a separate tag.
""".
encode_value(Value) when is_integer(Value) ->
    [Encoded, _] = hb_structured_fields:item({item, Value, []}),
    {<<"integer">>, Encoded};
encode_value(Value) when is_float(Value) ->
    ?no_prod("Must use structured field representation for floats!"),
    {<<"float">>, float_to_binary(Value)};
encode_value(Value) when is_atom(Value) ->
    [EncodedIOList, _] =
        hb_structured_fields:item(
            {item, {string, atom_to_binary(Value, latin1)}, []}),
    Encoded = list_to_binary(EncodedIOList),
    {<<"atom">>, Encoded};
encode_value(Values) when is_list(Values) ->
    EncodedValues =
        lists:map(
            fun(Bin) when is_binary(Bin) -> {item, {string, Bin}, []};
               (Item) ->
                {RawType, Encoded} = encode_value(Item),
                Type = hb_ao:normalize_key(RawType),
                {
                    item,
                    {
                        string,
                        <<
                            "(ao-type-", Type/binary, ") ",
                            Encoded/binary
                        >>
                    },
                    []
                }
            end,
            Values
        ),
    EncodedList = hb_structured_fields:list(EncodedValues),
    {<<"list">>, iolist_to_binary(EncodedList)};
encode_value(Value) when is_binary(Value) ->
    {<<"binary">>, Value};
encode_value(Value) ->
    Value.

-doc """
Convert non-binary values to binary for serialization.
""".
decode_value(Type, Value) when is_list(Type) ->
    decode_value(list_to_binary(Type), Value);
decode_value(Type, Value) when is_binary(Type) ->
    decode_value(
        binary_to_existing_atom(
            list_to_binary(string:to_lower(binary_to_list(Type))),
            latin1
        ),
        Value
    );
decode_value(integer, Value) ->
    {item, Number, _} = hb_structured_fields:parse_item(Value),
    Number;
decode_value(float, Value) ->
    binary_to_float(Value);
decode_value(atom, Value) ->
    {item, {_, AtomString}, _} =
        hb_structured_fields:parse_item(Value),
    binary_to_existing_atom(AtomString);
decode_value(list, Value) ->
    lists:map(
        fun({item, {string, <<"(ao-type-", Rest/binary>>}, _}) ->
            [Type, Item] = binary:split(Rest, <<") ">>),
            decode_value(Type, Item);
           ({item, Item, _}) -> hb_structured_fields:from_bare_item(Item)
        end,
        hb_structured_fields:parse_list(iolist_to_binary(Value))
    );
decode_value(map, Value) ->
    maps:from_list(
        lists:map(
            fun({Key, {item, Item, _}}) ->
                ?event({decoded_item, {explicit, Key}, Item}),
                {Key, hb_structured_fields:from_bare_item(Item)}
            end,
            hb_structured_fields:parse_dictionary(iolist_to_binary(Value))
        )
    );
decode_value(BinType, Value) when is_binary(BinType) ->
    decode_value(
        list_to_existing_atom(
            string:to_lower(
                binary_to_list(BinType)
            )
        ),
        Value
    );
decode_value(OtherType, Value) ->
    ?event({unexpected_type, OtherType, Value}),
    throw({unexpected_type, OtherType, Value}).

%%% Tests

list_encoding_test() ->
    % Test that we can encode and decode a list of integers.
    {<<"list">>, Encoded} = encode_value(List1 = [1, 2, 3]),
    Decoded = decode_value(list, Encoded),
    ?assertEqual(List1, Decoded),
    % Test that we can encode and decode a list of binaries.
    {<<"list">>, Encoded2} = encode_value(List2 = [<<"1">>, <<"2">>, <<"3">>]),
    ?assertEqual(List2, decode_value(list, Encoded2)),
    % Test that we can encode and decode a mixed list.
    {<<"list">>, Encoded3} = encode_value(List3 = [1, <<"2">>, 3]),
    ?assertEqual(List3, decode_value(list, Encoded3)).