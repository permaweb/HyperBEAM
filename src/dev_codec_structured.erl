%%% @doc A device implementing the codec interface (to/1, from/1) for 
%%% HyperBEAM's internal, richly typed message format.
%%% 
%%% This format mirrors HTTP Structured Fields, aside from its limitations of 
%%% compound type depths, as well as limited floating point representations.
%%% 
%%% As with all Converge codecs, its target format (the format it expects to 
%%% receive in the `to/1' function, and give in `from/1') is TABM.
%%% 
%%% For more details, see the HTTP Structured Fields (RFC-9651) specification.
-module(dev_codec_structured).
-export([to/1, from/1, attest/3, attested/3, verify/3]).
-export([decode_value/2, encode_value/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Route signature functions to the `dev_codec_httpsig' module
attest(Msg, Req, Opts) -> dev_codec_httpsig:attest(Msg, Req, Opts).
verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).
attested(Msg, Req, Opts) -> dev_codec_httpsig:attested(Msg, Req, Opts).

%% @doc Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).
from(Bin) when is_binary(Bin) -> Bin;
from(Msg) when is_map(Msg) ->
    {Types, Values} = lists:foldl(
        fun (Key, {Types, Values}) ->
            case maps:find(Key, Msg) of
                {ok, <<>>} ->
                    BinKey = hb_converge:normalize_key(Key),
                    {[{BinKey, <<"empty-binary">>} | Types], Values};
                {ok, Value} when is_binary(Value) ->
                    {Types, [{Key, Value} | Values]};
                {ok, Map} when is_map(Map) ->
                    {Types, [{Key, from(Map)} | Values]};
                {ok, Msgs = [Msg1|_]} when is_map(Msg1) ->
                    % We have a list of maps. Convert to a numbered map and
                    % recurse.
                    {Types, [{Key, from(hb_converge:normalize_keys(Msgs))} | Values]};
                {ok, []} ->
                    BinKey = hb_converge:normalize_key(Key),
                    {[{BinKey, <<"empty-list">>} | Types], Values};
                {ok, Value} when
                        is_atom(Value) or is_integer(Value)
                        or is_list(Value) or is_float(Value) ->
                    BinKey = hb_converge:normalize_key(Key),
                    {Type, BinValue} = encode_value(Value),
                    {[{BinKey, Type} | Types], [{BinKey, BinValue} | Values]};
                {ok, {resolve, Operations}} when is_list(Operations) ->
                    {Types, [{Key, {resolve, Operations}} | Values]};
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
            maps:keys(Msg)
        )
    ),
    % Encode the AoTypes as a structured dictionary
    % And include as a field on the produced TABM
    WithTypes = case Types of 
        [] -> Values;
        T ->
            AoTypes = iolist_to_binary(dev_codec_structured_conv:dictionary(
                lists:map(
                    fun({Key, Value}) ->
                        {ok, Item} = dev_codec_structured_conv:to_item(Value),
                        {Key, Item}
                    end,
                    lists:reverse(T)
                )    
            )),
            [{<<"ao-types">>, AoTypes} | Values]
    end,
    maps:from_list(lists:reverse(WithTypes));
from(Other) -> hb_path:to_binary(Other).

%% @doc Convert a TABM into a native HyperBEAM message.
to(Bin) when is_binary(Bin) -> Bin;
to(TABM0) ->
    Types = case maps:get(<<"ao-types">>, TABM0, <<>>) of
        <<>> -> #{};
        Bin -> maps:from_list(
            lists:map(
                fun({Key, {item, {_, Value}, _}}) -> {Key, Value} end,
                dev_codec_structured_conv:parse_dictionary(Bin)    
            )
        )
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
            case maps:find(hb_converge:normalize_key(RawKey), Types) of
                % The value is a binary, no parsing required
                error -> Acc#{ RawKey => BinValue };
                % Parse according to its type
                {ok, Type} ->
                    Decoded = decode_value(Type, BinValue),
                    Acc#{ RawKey => Decoded }
            end;
        (RawKey, ChildTABM, Acc) when is_map(ChildTABM) ->
            Acc#{ RawKey => to(ChildTABM)};
        (RawKey, Value, Acc) ->
            % We encountered a key that already has a converted type.
            % We can just return it as is.
            Acc#{ RawKey => Value }
        end,
        TABM1,
        TABM0
    )).

%% @doc Convert a term to a binary representation, emitting its type for
%% serialization as a separate tag.
encode_value(Value) when is_integer(Value) ->
    [Encoded, _] = dev_codec_structured_conv:item({item, Value, []}),
    {<<"integer">>, Encoded};
encode_value(Value) when is_float(Value) ->
    ?no_prod("Must use structured field representation for floats!"),
    {<<"float">>, float_to_binary(Value)};
encode_value(Value) when is_atom(Value) ->
    [EncodedIOList, _] =
        dev_codec_structured_conv:item(
            {item, {string, atom_to_binary(Value, latin1)}, []}),
    Encoded = list_to_binary(EncodedIOList),
    {<<"atom">>, Encoded};
encode_value(Values) when is_list(Values) ->
    EncodedValues =
        lists:map(
            fun(Bin) when is_binary(Bin) -> {item, {string, Bin}, []};
               (Item) ->
                {RawType, Encoded} = encode_value(Item),
                Type = hb_converge:normalize_key(RawType),
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
    EncodedList = dev_codec_structured_conv:list(EncodedValues),
    {<<"list">>, iolist_to_binary(EncodedList)};
encode_value(Value) when is_binary(Value) ->
    {<<"binary">>, Value};
encode_value(Value) ->
    Value.

%% @doc Convert non-binary values to binary for serialization.
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
    {item, Number, _} = dev_codec_structured_conv:parse_item(Value),
    Number;
decode_value(float, Value) ->
    binary_to_float(Value);
decode_value(atom, Value) ->
    {item, {string, AtomString}, _} =
        dev_codec_structured_conv:parse_item(Value),
    binary_to_existing_atom(AtomString);
decode_value(list, Value) ->
    lists:map(
        fun({item, {string, <<"(ao-type-", Rest/binary>>}, _}) ->
            [Type, Item] = binary:split(Rest, <<") ">>),
            decode_value(Type, Item);
           ({item, Item, _}) -> dev_codec_structured_conv:from_bare_item(Item)
        end,
        dev_codec_structured_conv:parse_list(iolist_to_binary(Value))
    );
decode_value(map, Value) ->
    maps:from_list(
        lists:map(
            fun({Key, {item, Item, _}}) ->
                ?event({decoded_item, {explicit, Key}, Item}),
                {Key, dev_codec_structured_conv:from_bare_item(Item)}
            end,
            dev_codec_structured_conv:parse_dictionary(iolist_to_binary(Value))
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