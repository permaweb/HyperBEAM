%%% @doc A device implementing the codec interface (to/1, from/1) for
%%% HyperBEAM's internal, richly typed message format.
%%%
%%% This format mirrors HTTP Structured Fields, aside from its limitations of
%%% compound type depths, as well as limited floating point representations.
%%%
%%% As with all AO-Core codecs, its target format (the format it expects to
%%% receive in the `to/1' function, and give in `from/1') is TABM.
%%%
%%% For more details, see the HTTP Structured Fields (RFC-9651) specification.
-module(dev_codec_structured).
-export([to/3, from/3, commit/3, verify/3]).
-export([encode_ao_types/2, decode_ao_types/2, is_list_from_ao_types/2]).
-export([decode_value/2, encode_value/1, implicit_keys/2]).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").


%%% Route signature functions to the `dev_codec_httpsig' module
commit(Msg, Req, Opts) -> dev_codec_httpsig:commit(Msg, Req, Opts).


verify(Msg, Req, Opts) -> dev_codec_httpsig:verify(Msg, Req, Opts).


%% @doc Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).
from(Bin, _Req, _Opts) when is_binary(Bin) -> {ok, Bin};
from(List, Req, Opts) when is_list(List) ->
    % If the message to encode is a list, we encode it as if it is a map, then
    % add the `.' key to the `ao-types' field, indicating that this message is
    % a list.
    {ok, DecodedAsMap} = from(hb_util:list_to_numbered_message(List), Req, Opts),
    AOTypes = decode_ao_types(DecodedAsMap, Opts),
    {ok,
     DecodedAsMap#{
       <<"ao-types">> =>
           encode_ao_types(
             AOTypes#{
               <<".">> => <<"list">>
              },
             Opts)
      }};
from(Msg, Req, Opts) when is_map(Msg) ->
    % Normalize the message, offloading links to the cache.
    NormLinks = hb_link:normalize(Msg, linkify_mode(Req, Opts), Opts),
    NormKeysMap = hb_ao:normalize_keys(NormLinks, Opts),
    {Types, Values} = lists:foldl(
                        fun(Key, {Types, Values}) ->
                                case hb_maps:find(Key, NormKeysMap, Opts) of
                                    {ok, Value} when is_binary(Value) ->
                                        {Types, [{Key, Value} | Values]};
                                    {ok, Nested} when is_map(Nested) or is_list(Nested) ->
                                        ?event({from_recursing, {nested, Nested}}),
                                        {Types, [{Key, hb_util:ok(from(Nested, Req, Opts))} | Values]};
                                    {ok, Value} when is_atom(Value) or is_integer(Value) or
                                                     is_float(Value) ->
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
                        {[], []},
                        lists:filter(
                          fun(Key) ->
                                  % Filter keys that the user could set directly, but
                                  % should be regenerated when converting. Additionally, we remove
                                  % the `commitments' submessage, if applicable, as it should not
                                  % be modified during encoding.
                                  not lists:member(Key, ?REGEN_KEYS) andalso
                                  not hb_private:is_private(Key) andalso
                                  not (Key == <<"commitments">>)
                          end,
                          hb_util:to_sorted_keys(NormKeysMap, Opts))),
    % Encode the AoTypes as a structured dictionary
    % And include as a field on the produced TABM
    WithTypes =
        hb_maps:from_list(case Types of
                              [] -> Values;
                              T ->
                                  AoTypes = iolist_to_binary(hb_structured_fields:dictionary(
                                                               lists:map(
                                                                 fun({Key, Value}) ->
                                                                         {ok, Item} = hb_structured_fields:to_item(Value),
                                                                         {hb_escape:encode(Key), Item}
                                                                 end,
                                                                 lists:reverse(T)))),
                                  [{<<"ao-types">>, AoTypes} | Values]
                          end),
    % If the message has a `commitments' field, add it to the TABM unmodified.
    {ok,
     case maps:get(<<"commitments">>, Msg, not_found) of
         not_found ->
             WithTypes;
         Commitments ->
             WithTypes#{
               <<"commitments">> => Commitments
              }
     end};
from(Other, _Req, _Opts) -> {ok, hb_path:to_binary(Other)}.


%% @doc Discern the linkify mode from the request and the options.
linkify_mode(Req, Opts) ->
    case hb_maps:get(<<"bundle">>, Req, not_found, Opts) of
        not_found -> hb_opts:get(linkify_mode, offload, Opts);
        true ->
            % The request is asking for a bundle, so we should _not_ linkify.
            false;
        false ->
            % The request is asking for a flat message, so we should linkify.
            true
    end.


%% @doc Convert a TABM into a native HyperBEAM message.
to(Bin, _Req, _Opts) when is_binary(Bin) -> {ok, Bin};
to(TABM0, Req, Opts) ->
    Types = decode_ao_types(TABM0, Opts),
    % Decode all links to their HyperBEAM-native, resolvable form.
    TABM1 = hb_link:decode_all_links(TABM0),
    % 1. Remove 'ao-types' field
    % 2. Decode any binary values that have a type;
    % 3. Recursively decode any maps that we encounter;
    % 4. Return the remaining keys and values as a map.
    ResMsg =
        maps:fold(
          fun(<<"ao-types">>, _Value, Acc) -> Acc;
             (RawKey, BinValue, Acc) when is_binary(BinValue) ->
                  case hb_maps:find(hb_ao:normalize_key(RawKey), Types, Opts) of
                      % The value is a binary, no parsing required
                      error -> Acc#{RawKey => BinValue};
                      % Parse according to its type
                      {ok, Type} ->
                          Acc#{RawKey => decode_value(Type, BinValue)}
                  end;
             (RawKey, ChildTABM, Acc) when is_map(ChildTABM) ->
                  % Decode the child TABM
                  Acc#{
                    RawKey => hb_util:ok(to(ChildTABM, Req, Opts))
                   };
             (RawKey, Value, Acc) ->
                  % We encountered a key that already has a converted type.
                  % We can just return it as is.
                  Acc#{RawKey => Value}
          end,
          #{},
          TABM1),
    % If the message is a list, we need to convert it back.
    case maps:get(<<".">>, Types, not_found) of
        not_found -> {ok, ResMsg};
        <<"list">> -> {ok, hb_util:message_to_ordered_list(ResMsg, Opts)}
    end.


%% @doc Generate an `ao-types' structured field from a map of keys and their
%% types.
encode_ao_types(Types, _Opts) ->
    iolist_to_binary(hb_structured_fields:dictionary(
                       lists:map(
                         fun(Key) ->
                                 {ok, Item} = hb_structured_fields:to_item(maps:get(Key, Types)),
                                 {hb_escape:encode(Key), Item}
                         end,
                         hb_util:to_sorted_keys(Types)))).


%% @doc Parse the `ao-types' field of a TABM and return a map of keys and their
%% types
decode_ao_types(Msg, Opts) when is_map(Msg) ->
    decode_ao_types(hb_maps:get(<<"ao-types">>, Msg, <<>>, Opts), Opts);
decode_ao_types(Bin, _Opts) when is_binary(Bin) ->
    hb_maps:from_list(
      lists:map(
        fun({Key, {item, {_, Value}, _}}) ->
                {hb_escape:decode(Key), Value}
        end,
        hb_structured_fields:parse_dictionary(Bin))).


%% @doc Determine if the `ao-types' field of a TABM indicates that the message
%% is a list.
is_list_from_ao_types(Types, Opts) when is_binary(Types) ->
    is_list_from_ao_types(decode_ao_types(Types, Opts), Opts);
is_list_from_ao_types(Types, _Opts) ->
    case maps:find(<<".">>, Types) of
        {ok, <<"list">>} -> true;
        _ -> false
    end.


%% @doc Find the implicit keys of a TABM.
implicit_keys(Req) ->
    implicit_keys(Req, #{}).


implicit_keys(Req, Opts) ->
    hb_maps:keys(
      hb_maps:filtermap(
        fun(_Key, Val = <<"empty-", _/binary>>) -> {true, Val};
           (_Key, _Val) -> false
        end,
        decode_ao_types(Req, Opts),
        Opts),
      Opts).


%% @doc Convert a term to a binary representation, emitting its type for
%% serialization as a separate tag.
encode_value(Value) when is_integer(Value) ->
    [Encoded, _] = hb_structured_fields:item({item, Value, []}),
    {<<"integer">>, Encoded};
encode_value(Value) when is_float(Value) ->
    ?no_prod("Must use structured field representation for floats!"),
    {<<"float">>, float_to_binary(Value)};
encode_value(Value) when is_atom(Value) ->
    EncodedIOList =
        hb_structured_fields:item({item, {token, hb_util:bin(Value)}, []}),
    Encoded = hb_util:bin(EncodedIOList),
    {<<"atom">>, Encoded};
encode_value(Values) when is_list(Values) ->
    EncodedValues =
        lists:map(
          fun(Bin) when is_binary(Bin) -> {item, {string, Bin}, []};
             (Item) ->
                  {RawType, Encoded} = encode_value(Item),
                  Type = hb_ao:normalize_key(RawType),
                  {item,
                   {string,
                    <<"(ao-type-", Type/binary, ") ",
                      Encoded/binary>>},
                   []}
          end,
          Values),
    EncodedList = hb_structured_fields:list(EncodedValues),
    {<<"list">>, iolist_to_binary(EncodedList)};
encode_value(Value) when is_binary(Value) ->
    {<<"binary">>, Value};
encode_value(Value) ->
    Value.


%% @doc Convert non-binary values to binary for serialization.
decode_value(Type, Value) when is_list(Type) ->
    decode_value(list_to_binary(Type), Value);
decode_value(Type, Value) when is_binary(Type) ->
    ?event({decoding, {type, Type}, {value, Value}}),
    decode_value(
      binary_to_existing_atom(
        list_to_binary(string:to_lower(binary_to_list(Type))),
        latin1),
      Value);
decode_value(integer, Value) ->
    {item, Number, _} = hb_structured_fields:parse_item(Value),
    Number;
decode_value(float, Value) ->
    binary_to_float(Value);
decode_value(atom, Value) ->
    {item, {_, AtomString}, _} =
        hb_structured_fields:parse_item(Value),
    hb_util:atom(AtomString);
decode_value(list, Value) when is_binary(Value) ->
    lists:map(
      fun({item, {string, <<"(ao-type-", Rest/binary>>}, _}) ->
              [Type, Item] = binary:split(Rest, <<") ">>),
              decode_value(Type, Item);
         ({item, Item, _}) -> hb_structured_fields:from_bare_item(Item)
      end,
      hb_structured_fields:parse_list(iolist_to_binary(Value)));
decode_value(list, Value) when is_map(Value) ->
    hb_util:message_to_ordered_list(Value);
decode_value(map, Value) ->
    hb_maps:from_list(
      lists:map(
        fun({Key, {item, Item, _}}) ->
                ?event({decoded_item, {explicit, Key}, Item}),
                {Key, hb_structured_fields:from_bare_item(Item)}
        end,
        hb_structured_fields:parse_dictionary(iolist_to_binary(Value))));
decode_value(BinType, Value) when is_binary(BinType) ->
    decode_value(
      list_to_existing_atom(
        string:to_lower(
          binary_to_list(BinType))),
      Value);
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
