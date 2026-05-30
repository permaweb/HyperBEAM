%%% @doc A device implementing the codec interface (to/1, from/1) for 
%%% HyperBEAM's internal, richly typed message format. Supported rich types are:
%%% - `integer'
%%% - `float'
%%% - `atom'
%%% - `list'
%%% 
%%% Encoding to TABM can be limited to a subset of types (with other types
%%% passing through in their rich representation) by specifying the types 
%%% that should be encoded with the `encode-types' request key.
%%% 
%%% This format mirrors HTTP Structured Fields, aside from its limitations of 
%%% compound type depths, as well as limited floating point representations.
%%% 
%%% As with all AO-Core codecs, its target format (the format it expects to 
%%% receive in the `to/1' function, and give in `from/1') is TABM.
%%% 
%%% For more details, see the HTTP Structured Fields (RFC-9651) specification.
-module(dev_structured).
-export([to/3, from/3, commit/3, verify/3, encode_types/3, decode_types/3]).
-export([encode_ao_types/2, decode_ao_types/2, is_list_from_ao_types/2]).
-export([decode_value/2, encode_value/1, implicit_keys/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SUPPORTED_TYPES, [<<"integer">>, <<"float">>, <<"atom">>, <<"list">>]).

%% @doc Route commitments through `httpsig@1.0'.
commit(Msg, Req, Opts) ->
    {ok,
        hb_message:commit(
            Msg,
            Opts,
            Req#{ <<"commitment-device">> => <<"httpsig@1.0">> }
        )
    }.

%% @doc Route verification through `httpsig@1.0'.
verify(Msg, Req, Opts) ->
    {ok,
        hb_message:verify(
            Msg,
            Req#{ <<"commitment-device">> => <<"httpsig@1.0">> },
            Opts
        )
    }.

%% @doc Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).
from(Bin, _Req, _Opts) when is_binary(Bin) -> {ok, Bin};
from(List, Req, Opts) when is_list(List) ->
    % Encode the list as a map, then -- if our request indicates that we are
    % encoding lists -- add the `.' key to the `ao-types' field, indicating
    % that this message is a list and return. Otherwise, if the downstream
    % encoding did not set its own `ao-types' field, we convert the message
    % back to a list.
    {ok, DecodedAsMap} =
        from(
            hb_util:list_to_numbered_message(List),
            Req,
            Opts
        ),
    EncodingLists = lists:member(<<"list">>, find_encode_types(Req, Opts)),
    EncodingHasAOTypes = hb_maps:is_key(<<"ao-types">>, DecodedAsMap, Opts),
    case EncodingLists orelse EncodingHasAOTypes of
        true ->
            AOTypes = decode_ao_types(DecodedAsMap, Opts),
            {ok, DecodedAsMap#{
                <<"ao-types">> =>
                    encode_ao_types(
                        AOTypes#{
                            <<".">> => <<"list">>
                        },
                        Opts
                    )
                }
            };
        false ->
            % If the downstream encoding did not set its own `ao-types' field
            % we return the message as a list.
            {ok, hb_util:numbered_keys_to_list(DecodedAsMap, Opts)}
    end;
from(Msg, Req, Opts) when is_map(Msg) ->
    HintedReq = apply_bundle_hint(Msg, Req, Opts),
    NormLinks = hb_link:normalize(Msg, linkify_mode(HintedReq, Opts), Opts),
    NormKeysMap = hb_ao:normalize_keys(NormLinks, Opts),
    EncodeTypes = find_encode_types(HintedReq, Opts),
    {Types, Values} = lists:foldl(
        fun (Key, {Types, Values}) ->
            case hb_maps:find(Key, NormKeysMap, Opts) of
                {ok, Value} when is_binary(Value) ->
                    {Types, [{Key, Value} | Values]};
                {ok, Nested} when is_map(Nested) orelse is_list(Nested) ->
                    ?event({from_recursing, {nested, Nested}}),
                    % We pass the HintedReq to the recursive call rather than
                    % Req so that this message's bundle status serves as the
                    % default for any children that don't explicitly set the
                    % `bundle' flag on the hinted commitment.
                    {Types,
                        [{
                            Key,
                            hb_util:ok(from(Nested, HintedReq, Opts))
                        } | Values]};
                {ok, Value} when
                        is_atom(Value)
                        orelse is_integer(Value)
                        orelse is_float(Value) ->
                    BinKey = hb_ao:normalize_key(Key),
                    ?event({encode_value, Value}),
                    case maybe_encode_value(Value, EncodeTypes) of
                        {Type, BinValue} ->
                            {
                                [{BinKey, Type} | Types],
                                [{BinKey, BinValue} | Values]
                            };
                        skip ->
                            {Types, [{Key, Value} | Values]}
                    end;
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
                % should be regenerated when converting. Additionally, we remove
                % the `commitments' submessage, if applicable, as it should not
                % be modified during encoding.
                not lists:member(Key, ?REGEN_KEYS) andalso
                    not hb_private:is_private(Key) andalso
                    not (Key == <<"commitments">>)
            end,
            hb_util:to_sorted_keys(NormKeysMap, Opts)
        )
    ),
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
                        lists:reverse(T)
                    )
                )),
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
        end
    };
from(Other, _Req, _Opts) -> {ok, hb_path:to_binary(Other)}.

%% @doc Find the types that should be encoded from the request and options.
find_encode_types(Req, Opts) ->
    hb_maps:get(<<"encode-types">>, Req, ?SUPPORTED_TYPES, Opts).

%% @doc Determine the type for a value.
type(Int) when is_integer(Int) -> <<"integer">>;
type(Float) when is_float(Float) -> <<"float">>;
type(Atom) when is_atom(Atom) -> <<"atom">>;
type(List) when is_list(List) -> <<"list">>;
type(Other) -> Other.

%% @doc If a `hint-device` key is present it indicates the desired
%% terminal format (after being converted via an intermediate `tabm`
%% format). In that case dev_structured defers to the target codec
%% to determine whether child messages should be loaded or unloaded.
apply_bundle_hint(Msg, Req, Opts) ->
    case hb_maps:get(<<"hint-device">>, Req, undefined, Opts) of
        undefined -> Req;
        DeviceBin ->
            % May add a `bundle` key to the request
            try hb_util:ok(
                hb_ao:raw(DeviceBin, <<"to-hint">>, Msg, Req, Opts)
            )
            catch _:_ ->
                Req
            end
    end.

%% @doc Discern the linkify mode from the request and the options.
linkify_mode(Req, Opts) ->
    case hb_maps:get(<<"bundle">>, Req, not_found, Opts) of
    	true ->
            % The request is asking for a bundle, so we should _not_ linkify.
            false;
        _ ->
            % The request is either asking for a flat message or has not
            % specified. In both cases we should linkify.
            hb_opts:get(linkify_mode, offload, Opts)
    end.

%% @doc Convert a TABM into a native HyperBEAM message.
to(Bin, _Req, _Opts) when is_binary(Bin) -> {ok, Bin};
to(TABM0, Req, Opts) when is_list(TABM0) ->
    % If we receive a list, we convert it to a message and run `to/3' on it. 
    % Finally, we convert the result back to a list.
    {ok, TABM1} = to(hb_util:list_to_numbered_message(TABM0), Req, Opts),
    {ok, hb_util:numbered_keys_to_list(TABM1, Opts)};
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
            fun (<<"ao-types">>, _Value, Acc) -> Acc;
            (RawKey, BinValue, Acc) when is_binary(BinValue) ->
                case hb_maps:find(hb_ao:normalize_key(RawKey), Types, Opts) of
                    % The value is a binary, no parsing required
                    error -> Acc#{ RawKey => BinValue };
                    % Parse according to its type
                    {ok, Type} ->
                        Acc#{ RawKey => hb_util:decode(Type, BinValue) }
                end;
            (RawKey, ChildTABM, Acc) when is_map(ChildTABM) or is_list(ChildTABM) ->
                % Decode the child TABM
                Acc#{
                    RawKey => hb_util:ok(to(ChildTABM, Req, Opts))
                };
            (RawKey, Value, Acc) ->
                % We encountered a key that already has a converted type.
                % We can just return it as is.
                Acc#{ RawKey => Value }
            end,
            #{},
            TABM1
        ),
    % If the message is a list, we need to convert it back.
    case maps:get(<<".">>, Types, not_found) of
        not_found -> {ok, ResMsg};
        <<"list">> -> {ok, hb_util:message_to_ordered_list(ResMsg, Opts)}
    end.

%% @doc Generate an `ao-types' structured field from a map of keys and their
%% types.
encode_types(Base, Req, Opts) ->
    {ok, encode_ao_types(hb_maps:get(<<"body">>, Req, Base, Opts), Opts)}.

encode_ao_types(Types, _Opts) ->
    iolist_to_binary(hb_structured_fields:dictionary(
        lists:map(
            fun(Key) ->
                {ok, Item} = hb_structured_fields:to_item(maps:get(Key, Types)),
                {hb_escape:encode(Key), Item}
            end,
            hb_util:to_sorted_keys(Types)
        )
    )).

%% @doc Device key for parsing an `ao-types' field.
decode_types(Base, Req, Opts) ->
    {ok, decode_ao_types(hb_maps:get(<<"body">>, Req, Base, Opts), Opts)}.

%% @doc Parse the `ao-types' field of a TABM if present, and return a map of
%% keys and their types. If the given value is a list, we return an empty map
%% as there can be no `ao-types'.
decode_ao_types(List, _Opts) when is_list(List) -> #{};
decode_ao_types(Msg, Opts) when is_map(Msg) ->
    decode_ao_types(hb_maps:get(<<"ao-types">>, Msg, <<>>, Opts), Opts);
decode_ao_types(Bin, _Opts) when is_binary(Bin) ->
    hb_maps:from_list(
        lists:map(
            fun({Key, {item, {_, Value}, _}}) ->
                {hb_escape:decode(Key), Value}
            end,
            hb_structured_fields:parse_dictionary(Bin)    
        )
    ).

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
implicit_keys(Req, Opts) ->
    hb_maps:keys(
        hb_maps:filtermap(
            fun(_Key, Val = <<"empty-", _/binary>>) -> {true, Val};
            (_Key, _Val) -> false
            end,
            decode_ao_types(Req, Opts),
            Opts
        ),
		Opts
    ).

%% @doc Encode a value if it is in the list of supported types.
maybe_encode_value(Value, EncodeTypes) ->
    case lists:member(type(Value), EncodeTypes) of
        true -> encode_value(Value);
        false -> skip
    end.

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

%% @doc Decode a structured field value by AO-Core structured type.
decode_value(Type, Value) ->
    hb_util:decode(Type, Value).

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
