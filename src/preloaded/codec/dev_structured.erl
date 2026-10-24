%%% @doc A device implementing the codec interface (to/1, from/1) for 
%%% HyperBEAM's internal, richly typed message format. Each supported rich type
%%% has one encoding, and `to/1' refuses a value in any other:
%%% - `integer': decimal digits with no leading zero, and `-' before any value
%%%   but zero;
%%% - `float': an RFC 9651 Decimal, the float rounded to three places, to the
%%%   even digit when halfway, with no trailing zero after the first
%%%   fractional digit and at most 12 integer digits (a larger float gives the
%%%   largest Decimal of its sign); and the parameter `binary64', the float's
%%%   8 IEEE 754 bytes, big-endian, as base64url with no padding, which gives
%%%   its value: `0.062;binary64="P7AAAAAAAAA"' for 0.0625;
%%% - `atom': an RFC 9651 Token;
%%% - `list': a message with the keys `1' to `N', marked in `ao-types';
%%% - `binary', when listed in `encode-types': a binary that is not UTF-8 text,
%%%   as base64url with no padding.
%%% 
%%% `ao-types' gives the types of a message's values: an RFC 9651 dictionary
%%% with a member for each type, whose inner list contains the keys of that type
%%% as strings, and a key with a byte outside `0x20' to `0x7e' as a byte
%%% sequence. Types, and the keys of each type, are sorted by their bytes,
%%% and the key `.' of type `list' marks a message that is a list:
%%% `integer=("1" "2"), list=(".")'. `to/3' refuses any other encoding, and a
%%% type for a key with no binary value.
%%% 
%%% Encoding to TABM can be limited to a subset of types (with other types
%%% passing through in their rich representation) by specifying the types 
%%% that should be encoded with the `encode-types' request key.
%%% 
%%% This format mirrors HTTP Structured Fields, aside from its limitations of 
%%% compound type depths, and integers of more than 15 digits.
%%% 
%%% As with all AO-Core codecs, its target format (the format it expects to 
%%% receive in the `to/1' function, and give in `from/1') is TABM.
%%% 
%%% For more details, see the HTTP Structured Fields (RFC-9651) specification.
-module(dev_structured).
-specification("../../../specs/structured@1.0.md").
-export([to/3, from/3, commit/3, verify/3, encode_types/3, decode_types/3]).
-export([encode_ao_types/2, decode_ao_types/2, is_list_from_ao_types/2]).
-export([decode_value/3, encode_value/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SUPPORTED_TYPES, [<<"integer">>, <<"float">>, <<"atom">>, <<"list">>]).
%% The types that `ao-types' holds, in the order of their bytes.
-define(AO_TYPES,
    [<<"atom">>, <<"binary">>, <<"float">>, <<"integer">>, <<"list">>]
).

%% @doc Route commitments through `httpsig@1.0'.
-spec commit(#{ _ => _ }, #{ _ => _ }, map()) -> term().
commit(Msg, Req, Opts) ->
    % `hb_message:commit/3' without its first line, which takes the node key:
    % a client can call this.
    {ok, Structured} = to(Msg, #{}, Opts),
    hb_ao:raw(
        <<"message@1.0">>,
        <<"commit">>,
        Structured,
        Req#{ <<"commitment-device">> => <<"httpsig@1.0">> },
        Opts
    ).

%% @doc Route verification through `httpsig@1.0'.
-spec verify(#{ _ => _ }, #{ _ => _ }, map()) -> term().
verify(Msg, Req, Opts) ->
    hb_ao:raw(<<"httpsig@1.0">>, <<"verify">>, Msg, Req, Opts).

%% @doc Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).
%% An integer whose magnitude is greater than the request's `max-integer' is
%% encoded with its type, as a value of a type in `encode-types' is.
-spec from(
    binary() | [_] | #{ _ => _ },
    #{ 'encode-types' => [binary()], 'max-integer' => integer(),
        bundle => boolean(), _ => _ },
    #{ _ => _ }
) -> {ok, binary() | [_] | #{ _ => _ }}.
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
    NormLinks =
        hb_link:normalize(
            case Msg of
                #{ <<"ao-types">> := AOTypeMap } when is_map(AOTypeMap) ->
                    Msg#{ <<"ao-types">> := encode_ao_types(AOTypeMap, Opts) };
                _ ->
                    Msg
            end,
            linkify_mode(HintedReq, Opts),
            Opts
        ),
    NormKeysMap =
        hinted_keys(hb_ao:normalize_keys(NormLinks, Opts), HintedReq, Opts),
    EncodeTypes = find_encode_types(HintedReq, Opts),
    EncodeBinaries = lists:member(<<"binary">>, EncodeTypes),
    MaxInteger = hb_maps:get(<<"max-integer">>, HintedReq, infinity, Opts),
    {Types, Values} = lists:foldl(
        fun (Key, {Types, Values}) ->
            case hb_maps:find(Key, NormKeysMap, Opts) of
                {ok, Value} when is_binary(Value), not EncodeBinaries ->
                    {Types, [{Key, Value} | Values]};
                {ok, Nested} when is_map(Nested) orelse is_list(Nested) ->
                    ?event_debug({from_recursing, {nested, Nested}}),
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
                        orelse is_float(Value)
                        orelse is_binary(Value) ->
                    BinKey = hb_ao:normalize_key(Key),
                    ?event_debug({encode_value, Value}),
                    case maybe_encode_value(Value, EncodeTypes, MaxInteger) of
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
                AoTypes = encode_ao_types(maps:from_list(T), Opts),
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
type(Bin) when is_binary(Bin) -> <<"binary">>;
type(Other) -> Other.

%% @doc If a `hint-device` key is present it indicates the desired
%% terminal format (after being converted via an intermediate `tabm`
%% format). In that case dev_structured defers to the target codec
%% to determine whether child messages should be loaded or unloaded.
apply_bundle_hint(Msg, Req, Opts) ->
    case hb_maps:get(<<"hint-device">>, Req, undefined, Opts) of
        undefined -> Req;
        DeviceBin ->
            case hb_ao:raw(DeviceBin, <<"to-hint">>, Msg, Req, Opts) of
                {ok, HintedReq} ->
                    % May add a `bundle` key to the request
                    HintedReq;
                _ ->
                    Req
            end
    end.

%% @doc Keep the keys of a message that the target codec's hint gives, and its
%% commitments: the codec's encoding of the message contains only those keys.
hinted_keys(Msg, #{ <<"committed">> := Committed }, Opts) ->
    hb_maps:with([<<"commitments">> | Committed], Msg, Opts);
hinted_keys(Msg, _Req, _Opts) ->
    Msg.

%% @doc Discern the linkify mode from the request and the options.
linkify_mode(Req, Opts) ->
    case hb_util:bool(hb_maps:get(<<"bundle">>, Req, not_found, Opts)) of
    	true ->
            % The request is asking for a bundle, so we should _not_ linkify.
            false;
        _ ->
            % The request is either asking for a flat message or has not
            % specified. In both cases we should linkify.
            hb_opts:get(<<"linkify-mode">>, offload, Opts)
    end.

%% @doc Convert a TABM into a native HyperBEAM message. Its keys are lowercased.
-spec to(binary() | [_] | #{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, binary() | [_] | #{ _ => _ }} | {error, #{ _ => _ }}.
to(Bin, _Req, _Opts) when is_binary(Bin) -> {ok, Bin};
to(TABM, _Req, Opts) ->
    try decode(TABM, true, Opts)
    catch
        error:badarg -> invalid_types();
        error:function_clause -> invalid_types();
        error:{badmatch, _} -> invalid_types();
        error:{case_clause, _} -> invalid_types();
        throw:{unexpected_type, _, _} -> invalid_types()
    end.

%% @doc Decode types and links recursively, leaving malformed inputs to the
%% codec boundary. If `Links' is `true', a value that starts with `ao:' is
%% decoded to a link, which is not loaded. Private keys keep their values, and
%% the values in `commitments' are never decoded as links.
decode(Bin, true, _Opts) when is_binary(Bin) -> {ok, hb_link:decode(Bin)};
decode(Bin, _Links, _Opts) when is_binary(Bin) -> {ok, Bin};
decode(List, Links, Opts) when is_list(List) ->
    {ok, [ hb_util:ok(decode(Value, Links, Opts)) || Value <- List ]};
decode(TABM0, Links, Opts) when is_map(TABM0) ->
    % `ao-types' names keys, so its names are lowercased as the keys are, and
    % two names of one key are refused. An `ao-types' that does not parse is
    % kept as a key of the message, and no types are applied.
    {AOTypes, TABM1} =
        case decode_types(TABM0, #{}, Opts) of
            {ok, Parsed} ->
                {Parsed, maps:remove(<<"ao-types">>, decode_keys(TABM0))};
            {error, _} -> {#{}, decode_keys(TABM0)}
        end,
    Types =
        maps:from_list(
            [
                {hb_util:lowercase_key(Name), Type}
            ||
                {Name, Type} <- maps:to_list(AOTypes)
            ]
        ),
    true = map_size(Types) == map_size(AOTypes),
    % 1. Remove 'ao-types' field
    % 2. Decode the value of each key that `ao-types' lists, which is a
    %    binary, and the other values that are links;
    % 3. Recursively decode any maps that we encounter;
    % 4. Return the remaining keys and values as a map.
    Typed =
        maps:fold(
            fun(<<".">>, <<"list">>, Acc) -> Acc;
               (Key, Type, Acc) ->
                    #{ Key := Encoded } = Acc,
                    Acc#{ Key := decode_value(Type, Encoded) }
            end,
            TABM1,
            Types
        ),
    % The fold starts from the TABM, so a key that needs no decoding is not
    % rebuilt.
    ResMsg =
        maps:fold(
            fun (<<"ao-types">>, _Value, Acc) -> Acc;
            (RawKey, Value = <<"ao:", _/binary>>, Acc)
                    when Links, not is_map_key(RawKey, Types) ->
                % The value is an untyped binary that starts with `ao:', so we
                % decode it as a link. Values under private keys are left as
                % they are.
                case hb_private:is_private(RawKey) of
                    true -> Acc;
                    false -> Acc#{ RawKey => hb_link:decode(RawKey, Value) }
                end;
            (<<"commitments">>, Commitments, Acc) when is_map(Commitments) ->
                % The keys of `commitments' are IDs, so only its values are
                % decoded. The values in a commitment are not links.
                Acc#{
                    <<"commitments">> =>
                        maps:map(
                            fun(_ID, Commitment) ->
                                hb_util:ok(decode(Commitment, false, Opts))
                            end,
                            Commitments
                        )
                };
            (RawKey, ChildTABM, Acc) when is_map(ChildTABM) or is_list(ChildTABM) ->
                % Decode the child TABM, unless the key is private
                case hb_private:is_private(RawKey) of
                    true -> Acc;
                    false ->
                        Acc#{
                            RawKey => hb_util:ok(decode(ChildTABM, Links, Opts))
                        }
                end;
            (_RawKey, _Value, Acc) ->
                % A binary, or a value that already has its type, is kept as
                % it is.
                Acc
            end,
            Typed,
            TABM1
        ),
    % If the message is a list, we need to convert it back. A `.' type other
    % than `list' is the type of the message's own `.' key.
    case maps:get(<<".">>, Types, not_found) of
        <<"list">> -> {ok, hb_util:message_to_ordered_list(ResMsg, Opts)};
        _ -> {ok, ResMsg}
    end;
decode(Literal, _Links, _Opts) -> {ok, Literal}.

%% @doc Decode the keys of a TABM by lowercasing them. Keys that decode to the
%% same key combine their values in a list, in the order of the keys' bytes, as
%% HTTP joins the values of a repeated header: `Action' and `action' give
%% `action => [A, B]'. Private keys and keys that are not binaries are kept.
%% The same term is returned when no key changes.
decode_keys(TABM) ->
    Groups =
        maps:groups_from_list(
            fun hb_util:lowercase_key/1,
            [
                Key
            ||
                Key <- maps:keys(TABM),
                is_binary(Key),
                hb_util:lowercase_key(Key) =/= Key,
                not hb_private:is_private(Key)
            ]
        ),
    maps:fold(
        fun(Key, RawKeys, Acc) ->
            Members =
                lists:sort(
                    case is_map_key(Key, TABM) of
                        true -> [Key | RawKeys];
                        false -> RawKeys
                    end
                ),
            Values = [ maps:get(RawKey, TABM) || RawKey <- Members ],
            Decoded =
                case Values of
                    [Value] -> Value;
                    _ -> Values
                end,
            (maps:without(Members, Acc))#{ Key => Decoded }
        end,
        TABM,
        Groups
    ).

%% @doc Generate an `ao-types' structured field from a map of keys and their
%% types.
encode_types(Base, Req, Opts) ->
    {ok, encode_ao_types(hb_maps:get(<<"body">>, Req, Base, Opts), Opts)}.

%% @doc Encode a map of keys and their types as an `ao-types' dictionary: a
%% member for each type, whose inner list contains the keys of that type as
%% strings or byte sequences (`encode_ao_types_key/1'). Types, and the keys of
%% each type, are sorted by their bytes.
encode_ao_types(Types, _Opts) ->
    Groups =
        maps:groups_from_list(
            fun(Key) -> maps:get(Key, Types) end,
            hb_util:to_sorted_keys(Types)
        ),
    iolist_to_binary(hb_structured_fields:dictionary(
        [
            {Type, {list, lists:map(fun encode_ao_types_key/1, Keys), []}}
        ||
            {Type, Keys} <- hb_util:to_sorted_list(Groups)
        ]
    )).

%% @doc Encode a key of `ao-types' as a string, which allows the bytes `0x20'
%% to `0x7e', or as a byte sequence if the key has any other byte.
encode_ao_types_key(Key) ->
    case << <<C>> || <<C>> <= Key, C < 16#20 orelse C > 16#7e >> of
        <<>> -> {item, {string, Key}, []};
        _ -> {item, {binary, Key}, []}
    end.

%% @doc Device key for parsing an `ao-types' field.
decode_types(Base, Req, Opts) ->
    Types = hb_maps:get(<<"body">>, Req, Base, Opts),
    try {ok, decode_ao_types(Types, Opts)}
    catch
        error:badarg -> invalid_types();
        error:function_clause -> invalid_types();
        error:{badmatch, _} -> invalid_types();
        error:{case_clause, _} -> invalid_types()
    end.

%% @doc Device key for decoding a typed value: the base, by the `type' of the
%% request.
decode_value(Base, Req, Opts) ->
    Type = hb_maps:get(<<"type">>, Req, undefined, Opts),
    try {ok, decode_value(Type, Base)}
    catch
        error:badarg -> invalid_types();
        error:function_clause -> invalid_types();
        error:{badmatch, _} -> invalid_types()
    end.

%% @doc Reject malformed types without reflecting caller-controlled data.
invalid_types() ->
    {error, #{ <<"status">> => 400, <<"body">> => <<"Invalid AO types.">> }}.

%% @doc Parse the `ao-types' field of a TABM if present, and return a map of
%% keys and their types. If the given value is a list, we return an empty map
%% as there can be no `ao-types'.
decode_ao_types(List, _Opts) when is_list(List) -> #{};
decode_ao_types(Msg, Opts) when is_map(Msg) ->
    case hb_maps:find(<<"ao-types">>, Msg, Opts) of
        {ok, Types} -> decode_ao_types(Types, Opts);
        error -> #{}
    end;
decode_ao_types(Bin, _Opts) when is_binary(Bin) ->
    decode_ao_types(Bin, ?AO_TYPES, #{}).

%% @doc Read `ao-types' in the one encoding that `encode_ao_types/2' gives,
%% and refuse any other: at least one member, the members in the order of
%% their types, each with its keys in the order of their bytes, and each
%% key in one member.
decode_ao_types(Bin, [Type | Types], Acc) ->
    Size = byte_size(Type),
    case Bin of
        <<Type:Size/binary, "=(", Rest/binary>> ->
            {Keys, Next} = decode_ao_types_keys(Rest, []),
            Keys = lists:usort(Keys),
            Merged = maps:merge(Acc, maps:from_keys(Keys, Type)),
            true = map_size(Merged) == map_size(Acc) + length(Keys),
            case Next of
                <<>> -> Merged;
                <<", ", Members/binary>> ->
                    decode_ao_types(Members, Types, Merged)
            end;
        _ -> decode_ao_types(Bin, Types, Acc)
    end.

%% @doc Read the keys of an `ao-types' member up to its `)': strings and byte
%% sequences, separated by a space.
decode_ao_types_keys(Bin, Keys) ->
    {Item, Rest} = hb_structured_fields:parse_bare_item(Bin),
    Key =
        decode_ao_types_key(
            Item,
            binary:part(Bin, 0, byte_size(Bin) - byte_size(Rest))
        ),
    case Rest of
        <<")", Next/binary>> -> {lists:reverse([Key | Keys]), Next};
        <<" ", More/binary>> -> decode_ao_types_keys(More, [Key | Keys])
    end.

%% @doc Decode a key of `ao-types' from its item, a string or a byte sequence.
%% Refuse a byte sequence if `Form', the bytes of the item, is not the form
%% that `encode_ao_types_key/1' gives for its key. A string has only one form.
decode_ao_types_key({string, Key}, _Form) -> Key;
decode_ao_types_key({binary, Key}, Form) ->
    Form =
        iolist_to_binary(hb_structured_fields:item(encode_ao_types_key(Key))),
    Key.

%% @doc Determine if the `ao-types' field of a TABM indicates that the message
%% is a list.
is_list_from_ao_types(Types, Opts) when is_binary(Types) ->
    is_list_from_ao_types(decode_ao_types(Types, Opts), Opts);
is_list_from_ao_types(Types, _Opts) ->
    case maps:find(<<".">>, Types) of
        {ok, <<"list">>} -> true;
        _ -> false
    end.

%% @doc Encode a value if it is in the list of supported types, or if it is an
%% integer whose magnitude is greater than `MaxInteger'. Every integer is below
%% `infinity', the default. A binary that is UTF-8 text is not encoded.
maybe_encode_value(Value, _EncodeTypes, MaxInteger)
        when is_integer(Value), abs(Value) > MaxInteger ->
    encode_value(Value);
maybe_encode_value(Value, EncodeTypes, _MaxInteger) ->
    case lists:member(type(Value), EncodeTypes) andalso not is_text(Value) of
        true -> encode_value(Value);
        false -> skip
    end.

%% @doc Check whether a value is UTF-8 text.
is_text(Value) ->
    is_binary(Value) andalso unicode:characters_to_binary(Value) =:= Value.

%% @doc Convert a term to a binary representation, emitting its type for
%% serialization as a separate tag.
encode_value(Value) when is_integer(Value) ->
    [Encoded, _] = hb_structured_fields:item({item, Value, []}),
    {<<"integer">>, Encoded};
encode_value(Value) when is_float(Value) ->
    {
        <<"float">>,
        <<
            (decimal(thousandths(Value)))/binary,
            ";binary64=\"", (hb_util:encode(<<Value/float>>))/binary, "\""
        >>
    };
encode_value(Value) when is_atom(Value) ->
    EncodedIOList =
        hb_structured_fields:item({item, {token, hb_util:bin(Value)}, []}),
    Encoded = hb_util:bin(EncodedIOList),
    {<<"atom">>, Encoded};
encode_value(Value) when is_binary(Value) ->
    {<<"binary">>, hb_util:encode(Value)};
encode_value(Value) ->
    Value.

%% @doc Write a number of thousandths as an RFC 9651 Decimal, which has at most
%% 12 integer digits: a larger number gives the largest Decimal of its sign.
decimal(Thousandths) when Thousandths > 999999999999999 ->
    decimal(999999999999999);
decimal(Thousandths) when Thousandths < -999999999999999 ->
    decimal(-999999999999999);
decimal(Thousandths) ->
    <<_, Fraction/binary>> =
        integer_to_binary(1000 + abs(Thousandths) rem 1000),
    iolist_to_binary(
        [
            case Thousandths < 0 of true -> <<"-">>; false -> <<>> end,
            integer_to_binary(abs(Thousandths) div 1000),
            <<".">>,
            case string:trim(Fraction, trailing, "0") of
                <<>> -> <<"0">>;
                Digits -> Digits
            end
        ]
    ).

%% @doc A float in thousandths, rounded to the nearest, or to the even one
%% when halfway. A float is exactly `Mantissa * 2^Exponent'.
thousandths(Float) ->
    {Sign, Mantissa, Exponent} =
        case <<Float/float>> of
            <<S:1, 0:11, M:52>> -> {S, M, -1074};
            <<S:1, E:11, M:52>> -> {S, M bor (1 bsl 52), E - 1075}
        end,
    Scaled = Mantissa * 1000,
    Abs =
        case Exponent >= 0 of
            true -> Scaled bsl Exponent;
            false ->
                Floor = Scaled bsr -Exponent,
                Half = 1 bsl (-Exponent - 1),
                case Scaled - (Floor bsl -Exponent) of
                    Rest when Rest > Half -> Floor + 1;
                    Half -> Floor + (Floor band 1);
                    _ -> Floor
                end
        end,
    case Sign of
        1 -> -Abs;
        0 -> Abs
    end.

%% @doc Decode a typed value from the one encoding that `encode_value/1'
%% writes for it. Any other encoding of the value is refused.
decode_value(<<"integer">>, <<"0">>) -> 0;
decode_value(<<"integer">>, Encoded = <<"-", D, _/binary>>)
        when D >= $1, D =< $9 ->
    binary_to_integer(Encoded);
decode_value(<<"integer">>, Encoded = <<D, _/binary>>) when D >= $1, D =< $9 ->
    binary_to_integer(Encoded);
decode_value(<<"float">>, Encoded) ->
    % The value is the `binary64' parameter, and the Decimal its rounding.
    [Decimal, <<"binary64=\"", Bits:11/binary, "\"">>] =
        binary:split(Encoded, <<";">>),
    <<Value/float>> = base64url(Bits),
    Decimal = decimal(thousandths(Value)),
    Value;
decode_value(<<"atom">>, Encoded) ->
    {{token, Encoded}, <<>>} = hb_structured_fields:parse_bare_item(Encoded),
    hb_util:atom(Encoded);
decode_value(<<"binary">>, Encoded) ->
    Value = base64url(Encoded),
    false = is_text(Value),
    Value.

%% @doc Decode unpadded base64url. The decoder refuses every byte outside
%% base64url but `+' and `/', which it reads as `-' and `_', and a final `=';
%% those are refused here, as are a length that leaves one character over and
%% a last character that sets a bit past the last byte.
base64url(Encoded) ->
    nomatch = binary:match(Encoded, <<"+">>),
    nomatch = binary:match(Encoded, <<"/">>),
    true =
        case byte_size(Encoded) rem 4 of
            0 -> Encoded == <<>> orelse binary:last(Encoded) =/= $=;
            1 -> false;
            2 -> lists:member(binary:last(Encoded), "AQgw");
            3 -> lists:member(binary:last(Encoded), "AEIMQUYcgkosw048")
        end,
    hb_util:decode(Encoded).

%%% Tests

malformed_types_return_error_test() ->
    lists:foreach(
        fun(Types) ->
            Msg = #{ <<"ao-types">> => Types, <<"a">> => <<"text">> },
            lists:foreach(
                fun(Input) ->
                    ?assertMatch({error, #{ <<"status">> := 400 }}, hb_ao:raw(
                        <<"structured@1.0">>, <<"to">>, Input, #{}, #{}
                    ))
                end,
                [Msg, #{ <<"nested">> => Msg }, [Msg]]
            )
        end,
        [<<"float=(\"a\")">>, <<"integer=(\"a\")">>]
    ),
    % An `ao-types' that does not parse, such as `a="integer"', is kept as a
    % key of the message, and no types are applied.
    lists:foreach(
        fun(Types) ->
            Msg = #{ <<"ao-types">> => Types, <<"a">> => <<"text">> },
            ?assertEqual({ok, Msg}, hb_ao:raw(
                <<"structured@1.0">>, <<"to">>, Msg, #{}, #{}
            ))
        end,
        [<<"a=1.2345">>, <<"a=1">>, <<"a=?1">>,
            <<"unknown-type=(\"a\")">>, <<"a=\"integer\"">>]
    ),
    % `ao-types' in an encoding that `from/3' does not write: empty, a member
    % for each key, keys or types out of order, a key of two types, a space, a
    % byte sequence for the key `a', and one with a bit set past its last byte.
    lists:foreach(
        fun(Types) ->
            ?assertMatch({error, #{ <<"status">> := 400 }}, hb_ao:raw(
                <<"structured@1.0">>, <<"decode-types">>, Types, #{}, #{}
            ))
        end,
        [<<>>, <<"a=1">>, <<"a=\"integer\"">>, <<"integer=(\"b\" \"a\")">>,
            <<"list=(\".\"), integer=(\"a\")">>,
            <<"atom=(\"a\"), integer=(\"a\")">>, <<"integer=( \"a\")">>,
            <<"integer=(:YQ==:)">>, <<"integer=(:Y2Fmw6l=:)">>]
    ),
    ?assertEqual({ok, #{ <<"a">> => 42 }}, hb_ao:raw(
        <<"structured@1.0">>, <<"to">>,
        #{ <<"ao-types">> => <<"integer=(\"a\")">>, <<"a">> => <<"42">> },
        #{}, #{}
    )),
    % A type for a key with no binary value.
    ?assertMatch({error, #{ <<"status">> := 400 }}, hb_ao:raw(
        <<"structured@1.0">>, <<"to">>,
        #{ <<"ao-types">> => <<"integer=(\"b\")">>, <<"a">> => <<"42">> },
        #{}, #{}
    )),
    % Two names of one key: `A' and `a' are one key.
    ?assertMatch({error, #{ <<"status">> := 400 }}, hb_ao:raw(
        <<"structured@1.0">>, <<"to">>,
        #{ <<"ao-types">> => <<"integer=(\"A\" \"a\")">>, <<"a">> => <<"42">> },
        #{}, #{}
    )),
    % A typed value in an encoding that `from/3' does not write.
    lists:foreach(
        fun({Type, Value}) ->
            ?assertMatch({error, #{ <<"status">> := 400 }}, hb_ao:raw(
                <<"structured@1.0">>, <<"to">>,
                #{
                    <<"ao-types">> => <<Type/binary, "=(\"a\")">>,
                    <<"a">> => Value
                },
                #{}, #{}
            ))
        end,
        [{<<"integer">>, <<"042">>}, {<<"integer">>, <<"-0">>},
            {<<"float">>, <<"0.062">>},
            {<<"float">>, <<"0.063;binary64=\"P7AAAAAAAAA\"">>},
            {<<"float">>, <<"0.062;binary64=\"P7AAAAAAAAB\"">>},
            {<<"float">>, <<"0.062;binary64=\"P7AAAAAAAAA\";x=1">>},
            {<<"atom">>, <<"\"true\"">>}, {<<"binary">>, <<"aGVsbG8">>},
            {<<"binary">>, <<"AP9">>}]
    ),
    % A float is its rounding to three places, as an RFC 9651 Decimal, and its
    % IEEE 754 bytes, which give its value.
    lists:foreach(
        fun({Float, Encoded}) ->
            ?assertMatch({ok, #{ <<"a">> := Encoded }}, hb_ao:raw(
                <<"structured@1.0">>, <<"from">>,
                #{ <<"a">> => Float },
                #{}, #{}
            )),
            ?assertEqual({ok, #{ <<"a">> => Float }}, hb_ao:raw(
                <<"structured@1.0">>, <<"to">>,
                #{ <<"ao-types">> => <<"float=(\"a\")">>, <<"a">> => Encoded },
                #{}, #{}
            ))
        end,
        [{0.0625, <<"0.062;binary64=\"P7AAAAAAAAA\"">>},
            {0.1, <<"0.1;binary64=\"P7mZmZmZmZo\"">>}]
    ),
    % A key with a byte outside `0x20'-`0x7e' is a byte sequence in `ao-types'.
    TABM =
        #{
            <<"ao-types">> => <<"integer=(:Y2Fmw6k=:)">>,
            <<"caf", 195, 169>> => <<"1">>
        },
    ?assertEqual({ok, TABM}, hb_ao:raw(
        <<"structured@1.0">>, <<"from">>,
        #{ <<"caf", 195, 169>> => 1 },
        #{}, #{}
    )),
    ?assertEqual({ok, #{ <<"caf", 195, 169>> => 1 }}, hb_ao:raw(
        <<"structured@1.0">>, <<"to">>, TABM, #{}, #{}
    )).

%% @doc Keys are lowercased, a link key gives a link, keys that become one key
%% give a list, and a private key and the IDs that key `commitments' keep their
%% case.
lowercase_keys_test() ->
    ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    ?assertEqual(
        {ok, #{
            <<"count">> => 1,
            <<"child">> => {link, <<"ao:", ID/binary>>, #{}},
            <<"action">> => [<<"a">>, <<"b">>],
            hb_util:lowercase_key(ID) => <<"balance">>,
            <<"commitments">> => #{ ID => #{ <<"keyid">> => <<"k">> } },
            <<"Priv-Key">> => #{ <<"Inner">> => 1 }
        }},
        hb_ao:raw(
            <<"structured@1.0">>, <<"to">>,
            #{
                <<"ao-types">> => <<"integer=(\"Count\")">>,
                <<"Count">> => <<"1">>,
                <<"Child">> => <<"ao:", ID/binary>>,
                <<"Action">> => <<"a">>,
                <<"action">> => <<"b">>,
                ID => <<"balance">>,
                <<"commitments">> => #{ ID => #{ <<"Keyid">> => <<"k">> } },
                <<"Priv-Key">> => #{ <<"Inner">> => 1 }
            },
            #{}, #{}
        )
    ).
