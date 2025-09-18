# hb_structured_fields

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_structured_fields.erl)

A module for parsing and converting between Erlang and HTTP Structured
Fields, as described in RFC-9651.
The mapping between Erlang and structured headers types is as follow:
List: list()
Inner list: {list, [item()], params()}
Dictionary: [{binary(), item()}]
  There is no distinction between empty list and empty dictionary.
Item with parameters: {item, bare_item(), params()}
Parameters: [{binary(), bare_item()}]
Bare item: one bare_item() that can be of type:
Integer: integer()
Decimal: {decimal, {integer(), integer()}}
String: {string, binary()}
Token: {token, binary()}
Byte sequence: {binary, binary()}
Boolean: boolean()

---

## Exported Functions

- `bare_item/1`
- `dictionary/1`
- `from_bare_item/1`
- `item/1`
- `list/1`
- `parse_bare_item/1`
- `parse_binary/1`
- `parse_dictionary/1`
- `parse_item/1`
- `parse_list/1`
- `to_dictionary/1`
- `to_item/1`
- `to_item/2`
- `to_list/1`

---

### to_dictionary

A module for parsing and converting between Erlang and HTTP Structured
Convert a map to a dictionary.

```erlang
to_dictionary(Map) when is_map(Map) ->
   to_dictionary(maps:to_list(Map));
```

### to_dictionary

A module for parsing and converting between Erlang and HTTP Structured
Convert a map to a dictionary.

```erlang
to_dictionary(Pairs) when is_list(Pairs) ->
    to_dictionary([], Pairs).
```

### to_dictionary

```erlang
to_dictionary(Dict, []) ->
    {ok, Dict};
```

### to_dictionary

```erlang
to_dictionary(_Dict, [{ Name, Value } | _Rest]) when is_map(Value) ->
    {too_deep, Name};
```

### to_dictionary

```erlang
to_dictionary(Dict, [{Name, Value} | Rest]) ->
    case to_item_or_inner_list(Value) of
        {ok, ItemOrInner} -> to_dictionary([{key_to_binary(Name), ItemOrInner} | Dict], Rest);
        E -> E
    end.
```

### to_item

Convert an item to a dictionary.

```erlang
to_item({item, Kind, Params}) when is_list(Params) ->
    {ok, {item, to_bare_item(Kind), [to_param(Pair) || Pair <- Params] }};
```

### to_item

Convert an item to a dictionary.

```erlang
to_item(Item) ->
    to_item(Item, []).
```

### to_item

```erlang
to_item(Item, Params) when is_list(Params) ->
    to_item({ item, to_bare_item(Item), Params}).
```

### to_list

Convert a list to an SF term.

```erlang
to_list(List) when is_list(List) ->
    to_list([], List).
```

### to_list

```erlang
to_list(Acc, []) ->
    {ok, lists:reverse(Acc)};
```

### to_list

```erlang
to_list(Acc, [ItemOrInner | Rest]) ->
    Res = to_item_or_inner_list(ItemOrInner),
    case Res of
        {ok, Elem} -> to_list([Elem | Acc], Rest);
        E -> E
    end.
```

### to_inner_list

Convert an inner list to an SF term.

```erlang
to_inner_list({list, Inner, Params}) when is_list(Inner) andalso is_list(Params) ->
    {ok, {list, [to_inner_item(I) || I <-- Inner], [to_param(Pair) || Pair <- Params]}};
```

### to_inner_list

Convert an inner list to an SF term.

```erlang
to_inner_list(Inner) ->
    to_inner_list(Inner, []).
```

### to_inner_list

```erlang
to_inner_list(Inner, Params) when is_list(Inner) andalso is_list(Params) ->
    to_inner_list([], Inner, Params).
```

### to_inner_list

```erlang
to_inner_list(Inner, [], Params) when is_list(Params) ->
    {ok, {list, lists:reverse(Inner), [to_param(Param) || Param <- Params]}};
```

### to_inner_list

```erlang
to_inner_list(_List, [Item | _Rest], _Params) when is_list(Item) orelse is_map(Item) ->
    {too_deep, Item};
```

### to_inner_list

```erlang
to_inner_list(Inner, [Item | Rest], Params) ->
    case to_item(Item) of
        {ok, I} -> to_inner_list([I | Inner], Rest, Params);
        E -> E
    end.
```

### to_item_or_inner_list

Convert an Erlang term to an SF `item` or `inner_list`.

```erlang
to_item_or_inner_list(ItemOrInner) ->
    case ItemOrInner of
        Map when is_map(Map) -> {too_deep, Map};
        % pre-parsed inner list
        {list, Inner, Params} -> to_inner_list(Inner, Params);
        Item when not is_list(Item) -> to_item(Item);
        Inner when is_list(Inner) -> to_inner_list(Inner)
    end.
```

### to_inner_item

Convert an Erlang term to an SF `item`.

```erlang
to_inner_item(Item) when is_list(Item) ->
    {too_deep, Item};
```

### to_inner_item

Convert an Erlang term to an SF `item`.

```erlang
to_inner_item(Item) ->
    case to_item(Item) of
        {ok, I} -> I;
        E -> E
    end.
```

### to_param

Convert an Erlang term to an SF `parameter`.

```erlang
to_param({Name, Value}) ->
    NormalizedName = key_to_binary(Name),
    {NormalizedName, to_bare_item(Value)}.
```

### to_bare_item

Convert an Erlang term to an SF `bare_item`.

```erlang
to_bare_item(BareItem) ->
     case BareItem of
        % Assume tuple is already parsed
        BI when is_tuple(BI) -> BI;
        % Serialize -> Parse numbers in order to ensure their lengths adhere to structured fields
        B when is_boolean(B) -> B;
        I when is_integer(I) ->
            {Int, _} = parse_bare_item(bare_item(I)),
            Int;
        F when is_float(F) ->
            {Dec, _} = parse_bare_item(bare_item({decimal, {F, 0}})),
            Dec;
        A when is_atom(A) -> {token, atom_to_binary(A)};
        S when is_binary(S) or is_list(S) -> {string, iolist_to_binary(S)}
    end.
```

### from_bare_item

Convert an SF `bare_item` to an Erlang term.

```erlang
from_bare_item(BareItem) ->
    case BareItem of
        I when is_integer(I) -> I;
        B when is_boolean(B) -> B;
        D = {decimal, _} ->
            list_to_float(
                binary_to_list(
                    iolist_to_binary(
                        bare_item(D)
                    )
                )
            );
        {string, S} -> S;
        {token, T} ->
            try binary_to_existing_atom(T) of
                Atom -> Atom
            catch
                error:badarg -> T
            end;
        {binary, B} -> B
    end.
```

### key_to_binary

Convert an Erlang term to a binary key.

```erlang
key_to_binary(Key) when is_atom(Key) -> atom_to_binary(Key);
```

### key_to_binary

Convert an Erlang term to a binary key.
Parse a binary SF dictionary.

```erlang
-spec parse_dictionary(binary()) -> sh_dictionary().
parse_dictionary(<<>>) ->
    [];
parse_dictionary(<<C, R/bits>>) when ?IS_ALPHA(C)
        or ?IS_DIGIT(C) or (C =:= $*) or (C =:= $%) or (C =:= $_) or (C =:= $-) 
        or (C =:= $.) ->
    parse_dict_key(R, [], <<C>>).
```

```erlang
key_to_binary(Key) -> iolist_to_binary(Key).
```

### parse_dict_key

Convert an Erlang term to a binary key.
Parse a binary SF dictionary.

```erlang
-spec parse_dictionary(binary()) -> sh_dictionary().
parse_dictionary(<<>>) ->
    [];
parse_dictionary(<<C, R/bits>>) when ?IS_ALPHA(C)
        or ?IS_DIGIT(C) or (C =:= $*) or (C =:= $%) or (C =:= $_) or (C =:= $-) 
        or (C =:= $.) ->
    parse_dict_key(R, [], <<C>>).
```

```erlang
parse_dict_key(<<$=, $(, R0/bits>>, Acc, K) ->
    {Item, R} = parse_inner_list(R0, []),
    parse_dict_before_sep(R, lists:keystore(K, 1, Acc, {K, Item}));
```

### parse_dict_key

Convert an Erlang term to a binary key.
Parse a binary SF dictionary.

```erlang
-spec parse_dictionary(binary()) -> sh_dictionary().
parse_dictionary(<<>>) ->
    [];
parse_dictionary(<<C, R/bits>>) when ?IS_ALPHA(C)
        or ?IS_DIGIT(C) or (C =:= $*) or (C =:= $%) or (C =:= $_) or (C =:= $-) 
        or (C =:= $.) ->
    parse_dict_key(R, [], <<C>>).
```

```erlang
parse_dict_key(<<$=, R0/bits>>, Acc, K) ->
    {Item, R} = parse_item1(R0),
    parse_dict_before_sep(R, lists:keystore(K, 1, Acc, {K, Item}));
```

### parse_dict_key

Convert an Erlang term to a binary key.
Parse a binary SF dictionary.

```erlang
-spec parse_dictionary(binary()) -> sh_dictionary().
parse_dictionary(<<>>) ->
    [];
parse_dictionary(<<C, R/bits>>) when ?IS_ALPHA(C)
        or ?IS_DIGIT(C) or (C =:= $*) or (C =:= $%) or (C =:= $_) or (C =:= $-) 
        or (C =:= $.) ->
    parse_dict_key(R, [], <<C>>).
```

```erlang
parse_dict_key(<<C, R/bits>>, Acc, K) when
        ?IS_ALPHA(C) or ?IS_DIGIT(C) or
        (C =:= $_) or (C =:= $-) or (C =:= $.) or (C =:= $*) or (C =:= $%) ->
    parse_dict_key(R, Acc, <<K/binary, C>>);
```

### parse_dict_key

Convert an Erlang term to a binary key.
Parse a binary SF dictionary.

```erlang
-spec parse_dictionary(binary()) -> sh_dictionary().
parse_dictionary(<<>>) ->
    [];
parse_dictionary(<<C, R/bits>>) when ?IS_ALPHA(C)
        or ?IS_DIGIT(C) or (C =:= $*) or (C =:= $%) or (C =:= $_) or (C =:= $-) 
        or (C =:= $.) ->
    parse_dict_key(R, [], <<C>>).
```

```erlang
parse_dict_key(<<$;, R0/bits>>, Acc, K) ->
    {Params, R} = parse_before_param(R0, []),
    parse_dict_before_sep(R, lists:keystore(K, 1, Acc, {K, {item, true, Params}}));
```

### parse_dict_key

Convert an Erlang term to a binary key.
Parse a binary SF dictionary.

```erlang
-spec parse_dictionary(binary()) -> sh_dictionary().
parse_dictionary(<<>>) ->
    [];
parse_dictionary(<<C, R/bits>>) when ?IS_ALPHA(C)
        or ?IS_DIGIT(C) or (C =:= $*) or (C =:= $%) or (C =:= $_) or (C =:= $-) 
        or (C =:= $.) ->
    parse_dict_key(R, [], <<C>>).
```

```erlang
parse_dict_key(R, Acc, K) ->
    parse_dict_before_sep(R, lists:keystore(K, 1, Acc, {K, {item, true, []}})).
```

### parse_dict_before_sep

Parse a binary SF dictionary before a separator.

```erlang
parse_dict_before_sep(<<$\s, R/bits>>, Acc) ->
    parse_dict_before_sep(R, Acc);
```

### parse_dict_before_sep

Parse a binary SF dictionary before a separator.

```erlang
parse_dict_before_sep(<<$\t, R/bits>>, Acc) ->
    parse_dict_before_sep(R, Acc);
```

### parse_dict_before_sep

Parse a binary SF dictionary before a separator.

```erlang
parse_dict_before_sep(<<C, R/bits>>, Acc) when C =:= $, ->
    parse_dict_before_member(R, Acc);
```

### parse_dict_before_sep

Parse a binary SF dictionary before a separator.

```erlang
parse_dict_before_sep(<<>>, Acc) ->
    Acc.
```

### parse_dict_before_member

Parse a binary SF dictionary before a member.

```erlang
parse_dict_before_member(<<$\s, R/bits>>, Acc) ->
    parse_dict_before_member(R, Acc);
```

### parse_dict_before_member

Parse a binary SF dictionary before a member.

```erlang
parse_dict_before_member(<<$\t, R/bits>>, Acc) ->
    parse_dict_before_member(R, Acc);
```

### parse_dict_before_member

Parse a binary SF dictionary before a member.

```erlang
parse_dict_before_member(<<C, R/bits>>, Acc)
        when ?IS_ALPHA(C) or ?IS_DIGIT(C) or (C =:= $*)
        or (C =:= $%) or (C =:= $_) or (C =:= $-) ->
    parse_dict_key(R, Acc, <<C>>).
```

### parse_item1

```erlang
-spec parse_item(binary()) -> sh_item().
parse_item(Bin) ->
    {Item, <<>>} = parse_item1(Bin),
    Item.
```

```erlang
parse_item1(Bin) ->
    case parse_bare_item(Bin) of
        {Item, <<$;, R/bits>>} ->
            {Params, Rest} = parse_before_param(R, []),
            {{item, Item, Params}, Rest};
        {Item, Rest} ->
            {{item, Item, []}, Rest}
    end.
```

### parse_list_member

Parse a binary SF list before a member.

```erlang
-spec parse_list(binary()) -> sh_list().
parse_list(<<>>) ->
    [];
parse_list(Bin) ->
    parse_list_before_member(Bin, []).
```

```erlang
parse_list_member(<<$(, R0/bits>>, Acc) ->
    {Item, R} = parse_inner_list(R0, []),
    parse_list_before_sep(R, [Item | Acc]);
```

### parse_list_member

Parse a binary SF list before a member.

```erlang
-spec parse_list(binary()) -> sh_list().
parse_list(<<>>) ->
    [];
parse_list(Bin) ->
    parse_list_before_member(Bin, []).
```

```erlang
parse_list_member(R0, Acc) ->
    {Item, R} = parse_item1(R0),
    parse_list_before_sep(R, [Item | Acc]).
```

### parse_list_before_sep

Parse a binary SF list before a separator.

```erlang
parse_list_before_sep(<<$\s, R/bits>>, Acc) ->
    parse_list_before_sep(R, Acc);
```

### parse_list_before_sep

Parse a binary SF list before a separator.

```erlang
parse_list_before_sep(<<$\t, R/bits>>, Acc) ->
    parse_list_before_sep(R, Acc);
```

### parse_list_before_sep

Parse a binary SF list before a separator.

```erlang
parse_list_before_sep(<<$,, R/bits>>, Acc) ->
    parse_list_before_member(R, Acc);
```

### parse_list_before_sep

Parse a binary SF list before a separator.

```erlang
parse_list_before_sep(<<>>, Acc) ->
    lists:reverse(Acc).
```

### parse_list_before_member

Parse a binary SF list before a member.

```erlang
parse_list_before_member(<<$\s, R/bits>>, Acc) ->
    parse_list_before_member(R, Acc);
```

### parse_list_before_member

Parse a binary SF list before a member.

```erlang
parse_list_before_member(<<$\t, R/bits>>, Acc) ->
    parse_list_before_member(R, Acc);
```

### parse_list_before_member

Parse a binary SF list before a member.

```erlang
parse_list_before_member(R, Acc) ->
    parse_list_member(R, Acc).
```

### parse_inner_list

```erlang
parse_inner_list(<<$\s, R/bits>>, Acc) ->
    parse_inner_list(R, Acc);
```

### parse_inner_list

```erlang
parse_inner_list(<<$), $;, R0/bits>>, Acc) ->
    {Params, R} = parse_before_param(R0, []),
    {{list, lists:reverse(Acc), Params}, R};
```

### parse_inner_list

```erlang
parse_inner_list(<<$), R/bits>>, Acc) ->
    {{list, lists:reverse(Acc), []}, R};
```

### parse_inner_list

```erlang
parse_inner_list(R0, Acc) ->
    {Item, R = <<C, _/bits>>} = parse_item1(R0),
    true = (C =:= $\s) orelse (C =:= $)),
    parse_inner_list(R, [Item | Acc]).
```

### parse_before_param

```erlang
parse_before_param(<<$\s, R/bits>>, Acc) ->
    parse_before_param(R, Acc);
```

### parse_before_param

```erlang
parse_before_param(<<C, R/bits>>, Acc) when ?IS_LC_ALPHA(C) or (C =:= $*) ->
    parse_param(R, Acc, <<C>>).
```

### parse_param

```erlang
parse_param(<<$;, R/bits>>, Acc, K) ->
    parse_before_param(R, lists:keystore(K, 1, Acc, {K, true}));
```

### parse_param

```erlang
parse_param(<<$=, R0/bits>>, Acc, K) ->
    case parse_bare_item(R0) of
        {Item, <<$;, R/bits>>} ->
            parse_before_param(R, lists:keystore(K, 1, Acc, {K, Item}));
        {Item, R} ->
            {lists:keystore(K, 1, Acc, {K, Item}), R}
    end;
```

### parse_param

```erlang
parse_param(<<C, R/bits>>, Acc, K) when
    ?IS_LC_ALPHA(C) or ?IS_DIGIT(C) or
        (C =:= $_) or (C =:= $-) or (C =:= $.) or (C =:= $*) ->
    parse_param(R, Acc, <<K/binary, C>>);
```

### parse_param

```erlang
parse_param(R, Acc, K) ->
    {lists:keystore(K, 1, Acc, {K, true}), R}.
```

### parse_bare_item

Parse an integer or decimal.

```erlang
parse_bare_item(<<$-, R/bits>>) -> parse_number(R, 0, <<$->>);
```

### parse_bare_item

Parse an integer or decimal.

```erlang
parse_bare_item(<<C, R/bits>>) when ?IS_DIGIT(C) -> parse_number(R, 1, <<C>>);
```

### parse_bare_item

Parse an integer or decimal.

```erlang
parse_bare_item(<<$", R/bits>>) ->
    % Parse a string.
```

### parse_bare_item

```erlang
parse_bare_item(<<C, R/bits>>) when ?IS_ALPHA(C) or (C =:= $*) ->
    % Parse a token.
```

### parse_bare_item

```erlang
parse_bare_item(<<$:, R/bits>>) ->
    % Parse a byte sequence.
```

### parse_bare_item

```erlang
parse_bare_item(<<"?0", R/bits>>) ->
    % Parse a boolean false.
```

### parse_bare_item

```erlang
parse_bare_item(<<"?1", R/bits>>) ->
    % Parse a boolean true.
```

### parse_number

Parse an integer or decimal binary.

```erlang
parse_number(<<C, R/bits>>, L, Acc) when ?IS_DIGIT(C) ->
    parse_number(R, L + 1, <<Acc/binary, C>>);
```

### parse_number

Parse an integer or decimal binary.

```erlang
parse_number(<<$., R/bits>>, L, Acc) ->
    parse_decimal(R, L, 0, Acc, <<>>);
```

### parse_number

Parse an integer or decimal binary.

```erlang
parse_number(R, L, Acc) when L =< 15 ->
    {binary_to_integer(Acc), R}.
```

### parse_decimal

Parse a decimal binary.

```erlang
parse_decimal(<<C, R/bits>>, L1, L2, IntAcc, FracAcc) when ?IS_DIGIT(C) ->
    parse_decimal(R, L1, L2 + 1, IntAcc, <<FracAcc/binary, C>>);
```

### parse_decimal

Parse a decimal binary.

```erlang
parse_decimal(R, L1, L2, IntAcc, FracAcc0) when L1 =< 12, L2 >= 1, L2 =< 3 ->
    %% While not strictly required this gives a more consistent representation.
```

### parse_string

Parse a string binary.

```erlang
parse_string(<<$\\, $", R/bits>>, Acc) ->
    parse_string(R, <<Acc/binary, $">>);
```

### parse_string

Parse a string binary.

```erlang
parse_string(<<$\\, $\\, R/bits>>, Acc) ->
    parse_string(R, <<Acc/binary, $\\>>);
```

### parse_string

Parse a string binary.

```erlang
parse_string(<<$", R/bits>>, Acc) ->
    {{string, Acc}, R};
```

### parse_string

Parse a string binary.

```erlang
parse_string(<<C, R/bits>>, Acc) when
        C >= 16#20, C =< 16#21;
        C >= 16#23, C =< 16#5b;
        C >= 16#5d, C =< 16#7e ->
    parse_string(R, <<Acc/binary, C>>).
```

### parse_token

Parse a token binary.

```erlang
parse_token(<<C, R/bits>>, Acc) when ?IS_TOKEN(C) or (C =:= $:) or (C =:= $/) ->
    parse_token(R, <<Acc/binary, C>>);
```

### parse_token

Parse a token binary.

```erlang
parse_token(R, Acc) ->
    {{token, Acc}, R}.
```

### parse_binary

Parse a byte sequence binary.

```erlang
parse_binary(Bin) when is_binary(Bin) ->
    parse_binary(Bin, <<>>).
```

### parse_binary

```erlang
parse_binary(<<$:, R/bits>>, Acc) ->
    {{binary, base64:decode(Acc)}, R};
```

### parse_binary

```erlang
parse_binary(<<C, R/bits>>, Acc) when ?IS_ALPHANUM(C) or (C =:= $+) or (C =:= $/) or (C =:= $=) ->
    parse_binary(R, <<Acc/binary, C>>).
```

### parse_struct_hd_test_

```erlang
parse_struct_hd_test_() ->
    Files = filelib:wildcard("deps/structured-header-tests/*.json"),
    lists:flatten([
        begin
            {ok, JSON} = file:read_file(File),
            Tests = json:decode(JSON),
            [
                {iolist_to_binary(io_lib:format("~s: ~s", [filename:basename(File), Name])), fun() ->
                    %% The implementation is strict. We fail whenever we can.
```

### expected_to_term

```erlang
expected_to_term([Bare, []]) when
        is_boolean(Bare); is_number(Bare); is_binary(Bare); is_map(Bare) ->
    {item, e2tb(Bare), []};
```

### expected_to_term

```erlang
expected_to_term([Bare, Params = [[<<_/bits>>, _] | _]]) when
        is_boolean(Bare); is_number(Bare); is_binary(Bare); is_map(Bare) ->
    {item, e2tb(Bare), e2tp(Params)};
%% Empty list or dictionary.
```

### expected_to_term

```erlang
expected_to_term([]) ->
    [];
%% Dictionary.
```

### expected_to_term

```erlang
expected_to_term(Dict = [[<<_/bits>>, V] | _]) when V =/= [] ->
    e2t(Dict);
%% Outer list.
```

### expected_to_term

```erlang
expected_to_term(List) when is_list(List) ->
    [ e2t(E) || E <- List ].
```

### e2t

```erlang
e2t(Dict = [[<<_/bits>>, _] | _]) ->
    [{K, e2t(V)} || [K, V] <- Dict];
%% Inner list.
```

### e2t

```erlang
e2t([List, Params]) when is_list(List) ->
    {list, [ e2t(E) || E <- List ], e2tp(Params)};
%% Item.
```

### e2t

```erlang
e2t([Bare, Params]) ->
    {item, e2tb(Bare), e2tp(Params)}.
```

### e2tb

```erlang
e2tb(#{<<"__type">> := <<"token">>, <<"value">> := V}) ->
    {token, V};
```

### e2tb

```erlang
e2tb(#{<<"__type">> := <<"binary">>, <<"value">> := V}) ->
    {binary, base32:decode(V)};
```

### e2tb

```erlang
e2tb(V) when is_binary(V) ->
    {string, V};
```

### e2tb

```erlang
e2tb(V) when is_float(V) ->
    %% There should be no rounding needed for the test cases.
```

### e2tb

```erlang
e2tb(V) ->
    V.
```

### e2tp

```erlang
e2tp([]) ->
    [];
```

### e2tp

```erlang
e2tp(Params) ->
    [{K, e2tb(V)} || [K, V] <- Params].
```

### raw_to_binary

```erlang
raw_to_binary(RawList) ->
    trim_ws(iolist_to_binary(lists:join(<<", ">>, RawList))).
```

### trim_ws

```erlang
trim_ws(<<$\s, R/bits>>) -> trim_ws(R);
```

### trim_ws

```erlang
trim_ws(R) -> trim_ws_end(R, byte_size(R) - 1).
```

### trim_ws_end

```erlang
trim_ws_end(_, -1) ->
    <<>>;
```

### trim_ws_end

```erlang
trim_ws_end(Value, N) ->
    case binary:at(Value, N) of
        $\s ->
            trim_ws_end(Value, N - 1);
        _ ->
            S = N + 1,
            <<Value2:S/binary, _/bits>> = Value,
            Value2
    end.
```

### dictionary

```erlang
-spec dictionary(#{binary() => sh_item() | sh_inner_list()} | sh_dictionary()) ->
    iolist().
```

```erlang
dictionary(Map) when is_map(Map) ->
    dictionary(maps:to_list(Map));
```

### dictionary

```erlang
-spec dictionary(#{binary() => sh_item() | sh_inner_list()} | sh_dictionary()) ->
    iolist().
```

```erlang
dictionary(KVList) when is_list(KVList) ->
    lists:join(
        <<", ">>,
        [
            case Value of
                true -> Key;
                _ -> [Key, $=, item_or_inner_list(Value)]
            end
        ||
            {Key, Value} <- KVList
        ]
    ).
```

### item_or_inner_list

```erlang
-spec list(sh_list()) -> iolist().
list(List) ->
    lists:join(<<", ">>, [item_or_inner_list(Value) || Value <- List]).
```

```erlang
item_or_inner_list(Value = {list, _, _}) ->
    inner_list(Value);
```

### item_or_inner_list

```erlang
-spec list(sh_list()) -> iolist().
list(List) ->
    lists:join(<<", ">>, [item_or_inner_list(Value) || Value <- List]).
```

```erlang
item_or_inner_list(Value) ->
    item(Value).
```

### inner_list

```erlang
inner_list({list, List, Params}) ->
    [$(, lists:join($\s, [item(Value) || Value <- List]), $), params(Params)].
```

### bare_item

```erlang
bare_item({string, String}) ->
    [$", escape_string(String, <<>>), $"];
%% @todo Must fail if Token has invalid characters.
```

### bare_item

```erlang
bare_item({token, Token}) ->
    Token;
```

### bare_item

```erlang
bare_item({binary, Binary}) ->
    [$:, base64:encode(Binary), $:];
```

### bare_item

```erlang
bare_item({decimal, {Base, Exp}}) when Exp >= 0 ->
    Mul =
        case Exp of
            0 -> 1;
            1 -> 10;
            2 -> 100;
            3 -> 1000;
            4 -> 10000;
            5 -> 100000;
            6 -> 1000000;
            7 -> 10000000;
            8 -> 100000000;
            9 -> 1000000000;
            10 -> 10000000000;
            11 -> 100000000000;
            12 -> 1000000000000
        end,
    MaxLenWithSign =
        if
            Base < 0 -> 13;
            true -> 12
        end,
    Bin = integer_to_binary(Base * Mul),
    true = byte_size(Bin) =< MaxLenWithSign,
    [Bin, <<".0">>];
```

### bare_item

```erlang
bare_item({decimal, {Base, -1}}) ->
    Int = Base div 10,
    Frac = abs(Base) rem 10,
    [integer_to_binary(Int), $., integer_to_binary(Frac)];
```

### bare_item

```erlang
bare_item({decimal, {Base, -2}}) ->
    Int = Base div 100,
    Frac = abs(Base) rem 100,
    [integer_to_binary(Int), $., integer_to_binary(Frac)];
```

### bare_item

```erlang
bare_item({decimal, {Base, -3}}) ->
    Int = Base div 1000,
    Frac = abs(Base) rem 1000,
    [integer_to_binary(Int), $., integer_to_binary(Frac)];
```

### bare_item

```erlang
bare_item({decimal, {Base, Exp}}) ->
    Div = exp_div(Exp),
    Int0 = Base div Div,
    true = abs(Int0) < 1000000000000,
    Frac0 = abs(Base) rem Div,
    DivFrac = Div div 1000,
    Frac1 = Frac0 div DivFrac,
    {Int, Frac} =
        if
            (Frac0 rem DivFrac) > (DivFrac div 2) ->
                case Frac1 of
                    999 when Int0 < 0 -> {Int0 - 1, 0};
                    999 -> {Int0 + 1, 0};
                    _ -> {Int0, Frac1 + 1}
                end;
            true ->
                {Int0, Frac1}
        end,
    [
        integer_to_binary(Int),
        $.,
        if
            Frac < 10 -> [$0, $0, integer_to_binary(Frac)];
            Frac < 100 -> [$0, integer_to_binary(Frac)];
            true -> integer_to_binary(Frac)
        end
    ];
```

### bare_item

```erlang
bare_item(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
```

### bare_item

```erlang
bare_item(true) ->
    <<"?1">>;
```

### bare_item

```erlang
bare_item(false) ->
    <<"?0">>.
```

### exp_div

```erlang
exp_div(0) -> 1;
```

### exp_div

```erlang
exp_div(N) -> 10 * exp_div(N + 1).
```

### escape_string

```erlang
escape_string(<<>>, Acc) -> Acc;
```

### escape_string

```erlang
escape_string(<<$\\, R/bits>>, Acc) -> escape_string(R, <<Acc/binary, $\\, $\\>>);
```

### escape_string

```erlang
escape_string(<<$", R/bits>>, Acc) -> escape_string(R, <<Acc/binary, $\\, $">>);
```

### escape_string

```erlang
escape_string(<<C, R/bits>>, Acc) -> escape_string(R, <<Acc/binary, C>>).
```

### params

```erlang
params(Params) ->
    [
        case Param of
            {Key, true} -> [$;, Key];
            {Key, Value} -> [$;, Key, $=, bare_item(Value)]
        end
    ||
        Param <- Params
    ].
```

### to_dictionary_test

```erlang
to_dictionary_test() ->
    {ok, SfDictionary} = to_dictionary(#{
        foo => bar,
        <<"fizz">> => <<"buzz">>,
        <<"item-with">> => { item, <<"params">>, [{first, param}, {another, true}] },
        <<"int-item">> => 1,
        <<"int-item-with-params">> => { item, 1, [{int, <<"param">>}] },
        <<"no">> => <<"params">>,
        <<"empty">> => {item, params, []},
        inner => [<<"a">>, b, true, 3],
        inner_with_params => {list, [{item, 1, []}, 2], [{first, param}]},
        inner_inner_params => [{item, 1, [{heres, <<"one">>}]}, 2]
    }),
    ?assertEqual(
        {<<"foo">>, {item, {token,<<"bar">>}, []}},
        lists:keyfind(<<"foo">>, 1, SfDictionary)    
    ),
    ?assertEqual(
        {<<"fizz">>, {item, {string,<<"buzz">>}, []}},
        lists:keyfind(<<"fizz">>, 1, SfDictionary)    
    ),
    ?assertEqual(
        {<<"item-with">>,
            {item,
                {string,<<"params">>},
                [{<<"first">>, {token,<<"param">>}}, {<<"another">>, true}]
            }
        },
        lists:keyfind(<<"item-with">>, 1, SfDictionary)    
    ),
    ?assertEqual(
        {<<"int-item">>, {item, 1, []}},
        lists:keyfind(<<"int-item">>, 1, SfDictionary)    
    ),
    ?assertEqual(
        {<<"int-item-with-params">>, {item, 1, [{<<"int">>, {string, <<"param">>}}]}}, 
        lists:keyfind(<<"int-item-with-params">>, 1, SfDictionary)
    ),
    ?assertEqual(
        {<<"no">>, {item, {string, <<"params">>}, []}}, 
        lists:keyfind(<<"no">>, 1, SfDictionary)
    ),
    ?assertEqual(
        {<<"empty">>, {item, {token, <<"params">>}, []}}, 
        lists:keyfind(<<"empty">>, 1, SfDictionary)
    ),
    ?assertEqual(
        {
            <<"inner">>,
            {
                list,
                [
                    {item, {string, <<"a">>}, []},
                    {item, {token, <<"b">>}, []},
                    {item, true, []},
                    {item, 3, []}
                ],
                []
            }
        },
        lists:keyfind(<<"inner">>, 1, SfDictionary)
    ),
    ?assertEqual(
        {<<"inner_with_params">>,
            {list,
                [{item, 1, []}, {item, 2, []}],
                [{<<"first">>, {token, <<"param">>}}]
            }
        },
        lists:keyfind(<<"inner_with_params">>, 1, SfDictionary)
    ),
    ?assertEqual(
       {<<"inner_inner_params">>,
            {list,
                [{item, 1, [{<<"heres">>, {string, <<"one">>}}]}, {item, 2, []}],
                []
            }
        },
        lists:keyfind(<<"inner_inner_params">>, 1, SfDictionary)
    ),
    dictionary(SfDictionary).
```

### to_dictionary_depth_test

```erlang
to_dictionary_depth_test() ->
    {too_deep, _} = to_dictionary(#{
        foo => #{ bar => buzz }
    }),
    {too_deep, _} = to_dictionary(#{
        foo => [1, 2, [3]]
    }),
    ok.
```

### to_item_test

```erlang
to_item_test() ->
    ?assertEqual(to_item(1), {ok, {item, 1, []}}),
    ?assertEqual(to_item(true), {ok, {item, true, []}}),
    ?assertEqual(to_item(<<"foobar">>), {ok, {item, {string, <<"foobar">>}, []}}),
    ?assertEqual(to_item("foobar"), {ok, {item, {string, <<"foobar">>}, []}}),
    ?assertEqual(to_item(foobar), {ok, {item, {token, <<"foobar">>}, []}}),
    ?assertEqual(
        to_item({item, "foobar", [{first, param}]}),
        {ok, {item, {string, <<"foobar">>}, [{<<"first">>, {token, <<"param">>}}]}}
    ),
    ok.
```

### to_list_test

```erlang
to_list_test() ->
    ?assertEqual(
        to_list(
            [1, 2, <<"three">>, [4, <<"five">>],
                {list, [6, <<"seven">>],
                    [{<<"first">>, {token, <<"param">>}}]
                }
            ]
        ),
        {ok, [
            {item, 1, []},
            {item, 2, []},
            {item, {string, <<"three">>}, []},
            {list, [{ item, 4, []}, {item, {string, <<"five">>}, []}], []},
            {list,
                [{ item, 6, []}, {item, {string, <<"seven">>}, []}],
                [{<<"first">>, {token, <<"param">>}}]
            }
        ]}
    ),
    ok.
```

### to_list_depth_test

```erlang
to_list_depth_test() ->
    {too_deep, _} = to_list([1,2,3, [4, [5]]]),
    {too_deep, _} = to_list([1,2,3, #{ foo => bar } ]),
    {too_deep, _} = to_list([1,2,3, [#{ foo => bar }] ]),
    ok.
```

---

*Generated from [hb_structured_fields.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_structured_fields.erl)*
