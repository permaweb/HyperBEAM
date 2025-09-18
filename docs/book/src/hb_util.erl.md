# hb_util

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_util.erl)

Simple type coercion functions, useful for quickly turning inputs from the
HTTP API into the correct types for the HyperBEAM runtime, if they are not
annotated by the user.

---

## Exported Functions

- `all_atoms/0`
- `all_hb_modules/0`
- `atom/1`
- `bin/1`
- `binary_is_atom/1`
- `binary_to_addresses/1`
- `ceil_int/2`
- `check_size/2`
- `check_type/2`
- `check_value/2`
- `count/2`
- `decode/1`
- `deep_get/3`
- `deep_get/4`
- `deep_merge/3`
- `deep_set/4`
- `encode/1`
- `find_target_path/2`
- `find_value/2`
- `find_value/3`
- `float/1`
- `floor_int/2`
- `hd/1`
- `hd/2`
- `hd/3`
- `human_id/1`
- `human_int/1`
- `id/1`
- `id/2`
- `int/1`
- `is_hb_module/1`
- `is_hb_module/2`
- `is_ordered_list/2`
- `is_string_list/1`
- `key_to_atom/1`
- `key_to_atom/2`
- `list_replace/3`
- `list_to_numbered_message/1`
- `list_with/2`
- `list_without/2`
- `list/1`
- `lower_case_key_map/2`
- `map/1`
- `maybe_throw/2`
- `mean/1`
- `message_to_ordered_list/1`
- `message_to_ordered_list/2`
- `native_id/1`
- `number/1`
- `numbered_keys_to_list/2`
- `ok_or_throw/3`
- `ok/1`
- `ok/2`
- `remove_common/2`
- `safe_decode/1`
- `safe_encode/1`
- `split_depth_string_aware_single/2`
- `split_depth_string_aware/2`
- `split_escaped_single/2`
- `stddev/1`
- `template_matches/3`
- `to_hex/1`
- `to_lower/1`
- `to_sorted_keys/1`
- `to_sorted_keys/2`
- `to_sorted_list/1`
- `to_sorted_list/2`
- `unique/1`
- `until/1`
- `until/2`
- `until/3`
- `variance/1`
- `weighted_random/1`

---

### int

A collection of utility functions for building with HyperBEAM.
Coerce a string to an integer.

```erlang
int(Str) when is_binary(Str) ->
    list_to_integer(binary_to_list(Str));
```

### int

A collection of utility functions for building with HyperBEAM.
Coerce a string to an integer.

```erlang
int(Str) when is_list(Str) ->
    list_to_integer(Str);
```

### int

A collection of utility functions for building with HyperBEAM.
Coerce a string to an integer.

```erlang
int(Int) when is_integer(Int) ->
    Int.
```

### float

Coerce a string to a float.

```erlang
float(Str) when is_binary(Str) ->
    list_to_float(binary_to_list(Str));
```

### float

Coerce a string to a float.

```erlang
float(Str) when is_list(Str) ->
    list_to_float(Str);
```

### float

Coerce a string to a float.

```erlang
float(Float) when is_float(Float) ->
    Float;
```

### float

Coerce a string to a float.

```erlang
float(Int) when is_integer(Int) ->
    Int / 1.
```

### atom

Coerce a string to an atom.

```erlang
atom(Str) when is_binary(Str) ->
    list_to_existing_atom(binary_to_list(Str));
```

### atom

Coerce a string to an atom.

```erlang
atom(Str) when is_list(Str) ->
    list_to_existing_atom(Str);
```

### atom

Coerce a string to an atom.

```erlang
atom(Atom) when is_atom(Atom) ->
    Atom.
```

### bin

Coerce a value to a binary.

```erlang
bin(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
```

### bin

Coerce a value to a binary.

```erlang
bin(Value) when is_integer(Value) ->
    integer_to_binary(Value);
```

### bin

Coerce a value to a binary.

```erlang
bin(Value) when is_float(Value) ->
    float_to_binary(Value, [{decimals, 10}, compact]);
```

### bin

Coerce a value to a binary.

```erlang
bin(Value) when is_list(Value) ->
    list_to_binary(Value);
```

### bin

Coerce a value to a binary.

```erlang
bin(Value) when is_binary(Value) ->
    Value.
```

### list

Coerce a value to a string list.

```erlang
list(Value) when is_binary(Value) ->
    binary_to_list(Value);
```

### list

Coerce a value to a string list.

```erlang
list(Value) when is_list(Value) -> Value;
```

### list

Coerce a value to a string list.
Ensure that a value is a map. Only supports maps and lists of key-value

```erlang
list(Value) when is_atom(Value) -> atom_to_list(Value).
```

### map

Coerce a value to a string list.
Ensure that a value is a map. Only supports maps and lists of key-value

```erlang
map(Value) when is_list(Value) ->
    maps:from_list(Value);
```

### map

Coerce a value to a string list.
Ensure that a value is a map. Only supports maps and lists of key-value

```erlang
map(Value) when is_map(Value) ->
    Value.
```

### ceil_int

```erlang
ceil_int(IntValue, Nearest) ->
	IntValue - (IntValue rem Nearest) + Nearest.
```

### floor_int

```erlang
floor_int(IntValue, Nearest) ->
	IntValue - (IntValue rem Nearest).
```

### ok

Unwrap a tuple of the form `{ok, Value}`, or throw/return, depending on

```erlang
ok(Value) -> ok(Value, #{}).
```

### ok

Unwrap a tuple of the form `{ok, Value}`, or throw/return, depending on

```erlang
ok({ok, Value}, _Opts) -> Value;
```

### ok

Unwrap a tuple of the form `{ok, Value}`, or throw/return, depending on

```erlang
ok(Other, Opts) ->
	case hb_opts:get(error_strategy, throw, Opts) of
		throw -> throw({unexpected, Other});
		_ -> {unexpected, Other}
	end.
```

### until

Utility function to wait for a condition to be true. Optionally,

```erlang
until(Condition) ->
    until(Condition, 0).
```

### until

```erlang
until(Condition, Count) ->
    until(Condition, fun() -> receive after 100 -> 1 end end, Count).
```

### until

```erlang
until(Condition, Fun, Count) ->
    case Condition() of
        false ->
            case apply(Fun, hb_ao:truncate_args(Fun, [Count])) of
                {count, AddToCount} ->
                    until(Condition, Fun, Count + AddToCount);
                _ ->
                    until(Condition, Fun, Count + 1)
            end;
        true -> Count
    end.
```

### id

Return the human-readable form of an ID of a message when given either

```erlang
id(Item) -> id(Item, unsigned).
```

### id

Return the human-readable form of an ID of a message when given either

```erlang
id(TX, Type) when is_record(TX, tx) ->
    encode(ar_bundles:id(TX, Type));
```

### id

Return the human-readable form of an ID of a message when given either

```erlang
id(Map, Type) when is_map(Map) ->
    hb_message:id(Map, Type);
```

### id

Return the human-readable form of an ID of a message when given either

```erlang
id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    Bin;
```

### id

Return the human-readable form of an ID of a message when given either

```erlang
id(Bin, _) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    encode(Bin);
```

### id

Return the human-readable form of an ID of a message when given either

```erlang
id(Data, Type) when is_list(Data) ->
    id(list_to_binary(Data), Type).
```

### to_lower

Convert a binary to a lowercase.

```erlang
to_lower(Str) ->
    string:lowercase(Str).
```

### is_string_list

Is the given term a string list?

```erlang
is_string_list(MaybeString) ->
    lists:all(fun is_integer/1, MaybeString).
```

### to_sorted_list

Given a map or KVList, return a deterministically sorted list of its

```erlang
to_sorted_list(Msg) ->
    to_sorted_list(Msg, #{}).
```

### to_sorted_list

```erlang
to_sorted_list(Msg, Opts) when is_map(Msg) ->
	to_sorted_list(hb_maps:to_list(Msg, Opts), Opts);
```

### to_sorted_list

```erlang
to_sorted_list(Msg = [{_Key, _} | _], _Opts) when is_list(Msg) ->
	lists:sort(fun({Key1, _}, {Key2, _}) -> Key1 < Key2 end, Msg);
```

### to_sorted_list

```erlang
to_sorted_list(Msg, _Opts) when is_list(Msg) ->
	lists:sort(fun(Key1, Key2) -> Key1 < Key2 end, Msg).
```

### to_sorted_keys

Given a map or KVList, return a deterministically ordered list of its keys.

```erlang
to_sorted_keys(Msg) ->
	to_sorted_keys(Msg, #{}).
```

### to_sorted_keys

```erlang
to_sorted_keys(Msg, Opts) when is_map(Msg) ->
    to_sorted_keys(hb_maps:keys(Msg, Opts), Opts);
```

### to_sorted_keys

```erlang
to_sorted_keys(Msg, _Opts) when is_list(Msg) ->
    lists:sort(fun(Key1, Key2) -> Key1 < Key2 end, Msg).
```

### key_to_atom

Convert keys in a map to atoms, lowering `-` to `_`.

```erlang
key_to_atom(Key) -> key_to_atom(Key, existing).
```

### key_to_atom

Convert keys in a map to atoms, lowering `-` to `_`.

```erlang
key_to_atom(Key, _Mode) when is_atom(Key) -> Key;
```

### key_to_atom

Convert keys in a map to atoms, lowering `-` to `_`.

```erlang
key_to_atom(Key, Mode) ->
    WithoutDashes = to_lower(binary:replace(Key, <<"-">>, <<"_">>, [global])),
    case Mode of
        new_atoms -> binary_to_atom(WithoutDashes, utf8);
        _ -> binary_to_existing_atom(WithoutDashes, utf8)
    end.
```

### native_id

Convert a human readable ID to a native binary ID. If the ID is already

```erlang
native_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    decode(Bin);
```

### native_id

Convert a human readable ID to a native binary ID. If the ID is already

```erlang
native_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    Bin;
```

### native_id

Convert a human readable ID to a native binary ID. If the ID is already

```erlang
native_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 42 ->
    Bin;
```

### native_id

Convert a human readable ID to a native binary ID. If the ID is already

```erlang
native_id(Wallet = {_Priv, _Pub}) ->
    native_id(ar_wallet:to_address(Wallet)).
```

### human_id

Convert a native binary ID to a human readable ID. If the ID is already

```erlang
human_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 32 ->
    encode(Bin);
```

### human_id

Convert a native binary ID to a human readable ID. If the ID is already

```erlang
human_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 43 ->
    Bin;
```

### human_id

Convert a native binary ID to a human readable ID. If the ID is already

```erlang
human_id(Bin) when is_binary(Bin) andalso byte_size(Bin) == 42 ->
    Bin;
```

### human_id

Convert a native binary ID to a human readable ID. If the ID is already

```erlang
human_id(Wallet = {_Priv, _Pub}) ->
    human_id(ar_wallet:to_address(Wallet)).
```

### human_int

Add `,` characters to a number every 3 digits to make it human readable.

```erlang
human_int(Float) when is_float(Float) ->
    human_int(erlang:round(Float));
```

### human_int

Add `,` characters to a number every 3 digits to make it human readable.

```erlang
human_int(Int) ->
    lists:reverse(add_commas(lists:reverse(integer_to_list(Int)))).
```

### add_commas

```erlang
add_commas([A,B,C,Z|Rest]) -> [A,B,C,$,|add_commas([Z|Rest])];
```

### add_commas

Encode a binary to URL safe base64 binary string.

```erlang
add_commas(List) -> List.
```

### encode

Encode a binary to URL safe base64 binary string.

```erlang
encode(Bin) ->
    b64fast:encode(Bin).
```

### decode

Try to decode a URL safe base64 into a binary or throw an error when

```erlang
decode(Input) ->
    b64fast:decode(Input).
```

### safe_encode

Safely encode a binary to URL safe base64.

```erlang
safe_encode(Bin) when is_binary(Bin) ->
    encode(Bin);
```

### safe_encode

Safely encode a binary to URL safe base64.

```erlang
safe_encode(Bin) ->
    Bin.
```

### safe_decode

Safely decode a URL safe base64 into a binary returning an ok or error

```erlang
safe_decode(E) ->
    try
        D = decode(E),
        {ok, D}
    catch
        _:_ -> {error, invalid}
    end.
```

### to_hex

Convert a binary to a hex string. Do not use this for anything other than

```erlang
to_hex(Bin) when is_binary(Bin) ->
    to_lower(
        iolist_to_binary(
            [io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Bin)]
        )
    ).
```

### deep_merge

Deep merge two maps, recursively merging nested maps.

```erlang
deep_merge(Map1, Map2, Opts) when is_map(Map1), is_map(Map2) ->
    hb_maps:fold(
        fun(Key, Value2, AccMap) ->
            case deep_get(Key, AccMap, Opts) of
                Value1 when is_map(Value1), is_map(Value2) ->
                    % Both values are maps, recursively merge them
                    deep_set(Key, deep_merge(Value1, Value2, Opts), AccMap, Opts);
                _ ->
                    % Either the key doesn't exist in Map1 or at least one of 
                    % the values isn't a map. Simply use the value from Map2
                    deep_set(Key, Value2, AccMap, Opts)
            end
        end,
        Map1,
        Map2,
		Opts
    ).
```

### deep_set

Set a deep value in a message by its path, _assuming all messages are

```erlang
deep_set(_Path, undefined, Msg, _Opts) -> Msg;
```

### deep_set

Set a deep value in a message by its path, _assuming all messages are

```erlang
deep_set(Path, Value, Msg, Opts) when not is_list(Path) ->
    deep_set(hb_path:term_to_path_parts(Path, Opts), Value, Msg, Opts);
```

### deep_set

Set a deep value in a message by its path, _assuming all messages are

```erlang
deep_set([Key], unset, Msg, Opts) ->
    hb_maps:remove(Key, Msg, Opts);
```

### deep_set

Set a deep value in a message by its path, _assuming all messages are

```erlang
deep_set([Key], Value, Msg, Opts) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        ExistingMap when is_map(ExistingMap) andalso is_map(Value) ->
            % If both are maps, merge them
            Msg#{ Key => hb_maps:merge(ExistingMap, Value, Opts) };
        _ ->
            Msg#{ Key => Value }
    end;
```

### deep_set

Set a deep value in a message by its path, _assuming all messages are

```erlang
deep_set([Key|Rest], Value, Map, Opts) ->
    SubMap = hb_maps:get(Key, Map, #{}, Opts),
    hb_maps:put(Key, deep_set(Rest, Value, SubMap, Opts), Map, Opts).
```

### deep_get

Get a deep value from a message.

```erlang
deep_get(Path, Msg, Opts) -> deep_get(Path, Msg, not_found, Opts).
```

### deep_get

Get a deep value from a message.

```erlang
deep_get(Path, Msg, Default, Opts) when not is_list(Path) ->
    deep_get(hb_path:term_to_path_parts(Path, Opts), Msg, Default, Opts);
```

### deep_get

Get a deep value from a message.

```erlang
deep_get([Key], Msg, Default, Opts) ->
    case hb_maps:find(Key, Msg, Opts) of
        {ok, Value} -> Value;
        error -> Default
    end;
```

### deep_get

Get a deep value from a message.

```erlang
deep_get([Key|Rest], Msg, Default, Opts) ->
    case hb_maps:find(Key, Msg, Opts) of
        {ok, DeepMsg} when is_map(DeepMsg) ->
            deep_get(Rest, DeepMsg, Default, Opts);
        error -> Default
    end.
```

### find_target_path

Find the target path to route for a request message.

```erlang
find_target_path(Msg, Opts) ->
    case hb_ao:get(<<"route-path">>, Msg, not_found, Opts) of
        not_found ->
            ?event({find_target_path, {msg, Msg}, not_found}),
            hb_ao:get(<<"path">>, Msg, no_path, Opts);
        RoutePath -> RoutePath
    end.
```

### template_matches

Check if a message matches a given template.

```erlang
template_matches(ToMatch, Template, _Opts) when is_map(Template) ->
    case hb_message:match(Template, ToMatch, primary) of
        {value_mismatch, _Key, _Val1, _Val2} -> false;
        Match -> Match
    end;
```

### template_matches

Check if a message matches a given template.

```erlang
template_matches(ToMatch, Regex, Opts) when is_binary(Regex) ->
    MsgPath = find_target_path(ToMatch, Opts),
    hb_path:regex_matches(MsgPath, Regex).
```

### number

Label a list of elements with a number.

```erlang
number(List) ->
    lists:map(
        fun({N, Item}) -> {integer_to_binary(N), Item} end,
        lists:zip(lists:seq(1, length(List)), List)
    ).
```

### list_to_numbered_message

Convert a list of elements to a map with numbered keys.

```erlang
list_to_numbered_message(Msg) when is_map(Msg) ->
    case is_ordered_list(Msg, #{}) of
        true -> Msg;
        false ->
            throw({cannot_convert_to_numbered_message, Msg})
    end;
```

### list_to_numbered_message

Convert a list of elements to a map with numbered keys.

```erlang
list_to_numbered_message(List) ->
    hb_maps:from_list(number(List)).
```

### is_ordered_list

Determine if the message given is an ordered list, starting from 1.

```erlang
is_ordered_list(Msg, _Opts) when is_list(Msg) -> true;
```

### is_ordered_list

Determine if the message given is an ordered list, starting from 1.

```erlang
is_ordered_list(Msg, Opts) ->
    is_ordered_list(1, hb_ao:normalize_keys(Msg, Opts), Opts).
```

### is_ordered_list

```erlang
is_ordered_list(_, Msg, _Opts) when map_size(Msg) == 0 -> true;
```

### is_ordered_list

```erlang
is_ordered_list(N, Msg, _Opts) ->
    case maps:get(NormKey = hb_ao:normalize_key(N), Msg, not_found) of
        not_found -> false;
        _ ->
            is_ordered_list(
                N + 1,
                maps:without([NormKey], Msg),
				_Opts
            )
    end.
```

### list_replace

Replace a key in a list with a new value.

```erlang
list_replace(List, Key, Value) ->
    lists:foldr(
        fun(Elem, Acc) ->
            case Elem of
                Key when is_list(Value) -> Value ++ Acc;
                Key -> [Value | Acc];
                _ -> [Elem | Acc]
            end
        end,
        [],
        List
    ).
```

### unique

Take a list and return a list of unique elements. The function is

```erlang
unique(List) ->
    Unique =
        lists:foldl(
            fun(Item, Acc) ->
                case lists:member(Item, Acc) of
                    true -> Acc;
                    false -> [Item | Acc]
                end
            end,
            [],
            List
        ),
    lists:reverse(Unique).
```

### list_with

Returns the intersection of two lists, with stable ordering.

```erlang
list_with(List1, List2) ->
    lists:filter(fun(Item) -> lists:member(Item, List2) end, List1).
```

### list_without

Remove all occurrences of all items in the first list from the second list.

```erlang
list_without(List1, List2) ->
    lists:filter(fun(Item) -> not lists:member(Item, List1) end, List2).
```

### message_to_ordered_list

Take a message with numbered keys and convert it to a list of tuples

```erlang
message_to_ordered_list(Message) ->
    message_to_ordered_list(Message, #{}).
```

### message_to_ordered_list

```erlang
message_to_ordered_list(Message, _Opts) when ?IS_EMPTY_MESSAGE(Message) ->
    [];
```

### message_to_ordered_list

```erlang
message_to_ordered_list(List, _Opts) when is_list(List) ->
    List;
```

### message_to_ordered_list

```erlang
message_to_ordered_list(Message, Opts) ->
    NormMessage = hb_ao:normalize_keys(Message, Opts),
    Keys = hb_maps:keys(NormMessage, Opts) -- [<<"priv">>, <<"commitments">>],
    SortedKeys =
        lists:map(
            fun hb_ao:normalize_key/1,
            lists:sort(lists:map(fun int/1, Keys))
        ),
    message_to_ordered_list(NormMessage, SortedKeys, erlang:hd(SortedKeys), Opts).
```

### message_to_ordered_list

```erlang
message_to_ordered_list(_Message, [], _Key, _Opts) ->
    [];
```

### message_to_ordered_list

```erlang
message_to_ordered_list(Message, [Key|Keys], Key, Opts) ->
    case hb_maps:get(Key, Message, undefined, Opts#{ hashpath => ignore }) of
        undefined ->
            throw(
                {missing_key,
                    {key, Key},
                    {remaining_keys, Keys},
                    {message, Message}
                }
            );
        Value ->
            [
                Value
            |
                message_to_ordered_list(
                    Message,
                    Keys,
                    hb_ao:normalize_key(int(Key) + 1),
                    Opts
                )
            ]
    end;
```

### message_to_ordered_list

```erlang
message_to_ordered_list(Message, [Key|_Keys], ExpectedKey, _Opts) ->
    throw({missing_key, {expected, ExpectedKey, {next, Key}, {message, Message}}}).
```

### numbered_keys_to_list

Convert a message with numbered keys and others to a sorted list with only

```erlang
numbered_keys_to_list(Message, Opts) ->
    OnlyNumbered =
        hb_maps:filter(
            fun(Key, _Value) ->
                try int(hb_ao:normalize_key(Key)) of
                    IntKey when is_integer(IntKey) -> true;
                    _ -> false
                catch _:_ -> false
                end
            end,
            Message,
            Opts
        ),
    message_to_ordered_list(OnlyNumbered, Opts).
```

### hd

Get the first element (the lowest integer key >= 1) of a numbered map.

```erlang
hd(Message) -> hd(Message, value).
```

### hd

Get the first element (the lowest integer key >= 1) of a numbered map.

```erlang
hd(Message, ReturnType) ->
    hd(Message, ReturnType, #{ error_strategy => throw }).
```

### hd

```erlang
hd(Message, ReturnType, Opts) -> 
    hd(Message, hb_ao:keys(Message, Opts), 1, ReturnType, Opts).
```

### hd

```erlang
hd(_Map, [], _Index, _ReturnType, #{ error_strategy := throw }) ->
    throw(no_integer_keys);
```

### hd

```erlang
hd(_Map, [], _Index, _ReturnType, _Opts) -> undefined;
```

### hd

```erlang
hd(Message, [Key|Rest], Index, ReturnType, Opts) ->
    case hb_ao:normalize_key(Key, Opts#{ error_strategy => return }) of
        undefined ->
            hd(Message, Rest, Index + 1, ReturnType, Opts);
        Key ->
            case ReturnType of
                key -> Key;
                value -> hb_ao:resolve(Message, Key, #{})
            end
    end.
```

### find_value

Find the value associated with a key in parsed a JSON structure list.

```erlang
find_value(Key, List) ->
    find_value(Key, List, undefined).
```

### find_value

```erlang
find_value(Key, Map, Default) ->
	find_value(Key, Map, Default, #{}).
```

### find_value

```erlang
find_value(Key, Map, Default, Opts) when is_map(Map) ->
    case hb_maps:find(Key, Map, Opts) of
        {ok, Value} -> Value;
        error -> Default
    end;
```

### find_value

```erlang
find_value(Key, List, Default, _Opts) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Val} -> Val;
        false -> Default
    end.
```

### remove_common

Remove the common prefix from two strings, returning the remainder of the

```erlang
remove_common(MainStr, SubStr) when is_binary(MainStr) and is_list(SubStr) ->
    remove_common(MainStr, list_to_binary(SubStr));
```

### remove_common

Remove the common prefix from two strings, returning the remainder of the

```erlang
remove_common(MainStr, SubStr) when is_list(MainStr) and is_binary(SubStr) ->
    binary_to_list(remove_common(list_to_binary(MainStr), SubStr));
```

### remove_common

Remove the common prefix from two strings, returning the remainder of the

```erlang
remove_common(<< X:8, Rest1/binary>>, << X:8, Rest2/binary>>) ->
    remove_common(Rest1, Rest2);
```

### remove_common

Remove the common prefix from two strings, returning the remainder of the

```erlang
remove_common([X|Rest1], [X|Rest2]) ->
    remove_common(Rest1, Rest2);
```

### remove_common

Remove the common prefix from two strings, returning the remainder of the

```erlang
remove_common([$/|Path], _) -> Path;
```

### remove_common

Remove the common prefix from two strings, returning the remainder of the
Throw an exception if the Opts map has an `error_strategy` key with the

```erlang
remove_common(Rest, _) -> Rest.
```

### maybe_throw

Remove the common prefix from two strings, returning the remainder of the
Throw an exception if the Opts map has an `error_strategy` key with the

```erlang
maybe_throw(Val, Opts) ->
    case hb_ao:get(error_strategy, Opts) of
        throw -> throw(Val);
        _ -> Val
    end.
```

### is_hb_module

Is the given module part of HyperBEAM?

```erlang
is_hb_module(Atom) ->
    is_hb_module(Atom, hb_opts:get(stack_print_prefixes, [], #{})).
```

### is_hb_module

```erlang
is_hb_module(Atom, Prefixes) when is_atom(Atom) ->
    is_hb_module(atom_to_list(Atom), Prefixes);
```

### is_hb_module

```erlang
is_hb_module("hb_event" ++ _, _) ->
    % Explicitly exclude hb_event from the stack trace, as it is always included,
    % creating noise in the output.
```

### is_hb_module

```erlang
is_hb_module(Str, Prefixes) ->
    case string:tokens(Str, "_") of
        [Pre|_] ->
            lists:member(Pre, Prefixes);
        _ ->
            false
    end.
```

### all_hb_modules

Get all loaded modules that are loaded and are part of HyperBEAM.

```erlang
all_hb_modules() ->
    lists:filter(fun(Module) -> is_hb_module(Module) end, erlang:loaded()).
```

### count

```erlang
count(Item, List) ->
    length(lists:filter(fun(X) -> X == Item end, List)).
```

### mean

```erlang
mean(List) ->
    lists:sum(List) / length(List).
```

### stddev

```erlang
stddev(List) ->
    math:sqrt(variance(List)).
```

### variance

```erlang
variance(List) ->
    Mean = mean(List),
    lists:sum([ math:pow(X - Mean, 2) || X <- List ]) / length(List).
```

### shuffle

Shuffle a list.

```erlang
shuffle(List) ->
    [ Y || {_, Y} <- lists:sort([ {rand:uniform(), X} || X <- List]) ].
```

### weighted_random

Return a random element from a list, weighted by the values in the list.

```erlang
weighted_random(List) ->
    TotalWeight = lists:sum([ Weight || {_, Weight} <- List ]),
    Normalized = [ {Item, Weight / TotalWeight} || {Item, Weight} <- List ],
    Shuffled = shuffle(Normalized),
    pick_weighted(Shuffled, rand:uniform()).
```

### pick_weighted

Pick a random element from a list, weighted by the values in the list.

```erlang
pick_weighted([], _) ->
    error(empty_list);
```

### pick_weighted

Pick a random element from a list, weighted by the values in the list.

```erlang
pick_weighted([{Item, Weight}|_Rest], Remaining) when Remaining < Weight ->
    Item;
```

### pick_weighted

Pick a random element from a list, weighted by the values in the list.

```erlang
pick_weighted([{_Item, Weight}|Rest], Remaining) ->
    pick_weighted(Rest, Remaining - Weight).
```

### addresses_to_binary

Serialize the given list of addresses to a binary, using the structured

```erlang
addresses_to_binary(List) when is_list(List) ->
    try
        iolist_to_binary(
            hb_structured_fields:list(
                [
                    {item, {string, hb_util:human_id(Addr)}, []}
                ||
                    Addr <- List
                ]
            )
        )
    catch
        _:_ ->
            error({cannot_parse_list, List})
    end.
```

### binary_to_addresses

Parse a list from a binary. First attempts to parse the binary as a

```erlang
binary_to_addresses(List) when is_list(List) ->
    % If the argument is already a list, return it.
```

### binary_to_addresses

```erlang
binary_to_addresses(List) when is_binary(List) ->
    try 
        Res = lists:map(
            fun({item, {string, Item}, []}) ->
                Item
            end,
            hb_structured_fields:parse_list(List)
        ),
        Res
    catch
        _:_ ->
        try
            binary:split(
                binary:replace(List, <<"\"">>, <<"">>, [global]),
                <<",">>,
                [global, trim_all]
            )
        catch
            _:_ ->
                error({cannot_parse_list, List})
        end
    end.
```

### split_depth_string_aware

Extract all of the parts from the binary, given (a list of) separators.

```erlang
split_depth_string_aware(_Sep, <<>>) -> [];
```

### split_depth_string_aware

Extract all of the parts from the binary, given (a list of) separators.

```erlang
split_depth_string_aware(Sep, Bin) ->
    {_MatchedSep, Part, Rest} = split_depth_string_aware_single(Sep, Bin),
    [Part | split_depth_string_aware(Sep, Rest)].
```

### split_depth_string_aware_single

Parse a binary, extracting a part until a separator is found, while

```erlang
split_depth_string_aware_single(Sep, Bin) when not is_list(Sep) ->
    split_depth_string_aware_single([Sep], Bin);
```

### split_depth_string_aware_single

Parse a binary, extracting a part until a separator is found, while

```erlang
split_depth_string_aware_single(Seps, Bin) ->
    split_depth_string_aware_single(Seps, Bin, 0, <<>>).
```

### split_depth_string_aware_single

```erlang
split_depth_string_aware_single(_Seps, <<>>, _Depth, CurrAcc) ->
    {no_match, CurrAcc, <<>>};
```

### split_depth_string_aware_single

```erlang
split_depth_string_aware_single(Seps, << $\", Rest/binary>>, Depth, CurrAcc) ->
    {QuotedStr, AfterStr} = split_escaped_single($\", Rest),
    split_depth_string_aware_single(
        Seps,
        AfterStr,
        Depth,
        << CurrAcc/binary, "\"", QuotedStr/binary, "\"">>
    );
```

### split_depth_string_aware_single

```erlang
split_depth_string_aware_single(Seps, << $\(, Rest/binary>>, Depth, CurrAcc) ->
    %% Increase depth
    split_depth_string_aware_single(Seps, Rest, Depth + 1, << CurrAcc/binary, "(" >>);
```

### split_depth_string_aware_single

```erlang
split_depth_string_aware_single(Seps, << $\), Rest/binary>>, Depth, Acc) when Depth > 0 ->
    %% Decrease depth
    split_depth_string_aware_single(Seps, Rest, Depth - 1, << Acc/binary, ")">>);
```

### split_depth_string_aware_single

```erlang
split_depth_string_aware_single(Seps, <<C:8/integer, Rest/binary>>, Depth, CurrAcc) ->
    case Depth == 0 andalso lists:member(C, Seps) of
        true -> {C, CurrAcc, Rest};
        false ->
            split_depth_string_aware_single(
                Seps,
                Rest,
                Depth,
                << CurrAcc/binary, C:8/integer >>
            )
    end.
```

### split_escaped_single

Read a binary until a separator is found without a preceding backslash.

```erlang
split_escaped_single(Sep, Bin) ->
    split_escaped_single(Sep, Bin, []).
```

### split_escaped_single

```erlang
split_escaped_single(_Sep, <<>>, Acc) ->
    {hb_util:bin(lists:reverse(Acc)), <<>>};
```

### split_escaped_single

```erlang
split_escaped_single(Sep, <<"\\", Char:8/integer, Rest/binary>>, Acc) ->
    split_escaped_single(Sep, Rest, [Char, $\\ | Acc]);
```

### split_escaped_single

```erlang
split_escaped_single(Sep, <<Sep:8/integer, Rest/binary>>, Acc) ->
    {hb_util:bin(lists:reverse(Acc)), Rest};
```

### split_escaped_single

```erlang
split_escaped_single(Sep, <<C:8/integer, Rest/binary>>, Acc) ->
    split_escaped_single(Sep, Rest, [C | Acc]).
```

### check_size

Force that a binary is either empty or the given number of bytes.

```erlang
check_size(Bin, {range, Start, End}) ->
    check_type(Bin, binary)
        andalso byte_size(Bin) >= Start
        andalso byte_size(Bin) =< End;
```

### check_size

Force that a binary is either empty or the given number of bytes.

```erlang
check_size(Bin, Sizes) ->
    check_type(Bin, binary)
        andalso lists:member(byte_size(Bin), Sizes).
```

### check_value

```erlang
check_value(Value, ExpectedValues) ->
    lists:member(Value, ExpectedValues).
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, binary) -> is_binary(Value);
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, integer) -> is_integer(Value);
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, list) -> is_list(Value);
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, map) -> is_map(Value);
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, tx) -> is_record(Value, tx);
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, message) ->
    is_record(Value, tx) or is_map(Value) or is_list(Value);
```

### check_type

Ensure that a value is of the given type.
Throw an error if the given value is not ok.

```erlang
check_type(_Value, _) -> false.
```

### ok_or_throw

Ensure that a value is of the given type.
Throw an error if the given value is not ok.

```erlang
ok_or_throw(_, true, _) -> true;
```

### ok_or_throw

Ensure that a value is of the given type.
Throw an error if the given value is not ok.

```erlang
ok_or_throw(_TX, false, Error) ->
    throw(Error).
```

### all_atoms

List the loaded atoms in the Erlang VM.

```erlang
all_atoms() -> all_atoms(0).
```

### all_atoms

List the loaded atoms in the Erlang VM.

```erlang
all_atoms(N) ->
    case atom_from_int(N) of
        not_found -> [];
        A -> [A | all_atoms(N+1)]
    end.
```

### atom_from_int

Find the atom with the given integer reference.

```erlang
atom_from_int(Int) ->
    case catch binary_to_term(<<131,75,Int:24>>) of
        A -> A;
        _ -> not_found
    end.
```

### binary_is_atom

Check if a given binary is already an atom.

```erlang
binary_is_atom(X) ->
    lists:member(X, lists:map(fun hb_util:bin/1, all_atoms())).
```

### lower_case_key_map

```erlang
lower_case_key_map(Map, Opts) ->
    hb_maps:fold(fun
        (K, V, Acc) when is_map(V) ->
            maps:put(hb_util:to_lower(K), lower_case_key_map(V, Opts), Acc);
        (K, V, Acc) ->
            maps:put(hb_util:to_lower(K), V, Acc)
```

---

*Generated from [hb_util.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_util.erl)*
