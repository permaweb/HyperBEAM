# hb_private

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_private.erl)

This module provides basic helper utilities for managing the
private element of a message, which can be used to store state that is
not included in serialized messages, or those granted to users via the
APIs. Private elements of a message can be useful for storing state that
is only relevant temporarily. For example, a device might use the private
element to store a cache of values that are expensive to recompute. They
should _not_ be used for encoding state that makes the execution of a
device non-deterministic (unless you are sure you know what you are doing).
The `set` and `get` functions of this module allow you to run those keys
as AO-Core paths if you would like to have private `devices` in the
messages non-public zone.
See `hb_ao` for more information about the AO-Core protocol
and private elements of messages.

---

## Exported Functions

- `from_message/1`
- `get/3`
- `get/4`
- `is_private/1`
- `merge/3`
- `opts/1`
- `reset/1`
- `set_priv/2`
- `set/3`
- `set/4`

---

### from_message

This module provides basic helper utilities for managing the
Return the `private` key from a message. If the key does not exist, an

```erlang
from_message(Msg) when is_map(Msg) ->
    case maps:is_key(<<"priv">>, Msg) of
        true -> maps:get(<<"priv">>, Msg, #{});
        false -> maps:get(priv, Msg, #{})
    end;
```

### from_message

This module provides basic helper utilities for managing the
Return the `private` key from a message. If the key does not exist, an
Helper for getting a value from the private element of a message. Uses

```erlang
from_message(_NonMapMessage) -> #{}.
```

### get

This module provides basic helper utilities for managing the
Return the `private` key from a message. If the key does not exist, an
Helper for getting a value from the private element of a message. Uses

```erlang
get(Key, Msg, Opts) ->
    get(Key, Msg, not_found, Opts).
```

### get

```erlang
get(InputPath, Msg, Default, Opts) ->
    % Resolve the path against the private element of the message.
```

### set

Helper function for setting a key in the private element of a message.

```erlang
set(Msg, InputPath, Value, Opts) ->
    Path = remove_private_specifier(InputPath, Opts),
    Priv = from_message(Msg),
    ?event({set_private, {in, Path}, {out, Path}, {value, Value}, {opts, Opts}}),
    NewPriv = hb_util:deep_set(Path, Value, Priv, opts(Opts)),
    ?event({set_private_res, {out, NewPriv}}),
    set_priv(Msg, NewPriv).
```

### set

```erlang
set(Msg, PrivMap, Opts) ->
    CurrentPriv = from_message(Msg),
    ?event({set_private, {in, PrivMap}, {opts, Opts}}),
    NewPriv = hb_util:deep_merge(CurrentPriv, PrivMap, opts(Opts)),
    ?event({set_private_res, {out, NewPriv}}),
    set_priv(Msg, NewPriv).
```

### merge

Merge the private elements of two messages into one. The keys in the

```erlang
merge(Msg1, Msg2, Opts) ->
    % Merge the private elements of the two messages.
```

### set_priv

Helper function for setting the complete private element of a message.

```erlang
set_priv(Msg, PrivMap)
        when map_size(PrivMap) =:= 0 andalso not is_map_key(<<"priv">>, Msg) ->
    Msg;
```

### set_priv

Helper function for setting the complete private element of a message.
Check if a key is private.

```erlang
set_priv(Msg, PrivMap) ->
    Msg#{ <<"priv">> => PrivMap }.
```

### is_private

Helper function for setting the complete private element of a message.
Check if a key is private.

```erlang
is_private(Key) ->
	try hb_util:bin(Key) of
		<<"priv", _/binary>> -> true;
		_ -> false
    catch _:_ -> false
	end.
```

### remove_private_specifier

Remove the first key from the path if it is a private specifier.

```erlang
remove_private_specifier(InputPath, Opts) ->
    case is_private(hd(Path = hb_path:term_to_path_parts(InputPath, Opts))) of
        true -> tl(Path);
        false -> Path
    end.
```

### opts

The opts map that should be used when resolving paths against the

```erlang
opts(Opts) ->
    PrivStore =
        case hb_opts:get(priv_store, undefined, Opts) of
            undefined -> [];
            PrivateStores when is_list(PrivateStores) -> PrivateStores;
            PrivateStore -> [PrivateStore]
        end,
    BaseStore =
        case hb_opts:get(store, [], Opts) of
            SingleStore when is_map(SingleStore) -> [SingleStore];
            Stores when is_list(Stores) -> Stores
        end,
    NormStore = PrivStore ++ BaseStore,
    Opts#{
        hashpath => ignore,
        cache_control => [<<"no-cache">>, <<"no-store">>],
        store => NormStore
    }.
```

### reset

Unset all of the private keys in a message or deep Erlang term.

```erlang
reset(Msg) when is_map(Msg) ->
    maps:map(
        fun(_Key, Val) -> reset(Val) end,
        maps:without(
            lists:filter(fun is_private/1, maps:keys(Msg)),
            Msg
        )
    );
```

### reset

Unset all of the private keys in a message or deep Erlang term.

```erlang
reset(List) when is_list(List) ->
    % Check if any of the terms in the list are private specifiers, return an
    % empty list if so.
```

### reset

```erlang
reset(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(reset(tuple_to_list(Tuple)));
```

### reset

```erlang
reset(NonMapMessage) ->
    NonMapMessage.
```

### set_private_test

```erlang
set_private_test() ->
    ?assertEqual(
        #{<<"a">> => 1, <<"priv">> => #{<<"b">> => 2}},
        set(#{<<"a">> => 1}, <<"b">>, 2, #{})
    ),
    Res = set(#{<<"a">> => 1}, <<"a">>, 1, #{}),
    ?assertEqual(#{<<"a">> => 1, <<"priv">> => #{<<"a">> => 1}}, Res),
    ?assertEqual(
        #{<<"a">> => 1, <<"priv">> => #{<<"a">> => 1}},
        set(Res, <<"a">>, 1, #{})
    ).
```

### get_private_key_test

```erlang
get_private_key_test() ->
    M1 = #{<<"a">> => 1, <<"priv">> => #{<<"b">> => 2}},
    ?assertEqual(not_found, get(<<"a">>, M1, #{})),
    {ok, [<<"a">>]} = hb_ao:resolve(M1, <<"keys">>, #{}),
    ?assertEqual(2, get(<<"b">>, M1, #{})),
    {error, _} = hb_ao:resolve(M1, <<"priv/a">>, #{}),
    {error, _} = hb_ao:resolve(M1, <<"priv">>, #{}).
```

### get_deep_key_test

```erlang
get_deep_key_test() ->
    M1 = #{<<"a">> => 1, <<"priv">> => #{<<"b">> => #{<<"c">> => 3}}},
    ?assertEqual(3, get(<<"b/c">>, M1, #{})).
```

### priv_opts_store_read_link_test

```erlang
priv_opts_store_read_link_test() ->
    % Write a message to the public store.
```

### priv_opts_cache_read_message_test

```erlang
priv_opts_cache_read_message_test() ->
    hb:init(),
    PublicStore = [hb_test_utils:test_store(hb_store_lmdb)],
    OnlyPrivStore = [hb_test_utils:test_store(hb_store_fs)],
    Opts = #{ store => PublicStore, priv_store => OnlyPrivStore },
    PrivOpts = opts(Opts),
    % Use the `~scheduler@1.0' and `~process@1.0' infrastructure to write a
    % complex message into the public store.
```

---

*Generated from [hb_private.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_private.erl)*
