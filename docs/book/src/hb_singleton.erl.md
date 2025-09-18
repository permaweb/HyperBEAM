# hb_singleton

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_singleton.erl)

A parser that translates AO-Core HTTP API requests in TABM format
into an ordered list of messages to evaluate. The details of this format
are described in `docs/ao-core-http-api.md`.
Syntax overview:
<pre>
    Singleton: Message containing keys and a `path` field,
               which may also contain a query string of key-value pairs.
    Path:
        - /Part1/Part2/.../PartN/ => [Part1, Part2, ..., PartN]
        - /ID/Part2/.../PartN => [ID, Part2, ..., PartN]
    Part: (Key + Resolution), Device?, #{ K => V}?
        - Part => #{ path => Part }
        - `Part&Key=Value => #{ path => Part, Key => Value }`
        - `Part=Value&... => #{ path => Part, Part => Value, ... }`
        - `Part&Key => #{ path => Part, Key => true }`
        - `Part&k1=v1&k2=v2 => #{ path => Part, k1 => `<<"v1">>`, k2 => `<<"v2">>` }'
        - `Part~Device => {as, Device, #{ path => Part }}`
        - `Part~D&K1=V1 => {as, D, #{ path => Part, K1 => `<<"v1">>` }}'
        - `pt&k1+int=1 => #{ path => pt, k1 => 1 }`
        - `pt~d&k1+int=1 => {as, d, #{ path => pt, k1 => 1 }}`
        - `(/nested/path) => Resolution of the path /nested/path`
        - `(/nested/path&k1=v1) => (resolve /nested/path)#{k1 => v1}`
        - `(/nested/path~D&K1=V1) => (resolve /nested/path)#{K1 => V1}`
        - `pt&k1+res=(/a/b/c) => #{ path => pt, k1 => (resolve /a/b/c) }`
    Key:
        - key: `<<"value">>` => #{ key => `<<"value">>`, ... } for all messages
        - n.key: `<<"value">>` => #{ key => `<<"value">>`, ... } for Nth message
        - key+int: 1 => #{ key => 1, ... }
        - key+res: /nested/path => #{ key => (resolve /nested/path), ... }
        - N.Key+res=(/a/b/c) => #{ Key => (resolve /a/b/c), ... }
</pre>

---

## Exported Functions

- `from_path/1`
- `from/2`
- `to/1`

---

### append_path

```erlang
-spec to(list(ao_message())) -> tabm_message().
to(Messages) ->
    % Iterate through all AO-Core messages folding them into the TABM message
    % Scopes contains the following map: #{Key => [StageIndex, StageIndex2...]}
    % that allows to scope keys to the given stage.
```

```erlang
append_path(PathPart, #{<<"path">> := Path} = Message) ->
    hb_maps:put(<<"path">>, <<Path/binary, "/", PathPart/binary>>, Message);
```

### append_path

```erlang
-spec to(list(ao_message())) -> tabm_message().
to(Messages) ->
    % Iterate through all AO-Core messages folding them into the TABM message
    % Scopes contains the following map: #{Key => [StageIndex, StageIndex2...]}
    % that allows to scope keys to the given stage.
```

```erlang
append_path(PathPart, Message) ->
    hb_maps:put(<<"path">>, <<"/", PathPart/binary>>, Message).
```

### type

```erlang
-spec to(list(ao_message())) -> tabm_message().
to(Messages) ->
    % Iterate through all AO-Core messages folding them into the TABM message
    % Scopes contains the following map: #{Key => [StageIndex, StageIndex2...]}
    % that allows to scope keys to the given stage.
```

```erlang
type(Value) when is_binary(Value) -> binary;
```

### type

```erlang
-spec to(list(ao_message())) -> tabm_message().
to(Messages) ->
    % Iterate through all AO-Core messages folding them into the TABM message
    % Scopes contains the following map: #{Key => [StageIndex, StageIndex2...]}
    % that allows to scope keys to the given stage.
```

```erlang
type(Value) when is_integer(Value) -> integer;
```

### type

Normalize a singleton TABM message into a list of executable AO-Core

```erlang
-spec to(list(ao_message())) -> tabm_message().
to(Messages) ->
    % Iterate through all AO-Core messages folding them into the TABM message
    % Scopes contains the following map: #{Key => [StageIndex, StageIndex2...]}
    % that allows to scope keys to the given stage.
```

```erlang
type(_Value) -> unknown.
```

### from

Normalize a singleton TABM message into a list of executable AO-Core

```erlang
-spec to(list(ao_message())) -> tabm_message().
to(Messages) ->
    % Iterate through all AO-Core messages folding them into the TABM message
    % Scopes contains the following map: #{Key => [StageIndex, StageIndex2...]}
    % that allows to scope keys to the given stage.
```

```erlang
from(RawMsg, Opts) when is_binary(RawMsg) ->
    from(#{ <<"path">> => RawMsg }, Opts);
```

### from

Normalize a singleton TABM message into a list of executable AO-Core

```erlang
-spec to(list(ao_message())) -> tabm_message().
to(Messages) ->
    % Iterate through all AO-Core messages folding them into the TABM message
    % Scopes contains the following map: #{Key => [StageIndex, StageIndex2...]}
    % that allows to scope keys to the given stage.
```

```erlang
from(RawMsg, Opts) ->
    RawPath = hb_maps:get(<<"path">>, RawMsg, <<>>),
    ?event(parsing, {raw_path, RawPath}),
    {ok, Path, Query} = from_path(RawPath),
    ?event(parsing, {parsed_path, Path, Query}),
    MsgWithoutBasePath =
        hb_maps:merge(
            hb_maps:remove(<<"path">>, RawMsg),
            Query
        ),
    % 2. Decode, split, and sanitize path segments. Each yields one step message.
```

### from_path

Parse the relative reference into path, query, and fragment.

```erlang
from_path(RelativeRef) ->
    %?event(parsing, {raw_relative_ref, RawRelativeRef}),
    %RelativeRef = hb_escape:decode(RawRelativeRef),
    Decoded = decode_string(RelativeRef),
    ?event(parsing, {parsed_relative_ref, Decoded}),
    {Path, QKVList} =
        case hb_util:split_depth_string_aware_single("?", Decoded) of
            {_Sep, P, QStr} -> {P, cowboy_req:parse_qs(#{ qs => QStr })};
            {no_match, P, <<>>} -> {P, []}
        end,
    {
        ok,
        path_parts($/, Path),
        hb_maps:from_list(QKVList)
    }.
```

### path_messages

Step 2: Decode, split and sanitize the path. Split by `/` but avoid

```erlang
path_messages(Bin, Opts) when is_binary(Bin) ->
    lists:map(fun(Part) -> parse_part(Part, Opts) end, path_parts([$/], Bin)).
```

### normalize_base

Normalize the base path.

```erlang
normalize_base([]) -> [];
```

### normalize_base

Normalize the base path.

```erlang
normalize_base([First|Rest]) when ?IS_ID(First) -> [First|Rest];
```

### normalize_base

Normalize the base path.

```erlang
normalize_base([{as, DevID, First}|Rest]) -> [{as, DevID, First}|Rest];
```

### normalize_base

Normalize the base path.

```erlang
normalize_base([Subres = {resolve, _}|Rest]) -> [Subres|Rest];
```

### normalize_base

Normalize the base path.
Split the path into segments, filtering out empty segments and

```erlang
normalize_base(Rest) -> [#{}|Rest].
```

### path_parts

Normalize the base path.
Split the path into segments, filtering out empty segments and

```erlang
path_parts(Sep, PathBin) when is_binary(PathBin) ->
    Res = lists:filtermap(
        fun(Part) ->
            case byte_size(Part) of
                0 -> false;
                TooLong when TooLong > ?MAX_SEGMENT_LENGTH ->
                    throw({error, segment_too_long, Part});
                _ -> {true, Part}
            end
        end,
        all_path_parts(Sep, PathBin)
    ),
    ?event({path_parts, Res}),
    Res.
```

### all_path_parts

Extract all of the parts from the binary, given (a list of) separators.

```erlang
all_path_parts(_Sep, <<>>) -> [];
```

### all_path_parts

Extract all of the parts from the binary, given (a list of) separators.

```erlang
all_path_parts(Sep, Bin) ->
    hb_util:split_depth_string_aware(Sep, Bin).
```

### part

Extract the characters from the binary until a separator is found.

```erlang
part(Sep, Bin) when not is_list(Sep) ->
    part([Sep], Bin);
```

### part

Extract the characters from the binary until a separator is found.

```erlang
part(Seps, Bin) ->
    hb_util:split_depth_string_aware_single(Seps, Bin).
```

### apply_types

Step 3: Apply types to values and remove specifiers.

```erlang
apply_types(Msg, Opts) ->
    hb_maps:fold(
        fun(Key, Val, Acc) ->
            {_, K, V} = maybe_typed(Key, Val, Opts),
            hb_maps:put(K, V, Acc, Opts)
        end,
        #{},
        Msg,
        Opts
    ).
```

### group_scoped

Step 4: Group headers/query by N-scope.

```erlang
group_scoped(Map, Msgs) ->
    {NScope, Global} =
        hb_maps:fold(
            fun(KeyBin, Val, {Ns, Gs}) ->
                case parse_scope(KeyBin) of
                    {OkN, RealKey} when OkN > 0 ->
                        Curr = hb_maps:get(OkN, Ns, #{}),
                        Ns2 = hb_maps:put(OkN, hb_maps:put(RealKey, Val, Curr), Ns),
                        {Ns2, Gs};
                    global -> {Ns, hb_maps:put(KeyBin, Val, Gs)}
                end
          end,
          {#{}, #{}},
          Map
        ),
    [
        hb_maps:merge(Global, hb_maps:get(N, NScope, #{}))
    ||
        N <- lists:seq(1, length(Msgs))
    ].
```

### parse_scope

Get the scope of a key. Adds 1 to account for the base message.

```erlang
parse_scope(KeyBin) ->
    case binary:split(KeyBin, <<".">>, [global]) of
        [Front, Remainder] ->
            case catch erlang:binary_to_integer(Front) of
                NInt when is_integer(NInt) -> {NInt + 1, Remainder};
                _ -> throw({error, invalid_scope, KeyBin})
            end;
        _ -> global
    end.
```

### build_messages

Step 5: Merge the base message with the scoped messages.

```erlang
build_messages(Msgs, ScopedModifications, Opts) ->
    do_build(1, Msgs, ScopedModifications, Opts).
```

### do_build

```erlang
do_build(_, [], _, _) -> [];
```

### do_build

```erlang
do_build(I, [{as, DevID, RawMsg} | Rest], ScopedKeys, Opts) when is_map(RawMsg) ->
    % We are processing an `as' message. If the path is empty, we need to
    % remove it from the message and the additional message, such that AO-Core
    % returns only the message with the device specifier changed. If the message
    % does have a path, AO-Core will subresolve it.
```

### do_build

```erlang
do_build(I, [Msg | Rest], ScopedKeys, Opts) when not is_map(Msg) ->
    [Msg | do_build(I + 1, Rest, ScopedKeys, Opts)];
```

### do_build

```erlang
do_build(I, [Msg | Rest], ScopedKeys, Opts) ->
    Additional = lists:nth(I, ScopedKeys),
    Merged = hb_maps:merge(Additional, Msg, Opts),
    StepMsg = hb_message:convert(
        Merged, 
        <<"structured@1.0">>, 
        Opts#{ topic => ao_internal }
    ),
    ?event(parsing, {build_messages, {base, Msg}, {additional, Additional}}),
    [StepMsg | do_build(I + 1, Rest, ScopedKeys, Opts)].
```

### parse_part

Parse a path part into a message or an ID.

```erlang
parse_part(ID, _Opts) when ?IS_ID(ID) -> ID;
```

### parse_part

Parse a path part into a message or an ID.

```erlang
parse_part(Part, Opts) ->
    case maybe_subpath(Part, Opts) of
        {resolve, Subpath} -> {resolve, Subpath};
        Part ->
            case part([$&, $~, $+, $ , $=], Part) of
                {no_match, PartKey, <<>>} ->
                    #{ <<"path">> => PartKey };
                {Sep, PartKey, PartModBin} ->
                    parse_part_mods(
                        << Sep:8/integer, PartModBin/binary >>,
                        #{ <<"path">> => PartKey },
						Opts
                    )
            end
    end.
```

### parse_part_mods

Parse part modifiers:

```erlang
parse_part_mods([], Msg, _Opts) -> Msg;
```

### parse_part_mods

Parse part modifiers:

```erlang
parse_part_mods(<<>>, Msg, _Opts) -> Msg;
```

### parse_part_mods

Parse part modifiers:

```erlang
parse_part_mods(<<"~", PartMods/binary>>, Msg, Opts) ->
    % Get the string until the end of the device specifier or end of string.
```

### parse_part_mods

```erlang
parse_part_mods(<< "&", InlinedMsgBin/binary >>, Msg, Opts) ->
    InlinedKeys = path_parts($&, InlinedMsgBin),
    MsgWithInlined =
        lists:foldl(
            fun(InlinedKey, Acc) ->
                {Key, Val} = parse_inlined_key_val(InlinedKey, Opts),
                ?event({inlined_key, {explicit, Key}, {explicit, Val}}),
                hb_maps:put(Key, Val, Acc)
            end,
            Msg,
            InlinedKeys
        ),
    MsgWithInlined;
```

### parse_part_mods

```erlang
parse_part_mods(<<$=, InlinedMsgBin/binary>>, M = #{ <<"path">> := Path }, Opts)
        when map_size(M) =:= 1, is_binary(Path) ->
    parse_part_mods(<< "&", Path/binary, "=", InlinedMsgBin/binary >>, M, Opts);
```

### parse_part_mods

Extrapolate the inlined key-value pair from a path segment. If the

```erlang
parse_part_mods(<<$+, InlinedMsgBin/binary>>, M = #{ <<"path">> := Path }, Opts)
        when map_size(M) =:= 1, is_binary(InlinedMsgBin) ->
    parse_part_mods(<< "&", Path/binary, "+", InlinedMsgBin/binary >>, M, Opts).
```

### parse_inlined_key_val

Extrapolate the inlined key-value pair from a path segment. If the

```erlang
parse_inlined_key_val(Bin, Opts) ->
    case part([$=, $&], Bin) of
        {no_match, K, <<>>} -> {K, true};
        {$=, K, RawV} ->
            V = unquote(RawV),
            {_, Key, Val} = maybe_typed(K, maybe_subpath(V, Opts), Opts),
            {Key, Val}
    end.
```

### unquote

Unquote a string.

```erlang
unquote(<<"\"", Inner/binary>>) ->
    case binary:last(Inner) of
        $" -> binary:part(Inner, 0, byte_size(Inner) - 1);
        _ -> Inner
    end;
```

### unquote

Unquote a string.
Attempt Cowboy URL decode, then sanitize the result.

```erlang
unquote(Bin) -> Bin.
```

### decode_string

Unquote a string.
Attempt Cowboy URL decode, then sanitize the result.

```erlang
decode_string(B) ->
    case catch uri_string:unquote(B) of
        DecodedBin when is_binary(DecodedBin) -> DecodedBin;
        _ -> throw({error, cannot_decode, B})
    end.
```

### maybe_subpath

Check if the string is a subpath, returning it in parsed form,

```erlang
maybe_subpath(Str, Opts) when byte_size(Str) >= 2 ->
    case {binary:first(Str), binary:last(Str)} of
        {$(, $)} ->
            Inside = binary:part(Str, 1, byte_size(Str) - 2),
            {resolve, from(#{ <<"path">> => Inside }, Opts)};
        _ -> Str
    end;
```

### maybe_subpath

Check if the string is a subpath, returning it in parsed form,
Parse a key's type (applying it to the value) and device name if present.

```erlang
maybe_subpath(Other, _Opts) -> Other.
```

### maybe_typed

Check if the string is a subpath, returning it in parsed form,
Parse a key's type (applying it to the value) and device name if present.

```erlang
maybe_typed(Key, Value, Opts) ->
    case part([$+, $ ], Key) of
        {no_match, OnlyKey, <<>>} -> {untyped, OnlyKey, Value};
        {_, OnlyKey, Type} ->
            case {Type, Value} of
                {<<"resolve">>, Subpath} ->
                    % If the value needs to be resolved before it is converted,
                    % use the `Codec/1.0' device to resolve it.
```

### maybe_join

Join a list of items with a separator, or return the first item if there

```erlang
maybe_join(Items, Sep) ->
    case length(Items) of
        0 -> <<>>;
        1 -> hd(Items);
        _ -> iolist_to_binary(lists:join(Sep, Items))
    end.
```

### parse_explicit_message_test

```erlang
parse_explicit_message_test() ->
    Singleton1 = #{
        <<"path">> => <<"/a">>,
        <<"a">> => <<"b">>
    },
    ?assertEqual(
        [
            #{ <<"a">> => <<"b">>},
            #{ <<"path">> => <<"a">>, <<"a">> => <<"b">> }
        ],
        from(Singleton1, #{})
    ),
    DummyID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Singleton2 = #{
        <<"path">> => <<"/", DummyID/binary, "/a">>
    },
    ?assertEqual([DummyID, #{ <<"path">> => <<"a">> }], from(Singleton2, #{})),
    Singleton3 = #{
        <<"path">> => <<"/", DummyID/binary, "/a">>,
        <<"a">> => <<"b">>
    },
    ?assertEqual(
        [DummyID, #{ <<"path">> => <<"a">>, <<"a">> => <<"b">> }],
        from(Singleton3, #{})
    ).
```

### to_suite_test_

```erlang
to_suite_test_() ->
    [
        fun simple_to_test/0,
        fun multiple_messages_to_test/0,
        fun basic_hashpath_to_test/0,
        fun scoped_key_to_test/0,
        fun typed_key_to_test/0,
        fun subpath_in_key_to_test/0,
        fun subpath_in_path_to_test/0,
        fun inlined_keys_to_test/0,
        fun multiple_inlined_keys_to_test/0,
        fun subpath_in_inlined_to_test/0
    ].
```

### simple_to_test

```erlang
simple_to_test() ->
    Messages = [
        #{<<"test-key">> => <<"test-value">>},
        #{<<"path">> => <<"a">>, <<"test-key">> => <<"test-value">>}
    ],
    Expected = #{<<"path">> => <<"/a">>, <<"test-key">> => <<"test-value">>},
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).
```

### multiple_messages_to_test

```erlang
multiple_messages_to_test() ->
    Messages =
        [
            #{<<"test-key">> => <<"test-value">>},
            #{<<"path">> => <<"a">>, <<"test-key">> => <<"test-value">>},
            #{<<"path">> => <<"b">>, <<"test-key">> => <<"test-value">>},
            #{<<"path">> => <<"c">>, <<"test-key">> => <<"test-value">>}
        ],
    Expected = #{
        <<"path">> => <<"/a/b/c">>,
        <<"test-key">> => <<"test-value">>
    },
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).
```

### basic_hashpath_to_test

```erlang
basic_hashpath_to_test() ->
    Messages = [
        <<"e5ohB7TgMYRoc0BLllkmAqkqLy1SrliEkOPJlNPXBQ8">>,
        #{<<"method">> => <<"GET">>, <<"path">> => <<"some-other">>}
    ],
    Expected = #{
        <<"path">> => <<"/e5ohB7TgMYRoc0BLllkmAqkqLy1SrliEkOPJlNPXBQ8/some-other">>,
        <<"method">> => <<"GET">>
    },
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).
```

### scoped_key_to_test

```erlang
scoped_key_to_test() ->
    Messages = [
        #{},
        #{<<"path">> => <<"a">>},
        #{<<"path">> => <<"b">>, <<"test-key">> => <<"test-value">>},
        #{<<"path">> => <<"c">>}
    ],
    Expected = #{<<"2.test-key">> => <<"test-value">>, <<"path">> => <<"/a/b/c">>},
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).
```

### typed_key_to_test

```erlang
typed_key_to_test() ->
    Messages =
        [
            #{},
            #{<<"path">> => <<"a">>},
            #{<<"path">> => <<"b">>, <<"test-key">> => 123},
            #{<<"path">> => <<"c">>}
        ],
    Expected = #{<<"2.test-key+integer">> => <<"123">>, <<"path">> => <<"/a/b/c">>},
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).
```

### subpath_in_key_to_test

```erlang
subpath_in_key_to_test() ->
    Messages = [
        #{},
        #{<<"path">> => <<"a">>},
        #{
            <<"path">> => <<"b">>,
            <<"test-key">> =>
                {resolve,
                    [
                        #{},
                        #{<<"path">> => <<"x">>},
                        #{<<"path">> => <<"y">>},
                        #{<<"path">> => <<"z">>}
                    ]
                }
        },
        #{<<"path">> => <<"c">>}
    ],
    Expected = #{<<"2.test-key+resolve">> => <<"/x/y/z">>, <<"path">> => <<"/a/b/c">>},
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).
```

### subpath_in_path_to_test

```erlang
subpath_in_path_to_test() ->
    Messages = [
        #{},
        #{<<"path">> => <<"a">>},
        {resolve,
            [
                #{},
                #{<<"path">> => <<"x">>},
                #{<<"path">> => <<"y">>},
                #{<<"path">> => <<"z">>}
            ]
        },
        #{<<"path">> => <<"z">>}
    ],
    Expected = #{
        <<"path">> => <<"/a/(x/y/z)/z">>
    },
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages), #{})).
```

### inlined_keys_to_test

```erlang
inlined_keys_to_test() ->
    Messages =
        [
            #{<<"method">> => <<"POST">>},
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"a">>
            },
            #{
                <<"k1">> => <<"v1">>,
                <<"method">> => <<"POST">>,
                <<"path">> => <<"b">>
            },
            #{
                <<"k2">> => <<"v2">>,
                <<"method">> => <<"POST">>,
                <<"path">> => <<"c">>
            }
        ],
    % NOTE: The implementation above does not convert the given list of messages
    % into the original format, however it assures that the `to/1' and `from/1'
    % operations are idempotent.
```

### multiple_inlined_keys_to_test

```erlang
multiple_inlined_keys_to_test() ->
    Messages = [
        #{<<"method">> => <<"POST">>},
            #{<<"method">> => <<"POST">>, <<"path">> => <<"a">>},
            #{
                <<"k1">> => <<"v1">>,
                <<"k2">> => <<"v2">>,
                <<"method">> => <<"POST">>,
                <<"path">> => <<"b">>
            }
        ],
    % NOTE: The implementation above does not convert the given list of messages
    % into the original format, however it assures that the `to/1' and `from/1'
    % operations are idempotent.
```

### subpath_in_inlined_to_test

```erlang
subpath_in_inlined_to_test() ->
    Messages = [
        #{},
        #{<<"path">> => <<"part1">>},
        #{<<"b">> =>
                {resolve,
                    [#{},
                     #{<<"path">> => <<"x">>},
                     #{<<"path">> => <<"y">>}]},
                    <<"path">> => <<"part2">>,
                    <<"test">> => <<"1">>},
        #{<<"path">> => <<"part3">>}],
    % NOTE: The implementation above does not convert the given list of messages
    % into the original format, however it assures that the `to/1' and `from/1'
    % operations are idempotent.
```

### single_message_test

```erlang
single_message_test() ->
    % This is a singleton TABM message
    Req = #{
        <<"path">> => <<"/a">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(2, length(Msgs)),
    ?assert(is_map(hd(Msgs))),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, hd(Msgs))).
```

### basic_hashpath_test

```erlang
basic_hashpath_test() ->
    Hashpath = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Path = <<"/", Hashpath/binary, "/some-other">>,
    Req = #{
        <<"path">> => Path,
        <<"method">> => <<"GET">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(2, length(Msgs)),
    [Base, Msg2] = Msgs,
    ?assertEqual(Base, Hashpath),
    ?assertEqual(<<"GET">>, hb_maps:get(<<"method">>, Msg2)),
    ?assertEqual(<<"some-other">>, hb_maps:get(<<"path">>, Msg2)).
```

### multiple_messages_test

```erlang
multiple_messages_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_Base, Msg1, Msg2, Msg3] = Msgs,
    ?assert(lists:all(fun is_map/1, Msgs)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Msg1)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Msg2)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Msg3)).
%%% Advanced key syntax tests
```

### scoped_key_test

```erlang
scoped_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key">> => <<"test-value">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Msg3, not_found)).
```

### typed_key_test

```erlang
typed_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key+integer">> => <<"123">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(123, hb_maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Msg3, not_found)).
```

### subpath_in_key_test

```erlang
subpath_in_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key+resolve">> => <<"/x/y/z">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(
        {resolve,
            [
                #{},
                #{ <<"path">> => <<"x">> },
                #{ <<"path">> => <<"y">> },
                #{ <<"path">> => <<"z">> }
            ]
        },
        hb_maps:get(<<"test-key">>, Msg2, not_found)
    ),
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Msg3, not_found)).
%%% Advanced path syntax tests
```

### subpath_in_path_test

```erlang
subpath_in_path_test() ->
    Req = #{
        <<"path">> => <<"/a/(x/y/z)/z">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(<<"a">>, hb_maps:get(<<"path">>, Msg1)),
    ?assertEqual(
        {resolve,
            [
                #{},
                #{ <<"path">> => <<"x">> },
                #{ <<"path">> => <<"y">> },
                #{ <<"path">> => <<"z">> }
            ]
        },
        Msg2
    ),
    ?assertEqual(<<"z">>, hb_maps:get(<<"path">>, Msg3)).
```

### inlined_keys_test

```erlang
inlined_keys_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/a/b&k1=v1/c&k2=v2">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(<<"v1">>, hb_maps:get(<<"k1">>, Msg2)),
    ?assertEqual(<<"v2">>, hb_maps:get(<<"k2">>, Msg3)),
    ?assertEqual(not_found, hb_maps:get(<<"k1">>, Msg1, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"k2">>, Msg2, not_found)).
```

### inlined_quoted_key_test

```erlang
inlined_quoted_key_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/a/b&k1=\"v/1\"/c&k2=v2">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(<<"v/1">>, hb_maps:get(<<"k1">>, Msg2)),
    ?assertEqual(<<"v2">>, hb_maps:get(<<"k2">>, Msg3)),
    ?assertEqual(not_found, hb_maps:get(<<"k1">>, Msg1, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"k2">>, Msg2, not_found)),
    ReqB = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/~profile@1.0/eval=%22~meta@1.0/info%22">>
    },
    MsgsB = from(ReqB, #{}),
    [_, Msg2b] = MsgsB,
    ?assertEqual(<<"~meta@1.0/info">>, hb_maps:get(<<"eval">>, Msg2b)).
```

### inlined_assumed_key_test

```erlang
inlined_assumed_key_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/a/b=4/c&k2=v2">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?event({parsed, Msgs}),
    ?assertEqual(<<"4">>, hb_maps:get(<<"b">>, Msg2)),
    ?assertEqual(not_found, hb_maps:get(<<"b">>, Msg1, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"b">>, Msg3, not_found)),
    ReqB = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/a/b+integer=4/c&k2=v2">>
    },
    MsgsB = from(ReqB, #{}),
    [_, Msg1b, Msg2b, Msg3b] = MsgsB,
    ?event({parsed, MsgsB}),
    ?assertEqual(4, hb_maps:get(<<"b">>, Msg2b)),
    ?assertEqual(not_found, hb_maps:get(<<"b">>, Msg1b, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"b">>, Msg3b, not_found)).
```

### multiple_inlined_keys_test

```erlang
multiple_inlined_keys_test() ->
    Path = <<"/a/b&k1=v1&k2=v2">>,
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => Path
    },
    Msgs = from(Req, #{}),
    ?assertEqual(3, length(Msgs)),
    [_, Msg1, Msg2] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"k1">>, Msg1, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"k2">>, Msg1, not_found)),
    ?assertEqual(<<"v1">>, hb_maps:get(<<"k1">>, Msg2, not_found)),
    ?assertEqual(<<"v2">>, hb_maps:get(<<"k2">>, Msg2, not_found)).
```

### subpath_in_inlined_test

```erlang
subpath_in_inlined_test() ->
    Path = <<"/part1/part2&test=1&b=(/x/y)/part3">>,
    Req = #{
        <<"path">> => Path
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, First, Second, Third] = Msgs,
    ?assertEqual(<<"part1">>, hb_maps:get(<<"path">>, First)),
    ?assertEqual(<<"part3">>, hb_maps:get(<<"path">>, Third)),
    ?assertEqual(
        {resolve, [#{}, #{ <<"path">> => <<"x">> }, #{ <<"path">> => <<"y">> }] },
        hb_maps:get(<<"b">>, Second)
    ).
```

### path_parts_test

```erlang
path_parts_test() ->
    ?assertEqual(
        [<<"a">>, <<"b&c=(/d/e)">>, <<"f">>],
        path_parts($/, <<"/a/b&c=(/d/e)/f">>)
    ),
    ?assertEqual([<<"a">>], path_parts($/, <<"/a">>)),
    ?assertEqual([<<"a">>, <<"b">>, <<"c">>], path_parts($/, <<"/a/b/c">>)),
    ?assertEqual(
        [
            <<"IYkkrqlZNW_J-4T-5eFApZOMRl5P4VjvrcOXWvIqB1Q">>,
            <<"msg2">>
        ],
        path_parts($/, <<"/IYkkrqlZNW_J-4T-5eFApZOMRl5P4VjvrcOXWvIqB1Q/msg2">>)
    ),
    ?assertEqual(
        [<<"a">>, <<"b&K1=V1">>, <<"c&K2=V2">>],
        path_parts($/, <<"/a/b&K1=V1/c&K2=V2">>)
    ),
    ?assertEqual(
        [<<"a">>, <<"(x/y/z)">>, <<"c">>],
        path_parts($/, <<"/a/(x/y/z)/c">>)
    ),
```

---

*Generated from [hb_singleton.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_singleton.erl)*
