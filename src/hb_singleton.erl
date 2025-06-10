%%% @doc A parser that translates AO-Core HTTP API requests in TABM format
%%% into an ordered list of messages to evaluate. The details of this format
%%% are described in `docs/ao-core-http-api.md'.
%%%
%%% Syntax overview:
%%% <pre>
%%%     Singleton: Message containing keys and a `path' field,
%%%                which may also contain a query string of key-value pairs.
%%%
%%%     Path:
%%%         - /Part1/Part2/.../PartN/ => [Part1, Part2, ..., PartN]
%%%         - /ID/Part2/.../PartN => [ID, Part2, ..., PartN]
%%%
%%%     Part: (Key + Resolution), Device?, #{ K => V}?
%%%         - Part => #{ path => Part }
%%%         - `Part&Key=Value => #{ path => Part, Key => Value }'
%%%         - `Part&Key => #{ path => Part, Key => true }'
%%%         - `Part&k1=v1&k2=v2 => #{ path => Part, k1 => `<<"v1">>', k2 => `<<"v2">>' }'
%%%         - `Part~Device => {as, Device, #{ path => Part }}'
%%%         - `Part~D&K1=V1 => {as, D, #{ path => Part, K1 => `<<"v1">>' }}'
%%%         - `pt&k1+int=1 => #{ path => pt, k1 => 1 }'
%%%         - `pt~d&k1+int=1 => {as, d, #{ path => pt, k1 => 1 }}'
%%%         - `(/nested/path) => Resolution of the path /nested/path'
%%%         - `(/nested/path&k1=v1) => (resolve /nested/path)#{k1 => v1}'
%%%         - `(/nested/path~D&K1=V1) => (resolve /nested/path)#{K1 => V1}'
%%%         - `pt&k1+res=(/a/b/c) => #{ path => pt, k1 => (resolve /a/b/c) }'
%%%     Key:
%%%         - key: `<<"value">>' => #{ key => `<<"value">>', ... } for all messages
%%%         - n.key: `<<"value">>' => #{ key => `<<"value">>', ... } for Nth message
%%%         - key+Int: 1 => #{ key => 1, ... }
%%%         - key+Res: /nested/path => #{ key => (resolve /nested/path), ... }
%%%         - N.Key+Res=(/a/b/c) => #{ Key => (resolve /a/b/c), ... }
%%% </pre>
-module(hb_singleton).
-export([from/1, to/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(MAX_SEGMENT_LENGTH, 512).

-type ao_message() :: map() | binary().
-type tabm_message() :: map().

%% @doc Convert a list of AO-Core message into TABM message.
-spec to(list(ao_message())) -> tabm_message().
to(Messages) ->
    % Iterate through all AO-Core messages folding them into the TABM message
    % Scopes contains the following map: #{Key => [StageIndex, StageIndex2...]}
    % that allows to scope keys to the given stage.
    {TABMMessage, _FinalIndex, Scopes} =
        lists:foldl(
            fun
                % Special case when AO-Core message is ID
                (Message, {Acc, Index, ScopedModifications}) when ?IS_ID(Message) ->
                    {append_path(Message, Acc), Index + 1, ScopedModifications};

                % Special case when AO-Core message contains resolve command
                ({resolve, SubMessages0}, {Acc, Index, ScopedModifications}) ->
                    SubMessages1 = maps:get(<<"path">>, to(SubMessages0)),
                    <<"/", SubMessages2/binary>> = SubMessages1,
                    SubMessages = <<"(", SubMessages2/binary, ")">>,
                    {append_path(SubMessages, Acc), Index + 1, ScopedModifications};

                % Regular case when message is a map
                (Message, {Acc, Index, ScopedModifications}) ->
                    {NewMessage, NewScopedModifications} =
                        maps:fold(
                            fun
                                (<<"path">>, PathPart, {AccIn, Scoped}) ->
                                    {append_path(PathPart, AccIn), Scoped};
                                % Specifically ignore method field from scope modifications
                                (<<"method">>, Value, {AccIn, Scoped}) ->
                                    {maps:put(<<"method">>, Value, AccIn), Scoped};
                                (Key, {resolve, SubMessages}, {AccIn, Scoped}) ->
                                    NewKey = <<Key/binary, "+resolve">>,
                                    NewSubMessages = maps:get(<<"path">>, to(SubMessages)),
                                    {
                                        maps:put(NewKey, NewSubMessages, AccIn),
                                        maps:update_with(
                                            NewKey,
                                            fun(Indexes) -> [Index | Indexes] end,
                                            [Index],
                                            Scoped
                                        )
                                    };
                                (Key, Value, {AccIn, Scoped}) ->
                                    {
                                        maps:put(Key, Value, AccIn),
                                        maps:update_with(
                                            Key,
                                            fun(Indexes) -> [Index | Indexes] end,
                                            [Index],
                                            Scoped
                                        )
                                    }
                            end,
                            {Acc, ScopedModifications},
                            Message),

                        {NewMessage, Index + 1, NewScopedModifications}

            end,
            {#{}, 0, #{}},
            Messages),
    MessageWithTypeAndScopes =
        maps:fold(
            fun
                % For the case when a given Key appeared only once in scopes
                (Key, [SingleIndexScope], AccIn) ->
                    Index = integer_to_binary(SingleIndexScope),
                    {Value, NewAccIn} = maps:take(Key, AccIn),
                    {NewKey, NewValue} =
                        case type(Value) of
                            integer ->
                                K = <<Index/binary, ".", Key/binary, "+integer">>,
                                V = integer_to_binary(Value),
                                {K, V};
                            _ -> {<<Index/binary, ".", Key/binary>>, Value}
                        end,
                    maps:put(NewKey, NewValue, NewAccIn);
                (_Key, _Value, AccIn) -> AccIn
            end,
            TABMMessage,
            Scopes),
    MessageWithTypeAndScopes.

append_path(PathPart, #{<<"path">> := Path} = Message) ->
    maps:put(<<"path">>, <<Path/binary, "/", PathPart/binary>>, Message);
append_path(PathPart, Message) ->
    maps:put(<<"path">>, <<"/", PathPart/binary>>, Message).

type(Value) when is_binary(Value) -> binary;
type(Value) when is_integer(Value) -> integer;
type(_Value) -> unknown.

%% @doc Normalize a singleton TABM message into a list of executable AO-Core
%% messages.
from(Path) when is_binary(Path) ->
    from(#{ <<"path">> => Path });
from(RawMsg) ->
    RawPath = maps:get(<<"path">>, RawMsg, <<>>),
    ?event(parsing, {raw_path, RawPath}),
    {ok, Path, Query} = parse_full_path(RawPath),
    ?event(parsing, {parsed_path, Path, Query}),
    MsgWithoutBasePath = maps:merge(
        maps:remove(<<"path">>, RawMsg),
        Query
    ),
    % 2. Decode, split, and sanitize path segments. Each yields one step message.
    RawMsgs = lists:flatten(lists:map(fun path_messages/1, Path)),
    ?event(parsing, {raw_messages, RawMsgs}),
    Msgs = normalize_base(RawMsgs),
    ?event(parsing, {normalized_messages, Msgs}),
    % 3. Type keys and values
    Typed = apply_types(MsgWithoutBasePath),
    ?event(parsing, {typed_messages, Typed}),
    % 4. Group keys by N-scope and global scope
    ScopedModifications = group_scoped(Typed, Msgs),
    ?event(parsing, {scoped_modifications, ScopedModifications}),
    % 5. Generate the list of messages (plus-notation, device, typed keys).
    Result = build_messages(Msgs, ScopedModifications),
    ?event(parsing, {result, Result}),
    Result.

%% @doc Parse the relative reference into path, query, and fragment.
parse_full_path(RelativeRef) ->
    {Path, QueryMap} =
        case part([$?], RelativeRef) of
            {$?, Base, Query} ->
                {Base, parse_inlined_keys(Query, #{})};
            {no_match, Base, <<>>} -> {Base, #{}}
        end,
    {
        ok,
        lists:map(fun(Part) -> decode_string(Part) end, path_parts($/, Path)),
        QueryMap
    }.

%% @doc Step 2: Decode, split and sanitize the path. Split by `/' but avoid
%% subpath components, such that their own path parts are not dissociated from
%% their parent path.
path_messages(RawBin) when is_binary(RawBin) ->
    lists:map(fun parse_part/1, path_parts([$/], decode_string(RawBin))).

%% @doc Normalize the base path.
normalize_base([]) -> [];
normalize_base([First|Rest]) when ?IS_ID(First) -> [First|Rest];
normalize_base([{as, DevID, First}|Rest]) -> [{as, DevID, First}|Rest];
normalize_base([Subres = {resolve, _}|Rest]) -> [Subres|Rest];
normalize_base(Rest) -> [#{}|Rest].

%% @doc Split the path into segments, filtering out empty segments and
%% segments that are too long.
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
    Res.

%% @doc Extract all of the parts from the binary, given (a list of) separators.
all_path_parts(_Sep, <<>>) -> [];
all_path_parts(Sep, Bin) ->
    {_MatchedSep, Part, Rest} = part(Sep, Bin),
    [Part | all_path_parts(Sep, Rest)].

%% @doc Extract the characters from the binary until a separator is found.
%% The first argument of the function is an explicit separator character, or
%% a list of separator characters. Returns a tuple with the separator, the
%% accumulated characters, and the rest of the binary.
part(Sep, Bin) when not is_list(Sep) ->
    part([Sep], Bin);
part(Seps, Bin) ->
    part(Seps, Bin, 0, <<>>).
part(_Seps, <<>>, _Depth, CurrAcc) -> {no_match, CurrAcc, <<>>};
part(Seps, << $\(, Rest/binary>>, Depth, CurrAcc) ->
    %% Increase depth
    part(Seps, Rest, Depth + 1, << CurrAcc/binary, "(" >>);
part(Seps, << $\), Rest/binary>>, Depth, CurrAcc) when Depth > 0 ->
    %% Decrease depth
    part(Seps, Rest, Depth - 1, << CurrAcc/binary, ")">>);
part(Seps, <<C:8/integer, Rest/binary>>, Depth, CurrAcc) ->
    case Depth == 0 andalso lists:member(C, Seps) of
        true -> {C, CurrAcc, Rest};
        false ->
            part(Seps, Rest, Depth, << CurrAcc/binary, C:8/integer >>)
    end.

%% @doc Step 3: Apply types to values and remove specifiers.
apply_types(Msg) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            {_, K, V} = maybe_typed(Key, Val),
            maps:put(K, V, Acc)
        end,
        #{},
        Msg
    ).

%% @doc Step 4: Group headers/query by N-scope.
%% `N.Key' => applies to Nth step. Otherwise => `global'
group_scoped(Map, Msgs) ->
    {NScope, Global} =
        maps:fold(
            fun(KeyBin, Val, {Ns, Gs}) ->
                case parse_scope(KeyBin) of
                    {OkN, RealKey} when OkN > 0 ->
                        Curr = maps:get(OkN, Ns, #{}),
                        Ns2 = maps:put(OkN, maps:put(RealKey, Val, Curr), Ns),
                        {Ns2, Gs};
                    global -> {Ns, maps:put(KeyBin, Val, Gs)}
                end
          end,
          {#{}, #{}},
          Map
        ),
    [
        maps:merge(Global, maps:get(N, NScope, #{}))
    ||
        N <- lists:seq(1, length(Msgs))
    ].

%% @doc Get the scope of a key. Adds 1 to account for the base message.
parse_scope(KeyBin) ->
    case binary:split(KeyBin, <<".">>, [global]) of
        [Front, Remainder] ->
            case catch erlang:binary_to_integer(Front) of
                NInt when is_integer(NInt) -> {NInt + 1, Remainder};
                _ -> throw({error, invalid_scope, KeyBin})
            end;
        _ -> global
    end.

%% @doc Step 5: Merge the base message with the scoped messages.
build_messages(Msgs, ScopedModifications) ->
    build(1, Msgs, ScopedModifications).
build(_, [], _ScopedKeys) -> [];
build(I, [{as, DevID, Msg = #{ <<"path">> := <<"">> }}|Rest], ScopedKeys) ->
    ScopedKey = lists:nth(I, ScopedKeys),
    StepMsg =
        hb_message:convert(
            Merged = maps:merge(Msg, ScopedKey),
            <<"structured@1.0">>,
            #{ topic => ao_internal }
        ),
    ?event({merged, {dev, DevID}, {input, Msg}, {merged, Merged}, {output, StepMsg}}),
    [{as, DevID, StepMsg} | build(I + 1, Rest, ScopedKeys)];
build(I, [Msg|Rest], ScopedKeys) when not is_map(Msg) ->
    [Msg | build(I + 1, Rest, ScopedKeys)];
build(I, [Msg | Rest], ScopedKeys) ->
    StepMsg =
        hb_message:convert(
            maps:merge(Msg, lists:nth(I, ScopedKeys)),
            <<"structured@1.0">>,
            #{ topic => ao_internal }
        ),
    [StepMsg | build(I + 1, Rest, ScopedKeys)].

%% @doc Parse a path part into a message or an ID.
%% Applies the syntax rules outlined in the module doc, in the following order:
%% 1. ID
%% 2. Part subpath resolutions
%% 3. Inlined key-value pairs
%% 4. Device specifier
parse_part(ID) when ?IS_ID(ID) -> ID;
parse_part(Part) ->
    case maybe_subpath(Part) of
        {resolve, Subpath} -> {resolve, Subpath};
        Part ->
            case part([$&, $~, $+], Part) of
                {no_match, PartKey, <<>>} ->
                    #{ <<"path">> => PartKey };
                {Sep, PartKey, PartModBin} ->
                    parse_part_mods(
                        << Sep:8/integer, PartModBin/binary >>,
                        #{ <<"path">> => PartKey }
                    )
            end
    end.

%% @doc Parse part modifiers:
%% 1. `~Device' => `{as, Device, Msg}'
%% 2. `&K=V' => `Msg#{ K => V }'
parse_part_mods([], Msg) -> Msg;
parse_part_mods(<<>>, Msg) -> Msg;
parse_part_mods(<<"~", PartMods/binary>>, Msg) ->
    % Get the string until the end of the device specifier or end of string.
    {_, DeviceBin, InlinedMsgBin} = part([$&], PartMods),
    % Calculate the inlined keys
    MsgWithInlines = parse_part_mods(<<"&", InlinedMsgBin/binary >>, Msg),
    % Apply the device specifier
    {as, maybe_subpath(DeviceBin), MsgWithInlines};
parse_part_mods(<< "&", InlinedMsgBin/binary >>, Msg) ->
    parse_inlined_keys(InlinedMsgBin, Msg).

%% @doc Parse inlined key-value pairs from a path segment. Each key-value pair
%% is separated by `&' and is of the form `K=V'.
parse_inlined_keys(InlinedMsgBin, Msg) ->
    InlinedKeys = path_parts($&, InlinedMsgBin),
    MsgWithInlined =
        lists:foldl(
            fun(InlinedKey, Acc) ->
                {Key, Val} = parse_inlined_key_val(InlinedKey),
                ?event({inlined_key, {explicit, Key}, {explicit, Val}}),
                Acc#{ Key => Val }
            end,
            Msg,
            InlinedKeys
        ),
    MsgWithInlined.

%% @doc Extrapolate the inlined key-value pair from a path segment. If the
%% key has a value, it may provide a type (as with typical keys), but if a
%% value is not provided, it is assumed to be a boolean `true'.
parse_inlined_key_val(Bin) ->
    case part([$=, $&], Bin) of
        {no_match, K, <<>>} -> {K, true};
        {$=, K, V} ->
            {_, Key, Val} = maybe_typed(K, maybe_subpath(V)),
            {Key, Val}
    end.

%% @doc Attempt Cowboy URL decode, then sanitize the result.
decode_string(B) ->
    case catch uri_string:unquote(B) of
        DecodedBin when is_binary(DecodedBin) -> DecodedBin;
        _ -> throw({error, cannot_decode, B})
    end.

%% @doc Check if the string is a subpath, returning it in parsed form,
%% or the original string with a specifier.
maybe_subpath(Str) when byte_size(Str) >= 2 ->
    case {binary:first(Str), binary:last(Str)} of
        {$(, $)} ->
            Inside = binary:part(Str, 1, byte_size(Str) - 2),
            {resolve, from(#{ <<"path">> => Inside })};
        _ -> Str
    end;
maybe_subpath(Other) -> Other.

%% @doc Parse a key's type (applying it to the value) and device name if present.
maybe_typed(Key, Value) ->
    case part($+, Key) of
        {no_match, OnlyKey, <<>>} -> {untyped, OnlyKey, Value};
        {$+, OnlyKey, Type} ->
            case {Type, Value} of
                {<<"resolve">>, Subpath} ->
                    % If the value needs to be resolved before it is converted,
                    % use the `Codec/1.0' device to resolve it.
                    % For example:
                    % `/a/b&k+Int=(/x/y/z) => /a/b&k=(/x/y/z/body&Type=Int+Codec)'
                    {typed,
                        OnlyKey,
                        {resolve, from(#{ <<"path">> => Subpath })}
                    };
                {_T, Bin} when is_binary(Bin) ->
                    {typed, OnlyKey, dev_codec_structured:decode_value(Type, Bin)}
            end
    end.

%% @doc Join a list of items with a separator, or return the first item if there
%% is only one item. If there are no items, return an empty binary.
maybe_join(Items, Sep) ->
    case length(Items) of
        0 -> <<>>;
        1 -> hd(Items);
        _ -> iolist_to_binary(lists:join(Sep, Items))
    end.

%%% Tests

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
        from(Singleton1)
    ),
    DummyID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Singleton2 = #{
        <<"path">> => <<"/", DummyID/binary, "/a">>
    },
    ?assertEqual([DummyID, #{ <<"path">> => <<"a">> }], from(Singleton2)),
    Singleton3 = #{
        <<"path">> => <<"/", DummyID/binary, "/a">>,
        <<"a">> => <<"b">>
    },
    ?assertEqual(
        [DummyID, #{ <<"path">> => <<"a">>, <<"a">> => <<"b">> }],
        from(Singleton3)
    ).

%%% `to/1' function tests
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

simple_to_test() ->
    Messages = [
        #{<<"test-key">> => <<"test-value">>},
        #{<<"path">> => <<"a">>, <<"test-key">> => <<"test-value">>}
    ],
    Expected = #{<<"path">> => <<"/a">>, <<"test-key">> => <<"test-value">>},
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages))).

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
    ?assertEqual(Messages, from(to(Messages))).

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
    ?assertEqual(Messages, from(to(Messages))).

scoped_key_to_test() ->
    Messages = [
        #{},
        #{<<"path">> => <<"a">>},
        #{<<"path">> => <<"b">>, <<"test-key">> => <<"test-value">>},
        #{<<"path">> => <<"c">>}
    ],
    Expected = #{<<"2.test-key">> => <<"test-value">>, <<"path">> => <<"/a/b/c">>},
    ?assertEqual(Expected, to(Messages)),
    ?assertEqual(Messages, from(to(Messages))).

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
    ?assertEqual(Messages, from(to(Messages))).

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
    ?assertEqual(Messages, from(to(Messages))).

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
    ?assertEqual(Messages, from(to(Messages))).

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
    ?assertEqual(Messages, from(to(Messages))).

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
    ?assertEqual(Messages, from(to(Messages))).

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
    ?assertEqual(Messages, from(to(Messages))).

%%% `from/1' function tests
single_message_test() ->
    % This is a singleton TABM message
    Req = #{
        <<"path">> => <<"/a">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req),
    ?assertEqual(2, length(Msgs)),
    ?assert(is_map(hd(Msgs))),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, hd(Msgs))).

basic_hashpath_test() ->
    Hashpath = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Path = <<"/", Hashpath/binary, "/some-other">>,
    Req = #{
        <<"path">> => Path,
        <<"method">> => <<"GET">>
    },
    Msgs = from(Req),
    ?assertEqual(2, length(Msgs)),
    [Base, Msg2] = Msgs,
    ?assertEqual(Base, Hashpath),
    ?assertEqual(<<"GET">>, maps:get(<<"method">>, Msg2)),
    ?assertEqual(<<"some-other">>, maps:get(<<"path">>, Msg2)).

multiple_messages_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_Base, Msg1, Msg2, Msg3] = Msgs,
    ?assert(lists:all(fun is_map/1, Msgs)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg1)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg2)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg3)).

%%% Advanced key syntax tests

scoped_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key">> => <<"test-value">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(<<"test-value">>, maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg3, not_found)).

typed_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key+integer">> => <<"123">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(123, maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg3, not_found)).

subpath_in_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key+resolve">> => <<"/x/y/z">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg1, not_found)),
    ?assertEqual(
        {resolve,
            [
                #{},
                #{ <<"path">> => <<"x">> },
                #{ <<"path">> => <<"y">> },
                #{ <<"path">> => <<"z">> }
            ]
        },
        maps:get(<<"test-key">>, Msg2, not_found)
    ),
    ?assertEqual(not_found, maps:get(<<"test-key">>, Msg3, not_found)).

%%% Advanced path syntax tests

subpath_in_path_test() ->
    Req = #{
        <<"path">> => <<"/a/(x/y/z)/z">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(<<"a">>, maps:get(<<"path">>, Msg1)),
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
    ?assertEqual(<<"z">>, maps:get(<<"path">>, Msg3)).

inlined_keys_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/a/b&k1=v1/c&k2=v2">>
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, Msg1, Msg2, Msg3] = Msgs,
    ?assertEqual(<<"v1">>, maps:get(<<"k1">>, Msg2)),
    ?assertEqual(<<"v2">>, maps:get(<<"k2">>, Msg3)),
    ?assertEqual(not_found, maps:get(<<"k1">>, Msg1, not_found)),
    ?assertEqual(not_found, maps:get(<<"k2">>, Msg2, not_found)).

multiple_inlined_keys_test() ->
    Path = <<"/a/b&k1=v1&k2=v2">>,
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => Path
    },
    Msgs = from(Req),
    ?assertEqual(3, length(Msgs)),
    [_, Msg1, Msg2] = Msgs,
    ?assertEqual(not_found, maps:get(<<"k1">>, Msg1, not_found)),
    ?assertEqual(not_found, maps:get(<<"k2">>, Msg1, not_found)),
    ?assertEqual(<<"v1">>, maps:get(<<"k1">>, Msg2, not_found)),
    ?assertEqual(<<"v2">>, maps:get(<<"k2">>, Msg2, not_found)).

subpath_in_inlined_test() ->
    Path = <<"/part1/part2&test=1&b=(/x/y)/part3">>,
    Req = #{
        <<"path">> => Path
    },
    Msgs = from(Req),
    ?assertEqual(4, length(Msgs)),
    [_, First, Second, Third] = Msgs,
    ?assertEqual(<<"part1">>, maps:get(<<"path">>, First)),
    ?assertEqual(<<"part3">>, maps:get(<<"path">>, Third)),
    ?assertEqual(
        {resolve, [#{}, #{ <<"path">> => <<"x">> }, #{ <<"path">> => <<"y">> }] },
        maps:get(<<"b">>, Second)
    ).

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
    ok.