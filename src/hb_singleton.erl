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
%%%         - `Part=Value&... => #{ path => Part, Part => Value, ... }'
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
%%%         - key+int: 1 => #{ key => 1, ... }
%%%         - key+res: /nested/path => #{ key => (resolve /nested/path), ... }
%%%         - N.Key+res=(/a/b/c) => #{ Key => (resolve /a/b/c), ... }
%%% </pre>
-module(hb_singleton).
-export([from/2, from_path/1, to/1]).
% TODO: Move this
-export([expand_all/1]).
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
                    SubMessages1 = hb_maps:get(<<"path">>, to(SubMessages0)),
                    <<"/", SubMessages2/binary>> = SubMessages1,
                    SubMessages = <<"(", SubMessages2/binary, ")">>,
                    {append_path(SubMessages, Acc), Index + 1, ScopedModifications};
                % Regular case when message is a map
                (Message, {Acc, Index, ScopedModifications}) ->
                    {NewMessage, NewScopedModifications} =
                        hb_maps_raw:fold(
                            fun
                                (<<"path">>, PathPart, {AccIn, Scoped}) ->
                                    {append_path(PathPart, AccIn), Scoped};
                                % Specifically ignore method field from scope modifications
                                (<<"method">>, Value, {AccIn, Scoped}) ->
                                    {hb_maps_raw:put(<<"method">>, Value, AccIn), Scoped};
                                (Key, {resolve, SubMessages}, {AccIn, Scoped}) ->
                                    NewKey = <<Key/binary, "+resolve">>,
                                    NewSubMessages = hb_maps:get(<<"path">>, to(SubMessages)),
                                    {
                                        hb_maps_raw:put(NewKey, NewSubMessages, AccIn),
                                        hb_maps_raw:update_with(
                                            NewKey,
                                            fun(Indexes) -> [Index | Indexes] end,
                                            [Index],
                                            Scoped
                                        )
                                    };
                                (Key, Value, {AccIn, Scoped}) ->
                                    {
                                        hb_maps_raw:put(Key, Value, AccIn),
                                        hb_maps_raw:update_with(
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
        hb_maps:fold(
            fun
                % For the case when a given Key appeared only once in scopes
                (Key, [SingleIndexScope], AccIn) ->
                    Index = integer_to_binary(SingleIndexScope),
                    {Value, NewAccIn} = hb_maps:take(Key, AccIn),
                    {NewKey, NewValue} =
                        case type(Value) of
                            integer ->
                                K = <<Index/binary, ".", Key/binary, "+integer">>,
                                V = integer_to_binary(Value),
                                {K, V};
                            _ -> {<<Index/binary, ".", Key/binary>>, Value}
                        end,
                    hb_maps_raw:put(NewKey, NewValue, NewAccIn);
                (_Key, _Value, AccIn) -> AccIn
            end,
            TABMMessage,
            Scopes
        ),
    MessageWithTypeAndScopes.

append_path(PathPart, #{<<"path">> := Path} = Message) ->
    hb_maps_raw:put(<<"path">>, <<Path/binary, "/", PathPart/binary>>, Message);
append_path(PathPart, Message) ->
    hb_maps_raw:put(<<"path">>, <<"/", PathPart/binary>>, Message).

type(Value) when is_binary(Value) -> binary;
type(Value) when is_integer(Value) -> integer;
type(_Value) -> unknown.

%% @doc Normalize a singleton TABM message into a list of executable AO-Core
%% messages.
from(RawMsg, Opts) when is_binary(RawMsg) ->
    from(#{ <<"path">> => RawMsg }, Opts);
from(RawMsg, Opts) ->
    ?event(parsing, {from, {raw_msg, RawMsg}}),
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
    RawMsgs =
        lists:flatten(
            lists:map(
                fun(Msg) -> path_messages(Msg, Opts) end,
                Path
            )
        ),
    ?event(parsing, {raw_messages, {explicit, RawMsgs}}),
    Msgs = normalize_base(RawMsgs),
    ?event(parsing, {normalized_messages, Msgs}),
    % 3. Type keys and values
    ?event(parsing, {msg_without_base_path, {explicit, MsgWithoutBasePath}}),
    Typed = apply_types(MsgWithoutBasePath, Opts),
    ?event(parsing, {pre_group_scoped, {typed, Typed}, {msgs, Msgs}}),
    % 4. Group keys by N-scope and global scope
    ScopedModifications = group_scoped(Typed, Msgs),
    ?event(parsing, {scoped_modifications, ScopedModifications}),
    % 5. Generate the list of messages (plus-notation, device, typed keys).
    Result = build_messages(Msgs, ScopedModifications, Opts),
    ?event(parsing, {from_result, {result, Result}}),
    Result.

%% @doc Parse the relative reference into path, query, and fragment.
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
        maps:map(
            fun(_, Val) -> hb_util:unquote(Val) end,
            hb_maps:from_list(QKVList)
        )
    }.

%% @doc Step 2: Decode, split and sanitize the path. Split by `/' but avoid
%% subpath components, such that their own path parts are not dissociated from
%% their parent path.
path_messages(Bin, Opts) when is_binary(Bin) ->
    lists:map(fun(Part) -> parse_part(Part, Opts) end, path_parts([$/], Bin)).

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
    ?event({path_parts, Res}),
    Res.

%% @doc Extract all of the parts from the binary, given (a list of) separators.
all_path_parts(_Sep, <<>>) -> [];
all_path_parts(Sep, Bin) ->
    hb_util:split_depth_string_aware(Sep, Bin).

%% @doc Extract the characters from the binary until a separator is found.
%% The first argument of the function is an explicit separator character, or
%% a list of separator characters. Returns a tuple with the separator, the
%% accumulated characters, and the rest of the binary.
part(Sep, Bin) when not is_list(Sep) ->
    part([Sep], Bin);
part(Sep, Atom) when is_atom(Atom) ->
    part(Sep, hb_util:bin(Atom));
part(Seps, Bin) ->
    Res = hb_util:split_depth_string_aware_single(Seps, Bin).

%% @doc Step 3: Apply types to values and remove specifiers.
apply_types(Msg, Opts) ->
    hb_maps:fold(
        fun(Key, Val, Acc) ->
            ?event(debug_apply_types, {applying_types, {key, Key}, {value, Val}, {acc, Acc}}),
            {_, K, V} = maybe_typed(Key, Val, Opts),
            ?event(debug_apply_types, {applied_types, {result, {key, K}, {value, V}}}),
            hb_maps_raw:put(K, V, Acc, Opts)
        end,
        #{},
        Msg,
        Opts
    ).

%% @doc Parse a key's type (applying it to the value) and device name if present.
%% We allow ` ` characters as type indicators because some URL-string encoders
%% (e.g. Chrome) will encode `+` characters in a form that query-string parsers
%% interpret as ` ' characters.
maybe_typed(Key, Value, Opts) ->
    ?event(debug_maybe_typed, {maybe_typed, {key, Key}, {value, Value}}),
    case {part([$+, $ ], Key), Key} of
        {_, <<"...">>} -> {untyped, <<"...">>, apply_types(Value, Opts)};
        {{no_match, OnlyKey, <<>>}, _} -> {untyped, OnlyKey, Value};
        {{_, OnlyKey, Type}, _} ->
            case {Type, Value} of
                {<<"resolve">>, Subpath} ->
                    % If the value needs to be resolved before it is converted,
                    % use the `Codec/1.0' device to resolve it.
                    % For example:
                    % `/a/b&k+Int=(/x/y/z) => /a/b&k=(/x/y/z/body&Type=Int+Codec)'
                    {typed,
                        OnlyKey,
                        {resolve, from(#{ <<"path">> => Subpath }, Opts)}
                    };
                {_T, RawValue} when is_binary(RawValue) ->
                    Decoded = hb_escape:decode_quotes(RawValue),
                    {typed, OnlyKey, dev_codec_structured:decode_value(Type, Decoded)}
            end
    end.

%% @doc Step 4: Group headers/query by N-scope.
%% `N.Key' => applies to Nth step. Otherwise => `global'
group_scoped(Map, Msgs) ->
    ?event(debug_group_scoped, {grouping_scoped, {map, Map}, {msgs, Msgs}}),
    {NScope, Global} = get_map_scoped(Map),
    ?event(
        debug_group_scoped,
        {get_map_scoped, {nscope, NScope}, {global, Global}}
    ),
    ?event(debug_group_scoped, {grouped_scoped, {nscope, NScope}, {global, Global}}),
    ScopedMsgs =
        [
            hb_maps:merge(Global, hb_maps:get(N, NScope, #{}))
        ||
            N <- lists:seq(1, length(Msgs))
        ],
    ?event(debug_group_scoped, {scoped_msgs, {scoped_msgs, ScopedMsgs}}),
    ScopedMsgs.

get_map_scoped(Map) when ?IS_EMPTY_MESSAGE(Map) -> 
    ?event(debug_group_scoped, {empty_map, {explicit, Map}}),
    {#{}, #{}};
get_map_scoped(Map) -> 
    ?event(debug_group_scoped, {doing_layer, {map, Map}}),
    {NScope, Global} =
        hb_maps:fold(
            fun(KeyBin, Val, {Ns, Gs}) ->
                Scope = parse_scope(KeyBin),
                ?event(debug_group_scoped, {grouping_scoped, {key, KeyBin}, {scope, Scope}, {value, Val}, {ns, Ns}, {gs, Gs}}),
                case Scope of
                    {OkN, RealKey} when OkN > 0 ->
                        Curr = hb_maps_raw:get(OkN, Ns, #{}),
                        Ns2 = hb_maps_raw:put(OkN, hb_maps_raw:put(RealKey, Val, Curr), Ns),
                        {Ns2, Gs};
                    global -> {Ns, hb_maps_raw:put(KeyBin, Val, Gs)}
                end
            end,
            {#{}, #{}},
            hb_maps_raw:remove(<<"...">>, Map)
        ),
    ?event(debug_group_scoped, {did_top_layer, {nscope, NScope}, {global, Global}}),
    Nested = hb_maps_raw:get(<<"...">>, Map, #{}),
    case ?IS_EMPTY_MESSAGE(Nested) of
        true -> {NScope, Global};
        false ->
            {NestedNs, NestedGs} = get_map_scoped(Nested),
            {NScope#{ <<"...">> => NestedNs }, Global#{ <<"...">> => NestedGs }}
    end.
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
build_messages(Msgs, ScopedModifications, Opts) ->
    do_build(1, Msgs, ScopedModifications, Opts).

do_build(_, [], _, _) -> [];
do_build(I, [{as, DevID, RawMsg} | Rest], ScopedKeys, Opts) when is_map(RawMsg) ->
    % We are processing an `as' message. If the path is empty, we need to
    % remove it from the message and the additional message, such that AO-Core
    % returns only the message with the device specifier changed. If the message
    % does have a path, AO-Core will subresolve it.
    RawAdditional = lists:nth(I, ScopedKeys),
    {Msg, Additional} =
        case hb_maps:get(<<"path">>, RawMsg, <<"">>, Opts) of
            ID when ?IS_ID(ID) ->
                % When we have an ID, we do not merge the globally scoped elements.
                {
                    RawMsg,
                    #{}
                };
            <<"">> ->
                % When we have an empty path, we remove the path from both
                % messages. AO-Core will then simply set the device specifier
                % and not execute a subresolve.
                {
                    hb_maps:remove(<<"path">>, RawMsg, Opts),
                    hb_maps:remove(<<"path">>, RawAdditional, Opts)
                };
            _BasePath ->
                % When we have a non-empty path, we merge the messages in
                % totality. The path-part's path will be subresolved.
                {RawMsg, RawAdditional}
        end,
    Merged = hb_maps:merge(Additional, Msg, Opts),
    StepMsg = hb_message:convert(
        Merged, 
        <<"structured@1.0">>, 
        Opts#{ topic => ao_internal }
    ),
    ?event(parsing, {build_messages, {base, Msg}, {additional, Additional}}),
    [{as, DevID, StepMsg} | do_build(I + 1, Rest, ScopedKeys, Opts)];
do_build(I, [Msg | Rest], ScopedKeys, Opts) when not is_map(Msg) ->
    [Msg | do_build(I + 1, Rest, ScopedKeys, Opts)];
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

%% @doc Parse a path part into a message or an ID.
%% Applies the syntax rules outlined in the module doc, in the following order:
%% 1. ID
%% 2. Part subpath resolutions
%% 3. Inlined key-value pairs
%% 4. Device specifier
parse_part(ID, _Opts) when ?IS_ID(ID) -> ID;
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

%% @doc Parse part modifiers:
%% 1. `~Device' => `{as, Device, Msg}'
%% 2. `&K=V' => `Msg#{ K => V }'
parse_part_mods([], Msg, _Opts) -> Msg;
parse_part_mods(<<>>, Msg, _Opts) -> Msg;
parse_part_mods(<<"~", PartMods/binary>>, Msg, Opts) ->
    % Get the string until the end of the device specifier or end of string.
    {_, DeviceBin, InlinedMsgBin} = part([$&], PartMods),
    % Calculate the inlined keys
    MsgWithInlines = parse_part_mods(<<"&", InlinedMsgBin/binary >>, Msg, Opts),
    % Apply the device specifier
    {as, maybe_subpath(DeviceBin, Opts), MsgWithInlines};
parse_part_mods(<< "&", InlinedMsgBin/binary >>, Msg, Opts) ->
    InlinedKeys = path_parts($&, InlinedMsgBin),
    MsgWithInlined =
        lists:foldl(
            fun(InlinedKey, Acc) ->
                {Key, Val} = parse_inlined_key_val(InlinedKey, Opts),
                ?event({inlined_key, {explicit, Key}, {explicit, Val}}),
                hb_maps_raw:put(Key, Val, Acc)
            end,
            Msg,
            InlinedKeys
        ),
    MsgWithInlined;
parse_part_mods(<<$=, InlinedMsgBin/binary>>, M = #{ <<"path">> := Path }, Opts)
        when map_size(M) =:= 1, is_binary(Path) ->
    parse_part_mods(<< "&", Path/binary, "=", InlinedMsgBin/binary >>, M, Opts);
parse_part_mods(<<$+, InlinedMsgBin/binary>>, M = #{ <<"path">> := Path }, Opts)
        when map_size(M) =:= 1, is_binary(InlinedMsgBin) ->
    parse_part_mods(<< "&", Path/binary, "+", InlinedMsgBin/binary >>, M, Opts);
parse_part_mods(_, Msg, _Opts) -> Msg.

%% @doc Extrapolate the inlined key-value pair from a path segment. If the
%% key has a value, it may provide a type (as with typical keys), but if a
%% value is not provided, it is assumed to be a boolean `true'.
parse_inlined_key_val(Bin, Opts) ->
    case part([$=, $&], Bin) of
        {no_match, K, <<>>} -> {K, true};
        {$=, K, RawV} ->
            V = hb_util:unquote(RawV),
            {_, Key, Val} = maybe_typed(K, maybe_subpath(V, Opts), Opts),
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
maybe_subpath(Str, Opts) when byte_size(Str) >= 2 ->
    case {binary:first(Str), binary:last(Str)} of
        {$(, $)} ->
            Inside = binary:part(Str, 1, byte_size(Str) - 2),
            {resolve, from(#{ <<"path">> => Inside }, Opts)};
        _ -> Str
    end;
maybe_subpath(Other, _Opts) -> Other.


%% @doc Join a list of items with a separator, or return the first item if there
%% is only one item. If there are no items, return an empty binary.
maybe_join(Items, Sep) ->
    case length(Items) of
        0 -> <<>>;
        1 -> hd(Items);
        _ -> iolist_to_binary(lists:join(Sep, Items))
    end.

%%% Tests
expand_all(List) when is_list(List) ->
    lists:map(
        fun
            (Item) when is_map(Item) -> expand_all(Item);
            (Item) when is_list(Item) -> expand_all(Item);
            ({resolve, SubMessages}) -> {resolve, expand_all(SubMessages)};
            (Item) -> Item
        end,
        List
    );
expand_all(RawMap) when is_map(RawMap) ->
    Map = hb_maps:expand(RawMap, #{}),
    maps:map(
        fun(_, Value) ->
            expand_all(Value)
        end,
        Map
    );
expand_all({resolve, SubMessages}) -> {resolve, expand_all(SubMessages)};
expand_all(Other) ->
    Other.

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
        expand_all(from(Singleton1, #{}))
    ),
    DummyID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Singleton2 = #{
        <<"path">> => <<"/", DummyID/binary, "/a">>
    },
    ?assertEqual([DummyID, #{ <<"path">> => <<"a">> }], expand_all(from(Singleton2, #{}))),
    Singleton3 = #{
        <<"path">> => <<"/", DummyID/binary, "/a">>,
        <<"a">> => <<"b">>
    },
    ?assertEqual(
        [DummyID, #{ <<"path">> => <<"a">>, <<"a">> => <<"b">> }],
        expand_all(from(Singleton3, #{}))
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
    ?assertEqual(Expected, expand_all(to(Messages))),
    ?assertEqual(Messages, expand_all(from(to(Messages), #{}))).

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
    ?assertEqual(Expected, expand_all(to(Messages))),
    ?assertEqual(Messages, expand_all(from(to(Messages), #{}))).

basic_hashpath_to_test() ->
    Messages = [
        <<"e5ohB7TgMYRoc0BLllkmAqkqLy1SrliEkOPJlNPXBQ8">>,
        #{<<"method">> => <<"GET">>, <<"path">> => <<"some-other">>}
    ],
    Expected = #{
        <<"path">> => <<"/e5ohB7TgMYRoc0BLllkmAqkqLy1SrliEkOPJlNPXBQ8/some-other">>,
        <<"method">> => <<"GET">>
    },
    ?assertEqual(Expected, expand_all(to(Messages))),
    ?assertEqual(Messages, expand_all(from(to(Messages), #{}))).

scoped_key_to_test() ->
    Messages = [
        #{},
        #{<<"path">> => <<"a">>},
        #{<<"path">> => <<"b">>, <<"test-key">> => <<"test-value">>},
        #{<<"path">> => <<"c">>}
    ],
    Expected = #{<<"2.test-key">> => <<"test-value">>, <<"path">> => <<"/a/b/c">>},
    ?assertEqual(Expected, expand_all(to(Messages))),
    ?assertEqual(Messages, expand_all(from(to(Messages), #{}))).

typed_key_to_test() ->
    Messages =
        [
            #{},
            #{<<"path">> => <<"a">>},
            #{<<"path">> => <<"b">>, <<"test-key">> => 123},
            #{<<"path">> => <<"c">>}
        ],
    Expected = #{<<"2.test-key+integer">> => <<"123">>, <<"path">> => <<"/a/b/c">>},
    ?assertEqual(Expected, expand_all(to(Messages))),
    ?assertEqual(Messages, expand_all(from(to(Messages), #{}))).

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
    ?assertEqual(Expected, expand_all(to(Messages))),
    Res = from(to(Messages), #{}),
    ?event(debug_expand_all, {res, {explicit, Res}}),
    ERes = expand_all(Res),
    ?event(debug_expand_all, {eres, {explicit, ERes}}),
    ?assertEqual(Messages, ERes).

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
    ?assertEqual(Expected, expand_all(to(Messages))),
    ?assertEqual(Messages, expand_all(from(to(Messages), #{}))).

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
    ?assertEqual(Messages, expand_all(from(to(Messages), #{}))).

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
    ?assertEqual(Messages, expand_all(from(to(Messages), #{}))).

subpath_in_inlined_to_test() ->
    Messages =
        [
            #{},
            #{<<"path">> => <<"part1">>},
            #{
                <<"b">> =>
                    {
                        resolve,
                        [
                            #{},
                            #{<<"path">> => <<"x">>},
                            #{<<"path">> => <<"y">>}
                        ]
                    },
                <<"path">> => <<"part2">>,
                <<"test">> => <<"1">>
            },
            #{<<"path">> => <<"part3">>}
        ],
    % NOTE: The implementation above does not convert the given list of messages
    % into the original format, however it assures that the `to/1' and `from/1'
    % operations are idempotent.
    ?assertEqual(Messages, expand_all(from(to(Messages), #{}))).

%%% `from/1' function tests
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

multiple_messages_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"test-key">> => <<"test-value">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_Base, Base, Msg2, Res] = Msgs,
    ?assert(lists:all(fun is_map/1, Msgs)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Base)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Msg2)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Res)).

%%% Advanced key syntax tests

scoped_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key">> => <<"test-value">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Base, not_found)),
    ?assertEqual(<<"test-value">>, hb_maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Res, not_found)).

typed_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key+integer">> => <<"123">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Base, not_found)),
    ?assertEqual(123, hb_maps:get(<<"test-key">>, Msg2, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Res, not_found)).

subpath_in_key_test() ->
    Req = #{
        <<"path">> => <<"/a/b/c">>,
        <<"2.test-key+resolve">> => <<"/x/y/z">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Base, not_found)),
    ?assertEqual(
        {resolve,
            [
                #{},
                #{ <<"path">> => <<"x">> },
                #{ <<"path">> => <<"y">> },
                #{ <<"path">> => <<"z">> }
            ]
        },
        expand_all(hb_maps:get(<<"test-key">>, Msg2, not_found))
    ),
    ?assertEqual(not_found, hb_maps:get(<<"test-key">>, Res, not_found)).

%%% Advanced path syntax tests

subpath_in_path_test() ->
    Req = #{
        <<"path">> => <<"/a/(x/y/z)/z">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(<<"a">>, hb_maps:get(<<"path">>, Base)),
    ?assertEqual(
        {resolve,
            [
                #{},
                #{ <<"path">> => <<"x">> },
                #{ <<"path">> => <<"y">> },
                #{ <<"path">> => <<"z">> }
            ]
        },
        expand_all(Msg2)
    ),
    ?assertEqual(<<"z">>, hb_maps:get(<<"path">>, Res)).

inlined_keys_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/a/b&k1=v1/c&k2=v2">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(<<"v1">>, hb_maps:get(<<"k1">>, Msg2)),
    ?assertEqual(<<"v2">>, hb_maps:get(<<"k2">>, Res)),
    ?assertEqual(not_found, hb_maps:get(<<"k1">>, Base, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"k2">>, Msg2, not_found)).

inlined_quoted_key_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/a/b&k1=\"v/1\"/c&k2=v2">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?assertEqual(<<"v/1">>, hb_maps:get(<<"k1">>, Msg2)),
    ?assertEqual(<<"v2">>, hb_maps:get(<<"k2">>, Res)),
    ?assertEqual(not_found, hb_maps:get(<<"k1">>, Base, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"k2">>, Msg2, not_found)),
    ReqB = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/~profile@1.0/eval=%22~meta@1.0/info%22">>
    },
    MsgsB = from(ReqB, #{}),
    [_, Msg2b] = MsgsB,
    ?assertEqual(<<"~meta@1.0/info">>, hb_maps:get(<<"eval">>, Msg2b)).

inlined_assumed_key_test() ->
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => <<"/a/b=4/c&k2=v2">>
    },
    Msgs = from(Req, #{}),
    ?assertEqual(4, length(Msgs)),
    [_, Base, Msg2, Res] = Msgs,
    ?event({parsed, Msgs}),
    ?assertEqual(<<"4">>, hb_maps:get(<<"b">>, Msg2)),
    ?assertEqual(not_found, hb_maps:get(<<"b">>, Base, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"b">>, Res, not_found)),
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


multiple_inlined_keys_test() ->
    Path = <<"/a/b&k1=v1&k2=v2">>,
    Req = #{
        <<"method">> => <<"POST">>,
        <<"path">> => Path
    },
    Msgs = from(Req, #{}),
    ?assertEqual(3, length(Msgs)),
    [_, Base, Msg2] = Msgs,
    ?assertEqual(not_found, hb_maps:get(<<"k1">>, Base, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"k2">>, Base, not_found)),
    ?assertEqual(<<"v1">>, hb_maps:get(<<"k1">>, Msg2, not_found)),
    ?assertEqual(<<"v2">>, hb_maps:get(<<"k2">>, Msg2, not_found)).

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
        expand_all(hb_maps:get(<<"b">>, Second))
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
            <<"req">>
        ],
        path_parts($/, <<"/IYkkrqlZNW_J-4T-5eFApZOMRl5P4VjvrcOXWvIqB1Q/req">>)
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

path_messages_space_edge_case_test() ->
    path_messages(<<"42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA~manifest@1.0/[object Object]">>, #{}).
