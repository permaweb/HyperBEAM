%%% @doc Build human-facing and agent-facing descriptions of AO-Core devices.
-module(hb_introspect).
-export([describe/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Describe the device semantics of a message in a transport-friendly form.
describe(Base, Opts) ->
    Device = hb_ao_device:message_to_device(Base, Opts),
    DeviceName = device_name(Device, Base, Opts),
    DeviceInfo = normalize_value(hb_ao_device:info(Device, Base, Opts)),
    {KeySchemas, NamedTypes} =
        case hb_types:extract(Device, Opts) of
            {ok, #{ <<"keys">> := Keys, <<"types">> := Types }} -> {Keys, Types};
            _ -> {#{}, #{}}
        end,
    Docs = docs(Device),
    PublicKeys = public_keys(Device, DeviceInfo, KeySchemas, Opts),
    KeyEntries =
        lists:map(
            fun(Key) -> describe_key(Device, Key, KeySchemas, Docs, Opts) end,
            PublicKeys
        ),
    Cookbook = cookbook(Device, KeyEntries, Opts),
    {ok,
        #{
            <<"name">> => DeviceName,
            <<"module">> => hb_util:bin(atom_to_binary(Device, utf8)),
            <<"module-doc">> => maps:get(module_doc, Docs, <<>>),
            <<"cookbook">> => Cookbook,
            <<"keys">> => KeyEntries,
            <<"types">> => NamedTypes,
            <<"metadata">> => DeviceInfo
        }
    }.

device_name(Device, Base, Opts) ->
    case hb_maps:find(<<"device">>, Base, Opts) of
        {ok, DeviceName} ->
            hb_util:bin(DeviceName);
        error ->
            case lists:search(
                fun
                    (#{ <<"module">> := Mod }) when Mod =:= Device -> true;
                    (_) -> false
                end,
                hb_opts:get(preloaded_devices, [], Opts)
            ) of
                {value, #{ <<"name">> := Name }} -> Name;
                false -> hb_util:bin(atom_to_binary(Device, utf8))
            end
    end.

public_keys(Device, Info, KeySchemas, _Opts) ->
    InfoKeys =
        case maps:get(<<"exports">>, Info, undefined) of
            undefined -> inferred_public_keys(Device);
            Exports ->
                lists:map(fun normalize_name/1, Exports)
        end,
    GenericKeys = [<<"info">>, <<"cookbook">>, <<"types">>],
    lists:sort(lists:usort(InfoKeys ++ GenericKeys ++ maps:keys(KeySchemas))).

inferred_public_keys(Device) ->
    lists:usort(
        [
            normalize_name(Name)
        ||
            {Name, Arity} <- Device:module_info(exports),
            public_arity(Arity),
            not internal_export(Name)
        ]
    ).

public_arity(Arity) ->
    is_integer(Arity) andalso Arity >= 0 andalso Arity =< 3.

internal_export(module_info) -> true;
internal_export(Name) ->
    NameBin = hb_util:bin(atom_to_binary(Name, utf8)),
    binary:match(NameBin, <<"_test">>) =/= nomatch.

describe_key(Device, Key, KeySchemas, Docs, _Opts) ->
    TypeInfo = maps:get(Key, KeySchemas, #{}),
    DocInfo =
        case find_function_doc(Key, Docs) of
            undefined when Key =:= <<"info">>; Key =:= <<"cookbook">>; Key =:= <<"types">> ->
                find_function_doc(Key, docs(dev_message));
            undefined ->
                #{};
            Found ->
                Found
        end,
    #{
        <<"key">> => Key,
        <<"doc">> => maps:get(doc, DocInfo, <<>>),
        <<"signatures">> => maps:get(signatures, DocInfo, []),
        <<"types">> => normalize_value(TypeInfo),
        <<"base">> => format_type_field(<<"base">>, TypeInfo),
        <<"request">> => format_type_field(<<"request">>, TypeInfo),
        <<"return">> => format_type_field(<<"return">>, TypeInfo),
        <<"module">> => hb_util:bin(atom_to_binary(Device, utf8))
    }.

format_type_field(Field, TypeInfo) ->
    case maps:get(Field, TypeInfo, undefined) of
        undefined -> <<>>;
        Type -> hb_types:format(Type)
    end.

cookbook(Device, KeyEntries, Opts) ->
    Manual = manual_cookbook(Device, Opts),
    Defaults =
        lists:map(
            fun(Entry) ->
                #{
                    <<"title">> => <<"/", (maps:get(<<"key">>, Entry))/binary>>,
                    <<"path">> => maps:get(<<"key">>, Entry),
                    <<"body">> =>
                        case maps:get(<<"doc">>, Entry, <<>>) of
                            <<>> -> <<"Resolve this key on the current message.">>;
                            Doc -> first_paragraph(Doc)
                        end
                }
            end,
            KeyEntries
        ),
    dedupe_cookbook(normalize_cookbook(Manual ++ Defaults, Opts)).

manual_cookbook(Device, Opts) ->
    case erlang:function_exported(Device, cookbook, 1) of
        true -> Device:cookbook(Opts);
        false ->
            case erlang:function_exported(Device, cookbook, 0) of
                true -> Device:cookbook();
                false -> []
            end
    end.

normalize_cookbook(Entries, Opts) ->
    lists:map(
        fun
            (Entry) when is_map(Entry) ->
                Norm = hb_ao:normalize_keys(Entry, Opts),
                BaseEntry =
                    #{
                        <<"title">> => maps:get(<<"title">>, Norm, <<"Untitled example">>),
                        <<"path">> => maps:get(<<"path">>, Norm, <<>>),
                        <<"body">> => maps:get(<<"body">>, Norm, <<>>)
                    },
                case maps:find(<<"expected">>, Norm) of
                    {ok, Value} ->
                        BaseEntry#{ <<"expected">> => normalize_value(Value) };
                    error ->
                        BaseEntry
                end;
            (Path) ->
                #{
                    <<"title">> => hb_util:bin(io_lib:format("~ts", [Path])),
                    <<"path">> => hb_util:bin(Path),
                    <<"body">> => <<>>,
                    <<"expected">> => undefined
                }
        end,
        Entries
    ).

dedupe_cookbook(Entries) ->
    {_, Deduped} =
        lists:foldl(
            fun(Entry = #{ <<"path">> := Path }, {Seen, Acc}) ->
                case sets:is_element(Path, Seen) of
                    true -> {Seen, Acc};
                    false -> {sets:add_element(Path, Seen), [Entry | Acc]}
                end
            end,
            {sets:new([{version, 2}]), []},
            Entries
        ),
    lists:reverse(Deduped).

docs(Device) ->
    case code:get_doc(Device) of
        {ok, {docs_v1, _, _, _, ModuleDoc, _, Entries}} ->
            #{
                module_doc => doc_value(ModuleDoc),
                functions => docs_by_key(Entries)
            };
        _ ->
            #{ module_doc => <<>>, functions => #{} }
    end.

docs_by_key(Entries) ->
    lists:foldl(
        fun
            ({{function, Name, Arity}, _, Signatures, Doc, _Meta}, Acc) ->
                Key = normalize_name(Name),
                Entry =
                    #{
                        arity => Arity,
                        doc => doc_value(Doc),
                        signatures => normalize_signatures(Signatures)
                    },
                Acc#{
                    Key => [Entry | maps:get(Key, Acc, [])]
                };
            (_, Acc) ->
                Acc
        end,
        #{},
        Entries
    ).

find_function_doc(Key, Docs) ->
    Entries = maps:get(Key, maps:get(functions, Docs, #{}), []),
    case lists:sort(fun compare_doc_priority/2, Entries) of
        [Best | _] -> Best;
        [] -> undefined
    end.

compare_doc_priority(#{ arity := ArityA }, #{ arity := ArityB }) ->
    doc_priority(ArityA) =< doc_priority(ArityB).

doc_priority(3) -> 0;
doc_priority(2) -> 1;
doc_priority(1) -> 2;
doc_priority(0) -> 3;
doc_priority(Other) -> Other + 10.

normalize_signatures(Signatures) ->
    lists:map(fun hb_util:bin/1, Signatures).

doc_value(#{ <<"en">> := Value }) -> hb_util:bin(Value);
doc_value(none) -> <<>>;
doc_value(hidden) -> <<>>;
doc_value(Value) when is_binary(Value) -> Value;
doc_value(_) -> <<>>.

first_paragraph(Doc) ->
    hd(binary:split(Doc, <<"\n\n">>, [global])).

normalize_value(Value) when is_map(Value) ->
    maps:from_list(
        lists:map(
            fun({Key, Inner}) ->
                {normalize_name(Key), normalize_value(Inner)}
            end,
            maps:to_list(Value)
        )
    );
normalize_value(Value) when is_list(Value) ->
    lists:map(fun normalize_value/1, Value);
normalize_value(Value) when is_atom(Value), Value =/= true, Value =/= false ->
    hb_util:bin(atom_to_binary(Value, utf8));
normalize_value(Value) when is_function(Value) ->
    {arity, Arity} = erlang:fun_info(Value, arity),
    <<"#Fun/", (integer_to_binary(Arity))/binary>>;
normalize_value(Value) ->
    Value.

normalize_name(Name) when is_binary(Name) ->
    binary:replace(hb_util:bin(Name), <<"_">>, <<"-">>, [global]);
normalize_name(Name) when is_atom(Name) ->
    normalize_name(atom_to_binary(Name, utf8));
normalize_name(Name) when is_list(Name) ->
    normalize_name(hb_util:bin(Name));
normalize_name(Name) ->
    hb_util:bin(io_lib:format("~tp", [Name])).

%%% Tests

normalize_value_fun_test() ->
    ?assertMatch(<<"#Fun/", _/binary>>, normalize_value(fun erlang:length/1)).
