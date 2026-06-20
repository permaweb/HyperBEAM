#!/usr/bin/env escript
%%! -noshell
%%% @doc Generate a standalone HyperBEAM codebase call-graph visualizer.

main(Args) ->
    Root = canonical_root(value_arg("--root", Args, ".")),
    Out = filename:absname(
        value_arg(
            "--out",
            Args,
            filename:join([Root, "build", "codeviz", "hyperbeam-codeviz.html"])
        )
    ),
    ok = filelib:ensure_dir(Out),
    Modules = parse_modules(Root),
    Graph = graph(Root, Modules),
    Html = render_html(Root, Graph),
    ok = file:write_file(Out, Html),
    io:format(
        "Wrote ~s (~p modules, ~p functions, ~p calls).~n",
        [
            Out,
            length(maps:get(<<"modules">>, Graph)),
            length(maps:get(<<"functions">>, Graph)),
            length(maps:get(<<"edges">>, Graph))
        ]
    ).

value_arg(Name, Args, Default) ->
    Prefix = Name ++ "=",
    case [ string:slice(Arg, length(Prefix)) || Arg <- Args,
            lists:prefix(Prefix, Arg) ] of
        [Value | _] -> Value;
        [] -> Default
    end.

canonical_root(Path) ->
    Abs = filename:absname(Path),
    case filename:basename(Abs) of
        "." -> filename:dirname(Abs);
        _ -> Abs
    end.

parse_modules(Root) ->
    Files = erl_files(Root),
    Parsed = [parse_module(Root, File) || File <- Files],
    lists:sort(fun(A, B) -> maps:get(module, A) =< maps:get(module, B) end, Parsed).

erl_files(Root) ->
    Patterns = [
        filename:join([Root, "src", "core", "*", "*.erl"]),
        filename:join([Root, "src", "preloaded", "*", "*.erl"]),
        filename:join([Root, "src", "forge", "*.erl"]),
        filename:join([Root, "src", "forge", "plugin", "src", "*.erl"])
    ],
    Files = lists:usort(lists:append([filelib:wildcard(P) || P <- Patterns])),
    [File || File <- Files, not excluded_file(Root, File)].

excluded_file(Root, File) ->
    Rel = relpath(Root, File),
    string:find("/" ++ Rel, "/test/") =/= nomatch orelse
        lists:suffix("_test_vectors.erl", Rel) orelse
        lists:suffix("_tests.erl", Rel).

parse_module(Root, File) ->
    {ok, Source} = file:read_file(File),
    Lines = binary:split(Source, <<"\n">>, [global]),
    Forms =
        case epp:parse_file(File, include_paths(Root), []) of
            {ok, ParsedForms} -> ParsedForms;
            {error, _Reason} -> []
        end,
    Module = module_name(File, Forms),
    Exports = exports(Forms),
    Role = role(Root, File),
    Group = group(Root, File, Role),
    Device = device_name(Module, Source, Forms, Role),
    DeviceRefs = device_refs(Source),
    #{
        module => Module,
        id => atom_bin(Module),
        path => unicode:characters_to_binary(relpath(Root, File)),
        role => Role,
        group => Group,
        device => Device,
        device_refs => DeviceRefs,
        doc => module_doc(Lines),
        loc => length(Lines),
        exports => Exports,
        functions => functions(Module, Exports, Forms, Lines)
    }.

include_paths(Root) ->
    [
        filename:join([Root, "include"]),
        filename:join([Root, "src"]),
        filename:join([Root, "_build", "default", "lib", "hb", "include"])
    ].

module_name(File, Forms) ->
    case [M || {attribute, _, module, M} <- Forms] of
        [Module | _] -> Module;
        [] -> list_to_atom(filename:basename(File, ".erl"))
    end.

exports(Forms) ->
    lists:usort(
        lists:append([Pairs || {attribute, _, export, Pairs} <- Forms])
    ).

functions(Module, Exports, Forms, Lines) ->
    FunctionForms = [
        {Line, Name, Arity, Clauses}
    || {function, Line, Name, Arity, Clauses} <- Forms ],
    NextLines = tl([Line || {Line, _, _, _} <- FunctionForms] ++ [length(Lines) + 1]),
    [
        #{
            module => Module,
            name => Name,
            arity => Arity,
            id => function_id(Module, Name, Arity),
            line => Line,
            exported => lists:member({Name, Arity}, Exports),
            doc => function_doc(Line, Lines),
            source => source_excerpt(Line, NextLine, Lines),
            calls => collect_calls(Module, Clauses)
        }
    || {{Line, Name, Arity, Clauses}, NextLine} <- lists:zip(FunctionForms, NextLines),
        not excluded_function(Name) ].

excluded_function(Name) ->
    Text = atom_to_list(Name),
    lists:suffix("_test", Text) orelse lists:suffix("_test_", Text).

collect_calls(Module, Clauses) ->
    lists:usort(walk(Clauses, Module, [])).

walk({call, Line, {remote, _, {atom, _, Mod}, {atom, _, Fun}}, Args}, Module, Acc) ->
    walk(Args, Module, [#{type => remote, line => Line, module => Mod,
        function => Fun, arity => length(Args)} | Acc]);
walk({call, Line, {atom, _, apply}, [{atom, _, Mod}, {atom, _, Fun}, Args]},
        Module, Acc) ->
    Arity = literal_list_length(Args),
    walk(Args, Module, [#{type => apply, line => Line, module => Mod,
        function => Fun, arity => Arity} | Acc]);
walk({call, Line, {remote, _, {atom, _, erlang}, {atom, _, apply}},
        [{atom, _, Mod}, {atom, _, Fun}, Args]}, Module, Acc) ->
    Arity = literal_list_length(Args),
    walk(Args, Module, [#{type => apply, line => Line, module => Mod,
        function => Fun, arity => Arity} | Acc]);
walk({call, Line, {atom, _, Fun}, Args}, Module, Acc) ->
    walk(Args, Module, [#{type => local, line => Line, module => Module,
        function => Fun, arity => length(Args)} | Acc]);
walk({'fun', Line, {function, Fun, Arity}}, Module, Acc) ->
    [#{type => fun_ref, line => Line, module => Module,
        function => Fun, arity => Arity} | Acc];
walk({'fun', Line, {function, Mod, Fun, Arity}}, _Module, Acc)
        when is_atom(Mod), is_atom(Fun) ->
    [#{type => fun_ref, line => Line, module => Mod,
        function => Fun, arity => Arity} | Acc];
walk(Term, Module, Acc) when is_tuple(Term) ->
    walk(tuple_to_list(Term), Module, Acc);
walk(Term, Module, Acc) when is_list(Term) ->
    lists:foldl(fun(Child, ChildAcc) -> walk(Child, Module, ChildAcc) end, Acc, Term);
walk(Term, Module, Acc) when is_map(Term) ->
    walk(maps:to_list(Term), Module, Acc);
walk(_Term, _Module, Acc) ->
    Acc.

literal_list_length({nil, _}) -> 0;
literal_list_length({cons, _, _Head, Tail}) ->
    case literal_list_length(Tail) of
        unknown -> unknown;
        Count -> Count + 1
    end;
literal_list_length(_) -> unknown.

graph(Root, Modules) ->
    FunctionIndexes = function_indexes(Modules),
    Edges = edges(Modules, FunctionIndexes),
    Functions = function_nodes(Modules, Edges),
    ModuleNodes = module_nodes(Modules, Functions, Edges),
    Devices = device_nodes(ModuleNodes),
    Groups = group_nodes(ModuleNodes),
    #{
        <<"meta">> => #{
            <<"generated-at">> => generated_at(),
            <<"root">> => unicode:characters_to_binary(Root),
            <<"tool">> => <<"scripts/codeviz/generate.escript">>
        },
        <<"modules">> => ModuleNodes,
        <<"functions">> => Functions,
        <<"edges">> => Edges,
        <<"devices">> => Devices,
        <<"groups">> => Groups
    }.

function_indexes(Modules) ->
    lists:foldl(
        fun(Module, Acc) ->
            lists:foldl(
                fun(Fun, FunAcc) ->
                    Key = {maps:get(module, Fun), maps:get(name, Fun), maps:get(arity, Fun)},
                    maps:put(Key, maps:get(id, Fun), FunAcc)
                end,
                Acc,
                maps:get(functions, Module)
            )
        end,
        #{},
        Modules
    ).

edges(Modules, FunctionIndexes) ->
    Raw = lists:append([
        function_edges(Fun, FunctionIndexes)
    || Module <- Modules, Fun <- maps:get(functions, Module) ]),
    maps:values(
        lists:foldl(
            fun(Edge, Acc) ->
                Key = {maps:get(<<"source">>, Edge), maps:get(<<"target">>, Edge)},
                maps:update_with(
                    Key,
                    fun(Old) ->
                        Old#{
                            <<"count">> => maps:get(<<"count">>, Old) + 1,
                            <<"lines">> => add_line(
                                maps:get(<<"line">>, Edge),
                                maps:get(<<"lines">>, Old)
                            )
                        }
                    end,
                    Edge#{
                        <<"id">> => edge_id(Edge),
                        <<"count">> => 1,
                        <<"lines">> => add_line(maps:get(<<"line">>, Edge), [])
                    },
                    Acc
                )
            end,
            #{},
            Raw
        )
    ).

function_edges(Fun, FunctionIndexes) ->
    Source = maps:get(id, Fun),
    SourceMod = atom_bin(maps:get(module, Fun)),
    lists:filtermap(
        fun(Call) ->
            Arity = maps:get(arity, Call),
            case is_integer(Arity) of
                false ->
                    false;
                true ->
                    TargetKey = {
                        maps:get(module, Call),
                        maps:get(function, Call),
                        Arity
                    },
                    case maps:find(TargetKey, FunctionIndexes) of
                        {ok, Target} when Target =/= Source ->
                            {true, #{
                                <<"source">> => Source,
                                <<"target">> => Target,
                                <<"source-module">> => SourceMod,
                                <<"target-module">> =>
                                    atom_bin(maps:get(module, Call)),
                                <<"type">> => atom_bin(maps:get(type, Call)),
                                <<"line">> => maps:get(line, Call)
                            }};
                        _ ->
                            false
                    end
            end
        end,
        maps:get(calls, Fun)
    ).

add_line(Line, Lines) ->
    case lists:member(Line, Lines) orelse length(Lines) >= 8 of
        true -> Lines;
        false -> Lines ++ [Line]
    end.

edge_id(Edge) ->
    <<(maps:get(<<"source">>, Edge))/binary, "->", (maps:get(<<"target">>, Edge))/binary>>.

function_nodes(Modules, Edges) ->
    Counts = edge_counts(Edges),
    lists:append([
        [
            function_node(Module, Fun, Counts)
        || Fun <- maps:get(functions, Module) ]
    || Module <- Modules ]).

function_node(Module, Fun, Counts) ->
    Id = maps:get(id, Fun),
    #{
        <<"id">> => Id,
        <<"module">> => maps:get(id, Module),
        <<"function">> => atom_bin(maps:get(name, Fun)),
        <<"arity">> => maps:get(arity, Fun),
        <<"label">> => function_label(Fun),
        <<"path">> => maps:get(path, Module),
        <<"line">> => maps:get(line, Fun),
        <<"role">> => maps:get(role, Module),
        <<"group">> => maps:get(group, Module),
        <<"device">> => maps:get(device, Module),
        <<"device-refs">> => maps:get(device_refs, Module),
        <<"exported">> => maps:get(exported, Fun),
        <<"doc">> => maps:get(doc, Fun),
        <<"source">> => maps:get(source, Fun),
        <<"calls-out">> => maps:get({out, Id}, Counts, 0),
        <<"calls-in">> => maps:get({in, Id}, Counts, 0)
    }.

edge_counts(Edges) ->
    lists:foldl(
        fun(Edge, Acc) ->
            Source = maps:get(<<"source">>, Edge),
            Target = maps:get(<<"target">>, Edge),
            Count = maps:get(<<"count">>, Edge),
            Acc#{
                {out, Source} => maps:get({out, Source}, Acc, 0) + Count,
                {in, Target} => maps:get({in, Target}, Acc, 0) + Count
            }
        end,
        #{},
        Edges
    ).

module_nodes(Modules, Functions, Edges) ->
    FunctionCounts = count_by(<<"module">>, Functions),
    ExportCounts = count_exported(Functions),
    InCounts = count_module_edges(<<"target-module">>, Edges),
    OutCounts = count_module_edges(<<"source-module">>, Edges),
    [
        #{
            <<"id">> => maps:get(id, Module),
            <<"module">> => maps:get(id, Module),
            <<"path">> => maps:get(path, Module),
            <<"role">> => maps:get(role, Module),
            <<"group">> => maps:get(group, Module),
            <<"subsystem">> => maps:get(group, Module),
            <<"device">> => maps:get(device, Module),
            <<"device-refs">> => maps:get(device_refs, Module),
            <<"doc">> => maps:get(doc, Module),
            <<"loc">> => maps:get(loc, Module),
            <<"functions">> => maps:get(maps:get(id, Module), FunctionCounts, 0),
            <<"exports">> => maps:get(maps:get(id, Module), ExportCounts, 0),
            <<"calls-in">> => maps:get(maps:get(id, Module), InCounts, 0),
            <<"calls-out">> => maps:get(maps:get(id, Module), OutCounts, 0)
        }
    || Module <- Modules ].

count_by(Key, Items) ->
    lists:foldl(
        fun(Item, Acc) ->
            Value = maps:get(Key, Item),
            maps:put(Value, maps:get(Value, Acc, 0) + 1, Acc)
        end,
        #{},
        Items
    ).

count_exported(Functions) ->
    lists:foldl(
        fun(Fun, Acc) ->
            case maps:get(<<"exported">>, Fun) of
                true ->
                    Module = maps:get(<<"module">>, Fun),
                    maps:put(Module, maps:get(Module, Acc, 0) + 1, Acc);
                false ->
                    Acc
            end
        end,
        #{},
        Functions
    ).

count_module_edges(Key, Edges) ->
    lists:foldl(
        fun(Edge, Acc) ->
            Module = maps:get(Key, Edge),
            maps:put(Module, maps:get(Module, Acc, 0) + maps:get(<<"count">>, Edge), Acc)
        end,
        #{},
        Edges
    ).

device_nodes(Modules) ->
    DeviceModules = [M || M <- Modules, maps:get(<<"role">>, M) =:= <<"device">>],
    DeviceIds = lists:usort([maps:get(<<"device">>, M) || M <- DeviceModules]),
    [
        device_node(Device, [M || M <- DeviceModules, maps:get(<<"device">>, M) =:= Device])
    || Device <- DeviceIds ].

device_node(Device, Modules) ->
    #{
        <<"id">> => Device,
        <<"label">> => device_label(Device),
        <<"group">> => device_group(Modules),
        <<"modules">> => [maps:get(<<"id">>, M) || M <- Modules],
        <<"functions">> => lists:sum([maps:get(<<"functions">>, M) || M <- Modules])
    }.

device_label(<<"support:", Rest/binary>>) -> Rest;
device_label(Device) -> <<"~", Device/binary>>.

device_group([]) -> <<"unknown">>;
device_group([Module | _]) -> maps:get(<<"group">>, Module).

group_nodes(Modules) ->
    Keys = lists:usort([{maps:get(<<"role">>, M), maps:get(<<"group">>, M)} || M <- Modules]),
    [
        group_node(Role, Group, [M || M <- Modules,
            maps:get(<<"role">>, M) =:= Role,
            maps:get(<<"group">>, M) =:= Group])
    || {Role, Group} <- Keys ].

group_node(Role, Group, Modules) ->
    #{
        <<"id">> => <<Role/binary, ":", Group/binary>>,
        <<"role">> => Role,
        <<"group">> => Group,
        <<"label">> => group_label(Role, Group),
        <<"modules">> => length(Modules),
        <<"functions">> => lists:sum([maps:get(<<"functions">>, M) || M <- Modules])
    }.

group_label(<<"kernel">>, Group) -> <<"kernel/", Group/binary>>;
group_label(<<"device">>, Group) -> <<"devices/", Group/binary>>;
group_label(Role, Group) -> <<Role/binary, "/", Group/binary>>.

role(Root, File) ->
    Rel = relpath(Root, File),
    case lists:prefix("src/core/", Rel) of
        true ->
            <<"kernel">>;
        false ->
            case lists:prefix("src/preloaded/", Rel) of
                true ->
                    <<"device">>;
                false ->
                    case lists:prefix("src/forge/", Rel) of
                        true -> <<"forge">>;
                        false -> <<"other">>
                    end
            end
    end.

group(Root, File, Role) ->
    Rel = relpath(Root, File),
    Parts = filename:split(Rel),
    case {Role, Parts} of
        {<<"kernel">>, ["src", "core", Group | _]} -> unicode:characters_to_binary(Group);
        {<<"device">>, ["src", "preloaded", Group | _]} -> unicode:characters_to_binary(Group);
        {<<"forge">>, _} -> <<"forge">>;
        _ -> <<"other">>
    end.

device_name(Module, Source, Forms, <<"device">>) ->
    case [Device || {attribute, _, implements, Device} <- Forms, is_binary(Device)] of
        [Device | _] ->
            Device;
        [] ->
            case re:run(Source, <<"-define\\(DEVICE,\\s*<<\"([^\"]+@[^\"]+)\"">>,
                    [{capture, [1], binary}]) of
                {match, [Device]} -> Device;
                nomatch -> inferred_device_name(Module)
            end
    end;
device_name(_Module, _Source, _Forms, _Role) ->
    <<>>.

device_refs(Source) ->
    case re:run(
        Source,
        <<"\"([A-Za-z0-9_.-]+@[0-9][A-Za-z0-9_.-]*)\"">>,
        [global, {capture, [1], binary}]
    ) of
        {match, Matches} ->
            lists:usort([Ref || [Ref] <- Matches, byte_size(Ref) =< 80]);
        nomatch ->
            []
    end.

inferred_device_name(Module) ->
    Name = atom_bin(Module),
    case Name of
        <<"dev_", Rest/binary>> ->
            <<(binary:replace(Rest, <<"_">>, <<"-">>, [global]))/binary, "@1.0">>;
        _ ->
            <<"support:", Name/binary>>
    end.

module_doc(Lines) ->
    DocLines = lists:takewhile(
        fun(Line) ->
            Trim = string:trim(binary_to_list(Line)),
            lists:prefix("%%%", Trim) orelse Trim =:= ""
        end,
        Lines
    ),
    clean_doc(DocLines).

function_doc(Line, Lines) ->
    function_doc(Line - 1, Lines, []).

function_doc(0, _Lines, Acc) ->
    clean_doc(Acc);
function_doc(Line, Lines, Acc) ->
    Text = binary_to_list(lists:nth(Line, Lines)),
    Trim = string:trim(Text),
    case lists:prefix("%%", Trim) of
        true ->
            function_doc(Line - 1, Lines, [unicode:characters_to_binary(Trim) | Acc]);
        false ->
            case Trim of
                "" -> function_doc(Line - 1, Lines, Acc);
                _ -> clean_doc(Acc)
            end
    end.

clean_doc(Lines) ->
    Text = string:trim(
        unicode:characters_to_list(
            iolist_to_binary([
                clean_doc_line(Line) || Line <- Lines
            ])
        )
    ),
    unicode:characters_to_binary(Text).

clean_doc_line(Line) ->
    Trim = string:trim(unicode:characters_to_list(Line)),
    Stripped0 =
        case lists:prefix("%%% @doc", Trim) of
            true -> string:trim(string:slice(Trim, 8));
            false ->
                case lists:prefix("%% @doc", Trim) of
                    true -> string:trim(string:slice(Trim, 7));
                    false ->
                        case lists:prefix("%%%", Trim) of
                            true -> string:trim(string:slice(Trim, 3));
                            false ->
                                case lists:prefix("%%", Trim) of
                                    true -> string:trim(string:slice(Trim, 2));
                                    false -> Trim
                                end
                        end
                end
        end,
    [Stripped0, "\n"].

source_excerpt(Line, NextLine, Lines) ->
    LastLine = min(NextLine - 1, min(length(Lines), Line + 18)),
    Width = length(integer_to_list(LastLine)),
    Snippet = [
        [
            io_lib:format("~*.. B", [Width, LineNo]),
            " | ",
            binary_to_list(lists:nth(LineNo, Lines)),
            "\n"
        ]
    || LineNo <- lists:seq(Line, LastLine) ],
    unicode:characters_to_binary(Snippet).

function_id(Module, Name, Arity) ->
    <<(atom_bin(Module))/binary, ":", (atom_bin(Name))/binary,
        "/", (int_bin(Arity))/binary>>.

function_label(Fun) ->
    <<(atom_bin(maps:get(name, Fun)))/binary, "/",
        (int_bin(maps:get(arity, Fun)))/binary>>.

atom_bin(Atom) ->
    unicode:characters_to_binary(atom_to_list(Atom)).

int_bin(Int) ->
    erlang:integer_to_binary(Int).

relpath(Root, File) ->
    RootAbs = filename:absname(Root),
    FileAbs = filename:absname(File),
    RootLen = length(RootAbs),
    case lists:prefix(RootAbs, FileAbs) of
        true ->
            string:trim(string:slice(FileAbs, RootLen), leading, "/");
        false ->
            File
    end.

generated_at() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    list_to_binary(
        io_lib:format(
            "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
            [Year, Month, Day, Hour, Minute, Second]
        )
    ).

render_html(Root, Graph) ->
    Dir = filename:join([Root, "scripts", "codeviz"]),
    {ok, Template} = file:read_file(filename:join([Dir, "index.html"])),
    {ok, Css} = file:read_file(filename:join([Dir, "styles.css"])),
    {ok, Js} = file:read_file(filename:join([Dir, "visualizer.js"])),
    Data = base64:encode(iolist_to_binary(json(Graph))),
    Step1 = binary:replace(Template, <<"{{CODEVIZ_CSS}}">>, Css),
    Step2 = binary:replace(Step1, <<"{{CODEVIZ_JS}}">>, Js),
    binary:replace(Step2, <<"{{GRAPH_JSON_BASE64}}">>, Data).

json(Map) when is_map(Map) ->
    Pairs = [
        [json_key(Key), $:, json(Value)]
    || {Key, Value} <- maps:to_list(Map) ],
    [$\{, join(Pairs, $,), $\}];
json(List) when is_list(List) ->
    case string_like(List) of
        true -> quote(unicode:characters_to_binary(List));
        false -> [$[, join([json(Value) || Value <- List], $,), $]]
    end;
json(Binary) when is_binary(Binary) ->
    quote(Binary);
json(true) -> <<"true">>;
json(false) -> <<"false">>;
json(unknown) -> <<"null">>;
json(Atom) when is_atom(Atom) ->
    quote(atom_bin(Atom));
json(Int) when is_integer(Int) ->
    integer_to_list(Int).

json_key(Key) when is_binary(Key) -> quote(Key);
json_key(Key) when is_atom(Key) -> quote(atom_bin(Key)).

string_like([]) -> false;
string_like(List) -> lists:all(fun is_integer/1, List).

quote(Binary) ->
    [$", escape(binary_to_list(Binary)), $"].

escape([]) -> [];
escape([$" | Rest]) -> [$\\, $" | escape(Rest)];
escape([$\\ | Rest]) -> [$\\, $\\ | escape(Rest)];
escape([$\n | Rest]) -> [$\\, $n | escape(Rest)];
escape([$\r | Rest]) -> [$\\, $r | escape(Rest)];
escape([$\t | Rest]) -> [$\\, $t | escape(Rest)];
escape([C | Rest]) when C < 32 ->
    [io_lib:format("\\u~4.16.0B", [C]) | escape(Rest)];
escape([C | Rest]) -> [C | escape(Rest)].

join([], _Sep) -> [];
join([Item], _Sep) -> Item;
join([Item | Rest], Sep) -> [Item, Sep, join(Rest, Sep)].
