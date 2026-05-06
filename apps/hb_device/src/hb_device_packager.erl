%%% @doc Build flattened BEAM artifacts for multi-module HyperBEAM devices.
-module(hb_device_packager).
-export([package/1, package/2]).
-export([package_devices/0, package_devices/1, package_devices/2]).
-export([discover_devices/1, source_modules/1, verify/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_SRC_DIR, "src").
-define(DEFAULT_OUT_DIR, "_build/default/packaged-devices").
-define(HASH_CHARS, 20).
-define(BASE32_ALPHABET, "abcdefghijklmnopqrstuvwxyz234567").

%% @doc Package a root device module from the default source directory.
package(Root) ->
    package(Root, #{}).

%% @doc Package a root device module and return its generated BEAM artifact.
package(Root, Opts) when is_atom(Root) ->
    SrcDir = maps:get(src_dir, Opts, ?DEFAULT_SRC_DIR),
    OutDir = maps:get(out_dir, Opts, ?DEFAULT_OUT_DIR),
    Sources = source_modules(SrcDir),
    Files = namespace_files(Root, Sources),
    case Files of
        [] -> {error, {root_not_found, Root}};
        _ -> package(Root, Files, SrcDir, OutDir, Opts)
    end.

%% @doc Package all multi-module devices from the default source directory.
package_devices() ->
    package_devices(#{}).

%% @doc Package all multi-module devices in `SrcDir' into `OutDir'.
package_devices(SrcDir, OutDir) ->
    package_devices(#{ src_dir => SrcDir, out_dir => OutDir }).

%% @doc Package all configured multi-module devices.
package_devices(Opts) ->
    SrcDir = maps:get(src_dir, Opts, ?DEFAULT_SRC_DIR),
    OutDir = maps:get(out_dir, Opts, ?DEFAULT_OUT_DIR),
    Sources = source_modules(SrcDir),
    Roots = device_roots(Sources, maps:get(roots, Opts, all)),
    Results =
        lists:map(
            fun(Root) ->
                try package(
                    Root,
                    Opts#{ src_dir => SrcDir, out_dir => OutDir }
                ) of
                    {error, Reason} ->
                        {error, Root, Reason};
                    Res ->
                        print_package_result(Res, Opts),
                        {ok, Res}
                catch
                    Class:Reason:Stacktrace ->
                        {error, Root, {Class, Reason, Stacktrace}}
                end
            end,
            Roots
        ),
    Failures =
        lists:filter(
            fun
                ({error, _, _}) -> true;
                (_) -> false
            end,
            Results
        ),
    case Failures of
        [] ->
            maybe_print_summary(length(Results), Opts),
            [Res || {ok, Res} <- Results];
        _ ->
            print_package_failures(Failures, Opts),
            erlang:error(
                {failed_to_package_devices, simplify_failures(Failures)}
            )
    end.

%% @doc Package and load-check all configured multi-module devices.
verify(Opts) ->
    Results = package_devices(Opts),
    lists:foreach(fun verify_loadable/1, Results),
    Results.

%% @doc Package a set of source files in a device namespace.
package(Root, Files, SrcDir, OutDir, Opts) ->
    SourceOutDir = filename:join(OutDir, "src"),
    BeamOutDir = filename:join(OutDir, "ebin"),
    PreparedDir = filename:join(OutDir, "prepared"),
    ensure_dir(SourceOutDir),
    ensure_dir(BeamOutDir),
    ensure_dir(PreparedDir),
    MergeFiles = mergeable_files(Root, Files),
    RootFile = root_file(Root, MergeFiles),
    Generated = generated_module_name(Root, RootFile, MergeFiles),
    OnLoads = source_on_loads(MergeFiles),
    PreparedFiles = prepare_source_files(MergeFiles, PreparedDir),
    [GeneratedSrc] =
        igor:merge(
            Generated,
            [File || {_Mod, File} <- PreparedFiles],
            igor_options(Root, SrcDir, SourceOutDir)
        ),
    ok = clean_generated_source(GeneratedSrc),
    ok = add_combined_on_load(GeneratedSrc, OnLoads),
    {ok, Generated, Beam, Warnings} = compile_generated(GeneratedSrc),
    ok = maybe_print_warnings(GeneratedSrc, Warnings),
    RootExports = source_exports(RootFile),
    case verify_generated(Generated, Beam, RootExports, GeneratedSrc, MergeFiles) of
        ok ->
            BeamOut =
                filename:join(BeamOutDir, atom_to_list(Generated) ++ ".beam"),
            ok = file:write_file(BeamOut, Beam),
            #{
                root => Root,
                name => device_name(Root, RootFile),
                implements => device_implements(Root, RootFile),
                module => Generated,
                source => GeneratedSrc,
                beam_file => BeamOut,
                beam => Beam,
                exports => RootExports,
                files => MergeFiles,
                spec => device_spec(Root, RootFile, Opts)
            };
        Error ->
            Error
    end.

%% @doc Return source modules in the given directory.
source_modules(SrcDir) ->
    lists:sort(
        lists:filtermap(
            fun(File) ->
                case source_module(File) of
                    {ok, Module} -> {true, {Module, File}};
                    error -> false
                end
            end,
            filelib:wildcard(filename:join(SrcDir, "*.erl"))
        )
    ).

%% @doc Discover source device roots and names in `SrcDir'.
discover_devices(SrcDir) ->
    Sources = source_modules(SrcDir),
    [
        #{
            name => device_name(Root, File),
            root => Root,
            files => namespace_files(Root, Sources)
        }
    ||
        {Root, File} <- Sources,
        is_device_root(Root, Sources)
    ].

%% @doc Return root device modules that have namespace helper modules.
device_roots(Sources, all) ->
    [Root || #{ root := Root } <- discover_devices_from_sources(Sources)];
device_roots(_Sources, Roots) when is_list(Roots) ->
    lists:sort(Roots).

%% @doc Discover device roots from an already-scanned source set.
discover_devices_from_sources(Sources) ->
    [
        #{
            name => device_name(Root, File),
            root => Root,
            files => namespace_files(Root, Sources)
        }
    ||
        {Root, File} <- Sources,
        is_device_root(Root, Sources)
    ].

%% @doc Return true if `Mod' is the root of a device namespace.
is_device_root(Mod, Sources) ->
    is_dev_module(Mod) andalso
        (
            explicit_device_root(Mod, Sources)
            orelse
            not lists:any(
                fun(Prefix) -> lists:keymember(Prefix, 1, Sources) end,
                source_prefixes(Mod)
            )
        ).

explicit_device_root(Mod, Sources) ->
    case lists:keyfind(Mod, 1, Sources) of
        {Mod, File} -> source_implements(File) =/= error;
        false -> false
    end.

%% @doc Return shorter source module prefixes for `Mod'.
source_prefixes(Mod) ->
    Name = atom_to_list(Mod),
    lists:filtermap(
        fun(Index) ->
            case lists:nth(Index, Name) of
                $_ ->
                    Prefix = string:substr(Name, 1, Index - 1),
                    case Prefix of
                        "dev" -> false;
                        _ -> {true, list_to_atom(Prefix)}
                    end;
                _ ->
                    false
            end
        end,
        lists:seq(1, length(Name))
    ).

%% @doc Return all source files in a root device namespace.
namespace_files(Root, Sources) ->
    Prefix = atom_to_list(Root) ++ "_",
    lists:sort(
        fun({ModA, _}, {ModB, _}) ->
            {namespace_file_order(Root, ModA), atom_to_list(ModA)} =<
                {namespace_file_order(Root, ModB), atom_to_list(ModB)}
        end,
        [
            {Mod, File}
        ||
            {Mod, File} <- Sources,
            Mod =:= Root orelse lists:prefix(Prefix, atom_to_list(Mod))
        ]
    ).

%% @doc Sort the root module before helper modules.
namespace_file_order(Root, Root) -> 0;
namespace_file_order(_Root, _Mod) -> 1.

%% @doc Return namespace files that can be safely flattened into the package.
mergeable_files(Root, Files) ->
    [
        {Mod, File}
    ||
        {Mod, File} <- Files,
        Mod =:= Root orelse not source_loads_nif(File)
    ].

%% @doc NIF helpers bind to their original module name, so leave them external.
source_loads_nif(File) ->
    binary:match(read_file(File), <<"load_nif">>) =/= nomatch.

%% @doc Read the declared module name from an Erlang source file.
source_module(File) ->
    case source_attributes(File) of
        {ok, Attrs} ->
            case [Mod || {module, Mod} <- Attrs] of
                [Mod | _] -> {ok, Mod};
                [] -> error
            end;
        error ->
            error
    end.

%% @doc Return the declared exports from an Erlang source file.
source_exports(File) ->
    {ok, Attrs} = source_attributes(File),
    lists:usort(lists:append([Exports || {export, Exports} <- Attrs])).

%% @doc Return declared on-load callbacks from a source file set.
source_on_loads(Files) ->
    lists:filtermap(
        fun({Mod, File}) ->
            case source_on_load(File) of
                {ok, Fun} -> {true, {Mod, Fun}};
                error -> false
            end
        end,
        Files
    ).

source_on_load(File) ->
    case source_attributes(File) of
        {ok, Attrs} ->
            case [OnLoad || {on_load, OnLoad} <- Attrs] of
                [{Fun, 0} | _] -> {ok, Fun};
                _ -> error
            end;
        error ->
            error
    end.

%% @doc Return the Erlang source attributes that can be read without compiling.
source_attributes(File) ->
    case epp_dodger:parse_file(File) of
        {ok, Forms} ->
            {ok,
                lists:filtermap(
                    fun(Form) ->
                        case erl_syntax:type(Form) of
                            attribute ->
                                {true, erl_syntax_lib:analyze_attribute(Form)};
                            _ ->
                                false
                        end
                    end,
                    Forms
                )
            };
        _ ->
            error
    end.

%% @doc Return the root module source file from a namespace file list.
root_file(Root, Files) ->
    {Root, File} = lists:keyfind(Root, 1, Files),
    File.

%% @doc Return true if a module follows the local `dev_' naming convention.
is_dev_module(Mod) ->
    lists:prefix("dev_", atom_to_list(Mod)).

%% @doc Return the public device name for a root source module.
device_name(Root, File) ->
    case source_implements(File) of
        {ok, ID} when is_binary(ID), byte_size(ID) == 43 ->
            derived_device_name(Root);
        {ok, Name} when is_binary(Name) ->
            Name;
        error ->
            derived_device_name(Root)
    end.

%% @doc Return the implemented device name or spec ID for a root source module.
device_implements(Root, File) ->
    case source_implements(File) of
        {ok, Implements} -> Implements;
        error -> derived_device_name(Root)
    end.

%% @doc Return the explicit `implements' attribute, if present.
source_implements(File) ->
    case source_attributes(File) of
        {ok, Attrs} ->
            case [Name || {implements, Name} <- Attrs] of
                [Name | _] when is_binary(Name) -> {ok, Name};
                _ -> error
            end;
        error ->
            error
    end.

%% @doc Derive a device name from its root source module.
derived_device_name(Root) ->
    Name0 = atom_to_list(Root),
    Name1 = string:prefix(Name0, "dev_"),
    iolist_to_binary([
        string:replace(Name1, "_", "-", all),
        "@1.0"
    ]).

%% @doc Return the markdown device specification for a root source module.
device_spec(Root, File, Opts) ->
    case spec_file(Root, Opts) of
        undefined ->
            #{
                <<"content-type">> => <<"text/markdown">>,
                <<"body">> => module_doc_spec(Root, File)
            };
        SpecFile ->
            #{
                <<"content-type">> => spec_content_type(SpecFile),
                <<"body">> => read_file(SpecFile)
            }
    end.

%% @doc Return an explicit spec file for a root if one was configured.
spec_file(Root, Opts) ->
    Specs = specs_map(maps:get(specs, Opts, #{})),
    case maps:find(Root, Specs) of
        {ok, File} ->
            File;
        error ->
            spec_file_for_root(
                Root,
                maps:get(spec, Opts, undefined),
                maps:get(roots, Opts, [Root])
            )
    end.

spec_file_for_root(_Root, undefined, _Roots) ->
    undefined;
spec_file_for_root(_Root, _Spec, all) ->
    undefined;
spec_file_for_root(Root, Spec, [Root]) ->
    Spec;
spec_file_for_root(_Root, _Spec, _Roots) ->
    undefined.

specs_map(Specs) when is_map(Specs) ->
    Specs;
specs_map(Specs) when is_list(Specs) ->
    maps:from_list(Specs).

%% @doc Infer a text content type for an explicit spec file.
spec_content_type(File) ->
    case filename:extension(File) of
        ".html" -> <<"text/html">>;
        ".htm" -> <<"text/html">>;
        _ -> <<"text/markdown">>
    end.

%% @doc Convert the top module `@doc' comment to a markdown spec body.
module_doc_spec(Root, File) ->
    Doc = module_doc(File),
    Title = iolist_to_binary(["# ", device_name(Root, File), "\n\n"]),
    case Doc of
        <<>> ->
            iolist_to_binary([
                Title,
                "Generated from `",
                atom_to_binary(Root),
                "`."
            ]);
        _ -> <<Title/binary, Doc/binary>>
    end.

%% @doc Extract the top module documentation comments from a source file.
module_doc(File) ->
    {ok, Bin} = file:read_file(File),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    iolist_to_binary(module_doc_lines(Lines, seek, [])).

module_doc_lines([], _Mode, Acc) ->
    lists:join(<<"\n">>, lists:reverse(Acc));
module_doc_lines([Line | Lines], seek, Acc) ->
    case first_doc_line(Line) of
        {ok, DocLine} ->
            module_doc_lines(Lines, collect, [DocLine | Acc]);
        skip ->
            module_doc_lines(Lines, seek, Acc);
        stop ->
            []
    end;
module_doc_lines([Line | Lines], collect, Acc) ->
    case module_doc_line(Line) of
        {ok, DocLine} ->
            module_doc_lines(Lines, collect, [DocLine | Acc]);
        skip ->
            module_doc_lines(Lines, collect, Acc);
        stop ->
            lists:join(<<"\n">>, lists:reverse(Acc))
    end.

first_doc_line(Line) ->
    case module_doc_line(Line) of
        {ok, _} = DocLine -> DocLine;
        skip -> skip;
        stop -> top_source_line(Line)
    end.

module_doc_line(<<"%%% @doc ", Rest/binary>>) -> {ok, Rest};
module_doc_line(<<"%%% @doc", Rest/binary>>) -> {ok, string:trim(Rest)};
module_doc_line(<<"%%% ", Rest/binary>>) -> {ok, Rest};
module_doc_line(<<"%%%", Rest/binary>>) -> {ok, string:trim(Rest)};
module_doc_line(<<"%%", _/binary>>) -> skip;
module_doc_line(<<>>) -> skip;
module_doc_line(_) -> stop.

top_source_line(<<"-", _/binary>>) -> skip;
top_source_line(<<" ", Rest/binary>>) -> top_source_line(Rest);
top_source_line(<<"\t", Rest/binary>>) -> top_source_line(Rest);
top_source_line(_) -> stop.

%% @doc Return the deterministic generated module name for a device package.
generated_module_name(Root, RootFile, Files) ->
    Hash =
        base32_hash(
            crypto:hash(
                sha256,
                term_to_binary(
                    {
                        Root,
                        [
                            {Mod, read_file(File)}
                        ||
                            {Mod, File} <- Files
                        ]
                    }
                )
            )
        ),
    list_to_atom(
        "_hb_device_" ++ sanitize_device_name(device_name(Root, RootFile)) ++
            "_" ++ Hash
    ).

%% @doc Return a short base32 hash suitable for a generated module name.
base32_hash(Hash) ->
    lists:sublist(base32_encode(Hash), ?HASH_CHARS).

%% @doc Encode bytes as lowercase, unpadded RFC 4648 base32.
base32_encode(Bin) ->
    base32_encode(Bin, 0, 0, []).

base32_encode(<<Byte, Rest/binary>>, Buffer, Bits, Acc) ->
    emit_base32(Rest, (Buffer bsl 8) bor Byte, Bits + 8, Acc);
base32_encode(<<>>, _Buffer, 0, Acc) ->
    lists:reverse(Acc);
base32_encode(<<>>, Buffer, Bits, Acc) ->
    Index = (Buffer bsl (5 - Bits)) band 31,
    lists:reverse([base32_char(Index) | Acc]).

emit_base32(Rest, Buffer, Bits, Acc) when Bits >= 5 ->
    Shift = Bits - 5,
    Index = (Buffer bsr Shift) band 31,
    Buffer1 =
        case Shift of
            0 -> 0;
            _ -> Buffer band ((1 bsl Shift) - 1)
        end,
    emit_base32(Rest, Buffer1, Shift, [base32_char(Index) | Acc]);
emit_base32(Rest, Buffer, Bits, Acc) ->
    base32_encode(Rest, Buffer, Bits, Acc).

base32_char(Index) ->
    lists:nth(Index + 1, ?BASE32_ALPHABET).

%% @doc Convert a device name into a safe generated-name component.
sanitize_device_name(Name) when is_binary(Name) ->
    sanitize_device_name(binary_to_list(Name));
sanitize_device_name(Name) ->
    [
        case is_module_name_char(Char) of
            true -> Char;
            false -> $_
        end
    ||
        Char <- Name
    ].

%% @doc Return true if a character is safe in a generated module name component.
is_module_name_char(Char) when Char >= $a, Char =< $z -> true;
is_module_name_char(Char) when Char >= $A, Char =< $Z -> true;
is_module_name_char(Char) when Char >= $0, Char =< $9 -> true;
is_module_name_char($_) -> true;
is_module_name_char(_) -> false.

%% @doc Read a source file as bytes for deterministic hashing.
read_file(File) ->
    {ok, Bin} = file:read_file(File),
    Bin.

%% @doc Copy package sources with packager-only attributes removed.
prepare_source_files(Files, Dir) ->
    Internal = sets:from_list([Mod || {Mod, _File} <- Files]),
    [
        begin
            Out = filename:join(Dir, filename:basename(File)),
            {ok, Bin} = file:read_file(File),
            ok =
                file:write_file(
                    Out,
                    prepare_source(Mod, File, Bin, Internal)
                ),
            {Mod, Out}
        end
    ||
        {Mod, File} <- Files
    ].

prepare_source(Mod, File, Bin, Internal) ->
    Source0 = strip_packager_attributes(Bin),
    {Source, CaptureWrappers} =
        rewrite_internal_function_captures(Source0, Mod, Internal),
    case source_on_load(File) of
        {ok, Fun} ->
            iolist_to_binary([
                Source,
                CaptureWrappers,
                "\n",
                on_load_wrapper_function(Mod, Fun)
            ]);
        error ->
            iolist_to_binary([Source, CaptureWrappers])
    end.

%% @doc Wrap internal remote fun captures so Igor can localize their calls.
rewrite_internal_function_captures(Bin, Mod, Internal) ->
    {Source, Captures} =
        rewrite_internal_function_captures(Bin, Mod, Internal, []),
    {
        Source,
        function_capture_wrappers(
            Mod,
            lists:usort(Captures)
        )
    }.

rewrite_internal_function_captures(Bin, Mod, Internal, Captures) ->
    Pattern =
        <<"fun\\s+([a-z][a-zA-Z0-9_@]*)\\s*:\\s*"
            "([a-z][a-zA-Z0-9_@]*)\\s*/\\s*([0-9]+)">>,
    case re:run(Bin, Pattern, [{capture, [0, 1, 2, 3], index}]) of
        {match, [{Start, Len}, ModPos, FunPos, ArityPos]} ->
            Before = binary:part(Bin, 0, Start),
            Match = binary:part(Bin, Start, Len),
            RestStart = Start + Len,
            Rest = binary:part(Bin, RestStart, byte_size(Bin) - RestStart),
            TargetMod = binary_to_atom(capture_binary_part(Bin, ModPos), utf8),
            TargetFun = binary_to_atom(capture_binary_part(Bin, FunPos), utf8),
            Arity = binary_to_integer(capture_binary_part(Bin, ArityPos)),
            case sets:is_element(TargetMod, Internal) of
                true ->
                    Capture = {TargetMod, TargetFun, Arity},
                    Replacement =
                        io_lib:format(
                            "fun ~p/~B",
                            [function_capture_wrapper(Mod, Capture), Arity]
                        ),
                    {Rest1, Captures1} =
                        rewrite_internal_function_captures(
                            Rest,
                            Mod,
                            Internal,
                            [Capture | Captures]
                        ),
                    {iolist_to_binary([Before, Replacement, Rest1]), Captures1};
                false ->
                    {Rest1, Captures1} =
                        rewrite_internal_function_captures(
                            Rest,
                            Mod,
                            Internal,
                            Captures
                        ),
                    {iolist_to_binary([Before, Match, Rest1]), Captures1}
            end;
        nomatch ->
            {Bin, Captures}
    end.

capture_binary_part(Bin, {Start, Len}) ->
    binary:part(Bin, Start, Len).

function_capture_wrappers(_Mod, []) ->
    <<>>;
function_capture_wrappers(Mod, Captures) ->
    [
        "\n",
        [
            function_capture_wrapper_function(Mod, Capture)
        ||
            Capture <- Captures
        ]
    ].

function_capture_wrapper_function(Mod, {TargetMod, TargetFun, Arity}) ->
    Args = function_capture_args(Arity),
    ArgsList = lists:flatten(lists:join(", ", Args)),
    io_lib:format(
        "~p(~s) ->~n    ~p:~p(~s).~n",
        [
            function_capture_wrapper(
                Mod,
                {TargetMod, TargetFun, Arity}
            ),
            ArgsList,
            TargetMod,
            TargetFun,
            ArgsList
        ]
    ).

function_capture_args(0) ->
    [];
function_capture_args(Arity) ->
    [
        "A" ++ integer_to_list(Index)
    ||
        Index <- lists:seq(1, Arity)
    ].

function_capture_wrapper(Mod, {TargetMod, TargetFun, Arity}) ->
    list_to_atom(
        lists:flatten(
            io_lib:format(
                "__hb_device_fun_~s__~s__~s__~B__",
                [
                    atom_to_list(Mod),
                    atom_to_list(TargetMod),
                    atom_to_list(TargetFun),
                    Arity
                ]
            )
        )
    ).

%% @doc Remove source attributes that only make sense before packaging.
strip_packager_attributes(Bin) ->
    lists:foldl(
        fun(Pattern, Acc) ->
            re:replace(Acc, Pattern, <<>>, [global, {return, binary}])
        end,
        Bin,
        [
            <<"(?m)^-implements\\([^\\n]*\\)\\.\\n">>,
            <<"(?m)^-on_load\\([^\\n]*\\)\\.\\n">>
        ]
    ).

on_load_wrapper_function(Mod, Fun) ->
    io_lib:format(
        "~p() ->~n    ~p().~n",
        [on_load_wrapper(Mod), Fun]
    ).

on_load_wrapper(Mod) ->
    list_to_atom("__hb_device_on_load_" ++ atom_to_list(Mod) ++ "__").

%% @doc Clean generated deployment source before compiling it.
clean_generated_source(File) ->
    {ok, Bin} = file:read_file(File),
    WithoutTransforms =
        lists:foldl(
            fun(Transform, Acc) ->
                binary:replace(
                    Acc,
                    iolist_to_binary([
                        "-compile({parse_transform, ",
                        atom_to_binary(Transform),
                        "}).\n\n"
                    ]),
                    <<>>,
                    [global]
                )
            end,
            Bin,
            [eunit_autoexport, hb_test_parallel]
        ),
    file:write_file(File, fix_igor_comments(WithoutTransforms)).

%% @doc Add one generated on-load callback that executes merged callbacks.
add_combined_on_load(_File, []) ->
    ok;
add_combined_on_load(File, OnLoads) ->
    {ok, Bin} = file:read_file(File),
    file:write_file(
        File,
        iolist_to_binary([
            add_on_load_attribute(Bin),
            "\n",
            combined_on_load_functions(OnLoads)
        ])
    ).

add_on_load_attribute(Bin) ->
    re:replace(
        Bin,
        <<"(?m)^-module\\(([^)]*)\\)\\.\\n">>,
        <<"-module(\\1).\n-on_load('__hb_device_on_load__'/0).\n">>,
        [{return, binary}]
    ).

combined_on_load_functions(OnLoads) ->
    [
        "\n'__hb_device_on_load__'() ->\n",
        "    '__hb_device_run_on_load__'([\n",
        lists:join(",\n", [on_load_fun(Mod) || {Mod, _Fun} <- OnLoads]),
        "\n    ]).\n\n",
        "'__hb_device_run_on_load__'([]) -> ok;\n",
        "'__hb_device_run_on_load__'([Fun | Rest]) ->\n",
        "    case Fun() of\n",
        "        ok -> '__hb_device_run_on_load__'(Rest);\n",
        "        Other -> Other\n",
        "    end.\n"
    ].

on_load_fun(Mod) ->
    io_lib:format("        fun ~p/0", [on_load_wrapper(Mod)]).

%% @doc Normalize Igor's old-source comment markers.
fix_igor_comments(Bin) ->
    {Lines, _State} =
        lists:mapfoldl(
            fun fix_igor_comment_line/2,
            #{ mode => keep, records => sets:new() },
            binary:split(Bin, <<"\n">>, [global])
        ),
    iolist_to_binary(lists:join(<<"\n">>, Lines)).

fix_igor_comment_line(Line, State = #{ mode := drop }) ->
    {comment_line(Line), next_igor_state(Line, State)};
fix_igor_comment_line(Line, State = #{ mode := restore_record }) ->
    Restored = uncomment_igor_line(Line),
    {Restored, next_igor_state(Restored, State)};
fix_igor_comment_line(Line, State = #{ mode := keep, records := Records }) ->
    case active_record_name(Line) of
        {ok, Name} ->
            {Line, State#{ records => sets:add_element(Name, Records) }};
        error ->
            case strip_igor_markers(Line) of
                Line ->
                    {Line, State};
                Rest ->
                    fix_igor_marked_line(Line, Rest, State)
            end
    end.

fix_igor_marked_line(Line, <<"-record", _/binary>> = Rest, State) ->
    Name = record_name(Rest),
    case sets:is_element(Name, maps:get(records, State)) of
        true ->
            {comment_line(Line), next_igor_state(Line, State#{ mode => drop })};
        false ->
            {Rest,
                next_igor_state(
                    Rest,
                    State#{
                        mode => restore_record,
                        records => sets:add_element(Name, maps:get(records, State))
                    }
                )}
    end;
fix_igor_marked_line(Line, <<"-", _/binary>>, State) ->
    {comment_line(Line), next_igor_state(Line, State#{ mode => drop })};
fix_igor_marked_line(_Line, Rest, State) ->
    {Rest, State}.

next_igor_state(Line, State = #{ mode := Mode }) ->
    case form_ends(Line) of
        true -> State#{ mode => keep };
        false -> State#{ mode => Mode }
    end.

strip_igor_markers(<<"%%<<< ", Rest/binary>>) ->
    strip_igor_markers(Rest);
strip_igor_markers(Line) ->
    Line.

uncomment_igor_line(<<"%%<<< ", Rest/binary>>) ->
    uncomment_igor_line(Rest);
uncomment_igor_line(<<"%%", Rest/binary>>) ->
    uncomment_igor_line(Rest);
uncomment_igor_line(Line) ->
    Line.

comment_line(<<"%%", _/binary>> = Line) ->
    Line;
comment_line(Line) ->
    <<"%%", Line/binary>>.

active_record_name(Line) ->
    case re:run(Line, <<"^\\s*-record\\(([a-zA-Z0-9_]+)">>, [
        {capture, [1], binary}
    ]) of
        {match, [Name]} -> {ok, binary_to_atom(Name)};
        nomatch -> error
    end.

record_name(Line) ->
    {ok, Name} = active_record_name(Line),
    Name.

form_ends(Line) ->
    case string:trim(binary_to_list(Line), trailing) of
        [] -> false;
        Trimmed -> lists:last(Trimmed) =:= $.
    end.

%% @doc Build the Igor merge options for a device namespace.
igor_options(Root, SrcDir, OutDir) ->
    [
        {dir, OutDir},
        {export, [Root]},
        {stubs, false},
        {preprocess, true},
        {comments, false},
        {notes, no},
        no_headers,
        no_banner,
        {tidy, false},
        {includes, [SrcDir, filename:join(SrcDir, "include")]}
    ].

%% @doc Compile a generated device source file to an in-memory BEAM.
compile_generated(Source) ->
    CompileOpts =
        [
            binary,
            debug_info,
            return_errors,
            return_warnings,
            nowarn_unused_function,
            nowarn_unused_record,
            nowarn_unused_vars
        ],
    case compile:file(Source, CompileOpts) of
        {ok, Mod, Beam} ->
            {ok, Mod, Beam, []};
        {ok, Mod, Beam, Warnings} ->
            {ok, Mod, Beam, Warnings};
        {error, Errors, Warnings} ->
            {error, {compile_failed, Errors, Warnings}};
        error ->
            {error, compile_failed}
    end.

%% @doc Print generated-source warnings, if compilation produced any.
maybe_print_warnings(_Source, []) ->
    ok;
maybe_print_warnings(Source, Warnings) ->
    io:format(
        standard_error,
        "Warnings compiling generated device source ~s: ~p~n",
        [Source, Warnings]
    ).

%% @doc Verify that only root exports are visible from the generated BEAM.
verify_exports(Module, Beam, RootExports) ->
    Expected = lists:usort([{module_info, 0}, {module_info, 1} | RootExports]),
    {ok, {Module, [{exports, Actual}]}} =
        beam_lib:chunks(Beam, [exports]),
    case lists:sort(Actual) =:= lists:sort(Expected) of
        true -> ok;
        false -> {error, {unexpected_exports, Actual, Expected}}
    end.

%% @doc Verify all generated package invariants before writing the BEAM.
verify_generated(Generated, Beam, RootExports, GeneratedSrc, MergeFiles) ->
    case verify_exports(Generated, Beam, RootExports) of
        ok ->
            verify_no_internal_remote_calls(
                GeneratedSrc,
                [Mod || {Mod, _File} <- MergeFiles]
            );
        Error ->
            Error
    end.

%% @doc Verify Igor rewrote all internal remote calls in generated source.
verify_no_internal_remote_calls(Source, InternalModules) ->
    Internal = sets:from_list(InternalModules),
    {ok, Forms} = epp_dodger:parse_file(Source),
    Calls =
        lists:usort(
            lists:append(
                [
                    erl_syntax_lib:fold(
                        fun(Node, Acc) ->
                            find_internal_remote_call(Node, Internal, Acc)
                        end,
                        [],
                        Form
                    )
                ||
                    Form <- Forms
                ]
            )
        ),
    case Calls of
        [] -> ok;
        _ -> {error, {internal_remote_calls, Calls}}
    end.

%% @doc Accumulate internal remote calls found in an Erlang syntax tree node.
find_internal_remote_call(Node, Internal, Acc) ->
    case erl_syntax:type(Node) of
        application ->
            find_internal_remote_call(
                erl_syntax:application_operator(Node),
                Internal,
                Acc
            );
        module_qualifier ->
            Mod = erl_syntax:module_qualifier_argument(Node),
            Func = erl_syntax:module_qualifier_body(Node),
            case erl_syntax:type(Mod) of
                atom ->
                    ModName = erl_syntax:atom_value(Mod),
                    case sets:is_element(ModName, Internal) of
                        true ->
                            [{ModName, remote_call_name(Func)} | Acc];
                        false ->
                            Acc
                    end;
                _ ->
                    Acc
            end;
        _ ->
            Acc
    end.

%% @doc Return a compact function identifier for an internal remote call.
remote_call_name(Func) ->
    case erl_syntax:type(Func) of
        atom -> erl_syntax:atom_value(Func);
        arity_qualifier -> remote_call_capture(Func);
        variable -> {dynamic, erl_syntax:variable_name(Func)};
        _ -> dynamic
    end.

remote_call_capture(Func) ->
    Name = erl_syntax:arity_qualifier_body(Func),
    Arity = erl_syntax:arity_qualifier_argument(Func),
    case {erl_syntax:type(Name), erl_syntax:type(Arity)} of
        {atom, integer} ->
            {function_capture,
                erl_syntax:atom_value(Name),
                erl_syntax:integer_value(Arity)};
        _ ->
            {function_capture, dynamic}
    end.

%% @doc Verify a generated package BEAM can be loaded.
verify_loadable(#{ module := Module, beam := Beam }) ->
    code:purge(Module),
    code:delete(Module),
    case code:load_binary(Module, atom_to_list(Module) ++ ".beam", Beam) of
        {module, Module} ->
            code:purge(Module),
            code:delete(Module),
            ok;
        Error ->
            erlang:error({generated_beam_load_failed, Module, Error})
    end.

%% @doc Ensure a target artifact directory exists.
ensure_dir(Dir) ->
    ok = filelib:ensure_dir(filename:join(Dir, ".keep")).

%% @doc Print a compact package result line for the rebar3 provider.
print_package_result(
    #{ root := Root, module := Module, files := Files, beam_file := BeamFile },
    Opts
) ->
    case maps:get(print, Opts, true) of
        true ->
            io:format(
                user,
                "Packaged ~p -> ~p (~p files): ~s~n",
                [Root, Module, length(Files), BeamFile]
            );
        false ->
            ok
    end.

%% @doc Print a compact package count summary.
maybe_print_summary(Count, Opts) ->
    case maps:get(print, Opts, true) of
        true -> io:format(user, "Packaged ~p device namespace(s).~n", [Count]);
        false -> ok
    end.

%% @doc Print compact expected packaging failures for the operator.
print_package_failures(Failures, Opts) ->
    case maps:get(print, Opts, true) of
        true -> lists:foreach(fun print_package_failure/1, Failures);
        false -> ok
    end.

print_package_failure({error, Root, {internal_remote_calls, Calls}}) ->
    io:format(
        user,
        "Cannot package ~p: unresolved internal remote call(s): ~p~n",
        [Root, Calls]
    );
print_package_failure({error, Root, Reason}) ->
    io:format(user, "Cannot package ~p: ~p~n", [Root, Reason]).

%% @doc Strip stacktraces from the command-level error term.
simplify_failures(Failures) ->
    [simplify_failure(Failure) || Failure <- Failures].

simplify_failure({error, Root, {Class, Reason, _Stacktrace}}) ->
    {Root, {Class, Reason}};
simplify_failure({error, Root, Reason}) ->
    {Root, Reason}.

-ifdef(TEST).
%% @doc Prove a multi-module device packages into one local module.
package_fixture_test() ->
    SrcDir = fixture_src_dir(),
    OutDir = "_build/test/packaged-devices",
    Res = package(dev_example, #{ src_dir => SrcDir, out_dir => OutDir }),
    #{
        module := Module,
        beam := Beam,
        exports := Exports,
        files := Files,
        source := Source,
        implements := Implements,
        spec := Spec
    } = Res,
    ?assertEqual(<<"example@1.0">>, Implements),
    ?assertMatch(
        {_, _},
        binary:match(
            maps:get(<<"body">>, Spec),
            <<"Example root module for HyperBEAM device packaging.">>
        )
    ),
    ?assert(lists:keymember(dev_example_codec, 1, Files)),
    ?assert(lists:keymember(dev_example_state, 1, Files)),
    ?assert(lists:member({ping, 3}, Exports)),
    ?assert(not lists:member({encode, 1}, Exports)),
    ?assert(not lists:member({default, 0}, Exports)),
    ?assertEqual(Module, maps:get(
        module,
        package(dev_example, #{ src_dir => SrcDir, out_dir => OutDir })
    )),
    {ok, GeneratedSource} = file:read_file(Source),
    ?assertEqual(nomatch, binary:match(GeneratedSource, <<"dev_example_codec:">>)),
    code:purge(Module),
    code:delete(Module),
    ?assertEqual(
        {module, Module},
        code:load_binary(Module, atom_to_list(Module) ++ ".beam", Beam)
    ),
    ?assertEqual(
        {ok, <<"example:pong">>},
        Module:ping(#{}, #{}, #{})
    ),
    ?assertEqual(
        {ok, <<"example:capture">>},
        Module:capture(#{}, #{ <<"body">> => <<"capture">> }, #{})
    ),
    code:purge(Module),
    code:delete(Module).

%% @doc Prove an explicit spec file can override the root module documentation.
explicit_spec_file_test() ->
    SrcDir = fixture_src_dir(),
    OutDir = "_build/test/packaged-devices-with-spec",
    SpecFile = "_build/test/hb_device_packager/spec.md",
    ok = filelib:ensure_dir(SpecFile),
    ok = file:write_file(SpecFile, <<"# Explicit Spec\n">>),
    #{ spec := Spec } =
        package(dev_example, #{
            src_dir => SrcDir,
            out_dir => OutDir,
            spec => SpecFile
        }),
    ?assertEqual(<<"text/markdown">>, maps:get(<<"content-type">>, Spec)),
    ?assertEqual(<<"# Explicit Spec\n">>, maps:get(<<"body">>, Spec)).

%% @doc Prove merged on-load callbacks run when the package is loaded.
combined_on_load_test() ->
    SrcDir = "_build/test/hb_device_packager/on_load/src",
    OutDir = "_build/test/hb_device_packager/on_load/out",
    RootKey = {dev_onload, root},
    HelperKey = {dev_onload_helper, helper},
    ok = write_on_load_fixture(SrcDir),
    persistent_term:erase(RootKey),
    persistent_term:erase(HelperKey),
    #{ module := Module, beam := Beam } =
        package(dev_onload, #{ src_dir => SrcDir, out_dir => OutDir }),
    code:purge(Module),
    code:delete(Module),
    ?assertEqual(
        {module, Module},
        code:load_binary(Module, atom_to_list(Module) ++ ".beam", Beam)
    ),
    ?assertEqual(loaded, persistent_term:get(RootKey)),
    ?assertEqual(loaded, persistent_term:get(HelperKey)),
    ?assertEqual({ok, helper}, Module:value(#{}, #{}, #{})),
    persistent_term:erase(RootKey),
    persistent_term:erase(HelperKey),
    code:purge(Module),
    code:delete(Module).

%% @doc Prove verification packages and load-checks configured devices.
verify_fixture_test() ->
    Res =
        verify(#{
            src_dir => fixture_src_dir(),
            out_dir => "_build/test/verified-devices",
            roots => [dev_example],
            print => false
        }),
    ?assertMatch([#{ root := dev_example }], Res).

%% @doc Prove dynamic internal remote calls are rejected, not hidden.
internal_remote_call_detection_test() ->
    Source = "_build/test/hb_device_packager/internal_remote_calls.erl",
    ok = filelib:ensure_dir(Source),
    ok =
        file:write_file(
            Source,
            [
                "-module(internal_remote_calls).\n",
                "run(F) -> dev_example_codec:F([]).\n",
                "run_static() -> dev_example_codec:encode(<<>>).\n",
                "capture() -> fun dev_example_codec:decode/1.\n"
            ]
        ),
    {error, {internal_remote_calls, Calls}} =
        verify_no_internal_remote_calls(Source, [dev_example_codec]),
    ?assert(lists:member({dev_example_codec, {dynamic, 'F'}}, Calls)),
    ?assert(lists:member({dev_example_codec, encode}, Calls)),
    ?assert(lists:member(
        {dev_example_codec, {function_capture, decode, 1}},
        Calls
    )).

fixture_src_dir() ->
    case filelib:is_dir("fixtures/example/src") of
        true -> "fixtures/example/src";
        false -> "apps/hb_device/fixtures/example/src"
    end.

write_on_load_fixture(SrcDir) ->
    Root = filename:join(SrcDir, "dev_onload.erl"),
    Helper = filename:join(SrcDir, "dev_onload_helper.erl"),
    ok = filelib:ensure_dir(Root),
    ok =
        file:write_file(
            Root,
            [
                "%%% @doc On-load package fixture.\n",
                "-module(dev_onload).\n",
                "-implements(<<\"onload@1.0\">>).\n",
                "-on_load(load/0).\n",
                "-export([value/3]).\n",
                "load() ->\n",
                "    persistent_term:put({dev_onload, root}, loaded),\n",
                "    ok.\n",
                "value(_Base, _Req, _Opts) ->\n",
                "    {ok, dev_onload_helper:value()}.\n"
            ]
        ),
    file:write_file(
        Helper,
        [
            "-module(dev_onload_helper).\n",
            "-on_load(load/0).\n",
            "-export([value/0]).\n",
            "load() ->\n",
            "    persistent_term:put({dev_onload_helper, helper}, loaded),\n",
            "    ok.\n",
            "value() -> helper.\n"
        ]
    ).
-endif.
