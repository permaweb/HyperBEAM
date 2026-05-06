%%% @doc Build flattened BEAM artifacts for multi-module HyperBEAM devices.
-module(hb_device_packager).
-export([package/1, package/2]).
-export([package_devices/0, package_devices/1, package_devices/2]).
-export([source_modules/1, verify/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_SRC_DIR, "src").
-define(DEFAULT_OUT_DIR, "_build/default/packaged-devices").
-define(HASH_CHARS, 20).
-define(BASE32_ALPHABET, "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567").

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
        [] ->
            {error, {root_not_found, Root}};
        [_] ->
            {error, {single_module_device, Root}};
        _ ->
            package(Root, Files, SrcDir, OutDir)
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
                try package(Root, #{ src_dir => SrcDir, out_dir => OutDir }) of
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
            erlang:error({failed_to_package_devices, Failures})
    end.

%% @doc Package and load-check all configured multi-module devices.
verify(Opts) ->
    Results = package_devices(Opts),
    lists:foreach(fun verify_loadable/1, Results),
    Results.

%% @doc Package a set of source files in a device namespace.
package(Root, Files, SrcDir, OutDir) ->
    SourceOutDir = filename:join(OutDir, "src"),
    BeamOutDir = filename:join(OutDir, "ebin"),
    ensure_dir(SourceOutDir),
    ensure_dir(BeamOutDir),
    Generated = generated_module_name(Root, Files),
    [GeneratedSrc] =
        igor:merge(
            Generated,
            [File || {_Mod, File} <- Files],
            igor_options(Root, SrcDir, SourceOutDir)
        ),
    {ok, Generated, Beam, Warnings} = compile_generated(GeneratedSrc),
    ok = maybe_print_warnings(GeneratedSrc, Warnings),
    RootExports = source_exports(root_file(Root, Files)),
    ok = verify_exports(Generated, Beam, RootExports),
    ok = verify_no_internal_remote_calls(
        GeneratedSrc,
        [Mod || {Mod, _File} <- Files]
    ),
    BeamOut = filename:join(BeamOutDir, atom_to_list(Generated) ++ ".beam"),
    ok = file:write_file(BeamOut, Beam),
    #{
        root => Root,
        module => Generated,
        source => GeneratedSrc,
        beam_file => BeamOut,
        beam => Beam,
        exports => RootExports,
        files => Files
    }.

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

%% @doc Return root device modules that have namespace helper modules.
device_roots(Sources, all) ->
    lists:sort(
        [
            Root
        ||
            {Root, _File} <- Sources,
            is_dev_module(Root),
            length(namespace_files(Root, Sources)) > 1
        ]
    );
device_roots(_Sources, Roots) when is_list(Roots) ->
    lists:sort(Roots).

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

%% @doc Return the deterministic generated module name for a device package.
generated_module_name(Root, Files) ->
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
        "_hb_device_" ++ sanitize_module_name(Root) ++ "_" ++ Hash
    ).

%% @doc Return a short base32 hash suitable for a generated module name.
base32_hash(Hash) ->
    lists:sublist(base32_encode(Hash), ?HASH_CHARS).

%% @doc Encode bytes as uppercase, unpadded RFC 4648 base32.
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

%% @doc Convert a source module atom into a safe generated-name component.
sanitize_module_name(Mod) ->
    [
        case is_module_name_char(Char) of
            true -> Char;
            false -> $_
        end
    ||
        Char <- atom_to_list(Mod)
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
            case {erl_syntax:type(Mod), erl_syntax:type(Func)} of
                {atom, atom} ->
                    ModName = erl_syntax:atom_value(Mod),
                    case sets:is_element(ModName, Internal) of
                        true ->
                            [{ModName, erl_syntax:atom_value(Func)} | Acc];
                        false ->
                            Acc
                    end;
                _ ->
                    Acc
            end;
        _ ->
            Acc
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
        source := Source
    } = Res,
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

fixture_src_dir() ->
    case filelib:is_dir("fixtures/example/src") of
        true -> "fixtures/example/src";
        false -> "apps/hb_device/fixtures/example/src"
    end.
-endif.
