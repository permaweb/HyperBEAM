%%% @doc HyperBEAM device packager.
%%%
%%% This module turns a namespace of `dev_<name>'/`dev_<name>_*' Erlang
%%% source files into a deterministic archive of debug-info BEAM modules.
%%% Every source module is renamed under a shared `_hb_device_*' root:
%%% the root becomes `_hb_device_<sanitized-name>_<hash>' and helpers
%%% become `_hb_device_<sanitized-name>_<hash>__<helper-tail>'.
%%%
%%% The hash is the unsigned message ID of an AO-Core message that
%%% contains the device's source files (file name and contents).  This
%%% means the hash is uniquely determined by the source set and is not
%%% materially controllable by the device author.  The hash is encoded
%%% as lowercase unpadded base32 so that it can appear in an Erlang
%%% atom.
%%%
%%% The packager also produces signed AO-Core messages for the device's
%%% _specification_ (markdown derived from the root module's top moduledoc,
%%% or a custom file) and its _implementation_ (the BEAM archive).
%%% Both message shapes are described in the device packaging spec.
%%%
%%% Public API:
%%% <ul>
%%%   <li>{@link scan/2}        scan source directories for device groups</li>
%%%   <li>{@link package/2}     package a single device group</li>
%%%   <li>{@link package_all/2} package every device discovered by `scan/2'</li>
%%%   <li>{@link spec_message/2} build the (unsigned) specification message</li>
%%%   <li>{@link impl_message/3} build the (unsigned) implementation message</li>
%%% </ul>
-module(hb_packager).

-export([scan/2, scan/1]).
-export([package/2, package_all/2, group_device_name/1]).
-export([spec_message/2, impl_message/3]).
-export([format_error/1]).

-include("include/hb.hrl").
-include_lib("kernel/include/file.hrl").

-define(VARIANT, <<"ao.N.1">>).
-define(DEFAULT_DEVICE_VERSION, <<"@1.0">>).
-define(ARCHIVE_CONTENT_TYPE, <<"application/beam-archive">>).

%%% --------------------------------------------------------------------
%%% Scanning
%%% --------------------------------------------------------------------

%% @doc Scan one or more source directories and return a list of device
%% groups. Each group has the form
%% ```
%% #{ root := atom(),
%%    root_file := binary(),
%%    helpers := [{atom(), binary()}],
%%    files := #{ binary() => binary() } }
%% ```
%% Files are returned by their bare filename (not their full path) so the
%% hash is independent of the build location.
scan(Dirs) -> scan(Dirs, #{}).

scan(Dirs, Opts) when is_list(Dirs) ->
    Files = lists:flatmap(fun list_dev_files/1, Dirs),
    LibFiles = maps:from_list(lists:flatmap(fun list_lib_files/1, Dirs)),
    Sorted = lists:keysort(1, Files),
    ForcedRoots =
        sets:from_list(
            [N || {N, P} <- Sorted, file_has_implements(P)]
        ),
    Groups = group_by_namespace(Sorted, ForcedRoots, LibFiles),
    DeviceRoots = hb_maps:get(<<"device-roots">>, Opts, all, Opts),
    case DeviceRoots of
        all -> Groups;
        Filter when is_list(Filter) ->
            FilterAtoms = [hb_util:key_to_atom(F, new_atoms) || F <- Filter],
            [G || G = #{ root := R } <- Groups, lists:member(R, FilterAtoms)]
    end;
scan(Dir, Opts) when is_binary(Dir) ->
    scan([Dir], Opts).

%% @doc Recursively list `dev_*.erl' files in a directory.
%% Returns a list of {ModuleNameAtom, FilePathBin} pairs.
list_dev_files(Dir) ->
    list_source_files(Dir, <<"dev">>).

%% @doc Recursively list `lib_*.erl' files in a directory.
list_lib_files(Dir) ->
    list_source_files(Dir, <<"lib">>).

%% @doc Recursively list source files with a given module prefix.
list_source_files(Dir, Prefix) ->
    Bin = hb_util:bin(Dir),
    case filelib:is_dir(Bin) of
        false -> [];
        true ->
            Pattern =
                filename:join(
                    hb_util:list(Bin),
                    ["**/", hb_util:list(Prefix), "_*.erl"]
                ),
            [
                {atom_of_file(P), hb_util:bin(P)}
              ||
                P <- filelib:wildcard(Pattern)
            ]
    end.

%% @doc Convert a filename to an atom.
atom_of_file(Path) ->
    list_to_atom(filename:rootname(filename:basename(Path))).

%% @doc Return true if source declares `-implements(...)'. The check is
%% intentionally light-weight: it only decides whether a module is a root.
file_has_implements(Path) ->
    case file:read_file(hb_util:bin(Path)) of
        {ok, Bin} ->
            nomatch =/= binary:match(Bin, <<"-implements(">>);
        _ ->
            false
    end.

%% @doc Group module files into device packages by namespace prefix.
%% A module `dev_foo_bar' is a helper of `dev_foo' iff `dev_foo' exists
%% in the candidate set AND `dev_foo_bar' itself does not declare a
%% `-implements(...)' attribute. Modules that explicitly declare the
%% device they implement are always roots.
group_by_namespace(Files, ForcedRoots, LibFiles) ->
    ByDepth =
        lists:sort(
            fun({A, _}, {B, _}) -> namespace_key(A) =< namespace_key(B) end,
            Files
        ),
    {Roots, Helpers, _RootSet} =
        lists:foldl(
            fun({Module, _Path}, {RAcc, HAcc, RootSet}) ->
                case sets:is_element(Module, ForcedRoots) of
                    true ->
                        {
                            [Module | RAcc],
                            HAcc,
                            sets:add_element(Module, RootSet)
                        };
                    false ->
                        % If there is no prefix match, add it to the roots list.
                        case longest_root_prefix(Module, RootSet) of
                            Module ->
                                {
                                    [Module | RAcc],
                                    HAcc,
                                    sets:add_element(Module, RootSet)
                                };
                            Root ->
                                {RAcc, [{Root, Module} | HAcc], RootSet}
                        end
                end
            end,
            {[], [], ForcedRoots},
            ByDepth
        ),
    ?event({roots, Roots}),
    ?event({helpers, Helpers}),
    SortedRoots = lists:sort(Roots),
    FilesMap = maps:from_list(Files),
    [
        begin
            RootHelpers =
                lists:sort(
                    [H || {R, H} <- Helpers, R =:= Root]
                ),
            SourceFiles =
                [maps:get(Root, FilesMap)]
                ++ [maps:get(H, FilesMap) || H <- RootHelpers],
            RootLibraries = library_modules(SourceFiles, LibFiles),
            #{
                root => Root,
                root_file => maps:get(Root, FilesMap),
                helpers =>
                    [{H, maps:get(H, FilesMap)} || H <- RootHelpers],
                libraries => RootLibraries,
                files =>
                    maps:from_list(
                        [
                            {filename_only(maps:get(M, FilesMap)),
                                read_file(maps:get(M, FilesMap))}
                          ||
                            M <- [Root | RootHelpers]
                        ] ++
                        [
                            {filename_only(Path), read_file(Path)}
                         || {_Lib, Path} <- RootLibraries
                        ]
                    )
            }
        end
      ||
        Root <- SortedRoots
    ].

%% @doc Return the lib_* modules requested by a device root or helper.
library_modules(SourceFiles, LibFiles) ->
    Libs = lists:usort(lists:flatmap(
        fun(SourceFile) ->
            {_Forms, Attrs} = parse_module(SourceFile),
            lists:flatmap(fun library_attr/1, Attrs)
        end,
        SourceFiles
    )),
    lists:map(
        fun(Mod) ->
            case maps:find(Mod, LibFiles) of
                {ok, Path} -> {Mod, Path};
                error -> erlang:error({missing_device_library, Mod, SourceFiles})
            end
        end,
        Libs
    ).

library_attr({device_libraries, Mods}) ->
    normalize_libraries(Mods);
library_attr(_) ->
    [].

%% @doc Normalize the modules declared by `-device_libraries(...)'.
normalize_libraries(Mods) when is_list(Mods) ->
    lists:flatmap(fun normalize_libraries/1, Mods);
normalize_libraries(Mod) when is_atom(Mod) ->
    case lists:prefix("lib_", atom_to_list(Mod)) of
        true -> [Mod];
        false -> erlang:error({invalid_device_library, Mod})
    end.

%% @doc Sort shorter namespaces before their helpers.
namespace_key(Mod) ->
    case atom_to_list(Mod) of
        "dev_" ++ Tail -> {length(string:split(Tail, "_", all)), Mod};
        _ -> {1, Mod}
    end.

%% @doc Find the longest existing dev_* prefix in `Names' for the given
%% module name. If only the module itself exists in the set, returns it
%% unchanged.
longest_root_prefix(Mod, NameSet) ->
    case atom_to_list(Mod) of
        "dev_" ++ Tail ->
            Parts = string:split(Tail, "_", all),
            longest_root_prefix(Mod, Parts, NameSet);
        _ ->
            Mod
    end.

longest_root_prefix(Mod, Parts, NameSet) ->
    case length(Parts) of
        N when N =< 1 -> Mod;
        _ ->
            Trials = [
                list_to_atom(
                    lists:flatten(
                        ["dev_",
                         lists:join("_", lists:sublist(Parts, K))]
                    ))
              ||
                K <- lists:seq(length(Parts) - 1, 1, -1)
            ],
            case [P || P <- Trials, sets:is_element(P, NameSet)] of
                [] -> Mod;
                [Best | _] -> Best
            end
    end.

%% @doc Return the basename of a path as a binary.
filename_only(Path) ->
    hb_util:bin(filename:basename(hb_util:list(Path))).

%% @doc Read a file as a binary.
read_file(Path) ->
    {ok, Bin} = file:read_file(hb_util:bin(Path)),
    Bin.

%%% --------------------------------------------------------------------
%%% Packaging
%%% --------------------------------------------------------------------

%% @doc Package every device group. A device's identity is the unsigned
%% AO-Core message ID of its source-file message; computing it (and
%% signing the preloaded messages) needs the seed codecs, which the
%% caller makes reachable through the `forge-bootstrap' option (see
%% {@link hb_forge_seed}). The runtime never sets it.
package_all(Groups, Opts) ->
    ?event(packager, {package_groups, {count, length(Groups)}}),
    [package(G, Opts) || G <- Groups].

%% @doc Package one device group. Returns a map containing the generated
%% root module name, BEAM archive, source, declared `implements' name
%% (if present), and metadata used to construct the spec/implementation
%% messages.
package(#{ root := Root, root_file := RootFile, helpers := Helpers,
    files := Files } = Group, Opts) ->
    Libraries = maps:get(libraries, Group, []),
    {_RootForms, RootAttrs} = parse_module(RootFile),
    Implements = derived_or_declared_implements(Root, RootAttrs),
    {SpecBody, SpecContentType} = derive_spec(RootFile, RootAttrs, Opts),
    PrivFiles = priv_files(Root, RootFile),
    SourceID = source_id(maps:merge(Files, PrivFiles), Opts),
    ModName =
        hb_device_name:generated(Implements, source_id_to_hash(SourceID)),
    ?event(
        packager,
        {packaging, {root, Root}, {id, SourceID}, {mod, ModName}}
    ),
    Archive =
        compile_archive(
            ModName, Root, RootFile, Helpers, Libraries, PrivFiles, Opts
        ),
    Pkg = #{
        device_name => Implements,
        source_id => SourceID,
        module_name => ModName,
        archive => Archive,
        spec_body => SpecBody,
        spec_content_type => SpecContentType,
        requires_otp_release =>
            hb_util:bin(erlang:system_info(otp_release))
    },
    case maps:get(<<"requires-system-architecture">>, Opts, false) of
        true ->
            Pkg#{
                requires_system_architecture =>
                    hb_util:bin(erlang:system_info(system_architecture))
            };
        false ->
            Pkg
    end.

%% @doc The device name (`name@version') a scanned group implements,
%% without packaging it. Build tooling uses this to correlate a source
%% group with its package.
group_device_name(#{ root := Root, root_file := RootFile }) ->
    {_Forms, Attrs} = parse_module(RootFile),
    derived_or_declared_implements(Root, Attrs).

%%% Source parsing. We use `epp_dodger' so we do not need include paths
%%% or macro definitions to read a module's attributes (`-export',
%%% `-implements', `-specification', `-device_libraries').
%%% Reference rewriting is done later by the `hb_device_rename'
%%% parse_transform, operating on the compiler's own preprocessed forms.
parse_module(Path) when is_binary(Path) ->
    parse_module(hb_util:list(Path));
parse_module(Path) when is_list(Path) ->
    case epp_dodger:parse_file(Path, []) of
        {ok, Forms} ->
            {Forms, collect_attributes(Forms)};
        {error, Reason} ->
            erlang:error({source_parse_failed, Path, Reason})
    end.

%% Extract the standard Erlang attributes (-export, -implements,
%% -specification, ...) from a list of syntax-tree forms. Forms whose
%% macro shape `erl_syntax_lib:analyze_form/1' cannot understand are
%% silently skipped; the device contract only cares about the small
%% set of attributes consumed below.
%% @doc Collect Erlang attributes from parsed forms.
collect_attributes(Forms) ->
    lists:foldl(
        fun(Form, Acc) ->
            try erl_syntax_lib:analyze_form(Form) of
                {attribute, {Name, Args}} -> [{Name, Args} | Acc];
                _ -> Acc
            catch _:_ -> Acc
            end
        end,
        [],
        Forms
    ).

declared_implements(Attrs) ->
    case lists:keyfind(implements, 1, Attrs) of
        {implements, Bin} when is_binary(Bin) -> Bin;
        {implements, [Bin]} when is_binary(Bin) -> Bin;
        {implements, Str} when is_list(Str) ->
            case io_lib:printable_unicode_list(Str) of
                true -> hb_util:bin(Str);
                false -> undefined
            end;
        _ -> undefined
    end.

derived_or_declared_implements(Root, Attrs) ->
    case declared_implements(Attrs) of
        undefined -> derived_implements(Root);
        Decl when is_binary(Decl) ->
            case ?IS_ID(Decl) of
                true -> Decl;
                false ->
                    % Already a name@version binary. Pass through.
                    Decl
            end
    end.

derived_implements(Root) ->
    <<"dev_", Tail/binary>> = atom_to_binary(Root, utf8),
    Hyphenated = binary:replace(Tail, <<"_">>, <<"-">>, [global]),
    <<Hyphenated/binary, ?DEFAULT_DEVICE_VERSION/binary>>.


%%% Specification body extraction.
derive_spec(RootFile, Attrs, _Opts) ->
    case lists:keyfind(specification, 1, Attrs) of
        {specification, Path} when is_list(Path) orelse is_binary(Path) ->
            ResolvedPath = resolve_spec_path(Path, RootFile),
            {ok, Bin} = file:read_file(hb_util:bin(ResolvedPath)),
            {Bin, content_type_of(ResolvedPath)};
        _ ->
            {extract_moduledoc(RootFile), <<"text/markdown">>}
    end.

resolve_spec_path(Path, RootFile) ->
    PathBin = hb_util:bin(Path),
    case filelib:is_file(PathBin) of
        true -> PathBin;
        false ->
            Dir = filename:dirname(hb_util:list(RootFile)),
            hb_util:bin(filename:join(Dir, hb_util:list(PathBin)))
    end.

content_type_of(Path) ->
    case filename:extension(hb_util:list(Path)) of
        ".html" -> <<"text/html">>;
        ".htm" -> <<"text/html">>;
        _ -> <<"text/markdown">>
    end.

%% @doc Return `priv/...' archive entries for a device package.
priv_files(Root, RootFile) ->
    Dirs = default_priv_dirs(Root, RootFile),
    maps:from_list(lists:append([priv_files_from_dir(Dir) || Dir <- Dirs])).

default_priv_dirs(Root, RootFile) ->
    SourceDir = hb_util:bin(filename:dirname(hb_util:list(RootFile))),
    RootDir = atom_to_binary(Root, utf8),
    RootPrivDirs = [
        filename:join([SourceDir, <<"priv">>, RootDir]),
        filename:join([SourceDir, RootDir, <<"priv">>])
    ],
    case [Dir || Dir <- RootPrivDirs, filelib:is_dir(Dir)] of
        [] -> shared_priv_dirs(SourceDir);
        Dirs -> lists:usort(Dirs)
    end.

shared_priv_dirs(SourceDir) ->
    case hb_util:bin(filename:basename(hb_util:list(SourceDir))) of
        <<"src">> ->
            [
                filename:join(SourceDir, <<"priv">>),
                filename:join(
                    hb_util:bin(filename:dirname(hb_util:list(SourceDir))),
                    <<"priv">>
                )
            ];
        _ ->
            [filename:join(SourceDir, <<"priv">>)]
    end.

priv_files_from_dir(Dir0) ->
    Dir = filename:absname(hb_util:list(Dir0)),
    case filelib:is_dir(Dir) of
        false -> [];
        true ->
            files_from_priv_dir(Dir)
    end.

files_from_priv_dir(Dir) ->
    Files =
        [
            Path
         ||
            Path <- lists:sort(filelib:wildcard(filename:join(Dir, "**/*"))),
            is_regular_file(Path)
        ],
    [
        {hb_util:bin(filename:join(<<"priv">>, relative_path(Dir, Path))),
            read_file(Path)}
     ||
        Path <- Files
    ].

is_regular_file(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular}} -> true;
        _ -> false
    end.

relative_path(Dir, Path) ->
    DirParts = filename:split(filename:absname(Dir)),
    PathParts = filename:split(filename:absname(Path)),
    filename:join(lists:nthtail(length(DirParts), PathParts)).

%% Extract the leading `%%%' moduledoc block.
extract_moduledoc(Path) ->
    {ok, Bin} = file:read_file(hb_util:bin(Path)),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    extract_moduledoc_lines(Lines, []).

extract_moduledoc_lines([], Acc) -> reverse_concat(Acc);
extract_moduledoc_lines([Line | Rest], Acc) ->
    case match_doc_line(Line) of
        {ok, Stripped} ->
            extract_moduledoc_lines(Rest, [Stripped | Acc]);
        skip when Acc =:= [] ->
            % Skip leading lines until we see the first doc line.
            extract_moduledoc_lines(Rest, []);
        _ ->
            reverse_concat(Acc)
    end.

match_doc_line(<<"%%% ", Rest/binary>>) -> {ok, Rest};
match_doc_line(<<"%%%", Rest/binary>>) -> {ok, Rest};
match_doc_line(<<>>) -> skip;
match_doc_line(_) -> stop.

reverse_concat([]) -> <<>>;
reverse_concat(Lines) ->
    Joined = lists:join(<<"\n">>, lists:reverse(Lines)),
    iolist_to_binary(Joined).

%%% --------------------------------------------------------------------
%%% Hashing & module naming
%%% --------------------------------------------------------------------

%% @doc A device's identity is the unsigned AO-Core message ID of the
%% message whose keys are the bare source filenames and whose values
%% are the file contents -- the cryptographic anchor linking the source
%% set to its compiled archive. The seed codecs needed to compute it
%% are supplied by the caller via `forge-bootstrap'.
source_id(FilesMap, Opts) ->
    SourceMsg = maps:from_list(lists:sort(maps:to_list(FilesMap))),
    hb_message:id(SourceMsg, unsigned, Opts).

%% @doc The atom-safe hash embedded in generated module names: the
%% native (32-byte) form of the source ID as lowercase unpadded base32.
source_id_to_hash(SourceID) ->
    base32:encode(hb_util:native_id(SourceID), [lower, nopad]).

%%% --------------------------------------------------------------------
%%% Namespace rename + archive compile
%%% --------------------------------------------------------------------

%% @doc Compile every source module into its generated namespace and pack
%% the resulting BEAMs into a deterministic in-memory ZIP archive.
%%
%% Each module is compiled from its original source with the
%% `hb_device_rename' transform, which rewrites intra-package module atoms
%% on the compiler's preprocessed forms.
compile_archive(
        RootMod, Root, RootFile, Helpers, Libraries, PrivFiles, Opts) ->
    Entries = [{Root, RootFile} | Helpers ++ Libraries],
    RenameMap = maps:from_list(module_renamings(RootMod, Root, Entries)),
    IncludeDirs = include_dirs(Entries),
    Compiled =
        [
            compile_module(Old, Path, RenameMap, IncludeDirs, Opts)
         ||
            {Old, Path} <- Entries
        ],
    hb_device_archive:create(Compiled, PrivFiles).

%% @doc Compile one source module into its generated module name.
compile_module(Old, Path, RenameMap, IncludeDirs, Opts) ->
    New = maps:get(Old, RenameMap),
    CompileOpts =
        [
            binary,
            debug_info,
            return_errors,
            return_warnings,
            nowarn_unused_function,
            nowarn_unused_vars,
            nowarn_shadow_vars,
            nowarn_export_all,
            nowarn_unused_record,
            nowarn_unused_type,
            {parse_transform, hb_device_rename},
            {hb_device_renames, RenameMap}
        ]
        ++ test_compile_opts(Opts)
        ++ [{i, hb_util:list(Dir)} || Dir <- IncludeDirs],
    case compile:file(hb_util:list(Path), CompileOpts) of
        {ok, New, Beam} ->
            #{ module => New, source => Path, beam => Beam };
        {ok, New, Beam, _Warnings} ->
            #{ module => New, source => Path, beam => Beam };
        {ok, Other, _Beam} ->
            erlang:error({device_module_name_mismatch, Old, New, Other});
        {ok, Other, _Beam, _Warnings} ->
            erlang:error({device_module_name_mismatch, Old, New, Other});
        {error, Errors, Warnings} ->
            erlang:error(
                {device_compile_failed, Old, Path, Errors, Warnings}
            );
        Other ->
            erlang:error(
                {device_compile_failed,
                    Old,
                    Path,
                    [{Path, [{none, ?MODULE, Other}]}],
                    []}
            )
    end.

%% @doc Render packager errors for user-facing forge providers.
format_error({device_compile_failed, Device, Path, Errors, Warnings}) ->
    [
        io_lib:format(
            "While building device package ~p from ~ts:~n",
            [Device, hb_util:list(Path)]
        ),
        compile_messages("", Errors),
        compile_messages("Warning: ", Warnings)
    ];
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

compile_messages(Prefix, Messages) ->
    [
        Text
     ||
        {File, Infos} <- Messages,
        {_Where, Text} <-
            sys_messages:format_messages(
                hb_util:list(compile_file(File)),
                Prefix,
                Infos,
                []
            )
    ].

compile_file({Path, _Line}) -> Path;
compile_file(Path) -> Path.

%% @doc Build the old-module -> generated-module mapping.
module_renamings(RootMod, Root, Entries) ->
    [{Mod, generated_constituent_module_name(RootMod, Root, Mod)}
        || {Mod, _Path} <- Entries].

%% @doc Keep the root module at the generated public implementation name.
generated_constituent_module_name(RootMod, Root, Root) ->
    RootMod;
%% @doc Give helpers and lib_* modules private generated names under root.
generated_constituent_module_name(RootMod, Root, Mod) ->
    "dev_" ++ RootTail = atom_to_list(Root),
    RootPrefix = "dev_" ++ RootTail ++ "_",
    ModStr = atom_to_list(Mod),
    Tail =
        case {lists:prefix(RootPrefix, ModStr), lists:prefix("lib_", ModStr)} of
            {true, _} -> lists:nthtail(length(RootPrefix), ModStr);
            {_, true} -> lists:nthtail(length("lib_"), ModStr);
            _ -> ModStr
        end,
    binary_to_atom(
        <<(atom_to_binary(RootMod, utf8))/binary, "__",
            (hb_device_name:sanitize(Tail))/binary>>,
        utf8
    ).

%% @doc Return include dirs needed to compile package sources.
include_dirs(Entries) ->
    lists:usort(
        [
            <<"src">>,
            hb_util:bin(filename:absname("src")),
            <<"src/core">>,
            hb_util:bin(filename:absname("src/core"))
        ]
        ++ [
            hb_util:bin(filename:dirname(hb_util:list(Path)))
         ||
            {_Mod, Path} <- Entries
        ]
        ++ lists:filtermap(
            fun({_Mod, Path}) -> source_core_dir(Path) end,
            Entries
        )
    ).

%% @doc Infer the sibling `src/core' include dir for preloaded source files.
source_core_dir(Source) ->
    Abs = filename:absname(hb_util:list(Source)),
    case string:str(Abs, "/src/preloaded/") of
        0 -> false;
        Pos ->
            {true,
                filename:join(
                    [string:substr(Abs, 1, Pos - 1), <<"src">>, <<"core">>]
                )
            }
    end.

%% @doc Add test-only compile flags when packaging for `device test'.
test_compile_opts(Opts) ->
    case hb_maps:get(<<"test">>, Opts, false, Opts) of
        true -> [{d, 'TEST'}];
        _ -> []
    end.

%%% --------------------------------------------------------------------
%%% AO-Core message construction
%%% --------------------------------------------------------------------

%% @doc Build the (unsigned) device-specification message.
spec_message(
    #{ device_name := Name, spec_body := Body, spec_content_type := CType },
    _Opts
) ->
    #{
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => ?VARIANT,
        <<"type">> => <<"Device-Specification">>,
        <<"name">> => Name,
        <<"content-type">> => CType,
        <<"body">> => Body
    }.

%% @doc Build the (unsigned) device-implementation message. `SpecID' must
%% be the (committed) ID of the specification message that this BEAM
%% archive implements; it is written into the implementation message as
%% the `implements-device' key.
impl_message(Pkg, SpecID, _Opts) ->
    #{
        module_name := ModName,
        archive := Archive,
        requires_otp_release := OtpRel
    } = Pkg,
    % Keep archive bytes and loader metadata flat in the implementation message.
    Msg = #{
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => ?VARIANT,
        <<"content-type">> => ?ARCHIVE_CONTENT_TYPE,
        <<"archive-format">> => <<"zip">>,
        <<"implements-device">> => SpecID,
        <<"module-name">> => atom_to_binary(ModName, utf8),
        <<"requires-otp-release">> => OtpRel,
        <<"body">> => Archive
    },
    case maps:get(requires_system_architecture, Pkg, undefined) of
        undefined -> Msg;
        Arch -> Msg#{ <<"requires-system-architecture">> => Arch }
    end.
