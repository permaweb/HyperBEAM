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
-export([package/2, package_all/2]).
-export([spec_message/2, impl_message/3]).
-export([base32_lower/1, load_archive/1]).
-export([
    seed_device_names/1, volatile_device_store/1, load_and_cache_devices/3
]).
-ifdef(TEST).
-export([test_fixture_dir/0]).
-endif.

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
    Names = [Name || {Name, _Path} <- Sorted],
    ForcedRoots =
        sets:from_list(
            [N || {N, P} <- Sorted, file_has_implements(P)]
        ),
    Groups = group_by_namespace(Sorted, Names, ForcedRoots, LibFiles),
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
    Bin = hb_util:bin(Dir),
    case filelib:is_dir(Bin) of
        false -> [];
        true ->
            Pattern = filename:join(hb_util:list(Bin), "**/dev_*.erl"),
            [
                {atom_of_file(P), hb_util:bin(P)}
              ||
                P <- filelib:wildcard(Pattern)
            ]
    end.

%% @doc Recursively list `lib_*.erl' files in a directory.
list_lib_files(Dir) ->
    Bin = hb_util:bin(Dir),
    case filelib:is_dir(Bin) of
        false -> [];
        true ->
            Pattern = filename:join(hb_util:list(Bin), "**/lib_*.erl"),
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
group_by_namespace(Files, _Names, ForcedRoots, LibFiles) ->
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

%% @doc Package every device returned by {@link scan/2}. Normal package
%% identity uses the AO-Core message ID of the source-file message. The
%% packager privately bootstraps the devices needed to calculate that ID.
package_all(Groups, Opts) ->
    PackageGroups = package_groups(Groups, Opts),
    ?event(packager, {package_groups, {count, length(PackageGroups)}}),
    case package_id_mode(Opts) of
        bootstrap ->
            [package(G, Opts) || G <- PackageGroups];
        normal ->
            with_bootstrap_package_devices(
                PackageGroups,
                Opts,
                fun(BootOpts) ->
                    NormalOpts = BootOpts#{ <<"package-id-mode">> => normal },
                    [package(G, NormalOpts) || G <- PackageGroups]
                end
            )
    end.

%% @doc Include the seed devices in a group list when requested.
package_groups(Groups, Opts) ->
    case hb_maps:get(<<"include-build-seeds">>, Opts, false, Opts) of
        true -> unique_groups(Groups ++ seed_groups(Groups, Opts));
        false -> Groups
    end.

%% @doc Deduplicate package groups by root module while preserving order.
unique_groups(Groups) ->
    {_, Unique} =
        lists:foldl(
            fun(G, {Seen, Acc}) ->
                Root = maps:get(root, G),
                case sets:is_element(Root, Seen) of
                    true -> {Seen, Acc};
                    false -> {sets:add_element(Root, Seen), [G | Acc]}
                end
            end,
            {sets:new(), []},
            Groups
        ),
    lists:reverse(Unique).

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
    PackageFiles = maps:merge(Files, PrivFiles),
    SourceID = source_id(PackageFiles, Opts),
    SourceHash = source_id_to_hash(SourceID),
    ModName = hb_device_name:generated(Implements, SourceHash),
    ?event(
        packager,
        {packaging, {root, Root}, {hash, SourceHash}, {mod, ModName}}
    ),
    ArchivePkg =
        compile_archive(
            ModName, Root, RootFile, Helpers, Libraries, SourceHash,
            PrivFiles, Opts
        ),
    Exports = root_exports(RootAttrs),
    #{
        module_name => ModName,
        module_names => maps:get(module_names, ArchivePkg),
        device_name => Implements,
        hash => SourceHash,
        source_id => SourceID,
        archive => maps:get(archive, ArchivePkg),
        archive_modules => maps:get(archive_modules, ArchivePkg),
        beams => maps:get(beams, ArchivePkg),
        spec_body => SpecBody,
        spec_content_type => SpecContentType,
        implements => declared_implements(RootAttrs),
        exports => Exports,
        requires_otp_release =>
            hb_util:bin(erlang:system_info(otp_release)),
        requires_system_architecture =>
            hb_util:bin(erlang:system_info(system_architecture)),
        root_module => Root,
        helpers => [H || {H, _} <- Helpers],
        libraries => [L || {L, _} <- Libraries],
        files => Files,
        priv_files => PrivFiles
    }.

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

root_exports(Attrs) ->
    lists:flatten(
        [
            E
          ||
            {export, ExportList} <- Attrs,
            E <- ExportList
        ]
    ).

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

%% @doc Compute the normal package ID of a device's file set. The source
%% set is represented as a normal AO-Core message whose keys are bare source
%% filenames and whose values are the complete file contents.
source_id(FilesMap, Opts) ->
    SourceMsg = maps:from_list(lists:sort(maps:to_list(FilesMap))),
    case package_id_mode(Opts) of
        normal -> hb_message:id(SourceMsg, unsigned, Opts);
        bootstrap -> bootstrap_source_id(SourceMsg)
    end.

%% @doc Return the package identity mode for the current build.
package_id_mode(Opts) ->
    case hb_maps:get(<<"package-id-mode">>, Opts, normal, Opts) of
        normal -> normal;
        <<"normal">> -> normal;
        bootstrap -> bootstrap;
        <<"bootstrap">> -> bootstrap
    end.

%% @doc Build a forge-private source ID for first-phase bootstrap packages.
bootstrap_source_id(FilesMap) ->
    <<"bootstrap_", (base32_lower(crypto:hash(
        sha256,
        source_id_stream(FilesMap)
    )))/binary>>.

%% @doc Return a deterministic byte stream for bootstrap package identity.
source_id_stream(FilesMap) ->
    [
        <<"hyperbeam-bootstrap-device-source-v1">>,
        [
            [<<(byte_size(Name)):32>>, Name, <<(byte_size(Body)):64>>, Body]
         ||
            {Name, Body} <- lists:sort(maps:to_list(FilesMap))
        ]
    ].

%% @doc Convert package source identity to the suffix used in module names.
source_id_to_hash(<<"bootstrap_", _/binary>> = SourceID) ->
    SourceID;
source_id_to_hash(SourceID) ->
    base32_lower(hb_util:native_id(SourceID)).

%% @doc Load a generated package archive into the current code server.
load_archive(Pkg) ->
    hb_device_archive:load(maps:get(archive, Pkg)).

%% @doc Package with temporary generated message/codec devices loaded into a
%% build-local volatile device-store. The temporary packages are never written
%% to the preloaded-store.
with_bootstrap_package_devices(Groups, Opts, Fun) ->
    Store = volatile_device_store(<<"package-bootstrap">>),
    hb_store:start(Store, #{}, Opts),
    try
        BootOpts =
            Opts#{
                <<"package-id-mode">> => bootstrap,
                <<"device-store">> => Store
            },
        SeedPkgs = [package(G, BootOpts) || G <- seed_groups(Groups, Opts)],
        ok =
            load_and_cache_devices(
                SeedPkgs,
                seed_device_names(Opts),
                BootOpts
            ),
        try Fun(BootOpts)
        after purge_package_modules(SeedPkgs)
        end
    after
        hb_store:stop(Store, #{}, Opts)
    end.

%% @doc Build a uniquely-named volatile device store.
volatile_device_store(Prefix) ->
    #{
        <<"store-module">> => hb_store_volatile,
        <<"name">> =>
            iolist_to_binary([
                Prefix,
                <<"-">>,
                integer_to_binary(erlang:unique_integer([positive]))
            ])
    }.

%% @doc Return the groups needed to bootstrap package identity calculation.
seed_groups(Groups, Opts) ->
    Roots = seed_roots(Opts),
    Found = [G || G = #{ root := Root } <- Groups, lists:member(Root, Roots)],
    Missing = Roots -- [Root || #{ root := Root } <- Found],
    Extra = scan_seed_groups(Missing, Opts),
    StillMissing = Missing -- [Root || #{ root := Root } <- Extra],
    case StillMissing of
        [] -> Found ++ Extra;
        _ -> error({missing_bootstrap_device_sources, StillMissing})
    end.

scan_seed_groups([], _Opts) ->
    [];
scan_seed_groups(Roots, Opts) ->
    scan(bootstrap_device_dirs(Opts), #{ <<"device-roots">> => Roots }).

%% @doc Return the source root atoms needed by the bootstrap phase.
seed_roots(Opts) ->
    [device_name_to_root(Name) || Name <- seed_device_names(Opts)].

%% @doc Return the public device names required to sign preload messages.
seed_device_names(Opts) ->
    lists:usort([
        <<"message@1.0">>,
        <<"structured@1.0">>,
        hb_opts:get(commitment_device, <<"httpsig@1.0">>, Opts)
    ]).

%% @doc Convert a device name to its preloaded source module root.
device_name_to_root(Name) when ?IS_ID(Name) ->
    error({bootstrap_commitment_device_must_be_named, Name});
device_name_to_root(<<"~", Rest/binary>>) ->
    device_name_to_root(Rest);
device_name_to_root(Name) ->
    [Base | _] = binary:split(hb_util:bin(Name), <<"@">>),
    Tail0 = binary:replace(Base, <<"-">>, <<"_">>, [global]),
    Tail = binary:replace(Tail0, <<"/">>, <<"_">>, [global]),
    binary_to_atom(<<"dev_", Tail/binary>>, utf8).

%% @doc Return source directories to search for bootstrap seed devices.
bootstrap_device_dirs(Opts) ->
    case hb_maps:get(
        <<"bootstrap-device-src">>,
        Opts,
        [<<"src/preloaded">>, <<"_build/default/lib/hb/src/preloaded">>],
        Opts
    ) of
        Dir when is_binary(Dir) -> [Dir];
        Dir = [C | _] when is_integer(C) -> [Dir];
        Dirs when is_list(Dirs) -> Dirs
    end.

%% @doc Load packages and cache their public device names in the device-store.
load_and_cache_devices(Pkgs, Names, Opts) ->
    ByName = maps:from_list([{maps:get(device_name, Pkg), Pkg} || Pkg <- Pkgs]),
    lists:foreach(
        fun(Name) ->
            Pkg = maps:get(Name, ByName),
            ok = load_archive(Pkg),
            cache_device(Name, maps:get(module_name, Pkg), Opts)
        end,
        Names
    ).

%% @doc Cache a generated module for a device name.
cache_device(Name, ModName, Opts) ->
    Store = hb_maps:get(<<"device-store">>, Opts, undefined, Opts),
    hb_store:write(
        Store,
        #{ <<"devices/", Name/binary>> => atom_to_binary(ModName, utf8) },
        Opts
    ).

%% @doc Purge all generated modules for a set of packages.
purge_package_modules(Pkgs) ->
    lists:foreach(
        fun(Mod) ->
            code:purge(Mod),
            code:delete(Mod),
            code:purge(Mod)
        end,
        lists:append([maps:get(module_names, Pkg) || Pkg <- Pkgs])
    ).

%% @doc Encode bytes as lowercase, unpadded base32 (RFC 4648 alphabet).
base32_lower(Bin) when is_binary(Bin) ->
    base32_encode_lower(Bin, <<>>).

base32_encode_lower(<<>>, Acc) -> Acc;
base32_encode_lower(<<A, B, C, D, E, Rest/binary>>, Acc) ->
    % Encode 5 input bytes into 8 base32 characters.
    Bits = <<A, B, C, D, E>>,
    Acc1 = encode_chunk(Bits, Acc, 8),
    base32_encode_lower(Rest, Acc1);
base32_encode_lower(Tail, Acc) ->
    % Tail is 1..4 bytes, so emit only the meaningful base32 chars.
    Bytes = byte_size(Tail),
    Bits = <<Tail/binary, 0:((5 - Bytes) * 8)>>,
    OutChars =
        case Bytes of
            1 -> 2;
            2 -> 4;
            3 -> 5;
            4 -> 7
        end,
    encode_chunk(Bits, Acc, OutChars).

encode_chunk(_Bits, Acc, 0) -> Acc;
encode_chunk(<<I:5, Rest/bitstring>>, Acc, N) ->
    encode_chunk(Rest, <<Acc/binary, (b32_char(I))>>, N - 1).

b32_char(I) when I < 26 -> $a + I;
b32_char(I) when I < 32 -> $2 + (I - 26).

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
        RootMod, Root, RootFile, Helpers, Libraries, Hash, PrivFiles, Opts) ->
    Entries = [{Root, RootFile} | Helpers ++ Libraries],
    Renamings = module_renamings(RootMod, Root, Entries),
    RenameMap = maps:from_list(Renamings),
    IncludeDirs = include_dirs(Entries),
    Compiled =
        [
            compile_module(Old, Path, RenameMap, IncludeDirs, Opts)
         ||
            {Old, Path} <- Entries
        ],
    ArchiveModules = archive_module_metadata(Compiled),
    #{
        archive => make_archive(Compiled, PrivFiles),
        archive_modules => ArchiveModules,
        beams => maps:from_list(
            [
                {atom_to_binary(Mod, utf8), Beam}
              ||
                #{ module := Mod, beam := Beam } <- Compiled
            ]
        ),
        module_names => [New || {_Old, New} <- Renamings],
        hash => Hash
    }.

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
                {device_compile_failed, Old,
                    [{errors, Errors}, {warnings, Warnings},
                     {source_path, Path}]}
            );
        Other ->
            erlang:error({device_compile_failed, Old, Other})
    end.

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

%% @doc Build flat archive metadata for every generated BEAM module.
archive_module_metadata(Compiled) ->
    [
        #{
            <<"module-name">> => atom_to_binary(Mod, utf8),
            <<"archive-path">> =>
                hb_util:bin(
                    filename:join(
                        <<"ebin">>,
                        <<(atom_to_binary(Mod, utf8))/binary, ".beam">>
                    )
                )
        }
      ||
        #{ module := Mod } <- Compiled
    ].

%% @doc Create the deterministic in-memory ZIP used as implementation body.
make_archive(Compiled, PrivFiles) ->
    Files =
        [
            {
                hb_util:list(
                    filename:join(
                        <<"ebin">>,
                        <<(atom_to_binary(Mod, utf8))/binary, ".beam">>
                    )
                ),
                Beam,
                archive_file_info(<<"ebin">>, byte_size(Beam))
            }
          ||
            #{ module := Mod, beam := Beam } <- Compiled
        ] ++ [
            {
                hb_util:list(Path),
                Body,
                archive_file_info(Path, byte_size(Body))
            }
          ||
            {Path, Body} <- lists:sort(maps:to_list(PrivFiles))
        ],
    {ok, {_, Archive}} =
        zip:create(
            <<"device.beams.zip">>,
            Files,
            [memory, {extra, []}, {uncompress, all}]
        ),
    Archive.

%% @doc Return deterministic zip file metadata for reproducible archives.
archive_file_info(Path, Size) ->
    FixedTime = {{1980, 1, 1}, {0, 0, 0}},
    #file_info{
        size = Size,
        type = regular,
        access = read,
        atime = FixedTime,
        mtime = FixedTime,
        ctime = FixedTime,
        mode = archive_file_mode(Path)
    }.

%% @doc Mark executable archive resources when their path convention implies it.
archive_file_mode(<<"priv/bin/", _/binary>>) -> 8#100755;
archive_file_mode(Path) ->
    case filename:extension(hb_util:list(Path)) of
        ".sh" -> 8#100755;
        _ -> 8#100644
    end.

%%% --------------------------------------------------------------------
%%% AO-Core message construction
%%% --------------------------------------------------------------------

%% @doc Build the (unsigned) device-specification message.
spec_message(#{ device_name := Name, spec_body := Body, spec_content_type := CType }, _Opts) ->
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
        requires_otp_release := OtpRel,
        requires_system_architecture := Arch
    } = Pkg,
    % Keep archive bytes and loader metadata flat in the implementation message.
    #{
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => ?VARIANT,
        <<"content-type">> => ?ARCHIVE_CONTENT_TYPE,
        <<"archive-format">> => <<"zip">>,
        <<"implements-device">> => SpecID,
        <<"module-name">> => atom_to_binary(ModName, utf8),
        <<"requires-otp-release">> => OtpRel,
        <<"requires-system-architecture">> => Arch,
        <<"body">> => Archive
    }.

%%% --------------------------------------------------------------------
%%% Tests
%%% --------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Build a temporary source directory with a minimal device root and
%% one helper module, then exercise the full scan/package pipeline.
test_fixture_dir() ->
    Tmp =
        filename:join([
            <<"/tmp">>,
            <<"hb_packager_test_",
                (integer_to_binary(erlang:system_time()))/binary>>
        ]),
    ok = filelib:ensure_dir(filename:join(Tmp, <<".keep">>)),
    Stamp = integer_to_binary(erlang:unique_integer([positive])),
    Root = <<
        "%% fixture ", Stamp/binary, "\n"
        "%%% @doc Test device - packager fixture.\n"
        "%%% Lines of moduledoc become the spec body.\n"
        "-module(dev_test_pkg).\n"
        "-export([echo/3, hello/3, hello_via_capture/3]).\n"
        "\n"
        "echo(_Base, Req, _Opts) -> {ok, Req}.\n"
        "hello(Base, _Req, Opts) ->\n"
        "    Greeting = dev_test_pkg_helper:greet(Base, Opts),\n"
        "    {ok, Greeting}.\n"
        "hello_via_capture(Base, _Req, Opts) ->\n"
        "    Greeting = (fun dev_test_pkg_helper:greet/2)(Base, Opts),\n"
        "    {ok, Greeting}.\n"
    >>,
    Helper = <<
        "-module(dev_test_pkg_helper).\n"
        "-export([greet/2]).\n"
        "\n"
        "greet(_Base, _Opts) -> <<\"hello\">>.\n"
    >>,
    ok = file:write_file(
        filename:join(Tmp, <<"dev_test_pkg.erl">>), Root),
    ok = file:write_file(
        filename:join(Tmp, <<"dev_test_pkg_helper.erl">>), Helper),
    Tmp.

dynamic_dispatch_fixture_dir() ->
    Tmp =
        filename:join([
            <<"/tmp">>,
            <<"hb_packager_dynamic_test_",
                (integer_to_binary(erlang:system_time()))/binary>>
        ]),
    ok = filelib:ensure_dir(filename:join(Tmp, <<".keep">>)),
    Root = <<
        "-module(dev_dyn_pkg).\n"
        "-export([call/3]).\n"
        "\n"
        "call(Base, _Req, Opts) ->\n"
        "    {ok, dev_dyn_pkg_helper:dispatch(greet, Base, Opts)}.\n"
    >>,
    Helper = <<
        "-module(dev_dyn_pkg_helper).\n"
        "-export([dispatch/3, greet/2]).\n"
        "\n"
        "dispatch(F, Base, Opts) -> dev_dyn_pkg_helper:F(Base, Opts).\n"
        "greet(_Base, _Opts) -> <<\"hello\">>.\n"
    >>,
    ok = file:write_file(
        filename:join(Tmp, <<"dev_dyn_pkg.erl">>), Root),
    ok = file:write_file(
        filename:join(Tmp, <<"dev_dyn_pkg_helper.erl">>), Helper),
    Tmp.

priv_fixture_dir() ->
    Tmp = test_fixture_dir(),
    PrivDir = filename:join([Tmp, <<"priv">>, <<"dev_test_pkg">>]),
    ok = filelib:ensure_dir(filename:join([PrivDir, <<"bin">>, <<"tool">>])),
    ok = file:write_file(filename:join([PrivDir, <<"bin">>, <<"tool">>]),
        <<"#!/bin/sh\n">>),
    ok = filelib:ensure_dir(filename:join([PrivDir, <<"share">>, <<"data">>])),
    ok = file:write_file(filename:join([PrivDir, <<"share">>, <<"data">>]),
        <<"fixture-data">>),
    Tmp.

scan_groups_root_with_helper_test() ->
    Dir = test_fixture_dir(),
    Groups = scan([Dir], #{}),
    ?assertMatch([_], Groups),
    [#{ root := Root, helpers := Helpers, files := Files }] = Groups,
    ?assertEqual(dev_test_pkg, Root),
    ?assertMatch([{dev_test_pkg_helper, _}], Helpers),
    ?assertEqual(2, map_size(Files)).

scan_groups_transitive_helpers_under_root_test() ->
    Tmp =
        filename:join([
            <<"/tmp">>,
            <<"hb_packager_transitive_test_",
                (integer_to_binary(erlang:system_time()))/binary>>
        ]),
    ok = filelib:ensure_dir(filename:join(Tmp, <<".keep">>)),
    write_module(Tmp, dev_test_tree, <<"-export([ok/3]).\n">>),
    write_module(Tmp, dev_test_tree_branch, <<"-export([ok/0]).\n">>),
    write_module(Tmp, dev_test_tree_branch_leaf, <<"-export([ok/0]).\n">>),
    [#{ root := Root, helpers := Helpers, files := Files }] = scan([Tmp], #{}),
    ?assertEqual(dev_test_tree, Root),
    ?assertEqual(
        [dev_test_tree_branch, dev_test_tree_branch_leaf],
        [H || {H, _} <- Helpers]
    ),
    ?assertEqual(3, map_size(Files)).

write_module(Dir, Mod, Body) ->
    Name = atom_to_binary(Mod, utf8),
    ok = file:write_file(
        filename:join(Dir, <<Name/binary, ".erl">>),
        <<"-module(", Name/binary, ").\n", Body/binary>>
    ).

generated_module_name_pattern_test() ->
    Hash = base32_lower(crypto:hash(sha256, <<"abc">>)),
    Mod = hb_device_name:generated(<<"message@1.0">>, Hash),
    Bin = atom_to_binary(Mod, utf8),
    ?assertMatch(<<"_hb_device_message_1_0_", _/binary>>, Bin),
    ?assert(hb_device_name:is_generated(Mod)),
    ?assertMatch({<<"message_1_0">>, _}, hb_device_name:parts(Mod)).

base32_lower_known_vector_test() ->
    % RFC 4648 section 10 vectors, lowercase, unpadded.
    ?assertEqual(<<>>, base32_lower(<<>>)),
    ?assertEqual(<<"my">>, base32_lower(<<"f">>)),
    ?assertEqual(<<"mzxq">>, base32_lower(<<"fo">>)),
    ?assertEqual(<<"mzxw6">>, base32_lower(<<"foo">>)),
    ?assertEqual(<<"mzxw6yq">>, base32_lower(<<"foob">>)),
    ?assertEqual(<<"mzxw6ytb">>, base32_lower(<<"fooba">>)),
    ?assertEqual(<<"mzxw6ytboi">>, base32_lower(<<"foobar">>)).

package_emits_root_only_exports_test() ->
    Dir = test_fixture_dir(),
    [Group] = scan([Dir], #{}),
    Pkg = package_for_test(Group),
    Mod = maps:get(module_name, Pkg),
    ?assert(hb_device_name:is_generated(Mod)),
    ok = load_pkg_archive(Pkg),
    Exports = lists:sort(Mod:module_info(exports)),
    % Root exports plus module_info.
    ?assert(lists:member({echo, 3}, Exports)),
    ?assert(lists:member({hello, 3}, Exports)),
    ?assert(lists:member({hello_via_capture, 3}, Exports)),
    ?assertNot(lists:member({greet, 2}, Exports)).

package_helper_not_loaded_separately_test() ->
    Dir = test_fixture_dir(),
    [Group] = scan([Dir], #{}),
    Pkg = package_for_test(Group),
    Mod = maps:get(module_name, Pkg),
    [Mod, HelperMod] = maps:get(module_names, Pkg),
    % Ensure helper isn't loaded yet.
    code:purge(dev_test_pkg_helper),
    code:delete(dev_test_pkg_helper),
    ok = load_pkg_archive(Pkg),
    {ok, Greeting} = Mod:hello(#{}, #{}, #{}),
    ?assertEqual(<<"hello">>, Greeting),
    {ok, CapturedGreeting} = Mod:hello_via_capture(#{}, #{}, #{}),
    ?assertEqual(<<"hello">>, CapturedGreeting),
    % The source helper is not loaded; only the generated helper is.
    ?assertEqual(false, code:is_loaded(dev_test_pkg_helper)),
    ?assertMatch({file, _}, code:is_loaded(HelperMod)).

dynamic_internal_dispatch_rejected_test() ->
    Dir = dynamic_dispatch_fixture_dir(),
    [Group] = scan([Dir], #{}),
    ?assertError(
        {dynamic_internal_dispatch, _, _},
        package_for_test(Group)
    ).

archive_contains_ebin_and_priv_entries_test() ->
    Dir = priv_fixture_dir(),
    [Group] = scan([Dir], #{}),
    Pkg = package_for_test(Group),
    Msg = impl_message(Pkg, <<"spec-id">>, #{}),
    ?assertEqual(
        hb_util:bin(erlang:system_info(system_architecture)),
        maps:get(<<"requires-system-architecture">>, Msg)
    ),
    {ok, Files} = zip:unzip(maps:get(archive, Pkg), [memory]),
    ByPath =
        maps:from_list([{hb_util:bin(Path), Body} || {Path, Body} <- Files]),
    ?assert(maps:is_key(
        <<"ebin/", (atom_to_binary(maps:get(module_name, Pkg), utf8))/binary,
            ".beam">>,
        ByPath
    )),
    ?assertEqual(<<"#!/bin/sh\n">>, maps:get(<<"priv/bin/tool">>, ByPath)),
    ?assertEqual(<<"fixture-data">>, maps:get(<<"priv/share/data">>, ByPath)).

derived_implements_uses_module_name_test() ->
    Dir = test_fixture_dir(),
    [Group] = scan([Dir], #{}),
    Pkg = package_for_test(Group),
    % No `-implements' attribute, so derived from module name.
    ?assertEqual(<<"test-pkg@1.0">>, maps:get(device_name, Pkg)).

hash_changes_with_content_test() ->
    Dir = test_fixture_dir(),
    [Group] = scan([Dir], #{}),
    Pkg1 = package_for_test(Group),
    Hash1 = maps:get(hash, Pkg1),
    % Mutate the helper file slightly and re-scan.
    HelperPath = filename:join(Dir, <<"dev_test_pkg_helper.erl">>),
    {ok, Old} = file:read_file(HelperPath),
    ok = file:write_file(HelperPath,
        <<Old/binary, "%% noise\n">>),
    [Group2] = scan([Dir], #{}),
    Pkg2 = package_for_test(Group2),
    Hash2 = maps:get(hash, Pkg2),
    ?assertNotEqual(Hash1, Hash2).

package_hash_is_source_message_id_test() ->
    Dir = test_fixture_dir(),
    [Group] = scan([Dir], #{}),
    [Pkg] = package_all(
        [Group],
        #{ <<"bootstrap-device-src">> => [<<"src/preloaded">>] }
    ),
    SourceID = maps:get(source_id, Pkg),
    ?assert(?IS_ID(SourceID)),
    ?assertEqual(source_id_to_hash(SourceID), maps:get(hash, Pkg)).

package_for_test(Group) ->
    package(Group, #{ <<"package-id-mode">> => bootstrap }).

load_pkg_archive(Pkg) ->
    Archive = maps:get(archive, Pkg),
    {ok, Files} = zip:unzip(Archive, [memory]),
    Beams =
        maps:from_list([{hb_util:bin(Name), Beam} || {Name, Beam} <- Files]),
    Modules =
        [
            begin
                Path = maps:get(<<"archive-path">>, Meta),
                ModBin = maps:get(<<"module-name">>, Meta),
                Mod = binary_to_atom(ModBin, utf8),
                {Mod, hb_util:list(Path), maps:get(Path, Beams)}
            end
          ||
            Meta <- maps:get(archive_modules, Pkg)
        ],
    hb_device_archive:load_modules(Modules).

-endif.
