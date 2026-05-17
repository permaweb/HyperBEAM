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
-export([sanitize_device_name/1, generated_module_name/2]).
-export([is_generated_module/1, generated_module_parts/1]).
-export([encode_on_loads/1, decode_on_loads/1]).
-export([base32_lower/1, load_archive/1]).
-ifdef(TEST).
-export([test_fixture_dir/0]).
-endif.

-include("include/hb.hrl").
-include_lib("kernel/include/file.hrl").

-define(VARIANT, <<"ao.N.1">>).
-define(DEFAULT_DEVICE_VERSION, <<"@1.0">>).
-define(GENERATED_MOD_PREFIX, <<"_hb_device_">>).
-define(ARCHIVE_CONTENT_TYPE, <<"application/beam-archive">>).
-define(ON_LOAD_FORMAT, <<"hb-device-on-load-v1">>).

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
%% '''
%% Files are returned by their bare filename (not their full path) so the
%% hash is independent of the build location.
scan(Dirs) -> scan(Dirs, #{}).

scan(Dirs, Opts) when is_list(Dirs) ->
    %% Get all device files under the given directories.
    Files = lists:flatmap(fun list_dev_files/1, Dirs),
    LibFiles = maps:from_list(lists:flatmap(fun list_lib_files/1, Dirs)),
    %% Sort once so grouping is deterministic.
    Sorted = lists:keysort(1, Files),
    Names = [Name || {Name, _Path} <- Sorted],
    %% Modules that declare `-implements(...)' are unambiguously
    %% roots — they cannot be folded into another namespace as
    %% helpers regardless of how their atom name happens to look.
    ForcedRoots =
        sets:from_list(
            [N || {N, P} <- Sorted, file_has_implements(P)]
        ),
    Groups = group_by_namespace(Sorted, Names, ForcedRoots, LibFiles),
    DeviceRoots = hb_maps:get(<<"device-roots">>, Opts, all, Opts),
    case DeviceRoots of
        all -> Groups;
        Filter when is_list(Filter) ->
            % If a filter is present, remove any groups that do not match the filter.
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
            Pattern = filename:join(binary_to_list(Bin), "**/dev_*.erl"),
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
            Pattern = filename:join(binary_to_list(Bin), "**/lib_*.erl"),
            [
                {atom_of_file(P), hb_util:bin(P)}
              ||
                P <- filelib:wildcard(Pattern)
            ]
    end.

%% @doc Convert a filename to an atom.
atom_of_file(Path) ->
    list_to_atom(filename:rootname(filename:basename(Path))).

%% Cheap, lossy check for an `-implements(...)' attribute. Reading
%% the source as a binary is enough — we just want to know whether
%% the module is intentionally a root (and so should not be folded
%% into another namespace as a helper).
file_has_implements(Path) ->
    case file:read_file(binary_to_list(hb_util:bin(Path))) of
        {ok, Bin} ->
            nomatch =/= binary:match(Bin, <<"-implements(">>);
        _ ->
            false
    end.

%% @doc Group module files into device packages by namespace prefix.
%% A module `dev_foo_bar' is a helper of `dev_foo' iff `dev_foo' exists
%% in the candidate set AND `dev_foo_bar' itself does not declare a
%% `-implements(...)' attribute.  Modules that explicitly declare the
%% device they implement are always roots.
group_by_namespace(Files, _Names, ForcedRoots, LibFiles) ->
    ByDepth =
        lists:sort(
            fun({A, _}, {B, _}) -> namespace_key(A) =< namespace_key(B) end,
            Files
        ),
    % Resolve each file into a package root. If a file declares
    % -implements(...), it is added to the roots list. If a file
    % matches an existing root prefix, it is added to that root's
    % helpers list. Otherwise, it is added to the roots list.
    {Roots, Helpers, _RootSet} =
        lists:foldl(
            fun({Module, _Path}, {RAcc, HAcc, RootSet}) ->
                case sets:is_element(Module, ForcedRoots) of
                    true ->
                        % If the module declares -implements(...),
                        % add it to the roots list.
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
    % For each root, create a package map. The helpers list contains
    % {helper module atom, helper file path binary} pairs; files maps
    % file name binaries to file content binaries.
    [
        begin
            RootHelpers =
                lists:sort(
                    [H || {R, H} <- Helpers, R =:= Root]
                ),
            RootLibraries = library_modules(maps:get(Root, FilesMap), LibFiles),
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

%% @doc Return the lib_* modules explicitly requested by a device root.
library_modules(RootFile, LibFiles) ->
    {_Forms, Attrs} = parse_module(RootFile),
    lists:map(
        fun(Mod) ->
            case maps:find(Mod, LibFiles) of
                {ok, Path} -> {Mod, Path};
                error -> erlang:error({missing_device_library, Mod, RootFile})
            end
        end,
        lists:usort(lists:flatmap(fun library_attr/1, Attrs))
    ).

library_attr({device_libraries, Mods}) ->
    normalize_libraries(Mods);
library_attr(_) ->
    [].

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
            %% Walk longest-prefix-first so the first match wins.
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

filename_only(Path) ->
    hb_util:bin(filename:basename(binary_to_list(hb_util:bin(Path)))).

read_file(Path) ->
    {ok, Bin} = file:read_file(binary_to_list(hb_util:bin(Path))),
    Bin.

%%% --------------------------------------------------------------------
%%% Packaging
%%% --------------------------------------------------------------------

%% @doc Package every device returned by {@link scan/2}. Normal package
%% identity uses the AO-Core message ID of the source-file message. The
%% packager privately bootstraps the devices needed to calculate that ID.
package_all(Groups, Opts) ->
    PackageGroups = package_groups(Groups, Opts),
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

package_groups(Groups, Opts) ->
    case hb_maps:get(<<"include-build-seeds">>, Opts, false, Opts) of
        true -> unique_groups(Groups ++ seed_groups(Groups, Opts));
        false -> Groups
    end.

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
    % Get the attirbutes of the root module.
    {_RootForms, RootAttrs} = parse_module(RootFile),
    % If the root or any helpers dynamically call the
    % original internal module namespace, reject.
    reject_dynamic_internal_source_calls(
        [Root | [M || {M, _} <- Helpers ++ Libraries]],
        [{Root, RootFile} | Helpers ++ Libraries]
    ),
    % Use the declared `-implements' value when present, otherwise derive the
    % device name from the root module atom.
    Implements = derived_or_declared_implements(Root, RootAttrs),
    % Build the human-readable module spec. This is derived from either
    % the specification attribute or the moduledoc.
    {SpecBody, SpecContentType} = derive_spec(RootFile, RootAttrs, Opts),
    PrivFiles = priv_files(Root, RootFile),
    % Package identity is derived from the source set itself, so a
    % rebuilt package gets the same generated module name as long as its input
    % files are byte-for-byte identical.
    PackageFiles = maps:merge(Files, PrivFiles),
    SourceID = source_id(PackageFiles, Opts),
    SourceHash = source_id_to_hash(SourceID),
    % Generate the module name using the hash of the source set.
    ModName = generated_module_name(Implements, SourceHash),
    ?event(packager, {packaging, {root, Root}, {SourceHash, SourceHash}, {mod, ModName}}),
    % Compile the rewritten root + helpers into one deterministic archive.
    ArchivePkg =
        compile_archive(
            ModName, Root, RootFile, Helpers, Libraries, SourceHash,
            PrivFiles, Opts
        ),
    % Export metadata is kept separately so the runtime can verify the root
    % API without unpacking the original source again.
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
        on_load => maps:get(on_load, ArchivePkg),
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

%%% Source parsing. We use `epp_dodger' so we do not need include
%%% paths or macro definitions to read the file. The dodger output is
%%% a syntax-tree list (not necessarily reverted Erlang abstract
%%% forms): we keep it that way and pass it straight to Igor, which
%%% accepts syntax trees.
parse_module(Path) when is_binary(Path) ->
    parse_module(binary_to_list(Path));
parse_module(Path) when is_list(Path) ->
    case epp_dodger:parse_file(Path, []) of
        {ok, Forms} ->
            {Forms, collect_attributes(Forms)};
        {error, Reason} ->
            erlang:error({source_parse_failed, Path, Reason})
    end.

reject_dynamic_internal_source_calls(InternalMods, Modules) ->
    lists:foreach(
        fun({Mod, Path}) ->
            Source = read_file(Path),
            case dynamic_internal_source_calls(Source, InternalMods) of
                [] -> ok;
                Bad -> erlang:error({dynamic_internal_dispatch, Mod, Bad})
            end
        end,
        Modules
    ).

dynamic_internal_source_calls(Source, InternalMods) ->
    lists:filtermap(
        fun(InternalMod) ->
            ModBin = iolist_to_binary(io_lib:write_atom(InternalMod)),
            Pattern = <<ModBin/binary, "\\s*:\\s*[A-Z_]">>,
            case re:run(Source, Pattern) of
                nomatch -> false;
                _ -> {true, InternalMod}
            end
        end,
        InternalMods
    ).

%% Extract the standard Erlang attributes (-export, -implements,
%% -specification, ...) from a list of syntax-tree forms. Forms whose
%% macro shape `erl_syntax_lib:analyze_form/1' cannot understand are
%% silently skipped — the device contract only cares about the small
%% set of attributes consumed below.
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
                    %% Already a name@version binary. Pass through.
                    Decl
            end
    end.

derived_implements(Root) ->
    "dev_" ++ Tail = atom_to_list(Root),
    Hyphenated = list_to_binary(string:replace(Tail, "_", "-", all)),
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
            {ok, Bin} = file:read_file(binary_to_list(hb_util:bin(ResolvedPath))),
            {Bin, content_type_of(ResolvedPath)};
        _ ->
            {extract_moduledoc(RootFile), <<"text/markdown">>}
    end.

resolve_spec_path(Path, RootFile) ->
    PathBin = hb_util:bin(Path),
    case filelib:is_file(PathBin) of
        true -> PathBin;
        false ->
            Dir = filename:dirname(binary_to_list(hb_util:bin(RootFile))),
            hb_util:bin(filename:join(Dir, binary_to_list(PathBin)))
    end.

content_type_of(Path) ->
    case filename:extension(binary_to_list(hb_util:bin(Path))) of
        ".html" -> <<"text/html">>;
        ".htm" -> <<"text/html">>;
        _ -> <<"text/markdown">>
    end.

%% @doc Return `priv/...' archive entries for a device package.
priv_files(Root, RootFile) ->
    Dirs = default_priv_dirs(Root, RootFile),
    maps:from_list(lists:append([priv_files_from_dir(Dir) || Dir <- Dirs])).

default_priv_dirs(Root, RootFile) ->
    SourceDir = filename:dirname(binary_to_list(hb_util:bin(RootFile))),
    RootDir = atom_to_list(Root),
    RootPrivDirs = [
        filename:join([SourceDir, "priv", RootDir]),
        filename:join([SourceDir, RootDir, "priv"])
    ],
    case [Dir || Dir <- RootPrivDirs, filelib:is_dir(Dir)] of
        [] -> shared_priv_dirs(SourceDir);
        Dirs -> lists:usort(Dirs)
    end.

shared_priv_dirs(SourceDir) ->
    case filename:basename(SourceDir) of
        "src" ->
            [
                filename:join(SourceDir, "priv"),
                filename:join(filename:dirname(SourceDir), "priv")
            ];
        _ ->
            [filename:join(SourceDir, "priv")]
    end.

priv_files_from_dir(Dir0) ->
    Dir = filename:absname(binary_to_list(hb_util:bin(Dir0))),
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
        {hb_util:bin(filename:join("priv", relative_path(Dir, Path))),
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
    {ok, Bin} = file:read_file(binary_to_list(hb_util:bin(Path))),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    extract_moduledoc_lines(Lines, []).

extract_moduledoc_lines([], Acc) -> reverse_concat(Acc);
extract_moduledoc_lines([Line | Rest], Acc) ->
    case match_doc_line(Line) of
        {ok, Stripped} ->
            extract_moduledoc_lines(Rest, [Stripped | Acc]);
        skip when Acc =:= [] ->
            %% Skip leading lines until we see the first doc line.
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

package_id_mode(Opts) ->
    case hb_maps:get(<<"package-id-mode">>, Opts, normal, Opts) of
        normal -> normal;
        <<"normal">> -> normal;
        bootstrap -> bootstrap;
        <<"bootstrap">> -> bootstrap
    end.

bootstrap_source_id(FilesMap) ->
    <<"bootstrap_", (base32_lower(crypto:hash(
        sha256,
        source_id_stream(FilesMap)
    )))/binary>>.

source_id_stream(FilesMap) ->
    [
        <<"hyperbeam-bootstrap-device-source-v1">>,
        [
            [<<(byte_size(Name)):32>>, Name, <<(byte_size(Body)):64>>, Body]
         ||
            {Name, Body} <- lists:sort(maps:to_list(FilesMap))
        ]
    ].

source_id_to_hash(<<"bootstrap_", _/binary>> = SourceID) ->
    SourceID;
source_id_to_hash(SourceID) ->
    base32_lower(hb_util:native_id(SourceID)).

%% @doc Load a generated package archive into the current code server.
load_archive(Pkg) ->
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
                {Mod, binary_to_list(Path), maps:get(Path, Beams)}
            end
         ||
            Meta <- maps:get(archive_modules, Pkg)
        ],
    case code:atomic_load(Modules) of
        ok ->
            ok;
        {error, Reason} ->
            case lists:all(
                fun({Mod, _, _}) -> code:is_loaded(Mod) =/= false end,
                Modules
            ) of
                true -> ok;
                false -> {error, {archive_load_failed, Reason}}
            end
    end.

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
        ok = load_and_cache_seed_devices(SeedPkgs, BootOpts),
        try Fun(BootOpts)
        after purge_package_modules(SeedPkgs)
        end
    after
        hb_store:stop(Store, #{}, Opts)
    end.

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

seed_roots(Opts) ->
    [device_name_to_root(Name) || Name <- seed_device_names(Opts)].

seed_device_names(Opts) ->
    lists:usort([
        <<"message@1.0">>,
        <<"structured@1.0">>,
        hb_opts:get(commitment_device, <<"httpsig@1.0">>, Opts)
    ]).

device_name_to_root(Name) when ?IS_ID(Name) ->
    error({bootstrap_commitment_device_must_be_named, Name});
device_name_to_root(<<"~", Rest/binary>>) ->
    device_name_to_root(Rest);
device_name_to_root(Name) ->
    [Base | _] = binary:split(hb_util:bin(Name), <<"@">>),
    Tail0 = binary:replace(Base, <<"-">>, <<"_">>, [global]),
    Tail = binary:replace(Tail0, <<"/">>, <<"_">>, [global]),
    binary_to_atom(<<"dev_", Tail/binary>>, utf8).

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

load_and_cache_seed_devices(Pkgs, Opts) ->
    ByName = maps:from_list([{maps:get(device_name, Pkg), Pkg} || Pkg <- Pkgs]),
    lists:foreach(
        fun(Name) ->
            Pkg = maps:get(Name, ByName),
            ok = load_archive(Pkg),
            cache_seed_device(Name, maps:get(module_name, Pkg), Opts)
        end,
        seed_device_names(Opts)
    ).

cache_seed_device(Name, ModName, Opts) ->
    Store = hb_maps:get(<<"device-store">>, Opts, undefined, Opts),
    hb_store:write(
        Store,
        #{ <<"devices/", Name/binary>> => atom_to_binary(ModName, utf8) },
        Opts
    ).

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
    %% Encode 5 input bytes into 8 base32 characters.
    Bits = <<A, B, C, D, E>>,
    Acc1 = encode_chunk(Bits, Acc, 8),
    base32_encode_lower(Rest, Acc1);
base32_encode_lower(Tail, Acc) ->
    %% Tail is 1..4 bytes — emit only the meaningful base32 chars.
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

%% @doc Build the generated module atom for a device.
generated_module_name(DeviceName, Hash) ->
    Sanitized = sanitize_device_name(DeviceName),
    Bin = <<?GENERATED_MOD_PREFIX/binary, Sanitized/binary, "_", Hash/binary>>,
    binary_to_atom(Bin, utf8).

%% @doc Sanitize a device name so it can appear inside an Erlang atom.
%% `name@1.0' becomes `name_1_0', `~codec/cookie@1.0' becomes
%% `codec_cookie_1_0', etc.  ID-style device names are passed through
%% lowercased.
sanitize_device_name(Name) when is_binary(Name) ->
    Lower = string:lowercase(Name),
    list_to_binary(
        [sanitize_char(C) || <<C>> <= Lower]
    );
sanitize_device_name(Name) when is_list(Name) ->
    sanitize_device_name(hb_util:bin(Name));
sanitize_device_name(Name) when is_atom(Name) ->
    sanitize_device_name(atom_to_binary(Name, utf8)).

sanitize_char(C) when C >= $a, C =< $z -> C;
sanitize_char(C) when C >= $0, C =< $9 -> C;
sanitize_char(_) -> $_.

%% @doc Recognise a generated `_hb_device_*' module atom.
is_generated_module(Atom) when is_atom(Atom) ->
    is_generated_module(atom_to_binary(Atom, utf8));
is_generated_module(Bin) when is_binary(Bin) ->
    case Bin of
        <<"_hb_device_", _/binary>> -> true;
        _ -> false
    end;
is_generated_module(_) -> false.

%% @doc Decompose a generated module name into its sanitized device name
%% and hash. Returns `not_generated' if the atom is not in generated form.
generated_module_parts(Atom) when is_atom(Atom) ->
    generated_module_parts(atom_to_binary(Atom, utf8));
generated_module_parts(Bin) when is_binary(Bin) ->
    case Bin of
        <<"_hb_device_", Rest/binary>> ->
            [RootPart | HelperParts] = binary:split(Rest, <<"__">>, [global]),
            case binary:split(RootPart, <<"_">>, [global]) of
                Parts when length(Parts) >= 2 ->
                    [Hash | RevName] = lists:reverse(Parts),
                    Name =
                        iolist_to_binary(
                            lists:join(<<"_">>, lists:reverse(RevName))
                        ),
                    case HelperParts of
                        [] ->
                            {Name, Hash};
                        _ ->
                            Helper = iolist_to_binary(
                                lists:join(<<"__">>, HelperParts)
                            ),
                            {Name, Hash, Helper}
                    end;
                _ -> not_generated
            end;
        _ -> not_generated
    end;
generated_module_parts(_) -> not_generated.

%%% --------------------------------------------------------------------
%%% Igor rename + archive compile
%%% --------------------------------------------------------------------

%% @doc Rename every source module in a device namespace, compile the
%% renamed modules independently, and pack their BEAMs into a
%% deterministic in-memory ZIP archive.
compile_archive(
        RootMod, Root, RootFile, Helpers, Libraries, Hash, PrivFiles, Opts) ->
    Entries = [{Root, RootFile} | Helpers ++ Libraries],
    Renamings = module_renamings(RootMod, Root, Entries),
    TmpDir = package_tmp_dir(RootMod),
    Copied =
        [copy_package_source(TmpDir, Mod, Path) || {Mod, Path} <- Entries],
    CopiedPaths = [Path || #{ path := Path } <- Copied],
    OnLoads = package_on_loads(Renamings, Copied),
    IncludeDirs = include_dirs(Copied),
    %% Igor rewrites the root and helpers into one generated namespace before
    %% compilation, so the archive is self-contained and never refers back to
    %% the original `dev_*' module names at runtime.
    IgorOpts = [
        {stubs, false},
        {backups, false},
        {comments, true},
        {notes, no},
        {tidy, false},
        {preprocess, true},
        {includes, IncludeDirs}
    ],
    RenamedPaths = igor:rename(CopiedPaths, Renamings, IgorOpts),
    Compiled =
        compile_renamed_modules(
            RenamedPaths,
            Copied,
            Renamings,
            OnLoads,
            IncludeDirs,
            Opts
        ),
    OriginalMods = [Mod || {Mod, _} <- Entries],
    GeneratedMods = [New || {_Old, New} <- Renamings],
    verify_no_original_beam_refs(Compiled, OriginalMods),
    verify_no_dynamic_internal_applies(Compiled, GeneratedMods),
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
        module_names => GeneratedMods,
        on_load => OnLoads,
        hash => Hash
    }.

module_renamings(RootMod, Root, Entries) ->
    [{Mod, generated_constituent_module_name(RootMod, Root, Mod)}
        || {Mod, _Path} <- Entries].

generated_constituent_module_name(RootMod, Root, Root) ->
    RootMod;
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
            (sanitize_device_name(Tail))/binary>>,
        utf8
    ).

%% @doc Allocate an isolated package workspace for one compile operation.
package_tmp_dir(RootMod) ->
    Unique = integer_to_list(erlang:unique_integer([monotonic, positive])),
    filename:join(
        [
            "_build",
            "tmp_devices",
            atom_to_list(RootMod) ++ "-" ++ os:getpid() ++ "-" ++ Unique
        ]
    ).

include_dirs(Copied) ->
    lists:usort(
        [
            "src",
            filename:absname("src"),
            "src/kernel",
            filename:absname("src/kernel")
        ] ++
        lists:filtermap(
            fun(#{ source := Source }) -> source_kernel_dir(Source) end,
            Copied
        )
    ).

source_kernel_dir(Source) ->
    Abs = filename:absname(binary_to_list(hb_util:bin(Source))),
    case string:str(Abs, "/src/preloaded/") of
        0 -> false;
        Pos ->
            {true,
                filename:join(
                    [string:substr(Abs, 1, Pos - 1), "src", "kernel"]
                )
            }
    end.

copy_package_source(TmpDir, Mod, Path) ->
    Target = filename:join(TmpDir, atom_to_list(Mod) ++ ".erl"),
    ok = filelib:ensure_dir(Target),
    ok = file:write_file(Target, read_file(Path)),
    {_Forms, Attrs} = parse_module(Target),
    #{ module => Mod, path => Target, source => Path, attrs => Attrs }.

package_on_loads(Renamings, Copied) ->
    lists:filtermap(
        fun({Old, New}) ->
            #{ attrs := Attrs } = find_copied_module(Old, Copied),
            case on_load_fun(Attrs) of
                undefined -> false;
                Fun ->
                    {true, #{
                        <<"module-name">> => atom_to_binary(New, utf8),
                        <<"function">> => atom_to_binary(Fun, utf8)
                    }}
            end
        end,
        Renamings
    ).

find_copied_module(Mod, Copied) ->
    [Found] = [C || C = #{ module := M } <- Copied, M =:= Mod],
    Found.

on_load_fun(Attrs) ->
    case lists:keyfind(on_load, 1, Attrs) of
        {on_load, {Fun, 0}} -> Fun;
        {on_load, [Fun, 0]} -> Fun;
        _ -> undefined
    end.

%% @doc Encode on-load callback metadata into one binary field. Keeping the
%% implementation message flat avoids nested cache links for load-time data.
encode_on_loads(OnLoads) ->
    iolist_to_binary(
        [
            begin
                ModLen = byte_size(ModBin),
                FunLen = byte_size(FunBin),
                <<ModLen:32, ModBin/binary, FunLen:32, FunBin/binary>>
            end
         ||
            #{ <<"module-name">> := ModBin,
               <<"function">> := FunBin } <- OnLoads
        ]
    ).

%% @doc Decode on-load callback metadata produced by {@link encode_on_loads/1}.
decode_on_loads(Bin) when is_binary(Bin) ->
    decode_on_loads(Bin, []).

decode_on_loads(<<>>, Acc) ->
    {ok, lists:reverse(Acc)};
decode_on_loads(<<ModLen:32, Rest0/binary>>, Acc)
        when byte_size(Rest0) >= ModLen + 4 ->
    <<ModBin:ModLen/binary, FunLen:32, Rest1/binary>> = Rest0,
    case byte_size(Rest1) >= FunLen of
        true ->
            <<FunBin:FunLen/binary, Rest2/binary>> = Rest1,
            decode_on_loads(
                Rest2,
                [#{
                    <<"module-name">> => ModBin,
                    <<"function">> => FunBin
                } | Acc]
            );
        false ->
            {error, invalid_on_load_metadata}
    end;
decode_on_loads(_Other, _Acc) ->
    {error, invalid_on_load_metadata}.

compile_renamed_modules(
    RenamedPaths,
    Copied,
    Renamings,
    OnLoads,
    IncludeDirs,
    Opts
) ->
    [
        compile_renamed_module(
            Path,
            Copied,
            Renamings,
            OnLoads,
            IncludeDirs,
            Opts
        )
      ||
        Path <- lists:sort(RenamedPaths)
    ].

compile_renamed_module(Path, Copied, Renamings, OnLoads, IncludeDirs, Opts) ->
    Mod = atom_of_file(Path),
    Source = source_for_renamed_module(Mod, Copied, Renamings),
    %% Persist `on_load' callbacks as archive metadata instead of leaving the
    %% attribute in source so the runtime can decide when and how to invoke it.
    strip_on_load(Path, on_load_funs_for(Mod, OnLoads)),
    rewrite_surviving_captures(Path, Renamings),
    restore_test_parallel_transform(Path, Opts),
    CompileOpts =
        [
            binary,
            debug_info,
            {source, binary_to_list(filename_only(Source))}
        ] ++
        test_compile_opts(Opts) ++
        [{i, Dir} || Dir <- IncludeDirs] ++ [
            nowarn_unused_function,
            nowarn_unused_vars,
            nowarn_shadow_vars,
            nowarn_export_all,
            nowarn_unused_record,
            nowarn_unused_type,
            return_errors,
            return_warnings
        ],
    case compile:file(Path, CompileOpts) of
        {ok, Mod, Beam} ->
            #{ module => Mod, source => Source, beam => Beam };
        {ok, Mod, Beam, _Warnings} ->
            #{ module => Mod, source => Source, beam => Beam };
        {error, Errors, Warnings} ->
            erlang:error(
                {device_compile_failed, Mod,
                    [{errors, Errors}, {warnings, Warnings},
                     {source_path, Path}]}
            );
        Other ->
            erlang:error({device_compile_failed, Mod, Other})
    end.

test_compile_opts(Opts) ->
    case hb_maps:get(<<"test">>, Opts, false, Opts) of
        true -> [{d, 'TEST'}];
        _ -> []
    end.

restore_test_parallel_transform(Path, Opts) ->
    case hb_maps:get(<<"test">>, Opts, false, Opts) of
        true -> inject_test_parallel_transform(Path);
        _ -> ok
    end.

inject_test_parallel_transform(Path) ->
    Source0 = read_file(Path),
    case binary:match(Source0, <<"_test_parallel">>) of
        nomatch -> ok;
        _ ->
            Attr = <<"-compile({parse_transform, hb_test_parallel}).\n">>,
            Marker = <<"-compile({parse_transform, eunit_autoexport}).">>,
            Source1 =
                binary:replace(
                    Source0,
                    Marker,
                    <<Attr/binary, Marker/binary>>
                ),
            ok = file:write_file(Path, Source1)
    end.

source_for_renamed_module(Mod, Copied, Renamings) ->
    [{Old, Mod}] = [{Old, New} || {Old, New} <- Renamings, New =:= Mod],
    #{ source := Source } = find_copied_module(Old, Copied),
    Source.

on_load_funs_for(Mod, OnLoads) ->
    [
        hb_util:key_to_atom(Fun, new_atoms)
      ||
        #{ <<"module-name">> := ModBin, <<"function">> := Fun } <- OnLoads,
        ModBin =:= atom_to_binary(Mod, utf8)
    ].

strip_on_load(_Path, []) ->
    ok;
strip_on_load(Path, Funs) ->
    Source0 = read_file(Path),
    Source1 =
        re:replace(
            Source0,
            <<"-on_load\\s*\\([^\\.]+\\)\\.\\s*\\n?">>,
            <<"">>,
            [global, {return, binary}]
        ),
    ExportLines =
        iolist_to_binary(
            [
                io_lib:format("-export([~s/0]).~n", [io_lib:write_atom(Fun)])
              ||
                Fun <- Funs
            ]
        ),
    Source2 =
        re:replace(
            Source1,
            <<"(-module\\([^)]+\\)\\.\\s*)">>,
            <<"\\1", ExportLines/binary>>,
            [{return, binary}]
        ),
    ok = file:write_file(Path, Source2).

rewrite_surviving_captures(Path, Renamings) ->
    Source0 = read_file(Path),
    Source1 =
        lists:foldl(
            fun rewrite_surviving_captures_for_module/2,
            Source0,
            Renamings
        ),
    ok = file:write_file(Path, Source1).

rewrite_surviving_captures_for_module({Old, New}, Source) ->
    OldBin = iolist_to_binary(io_lib:write_atom(Old)),
    NewBin = iolist_to_binary(io_lib:write_atom(New)),
    Pattern =
        <<"fun\\s+", OldBin/binary,
            ":('(?:[^'\\\\]|\\\\.)*'|[a-z][A-Za-z0-9_@]*)/([0-9]+)">>,
    Replacement = <<"fun ", NewBin/binary, ":\\1/\\2">>,
    re:replace(Source, Pattern, Replacement, [global, {return, binary}]).

archive_module_metadata(Compiled) ->
    [
        #{
            <<"module-name">> => atom_to_binary(Mod, utf8),
            <<"archive-path">> =>
                hb_util:bin(filename:join("ebin", atom_to_list(Mod) ++ ".beam"))
        }
      ||
        #{ module := Mod } <- Compiled
    ].

make_archive(Compiled, PrivFiles) ->
    Files =
        [
            {
                filename:join("ebin", atom_to_list(Mod) ++ ".beam"),
                Beam,
                archive_file_info(<<"ebin">>, byte_size(Beam))
            }
          ||
            #{ module := Mod, beam := Beam } <- Compiled
        ] ++ [
            {
                binary_to_list(Path),
                Body,
                archive_file_info(Path, byte_size(Body))
            }
          ||
            {Path, Body} <- lists:sort(maps:to_list(PrivFiles))
        ],
    {ok, {_, Archive}} =
        zip:create(
            "device.beams.zip",
            Files,
            [memory, {extra, []}, {uncompress, all}]
        ),
    Archive.

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

archive_file_mode(<<"priv/bin/", _/binary>>) -> 8#100755;
archive_file_mode(Path) ->
    case filename:extension(hb_util:list(Path)) of
        ".sh" -> 8#100755;
        _ -> 8#100644
    end.

verify_no_original_beam_refs(Compiled, OriginalMods) ->
    OriginalSet = sets:from_list(OriginalMods),
    Bad =
        lists:usort(
            lists:flatmap(
                fun(#{ beam := Beam }) ->
                    {beam_file, _Mod, _Exports, _Attrs, _Info, Code} =
                        beam_disasm:file(Beam),
                    find_internal_beam_refs(Code, OriginalSet, [])
                end,
                Compiled
            )
        ),
    case Bad of
        [] -> ok;
        _ -> erlang:error({unresolved_internal_calls, archive, Bad})
    end.

verify_no_dynamic_internal_applies(Compiled, GeneratedMods) ->
    GeneratedSet = sets:from_list(GeneratedMods),
    Bad =
        lists:usort(
            lists:flatmap(
                fun(#{ beam := Beam }) ->
                    {beam_file, _Mod, _Exports, _Attrs, _Info, Code} =
                        beam_disasm:file(Beam),
                    find_dynamic_internal_applies(Code, GeneratedSet)
                end,
                Compiled
            )
        ),
    case Bad of
        [] -> ok;
        _ -> erlang:error({dynamic_internal_dispatch, archive, Bad})
    end.

find_internal_beam_refs({extfunc, M, F, A}, InternalSet, Acc) ->
    case sets:is_element(M, InternalSet) of
        true -> [{M, F, A} | Acc];
        false -> Acc
    end;
find_internal_beam_refs({literal, Fun}, InternalSet, Acc)
        when is_function(Fun) ->
    case {erlang:fun_info(Fun, type), erlang:fun_info(Fun, module)} of
        {{type, external}, {module, M}} ->
            case sets:is_element(M, InternalSet) of
                true ->
                    {name, F} = erlang:fun_info(Fun, name),
                    {arity, A} = erlang:fun_info(Fun, arity),
                    [{M, F, A} | Acc];
                false ->
                    Acc
            end;
        _ ->
            Acc
    end;
find_internal_beam_refs(Tuple, InternalSet, Acc) when is_tuple(Tuple) ->
    lists:foldl(
        fun(Elem, AccIn) -> find_internal_beam_refs(Elem, InternalSet, AccIn) end,
        Acc,
        tuple_to_list(Tuple)
    );
find_internal_beam_refs(List, InternalSet, Acc) when is_list(List) ->
    lists:foldl(
        fun(Elem, AccIn) -> find_internal_beam_refs(Elem, InternalSet, AccIn) end,
        Acc,
        List
    );
find_internal_beam_refs(_Term, _InternalSet, Acc) ->
    Acc.

find_dynamic_internal_applies(Code, InternalSet) ->
    lists:flatmap(
        fun({function, F, A, _Label, Insns}) ->
            find_dynamic_internal_applies(Insns, InternalSet, F, A, [])
        end,
        Code
    ).

find_dynamic_internal_applies([], _InternalSet, _F, _A, _Prev) ->
    [];
find_dynamic_internal_applies([{apply, Arity} | Rest], InternalSet, F, A, Prev) ->
    Bad =
        case internal_apply_module(Arity, Prev, InternalSet) of
            false -> [];
            {true, Mod} -> [{dynamic_apply, Mod, F, A, Arity}]
        end,
    Bad ++ find_dynamic_internal_applies(Rest, InternalSet, F, A, []);
find_dynamic_internal_applies([Insn | Rest], InternalSet, F, A, Prev) ->
    find_dynamic_internal_applies(Rest, InternalSet, F, A, [Insn | Prev]).

internal_apply_module(Arity, Prev, InternalSet) ->
    case find_apply_module_move(Arity, Prev) of
        {ok, Mod} ->
            case sets:is_element(Mod, InternalSet) of
                true -> {true, Mod};
                false -> false
            end;
        false ->
            false
    end.

find_apply_module_move(_Arity, []) ->
    false;
find_apply_module_move(Arity, [{move, {atom, Mod}, {x, Reg}} | _])
        when Reg =:= Arity ->
    {ok, Mod};
find_apply_module_move(Arity, [{move, _Value, {x, Reg}} | _])
        when Reg =:= Arity ->
    false;
find_apply_module_move(Arity, [{put_tuple2, {x, Reg}, _} | _])
        when Reg =:= Arity ->
    false;
find_apply_module_move(Arity, [{put_list, _Head, _Tail, {x, Reg}} | _])
        when Reg =:= Arity ->
    false;
find_apply_module_move(Arity, [_ | Rest]) ->
    find_apply_module_move(Arity, Rest).

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
        on_load := OnLoad,
        requires_otp_release := OtpRel,
        requires_system_architecture := Arch
    } = Pkg,
    %% Keep the implementation message flat: archive bytes and loader metadata
    %% stay at top level so stores can serve them directly without rebuilding
    %% nested submessages first.
    Base = #{
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => ?VARIANT,
        <<"content-type">> => ?ARCHIVE_CONTENT_TYPE,
        <<"archive-format">> => <<"zip">>,
        <<"implements-device">> => SpecID,
        <<"module-name">> => atom_to_binary(ModName, utf8),
        <<"requires-otp-release">> => OtpRel,
        <<"requires-system-architecture">> => Arch,
        <<"body">> => Archive
    },
    case OnLoad of
        [] -> Base;
        _ ->
            Base#{
                <<"on-load-format">> => ?ON_LOAD_FORMAT,
                <<"on-load">> => encode_on_loads(OnLoad)
            }
    end.

%%% --------------------------------------------------------------------
%%% Tests
%%% --------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Build a temporary source directory with a minimal device root and
%% one helper module, then exercise the full scan/package pipeline.
test_fixture_dir() ->
    Tmp = filename:join(["/tmp",
        "hb_packager_test_" ++ integer_to_list(erlang:system_time())]),
    ok = filelib:ensure_dir(filename:join(Tmp, ".keep")),
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
        filename:join(Tmp, "dev_test_pkg.erl"), Root),
    ok = file:write_file(
        filename:join(Tmp, "dev_test_pkg_helper.erl"), Helper),
    Tmp.

dynamic_dispatch_fixture_dir() ->
    Tmp = filename:join(["/tmp",
        "hb_packager_dynamic_test_" ++ integer_to_list(erlang:system_time())]),
    ok = filelib:ensure_dir(filename:join(Tmp, ".keep")),
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
        filename:join(Tmp, "dev_dyn_pkg.erl"), Root),
    ok = file:write_file(
        filename:join(Tmp, "dev_dyn_pkg_helper.erl"), Helper),
    Tmp.

on_load_fixture_dir() ->
    Tmp = filename:join(["/tmp",
        "hb_packager_on_load_test_" ++ integer_to_list(erlang:system_time())]),
    ok = filelib:ensure_dir(filename:join(Tmp, ".keep")),
    Root = <<
        "-module(dev_on_load_pkg).\n"
        "-on_load(init/0).\n"
        "-export([status/3]).\n"
        "\n"
        "init() ->\n"
        "    persistent_term:put(hb_packager_on_load_test, true),\n"
        "    ok.\n"
        "status(_Base, _Req, _Opts) -> {ok, true}.\n"
    >>,
    ok = file:write_file(
        filename:join(Tmp, "dev_on_load_pkg.erl"), Root),
    Tmp.

priv_fixture_dir() ->
    Tmp = test_fixture_dir(),
    PrivDir = filename:join([Tmp, "priv", "dev_test_pkg"]),
    ok = filelib:ensure_dir(filename:join([PrivDir, "bin", "tool"])),
    ok = file:write_file(filename:join([PrivDir, "bin", "tool"]),
        <<"#!/bin/sh\n">>),
    ok = filelib:ensure_dir(filename:join([PrivDir, "share", "data"])),
    ok = file:write_file(filename:join([PrivDir, "share", "data"]),
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
    Tmp = filename:join(["/tmp",
        "hb_packager_transitive_test_" ++
            integer_to_list(erlang:system_time())]),
    ok = filelib:ensure_dir(filename:join(Tmp, ".keep")),
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
        filename:join(Dir, binary_to_list(<<Name/binary, ".erl">>)),
        <<"-module(", Name/binary, ").\n", Body/binary>>
    ).

generated_module_name_pattern_test() ->
    Hash = base32_lower(crypto:hash(sha256, <<"abc">>)),
    Mod = generated_module_name(<<"message@1.0">>, Hash),
    Bin = atom_to_binary(Mod, utf8),
    ?assertMatch(<<"_hb_device_message_1_0_", _/binary>>, Bin),
    ?assert(is_generated_module(Mod)),
    ?assertMatch({<<"message_1_0">>, _}, generated_module_parts(Mod)).

base32_lower_known_vector_test() ->
    %% RFC 4648 §10 vectors, lowercase, unpadded.
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
    ?assert(is_generated_module(Mod)),
    ok = load_pkg_archive(Pkg),
    Exports = lists:sort(Mod:module_info(exports)),
    %% Root exports plus module_info.
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

pure_on_load_metadata_is_flat_and_runnable_test() ->
    persistent_term:erase(hb_packager_on_load_test),
    Dir = on_load_fixture_dir(),
    [Group] = scan([Dir], #{}),
    Pkg = package_for_test(Group),
    Msg = impl_message(Pkg, <<"spec-id">>, #{}),
    ?assert(is_binary(maps:get(<<"on-load">>, Msg))),
    ?assertEqual(?ON_LOAD_FORMAT, maps:get(<<"on-load-format">>, Msg)),
    {ok, OnLoads} = decode_on_loads(maps:get(<<"on-load">>, Msg)),
    ?assertEqual(maps:get(on_load, Pkg), OnLoads),
    ok = load_pkg_archive(Pkg),
    ok = run_pkg_on_loads(OnLoads),
    ?assertEqual(true, persistent_term:get(hb_packager_on_load_test)).

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
    %% No `-implements' attribute, so derived from module name.
    ?assertEqual(<<"test-pkg@1.0">>, maps:get(device_name, Pkg)).

hash_changes_with_content_test() ->
    Dir = test_fixture_dir(),
    [Group] = scan([Dir], #{}),
    Pkg1 = package_for_test(Group),
    Hash1 = maps:get(hash, Pkg1),
    %% Mutate the helper file slightly and re-scan.
    HelperPath = filename:join(Dir, "dev_test_pkg_helper.erl"),
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
                {Mod, binary_to_list(Path), maps:get(Path, Beams)}
            end
          ||
            Meta <- maps:get(archive_modules, Pkg)
        ],
    code:atomic_load(Modules).

run_pkg_on_loads([]) ->
    ok;
run_pkg_on_loads([#{ <<"module-name">> := ModBin,
                     <<"function">> := FunBin } | Rest]) ->
    Mod = hb_util:key_to_atom(ModBin, existing),
    Fun = hb_util:key_to_atom(FunBin, existing),
    ok = apply(Mod, Fun, []),
    run_pkg_on_loads(Rest).

-endif.
