%%% @doc HyperBEAM device packager.
%%%
%%% This module turns a namespace of `dev_<name>'/`dev_<name>_*' Erlang
%%% source files into a single, deterministically-named, debug-info BEAM
%%% module: `_hb_device_<sanitized-name>_<hash>'.
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
%%% or a custom file) and its _implementation_ (the packaged BEAM).
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
-export([sign/3]).
-export([sanitize_device_name/1, generated_module_name/2]).
-export([is_generated_module/1, generated_module_parts/1]).
-export([base32_lower/1, bootstrap_device_map/0]).
-ifdef(TEST).
-export([test_fixture_dir/0]).
-endif.

-include("include/hb.hrl").

-define(VARIANT, <<"ao.N.1">>).
-define(DEFAULT_DEVICE_VERSION, <<"@1.0">>).
-define(GENERATED_MOD_PREFIX, <<"_hb_device_">>).

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
    Roots0 = hb_maps:get(<<"device-roots">>, Opts, all, Opts),
    Files = lists:flatmap(fun list_dev_files/1, Dirs),
    %% Sort once so grouping is deterministic.
    Sorted = lists:keysort(1, Files),
    Names = [Name || {Name, _Path} <- Sorted],
    %% Modules that declare `-implements(...)' are unambiguously
    %% roots — they cannot be folded into another namespace as
    %% helpers regardless of how their atom name happens to look.
    ForcedRoots = sets:from_list(
        [N || {N, P} <- Sorted, file_has_implements(P)]
    ),
    Groups = group_by_namespace(Sorted, Names, ForcedRoots),
    case Roots0 of
        all -> Groups;
        Filter when is_list(Filter) ->
            FilterAtoms = [hb_util:key_to_atom(F, new_atoms) || F <- Filter],
            [G || G = #{ root := R } <- Groups, lists:member(R, FilterAtoms)]
    end;
scan(Dir, Opts) when is_binary(Dir) orelse is_list(Dir) ->
    scan([Dir], Opts).

%% @doc Recursively list `dev_*.erl' files in a directory.
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
group_by_namespace(Files, Names, ForcedRoots) ->
    NameSet = sets:from_list(Names),
    %% Build {Root, [Helper, ...]} pairs.
    {Roots, Assignments} =
        lists:foldl(
            fun({Mod, _Path}, {RAcc, AAcc}) ->
                case sets:is_element(Mod, ForcedRoots) of
                    true ->
                        {[Mod | RAcc], AAcc};
                    false ->
                        case longest_root_prefix(Mod, NameSet) of
                            Mod ->
                                {[Mod | RAcc], AAcc};
                            Root ->
                                {RAcc, [{Root, Mod} | AAcc]}
                        end
                end
            end,
            {[], []},
            Files
        ),
    SortedRoots = lists:sort(Roots),
    FilesMap = maps:from_list(Files),
    [
        begin
            Helpers =
                lists:sort(
                    [H || {R, H} <- Assignments, R =:= Root]
                ),
            #{
                root => Root,
                root_file => maps:get(Root, FilesMap),
                helpers =>
                    [{H, maps:get(H, FilesMap)} || H <- Helpers],
                files =>
                    maps:from_list(
                        [
                            {filename_only(maps:get(M, FilesMap)),
                                read_file(maps:get(M, FilesMap))}
                          ||
                            M <- [Root | Helpers]
                        ]
                    )
            }
        end
      ||
        Root <- SortedRoots
    ].

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

%% @doc Package every device returned by {@link scan/2}.
package_all(Groups, Opts) ->
    [package(G, Opts) || G <- Groups].

%% @doc Package one device group. Returns a map containing the generated
%% module name, BEAM, source, declared `implements' name (if present),
%% and metadata used to construct the spec/implementation messages.
package(#{ root := Root, root_file := RootFile, helpers := Helpers, files := Files }, Opts) ->
    {_RootForms, RootAttrs} = parse_module(RootFile),
    reject_nif_loading_modules([{Root, RootFile} | Helpers]),
    Implements = derived_or_declared_implements(Root, RootAttrs),
    {SpecBody, SpecContentType} = derive_spec(RootFile, RootAttrs, Opts),
    %% Hash the canonical file set as an unsigned AO-Core message ID.
    SourceID = source_id(Files, Opts),
    Hash = source_id_to_hash(SourceID),
    ModName = generated_module_name(Implements, Hash),
    ?event(packager, {packaging, {root, Root}, {hash, Hash}, {mod, ModName}}),
    %% Merge sources into a single BEAM via Igor's file-based merger.
    %% Igor handles preprocessor expansion itself, so we hand it the
    %% absolute paths of the root + helper sources.
    HelperFiles = [hb_util:bin(P) || {_, P} <- Helpers],
    SourcePaths = [
        binary_to_list(filename:absname(hb_util:bin(F)))
      ||
        F <- [RootFile | HelperFiles]
    ],
    Beam = compile_merged(ModName, Root, SourcePaths, Opts),
    Exports = root_exports(RootAttrs),
    #{
        module_name => ModName,
        device_name => Implements,
        hash => Hash,
        source_id => SourceID,
        beam => Beam,
        spec_body => SpecBody,
        spec_content_type => SpecContentType,
        implements => declared_implements(RootAttrs),
        exports => Exports,
        requires_otp_release =>
            hb_util:bin(erlang:system_info(otp_release)),
        root_module => Root,
        helpers => [H || {H, _} <- Helpers],
        files => Files
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

reject_nif_loading_modules(Modules) ->
    lists:foreach(
        fun({Mod, Path}) ->
            Source = read_file(Path),
            case is_nif_loading_source(Source) of
                true -> erlang:error({nif_loading_device_module, Mod});
                false -> ok
            end
        end,
        Modules
    ).

is_nif_loading_source(Source) ->
    nomatch =/= binary:match(Source, <<"erlang:load_nif">>) orelse
        nomatch =/= binary:match(Source, <<"load_nif(">>) orelse
        nomatch =/= binary:match(Source, <<"load_nif_from_crate">>).

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

%% @doc Compute the canonical package ID of a device's file set. The source
%% set is represented as a normal AO-Core message whose keys are bare source
%% filenames and whose values are the complete file contents.
source_id(FilesMap, Opts) ->
    SourceMsg = maps:from_list(lists:sort(maps:to_list(FilesMap))),
    hb_message:id(SourceMsg, unsigned, source_id_opts(Opts)).

source_id_to_hash(SourceID) ->
    base32_lower(hb_util:native_id(SourceID)).

source_id_opts(Opts) ->
    case hb_opts:get(device_bootstrap, undefined, Opts) of
        Map when is_map(Map) -> Opts;
        _ -> Opts#{ <<"device-bootstrap">> => bootstrap_device_map() }
    end.

%% @doc The build-time source modules needed before a preloaded-store exists.
%% Runtime node opts should not set `<<"device-bootstrap">>'.
bootstrap_device_map() ->
    #{
        <<"message@1.0">> => dev_message,
        <<"httpsig@1.0">> => dev_httpsig,
        <<"structured@1.0">> => dev_structured,
        <<"ans104@1.0">> => dev_ans104,
        <<"flat@1.0">> => dev_flat,
        <<"json@1.0">> => dev_json,
        <<"tx@1.0">> => dev_tx
    }.

%% @doc Encode bytes as lowercase, unpadded base32 (RFC 4648 alphabet).
base32_lower(Bin) when is_binary(Bin) ->
    Encoded = base32_encode_lower(Bin, <<>>),
    Encoded.

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
            case binary:split(Rest, <<"_">>, [global]) of
                Parts when length(Parts) >= 2 ->
                    [Hash | RevName] = lists:reverse(Parts),
                    Name =
                        iolist_to_binary(
                            lists:join(<<"_">>, lists:reverse(RevName))
                        ),
                    {Name, Hash};
                _ -> not_generated
            end;
        _ -> not_generated
    end;
generated_module_parts(_) -> not_generated.

%%% --------------------------------------------------------------------
%%% Igor merge + compile
%%% --------------------------------------------------------------------

%% Igor's file-based merger does its own preprocess pass per input.
%% That way `-include', `-include_lib', `-ifdef', and `-define' all
%% expand to a single canonical form in the merged tree, and there
%% is one set of `-record(...)' declarations rather than one per
%% input file.
compile_merged(ModName, Root, Files, _Opts) ->
    InternalMods = [atom_of_file(F) || F <- Files],
    IgorOpts = [
        no_imports,
        {comments, false},
        {notes, no},
        {file_attributes, no},
        {tidy, false},
        {preprocess, true},
        {includes, [
            "src",
            filename:absname("src"),
            "src/kernel",
            filename:absname("src/kernel")
        ]},
        {export, [Root]}
    ],
    {Tree, _Stubs} = igor:merge_files(ModName, Files, IgorOpts),
    Pretty = erl_prettypr:format(Tree, [{ribbon, 80}, {paper, 100}]),
    Source = unicode:characters_to_binary(Pretty),
    SourceWithModule = rename_in_source(Source, ModName),
    SourceWithCaptures = rewrite_internal_captures(SourceWithModule, InternalMods),
    do_compile_source(SourceWithCaptures, ModName, InternalMods).

%% Rewrite the `-module(...).' attribute in a piece of pretty-printed
%% Erlang source so the compiled BEAM lands at our generated atom.
%% Atoms beginning with `_' need explicit single-quote delimiters so
%% the parser does not treat them as variable patterns.
rename_in_source(Source, NewName) ->
    Quoted = io_lib:write_atom(NewName),
    Replacement =
        iolist_to_binary(
            io_lib:format("-module(~s)", [Quoted])
        ),
    re:replace(
        Source,
        <<"-module\\([^)]+\\)">>,
        Replacement,
        [{return, binary}]
    ).

rewrite_internal_captures(Source, InternalMods) ->
    lists:foldl(
        fun rewrite_internal_captures_for_module/2,
        Source,
        InternalMods
    ).

rewrite_internal_captures_for_module(Mod, Source) ->
    ModBin = iolist_to_binary(io_lib:write_atom(Mod)),
    Pattern =
        <<"fun\\s+", ModBin/binary,
            ":('(?:[^'\\\\]|\\\\.)*'|[a-z][A-Za-z0-9_@]*)/([0-9]+)">>,
    re:replace(Source, Pattern, <<"fun \\1/\\2">>, [global, {return, binary}]).

%% Compile a pretty-printed source into a BEAM. We dump the source to
%% a temp .erl file so the standard compiler driver handles
%% `-define', `-include', `-ifdef' resolution. Returns the BEAM bytes.
%% Warnings that would otherwise be promoted to errors (unused
%% functions, etc.) are tolerated — the merged module legitimately
%% contains a superset of helper code that may not all be reachable
%% via the root's exports.
do_compile_source(Source, ModName, InternalMods) ->
    Tmp = filename:join(
        ["_build", "tmp_devices", atom_to_list(ModName)]
    ),
    SourcePath = Tmp ++ ".erl",
    BeamPath = Tmp ++ ".beam",
    ok = filelib:ensure_dir(SourcePath),
    ok = file:write_file(SourcePath, Source),
    Opts = [
        {outdir, filename:dirname(BeamPath)},
        debug_info,
        %% Allow the merged module to find its `-include' and
        %% `-include_lib' targets (`include/hb.hrl', etc).  The kernel
        %% directory is the canonical home for those headers.
        {i, "src"},
        {i, filename:absname("src")},
        {i, "src/kernel"},
        {i, filename:absname("src/kernel")},
        nowarn_unused_function,
        nowarn_unused_vars,
        nowarn_shadow_vars,
        nowarn_export_all,
        nowarn_unused_record,
        nowarn_unused_type,
        return_errors,
        return_warnings
    ],
    case compile:file(SourcePath, Opts) of
        {ok, _ModName} ->
            {ok, Beam} = file:read_file(BeamPath),
            verify_no_internal_beam_refs(BeamPath, ModName, InternalMods),
            Beam;
        {ok, _ModName, _Warnings} ->
            {ok, Beam} = file:read_file(BeamPath),
            verify_no_internal_beam_refs(BeamPath, ModName, InternalMods),
            Beam;
        {error, Errors, Warnings} ->
            erlang:error(
                {device_compile_failed, ModName,
                    [{errors, Errors}, {warnings, Warnings},
                     {source_path, SourcePath}]}
            );
        Other ->
            erlang:error({device_compile_failed, ModName, Other})
    end.

verify_no_internal_beam_refs(BeamPath, ModName, InternalMods) ->
    InternalSet = sets:from_list(InternalMods),
    {beam_file, _ModName, _Exports, _Attrs, _Info, Code} =
        beam_disasm:file(BeamPath),
    Bad = lists:usort(
        find_internal_beam_refs(Code, InternalSet, []) ++
            find_dynamic_internal_applies(Code, InternalSet)
    ),
    case Bad of
        [] ->
            ok;
        _ ->
            erlang:error({unresolved_internal_calls, ModName, Bad})
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
%% implements; it is written into the implementation message as the
%% `implements-device' key.
impl_message(Pkg, SpecID, _Opts) ->
    #{
        module_name := ModName,
        beam := Beam,
        requires_otp_release := OtpRel
    } = Pkg,
    #{
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => ?VARIANT,
        <<"content-type">> => <<"application/beam">>,
        <<"implements-device">> => SpecID,
        <<"module-name">> => atom_to_binary(ModName, utf8),
        <<"requires-otp-release">> => OtpRel,
        <<"body">> => Beam
    }.

%% @doc Sign an unsigned message using the configured commitment device
%% and a node wallet at `Opts/priv-wallet'.
sign(Msg, Wallet, Opts) ->
    Local = Opts#{ <<"priv-wallet">> => Wallet },
    hb_message:commit(Msg, Local).

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
    Root = <<
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

nif_fixture_dir() ->
    Tmp = filename:join(["/tmp",
        "hb_packager_nif_test_" ++ integer_to_list(erlang:system_time())]),
    ok = filelib:ensure_dir(filename:join(Tmp, ".keep")),
    Root = <<
        "-module(dev_nif_pkg).\n"
        "-export([call/3]).\n"
        "\n"
        "call(_Base, _Req, _Opts) -> {ok, dev_nif_pkg_helper:loaded()}.\n"
    >>,
    Helper = <<
        "-module(dev_nif_pkg_helper).\n"
        "-export([loaded/0]).\n"
        "-on_load(init/0).\n"
        "\n"
        "init() -> erlang:load_nif(\"/tmp/nope\", 0).\n"
        "loaded() -> false.\n"
    >>,
    ok = file:write_file(
        filename:join(Tmp, "dev_nif_pkg.erl"), Root),
    ok = file:write_file(
        filename:join(Tmp, "dev_nif_pkg_helper.erl"), Helper),
    Tmp.

scan_groups_root_with_helper_test() ->
    Dir = test_fixture_dir(),
    Groups = scan([Dir], #{}),
    ?assertMatch([_], Groups),
    [#{ root := Root, helpers := Helpers, files := Files }] = Groups,
    ?assertEqual(dev_test_pkg, Root),
    ?assertMatch([{dev_test_pkg_helper, _}], Helpers),
    ?assertEqual(2, map_size(Files)).

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
    Pkg = package(Group, #{}),
    Mod = maps:get(module_name, Pkg),
    ?assert(is_generated_module(Mod)),
    Beam = maps:get(beam, Pkg),
    %% Load and inspect.
    {module, Mod} = code:load_binary(Mod,
        atom_to_list(Mod) ++ ".beam", Beam),
    Exports = lists:sort(Mod:module_info(exports)),
    %% Root exports plus module_info.
    ?assert(lists:member({echo, 3}, Exports)),
    ?assert(lists:member({hello, 3}, Exports)),
    ?assert(lists:member({hello_via_capture, 3}, Exports)),
    %% Helper export greet/2 must NOT be exposed.
    ?assertNot(lists:member({greet, 2}, Exports)).

package_helper_not_loaded_separately_test() ->
    Dir = test_fixture_dir(),
    [Group] = scan([Dir], #{}),
    Pkg = package(Group, #{}),
    Mod = maps:get(module_name, Pkg),
    Beam = maps:get(beam, Pkg),
    %% Ensure helper isn't loaded yet.
    code:purge(dev_test_pkg_helper),
    code:delete(dev_test_pkg_helper),
    {module, Mod} = code:load_binary(Mod,
        atom_to_list(Mod) ++ ".beam", Beam),
    {ok, Greeting} = Mod:hello(#{}, #{}, #{}),
    ?assertEqual(<<"hello">>, Greeting),
    {ok, CapturedGreeting} = Mod:hello_via_capture(#{}, #{}, #{}),
    ?assertEqual(<<"hello">>, CapturedGreeting),
    %% Helper should still NOT be loaded.
    ?assertEqual(false, code:is_loaded(dev_test_pkg_helper)).

dynamic_internal_dispatch_rejected_test() ->
    Dir = dynamic_dispatch_fixture_dir(),
    [Group] = scan([Dir], #{}),
    ?assertError({unresolved_internal_calls, _, _}, package(Group, #{})).

nif_loading_modules_rejected_test() ->
    Dir = nif_fixture_dir(),
    [Group] = scan([Dir], #{}),
    ?assertError({nif_loading_device_module, dev_nif_pkg_helper},
        package(Group, #{})).

derived_implements_uses_module_name_test() ->
    Dir = test_fixture_dir(),
    [Group] = scan([Dir], #{}),
    Pkg = package(Group, #{}),
    %% No `-implements' attribute, so derived from module name.
    ?assertEqual(<<"test-pkg@1.0">>, maps:get(device_name, Pkg)).

hash_changes_with_content_test() ->
    Dir = test_fixture_dir(),
    [Group] = scan([Dir], #{}),
    Pkg1 = package(Group, #{}),
    Hash1 = maps:get(hash, Pkg1),
    %% Mutate the helper file slightly and re-scan.
    HelperPath = filename:join(Dir, "dev_test_pkg_helper.erl"),
    {ok, Old} = file:read_file(HelperPath),
    ok = file:write_file(HelperPath,
        <<Old/binary, "%% noise\n">>),
    [Group2] = scan([Dir], #{}),
    Pkg2 = package(Group2, #{}),
    Hash2 = maps:get(hash, Pkg2),
    ?assertNotEqual(Hash1, Hash2).

package_hash_is_source_message_id_test() ->
    Dir = test_fixture_dir(),
    [Group = #{ files := Files }] = scan([Dir], #{}),
    Pkg = package(Group, #{}),
    SourceID = hb_message:id(
        maps:from_list(lists:sort(maps:to_list(Files))),
        unsigned,
        source_id_opts(#{})
    ),
    ?assertEqual(SourceID, maps:get(source_id, Pkg)),
    ?assertEqual(source_id_to_hash(SourceID), maps:get(hash, Pkg)).

-endif.
