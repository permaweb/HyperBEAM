%%% @doc A self-contained codec for device implementation archives.
%%%
%%% A device implementation is a group of compiled Erlang modules plus
%%% the on-disk data those modules depend on. This module is the only
%%% place that defines, produces, verifies, and loads that encoding. It
%%% has no opinion about trust, signing, AO-Core message wrapping, or
%%% where on disk resources should ultimately live -- those are the
%%% caller's concerns. It only deals in archive bytes, modules, and
%%% files.
%%%
%%% == Format ==
%%%
%%% The archive is a deterministic, uncompressed ZIP (fixed entry
%%% metadata, sorted modules and resources) so the same inputs always produce
%%% byte-identical output:
%%%
%%% <ul>
%%%   <li>`ebin/&lt;module&gt;.beam' -- one entry per compiled module;
%%%       the file name must match the module's own declared name.</li>
%%%   <li>`priv/&lt;path&gt;' -- arbitrary resource files the modules
%%%       depend on at runtime; `&lt;path&gt;' may not escape `priv/'.</li>
%%% </ul>
%%%
%%% == Namespace ==
%%%
%%% Every module in the archive is named with the `_hb_device_' prefix.
%%% Exactly one module is the <em>root</em> (the prefixed name with no
%%% `__' helper separator); every other module's name begins with
%%% `&lt;root&gt;__'. This is a structural part of the format, and it is
%%% also a security boundary: a verified archive can only define modules
%%% in its own device namespace, never shadow a kernel or application
%%% module.
%%%
%%% == Interface ==
%%%
%%% <ul>
%%%   <li>{@link create/2}    modules + files -&gt; archive bytes</li>
%%%   <li>{@link contents/1}  archive bytes -&gt; modules + files</li>
%%%   <li>{@link verify/1}    structural + namespace check; -&gt; root</li>
%%%   <li>{@link load/2}      verify, extract files, load modules</li>
%%% </ul>
-module(hb_device_archive).
-export([create/2, module_metadata/1, contents/1, verify/1]).
-export([load/2, loaded/1]).
-include_lib("kernel/include/file.hrl").

-define(PREFIX, <<"_hb_device_">>).
-define(EBIN, <<"ebin/">>).
-define(PRIV, <<"priv/">>).

%%% --------------------------------------------------------------------
%%% Produce
%%% --------------------------------------------------------------------

%% @doc Build a deterministic archive from `[{Module, Beam}]' and a
%% `#{ RelPath => Body }' map of priv resources.
create(Modules, PrivFiles) ->
    Entries =
        [beam_entry(Mod, Beam) || {Mod, Beam} <- lists:keysort(1, Modules)]
        ++ [
            priv_entry(Rel, Body)
          ||
            {Rel, Body} <- lists:sort(maps:to_list(PrivFiles))
        ],
    {ok, {_, Archive}} =
        zip:create(
            <<"device.zip">>,
            Entries,
            [memory, {extra, []}, {uncompress, all}]
        ),
    Archive.

%% @doc Describe the BEAM members of an archive, for callers that build
%% an AO-Core implementation message around it.
module_metadata(Modules) ->
    [
        #{
            <<"module-name">> => atom_to_binary(Mod, utf8),
            <<"archive-path">> => beam_path(Mod)
        }
     ||
        {Mod, _Beam} <- lists:keysort(1, Modules)
    ].

beam_entry(Mod, Beam) ->
    {hb_util:list(beam_path(Mod)), Beam, file_info(8#100644, byte_size(Beam))}.

priv_entry(Rel, Body) ->
    Path = priv_path(Rel),
    {hb_util:list(Path), Body, file_info(resource_mode(Rel), byte_size(Body))}.

beam_path(Mod) ->
    <<?EBIN/binary, (atom_to_binary(Mod, utf8))/binary, ".beam">>.

priv_path(Rel) ->
    <<?PRIV/binary, (hb_util:bin(Rel))/binary>>.

%% @doc Fixed ZIP metadata so identical inputs yield identical bytes.
file_info(Mode, Size) ->
    Epoch = {{1980, 1, 1}, {0, 0, 0}},
    #file_info{
        size = Size,
        type = regular,
        access = read,
        atime = Epoch,
        mtime = Epoch,
        ctime = Epoch,
        mode = Mode
    }.

%% @doc `priv/bin/*' and `*.sh' resources are marked executable.
resource_mode(<<"bin/", _/binary>>) -> 8#100755;
resource_mode(Rel) ->
    case filename:extension(hb_util:list(Rel)) of
        ".sh" -> 8#100755;
        _ -> 8#100644
    end.

%%% --------------------------------------------------------------------
%%% Decode
%%% --------------------------------------------------------------------

%% @doc Extract an archive into `{ok, Modules, Resources}' where
%% `Modules' is `[{Module, FileName, Beam}]' and `Resources' is
%% `[{RelPath, Body}]'. Performs structural checks only (valid ZIP,
%% valid BEAMs whose name matches their path, safe resource paths, no
%% duplicates); namespace policy is applied by {@link verify/1}.
contents(Archive) ->
    case zip:unzip(Archive, [memory]) of
        {ok, Files} -> classify(Files, [], []);
        {error, Reason} -> {error, {archive_extract_failed, Reason}}
    end.

classify([], Mods, Res) ->
    case {duplicates([M || {M, _, _} <- Mods]),
          duplicates([P || {P, _} <- Res])} of
        {[], []} -> {ok, lists:reverse(Mods), lists:reverse(Res)};
        {[_ | _], _} -> {error, duplicate_archive_module};
        {_, [_ | _]} -> {error, duplicate_archive_file}
    end;
classify([{Name0, Body} | Rest], Mods, Res) ->
    case hb_util:bin(Name0) of
        <<"ebin/", _/binary>> = Path ->
            case beam_member(Path, Body) of
                {ok, Mod} ->
                    classify(
                        Rest,
                        [{Mod, binary_to_list(Path), Body} | Mods],
                        Res
                    );
                {error, _} = Error ->
                    Error
            end;
        <<"priv/", Rel/binary>> ->
            case safe_resource(Rel) of
                ok -> classify(Rest, Mods, [{Rel, Body} | Res]);
                {error, _} = Error -> Error
            end;
        Path ->
            {error, {unsupported_archive_path, Path}}
    end.

%% @doc A BEAM member must be a valid BEAM whose declared module name
%% matches its archive path. Naming policy is checked in `verify/1'.
beam_member(Path, Beam) ->
    case beam_lib:chunks(Beam, []) of
        {ok, {Mod, _}} ->
            Expected =
                <<?EBIN/binary, (atom_to_binary(Mod, utf8))/binary, ".beam">>,
            case Path of
                Expected -> {ok, Mod};
                _ -> {error, {archive_path_mismatch, Path, Expected}}
            end;
        {error, beam_lib, Reason} ->
            {error, {invalid_beam, Path, Reason}}
    end.

duplicates(List) -> List -- lists:usort(List).

%% @doc Reject resource paths that could escape the extraction dir.
safe_resource(<<>>) ->
    {error, empty_resource_path};
safe_resource(Rel) ->
    Parts = binary:split(Rel, <<"/">>, [global]),
    case binary:match(Rel, <<"\\">>) =/= nomatch
        orelse lists:any(fun unsafe_part/1, Parts)
    of
        true -> {error, {unsafe_resource_path, Rel}};
        false -> ok
    end.

unsafe_part(<<>>) -> true;
unsafe_part(<<".">>) -> true;
unsafe_part(<<"..">>) -> true;
unsafe_part(_) -> false.

%%% --------------------------------------------------------------------
%%% Verify
%%% --------------------------------------------------------------------

%% @doc Structurally decode the archive and check the device namespace:
%% every module is `_hb_device_*', exactly one is the root, and every
%% other module lives under `<root>__'. Returns the root module.
verify(Archive) ->
    case contents(Archive) of
        {ok, Mods, _Res} -> verify_namespace([M || {M, _, _} <- Mods]);
        {error, _} = Error -> Error
    end.

verify_namespace([]) ->
    {error, archive_has_no_modules};
verify_namespace(Modules) ->
    case [M || M <- Modules, not is_device_module(M)] of
        [Bad | _] ->
            {error, {non_device_module, Bad}};
        [] ->
            case [M || M <- Modules, is_root(M)] of
                [Root] -> verify_helpers(Root, Modules);
                [] -> {error, archive_missing_root};
                Roots -> {error, {multiple_roots, lists:sort(Roots)}}
            end
    end.

verify_helpers(Root, Modules) ->
    Prefix = <<(atom_to_binary(Root, utf8))/binary, "__">>,
    case [
        M
      ||
        M <- Modules,
        M =/= Root,
        not has_prefix(atom_to_binary(M, utf8), Prefix)
    ] of
        [] -> {ok, Root};
        Outside -> {error, {modules_outside_namespace, lists:sort(Outside)}}
    end.

is_device_module(Mod) ->
    has_prefix(atom_to_binary(Mod, utf8), ?PREFIX).

%% @doc The root is the device module with no `__' helper separator.
is_root(Mod) ->
    Bin = atom_to_binary(Mod, utf8),
    has_prefix(Bin, ?PREFIX) andalso binary:match(Bin, <<"__">>) =:= nomatch.

has_prefix(Bin, Prefix) ->
    binary:match(Bin, Prefix) =:= {0, byte_size(Prefix)}.

%%% --------------------------------------------------------------------
%%% Load
%%% --------------------------------------------------------------------

%% @doc Verify the archive, extract its resources under `Dir', and load
%% its modules. Returns `{ok, Root, AllModules}'. The caller is
%% responsible for having decided the archive is trustworthy and for
%% choosing `Dir'.
load(Archive, Dir) ->
    maybe
        {ok, Mods, Res} ?= contents(Archive),
        {ok, Root} ?= verify_namespace([M || {M, _, _} <- Mods]),
        ok ?= write_resources(Dir, Res),
        ok ?= load_modules(Mods),
        {ok, Root, [M || {M, _, _} <- Mods]}
    end.

%% @doc Load each module with normal code loading, so any `-on_load'
%% callback runs with standard Erlang semantics. A device module name is
%% a hash of its source set, so a module already in the code server is
%% byte-identical and is left untouched (its `-on_load' must not run
%% twice). A partially-loaded device is acceptable: at worst a build
%% that never ran on this machine reloads slightly differently next
%% time -- not a case worth guarding against.
load_modules(Modules) ->
    lists:foldl(fun load_one/2, ok, Modules).

load_one(_Module, {error, _} = Error) ->
    Error;
load_one({Mod, File, Beam}, ok) ->
    case code:is_loaded(Mod) of
        false ->
            case code:load_binary(Mod, File, Beam) of
                {module, Mod} -> ok;
                {error, Reason} -> {error, {Mod, Reason}}
            end;
        _ ->
            ok
    end.

%% @doc Whether every module of a decoded archive is already loaded.
loaded(Modules) ->
    lists:all(fun({Mod, _, _}) -> code:is_loaded(Mod) =/= false end, Modules).

write_resources(_Dir, []) ->
    ok;
write_resources(Dir, [{Rel, Body} | Rest]) ->
    Path = filename:join(hb_util:list(Dir), hb_util:list(Rel)),
    case filelib:ensure_dir(Path) of
        ok ->
            case file:write_file(Path, Body) of
                ok ->
                    case maybe_make_executable(Rel, Path) of
                        ok -> write_resources(Dir, Rest);
                        {error, Reason} ->
                            {error, {resource_mode_failed, Rel, Reason}}
                    end;
                {error, Reason} ->
                    {error, {resource_write_failed, Rel, Reason}}
            end;
        {error, Reason} ->
            {error, {resource_dir_failed, Rel, Reason}}
    end.

maybe_make_executable(Rel, Path) ->
    case resource_mode(Rel) of
        8#100755 -> file:change_mode(Path, 8#100755);
        _ -> ok
    end.

%%% --------------------------------------------------------------------
%%% Tests
%%% --------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Compile a throwaway module to a real BEAM binary.
beam(NameBin) ->
    beam(NameBin, <<>>).
beam(NameBin, Extra) ->
    Dir = "/tmp/hb_device_archive_test",
    ok = filelib:ensure_dir(Dir ++ "/x"),
    File = Dir ++ "/" ++ binary_to_list(NameBin) ++ ".erl",
    Src =
        iolist_to_binary([
            % `_hb_device_*' atoms start with `_', so they must be
            % quoted in source.
            "-module('", NameBin, "').\n",
            "-export([ping/0]).\n",
            "ping() -> pong.\n",
            Extra
        ]),
    ok = file:write_file(File, Src),
    {ok, Mod, Bin} = compile:file(File, [binary, debug_info, return_errors]),
    {Mod, Bin}.

device_archive(Modules) ->
    device_archive(Modules, #{}).
device_archive(Modules, Priv) ->
    create([beam(N) || N <- Modules], Priv).

create_is_deterministic_test() ->
    {M, B} = beam(<<"_hb_device_det_1">>),
    A1 = create([{M, B}], #{ <<"data/x">> => <<"hi">> }),
    A2 = create([{M, B}], #{ <<"data/x">> => <<"hi">> }),
    ?assertEqual(A1, A2),
    ?assert(is_binary(A1)).

create_sorts_modules_test() ->
    {Root, RootBeam} = beam(<<"_hb_device_sort_1">>),
    {Helper, HelperBeam} = beam(<<"_hb_device_sort_1__h">>),
    A1 = create([{Root, RootBeam}, {Helper, HelperBeam}], #{}),
    A2 = create([{Helper, HelperBeam}, {Root, RootBeam}], #{}),
    ?assertEqual(A1, A2),
    ?assertEqual(
        [
            #{
                <<"module-name">> => <<"_hb_device_sort_1">>,
                <<"archive-path">> => <<"ebin/_hb_device_sort_1.beam">>
            },
            #{
                <<"module-name">> => <<"_hb_device_sort_1__h">>,
                <<"archive-path">> => <<"ebin/_hb_device_sort_1__h.beam">>
            }
        ],
        module_metadata([{Helper, HelperBeam}, {Root, RootBeam}])
    ).

roundtrip_test() ->
    A = device_archive(
        [<<"_hb_device_rt_1">>, <<"_hb_device_rt_1__h">>],
        #{ <<"d/f">> => <<"data">> }
    ),
    {ok, Mods, Res} = contents(A),
    ?assertEqual(
        ['_hb_device_rt_1', '_hb_device_rt_1__h'],
        lists:sort([M || {M, _, _} <- Mods])
    ),
    ?assertEqual([{<<"d/f">>, <<"data">>}], Res).

verify_ok_returns_root_test() ->
    A = device_archive([<<"_hb_device_v_1">>, <<"_hb_device_v_1__a">>]),
    ?assertEqual({ok, '_hb_device_v_1'}, verify(A)).

verify_missing_root_test() ->
    A = device_archive([<<"_hb_device_v_1__a">>]),
    ?assertEqual({error, archive_missing_root}, verify(A)).

verify_multiple_roots_test() ->
    A = device_archive([<<"_hb_device_a_1">>, <<"_hb_device_b_1">>]),
    ?assertMatch({error, {multiple_roots, _}}, verify(A)).

verify_outside_namespace_test() ->
    A = device_archive([<<"_hb_device_a_1">>, <<"_hb_device_b_1__x">>]),
    ?assertEqual(
        {error, {modules_outside_namespace, ['_hb_device_b_1__x']}},
        verify(A)
    ).

verify_rejects_non_device_module_test() ->
    A = device_archive([<<"plain_mod">>]),
    ?assertEqual({error, {non_device_module, plain_mod}}, verify(A)).

verify_rejects_invalid_beam_test() ->
    {ok, {_, A}} =
        zip:create(
            <<"z">>,
            [{"ebin/_hb_device_bad_1.beam", <<"not a beam">>,
              file_info(8#100644, 10)}],
            [memory, {uncompress, all}]
        ),
    ?assertMatch({error, {invalid_beam, _, _}}, verify(A)).

contents_rejects_path_traversal_test() ->
    {ok, {_, A}} =
        zip:create(
            <<"z">>,
            [{"priv/../escape", <<"x">>, file_info(8#100644, 1)}],
            [memory, {uncompress, all}]
        ),
    % The malicious entry must be rejected; the exact error tag depends
    % on whether zip preserves or normalizes the `..' segment.
    ?assertMatch({error, _}, contents(A)).

load_extracts_and_loads_test() ->
    Root = <<"_hb_device_load_1">>,
    A = device_archive([Root, <<"_hb_device_load_1__h">>],
        #{ <<"share/d">> => <<"payload">>,
           <<"bin/run">> => <<"#!/bin/sh\n">> }),
    Dir = "/tmp/hb_device_archive_test_out_"
        ++ integer_to_list(erlang:unique_integer([positive])),
    code:purge('_hb_device_load_1'),
    code:delete('_hb_device_load_1'),
    ?assertEqual(
        {ok, '_hb_device_load_1',
            ['_hb_device_load_1', '_hb_device_load_1__h']},
        load(A, Dir)
    ),
    ?assertEqual(pong, '_hb_device_load_1':ping()),
    ?assertEqual({ok, <<"payload">>},
        file:read_file(filename:join(Dir, "share/d"))),
    {ok, #file_info{ mode = Mode }} =
        file:read_file_info(filename:join(Dir, "bin/run")),
    ?assertEqual(8#755, Mode band 8#777),
    % Loading again is a no-op (already-loaded modules are skipped).
    ?assertMatch({ok, '_hb_device_load_1', _}, load(A, Dir)).

-endif.
