#!/usr/bin/env escript

%%% @doc Build the in-repo `_build/preloaded-store' from `src/preloaded'.
%%%
%%% Invoked from the rebar.config post-compile hook so every build of
%%% HyperBEAM ends with a working `preloaded-store' on disk and a
%%% matching `_build/hb_preloaded_index.hrl' header. The store is signed
%%% with the node wallet, so the runtime's default device-author trust
%%% rule (trust the node wallet unless configured otherwise) applies.

-include("../include/hb.hrl").

main(_Args) ->
    with_lock("_build/preloaded-store.lock", fun build/0),
    halt(0).

build() ->
    add_code_paths(),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(asn1),
    {ok, _} = application:ensure_all_started(public_key),
    OutputDir = <<"_build/preloaded-store">>,
    SrcDir = <<"src/preloaded">>,
    Wallet = hb:wallet(),
    ?event(preload, {scanning_preloaded_devices, {dir, SrcDir}}),
    Groups = hb_packager:scan([SrcDir], #{}),
    ?event(preload, {packaging_preloaded_devices, {count, length(Groups)}}),
    {ok, Result} =
        hb_preload:build_groups(
            Groups,
            Wallet,
            OutputDir,
            #{ <<"bootstrap-device-src">> => [SrcDir] }
        ),
    lists:foreach(
        fun(Pkg) ->
            ?event(preload, {
                packaged_preloaded_device,
                {name, maps:get(device_name, Pkg)},
                {module, maps:get(module_name, Pkg)}
            })
        end,
        maps:get(pkgs, Result)
    ),
    Index = maps:get(index, Result),
    ?event(preload, {preloaded_index, Index}),
    HeaderPath = <<"_build/hb_preloaded_index.hrl">>,
    ok = hb_preload:write_index_header(Index, HeaderPath),
    ?event(preload, {preloaded_index_header, HeaderPath}),
    recompile_hb_opts().

with_lock(LockDir, Fun) ->
    ensure_build_dir(),
    acquire_lock(LockDir),
    try
        Fun()
    after
        file:del_dir(LockDir)
    end.

ensure_build_dir() ->
    case file:make_dir("_build") of
        ok -> ok;
        {error, eexist} -> ok;
        {error, Reason} -> erlang:error({build_dir_failed, Reason})
    end.

acquire_lock(LockDir) ->
    case file:make_dir(LockDir) of
        ok ->
            ok;
        {error, eexist} ->
            timer:sleep(500),
            acquire_lock(LockDir);
        {error, enoent} ->
            ensure_build_dir(),
            acquire_lock(LockDir);
        {error, Reason} ->
            erlang:error({preloaded_store_lock_failed, Reason})
    end.

add_code_paths() ->
    DefaultPaths =
        filelib:wildcard("_build/default/lib/*/ebin") ++
            filelib:wildcard("_build/default/plugins/*/ebin"),
    AllPaths =
        filelib:wildcard("_build/*/lib/*/ebin") ++
            filelib:wildcard("_build/*/plugins/*/ebin"),
    Paths = DefaultPaths ++ lists:sort(fun newer_path/2, AllPaths -- DefaultPaths),
    lists:foreach(
        fun(P) -> code:add_pathz(P) end,
        Paths
    ).

newer_path(A, B) ->
    newest_beam_time(A) >= newest_beam_time(B).

newest_beam_time(Path) ->
    case filelib:wildcard(filename:join(Path, "*.beam")) of
        [] -> filelib:last_modified(Path);
        Beams -> lists:max([filelib:last_modified(B) || B <- Beams])
    end.

recompile_hb_opts() ->
    lists:foreach(
        fun(Ebin) ->
            {ok, hb_opts} =
                compile:file(
                    "src/core/resolver/hb_opts.erl",
                    [{outdir, Ebin} | hb_opts_compile_opts(Ebin)]
                )
        end,
        filelib:wildcard("_build/*/lib/hb/ebin")
    ).

hb_opts_compile_opts(Ebin) ->
    Beam = filename:join(Ebin, "hb_opts.beam"),
    case beam_lib:chunks(Beam, [compile_info]) of
        {ok, {_, [{compile_info, Info}]}} ->
            drop_outdir(proplists:get_value(options, Info, fallback_opts()));
        _ ->
            fallback_opts()
    end.

drop_outdir([{outdir, _} | Rest]) -> drop_outdir(Rest);
drop_outdir([{d, Name, Value} | Rest]) when is_list(Name) ->
    [{d, list_to_atom(Name), Value} | drop_outdir(Rest)];
drop_outdir([Opt | Rest]) -> [Opt | drop_outdir(Rest)];
drop_outdir([]) -> [].

fallback_opts() ->
    [
        debug_info,
        {i, "src/core"}
    ].
