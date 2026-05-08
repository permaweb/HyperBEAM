#!/usr/bin/env escript

%%% @doc Build the in-repo `_build/preloaded-store' from `src/preloaded'.
%%%
%%% Invoked from the rebar.config post-compile hook so every build of
%%% HyperBEAM ends with a working `preloaded-store' on disk and a
%%% matching `_build/hb_preloaded_index.hrl' header. The store is signed
%%% with the node wallet, so the runtime's default device-author trust
%%% rule (trust the node wallet unless configured otherwise) applies.

main(_Args) ->
    add_code_paths(),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(asn1),
    {ok, _} = application:ensure_all_started(public_key),
    OutputDir = <<"_build/preloaded-store">>,
    SrcDir = "src/preloaded",
    Wallet = hb:wallet(),
    io:format("[preload] scanning ~s...~n", [SrcDir]),
    Groups = hb_packager:scan([SrcDir], #{}),
    io:format("[preload] packaging ~p devices~n", [length(Groups)]),
    Pkgs =
        lists:map(
            fun(G) ->
                Pkg = hb_packager:package(G, #{}),
                io:format("[preload]   ~s -> ~p~n",
                    [maps:get(device_name, Pkg),
                     maps:get(module_name, Pkg)]),
                Pkg
            end,
            Groups
        ),
    {ok, Result} =
        hb_preload:build_dir(Pkgs, Wallet, OutputDir, #{}),
    Index = maps:get(index, Result),
    io:format("[preload] index = ~s~n", [Index]),
    HeaderPath = <<"_build/hb_preloaded_index.hrl">>,
    ok = hb_preload:write_index_header(Index, HeaderPath),
    io:format("[preload] header written: ~s~n", [HeaderPath]),
    recompile_hb_opts(),
    halt(0).

add_code_paths() ->
    Paths =
        filelib:wildcard("_build/*/lib/*/ebin") ++
            filelib:wildcard("_build/*/plugins/*/ebin"),
    lists:foreach(
        fun(P) -> code:add_patha(P) end,
        lists:reverse(lists:sort(fun newer_path/2, Paths))
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
            compile:file(
                "src/kernel/hb_opts.erl",
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
drop_outdir([Opt | Rest]) -> [Opt | drop_outdir(Rest)];
drop_outdir([]) -> [].

fallback_opts() ->
    [
        debug_info,
        {i, "src/kernel"}
    ].
