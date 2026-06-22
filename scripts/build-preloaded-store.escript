#!/usr/bin/env escript

%%% @doc Build the in-repo `_build/preloaded-store' from `src/preloaded'.
%%%
%%% Invoked from the rebar.config post-compile hook so every build of
%%% HyperBEAM ends with a working `preloaded-store' on disk and a
%%% matching in-store preloaded index. The store is signed with the node wallet,
%%% so the runtime's default device-author trust rule (trust the node wallet
%%% unless configured otherwise) applies.

-include("../include/hb.hrl").

main(Args) ->
    add_code_paths(),
    try run(Args)
    catch
        error:{device_compile_failed, _, _, _, _} = Reason ->
            io:put_chars(
                standard_error,
                hb_packager:format_error(Reason)
            ),
            halt(1)
    end.

run(_Args) ->
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
    halt(0).

add_code_paths() ->
    AllPaths =
        filelib:wildcard("_build/*/lib/*/ebin") ++
            filelib:wildcard("_build/*/plugins/*/ebin"),
    Paths = lists:sort(fun newer_path/2, AllPaths),
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
