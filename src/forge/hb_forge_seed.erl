%%% @doc Build-only seed codec loading for the Forge.
%%%
%%% Computing a device's identity (the unsigned AO-Core message ID of
%%% its source-file message) and signing the preloaded spec/impl
%%% messages requires the `message@1.0', `structured@1.0',
%%% `httpsig@1.0' and commitment codec devices. At build time the Forge
%%% compiles those few device groups from `src/preloaded' under their
%%% ordinary module names, loads them, and exposes a `Name => RootModule' message.
%%%
%%% The packager threads that message through the `forge-bootstrap' option.
%%% When present, {@link hb_device:load/2} resolves a device name
%%% with a single `maps:find' in it and nothing else; the runtime never
%%% sets `forge-bootstrap', so production resolution is the pure
%%% preloaded store. The two paths cannot intermingle. There is no
%%% multi-phase bootstrap and no temporary archives.
-module(hb_forge_seed).
-export([seed_names/1, with_forge_bootstrap/2]).
-include("include/hb.hrl").

%% @doc The device names whose codecs the build needs.
seed_names(Opts) ->
    lists:usort([
        <<"message@1.0">>,
        <<"structured@1.0">>,
        <<"httpsig@1.0">>,
        hb_opts:get(commitment_device, <<"httpsig@1.0">>, Opts)
    ]).

%% @doc Run `Fun(Opts')' with the seed codecs loaded and reachable
%% through the `forge-bootstrap' option (caller-provided entries win).
with_forge_bootstrap(Opts, Fun) ->
    Seeds = maps:merge(seed_map(Opts), existing_seeds(Opts)),
    Fun(Opts#{ <<"forge-bootstrap">> => Seeds }).

existing_seeds(Opts) ->
    case hb_opts:get(forge_bootstrap, #{}, Opts) of
        M when is_map(M) -> M;
        _ -> #{}
    end.

%% @doc Compile + load each seed device group under its ordinary names
%% and return a `DeviceName => RootModule' map.
seed_map(Opts) ->
    Names = seed_names(Opts),
    Roots = [name_to_root(N) || N <- Names],
    Groups =
        hb_packager:scan(seed_dirs(Opts), #{ <<"device-roots">> => Roots }),
    maps:from_list(
        lists:map(
            fun(Name) ->
                Root = name_to_root(Name),
                ok = load_group(find_group(Root, Groups)),
                {Name, Root}
            end,
            Names
        )
    ).

%% @doc Compile and load every module of a seed device group under its
%% own name (no rename transform): intra-device calls keep working and
%% device-name resolution is satisfied by the `forge-bootstrap' map.
load_group(#{ root := Root, root_file := RootFile, helpers := Helpers }
        = Group) ->
    Libraries = maps:get(libraries, Group, []),
    Entries = [{Root, RootFile} | Helpers ++ Libraries],
    lists:foreach(fun load_module/1, Entries).

load_module({Mod, Path}) ->
    case code:is_loaded(Mod) of
        {file, _} ->
            ok;
        false ->
            File = hb_util:list(Path),
            Includes =
                [{i, hb_util:list(Dir)} || Dir <- include_dirs(Path)],
            case compile:file(File, [binary, debug_info, return_errors]
                    ++ Includes) of
                {ok, Mod, Bin} ->
                    {module, Mod} =
                        code:load_binary(Mod, File, Bin),
                    ok;
                {ok, Mod, Bin, _Warnings} ->
                    {module, Mod} =
                        code:load_binary(Mod, File, Bin),
                    ok;
                Error ->
                    erlang:error({seed_compile_failed, Mod, Error})
            end
    end.

%% @doc Include dirs needed to compile one seed source file.
include_dirs(Path) ->
    lists:usort(
        [
            <<"src">>,
            <<"src/core">>,
            hb_util:bin(filename:dirname(hb_util:list(Path)))
        ]
        ++ source_core_dir(Path)
    ).

%% @doc Infer the sibling `src/core' include dir for dependency sources.
source_core_dir(Source) ->
    Abs = filename:absname(hb_util:list(Source)),
    case string:str(Abs, "/src/preloaded/") of
        0 -> [];
        Pos ->
            [
                hb_util:bin(
                    filename:join(
                        [string:substr(Abs, 1, Pos - 1), "src", "core"]
                    )
                )
            ]
    end.

find_group(Root, Groups) ->
    case [G || G = #{ root := R } <- Groups, R =:= Root] of
        [G | _] -> G;
        [] -> erlang:error({missing_seed_device_source, Root})
    end.

%% @doc Map a device name to its `dev_*' source root module.
name_to_root(<<"~", Rest/binary>>) ->
    name_to_root(Rest);
name_to_root(Name) ->
    [Base | _] = binary:split(hb_util:bin(Name), <<"@">>),
    Tail0 = binary:replace(Base, <<"-">>, <<"_">>, [global]),
    Tail = binary:replace(Tail0, <<"/">>, <<"_">>, [global]),
    binary_to_atom(<<"dev_", Tail/binary>>, utf8).

%% @doc Source directories searched for the seed device groups.
seed_dirs(Opts) ->
    case hb_maps:get(<<"bootstrap-device-src">>, Opts, undefined, Opts) of
        undefined -> [<<"src/preloaded">>];
        Dir when is_binary(Dir) -> [Dir];
        Dir = [C | _] when is_integer(C) -> [Dir];
        Dirs when is_list(Dirs) -> Dirs
    end.
