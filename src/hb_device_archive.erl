%%% @doc Helpers for packaged-device implementation archives.
-module(hb_device_archive).
-export([contents/1, load/1, load_modules/1, loaded/1]).
-export([modules_match_root/2, write_resources/2]).

%% @doc Extract loadable modules and resources from an implementation archive.
contents(Archive) ->
    case zip:unzip(Archive, [memory]) of
        {ok, Files} -> read_entries(Files, [], []);
        {error, Reason} -> {error, {archive_extract_failed, Reason}}
    end.

%% @doc Load every BEAM in an archive into the current code server.
load(Archive) ->
    case contents(Archive) of
        {ok, Modules, _Resources} ->
            load_modules(Modules);
        {error, _} = Error ->
            Error
    end.

%% @doc Parse archive entries into BEAM modules and priv resources.
read_entries([], ModulesAcc, ResourceAcc) ->
    Modules = [Mod || {Mod, _, _} <- ModulesAcc],
    Resources = [Path || {Path, _} <- ResourceAcc],
    case {
        length(Modules) =:= length(lists:usort(Modules)),
        length(Resources) =:= length(lists:usort(Resources))
    } of
        {true, true} ->
            {ok, lists:reverse(ModulesAcc), lists:reverse(ResourceAcc)};
        {false, _} -> {error, duplicate_archive_module};
        {_, false} -> {error, duplicate_archive_file}
    end;
read_entries([{Path0, Body} | Rest], ModulesAcc, ResourceAcc) ->
    Path = hb_util:bin(Path0),
    case Path of
        <<"ebin/", _/binary>> ->
            case beam_module(Path, Body) of
                {ok, Mod} ->
                    read_entries(
                        Rest,
                        [{Mod, binary_to_list(Path), Body} | ModulesAcc],
                        ResourceAcc
                    );
                {error, Reason} ->
                    {error, Reason}
            end;
        <<"priv/", Rel/binary>> ->
            case safe_resource(Rel) of
                ok -> read_entries(Rest, ModulesAcc, [{Rel, Body} | ResourceAcc]);
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, {unsupported_archive_path, Path}}
    end.

%% @doc Return the declared module name of a generated BEAM archive member.
beam_module(Path, Beam) ->
    case beam_lib:chunks(Beam, [exports]) of
        {ok, {Mod, _Chunks}} ->
            ModBin = atom_to_binary(Mod, utf8),
            ExpectedPath = <<"ebin/", ModBin/binary, ".beam">>,
            case {hb_device_name:is_generated(Mod), Path} of
                {false, _} -> {error, {non_generated_module_name, ModBin}};
                {true, ExpectedPath} -> {ok, Mod};
                {true, _} -> {error, {archive_path_mismatch, Path, ExpectedPath}}
            end;
        {error, _Module, Reason} ->
            {error, {invalid_beam, Path, Reason}}
    end.

%% @doc Ensure every archive module belongs to the root generated namespace.
modules_match_root(RootMod, Modules) ->
    case lists:keymember(RootMod, 1, Modules) of
        false ->
            {error, archive_missing_root};
        true ->
            RootBin = atom_to_binary(RootMod, utf8),
            Prefix = <<RootBin/binary, "__">>,
            case [
                Mod
             ||
                {Mod, _, _} <- Modules,
                not same_archive_namespace(Mod, RootBin, Prefix)
            ] of
                [] -> ok;
                Bad -> {error, {archive_module_outside_namespace, Bad}}
            end
    end.

%% @doc Return true if a module belongs to the archive root namespace.
same_archive_namespace(Mod, RootBin, Prefix) ->
    ModBin = atom_to_binary(Mod, utf8),
    ModBin =:= RootBin orelse
        binary:match(ModBin, Prefix) =:= {0, byte_size(Prefix)}.

%% @doc Reject archive resource paths that could escape the target directory.
safe_resource(<<>>) ->
    {error, empty_archive_resource_path};
safe_resource(Rel) ->
    Parts = binary:split(Rel, <<"/">>, [global]),
    case binary:match(Rel, <<"\\">>) =/= nomatch orelse
        lists:any(fun unsafe_resource_part/1, Parts)
    of
        true -> {error, {unsafe_archive_resource_path, Rel}};
        false -> ok
    end.

%% @doc Return true for path components unsafe inside an archive resource path.
unsafe_resource_part(<<>>) -> true;
unsafe_resource_part(<<".">>) -> true;
unsafe_resource_part(<<"..">>) -> true;
unsafe_resource_part(_) -> false.

%% @doc Load archive modules. OTP cannot atomically load modules that
%% declare `-on_load', so normal devices use the atomic path while
%% on-load devices fall back to ordinary Erlang loading semantics.
load_modules(Modules) ->
    case code:atomic_load(Modules) of
        ok -> ok;
        {error, Reason} ->
            case atomic_load_rejected_on_load(Reason) of
                true -> load_modules_naturally(Modules);
                false ->
                    case loaded(Modules) of
                        true -> ok;
                        false -> {error, Reason}
                    end
            end
    end.

%% @doc Return true if `code:atomic_load/1' rejected module on-load callbacks.
atomic_load_rejected_on_load(Reason) when is_list(Reason) ->
    lists:any(
        fun({_Mod, on_load_not_allowed}) -> true;
           (_) -> false
        end,
        Reason
    );
atomic_load_rejected_on_load(_Reason) ->
    false.

%% @doc Load each archive module with ordinary Erlang code loading.
load_modules_naturally(Modules) ->
    lists:foldl(fun load_one_module/2, ok, Modules).

%% @doc Load a single archive module unless it is already loaded.
load_one_module(_Module, {error, _} = Error) ->
    Error;
load_one_module({Mod, File, Beam}, ok) ->
    case code:is_loaded(Mod) of
        false ->
            case code:load_binary(Mod, File, Beam) of
                {module, Mod} -> ok;
                {error, Reason} -> {error, {Mod, Reason}}
            end;
        _ ->
            ok
    end.

%% @doc Check whether every archive module is present in the code server.
loaded(Modules) ->
    lists:all(fun({Mod, _, _}) -> code:is_loaded(Mod) =/= false end, Modules).

%% @doc Write implementation resources under a private implementation directory.
write_resources(_Dir, []) ->
    ok;
write_resources(Dir, [{Rel, Body} | Rest]) ->
    Path = filename:join(Dir, hb_util:list(Rel)),
    case filelib:ensure_dir(Path) of
        ok ->
            case file:write_file(Path, Body) of
                ok ->
                    maybe_make_executable(Rel, Path),
                    write_resources(Dir, Rest);
                {error, Reason} ->
                    {error, {resource_write_failed, Rel, Reason}}
            end;
        {error, Reason} ->
            {error, {resource_dir_failed, Rel, Reason}}
    end.

%% @doc Mark scripts and bin resources executable after extraction.
maybe_make_executable(<<"bin/", _/binary>>, Path) ->
    file:change_mode(Path, 8#100755);
maybe_make_executable(Rel, Path) ->
    case filename:extension(hb_util:list(Rel)) of
        ".sh" -> file:change_mode(Path, 8#100755);
        _ -> ok
    end.
