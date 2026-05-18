%%% @doc Helpers for packaged-device implementation archives.
-module(hb_device_archive).
-export([contents/1, load/1, load_modules/1, loaded/1]).
-export([modules_match_root/2, write_resources/2]).
-export([on_load_format/0, encode_on_loads/1, decode_on_loads/1, run_on_loads/2]).

-define(ON_LOAD_FORMAT, <<"hb-device-on-load-v1">>).

%% @doc Return the on-load metadata format tag.
on_load_format() ->
    ?ON_LOAD_FORMAT.

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
            case load_modules(Modules) of
                ok -> ok;
                already_loaded -> ok;
                {error, _} = Error -> Error
            end;
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

%% @doc Atomically load archive modules.
load_modules(Modules) ->
    case code:atomic_load(Modules) of
        ok ->
            ok;
        {error, Reason} ->
            case loaded(Modules) of
                true -> already_loaded;
                false -> {error, Reason}
            end
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

%% @doc Encode on-load callback metadata into one flat binary field.
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

%% @doc Decode on-load callback metadata.
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

%% @doc Execute flat on-load metadata embedded in an implementation message.
run_on_loads(Msg, Opts) ->
    case hb_maps:get(<<"on-load">>, Msg, <<>>, Opts) of
        <<>> ->
            ok;
        OnLoad when is_binary(OnLoad) ->
            case hb_maps:get(<<"on-load-format">>, Msg, undefined, Opts) of
                ?ON_LOAD_FORMAT ->
                    case decode_on_loads(OnLoad) of
                        {ok, OnLoads} -> run_on_load_list(OnLoads);
                        {error, _} = Error -> Error
                    end;
                Other ->
                    {error, {unsupported_on_load_format, Other}}
            end;
        Other ->
            {error, {invalid_on_load_metadata, Other}}
    end.

run_on_load_list([]) ->
    ok;
run_on_load_list([#{ <<"module-name">> := ModBin,
                     <<"function">> := FunBin } | Rest]) ->
    Mod = hb_util:key_to_atom(ModBin, existing),
    Fun = hb_util:key_to_atom(FunBin, existing),
    case apply(Mod, Fun, []) of
        ok -> run_on_load_list(Rest);
        Other -> {error, {on_load_failed, Mod, Fun, Other}}
    end.
