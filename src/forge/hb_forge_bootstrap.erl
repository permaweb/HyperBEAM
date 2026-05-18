%%% @doc Forge-private bootstrap helpers for package identity generation.
%%%
%%% Normal package IDs are AO-Core message IDs, which means the forge must
%%% temporarily load the message, structured, and commitment devices before it
%%% can build the production preloaded-store. This module contains that
%%% build-only loop; runtime device resolution never sees these shortcuts.
-module(hb_forge_bootstrap).
-export([include_seed_groups/2, with_package_devices/3]).
-export([seed_device_names/1, volatile_device_store/1]).
-export([load_and_cache_devices/3]).
-include("include/hb.hrl").

%% @doc Add the seed device groups to a package group list.
include_seed_groups(Groups, Opts) ->
    unique_groups(Groups ++ seed_groups(Groups, Opts)).

%% @doc Package with temporary generated message/codec devices loaded into a
%% build-local volatile device-store. The temporary packages are never written
%% to the preloaded-store.
with_package_devices(Groups, Opts, Fun) ->
    Store = volatile_device_store(<<"package-bootstrap">>),
    hb_store:start(Store, #{}, Opts),
    try
        BootOpts =
            Opts#{
                <<"package-id-mode">> => bootstrap,
                <<"device-store">> => Store
            },
        SeedPkgs =
            [
                hb_packager:package(G, BootOpts)
             ||
                G <- seed_groups(Groups, Opts)
            ],
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

%% @doc Return the public device names required to sign preload messages.
seed_device_names(Opts) ->
    lists:usort([
        <<"message@1.0">>,
        <<"structured@1.0">>,
        hb_opts:get(commitment_device, <<"httpsig@1.0">>, Opts)
    ]).

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

%% @doc Load packages and cache their public device names in the device-store.
load_and_cache_devices(Pkgs, Names, Opts) ->
    ByName =
        maps:from_list(
            [{maps:get(device_name, Pkg), Pkg} || Pkg <- Pkgs]
        ),
    lists:foreach(
        fun(Name) ->
            Pkg = maps:get(Name, ByName),
            ok = hb_device_archive:load(maps:get(archive, Pkg)),
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

%% @doc Return the groups needed to bootstrap package identity calculation.
seed_groups(Groups, Opts) ->
    Roots = seed_roots(Opts),
    Found =
        [G || G = #{ root := Root } <- Groups, lists:member(Root, Roots)],
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
    hb_packager:scan(
        bootstrap_device_dirs(Opts),
        #{ <<"device-roots">> => Roots }
    ).

%% @doc Return the source root atoms needed by the bootstrap phase.
seed_roots(Opts) ->
    [device_name_to_root(Name) || Name <- seed_device_names(Opts)].

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
