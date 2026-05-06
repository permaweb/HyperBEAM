%%% @doc Helpers for using the generated local preloaded device store.
-module(hb_device_preload).
-export([
    default_metadata_file/0,
    default_store/0,
    metadata/0,
    metadata/1,
    name_resolver/0,
    name_resolver/1,
    name_resolvers/1,
    resolve_name/2,
    store_opts/1,
    trusted_signers/1,
    device_names/1
]).
-include("include/hb.hrl").

-define(DEFAULT_METADATA_FILE, "_build/default/preloaded-device-metadata.eterm").
-define(DEFAULT_STORE_DIR, <<"_build/default/preloaded-device-store">>).

%% @doc Return the default preload metadata file.
default_metadata_file() ->
    ?DEFAULT_METADATA_FILE.

%% @doc Return the default filesystem preload store.
default_store() ->
    #{
        <<"store-module">> => hb_store_fs,
        <<"name">> => ?DEFAULT_STORE_DIR
    }.

%% @doc Read preload metadata using default options.
metadata() ->
    metadata(#{}).

%% @doc Read generated preload metadata, or an empty metadata set.
metadata(Opts) ->
    File = metadata_file(Opts),
    case file:consult(hb_util:list(File)) of
        {ok, [Metadata]} when is_map(Metadata) -> Metadata;
        _ -> empty_metadata()
    end.

%% @doc Return the generated local name resolver map.
name_resolver() ->
    name_resolver(#{}).
name_resolver(Opts) ->
    maps:get(<<"name-resolver">>, metadata(Opts), #{}).

%% @doc Prepend the generated name resolver to configured resolvers.
name_resolvers(Resolvers) ->
    case name_resolver() of
        Resolver when map_size(Resolver) == 0 -> Resolvers;
        Resolver -> [Resolver | Resolvers]
    end.

%% @doc Resolve a device name to its generated local spec ID.
resolve_name(Name, Opts) ->
    NormName = hb_ao:normalize_key(Name),
    case maps:find(NormName, name_resolver(Opts)) of
        {ok, SpecID} ->
            {ok, SpecID};
        error ->
            resolve_with_name_device(NormName, Opts)
    end.

%% @doc Return opts that read device data only from the preloaded store.
store_opts(Opts) ->
    case preloaded_store(Opts) of
        [] ->
            Opts#{ <<"store">> => [], <<"match-index">> => [] };
        Store ->
            Opts#{
                <<"store">> => [read_only_store(Store)],
                <<"match-index">> => Store
            }
    end.

%% @doc Return configured trusted signers, defaulting to the local node identity.
trusted_signers(Opts) ->
    case hb_opts:get(trusted_device_signers, [], Opts) of
        [] -> self_signers(Opts);
        Signers ->
            Signers
    end.

%% @doc Return the set of generated device names known locally.
device_names(Opts) ->
    maps:keys(name_resolver(Opts)).

empty_metadata() ->
    #{
        <<"store">> => default_store(),
        <<"name-resolver">> => #{},
        <<"devices">> => []
    }.

metadata_file(#{ <<"preloaded-device-metadata">> := File }) ->
    File;
metadata_file(#{ preloaded_device_metadata := File }) ->
    File;
metadata_file(_) ->
    ?DEFAULT_METADATA_FILE.

%% @doc Return the local node identity used as the default trust root.
self_signers(Opts) ->
    case hb_opts:get(priv_wallet, no_viable_wallet, Opts) of
        no_viable_wallet ->
            Wallet = hb:wallet(hb_opts:get(priv_key_location)),
            [hb_util:human_id(ar_wallet:to_address(Wallet))];
        Wallet -> [hb_util:human_id(ar_wallet:to_address(Wallet))]
    end.

preloaded_store(Opts) ->
    case hb_opts:get(preloaded_store, default_store(), Opts) of
        false -> [];
        [] -> [];
        Store -> Store
    end.

read_only_store(Store) ->
    Store#{ <<"access">> => [<<"read">>] }.

resolve_with_name_device(<<"name@1.0">>, _Opts) ->
    not_found;
resolve_with_name_device(Name, Opts) ->
    ResolveOpts =
        Opts#{
            <<"name-resolvers">> =>
                name_resolvers(hb_opts:get(name_resolvers, [], Opts))
        },
    try
        case hb_ao:resolve(
            #{ <<"device">> => <<"name@1.0">> },
            #{ <<"path">> => Name, <<"load">> => false },
            ResolveOpts
        ) of
            {ok, SpecID} when ?IS_ID(SpecID) -> {ok, SpecID};
            _ -> not_found
        end
    catch
        _:_ -> not_found
    end.
