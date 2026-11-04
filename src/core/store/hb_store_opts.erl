%%% @doc A module responsible for applying default configuration to store options.
%%%
%%% This module takes store options and store defaults and returns a new list
%%% of stores with default properties applied based on the store-module type.
%%% Supports recursive application to nested store configurations.
-module(hb_store_opts).
-export([apply/2]).
-compile({no_auto_import,[apply/2]}).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Apply store defaults to store options.
%% Takes StoreOpts (list of store configuration maps) and Defaults (map of defaults)
%% and returns a new list with defaults applied where appropriate.

apply(StoreOpts, Defaults) when is_list(StoreOpts), is_map(Defaults) ->
    lists:map(
        fun(StoreOpt) ->
            apply_defaults_to_store(StoreOpt, Defaults)
        end,
        StoreOpts
    ).

%% @doc Apply defaults to a single store configuration.
apply_defaults_to_store(StoreOpt, Defaults) when is_map(StoreOpt), is_map(Defaults) ->
    UpdatedStore = apply_defaults_by_module_type(StoreOpt, Defaults),
    apply_defaults_to_substores(UpdatedStore, Defaults).

%% @doc Apply defaults based on store-module.
apply_defaults_by_module_type(StoreOpt, Defaults) ->
    case maps:get(<<"store-module">>, StoreOpt, undefined) of
        hb_store_lmdb ->
            apply_type_defaults(StoreOpt, <<"lmdb">>, Defaults);
        hb_store_fs ->
            apply_type_defaults(StoreOpt, <<"fs">>, Defaults);
        hb_store_rocksdb ->
            apply_type_defaults(StoreOpt, <<"rocksdb">>, Defaults);
        hb_store_gateway ->
            apply_type_defaults(StoreOpt, <<"gateway">>, Defaults);
        _ ->
            StoreOpt
    end.

%% @doc Apply type-specific defaults to a store.
apply_type_defaults(StoreOpt, TypeKey, Defaults) ->
    case maps:get(TypeKey, Defaults, #{}) of
        TypeDefaults when is_map(TypeDefaults) ->
            maps:merge(TypeDefaults, StoreOpt);
        _ ->
            StoreOpt
    end.

%% @doc Apply defaults to sub-stores recursively.
apply_defaults_to_substores(StoreOpt, Defaults) ->
    case maps:get(<<"store">>, StoreOpt, undefined) of
        SubStores when is_list(SubStores) ->
            UpdatedSubStores = 
                lists:map(
                    fun(SubStore) ->
                        apply_defaults_to_store(SubStore, Defaults)
                    end,
                    SubStores
                ),
            maps:put(<<"store">>, UpdatedSubStores, StoreOpt);
        _ ->
            StoreOpt
    end.

%% EUnit tests

basic_apply_test() ->
    StoreOpts = 
        [
            #{
                <<"name">> => <<"cache-mainnet/lmdb">>,
                <<"store-module">> => hb_store_lmdb
            }
        ],
    Defaults = 
        #{
            <<"lmdb">> => #{
                <<"capacity">> => 1073741824
            }
        },
    Expected = 
        [
            #{
                <<"name">> => <<"cache-mainnet/lmdb">>,
                <<"store-module">> => hb_store_lmdb,
                <<"capacity">> => 1073741824
            }
        ],
    Result = apply(StoreOpts, Defaults),
    ?assertEqual(Expected, Result).

empty_defaults_test() ->
    StoreOpts = 
        [
            #{
                <<"name">> => <<"cache-mainnet/lmdb">>,
                <<"store-module">> => hb_store_lmdb
            }
        ],
    Defaults = #{},
    Expected = 
        [
            #{
                <<"name">> => <<"cache-mainnet/lmdb">>,
                <<"store-module">> => hb_store_lmdb
            }
        ],
    Result = apply(StoreOpts, Defaults),
    ?assertEqual(Expected, Result).

empty_store_opts_test() ->
    StoreOpts = [],
    Defaults = 
        #{
            <<"lmdb">> => #{
                <<"capacity">> => 1073741824
            }
        },
    Expected = [],
    Result = apply(StoreOpts, Defaults),
    ?assertEqual(Expected, Result).

nested_stores_test() ->
    StoreOpts = 
        [
            #{
                <<"store-module">> => hb_store_gateway,
                <<"store">> => [
                    #{
                        <<"name">> => <<"cache-mainnet/lmdb">>,
                        <<"store-module">> => hb_store_lmdb
                    }
                ]
            }
        ],
    Defaults = 
        #{
            <<"lmdb">> => #{
                <<"capacity">> => 1073741824
            }
        },
    Expected = 
        [
            #{
                <<"store-module">> => hb_store_gateway,
                <<"store">> => [
                    #{
                        <<"name">> => <<"cache-mainnet/lmdb">>,
                        <<"store-module">> => hb_store_lmdb,
                        <<"capacity">> => 1073741824
                    }
                ]
            }
        ],
    Result = apply(StoreOpts, Defaults),
    ?assertEqual(Expected, Result).

%% @doc Integration test to verify that capacity is properly set for hb_store_lmdb
%% This test verifies that the capacity value is correctly applied and accessible
%% to the hb_store_lmdb module before environment creation.
lmdb_capacity_integration_test() ->
    CustomCapacity = 5000,
    StoreOpts = 
        [
            #{
                <<"name">> => <<"test-lmdb">>,
                <<"store-module">> => hb_store_lmdb
            }
        ],
    Defaults = 
        #{
            <<"lmdb">> => #{
                <<"capacity">> => CustomCapacity
            }
        },
    [UpdatedStoreOpt] = apply(StoreOpts, Defaults),
    ?assertEqual(CustomCapacity, maps:get(<<"capacity">>, UpdatedStoreOpt)),
    ?assertEqual(<<"test-lmdb">>, maps:get(<<"name">>, UpdatedStoreOpt)),
    ?assertEqual(hb_store_lmdb, maps:get(<<"store-module">>, UpdatedStoreOpt)),
    ?assertNotEqual(16 * 1024 * 1024 * 1024, maps:get(<<"capacity">>, UpdatedStoreOpt)),
    MultipleStoreOpts = 
        [
            #{
                <<"name">> => <<"test-lmdb-1">>,
                <<"store-module">> => hb_store_lmdb
            },
            #{
                <<"name">> => <<"test-lmdb-2">>,
                <<"store-module">> => hb_store_lmdb
            },
            #{
                <<"name">> => <<"test-fs">>,
                <<"store-module">> => hb_store_fs
            }
        ],
    UpdatedMultipleStoreOpts = apply(MultipleStoreOpts, Defaults),
    [LmdbStore1, LmdbStore2, FsStore] = UpdatedMultipleStoreOpts,
    ?assertEqual(CustomCapacity, maps:get(<<"capacity">>, LmdbStore1)),
    ?assertEqual(CustomCapacity, maps:get(<<"capacity">>, LmdbStore2)),
    ?assertEqual(false, maps:is_key(<<"capacity">>, FsStore)),
    ?event({integration_test_passed, {lmdb_capacity, CustomCapacity}, {note, "correctly applied to store options"}}).

%% @doc Full integration test simulating the hb_http_server flow
%% This test verifies that the complete flow from config loading to store defaults
%% application works correctly, simulating what happens in hb_http_server:start/0
full_integration_flow_test() ->
    LoadedConfig = #{
        <<"store_defaults">> => #{
            <<"lmdb">> => #{
                <<"capacity">> => 5000
            }
        }
    },
    DefaultStoreOpts = [
        #{
            <<"name">> => <<"cache-mainnet/lmdb">>,
            <<"store-module">> => hb_store_lmdb
        },
        #{
            <<"store-module">> => hb_store_fs,
            <<"name">> => <<"cache-mainnet">>
        },
        #{
            <<"store-module">> => hb_store_gateway,
            <<"subindex">> => [
                #{
                    <<"name">> => <<"Data-Protocol">>,
                    <<"value">> => <<"ao">>
                }
            ],
            <<"store">> => [
                #{
                    <<"store-module">> => hb_store_lmdb,
                    <<"name">> => <<"cache-mainnet/lmdb">>
                }
            ]
        }
    ],
    MergedConfig = maps:merge(
        #{<<"store">> => DefaultStoreOpts},
        LoadedConfig
    ),
    StoreOpts = maps:get(<<"store">>, MergedConfig),
    StoreDefaults = maps:get(<<"store_defaults">>, MergedConfig, #{}),
    UpdatedStoreOpts = apply(StoreOpts, StoreDefaults),
    [LmdbStore, FsStore, GatewayStore] = UpdatedStoreOpts,
    ?assertEqual(5000, maps:get(<<"capacity">>, LmdbStore)),
    ?assertEqual(<<"cache-mainnet/lmdb">>, maps:get(<<"name">>, LmdbStore)),
    ?assertEqual(hb_store_lmdb, maps:get(<<"store-module">>, LmdbStore)),
    ?assertEqual(false, maps:is_key(<<"capacity">>, FsStore)),
    ?assertEqual(hb_store_fs, maps:get(<<"store-module">>, FsStore)),
    ?assertEqual(hb_store_gateway, maps:get(<<"store-module">>, GatewayStore)),
    NestedStores = maps:get(<<"store">>, GatewayStore),
    [NestedLmdbStore] = NestedStores,
    ?assertEqual(5000, maps:get(<<"capacity">>, NestedLmdbStore)),
    ?assertEqual(hb_store_lmdb, maps:get(<<"store-module">>, NestedLmdbStore)),
    ?assertEqual(3, length(UpdatedStoreOpts)),
    ?event({full_integration_test_passed, store_defaults_correctly_applied_through_complete_flow}).