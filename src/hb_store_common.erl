%% @doc Store standard library
%%
%% Common patterns to access Stores (File System, LMDB, etc).

-module(hb_store_common).
-export([resolved_list/2, resolved_type/2]).
-export([get_multiple_stores/0, shutdown_stores/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Unify resolve and list functions into one call.
resolved_list(Stores, Path) when is_list(Stores) ->
    do_resolved_list(Stores, Path);
resolved_list(Store, Path) -> 
    do_resolved_list([Store], Path).

do_resolved_list([], _Path) ->
    not_found;
do_resolved_list([Store|RemainingStores], Path) ->
    ResolvedPath = hb_store:resolve(Store, Path),
    ?event({resolved_list, {path, Path}, {resolved_path, ResolvedPath}}),
    case hb_store:list(Store, ResolvedPath) of 
        {ok, _} = Result -> Result;
        not_found -> do_resolved_list(RemainingStores, Path)
    end.

%% @doc Unify resolve and type functions into one call.
resolved_type(Stores, Path) when is_list(Stores) ->
    do_resolved_type(Stores, Path);
resolved_type(Store, Path) -> 
    do_resolved_type([Store], Path).

do_resolved_type([], _Path) -> not_found;
do_resolved_type([Store|RemainingStores], Path) -> 
    ResolvedPath = hb_store:resolve(Store, Path),
    ?event({resolved_type, {path, Path}, {resolved_path, ResolvedPath}}),
    case hb_store:type(Store, ResolvedPath) of 
        Result when Result =/= not_found -> Result;
        _ -> do_resolved_type(RemainingStores, Path)
    end.

%% Tests

%% @doc Test that resolve and type must be made in the same store, 
%% when multiple stores are provided.
resolved_type_test() -> 
    [_Store1, Store2] = Stores = get_multiple_stores(),
    %% Write test data
    hb_store:make_group(Store2, <<"group1">>),
    hb_store:write(Store2, <<"data/final_id">>, <<"data">>),
    hb_store:make_link(Store2, <<"data/final_id">>, <<"group1/data">>),
    hb_store:make_link(Store2, <<"group1">>, <<"random_id">>),
    %% Check result
    RawPath = <<"random_id/data">>,
    Result = resolved_type(Stores, RawPath),
    try
        ?assertEqual(simple, Result)
    after
        shutdown_stores(Stores)
    end.

%% @doc Test that resolve and list must be made in the same store, 
%% when multiple stores are provided.
resolved_list_test() -> 
    [_Store1, Store2] = Stores = get_multiple_stores(),
    %% Write test data
    hb_store:make_group(Store2, <<"group1">>),
    hb_store:make_group(Store2, <<"group1/group12">>),
    hb_store:write(Store2, <<"data/final_id2">>, <<"7890">>),
    %% Link 
    %% TODO: Not sure if this structure is possible in HB
    hb_store:make_link(Store2, <<"data/final_id2">>, <<"group1/group12/data">>),
    hb_store:make_link(Store2, <<"group1">>, <<"random_id">>),
    %% Check result
    RawPath = <<"random_id/group12">>,
    Result = resolved_list(Stores, RawPath),
    try
        ?assertEqual({ok, [<<"data">>]}, Result)
    after
        shutdown_stores(Stores)
    end.

%% Test utilities

%% @doc Initialize multiple stores
get_multiple_stores() -> 
    get_multiple_stores(hb_store_lmdb).
get_multiple_stores(StoreModule) -> 
    Store1 = hb_test_utils:test_store(StoreModule, <<"store1">>),
    Store2 = hb_test_utils:test_store(StoreModule, <<"store2">>),
    hb_store:reset(Store1),
    hb_store:reset(Store2),
    [Store1, Store2].

%% @doc Shutdown multiple stores
shutdown_stores([]) -> ok;
shutdown_stores([Store | RemainingStores]) -> 
    hb_store:reset(Store),
    hb_store:stop(Store),
    shutdown_stores(RemainingStores).
