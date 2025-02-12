-module(hb_store).
-export([behavior_info/1]).
-export([start/1, stop/1, reset/1]).
-export([filter/2, scope/2, sort/2]).
-export([type/2, read/2, write/3, list/2]).
-export([path/1, path/2, add_path/2, add_path/3, join/1]).
-export([make_group/2, make_link/3, resolve/2]).
-export([generate_test_suite/1, generate_test_suite/2, test_stores/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% A simple abstraction layer for AO key value store operations.
%%% This interface allows us to swap out the underlying store
%%% implementation(s) as desired.
%%% 
%%% It takes a list of modules and their options, and calls the appropriate
%%% function on the first module that succeeds. If all modules fail, it returns
%%% {error, no_viable_store}.

behavior_info(callbacks) ->
    [
        {start, 1}, {stop, 1}, {reset, 1}, {make_group, 2}, {make_link, 3},
        {type, 2}, {read, 2}, {write, 3},
        {list, 2}, {path, 2}, {add_path, 3}
    ].

-define(DEFAULT_SCOPE, local).

%%% Library wrapper implementations.

start(Modules) -> call_all(Modules, start, []).

stop(Modules) -> call_function(Modules, stop, []).

%% @doc Takes a store object and a filter function or match spec, returning a
%% new store object with only the modules that match the filter. The filter
%% function takes 2 arguments: the scope and the options. It calls the store's
%% scope function to get the scope of the module.
filter(Module, Filter) when not is_list(Module) ->
    filter([Module], Filter);
filter(Modules, Filter) ->
    lists:filter(
        fun(Store) ->
            try Filter(get_store_scope(Store), Store)
            catch _:_ -> false
            end
        end,
        Modules
    ).

%% @doc Limit the store scope to only a specific (set of) option(s).
%% Takes either a single scope or a list of scopes.
scope(Store, Scope) ->
    filter(
        Store,
        fun(StoreScope, _) ->
            StoreScope == Scope orelse
                (is_list(Scope) andalso lists:member(StoreScope, Scope))
        end
    ).

%% @doc Ask a store for its own scope. If it doesn't have one, return the
%% default scope (local).
get_store_scope(Store) ->
    case call_function(Store, scope, []) of
        no_viable_store -> ?DEFAULT_SCOPE;
        Scope -> Scope
    end.

%% @doc Order a store by a preference of its scopes. This is useful for making
%% sure that faster (or perhaps cheaper) stores are used first. If a list is
%% provided, it will be used as a preference order. If a map is provided,
%% scopes will be ordered by the scores in the map. Any unknown scopes will
%% default to a score of 0.
sort(Stores, PreferenceOrder) when is_list(PreferenceOrder) ->
    sort(
        Stores,
        maps:from_list(
            [
                {Scope, -Index}
            ||
                {Scope, Index} <-
                    lists:zip(
                        PreferenceOrder,
                        lists:seq(1, length(PreferenceOrder))
                    )
            ]
        )
    );
sort(Stores, ScoreMap) ->
    lists:sort(
        fun(Store1, Store2) ->
            maps:get(get_store_scope(Store1), ScoreMap, 0) >
                maps:get(get_store_scope(Store2), ScoreMap, 0)
        end,
        Stores
    ).

%% @doc Join a list of path components together.
join(Path) -> hb_path:to_binary(Path).

%%% The store interface that modules should implement.

%% @doc Read a key from the store.
read(Modules, Key) -> call_function(Modules, read, [Key]).

%% @doc Write a key with a value to the store.
write(Modules, Key, Value) -> call_function(Modules, write, [Key, Value]).

%% @doc Make a group in the store. A group can be seen as a namespace or
%% 'directory' in a filesystem.
make_group(Modules, Path) -> call_function(Modules, make_group, [Path]).

%% @doc Make a link from one path to another in the store.
make_link(Modules, Existing, New) ->
    call_function(Modules, make_link, [Existing, New]).

%% @doc Delete all of the keys in a store. Should be used with extreme
%% caution. Lost data can lose money in many/most of hyperbeam's use cases.
reset(Modules) -> call_function(Modules, reset, []).

%% @doc Get the type of element of a given path in the store. This can be
%% a performance killer if the store is remote etc. Use only when necessary.
type(Modules, Path) -> call_function(Modules, type, [Path]).

%% @doc Create a path from a list of path components. If no store implements
%% the path function, we return the path with the 'default' transformation (id).
path(Path) -> join(Path).
path(_, Path) -> path(Path).

%% @doc Add two path components together. If no store implements the add_path
%% function, we concatenate the paths.
add_path(Path1, Path2) -> Path1 ++ Path2.
add_path(Store, Path1, Path2) ->
    case call_function(Store, add_path, [Path1, Path2]) of
        no_viable_store -> add_path(Path1, Path2);
        Result -> Result
    end.

%% @doc Follow links through the store to resolve a path to its ultimate target.
resolve(Modules, Path) -> call_function(Modules, resolve, [Path]).

%% @doc List the keys in a group in the store. Use only in debugging.
%% The hyperbeam model assumes that stores are built as efficient hash-based
%% structures, so this is likely to be very slow for most stores.
list(Modules, Path) -> call_function(Modules, list, [Path]).

%% @doc Call a function on the first store module that succeeds. Returns its
%% result, or no_viable_store if none of the stores succeed.
call_function(X, _Function, _Args) when not is_list(X) ->
    call_function([X], _Function, _Args);
call_function([], _Function, _Args) ->
    no_viable_store;
call_function([{Mod, Opts} | Rest], Function, Args) ->
    ?event({calling, Mod, Function, Args}),
    try apply(Mod, Function, [Opts | Args]) of
        not_found ->
            call_function(Rest, Function, Args);
        Result ->
            Result
    catch
        Class:Reason:Stacktrace ->
            ?event(error, {store_call_failed, {Class, Reason, Stacktrace}}),
            call_function(Rest, Function, Args)
    end.

%% @doc Call a function on all modules in the store.
call_all(X, _Function, _Args) when not is_list(X) ->
    call_all([X], _Function, _Args);
call_all([], _Function, _Args) ->
    ok;
call_all([{Mod, Opts} | Rest], Function, Args) ->
    try
        apply(Mod, Function, [Opts | Args])
    catch
        Class:Reason:Stacktrace ->
            ?event(error, {store_call_failed, {Class, Reason, Stacktrace}}),
            ok
    end,
    call_all(Rest, Function, Args).

%%% Test helpers

test_stores() ->
    [
        {hb_store_rocksdb, #{ prefix => "TEST-cache-rocks" }},
        {hb_store_fs, #{ prefix => "TEST-cache-fs" }}
    ].

generate_test_suite(Suite) ->
    generate_test_suite(Suite, test_stores()).
generate_test_suite(Suite, Stores) ->
    lists:map(
        fun(Store = {Mod, _Opts}) ->
            {foreach,
                fun() -> hb_store:start(Store) end,
                fun(_) -> hb_store:reset(Store) end,
                [
                    {atom_to_list(Mod) ++ ": " ++ Desc,
                        fun() -> Test(#{ store => Store }) end}
                ||
                    {Desc, Test} <- Suite
                ]
            }
        end,
        Stores
    ).

%%% Tests

%% @doc Test path resolution dynamics.
simple_path_resolution_test(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ok = hb_store:write(Store, <<"test-file">>, <<"test-data">>),
    hb_store:make_link(Store, <<"test-file">>, <<"test-link">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, <<"test-link">>)).

%% @doc Ensure that we can resolve links recursively.
resursive_path_resolution_test(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:write(Store, <<"test-file">>, <<"test-data">>),
    hb_store:make_link(Store, <<"test-file">>, <<"test-link">>),
    hb_store:make_link(Store, <<"test-link">>, <<"test-link2">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, <<"test-link2">>)).

%% @doc Ensure that we can resolve links through a directory.
hierarchical_path_resolution_test(Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    hb_store:make_group(Store, <<"test-dir1">>),
    hb_store:write(Store, [<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
    hb_store:make_link(Store, [<<"test-dir1">>], <<"test-link">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, [<<"test-link">>, <<"test-file">>])).

store_suite_test_() ->
    hb_store:generate_test_suite([
        {"simple path resolution", fun simple_path_resolution_test/1},
        {"resursive path resolution", fun resursive_path_resolution_test/1},
        {"hierarchical path resolution", fun hierarchical_path_resolution_test/1}
    ]).