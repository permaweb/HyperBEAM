%%% @doc A store module that reads keys from the preloaded device modules of 
%%% a node.
-module(hb_store_preloaded).
-export([scope/1, type/2, read/2, write/3, make_link/3, resolve/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Return the scope of this store.
scope(_StoreOpts) ->
    local.

%% @doc No-op resolution.
resolve(_StoreOpts, Key) ->
    Key.

%% @doc Determine the type of value at a given key. If the key exists, it is
%% a simple value. If not, return not_found.
type(StoreOpts, Key) ->
    case read(StoreOpts, Key) of
        not_found -> not_found;
        _ -> simple
    end.

%% @doc Read a key from a preloaded device module, if it exists.
read(StoreOpts, Key) ->
    ?event({reading_preloaded_key, {key, Key}, {store_opts, StoreOpts}}),
    maybe
        [Device, FunctionString] ?= hb_path:term_to_path_parts(Key),
        {ok, ModName} ?= maps:find(Device, StoreOpts),
        FunctionKey = hb_util:key_to_atom(FunctionString),
        ?event(
            {finding_max_arity,
                {mod_name, ModName},
                {function_key, FunctionKey},
                {module, ModName},
                {module_info, erlang:module_info(ModName)}
            }
        ),
        {ok, MaxArity} = max_arity(ModName, FunctionKey),
        {ok, fun ModName:FunctionKey/MaxArity}
    else _ -> not_found
    end.

max_arity(Mod, Function) -> max_arity(Mod, Function, 4).
max_arity(Mod, Function, MaxArity) when MaxArity > 0 ->
    case erlang:function_exported(Mod, Function, MaxArity) of
        false -> max_arity(Mod, Function, MaxArity - 1);
        true -> {ok, MaxArity}
    end;
max_arity(_Mod, _Function, _MaxArity) ->
    not_found.

%% @doc Store is read-only, so writing is not supported.
write(_Opts, _Key, _Value) ->
    not_found.

%% @doc Store is read-only, so linking is not supported.
make_link(_Opts, _Source, _Destination) ->
    not_found.

%%% Tests

default_preloaded_store() ->
    #{store := Stores} = hb_opts:default_message(),
    [PreloadedStore] =
        lists:filter(
            fun(#{ <<"store-module">> := hb_store_preloaded }) -> true;
                (_) -> false
            end,
            Stores
        ),
    PreloadedStore.

find_message_set_test() ->
    PreloadedStore = default_preloaded_store(),
    Result = hb_store_preloaded:read(PreloadedStore, <<"message@1.0/set">>),
    io:format("Result: ~p~n", [Result]),
    ?assertEqual({ok, fun dev_message:set/3}, Result).
