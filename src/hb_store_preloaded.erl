%%% @doc A store module that reads keys from the preloaded device modules of 
%%% a node.
-module(hb_store_preloaded).
-export([start/1, scope/1, type/2, read/2, write/3, make_link/3, resolve/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_DEVICE_ID, <<"message@1.0">>).

%% @doc Invoke the `module_info/0' function on all preloaded device modules.
start(Preloaded) ->
    maps:map(fun(_, Mod) -> Mod:module_info() end, Preloaded),
    ok.

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
        [BaseID, FunctionString] ?= hb_path:term_to_path_parts(Key),
        {ok, ModName} ?= maps:find(BaseID, StoreOpts),
        {Status, FunctionKey} =
            try {ok, hb_util:key_to_atom(FunctionString)}
            catch _:_ -> {error, undefined}
            end,
        ?event(
            {finding_resolver_function,
                {base, BaseID},
                {function_key, FunctionString},
                {device_module, ModName}
            }
        ),
        if (FunctionKey == '*') orelse Status =/= ok ->
            default_function(StoreOpts, ModName, BaseID, FunctionString);
        true ->
            case max_arity(ModName, FunctionKey) of
                {ok, MaxArity} ->
                    {ok,
                        #{
                            <<"function">> => fun ModName:FunctionKey/MaxArity,
                            <<"vary">> =>
                                fun(Base, Req, Opts) ->
                                    hb_types:vary(ModName, Req, Base, Opts)
                                end
                        }
                    };
                not_found ->
                    default_function(StoreOpts, ModName, BaseID, FunctionString)
            end
        end
    else _ -> not_found
    end.

%% @doc Find the maximum arity of a function exported by a module.
max_arity(Mod, info) -> max_arity(Mod, info, 2);
max_arity(Mod, Function) -> max_arity(Mod, Function, 4).
max_arity(Mod, Function, MaxArity) when MaxArity >= 0 ->
    ?event({finding_max_arity, {mod, Mod}, {function, Function}, {max_arity, MaxArity}}),
    case erlang:function_exported(Mod, Function, MaxArity) of
        false -> max_arity(Mod, Function, MaxArity - 1);
        true -> {ok, MaxArity}
    end;
max_arity(_Mod, _Function, _MaxArity) ->
    not_found.

%% @doc Return the default function for a device. Uses `.` if exported, otherwise
%% checks `info/handler'.
default_function(StoreOpts, ModName, BaseID, Key) ->
    case max_arity(ModName, '*') of
        {ok, MaxArity} -> {ok, fun ModName:'*'/MaxArity};
        not_found ->
            case info(StoreOpts, BaseID) of
                #{ default := Fun } when is_function(Fun) ->
                    case erlang:fun_info(Fun, arity) of
                        {arity, 4} ->
                            {
                                ok,
                                fun(Base, Req, Opts) ->
                                    Fun(Key, Base, Req, Opts)
                                end
                            };
                        _ -> {ok, Fun}
                    end;
                #{ default := DefaultDevice } when is_binary(DefaultDevice) ->
                    read(
                        StoreOpts,
                        <<
                            DefaultDevice/binary,
                            "/",
                            Key/binary
                        >>
                    );
                _ -> not_found
            end
    end.

info(StoreOpts, BaseID) ->
    case read(StoreOpts, <<BaseID/binary, "/info">>) of
        {ok, Info} -> Info();
        not_found -> info(StoreOpts, ?DEFAULT_DEVICE_ID)
    end.  

%% @doc Store is read-only, so writing is not supported.
write(_Opts, _Key, _Value) ->
    not_found.

%% @doc Store is read-only, so linking is not supported.
make_link(_Opts, _Source, _Destination) ->
    not_found.

%%% Tests

default_preloaded_store() ->
    application:ensure_all_started(hb),
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

find_message_default_test() ->
    PreloadedStore = default_preloaded_store(),
    {ok, Fun} = hb_store_preloaded:read(PreloadedStore, <<"message@1.0/abc">>),
    ?event({default_func, Fun}),
    FunInfo = erlang:fun_info(Fun),
    ?event(preloaded, {fun_info, FunInfo}),
    ?assertEqual(fun dev_message:get/4, unwrap_4_fun(Fun)).

find_message_default_device_test() ->
    PreloadedStore = default_preloaded_store(),
    start(PreloadedStore),
    {ok, Fun} = hb_store_preloaded:read(PreloadedStore, <<"test-device@1.0/xyz">>),
    ?event({default_func, Fun}),
    ?assertEqual(fun dev_message:get/4, unwrap_4_fun(Fun)).

%%% Test utilities

unwrap_4_fun(Fun) ->
    {env, [_X, InnerFunc]} = erlang:fun_info(Fun, env),
    InnerFunc.