% @doc An lmdb (Lighting Memory Db) implementation of the hb_store
-module(hb_store_lmdb).
-export([start/1, stop/1, scope/0, scope/1]).
-export([read/2, write/3]).

-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(DEFAULT_SIZE, 16 * 1024 * 1024 * 1024).
-define(CONNECT_TIMEOUT, 3000).

%% @doc Start singleton process holding lmdb env.
start(#{ <<"prefix">> := DataDir, <<"max-size">> := MaxSize}) ->
    case whereis(?MODULE) of
        undefined ->
            {ok, Env} = lmdb:env_create(DataDir, #{ max_mapsize => MaxSize }),
            % Create singleton
            Pid = spawn(fun() -> loop(Env) end),
            hb_name:register({?MODULE, DataDir}, Pid),
            {ok, Pid};
        Pid -> 
            {ok, Pid}
    end.

% @doc The lmdb store is always local for now.
scope() -> local.
scope(_) -> scope().

%% @doc Get env from singleton process.
get_env(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    case get({?MODULE, DataDir}) of
        undefined ->
            PID = ensure_instance(StoreOpts),
            PID ! {get_env, self(), Ref = make_ref()},
            receive
                {env, Env, Ref} ->
                    put({?MODULE, DataDir}, Env),
                    Env
            after ?CONNECT_TIMEOUT -> timeout
            end;
        Env -> Env
    end.

%% @doc Ensure that the requested instance exists. If it doesn't, start it.
ensure_instance(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    case hb_name:lookup({?MODULE, DataDir}) of
        undefined ->
            {ok, Pid} = start(StoreOpts),
            Pid;
        Pid -> Pid
    end.

%% @doc Stop lmdb singleton process.
stop(StoreOpts) ->
    PID = ensure_instance(StoreOpts),
    PID ! stop,
    ok.

%% @doc listen for messages on singleton process.
loop(Env) ->
    receive
        {get_env, From, Ref} ->
            From ! {env, Env, Ref},
            loop(Env);
        stop ->
            lmdb:env_close(Env)
    end.

write(StoreOpts, Key, Value) ->
    lmdb:put(get_env(StoreOpts), Key, Value).

%% @doc Read value from lmdb by key.
read(StoreOpts, Key) ->
    lmdb:get(get_env(StoreOpts), Key).

%% Tests
basic_test() ->
    StoreOpts = #{
        <<"prefix">> => <<"/tmp/store-1">>,
        <<"max-size">> => ?DEFAULT_SIZE
    },
    write(StoreOpts, <<"Hello">>, <<"World2">>),
    {ok, Value} = read(StoreOpts, <<"Hello">>),
    ?assertEqual(Value, <<"World2">>),
    ok = stop(StoreOpts).
