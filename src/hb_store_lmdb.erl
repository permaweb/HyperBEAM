% @doc An lmdb (Lighting Memory Db) implementation of the hb_store
-module(hb_store_lmdb).
-export([start/1, stop/0, stop/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(DEFAULT_SIZE, 16 * 1024 * 1024 * 1024).

%% @doc Start singleton process holding lmdb env.
start(#{ <<"prefix">> := DataDir, <<"max-size">> := MaxSize}) ->
    case whereis(?MODULE) of
        undefined ->
            {ok, Env} = lmdb:env_create(),
            lmdb:env_set_mapsize(Env, MaxSize),
            % TODO: ensure_dir(DataDir)
            lmdb:env_open(Env, binary_to_list(DataDir)),
            Pid = spawn(fun() -> loop(Env) end),
            register(?MODULE, Pid),
            {ok, Pid};
        Pid -> 
            {ok, Pid}
    end.

%% @doc Get env from singleton process.
get_env() ->
    ?MODULE ! {get_env, self()},
    receive
        {env, Env} -> {ok, Env}
    after 5000 -> timeout
    end.

%% @doc Stop lmdb singleton process.
stop() ->
    stop(#{}).

stop(_) ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid ->
            Pid ! stop,
            ok
    end.

%% @doc listen for messages on singleton process.
loop(Env) ->
    receive
        {get_env, From} ->
            From ! {env, Env},
            loop(Env);
        stop ->
            lmdb:env_close(Env)
    end.

write(_, Key, Value) ->
    case get_env() of
        {ok, Env} ->
            lmdb:with_txn(Env, fun(Txn) ->
                {ok, Dbi} = lmdb:open_db(Txn, default),
                lmdb:put(Txn, Dbi, Key, Value),
                lmdb:close_db(Env, Dbi)
            end)
    end,
    ?event({ write_key, Key}),
    {ok}.

%% @doc Read value from lmdb by key.
read(_, Key) ->
    case get_env() of
        {ok, Env} ->
            lmdb:with_txn(Env, fun(Txn) ->
                {ok, Dbi} = lmdb:open_db(Txn, default),
                {ok, Value } =lmdb:get(Txn, Dbi, Key),
                lmdb:close_db(Env, Dbi),
                Value
            end)
    end.

%% Tests
basic_test() ->
    { Result, _ } = start(#{ <<"prefix">> => <<"/tmp/store-1">>, <<"max-size">> => ?DEFAULT_SIZE }),
    write(#{}, <<"Hello">>, <<"World2">>),
    {ok, Value} = read(#{}, <<"Hello">>),
    ?assertEqual(Value, <<"World2">>),
    stop(#{}),
    ?assertEqual(ok, Result).
