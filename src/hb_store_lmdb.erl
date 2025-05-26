% @doc An lmdb (Lighting Memory Db) implementation of the hb_store
-module(hb_store_lmdb).
-export([start/1, stop/1, scope/0, scope/1, reset/1]).
-export([read/2, write/3]).

-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(DEFAULT_SIZE, 16 * 1024 * 1024 * 1024).
-define(CONNECT_TIMEOUT, 3000).
-define(DEFAULT_IDLE_FLUSH_TIME, 5).
-define(DEFAULT_MAX_FLUSH_TIME, 5).

%% @doc Start singleton process holding lmdb env.
start(StoreOpts = #{ <<"prefix">> := DataDir, <<"max-size">> := MaxSize}) ->
    case whereis(?MODULE) of
        undefined ->
            {ok, Env} = lmdb:env_create(DataDir, #{ max_mapsize => MaxSize }),
            % Create the server process
            Server = start_server(StoreOpts#{ <<"env">> => Env }),
            hb_name:register({?MODULE, DataDir}, Server),
            {ok, Server};
        Pid -> 
            {ok, Pid}
    end.

%% @doc Write value to lmdb by key by sending a message to the server process.
write(StoreOpts, Key, Value) ->
    PID = ensure_instance(StoreOpts),
    PID ! {write, Key, Value},
    ok.

%% @doc Read value from lmdb by key.
read(StoreOpts, Key) ->
    case lmdb:get(get_env(StoreOpts), Key) of
        {ok, Value} ->
            {ok, Value};
        not_found ->
            ?event(read_miss, {miss, Key}),
            ensure_instance(StoreOpts) ! {flush, self(), Ref = make_ref()},
            receive
                {flushed, Ref} -> lmdb:get(get_env(StoreOpts), Key)
            after ?CONNECT_TIMEOUT -> {error, timeout}
            end
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
    Env = get_env(StoreOpts),
    PID ! stop,
    lmdb:env_close(Env),
    ok.

%% @doc Reset (delete!) the entire database.
reset(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    stop(StoreOpts),
    os:cmd(binary_to_list(<< "rm -Rf ", DataDir/binary >>)),
    ok.

%% @doc Start the server process.
start_server(StoreOpts) ->
    spawn(
        fun() ->
            spawn_link(fun() -> commit_manager(StoreOpts, self()) end),
            server(StoreOpts)
        end
    ).

%% @doc Listen for messages on singleton process, periodically sync the database.
server(State) ->
    receive
        {get_env, From, Ref} ->
            From ! {env, maps:get(<<"env">>, State), Ref},
            server(State);
        {write, Key, Value} ->
            server(server_write(State, Key, Value));
        {flush, From, Ref} ->
            NewState = server_flush(State),
            From ! {flushed, Ref},
            server(NewState);
        stop ->
            NewState = server_flush(State),
            lmdb:env_close(maps:get(<<"env">>, NewState)),
            ok
    after
        maps:get(<<"idle-flush-time">>, State, ?DEFAULT_IDLE_FLUSH_TIME) ->
        server(server_flush(State))
    end.

%% @doc Write value to lmdb `put`ing it into the current transaction. If the 
%% transaction does not exist, create it.
server_write(RawState, Key, Value) ->
    State = ensure_transaction(RawState),
    lmdb_nif:put(
        maps:get(<<"transaction">>, State),
        maps:get(<<"instance">>, State),
        Key,
        Value,
        0
    ),
    State.

%% @doc Flush the current transaction to the database.
server_flush(RawState) ->
    State = ensure_transaction(RawState),
    lmdb_nif:txn_commit(maps:get(<<"transaction">>, State)),
    State#{ <<"transaction">> => undefined, <<"instance">> => undefined }.

%% @doc A monitor process that periodically flushes the transaction to the
%% database.
commit_manager(StoreOpts, Server) ->
    Time = maps:get(<<"max-flush-time">>, StoreOpts, ?DEFAULT_MAX_FLUSH_TIME),
    receive after Time ->
        Server ! {flush, self(), Ref = make_ref()},
        receive
            {flushed, Ref} ->
                commit_manager(StoreOpts, Server)
        after ?CONNECT_TIMEOUT -> timeout
        end,
        commit_manager(StoreOpts, Server)
    end.

%% @doc Ensure that a server state has a live transaction.
ensure_transaction(State) ->
    case maps:get(<<"transaction">>, State, undefined) of
        undefined ->
            {ok, Txn} =
                lmdb_nif:txn_begin(
                    maps:get(<<"env">>, State),
                    undefined,
                    0
                ),
            {ok, Dbi} = lmdb:open_db(Txn, default),
            State#{<<"transaction">> => Txn, <<"instance">> => Dbi};
        _ ->
            State
    end.

%% Tests
basic_test() ->
    StoreOpts = #{
        <<"prefix">> => <<"/tmp/store-1">>,
        <<"max-size">> => ?DEFAULT_SIZE
    },
    Res = write(StoreOpts, <<"Hello">>, <<"World2">>),
    ?assertEqual(ok, Res),
    {ok, Value} = read(StoreOpts, <<"Hello">>),
    ?assertEqual(Value, <<"World2">>),
    ok = stop(StoreOpts).
