% @doc An lmdb (Lighting Memory Db) implementation of the hb_store
-module(hb_store_lmdb).
-export([start/1, stop/1, scope/0, scope/1, reset/1]).
-export([read/2, write/3]).

-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(DEFAULT_SIZE, 16 * 1024 * 1024 * 1024).
-define(CONNECT_TIMEOUT, 3000).
-define(DEFAULT_IDLE_FLUSH_TIME, 5).
-define(DEFAULT_MAX_FLUSH_TIME, 50).

%% @doc Start singleton process holding lmdb env.
start(StoreOpts) ->
    {ok, find_or_spawn_instance(StoreOpts)}.

%% @doc Write value to lmdb by key by sending a message to the server process.
write(StoreOpts, Key, Value) ->
    PID = find_or_spawn_instance(StoreOpts),
    PID ! {write, Key, Value},
    ok.

%% @doc Read value from lmdb by key.
read(StoreOpts, Key) ->
    case lmdb:get(find_env(StoreOpts), Key) of
        {ok, Value} ->
            {ok, Value};
        not_found ->
            ?event(read_miss, {miss, Key}),
            find_or_spawn_instance(StoreOpts) ! {flush, self(), Ref = make_ref()},
            receive
                {flushed, Ref} -> lmdb:get(find_env(StoreOpts), Key)
            after ?CONNECT_TIMEOUT -> {error, timeout}
            end
    end.

% @doc The lmdb store is always local for now.
scope() -> local.
scope(_) -> scope().

%% @doc Get env from singleton process.
find_env(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    case get({?MODULE, DataDir}) of
        undefined ->
            ?event(debug_process_cache, {not_in_env_cache, {?MODULE, DataDir}}),
            PID = find_or_spawn_instance(StoreOpts),
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
find_or_spawn_instance(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    case get({?MODULE, {server, DataDir}}) of
        undefined ->
            ?event(debug_process_cache, {not_in_process_cache, {?MODULE, DataDir}}),
            case hb_name:lookup({?MODULE, DataDir}) of
                undefined ->
                    % Create the server process
                    Pid = start_server(StoreOpts),
                    Pid;
                Pid -> Pid
            end;
        Pid ->
            Pid
    end.

%% @doc Stop lmdb singleton process.
stop(StoreOpts) ->
    PID = find_or_spawn_instance(StoreOpts),
    Env = find_env(StoreOpts),
    PID ! stop,
    lmdb:env_close(Env),
    ok.

%% @doc Reset (delete!) the entire database.
reset(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    stop(StoreOpts),
    os:cmd(binary_to_list(<< "rm -Rf ", DataDir/binary >>)),
    ok.

%% @doc Start the server process.
start_server(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    filelib:ensure_dir(
        binary_to_list(hb_util:bin(DataDir)) ++ "/mbd.data"
    ),
    {ok, Env} =
        lmdb:env_create(
            DataDir,
            #{
                max_mapsize => maps:get(<<"max-size">>, StoreOpts, ?DEFAULT_SIZE)
            }
        ),
    ServerOpts = StoreOpts#{ <<"env">> => Env },
    Server = 
        spawn(
            fun() ->
                spawn_link(fun() -> commit_manager(ServerOpts, self()) end),
                server(ServerOpts)
            end
        ),
    put({?MODULE, {server, DataDir}}, Server),
    Server.

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
            server_flush(State),
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
    case maps:get(<<"transaction">>, RawState, undefined) of
        undefined ->
            RawState;
        _ ->
            Res = lmdb_nif:txn_commit(maps:get(<<"transaction">>, RawState)),
            notify_flush(RawState),
            RawState#{ <<"transaction">> => undefined, <<"instance">> => undefined }
    end.

%% @doc Signal to all flush requesters that the flush is complete.
notify_flush(State) ->
    receive
        {flush, From, Ref} ->
            From ! {flushed, Ref},
            notify_flush(State)
    after 0 ->
        ok
    end.

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
