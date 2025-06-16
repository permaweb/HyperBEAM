-module(ar_timestamp).
-export([start/0, get/0]).
-define(TIMEOUT, 1000 * 15).
-include_lib("include/hb.hrl").

%%% A simple Erlang server that caches the current Arweave timestamp and
%%% refreshes it periodically.


%% @doc Check if the server is already running, and if not, start it.
start() ->
    ?event(starting_ar_timestamp_server),
    case whereis(?MODULE) of
        undefined -> spawn_server();
        PID ->
            case is_process_alive(PID) of
                true -> PID;
                false -> spawn_server()
            end
    end.


%% @doc Spawn a new server and its refresher.
spawn_server() ->
    TSServer =
        spawn(fun() -> cache(hb_client:arweave_timestamp()) end),
    spawn(fun() -> refresher(TSServer) end),
    register(?MODULE, TSServer),
    TSServer.


%% @doc Get the current timestamp from the server, starting the server if it
%% isn't already running.
get() ->
    ?event(getting_ar_timestamp),
    PID = start(),
    ?event({got_ar_timestamp_pid, PID}),
    PID ! {get, self()},
    ?event(waiting_for_ar_timestamp),
    receive
        {timestamp, Timestamp} ->
            ?event({got_ar_timestamp, Timestamp}),
            Timestamp
    end.


%% @doc Cache the current timestamp from Arweave.
cache(Current) ->
    ?event(cache_waiting),
    receive
        {get, Pid} ->
            ?event({got_get_request, Pid}),
            Pid ! {timestamp, Current},
            ?event({sent_timestamp, Current}),
            cache(Current);
        {refresh, New} ->
            ?event({refreshed_ar_timestamp, New}),
            cache(New)
    end.


%% @doc Refresh the timestamp cache periodically.
refresher(TSServer) ->
    timer:sleep(?TIMEOUT),
    TS =
        case hb_opts:get(mode) of
            debug -> {0, 0, <<0:256>>};
            prod -> hb_client:arweave_timestamp()
        end,
    TSServer ! {refresh, TS},
    refresher(TSServer).
