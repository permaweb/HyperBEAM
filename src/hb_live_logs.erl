-module(hb_live_logs).
-behaviour(gen_server).
-export([start_link/0, get_logs/0, get_logs/1, clear_logs/0, add_log/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_LOGS, 1000).
-define(SERVER, ?MODULE).

-record(state, {
    logs = [],
    max_logs = ?MAX_LOGS
}).

%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_logs() ->
    get_logs(?MAX_LOGS).

get_logs(Count) ->
    gen_server:call(?SERVER, {get_logs, Count}).

clear_logs() ->
    gen_server:cast(?SERVER, clear_logs).

add_log(Message) ->
    gen_server:cast(?SERVER, {add_log, Message}).

%% Callbacks
init([]) ->
    % Install ourselves as the default group leader to capture io:format calls
    process_flag(trap_exit, true),
    % Add a startup message
    add_log("HyperBEAM logs initilized"),
    {ok, #state{}}.

handle_call({get_logs, Count}, _From, #state{logs = Logs} = State) ->
    RecentLogs = lists:sublist(lists:reverse(Logs), Count),
    {reply, RecentLogs, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(clear_logs, State) ->
    {noreply, State#state{logs = []}};

handle_cast({add_log, Message}, #state{logs = Logs, max_logs = MaxLogs} = State) ->
    Timestamp = format_timestamp(os:timestamp()),
    LogEntry = #{
        timestamp => Timestamp,
        message => unicode:characters_to_binary(Message),
        raw_timestamp => os:timestamp()
    },
    NewLogs = lists:sublist([LogEntry | Logs], MaxLogs),
    {noreply, State#state{logs = NewLogs}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
format_timestamp({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = 
        calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
    Millisecs = MicroSecs div 1000,
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0w",
                  [Year, Month, Day, Hour, Min, Sec, Millisecs]).
