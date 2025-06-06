-module(hb_terminal_capture).
-behaviour(gen_server).
-export([start_link/0, get_logs/0, get_logs/1, clear_logs/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_LOGS, 1000).
-define(SERVER, ?MODULE).

-record(state, {
    logs = [],
    max_logs = ?MAX_LOGS,
    port = undefined,
    log_file = undefined
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

%% Callbacks
init([]) ->
    % Create a log file to capture all output
    LogFile = "/tmp/hyperbeam_output.log",
    
    % Create the log file if it doesn't exist
    file:write_file(LogFile, <<>>),
    
    % Start a port to tail the log file
    Port = erlang:open_port({spawn, "tail -f " ++ LogFile}, [
        stream, 
        {line, 1024}, 
        exit_status,
        stderr_to_stdout
    ]),
    
    % Set up a timer to periodically capture console output
    timer:send_interval(100, self(), capture_console),
    
    {ok, #state{port = Port, log_file = LogFile}}.

handle_call({get_logs, Count}, _From, #state{logs = Logs} = State) ->
    RecentLogs = lists:sublist(lists:reverse(Logs), Count),
    {reply, RecentLogs, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(clear_logs, State) ->
    {noreply, State#state{logs = []}};

handle_cast(_Msg, State) ->
    {noreply, State}.

% Handle output from the tail command
handle_info({Port, {data, {eol, Line}}}, #state{port = Port, logs = Logs, max_logs = MaxLogs} = State) ->
    Timestamp = format_timestamp(os:timestamp()),
    LogEntry = #{
        timestamp => Timestamp,
        message => unicode:characters_to_binary(Line),
        raw_timestamp => os:timestamp()
    },
    NewLogs = lists:sublist([LogEntry | Logs], MaxLogs),
    {noreply, State#state{logs = NewLogs}};

handle_info({Port, {data, {noeol, Line}}}, #state{port = Port, logs = Logs, max_logs = MaxLogs} = State) ->
    Timestamp = format_timestamp(os:timestamp()),
    LogEntry = #{
        timestamp => Timestamp,
        message => unicode:characters_to_binary(Line),
        raw_timestamp => os:timestamp()
    },
    NewLogs = lists:sublist([LogEntry | Logs], MaxLogs),
    {noreply, State#state{logs = NewLogs}};

handle_info({Port, {exit_status, _Status}}, #state{port = Port} = State) ->
    % Port exited, restart it
    LogFile = State#state.log_file,
    NewPort = erlang:open_port({spawn, "tail -f " ++ LogFile}, [
        stream, 
        {line, 1024}, 
        exit_status,
        stderr_to_stdout
    ]),
    {noreply, State#state{port = NewPort}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
    case Port of
        undefined -> ok;
        _ -> erlang:port_close(Port)
    end,
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
