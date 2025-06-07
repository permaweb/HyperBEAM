-module(hb_log_capture).
-behaviour(gen_server).
-behaviour(gen_event).
-export([start_link/0, get_logs/0, get_logs/1, clear_logs/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([log/2, adding_handler/1, removing_handler/1]).
-export([handle_event/2, handle_call/2, handle_info/2]).

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

%% Callbacks
init([]) ->
    % Install a logger handler to capture all log messages
    logger:add_handler(hb_log_capture_handler, ?MODULE, #{
        level => all,
        formatter => {logger_formatter, #{}}
    }),
    % Also try to capture io:format output
    group_leader(self(), self()),
    % Try to intercept standard_error by registering as the error_logger
    error_logger:add_report_handler(?MODULE, self()),
    {ok, #state{}}.

handle_call({get_logs, Count}, _From, #state{logs = Logs} = State) ->
    RecentLogs = lists:sublist(lists:reverse(Logs), Count),
    {reply, RecentLogs, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(clear_logs, State) ->
    {noreply, State#state{logs = []}};

handle_cast({add_log, LogEntry}, #state{logs = Logs, max_logs = MaxLogs} = State) ->
    NewLogs = lists:sublist([LogEntry | Logs], MaxLogs),
    {noreply, State#state{logs = NewLogs}};

handle_cast(_Msg, State) ->
    {noreply, State}.

% Capture io:format output
handle_info({io_request, From, ReplyAs, Request}, #state{logs = Logs, max_logs = MaxLogs} = State) ->
    case Request of
        {put_chars, Encoding, Chars} ->
            Timestamp = format_timestamp(os:timestamp()),
            LogEntry = #{
                timestamp => Timestamp,
                message => unicode:characters_to_binary(Chars, Encoding),
                raw_timestamp => os:timestamp()
            },
            NewLogs = lists:sublist([LogEntry | Logs], MaxLogs),
            From ! {io_reply, ReplyAs, ok},
            {noreply, State#state{logs = NewLogs}};
        {put_chars, Chars} ->
            Timestamp = format_timestamp(os:timestamp()),
            LogEntry = #{
                timestamp => Timestamp,
                message => unicode:characters_to_binary(Chars),
                raw_timestamp => os:timestamp()
            },
            NewLogs = lists:sublist([LogEntry | Logs], MaxLogs),
            From ! {io_reply, ReplyAs, ok},
            {noreply, State#state{logs = NewLogs}};
        _ ->
            From ! {io_reply, ReplyAs, {error, request}},
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Logger handler callbacks
log(LogEvent, Config) ->
    #{level := Level, msg := Msg, meta := Meta} = LogEvent,
    Timestamp = format_timestamp(os:timestamp()),
    FormattedMsg = case Msg of
        {string, String} -> unicode:characters_to_binary(String);
        {report, Report} -> unicode:characters_to_binary(io_lib:format("~p", [Report]));
        {Format, Args} -> unicode:characters_to_binary(io_lib:format(Format, Args))
    end,
    LogEntry = #{
        timestamp => Timestamp,
        message => FormattedMsg,
        raw_timestamp => os:timestamp()
    },
    gen_server:cast(?SERVER, {add_log, LogEntry}),
    Config.

adding_handler(Config) ->
    {ok, Config}.

removing_handler(_Config) ->
    ok.

%% gen_event callbacks for error_logger
handle_event({error, _GL, {Pid, Format, Data}}, State) ->
    Message = io_lib:format(Format, Data),
    Timestamp = format_timestamp(os:timestamp()),
    LogEntry = #{
        timestamp => Timestamp,
        message => unicode:characters_to_binary(Message),
        raw_timestamp => os:timestamp()
    },
    gen_server:cast(?SERVER, {add_log, LogEntry}),
    {ok, State};

handle_event({error_report, _GL, {Pid, std_error, Report}}, State) ->
    Message = io_lib:format("~p", [Report]),
    Timestamp = format_timestamp(os:timestamp()),
    LogEntry = #{
        timestamp => Timestamp,
        message => unicode:characters_to_binary(Message),
        raw_timestamp => os:timestamp()
    },
    gen_server:cast(?SERVER, {add_log, LogEntry}),
    {ok, State};

handle_event({info_msg, _GL, {Pid, Format, Data}}, State) ->
    Message = io_lib:format(Format, Data),
    Timestamp = format_timestamp(os:timestamp()),
    LogEntry = #{
        timestamp => Timestamp,
        message => unicode:characters_to_binary(Message),
        raw_timestamp => os:timestamp()
    },
    gen_server:cast(?SERVER, {add_log, LogEntry}),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

%% Internal functions
format_timestamp({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = 
        calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
    Millisecs = MicroSecs div 1000,
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0w",
                  [Year, Month, Day, Hour, Min, Sec, Millisecs]).
