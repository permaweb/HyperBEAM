%%% @doc Session manager for vLLM device to handle session history across processes.
%%%
%%% This module implements a gen_server that owns the ETS tables for session
%%% management, allowing different processes to share session data.
-module(vllm_session_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, get_session_history/2, update_session_history/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MAX_SESSIONS, 1000).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the session manager server.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get session history for a given session ID and prompt.
get_session_history(SessionId, Prompt) ->
    gen_server:call(?SERVER, {get_session_history, SessionId, Prompt}).

%% @doc Update session history with assistant response.
update_session_history(SessionId, Prompt, AssistantResponse) ->
    gen_server:call(?SERVER, {update_session_history, SessionId, Prompt, AssistantResponse}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Create ETS tables for session data with timestamp tracking
    ets:new(vllm_sessions_lru, [named_table, ordered_set, public]),
    % Create ETS table for LRU timestamps
    ets:new(vllm_session_timestamps, [named_table, ordered_set, public]),
    {ok, #state{}}.

handle_call({get_session_history, SessionId, Prompt}, _From, State) ->
    Timestamp = erlang:system_time(millisecond),
    
    % Get existing history or create new
    History = case ets:lookup(vllm_sessions_lru, SessionId) of
        [] -> 
            % New session
            [];
        [{SessionId, ExistingHistory}] -> 
            % Update timestamp for LRU
            ets:delete(vllm_session_timestamps, {get_session_timestamp(SessionId), SessionId}),
            ExistingHistory
    end,
    
    % Add new user message
    NewHistory = History ++ [#{<<"role">> => <<"user">>, <<"content">> => Prompt}],
    
    % Trim history if it exceeds reasonable length
    TrimmedHistory = case length(NewHistory) > 20 of
        true -> 
            lists:nthtail(length(NewHistory) - 20, NewHistory);
        false -> 
            NewHistory
    end,
    
    % Update session in ETS with new timestamp
    ets:insert(vllm_sessions_lru, {SessionId, TrimmedHistory}),
    ets:insert(vllm_session_timestamps, {{Timestamp, SessionId}, TrimmedHistory}),
    
    % Check if we need to evict old sessions
    evict_old_sessions(),
    
    {reply, TrimmedHistory, State};

handle_call({update_session_history, SessionId, _UserPrompt, AssistantResponse}, _From, State) ->
    Timestamp = erlang:system_time(millisecond),
    
    case ets:lookup(vllm_sessions_lru, SessionId) of
        [] -> 
            % Create new session with just the assistant response
            NewHistory = [#{<<"role">> => <<"assistant">>, <<"content">> => AssistantResponse}],
            ets:insert(vllm_sessions_lru, {SessionId, NewHistory}),
            ets:insert(vllm_session_timestamps, {{Timestamp, SessionId}, NewHistory});
        [{SessionId, ExistingHistory}] ->
            % Add assistant response to history
            NewHistory = ExistingHistory ++ [#{<<"role">> => <<"assistant">>, <<"content">> => AssistantResponse}],
            ets:insert(vllm_sessions_lru, {SessionId, NewHistory}),
            % Update timestamp
            OldTimestamp = get_session_timestamp(SessionId),
            ets:delete(vllm_session_timestamps, {OldTimestamp, SessionId}),
            ets:insert(vllm_session_timestamps, {{Timestamp, SessionId}, NewHistory})
    end,
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    % Clean up ETS tables
    ets:delete(vllm_sessions_lru),
    ets:delete(vllm_session_timestamps),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Get timestamp for a session ID.
get_session_timestamp(SessionId) ->
    case ets:match(vllm_session_timestamps, {{'$1', SessionId}, '_'}) of
        [[Timestamp]|_] -> Timestamp;
        [] -> 0
    end.

%% @doc Evict old sessions based on LRU policy.
evict_old_sessions() ->
    SessionCount = ets:info(vllm_sessions_lru, size),
    if 
        SessionCount > ?MAX_SESSIONS ->
            % Need to evict oldest sessions
            EvictCount = SessionCount - ?MAX_SESSIONS + 10, % Evict extra to prevent frequent eviction
            evict_oldest_sessions(EvictCount);
        true ->
            ok
    end.

%% @doc Evict the oldest sessions.
evict_oldest_sessions(Count) ->
    OldestSessions = ets:first(vllm_session_timestamps),
    evict_oldest_sessions(Count, OldestSessions, 0).

evict_oldest_sessions(Count, Key, Evicted) when Evicted >= Count; Key =:= '$end_of_table' ->
    ok;
evict_oldest_sessions(Count, {Timestamp, SessionId}, Evicted) ->
    % Delete from both tables
    ets:delete(vllm_sessions_lru, SessionId),
    ets:delete(vllm_session_timestamps, {Timestamp, SessionId}),
    
    % Move to next
    NextKey = ets:next(vllm_session_timestamps, {Timestamp, SessionId}),
    evict_oldest_sessions(Count, NextKey, Evicted + 1).