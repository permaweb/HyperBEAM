%%% @doc Wrapper process responsible for serializing requests to prometheus, so they
%%% are called asyncronousely.

-module(hb_event_counts).
-behaviour(gen_server).

% Starting the process
-export([start_link/0]).

% API functions
-export([increment/2]).

% Gen server callbacks
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_info/2, handle_call/3]).
-export([code_change/3]).

increment(RawMessage, RawTopic) ->
    % e.g. (hb_beamr, wasm_init_success}
    Message = prefix_with("event_", parse_name(RawMessage)),
    Topic = prefix_with("topic_", parse_name(RawTopic)),
    gen_server:cast(?MODULE, {increment_message, Message, Topic}).

%%%=============================================================================
%%% Gen server callbacks
%%%=============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #{}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({increment_message, Message, Topic}, State) ->
    % event_wasm_send - message
    % event_hb_beamr - topic
    NewState =
        case maps:get({Message, Topic}, State, undefined) of
            undefined ->
                % register the counter first
                prometheus_counter:declare(
                    [
                        {name, Message},
                        {help, ""},
                        {labels, [topic]}
                    ]),
                maps:put({Message, Topic}, true, State);
            _ ->
                % the counter is already registered
                State
        end,
    prometheus_counter:inc(Message, [Topic]),
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Private functions
%%%=============================================================================
prefix_with(Prefix, Name) ->
    lists:flatten(io_lib:format("~s~s", [Prefix, Name])).

parse_name(Name) when is_tuple(Name) ->
    parse_name(element(1, Name));
parse_name(Name) when is_list(Name) ->
    parse_name(hd(Name));
parse_name(Name) ->
    Name.
