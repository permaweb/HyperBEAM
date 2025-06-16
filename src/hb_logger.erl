-module(hb_logger).
-export([start/0, start/1, log/2, register/1, report/1]).

-include("include/hb.hrl").

-record(state, {
          client = undefined,
          activity = [],
          processes = waiting,
          console = true
         }).


start() -> start(undefined).


start(Client) ->
    spawn(fun() ->
                  loop(#state{client = Client})
          end).


log(Monitor, Data) ->
    Monitor ! {log, Data}.


register(Monitor) ->
    ?event({self(), registering}),
    Monitor ! {register, self()}.


report(Monitor) ->
    Monitor ! {report, self()},
    receive
        {report, Activity} ->
            Activity
    end.


loop(#state{processes = [], client = undefined}) -> done;
loop(#state{processes = [], client = C, activity = A}) ->
    C ! {?MODULE, self(), done, A};
loop(State) ->
    receive
        {log, Activity} ->
            console(State, Activity),
            loop(State#state{activity = [Activity | State#state.activity]});
        {register, PID} ->
            ?event(registered),
            %erlang:monitor(process, PID),
            console(State, Act = {ok, registered, PID}),
            ?event({registered, PID}),
            loop(State#state{
                   processes =
                       [PID | case State#state.processes of waiting -> []; L -> L end],
                   activity = [Act | State#state.activity]
                  });
        {'DOWN', _MonitorRef, process, PID, Reason} ->
            console(State, Act = {terminated, Reason, PID}),
            ?event({dead, PID}),
            loop(State#state{
                   processes = State#state.processes -- [PID],
                   activity = [Act | State#state.activity]
                  });
        {report, PID} ->
            PID ! {report, State#state.activity},
            loop(State)
    end.


console(#state{console = false}, _) ->
    not_printing;
console(S, {Status, Type, Details}) when is_record(Details, tx) ->
    console(S, {Status, Type, hb_util:id(Details)});
console(_S, {Status, Type, Details}) ->
    io:format("### MU PUSH REPORT ~p ###~n~p: ~p~n~p~n~n",
              [self(), Status, Type, Details]);
console(_S, Act) ->
    io:format("### MU PUSH UNEXPECTED ~p ###~n~p~n~n", [self(), Act]).
