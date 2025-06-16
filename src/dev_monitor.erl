-module(dev_monitor).
-export([init/3, execute/2, end_of_schedule/1, uses/0, add_monitor/2]).

-include("include/hb.hrl").

%%% A simple device that allows flexible monitoring of a process execution.
%%% Adding a dev_monitor device to a process will cause the listed functions
%%% to be called with the current process state during each pass. The monitor
%%% functions must not mutate state.


init(State, _, InitState) ->
    {ok, State#{<<"monitors">> => InitState}}.


execute(Message, State = #{<<"pass">> := Pass, <<"passes">> := Passes}) when Pass == Passes ->
    signal(State, {message, Message});
execute(_, S) -> {ok, S}.


add_monitor(Mon, State = #{<<"monitors">> := Monitors}) ->
    {ok, State#{<<"monitors">> => [Mon | Monitors]}}.


end_of_schedule(State) -> signal(State, end_of_schedule).


signal(State = #{<<"monitors">> := StartingMonitors}, Signal) ->
    RemainingMonitors =
        lists:filter(
          fun(Mon) ->
                  case Mon(State, Signal) of
                      done -> false;
                      _ -> true
                  end
          end,
          StartingMonitors),
    ?event({remaining_monitors, length(RemainingMonitors)}),
    {ok, State#{<<"monitors">> := RemainingMonitors}}.


uses() -> all.
