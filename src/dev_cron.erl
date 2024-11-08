-module(dev_cron).
-export([init/2, execute/2, uses/0]).

%%% A device that inserts new messages into the schedule to allow processes
%%% to passively 'call' themselves without user interaction.

-include("include/ao.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(CRON_INTERVAL, <<"Cron-Interval">>).

-record(schedule, {
	name :: binary(),
	unit :: binary(),
	scalar :: non_neg_integer()
}).

-record(state, {
	% The most recent timestamp and block height
	last_run :: {non_neg_integer(), non_neg_integer()},
	% The timestamp and block height at the point
	% of the process spawn
	spawn :: {non_neg_integer(), non_neg_integer()},
	crons :: [#schedule{}]
}).

init(State = #{timestamp := Timestamp, block_height := BlockHeight, process := Process}, _Params) ->
	ProcessTimestamp = timestamp(Process),
	ProcessBlockHeight = binary_to_integer(ar_util:find_value(Process#tx.tags, <<"Block-Height">>)),
	?no_prod("How will the timestamp and block_height be initialized on state? Presumably a checkpoint?"),
	% Default to values from process, which is to say the beginning of its schedule
	LastTimestamp = Timestamp orelse ProcessTimestamp,
	LastBlockHeight = BlockHeight orelse ProcessBlockHeight,
	case parse_crons(Process#tx.tags) of
		[] ->
			{ok, State#{cron => inactive}};
		Crons ->
			{
				ok,
				State#{
					cron => #state{
						last_run = {LastTimestamp, LastBlockHeight},
						spawn = {ProcessTimestamp, ProcessBlockHeight},
						crons = Crons
					}
				}
			}
	end.

execute(_Msg, State = #{cron := inactive}) ->
	{ok, State};
execute(
	Msg,
	State = #{
		schedule := Sched,
		cron := #state{
			last_run = LastRun,
			spawn = Spawn,
			crons = Crons
		}
	}
) ->
	CurTime = timestamp(Msg),
	{NewLastRun, NewSched} = maybe_append_cron_msgs(Crons, Sched, LastRun, CurTime),
	{
		ok,
		State#{
			schedule := NewSched,
			cron := #state{last_run = NewLastRun, spawn = Spawn, crons = Crons}
		}
	}.

% execute(_M, State = #{cron := inactive}) ->
% 	{ok, State};
% execute(M, State = #{pass := 1, cron := #state{last_run = undefined}}) ->
% 	{ok, State#{cron := #state{last_run = timestamp(M)}}};
% execute(Message, State = #{pass := 1, cron := #state{time = MilliSecs, last_run = LastRun}, schedule := Sched}) ->
% 	case timestamp(Message) - LastRun of
% 		Time when Time > MilliSecs ->
% 			NextCronMsg = create_cron(State, CronTime = timestamp(Message) + MilliSecs),
% 			{pass, State#{
% 				cron := #state{last_run = CronTime},
% 				schedule := [NextCronMsg | Sched]
% 			}};
% 		_ ->
% 			{ok, State}
% 	end;
% execute(_, S) ->
% 	{ok, S}.

uses() -> all.

%%% ================================
%%% Private Functions
%%% ================================

% TODO: implement matching crons using last run and cur time
% and appending crons to Schedule
maybe_append_cron_msgs(Crons, Sched, LastRun, CurTime) ->
	NewSched = Sched,
	{LastRun, NewSched}.

create_cron_msg(State = #{process := Process}, {_, CronTags}, Timestamp, BlockHeight) ->
	?no_prod("What about derived top-lvl like Timestamp, Block-Height, From, Cron, etc.?"),
	#tx{
		owner = Process#tx.owner,
		target = Process#tx.id,
		tags =
			[
				{<<"Data-Protocol">>, <<"ao">>},
				{<<"Variant">>, <<"ao.TN.2">>},
				{<<"Process">>, Process#tx.id},
				{<<"Target">>, Process#tx.id},
				{<<"Timestamp">>, list_to_binary(integer_to_list(Timestamp))},
				{<<"Block-Height">>, list_to_binary(integer_to_list(BlockHeight))}
			] ++ CronTags
	}.

timestamp(Tx) -> binary_to_integer(ar_util:find_value(Tx#tx.tags, <<"Timestamp">>)).

block(Height) ->
	{Timestamp, Height, _} = ao_client:arweave_timestamp(Height),
	{Timestamp, Height}.

%%% @doc Given a Time, find the block that was the "current" block on arweave,
%%% at the given time.
%%%
%%% It is STRONGLY recommended to utilize a Low and High height
%%% to help constrain the search space. This impl employs a binary search, querying
%%% arweave for block metadata.
%%%
%%% TODO: maybe should be abstracted behind ao_client:arweave_timestamp({timestamp, Time}, Low, High)
find_block_at_time(LowHeight, HighHeight, Time) -> find_block_at_time(LowHeight, HighHeight, Time, LowHeight).
find_block_at_time(LowHeight, HighHeight, _Time, CurHeight) when LowHeight > HighHeight ->
	block(CurHeight);
find_block_at_time(LowHeight, HighHeight, Time, CurHeight) ->
	Mid = LowHeight + (HighHeight - LowHeight) div 2,
	{CurTime, _} = block(Mid),
	if
		CurTime =< Time -> find_block_at_time(Mid + 1, HighHeight, Time, Mid);
		true -> find_block_at_time(LowHeight, Mid - 1, Time, CurHeight)
	end.

%%%
%%% Parse a set of ao tags into a list of Cron Schedules
%%%
parse_crons(Tags) -> parse_crons([], Tags).

parse_crons(Crons, []) ->
	lists:reverse(Crons);
parse_crons(Crons, [{?CRON_INTERVAL, Value} | RestTags]) ->
	parse_crons([{parse_interval(Value), []} | Crons], RestTags);
parse_crons([], [_ | RestTags]) ->
	% Cron-Tag-* are associated with the most-recent Cron-Interval
	% so if there are no Intervals yet found, then there is nothing to do
	parse_crons([], RestTags);
parse_crons(Crons, [Tag | RestTags]) ->
	parse_crons(maybe_append_cron_tag(Crons, Tag), RestTags).

maybe_append_cron_tag(Crons, {Name, Value}) ->
	Parsed = binary_to_list(Name),
	case Parsed of
		"Cron-Tag-" ++ _ ->
			[{Interval, CronTags} | Rest] = Crons,
			TagName = list_to_binary(string:slice(Parsed, length("Cron-Tag-"))),
			[{Interval, CronTags ++ [{TagName, Value}]} | Rest];
		_ ->
			Crons
	end.

parse_interval(Interval) ->
	[AmountStr, UnitStr] = lists:map(
		fun(S) -> string:trim(S) end,
		string:split(Interval, <<"-">>)
	),
	Amount = binary_to_integer(AmountStr),
	Unit = string:lowercase(binary_to_list(UnitStr)),
	case Unit of
		"millisecond" ++ _ -> to_time_schedule(Interval, Amount);
		"second" ++ _ -> to_time_schedule(Interval, Amount * 1000);
		"minute" ++ _ -> to_time_schedule(Interval, Amount * 60 * 1000);
		"hour" ++ _ -> to_time_schedule(Interval, Amount * 60 * 60 * 1000);
		"day" ++ _ -> to_time_schedule(Interval, Amount * 24 * 60 * 60 * 1000);
		"week" ++ _ -> to_time_schedule(Interval, Amount * 7 * 24 * 60 * 60 * 1000);
		"year" ++ _ -> to_time_schedule(Interval, Amount * 365 * 24 * 60 * 60 * 1000);
		"block" ++ _ -> to_block_schedule(Interval, Amount);
		_ -> throw({error, invalid_cron_interval, UnitStr})
	end.

to_time_schedule(Name, Millis) -> #schedule{name = Name, unit = <<"millisecond">>, scalar = Millis}.
to_block_schedule(Name, Blocks) -> #schedule{name = Name, unit = <<"block">>, scalar = Blocks}.

%%%
%%% TESTS
%%%

find_block_at_time_test() ->
	TargetBlock = {1731090175, 1543761},
	{TargetTime, _} = TargetBlock,

	ExactTime = find_block_at_time(1543732, 1543773, TargetTime),
	?assertEqual(TargetBlock, ExactTime),
	AfterTime = find_block_at_time(1543722, 1543773, TargetTime + 100),
	?assertEqual(TargetBlock, AfterTime).

parse_crons_test() ->
	Tags = [
		{<<"Foo">>, <<"Bar">>},
		{?CRON_INTERVAL, <<"10-blocks">>},
		{<<"Cron-Tag-Action">>, <<"notify">>},
		{<<"Cron-Tag-Action-Function">>, <<"transfer">>},
		{<<"Random">>, <<"Tag">>},
		{?CRON_INTERVAL, <<" 10-minutes ">>},
		{<<"Cron-Tag-Action">>, <<"notify">>},
		{<<"Cron-Tag-Action-Function">>, <<"transfer">>},
		{?CRON_INTERVAL, <<"1-hour">>},
		{<<"Another">>, <<"Tag">>},
		{<<"Cron-Tag-Action">>, <<"transfer">>}
	],
	[{BSchedule, BTags}, {MSchedule, MTags}, {HSchedule, HTags}] = parse_crons(Tags),

	?assertEqual(#schedule{name = <<"10-blocks">>, unit = <<"block">>, scalar = 10}, BSchedule),
	?assertEqual([{<<"Action">>, <<"notify">>}, {<<"Action-Function">>, <<"transfer">>}], BTags),

	?assertEqual(#schedule{name = <<" 10-minutes ">>, unit = <<"millisecond">>, scalar = 10 * 60 * 1000}, MSchedule),
	?assertEqual([{<<"Action">>, <<"notify">>}, {<<"Action-Function">>, <<"transfer">>}], MTags),

	?assertEqual(#schedule{name = <<"1-hour">>, unit = <<"millisecond">>, scalar = 60 * 60 * 1000}, HSchedule),
	?assertEqual([{<<"Action">>, <<"transfer">>}], HTags).
