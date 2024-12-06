%%%-----------------------------------------------------------------------------
%%% @moduledoc Provides an API for creating, registering, and interacting with custom metrics.
%%%		 This module maintains a registry of user-defined metrics and allows interaction
%%%		 with them according to their type.
%%%
%%%		The process provides a wrapper over an ets table, where writes (register/5) are
%%%		serialized by the process, while all other operations are executed in the context
%%%     of caller.
%%%
%%% 	Examples:
%%%     1> hb_metrics:register(counter, "read_cnt", "Counts the number of reads", [], []).
%%%        ok
%%%     2> hb_metrics:increment("read_cnt", []).
%%%        ok
%%%     3> hb_metrics:value("read_cnt", []).
%%%        1
%%%
%%%    Metrics can also be automatically registered via the `sys.config` file, where
%%%    metrics are defined as a list of maps in the following format:
%%%
%%%    {metrics, [
%%% 			#{
%%% 				type => histogram,
%%% 				name => "ao_cache_write_time",
%%% 				description => "Histogram of write operation duration",
%%% 				buckets => [5, 10, 100, 500, 1000]
%%% 			},
%%% 			#{
%%% 				type => counter,
%%% 				name => "ao_cache_read",
%%% 				description => "Counts ao cache read operations"
%%% 			}
%%% 	]}
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(hb_metrics).
-behaviour(gen_server).

% API functions
-export([register/5]).
-export([register/4]).
-export([increment/2]).
-export([decrement/2]).
-export([set/3]).
-export([observe/3]).
-export([value/2]).

% Starting the process
-export([start_link/1]).

% Gen server callbacks
-export([init/1, terminate/2]).
-export([handle_cast/2, handle_info/2, handle_call/3]).
-export([code_change/3]).

-define(TABLE_NAME, ?MODULE).

-type metric_type() :: counter | gauge | histogram | summary.
-type operation_result() ::
	ok
	| {error, unsupported_operation}
	| {error, labels_mismatch}
	| {error, term()}.

%%% @doc Registers metric in the hb_metrics system. Equivalent to calling:
%%% register(Type, Name, Description, Labels, []).

-spec register(Type, Name, Description, Labels) -> Result when
	Type :: metric_type(),
	Name :: string(),
	Description :: string(),
	Labels :: [string()],
	Result ::
		ok
		| {error, {invalid_metric_name, term()}}
		| {error, {invalid_histogram, no_buckets}}
		| {error, could_not_register_metric}.
register(Type, Name, Description, Labels) ->
	register(Type, Name, Description, Labels, []).

%%% @doc Registers metric in the hb_metrics system.
-spec register(Type, Name, Description, Labels, Buckets) -> Result when
	Type :: metric_type(),
	Name :: string(),
	Description :: string(),
	Labels :: [string()],
	Buckets :: [pos_integer()],
	Result ::
		ok
		| {error, {invalid_metric_name, term()}}
		| {error, {invalid_histogram, no_buckets}}
		| {error, could_not_register_metric}.
register(Type, Name, Description, Labels, Buckets) ->
	gen_server:call(?MODULE, {register, Type, Name, Description, Labels, Buckets}).

%%% @doc Increments given metric
-spec increment(Name, Labels) -> Result when
	Name :: string(),
	Labels :: [string()],
	Result :: operation_result().
increment(Name, Labels) ->
	case fetch_metric_info(Name, Labels) of
		{error, _Err} = Err -> Err;
		{ok, #{type := counter}} -> prometheus_counter:inc(Name, Labels);
		{ok, #{type := gauge}} -> prometheus_gauge:inc(Name, Labels);
		{ok, _} -> {error, unsupported_operation}
	end.

%%% @doc Decrement given metric
-spec decrement(Name, Labels) -> Result when
	Name :: string(),
	Labels :: [string()],
	Result :: ok | {error, unsupported_operation} | {error, labels_mismatch} | {error, term()}.
decrement(Name, Labels) ->
	case fetch_metric_info(Name, Labels) of
		{error, _Err} = Err -> Err;
		{ok, #{type := gauge}} -> prometheus_gauge:dec(Name, Labels);
		{ok, _} -> {error, unsupported_operation}
	end.

%%% @doc Set given metric to a specific value
-spec set(Name, Labels, Value) -> Result when
	Name :: string(),
	Labels :: [string()],
	Value :: integer(),
	Result :: operation_result().

set(Name, Labels, Value) ->
	case fetch_metric_info(Name, Labels) of
		{error, _Err} = Err -> Err;
		{ok, #{type := gauge}} -> prometheus_gauge:set(Name, Labels, Value);
		{ok, _} -> {error, unsupported_operation}
	end.

%%% @doc Observe value into histogram or summary
-spec observe(Name, Labels, Value) -> Result when
	Name :: string(),
	Labels :: [string()],
	Value :: integer(),
	Result :: operation_result().

observe(Name, Labels, Value) ->
	case fetch_metric_info(Name, Labels) of
		{error, _Err} = Err -> Err;
		{ok, #{type := histogram}} -> prometheus_histogram:observe(Name, Labels, Value);
		{ok, #{type := summary}} -> prometheus_summary:observe(Name, Labels, Value);
		{ok, _} -> {error, unsupported_operation}
	end.

%%% @doc Return value for the given counter
-spec value(Name, Labels) -> Result when
	Name :: string(),
	Labels :: [string()],
	Result :: integer() | tuple() | undefined | {error, term()}.

value(Name, Labels) ->
	case fetch_metric_info(Name, Labels) of
		{error, _Reason} = Err ->
			Err;
		{ok, #{type := counter}} ->
			prometheus_counter:value(Name, Labels);
		{ok, #{type := gauge}} ->
			prometheus_gauge:value(Name, Labels);
		{ok, #{type := histogram}} ->
			prometheus_histogram:value(Name, Labels);
		{ok, #{type := summary}} ->
			prometheus_summary:value(Name, Labels)
	end.

%%%=============================================================================
%%% Gen server callbacks
%%%=============================================================================
-spec start_link([map()]) -> {ok, pid()}.
start_link(MetricsList) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, MetricsList, []).

% Note: we do not expect a large number of metrics (1000_) to be defined by the sys.config
% in case if it's going to be the case, we would need to iterate on the `init` function
% in order to release the parent supervisor fast, and to use `handle_continue` instead.
init(MetricsList) ->
	_TableId = ets:new(?MODULE, [set, named_table, protected, {read_concurrency, true}]),
	lists:foreach(
		fun(Metric) ->
			Type = maps:get(type, Metric, undefined),
			Name = maps:get(name, Metric, undefined),
			Description = maps:get(description, Metric, undefined),
			Labels = maps:get(labels, Metric, []),
			Buckets = maps:get(buckets, Metric, []),
			case do_register(Type, Name, Description, Labels, Buckets) of
				ok -> ok;
				{error, Reason} -> logger:error("Could not register metric from static config: ~p", [Reason])
			end
		end,
		MetricsList
	),
	{ok, #{}}.

handle_call({register, Type, Name, Description, Labels, Buckets}, _From, State) ->
	Result = do_register(Type, Name, Description, Labels, Buckets),
	{reply, Result, State}.

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
do_register(Type, Name, Description, Labels, Buckets) ->
	case catch declare(Type, Name, Description, Labels, Buckets) of
		true ->
			true = ets:insert(?TABLE_NAME, {Name, Type, Labels}),
			ok;
		false ->
			% Metric was already registered
			ok;
		{'EXIT', {{invalid_metric_name, _MetricName, Details}, _Trace}} ->
			{error, {invalid_metric_name, Details}};
		{'EXIT', {{no_buckets, []}, _Trace}} ->
			{error, {invalid_histogram, no_buckets}};
		{error, _Reason} = Err ->
			Err;
		Other ->
			{error, {could_not_register_metric, Other}}
	end.

declare(counter, Name, Description, Labels, _Buckets) ->
	prometheus_counter:declare([{name, Name}, {help, Description}, {labels, Labels}]);
declare(gauge, Name, Description, Labels, _Buckets) ->
	prometheus_gauge:declare([{name, Name}, {help, Description}, {labels, Labels}]);
declare(histogram, Name, Description, Labels, Buckets) ->
	prometheus_histogram:declare(
		[
			{name, Name},
			{help, Description},
			{labels, Labels},
			{buckets, Buckets}
		]
	);
declare(summary, Name, Description, _Labels, _Buckets) ->
	prometheus_summary:declare(
		[
			{name, Name},
			{help, Description}
		]
	);
declare(OtherType, _Name, _Description, _Labels, _Buckets) ->
	{error, {unsupported_metric_type, OtherType}}.

fetch_metric_info(Name, Labels) ->
	case ets:lookup(?TABLE_NAME, Name) of
		[] ->
			{error, metric_not_registered};
		[{_Name, _Type, StoredLabels}] when length(StoredLabels) /= length(Labels) ->
			{error, labels_mismatch};
		[{_Name, Type, StoredLabels}] ->
			{ok, #{name => Name, type => Type, labels => StoredLabels}}
	end.

-include_lib("eunit/include/eunit.hrl").

observe_test_() ->
	{
		foreach,
		fun setup/0,
		fun cleanup/1,
		[
			{
				"can observe histogram",
				fun() ->
					ok = hb_metrics:register(histogram, "histogram", "description", [], [0, 1]),
					?assertEqual(
						ok, hb_metrics:observe("histogram", [], 1)
					),
					?assertEqual({[0, 1, 0], 1}, hb_metrics:value("histogram", []))
				end
			},
			{
				"can observe summary",
				fun() ->
					ok = hb_metrics:register(summary, "summary", "description", [], [0, 1]),
					?assertEqual(
						ok, hb_metrics:observe("summary", [], 1)
					),
					?assertEqual({1, 1}, hb_metrics:value("summary", []))
				end
			},
			{
				"can observe histogram with labels",
				fun() ->
					ok = hb_metrics:register(histogram, "histogram", "description", [method], [0, 1]),
					?assertEqual(
						ok, hb_metrics:observe("histogram", [post], 1)
					),
					?assertEqual({[0, 1, 0], 1}, hb_metrics:value("histogram", [post]))
				end
			},
			{
				"verifies label counts",
				fun() ->
					ok = hb_metrics:register(histogram, "histogram", "description", [method], [0, 1]),
					?assertEqual(
						{error, labels_mismatch}, hb_metrics:observe("histogram", [post, get], 1)
					)
				end
			},
			{
				"not supported for counter/gauge",
				fun() ->
					ok = hb_metrics:register(counter, "counter", "description", []),
					?assertEqual(
						{error, unsupported_operation}, hb_metrics:observe("counter", [], 1)
					),
					ok = hb_metrics:register(gauge, "gauge", "description", []),
					?assertEqual(
						{error, unsupported_operation}, hb_metrics:observe("gauge", [], 1)
					)
				end
			}
		]
	}.

set_test_() ->
	{
		foreach,
		fun setup/0,
		fun cleanup/1,
		[
			{
				"can set gauge",
				fun() ->
					ok = hb_metrics:register(gauge, "gauge", "description", []),
					?assertEqual(
						ok, hb_metrics:set("gauge", [], 10)
					),
					?assertEqual(10, hb_metrics:value("gauge", []))
				end
			},
			{
				"can set gauge with labels",
				fun() ->
					ok = hb_metrics:register(gauge, "gauge", "description", [method]),
					?assertEqual(
						ok, hb_metrics:set("gauge", [post], 100)
					),
					?assertEqual(100, hb_metrics:value("gauge", [post]))
				end
			},
			{
				"verifies label counts",
				fun() ->
					ok = hb_metrics:register(gauge, "gauge", "description", [method]),
					?assertEqual(
						{error, labels_mismatch}, hb_metrics:set("gauge", [post, get], 1)
					)
				end
			},
			{
				"Not supported for counter/histogram/summary",
				fun() ->
					ok = hb_metrics:register(counter, "counter", "description", []),
					?assertEqual(
						{error, unsupported_operation}, hb_metrics:set("counter", [], 1)
					),
					ok = hb_metrics:register(histogram, "histogram", "description", [], [1]),
					?assertEqual(
						{error, unsupported_operation}, hb_metrics:set("histogram", [], 1)
					),
					ok = hb_metrics:register(summary, "summary", "description", []),
					?assertEqual(
						{error, unsupported_operation}, hb_metrics:set("summary", [], 1)
					)
				end
			}
		]
	}.
decrement_test_() ->
	{
		foreach,
		fun setup/0,
		fun cleanup/1,
		[
			{
				"can decrement gauge",
				fun() ->
					ok = hb_metrics:register(gauge, "gauge", "description", []),
					?assertEqual(
						ok, hb_metrics:decrement("gauge", [])
					),
					?assertEqual(-1, hb_metrics:value("gauge", []))
				end
			},
			{
				"can decrement gauge with labels",
				fun() ->
					ok = hb_metrics:register(gauge, "gauge", "description", [method]),
					?assertEqual(
						ok, hb_metrics:decrement("gauge", [post])
					),
					?assertEqual(-1, hb_metrics:value("gauge", [post]))
				end
			},
			{
				"verifies label counts",
				fun() ->
					ok = hb_metrics:register(gauge, "gauge", "description", [method]),
					?assertEqual(
						{error, labels_mismatch}, hb_metrics:decrement("gauge", [post, get])
					)
				end
			},
			{
				"Not supported for counter/histogram/summary",
				fun() ->
					ok = hb_metrics:register(counter, "counter", "description", []),
					?assertEqual(
						{error, unsupported_operation}, hb_metrics:decrement("counter", [])
					),
					ok = hb_metrics:register(histogram, "histogram", "description", [], [1]),
					?assertEqual(
						{error, unsupported_operation}, hb_metrics:decrement("histogram", [])
					),
					ok = hb_metrics:register(summary, "summary", "description", []),
					?assertEqual(
						{error, unsupported_operation}, hb_metrics:decrement("summary", [])
					)
				end
			}
		]
	}.

increment_test_() ->
	{foreach, fun setup/0, fun cleanup/1, [
		{
			"Can increment counter",
			fun() ->
				ok = hb_metrics:register(counter, "sample_counter", "description", []),
				?assertEqual(
					ok, hb_metrics:increment("sample_counter", [])
				),
				?assertEqual(1, hb_metrics:value("sample_counter", []))
			end
		},
		{
			"Can increment counter with labels",
			fun() ->
				ok = hb_metrics:register(counter, "counter_with_labels", "description", ["method"]),
				?assertEqual(
					ok, hb_metrics:increment("counter_with_labels", [get])
				),
				?assertEqual(1, hb_metrics:value("counter_with_labels", [get])),
				?assertEqual(undefined, hb_metrics:value("counter_with_labels", [post]))
			end
		},
		{
			"verifies label counts",
			fun() ->
				ok = hb_metrics:register(gauge, "gauge", "description", [method]),
				?assertEqual(
					{error, labels_mismatch}, hb_metrics:increment("gauge", [post, get])
				)
			end
		},
		{
			"Can increment gauge",
			fun() ->
				ok = hb_metrics:register(gauge, "sample_gauge", "description", []),
				?assertEqual(
					ok, hb_metrics:increment("sample_gauge", [])
				),
				?assertEqual(1, hb_metrics:value("sample_gauge", []))
			end
		},
		{
			"Increment is not supported for histogram",
			fun() ->
				ok = hb_metrics:register(histogram, "sample_histogram", "description", [], [10, 20, 30]),
				?assertEqual(
					{error, unsupported_operation},
					hb_metrics:increment("sample_histogram", [])
				)
			end
		},
		{
			"Increment is not supported for summary",
			fun() ->
				ok = hb_metrics:register(summary, "summary", "description", [], [10, 20, 30]),
				?assertEqual(
					{error, unsupported_operation},
					hb_metrics:increment("summary", [])
				)
			end
		}
	]}.

register_test_() ->
	{foreach, fun setup/0, fun cleanup/1, [
		{
			"Can register counter metric",
			fun() ->
				Result = hb_metrics:register(counter, "sample_counter", "description", []),
				?assertEqual(ok, Result)
			end
		},
		{
			"Can register counter metric twice",
			fun() ->
				Result = hb_metrics:register(counter, "sample_counter", "description", []),
				?assertEqual(ok, Result)
			end
		},
		{
			"Can register gauge metric",
			fun() ->
				Result = hb_metrics:register(gauge, "sample_gauge", "description", []),
				?assertEqual(ok, Result)
			end
		},
		{
			"Can register histogram",
			fun() ->
				Result = hb_metrics:register(histogram, "sample_histogram", "description", [], [10, 20, 30]),
				?assertEqual(ok, Result)
			end
		},
		{
			"Can register summary",
			fun() ->
				Result = hb_metrics:register(summary, "sample_summary", "description", []),
				?assertEqual(ok, Result)
			end
		},
		{
			"Can register metrics with labels",
			fun() ->
				Result = hb_metrics:register(summary, "requests_counter", "description", [label1]),
				?assertEqual(ok, Result)
			end
		},
		{
			"Handles invalid names",
			fun() ->
				% Metric names are expected to match: ^[a-zA-Z_:][a-zA-Z0-9_:]*$
				?assertEqual(
					{error, {invalid_metric_name, "metric name doesn't match regex ^[a-zA-Z_:][a-zA-Z0-9_:]*$"}},
					hb_metrics:register(counter, "sample counter", "description", [])
				),
				?assertEqual(
					{error, {invalid_metric_name, "metric name is not a string"}},
					hb_metrics:register(counter, {name, "myname"}, "description", [])
				)
			end
		},
		{
			"Can't register histogram without buckets",
			fun() ->
				Result = hb_metrics:register(histogram, "sample_histogram", "description", [], []),
				?assertEqual({error, {invalid_histogram, no_buckets}}, Result)
			end
		}
	]}.

get_or_start_server(Params) ->
	case start_link(Params) of
		{ok, Pid} ->
			{ok, Pid};
		{error, {already_started, Pid}} ->
			{ok, Pid};
		Err ->
			Err
	end.

setup() ->
	logger:set_primary_config(level, error),
	{ok, Pid} = get_or_start_server([]),
	application:ensure_all_started(prometheus),
	unlink(Pid).

cleanup(_) ->
	gen_server:stop(hb_metrics),
	application:stop(prometheus).
