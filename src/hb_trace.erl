-module(hb_trace).
-export([start/0, stop/0, span/2, get_timings/0, format_header/0]).
-export([enabled/0, enable/0, disable/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TRACE_KEY, hb_trace_spans).
-define(TRACE_START, hb_trace_start).
-define(TRACE_ENABLED, hb_trace_enabled).

enabled() ->
    get(?TRACE_ENABLED) =:= true.

enable() ->
    put(?TRACE_ENABLED, true),
    ok.

disable() ->
    put(?TRACE_ENABLED, false),
    ok.

start() ->
    put(?TRACE_START, erlang:monotonic_time(microsecond)),
    put(?TRACE_KEY, []),
    ok.

stop() ->
    erase(?TRACE_KEY),
    erase(?TRACE_START),
    ok.

span(Name, Fun) when is_function(Fun, 0) ->
    case enabled() of
        true ->
            Start = erlang:monotonic_time(microsecond),
            try Fun() of
                Result ->
                    End = erlang:monotonic_time(microsecond),
                    Duration = End - Start,
                    add_span(Name, Duration),
                    Result
            catch
                Class:Reason:Stack ->
                    End = erlang:monotonic_time(microsecond),
                    Duration = End - Start,
                    add_span(iolist_to_binary([Name, <<":error">>]), Duration),
                    erlang:raise(Class, Reason, Stack)
            end;
        false ->
            Fun()
    end.

add_span(Name, Duration) ->
    Spans = get(?TRACE_KEY),
    case Spans of
        undefined -> ok;
        _ -> put(?TRACE_KEY, [{Name, Duration} | Spans])
    end.

get_timings() ->
    case get(?TRACE_KEY) of
        undefined -> [];
        Spans -> lists:reverse(Spans)
    end.

format_header() ->
    Timings = get_timings(),
    case Timings of
        [] -> <<>>;
        _ ->
            TotalStart = get(?TRACE_START),
            TotalEnd = erlang:monotonic_time(microsecond),
            TotalDuration = TotalEnd - TotalStart,
            Parts = lists:map(
                fun({Name, Duration}) ->
                    iolist_to_binary([
                        Name,
                        <<"=">>,
                        integer_to_binary(Duration),
                        <<"us">>
                    ])
                end,
                Timings
            ),
            AllParts = Parts ++ [iolist_to_binary([<<"total=">>, integer_to_binary(TotalDuration), <<"us">>])],
            iolist_to_binary(lists:join(<<", ">>, AllParts))
    end.

basic_tracing_test() ->
    enable(),
    start(),
    Result = span(<<"test_span">>, fun() ->
        timer:sleep(10),
        ok
    end),
    ?assertEqual(ok, Result),
    Header = format_header(),
    stop(),
    disable(),
    ?assert(binary:match(Header, <<"test_span=">>) =/= nomatch),
    ?assert(binary:match(Header, <<"total=">>) =/= nomatch).

disabled_tracing_test() ->
    disable(),
    start(),
    Result = span(<<"test_span">>, fun() -> ok end),
    ?assertEqual(ok, Result),
    Timings = get_timings(),
    stop(),
    ?assertEqual([], Timings).

nested_spans_test() ->
    enable(),
    start(),
    span(<<"outer">>, fun() ->
        span(<<"inner">>, fun() ->
            timer:sleep(5)
        end)
    end),
    Header = format_header(),
    stop(),
    disable(),
    ?assert(binary:match(Header, <<"outer=">>) =/= nomatch),
    ?assert(binary:match(Header, <<"inner=">>) =/= nomatch).
