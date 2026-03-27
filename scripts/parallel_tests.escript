#!/usr/bin/env escript
%% -*- erlang -*-
%%! -noshell

-module(parallel_tests).
-export([main/1]).

-define(BASE_PORT, 19000).
-define(LOG_DIR, "/tmp/eunit_parallel").
-define(PA_GLOB, "_build/test/lib/*/ebin").

main(Args) ->
    io:format("=== Compiling (test profile) ===~n"),
    case run_command(
        "rebar3",
        ["as", "test", "compile"],
        [{"CFLAGS", "-fpermissive"}],
        stdout
    ) of
        0 -> ok;
        _ ->
            io:format("Compilation failed~n"),
            halt(1)
    end,

    Modules =
        case Args of
            [] ->
                io:format("=== Discovering test modules ===~n"),
                discover_modules();
            _ ->
                [list_to_atom(Arg) || Arg <- Args]
        end,
    ModuleCount = length(Modules),

    ok = reset_log_dir(),

    Workers = worker_count(),
    io:format("=== Running ~B modules with ~B workers ===~n~n", [
        ModuleCount,
        Workers
    ]),

    Start = erlang:monotonic_time(second),
    State =
        run_modules(
            Modules,
            #{
                workers => Workers,
                port => ?BASE_PORT,
                running => 0,
                failed_modules => 0,
                total_failed => 0,
                total_passed => 0,
                total_cancelled => 0
            }
        ),
    Elapsed = erlang:monotonic_time(second) - Start,

    io:format("~n"),
    io:format("=== Done in ~Bs ===~n", [Elapsed]),
    io:format("    Modules: ~B~n", [ModuleCount]),
    io:format("=======================================================~n"),
    io:format(
        "  Failed: ~B.  Skipped: ~B.  Passed: ~B.~n",
        [
            maps:get(total_failed, State),
            maps:get(total_cancelled, State),
            maps:get(total_passed, State)
        ]
    ),

    case maps:get(failed_modules, State) of
        0 ->
            halt(0);
        _ ->
            io:format("~n"),
            io:format("Failed module logs in ~s/~n", [?LOG_DIR]),
            halt(1)
    end.

run_modules([], State = #{running := 0}) ->
    State;
run_modules(Modules, State = #{workers := Workers, running := Running})
        when Modules =/= [], Running < Workers ->
    [Mod | Rest] = Modules,
    Port = maps:get(port, State),
    Parent = self(),
    spawn(fun() ->
        Parent ! {module_done, safe_run_module(Mod, Port)}
    end),
    run_modules(
        Rest,
        State#{
            port => Port + 1,
            running => Running + 1
        }
    );
run_modules(Modules, State) ->
    receive
        {module_done, Result} ->
            print_result(Result),
            run_modules(Modules, accumulate(Result, State))
    end.

run_module(Mod, Port) ->
    Logfile = filename:join(?LOG_DIR, atom_to_list(Mod) ++ ".log"),
    Start = erlang:monotonic_time(millisecond),
    ExitStatus = run_test_vm(Mod, Port, Logfile),
    Elapsed = (erlang:monotonic_time(millisecond) - Start) div 1000,
    {Passed, Failed, Cancelled} = parse_log_stats(Logfile),
    #{
        mod => Mod,
        rc => ExitStatus,
        elapsed => Elapsed,
        logfile => Logfile,
        passed => Passed,
        failed => Failed,
        cancelled => Cancelled
    }.

safe_run_module(Mod, Port) ->
    try
        run_module(Mod, Port)
    catch
        Class:Reason:Stacktrace ->
            Logfile = filename:join(?LOG_DIR, atom_to_list(Mod) ++ ".log"),
            Error =
                io_lib:format(
                    "~p:~p~n~p~n",
                    [Class, Reason, Stacktrace]
                ),
            ok = file:write_file(Logfile, Error),
            #{
                mod => Mod,
                rc => 1,
                elapsed => 0,
                logfile => Logfile,
                passed => 0,
                failed => 0,
                cancelled => 0
            }
    end.

run_test_vm(Mod, Port, Logfile) ->
    {ok, FD} = file:open(Logfile, [write, raw, binary]),
    Eval =
        lists:flatten(
            io_lib:format(
                "application:ensure_all_started(hb), "
                "case eunit:test('~s', [verbose, {scale_timeouts, 20}]) of "
                "ok -> halt(0); "
                "error -> halt(1) "
                "end.",
                [atom_to_list(Mod)]
            )
        ),
    Args =
        [
            "-noshell"
            |
            lists:append(
                [[
                    "-pa",
                    Dir
                ] || Dir <- pa_dirs()]
            )
        ] ++ ["-eval", Eval],
    Status =
        try
            run_command(
                "erl",
                Args,
                [{"HB_PORT", integer_to_list(Port)}],
                {file, FD}
            )
        after
            file:close(FD)
        end,
    Status.

discover_modules() ->
    Beams = filelib:wildcard(filename:join(?PA_GLOB, "*.beam")),
    lists:sort(
        [
            Mod
         || Beam <- Beams,
            {ok, Mod} <- [beam_test_module(Beam)]
        ]
    ).

beam_test_module(Beam) ->
    case beam_lib:chunks(Beam, [exports]) of
        {ok, {Mod, [{exports, Exports}]}} ->
            case has_test_export(Exports) of
                true -> {ok, Mod};
                false -> false
            end;
        _ ->
            false
    end.

has_test_export(Exports) ->
    lists:any(
        fun
            ({Fun, 0}) ->
                Name = atom_to_list(Fun),
                lists:suffix("_test", Name) orelse
                    lists:suffix("_test_", Name);
            (_) ->
                false
        end,
        Exports
    ).

run_command(Name, Args, Env, Sink) ->
    case os:find_executable(Name) of
        false ->
            io:format("Executable not found: ~s~n", [Name]),
            127;
        Exec ->
            Port =
                open_port(
                    {spawn_executable, Exec},
                    [
                        binary,
                        use_stdio,
                        stderr_to_stdout,
                        exit_status,
                        hide,
                        eof,
                        {args, Args},
                        {env, Env}
                    ]
                ),
            collect_output(Port, Sink)
    end.

collect_output(Port, Sink) ->
    receive
        {Port, {data, Data}} ->
            write_output(Sink, Data),
            collect_output(Port, Sink);
        {Port, eof} ->
            collect_output(Port, Sink);
        {Port, {exit_status, Status}} ->
            Status
    end.

write_output(stdout, Data) ->
    io:put_chars(unicode:characters_to_list(Data));
write_output({file, FD}, Data) ->
    ok = file:write(FD, Data).

print_result(#{mod := Mod, rc := 0, elapsed := Elapsed}) ->
    io:format(
        "  \033[32mPASS\033[0m  ~-45s ~3Bs~n",
        [atom_to_list(Mod), Elapsed]
    );
print_result(#{mod := Mod, elapsed := Elapsed, logfile := Logfile}) ->
    io:format(
        "  \033[31mFAIL\033[0m  ~-45s ~3Bs  (see ~s)~n",
        [atom_to_list(Mod), Elapsed, Logfile]
    ).

accumulate(
    #{
        rc := RC,
        passed := Passed,
        failed := Failed,
        cancelled := Cancelled
    },
    State
) ->
    State#{
        running => maps:get(running, State) - 1,
        failed_modules =>
            maps:get(failed_modules, State) + failed_module_inc(RC),
        total_failed => maps:get(total_failed, State) + Failed,
        total_passed => maps:get(total_passed, State) + Passed,
        total_cancelled => maps:get(total_cancelled, State) + Cancelled
    }.

failed_module_inc(0) -> 0;
failed_module_inc(_) -> 1.

parse_log_stats(Logfile) ->
    case file:read_file(Logfile) of
        {ok, Bin} ->
            Cancelled = match_count(Bin, <<"\\*timed out\\*">>),
            case summary_triplet(Bin) of
                {ok, Failed, _Skipped, Passed} ->
                    {Passed, Failed, Cancelled};
                error ->
                    case passed_count(Bin) of
                        {ok, Passed} ->
                            {Passed, 0, Cancelled};
                        error ->
                            case binary:match(Bin, <<"Test passed.">>) of
                                nomatch -> {0, 0, Cancelled};
                                _ -> {1, 0, Cancelled}
                            end
                    end
            end;
        _ ->
            {0, 0, 0}
    end.

summary_triplet(Bin) ->
    case re:run(
        Bin,
        <<"Failed: ([0-9]+)\\.  Skipped: ([0-9]+)\\.  Passed: ([0-9]+)\\.">>,
        [global, {capture, all_but_first, binary}]
    ) of
        {match, Matches} ->
            [FailedBin, SkippedBin, PassedBin] = lists:last(Matches),
            {
                ok,
                binary_to_integer(FailedBin),
                binary_to_integer(SkippedBin),
                binary_to_integer(PassedBin)
            };
        nomatch ->
            error
    end.

passed_count(Bin) ->
    case re:run(
        Bin,
        <<"([0-9]+) tests? passed\\.">>,
        [global, {capture, all_but_first, binary}]
    ) of
        {match, Matches} ->
            [PassedBin] = lists:last(Matches),
            {ok, binary_to_integer(PassedBin)};
        nomatch ->
            error
    end.

match_count(Bin, Pattern) ->
    case re:run(Bin, Pattern, [global]) of
        {match, Matches} -> length(Matches);
        nomatch -> 0
    end.

worker_count() ->
    case os:getenv("MAKEFLAGS") of
        false ->
            erlang:system_info(schedulers_online);
        Makeflags ->
            case re:run(Makeflags, "-j([0-9]+)", [{capture, [1], list}]) of
                {match, [Value]} ->
                    list_to_integer(Value);
                nomatch ->
                    erlang:system_info(schedulers_online)
            end
    end.

pa_dirs() ->
    lists:sort(filelib:wildcard(?PA_GLOB)).

reset_log_dir() ->
    _ =
        case filelib:is_dir(?LOG_DIR) of
            true -> file:del_dir_r(?LOG_DIR);
            false -> ok
        end,
    filelib:ensure_dir(filename:join(?LOG_DIR, "dummy")).
