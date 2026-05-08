%%% @doc A module for running different profiling tools upon HyperBEAM executions.
%%% This device allows a variety of profiling tools to be used and for their
%%% outputs to be returned as messages, or displayed locally on the console.
%%% 
%%% When called from an AO-Core request, the path at the given key is resolved.
%%% If the `eval' function is instead directly invoked via Erlang, the first
%%% argument may be a function to profile instead.
-module(dev_profile).
-export([info/1, eval/1, eval/2, eval/3, eval/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Default to the `eval' function.
info(_) ->
    #{
        excludes => [<<"keys">>, <<"set">>],
        default => fun eval/4
    }.

%% @doc Invoke a profiling tool on a function or an AO-Core resolution. If a 
%% message is provided as the first argument, a function to profile is produced
%% from an AO-Core resolution of the path referenced by `path` key. For example,
%% `/~profile@1.0/run?run=/~meta@1.0/build' will resolve to the `build' function
%% in the `~meta@1.0' device. The `request` message (if provided) is passed 
%% downstream to the profiling engine as well as the AO-Core resolution.
%% 
%% If the `return-mode' option is not set, we default to `console' for Erlang
%% invocations. We determine this by checking if the first argument is a
%% function. This is not possible if the function has been invoked by an
%% AO-Core resolution.
%% 
%% When in `return-mode: console' mode, the return format will be
%% `{EngineOut, Res}' where `EngineOut' is the output from the engine, and `Res'
%% is the result of the function or resolution. In `return-mode: message' mode,
%% the return format will be `{ok, EngineMessage}' where `EngineMessage' is the
%% output from the engine formatted as an AO-Core message.
eval(Fun) -> eval(Fun, #{}).
eval(Fun, Opts) -> eval(Fun, #{}, Opts).
eval(Fun, Req, Opts) when is_function(Fun) ->
    do_eval(
        Fun,
        case return_mode(Req, Opts, undefined) of
            undefined -> Req#{ <<"return-mode">> => <<"open">> };
            _ -> Req
        end,
        Opts
    );
eval(Base, Request, Opts) ->
    eval(<<"eval">>, Base, Request, Opts).
eval(PathKey, Base, Req, Opts) when not is_function(Base) ->
    case hb_ao:get(PathKey, Req, undefined, Opts) of
        undefined ->
            {
                error,
                <<
                    "Path key `",
                    (hb_util:bin(PathKey))/binary,
                    "` not found in request."
                >>
            };
        Path ->
            do_eval(
                fun() -> hb_ao:resolve(Req#{ <<"path">> => Path }, Opts) end,
                Req,
                Opts
            )
    end.

do_eval(Fun, Req, Opts) ->
    % Validate the request and options, then invoke the engine-specific profile
    % function. We match the user-requested engine against the supported engines
    % on the node. Each engine takes three arguments:
    % 1. The function to profile.
    % 2. The request message containing user-provided options.
    % 3. The node options.
    maybe
        true ?= validate_enabled(Opts),
        true ?= validate_signer(Req, Opts),
        true ?= validate_return_mode(Req, Opts),
        {ok, ProfileEngine} ?= engine(hb_ao:get(<<"engine">>, Req, default, Opts)),
        ProfileEngine(Fun, Req, Opts)
    else
        {unknown_engine, Unknown} ->
            {error, <<"Unsupported engine `", (hb_util:bin(Unknown))/binary, "`">>};
        {validation_error, disabled} ->
            {error, <<"Profiling is disabled.">>};
        {validation_error, invalid_request} ->
            {error, <<"Invalid request.">>}
    end.

%%% Validation helpers:

%% @doc Find the profiling options. The supported options for `profiling' in the
%% node message are:
%%  - `true' to enable profiling to allow all requests to be profiled.
%%  - `false' to disable all profiling.
%%  - A list of signers whose requests are allowed to be profiled.
%% If the `profiling' option is not set, check the following other node config
%% options to determine if profiling should be enabled:
%%  - Node message key `mode': `prod' => false, others continue;
%%  - `TEST` build flag: `true' => true, others => false.
find_profiling_config(Opts) ->
    case hb_opts:get(profiling, not_found, Opts) of
        not_found ->
            case hb_opts:get(mode, prod, Opts) of
                prod -> false;
                _ -> hb_features:test()
            end;
        EnableProfiling -> EnableProfiling
    end.

%% @doc Validate that profiling is enabled. 
%% 
%% Return the calculated value _only_ if it is false. If it is not, return
%% true. This allows the `profiling` option to also be used to set a list
%% of valid signers for profiling requests.
validate_enabled(Opts) ->
    case find_profiling_config(Opts) of
        false -> {validation_error, disabled};
        _ -> true
    end.

%% @doc Validate that the request return mode is acceptable. We only allow the
%% `open' mode if the node is in `debug' mode.
validate_return_mode(Req, Opts) ->
    case return_mode(Req, Opts) of
        <<"open">> -> hb_opts:get(mode, prod, Opts) == debug;
        _ -> true
    end.

%% @doc Validate that the request is from a valid signer, if set by the node
%% operator. If the `profiling' config option is `true', all requests are
%% allowed. If it is a list, check if the request is from a valid signer.
validate_signer(Req, Opts) ->
    case find_profiling_config(Opts) of
        ValidSigners when is_list(ValidSigners) ->
            lists:any(
                fun(Signer) -> lists:member(Signer, ValidSigners) end,
                hb_message:signers(Req, Opts)
            );
        EnableProfiling -> EnableProfiling
    end orelse {validation_error, invalid_signer}.

%% @doc Return the profiling function for the given engine.
engine(<<"eflame">>) -> {ok, fun eflame_profile/3};
engine(<<"eprof">>) -> {ok, fun eprof_profile/3};
engine(<<"event">>) -> {ok, fun event_profile/3};
engine(default) -> {ok, default()};
engine(Unknown) -> {unknown_engine, Unknown}.

%% @doc Return the default profiling engine to use. `eflame' if preferred if
%% available, falling back to `eprof' if not.
default() ->
    case hb_features:eflame() of
        true -> fun eflame_profile/3;
        false -> fun eprof_profile/3
    end.

%%% Profiling engines.

-ifdef(ENABLE_EFLAME).
%% @doc Profile a function using the `eflame' tool. This tool is only available
%% if HyperBEAM was built with the `eflame' feature (`rebar3 as eflame ...').
%% If the return mode is `open` and the `profiler_allow_open` option is `true`,
%% we open the flame graph. The command to open the graph is specified by the
%% `profiler_open_cmd' node option, or `open' if not set. A delay of 500ms is
%% added to allow the file to be opened before it is cleaned up. This delay can
%% be configured by the `profiler_open_delay' node option, or 500ms if not set.
eflame_profile(Fun, Req, Opts) ->
    File = temp_file(),
    Res = eflame:apply(normal, File, Fun, []),
    MergeStacks = hb_maps:get(<<"mode">>, Req, <<"merge">>, Opts),
    EflameDir = code:lib_dir(eflame),
    % Get the name of the function to profile. If the path in the request is
    % set, attempt to find it. If that is not found, we use the bare path.
    % This follows the semantics of the request evaluation scheme, in which the
    % user's provided path is a pointer to the actual path to resolve. If the
    % path is not set, we use Erlang's short string encoding of the function.
    Name =
        case hb_maps:get(<<"path">>, Req, undefined, Opts) of
            undefined ->
                hb_escape:encode(hb_util:bin(io_lib:format("~p", [Fun])));
            Path ->
                hb_escape:encode_ampersand(
                    hb_escape:encode_quotes(
                        hb_maps:get(Path, Req, Path, Opts)
                    )
                )
        end,
    StackToFlameScript = hb_util:bin(filename:join(EflameDir, "flamegraph.pl")),
    FlameArg =
        case MergeStacks of
            <<"merge">> -> <<"">>;
            <<"time">> -> <<"--flamechart">>
        end,
    PreparedCommand = 
        hb_util:list(
            <<
                "cat ", (hb_util:bin(File))/binary,
                " | uniq -c | awk '{print $2, \" \", $1}' | ",
                StackToFlameScript/binary, " ", FlameArg/binary,
                " --title=\"", Name/binary, "\" 2>/dev/null"
            >>
        ),
    Flame = hb_util:bin(os:cmd(PreparedCommand)),
    ?event(debug_profile,
        {flame_graph,
            {name, Name},
            {command, PreparedCommand},
            {flame, Flame}
        }
    ),
    file:delete(File),
    case return_mode(Req, Opts) of
        <<"open">> ->
            % We cannot return a text version of the flame graph, so we open it
            % on the local machine.
            file:write_file(
                SVG = filename:absname(temp_file(<<"svg">>)),
                Flame
            ),
            ?event(debug_profile, {svg, SVG}),
            case hb_opts:get(profiler_allow_open, true, Opts) of
                true ->
                    OpenCmd = hb_opts:get(profiler_open_cmd, "open", Opts),
                    CmdRes = os:cmd(OpenCmd ++ " " ++ hb_util:list(SVG)),
                    timer:sleep(hb_opts:get(profiler_open_delay, 500, Opts)),
                    ?event(debug_profile, {open_command_result, CmdRes}),
                    file:delete(SVG),
                    {ok, Res};
                _ ->
                    {SVG, Res}
            end;
        <<"message">> ->
            % We can return the flame graph as an SVG image suitable for output
            % to a browser.
            {ok,
                #{
                    <<"content-type">> => <<"image/svg+xml">>,
                    <<"body">> => Flame
                }
            }
    end.
-else.
eflame_profile(_Fun, _Req, _Opts) ->
    {error, <<"eflame is not enabled.">>}.
-endif.

%% @doc Profile a function using the `eprof' tool.
eprof_profile(Fun, Req, Opts) ->
    File = temp_file(),
    % Attempt to profile the function, stopping the profiler afterwards.
    Res =
        try 
            eprof:start(),
            eprof:start_profiling([self()]),
            Fun()
        catch
            _:_ -> {error, <<"Execution of function to profile failed.">>}
        after eprof:stop_profiling()
        end,
    % If we are writing to the console we do not need to write and read the
    % file.
    case return_mode(Req, Opts) of
        <<"message">> -> eprof:log(File);
        _ -> do_nothing
    end,
    eprof:analyze(total),
    eprof:stop(),
    case return_mode(Req, Opts) of
        <<"message">> ->
            {ok, Log} = file:read_file(File),
            file:delete(File),
            {
                ok,
                #{
                    <<"content-type">> => <<"text/plain">>,
                    <<"body">> => Log
                }
            };
        _ ->
            Res
    end.

%% @doc Profile using HyperBEAM's events.
event_profile(Fun, Req, Opts) ->
    Start = hb_event:counters(),
    Res = Fun(),
    End = hb_event:counters(),
    Diff = hb_message:diff(Start, End, Opts),
    case return_mode(Req, Opts) of
        <<"message">> ->
            {ok, Diff};
        _ ->
            hb_format:print(Diff),
            Res
    end.

%%% Engine helpers: Generalized tools useful for multiple engines.

%% @doc Get the return mode of a profiler run. The run mode is set to `console'
%% by the default handler if the profiler is called from Erlang, and `message'
%% if the profiler is called from AO-Core.
return_mode(Req, Opts) ->
    return_mode(Req, Opts, <<"message">>).
return_mode(Req, Opts, Default) ->
    hb_ao:get(<<"return-mode">>, Req, Default, Opts).

%% @doc Returns a temporary filename for use in a profiling run.
temp_file() -> temp_file(<<"out">>).
temp_file(Ext) ->
    <<
        "profile-",
        (integer_to_binary(os:system_time(microsecond)))/binary,
        ".",
        Ext/binary
    >>.

%%% Tests.

eprof_fun_test() -> test_engine(function, <<"eprof">>).
eprof_resolution_test() -> test_engine(resolution, <<"eprof">>).

-ifdef(ENABLE_EFLAME).
eflame_fun_test() -> test_engine(function, <<"eflame">>).
eflame_resolution_test() -> test_engine(resolution, <<"eflame">>).
-endif.

%%% Test utilities.

%% @doc Run a test and validate the output for a given engine.
test_engine(Type, Engine) ->
    validate_profiler_output(Engine, test_profiler_exec(Type, Engine)).

%% @doc Invoke an engine in either a function (as called from Erlang) or
%% resolution context. We get the build information from the node in order to
%% simulate some basic compute that is performant.
test_profiler_exec(function, Engine) ->
    eval(
        fun() -> dev_meta:build(#{}, #{}, #{}) end,
        #{ <<"engine">> => Engine, <<"return-mode">> => <<"message">> },
        #{}
    );
test_profiler_exec(resolution, Engine) ->
    hb_ao:resolve(
        #{
            <<"path">> => <<"/~profile@1.0/run?run=/~meta@1.0/build">>,
            <<"engine">> => Engine, <<"return-mode">> => <<"message">> },
        #{}
    ).

%% @doc Verify the expected type of output from a profiler.
validate_profiler_output(<<"eprof">>, Res) ->
    ?assertMatch(
        {ok,
            #{
                <<"content-type">> := <<"text/plain">>,
                <<"body">> := Body
            }
        } when byte_size(Body) > 100,
        Res
    );
validate_profiler_output(<<"eflame">>, Res) ->
    ?assertMatch(
        {ok,
            #{
                <<"content-type">> := <<"image/svg+xml">>,
                <<"body">> := Body
            }
        } when byte_size(Body) > 100,
        Res
    ).