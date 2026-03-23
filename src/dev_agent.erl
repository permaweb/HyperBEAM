%%% @doc A ReAct agent device that implements an iterative Reason-Act-Observe
%%% loop. The agent calls an LLM (via relay to an OpenAI-compatible endpoint),
%%% parses tool_calls from the response, executes tools, and loops until the
%%% LLM returns a final text answer or max iterations are reached.
%%%
%%% Architecture:
%%%   dev_agent (standalone device)
%%%     ├── Calls LLM via relay@1.0 → localhost:8080/v1/chat/completions
%%%     ├── Parses OpenAI tool_calls format
%%%     ├── Executes tools via relay@1.0 (HTTP requests)
%%%     └── Manages conversation history internally (Erlang recursion)
%%%
%%% Usage:
%%%   {ok, Result} = hb_ao:resolve(
%%%     #{<<"device">> => <<"agent@1.0">>},
%%%     #{<<"path">> => <<"run">>,
%%%       <<"agent-user-prompt">> => <<"What is the weather?">>},
%%%     #{}).
-module(dev_agent).
-export([info/1, run/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_MAX_ITERATIONS, 10).
-define(DEFAULT_MODEL, <<"gpt-4o-mini">>).
-define(MAX_TOOL_RESULT_SIZE, 4000).

%%%===================================================================
%%% Device API
%%%===================================================================

info(_Msg) ->
    #{
        exports => [<<"run">>]
    }.

%% @doc Entry point for the agent. Reads user prompt from the request,
%% builds initial messages, and starts the ReAct loop.
run(Base, Req, Opts) ->
    UserPrompt = hb_ao:get(<<"agent-user-prompt">>, Req, <<"Hello">>, Opts),
    SystemPrompt = default_system_prompt(),
    Tools = default_tools(),
    Model = hb_ao:get(<<"agent-model">>, Req, ?DEFAULT_MODEL, Opts),
    MaxIter = hb_ao:get(<<"agent-max-iterations">>, Req, ?DEFAULT_MAX_ITERATIONS, Opts),
    LLMFun = maps:get(<<"agent-llm-fun">>, Opts, fun default_call_llm/2),
    ToolFun = maps:get(<<"agent-tool-fun">>, Opts, fun default_execute_tool/2),
    Messages = [
        #{<<"role">> => <<"system">>, <<"content">> => SystemPrompt},
        #{<<"role">> => <<"user">>, <<"content">> => UserPrompt}
    ],
    AgentOpts = #{
        model => Model,
        tools => Tools,
        max_iterations => MaxIter,
        llm_fun => LLMFun,
        tool_fun => ToolFun,
        opts => Opts
    },
    loop(Messages, 1, AgentOpts, Base).

%%%===================================================================
%%% ReAct Loop
%%%===================================================================

%% @doc The main ReAct loop. Calls LLM, checks for tool_calls or final answer.
loop(Messages, Iteration, #{max_iterations := MaxIter} = AgentOpts, Base)
  when Iteration > MaxIter ->
    ?event(agent, {max_iterations_reached, Iteration}),
    {ok, Base#{
        <<"agent-answer">> =>
            <<"Max iterations reached. Could not complete the task.">>,
        <<"agent-error">> => <<"max_iterations_exceeded">>,
        <<"agent-iterations">> => Iteration - 1
    }};
loop(Messages, Iteration, AgentOpts, Base) ->
    #{model := Model, tools := Tools, llm_fun := LLMFun,
      tool_fun := ToolFun, opts := Opts} = AgentOpts,
    RequestBody = build_request_body(Messages, Tools, Model),
    case LLMFun(RequestBody, Opts) of
        {ok, ResponseBody} ->
            case parse_response(ResponseBody) of
                {tool_calls, AssistantMsg, ToolCalls} ->
                    ?event(agent, {tool_calls, Iteration, length(ToolCalls)}),
                    ToolResultMsgs = execute_tool_calls(ToolCalls, ToolFun, Opts),
                    NewMessages = Messages ++ [AssistantMsg | ToolResultMsgs],
                    loop(NewMessages, Iteration + 1, AgentOpts, Base);
                {final_answer, Content} ->
                    ?event(agent, {final_answer, Iteration}),
                    {ok, Base#{
                        <<"agent-answer">> => Content,
                        <<"agent-iterations">> => Iteration
                    }};
                {error, ParseError} ->
                    {ok, Base#{
                        <<"agent-answer">> => <<"Error parsing LLM response.">>,
                        <<"agent-error">> => ParseError,
                        <<"agent-iterations">> => Iteration
                    }}
            end;
        {error, LLMError} ->
            {ok, Base#{
                <<"agent-answer">> => <<"Error calling LLM.">>,
                <<"agent-error">> => LLMError,
                <<"agent-iterations">> => Iteration
            }}
    end.

%%%===================================================================
%%% LLM Interaction
%%%===================================================================

%% @doc Build the OpenAI-compatible request body.
build_request_body(Messages, Tools, Model) ->
    hb_json:encode(#{
        <<"model">> => Model,
        <<"messages">> => Messages,
        <<"tools">> => Tools,
        <<"tool_choice">> => <<"auto">>
    }).

%% @doc Default LLM call via relay@1.0 to localhost:8080.
default_call_llm(RequestBody, Opts) ->
    case hb_ao:resolve(
        #{<<"device">> => <<"relay@1.0">>,
          <<"content-type">> => <<"application/json">>},
        #{<<"path">> => <<"call">>,
          <<"target">> => <<"payload">>,
          <<"payload">> => #{
              <<"path">> => <<"/v1/chat/completions">>,
              <<"method">> => <<"POST">>,
              <<"body">> => RequestBody,
              <<"content-type">> => <<"application/json">>
          }},
        Opts#{hashpath => ignore,
              cache_control => [<<"no-store">>, <<"no-cache">>]}
    ) of
        {ok, Res} ->
            {ok, hb_ao:get(<<"body">>, Res, <<>>, Opts)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Parse an OpenAI chat completion response.
%% Returns {tool_calls, AssistantMsg, ToolCalls} | {final_answer, Content} | {error, Reason}.
parse_response(ResponseBody) when is_binary(ResponseBody) ->
    try
        Decoded = hb_json:decode(ResponseBody),
        parse_response(Decoded)
    catch
        _:_ -> {error, <<"Failed to decode JSON response">>}
    end;
parse_response(Response) when is_map(Response) ->
    Choices = maps:get(<<"choices">>, Response, []),
    case Choices of
        [FirstChoice | _] ->
            Message = maps:get(<<"message">>, FirstChoice, #{}),
            parse_assistant_message(Message);
        [] ->
            {error, <<"No choices in response">>}
    end.

%% @doc Parse the assistant message for tool_calls or final answer.
parse_assistant_message(Message) ->
    ToolCalls = maps:get(<<"tool_calls">>, Message, null),
    Content = maps:get(<<"content">>, Message, null),
    case ToolCalls of
        null -> {final_answer, content_to_binary(Content)};
        [] -> {final_answer, content_to_binary(Content)};
        Calls when is_list(Calls) ->
            %% Build assistant message to include in history
            AssistantMsg = #{
                <<"role">> => <<"assistant">>,
                <<"content">> => Content,
                <<"tool_calls">> => Calls
            },
            {tool_calls, AssistantMsg, Calls}
    end.

content_to_binary(null) -> <<>>;
content_to_binary(B) when is_binary(B) -> B;
content_to_binary(Other) -> hb_util:bin(Other).

%%%===================================================================
%%% Tool Execution
%%%===================================================================

%% @doc Execute a list of tool calls, returning tool result messages.
execute_tool_calls(ToolCalls, ToolFun, Opts) ->
    lists:map(
        fun(ToolCall) ->
            ToolCallId = maps:get(<<"id">>, ToolCall, <<>>),
            Function = maps:get(<<"function">>, ToolCall, #{}),
            FuncName = maps:get(<<"name">>, Function, <<>>),
            ArgsJson = maps:get(<<"arguments">>, Function, <<"{}">>),
            Args = try hb_json:decode(ArgsJson) catch _:_ -> #{} end,
            Result = ToolFun(#{name => FuncName, args => Args}, Opts),
            #{
                <<"role">> => <<"tool">>,
                <<"tool_call_id">> => ToolCallId,
                <<"content">> => truncate_result(Result)
            }
        end,
        ToolCalls
    ).

%% @doc Default tool executor. Dispatches by function name.
default_execute_tool(#{name := <<"http_request">>, args := Args}, Opts) ->
    execute_http_request(Args, Opts);
default_execute_tool(#{name := Name}, _Opts) ->
    <<"Unknown tool: ", Name/binary>>.

%% @doc Execute an HTTP request via relay@1.0.
execute_http_request(Args, Opts) ->
    Method = maps:get(<<"method">>, Args, <<"GET">>),
    Url = maps:get(<<"url">>, Args, <<>>),
    Body = maps:get(<<"body">>, Args, <<>>),
    ContentType = maps:get(<<"content_type">>, Args, <<"text/plain">>),
    case hb_ao:resolve(
        #{<<"device">> => <<"relay@1.0">>,
          <<"content-type">> => ContentType},
        #{<<"path">> => <<"call">>,
          <<"target">> => <<"payload">>,
          <<"payload">> => #{
              <<"path">> => Url,
              <<"method">> => Method,
              <<"body">> => Body,
              <<"content-type">> => ContentType
          }},
        Opts#{hashpath => ignore,
              cache_control => [<<"no-store">>, <<"no-cache">>]}
    ) of
        {ok, Res} ->
            hb_ao:get(<<"body">>, Res, <<"No body">>, Opts);
        {error, Reason} ->
            iolist_to_binary(io_lib:format("HTTP request failed: ~p", [Reason]))
    end.

%% @doc Truncate tool results to avoid exceeding LLM context limits.
truncate_result(Result) when is_binary(Result),
                             byte_size(Result) > ?MAX_TOOL_RESULT_SIZE ->
    <<Truncated:?MAX_TOOL_RESULT_SIZE/binary, _/binary>> = Result,
    <<Truncated/binary, "\n... [truncated]">>;
truncate_result(Result) when is_binary(Result) ->
    Result;
truncate_result(Result) ->
    truncate_result(iolist_to_binary(io_lib:format("~p", [Result]))).

%%%===================================================================
%%% Default Configuration (hardcoded for MVP)
%%%===================================================================

default_system_prompt() ->
    <<"You are a helpful AI assistant running on the AO network. "
      "You can make HTTP requests using the http_request tool. "
      "When you have enough information to answer the user's question, "
      "respond with a text message (do not call any tools). "
      "Keep responses concise.">>.

default_tools() ->
    [#{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"http_request">>,
            <<"description">> => <<"Make an HTTP request to a URL.">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"url">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The URL to request.">>
                    },
                    <<"method">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>],
                        <<"description">> => <<"HTTP method. Defaults to GET.">>
                    },
                    <<"body">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"Request body (for POST/PUT).">>
                    },
                    <<"content_type">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> =>
                            <<"Content-Type header. Defaults to text/plain.">>
                    }
                },
                <<"required">> => [<<"url">>]
            }
        }
    }].

%%%===================================================================
%%% Tests
%%%===================================================================

%% Helper: create a mock LLM function that returns a sequence of responses.
mock_llm_sequence(Responses) ->
    Counter = atomics:new(1, [{signed, false}]),
    fun(_RequestBody, _Opts) ->
        Idx = atomics:add_get(Counter, 1, 1),
        case Idx =< length(Responses) of
            true -> lists:nth(Idx, Responses);
            false -> {error, <<"No more mock responses">>}
        end
    end.

%% Helper: build a mock OpenAI response with a text answer (no tool_calls).
mock_text_response(Content) ->
    {ok, hb_json:encode(#{
        <<"choices">> => [#{
            <<"message">> => #{
                <<"role">> => <<"assistant">>,
                <<"content">> => Content
            }
        }]
    })}.

%% Helper: build a mock OpenAI response with tool_calls.
mock_tool_call_response(ToolCalls) ->
    {ok, hb_json:encode(#{
        <<"choices">> => [#{
            <<"message">> => #{
                <<"role">> => <<"assistant">>,
                <<"content">> => null,
                <<"tool_calls">> => ToolCalls
            }
        }]
    })}.

%% Helper: build a single tool_call structure.
make_tool_call(Id, FuncName, ArgsMap) ->
    #{
        <<"id">> => Id,
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => FuncName,
            <<"arguments">> => hb_json:encode(ArgsMap)
        }
    }.

%% Helper: a mock tool function that records calls and returns a fixed result.
mock_tool_fun(Result) ->
    fun(_ToolInfo, _Opts) -> Result end.

%% Helper: a mock tool function that records calls.
mock_tool_fun_with_tracker(Result) ->
    Tracker = ets:new(tool_calls_tracker, [bag, public]),
    Fun = fun(ToolInfo, _Opts) ->
        ets:insert(Tracker, {call, ToolInfo}),
        Result
    end,
    {Fun, Tracker}.

%% @doc Test 1: LLM directly returns text answer, no tool calls.
single_turn_no_tools_test() ->
    LLMFun = mock_llm_sequence([
        mock_text_response(<<"Hello! How can I help you?">>)
    ]),
    ToolFun = mock_tool_fun(<<"should not be called">>),
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Hi there">>},
    Opts = #{
        <<"agent-llm-fun">> => LLMFun,
        <<"agent-tool-fun">> => ToolFun
    },
    {ok, Result} = run(Base, Req, Opts),
    ?assertEqual(<<"Hello! How can I help you?">>,
                 maps:get(<<"agent-answer">>, Result)),
    ?assertEqual(1, maps:get(<<"agent-iterations">>, Result)).

%% @doc Test 2: LLM calls a tool, then returns final answer.
single_tool_call_test() ->
    ToolCall = make_tool_call(
        <<"call_1">>,
        <<"http_request">>,
        #{<<"url">> => <<"https://example.com/api">>}
    ),
    LLMFun = mock_llm_sequence([
        mock_tool_call_response([ToolCall]),
        mock_text_response(<<"The result is 42.">>)
    ]),
    ToolFun = mock_tool_fun(<<"{\"value\": 42}">>),
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Get the value">>},
    Opts = #{
        <<"agent-llm-fun">> => LLMFun,
        <<"agent-tool-fun">> => ToolFun
    },
    {ok, Result} = run(Base, Req, Opts),
    ?assertEqual(<<"The result is 42.">>,
                 maps:get(<<"agent-answer">>, Result)),
    ?assertEqual(2, maps:get(<<"agent-iterations">>, Result)).

%% @doc Test 3: LLM returns multiple tool calls in a single response.
multiple_tool_calls_test() ->
    ToolCall1 = make_tool_call(
        <<"call_1">>, <<"http_request">>,
        #{<<"url">> => <<"https://api.example.com/a">>}
    ),
    ToolCall2 = make_tool_call(
        <<"call_2">>, <<"http_request">>,
        #{<<"url">> => <<"https://api.example.com/b">>}
    ),
    LLMFun = mock_llm_sequence([
        mock_tool_call_response([ToolCall1, ToolCall2]),
        mock_text_response(<<"Combined result.">>)
    ]),
    {ToolFun, Tracker} = mock_tool_fun_with_tracker(<<"ok">>),
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Fetch both">>},
    Opts = #{
        <<"agent-llm-fun">> => LLMFun,
        <<"agent-tool-fun">> => ToolFun
    },
    {ok, Result} = run(Base, Req, Opts),
    ?assertEqual(<<"Combined result.">>,
                 maps:get(<<"agent-answer">>, Result)),
    %% Verify both tools were called
    Calls = ets:lookup(Tracker, call),
    ?assertEqual(2, length(Calls)),
    ets:delete(Tracker).

%% @doc Test 4: Max iterations reached, agent force-stops.
max_iterations_test() ->
    %% LLM always returns tool_calls, never a final answer
    ToolCall = make_tool_call(
        <<"call_loop">>, <<"http_request">>,
        #{<<"url">> => <<"https://example.com">>}
    ),
    AlwaysToolCall = fun(_Body, _Opts) ->
        mock_tool_call_response([ToolCall])
    end,
    ToolFun = mock_tool_fun(<<"result">>),
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Loop forever">>,
            <<"agent-max-iterations">> => 3},
    Opts = #{
        <<"agent-llm-fun">> => AlwaysToolCall,
        <<"agent-tool-fun">> => ToolFun
    },
    {ok, Result} = run(Base, Req, Opts),
    ?assertEqual(<<"max_iterations_exceeded">>,
                 maps:get(<<"agent-error">>, Result)),
    ?assertEqual(3, maps:get(<<"agent-iterations">>, Result)).

%% @doc Test 5: Tool execution returns an error, which is passed back to LLM.
tool_error_handling_test() ->
    ToolCall = make_tool_call(
        <<"call_err">>, <<"http_request">>,
        #{<<"url">> => <<"https://fail.example.com">>}
    ),
    LLMFun = mock_llm_sequence([
        mock_tool_call_response([ToolCall]),
        mock_text_response(<<"Sorry, the request failed.">>)
    ]),
    ToolFun = mock_tool_fun(<<"HTTP request failed: timeout">>),
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Try failing URL">>},
    Opts = #{
        <<"agent-llm-fun">> => LLMFun,
        <<"agent-tool-fun">> => ToolFun
    },
    {ok, Result} = run(Base, Req, Opts),
    %% Agent should still complete despite tool error
    ?assertEqual(<<"Sorry, the request failed.">>,
                 maps:get(<<"agent-answer">>, Result)),
    ?assertEqual(2, maps:get(<<"agent-iterations">>, Result)).

%% @doc Test 6: Verify conversation history format is correct OpenAI format.
message_history_test() ->
    ToolCall = make_tool_call(
        <<"call_hist">>, <<"http_request">>,
        #{<<"url">> => <<"https://example.com">>}
    ),
    %% Track what messages the LLM receives on 2nd call
    MessageTracker = ets:new(msg_tracker, [set, public]),
    LLMFun = fun(RequestBody, _Opts) ->
        Decoded = hb_json:decode(RequestBody),
        Messages = maps:get(<<"messages">>, Decoded, []),
        ets:insert(MessageTracker, {length(Messages), Messages}),
        case length(Messages) of
            2 -> %% First call: [system, user]
                mock_tool_call_response([ToolCall]);
            _ -> %% Second call: should have full history
                mock_text_response(<<"Done.">>)
        end
    end,
    ToolFun = mock_tool_fun(<<"tool output">>),
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Test history">>},
    Opts = #{
        <<"agent-llm-fun">> => LLMFun,
        <<"agent-tool-fun">> => ToolFun
    },
    {ok, _Result} = run(Base, Req, Opts),
    %% Check 2nd call had correct history:
    %% [system, user, assistant(tool_calls), tool(result)] = 4 messages
    [{4, SecondCallMessages}] = ets:lookup(MessageTracker, 4),
    %% Verify message roles in order
    Roles = [maps:get(<<"role">>, M) || M <- SecondCallMessages],
    ?assertEqual([<<"system">>, <<"user">>, <<"assistant">>, <<"tool">>], Roles),
    %% Verify the tool message has the correct tool_call_id
    ToolMsg = lists:last(SecondCallMessages),
    ?assertEqual(<<"call_hist">>, maps:get(<<"tool_call_id">>, ToolMsg)),
    ?assertEqual(<<"tool output">>, maps:get(<<"content">>, ToolMsg)),
    ets:delete(MessageTracker).

%% @doc Test: truncate_result works correctly.
truncate_result_test() ->
    Short = <<"short">>,
    ?assertEqual(Short, truncate_result(Short)),
    Long = binary:copy(<<"x">>, 5000),
    Truncated = truncate_result(Long),
    ?assert(byte_size(Truncated) < 5000),
    ?assert(binary:match(Truncated, <<"[truncated]">>) =/= nomatch).

%% @doc Test: unknown tool returns error message.
unknown_tool_test() ->
    ToolCall = make_tool_call(
        <<"call_unk">>, <<"unknown_func">>,
        #{<<"arg">> => <<"val">>}
    ),
    LLMFun = mock_llm_sequence([
        mock_tool_call_response([ToolCall]),
        mock_text_response(<<"I don't know that tool.">>)
    ]),
    %% Use default_execute_tool which handles unknown tools
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Use unknown tool">>},
    Opts = #{
        <<"agent-llm-fun">> => LLMFun,
        <<"agent-tool-fun">> => fun default_execute_tool/2
    },
    {ok, Result} = run(Base, Req, Opts),
    ?assertEqual(<<"I don't know that tool.">>,
                 maps:get(<<"agent-answer">>, Result)).
