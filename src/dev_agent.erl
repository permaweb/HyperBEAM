%%% @doc A ReAct agent device that implements an iterative Reason-Act-Observe
%%% loop. The agent calls an LLM (via relay to an OpenAI-compatible endpoint),
%%% parses tool_calls from the response, executes tools, and loops until the
%%% LLM returns a final text answer or max iterations are reached.
%%%
%%% Architecture:
%%%   dev_agent (standalone device)
%%%     ├── Calls LLM via inference@1.0 (OpenAI-compatible, local or remote)
%%%     │     └── inference@1.0 relays via relay@1.0 to the configured peer
%%%     ├── Parses OpenAI tool_calls format
%%%     ├── Executes built-in tools (http_request, lookup_data, …)
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
    %% Merge agent API config from Req/Base into Opts so that
    %% Lua ao.resolve calls can pass config via message fields.
    MergedOpts = merge_agent_config(Req, merge_agent_config(Base, Opts)),
    LLMFun = maps:get(<<"agent-llm-fun">>, MergedOpts, fun default_call_llm/2),
    ToolFun = maps:get(<<"agent-tool-fun">>, MergedOpts, fun default_execute_tool/2),
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
        opts => MergedOpts
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

%% @doc Default LLM call via inference@1.0.
%% inference@1.0 relays to any OpenAI-compatible provider via relay@1.0,
%% making local and remote LLM backends interchangeable from the agent's
%% perspective. Supported Opts keys (consumed by inference@1.0/relay@1.0):
%%   agent-api-peer: Base URL           (default: "http://localhost:8080")
%%   agent-api-path: Completions path   (default: "/v1/chat/completions")
%%   agent-api-key:  Bearer token       (optional)
default_call_llm(RequestBody, Opts) ->
    case hb_ao:resolve(
        #{<<"device">>    => <<"inference@1.0">>,
          <<"chat-mode">> => true},
        #{<<"path">> => <<"completions">>,
          <<"body">> => RequestBody},
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
default_execute_tool(#{name := <<"lookup_data">>, args := Args}, Opts) ->
    execute_lookup(Args, Opts);
default_execute_tool(#{name := <<"search_messages">>, args := Args}, Opts) ->
    execute_search(Args, Opts);
default_execute_tool(#{name := <<"get_arweave_tx">>, args := Args}, Opts) ->
    execute_get_arweave_tx(Args, Opts);
default_execute_tool(#{name := <<"bundle_item">>, args := Args}, Opts) ->
    execute_bundle_item(Args, Opts);
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

%% @doc Look up an item from the local cache by ID via lookup@1.0.
execute_lookup(Args, Opts) ->
    ID = maps:get(<<"id">>, Args, <<>>),
    case hb_ao:resolve(
        #{<<"device">> => <<"lookup@1.0">>},
        #{<<"path">> => <<"read">>, <<"target">> => ID},
        Opts#{hashpath => ignore}
    ) of
        {ok, Res} when is_map(Res) -> hb_json:encode(Res);
        {ok, Res} when is_binary(Res) -> Res;
        {ok, Res} -> hb_util:bin(Res);
        {error, not_found} -> <<"Not found: ", ID/binary>>;
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Lookup failed: ~p", [Reason]))
    end.

%% @doc Search the node cache for messages matching a key-value spec via query@1.0.
execute_search(Args, Opts) ->
    MatchSpec = maps:get(<<"match">>, Args, #{}),
    ReturnType = maps:get(<<"return">>, Args, <<"paths">>),
    %% Merge match keys into the request so query@1.0's `all' path picks them up.
    Req = maps:merge(MatchSpec, #{<<"path">> => <<"all">>, <<"return">> => ReturnType}),
    case hb_ao:resolve(
        #{<<"device">> => <<"query@1.0">>},
        Req,
        Opts#{hashpath => ignore}
    ) of
        {ok, Results} -> hb_json:encode(Results);
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Search failed: ~p", [Reason]))
    end.

%% @doc Fetch an Arweave transaction by TXID via arweave@2.9-pre.
execute_get_arweave_tx(Args, Opts) ->
    TXID = maps:get(<<"id">>, Args, <<>>),
    case hb_ao:resolve(
        #{<<"device">> => <<"arweave@2.9-pre">>},
        #{<<"path">> => <<"tx">>, <<"tx">> => TXID},
        Opts#{hashpath => ignore}
    ) of
        {ok, Res} when is_map(Res) -> hb_json:encode(Res);
        {ok, Res} when is_binary(Res) -> Res;
        {ok, Res} -> hb_util:bin(Res);
        {error, not_found} -> <<"Transaction not found: ", TXID/binary>>;
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Arweave TX fetch failed: ~p", [Reason]))
    end.

%% @doc Submit a data item to Arweave via the node's bundler@1.0.
execute_bundle_item(Args, Opts) ->
    Data = maps:get(<<"data">>, Args, <<>>),
    ContentType = maps:get(<<"content_type">>, Args, <<"text/plain">>),
    case hb_ao:resolve(
        #{<<"device">> => <<"bundler@1.0">>},
        #{<<"path">> => <<"item">>,
          <<"data">> => Data,
          <<"content-type">> => ContentType},
        Opts#{hashpath => ignore}
    ) of
        {ok, Res} when is_binary(Res) -> Res;
        {ok, Res} -> hb_util:bin(Res);
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Bundle failed: ~p", [Reason]))
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
%%% Configuration Helpers
%%%===================================================================

%% @doc Merge agent API config from a Source message into Opts.
%% Keys already present in Opts are not overwritten.
%% This allows Lua ao.resolve calls to pass config via Req/Base.
merge_agent_config(Source, Opts) ->
    Keys = [<<"agent-api-peer">>, <<"agent-api-path">>, <<"agent-api-key">>],
    lists:foldl(fun(Key, Acc) ->
        case maps:is_key(Key, Acc) of
            true -> Acc;
            false ->
                case hb_ao:get(Key, Source, not_found, Opts#{hashpath => ignore}) of
                    not_found -> Acc;
                    Val -> Acc#{Key => Val}
                end
        end
    end, Opts, Keys).

%%%===================================================================
%%% Default Configuration (hardcoded for MVP)
%%%===================================================================

default_system_prompt() ->
    <<"You are a helpful AI assistant running on the AO network. "
      "You have the following tools available:\n"
      "- http_request: Make HTTP requests to any URL.\n"
      "- lookup_data: Retrieve a cached message or data item by its ID.\n"
      "- search_messages: Search the node cache for messages matching key-value criteria.\n"
      "- get_arweave_tx: Fetch an Arweave transaction by its transaction ID.\n"
      "- bundle_item: Submit data to be bundled and uploaded to Arweave.\n"
      "When you have enough information to answer the user's question, "
      "respond with a text message (do not call any tools). "
      "Keep responses concise.">>.

default_tools() ->
    [tool_def(http_request), tool_def(lookup_data),
     tool_def(search_messages), tool_def(get_arweave_tx),
     tool_def(bundle_item)].

tool_def(http_request) ->
    #{
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
    };
tool_def(lookup_data) ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"lookup_data">>,
            <<"description">> =>
                <<"Retrieve a message or data item from the node's local cache by its ID.">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"id">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The hash ID of the item to retrieve.">>
                    }
                },
                <<"required">> => [<<"id">>]
            }
        }
    };
tool_def(search_messages) ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"search_messages">>,
            <<"description">> =>
                <<"Search the node's cache for messages matching key-value criteria.">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"match">> => #{
                        <<"type">> => <<"object">>,
                        <<"description">> =>
                            <<"JSON object of key-value pairs to match against cached messages.">>
                    },
                    <<"return">> => #{
                        <<"type">> => <<"string">>,
                        <<"enum">> => [<<"paths">>, <<"messages">>, <<"count">>],
                        <<"description">> =>
                            <<"What to return: paths (default), full messages, or count.">>
                    }
                },
                <<"required">> => [<<"match">>]
            }
        }
    };
tool_def(get_arweave_tx) ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"get_arweave_tx">>,
            <<"description">> =>
                <<"Fetch an Arweave transaction header by its 43-character base64url transaction ID.">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"id">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The Arweave transaction ID.">>
                    }
                },
                <<"required">> => [<<"id">>]
            }
        }
    };
tool_def(bundle_item) ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => <<"bundle_item">>,
            <<"description">> =>
                <<"Submit a data item to be bundled and uploaded to Arweave.">>,
            <<"parameters">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"data">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> => <<"The data content to store on Arweave.">>
                    },
                    <<"content_type">> => #{
                        <<"type">> => <<"string">>,
                        <<"description">> =>
                            <<"Content-Type of the data. Defaults to text/plain.">>
                    }
                },
                <<"required">> => [<<"data">>]
            }
        }
    }.

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

%% @doc LLM directly returns text answer, no tool calls.
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

%% @doc LLM calls a tool, then returns final answer.
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

%% @doc LLM returns multiple tool calls in a single response.
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

%% @doc Max iterations reached, agent force-stops.
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

%% @doc Tool execution returns an error, which is passed back to LLM.
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

%% @doc Verify conversation history is in correct OpenAI format.
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

%% @doc truncate_result/1 correctly truncates long binaries.
truncate_result_test() ->
    Short = <<"short">>,
    ?assertEqual(Short, truncate_result(Short)),
    Long = binary:copy(<<"x">>, 5000),
    Truncated = truncate_result(Long),
    ?assert(byte_size(Truncated) < 5000),
    ?assert(binary:match(Truncated, <<"[truncated]">>) =/= nomatch).

%% @doc Unknown tool name returns a descriptive error binary.
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

%% @doc Test: Lua ao.resolve style call — config in Req, not Opts.
%% Simulates: ao.resolve({path="/~agent@1.0/run", ["agent-api-peer"]=..., ...})
lua_resolve_style_test() ->
    LLMFun = mock_llm_sequence([
        mock_text_response(<<"Paris">>)
    ]),
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{
        <<"path">> => <<"run">>,
        <<"agent-user-prompt">> => <<"Capital of France?">>,
        <<"agent-model">> => <<"test-model">>,
        <<"agent-api-peer">> => <<"https://api.example.com">>,
        <<"agent-api-path">> => <<"/v1/chat/completions">>,
        <<"agent-api-key">> => <<"sk_test_key">>
    },
    Opts = #{
        <<"agent-llm-fun">> => LLMFun,
        <<"agent-tool-fun">> => fun default_execute_tool/2
    },
    {ok, Result} = run(Base, Req, Opts),
    ?assertEqual(<<"Paris">>, maps:get(<<"agent-answer">>, Result)),
    ?assertEqual(1, maps:get(<<"agent-iterations">>, Result)).

%% @doc lookup_data tool call is routed with correct args.
lookup_data_tool_test() ->
    ToolCall = make_tool_call(<<"c_lookup">>, <<"lookup_data">>,
                              #{<<"id">> => <<"deadbeef123">>}),
    LLMFun = mock_llm_sequence([
        mock_tool_call_response([ToolCall]),
        mock_text_response(<<"Found the item.">>)
    ]),
    ToolFun = fun(#{name := <<"lookup_data">>, args := Args}, _Opts) ->
        ?assertEqual(<<"deadbeef123">>, maps:get(<<"id">>, Args)),
        <<"mock cached data">>
    end,
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Look up the item">>},
    Opts = #{<<"agent-llm-fun">> => LLMFun, <<"agent-tool-fun">> => ToolFun},
    {ok, Result} = run(Base, Req, Opts),
    ?assertEqual(<<"Found the item.">>, maps:get(<<"agent-answer">>, Result)),
    ?assertEqual(2, maps:get(<<"agent-iterations">>, Result)).

%% @doc search_messages tool call is routed with correct args.
search_messages_tool_test() ->
    ToolCall = make_tool_call(<<"c_search">>, <<"search_messages">>,
                              #{<<"match">> => #{<<"type">> => <<"Process">>},
                                <<"return">> => <<"paths">>}),
    LLMFun = mock_llm_sequence([
        mock_tool_call_response([ToolCall]),
        mock_text_response(<<"Found 2 processes.">>)
    ]),
    ToolFun = fun(#{name := <<"search_messages">>, args := Args}, _Opts) ->
        Match = maps:get(<<"match">>, Args),
        ?assertEqual(<<"Process">>, maps:get(<<"type">>, Match)),
        ?assertEqual(<<"paths">>, maps:get(<<"return">>, Args)),
        <<"[\"/path/a\",\"/path/b\"]">>
    end,
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Find all processes">>},
    Opts = #{<<"agent-llm-fun">> => LLMFun, <<"agent-tool-fun">> => ToolFun},
    {ok, Result} = run(Base, Req, Opts),
    ?assertEqual(<<"Found 2 processes.">>, maps:get(<<"agent-answer">>, Result)).

%% @doc get_arweave_tx tool call is routed with correct args.
get_arweave_tx_tool_test() ->
    TXID = <<"43characterbase64urlTXIDxxxxxxxxxxxxxxxx123">>,
    ToolCall = make_tool_call(<<"c_tx">>, <<"get_arweave_tx">>,
                              #{<<"id">> => TXID}),
    LLMFun = mock_llm_sequence([
        mock_tool_call_response([ToolCall]),
        mock_text_response(<<"Transaction found.">>)
    ]),
    ToolFun = fun(#{name := <<"get_arweave_tx">>, args := Args}, _Opts) ->
        ?assertEqual(TXID, maps:get(<<"id">>, Args)),
        <<"{\"id\":\"", TXID/binary, "\"}">>
    end,
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Get the Arweave TX">>},
    Opts = #{<<"agent-llm-fun">> => LLMFun, <<"agent-tool-fun">> => ToolFun},
    {ok, Result} = run(Base, Req, Opts),
    ?assertEqual(<<"Transaction found.">>, maps:get(<<"agent-answer">>, Result)).

%% @doc bundle_item tool call is routed with correct args.
bundle_item_tool_test() ->
    Data = <<"Hello Arweave">>,
    ToolCall = make_tool_call(<<"c_bundle">>, <<"bundle_item">>,
                              #{<<"data">> => Data,
                                <<"content_type">> => <<"text/plain">>}),
    LLMFun = mock_llm_sequence([
        mock_tool_call_response([ToolCall]),
        mock_text_response(<<"Data submitted to Arweave.">>)
    ]),
    ToolFun = fun(#{name := <<"bundle_item">>, args := Args}, _Opts) ->
        ?assertEqual(Data, maps:get(<<"data">>, Args)),
        ?assertEqual(<<"text/plain">>, maps:get(<<"content_type">>, Args)),
        <<"Message queued.">>
    end,
    Base = #{<<"device">> => <<"agent@1.0">>},
    Req = #{<<"path">> => <<"run">>,
            <<"agent-user-prompt">> => <<"Store this to Arweave">>},
    Opts = #{<<"agent-llm-fun">> => LLMFun, <<"agent-tool-fun">> => ToolFun},
    {ok, Result} = run(Base, Req, Opts),
    ?assertEqual(<<"Data submitted to Arweave.">>, maps:get(<<"agent-answer">>, Result)).

%% @doc Integration test: real LLM API call via OpenRouter with tool use.
%% Requires a valid OPENROUTER_API_KEY environment variable or direct substitution.
%% Run with: rebar3 eunit --module=dev_agent --test=integration_real_api_test
integration_real_api_test_() ->
    {timeout, 120, fun() ->
        application:ensure_all_started(gun),
        ApiKey = list_to_binary(
            os:getenv("OPENROUTER_API_KEY", "sk-or-v1-YOUR_KEY_HERE")),
        Base = #{<<"device">> => <<"agent@1.0">>},
        Req = #{
            <<"path">> => <<"run">>,
            <<"agent-user-prompt">> =>
                <<"Use the http_request tool to GET https://httpbin.org/get "
                  "and tell me what the 'origin' IP address is from the response.">>,
            <<"agent-model">> => <<"openai/gpt-4o-mini">>,
            <<"agent-max-iterations">> => 5
        },
        Opts = #{
            <<"agent-api-peer">> => <<"https://openrouter.ai">>,
            <<"agent-api-path">> => <<"/api/v1/chat/completions">>,
            <<"agent-api-key">> => ApiKey,
            protocol => http2
        },
        {ok, Result} = run(Base, Req, Opts),
        Answer = maps:get(<<"agent-answer">>, Result),
        Iterations = maps:get(<<"agent-iterations">>, Result),
        ?debugFmt("~n=== Integration Test Result ===~n"
                  "Answer: ~s~nIterations: ~p~nFull result: ~p~n",
                  [Answer, Iterations, Result]),
        %% Should have completed in more than 1 iteration (tool was called)
        ?assert(Iterations >= 2),
        %% Answer should contain meaningful content (not an error)
        ?assert(byte_size(Answer) > 10),
        ?assertNot(maps:is_key(<<"agent-error">>, Result))
    end}.
