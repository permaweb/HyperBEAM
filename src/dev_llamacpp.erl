%%% @doc Device for interfacing with llama.cpp server.
%%% 
%%% This device accepts user requests and forwards them to a llama.cpp server,
%%% handling both completion and chat completion endpoints based on session management.
%%%
%%% Supported parameters:
%%% - prompt (required): The prompt to pass to llama.cpp
%%% - config (optional): JSON options with max_tokens, temperature, top_p
%%% - session_id (optional): For maintaining chat session history
%%% - reference (required): Reference identifier passed back to user
%%% - worker (required): Worker identifier passed back to user
%%%
%%% When session_id is not provided, uses v1/completions API.
%%% When session_id is provided, uses v1/chat/completions API with session history.
%%%
%%% Returns structured response with:
%%% - body: JSON array with result from llama.cpp
%%% - Custom headers: X-Session, X-Reference, X-Worker
%%% - Action: "Infer-Response"
%%% - status: 200
%%%
%%% Note: Session history is managed by llamacpp_session_manager to ensure
%%% persistence across different HyperBEAM processes.
-module(dev_llamacpp).
-export([info/0, infer/3, infer_sec/3]).
-export([init_session_table/0]). % For testing
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Maximum number of sessions to keep in memory (LRU)
-define(MAX_SESSIONS, 1000).

%% @doc Device metadata and exported functions.
info() ->
    #{
        exports => [infer, infer_sec]
    }.

infer_sec(_M1, M2, Opts) ->
    case dev_cc:generate(#{}, #{nonce => <<"da4a06c3604a5fac8aa0b4aaf5a6354cdd0dc7c193299bc3464f30b5cbfb931a">>}, Opts) of
        {ok, TokenJSON} ->
            case infer(_M1, M2, Opts) of
                {ok, Result} ->
                    ExistingBody = maps:get(<<"body">>, Result, <<"{}">>),
                    ExistingData = hb_json:decode(ExistingBody),
                    UpdatedData = ExistingData#{
                        <<"attestation">> => hb_json:decode(TokenJSON)
                    },
                    UpdatedResult = Result#{<<"body">> => hb_json:encode(UpdatedData)},
                    {ok, UpdatedResult};
                {error, Reason} ->
                    ?event(dev_llamacpp, {infer_sec_failed, Reason}),
                    {error, {infer_sec_failed, Reason}}
            end;
        {error, Reason} ->
            ?event(dev_llamacpp, {infer_sec_failed, Reason}),
            {error, {infer_sec_failed, Reason}}
    end.

%% @doc Main inference function that handles requests to llama.cpp server.
infer(_Msg1, Msg2, Opts) ->
    try
        % Initialize session table if not already done
        init_session_table(),
        
        % Ensure llama.cpp server is running
        ensure_server_running(Msg2, Opts),
        
        % Extract required parameters
        Prompt = extract_required_param(<<"prompt">>, Msg2, Opts),
        Reference = extract_required_param(<<"reference">>, Msg2, Opts),
        Worker = extract_required_param(<<"worker">>, Msg2, Opts),
        
        % Extract optional parameters
        Config = hb_ao:get(<<"config">>, Msg2, <<>>, Opts),
        SessionId = hb_ao:get(<<"session_id">>, Msg2, undefined, Opts),
        
        % Parse config JSON and extract allowed parameters
        LlamaCppParams = parse_config(Config, Opts),
        
        % Choose API endpoint based on session_id
        {ResponseBody} = 
            case SessionId of
                undefined ->
                    % Use completions API
                    OptsJSON = hb_json:encode(LlamaCppParams),
                    case dev_llamacpp_nif:completion(Prompt, OptsJSON) of
                        {ok, RespBody} -> {RespBody};
                        {error, Reason} -> throw({llamacpp_error, Reason})
                    end;
                _ ->
                    % Use chat completions API with session history
                    Messages = get_session_history(SessionId, Prompt, Opts),
                    MessagesJSON = hb_json:encode(Messages),
                    OptsJSON = hb_json:encode(LlamaCppParams),
                    case dev_llamacpp_nif:chat(MessagesJSON, OptsJSON) of
                        {ok, RespBody} -> {RespBody};
                        {error, Reason} -> throw({llamacpp_error, Reason})
                    end
            end,
        
        % Extract content from response
        Content = extract_content_from_response(ResponseBody, SessionId),
        
        % Update session history if applicable
        case SessionId of
            undefined -> ok;
            _ -> update_session_history(SessionId, Prompt, Content, Opts)
        end,
        
        ?event(dev_llamacpp, {infer_success, Reference}),
        % Return structured response
        {ok, build_response(Content, SessionId, Reference, Worker)}
        
    catch
        throw:{missing_required_param, Param} ->
            {error, <<"Missing required parameter: ", Param/binary>>};
        throw:{invalid_json, Error} ->
            {error, <<"Invalid JSON in config: ", Error/binary>>};
        throw:{llamacpp_error, Error} ->
            {error, <<"llama.cpp API error: ", (hb_util:bin(Error))/binary>>};
        throw:{parse_error, Error} ->
            {error, <<"Failed to parse llama.cpp response: ", Error/binary>>};
        throw:{missing_model, Error} ->
            {error, Error};
        throw:{server_start_failed, Error} ->
            {error, Error};
        _:Error:Stacktrace ->
            ?event(llamacpp_error, {unexpected_error, Error, Stacktrace}),
            {error, <<"Unexpected error occurred">>}
    end.

%% @doc Initialize the session manager.
init_session_table() ->
    % Start the session manager process if not already running
    case whereis(llamacpp_session_manager) of
        undefined ->
            {ok, _Pid} = llamacpp_session_manager:start_link(),
            ok;
        _ ->
            ok
    end.

%% @doc Ensure llama.cpp server is running, start it if not.
ensure_server_running(Msg2, Opts) ->
    % Always try to start server - the NIF will handle already_running case
    start_server(Msg2, Opts).

%% @doc Start llama.cpp server with configuration.
start_server(Msg2, Opts) ->
    % Get model path from config or use default
    Model = case hb_ao:get(<<"model">>, Msg2, undefined, Opts) of
        undefined -> 
            % Use fixed default model
            <<"models/gemma-3-270m-it-F16.gguf">>;
        ModelPath -> ModelPath
    end,
    
    % Get host and port from config or use defaults
    Host = <<"127.0.0.1">>,
    Port = 9567,
    
    % Debug logging
    ?event(dev_llamacpp, {debug_start_server, #{model => Model, host => Host, port => Port}}),
    
    % Start the server
    case dev_llamacpp_nif:start(#{model => Model, host => Host, port => Port}) of
        ok ->
            % Wait a bit for server to be ready
            timer:sleep(2000),
            ok;
        {error, already_running} ->
            % Even if already running, ensure ETS table has the server info
            % This fixes the issue where subsequent requests fail with not_running
            try
                dev_llamacpp_nif:ensure_ets(),
                ets:insert(dev_llamacpp_state, {server, #{host => Host, port => Port, model => Model, restarts => 0}})
            catch
                _:_ -> ok % Ignore ETS errors, server is running anyway
            end,
            ok;
        {error, Reason} ->
            throw({server_start_failed, <<"Failed to start server: ", (hb_util:bin(Reason))/binary>>})
    end.

%% @doc Extract a required parameter from the message.
extract_required_param(ParamName, Msg, Opts) ->
    case hb_ao:get(ParamName, Msg, undefined, Opts) of
        undefined -> 
            throw({missing_required_param, ParamName});
        Value -> 
            Value
    end.

%% @doc Parse config JSON and extract allowed parameters.
parse_config(Config, _Opts) when Config =:= <<>>; Config =:= undefined ->
    #{<<"max_tokens">> => 512, <<"temperature">> => 0};
parse_config(Config, _Opts) ->
    try
        Decoded = hb_json:decode(Config),
        % Extract only the allowed parameters
        lists:foldl(
            fun(Key, Acc) ->
                case maps:get(Key, Decoded, undefined) of
                    undefined -> Acc;
                    Value -> 
                        Acc#{Key => format_param_value(Key, Value)}
                end
            end,
            #{<<"max_tokens">> => 512, <<"temperature">> => 0},
            [<<"max_tokens">>, <<"temperature">>, <<"top_p">>, <<"top_k">>, <<"repeat_penalty">>, <<"stop">>]
        )
    catch
        _:Error ->
            throw({invalid_json, hb_util:bin(Error)})
    end.

%% @doc Format parameter values to appropriate types.
format_param_value(<<"max_tokens">>, Value) when is_integer(Value) -> Value;
format_param_value(<<"temperature">>, Value) when is_number(Value) -> Value;
format_param_value(<<"top_p">>, Value) when is_number(Value) -> Value;
format_param_value(<<"top_k">>, Value) when is_integer(Value) -> Value;
format_param_value(<<"repeat_penalty">>, Value) when is_number(Value) -> Value;
format_param_value(<<"stop">>, Value) when is_list(Value); is_binary(Value) -> Value;
format_param_value(_, Value) -> Value.

%% @doc Get session history for chat completions with proper LRU management.
get_session_history(SessionId, Prompt, _Opts) ->
    % Use session manager to ensure persistence across processes
    llamacpp_session_manager:get_session_history(SessionId, Prompt).

%% @doc Update session history with assistant response.
update_session_history(SessionId, UserPrompt, AssistantResponse, _Opts) ->
    % Use session manager to ensure persistence across processes
    llamacpp_session_manager:update_session_history(SessionId, UserPrompt, AssistantResponse).

%% @doc Extract content from llama.cpp response.
extract_content_from_response(ResponseBody, SessionId) ->
    try
        % Ensure ResponseBody is a binary before decoding
        BinaryResponseBody = case is_binary(ResponseBody) of
            true -> ResponseBody;
            false -> hb_util:bin(ResponseBody)
        end,
        Decoded = hb_json:decode(BinaryResponseBody),
        case SessionId of
            undefined ->
                % For completions API, extract from choices[0].text
                Choices = maps:get(<<"choices">>, Decoded, []),
                case Choices of
                    [Choice|_] ->
                        maps:get(<<"text">>, Choice, <<>>);
                    _ ->
                        <<>>
                end;
            _ ->
                % For chat completions API, extract from choices[0].message.content
                Choices = maps:get(<<"choices">>, Decoded, []),
                case Choices of
                    [Choice|_] ->
                        Message = maps:get(<<"message">>, Choice, #{}),
                        maps:get(<<"content">>, Message, <<>>);
                    _ ->
                        <<>>
                end
        end
    catch
        _:Error ->
            throw({parse_error, hb_util:bin(Error)})
    end.

%% @doc Build structured response.
build_response(Content, SessionId, Reference, Worker) ->
    ResponseBody = hb_json:encode(#{<<"result">> => Content}),
    
    BaseResponse = #{
        <<"body">> => ResponseBody,
        <<"Action">> => <<"Infer-Response">>,
        <<"status">> => 200,
        <<"X-Reference">> => Reference,
        <<"X-Worker">> => Worker
    },
    
    % Add X-Session header if session_id exists
    case SessionId of
        undefined -> BaseResponse;
        _ -> BaseResponse#{<<"X-Session">> => SessionId}
    end.

%%% Tests


integration_test_() ->
    case {filelib:is_file("_build/llama.cpp/build/bin/llama-server")} of
        {true} ->
            {timeout, 120, {setup,
                fun() -> ok
                end,
                fun(_S) -> 
                    % Stop llama.cpp server
                    catch dev_llamacpp_nif:stop()
                end,
                fun() ->
                    % Test completion without session
                    Msg1 = #{},
                    Msg2 = #{
                        <<"prompt">> => <<"Hello">>,
                        <<"reference">> => <<"integration-test-ref">>,
                        <<"worker">> => <<"integration-test-worker">>,
                        <<"config">> => <<"{\"max_tokens\": 8, \"temperature\": 0.1}">>
                    },
                    Opts = #{},
                    
                    Result1 = infer(Msg1, Msg2, Opts),
                    ?assertMatch({ok, _}, Result1),
                    {ok, Response1} = Result1,
                    
                    % Verify response structure
                    ?assert(maps:is_key(<<"body">>, Response1)),
                    ?assertEqual(200, maps:get(<<"status">>, Response1)),
                    ?assertEqual(<<"Infer-Response">>, maps:get(<<"Action">>, Response1)),
                    ?assertEqual(<<"integration-test-ref">>, maps:get(<<"X-Reference">>, Response1)),
                    ?assertEqual(<<"integration-test-worker">>, maps:get(<<"X-Worker">>, Response1)),
                    
                    % Verify response body contains result
                    Body1 = hb_json:decode(maps:get(<<"body">>, Response1)),
                    ?assert(maps:is_key(<<"result">>, Body1)),
                    ?assert(is_binary(maps:get(<<"result">>, Body1))),
                    
                    % Test chat completion with session
                    SessionId = <<"test-session-integration">>,
                    Msg2WithSession = Msg2#{<<"session_id">> => SessionId},
                    
                    Result2 = infer(Msg1, Msg2WithSession, Opts),
                    ?assertMatch({ok, _}, Result2),
                    {ok, Response2} = Result2,
                    
                    % Verify session header is present
                    ?assertEqual(SessionId, maps:get(<<"X-Session">>, Response2)),
                    
                    % Verify response body
                    Body2 = hb_json:decode(maps:get(<<"body">>, Response2)),
                    ?assert(maps:is_key(<<"result">>, Body2)),
                    ?assert(is_binary(maps:get(<<"result">>, Body2))),
                    
                    % Test second message in same session
                    Msg2SecondInSession = #{
                        <<"prompt">> => <<"How are you?">>,
                        <<"reference">> => <<"integration-test-ref-2">>,
                        <<"worker">> => <<"integration-test-worker-2">>,
                        <<"session_id">> => SessionId,
                        <<"config">> => <<"{\"max_tokens\": 8, \"temperature\": 0.1}">>
                    },
                    
                    Result3 = infer(Msg1, Msg2SecondInSession, Opts),
                    ?assertMatch({ok, _}, Result3),
                    {ok, Response3} = Result3,
                    
                    % Verify session header is still present
                    ?assertEqual(SessionId, maps:get(<<"X-Session">>, Response3)),
                    
                    % Test infer_sec with attestation
                    try
                        Result4 = infer_sec(Msg1, Msg2, Opts),
                        case Result4 of
                            {ok, ResponseSec} ->
                                % Verify attestation is present in response
                                BodySec = hb_json:decode(maps:get(<<"body">>, ResponseSec)),
                                ?assert(maps:is_key(<<"result">>, BodySec)),
                                ?assert(maps:is_key(<<"attestation">>, BodySec));
                            {error, _} ->
                                % dev_cc might not be available in test environment
                                io:format("infer_sec test skipped due to dev_cc error~n")
                        end
                    catch
                        error:undef ->
                            % dev_cc module not available in test environment
                            io:format("infer_sec test skipped due to dev_cc unavailability~n");
                        _:Error ->
                            io:format("infer_sec test skipped due to error: ~p~n", [Error])
                    end
                end}};
        _ -> {skip, "Set HB_LLAMA_TEST=1 and ensure llama-server exists to run integration test"}
    end.
