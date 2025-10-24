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
%%%
%%% When session_id is not provided, uses v1/completions API.
%%% When session_id is provided, uses v1/chat/completions API with session history.
%%%
%%% Returns structured response with:
%%% - body: JSON array with result from llama.cpp
%%% - Custom headers: X-Session, X-Reference
%%% - Action: "Infer-Response"
%%% - status: 200
%%%
%%% Note: Session history is managed by llamacpp_session_manager to ensure
%%% persistence across different HyperBEAM processes.
-module(dev_llamacpp).
-export([info/1, chat/3, completion/3, load_model/3, read_model_by_ID/2]).
-export([init_session_table/0]). % For testing
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Maximum number of sessions to keep in memory (LRU)
-define(MAX_SESSIONS, 1000).
-define(DEFAULT_MODEL, <<"models/qwen2.5-14b-instruct-q2_k.gguf">>).

%% @doc Device metadata and exported functions.
info(_Opts) ->
    #{
        exports => [<<"chat">>, <<"completion">>, <<"load_model">>]
    }.

%% @doc Handles completion requests to llama.cpp server.
completion(_Msg1, Msg2, Opts) ->
    try
        % Ensure HTTP client is started for health checks
        application:ensure_all_started(inets),
        
        ensure_server_running(Msg2, Opts),
        
        Prompt = extract_required_param(<<"prompt">>, Msg2, Opts),
        Reference = extract_required_param(<<"reference">>, Msg2, Opts),
        
        Config = hb_ao:get(<<"config">>, Msg2, <<>>, Opts),
        
        LlamaCppParams = parse_config(Config, Opts),
        
        OptsJSON = hb_json:encode(LlamaCppParams),
        {ResponseBody} = case dev_llamacpp_nif:completion(Prompt, OptsJSON) of
            {ok, RespBody} -> {RespBody};
            {error, not_running} -> 
                ?event(dev_llamacpp, {completion_failed_server_not_running, Reference}),
                % Try to restart server once more
                start_server(Msg2, Opts),
                case dev_llamacpp_nif:completion(Prompt, OptsJSON) of
                    {ok, RespBody} -> {RespBody};
                    {error, Reason} -> throw({llamacpp_error, Reason})
                end;
            {error, Reason} -> throw({llamacpp_error, Reason})
        end,
        
        Content = extract_content_from_response(ResponseBody, undefined), % No session Id
        
        ?event(dev_llamacpp, {completion_success, Reference}),
        {ok, build_response(Content, undefined, Reference)} 
        
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
            ?event(llamacpp_error, {completion_unexpected_error, Error, Stacktrace}),
            {error, <<"Unexpected error occurred">>} 
    end.

%% @doc Handles chat requests to llama.cpp server.
chat(_Msg1, Msg2, Opts) ->
    try
        init_session_table(),
        
        ensure_server_running(Msg2, Opts),
        
        Prompt = extract_required_param(<<"prompt">>, Msg2, Opts),
        Reference = extract_required_param(<<"reference">>, Msg2, Opts),
        SessionId = extract_required_param(<<"session_id">>, Msg2, Opts), % session_id is required for chat
        
        Config = hb_ao:get(<<"config">>, Msg2, <<>>, Opts),
        
        LlamaCppParams = parse_config(Config, Opts),
        
        Messages = get_session_history(SessionId, Prompt, Opts),
        MessagesJSON = hb_json:encode(Messages),
        OptsJSON = hb_json:encode(LlamaCppParams),
        ?event(dev_llamacpp, {debug_chat_request, #{session_id => SessionId, reference => Reference, messages => Messages, config => LlamaCppParams}}),
        {ResponseBody} = case dev_llamacpp_nif:chat(MessagesJSON, OptsJSON) of
            {ok, RespBody} -> {RespBody};
            {error, Reason} -> throw({llamacpp_error, Reason})
        end,
        
        Content = extract_content_from_response(ResponseBody, SessionId),
        
        update_session_history(SessionId, Prompt, Content, Opts),
        
        ?event(dev_llamacpp, {chat_success, Reference}),
        {ok, build_response(Content, SessionId, Reference)} 
        
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
            ?event(llamacpp_error, {chat_unexpected_error, Error, Stacktrace}),
            {error, <<"Unexpected error occurred">>} 
    end.

%% @doc Load a new model into the llama.cpp server.
load_model(_Msg1, Msg2, Opts) ->
    try
        Model = hb_ao:get(<<"model">>, Msg2, ?DEFAULT_MODEL, Opts),

        % Check if the model is a URL or a local path
        case read_model_by_ID(Model, Opts) of
            {ok, LocalModelPath} ->
                LoadOpts = #{model => list_to_binary(LocalModelPath)},
                case dev_llamacpp_nif:load_model(LoadOpts) of
                    ok ->
                        timer:sleep(2000),
                        {ok, <<"Model downloaded and loaded successfully">>};
                    {error, Reason} ->
                        throw({load_model_failed, Reason})
                end;
            {error, Reason} ->
                throw({model_download_failed, Reason})
        end
    catch
        throw:{missing_required_param, Param} ->
            {error, <<"Missing required parameter: ", Param/binary>>};
        throw:{load_model_failed, Error} ->
            {error, <<"llama.cpp load_model error: ", (hb_util:bin(Error))/binary>>};
        throw:{model_download_failed, Error} ->
            {error, <<"Model download failed: ", (hb_util:bin(Error))/binary>>};
        _:Error:Stacktrace ->
            ?event(llamacpp_error, {load_model_error, Error, Stacktrace}),
            {error, <<"Unexpected error occurred during model load">>} 
    end.

%% @doc Configure options with model storage settings.
opts(BaseOpts) ->
    %% Allow user to configure model store, or use default
    DefaultModelStore = #{
        <<"store-module">> => hb_store_fs,
        <<"name">> => <<"model-cache">>
    },
    ModelStore = hb_opts:get(model_store, DefaultModelStore, BaseOpts),
    %% Extend base options with model store configuration
    BaseOpts#{
        store => [ModelStore | hb_opts:get(store, [], BaseOpts)]
    }.

%% @doc Download and retrieve a model by Arweave transaction ID.
read_model_by_ID(TxID, Opts) ->
    %% Check if TxID is a valid Arweave transaction ID format
    %% If not, return the original parameter as a local path
    case ?IS_ID(TxID) of
        false ->
            {ok, TxID};
        true ->
            hb_http_server:start_node(#{}),
            ConfiguredOpts = opts(Opts),
            ModelStore = hd(hb_opts:get(store, [], ConfiguredOpts)),
            case hb_cache:read(TxID, ConfiguredOpts) of
                {ok, Message} ->
                    ?event(cache, {successfully_read_message_from_arweave}),
                    DataLink = hb_maps:get(<<"data">>, Message, undefined, ConfiguredOpts),
                    
                    {ok, LoadedData, _LoadedOpts} = hb_cache:ensure_loaded_with_opts(DataLink, ConfiguredOpts),
                    Hashpath = hb_path:hashpath(LoadedData, ConfiguredOpts),
                    DataPath = <<"data/", Hashpath/binary>>,
                    ResolvedPath = hb_store:resolve(ModelStore, DataPath),
                    StoreName = hb_maps:get(<<"name">>, ModelStore, undefined, ConfiguredOpts),
                    ActualFilePath = <<StoreName/binary, "/", ResolvedPath/binary>>,
                    ?event(cache, {actual_file_path, ActualFilePath}),
                    {ok, ActualFilePath};
                not_found ->
                    %% Model transaction ID not found on Arweave network
                    ?event({string, <<"Message not found on Arweave">>}),
                    {error, not_found}
            end
    end.

%% @doc Initialize the session manager.
init_session_table() ->
    case whereis(llamacpp_session_manager) of
        undefined ->
            {ok, _Pid} = llamacpp_session_manager:start_link(),
            ok;
        _ ->
            ok
    end.

%% @doc Check if the llama.cpp server is healthy and responding.
check_server_health() ->
    try
        dev_llamacpp_nif:ensure_ets(),
        case ets:lookup(dev_llamacpp_state, server) of
            [{server, #{host := Host, port := Port}}] ->
                % Try to make a simple health check request
                URL = io_lib:format("http://~s:~p/health", [binary_to_list(Host), Port]),
                case httpc:request(get, {lists:flatten(URL), []}, [{timeout, 5000}], []) of
                    {ok, {{_, 200, _}, _, _}} -> true;
                    _ -> false
                end;
            _ -> false
        end
    catch
        _:_ -> false
    end.

%% @doc Ensure llama.cpp server is running, start it if not.
ensure_server_running(Msg2, Opts) ->
    % Check if server is actually running and restart if needed
    case check_server_health() of
        true -> 
            ok;
        false ->
            ?event(dev_llamacpp, {server_not_healthy_restarting}),
            start_server(Msg2, Opts)
    end.

%% @doc Start llama.cpp server with configuration.
start_server(Msg2, Opts) ->
    Model = case hb_ao:get(<<"model">>, Msg2, undefined, Opts) of
        undefined -> ?DEFAULT_MODEL;
        ModelPath -> ModelPath
    end,

    Host = <<"127.0.0.1">>,
    Port = 9567,
    ?event(dev_llamacpp, {debug_start_server, #{model => Model, host => Host, port => Port}}),
    
    % Ensure ETS table exists before attempting to start server
    dev_llamacpp_nif:ensure_ets(),
    
    case dev_llamacpp_nif:start(#{model => Model, host => Host, port => Port}) of
        ok ->
            % Wait a bit for server to be ready
            timer:sleep(2000),
            % Verify server state is properly stored
            dev_llamacpp_nif:ensure_ets(),
            ets:insert(dev_llamacpp_state, {server, #{host => Host, port => Port, model => Model, restarts => 0}}),
            ?event(dev_llamacpp, {server_started_successfully, #{host => Host, port => Port}}),
            ok;
        {error, already_running} ->
            % Ensure ETS state is consistent even when server is already running
            dev_llamacpp_nif:ensure_ets(),
            ets:insert(dev_llamacpp_state, {server, #{host => Host, port => Port, model => Model, restarts => 0}}),
            ?event(dev_llamacpp, {server_already_running, #{host => Host, port => Port}}),
            ok;
        {error, Reason} ->
            ?event(dev_llamacpp, {server_start_failed, Reason}),
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
build_response(Content, SessionId, Reference) ->
    ResponseBody = hb_json:encode(#{<<"result">> => Content}),
    
    BaseResponse = #{
        <<"body">> => ResponseBody,
        <<"Action">> => <<"Infer-Response">>,
        <<"status">> => 200,
        <<"X-Reference">> => Reference
    },
    
    case SessionId of
        undefined -> BaseResponse;
        _ -> BaseResponse#{<<"X-Session">> => SessionId}
    end.

%%% Tests

% read_model_by_ID_test() ->
%     ModelID = <<"ISrbGzQot05rs_HKC08O_SmkipYQnqgB1yC3mjZZeEo">>,
%     Opts = #{},
%     Result = dev_llamacpp:read_model_by_ID(ModelID, Opts),
%     ?assertMatch({ok, _}, Result),
%     {ok, Path} = Result,
%     ?assert(is_list(Path)).

% load_model_test() ->
%     ModelID = <<"ISrbGzQot05rs_HKC08O_SmkipYQnqgB1yC3mjZZeEo">>,
%     Msg1 = #{},
%     LoadModelMsg2 = #{ 
%         <<"model">> => ModelID,
%         <<"reference">> => <<"load-model-ref">>
%     },
%     Opts = #{},
    
%     Result = dev_llamacpp:load_model(Msg1, LoadModelMsg2, Opts),
%     ?assertMatch({ok, _}, Result),
%     {ok, Response} = Result,
%     ?assertEqual(<<"Model downloaded and loaded successfully">>, Response).

completion_test() ->
    Msg1 = #{},
    Msg2 = #{
        <<"prompt">> => <<"Hello">>,
        <<"reference">> => <<"completion-test-ref">>,
        <<"config">> => <<"{\"max_tokens\": 8, \"temperature\": 0.1}">>
    },
    Opts = #{},
    
    Result = completion(Msg1, Msg2, Opts),
    ?assertMatch({ok, _}, Result),
    {ok, Response} = Result,
    
    ?assert(maps:is_key(<<"body">>, Response)),
    ?assertEqual(200, maps:get(<<"status">>, Response)),
    ?assertEqual(<<"Infer-Response">>, maps:get(<<"Action">>, Response)),
    ?assertEqual(<<"completion-test-ref">>, maps:get(<<"X-Reference">>, Response)),
    
    Body = hb_json:decode(maps:get(<<"body">>, Response)),
    ?assert(maps:is_key(<<"result">>, Body)),
    ?assert(is_binary(maps:get(<<"result">>, Body))).

chat_test() ->
    Msg1 = #{},
    SessionId = <<"test-session-chat">>,
    Msg2 = #{
        <<"prompt">> => <<"Hello">>,
        <<"reference">> => <<"chat-test-ref">>,
        <<"session_id">> => SessionId,
        <<"config">> => <<"{\"max_tokens\": 8, \"temperature\": 0.1}">>
    },
    Opts = #{},
    
    Result = chat(Msg1, Msg2, Opts),
    ?assertMatch({ok, _}, Result),
    {ok, Response} = Result,
    
    ?assertEqual(SessionId, maps:get(<<"X-Session">>, Response)),
    
    Body = hb_json:decode(maps:get(<<"body">>, Response)),
    ?assert(maps:is_key(<<"result">>, Body)),
    ?assert(is_binary(maps:get(<<"result">>, Body))).