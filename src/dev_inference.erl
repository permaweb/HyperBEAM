%%% @doc A device that provides inference capabilities by interfacing with
%%% the HyperBEAM inference server. This device manages the lifecycle of the
%%% inference server and provides OpenAI-compatible API endpoints for
%%% text generation and other inference tasks.
%%%
%%% HTTP Endpoints:
%%% - /~inference@1.0/completions - OpenAI-compatible completions API
%%% - /~inference@1.0/chat - Chat completions API
%%% - /~inference@1.0/health - Server health check
-module(dev_inference).
-export([info/1, completions/3, chat/3, health/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%%% Timeout for inference server status check.
-define(STATUS_TIMEOUT, 100).

%% @doc Device information and exported endpoints.
info(_Opts) ->
    #{
        exports => [<<"completions">>, <<"chat">>, <<"health">>],
        description => <<"HyperBEAM inference device providing OpenAI-compatible API endpoints">>,
        version => <<"1.0">>
    }.

%% @doc OpenAI-compatible completions endpoint.
%% Accessible via HTTP: POST /~inference@1.0/completions
completions(Base, Req, Opts) ->
    handle_inference_request(Base, Req, Opts, <<"/v1/completions">>).

%% @doc OpenAI-compatible chat completions endpoint.
%% Accessible via HTTP: POST /~inference@1.0/chat
chat(Base, Req, Opts) ->
    handle_inference_request(Base, Req, Opts, <<"/v1/chat/completions">>).

%% @doc Health check endpoint.
%% Accessible via HTTP: GET /~inference@1.0/health
health(_Base, _Req, Opts) ->
    case ensure_started(Opts) of
        true ->
            {ok, #{
                <<"status">> => <<"healthy">>,
                <<"server">> => <<"running">>,
                <<"timestamp">> => os:system_time(millisecond)
            }};
        false ->
            {error, #{
                <<"status">> => 503,
                <<"message">> => <<"Inference server not available">>
            }}
    end.

%% @doc Handle inference requests by proxying to the inference server.
handle_inference_request(Base, Req, Opts, InferencePath) ->
    % Ensure inference server is running
    case ensure_started(Opts) of
        true ->
            do_inference_request(Base, Req, Opts, InferencePath);
        false ->
            {error, #{
                <<"status">> => 503,
                <<"message">> => <<"Inference server not available">>
            }}
    end.

%% @doc Execute inference request through relay device.
do_inference_request(Base, Req, Opts, InferencePath) ->
    % Extract request body and headers
    Body = hb_maps:get(<<"body">>, Req, <<"{}">>, Opts),
    Headers = prepare_inference_headers(Req, Opts),

    % Send request to inference server via relay
    Response = do_relay(
        <<"POST">>,
        InferencePath,
        Body,
        Headers,
        Opts#{
            hashpath => ignore,
            cache_control => [<<"no-store">>, <<"no-cache">>]
        }
    ),

    handle_inference_response(Base, Response, Opts).

%% @doc Prepare headers for the inference server request.
prepare_inference_headers(Req, Opts) ->
    % Extract relevant headers from the request
    Headers = hb_maps:without([<<"body">>, <<"path">>, <<"method">>], Req, Opts),

    % Ensure content-type is set appropriately
    DefaultHeaders = #{
        <<"content-type">> => <<"application/json">>,
        <<"accept">> => <<"application/json">>
    },

    maps:merge(DefaultHeaders, Headers).

%% @doc Send request to inference server via relay device.
do_relay(Method, Path, Body, Headers, Opts) ->
    ContentType = hb_maps:get(
        <<"content-type">>,
        Headers,
        <<"application/json">>,
        Opts
    ),

    hb_ao:resolve(
        #{
            <<"device">> => <<"relay@1.0">>,
            <<"content-type">> => ContentType
        },
        Headers#{
            <<"path">> => <<"call">>,
            <<"target">> => <<"payload">>,
            <<"payload">> => Headers#{
                <<"path">> => Path,
                <<"method">> => Method,
                <<"body">> => Body,
                <<"content-type">> => ContentType
            }
        },
        Opts
    ).

%% @doc Handle the response from the inference server.
handle_inference_response(Base, Response, Opts) ->
    case Response of
        {ok, Res} ->
            Body = hb_maps:get(<<"body">>, Res, <<"{}">>, Opts),
            ResponseHeaders = hb_maps:without([<<"body">>], Res, Opts),
            {ok, Base#{
                <<"results">> => #{
                    <<"data">> => Body,
                    <<"headers">> => ResponseHeaders,
                    <<"timestamp">> => os:system_time(millisecond)
                }
            }};
        {error, Error} ->
            {error, #{
                <<"status">> => 500,
                <<"message">> => <<"Inference request failed">>,
                <<"details">> => Error
            }}
    end.

%% @doc Start the inference server process.
start_inference_server(InferenceServerDir, Opts) ->
    % Get configuration options
    ModelPath = hb_opts:get(inference_model_path, "", Opts),
    SGLangHost = hb_opts:get(inference_sglang_host, "127.0.0.1", Opts),
    SGLangPort = hb_opts:get(inference_sglang_port, 30000, Opts),
    ProxyHost = hb_opts:get(inference_proxy_host, "127.0.0.1", Opts),
    ProxyPort = hb_opts:get(inference_proxy_port, 8080, Opts),

    % Convert binary values to strings for port arguments
    ModelPathStr = binary_to_list(iolist_to_binary(ModelPath)),
    SGLangHostStr = binary_to_list(iolist_to_binary(SGLangHost)),
    ProxyHostStr = binary_to_list(iolist_to_binary(ProxyHost)),

    % Verify the inference server script exists
    ScriptPath = filename:join([InferenceServerDir, "hb_inference_server.py"]),
    case filelib:is_file(ScriptPath) of
        false ->
            exit({inference_server_script_not_found, ScriptPath});
        true ->
            ok
    end,

    % Find Python executable
    PythonExe = find_python_executable(),

    % Build the command to start the inference server
    try
        Port = open_port(
            {spawn_executable, PythonExe},
            [
                binary,
                use_stdio,
                stderr_to_stdout,
                {args, [
                    ScriptPath,
                    "--model-path", ModelPathStr,
                    "--sglang-host", SGLangHostStr,
                    "--sglang-port", integer_to_list(SGLangPort),
                    "--proxy-host", ProxyHostStr,
                    "--proxy-port", integer_to_list(ProxyPort)
                ]},
                {env, [
                    {"PYTHONPATH", InferenceServerDir}
                ]}
            ])
        ),
        collect_server_events(Port)
    catch
        error:Reason ->
            exit({inference_server_failed_to_start, Reason})
    end.

%% @doc Ensure the local inference server is live. If not, start it.
ensure_started(Opts) ->
    case is_inference_server_running(Opts) of
        true ->
            true;
        false ->
            {ok, Cwd} = file:get_cwd(),
            InferenceServerDir = determine_inference_server_dir(Cwd, Opts),
            PID = spawn(fun() -> start_inference_server(InferenceServerDir, Opts) end),
            hb_name:register(<<"inference@1.0">>, PID),
            % Wait for the server to start
            hb_util:until(
                fun() ->
                    receive after 2000 -> ok end,
                    is_inference_server_running(Opts)
                end
            ),
            true
    end.

%% @doc Determine the inference server directory based on build mode.
determine_inference_server_dir(Cwd, Opts) ->
    case init:get_argument(mode) of
        {ok, [["embedded"]]} ->
            % We're in release mode
            filename:join([Cwd, "hb_inference"]);
        _ ->
            % We're in development mode - look in the native directory
            DevPath = filename:join([Cwd, "native", "hb_inference"]),
            case filelib:is_dir(DevPath) of
                true -> DevPath;
                false -> filename:join([Cwd, "hb_inference"]) % Fallback
            end
    end.

%% @doc Check if the inference server is running.
is_inference_server_running(Opts) ->
    check_server_status_async(Opts).

%% @doc Asynchronously check the server status to avoid hanging.
check_server_status_async(Opts) ->
    case get(inference_server_pid) of
        undefined ->
            Parent = self(),
            PID = spawn(fun() ->
                Parent ! {ok, self(), check_server_status(Opts)}
            end),
            receive
                {ok, PID, Status} ->
                    put(inference_server_pid, Status),
                    Status
            after ?STATUS_TIMEOUT ->
                erlang:exit(PID, kill),
                false
            end;
        Status -> 
            Status
    end.

%% @doc Check if the inference server is running by requesting its health endpoint.
check_server_status(Opts) ->
    ServerPort = hb_opts:get(inference_proxy_port, 8080, Opts),
    case gen_tcp:connect("localhost", ServerPort, [], 1000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            check_http_health(ServerPort, Opts);
        {error, _Reason} ->
            false
    end.

%% @doc Check the HTTP health endpoint after confirming TCP connectivity.
check_http_health(ServerPort, Opts) ->
    HealthURL = <<"http://localhost:", (hb_util:bin(ServerPort))/binary, "/health">>,
    try hb_http:get(HealthURL, Opts) of
        {ok, Res} ->
            Status = hb_maps:get(<<"status">>, Res, 0, Opts),
            Status =:= 200;
        {error, Res} ->
            Status = hb_maps:get(<<"status">>, Res, 0, Opts),
            Server = hb_maps:get(<<"server">>, Res, <<"">>, Opts),
            case Status of
                404 ->
                    % 404 from Python server means server is running but wrong endpoint
                    binary:match(Server, <<"Python">>) =/= nomatch;
                _ when Status >= 500 ->
                    true;
                _ ->
                    false
            end;
        _Err ->
            false
    catch
        _:_Err ->
            false
    end.

%% @doc Collect events from the server port and log them.
collect_server_events(Port) ->
    collect_server_events(Port, <<>>).

collect_server_events(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_server_events(Port, <<Acc/binary, Data/binary>>);
        stop ->
            port_close(Port),
            ok
    end.

%% @doc Find a working Python executable.
find_python_executable() ->
    % Try common Python executable names in order of preference
    Candidates = ["python3", "/usr/bin/python3", "/usr/local/bin/python3", "python"],
    find_working_python(Candidates).

%% @doc Find the first working Python executable from a list of candidates.
find_working_python([]) ->
    error(no_python_executable_found);
find_working_python([Candidate | Rest]) ->
    case os:find_executable(Candidate) of
        false ->
            find_working_python(Rest);
        Path ->
            Path
    end.

%%% Tests
-ifdef(ENABLE_INFERENCE).

inference_completions_test_() ->
    {timeout, 300, fun test_inference_completions/0}.

test_inference_completions() ->
    application:ensure_all_started(hb),
    Opts = #{
        inference_model_path => "/path/to/model", % Mock path for testing
        inference_proxy_port => 8080
    },

    % Test health endpoint
    {ok, HealthResult} = health(#{}, #{}, Opts),
    ?assertMatch(#{<<"status">> := <<"healthy">>}, HealthResult),

    % Test completions endpoint
    CompletionReq = #{
        <<"body">> => <<"{\"model\":\"test\",\"prompt\":\"Hello world\",\"max_tokens\":100}">>,
        <<"content-type">> => <<"application/json">>
    },

    BaseMsg = #{},
    {ok, Result} = completions(BaseMsg, CompletionReq, Opts),
    ?assertMatch(#{<<"results">> := #{<<"data">> := _}}, Result).

inference_chat_test_() ->
    {timeout, 300, fun test_inference_chat/0}.

test_inference_chat() ->
    application:ensure_all_started(hb),
    Opts = #{
        inference_proxy_port => 8080
    },

    % Test chat endpoint
    ChatReq = #{
        <<"body">> => <<"{\"model\":\"test\",\"messages\":[{\"role\":\"user\",\"content\":\"Hello\"}]}">>,
        <<"content-type">> => <<"application/json">>
    },

    BaseMsg = #{},
    {ok, Result} = chat(BaseMsg, ChatReq, Opts),
    ?assertMatch(#{<<"results">> := #{<<"data">> := _}}, Result).

-endif.