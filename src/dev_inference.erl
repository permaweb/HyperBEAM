-module(dev_inference).
-export([info/1, completions/3, chat/3, health/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

-define(STATUS_TIMEOUT, 100).

info(_Opts) ->
    #{
        exports => [<<"completions">>, <<"chat">>, <<"health">>],
        description => <<"Inference device with OpenAI-compatible API">>,
        version => <<"1.0">>
    }.

completions(Base, Req, Opts) ->
    handle_inference_request(Base, Req, Opts, <<"/v1/completions">>).

chat(Base, Req, Opts) ->
    handle_inference_request(Base, Req, Opts, <<"/v1/chat/completions">>).
    
health(_Base, _Req, Opts) ->
    case ensure_started(Opts) of
        true ->
            {ok, #{
                <<"server">> => <<"running">>,
                <<"timestamp">> => os:system_time(millisecond)
            }};
        false ->
            {error, #{<<"status">> => 503, <<"message">> => <<"Server not available">>}}
    end.

handle_inference_request(Base, Req, Opts, InferencePath) ->
    case ensure_started(Opts) of
        true -> do_inference_request(Base, Req, Opts, InferencePath);
        false -> {error, #{<<"status">> => 503, <<"message">> => <<"Server not available">>}}
    end.

do_inference_request(_Base, Req, Opts, InferencePath) ->
    Body = case hb_ao:get(<<"body">>, Req, not_found, Opts) of
        not_found -> hb_json:encode(extract_inference_params(Req, Opts));
        ExistingBody when is_binary(ExistingBody) -> ExistingBody;
        ExistingBody when is_map(ExistingBody) -> hb_json:encode(ExistingBody)
    end,
    Headers = prepare_inference_headers(Req, Opts),
    Response = do_relay(<<"POST">>, InferencePath, Body, Headers, 
        Opts#{hashpath => ignore, cache_control => [<<"no-store">>, <<"no-cache">>]}),
    handle_inference_response(Response, Opts).

extract_inference_params(Req, Opts) ->
    ParamKeys = [
        <<"model">>, <<"prompt">>, <<"messages">>, <<"max_tokens">>,
        <<"temperature">>, <<"top_p">>, <<"n">>, <<"stream">>,
        <<"stop">>, <<"presence_penalty">>, <<"frequency_penalty">>,
        <<"logit_bias">>, <<"user">>, <<"seed">>, <<"top_k">>,
        <<"repetition_penalty">>, <<"length_penalty">>, <<"early_stopping">>
    ],
    lists:foldl(
        fun(Key, Acc) ->
            case hb_ao:get(Key, Req, not_found, Opts) of
                not_found -> Acc;
                Value -> Acc#{Key => Value}
            end
        end,
        #{},
        ParamKeys
    ).

prepare_inference_headers(Req, Opts) ->
    Headers = hb_maps:without([<<"body">>, <<"path">>, <<"method">>], Req, Opts),
    DefaultHeaders = #{
        <<"content-type">> => <<"application/json">>,
        <<"accept">> => <<"application/json">>
    },
    maps:merge(DefaultHeaders, Headers).

do_relay(Method, Path, Body, Headers, Opts) ->
    ContentType = hb_maps:get(<<"content-type">>, Headers, <<"application/json">>, Opts),
    ProxyHost = hb_opts:get(inference_proxy_host, "127.0.0.1", Opts),
    ProxyPort = hb_opts:get(inference_proxy_port, 8080, Opts),
    ProxyHostBin = iolist_to_binary(ProxyHost),
    ProxyPortBin = integer_to_binary(ProxyPort),
    PeerURL = <<"http://", ProxyHostBin/binary, ":", ProxyPortBin/binary>>,
    hb_ao:resolve(
        #{<<"device">> => <<"relay@1.0">>, <<"content-type">> => ContentType},
        Headers#{
            <<"path">> => <<"call">>,
            <<"target">> => <<"payload">>,
            <<"payload">> => Headers#{
                <<"path">> => Path,
                <<"method">> => Method,
                <<"body">> => Body,
                <<"content-type">> => ContentType,
                <<"peer">> => PeerURL
            }
        },
        Opts
    ).

handle_inference_response(Response, Opts) ->
    case Response of
        {ok, Res} ->
            {ok, #{
                <<"status">> => hb_ao:get(<<"status">>, Res, 200, Opts),
                <<"content-type">> => hb_ao:get(<<"content-type">>, Res, <<"application/json">>, Opts),
                <<"body">> => hb_ao:get(<<"body">>, Res, Opts)
            }};
        {error, Error} when is_map(Error) ->
            {error, #{
                <<"status">> => hb_ao:get(<<"status">>, Error, 500, Opts),
                <<"message">> => hb_ao:get(<<"message">>, Error, <<"Request failed">>, Opts),
                <<"details">> => hb_ao:get(<<"body">>, Error, <<"Unknown error">>, Opts)
            }};
        {error, Error} ->
            {error, #{
                <<"status">> => 500,
                <<"message">> => <<"Request failed">>,
                <<"details">> => hb_util:bin(Error)
            }}
    end.

start_inference_server(InferenceServerDir, Opts) ->
    ModelPath = hb_util:list(hb_opts:get(inference_model_path, "", Opts)),
    ProxyPort = integer_to_list(hb_opts:get(inference_proxy_port, 8080, Opts)),
    BackendPort = integer_to_list(hb_opts:get(inference_backend_port, 30000, Opts)),
    UvExe = find_uv_executable(),
    LaunchScript = filename:join([InferenceServerDir, "launch-monitored.sh"]),
    Port = open_port(
        {spawn_executable, LaunchScript},
        [binary, use_stdio, stderr_to_stdout, exit_status, {cd, InferenceServerDir},
         {args, [UvExe, "run", "deterministic-inference-server",
                 "--model-path", ModelPath, "--proxy-port", ProxyPort,
                 "--backend-port", BackendPort]}]
    ),
    collect_server_events(Port).

ensure_started(Opts) ->
    {ok, Cwd} = file:get_cwd(),
    InferenceServerDir = determine_inference_server_dir(Cwd),
    IsRunning = is_inference_server_running(Opts),
    InferenceProc = is_pid(hb_name:lookup(<<"inference-server@1.0">>)),
    case IsRunning orelse InferenceProc of
        true -> true;
        false ->
            PID = spawn(fun() -> start_inference_server(InferenceServerDir, Opts) end),
            hb_name:register(<<"inference-server@1.0">>, PID),
            hb_util:until(fun() ->
                receive after 2000 -> ok end,
                is_inference_server_running(Opts)
            end),
            true
    end.

determine_inference_server_dir(Cwd) ->
    case init:get_argument(mode) of
        {ok, [["embedded"]]} -> filename:join([Cwd, "deterministic-inference"]);
        _ ->
            DevPath = filename:join([Cwd, "_build", "deterministic-inference"]),
            case filelib:is_dir(DevPath) of
                true -> DevPath;
                false -> filename:join([Cwd, "deterministic-inference"])
            end
    end.

is_inference_server_running(Opts) ->
    case get(inference_server_status) of
        undefined ->
            Parent = self(),
            PID = spawn(fun() -> Parent ! {ok, self(), check_health(Opts)} end),
            receive
                {ok, PID, Status} ->
                    put(inference_server_status, Status),
                    Status
            after ?STATUS_TIMEOUT ->
                erlang:exit(PID, kill),
                false
            end;
        _ -> true
    end.

check_health(Opts) ->
    ServerPort = integer_to_binary(hb_opts:get(inference_proxy_port, 8080, Opts)),
    try hb_http:get(<<"http://localhost:", ServerPort/binary, "/health">>, Opts) of
        {ok, _} -> true;
        _ -> false
    catch
        _:_ -> false
    end.

collect_server_events(Port) ->
    collect_server_events(Port, <<>>).

collect_server_events(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_server_events(Port, log_server_events(<<Acc/binary, Data/binary>>));
        stop ->
            port_close(Port),
            ok
    end.

log_server_events(Bin) when is_binary(Bin) ->
    log_server_events(binary:split(Bin, <<"\n">>, [global]));
log_server_events([Remaining]) -> Remaining;
log_server_events([Line | Rest]) ->
    ?event(inference, {server_logged, {string, Line}}),
    log_server_events(Rest).

find_uv_executable() ->
    HomePath = os:getenv("HOME"),
    Candidates = ["uv", filename:join([HomePath, ".cargo", "bin", "uv"]),
                  filename:join([HomePath, ".local", "bin", "uv"]), "/usr/local/bin/uv"],
    case lists:search(fun(C) -> os:find_executable(C) =/= false end, Candidates) of
        {value, Found} -> os:find_executable(Found);
        false -> error(uv_executable_not_found)
    end.

-ifdef(ENABLE_INFERENCE).

inference_test_() ->
    {timeout, 300, fun test_inference/0}.

test_inference() ->
    application:ensure_all_started(hb),
    Opts = #{inference_model_path => "/path/to/model", inference_proxy_port => 8080},
    {ok, HealthResult} = health(#{}, #{}, Opts),
    ?assertMatch(#{<<"server">> := <<"running">>}, HealthResult).

-endif.