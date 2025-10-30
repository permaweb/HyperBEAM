-module(dev_inference).
-export([info/1, completions/3, chat/3, health/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

-define(SERVER_PORT, "8080").

%% Device API

info(_Opts) ->
    #{
        exports => [<<"completions">>, <<"chat">>, <<"health">>],
        description => <<"Inference device with OpenAI-compatible API">>,
        version => <<"1.0">>
    }.

completions(Base, Req, Opts) ->
    ensure_started(Opts),
    Path = case hb_ao:get(<<"chat-mode">>, Base, false, Opts) of
        true -> <<"/v1/chat/completions">>;
        false -> <<"/v1/completions">>
    end,
    do_inference_request(Base, Req, Opts, Path).

chat(Base, Req, Opts) ->
    {ok, hb_util:deep_merge(
        Base, 
        Req#{
            <<"device">> => <<"inference@1.0">>,
            <<"chat-mode">> => true
        }, 
        Opts
    )}.

health(_Base, _Req, Opts) ->
    ensure_started(Opts),
    forward_health_check(Opts).


%% Internal functions

forward_health_check(Opts) ->
    Peer = iolist_to_binary(["http://localhost:", ?SERVER_PORT]),
    
    case hb_http_client:request(
        #{
            method => <<"GET">>,
            peer => Peer,
            path => <<"/health">>,
            headers => #{},
            body => <<>>
        },
        Opts
    ) of
        {ok, Status, _Headers, Body} when Status >= 200 andalso Status < 300 ->
            {ok, #{
                <<"status">> => <<"healthy">>,
                <<"body">> => Body,
                <<"timestamp">> => os:system_time(millisecond)
            }};
        {ok, Status, _Headers, Body} ->
            {error, #{
                <<"status">> => Status,
                <<"message">> => <<"Backend unhealthy">>,
                <<"body">> => Body
            }};
        {error, Details} ->
            {error, #{
                <<"status">> => 503,
                <<"message">> => hb_util:bin(Details)
            }}
    end.

do_inference_request(_Base, Req, Opts, Path) ->
    Body = prepare_request_body(Req, Opts),
    Response = relay_to_backend(<<"POST">>, Path, Body, Opts),
    
    case should_include_attestation(Req, Opts) of
        true -> add_attestation(Response, Req, Opts);
        false -> format_response(Response, Opts)
    end.

prepare_request_body(Req, Opts) ->
    case hb_ao:get(<<"body">>, Req, not_found, Opts) of
        not_found -> hb_json:encode(extract_inference_params(Req, Opts));
        Body when is_binary(Body) -> Body;
        Body when is_map(Body) -> hb_json:encode(Body)
    end.

should_include_attestation(Req, Opts) ->
    case hb_ao:get(<<"tee">>, Req, false, Opts) of
        <<"true">> -> true;
        true -> true;
        _ -> false
    end.

extract_inference_params(Req, Opts) ->
    ParamKeys = [
        <<"model">>, <<"prompt">>, <<"messages">>, <<"max_tokens">>,
        <<"temperature">>, <<"top_p">>, <<"n">>, <<"stream">>,
        <<"stop">>, <<"presence_penalty">>, <<"frequency_penalty">>,
        <<"logit_bias">>, <<"user">>, <<"seed">>, <<"top_k">>,
        <<"repetition_penalty">>, <<"length_penalty">>, <<"early_stopping">>
    ],
    Params = lists:foldl(
        fun(Key, Acc) ->
            case hb_ao:get(Key, Req, not_found, Opts) of
                not_found -> Acc;
                Value -> Acc#{Key => Value}
            end
        end,
        #{},
        ParamKeys
    ),
    hb_cache:ensure_all_loaded(Params, Opts).

relay_to_backend(Method, Path, Body, Opts) ->
    hb_ao:resolve(
        #{<<"device">> => <<"relay@1.0">>, <<"content-type">> => <<"application/json">>},
        #{
            <<"path">> => <<"call">>,
            <<"target">> => <<"payload">>,
            <<"payload">> => #{
                <<"path">> => Path,
                <<"method">> => Method,
                <<"body">> => Body,
                <<"content-type">> => <<"application/json">>
            }
        },
        Opts#{hashpath => ignore, cache_control => [<<"no-store">>, <<"no-cache">>]}
    ).

format_response({ok, Res}, Opts) ->
    {ok, #{
        <<"status">> => hb_ao:get(<<"status">>, Res, 200, Opts),
        <<"content-type">> => hb_ao:get(<<"content-type">>, Res, <<"application/json">>, Opts),
        <<"body">> => hb_ao:get(<<"body">>, Res, Opts)
    }};
format_response({error, Error}, _Opts) ->
    {error, #{
        <<"status">> => 500,
        <<"message">> => <<"Request failed">>,
        <<"details">> => hb_util:bin(Error)
    }}.

add_attestation({ok, Res}, Req, Opts) ->
    MergedData = #{
        <<"request">> => hb_private:reset(Req),
        <<"response">> => hb_private:reset(Res),
        <<"timestamp">> => os:system_time(millisecond),
        <<"nonce">> => hb_util:to_hex(crypto:strong_rand_bytes(32))
    },
    NonceHex = hb_util:to_hex(hb_crypto:sha256(hb_json:encode(MergedData))),
    
    AttestationToken = case dev_sev_gpu:generate(#{}, #{nonce => NonceHex}, Opts) of
        {ok, Token} -> Token;
        _ -> null
    end,
    
    ResponseBody = hb_ao:get(<<"body">>, Res, Opts),
    DecodedBody = hb_json:decode(ResponseBody),
    EnhancedBody = hb_json:encode(DecodedBody#{
        <<"attestation">> => #{
            <<"raw">> => hb_json:encode(MergedData),
            <<"nonce">> => NonceHex,
            <<"token">> => AttestationToken
        },
        <<"resolved_model">> => hb_opts:get(inference_opts, #{}, Opts)
    }),
    
    {ok, #{
        <<"status">> => hb_ao:get(<<"status">>, Res, 200, Opts),
        <<"content-type">> => <<"application/json">>,
        <<"body">> => EnhancedBody
    }};
add_attestation({error, Error}, _Req, Opts) ->
    format_response({error, Error}, Opts).

%% Server lifecycle management

ensure_started(Opts) ->
    case hb_name:lookup(<<"inference-server@1.0">>) of
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> ok;
                false ->
                    hb_name:unregister(<<"inference-server@1.0">>),
                    start_server(Opts)
            end;
        _ ->
            start_server(Opts)
    end.

start_server(Opts) ->
    {ok, Cwd} = file:get_cwd(),
    ServerDir = determine_server_dir(Cwd),
    Pid = spawn_link(fun() -> run_server(ServerDir, Opts) end),
    hb_name:register(<<"inference-server@1.0">>, Pid),
    ok.

run_server(ServerDir, Opts) ->
    ModelPath = maps:get(<<"model_name">>, hb_opts:get(inference_opts, #{}, Opts)),
    UvExe = find_uv_executable(),
    LaunchScript = filename:join([ServerDir, "launch-monitored.sh"]),
    
    Port = open_port(
        {spawn_executable, LaunchScript},
        [binary, use_stdio, stderr_to_stdout, exit_status, {cd, ServerDir},
         {args, [UvExe, "run", "deterministic-inference-server",
                 "--model-path", ModelPath, 
                 "--proxy-port", ?SERVER_PORT]}]
    ),
    collect_server_events(Port).

determine_server_dir(Cwd) ->
    case init:get_argument(mode) of
        {ok, [["embedded"]]} -> 
            filename:join([Cwd, "deterministic-inference"]);
        _ ->
            DevPath = filename:join([Cwd, "_build", "deterministic-inference"]),
            case filelib:is_dir(DevPath) of
                true -> DevPath;
                false -> filename:join([Cwd, "deterministic-inference"])
            end
    end.

collect_server_events(Port) ->
    collect_server_events(Port, <<>>).

collect_server_events(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            NewAcc = log_server_output(<<Acc/binary, Data/binary>>),
            collect_server_events(Port, NewAcc);
        {Port, {exit_status, _Status}} ->
            ok
    end.

log_server_output(Binary) ->
    Lines = binary:split(Binary, <<"\n">>, [global]),
    log_lines(Lines).

log_lines([Remaining]) -> 
    Remaining;
log_lines([Line | Rest]) ->
    ?event(inference, {server_logged, {string, Line}}),
    log_lines(Rest).

find_uv_executable() ->
    HomePath = os:getenv("HOME"),
    Candidates = [
        "uv",
        filename:join([HomePath, ".cargo", "bin", "uv"]),
        filename:join([HomePath, ".local", "bin", "uv"]),
        "/usr/local/bin/uv"
    ],
    case lists:search(fun(C) -> os:find_executable(C) =/= false end, Candidates) of
        {value, Found} -> os:find_executable(Found);
        false -> error(uv_executable_not_found)
    end.
