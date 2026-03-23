%%% @doc Inference device with OpenAI-compatible API.
%%% This module provides an interface to a local Python-based inference server.
-module(dev_inference).
-export([info/1, completions/3, chat/3, models/3, health/3, v1/3, stop/0]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(SERVER_PORT, "8080").

%% Device API

%% @doc Return information about the device.
info(_Opts) ->
    #{
        exports => [<<"completions">>, <<"chat">>, <<"models">>, <<"health">>, <<"v1">>],
        description => <<"Inference device with OpenAI-compatible API">>,
        version => <<"1.0">>
    }.

%% @doc Stop the inference server process.
stop() ->
    case whereis(inference_server) of
        undefined -> ok;
        Pid -> 
            Pid ! stop,
            unregister(inference_server),
            ok
    end.

%% @doc Handle completion requests.
completions(Base, Req, Opts) ->
    Path = case hb_ao:get(<<"chat-mode">>, Base, false, Opts) of
        true -> <<"/v1/chat/completions">>;
        false -> <<"/v1/completions">>
    end,
    do_inference_request(Base, Req, Opts, Path).

%% @doc Handle chat completion requests.
chat(Base, Req, Opts) ->
    {ok, hb_util:deep_merge(
        Base, 
        Req#{
            <<"device">> => <<"inference@1.0">>,
            <<"chat-mode">> => true
        }, 
        Opts
    )}.

%% @doc Handle models request.
models(_Base, _Req, _Opts) ->
    Body = <<"{\"object\":\"list\",\"data\":[{\"id\":\"google/gemma-3-27b-it\",\"object\":\"model\",\"created\":1766123915,\"owned_by\":\"sglang\",\"root\":\"google/gemma-3-27b-it\",\"max_model_len\":16384}]}">>,
    {ok, #{
        <<"status">> => 200,
        <<"content-type">> => <<"application/json">>,
        <<"body">> => Body
    }}.

%% @doc Handle v1 API requests.
v1(Base, Req, Opts) ->
    {ok, hb_util:deep_merge(
        Base, 
        Req#{
            <<"device">> => <<"inference@1.0">>
        }, 
        Opts
    )}.

%% @doc Check the health of the inference server.
health(_Base, _Req, Opts) ->
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
    Params = extract_inference_params(Req, Opts),
    IsStream = case maps:get(<<"stream">>, Params, false) of
        true -> true;
        <<"true">> -> true;
        _ -> false
    end,
    case IsStream of
        true ->
            Body = hb_json:encode(Params),
            #{
                <<"stream_generator">> => fun(Sender) -> 
                    stream_from_backend(Sender, <<"POST">>, Path, Body, Opts) 
                end
            };
        false ->
            Body = prepare_request_body(Req, Opts),
            Response = relay_to_backend(<<"POST">>, Path, Body, Opts),
            
            case should_include_attestation(Req, Opts) of
                true -> add_attestation(Response, Req, Opts);
                false -> format_response(Response, Req, Opts)
            end
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
        <<"repetition_penalty">>, <<"length_penalty">>, <<"early_stopping">>,
        <<"tools">>, <<"tool_choice">>
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

format_response({ok, Res}, Req, Opts) ->
    Body = hb_ao:get(<<"body">>, Res, Opts),
    ?event(push, {format_response, Req, hb_ao:get(<<"type">>, Req)}),
    %% Check if this is being called for scheduling (aos Send with resolve)
    %% vs direct HTTP call. dev_push:maybe_evaluate_message sets force_message => true
    case hb_ao:get(<<"type">>, Req) of
        <<"Message">> ->
            %% For scheduling: return only valid AO message fields (ANS-104 compatible)
            {ok, #{
                <<"data">> => Body,
                <<"action">> => <<"Inference-Response">>
            }};
        _ ->
            %% For direct HTTP: return full HTTP response format
            {ok, #{
                <<"status">> => hb_ao:get(<<"status">>, Res, 200, Opts),
                <<"content-type">> => hb_ao:get(<<"content-type">>, Res, <<"application/json">>, Opts),
                <<"body">> => Body
            }}
    end;
format_response({error, Error}, Req, Opts) ->
    case hb_ao:get(<<"type">>, Req) of
        <<"Message">> ->
            %% For scheduling: return AO message format
            {error, #{
                <<"data">> => hb_util:bin(Error),
                <<"action">> => <<"Inference-Error">>
            }};
        _ ->
            %% For direct HTTP: return HTTP error format
            {error, #{
                <<"status">> => 500,
                <<"message">> => <<"Request failed">>,
                <<"body">> => hb_util:bin(Error)
            }}
    end.

add_attestation({ok, Res}, Req, Opts) ->
    MergedData = #{
        <<"request">> => hb_private:reset(Req),
        <<"response">> => hb_private:reset(Res),
        <<"timestamp">> => os:system_time(millisecond),
        <<"nonce">> => hb_util:to_hex(crypto:strong_rand_bytes(32))
    },
    NonceHex = hb_util:to_hex(hb_crypto:sha256(hb_json:encode(MergedData))),
    
    %% Generate attestation - result contains evidences, claims, eat, verified
    AttestationResult = case dev_sev_gpu:generate(#{}, #{nonce => NonceHex}, Opts) of
        {ok, ResultJSON} -> 
            case hb_json:decode(ResultJSON) of
                Map when is_map(Map) -> Map;
                _ -> #{}
            end;
        _ -> #{}
    end,
    
    %% Merge attestation result with raw and nonce at same level
    AttestationData = maps:merge(AttestationResult, #{
        <<"raw">> => hb_json:encode(MergedData),
        <<"nonce">> => NonceHex
    }),
    
    ResponseBody = hb_ao:get(<<"body">>, Res, Opts),
    DecodedBody = hb_json:decode(ResponseBody),
    EnhancedBody = hb_json:encode(DecodedBody#{
        <<"attestation">> => AttestationData,
        <<"resolved_model">> => hb_opts:get(inference_opts, #{}, Opts)
    }),
    
    %% Check context: scheduling (aos) vs direct HTTP
    case hb_ao:get(<<"type">>, Req) of
        <<"Message">> ->
            %% For scheduling: return only valid AO message fields (ANS-104 compatible)
            {ok, #{
                <<"data">> => EnhancedBody,
                <<"action">> => <<"Inference-Response">>
            }};
        _ ->
            %% For direct HTTP: return full HTTP response format
            {ok, #{
                <<"status">> => hb_ao:get(<<"status">>, Res, 200, Opts),
                <<"content-type">> => <<"application/json">>,
                <<"body">> => EnhancedBody
            }}
    end;
add_attestation({error, Error}, Req, Opts) ->
    format_response({error, Error}, Req, Opts).

stream_from_backend(Sender, Method, Path, Body, _Opts) ->
    {ok, ConnPid} = gun:open("localhost", list_to_integer(?SERVER_PORT)),
    {ok, _Protocol} = gun:await_up(ConnPid),
    StreamRef = gun:request(ConnPid, Method, Path, [{<<"content-type">>, <<"application/json">>}], Body),
    receive_stream(ConnPid, StreamRef, Sender).

receive_stream(ConnPid, StreamRef, Sender) ->
    receive
        {gun_response, ConnPid, StreamRef, nofin, _Status, _Headers} ->
            receive_stream(ConnPid, StreamRef, Sender);
        {gun_response, ConnPid, StreamRef, fin, _Status, _Headers} ->
            ok;
        {gun_data, ConnPid, StreamRef, nofin, Data} ->
            Sender(Data),
            receive_stream(ConnPid, StreamRef, Sender);
        {gun_data, ConnPid, StreamRef, fin, Data} ->
            Sender(Data);
        {gun_error, ConnPid, StreamRef, Reason} ->
            ?event(inference, {stream_error, Reason}),
            ok;
        {gun_down, ConnPid, _Protocol, _Reason, _KilledStreams} ->
             ok;
        Other ->
            ?event(inference, {unexpected_stream_msg, Other}),
            receive_stream(ConnPid, StreamRef, Sender)
    after 30000 ->
        ok
    end.
