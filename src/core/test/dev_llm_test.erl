%%% @doc Tests for dev_llm@1.0 — OpenAI-compatible LLM proxy.
-module(dev_llm_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Ensure dev_llm is loaded (preloaded store not in test code path)
ensure_dev_llm() ->
    case code:is_loaded(dev_llm) of
        {file, _} -> ok;
        false ->
            case compile:file("src/preloaded/llm/dev_llm.erl", [binary, {i, "include"}, {i, "src/core/include"}]) of
                {ok, dev_llm, Bin} -> code:load_binary(dev_llm, "dev_llm.erl", Bin);
                {ok, dev_llm, Bin, _Warn} -> code:load_binary(dev_llm, "dev_llm.erl", Bin);
                Error -> Error
            end
    end.

%% Unit helpers (no network)
resolve_endpoint_test() ->
    ensure_dev_llm(),
    ?assertEqual(<<"http://spark-1b7b.local:8888/v1/chat/completions">>, dev_llm:resolve_endpoint(#{}, #{}, #{}, chat)).
resolve_endpoint_embed_default_test() ->
    ensure_dev_llm(),
    ?assertEqual(<<"http://spark-1b7b.local:8888/v1/embeddings">>, dev_llm:resolve_endpoint(#{}, #{}, #{}, embed)).
resolve_endpoint_override_test() ->
    ensure_dev_llm(),
    ?assertEqual(<<"http://localhost:8000/v1/chat/completions">>, dev_llm:resolve_endpoint(#{}, #{<<"endpoint">> => <<"http://localhost:8000/v1/chat/completions">>}, #{}, chat)).
resolve_endpoint_llm_endpoint_test() ->
    ensure_dev_llm(),
    ?assertEqual(<<"http://spark-1b7b.local:8888/v1/embeddings">>, dev_llm:resolve_endpoint(#{<<"llm-endpoint">> => <<"http://spark-1b7b.local:8888/v1/chat/completions">>}, #{}, #{}, embed)).
resolve_endpoint_embed_endpoint_override_test() ->
    ensure_dev_llm(),
    ?assertEqual(<<"http://custom:9999/embed">>, dev_llm:resolve_endpoint(#{}, #{<<"embed-endpoint">> => <<"http://custom:9999/embed">>}, #{}, embed)).

build_chat_body_prompt_test() ->
    ensure_dev_llm(),
    Body = dev_llm:build_chat_body(#{<<"prompt">> => <<"hello">>}, <<"llama3.2">>, false, #{}),
    Decoded = hb_json:decode(Body),
    ?assertEqual(<<"llama3.2">>, maps:get(<<"model">>, Decoded)),
    ?assertEqual([#{<<"role">> => <<"user">>, <<"content">> => <<"hello">>}], maps:get(<<"messages">>, Decoded)),
    ?assertEqual(false, maps:get(<<"stream">>, Decoded)).
build_chat_body_messages_test() ->
    ensure_dev_llm(),
    Msgs = [#{<<"role">> => <<"user">>, <<"content">> => <<"hi">>}],
    Body = dev_llm:build_chat_body(#{<<"messages">> => Msgs}, <<"m">>, false, #{}),
    Decoded = hb_json:decode(Body),
    ?assertEqual(Msgs, maps:get(<<"messages">>, Decoded)).
build_chat_body_data_test() ->
    ensure_dev_llm(),
    Body = dev_llm:build_chat_body(#{<<"data">> => <<"hello data">>}, <<"m">>, false, #{}),
    Decoded = hb_json:decode(Body),
    ?assertEqual([#{<<"role">> => <<"user">>, <<"content">> => <<"hello data">>}], maps:get(<<"messages">>, Decoded)).
is_stream_true_test() ->
    ensure_dev_llm(),
    ?assertEqual(true, dev_llm:is_stream(#{<<"stream">> => <<"true">>}, #{})),
    ?assertEqual(true, dev_llm:is_stream(#{<<"Stream">> => <<"true">>}, #{})),
    ?assertEqual(false, dev_llm:is_stream(#{}, #{})).
extract_content_test() ->
    ensure_dev_llm(),
    M = #{<<"choices">> => [#{<<"message">> => #{<<"content">> => <<"hi">>}}]},
    ?assertEqual(<<"hi">>, dev_llm:extract_content(M)),
    ?assertEqual(<<"hi">>, dev_llm:extract_content(#{<<"content">> => <<"hi">>})),
    ?assertEqual(<<>>, dev_llm:extract_content(#{<<"foo">> => 1})).

%% Integration with mock Ollama (gen_tcp)
mock_integration_test_() ->
    {timeout, 30, fun mock_integration/0}.

mock_integration() ->
    {ok,_}=application:ensure_all_started(prometheus),
    {ok,_}=application:ensure_all_started(hb),
    Port = 54329,
    Pid = spawn(fun() -> mock_server(Port) end),
    timer:sleep(200),
    Endpoint = list_to_binary(io_lib:format("http://localhost:~p/v1/chat/completions", [Port])),
    EmbedEndpoint = list_to_binary(io_lib:format("http://localhost:~p/v1/embeddings", [Port])),
    % chat non-stream
    {ok, #{<<"body">> := Body1}} = dev_llm:chat(#{}, #{<<"prompt">> => <<"hello">>, <<"endpoint">> => Endpoint, <<"model">> => <<"test-model">>}, #{}),
    Decoded1 = hb_json:decode(Body1),
    ?assertMatch(#{<<"choices">> := [#{<<"message">> := #{<<"content">> := <<"Hello from mock LLM">>}}|_]}, Decoded1),
    % generate extracts content
    {ok, #{<<"content">> := Content2}} = dev_llm:generate(#{}, #{<<"prompt">> => <<"hello">>, <<"endpoint">> => Endpoint}, #{}),
    ?assertEqual(<<"Hello from mock LLM">>, Content2),
    % embed
    {ok, #{<<"embedding">> := Emb, <<"embeddings">> := Embs}} = dev_llm:embed(#{}, #{<<"input">> => <<"hello world">>, <<"embed-endpoint">> => EmbedEndpoint}, #{}),
    ?assertEqual([0.1,0.2,0.3], Emb),
    ?assertEqual([[0.1,0.2,0.3]], Embs),
    % stream -> text/event-stream
    {ok, #{<<"headers">> := H4, <<"body">> := B4}} = dev_llm:chat(#{}, #{<<"prompt">> => <<"hello">>, <<"endpoint">> => Endpoint, <<"stream">> => <<"true">>}, #{}),
    ?assertEqual(<<"text/event-stream">>, maps:get(<<"content-type">>, H4)),
    ?assertMatch({_,_}, binary:match(B4, <<"data:">>)),
    % hb_ao resolve path
    {ok, _} = hb_ao:resolve(#{<<"device">> => <<"llm@1.0">>, <<"path">> => <<"chat">>, <<"prompt">> => <<"hello">>, <<"endpoint">> => Endpoint}, #{}),
    % missing input -> error
    ?assertMatch({error, _}, dev_llm:embed(#{}, #{}, #{})),
    exit(Pid, kill),
    ok.

%% Lua ao.resolve path (same as AO process)
lua_ao_resolve_test_() ->
    {timeout, 30, fun lua_ao_resolve/0}.
lua_ao_resolve() ->
    {ok,_}=application:ensure_all_started(prometheus),
    {ok,_}=application:ensure_all_started(hb),
    Port = 54330,
    Pid = spawn(fun() -> mock_server(Port) end),
    timer:sleep(200),
    Endpoint = list_to_binary(io_lib:format("http://localhost:~p/v1/chat/completions", [Port])),
    Script = <<
        "function llm_chat()\n"
        "  local status, res = ao.resolve({device='llm@1.0', path='chat', prompt='Say hi', endpoint='", Endpoint/binary, "'})\n"
        "  if status ~= 'ok' then return 'fail' end\n"
        "  local body = res.body or res.Body or ''\n"
        "  return body\n"
        "end\n"
    >>,
    Base = #{
        <<"device">> => <<"lua@5.3a">>,
        <<"module">> => #{<<"content-type">> => <<"application/lua">>, <<"body">> => Script},
        <<"parameters">> => []
    },
    {ok, Res} = hb_ao:resolve(Base, <<"llm_chat">>, #{}),
    ?assertMatch({_,_}, binary:match(hb_util:bin(Res), <<"Hello from mock LLM">>)),
    exit(Pid, kill),
    ok.

%% Live Ollama (only if LLM_LIVE=1)
live_ollama_test_() ->
    case os:getenv("LLM_LIVE") of
        "1" -> {timeout, 30, fun live_ollama/0};
        _ -> []
    end.
live_ollama() ->
    {ok,_}=application:ensure_all_started(prometheus),
    {ok,_}=application:ensure_all_started(hb),
    EP = <<"http://spark-1b7b.local:8888/v1/chat/completions">>,
    {ok, #{<<"body">> := Body}} = dev_llm:chat(#{}, #{<<"prompt">> => <<"Say hello in 2 words">>, <<"model">> => <<"qwen3.6">>, <<"endpoint">> => EP}, #{}),
    Decoded = hb_json:decode(Body),
    Content = dev_llm:extract_content(Decoded),
    ?assert(byte_size(Content) > 0).

%% Helpers: tiny HTTP mock
mock_server(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, raw}, {active, false}, {reuseaddr, true}]),
    accept_loop(LSock).
accept_loop(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Sock} -> spawn(fun() -> handle(Sock) end), accept_loop(LSock);
        {error, closed} -> ok
    end.
handle(Sock) ->
    case gen_tcp:recv(Sock, 0, 5000) of
        {ok, Data} ->
            Path = case binary:match(Data, <<"POST ">>) of
                {_,_} -> [_, Rest] = binary:split(Data, <<"POST ">>), [P,_]=binary:split(Rest, <<" HTTP">>), P;
                _ -> <<"/">>
            end,
            Body = case binary:match(Path, <<"/embeddings">>) of
                nomatch ->
                    IsStream = binary:match(Data, <<"\"stream\":true">>) =/= nomatch,
                    case IsStream of
                        true -> <<"data: {\"choices\":[{\"delta\":{\"content\":\"Hello \"}}]}\n\ndata: {\"choices\":[{\"delta\":{\"content\":\"world\"}}]}\n\ndata: [DONE]\n\n">>;
                        false -> hb_json:encode(#{<<"id">> => <<"chatcmpl-test">>, <<"choices">> => [#{<<"message">> => #{<<"role">> => <<"assistant">>, <<"content">> => <<"Hello from mock LLM">>}, <<"finish_reason">> => <<"stop">>}], <<"model">> => <<"test-model">>})
                    end;
                _ -> hb_json:encode(#{<<"data">> => [#{<<"embedding">> => [0.1,0.2,0.3], <<"index">> => 0}], <<"model">> => <<"test-embed">>})
            end,
            Resp = iolist_to_binary(["HTTP/1.1 200 OK\r\n","Content-Type: application/json\r\n","Content-Length: ", integer_to_binary(byte_size(Body)), "\r\n","Connection: close\r\n","\r\n", Body]),
            gen_tcp:send(Sock, Resp), gen_tcp:close(Sock);
        _ -> gen_tcp:close(Sock)
    end.
