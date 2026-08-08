%%% @doc Local LLM device for HyperBEAM — `llm@1.0`.
%%%
%%% OpenAI-compatible proxy for Ollama / vLLM / llama.cpp running on localhost.
%%% Unlike `dev_relay@1.0`, this calls `hb_http` directly so `localhost` is NOT blocked.
%%%
%%% Actions:
%%%   `chat`       -> POST `/v1/chat/completions` (OpenAI shape, supports `stream=true` -> SSE)
%%%   `generate`   -> `chat` + extracts content string
%%%   `embed`      -> POST `/v1/embeddings`  (alias: `embeddings`)
-module(dev_llm).
-implements(<<"llm@1.0">>).
-export([info/1, chat/3, generate/3, embed/3, embeddings/3]).
-export([resolve_endpoint/4, build_chat_body/4, build_chat_body/5, is_stream/2, extract_content/1]).

-include_lib("eunit/include/eunit.hrl").

info(_) ->
    #{
        <<"chat">> => dev,
        <<"generate">> => dev,
        <<"embed">> => dev,
        <<"embeddings">> => dev
    }.

%% Chat — OpenAI-compatible passthrough with optional SSE streaming
chat(Base, Req, Opts) ->
    Endpoint = resolve_endpoint(Base, Req, Opts, chat),
    RawModel = get_val(<<"model">>, [Req, Base], <<"unsloth/Qwen3.6-35B-A3B-NVFP4">>, Opts),
    Model = normalize_model(RawModel),
    Stream = is_stream(Req, Opts),
    Body = build_chat_body(Base, Req, Model, Stream, Opts),
    Headers = #{<<"content-type">> => <<"application/json">>},
    case Stream of
        true ->
            case do_request(Endpoint, Body, Headers, Opts) of
                {ok, #{ <<"body">> := StreamBody } = Res} ->
                    HeadersOut = maps:get(<<"headers">>, Res, #{}),
                    {ok, Res#{
                        <<"status">> => maps:get(<<"status">>, Res, 200),
                        <<"headers">> => maps:merge(HeadersOut, #{<<"content-type">> => <<"text/event-stream">>}),
                        <<"body">> => StreamBody
                    }};
                {ok, #{ body := StreamBody } = Res} ->
                    HeadersOut = maps:get(headers, Res, #{}),
                    {ok, Res#{
                        <<"status">> => maps:get(status, Res, 200),
                        <<"headers">> => maps:merge(HeadersOut, #{<<"content-type">> => <<"text/event-stream">>}),
                        <<"body">> => StreamBody
                    }};
                {ok, Res} -> {ok, Res};
                Err -> Err
            end;
        false ->
            do_request(Endpoint, Body, Headers, Opts)
    end.

%% Generate — same as chat but extracts content string for easier AO use
generate(Base, Req, Opts) ->
    case chat(Base, Req, Opts) of
        {ok, #{ <<"body">> := Body } = Res} ->
            Parsed = try hb_json:decode(Body) catch _:_ -> #{<<"raw">> => Body} end,
            Content = extract_content(Parsed),
            {ok, Res#{ <<"content">> => Content, <<"raw">> => Parsed }};
        {ok, #{ body := Body } = Res} ->
            Parsed = try hb_json:decode(Body) catch _:_ -> #{<<"raw">> => Body} end,
            Content = extract_content(Parsed),
            {ok, Res#{ <<"content">> => Content, <<"raw">> => Parsed }};
        {ok, #{ <<"status">> := _} = Res} ->
            % Try to extract from body if present under different key
            Body = maps:get(<<"body">>, Res, maps:get(<<"Body">>, Res, undefined)),
            case Body of
                undefined -> {ok, Res};
                _ ->
                    Parsed = try hb_json:decode(Body) catch _:_ -> #{<<"raw">> => Body} end,
                    Content = extract_content(Parsed),
                    {ok, Res#{ <<"content">> => Content, <<"raw">> => Parsed }}
            end;
        Other -> Other
    end.

%% Embed — POST /v1/embeddings
embed(Base, Req, Opts) ->
    Endpoint = resolve_endpoint(Base, Req, Opts, embed),
    Model = get_val(<<"model">>, [Req, Base], <<"nomic-embed-text">>, Opts),
    Input = get_val(<<"input">>, [Req, Base], get_val(<<"prompt">>, [Req, Base], get_val(<<"data">>, [Req, Base], undefined, Opts), Opts), Opts),
    case Input of
        undefined -> {error, <<"missing input/prompt/data for embeddings">>};
        _ ->
            Body = hb_json:encode(#{
                <<"model">> => Model,
                <<"input">> => Input
            }),
            Headers = #{<<"content-type">> => <<"application/json">>},
            case do_request(Endpoint, Body, Headers, Opts) of
                {ok, #{ <<"body">> := RespBody } = Res} ->
                    Parsed = try hb_json:decode(RespBody) catch _:_ -> #{<<"raw">> => RespBody} end,
                    Ems = case Parsed of
                        #{<<"data">> := Data} when is_list(Data) ->
                            [ maps:get(<<"embedding">>, D, []) || D <- Data ];
                        #{<<"embeddings">> := E} -> E;
                        #{<<"embedding">> := E} -> [E];
                        _ -> []
                    end,
                    Single = case Ems of [One|_] -> One; [] -> [] end,
                    {ok, Res#{
                        <<"embedding">> => Single,
                        <<"embeddings">> => Ems,
                        <<"raw">> => Parsed
                    }};
                {ok, #{ body := RespBody } = Res} ->
                    Parsed = try hb_json:decode(RespBody) catch _:_ -> #{<<"raw">> => RespBody} end,
                    Ems = case Parsed of
                        #{<<"data">> := Data} when is_list(Data) ->
                            [ maps:get(<<"embedding">>, D, []) || D <- Data ];
                        #{<<"embeddings">> := E} -> E;
                        #{<<"embedding">> := E} -> [E];
                        _ -> []
                    end,
                    Single = case Ems of [One|_] -> One; [] -> [] end,
                    {ok, Res#{
                        <<"embedding">> => Single,
                        <<"embeddings">> => Ems,
                        <<"raw">> => Parsed
                    }};
                Err -> Err
            end
    end.

embeddings(Base, Req, Opts) -> embed(Base, Req, Opts).

%% Internals

resolve_endpoint(Base, Req, Opts, Type) ->
    Override = case Type of
        embed -> get_val(<<"embed-endpoint">>, [Req, Base], undefined, Opts);
        chat  -> get_val(<<"endpoint">>, [Req, Base], undefined, Opts)
    end,
    case Override of
        undefined ->
            BaseEndpoint = get_val(<<"llm-endpoint">>, [Base], undefined, Opts),
            DefaultChat = <<"http://spark-1b7b.local:8888/v1/chat/completions">>,
            DefaultEmbed = <<"http://spark-1b7b.local:8888/v1/embeddings">>,
            case {BaseEndpoint, Type} of
                {undefined, chat}  -> DefaultChat;
                {undefined, embed} -> DefaultEmbed;
                {EP, chat}  -> EP;
                {EP, embed} ->
                    case get_val(<<"llm-embed-endpoint">>, [Base], undefined, Opts) of
                        undefined ->
                            case binary:match(EP, <<"/chat/completions">>) of
                                {Pos, _} -> <<(binary:part(EP, 0, Pos))/binary, "/embeddings">>;
                                nomatch -> 
                                    case binary:match(EP, <<"/v1">>) of
                                        nomatch -> DefaultEmbed;
                                        _ -> <<EP/binary, "/embeddings">>
                                    end
                            end;
                        E2 -> E2
                    end
            end;
        EP -> EP
    end.

build_chat_body(Req, Model, Stream, Opts) ->
    build_chat_body(#{}, Req, Model, Stream, Opts).

build_chat_body(Base, Req, Model, Stream, Opts) ->
    Prompt = get_val(<<"prompt">>, [Req, Base], undefined, Opts),
    MessagesRaw0 = get_val(<<"messages">>, [Req, Base], undefined, Opts),
    MessagesRaw = deep_deref(MessagesRaw0, Opts),
    Data = get_val(<<"data">>, [Req, Base], undefined, Opts),
    Messages = case {MessagesRaw, Prompt, Data} of
        {undefined, undefined, undefined} -> [#{<<"role">> => <<"user">>, <<"content">> => <<>>}];
        {undefined, P, _} when P =/= undefined -> [#{<<"role">> => <<"user">>, <<"content">> => hb_util:bin(P)}];
        {undefined, undefined, D} when D =/= undefined -> [#{<<"role">> => <<"user">>, <<"content">> => hb_util:bin(D)}];
        {M, _, _} when is_binary(M) ->
            try hb_json:decode(M) catch _:_ -> [#{<<"role">> => <<"user">>, <<"content">> => M}] end;
        {M, _, _} -> deep_deref(M, Opts)
    end,
    Tools0 = get_val(<<"tools">>, [Req, Base], undefined, Opts),
    Tools = deep_deref(Tools0, Opts),
    ToolChoice = get_val(<<"tool_choice">>, [Req, Base], get_val(<<"tool-choice">>, [Req, Base], undefined, Opts), Opts),
    HasRawBody = get_val(<<"body">>, [Req, Base], undefined, Opts),
    case HasRawBody of
        undefined ->
            BaseBody = #{
                <<"model">> => Model,
                <<"messages">> => Messages,
                <<"stream">> => Stream
            },
            BodyWithTools = case Tools of
                undefined -> BaseBody;
                _ -> BaseBody#{ <<"tools">> => Tools }
            end,
            BodyFinal = case ToolChoice of
                undefined -> BodyWithTools;
                _ -> BodyWithTools#{ <<"tool_choice">> => ToolChoice }
            end,
            hb_json:encode(BodyFinal);
        B when is_binary(B) -> B;
        B when is_map(B) -> hb_json:encode(B#{ <<"stream">> => Stream });
        _ ->
            BaseBody2 = #{<<"model">> => Model, <<"messages">> => Messages, <<"stream">> => Stream},
            BodyWithTools2 = case Tools of
                undefined -> BaseBody2;
                _ -> BaseBody2#{ <<"tools">> => Tools }
            end,
            hb_json:encode(BodyWithTools2)
    end.

is_stream(Req, Opts) ->
    V = get_val(<<"stream">>, [Req], get_val(<<"Stream">>, [Req], false, Opts), Opts),
    case V of
        true -> true;
        <<"true">> -> true;
        <<"1">> -> true;
        1 -> true;
        _ -> false
    end.

do_request(Endpoint, Body, Headers, Opts) ->
    % Use hb_http:request/2 with a message containing path/method/body.
    % This bypasses dev_relay's is_blocked_host check and allows localhost.
    ReqMsg = #{
        <<"path">> => Endpoint,
        <<"method">> => <<"POST">>,
        <<"body">> => Body
    },
    % Merge headers into request message (content-type already in Headers)
    ReqWithHeaders = maps:merge(ReqMsg, Headers),
    case hb_http:request(ReqWithHeaders, Opts#{ <<"http-only-result">> => false }) of
        {ok, Res} when is_map(Res) ->
            % Normalize to #{<<"body">> => ..., <<"status">> => ...} shape
            Status = maps:get(<<"status">>, Res, maps:get(status, Res, 200)),
            RespBody = maps:get(<<"body">>, Res, maps:get(body, Res, <<>>)),
            RespHeaders = maps:get(<<"headers">>, Res, maps:get(headers, Res, #{})),
            {ok, #{ <<"status">> => Status, <<"body">> => RespBody, <<"headers">> => RespHeaders, <<"raw">> => Res }};
        {error, _} = Err -> Err;
        Other -> {error, #{ <<"body">> => hb_util:bin(Other) }}
    end.

get_val(Key, Maps, Default, Opts) when is_list(Maps) ->
    case Maps of
        [] -> Default;
        [H|T] -> get_val(Key, H, get_val(Key, T, Default, Opts), Opts)
    end;
get_val(Key, Map, Default, Opts) when is_map(Map) ->
    % Use hb_maps:find to handle link dereferencing via hb_cache
    case hb_maps:find(Key, Map, Opts) of
        {ok, V} -> V;
        error ->
            AtomKey = try binary_to_existing_atom(Key, utf8) catch _:_ -> undefined end,
            case AtomKey of
                undefined -> Default;
                _ -> case hb_maps:find(AtomKey, Map, Opts) of {ok, V} -> V; error -> Default end
            end
    end;
get_val(_, _, Default, _) -> Default.

deep_deref(V, Opts) when is_map(V) ->
    % Deref top-level if it's a link
    case (catch hb_cache:ensure_loaded(V, Opts)) of
        V2 when is_map(V2), V2 =/= V -> deep_deref(V2, Opts);
        _ ->
            maps:from_list([{K, deep_deref(Val, Opts)} || {K, Val} <- maps:to_list(V)])
    end;
deep_deref(V, Opts) when is_list(V) ->
    [deep_deref(X, Opts) || X <- V];
deep_deref(V, Opts) ->
    case (catch hb_cache:ensure_loaded(V, Opts)) of
        V2 when V2 =/= V -> deep_deref(V2, Opts);
        _ -> V
    end.

extract_content(#{ <<"choices">> := [#{ <<"message">> := #{ <<"content">> := C }}|_] }) -> C;
extract_content(#{ <<"choices">> := [#{ <<"delta">> := #{ <<"content">> := C }}|_] }) -> C;
extract_content(#{ <<"content">> := C }) -> C;
extract_content(_) -> <<>>.

normalize_model(<<"qwen3.6">>) -> <<"unsloth/Qwen3.6-35B-A3B-NVFP4">>;
normalize_model(<<"qwen3.6:27b">>) -> <<"unsloth/Qwen3.6-35B-A3B-NVFP4">>;
normalize_model(<<"qwen3.6:27b-coding-nvfp4">>) -> <<"unsloth/Qwen3.6-35B-A3B-NVFP4">>;
normalize_model(M) -> M.

%% Tests (run with rebar3 eunit)
-ifdef(TEST).
resolve_endpoint_test() ->
    ?assertEqual(<<"http://spark-1b7b.local:8888/v1/chat/completions">>, resolve_endpoint(#{}, #{}, #{}, chat)).
resolve_endpoint_embed_default_test() ->
    ?assertEqual(<<"http://spark-1b7b.local:8888/v1/embeddings">>, resolve_endpoint(#{}, #{}, #{}, embed)).
resolve_endpoint_override_test() ->
    ?assertEqual(<<"http://localhost:8000/v1/chat/completions">>, resolve_endpoint(#{}, #{<<"endpoint">> => <<"http://localhost:8000/v1/chat/completions">>}, #{}, chat)).
build_chat_body_prompt_test() ->
    Body = build_chat_body(#{<<"prompt">> => <<"hello">>}, <<"llama3.2">>, false, #{}),
    Decoded = hb_json:decode(Body),
    ?assertEqual(<<"llama3.2">>, maps:get(<<"model">>, Decoded)),
    ?assertEqual([#{<<"role">> => <<"user">>, <<"content">> => <<"hello">>}], maps:get(<<"messages">>, Decoded)).
is_stream_true_test() ->
    ?assertEqual(true, is_stream(#{<<"stream">> => <<"true">>}, #{})),
    ?assertEqual(false, is_stream(#{}, #{})).
-endif.
