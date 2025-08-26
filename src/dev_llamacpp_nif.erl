-module(dev_llamacpp_nif).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

%% C NIF does not use cargo.hrl loader; load from priv root

-on_load(init/0).

init() ->
    load().

load() ->
    PrivDir = case code:priv_dir(hb) of
        {error, bad_name} -> 
            %% During tests, application might not be started. Use relative path.
            "./priv";
        Dir -> Dir
    end,
    erlang:load_nif(filename:join(PrivDir, "hb_llamacpp_nif"), 0).

start(Opts) ->
    Host = maps:get(host, Opts, <<"127.0.0.1">>),
    Port = maps:get(port, Opts, 4567),
    Model = maps:get(model, Opts),
    ensure_ets(),
    case start_server_nif(Model, Host, Port) of
        ok ->
            ets:insert(dev_llamacpp_state, {server, #{host => Host, port => Port, model => Model, restarts => 0}}),
            ok;
        {error, _}=E -> E
    end.

stop() ->
    ensure_ets(),
    _ = stop_server_nif(),
    ets:delete(dev_llamacpp_state, server),
    ok.

%% JSON binaries for opts
completion(PromptBin, OptsJSONBin) when is_binary(PromptBin), is_binary(OptsJSONBin) ->
    completion(PromptBin, OptsJSONBin, 60000).

completion(PromptBin, OptsJSONBin, _TimeoutMs) ->
    %% Use httpc to call llama.cpp server; return raw JSON
    ensure_inets(),
    ensure_ets(),
    case ets:lookup(dev_llamacpp_state, server) of
        [{server, #{host := Host, port := Port}}] ->
            URL = io_lib:format("http://~s:~p/v1/completions", [binary_to_list(Host), Port]),
            %% Parse options JSON and merge with standard fields
            case jsx:decode(OptsJSONBin, [return_maps]) of
                OptsMap when is_map(OptsMap) ->
                    %% Build complete request body
                    RequestBody = OptsMap#{
                        <<"prompt">> => PromptBin,
                        <<"stream">> => false
                    },
                    Body = jsx:encode(RequestBody),
                    case httpc:request(post, {lists:flatten(URL), [{"content-type","application/json"}], "application/json", Body}, [{timeout, 60000}], []) of
                        {ok, {{_,200,_}, _Headers, Resp}} -> {ok, iolist_to_binary(Resp)};
                        {ok, {{_,Code,_}, _H, Resp}} when Code >= 400 -> 
                            {error, {http_error, Code, iolist_to_binary(Resp)}};
                        {error, _} -> {error, timeout}
                    end;
                _ -> {error, invalid_json}
            end;
        _ -> {error, not_running}
    end.

chat(MessagesJSONBin, OptsJSONBin) when is_binary(MessagesJSONBin), is_binary(OptsJSONBin) ->
    chat(MessagesJSONBin, OptsJSONBin, 60000).

chat(MessagesJSONBin, OptsJSONBin, _TimeoutMs) ->
    ensure_inets(),
    ensure_ets(),
    case ets:lookup(dev_llamacpp_state, server) of
        [{server, #{host := Host, port := Port}}] ->
            URL = io_lib:format("http://~s:~p/v1/chat/completions", [binary_to_list(Host), Port]),
            %% Parse JSON inputs and merge properly
            case {jsx:decode(MessagesJSONBin, [return_maps]), jsx:decode(OptsJSONBin, [return_maps])} of
                {Messages, OptsMap} when is_list(Messages), is_map(OptsMap) ->
                    %% Build complete chat completion request
                    RequestBody = OptsMap#{
                        <<"messages">> => Messages,
                        <<"stream">> => false
                    },
                    Body = jsx:encode(RequestBody),
                    case httpc:request(post, {lists:flatten(URL), [{"content-type","application/json"}], "application/json", Body}, [{timeout, 60000}], []) of
                        {ok, {{_,200,_}, _Headers, Resp}} -> {ok, iolist_to_binary(Resp)};
                        {ok, {{_,Code,_}, _H, Resp}} when Code >= 400 -> 
                            {error, {http_error, Code, iolist_to_binary(Resp)}};
                        {error, _} -> {error, timeout}
                    end;
                _ -> {error, invalid_json}
            end;
        _ -> {error, not_running}
    end.

ensure_ets() ->
    case ets:info(dev_llamacpp_state) of
        undefined -> ets:new(dev_llamacpp_state, [named_table, set, public]);
        _ -> ok
    end.

ensure_inets() ->
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

%% NIF stubs
start_server_nif(_Model, _Host, _Port) -> erlang:nif_error(not_loaded).
stop_server_nif() -> erlang:nif_error(not_loaded).

integration_test_() ->
    case {os:getenv("HB_LLAMA_TEST"), filelib:is_file("_build/llama.cpp/build/bin/llama-server"), filelib:wildcard("models/*.gguf")} of
        {"1", true, [_Model|_]} ->
            {timeout, 120, {setup,
                fun() -> ok end,
                fun(_S) -> catch stop() end,
                fun() ->
                    %% Use a high port to avoid conflicts
                    Port = 4571,
                    Host = <<"127.0.0.1">>,
                    R = start(#{model => list_to_binary("models/qwen2.5-14b-instruct-q2_k.gguf"), host => Host, port => Port}),
                    case R of
                        ok -> ok;
                        {error, already_running} -> ok;
                        Other -> ?assertEqual(ok, Other)
                    end,
                    %% Small completion
                    Comp = completion(<<"Hello">>, <<"{\"max_tokens\": 8}">>),
                    ?assertMatch({ok, _}, Comp),
                    %% Small chat
                    Chat = chat(<<"[{\"role\":\"user\",\"content\":\"Hi\"}]">>, <<"{\"max_tokens\": 8}">>),
                    ?assertMatch({ok, _}, Chat)
                end}};
        _ -> {skip, "Set HB_LLAMA_TEST=1 and ensure llama-server + models/*.gguf exist to run integration test"}
    end.
