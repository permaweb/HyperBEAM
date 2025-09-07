-module(dev_llamacpp_nif).
-export([start/1, stop/0, load_model/1, completion/2, completion/3, chat/2, chat/3, ensure_ets/0]).
-include_lib("eunit/include/eunit.hrl").

%% C NIF does not use cargo.hrl loader; load from priv root

-on_load(init/0).

init() ->
    load().

load() ->
    PrivDir = case code:priv_dir(hb) of
        {error, bad_name} -> 
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

load_model(Opts) ->
    ensure_ets(),
    case ets:lookup(dev_llamacpp_state, server) of
        [{server, OldState}] ->
            %% Server is running, restart it
            _ = stop_server_nif(),
            
            MergedOpts = maps:merge(OldState, Opts),
            Host = maps:get(host, MergedOpts),
            Port = maps:get(port, MergedOpts),
            Model = maps:get(model, MergedOpts),
            Restarts = maps:get(restarts, OldState, 0),

            case start_server_nif(Model, Host, Port) of
                ok ->
                    ets:insert(dev_llamacpp_state, {server, MergedOpts#{restarts => Restarts + 1}}),
                    ok;
                {error, _}=E -> E
            end;
        [] ->
            %% Server not running, just start it
            start(Opts)
    end.

%% JSON binaries for opts
completion(PromptBin, OptsJSONBin) when is_binary(PromptBin), is_binary(OptsJSONBin) ->
    completion(PromptBin, OptsJSONBin, 60000).

completion(PromptBin, OptsJSONBin, _TimeoutMs) ->
    ensure_inets(),
    ensure_ets(),
    case ets:lookup(dev_llamacpp_state, server) of
        [{server, #{host := Host, port := Port}}] ->
            URL = io_lib:format("http://~s:~p/v1/completions", [binary_to_list(Host), Port]),
            case jsx:decode(OptsJSONBin, [return_maps]) of
                OptsMap when is_map(OptsMap) ->
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
        [] -> 
            % Log that ETS table is empty
            error_logger:warning_msg("ETS table dev_llamacpp_state is empty~n"),
            {error, not_running};
        _ -> 
            % Log unexpected ETS content
            error_logger:warning_msg("ETS table dev_llamacpp_state has unexpected content~n"),
            {error, not_running}
    end.

chat(MessagesJSONBin, OptsJSONBin) when is_binary(MessagesJSONBin), is_binary(OptsJSONBin) ->
    chat(MessagesJSONBin, OptsJSONBin, 60000).

chat(MessagesJSONBin, OptsJSONBin, _TimeoutMs) ->
    ensure_inets(),
    ensure_ets(),
    case ets:lookup(dev_llamacpp_state, server) of
        [{server, #{host := Host, port := Port}}] ->
            URL = io_lib:format("http://~s:~p/v1/chat/completions", [binary_to_list(Host), Port]),
            case {jsx:decode(MessagesJSONBin, [return_maps]), jsx:decode(OptsJSONBin, [return_maps])} of
                {Messages, OptsMap} when is_list(Messages), is_map(OptsMap) ->
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
        undefined -> 
            ets:new(dev_llamacpp_state, [named_table, set, public]);
        _ -> 
            ok
    end,
    % Verify table is accessible
    case ets:info(dev_llamacpp_state, name) of
        dev_llamacpp_state -> ok;
        _ -> error(ets_table_not_accessible)
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
                    Port = 4571,
                    Host = list_to_binary("127.0.0.1"),
                    R = start(#{model => list_to_binary("models/qwen2.5-14b-instruct-q2_k.gguf"), host => Host, port => Port}),
                    case R of
                        ok -> ok;
                        {error, already_running} -> ok;
                        Other -> ?assertEqual(ok, Other)
                    end,
                    Comp = completion(list_to_binary("Hello"), list_to_binary("{\"max_tokens\": 8}")),
                    ?assertMatch({ok, _}, Comp),
                    Chat = chat(list_to_binary("[{\"role\":\"user\",\"content\":\"Hi\"}]"), list_to_binary("{\"max_tokens\": 8}")),
                    ?assertMatch({ok, _}, Chat)
                end}};
        _ -> {skip, "Set HB_LLAMA_TEST=1 and ensure llama-server + models/*.gguf exist to run integration test"}
    end.
