%%%-------------------------------------------------------------------
%%% @doc
%%% The Inference Device provides integration with llama.cpp for running
%%% large language models directly within the HyperBEAM environment.
%%% This device enables text completion and chat conversation capabilities
%%% using GGUF format models.
%%% @end
%%%-------------------------------------------------------------------
-module(dev_inference).
-export([info/0, chat/3, completion/3]).
-on_load(init/0).
-define(DEFAULT_MODEL, <<"gemma-3-270m-it-F16.gguf">>).

init() ->
    SoPath = "native/hb_inference/build/hb_inference_nif",
    erlang:load_nif(SoPath, 0).

%% @doc
%% Returns information about the device's exported functions.
%% @spec info() -> map()
info() ->
    #{
        exports => [chat, completion]
    }.

%% @doc
%% Processes a text completion request using the loaded language model.
%% @param Msg1 The first message parameter (unused).
%% @param Msg2 The second message parameter containing the request data.
%% @param Opts The options for the request.
%% @spec completion(term(), map(), list()) -> {ok, map()} | {error, term()}
completion(_Msg1, Msg2, Opts) ->
    Prompt = extract_required_param(<<"prompt">>, Msg2, Opts),
    Reference = hb_ao:get(<<"reference">>, Msg2, undefined, Opts),
    MaxTokens = hb_ao:get(<<"max_tokens">>, Msg2, 512, Opts),
    TopP = hb_ao:get(<<"top_p">>, Msg2, 0.9, Opts),
    ModelFile = hb_ao:get(<<"model">>, Msg2, ?DEFAULT_MODEL, Opts),
    ModelPath = filename:join(["models", ModelFile]),

    case nif_completion(ModelPath, Prompt, #{top_p => TopP, n_predict => MaxTokens}) of
        {ok, Content} -> {ok, build_response(Content, Reference)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc
%% Processes a chat conversation request using the loaded language model.
%% @param Msg1 The first message parameter (unused).
%% @param Msg2 The second message parameter containing the request data.
%% @param Opts The options for the request.
%% @spec chat(term(), map(), list()) -> {ok, map()} | {error, term()}
chat(_Msg1, Msg2, Opts) ->
    Messages = extract_required_param(<<"messages">>, Msg2, Opts),
    Reference = hb_ao:get(<<"reference">>, Msg2, undefined, Opts),
    MaxTokens = hb_ao:get(<<"max_tokens">>, Msg2, 512, Opts),
    TopP = hb_ao:get(<<"top_p">>, Msg2, 0.9, Opts),
    ModelFile = hb_ao:get(<<"model">>, Msg2, ?DEFAULT_MODEL, Opts),
    ModelPath = filename:join(["models", ModelFile]),

    case nif_chat(ModelPath, Messages, #{top_p => TopP, n_predict => MaxTokens}) of
        {ok, Content} -> {ok, build_response(Content, Reference)};
        {error, Reason} -> {error, Reason}
    end.

nif_completion(_ModelPath, _Prompt, _Params) ->
    erlang:nif_error(nif_not_loaded).

nif_chat(_ModelPath, _Messages, _Params) ->
    erlang:nif_error(nif_not_loaded).

%% @doc
%% Extracts a required parameter from the message.
%% @param ParamName The name of the parameter to extract.
%% @param Msg The message containing the parameters.
%% @param Opts The options for the request.
%% @spec extract_required_param(binary(), map(), list()) -> term()
extract_required_param(ParamName, Msg, Opts) ->
    case hb_ao:get(ParamName, Msg, undefined, Opts) of
        undefined -> 
            throw({missing_required_param, ParamName});
        Value -> 
            Value
    end.

%% @doc
%% Builds a response map with the content and optional reference.
%% @param Content The content to include in the response.
%% @param Reference The optional reference to include in the response.
%% @spec build_response(binary(), term()) -> map()
build_response(Content, Reference) ->
    BaseResponse = #{
        <<"body">> => Content
    },

    case Reference of
        undefined -> BaseResponse;
        _ -> BaseResponse#{<<"X-Reference">> => Reference}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_model_path() ->
    filename:join(["models", ?DEFAULT_MODEL]).

nif_test_() ->
    {setup,
     fun() ->
             ModelPath = get_model_path(),
             case filelib:is_regular(ModelPath) of
                 true -> {ok, ModelPath};
                 false -> {skip, lists:flatten(io_lib:format("Model file not found at ~s", [ModelPath]))}
             end
     end,
     fun(_) -> ok end,
     fun(Result) ->
        case Result of
            {skip, _Reason} ->
                [];
            {ok, ModelPath} ->
                [
                    {"Completion NIF", {timeout, 60, fun() -> test_completion_nif(ModelPath) end}},
                    {"Chat NIF", {timeout, 60, fun() -> test_chat_nif(ModelPath) end}}
                ]
        end
     end
    }.

test_completion_nif(ModelPath) ->
    Prompt = <<"What is the capital of France? Answer concisely.">>, 
    Params = #{top_p => 0.9, n_predict => 20},
    ?assertMatch({ok, _}, nif_completion(ModelPath, Prompt, Params)).

test_chat_nif(ModelPath) ->
    Messages = [
        #{<<"role">> => <<"system">>, <<"content">> => <<"You are a helpful assistant.">>},
        #{<<"role">> => <<"user">>, <<"content">> => <<"What is the capital of France? Answer concisely." >>}
    ],
    Params = #{top_p => 0.9, n_predict => 20},
    ?assertMatch({ok, _}, nif_chat(ModelPath, Messages, Params)).

-endif.
