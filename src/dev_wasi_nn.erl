%%% @doc A WASI-NN device implementation for HyperBEAM that provides AI inference
%%% capabilities. This device supports loading models from Arweave transactions
%%% and performing inference with session management for optimal performance.
%%% Models are cached locally to avoid repeated downloads.
-module(dev_wasi_nn).
-export([info/1, info/3, infer/3]).
-export([read_model_by_ID/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
%% @doc Get device information and exported functions.
%% Returns the list of functions that are exposed via the device API.
%% This is used by the HyperBEAM runtime to determine which endpoints
%% are available for this device.
%%
%% @param _ Ignored parameter.
%% @returns A map containing the list of exported functions.
info(_) -> 
    #{ exports => [info, infer] }.

%% @doc Provide HTTP info response about this device.
%% Returns comprehensive information about the WASI-NN device including
%% its capabilities, version, and API documentation. This endpoint helps
%% users understand how to interact with the AI inference functionality.
%%
%% @param _Msg1 Ignored parameter.
%% @param _Msg2 Ignored parameter.
%% @param _Opts Ignored parameter.
%% @returns {ok, InfoBody} containing device information and API documentation.
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"GPU device for handling LLM Inference">>,
        <<"version">> => <<"1.0">>,
        <<"api">> => #{
            <<"infer">> => #{
                <<"description">> => <<"LLM Inference">>,
                <<"method">> => <<"GET or POST">>,
                <<"required_params">> => #{
                    <<"prompt">> => <<"Prompt for Infer">>,
                    <<"model-id">> => <<"Arweave TXID of the model file">>
                }
            }
        }
    },
    {ok, InfoBody}.

%% @doc Perform AI inference using a specified model and prompt.
%% This function handles the complete inference workflow including model
%% retrieval (either from local cache or Arweave), session management,
%% and inference execution. Models are automatically downloaded and cached
%% locally for improved performance on subsequent requests.
%%
%% @param _M1 Ignored parameter.
%% @param M2 The request message containing inference parameters:
%%           - <<"model-id">>: Arweave transaction ID of the model file
%%           - <<"config">>: JSON configuration for the model (optional)
%%           - <<"prompt">>: The input prompt for inference
%%           - <<"session-id">>: Session identifier for context reuse (optional)
%% @param Opts A map of configuration options.
%% @returns {ok, #{<<"result">> := Result, <<"session-id">> := SessionId}} on success,
%%          {error, Reason} on failure.
infer(_M1, M2, Opts) ->
    TxID = hb_ao:get(<<"model-id">>, M2, undefined, Opts),
    ModelConfig = hb_ao:get(<<"config">>, M2, 
        "{\"n_gpu_layers\":96,\"ctx_size\":64000,\"batch_size\":64000}", Opts),
    Prompt = hb_ao:get(<<"prompt">>, M2, Opts),
    SessionId = hb_ao:get(<<"session-id">>, M2, undefined, Opts),
    ?event(dev_wasi_nn, {infer, {tx_id, TxID}, {session_id, SessionId}}),
    case TxID of
        undefined ->
            ?event(dev_wasi_nn, {infer, {fallback_to_default_model}}),
            DefaultTxID = <<"ISrbGzQot05rs_HKC08O_SmkipYQnqgB1yC3mjZZeEo">>,
            case read_model_by_ID(DefaultTxID, Opts) of
                {ok, LocalModelPath} ->
                    ?event(dev_wasi_nn, {infer, {model_ready, LocalModelPath}}),
                    load_and_infer(LocalModelPath, ModelConfig, Prompt, SessionId, Opts);
                {error, Reason} ->
                    ?event(dev_wasi_nn, {infer, {model_download_failed, Reason}}),
                    {error, {model_download_failed, Reason}}
            end;
        _ ->
            ?event(dev_wasi_nn, {infer, {downloading_model, TxID}}),
            case read_model_by_ID(TxID, Opts) of
                {ok, LocalModelPath} ->
                    ?event(dev_wasi_nn, {infer, {model_ready, LocalModelPath}}),
                    load_and_infer(LocalModelPath, ModelConfig, Prompt, SessionId, Opts);
                {error, Reason} ->
                    ?event(dev_wasi_nn, {infer, {model_download_failed, Reason}}),
                    {error, {model_download_failed, Reason}}
            end
    end.
%%%--------------------------------------------------------------------
%%% Helper Functions
%%%--------------------------------------------------------------------
%% @doc Load model and perform inference using persistent context management.
%% Handles the complete inference workflow including model loading, session
%% management, and inference execution. Uses session IDs to maintain context
%% across multiple requests for improved performance. If no session ID is
%% provided, a new unique session ID will be generated.
%%
%% @param ModelPath The local file path to the model.
%% @param ModelConfig JSON configuration string for the model.
%% @param Prompt The input prompt for inference.
%% @param ProvidedSessionId Optional session ID for context reuse. If undefined,
%%        a new session ID will be generated.
%% @param Opts A map of configuration options.
%% @returns {ok, #{<<"result">> := Result, <<"session-id">> := SessionId}} on success,
%%          {error, Reason} on failure.
load_and_infer(ModelPath, ModelConfig, Prompt, ProvidedSessionId, Opts) ->
    SessionId = case ProvidedSessionId of
        undefined -> hb_util:human_id(crypto:strong_rand_bytes(32));
        _ -> ProvidedSessionId
    end,
    ?event(dev_wasi_nn, {load_and_infer, {model_path, ModelPath}, {session_id, SessionId}}),
        % Use persistent context management (fast if model already loaded)
    case dev_wasi_nn_nif:switch_model(ModelPath, ModelConfig) of
        {ok, Context} ->
            % Create or reuse session-specific execution context
            case dev_wasi_nn_nif:init_execution_context_once(Context, binary_to_list(SessionId)) of
                {ok, ExecContextId} ->
                    % Run inference with session-specific context
                    case dev_wasi_nn_nif:run_inference(Context, ExecContextId, binary_to_list(Prompt)) of
                        {ok, Output} ->
                            ?event(output, Output),
                            {ok, #{
                                <<"result">> => Output,
                                <<"session-id">> => list_to_binary(SessionId)
                            }};
                        {error, Reason} ->
                            ?event(dev_wasi_nn, {inference_failed, SessionId, Reason}),
                            {error, Reason}
                    end;
                {error, Reason2} ->
                    ?event(dev_wasi_nn, {session_init_failed, SessionId, Reason2}),
                    {error, Reason2}
            end;
        {error, Reason3} ->
            ?event(dev_wasi_nn, {model_load_failed, SessionId, ModelPath, Reason3}),
            {error, Reason3}
    end.
%% @doc Configure options with model storage settings.
%% This helper function extends base options with appropriate model storage
%% configuration. It allows users to customize the model store if desired,
%% otherwise uses sensible defaults for local filesystem caching.
%%
%% @param BaseOpts The base options to extend with model storage configuration.
%% @returns Extended options map with model store configuration.
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
%% This function handles the complete model retrieval workflow including:
%% - Starting the HTTP server for Arweave gateway access
%% - Configuring local filesystem caching to avoid repeated downloads
%% - Downloading the model from Arweave if not already cached
%% - Resolving the local file path where the model is stored
%%
%% The function uses a two-tier storage strategy:
%% 1. First checks local cache (hb_store_fs) for existing model
%% 2. Falls back to Arweave gateway (hb_store_gateway) if not cached
%% 3. Automatically caches downloaded models locally for future use
%%
%% @param TxID The Arweave transaction ID containing the model file as a binary.
%% @param Opts The base options to extend with model storage configuration.
%% @returns {ok, LocalFilePath} where LocalFilePath is a string path to the 
%%          cached model file, or {error, Reason} on failure.
read_model_by_ID(TxID, Opts) ->
    %% Start the HTTP server (required for gateway access)
    hb_http_server:start_node(#{}),
    %% Configure options with model storage settings
    ConfiguredOpts = opts(Opts),
    %% Attempt to read the model from cache or download from Arweave
    case hb_cache:read(TxID, ConfiguredOpts) of
        {ok, Message} ->
            ?event(cache, {successfully_read_message_from_arweave}),
            %% Extract the data reference from the message
            %% This could be either a link to existing cached data or binary data
            DataLink = hb_maps:get(<<"data">>, Message, undefined, ConfiguredOpts),
            ?event(cache, {data_link, DataLink}),
            %% Handle two different data storage formats
            case DataLink of
                %% Case 1: Data is stored as a link reference to existing cached file
                {link, DataPath, _LinkOpts} ->
                    ?event(cache, {extracted_data_path, DataPath}),
                    %% Resolve the relative path to absolute filesystem path
                    %% The store resolves internal paths to actual file locations
                    ResolvedPath = hb_store:resolve(ModelStore, DataPath),
                    StoreName = hb_maps:get(<<"name">>, ModelStore, undefined, ConfiguredOpts),
                    %% Construct full path: "model-cache/resolved/path/to/file"
                    ActualFilePath = <<StoreName/binary, "/", ResolvedPath/binary>>,
                    ?event(cache, {actual_file_path, ActualFilePath}),
                    %% Convert binary path to string for external API compatibility
                    StringPath = case is_binary(ActualFilePath) of
                        true -> binary_to_list(ActualFilePath);
                        false -> ActualFilePath
                    end,
                    {ok, StringPath};
                %% Case 2: Data is stored as direct binary content (needs hash-based path)
                _ ->
                    %% Load the binary data into memory if not already loaded
                    LoadedData = hb_cache:ensure_loaded(DataLink, ConfiguredOpts),
                    ?event(cache, {loaded_data_size, byte_size(LoadedData)}),
                    %% Generate content-based hash path for storage location
                    %% This ensures identical files share the same storage location
                    Hashpath = hb_path:hashpath(LoadedData, ConfiguredOpts),
                    ?event(cache, {calculated_hashpath, Hashpath}),
                    %% Construct the standardized data path using content hash
                    DataPath = <<"data/", Hashpath/binary>>,
                    ?event(cache, {data_path, DataPath}),
                    %% Resolve to actual filesystem path and construct full path
                    ResolvedPath = hb_store:resolve(ModelStore, DataPath),
                    StoreName = hb_maps:get(<<"name">>, ModelStore, undefined, ConfiguredOpts),
                    ActualFilePath = <<StoreName/binary, "/", ResolvedPath/binary>>,
                    ?event(cache, {actual_file_path, ActualFilePath}),
                    %% Convert binary path to string for external API compatibility
                    StringPath = case is_binary(ActualFilePath) of
                        true -> binary_to_list(ActualFilePath);
                        false -> ActualFilePath
                    end,
                    {ok, StringPath}
            end;
        not_found ->
            %% Model transaction ID not found on Arweave network
            ?event({string, <<"Message not found on Arweave">>}),
            {error, not_found}
    end.

%% @doc Unit test for the complete inference API.
%% This test validates the end-to-end inference functionality by testing
%% the complete pipeline from model retrieval to inference execution.
%% The test uses the infer/3 function directly to simulate real API usage.
%%
%% IMPORTANT: This test requires the model to be available locally.
%% Run model_download_test() first to ensure the model is downloaded.
%%
%% The test performs the following steps:
%% 1. Creates a test message with model ID and prompt
%% 2. Calls the infer/3 function with the test parameters
%% 3. Validates the response format and content
%% 4. Ensures the inference result is meaningful
%%
%% This test simulates real-world API usage and validates the complete
%% inference workflow including model loading, session management,
%% and inference execution.
%%
%% @returns ok on success, throws an error on failure.
infer_test() ->
    % Create test message with inference parameters
    % - model-id: Arweave transaction ID of the model to use
    % - prompt: Input text for inference
    M2 = #{
        <<"model-id">> => <<"ISrbGzQot05rs_HKC08O_SmkipYQnqgB1yC3mjZZeEo">>,
        <<"prompt">> => <<"Hello who are you?">>
    },
    % Empty options map for this test
    Opts = #{},
    % Execute the inference API call
    case infer(#{}, M2, Opts) of
        {ok, #{<<"result">> := Result, <<"session-id">> := SessionId}} ->
            % Inference completed successfully
            ?event(dev_wasi_nn, {infer_test, {result, Result}, {session_id, SessionId}}),
            % Validate the inference result
            % Ensure result is a binary and has content
            ?assert(is_binary(Result)),
            ?assert(byte_size(Result) > 0),
            % Validate session ID is present
            ?assert(is_binary(SessionId)),
            ?assert(byte_size(SessionId) > 0);
        {error, Reason} ->
            % Inference failed
            ?event(dev_wasi_nn, {infer_test, {inference_failed, Reason}}),
            ?assert(false, Reason)
    end.

%%% Tests

%% read model ID test
read_model_by_ID_test() ->
    ID = <<"ISrbGzQot05rs_HKC08O_SmkipYQnqgB1yC3mjZZeEo">>,
    case read_model_by_ID(ID, #{}) of
        {ok, LocalModelPath} ->
            ?event(dev_wasi_nn, {read_model_by_ID_test, {model_ready, LocalModelPath}}),
            ?assert(is_list(LocalModelPath)),
            ?assert(length(LocalModelPath) > 0);
        {error, Reason} ->
            ?event(dev_wasi_nn, {read_model_by_ID_test, {model_read_failed, Reason}}),
            ?assert(false, {model_read_failed, Reason})
    end.