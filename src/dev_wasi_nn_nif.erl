%%% @doc WASI-NN NIF module for HyperBEAM.
%%% Implements native functions for AI model loading and inference.
%%% This module provides the NIF interface for the dev_wasi_nn module.
-module(dev_wasi_nn_nif).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([
    init_backend/0,
    load_by_name_with_config/3,
    init_execution_context/2,
    close_execution_context/2,
    deinit_backend/1,
    run_inference/3
]).
-export([init_execution_context_once/2, switch_model/2]).
-export([cleanup_model_contexts/1, cleanup_all_contexts/0, get_current_model_info/0]).

-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).
%% Module-level cache
-define(CACHE_TAB, wasi_nn_cache).
-define(SINGLETON_KEY, global_cache).
-define(CACHE_OWNER_NAME, wasi_nn_cache_owner).  % Registered name for cache owner process
%% @doc Start the dedicated ETS table owner process.
%% Creates a persistent process that owns the ETS table to ensure it remains
%% available even if the calling process terminates.
%%
%% @returns {ok, Pid} where Pid is the process ID of the cache owner.
start_cache_owner() ->
    case whereis(?CACHE_OWNER_NAME) of
        undefined ->
            % No owner process exists, create one
            Pid = spawn(fun() -> 
                % Create the table if it doesn't exist
                case ets:info(?CACHE_TAB) of
                    undefined ->
                        ?event(dev_wasi_nn_nif, {cache_owner_creating_table, ?CACHE_TAB}),
                        ets:new(?CACHE_TAB, [set, named_table, public]);
                    _ ->
                        ?event(dev_wasi_nn_nif, {cache_table_already_exists, ?CACHE_TAB})
                end,
                % Register the process with a name for easy lookup
                register(?CACHE_OWNER_NAME, self()),
                cache_owner_loop()
            end),
            {ok, Pid};
        Pid ->
            % Owner process already exists
            {ok, Pid}
    end.

%% @doc Loop function for the cache owner process.
%% Keeps the process alive to maintain ownership of the ETS table.
%% Handles stop messages and ping requests.
%%
%% @returns ok when the process is stopped.
cache_owner_loop() ->
    receive
        stop -> 
            ?event(dev_wasi_nn_nif, {cache_owner_stopping}),
            ok;
        {From, ping} ->
            From ! {self(), pong},
            cache_owner_loop();
        _ -> 
            cache_owner_loop()
    after 
        3600000 -> % Stay alive for a long time (1 hour), then check again
            cache_owner_loop()
    end.

%% @doc Initialize the NIF library and cache management.
%% This function is automatically called when the module is loaded.
%% It starts the cache owner process and loads the NIF library.
%%
%% @returns ok on success, exits with error on failure.
init() ->
    PrivDir = code:priv_dir(hb),
    Path = filename:join(PrivDir, "wasi_nn"),
    ?event(dev_wasi_nn_nif, {loading_nif_from, Path}),
    % Start the dedicated cache owner process
    start_cache_owner(),
    % Load the NIF library
    case erlang:load_nif(Path, 0) of
        ok ->
            ?event(dev_wasi_nn_nif, {nif_loaded_successfully}),
            ok;
        {error, {load_failed, Reason}} ->
            ?event(dev_wasi_nn_nif, {failed_to_load_nif, Reason}),
            exit({load_failed, {load_failed, Reason}});
        {error, Reason} ->
            ?event(dev_wasi_nn_nif, {failed_to_load_nif_with_error, Reason}),
            exit({load_failed, Reason})
    end.

%% Error handler for NIF functions that are not loaded
not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

init_backend() ->
    ?NOT_LOADED.

load_by_name_with_config(_Context, _Path, _Config) ->
    ?NOT_LOADED.

init_execution_context(_Context, _SessionId) ->
    ?NOT_LOADED.

close_execution_context(_Context, _ExecContextId) ->
    ?NOT_LOADED.

deinit_backend(_Context) ->
    ?NOT_LOADED.
run_inference(_Context, _ExecContextId, _Prompt) ->
    ?NOT_LOADED.

%% ============================================================================
%% GLOBAL PERSISTENT CONTEXT MANAGEMENT
%% ============================================================================

%% @doc Switch to a different model, creating a new context for each model.
%% Checks if the model is already loaded with the same configuration,
%% and reuses the existing context if possible. Otherwise, creates a new context.
%%
%% @param ModelPath Path to the model file.
%% @param Config Configuration for the model.
%% @returns {ok, Context} on success, {error, Reason} on failure.
switch_model(ModelPath, Config) ->
    ensure_cache_table(),
    ModelKey = {?SINGLETON_KEY, model_context, ModelPath},
    case ets:lookup(?CACHE_TAB, ModelKey) of
        [{_, {ok, Context, CachedConfig}}] when CachedConfig =:= Config ->
            ?event(dev_wasi_nn_nif, {model_already_loaded, ModelPath, reusing_context}),
            % Update current model reference
            ets:insert(?CACHE_TAB, {{?SINGLETON_KEY, current_model}, {ModelPath, Config, Context}}),
            {ok, Context};
        [{_, {ok, OldContext, _OldConfig}}] ->
            ?event(dev_wasi_nn_nif, {model_different_config, ModelPath, reinitializing}),
            % Cleanup old context for this model
            deinit_backend(OldContext),
            % Create new context for this model
            create_model_context(ModelPath, Config);
        [] ->
            ?event(dev_wasi_nn_nif, {model_not_loaded, ModelPath, creating_new_context}),
            create_model_context(ModelPath, Config)
    end.

%% @doc Create a new model context.
%% Initializes a new backend context and loads the model.
%%
%% @param ModelPath Path to the model file.
%% @param Config Configuration for the model.
%% @returns {ok, Context} on success, {error, Reason} on failure.
create_model_context(ModelPath, Config) ->
    ensure_cache_table(),
    ModelKey = {?SINGLETON_KEY, global_backend, ModelPath},
    % Get or create the global backend context for this model
    case ets:lookup(?CACHE_TAB, ModelKey) of
        [{_, {ok, Context}}] ->
            ?event(dev_wasi_nn_nif, {using_existing_global_backend, ModelPath}),
            load_model_with_context(Context, ModelPath, Config);
        [] ->
            ?event(dev_wasi_nn_nif, {creating_new_global_backend, ModelPath}),
            case init_backend() of
                {ok, Context} ->
                    ets:insert(?CACHE_TAB, {ModelKey, {ok, Context}}),
                    ?event(dev_wasi_nn_nif, {global_backend_created, ModelPath}),
                    load_model_with_context(Context, ModelPath, Config);
                Error ->
                    ?event(dev_wasi_nn_nif, {failed_to_create_global_backend, ModelPath, Error}),
                    Error
            end
    end.

%% @doc Load a model with an existing context.
%% Uses an existing backend context to load a model.
%%
%% @param Context The backend context.
%% @param ModelPath Path to the model file.
%% @param Config Configuration for the model.
%% @returns {ok, Context} on success, {error, Reason} on failure.
load_model_with_context(Context, ModelPath, Config) ->
    case load_by_name_with_config(Context, ModelPath, Config) of
        ok ->
            ModelKey = {?SINGLETON_KEY, model_context, ModelPath},
            ets:insert(?CACHE_TAB, {ModelKey, {ok, Context, Config}}),
            ets:insert(?CACHE_TAB, {{?SINGLETON_KEY, current_model}, {ModelPath, Config, Context}}),
            ?event(dev_wasi_nn_nif, {model_context_created, ModelPath}),
            {ok, Context};
        Error ->
            ?event(dev_wasi_nn_nif, {failed_to_load_model, ModelPath, Error}),
            % Cleanup the backend context since model loading failed
            deinit_backend(Context),
            ets:delete(?CACHE_TAB, {?SINGLETON_KEY, global_backend, ModelPath}),
            {error, {model_load_failed, Error}}
    end.

%% @doc Get information about the currently loaded model.
%% Retrieves the model path, configuration, and context for the currently loaded model.
%%
%% @returns {ok, {ModelPath, Config, Context}} on success,
%%          {error, no_model_loaded} if no model is loaded.
get_current_model_info() ->
    ensure_cache_table(),
    case ets:lookup(?CACHE_TAB, {?SINGLETON_KEY, current_model}) of
        [{_, {ModelPath, Config, Context}}] -> {ok, {ModelPath, Config, Context}};
        [] -> {error, no_model_loaded}
    end.

%% @doc Clean up all contexts for a specific model.
%% Removes all execution contexts and the model context for a specific model.
%%
%% @param ModelPath Path to the model file.
%% @returns ok.
cleanup_model_contexts(ModelPath) ->
    ensure_cache_table(),
    % Clean up all execution contexts for this model
    ets:match_delete(?CACHE_TAB, {{?SINGLETON_KEY, context_initialized, ModelPath, '_'}, '_'}),
    % Clean up the model context
    case ets:lookup(?CACHE_TAB, {?SINGLETON_KEY, model_context, ModelPath}) of
        [{_, {ok, Context, _Config}}] ->
            deinit_backend(Context),
            ets:delete(?CACHE_TAB, {?SINGLETON_KEY, model_context, ModelPath}),
            ets:delete(?CACHE_TAB, {?SINGLETON_KEY, global_backend, ModelPath}),
            ?event(dev_wasi_nn_nif, {cleaned_up_contexts, ModelPath}),
            ok;
        [] ->
            ?event(dev_wasi_nn_nif, {no_context_to_cleanup, ModelPath}),
            ok
    end.

%% @doc Clean up all cached contexts.
%% Removes all model contexts and execution contexts from the cache.
%% Useful for testing or memory management.
%%
%% @returns ok.
cleanup_all_contexts() ->
    ensure_cache_table(),
    % Get all model contexts and clean them up
    ModelContexts = ets:match(?CACHE_TAB, {{?SINGLETON_KEY, model_context, '$1'}, {ok, '$2', '$3'}}),
    lists:foreach(fun([ModelPath, Context, _Config]) ->
        deinit_backend(Context),
        ?event(dev_wasi_nn_nif, {cleaned_up_context, ModelPath})
    end, ModelContexts),
    % Clear the entire cache
    ets:delete_all_objects(?CACHE_TAB),
    ?event(dev_wasi_nn_nif, {all_contexts_cleaned_up}),
    ok.

%% @doc Helper function to safely access the ETS table.
%% Ensures that the ETS table exists and has an owner process.
%% If the table doesn't exist, it starts the cache owner process.
%% If the table exists but has no owner, it restarts the owner process.
%%
%% @returns ok if the table exists and has an owner,
%%          {ok, Pid} if a new owner process was started.
ensure_cache_table() ->
    case ets:info(?CACHE_TAB) of
        undefined ->
            % Start the cache owner which will create the table
            ?event(dev_wasi_nn_nif, {table_doesnt_exist, starting_cache_owner}),
            start_cache_owner();
        _ ->
            % Table exists, ensure owner process is running
            case whereis(?CACHE_OWNER_NAME) of
                undefined ->
                    % Strange case: table exists but no owner - restart owner
                    ?event(dev_wasi_nn_nif, {table_exists_no_owner, restarting_owner}),
                    start_cache_owner();
                _ ->
                    % All good, table exists and owner is running
                    ok
            end
    end.

%% @doc Function to ensure execution context is only initialized once per session and model.
%% Checks if an execution context already exists for the given session and model,
%% and reuses it if possible. Otherwise, creates a new execution context.
%%
%% @param Context The model context.
%% @param SessionId The session identifier.
%% @returns {ok, ExecContextId} on success, {error, Reason} on failure.
init_execution_context_once(Context, SessionId) ->
    ensure_cache_table(),
    % Get current model info to create a unique session key per model
    case get_current_model_info() of
        {ok, {ModelPath, _Config, _Context}} ->
            SessionKey = {?SINGLETON_KEY, context_initialized, ModelPath, SessionId},
            case ets:lookup(?CACHE_TAB, SessionKey) of
                [{_, {ok, ExecContextId}}] ->
                    ?event(dev_wasi_nn_nif, {execution_context_already_initialized, SessionId, ModelPath}),
                    {ok, ExecContextId};
                [] ->
                    Result = init_execution_context(Context, SessionId),
                    case Result of
                        {ok, ExecContextId} ->
                            ets:insert(?CACHE_TAB, {SessionKey, {ok, ExecContextId}}),
                            ?event(dev_wasi_nn_nif, {execution_context_initialized, SessionId, ModelPath}),
                            {ok, ExecContextId};
                        Error ->
                            ?event(dev_wasi_nn_nif, {failed_to_initialize_execution_context, SessionId, ModelPath, Error}),
                            Error
                    end
            end;
        {error, no_model_loaded} ->
            ?event(dev_wasi_nn_nif, {no_model_loaded, cannot_initialize_execution_context}),
            {error, no_model_loaded}
    end.


%% @doc Test WASI-NN inference with a single model.
%% This test validates the complete inference pipeline including model loading,
%% session management, and inference execution. The test uses a model from Arweave
%% to avoid network dependencies during inference testing.
%%
%% The test performs the following steps:
%% 1. Loads a  model from Arweave
%% 2. Creates a model context using the NIF
%% 3. Initializes an execution context for the session
%% 4. Runs inference with a test prompt
%% 5. Validates the output is not empty
%% 6. Cleans up all contexts
%%
%% @returns ok on success, throws an error on failure.
run_inference_test() ->    
    % Path to the pre-downloaded model file
    {ok, Path} = dev_wasi_nn:read_model_by_ID("ISrbGzQot05rs_HKC08O_SmkipYQnqgB1yC3mjZZeEo"),
    ?event(dev_wasi_nn_nif, {model_path, Path}),
    % Model configuration for GPU inference
    % - n_gpu_layers: Number of layers to offload to GPU
    % - ctx_size: Context window size for the model
    % - stream-stdout: Enable streaming output
    % - enable_debug_log: Enable debug logging
    Config =
        "{\"n_gpu_layers\":98,\"ctx_size\":2048,\"stream-stdout\":true,\"enable_debug_log\":true}",
    % Session identifier for context management
    SessionId = "test_session_1",
    % Test prompt for inference
    Prompt = "Who are you ?",
    % Step 1: Load the model and create a context
    % This will either create a new context or reuse an existing one
    {ok, Context} = switch_model(Path, Config),
    ?event(dev_wasi_nn_nif, {model_loaded, Context, Path, Config}),
    % Step 2: Create or reuse execution context for the session
    % This ensures session-specific state management
    {ok, ExecContextId} = init_execution_context_once(Context, SessionId),
    % Step 3: Run inference with the provided prompt
    % This is the core inference operation
    {ok, Output} = run_inference(Context, ExecContextId, Prompt),
    ?event(output, Output),
    % Step 4: Validate the inference output
    % Ensure we got a meaningful response
    ?assertNotEqual(Output, ""),
    % Step 5: Clean up all contexts to free resources
    % This is important for memory management
    cleanup_all_contexts(),
    ok.
