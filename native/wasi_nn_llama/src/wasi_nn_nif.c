#include "../include/wasi_nn_nif.h"
#include "../include/wasi_nn_logging.h"
#define LIB_PATH "./native/wasi_nn_llama/libwasi_nn_backend.so"
#define MAX_MODEL_PATH 256
#define MAX_INPUT_SIZE 4096
#define MAX_CONFIG_SIZE 1024
#define MAX_OUTPUT_SIZE 8192

typedef struct {
    void* ctx;
    graph g;
    graph_execution_context exec_ctx;
} LlamaContext;

static wasi_nn_backend_api g_wasi_nn_functions = {0};
static ErlNifResourceType* llama_context_resource;

static void llama_context_destructor(ErlNifEnv* env, void* obj)
{

    LlamaContext* ctx = (LlamaContext*)obj;
    if (ctx) {
        // Cleanup backend context
        if (ctx->ctx && g_wasi_nn_functions.deinit_backend) {
            g_wasi_nn_functions.deinit_backend(ctx->ctx);
            ctx->ctx = NULL;
        }
        // No need to cleanup shared library here since it's managed globally
        // Clear the context structure
        memset(ctx, 0, sizeof(LlamaContext));
    }

}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    DRV_DEBUG("Load nif start\n");
	g_wasi_nn_functions.handle = dlopen(LIB_PATH, RTLD_LAZY);
    if (!g_wasi_nn_functions.handle) {
        DRV_DEBUG("Failed to load wasi library: %s\n", dlerror());
        return 1;
    }
	// Load all required functions once
	g_wasi_nn_functions.init_backend = (init_backend_fn)dlsym(g_wasi_nn_functions.handle, "init_backend");
    g_wasi_nn_functions.init_backend_with_config = (init_backend_with_config_fn)dlsym(g_wasi_nn_functions.handle, "init_backend_with_config");
    g_wasi_nn_functions.deinit_backend = (deinit_backend_fn)dlsym(g_wasi_nn_functions.handle, "deinit_backend");
    g_wasi_nn_functions.init_execution_context = (init_execution_context_fn)dlsym(g_wasi_nn_functions.handle, "init_execution_context");
    g_wasi_nn_functions.close_execution_context = (close_execution_context_fn)dlsym(g_wasi_nn_functions.handle, "close_execution_context");
    g_wasi_nn_functions.set_input = (set_input_fn)dlsym(g_wasi_nn_functions.handle, "set_input");
    g_wasi_nn_functions.compute = (compute_fn)dlsym(g_wasi_nn_functions.handle, "compute");
    g_wasi_nn_functions.get_output = (get_output_fn)dlsym(g_wasi_nn_functions.handle, "get_output");
    g_wasi_nn_functions.load_by_name_with_config = (load_by_name_with_config_fn)dlsym(g_wasi_nn_functions.handle, "load_by_name_with_config");
	g_wasi_nn_functions.run_inference = (run_inference_fn)dlsym(g_wasi_nn_functions.handle, "run_inference");
    if (!g_wasi_nn_functions.init_backend ||!g_wasi_nn_functions.deinit_backend  ||
       !g_wasi_nn_functions.init_execution_context ||!g_wasi_nn_functions.close_execution_context ||
       !g_wasi_nn_functions.set_input ||!g_wasi_nn_functions.compute ||
       !g_wasi_nn_functions.get_output	||!g_wasi_nn_functions.load_by_name_with_config ||!g_wasi_nn_functions.run_inference) {
        dlclose(g_wasi_nn_functions.handle);
        return 1;
    }
	DRV_DEBUG("Load nif Finished\n");
	llama_context_resource = enif_open_resource_type(env, NULL, "llama_context",
        llama_context_destructor, ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    return llama_context_resource ? 0 : 1;
}




static ERL_NIF_TERM nif_init_backend(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LlamaContext* ctx = enif_alloc_resource(llama_context_resource, sizeof(LlamaContext));
    if (!ctx) {
        DRV_DEBUG("Failed to allocate LlamaContext resource\n");
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                              enif_make_atom(env, "allocation_failed"));
    }
	DRV_DEBUG("Initializing backend...\n");
    wasi_nn_error err = g_wasi_nn_functions.init_backend(&ctx->ctx);
    if (err != success) {
        DRV_DEBUG("Backend initialization failed with error: %d\n", err);
        enif_release_resource(ctx);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                              enif_make_atom(env, "init_failed"));
    }
	DRV_DEBUG("nif_init_backend finished \n");
    ERL_NIF_TERM ctx_term = enif_make_resource(env, ctx);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), ctx_term);
}
static ERL_NIF_TERM nif_load_by_name_with_config(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LlamaContext* ctx;
    char *model_path = (char *)malloc(MAX_MODEL_PATH * sizeof(char));
	char *config = (char *)malloc(MAX_CONFIG_SIZE * sizeof(char));
	ERL_NIF_TERM ret_term; // Variable to hold the return term
	// if allocate failed
	if (!model_path || !config) {
        DRV_DEBUG("Memory allocation failed for model_path or config\n");
        free(model_path); // free(NULL) is safe
        free(config);
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "allocation_failed"));
    }
	// Get the context from the first argument
	if(!enif_get_resource(env, argv[0], llama_context_resource, (void**)&ctx))
	{
		DRV_DEBUG("Invalid context\n");
		ret_term = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_context"));
        goto cleanup; // Use goto for centralized cleanup
	}
	// Get the model path from the second argument
    if (!enif_get_string(env, argv[1], model_path, MAX_MODEL_PATH, ERL_NIF_LATIN1)) {
		ret_term = enif_make_tuple2(env, enif_make_atom(env, "error"),enif_make_atom(env, "invalid_model_path"));
		goto cleanup;
    }
	// Get the config from the third argument
	if (!enif_get_string(env, argv[2], config, MAX_CONFIG_SIZE, ERL_NIF_LATIN1)) {
        ret_term = enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "invalid_config"));
        goto cleanup;
    }
	DRV_DEBUG("Loading model: %s  config : %s\n", model_path, config);

    if (g_wasi_nn_functions.load_by_name_with_config(ctx->ctx, model_path, strlen(model_path),
                                                   config, strlen(config), &ctx->g) != success) {
        ret_term = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "load_failed"));
        goto cleanup;
    }

    ret_term = enif_make_atom(env, "ok");


cleanup:
    free(model_path);
    free(config);
    return ret_term;
}

static ERL_NIF_TERM nif_init_execution_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	DRV_DEBUG("Init context Start \n" );
    LlamaContext* ctx;
    char session_id[256];
    
    if (!enif_get_resource(env, argv[0], llama_context_resource, (void**)&ctx)) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_args_init_execution"));
    }
    
    if (!enif_get_string(env, argv[1], session_id, sizeof(session_id), ERL_NIF_LATIN1)) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_session_id"));
    }

    if (g_wasi_nn_functions.init_execution_context(ctx->ctx, session_id, &ctx->exec_ctx)!= success) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "init_execution_failed"));
    }
	DRV_DEBUG("Init context finished for session: %s\n", session_id);
	return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_ulong(env, ctx->exec_ctx));
}

static ERL_NIF_TERM nif_close_execution_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LlamaContext* ctx;
    unsigned long exec_ctx_id;
    
    if (!enif_get_resource(env, argv[0], llama_context_resource, (void**)&ctx)) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_args"));
    }
    
    if (!enif_get_ulong(env, argv[1], &exec_ctx_id)) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_exec_ctx"));
    }

    if (g_wasi_nn_functions.close_execution_context(ctx->ctx, (graph_execution_context)exec_ctx_id) != success) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "close_execution_failed"));
    }
    
    return enif_make_atom(env, "ok");
}
static ERL_NIF_TERM nif_run_inference(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	DRV_DEBUG("Start to run_inference \n" );
	LlamaContext* ctx;
    unsigned long exec_ctx_id;
    char *input = NULL;
    tensor_data output;
    ERL_NIF_TERM ret_term; // Variable for the return term
    ERL_NIF_TERM result_bin;
    uint32_t output_size = 0; // Initialize output_size

    // Allocate memory
    input = (char *)malloc(MAX_INPUT_SIZE * sizeof(char));
    output = (uint8_t *)malloc(MAX_OUTPUT_SIZE * sizeof(uint8_t));
    // Check allocations
    if (!input || !output ) {
        fprintf(stderr, "Initial memory allocation failed\n");
        ret_term = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "allocation_failed"));
        goto cleanup; // Jump to cleanup section
    }
	// Get the context from the first argument
	if (!enif_get_resource(env, argv[0], llama_context_resource, (void**)&ctx)) {
        ret_term = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_args"));
        goto cleanup;
    }
    
    // Get the execution context ID from the second argument
    if (!enif_get_ulong(env, argv[1], &exec_ctx_id)) {
        ret_term = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_exec_ctx"));
        goto cleanup;
    }
    
	//Get input from the third argument
	if (!enif_get_string(env, argv[2], input, MAX_INPUT_SIZE, ERL_NIF_LATIN1)) {
		DRV_DEBUG("Invalid input\n");
        ret_term = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_input"));
        goto cleanup;
	}

    tensor input_tensor = {
		.dimensions = NULL,
		.type =  fp32,
        .data = (tensor_data)input,
    };
    // Run inference with session-specific execution context
    if (g_wasi_nn_functions.run_inference(ctx->ctx, (graph_execution_context)exec_ctx_id, 0, &input_tensor, output, &output_size, NULL) != success) {
        ret_term = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "run_inference_failed"));
        goto cleanup;
    }
	DRV_DEBUG("Output size: %d\n", output_size);
	DRV_DEBUG("Output  %s\n", output);
	// TODO limit output size
    unsigned char* bin_data = enif_make_new_binary(env, output_size, &result_bin);
    if (!bin_data) {
        ret_term = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "binary_creation_failed"));
        // Output buffer still needs freeing in cleanup
        goto cleanup;
    }

    // Copy the output_buffer into the Erlang binary
    memcpy(bin_data, output, output_size);
	ret_term = enif_make_tuple2(env, enif_make_atom(env, "ok"), result_bin);
cleanup:
    // Free all allocated memory. free(NULL) is safe.
    free(input);
    free(output);
	DRV_DEBUG("Clean all");
    return ret_term;
}
static ERL_NIF_TERM nif_set_input(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	// TBD
	return enif_make_atom(env, "ok");
}
static ERL_NIF_TERM nif_compute(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	// TBD
	return enif_make_atom(env, "ok");
}
static ERL_NIF_TERM nif_get_output(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	// TBD
	return enif_make_atom(env, "ok");
}
static ERL_NIF_TERM nif_deinit_backend(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    LlamaContext* ctx;
    if (!enif_get_resource(env, argv[0], llama_context_resource, (void**)&ctx)) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "invalid_args"));
    }
    if (g_wasi_nn_functions.deinit_backend(ctx->ctx)!= success) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "deinit_failed"));
    }
    return enif_make_atom(env, "ok");
}
static ErlNifFunc nif_funcs[] = {
    {"init_backend", 0, nif_init_backend},
    {"load_by_name_with_config", 3, nif_load_by_name_with_config},
    {"init_execution_context", 2, nif_init_execution_context},
    {"close_execution_context", 2, nif_close_execution_context},
	{"deinit_backend", 1, nif_deinit_backend},
	{"run_inference", 3, nif_run_inference},
};


static void unload(ErlNifEnv* env, void* priv_data)
{
	// The resource destructor will be called automatically for any remaining resources
	if (g_wasi_nn_functions.handle) {
        dlclose(g_wasi_nn_functions.handle);
        g_wasi_nn_functions.handle = NULL;
	}
}
ERL_NIF_INIT(dev_wasi_nn_nif, nif_funcs, load, NULL, NULL, unload)