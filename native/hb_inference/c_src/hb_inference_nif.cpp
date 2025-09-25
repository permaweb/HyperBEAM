#include "erl_nif.h"
#include "hb_inference.h"
#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <cstdlib>

// Global state
static std::map<std::string, hb_instance*> model_instances;
static ErlNifMutex* mutex;

// Forward declarations
static ERL_NIF_TERM nif_completion(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nif_chat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"nif_completion", 3, nif_completion},
    {"nif_chat", 3, nif_chat}
};

// Helper to get a string from an Erlang binary
static bool get_string_from_binary(ErlNifEnv* env, ERL_NIF_TERM term, std::string& var) {
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, term, &bin)) {
        return false;
    }
    var.assign(reinterpret_cast<char*>(bin.data), bin.size);
    return true;
}

// Helper to load a model, assuming the mutex is already locked
static hb_instance* get_or_load_model_locked(ErlNifEnv* env, const std::string& model_path) {
    auto it = model_instances.find(model_path);
    if (it != model_instances.end()) {
        return it->second;
    }

    hb_instance* instance = init();
    if (!instance) {
        return nullptr;
    }

    llama_model_params model_params = llama_model_default_params();
    model_params.n_gpu_layers = 100;
    llama_context_params ctx_params = llama_context_default_params();
    ctx_params.n_ctx = 4096;

    if (load_model(instance, model_path.c_str(), model_params, ctx_params) != 0) {
        destroy(instance);
        return nullptr;
    }

    model_instances[model_path] = instance;
    return instance;
}

static ERL_NIF_TERM nif_completion(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    std::string model_path, prompt;
    if (!get_string_from_binary(env, argv[0], model_path) || !get_string_from_binary(env, argv[1], prompt)) {
        return enif_make_badarg(env);
    }

    hb_generate_params params;
    params.n_predict = 512;
    params.top_p = 0.9f;

    ERL_NIF_TERM term_top_p;
    if (enif_get_map_value(env, argv[2], enif_make_atom(env, "top_p"), &term_top_p)) {
        double top_p;
        if (enif_get_double(env, term_top_p, &top_p)) {
            params.top_p = (float)top_p;
        }
    }
    ERL_NIF_TERM term_n_predict;
    if (enif_get_map_value(env, argv[2], enif_make_atom(env, "n_predict"), &term_n_predict)) {
        int n_predict;
        if (enif_get_int(env, term_n_predict, &n_predict)) {
            params.n_predict = n_predict;
        }
    }

    enif_mutex_lock(mutex);

    hb_instance* instance = get_or_load_model_locked(env, model_path);
    if (!instance) {
        enif_mutex_unlock(mutex);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Failed to load model", ERL_NIF_LATIN1));
    }

    const char* result = completion(instance, prompt.c_str(), params);
    std::string result_copy = result;

    enif_mutex_unlock(mutex);

    ERL_NIF_TERM result_term;
    unsigned char* result_buf = enif_make_new_binary(env, result_copy.length(), &result_term);
    memcpy(result_buf, result_copy.c_str(), result_copy.length());

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result_term);
}

static ERL_NIF_TERM nif_chat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    std::string model_path;
    if (!get_string_from_binary(env, argv[0], model_path)) {
        return enif_make_badarg(env);
    }

    unsigned int n_messages;
    if (!enif_get_list_length(env, argv[1], &n_messages)) {
        return enif_make_badarg(env);
    }

    std::vector<llama_chat_message> messages(n_messages);
    std::vector<std::string> roles(n_messages), contents(n_messages);
    ERL_NIF_TERM list = argv[1], head, tail;
    int i = 0;
    ERL_NIF_TERM role_key, content_key;
    unsigned char* role_buf = enif_make_new_binary(env, 4, &role_key);
    memcpy(role_buf, "role", 4);
    unsigned char* content_buf = enif_make_new_binary(env, 7, &content_key);
    memcpy(content_buf, "content", 7);

    while (enif_get_list_cell(env, list, &head, &tail)) {
        ERL_NIF_TERM role_term, content_term;
        if (enif_get_map_value(env, head, role_key, &role_term) && enif_get_map_value(env, head, content_key, &content_term)) {
            get_string_from_binary(env, role_term, roles[i]);
            get_string_from_binary(env, content_term, contents[i]);
        }
        messages[i] = {roles[i].c_str(), contents[i].c_str()};
        list = tail;
        i++;
    }

    hb_generate_params params;
    params.n_predict = 512;
    params.top_p = 0.9f;

    ERL_NIF_TERM term_top_p;
    if (enif_get_map_value(env, argv[2], enif_make_atom(env, "top_p"), &term_top_p)) {
        double top_p;
        if (enif_get_double(env, term_top_p, &top_p)) {
            params.top_p = (float)top_p;
        }
    }
    ERL_NIF_TERM term_n_predict;
    if (enif_get_map_value(env, argv[2], enif_make_atom(env, "n_predict"), &term_n_predict)) {
        int n_predict;
        if (enif_get_int(env, term_n_predict, &n_predict)) {
            params.n_predict = n_predict;
        }
    }

    enif_mutex_lock(mutex);

    hb_instance* instance = get_or_load_model_locked(env, model_path);
    if (!instance) {
        enif_mutex_unlock(mutex);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Failed to load model", ERL_NIF_LATIN1));
    }

    const char* result = chat(instance, messages.data(), n_messages, params);
    std::string result_copy = result;

    enif_mutex_unlock(mutex);

    ERL_NIF_TERM result_term;
    unsigned char* result_buf = enif_make_new_binary(env, result_copy.length(), &result_term);
    memcpy(result_buf, result_copy.c_str(), result_copy.length());

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result_term);
}

void null_log_callback(ggml_log_level level, const char * text, void * user_data) {
    // Do nothing.
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    llama_log_set(null_log_callback, NULL);
    mutex = enif_mutex_create((char*)"hb_inference_mutex");
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
    enif_mutex_lock(mutex);
    for (auto const& [key, val] : model_instances) {
        destroy(val);
    }
    model_instances.clear();
    enif_mutex_unlock(mutex);
    enif_mutex_destroy(mutex);
}

ERL_NIF_INIT(dev_inference, nif_funcs, load, NULL, NULL, unload)
