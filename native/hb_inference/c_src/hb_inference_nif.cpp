#include "erl_nif.h"
#include "hb_inference.h"
#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <cstdlib>
#include <algorithm>

// Global state
static ErlNifMutex* mutex;
static hb_instance* current_instance = nullptr;
static std::string current_model_path;

// Helper to convert Erlang map to C++ map
std::map<std::string, std::string> parse_nif_params_map(ErlNifEnv* env, ERL_NIF_TERM map_term) {
    std::map<std::string, std::string> params;
    ErlNifMapIterator iter;
    enif_map_iterator_create(env, map_term, &iter, ERL_NIF_MAP_ITERATOR_FIRST);

    ERL_NIF_TERM key_term, value_term;
    while (enif_map_iterator_get_pair(env, &iter, &key_term, &value_term)) {
        ErlNifBinary key_bin, value_bin;
        std::string key_str, value_str;

        if (enif_inspect_binary(env, key_term, &key_bin)) {
            key_str = std::string(reinterpret_cast<const char*>(key_bin.data), key_bin.size);
        } else {
            char key_buf[256];
            if (enif_get_string(env, key_term, key_buf, sizeof(key_buf), ERL_NIF_LATIN1) > 0) {
                key_str = key_buf;
            } else {
                enif_map_iterator_next(env, &iter);
                continue;
            }
        }

        if (enif_inspect_binary(env, value_term, &value_bin)) {
            value_str = std::string(reinterpret_cast<const char*>(value_bin.data), value_bin.size);
        } else {
            char value_buf[256];
            if (enif_get_string(env, value_term, value_buf, sizeof(value_buf), ERL_NIF_LATIN1) > 0) {
                value_str = value_buf;
            } else {
                enif_map_iterator_next(env, &iter);
                continue;
            }
        }
        params[key_str] = value_str;
        enif_map_iterator_next(env, &iter);
    }
    enif_map_iterator_destroy(env, &iter);
    return params;
}

// Helper to parse hb_generate_params from std::map
hb_generate_params parse_hb_generate_params(ErlNifEnv* env, const std::map<std::string, std::string>& map_params) {
    hb_generate_params params;
    params.top_p = 0.9f; // Default value
    params.n_predict = 512; // Default value

    auto it_top_p = map_params.find("top_p");
    if (it_top_p != map_params.end()) {
        try {
            params.top_p = std::stof(it_top_p->second);
        } catch (...) { /* ignore conversion errors */ }
    }

    auto it_n_predict = map_params.find("n_predict");
    if (it_n_predict != map_params.end()) {
        try {
            params.n_predict = std::stoi(it_n_predict->second);
        } catch (...) { /* ignore conversion errors */ }
    }
    return params;
}

// Helper to parse llama_model_params and llama_context_params from std::map
void parse_llama_params(ErlNifEnv* env, const std::map<std::string, std::string>& map_params,
                        llama_model_params& model_params, llama_context_params& ctx_params) {
    model_params = llama_model_default_params();
    ctx_params = llama_context_default_params();

    auto it_n_gpu_layers = map_params.find("n_gpu_layers");
    if (it_n_gpu_layers != map_params.end()) {
        try {
            model_params.n_gpu_layers = std::stoi(it_n_gpu_layers->second);
        } catch (...) { /* ignore conversion errors */ }
    }
    // Add other parameters as needed
}

void null_log_callback(ggml_log_level level, const char * text, void * user_data) {
    // Do nothing.
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    llama_log_set(null_log_callback, NULL);
    mutex = enif_mutex_create((char*)"hb_inference_mutex");
    return 0;
}

static ERL_NIF_TERM nif_load_model(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    ErlNifBinary model_path_bin;
    if (!enif_inspect_binary(env, argv[0], &model_path_bin)) {
        return enif_make_badarg(env);
    }
    std::string model_path_str(reinterpret_cast<const char*>(model_path_bin.data), model_path_bin.size);

    ERL_NIF_TERM params_term = argv[1];
    if (!enif_is_map(env, params_term)) {
        return enif_make_badarg(env);
    }
    std::map<std::string, std::string> map_params = parse_nif_params_map(env, params_term);

    enif_mutex_lock(mutex);

    if (current_instance != nullptr) {
        free_model(current_instance); // Use free_model to free resources, but keep the instance
    } else {
        current_instance = init(); // Initialize if not already initialized
        if (current_instance == nullptr) {
            enif_mutex_unlock(mutex);
            return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "instance_init_failed"));
        }
    }

    llama_model_params model_params;
    llama_context_params ctx_params;
    parse_llama_params(env, map_params, model_params, ctx_params);

    int load_result = load_model(current_instance, model_path_str.c_str(), model_params, ctx_params);

    if (load_result != 0) {
        enif_mutex_unlock(mutex);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "model_load_failed"));
    }

    current_model_path = model_path_str;

    enif_mutex_unlock(mutex);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_completion(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) { // Now expects 2 arguments: Prompt, Params
        return enif_make_badarg(env);
    }

    ErlNifBinary prompt_bin;
    if (!enif_inspect_binary(env, argv[0], &prompt_bin)) {
        return enif_make_badarg(env);
    }
    std::string prompt_str(reinterpret_cast<const char*>(prompt_bin.data), prompt_bin.size);

    ERL_NIF_TERM params_term = argv[1];
    if (!enif_is_map(env, params_term)) {
        return enif_make_badarg(env);
    }
    std::map<std::string, std::string> map_params = parse_nif_params_map(env, params_term);
    hb_generate_params gen_params = parse_hb_generate_params(env, map_params); // Convert map to hb_generate_params

    enif_mutex_lock(mutex);

    if (current_instance == nullptr) {
        enif_mutex_unlock(mutex);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "no_model_loaded"));
    }

    const char* result = completion(current_instance, prompt_str.c_str(), gen_params); // Use gen_params

    ERL_NIF_TERM ret;
    if (result) {
        ret = enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_string(env, result, ERL_NIF_LATIN1));
    } else {
        ret = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "completion_failed"));
    }

    enif_mutex_unlock(mutex);
    return ret;
}

static ERL_NIF_TERM nif_chat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) { // Now expects 2 arguments: Messages, Params
        return enif_make_badarg(env);
    }

    // Parse messages list of maps
    ERL_NIF_TERM head, tail;
    std::vector<std::map<std::string, std::string>> messages_map_vec; // Renamed to avoid confusion
    ERL_NIF_TERM messages_list = argv[0];

    while (enif_get_list_cell(env, messages_list, &head, &tail)) {
        if (!enif_is_map(env, head)) {
            return enif_make_badarg(env);
        }
        messages_map_vec.push_back(parse_nif_params_map(env, head)); // Reusing parse_nif_params_map for message maps
        messages_list = tail;
    }
    if (!enif_is_empty_list(env, messages_list)) {
        return enif_make_badarg(env); // List not properly terminated
    }

    // Convert messages_map_vec to std::vector<llama_chat_message>
    std::vector<llama_chat_message> messages_vec;
    std::vector<std::string> roles_storage; // To store string data for roles
    std::vector<std::string> contents_storage; // To store string data for contents

    for (const auto& msg_map : messages_map_vec) {
        auto it_role = msg_map.find("role");
        auto it_content = msg_map.find("content");

        if (it_role != msg_map.end() && it_content != msg_map.end()) {
            roles_storage.push_back(it_role->second);
            contents_storage.push_back(it_content->second);
            messages_vec.push_back({roles_storage.back().c_str(), contents_storage.back().c_str()});
        } else {
            return enif_make_badarg(env); // Malformed message map
        }
    }

    ERL_NIF_TERM params_term = argv[1];
    if (!enif_is_map(env, params_term)) {
        return enif_make_badarg(env);
    }
    std::map<std::string, std::string> map_params = parse_nif_params_map(env, params_term);
    hb_generate_params gen_params = parse_hb_generate_params(env, map_params); // Convert map to hb_generate_params

    enif_mutex_lock(mutex);

    if (current_instance == nullptr) {
        enif_mutex_unlock(mutex);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "no_model_loaded"));
    }

    const char* result = chat(current_instance, messages_vec.data(), messages_vec.size(), gen_params); // Use gen_params

    ERL_NIF_TERM ret;
    if (result) {
        ret = enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_string(env, result, ERL_NIF_LATIN1));
    } else {
        ret = enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "chat_failed"));
    }

    enif_mutex_unlock(mutex);
    return ret;
}

static void unload(ErlNifEnv* env, void* priv_data) {
    enif_mutex_lock(mutex);
    if (current_instance != nullptr) {
        destroy(current_instance);
        current_instance = nullptr;
        current_model_path.clear();
    }
    enif_mutex_unlock(mutex);
    enif_mutex_destroy(mutex);
}

static ErlNifFunc nif_funcs[] = {
    {"nif_load_model", 2, nif_load_model, 0},
    {"nif_completion", 2, nif_completion, 0},
    {"nif_chat", 2, nif_chat, 0}
};

ERL_NIF_INIT(dev_inference, nif_funcs, load, NULL, NULL, unload)
