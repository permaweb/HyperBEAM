#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
#include <cstring>
#include "hb_inference.h"

struct hb_instance {
    llama_model* model = nullptr;
    llama_context* ctx = nullptr;
};

struct hb_instance* init() {
    llama_backend_init();
    return new hb_instance;
}

void destroy(struct hb_instance* instance) {
    if (instance) {
        if (instance->ctx) {
            llama_free(instance->ctx);
        }
        if (instance->model) {
            llama_model_free(instance->model);
        }
        delete instance;
    }
    llama_backend_free();
}

int load_model(
    struct hb_instance* instance,
    const char* model_path,
    struct llama_model_params model_params,
    struct llama_context_params ctx_params
) {
    if (!instance) {
        return 1;
    }

    instance->model = llama_model_load_from_file(model_path, model_params);
    if (instance->model == nullptr) {
        std::cerr << "Failed to load model" << std::endl;
        return 1;
    }

    instance->ctx = llama_init_from_model(instance->model, ctx_params);
    if (instance->ctx == nullptr) {
        std::cerr << "Failed to create context" << std::endl;
        llama_model_free(instance->model);
        instance->model = nullptr;
        return 1;
    }

    return 0;
}

void free_model(struct hb_instance* instance) {
    if (instance) {
        if (instance->ctx) {
            llama_free(instance->ctx);
            instance->ctx = nullptr;
        }
        if (instance->model) {
            llama_model_free(instance->model);
            instance->model = nullptr;
        }
    }
}

// Stubs for chat and completion
const char* chat(
    struct hb_instance* instance,
    const struct llama_chat_message* messages,
    size_t n_messages,
    struct hb_generate_params params
) {
    if (!instance || !instance->ctx) {
        return "Error: instance not initialized";
    }

    // Use a large buffer for the formatted prompt
    std::vector<char> buf(1024 * 8);

    // Apply the chat template
    const char * tmpl = llama_model_chat_template(instance->model, nullptr);
    int32_t result = llama_chat_apply_template(tmpl, messages, n_messages, true, buf.data(), buf.size());
    if (result < 0) {
        return "Error: failed to apply chat template";
    }
    if ((size_t)result > buf.size()) {
        buf.resize(result);
        result = llama_chat_apply_template(tmpl, messages, n_messages, true, buf.data(), buf.size());
    }

    // Call the generate function with the formatted prompt
    return generate(instance, buf.data(), params);
}

const char* completion(
    struct hb_instance* instance,
    const char* prompt,
    struct hb_generate_params params
) {
    struct llama_chat_message messages[] = {
        {"user", prompt}
    };
    return chat(instance, messages, 1, params);
}

const char* generate(
    struct hb_instance* instance,
    const char* prompt,
    struct hb_generate_params params
) {
    if (!instance || !instance->ctx) {
        return "Error: instance not initialized";
    }

    llama_context* ctx = instance->ctx;
    llama_model* model = instance->model;
    const auto vocab = llama_model_get_vocab(model);
    const int n_ctx = llama_n_ctx(ctx);

    // Create sampler chain
    auto sparams = llama_sampler_chain_default_params();
    llama_sampler * smpl = llama_sampler_chain_init(sparams);
    llama_sampler_chain_add(smpl, llama_sampler_init_top_p(params.top_p, 1));
    llama_sampler_chain_add(smpl, llama_sampler_init_temp(0.0f));
    llama_sampler_chain_add(smpl, llama_sampler_init_dist(1234));
    // llama_sampler_chain_add(smpl, llama_sampler_init_penalties(64, 1.5f, 0.0f, 0.0f));

    static std::string result_str; // not thread safe
    result_str = "";

    const bool is_first = llama_memory_seq_pos_max(llama_get_memory(ctx), 0) == -1;

    // Tokenize the prompt
    const int n_prompt_tokens = -llama_tokenize(vocab, prompt, strlen(prompt), NULL, 0, is_first, true);
    if (n_prompt_tokens < 0) {
        llama_sampler_free(smpl);
        return "Error: failed to tokenize prompt (get size)";
    }
    std::vector<llama_token> prompt_tokens(n_prompt_tokens);
    if (llama_tokenize(vocab, prompt, strlen(prompt), prompt_tokens.data(), prompt_tokens.size(), is_first, true) < 0) {
        llama_sampler_free(smpl);
        return "Error: failed to tokenize prompt (actual)";
    }

    // Prepare a batch for the prompt
    llama_batch batch = llama_batch_get_one(prompt_tokens.data(), prompt_tokens.size());
    llama_token new_token_id;

    int n_predict = std::min(params.n_predict, n_ctx - (int)prompt_tokens.size());
    int n_decoded = 0;

    while (n_decoded < n_predict) {
        // Check if we have enough space in the context to evaluate this batch
        if (llama_memory_seq_pos_max(llama_get_memory(ctx), 0) + 1 + batch.n_tokens > n_ctx) {
            llama_sampler_free(smpl);
            return "Error: context size exceeded";
        }

        int ret = llama_decode(ctx, batch);
        if (ret != 0) {
            llama_sampler_free(smpl);
            return "Error: llama_decode failed";
        }

        // Sample the next token
        new_token_id = llama_sampler_sample(smpl, ctx, -1);
        llama_sampler_accept(smpl, new_token_id);

        // Is it an end of generation?
        if (llama_vocab_is_eog(vocab, new_token_id)) {
            break;
        }

        // Convert the token to a string and add it to the response
        char buf[256];
        int n = llama_token_to_piece(vocab, new_token_id, buf, sizeof(buf), 0, true);
        if (n < 0) {
            llama_sampler_free(smpl);
            return "Error: failed to convert token to piece";
        }
        result_str.append(buf, n);

        // Prepare the next batch with the sampled token
        batch = llama_batch_get_one(&new_token_id, 1);
        n_decoded++;
    }

    llama_sampler_free(smpl);
    return result_str.c_str();
}
