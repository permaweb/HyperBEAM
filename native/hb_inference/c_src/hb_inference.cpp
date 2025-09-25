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

extern "C" {

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
            llama_free_model(instance->model);
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

    instance->model = llama_load_model_from_file(model_path, model_params);
    if (instance->model == nullptr) {
        std::cerr << "Failed to load model" << std::endl;
        return 1;
    }

    instance->ctx = llama_new_context_with_model(instance->model, ctx_params);
    if (instance->ctx == nullptr) {
        std::cerr << "Failed to create context" << std::endl;
        llama_free_model(instance->model);
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
            llama_free_model(instance->model);
            instance->model = nullptr;
        }
    }
}

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
    int32_t result = llama_chat_apply_template(nullptr, messages, n_messages, true, buf.data(), buf.size());
    if (result < 0) {
        return "Error: failed to apply chat template";
    }
    if ((size_t)result > buf.size()) {
        buf.resize(result);
        result = llama_chat_apply_template(nullptr, messages, n_messages, true, buf.data(), buf.size());
    }

    // Call the completion function with the formatted prompt
    return completion(instance, buf.data(), params);
}

const char* completion(
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

    // Tokenize the prompt
    std::vector<llama_token> tokens_list(n_ctx);
    int n_tokens = llama_tokenize(vocab, prompt, strlen(prompt), tokens_list.data(), tokens_list.size(), true, false);
    if (n_tokens < 0) {
        return "Error: failed to tokenize prompt";
    }
    tokens_list.resize(n_tokens);


    if ((int)tokens_list.size() > n_ctx - 4) {
        return "Error: prompt is too long";
    }

    // Clear the KV cache
    llama_kv_cache_clear(ctx);

    // Create a batch
    llama_batch batch = llama_batch_init(tokens_list.size(), 0, 1);
    for (size_t i = 0; i < tokens_list.size(); ++i) {
        batch.token[i] = tokens_list[i];
        batch.pos[i] = i;
        batch.n_seq_id[i] = 1;
        batch.seq_id[i][0] = 0;
        batch.logits[i] = (i == tokens_list.size() - 1); // only get logits for the last token
    }

    // Feed the prompt to the model
    if (llama_decode(ctx, batch)) {
        llama_batch_free(batch);
        return "Error: llama_decode failed";
    }
    llama_batch_free(batch);


    // Main generation loop
    static std::string result_str; // not thread safe
    result_str = "";
    int n_cur = tokens_list.size();
    int n_predict = std::min(params.n_predict, n_ctx - n_tokens);


    // Create sampler chain
    auto sparams = llama_sampler_chain_default_params();
    llama_sampler * smpl = llama_sampler_chain_init(sparams);
    llama_sampler_chain_add(smpl, llama_sampler_init_top_k(40));
    llama_sampler_chain_add(smpl, llama_sampler_init_top_p(params.top_p, 1));
    llama_sampler_chain_add(smpl, llama_sampler_init_temp(params.temp));
    llama_sampler_chain_add(smpl, llama_sampler_init_dist(llama_rng_get_seed(llama_get_rng(ctx))));


    while (n_cur < n_tokens + n_predict) {
        // Sample the next token
        llama_token new_token_id = llama_sampler_sample(smpl, ctx, -1);
        llama_sampler_accept(smpl, new_token_id);

        // Check for EOS
        if (new_token_id == llama_vocab_eos(vocab)) {
            break;
        }

        // Append the new token to the result
        char piece[8] = {0};
        llama_token_to_piece(vocab, new_token_id, piece, sizeof(piece), 0, false);
        result_str += piece;


        // Prepare for the next iteration
        llama_batch next_batch = llama_batch_init(1, 0, 1);
        next_batch.token[0] = new_token_id;
        next_batch.pos[0] = n_cur;
        next_batch.n_seq_id[0] = 1;
        next_batch.seq_id[0][0] = 0;
        next_batch.logits[0] = 1;


        if (llama_decode(ctx, next_batch)) {
            llama_batch_free(next_batch);
            llama_sampler_free(smpl);
            return "Error: llama_decode failed";
        }
        llama_batch_free(next_batch);

        n_cur++;
    }

    llama_sampler_free(smpl);

    return result_str.c_str();
}

} // extern "C"
