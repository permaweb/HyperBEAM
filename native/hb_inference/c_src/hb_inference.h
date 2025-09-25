#ifndef HB_INFERENCE_H
#define HB_INFERENCE_H

#include "llama.h"

#ifdef __cplusplus
extern "C" {
#endif

// Opaque pointer to the wrapper instance
struct hb_instance;

// Parameters for text generation
struct hb_generate_params {
    float top_p;
    int32_t n_predict;
    // Add other sampler parameters here
};

// API for the wrapper
struct hb_instance* init();
void destroy(struct hb_instance* instance);

int load_model(
    struct hb_instance* instance,
    const char* model_path,
    struct llama_model_params model_params,
    struct llama_context_params ctx_params
);
void free_model(struct hb_instance* instance);

// Chat and Completion APIs
const char* chat(
    struct hb_instance* instance,
    const struct llama_chat_message* messages,
    size_t n_messages,
    struct hb_generate_params params
);

const char* completion(
    struct hb_instance* instance,
    const char* prompt,
    struct hb_generate_params params
);

#ifdef __cplusplus
}
#endif

#endif // HB_INFERENCE_H
