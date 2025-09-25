#include "hb_inference.h"
#include <stdio.h>
#include <string.h>

int main() {
    // Get default model and context parameters
    struct llama_model_params model_params = llama_model_default_params();
    struct llama_context_params ctx_params = llama_context_default_params();

    // Create an instance
    struct hb_instance* instance = init();
    if (!instance) {
        printf("Failed to create instance\n");
        return 1;
    }

    // Load the model
    const char* model_path = "models/ggml-model-q4_0.bin"; // Assuming this model exists
    if (load_model(instance, model_path, model_params, ctx_params) != 0) {
        printf("Failed to load model\n");
        destroy(instance);
        return 1;
    }

    // --- Test completion ---
    printf("--- Completion Test ---\n");
    struct hb_generate_params gen_params = {
        .temp = 0.8f,
        .top_p = 0.95f,
        .n_predict = 128,
    };
    const char* prompt = "Building a website can be done in 10 simple steps:";
    const char* completion_result = completion(instance, prompt, gen_params);
    printf("Prompt: %s\n", prompt);
    printf("Completion: %s\n", completion_result);
    printf("\n");


    // --- Test chat ---
    printf("--- Chat Test ---\n");
    struct llama_chat_message messages[] = {
        {"system", "You are a helpful assistant."},
        {"user", "What is the capital of France?"},
    };
    size_t n_messages = sizeof(messages) / sizeof(messages[0]);

    const char* chat_result = chat(instance, messages, n_messages, gen_params);
    printf("Chat History:\n");
    for (size_t i = 0; i < n_messages; ++i) {
        printf("- %s: %s\n", messages[i].role, messages[i].content);
    }
    printf("Assistant: %s\n", chat_result);


    // Clean up
    free_model(instance);
    destroy(instance);

    return 0;
}
