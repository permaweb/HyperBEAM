#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string> // Added for std::string
#include <iostream> // Added for std::cout
#include "hb_inference.h"

// Helper to create default model parameters
static struct llama_model_params get_default_model_params() {
    struct llama_model_params params = llama_model_default_params();
    params.n_gpu_layers = 100; // Adjust as needed
    return params;
}

// Helper to create default context parameters
static struct llama_context_params get_default_context_params() {
    struct llama_context_params params = llama_context_default_params();
    params.n_ctx = 4096;
    return params;
}

// Helper to create default generation parameters
static struct hb_generate_params get_default_generate_params() {
    struct hb_generate_params params;
    params.n_predict = 512;
    params.top_p = 0.9f;
    return params;
}

void run_completion_test(struct hb_instance* instance) {
    std::cout << "\n--- Running Completion Test ---\n";
    const char* prompt = "/no_think What's Arweave? Answer concisely.";
    struct hb_generate_params gen_params = get_default_generate_params();

    std::cout << "Prompt: " << prompt << "\n";
    std::string result = completion(instance, prompt, gen_params); // Now returns std::string
    std::cout << "Result: " << result << "\n";
    std::cout << "--- Completion Test Finished ---\n";
}

void run_chat_test(struct hb_instance* instance) {
    std::cout << "\n--- Running Chat Test ---\n";
    struct llama_chat_message messages[] = {
        {"system", "/no_think You are an assistant who say no to everything."},
        {"user", "What is the capital of France?"},
        {"assistant", "No, I won't tell you."},
        {"user", "Why not?"},
    };
    size_t n_messages = sizeof(messages) / sizeof(messages[0]);
    struct hb_generate_params gen_params = get_default_generate_params();

    std::cout << "Chatting with " << n_messages << " messages...\n";
    std::string result = chat(instance, messages, n_messages, gen_params); // Now returns std::string
    std::cout << "Assistant: " << result << "\n";
    std::cout << "--- Chat Test Finished ---\n";
}


int main(int argc, char* argv[]) {
    struct hb_instance* instance = init();
    if (!instance) {
        std::cerr << "Failed to initialize instance\n";
        return 1;
    }

    const char* gemma_model_path = "/home/jax/Desktop/apuslabs/HyperBEAM/models/gemma-3-270m-it-F16.gguf";
    const char* qwen3_model_path = "/home/jax/Desktop/apuslabs/HyperBEAM/models/Qwen3-4B-BF16.gguf";

    struct llama_model_params model_params = get_default_model_params();
    struct llama_context_params ctx_params = get_default_context_params();

    std::cout << "Loading model: " << gemma_model_path << "\n";
    if (load_model(instance, gemma_model_path, model_params, ctx_params) != 0) {
        std::cerr << "Failed to load model\n";
        destroy(instance);
        return 1;
    }

    run_completion_test(instance);
    run_chat_test(instance);

    // Free the current model before loading a new one
    free_model(instance);
    std::cout << "Loading model: " << qwen3_model_path << "\n";
    if (load_model(instance, qwen3_model_path, model_params, ctx_params) != 0) {
        std::cerr << "Failed to load model\n";
        destroy(instance);
        return 1;
    }
    run_completion_test(instance);
    run_chat_test(instance);

    destroy(instance);

    return 0;
}
