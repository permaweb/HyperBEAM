#include <erl_nif.h>
#include "include/dev_calculator.h"

// Load the NIF library
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    return 0; // Initialization code if needed
}

// Unload the NIF library
static void unload(ErlNifEnv* env, void* priv_data) {
    // Cleanup code if needed
}

// NIF function to perform a calculation
static ERL_NIF_TERM calculate_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    char operation[256];
	
	printf("in C++ calculate process\n");

    if (!enif_get_string(env, argv[0], operation, sizeof(operation), ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    double operands[2];
    if (!enif_get_double(env, argv[1], &operands[0]) || !enif_get_double(env, argv[2], &operands[1])) {
        return enif_make_badarg(env);
    }

    double result = perform_calculation(operation, operands);
    return enif_make_double(env, result);
}

static ErlNifFunc nif_funcs[] = {
    {"calculate", 3 , calculate_nif}
};

ERL_NIF_INIT(dev_calculator_nif, nif_funcs, load, NULL, NULL, unload)