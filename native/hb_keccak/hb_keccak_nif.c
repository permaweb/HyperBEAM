#include "erl_nif.h"
#include "include/hb_keccak.h"
#include <string.h>
#include <stdlib.h>

static ERL_NIF_TERM nif_sha3_256(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary input;
    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    uint8_t output[32];
    sha3_256(output, 32, input.data, input.size);  // this is the actual C implementation

    ERL_NIF_TERM result;
    uint8_t* bin = enif_make_new_binary(env, 32, &result);
    memcpy(bin, output, 32);
    return result;
}

static ERL_NIF_TERM nif_keccak_256(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
	ErlNifBinary input;
    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }
	uint8_t output[32];
    keccak_256(output, 32, input.data, input.size);  

    ERL_NIF_TERM result;
    uint8_t* bin = enif_make_new_binary(env, 32, &result);
    memcpy(bin, output, 32);
    return result;
}

static ErlNifFunc nif_funcs[] = {
    {"sha3_256", 1, nif_sha3_256},
    {"keccak_256", 1, nif_keccak_256}
};

ERL_NIF_INIT(hb_keccak, nif_funcs, NULL, NULL, NULL, NULL)
