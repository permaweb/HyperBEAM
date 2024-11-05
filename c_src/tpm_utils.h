#ifndef TPM_UTILS_H
#define TPM_UTILS_H

#include "erl_nif.h"

ERL_NIF_TERM tpm_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM tpm_attest(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif // TPM_UTILS_H