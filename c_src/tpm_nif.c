#include "erl_nif.h"
#include "tpm_utils.h"

static ErlNifFunc nif_funcs[] = {
    {"get_tpm_info", 0, tpm_info},
    {"tpm_attest", 0, tpm_attest}  // Register the tpm_attest function
};

ERL_NIF_INIT(tpm_tools, nif_funcs, NULL, NULL, NULL, NULL)
