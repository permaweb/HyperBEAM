#ifndef LAPEE_TPM_HELPERS_H
#define LAPEE_TPM_HELPERS_H

#include <erl_nif.h>
#include <tss2/tss2_esys.h>
#include <tss2/tss2_mu.h>
#include <tss2/tss2_rc.h>
#include <tss2/tss2_tctildr.h>
#include <stddef.h>

/* Global ESYS context and TCTI, initialised on load. */
extern ESYS_CONTEXT *g_esys_ctx;
extern TSS2_TCTI_CONTEXT *g_tcti_ctx;
extern char g_tcti_conf[512];

/* Build an {error, Reason} tuple where Reason is a TPM2_RC decoded string. */
ERL_NIF_TERM lapee_make_tss_error(ErlNifEnv *env, const char *op, TSS2_RC rc);

/* Build an {error, atom} tuple. */
ERL_NIF_TERM lapee_make_error(ErlNifEnv *env, const char *reason);

/* Convert a TPM2B_PUBLIC (RSA) to PEM-encoded SubjectPublicKeyInfo in a binary. */
int lapee_tpm2b_public_to_pem(const TPM2B_PUBLIC *pub, unsigned char **out,
                              size_t *outlen);

/* Marshal a TPM2B_PUBLIC into a binary buffer (allocated with enif_alloc). */
int lapee_marshal_public(const TPM2B_PUBLIC *pub, unsigned char **out,
                         size_t *outlen);

#endif
