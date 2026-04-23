#include "tpm_helpers.h"

#include <openssl/bn.h>
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/rsa.h>
#include <openssl/core_names.h>
#include <openssl/param_build.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

ESYS_CONTEXT *g_esys_ctx = NULL;
TSS2_TCTI_CONTEXT *g_tcti_ctx = NULL;
char g_tcti_conf[512] = {0};

ERL_NIF_TERM
lapee_make_tss_error(ErlNifEnv *env, const char *op, TSS2_RC rc)
{
    const char *msg = Tss2_RC_Decode(rc);
    char buf[512];
    snprintf(buf, sizeof(buf), "%s: 0x%08x (%s)", op, rc, msg ? msg : "?");
    ERL_NIF_TERM msg_bin;
    unsigned char *out = enif_make_new_binary(env, strlen(buf), &msg_bin);
    memcpy(out, buf, strlen(buf));
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_tuple2(env,
                                             enif_make_atom(env, "tss2_rc"),
                                             msg_bin));
}

ERL_NIF_TERM
lapee_make_error(ErlNifEnv *env, const char *reason)
{
    return enif_make_tuple2(env,
                            enif_make_atom(env, "error"),
                            enif_make_atom(env, reason));
}

int
lapee_marshal_public(const TPM2B_PUBLIC *pub, unsigned char **out, size_t *outlen)
{
    size_t needed = 0;
    TSS2_RC rc = Tss2_MU_TPM2B_PUBLIC_Marshal(pub, NULL, 4096, &needed);
    if (rc != TSS2_RC_SUCCESS || needed == 0) return -1;

    unsigned char *buf = enif_alloc(needed);
    if (!buf) return -1;
    size_t off = 0;
    rc = Tss2_MU_TPM2B_PUBLIC_Marshal(pub, buf, needed, &off);
    if (rc != TSS2_RC_SUCCESS) {
        enif_free(buf);
        return -1;
    }
    *out = buf;
    *outlen = off;
    return 0;
}

/* Build an OpenSSL RSA EVP_PKEY from TPM2B_PUBLIC containing an RSA key, then
 * PEM-encode its SubjectPublicKeyInfo. Uses OpenSSL 3.x EVP_PKEY_fromdata. */
int
lapee_tpm2b_public_to_pem(const TPM2B_PUBLIC *pub, unsigned char **out, size_t *outlen)
{
    if (pub->publicArea.type != TPM2_ALG_RSA) return -1;

    const TPMS_RSA_PARMS *rparms = &pub->publicArea.parameters.rsaDetail;
    const TPM2B_PUBLIC_KEY_RSA *rmod = &pub->publicArea.unique.rsa;

    uint32_t exp_u32 = rparms->exponent;
    if (exp_u32 == 0) exp_u32 = 65537;

    BIGNUM *n = BN_bin2bn(rmod->buffer, rmod->size, NULL);
    BIGNUM *e = BN_new();
    if (!n || !e) goto fail;
    BN_set_word(e, exp_u32);

    OSSL_PARAM_BLD *bld = OSSL_PARAM_BLD_new();
    if (!bld) goto fail;
    OSSL_PARAM_BLD_push_BN(bld, OSSL_PKEY_PARAM_RSA_N, n);
    OSSL_PARAM_BLD_push_BN(bld, OSSL_PKEY_PARAM_RSA_E, e);
    OSSL_PARAM *params = OSSL_PARAM_BLD_to_param(bld);
    OSSL_PARAM_BLD_free(bld);
    if (!params) goto fail;

    EVP_PKEY *pkey = NULL;
    EVP_PKEY_CTX *pctx = EVP_PKEY_CTX_new_from_name(NULL, "RSA", NULL);
    if (!pctx) { OSSL_PARAM_free(params); goto fail; }
    if (EVP_PKEY_fromdata_init(pctx) <= 0 ||
        EVP_PKEY_fromdata(pctx, &pkey, EVP_PKEY_PUBLIC_KEY, params) <= 0) {
        EVP_PKEY_CTX_free(pctx);
        OSSL_PARAM_free(params);
        goto fail;
    }
    EVP_PKEY_CTX_free(pctx);
    OSSL_PARAM_free(params);

    BIO *bio = BIO_new(BIO_s_mem());
    if (!bio) { EVP_PKEY_free(pkey); goto fail; }
    if (PEM_write_bio_PUBKEY(bio, pkey) != 1) {
        BIO_free(bio);
        EVP_PKEY_free(pkey);
        goto fail;
    }
    BUF_MEM *bm = NULL;
    BIO_get_mem_ptr(bio, &bm);
    unsigned char *pem = enif_alloc(bm->length);
    if (!pem) { BIO_free(bio); EVP_PKEY_free(pkey); goto fail; }
    memcpy(pem, bm->data, bm->length);
    *out = pem;
    *outlen = bm->length;
    BIO_free(bio);
    EVP_PKEY_free(pkey);
    BN_free(n);
    BN_free(e);
    return 0;

fail:
    if (n) BN_free(n);
    if (e) BN_free(e);
    return -1;
}
