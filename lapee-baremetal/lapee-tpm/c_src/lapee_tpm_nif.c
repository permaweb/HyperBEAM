/*
 * lapee_tpm_nif.c — Erlang NIF wrapping libtss2-esys for TPM 2.0.
 *
 * Real FFI into the ESYS API. No subprocess, no CLI wrapping.
 * Connects to swtpm via the mssim or swtpm TCTI (chosen via load info).
 */

#include <erl_nif.h>
#include <tss2/tss2_esys.h>
#include <tss2/tss2_mu.h>
#include <tss2/tss2_rc.h>
#include <tss2/tss2_tctildr.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include "tpm_helpers.h"

/*-------------------------------- Load / Unload -----------------------------*/

static TSS2_RC
parse_tcti_load_info(ErlNifEnv *env, ERL_NIF_TERM load_info, char *out, size_t outlen)
{
    /* load_info is expected to be a string (list) like "swtpm:host=..." */
    unsigned len = 0;
    if (!enif_get_list_length(env, load_info, &len)) {
        /* Try binary */
        ErlNifBinary bin;
        if (enif_inspect_binary(env, load_info, &bin)) {
            if (bin.size >= outlen) return 1;
            memcpy(out, bin.data, bin.size);
            out[bin.size] = 0;
            return 0;
        }
        return 1;
    }
    if (len >= outlen) return 1;
    if (enif_get_string(env, load_info, out, outlen, ERL_NIF_LATIN1) <= 0)
        return 1;
    return 0;
}

static int
do_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    (void)priv_data;

    if (parse_tcti_load_info(env, load_info, g_tcti_conf, sizeof(g_tcti_conf)) != 0) {
        /* Default if not provided. */
        snprintf(g_tcti_conf, sizeof(g_tcti_conf),
                 "swtpm:host=127.0.0.1,port=2321");
    }

    TSS2_RC rc = Tss2_TctiLdr_Initialize(g_tcti_conf, &g_tcti_ctx);
    if (rc != TSS2_RC_SUCCESS) {
        fprintf(stderr, "[lapee_tpm_nif] Tss2_TctiLdr_Initialize(%s) failed: 0x%x (%s)\n",
                g_tcti_conf, rc, Tss2_RC_Decode(rc));
        return 1;
    }
    rc = Esys_Initialize(&g_esys_ctx, g_tcti_ctx, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        fprintf(stderr, "[lapee_tpm_nif] Esys_Initialize failed: 0x%x (%s)\n",
                rc, Tss2_RC_Decode(rc));
        Tss2_TctiLdr_Finalize(&g_tcti_ctx);
        return 1;
    }
    return 0;
}

static void
do_unload(ErlNifEnv *env, void *priv_data)
{
    (void)env; (void)priv_data;
    if (g_esys_ctx) { Esys_Finalize(&g_esys_ctx); g_esys_ctx = NULL; }
    if (g_tcti_ctx) { Tss2_TctiLdr_Finalize(&g_tcti_ctx); g_tcti_ctx = NULL; }
}

/*-------------------------------- startup/0 ---------------------------------*/

static ERL_NIF_TERM
nif_startup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc; (void)argv;
    TSS2_RC rc = Esys_Startup(g_esys_ctx, TPM2_SU_CLEAR);
    if (rc == TPM2_RC_INITIALIZE) {
        /* Already started. Idempotent. */
        return enif_make_atom(env, "ok");
    }
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_Startup", rc);
    }
    return enif_make_atom(env, "ok");
}

/*-------------------------------- pcr_read/1 --------------------------------*/

static ERL_NIF_TERM
nif_pcr_read(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    int idx;
    if (!enif_get_int(env, argv[0], &idx) || idx < 0 || idx > 23) {
        return enif_make_badarg(env);
    }

    TPML_PCR_SELECTION sel = {
        .count = 1,
        .pcrSelections = {
            {
                .hash = TPM2_ALG_SHA256,
                .sizeofSelect = 3,
                .pcrSelect = {0, 0, 0},
            }
        }
    };
    sel.pcrSelections[0].pcrSelect[idx / 8] = 1 << (idx % 8);

    UINT32 update_counter = 0;
    TPML_PCR_SELECTION *out_sel = NULL;
    TPML_DIGEST *digests = NULL;
    TSS2_RC rc = Esys_PCR_Read(g_esys_ctx,
                               ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE,
                               &sel, &update_counter, &out_sel, &digests);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_PCR_Read", rc);
    }
    if (!digests || digests->count < 1) {
        if (out_sel) Esys_Free(out_sel);
        if (digests) Esys_Free(digests);
        return lapee_make_error(env, "no_digest");
    }
    ERL_NIF_TERM result;
    unsigned char *bin = enif_make_new_binary(env, digests->digests[0].size, &result);
    memcpy(bin, digests->digests[0].buffer, digests->digests[0].size);

    Esys_Free(out_sel);
    Esys_Free(digests);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

/*-------------------------------- pcr_extend/2 ------------------------------*/

static ERL_NIF_TERM
nif_pcr_extend(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    int idx;
    ErlNifBinary data;
    if (!enif_get_int(env, argv[0], &idx) || idx < 0 || idx > 23) {
        return enif_make_badarg(env);
    }
    if (!enif_inspect_binary(env, argv[1], &data) || data.size != 32) {
        return enif_make_badarg(env);
    }

    TPML_DIGEST_VALUES digests = {
        .count = 1,
        .digests = {
            {
                .hashAlg = TPM2_ALG_SHA256,
            }
        }
    };
    memcpy(digests.digests[0].digest.sha256, data.data, 32);

    ESYS_TR pcr_handle = (ESYS_TR)idx; /* PCR index == ESYS_TR for PCRs 0..23. */

    TSS2_RC rc = Esys_PCR_Extend(g_esys_ctx,
                                 pcr_handle,
                                 ESYS_TR_PASSWORD, ESYS_TR_NONE, ESYS_TR_NONE,
                                 &digests);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_PCR_Extend", rc);
    }
    return enif_make_atom(env, "ok");
}

/*-------------------------------- create_primary_ek/0 -----------------------*/

/* The standard EK template (TCG EK Credential Profile, low range) —
 * RSA 2048, SHA-256, restricted decryption key in the endorsement hierarchy. */
static const TPM2B_PUBLIC ek_template = {
    .size = 0,
    .publicArea = {
        .type = TPM2_ALG_RSA,
        .nameAlg = TPM2_ALG_SHA256,
        .objectAttributes =
            TPMA_OBJECT_FIXEDTPM | TPMA_OBJECT_FIXEDPARENT |
            TPMA_OBJECT_SENSITIVEDATAORIGIN | TPMA_OBJECT_ADMINWITHPOLICY |
            TPMA_OBJECT_RESTRICTED | TPMA_OBJECT_DECRYPT,
        .authPolicy = {
            .size = 32,
            .buffer = {
                /* TPM2_PolicySecret(TPM_RH_ENDORSEMENT) SHA-256 digest. */
                0x83, 0x71, 0x97, 0x67, 0x44, 0x84, 0xb3, 0xf8,
                0x1a, 0x90, 0xcc, 0x8d, 0x46, 0xa5, 0xd7, 0x24,
                0xfd, 0x52, 0xd7, 0x6e, 0x06, 0x52, 0x0b, 0x64,
                0xf2, 0xa1, 0xda, 0x1b, 0x33, 0x14, 0x69, 0xaa
            }
        },
        .parameters.rsaDetail = {
            .symmetric = {
                .algorithm = TPM2_ALG_AES,
                .keyBits.aes = 128,
                .mode.aes = TPM2_ALG_CFB,
            },
            .scheme = { .scheme = TPM2_ALG_NULL },
            .keyBits = 2048,
            .exponent = 0,
        },
        .unique.rsa = { .size = 256, .buffer = {0} }
    }
};

static ERL_NIF_TERM
nif_create_primary_ek(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc; (void)argv;

    TPM2B_SENSITIVE_CREATE in_sensitive = { .size = 0 };
    TPM2B_DATA outside_info = { .size = 0 };
    TPML_PCR_SELECTION creation_pcr = { .count = 0 };

    ESYS_TR ek_tr = ESYS_TR_NONE;
    TPM2B_PUBLIC *out_public = NULL;
    TPM2B_CREATION_DATA *creation_data = NULL;
    TPM2B_DIGEST *creation_hash = NULL;
    TPMT_TK_CREATION *creation_ticket = NULL;

    TSS2_RC rc = Esys_CreatePrimary(g_esys_ctx,
                                    ESYS_TR_RH_ENDORSEMENT,
                                    ESYS_TR_PASSWORD, ESYS_TR_NONE, ESYS_TR_NONE,
                                    &in_sensitive, &ek_template,
                                    &outside_info, &creation_pcr,
                                    &ek_tr, &out_public,
                                    &creation_data, &creation_hash,
                                    &creation_ticket);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_CreatePrimary(EK)", rc);
    }

    TPM2_HANDLE tpm_handle = 0;
    rc = Esys_TR_GetTpmHandle(g_esys_ctx, ek_tr, &tpm_handle);
    if (rc != TSS2_RC_SUCCESS) {
        Esys_FlushContext(g_esys_ctx, ek_tr);
        if (out_public) Esys_Free(out_public);
        if (creation_data) Esys_Free(creation_data);
        if (creation_hash) Esys_Free(creation_hash);
        if (creation_ticket) Esys_Free(creation_ticket);
        return lapee_make_tss_error(env, "Esys_TR_GetTpmHandle", rc);
    }

    unsigned char *pem = NULL; size_t pem_len = 0;
    if (lapee_tpm2b_public_to_pem(out_public, &pem, &pem_len) != 0) {
        Esys_FlushContext(g_esys_ctx, ek_tr);
        if (out_public) Esys_Free(out_public);
        if (creation_data) Esys_Free(creation_data);
        if (creation_hash) Esys_Free(creation_hash);
        if (creation_ticket) Esys_Free(creation_ticket);
        return lapee_make_error(env, "pem_encode_failed");
    }

    ERL_NIF_TERM pem_term;
    unsigned char *pem_out = enif_make_new_binary(env, pem_len, &pem_term);
    memcpy(pem_out, pem, pem_len);
    enif_free(pem);

    /* We deliberately store ESYS_TR in the map too under 'esys_tr' so the
     * caller can re-use it for Esys_* calls without a re-load. */
    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map,
                      enif_make_atom(env, "handle"),
                      enif_make_uint(env, tpm_handle), &map);
    enif_make_map_put(env, map,
                      enif_make_atom(env, "esys_tr"),
                      enif_make_uint(env, ek_tr), &map);
    enif_make_map_put(env, map,
                      enif_make_atom(env, "public_pem"),
                      pem_term, &map);

    if (out_public) Esys_Free(out_public);
    if (creation_data) Esys_Free(creation_data);
    if (creation_hash) Esys_Free(creation_hash);
    if (creation_ticket) Esys_Free(creation_ticket);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), map);
}

/*-------------------------------- create_signing_key/1 ----------------------*/

/* RSA-2048 SHA-256 RSASSA-PSS signing key, created under a primary (EK).
 * Note: real EK-AK binding requires a policy session (TPM2_PolicySecret with
 * endorsement auth). For first-cut correctness against swtpm, we instead
 * create the AK under the Owner hierarchy primary or Null hierarchy. Here
 * we actually make a fresh primary under the Owner hierarchy — simpler and
 * still proves end-to-end quote+verify. The parent handle argument is
 * accepted but ignored for this milestone; see RESULT.md. */
static ERL_NIF_TERM
nif_create_signing_key(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    unsigned parent_handle;
    if (!enif_get_uint(env, argv[0], &parent_handle)) {
        return enif_make_badarg(env);
    }

    /* Template for restricted RSA-2048 signing key (PSS, SHA-256). */
    TPM2B_PUBLIC in_public = {
        .size = 0,
        .publicArea = {
            .type = TPM2_ALG_RSA,
            .nameAlg = TPM2_ALG_SHA256,
            .objectAttributes =
                TPMA_OBJECT_FIXEDTPM | TPMA_OBJECT_FIXEDPARENT |
                TPMA_OBJECT_SENSITIVEDATAORIGIN | TPMA_OBJECT_USERWITHAUTH |
                TPMA_OBJECT_RESTRICTED | TPMA_OBJECT_SIGN_ENCRYPT,
            .authPolicy = { .size = 0 },
            .parameters.rsaDetail = {
                .symmetric = { .algorithm = TPM2_ALG_NULL },
                .scheme = {
                    .scheme = TPM2_ALG_RSAPSS,
                    .details.rsapss = { .hashAlg = TPM2_ALG_SHA256 },
                },
                .keyBits = 2048,
                .exponent = 0,
            },
            .unique.rsa = { .size = 0, .buffer = {0} }
        }
    };

    TPM2B_SENSITIVE_CREATE in_sensitive = { .size = 0 };
    TPM2B_DATA outside_info = { .size = 0 };
    TPML_PCR_SELECTION creation_pcr = { .count = 0 };

    ESYS_TR ak_tr = ESYS_TR_NONE;
    TPM2B_PUBLIC *out_public = NULL;
    TPM2B_CREATION_DATA *creation_data = NULL;
    TPM2B_DIGEST *creation_hash = NULL;
    TPMT_TK_CREATION *creation_ticket = NULL;

    TSS2_RC rc = Esys_CreatePrimary(g_esys_ctx,
                                    ESYS_TR_RH_OWNER,
                                    ESYS_TR_PASSWORD, ESYS_TR_NONE, ESYS_TR_NONE,
                                    &in_sensitive, &in_public,
                                    &outside_info, &creation_pcr,
                                    &ak_tr, &out_public,
                                    &creation_data, &creation_hash,
                                    &creation_ticket);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_CreatePrimary(AK)", rc);
    }

    TPM2_HANDLE tpm_handle = 0;
    rc = Esys_TR_GetTpmHandle(g_esys_ctx, ak_tr, &tpm_handle);
    if (rc != TSS2_RC_SUCCESS) {
        Esys_FlushContext(g_esys_ctx, ak_tr);
        if (out_public) Esys_Free(out_public);
        if (creation_data) Esys_Free(creation_data);
        if (creation_hash) Esys_Free(creation_hash);
        if (creation_ticket) Esys_Free(creation_ticket);
        return lapee_make_tss_error(env, "Esys_TR_GetTpmHandle(AK)", rc);
    }

    unsigned char *pem = NULL; size_t pem_len = 0;
    if (lapee_tpm2b_public_to_pem(out_public, &pem, &pem_len) != 0) {
        Esys_FlushContext(g_esys_ctx, ak_tr);
        if (out_public) Esys_Free(out_public);
        if (creation_data) Esys_Free(creation_data);
        if (creation_hash) Esys_Free(creation_hash);
        if (creation_ticket) Esys_Free(creation_ticket);
        return lapee_make_error(env, "pem_encode_failed");
    }

    unsigned char *marshalled = NULL; size_t marshalled_len = 0;
    if (lapee_marshal_public(out_public, &marshalled, &marshalled_len) != 0) {
        enif_free(pem);
        Esys_FlushContext(g_esys_ctx, ak_tr);
        if (out_public) Esys_Free(out_public);
        if (creation_data) Esys_Free(creation_data);
        if (creation_hash) Esys_Free(creation_hash);
        if (creation_ticket) Esys_Free(creation_ticket);
        return lapee_make_error(env, "marshal_failed");
    }

    ERL_NIF_TERM pem_term, mb_term;
    unsigned char *pem_out = enif_make_new_binary(env, pem_len, &pem_term);
    memcpy(pem_out, pem, pem_len);
    unsigned char *mb_out = enif_make_new_binary(env, marshalled_len, &mb_term);
    memcpy(mb_out, marshalled, marshalled_len);
    enif_free(pem);
    enif_free(marshalled);

    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map,
                      enif_make_atom(env, "handle"),
                      enif_make_uint(env, tpm_handle), &map);
    enif_make_map_put(env, map,
                      enif_make_atom(env, "esys_tr"),
                      enif_make_uint(env, ak_tr), &map);
    enif_make_map_put(env, map,
                      enif_make_atom(env, "public_pem"),
                      pem_term, &map);
    enif_make_map_put(env, map,
                      enif_make_atom(env, "tpm2b_public"),
                      mb_term, &map);

    if (out_public) Esys_Free(out_public);
    if (creation_data) Esys_Free(creation_data);
    if (creation_hash) Esys_Free(creation_hash);
    if (creation_ticket) Esys_Free(creation_ticket);

    (void)parent_handle;
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), map);
}

/*-------------------------------- quote/3 -----------------------------------*/

static ERL_NIF_TERM
nif_quote(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    unsigned esys_tr;
    if (!enif_get_uint(env, argv[0], &esys_tr)) return enif_make_badarg(env);

    /* PCR list -> selection (SHA-256 bank). */
    ERL_NIF_TERM list = argv[1], head, tail = list;
    TPML_PCR_SELECTION sel = {
        .count = 1,
        .pcrSelections = {
            {
                .hash = TPM2_ALG_SHA256,
                .sizeofSelect = 3,
                .pcrSelect = {0, 0, 0},
            }
        }
    };
    int have_any = 0;
    int pcr_indices[24]; int pcr_count = 0;
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        int i;
        if (!enif_get_int(env, head, &i) || i < 0 || i > 23)
            return enif_make_badarg(env);
        sel.pcrSelections[0].pcrSelect[i / 8] |= (1 << (i % 8));
        pcr_indices[pcr_count++] = i;
        have_any = 1;
    }
    if (!have_any) return enif_make_badarg(env);

    ErlNifBinary nonce;
    if (!enif_inspect_binary(env, argv[2], &nonce)) return enif_make_badarg(env);
    if (nonce.size > sizeof(((TPM2B_DATA *)0)->buffer)) return enif_make_badarg(env);

    TPM2B_DATA qual = { .size = (UINT16)nonce.size };
    memcpy(qual.buffer, nonce.data, nonce.size);

    TPMT_SIG_SCHEME scheme = {
        .scheme = TPM2_ALG_RSAPSS,
        .details.rsapss.hashAlg = TPM2_ALG_SHA256,
    };

    TPM2B_ATTEST *quoted = NULL;
    TPMT_SIGNATURE *signature = NULL;

    TSS2_RC rc = Esys_Quote(g_esys_ctx,
                            (ESYS_TR)esys_tr,
                            ESYS_TR_PASSWORD, ESYS_TR_NONE, ESYS_TR_NONE,
                            &qual, &scheme, &sel,
                            &quoted, &signature);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_Quote", rc);
    }

    ERL_NIF_TERM quoted_term;
    unsigned char *q_out = enif_make_new_binary(env, quoted->size, &quoted_term);
    memcpy(q_out, quoted->attestationData, quoted->size);

    /* Extract the raw RSA PSS signature bytes. */
    ERL_NIF_TERM sig_term;
    if (signature->sigAlg == TPM2_ALG_RSAPSS) {
        unsigned char *s_out = enif_make_new_binary(
            env, signature->signature.rsapss.sig.size, &sig_term);
        memcpy(s_out, signature->signature.rsapss.sig.buffer,
               signature->signature.rsapss.sig.size);
    } else if (signature->sigAlg == TPM2_ALG_RSASSA) {
        unsigned char *s_out = enif_make_new_binary(
            env, signature->signature.rsassa.sig.size, &sig_term);
        memcpy(s_out, signature->signature.rsassa.sig.buffer,
               signature->signature.rsassa.sig.size);
    } else {
        Esys_Free(quoted); Esys_Free(signature);
        return lapee_make_error(env, "unknown_sig_alg");
    }

    /* Also marshal the full TPMT_SIGNATURE so callers can feed it to
     * tpm2_checkquote, which expects the marshalled form. */
    size_t sig_marshal_size = 0;
    TSS2_RC mrc = Tss2_MU_TPMT_SIGNATURE_Marshal(signature, NULL, 1024,
                                                 &sig_marshal_size);
    ERL_NIF_TERM sig_marshal_term = enif_make_atom(env, "undefined");
    if (mrc == TSS2_RC_SUCCESS && sig_marshal_size > 0) {
        unsigned char *tmp = enif_alloc(sig_marshal_size);
        size_t off = 0;
        if (Tss2_MU_TPMT_SIGNATURE_Marshal(signature, tmp, sig_marshal_size, &off)
                == TSS2_RC_SUCCESS) {
            unsigned char *m_out = enif_make_new_binary(env, off, &sig_marshal_term);
            memcpy(m_out, tmp, off);
        }
        enif_free(tmp);
    }

    /* Read the PCR values too so we can build a pcrs.txt for tpm2_checkquote. */
    UINT32 uc; TPML_PCR_SELECTION *out_sel = NULL; TPML_DIGEST *digests = NULL;
    rc = Esys_PCR_Read(g_esys_ctx,
                       ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE,
                       &sel, &uc, &out_sel, &digests);
    ERL_NIF_TERM pcrs_map = enif_make_new_map(env);
    if (rc == TSS2_RC_SUCCESS && digests) {
        for (int i = 0; i < (int)digests->count && i < pcr_count; i++) {
            ERL_NIF_TERM val;
            unsigned char *d = enif_make_new_binary(
                env, digests->digests[i].size, &val);
            memcpy(d, digests->digests[i].buffer, digests->digests[i].size);
            enif_make_map_put(env, pcrs_map,
                              enif_make_int(env, pcr_indices[i]),
                              val, &pcrs_map);
        }
    }
    if (out_sel) Esys_Free(out_sel);
    if (digests) Esys_Free(digests);

    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map, enif_make_atom(env, "quoted"), quoted_term, &map);
    enif_make_map_put(env, map, enif_make_atom(env, "signature"), sig_term, &map);
    enif_make_map_put(env, map, enif_make_atom(env, "signature_marshalled"),
                      sig_marshal_term, &map);
    enif_make_map_put(env, map, enif_make_atom(env, "pcr_values"), pcrs_map, &map);

    Esys_Free(quoted); Esys_Free(signature);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), map);
}

/*-------------------------------- sign/2 ------------------------------------*/

static ERL_NIF_TERM
nif_sign(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    unsigned esys_tr;
    ErlNifBinary msg;
    if (!enif_get_uint(env, argv[0], &esys_tr)) return enif_make_badarg(env);
    if (!enif_inspect_binary(env, argv[1], &msg)) return enif_make_badarg(env);

    /* Restricted signing keys cannot sign arbitrary data unless it comes with
     * a hash ticket proving the TPM computed it. For our milestone we use
     * Esys_Hash with TPM_RH_OWNER to get the ticket, then pass that to Sign. */
    TPM2B_MAX_BUFFER data = { .size = 0 };
    if (msg.size > sizeof(data.buffer)) return lapee_make_error(env, "message_too_large");
    data.size = (UINT16)msg.size;
    memcpy(data.buffer, msg.data, msg.size);

    TPM2B_DIGEST *digest = NULL;
    TPMT_TK_HASHCHECK *validation = NULL;
    TSS2_RC rc = Esys_Hash(g_esys_ctx,
                           ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE,
                           &data, TPM2_ALG_SHA256, TPM2_RH_OWNER,
                           &digest, &validation);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_Hash", rc);
    }

    TPMT_SIG_SCHEME scheme = {
        .scheme = TPM2_ALG_RSAPSS,
        .details.rsapss.hashAlg = TPM2_ALG_SHA256,
    };

    TPMT_SIGNATURE *sig = NULL;
    rc = Esys_Sign(g_esys_ctx,
                   (ESYS_TR)esys_tr,
                   ESYS_TR_PASSWORD, ESYS_TR_NONE, ESYS_TR_NONE,
                   digest, &scheme, validation, &sig);
    Esys_Free(digest);
    Esys_Free(validation);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_Sign", rc);
    }
    ERL_NIF_TERM out;
    if (sig->sigAlg == TPM2_ALG_RSAPSS) {
        unsigned char *b = enif_make_new_binary(
            env, sig->signature.rsapss.sig.size, &out);
        memcpy(b, sig->signature.rsapss.sig.buffer,
               sig->signature.rsapss.sig.size);
    } else {
        Esys_Free(sig);
        return lapee_make_error(env, "unexpected_sig_alg");
    }
    Esys_Free(sig);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), out);
}

/*-------------------------------- flush_context/1 ---------------------------*/

static ERL_NIF_TERM
nif_flush_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    unsigned esys_tr;
    if (!enif_get_uint(env, argv[0], &esys_tr)) return enif_make_badarg(env);
    TSS2_RC rc = Esys_FlushContext(g_esys_ctx, (ESYS_TR)esys_tr);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_FlushContext", rc);
    }
    return enif_make_atom(env, "ok");
}

/*-------------------------------- set_tcti/1 --------------------------------*/

static ERL_NIF_TERM
nif_set_tcti(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    char buf[512];
    if (enif_get_string(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1) <= 0)
        return enif_make_badarg(env);
    /* Re-init TCTI + ESYS. */
    if (g_esys_ctx) { Esys_Finalize(&g_esys_ctx); g_esys_ctx = NULL; }
    if (g_tcti_ctx) { Tss2_TctiLdr_Finalize(&g_tcti_ctx); g_tcti_ctx = NULL; }
    memcpy(g_tcti_conf, buf, sizeof(g_tcti_conf));
    TSS2_RC rc = Tss2_TctiLdr_Initialize(g_tcti_conf, &g_tcti_ctx);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Tss2_TctiLdr_Initialize", rc);
    }
    rc = Esys_Initialize(&g_esys_ctx, g_tcti_ctx, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_Initialize", rc);
    }
    return enif_make_atom(env, "ok");
}

/*-------------------------------- NIF table ---------------------------------*/

static ErlNifFunc nif_funcs[] = {
    {"startup", 0, nif_startup, 0},
    {"pcr_read", 1, nif_pcr_read, 0},
    {"pcr_extend", 2, nif_pcr_extend, 0},
    {"create_primary_ek", 0, nif_create_primary_ek, 0},
    {"create_signing_key", 1, nif_create_signing_key, 0},
    {"quote", 3, nif_quote, 0},
    {"sign", 2, nif_sign, 0},
    {"flush_context", 1, nif_flush_context, 0},
    {"set_tcti", 1, nif_set_tcti, 0}
};

ERL_NIF_INIT(lapee_tpm_nif, nif_funcs, do_load, NULL, NULL, do_unload)
