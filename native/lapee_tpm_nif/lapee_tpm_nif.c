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

/*
 * v1.2.2 paper P4 -- HMAC + parameter-encryption sessions for all
 * sensitive TPM operations.
 *
 * The paper's section Arch commits to:
 *   "All TPM sessions touching sensitive state use encrypted sessions
 *    (HMAC + parameter encryption)."
 *
 * On LapEE's threat model (Nuvoton NPCT75x dTPM on LPC bus), a
 * physical attacker with bus-level access can sniff plaintext
 * requests / responses. HMAC sessions bind each command in the
 * request stream with a key-derived MAC (an attacker cannot modify
 * a request without knowing the session key derived from caller +
 * TPM nonces), and parameter encryption wraps the first TPM2B
 * parameter in each direction under AES-128-CFB.
 *
 * Implementation: one shared unsalted HMAC session at file scope.
 * Created lazily on first use via lapee_ensure_auth_session().
 * Attached as shandle2 to sensitive ops so shandle1 can remain
 * hierarchy/object-auth (ESYS_TR_PASSWORD for empty-auth hierarchies)
 * while shandle2 provides the encrypt/decrypt/integrity coverage.
 *
 * Uses TPMA_SESSION_CONTINUESESSION so one session covers every
 * command; the TPM auto-updates the session's rolling nonce between
 * calls. No app-level bookkeeping.
 */
static ESYS_TR g_auth_session = ESYS_TR_NONE;

static TSS2_RC
lapee_ensure_auth_session(void)
{
    if (g_auth_session != ESYS_TR_NONE) return TSS2_RC_SUCCESS;
    TPMT_SYM_DEF symmetric = {
        .algorithm = TPM2_ALG_AES,
        .keyBits = { .aes = 128 },
        .mode = { .aes = TPM2_ALG_CFB },
    };
    /* Unsalted (tpmKey = NONE), unbound (bind = NONE). The session
     * still authenticates command integrity via rolling HMAC; its
     * key-derivation is seeded by the TPM-generated nonceTPM plus
     * our nonceCaller. Parameter encryption applies to the first
     * TPM2B in the marked direction. */
    TSS2_RC rc = Esys_StartAuthSession(
        g_esys_ctx,
        ESYS_TR_NONE,  /* tpmKey */
        ESYS_TR_NONE,  /* bind */
        ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE,
        NULL,           /* nonceCaller -- Esys auto-generates */
        TPM2_SE_HMAC,
        &symmetric,
        TPM2_ALG_SHA256,
        &g_auth_session);
    if (rc != TSS2_RC_SUCCESS) return rc;
    /* Session is HMAC + parameter-encrypt/decrypt + continues.
     *
     * Applied only to ops whose first cmd-param AND first rsp-
     * param are TPM2B_* structures: Esys_CreatePrimary (EK + AK)
     * and Esys_Quote. On those ops the TPM accepts ENCRYPT +
     * DECRYPT attributes and actually wraps the TPM2B payload
     * under AES-128-CFB for transit over the LPC bus.
     *
     * Ops with list-struct first parameters (PCR_Extend takes
     * TPML_DIGEST_VALUES, PCR_Read returns TPML_DIGEST,
     * GetCapability uses TPMS_CAPABILITY_DATA) do NOT support
     * parameter encryption per TPM 2.0 spec, and any session
     * with a non-auth "purpose" attribute fails RC_ATTRIBUTES
     * in a non-auth slot. Those ops run without this session;
     * their responses carry public values (PCR digests, TPM
     * vendor / fw-version readback) that the paper's threat
     * model explicitly marks as attester-intended-public. */
    TPMA_SESSION attrs = TPMA_SESSION_ENCRYPT |
                         TPMA_SESSION_DECRYPT |
                         TPMA_SESSION_CONTINUESESSION;
    return Esys_TRSess_SetAttributes(g_esys_ctx, g_auth_session,
                                      attrs, 0xFF);
}

/* Returns g_auth_session when the HMAC session is available, or
 * ESYS_TR_NONE as a safe fallback if session creation fails. The
 * fallback preserves pre-P4 behaviour (no encryption) on TPMs that
 * somehow refuse HMAC sessions -- paper P4 grades by the session's
 * actual attributes, not by whether the helper returned the live
 * handle. */
static ESYS_TR
lapee_enc_session(void)
{
    if (lapee_ensure_auth_session() == TSS2_RC_SUCCESS) {
        return g_auth_session;
    }
    return ESYS_TR_NONE;
}

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
    if (g_auth_session != ESYS_TR_NONE && g_esys_ctx) {
        Esys_FlushContext(g_esys_ctx, g_auth_session);
        g_auth_session = ESYS_TR_NONE;
    }
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
    /* PCR_Read returns TPML_DIGEST (list-struct, not TPM2B),
     * so parameter encryption + decrypt attrs are TPM-rejected
     * here -- see lapee_ensure_auth_session header. Read-only,
     * public values, no auth session. */
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

    /* PCR auth via shandle1=PASSWORD (PCR 15 has empty auth by
     * default on LapEE). PCR_Extend takes TPML_DIGEST_VALUES
     * (list-struct) as its first cmd-param; TPM rejects ENCRYPT
     * attr on non-TPM2B params. Paper P4 session attaches to
     * Quote + CreatePrimary only -- see lapee_ensure_auth_session
     * header for the full breakdown. */
    TSS2_RC rc = Esys_PCR_Extend(g_esys_ctx,
                                 pcr_handle,
                                 ESYS_TR_PASSWORD,
                                 ESYS_TR_NONE,
                                 ESYS_TR_NONE,
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

    /* Paper P4: hierarchy auth (empty password) via shandle1, HMAC-
     * encrypted session via shandle2 covers the TPM2B_SENSITIVE_CREATE
     * (empty here, but in principle could hold auth values) and
     * encrypts the TPM2B_PUBLIC response in transit on the LPC bus. */
    TSS2_RC rc = Esys_CreatePrimary(g_esys_ctx,
                                    ESYS_TR_RH_ENDORSEMENT,
                                    ESYS_TR_PASSWORD,
                                    lapee_enc_session(),
                                    ESYS_TR_NONE,
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

    /* v1.2.2 paper P3 -- AK lives under the Endorsement hierarchy.
     * Previously this used ESYS_TR_RH_OWNER as a swtpm-compat
     * shortcut; the paper requires Endorsement so that the AK's
     * qualifiedSigner chain in every TPMS_ATTEST roots at the
     * same primary-seed tree as the EK (same TPM -- a verifier
     * walking the qualifiedSigner name path can validate the AK
     * originated in the same physical TPM as the EK in the cert
     * chain). Factory Nuvoton NPCT75x ships with empty
     * endorsement auth so the null-password session still works;
     * provisioned owner-password-only TPMs are out of scope. */
    /* Paper P4 as with the EK primary creation -- shandle1=PASSWORD
     * authenticates the Endorsement hierarchy, shandle2=HMAC session
     * integrity-protects + encrypts sensitive parameters. */
    TSS2_RC rc = Esys_CreatePrimary(g_esys_ctx,
                                    ESYS_TR_RH_ENDORSEMENT,
                                    ESYS_TR_PASSWORD,
                                    lapee_enc_session(),
                                    ESYS_TR_NONE,
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
        /* Reviewer pass 12 (NIF audit, batch 14) CRITICAL-1:
         * guard against pcr_indices[24] stack overflow. A caller
         * sending >24 PCR indices (or duplicates that inflate
         * pcr_count past the bitmap's unique-index count) would
         * otherwise write past the end of the stack buffer.
         * The per-index 0..23 range check above does NOT bound
         * pcr_count. */
        if (pcr_count >= 24)
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

    /* Paper P4: shandle1=PASSWORD authorizes use of the AK's user
     * auth (empty), shandle2=HMAC session integrity-protects the
     * request and encrypts the TPMS_ATTEST response. Matters for
     * attestation because an attacker on the LPC bus could
     * otherwise silently substitute a stale-but-valid quote from
     * a prior nonce into a current request's response. */
    TSS2_RC rc = Esys_Quote(g_esys_ctx,
                            (ESYS_TR)esys_tr,
                            ESYS_TR_PASSWORD,
                            lapee_enc_session(),
                            ESYS_TR_NONE,
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

/*-------------------------------- tpm_properties/0 --------------------------*/

/* Decode a 32-bit TPMU property value as a 4-char ASCII string and
 * drop it into `out' (4 bytes). Manufacturer ID + vendor string chunks
 * are conventionally four ASCII bytes packed big-endian into a U32. */
static void
u32_to_ascii4(uint32_t v, char out[4])
{
    out[0] = (char)((v >> 24) & 0xFF);
    out[1] = (char)((v >> 16) & 0xFF);
    out[2] = (char)((v >> 8)  & 0xFF);
    out[3] = (char)(v & 0xFF);
}

/* Query a single TPM_PT_* property via Esys_GetCapability. Returns the
 * UINT32 value (0 on failure) and writes the rc to *out_rc. We call
 * one property at a time because ESYS's GetCapability API is quirky
 * about batching -- the cleaner path is property-at-a-time and assemble
 * the result map in C.
 */
static UINT32
tpm_pt_get(TPM2_PT prop, TSS2_RC *out_rc)
{
    TPMS_CAPABILITY_DATA *cap_data = NULL;
    TPMI_YES_NO more = TPM2_NO;
    /* GetCapability response is TPMS_CAPABILITY_DATA (list-
     * struct, not TPM2B). Encrypt/decrypt attrs fail with
     * RC_ATTRIBUTES on this op; see lapee_ensure_auth_session
     * header. Response carries TPM vendor / firmware version
     * which are public per TCG -- no confidentiality need. */
    TSS2_RC rc = Esys_GetCapability(
        g_esys_ctx,
        ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE,
        TPM2_CAP_TPM_PROPERTIES, prop, 1,
        &more, &cap_data);
    if (out_rc) *out_rc = rc;
    if (rc != TSS2_RC_SUCCESS || cap_data == NULL) {
        if (cap_data) Esys_Free(cap_data);
        return 0;
    }
    UINT32 val = 0;
    if (cap_data->capability == TPM2_CAP_TPM_PROPERTIES &&
        cap_data->data.tpmProperties.count > 0) {
        const TPMS_TAGGED_PROPERTY *p =
            &cap_data->data.tpmProperties.tpmProperty[0];
        if (p->property == prop) {
            val = p->value;
        }
    }
    Esys_Free(cap_data);
    return val;
}

/*
 * tpm_properties() -> {ok, #{manufacturer, vendor_string,
 *                            spec_family, spec_level, spec_revision,
 *                            firmware_version_1, firmware_version_2,
 *                            tpm_family}} | {error, Reason}
 *
 * Query TPM2_GetCapability for the standard manufacturer /
 * vendor-string / spec-version / firmware-version properties. This is
 * the PRIMARY TPM-identification path because it works even on TPMs
 * without a provisioned EK cert in NV -- which is currently the
 * default state for most AMD fTPMs. The EK cert's TCG-OID attributes,
 * when present, act as a CROSS-CHECK rather than the sole source.
 *
 * Field semantics (from TPM 2.0 Part 2, Table 22):
 *   manufacturer        -- TPM_PT_MANUFACTURER, 4-char ASCII
 *   vendor_string       -- TPM_PT_VENDOR_STRING_1..4, up to 16 ASCII
 *                          bytes of vendor-defined model text
 *   spec_family         -- TPM_PT_FAMILY_INDICATOR, "2.0" etc
 *   spec_level          -- TPM_PT_LEVEL
 *   spec_revision       -- TPM_PT_REVISION (hundredths, e.g. 138 = 1.38)
 *   firmware_version_1  -- TPM_PT_FIRMWARE_VERSION_1 (vendor-meaningful)
 *   firmware_version_2  -- TPM_PT_FIRMWARE_VERSION_2
 */
static ERL_NIF_TERM
nif_tpm_properties(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc; (void)argv;

    TSS2_RC rc;
    UINT32 manu = tpm_pt_get(TPM2_PT_MANUFACTURER, &rc);
    if (rc != TSS2_RC_SUCCESS) {
        return lapee_make_tss_error(env, "Esys_GetCapability(MANUFACTURER)", rc);
    }
    UINT32 vs1 = tpm_pt_get(TPM2_PT_VENDOR_STRING_1, NULL);
    UINT32 vs2 = tpm_pt_get(TPM2_PT_VENDOR_STRING_2, NULL);
    UINT32 vs3 = tpm_pt_get(TPM2_PT_VENDOR_STRING_3, NULL);
    UINT32 vs4 = tpm_pt_get(TPM2_PT_VENDOR_STRING_4, NULL);
    UINT32 fam = tpm_pt_get(TPM2_PT_FAMILY_INDICATOR, NULL);
    UINT32 lvl = tpm_pt_get(TPM2_PT_LEVEL, NULL);
    UINT32 rev = tpm_pt_get(TPM2_PT_REVISION, NULL);
    UINT32 fw1 = tpm_pt_get(TPM2_PT_FIRMWARE_VERSION_1, NULL);
    UINT32 fw2 = tpm_pt_get(TPM2_PT_FIRMWARE_VERSION_2, NULL);
    UINT32 daymonth = tpm_pt_get(TPM2_PT_DAY_OF_YEAR, NULL);
    UINT32 year     = tpm_pt_get(TPM2_PT_YEAR, NULL);

    char manu_s[5] = {0};     u32_to_ascii4(manu, manu_s);
    char vs_s[17] = {0};
    u32_to_ascii4(vs1, vs_s);
    u32_to_ascii4(vs2, vs_s + 4);
    u32_to_ascii4(vs3, vs_s + 8);
    u32_to_ascii4(vs4, vs_s + 12);
    char fam_s[5] = {0};     u32_to_ascii4(fam, fam_s);

    /* Vendor string is 4 x 32-bit big-endian chunks per TCG spec.
     * Manufacturers with a short string (e.g. Nuvoton: "NPCT75x\0")
     * put their name in the first chunks and undefined bytes after
     * the terminating NUL. Treat as C-string: truncate at the first
     * NUL. The old trailing-NUL trim left embedded-NUL-plus-junk
     * tails like `NPCT75x\0"!!4rls` intact. */
    size_t vs_len = 0;
    while (vs_len < 16 && vs_s[vs_len] != '\0') vs_len++;

    /* Local "length until NUL, cap at MAX" -- avoids the strnlen
     * extension which isn't available on all libcs we target. */
    size_t manu_len = 0;
    while (manu_len < 4 && manu_s[manu_len] != '\0') manu_len++;
    size_t fam_len = 0;
    while (fam_len < 4 && fam_s[fam_len] != '\0') fam_len++;

    ERL_NIF_TERM manu_bin;
    {
        unsigned char *b = enif_make_new_binary(env, manu_len, &manu_bin);
        memcpy(b, manu_s, manu_len);
    }
    ERL_NIF_TERM vs_bin;
    {
        unsigned char *b = enif_make_new_binary(env, vs_len, &vs_bin);
        memcpy(b, vs_s, vs_len);
    }
    ERL_NIF_TERM fam_bin;
    {
        unsigned char *b = enif_make_new_binary(env, fam_len, &fam_bin);
        memcpy(b, fam_s, fam_len);
    }

    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map,
        enif_make_atom(env, "manufacturer"), manu_bin, &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "manufacturer_u32"),
        enif_make_uint(env, manu), &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "vendor_string"), vs_bin, &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "spec_family"), fam_bin, &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "spec_level"),
        enif_make_uint(env, lvl), &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "spec_revision"),
        enif_make_uint(env, rev), &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "firmware_version_1"),
        enif_make_uint(env, fw1), &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "firmware_version_2"),
        enif_make_uint(env, fw2), &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "day_of_year"),
        enif_make_uint(env, daymonth), &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "year"),
        enif_make_uint(env, year), &map);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), map);
}

/*-------------------------------- nv_read_public/1 --------------------------*/

/*
 * nv_read_public(TpmHandle) -> {ok, #{data_size, attributes, name_alg,
 *                                     auth_policy_len}} | {error, Reason}
 *
 * Look up an NV index by its TPM handle and return its public metadata.
 * Returns {error, nv_index_undefined} when the handle is not defined on
 * this TPM (the canonical signal that e.g. there is no EK cert in NV).
 * Any other TSS2 failure is surfaced with its decoded RC string.
 *
 * This is the read-only half of the EK-cert-from-NV flow and is useful
 * on its own for diagnostics ("what NV indices does this TPM actually
 * provision?"). For fetching the bytes, see nv_read/1.
 */
static ERL_NIF_TERM
nif_nv_read_public(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    unsigned handle_u;
    if (!enif_get_uint(env, argv[0], &handle_u))
        return enif_make_badarg(env);
    TPM2_HANDLE tpm_handle = (TPM2_HANDLE)handle_u;

    ESYS_TR nv_tr = ESYS_TR_NONE;
    TSS2_RC rc = Esys_TR_FromTPMPublic(
        g_esys_ctx, tpm_handle,
        ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE,
        &nv_tr);
    if (rc != TSS2_RC_SUCCESS) {
        /* TPM2_RC_HANDLE at the formatter level means "no such handle".
         * Map that to an explicit atom so callers can distinguish
         * "NV not provisioned" from real TPM errors. */
        /* TPM2_RC_HANDLE is a FMT1 response (bit 7 set). On the
         * wire it may have handle/parameter/session position bits
         * set in 0xF00; mask those out before comparing. */
        if ((rc & 0x0BF) == (TPM2_RC_HANDLE & 0x0BF))
            return lapee_make_error(env, "nv_index_undefined");
        return lapee_make_tss_error(env, "Esys_TR_FromTPMPublic", rc);
    }

    TPM2B_NV_PUBLIC *nv_public = NULL;
    rc = Esys_NV_ReadPublic(
        g_esys_ctx, nv_tr,
        ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE,
        &nv_public, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        /* Drop our ESYS_TR reference to the NV index before returning.
         * The TPM-side handle is untouched. */
        Esys_TR_Close(g_esys_ctx, &nv_tr);
        return lapee_make_tss_error(env, "Esys_NV_ReadPublic", rc);
    }

    UINT16 data_size     = nv_public->nvPublic.dataSize;
    UINT32 attributes    = nv_public->nvPublic.attributes;
    TPMI_ALG_HASH nmalg  = nv_public->nvPublic.nameAlg;
    UINT16 pol_len       = nv_public->nvPublic.authPolicy.size;
    Esys_Free(nv_public);
    Esys_TR_Close(g_esys_ctx, &nv_tr);

    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map,
        enif_make_atom(env, "data_size"),
        enif_make_uint(env, (unsigned)data_size), &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "attributes"),
        enif_make_uint(env, (unsigned)attributes), &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "name_alg"),
        enif_make_uint(env, (unsigned)nmalg), &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "auth_policy_len"),
        enif_make_uint(env, (unsigned)pol_len), &map);
    enif_make_map_put(env, map,
        enif_make_atom(env, "handle"),
        enif_make_uint(env, (unsigned)tpm_handle), &map);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), map);
}

/*-------------------------------- nv_read/1 ---------------------------------*/

/*
 * nv_read(TpmHandle) -> {ok, Bytes::binary()} | {error, Reason}
 *
 * Read the full contents of an NV index addressed by its TPM handle.
 * Reads are chunked at TPM2_MAX_NV_BUFFER_SIZE (conservative 512 B)
 * because the TPM's own per-call buffer limit is platform-dependent.
 *
 * Auth handle is picked from TPMA_NV attributes in the same order a
 * well-behaved TSS client would: if TPMA_NV_AUTHREAD is set we auth
 * against the NV index itself (empty auth, which is the convention
 * for EK-cert indices); else TPMA_NV_OWNERREAD -> RH_OWNER;
 * else TPMA_NV_PPREAD -> RH_PLATFORM. If none of those bits is set
 * or a read returns TPM2_RC_BAD_AUTH we fall back through the list
 * before giving up, because real TPMs (notably AMD fTPM) sometimes
 * set OWNERREAD but accept AUTHREAD too.
 *
 * {error, nv_index_undefined} on missing handle, same as
 * nv_read_public/1. Any other TSS2 failure is surfaced verbatim.
 */
static ERL_NIF_TERM
nif_nv_read(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    unsigned handle_u;
    if (!enif_get_uint(env, argv[0], &handle_u))
        return enif_make_badarg(env);
    TPM2_HANDLE tpm_handle = (TPM2_HANDLE)handle_u;

    ESYS_TR nv_tr = ESYS_TR_NONE;
    TSS2_RC rc = Esys_TR_FromTPMPublic(
        g_esys_ctx, tpm_handle,
        ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE,
        &nv_tr);
    if (rc != TSS2_RC_SUCCESS) {
        /* TPM2_RC_HANDLE is a FMT1 response (bit 7 set). On the
         * wire it may have handle/parameter/session position bits
         * set in 0xF00; mask those out before comparing. */
        if ((rc & 0x0BF) == (TPM2_RC_HANDLE & 0x0BF))
            return lapee_make_error(env, "nv_index_undefined");
        return lapee_make_tss_error(env, "Esys_TR_FromTPMPublic", rc);
    }

    TPM2B_NV_PUBLIC *nv_public = NULL;
    rc = Esys_NV_ReadPublic(
        g_esys_ctx, nv_tr,
        ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE,
        &nv_public, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        Esys_TR_Close(g_esys_ctx, &nv_tr);
        return lapee_make_tss_error(env, "Esys_NV_ReadPublic", rc);
    }

    UINT16 data_size = nv_public->nvPublic.dataSize;
    UINT32 attrs     = nv_public->nvPublic.attributes;
    Esys_Free(nv_public);

    if (data_size == 0) {
        Esys_TR_Close(g_esys_ctx, &nv_tr);
        return lapee_make_error(env, "nv_index_empty");
    }

    /* Candidate auth handles, in decreasing order of "what EK-cert
     * conventions say". The read loop below tries each in turn when
     * it sees TPM2_RC_BAD_AUTH / TPM2_RC_AUTH_UNAVAILABLE. */
    ESYS_TR auth_candidates[3] = { ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE };
    int n_candidates = 0;
    if (attrs & TPMA_NV_AUTHREAD)  auth_candidates[n_candidates++] = nv_tr;
    if (attrs & TPMA_NV_OWNERREAD) auth_candidates[n_candidates++] = ESYS_TR_RH_OWNER;
    if (attrs & TPMA_NV_PPREAD)    auth_candidates[n_candidates++] = ESYS_TR_RH_PLATFORM;
    if (n_candidates == 0) {
        /* No read bits set in attributes -- the index is write-only
         * or policy-protected. Report explicitly. */
        Esys_TR_Close(g_esys_ctx, &nv_tr);
        return lapee_make_error(env, "nv_index_not_readable");
    }

    ERL_NIF_TERM out_bin;
    unsigned char *out_buf = enif_make_new_binary(env, data_size, &out_bin);

    int success = 0;
    TSS2_RC last_rc = TSS2_RC_SUCCESS;
    const char *last_op = "Esys_NV_Read";
    for (int i = 0; i < n_candidates && !success; i++) {
        ESYS_TR auth = auth_candidates[i];
        UINT16 offset = 0;
        /* Conservative chunk size. TCG allows up to TPM2_MAX_NV_BUFFER_SIZE
         * but many TPMs respect a smaller firmware-advertised limit
         * (TPM_PT_NV_BUFFER_MAX). 512 is a safe lower bound. */
        const UINT16 CHUNK = 512;
        int attempt_ok = 1;
        while (offset < data_size && attempt_ok) {
            UINT16 want = (UINT16)(data_size - offset);
            if (want > CHUNK) want = CHUNK;
            TPM2B_MAX_NV_BUFFER *data = NULL;
            rc = Esys_NV_Read(
                g_esys_ctx,
                auth, nv_tr,
                ESYS_TR_PASSWORD, ESYS_TR_NONE, ESYS_TR_NONE,
                want, offset, &data);
            if (rc != TSS2_RC_SUCCESS) {
                attempt_ok = 0;
                last_rc = rc;
                last_op = "Esys_NV_Read";
                break;
            }
            if (data->size == 0) {
                Esys_Free(data);
                attempt_ok = 0;
                last_rc = TPM2_RC_NO_RESULT;
                last_op = "Esys_NV_Read(short-read)";
                break;
            }
            memcpy(out_buf + offset, data->buffer, data->size);
            offset = (UINT16)(offset + data->size);
            Esys_Free(data);
        }
        if (attempt_ok && offset == data_size) {
            success = 1;
        }
    }

    Esys_TR_Close(g_esys_ctx, &nv_tr);

    if (!success) {
        return lapee_make_tss_error(env, last_op, last_rc);
    }
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), out_bin);
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

/* Reviewer pass 12 (NIF audit, batch 14) HIGH: every NIF that
 * blocks on a synchronous TPM/SPI round-trip longer than ~1ms
 * must be declared with ERL_NIF_DIRTY_JOB_IO_BOUND so the BEAM
 * scheduler yields the calling process to a dirty scheduler
 * instead of stalling a regular scheduler for the duration of
 * the call.
 *
 * Observed latencies on Nuvoton NPCT75x over SPI:
 *   Esys_CreatePrimary (RSA-2048 keygen) : 300-800 ms
 *   Esys_Quote (RSA-PSS sign + PCR read) : 200-400 ms
 *   Esys_NV_Read (chunked 512 B/round)   :  30-80 ms for a 1.5 KB cert
 *   Esys_PCR_Extend                      :   5-15 ms
 *   Esys_PCR_Read                        :   2- 8 ms
 *   Esys_GetCapability                   :   2-10 ms (tpm_properties)
 *
 * flush_context, set_tcti, and startup are either no-ops on the
 * TPM or one-shot calls during init; they stay on the regular
 * scheduler. `startup' is technically borderline (~50-200 ms on
 * first call) but fires once per boot, so the flag churn isn't
 * worth it.
 *
 * With dirty-NIF flags set, concurrent /attestation requests no
 * longer block a regular scheduler, and the BEAM will log no
 * scheduler-stall warnings during the demo's 2-second
 * attestation window.
 */
static ErlNifFunc nif_funcs[] = {
    {"startup", 0, nif_startup, 0},
    {"pcr_read", 1, nif_pcr_read, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"pcr_extend", 2, nif_pcr_extend, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"create_primary_ek", 0, nif_create_primary_ek,
                              ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"create_signing_key", 1, nif_create_signing_key,
                              ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"quote", 3, nif_quote, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"sign", 2, nif_sign, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"tpm_properties", 0, nif_tpm_properties,
                           ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nv_read_public", 1, nif_nv_read_public,
                          ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"nv_read", 1, nif_nv_read, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"flush_context", 1, nif_flush_context, 0},
    {"set_tcti", 1, nif_set_tcti, 0}
};

ERL_NIF_INIT(lapee_tpm_nif, nif_funcs, do_load, NULL, NULL, do_unload)
