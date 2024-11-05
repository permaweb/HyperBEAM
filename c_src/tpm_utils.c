#include "tpm_utils.h"
#include <tss2/tss2_esys.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

ERL_NIF_TERM tpm_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ESYS_CONTEXT *ctx;
    TSS2_RC rc = Esys_Initialize(&ctx, NULL, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        return enif_make_string(env, "Error initializing TPM context", ERL_NIF_LATIN1);
    }

    // Example operation: reading TPM capabilities
    TPM2_CAP capability = TPM2_CAP_TPM_PROPERTIES;
    UINT32 property = TPM2_PT_MANUFACTURER;
    UINT32 property_count = 1;
    TPMS_CAPABILITY_DATA *capability_data;

    rc = Esys_GetCapability(ctx, ESYS_TR_NONE, ESYS_TR_NONE, ESYS_TR_NONE,
                            capability, property, property_count, NULL, &capability_data);
    if (rc != TSS2_RC_SUCCESS) {
        Esys_Finalize(&ctx);
        return enif_make_string(env, "Error getting TPM capability", ERL_NIF_LATIN1);
    }

    // Extract information (for demonstration, assume manufacturer ID is an integer)
    uint32_t manufacturer_id = capability_data->data.tpmProperties.tpmProperty[0].value;
    Esys_Free(capability_data);
    Esys_Finalize(&ctx);

    // Convert the manufacturer ID to a string and return it to Erlang
    char info_string[64];
    snprintf(info_string, sizeof(info_string), "TPM Manufacturer ID: %u", manufacturer_id);
    return enif_make_string(env, info_string, ERL_NIF_LATIN1);
}


ERL_NIF_TERM tpm_attest(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ESYS_CONTEXT *ctx;
    TSS2_RC rc = Esys_Initialize(&ctx, NULL, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        return enif_make_string(env, "Error initializing TPM context", ERL_NIF_LATIN1);
    }

    // 1. Define a primary RSA signing key template.
    TPM2B_PUBLIC primaryKeyTemplate = {
        .publicArea = {
            .type = TPM2_ALG_RSA,
            .nameAlg = TPM2_ALG_SHA256,
            .objectAttributes = (TPMA_OBJECT_USERWITHAUTH | TPMA_OBJECT_SIGN_ENCRYPT | 
                                 TPMA_OBJECT_FIXEDTPM | TPMA_OBJECT_FIXEDPARENT | 
                                 TPMA_OBJECT_SENSITIVEDATAORIGIN),
            .authPolicy = {.size = 0},
            .parameters.rsaDetail = {
                .symmetric = {.algorithm = TPM2_ALG_NULL},
                .scheme = {.scheme = TPM2_ALG_NULL},  // No specific scheme in primary key creation
                .keyBits = 2048,
                .exponent = 0
            },
            .unique.rsa = {.size = 0}
        }
    };

    // 2. Create the primary RSA signing key.
    ESYS_TR primaryHandle;
    rc = Esys_CreatePrimary(ctx, ESYS_TR_RH_ENDORSEMENT, ESYS_TR_PASSWORD, 
                            ESYS_TR_NONE, ESYS_TR_NONE, NULL, &primaryKeyTemplate, 
                            NULL, NULL, &primaryHandle, NULL, NULL, NULL, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        Esys_Finalize(&ctx);
        return enif_make_string(env, "Error creating primary key", ERL_NIF_LATIN1);
    }

    // 3. Define the attestation key (AK) with required attributes.
    TPM2B_PUBLIC akTemplate = {
        .publicArea = {
            .type = TPM2_ALG_RSA,
            .nameAlg = TPM2_ALG_SHA256,
            .objectAttributes = (TPMA_OBJECT_USERWITHAUTH | TPMA_OBJECT_SIGN_ENCRYPT | 
                                 TPMA_OBJECT_FIXEDTPM | TPMA_OBJECT_FIXEDPARENT | 
                                 TPMA_OBJECT_SENSITIVEDATAORIGIN),
            .authPolicy = {.size = 0},
            .parameters.rsaDetail = {
                .symmetric = {.algorithm = TPM2_ALG_NULL},
                .scheme = {.scheme = TPM2_ALG_NULL}, // Scheme handled in `Esys_Quote`
                .keyBits = 2048,
                .exponent = 0
            },
            .unique.rsa = {.size = 0}
        }
    };

    // 4. Define the sensitive data structure for the AK.
    TPM2B_SENSITIVE_CREATE inSensitive = {
        .size = 0,
        .sensitive = {
            .userAuth = {.size = 0},
            .data = {.size = 0}
        }
    };

    // 5. Generate the attestation key (AK).
    TPM2B_PRIVATE *outPrivate = NULL;
    TPM2B_PUBLIC *outPublic = NULL;
    ESYS_TR akHandle;
    rc = Esys_Create(ctx, primaryHandle, ESYS_TR_PASSWORD, ESYS_TR_NONE, ESYS_TR_NONE,
                     &inSensitive, &akTemplate, NULL, NULL, &outPrivate, &outPublic, NULL, NULL, NULL);
    if (rc != TSS2_RC_SUCCESS || !outPrivate || !outPublic) {
        if (outPrivate) Esys_Free(outPrivate);
        if (outPublic) Esys_Free(outPublic);
        Esys_FlushContext(ctx, primaryHandle);
        Esys_Finalize(&ctx);
        return enif_make_string(env, "Error creating attestation key", ERL_NIF_LATIN1);
    }

    // 6. Load the attestation key (AK) under the primary key.
    rc = Esys_Load(ctx, primaryHandle, ESYS_TR_PASSWORD, ESYS_TR_NONE, ESYS_TR_NONE, 
                   outPrivate, outPublic, &akHandle);
    Esys_Free(outPrivate);
    Esys_Free(outPublic);
    if (rc != TSS2_RC_SUCCESS) {
        Esys_FlushContext(ctx, primaryHandle);
        Esys_Finalize(&ctx);
        return enif_make_string(env, "Error loading attestation key", ERL_NIF_LATIN1);
    }

    // Define the nonce for attestation
    uint8_t nonce[20] = {0x00};
    for (int i = 0; i < sizeof(nonce); i++) {
        nonce[i] = rand() % 256;  // Create a random nonce
    }
    TPM2B_DATA tpmNonce = {.size = sizeof(nonce)};
    memcpy(tpmNonce.buffer, nonce, sizeof(nonce));

    // PCR selection, e.g., PCR 0
    TPML_PCR_SELECTION pcrSelect = {.count = 1};
    pcrSelect.pcrSelections[0].hash = TPM2_ALG_SHA256;
    pcrSelect.pcrSelections[0].sizeofSelect = 3;
    pcrSelect.pcrSelections[0].pcrSelect[0] = 0x01;  // Selecting PCR 0

    // Define the signature scheme
    TPMT_SIG_SCHEME inScheme;
    inScheme.scheme = TPM2_ALG_RSASSA;
    inScheme.details.rsassa.hashAlg = TPM2_ALG_SHA256;

    // Call Esys_Quote to generate the attestation quote
    TPM2B_ATTEST *quoted = NULL;
    TPMT_SIGNATURE *signature = NULL;
    rc = Esys_Quote(ctx, akHandle, ESYS_TR_PASSWORD, ESYS_TR_NONE, ESYS_TR_NONE,
                    &tpmNonce, &inScheme, &pcrSelect, &quoted, &signature);
    if (rc != TSS2_RC_SUCCESS || !quoted) {
        if (quoted) Esys_Free(quoted);
        if (signature) Esys_Free(signature);
        Esys_FlushContext(ctx, primaryHandle);
        Esys_FlushContext(ctx, akHandle);
        Esys_Finalize(&ctx);
        return enif_make_string(env, "Error generating TPM quote", ERL_NIF_LATIN1);
    }

    // Allocate an ErlNifBinary for the attestation data
    ErlNifBinary result_bin;
    if (!enif_alloc_binary(quoted->size, &result_bin)) {
        Esys_Free(quoted);
        Esys_Free(signature);
        Esys_FlushContext(ctx, primaryHandle);
        Esys_FlushContext(ctx, akHandle);
        Esys_Finalize(&ctx);
        return enif_make_string(env, "Failed to allocate binary for attestation data", ERL_NIF_LATIN1);
    }
    memcpy(result_bin.data, quoted->attestationData, quoted->size);

    // Wrap the binary in an ERL_NIF_TERM to return to Erlang
    ERL_NIF_TERM result = enif_make_binary(env, &result_bin);

    // Clean up
    Esys_Free(quoted);
    Esys_Free(signature);
    Esys_FlushContext(ctx, primaryHandle);
    Esys_FlushContext(ctx, akHandle);
    Esys_Finalize(&ctx);

    return result;
}
