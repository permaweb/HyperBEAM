/*
 * Copyright (c) 2024 HyperBEAM Contributors. All rights reserved.
 * SPDX-License-Identifier: Apache-2.0
 *
 * NIF bindings for NVIDIA GPU TEE Attestation using nvat C SDK.
 * 
 * This module provides Erlang bindings to the nvat library for:
 * - Collecting GPU attestation evidence
 * - Verifying GPU attestation evidence locally
 */

#include <erl_nif.h>
#include <nvat.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

/* SDK initialization state */
static int sdk_initialized = 0;
static pthread_mutex_t sdk_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Helper: create {ok, Result} tuple */
static ERL_NIF_TERM make_ok(ErlNifEnv* env, ERL_NIF_TERM result) {
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

/* Helper: create {error, Reason} tuple */
static ERL_NIF_TERM make_error(ErlNifEnv* env, const char* reason) {
    ERL_NIF_TERM reason_term;
    unsigned char* buf = enif_make_new_binary(env, strlen(reason), &reason_term);
    memcpy(buf, reason, strlen(reason));
    return enif_make_tuple2(env, enif_make_atom(env, "error"), reason_term);
}

/* Helper: create binary from C string */
static ERL_NIF_TERM make_binary(ErlNifEnv* env, const char* str) {
    ERL_NIF_TERM bin;
    size_t len = strlen(str);
    unsigned char* buf = enif_make_new_binary(env, len, &bin);
    memcpy(buf, str, len);
    return bin;
}

/* Auto-initialize SDK on first use (thread-safe) */
static nvat_rc_t ensure_sdk_initialized(void) {
    pthread_mutex_lock(&sdk_mutex);
    if (!sdk_initialized) {
        nvat_sdk_opts_t opts = NULL;
        nvat_rc_t err = nvat_sdk_opts_create(&opts);
        if (err != NVAT_RC_OK) {
            pthread_mutex_unlock(&sdk_mutex);
            return err;
        }
        
        /* Create logger with configurable log level */
        nvat_logger_t logger = NULL;
#ifdef NVAT_DEBUG_LOG
        err = nvat_logger_spdlog_create(&logger, NVAT_LOG_LEVEL_DEBUG, NULL);
#else
        err = nvat_logger_spdlog_create(&logger, NVAT_LOG_LEVEL_ERROR, NULL);
#endif
        if (err == NVAT_RC_OK && logger != NULL) {
            nvat_sdk_opts_set_logger(opts, logger);
        }
        
        err = nvat_sdk_init(opts);
        nvat_sdk_opts_free(&opts);
        
        if (err != NVAT_RC_OK) {
            pthread_mutex_unlock(&sdk_mutex);
            return err;
        }
        sdk_initialized = 1;
    }
    pthread_mutex_unlock(&sdk_mutex);
    return NVAT_RC_OK;
}

/*
 * collect_evidence_nif/1
 * 
 * Collect GPU attestation evidence and verify it locally.
 * 
 * Input: Nonce (binary, hex string)
 * Output: {ok, JSON} | {error, Reason}
 *         JSON contains: evidences, claims, detached_eat
 */
static ERL_NIF_TERM collect_evidence_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    /* Ensure SDK is initialized */
    nvat_rc_t err = ensure_sdk_initialized();
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Parse nonce argument */
    ErlNifBinary nonce_bin;
    if (!enif_inspect_binary(env, argv[0], &nonce_bin)) {
        return make_error(env, "nonce must be a binary");
    }
    
    /* Create null-terminated nonce string */
    char* nonce_str = (char*)malloc(nonce_bin.size + 1);
    if (!nonce_str) {
        return make_error(env, "memory allocation failed");
    }
    memcpy(nonce_str, nonce_bin.data, nonce_bin.size);
    nonce_str[nonce_bin.size] = '\0';
    
    /* Parse nonce from hex */
    nvat_nonce_t nonce = NULL;
    err = nvat_nonce_from_hex(&nonce, nonce_str);
    free(nonce_str);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Create GPU evidence source from NVML */
    nvat_gpu_evidence_source_t source = NULL;
    err = nvat_gpu_evidence_source_nvml_create(&source);
    if (err != NVAT_RC_OK) {
        nvat_nonce_free(&nonce);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Collect GPU evidence */
    nvat_gpu_evidence_t* evidence_array = NULL;
    size_t num_evidences = 0;
    err = nvat_gpu_evidence_collect(source, nonce, &evidence_array, &num_evidences);
    if (err != NVAT_RC_OK) {
        nvat_gpu_evidence_source_free(&source);
        nvat_nonce_free(&nonce);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Serialize evidence to JSON */
    nvat_str_t serialized_evidence = NULL;
    err = nvat_gpu_evidence_serialize_json(evidence_array, num_evidences, &serialized_evidence);
    if (err != NVAT_RC_OK) {
        nvat_gpu_evidence_array_free(&evidence_array, num_evidences);
        nvat_gpu_evidence_source_free(&source);
        nvat_nonce_free(&nonce);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    char* evidence_data = NULL;
    nvat_str_get_data(serialized_evidence, &evidence_data);
    
    /* Create RIM store and OCSP client for local verification */
    nvat_rim_store_t rim_store = NULL;
    err = nvat_rim_store_create_remote(&rim_store, NULL, NULL, NULL);
    if (err != NVAT_RC_OK) {
        nvat_str_free(&serialized_evidence);
        nvat_gpu_evidence_array_free(&evidence_array, num_evidences);
        nvat_gpu_evidence_source_free(&source);
        nvat_nonce_free(&nonce);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    nvat_ocsp_client_t ocsp_client = NULL;
    err = nvat_ocsp_client_create_default(&ocsp_client, NULL, NULL, NULL);
    if (err != NVAT_RC_OK) {
        nvat_rim_store_free(&rim_store);
        nvat_str_free(&serialized_evidence);
        nvat_gpu_evidence_array_free(&evidence_array, num_evidences);
        nvat_gpu_evidence_source_free(&source);
        nvat_nonce_free(&nonce);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Create local verifier */
    nvat_gpu_local_verifier_t local_verifier = NULL;
    err = nvat_gpu_local_verifier_create(&local_verifier, rim_store, ocsp_client, NULL);
    if (err != NVAT_RC_OK) {
        nvat_ocsp_client_free(&ocsp_client);
        nvat_rim_store_free(&rim_store);
        nvat_str_free(&serialized_evidence);
        nvat_gpu_evidence_array_free(&evidence_array, num_evidences);
        nvat_gpu_evidence_source_free(&source);
        nvat_nonce_free(&nonce);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    nvat_gpu_verifier_t verifier = nvat_gpu_local_verifier_upcast(local_verifier);
    
    /* Create evidence policy */
    nvat_evidence_policy_t policy = NULL;
    err = nvat_evidence_policy_create_default(&policy);
    if (err != NVAT_RC_OK) {
        nvat_gpu_verifier_free(&verifier);
        nvat_ocsp_client_free(&ocsp_client);
        nvat_rim_store_free(&rim_store);
        nvat_str_free(&serialized_evidence);
        nvat_gpu_evidence_array_free(&evidence_array, num_evidences);
        nvat_gpu_evidence_source_free(&source);
        nvat_nonce_free(&nonce);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Verify evidence */
    nvat_str_t detached_eat = NULL;
    nvat_claims_collection_t claims = NULL;
    err = nvat_verify_gpu_evidence(verifier, evidence_array, num_evidences, policy, &detached_eat, &claims);
    
    /* Even on verification failure, we may have partial results */
    int verification_success = (err == NVAT_RC_OK);
    
    /* Serialize claims */
    nvat_str_t serialized_claims = NULL;
    char* claims_data = "{}";
    if (claims != NULL) {
        if (nvat_claims_collection_serialize_json(claims, &serialized_claims) == NVAT_RC_OK) {
            nvat_str_get_data(serialized_claims, &claims_data);
        }
    }
    
    /* Get detached EAT data */
    char* eat_data = "{}";
    if (detached_eat != NULL) {
        nvat_str_get_data(detached_eat, &eat_data);
    }
    
    /* Build result JSON string */
    size_t result_len = strlen(evidence_data) + strlen(claims_data) + strlen(eat_data) + 200;
    char* result_json = (char*)malloc(result_len);
    if (!result_json) {
        /* Cleanup */
        if (serialized_claims) nvat_str_free(&serialized_claims);
        if (detached_eat) nvat_str_free(&detached_eat);
        if (claims) nvat_claims_collection_free(&claims);
        nvat_evidence_policy_free(&policy);
        nvat_gpu_verifier_free(&verifier);
        nvat_ocsp_client_free(&ocsp_client);
        nvat_rim_store_free(&rim_store);
        nvat_str_free(&serialized_evidence);
        nvat_gpu_evidence_array_free(&evidence_array, num_evidences);
        nvat_gpu_evidence_source_free(&source);
        nvat_nonce_free(&nonce);
        return make_error(env, "memory allocation failed");
    }
    
    snprintf(result_json, result_len,
        "{\"evidences\":%s,\"claims\":%s,\"eat\":%s,\"verified\":%s}",
        evidence_data,
        claims_data,
        eat_data,
        verification_success ? "true" : "false"
    );
    
    ERL_NIF_TERM result = make_binary(env, result_json);
    free(result_json);
    
    /* Cleanup */
    if (serialized_claims) nvat_str_free(&serialized_claims);
    if (detached_eat) nvat_str_free(&detached_eat);
    if (claims) nvat_claims_collection_free(&claims);
    nvat_evidence_policy_free(&policy);
    nvat_gpu_verifier_free(&verifier);
    nvat_ocsp_client_free(&ocsp_client);
    nvat_rim_store_free(&rim_store);
    nvat_str_free(&serialized_evidence);
    nvat_gpu_evidence_array_free(&evidence_array, num_evidences);
    nvat_gpu_evidence_source_free(&source);
    nvat_nonce_free(&nonce);
    
    return make_ok(env, result);
}

/*
 * verify_evidence_nif/1
 * 
 * Verify GPU attestation evidence from JSON.
 * The evidence JSON already contains the nonce from when it was collected.
 * Uses nvat_attestation_ctx with nvat_attest_device for proper verification.
 * 
 * Input: Evidence JSON (binary)
 * Output: {ok, JSON} | {error, Reason}
 *         JSON contains: valid (boolean), claims, eat
 */
static ERL_NIF_TERM verify_evidence_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    /* Ensure SDK is initialized */
    nvat_rc_t err = ensure_sdk_initialized();
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Parse evidence JSON argument */
    ErlNifBinary evidence_bin;
    if (!enif_inspect_binary(env, argv[0], &evidence_bin)) {
        return make_error(env, "evidence must be a binary");
    }
    
    /* Write evidence to temporary file (nvat requires file path) */
    char temp_path[] = "/tmp/nvat_evidence_XXXXXX.json";
    int fd = mkstemps(temp_path, 5);
    if (fd < 0) {
        return make_error(env, "failed to create temp file");
    }
    write(fd, evidence_bin.data, evidence_bin.size);
    close(fd);
    
    /* Create attestation context */
    nvat_attestation_ctx_t ctx = NULL;
    err = nvat_attestation_ctx_create(&ctx);
    if (err != NVAT_RC_OK) {
        unlink(temp_path);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Set device type to GPU */
    err = nvat_attestation_ctx_set_device_type(ctx, NVAT_DEVICE_GPU);
    if (err != NVAT_RC_OK) {
        nvat_attestation_ctx_free(&ctx);
        unlink(temp_path);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Set evidence source from JSON file (nonce is embedded in the evidence) */
    err = nvat_attestation_ctx_set_gpu_evidence_source_json_file(ctx, temp_path);
    unlink(temp_path);  /* Remove temp file */
    if (err != NVAT_RC_OK) {
        nvat_attestation_ctx_free(&ctx);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Create and set evidence policy */
    nvat_evidence_policy_t policy = NULL;
    err = nvat_evidence_policy_create_default(&policy);
    if (err != NVAT_RC_OK) {
        nvat_attestation_ctx_free(&ctx);
        return make_error(env, nvat_rc_to_string(err));
    }
    err = nvat_attestation_ctx_set_evidence_policy(ctx, &policy);
    if (err != NVAT_RC_OK) {
        nvat_evidence_policy_free(&policy);
        nvat_attestation_ctx_free(&ctx);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Set local verification */
    err = nvat_attestation_ctx_set_verifier_type(ctx, NVAT_VERIFY_LOCAL);
    if (err != NVAT_RC_OK) {
        nvat_attestation_ctx_free(&ctx);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Perform attestation verification (nonce=NULL means use nonce from evidence) */
    nvat_str_t detached_eat = NULL;
    nvat_claims_collection_t claims = NULL;
    err = nvat_attest_device(ctx, NULL, &detached_eat, &claims);
    
    /* Check if verification succeeded */
    int verification_success = (err == NVAT_RC_OK);
    
    /* Serialize claims */
    nvat_str_t serialized_claims = NULL;
    char* claims_data = "{}";
    if (claims != NULL) {
        if (nvat_claims_collection_serialize_json(claims, &serialized_claims) == NVAT_RC_OK) {
            nvat_str_get_data(serialized_claims, &claims_data);
        }
    }
    
    /* Get EAT data */
    char* eat_data = "{}";
    if (detached_eat != NULL) {
        nvat_str_get_data(detached_eat, &eat_data);
    }
    
    /* Build result JSON */
    size_t result_len = strlen(claims_data) + strlen(eat_data) + 100;
    char* result_json = (char*)malloc(result_len);
    if (!result_json) {
        if (serialized_claims) nvat_str_free(&serialized_claims);
        if (detached_eat) nvat_str_free(&detached_eat);
        if (claims) nvat_claims_collection_free(&claims);
        nvat_attestation_ctx_free(&ctx);
        return make_error(env, "memory allocation failed");
    }
    
    snprintf(result_json, result_len,
        "{\"valid\":%s,\"claims\":%s,\"eat\":%s}",
        verification_success ? "true" : "false",
        claims_data,
        eat_data
    );
    
    ERL_NIF_TERM result = make_binary(env, result_json);
    free(result_json);
    
    /* Cleanup */
    if (serialized_claims) nvat_str_free(&serialized_claims);
    if (detached_eat) nvat_str_free(&detached_eat);
    if (claims) nvat_claims_collection_free(&claims);
    nvat_attestation_ctx_free(&ctx);
    
    return make_ok(env, result);
}

/* NIF function table */
static ErlNifFunc nif_funcs[] = {
    {"collect_evidence_nif", 1, collect_evidence_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"verify_evidence_nif", 1, verify_evidence_nif, ERL_NIF_DIRTY_JOB_IO_BOUND}
};

/* Module callbacks */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
    pthread_mutex_lock(&sdk_mutex);
    if (sdk_initialized) {
        nvat_sdk_shutdown();
        sdk_initialized = 0;
    }
    pthread_mutex_unlock(&sdk_mutex);
}

ERL_NIF_INIT(dev_sev_gpu, nif_funcs, load, NULL, NULL, unload)
