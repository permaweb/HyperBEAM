/*
 * NVIDIA GPU TEE Attestation NIF for Erlang
 * 
 * This C++ NIF wraps the nvat (NVIDIA Attestation) SDK to provide
 * GPU attestation evidence collection and verification for Erlang.
 * 
 * Flow mapping to official nvattest CLI:
 * - generate/1 = collect-evidence + attest (with nonce)
 * - verify/1   = attest (with evidence file + nonce from evidence)
 */

#include <cstring>
#include <cstdlib>
#include <string>
#include <memory>
#include <mutex>
#include <fstream>
#include <unistd.h>

#include "erl_nif.h"
#include "nvat.h"
#include "nlohmann/json.hpp"

using json = nlohmann::json;

/* ============================================================================
 * RAII Deleters for NVAT types (matching nvattest_types.h pattern)
 * ============================================================================ */

template<class T> struct NvatDeleter;

template<> struct NvatDeleter<nvat_attestation_ctx_t> {
    void operator()(nvat_attestation_ctx_t* ptr) const {
        if (ptr) nvat_attestation_ctx_free(ptr);
    }
};

template<> struct NvatDeleter<nvat_sdk_opts_t> {
    void operator()(nvat_sdk_opts_t* ptr) const {
        if (ptr) nvat_sdk_opts_free(ptr);
    }
};

template<> struct NvatDeleter<nvat_gpu_evidence_source_t> {
    void operator()(nvat_gpu_evidence_source_t* ptr) const {
        if (ptr) nvat_gpu_evidence_source_free(ptr);
    }
};

template<> struct NvatDeleter<nvat_nonce_t> {
    void operator()(nvat_nonce_t* ptr) const {
        if (ptr) nvat_nonce_free(ptr);
    }
};

template<> struct NvatDeleter<nvat_str_t> {
    void operator()(nvat_str_t* ptr) const {
        if (ptr) nvat_str_free(ptr);
    }
};

template<> struct NvatDeleter<nvat_claims_collection_t> {
    void operator()(nvat_claims_collection_t* ptr) const {
        if (ptr) nvat_claims_collection_free(ptr);
    }
};

template<class T>
using nvat_ptr = std::unique_ptr<T, NvatDeleter<T>>;

/* GPU evidence array wrapper with RAII - non-copyable */
class GpuEvidenceArray {
public:
    nvat_gpu_evidence_t* evidences = nullptr;
    size_t num_evidences = 0;
    
    GpuEvidenceArray() = default;
    ~GpuEvidenceArray() {
        if (evidences) {
            nvat_gpu_evidence_array_free(&evidences, num_evidences);
        }
    }
    
    // Non-copyable
    GpuEvidenceArray(const GpuEvidenceArray&) = delete;
    GpuEvidenceArray& operator=(const GpuEvidenceArray&) = delete;
};

/* ============================================================================
 * SDK Lifecycle Management
 * ============================================================================ */

static std::mutex sdk_mutex;
static bool sdk_initialized = false;

static nvat_rc_t ensure_sdk_initialized() {
    std::lock_guard<std::mutex> lock(sdk_mutex);
    
    if (!sdk_initialized) {
        nvat_sdk_opts_t raw_opts = nullptr;
        nvat_rc_t err = nvat_sdk_opts_create(&raw_opts);
        if (err != NVAT_RC_OK) return err;
        
        nvat_ptr<nvat_sdk_opts_t> opts(&raw_opts);
        
        /* Create logger */
        nvat_logger_t logger = nullptr;
#ifdef NVAT_DEBUG_LOG
        err = nvat_logger_spdlog_create(&logger, "dev_sev_gpu_nif", NVAT_LOG_LEVEL_DEBUG);
#else
        err = nvat_logger_spdlog_create(&logger, "dev_sev_gpu_nif", NVAT_LOG_LEVEL_ERROR);
#endif
        if (err == NVAT_RC_OK && logger) {
            nvat_sdk_opts_set_logger(*opts.get(), logger);
            nvat_logger_free(&logger);
        }
        
        err = nvat_sdk_init(*opts.get());
        if (err != NVAT_RC_OK) return err;
        
        sdk_initialized = true;
    }
    
    return NVAT_RC_OK;
}

/* ============================================================================
 * NIF Helper Functions
 * ============================================================================ */

static ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* atom) {
    ERL_NIF_TERM ret;
    if (enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, atom);
}

static ERL_NIF_TERM make_binary(ErlNifEnv* env, const std::string& str) {
    ERL_NIF_TERM bin;
    unsigned char* buf = enif_make_new_binary(env, str.size(), &bin);
    if (buf) {
        memcpy(buf, str.data(), str.size());
    }
    return bin;
}

static ERL_NIF_TERM make_ok(ErlNifEnv* env, ERL_NIF_TERM result) {
    return enif_make_tuple2(env, make_atom(env, "ok"), result);
}

static ERL_NIF_TERM make_error(ErlNifEnv* env, const std::string& reason) {
    return enif_make_tuple2(env, make_atom(env, "error"), make_binary(env, reason));
}

/* ============================================================================
 * collect_evidence_nif/1
 * 
 * Equivalent to: nvattest collect-evidence --device gpu --nonce <nonce>
 *              + nvattest attest --device gpu --verifier local --nonce <nonce>
 * 
 * Input: Nonce (binary hex string)
 * Output: {ok, JSON} | {error, Reason}
 *         JSON contains: nonce, evidences, claims, eat, verified
 * ============================================================================ */
static ERL_NIF_TERM collect_evidence_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    nvat_rc_t err = ensure_sdk_initialized();
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Parse nonce argument */
    ErlNifBinary nonce_bin;
    if (!enif_inspect_binary(env, argv[0], &nonce_bin)) {
        return make_error(env, "nonce must be a binary");
    }
    std::string nonce_str(reinterpret_cast<char*>(nonce_bin.data), nonce_bin.size);
    
    /* ========== Phase 1: collect-evidence ========== */
    
    /* Create nonce */
    nvat_nonce_t raw_nonce = nullptr;
    err = nvat_nonce_from_hex(&raw_nonce, nonce_str.c_str());
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    nvat_ptr<nvat_nonce_t> nonce(&raw_nonce);
    
    /* Create NVML evidence source */
    nvat_gpu_evidence_source_t raw_source = nullptr;
    err = nvat_gpu_evidence_source_nvml_create(&raw_source);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    nvat_ptr<nvat_gpu_evidence_source_t> source(&raw_source);
    
    /* Collect evidence */
    GpuEvidenceArray evidence;
    err = nvat_gpu_evidence_collect(*source.get(), *nonce.get(), 
                                    &evidence.evidences, &evidence.num_evidences);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Serialize evidence to JSON */
    nvat_str_t raw_serialized = nullptr;
    err = nvat_gpu_evidence_serialize_json(evidence.evidences, evidence.num_evidences, &raw_serialized);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    nvat_ptr<nvat_str_t> serialized(&raw_serialized);
    
    char* evidence_data = nullptr;
    err = nvat_str_get_data(*serialized.get(), &evidence_data);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* ========== Phase 2: attest (local verification) ========== */
    
    /* Create attestation context */
    nvat_attestation_ctx_t raw_ctx = nullptr;
    err = nvat_attestation_ctx_create(&raw_ctx);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    nvat_ptr<nvat_attestation_ctx_t> ctx(&raw_ctx);
    
    /* Set device type to GPU */
    err = nvat_attestation_ctx_set_device_type(*ctx.get(), NVAT_DEVICE_GPU);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Create and set evidence policy (ownership transfers to context) */
    nvat_evidence_policy_t raw_policy = nullptr;
    err = nvat_evidence_policy_create_default(&raw_policy);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    err = nvat_attestation_ctx_set_evidence_policy(*ctx.get(), &raw_policy);
    if (err != NVAT_RC_OK) {
        nvat_evidence_policy_free(&raw_policy);
        return make_error(env, nvat_rc_to_string(err));
    }
    // Note: raw_policy ownership is transferred to ctx, do not free
    
    /* Set local verifier */
    err = nvat_attestation_ctx_set_verifier_type(*ctx.get(), NVAT_VERIFY_LOCAL);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Perform attestation */
    nvat_str_t raw_eat = nullptr;
    nvat_claims_collection_t raw_claims = nullptr;
    err = nvat_attest_device(*ctx.get(), *nonce.get(), &raw_eat, &raw_claims);
    
    bool verified = (err == NVAT_RC_OK);
    nvat_ptr<nvat_str_t> eat(&raw_eat);
    nvat_ptr<nvat_claims_collection_t> claims(&raw_claims);
    
    /* Serialize claims */
    std::string claims_json = "{}";
    if (raw_claims) {
        nvat_str_t raw_claims_str = nullptr;
        if (nvat_claims_collection_serialize_json(raw_claims, &raw_claims_str) == NVAT_RC_OK) {
            char* claims_data = nullptr;
            if (nvat_str_get_data(raw_claims_str, &claims_data) == NVAT_RC_OK && claims_data) {
                claims_json = claims_data;
            }
            nvat_str_free(&raw_claims_str);
        }
    }
    
    /* Get EAT data */
    std::string eat_json = "{}";
    if (raw_eat) {
        char* eat_data = nullptr;
        if (nvat_str_get_data(raw_eat, &eat_data) == NVAT_RC_OK && eat_data) {
            eat_json = eat_data;
        }
    }
    
    /* Build result JSON */
    json result;
    result["verified"] = verified;
    
    try {
        result["evidences"] = json::parse(evidence_data);
    } catch (...) {
        result["evidences"] = json::object();
    }
    
    try {
        result["claims"] = json::parse(claims_json);
    } catch (...) {
        result["claims"] = json::object();
    }
    
    try {
        result["eat"] = json::parse(eat_json);
    } catch (...) {
        result["eat"] = json::object();
    }
    
    return make_ok(env, make_binary(env, result.dump()));
}

/* ============================================================================
 * verify_evidence_nif/1
 * 
 * Equivalent to: nvattest attest --device gpu --verifier local 
 *                --gpu-evidence <evidence_file> --nonce <nonce_from_evidence>
 * 
 * The evidence JSON already contains the nonce used during collection.
 * Input: Evidence JSON (binary)
 * Output: {ok, JSON} | {error, Reason}
 *         JSON contains: verified, claims, eat
 * ============================================================================ */
static ERL_NIF_TERM verify_evidence_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    nvat_rc_t err = ensure_sdk_initialized();
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Parse evidence JSON argument */
    ErlNifBinary evidence_bin;
    if (!enif_inspect_binary(env, argv[0], &evidence_bin)) {
        return make_error(env, "evidence must be a binary");
    }
    std::string evidence_str(reinterpret_cast<char*>(evidence_bin.data), evidence_bin.size);
    
    /* Extract nonce from evidence JSON */
    std::string nonce_str;
    try {
        json evidence_json = json::parse(evidence_str);
        if (evidence_json.contains("nonce") && evidence_json["nonce"].is_string()) {
            nonce_str = evidence_json["nonce"].get<std::string>();
        }
    } catch (...) {
        return make_error(env, "failed to parse evidence JSON");
    }
    
    if (nonce_str.empty()) {
        return make_error(env, "nonce not found in evidence JSON");
    }
    
    /* Write evidence to temporary file (nvat requires file path for --gpu-evidence) */
    char temp_path[] = "/tmp/nvat_evidence_XXXXXX.json";
    int fd = mkstemps(temp_path, 5);
    if (fd < 0) {
        return make_error(env, "failed to create temp file");
    }
    ssize_t written = write(fd, evidence_bin.data, evidence_bin.size);
    close(fd);
    if (written != static_cast<ssize_t>(evidence_bin.size)) {
        unlink(temp_path);
        return make_error(env, "failed to write temp file");
    }
    
    /* ========== attest with evidence file ========== */
    
    /* Create attestation context */
    nvat_attestation_ctx_t raw_ctx = nullptr;
    err = nvat_attestation_ctx_create(&raw_ctx);
    if (err != NVAT_RC_OK) {
        unlink(temp_path);
        return make_error(env, nvat_rc_to_string(err));
    }
    nvat_ptr<nvat_attestation_ctx_t> ctx(&raw_ctx);
    
    /* Set device type to GPU */
    err = nvat_attestation_ctx_set_device_type(*ctx.get(), NVAT_DEVICE_GPU);
    if (err != NVAT_RC_OK) {
        unlink(temp_path);
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Set evidence source from JSON file (equivalent to --gpu-evidence option) */
    err = nvat_attestation_ctx_set_gpu_evidence_source_json_file(*ctx.get(), temp_path);
    unlink(temp_path);  /* Remove temp file immediately after reading */
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Create and set evidence policy (ownership transfers to context) */
    nvat_evidence_policy_t raw_policy = nullptr;
    err = nvat_evidence_policy_create_default(&raw_policy);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    err = nvat_attestation_ctx_set_evidence_policy(*ctx.get(), &raw_policy);
    if (err != NVAT_RC_OK) {
        nvat_evidence_policy_free(&raw_policy);
        return make_error(env, nvat_rc_to_string(err));
    }
    // Note: raw_policy ownership is transferred to ctx
    
    /* Set local verifier */
    err = nvat_attestation_ctx_set_verifier_type(*ctx.get(), NVAT_VERIFY_LOCAL);
    if (err != NVAT_RC_OK) {
        return make_error(env, nvat_rc_to_string(err));
    }
    
    /* Parse nonce from evidence */
    nvat_nonce_t raw_nonce = nullptr;
    err = nvat_nonce_from_hex(&raw_nonce, nonce_str.c_str());
    if (err != NVAT_RC_OK) {
        return make_error(env, "failed to parse nonce from evidence");
    }
    nvat_ptr<nvat_nonce_t> nonce(&raw_nonce);
    
    /* Perform attestation verification */
    nvat_str_t raw_eat = nullptr;
    nvat_claims_collection_t raw_claims = nullptr;
    err = nvat_attest_device(*ctx.get(), *nonce.get(), &raw_eat, &raw_claims);
    
    bool verified = (err == NVAT_RC_OK);
    nvat_ptr<nvat_str_t> eat(&raw_eat);
    nvat_ptr<nvat_claims_collection_t> claims(&raw_claims);
    
    /* Serialize claims */
    std::string claims_json = "{}";
    if (raw_claims) {
        nvat_str_t raw_claims_str = nullptr;
        if (nvat_claims_collection_serialize_json(raw_claims, &raw_claims_str) == NVAT_RC_OK) {
            char* claims_data = nullptr;
            if (nvat_str_get_data(raw_claims_str, &claims_data) == NVAT_RC_OK && claims_data) {
                claims_json = claims_data;
            }
            nvat_str_free(&raw_claims_str);
        }
    }
    
    /* Get EAT data */
    std::string eat_json = "{}";
    if (raw_eat) {
        char* eat_data = nullptr;
        if (nvat_str_get_data(raw_eat, &eat_data) == NVAT_RC_OK && eat_data) {
            eat_json = eat_data;
        }
    }
    
    /* Build result JSON */
    json result;
    result["verified"] = verified;
    
    try {
        result["claims"] = json::parse(claims_json);
    } catch (...) {
        result["claims"] = json::object();
    }
    
    try {
        result["eat"] = json::parse(eat_json);
    } catch (...) {
        result["eat"] = json::object();
    }
    
    return make_ok(env, make_binary(env, result.dump()));
}

/* ============================================================================
 * NIF Registration
 * ============================================================================ */

static ErlNifFunc nif_funcs[] = {
    {"collect_evidence_nif", 1, collect_evidence_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"verify_evidence_nif", 1, verify_evidence_nif, ERL_NIF_DIRTY_JOB_IO_BOUND}
};

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
    std::lock_guard<std::mutex> lock(sdk_mutex);
    if (sdk_initialized) {
        nvat_sdk_shutdown();
        sdk_initialized = false;
    }
}

extern "C" {
    ERL_NIF_INIT(dev_sev_gpu, nif_funcs, load, NULL, NULL, unload)
}
