// Minimal NIF - only for ioctl to /dev/sev-guest
// Everything else can be done in Erlang

#include "erl_nif.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <linux/types.h>
#include <openssl/evp.h>
#include <openssl/ec.h>
#include <openssl/ecdsa.h>
#include <openssl/sha.h>
#include <openssl/x509.h>
#include <openssl/pem.h>
#include <openssl/x509v3.h>
#include <openssl/x509_vfy.h>

// Simple logging macro for NIF (similar to DRV_DEBUG in driver code)
#define NIF_DEBUG 1  // Set to 0 to disable debug logging
#define NIF_LOG(format, ...) \
    do { \
        if (NIF_DEBUG) { \
            fprintf(stderr, "[C-NIF @ %s:%d] " format "\n", __FILE__, __LINE__, ##__VA_ARGS__); \
        } \
    } while(0)

// SEV ioctl definitions
#ifndef _UAPI_LINUX_SEV_GUEST_H_
#define SEV_GUEST_IOC_TYPE 'S'
#define SEV_GUEST_IOC_NR_GET_REPORT 0

#define _IOC_NRBITS     8
#define _IOC_TYPEBITS   8
#define _IOC_SIZEBITS   14
#define _IOC_DIRBITS    2

#define _IOC_NRSHIFT    0
#define _IOC_TYPESHIFT  (_IOC_NRSHIFT+_IOC_NRBITS)
#define _IOC_SIZESHIFT  (_IOC_TYPESHIFT+_IOC_TYPEBITS)
#define _IOC_DIRSHIFT   (_IOC_SIZESHIFT+_IOC_SIZEBITS)

#define _IOC_NONE       0U
#define _IOC_WRITE      1U
#define _IOC_READ       2U

#define _IOC(dir,type,nr,size) \
    (((dir)  << _IOC_DIRSHIFT) | \
     ((type) << _IOC_TYPESHIFT) | \
     ((nr)   << _IOC_NRSHIFT) | \
     ((size) << _IOC_SIZESHIFT))

#define _IOWR(type,nr,size) _IOC(_IOC_READ|_IOC_WRITE,(type),(nr),sizeof(size))

struct sev_guest_request {
    __u32 msg_version;
    __u64 request_data;
    __u64 response_data;
    __u64 fw_err;
};

#define SEV_GUEST_IOC_GET_REPORT \
    _IOWR(SEV_GUEST_IOC_TYPE, SEV_GUEST_IOC_NR_GET_REPORT, \
          struct sev_guest_request)
#endif

// Report request structure (96 bytes)
struct snp_report_req {
    __u8 report_data[64];
    __u32 vmpl;
    __u8 reserved[28];
};

// Report response structure (4000 bytes)
struct snp_report_resp {
    __u32 status;
    __u32 report_size;
    __u8 reserved[24];
    __u8 report[1184];  // AttestationReport size
    __u8 padding[2784]; // Padding to 4000 bytes
};

// Error codes
typedef enum {
    SNP_ERR_NONE = 0,
    SNP_ERR_INVALID_INPUT,
    SNP_ERR_IOCTL_FAILED,
    SNP_ERR_FIRMWARE_ERROR,
    SNP_ERR_MEMORY_ERROR
} snp_error_t;

// Helper to create error tuple
static ERL_NIF_TERM make_error(ErlNifEnv *env, snp_error_t err_code, const char *msg) {
    ERL_NIF_TERM error_code = enif_make_int(env, err_code);
    ERL_NIF_TERM error_msg = enif_make_string(env, msg, ERL_NIF_LATIN1);
    ERL_NIF_TERM error_tuple = enif_make_tuple2(env, error_code, error_msg);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), error_tuple);
}

// NIF: check_snp_support
static ERL_NIF_TERM nif_check_snp_support(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int fd = open("/dev/sev-guest", O_RDONLY);
    if (fd < 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "false"));
    }
    close(fd);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "true"));
}

// NIF: generate_attestation_report
// This is the ONLY function that needs C - everything else can be Erlang
static ERL_NIF_TERM nif_generate_attestation_report(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary unique_data;
    unsigned int vmpl;
    
    // Input validation
    if (!enif_inspect_binary(env, argv[0], &unique_data) || unique_data.size != 64) {
        return make_error(env, SNP_ERR_INVALID_INPUT, "Input binary must be exactly 64 bytes");
    }
    
    if (!enif_get_uint(env, argv[1], &vmpl)) {
        return make_error(env, SNP_ERR_INVALID_INPUT, "Invalid VMPL value: must be an integer");
    }
    
    if (vmpl > 3) {
        return make_error(env, SNP_ERR_INVALID_INPUT, "VMPL must be <= 3");
    }
    
    // Open SEV guest device
    int fd = open("/dev/sev-guest", O_RDWR);
    if (fd < 0) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "Failed to open /dev/sev-guest: %s", strerror(errno));
        return make_error(env, SNP_ERR_IOCTL_FAILED, err_msg);
    }
    
    // Prepare request structure
    struct snp_report_req req;
    memset(&req, 0, sizeof(req));
    memcpy(req.report_data, unique_data.data, 64);
    req.vmpl = vmpl;
    
    // Prepare response structure
    struct snp_report_resp resp;
    memset(&resp, 0, sizeof(resp));
    
    // Prepare guest request structure
    struct sev_guest_request guest_req;
    guest_req.msg_version = 1;
    guest_req.request_data = (__u64)(unsigned long)&req;
    guest_req.response_data = (__u64)(unsigned long)&resp;
    guest_req.fw_err = 0;
    
    // Perform ioctl - THIS IS THE ONLY REASON WE NEED C
    int ret = ioctl(fd, SEV_GUEST_IOC_GET_REPORT, &guest_req);
    close(fd);
    
    if (ret < 0) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "ioctl(SNP_GET_REPORT) failed: %s", strerror(errno));
        return make_error(env, SNP_ERR_IOCTL_FAILED, err_msg);
    }
    
    if (resp.status != 0) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "Firmware error (status=0x%x): SNP_GET_REPORT failed", resp.status);
        return make_error(env, SNP_ERR_FIRMWARE_ERROR, err_msg);
    }
    
    // Validate report size
    if (resp.report_size != 1184) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "Invalid report size: expected 1184, got %u", resp.report_size);
        return make_error(env, SNP_ERR_INVALID_INPUT, err_msg);
    }
    
    // Return binary report structure (1184 bytes)
    // All parsing, verification, etc. happens in Erlang
    ERL_NIF_TERM result;
    unsigned char *bin = enif_make_new_binary(env, 1184, &result);
    if (!bin) {
        return make_error(env, SNP_ERR_MEMORY_ERROR, "Failed to allocate binary for report");
    }
    memcpy(bin, resp.report, 1184);
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

// Helper function to parse certificate chain (ARK + ASK) from DER
// Returns: STACK_OF(X509) containing ARK and ASK, or NULL on error
static STACK_OF(X509) *parse_cert_chain(const unsigned char *data, long len) {
    STACK_OF(X509) *chain = sk_X509_new_null();
    if (!chain) {
        return NULL;
    }
    
    const unsigned char *p = data;
    long remaining = len;
    
    // Parse certificates sequentially from the DER blob
    // AMD KDS returns ASK first, then ARK (as per SEV spec)
    // The DER blob contains concatenated DER-encoded certificates
    while (remaining > 0) {
        const unsigned char *cert_start = p;
        X509 *cert = d2i_X509(NULL, &p, remaining);
        if (!cert) {
            // No more certificates or parse error
            break;
        }
        
        // Calculate how many bytes were consumed
        long cert_len = p - cert_start;
        remaining -= cert_len;
        
        if (!sk_X509_push(chain, cert)) {
            X509_free(cert);
            sk_X509_pop_free(chain, X509_free);
            return NULL;
        }
    }
    
    if (sk_X509_num(chain) < 2) {
        NIF_LOG("Certificate chain must contain at least 2 certificates (ARK + ASK), got %d", sk_X509_num(chain));
        sk_X509_pop_free(chain, X509_free);
        return NULL;
    }
    
    return chain;
}

// Verify certificate chain: ARK -> ASK -> VCEK
// Returns 1 on success, 0 on failure
static int verify_cert_chain(STACK_OF(X509) *chain, X509 *vcek) {
    if (!chain || !vcek || sk_X509_num(chain) < 2) {
        NIF_LOG("Invalid certificate chain or VCEK");
        return 0;
    }
    
    // Create X509_STORE and add ARK as trusted root
    X509_STORE *store = X509_STORE_new();
    if (!store) {
        NIF_LOG("Failed to create X509_STORE");
        return 0;
    }
    
    // Certificate order in DER blob: ASK first, then ARK (as per SEV spec)
    // ARK is the root (self-signed), ASK is signed by ARK
    X509 *ask = sk_X509_value(chain, 0);  // First cert is ASK
    X509 *ark = sk_X509_value(chain, 1);  // Second cert is ARK (root)
    
    // Set verification flags - allow self-signed root and enable chain building
    unsigned long flags = X509_V_FLAG_ALLOW_PROXY_CERTS;
    X509_STORE_set_flags(store, flags);
    
    // Add ARK to store as trusted root
    // Note: We need to add it as a trusted cert, not just any cert
    if (!X509_STORE_add_cert(store, ark)) {
        NIF_LOG("Failed to add ARK to store");
        X509_STORE_free(store);
        return 0;
    }
    
    // Verify ARK is self-signed (it should be)
    X509_NAME *ark_subject = X509_get_subject_name(ark);
    X509_NAME *ark_issuer = X509_get_issuer_name(ark);
    int is_self_signed = X509_NAME_cmp(ark_subject, ark_issuer) == 0;
    NIF_LOG("ARK is self-signed: %d", is_self_signed);
    
    // Create verification context
    X509_STORE_CTX *ctx = X509_STORE_CTX_new();
    if (!ctx) {
        NIF_LOG("Failed to create X509_STORE_CTX");
        X509_STORE_free(store);
        return 0;
    }
    
    // Build untrusted chain: ARK -> ASK -> VCEK
    // Include ARK in the chain so OpenSSL can find it as ASK's issuer
    // ARK is also in the store as trusted, so OpenSSL will trust it
    STACK_OF(X509) *untrusted_chain = sk_X509_new_null();
    if (!untrusted_chain) {
        X509_STORE_CTX_free(ctx);
        X509_STORE_free(store);
        return 0;
    }
    
    // Add ARK first (root), then ASK (intermediate), then VCEK (end entity)
    // Order: root to end entity (OpenSSL builds chain backwards from target)
    if (!sk_X509_push(untrusted_chain, X509_dup(ark)) ||
        !sk_X509_push(untrusted_chain, X509_dup(ask)) || 
        !sk_X509_push(untrusted_chain, X509_dup(vcek))) {
        sk_X509_pop_free(untrusted_chain, X509_free);
        X509_STORE_CTX_free(ctx);
        X509_STORE_free(store);
        return 0;
    }
    
    // Initialize verification context with VCEK as target
    // The untrusted chain contains ASK and VCEK
    // OpenSSL will look for ARK (ASK's issuer) in the store
    if (!X509_STORE_CTX_init(ctx, store, vcek, untrusted_chain)) {
        NIF_LOG("Failed to initialize X509_STORE_CTX");
        sk_X509_pop_free(untrusted_chain, X509_free);
        X509_STORE_CTX_free(ctx);
        X509_STORE_free(store);
        return 0;
    }
    
    // Enable chain building - this helps OpenSSL find issuers
    X509_VERIFY_PARAM *param = X509_STORE_CTX_get0_param(ctx);
    if (param) {
        X509_VERIFY_PARAM_set_flags(param, X509_V_FLAG_ALLOW_PROXY_CERTS);
    }
    
    // Verify the chain
    // OpenSSL will automatically handle RSASSA-PSS signatures
    int verify_result = X509_verify_cert(ctx);
    
    if (verify_result == 1) {
        NIF_LOG("Certificate chain verification: SUCCESS");
    } else {
        int err = X509_STORE_CTX_get_error(ctx);
        NIF_LOG("Certificate chain verification: FAILED (error %d: %s)", 
                err, X509_verify_cert_error_string(err));
    }
    
    // Cleanup
    sk_X509_pop_free(untrusted_chain, X509_free);
    X509_STORE_CTX_free(ctx);
    X509_STORE_free(store);
    
    return verify_result == 1;
}

// NIF: verify_report_signature
// Uses OpenSSL to verify ECDSA P-384 signature, matching Rust implementation
static ERL_NIF_TERM nif_verify_report_signature(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    NIF_LOG("verify_report_signature called");
    ErlNifBinary report_binary;
    ErlNifBinary vcek_der;
    
    // Input validation
    if (!enif_inspect_binary(env, argv[0], &report_binary) || report_binary.size != 1184) {
        NIF_LOG("Invalid report binary size: %zu (expected 1184)", report_binary.size);
        return make_error(env, SNP_ERR_INVALID_INPUT, "Report binary must be exactly 1184 bytes");
    }
    
    if (!enif_inspect_binary(env, argv[1], &vcek_der)) {
        NIF_LOG("Failed to inspect VCEK DER");
        return make_error(env, SNP_ERR_INVALID_INPUT, "VCEK DER must be a binary");
    }
    
    NIF_LOG("Report size: %zu, VCEK DER size: %zu", report_binary.size, vcek_der.size);
    
    // Extract measurable bytes (first 672 bytes = 0x2A0)
    const unsigned char *measurable_bytes = report_binary.data;
    size_t measurable_size = 672;
    
    // Compute SHA-384 hash
    unsigned char hash[SHA384_DIGEST_LENGTH];
    EVP_MD_CTX *md_ctx = EVP_MD_CTX_new();
    if (!md_ctx) {
        return make_error(env, SNP_ERR_MEMORY_ERROR, "Failed to create EVP_MD_CTX");
    }
    
    if (EVP_DigestInit_ex(md_ctx, EVP_sha384(), NULL) != 1 ||
        EVP_DigestUpdate(md_ctx, measurable_bytes, measurable_size) != 1 ||
        EVP_DigestFinal_ex(md_ctx, hash, NULL) != 1) {
        EVP_MD_CTX_free(md_ctx);
        return make_error(env, SNP_ERR_INVALID_INPUT, "Failed to compute SHA-384 hash");
    }
    EVP_MD_CTX_free(md_ctx);
    
    // Parse VCEK certificate from DER
    const unsigned char *vcek_data = vcek_der.data;
    X509 *vcek = d2i_X509(NULL, &vcek_data, vcek_der.size);
    if (!vcek) {
        return make_error(env, SNP_ERR_INVALID_INPUT, "Failed to parse VCEK certificate");
    }
    
    // Extract public key from VCEK
    EVP_PKEY *pubkey = X509_get_pubkey(vcek);
    X509_free(vcek);
    if (!pubkey) {
        return make_error(env, SNP_ERR_INVALID_INPUT, "Failed to extract public key from VCEK");
    }
    
    // Verify it's an EC key on P-384
    if (EVP_PKEY_id(pubkey) != EVP_PKEY_EC) {
        EVP_PKEY_free(pubkey);
        return make_error(env, SNP_ERR_INVALID_INPUT, "VCEK public key is not an EC key");
    }
    
    EC_KEY *ec_key = EVP_PKEY_get1_EC_KEY(pubkey);
    if (!ec_key) {
        EVP_PKEY_free(pubkey);
        return make_error(env, SNP_ERR_INVALID_INPUT, "Failed to get EC key from public key");
    }
    
    const EC_GROUP *group = EC_KEY_get0_group(ec_key);
    int nid = EC_GROUP_get_curve_name(group);
    if (nid != NID_secp384r1) {
        EC_KEY_free(ec_key);
        EVP_PKEY_free(pubkey);
        return make_error(env, SNP_ERR_INVALID_INPUT, "VCEK is not on P-384 curve");
    }
    
    // Extract signature R and S from report (72 bytes each, starting at offset 1016)
    // For P-384, only the first 48 bytes of each are used (384 bits)
    const unsigned char *sig_r_le = report_binary.data + 1016;
    const unsigned char *sig_s_le = report_binary.data + 1016 + 72;
    
    // Convert from little-endian to big-endian (reverse first 48 bytes)
    unsigned char sig_r_be[48];
    unsigned char sig_s_be[48];
    for (int i = 0; i < 48; i++) {
        sig_r_be[i] = sig_r_le[47 - i];
        sig_s_be[i] = sig_s_le[47 - i];
    }
    
    // Create ECDSA signature from R and S
    BIGNUM *r = BN_bin2bn(sig_r_be, 48, NULL);
    BIGNUM *s = BN_bin2bn(sig_s_be, 48, NULL);
    if (!r || !s) {
        if (r) BN_free(r);
        if (s) BN_free(s);
        EC_KEY_free(ec_key);
        EVP_PKEY_free(pubkey);
        return make_error(env, SNP_ERR_MEMORY_ERROR, "Failed to create BIGNUM from signature");
    }
    
    ECDSA_SIG *sig = ECDSA_SIG_new();
    if (!sig) {
        BN_free(r);
        BN_free(s);
        EC_KEY_free(ec_key);
        EVP_PKEY_free(pubkey);
        return make_error(env, SNP_ERR_MEMORY_ERROR, "Failed to create ECDSA_SIG");
    }
    
    ECDSA_SIG_set0(sig, r, s);
    
    // Verify signature
    NIF_LOG("Calling ECDSA_do_verify...");
    int verify_result = ECDSA_do_verify(hash, SHA384_DIGEST_LENGTH, sig, ec_key);
    NIF_LOG("ECDSA_do_verify result: %d (1=valid, 0=invalid, -1=error)", verify_result);
    
    // Cleanup
    ECDSA_SIG_free(sig);
    EC_KEY_free(ec_key);
    EVP_PKEY_free(pubkey);
    
    if (verify_result == 1) {
        NIF_LOG("Signature verification: SUCCESS");
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "true"));
    } else if (verify_result == 0) {
        NIF_LOG("Signature verification: FAILED (invalid signature)");
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "report_signature_invalid"));
    } else {
        NIF_LOG("Signature verification: ERROR (OpenSSL error)");
        return make_error(env, SNP_ERR_INVALID_INPUT, "ECDSA verification error");
    }
}

// NIF: verify_signature_nif
// Verifies both certificate chain (ARK -> ASK -> VCEK) and report signature
static ERL_NIF_TERM nif_verify_signature_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    NIF_LOG("verify_signature_nif called");
    ErlNifBinary report_binary;
    ErlNifBinary cert_chain_der;
    ErlNifBinary vcek_der;
    
    // Input validation
    if (!enif_inspect_binary(env, argv[0], &report_binary) || report_binary.size != 1184) {
        NIF_LOG("Invalid report binary size: %zu (expected 1184)", report_binary.size);
        return make_error(env, SNP_ERR_INVALID_INPUT, "Report binary must be exactly 1184 bytes");
    }
    
    if (!enif_inspect_binary(env, argv[1], &cert_chain_der)) {
        NIF_LOG("Failed to inspect cert chain DER");
        return make_error(env, SNP_ERR_INVALID_INPUT, "Certificate chain DER must be a binary");
    }
    
    if (!enif_inspect_binary(env, argv[2], &vcek_der)) {
        NIF_LOG("Failed to inspect VCEK DER");
        return make_error(env, SNP_ERR_INVALID_INPUT, "VCEK DER must be a binary");
    }
    
    NIF_LOG("Report size: %zu, Cert chain size: %zu, VCEK DER size: %zu", 
            report_binary.size, cert_chain_der.size, vcek_der.size);
    
    // Parse certificate chain (ARK + ASK)
    const unsigned char *chain_data = cert_chain_der.data;
    STACK_OF(X509) *chain = parse_cert_chain(chain_data, cert_chain_der.size);
    if (!chain) {
        return make_error(env, SNP_ERR_INVALID_INPUT, "Failed to parse certificate chain (expected ARK + ASK)");
    }
    
    // Parse VCEK certificate
    const unsigned char *vcek_data = vcek_der.data;
    X509 *vcek = d2i_X509(NULL, &vcek_data, vcek_der.size);
    if (!vcek) {
        sk_X509_pop_free(chain, X509_free);
        return make_error(env, SNP_ERR_INVALID_INPUT, "Failed to parse VCEK certificate");
    }
    
    // Verify certificate chain: ARK -> ASK -> VCEK
    if (!verify_cert_chain(chain, vcek)) {
        X509_free(vcek);
        sk_X509_pop_free(chain, X509_free);
        return make_error(env, SNP_ERR_INVALID_INPUT, "Certificate chain verification failed");
    }
    
    // Now verify the report signature using VCEK
    // Extract measurable bytes (first 672 bytes = 0x2A0)
    const unsigned char *measurable_bytes = report_binary.data;
    size_t measurable_size = 672;
    
    // Compute SHA-384 hash
    unsigned char hash[SHA384_DIGEST_LENGTH];
    EVP_MD_CTX *md_ctx = EVP_MD_CTX_new();
    if (!md_ctx) {
        X509_free(vcek);
        sk_X509_pop_free(chain, X509_free);
        return make_error(env, SNP_ERR_MEMORY_ERROR, "Failed to create EVP_MD_CTX");
    }
    
    if (EVP_DigestInit_ex(md_ctx, EVP_sha384(), NULL) != 1 ||
        EVP_DigestUpdate(md_ctx, measurable_bytes, measurable_size) != 1 ||
        EVP_DigestFinal_ex(md_ctx, hash, NULL) != 1) {
        EVP_MD_CTX_free(md_ctx);
        X509_free(vcek);
        sk_X509_pop_free(chain, X509_free);
        return make_error(env, SNP_ERR_INVALID_INPUT, "Failed to compute SHA-384 hash");
    }
    EVP_MD_CTX_free(md_ctx);
    
    // Extract public key from VCEK
    EVP_PKEY *pubkey = X509_get_pubkey(vcek);
    if (!pubkey) {
        X509_free(vcek);
        sk_X509_pop_free(chain, X509_free);
        return make_error(env, SNP_ERR_INVALID_INPUT, "Failed to extract public key from VCEK");
    }
    
    // Verify it's an EC key on P-384
    if (EVP_PKEY_id(pubkey) != EVP_PKEY_EC) {
        EVP_PKEY_free(pubkey);
        X509_free(vcek);
        sk_X509_pop_free(chain, X509_free);
        return make_error(env, SNP_ERR_INVALID_INPUT, "VCEK public key is not an EC key");
    }
    
    EC_KEY *ec_key = EVP_PKEY_get1_EC_KEY(pubkey);
    if (!ec_key) {
        EVP_PKEY_free(pubkey);
        X509_free(vcek);
        sk_X509_pop_free(chain, X509_free);
        return make_error(env, SNP_ERR_INVALID_INPUT, "Failed to get EC key from public key");
    }
    
    const EC_GROUP *group = EC_KEY_get0_group(ec_key);
    int nid = EC_GROUP_get_curve_name(group);
    if (nid != NID_secp384r1) {
        EC_KEY_free(ec_key);
        EVP_PKEY_free(pubkey);
        X509_free(vcek);
        sk_X509_pop_free(chain, X509_free);
        return make_error(env, SNP_ERR_INVALID_INPUT, "VCEK is not on P-384 curve");
    }
    
    // Extract signature R and S from report (72 bytes each, starting at offset 1016)
    const unsigned char *sig_r_le = report_binary.data + 1016;
    const unsigned char *sig_s_le = report_binary.data + 1016 + 72;
    
    // Convert from little-endian to big-endian (reverse first 48 bytes)
    unsigned char sig_r_be[48];
    unsigned char sig_s_be[48];
    for (int i = 0; i < 48; i++) {
        sig_r_be[i] = sig_r_le[47 - i];
        sig_s_be[i] = sig_s_le[47 - i];
    }
    
    // Create ECDSA signature from R and S
    BIGNUM *r = BN_bin2bn(sig_r_be, 48, NULL);
    BIGNUM *s = BN_bin2bn(sig_s_be, 48, NULL);
    if (!r || !s) {
        if (r) BN_free(r);
        if (s) BN_free(s);
        EC_KEY_free(ec_key);
        EVP_PKEY_free(pubkey);
        X509_free(vcek);
        sk_X509_pop_free(chain, X509_free);
        return make_error(env, SNP_ERR_MEMORY_ERROR, "Failed to create BIGNUM from signature");
    }
    
    ECDSA_SIG *sig = ECDSA_SIG_new();
    if (!sig) {
        BN_free(r);
        BN_free(s);
        EC_KEY_free(ec_key);
        EVP_PKEY_free(pubkey);
        X509_free(vcek);
        sk_X509_pop_free(chain, X509_free);
        return make_error(env, SNP_ERR_MEMORY_ERROR, "Failed to create ECDSA_SIG");
    }
    
    ECDSA_SIG_set0(sig, r, s);
    
    // Verify signature
    NIF_LOG("Calling ECDSA_do_verify...");
    int verify_result = ECDSA_do_verify(hash, SHA384_DIGEST_LENGTH, sig, ec_key);
    NIF_LOG("ECDSA_do_verify result: %d (1=valid, 0=invalid, -1=error)", verify_result);
    
    // Cleanup
    ECDSA_SIG_free(sig);
    EC_KEY_free(ec_key);
    EVP_PKEY_free(pubkey);
    X509_free(vcek);
    sk_X509_pop_free(chain, X509_free);
    
    if (verify_result == 1) {
        NIF_LOG("Signature verification: SUCCESS");
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "true"));
    } else if (verify_result == 0) {
        NIF_LOG("Signature verification: FAILED (invalid signature)");
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "report_signature_invalid"));
    } else {
        NIF_LOG("Signature verification: ERROR (OpenSSL error)");
        return make_error(env, SNP_ERR_INVALID_INPUT, "ECDSA verification error");
    }
}

// NIF function table
static ErlNifFunc nif_funcs[] = {
    {"check_snp_support", 0, nif_check_snp_support},
    {"generate_attestation_report", 2, nif_generate_attestation_report},
    {"verify_report_signature", 2, nif_verify_report_signature},
    {"verify_signature_nif", 3, nif_verify_signature_nif}
};

ERL_NIF_INIT(snp_nif, nif_funcs, NULL, NULL, NULL, NULL)