#include "dev_snp_nif.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <openssl/sha.h>
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/x509.h>
#include <openssl/ecdsa.h>
#include <openssl/ec.h>
#include <openssl/bio.h>
#include <openssl/bn.h>
#include <openssl/err.h>

// SEV ioctl definitions (from Linux kernel headers)
// If linux/sev-guest.h is not available, define structures manually
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

// Structure definitions matching Linux kernel sev-guest.h
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

// Helper function to convert binary to hex string
static int binary_to_hex(const unsigned char *bin, size_t bin_len, char *hex) {
    for (size_t i = 0; i < bin_len; i++) {
        sprintf(hex + (i * 2), "%02x", bin[i]);
    }
    return 0;
}

// Error codes for better error reporting
typedef enum {
    SNP_ERR_NONE = 0,
    SNP_ERR_INVALID_INPUT,
    SNP_ERR_IOCTL_FAILED,
    SNP_ERR_FIRMWARE_ERROR,
    SNP_ERR_CERT_PARSE_FAILED,
    SNP_ERR_CERT_VERIFY_FAILED,
    SNP_ERR_SIGNATURE_VERIFY_FAILED,
    SNP_ERR_MEMORY_ERROR
} snp_error_t;

// Helper to create error tuple with error code and message
static ERL_NIF_TERM make_error(ErlNifEnv *env, snp_error_t err_code, const char *msg) {
    ERL_NIF_TERM error_code = enif_make_int(env, err_code);
    ERL_NIF_TERM error_msg = enif_make_string(env, msg, ERL_NIF_LATIN1);
    ERL_NIF_TERM error_tuple = enif_make_tuple2(env, error_code, error_msg);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), error_tuple);
}

// Helper to return binary report structure (1184 bytes)
// This is more efficient than JSON serialization and moves that responsibility to Erlang
static ERL_NIF_TERM return_report_binary(ErlNifEnv *env, struct snp_attestation_report *report) {
    ERL_NIF_TERM result;
    unsigned char *bin = enif_make_new_binary(env, sizeof(struct snp_attestation_report), &result);
    if (!bin) {
        return make_error(env, SNP_ERR_MEMORY_ERROR, "Failed to allocate binary for report");
    }
    memcpy(bin, report, sizeof(struct snp_attestation_report));
    return result;
}

// NIF: check_snp_support
static ERL_NIF_TERM nif_check_snp_support(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int fd = open("/dev/sev-guest", O_RDONLY);
    if (fd < 0) {
        // Device not available - not an error, just unsupported
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "false"));
    }
    close(fd);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "true"));
}

// NIF: generate_attestation_report
// Returns binary report structure (1184 bytes) instead of JSON
// JSON serialization is handled in Erlang for better error handling and maintainability
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
    
    // Perform ioctl
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
    if (resp.report_size != sizeof(struct snp_attestation_report)) {
        char err_msg[256];
        snprintf(err_msg, sizeof(err_msg), "Invalid report size: expected %zu, got %u", 
                 sizeof(struct snp_attestation_report), resp.report_size);
        return make_error(env, SNP_ERR_INVALID_INPUT, err_msg);
    }
    
    // Parse the report structure
    struct snp_attestation_report *report = (struct snp_attestation_report *)resp.report;
    
    // Return binary report structure (JSON serialization moved to Erlang)
    ERL_NIF_TERM report_binary = return_report_binary(env, report);
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), report_binary);
}

// Forward declaration
extern int compute_launch_digest(uint32_t vcpus, uint8_t vcpu_type, uint8_t vmm_type,
                                 uint64_t guest_features, const char *ovmf_hash_hex,
                                 const unsigned char *kernel_hash,
                                 const unsigned char *initrd_hash,
                                 const unsigned char *append_hash,
                                 unsigned char *output_digest);

// Helper to decode hex string to binary
static int hex_to_binary(const char *hex, unsigned char *bin, size_t bin_len) {
    size_t hex_len = strlen(hex);
    if (hex_len != bin_len * 2) {
        return -1;
    }
    for (size_t i = 0; i < bin_len; i++) {
        char hex_byte[3] = {hex[i*2], hex[i*2+1], 0};
        char *endptr;
        bin[i] = (unsigned char)strtoul(hex_byte, &endptr, 16);
        if (*endptr != '\0') {
            return -1;
        }
    }
    return 0;
}

// Helper to get a binary value from Erlang map by atom key
static int get_map_binary(ErlNifEnv *env, ERL_NIF_TERM map, const char *key_atom, ErlNifBinary *bin) {
    ERL_NIF_TERM key = enif_make_atom(env, key_atom);
    ERL_NIF_TERM value;
    if (!enif_get_map_value(env, map, key, &value)) {
        return 0;
    }
    if (!enif_inspect_binary(env, value, bin)) {
        return 0;
    }
    return 1;
}

// Helper to get an integer value from Erlang map by atom key
static int get_map_uint(ErlNifEnv *env, ERL_NIF_TERM map, const char *key_atom, unsigned int *val) {
    ERL_NIF_TERM key = enif_make_atom(env, key_atom);
    ERL_NIF_TERM value;
    if (!enif_get_map_value(env, map, key, &value)) {
        return 0;
    }
    if (!enif_get_uint(env, value, val)) {
        return 0;
    }
    return 1;
}

// Helper to convert hex string binary to raw binary
static int hex_binary_to_raw(const ErlNifBinary *hex_bin, unsigned char *raw, size_t raw_len) {
    if (hex_bin->size != raw_len * 2) {
        return 0;
    }
    for (size_t i = 0; i < raw_len; i++) {
        char hex_byte[3] = {hex_bin->data[i*2], hex_bin->data[i*2+1], 0};
        char *endptr;
        unsigned long val = strtoul(hex_byte, &endptr, 16);
        if (*endptr != '\0' || val > 255) {
            return 0;
        }
        raw[i] = (unsigned char)val;
    }
    return 1;
}

// NIF: compute_launch_digest
static ERL_NIF_TERM nif_compute_launch_digest(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM map;
    uint32_t vcpus;
    uint8_t vcpu_type;
    uint8_t vmm_type;
    uint64_t guest_features;
    
    // Parse input map
    if (argc != 1 || !enif_is_map(env, argv[0])) {
        return make_error(env, SNP_ERR_INVALID_INPUT, "Expected a map as argument");
    }
    map = argv[0];
    
    // Extract required parameters
    unsigned int vcpus_uint, vcpu_type_uint, vmm_type_uint;
    if (!get_map_uint(env, map, "vcpus", &vcpus_uint)) {
        return make_error(env, SNP_ERR_INVALID_INPUT, "Missing or invalid vcpus");
    }
    vcpus = (uint32_t)vcpus_uint;
    
    if (!get_map_uint(env, map, "vcpu_type", &vcpu_type_uint) || vcpu_type_uint > 255) {
        return make_error(env, SNP_ERR_INVALID_INPUT, "Missing or invalid vcpu_type");
    }
    vcpu_type = (uint8_t)vcpu_type_uint;
    
    if (!get_map_uint(env, map, "vmm_type", &vmm_type_uint) || vmm_type_uint > 255) {
        return make_error(env, SNP_ERR_INVALID_INPUT, "Missing or invalid vmm_type");
    }
    vmm_type = (uint8_t)vmm_type_uint;
    
    unsigned int guest_features_uint;
    if (!get_map_uint(env, map, "guest_features", &guest_features_uint)) {
        guest_features = 0;  // Default to 0 if not provided
    } else {
        guest_features = (uint64_t)guest_features_uint;
    }
    
    // Extract firmware hash (OVMF hash) - hex string
    ErlNifBinary firmware_bin;
    const char *ovmf_hash_hex = NULL;
    char ovmf_hash_hex_buf[97];  // 96 chars + null terminator
    if (get_map_binary(env, map, "firmware", &firmware_bin)) {
        if (firmware_bin.size == 96) {  // 48 bytes * 2 hex chars
            memcpy(ovmf_hash_hex_buf, firmware_bin.data, 96);
            ovmf_hash_hex_buf[96] = '\0';
            ovmf_hash_hex = ovmf_hash_hex_buf;
        }
    }
    
    // Extract kernel, initrd, append hashes (SHA-256, 32 bytes each)
    ErlNifBinary kernel_bin, initrd_bin, append_bin;
    unsigned char kernel_hash[32] = {0};
    unsigned char initrd_hash[32] = {0};
    unsigned char append_hash[32] = {0};
    const unsigned char *kernel_hash_ptr = NULL;
    const unsigned char *initrd_hash_ptr = NULL;
    const unsigned char *append_hash_ptr = NULL;
    
    if (get_map_binary(env, map, "kernel", &kernel_bin)) {
        if (kernel_bin.size == 64) {  // 32 bytes * 2 hex chars
            if (hex_binary_to_raw(&kernel_bin, kernel_hash, 32)) {
                kernel_hash_ptr = kernel_hash;
            }
        } else if (kernel_bin.size == 32) {
            // Already raw binary
            memcpy(kernel_hash, kernel_bin.data, 32);
            kernel_hash_ptr = kernel_hash;
        }
    }
    
    if (get_map_binary(env, map, "initrd", &initrd_bin)) {
        if (initrd_bin.size == 64) {  // 32 bytes * 2 hex chars
            if (hex_binary_to_raw(&initrd_bin, initrd_hash, 32)) {
                initrd_hash_ptr = initrd_hash;
            }
        } else if (initrd_bin.size == 32) {
            // Already raw binary
            memcpy(initrd_hash, initrd_bin.data, 32);
            initrd_hash_ptr = initrd_hash;
        }
    }
    
    if (get_map_binary(env, map, "append", &append_bin)) {
        if (append_bin.size == 64) {  // 32 bytes * 2 hex chars
            if (hex_binary_to_raw(&append_bin, append_hash, 32)) {
                append_hash_ptr = append_hash;
            }
        } else if (append_bin.size == 32) {
            // Already raw binary
            memcpy(append_hash, append_bin.data, 32);
            append_hash_ptr = append_hash;
        }
    }
    
    // Compute launch digest
    unsigned char output_digest[48];
    int ret = compute_launch_digest(
        vcpus,
        vcpu_type,
        vmm_type,
        guest_features,
        ovmf_hash_hex,
        kernel_hash_ptr,
        initrd_hash_ptr,
        append_hash_ptr,
        output_digest
    );
    
    if (ret != 0) {
        return make_error(env, SNP_ERR_MEMORY_ERROR, "Failed to compute launch digest");
    }
    
    // Return digest as binary
    ERL_NIF_TERM result_bin;
    unsigned char *dest = enif_make_new_binary(env, 48, &result_bin);
    memcpy(dest, output_digest, 48);
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result_bin);
}

// NIF: verify_signature
// Accepts binary report structure (1184 bytes) instead of JSON for better performance
// Certificate chain and VCEK are passed as DER-encoded binaries
static ERL_NIF_TERM nif_verify_signature(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary report_binary, cert_chain_der, vcek_der;
    
    // Input validation
    if (!enif_inspect_binary(env, argv[0], &report_binary) ||
        report_binary.size != sizeof(struct snp_attestation_report)) {
        return make_error(env, SNP_ERR_INVALID_INPUT, 
                         "Report binary must be exactly 1184 bytes");
    }
    
    if (!enif_inspect_binary(env, argv[1], &cert_chain_der) || cert_chain_der.size == 0) {
        return make_error(env, SNP_ERR_INVALID_INPUT, 
                         "Certificate chain DER is required");
    }
    
    if (!enif_inspect_binary(env, argv[2], &vcek_der) || vcek_der.size == 0) {
        return make_error(env, SNP_ERR_INVALID_INPUT, 
                         "VCEK certificate DER is required");
    }
    
    // Parse certificate chain (ASK + ARK) from concatenated DER
    // The chain is concatenated DER: ASK DER + ARK DER
    const unsigned char *ptr = cert_chain_der.data;
    size_t remaining = cert_chain_der.size;
    
    // Parse ASK (first certificate)
    const unsigned char *ask_ptr = ptr;
    X509 *ask = d2i_X509(NULL, &ask_ptr, remaining);
    if (!ask) {
        unsigned long err = ERR_get_error();
        char err_buf[256];
        ERR_error_string_n(err, err_buf, sizeof(err_buf));
        char err_msg[512];
        snprintf(err_msg, sizeof(err_msg), "Failed to parse ASK certificate (first in chain): %s", err_buf);
        return make_error(env, SNP_ERR_CERT_PARSE_FAILED, err_msg);
    }
    
    size_t ask_size = ask_ptr - ptr;
    remaining -= ask_size;
    
    if (remaining == 0) {
        X509_free(ask);
        return make_error(env, SNP_ERR_CERT_PARSE_FAILED, 
                         "Certificate chain incomplete: missing ARK certificate");
    }
    
    // Parse ARK (second certificate)
    const unsigned char *ark_ptr = ask_ptr;
    X509 *ark = d2i_X509(NULL, &ark_ptr, remaining);
    if (!ark) {
        unsigned long err = ERR_get_error();
        char err_buf[256];
        ERR_error_string_n(err, err_buf, sizeof(err_buf));
        char err_msg[512];
        snprintf(err_msg, sizeof(err_msg), "Failed to parse ARK certificate (second in chain): %s", err_buf);
        X509_free(ask);
        return make_error(env, SNP_ERR_CERT_PARSE_FAILED, err_msg);
    }
    
    // Verify ARK is self-signed
    if (verify_ark_self_signed(ark) != 0) {
        unsigned long err = ERR_get_error();
        char err_buf[256];
        ERR_error_string_n(err, err_buf, sizeof(err_buf));
        char err_msg[512];
        snprintf(err_msg, sizeof(err_msg), "ARK self-signature verification failed: %s", err_buf);
        X509_free(ark);
        X509_free(ask);
        return make_error(env, SNP_ERR_CERT_VERIFY_FAILED, err_msg);
    }
    
    // Verify ASK is signed by ARK
    if (verify_ask_signed_by_ark(ask, ark) != 0) {
        unsigned long err = ERR_get_error();
        char err_buf[256];
        ERR_error_string_n(err, err_buf, sizeof(err_buf));
        char err_msg[512];
        snprintf(err_msg, sizeof(err_msg), "ASK signature verification failed (not signed by ARK): %s", err_buf);
        X509_free(ark);
        X509_free(ask);
        return make_error(env, SNP_ERR_CERT_VERIFY_FAILED, err_msg);
    }
    
    // Parse VCEK certificate
    const unsigned char *vcek_ptr = vcek_der.data;
    X509 *vcek = d2i_X509(NULL, &vcek_ptr, vcek_der.size);
    if (!vcek) {
        unsigned long err = ERR_get_error();
        char err_buf[256];
        ERR_error_string_n(err, err_buf, sizeof(err_buf));
        char err_msg[512];
        snprintf(err_msg, sizeof(err_msg), "Failed to parse VCEK certificate: %s", err_buf);
        X509_free(ark);
        X509_free(ask);
        return make_error(env, SNP_ERR_CERT_PARSE_FAILED, err_msg);
    }
    
    // Verify VCEK is signed by ASK
    if (verify_vcek_signed_by_ask(vcek, ask) != 0) {
        unsigned long err = ERR_get_error();
        char err_buf[256];
        ERR_error_string_n(err, err_buf, sizeof(err_buf));
        char err_msg[512];
        snprintf(err_msg, sizeof(err_msg), "VCEK signature verification failed (not signed by ASK): %s", err_buf);
        X509_free(ark);
        X509_free(ask);
        X509_free(vcek);
        return make_error(env, SNP_ERR_CERT_VERIFY_FAILED, err_msg);
    }
    
    // Parse report from binary
    struct snp_attestation_report *report = (struct snp_attestation_report *)report_binary.data;
    
    // Verify report signature
    if (verify_report_signature(report, vcek) != 0) {
        unsigned long err = ERR_get_error();
        char err_buf[256];
        ERR_error_string_n(err, err_buf, sizeof(err_buf));
        char err_msg[512];
        snprintf(err_msg, sizeof(err_msg), "Report signature verification failed: %s", err_buf);
        X509_free(ark);
        X509_free(ask);
        X509_free(vcek);
        return make_error(env, SNP_ERR_SIGNATURE_VERIFY_FAILED, err_msg);
    }
    
    // All verifications passed
    X509_free(ark);
    X509_free(ask);
    X509_free(vcek);
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "true"));
}

// NIF function table
static ErlNifFunc nif_funcs[] = {
    {"check_snp_support", 0, nif_check_snp_support},
    {"generate_attestation_report", 2, nif_generate_attestation_report},
    {"compute_launch_digest", 1, nif_compute_launch_digest},
    {"verify_signature", 3, nif_verify_signature}
};

ERL_NIF_INIT(dev_snp_nif, nif_funcs, NULL, NULL, NULL, NULL)

