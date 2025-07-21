#ifndef TPM_ESAPI_H
#define TPM_ESAPI_H

// TPM Enhanced System API (ESAPI) wrapper for Erlang NIF
// This header provides a simplified C interface to TPM 2.0 functionality
// using the TSS2 Enhanced System API library.

#include <tss2/tss2_esys.h>    // TPM Enhanced System API
#include <tss2/tss2_tctildr.h> // TPM Command Transmission Interface Loader
#include <tss2/tss2_rc.h>      // TPM Return Codes
#include <stdint.h>
#include <stdbool.h>

// Maximum sizes for various TPM data structures
// These limits are based on TPM 2.0 specification and practical considerations
#define TPM_MAX_PCR_VALUE_SIZE 64    // Max size for PCR values (SHA512 = 64 bytes)
#define TPM_MAX_QUOTE_SIZE 2048      // Max size for TPM quote/attestation data
#define TPM_MAX_RANDOM_SIZE 64       // Max random bytes per TPM_GetRandom call
#define TPM_MAX_DIGEST_SIZE 64       // Max digest size (SHA512 = 64 bytes)
#define TPM_MAX_SIGN_DATA_SIZE 1024  // Max size for data to be signed directly

/**
 * TPM context structure
 * 
 * This structure maintains the state of a TPM session, including the
 * Enhanced System API context and initialization status.
 */
typedef struct {
    ESYS_CONTEXT *esys_context;  // TSS2 Enhanced System API context
    bool initialized;            // Flag indicating if context is properly initialized
} tpm_context_t;

// === Context Management Functions ===

/**
 * Initialize a TPM context
 * 
 * Attempts to connect to a TPM device using various TCTI (TPM Command
 * Transmission Interface) configurations in order of preference.
 * 
 * @param ctx Pointer to TPM context structure to initialize
 * @return 0 on success, -1 on error, -2 if no TPM device is available
 */
int tpm_init_context(tpm_context_t *ctx);

/**
 * Clean up and finalize a TPM context
 * 
 * Releases all resources associated with the TPM context.
 * Safe to call multiple times or on uninitialized contexts.
 * 
 * @param ctx Pointer to TPM context to clean up
 */
void tpm_cleanup_context(tpm_context_t *ctx);

// === TPM Capability and Status Functions ===

/**
 * Check if TPM support is available on this system
 * 
 * Performs a lightweight test to determine if a TPM device is accessible
 * and can be used for cryptographic operations.
 * 
 * @return 1 if TPM is supported and available, 0 otherwise
 */
int tpm_check_support(void);

// === TPM Platform Configuration Register (PCR) Functions ===

/**
 * Read a Platform Configuration Register (PCR) value
 * 
 * PCRs are special registers in the TPM that store hash measurements
 * of system state. They are fundamental to trusted computing and attestation.
 * 
 * @param ctx Initialized TPM context
 * @param pcr_index PCR index to read (typically 0-23)
 * @param pcr_value Buffer to store the PCR value
 * @param pcr_size In: size of buffer, Out: actual size of PCR value
 * @return 0 on success, -1 on error
 */
int tpm_read_pcr(tpm_context_t *ctx, uint32_t pcr_index, uint8_t *pcr_value, size_t *pcr_size);

// === TPM Random Number Generation ===

/**
 * Generate cryptographically secure random bytes using TPM hardware
 * 
 * The TPM contains a hardware random number generator that can provide
 * high-quality entropy for cryptographic operations.
 * 
 * @param ctx Initialized TPM context
 * @param requested_bytes Number of random bytes to generate
 * @param random_data Buffer to store the random data
 * @param actual_size In: size of buffer, Out: actual bytes generated
 * @return 0 on success, -1 on error
 */
int tpm_get_random(tpm_context_t *ctx, size_t requested_bytes, uint8_t *random_data, size_t *actual_size);

// === TPM Key Management Functions ===

/**
 * Create a primary key in the TPM
 * 
 * Creates an RSA 2048-bit signing key in the owner hierarchy.
 * This key can be used for signing operations and as a parent for other keys.
 * 
 * @param ctx Initialized TPM context
 * @param primary_handle Output handle for the created key
 * @return 0 on success, -1 on error
 */
int tpm_create_primary_key(tpm_context_t *ctx, ESYS_TR *primary_handle);

/**
 * Clean up a TPM key handle and free associated memory
 * 
 * Flushes a key handle from the TPM, releasing memory and resources.
 * Important for preventing TPM resource exhaustion.
 * 
 * @param ctx Initialized TPM context
 * @param key_handle Handle to the key to be flushed
 * @return 0 on success, -1 on error
 */
int tpm_flush_key(tpm_context_t *ctx, ESYS_TR key_handle);

/**
 * Sign data using a TPM key
 * 
 * Uses a TPM-resident key to generate a digital signature over the provided data.
 * The signature scheme used is RSAPSS with SHA256.
 * 
 * @param ctx Initialized TPM context
 * @param key_handle Handle to the signing key
 * @param data Data to be signed
 * @param data_size Size of data to be signed
 * @param signature Buffer to store the signature
 * @param signature_size In: size of buffer, Out: actual signature size
 * @return 0 on success, -1 on error
 */
int tpm_sign_data(tpm_context_t *ctx, ESYS_TR key_handle, const uint8_t *data, size_t data_size,
                  uint8_t *signature, size_t *signature_size);

// === TPM Clock and Time Functions ===

/**
 * Read the TPM's internal clock and time information
 * 
 * The TPM maintains a monotonic clock that provides timing information
 * and tracks system resets/restarts for security purposes.
 * 
 * @param ctx Initialized TPM context
 * @param current_time Current time value from TPM clock
 * @param reset_count Number of TPM resets since manufacturing
 * @param restart_count Number of TPM restarts since last reset
 * @param safe Flag indicating if clock is considered safe/reliable
 * @return 0 on success, -1 on error
 */
int tpm_read_clock(tpm_context_t *ctx, uint64_t *current_time, uint32_t *reset_count, 
                   uint32_t *restart_count, uint8_t *safe);

#endif // TPM_ESAPI_H 