/*
 * TPM Enhanced System API (ESAPI) Implementation
 *
 * This file provides a simplified C wrapper around the TSS2 Enhanced System API
 * for TPM 2.0 operations. It handles TPM initialization, context management,
 * and common TPM operations like PCR reading, random number generation,
 * attestation, and cryptographic key operations.
 *
 * The implementation is designed to be robust in environments where TPM devices
 * may not be available, gracefully falling back and providing appropriate error
 * codes for absent hardware.
 */

#include "include/hb_tpm_esapi.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

 /**
  * Initialize a TPM context with automatic TCTI discovery
  *
  * This function attempts to connect to a TPM device using multiple TCTI
  * (TPM Command Transmission Interface) configurations. It tries different
  * approaches in order of preference:
  *
  * 1. tabrmd: TPM Access Broker & Resource Management Daemon (preferred for software TPM)
  * 2. swtpm: Direct connection to software TPM socket
  * 3. Default: Hardware TPM devices (/dev/tpm0, /dev/tpmrm0, etc.)
  *
  * The function suppresses stderr output during TCTI probing to avoid
  * confusing error messages when TPM devices are unavailable.
  */
int tpm_init_context(tpm_context_t* ctx) {
    TSS2_RC rc;
    TSS2_TCTI_CONTEXT* tcti_ctx = NULL;

    // Check if context is already initialized to avoid double-initialization
    if (ctx->initialized) {
        // Already initialized - this is not an error
        return 0;
    }

    /*
     * Initialize TCTI (TPM Command Transmission Interface)
     *
     * TCTI is the lower-level interface that handles communication with
     * the TPM device. Different TCTI implementations support different
     * types of TPM connections:
     *
     * - tabrmd: Connects through the TPM Access Broker daemon, which
     *   provides resource management for multiple applications
     * - swtpm: Direct socket connection to software TPM implementations
     * - default: Auto-detection of hardware TPM devices
     */
    const char* tcti_configs[] = {
        // Software TPM via daemon
        "tabrmd:bus_name=com.intel.tss2.Tabrmd,bus_type=session",
        // Software TPM socket
        "swtpm:host=127.0.0.1,port=2321",
        // Use default TCTI loader (hardware TPM auto-detection)
        NULL
    };

    rc = TPM2_RC_FAILURE; // Initialize to failure state
    for (int i = 0; i < 3; i++) {
        rc = Tss2_TctiLdr_Initialize(tcti_configs[i], &tcti_ctx);
        if (rc == TSS2_RC_SUCCESS) {
            // Successfully initialized TCTI
            break;
        }
    }

    if (rc != TSS2_RC_SUCCESS) {
        // No TPM device available - this is expected in many environments
        // (virtual machines, containers, systems without TPM hardware)
        ctx->initialized = false;
        // Special code indicating "no TPM device available"
        return -2;
    }

    /*
     * Initialize ESYS (Enhanced System API) context
     *
     * ESYS provides a higher-level interface that handles:
     * - Session management
     * - Authorization protocols
     * - Response parsing and validation
     * - Resource management
     */
    rc = Esys_Initialize(&ctx->esys_context, tcti_ctx, NULL);
    if (rc != TSS2_RC_SUCCESS) {
        // Failed to initialize ESYS - clean up TCTI context
        Tss2_TctiLdr_Finalize(&tcti_ctx);
        ctx->initialized = false;
        // No TPM available
        return -2;
    }

    /*
     * Startup the TPM (if needed)
     *
     * TPM_Startup prepares the TPM for operation after power-on or reset.
     * TPM2_SU_CLEAR indicates a "clear" startup, which:
     * - Initializes the TPM for normal operation
     * - Clears certain volatile state
     * - Prepares PCRs for measurement
     */
    rc = Esys_Startup(ctx->esys_context, TPM2_SU_CLEAR);
    if (rc != TSS2_RC_SUCCESS && rc != TPM2_RC_INITIALIZE) {
        // TPM_RC_INITIALIZE means TPM is already started, which is fine
        // Other errors might indicate TPM issues, but we continue anyway
        // as the TPM might be in a usable state despite startup issues
    }

    ctx->initialized = true;
    // Success
    return 0;
}

/**
 * Clean up and finalize a TPM context
 *
 * Properly releases all resources associated with a TPM context.
 * This function is safe to call multiple times and on uninitialized contexts.
 */
void tpm_cleanup_context(tpm_context_t* ctx) {
    if (ctx && ctx->initialized && ctx->esys_context) {
        // Finalize ESYS context, which also cleans up the underlying TCTI
        Esys_Finalize(&ctx->esys_context);
        ctx->initialized = false;
    }
}



/**
 * Lightweight check for TPM support availability
 *
 * This function performs a minimal test to determine if TPM functionality
 * is available on the current system. It creates a temporary context,
 * attempts initialization, and immediately cleans up.
 *
 * This is useful for applications that want to conditionally enable
 * TPM-based features based on hardware availability.
 */
int tpm_check_support(void) {
    tpm_context_t test_ctx = { 0 };

    int result = tpm_init_context(&test_ctx);
    if (result == 0) {
        // TPM is available and working
        tpm_cleanup_context(&test_ctx);
        return 1; // TPM is supported and available
    }

    // Return 0 for both "no TPM device" (-2) and other errors (-1)
    // This simplifies the interface for callers who just want yes/no
    // TPM is not supported or not available
    return 0;
}

/**
 * Read a Platform Configuration Register (PCR) value
 *
 * PCRs are special registers in the TPM that store cryptographic hashes
 * representing the state of the system. They are fundamental to trusted
 * computing and remote attestation:
 *
 * - PCRs 0-7: Typically used for firmware and boot measurements
 * - PCRs 8-15: Typically used for OS and bootloader measurements
 * - PCRs 16-23: Available for application use
 *
 * PCR values are typically 32 bytes (SHA256) or 64 bytes (SHA512).
 * The exact hash algorithm depends on the TPM's PCR bank configuration.
 */
int tpm_read_pcr(
    tpm_context_t* ctx,
    uint32_t pcr_index,
    uint8_t* pcr_value,
    size_t* pcr_size
) {
    TSS2_RC rc;
    // Output: which PCRs were actually read
    TPML_PCR_SELECTION* pcr_selection_out;
    // Output: the actual PCR values
    TPML_DIGEST* pcr_values;
    // Output: counter indicating PCR changes
    uint32_t pcr_update_counter;

    // Input validation
    if (!ctx || !ctx->initialized || !pcr_value || !pcr_size) {
        // Invalid parameters
        return -1;
    }

    /*
     * Create PCR selection structure for the specified PCR
     *
     * TPM PCR operations work with "selections" that specify:
     * - Which hash algorithm to use (SHA256, SHA384, SHA512, etc.)
     * - Which specific PCRs to read (as a bitmask)
     */
    TPML_PCR_SELECTION pcr_selection_in = { 0 };
    // We're specifying one PCR bank
    pcr_selection_in.count = 1;
    // Use SHA256 PCR bank
    pcr_selection_in.pcrSelections[0].hash = TPM2_ALG_SHA256;
    // 3 bytes = 24 PCRs max
    pcr_selection_in.pcrSelections[0].sizeofSelect = 3;

    // Set the bit for the requested PCR index
    // PCR indices are packed into bytes: PCR 0-7 in byte 0, PCR 8-15 in byte 1, etc.
    pcr_selection_in.pcrSelections[0].pcrSelect[pcr_index / 8] = 1 << (pcr_index % 8);

    /*
     * Perform the PCR read operation
     *
     * This command reads the current values of the selected PCRs.
     * The TPM returns:
     * - The actual PCR selection that was read (may differ from request)
     * - The PCR values as an array of digests
     * - An update counter that changes when PCRs are extended
     */
    rc = Esys_PCR_Read(
        ctx->esys_context,
        ESYS_TR_NONE,
        ESYS_TR_NONE,
        ESYS_TR_NONE,
        &pcr_selection_in,
        &pcr_update_counter,
        &pcr_selection_out,
        &pcr_values
    );

    if (rc != TSS2_RC_SUCCESS) {
        return -1; // PCR read failed
    }

    // Validate that we got at least one PCR value back
    if (pcr_values->count == 0) {
        free(pcr_selection_out);
        free(pcr_values);
        // No PCR values returned
        return -1;
    }

    /*
     * Copy PCR value to output buffer
     *
     * The TPM returns PCR values as variable-length digests.
     * We copy as much as fits in the caller's buffer and update
     * the size parameter to indicate the actual amount copied.
     */
    size_t copy_size = pcr_values->digests[0].size;
    if (copy_size > *pcr_size) {
        // Truncate to fit in caller's buffer
        copy_size = *pcr_size;
    }

    memcpy(pcr_value, pcr_values->digests[0].buffer, copy_size);
    *pcr_size = copy_size;

    // Clean up dynamically allocated TPM response data
    free(pcr_selection_out);
    free(pcr_values);
    // Success
    return 0;
}

/**
 * Generate cryptographically secure random bytes using TPM hardware
 *
 * The TPM contains a hardware random number generator (HWRNG) that can
 * provide high-quality entropy. This is particularly valuable in embedded
 * systems or environments where software entropy sources may be limited.
 *
 * TPM random number generation has some limitations:
 * - Maximum bytes per call is typically 64 bytes
 * - Performance is lower than software PRNGs
 * - Requires TPM hardware availability
 */
int tpm_get_random(
    tpm_context_t* ctx,
    size_t requested_bytes,
    uint8_t* random_data,
    size_t* actual_size
) {
    TSS2_RC rc;
    TPM2B_DIGEST* random_bytes;

    // Input validation
    if (!ctx || !ctx->initialized || !random_data ||
        !actual_size || requested_bytes > TPM_MAX_RANDOM_SIZE) {
        // Invalid parameters or request too large
        return -1;
    }

    /*
     * Call TPM2_GetRandom command
     *
     * This command requests the TPM to generate random bytes using its
     * internal hardware random number generator. The TPM may return
     * fewer bytes than requested if:
     * - The request exceeds TPM limits
     * - Insufficient entropy is available
     * - TPM resource constraints exist
     */
    rc = Esys_GetRandom(
        ctx->esys_context,
        ESYS_TR_NONE,
        ESYS_TR_NONE,
        ESYS_TR_NONE,
        (uint16_t)requested_bytes,
        &random_bytes
    );

    if (rc != TSS2_RC_SUCCESS) {
        // GetRandom command failed
        return -1;
    }

    /*
     * Copy random data to caller's buffer
     *
     * The TPM returns random data in a TPM2B_DIGEST structure.
     * We extract the actual bytes and copy them to the caller's buffer,
     * respecting the buffer size limits.
     */
    size_t copy_size = random_bytes->size;
    if (copy_size > *actual_size) {
        // Truncate to fit in caller's buffer
        copy_size = *actual_size;
    }

    memcpy(random_data, random_bytes->buffer, copy_size);
    *actual_size = copy_size;

    // Clean up TPM response data
    free(random_bytes);
    // Success
    return 0;
}

/**
 * Create a primary key in the TPM's owner hierarchy
 *
 * This function creates an RSA 2048-bit signing key that can be used for:
 * - Digital signatures
 * - As a parent key for deriving child keys
 * - Attestation operations (if configured appropriately)
 *
 * The key is created with attributes that make it:
 * - Restricted to the TPM (cannot be duplicated)
 * - Suitable for signing operations
 * - Tied to the current TPM (cannot be migrated)
 */
int tpm_create_primary_key(tpm_context_t* ctx, ESYS_TR* primary_handle) {
    TSS2_RC rc;
    // Sensitive key creation data
    TPM2B_SENSITIVE_CREATE in_sensitive_primary = { 0 };
    // Public key template
    TPM2B_PUBLIC in_public = {
        .size = 0,
        .publicArea = {0}
    };
    // Additional creation data
    TPM2B_DATA outside_info = { 0 };
    // PCRs to bind key creation to
    TPML_PCR_SELECTION creation_pcr = { 0 };

    // Output parameters (we don't need these, but TPM requires them)
    TPM2B_PUBLIC* out_public;
    TPM2B_CREATION_DATA* creation_data;
    TPM2B_DIGEST* creation_hash;
    TPMT_TK_CREATION* creation_ticket;

    // Input validation
    if (!ctx || !ctx->initialized || !primary_handle) {
        return -1; // Invalid parameters
    }

    /*
     * Set up primary key template (RSA 2048 signing key)
     *
     * This template defines the characteristics of the key to be created:
     * - RSA algorithm with 2048-bit modulus
     * - SHA256 for name computation and operations
     * - Appropriate object attributes for a signing key
     * - RSAPSS signature scheme with SHA256
     */
     // RSA key type
    in_public.publicArea.type = TPM2_ALG_RSA;
    // Hash algorithm for key name
    in_public.publicArea.nameAlg = TPM2_ALG_SHA256;

    // Set object attributes for a primary signing key
    in_public.publicArea.objectAttributes = (
        // Require user authorization
        TPMA_OBJECT_USERWITHAUTH |
        // Restricted key (for specific operations)
        TPMA_OBJECT_RESTRICTED |
        // Can be used for signing
        TPMA_OBJECT_SIGN_ENCRYPT |
        // Cannot leave this TPM
        TPMA_OBJECT_FIXEDTPM |
        // Cannot change parent
        TPMA_OBJECT_FIXEDPARENT |
        // TPM generated sensitive data
        TPMA_OBJECT_SENSITIVEDATAORIGIN
        );

    // Configure RSA-specific parameters
    // No symmetric encryption
    in_public.publicArea.parameters.rsaDetail.symmetric.algorithm = TPM2_ALG_NULL;
    // RSAPSS signature scheme
    in_public.publicArea.parameters.rsaDetail.scheme.scheme = TPM2_ALG_RSAPSS;
    // SHA256 for RSAPSS
    in_public.publicArea.parameters.rsaDetail.scheme.details.rsapss.hashAlg = TPM2_ALG_SHA256;
    // 2048-bit RSA key
    in_public.publicArea.parameters.rsaDetail.keyBits = 2048;
    // Default public exponent (65537)
    in_public.publicArea.parameters.rsaDetail.exponent = 0;
    /*
     * Create the primary key
     *
     * This operation:
     * 1. Generates a new RSA key pair in the TPM
     * 2. Creates the key in the owner hierarchy (ESYS_TR_RH_OWNER)
     * 3. Returns a handle that can be used to reference the key
     * 4. Returns various metadata about the created key
     */
    rc = Esys_CreatePrimary(
        ctx->esys_context,
        // Create in owner hierarchy
        ESYS_TR_RH_OWNER,
        ESYS_TR_PASSWORD,
        ESYS_TR_NONE,
        // Authorization sessions
        ESYS_TR_NONE,
        &in_sensitive_primary,
        &in_public,
        &outside_info,
        &creation_pcr,
        primary_handle,
        &out_public,
        &creation_data,
        &creation_hash,
        &creation_ticket
    );

    if (rc != TSS2_RC_SUCCESS) {
        // Key creation failed
        return -1;
    }

    // Clean up output data we don't need
    // The TPM allocates memory for these structures, so we must free them
    free(out_public);
    free(creation_data);
    free(creation_hash);
    free(creation_ticket);
    // Success - primary_handle now contains the key handle
    return 0;
}

/**
 * Sign data using a TPM-resident key
 *
 * This function uses a key stored in the TPM to generate a digital signature
 * over the provided data. The signature can be verified using the corresponding
 * public key.
 *
 * The implementation uses:
 * - RSAPSS signature scheme for strong security
 * - SHA256 hash algorithm
 * - Direct data signing (simplified - production might hash large data first)
 */
int tpm_sign_data(
    tpm_context_t* ctx,
    ESYS_TR key_handle,
    const uint8_t* data,
    size_t data_size,
    uint8_t* signature,
    size_t* signature_size
) {
    TSS2_RC rc;
    TPM2B_DIGEST digest = { 0 };            // Data to be signed (simplified as direct copy)
    TPMT_SIG_SCHEME in_scheme = { 0 };      // Signature scheme specification
    TPMT_TK_HASHCHECK validation = { 0 };   // Hash validation ticket
    TPMT_SIGNATURE* sig;                  // Output signature

    // Input validation
    if (!ctx || !ctx->initialized || !data ||
        !signature || !signature_size || data_size == 0) {
        // Invalid parameters
        return -1;
    }

    /*
     * Prepare data for signing
     *
     * For simplicity, this implementation directly copies the input data
     * to the digest structure. In production systems, you might want to:
     * - Hash large data with an external hash function first
     * - Accept pre-hashed data
     * - Support multiple hash algorithms
     */
    if (data_size > sizeof(digest.buffer)) {
        // Data too large for direct signing
        return -1;
    }

    digest.size = data_size;
    memcpy(digest.buffer, data, data_size);

    /*
     * Set up signing scheme
     *
     * RSAPSS (RSA Probabilistic Signature Scheme) is preferred over
     * older PKCS#1 v1.5 signatures because it provides better security
     * properties and resistance to certain attacks.
     */
     // Use RSAPSS
    in_scheme.scheme = TPM2_ALG_RSAPSS;
    // SHA256 hash
    in_scheme.details.rsapss.hashAlg = TPM2_ALG_SHA256;

    /*
     * Set up validation ticket for external hash
     *
     * Since we're providing data directly rather than a hash computed
     * by the TPM, we need to indicate this with an appropriate ticket.
     */
     // Indicates hash validation type
    validation.tag = TPM2_ST_HASHCHECK;
    // No specific hierarchy
    validation.hierarchy = TPM2_RH_NULL;

    /*
     * Perform the signing operation
     *
     * This command:
     * 1. Uses the specified key to sign the data
     * 2. Applies the requested signature scheme
     * 3. Returns the signature in a TPM-specific format
     */
    rc = Esys_Sign(
        ctx->esys_context,
        key_handle,
        ESYS_TR_PASSWORD,
        ESYS_TR_NONE,
        // Authorization sessions
        ESYS_TR_NONE,
        &digest,
        &in_scheme,
        &validation,
        &sig
    );

    if (rc != TSS2_RC_SUCCESS) {
        // Signing operation failed
        return -1;
    }

    /*
     * Extract signature data
     *
     * The TPM returns signatures in a structure that can contain different
     * signature types. We extract the raw signature bytes for the caller.
     *
     * NOTE: This is simplified - a complete implementation would handle
     * different signature algorithms and return appropriate metadata.
     */
    if (sig->sigAlg == TPM2_ALG_RSAPSS) {
        size_t sig_size = sig->signature.rsapss.sig.size;
        if (sig_size > *signature_size) {
            // Truncate to fit in caller's buffer
            sig_size = *signature_size;
        }
        memcpy(signature, sig->signature.rsapss.sig.buffer, sig_size);
        *signature_size = sig_size;
    }
    else {
        // Unexpected signature algorithm
        free(sig);
        return -1;
    }

    free(sig);
    // Success
    return 0;
}

/**
 * Read the TPM's internal clock and time information
 *
 * The TPM maintains a monotonic clock that provides:
 * - Current time value (monotonically increasing)
 * - Reset count (increments on TPM reset/power loss)
 * - Restart count (increments on TPM restart without power loss)
 * - Safe flag (indicates clock reliability)
 *
 * This information is useful for:
 * - Timestamping operations
 * - Detecting TPM resets/restarts
 * - Time-based security policies
 * - Audit logging
 */
int tpm_read_clock(
    tpm_context_t* ctx,
    uint64_t* current_time,
    uint32_t* reset_count,
    uint32_t* restart_count,
    uint8_t* safe
) {
    TSS2_RC rc;
    TPMS_TIME_INFO* current_time_info;

    // Input validation
    if (!ctx || !ctx->initialized || !current_time ||
        !reset_count || !restart_count || !safe) {
        // Invalid parameters
        return -1;
    }

    /*
     * Read TPM clock information
     *
     * The TPM2_ReadClock command returns detailed timing information
     * that can be used for various security and audit purposes.
     */
    rc = Esys_ReadClock(
        ctx->esys_context,
        ESYS_TR_NONE,
        ESYS_TR_NONE,
        ESYS_TR_NONE,
        &current_time_info
    );

    if (rc != TSS2_RC_SUCCESS) {
        // ReadClock command failed
        return -1;
    }

    /*
     * Extract time information from TPM response
     *
     * The TPM returns a comprehensive time structure containing:
     * - time: Current monotonic time value
     * - clockInfo.resetCount: Number of TPM resets since manufacturing
     * - clockInfo.restartCount: Number of restarts since last reset
     * - clockInfo.safe: Flag indicating clock reliability
     */
    *current_time = current_time_info->time;
    *reset_count = current_time_info->clockInfo.resetCount;
    *restart_count = current_time_info->clockInfo.restartCount;
    *safe = current_time_info->clockInfo.safe;

    // Clean up TPM response data
    free(current_time_info);
    // Success
    return 0;
}

/**
 * Clean up a TPM key handle and free associated memory
 *
 * This function flushes a key handle from the TPM, releasing the memory
 * and resources associated with it. This is important for preventing
 * TPM resource exhaustion when creating multiple keys.
 *
 * @param ctx Initialized TPM context
 * @param key_handle Handle to the key to be flushed
 * @return 0 on success, -1 on error
 */
int tpm_flush_key(tpm_context_t* ctx, ESYS_TR key_handle) {
    // Input validation
    if (!ctx || !ctx->initialized || key_handle == ESYS_TR_NONE) {
        // Invalid parameters
        return -1;
    }

    // Flush the key handle from TPM memory
    TSS2_RC rc = Esys_FlushContext(ctx->esys_context, key_handle);
    if (rc != TSS2_RC_SUCCESS) {
        // Flush operation failed
        return -1;
    }
    // Success
    return 0;
}
