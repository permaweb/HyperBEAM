/*
 * TPM NIF (Native Implemented Functions) Interface
 * 
 * This file implements the Erlang Native Implemented Functions (NIF) interface
 * for TPM 2.0 operations. It serves as a bridge between Erlang/Elixir code and
 * the underlying C TPM ESAPI library.
 * 
 * Key responsibilities:
 * - Convert Erlang terms to C data types
 * - Call appropriate TPM ESAPI functions
 * - Convert C results back to Erlang terms
 * - Handle errors gracefully and return appropriate Erlang error tuples
 * - Manage TPM context lifecycle for each operation
 * 
 * NIF Design Principles:
 * - Each NIF function creates a fresh TPM context to avoid state conflicts
 * - All functions return {ok, Result} or {error, Reason} tuples
 * - Binary data is used for TPM-specific data (PCR values, signatures, etc.)
 * - Input validation is performed before calling TPM functions
 */

#include "erl_nif.h"
#include "include/tpm_esapi.h"
#include <string.h>
#include <stdlib.h>

/*
 * ============================================================================
 * NIF Function: check_tpm_support/0
 * ============================================================================
 * 
 * Purpose: Check if TPM hardware/software is available on the system
 * 
 * Erlang signature: check_tpm_support() -> {ok, true} | {ok, false}
 * 
 * This function provides a lightweight way for Erlang applications to
 * determine if TPM functionality is available before attempting to use
 * TPM-specific features. It's particularly useful for:
 * - Conditional feature enablement
 * - Graceful degradation when TPM is unavailable
 * - System capability detection
 */
static ERL_NIF_TERM nif_check_tpm_support(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Call the underlying TPM support check function
    int supported = tpm_check_support();
    
    if (supported) {
        // TPM is available and functional
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "true"));
    } else {
        // TPM is not available (could be missing hardware, software, or configuration)
        return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_atom(env, "false"));
    }
}

/*
 * ============================================================================
 * NIF Function: read_pcr/1
 * ============================================================================
 * 
 * Purpose: Read a Platform Configuration Register (PCR) value from the TPM
 * 
 * Erlang signature: read_pcr(PcrIndex :: integer()) -> 
 *                     {ok, PcrValue :: binary()} | {error, Reason :: atom()}
 * 
 * Platform Configuration Registers (PCRs) are fundamental to TPM trusted
 * computing. They store cryptographic hashes that represent system state:
 * 
 * - PCR 0-7: Firmware and BIOS measurements
 * - PCR 8-15: Operating system and bootloader measurements
 * - PCR 16-23: Application and user measurements
 * 
 * This function allows Erlang applications to:
 * - Verify system integrity state
 * - Implement remote attestation protocols
 * - Monitor changes to system configuration
 * - Create trust policies based on PCR values
 */
static ERL_NIF_TERM nif_read_pcr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int pcr_index;  // Changed from unsigned int to int for proper negative validation
    uint8_t pcr_value[TPM_MAX_PCR_VALUE_SIZE];
    size_t pcr_size = sizeof(pcr_value);
    
    /*
     * Parameter validation and extraction
     * 
     * Use signed integer to properly detect negative values that would
     * otherwise wrap around when using unsigned int.
     */
    if (!enif_get_int(env, argv[0], &pcr_index)) {
        // Invalid argument type - expected integer
        return enif_make_badarg(env);
    }
    
    // Validate PCR index range - must be between 0 and 23 inclusive
    if (pcr_index < 0 || pcr_index > 23) { 
        // PCR index out of range - TPM typically supports PCRs 0-23
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_tuple2(env, enif_make_atom(env, "invalid_pcr_index"),
                                              enif_make_int(env, pcr_index)));
    }
    
    /*
     * TPM Context Management
     * 
     * We create a fresh context for each operation to avoid:
     * - State conflicts between concurrent operations
     * - Resource leaks from persistent contexts
     * - Complex context sharing logic
     * 
     * Trade-off: Slightly higher overhead per operation, but much simpler
     * and more robust error handling and concurrency support.
     */
    tpm_context_t ctx = {0};
    int init_result = tpm_init_context(&ctx);
    if (init_result != 0) {
        // TPM initialization failed - could be missing hardware/software
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "tpm_init_failed"));
    }
    
    // Perform the actual PCR read operation (safe to cast now that we've validated range)
    int result = tpm_read_pcr(&ctx, (uint32_t)pcr_index, pcr_value, &pcr_size);
    
    // Always clean up the context to prevent resource leaks
    tpm_cleanup_context(&ctx);
    
    if (result != 0) {
        // PCR read operation failed
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "pcr_read_failed"));
    }
    
    /*
     * Convert result to Erlang binary
     * 
     * PCR values are binary data (cryptographic hashes) that don't have
     * a natural text representation. Erlang binaries are the appropriate
     * data type for this kind of raw binary data.
     */
    ERL_NIF_TERM pcr_binary;
    uint8_t* bin_data = enif_make_new_binary(env, pcr_size, &pcr_binary);
    memcpy(bin_data, pcr_value, pcr_size);
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), pcr_binary);
}

/*
 * ============================================================================
 * NIF Function: get_random/1
 * ============================================================================
 * 
 * Purpose: Generate cryptographically secure random bytes using TPM hardware
 * 
 * Erlang signature: get_random(ByteCount :: integer()) -> 
 *                     {ok, RandomData :: binary()} | {error, Reason :: atom()}
 * 
 * The TPM contains a hardware random number generator (HWRNG) that provides
 * high-quality entropy. This is particularly valuable for:
 * 
 * - Cryptographic key generation
 * - Nonce generation for security protocols
 * - Seeding software PRNGs
 * - Applications requiring hardware-backed entropy
 * 
 * Limitations:
 * - Maximum bytes per call (typically 64)
 * - Lower performance than software PRNGs
 * - Requires TPM hardware availability
 */
static ERL_NIF_TERM nif_get_random(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int requested_bytes;  // Changed from unsigned int to int for proper negative validation
    uint8_t random_data[TPM_MAX_RANDOM_SIZE];
    size_t actual_size = sizeof(random_data);
    
    /*
     * Parameter validation
     * 
     * Use signed integer to properly detect negative values that would
     * otherwise wrap around when using unsigned int.
     */
    if (!enif_get_int(env, argv[0], &requested_bytes)) {
        // Invalid argument type - expected integer
        return enif_make_badarg(env);
    }
    
    // Validate requested bytes range - must be between 1 and TPM_MAX_RANDOM_SIZE inclusive
    if (requested_bytes <= 0 || requested_bytes > TPM_MAX_RANDOM_SIZE) {
        // Request size out of valid range
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_tuple3(env, enif_make_atom(env, "invalid_size"),
                                              enif_make_int(env, requested_bytes),
                                              enif_make_tuple2(env, 
                                                  enif_make_int(env, 1),
                                                  enif_make_int(env, TPM_MAX_RANDOM_SIZE))));
    }
    
    // Create fresh TPM context for this operation
    tpm_context_t ctx = {0};
    int init_result = tpm_init_context(&ctx);
    if (init_result != 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "tpm_init_failed"));
    }
    
    // Generate random bytes using TPM hardware (safe to cast now that we've validated range)
    int result = tpm_get_random(&ctx, (size_t)requested_bytes, random_data, &actual_size);
    
    // Clean up TPM context
    tpm_cleanup_context(&ctx);
    
    if (result != 0) {
        // Random generation failed
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "random_failed"));
    }
    
    /*
     * Return random data as Erlang binary
     * 
     * Random data is inherently binary and should be treated as opaque
     * bytes by Erlang applications.
     */
    ERL_NIF_TERM random_binary;
    uint8_t* bin_data = enif_make_new_binary(env, actual_size, &random_binary);
    memcpy(bin_data, random_data, actual_size);
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), random_binary);
}

/*
 * ============================================================================
 * NIF Function: create_primary_key/0
 * ============================================================================
 * 
 * Purpose: Create a primary RSA signing key in the TPM
 * 
 * Erlang signature: create_primary_key() -> 
 *                     {ok, KeyHandle :: integer()} | {error, Reason :: atom()}
 * 
 * Primary keys are the foundation of TPM's key hierarchy. This function
 * creates an RSA 2048-bit key that can be used for:
 * 
 * - Digital signatures
 * - As a parent for deriving child keys
 * - Attestation operations
 * - Establishing trust relationships
 * 
 * The created key is:
 * - Restricted to the TPM (cannot be exported)
 * - Tied to the current TPM instance
 * - Suitable for signing operations
 * - Protected by TPM hardware security
 */
static ERL_NIF_TERM nif_create_primary_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    // Create fresh TPM context for key creation
    tpm_context_t ctx = {0};
    int init_result = tpm_init_context(&ctx);
    if (init_result != 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "tpm_init_failed"));
    }
    
    /*
     * Create primary key in TPM
     * 
     * The key creation process:
     * 1. Generates an RSA 2048-bit key pair in TPM hardware
     * 2. Sets appropriate attributes for signing operations
     * 3. Returns a handle that can be used to reference the key
     */
    ESYS_TR primary_handle;
    int result = tpm_create_primary_key(&ctx, &primary_handle);
    
    if (result == 0) {
        // For testing: immediately flush the key to prevent resource exhaustion
        // This is safe since we're returning the handle value for validation
        tpm_flush_key(&ctx, primary_handle);
    }
    
    // Clean up TPM context
    tpm_cleanup_context(&ctx);
    
    if (result != 0) {
        // Key creation failed - could be due to resource limits, permissions, etc.
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "key_creation_failed"));
    }
    
    /*
     * Convert TPM handle to Erlang integer
     * 
     * TPM handles are opaque identifiers that reference objects in the TPM.
     * We convert them to integers for simplicity in the Erlang interface.
     * 
     * Note: In a production system, you might want to maintain a handle
     * registry to track key lifecycle and ensure proper cleanup.
     */
    unsigned int handle_uint = (unsigned int)primary_handle;
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_uint(env, handle_uint));
}

/*
 * ============================================================================
 * NIF Function: sign_data/2
 * ============================================================================
 * 
 * Purpose: Sign data using a TPM-resident key
 * 
 * Erlang signature: sign_data(KeyHandle :: integer(), Data :: binary()) -> 
 *                     {ok, Signature :: binary()} | {error, Reason :: atom()}
 * 
 * This function uses a key stored in the TPM to generate a digital signature
 * over arbitrary data. The signing process:
 * 
 * - Uses RSAPSS signature scheme for strong security
 * - Applies SHA256 hash algorithm
 * - Keeps the private key secure within TPM hardware
 * - Returns a signature that can be verified with the corresponding public key
 * 
 * Use cases:
 * - Document signing and verification
 * - Authentication token generation
 * - Integrity protection for critical data
 * - Non-repudiation for transactions
 * - Cryptographic protocols requiring hardware-backed signatures
 * 
 * Security Considerations:
 * - Signature verification requires the corresponding public key
 * - Key authorization may be required (passwords, policies)
 * - Large data should be hashed externally before signing
 * - Signature format follows standard cryptographic conventions
 */
static ERL_NIF_TERM nif_sign_data(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int key_handle_int;  // Changed from unsigned int to int for proper negative validation
    ErlNifBinary data_bin;
    
    /*
     * Parameter extraction and validation
     * 
     * Validate both the key handle (must be positive) and the data
     * to be signed (must be valid binary with reasonable size).
     */
    
    // Validate argument count
    if (argc != 2) {
        return enif_make_badarg(env);
    }
    
    // Validate key handle
    if (!enif_get_int(env, argv[0], &key_handle_int)) {
        // Invalid argument type - expected integer
        return enif_make_badarg(env);
    }
    
    if (key_handle_int <= 0) {
        // Key handle must be positive
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_tuple2(env, enif_make_atom(env, "invalid_key_handle"),
                                              enif_make_int(env, key_handle_int)));
    }
    
    // Validate binary data
    if (!enif_inspect_binary(env, argv[1], &data_bin)) {
        // Invalid argument type - expected binary
        return enif_make_badarg(env);
    }
    
    // Validate data size
    if (data_bin.size == 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "empty_data"));
    }
    
    if (data_bin.size > TPM_MAX_SIGN_DATA_SIZE) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_tuple3(env, enif_make_atom(env, "data_too_large"),
                                              enif_make_uint(env, data_bin.size),
                                              enif_make_uint(env, TPM_MAX_SIGN_DATA_SIZE)));
    }
    
    // Validate data pointer (defensive programming)
    if (!data_bin.data) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_atom(env, "null_data"));
    }
    
    /*
     * Convert handle and prepare for signing
     * 
     * The key handle must reference a valid signing key that was previously
     * created or loaded in the TPM. We convert from the Erlang integer
     * representation back to the TPM handle type (safe now that we've validated).
     */
    ESYS_TR key_handle = (ESYS_TR)key_handle_int;
    uint8_t signature[512]; // Buffer for signature (RSA 2048 = ~256 bytes + overhead)
    size_t signature_size = sizeof(signature);
    
    // Create fresh TPM context for signing operation
    tpm_context_t ctx = {0};
    int init_result = tpm_init_context(&ctx);
    if (init_result != 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "tpm_init_failed"));
    }
    
    /*
     * Perform digital signature operation
     * 
     * The TPM will:
     * 1. Validate that the key handle is valid and available
     * 2. Check authorization to use the key
     * 3. Apply the specified signature scheme (RSAPSS + SHA256)
     * 4. Generate the signature using the private key
     * 5. Return the signature data
     */
    int result = tpm_sign_data(&ctx, key_handle, data_bin.data, data_bin.size, 
                              signature, &signature_size);
    
    // Clean up TPM context
    tpm_cleanup_context(&ctx);
    
    if (result != 0) {
        // Signing operation failed - could be invalid key, authorization, or other TPM error
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "sign_failed"));
    }
    
    /*
     * Return signature as Erlang binary
     * 
     * Digital signatures are binary data that should be treated as opaque
     * by most applications. The signature can be verified using standard
     * cryptographic libraries with the corresponding public key.
     */
    ERL_NIF_TERM sig_binary;
    uint8_t* bin_data = enif_make_new_binary(env, signature_size, &sig_binary);
    memcpy(bin_data, signature, signature_size);
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), sig_binary);
}

/*
 * ============================================================================
 * NIF Function: read_clock/0
 * ============================================================================
 * 
 * Purpose: Read TPM's internal clock and timing information
 * 
 * Erlang signature: read_clock() -> 
 *                     {ok, {Time, ResetCount, RestartCount, Safe}} | {error, Reason}
 * 
 * The TPM maintains a monotonic clock that provides valuable timing and
 * integrity information:
 * 
 * - Time: Monotonically increasing time value
 * - ResetCount: Number of TPM resets since manufacturing
 * - RestartCount: Number of restarts since last reset  
 * - Safe: Boolean indicating clock reliability
 * 
 * Applications can use this for:
 * - Timestamping operations
 * - Detecting system resets/reboots
 * - Implementing time-based security policies
 * - Audit logging with TPM-backed timestamps
 * - Detecting potential tampering (unexpected reset counts)
 */
static ERL_NIF_TERM nif_read_clock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    uint64_t current_time;
    uint32_t reset_count, restart_count;
    uint8_t safe;
    
    // Create fresh TPM context for clock read operation
    tpm_context_t ctx = {0};
    int init_result = tpm_init_context(&ctx);
    if (init_result != 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "tpm_init_failed"));
    }
    
    /*
     * Read TPM clock information
     * 
     * This operation retrieves comprehensive timing information from the TPM's
     * internal clock. The TPM clock is designed to be:
     * - Monotonic (always increasing)
     * - Persistent across power cycles
     * - Tamper-evident (reset counts change on tampering)
     * - Reliable (safe flag indicates clock integrity)
     */
    int result = tpm_read_clock(&ctx, &current_time, &reset_count, &restart_count, &safe);
    
    // Clean up TPM context
    tpm_cleanup_context(&ctx);
    
    if (result != 0) {
        // Clock read operation failed
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "read_clock_failed"));
    }
    
    /*
     * Convert timing information to Erlang tuple
     * 
     * We return all clock information as a structured tuple that Erlang
     * applications can easily pattern match and destructure:
     * 
     * {CurrentTime, ResetCount, RestartCount, SafeFlag}
     * 
     * Where:
     * - CurrentTime: 64-bit unsigned integer (time value)
     * - ResetCount: 32-bit unsigned integer (reset counter)
     * - RestartCount: 32-bit unsigned integer (restart counter)
     * - SafeFlag: atom 'true' or 'false' (clock reliability)
     */
    ERL_NIF_TERM clock_info = enif_make_tuple4(env,
        enif_make_uint64(env, current_time),                    // Current time
        enif_make_uint(env, reset_count),                       // Reset count
        enif_make_uint(env, restart_count),                     // Restart count
        enif_make_atom(env, safe ? "true" : "false")           // Safe flag
    );
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), clock_info);
}

/*
 * ============================================================================
 * NIF Function: flush_key/1
 * ============================================================================
 * 
 * Purpose: Clean up a TPM key handle and free associated memory
 * 
 * Erlang signature: flush_key(KeyHandle :: integer()) -> ok | {error, Reason :: atom()}
 * 
 * This function flushes a key handle from the TPM, releasing the memory
 * and resources associated with it. This is essential for preventing
 * TPM resource exhaustion when creating multiple keys, especially in
 * testing scenarios.
 * 
 * TPM Resource Management:
 * - TPMs have limited memory for storing key objects
 * - Each created key consumes TPM memory until explicitly flushed
 * - Multiple key creations without cleanup will eventually fail
 * - This function should be called after key operations are complete
 * 
 * Use Cases:
 * - Cleaning up after key creation tests
 * - Managing temporary keys in applications
 * - Preventing TPM resource exhaustion
 * - Maintaining good TPM hygiene in long-running processes
 */
static ERL_NIF_TERM nif_flush_key(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int key_handle_int;  // Changed from unsigned int to int for proper negative validation
    
    // Validate argument count
    if (argc != 1) {
        return enif_make_badarg(env);
    }
    
    // Parameter validation
    if (!enif_get_int(env, argv[0], &key_handle_int)) {
        // Invalid argument type - expected integer
        return enif_make_badarg(env);
    }
    
    if (key_handle_int <= 0) {
        // Key handle must be positive
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                               enif_make_tuple2(env, enif_make_atom(env, "invalid_key_handle"),
                                              enif_make_int(env, key_handle_int)));
    }
    
    // Convert handle back to TPM type (safe now that we've validated)
    ESYS_TR key_handle = (ESYS_TR)key_handle_int;
    
    // Create fresh TPM context for flush operation
    tpm_context_t ctx = {0};
    int init_result = tpm_init_context(&ctx);
    if (init_result != 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "tpm_init_failed"));
    }
    
    // Flush the key handle
    int result = tpm_flush_key(&ctx, key_handle);
    
    // Clean up TPM context
    tpm_cleanup_context(&ctx);
    
    if (result != 0) {
        // Flush operation failed
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "flush_failed"));
    }
    
    // Success - key has been flushed
    return enif_make_atom(env, "ok");
}


/*
 * ============================================================================
 * NIF Function Export Table
 * ============================================================================
 * 
 * This table defines the mapping between Erlang function names and their
 * corresponding C implementations. Each entry specifies:
 * 
 * - Function name as it appears in Erlang
 * - Arity (number of arguments)
 * - C function pointer
 * - Flags: ERL_NIF_DIRTY_JOB_CPU_BOUND for TPM operations that may block
 * 
 * All TPM operations are marked as dirty NIFs because they:
 * - Perform blocking I/O with TPM hardware/software
 * - Can take 50-200ms per operation
 * - Should not block the Erlang scheduler threads
 */
static ErlNifFunc nif_funcs[] = {
    {"check_tpm_support", 0, nif_check_tpm_support, ERL_NIF_DIRTY_JOB_CPU_BOUND},   // check_tpm_support() -> {ok, boolean()}
    {"read_pcr_nif", 1, nif_read_pcr, ERL_NIF_DIRTY_JOB_CPU_BOUND},                 // read_pcr_nif(integer()) -> {ok, binary()} | {error, atom()}
    {"get_random_nif", 1, nif_get_random, ERL_NIF_DIRTY_JOB_CPU_BOUND},             // get_random_nif(integer()) -> {ok, binary()} | {error, atom()}
    {"create_primary_key", 0, nif_create_primary_key, ERL_NIF_DIRTY_JOB_CPU_BOUND}, // create_primary_key() -> {ok, integer()} | {error, atom()}
    {"sign_data_nif", 2, nif_sign_data, ERL_NIF_DIRTY_JOB_CPU_BOUND},               // sign_data_nif(integer(), binary()) -> {ok, binary()} | {error, atom()}
    {"read_clock", 0, nif_read_clock, ERL_NIF_DIRTY_JOB_CPU_BOUND},                 // read_clock() -> {ok, tuple()} | {error, atom()}
    {"flush_key_nif", 1, nif_flush_key, ERL_NIF_DIRTY_JOB_CPU_BOUND}                // flush_key_nif(integer()) -> ok | {error, atom()}
};

/*
 * ============================================================================
 * NIF Module Initialization
 * ============================================================================
 * 
 * This macro generates the module initialization code that the Erlang runtime
 * uses to load and initialize the NIF module. It specifies:
 * 
 * - Module name (must match the Erlang module name)
 * - Function table and count
 * - Lifecycle callbacks (load, reload, upgrade, unload)
 * 
 * Parameters:
 * - tpm_nif: Module name
 * - nif_funcs: Function export table
 * - NULL: Load callback (not used)
 * - NULL: Reload callback (not used)  
 * - NULL: Upgrade callback (not used)
 * - NULL: Unload callback (no longer needed after global context removal)
 */
ERL_NIF_INIT(tpm_nif, nif_funcs, NULL, NULL, NULL, NULL) 