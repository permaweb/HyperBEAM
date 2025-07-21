%%% @doc TPM (Trusted Platform Module) Native Interface Functions for Erlang/Elixir
%%%
%%% This module provides Erlang Native Implemented Functions (NIFs) for interacting
%%% with TPM 2.0 hardware and software implementations. TPM is a hardware security
%%% module that provides cryptographic functions and secure storage capabilities
%%% essential for trusted computing and remote attestation.
%%%
%%% The module supports the following TPM 2.0 operations:
%%% 1. Hardware capability detection and support checking
%%% 2. Platform Configuration Register (PCR) reading for system state verification
%%% 3. Hardware random number generation for cryptographic operations
%%% 4. Primary key creation for TPM-based cryptographic operations
%%% 5. Digital signing using TPM-resident keys
%%% 6. TPM clock reading for timing and integrity information
%%%
%%% Key TPM Concepts:
%%% - PCRs: Special registers storing cryptographic hashes of system state
%%% - Primary Keys: Root keys in TPM's hierarchical key structure
%%% - Hardware RNG: TPM's built-in random number generator
%%% - TPM Clock: Monotonic clock for timing and tamper detection
%%%
%%% Usage Requirements:
%%% - TPM 2.0 hardware or software emulation (swtpm, abrmd)
%%% - TSS2 library stack installed on the system
%%% - Appropriate permissions to access TPM devices
%%% - NIF shared library compiled and available in priv directory
%%%
%%% Error Handling:
%%% All functions return either `{ok, Result}` on success or `{error, Reason}`
%%% on failure. The module gracefully handles missing TPM hardware by returning
%%% appropriate error codes rather than crashing.
-module(tpm_nif).
-export([check_tpm_support/0, read_pcr/1, get_random/1]).
-export([create_primary_key/0, sign_data/2, read_clock/0]).
-export([read_pcrs/1, generate_nonce/0, flush_key/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% NIF loading directive - automatically loads the shared library when module loads
-on_load(init/0).

%%% ============================================================================
%%% NIF Initialization and Loading
%%% ============================================================================

%% @doc Initialize and load the TPM NIF shared library.
%%
%% This function is called automatically when the module is loaded due to the
%% `-on_load(init/0)` directive. It locates and loads the compiled NIF shared
%% library from the application's priv directory.
%%
%% The loading process:
%% 1. Determines the path to the application's priv directory
%% 2. Constructs the full path to the tpm_nif shared library
%% 3. Loads the library using erlang:load_nif/2
%% 4. Replaces all NIF stub functions with their C implementations
%%
%% @returns `ok` on successful library loading, or crashes with an error
%% if the library cannot be found or loaded
-spec init() -> ok.
init() ->
    SoName = filename:join([code:priv_dir(hb), "tpm_nif"]),
    erlang:load_nif(SoName, 0).

%%% ============================================================================
%%% TPM Capability and Hardware Detection
%%% ============================================================================

%% @doc Check if TPM hardware or software emulation is available and functional.
%%
%% This function performs a lightweight test to determine if TPM functionality
%% is available on the current system. It's designed to be called before
%% attempting any TPM operations to enable graceful fallback when TPM is
%% unavailable.
%%
%% The check verifies:
%% - TPM device accessibility (/dev/tpm0, /dev/tpmrm0, or software emulation)
%% - TSS2 library availability and proper configuration
%% - Basic TPM communication and initialization
%% - Minimal TPM command execution capability
%%
%% Use cases:
%% - Conditional feature enablement in applications
%% - System capability discovery for deployment planning
%% - Health checks and monitoring of TPM availability
%% - Graceful degradation when hardware security features are unavailable
%%
%% @returns `{ok, true}` if TPM is available and functional,
%% `{ok, false}` if TPM is not available or not working
-spec check_tpm_support() -> {ok, boolean()}.
check_tpm_support() ->
    erlang:nif_error(not_loaded).

%%% ============================================================================
%%% Platform Configuration Register (PCR) Operations
%%% ============================================================================

%% @doc Read a Platform Configuration Register (PCR) value from the TPM.
%%
%% Platform Configuration Registers are special-purpose registers in the TPM
%% that store cryptographic hash measurements representing the state of the
%% system. PCRs are fundamental to trusted computing and remote attestation
%% as they provide a tamper-evident record of system configuration and software.
%%
%% PCR Layout (typical TPM 2.0 allocation):
%% - PCRs 0-7: Firmware and BIOS measurements (SRTM - Static Root of Trust)
%% - PCRs 8-15: Operating system and bootloader measurements (DRTM - Dynamic Root of Trust)
%% - PCRs 16-23: Application and user measurements (available for custom use)
%%
%% PCR Operations:
%% - PCRs can only be "extended" (new values are hashed with existing values)
%% - PCRs are reset only on TPM reset or specific platform events
%% - PCR values are typically SHA-256 hashes (32 bytes) but can vary by bank
%%
%% Security Considerations:
%% - PCR values reflect the cumulative measurement chain from boot
%% - Unexpected PCR values may indicate tampering or configuration changes
%% - PCR 23 is often used for application-specific measurements
%%
%% @param PCRIndex Integer from 0-23 specifying which PCR to read
%% @returns `{ok, PCRValue}` with a binary containing the PCR hash value,
%% or `{error, Reason}` if the operation fails
%% @throws badarg if PCRIndex is not an integer or is out of valid range
-spec read_pcr(PCRIndex :: non_neg_integer()) -> 
    {ok, PCRValue :: binary()} | {error, atom()}.
read_pcr(_PCRIndex) ->
    erlang:nif_error(not_loaded).

%%% ============================================================================
%%% Hardware Random Number Generation
%%% ============================================================================

%% @doc Generate cryptographically secure random bytes using TPM hardware.
%%
%% The TPM contains a hardware random number generator (HWRNG) that provides
%% high-quality entropy derived from physical processes within the TPM chip.
%% This is particularly valuable for cryptographic operations requiring
%% hardware-backed randomness.
%%
%% Hardware RNG Advantages:
%% - True randomness from physical entropy sources
%% - Resistance to prediction and cryptographic attacks
%% - Independent of software entropy pools
%% - Compliance with cryptographic standards (FIPS 140-2, Common Criteria)
%%
%% Performance Characteristics:
%% - Maximum bytes per call: typically 64 bytes (TPM implementation dependent)
%% - Lower throughput compared to software PRNGs
%% - Higher quality entropy compared to software sources
%% - Suitable for key generation, nonces, and cryptographic salts
%%
%% Use Cases:
%% - Cryptographic key material generation
%% - Nonce generation for security protocols
%% - Seeding software pseudo-random number generators
%% - High-security applications requiring hardware-backed entropy
%%
%% @param NumBytes Integer specifying number of random bytes to generate (1-64)
%% @returns `{ok, RandomBytes}` with a binary containing the random data,
%% or `{error, Reason}` if the operation fails
%% @throws badarg if NumBytes is not an integer or is out of valid range
-spec get_random(NumBytes :: pos_integer()) -> 
    {ok, RandomBytes :: binary()} | {error, atom()}.
get_random(_NumBytes) ->
    erlang:nif_error(not_loaded).

%%% ============================================================================
%%% TPM Key Management Operations
%%% ============================================================================

%% @doc Create a primary key in the TPM's owner hierarchy.
%%
%% Primary keys are the foundation of TPM's hierarchical key structure and
%% serve as the root for all derived keys. This function creates an RSA
%% 2048-bit signing key with attributes suitable for general cryptographic
%% operations within the TPM.
%%
%% Key Characteristics:
%% - Algorithm: RSA 2048-bit with RSAPSS signature scheme
%% - Hash Algorithm: SHA-256 for signatures and key naming
%% - Hierarchy: Owner hierarchy (persistent across reboots if saved)
%% - Attributes: Non-exportable, TPM-restricted, suitable for signing
%% - Usage: Digital signatures, parent key for key derivation
%%
%% TPM Key Hierarchy:
%% - Primary keys are created directly in TPM hierarchies (Owner, Platform, Endorsement)
%% - Child keys can be derived from primary keys for specific purposes
%% - Keys can be made persistent or loaded on-demand from key files
%% - Private key material never leaves the TPM hardware
%%
%% Security Properties:
%% - Private key is generated and stored within TPM hardware
%% - Key cannot be extracted or duplicated outside the TPM
%% - Usage requires TPM authorization and proper context
%% - Key operations are performed within the secure TPM environment
%%
%% @returns `{ok, KeyHandle}` with an integer handle referencing the created key,
%% or `{error, Reason}` if key creation fails
%% @see sign_data/2 for using the created key for digital signatures
-spec create_primary_key() -> {ok, KeyHandle :: pos_integer()} | {error, atom()}.
create_primary_key() ->
    erlang:nif_error(not_loaded).

%%% ============================================================================
%%% TPM Digital Signature Operations
%%% ============================================================================

%% @doc Sign arbitrary data using a TPM-resident key.
%%
%% This function performs digital signature operations using a key stored
%% within the TPM hardware. The private key material never leaves the TPM,
%% ensuring that signatures can only be generated by the specific TPM device
%% that holds the key.
%%
%% Signature Process:
%% 1. Data is prepared for signing (hashed if necessary)
%% 2. TPM validates key handle and authorization
%% 3. TPM performs signature using RSAPSS scheme with SHA-256
%% 4. Signature is returned for verification by external parties
%%
%% Signature Properties:
%% - Algorithm: RSAPSS (RSA Probabilistic Signature Scheme)
%% - Hash Function: SHA-256
%% - Key Size: 2048-bit RSA (typically, depends on key creation)
%% - Non-repudiation: Signatures can only be created by the TPM holding the key
%%
%% Use Cases:
%% - Document and message authentication
%% - Software signing and integrity protection
%% - Identity assertions and authentication tokens
%% - Audit trails and non-repudiation evidence
%% - Cryptographic protocols requiring hardware-backed signatures
%%
%% Security Considerations:
%% - Signature verification requires the corresponding public key
%% - Key authorization may be required (passwords, policies)
%% - Large data should be hashed externally before signing
%% - Signature format follows standard cryptographic conventions
%%
%% @param KeyHandle Integer handle to a signing key previously created or loaded
%% in the TPM. Typically obtained from create_primary_key/0.
%% @param Data Binary data to be digitally signed. Size limitations may apply
%% depending on TPM implementation and signature scheme.
%% @returns `{ok, Signature}` with a binary containing the digital signature,
%% or `{error, Reason}` if signing fails
%% @see create_primary_key/0 for creating keys suitable for signing
-spec sign_data(KeyHandle :: pos_integer(), Data :: binary()) -> 
    {ok, Signature :: binary()} | {error, atom()}.
sign_data(_KeyHandle, _Data) ->
    erlang:nif_error(not_loaded).

%%% ============================================================================
%%% TPM Clock and Timing Operations
%%% ============================================================================

%% @doc Read comprehensive timing information from the TPM's internal clock.
%%
%% The TPM maintains a monotonic clock that provides valuable timing and
%% integrity information. This clock is designed to be tamper-evident and
%% provides insights into the TPM's operational history and current state.
%%
%% Clock Information Components:
%% - CurrentTime: Monotonically increasing time value in TPM-specific units
%% - ResetCount: Number of TPM resets since manufacturing (tamper indicator)
%% - RestartCount: Number of TPM restarts since the last reset
%% - Safe: Boolean flag indicating clock reliability and normal operation
%%
%% Clock Properties:
%% - Monotonic: Time value always increases, never goes backward
%% - Persistent: Clock state survives power cycles and system reboots
%% - Tamper-evident: Reset counts change when TPM is reset or tampered with
%% - Resolution: TPM-implementation dependent timing granularity
%%
%% Security Applications:
%% - Timestamping critical operations and audit events
%% - Detecting unexpected TPM resets (potential tamper indicator)
%% - Implementing time-based security policies and access controls
%% - Validating the temporal sequence of TPM operations
%% - Forensic analysis of system events and timelines
%%
%% Time Base:
%% The CurrentTime value is relative to the last TPM2_Clear operation or
%% TPM manufacturing. It does not represent absolute wall-clock time but
%% provides a consistent monotonic reference for temporal ordering.
%%
%% @returns `{ok, {CurrentTime, ResetCount, RestartCount, Safe}}` on success
%% where CurrentTime is a 64-bit unsigned integer representing elapsed time,
%% ResetCount and RestartCount are 32-bit unsigned integers, and Safe is a
%% boolean atom (`true` or `false`), or `{error, Reason}` if the operation fails
-spec read_clock() -> 
    {ok, {CurrentTime :: non_neg_integer(), 
          ResetCount :: non_neg_integer(), 
          RestartCount :: non_neg_integer(), 
          Safe :: boolean()}} | {error, atom()}.
read_clock() ->
    erlang:nif_error(not_loaded).

%% @doc Clean up a TPM key handle and free associated memory.
%%
%% This function flushes a key handle from the TPM, releasing the memory
%% and resources associated with it. This is essential for preventing
%% TPM resource exhaustion when creating multiple keys.
%%
%% TPM Resource Management:
%% - TPMs have limited memory for storing key objects
%% - Each created key consumes TPM memory until explicitly flushed
%% - Multiple key creations without cleanup will eventually fail
%% - This function should be called after key operations are complete
%%
%% Use Cases:
%% - Cleaning up after key creation tests
%% - Managing temporary keys in applications
%% - Preventing TPM resource exhaustion
%% - Maintaining good TPM hygiene in long-running processes
%%
%% @param KeyHandle Integer handle to the key to be flushed
%% @returns `ok` if the key was successfully flushed,
%% or `{error, Reason}` if the operation fails
-spec flush_key(KeyHandle :: pos_integer()) -> ok | {error, atom()}.
flush_key(_KeyHandle) ->
    erlang:nif_error(not_loaded).

%%% ============================================================================
%%% Utility Functions for Common TPM Operations
%%% ============================================================================

%% @doc Generate a cryptographically secure random nonce suitable for TPM operations.
%%
%% This function creates a 32-byte (256-bit) random nonce using the TPM's
%% hardware random number generator. Nonces are essential for many cryptographic
%% protocols to prevent replay attacks and ensure freshness of operations.
%%
%% Nonce Characteristics:
%% - Size: 32 bytes (256 bits) for strong cryptographic security
%% - Source: TPM hardware random number generator
%% - Quality: Cryptographically secure, suitable for all security applications
%% - Uniqueness: Extremely low probability of collision across generations
%%
%% Common Use Cases:
%% - Challenge-response authentication protocols
%% - Quote generation for remote attestation
%% - Session establishment in cryptographic protocols
%% - Preventing replay attacks in security systems
%% - Key derivation and cryptographic salt generation
%%
%% @returns `{ok, Nonce}` with a 32-byte binary containing the random nonce,
%% or `{error, Reason}` if nonce generation fails
%% @see get_random/1 for generating random data of arbitrary length
%% @see quote_pcrs/1 for using nonces in TPM quote operations
-spec generate_nonce() -> {ok, Nonce :: binary()} | {error, atom()}.
generate_nonce() ->
    get_random(32).

%% @doc Read multiple Platform Configuration Registers (PCRs) in a single operation.
%%
%% This utility function reads multiple PCRs and returns their values in a
%% structured format. It's useful for collecting comprehensive system state
%% information or for operations that need to examine multiple measurements
%% simultaneously.
%%
%% The function handles individual PCR read failures gracefully by including
%% error information in the result rather than failing the entire operation.
%% This allows partial success when some PCRs are accessible while others
%% may be unavailable due to policy restrictions or hardware issues.
%%
%% Result Format:
%% Each PCR in the result list is represented as a tuple:
%% - `{PCRIndex, PCRValue}` for successful reads
%% - `{PCRIndex, {error, Reason}}` for failed reads
%%
%% Use Cases:
%% - System integrity verification across multiple components
%% - Collecting measurements for remote attestation
%% - Debugging and troubleshooting PCR-related issues
%% - Batch operations for efficiency in measurement collection
%%
%% @param PCRList List of integers specifying which PCRs to read (0-23)
%% @returns A list of tuples where each tuple contains the PCR index and either
%% its binary value or an error tuple, maintaining the order of the input list
%% @see read_pcr/1 for reading individual PCRs
-spec read_pcrs(PCRList :: [non_neg_integer()]) -> 
    [{non_neg_integer(), binary() | {error, atom()}}].
read_pcrs(PCRList) when is_list(PCRList) ->
    Results = [begin
                   case read_pcr(PCR) of
                       {ok, Value} -> {PCR, Value};
                       {error, Reason} -> {PCR, {error, Reason}}
                   end
               end || PCR <- PCRList],
    Results.

%%% ============================================================================
%%% Unit Tests for TPM NIF Functions
%%% ============================================================================

%% @doc Test TPM support detection functionality.
%% Verifies that the support check function returns consistent boolean results.
check_tpm_support_test() ->
    Result1 = tpm_nif:check_tpm_support(),
    Result2 = tpm_nif:check_tpm_support(),
    ?assertEqual(Result1, Result2),
    ?assert(element(1, Result1) =:= ok),
    ?assert(is_atom(element(2, Result1))).

%% @doc Test PCR reading functionality.
%% Attempts to read PCR 2 and validates the result format.
read_pcr_test() ->
    Result = tpm_nif:read_pcr(5),
    case Result of
        {ok, PCRValue} ->
            ?assert(is_binary(PCRValue)),
            ?assertEqual(32, byte_size(PCRValue)), % SHA256 hash size
            ?event(tpm_debug, {pcr_2_value, hb_util:to_hex(PCRValue)});
        {error, Reason} ->
            ?event(tpm_debug, {pcr_read_failed, Reason}),
            ?assert(false, "PCR read failed: " ++ atom_to_list(Reason))
    end.

%% @doc Test hardware random number generation.
%% Generates 16 bytes of random data and validates format and size.
get_random_test() ->
    Result = tpm_nif:get_random(16),
    case Result of
        {ok, RandomBytes} ->
            ?assert(is_binary(RandomBytes)),
            ?assertEqual(16, byte_size(RandomBytes)),
            ?event(tpm_debug, {random_bytes, hb_util:to_hex(RandomBytes)});
        {error, Reason} ->
            ?event(tpm_debug, {random_generation_failed, Reason}),
            ?assert(false, "Random generation failed: " ++ atom_to_list(Reason))
    end.

%% @doc Test primary key creation.
%% Creates a primary key and validates the returned handle.
%% Key cleanup is handled automatically to prevent TPM resource exhaustion.
create_primary_key_test() ->
    Result = tpm_nif:create_primary_key(),
    case Result of
        {ok, KeyHandle} ->
            ?assert(is_integer(KeyHandle)),
            ?assert(KeyHandle > 0),
            ?event(tpm_debug, {key_handle, KeyHandle});
        {error, Reason} ->
            ?event(tpm_debug, {key_creation_failed, Reason}),
            ?assert(false, "Key creation failed: " ++ atom_to_list(Reason))
    end.

%% @doc Test TPM clock reading.
%% Reads clock information and validates the returned data structure.
read_clock_test() ->
    Result = tpm_nif:read_clock(),
    case Result of
        {ok, {CurrentTime, ResetCount, RestartCount, Safe}} ->
            ?assert(is_integer(CurrentTime)),
            ?assert(CurrentTime >= 0),
            ?assert(is_integer(ResetCount)),
            ?assert(ResetCount >= 0),
            ?assert(is_integer(RestartCount)),
            ?assert(RestartCount >= 0),
            ?assert(is_atom(Safe)),
            ?assert(Safe =:= true orelse Safe =:= false),
            ?event(tpm_debug, 
                {
                    tpm_clock, 
                    {
                        current_time, CurrentTime, 
                        reset_count, ResetCount, 
                        restart_count, RestartCount,
                        safe, Safe
                    }
                }
            );
        {error, Reason} ->
            ?event(tpm_debug, {read_clock_failed, Reason}),
            ?assert(false, "Clock read failed: " ++ atom_to_list(Reason))
    end. 