# Attestation Device Usage Guide

## Overview

The `attestation@1.0` device provides a complete IoT device attestation system with:
- Device registration (controlled by device, not directly accessible)
- Registration status checking
- Attestation challenge generation with nonces
- Attestation response verification

## Setup

### 1. Create Registration Process

First, create a registration process that will store device registrations:

```lua
-- registration_process.lua
Registry = Registry or {}

Handlers.add("InternalRegister", function(msg)
    local publicKey = msg.PublicKey
    local wallet = msg.Wallet
    local signedHash = msg.SignedHash
    local multipleWallet = msg.MultipleWallet or "not_allowed"
    
    if Registry[publicKey] then
        if multipleWallet == "not_allowed" then
            return {error = "Multiple wallets not allowed for this public key"}
        end
        if not table.contains(Registry[publicKey].wallets, wallet) then
            table.insert(Registry[publicKey].wallets, wallet)
        end
    else
        Registry[publicKey] = {
            wallets = {wallet},
            signed_hash = signedHash,
            multiple_wallet = multipleWallet,
            registered_at = msg.Timestamp
        }
    end
    
    return {status = "registered", public_key = publicKey}
end)

Handlers.add("InternalCheck", function(msg)
    local publicKey = msg.PublicKey
    local wallet = msg.Wallet
    
    if not Registry[publicKey] then
        return {registered = false}
    end
    
    local isRegistered = false
    if wallet then
        isRegistered = table.contains(Registry[publicKey].wallets, wallet)
    else
        -- Check if public key exists with any wallet
        isRegistered = #Registry[publicKey].wallets > 0
    end
    
    return {
        registered = isRegistered,
        public_key = publicKey,
        multiple_wallet = Registry[publicKey].multiple_wallet,
        all_wallets = Registry[publicKey].wallets
    }
end)
```

Spawn this process and note its Process ID.

### 2. Configure Node

Add to your `config.flat` or node configuration:

```
registration_process_id: <YourRegistrationProcessID>
user_process_prefix: nonce-
```

## API Usage

### Register Device

```http
POST /~attestation@1.0/register
Content-Type: application/json

{
  "public-key": "<base64_public_key>",
  "wallet": "<arweave_wallet_address>",
  "signed-hash": "<hash_signed_by_secure_element>",
  "multiple-wallet": "allowed"  // or "not_allowed"
}
```

**Response:**
```json
{
  "status": "registered",
  "public_key": "<public_key>"
}
```

### Check Registration

```http
GET /~attestation@1.0/check-registration?public-key=<public_key>&wallet=<wallet>
```

**Response:**
```json
{
  "registered": true,
  "public_key": "<public_key>",
  "multiple_wallet": "allowed",
  "all_wallets": ["<wallet1>", "<wallet2>"]
}
```

### Generate Attestation Challenge

```http
POST /~attestation@1.0/attest
Content-Type: application/json

{
  "wallet": "<arweave_wallet_address>",
  "public-key": "<base64_public_key>",
  "code-hash": "<hash_of_code_to_attest>"
}
```

**Response:**
```json
{
  "nonce": "<unique_nonce>",
  "challenge": "<base64_challenge_string>",
  "registered": true
}
```

### Verify Attestation Response

```http
POST /~attestation@1.0/verify
Content-Type: application/json

{
  "wallet": "<arweave_wallet_address>",
  "public-key": "<base64_public_key>",
  "nonce": "<nonce_from_challenge>",
  "response": "<device_response_data>",
  "signature": "<signature_from_secure_element>"
}
```

**Response:**
```json
{
  "verified": true,
  "nonce": "<nonce>",
  "public-key": "<public_key>",
  "wallet": "<wallet>"
}
```

## Important Notes

1. **Signature Verification**: The `verify_signature/4` function currently has a placeholder implementation. You MUST implement actual cryptographic verification based on your secure element's signature scheme (RSA, ECDSA, etc.).

2. **Registration Process**: The registration process is NOT directly accessible. All registration operations must go through the `attestation@1.0` device.

3. **User Nonce Processes**: Each user automatically gets their own nonce process created on first attestation request. These processes are named `nonce-<wallet_address>`.

4. **Nonce Uniqueness**: Nonces are guaranteed to be unique per user process and are tracked to prevent reuse.

## Error Responses

- `{error, <<"Device not registered. Please register first.">>}` - Device must be registered before attestation
- `{error, <<"Device already registered and multiple_wallet not allowed">>}` - Attempting to register with multiple wallets when not allowed
- `{error, <<"Nonce already used">>}` - Nonce has been used before (replay attack prevention)
- `{error, <<"Invalid signature">>}` - Signature verification failed
- `{error, <<"Registration process not configured">>}` - Missing `registration_process_id` in config

