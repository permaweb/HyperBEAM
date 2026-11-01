# Device: ~rng@1.0

## Overview

The `~rng@1.0` device implements the HyperBEAM RNG verification and derivation primitives. It checks randomness-beacon signatures, verifies Any-Honest time-lock reveals and cracks, and derives bytes from a seed. It is stateless and every key is a pure function of its inputs, so a process can call it from inside its computation (for example with `ao.resolve` from Lua) and every node that replays the process gets the same answer.

RNG Labs, a CipherPlay Inc. product, maintains the canonical protocol, versioning, reference beacon network, and provider implementation. CipherPlay Inc. owns the originating IP. This HyperBEAM device is the interoperable consensus verifier distributed under HyperBEAM's repository license.

Signatures are BLS12-381 in the IETF "minimal-pubkey-size" variant: public keys are compressed G1 points (48 bytes), signatures are compressed G2 points (96 bytes), and messages hash to G2 with the ciphersuite `BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_`. This is what noble-curves calls `bls12_381.longSignatures`. A t-of-n threshold group signature verifies as an ordinary signature under the group public key. Verification uses the audited [`blst`](https://github.com/supranational/blst) library (v0.3.16, `native/lib/blst`) through `native/blst_nif`, on a dirty CPU scheduler. Both points are checked to be on the curve and in their prime-order subgroup, and the identity is refused as a public key.

Inputs are read from the request, then from the base message. Bad or missing inputs never raise an error: the result is `valid: false` with a `reason`.

## Core Functions (Keys)

### `generator`

Returns the fixed RSA-2048 unknown-order group as lowercase hexadecimal `g`
and `n`. The factors of `n` are not known.

### `verify-reveal`

Checks `g^alpha = c mod n` and returns the canonical time-lock output
`y = min(h^alpha mod n, n - h^alpha mod n)`.

### `verify-crack`

Checks the Wesolowski proof that `y` is the result of `t` sequential squarings
from `x`, returning the same canonical `y` used by `verify-reveal`.

### `verify-bls`

*   **Inputs:** `public-key` (hex, 48 bytes), `message` (binary), `signature` (hex, 96 bytes).
*   **Returns:** `valid` (boolean). When valid, `value` is the hex of `SHA-256("hyperbeam/rng/v1/round|" ++ message ++ "|" ++ hex(signature))`, the beacon's round value, with the signature hex in lowercase. When not, `reason` names the check that failed (`verify-failed`, `signature-not-in-group`, `invalid-public-key`, `missing-signature`, ...).
*   **Example:** `GET /~rng@1.0/verify-bls?public-key=8085...&message=hyperbeam%2Frng%2Fv1%7C<id>%7C1&signature=ae5c...`

### `derive`

*   **Inputs:** `seed` (binary), `counter` (integer, at least 0), `bytes` (integer, 1 to 1024). Integers may be given in decimal.
*   **Returns:** `valid` and `value`: the hex of the first `bytes` bytes of `SHA-256("hyperbeam/rng/v1/derive|" ++ decimal(byte_length(seed)) ++ "|" ++ seed ++ "|" ++ decimal(C)) ++ ...`. The domain and seed length prevent different `(seed, counter)` pairs from encoding to the same hash input.
*   **Example:** `GET /~rng@1.0/derive?seed=abc&counter=0&bytes=64`

## Calling it from Lua

From a `lua@5.3a` process, put the inputs next to the path in one `ao.resolve` message:

```lua
local status, res = ao.resolve({
    path = "/~rng@1.0/verify-bls",
    ["public-key"] = groupKey,
    message = "hyperbeam/rng/v1|" .. processId .. "|" .. round,
    signature = signatureHex
})
-- status == "ok"; res.valid == true; res.value is the round value.
```

To name the device with a `device` key instead, pass the inputs and the path as two messages: `ao.resolve({ device = "rng@1.0", ["public-key"] = k, message = m, signature = s }, { path = "verify-bls" })`. A single message holding both `device` and `path = "verify-bls"` does not reach the device (it resolves to `not_found`). `derive` takes the first shape: `ao.resolve({ path = "/~rng@1.0/derive", seed = s, counter = 0, bytes = 32 })`.

Do not name a Lua handler `verify`: `verify` is a key every message answers itself (commitment verification), so a request for it never calls the Lua function.

## Tests

Run `rebar3 device test --devices dev_rng,dev_lua` for the focused device and Lua interop suite. The BLS vectors in `dev_rng_test_vectors` come from the beacon's own JavaScript (noble-curves): a 2-of-3 DKG group signature, a single-key signature, a wrong message, a wrong key, a corrupted signature, on-curve points outside the subgroups, the identity, and garbage. They include a live beacon round with the value it published.
