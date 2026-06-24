# Bundler Invalid Item Under Cache-Write Paranoia

## Issue

`dev_bundler:invalid_item_test_parallel/0` intentionally tampers with a signed
ANS-104 item, then expects `bundler@1.0` to reject it with
`signature-verification-failed`.

With `HB_PARANOID=cache_read,cache_write`, sending that deliberately invalid
signed message as a nested HTTP request body can fail before the request reaches
`bundler@1.0`: lower-level link normalization/cache writes refuse to persist the
invalid signed body.

## Decision

Keep the test focused on the device contract. Build the invalid structured item
in memory with linkification disabled, call `dev_bundler:item/3` directly, and
assert the device returns the intended `400 invalid-item` response.

Do not weaken paranoid cache verification and do not add a special cache-write
escape hatch for invalid signed messages.
