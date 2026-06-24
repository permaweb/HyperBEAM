# Secret Commitment Verification

## Issue

Cache paranoia verifies materialized committed subsets on `cache_read` and
`cache_write`. Secret-key HMAC commitments, such as `cookie@1.0` commitments
with `secret:` key IDs, cannot be verified from a generic cache read/write
because the secret is intentionally not present in the commitment.

## Options

- Skip all commitment verification for cache topics.
- Treat missing secret material as corruption.
- Defer only secret-key HMAC commitments that lack the secret, while continuing
  to verify self-contained materialized commitments.

## Decision

Defer `hmac-sha256` commitments with `secret:` key IDs when the cache verifier
does not have a `secret` value. This preserves strict cache checking for
self-contained commitments while acknowledging that cookie/secret commitments
are only verifiable in their authenticated request context.
