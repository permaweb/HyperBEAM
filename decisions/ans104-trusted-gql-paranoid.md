# ANS-104 Trusted GraphQL Commitments Under Paranoid Verification

## Issue

Gateway GraphQL reads may synthesize ANS-104-shaped messages from data the node
is configured to trust with `ans104-trust-gql`. These messages can carry
`trusted-keys` and signature fields, but they may not have enough raw bundle
material for `ar_bundles:verify_item/1` to prove the item cryptographically.

Under paranoid cache writes, rejecting those trusted GraphQL commitments changed
the expected remote commitment identity for real gateway-backed manifest asset
tests.

## Options

- Suppress GraphQL trust under paranoid mode. This changes gateway semantics
  and loses expected remote commitments.
- Treat every failed ANS-104 verification as trusted. This is too broad.
- Preserve the existing trust policy only when `ans104-trust-gql` is enabled
  and the request carries the GraphQL trust markers.

## Decision

Use the third option. `dev_ans104:verify/3` still prefers
`ar_bundles:verify_item/1`; it only accepts the trusted path when node opts
enable GraphQL trust and the request has both `trusted-keys` and `signature`.
