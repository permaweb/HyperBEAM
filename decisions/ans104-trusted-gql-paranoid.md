# ANS-104 GraphQL Trust Is Not Verification

## Superseded Decision

The previous decision allowed `dev_ans104:verify/3` to return true when
`ans104-trust-gql` was enabled and GraphQL trust markers were present, even if
`ar_bundles:verify_item/1` failed. Morning review treats that as papering over
the real issue.

GraphQL trust may be an input-selection or gateway policy, but it is not a
cryptographic ANS-104 verification result. A message that cannot be verified as
an ANS-104 item should not pass `dev_ans104:verify/3` because of trust markers.

## Current Rule

`dev_ans104:verify/3` returns the result of `ar_bundles:verify_item/1`. If
trusted GraphQL data lacks enough material to verify, the caller must avoid
presenting it as a verified ANS-104 commitment or must resolve the missing raw
material.
