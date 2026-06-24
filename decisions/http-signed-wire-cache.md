# HTTP Signed Wire Cache

## Issue

`HB_PARANOID=cache_read,cache_write` exposed that `hb_http` could store a
signed message decoded from the wire even when the node was not requiring
request signature verification and the decoded commitments did not verify in
that post-transport shape.

## Options

- Broaden cache paranoia so cache writes skip such commitments.
- Change HTTP request policy and reject all unverifiable signed messages.
- Keep request policy unchanged, but only cache signed wire messages after
  verifying their commitments.

## Decision

Keep HTTP acceptance semantics unchanged for nodes that do not force signed
requests, but require successful verification before `store-all-signed` writes
the decoded signed message to the cache.

This keeps cache writes cryptographically honest without expanding the policy
surface of this branch.
