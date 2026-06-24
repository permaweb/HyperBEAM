# Manifest Fallback Cache Policy

## Issue

Ordinary manifest route hits are deterministic products of the manifest message
and path, and should be cacheable.

Invalid-path fallback is different. Whether an invalid manifest path should
fallback to `index` or return `not_found` is controlled by local
`manifest_404` node options. That option is not part of the AO message cache
key. If a fallback result is cached at `/invalid_path`, a later request under
`manifest_404 = error` can read the stale fallback.

## Decision

Keep ordinary manifest route results cacheable. Mark only fallback results
private `no-store`, because they depend on node-local policy that is outside
the message input. If this becomes common, the cleaner general solution is to
make such local policy explicit in the request or in a cache-key dimension, not
to broaden manifest no-store again.
