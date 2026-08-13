# Arweave scheduler rollout boundary

## Original prompt

Replace the experimental dense Arweave scheduler with a clean global target
index. Retain the existing test assets by moving the fixed boundary 10,000
blocks earlier; their pre-`Assign-To` payments intentionally remain absent.

## Issue

Every node needs the same first completely indexed block. Deriving that block
from local startup time would make schedule completeness node-dependent, while
backfilling all historical Arweave blocks would retain work the clean rollout
explicitly makes unnecessary.

## Options

1. Start from each node's first observed block. Smallest initial scan, but
   different nodes can present different schedules for the same process.
2. Backfill from genesis. Complete for discarded test processes, but far beyond
   the requested migration and operationally expensive.
3. Fix a common rollout block 10,000 blocks before the original boundary and
   reject older processes.

## Decision

Use block `1968888` as the default inclusive rollout boundary. This is exactly
10,000 blocks before the original `1978888` boundary and covers the existing
test assets while preserving a finite, deterministic scan. Persist `from` in
`sync/global` and fail closed if a node later supplies a different configured
value for the same store.

This gives every fresh node the same completeness boundary and introduces no
compatibility path for payments that lacked `Assign-To`. Deployments may
override the compiled default only as a coordinated protocol choice before
building the store.
