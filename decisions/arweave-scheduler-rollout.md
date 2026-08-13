# Arweave scheduler rollout boundary

## Original prompt

Replace the experimental dense Arweave scheduler with a clean global target
index, ignore all existing test processes, and reissue the assets against the
new protocol without legacy compatibility.

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
3. Fix a common rollout block immediately before reissuance and reject older
   processes.

## Decision

Use block `1978888` as the default inclusive rollout boundary. It was the
confirmed frontier at implementation time (current height minus the default
10-block confirmation depth). Persist `from` in `sync/global` and fail closed
if a node later supplies a different configured value for the same store.

This gives every fresh node the same completeness boundary, keeps the first
scan small for the reissued assets, and introduces no compatibility path for
the abandoned test processes. Deployments may override the compiled default
only as a coordinated protocol choice before building the store.
