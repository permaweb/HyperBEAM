# Decision: size the demo upload to the wallet

## The prompt, as understood

Intent #1 requires uploading an index in this format covering at least 10M
indexed messages, using ~/Documents/hyperbeam-key.json, and demonstrating a
node querying it as an arlmdb match-store.

## The issue

The W6 scan of ~900 GB of module 106 yielded 26.1M items and 246M match
rows. At mainnet's ~11.16 AR/GB, the full match container (~4.2 GB) alone
costs ~47 AR; the wallet holds 50.23 AR. Uploading everything scanned would
drain the wallet and still risk failure mid-seed.

## Options

1. Upload full offset (~550 MB, ~6 AR) + full match (~4.2 GB, ~47 AR):
   ~53 AR — exceeds the balance.
2. Upload a contiguous lowest-offset slice of ~11M items: offset ~230 MB
   (~2.6 AR) + match ~105M rows ~1.8 GB (~20 AR): ~23 AR total, leaving
   ~27 AR margin.
3. Offset container only: cheap but does not demonstrate the match-store,
   which the brief names explicitly.

## Decision

Option 2. "At least 10M indexed messages" is met with margin; the slice is
bounded by a single offset O (rows with offset < O in both containers), so
it is a consistent snapshot and pagination/leapfrog over it behaves exactly
as a full index would. The full artifacts stay on disk for a later, funded
publication.
