%%% Shared state and task records for the bundler server and workers.

-record(state, {
    max_size,
    max_idle_time,
    max_items,
    queue,
    bytes,
    workers,
    task_queue,
    bundles,
    opts,
    dispatch_ref
}).

-record(task, {
    bundle_id,
    type,
    data,
    opts,
    retry_count = 0
}).

-record(proof, {
    proof,
    status
}).

-record(bundle, {
    id,
    items,
    item_sizes,
    status,
    tx,
    proofs,
    start_time
}).

-define(DEFAULT_NUM_WORKERS, 20).
-define(DEFAULT_RETRY_BASE_DELAY_MS, 1000).
-define(DEFAULT_RETRY_MAX_DELAY_MS, 600000).
-define(DEFAULT_RETRY_JITTER, 0.25).
