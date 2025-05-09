# Device: ~scheduler@1.0

## Overview

The [`~scheduler@1.0`](../resources/source-code/dev_scheduler.md) device manages the queueing and ordering of messages targeted at a specific process ([`~process@1.0`](../resources/source-code/dev_process.md)). It ensures that messages are processed according to defined scheduling rules.

## Core Concept: Message Ordering

When messages are sent to an AO process (typically via the [`~push@1.0`](../resources/source-code/dev_push.md) device or a `POST` to the process's `/schedule` endpoint), they are added to a queue managed by the Scheduler Device associated with that process. The scheduler ensures that messages are processed one after another in a deterministic order, typically based on arrival time and potentially other factors like message nonces or timestamps (depending on the specific scheduler implementation details).

The [`~process@1.0`](../resources/source-code/dev_process.md) device interacts with its configured Scheduler Device (which defaults to `~scheduler@1.0`) primarily through the `next` key to retrieve the next message to be executed.

## Slot System

Slots are a fundamental concept in the `~scheduler@1.0` device, providing a structured mechanism for organizing and sequencing computation.

*   **Sequential Ordering:** Slots act as numbered containers (starting at 0) that hold specific messages or tasks to be processed in a deterministic order.
*   **State Tracking:** The `at-slot` key in a process's state (or a similar internal field like `current-slot` within the scheduler itself) tracks execution progress, indicating which messages have been processed and which are pending. The `slot` function can be used to query this.
*   **Assignment Storage:** Each slot contains an "assignment" - the cryptographically verified message waiting to be executed. These assignments are retrieved using the `schedule` function or internally via `next`.
*   **Schedule Organization:** The collection of all slots for a process forms its "schedule".
*   **Application Scenarios:**
    * **Scheduling Messages:** When a message is posted to a process (e.g., via `register`), it's assigned to the next available slot.
    * **Status Monitoring:** Clients can query a process's current slot (via the `slot` function) to check progress.
    * **Task Retrieval:** Processes find their next task by requesting the next assignment via the `next` function, which implicitly uses the next slot number based on the current state.
    * **Distributed Consistency:** Slots ensure deterministic execution order across nodes, crucial for maintaining consistency in AO.

This slotting mechanism is central to AO processes built on HyperBEAM, allowing for deterministic, verifiable computation.

## Key Functions (Keys)

These keys are typically accessed via the [`~process@1.0`](../resources/source-code/dev_process.md) device, which delegates the calls to its configured scheduler.

*   **`schedule` (Handler for `GET /<ProcessID>~process@1.0/schedule`)**
    *   **Action:** Retrieves the list of pending assignments (messages) for the process. May support cursor-based traversal for long schedules.
    *   **Response:** A message map containing the assignments, often keyed by slot number or message ID.
*   **`register` (Handler for `POST /<ProcessID>~process@1.0/schedule`)**
    *   **Action:** Adds/registers a new message to the process's schedule. If this is the first message for a process, it might initialize the scheduler state.
    *   **Request Body:** The message to schedule.
    *   **Response:** Confirmation, potentially including the assigned slot or message ID.
*   **`slot` (Handler for `GET /<ProcessID>~process@1.0/slot`)**
    *   **Action:** Queries the current or a specific slot number within the process's schedule.
    *   **Response:** Information about the requested slot, such as the current highest slot number.
*   **`status` (Handler for `GET /<ProcessID>~process@1.0/status`)**
    *   **Action:** Retrieves status information about the scheduler for the process.
    *   **Response:** A status message.
*   **`next` (Internal Key used by [`~process@1.0`](../resources/source-code/dev_process.md))**
    *   **Action:** Retrieves the next assignment message from the schedule based on the process's current `at-slot` state.
    *   **State Management:** Requires the current process state (`Msg1`) containing the `at-slot` key.
    *   **Response:** `{ok, #{ "body" => <NextAssignmentMsg>, "state" => <UpdatedProcessState> }}` or `{error, Reason}` if no next assignment is found.
    *   **Caching & Lookahead:** The implementation uses internal caching (`dev_scheduler_cache`, `priv/assignments`) and potentially background lookahead workers to optimize fetching subsequent assignments.
*   **`init` (Internal Key)**
    *   **Action:** Initializes the scheduler state for a process, often called when the process itself is initialized.
*   **`checkpoint` (Internal Key)**
    *   **Action:** Triggers the scheduler to potentially persist its current state or perform other checkpointing operations.

## Interaction with Other Components

*   **[`~process@1.0`](../resources/source-code/dev_process.md):** The primary user of the scheduler, calling `next` to drive process execution.
*   **[`~push@1.0`](../resources/source-code/dev_push.md):** Often used to add messages to the schedule via `POST /schedule`.
*   **`dev_scheduler_cache`:** Internal module used for caching assignments locally on the node to reduce latency.
*   **Scheduling Unit (SU):** Schedulers may interact with external entities (like Arweave gateways or dedicated SU nodes) to fetch or commit schedules, although `~scheduler@1.0` aims for a simpler, often node-local or SU-client model.

`~scheduler@1.0` provides the fundamental mechanism for ordered, sequential execution within the potentially asynchronous and parallel environment of AO.

[scheduler module](../resources/source-code/dev_scheduler.md)
