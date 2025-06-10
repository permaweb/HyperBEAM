# Exposing Process State with the Patch Device

The [`~patch@1.0`](../resources/source-code/dev_patch.md) device provides a mechanism for AO processes to expose parts of their internal state, making it readable via direct HTTP GET requests along the process's HyperPATH.

## Why Use the Patch Device?

Standard AO process execution typically involves sending a message to a process, letting it compute, and then potentially reading results from its outbox or state after the computation is scheduled and finished. This is asynchronous.

The `patch` device allows for a more direct, synchronous-like read pattern. A process can use it to "patch" specific data elements from its internal state into a location that becomes directly accessible via a HyperPATH GET request *before* the full asynchronous scheduling might complete.

This is particularly useful for:

*   **Web Interfaces:** Building frontends that need to quickly read specific data points from an AO process without waiting for a full message round-trip.
*   **Data Feeds:** Exposing specific metrics or state variables for monitoring or integration with other systems.
*   **Caching:** Allowing frequently accessed data to be retrieved efficiently via simple HTTP GETs.

## How it Works

1.  **Process Logic:** Inside your AO process code (e.g., in Lua or WASM), when you want to expose data, you construct an **Outbound Message** targeted at the [`~patch@1.0`](../resources/source-code/dev_patch.md) device.
2.  **Patch Message Format:** This outbound message typically includes tags that specify:
    *   `device = 'patch@1.0'`
    *   A `cache` tag containing a table. The **keys** within this table become the final segments in the HyperPATH used to access the data, and the **values** are the data itself.
    *   Example Lua using `aos`: `Send({ Target = ao.id, device = 'patch@1.0', cache = { mydatakey = MyValue } })`
3.  **HyperBEAM Execution:** When HyperBEAM executes the process schedule and encounters this outbound message:
    *   It invokes the `dev_patch` module.
    *   `dev_patch` inspects the message.
    *   It takes the keys from the `cache` table (`mydatakey` in the example) and their associated values (`MyValue`) and makes these values available under the `/cache/` path segment.
4.  **HTTP Access:** You (or any HTTP client) can now access this data directly using a GET request:
    ```
    GET /<process-id>~process@1.0/compute/cache/<mydatakey>
    # Or potentially using /now/
    GET /<process-id>~process@1.0/now/cache/<mydatakey>
    ```
    The HyperBEAM node serving the request will resolve the path up to `/compute/cache` (or `/now/cache`), then use the logic associated with the patched data (`mydatakey`) to return the `MyValue` directly.

## Initial State Sync (Optional)

It can be beneficial to expose the initial state of your process via the `patch` device as soon as the process is loaded or spawned. This makes key data points immediately accessible via HTTP GET requests without requiring an initial interaction message to trigger a `Send` to the patch device.

This pattern typically involves checking a flag within your process state to ensure the initial sync only happens once. Here's an example from the Token Blueprint, demonstrating how to sync `Balances` and `TotalSupply` right after the process starts:

```lua
-- Place this logic at the top level of your process script, 
-- outside of specific handlers, so it runs on load.

-- Initialize the sync flag if it doesn't exist
InitialSync = InitialSync or 'INCOMPLETE'

-- Sync state on spawn/load if not already done
if InitialSync == 'INCOMPLETE' then
  -- Send the relevant state variables to the patch device
  Send({ device = 'patch@1.0', cache = { balances = Balances, totalsupply = TotalSupply } })
  -- Update the flag to prevent re-syncing on subsequent executions
  InitialSync = 'COMPLETE'
  print("Initial state sync complete. Balances and TotalSupply patched.")
end
```

**Explanation:**

1.  `InitialSync = InitialSync or 'INCOMPLETE'`: This line ensures the `InitialSync` variable exists in the process state, initializing it to `'INCOMPLETE'` if it's the first time the code runs.
2.  `if InitialSync == 'INCOMPLETE' then`: The code proceeds only if the initial sync hasn't been marked as complete.
3.  `Send(...)`: The relevant state (`Balances`, `TotalSupply`) is sent to the `patch` device, making it available under `/cache/balances` and `/cache/totalsupply`.
4.  `InitialSync = 'COMPLETE'`: The flag is updated, so this block won't execute again in future message handlers within the same process lifecycle.

This ensures that clients or frontends can immediately query essential data like token balances as soon as the process ID is known, improving the responsiveness of applications built on AO.

## Example (Lua in `aos`)

```lua
-- In your process code (e.g., loaded via .load)
Handlers.add(
  "PublishData",
  Handlers.utils.hasMatchingTag("Action", "PublishData"),
  function (msg)
    local dataToPublish = "Some important state: " .. math.random()
    -- Expose 'currentstatus' key under the 'cache' path
    Send({ device = 'patch@1.0', cache = { currentstatus = dataToPublish } })
    print("Published data to /cache/currentstatus")
  end
)

-- Spawning and interacting
[aos]> MyProcess = spawn(MyModule)

[aos]> Send({ Target = MyProcess, Action = "PublishData" })
-- Wait a moment for scheduling

```

## Avoiding Key Conflicts

When defining keys within the `cache` table (e.g., `cache = { mydatakey = MyValue }`), these keys become path segments under `/cache/` (e.g., `/compute/cache/mydatakey` or `/now/cache/mydatakey`). It's important to choose keys that do not conflict with existing, reserved path segments used by HyperBEAM or the `~process` device itself for state access.

Using reserved keywords as your cache keys can lead to routing conflicts or prevent you from accessing your patched data as expected. While the exact list can depend on device implementations, it's wise to avoid keys commonly associated with state access, such as: `now`, `compute`, `state`, `info`, `test`.

It's recommended to use descriptive and specific keys for your cached data to prevent clashes with the underlying HyperPATH routing mechanisms. For example, instead of `cache = { state = ... }`, prefer `cache = { myappstate = ... }` or `cache = { usercount = ... }`.

!!! warning
    Be aware that HTTP path resolution is case-insensitive and automatically normalizes paths to lowercase. While the `patch` device itself stores keys with case sensitivity (e.g., distinguishing `MyKey` from `mykey`), accessing them via an HTTP GET request will treat `/cache/MyKey` and `/cache/mykey` as the same path. This means that using keys that only differ in case (like `MyKey` and `mykey` in your `cache` table) will result in unpredictable behavior or data overwrites when accessed via HyperPATH. To prevent these issues, it is **strongly recommended** to use **consistently lowercase keys** within the `cache` table (e.g., `mykey`, `usercount`, `appstate`).

## Key Points

*   **Path Structure:** The data is exposed under the `/cache/` path segment. The tag name you use *inside* the `cache` table in the `Send` call (e.g., `currentstatus`) becomes the final segment in the accessible HyperPATH (e.g., `/compute/cache/currentstatus`).
*   **Data Types:** The `patch` device typically handles basic data types (strings, numbers) within the `cache` table effectively. Complex nested tables might require specific encoding or handling.
*   **`compute` vs `now`:** Accessing patched data via `/compute/cache/...` typically serves the last known patched value quickly. Accessing via `/now/cache/...` might involve more computation to ensure the absolute latest state before checking for the patched key under `/cache/`.
*   **Not a Replacement for State:** Patching is primarily for *exposing* reads. It doesn't replace the core state management within your process handler logic.

By using the `patch` device, you can make parts of your AO process state easily and efficiently readable over standard HTTP, bridging the gap between decentralized computation and web-based applications.