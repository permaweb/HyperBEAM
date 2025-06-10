# Serverless Decentralized Compute on AO

AO enables powerful "serverless" computation patterns by allowing you to run code (WASM, Lua) directly within decentralized processes, triggered by messages. Furthermore, if computations are performed on nodes running in Trusted Execution Environments (TEEs), you can obtain cryptographic attestations verifying the execution integrity.

## Core Concept: Compute Inside Processes

Instead of deploying code to centralized servers, you deploy code *to* the Arweave permaweb and instantiate it as an AO process. Interactions happen by sending messages to this process ID.

*   **Code Deployment:** Your WASM binary or Lua script is uploaded to Arweave, getting a permanent transaction ID.
*   **Process Spawning:** You create an AO process, associating it with your code's transaction ID and specifying the appropriate compute device ([`~wasm64@1.0`](../devices/wasm64-at-1-0.md) or [`~lua@5.3a`](../devices/lua-at-5-3a.md)).
*   **Execution via Messages:** Sending a message to the process ID triggers the HyperBEAM node (that picks up the message) to:
    1.  Load the process state.
    2.  Fetch the associated WASM/Lua code from Arweave.
    3.  Execute the code using the relevant device ([`dev_wasm`](../resources/source-code/dev_wasm.md) or [`dev_lua`](../resources/source-code/dev_lua.md)), passing the message data and current state.
    4.  Update the process state based on the execution results.
<!-- 
## Example 1: Running WASM Containers

WebAssembly (WASM) allows you to run precompiled code written in languages like Rust, C++, Go, or AssemblyScript within your AO process.

1.  **Compile:** Compile your code (e.g., Rust) to a WASM target.
2.  **Upload:** Upload the `.wasm` file to Arweave to get a `<WasmCodeTxID>`.
3.  **Spawn Process (using `aos`):**
    ```lua
    [aos]> Send({ Target = "AOS", Action = "Spawn", Module = "<WasmCodeTxID>", Scheduler = "<OptionalSchedulerProcessID>" })
    -- This returns a <ProcessID>
    ```
    *(Note: The exact spawning mechanism might vary; consult `aos` or relevant SDK documentation. You specify that this process uses WASM.)*
4.  **Send Message to Trigger Execution:**
    ```lua
    [aos]> Send({ Target = "<ProcessID>", Action = "ExecuteFunction", InputData = "Some data for WASM" })
    ```
    The HyperBEAM node executing this will load the WASM module identified by `<WasmCodeTxID>`, run its handler function (triggered by `Action = "ExecuteFunction"`) with `"Some data for WASM"` as input, and update the state of `<ProcessID>`.

## Example 2: Running Lua Scripts

Lua provides a lightweight scripting environment directly within AO.

1.  **Write Script:** Create your `my_script.lua` file.
    ```lua
    -- my_script.lua
    Handlers.add(
      "Calculate",
      Handlers.utils.hasMatchingTag("Action", "Calculate"),
      function (msg)
        local input = tonumber(msg.Tags.Value) or 0
        local result = input * input
        -- Update state or send messages back
        ao.send({ Target = msg.From, Data = "Result: " .. result })
        print("Calculated square of " .. input .. ": " .. result)
      end
    )
    ```
2.  **Load & Spawn (using `aos`):**
    ```lua
    [aos]> .load my_script.lua
    [aos]> MyLuaProcess = spawn(MyModule)
    ```
3.  **Send Message:**
    ```lua
    [aos]> Send({ Target = MyLuaProcess, Action = "Calculate", Value = "7" })
    ```
    The node executes the `Calculate` handler within the Lua script associated with `MyLuaProcess`. -->

## TEE Attestations (via [`~snp@1.0`](../resources/source-code/dev_snp.md))

If a HyperBEAM node performing these computations runs within a supported Trusted Execution Environment (like AMD SEV-SNP), it can provide cryptographic proof of execution.

*   **How it works:** The [`~snp@1.0`](../resources/source-code/dev_snp.md) device interacts with the TEE hardware.
*   **Signed Responses:** When a TEE-enabled node processes your message (e.g., executes your WASM function), the HTTP response containing the result can be cryptographically signed by a key that *provably* only exists inside the TEE.
*   **Verification:** Clients receiving this response can verify the signature against the TEE platform's attestation mechanism (e.g., AMD's KDS) to gain high confidence that the computation was performed correctly and confidentially within the secure environment, untampered by the node operator.

**Obtaining Attested Responses:**

This usually involves interacting with nodes specifically advertised as TEE-enabled. The exact mechanism for requesting and verifying attestations depends on the specific TEE technology and node configuration.

*   The HTTP response headers might contain specific signature or attestation data (e.g., using HTTP Message Signatures RFC-9421 via [`dev_codec_httpsig`](../resources/source-code/dev_codec_httpsig.md)).
*   You might query the [`~snp@1.0`](../resources/source-code/dev_snp.md) device directly on the node to get its attestation report.

Refer to documentation on [TEE Nodes](./run/tee-nodes.md) and the [`~snp@1.0`](../resources/source-code/dev_snp.md) device for details.

By leveraging WASM, Lua, and optional TEE attestations, AO provides a powerful platform for building complex, verifiable, and truly decentralized serverless applications.
