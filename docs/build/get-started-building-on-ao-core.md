# Getting Started Building on AO-Core

Welcome to building on AO, the decentralized supercomputer!

AO combines the permanent storage of Arweave with the flexible, scalable computation enabled by the AO-Core protocol and its HyperBEAM implementation. This allows you to create truly autonomous applications, agents, and services that run trustlessly and permissionlessly.

## Core Idea: Processes & Messages

At its heart, building on AO involves:

1.  **Creating Processes:** Think of these as independent programs or stateful contracts. Each process has a unique ID and maintains its own state.
2.  **Sending Messages:** You interact with processes by sending them messages. These messages trigger computations, update state, or cause the process to interact with other processes or the outside world.

Messages are processed by [Devices](../begin/ao-devices.md), which define *how* the computation happens (e.g., running WASM code, executing Lua scripts, managing state transitions).

## Starting `aos`: Your Development Environment

The primary tool for interacting with AO and developing processes is `aos`, a command-line interface and development environment.

=== "npm"
    ```bash
    npm i -g https://get_ao.arweave.net
    ```

=== "bun"
    ```bash
    bun install -g https://get_ao.arweave.net
    ```

=== "pnpm"
    ```bash
    pnpm add -g https://get_ao.arweave.net
    ```

**Starting `aos`:**

Simply run the command in your terminal:

```bash
aos
```

This connects you to an interactive Lua environment running within a **process** on the AO network. This process acts as your command-line interface (CLI) to the AO network, allowing you to interact with other processes, manage your wallet, and develop new AO processes. By default, it connects to a process running on the mainnet Compute Unit (CU).

**What `aos` is doing:**

*   **Connecting:** Establishes a connection from your terminal to a remote process running the `aos` environment.
*   **Loading Wallet:** Looks for a default Arweave key file (usually `~/.aos.json` or specified via arguments) to load into the remote process context for signing outgoing messages.
*   **Providing Interface:** Gives you a Lua prompt (`[aos]>`) within the remote process where you can:
    *   Load code for new persistent processes on the network.
    *   Send messages to existing network processes.
    *   Inspect process state.
    *   Manage your local environment.

## Your First Interaction: Assigning a Variable

From the `aos` prompt, you can assign a variable. Let's assign a basic Lua process that just holds some data:

```lua
[aos]> myVariable = "Hello from aos!"
-- This assigns the string "Hello from aos!" to the variable 'myVariable'
-- within the current process's Lua environment.

[aos]> myVariable
-- Displays the content of 'myVariable'
Hello from aos!
```


## Your First Handler

Follow these steps to create and interact with your first message handler in AO:

1.  **Create a Lua File to Handle Messages:**
    Create a new file named `main.lua` in your local directory and add the following Lua code:

    ```lua
    Handlers.add(
      "HelloWorld",
      function(msg)
        -- This function gets called when a message with Action = "HelloWorld" arrives.
        print("Handler triggered by message from: " .. msg.From)
        -- It replies to the sender with a new message containing the specified data.
        msg.reply({ Data = "Hello back from your process!" })
      end
    )

    print("HelloWorld handler loaded.") -- Confirmation message
    ```

    *   `Handlers.add`: Registers a function to handle incoming messages.
    *   `"HelloWorld"`: The name of this handler. It will be triggered by messages with `Action = "HelloWorld"`.
    *   `function(msg)`: The function that executes when the handler is triggered. `msg` contains details about the incoming message (like `msg.From`, the sender's process ID).
    *   `msg.reply({...})`: Sends a response message back to the original sender. The response must be a Lua table, typically containing a `Data` field.

2.  **Load the Handler into `aos`:**
    From your `aos` prompt, load the handler code into your running process:

    ```lua
    [aos]> .load main.lua
    ```

3.  **Send a Message to Trigger the Handler:**
    Now, send a message to your own process (`ao.id` refers to the current process ID) with the action that matches your handler's name:

    ```lua
    [aos]> Send({ Target = ao.id, Action = "HelloWorld" })
    ```

4.  **Observe the Output:**
    You should see two things happen in your `aos` terminal:
    *   The `print` statement from your handler: `Handler triggered by message from: <your-process-id>`
    *   A notification about the reply message: `New Message From <your-process-id>: Data = Hello back from your process!`

5.  **Inspect the Reply Message:**
    The reply message sent by your handler is now in your process's inbox. You can inspect its data like this:

    ```lua
    [aos]> Inbox[#Inbox].Data
    ```
    This should output: `"Hello back from your process!"`

You've successfully created a handler, loaded it into your AO process, triggered it with a message, and received a reply!

## Next Steps

This is just the beginning. To dive deeper:

*   **AO Cookbook:** Explore practical examples and recipes for common tasks: [AO Cookbook](https://cookbook_ao.arweave.net/)
*   **Expose Process State:** Learn how to make your process data accessible via HTTP using the `patch` device: [Exposing Process State](./exposing-process-state.md)
*   **Serverless Compute:** Discover how to run WASM or Lua computations within your processes: [Serverless Decentralized Compute](./serverless-decentralized-compute.md)
*   **aos Documentation:** Refer to the official `aos` documentation for detailed commands and usage.
