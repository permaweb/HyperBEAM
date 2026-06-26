# Intro to `process@1.0` & Processes

AO has many devices. Among them, the `process@1.0` device is the one you use to create and interact with "smart contracts" called processes.

Inspect workflow examples show request, configuration, or operator command shapes. Run them only after providing the stated local prerequisites; the browser runner does not execute these blocks.

A process is a deterministic log of state transitions stored on Arweave. Messages add inputs to that log, the process evaluates those messages, and HyperBEAM makes selected process data available through endpoints.

Use this page to create a process, load a small counter, send it a message, and read exposed state through the `process@1.0` device.

## Process Model

- A process is the running stateful instance.
- A module is the code used to initialize or execute the process.
- A message is a signed ANS-104 data item addressed to a process.
- A handler matches messages and runs process logic.
- The inbox keeps unhandled messages.
- `ao.id` is the current process ID inside AOS.
- `Send` sends outbound messages.
- `ao.spawn` can create a new process from a module.

## Message Shape

Messages include data, target, sender, tags, signatures, and scheduling metadata. The core fields a handler usually reads are:

```lua
msg.From
msg.Target
msg.Data
```

`msg.Data` can be empty, but it is the right place for larger payloads. Tags can carry routing and metadata such as `Action`, `Amount`, or `Recipient`.

ANS-104 tag limits:

- Up to 128 tags per message.
- Tag key/name: up to 1024 bytes.
- Tag value: up to 3072 bytes.

Keep tag values as strings unless a process explicitly documents otherwise.

## Process Authorities

For process-to-process messages to be accepted, the recipient process must trust the authority used by the incoming message. In practice, processes that talk to each other should use the same HyperBEAM authority or compatible authority sets. If the recipient does not have the authority, it can reject the message.

# Using AOS

## Prerequisites

- Node.js 20 or newer.
- A terminal.
- An Arweave wallet keyfile, or let AOS create one.
- A HyperBEAM node URL. Use your own local node or select a public node from `https://lunar.arweave.net` or another current marketplace/list.

## Install AOS

```text
npm i -g https://get_ao.arweave.net
```

## Create a Process

```text
aos process_name --url http://localhost:8734
```


Replace `process_name` with a friendly local name for the process, such as `counter`. Reuse the same `aos process_name --url https://node.url` command to re-enter the process console after exiting. Pass `--url https://node.url` so the selected node is explicit as the executor of the process.

To use a specific wallet:

```text
aos process_name --wallet ./wallet.json --url http://localhost:8734
```

## Create A Counter

To paste multiline Lua into the AOS prompt, open the inline editor first:

```lua
.editor
```

Paste the counter code below into the editor, then type this on its own line to exit the editor and execute the code:

```lua
.done
```

Alternatively, save the code below to an `example.lua` file and load it with:

```lua
.load path/to/example.lua
```

Note: The [`patch@1.0` device](/~patch@1.0/docs) pushes selected process data to HyperBEAM endpoints so clients can discover and fetch it. In this example, the `counter`, `lastupdate`, and `updatedby` keys become readable through the process endpoint.

The code below registers an `Increment` handler. A handler has a name, a matcher, and a function that runs when the matcher succeeds.

```lua
Counter = Counter or 0

Handlers.add(
  "Increment",
  Handlers.utils.hasMatchingTag("Action", "Increment"),
  function(msg)
    Counter = Counter + 1

    Send({
      device = "patch@1.0",
      counter = tostring(Counter),
      lastupdate = tostring(os.time()),
      updatedby = msg.From
    })

    return msg.reply({
      Status = "OK",
      Counter = tostring(Counter)
    })
  end
)
```

Send a message to your own process:

```lua
Send({ Target = ao.id, Tags = { Action = "Increment" } })
```

## Read State Through `process@1.0`

Use your selected node and process ID:

```sh
HB="${HB:-http://localhost:8734}"
PROCESS_ID="<process-id>"
curl -fsS "$HB/$PROCESS_ID~process@1.0/compute/counter"
```


The response is the patched `counter` value. This is the core HyperBEAM read pattern: mutate process state through messages, then expose selected read state over HTTP.

## Initial State Sync

Patch important read keys once when the process starts:

```lua
Counter = Counter or 0
InitialSync = InitialSync or "INCOMPLETE"

if InitialSync == "INCOMPLETE" then
  Send({
    device = "patch@1.0",
    counter = tostring(Counter)
  })

  InitialSync = "COMPLETE"
end
```

## Naming Rules

- Prefer lowercase patch keys: `counter`, `balances`, `totalsupply`.
- Avoid reserved path words as keys: `now`, `compute`, `state`, `info`, `test`.
- Prefer dash-separated tag names: `Example-Tag`, `Transfer-Id`.
- Do not rely on mixed-case tag normalization. HyperBEAM can normalize tag casing in ways that break older handlers.

## Local HyperBEAM Node

For local development, start HyperBEAM with the profile your environment requires, then point AOS at it:

```text
aos myLocalProcess --url http://localhost:8734
```

The upstream cookbook commonly references a `genesis_wasm` profile for local AOS execution. Treat the exact local boot command as environment-specific and keep the selected node URL explicit.
