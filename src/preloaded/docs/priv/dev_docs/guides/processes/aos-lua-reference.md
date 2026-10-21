# AOS Lua Reference

This page covers the AOS essentials used throughout the `process@1.0` examples.

Inspect workflow examples show request, configuration, or operator command shapes. Run them only after providing the stated local prerequisites; the browser runner does not execute these blocks.

## Install And Start

```text
npm i -g https://get_ao.arweave.net
aos --url http://localhost:8734
```

Use a wallet:

```text
aos --wallet ./wallet.json --url http://localhost:8734
```

Run a command non-interactively when supported by your AOS version:

```text
aos <process-id> --run "return 1 + 1"
```

## Load Code

Load a local Lua file:

```lua
.load ./process.lua
```

Load a blueprint:

```lua
.load-blueprint chat
```

Blueprints are templates for common process patterns such as chat, token, staking, and voting.

## Handlers

```lua
Handlers.add(
  "ActionName",
  Handlers.utils.hasMatchingTag("Action", "ActionName"),
  function(msg)
    -- mutate process state
    -- send patch updates for public reads
    return msg.reply({ Status = "OK" })
  end
)
```

Unhandled messages remain in the inbox. Handled messages do not need manual inbox processing.

## Common Globals

- `ao.id`: current process ID.
- `ao.env`: process and module metadata.
- `Inbox`: unhandled inbound messages.
- `Handlers`: handler registration utilities.
- `Send`: send a message.
- `ao.spawn`: spawn a process.

## JSON

Use the AOS JSON module when a handler needs to encode or decode structured data:

```lua
local json = require("json")

local data = json.decode(msg.Data)
local out = json.encode({ status = "ok", value = data.value })
```

## Replies

```lua
return msg.reply({
  Status = "OK",
  Data = "hello"
})
```

For app data that should be read repeatedly, prefer patching state and reading over HTTP instead of asking clients to message the process for every read.

## Tag Rules

Tags are the process API surface. Keep them predictable:

```lua
Tags = {
  Action = "Transfer",
  Recipient = "<address>",
  Amount = "1000",
  ["Request-Id"] = "abc123"
}
```

Use dash-separated tag names for multi-word tags. Avoid mixed-case tags where exact casing matters.

## Debugging

- Print small values during development.
- Inspect replies from `ao.message` or `ao.result`.
- Check patched state with `curl`.
- If an HTTP read fails, confirm the handler actually sent a `patch@1.0` message after the state changed.

## Production Notes

- Validate all message inputs.
- Check authorization before modifying privileged state.
- Patch only public state.
- Keep handler side effects explicit.
- Use selected node URLs from current marketplace/listing sources, not stale examples.
