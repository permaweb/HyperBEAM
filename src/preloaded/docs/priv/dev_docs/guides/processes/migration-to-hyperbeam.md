# Migration To HyperBEAM

Use this page to move old AO or Legacynet processes and clients into the HyperBEAM-first `@PROCESS1.0` model.

Inspect workflow examples show request, configuration, or operator command shapes. Run them only after providing the stated local prerequisites; the browser runner does not execute these blocks.

## Main Change

Primary HyperBEAM read pattern:

```js
const balances = await fetch(
  `http://localhost:8734/${processId}~process@1.0/compute/balances`,
).then((res) => res.json());
```

The process still mutates state through messages. The difference is that read-facing state is patched and read through HTTP. Use historical read simulations only when supporting existing Legacynet processes.

## Step 1: Connect To HyperBEAM

```text
aos --url http://localhost:8734
```

The node URL is a deployment choice. Pick a current node from `https://lunar.arweave.net` or another marketplace/listing source.

## Step 2: Add Initial Patches

```lua
Balances = Balances or {}
TotalSupply = TotalSupply or "0"
InitialSync = InitialSync or "INCOMPLETE"

if InitialSync == "INCOMPLETE" then
  Send({
    device = "patch@1.0",
    balances = Balances,
    totalsupply = TotalSupply
  })

  InitialSync = "COMPLETE"
end
```

## Step 3: Patch In Mutating Handlers

```lua
local function patchPublicState()
  Send({
    device = "patch@1.0",
    balances = Balances,
    totalsupply = TotalSupply
  })
end

Handlers.add(
  "Mint",
  Handlers.utils.hasMatchingTag("Action", "Mint"),
  function(msg)
    local amount = tonumber(msg.Tags.Amount)
    local recipient = msg.Tags.Recipient

    if not amount or amount <= 0 or not recipient then
      return msg.reply({ Error = "Invalid mint" })
    end

    Balances[recipient] = tostring((tonumber(Balances[recipient]) or 0) + amount)
    TotalSupply = tostring((tonumber(TotalSupply) or 0) + amount)

    patchPublicState()
    return msg.reply({ Status = "OK" })
  end
)
```

## Step 4: Replace Client Reads

```js
const balances = await fetch(
  `http://localhost:8734/${processId}~process@1.0/compute/balances`,
).then((res) => res.json());

const balance = balances[address] || "0";
```

## Step 5: Update Tags

Audit handlers that rely on exact mixed-case tag names. Prefer:

```lua
Handlers.utils.hasMatchingTag("Action", "Transfer")
msg.Tags["Request-Id"]
msg.Tags["Transfer-Id"]
```

Avoid relying on names like `ExampleTag` when casing normalization could affect routing.

## Migration Checklist

- HyperBEAM node selected from a current list.
- AOS connects with `--url`.
- Remove hard-coded Legacynet MU/CU URLs.
- No legacy read simulations in the primary read path.
- Public state is patched with lowercase keys.
- Mutating handlers patch after successful state changes.
- Client reads use `process@1.0/compute`.
- Use `--legacy` only for existing Legacynet processes.
