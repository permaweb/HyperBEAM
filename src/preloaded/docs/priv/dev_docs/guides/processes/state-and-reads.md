# State And Reads

HyperBEAM changes the default AO read pattern. Instead of sending dry-run messages just to inspect state, expose read keys through the `patch@1.0` device and fetch them over HTTP.

Inspect workflow examples show request, configuration, or operator command shapes. Run them only after providing the stated local prerequisites; the browser runner does not execute these blocks.

## Patch Device

The [`patch@1.0` device](/~patch@1.0/docs) pushes selected process data to HyperBEAM endpoints so it can be discovered and fetched by clients. Each key sent to the patch device becomes readable through the process endpoint.

Patch selected state:

```lua
Send({
  device = "patch@1.0",
  counter = "42",
  status = "active"
})
```

Read selected state:

```text
GET /<process-id>~process@1.0/compute/counter
GET /<process-id>~process@1.0/compute/status
```

With a node:

```sh
HB="${HB:-http://localhost:8734}"
PROCESS_ID="<process-id>"
curl -fsS "$HB/$PROCESS_ID~process@1.0/compute/counter"
```

## Initial Sync

Use an initial sync block so clients can read useful state immediately:

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

## Patch After Mutations

Patch state in every handler that changes read-facing data:

```lua
Handlers.add(
  "Transfer",
  Handlers.utils.hasMatchingTag("Action", "Transfer"),
  function(msg)
    local amount = tonumber(msg.Tags.Amount)
    local recipient = msg.Tags.Recipient

    if not amount or amount <= 0 or not recipient then
      return msg.reply({ Error = "Invalid transfer" })
    end

    Balances[msg.From] = tostring((tonumber(Balances[msg.From]) or 0) - amount)
    Balances[recipient] = tostring((tonumber(Balances[recipient]) or 0) + amount)

    Send({
      device = "patch@1.0",
      balances = Balances
    })

    return msg.reply({ Status = "OK" })
  end
)
```

## `compute` vs `now`

- `compute/<key>` reads an exposed value quickly.
- `now/<key>` can ask HyperBEAM to evaluate the freshest process state path.

Use `compute` for normal app reads. Use `now` only when a workflow explicitly requires the most current process state path.
