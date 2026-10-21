# Builder Templates

These templates can be copied into real processes and adapted around the `process@1.0` read model.

## Token State

```lua
Balances = Balances or {}
TotalSupply = TotalSupply or "0"
Name = Name or "ExampleToken"
Ticker = Ticker or "EXT"

local function patchToken()
  Send({
    device = "patch@1.0",
    balances = Balances,
    totalsupply = TotalSupply,
    name = Name,
    ticker = Ticker
  })
end

InitialSync = InitialSync or "INCOMPLETE"
if InitialSync == "INCOMPLETE" then
  patchToken()
  InitialSync = "COMPLETE"
end
```

Transfer handler:

```lua
Handlers.add(
  "Transfer",
  Handlers.utils.hasMatchingTag("Action", "Transfer"),
  function(msg)
    local recipient = msg.Tags.Recipient or msg.Tags.Target
    local amount = tonumber(msg.Tags.Amount or msg.Tags.Quantity)

    if not recipient or not amount or amount <= 0 then
      return msg.reply({ Error = "Invalid transfer" })
    end

    local senderBalance = tonumber(Balances[msg.From]) or 0
    if senderBalance < amount then
      return msg.reply({ Error = "Insufficient balance" })
    end

    Balances[msg.From] = tostring(senderBalance - amount)
    Balances[recipient] = tostring((tonumber(Balances[recipient]) or 0) + amount)

    patchToken()

    return msg.reply({
      Status = "OK",
      From = msg.From,
      To = recipient,
      Amount = tostring(amount)
    })
  end
)
```

Client read:

```js
const balances = await fetch(
  `http://localhost:8734/${processId}~process@1.0/compute/balances`,
).then((res) => res.json());
```

## Chat State

```lua
Messages = Messages or {}
Users = Users or {}

local function patchChat()
  Send({
    device = "patch@1.0",
    messages = Messages,
    users = Users,
    messagecount = tostring(#Messages)
  })
end

Handlers.add(
  "AddMessage",
  Handlers.utils.hasMatchingTag("Action", "AddMessage"),
  function(msg)
    if not msg.Data or msg.Data == "" then
      return msg.reply({ Error = "Message cannot be empty" })
    end

    local id = tostring(#Messages + 1)
    Messages[#Messages + 1] = {
      id = id,
      user = msg.From,
      content = msg.Data,
      timestamp = tostring(os.time())
    }

    Users[msg.From] = tostring((tonumber(Users[msg.From]) or 0) + 1)
    patchChat()

    return msg.reply({ Status = "OK", MessageId = id })
  end
)
```

Read latest patched messages:

```text
http://localhost:8734/YOUR_PROCESS_ID~process@1.0/compute/messages
```

## Frontend Client

```js
import { connect, createSigner } from "@permaweb/aoconnect";

export class ProcessClient {
  constructor({ nodeUrl, processId, jwk }) {
    this.nodeUrl = nodeUrl;
    this.processId = processId;
    this.ao = connect({
      MODE: "mainnet",
      URL: nodeUrl,
      signer: createSigner(jwk),
    });
  }

  async read(key) {
    const url = `${this.nodeUrl}/${this.processId}~process@1.0/compute/${key}`;
    const response = await fetch(url);
    const contentType = response.headers.get("content-type") || "";

    if (contentType.includes("application/json")) {
      return response.json();
    }

    return response.text();
  }

  async message(action, tags = [], data = "") {
    return this.ao.message({
      process: this.processId,
      tags: [{ name: "Action", value: action }, ...tags],
      data,
    });
  }
}
```

## External Data

HyperBEAM can resolve external HTTP data through relay-style flows. Keep this behind explicit trust decisions because node authority matters.

```lua
table.insert(ao.authorities, "<hyperbeam-node-authority>")

Send({
  target = ao.id,
  ["relay-path"] = "https://arweave.net/info",
  resolve = "~relay@1.0/call"
})
```

Use cases:

- Price feeds.
- Weather data.
- Sports results.
- API-backed games.
- IoT or sensor data.

## Web Serving

Some HyperBEAM workflows can serve HTML, CSS, or JavaScript from process data. Prefer publishing the content through the [`patch@1.0` device](/~patch@1.0/docs) and reading it from a `compute` endpoint. Raw state requests through `now` can be finicky across nodes and runtimes.

Minimal patched HTML:

```lua
WebPage = [[
<!doctype html>
<html>
  <head><title>AO Process Page</title></head>
  <body><h1>Hello from process state</h1></body>
</html>
]]

Send({
  device = "patch@1.0",
  web = WebPage
})
```

Read path:

```text
http://localhost:8734/YOUR_PROCESS_ID~process@1.0/compute/web
```

For multi-file experiences, patch separate keys such as `html`, `css`, and `js`, or store larger assets on Arweave and patch an index of asset IDs.
