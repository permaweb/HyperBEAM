# AO Connect Mainnet

Use AO Connect to spawn, message, and read results from `process@1.0` processes on HyperBEAM mainnet.

Inspect workflow examples show request, configuration, or operator command shapes. Run them only after providing the stated local prerequisites; the browser runner does not execute these blocks.

Install:

```text
npm install @permaweb/aoconnect
```

The mainnet release note references `@permaweb/aoconnect@0.0.93` for the first mainnet-capable workflow. Use the current package version available in your project lockfile or registry.

## Mainnet Connect Flow

```js
import { connect, createSigner } from "@permaweb/aoconnect";
import fs from "node:fs";

const jwk = JSON.parse(fs.readFileSync("wallet.json", "utf-8"));

// Example only. Pick a current node from https://lunar.arweave.net
// or another marketplace/listing source.
const HYPERBEAM_URL = "http://localhost:8734";

// The node address belongs to the selected node URL.
// For a local node, this comes from:
// curl "http://localhost:8734/~meta@1.0/info/address"
const HYPERBEAM_NODE_ADDRESS = "your_local_node_address";

// Scheduler and authority addresses match the selected node.
const HYPERBEAM_SCHEDULER = HYPERBEAM_NODE_ADDRESS;

// List every node authority this process should trust.
const HYPERBEAM_AUTHORITIES = [HYPERBEAM_NODE_ADDRESS];

// Current AOS module from the mainnet release notes.
const AOS_MODULE = "ISShJH1ij-hPPt9St5UFFr_8Ys3Kj5cyg7zrMGt7H9s";

const ao = connect({
  MODE: "mainnet",
  URL: HYPERBEAM_URL,
  SCHEDULER: HYPERBEAM_SCHEDULER,
  signer: createSigner(jwk),
});
```

You can retrieve a node's authority address from its metadata endpoint:

```text
curl "http://localhost:8734/~meta@1.0/info/address"
```

Use the returned address for that node's scheduler and authority values. If the process should trust more than one node, add each node address to `HYPERBEAM_AUTHORITIES`.

## Spawn

```js
const process = await ao.spawn({
  authority: HYPERBEAM_AUTHORITIES,
  module: AOS_MODULE,
  data: "1984",
  tags: [{ name: "Example-Tag", value: "Example Value" }],
});
```

Use dash-separated tag names. Avoid relying on mixed-case tag names such as `ExampleTag`; HyperBEAM may normalize tag casing and older handlers can miss the tag.

## Message

```js
const message = await ao.message({
  process,
  data: "1984",
  tags: [{ name: "Example-Tag", value: "Example Value" }],
});
```

## Result

```js
const result = await ao.result({
  process,
  message,
});
```

Results can include:

- `Messages`: outbound messages produced by the process.
- `Spawns`: spawned process outputs.
- `Output`: console or evaluation output.
- `Error`: evaluation error data.

## Direct Request Path

Some HyperBEAM workflows use `request` for explicit `process@1.0` paths:

```js
const { request } = connect({
  MODE: "mainnet",
  URL: HYPERBEAM_URL,
  signer: createSigner(jwk),
});

const pushed = await request({
  path: `/${processId}~process@1.0/push/serialize~json@1.0`,
  method: "POST",
  target: processId,
  signingFormat: "ANS-104",
});
```

Use `ao.message` for normal app messages and reach for `request` when you need to call device-specific paths.

## AOS Mainnet CLI

Install:

```text
npm install -g https://get_ao.arweave.net
```

Connect to a selected node:

```text
aos <process-id> --url http://localhost:8734
```

Mainnet flags:

- `--url`: HyperBEAM node URL. If omitted, AOS defaults to a HyperBEAM mainnet node.
- `--scheduler`: HyperBEAM scheduler address. If omitted, the release-note default is `n_XZJhUnmldNFo4dhajoPZWhBXuJk-OcQr5JQ49c4Zo`.

## Browser Signer

For browser wallets, keep the same mainnet connection shape and pass the browser signer supported by your wallet integration:

```js
import { connect, createSigner } from "@permaweb/aoconnect";

const ao = connect({
  MODE: "mainnet",
  URL: "http://localhost:8734",
  signer: createSigner(globalThis.arweaveWallet),
});
```

Confirm the wallet adapter supports the signing format required by the target AO Connect version.
