# Legacynet Appendix

This appendix is for Legacynet AO patterns that only matter when supporting existing Legacynet processes.

Inspect workflow examples show request, configuration, or operator command shapes. Run them only after providing the stated local prerequisites; the browser runner does not execute these blocks.

Use this only when supporting existing Legacynet processes.

## When To Use Legacynet Material

- You are connecting to an existing Legacynet process.
- A process you need to interact with requires old MU/CU/SU endpoints.
- You need WeaveDrive or dry-run examples.
- You are migrating a process to HyperBEAM and need to understand old behavior.

For new work, use HyperBEAM mainnet.

## AOS Legacy Flag

Mainnet AOS defaults to HyperBEAM. To create or connect through the Legacynet path, use `--legacy`:

```text
aos <Legacynet-process-id> --wallet ./wallet.json --legacy
```

If connecting directly to an existing Legacynet process by ID, AOS may detect the network automatically. Keep `--legacy` in examples where the intent must be explicit.

## Old AO Connect Unit Configuration

Legacynet AO Connect examples configured Messenger Unit, Compute Unit, and gateway URLs directly:

```js
import { connect } from "@permaweb/aoconnect";

const { result, results, message, spawn, monitor, unmonitor, dryrun } =
  connect({
    MU_URL: "https://mu.ao-testnet.xyz",
    CU_URL: "https://cu.ao-testnet.xyz",
    GATEWAY_URL: "https://arweave.net",
  });
```

## Legacy Spawn Constants

Legacynet spawn uses a scheduler and Legacynet MU authority:

```text
Scheduler:
_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA

Legacynet MU authority:
fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY
```

These constants are Legacynet-only. For mainnet, use the selected HyperBEAM node authority and the current scheduler flow.

## Dry Run

Dry-run reads belong to the old model. They were used to simulate a message and read a result without saving memory:

```js
import { dryrun } from "@permaweb/aoconnect";

const result = await dryrun({
  process: "PROCESS_ID",
  tags: [{ name: "Action", value: "Balance" }],
});
```

For HyperBEAM, expose state via the [`patch@1.0` device](/~patch@1.0/docs) and read it through:

```text
GET /<process-id>~process@1.0/compute/<key>
```

## WeaveDrive

The cookbook WeaveDrive snack was written for AO Legacynet. Use it only for existing Legacynet processes. On HyperBEAM, use the `bundler@1.0`, `copycat@1.0`, and `query@1.0` devices to store, index, and query Arweave data.

Setup:

```text
aos test-weavedrive \
  --tag-name Extension --tag-value WeaveDrive \
  --tag-name Attestor --tag-value <attestor-address> \
  --tag-name Availability-Type --tag-value Assignments
```

## Legacynet Release Notes

For historical AOS changes, see the [AOS GitHub release notes](https://github.com/permaweb/aos/releases).
