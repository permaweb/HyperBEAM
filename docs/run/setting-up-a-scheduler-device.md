# Setting Up a Scheduler Device

This guide is for operators who are already running a production HyperBEAM node and want to register it as an AO scheduler.

## Prerequisites

If you started your node using the default device list in [hb_opts.erl](../../src/hb_opts.erl), both `~location@1.0` and `~scheduler@1.0` are already onboarded — no additional configuration is required to enable them.

## Announcing Your Scheduler Location

To register your node as a scheduler on the AO network, you need to call the `~location@1.0` device endpoint **from the node host itself** (i.e. run the command on the same machine running HyperBEAM). This ensures the request originates from the correct origin.

Run the following curl command on your node host (Linux/Mac):

```bash
curl -sS -H 'Accept: application/json' http://127.0.0.1:8734/~location@1.0/node
```

### What Success Looks Like

A successful call returns an **HTTP 200 OK** response from `~location@1.0` in the node logs and will give you some details in your local console. This publishes a transaction to the network announcing your scheduler's location.

## Emitted Transaction Tags

The published transaction should include tags similar to the following:

```
Data-Protocol: ao
Variant: ao.N.1
Type: Scheduler-Location
Url: https://your-node-domain.example.com
nonce: <nonce-value>
```

> **Note:** The `Url` field should point to your node's publicly accessible address (for example, `https://push.forward.computer`). The `nonce` value is generated automatically.
