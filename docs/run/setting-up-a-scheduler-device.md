# Setting Up a Scheduler Device

> **Note:** When your node launches, it will normally post (announce) its scheduler location automatically. This guide is useful if you need to re-announce the location or manually update the published location data.

This guide is for operators who are already running a production HyperBEAM node and want to register it as an AO scheduler.

## Prerequisites

If you started your node using the default device list in [hb_opts.erl](../../src/hb_opts.erl), both `~location@1.0` and `~scheduler@1.0` are already onboarded — no additional configuration is required.

You must set `host` in your `config.flat` so the `~location@1.0` device announces the correct public URL for your node.

Example:

```erlang
host = "your-node-domain.example.com".
```

## Announcing Your Scheduler Location

To register your node as a scheduler on the AO network, you need to call the `~location@1.0` device endpoint **from the node host itself** (i.e. run the command on the same machine running HyperBEAM).

Run the following curl command on your node host (Linux/Mac):

```bash
curl -sS -v http://127.0.0.1:8734/~location@1.0/node
```

### What Success Looks Like

A successful call returns an **HTTP 200 OK** response from `~location@1.0` in the node logs and will give you some details in your local console. This publishes a transaction to the network announcing your scheduler location.

## Emitted Transaction Tags

The published transaction should include tags similar to the following:

```
data-Protocol: ao
variant: ao.N.1
type: location
url: https://your-node-domain.example.com
nonce: <nonce-value>
time-to-live:  TTL
codec-device: Codec
```

> **Note:** The `url` field should point to your node's publicly accessible address (for example, `https://push.forward.computer`). The `nonce` value is generated automatically.
