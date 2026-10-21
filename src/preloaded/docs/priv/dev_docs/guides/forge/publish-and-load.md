# Publish And Load

Publishing signs and uploads the spec and implementation messages:

Inspect workflow examples show request, configuration, or operator command shapes. Run them only after providing the stated local prerequisites; the browser runner does not execute these blocks.

```text
rebar3 device publish --key wallet.json
```

The command prints IDs for the device specification and implementation. Operators can then load the implementation in one of three ways:

| Method | Use it when |
|---|---|
| Local preloaded store | You control the node build or are running `rebar3 device local`. |
| Direct pin in `trusted-devices` | You want one exact implementation ID for a name or spec. |
| Trusted signer | You want to accept implementations signed by a known address. |

A pinned implementation is the narrowest policy. A trusted signer is more flexible and should be limited to signers whose device updates you intend to run.

Inspect the node you operate for the current loading policy before publishing production devices.
