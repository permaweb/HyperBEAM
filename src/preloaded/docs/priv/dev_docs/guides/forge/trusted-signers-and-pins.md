# Trusted Signers And Pins

Remote device loading is controlled by operator policy.

| Configuration | Meaning |
|---|---|
| `load-remote-devices` | Allows the node to fetch device specs/implementations that are not already local. |
| `trusted-device-signers` | Accepts implementation messages signed by listed addresses. |
| `trusted-devices` | Pins a device name or spec ID to a specific implementation ID. |

Read the current trust policy from the node you operate before enabling remote loading or signer trust.

Use pins when the implementation must not change without an operator edit. Use signer trust when the signer should be able to publish upgrades. Keep remote loading disabled when a node should only run its preloaded device set.
