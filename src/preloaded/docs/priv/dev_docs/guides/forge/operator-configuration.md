# Operator Configuration

Device loading, cache writes, auth, payments, routes, and remote fetches are operator decisions. Review your own node's configuration and public operator info before changing behavior; a public docs node may not expose every operator policy value as a runnable example.

Common device-related keys:

| Key | Used by |
|---|---|
| `preloaded-devices-index` | Core device name resolution. |
| `load-remote-devices` | Remote Forge-published device loading. |
| `trusted-device-signers` | Signer-based device trust. |
| `trusted-devices` | Direct implementation pins. |
| `cache_writers` | `~cache@1.0` mutating writes, links, and groups. |
| `routes` | `~router@1.0`, `~relay@1.0`, and Arweave gateway behavior. |
| `p4_pricing-device`, `p4_ledger-device` | P4 payment policy. |
| `simple_pay_ledger`, `simple_pay_price` | Simple-pay balances and default pricing. |
| `metering-rates` | Metering resource prices. |

Operator-facing devices are documented because many HyperBEAM users run their own node. Test changes on a disposable node first when a command can mutate cache, policy, secrets, payment state, or trusted-code settings.
