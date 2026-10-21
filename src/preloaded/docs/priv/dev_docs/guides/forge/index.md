# Device Forge

The Device Forge turns Erlang `dev_*` modules into signed, loadable HyperBEAM devices. Start with the runbook:

- [Device Forge Runbook](runbook.md)

Focused references:

- [Create a device](create-a-device.md)
- [Package, verify, and test](test-package-verify.md)
- [Run locally](run-local.md)
- [Publish and load](publish-and-load.md)
- [Trusted signers and pins](trusted-signers-and-pins.md)
- [Operator configuration](operator-configuration.md)

Device loading is an operator policy decision. The relevant policy keys are `preloaded-devices-index`, `load-remote-devices`, `trusted-device-signers`, and `trusted-devices`; inspect them on the node you operate rather than assuming a public docs node exposes live policy values.
