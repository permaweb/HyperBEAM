# Install The Forge Template

Install from a HyperBEAM checkout:

Inspect workflow examples show request, configuration, or operator command shapes. Run them only after providing the stated local prerequisites; the browser runner does not execute these blocks.

```text
cd /tmp/hyperbeam-source
./install-template --local /tmp/hyperbeam-source
```

Install from upstream edge:

```text
./install-template --branch edge
```

Pin to a commit for repeatable scaffolding:

```text
./install-template --commit <commit-or-tag>
```

The installer writes a `rebar3 new device` template into your user template directory. New projects generated from it pin both the `hb` dependency and Forge plugin to the same HyperBEAM reference.

Create a project:

```text
mkdir -p /tmp/hb-device-docs-forge
cd /tmp/hb-device-docs-forge
rebar3 new device name=echo_lens
```

Continue with the [runbook](runbook.md).
