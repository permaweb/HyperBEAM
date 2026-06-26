# Package, Verify, And Test

From the device project root:

Inspect workflow examples show request, configuration, or operator command shapes. Run them only after providing the stated local prerequisites; the browser runner does not execute these blocks.

```text
rebar3 device package
rebar3 device verify
rebar3 device test
```

`package` writes a deterministic archive under `_build/device-packages/`.

`verify` reloads the archive and checks that generated `_hb_device_*` modules export the expected handlers while raw helper names are not loadable.

`test` builds a temporary preloaded store containing HyperBEAM core devices plus your device, then runs EUnit against that store.

Use recorder output for failures:

```text
rebar3 device test --record=errors
```

Use a full local check when core tests matter too:

```text
rebar3 device test --with-core
```
