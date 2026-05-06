# HyperBEAM Device Example

This is a small external Erlang project that packages a multi-module device
without adding the device to the HyperBEAM source tree.

Its `rebar.config` uses `rebar3_path_deps` so the example can load the local
plugin app from `../../apps/hb_device`. A device repo outside this checkout can
instead use the `git_subdir` plugin dependency shown in the packaging guide.

The namespace is:

```text
src/dev_greeting.erl
src/dev_greeting_state.erl
src/dev_greeting_text.erl
```

Run:

```sh
rebar3 hb_device package
rebar3 hb_device verify
```

The packaged source and BEAM files are written to:

```text
_build/default/packaged-devices/src
_build/default/packaged-devices/ebin
```

Only the exports from `dev_greeting.erl` are exported from the generated module.
The helper modules are folded into private functions during packaging.
