# hb_device rebar3 plugin

`hb_device` packages multi-module Erlang HyperBEAM devices into a single BEAM.
Device source stays ordinary Erlang:

```text
src/dev_example.erl
src/dev_example_codec.erl
src/dev_example_state.erl
```

The root module is `dev_example`; every module whose name starts with
`dev_example_` is treated as package-internal. The generated module exports only
the functions exported by `dev_example.erl`.

## Use in a Device Repo

Add the plugin to `rebar.config`:

```erlang
{plugins, [
    {hb_device,
        {git_subdir,
            "https://github.com/permaweb/HyperBEAM.git",
            {branch, "edge"},
            "apps/hb_device"}}
]}.

{hb_device, [
    {roots, [dev_example]},
    {out_dir, "_build/default/packaged-devices"}
]}.
```

For local development with a HyperBEAM checkout, use `rebar3_path_deps`:

```erlang
{plugins, [
    rebar3_path_deps,
    {hb_device, {path, "../hyperbeam/apps/hb_device"}}
]}.
```

## Commands

Package configured roots:

```sh
rebar3 hb_device package
```

Package one root:

```sh
rebar3 hb_device package --root dev_example
```

Verify packages by loading each generated BEAM:

```sh
rebar3 hb_device verify
```

Common options:

```text
--root, -r      Root module, or comma-separated root modules.
--src-dir, -s   Source directory. Defaults to src.
--out-dir, -o   Artifact directory. Defaults to _build/default/packaged-devices.
```

## Output

Artifacts are written under:

```text
_build/default/packaged-devices/src
_build/default/packaged-devices/ebin
```

Generated modules are named:

```text
_hb_device_<root_module>_<BASE32HASH>
```

The root name is included so stack traces stay readable. HyperBEAM's trace
formatter demangles that generated module name back to the root device name.
