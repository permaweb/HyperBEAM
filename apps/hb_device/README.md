# hb_device rebar3 plugin

`hb_device` packages Erlang HyperBEAM device namespaces into a single BEAM.
Device source stays ordinary Erlang:

```text
src/dev_example.erl
src/dev_example_codec.erl
src/dev_example_state.erl
```

The root module is `dev_example`; every module whose name starts with
`dev_example_` is treated as package-internal. The generated module exports
only the functions exported by `dev_example.erl`.

## Use in a Device Repo

Add HyperBEAM as a dependency for editor support and runtime APIs, and add the
plugin from the same ref:

```erlang
{deps, [
    {hb,
        {git,
            "https://github.com/permaweb/HyperBEAM.git",
            {branch, "edge"}}}
]}.

{plugins, [
    {hb_device,
        {git_subdir,
            "https://github.com/permaweb/HyperBEAM.git",
            {branch, "edge"},
            "apps/hb_device"}}
]}.

{hb_device, [
    {roots, all},
    {out_dir, "_build/default/packaged-devices"}
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

Build a local preload store:

```sh
rebar3 hb_device preload
```

Common options:

```text
--root, -r      Root module, or comma-separated root modules.
--src-dir, -s   Source directory. Defaults to src.
--out-dir, -o   Artifact directory. Defaults to _build/default/packaged-devices.
--store-dir     Filesystem preload store. Defaults to _build/default/preloaded-device-store.
--metadata-file Preload metadata term file. Defaults to _build/default/preloaded-device-metadata.eterm.
--key, -k       Wallet keyfile used to sign preload messages. Defaults to hyperbeam-key.json.
```

## Output

Artifacts are written under:

```text
_build/default/packaged-devices/src
_build/default/packaged-devices/ebin
_build/default/preloaded-device-store
_build/default/preloaded-device-metadata.eterm
```

Generated modules are named:

```text
_hb_device_<root_module>_<BASE32HASH>
```

The root name is included so stack traces stay readable. HyperBEAM's trace
formatter demangles that generated module name back to the root device name.
