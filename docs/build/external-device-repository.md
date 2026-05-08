# Building a third-party device repository

Devices live in their own repos as ordinary Erlang projects. The
HyperBEAM SDK ships as a rebar3 plugin under the `device` namespace,
so the workflow looks like a normal rebar3 project plus a few extra
commands.

## Repository layout

```
my-device/
├── rebar.config
└── src/
    ├── my_device.app.src
    ├── dev_my_device.erl              %% root
    └── dev_my_device_helpers.erl      %% optional helpers
```

The packager treats `dev_<name>.erl` as the root, with any
`dev_<name>_*.erl` siblings merged in as helpers. Only the root's
exports become public; helpers are private after the merge.

## `rebar.config`

```erlang
{deps, [
    %% Pull HyperBEAM in so the kernel modules and SDK are on the
    %% code path. Pin to a specific tag for reproducible builds.
    {hb, {git, "https://github.com/permaweb/hyperbeam.git",
                {tag, "v0.0.1"}}}
]}.

{plugins, [
    %% The device provider lives inside HyperBEAM. Rebar requires us
    %% to declare it as a plugin separately from the dependency,
    %% which is purely a Rebar constraint — keep the ref identical
    %% to the dep above.
    {rebar3_device, {git, "https://github.com/permaweb/hyperbeam.git",
                          {tag, "v0.0.1"}}}
]}.
```

`rebar3` will fetch HyperBEAM, place its kernel modules on the path
(`hb_ao`, `hb_message`, `hb_cache`, …), and load `rebar3_device` as a
plugin. The SDK can then run against your `src/` source set without
operating on the HyperBEAM source you depend on.

## Day-to-day commands

### Iterate on a device

```bash
rebar3 device package --device-roots dev_my_device
rebar3 device verify  --device-roots dev_my_device
```

`package` writes the generated BEAM to `_build/devices/`; `verify`
re-loads it and checks the merge invariants.

### Run your tests against a fresh preloaded-store

```bash
rebar3 device test --device-roots dev_my_device
```

`device test` packages your devices, signs spec and impl messages
into a temporary `preloaded-store`, and runs
`rebar3 eunit --module=dev_my_device` against it. The kernel's
built-in EUnit cases are skipped — the SDK only tests *your* root
modules.

### Publish to Arweave

```bash
rebar3 device publish --key wallet.json --device-roots dev_my_device
```

Each device prints its `spec_id` and `impl_id` on stdout. Operators
who trust your wallet can resolve `dev_my_device` either by name (if
you also publish a `name@1.0` provider message that maps the human
name to the spec ID) or by quoting the spec ID directly.

## Iterating on HyperBEAM kernel changes

Because `hb` is a regular dependency, your editor and `rebar3 shell`
can step into kernel sources via `_build/default/lib/hb/src/kernel`.
When you need a kernel patch your device depends on, ship it as a
separate PR against HyperBEAM and bump the `tag` in your
`rebar.config` to pick it up.
