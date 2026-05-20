# Building a third-party device repository

Devices live in their own repos as ordinary Erlang projects. The
HyperBEAM Forge ships as a rebar3 plugin under the `device` namespace,
so the workflow looks like a normal rebar3 project plus a few extra
commands.

## Repository layout

After installing the Forge template, a new project can be created with:

```bash
rebar3 new device name=my_device
```

The generated project has this shape:

```
my-device/
├── .gitignore
├── README.md
├── rebar.config
└── src/
    ├── my_device.app.src
    ├── dev_my_device.erl              %% root
    └── dev_my_device_helpers.erl      %% optional helpers
```

The packager treats `dev_<name>.erl` as the root, with any
`dev_<name>_*.erl` siblings renamed into the same generated namespace
as helpers. The root module's exports remain the public device API;
helpers are loaded only by their generated names.

## Installing the template

From a HyperBEAM checkout, install the Forge template into your user
rebar3 template directory:

```bash
./install-template --branch edge
```

Use a local checkout while developing HyperBEAM itself:

```bash
./install-template --local /path/to/hyperbeam
```

`--local` pins the checkout's committed `HEAD`. Commit or switch to the
desired local revision, then rerun the installer when you want the
template to move.

Pin to an exact commit for reproducible local scaffolding:

```bash
./install-template --commit COMMIT_SHA
```

The script writes only template files under
`~/.config/rebar3/templates` by default. Pass `--template-dir PATH` to
install them elsewhere. The generated project receives the selected
HyperBEAM dependency and plugin terms in its own `rebar.config`.

## `rebar.config`

The generated `rebar.config` pins the `hb` dependency and the Forge
plugin to the same HyperBEAM ref. Rebar requires the
dependency and plugin to be declared separately; keeping their refs
identical ensures the provider and kernel APIs match.

```erlang
{deps, [
    %% Pull HyperBEAM in so the core modules and Forge are on the
    %% code path. Pin to a specific branch, tag, or commit for
    %% reproducible builds.
    {hb, {git, "https://github.com/permaweb/hyperbeam.git",
                {branch, "edge"}}}
]}.

{plugins, [
    {plugin,
        {git_subdir, "https://github.com/permaweb/hyperbeam.git",
            {branch, "edge"},
            "src/forge"}}
]}.
```

`rebar3` will fetch HyperBEAM, place its core modules on the path
(`hb_ao`, `hb_message`, `hb_cache`, …), and load the Forge plugin. The
Forge uses that `hb` dependency as the source of the
default preloaded device library, then adds your project devices to
the same generated preloaded-store.

If your device uses HyperBEAM macros such as `?event`, include the
core header from your device module and list `hb` in your app:

```erlang
-include_lib("hb/include/hb.hrl").
```

## Day-to-day commands

### Iterate on a device

```bash
rebar3 device package
rebar3 device verify
```

`package` writes the generated BEAM archive to `_build/device-packages/`;
`verify` re-loads it and checks the archive invariants.

### Run your tests against a fresh preloaded-store

```bash
rebar3 device test
```

`device test` packages HyperBEAM's built-in preloaded devices and the
configured project source set into one temporary `preloaded-store`,
then runs the project device root EUnit suites against it. The store
contains the full local source set so those tests can resolve
dependencies.

### Start a local node with your device

```bash
rebar3 device local
```

`device local` builds the same kind of preloaded-store, sets the
preloaded-store environment for the shell, and then starts the normal
HyperBEAM node locally. The generated template configures Rebar shell
to start `hb`, so custom app configuration can use the normal
environment form:

```bash
HB_CONFIG=custom.json rebar3 device local
```

### Publish to Arweave

```bash
rebar3 device publish --key wallet.json
```

Each device prints its `spec_id` and `impl_id` on stdout. Operators
who trust your wallet can resolve `dev_my_device` either by name (if
you also publish a `name@1.0` provider message that maps the human
name to the spec ID) or by quoting the spec ID directly.

## Iterating on HyperBEAM core changes

Because `hb` is a regular dependency, your editor and `rebar3 shell`
can step into core sources via `_build/default/lib/hb/src/core`.
When you need a core patch your device depends on, ship it as a
separate PR against HyperBEAM and bump the `tag` in your
`rebar.config` to pick it up.
