# Packaging HyperBEAM Devices

HyperBEAM devices can be written as normal multi-module Erlang namespaces and
packaged into one generated BEAM with the `hb_device` rebar3 plugin.

## Namespace Rule

A package has one root module and one or more helper modules:

```text
src/dev_example.erl
src/dev_example_codec.erl
src/dev_example_state.erl
```

`dev_example.erl` is the public root. Modules starting with `dev_example_` are
internal to the package. During packaging, Igor merges the namespace into a
generated module named like:

```text
_hb_device_dev_example_BASE32HASH
```

Only exports from the root module remain exported. Helper exports become private
generated functions, so callers cannot accidentally reach across device
boundaries.

## External Device Project

Add the plugin to an Erlang device repo:

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

Then run:

```sh
rebar3 hb_device package
rebar3 hb_device verify
```

`package` writes generated source and BEAM files. `verify` also loads each
generated BEAM to prove the artifact can be loaded by the Erlang code server.

## HyperBEAM Repo Alias

Inside the HyperBEAM repo, this is also available as:

```sh
rebar3 package-devices
```

That alias packages all multi-module `dev_*` namespaces in `src`.

## Example

See `examples/hyperbeam-device` for a complete external device project. It
contains a root module, two helper modules, plugin configuration, and a short
README with the packaging commands.

## Current Limits

The packager targets Erlang source. It expects package-internal calls to be
static module calls, such as:

```erlang
dev_example_codec:encode(Body)
```

Dynamic dispatch to internal modules is not a supported device boundary. Keep
dynamic resolution at AO-Core boundaries or behind ordinary local functions in
the root namespace.
