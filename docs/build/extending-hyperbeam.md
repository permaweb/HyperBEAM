# Extending HyperBEAM

There is one production path for putting a new device into a
HyperBEAM node: write Erlang sources, package them with the Forge, and
let the runtime load the resulting `_hb_device_*` BEAM archive. That
single path covers both the devices baked into the HyperBEAM
repository and third-party devices that ship in their own repos.

This page is a quick orientation. The reference details live in the
[Device packaging](device-packaging.md) and
[External device repository](external-device-repository.md) guides.

## The shape of a device

A device is a namespace of Erlang modules:

* one root: `dev_<name>.erl` whose exports become the device's
  public API;
* optionally one or more helpers: `dev_<name>_*.erl` whose functions
  are loaded only under the generated `_hb_device_*__*` helper names.

The root may declare `-implements(<<"name@version">>).` (or a 43-char
specification ID); without it the Forge derives the human name from the
module — `dev_my_thing` → `my-thing@1.0`.

```erlang
%%% @doc One-paragraph description that becomes the device's
%%% Device-Specification body. Markdown is fine.
-module(dev_my_thing).
-export([info/1, do/3]).

info(_Opts) ->
    #{ exports => [<<"do">>] }.

do(Base, Req, Opts) ->
    %% Implement the device's behaviour by returning {ok, Result}
    %% or {error, Reason}.
    {ok, Base#{ <<"echo">> => maps:get(<<"input">>, Req, undefined) }}.
```

A top-level `%%% @doc` block becomes the spec body; alternatively
`-specification("path/to/spec.md").` points at an out-of-line file.

## In-repo devices

The HyperBEAM repository keeps every device source under
`src/preloaded`. The `compile` step runs the same Forge preload pipeline
over that directory and emits an LMDB `preloaded-store` plus the index
link the core default config consumes:

```bash
rebar3 compile          # builds core + forge, then preloads
rebar3 eunit            # runs core tests against the bake
```

`hb_device_load:reference/2` resolves every device — including
`message@1.0`, `httpsig@1.0`, etc. — through that store. There is no
privileged kernel path that would let `dev_message` be used directly
as a runtime device.

## External devices

Third-party devices live in their own rebar3 projects, depending on
HyperBEAM. Once the Forge template is installed in the user rebar3
config, create one with:

```bash
rebar3 new device name=my_device
```

Then iterate with:

```bash
rebar3 device package    # build _hb_device_*.beam-archive.zip
rebar3 device verify     # check archive invariants
rebar3 device test       # run dev_<root> EUnit against a fresh store
rebar3 eunit-all         # run core EUnit plus packaged-device EUnit
rebar3 device publish    # sign and upload to Arweave
```

See [External device repository](external-device-repository.md) for
the full template.

## Runtime shape

The build-time preloaded resolver message maps each name to a signed
specification ID, and the optional `loaded-device-store` runtime cache
maps resolved names and IDs to loaded generated module atoms. Operators
who want to change the baked-in set rebuild the in-repo source set or
point `<<"preloaded-store">>` at a store produced by an external build.

Codec devices use the same source and runtime naming as every other
device: source modules are `dev_<name>.erl`, and runtime atoms are
`_hb_device_<name>_<hash>`.
