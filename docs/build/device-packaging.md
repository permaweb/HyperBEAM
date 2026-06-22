# Device packaging

HyperBEAM packages every runtime device — kernel-baked or third-party
— into generated `_hb_device_*` BEAM modules. The Forge writes the
normal multi-module form as a deterministic archive of debug-info BEAM
modules. The packaging tooling lives in
`src/forge`, ships as a rebar3 plugin under one canonical namespace
(`device`), and is the only path for getting a device into a running
node.

## What the packager does

For each `dev_<name>` namespace under your source tree (root +
optional `dev_<name>_*` helpers):

1. Read every file in deterministic order and assemble an AO-Core
   message of `{filename, body}` pairs. The unsigned ID of that
   message is the device's content hash.
2. Decode that ID to raw bytes and encode it as lowercase, unpadded
   base32 — appearing in each generated module's atom name.
3. Compile the root and helpers with the Forge rename transform, producing
   their generated namespace while rewriting internal calls.
4. Compile each generated module with `debug_info` and pack the BEAMs
   under `ebin/` into a deterministic ZIP archive. Files under a
   package `priv/` directory are included under `priv/`; in source
   directories with multiple roots, `priv/dev_<root>/` is used for
   root-specific files.
5. Build two unsigned AO-Core messages — a `Device-Specification`
   (markdown derived from the root module's `%%% @doc` block) and an
   `Device-Implementation` (the BEAM archive, with `module-name`,
   `archive-format`, `implements-device`, `requires-otp-release`, and
   optional `requires-system-architecture` keys) — and sign them with the
   configured wallet.

At load time, `priv/` archive entries are materialized under the
node's implementation resource root:
`HB_DEVICE_IMPLEMENTATION_DIR/<implementation-id>/` (default:
`_build/device-implementations/<implementation-id>/`). The same root can
be set in node opts with `<<"device-implementation-dir">>`. Device
modules can locate their extracted files with
`hb_device_archive:implementation_dir(?MODULE)` and then use normal Erlang
file/NIF APIs, including `erlang:load_nif/2`.

The runtime never loads a raw `dev_*` module. Devices reach the
runtime exclusively as the generated `_hb_device_*` form, no matter
whether they came from the in-repo preloaded-store, an Arweave bundle,
or a peer's gateway.

## Provider commands

The plugin exposes one namespace, `device`. Every command shares the
same flag set:

| Flag | Purpose | Default |
|------|---------|---------|
| `--device-src dir[,dir2]` | Source roots to scan | `src/preloaded` in HyperBEAM, `src` elsewhere |
| `--output-dir dir` | Where to write artifacts | command-specific |
| `--key path` | Wallet keyfile used for signing | `hyperbeam-key.json` |
| `--requires-system-architecture` | Include host architecture requirement metadata | off |
| `-d, --devices p[,p2]` | Restrict to specific `dev_*` roots | (all) |
| `--record[=all\|errors]` | For `device test`, write recorder@1.0 test flights | off |

### `rebar3 device package`

Scans `--device-src`, packages each device, and writes
`_hb_device_<name>_<hash>.beam-archive.zip` to `--output-dir`
(default `_build/device-packages`).

```text
rebar3 device package
  └── _build/device-packages/_hb_device_message_1_0_<hash>.beam-archive.zip
  └── _build/device-packages/_hb_device_meta_1_0_<hash>.beam-archive.zip
  └── ...
```

### `rebar3 device verify`

Re-loads each generated archive and asserts:

* the module's atom is in `_hb_device_*` form;
* the archive loads cleanly with normal Erlang module loading;
* its exports are a superset of the root device's expected handlers;
* helper modules from the source set are *not* loadable under their
  original names.

### `rebar3 device preload`

Packages, signs, and indexes every discovered device into a
LMDB-backed `preloaded-store`. Output:

* `<output-dir>/<spec-id>` and `<output-dir>/<impl-id>` — signed
  spec and implementation messages, stored as TABM via
  `hb_cache:write/2`.
* `<output-dir>/<index-id>` — a signed flat resolver message whose
  fields map each human-readable device name to its spec ID.
  `name@1.0` is one of those names, so the runtime can read that
  first resolver entry directly before the name device itself is
  loaded.
* `<output-dir>/~meta@1.0/preloaded-devices-index` — a stable link
  to the signed flat resolver message. The runtime reads this link.

### `rebar3 device test`

Builds a fresh preloaded-store from HyperBEAM's built-in preloaded
devices plus `--device-src`, then runs the selected device root EUnit
suites against that store. The store contains the full local source
set so root tests can resolve device dependencies. In an external
device repo this is normally just:

```bash
rebar3 device test
```

Use `rebar3 device test --with-core` to include the normal core
`rebar3 eunit` modules in the same EUnit run. The `rebar3 eunit-all`
alias is shorthand for that full local check.

Use `--record` or `--record=errors` to write `~recorder@1.0` test flights
for failures, or `--record=all` to write one HTML archive for every test.

### `rebar3 device local`

Builds a fresh preloaded-store, points `HB_PRELOADED_STORE` at it,
then starts the normal Rebar shell.
Use this when you want a local node that can resolve your packaged
devices immediately:

```bash
rebar3 device local
```

The generated device template configures Rebar shell to start `hb`.
Custom runtime config works through the usual environment:

```bash
HB_CONFIG=custom.json rebar3 device local
```

### `rebar3 device publish`

Packages, signs, and uploads spec + implementation messages to
the configured ANS-104 bundler. Before signing, the provider builds the
same local preloaded-store used by `device test`, so the signing path
can resolve HyperBEAM's built-in devices without extra environment
variables. Returns each device's spec, impl, and signer IDs on stdout.

By default, publish uses HyperBEAM's configured `bundler-ans104`.
Use `--bundler` to override the endpoint. Forge posts ANS-104 items to
`/~bundler@1.0/tx`.

## Configuration the runtime cares about

| Key | Type | Role |
|-----|------|------|
| `<<"preloaded-store">>` | store map | LMDB preloaded device store. |
| `<<"loaded-device-store">>` | store map | Optional shared cache of name/spec-ID → loaded module atom. |
| `<<"trusted-device-signers">>` | `[Address \| SignerPolicy]` | Acceptable signer addresses for impl messages. A non-empty configured list enables remote implementation lookup; omitted or empty disables it. A signer policy object may include `<<"address">>`, `<<"valid-until-height">>` to cap remote GraphQL lookup by block height, and `<<"devices">>` to scope that signer to device refs or spec IDs. |
| `<<"trusted-devices">>` | `#{NameOrSpecID => ImplID}` | Operator-pinned implementation IDs trusted directly for the named device or spec ID. |
| `<<"admissible-devices">>` | `all` or `[Name]` | Per-execution allowlist (used by the Lua sandbox). |

In `config.json`, signer entries may be plain addresses or policy
objects:

```json
{
  "trusted-device-signers": [
    "PLAIN_SIGNER_ADDR",
    {
      "address": "SCOPED_SIGNER_ADDR",
      "valid-until-height": 1940492,
      "devices": ["arweave@2.9"]
    }
  ]
}
```

Plain signer entries have no lookup cutoff. `valid-until-height` only limits
remote implementation lookup; loaded implementations must still be signed
by an address in `trusted-device-signers`. `devices` scopes a signer
to matching device refs or resolved spec IDs; omitted means all devices.
Omit `trusted-device-signers` or set it to `[]` to disable remote lookup.

`HB_PRELOADED_STORE` points provider-driven test runs at the freshly
generated preloaded-store.

Operators control the bake via the source set their build runs
`rebar3 device preload` over.

### Forge preload bootstrap

The preloaded-store builder has one forge-private bootstrap step. To
compute source IDs and sign normal AO-Core messages, the builder
compiles and loads only the minimal build devices under their source
module names: `message@1.0`, `structured@1.0`, and the configured
commitment device (`httpsig@1.0` by default). Those modules are
reachable only through the build-local `forge-bootstrap` option. The
runtime never sets that option, and the seed modules are never written
to the final preloaded-store.

All final package identities are normal AO-Core unsigned message IDs
of the source-file message, and every final runtime implementation is
loaded from a signed archive message in generated `_hb_device_*` form.

## Project template

The Forge also ships a `rebar3 new` template for external device
authors. Install it into the user-level rebar3 template directory from
a HyperBEAM checkout:

```bash
./install-template --branch edge
```

Development checkouts can be used directly:

```bash
./install-template --local /path/to/hyperbeam
```

For reproducible scaffolding, use `--commit COMMIT_SHA`; for a
non-default remote, pair `--branch` or `--commit` with `--repo URL`.
If no source option is given, the installer uses the `edge` branch of
the default HyperBEAM repository.

Then scaffold a device project:

```bash
rebar3 new device name=my_device
```

The template writes `rebar.config`, `src/<name>.app.src`,
`src/dev_<name>.erl`, `README.md`, and `.gitignore`. Its
`rebar.config` keeps the `hb` dependency and Forge plugin on
the same HyperBEAM ref.
