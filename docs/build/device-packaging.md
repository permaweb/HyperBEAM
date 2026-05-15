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
3. Use the imported `igor` module to rename the root and helpers into
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
   `requires-system-architecture` keys) — and sign them with the
   configured wallet.

At load time, `priv/` archive entries are materialized under the
node's implementation resource root:
`HB_DEVICE_IMPLEMENTATION_DIR/<implementation-id>/` (default:
`_build/device-implementations/<implementation-id>/`). The same root can
be set in node opts with `<<"device-implementation-dir">>`. Device
modules can locate their extracted files with
`hb_ao_device:implementation_dir(?MODULE)` and then use normal Erlang
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
| `--device-src dir[,dir2]` | Source roots to scan | `src` |
| `--output-dir dir` | Where to write artifacts | command-specific |
| `--key path` | Wallet keyfile used for signing | `hyperbeam-key.json` |
| `--device-roots p[,p2]` | Restrict to specific `dev_*` roots | (all) |

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
* the archive loads cleanly via `code:atomic_load/1`;
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
  `name@1.0` is one of those names — it is what the runtime adds to
  `name-resolvers` first to bootstrap.
* `_build/hb_preloaded_index.hrl` — a generated compile-time macro
  containing the index ID. The build hook recompiles `hb_opts` after
  writing it, so the default node config embeds the correct index
  without reading a separate metadata file at runtime.

### `rebar3 device test`

Builds a fresh preloaded-store from HyperBEAM's built-in preloaded
devices plus `--device-src`, then runs the selected device root EUnit
suites against that store. The store contains the full local source
set so root tests can resolve device dependencies. In an external
device repo this is normally just:

```bash
rebar3 device test
```

### `rebar3 device publish`

Packages, signs, and uploads spec + implementation messages to
Arweave via `dev_arweave`. Before signing, the provider builds the
same local preloaded-store used by `device test`, so the signing path
can resolve HyperBEAM's built-in devices without extra environment
variables. Returns each device's spec and impl IDs on stdout.

## Configuration the runtime cares about

| Key | Type | Role |
|-----|------|------|
| `<<"preloaded-store">>` | store map | LMDB preloaded device store. |
| `<<"preloaded-devices-index">>` | binary | Committed ID of the flat preloaded resolver message. Embedded into `hb_opts` from `_build/hb_preloaded_index.hrl` during compilation. |
| `<<"device-store">>` | store map | Volatile cache of name/spec-ID → loaded module atom. |
| `<<"trusted-device-signers">>` | `[Address]` | Acceptable signer addresses for impl messages. Empty/default uses the node wallet. |
| `<<"trusted-devices">>` | `[ImplID]` | Implementation message IDs trusted directly, bypassing signer matching for those IDs only. |
| `<<"load-remote-devices">>` | bool | Whether unmatched devices may be fetched via the Arweave gateway. |
| `<<"admissible-devices">>` | `all` or `[Name]` | Per-execution allowlist (used by the Lua sandbox). |

`HB_PRELOADED_STORE` and `HB_PRELOADED_DEVICES_INDEX` override the
first two fields for provider-driven test runs, so the nested EUnit
node uses the freshly generated preloaded-store.

Operators control the bake via the source set their build runs
`rebar3 device preload` over.

## Project template

The Forge also ships a `rebar3 new` template for external device
authors. Install it into the user-level rebar3 template directory from
a HyperBEAM checkout:

```bash
./src/forge/plugin/install-template --branch edge
```

Development checkouts can be used directly:

```bash
./src/forge/plugin/install-template --local /path/to/hyperbeam
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
