# Device packaging

HyperBEAM packages every runtime device — kernel-baked or third-party
— into a deterministically named, debug-info BEAM module of the form
`_hb_device_<name>_<hash>`. The packaging tooling lives in `src/sdk`,
ships as a rebar3 plugin under one canonical namespace (`device`), and
is the only path for getting a device into a running node.

## What the packager does

For each `dev_<name>` namespace under your source tree (root +
optional `dev_<name>_*` helpers):

1. Read every file in deterministic order and assemble an AO-Core
   message of `{filename, body}` pairs. The unsigned ID of that
   message is the device's content hash.
2. Encode the hash as lowercase, unpadded base32 — appearing in the
   generated module's atom name.
3. Merge the root and helpers via the imported `igor` module, keeping
   only the root module's exports public.
4. Compile to a BEAM with `debug_info` so the artifact is auditable.
5. Build two unsigned AO-Core messages — a `Device-Specification`
   (markdown derived from the root module's `%%% @doc` block) and an
   `Device-Implementation` (the BEAM, with `module-name`,
   `implements-device`, and `requires-otp-release` keys) — and sign
   them with the configured wallet.

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
`_hb_device_<name>_<hash>.beam` to `--output-dir` (default
`_build/devices`).

```text
rebar3 device package
  └── _build/devices/_hb_device_message_1_0_<hash>.beam
  └── _build/devices/_hb_device_meta_1_0_<hash>.beam
  └── ...
```

### `rebar3 device verify`

Re-loads each generated BEAM and asserts:

* the module's atom is in `_hb_device_*` form;
* it loads cleanly via `code:load_binary/3`;
* its exports are a superset of the root device's expected handlers;
* helper modules from the source set are *not* loadable under their
  original names (i.e. `igor`'s merge succeeded).

### `rebar3 device preload`

Packages, signs, and indexes every discovered device into a
LMDB-backed `preloaded-store`. Output:

* `<output-dir>/<spec-id>` and `<output-dir>/<impl-id>` — signed
  spec and implementation messages, stored as TABM via
  `hb_cache:write/2`.
* `<output-dir>/<index-id>/<device-name>` — the `Device-Index`
  provider message that maps each human-readable device name to its
  spec ID. `name@1.0` is one of those names — it is what the runtime
  reads first to bootstrap.
* `_build/hb_preloaded_index.hrl` — a generated compile-time macro
  containing the index ID. The build hook recompiles `hb_opts` after
  writing it, so the default node config embeds the correct index
  without reading a separate metadata file at runtime.

### `rebar3 device test`

Builds a fresh preloaded-store from `--device-src`, then runs
`rebar3 eunit --module=<dev1>,<dev2>,...` over only the *root*
modules in that source set. Built-in HyperBEAM tests are not
re-executed.

### `rebar3 device publish`

Packages, signs, and uploads spec + implementation messages to
Arweave via `dev_arweave`. Returns each device's spec and impl IDs
on stdout.

## Configuration the runtime cares about

| Key | Type | Role |
|-----|------|------|
| `<<"preloaded-store">>` | store map | LMDB preloaded device store. |
| `<<"preloaded-devices-index">>` | binary | Committed ID of the `Device-Index`. Embedded into `hb_opts` from `_build/hb_preloaded_index.hrl` during compilation. |
| `<<"device-store">>` | store map | Volatile cache of name/spec-ID → loaded module atom. Falls back to `<<"store">>`. |
| `<<"trusted-device-signers">>` | `[Address]` or `all` | Acceptable signer addresses for impl messages. Defaults to the node wallet. |
| `<<"load-remote-devices">>` | bool | Whether unmatched devices may be fetched via the Arweave gateway. |
| `<<"admissible-devices">>` | `all` or `[Name]` | Per-execution allowlist (used by the Lua sandbox). |

`HB_PRELOADED_STORE` and `HB_PRELOADED_DEVICES_INDEX` override the
first two fields for provider-driven test runs, so the nested EUnit
node uses the freshly generated preloaded-store.

Operators control the bake via the source set their build runs
`rebar3 device preload` over.
