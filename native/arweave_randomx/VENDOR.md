# Arweave RandomX NIF sources

| Field | Value |
|---|---|
| Arweave source | `ArweaveTeam/arweave`, `apps/arweave/c_src` |
| Arweave commit | `50e47de6d054afefdee112fa124695eb8d0176fc` |
| RandomX fork | `ArweaveTeam/RandomX` |
| RandomX commit | `eef4dc86485473457ee42e39d88a78caaf4c9035` |
| Arweave license | GNU GPL v2; `src/core/lib/arweave/LICENSE.md` |

`native/lib/RandomX` is the exact Arweave RandomX fork as a git submodule. A
system RandomX package is not interchangeable: the replica-2.9 code uses fork
internals that are not part of the public RandomX API.

The three build variants reproduce Arweave's 512 MiB, 4 GiB, and replica-2.9
cache/dataset definitions. They produce:

```text
priv/rx512_arweave.so
priv/rx4096_arweave.so
priv/rxsquared_arweave.so
```

## Local adaptations

1. `Makefile` is HyperBEAM-owned and links the platform `libcrypto` instead of
   Arweave's `openssl-sha-lite` submodule.
2. Darwin uses the bundle/link flags already used by HyperBEAM NIFs; Linux uses
   a shared object.
3. `native/lib/Makefile` retains Arweave's three RandomX CMake configurations
   and builds them in parallel.
4. `randomx/ar_randomx_impl.h` makes `stateType` file-local. Without this, the
   three flat-namespace bundles can interpose on one global symbol and make
   states created by an earlier-loaded variant unreadable. The change is marked
   `VENDOR:` in the header.
5. Erlang wrappers load from `code:priv_dir(hb)` and omit Arweave logging
   macros; NIF names, arities, and argument order are unchanged.

All other files below `randomx/` and `ar_nif.{c,h}` should match the pinned
Arweave checkout. Confirm this with `git diff --no-index`, excluding build
outputs, whenever the pin changes.

## Updating

1. Copy the pinned upstream `ar_nif.{c,h}` and `randomx/` sources into a clean
   worktree.
2. Reapply only the marked `stateType` change.
3. Repin `native/lib/RandomX` to the gitlink used by the same Arweave commit.
4. Compare the CMake size options in `native/lib/Makefile` with upstream.
5. Clean-build all three NIFs and run the Arweave device test vectors.
