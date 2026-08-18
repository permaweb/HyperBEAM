# Arweave VDF NIF sources

| Field | Value |
|---|---|
| Source | `ArweaveTeam/arweave`, `apps/arweave/c_src` |
| Commit | `50e47de6d054afefdee112fa124695eb8d0176fc` |
| License | GNU GPL v2; `src/core/lib/arweave/LICENSE.md` |

The build produces `priv/vdf_arweave.so`. The portable implementation uses
OpenSSL SHA-256; supported hardware may select the upstream ARM or x86 fused
SHA-256 kernel.

## Local adaptations

1. `Makefile` is HyperBEAM-owned and links the platform `libcrypto` instead of
   Arweave's `openssl-sha-lite` submodule.
2. Darwin uses HyperBEAM's normal NIF bundle flags; Linux uses a shared object.
3. The Erlang wrapper loads from `code:priv_dir(hb)`.
4. Verification, not only VDF computation, can use the fused hardware kernel.
   The local changes are confined to `vdf/vdf.h`, `vdf/vdf.cpp`, and
   `vdf/ar_vdf_nif.c` and are marked `VENDOR:`. A load-time known-answer check
   compares the selected implementation with the portable implementation,
   including the advanced salt. A mismatch leaves the portable kernel active.
5. The verification result shared by native worker threads is atomic, and
   `std::thread` objects are destroyed with the matching C++ `delete`. Upstream
   uses `volatile` for the shared result and `free` for an object created with
   `new`; both are undefined behavior in C++.
6. `vdf_backend_info_nif/0` exposes the physical computation and verification
   kernels selected at load time. The logical `hiopt` and `fused` entry points
   may alias the same kernel, so their Erlang function names are not sufficient
   production evidence of which implementation ran.

The following files should otherwise remain byte-identical to upstream:

```text
ar_nif.c
ar_nif.h
vdf/vdf_fused_arm.cpp
vdf/vdf_fused_x86.cpp
vdf/vdf_hiopt_arm.cpp
vdf/sha256-armv8.S
```

Arweave's NIF reset branch remains unchanged. Consensus entropy mixing is
performed by the Erlang nonce-limiter path before invoking the native verifier.

## Updating

1. Copy the pinned upstream VDF sources into a clean worktree.
2. Reapply the marked verification-kernel selector changes in the three files
   above.
3. Confirm all unmodified files with `git diff --no-index`.
4. Clean-build and run the VDF and block device suites. NIF load must report a
   fused kernel on supported hardware or the explicit OpenSSL fallback.
