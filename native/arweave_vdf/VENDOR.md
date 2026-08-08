# Vendored Arweave VDF NIF

The kernels are copied **byte-for-byte** from Arweave upstream:
`vdf_fused_arm.cpp`, `vdf_fused_x86.cpp`, `vdf_hiopt_arm.cpp`,
`sha256-armv8.S` and the `ar_nif.{c,h}` helpers are byte-identical, and they
are the files that decide what a VDF step hashes to.

Three files are edited, all to route verification through the fused kernel
rather than to change what it computes -- `vdf/vdf.cpp`, `vdf/vdf.h` and
`vdf/ar_vdf_nif.c`. Deviation 4 below describes the change and the
known-answer test that gates it. The `Makefile` is HyperBEAM-authored.

## Upstream pin

| What | Source | Pin |
| --- | --- | --- |
| C sources | [ArweaveTeam/arweave](https://github.com/ArweaveTeam/arweave) `apps/arweave/c_src/` | `50e47de` (release 2.9.6-alpha1) |

No RandomX involvement — the VDF is chained SHA-256 only.

## Layout

    native/arweave_vdf/
      ar_nif.{c,h}          verbatim from c_src/ (same file as arweave_randomx)
      vdf/                  verbatim from c_src/vdf/
        ar_vdf_nif.c
        vdf.{cpp,h}         portable OpenSSL implementation
        vdf_fused_x86.cpp   x86 SHA-NI  (built with -msha on Linux)
        vdf_fused_arm.cpp   armv8 crypto extensions
        vdf_hiopt_arm.cpp   armv8, calls sha256-armv8.S
        sha256-armv8.S      hand-written arm64 assembly (Darwin only)
      Makefile              HyperBEAM

Output lands in `priv/vdf_arweave.so`. The Erlang wrapper lives at
`src/core/lib/arweave/ar_vdf_nif.erl`.

`ar_vdf_nif.c` prints its selected backend on load, e.g. `VDF arch ARM macos`.

## Deviations from upstream

1. **No `openssl-sha-lite` submodule.** Same rationale as
   `native/arweave_randomx/VENDOR.md`: we link the platform `libcrypto`
   (located via `pkg-config --variable=prefix libcrypto`, override with
   `OPENSSL_DIR=`) rather than statically linking Arweave's trimmed
   OpenSSL fork. Only `SHA256_Init/Update/Final` are used. Verified
   against a real mainnet VDF step (see `scripts/nif_check.erl`).
   `-Wno-deprecated-declarations` silences OpenSSL 3's deprecation of the
   low-level SHA-256 API.

2. **Darwin links with `-bundle -flat_namespace -undefined suppress`**
   rather than upstream's `-shared`, matching `native/secp256k1/Makefile`.
   Linux still uses `-shared`.

3. **The Erlang wrapper loads from `code:priv_dir(hb)`** instead of
   `code:priv_dir(arweave)`. Nothing else changed — `ar_vdf_nif.erl` never
   included `ar.hrl`.

The `sha256-armv8.o` assembly rule and the per-object `-march=armv8-a+crypto`
/ `-msha` `CXXFLAGS` overrides are carried across from
`apps/arweave/c_src/Makefile` unchanged (including its "NOTE tabs here will
cause build fail" warning).

4. **The verification driver reaches the hardware SHA-256 kernels.**
   Upstream wires `vdf_fused_arm.cpp`, `vdf_hiopt_arm.cpp` and
   `vdf_fused_x86.cpp` to the three `vdf_sha2*_nif` *computation* entry
   points only; `vdf_parallel_sha_verify_with_reset` calls `_vdf_sha2` --
   the OpenSSL kernel -- directly. A node that only validates therefore
   never runs them, and pays about 6.4x for it (measured through the NIF
   on an Apple M5 Max at mainnet difficulty 1,111,546: one step 6.9 s
   OpenSSL against 1.08 s fused ARM). Three files change:

   - `vdf/vdf.h` declares the underscored, salt-advancing kernels
     (`_vdf_sha2`, `_vdf_sha2_fused_arm`, `_vdf_sha2_hiopt_arm`,
     `_vdf_sha2_fused_x86`), the `vdf_sha2_fn` typedef that
     `ar_vdf_nif.c` already had privately, and `vdf_set_verify_sha2`.
     The public `vdf_sha2*` wrappers are the wrong entry points here:
     they copy the salt to the stack, and the verification driver
     depends on it advancing in place.
   - `vdf/vdf.cpp` holds the installed kernel in a file-scope pointer
     that starts at `_vdf_sha2`, and the three verification call sites
     go through it. `vdf_set_verify_sha2` installs a candidate only
     after it has reproduced `_vdf_sha2` bit for bit -- digest and
     advanced salt, over the iteration counts whose loop bounds the
     kernels special-case -- on the machine that is about to run it.
     A candidate that fails is not installed and the node validates
     exactly as upstream does.
   - `vdf/ar_vdf_nif.c` hands `vdf_load`'s existing architecture probe's
     choice to `vdf_set_verify_sha2` and logs it, e.g.
     `VDF verify kernel fused ARM`. Hardware without the extensions
     takes the existing `VDF arch unknown` branch and logs
     `VDF verify kernel OpenSSL`. The probe is unchanged, so an x86
     build never calls into the ARM assembly and vice versa. Its
     private `vdf_sha2_fn` typedef is gone, since vdf.h now carries it
     and duplicate typedefs are a C11 feature in a C99 translation unit.

   Nothing about the reset path changes -- see below; the driver's reset
   branch is dead by design and stays dead. The computation entry points,
   and `ar_vdf.erl`'s `?VDF_BACKEND` selector for them, are untouched.

Every deviation is marked `/* VENDOR: */` at its site. Apart from those
marks and the lines they annotate, `diff -r` against
`apps/arweave/c_src/vdf` at `50e47de` reports no differences (build
artefacts aside); `vdf_fused_arm.cpp`, `vdf_hiopt_arm.cpp`,
`vdf_fused_x86.cpp` and `sha256-armv8.S` are still byte-for-byte
upstream.

## Note on the reset path

`vdf_parallel_sha_verify_with_reset_nif/10` takes `ResetSalt` and
`ResetSeed`, but the reset path is dead code upstream: every caller reaches
it through `ar_vdf:verify2/8` with `ResetStepNumber = 0`, giving
`ResetSalt = step_number_to_salt_number(-1) = -49`, which is out of range
for the step being verified. The entropy mix is done in Erlang
(`ar_nonce_limiter:maybe_add_entropy/4`) before the call. Both arguments
are still length-checked in C (`ResetSalt` must be exactly 32 bytes,
`ResetSeed` exactly 32) so they cannot be passed as `<<>>`.

## Upgrading in place

    UP=<arweave checkout>/apps/arweave/c_src
    cp $UP/ar_nif.c $UP/ar_nif.h  native/arweave_vdf/
    cp $UP/vdf/*                  native/arweave_vdf/vdf/

`vdf.h`, `vdf.cpp` and `ar_vdf_nif.c` carry deviation 4, so a plain copy
drops it. Reapply the `/* VENDOR: */` hunks in those three files, then
confirm `VDF verify kernel fused ARM` (or `fused x86`) still prints at NIF
load: `VDF verify kernel OpenSSL` after an upgrade means the self-test
rejected the kernel and the node has silently fallen back to the slow
path. The other four files under `vdf/` copy straight across.
