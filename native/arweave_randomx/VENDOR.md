# Vendored Arweave RandomX NIFs

The C/C++ sources under `randomx/` and the `ar_nif.{c,h}` helpers are
copied **byte-for-byte** from Arweave upstream. Only the `Makefile` in
this directory is HyperBEAM-authored.

## Upstream pins

| What | Source | Pin |
| --- | --- | --- |
| C sources | [ArweaveTeam/arweave](https://github.com/ArweaveTeam/arweave) `apps/arweave/c_src/` | `50e47de` (release 2.9.6-alpha1) |
| RandomX fork | [ArweaveTeam/RandomX](https://github.com/ArweaveTeam/RandomX) | `eef4dc86485473457ee42e39d88a78caaf4c9035` (`v1.0.3-219-geef4dc8`, "Expose RANDOMX_SCRATCHPAD_L3 as a function") |

The fork is a git submodule at `native/lib/RandomX`. It is **not**
interchangeable with a packaged `librandomx`: `rsp_fused_entropy_nif`
reaches into RandomX internals (`vm_compiled.hpp`,
`machine->getScratchpad()`, `machine->run()`, `randomx_blake2b`) that the
public `randomx.h` does not expose.

## Layout

    native/arweave_randomx/
      ar_nif.{c,h}                       verbatim from c_src/
      randomx/                           verbatim from c_src/randomx/
        ar_randomx_impl.h crc32.h
        feistel_msgsize_key_cipher.{cpp,h}
        randomx_long_with_entropy.{cpp,h}
        randomx_squared.{cpp,h}
        rx512/ar_rx512_nif.c
        rx4096/ar_rx4096_nif.c
        rxsquared/ar_rxsquared_nif.c
      Makefile                           HyperBEAM
    native/lib/RandomX                    submodule (ArweaveTeam fork)

Outputs land in `priv/`: `rx512_arweave.so`, `rx4096_arweave.so`,
`rxsquared_arweave.so`. The Erlang wrappers live in
`src/core/lib/arweave/ar_rx{512,4096,squared}_nif.erl`.

## Three builds of one fork

The variants differ only in the cache/dataset/scratchpad sizes compiled
into `librandomx.a`; the NIF C sources are shared. `native/lib/Makefile`
carries the CMake options, unchanged from Arweave's `apps/arweave/lib/Makefile`:

| Variant | Argon memory | Dataset base size | Scratchpad L1/L2/L3 |
| --- | --- | --- | --- |
| `512` | 262144 | 536870912 | default |
| `4096` | 524288 | 4294967296 | default |
| `squared` | 524288 | 2147483648 | 2097152 each |

Because those are `PRIVATE` `target_compile_definitions` on the `randomx`
CMake target they do not leak into our NIF translation units, which is why
the shared `randomx/*.o` objects can be linked into all three `.so` files
— exactly as upstream's `c_src/Makefile` does.

## Deviations from upstream

1. **No `openssl-sha-lite` submodule.** Upstream statically links its own
   trimmed OpenSSL fork (`../lib/openssl-sha-lite/libcrypto.a`) purely to
   supply `SHA256_Init/Update/Final`. We link the platform `libcrypto`
   instead (located via `pkg-config --variable=prefix libcrypto`, override
   with `OPENSSL_DIR=`). Rationale: `libcrypto` is already an unconditional
   runtime dependency of every OTP release — OTP's own `crypto` NIF links
   it — so this adds no deployment burden, and it removes a full OpenSSL
   `./config && make` from HyperBEAM's build. SHA-256 is SHA-256; the
   upstream test vectors confirm bit-identical output (see
   `scripts/nif_check.erl`). The low-level `SHA256_*` entry points are
   deprecated in OpenSSL 3, hence `-Wno-deprecated-declarations`.

2. **Darwin links with `-bundle -flat_namespace -undefined suppress`**
   rather than upstream's `-shared`, matching the existing HyperBEAM
   precedent in `native/secp256k1/Makefile` and rebar's `pc` plugin.
   Linux still uses `-shared`.

3. **`cmake --build . --parallel`** in `native/lib/Makefile` (upstream
   builds single-threaded). Build-time only; ~8 s for all three variants
   on an 18-core arm64 host.

4. **The Erlang wrappers drop `-include_lib("arweave/include/ar.hrl")`
   and its `?LOG_ERROR` calls**, and load from `code:priv_dir(hb)` instead
   of `code:priv_dir(arweave)`. The `?LOG_ERROR` lines only ever fired
   immediately before `erlang:nif_error(nif_not_loaded)`, i.e. when the
   NIF failed to load. Function names, arities and argument order are
   unchanged, so the modules remain drop-in for Arweave call sites.

One C header was modified: `randomx/ar_randomx_impl.h:50` makes `stateType`
`static`, with a `/* VENDOR: */` note at the site. Upstream declares it
non-static, so the three libraries -- three builds of one source tree, bundled
into a flat namespace -- interpose on a single symbol and the last to load owns
it, leaving every state built before that load unreadable. Losing the `static`
breaks every post-2.9 node. No other C, C++ or assembly source was modified;
`diff -r` reports no further differences (build artefacts aside).
`apps/arweave/c_src/randomx` and `apps/arweave/c_src/ar_nif.*` at `50e47de`
reports no differences (build artefacts aside).

## Upgrading in place

    UP=<arweave checkout>/apps/arweave/c_src
    cp $UP/ar_nif.c $UP/ar_nif.h            native/arweave_randomx/
    cp $UP/randomx/*.h $UP/randomx/*.cpp    native/arweave_randomx/randomx/
    cp $UP/randomx/rx512/*.c                native/arweave_randomx/randomx/rx512/
    cp $UP/randomx/rx4096/*.c               native/arweave_randomx/randomx/rx4096/
    cp $UP/randomx/rxsquared/*.c            native/arweave_randomx/randomx/rxsquared/

then repin the `native/lib/RandomX` submodule to whatever
`apps/arweave/lib/RandomX` points at, and re-check the CMake size options
in `native/lib/Makefile` against `apps/arweave/lib/Makefile`.
