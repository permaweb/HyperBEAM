# LapEE guest — hardening + size audit (2026-04-19)

Audit of the guest the paper-aligned `~tpm2@2.0a` flow actually boots
(Buildroot-built `vmlinuz-lapee` + `initramfs-hb.cpio.gz`).

## Current footprint

| artefact | size | note |
|---|---|---|
| `build-alpine/vmlinuz-lapee` | 12 MB | Linux 6.6.51 x86_64 bzImage |
| `work/initramfs-hb.cpio.gz`  | 82 MB gz | cpio+gz wire format |
| — extracted                  | 197 MB | runtime footprint |
| — `usr/lib/hyperbeam`        | 180 MB | HB release (biggest block) |
| — `erts-15.2.7.7/bin/beam.smp` | 46 MB | unstripped BEAM |
| — `hyperbuddy@1.0/bundle.js` | 15 MB × 3 | shipped at `bin/priv/`, `lib/hb-0.0.1/priv/`, and `lib/hb-0.0.1/src/` |
| — `dev_snp_nif.so`           | 10 MB × 2 | SNP NIF we don't use on TPM hosts, shipped twice |

## Kernel hardening — status

Read from `work/linux.config.built`.

### In place (good)

`CONFIG_RANDOMIZE_BASE`, `CONFIG_RANDOMIZE_MEMORY`, `CONFIG_RANDOMIZE_KSTACK_OFFSET`,
`CONFIG_PAGE_TABLE_ISOLATION`, `CONFIG_RETPOLINE`,
`CONFIG_STACKPROTECTOR_STRONG`, `CONFIG_STRICT_KERNEL_RWX`,
`CONFIG_STRICT_MODULE_RWX`, `CONFIG_STRICT_DEVMEM`,
`CONFIG_SECURITY_LOCKDOWN_LSM`, `CONFIG_SECURITY_LOCKDOWN_LSM_EARLY`,
`CONFIG_LOCK_DOWN_KERNEL_FORCE_INTEGRITY`,
`CONFIG_MODULE_SIG`, `CONFIG_MODULE_SIG_ALL`,
`CONFIG_TCG_TPM`, `CONFIG_TCG_TIS`, `CONFIG_TCG_CRB`,
`CONFIG_IMA`, `CONFIG_IMA_MEASURE_ASYMMETRIC_KEYS`,
`CONFIG_VMAP_STACK`, `CONFIG_THREAD_INFO_IN_TASK`,
`CONFIG_DM_VERITY`, `CONFIG_EFI_STUB`, `CONFIG_EFI_MIXED`.
Attack-surface disabled: USB_HID, BT, SOUND, MEDIA_SUPPORT.

### Missing or weaker than the LapEE posture wants

| flag | current | target | reason |
|---|---|---|---|
| `FORTIFY_SOURCE` | off | **y** | compiler-level memcpy/strcpy bounds check |
| `HARDENED_USERCOPY` | off | **y** | kernel ↔ userspace copies size/slab-validated |
| `INIT_ON_ALLOC_DEFAULT_ON` | off | **y** | zero-on-alloc to kill use-before-init infoleaks |
| `INIT_ON_FREE_DEFAULT_ON` | off | **y** | zero-on-free to kill use-after-free infoleaks |
| `SLAB_FREELIST_HARDENED` | off | **y** | mitigates freelist-overwrite heap exploits |
| `SLAB_FREELIST_RANDOM` | off | **y** | randomises slab alloc order |
| `IO_STRICT_DEVMEM` | off | **y** | no unprivileged `/dev/mem` to device I/O |
| `MODULE_SIG_FORCE` | off | **y** | signing is present but not enforced → unsigned modules can still load |
| `KEXEC_SIG` | absent | **y** | signed-only kexec (prevents bootkit re-injection) |
| `SECURITY_DMESG_RESTRICT` | off | **y** | hide kernel addresses from unpriv users |
| `IMA_APPRAISE` + `INTEGRITY_TRUSTED_KEYRING` | off | **y** | IMA currently only *measures*; appraisal would actually block unsigned binaries |
| `LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY` | off (integrity only) | **y** | also denies kernel-memory disclosure paths (kprobes, tracefs, etc.) |
| `DEBUG_KERNEL` | y | **n** | production kernels should be non-debug |

All are one-line additions to
`buildroot-external/board/lapee/linux-m1-fragment.config`, producing
a stricter kernel on the next `make buildroot`. We leave
`LOCK_DOWN_KERNEL_FORCE_INTEGRITY` as the default until we've
validated HB itself still boots cleanly under
`LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY` (which would also block
profiling, perf, kprobes — some of which HB uses via `runtime_tools`).

### Configurable from the LapEE posture spec

These are properties of the _boot chain_ the kernel is embedded in,
not the kernel config itself:

- **UKI + Secure Boot (Microsoft UEFI CA / custom PK/KEK).** The
  scripts in `scripts/uki.sh` + `scripts/secureboot-keys.sh' exist
  but are not exercised on the QEMU demo run, because QEMU TCG
  without OVMF-with-secure-boot doesn't give us a useful SRTM.
  Real-silicon next.
- **dm-verity root with signed roothash.** Kernel supports it
  (`DM_VERITY=y`); an actual verity image is a rootfs-packaging
  change, not a kernel change. Current demo uses a pure initramfs
  so the root is the in-memory tmpfs.
- **TPM2 sealed roothash / PCR policy to unwrap workload secrets.**
  Out of scope for the current `~tpm2@2.0a` device — that is a
  future `~tpm2-seal@2.0a` or equivalent.

## Initramfs — what can be stripped

Total 197 MB. The HB release alone contributes ~180 MB. Three
clear wins:

1. **Double-shipped `priv/`.** relx's `{copy, "priv", "bin/priv"}`
   overlay duplicates the whole priv tree under `bin/priv/`, on top
   of the standard `lib/hb-0.0.1/priv/` that relx produces anyway.
   Inside `priv/`: `dev_snp_nif.so` (10 MB), `secp256k1_arweave.so`
   (1.5 MB), `hyperbuddy@1.0/bundle.js` (15 MB). Dropping the
   overlay copy saves ~25 MB.
2. **`lib/hb-0.0.1/src/`.** relx ships the `.erl` source alongside
   the `.beam` — convenient for stack traces on a dev box, dead
   weight in the guest. Another ~15 MB saved (mostly the
   hyperbuddy bundle.js which lives under `src/html/`).
3. **`dev_snp_nif.so`.** LapEE uses TPM, not AMD SEV-SNP — the
   SNP NIF is ~10 MB of Rust we don't call. It's built because
   `rebar.config`'s `cargo_opts` always builds `native/dev_snp_nif/`.
   Fixing this is a rebar change (or a lapee-profile exclusion),
   so it's deferred in this cleanup pass.
4. **Strip `beam.smp`.** BEAM ships with full debug symbols
   (46 MB → ~12 MB after `strip -s`).
5. **`hyperbuddy@1.0/bundle.js` (15 MB).** The web UI. The LapEE
   guest serves the attestation; nobody drives it through the
   browser. Remove both copies in the initramfs (keep the release's
   own ebin untouched so HB still loads) → ~30 MB saved.

Items 1, 2, 4, 5 are done in this pass (see
`scripts/build-initramfs-hb.sh` diff). Item 3 deferred.

### After strip

After the four easy wins, expected initramfs footprint:

| before | after | saved |
|---|---|---|
| 197 MB extracted | ~115 MB | ~80 MB |
| 82 MB gz        | ~40–50 MB | ~30–40 MB |
