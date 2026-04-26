# LapEE guest -- hardening + size audit (2026-04-19)

> **Note:** figures below are the 2026-04-19 baseline. v1.2
> (2026-04-23) added more NIC drivers to the kernel (negligible
> size impact) and shrank the initramfs via fixtures-removal
> + doc-dir pruning (115 MB HB tree -> 60 MB compressed
> initramfs). See [`STATUS.md`](STATUS.md) "Image slim" for
> the current post-v1.2 numbers. The threat model + hardening
> stance described below is unchanged through v1.2.

Audit of the guest the paper-aligned `~tpm2@2.0a` flow actually boots
(Buildroot-built `vmlinuz-lapee` + `initramfs-hb.cpio.gz`).

## Current footprint

| artefact | size | note |
|---|---|---|
| `build-kernel/vmlinuz-lapee` | 12 MB | Linux 6.6.51 x86_64 bzImage |
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

### Hardening status (post-v1.2.2; verified against
### `linux-m1-fragment.config' at 2026-04-23)

| flag | state | note |
|---|---|---|
| `FORTIFY_SOURCE` | **y** | compiler-level memcpy/strcpy bounds check |
| `HARDENED_USERCOPY` | **y** | kernel ↔ userspace copies size/slab-validated |
| `INIT_ON_ALLOC_DEFAULT_ON` | **y** | zero-on-alloc — no use-before-init infoleaks |
| `INIT_ON_FREE_DEFAULT_ON` | **y** | zero-on-free — no use-after-free infoleaks |
| `SLAB_FREELIST_HARDENED` | **y** | freelist-overwrite heap-exploit mitigation |
| `SLAB_FREELIST_RANDOM` | **y** | randomises slab alloc order |
| `IO_STRICT_DEVMEM` | **y** | no unprivileged `/dev/mem' to device I/O |
| `MODULE_SIG_FORCE` | **y** | unsigned modules rejected at load |
| `SECURITY_DMESG_RESTRICT` | **y** | kernel addresses hidden from unpriv users |
| `IMA_APPRAISE` + `INTEGRITY_TRUSTED_KEYRING` | **y** | IMA appraises, not just measures |
| `DEBUG_KERNEL` | **n** | production kernel, no debug scaffolding |
| `LOCK_DOWN_KERNEL_FORCE_INTEGRITY` | **y** | rejects kernel-modifying operations |
| `KEXEC_SIG` | **absent** | intentional: kexec disabled outright via lockdown, so signed-kexec is moot. Re-evaluate if/when kexec is re-enabled. |
| `LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY` | **off** | deferred: blocks perf/kprobes/tracefs which HB's `runtime_tools' exercises. Track-step for v1.3 once the HB runtime-observability story is refactored onto non-lockdown-fenced primitives. |

Every row that says **y** was "off" or "absent" on 2026-04-19.
The convergence to the stricter posture landed incrementally through
v1.1 + v1.2; the audit above is the verified ground truth as of
v1.2.2 shipping. See [`STATUS.md`](STATUS.md) for the reviewer-pass
history that drove the convergence.

### Configurable from the LapEE posture spec

These are properties of the _boot chain_ the kernel is embedded in,
not the kernel config itself:

- **UKI + Secure Boot (custom PK/KEK).** `scripts/sb-setup.sh`
  drives the operator-owned key pipeline end-to-end: generate
  PK/KEK/db, sign the UKI, produce UEFI-enrolment `.auth` files.
  Exercised on real silicon. No shim / no Microsoft CA; chain of
  trust terminates at the operator-owned PK, matching the paper's
  "device identity anchored at the TPM vendor root via the EK
  cert chain, operator-owned UEFI trust anchor via PK/KEK/db"
  statement.
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
