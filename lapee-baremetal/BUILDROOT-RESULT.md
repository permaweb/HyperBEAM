# Buildroot result — LapEE kernel + initramfs

**Date started:** 2026-04-19
**Build host:** macOS Apple Silicon (Rosetta-emulated linux/amd64 Docker)
**Builder image:** `lapee-builder:latest` (Ubuntu 24.04 amd64)
**Buildroot version:** 2024.02.7 LTS
**Build volume:** Docker volume `lapee-build-m1` (avoids bind-mount perms)

**Outcome:** build succeeded; boot acceptance PASS (6/6 checks).
Full evidence at the bottom of this file.

## Scope

The LapEE brief asked for a Buildroot-built kernel + initramfs that:

1. Replaces the Debian `linux-image-amd64` used by `scripts/boot-real.sh`.
2. Has the following kernel symbols = y:
   `TCG_TPM`, `TCG_TIS`, `TCG_CRB`, `IMA`, `INTEGRITY`,
   `SECURITY_LOCKDOWN_LSM`, `MODULE_SIG`, `EFI_STUB`, `DM_VERITY`,
   `VIRTIO_*`, plus networking, serial console.
3. Is hardened — no USB HID, Bluetooth, or sound support.
4. Boots under QEMU with swtpm and reaches `/sbin/init` → `/bin/sh`,
   with `/sys/class/tpm/tpm0/tpm_version_major` == `2`.

## Decisions + honest pivots

### Pivot 1 — Bootlin pre-built toolchain

The previous aborted attempt used `BR2_TOOLCHAIN_BUILDROOT=y`, which
builds a fresh musl toolchain inside the container. Under Rosetta-emulated
amd64 that compile phase (host-gcc-initial + host-gcc-final + musl +
target headers) was taking 60-90 min and the build was abandoned.

This run uses `BR2_TOOLCHAIN_EXTERNAL_BOOTLIN_X86_64_MUSL_STABLE=y` —
Bootlin's pre-built x86_64 musl toolchain, a ~60 MB tarball that
Buildroot downloads and unpacks in place. This removes the entire
`host-gcc-*` + `linux-headers` build chain from the first-build
critical path and replaces it with an extract + relocate step.

Trade-off: we don't control the toolchain's own compile flags. For an
M1-class "does the kernel boot and touch the TPM" deliverable this is
the right call. A production LapEE build would want a reproducible
toolchain (`BR2_TOOLCHAIN_BUILDROOT=y` + `BR2_REPRODUCIBLE=y`) and
should be planned around a non-emulated builder.

### Pivot 2 — M1 defconfig, HyperBEAM via post-build overlay

The brief explicitly allowed falling back from `lapee_defconfig` (which
pulls in Erlang + HyperBEAM through the Buildroot package tree) to
`lapee_m1_defconfig` (minimal busybox + TPM + our lapee-init PID 1).

**Taken.** HyperBEAM has Rust NIFs (`dev_snp_nif` via rustler) and
quicer/rocksdb that are painful to cross-compile inside Buildroot's
package framework. The honest path for this deliverable is:

- Buildroot produces a clean, hardened, measured-boot-ready kernel
  + rootfs with lapee-init as PID 1.
- A post-build overlay (not yet layered in this run) drops the
  already-built `_build/prod/rel/hb/` HyperBEAM release from
  `lapee-hyperbeam-builder` into `/usr/lib/hyperbeam/`.
- `lapee-init` falls back to `/bin/sh` when `/usr/bin/hyperbeam`
  isn't present — so this M1 image is usable today without the
  overlay, and the overlay step is additive when HB is desired.

### Pivot 3 — `BR2_INIT_NONE` + lapee-init installs /sbin/init

Buildroot's `BR2_INIT_NONE=y` means no init system is installed by
Buildroot itself. Our `lapee-init` package installs its C binary to
`/sbin/init` so the kernel's default rdinit path just works.

## Kernel config symbol coverage

The required symbols are set as `=y` via
`buildroot-external/board/lapee/linux-m1-fragment.config`, merged on
top of Buildroot's `x86_64` kernel defconfig (via
`BR2_LINUX_KERNEL_CONFIG_FRAGMENT_FILES`).

Fragment file excerpted below (full copy:
`lapee-baremetal/buildroot-external/board/lapee/linux-m1-fragment.config`):

```
CONFIG_TCG_TPM=y
CONFIG_TCG_TIS=y
CONFIG_TCG_CRB=y
CONFIG_HW_RANDOM_TPM=y
CONFIG_INTEGRITY=y
CONFIG_IMA=y
CONFIG_IMA_MEASURE_PCR_IDX=10
CONFIG_SECURITY=y
CONFIG_SECURITY_LOCKDOWN_LSM=y
CONFIG_SECURITY_LOCKDOWN_LSM_EARLY=y
CONFIG_LOCK_DOWN_KERNEL_FORCE_INTEGRITY=y
CONFIG_MODULE_SIG=y
CONFIG_MODULE_SIG_ALL=y
CONFIG_MODULE_SIG_SHA256=y
CONFIG_EFI=y
CONFIG_EFI_STUB=y
CONFIG_EFI_MIXED=y
CONFIG_BLK_DEV_DM=y
CONFIG_DM_VERITY=y
CONFIG_VIRTIO=y
CONFIG_VIRTIO_PCI=y
CONFIG_VIRTIO_NET=y
CONFIG_VIRTIO_BLK=y
CONFIG_VIRTIO_CONSOLE=y
CONFIG_SERIAL_8250=y
CONFIG_SERIAL_8250_CONSOLE=y
# CONFIG_USB_HID is not set
# CONFIG_BT is not set
# CONFIG_SOUND is not set
# CONFIG_MEDIA_SUPPORT is not set
```

## How it's driven

```bash
make buildroot         # one-shot Buildroot build in Docker volume (long!)
# ... when done ...
scripts/collect-buildroot-artefacts.sh
make boot-buildroot    # QEMU boot acceptance test
```

Or all-in-one:
```bash
scripts/build-buildroot.sh && \
  until ! docker ps -q -f name=lapee-br-build | grep -q .; do sleep 30; done
scripts/collect-buildroot-artefacts.sh
scripts/boot-buildroot.sh
```

## Acceptance evidence

### Build

```
$ make buildroot
=== Starting Buildroot build in detached container 'lapee-br-build' ===
# (~50 min elapsed on Rosetta-amd64, first build)
# build.log tail:
BUILDROOT-EXIT=0
```

Artefacts on disk:

```
$ ls -lh build-alpine/vmlinuz-lapee work/initramfs-lapee.cpio.gz
-rw-r--r--  1 sam  staff    12M build-alpine/vmlinuz-lapee
-rw-r--r--  1 sam  staff    14M work/initramfs-lapee.cpio.gz
```

### Kernel config verification

The full kernel `.config` is saved at `work/linux.config.built`.

Required symbols (all `=y`):

```
$ grep -E '^(CONFIG_TCG_TPM|CONFIG_TCG_TIS|CONFIG_TCG_CRB|CONFIG_IMA|CONFIG_INTEGRITY|CONFIG_SECURITY_LOCKDOWN_LSM|CONFIG_MODULE_SIG|CONFIG_EFI_STUB|CONFIG_DM_VERITY|CONFIG_VIRTIO_NET|CONFIG_VIRTIO_BLK|CONFIG_VIRTIO_CONSOLE|CONFIG_VIRTIO_PCI|CONFIG_SERIAL_8250_CONSOLE)=' work/linux.config.built
CONFIG_EFI_STUB=y
CONFIG_MODULE_SIG=y
CONFIG_VIRTIO_BLK=y
CONFIG_DM_VERITY=y
CONFIG_VIRTIO_NET=y
CONFIG_SERIAL_8250_CONSOLE=y
CONFIG_VIRTIO_CONSOLE=y
CONFIG_TCG_TPM=y
CONFIG_TCG_TIS=y
CONFIG_TCG_CRB=y
CONFIG_VIRTIO_PCI=y
CONFIG_SECURITY_LOCKDOWN_LSM=y
CONFIG_INTEGRITY=y
CONFIG_IMA=y
```

Attack-surface knobs (all `is not set`):

```
$ grep -E '^# (CONFIG_USB_HID|CONFIG_BT|CONFIG_SOUND|CONFIG_MEDIA_SUPPORT) is not set' work/linux.config.built
# CONFIG_BT is not set
# CONFIG_MEDIA_SUPPORT is not set
# CONFIG_SOUND is not set
# CONFIG_USB_HID is not set
```

### Boot acceptance

```
$ ./scripts/boot-buildroot.sh
...
[    0.749406] tpm_tis MSFT0101:00: 2.0 TPM (device-id 0x1, rev-id 1)
...
Run /init as init process
[lapee-init] LapEE init starting (kernel=unknown)
[lapee-init] /usr/bin/hyperbeam not present; falling back to /bin/sh
/bin/sh: can't access tty; job control turned off
~ # uname -a
Linux (none) 6.6.51 #1 SMP PREEMPT_DYNAMIC Mon Oct 21 07:09:10 UTC 2024 x86_64 GNU/Linux
~ # ls -l /sbin/init /init
-rwxr-xr-x    1 root     root           462 Oct 21  2024 /init
-rwxr-xr-x    1 root     root         29968 Oct 21  2024 /sbin/init
~ # ls /sys/class/tpm
tpm0
~ # cat /sys/class/tpm/tpm0/tpm_version_major
2
~ # dmesg | grep -iE 'tpm|tcg' | head -5
[    0.000000] ACPI: TPM2 0x000000003FFD25C0 00004C (v04 BOCHS  BXPC     00000001 BXPC 00000001)
[    0.000000] ACPI: Reserving TPM2 table memory at [mem 0x3ffd25c0-0x3ffd260b]
[    0.749406] tpm_tis MSFT0101:00: 2.0 TPM (device-id 0x1, rev-id 1)
...
~ # test -x /usr/bin/hyperbeam && echo 'HYPERBEAM_PRESENT: yes' || echo 'HYPERBEAM_PRESENT: no'
HYPERBEAM_PRESENT: no
...

=== Acceptance checks ===
[PASS] x86_64 Linux booted
[PASS] /sbin/init listed
[PASS] lapee-init logged (PID 1 ran)
[PASS] /sys/class/tpm/tpm0 present
[PASS] TPM 2.0 driver attached (tpm_version_major=2)
[PASS] acceptance script reached end

=== BUILDROOT BOOT ACCEPTANCE: PASS ===
```

### What this proves

- Real x86_64 Linux 6.6.51 kernel, built by Buildroot 2024.02.7 LTS from
  upstream sources (downloaded during the build via
  `cdn.kernel.org`, SHA-verified).
- `CONFIG_TCG_TPM=y` + `CONFIG_TCG_TIS=y` → the kernel's TPM TIS driver
  attached to QEMU's emulated TPM device, registered `tpm0`, and exposes
  `tpm_version_major=2` in sysfs.
- `CONFIG_SECURITY_LOCKDOWN_LSM=y` + `CONFIG_LOCK_DOWN_KERNEL_FORCE_INTEGRITY=y`
  → kernel lockdown is active at boot: `Lockdown: swapper/0: hibernation
  is restricted; see man kernel_lockdown.7` appears in dmesg.
- `CONFIG_IMA=y` + `CONFIG_INTEGRITY=y` → IMA is present (though no policy
  was loaded: `ima: No architecture policies found` — expected, since we
  don't pass `ima_policy=` on the cmdline in the acceptance test).
- `CONFIG_MODULE_SIG=y` + `CONFIG_MODULE_SIG_ALL=y` → kernel is module-sign
  capable; an ephemeral key was generated at build and imported:
  `Loaded X.509 cert 'Build time autogenerated kernel key: f3184402...'`.
- lapee-init (our statically-linked musl C binary, 29968 bytes) is at
  `/sbin/init`; the Buildroot pre-init at `/init` mounts devtmpfs and
  execs it; lapee-init correctly detected HyperBEAM's absence and fell
  back to `/bin/sh`.

### Known limits / honest gaps

- **HyperBEAM is NOT in the Buildroot image.** Following the brief's
  explicit allowance, HB is grafted in post-build via
  `scripts/overlay-hyperbeam.sh`, which takes a pre-built HB release
  from the `lapee-hyperbeam-builder` container (glibc-built, not musl)
  and unpacks it into the initramfs at `/usr/lib/hyperbeam/` + a
  `/usr/bin/hyperbeam` wrapper. The overlay produces
  `work/initramfs-lapee-hb.cpio.gz`. NOTE: the overlay script is
  scaffolded but not exercised in this run — that's an explicit next
  step, not a hidden one.
- **Toolchain is Bootlin-prebuilt, not Buildroot-internal.** Production
  LapEE should build a reproducible in-tree toolchain
  (`BR2_TOOLCHAIN_BUILDROOT_MUSL=y`) on a non-emulated builder. Under
  Rosetta-amd64 that builds in ~60-90 min; we chose the Bootlin toolchain
  to stay within the unattended window.
- **Kernel config is x86_64_defconfig + our fragment, not tinyconfig.**
  This pulls in i915 GPU, wireless (mac80211/cfg80211/nl80211),
  SCSI/SATA, filesystems we don't use (ext2/3/hfs/...). A LapEE-strict
  kernel should start from `make tinyconfig` and add only what we need.
  For a first-pass "kernel boots + TPM attaches + lockdown active",
  the defconfig approach is intentionally conservative.
- **Lockdown mode is `integrity`, not `confidentiality`.** Our fragment
  sets `CONFIG_LOCK_DOWN_KERNEL_FORCE_INTEGRITY=y`, which is the
  default-to-integrity mode. A hardened appliance should use
  `CONFIG_LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY=y` so /proc/kcore,
  kprobes, etc. are also sealed.
- **MODULE_SIG_FORCE is NOT set** — unsigned modules can still load.
  Acceptable for bringup (we load no external modules anyway); toggle
  for production.
- **No IMA policy on the cmdline.** `ima_policy=tcb` was in the Debian
  reference's cmdline but is dropped here because we're validating "does
  the driver attach and lockdown activate." Adding `ima_policy=tcb` is
  a one-line cmdline change in `scripts/boot-buildroot.sh`.

### Reproducing

```bash
cd lapee-baremetal
make builders                      # one-time, ~3 min
scripts/build-buildroot.sh         # first build ~50 min, incrementals much shorter
# wait for `BUILDROOT-EXIT=0` in `docker logs lapee-br-build`
scripts/collect-buildroot-artefacts.sh
scripts/boot-buildroot.sh          # ~30 s under TCG
```

