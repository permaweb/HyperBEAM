# LapEE bare-metal orchestration.
#
# === v1.2 hot path (current; see STATUS.md for mission brief) ===
#
#   make builders          one-time Docker amd64 builders (~3 min)
#   make buildroot         kernel + minimal rootfs; first build
#                          ~30-60 min on Rosetta, incremental ~3 min
#   make hb-release        HB release with dev_tpm2 NIF + v1.2 devices
#   make hb-initramfs      assemble guest image with HB + splash + DHCP
#   make hb-usb-image      wrap kernel + initramfs into a bootable
#                          UEFI USB image at work/lapee-usb.img
#   make hb-usb-write DEV=/dev/diskN
#                          dd to a physical USB stick (asks for YES
#                          confirmation; detects non-disk devices)
#   make hb-cross-node-verify
#                          run verify-peer against a live peer HB
#
#   Verifier side (after writing + booting the target laptop):
#     ./scripts/interpret-local-capture.sh \
#         --label 'my laptop' \
#         /Volumes/LAPEE_ESP/attestation-latest.json
#
# === Legacy (pre-v1.0; scripts under scripts/legacy/) ===
#
# These targets still run but are kept for historical repro only.
# Not on the Framework USB-boot path.
#
#   make demo / verify / all
#     M3-era container-based flow (lapee_node inside Docker vs swtpm)
#   make real-all
#     Debian-kernel-under-QEMU milestone ladder (M2-M5)
#   make hb-all / hb-acceptance / hb-tamper-test / hb-interpret-demo
#     Pre-v1.0 QEMU+swtpm acceptance batteries. The v1.2 equivalent
#     is `make hb-usb-image' -> QEMU smoke-boot or iron reflash.
#
# See README.md and STATUS.md for documentation.

LAPEE_ROOT := $(shell pwd)
export LAPEE_ROOT

DOCKER_AMD64 := docker run --rm --platform=linux/amd64

.PHONY: help builders nif-linux tpm-start tpm-stop tpm-status \
        demo verify all clean reset-tpm \
        fetch-debian-kernel initramfs boot-real real-all \
        buildroot buildroot-shell buildroot-clean boot-buildroot

help:
	@awk 'BEGIN{FS=" *—"} /^[a-zA-Z_-]+ *— / {printf "  make %-22s %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort -u

builders:
	docker build --platform=linux/amd64 -t lapee-builder:latest       -f docker/Dockerfile.builder       docker/
	docker build --platform=linux/amd64 -t lapee-hyperbeam-builder:latest -f docker/Dockerfile.hyperbeam docker/
	docker build --platform=linux/amd64 -t lapee-tools:latest         -f reference-demo/tpm-tools-docker/Dockerfile reference-demo/tpm-tools-docker/

tpm-start:
	./scripts/legacy/swtpm.sh start

tpm-stop:
	./scripts/legacy/swtpm.sh stop

tpm-status:
	./scripts/legacy/swtpm.sh status

reset-tpm:
	./scripts/legacy/swtpm.sh reset && ./scripts/legacy/swtpm.sh start

nif-linux:
	@mkdir -p work
	$(DOCKER_AMD64) \
	  -v $(LAPEE_ROOT)/lapee-tpm:/src:ro \
	  -v $(LAPEE_ROOT)/work:/work \
	  lapee-hyperbeam-builder:latest \
	  bash -c "mkdir -p /work/lapee-tpm-linux && \
	           cp -r /src/* /work/lapee-tpm-linux/ && \
	           rm -rf /work/lapee-tpm-linux/_build /work/lapee-tpm-linux/priv && \
	           cp /src/rebar.config.linux /work/lapee-tpm-linux/rebar.config && \
	           cd /work/lapee-tpm-linux && rebar3 compile && \
	           ls -lh priv/"

# Demo: container standing in for guest. Quick iteration for NIF / orchestrator changes.
demo: reset-tpm
	@mkdir -p out
	$(DOCKER_AMD64) \
	  -e LAPEE_TPM_TCTI=swtpm:host=host.docker.internal,port=2321 \
	  -v $(LAPEE_ROOT)/lapee-tpm/src:/src-overlay:ro \
	  -v $(LAPEE_ROOT)/work:/work \
	  -v $(LAPEE_ROOT)/out:/out \
	  lapee-hyperbeam-builder:latest \
	  bash -c "cp /src-overlay/lapee_hashpath.erl /src-overlay/lapee_node.erl /work/lapee-tpm-linux/src/ && \
	           cd /work/lapee-tpm-linux && rm -rf _build/default/lib/lapee_tpm/ebin && \
	           rebar3 compile >/dev/null 2>&1 && \
	           erl -noshell -pa _build/default/lib/lapee_tpm/ebin -eval 'lapee_node:run(#{}), halt()'"

verify:
	@mkdir -p reference-demo/out
	@cp out/attestation.json reference-demo/out/
	@cp out/test-tpm-ca.crt  reference-demo/out/
	python3 reference-demo/verifier/verifier.py

all: tpm-start nif-linux demo verify

# ----- Real guest path -----

fetch-debian-kernel:
	@mkdir -p build-alpine
	$(DOCKER_AMD64) -v $(LAPEE_ROOT)/build-alpine:/out debian:bookworm-slim bash -c \
	  "apt-get update -qq 2>&1 | tail -1 && \
	   cd /tmp && apt-get install -y -qq --download-only linux-image-amd64 2>&1 | tail -1 && \
	   dpkg -x /var/cache/apt/archives/linux-image-6*.deb /tmp/x && \
	   cp /tmp/x/boot/vmlinuz-* /out/vmlinuz-debian && \
	   echo 'kernel extracted.'"
	@ls -lh build-alpine/vmlinuz-debian

initramfs: nif-linux
	@mkdir -p work
	@./scripts/legacy/build-initramfs.sh

boot-real:
	@./scripts/legacy/boot-real.sh

real-all: builders fetch-debian-kernel nif-linux initramfs boot-real verify

# ----- HyperBEAM-as-userspace path (dev_tpm2) -----
#
# Full LapEE stack:
#   - Debian kernel (TPM + IMA builtin)
#   - Our initramfs containing a full HB release (edge branch + lapee
#     profile, with dev_tpm2 NIF preloaded)
#   - `on.start' hook extends PCR 15 with `hb_message:id(node_msg)'
#   - HB HTTP server answers `/~tpm2@2.0a/attestation' with the full envelope

.PHONY: hb-release hb-initramfs hb-boot hb-verify hb-all hb-acceptance hb-tamper-test hb-interpret-demo hb-cross-node-verify hb-dashboard hb-dashboard-open

hb-release:
	./scripts/build-hb-release.sh

hb-initramfs:
	./scripts/build-initramfs-hb.sh

hb-boot:
	./scripts/legacy/boot-hb.sh

# hb-usb-image: produce a UEFI-bootable .img combining the
# Buildroot kernel + our HB initramfs + a default LapEE cmdline.
# Override KERNEL / INITRAMFS / CMDLINE / SIZE_MIB / OUT on the
# make command line.
#
# The default KERNEL is the Buildroot-built kernel at
# build-alpine/vmlinuz-lapee. It has CONFIG_TCG_TPM + USB
# storage + VFAT all built-in, which is everything the USB-
# stick round-trip path (LAPEE_WRITEBACK=1) needs — no network,
# no kernel-module side-loading.
KERNEL     ?= build-alpine/vmlinuz-lapee
INITRAMFS  ?= work/initramfs-hb.cpio.gz
#
# CMDLINE validated end-to-end on:
#   * QEMU + OVMF + swtpm (scripts/boot-usb-image.sh, 2026-04-22)
#   * Framework 13 / Insyde BIOS IFR30.03.04, Secure Boot off,
#     AMD fTPM, microSD expansion card -- first successful
#     bare-metal boot + signed-attestation writeback 2026-04-22
#
# Token-by-token:
#   console=tty0       physical screen (Framework + any laptop)
#   console=ttyS0      serial (QEMU -nographic; ignored on iron)
#   quiet loglevel=1   suppress kernel info/notice/warning/err;
#                      only PANIC/ALERT/CRIT/ERR reach the
#                      physical console. Userspace echoes
#                      (including our splash + init prints) are
#                      unaffected. The v1.2 demo-ops review
#                      flagged that WARN-level messages (ACPI
#                      quirks, thunderbolt init, IOMMU group
#                      allocation) scroll over the boot splash
#                      and leave a garbled final frame on screen.
#                      Dmesg still has everything for serial-
#                      console post-mortem.
#   panic=10           reboot 10s after kernel panic (safety)
#   rdinit=/init       run our initramfs init as PID 1
#   LAPEE_WRITEBACK=1  init writes attestation to the ESP
#                      partition it was booted from, then
#                      waits for power-off
#
# v1.2 removed `ima_policy=tcb' -- the initramfs stub doesn't
# emit an IMA log, so PCR 10 reads as zero anyway. Re-add when
# the guest grows an IMA runtime; keeping the default off saves
# kernel time on every exec.
CMDLINE    ?= console=tty0 console=ttyS0 video=efifb:mtrr,nowc fbcon=nodefer loglevel=4 panic=10 rdinit=/init LAPEE_WRITEBACK=1
# v1.2.1 cmdline changes (matches the kernel-fragment update that
# enables CONFIG_FB_EFI + CONFIG_FRAMEBUFFER_CONSOLE):
#
#   video=efifb:mtrr,nowc  -- keep the EFI-stub-lit framebuffer
#                             active when the kernel takes over.
#                             Without this, Framework 13 boot after
#                             EFI stub goes dark because no DRM
#                             driver claims the display. The `mtrr'
#                             param re-enables write-combining
#                             through MTRR; `nowc' disables it inside
#                             the framebuffer as a minimum-compat
#                             setting (works on every tested board).
#   fbcon=nodefer          -- tell fbcon to bind IMMEDIATELY on the
#                             efifb; no "deferred takeover" phase
#                             that waits for a nonexistent KMS.
#   loglevel=1 -> 4        -- was PANIC/ALERT/CRIT/ERR only (too
#                             quiet; hid the post-stub hang on iron).
#                             4 also emits WARNING (needed to see
#                             NIC probe + DHCP lines) but suppresses
#                             the INFO/DEBUG firehose. Demo-friendly.
#   `quiet' removed        -- the quiet flag overrides loglevel=4
#                             by also filtering KERN_WARNING. We want
#                             warnings visible.
SIZE_MIB   ?= 1024
OUT        ?= work/lapee-usb.img
hb-usb-image:
	./scripts/build-usb-image.sh \
	    --kernel     "$(KERNEL)" \
	    --initramfs  "$(INITRAMFS)" \
	    --cmdline    "$(CMDLINE)" \
	    --size       "$(SIZE_MIB)" \
	    --image      "$(OUT)"

# hb-usb-write DEV=/dev/diskN — build and write directly to a
# USB stick. Prompts before writing.
hb-usb-write:
	@test -n "$(DEV)" || { \
	    echo "usage: make hb-usb-write DEV=/dev/diskN"; exit 1; }
	./scripts/build-usb-image.sh \
	    --kernel     "$(KERNEL)" \
	    --initramfs  "$(INITRAMFS)" \
	    --cmdline    "$(CMDLINE)" \
	    --size       "$(SIZE_MIB)" \
	    --device     "$(DEV)"

hb-verify:
	python3 reference-demo/verifier/verifier_hb.py \
		out/attestation.json out/test-tpm-ca.crt

# hb-all drives the default flow end-to-end. Prefer the Buildroot
# kernel (virtio_net, tpm_tis, IMA builtin) when it's present;
# otherwise fall back to the Debian linux-image-amd64 via
# fetch-debian-kernel — but note that the Debian kernel requires
# the dev_tpm2 end-to-end test to run on a kernel that has virtio_net
# in-tree, so `make buildroot' first is strongly preferred.
hb-all:
	@if [ ! -f build-alpine/vmlinuz-lapee ] && [ ! -f build-alpine/vmlinuz-debian ]; then \
		echo "no kernel available; run: make buildroot  OR  make fetch-debian-kernel" >&2; \
		exit 1; \
	fi
	@if [ ! -x build-hyperbeam/src-edge/_build/lapee/rel/hb/bin/hb ]; then \
		echo "no HB release available; run: make hb-release" >&2; \
		exit 1; \
	fi
	$(MAKE) hb-initramfs
	$(MAKE) hb-boot
	$(MAKE) hb-verify

# hb-acceptance: the three-envelope battery documented in
# out/evidence/README.md. Boots the guest three times with
# (no user config / benign user config / hostile user config) and
# verifies each envelope. Expected output: three "ATTESTATION
# ACCEPTED" verdicts plus a final summary showing that each run's
# `node_message_id_hex' differs and that the hostile run did NOT
# succeed in swapping the `on.start' hook device.
# Each of hb-acceptance / hb-tamper-test / hb-interpret-demo rebuilds
# the evidence dashboard at the end (even on failure) so the operator
# always has a human-readable view of the latest run. The exit code
# still reflects the underlying script's outcome.
hb-acceptance:
	./scripts/legacy/hb-acceptance.sh; rc=$$?; ./scripts/legacy/build-evidence-dashboard.py >/dev/null || true; exit $$rc

# hb-tamper-test: starting from a valid envelope in out/evidence/,
# produces seven targeted byte-flips and asserts the verifier rejects
# each at the expected check. Completeness pair to hb-acceptance.
hb-tamper-test:
	./scripts/legacy/hb-tamper-test.sh; rc=$$?; ./scripts/legacy/build-evidence-dashboard.py >/dev/null || true; exit $$rc

# hb-interpret-demo: end-to-end demo of ~tpm-interpret@1.0, feeding
# a saved envelope to /~tpm-interpret@1.0/verify over HTTP and
# pretty-printing the rich per-section interpretation. Also exercises
# /events + /claim via the new events+claim demo script.
hb-interpret-demo:
	./scripts/legacy/hb-interpret-demo.sh && ./scripts/legacy/hb-events-claim-demo.sh; rc=$$?; \
	mkdir -p out/evidence; \
	[ -f /tmp/hb-interpret-result.json ] && cp /tmp/hb-interpret-result.json out/evidence/interpret-verify-live.json || true; \
	[ -f /tmp/hb-events-result.json ]    && cp /tmp/hb-events-result.json    out/evidence/events-live.json    || true; \
	[ -f /tmp/hb-claim-result.json ]     && cp /tmp/hb-claim-result.json     out/evidence/claim-live.json     || true; \
	./scripts/legacy/build-evidence-dashboard.py >/dev/null || true; \
	exit $$rc

# hb-cross-node-verify: the paper's real use-case. Starts a SEPARATE
# HB node natively on macOS (outside QEMU) and has it verify the
# guest peer via `GET /~tpm-interpret@1.0/verify-peer&peer=<url>'.
# The verifier's trust anchor (TPM CA) is installed BEFORE the call,
# mirroring how a real operator pre-configures a vendor root CA.
# Minimises the possibility of same-process shortcuts or accidental
# trust propagation: two separate BEAM VMs, two separate network
# origins, independently-installed trust anchors, independent five-
# check cryptographic verification on the verifier side.
hb-cross-node-verify:
	./scripts/hb-cross-node-verify.sh

# hb-dashboard: build a single-file HTML evidence dashboard from every
# file in out/evidence/ + out/acceptance/. Use `hb-dashboard-open` to
# auto-open it in Chrome on macOS. The dashboard is fully self-contained
# (inline CSS, embedded JSON) so it works when emailed, stashed in
# git, or served from a static file host.
hb-dashboard:
	./scripts/legacy/build-evidence-dashboard.py

hb-dashboard-open:
	./scripts/legacy/build-evidence-dashboard.py --open

# ----- Buildroot path -----
#
# Builds a LapEE kernel + initramfs from Buildroot 2024.02 LTS inside the
# `lapee-builder:latest` Ubuntu-amd64 container. First-build under Rosetta
# is ~30-60 min (we skip the host-gcc build by using Bootlin's pre-built
# external toolchain). Subsequent builds are incremental via the shared
# Docker volume `lapee-build-m1`.
#
# Artefacts dropped into:
#   build-alpine/vmlinuz-lapee
#   work/initramfs-lapee.cpio.gz

buildroot:
	./scripts/build-buildroot.sh

buildroot-shell:
	docker run --rm -it --platform=linux/amd64 \
	  -v lapee-build-m1:/build \
	  -v $(LAPEE_ROOT)/buildroot-external:/src-external:ro \
	  lapee-builder:latest bash

buildroot-clean:
	docker run --rm --platform=linux/amd64 -v lapee-build-m1:/build lapee-builder:latest \
	  bash -c "rm -rf /build/out /build/buildroot-external"

boot-buildroot:
	./scripts/legacy/boot-buildroot.sh

clean:
	rm -rf work/lapee-tpm-linux work/initramfs-mini.cpio.gz out/*.json out/test-ek.* out/node-address.txt
	@echo "cleaned; run 'make reset-tpm' to wipe TPM state too."
