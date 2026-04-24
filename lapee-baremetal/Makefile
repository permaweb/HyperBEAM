# LapEE bare-metal -- build orchestration for the v1.2.2 USB-boot path.
#
# The mission: produce a single UEFI-bootable USB image that boots a
# real laptop (Framework 13 validated on iron, other UEFI+TPM2 machines
# expected to work), produces a signed TPM attestation envelope with
# the paper's security properties, and writes it back to the ESP the
# image was booted from.
#
# See README.md for the demo flow + STATUS.md for the mission brief.
#
# ============================================================
# Usage (in order; each step caches results from the last).
#
#   make builders       one-time: build the two Docker amd64 images
#                       (`lapee-builder' for buildroot kernel, and
#                        `lapee-hyperbeam-builder' for HB release).
#                       ~3 min.
#
#   make buildroot      build the LapEE kernel. ~15-30 min first
#                       time, incremental ~3 min. Artefact:
#                         build-alpine/vmlinuz-lapee
#
#   make hb-release     compile the HyperBEAM release with
#                       dev_tpm2 NIF + v1.2 devices. Syncs
#                       src/ + native/ + priv/ from the parent
#                       HB repo into build-hyperbeam/src-edge/
#                       and runs rebar3 release. ~3-5 min
#                       incremental.
#
#   make hb-initramfs   assemble the guest initramfs around
#                       that HB release. Ships wpa_supplicant +
#                       WiFi firmware + init script. ~30 s.
#                       Artefact: work/initramfs-hb.cpio.gz
#
#   make hb-usb-image   wrap kernel + initramfs + cmdline into
#                       a UEFI-bootable .img (UKI at /EFI/Boot/
#                       BootX64.efi, FAT32 ESP, 1 GB). ~15 s.
#                       Artefact: work/lapee-usb.img
#
#   make hb-usb-write DEV=/dev/diskN
#                       dd the .img to a USB stick. Prompts
#                       for confirmation. See README.md for
#                       macOS / Linux flash recipes.
#
# Optional:
#
#   make hb-usb-qemu    smoke-boot the just-built lapee-usb.img
#                       under QEMU + OVMF + swtpm. Useful
#                       validation pass before writing to real
#                       hardware. ~5 min.
#
#   make hb-wifi-apply  inject host-side `wifi.conf' (two lines:
#                       SSID / PSK) into the ESP of the currently-
#                       built work/lapee-usb.img at /EFI/boot/
#                       wifi.conf. Credentials are NOT measured
#                       (wifi.conf is not part of the signed UKI);
#                       re-run freely to change networks without
#                       re-signing. File lives at wifi.conf in
#                       this directory and is .gitignored.
#
#   make hb-sb-apply    inject the operator's SB enrolment bundle
#                       (secureboot/enrol/*.{cer,auth}) into the
#                       ESP root of the currently-built
#                       work/lapee-usb.img. Fast: no UKI re-sign,
#                       no rebuild. Run after `sb-setup.sh enrol'
#                       against an already-built image, or when
#                       you've rotated keys and want to re-flash.
#
#   make hb-cross-node-verify
#                       start a verifier HB on the Mac, have
#                       it call /~tpm-interpret@1.0/verify-peer
#                       against a live target node.
#
# ============================================================

LAPEE_ROOT := $(shell pwd)
export LAPEE_ROOT

DOCKER_AMD64 := docker run --rm --platform=linux/amd64

.PHONY: help builders \
        buildroot buildroot-shell buildroot-clean \
        hb-release hb-initramfs hb-usb-image hb-usb-write \
        hb-usb-qemu hb-wifi-apply hb-sb-apply hb-cross-node-verify clean

help:
	@awk '/^# =+$$/{flag=!flag;if(!flag)exit;next} \
	      flag{sub(/^# ?/,"");print}' $(firstword $(MAKEFILE_LIST))

# ------------------------------------------------------------
# One-time Docker builders.
# ------------------------------------------------------------

builders:
	docker build --platform=linux/amd64 -t lapee-builder:latest \
	    -f docker/Dockerfile.builder       docker/
	docker build --platform=linux/amd64 -t lapee-hyperbeam-builder:latest \
	    -f docker/Dockerfile.hyperbeam     docker/
	docker build --platform=linux/amd64 -t lapee-tools:latest \
	    -f docker/Dockerfile.tools         docker/

# ------------------------------------------------------------
# Kernel build via Buildroot 2024.02 LTS inside lapee-builder.
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# HyperBEAM release + initramfs + USB image.
# ------------------------------------------------------------

hb-release:
	./scripts/build-hb-release.sh

hb-initramfs:
	./scripts/build-initramfs-hb.sh

# Defaults. Override on the command line to re-use the target for
# alternate kernels, custom cmdlines, image sizes, or output paths.
KERNEL     ?= build-alpine/vmlinuz-lapee
INITRAMFS  ?= work/initramfs-hb.cpio.gz
SIZE_MIB   ?= 1024
OUT        ?= work/lapee-usb.img

# Default cmdline (v1.2.2). Preserves the EFI-stub framebuffer into
# simpledrm + fbcon on AMD Ryzen / Intel Alder-Lake-P where efifb
# alone silently fails to bind, keeps the kernel log visible at
# loglevel=4 (warnings and above), wires init + attestation
# writeback, and opens the WiFi capability gate. `lapee.wifi=
# enabled' measured into PCR 4 via the UKI is the PUBLIC signal
# that the 802.11 stack is allowed to run; the PSK never touches
# the cmdline (comes from a strictly-validated file on the ESP --
# see `make hb-wifi-apply' / docs). Operators who want WiFi off
# even as a possibility can override CMDLINE to drop the token.
CMDLINE    ?= console=tty0 console=ttyS0 earlyprintk=efi,keep keep_bootcon \
              fbcon=nodefer loglevel=4 panic=10 rdinit=/init \
              LAPEE_WRITEBACK=1 LAPEE_TRACE=1 lapee.wifi=enabled

hb-usb-image:
	./scripts/build-usb-image.sh \
	    --kernel     "$(KERNEL)" \
	    --initramfs  "$(INITRAMFS)" \
	    --cmdline    "$(CMDLINE)" \
	    --size       "$(SIZE_MIB)" \
	    --image      "$(OUT)"

hb-usb-write:
	@test -n "$(DEV)" || { \
	    echo "usage: make hb-usb-write DEV=/dev/diskN"; exit 1; }
	./scripts/build-usb-image.sh \
	    --kernel     "$(KERNEL)" \
	    --initramfs  "$(INITRAMFS)" \
	    --cmdline    "$(CMDLINE)" \
	    --size       "$(SIZE_MIB)" \
	    --device     "$(DEV)"

# ------------------------------------------------------------
# QEMU smoke-boot of the USB image (optional validation before
# burning to real hardware).
# ------------------------------------------------------------

hb-usb-qemu:
	./scripts/boot-usb-image.sh

# ------------------------------------------------------------
# Inject a host-side wifi.conf into the ESP of the currently-
# built lapee-usb.img WITHOUT re-signing. The UKI stays the
# same (wifi.conf isn't inside it); only the ESP data changes.
# Fast: ~5 s. Handy for changing networks between boots of the
# same signed image.
# ------------------------------------------------------------

hb-wifi-apply:
	@test -f wifi.conf || { \
	    echo "wifi.conf missing. Create it with EXACTLY two lines:"; \
	    echo "  <SSID>"; echo "  <PSK>"; \
	    echo "(and nothing else). See README.md."; exit 1; }
	@test -f work/lapee-usb.img || { \
	    echo "work/lapee-usb.img missing. Run: make hb-usb-image"; \
	    exit 1; }
	docker run --rm --platform=linux/amd64 \
	    -v $(LAPEE_ROOT):/w -w /w lapee-tools:latest \
	    bash -euo pipefail -c '\
	        START=$$(parted --script --machine /w/work/lapee-usb.img \
	            unit s print | awk -F: "/^1:/ {gsub(\"s\",\"\",\$$2); print \$$2}"); \
	        SECT=$$(parted --script --machine /w/work/lapee-usb.img \
	            unit s print | awk -F: "/^1:/ {gsub(\"s\",\"\",\$$4); print \$$4}"); \
	        dd if=/w/work/lapee-usb.img of=/tmp/esp.img \
	            bs=512 skip=$$START count=$$SECT status=none; \
	        mmd -i /tmp/esp.img -D s ::/EFI/boot 2>/dev/null || true; \
	        mcopy -i /tmp/esp.img -o /w/wifi.conf ::/EFI/boot/wifi.conf; \
	        dd if=/tmp/esp.img of=/w/work/lapee-usb.img \
	            bs=512 seek=$$START count=$$SECT \
	            conv=notrunc status=none; \
	        echo ">> ESP /EFI/boot/ after apply:"; \
	        mdir -i /tmp/esp.img ::/EFI/boot/ | tail -n +3'
	@echo ">> wifi.conf applied to work/lapee-usb.img"
	@echo "   flash with: make hb-usb-write DEV=/dev/diskN"

# ------------------------------------------------------------
# Inject operator SB enrolment bundle into an already-built
# USB image, without touching the signed UKI.
# ------------------------------------------------------------

hb-sb-apply:
	@test -d secureboot/enrol || { \
	    echo "secureboot/enrol/ missing. Run: ./scripts/sb-setup.sh enrol"; \
	    exit 1; }
	@test -f work/lapee-usb.img || { \
	    echo "work/lapee-usb.img missing. Run: make hb-usb-image"; \
	    exit 1; }
	@count=0; for f in secureboot/enrol/PK.cer secureboot/enrol/KEK.cer \
	                   secureboot/enrol/db.cer secureboot/enrol/PK.auth \
	                   secureboot/enrol/KEK.auth secureboot/enrol/db.auth; do \
	    test -f "$$f" && count=$$((count + 1)); done; \
	test $$count -gt 0 || { \
	    echo "no .cer / .auth files in secureboot/enrol/"; exit 1; }
	docker run --rm --platform=linux/amd64 \
	    -v $(LAPEE_ROOT):/w -w /w lapee-tools:latest \
	    bash -euo pipefail -c '\
	        START=$$(parted --script --machine /w/work/lapee-usb.img \
	            unit s print | awk -F: "/^1:/ {gsub(\"s\",\"\",\$$2); print \$$2}"); \
	        SECT=$$(parted --script --machine /w/work/lapee-usb.img \
	            unit s print | awk -F: "/^1:/ {gsub(\"s\",\"\",\$$4); print \$$4}"); \
	        dd if=/w/work/lapee-usb.img of=/tmp/esp.img \
	            bs=512 skip=$$START count=$$SECT status=none; \
	        for f in PK.cer KEK.cer db.cer PK.auth KEK.auth db.auth; do \
	            if [[ -f /w/secureboot/enrol/$$f ]]; then \
	                mcopy -i /tmp/esp.img -o \
	                    /w/secureboot/enrol/$$f ::/$$f; \
	            fi; \
	        done; \
	        dd if=/tmp/esp.img of=/w/work/lapee-usb.img \
	            bs=512 seek=$$START count=$$SECT \
	            conv=notrunc status=none; \
	        echo ">> ESP root after apply:"; \
	        mdir -i /tmp/esp.img :: | tail -n +3'
	@echo ">> SB enrolment bundle applied to work/lapee-usb.img"
	@echo "   flash with: make hb-usb-write DEV=/dev/diskN"

# ------------------------------------------------------------
# Cross-node verification.
# ------------------------------------------------------------

hb-cross-node-verify:
	./scripts/hb-cross-node-verify.sh

# ------------------------------------------------------------
# Cleanup.
# ------------------------------------------------------------

clean:
	rm -rf work/lapee-usb.img work/lapee-usb-last.img \
	       work/initramfs-hb.cpio.gz work/initramfs-hb.cpio.zst \
	       work/usb-build/ out/local-capture/
	@echo "cleaned. Kernel artefact at build-alpine/vmlinuz-lapee preserved;"
	@echo "run 'make buildroot-clean' to wipe that too."
