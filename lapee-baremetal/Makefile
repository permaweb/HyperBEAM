# LapEE bare-metal orchestration.
#
# Short path (software stack only, no QEMU):
#   make builders       build Docker images (one-time, ~3 min)
#   make nif-linux      cross-compile the NIF for x86_64 Linux
#   make demo           run lapee_node inside a container against swtpm
#   make verify         offline verify the resulting attestation.json
#
# Real-guest path (a Debian Linux kernel boots under QEMU, real TPM
# driver attaches, our NIF runs as init inside the guest, attestation
# comes out the serial port):
#   make fetch-debian-kernel   pull Debian linux-image-amd64 .deb, extract vmlinuz
#   make initramfs             build the 63 MB cpio initramfs
#   make boot-real             boot guest under QEMU, capture attestation
#   make verify                run verifier on captured attestation
#   make real-all              chain: fetch-debian-kernel + initramfs + boot-real + verify
#
# dev_tpm2 path (the paper-aligned flow: real kernel, HB release,
# ~tpm2@2.0a device, /~tpm2@2.0a/attestation-json, verifier_hb.py):
#   make buildroot         build the LapEE kernel + minimal rootfs
#   make hb-release        build the HB release with the dev_tpm2 NIF
#   make hb-initramfs      assemble the guest image
#   make hb-all            initramfs + boot + verify, one envelope
#   make hb-acceptance     three-envelope positive battery
#   make hb-tamper-test    seven-way verifier completeness probe
#   make hb-interpret-demo end-to-end demo of ~tpm-interpret@1.0

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

tpm-start:
	./scripts/swtpm.sh start

tpm-stop:
	./scripts/swtpm.sh stop

tpm-status:
	./scripts/swtpm.sh status

reset-tpm:
	./scripts/swtpm.sh reset && ./scripts/swtpm.sh start

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
	@./scripts/build-initramfs.sh

boot-real:
	@./scripts/boot-real.sh

real-all: builders fetch-debian-kernel nif-linux initramfs boot-real verify

# ----- HyperBEAM-as-userspace path (dev_tpm2) -----
#
# Full LapEE stack:
#   - Debian kernel (TPM + IMA builtin)
#   - Our initramfs containing a full HB release (edge branch + lapee
#     profile, with dev_tpm2 NIF preloaded)
#   - `on.start' hook extends PCR 15 with `hb_message:id(node_msg)'
#   - HB HTTP server answers `/~tpm2@2.0a/attestation' with the full envelope

.PHONY: hb-release hb-initramfs hb-boot hb-verify hb-all hb-acceptance hb-tamper-test hb-interpret-demo hb-cross-node-verify

hb-release:
	./scripts/build-hb-release.sh

hb-initramfs:
	./scripts/build-initramfs-hb.sh

hb-boot:
	./scripts/boot-hb.sh

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
hb-acceptance:
	./scripts/hb-acceptance.sh

# hb-tamper-test: starting from a valid envelope in out/evidence/,
# produces seven targeted byte-flips and asserts the verifier rejects
# each at the expected check. Completeness pair to hb-acceptance.
hb-tamper-test:
	./scripts/hb-tamper-test.sh

# hb-interpret-demo: end-to-end demo of ~tpm-interpret@1.0, feeding
# a saved envelope to /~tpm-interpret@1.0/verify over HTTP and
# pretty-printing the rich per-section interpretation.
hb-interpret-demo:
	./scripts/hb-interpret-demo.sh

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
	./scripts/boot-buildroot.sh

clean:
	rm -rf work/lapee-tpm-linux work/initramfs-mini.cpio.gz out/*.json out/test-ek.* out/node-address.txt
	@echo "cleaned; run 'make reset-tpm' to wipe TPM state too."
