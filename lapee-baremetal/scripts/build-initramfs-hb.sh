#!/usr/bin/env bash
# build-initramfs-hb.sh — assemble a LapEE guest initramfs containing a
# FULL HyperBEAM release (edge branch + lapee profile NIF) + enforced
# config + slim init.
#
# Requires:
#   - build-hyperbeam/src-edge/_build/lapee/rel/hb  (the HB release)
#   - config/lapee-enforced.flat in the HB tree
#   - the lapee-hyperbeam-builder Docker image for library harvesting
#
# Output: work/initramfs-hb.cpio.gz
set -euo pipefail
cd "$(dirname "$0")/.."
LAPEE=$(pwd)

HB_REL="$LAPEE/build-hyperbeam/src-edge/_build/lapee/rel/hb"
if [[ ! -d "$HB_REL" ]]; then
    echo "missing HB release at $HB_REL; run the HB build first" >&2
    exit 1
fi

docker rm -f lapee-hb-mini 2>/dev/null || true
docker run -d --platform=linux/amd64 --name lapee-hb-mini \
    lapee-hyperbeam-builder:latest sleep infinity >/dev/null
# Install busybox + iproute2 + WiFi userspace (wpa_supplicant, iw).
# Firmware blobs for Intel AX210 + MediaTek MT7922 ship in Ubuntus
# `linux-firmware' package (monolithic ~1 GB installed; we only
# copy the ~8 MB we actually need from /lib/firmware/ further
# down). `iw' lives in Ubuntus universe component which is enabled
# by default on the builder image.
docker exec -i lapee-hb-mini bash <<'SH_APT'
set -e
apt-get update -qq 2>&1 | tail -2
apt-get install -y -qq busybox-static iproute2 wpasupplicant iw \
        linux-firmware zstd 2>&1 | tail -2
SH_APT

# Copy HB release into the container.
docker cp "$HB_REL" lapee-hb-mini:/opt/hb
# Copy enforced config. Ships in this repo at config/lapee-enforced.flat;
# early batches pulled from an external worktree, now unified.
docker cp "$LAPEE/../config/lapee-enforced.flat" \
    lapee-hb-mini:/opt/lapee-enforced.flat
# Copy the init + splash + DHCP-hook + ASCII logo.
docker cp "$LAPEE/initramfs-hb/init"              lapee-hb-mini:/init-hb
docker exec lapee-hb-mini mkdir -p /ramfs-src
docker cp "$LAPEE/initramfs-hb/logo.ascii"        lapee-hb-mini:/ramfs-src/logo.ascii
docker cp "$LAPEE/initramfs-hb/lapee-splash"      lapee-hb-mini:/ramfs-src/lapee-splash
docker cp "$LAPEE/initramfs-hb/lapee-dhcp-hook"   lapee-hb-mini:/ramfs-src/lapee-dhcp-hook

docker exec -i lapee-hb-mini bash <<'SH'
set -e
mkdir -p /ramfs/bin /ramfs/sbin /ramfs/etc/lapee /ramfs/lib/x86_64-linux-gnu /ramfs/lib64 \
    /ramfs/usr/bin /ramfs/usr/sbin /ramfs/usr/local/bin /ramfs/usr/local/lib \
    /ramfs/usr/lib/ssl /ramfs/usr/lib/hyperbeam \
    /ramfs/proc /ramfs/sys /ramfs/dev /ramfs/tmp /ramfs/run /ramfs/out /ramfs/mnt

# busybox
cp /usr/bin/busybox /ramfs/bin/busybox
cd /ramfs/bin
for cmd in sh mount umount ls cat cp mv rm mkdir ln chmod chown echo grep sed awk find hostname \
           ifconfig dmesg ps head tail wc tar gzip sleep stat uname date touch test mknod reboot poweroff \
           vi env printf sync tee base64 \
           udhcpc stty; do
    ln -sf busybox $cmd
done
cd /

# iproute2
cp /usr/sbin/ip /ramfs/sbin/ip

# v1.2.2 WiFi userspace. wpa_supplicant does WPA2/WPA3 auth and
# drives the kernel drivers via nl80211. iw is a thin debugging
# tool (scan, station info) useful when the network isn't
# associating and we need to see what the card sees.
#
# Credentials are NOT on the kernel cmdline. Init parses a strictly-
# validated `/EFI/boot/wifi.conf' off the ESP (unmeasured, written
# by the operator on any host that can mount FAT32). The format is
# exactly two lines: SSID\nPASSWORD. Anything else is rejected by
# the parser, so wpa_supplicant.conf injection through the ESP file
# is not possible.
for bin in /usr/sbin/wpa_supplicant /sbin/wpa_supplicant; do
    [ -x "$bin" ] && cp "$bin" /ramfs/sbin/wpa_supplicant && break
done
for bin in /usr/sbin/iw /sbin/iw; do
    [ -x "$bin" ] && cp "$bin" /ramfs/sbin/iw && break
done

# WiFi firmware. Ubuntu 24.04 ships all firmware zstd-compressed as
# `*.ucode.zst' under /lib/firmware/. Our kernel doesn't enable
# CONFIG_FW_LOADER_COMPRESS_ZSTD (would need another rebuild), so
# we decompress here and ship raw .ucode files. Trade-off: larger
# initramfs (raw is ~1.6x zstd), avoided rebuild.
#
# Versioning: iwlwifi probes `-73` then `-72` then `-66` then `-59';
# MediaTek has a single version per chip. We ship the latest
# version of each so the driver's first-try succeeds; older
# fallback versions are skipped to save ~3 MB.
mkdir -p /ramfs/lib/firmware/mediatek
FW_SRC=/lib/firmware
[ -d "$FW_SRC" ] || FW_SRC=/usr/lib/firmware

dec() {
    # dec <.zst file> <target dir>
    #   Decompress with zstd; strip `.zst' suffix.
    [ -f "$1" ] || return 1
    _base=$(basename "$1" .zst)
    zstd -d -q "$1" -o "$2/$_base" 2>/dev/null \
        || cp "$1" "$2/$(basename "$1")"
}

if [ -d "$FW_SRC" ]; then
    # Intel AX210 (Framework 13 Intel variant):
    #   iwlwifi-ty-a0-gf-a0-73.ucode + matching .pnvm
    dec "$FW_SRC/iwlwifi-ty-a0-gf-a0-73.ucode.zst" /ramfs/lib/firmware \
        || dec "$FW_SRC/iwlwifi-ty-a0-gf-a0-72.ucode.zst" /ramfs/lib/firmware
    dec "$FW_SRC/iwlwifi-ty-a0-gf-a0.pnvm.zst"           /ramfs/lib/firmware
    # Intel AX211 (Raptor Lake P refresh):
    #   iwlwifi-ma-b0-gf-a0-89.ucode  + matching .pnvm
    dec "$FW_SRC/iwlwifi-ma-b0-gf-a0-89.ucode.zst"       /ramfs/lib/firmware \
        || dec "$FW_SRC/iwlwifi-ma-b0-gf-a0-86.ucode.zst" /ramfs/lib/firmware
    dec "$FW_SRC/iwlwifi-ma-b0-gf-a0.pnvm.zst"           /ramfs/lib/firmware
    # Regulatory DB (wireless compliance; iwlwifi refuses to
    # associate without it on channels above 2.4 GHz-world-regdom).
    for plain in "$FW_SRC/regulatory.db" "$FW_SRC/regulatory.db.p7s"; do
        [ -f "$plain" ] && cp "$plain" /ramfs/lib/firmware/
    done
    # MediaTek MT7922 (Framework 13 AMD variant):
    dec "$FW_SRC/mediatek/WIFI_MT7922_patch_mcu_1_1_hdr.bin.zst" \
        /ramfs/lib/firmware/mediatek
    dec "$FW_SRC/mediatek/WIFI_RAM_CODE_MT7922_1.bin.zst" \
        /ramfs/lib/firmware/mediatek
fi
echo "--- firmware shipped ---"
ls -la /ramfs/lib/firmware/ /ramfs/lib/firmware/mediatek/ 2>/dev/null
du -sh /ramfs/lib/firmware/ 2>/dev/null

# Shared libraries needed by HB (OTP + libtss2 + libcrypto + libssl + ...).
LIB=/ramfs/lib/x86_64-linux-gnu
for lib in libc.so.6 libc_malloc_debug.so.0 \
           libcrypto.so.3 libssl.so.3 \
           libtss2-esys.so.0 libtss2-mu.so.0 libtss2-tctildr.so.0 libtss2-rc.so.0 libtss2-sys.so.1 \
           libtss2-tcti-swtpm.so.0 \
           libpthread.so.0 libdl.so.2 libm.so.6 libz.so.1 libresolv.so.2 \
           libtinfo.so.6 libncursesw.so.6 \
           libstdc++.so.6 libgcc_s.so.1 libgmp.so.10 \
           libmnl.so.0 libbsd.so.0 libmd.so.0 libcap.so.2 \
           libnl-3.so.200 libnl-genl-3.so.200 libnl-route-3.so.200 \
           libdbus-1.so.3 libpcsclite.so.1 \
           libgcrypt.so.20 libgpg-error.so.0 liblzma.so.5 libzstd.so.1 \
           liblz4.so.1 libsystemd.so.0; do
    if [ -e /lib/x86_64-linux-gnu/$lib ]; then cp -L /lib/x86_64-linux-gnu/$lib $LIB/; fi
done
cp -L /usr/lib/x86_64-linux-gnu/libtss2-tcti-device.so.0 $LIB/ 2>/dev/null || true
cp -L /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 $LIB/
ln -sf /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 /ramfs/lib64/ld-linux-x86-64.so.2

# The openssl BINARY is deliberately NOT copied into the initramfs.
# Earlier revisions used it at boot to synthesize a "LapEE Test EK"
# certificate via `openssl x509 -req'. That path has been ripped out:
# dev_tpm2 now reads the real EK certificate from TPM NV storage and
# never fabricates one. The OpenSSL SHARED LIBRARIES (libcrypto /
# libssl, pulled in above) stay -- HyperBEAM's crypto NIFs still need
# them; what is gone is the userspace CLI tool + its default config.

# v1.2 boot splash + DHCP hook. These get called from /init:
#   - lapee-splash               render centred HB ASCII + status
#   - lapee-dhcp-hook            udhcpc action script; claims the
#                                first-to-lease interface as the
#                                default route + re-renders the
#                                splash with the node URL.
# logo.ascii is the HyperBEAM figlet-style art the splash centres.
cp /ramfs-src/logo.ascii          /ramfs/etc/lapee/logo.ascii
cp /ramfs-src/lapee-splash        /ramfs/usr/local/bin/lapee-splash
cp /ramfs-src/lapee-dhcp-hook     /ramfs/usr/local/bin/lapee-dhcp-hook
chmod +x /ramfs/usr/local/bin/lapee-splash \
         /ramfs/usr/local/bin/lapee-dhcp-hook

# HyperBEAM release.
cp -r /opt/hb/. /ramfs/usr/lib/hyperbeam/
# Make sure bin/hb is executable.
chmod +x /ramfs/usr/lib/hyperbeam/bin/hb 2>/dev/null || true

# --- slim the release for the guest initramfs (v1.2) ----------------
# The bare release is ~180 MB out of the box; the runtime guest only
# needs a fraction. Remove:
#
#   * `bin/priv/' -- overlay duplicate of `lib/hb-0.0.1/priv/' (~25 MB)
#   * `lib/hb-0.0.1/src/' -- .erl sources only needed for debug builds
#   * `lib/hb-0.0.1/priv/html` / `priv/static` -- hyperbuddy web UI
#     (~15 MB JS/CSS; LapEE serves attestation, not a browser UI)
#   * `priv/tpm-interpret/fixtures/` -- test vectors for the parser
#     eunit suite (~40 MB of TCG event-log samples), consumed only by
#     `rebar3 eunit' on the verifier host; never accessed at runtime.
#   * `lib/*/doc/`, `lib/*/examples/`, `lib/*/man/` -- docs / examples
#     that ship with OTP and some deps; not needed on a thin guest.
#   * `.beam.debug_info`-heavy debug symbols on `beam.smp` (~35 MB
#     stripped).
HB=/ramfs/usr/lib/hyperbeam

# ---- v1.2.1 aggressive slim (post-demo task list) ------------------
# Target: initramfs << 20 MB compressed (was ~60 MB in batch 14).
# Three categories removed:
#   (a) verifier-side-only artefacts: priv/tpm-interpret/ is the static
#       JSON DB + root CAs consumed by dev_tpm_interpret on the MAC.
#       The guest PRODUCES envelopes; it does not INTERPRET them, so
#       none of this is needed at runtime on the Framework. 1.3 MB saved.
#   (b) static UI: priv/html (already removed) + priv/static + the
#       index-page render path. LapEE serves /~tpm2@2.0a/attestation,
#       not a browser UI. Set via config.json -> render_index = false.
#   (c) unused devices: dev_snp (AMD SEV-SNP, not applicable on
#       Ryzen-TPM), dev_hyperbuddy (serves the static UI we just
#       removed). Corresponding .beam files deleted so HB's
#       preload_device skips them. priv/crates/ (Rust NIFs for
#       dev_snp) gone entirely.
#   (d) OTP weight: .erl sources on EVERY shipped lib (was only
#       hb-0.0.1); OTP boot-tools binaries not needed at runtime
#       (erlc, dialyzer, typer, ct_run); BEAM + every .so
#       aggressively stripped.
rm -rf $HB/bin/priv
rm -rf $HB/lib/hb-0.0.1/priv/html
rm -rf $HB/lib/hb-0.0.1/priv/static
rm -rf $HB/lib/hb-0.0.1/priv/tpm-interpret
# priv/crates/ retained: `dev_snp_nif.beam' has an `-on_load'
# that erlang:load_nifs the Rust .so out of this directory. Even
# when dev_snp isn't in preloaded_devices, on_load fires at module
# load time -- which is driven by the OTP boot script before any
# config is read. Removing the .so triggers a load failure that
# cascades exactly like removing the .beam itself did (see above).
# Saving these ~10 MB requires a LapEE-specific release profile,
# not a post-build rm.
# DO NOT delete dev_snp*/dev_hyperbuddy* .beam files here.
#
# Earlier versions of this slim step removed them on the logic
# "the lapee.json preloaded_devices list excludes them, so they're
# dead code". That logic was wrong: the OTP release boot script
# (releases/0.0.1/hb.boot, compiled at hb-release time by reltool)
# statically names every module in every listed application. On VM
# startup, erlexec's embedded boot loads every such module before
# any config (lapee.json / hb_opts) gets a chance to run. A missing
# module ==> `load_failed' ==> `Runtime terminating during boot' ==>
# init exits ==> kernel panic "Attempted to kill init".
#
# The correct slim is behavioural (don't PRELOAD them via config;
# our lapee.json already does that), not byte-surgical. Saving a
# few hundred KB isn't worth breaking boot. Leave the .beam files
# in lib/hb-0.0.1/ebin; they'll just never be dispatched because
# the device list doesn't reference them.
#
# If we ever want to strip them for real, the path is: regenerate
# the .boot/.script for a LapEE-specific release profile that
# omits the module manifest entries. That's a rebar3 relx change,
# not a post-build find -delete.
# .erl sources across all libs -- were only hb-0.0.1 before. None of
# these are loaded at runtime (compiled .beam is the runtime artefact).
find $HB/lib -type d -name src -exec rm -rf {} + 2>/dev/null || true
# Trim OTP docs/examples/include-dev from every shipped lib.
for d in $HB/lib/*; do
    rm -rf "$d/doc" "$d/examples" "$d/man" "$d/c_src"
done
# Build-time tools that don't belong in a runtime release. `erl` +
# `erlexec` + `run_erl` + `to_erl` + `erl_call` + `erl_child_setup` +
# `inet_gethost` + `heart` + `epmd` + `beam.smp` + `dyn_erl` +
# `start`/`erl.src`/`start_erl.src`/`start.src` are retained.
for tool in ct_run dialyzer typer erlc escript; do
    find $HB/erts-* -name "$tool" -delete 2>/dev/null || true
done
# Strip BEAM aggressively + every shared lib the NIFs ship.
find $HB/erts-*/bin/beam.smp \
     $HB/lib -name '*.so' -type f 2>/dev/null \
    | xargs -r strip --strip-all 2>/dev/null || true
# Report per-lib so any future regression to HB footprint is visible.
echo "--- post-slim HB size ---"
du -sh $HB /ramfs/usr/lib 2>/dev/null || true
echo "--- top 10 libs by size after slim ---"
du -sh $HB/lib/*/ 2>/dev/null | sort -hr | head -10
echo "--- top erts binaries ---"
du -sh $HB/erts-*/bin/* 2>/dev/null | sort -hr | head -5

# LapEE-specific sys.config overlay. The OTP os_mon app (disksup,
# memsup, cpu_sup, os_sup) expects a host-ish filesystem, and under the
# thin initramfs its probes crash fast enough to trip the supervisor's
# max restart intensity and bring the whole VM down. Disable them.
cat > /ramfs/usr/lib/hyperbeam/releases/0.0.1/sys.config <<'CFG'
[
    {prometheus, [
        {cowboy_instrumenter, [
            {duration_buckets,
                [0.001, 0.01, 0.1, 0.25, 0.5, 0.75, 1, 2, 4, 10, 30, 60]}
        ]}
    ]},
    {os_mon, [
        {start_disksup, false},
        {start_memsup,  false},
        {start_cpu_sup, false},
        {start_os_sup,  false}
    ]}
].
CFG

# Enforced LapEE config.
cp /opt/lapee-enforced.flat /ramfs/etc/lapee/lapee-enforced.flat

# LapEE-specific HB config.json. Loaded at boot via HB_CONFIG in the
# init script. Disables the hyperbuddy index-page render (no
# priv/static in this image anyway), and narrows preloaded_devices
# to the ~25 the LapEE appliance actually uses -- excluding
# dev_snp (AMD SEV-SNP, not Ryzen-TPM), dev_hyperbuddy (browser UI),
# dev_wasi / dev_wasm / dev_genesis_wasm (no WASM on this deploy),
# dev_green_zone (SEV-SNP-adjacent), dev_profile / dev_monitor /
# dev_rate_limit (runtime ops tooling, not demo path), and
# dev_copycat / dev_delegated_compute / dev_poda / dev_bundler /
# dev_json_iface / dev_codec_ans104 / dev_codec_tx (non-demo).
cat > /ramfs/etc/lapee/lapee.json <<'JSON'
{
    "render_index_page": false,
    "preloaded_devices": [
        {"name": "apply@1.0",        "module": "dev_apply"},
        {"name": "arweave@2.9",      "module": "dev_arweave"},
        {"name": "auth-hook@1.0",    "module": "dev_auth_hook"},
        {"name": "b32-name@1.0",     "module": "dev_b32_name"},
        {"name": "blacklist@1.0",    "module": "dev_blacklist"},
        {"name": "cache@1.0",        "module": "dev_cache"},
        {"name": "compute@1.0",      "module": "dev_cu"},
        {"name": "cookie@1.0",       "module": "dev_codec_cookie"},
        {"name": "cron@1.0",         "module": "dev_cron"},
        {"name": "faff@1.0",         "module": "dev_faff"},
        {"name": "flat@1.0",         "module": "dev_codec_flat"},
        {"name": "gzip@1.0",         "module": "dev_gzip"},
        {"name": "hook@1.0",         "module": "dev_hook"},
        {"name": "httpsig@1.0",      "module": "dev_codec_httpsig"},
        {"name": "http-auth@1.0",    "module": "dev_codec_http_auth"},
        {"name": "json@1.0",         "module": "dev_codec_json"},
        {"name": "local-name@1.0",   "module": "dev_local_name"},
        {"name": "location@1.0",     "module": "dev_location"},
        {"name": "lookup@1.0",       "module": "dev_lookup"},
        {"name": "lua@5.3a",         "module": "dev_lua"},
        {"name": "manifest@1.0",     "module": "dev_manifest"},
        {"name": "message@1.0",      "module": "dev_message"},
        {"name": "meta@1.0",         "module": "dev_meta"},
        {"name": "p4@1.0",           "module": "dev_p4"},
        {"name": "relay@1.0",        "module": "dev_relay"},
        {"name": "router@1.0",       "module": "dev_router"},
        {"name": "scheduler@1.0",    "module": "dev_scheduler"},
        {"name": "simple-pay@1.0",   "module": "dev_simple_pay"},
        {"name": "stack@1.0",        "module": "dev_stack"},
        {"name": "structured@1.0",   "module": "dev_codec_structured"},
        {"name": "tpm2@2.0a",        "module": "dev_tpm2"},
        {"name": "tpm-interpret@1.0","module": "dev_tpm_interpret"}
    ]
}
JSON

# Our init.
cp /init-hb /ramfs/init
chmod +x /ramfs/init

du -sh /ramfs
SH

rm -rf /tmp/lapee-hb-ramfs && mkdir /tmp/lapee-hb-ramfs
docker cp lapee-hb-mini:/ramfs /tmp/lapee-hb-ramfs
docker rm -f lapee-hb-mini >/dev/null

# v1.2.1 slim: switch cpio compression from gzip to zstd (kernel has
# CONFIG_RD_ZSTD=y in our fragment). zstd -19 at these sizes is
# 30-40 % smaller than gzip -1 for the same uncompressed corpus.
# Kernel-side decompression is faster too; the "gzip ->  zstd" swap
# actually saves a few ms of boot time as a bonus.
cd /tmp/lapee-hb-ramfs/ramfs
if command -v zstd >/dev/null 2>&1; then
    find . | cpio -o -H newc 2>/dev/null | zstd -19 -T0 --ultra -q > "$LAPEE/work/initramfs-hb.cpio.zst"
    # Kernel looks for the initramfs at `initramfs-hb.cpio.gz' by
    # default (Makefile INITRAMFS var); keep a gzip-wrapped copy too
    # so changes here don't break unrelated build steps. The real
    # artefact shipped in the UKI is the .zst one (see build-usb-
    # image.sh, which is taught to prefer .zst when present).
    find . | cpio -o -H newc 2>/dev/null | gzip -9 > "$LAPEE/work/initramfs-hb.cpio.gz"
    ls -lh "$LAPEE/work/initramfs-hb.cpio.zst" "$LAPEE/work/initramfs-hb.cpio.gz"
else
    echo "zstd not installed on host; falling back to gzip -9" >&2
    find . | cpio -o -H newc 2>/dev/null | gzip -9 > "$LAPEE/work/initramfs-hb.cpio.gz"
    ls -lh "$LAPEE/work/initramfs-hb.cpio.gz"
fi
