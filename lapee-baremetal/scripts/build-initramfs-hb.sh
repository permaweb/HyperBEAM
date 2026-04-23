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
docker exec lapee-hb-mini bash -c "apt-get update -qq 2>&1 | tail -1 && apt-get install -y -qq busybox-static iproute2 2>&1 | tail -1"

# Copy HB release into the container.
docker cp "$HB_REL" lapee-hb-mini:/opt/hb
# Copy enforced config.
docker cp "$LAPEE/../../lapee-dev-tpm2/config/lapee-enforced.flat" \
    lapee-hb-mini:/opt/lapee-enforced.flat 2>/dev/null || \
    docker cp "$(git -C /Users/sam/src/hyperbeam/.claude/worktrees/lapee-dev-tpm2 rev-parse --show-toplevel)/config/lapee-enforced.flat" \
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

# Shared libraries needed by HB (OTP + libtss2 + libcrypto + libssl + ...).
LIB=/ramfs/lib/x86_64-linux-gnu
for lib in libc.so.6 libc_malloc_debug.so.0 \
           libcrypto.so.3 libssl.so.3 \
           libtss2-esys.so.0 libtss2-mu.so.0 libtss2-tctildr.so.0 libtss2-rc.so.0 libtss2-sys.so.1 \
           libtss2-tcti-swtpm.so.0 \
           libpthread.so.0 libdl.so.2 libm.so.6 libz.so.1 libresolv.so.2 \
           libtinfo.so.6 libncursesw.so.6 \
           libstdc++.so.6 libgcc_s.so.1 libgmp.so.10 \
           libmnl.so.0 libbsd.so.0 libmd.so.0 libcap.so.2; do
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
rm -rf $HB/lib/hb-0.0.1/priv/crates
# Remove the device .beam files for deps we're not running on LapEE
# (dev_snp + dev_hyperbuddy). HB's preload_device tolerates missing
# modules -- they just won't resolve when a request asks for them.
# Keeps the binary footprint honest: if code is in the image, it can
# run; if not, it can't.
for mod in dev_snp dev_snp_lib dev_snp_nif dev_hyperbuddy \
           dev_hyperbuddy_cache dev_hyperbuddy_assets; do
    find $HB/lib -name "$mod.beam" -delete 2>/dev/null || true
done
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
