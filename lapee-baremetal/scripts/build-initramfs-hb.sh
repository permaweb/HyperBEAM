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
rm -rf $HB/bin/priv
rm -rf $HB/lib/hb-0.0.1/src
rm -rf $HB/lib/hb-0.0.1/priv/html
rm -rf $HB/lib/hb-0.0.1/priv/static
rm -rf $HB/lib/hb-0.0.1/priv/tpm-interpret/fixtures
# Trim OTP docs/examples from every shipped lib.
for d in $HB/lib/*; do
    rm -rf "$d/doc" "$d/examples" "$d/man"
done
# Strip BEAM + every shared lib the NIFs ship.
find $HB/erts-*/bin/beam.smp \
     $HB/lib -name '*.so' -type f 2>/dev/null \
    | xargs -r strip -s 2>/dev/null || true
# Report so we can see v1.2 slim progress on every rebuild.
echo "--- post-slim HB size ---"
du -sh $HB /ramfs/usr/lib 2>/dev/null || true

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

# Our init.
cp /init-hb /ramfs/init
chmod +x /ramfs/init

du -sh /ramfs
SH

rm -rf /tmp/lapee-hb-ramfs && mkdir /tmp/lapee-hb-ramfs
docker cp lapee-hb-mini:/ramfs /tmp/lapee-hb-ramfs
docker rm -f lapee-hb-mini >/dev/null

cd /tmp/lapee-hb-ramfs/ramfs && find . | cpio -o -H newc 2>/dev/null | gzip -1 > "$LAPEE/work/initramfs-hb.cpio.gz"
ls -lh "$LAPEE/work/initramfs-hb.cpio.gz"
