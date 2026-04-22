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
# busybox-static gives us udhcpc + `ip' handling; iproute2 provides
# the full `ip' binary; linux-image-generic is the source of BOTH
# (a) the kernel we boot on real hardware — the pre-built Buildroot
# kernel we had lacks USB-Ethernet drivers so tethered phones don't
# show up as NICs — AND (b) the matching kernel modules we copy into
# the initramfs for modprobe-at-boot. The builder image is Ubuntu
# 24.04 Noble so the package name is `linux-image-generic' (Debian
# calls the equivalent `linux-image-amd64').
docker exec lapee-hb-mini bash -c "apt-get update -qq 2>&1 | tail -1 && apt-get install -y -qq busybox-static iproute2 linux-image-generic kmod 2>&1 | tail -1"

# Copy HB release into the container.
docker cp "$HB_REL" lapee-hb-mini:/opt/hb
# Copy enforced config.
docker cp "$LAPEE/../../lapee-dev-tpm2/config/lapee-enforced.flat" \
    lapee-hb-mini:/opt/lapee-enforced.flat 2>/dev/null || \
    docker cp "$(git -C /Users/sam/src/hyperbeam/.claude/worktrees/lapee-dev-tpm2 rev-parse --show-toplevel)/config/lapee-enforced.flat" \
    lapee-hb-mini:/opt/lapee-enforced.flat
# Copy our init + DHCP hook.
docker cp "$LAPEE/initramfs-hb/init" lapee-hb-mini:/init-hb
docker cp "$LAPEE/initramfs-hb/udhcpc.script" \
    lapee-hb-mini:/etc-udhcpc.script

docker exec -i lapee-hb-mini bash <<'SH'
set -e
mkdir -p /ramfs/bin /ramfs/sbin /ramfs/etc/lapee /ramfs/lib/x86_64-linux-gnu /ramfs/lib64 \
    /ramfs/usr/bin /ramfs/usr/sbin /ramfs/usr/local/bin /ramfs/usr/local/lib \
    /ramfs/usr/lib/ssl /ramfs/usr/lib/hyperbeam \
    /ramfs/proc /ramfs/sys /ramfs/dev /ramfs/tmp /ramfs/run /ramfs/out /ramfs/mnt

# busybox — includes udhcpc + insmod/modprobe/depmod applets we
# rely on for real-hardware network + kernel-module loading.
cp /usr/bin/busybox /ramfs/bin/busybox
cd /ramfs/bin
for cmd in sh mount umount ls cat cp mv rm mkdir ln chmod chown echo grep sed awk find hostname \
           ifconfig dmesg ps head tail wc tar gzip sleep stat uname date touch test mknod reboot poweroff \
           vi env printf sync tee base64 udhcpc insmod modprobe rmmod lsmod depmod tr; do
    ln -sf busybox $cmd
done
cd /

# iproute2
cp /usr/sbin/ip /ramfs/sbin/ip

# DHCP hook (called by udhcpc on `bound' / `renew' / etc).
mkdir -p /ramfs/etc
cp /etc-udhcpc.script /ramfs/etc/udhcpc.script
chmod +x /ramfs/etc/udhcpc.script

# Kernel modules for USB-Ethernet + common Ethernet PHYs. On
# real Framework hardware plugging a USB-C Ethernet dongle is
# the simplest path to DHCP; these modules cover the common
# chipsets. The full Debian linux-image-amd64 package is ~80 MB
# of modules; we only copy the ~2 MB net-module subset.
KVER=$(ls /lib/modules | head -1)
if [ -n "$KVER" ]; then
    mkdir -p /ramfs/lib/modules/$KVER/kernel/drivers/net
    for subdir in usb ethernet; do
        src=/lib/modules/$KVER/kernel/drivers/net/$subdir
        if [ -d "$src" ]; then
            mkdir -p /ramfs/lib/modules/$KVER/kernel/drivers/net/$subdir
            # Prune to chipsets we care about to keep the initramfs
            # small. (Full USB-net dir is ~500KB; all ethernet PHYs
            # together are ~8MB — the second is worth pruning.)
            case $subdir in
                usb)
                    cp -r $src/* \
                        /ramfs/lib/modules/$KVER/kernel/drivers/net/$subdir/
                    ;;
                ethernet)
                    # Intel + Realtek cover ~90% of laptop wired NICs.
                    for vendor in intel realtek; do
                        [ -d "$src/$vendor" ] && \
                            cp -r "$src/$vendor" \
                            /ramfs/lib/modules/$KVER/kernel/drivers/net/$subdir/
                    done
                    ;;
            esac
        fi
    done
    # Copy module metadata so modprobe works (depmod output).
    cp /lib/modules/$KVER/modules.dep \
       /lib/modules/$KVER/modules.alias \
       /lib/modules/$KVER/modules.symbols \
       /lib/modules/$KVER/modules.builtin \
       /lib/modules/$KVER/modules.order \
        /ramfs/lib/modules/$KVER/ 2>/dev/null || true
    # Rebuild modules.dep against our pruned tree so the
    # busybox modprobe we ship can resolve aliases.
    depmod -b /ramfs $KVER 2>/dev/null || true
fi

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

# OpenSSL binary for EK cert issuance.
cp /usr/bin/openssl /ramfs/usr/bin/openssl
cp /etc/ssl/openssl.cnf /ramfs/usr/lib/ssl/openssl.cnf
cp -r /etc/ssl /ramfs/etc/

# HyperBEAM release.
cp -r /opt/hb/. /ramfs/usr/lib/hyperbeam/
# Make sure bin/hb is executable.
chmod +x /ramfs/usr/lib/hyperbeam/bin/hb 2>/dev/null || true

# --- slim the release for the guest initramfs -------------------------
# See HARDENING.md for the full rationale. The release tree we embed is
# ~180 MB out of the box; the guest doesn't need:
#
#   * the `bin/priv/' overlay copy of `priv/' (relx puts priv at
#     `lib/hb-0.0.1/priv/' already; the overlay duplicates ~25 MB of
#     NIFs + static HTML);
#   * the `lib/hb-0.0.1/src/' source tree (ships with .erl sources for
#     debug; BEAM only needs the .beam files in ebin/);
#   * the hyperbuddy web UI bundle (~15 MB of JS/CSS — the LapEE guest
#     serves attestation, not a browser UI);
#   * full debug symbols on `beam.smp' (~35 MB stripped).
rm -rf /ramfs/usr/lib/hyperbeam/bin/priv   # overlay duplicate of lib priv/
rm -rf /ramfs/usr/lib/hyperbeam/lib/hb-0.0.1/src
rm -rf /ramfs/usr/lib/hyperbeam/lib/hb-0.0.1/priv/html
rm -rf /ramfs/usr/lib/hyperbeam/lib/hb-0.0.1/priv/static
# Strip BEAM and any shared libs the NIFs ship.
find /ramfs/usr/lib/hyperbeam/erts-*/bin/beam.smp \
     /ramfs/usr/lib/hyperbeam/lib -name '*.so' -type f 2>/dev/null \
    | xargs -r strip -s 2>/dev/null || true

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

# Copy the Ubuntu generic kernel out so the USB-image builder
# boots a kernel whose version matches the modules we just
# baked into the initramfs. Overwrites any stale vmlinuz-lapee
# from the Buildroot flow (which lacked USB-Ethernet drivers).
KVER_OUT=$(docker exec lapee-hb-mini bash -c \
    'basename /lib/modules/*' | head -1)
if [ -n "$KVER_OUT" ]; then
    docker cp "lapee-hb-mini:/boot/vmlinuz-$KVER_OUT" \
        "$LAPEE/work/vmlinuz-lapee"
    echo "kernel: $LAPEE/work/vmlinuz-lapee (version $KVER_OUT)"
fi

docker rm -f lapee-hb-mini >/dev/null

cd /tmp/lapee-hb-ramfs/ramfs && find . | cpio -o -H newc 2>/dev/null | gzip -1 > "$LAPEE/work/initramfs-hb.cpio.gz"
ls -lh "$LAPEE/work/initramfs-hb.cpio.gz" "$LAPEE/work/vmlinuz-lapee"
