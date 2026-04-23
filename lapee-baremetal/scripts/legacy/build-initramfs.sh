#!/usr/bin/env bash
# build-initramfs.sh — assemble the LapEE guest initramfs.
#
# Output: work/initramfs-mini.cpio.gz (~63 MB gz, ~120 MB uncompressed)
# Contains: busybox, Erlang OTP 27, libtss2 + deps, openssl, the compiled
# lapee_tpm NIF, and /init that runs lapee_node.
set -euo pipefail
cd "$(dirname "$0")/.."
LAPEE=$(pwd)

# Precondition: NIF cross-compiled into work/lapee-tpm-linux/priv/
if [[ ! -f work/lapee-tpm-linux/priv/lapee_tpm_nif.so ]]; then
    echo "missing work/lapee-tpm-linux/priv/lapee_tpm_nif.so — run 'make nif-linux'" >&2
    exit 1
fi

docker rm -f lapee-mini 2>/dev/null || true
docker run -d --platform=linux/amd64 --name lapee-mini lapee-hyperbeam-builder:latest sleep infinity >/dev/null
docker exec lapee-mini bash -c "apt-get update -qq 2>&1 | tail -1 && apt-get install -y -qq busybox-static iproute2 2>&1 | tail -1"

docker exec lapee-mini mkdir -p /work
docker cp "$LAPEE/work/lapee-tpm-linux" lapee-mini:/work/lapee-tpm-linux
docker cp "$LAPEE/lapee-tpm/src/lapee_hashpath.erl" lapee-mini:/work/lapee-tpm-linux/src/
docker cp "$LAPEE/lapee-tpm/src/lapee_node.erl"     lapee-mini:/work/lapee-tpm-linux/src/
docker exec lapee-mini bash -c "cd /work/lapee-tpm-linux && rebar3 compile 2>&1 | tail -3"
docker cp "$LAPEE/work/init" lapee-mini:/init

docker exec lapee-mini bash -c '
  set -e
  mkdir -p /ramfs/bin /ramfs/sbin /ramfs/lib/x86_64-linux-gnu /ramfs/lib64 \
           /ramfs/usr/lib/ssl /ramfs/usr/bin /ramfs/usr/local/lib /ramfs/usr/local/bin \
           /ramfs/proc /ramfs/sys /ramfs/dev /ramfs/tmp /ramfs/run /ramfs/etc /ramfs/out /ramfs/work /ramfs/mnt
  cp /usr/bin/busybox /ramfs/bin/busybox
  cd /ramfs/bin
  for cmd in sh mount umount ls cat cp mv rm mkdir ln chmod chown echo grep sed awk find hostname \
             ifconfig dmesg ps head tail wc tar gzip sleep stat uname date touch test mknod reboot poweroff \
             vi env printf sync tee base64; do
    ln -sf busybox $cmd
  done
  cd /
  cp /usr/sbin/ip /ramfs/sbin/ip
  LIB_DIR=/ramfs/lib/x86_64-linux-gnu
  for lib in libc.so.6 libc_malloc_debug.so.0 \
             libcrypto.so.3 libssl.so.3 \
             libtss2-esys.so.0 libtss2-mu.so.0 libtss2-tctildr.so.0 libtss2-rc.so.0 libtss2-sys.so.1 \
             libtss2-tcti-swtpm.so.0 \
             libpthread.so.0 libdl.so.2 libm.so.6 libz.so.1 libresolv.so.2 \
             libtinfo.so.6 libncursesw.so.6 \
             libstdc++.so.6 libgcc_s.so.1 libgmp.so.10 \
             libmnl.so.0 libbsd.so.0 libmd.so.0 libcap.so.2; do
    if [ -e /lib/x86_64-linux-gnu/$lib ]; then cp -L /lib/x86_64-linux-gnu/$lib $LIB_DIR/; fi
  done
  cp -L /usr/lib/x86_64-linux-gnu/libtss2-tcti-device.so.0 $LIB_DIR/ 2>/dev/null
  cp -L /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 $LIB_DIR/
  ln -sf /lib/x86_64-linux-gnu/ld-linux-x86-64.so.2 /ramfs/lib64/ld-linux-x86-64.so.2
  cp -r /usr/local/lib/erlang /ramfs/usr/local/lib/erlang
  find /ramfs/usr/local/lib/erlang -type d \( -name doc -o -name man -o -name examples -o -name src -o -name test \) -exec rm -rf {} + 2>/dev/null || true
  cp /usr/local/bin/erl /ramfs/usr/local/bin/erl
  cp /usr/bin/openssl   /ramfs/usr/bin/openssl
  cp /etc/ssl/openssl.cnf /ramfs/usr/lib/ssl/openssl.cnf
  cp -r /etc/ssl /ramfs/etc/
  # NIF + compiled ebin
  mkdir -p /ramfs/work/lapee-tpm-linux/_build/default/lib/lapee_tpm/priv
  cp -r /work/lapee-tpm-linux/_build/default/lib/lapee_tpm/ebin /ramfs/work/lapee-tpm-linux/_build/default/lib/lapee_tpm/
  cp /work/lapee-tpm-linux/priv/lapee_tpm_nif.so /ramfs/work/lapee-tpm-linux/_build/default/lib/lapee_tpm/priv/
  cp /init /ramfs/init && chmod +x /ramfs/init
'

# Extract to host
rm -rf /tmp/initramfs-min && mkdir /tmp/initramfs-min
docker cp lapee-mini:/ramfs /tmp/initramfs-min
docker rm -f lapee-mini >/dev/null

# cpio
cd /tmp/initramfs-min/ramfs && find . | cpio -o -H newc 2>/dev/null | gzip -1 > "$LAPEE/work/initramfs-mini.cpio.gz"
ls -lh "$LAPEE/work/initramfs-mini.cpio.gz"
