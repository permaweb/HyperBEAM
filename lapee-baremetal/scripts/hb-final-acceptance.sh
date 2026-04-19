#!/usr/bin/env bash
# hb-final-acceptance.sh — run the whole LapEE acceptance battery
# from cold, suitable for regression testing or PR review.
#
#   1. make hb-release     (auto-seeds src-edge from parent HB repo)
#   2. make hb-initramfs
#   3. make hb-acceptance  (three-envelope positive battery)
#   4. make hb-tamper-test (seven-way verifier completeness probe)
#   5. boot with --keep-alive; run hb-interpret-demo against the
#      live node; tear down.
#   6. refresh out/evidence/ from the last baseline run
#
# Prints a one-line summary per step and a final PASS/FAIL verdict.
# Exits 0 iff every step succeeds. Intended to be callable from CI
# or by a reviewer running "does this PR actually work on my box?".
set -euo pipefail
cd "$(dirname "$0")/.."

RESULTS=()

run_step() {
    local name=$1
    shift
    echo ""
    echo "============================================================"
    echo "=== $name"
    echo "============================================================"
    if "$@"; then
        RESULTS+=("PASS: $name")
    else
        RESULTS+=("FAIL: $name")
        return 1
    fi
}

# 1. HB release
run_step "hb-release" make hb-release

# 2. Initramfs
run_step "hb-initramfs" make hb-initramfs

# 3. Three-envelope acceptance battery (positive)
run_step "hb-acceptance" make hb-acceptance

# 4. Seven-way tamper-rejection battery (negative)
run_step "hb-tamper-test" make hb-tamper-test

# 5. Boot with --keep-alive, run interpret-demo, tear down
echo ""
echo "============================================================"
echo "=== hb-interpret-demo (live)"
echo "============================================================"
# boot-hb.sh already captured out/evidence when running hb-acceptance;
# start the guest in --keep-alive mode for the interpret-demo.
./scripts/boot-hb.sh --keep-alive --log /tmp/lapee-hb-guest.log \
    >/tmp/boot-ka.log 2>&1 || {
    RESULTS+=("FAIL: hb-interpret-demo (boot)")
    cat /tmp/boot-ka.log
    echo ""
    printf '%s\n' "${RESULTS[@]}"
    exit 1
}
DEMO_OK=1
if ./scripts/hb-interpret-demo.sh; then
    RESULTS+=("PASS: hb-interpret-demo")
else
    DEMO_OK=0
    RESULTS+=("FAIL: hb-interpret-demo")
fi
# Tear down guest.
pkill -f qemu-system-x86_64 2>/dev/null || true
pkill -f swtpm 2>/dev/null || true

echo ""
echo "============================================================"
echo "=== ACCEPTANCE SUMMARY"
echo "============================================================"
printf '%s\n' "${RESULTS[@]}"

if printf '%s\n' "${RESULTS[@]}" | grep -q '^FAIL'; then
    echo ""
    echo "VERDICT: FAIL"
    exit 1
fi
echo ""
echo "VERDICT: PASS"
