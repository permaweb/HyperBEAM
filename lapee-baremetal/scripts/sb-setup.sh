#!/usr/bin/env bash
# sb-setup.sh -- generate LapEE operator Secure Boot keys, sign the
# UKI, produce UEFI-enrolment artefacts.
#
# Secure Boot in v1.2.1 is operator-provisioned. Each operator:
#   1. Runs `sb-setup.sh keys' ONCE to generate a fresh PK/KEK/db set
#      per their machine. Keys live under `secureboot/', which is
#      .gitignored -- keys never leave the operator's host.
#   2. Runs `sb-setup.sh sign' after every `make hb-usb-image' to
#      sign the produced UKI with db.key.
#   3. Runs `sb-setup.sh enrol' once per Framework to produce the
#      .auth files that go on a separate FAT-formatted USB stick the
#      Framework reads during Secure Boot setup-mode enrolment.
#
# After enrolment, Secure Boot ON in the Framework BIOS will accept
# the signed UKI and reject anything else.
#
# Rationale (vs. signing with Microsoft shim): shim is a generic
# bootloader and bundles MOK which defeats the single-purpose
# attestation premise. LapEE's operator-owned key means the chain of
# trust terminates at a key the operator physically controls --
# matches the paper's "device identity anchored at the TPM vendor
# root via the EK certificate chain, operator-owned UEFI trust
# anchor via PK/KEK/db" statement.
#
# Required tools:
#   - openssl   (keys + self-sign certs)
#   - sbsign    (sbsigntool package; signs PE+COFF binaries)
#   - sbsigntool's efi-updatevar / cert-to-efi-sig-list / sign-efi-sig-list
#
# On macOS: the SB tooling was pulled from Homebrew, so we ship
# `lapee-tools:latest' (docker/Dockerfile.tools) with sbsigntool +
# efitools preinstalled. `make builders' builds it; this script
# transparently falls back to `docker run' when sbsign / etc. are
# missing from the host PATH. On Linux: `apt install sbsigntool
# efitools' if you prefer to run the tools natively.

set -euo pipefail

cd "$(dirname "$0")/.."
LAPEE=$(pwd)
SB_DIR="$LAPEE/secureboot"
# On the host the UKI is named lapee.efi; it gets copied to
# /EFI/Boot/BootX64.efi inside the ESP by build-usb-image.sh
# (UEFI's fallback boot path). Sign the host-side file; the rename
# happens automatically when the image is re-wrapped.
BUILD_UKI="$LAPEE/work/usb-build/lapee.efi"
# Keep the signed UKI one level up from usb-build/ because
# build-usb-image.sh --uki does `rm -rf work/usb-build/` before
# re-populating it (which would wipe the signed file if it lived
# inside).
SIGNED_UKI="$LAPEE/work/lapee.signed.efi"
USB_IMAGE="$LAPEE/work/lapee-usb.img"

LAPEE_TOOLS_IMAGE="${LAPEE_TOOLS_IMAGE:-lapee-tools:latest}"

# run_tool TOOL [ARGS...]
# If TOOL is on the host PATH, run it natively. Otherwise run it
# inside the lapee-tools container, with $LAPEE mounted at /work.
# Any arg that starts with $LAPEE/ is rewritten to the container's
# /work/ path so the tool sees the same file. Relies on paths
# already being absolute; relative paths are passed through, so
# callers that `cd $SB_DIR' first just work.
run_tool() {
    local _tool="$1"; shift
    if command -v "$_tool" >/dev/null 2>&1; then
        "$_tool" "$@"
        return $?
    fi
    if ! command -v docker >/dev/null 2>&1; then
        echo "[fail] neither $_tool nor docker found on PATH." >&2
        echo "       Install Docker Desktop, or on Linux: apt install sbsigntool efitools" >&2
        return 2
    fi
    if ! docker image inspect "$LAPEE_TOOLS_IMAGE" >/dev/null 2>&1; then
        echo "[fail] $_tool not on PATH and $LAPEE_TOOLS_IMAGE image is absent." >&2
        echo "       Run: make builders" >&2
        return 2
    fi
    local _args=() _a
    for _a in "$@"; do
        case "$_a" in
            "$LAPEE"/*) _args+=("/work/${_a#$LAPEE/}") ;;
            *)          _args+=("$_a") ;;
        esac
    done
    local _workdir="/work"
    case "$PWD" in
        "$LAPEE"/*) _workdir="/work/${PWD#$LAPEE/}" ;;
        "$LAPEE")   _workdir="/work" ;;
    esac
    docker run --rm --platform=linux/amd64 \
        -v "$LAPEE":/work \
        -w "$_workdir" \
        "$LAPEE_TOOLS_IMAGE" \
        "$_tool" "${_args[@]}"
}

usage() {
    cat <<USAGE
usage: sb-setup.sh <command>

Commands:
  keys         Generate operator PK/KEK/db keys under $SB_DIR.
               Run ONCE per operator. Keys never leave the host.

  sign         Sign the UKI at $BUILD_UKI using db.key. Writes
               $SIGNED_UKI; a separate step copies it over the
               unsigned UKI into the USB image.

  enrol        Produce UEFI enrolment .auth files in $SB_DIR/enrol/.
               Copy those to a separate FAT-formatted USB stick;
               plug that stick into the Framework, enter Secure Boot
               setup mode, and use the BIOS "enrol db/KEK/PK" UI
               (F2 -> Security -> Secure Boot).

  check        Print the state of everything. Keys exist? UKI signed?
                Enrolment artefacts ready?

  tools        Print what's installed and what's missing.

USAGE
    exit 1
}

cmd="${1:-}"
[ -n "$cmd" ] || usage

case "$cmd" in
    tools|check|keys|sign|enrol) ;;
    *) usage ;;
esac

have_tool() { command -v "$1" >/dev/null 2>&1; }

if [ "$cmd" = "tools" ]; then
    echo "=== SB signing tools on this host ==="
    for t in openssl sbsign sbverify cert-to-efi-sig-list \
             sign-efi-sig-list efi-updatevar uuidgen; do
        if have_tool "$t"; then
            printf "  [ok native]  %-24s -> %s\n" "$t" "$(command -v "$t")"
        else
            printf "  [container]  %-24s -> %s\n" "$t" \
                "via $LAPEE_TOOLS_IMAGE"
        fi
    done
    echo ""
    printf "  lapee-tools image: "
    if have_tool docker && docker image inspect "$LAPEE_TOOLS_IMAGE" \
            >/dev/null 2>&1; then
        echo "present"
    else
        echo "MISSING (run: make builders)"
    fi
    cat <<'NOTE'

Tools shown as "[container]" will run inside lapee-tools:latest
(docker/Dockerfile.tools) automatically. The container ships
sbsigntool, efitools, openssl, uuid-runtime. If it isn't built
yet, run `make builders' once.

On Linux you can install natively: apt install sbsigntool efitools
NOTE
    exit 0
fi

if [ "$cmd" = "check" ]; then
    echo "=== SB state ==="
    printf "  secureboot dir:     "
    [ -d "$SB_DIR" ] && echo "$SB_DIR" || echo "MISSING (run: $0 keys)"
    for name in PK KEK db; do
        if [ -f "$SB_DIR/$name.key" ] && [ -f "$SB_DIR/$name.crt" ]; then
            subj=$(openssl x509 -in "$SB_DIR/$name.crt" -noout -subject \
                    2>/dev/null | sed 's/^subject=//')
            printf "  %-18s %s\n" "$name keys:" "$subj"
        else
            printf "  %-18s MISSING\n" "$name keys:"
        fi
    done
    printf "  unsigned UKI:       "
    [ -f "$BUILD_UKI" ] && echo "$BUILD_UKI ($(stat -f %z "$BUILD_UKI") bytes)" \
        || echo "MISSING (run: make hb-usb-image)"
    printf "  signed UKI:         "
    [ -f "$SIGNED_UKI" ] && echo "$SIGNED_UKI ($(stat -f %z "$SIGNED_UKI") bytes)" \
        || echo "MISSING (run: $0 sign)"
    for name in PK KEK db; do
        f="$SB_DIR/enrol/$name.auth"
        printf "  enrol/%s.auth:      " "$name"
        [ -f "$f" ] && echo "$f ($(stat -f %z "$f") bytes)" \
            || echo "MISSING (run: $0 enrol)"
    done
    exit 0
fi

if [ "$cmd" = "keys" ]; then
    mkdir -p "$SB_DIR"
    chmod 700 "$SB_DIR"
    gen() {
        local name="$1" cn="$2"
        if [ -f "$SB_DIR/$name.key" ] && [ -f "$SB_DIR/$name.crt" ]; then
            echo "  [skip] $name: already exists at $SB_DIR/$name.{key,crt}"
            return
        fi
        echo "  [gen] $name ($cn)"
        openssl req -new -x509 -newkey rsa:2048 -sha256 -nodes \
                -subj "/CN=$cn/" -days 3650 \
                -keyout "$SB_DIR/$name.key" \
                -out    "$SB_DIR/$name.crt" 2>/dev/null
        chmod 600 "$SB_DIR/$name.key"
    }
    echo "=== generating operator Secure Boot keys in $SB_DIR ==="
    gen PK  "LapEE operator Platform Key ($(hostname))"
    gen KEK "LapEE operator Key Exchange Key ($(hostname))"
    gen db  "LapEE operator db signing key ($(hostname))"
    echo ""
    echo "Keys are operator-owned. Keep $SB_DIR/*.key files private."
    echo "Commit ONLY the .crt files if you want key rotation to be"
    echo "auditable; the .key files must never leave this host."
    exit 0
fi

if [ "$cmd" = "sign" ]; then
    [ -f "$SB_DIR/db.key" ] || { echo "[fail] run $0 keys first"; exit 2; }
    [ -f "$BUILD_UKI" ] || {
        echo "[fail] UKI not found at $BUILD_UKI"
        echo "       Run: make hb-usb-image"; exit 2;
    }
    echo "=== signing UKI with db.key ==="
    run_tool sbsign \
        --key "$SB_DIR/db.key" \
        --cert "$SB_DIR/db.crt" \
        --output "$SIGNED_UKI" \
        "$BUILD_UKI"
    ls -la "$SIGNED_UKI"
    echo "--- sbverify with db.crt ---"
    run_tool sbverify --cert "$SB_DIR/db.crt" "$SIGNED_UKI"
    echo ""
    echo "=== re-wrapping signed UKI into $USB_IMAGE ==="
    # Inject the signed UKI into a fresh USB image. We call
    # build-usb-image.sh directly with --uki so it reuses the
    # signed PE rather than rebuilding from kernel + initramfs
    # (which would strip the signature).
    "$LAPEE/scripts/build-usb-image.sh" \
        --uki   "$SIGNED_UKI" \
        --image "$USB_IMAGE"
    echo ""
    echo "Signed USB image ready: $USB_IMAGE"
    echo ""
    echo "Next: flash it with"
    echo "    make hb-usb-write DEV=/dev/diskN"
    exit 0
fi

if [ "$cmd" = "enrol" ]; then
    [ -f "$SB_DIR/PK.key" ] || { echo "[fail] run $0 keys first"; exit 2; }

    # Use a stable GUID per operator for the enrolment; re-enrolment
    # on the same Framework stays idempotent instead of stacking
    # multiple PKs.
    if [ ! -f "$SB_DIR/GUID.txt" ]; then
        uuidgen > "$SB_DIR/GUID.txt"
    fi
    GUID=$(cat "$SB_DIR/GUID.txt")
    mkdir -p "$SB_DIR/enrol"

    echo "=== producing UEFI enrolment .auth files ==="
    echo "    GUID: $GUID"
    cd "$SB_DIR"
    # PK (top of the chain, self-signed with the PK key itself).
    run_tool cert-to-efi-sig-list -g "$GUID" PK.crt PK.esl
    run_tool sign-efi-sig-list -k PK.key -c PK.crt PK PK.esl enrol/PK.auth
    # KEK (chains under PK).
    run_tool cert-to-efi-sig-list -g "$GUID" KEK.crt KEK.esl
    run_tool sign-efi-sig-list -k PK.key -c PK.crt KEK KEK.esl enrol/KEK.auth
    # db (chains under KEK).
    run_tool cert-to-efi-sig-list -g "$GUID" db.crt db.esl
    run_tool sign-efi-sig-list -k KEK.key -c KEK.crt db db.esl enrol/db.auth
    rm -f PK.esl KEK.esl db.esl
    cd "$LAPEE"
    ls -la "$SB_DIR/enrol/"
    cat <<'ENROL'

Enrolment procedure on the Framework 13:
  1. Copy the three .auth files above to a separate FAT-formatted
     USB stick (not the LapEE boot stick). Root-level is fine.
  2. Plug the enrolment USB into the Framework.
  3. Power on, F2 to enter BIOS.
  4. Security -> Secure Boot -> Enter Setup Mode (clears factory
     Microsoft keys; takes the machine out of a trust-chain rooted
     at Redmond and into one rooted at your operator key).
  5. Still in Security -> Secure Boot, enrol the three files in
     order: db.auth, KEK.auth, PK.auth. (PK last; enrolling PK
     exits setup mode and re-enables Secure Boot on the operator
     chain.)
  6. Save + exit. Boot from the LapEE USB -- the signed UKI from
     `sb-setup.sh sign' will pass SB verification; any other UEFI
     binary (including the factory Microsoft-signed installer
     media) will be rejected. This is the intended single-purpose
     appliance posture.

To revert to factory Microsoft keys: Security -> Secure Boot ->
Restore Factory Keys.
ENROL
    exit 0
fi
