#!/usr/bin/env python3
"""
generate-pcr-profiles.py — walk every fixture under
`priv/tpm-interpret/fixtures/`, replay PCRs 0-7 by SHA-256-folding
each event's digest into its declared PCR (starting from 32
zero bytes), and write one `pcr-profiles/from-fixture-<name>.json`
file per fixture.

This turns the raw event-log corpus into first-class PCR-match
profiles the interpret device can use at attestation-time to
answer "this boot's PCR 0 + PCR 7 fingerprint matches the
<vendor> <firmware> baseline we've already seen".

Note on TPM 1.2 legacy logs: for fixtures whose first record is
`EV_ACTION (type=8)` instead of `EV_NO_ACTION (type=3) with
SpecID', the events are SHA-1 only. We emit a `sha1-match-pcrs'
block for those.
"""
from __future__ import annotations
import base64
import hashlib
import json
import os
import struct
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
LAPEE_BAREMETAL = os.path.dirname(SCRIPT_DIR)
REPO = os.path.dirname(LAPEE_BAREMETAL)
FIXTURES = os.path.join(REPO, "priv", "tpm-interpret", "fixtures")
PROFILES = os.path.join(REPO, "priv", "tpm-interpret", "pcr-profiles")

# TCG algorithm digest sizes.
ALG_SIZE = {
    0x04: 20,   # SHA-1
    0x0B: 32,   # SHA-256
    0x0C: 48,   # SHA-384
    0x0D: 64,   # SHA-512
    0x12: 32,   # SM3-256
    0x15: 32,   # SHA3-256
    0x16: 48,   # SHA3-384
    0x17: 64,   # SHA3-512
}

# TCG event type names we use for "was a CRTM version seen".
EV_NO_ACTION   = 0x03
EV_S_CRTM_VER  = 0x08
EV_SEPARATOR   = 0x04
EV_EFI_VAR_DRV = 0x80000001
EV_EFI_HCRTM   = 0x80000010
EV_POST_CODE   = 0x01


def b64u(bs: bytes) -> str:
    return base64.urlsafe_b64encode(bs).decode("ascii").rstrip("=")


def detect_format(data: bytes):
    """Return either ('crypto-agile', first-record-event-size)
    or ('tpm12-legacy', None) or None if malformed."""
    if len(data) < 32:
        return None
    pcr, et = struct.unpack("<II", data[:8])
    if pcr != 0:
        return None
    if et == EV_NO_ACTION:
        # First record is legacy TCG_PCR_EVENT (SHA-1 header).
        # Event data may be TCG_EfiSpecIdEvent (crypto-agile) OR
        # a pre-crypto-agile placeholder.
        ev_size = struct.unpack_from("<I", data, 28)[0]
        # Check for "Spec ID Event03" signature at offset 32 (first
        # record's event bytes).
        if ev_size >= 16 and data[32:47].startswith(b"Spec ID Event03"):
            return ("crypto-agile", ev_size)
        return ("tpm12-legacy", ev_size)
    return ("tpm12-legacy", None)


def replay_agile(data: bytes):
    """Replay PCRs 0-23 from a crypto-agile TCG event log.
    Returns (sha1_pcrs, sha256_pcrs, sha384_pcrs, records), where
    each `_pcrs' is a dict of {pcr_index: bytes}."""
    # Skip the first record (legacy TCG_PCR_EVENT with SpecID).
    first_ev_sz = struct.unpack_from("<I", data, 28)[0]
    pos = 32 + first_ev_sz
    pcr_sha1 = {i: b"\x00" * 20 for i in range(24)}
    pcr_sha256 = {i: b"\x00" * 32 for i in range(24)}
    pcr_sha384 = {i: b"\x00" * 48 for i in range(24)}
    records = []
    while pos + 12 <= len(data):
        pcr, et, dc = struct.unpack_from("<III", data, pos)
        if pcr > 23 or et > 0x9FFFFFFF or dc > 10:
            break
        pos += 12
        digests = {}
        ok = True
        for _ in range(dc):
            if pos + 2 > len(data):
                ok = False
                break
            (alg,) = struct.unpack_from("<H", data, pos)
            pos += 2
            size = ALG_SIZE.get(alg)
            if size is None or pos + size > len(data):
                ok = False
                break
            digests[alg] = data[pos:pos + size]
            pos += size
        if not ok:
            break
        if pos + 4 > len(data):
            break
        (ev_sz,) = struct.unpack_from("<I", data, pos)
        pos += 4
        ev_data = data[pos:pos + ev_sz]
        pos += ev_sz
        records.append((pcr, et, digests, ev_data))
        # EV_NO_ACTION is explicitly NOT extended per spec.
        if et == EV_NO_ACTION:
            continue
        if 0x04 in digests:
            pcr_sha1[pcr] = hashlib.sha1(
                pcr_sha1[pcr] + digests[0x04]).digest()
        if 0x0B in digests:
            pcr_sha256[pcr] = hashlib.sha256(
                pcr_sha256[pcr] + digests[0x0B]).digest()
        if 0x0C in digests:
            pcr_sha384[pcr] = hashlib.sha384(
                pcr_sha384[pcr] + digests[0x0C]).digest()
    return pcr_sha1, pcr_sha256, pcr_sha384, records


def replay_legacy(data: bytes):
    """Replay SHA-1 PCRs from a TPM 1.2 legacy TCG event log."""
    pcr_sha1 = {i: b"\x00" * 20 for i in range(24)}
    records = []
    pos = 0
    while pos + 32 <= len(data):
        pcr, et = struct.unpack_from("<II", data, pos)
        if pcr > 23 or et > 0x9FFFFFFF:
            break
        digest = data[pos + 8:pos + 28]
        ev_sz = struct.unpack_from("<I", data, pos + 28)[0]
        ev_data = data[pos + 32:pos + 32 + ev_sz]
        records.append((pcr, et, {0x04: digest}, ev_data))
        pos += 32 + ev_sz
        if et == EV_NO_ACTION:
            continue
        pcr_sha1[pcr] = hashlib.sha1(pcr_sha1[pcr] + digest).digest()
    return pcr_sha1, {}, {}, records


def extract_crtm_version(records):
    """Find the first EV_S_CRTM_VERSION record; return UTF-8 string
    or None."""
    for pcr, et, digests, ev_data in records:
        if et == EV_S_CRTM_VER and ev_data:
            # Heuristic: UTF-16LE if even length and looks ASCII
            # every other byte.
            if len(ev_data) % 2 == 0:
                try:
                    s = ev_data.decode("utf-16-le", errors="replace").rstrip(
                        "\x00")
                    if s.isprintable():
                        return s
                except Exception:
                    pass
            try:
                s = ev_data.decode("utf-8", errors="replace").rstrip("\x00")
                if s.isprintable():
                    return s
            except Exception:
                pass
            return ev_data.hex()
    return None


def extract_secure_boot(records):
    """Walk EV_EFI_VARIABLE_DRIVER_CONFIG events for SecureBoot;
    return bool or None."""
    for pcr, et, digests, ev_data in records:
        if et == EV_EFI_VAR_DRV and len(ev_data) >= 48:
            # UEFI_VARIABLE_DATA: GUID (16) + NameLen u64 + DataLen u64 +
            # UnicodeName (NameLen*2) + VariableData (DataLen)
            try:
                name_len = struct.unpack_from("<Q", ev_data, 16)[0]
                data_len = struct.unpack_from("<Q", ev_data, 24)[0]
                name_bytes = ev_data[32:32 + name_len * 2]
                var_data = ev_data[32 + name_len * 2:
                                    32 + name_len * 2 + data_len]
                name = name_bytes.decode("utf-16-le", errors="replace").rstrip(
                    "\x00")
                if name == "SecureBoot" and len(var_data) == 1:
                    return var_data[0] == 1
            except Exception:
                pass
    return None


def guess_platform(crtm: str | None, fixture_name: str) -> dict:
    """Best-effort platform/vendor classification from the CRTM
    string or the fixture filename."""
    out = {}
    if crtm:
        low = crtm.lower()
        if crtm.startswith("N") and len(crtm) >= 4 and crtm[1:4].isalnum():
            out["platform-vendor"] = "Lenovo"
            out["firmware-id-family"] = "thinkpad"
        elif "dell" in low:
            out["platform-vendor"] = "Dell"
        elif crtm.startswith("HP") or "hpq" in low:
            out["platform-vendor"] = "HP"
        elif "insyde" in low:
            out["platform-vendor"] = "Insyde (third-party UEFI)"
        elif "american megatrends" in low or "ami" in low:
            out["platform-vendor"] = "American Megatrends (AMI)"
        elif "phoenix" in low:
            out["platform-vendor"] = "Phoenix Technologies"
        elif "coreboot" in low:
            out["platform-vendor"] = "coreboot"
        elif "seabios" in low:
            out["platform-vendor"] = "QEMU SeaBIOS"
        elif "edk" in low or "ovmf" in low:
            out["platform-vendor"] = "EDK II / OVMF"
        elif "gce" in low or "google" in low:
            out["platform-vendor"] = "Google (GCE)"
    # Fall back to fixture filename hints.
    name_hint = fixture_name.lower()
    if "platform-vendor" not in out:
        if "lenovo" in name_hint or "thinkpad" in name_hint:
            out["platform-vendor"] = "Lenovo"
        elif "dell" in name_hint:
            out["platform-vendor"] = "Dell"
        elif "hp" in name_hint or "hpe" in name_hint:
            out["platform-vendor"] = "HP / HPE"
        elif "supermicro" in name_hint:
            out["platform-vendor"] = "Supermicro"
        elif "inspur" in name_hint:
            out["platform-vendor"] = "Inspur"
        elif "intel-nuc" in name_hint or "intel-desktop" in name_hint:
            out["platform-vendor"] = "Intel"
        elif "gce" in name_hint or "google" in name_hint:
            out["platform-vendor"] = "Google Cloud"
        elif "aws" in name_hint or "nitro" in name_hint:
            out["platform-vendor"] = "AWS"
        elif "azure" in name_hint:
            out["platform-vendor"] = "Azure"
        elif "tdx" in name_hint:
            out["platform-vendor"] = "Intel TDX"
        elif "qemu" in name_hint or "ovmf" in name_hint or "seabios" in name_hint:
            out["platform-vendor"] = "QEMU"
        elif "fedora" in name_hint or "arch" in name_hint \
                or "canonical" in name_hint or "ubuntu" in name_hint:
            out["platform-vendor"] = "generic-linux-distro-testing"
    return out


def trust_tier_for(attrs: dict, fixture_name: str) -> str:
    vendor = (attrs.get("platform-vendor") or "").lower()
    if "qemu" in vendor or "ovmf" in vendor or "seabios" in vendor:
        return "development-only"
    if "tdx" in vendor or "gce" in vendor or "cloud" in vendor \
            or "aws" in vendor or "azure" in vendor:
        return "cloud-vtpm"
    return "real-hardware"


def main():
    if not os.path.isdir(FIXTURES):
        sys.exit(f"no fixtures dir at {FIXTURES}")
    os.makedirs(PROFILES, exist_ok=True)
    written = 0
    skipped = 0
    for name in sorted(os.listdir(FIXTURES)):
        path = os.path.join(FIXTURES, name)
        if not name.endswith(".bin") or not os.path.isfile(path):
            continue
        with open(path, "rb") as f:
            data = f.read()
        if len(data) < 32:
            skipped += 1
            continue
        fmt = detect_format(data)
        if fmt is None:
            skipped += 1
            continue
        kind, _ = fmt
        try:
            if kind == "crypto-agile":
                sha1, sha256, sha384, records = replay_agile(data)
            else:
                sha1, sha256, sha384, records = replay_legacy(data)
        except Exception as e:
            print(f"  {name}: replay error — {e}", file=sys.stderr)
            skipped += 1
            continue
        crtm = extract_crtm_version(records)
        sb = extract_secure_boot(records)
        attrs = guess_platform(crtm, name)
        attrs["fixture"] = name
        attrs["record-count"] = len(records)
        attrs["log-format"] = kind
        if crtm:
            attrs["crtm-version"] = crtm
        if sb is not None:
            attrs["secure-boot-enabled"] = sb
        attrs["trust-tier"] = trust_tier_for(attrs, name)
        # Some legacy logs don't populate every PCR in the map;
        # `.get(i, <zeros>)' keeps us safe.
        def nonzero(d, i, zeros):
            return d.get(i, zeros) != zeros
        pcrs = {}
        has_sha1 = any(
            nonzero(sha1, i, b"\x00" * 20)
            for i in (0, 1, 2, 3, 4, 7))
        has_sha256 = any(
            nonzero(sha256, i, b"\x00" * 32)
            for i in (0, 1, 2, 3, 4, 7))
        if has_sha256:
            pcrs["sha256"] = {
                str(i): b64u(sha256[i])
                for i in (0, 1, 2, 3, 4, 5, 7, 10, 11, 14)
                if nonzero(sha256, i, b"\x00" * 32)
            }
        if has_sha1:
            pcrs["sha1"] = {
                str(i): b64u(sha1[i])
                for i in (0, 1, 2, 3, 4, 5, 7, 10, 11, 14)
                if nonzero(sha1, i, b"\x00" * 20)
            }
        profile = {
            "schema-version": 1,
            "name": (attrs.get("platform-vendor") or "unknown") +
                    " — fixture " + name,
            "match-pcrs": pcrs,
            "attributes": attrs,
            "notes": (
                "Derived by replaying every event in "
                f"priv/tpm-interpret/fixtures/{name} into its declared "
                "PCR (SHA-1 + SHA-256, starting from all-zero state, "
                "folding per TCG PC Client spec). These are the "
                "expected PCR fingerprints a TPM quote from this "
                "exact firmware + boot sequence would produce. "
                "Source: public test-vector corpus collected from "
                "tpm2-tools, go-attestation, keylime, immune-guard, "
                "fwupd, and others (see fixtures/ history)."
            ),
        }
        out_path = os.path.join(
            PROFILES, f"from-fixture-{name.replace('.bin', '')}.json")
        with open(out_path, "w") as f:
            json.dump(profile, f, indent=2, sort_keys=False)
        written += 1
    print(f"wrote {written} profiles; skipped {skipped}")


if __name__ == "__main__":
    main()
