"""measured_boot.py — simulate the measured-boot chain of a LapEE node.

On real hardware, UEFI firmware extends PCRs 0–7 as it loads firmware,
bootloader, and Secure Boot variable contents. systemd-boot loads the
UKI and atomically extends PCR 11. The kernel's dm-verity driver
verifies the rootfs Merkle root (baked into the signed cmdline, hence
also in PCR 11).

In this reference environment we don't have real UEFI; instead we
produce the same event log entries and PCR extensions that a real
measured boot would produce, using representative content hashes. A
verifier reasoning over the event log cannot distinguish between
simulated and real entries as long as the hashes are internally
consistent and the PCR extensions reproduce the quoted values.

This module is the M3/M4 analogue for the in-process demo path; once a
real Buildroot kernel + UKI is available the scripts in ./scripts/ take
over and this simulator is replaced by genuine UEFI + kernel
measurements.
"""

from __future__ import annotations

import hashlib
from dataclasses import dataclass
from typing import Any

from tpm_device import TpmDevice, EventLogEntry


# Representative artifact identifiers for the "golden" boot.
# On real hardware these would be real file hashes from Buildroot +
# Secure Boot + dm-verity output.

GOLDEN = {
    "firmware_version": "LapEE-ref-UEFI/Edk2 202502",
    "firmware_hash_sha256": "a" * 64,  # stand-in
    "secureboot_db_hash_sha256": "b" * 64,
    "uki_hash_sha256": "c" * 64,  # kernel + initramfs + cmdline, single hash
    "rootfs_verity_root": "d" * 64,
    "cmdline": (
        "root=/dev/mapper/verity-root ro quiet "
        "lockdown=confidentiality iommu=strict module.sig_enforce=1 "
        "init_on_alloc=1 init_on_free=1 roothash=" + ("d" * 64)
    ),
    "hyperbeam_version": "lapee-dev-M5M6",
}


@dataclass
class MeasuredBootResult:
    events: list[EventLogEntry]
    pcr_snapshot: dict[int, str]


def _sha(data: Any) -> bytes:
    if isinstance(data, bytes):
        return hashlib.sha256(data).digest()
    return hashlib.sha256(str(data).encode("utf-8")).digest()


def run_measured_boot(tpm: TpmDevice, golden: dict = GOLDEN) -> MeasuredBootResult:
    """Extend PCRs 0, 7, 11, 14 to represent the golden LapEE boot.

    Event types mirror the TCG PC Client Platform Firmware Profile's
    event types so that verifiers built against standard tooling can
    reason about them directly.
    """
    # PCR 0 — platform firmware.
    tpm.pcr_extend_event(
        pcr=0,
        event_type="EV_S_CRTM_VERSION",
        extend_bytes=golden["firmware_hash_sha256"].encode("utf-8"),
        data={
            "description": "Platform firmware (UEFI).",
            "firmware_version": golden["firmware_version"],
            "firmware_hash_sha256": golden["firmware_hash_sha256"],
        },
    )

    # PCR 7 — Secure Boot database contents (PK, KEK, db).
    tpm.pcr_extend_event(
        pcr=7,
        event_type="EV_EFI_VARIABLE_DRIVER_CONFIG",
        extend_bytes=golden["secureboot_db_hash_sha256"].encode("utf-8"),
        data={
            "description": "Secure Boot policy: operator-enrolled PK/KEK/db.",
            "state": "enabled",
            "db_hash_sha256": golden["secureboot_db_hash_sha256"],
        },
    )

    # PCR 11 — UKI (kernel + initramfs + cmdline as one signed PE).
    tpm.pcr_extend_event(
        pcr=11,
        event_type="EV_EFI_BOOT_SERVICES_APPLICATION",
        extend_bytes=golden["uki_hash_sha256"].encode("utf-8"),
        data={
            "description": "UKI (kernel + initramfs + cmdline).",
            "uki_hash_sha256": golden["uki_hash_sha256"],
            "cmdline": golden["cmdline"],
            "hyperbeam_version": golden["hyperbeam_version"],
        },
    )

    # PCR 14 — dm-verity rootfs root hash. On real LapEE this is carried
    # in the UKI cmdline and checked by dm-verity on every block read.
    tpm.pcr_extend_event(
        pcr=14,
        event_type="EV_COMPACT_HASH",
        extend_bytes=golden["rootfs_verity_root"].encode("utf-8"),
        data={
            "description": "dm-verity rootfs sealed by Merkle root.",
            "verity_root_hash_sha256": golden["rootfs_verity_root"],
        },
    )

    snapshot = tpm.pcr_read([0, 1, 7, 11, 14])
    return MeasuredBootResult(
        events=list(tpm.state.event_log),
        pcr_snapshot=snapshot,
    )
