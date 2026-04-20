"""Negative tests: verify that tampering with the attestation causes
the verifier to reject.

For each tamper, we mutate a copy of the signed artifact, feed it to
the verifier, and assert that the verdict is REJECTED. This
demonstrates that the trust chain is load-bearing end-to-end.

Run:
    python3 tests/negative_tests.py
"""

from __future__ import annotations

import copy
import json
import pathlib
import subprocess
import sys
import tempfile

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent
sys.path.insert(0, str(ROOT / "verifier"))
import verifier as V  # noqa: E402


def _verify(bundle: dict) -> tuple[bool, list[V.Check]]:
    """Run verifier over a bundle dict and return (overall_ok, checks)."""
    with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as f:
        json.dump(bundle, f)
        path = pathlib.Path(f.name)
    checks = V.verify(path, ROOT / "out" / "test-tpm-ca.crt")
    path.unlink()
    return all(c.ok for c in checks), checks


def _load_golden() -> dict:
    return json.loads((ROOT / "out" / "attestation.json").read_text())


def _fmt(passed: bool, label: str, detail: str = "") -> str:
    status = "PASS" if passed else "FAIL"
    prefix = f"  [{status}] {label}"
    return prefix if not detail else f"{prefix}\n         {detail}"


def run() -> int:
    print("=" * 68)
    print("LapEE negative tests — expect REJECTED for every tamper")
    print("=" * 68)

    # First, confirm the baseline passes.
    golden = _load_golden()
    ok, checks = _verify(golden)
    if not ok:
        print("  unexpected: baseline attestation does not verify!")
        for c in checks:
            print(f"    [{'PASS' if c.ok else 'FAIL'}] {c.name}: {c.detail[:80]}")
        return 2
    print("  baseline attestation verifies cleanly (sanity)")
    print()

    scenarios: list[tuple[str, str, callable]] = [
        (
            "T1 tampered event-log digest",
            "event log replay fails",
            lambda b: _tamper_event_digest(b),
        ),
        (
            "T2 inserted extra event after pubkey-extend",
            "PCR 15 replay diverges",
            lambda b: _insert_bogus_event(b),
        ),
        (
            "T3 substituted ephemeral pubkey",
            "pubkey no longer matches PCR-15 binding event",
            lambda b: _substitute_pubkey(b),
        ),
        (
            "T4 forged hashpath tip",
            "hashpath replay diverges",
            lambda b: _mutate_hashpath_event(b),
        ),
        (
            "T5 wrong quote nonce",
            "nonce/hashpath binding fails",
            lambda b: _mutate_quote_nonce(b),
        ),
        (
            "T6 corrupted final signature",
            "PSS verification fails",
            lambda b: _mutate_final_signature(b),
        ),
        (
            "T7 wrong EK cert (different CA)",
            "EK chain fails to verify",
            lambda b: _substitute_ek_cert(b),
        ),
    ]

    failures = 0
    for label, expected, mutator in scenarios:
        b = copy.deepcopy(golden)
        mutator(b)
        ok, checks = _verify(b)
        # A correct negative test is: overall_ok == False
        if not ok:
            # Find which check failed
            failed = [c.name for c in checks if not c.ok]
            print(_fmt(True, label, f"rejected as expected: {failed[0] if failed else expected}"))
        else:
            failures += 1
            print(_fmt(False, label, f"UNEXPECTED ACCEPT — tamper went undetected!"))

    print()
    print("=" * 68)
    print(f"Negative tests: {len(scenarios) - failures}/{len(scenarios)} rejected correctly")
    print("=" * 68)
    return 0 if failures == 0 else 1


# --- Mutators ---------------------------------------------------------------

def _tamper_event_digest(b: dict) -> None:
    b["tcg-event-log"][0]["digest-sha256"] = "f" * 64


def _insert_bogus_event(b: dict) -> None:
    b["tcg-event-log"].append({
        "seq": 999,
        "pcr": 15,
        "event-type": "EV_BOGUS",
        "digest-sha256": "a" * 64,
        "data": {"malicious": True},
    })


def _substitute_pubkey(b: dict) -> None:
    # Replace with a different valid-looking public key.
    from cryptography.hazmat.primitives import serialization
    from cryptography.hazmat.primitives.asymmetric import rsa
    new_key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    new_pem = new_key.public_key().public_bytes(
        serialization.Encoding.PEM,
        serialization.PublicFormat.SubjectPublicKeyInfo,
    ).decode()
    b["node-ephemeral-key"]["public-pem"] = new_pem
    b["signature-over-hashpath-tip"]["public-key-pem"] = new_pem


def _mutate_hashpath_event(b: dict) -> None:
    b["ao-core"]["hashpath"]["events"][1]["value"]["prompt"] = "MUTATED BY ATTACKER"


def _mutate_quote_nonce(b: dict) -> None:
    b["pcr-quote"]["nonce-hex"] = "0" * 64


def _mutate_final_signature(b: dict) -> None:
    import base64 as _b64
    sig = _b64.b64decode(b["signature-over-hashpath-tip"]["signature-b64"])
    # Flip a byte in the middle of the signature.
    mutated = sig[:100] + bytes([sig[100] ^ 0xFF]) + sig[101:]
    b["signature-over-hashpath-tip"]["signature-b64"] = _b64.b64encode(mutated).decode()


def _substitute_ek_cert(b: dict) -> None:
    # Generate a totally unrelated self-signed cert that does NOT chain
    # to the trusted CA.
    from cryptography.hazmat.primitives import hashes, serialization
    from cryptography.hazmat.primitives.asymmetric import rsa
    from cryptography import x509
    from cryptography.x509.oid import NameOID
    import datetime as dt

    key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
    subj = x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "Rogue EK")])
    cert = (
        x509.CertificateBuilder()
        .subject_name(subj)
        .issuer_name(subj)
        .public_key(key.public_key())
        .serial_number(x509.random_serial_number())
        .not_valid_before(dt.datetime.now(dt.timezone.utc))
        .not_valid_after(dt.datetime.now(dt.timezone.utc) + dt.timedelta(days=365))
        .sign(key, hashes.SHA256())
    )
    b["ek-cert-pem"] = cert.public_bytes(serialization.Encoding.PEM).decode()


if __name__ == "__main__":
    raise SystemExit(run())
