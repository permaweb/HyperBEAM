"""hyperbeam_node.py — orchestrate the end-to-end LapEE attestation.

Acts as the HyperBEAM node, wiring together:
  1. Measured boot (simulated; would be UEFI+kernel on real hardware)
  2. Ephemeral node key generation inside the TPM
  3. PCR 15 extension with the ephemeral public key (LapEE key binding,
     paper §4.2)
  4. AO-Core hashpath seeded from the TPM event log tip
  5. Processing an AO-Core message
  6. Producing the signed attestation artifact that satisfies:
       (a) all known TPM state in comprehensible form
       (b) the node's local key
       (c) a TPM attestation chain we trust (test CA EK → AK → quote)

The artifact is a single JSON document written to out/attestation.json,
self-contained and verifiable offline by verifier.py.
"""

from __future__ import annotations

import base64
import hashlib
import json
import pathlib
import secrets
import sys
import time

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

from ao_core import HashPath
from measured_boot import run_measured_boot, GOLDEN
from tpm_device import TpmDevice


LAPEE_ROOT = HERE.parent
OUT = LAPEE_ROOT / "out"
OUT.mkdir(exist_ok=True)


def derive_ao_core_seed(event_log: list[dict], pcrs: dict[int, str]) -> bytes:
    """Seed the AO-Core hashpath with a commitment to the TPM state.

    Structurally this is what makes the two merkle chains (TPM event
    log + AO-Core hashpath) compose end-to-end in the paper's §5:
    the AO-Core chain's seed is literally a hash of the TPM state as
    of the pubkey-extend event, so any divergence upstream breaks the
    downstream chain.
    """
    m = hashlib.sha256()
    m.update(b"lapee/ao-core/seed-v1\0")
    for e in event_log:
        m.update(json.dumps(e, sort_keys=True, separators=(",", ":")).encode())
    for idx in sorted(pcrs):
        m.update(f"{idx}:{pcrs[idx]}".encode())
    return m.digest()


def build_attestation(
    tpm: TpmDevice,
    hashpath: HashPath,
    user_message: dict,
) -> dict:
    """Assemble the signed LapEE attestation bundle."""
    # Final AO-Core computation: sign across the hashpath tip.
    final_tip = hashpath.tip

    # Quote the operative PCR set with a fresh nonce that also binds the
    # quote to the hashpath tip, preventing replay of a stale quote
    # against a new computation.
    quote_nonce = hashlib.sha256(b"lapee/quote/" + final_tip).digest()
    pcr_set = [0, 1, 7, 11, 14, 15]
    quote = tpm.quote(quote_nonce, pcr_set)

    # Sign the final AO-Core tip with the ephemeral key.
    signed = tpm.sign(final_tip)

    # Read the authoritative PCR values (not from our in-memory view,
    # but from the TPM itself) so the attestation is independently
    # verifiable.
    pcrs_now = tpm.pcr_read(pcr_set)

    # Machine-identifying fields in human-readable form. This is the
    # "comprehensible form" the paper emphasizes at §4.3 and §5.
    machine_fields = {
        "cpu_family": "x86_64 (simulated)",
        "tpm_manufacturer": "swtpm (software TPM 2.0)",
        "tpm_type": "software",
        "tme_active": True,  # would be MSR-read on hardware
        "secure_boot_state": "enabled (operator-enrolled PK/KEK/db)",
        "iommu_policy": "strict",
        "kernel_lockdown": "confidentiality",
        "os_image": {
            "golden_uki_hash": GOLDEN["uki_hash_sha256"],
            "cmdline": GOLDEN["cmdline"],
            "rootfs_verity_root": GOLDEN["rootfs_verity_root"],
        },
        "hyperbeam_version": GOLDEN["hyperbeam_version"],
    }

    return {
        "lapee_attestation_version": "0.1",
        "issued_at_unix": int(time.time()),
        "machine_fields": machine_fields,
        "ek_cert_pem": tpm.state.ek_cert_pem.decode(),
        "ak_pub_pem": quote["ak_pub_pem"],
        "tcg_event_log": tpm.event_log_json(),
        "pcr_quote": {
            "message_b64": quote["message_b64"],
            "signature_b64": quote["signature_b64"],
            "pcrs_b64": quote["pcrs_b64"],
            "nonce_hex": quote["nonce_hex"],
            "pcr_selection": pcr_set,
            "pcr_values": pcrs_now,
        },
        "node_ephemeral_key": {
            "public_pem": tpm.state.signing_key_public_pem.decode(),
            "bound_to_pcr": 15,
            "binding_event": "key-pubkey-extend",
        },
        "ao_core": {
            "hashpath": hashpath.to_json(),
            "user_message": user_message,
        },
        "signature_over_hashpath_tip": {
            "scheme": signed["scheme"],
            "digest_b64": signed["digest_b64"],
            "signature_b64": signed["signature_b64"],
            "signed_value_hex": final_tip.hex(),
            "public_key_pem": signed["public_key_pem"],
        },
    }


def main() -> None:
    print("=" * 68)
    print("LapEE reference node — end-to-end attestation run")
    print("=" * 68)

    tpm = TpmDevice()
    print("[1/7] initialising TPM endorsement + attestation keys...")
    tpm.init_endorsement_context()
    print("[2/7] issuing EK certificate (test CA stands in for TPM vendor)...")
    tpm.load_or_create_ek_certificate()

    print("[3/7] simulating measured boot (firmware/UEFI/UKI/rootfs)...")
    mb = run_measured_boot(tpm)
    for e in mb.events:
        print(f"      extend PCR{e.pcr:>2} {e.event_type:<40s} {e.digest_sha256[:16]}...")

    print("[4/7] generating ephemeral node signing key inside TPM...")
    pk_pem = tpm.create_ephemeral_key()
    pk_digest = hashlib.sha256(pk_pem).hexdigest()
    print(f"      public key SHA-256 = {pk_digest[:32]}...")

    print("[5/7] extending PCR 15 with ephemeral pubkey (LapEE key binding)...")
    tpm.pcr_extend_event(
        pcr=15,
        event_type="EV_HYPERBEAM_KEY_BINDING",
        extend_bytes=pk_pem,
        data={
            "description": "HyperBEAM ephemeral signing key bound to this boot.",
            "public_key_pem": pk_pem.decode(),
            "public_key_sha256": pk_digest,
        },
    )

    print("[6/7] seeding AO-Core hashpath + processing sample message...")
    post_boot_pcrs = tpm.pcr_read([0, 1, 7, 11, 14, 15])
    ao_seed = derive_ao_core_seed(tpm.event_log_json(), post_boot_pcrs)
    hp = HashPath(seed=ao_seed)
    hp.extend("device-load", {
        "device": "~tpm@2.0a",
        "revision": "0.1",
        "signer": "lapee-dev-test-signer",
    })
    user_message = {
        "kind": "demo-inference",
        "id": secrets.token_hex(8),
        "prompt": "What is the TPM-attested trust chain of this result?",
        "timestamp_unix": int(time.time()),
    }
    hp.extend("ao-message/request", user_message)
    hp.extend("ao-message/response", {
        "result": (
            "The result is produced by a LapEE node whose boot chain is "
            "attested end-to-end by the measured-boot PCR trajectory, "
            "whose signing key is bound to this specific boot via PCR 15, "
            "and whose computation transcript is committed by this "
            "hashpath tip."
        ),
    })

    print("[7/7] assembling + signing attestation artifact...")
    tpm.persist_event_log()
    bundle = build_attestation(tpm, hp, user_message)

    out_path = OUT / "attestation.json"
    out_path.write_text(json.dumps(bundle, indent=2))
    print(f"\n  wrote {out_path} ({out_path.stat().st_size} bytes)")

    # Convenience: also emit an address-style identifier for the node.
    # On AO-Core a signer's address is typically SHA-256 of the public
    # key's DER encoding, truncated or base64-encoded. We give both forms.
    node_id = hashlib.sha256(pk_pem).hexdigest()
    print(f"  node signer address (sha256 pubkey PEM) = {node_id}")
    (OUT / "node-address.txt").write_text(node_id + "\n")

    print("\nSuccess. Use verifier/verifier.py to validate.")


if __name__ == "__main__":
    main()
