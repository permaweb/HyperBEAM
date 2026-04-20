"""verifier_hb.py — validate a LapEE attestation produced by dev_tpm2.

The envelope follows AO-Core conventions: binary values are encoded as
URL-safe base64 (no padding). Fields:

  lapee_attestation_version : "0.3"
  issued_at_unix            : integer
  ek_cert_pem               : EK certificate (PEM text)
  ak_pub_pem                : Attestation Key public key (PEM text)
  tpm_quote                 :
    pcr_selection   : [integer]
    nonce           : base64url(raw_nonce)
    quoted          : base64url(TPMS_ATTEST bytes)
    signature       : base64url(TPMT_SIGNATURE bytes)
    pcr_values      : { "N" -> base64url(raw_pcr_value) }
  runtime_event_log         : [{ pcr, digest, event_type, ... }]
  node_message              : the running HB node message (map)
  node_message_id           : base64url of the 32-byte native id of
                              hb_message:id(node_message, all, Opts)
  wallet_address            : base64url human id of operator wallet

Checks:
  1. EK certificate chains to the trusted TPM vendor root CA.
  2. TPM2_Quote signature is valid under the AK public key.
  3. Quote's extraData == nonce provided in envelope.
  4. Quote's pcrDigest == SHA-256(pcr0||pcr1||...||pcr15) in selection order.
  5. Runtime event log's PCR 15 extension replays from zero to the
     quoted PCR 15 value.
  6. PCR 15 extension digest equals `node_message_id' from the envelope.
  7. node_message + node_message_id are present and well-shaped.
"""
from __future__ import annotations

import base64
import hashlib
import json
import pathlib
import subprocess
import sys
import tempfile


class Check:
    def __init__(self, name, ok, detail=""):
        self.name = name
        self.ok = ok
        self.detail = detail
    def __repr__(self):
        tag = "[PASS]" if self.ok else "[FAIL]"
        return f"{tag} {self.name}\n       {self.detail}"


def b64url_decode(s):
    """Decode a URL-safe base64 string (no padding). HB emits these via
    `hb_util:encode/1' (b64rs)."""
    if isinstance(s, bytes):
        s = s.decode("ascii")
    pad = "=" * (-len(s) % 4)
    return base64.urlsafe_b64decode(s + pad)


def _b64u(b: bytes) -> str:
    """Encode raw bytes as HyperBEAM-wire base64url (no padding).
    Used only in diagnostic output — matches the encoding of every
    binary on the HB wire, so a human reading a verifier message
    can copy/paste directly against `/attestation' output."""
    return base64.urlsafe_b64encode(b).rstrip(b"=").decode("ascii")


def _verify_cert_chain(ek_pem, ca_path):
    with tempfile.NamedTemporaryFile(suffix=".pem", mode="w", delete=False) as f:
        f.write(ek_pem)
        ek_path = f.name
    r = subprocess.run(
        ["openssl", "verify", "-CAfile", str(ca_path), ek_path],
        capture_output=True, text=True
    )
    ok = r.returncode == 0
    return Check(
        "EK certificate chains to trusted TPM vendor root CA",
        ok,
        (r.stdout + r.stderr).strip() or "(no output)",
    )


def _verify_quote_openssl(envelope):
    q = envelope["tpm_quote"]
    quoted = b64url_decode(q["quoted"])
    sig = b64url_decode(q["signature"])
    ak_pem = envelope["ak_pub_pem"].encode()
    nonce = b64url_decode(q["nonce"])
    pcr_values = q["pcr_values"]
    selection = q["pcr_selection"]

    from cryptography.hazmat.primitives import hashes, serialization
    from cryptography.hazmat.primitives.asymmetric import padding

    try:
        ak = serialization.load_pem_public_key(ak_pem)
        ak.verify(
            sig, quoted,
            padding.PSS(mgf=padding.MGF1(hashes.SHA256()), salt_length=32),
            hashes.SHA256(),
        )
    except Exception as e:
        return Check("TPM2_Quote signature valid under AK public key",
                     False, str(e)[:200])

    # Parse TPMS_ATTEST: magic(4) type(2) qualifiedSigner(TPM2B) extraData(TPM2B)
    #   clockInfo(17) firmwareVersion(8) attested(TPMS_QUOTE_INFO:
    #      pcrSelect(TPML_PCR_SELECTION) pcrDigest(TPM2B_DIGEST))
    off = 4 + 2
    qs_size = int.from_bytes(quoted[off:off + 2], "big"); off += 2 + qs_size
    ed_size = int.from_bytes(quoted[off:off + 2], "big"); off += 2
    extra = quoted[off:off + ed_size]; off += ed_size
    if extra != nonce:
        return Check("TPM2_Quote extraData == nonce",
                     False, f"extraData={_b64u(extra)[:22]}… "
                            f"nonce={_b64u(nonce)[:22]}…")
    off += 17 + 8
    n_sel = int.from_bytes(quoted[off:off + 4], "big"); off += 4
    for _ in range(n_sel):
        off += 2
        sz = quoted[off]; off += 1
        off += sz
    pd_size = int.from_bytes(quoted[off:off + 2], "big"); off += 2
    claimed_digest = quoted[off:off + pd_size]

    m = hashlib.sha256()
    for idx in selection:
        v = pcr_values.get(str(idx), pcr_values.get(idx))
        if v is None:
            return Check("Quote pcrDigest matches reported PCR values",
                         False, f"missing value for PCR {idx}")
        m.update(b64url_decode(v))
    if claimed_digest != m.digest():
        return Check("Quote pcrDigest matches reported PCR values",
                     False,
                     f"quote={_b64u(claimed_digest)[:22]} vs "
                     f"computed={_b64u(m.digest())[:22]}")
    return Check("TPM2_Quote signature + pcrDigest + nonce all valid",
                 True, "OpenSSL PSS + TPMS_ATTEST parse ok")


def _verify_pcr15_replay(envelope):
    """Replay the event log's PCR 15 extensions from zero; must match quote."""
    events = [e for e in envelope["runtime_event_log"] if int(e["pcr"]) == 15]
    pcr = b"\x00" * 32
    for e in events:
        digest = b64url_decode(e["digest"])
        pcr = hashlib.sha256(pcr + digest).digest()
    quoted15_b64 = envelope["tpm_quote"]["pcr_values"].get("15")
    if quoted15_b64 is None:
        return Check("Runtime event log replay of PCR 15 matches quoted value",
                     False, "no pcr_values[15] in envelope")
    quoted = b64url_decode(quoted15_b64)
    ok = pcr == quoted
    return Check(
        "Runtime event log replay of PCR 15 matches quoted value",
        ok,
        f"{len(events)} PCR-15 event(s); replay={_b64u(pcr)[:22]}… "
        f"quote={_b64u(quoted)[:22]}…"
    )


def _verify_node_msg_binding(envelope):
    """The single PCR 15 event's digest must equal the envelope's
    node_message_id; compare the decoded raw bytes so that either side
    can be encoded differently and we still notice inequality."""
    events = [e for e in envelope["runtime_event_log"] if int(e["pcr"]) == 15]
    claimed_id = envelope.get("node_message_id")
    if not claimed_id:
        return Check("PCR 15 extension commits to node_message_id",
                     False, "no node_message_id in envelope")
    if not events:
        return Check("PCR 15 extension commits to node_message_id",
                     False, "no PCR 15 events in runtime event log")
    id_raw = b64url_decode(claimed_id)
    matching = [
        e for e in events
        if b64url_decode(e.get("digest", "")) == id_raw
    ]
    if not matching:
        return Check("PCR 15 extension commits to node_message_id",
                     False,
                     f"no PCR-15 event with digest {claimed_id[:16]}...")
    return Check(
        "PCR 15 extension commits to node_message_id",
        True,
        f"event with digest {claimed_id[:16]}... "
        f"found at seq={matching[0].get('seq')}"
    )


def _verify_node_msg_id_matches_content(envelope):
    """Sanity check: node_message is present and node_message_id is a
    43-character base64url string (which decodes to 32 bytes). Full
    content-binding is via the PCR 15 event log (check 6)."""
    nm = envelope.get("node_message")
    idh = envelope.get("node_message_id")
    if not nm or not idh:
        return Check(
            "Embedded node_message + id shape",
            False,
            f"node_message={'yes' if nm else 'no'} id={'yes' if idh else 'no'}")
    if not isinstance(idh, str) or len(idh) != 43:
        return Check(
            "Embedded node_message + id shape",
            False,
            f"id length {len(idh) if isinstance(idh,str) else '?'} != 43")
    try:
        raw = b64url_decode(idh)
    except Exception as e:
        return Check("Embedded node_message + id shape",
                     False, f"id not base64url: {e}")
    if len(raw) != 32:
        return Check("Embedded node_message + id shape",
                     False, f"decoded id is {len(raw)} bytes, expected 32")
    return Check(
        "Embedded node_message + id present and correct shape",
        True,
        f"node_message is {len(nm)}-key map; id decodes to 32 bytes"
    )


def verify(envelope, ca_path):
    return [
        _verify_cert_chain(envelope["ek_cert_pem"], ca_path),
        _verify_quote_openssl(envelope),
        _verify_pcr15_replay(envelope),
        _verify_node_msg_binding(envelope),
        _verify_node_msg_id_matches_content(envelope),
    ]


def main():
    if len(sys.argv) < 3:
        print("usage: verifier_hb.py <attestation.json> <ca.crt>",
              file=sys.stderr)
        sys.exit(1)
    raw = json.loads(pathlib.Path(sys.argv[1]).read_text())
    # HB wraps device responses in `{status, commitments, body}' — the
    # attestation envelope lives under `body'. Accept either shape.
    if isinstance(raw, dict) and "lapee_attestation_version" in raw:
        envelope = raw
    elif isinstance(raw, dict) and isinstance(raw.get("body"), dict):
        envelope = raw["body"]
    else:
        raise SystemExit(
            "could not find a lapee attestation envelope in input JSON")
    ca = pathlib.Path(sys.argv[2])
    print("=" * 68)
    print("LapEE (dev_tpm2) verifier")
    print("=" * 68)
    print(f"  wallet_address    : {envelope.get('wallet_address')}")
    print(f"  node_message_id   : {envelope.get('node_message_id')}")
    q15 = envelope['tpm_quote']['pcr_values'].get('15')
    print(f"  quoted pcr15      : {q15}")
    print()
    results = verify(envelope, ca)
    for r in results:
        print(r)
    ok = all(r.ok for r in results)
    print()
    print(f"VERDICT: {'ATTESTATION ACCEPTED' if ok else 'ATTESTATION REJECTED'}")
    print("=" * 68)
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
