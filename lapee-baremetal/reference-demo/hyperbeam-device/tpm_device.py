"""tpm_device.py — reference implementation of HyperBEAM's ~tpm@2.0a device.

Wraps tpm2-tools (running inside the lapee-tools Docker container against
the host's swtpm on TCP) to expose the operations described in the LapEE
paper §4.3:

  - create_ephemeral_key()  : TPM2_Create under the EK hierarchy; private
                              material never leaves the TPM; public key
                              returned so callers can PCR-extend it.
  - pcr_extend(idx, data)   : extend a PCR with the SHA-256 of `data` and
                              emit a named event log entry.
  - pcr_read(indices)       : read current PCR values.
  - quote(nonce, pcrs)      : TPM2_Quote, signed by a fresh Attestation
                              Key whose EK binding is provable.
  - sign(data)              : RSASSA-PSS signature over `data` using the
                              ephemeral signing key.
  - event_log_read()        : return the accumulated HyperBEAM runtime
                              event log entries.
  - ek_certificate()        : PEM-encoded EK certificate (for this
                              development environment, self-signed by our
                              test CA; on real hardware this is the TPM
                              vendor's EK cert).

Implementation note: the reference implementation uses tpm2-tools via
subprocess for simplicity. A production HyperBEAM NIF would use libtss2
directly via a small Rust or C shim.
"""

from __future__ import annotations

import base64
import hashlib
import json
import os
import pathlib
import secrets
import subprocess
import time
import typing as t
from dataclasses import dataclass, field


LAPEE_ROOT = pathlib.Path(__file__).resolve().parent.parent
WORK = LAPEE_ROOT / "work" / "tpm"
OUT = LAPEE_ROOT / "out"
CONTEXT_DIR = WORK / "contexts"
LOG_PATH = WORK / "hb-event-log.json"

WORK.mkdir(parents=True, exist_ok=True)
OUT.mkdir(parents=True, exist_ok=True)
CONTEXT_DIR.mkdir(parents=True, exist_ok=True)

DOCKER_IMAGE = os.environ.get("LAPEE_IMAGE", "lapee-tools:latest")
SWTPM_HOST = os.environ.get("SWTPM_HOST", "host.docker.internal")
SWTPM_PORT = int(os.environ.get("SWTPM_TPM_PORT", "2321"))


@dataclass
class EventLogEntry:
    """A single entry in HyperBEAM's runtime event log.

    Mirrors the TCG event log structure: PCR index, event type, value
    hashed into the PCR, and a human-readable data field for the
    verifier's policy evaluation.
    """
    seq: int
    pcr: int
    event_type: str
    digest_sha256: str
    data: dict


@dataclass
class TpmState:
    """In-memory view of the node's TPM interaction with swtpm."""
    event_log: list[EventLogEntry] = field(default_factory=list)
    ak_ctx_path: pathlib.Path | None = None
    signing_key_ctx_path: pathlib.Path | None = None
    signing_key_public_pem: bytes | None = None
    ek_cert_pem: bytes | None = None


def _run(cmd: list[str], input_bytes: bytes | None = None, check: bool = True) -> subprocess.CompletedProcess:
    """Run `cmd` inside the lapee-tools container with access to ./work."""
    full = [
        "docker", "run", "--rm",
        "-v", f"{LAPEE_ROOT / 'work'}:/work",
        "-v", f"{LAPEE_ROOT / 'out'}:/out",
        "-e", f"TPM2TOOLS_TCTI=swtpm:host={SWTPM_HOST},port={SWTPM_PORT}",
        "--network=host",
        "-w", "/work/tpm",
        DOCKER_IMAGE,
    ] + cmd
    return subprocess.run(full, input=input_bytes, check=check, capture_output=True)


def _bash(script: str) -> subprocess.CompletedProcess:
    return _run(["bash", "-c", script])


def _hex_digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


class TpmDevice:
    """Python reference implementation of the ~tpm@2.0a HyperBEAM device.

    Usage:
        tpm = TpmDevice()
        tpm.init_endorsement_context()   # sets up EK + AK
        tpm.load_or_create_ek_certificate()
        # ...measured-boot simulator extends PCRs 0,7,11,14 here...
        pk_pem = tpm.create_ephemeral_key()
        tpm.pcr_extend_event(15, "key-pubkey-extend", pk_pem, {"pubkey-pem": pk_pem.decode()})
        quote = tpm.quote(nonce=<32 bytes>, pcrs=[0,1,7,11,14,15])
        sig = tpm.sign(data=some_bytes)
    """

    def __init__(self) -> None:
        self.state = TpmState()
        # Clear any stale context files; we want a fresh session each run.
        for p in CONTEXT_DIR.glob("*"):
            p.unlink()
        LOG_PATH.unlink(missing_ok=True)

    # -- Lifecycle ----------------------------------------------------------

    def init_endorsement_context(self) -> None:
        """Establish the Endorsement primary (EK) and an Attestation Key.

        The EK is the primary whose public half chains to the TPM vendor's
        signing key via the EK certificate. The AK is a restricted signing
        key under the endorsement hierarchy, used for quotes. We keep the
        AK separate from the node's signing key so that attestation
        signatures and application signatures are cryptographically
        distinguishable.
        """
        # Flush leftover transient handles, then set up EK + AK in a
        # single container invocation so the session doesn't exhaust
        # swtpm's very small (~3) transient object slot count.
        _bash("""
            set -e
            # Best-effort flush of any persistent/transient state left
            # over from a prior partial run.
            tpm2_flushcontext -t 2>/dev/null || true
            tpm2_flushcontext -l 2>/dev/null || true
            tpm2_flushcontext -s 2>/dev/null || true

            # EK primary under the endorsement hierarchy.
            tpm2_createek -Q -c /work/tpm/contexts/ek.ctx -G rsa -u /work/tpm/contexts/ek.pub
            tpm2_flushcontext -t

            # AK under the EK.
            tpm2_createak -Q -C /work/tpm/contexts/ek.ctx \\
                -c /work/tpm/contexts/ak.ctx -G rsa -g sha256 -s rsassa \\
                -u /work/tpm/contexts/ak.pub -f pem -n /work/tpm/contexts/ak.name
            tpm2_flushcontext -t

            # Extract EK public in PEM form now (while we have context loaded).
            tpm2_readpublic -Q -c /work/tpm/contexts/ek.ctx \\
                -o /work/tpm/contexts/ek.pub.pem -f pem
            tpm2_flushcontext -t
        """)
        self.state.ak_ctx_path = CONTEXT_DIR / "ak.ctx"

    def load_or_create_ek_certificate(self) -> bytes:
        """Return the EK certificate chain.

        In this development environment we synthesize a test CA that
        plays the role the TPM vendor CA plays on real hardware. On a
        real LapEE deployment the EK cert comes from the TPM's factory-
        provisioned NV index (NV handle 0x01c00002 typically) and chains
        to the vendor's published root.

        For our purposes the structure matters: a verifier validates the
        chain against a known root, and LapEE A2 (TPM vendor signing
        keys uncompromised) is the load-bearing assumption either way.
        """
        ek_crt = OUT / "test-ek.crt"
        ca_crt = OUT / "test-tpm-ca.crt"
        ca_key_path = OUT / "test-tpm-ca.key"

        if not ca_crt.exists():
            self._create_test_ca(ca_key_path, ca_crt)
        if not ek_crt.exists():
            self._issue_ek_certificate(ca_key_path, ca_crt, ek_crt)

        self.state.ek_cert_pem = ek_crt.read_bytes()
        return self.state.ek_cert_pem

    @staticmethod
    def _create_test_ca(key_path: pathlib.Path, crt_path: pathlib.Path) -> None:
        """Create a self-signed test root CA (stands in for the TPM vendor root)."""
        from cryptography.hazmat.primitives import hashes, serialization
        from cryptography.hazmat.primitives.asymmetric import rsa
        from cryptography import x509
        from cryptography.x509.oid import NameOID
        import datetime as _dt

        key = rsa.generate_private_key(public_exponent=65537, key_size=2048)
        subject = x509.Name([
            x509.NameAttribute(NameOID.COMMON_NAME, "LapEE Test TPM Vendor Root CA"),
        ])
        cert = (
            x509.CertificateBuilder()
            .subject_name(subject)
            .issuer_name(subject)
            .public_key(key.public_key())
            .serial_number(x509.random_serial_number())
            .not_valid_before(_dt.datetime.now(_dt.timezone.utc))
            .not_valid_after(_dt.datetime.now(_dt.timezone.utc) + _dt.timedelta(days=3650))
            .add_extension(x509.BasicConstraints(ca=True, path_length=1), critical=True)
            .sign(private_key=key, algorithm=hashes.SHA256())
        )
        key_path.write_bytes(key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption(),
        ))
        crt_path.write_bytes(cert.public_bytes(serialization.Encoding.PEM))

    @staticmethod
    def _issue_ek_certificate(ca_key_path: pathlib.Path, ca_crt_path: pathlib.Path,
                              ek_crt_path: pathlib.Path) -> None:
        """Issue the EK certificate from the test CA.

        The EK public key is extracted during init_endorsement_context()
        in PEM form; we load it here and wrap it in a CA-signed X.509
        certificate playing the role of the TPM vendor's EK cert.
        """
        from cryptography.hazmat.primitives import hashes, serialization
        from cryptography import x509
        from cryptography.x509.oid import NameOID
        import datetime as _dt

        ek_pub_pem = (CONTEXT_DIR / "ek.pub.pem").read_bytes()
        ek_pub = serialization.load_pem_public_key(ek_pub_pem)

        ca_key = serialization.load_pem_private_key(ca_key_path.read_bytes(), password=None)
        ca_crt = x509.load_pem_x509_certificate(ca_crt_path.read_bytes())

        cert = (
            x509.CertificateBuilder()
            .subject_name(x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, "LapEE Test EK")]))
            .issuer_name(ca_crt.subject)
            .public_key(ek_pub)
            .serial_number(x509.random_serial_number())
            .not_valid_before(_dt.datetime.now(_dt.timezone.utc))
            .not_valid_after(_dt.datetime.now(_dt.timezone.utc) + _dt.timedelta(days=3650))
            .add_extension(x509.BasicConstraints(ca=False, path_length=None), critical=True)
            .sign(private_key=ca_key, algorithm=hashes.SHA256())
        )
        ek_crt_path.write_bytes(cert.public_bytes(serialization.Encoding.PEM))

    # -- PCR ----------------------------------------------------------------

    def pcr_extend_event(
        self,
        pcr: int,
        event_type: str,
        extend_bytes: bytes,
        data: dict,
    ) -> EventLogEntry:
        """Extend PCR `pcr` with SHA-256(extend_bytes) and record an event."""
        digest = hashlib.sha256(extend_bytes).hexdigest()
        _bash(f"tpm2_pcrextend -Q {pcr}:sha256=0x{digest}")
        entry = EventLogEntry(
            seq=len(self.state.event_log),
            pcr=pcr,
            event_type=event_type,
            digest_sha256=digest,
            data=data,
        )
        self.state.event_log.append(entry)
        return entry

    def pcr_read(self, indices: list[int]) -> dict[int, str]:
        sel = "sha256:" + ",".join(str(i) for i in indices)
        r = _run(["tpm2-pcrread", "-Q", sel + "+sha1:0"])  # tpm2-tools wants at least one banks list
        r = _run(["tpm2-pcrread", sel])
        # Parse tpm2_pcrread output, which looks like:
        #   sha256:
        #     0 : 0xAB...
        out = {}
        for line in r.stdout.decode().splitlines():
            line = line.strip()
            if ":" not in line or line.startswith("sha"):
                continue
            idx_s, val = line.split(":", 1)
            idx_s = idx_s.strip()
            if not idx_s.isdigit():
                continue
            out[int(idx_s)] = val.strip().replace("0x", "").lower()
        return out

    # -- Keys ---------------------------------------------------------------

    def create_ephemeral_key(self) -> bytes:
        """Create a fresh RSA-PSS signing key inside the TPM.

        Private material never leaves the TPM. Returns the PEM-encoded
        public key; the caller should PCR-extend this pubkey into PCR 15
        as the final measured-boot event (paper §4.2).
        """
        _bash("""
            set -e
            tpm2_flushcontext -t 2>/dev/null || true

            # Primary under the owner hierarchy. Separate from endorsement
            # hierarchy so attestation key (AK, endorsement) and signing
            # key (sign, owner) are cryptographically distinguishable.
            tpm2_createprimary -Q -C o \\
                -c /work/tpm/contexts/primary.ctx \\
                -g sha256 -G rsa
            tpm2_flushcontext -t

            # Create the signing key blob (pub + priv).
            tpm2_create -Q -C /work/tpm/contexts/primary.ctx \\
                -g sha256 -G rsa2048:rsapss-sha256:null \\
                -u /work/tpm/contexts/sign.pub \\
                -r /work/tpm/contexts/sign.priv
            tpm2_flushcontext -t

            # Load the signing key into a usable context.
            tpm2_load -Q -C /work/tpm/contexts/primary.ctx \\
                -u /work/tpm/contexts/sign.pub \\
                -r /work/tpm/contexts/sign.priv \\
                -c /work/tpm/contexts/sign.ctx
            tpm2_flushcontext -t

            # Extract the PEM public key while the context is valid.
            tpm2_readpublic -Q -c /work/tpm/contexts/sign.ctx \\
                -o /work/tpm/contexts/sign.pub.pem -f pem
            tpm2_flushcontext -t
        """)
        self.state.signing_key_ctx_path = CONTEXT_DIR / "sign.ctx"
        pk = (CONTEXT_DIR / "sign.pub.pem").read_bytes()
        self.state.signing_key_public_pem = pk
        return pk

    # -- Attestation --------------------------------------------------------

    def quote(self, nonce: bytes, pcrs: list[int]) -> dict:
        """Produce a TPM2_Quote over the given PCRs bound to `nonce`.

        Returns a dict with base64-encoded quote message, signature, and
        the AK public key so a verifier can validate without TPM access.
        """
        assert self.state.ak_ctx_path is not None
        sel = "sha256:" + ",".join(str(i) for i in pcrs)
        nonce_hex = nonce.hex()
        _bash(f"""
            set -e
            tpm2_flushcontext -t 2>/dev/null || true
            tpm2_quote -Q \\
                -c /work/tpm/contexts/ak.ctx \\
                -l {sel} \\
                -q {nonce_hex} \\
                -m /work/tpm/contexts/quote.msg \\
                -s /work/tpm/contexts/quote.sig \\
                -o /work/tpm/contexts/quote.pcrs \\
                -f plain -g sha256
            tpm2_flushcontext -t
        """)
        return {
            "message-b64": base64.b64encode((CONTEXT_DIR / "quote.msg").read_bytes()).decode(),
            "signature-b64": base64.b64encode((CONTEXT_DIR / "quote.sig").read_bytes()).decode(),
            "pcrs-b64": base64.b64encode((CONTEXT_DIR / "quote.pcrs").read_bytes()).decode(),
            "ak-pub-pem": (CONTEXT_DIR / "ak.pub").read_text(),
            "nonce-hex": nonce.hex(),
            "pcr-selection": pcrs,
        }

    def sign(self, data: bytes) -> dict:
        """Sign `data` with the ephemeral signing key using RSASSA-PSS."""
        assert self.state.signing_key_ctx_path is not None
        digest = hashlib.sha256(data).digest()
        (CONTEXT_DIR / "sign.data").write_bytes(data)
        (CONTEXT_DIR / "sign.digest").write_bytes(digest)
        # -f plain: strip the TPMT_SIGNATURE header so the raw RSA
        # signature bytes are emitted, verifiable by standard tooling.
        _bash("""
            set -e
            tpm2_flushcontext -t 2>/dev/null || true
            tpm2_sign -Q \\
                -c /work/tpm/contexts/sign.ctx \\
                -g sha256 -s rsapss \\
                -d /work/tpm/contexts/sign.digest \\
                -f plain \\
                -o /work/tpm/contexts/sign.sig
            tpm2_flushcontext -t
        """)
        return {
            "scheme": "RSASSA-PSS/SHA-256",
            "digest-b64": base64.b64encode(digest).decode(),
            "signature-b64": base64.b64encode((CONTEXT_DIR / "sign.sig").read_bytes()).decode(),
            "public-key-pem": self.state.signing_key_public_pem.decode(),
        }

    # -- Event log ----------------------------------------------------------

    def event_log_json(self) -> list[dict]:
        return [
            {
                "seq": e.seq,
                "pcr": e.pcr,
                "event-type": e.event_type,
                "digest-sha256": e.digest_sha256,
                "data": e.data,
            }
            for e in self.state.event_log
        ]

    def persist_event_log(self) -> None:
        LOG_PATH.write_text(json.dumps(self.event_log_json(), indent=2))
