"""AO-Core hashpath primitives.

A hashpath is a merkle-style chain of named events: each event's hash is
computed as H(prev_tip || H(name) || H(value)), and the chain tip
uniquely commits to the full ordered sequence. Structurally identical to
the TPM's extend primitive, applied at the application layer.

The LapEE paper (§5 "AO-Core Continuity") describes how the TPM event
log and the AO-Core hashpath compose end-to-end: the hashpath is seeded
from the TPM event log tip immediately after key-pubkey-extend, then
extends on every HyperBEAM device first-load and every message.

This module provides a minimal reference implementation sufficient to
produce the signed attestation artifact.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass, field
from typing import Any


def _h(*parts: bytes) -> bytes:
    """Hash a sequence of byte strings by concatenation."""
    m = hashlib.sha256()
    for p in parts:
        m.update(p)
    return m.digest()


def _as_bytes(value: Any) -> bytes:
    """Canonical byte encoding of an AO-Core event payload."""
    if isinstance(value, bytes):
        return value
    if isinstance(value, str):
        return value.encode("utf-8")
    # JSON with sorted keys so the canonical form is stable across
    # languages / implementations. Matches AO-Core's deterministic
    # encoding philosophy.
    return json.dumps(value, sort_keys=True, separators=(",", ":")).encode("utf-8")


@dataclass
class Event:
    """A single hashpath event."""
    name: str
    value: Any
    value_hash: str
    prev_tip: str
    new_tip: str


@dataclass
class HashPath:
    """Append-only merkle chain of named events.

    seed: the initial chain tip, typically a commitment to the TPM
          event log's final PCR state so the AO-Core chain extends the
          measured-boot chain without a discontinuity.
    """

    seed: bytes
    events: list[Event] = field(default_factory=list)

    @property
    def tip(self) -> bytes:
        if not self.events:
            return self.seed
        return bytes.fromhex(self.events[-1].new_tip)

    @property
    def tip_hex(self) -> str:
        return self.tip.hex()

    def extend(self, name: str, value: Any) -> Event:
        prev = self.tip
        vh = _h(_as_bytes(value))
        nh = _h(_h(name.encode("utf-8")), vh)
        new = _h(prev, nh)
        ev = Event(
            name=name,
            value=value,
            value_hash=vh.hex(),
            prev_tip=prev.hex(),
            new_tip=new.hex(),
        )
        self.events.append(ev)
        return ev

    def to_json(self) -> dict:
        return {
            "seed": self.seed.hex(),
            "tip": self.tip_hex,
            "events": [
                {
                    "name": e.name,
                    "value": e.value,
                    "value-hash": e.value_hash,
                    "prev-tip": e.prev_tip,
                    "new-tip": e.new_tip,
                }
                for e in self.events
            ],
        }

    @classmethod
    def replay(cls, data: dict) -> "HashPath":
        """Rebuild a hashpath from serialized form and verify each step.

        Returns a HashPath whose tip should equal data['tip']. Raises
        ValueError on any mismatch — a verifier uses this to prove the
        transcript has not been tampered with.
        """
        hp = cls(seed=bytes.fromhex(data["seed"]))
        for raw in data["events"]:
            ev = hp.extend(raw["name"], raw["value"])
            if ev.new_tip != raw["new-tip"]:
                raise ValueError(
                    f"hashpath divergence at event {raw['name']!r}: "
                    f"expected {raw['new-tip']}, got {ev.new_tip}"
                )
        if hp.tip_hex != data["tip"]:
            raise ValueError(f"final tip mismatch: {data['tip']} vs {hp.tip_hex}")
        return hp
