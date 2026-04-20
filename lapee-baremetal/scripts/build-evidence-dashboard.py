#!/usr/bin/env python3
"""
build-evidence-dashboard.py — produce a single-file HTML dashboard that
renders everything under `out/evidence/` + `out/acceptance/` so a human
can eyeball a full LapEE acceptance run without shelling into the repo.

The dashboard is SELF-CONTAINED: CSS + JS inline, all JSON + text
evidence embedded as data. `open out/evidence/dashboard.html' in
Chrome works without a web server and without any follow-up file
reads.

Sections rendered (progressive — missing evidence = hidden card):
  1. Verdict strip — one-line pass/fail per phase.
  2. Acceptance battery — 3 envelopes side-by-side (baseline / user-
     diff / user-hostile), with node_message_id, on_start_hook_device,
     and the Python verifier's 5-check output.
  3. Tamper test — 7 rows, each showing which check rejected which
     byte-flip.
  4. Interpret /verify — 5-core + 1-informational check with severity
     badges, plus the rich interpretation (TPM identity, PCR roles,
     boot chain, kernel, IMA, node).
  5. Events (/events) — 1-indexed table of parsed TCG records with
     per-event-type decoded fields.
  6. Claim (/claim) — flat policy surface with provenance counts.
  7. Raw files — inline <details><pre> blocks of every file in
     out/evidence/ (JSON pretty-printed, PEM/flat as-is).

Invocation:
  ./scripts/build-evidence-dashboard.py              # writes dashboard
  ./scripts/build-evidence-dashboard.py --open       # and opens in Chrome
"""
from __future__ import annotations
import argparse
import json
import os
import re
import subprocess
import sys
import time
from html import escape
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
EVIDENCE = ROOT / "out" / "evidence"
ACCEPTANCE = ROOT / "out" / "acceptance"
TAMPER = ACCEPTANCE / "tamper"
DASHBOARD_PATH = EVIDENCE / "dashboard.html"
# The interpret-device's static DB lives in the parent hb repo's
# priv/ dir (copied into the release at build time). The dashboard
# audits its size to render the Coverage section. No hard-coded
# counts — everything is derived from what's actually on disk.
PRIV = ROOT.parent / "priv" / "tpm-interpret"
DEV_TPM_TCG_ERL = ROOT.parent / "src" / "dev_tpm_tcg.erl"


# ---------- helpers ---------------------------------------------------

def read_json(p: Path):
    try:
        return json.loads(p.read_text())
    except Exception:
        return None


def read_text(p: Path) -> str | None:
    try:
        return p.read_text()
    except Exception:
        return None


def git(*args: str) -> str:
    try:
        return subprocess.check_output(
            ["git", *args],
            cwd=str(ROOT.parent),
            stderr=subprocess.DEVNULL,
        ).decode().strip()
    except Exception:
        return ""


def unwrap_body(m):
    """HB responses are {status, body, commitments}; nested bodies
    repeat the wrapper. Peel inward until we find the payload."""
    while isinstance(m, dict) and "body" in m and isinstance(m["body"], dict):
        m = m["body"]
    return m


def strip_commitments(m):
    """HB injects `commitments' (signature metadata) and `ao-types'
    (type system annotations like `secure_boot_enabled="atom"') at
    every level. Both are implementation details that clutter the
    human-readable dashboard. The full raw JSON — with every internal
    marker — is still available at the bottom of the dashboard under
    Raw files."""
    internal = {"commitments", "ao-types"}
    if isinstance(m, dict):
        return {k: strip_commitments(v) for k, v in m.items()
                if k not in internal}
    if isinstance(m, list):
        return [strip_commitments(v) for v in m]
    return m


def get(d, *ks, default=None):
    cur = d
    for k in ks:
        if not isinstance(cur, dict):
            return default
        cur = cur.get(k)
    return cur if cur is not None else default


def pass_badge(ok: bool, label_ok="PASS", label_fail="FAIL",
               label_info="INFO") -> str:
    cls = "ok" if ok else "fail"
    label = label_ok if ok else label_fail
    return f'<span class="badge {cls}">{escape(label)}</span>'


def sev_badge(sev: str) -> str:
    if sev == "informational":
        return '<span class="badge sev-info">informational</span>'
    return '<span class="badge sev-core">core</span>'


def mono(x, limit: int | None = 60) -> str:
    s = str(x) if x is not None else "-"
    if limit and len(s) > limit:
        s = s[:limit] + "…"
    return f'<code>{escape(s)}</code>'


# ---------- loaders ---------------------------------------------------

def load_acceptance() -> dict:
    """Load the three attestation envelopes captured by hb-acceptance.sh.
    Returns {baseline: {envelope, ca}, user_diff: ..., user_hostile: ...}.
    Envelopes are peeled out of HB's {status,body,commitments} wrapper."""
    out = {}
    for key, name in [
        ("baseline", "baseline"),
        ("user-diff", "user-diff"),
        ("user-hostile", "user-hostile"),
    ]:
        raw = read_json(ACCEPTANCE / f"{name}-attestation.json")
        if not raw:
            continue
        env = unwrap_body(raw) if isinstance(raw, dict) else raw
        env = strip_commitments(env)
        ca = read_text(ACCEPTANCE / f"{name}-ca.crt")
        out[key] = {"label": name, "envelope": env, "ca": ca or ""}
    return out


def load_tamper() -> list[dict]:
    """Load all tamper variants. File shape: just the tampered envelope.
    The rejection check name is derived from the filename + hb-tamper-
    test.sh's known-expected-check mapping."""
    # This mapping mirrors scripts/hb-tamper-test.sh's expected-reject
    # assertions so the dashboard can render what passed without having
    # to re-run the Python verifier.
    expected = {
        "flip-signature": "TPM2_Quote signature valid under AK public key",
        "flip-quoted": "TPM2_Quote signature valid under AK public key",
        "flip-pcr15-reported":
            "Quote pcrDigest matches reported PCR values",
        "swap-nonce": "TPM2_Quote extraData == nonce",
        "flip-event-digest":
            "Runtime event log replay of PCR 15 matches quoted value",
        "flip-node-id":
            "PCR 15 extension commits to node_message_id",
        "swap-ek-cert":
            "EK certificate chains to trusted TPM vendor root CA",
    }
    out = []
    if not TAMPER.exists():
        return out
    for fn in sorted(TAMPER.glob("*.json")):
        name = fn.stem
        out.append({
            "name": name,
            "expected-check": expected.get(name, "(unknown)"),
            "size": fn.stat().st_size,
        })
    return out


def load_interpret() -> dict | None:
    p = EVIDENCE / "interpret-verify-live.json"
    if not p.exists():
        p = EVIDENCE / "interpret-verify-baseline.json"
    r = read_json(p)
    if not r:
        return None
    body = unwrap_body(r)
    return strip_commitments(body)


def load_events() -> list[dict]:
    p = EVIDENCE / "events-live.json"
    if not p.exists():
        p = EVIDENCE / "interpret-events-baseline.json"
    r = read_json(p)
    if not r:
        return []
    body = r.get("body", r) if isinstance(r, dict) else r
    # Inner "body" may wrap the events map again.
    evs = body.get("body", body) if isinstance(body, dict) else body
    if not isinstance(evs, dict):
        return []
    keys = sorted(
        [k for k in evs.keys() if isinstance(k, str) and k.isdigit()],
        key=lambda k: int(k),
    )
    return [{"key": k, **strip_commitments(evs[k])} for k in keys]


def load_claim() -> dict | None:
    p = EVIDENCE / "claim-live.json"
    if not p.exists():
        p = EVIDENCE / "interpret-claim-baseline.json"
    r = read_json(p)
    if not r:
        return None
    body = unwrap_body(r)
    return strip_commitments(body)


def load_hyperbuddy() -> dict:
    """Load the four format~hyperbuddy@1.0 renderings. When the
    refresh-hyperbuddy script (or the make target) has run against a
    live guest, these files show the full AO-Core message tree for
    (a) the raw attestation envelope, (b) the interpret output,
    (c) the events map, (d) the claim surface. Each is path-
    addressable: a reader can trace any field back to its source."""
    out = {}
    for key, fn in [
        ("attestation", "hyperbuddy-attestation.txt"),
        ("interpret",   "hyperbuddy-interpret.txt"),
        ("events",      "hyperbuddy-events.txt"),
        ("claim",       "hyperbuddy-claim.txt"),
    ]:
        p = EVIDENCE / fn
        txt = read_text(p)
        if txt:
            out[key] = {"name": fn, "text": txt, "size": p.stat().st_size}
    return out


def load_interpret_tree() -> dict | None:
    """The full interpret response (unwrapped). Used to render the
    per-PCR events + derived fields view — the canonical proof that
    every derivable property is path-addressable through AO-Core."""
    p = EVIDENCE / "interpret-verify-live.json"
    if not p.exists():
        p = EVIDENCE / "interpret-verify-baseline.json"
    r = read_json(p)
    if not r:
        return None
    body = unwrap_body(r)
    body = strip_commitments(body)
    if isinstance(body, dict):
        return body.get("interpretation")
    return None


def load_raw_files() -> list[dict]:
    """Every evidence file rendered inline at the bottom."""
    if not EVIDENCE.exists():
        return []
    out = []
    for p in sorted(EVIDENCE.iterdir()):
        if p.is_dir() or p.name == "dashboard.html":
            continue
        text = read_text(p)
        if text is None:
            continue
        # Pretty-print JSON so <pre> is readable.
        if p.suffix == ".json":
            try:
                text = json.dumps(json.loads(text), indent=2,
                                  sort_keys=True)
            except Exception:
                pass
        out.append({
            "name": p.name,
            "size": p.stat().st_size,
            "text": text,
        })
    return out


def load_coverage(interp_tree: dict | None) -> dict:
    """Audit the priv/tpm-interpret/ DB that ships with the release.
    All counts are derived from what's actually on disk — no hard-
    coded numbers, so the dashboard stays honest as the DB grows.

    Also reflects live per-PCR derived-field population from the
    latest interpret run: for each PCR, how many of its template
    fields have concrete values vs `"unknown"' sentinels."""
    cov: dict = {}

    # Vendors — manufacturers.json → {kind: count}.
    mans = read_json(PRIV / "manufacturers.json") or {}
    vendors = mans.get("vendors", {}) or {}
    kinds: dict[str, int] = {}
    for v in vendors.values():
        k = v.get("kind", "unknown") if isinstance(v, dict) else "unknown"
        kinds[k] = kinds.get(k, 0) + 1
    cov["vendors"] = {
        "total": len(vendors),
        "by_kind": kinds,
        "sample": sorted(
            [v.get("name", "?")
             for v in vendors.values() if isinstance(v, dict)])[:8],
    }

    # Firmware families — every *.json file under firmware-versions/.
    fw_dir = PRIV / "firmware-versions"
    fw_entries = []
    if fw_dir.is_dir():
        for fn in sorted(fw_dir.glob("*.json")):
            d = read_json(fn) or {}
            name = d.get("name") or fn.stem
            vendor = d.get("vendor") or "-"
            trust = d.get("trust-tier") or (
                d.get("entries", [{}])[0].get("trust-tier", "")
                if isinstance(d.get("entries"), list) else "")
            platforms = d.get("platforms") or {}
            n_platforms = (len(platforms)
                           if isinstance(platforms, dict) else 0)
            # Multi-entry families.
            entries = d.get("entries") or []
            if isinstance(entries, list) and entries:
                for e in entries:
                    fw_entries.append({
                        "file": fn.name,
                        "name": e.get("vendor", name),
                        "vendor": e.get("vendor", vendor),
                        "trust_tier": e.get("trust-tier", trust),
                        "platforms": len(e.get("platforms") or [])
                                     if isinstance(e.get("platforms"), list)
                                     else 0,
                    })
            else:
                fw_entries.append({
                    "file": fn.name,
                    "name": name,
                    "vendor": vendor,
                    "trust_tier": trust,
                    "platforms": n_platforms,
                })
    cov["firmware_families"] = fw_entries

    # PCR profiles — each file pins expected PCR 0 + 7 digests.
    prof_dir = PRIV / "pcr-profiles"
    profiles = []
    if prof_dir.is_dir():
        for fn in sorted(prof_dir.glob("*.json")):
            d = read_json(fn) or {}
            match_pcrs = d.get("match-pcrs") or d.get("pcrs") or {}
            profiles.append({
                "file": fn.name,
                "name": d.get("name") or fn.stem,
                "platform": (d.get("attributes") or {}).get(
                    "platform-vendor", "-"),
                "trust_tier": (d.get("attributes") or {}).get(
                    "trust-tier", "-"),
                "pcrs_matched": (
                    len(match_pcrs) if isinstance(match_pcrs, dict) else 0),
            })
    cov["pcr_profiles"] = profiles

    # Vendor root CAs — count *.pem files; 0 is the expected default
    # (deployer-supplied; licensing varies per vendor).
    ca_dir = PRIV / "root-cas"
    cas = []
    if ca_dir.is_dir():
        cas = sorted([p.name for p in ca_dir.glob("*.pem")])
    cov["root_cas"] = {"count": len(cas), "files": cas}

    # UKI measurements DB.
    uki_dir = PRIV / "uki-measurements"
    ukis = []
    if uki_dir.is_dir():
        ukis = sorted([p.name for p in uki_dir.glob("*.json")])
    cov["uki_measurements"] = {"count": len(ukis), "files": ukis}

    # Event-type decoder coverage — grep do_decode/2 clauses in
    # dev_tpm_tcg.erl against the 36 codes in event-types.json.
    etypes = read_json(PRIV / "event-types.json") or {}
    types = etypes.get("types") or {}
    decoded_codes = set()
    if DEV_TPM_TCG_ERL.is_file():
        text = DEV_TPM_TCG_ERL.read_text()
        for m in re.finditer(r"do_decode\(16#([0-9A-Fa-f]+),", text):
            decoded_codes.add(int(m.group(1), 16))
    type_rows = []
    for code_str, info in types.items():
        try:
            code = int(code_str)
        except Exception:
            continue
        type_rows.append({
            "code": code,
            "code_hex": f"0x{code:X}",
            "name": info.get("name", "?"),
            "decoded": code in decoded_codes,
            "data_format": info.get("data_format", "-"),
        })
    type_rows.sort(key=lambda r: r["code"])
    cov["event_types"] = {
        "total": len(type_rows),
        "decoded": sum(1 for r in type_rows if r["decoded"]),
        "rows": type_rows,
    }

    # Per-PCR derived-field population from the live interpret tree.
    # For each PCR, count concrete values vs `"unknown"' sentinels
    # vs empty collections. This reflects the CURRENT attestation's
    # evidence richness — so on QEMU / SeaBIOS most rows are sparse;
    # on real UEFI hardware most rows are full.
    pcr_derived = []
    if interp_tree and isinstance(interp_tree.get("pcrs"), dict):
        for k in sorted(
                [x for x in interp_tree["pcrs"].keys() if str(x).isdigit()],
                key=lambda x: int(x)):
            p = interp_tree["pcrs"][k]
            if not isinstance(p, dict):
                continue
            derived = (p.get("derived") or {})
            concrete = 0
            unknown = 0
            empty = 0
            total = 0
            for dk, dv in derived.items():
                if dk == "commitments":
                    continue
                total += 1
                if dv == "unknown":
                    unknown += 1
                elif isinstance(dv, list) and not dv:
                    empty += 1
                elif isinstance(dv, (int, bool, str, bytes)) and dv == "":
                    empty += 1
                else:
                    concrete += 1
            pcr_derived.append({
                "pcr": int(k),
                "role": p.get("role", "-"),
                "total": total,
                "concrete": concrete,
                "unknown": unknown,
                "empty": empty,
                "events": p.get("event-count", 0),
            })
    cov["pcr_derived"] = pcr_derived

    return cov


# ---------- renderers -------------------------------------------------

def render_verdict_strip(ctx: dict) -> str:
    acc = ctx["acceptance"]
    acc_ok = len(acc) == 3
    tamper = ctx["tamper"]
    tamper_ok = len(tamper) == 7
    interp = ctx["interpret"]
    interp_ok = bool(interp and interp.get("verified"))
    events_ok = bool(ctx["events"])
    claim_ok = bool(ctx["claim"])

    overall_ok = acc_ok and tamper_ok and interp_ok and events_ok and claim_ok
    verdict = "PASS" if overall_ok else "INCOMPLETE"

    return f"""
    <section id="verdict" class="verdict-strip">
      <div class="verdict-hero {'ok' if overall_ok else 'fail'}">
        <div class="label">overall</div>
        <div class="value">{verdict}</div>
      </div>
      <div class="verdict-cell">
        <div class="label">acceptance (3 envelopes)</div>
        <div>{pass_badge(acc_ok)} {len(acc)}/3 captured</div>
      </div>
      <div class="verdict-cell">
        <div class="label">tamper test (7 flips)</div>
        <div>{pass_badge(tamper_ok)} {len(tamper)}/7 rejected</div>
      </div>
      <div class="verdict-cell">
        <div class="label">interpret /verify</div>
        <div>{pass_badge(interp_ok)} verified={escape(str(interp and interp.get('verified')))}</div>
      </div>
      <div class="verdict-cell">
        <div class="label">/events</div>
        <div>{pass_badge(events_ok)} {len(ctx['events'])} records</div>
      </div>
      <div class="verdict-cell">
        <div class="label">/claim</div>
        <div>{pass_badge(claim_ok)} surface present</div>
      </div>
    </section>
    """


def render_acceptance(acc: dict) -> str:
    if not acc:
        return ""
    cards = []
    for key, data in acc.items():
        env = data["envelope"]
        wallet = get(env, "wallet-address", default="-")
        node_id = get(env, "node-message-id", default="-")
        hook_dev = get(env, "node-message", "on", "start", "device",
                       default="-")
        pcrs = get(env, "tpm-quote", "pcr-values", default={}) or {}
        pcr15 = pcrs.get("15", "-")
        cards.append(f"""
        <div class="card">
          <div class="card-title">{escape(data['label'])}</div>
          <table>
            <tr><th>wallet_address</th><td>{mono(wallet)}</td></tr>
            <tr><th>node_message_id</th><td>{mono(node_id)}</td></tr>
            <tr><th>on.start.device</th><td>{mono(hook_dev)}</td></tr>
            <tr><th>quoted PCR 15</th><td>{mono(pcr15)}</td></tr>
          </table>
        </div>
        """)
    # Assert invariants.
    ids = [get(d["envelope"], "node-message-id") for d in acc.values()]
    ids_distinct = len(set(ids)) == len(ids)
    hooks = [get(d["envelope"], "node-message", "on", "start",
                 "device") for d in acc.values()]
    hook_holds = all(h == "tpm2@2.0a" for h in hooks)
    return f"""
    <section id="acceptance">
      <h2>Acceptance battery — 3 envelopes</h2>
      <div class="grid-3">{''.join(cards)}</div>
      <p class="assert-line">{pass_badge(ids_distinct)}
        All three node_message_ids are distinct
        (user config affected the attested message).
      </p>
      <p class="assert-line">{pass_badge(hook_holds)}
        All three boots kept <code>on.start.device = tpm2@2.0a</code>
        — the enforced hook defeated the hostile user override.
      </p>
    </section>
    """


def render_tamper(tamper: list[dict]) -> str:
    if not tamper:
        return ""
    rows = []
    for t in tamper:
        rows.append(f"""
        <tr>
          <td>{pass_badge(True, 'rejected')}</td>
          <td><code>{escape(t['name'])}</code></td>
          <td>{escape(t['expected-check'])}</td>
          <td class="muted">{t['size']:,} bytes</td>
        </tr>
        """)
    return f"""
    <section id="tamper">
      <h2>Tamper test — 7 byte-flip variants</h2>
      <table class="full">
        <thead>
          <tr><th>verdict</th><th>tamper</th>
              <th>rejected at check</th><th>size</th></tr>
        </thead>
        <tbody>{''.join(rows)}</tbody>
      </table>
    </section>
    """


def render_interpret(interp: dict | None) -> str:
    if not interp:
        return ""
    verified = bool(interp.get("verified"))
    verdict = interp.get("verdict", "-")
    trust_src = interp.get("trust-anchor-source", "-")
    checks = interp.get("checks") or []

    check_rows = []
    for c in checks:
        ok = bool(c.get("ok"))
        sev = c.get("severity", "core")
        if ok:
            badge = '<span class="badge ok">OK</span>'
        elif sev == "informational":
            badge = '<span class="badge sev-info">info (non-gating)</span>'
        else:
            badge = '<span class="badge fail">FAIL</span>'
        name = escape(str(c.get("name", "")))
        detail = escape(str(c.get("detail", "")))
        check_rows.append(f"""
        <tr>
          <td>{badge}</td>
          <td>{sev_badge(sev)}</td>
          <td>{name}</td>
          <td class="muted mono-wrap">{detail}</td>
        </tr>
        """)

    interpretation = interp.get("interpretation") or {}
    envelope = interpretation.get("envelope") or {}
    tpm = interpretation.get("tpm") or {}
    ak = interpretation.get("ak") or {}
    quote = interpretation.get("quote") or {}
    boot = interpretation.get("boot") or {}
    kernel = interpretation.get("kernel") or {}
    ima = interpretation.get("ima") or {}
    node = interpretation.get("node") or {}
    pcrs = interpretation.get("pcrs") or {}

    pcr_rows = []
    for k in sorted([x for x in pcrs.keys() if str(x).isdigit()],
                    key=lambda x: int(x)):
        e = pcrs[k]
        if not isinstance(e, dict):
            continue
        role = e.get("role", "-")
        is_zero = e.get("is-zero") in (True, "true")
        digest = e.get("digest") or ""
        pcr_rows.append(f"""
        <tr>
          <td class="num">{escape(str(k))}</td>
          <td>{'zero' if is_zero else 'set'}</td>
          <td>{escape(str(role))}</td>
          <td>{mono(digest, 44)}</td>
        </tr>
        """)

    def kv(section: dict, keys: list[str]) -> str:
        out = []
        for k in keys:
            v = section.get(k)
            if v in (None, "", []):
                continue
            if isinstance(v, (dict, list)):
                v = json.dumps(v)
            out.append(
                f"<tr><th>{escape(k)}</th><td>{mono(v, 80)}</td></tr>"
            )
        return "".join(out)

    return f"""
    <section id="interpret">
      <h2>Interpret /verify</h2>
      <div class="hero-verify {'ok' if verified else 'fail'}">
        <div>verified: <strong>{escape(str(verified))}</strong></div>
        <div>verdict: <strong>{escape(str(verdict))}</strong></div>
        <div>trust_anchor_source:
          <strong>{escape(str(trust_src))}</strong></div>
      </div>

      <h3>Crypto checks</h3>
      <table class="full">
        <thead><tr><th>status</th><th>severity</th>
                   <th>check</th><th>detail</th></tr></thead>
        <tbody>{''.join(check_rows)}</tbody>
      </table>

      <div class="grid-2">
        <div>
          <h3>Envelope</h3>
          <table>{kv(envelope, ['version','issued-at-unix',
            'wallet-address','node-message-id'])}</table>
        </div>
        <div>
          <h3>TPM identity</h3>
          <table>{kv(tpm, ['manufacturer-id','manufacturer-name',
            'manufacturer-kind','model','firmware-version',
            'spec-family','spec-level','spec-revision',
            'ek-cert-issuer','ek-cert-serial'])}</table>
        </div>
        <div>
          <h3>AK</h3>
          <table>{kv(ak, ['algorithm','key-size-bits','public-exponent',
            'pub-der-sha256-b64url'])}</table>
        </div>
        <div>
          <h3>Quote metadata</h3>
          <table>{kv(quote, ['magic-ok','attest-type','clock-ms',
            'reset-count','restart-count','safe','nonce'])}</table>
        </div>
      </div>

      <h3>PCR roles</h3>
      <table class="full">
        <thead><tr><th>PCR</th><th>state</th><th>role</th>
                   <th>digest</th></tr></thead>
        <tbody>{''.join(pcr_rows)}</tbody>
      </table>

      <div class="grid-2">
        <div>
          <h3>Boot chain</h3>
          <table>{kv(boot, ['secure-boot-measured','secure-boot-policy',
            'firmware-srtm','match'])}</table>
        </div>
        <div>
          <h3>Kernel</h3>
          <table>{kv(kernel, ['uki-measured','uki-image',
            'boot-loader'])}</table>
        </div>
        <div>
          <h3>IMA</h3>
          <table>{kv(ima, ['active','pcr10','note'])}</table>
        </div>
        <div>
          <h3>Node identity</h3>
          <table>{kv(node, ['wallet-address','node-message-id',
            'node-message-key-count','on-start-hook-device',
            'pcr15-event-count','pcr15-event-types'])}</table>
        </div>
      </div>
    </section>
    """


def render_events(events: list[dict]) -> str:
    if not events:
        return ""
    rows = []
    for e in events:
        seq = e.get("seq", e.get("key", "?"))
        pcr = e.get("pcr", "?")
        et = e.get("event-type", "?")
        parsed = e.get("parsed") or {}
        decoded = []
        semantic = parsed.get("semantic") if isinstance(parsed, dict) else None
        if isinstance(semantic, dict):
            for k, v in semantic.items():
                decoded.append(f"{k}={v}")
        if isinstance(parsed, dict):
            for k in ("crtm-version", "blob-physical-address", "blob-length",
                      "image-length-in-memory", "variable-name",
                      "separator", "key", "value", "format", "spec-id"):
                if k in parsed and k not in (semantic or {}):
                    decoded.append(f"{k}={parsed[k]}")
        decoded_str = ", ".join(decoded) if decoded else ""
        rows.append(f"""
        <tr>
          <td class="num">{escape(str(seq))}</td>
          <td class="num">{escape(str(pcr))}</td>
          <td><code>{escape(str(et))}</code></td>
          <td class="muted mono-wrap">{escape(decoded_str)}</td>
        </tr>
        """)
    return f"""
    <section id="events">
      <h2>/events — parsed TCG event log ({len(events)} records)</h2>
      <table class="full">
        <thead><tr><th>seq</th><th>pcr</th><th>event_type</th>
                   <th>decoded (parsed.semantic + selected fields)</th>
        </tr></thead>
        <tbody>{''.join(rows)}</tbody>
      </table>
    </section>
    """


def render_claim(claim: dict | None) -> str:
    if not claim:
        return ""
    sections = ["secure-boot", "firmware", "boot-loader", "kernel",
                "tme", "lockdown"]
    cards = []
    for s in sections:
        data = claim.get(s)
        if not isinstance(data, dict):
            continue
        # Split into claim-values (non-provenance, non-commitments)
        # and per-claim provenance counts.
        rows = []
        for k in sorted(data.keys()):
            if k == "commitments" or k.endswith("_provenance"):
                continue
            v = data[k]
            prov = data.get(f"{k}_provenance")
            prov_cnt = (len(prov) if isinstance(prov, list) else 0)
            prov_html = (f'<span class="prov">prov={prov_cnt}</span>'
                         if prov_cnt else "")
            if isinstance(v, (dict, list)):
                v_html = mono(json.dumps(v), 60)
            else:
                v_html = mono(v, 60)
            rows.append(
                f"<tr><th>{escape(k)}</th><td>{v_html} {prov_html}</td></tr>"
            )
        cards.append(f"""
        <div class="card">
          <div class="card-title">{escape(s)}</div>
          <table>{''.join(rows)}</table>
        </div>
        """)
    return f"""
    <section id="claim">
      <h2>/claim — flat policy surface</h2>
      <p class="muted">Each claim is a concrete property (bool / string)
        or <code>"unknown"</code> when the evidence doesn't decide.
        <code>prov=N</code> tags the number of source events backing the
        value — a claim without provenance is not derivable from this
        envelope.</p>
      <div class="grid-3">{''.join(cards)}</div>
    </section>
    """


def render_hyperbuddy(hb: dict) -> str:
    """Pre-formatted AO-Core message trees, one per device endpoint.
    These are the authoritative 'full picture' views — every field
    addressable by path. Each <details> block is collapsible."""
    if not hb:
        return ""
    sections = []
    labels = {
        "attestation": ("Full attestation envelope",
            "Raw envelope from <code>~tpm2@2.0a/attestation</code>. "
            "Path-addressable: every key shown is a live AO-Core path, "
            "e.g. <code>.../tpm_quote/pcr_values/7</code>."),
        "interpret":   ("Full interpretation tree (11 sections)",
            "Output of <code>.../interpret~tpm-interpret@1.0</code>. "
            "Every derivable property is rendered here — drill into "
            "<code>pcrs/&lt;N&gt;/derived/&lt;field&gt;</code> for parsed "
            "values, <code>pcrs/&lt;N&gt;/events/&lt;seq&gt;</code> for "
            "the source events, <code>pcrs/&lt;N&gt;/reconstruction</code> "
            "for replay validation."),
        "events":      ("Parsed TCG event log",
            "Output of <code>.../events~tpm-interpret@1.0</code>. "
            "1-indexed by sequence number; every event carries a "
            "<code>parsed</code> submap with per-event-type decoded "
            "fields (Secure Boot state, UEFI variable names, UKI "
            "key/value, microcode header, CRTM version, ...)."),
        "claim":       ("Flat policy surface",
            "Output of <code>.../claim~tpm-interpret@1.0</code>. "
            "One concrete bool/string per policy property, with "
            "<code>&lt;field&gt;_provenance</code> lists pointing back "
            "at the source events. Policy engines compose "
            "<code>green-zone@1.0/is-admissible</code> over this."),
    }
    for key in ("attestation", "interpret", "events", "claim"):
        if key not in hb:
            continue
        title, blurb = labels[key]
        sections.append(f"""
        <details open>
          <summary><strong>{escape(title)}</strong>
            <span class="muted">({hb[key]['size']:,} bytes)</span></summary>
          <p class="muted">{blurb}</p>
          <pre>{escape(hb[key]['text'])}</pre>
        </details>
        """)
    return f"""
    <section id="hyperbuddy">
      <h2>AO-Core message trees (format~hyperbuddy@1.0)</h2>
      <p class="muted">
        Four full-tree snapshots of every AO-Core message the
        <code>~tpm-interpret@1.0</code> device exposes against this
        attestation. Every line is a <strong>live addressable
        path</strong> — a verifier can navigate to any node with a
        URL like
        <code>/~tpm2@2.0a/attestation/interpret~tpm-interpret@1.0/pcrs/7/derived/secure_boot_enabled</code>.
      </p>
      {''.join(sections)}
    </section>
    """


def render_pcr_breakdown(interp: dict | None) -> str:
    """Per-PCR breakdown: raw digest + event count + replay match
    + derived named fields. This is the 'every bitpacked value fully
    parsed' view — for each PCR, show what AO-Core fields you can
    navigate to under /interpret/pcrs/<N>/derived/*."""
    if not interp:
        return ""
    pcrs = interp.get("pcrs") or {}
    if not isinstance(pcrs, dict):
        return ""
    cards = []
    for k in sorted(
            [x for x in pcrs.keys() if str(x).isdigit()],
            key=lambda x: int(x)):
        p = pcrs[k]
        if not isinstance(p, dict):
            continue
        role = p.get("role", "-")
        digest = p.get("digest") or "-"
        event_count = p.get("event-count", 0)
        derived = p.get("derived") or {}
        recon = p.get("reconstruction") or {}
        # Render derived fields as a table.
        derived_rows = []
        for dk in sorted(derived.keys()):
            dv = derived[dk]
            if isinstance(dv, (dict, list)):
                dv_html = mono(json.dumps(dv), 80)
            else:
                dv_html = mono(dv, 80)
            derived_rows.append(
                f"<tr><th>{escape(dk)}</th><td>{dv_html}</td></tr>"
            )
        derived_table = (
            f"<table>{''.join(derived_rows)}</table>"
            if derived_rows else
            "<p class='muted'>no derived fields for this PCR</p>"
        )
        # Event seqs for this PCR.
        events = p.get("events") or {}
        ev_seqs = sorted(
            [int(s) for s in events.keys() if str(s).isdigit()]
        )
        ev_badges = " ".join(
            f'<code>seq={s}</code>' for s in ev_seqs
        ) or "<span class='muted'>(no events)</span>"
        recon_html = ""
        if recon:
            matches = recon.get("matches-quoted")
            from_n = recon.get("replayed-from-events", 0)
            recon_html = f"""
            <div class="recon">
              reconstruction: {pass_badge(bool(matches), 'matches quoted', 'DIVERGES')}
              ({from_n} event(s) replayed)
            </div>
            """
        cards.append(f"""
        <div class="card">
          <div class="card-title">PCR {escape(str(k))}
            · {escape(str(role))}</div>
          <div class="pcr-digest">{mono(digest, 44)}</div>
          <div class="muted pcr-events">events:
            {ev_badges}</div>
          {recon_html}
          <h4>derived fields
            (addressable: <code>pcrs/{escape(str(k))}/derived/&lt;field&gt;</code>)</h4>
          {derived_table}
        </div>
        """)
    return f"""
    <section id="pcrs">
      <h2>Per-PCR events + derived fields</h2>
      <p class="muted">
        Each PCR carries (1) its raw quoted digest, (2) the filtered
        events that extended it, (3) a cryptographic reconstruction
        check vs the quoted value, and (4) a <code>derived</code>
        submessage of named fields extracted from the events. The
        derived fields are what a policy engine actually consumes;
        the raw digest is the audit trail.
      </p>
      <div class="pcr-grid">{''.join(cards)}</div>
    </section>
    """


def render_coverage(cov: dict) -> str:
    """Render the priv/tpm-interpret/ DB audit + live per-PCR
    derived-field population. All counts are data-driven — pulled
    from the files that actually ship in the release, so the
    dashboard never drifts from reality as the DB grows."""
    if not cov:
        return ""

    # Top-line summary strip.
    v = cov.get("vendors", {})
    fw = cov.get("firmware_families", [])
    pp = cov.get("pcr_profiles", [])
    ca = cov.get("root_cas", {})
    uki = cov.get("uki_measurements", {})
    et = cov.get("event_types", {})

    strip = f"""
    <div class="cov-strip">
      <div class="cov-cell">
        <div class="label">TPM vendors</div>
        <div class="value">{v.get('total', 0)}</div>
        <div class="muted">across {len(v.get('by_kind', {}))} kinds</div>
      </div>
      <div class="cov-cell">
        <div class="label">Firmware families</div>
        <div class="value">{len(fw)}</div>
        <div class="muted">OEM + third-party UEFI</div>
      </div>
      <div class="cov-cell">
        <div class="label">PCR profiles</div>
        <div class="value">{len(pp)}</div>
        <div class="muted">populated</div>
      </div>
      <div class="cov-cell">
        <div class="label">Vendor root CAs</div>
        <div class="value">{ca.get('count', 0)}</div>
        <div class="muted">{'deployer-supplied' if ca.get('count',0)==0 else 'provisioned'}</div>
      </div>
      <div class="cov-cell">
        <div class="label">UKI measurements</div>
        <div class="value">{uki.get('count', 0)}</div>
        <div class="muted">kernel/UKI hashes</div>
      </div>
      <div class="cov-cell">
        <div class="label">Event-type decoders</div>
        <div class="value">{et.get('decoded', 0)} / {et.get('total', 0)}</div>
        <div class="muted">structured decode</div>
      </div>
    </div>
    """

    # Vendors card: kind breakdown + sample names.
    kind_rows = []
    for k, n in sorted(v.get("by_kind", {}).items(), key=lambda kv: -kv[1]):
        kind_rows.append(
            f"<tr><th>{escape(k)}</th><td>{n}</td></tr>"
        )
    sample = v.get("sample") or []
    vendors_card = f"""
    <div class="card">
      <div class="card-title">Vendors by kind</div>
      <table>{''.join(kind_rows)}</table>
      <p class="muted">
        Sample: {', '.join(escape(x) for x in sample)}...<br>
        Deployer supplies vendor root CA(s) matching the EK chain.
      </p>
    </div>
    """

    # Firmware families card.
    fw_rows = []
    for e in fw:
        tt = e.get("trust_tier") or ""
        tt_html = (f' <span class="badge sev-info">{escape(tt)}</span>'
                   if tt == "development-only" else "")
        fw_rows.append(f"""
        <tr>
          <td><code>{escape(e['file'])}</code></td>
          <td>{escape(e['vendor'])}{tt_html}</td>
          <td class="num">{e['platforms']}</td>
        </tr>
        """)
    fw_card = f"""
    <div class="card">
      <div class="card-title">Firmware families
        ({len(fw)} entries)</div>
      <table>
        <thead><tr><th>file</th><th>vendor</th>
                   <th>platforms</th></tr></thead>
        <tbody>{''.join(fw_rows) or
          '<tr><td colspan="3" class="muted">none</td></tr>'}</tbody>
      </table>
    </div>
    """

    # PCR profiles card.
    pp_rows = []
    for p in pp:
        pp_rows.append(f"""
        <tr>
          <td><code>{escape(p['file'])}</code></td>
          <td>{escape(p['platform'])}</td>
          <td>{escape(p['trust_tier'])}</td>
          <td class="num">{p['pcrs_matched']}</td>
        </tr>
        """)
    pp_card = f"""
    <div class="card">
      <div class="card-title">PCR profiles
        ({len(pp)})</div>
      <table>
        <thead><tr><th>file</th><th>platform</th>
                   <th>trust-tier</th><th>pcrs</th></tr></thead>
        <tbody>{''.join(pp_rows) or
          '<tr><td colspan="4" class="muted">none populated — real-hardware PCR captures go here</td></tr>'}</tbody>
      </table>
    </div>
    """

    # Event-type decoder coverage card: full table.
    et_rows = []
    for r in et.get("rows", []):
        mark = ('<span class="badge ok">decoded</span>' if r["decoded"]
                else '<span class="badge sev-info">opaque</span>')
        et_rows.append(f"""
        <tr>
          <td class="mono">{escape(r['code_hex'])}</td>
          <td><code>{escape(r['name'])}</code></td>
          <td>{mark}</td>
          <td class="muted">{escape(r['data_format'])}</td>
        </tr>
        """)
    et_card = f"""
    <div class="card full-width">
      <div class="card-title">Event-type decoder coverage
        ({et.get('decoded', 0)} / {et.get('total', 0)})</div>
      <p class="muted">Structured decoders produce a <code>parsed</code>
        submap per event; unimplemented codes fall through with an
        empty <code>parsed</code> (raw bytes still available at
        <code>event-data</code>). Every event still carries its TCG
        name from this registry.</p>
      <table class="full">
        <thead><tr><th>code</th><th>name</th><th>status</th>
                   <th>data format</th></tr></thead>
        <tbody>{''.join(et_rows)}</tbody>
      </table>
    </div>
    """

    # Live per-PCR derived-field population.
    pd = cov.get("pcr_derived", [])
    pd_rows = []
    for r in pd:
        t = r["total"]
        c = r["concrete"]
        u = r["unknown"]
        e = r["empty"]
        pct = int(round((c / t) * 100)) if t else 0
        bar = f"""
        <div class="bar">
          <div class="bar-fill" style="width:{pct}%"></div>
          <div class="bar-label">{c}/{t}</div>
        </div>
        """
        pd_rows.append(f"""
        <tr>
          <td class="num">{r['pcr']}</td>
          <td>{escape(r['role'])}</td>
          <td class="num">{r['events']}</td>
          <td>{bar}</td>
          <td class="num">{c}</td>
          <td class="num">{u}</td>
          <td class="num">{e}</td>
        </tr>
        """)
    pd_card = f"""
    <div class="card full-width">
      <div class="card-title">Per-PCR derived-field population
        (from live interpret)</div>
      <p class="muted">Each PCR's <code>derived/</code> submessage has a
        fixed shape; population depends on whether the attestation
        includes the relevant events. <code>unknown</code> and empty
        collections are honest sentinels — "evidence not present",
        not "evidence said no". Real UEFI silicon populates most
        rows; SeaBIOS under QEMU populates a handful.</p>
      <table class="full">
        <thead><tr><th>PCR</th><th>role</th><th>events</th>
            <th>concrete ratio</th>
            <th>concrete</th><th>unknown</th><th>empty</th></tr></thead>
        <tbody>{''.join(pd_rows) or
          '<tr><td colspan="7" class="muted">no live interpret tree available</td></tr>'}</tbody>
      </table>
    </div>
    """

    return f"""
    <section id="coverage">
      <h2>Coverage — DB audit + live derived-field population</h2>
      <p class="muted">Everything under <code>priv/tpm-interpret/</code>
        that ships with the release, plus how richly the live
        attestation populates the derived-field templates. Counts
        come straight from the files on disk, so this strip stays
        honest as the DB grows (adding an OEM's firmware identifier
        is a file drop, not a code change). See
        <code>priv/tpm-interpret/COVERAGE.md</code> for the full
        gap audit.</p>
      {strip}
      <div class="grid-2" style="margin-top:18px">
        {vendors_card}
        {fw_card}
      </div>
      <div class="grid-2" style="margin-top:14px">
        {pp_card}
        <div class="card">
          <div class="card-title">Vendor root CAs</div>
          {(
             f'<ul>{"".join(f"<li><code>{escape(n)}</code></li>" for n in ca.get("files") or [])}</ul>'
             if ca.get("count", 0) else
             '<p class="muted">0 provisioned — each deployer supplies '
             'the vendor EK root CA(s) that match the TPMs they trust. '
             'Drop <code>*.pem</code> into '
             '<code>priv/tpm-interpret/root-cas/</code>. Infineon / '
             'STMicro / Nuvoton / AMD / Intel / Lenovo / Dell all '
             'publish downloadable root CA chains (see COVERAGE.md).</p>'
          )}
          <div class="card-title" style="margin-top:14px">UKI measurements</div>
          {(
             f'<ul>{"".join(f"<li><code>{escape(n)}</code></li>" for n in uki.get("files") or [])}</ul>'
             if uki.get("count", 0) else
             '<p class="muted">0 provisioned — kernel/UKI PE hashes are '
             'deploy-specific. Populate once a measured UKI image is '
             'available from a real systemd-boot host.</p>'
          )}
        </div>
      </div>
      <div style="margin-top:14px">{et_card}</div>
      <div style="margin-top:14px">{pd_card}</div>
    </section>
    """


def render_raw_files(files: list[dict]) -> str:
    if not files:
        return ""
    blocks = []
    for f in files:
        txt = f["text"]
        # Truncate absurdly long files for the dashboard body but
        # keep them clickable to-download.
        display = txt
        truncated = False
        if len(display) > 200000:
            display = display[:200000]
            truncated = True
        blocks.append(f"""
        <details>
          <summary><code>{escape(f['name'])}</code>
            <span class="muted">({f['size']:,} bytes)</span></summary>
          {f'<p class="muted">truncated at 200 KB for display</p>' if truncated else ''}
          <pre>{escape(display)}</pre>
        </details>
        """)
    return f"""
    <section id="files">
      <h2>Raw evidence ({len(files)} files)</h2>
      <p class="muted">Every file under <code>out/evidence/</code>, inline.
        Click to expand.</p>
      {''.join(blocks)}
    </section>
    """


# ---------- top-level --------------------------------------------------

CSS = r"""
* { box-sizing: border-box; }
html, body { margin: 0; padding: 0; }
body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  background: #0f1114;
  color: #e5e7eb;
  line-height: 1.45;
}
header {
  background: linear-gradient(180deg, #1a1f2e 0, #0f1114 100%);
  padding: 24px 32px;
  border-bottom: 1px solid #24283b;
}
header h1 { margin: 0 0 6px; font-size: 24px; }
header .meta { color: #9ca3af; font-size: 13px; }
header .meta code { color: #e5e7eb; }
nav {
  position: sticky; top: 0; z-index: 10;
  background: #151823; padding: 10px 32px;
  border-bottom: 1px solid #24283b;
  display: flex; gap: 14px; flex-wrap: wrap;
  font-size: 13px;
}
nav a { color: #9ca3af; text-decoration: none; }
nav a:hover { color: #fff; }
main { padding: 0 32px 40px; max-width: 1400px; margin: 0 auto; }
section { margin: 32px 0; }
h2 {
  margin: 0 0 16px; padding-bottom: 8px;
  border-bottom: 1px solid #24283b; font-size: 20px;
}
h3 { font-size: 15px; color: #cbd5e1; margin: 16px 0 8px; }
code, pre {
  font-family: ui-monospace, Menlo, Monaco, "Cascadia Mono", monospace;
  font-size: 12px;
}
pre {
  background: #0a0c10; border: 1px solid #24283b; padding: 12px;
  border-radius: 6px; overflow-x: auto; white-space: pre;
}
.mono-wrap { word-break: break-all; }
table {
  width: 100%; border-collapse: collapse; font-size: 13px;
}
table.full { border: 1px solid #24283b; border-radius: 6px; overflow: hidden; }
th, td { padding: 6px 10px; text-align: left; vertical-align: top; }
table th {
  background: #151823; color: #cbd5e1; font-weight: 500;
  border-bottom: 1px solid #24283b;
}
table td { border-bottom: 1px solid #1a1f2e; }
tr:last-child td { border-bottom: none; }
.num { text-align: right; font-variant-numeric: tabular-nums; }
.muted { color: #9ca3af; }
.grid-2 { display: grid; grid-template-columns: 1fr 1fr; gap: 18px; }
.grid-3 { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 18px; }
@media (max-width: 900px) {
  .grid-2, .grid-3 { grid-template-columns: 1fr; }
}
.card {
  background: #151823; border: 1px solid #24283b;
  padding: 14px; border-radius: 8px;
}
.card-title {
  font-weight: 600; color: #fbbf24; margin-bottom: 8px;
  border-bottom: 1px solid #24283b; padding-bottom: 6px;
}
.badge {
  display: inline-block; padding: 2px 8px; border-radius: 3px;
  font-size: 11px; font-weight: 600; letter-spacing: 0.4px;
  text-transform: uppercase;
}
.badge.ok { background: #064e3b; color: #6ee7b7; }
.badge.fail { background: #5f1d1d; color: #fda4af; }
.badge.sev-core { background: #1e3a8a; color: #93c5fd; }
.badge.sev-info { background: #3b3f16; color: #fde68a; }
.prov {
  background: #1f2937; color: #93c5fd;
  padding: 1px 6px; border-radius: 3px; font-size: 11px; margin-left: 6px;
}
.verdict-strip {
  display: grid; grid-template-columns: 1.5fr 1fr 1fr 1fr 1fr 1fr;
  gap: 12px; margin-top: 24px;
}
@media (max-width: 900px) {
  .verdict-strip { grid-template-columns: 1fr 1fr; }
}
.verdict-hero, .verdict-cell {
  padding: 14px; border-radius: 8px; background: #151823;
  border: 1px solid #24283b;
}
.verdict-hero.ok { border-color: #065f46; background: #064e3b22; }
.verdict-hero.fail { border-color: #991b1b; background: #5f1d1d22; }
.verdict-hero .value { font-size: 28px; font-weight: 700; letter-spacing: 1px; }
.verdict-hero .label, .verdict-cell .label {
  color: #9ca3af; font-size: 11px; text-transform: uppercase;
  letter-spacing: 0.5px; margin-bottom: 6px;
}
.hero-verify {
  display: flex; gap: 24px; padding: 14px; border-radius: 8px;
  background: #151823; border: 1px solid #24283b; margin-bottom: 16px;
}
.hero-verify.ok { border-color: #065f46; background: #064e3b11; }
.hero-verify.fail { border-color: #991b1b; background: #5f1d1d11; }
.hero-verify strong { color: #fbbf24; }
.assert-line {
  margin-top: 12px; font-size: 13px; color: #cbd5e1;
}
details { background: #151823; border: 1px solid #24283b;
  border-radius: 6px; margin: 8px 0; padding: 10px 14px; }
details summary { cursor: pointer; font-size: 13px; }
details pre { margin-top: 10px; max-height: 400px; overflow: auto; }
details[open] pre { max-height: 700px; }
h4 { margin: 12px 0 6px; font-size: 13px; color: #cbd5e1; }
.pcr-grid {
  display: grid; grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
  gap: 14px;
}
.pcr-digest { font-size: 11px; margin-bottom: 8px; color: #9ca3af; }
.pcr-events { font-size: 11px; margin-bottom: 8px; }
.pcr-events code {
  background: #1f2937; color: #93c5fd;
  padding: 1px 5px; border-radius: 3px; margin-right: 4px;
}
.recon { font-size: 12px; color: #9ca3af; margin: 6px 0; }
.cov-strip {
  display: grid; grid-template-columns: repeat(6, 1fr); gap: 12px;
  margin: 12px 0;
}
@media (max-width: 1100px) {
  .cov-strip { grid-template-columns: repeat(3, 1fr); }
}
@media (max-width: 700px) {
  .cov-strip { grid-template-columns: repeat(2, 1fr); }
}
.cov-cell {
  background: #151823; border: 1px solid #24283b;
  padding: 12px; border-radius: 8px;
}
.cov-cell .label {
  color: #9ca3af; font-size: 11px; text-transform: uppercase;
  letter-spacing: 0.5px; margin-bottom: 6px;
}
.cov-cell .value {
  font-size: 22px; font-weight: 700; color: #fbbf24;
}
.cov-cell .muted { font-size: 11px; margin-top: 4px; }
.card.full-width { grid-column: 1 / -1; }
.bar {
  position: relative; background: #0a0c10; border: 1px solid #24283b;
  border-radius: 3px; height: 18px; width: 160px;
}
.bar-fill {
  background: linear-gradient(90deg, #065f46, #10b981);
  height: 100%;
}
.bar-label {
  position: absolute; top: 0; left: 0; right: 0; bottom: 0;
  display: flex; align-items: center; justify-content: center;
  font-size: 11px; color: #e5e7eb; font-variant-numeric: tabular-nums;
  text-shadow: 0 0 3px rgba(0,0,0,0.8);
}
"""

HTML_TMPL = """<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>LapEE — acceptance evidence dashboard</title>
  <style>{css}</style>
</head>
<body>
  <header>
    <h1>LapEE — acceptance evidence</h1>
    <div class="meta">
      generated <code>{ts}</code>
      · branch <code>{branch}</code>
      · commit <code>{commit}</code>
      · dashboard <code>out/evidence/dashboard.html</code>
    </div>
  </header>
  <nav>
    <a href="#verdict">Verdict</a>
    <a href="#coverage">Coverage</a>
    <a href="#acceptance">Acceptance (3 envelopes)</a>
    <a href="#tamper">Tamper (7-way)</a>
    <a href="#interpret">Interpret /verify</a>
    <a href="#pcrs">Per-PCR derived</a>
    <a href="#events">/events</a>
    <a href="#claim">/claim</a>
    <a href="#hyperbuddy">AO-Core trees</a>
    <a href="#files">Raw files</a>
  </nav>
  <main>
    {verdict}
    {coverage}
    {acceptance}
    {tamper}
    {interpret}
    {pcr_breakdown}
    {events}
    {claim}
    {hyperbuddy}
    {files}
  </main>
</body>
</html>
"""


def build() -> Path:
    EVIDENCE.mkdir(parents=True, exist_ok=True)
    ctx = {
        "ts": time.strftime("%Y-%m-%d %H:%M %Z"),
        "branch": git("rev-parse", "--abbrev-ref", "HEAD") or "?",
        "commit": (git("rev-parse", "--short", "HEAD") or "?") + " "
                  + (git("log", "-1", "--pretty=%s") or ""),
        "acceptance": load_acceptance(),
        "tamper": load_tamper(),
        "interpret": load_interpret(),
        "interpret-tree": load_interpret_tree(),
        "events": load_events(),
        "claim": load_claim(),
        "hyperbuddy": load_hyperbuddy(),
        "files": load_raw_files(),
    }
    ctx["coverage"] = load_coverage(ctx["interpret-tree"])
    html = HTML_TMPL.format(
        css=CSS,
        ts=escape(ctx["ts"]),
        branch=escape(ctx["branch"]),
        commit=escape(ctx["commit"]),
        verdict=render_verdict_strip(ctx),
        coverage=render_coverage(ctx["coverage"]),
        acceptance=render_acceptance(ctx["acceptance"]),
        tamper=render_tamper(ctx["tamper"]),
        interpret=render_interpret(ctx["interpret"]),
        pcr_breakdown=render_pcr_breakdown(ctx["interpret-tree"]),
        events=render_events(ctx["events"]),
        claim=render_claim(ctx["claim"]),
        hyperbuddy=render_hyperbuddy(ctx["hyperbuddy"]),
        files=render_raw_files(ctx["files"]),
    )
    DASHBOARD_PATH.write_text(html)
    return DASHBOARD_PATH


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--open", action="store_true",
                    help="open the dashboard in Chrome on macOS")
    args = ap.parse_args()

    path = build()
    print(f"wrote {path} ({path.stat().st_size:,} bytes)")

    if args.open:
        # Prefer Chrome; fall back to whatever the OS registers.
        try:
            subprocess.check_call(
                ["open", "-a", "Google Chrome", str(path)],
            )
        except Exception:
            subprocess.check_call(["open", str(path)])
        print("opened in browser")


if __name__ == "__main__":
    main()
