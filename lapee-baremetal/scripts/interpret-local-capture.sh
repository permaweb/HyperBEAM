#!/usr/bin/env bash
# interpret-local-capture.sh — run the ~tpm-interpret@1.0 parser
# on a TCG event log captured from a real machine, and produce
# a per-machine HTML dashboard.
#
# Usage:
#   ./scripts/interpret-local-capture.sh path/to/eventlog.bin
#   ./scripts/interpret-local-capture.sh --label "Framework 13 Ryzen" fw.bin
#   ./scripts/interpret-local-capture.sh --ima ima-ascii.log fw.bin
#
# Expects one argument: path to a binary TCG event log. Writes
# claim.json, interpret.json, interpret.txt, input-preview.txt,
# and dashboard.html to out/local-capture/<slug>/. Opens the
# dashboard in Chrome at the end.
#
# To capture an event log on:
#
#   Linux:
#     sudo cat /sys/kernel/security/tpm0/binary_bios_measurements > fw.bin
#     # IMA (optional):
#     sudo cat /sys/kernel/security/ima/ascii_runtime_measurements > fw.ima
#
#   Windows (PowerShell, admin):
#     Copy-Item C:\Windows\Logs\MeasuredBoot\*.log fw-wbcl.log
#     # pick the most recent MeasuredBoot log
#
#   Any bootable Linux USB / Fedora / Ubuntu liveCD is enough —
#   no install required, just boot it and run the Linux commands
#   above, copy the .bin off via USB stick or scp.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
REPO="$(cd "$ROOT/.." && pwd)"

LABEL="Local capture"
IMA_LOG=""
INPUT=""
URL=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --label) LABEL="$2"; shift 2 ;;
        --ima)   IMA_LOG="$2"; shift 2 ;;
        --url)   URL="$2"; shift 2 ;;
        -h|--help)
            grep -E '^# ' "$0" | sed 's/^# \{0,1\}//' ; exit 0 ;;
        *)       INPUT="$1"; shift ;;
    esac
done

# --url NODE  pulls the FULL attestation envelope over HTTP
# via `~tpm2@2.0a/attestation' and extracts `tcg-event-log'
# from the attested payload. This is the ONLY legitimate over-
# the-wire capture path — the envelope binds the TPM quote,
# the event log, the runtime log, and the node message into
# one attested message. Typical:
#   ./interpret-local-capture.sh --url http://framework.local:8734 \\
#                                 --label Framework
if [[ -n "$URL" && -z "$INPUT" ]]; then
    URL="${URL%/}"
    TMP="$(mktemp -t fw-attestation.XXXXXX)"
    echo ">> fetching ${URL}/~tpm2@2.0a/attestation"
    if ! curl -fsSL "${URL}/~tpm2@2.0a/attestation" \
           -H "accept: application/json@1.0" \
           -H "accept-bundle: true" \
           -o "$TMP.json"; then
        echo "error: fetch from ${URL} failed" >&2
        exit 2
    fi
    python3 -c "
import base64, json, sys
doc = json.load(open('$TMP.json'))
body = doc
while isinstance(body, dict) and 'body' in body and \
       isinstance(body['body'], dict):
    body = body['body']
raw = base64.urlsafe_b64decode(
    body.get('tcg-event-log', '') + '==')
sys.stdout.buffer.write(raw)
src = body.get('tcg-event-log-source-path', 'unknown')
fmt = body.get('tcg-event-log-format', 'unknown')
nbytes = body.get('tcg-event-log-length-bytes', len(raw))
print(f'>> attested tcg-event-log: {nbytes} bytes '
      f'from {src} (format={fmt})',
      file=sys.stderr)
" > "$TMP.bin"
    if [[ ! -s "$TMP.bin" ]]; then
        echo "error: attested event log is empty or missing" >&2
        echo "       (is a TPM driver loaded on that host? "
        echo "        this mode requires a working TPM+AK for "
        echo "        the /attestation call to succeed. ssh in "
        echo "        and run 'sudo cat /sys/kernel/security/"
        echo "        tpm0/binary_bios_measurements' to see the "
        echo "        raw log)" >&2
        exit 3
    fi
    INPUT="$TMP.bin"
    echo ">> captured $(stat -f %z "$TMP.bin" 2>/dev/null || \
                          stat -c %s "$TMP.bin") bytes"
fi

if [[ -z "$INPUT" ]]; then
    echo "usage: $(basename "$0") [--label TEXT] [--ima PATH] EVENTLOG.bin" >&2
    echo "   or: $(basename "$0") --url http://NODE:PORT --label TEXT" >&2
    echo "try: $(basename "$0") --help" >&2
    exit 1
fi

if [[ ! -f "$INPUT" ]]; then
    echo "error: event log not found at: $INPUT" >&2
    exit 1
fi

SLUG="$(echo "$LABEL" | tr 'A-Z ' 'a-z-' | tr -c 'a-z0-9-' '-' \
                     | sed 's/--*/-/g;s/^-//;s/-$//')"
[[ -z "$SLUG" ]] && SLUG="capture"

OUT="$REPO/out/local-capture/$SLUG"
mkdir -p "$OUT"
cp "$INPUT" "$OUT/input.bin"
if [[ -n "$IMA_LOG" && -f "$IMA_LOG" ]]; then
    cp "$IMA_LOG" "$OUT/ima.log"
fi

BYTES=$(stat -f %z "$OUT/input.bin" 2>/dev/null \
        || stat -c %s "$OUT/input.bin")
echo ">> captured input: $OUT/input.bin ($BYTES bytes)"
echo ">> label: $LABEL"
[[ -n "$IMA_LOG" ]] && echo ">> IMA log: $OUT/ima.log"

# Make sure the test profile is built so the parser beams are on
# the path.
(cd "$REPO" && rebar3 as test compile >/dev/null 2>&1) \
    || { echo "rebar3 compile failed; run manually from $REPO" >&2;
         exit 2; }

# Emit all three views through Erlang.
erl -noshell \
    -pa "$REPO/_build/test/lib/hb/ebin" \
    -pa "$REPO/_build/test/lib/prometheus/ebin" \
    -pa "$REPO/_build/test/lib/b64rs/ebin" \
    -pa "$REPO/_build/test/lib/base32/ebin" \
    -pa "$REPO/_build/test/lib/elmdb/ebin" \
    -eval "
        application:ensure_all_started(prometheus),
        {ok, Bin} = file:read_file(\"$OUT/input.bin\"),
        Ima = case file:read_file(\"$OUT/ima.log\") of
            {ok, I} -> hb_util:encode(I);
            _ -> <<>>
        end,
        Env0 = #{<<\"tcg-event-log\">> => hb_util:encode(Bin)},
        Env = case Ima of
            <<>> -> Env0;
            _ -> Env0#{<<\"ima-log-ascii\">> => Ima}
        end,
        {ok, #{<<\"body\">> := Claim}} =
            dev_tpm_interpret:claim(Env, #{}, #{}),
        {ok, #{<<\"body\">> := Interp}} =
            dev_tpm_interpret:interpret(Env, #{}, #{}),
        Scrub =
            fun Self(M) when is_map(M) ->
                    maps:fold(
                      fun(K, V, Acc) -> Acc#{Self(K) => Self(V)} end,
                      #{}, M);
                Self(L) when is_list(L) -> [Self(X) || X <- L];
                Self(B) when is_binary(B) ->
                    case unicode:characters_to_binary(B,utf8,utf8) of
                        B2 when is_binary(B2) -> B;
                        _ -> hb_util:encode(B)
                    end;
                Self(T) when is_tuple(T) ->
                    [Self(X) || X <- tuple_to_list(T)];
                Self(A) when is_atom(A) -> atom_to_binary(A);
                Self(X) -> X
            end,
        file:write_file(\"$OUT/claim.json\",
            iolist_to_binary(json:encode(Scrub(Claim)))),
        file:write_file(\"$OUT/interpret.json\",
            iolist_to_binary(json:encode(Scrub(Interp)))),
        FmtText = try
            iolist_to_binary(
              hb_format:message(Interp, #{
                linkify_mode => discard,
                debug_print_truncate => 200
              }))
        catch _:E ->
            iolist_to_binary(io_lib:format(
              \"hb_format failed: ~p\", [E]))
        end,
        file:write_file(\"$OUT/interpret.txt\", FmtText),
        HexRows =
            fun HR(<<>>, _, Acc) ->
                     list_to_binary(lists:reverse(Acc));
                HR(B, Off, Acc) ->
                     Sz = min(16, byte_size(B)),
                     <<Row:Sz/binary, Rest/binary>> = B,
                     Hex = lists:flatten(
                       [io_lib:format(\"~2.16.0B \", [X])
                        || <<X:8>> <= Row]),
                     Ascii = [case X of
                                  C when C >= 32, C =< 126 -> C;
                                  _ -> \$.
                              end || <<X:8>> <= Row],
                     Line = io_lib:format(
                       \"~8.16.0B  ~-48s |~s|~n\",
                       [Off, Hex, Ascii]),
                     HR(Rest, Off+Sz,
                        [iolist_to_binary(Line) | Acc])
            end,
        Preview = HexRows(
            binary:part(Bin, 0, min(256, byte_size(Bin))),
            0, []),
        file:write_file(\"$OUT/input-preview.txt\", Preview),
        PV = maps:get(<<\"policy-verdict\">>, Claim),
        AS = maps:get(<<\"attestation-summary\">>, Claim),
        io:format(
            \"~n=== verdict ===~n\"
            \"  verdict  = ~s (score ~p)~n\"
            \"  criticals= ~p  warnings= ~p~n\"
            \"~n=== attestation summary ===~n\"
            \"  machine  = ~s~n\"
            \"  firmware = ~s~n\"
            \"  TPM      = ~s~n\"
            \"  posture  = ~s~n\"
            \"  boot     = ~s~n\"
            \"  context  = ~s~n\"
            \"~n=== written to $OUT/ ===~n\"
            \"  input.bin (~p bytes)~n\"
            \"  claim.json  — flat policy surface~n\"
            \"  interpret.json  — full nested tree~n\"
            \"  interpret.txt  — HyperBuddy format~n\"
            \"  input-preview.txt  — first 256 bytes hex~n\",
            [maps:get(<<\"verdict\">>, PV),
             maps:get(<<\"score\">>, PV),
             length(maps:get(<<\"critical-failures\">>, PV)),
             length(maps:get(<<\"warnings\">>, PV)),
             maps:get(<<\"machine-identity\">>, AS),
             maps:get(<<\"firmware-identity\">>, AS),
             maps:get(<<\"tpm-identity\">>, AS),
             maps:get(<<\"security-posture\">>, AS),
             maps:get(<<\"boot-identity\">>, AS),
             maps:get(<<\"context\">>, AS),
             byte_size(Bin)]),
        halt(0).
    "

# Write a minimal single-machine dashboard.
python3 - "$OUT" "$LABEL" "$BYTES" <<'PY'
import html
import json
import os
import sys
from pathlib import Path

out = Path(sys.argv[1])
label = sys.argv[2]
bytes_count = sys.argv[3]

claim = json.loads((out / "claim.json").read_text())
interp = json.loads((out / "interpret.json").read_text())
hb_format_text = (out / "interpret.txt").read_text(
    encoding="utf-8", errors="replace")
preview = (out / "input-preview.txt").read_text(
    encoding="utf-8", errors="replace")
pv = claim.get("policy-verdict", {})
asm = claim.get("attestation-summary", {})
timeline = claim.get("timeline", {})
pc = claim.get("platform-config", {})
sbp = claim.get("secure-boot-policy", {})
cpu = claim.get("cpu", {})
fw = claim.get("firmware", {})
tpm = claim.get("tpm", {})
tme = claim.get("tme", {})
iommu = claim.get("iommu", {})
lockdown = claim.get("lockdown", {})
sb = claim.get("secure-boot", {})

verdict = pv.get("verdict", "unknown")
verdict_cls = {"trusted": "trusted",
               "attested-with-warnings": "warnings",
               "untrusted": "untrusted"}.get(
                   verdict, "unknown")
score = pv.get("score", 0)

css = """
* { box-sizing: border-box; }
body { margin: 0; padding: 0;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
               sans-serif;
  background: #f4f6f9; color: #0f172a; }
.container { max-width: 1100px; margin: 0 auto; padding: 24px; }
h1 { font-size: 26px; margin: 0 0 6px; letter-spacing: -0.3px; }
h2 { font-size: 18px; margin: 28px 0 12px;
  padding-bottom: 8px; border-bottom: 2px solid #e5e7eb; }
.hero { background: white; border: 1px solid #e5e7eb;
  border-radius: 12px; padding: 24px; margin-bottom: 20px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.05); }
.verdict { display: inline-block; padding: 4px 14px;
  border-radius: 6px; font-size: 14px; font-weight: 600;
  margin-right: 12px; }
.verdict.trusted { background: #d1fae5; color: #065f46; }
.verdict.warnings { background: #fef3c7; color: #92400e; }
.verdict.untrusted { background: #fecaca; color: #991b1b; }
.verdict.unknown { background: #e2e8f0; color: #475569; }
.grid { display: grid;
  grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
  gap: 14px; }
.tile { background: white; border: 1px solid #e5e7eb;
  border-radius: 8px; padding: 14px 18px; }
.tile .label { color: #64748b; font-size: 11px;
  text-transform: uppercase; letter-spacing: 1px;
  margin-bottom: 4px; }
.tile .value { font-size: 16px; font-weight: 500;
  color: #0f172a; word-break: break-word; }
.finding { background: white; border: 1px solid #e5e7eb;
  border-radius: 8px; padding: 12px 16px; margin-bottom: 8px;
  font-size: 14px; }
.finding.critical { border-left: 4px solid #dc2626; }
.finding.warn { border-left: 4px solid #d97706; }
.code { font-family: 'SF Mono', Menlo, Consolas, monospace;
  color: #9333ea; font-weight: 500;
  background: #f1f5f9; padding: 1px 6px; border-radius: 4px;
  font-size: 0.9em; }
.kvs { background: #f8fafc; border: 1px solid #e2e8f0;
  border-radius: 6px; padding: 10px 14px; font-size: 13px;
  line-height: 1.6;
  font-family: 'SF Mono', Menlo, Consolas, monospace;
  white-space: pre-wrap; word-break: break-all; }
pre { background: #0f172a; color: #e2e8f0;
  border-radius: 6px; padding: 12px 16px; overflow: auto;
  font-size: 11.5px; line-height: 1.5;
  max-height: 500px;
  font-family: 'SF Mono', Menlo, Consolas, monospace; }
pre.hex { background: #0b1220; color: #a7b2c4;
  max-height: 260px; }
pre.format { background: #1e293b; }
details { background: white; border: 1px solid #e5e7eb;
  border-radius: 8px; padding: 14px 18px; margin-bottom: 10px; }
details summary { cursor: pointer; font-weight: 500;
  user-select: none; }
details[open] summary { margin-bottom: 10px; }
"""

def findings_html(items, cls):
    out_ = []
    for it in items:
        out_.append(f"""
        <div class="finding {cls}">
          <span class="code">{html.escape(it.get('code','?'))}</span>
          &mdash;
          {html.escape(it.get('message', '?'))}
          <span style="color:#94a3b8;font-size:12px;">
            [{html.escape(it.get('section','?'))}]
          </span>
        </div>
        """)
    return "\n".join(out_) or \
        '<div style="color:#64748b;font-size:13px;">(none)</div>'

signals_map = pv.get("signals", {})
signals_tiles = "".join(f"""
  <div class="tile">
    <div class="label">{html.escape(str(k))}</div>
    <div class="value">{html.escape(str(v))}</div>
  </div>
""" for k, v in sorted(signals_map.items()))

sum_kv = "\n".join(
    f"  {k:24s} : {asm.get(k, '—')}"
    for k in ["machine-identity", "firmware-identity",
              "boot-identity", "tpm-identity",
              "security-posture", "context"])

platform_tiles = "".join(f"""
  <div class="tile">
    <div class="label">{html.escape(str(k))}</div>
    <div class="value">{html.escape(
        str(v) if not isinstance(v, (list, dict))
        else json.dumps(v))}</div>
  </div>
""" for k, v in sorted(pc.items())
    if not isinstance(v, (list, dict)) or len(str(v)) < 300)

interp_json = json.dumps(interp, indent=2, sort_keys=True)
if len(interp_json) > 120000:
    interp_json = interp_json[:120000] + \
        "\n/* ... truncated ... */"

html_out = f"""<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>{html.escape(label)} - TPM Attestation</title>
<style>{css}</style>
</head><body>
<div class="container">
  <h1>{html.escape(label)}</h1>
  <p style="color:#64748b;margin-bottom:20px;">
    TPM2 event-log interpretation from
    <code class="code">{html.escape(str(out / 'input.bin'))}</code>
    ({int(bytes_count):,} bytes).
  </p>
  <div class="hero">
    <div style="display:flex;align-items:center;gap:12px;margin-bottom:14px;">
      <span class="verdict {verdict_cls}">{html.escape(verdict)}</span>
      <span style="font-size:20px;font-weight:600;">
        score {score}
      </span>
      <span style="color:#64748b;font-size:14px;">
        &middot; {len(pv.get('critical-failures', []))} critical,
        {len(pv.get('warnings', []))} warning(s)
      </span>
    </div>
    <div class="kvs">{html.escape(sum_kv)}</div>
  </div>

  <h2>Findings ({len(pv.get('critical-failures', [])) +
                 len(pv.get('warnings', []))})</h2>
  {findings_html(pv.get('critical-failures', []), 'critical')}
  {findings_html(pv.get('warnings', []), 'warn')}

  <h2>Timeline</h2>
  <div class="grid">
    <div class="tile">
      <div class="label">TPM epoch</div>
      <div class="value">{html.escape(str(timeline.get('tpm-epoch','—')))}</div>
    </div>
    <div class="tile">
      <div class="label">reset / restart count</div>
      <div class="value">{timeline.get('reset-count','—')}
        / {timeline.get('restart-count','—')}</div>
    </div>
    <div class="tile">
      <div class="label">TPM clock</div>
      <div class="value">{timeline.get('clock-seconds','—')} s
        ({timeline.get('clock-ms','—')} ms)</div>
    </div>
    <div class="tile">
      <div class="label">event log</div>
      <div class="value">{timeline.get('event-log-count','—')}
        events / seq {timeline.get('event-log-seq-min','—')}
        .. {timeline.get('event-log-seq-max','—')}</div>
    </div>
  </div>

  <h2>Platform configuration</h2>
  <div class="grid">{platform_tiles}</div>

  <h2>Secure-Boot policy</h2>
  <div class="grid">
    <div class="tile"><div class="label">enabled</div>
      <div class="value">{sbp.get('enabled','—')}</div></div>
    <div class="tile"><div class="label">posture</div>
      <div class="value">{html.escape(str(sbp.get('policy-posture','—')))}</div></div>
    <div class="tile"><div class="label">strength</div>
      <div class="value">{html.escape(str(sbp.get('policy-strength','—')))}</div></div>
    <div class="tile"><div class="label">PK / KEK / db / dbx</div>
      <div class="value">{sbp.get('pk-entry-count',0)}
        / {sbp.get('kek-entry-count',0)}
        / {sbp.get('db-entry-count',0)}
        / {sbp.get('dbx-entry-count',0)}</div></div>
  </div>
  <details><summary>Trusted signers ({len(sbp.get('trusted-signers', []))})</summary>
    <div class="kvs">{html.escape(json.dumps(
       sbp.get('trusted-signers', []), indent=2))}</div></details>

  <h2>CPU / TPM / Firmware</h2>
  <div class="grid">
    <div class="tile"><div class="label">CPU codename</div>
      <div class="value">{html.escape(str(cpu.get('codename','—')))}</div></div>
    <div class="tile"><div class="label">CPU family / model / stepping</div>
      <div class="value">{cpu.get('cpu-family','—')}
        / {cpu.get('cpu-model','—')}
        / {cpu.get('cpu-stepping','—')}</div></div>
    <div class="tile"><div class="label">CPU TEE support</div>
      <div class="value">{html.escape(', '.join(cpu.get('tee-support', [])))}</div></div>
    <div class="tile"><div class="label">microcode</div>
      <div class="value" style="font-size:12px;">{html.escape(str(cpu.get('microcode-description','—')))}</div></div>
    <div class="tile"><div class="label">TPM vendor</div>
      <div class="value">{html.escape(str(tpm.get('manufacturer-name','—')))}</div></div>
    <div class="tile"><div class="label">TPM trust-tier</div>
      <div class="value">{html.escape(str(tpm.get('trust-tier','—')))}</div></div>
    <div class="tile"><div class="label">firmware CRTM</div>
      <div class="value">{html.escape(str(fw.get('crtm-version','—')))}</div></div>
    <div class="tile"><div class="label">firmware family</div>
      <div class="value">{html.escape(str(fw.get('family-vendor','—')))}
        {html.escape(str(fw.get('family-platform','')))}</div></div>
  </div>

  <h2>Security posture</h2>
  <div class="grid">
    <div class="tile"><div class="label">Secure Boot</div>
      <div class="value">{sb.get('enabled','—')}</div></div>
    <div class="tile"><div class="label">TME / SME</div>
      <div class="value">{tme.get('enabled','—')}</div></div>
    <div class="tile"><div class="label">IOMMU</div>
      <div class="value">{iommu.get('enabled','—')} ({html.escape(str(iommu.get('mode','—')))})</div></div>
    <div class="tile"><div class="label">Lockdown level</div>
      <div class="value">{html.escape(str(lockdown.get('level','—')))}</div></div>
  </div>

  <h2>Signals (flat policy-match surface)</h2>
  <div class="grid">{signals_tiles}</div>

  <h2>Input preview (first 256 bytes)</h2>
  <pre class="hex">{html.escape(preview)}</pre>

  <h2>HyperBuddy formatted output</h2>
  <pre class="format">{html.escape(hb_format_text[:50000])}</pre>

  <h2>Full interpret/3 tree</h2>
  <pre>{html.escape(interp_json)}</pre>

  <footer style="margin-top:36px;color:#94a3b8;font-size:12px;">
    Generated by
    <code>lapee-baremetal/scripts/interpret-local-capture.sh</code>.
  </footer>
</div></body></html>"""

dash = out / "dashboard.html"
dash.write_text(html_out)
print(f">> wrote dashboard: {dash}")
PY

DASH="$OUT/dashboard.html"
echo ""
echo ">> opening $DASH"
open -a "Google Chrome" "$DASH" 2>/dev/null || open "$DASH"
