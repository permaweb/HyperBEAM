#!/usr/bin/env python3
"""
span_time_report.py — aggregate wall-clock timings from Rust `tracing` logs.

• **Per-type summary** is always printed (span name before any `{…}` payload).
• Use `-d / --details` to also show the full per-span table.
• `-o` and `-t` dump the same tables to CSV.

```bash
python span_time_report.py build.log            # summary only
python span_time_report.py build.log -d         # + individual spans
python span_time_report.py build.log -o spans.csv -t types.csv
```

The script accepts logs where the level token can be `TRACE`, `DEBUG`, `INFO`,
etc.; only lines ending in **`enter`** or **`exit`** are timed.
"""

import argparse
import collections
import csv
import datetime as dt
import re
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Optional

# ──────────────────────────────────────────────────────────────────────────────
# Regexes

ANSI_RE = re.compile(r"\x1b\[[0-9;]*[mK]")  # strip colour codes
SPAN_RE = re.compile(
    r"(?P<ts>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+)Z\s+"  # timestamp
    r"(?P<level>[A-Z]+)\s+"                                           # TRACE / DEBUG …
    r"(?P<span>[^:]+):\s+"                                            # span name (may include {…})
    r"(?P<path>[^:]+):\d+:\s+"                                       # source path + line
    r"(?P<action>enter|exit)"                                          # action keyword
)

# ──────────────────────────────────────────────────────────────────────────────
# Helpers

def iso_to_dt(ts: str) -> dt.datetime:
    """Convert ISO-8601 *UTC* timestamp → datetime."""
    return dt.datetime.strptime(ts, "%Y-%m-%dT%H:%M:%S.%f").replace(tzinfo=dt.timezone.utc)


def base_span_name(raw: str) -> str:
    """Return the span name before any payload braces."""
    return raw.split("{", 1)[0].strip()


# ──────────────────────────────────────────────────────────────────────────────
# Pretty printing

def print_table(title: str, rows: List[dict], headers: Tuple[str, ...]):
    if not rows:
        print(f"\n{title}\n(no matching spans found)")
        return
    print(f"\n{title}")
    col_widths = [max(len(h), max((len(str(r[h])) for r in rows), default=0)) for h in headers]
    fmt = "  ".join(f"{{:{w}}}" for w in col_widths)
    divider = "  ".join("-" * w for w in col_widths)
    print(fmt.format(*headers))
    print(divider)
    for r in rows:
        print(fmt.format(*(r[h] for h in headers)))


# ──────────────────────────────────────────────────────────────────────────────
# Core processing

def process_log(path: Path):
    open_stack: Dict[Tuple[str, str], List[dt.datetime]] = collections.defaultdict(list)
    totals: Dict[Tuple[str, str], dt.timedelta] = collections.defaultdict(lambda: dt.timedelta())
    counts: Dict[Tuple[str, str], int] = collections.Counter()
    first_enter_ts: Optional[dt.datetime] = None
    last_exit_ts: Optional[dt.datetime] = None

    with path.open("r", encoding="utf-8", errors="replace") as fh:
        for raw in fh:
            line = ANSI_RE.sub("", raw)
            m = SPAN_RE.search(line)
            if not m:
                continue  # unrelated log line
            ts_str, span_raw, src_path, action = m.group("ts", "span", "path", "action")
            try:
                ts = iso_to_dt(ts_str)
            except ValueError:
                continue  # malformed timestamp

            key = (span_raw, src_path)
            if action == "enter":
                if first_enter_ts is None:
                    first_enter_ts = ts
                open_stack[key].append(ts)
            else:  # exit
                if open_stack[key]:
                    start = open_stack[key].pop()
                    totals[key] += ts - start
                    counts[key] += 1
                last_exit_ts = ts

    # Calculate overall duration from first enter to last exit
    overall_duration = dt.timedelta()
    if first_enter_ts and last_exit_ts and last_exit_ts > first_enter_ts:
        overall_duration = last_exit_ts - first_enter_ts

    # Build per-span rows
    span_rows: List[dict] = []
    for (span, path), td in totals.items():
        secs = td.total_seconds()
        span_rows.append({
            "span": span,
            "source_path": path,
            "calls": counts[(span, path)],
            "total_seconds": f"{secs:.6f}",
            "avg_seconds": f"{secs / counts[(span, path)]:.6f}",
        })
    span_rows.sort(key=lambda r: float(r["total_seconds"]), reverse=True)

    # Build per-type rows (collapse payload)
    type_totals: Dict[str, dt.timedelta] = collections.defaultdict(lambda: dt.timedelta())
    type_counts: Dict[str, int] = collections.Counter()
    for (span_raw, _), td in totals.items():
        t = base_span_name(span_raw)
        type_totals[t] += td
        type_counts[t] += counts[(span_raw, _)]

    type_rows: List[dict] = []
    for t, td in type_totals.items():
        secs = td.total_seconds()
        type_rows.append({
            "span": t,
            "calls": type_counts[t],
            "total_seconds": f"{secs:.6f}",
            "avg_seconds": f"{secs / type_counts[t]:.6f}",
        })
    type_rows.sort(key=lambda r: float(r["total_seconds"]), reverse=True)

    grand_total = sum(type_totals.values(), dt.timedelta())
    return span_rows, type_rows, grand_total, overall_duration


def write_csv(rows: List[dict], path: Path, headers: Tuple[str, ...]):
    with path.open("w", newline="") as fp:
        writer = csv.DictWriter(fp, fieldnames=headers)
        writer.writeheader()
        writer.writerows(rows)
    print(f"[written] {path}")


# ──────────────────────────────────────────────────────────────────────────────
# CLI

def main():
    ap = argparse.ArgumentParser(description="Sum tracing span times by type")
    ap.add_argument("logfile", type=Path, help="Log file to analyse")
    ap.add_argument("-d", "--details", action="store_true", help="Print individual span table too")
    ap.add_argument("-o", "--span-csv", type=Path, help="Write per-span CSV to this path")
    ap.add_argument("-t", "--type-csv", type=Path, help="Write per-type CSV to this path")
    args = ap.parse_args()

    if not args.logfile.exists():
        sys.exit(f"error: file not found — {args.logfile}")

    span_rows, type_rows, grand_total, overall_duration = process_log(args.logfile)

    # Print summary first (always)
    print_table("Totals per span type", type_rows, ("span", "calls", "total_seconds", "avg_seconds"))
    print("─" * 45)
    # Calculate total_excluding_create ensuring type_rows is not empty
    print(f"grand total: {grand_total.total_seconds():.6f} s ({(grand_total / overall_duration) if overall_duration.total_seconds() > 0 else 0:.2%})")
    total_excluding_create = dt.timedelta()
    if type_rows:
        # Convert the string representation of seconds to a float, then to a timedelta
        seconds_to_subtract = float(type_rows[0]["total_seconds"])
        total_excluding_create = grand_total - dt.timedelta(seconds=seconds_to_subtract)
        print(f"excl. create: {total_excluding_create.total_seconds():.6f} s ({(total_excluding_create / overall_duration) if overall_duration.total_seconds() > 0 else 0:.2%})")

    # print(f"overall duration: {overall_duration.total_seconds():.6f} s")

    if args.details:
        print_table("Individual spans", span_rows, ("span", "source_path", "calls", "total_seconds", "avg_seconds"))

    if args.span_csv:
        write_csv(span_rows, args.span_csv, ("span", "source_path", "calls", "total_seconds", "avg_seconds"))
    if args.type_csv:
        write_csv(type_rows, args.type_csv, ("span", "calls", "total_seconds", "avg_seconds"))


if __name__ == "__main__":
    main()
