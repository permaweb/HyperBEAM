# HyperBEAM Code Visualizer Status

Branch: `expr/visualizer`

## Current State

- Static visualizer generator produces `build/codeviz/hyperbeam-codeviz.html`.
- Current graph build: `167 modules`, `2477 functions`, `3401 calls`.
- Default view is now `Subsystems`, with module and function modes as drilldowns.
- Browser target in use: `http://127.0.0.1:8765/hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=system`.
- Live engine overlay supports `live=demo` and `live=<endpoint>`; endpoints are interpreted as event counter streams and painted onto the graph as hot nodes, rings, and flowing edges.

## Evidence

- `node --check scripts/codeviz/visualizer.js` passes.
- `escript scripts/codeviz/generate.escript --out=build/codeviz/hyperbeam-codeviz.html --json-out=build/codeviz/graph.json` passes.
- `git diff --check` passes.
- `HB_PORT=0 rebar3 eunit` passes: `All 947 tests passed`, `EXIT_CODE=0`.
- Browser screenshots saved under `build/codeviz/validation-*.png`.
- Live overlay browser proof: `?devices=recorder@1.0,scheduler@1.0&mode=system&live=demo` showed `3` hot nodes, `3` hot edges, `6` live rings, and status `demo live: +27 events · 40 hot`; screenshot saved to `build/codeviz/validation-live-demo.png`.

## Last Presentation Pass

- Added live engine overlay controls.
- Added event counter and Prometheus text normalizers.
- Added URL-addressable live mode with explicit demo feed and real endpoint mode.

## Next Work

- Inspect and extend recorder stacktrace/live-process surfaces.
- Paint recorder stacktrace data onto selected modules/functions.
- Continue browser validation after each presentation pass.
