# HyperBEAM Code Visualizer Status

Branch: `expr/visualizer`

## Current State

- Static visualizer generator produces `build/codeviz/hyperbeam-codeviz.html`.
- Current graph build: `167 modules`, `2489 functions`, `3424 calls`.
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
- Recorder stack proof: `rebar3 device test --devices dev_recorder --module recorder@1.0 --test live_report_processes_test+live_json_response_test` passes (`2 tests passed`).
- Browser stack overlay proof against a recorder-shaped local JSON feed: `selected=hb_message` showed status `stacks: 2 procs · +2 reductions · 47 hot`, one `Live stacks` inspector row, `8` live rings, and `8` warm edges; screenshot saved to `build/codeviz/validation-stack-live.png`.
- Recorder import proof: `recording=demo&selected=hb_message` showed status `demo: 3 events · 9 frames · 54 hot`, one recorded inspector stack row, `10` live rings, and `3` warm edges; screenshot saved to `build/codeviz/validation-recording-demo.png`.
- Heat panel proof: `recording=demo` showed a `Recorded heat` panel with `8` rows; clicking the top row selected `hb_http` and focused its module lens. Screenshot saved to `build/codeviz/validation-heat-panel.png`.
- Trace edge proof: `recording=demo` in module mode showed status `demo: 3 events · 9 frames · 16 traces · 54 hot`, with `4` visible animated trace paths including `hb_ao -> hb_message` and `dev_recorder -> hb_ao`; screenshot saved to `build/codeviz/validation-trace-edges.png`.
- Engine panel proof: `recording=demo` shows a docked `72px` engine deck with `4` hot rows and `4` trace-route rows; clicking the first route selected `hb_message` and focused its module lens. Screenshot saved to `build/codeviz/validation-engine-panel.png`.
- Event-delta proof: a local `~hyperbuddy@1.0/events`-shaped JSON counter feed showed rows including `hb_http/request+7 events`, status `live: +16 events · 41 hot`, and clicking the top event row selected `hb_http`; screenshot saved to `build/codeviz/validation-event-deltas.png`.
- Minimap proof: module mode rendered `72` minimap nodes and a viewport rectangle; clicking the minimap moved the main transform from `translate(24,24) scale(0.72)` to `translate(-791.8999999999999,-523.3805696661829) scale(0.72)`. Screenshot saved to `build/codeviz/validation-minimap.png`.

## Last Presentation Pass

- Added live engine overlay controls.
- Added event counter and Prometheus text normalizers.
- Added URL-addressable live mode with explicit demo feed and real endpoint mode.
- Added `~recorder@1.0/live` JSON endpoint for bounded BEAM process stack snapshots.
- Added visualizer stack mode that paints live process stacks onto modules/functions and surfaces matching process rows in the inspector.
- Added recorder report import support for saved `~recorder@1.0` HTML/JSON reports with embedded stack traces.
- Added graph heat panel for the hottest live, stack, or recorded nodes with click-to-focus behavior.
- Added animated trace edges that project live/recorded stack frames into the current subsystem, module, or function view.
- Moved heat and trace route telemetry into a compact docked engine panel above the graph so live data no longer blocks the map.
- Added event-delta rows for live counter feeds so `~hyperbuddy@1.0/events` activity can be inspected and clicked even when no stack traces are present.
- Added a clickable minimap with a live viewport rectangle for faster navigation around large module/function layouts.

## Next Work

- Improve stack heat ranking and selected-node timeline affordances.
- Add stronger UI affordances for switching between event heat, stack heat, and recording heat.
- Continue browser validation after each presentation pass.
