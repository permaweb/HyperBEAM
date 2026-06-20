# HyperBEAM Code Visualizer Status

Branch: `expr/visualizer`

## Current State

- Static visualizer generator produces `build/codeviz/hyperbeam-codeviz.html`.
- Current graph build: `167 modules`, `2477 functions`, `3401 calls`.
- Default view is now `Subsystems`, with module and function modes as drilldowns.
- Browser target in use: `http://127.0.0.1:8765/hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=system`.

## Evidence

- `node --check scripts/codeviz/visualizer.js` passes.
- `escript scripts/codeviz/generate.escript --out=build/codeviz/hyperbeam-codeviz.html --json-out=build/codeviz/graph.json` passes.
- `git diff --check` passes.
- Browser screenshots saved under `build/codeviz/validation-*.png`.

## Last Presentation Pass

- Added module inspector function lists for direct module-to-function drilldown.
- Search-scoped selections now expand their caller/callee neighborhood.
- Selected functions now render as a local `Callers -> Function -> Callees` lens.
- Function lens nodes include module context on a second line.
- Validated `hb_message -> commit/2` drilldown in the browser.

## Next Work

- Improve selected-neighborhood controls for modules and systems.
- Add richer dense-edge reduction and visual weighting.
- Continue browser validation after each presentation pass.
