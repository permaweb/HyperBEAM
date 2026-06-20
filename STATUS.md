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

- Added `Repeated calls` edge mode for dense graph reduction.
- Repeated-call mode persists through `edges=strong`.
- Validated subsystem edge count dropping from `39` to `32` in the browser.

## Next Work

- Add richer export/source affordances in the inspector.
- Continue dense-graph polish.
- Continue browser validation after each presentation pass.
