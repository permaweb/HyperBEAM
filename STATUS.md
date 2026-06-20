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

- Added shareable URL state for mode, devices, selection, search, group, edge mode, and toggles.
- Added `selected=` load support so shared URLs reopen directly into a lens.
- Validated a `kernel:resolver` selected subsystem URL in the browser.

## Next Work

- Add richer export/source affordances in the inspector.
- Add richer dense-graph controls.
- Continue browser validation after each presentation pass.
