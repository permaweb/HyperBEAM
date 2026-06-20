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

- Added call-count weighted edge strokes.
- Tightened mobile stats and filter layout so the graph starts much earlier.
- Fixed mobile Context stat truncation.
- Validated desktop/default subsystem, module lens, and 390px mobile layouts in the browser.

## Next Work

- Improve selected-neighborhood controls for systems and dense graphs.
- Add richer export/source affordances in the inspector.
- Continue browser validation after each presentation pass.
