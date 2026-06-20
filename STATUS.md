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

- Added selected-module local flow lenses.
- Direct graph clicks now focus the selected neighborhood.
- De-duplicated modules/functions that are both callers and callees in lenses.
- Tightened the responsive graph stage height for the in-app browser viewport.
- Validated `hb_message` module neighborhood in the browser.

## Next Work

- Improve selected-neighborhood controls for systems.
- Add richer dense-edge reduction and visual weighting.
- Continue browser validation after each presentation pass.
