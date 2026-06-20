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

- Promoted readable subsystem map as the default presentation layer.
- Added graph-first responsive layout for the in-app browser width.
- Added readable auto-fit for dense modes while keeping the Fit button available.
- Routed same-column subsystem edges through a gutter and subdued internal edges.
- Validated subsystem selection and module drilldown back to the graph.

## Next Work

- Improve module/function drilldown aesthetics and selected-neighborhood clarity.
- Add richer graph affordances for callers/callees and dense edge reduction.
- Continue browser validation after each presentation pass.
