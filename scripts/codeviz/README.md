# HyperBEAM Code Visualizer

Generate a standalone, recorder-styled call graph report from the current
checkout:

```sh
escript scripts/codeviz/generate.escript
```

By default the report is written to:

```text
build/codeviz/hyperbeam-codeviz.html
```

To also write the generated graph data as JSON:

```sh
escript scripts/codeviz/generate.escript --json-out=build/codeviz/graph.json
```

The generator parses Erlang source under `src/core`, `src/preloaded`, and
`src/forge`, excluding test directories. The visualizer starts with a subsystem
map of the kernel and lets device modules from `src/preloaded` be added to the
context.

Focused views can be opened with query parameters when served over HTTP:

```text
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=system
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=module&selected=hb_message
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=system&selected=kernel:resolver&edges=strong
```

Supported modes are `system`, `module`, and `function`. Selecting a subsystem,
module, or function opens a local callers/callees lens, and `edges=strong`
filters the graph to repeated call relationships.
