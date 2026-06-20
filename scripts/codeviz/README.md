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
`src/forge`, excluding test directories. Generated nodes carry source taxonomy
fields (`source-root`, `source-category`, `namespace`, `source-dirs`, and
`component-kind`) so the UI can distinguish kernel modules, device roots,
device support modules, and forge tooling. The visualizer starts with a
subsystem map of the kernel and lets root devices from `src/preloaded` be added
to the context. Selecting a device also includes its source-package siblings and
support modules, while support files such as `lib_process` remain grouped with
their package instead of becoming fake picker devices.

Focused views can be opened with query parameters when served over HTTP:

```text
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=system
hyperbeam-codeviz.html?devices=all&mode=system
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=module&selected=hb_message
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=system&selected=kernel:resolver&edges=strong
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=module&live=demo&follow=heat
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=module&live=demo&follow=heat&interval=1
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=function&layout=flow
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=module&layout=namespace
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=function&scope=kernel
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=function&scope=touchpoints
```

Supported modes are `system`, `module`, and `function`. Module and function
views default to a force-balanced call map, with soft module/role regions,
curved bidirectional edge ports, collision-relaxed labels, a canvas-backed
background traffic layer for dense call sets, and in-place caller/callee
highlighting when a node is selected.
Use `layout=namespace` for a source-ownership objective: outer role bands remain
visible while inner namespace hulls group modules or functions by
`kernel/...`, `devices/...`, and `forge/...` paths. Use `layout=flow` to restore
the staged column/lens layouts for narrow call-chain inspection. `scope=auto`
keeps broad function/device views readable by showing the selected device
package and direct touchpoints, while module views keep the kernel context.
Use `scope=kernel` to force the full kernel/device function context, or
`scope=touchpoints` to intentionally stay compact. `edges=strong` filters the
graph to repeated call
relationships. The context and inspector panes can be resized with the
recorder-style splitters around the graph.
Caller/callee rows preserve the selected call edge when clicked, so side-list
navigation keeps the graph edge and source/target inspector context visible.
Clicking an already-selected graph node drills down one level: subsystems open
their module graph, and modules open a function graph filtered to that module.
Call edges are drawn through the raster traffic layer and still support nearest
edge hit-testing; clicking one selects its callee, lights the surrounding
relationship, and records the selected call in the inspector with source/target
jump actions. Selected call/trace edge context is encoded in the URL as
`edge=...`, so copied links preserve the exact edge card.
Selected devices are pinned to the top of the device list. Search matches are
highlighted in the graph and minimap so surrounding context is easier to
distinguish from the actual result set. Hovering a node temporarily highlights
its callers and callees without changing the selected inspector target. Kernel
selections also show short directed device paths when the rendered graph
contains a route from a loaded device module into that node; clicking a path row
paints the route on the graph and minimap. Large unsearched function views open
as an overview, while searched function graphs stay at a readable first-fit
scale. Function nodes show their local `function/arity` label with the owning
module and namespace attached as visible subtitle text in map layouts, while
the full `module:function/arity` ID remains available in search, URLs, tooltips,
and the inspector.

## Live and Recorder Overlays

The standalone report can also paint runtime activity onto the static graph:

```text
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=module&live=demo
hyperbeam-codeviz.html?devices=recorder@1.0&mode=module&live=/~hyperbuddy@1.0/events
hyperbeam-codeviz.html?devices=recorder@1.0&mode=module&live=stack
hyperbeam-codeviz.html?devices=recorder@1.0&mode=module&recording=demo
hyperbeam-codeviz.html?devices=recorder@1.0&mode=module&recording=demo&recording-event=1
hyperbeam-codeviz.html?devices=recorder@1.0&mode=module&recording=http%3A%2F%2F127.0.0.1%3A8891%2Freport.json
```

`live=/~hyperbuddy@1.0/events` polls HyperBEAM event counters, computes deltas,
and highlights hot modules/functions. If the JSON response is linkified, the
visualizer fetches the formatted event message and extracts the numeric counters.
The static graph also indexes `?event(...)` and `hb_event:record(...)` topics so
counter names such as `scheduling/assigned` can resolve to the modules/functions
that emit them. Event rows decay over several poll ticks, leaving recent pulses
visible long enough to inspect. The event deck and selected-node live event rows
also render a small pulse history so repeated event streams read as a rhythm
instead of a single momentary delta.
`live=stack` polls
`/~recorder@1.0/live?limit=90&stack-limit=18`, paints sampled BEAM stack traces,
and draws animated trace routes between visible graph nodes. Trace route rows
carry the representative stack breadcrumb, and SVG trace titles include the
same frames. Clicking a trace route selects the trace edge and opens the same
source/target inspector card as clicking the graph edge. `follow=heat` keeps
the inspector and graph centered on the
hottest live node. Live stack rows show compact stack trails and can be clicked
to pull their module/function into the current graph view. Stack mode also shows
a process deck with the busiest sampled processes, including
pid/name/status/reductions/memory/queue metadata, and those rows click back into
the graph. `interval=<seconds>` controls the polling cadence for
live counters, stack sampling, and demo telemetry; the engine header exposes
the selected cadence and freshness of the last sample.

Use the Import button to load a saved `~recorder@1.0` HTML recording or JSON
report, or pass a URL in `recording=<url>`. Imported recordings can be viewed
as aggregate heat, replayed event-by-event with the timeline Play control, or
focused to one event with `recording-event=N`. Timeline Play/Prev/Next controls
step through the recorder events, ticks that carry warning/failure activity are
marked as error ticks, and each tick carries a small stack-depth heat bar.
Focused recording events render their stack frames
as clickable rows in the engine deck; frame rows select the adjacent trace edge
when the current graph can project it. The source switcher in the graph panel can jump
between HyperBuddy counters, live stack snapshots, demo telemetry, recorder
playback, and report import. The source header shows metric chips for event
rates, stack frames, trace counts, hot nodes, and errors.

Runtime overlays also mark hot nodes in the minimap, show numeric heat badges on
graph nodes, and add a dedicated error heat deck when warning/failure activity
is present. When a recording is loaded, the inspector lists recorded events that
touched the selected node; clicking one focuses the timeline and repaints the
graph to that event.
