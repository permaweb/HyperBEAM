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
context. Selecting a device also includes same-group modules that reference it,
so helper/server pieces stay visible with the root device without pulling in
unrelated device groups.

Focused views can be opened with query parameters when served over HTTP:

```text
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=system
hyperbeam-codeviz.html?devices=all&mode=system
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=module&selected=hb_message
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=system&selected=kernel:resolver&edges=strong
hyperbeam-codeviz.html?devices=recorder@1.0,scheduler@1.0&mode=module&live=demo&follow=heat
```

Supported modes are `system`, `module`, and `function`. Selecting a subsystem,
module, or function opens a local callers/callees lens, and `edges=strong`
filters the graph to repeated call relationships. The context and inspector
panes can be resized with the recorder-style splitters around the graph.
Selected devices are pinned to the top of the device list.

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
visible long enough to inspect.
`live=stack` polls
`/~recorder@1.0/live?limit=90&stack-limit=18`, paints sampled BEAM stack traces,
and draws animated trace routes between visible graph nodes. `follow=heat`
keeps the inspector and graph centered on the hottest live node. Live stack rows
show compact stack trails and can be clicked to pull their module/function into
the current graph view.

Use the Import button to load a saved `~recorder@1.0` HTML recording or JSON
report, or pass a URL in `recording=<url>`. Imported recordings can be viewed
as aggregate heat, replayed event-by-event with the timeline Play control, or
focused to one event with `recording-event=N`.
