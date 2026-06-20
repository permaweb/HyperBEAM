# HyperBEAM Code Visualizer Status

Branch: `expr/visualizer`

## Current State

- Static visualizer generator produces `build/codeviz/hyperbeam-codeviz.html`.
- Current graph build: `167 modules`, `3359 functions`, `8508 calls`.
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
- Linkified-event proof: a local HyperBuddy-shaped `/events` mock returned only `+link` keys, the visualizer fetched the formatted fallback, and the engine deck showed `http/request+5 events`, `http/parsed_singleton+3 events`, and `hb_message/commit+2 events`; screenshot saved to `build/codeviz/validation-linkified-events.png`.
- Real recorder proof: a throwaway node on `localhost:19876` returned `200 application/json` from `~recorder@1.0/live`; the browser connected to that absolute endpoint and showed `stacks: 90 procs · +4,266 reductions · 1 traces · 8 hot` with a visible `hb_http_server -> hb_ao` route. Screenshot saved to `build/codeviz/validation-real-recorder-live.png`.
- Fresh full-suite proof after the linkified-event pass: `HB_PORT=0 rebar3 eunit` completed with `All 947 tests passed`.
- Fresh full-suite proof after the event-alias, device-context, inspector, and recorder-action passes: `HB_PORT=0 rebar3 eunit` completed with `All 947 tests passed`.
- Fresh full-suite proof after the follow-heat, playback, splitter, stack-path, and inspector-event passes: `HB_PORT=0 rebar3 eunit` completed with `All 947 tests passed`.
- Event-alias proof: after adding the missing `src/core` include path to the generator, the graph expanded from `2489 functions / 3424 calls` to `3359 functions / 8508 calls`; `70` modules now carry event aliases harvested from `?event(...)` and `hb_event:record(...)`.
- Event-alias browser proof: a local counter feed with `scheduling/assigned`, `store_error/store_call_failed_retrying`, and `payment/charge` rows highlighted `dev_scheduler_server`, `hb_store`, `dev_simple_pay`, and `dev_p4`; clicking `scheduling/assigned` selected `dev_scheduler_server` and saved URL state. Screenshot saved to `build/codeviz/validation-event-aliases.png`.
- Device-family proof: `devices=scheduler@1.0&mode=module` now includes same-group modules that reference the selected device (`dev_scheduler`, `dev_scheduler_registry`, `dev_scheduler_server`, and `lib_process`) while excluding unrelated router/payment/vm referrers. Screenshot saved to `build/codeviz/validation-device-family-context.png`.
- Event-meter proof: a local live counter feed rendered three event pulse meters at `100%`, `42.857%`, and `14.286%` for `scheduling/assigned`, `store_error/store_call_failed_retrying`, and `payment/charge`; screenshot saved to `build/codeviz/validation-event-meters.png`.
- Inspector live-event proof: selecting the hot `hb_store` module from a live feed showed a `Live events` inspector section with `store_error/store_call_failed_retrying` at `100%`; screenshot saved to `build/codeviz/validation-inspector-live-events.png`.
- Recorder action proof: selecting `dev_recorder` shows `Recorder black box` actions (`Live stacks`, `Import`, `Demo recording`); clicking `Demo recording` painted the recorder timeline and trace rows from the inspector. Screenshot saved to `build/codeviz/validation-recorder-actions.png`.
- Event-search proof: `search=scheduling/assigned` with all devices loaded returned exactly one module, `dev_scheduler_server`, by searching static event aliases. Screenshot saved to `build/codeviz/validation-event-search.png`.
- Event-alias pill proof: selecting `dev_scheduler_server` shows event alias pills including `scheduling/assigned`; clicking that pill sets graph search to `scheduling/assigned` and preserves the selected scheduler server. Screenshot saved to `build/codeviz/validation-event-alias-pills.png`.
- Recording URL proof: `recording=http://127.0.0.1:8891/report.json` loaded a recorder-shaped JSON report, painted `2 events · 6 frames · 4 traces`, retained the recording URL state, and heated the actual structured frames (`hb_message`, `dev_recorder`, `hb_store`, `hb_ao`). Screenshot saved to `build/codeviz/validation-recording-url.png`.
- Event-rate proof: a local live feed rendered event rows with approximate rates (`3.6/s`, `1.8/s`, `0.9/s`) beside their recent deltas. Screenshot saved to `build/codeviz/validation-event-rates.png`.
- Aggregate-rate proof: `live=demo` status now shows an aggregate rate (`demo live: +27 events · 20/s · 8 hot`) in both the live badge and graph metadata.
- Minimap proof: module mode rendered `72` minimap nodes and a viewport rectangle; clicking the minimap moved the main transform from `translate(24,24) scale(0.72)` to `translate(-791.8999999999999,-523.3805696661829) scale(0.72)`. Screenshot saved to `build/codeviz/validation-minimap.png`.
- Device bridge proof: with `recorder@1.0,scheduler@1.0` loaded and live off, the engine deck showed top bridges including `dev_scheduler -> hb_message` and kernel touchpoints including `hb_util`; clicking the first bridge selected `hb_message`. Screenshot saved to `build/codeviz/validation-device-bridges.png`.
- Recording timeline proof: `recording=demo` rendered `All` plus `3` event ticks; focusing event `1` repainted the graph to `1 events · 3 frames · 4 traces` with visible `dev_recorder -> hb_ao` and `hb_ao -> hb_message` routes, and clicking `All` restored `3 events · 9 frames · 16 traces`. Screenshot saved to `build/codeviz/validation-recording-timeline.png`.
- Focused recording URL proof: `recording=demo&recording-event=1` opened directly with tick `1` active, status `demo: 1 events · 3 frames · 4 traces · 44 hot`, and retained `recording-event=1` in the URL.
- Mobile QA proof: at `390x844`, the page had `0` horizontal overflow, no offscreen controls, and the `2 devices` context stat fit its container. Screenshot saved to `build/codeviz/validation-mobile.png`.
- Refreshed mobile QA proof: at `390x844`, `dev_recorder` with `recording=demo` had `0` horizontal overflow, no offscreen controls, and the three recorder action buttons fit at about `99px` each. Screenshot saved to `build/codeviz/validation-mobile-recorder-actions.png`.
- Stack-row navigation proof: `recording=demo&selected=dev_recorder` rendered an enabled `hb_message:commit/3` stack row; clicking it selected and revealed `hb_message` even though the target was outside the pre-click recorder lens. Screenshot saved to `build/codeviz/validation-stack-row-pulls-target.png`.
- Follow-heat proof: `live=demo&follow=heat` auto-selected hot module `dev_scheduler`, showed the inspector, marked the `Follow` control active, and preserved `follow=heat` in the URL. Screenshot saved to `build/codeviz/validation-follow-heat.png`.
- Follow-heat mobile proof: at `390x844`, `live=demo&follow=heat` had `0` horizontal overflow, no offscreen controls, selected `dev_scheduler`, and fit six live-strip controls in two rows. Screenshot saved to `build/codeviz/validation-follow-heat-mobile.png`.
- Inspector heat wording proof: `live=demo&follow=heat` showed numeric `Live heat` in the inspector and no longer rendered the ambiguous `Errors hot` wording. Screenshot saved to `build/codeviz/validation-inspector-heat-wording.png`.
- Recording playback proof: `recording=demo` timeline now has a `Play` control; clicking it moved from aggregate `All` into a numbered event with `Pause` active and then returned to `All` with `Play` restored after replay. Screenshot saved to `build/codeviz/validation-recording-playback.png`.
- Workspace splitter proof: dragging the context splitter widened the context pane from `300px` to `364px` and kept page overflow at `0`; dragging the inspector splitter widened the detail pane from `340px` to `398px` and kept page overflow at `0`. Screenshot saved to `build/codeviz/validation-workspace-splitter.png`.
- Splitter mobile proof: at `390x844`, the workspace splitters were hidden, page overflow stayed `0`, and no controls/panels were offscreen. Screenshot saved to `build/codeviz/validation-splitter-mobile.png`.
- Stack-path row proof: `recording=demo&selected=hb_ao` rendered a stack row path `hb_message:commit/3 <- hb_ao:resolve/3 <- dev_recorder:record/3` and preserved the full newline stack in the row title. Screenshot saved to `build/codeviz/validation-stack-path-rows.png`.
- Inspector event-click proof: `live=demo&selected=dev_scheduler` rendered an enabled `dev_scheduler/events` inspector event row; clicking it focused `dev_scheduler` and preserved the event key in the row title. Screenshot saved to `build/codeviz/validation-inspector-event-click.png`.
- Follow-focus/doc-clamp proof: `live=demo&follow=heat` kept the hot `dev_scheduler` node visible in the graph, clamped a long `320px` module doc to `170px`, and left live event rows visible in the inspector. Screenshot saved to `build/codeviz/validation-follow-focus-doc-clamp.png`.
- Selected-device pinning proof: with `recorder@1.0,scheduler@1.0` loaded, the first two visible context rows are active `~recorder@1.0` and `~scheduler@1.0`, followed by inactive devices. Screenshot saved to `build/codeviz/validation-selected-device-pinning.png`.

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
- Added support for linkified HyperBuddy event counter responses by fetching the formatted event message and parsing numeric counters from it.
- Added static event-topic aliases to modules/functions so live HyperBuddy counters can resolve through instrumentation names instead of only module-name guesses.
- Added event-row decay so recent live pulses remain inspectable across quiet poll ticks.
- Added compact animated event meters to the live event deck so relative event frequency is visible at a glance.
- Added selected-node live event rows to the inspector so hot modules explain which event streams are driving their heat.
- Added recorder black-box actions directly to the `dev_recorder` inspector so recordings and live stack overlays can be launched from the recorder node itself.
- Added event aliases to graph search so runtime counter names can be used as static navigation terms.
- Added clickable event-alias pills in the inspector for modules/functions/subsystems with known event emitters.
- Added `recording=<url>` support for loading recorder JSON/HTML artifacts directly from a URL.
- Tightened structured stack-frame heat so exact module/function frames do not expand through unrelated loose event aliases.
- Added approximate event/sec readouts to live event rows and inspector live-event rows.
- Added aggregate event/sec readout to live status text.
- Added a clickable minimap with a live viewport rectangle for faster navigation around large module/function layouts.
- Added static device bridge and kernel touchpoint rows for selected device contexts when no live overlay is active.
- Expanded selected device contexts with same-group modules that reference the selected device so helper/server pieces appear with their root device.
- Added a recorder timeline rail that can repaint aggregate recordings or focus an individual recorded event.
- Added `recording-event=N` URL state for shareable focused recorder playback.
- Tightened mobile stat sizing so the context count fits in the four-card summary row.
- Made live/recorded stack rows clickable debugger targets that can pull their resolved module/function into the graph.
- Added shareable `follow=heat` engine-view mode that follows the hottest live/recorded module while keeping the control explicit.
- Renamed live inspector metrics to numeric `Live heat` / `Error heat` labels for clearer engine-view telemetry.
- Added recorder timeline playback so imported or demo recordings can repaint their stack traces event-by-event.
- Added recorder-style draggable workspace splitters for resizing the context, graph, and inspector panes.
- Added compact stack-path trails to live/recorded inspector stack rows.
- Made selected-node live event rows in the inspector clickable graph targets.
- Clamped long inspector docs and made follow mode focus the hot node after render instead of broad-fitting the whole lens.
- Pinned selected devices to the top of the context device list with an active row treatment.

## Next Work

- Improve stack heat ranking and selected-node timeline affordances.
- Add stronger UI affordances for switching between event heat, stack heat, and recording heat.
- Continue browser validation after each presentation pass.
