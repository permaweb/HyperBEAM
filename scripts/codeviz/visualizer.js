(function () {
  const els = {
    embedded: document.getElementById("embedded-graph"),
    modules: document.getElementById("stat-modules"),
    functions: document.getElementById("stat-functions"),
    calls: document.getElementById("stat-calls"),
    context: document.getElementById("stat-context"),
    deviceCount: document.getElementById("device-count"),
    deviceList: document.getElementById("device-list"),
    deviceSearch: document.getElementById("device-search"),
    groupChips: document.getElementById("group-chips"),
    clearDevices: document.getElementById("clear-devices"),
    selectedDevicesMode: document.getElementById("selected-devices-mode"),
    allDevices: document.getElementById("all-devices"),
    scopeSelectionSummary: document.getElementById("scope-selection-summary"),
    search: document.getElementById("search"),
    groupFilter: document.getElementById("group-filter"),
    edgeFilter: document.getElementById("edge-filter"),
    contextScope: document.getElementById("context-scope"),
    showPrivate: document.getElementById("show-private"),
    showForge: document.getElementById("show-forge"),
    liveStatus: document.getElementById("live-status"),
    liveEndpoint: document.getElementById("live-endpoint"),
    liveInterval: document.getElementById("live-interval"),
    liveFollow: document.getElementById("live-follow"),
    liveConnect: document.getElementById("live-connect"),
    liveStack: document.getElementById("live-stack"),
    liveRateMode: document.getElementById("live-rate-mode"),
    recordingImport: document.getElementById("recording-import"),
    recordingFile: document.getElementById("recording-file"),
    liveDemo: document.getElementById("live-demo"),
    liveStop: document.getElementById("live-stop"),
    fitGraph: document.getElementById("fit-graph"),
    resetGraph: document.getElementById("reset-graph"),
    workspace: document.querySelector(".workspace"),
    contextPanel: document.querySelector(".context-panel"),
    graphTitle: document.getElementById("graph-title"),
    graphMeta: document.getElementById("graph-meta"),
    graphPanel: document.querySelector(".graph-panel"),
    stage: document.getElementById("graph-stage"),
    enginePanel: document.getElementById("engine-panel"),
    engineSource: document.getElementById("engine-source"),
    heatPanel: document.getElementById("heat-panel"),
    tracePanel: document.getElementById("trace-panel"),
    processPanel: document.getElementById("process-panel"),
    errorPanel: document.getElementById("error-panel"),
    recordingTimeline: document.getElementById("recording-timeline"),
    edgeCanvas: document.getElementById("edge-canvas"),
    svg: document.getElementById("graph"),
    viewport: document.getElementById("viewport"),
    bands: document.getElementById("bands"),
    edges: document.getElementById("edges"),
    nodes: document.getElementById("nodes"),
    minimap: document.getElementById("minimap"),
    minimapSvg: document.getElementById("minimap-svg"),
    minimapNodes: document.getElementById("minimap-nodes"),
    minimapView: document.getElementById("minimap-view"),
    detailEmpty: document.getElementById("detail-empty"),
    detailView: document.getElementById("detail-view"),
    scopeTab: document.getElementById("scope-tab"),
    inspectorTab: document.getElementById("inspector-tab"),
    engineTab: document.getElementById("engine-tab"),
    detailCard: document.getElementById("detail-card"),
    detailPanel: document.querySelector(".detail-panel"),
    selectionLabel: document.getElementById("selection-label"),
    callers: document.getElementById("callers"),
    callees: document.getElementById("callees")
  };

  const nf = new Intl.NumberFormat("en-US");
  const graph = JSON.parse(new TextDecoder().decode(base64ToBytes(els.embedded.textContent)));
  const byFunction = new Map(graph.functions.map((node) => [node.id, node]));
  const byModule = new Map(graph.modules.map((node) => [node.id, node]));
  const functionsByModule = graph.functions.reduce((acc, fun) => {
    if (!acc.has(fun.module)) acc.set(fun.module, []);
    acc.get(fun.module).push(fun);
    return acc;
  }, new Map());
  const outgoing = relationMap("source", "target");
  const incoming = relationMap("target", "source");
  const moduleOutgoing = moduleRelationMap("source-module", "target-module");
  const moduleIncoming = moduleRelationMap("target-module", "source-module");
  const functionsByModuleId = new Map([...functionsByModule.entries()]
    .map(([module, functions]) => [module, functions.map((fun) => fun.id)]));
  const liveIndex = buildLiveIndex();
  const liveResolutionCache = new Map();
  const perfProbe = window.__codevizPerf = {
    renders: 0,
    liveFrames: 0,
    edgeDraws: 0,
    lastRenderMs: 0,
    lastLiveFrameMs: 0,
    lastEdgeMs: 0,
    lastEdgeCount: 0
  };
  const defaultLiveEndpoint = "/~hyperbuddy@1.0/events";
  const defaultStackEndpoint = "/~recorder@1.0/live?limit=90&stack-limit=18";
  const defaultLiveInterval = 2200;

  const state = {
    mode: "system",
    layoutMode: "map",
    selectedDevices: new Set(),
    selected: null,
    detailTab: "scope",
    relationFocus: null,
    groupFocus: null,
    selectedEdge: null,
    selectedPath: [],
    hovered: null,
    search: "",
    deviceSearch: "",
    group: "",
    edgeMode: "context",
    contextScope: "auto",
    showPrivate: true,
    showForge: false,
    transform: { x: 40, y: 40, scale: 1 },
    layout: { nodes: [], edges: [], modules: [], bands: [], bounds: null },
    dragging: null,
    resizing: null,
    focusAfterRender: null,
    fitAfterRender: true,
    ignoreNextClick: false,
    minimap: null,
    edgeDrawFrame: null,
    live: {
      enabled: false,
      mode: "off",
      endpoint: "",
      intervalMs: defaultLiveInterval,
      follow: false,
      timer: null,
      previous: new Map(),
      activity: new Map(),
      errors: new Map(),
      samples: new Map(),
      traceEdges: new Map(),
      eventDeltas: new Map(),
      eventRates: new Map(),
      previousRates: new Map(),
      rateChanges: new Map(),
      rateActivity: new Map(),
      rateMode: true,
      eventHistory: new Map(),
      eventTick: 0,
      lastCounterAt: 0,
      totalDelta: 0,
      processCount: 0,
      processSamples: [],
      frameCount: 0,
      sourceName: "",
      recordingUrl: "",
      recordingEvents: [],
      recordingFocus: -1,
      recordingPlaying: false,
      recordingTimer: null,
      pendingRecordingFocus: -1,
      lastSeen: 0,
      lastPollStarted: 0,
      lastError: "",
      demoTick: 0
    }
  };

  function base64ToBytes(value) {
    const clean = (value || "").replace(/\s+/g, "");
    const binary = atob(clean);
    const bytes = new Uint8Array(binary.length);
    for (let idx = 0; idx < binary.length; idx += 1) {
      bytes[idx] = binary.charCodeAt(idx) & 255;
    }
    return bytes;
  }

  function relationMap(fromKey, toKey) {
    const out = new Map();
    graph.edges.forEach((edge) => {
      const from = edge[fromKey];
      const to = edge[toKey];
      if (!out.has(from)) out.set(from, []);
      out.get(from).push({ id: to, edge });
    });
    return out;
  }

  function moduleRelationMap(fromKey, toKey) {
    const out = new Map();
    graph.edges.forEach((edge) => {
      const from = edge[fromKey];
      const to = edge[toKey];
      if (!from || !to || from === to) return;
      if (!out.has(from)) out.set(from, new Map());
      const relations = out.get(from);
      const existing = relations.get(to);
      if (existing) {
        existing.edge.count += edge.count;
      } else {
        relations.set(to, { id: to, edge: { ...edge, count: edge.count } });
      }
    });
    return new Map([...out.entries()].map(([key, value]) => [key, [...value.values()]]));
  }

  function buildLiveIndex() {
    const index = new Map();
    const add = (key, id) => {
      const normalized = liveToken(key);
      if (!normalized || normalized.length < 2) return;
      if (!index.has(normalized)) index.set(normalized, new Set());
      index.get(normalized).add(id);
    };
    graph.modules.forEach((mod) => {
      add(mod.id, mod.id);
      add(mod.module, mod.id);
      add(`${mod.role}:${mod.group}`, mod.id);
      (mod["device-refs"] || []).forEach((ref) => {
        add(ref, mod.id);
        add(`~${ref}`, mod.id);
        add(ref.replace(/@.*$/, ""), mod.id);
      });
      (mod["event-topics"] || []).forEach((topic) => add(topic, mod.id));
    });
    graph.functions.forEach((fun) => {
      add(fun.id, fun.id);
      add(`${fun.module}:${fun.label}`, fun.id);
      add(`${fun.module}.${fun.label}`, fun.id);
      (fun.events || []).forEach((topic) => add(topic, fun.id));
    });
    return index;
  }

  function liveToken(value) {
    return String(value || "")
      .trim()
      .replace(/^~/, "")
      .toLowerCase();
  }

  function init() {
    applyInitialParams();
    renderGroupFilter();
    renderGroupChips();
    renderDevices();
    bindEvents();
    render();
    if (state.live.endpoint) startLive(state.live.endpoint);
    if (state.live.recordingUrl) loadRecordingUrl(state.live.recordingUrl);
    if (state.live.sourceName === "demo") applyRecordingReport(demoRecordingReport(), "demo");
  }

  function applyInitialParams() {
    const params = new URLSearchParams(window.location.search);
    const knownDevices = new Set(graph.devices.map((device) => device.id));
    if (params.get("devices") === "all") {
      graph.devices.forEach((device) => state.selectedDevices.add(device.id));
    } else {
      const devices = (params.get("devices") || "")
        .split(",")
        .map((value) => value.trim().replace(/^~/, ""))
        .filter(Boolean);
      devices.forEach((device) => {
        if (knownDevices.has(device)) state.selectedDevices.add(device);
      });
    }
    if (["system", "module", "function"].includes(params.get("mode"))) {
      state.mode = params.get("mode");
    }
    if (["map", "namespace", "flow"].includes(params.get("layout"))) {
      state.layoutMode = params.get("layout");
    }
    if (["auto", "kernel", "touchpoints"].includes(params.get("scope"))) {
      state.contextScope = params.get("scope");
      els.contextScope.value = state.contextScope;
    }
    if (params.has("search")) {
      state.search = params.get("search").trim().toLowerCase();
      els.search.value = params.get("search");
    }
    if (params.has("selected")) {
      state.selected = params.get("selected");
    }
    if (params.has("edge")) {
      const edge = edgeParamValue(params.get("edge"));
      if (edge) {
        state.selectedEdge = edge;
        if (!state.selected) state.selected = edge.target;
      }
    }
    if (params.has("group")) state.group = params.get("group");
    if (["context", "selected", "cross", "strong"].includes(params.get("edges"))) {
      state.edgeMode = params.get("edges");
      els.edgeFilter.value = state.edgeMode;
    }
    if (params.get("private") === "false") {
      state.showPrivate = false;
      els.showPrivate.checked = false;
    }
    if (params.get("forge") === "true") {
      state.showForge = true;
      els.showForge.checked = true;
    }
    if (params.has("live")) {
      state.live.endpoint = liveParamValue(params.get("live"));
      if (state.live.endpoint !== "demo") els.liveEndpoint.value = state.live.endpoint;
    }
    if (params.has("interval")) {
      state.live.intervalMs = intervalParamValue(params.get("interval"));
      setLiveIntervalSelect(state.live.intervalMs);
    }
    if (params.get("follow") === "heat") state.live.follow = true;
    if (params.get("pulse") === "rate") state.live.rateMode = true;
    if (["event", "events", "activity"].includes(params.get("pulse"))) {
      state.live.rateMode = false;
    }
    if (["scope", "inspector", "engine"].includes(params.get("panel"))) {
      state.detailTab = params.get("panel");
    }
    if (state.selected && !params.has("panel")) state.detailTab = "inspector";
    if (params.get("recording") === "demo") {
      state.live.sourceName = "demo";
    } else if (params.has("recording")) {
      state.live.recordingUrl = params.get("recording");
    }
    if (params.has("recording-event")) {
      const focus = Number(params.get("recording-event"));
      if (Number.isInteger(focus) && focus > 0) {
        state.live.pendingRecordingFocus = focus - 1;
      }
    }
    document.querySelectorAll("[data-mode]").forEach((button) => {
      button.classList.toggle("active", button.dataset.mode === state.mode);
    });
    document.querySelectorAll("[data-layout]").forEach((button) => {
      button.classList.toggle("active", button.dataset.layout === state.layoutMode);
    });
    syncDetailTabs();
  }

  function liveParamValue(value) {
    if (!value || value === "true" || value === "1") return defaultLiveEndpoint;
    if (value === "stack") return defaultStackEndpoint;
    return value;
  }

  function intervalParamValue(value) {
    const raw = String(value || "").trim().replace(/s$/i, "");
    const numeric = Number(raw);
    const ms = numeric > 50 ? numeric : numeric * 1000;
    if (!Number.isFinite(ms)) return defaultLiveInterval;
    return Math.max(1000, Math.min(30000, Math.round(ms)));
  }

  function setLiveIntervalSelect(ms) {
    const value = String(ms);
    if (!els.liveInterval.querySelector(`option[value="${value}"]`)) {
      const option = document.createElement("option");
      option.value = value;
      option.textContent = formatInterval(ms);
      els.liveInterval.append(option);
    }
    els.liveInterval.value = value;
  }

  function bindEvents() {
    document.querySelectorAll("[data-mode]").forEach((button) => {
      button.addEventListener("click", () => {
        activateMode(button.dataset.mode);
        state.selected = null;
        state.relationFocus = null;
        state.groupFocus = null;
        state.selectedEdge = null;
        state.selectedPath = [];
        requestFit();
        render();
      });
    });
    document.querySelectorAll("[data-layout]").forEach((button) => {
      button.addEventListener("click", () => {
        activateLayout(button.dataset.layout);
        state.relationFocus = null;
        state.groupFocus = null;
        state.selectedEdge = null;
        state.selectedPath = [];
        requestFit();
        render();
      });
    });
    document.querySelectorAll("[data-detail-tab]").forEach((button) => {
      button.addEventListener("click", () => activateDetailTab(button.dataset.detailTab));
    });

    els.search.addEventListener("input", () => {
      state.search = els.search.value.trim().toLowerCase();
      requestFit();
      render();
    });
    els.deviceSearch.addEventListener("input", () => {
      state.deviceSearch = els.deviceSearch.value.trim().toLowerCase();
      renderDevices();
    });
    els.groupFilter.addEventListener("change", () => {
      state.group = els.groupFilter.value;
      requestFit();
      render();
    });
    els.edgeFilter.addEventListener("change", () => {
      state.edgeMode = els.edgeFilter.value;
      requestFit();
      render();
    });
    els.contextScope.addEventListener("change", () => {
      state.contextScope = els.contextScope.value;
      requestFit();
      render();
    });
    els.showPrivate.addEventListener("change", () => {
      state.showPrivate = els.showPrivate.checked;
      requestFit();
      render();
    });
    els.showForge.addEventListener("change", () => {
      state.showForge = els.showForge.checked;
      requestFit();
      render();
    });
    els.liveConnect.addEventListener("click", () => {
      startLive(els.liveEndpoint.value.trim() || defaultLiveEndpoint);
    });
    els.liveEndpoint.addEventListener("keydown", (event) => {
      if (event.key === "Enter") startLive(els.liveEndpoint.value.trim() || defaultLiveEndpoint);
    });
    els.liveInterval.addEventListener("change", () => {
      state.live.intervalMs = intervalParamValue(els.liveInterval.value);
      if (state.live.enabled && ["demo", "events", "stack"].includes(state.live.mode)) {
        startLive(state.live.endpoint || "demo");
      } else {
        render();
      }
    });
    els.liveFollow.addEventListener("click", () => {
      state.live.follow = !state.live.follow;
      if (state.live.follow) {
        const target = heatFollowTarget();
        if (target) {
          state.selected = target;
          state.groupFocus = null;
          state.focusAfterRender = target;
        } else {
          requestFit();
        }
      }
      render();
    });
    els.liveRateMode.addEventListener("click", () => {
      state.live.rateMode = !state.live.rateMode;
      render();
    });
    els.liveStack.addEventListener("click", () => startLive(defaultStackEndpoint));
    els.recordingImport.addEventListener("click", openRecordingImport);
    els.recordingFile.addEventListener("change", importRecordingFile);
    els.liveDemo.addEventListener("click", () => startLive("demo"));
    els.liveStop.addEventListener("click", stopLive);
    els.clearDevices.addEventListener("click", () => {
      state.selectedDevices.clear();
      state.selected = null;
      state.relationFocus = null;
      state.groupFocus = null;
      state.selectedEdge = null;
      state.selectedPath = [];
      requestFit();
      renderDevices();
      render();
      showGraph();
    });
    els.allDevices.addEventListener("click", () => {
      graph.devices.forEach((device) => state.selectedDevices.add(device.id));
      state.selected = null;
      state.relationFocus = null;
      state.groupFocus = null;
      state.selectedEdge = null;
      state.selectedPath = [];
      requestFit();
      renderDevices();
      render();
      showGraph();
    });
    els.selectedDevicesMode.addEventListener("click", () => {
      els.deviceSearch.focus();
    });
    els.fitGraph.addEventListener("click", () => fitGraph(false));
    els.resetGraph.addEventListener("click", () => {
      const viewport = graphViewport();
      state.transform = { x: viewport.x, y: viewport.y, scale: 1 };
      applyTransform();
    });
    els.svg.addEventListener("wheel", onWheel, { passive: false });
    els.svg.addEventListener("pointerdown", startPan);
    els.svg.addEventListener("click", clearSelectionFromBackground);
    els.svg.addEventListener("pointerleave", () => setHoveredNode(null));
    els.minimap.addEventListener("pointerdown", (event) => event.stopPropagation());
    els.minimap.addEventListener("click", onMinimapClick);
    document.querySelectorAll(".workspace-splitter").forEach((splitter) => {
      splitter.addEventListener("pointerdown", startWorkspaceResize);
    });
    window.addEventListener("pointermove", moveWorkspaceResize);
    window.addEventListener("pointermove", movePan);
    window.addEventListener("pointerup", endWorkspaceResize);
    window.addEventListener("pointerup", endPan);
  }

  function renderGroupFilter() {
    graph.groups.forEach((group) => {
      const option = document.createElement("option");
      option.value = group.id;
      option.textContent = `${group.label} (${group.modules})`;
      els.groupFilter.appendChild(option);
    });
    els.groupFilter.value = state.group;
  }

  function renderGroupChips() {
    const groups = [...new Set(graph.devices.map((device) => device.group))].sort();
    els.groupChips.replaceChildren(...groups.map((group) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "group-chip";
      const devices = graph.devices.filter((device) => device.group === group);
      const selectedCount = devices
        .filter((device) => state.selectedDevices.has(device.id))
        .length;
      const allActive = selectedCount === devices.length;
      const partiallyActive = selectedCount > 0 && !allActive;
      button.textContent = partiallyActive ?
        `${group} (${selectedCount}/${devices.length})` :
        `${allActive ? "✓ " : ""}${group} (${devices.length})`;
      button.setAttribute("aria-pressed", partiallyActive ? "mixed" : selectedCount ? "true" : "false");
      button.title = allActive ?
        `Remove ${group} devices from the map` :
        partiallyActive ?
        `Add remaining ${group} devices to the map` :
        `Add ${group} devices to the map`;
      button.addEventListener("click", () => {
        devices.forEach((device) => {
          if (allActive) {
            state.selectedDevices.delete(device.id);
          } else {
            state.selectedDevices.add(device.id);
          }
        });
        state.selected = null;
        state.relationFocus = null;
        state.groupFocus = null;
        state.selectedEdge = null;
        state.selectedPath = [];
        requestFit();
        renderGroupChips();
        renderDevices();
        render();
        showGraph();
      });
      button.classList.toggle("active", selectedCount > 0);
      button.classList.toggle("partial", partiallyActive);
      return button;
    }));
  }

  function renderDevices() {
    const needle = state.deviceSearch;
    const rows = graph.devices
      .filter((device) => {
        const haystack = `${device.label} ${device.id} ${device.group}`.toLowerCase();
        return !needle || haystack.includes(needle);
      })
      .sort((a, b) => {
        const selectedDelta =
          Number(state.selectedDevices.has(b.id)) - Number(state.selectedDevices.has(a.id));
        if (selectedDelta) return selectedDelta;
        return `${a.group}:${a.label}`.localeCompare(`${b.group}:${b.label}`);
      })
      .map((device) => deviceRow(device));
    els.deviceList.replaceChildren(...rows);
    renderScopeState();
    renderGroupChips();
  }

  function renderScopeState() {
    const count = state.selectedDevices.size;
    els.deviceCount.textContent = count ?
      countLabel(count, "device", "devices") :
      "kernel only";
    els.clearDevices.classList.toggle("active", count === 0);
    els.selectedDevicesMode.classList.toggle("active", count > 0 && count < graph.devices.length);
    els.allDevices.classList.toggle("active", count === graph.devices.length);
    els.scopeSelectionSummary.textContent = scopeSelectionSummary();
    const autoOption = els.contextScope.querySelector('option[value="auto"]');
    if (autoOption) {
      autoOption.textContent = state.selectedDevices.size ?
        "Auto: kernel + selected devices" :
        "Auto: kernel only";
    }
  }

  function scopeSelectionSummary() {
    const selected = graph.devices.filter((device) => state.selectedDevices.has(device.id));
    if (!selected.length) return "Kernel modules only";
    if (selected.length === graph.devices.length) return "All packaged devices loaded";
    const names = selected.slice(0, 3).map((device) => `~${device.id}`);
    const suffix = selected.length > names.length ? ` +${selected.length - names.length} more` : "";
    return names.join(", ") + suffix;
  }

  function deviceRow(device) {
    const label = document.createElement("label");
    label.className = "device-row";
    label.classList.toggle("active", state.selectedDevices.has(device.id));
    const input = document.createElement("input");
    input.type = "checkbox";
    input.checked = state.selectedDevices.has(device.id);
    input.addEventListener("change", () => {
      if (input.checked) {
        state.selectedDevices.add(device.id);
      } else {
        state.selectedDevices.delete(device.id);
      }
      state.selected = null;
      state.relationFocus = null;
      state.groupFocus = null;
      state.selectedEdge = null;
      state.selectedPath = [];
      requestFit();
      renderDevices();
      render();
      showGraph();
    });
    const body = document.createElement("div");
    const name = document.createElement("div");
    name.className = "device-name";
    name.textContent = device.label;
    const meta = document.createElement("div");
    meta.className = "device-meta";
    meta.textContent = `${device.group} / ${device.modules.length} modules / ${device.functions} functions`;
    body.append(name, meta);
    label.append(input, body);
    return label;
  }

  function render() {
    const started = performance.now();
    perfProbe.renders += 1;
    syncDetailTabs();
    const followTarget = heatFollowTarget();
    if (followTarget && followTarget !== state.selected) {
      state.selected = followTarget;
      state.relationFocus = null;
      state.groupFocus = null;
      state.focusAfterRender = followTarget;
    }
    const visible = visibleData();
    state.visible = visible;
    state.layout = layout(visible);
    pruneGroupFocusToLayout();
    if (state.hovered && !state.layout.nodes.some((node) => node.id === state.hovered)) {
      state.hovered = null;
    }
    renderStats(visible);
    renderGraph();
    renderInspector();
    syncUrl();
    if (state.focusAfterRender) {
      const target = state.focusAfterRender;
      state.focusAfterRender = null;
      state.fitAfterRender = false;
      focusNode(target);
    } else if (state.fitAfterRender) {
      state.fitAfterRender = false;
      fitGraph(true);
    }
    perfProbe.lastRenderMs = performance.now() - started;
  }

  function requestFit() {
    state.fitAfterRender = true;
  }

  function activateMode(mode) {
    state.mode = mode;
    document.querySelectorAll("[data-mode]").forEach((el) => {
      el.classList.toggle("active", el.dataset.mode === mode);
    });
  }

  function activateLayout(layoutMode) {
    state.layoutMode = layoutMode;
    document.querySelectorAll("[data-layout]").forEach((el) => {
      el.classList.toggle("active", el.dataset.layout === layoutMode);
    });
  }

  function activateDetailTab(tab) {
    state.detailTab = ["scope", "inspector", "engine"].includes(tab) ? tab : "scope";
    syncDetailTabs();
    syncUrl();
  }

  function syncDetailTabs() {
    document.querySelectorAll("[data-detail-tab]").forEach((button) => {
      button.classList.toggle("active", button.dataset.detailTab === state.detailTab);
    });
    els.scopeTab.hidden = state.detailTab !== "scope";
    els.scopeTab.classList.toggle("active", state.detailTab === "scope");
    els.inspectorTab.hidden = state.detailTab !== "inspector";
    els.inspectorTab.classList.toggle("active", state.detailTab === "inspector");
    els.engineTab.hidden = state.detailTab !== "engine";
    els.engineTab.classList.toggle("active", state.detailTab === "engine");
  }

  function selectNode(id, options = {}) {
    if (options.manual) state.live.follow = false;
    state.detailTab = "inspector";
    syncDetailTabs();
    state.relationFocus = options.relationFocus ? id : null;
    state.groupFocus = null;
    state.selectedEdge = options.edge || null;
    state.selectedPath = options.path || [];
    state.hovered = null;
    state.selected = id;
    if (options.render === true || !layoutHasNode(id)) {
      render();
    } else {
      refreshSelectionState();
    }
    if (options.focus === true) focusNode(id);
    if (options.showGraph) showGraph();
  }

  function clearSelectedNode() {
    state.live.follow = false;
    state.selected = null;
    state.relationFocus = null;
    state.groupFocus = null;
    state.selectedEdge = null;
    state.selectedPath = [];
    state.hovered = null;
    state.detailTab = "scope";
    refreshSelectionState();
  }

  function layoutHasNode(id) {
    return !!state.layout && state.layout.nodes.some((node) => node.id === id);
  }

  function refreshSelectionState() {
    syncDetailTabs();
    applyRelationClasses();
    renderInspector();
    syncUrl();
  }

  function selectGroupFrame(frame, options = {}) {
    const nodeIds = groupFrameNodeIds(frame);
    if (state.mode !== "module" || !nodeIds.length) return;
    if (options.manual) state.live.follow = false;
    const id = groupFrameKey(frame);
    if (state.groupFocus && state.groupFocus.id === id) {
      clearSelectedNode();
      return;
    }
    state.selected = null;
    state.relationFocus = null;
    state.selectedEdge = null;
    state.selectedPath = [];
    state.hovered = null;
    state.detailTab = "scope";
    state.groupFocus = {
      id,
      title: frame.title || frame.label || frame.id || "Selection",
      subtitle: frame.subtitle || "",
      nodeIds,
      nodeSet: new Set(nodeIds)
    };
    refreshSelectionState();
  }

  function groupFrameNodeIds(frame) {
    if (Array.isArray(frame.nodeIds)) return frame.nodeIds.filter((id) => layoutHasNode(id));
    if (frame.id && layoutHasNode(frame.id)) return [frame.id];
    return [];
  }

  function groupFrameKey(frame) {
    return `${frame.id || frame.title || frame.label}:${(frame.nodeIds || []).join(",")}`;
  }

  function groupFrameIsSelected(frame) {
    return !!state.groupFocus && state.groupFocus.id === groupFrameKey(frame);
  }

  function groupFocusHasNode(id) {
    return !!state.groupFocus && state.groupFocus.nodeSet.has(id);
  }

  function groupFocusHasEdge(edge) {
    return !!state.groupFocus && (
      groupFocusHasNode(edge.source) ||
      groupFocusHasNode(edge.target)
    );
  }

  function pruneGroupFocusToLayout() {
    if (!state.groupFocus) return;
    const visible = state.groupFocus.nodeIds.filter((id) => layoutHasNode(id));
    if (!visible.length) {
      state.groupFocus = null;
      return;
    }
    if (visible.length !== state.groupFocus.nodeIds.length) {
      state.groupFocus.nodeIds = visible;
      state.groupFocus.nodeSet = new Set(visible);
    }
  }

  function setHoveredNode(id) {
    const next = id || null;
    if (state.hovered === next) return;
    state.hovered = next;
    applyRelationClasses();
  }

  function syncUrl() {
    const params = new URLSearchParams();
    params.set("mode", state.mode);
    if (state.layoutMode !== "map") params.set("layout", state.layoutMode);
    if (state.selectedDevices.size) {
      params.set(
        "devices",
        state.selectedDevices.size === graph.devices.length ?
          "all" :
          [...state.selectedDevices].sort().join(",")
      );
    }
    if (state.selected) params.set("selected", state.selected);
    if (state.selectedEdge) params.set("edge", edgeUrlParam(state.selectedEdge));
    if (state.search) params.set("search", state.search);
    if (state.group) params.set("group", state.group);
    if (state.edgeMode !== "context") params.set("edges", state.edgeMode);
    if (state.contextScope !== "auto") params.set("scope", state.contextScope);
    if (!state.showPrivate) params.set("private", "false");
    if (state.showForge) params.set("forge", "true");
    if (state.live.enabled && state.live.mode !== "recording") {
      params.set("live", liveUrlParam());
      if (state.live.intervalMs !== defaultLiveInterval) {
        params.set("interval", String(state.live.intervalMs / 1000));
      }
    }
    if (state.live.enabled && state.live.follow) params.set("follow", "heat");
    if (!state.live.rateMode) params.set("pulse", "events");
    if (state.detailTab !== "inspector") params.set("panel", state.detailTab);
    if (state.live.mode === "recording" && state.live.sourceName === "demo") {
      params.set("recording", "demo");
    } else if (state.live.mode === "recording" && state.live.recordingUrl) {
      params.set("recording", state.live.recordingUrl);
    }
    if (state.live.mode === "recording" && state.live.recordingFocus >= 0) {
      params.set("recording-event", String(state.live.recordingFocus + 1));
    }
    const query = params.toString();
    const next = `${window.location.pathname}${query ? `?${query}` : ""}`;
    window.history.replaceState(null, "", next);
  }

  function liveUrlParam() {
    if (state.live.mode === "demo") return "demo";
    if (state.live.mode === "stack" && state.live.endpoint === defaultStackEndpoint) return "stack";
    return state.live.endpoint;
  }

  function edgeParamValue(value) {
    const [kind, source, target, count] = String(value || "").split("|");
    if (!["call", "trace"].includes(kind) || !source || !target) return null;
    const parsedCount = Number(count);
    return {
      kind,
      source,
      target,
      count: Number.isFinite(parsedCount) && parsedCount > 0 ? parsedCount : 1
    };
  }

  function edgeUrlParam(edge) {
    return [
      edge.kind || "call",
      edge.source || "",
      edge.target || "",
      String(Math.round(edge.count || 1))
    ].join("|");
  }

  function functionSearchText(fun) {
    return [
      fun.id,
      fun.path,
      fun.namespace,
      fun.category,
      fun["component-kind"],
      (fun["source-dirs"] || []).join("/"),
      fun.doc,
      (fun["device-refs"] || []).join(" "),
      (fun.events || []).join(" ")
    ].join(" ")
      .toLowerCase();
  }

  function moduleSearchText(mod) {
    return [
      mod.id,
      mod.path,
      mod.namespace,
      mod.category,
      mod["component-kind"],
      (mod["source-dirs"] || []).join("/"),
      mod.doc,
      (mod["device-refs"] || []).join(" "),
      (mod["event-topics"] || []).join(" ")
    ].join(" ")
      .toLowerCase();
  }

  function visibleData() {
    const activeModules = new Set();
    const selectedFunction = byFunction.get(state.selected);
    const scope = effectiveContextScope();
    const compactDeviceFunctions =
      state.mode === "function" &&
      scope === "touchpoints" &&
      state.selectedDevices.size &&
      !state.search &&
      (!state.selected || (mapLayoutModeActive() && selectedFunction));
    const deviceModules = activeDeviceModules({ includeReferences: !compactDeviceFunctions });
    graph.modules.forEach((mod) => {
      if (!compactDeviceFunctions && mod.role === "kernel") activeModules.add(mod.id);
      if (state.showForge && mod.role === "forge") activeModules.add(mod.id);
    });
    deviceModules.forEach((module) => activeModules.add(module));
    const selectedModule = byModule.get(state.selected) || (selectedFunction && byModule.get(selectedFunction.module));
    if (selectedModule) activeModules.add(selectedModule.id);

    const needle = state.search;
    const groupFilter = state.group;
    const functionInScope = (fun) => {
      if (fun.id === state.selected) return true;
      if (!activeModules.has(fun.module)) return false;
      if (!state.showPrivate && !fun.exported) return false;
      if (groupFilter && `${fun.role}:${fun.group}` !== groupFilter) return false;
      return true;
    };
    const moduleInScope = (mod) => {
      if (selectedModule && mod.id === selectedModule.id) return true;
      if (!activeModules.has(mod.id)) return false;
      if (groupFilter && `${mod.role}:${mod.group}` !== groupFilter) return false;
      return true;
    };
    let functions = graph.functions.filter((fun) => {
      if (!functionInScope(fun)) return false;
      if (!needle) return true;
      return functionSearchText(fun).includes(needle);
    });
    if (compactDeviceFunctions) {
      functions = compactDeviceFunctionSet(functions, selectedFunction);
    }
    let modules = graph.modules.filter((mod) => {
      if (!moduleInScope(mod)) return false;
      if (!needle) return true;
      return moduleSearchText(mod).includes(needle) ||
        functions.some((fun) => fun.module === mod.id);
    });
    if (compactDeviceFunctions) {
      const functionModules = new Set(functions.map((fun) => fun.module));
      modules = graph.modules.filter((mod) =>
        functionModules.has(mod.id) &&
        (!groupFilter || `${mod.role}:${mod.group}` === groupFilter)
      );
    }
    if (state.selected && !mapLayoutModeActive()) {
      functions = expandSelectedFunctions(functions, functionInScope);
      modules = expandSelectedModules(modules, functions, moduleInScope);
    }
    const functionIds = new Set(functions.map((fun) => fun.id));
    const moduleIds = new Set(modules.map((mod) => mod.id));
    let edges = graph.edges.filter((edge) => {
      if (state.mode === "module" || state.mode === "system") {
        return moduleIds.has(edge["source-module"]) &&
          moduleIds.has(edge["target-module"]) &&
          edge["source-module"] !== edge["target-module"];
      }
      return functionIds.has(edge.source) && functionIds.has(edge.target);
    });
    if (state.edgeMode === "cross") {
      edges = edges.filter((edge) => edge["source-module"] !== edge["target-module"]);
    } else if (state.edgeMode === "selected" && state.selected) {
      const selected = state.selected;
      edges = edges.filter((edge) => {
        if (state.mode === "system") {
          const sourceModule = byModule.get(edge["source-module"]);
          const targetModule = byModule.get(edge["target-module"]);
          return (sourceModule && systemId(sourceModule) === selected) ||
            (targetModule && systemId(targetModule) === selected);
        }
        if (state.mode === "module") {
          return edge["source-module"] === selected || edge["target-module"] === selected;
        }
        return edge.source === selected || edge.target === selected;
      });
    } else if (state.edgeMode === "strong" && state.mode === "function") {
      edges = edges.filter((edge) => edge.count > 1);
    }
    return { modules, functions, edges };
  }

  function expandDeviceFunctionTouchpoints(functions) {
    const ids = new Set(functions.map((fun) => fun.id));
    const additions = new Set();
    graph.edges.forEach((edge) => {
      const sourceInside = ids.has(edge.source);
      const targetInside = ids.has(edge.target);
      if (sourceInside === targetInside) return;
      const other = byFunction.get(sourceInside ? edge.target : edge.source);
      const mod = other && byModule.get(other.module);
      if (!other || !mod) return;
      if (!state.showPrivate && !other.exported) return;
      if (state.group && `${other.role}:${other.group}` !== state.group) return;
      if (!["kernel", "device"].includes(mod.role)) return;
      additions.add(other.id);
    });
    if (!additions.size) return functions;
    const expanded = functions.slice();
    graph.functions.forEach((fun) => {
      if (additions.has(fun.id)) expanded.push(fun);
    });
    return expanded;
  }

  function compactDeviceFunctionSet(functions, selectedFunction) {
    const deviceFunctions = functions.filter((fun) => {
      const mod = byModule.get(fun.module);
      return mod && mod.role === "device";
    });
    const expanded = expandDeviceFunctionTouchpoints(deviceFunctions);
    if (selectedFunction && !expanded.some((fun) => fun.id === selectedFunction.id)) {
      expanded.push(selectedFunction);
    }
    return expanded;
  }

  function activeDeviceModules(options = {}) {
    const includeReferences = options.includeReferences !== false;
    const modules = new Set();
    const selectedGroups = new Map();
    const selectedNamespaces = new Set();
    const selectedRefs = new Set();
    graph.devices.forEach((device) => {
      if (state.selectedDevices.has(device.id)) {
        selectedGroups.set(device.id, device.group);
        device.modules.forEach((module) => {
          modules.add(module);
          const mod = byModule.get(module);
          if (!mod) return;
          if (mod.namespace) selectedNamespaces.add(mod.namespace);
          (mod["device-refs"] || []).forEach((ref) => selectedRefs.add(ref));
        });
      }
    });
    graph.devices.forEach((device) => {
      if (includeReferences && selectedRefs.has(device.id)) {
        device.modules.forEach((module) => modules.add(module));
      }
    });
    graph.modules.forEach((mod) => {
      const refs = mod["device-refs"] || [];
      if (includeReferences && refs.some((ref) => selectedGroups.get(ref) === mod.group)) {
        modules.add(mod.id);
      }
      if (includeReferences && refs.some((ref) => state.selectedDevices.has(ref) || selectedRefs.has(ref))) {
        modules.add(mod.id);
      }
      if (mod.role === "device" && selectedNamespaces.has(mod.namespace)) modules.add(mod.id);
    });
    return modules;
  }

  function mapLayoutModeActive() {
    return state.layoutMode === "map" || state.layoutMode === "namespace";
  }

  function namespaceLayoutActive() {
    return state.layoutMode === "namespace";
  }

  function effectiveContextScope() {
    if (state.contextScope !== "auto") return state.contextScope;
    if (state.mode === "function" && state.selectedDevices.size && !state.search) {
      return "touchpoints";
    }
    return "kernel";
  }

  function filterLayoutEdges(edges) {
    return edges.filter((edge) => state.edgeMode !== "strong" || edge.count > 1);
  }

  function expandSelectedFunctions(functions, functionInScope) {
    const selected = byFunction.get(state.selected);
    if (!selected) return functions;
    const ids = new Set([selected.id]);
    (incoming.get(selected.id) || []).forEach((rel) => ids.add(rel.id));
    (outgoing.get(selected.id) || []).forEach((rel) => ids.add(rel.id));
    const existing = new Set(functions.map((fun) => fun.id));
    const expanded = functions.slice();
    graph.functions.forEach((fun) => {
      if (!ids.has(fun.id) || existing.has(fun.id) || !functionInScope(fun)) return;
      expanded.push(fun);
      existing.add(fun.id);
    });
    return expanded;
  }

  function expandSelectedModules(modules, functions, moduleInScope) {
    const ids = new Set(functions.map((fun) => fun.module));
    const selected = byModule.get(state.selected);
    if (selected) {
      ids.add(selected.id);
      (moduleIncoming.get(selected.id) || []).forEach((rel) => ids.add(rel.id));
      (moduleOutgoing.get(selected.id) || []).forEach((rel) => ids.add(rel.id));
    }
    const existing = new Set(modules.map((mod) => mod.id));
    const expanded = modules.slice();
    graph.modules.forEach((mod) => {
      if (!ids.has(mod.id) || existing.has(mod.id) || !moduleInScope(mod)) return;
      expanded.push(mod);
      existing.add(mod.id);
    });
    return expanded;
  }

  function layout(visible) {
    if (state.mode === "system") {
      return state.selected ? selectedSystemLayout(visible) : systemLayout(visible);
    }
    if (state.layoutMode === "flow" && state.mode === "module" && byModule.has(state.selected)) {
      return selectedModuleLayout(visible);
    }
    if (state.layoutMode === "flow" && state.mode === "function" && byFunction.has(state.selected)) {
      return selectedFunctionLayout(visible);
    }
    const nodes = state.mode === "module" ? moduleGraphNodes(visible) : functionGraphNodes(visible);
    const graphEdges = state.mode === "module" ? moduleEdges(visible.edges) : visible.edges;
    const positioned = forceMapActive(nodes) ?
      positionForceMapNodes(nodes, graphEdges) :
      state.mode === "function" ?
        positionFunctionNodes(nodes, visible.edges) :
        positionNodes(nodes);
    const nodeById = new Map(positioned.nodes.map((node) => [node.id, node]));
    const edges = filterLayoutEdges(graphEdges)
      .map((edge) => {
        const source = nodeById.get(edge.source);
        const target = nodeById.get(edge.target);
        if (!source || !target) return null;
        return { ...edge, sourceNode: source, targetNode: target };
      })
      .filter(Boolean);
    return { ...positioned, edges };
  }

  function selectedSystemLayout(visible) {
    const allNodes = systemGraphNodes(visible);
    const byId = new Map(allNodes.map((node) => [node.id, node]));
    const selected = byId.get(state.selected);
    if (!selected) return systemLayout(visible);
    const allEdges = systemEdges(visible.edges);
    const callerIds = new Set(allEdges
      .filter((edge) => edge.target === selected.id)
      .map((edge) => edge.source));
    const calleeIds = new Set(allEdges
      .filter((edge) => edge.source === selected.id)
      .map((edge) => edge.target));
    const callers = [...callerIds]
      .filter((id) => !calleeIds.has(id))
      .map((id) => byId.get(id))
      .filter(Boolean)
      .sort((a, b) => a.title.localeCompare(b.title));
    const callees = [...calleeIds]
      .map((id) => byId.get(id))
      .filter(Boolean)
      .sort((a, b) => a.title.localeCompare(b.title));
    const selectedNode = systemLensNode(selected, 320, 84);
    const callerNodes = callers.map((node) => systemLensNode(node, 290, 76));
    const calleeNodes = callees.map((node) => systemLensNode(node, 290, 76));
    const maxRows = Math.max(callerNodes.length, calleeNodes.length, 1);
    const stackHeight = maxRows * 92;
    const centerY = 72 + Math.max(0, (stackHeight - selectedNode.height) / 2);
    const nodes = [
      ...placeSystemStack(callerNodes, 40, 72),
      {
        ...selectedNode,
        x: 390,
        y: centerY,
        cx: 390 + selectedNode.width / 2,
        cy: centerY + selectedNode.height / 2
      },
      ...placeSystemStack(calleeNodes, 780, 72)
    ];
    const nodeById = new Map(nodes.map((node) => [node.id, node]));
    const ids = new Set(nodes.map((node) => node.id));
    const edges = filterLayoutEdges(allEdges)
      .filter((edge) => ids.has(edge.source) && ids.has(edge.target))
      .filter((edge) => edge.source === selected.id || edge.target === selected.id)
      .map((edge) => ({
        ...edge,
        sourceNode: nodeById.get(edge.source),
        targetNode: nodeById.get(edge.target)
      }))
      .filter((edge) => edge.sourceNode && edge.targetNode);
    const height = Math.max(540, stackHeight + 128);
    return {
      nodes,
      modules: [],
      edges,
      lens: true,
      bands: [
        { id: "system-callers", x: 20, y: 16, width: 340, height, label: "Callers" },
        { id: "system-selected", x: 370, y: 16, width: 370, height, label: selected.role },
        { id: "system-callees", x: 760, y: 16, width: 340, height, label: "Callees" }
      ],
      bounds: { x: 0, y: 0, width: 1120, height: height + 24 }
    };
  }

  function systemLensNode(node, width, height) {
    return {
      ...node,
      width,
      height
    };
  }

  function placeSystemStack(nodes, x, y) {
    return nodes.map((node, idx) => ({
      ...node,
      x,
      y: y + idx * 92,
      cx: x + node.width / 2,
      cy: y + idx * 92 + node.height / 2
    }));
  }

  function selectedModuleLayout(visible) {
    const selected = byModule.get(state.selected);
    const visibleById = new Map(visible.modules.map((mod) => [mod.id, mod]));
    let callers = (moduleIncoming.get(selected.id) || [])
      .map((rel) => visibleById.get(rel.id))
      .filter(Boolean)
      .sort(nodeSort);
    const callees = (moduleOutgoing.get(selected.id) || [])
      .map((rel) => visibleById.get(rel.id))
      .filter(Boolean)
      .sort(nodeSort);
    const calleeIds = new Set(callees.map((mod) => mod.id));
    callers = callers.filter((mod) => !calleeIds.has(mod.id));
    const selectedNode = moduleLensNode(selected, 330, 58);
    const callerNodes = callers.map((mod) => moduleLensNode(mod, 300, 52));
    const calleeNodes = callees.map((mod) => moduleLensNode(mod, 300, 52));
    const maxRows = Math.max(callerNodes.length, calleeNodes.length, 1);
    const stackHeight = maxRows * 62;
    const centerY = 72 + Math.max(0, (stackHeight - selectedNode.height) / 2);
    const nodes = [
      ...placeModuleStack(callerNodes, 40, 72),
      {
        ...selectedNode,
        x: 390,
        y: centerY,
        cx: 390 + selectedNode.width / 2,
        cy: centerY + selectedNode.height / 2
      },
      ...placeModuleStack(calleeNodes, 780, 72)
    ];
    const nodeById = new Map(nodes.map((node) => [node.id, node]));
    const ids = new Set(nodes.map((node) => node.id));
    const edges = filterLayoutEdges(moduleEdges(visible.edges))
      .filter((edge) => ids.has(edge.source) && ids.has(edge.target))
      .filter((edge) => edge.source === selected.id || edge.target === selected.id)
      .map((edge) => ({
        ...edge,
        sourceNode: nodeById.get(edge.source),
        targetNode: nodeById.get(edge.target)
      }))
      .filter((edge) => edge.sourceNode && edge.targetNode);
    const height = Math.max(540, stackHeight + 128);
    return {
      nodes,
      modules: [],
      edges,
      lens: true,
      bands: [
        { id: "module-callers", x: 20, y: 16, width: 340, height, label: "Callers" },
        { id: "module-selected", x: 370, y: 16, width: 370, height, label: selected.group },
        { id: "module-callees", x: 760, y: 16, width: 340, height, label: "Callees" }
      ],
      bounds: { x: 0, y: 0, width: 1120, height: height + 24 }
    };
  }

  function moduleLensNode(mod, width, height) {
    return {
      ...mod,
      kind: "module",
      title: mod.module,
      subtitle: `${mod.functions} functions · ${mod.exports} exports`,
      width,
      height
    };
  }

  function placeModuleStack(nodes, x, y) {
    return nodes.map((node, idx) => ({
      ...node,
      x,
      y: y + idx * 62,
      cx: x + node.width / 2,
      cy: y + idx * 62 + node.height / 2
    }));
  }

  function selectedFunctionLayout(visible) {
    const selected = byFunction.get(state.selected);
    const visibleById = new Map(visible.functions.map((fun) => [fun.id, fun]));
    let callers = (incoming.get(selected.id) || [])
      .map((rel) => visibleById.get(rel.id))
      .filter(Boolean)
      .sort(nodeSort);
    const callees = (outgoing.get(selected.id) || [])
      .map((rel) => visibleById.get(rel.id))
      .filter(Boolean)
      .sort(nodeSort);
    const calleeIds = new Set(callees.map((fun) => fun.id));
    callers = callers.filter((fun) => !calleeIds.has(fun.id));
    const selectedNode = {
      ...selected,
      kind: "function",
      lens: true,
      title: selected.label,
      subtitle: selected.module,
      width: 300,
      height: 42
    };
    const callerNodes = callers.map((fun) => ({
      ...fun,
      kind: "function",
      lens: true,
      title: fun.label,
      subtitle: fun.module,
      width: 280,
      height: 34
    }));
    const calleeNodes = callees.map((fun) => ({
      ...fun,
      kind: "function",
      lens: true,
      title: fun.label,
      subtitle: fun.module,
      width: 280,
      height: 34
    }));
    const maxRows = Math.max(callerNodes.length, calleeNodes.length, 1);
    const stackHeight = maxRows * 40;
    const centerY = 68 + Math.max(0, (stackHeight - selectedNode.height) / 2);
    const nodes = [
      ...placeFunctionStack(callerNodes, 40, 68),
      {
        ...selectedNode,
        x: 390,
        y: centerY,
        cx: 390 + selectedNode.width / 2,
        cy: centerY + selectedNode.height / 2
      },
      ...placeFunctionStack(calleeNodes, 760, 68)
    ];
    const nodeById = new Map(nodes.map((node) => [node.id, node]));
    const ids = new Set(nodes.map((node) => node.id));
    const edges = filterLayoutEdges(visible.edges)
      .filter((edge) => ids.has(edge.source) && ids.has(edge.target))
      .filter((edge) => edge.source === selected.id || edge.target === selected.id)
      .map((edge) => ({
        ...edge,
        sourceNode: nodeById.get(edge.source),
        targetNode: nodeById.get(edge.target)
      }))
      .filter((edge) => edge.sourceNode && edge.targetNode);
    const height = Math.max(540, stackHeight + 116);
    return {
      nodes,
      modules: [],
      edges,
      lens: true,
      bands: [
        { id: "callers", x: 20, y: 16, width: 320, height, label: "Callers" },
        { id: "selected", x: 370, y: 16, width: 340, height, label: selected.module },
        { id: "callees", x: 740, y: 16, width: 320, height, label: "Callees" }
      ],
      bounds: { x: 0, y: 0, width: 1080, height: height + 24 }
    };
  }

  function placeFunctionStack(nodes, x, y) {
    return nodes.map((node, idx) => ({
      ...node,
      x,
      y: y + idx * 40,
      cx: x + node.width / 2,
      cy: y + idx * 40 + node.height / 2
    }));
  }

  function systemLayout(visible) {
    const nodes = systemGraphNodes(visible);
    const positioned = positionSystemNodes(nodes);
    const nodeById = new Map(positioned.nodes.map((node) => [node.id, node]));
    const edges = filterLayoutEdges(systemEdges(visible.edges))
      .map((edge) => {
        const source = nodeById.get(edge.source);
        const target = nodeById.get(edge.target);
        if (!source || !target) return null;
        return { ...edge, sourceNode: source, targetNode: target };
      })
      .filter(Boolean);
    return { ...positioned, edges };
  }

  function systemGraphNodes(visible) {
    const grouped = new Map();
    visible.modules.forEach((mod) => {
      const id = systemId(mod);
      if (!grouped.has(id)) {
        grouped.set(id, {
          id,
          kind: "system",
          role: mod.role,
          group: mod.group,
          title: systemLabel(mod),
          modules: 0,
          functions: 0,
          exports: 0,
          loc: 0,
          deviceCount: 0,
          moduleIds: [],
          "device-refs": new Set(),
          width: 270,
          height: 76
        });
      }
      const node = grouped.get(id);
      node.modules += 1;
      node.functions += mod.functions;
      node.exports += mod.exports;
      node.loc += mod.loc;
      node.moduleIds.push(mod.id);
      if (mod.role === "device") node.deviceCount += 1;
      (mod["device-refs"] || []).forEach((ref) => node["device-refs"].add(ref));
    });
    return [...grouped.values()].map((node) => ({
      ...node,
      "device-refs": [...node["device-refs"]].sort(),
      subtitle: `${nf.format(node.modules)} modules · ${nf.format(node.functions)} functions`
    }));
  }

  function systemEdges(edges) {
    const merged = new Map();
    edges.forEach((edge) => {
      const sourceModule = byModule.get(edge["source-module"]);
      const targetModule = byModule.get(edge["target-module"]);
      if (!sourceModule || !targetModule) return;
      const source = systemId(sourceModule);
      const target = systemId(targetModule);
      if (source === target) return;
      const id = `${source}->${target}`;
      const existing = merged.get(id);
      if (existing) {
        existing.count += edge.count;
      } else {
        merged.set(id, { id, source, target, count: edge.count });
      }
    });
    return [...merged.values()];
  }

  function systemId(mod) {
    return `${mod.role}:${mod.group}`;
  }

  function systemLabel(mod) {
    if (mod.category) return mod.category;
    if (mod.role === "kernel") return mod.group === "device" ? "kernel/device-runtime" : `kernel/${mod.group}`;
    if (mod.role === "device") return `devices/${mod.group}`;
    return `${mod.role}/${mod.group}`;
  }

  function functionGraphNodes(visible) {
    return visible.functions.map((fun) => ({
      ...fun,
      kind: "function",
      title: fun.label,
      subtitle: functionSubtitle(fun),
      width: mapLayoutModeActive() ? 250 : 250,
      height: mapLayoutModeActive() ? 36 : 24
    }));
  }

  function functionSubtitle(fun) {
    if (namespaceLayoutActive() && fun.namespace) {
      return `${fun.module} · ${fun.namespace}`;
    }
    if (mapLayoutModeActive()) {
      return fun.namespace ? `${fun.module} · ${fun.namespace}` : fun.module;
    }
    return fun.module;
  }

  function moduleGraphNodes(visible) {
    return visible.modules.map((mod) => ({
      ...mod,
      kind: "module",
      title: mod.module,
      subtitle: namespaceLayoutActive() && mod.namespace ?
        `${mod.functions} functions · ${mod.namespace}` :
        `${mod.functions} functions`,
      width: mapLayoutModeActive() ? 260 : 270,
      height: mapLayoutModeActive() ? 44 : 44
    }));
  }

  function positionSystemNodes(nodes) {
    const roles = ["device", "kernel", "forge", "other"];
    const roleLabels = {
      device: "Loaded devices",
      kernel: "HyperBEAM kernel",
      forge: "Forge tooling",
      other: "Other"
    };
    const byRole = new Map(
      groupBy(nodes, (node) => node.role).map((items) => [items[0].role, items])
    );
    const placed = [];
    const bands = [];
    const laneStep = 300;
    const roleGap = 22;
    let xCursor = 36;
    let maxY = 0;
    roles.forEach((role) => {
      const columnNodes = (byRole.get(role) || [])
        .sort((a, b) => a.group.localeCompare(b.group));
      if (!columnNodes.length) return;
      const lanes = Math.min(3, Math.max(1, Math.ceil(columnNodes.length / 5)));
      const rows = Math.ceil(columnNodes.length / lanes);
      columnNodes.forEach((node, idx) => {
        const lane = Math.floor(idx / rows);
        const row = idx % rows;
        const x = xCursor + lane * laneStep;
        const y = 68 + row * (node.height + 18);
        placed.push({
          ...node,
          x,
          y,
          cx: x + node.width / 2,
          cy: y + node.height / 2
        });
      });
      const width = (lanes - 1) * laneStep + columnNodes[0].width + 32;
      const height = Math.max(500, 68 + rows * (columnNodes[0].height + 18) + 20);
      bands.push({
        id: role,
        x: xCursor - 16,
        y: 16,
        width,
        height,
        label: roleLabels[role] || role,
        nodeIds: columnNodes.map((node) => node.id)
      });
      maxY = Math.max(maxY, height + 24);
      xCursor += width + roleGap;
    });
    const maxX = placed.reduce((max, node) => Math.max(max, node.x + node.width), 0);
    return {
      nodes: placed,
      modules: [],
      bands,
      bounds: {
        x: 0,
        y: 0,
        width: Math.max(760, maxX + 52),
        height: Math.max(540, maxY)
      }
    };
  }

  function moduleEdges(edges) {
    const merged = new Map();
    edges.forEach((edge) => {
      if (edge["source-module"] === edge["target-module"]) return;
      const id = `${edge["source-module"]}->${edge["target-module"]}`;
      const existing = merged.get(id);
      if (existing) {
        existing.count += edge.count;
      } else {
        merged.set(id, {
          id,
          source: edge["source-module"],
          target: edge["target-module"],
          count: edge.count,
          lines: edge.lines
        });
      }
    });
    return [...merged.values()];
  }

  function positionNodes(nodes) {
    const columnMap = new Map();
    nodes.forEach((node) => {
      const column = columnKey(node);
      if (!columnMap.has(column)) columnMap.set(column, []);
      columnMap.get(column).push(node);
    });
    const columns = [...columnMap.keys()].sort(columnSort);
    const bands = [];
    const placed = [];
    let maxY = 0;
    columns.forEach((column, columnIdx) => {
      const x = 32 + columnIdx * 330;
      let y = 58;
      const columnNodes = columnMap.get(column).sort(nodeSort);
      const moduleGroups = groupBy(columnNodes, (node) => node.kind === "function" ? node.module : node.id);
      moduleGroups.forEach((moduleNodes) => {
        const moduleId = moduleNodes[0].kind === "function" ? moduleNodes[0].module : moduleNodes[0].id;
        const module = byModule.get(moduleId) || moduleNodes[0];
        const frameHeight = 38 + moduleNodes.length * (moduleNodes[0].height + 5);
        bands.push({
          id: moduleId,
          role: module.role,
          x: x - 12,
          y: y - 32,
          width: 292,
          height: frameHeight,
          title: moduleId,
          subtitle: module.group,
          nodeIds: moduleNodes.map((node) => node.id)
        });
        moduleNodes.forEach((node, idx) => {
          placed.push({
            ...node,
            x,
            y: y + idx * (node.height + 5),
            cx: x + node.width / 2,
            cy: y + idx * (node.height + 5) + node.height / 2
          });
        });
        y += frameHeight + 18;
      });
      maxY = Math.max(maxY, y);
    });
    const bounds = {
      x: 0,
      y: 0,
      width: Math.max(680, columns.length * 330 + 40),
      height: Math.max(520, maxY + 40)
    };
    return { nodes: placed, modules: bands, bands: columnBands(columns, maxY, columnMap), bounds };
  }

  function positionFunctionNodes(nodes, edges) {
    const columnMap = new Map();
    nodes.forEach((node) => {
      const column = columnKey(node);
      if (!columnMap.has(column)) columnMap.set(column, []);
      columnMap.get(column).push(node);
    });
    const columns = [...columnMap.keys()].sort(columnSort);
    const planned = columns.map((column) => {
      const columnNodes = columnMap.get(column).sort(nodeSort);
      const moduleGroups = groupBy(columnNodes, (node) => node.module);
      const groups = moduleGroups.map((moduleNodes) => functionModuleFlow(moduleNodes, edges));
      const width = Math.max(292, ...groups.map((group) => group.width));
      return { column, groups, width };
    });
    const placed = [];
    const modules = [];
    const bands = [];
    let x = 32;
    let maxY = 0;
    planned.forEach((column) => {
      let y = 58;
      column.groups.forEach((group) => {
        modules.push({
          ...group.frame,
          x: x - 12,
          y: y - 32,
          width: Math.max(group.width, column.width),
          height: group.height
        });
        group.nodes.forEach((node) => {
          placed.push({
            ...node,
            x: x + node.flowX,
            y: y + node.flowY,
            cx: x + node.flowX + node.width / 2,
            cy: y + node.flowY + node.height / 2
          });
        });
        y += group.height + 18;
      });
      const bandHeight = Math.max(500, y + 8);
      bands.push({
        x: x - 20,
        y: 12,
        width: column.width + 24,
        height: bandHeight,
        label: columnLabel(column.column),
        nodeIds: column.groups.flatMap((group) => group.nodes.map((node) => node.id))
      });
      maxY = Math.max(maxY, bandHeight);
      x += column.width + 42;
    });
    return {
      nodes: placed,
      modules,
      bands,
      bounds: {
        x: 0,
        y: 0,
        width: Math.max(760, x + 20),
        height: Math.max(520, maxY + 24)
      }
    };
  }

  function forceMapActive(nodes) {
    return mapLayoutModeActive() && state.mode !== "system" && nodes.length > 1;
  }

  function positionForceMapNodes(nodes, rawEdges) {
    const edgeInput = filterLayoutEdges(rawEdges)
      .filter((edge) => edge.source !== edge.target);
    const ids = new Set(nodes.map((node) => node.id));
    const links = edgeInput.filter((edge) => ids.has(edge.source) && ids.has(edge.target));
    const anchors = forceMapAnchors(nodes);
    const degree = new Map(nodes.map((node) => [node.id, 0]));
    links.forEach((edge) => {
      degree.set(edge.source, (degree.get(edge.source) || 0) + 1);
      degree.set(edge.target, (degree.get(edge.target) || 0) + 1);
    });
    const simNodes = nodes.map((node, idx) => {
      const anchor = anchors.nodeAnchors.get(node.id) || { x: 0, y: 0 };
      const angle = idx * 2.399963 + hashUnit(node.id) * Math.PI;
      const radius = 18 + Math.sqrt(idx + 1) * 11 + hashUnit(`${node.id}:r`) * 28;
      return {
        node,
        id: node.id,
        x: anchor.x + Math.cos(angle) * radius,
        y: anchor.y + Math.sin(angle) * radius,
        vx: 0,
        vy: 0,
        width: node.width,
        height: node.height,
        radius: Math.max(node.width, node.height) / 2,
        anchor,
        degree: degree.get(node.id) || 0
      };
    });
    const simById = new Map(simNodes.map((node) => [node.id, node]));
    const simLinks = links.map((edge) => ({
      edge,
      source: simById.get(edge.source),
      target: simById.get(edge.target),
      distance: forceLinkDistance(edge),
      strength: forceLinkStrength(edge)
    })).filter((link) => link.source && link.target);
    const iterations = forceIterationCount(simNodes.length);
    for (let iter = 0; iter < iterations; iter += 1) {
      const progress = iter / Math.max(1, iterations - 1);
      const alpha = 0.92 * Math.pow(1 - progress, 1.65) + 0.025;
      applyForceLinks(simLinks, alpha);
      applyForceCharge(simNodes, alpha);
      applyForceAnchors(simNodes, alpha);
      applyForceCollisions(simNodes, alpha);
      applyForceCenter(simNodes, alpha);
      simNodes.forEach((node) => {
        node.vx = clamp(node.vx, -72, 72) * 0.72;
        node.vy = clamp(node.vy, -72, 72) * 0.72;
        node.x += node.vx;
        node.y += node.vy;
      });
    }
    relaxForceCollisions(simNodes, forceRelaxPasses(simNodes.length));
    const placed = normalizeForceNodes(simNodes);
    return {
      nodes: placed,
      modules: forceMapRegions(placed),
      bands: forceMapBands(placed),
      bounds: forceBounds(placed, 92),
      force: true
    };
  }

  function forceMapAnchors(nodes) {
    const largeFunctionMap = state.mode === "function" && nodes.length > 360;
    const groups = groupBy(nodes, forceGroupKey)
      .map((items) => ({
        id: forceGroupKey(items[0]),
        role: items[0].role || "other",
        group: items[0].group || "other",
        items: items.slice().sort(nodeSort)
      }))
      .sort((a, b) => {
        const roleDelta = forceRoleRank(a.role) - forceRoleRank(b.role);
        if (roleDelta !== 0) return roleDelta;
        return `${a.group}:${a.id}`.localeCompare(`${b.group}:${b.id}`);
      });
    const byRole = new Map(groupBy(groups, (group) => group.role).map((items) => [items[0].role, items]));
    const roles = ["device", "kernel", "forge", "other"].filter((role) => byRole.has(role));
    const groupAnchors = new Map();
    let xCursor = 0;
    roles.forEach((role) => {
      const roleGroups = byRole.get(role);
      const cols = Math.max(1, Math.ceil(Math.sqrt(roleGroups.length * 1.25)));
      const rows = Math.max(1, Math.ceil(roleGroups.length / cols));
      const xStep = namespaceLayoutActive() ?
        state.mode === "function" ? largeFunctionMap ? 900 : 580 : 440 :
        state.mode === "function" ? largeFunctionMap ? 660 : 430 : 360;
      const yStep = namespaceLayoutActive() ?
        state.mode === "function" ? largeFunctionMap ? 650 : 410 : 320 :
        state.mode === "function" ? largeFunctionMap ? 460 : 300 : 250;
      roleGroups.forEach((group, idx) => {
        const col = idx % cols;
        const row = Math.floor(idx / cols);
        const x = xCursor + col * xStep + hashOffset(`${group.id}:x`, 40);
        const y = (row - (rows - 1) / 2) * yStep + hashOffset(`${group.id}:y`, 46);
        groupAnchors.set(group.id, { x, y, role, group: group.group });
      });
      xCursor += Math.max(1, cols) * xStep +
        (namespaceLayoutActive() ? 660 : largeFunctionMap ? 780 : 520);
    });
    const nodeAnchors = new Map();
    groups.forEach((group) => {
      const anchor = groupAnchors.get(group.id) || { x: 0, y: 0 };
      group.items.forEach((node, idx) => {
        const angle = idx * 2.399963 + hashUnit(`${node.id}:ga`) * Math.PI;
        const spread = state.mode === "function" ? largeFunctionMap ? 142 : 54 : 24;
        const radius = Math.sqrt(idx + 0.35) * spread;
        nodeAnchors.set(node.id, {
          x: anchor.x + Math.cos(angle) * radius,
          y: anchor.y + Math.sin(angle) * radius,
          role: anchor.role,
          group: anchor.group
        });
      });
    });
    return { groupAnchors, nodeAnchors };
  }

  function forceGroupKey(node) {
    if (state.mode === "function") {
      if (namespaceLayoutActive()) return `${node.namespace || node.group}:${node.module}`;
      return node.module;
    }
    if (namespaceLayoutActive()) return node.namespace || `${node.role || "other"}:${node.group || "other"}`;
    return `${node.role || "other"}:${node.group || "other"}`;
  }

  function forceRoleRank(role) {
    if (role === "device") return 0;
    if (role === "kernel") return 1;
    if (role === "forge") return 2;
    return 3;
  }

  function forceLinkDistance(edge) {
    const sameModule = edge["source-module"] && edge["source-module"] === edge["target-module"];
    const base = namespaceLayoutActive() ?
      state.mode === "function" ?
        sameModule ? 210 : 470 :
        380 :
      state.mode === "function" ?
        sameModule ? 172 : 360 :
        310;
    return Math.max(120, base - Math.min(95, Math.log1p(edge.count || 1) * 20));
  }

  function forceLinkStrength(edge) {
    const sameModule = edge["source-module"] && edge["source-module"] === edge["target-module"];
    const base = state.mode === "function" && sameModule ? 0.052 : 0.034;
    const objectiveScale = namespaceLayoutActive() ? 0.62 : 1;
    return (base + Math.min(0.035, Math.log1p(edge.count || 1) * 0.006)) *
      objectiveScale;
  }

  function forceIterationCount(count) {
    if (count > 2600) return 34;
    if (count > 1500) return 52;
    if (count > 900) return 120;
    if (count > 520) return 210;
    if (count > 260) return 270;
    return 430;
  }

  function forceRelaxPasses(count) {
    if (count > 2600) return 20;
    if (count > 1500) return 32;
    if (count > 900) return 48;
    if (count > 520) return 78;
    if (count > 260) return 120;
    return 90;
  }

  function applyForceLinks(links, alpha) {
    links.forEach((link) => {
      const dx = link.target.x - link.source.x || 0.001;
      const dy = link.target.y - link.source.y || 0.001;
      const distance = Math.sqrt(dx * dx + dy * dy);
      const force = ((distance - link.distance) / distance) * link.strength * alpha;
      const weight = 0.5;
      const fx = dx * force;
      const fy = dy * force;
      link.source.vx += fx * weight;
      link.source.vy += fy * weight;
      link.target.vx -= fx * weight;
      link.target.vy -= fy * weight;
    });
  }

  function applyForceCharge(nodes, alpha) {
    const cellSize = 520;
    const grid = forceGrid(nodes, cellSize);
    const seen = new Set();
    nodes.forEach((node) => {
      forceGridNeighbors(grid, node, cellSize, 1).forEach((other) => {
        if (node === other) return;
        const key = node.id < other.id ? `${node.id}|${other.id}` : `${other.id}|${node.id}`;
        if (seen.has(key)) return;
        seen.add(key);
        const dx = node.x - other.x || hashOffset(key, 0.01);
        const dy = node.y - other.y || hashOffset(`${key}:y`, 0.01);
        const distanceSq = dx * dx + dy * dy;
        if (distanceSq > 900000) return;
        const distance = Math.sqrt(distanceSq);
        const desired = node.radius + other.radius + 180;
        const force = (desired * desired / Math.max(2200, distanceSq)) * 1.7 * alpha;
        const fx = (dx / distance) * force;
        const fy = (dy / distance) * force;
        node.vx += fx;
        node.vy += fy;
        other.vx -= fx;
        other.vy -= fy;
      });
    });
  }

  function applyForceAnchors(nodes, alpha) {
    nodes.forEach((node) => {
      const strength = namespaceLayoutActive() ?
        state.mode === "function" ? 0.03 : 0.034 :
        state.mode === "function" ? 0.012 : 0.018;
      node.vx += (node.anchor.x - node.x) * strength * alpha;
      node.vy += (node.anchor.y - node.y) * strength * alpha;
      if (state.selected && node.id === state.selected) {
        node.vx += (0 - node.x) * 0.0025 * alpha;
        node.vy += (0 - node.y) * 0.0025 * alpha;
      }
    });
  }

  function applyForceCollisions(nodes, alpha) {
    const cellSize = 460;
    const grid = forceGrid(nodes, cellSize);
    const seen = new Set();
    nodes.forEach((node) => {
      forceGridNeighbors(grid, node, cellSize, 1).forEach((other) => {
        if (node === other) return;
        const key = node.id < other.id ? `${node.id}|${other.id}` : `${other.id}|${node.id}`;
        if (seen.has(key)) return;
        seen.add(key);
        const dx = node.x - other.x || hashOffset(key, 0.01);
        const dy = node.y - other.y || hashOffset(`${key}:y`, 0.01);
        const overlapX = (node.width + other.width) / 2 + 42 - Math.abs(dx);
        const overlapY = (node.height + other.height) / 2 + 28 - Math.abs(dy);
        if (overlapX <= 0 || overlapY <= 0) return;
        if (overlapX < overlapY) {
          const sign = dx < 0 ? -1 : 1;
          const push = overlapX * 0.92 * alpha;
          node.vx += sign * push;
          other.vx -= sign * push;
        } else {
          const sign = dy < 0 ? -1 : 1;
          const push = overlapY * 1.02 * alpha;
          node.vy += sign * push;
          other.vy -= sign * push;
        }
      });
    });
  }

  function relaxForceCollisions(nodes, passes) {
    for (let pass = 0; pass < passes; pass += 1) {
      const cellSize = 460;
      const grid = forceGrid(nodes, cellSize);
      const seen = new Set();
      let moved = false;
      nodes.forEach((node) => {
        forceGridNeighbors(grid, node, cellSize, 1).forEach((other) => {
          if (node === other) return;
          const key = node.id < other.id ? `${node.id}|${other.id}` : `${other.id}|${node.id}`;
          if (seen.has(key)) return;
          seen.add(key);
          const dx = node.x - other.x || hashOffset(key, 0.01);
          const dy = node.y - other.y || hashOffset(`${key}:y`, 0.01);
          const overlapX = (node.width + other.width) / 2 + 18 - Math.abs(dx);
          const overlapY = (node.height + other.height) / 2 + 12 - Math.abs(dy);
          if (overlapX <= 0 || overlapY <= 0) return;
          moved = true;
          if (overlapX < overlapY) {
            const sign = dx < 0 ? -1 : 1;
            const push = overlapX / 2 + 0.8;
            node.x += sign * push;
            other.x -= sign * push;
          } else {
            const sign = dy < 0 ? -1 : 1;
            const push = overlapY / 2 + 0.8;
            node.y += sign * push;
            other.y -= sign * push;
          }
        });
      });
      if (!moved) return;
    }
  }

  function applyForceCenter(nodes, alpha) {
    if (!nodes.length) return;
    const cx = nodes.reduce((sum, node) => sum + node.x, 0) / nodes.length;
    const cy = nodes.reduce((sum, node) => sum + node.y, 0) / nodes.length;
    nodes.forEach((node) => {
      node.vx -= cx * 0.002 * alpha;
      node.vy -= cy * 0.002 * alpha;
    });
  }

  function forceGrid(nodes, cellSize) {
    const grid = new Map();
    nodes.forEach((node) => {
      const key = forceGridKey(node.x, node.y, cellSize);
      if (!grid.has(key)) grid.set(key, []);
      grid.get(key).push(node);
    });
    return grid;
  }

  function forceGridNeighbors(grid, node, cellSize, radius) {
    const [cx, cy] = forceGridKey(node.x, node.y, cellSize).split(":").map(Number);
    const neighbors = [];
    for (let x = cx - radius; x <= cx + radius; x += 1) {
      for (let y = cy - radius; y <= cy + radius; y += 1) {
        const items = grid.get(`${x}:${y}`);
        if (items) neighbors.push(...items);
      }
    }
    return neighbors;
  }

  function forceGridKey(x, y, cellSize) {
    return `${Math.floor(x / cellSize)}:${Math.floor(y / cellSize)}`;
  }

  function normalizeForceNodes(simNodes) {
    const raw = simNodes.map((sim) => ({
      ...sim.node,
      x: sim.x - sim.width / 2,
      y: sim.y - sim.height / 2,
      cx: sim.x,
      cy: sim.y
    }));
    const bounds = forceBounds(raw, 92);
    const shiftX = -bounds.x;
    const shiftY = -bounds.y;
    return raw.map((node) => ({
      ...node,
      x: node.x + shiftX,
      y: node.y + shiftY,
      cx: node.cx + shiftX,
      cy: node.cy + shiftY
    }));
  }

  function forceBounds(nodes, padding) {
    if (!nodes.length) return { x: 0, y: 0, width: 760, height: 520 };
    const minX = Math.min(...nodes.map((node) => node.x)) - padding;
    const minY = Math.min(...nodes.map((node) => node.y)) - padding;
    const maxX = Math.max(...nodes.map((node) => node.x + node.width)) + padding;
    const maxY = Math.max(...nodes.map((node) => node.y + node.height)) + padding;
    return {
      x: minX,
      y: minY,
      width: Math.max(760, maxX - minX),
      height: Math.max(520, maxY - minY)
    };
  }

  function forceMapRegions(nodes) {
    const groups = groupBy(nodes, forceRegionKey);
    return groups
      .filter((items) => items.length > 1)
      .map((items) => {
        const first = items[0];
        const padX = state.mode === "function" ? 42 : 34;
        const padY = state.mode === "function" ? 34 : 28;
        const minX = Math.min(...items.map((node) => node.x)) - padX;
        const minY = Math.min(...items.map((node) => node.y)) - padY;
        const maxX = Math.max(...items.map((node) => node.x + node.width)) + padX;
        const maxY = Math.max(...items.map((node) => node.y + node.height)) + padY;
        return {
          id: forceRegionKey(first),
          role: first.role,
          x: minX,
          y: minY,
          width: maxX - minX,
          height: maxY - minY,
          title: forceRegionTitle(first),
          subtitle: first.namespace || first.group,
          nodeIds: items.map((node) => node.id),
          map: true
        };
      });
  }

  function forceRegionKey(node) {
    if (state.mode === "function") return node.module;
    if (namespaceLayoutActive()) return node.namespace || `${node.role}:${node.group}`;
    return `${node.role}:${node.group}`;
  }

  function forceRegionTitle(node) {
    if (state.mode === "function") return node.module;
    if (namespaceLayoutActive()) return node.namespace || columnLabel(columnKey(node));
    return columnLabel(columnKey(node));
  }

  function forceMapBands(nodes) {
    if (!namespaceLayoutActive()) {
      return forceBandsForGroups(
        groupBy(nodes, (node) => node.role || "other"),
        (items) => forceBandLabel(items[0], items.length)
      );
    }
    const roleBands = forceBandsForGroups(
      groupBy(nodes, (node) => `role:${node.role || "other"}`),
      (items) => roleBandLabel(items[0], items.length),
      98
    );
    const namespaceBands = forceBandsForGroups(
      groupBy(nodes, (node) => node.namespace || `${node.role || "other"}:${node.group || "other"}`),
      (items) => namespaceBandLabel(items[0], items.length),
      58
    );
    return [...roleBands, ...namespaceBands];
  }

  function forceBandsForGroups(groups, labelFun, padding = 78) {
    return groups
      .filter((items) => items.length > 1)
      .map((items) => {
        const minX = Math.min(...items.map((node) => node.x)) - padding;
        const minY = Math.min(...items.map((node) => node.y)) - Math.max(48, padding - 10);
        const maxX = Math.max(...items.map((node) => node.x + node.width)) + padding;
        const maxY = Math.max(...items.map((node) => node.y + node.height)) + Math.max(48, padding - 10);
        return {
          id: labelFun(items),
          x: minX,
          y: minY,
          width: maxX - minX,
          height: maxY - minY,
          label: labelFun(items),
          nodeIds: items.map((node) => node.id)
        };
      });
  }

  function forceBandLabel(node, count) {
    const role = node.role || "other";
    if (role === "device") return `loaded devices · ${count} nodes`;
    if (role === "kernel") return `kernel subsystems · ${count} nodes`;
    return `${role} · ${count} nodes`;
  }

  function roleBandLabel(node, count) {
    const role = node.role || "other";
    if (role === "device") return `devices · ${count} nodes`;
    if (role === "kernel") return `kernel · ${count} nodes`;
    return `${role} · ${count} nodes`;
  }

  function namespaceBandLabel(node, count) {
    const label = node.namespace || node.category || node.group || "namespace";
    const noun = state.mode === "function" ? "functions" : "modules";
    return `${label} · ${count} ${noun}`;
  }

  function hashNumber(value) {
    let hash = 2166136261;
    const text = String(value || "");
    for (let idx = 0; idx < text.length; idx += 1) {
      hash ^= text.charCodeAt(idx);
      hash = Math.imul(hash, 16777619);
    }
    return hash >>> 0;
  }

  function hashUnit(value) {
    return hashNumber(value) / 4294967295;
  }

  function hashOffset(value, spread) {
    return (hashUnit(value) - 0.5) * spread * 2;
  }

  function functionModuleFlow(moduleNodes, edges) {
    const ids = new Set(moduleNodes.map((node) => node.id));
    const moduleId = moduleNodes[0].module;
    const module = byModule.get(moduleId) || moduleNodes[0];
    const ranks = functionFlowRanks(moduleNodes, edges, ids);
    const maxRank = Math.max(0, ...ranks.values());
    const laneStep = 286;
    const rowStep = 34;
    const lanes = Array.from({ length: maxRank + 1 }, () => []);
    moduleNodes
      .slice()
      .sort(nodeSort)
      .forEach((node) => lanes[ranks.get(node.id) || 0].push(node));
    const laidOut = [];
    lanes.forEach((lane, rank) => {
      lane.forEach((node, row) => {
        laidOut.push({
          ...node,
          flowRank: rank,
          flowX: rank * laneStep,
          flowY: row * rowStep
        });
      });
    });
    const rows = Math.max(1, ...lanes.map((lane) => lane.length));
    const width = maxRank * laneStep + 286;
    const height = Math.max(78, 38 + rows * rowStep);
    return {
      nodes: laidOut,
      width,
      height,
      frame: {
        id: moduleId,
        role: module.role,
        title: moduleId,
        subtitle: module.group,
        nodeIds: moduleNodes.map((node) => node.id)
      }
    };
  }

  function functionFlowRanks(moduleNodes, edges, ids) {
    const incomingInternal = new Map(moduleNodes.map((node) => [node.id, 0]));
    const outgoingInternal = new Map(moduleNodes.map((node) => [node.id, []]));
    const incomingExternal = new Set();
    edges.forEach((edge) => {
      const sourceInside = ids.has(edge.source);
      const targetInside = ids.has(edge.target);
      if (sourceInside && targetInside && edge.source !== edge.target) {
        incomingInternal.set(edge.target, (incomingInternal.get(edge.target) || 0) + 1);
        outgoingInternal.get(edge.source).push(edge.target);
      } else if (!sourceInside && targetInside) {
        incomingExternal.add(edge.target);
      }
    });
    let roots = moduleNodes
      .filter((node) => !incomingInternal.get(node.id) || incomingExternal.has(node.id))
      .map((node) => node.id);
    if (!roots.length) {
      roots = moduleNodes
        .filter((node) => node.exported)
        .map((node) => node.id);
    }
    if (!roots.length) roots = [moduleNodes.slice().sort(nodeSort)[0].id];
    const maxRank = Math.min(7, Math.max(2, Math.ceil(Math.sqrt(moduleNodes.length))));
    const ranks = new Map(moduleNodes.map((node) => [node.id, maxRank]));
    const queue = roots.map((id) => ({ id, rank: 0 }));
    roots.forEach((id) => ranks.set(id, 0));
    while (queue.length) {
      const { id, rank } = queue.shift();
      (outgoingInternal.get(id) || []).forEach((target) => {
        const nextRank = Math.min(maxRank, rank + 1);
        const currentRank = ranks.has(target) ? ranks.get(target) : maxRank;
        if (nextRank < currentRank) {
          ranks.set(target, nextRank);
          queue.push({ id: target, rank: nextRank });
        }
      });
    }
    moduleNodes.forEach((node) => {
      if (ranks.get(node.id) === maxRank && !incomingInternal.get(node.id)) {
        ranks.set(node.id, 0);
      }
    });
    return ranks;
  }

  function groupBy(items, keyFun) {
    const grouped = new Map();
    items.forEach((item) => {
      const key = keyFun(item);
      if (!grouped.has(key)) grouped.set(key, []);
      grouped.get(key).push(item);
    });
    return [...grouped.values()];
  }

  function columnKey(node) {
    if (node.role === "device") return `0:device:${node.group}`;
    if (node.role === "kernel") return `1:kernel:${node.group}`;
    if (node.role === "forge") return `2:forge:${node.group}`;
    return `3:${node.role}:${node.group}`;
  }

  function columnSort(a, b) {
    return a.localeCompare(b);
  }

  function nodeSort(a, b) {
    return `${a.module || a.id}:${a.line || 0}:${a.label || a.id}`
      .localeCompare(`${b.module || b.id}:${b.line || 0}:${b.label || b.id}`);
  }

  function columnBands(columns, maxY, columnMap) {
    return columns.map((column, idx) => ({
      x: 12 + idx * 330,
      y: 12,
      width: 306,
      height: Math.max(500, maxY),
      label: columnLabel(column),
      nodeIds: (columnMap.get(column) || []).map((node) => node.id)
    }));
  }

  function columnLabel(column) {
    const parts = column.split(":");
    if (parts[1] === "device") return `devices/${parts[2]}`;
    if (parts[1] === "kernel") return parts[2] === "device" ? "kernel/device-runtime" : `kernel/${parts[2]}`;
    return `${parts[1]}/${parts[2]}`;
  }

  function startLive(endpoint) {
    const follow = state.live.follow;
    const intervalMs = intervalParamValue(els.liveInterval.value || state.live.intervalMs);
    stopLive({ renderAfter: false, clearFollow: false });
    const normalized = liveParamValue(endpoint);
    state.live.enabled = true;
    state.detailTab = "engine";
    state.live.mode = liveModeForEndpoint(normalized);
    state.live.endpoint = normalized;
    state.live.intervalMs = intervalMs;
    state.live.follow = follow;
    state.live.previous = new Map();
    state.live.activity = new Map();
    state.live.errors = new Map();
    state.live.samples = new Map();
    state.live.traceEdges = new Map();
    state.live.eventDeltas = new Map();
    state.live.eventRates = new Map();
    state.live.previousRates = new Map();
    state.live.rateChanges = new Map();
    state.live.rateActivity = new Map();
    state.live.eventHistory = new Map();
    state.live.eventTick = 0;
    state.live.lastCounterAt = 0;
    state.live.totalDelta = 0;
    state.live.processCount = 0;
    state.live.processSamples = [];
    state.live.frameCount = 0;
    state.live.sourceName = "";
    state.live.recordingUrl = "";
    state.live.recordingEvents = [];
    state.live.recordingFocus = -1;
    state.live.pendingRecordingFocus = -1;
    state.live.lastSeen = 0;
    state.live.lastPollStarted = 0;
    state.live.lastError = "";
    state.live.demoTick = 0;
    setLiveIntervalSelect(state.live.intervalMs);
    if (normalized !== "demo") els.liveEndpoint.value = normalized;
    if (state.live.mode === "demo") {
      demoLiveTick();
      state.live.timer = window.setInterval(demoLiveTick, state.live.intervalMs);
    } else {
      pollLive();
      state.live.timer = window.setInterval(pollLive, state.live.intervalMs);
    }
    renderLiveControls();
    syncUrl();
    renderLiveFrame();
  }

  function liveModeForEndpoint(endpoint) {
    if (endpoint === "demo") return "demo";
    if (endpoint === defaultStackEndpoint || endpoint.includes("~recorder@1.0/live")) {
      return "stack";
    }
    return "events";
  }

  function stopLive(options = {}) {
    if (state.live.timer) window.clearInterval(state.live.timer);
    stopRecordingPlayback(false);
    state.live.timer = null;
    state.live.enabled = false;
    state.live.mode = "off";
    state.live.endpoint = "";
    if (options.clearFollow !== false) state.live.follow = false;
    state.live.previous = new Map();
    state.live.activity = new Map();
    state.live.errors = new Map();
    state.live.samples = new Map();
    state.live.traceEdges = new Map();
    state.live.eventDeltas = new Map();
    state.live.eventRates = new Map();
    state.live.previousRates = new Map();
    state.live.rateChanges = new Map();
    state.live.rateActivity = new Map();
    state.live.eventHistory = new Map();
    state.live.eventTick = 0;
    state.live.lastCounterAt = 0;
    state.live.totalDelta = 0;
    state.live.processCount = 0;
    state.live.processSamples = [];
    state.live.frameCount = 0;
    state.live.sourceName = "";
    state.live.recordingUrl = "";
    state.live.recordingEvents = [];
    state.live.recordingFocus = -1;
    state.live.recordingPlaying = false;
    state.live.recordingTimer = null;
    state.live.pendingRecordingFocus = -1;
    state.live.lastSeen = 0;
    state.live.lastPollStarted = 0;
    state.live.lastError = "";
    renderLiveControls();
    if (options.renderAfter !== false) render();
  }

  async function pollLive() {
    if (!state.live.enabled || !["events", "stack"].includes(state.live.mode)) return;
    state.live.lastPollStarted = Date.now();
    state.live.eventTick += 1;
    decayLiveActivity();
    try {
      const response = await fetch(state.live.endpoint, {
        cache: "no-store",
        headers: { accept: "application/json, text/plain;q=0.9, */*;q=0.8" }
      });
      const text = await response.text();
      if (!response.ok) throw new Error(`${response.status} ${response.statusText}`);
      const headerPayload = responseHeaderPayload(response.headers);
      let payload = parseLivePayload(text);
      if (
        linkifiedCounterPayload(payload) ||
        (
          !processPayload(payload) &&
          !payloadHasCounters(payload) &&
          linkifiedCounterPayload(headerPayload)
        )
      ) {
        payload = parseLivePayload(await fetchFormattedCounters());
      }
      if (processPayload(payload)) {
        state.live.mode = "stack";
        applyLiveProcesses(payload.processes);
      } else {
        state.live.mode = "events";
        applyLiveCounters(flattenCounters(payload));
      }
      state.live.lastSeen = Date.now();
      state.live.lastError = "";
    } catch (error) {
      state.live.lastError = error.message || String(error);
      state.live.totalDelta = 0;
    }
    renderLiveFrame();
  }

  function parseLivePayload(text) {
    const trimmed = (text || "").trim();
    if (!trimmed) return {};
    try {
      const parsed = JSON.parse(trimmed);
      if (typeof parsed.body === "string" && parsed.body.includes("=>")) {
        return parseFormattedCounters(parsed.body);
      }
      return parsed;
    } catch (_error) {
      if (trimmed.includes("=>")) return parseFormattedCounters(trimmed);
      return parsePrometheusCounters(trimmed);
    }
  }

  function processPayload(payload) {
    return payload && Array.isArray(payload.processes);
  }

  function responseHeaderPayload(headers) {
    const payload = {};
    if (!headers || typeof headers.forEach !== "function") return payload;
    headers.forEach((value, key) => {
      if (key.endsWith("+link") || key === "status") payload[key] = value;
    });
    return payload;
  }

  async function fetchFormattedCounters() {
    const response = await fetch(formattedCounterEndpoint(state.live.endpoint), {
      cache: "no-store",
      headers: { accept: "application/json, text/plain;q=0.9, */*;q=0.8" }
    });
    if (!response.ok) throw new Error(`${response.status} ${response.statusText}`);
    return response.text();
  }

  function formattedCounterEndpoint(endpoint) {
    const url = new URL(endpoint, window.location.href);
    url.pathname = `${url.pathname.replace(/\/$/, "")}/~hyperbuddy@1.0/format`;
    url.search = "";
    url.searchParams.set("format", "base");
    return url.href;
  }

  function linkifiedCounterPayload(payload) {
    if (!payload || typeof payload !== "object" || Array.isArray(payload)) return false;
    const keys = Object.keys(payload);
    if (!keys.some((key) => key.endsWith("+link"))) return false;
    return !payloadHasCounters(payload);
  }

  function payloadHasCounters(payload) {
    return flattenCounters(payload).some(({ key }) => key !== "status");
  }

  function parseFormattedCounters(text) {
    const counters = {};
    let group = "";
    text.split(/\n/).forEach((line) => {
      const mapLine = line.match(/^\s*([A-Za-z0-9_@+.-]+)\s*=>\s*#\{(.+)\}/);
      if (mapLine) {
        [...mapLine[2].matchAll(/<<"([^"]+)">>\s*=>\s*([0-9.]+)/g)]
          .forEach((match) => {
            counters[`${mapLine[1]}/${match[1]}`] = Number(match[2]);
          });
        return;
      }
      const groupLine = line.match(/^\s*([A-Za-z0-9_@+.-]+)\s*=>\s*$/);
      if (groupLine) {
        group = groupLine[1];
        return;
      }
      const valueLine = line.match(/^\s*([A-Za-z0-9_@+.-]+)\s*=>\s*([0-9.]+)\s*$/);
      if (!valueLine) return;
      const key = group ? `${group}/${valueLine[1]}` : valueLine[1];
      counters[key] = Number(valueLine[2]);
    });
    return counters;
  }

  function parsePrometheusCounters(text) {
    const counters = {};
    text.split(/\n/).forEach((line) => {
      if (!line || line.startsWith("#")) return;
      const match = line.match(/^event(?:\{([^}]*)\})?\s+([0-9.]+)$/);
      if (!match) return;
      const labels = {};
      (match[1] || "").split(",").forEach((pair) => {
        const label = pair.match(/([^=]+)="([^"]*)"/);
        if (label) labels[label[1]] = label[2];
      });
      const topic = labels.topic || "event";
      const event = labels.event || "count";
      if (!counters[topic]) counters[topic] = {};
      counters[topic][event] = Number(match[2]);
    });
    return counters;
  }

  function flattenCounters(value) {
    const out = [];
    const walk = (node, path) => {
      if (typeof node === "number" && Number.isFinite(node)) {
        out.push({ key: path.join("/"), value: node });
      } else if (Array.isArray(node)) {
        node.forEach((item, idx) => walk(item, path.concat(String(idx))));
      } else if (node && typeof node === "object") {
        Object.entries(node).forEach(([key, child]) => walk(child, path.concat(key)));
      }
    };
    walk(value, []);
    return out;
  }

  function applyLiveCounters(counters) {
    const now = Date.now();
    const elapsedSeconds = state.live.lastCounterAt ?
      Math.max(0.1, (now - state.live.lastCounterAt) / 1000) :
      0;
    state.live.lastCounterAt = now;
    let totalDelta = 0;
    state.live.traceEdges = new Map();
    counters.forEach(({ key, value }) => {
      const previous = state.live.previous.get(key);
      const delta = previous === undefined ? 0 : Math.max(0, value - previous);
      state.live.previous.set(key, value);
      rememberLiveRate(key, delta, elapsedSeconds);
      if (delta > 0) {
        totalDelta += delta;
        rememberLiveEvent(key, delta, elapsedSeconds);
        applyLiveKey(key, delta);
      }
    });
    state.live.totalDelta = totalDelta;
    state.live.processCount = 0;
    state.live.processSamples = [];
    state.live.frameCount = 0;
    state.live.sourceName = "";
    state.live.samples = new Map();
  }

  function applyLiveProcesses(processes) {
    let totalDelta = 0;
    const nextSamples = new Map();
    const processSamples = [];
    state.live.traceEdges = new Map();
    state.live.eventDeltas = new Map();
    state.live.eventRates = new Map();
    state.live.previousRates = new Map();
    state.live.rateChanges = new Map();
    state.live.rateActivity = new Map();
    state.live.eventHistory = new Map();
    state.live.eventTick = 0;
    state.live.lastCounterAt = 0;
    processes.forEach((proc) => {
      const pid = String(proc.pid || "");
      const reductions = Number(proc.reductions || 0);
      const previous = state.live.previous.get(`proc:${pid}`);
      const delta = previous === undefined ? 1 : Math.max(0, reductions - previous);
      state.live.previous.set(`proc:${pid}`, reductions);
      const amount = Math.max(0.8, Math.min(28, Math.log1p(delta || 1) * 2.6));
      totalDelta += delta;
      const sample = {
        pid,
        name: proc["registered-name"] || "",
        entry: proc.entry || "unknown",
        currentFrame: proc.current,
        current: frameLabel(proc.current),
        stack: [proc.current, ...(Array.isArray(proc.stack) ? proc.stack.slice(0, 12) : [])]
          .filter(Boolean)
          .map(frameLabel),
        status: proc.status || "unknown",
        reductions: delta,
        memory: Number(proc.memory || 0),
        queue: Number(proc["message-queue-len"] || 0)
      };
      processSamples.push(sample);
      const hotFrames = [
        { frame: proc.current, weight: 1 },
        { frame: proc["initial-call"], weight: 0.45 },
        ...(Array.isArray(proc.stack) ? proc.stack.slice(0, 10).map((frame, idx) => ({
          frame,
          weight: Math.max(0.2, 0.82 - idx * 0.06)
        })) : [])
      ];
      hotFrames.forEach(({ frame, weight }) => {
        const ids = resolveFrameIds(frame);
        ids.forEach((id) => {
          bumpLive(id, amount * weight, /error|failed|exception|crash/i.test(sample.current));
          addLiveSample(nextSamples, id, sample);
        });
      });
      addTraceEdgesForFrames(
        [proc.current, ...(Array.isArray(proc.stack) ? proc.stack.slice(0, 12) : [])],
        amount
      );
      applyLiveKey(String(proc.entry || ""), amount * 0.5);
    });
    state.live.totalDelta = totalDelta;
    state.live.processCount = processes.length;
    state.live.frameCount = processes.reduce(
      (sum, proc) => sum + (Array.isArray(proc.stack) ? proc.stack.length : 0),
      0
    );
    state.live.sourceName = "";
    state.live.samples = nextSamples;
    state.live.processSamples = processSamples;
  }

  function openRecordingImport() {
    if (typeof els.recordingFile.click === "function") {
      els.recordingFile.click();
      return;
    }
    els.recordingFile.hidden = false;
    if (typeof els.recordingFile.focus === "function") els.recordingFile.focus();
  }

  async function importRecordingFile() {
    const file = els.recordingFile.files && els.recordingFile.files[0];
    if (!file) return;
    try {
      const report = parseRecordingReport(await file.text());
      applyRecordingReport(report, file.name || "imported");
    } catch (error) {
      stopLive({ renderAfter: false });
      state.live.enabled = true;
      state.live.mode = "recording";
      state.live.lastError = `import failed: ${error.message || error}`;
      render();
    } finally {
      els.recordingFile.value = "";
    }
  }

  async function loadRecordingUrl(recordingUrl) {
    try {
      const url = new URL(recordingUrl, window.location.href);
      const response = await fetch(url.href, {
        cache: "no-store",
        headers: { accept: "application/json, text/html;q=0.9, */*;q=0.8" }
      });
      if (!response.ok) throw new Error(`${response.status} ${response.statusText}`);
      applyRecordingReport(parseRecordingReport(await response.text()), url.pathname.split("/").pop() || "recording", recordingUrl);
    } catch (error) {
      stopLive({ renderAfter: false });
      state.live.enabled = true;
      state.live.mode = "recording";
      state.live.recordingUrl = recordingUrl;
      state.live.lastError = `recording failed: ${error.message || error}`;
      render();
    }
  }

  function parseRecordingReport(text) {
    const embedded = text.match(/<script[^>]+id=["']embedded-log["'][^>]*>([^<]*)<\/script>/i);
    if (embedded) {
      return JSON.parse(new TextDecoder().decode(base64ToBytes(embedded[1])));
    }
    return JSON.parse(text);
  }

  function applyRecordingReport(report, sourceName, recordingUrl = "") {
    const pendingFocus = state.live.pendingRecordingFocus;
    const follow = state.live.follow;
    stopLive({ renderAfter: false, clearFollow: false });
    state.live.enabled = true;
    state.detailTab = "engine";
    state.live.mode = "recording";
    state.live.endpoint = "";
    state.live.follow = follow;
    state.live.previous = new Map();
    state.live.activity = new Map();
    state.live.errors = new Map();
    state.live.samples = new Map();
    state.live.traceEdges = new Map();
    state.live.eventDeltas = new Map();
    state.live.eventRates = new Map();
    state.live.previousRates = new Map();
    state.live.rateChanges = new Map();
    state.live.rateActivity = new Map();
    state.live.eventHistory = new Map();
    state.live.eventTick = 0;
    state.live.lastCounterAt = 0;
    state.live.totalDelta = 0;
    state.live.processCount = 0;
    state.live.processSamples = [];
    state.live.frameCount = 0;
    state.live.sourceName = sourceName;
    state.live.recordingUrl = recordingUrl;
    state.live.recordingEvents = Array.isArray(report.events) ? report.events : [];
    state.live.recordingFocus = -1;
    state.live.recordingPlaying = false;
    state.live.recordingTimer = null;
    state.live.pendingRecordingFocus = pendingFocus;
    state.live.lastError = "";
    paintRecordingEntries(recordingEntries(state.live.recordingEvents));
    if (
      state.live.pendingRecordingFocus >= 0 &&
      state.live.pendingRecordingFocus < state.live.recordingEvents.length
    ) {
      const focus = state.live.pendingRecordingFocus;
      state.live.pendingRecordingFocus = -1;
      focusRecordingEvent(focus);
    } else {
      state.live.pendingRecordingFocus = -1;
      render();
    }
  }

  function recordingEntries(events) {
    return events.map((event, idx) => ({ event, idx }));
  }

  function paintRecordingEntries(entries) {
    state.live.activity = new Map();
    state.live.errors = new Map();
    state.live.samples = new Map();
    state.live.traceEdges = new Map();
    state.live.eventDeltas = new Map();
    state.live.eventRates = new Map();
    state.live.previousRates = new Map();
    state.live.rateChanges = new Map();
    state.live.rateActivity = new Map();
    state.live.eventHistory = new Map();
    state.live.eventTick = 0;
    state.live.lastCounterAt = 0;
    state.live.totalDelta = entries.length;
    state.live.frameCount = 0;
    const samples = new Map();
    entries.forEach(({ event, idx }) => paintRecordingEvent(event, idx, samples));
    state.live.samples = samples;
  }

  function paintRecordingEvent(event, idx, samples) {
    const frames = recordingFrames(event);
    const sample = {
      pid: `event ${event.sequence || idx + 1}`,
      entry: `${event.topic || "recording"}/${event.name || "event"}`,
      current: frames.length ? frameLabel(frames[0]) : recordingEventLabel(event),
      stack: frames.map(frameLabel),
      status: "recorded",
      reductions: 1,
      queue: 0
    };
    frames.forEach((frame, frameIdx) => {
      const amount = Math.max(0.45, 5 - frameIdx * 0.28);
      resolveFrameIds(frame).forEach((id) => {
        bumpLive(id, amount, recordingEventIsError(event));
        addLiveSample(samples, id, sample);
      });
    });
    addTraceEdgesForFrames(frames, 3.8);
    resolveFrameIds(recordingEventFrame(event)).forEach((id) => {
      bumpLive(id, 2.2, recordingEventIsError(event));
      addLiveSample(samples, id, sample);
    });
    state.live.frameCount += frames.length;
  }

  function recordingFrames(event) {
    return (Array.isArray(event.stack) ? event.stack : [])
      .map(recordingFrame)
      .filter(Boolean);
  }

  function recordingFrame(frame) {
    if (!frame) return null;
    if (typeof frame === "string") return frame;
    if (Array.isArray(frame) && frame.length >= 3) {
      const module = String(frame[0]);
      const func = String(frame[1]);
      const arity = recordingArity(frame[2]);
      return {
        label: `${module}:${func}/${arity}`,
        module,
        function: func,
        arity
      };
    }
    if (typeof frame === "object") return frame;
    return null;
  }

  function recordingArity(value) {
    if (typeof value === "number") return value;
    if (Array.isArray(value)) return value.length;
    const parsed = Number(value);
    return Number.isFinite(parsed) ? parsed : value;
  }

  function recordingEventFrame(event) {
    const module = event.module || "unknown";
    const func = event.function || "unknown";
    return {
      label: `${module}:${func}`,
      module,
      function: func,
      arity: ""
    };
  }

  function recordingEventLabel(event) {
    return `${event.module || "unknown"}:${event.function || "unknown"}`;
  }

  function recordingEventName(event) {
    return `${event.topic || "recording"}/${event.name || "event"}`;
  }

  function recordingEventIsError(event) {
    return /error|failed|warning|throw|crash|exception/i.test(recordingEventName(event));
  }

  function demoRecordingReport() {
    return {
      events: [
        {
          sequence: 1,
          topic: "ao_result",
          name: "resolving",
          module: "hb_ao",
          function: "resolve",
          stack: [
            ["hb_message", "commit", 3, []],
            ["hb_ao", "resolve", 3, []],
            ["dev_recorder", "record", 3, []]
          ]
        },
        {
          sequence: 2,
          topic: "scheduler",
          name: "compute",
          module: "dev_scheduler",
          function: "compute",
          stack: [
            ["dev_scheduler", "compute", 3, []],
            ["hb_process", "execute", 3, []],
            ["hb_cache", "read", 2, []]
          ]
        },
        {
          sequence: 3,
          topic: "warning",
          name: "process_sampler_failed",
          module: "hb_process_sampler",
          function: "sample_processes",
          stack: [
            ["hb_process_sampler", "sample_processes", 1, []],
            ["hb_event", "log", 6, []],
            ["hb_prometheus", "inc", 3, []]
          ]
        }
      ]
    };
  }

  function addLiveSample(samples, id, sample) {
    if (!samples.has(id)) samples.set(id, []);
    const bucket = samples.get(id);
    if (bucket.some((existing) => existing.pid === sample.pid && existing.current === sample.current)) {
      return;
    }
    bucket.push(sample);
    bucket.sort((a, b) => b.reductions - a.reductions);
    if (bucket.length > 8) bucket.length = 8;
  }

  function frameLabel(frame) {
    if (!frame) return "unknown";
    if (typeof frame === "string") return frame;
    return frame.label || [
      frame.module || "unknown",
      frame.function || "unknown",
      frame.arity === undefined ? "" : `/${frame.arity}`
    ].join(":").replace(":/", "/");
  }

  function resolveFrameIds(frame) {
    const ids = new Set();
    if (!frame) return ids;
    if (typeof frame === "string") {
      resolveLiveIds(frame).forEach((id) => ids.add(id));
      return ids;
    }
    if (frame.module) {
      addFrameModuleMatch(ids, frame.module);
      if (frame.function && frame.arity !== undefined) {
        addLiveMatches(ids, `${frame.module}:${frame.function}/${frame.arity}`);
      }
      return ids;
    }
    const label = frame.label || frameLabel(frame);
    resolveLiveIds(label).forEach((id) => ids.add(id));
    return ids;
  }

  function addFrameModuleMatch(ids, module) {
    const exact = liveToken(module);
    if (byModule.has(exact)) {
      ids.add(exact);
    } else {
      addLiveMatches(ids, module);
    }
  }

  function addTraceEdgesForFrames(frames, amount) {
    const cleanFrames = frames.filter(Boolean);
    const resolved = cleanFrames
      .map(primaryTraceIds)
      .filter((ids) => ids.length);
    for (let idx = 0; idx < resolved.length - 1; idx += 1) {
      const targets = resolved[idx];
      const sources = resolved[idx + 1];
      const weight = Math.max(0.35, amount * (1 - idx * 0.07));
      const route = cleanFrames.slice(idx, idx + 5).map(frameLabel);
      sources.forEach((source) => {
        targets.forEach((target) => addTraceEdge(source, target, weight, route));
      });
    }
  }

  function primaryTraceIds(frame) {
    const ids = [...resolveFrameIds(frame)];
    const functions = ids.filter((id) => byFunction.has(id));
    if (functions.length) return functions.slice(0, 3);
    return ids.filter((id) => byModule.has(id)).slice(0, 3);
  }

  function addTraceEdge(source, target, amount, route = []) {
    if (!source || !target || source === target) return;
    const key = `${source}->${target}`;
    const edge = state.live.traceEdges.get(key) || { source, target, count: 0, path: [], pathWeight: 0 };
    edge.count += amount;
    if (route.length && (!edge.path.length || amount >= edge.pathWeight)) {
      edge.path = route;
      edge.pathWeight = amount;
    }
    state.live.traceEdges.set(key, edge);
  }

  function demoLiveTick() {
    if (!state.live.enabled || state.live.mode !== "demo") return;
    state.live.lastPollStarted = Date.now();
    state.live.eventTick += 1;
    decayLiveActivity();
    const candidates = [
      "hb_message",
      "hb_ao",
      "hb_cache",
      "hb_http_server",
      "hb_process",
      "hb_event",
      "dev_scheduler",
      "dev_recorder",
      "dev_hyperbuddy"
    ].filter((id) => byModule.has(id));
    if (!candidates.length) return;
    const tick = state.live.demoTick;
    state.live.demoTick += 1;
    const tickSeconds = state.live.intervalMs / 1000;
    state.live.traceEdges = new Map();
    let totalDelta = 0;
    candidates.slice(0, 7).forEach((_, offset) => {
      const id = candidates[(tick + offset * 2) % candidates.length];
      const amount = 2 + ((tick + offset) % 5);
      totalDelta += amount;
      rememberLiveRate(`${id}/events`, amount, tickSeconds);
      rememberLiveEvent(`${id}/events`, amount, tickSeconds);
      bumpLive(id, amount, false);
    });
    if (tick % 5 === 3) {
      bumpLive("warning/process_sampler_failed", 5, true);
      rememberLiveRate("warning/process_sampler_failed", 5, tickSeconds);
      rememberLiveEvent("warning/process_sampler_failed", 5, tickSeconds);
      totalDelta += 5;
    }
    addTraceEdgesForFrames([
      "hb_message:commit/3",
      "hb_ao:resolve/3",
      "hb_http:handle/3",
      "hb_http_server:handle/2"
    ], 4.2);
    addTraceEdgesForFrames([
      "dev_scheduler:compute/3",
      "hb_process:execute/3",
      "hb_cache:read/2"
    ], 3.4);
    state.live.totalDelta = totalDelta;
    state.live.lastSeen = Date.now();
    state.live.lastError = "";
    renderLiveFrame();
  }

  function decayLiveActivity() {
    decayMap(state.live.activity, 0.7);
    decayMap(state.live.errors, 0.58);
    decayMap(state.live.eventDeltas, 0.66, 0.45);
    decayMap(state.live.eventRates, 0.66, 0.05);
    decaySignedMap(state.live.rateChanges, 0.64, 0.05);
    decaySignedMap(state.live.rateActivity, 0.68, 0.05);
  }

  function decayMap(map, factor, minimum = 0.18) {
    [...map.entries()].forEach(([key, value]) => {
      const next = value * factor;
      if (next < minimum) {
        map.delete(key);
      } else {
        map.set(key, next);
      }
    });
  }

  function decaySignedMap(map, factor, minimum = 0.18) {
    [...map.entries()].forEach(([key, value]) => {
      const next = value * factor;
      if (Math.abs(next) < minimum) {
        map.delete(key);
      } else {
        map.set(key, next);
      }
    });
  }

  function rememberLiveEvent(key, amount, seconds = 1) {
    state.live.eventDeltas.set(key, (state.live.eventDeltas.get(key) || 0) + amount);
    const tick = state.live.eventTick || 0;
    const history = (state.live.eventHistory.get(key) || []).slice(-15);
    const last = history[history.length - 1];
    if (last && last.tick === tick) {
      last.amount += amount;
    } else {
      history.push({ tick, amount });
    }
    state.live.eventHistory.set(key, history);
  }

  function rememberLiveRate(key, amount, seconds = 1) {
    if (seconds <= 0) return;
    const rate = Math.max(0, amount) / seconds;
    const previous = state.live.previousRates.get(key);
    state.live.previousRates.set(key, rate);
    if (rate > 0.05) {
      state.live.eventRates.set(key, rate);
    } else {
      state.live.eventRates.delete(key);
    }
    if (previous === undefined) return;
    const change = rate - previous;
    if (Math.abs(change) < 0.05) return;
    state.live.rateChanges.set(key, change);
    applyLiveRateKey(key, change);
  }

  function formatEventRate(rate) {
    if (!Number.isFinite(rate) || rate <= 0) return "0/s";
    const value = rate >= 10 ? Math.round(rate) : Math.round(rate * 10) / 10;
    return `${nf.format(value)}/s`;
  }

  function formatInterval(ms) {
    const seconds = Math.round(ms / 100) / 10;
    return `${nf.format(seconds)}s`;
  }

  function liveFreshnessLabel() {
    const timestamp = state.live.lastSeen || state.live.lastPollStarted;
    if (!timestamp) return "pending";
    const seconds = Math.max(0, Math.round((Date.now() - timestamp) / 1000));
    return seconds < 1 ? "now" : `${nf.format(seconds)}s ago`;
  }

  function formatBytes(bytes) {
    if (!Number.isFinite(bytes) || bytes <= 0) return "";
    if (bytes < 1024) return `${nf.format(Math.round(bytes))} B`;
    const units = ["KB", "MB", "GB"];
    let value = bytes / 1024;
    let unitIdx = 0;
    while (value >= 1024 && unitIdx < units.length - 1) {
      value /= 1024;
      unitIdx += 1;
    }
    const rounded = value >= 10 ? Math.round(value) : Math.round(value * 10) / 10;
    return `${nf.format(rounded)} ${units[unitIdx]}`;
  }

  function applyLiveKey(key, amount) {
    const error = /error|failed|warning|throw|crash|exception/i.test(key);
    const ids = resolveLiveIds(key);
    if (!ids.size) return;
    ids.forEach((id) => bumpLive(id, amount, error));
  }

  function applyLiveRateKey(key, change) {
    const ids = resolveLiveIds(key);
    if (!ids.size) return;
    ids.forEach((id) => bumpLiveRate(id, change));
  }

  function bumpLive(id, amount, error) {
    const resolved = byModule.has(id) || byFunction.has(id) ? new Set([id]) : resolveLiveIds(id);
    const targets = resolved.size ? resolved : new Set([id]);
    targets.forEach((target) => {
      state.live.activity.set(target, (state.live.activity.get(target) || 0) + amount);
      if (error) state.live.errors.set(target, (state.live.errors.get(target) || 0) + amount);
    });
  }

  function bumpLiveRate(id, change) {
    const resolved = byModule.has(id) || byFunction.has(id) ? new Set([id]) : resolveLiveIds(id);
    const targets = resolved.size ? resolved : new Set([id]);
    targets.forEach((target) => {
      state.live.rateActivity.set(target, (state.live.rateActivity.get(target) || 0) + change);
    });
  }

  function resolveLiveIds(key) {
    const raw = String(key || "");
    if (liveResolutionCache.has(raw)) return liveResolutionCache.get(raw);
    const ids = new Set();
    const pieces = raw
      .split(/[^A-Za-z0-9_@.:'/-]+/)
      .flatMap((piece) => piece.split("/"))
      .filter(Boolean);
    [raw, ...pieces].forEach((piece) => addLiveMatches(ids, piece));
    const mfa = raw.match(/\b([a-z][A-Za-z0-9_]*)(?::|\.)([A-Za-z0-9_'-]+\/\d+)/);
    if (mfa) addLiveMatches(ids, `${mfa[1]}:${mfa[2]}`);
    liveResolutionCache.set(raw, ids);
    return ids;
  }

  function addLiveMatches(ids, value) {
    const token = liveToken(value);
    const variants = [
      token,
      token.replace(/^~/, ""),
      token.replace(/@.*$/, ""),
      `hb_${token}`,
      `dev_${token}`,
      token.replace(/^hb_/, ""),
      token.replace(/^dev_/, "")
    ];
    variants.forEach((variant) => {
      if (byModule.has(variant)) ids.add(variant);
      if (byFunction.has(variant)) ids.add(variant);
      const indexed = liveIndex.get(variant);
      if (indexed) indexed.forEach((id) => ids.add(id));
    });
  }

  function liveNodeScore(node) {
    if (!state.live.enabled) return 0;
    let score = state.live.activity.get(node.id) || 0;
    if (node.kind === "system") {
      (node.moduleIds || []).forEach((moduleId) => {
        score += state.live.activity.get(moduleId) || 0;
        (functionsByModuleId.get(moduleId) || []).forEach((funId) => {
          score += (state.live.activity.get(funId) || 0) * 0.45;
        });
      });
    } else if (node.kind === "module") {
      (functionsByModuleId.get(node.id) || []).forEach((funId) => {
        score += (state.live.activity.get(funId) || 0) * 0.6;
      });
    } else if (node.kind === "function") {
      score += (state.live.activity.get(node.module) || 0) * 0.22;
    }
    return score;
  }

  function liveRateScore(node) {
    if (!state.live.enabled || !state.live.rateMode) return 0;
    let score = state.live.rateActivity.get(node.id) || 0;
    if (node.kind === "system") {
      (node.moduleIds || []).forEach((moduleId) => {
        score += state.live.rateActivity.get(moduleId) || 0;
        (functionsByModuleId.get(moduleId) || []).forEach((funId) => {
          score += (state.live.rateActivity.get(funId) || 0) * 0.45;
        });
      });
    } else if (node.kind === "module") {
      (functionsByModuleId.get(node.id) || []).forEach((funId) => {
        score += (state.live.rateActivity.get(funId) || 0) * 0.6;
      });
    } else if (node.kind === "function") {
      score += (state.live.rateActivity.get(node.module) || 0) * 0.22;
    }
    return score;
  }

  function heatFollowTarget() {
    if (!state.live.enabled || !state.live.follow) return null;
    const scores = new Map();
    const addScore = (id, amount) => {
      const target = frameTargetForMode(id);
      if (!target || !Number.isFinite(amount) || amount <= 0) return;
      scores.set(target, (scores.get(target) || 0) + amount);
    };
    state.live.activity.forEach((amount, id) => addScore(id, amount));
    state.live.errors.forEach((amount, id) => addScore(id, amount * 1.8));
    state.live.eventDeltas.forEach((amount, key) => {
      resolveLiveIds(key).forEach((id) => addScore(id, amount * 0.75));
    });
    let best = null;
    scores.forEach((score, target) => {
      if (!best || score > best.score) best = { target, score };
    });
    if (!best || best.score < 0.5) return null;
    const current = scores.get(state.selected) || 0;
    if (state.selected && current >= best.score * 0.62) return state.selected;
    return best.target;
  }

  function liveErrorScore(node) {
    if (!state.live.enabled) return 0;
    let score = state.live.errors.get(node.id) || 0;
    if (node.kind === "system") {
      (node.moduleIds || []).forEach((moduleId) => {
        score += state.live.errors.get(moduleId) || 0;
        (functionsByModuleId.get(moduleId) || []).forEach((funId) => {
          score += (state.live.errors.get(funId) || 0) * 0.45;
        });
      });
    } else if (node.kind === "module") {
      (functionsByModuleId.get(node.id) || []).forEach((funId) => {
        score += (state.live.errors.get(funId) || 0) * 0.6;
      });
    } else if (node.kind === "function") {
      score += (state.live.errors.get(node.module) || 0) * 0.22;
    }
    return score;
  }

  function liveSamplesForNode(node) {
    if (!state.live.enabled || !state.live.samples.size) return [];
    const ids = liveScopeIds(node);
    const seen = new Set();
    const samples = [];
    ids.forEach((id) => {
      (state.live.samples.get(id) || []).forEach((sample) => {
        const key = `${sample.pid}:${sample.current}`;
        if (seen.has(key)) return;
        seen.add(key);
        samples.push(sample);
      });
    });
    return samples.sort((a, b) => b.reductions - a.reductions).slice(0, 10);
  }

  function liveEventsForNode(node) {
    if (!state.live.enabled || !state.live.eventDeltas.size) return [];
    const ids = liveScopeIds(node);
    return [...state.live.eventDeltas.entries()]
      .filter(([, delta]) => delta > 0.45)
      .filter(([key]) => [...resolveLiveIds(key)].some((id) => ids.has(id)))
      .map(([key, delta]) => ({ key, delta, rate: state.live.eventRates.get(key) || 0 }))
      .sort((a, b) => b.delta - a.delta)
      .slice(0, 8);
  }

  function recordingEventsForNode(node) {
    if (state.live.mode !== "recording" || !state.live.recordingEvents.length) return [];
    const ids = liveScopeIds(node);
    return state.live.recordingEvents
      .map((event, idx) => {
        const matches = [];
        [...recordingFrames(event), recordingEventFrame(event)].forEach((frame) => {
          const resolved = resolveFrameIds(frame);
          if ([...resolved].some((id) => ids.has(id))) matches.push(frameLabel(frame));
        });
        if (!matches.length) return null;
        return {
          event,
          idx,
          matches: [...new Set(matches)].slice(0, 5),
          error: recordingEventIsError(event)
        };
      })
      .filter(Boolean)
      .slice(0, 12);
  }

  function liveScopeIds(node) {
    const ids = new Set([node.id]);
    if (node.kind === "system") {
      (node.moduleIds || []).forEach((moduleId) => {
        ids.add(moduleId);
        (functionsByModuleId.get(moduleId) || []).forEach((funId) => ids.add(funId));
      });
    } else if (node.kind === "module") {
      (functionsByModuleId.get(node.id) || []).forEach((funId) => ids.add(funId));
    }
    return ids;
  }

  function liveEdgeScore(edge) {
    return Math.min(liveNodeScore(edge.sourceNode), liveNodeScore(edge.targetNode));
  }

  function liveMetaText() {
    if (!state.live.enabled) return "";
    const active = [...state.live.activity.values()].filter((value) => value > 1).length;
    if (state.live.lastError) return `live error: ${state.live.lastError}`;
    if (state.live.mode === "stack") {
      return `stacks: ${nf.format(state.live.processCount)} procs · +${nf.format(Math.round(state.live.totalDelta))} reductions · ${state.live.traceEdges.size} traces · ${active} hot`;
    }
    if (state.live.mode === "recording") {
      const source = state.live.sourceName ? `${state.live.sourceName}: ` : "";
      return `${source}${nf.format(state.live.totalDelta)} events · ${nf.format(state.live.frameCount)} frames · ${state.live.traceEdges.size} traces · ${active} hot`;
    }
    const prefix = state.live.mode === "demo" ? "demo live" : "live";
    const rate = [...state.live.eventRates.values()].reduce((sum, value) => sum + value, 0);
    return `${prefix}: +${nf.format(Math.round(state.live.totalDelta))} events · ${formatEventRate(rate)} · ${active} hot`;
  }

  function renderLiveControls() {
    els.liveFollow.classList.toggle("active", state.live.follow);
    els.liveFollow.setAttribute("aria-pressed", state.live.follow ? "true" : "false");
    els.liveRateMode.classList.toggle("active", state.live.rateMode);
    els.liveRateMode.setAttribute("aria-pressed", state.live.rateMode ? "true" : "false");
    if (!state.live.enabled) {
      els.liveStatus.textContent = "live off";
      els.liveStatus.className = "live-status";
      els.liveStop.hidden = true;
      return;
    }
    els.liveStatus.textContent = liveMetaText() || "live connected";
    els.liveStatus.className = `live-status ${state.live.lastError ? "error" : "active"}`;
    els.liveStop.hidden = false;
  }

  function renderStats(visible) {
    els.modules.textContent = nf.format(visible.modules.length);
    els.functions.textContent = nf.format(visible.functions.length);
    els.calls.textContent = nf.format(visible.edges.reduce((sum, edge) => sum + edge.count, 0));
    const scope = effectiveContextScope();
    els.context.textContent = state.selectedDevices.size ?
      countLabel(
        state.selectedDevices.size,
        scope === "touchpoints" ? "touchpoint" : "device",
        scope === "touchpoints" ? "touchpoints" : "devices"
      ) :
      "kernel";
    els.graphTitle.textContent = graphTitleText();
    const searchMatches = state.search ?
      state.layout.nodes.filter((node) => nodeMatchesSearch(node)).length :
      0;
    els.graphMeta.textContent = [
      `${nf.format(state.layout.nodes.length)} visible nodes`,
      state.layout.force ? layoutMetaLabel() : "",
      functionOverviewActive() ? "overview" : "",
      state.selectedDevices.size ? contextScopeLabel() : "",
      state.search ? `${nf.format(searchMatches)} matches` : "",
      `${nf.format(state.layout.edges.length)} visible calls`,
      liveMetaText()
    ].filter(Boolean).join(" · ");
    renderLiveControls();
  }

  function graphTitleText() {
    if (state.mode === "system") return "Subsystem flow map";
    if (state.mode === "function") {
      if (functionOverviewActive()) return "Function call graph overview";
      return state.selectedDevices.size ?
        effectiveContextScope() === "touchpoints" ?
          "Function touchpoint map" :
          "Kernel/device function map" :
        "Function call graph";
    }
    return state.selectedDevices.size ? "Kernel plus device context" : "Kernel call graph";
  }

  function layoutMetaLabel() {
    if (namespaceLayoutActive()) return "namespace map";
    return "force map";
  }

  function contextScopeLabel() {
    const scope = effectiveContextScope();
    if (state.contextScope === "auto") {
      return scope === "touchpoints" ? "auto touchpoints" : "auto kernel";
    }
    return scope === "touchpoints" ? "device touchpoints" : "kernel context";
  }

  function functionOverviewActive() {
    return state.mode === "function" &&
      !state.search &&
      !state.selected &&
      state.layout.nodes.length > 420;
  }

  function renderGraph() {
    els.stage.dataset.layout = state.layout.force ? state.layoutMode : "flow";
    renderBands();
    renderEdges();
    renderNodes();
    renderTelemetryPanel();
    renderMinimap();
    applyTransform();
  }

  function renderTelemetryPanel() {
    els.enginePanel.classList.toggle("live-active", state.live.enabled);
    els.enginePanel.classList.toggle("context-active", !state.live.enabled);
    const hasSource = renderEngineSourcePanel();
    if (!state.live.enabled) {
      els.processPanel.replaceChildren();
      const hasBridge = renderBridgePanel();
      els.enginePanel.hidden = !hasSource && !hasBridge;
      return;
    }
    const hasHeat = renderHeatPanel();
    const hasTraces = renderTracePanel();
    const hasProcesses = renderProcessPanel();
    const hasErrors = renderErrorPanel();
    const hasTimeline = renderRecordingTimeline();
    els.enginePanel.hidden = !hasSource && !hasHeat && !hasTraces && !hasProcesses && !hasErrors && !hasTimeline;
  }

  function renderEngineSourcePanel() {
    if (!state.live.enabled && !state.selectedDevices.size) {
      els.engineSource.replaceChildren();
      return false;
    }
    const copy = document.createElement("div");
    copy.className = "engine-source-copy";
    const title = document.createElement("strong");
    title.textContent = engineSourceTitle();
    const detail = document.createElement("span");
    detail.textContent = engineSourceDetail();
    copy.append(title, detail);

    const metrics = document.createElement("div");
    metrics.className = "engine-source-metrics";
    engineSourceMetrics().forEach(({ label, value, kind }) => {
      const chip = document.createElement("span");
      chip.className = `engine-metric${kind ? ` ${kind}` : ""}`;
      const chipLabel = document.createElement("small");
      chipLabel.textContent = label;
      const chipValue = document.createElement("strong");
      chipValue.textContent = value;
      chip.append(chipLabel, chipValue);
      metrics.appendChild(chip);
    });

    const controls = document.createElement("div");
    controls.className = "engine-source-controls";
    [
      ["events", "Events", "Stream HyperBuddy event counters", () => startLive(defaultLiveEndpoint)],
      ["stack", "Stacks", "Poll recorder live process stacks", () => startLive(defaultStackEndpoint)],
      ["demo", "Demo", "Run demo telemetry", () => startLive("demo")],
      ["recording", "Recording", "Paint the demo recorder black box", () => applyRecordingReport(demoRecordingReport(), "demo")],
      ["import", "Import", "Import a recorder HTML or JSON report", openRecordingImport]
    ].forEach(([mode, label, titleText, action]) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = `engine-source-button${engineSourceActive(mode) ? " active" : ""}`;
      button.title = titleText;
      button.textContent = label;
      button.addEventListener("click", action);
      controls.appendChild(button);
    });
    els.engineSource.replaceChildren(copy, metrics, controls);
    return true;
  }

  function engineSourceTitle() {
    if (!state.live.enabled) return "Device context";
    if (state.live.mode === "recording") return "Recorder black box";
    if (state.live.mode === "stack") return "Live stack sampler";
    if (state.live.mode === "demo") return "Demo engine feed";
    return "HyperBuddy counters";
  }

  function engineSourceDetail() {
    if (!state.live.enabled) {
      return [
        `${nf.format(state.selectedDevices.size)} selected devices`,
        contextScopeLabel(),
        namespaceLayoutActive() ? "source namespaces visible" : "call-force objective"
      ].join(" · ");
    }
    if (state.live.lastError) return state.live.lastError;
    if (state.live.mode === "recording") {
      if (state.live.recordingFocus >= 0) {
        return `${state.live.sourceName || "recording"} event ${state.live.recordingFocus + 1}`;
      }
      return state.live.sourceName || state.live.recordingUrl || "imported recorder report";
    }
    if (state.live.mode === "stack") return state.live.endpoint || defaultStackEndpoint;
    if (state.live.mode === "demo") return "synthetic counters, traces, stack heat, and errors";
    return state.live.endpoint || defaultLiveEndpoint;
  }

  function engineSourceActive(mode) {
    if (mode === "import") return false;
    if (!state.live.enabled) return false;
    return state.live.mode === mode;
  }

  function engineSourceMetrics() {
    if (!state.live.enabled) {
      return [
        { label: "Devices", value: nf.format(state.selectedDevices.size) },
        { label: "Bridges", value: nf.format(deviceBridgeCount()) }
      ];
    }
    const hot = [...state.live.activity.values()].filter((value) => value > 1).length;
    const errors = [...state.live.errors.values()].reduce((sum, value) => sum + value, 0);
    const acceleration = [...state.live.rateChanges.values()]
      .reduce((sum, value) => sum + Math.abs(value), 0);
    if (state.live.mode === "stack") {
      return [
        { label: "Processes", value: nf.format(state.live.processCount) },
        { label: "Reductions", value: `+${nf.format(Math.round(state.live.totalDelta))}` },
        { label: "Traces", value: nf.format(state.live.traceEdges.size) },
        { label: "Cadence", value: formatInterval(state.live.intervalMs) },
        { label: "Fresh", value: liveFreshnessLabel() },
        { label: "Hot", value: nf.format(hot) },
        { label: "Errors", value: `+${nf.format(Math.round(errors))}`, kind: "error" }
      ];
    }
    if (state.live.mode === "recording") {
      return [
        { label: "Events", value: nf.format(state.live.totalDelta) },
        { label: "Frames", value: nf.format(state.live.frameCount) },
        { label: "Traces", value: nf.format(state.live.traceEdges.size) },
        { label: "Hot", value: nf.format(hot) },
        { label: "Errors", value: `+${nf.format(Math.round(errors))}`, kind: "error" }
      ];
    }
    const rate = [...state.live.eventRates.values()].reduce((sum, value) => sum + value, 0);
    return [
      { label: "Events", value: `+${nf.format(Math.round(state.live.totalDelta))}` },
      { label: "Rate", value: formatEventRate(rate) },
      { label: "Accel", value: state.live.rateMode ? formatEventRate(acceleration) : "off" },
      { label: "Streams", value: nf.format(state.live.eventDeltas.size) },
      { label: "Cadence", value: formatInterval(state.live.intervalMs) },
      { label: "Fresh", value: liveFreshnessLabel() },
      { label: "Hot", value: nf.format(hot) },
      { label: "Errors", value: `+${nf.format(Math.round(errors))}`, kind: "error" }
    ];
  }

  function deviceBridgeCount() {
    return state.layout.edges
      .filter((edge) => edge.sourceNode && edge.targetNode)
      .filter((edge) => edge.sourceNode.role !== edge.targetNode.role)
      .filter((edge) => edge.sourceNode.role === "device" || edge.targetNode.role === "device")
      .length;
  }

  function renderBridgePanel() {
    if (!state.selectedDevices.size || state.mode === "function") {
      els.heatPanel.replaceChildren();
      els.tracePanel.replaceChildren();
      els.processPanel.replaceChildren();
      els.errorPanel.replaceChildren();
      els.recordingTimeline.hidden = true;
      els.recordingTimeline.replaceChildren();
      return false;
    }
    const bridges = state.layout.edges
      .filter((edge) => edge.sourceNode && edge.targetNode)
      .filter((edge) => edge.sourceNode.role !== edge.targetNode.role)
      .filter((edge) => edge.sourceNode.role === "device" || edge.targetNode.role === "device")
      .sort((a, b) => (b.count || 0) - (a.count || 0))
      .slice(0, 4);
    const touchpoints = bridgeTouchpoints();
    if (!bridges.length && !touchpoints.length) {
      els.heatPanel.replaceChildren();
      els.tracePanel.replaceChildren();
      els.errorPanel.replaceChildren();
      els.recordingTimeline.hidden = true;
      els.recordingTimeline.replaceChildren();
      return false;
    }
    renderBridgeEdges(bridges);
    renderBridgeTouchpoints(touchpoints);
    return true;
  }

  function renderBridgeEdges(bridges) {
    const title = document.createElement("div");
    title.className = "heat-title";
    title.textContent = "Device bridges";
    const rows = bridges.map((edge) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "bridge-row";
      button.title = `${edge.source} -> ${edge.target} (${countLabel(edge.count || 1, "call", "calls")})`;
      button.addEventListener("click", () => {
        selectNode(edge.target, { manual: true });
      });
      const name = document.createElement("strong");
      name.textContent = `${edge.source} -> ${edge.target}`;
      const meta = document.createElement("span");
      meta.textContent = countLabel(edge.count || 1, "call", "calls");
      button.append(name, meta);
      return button;
    });
    els.heatPanel.replaceChildren(title, ...rows);
  }

  function renderBridgeTouchpoints(touchpoints) {
    const title = document.createElement("div");
    title.className = "heat-title";
    title.textContent = "Kernel touchpoints";
    const rows = touchpoints.map(({ node, count }) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "bridge-row touchpoint";
      button.title = `${node.id} (${countLabel(count, "bridge call", "bridge calls")})`;
      button.addEventListener("click", () => {
        selectNode(node.id, { manual: true });
      });
      const name = document.createElement("strong");
      name.textContent = node.title || node.id;
      const meta = document.createElement("span");
      meta.textContent = `${countLabel(count, "bridge call", "bridge calls")} · ${node.kind}`;
      button.append(name, meta);
      return button;
    });
    els.tracePanel.replaceChildren(title, ...rows);
  }

  function bridgeTouchpoints() {
    const counts = new Map();
    state.layout.edges.forEach((edge) => {
      if (!edge.sourceNode || !edge.targetNode) return;
      if (edge.sourceNode.role === edge.targetNode.role) return;
      [edge.sourceNode, edge.targetNode].forEach((node) => {
        if (node.role === "device") return;
        counts.set(node.id, (counts.get(node.id) || 0) + (edge.count || 1));
      });
    });
    return [...counts.entries()]
      .map(([id, count]) => ({ node: state.layout.nodes.find((node) => node.id === id), count }))
      .filter((item) => item.node)
      .sort((a, b) => b.count - a.count)
      .slice(0, 4);
  }

  function renderRecordingTimeline() {
    if (state.live.mode !== "recording" || state.live.recordingEvents.length < 2) {
      els.recordingTimeline.hidden = true;
      els.recordingTimeline.replaceChildren();
      return false;
    }
    const title = document.createElement("div");
    title.className = "recording-title";
    const focusedEvent = state.live.recordingEvents[state.live.recordingFocus];
    title.textContent = focusedEvent ?
      `Recording timeline · ${recordingEventName(focusedEvent)}` :
      "Recording timeline";
    if (focusedEvent) title.title = recordingEventName(focusedEvent);
    const play = document.createElement("button");
    play.type = "button";
    play.className = state.live.recordingPlaying ?
      "recording-play active" :
      "recording-play";
    play.textContent = state.live.recordingPlaying ? "Pause" : "Play";
    play.addEventListener("click", () => {
      if (state.live.recordingPlaying) {
        stopRecordingPlayback();
      } else {
        startRecordingPlayback();
      }
    });
    const prev = recordingStepButton(
      "Prev",
      state.live.recordingFocus <= 0,
      () => focusRecordingEvent(Math.max(0, state.live.recordingFocus - 1))
    );
    const nextIndex = state.live.recordingFocus < 0 ? 0 : state.live.recordingFocus + 1;
    const next = recordingStepButton(
      "Next",
      nextIndex >= state.live.recordingEvents.length,
      () => focusRecordingEvent(nextIndex)
    );
    const all = document.createElement("button");
    all.type = "button";
    all.className = state.live.recordingFocus < 0 ? "recording-tick active" : "recording-tick";
    all.textContent = "All";
    all.addEventListener("click", () => {
      stopRecordingPlayback(false);
      state.live.recordingFocus = -1;
      state.relationFocus = null;
      state.groupFocus = null;
      state.selectedEdge = null;
      state.selectedPath = [];
      paintRecordingEntries(recordingEntries(state.live.recordingEvents));
      render();
    });
    const timelineEvents = state.live.recordingEvents.slice(0, 48);
    const maxTickHeat = Math.max(1, ...timelineEvents.map(recordingTimelineHeat));
    const ticks = timelineEvents.map((event, idx) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = [
        "recording-tick",
        recordingEventIsError(event) ? "error" : "",
        state.live.recordingFocus === idx ? "active" : ""
      ].filter(Boolean).join(" ");
      button.style.setProperty(
        "--tick-level",
        `${Math.max(18, Math.min(100, (recordingTimelineHeat(event) / maxTickHeat) * 100))}%`
      );
      button.textContent = String(event.sequence || idx + 1);
      button.title = `${recordingEventName(event)} · ${recordingTimelineHeat(event)} frames`;
      button.addEventListener("click", () => {
        stopRecordingPlayback(false);
        focusRecordingEvent(idx);
      });
      return button;
    });
    els.recordingTimeline.replaceChildren(title, play, prev, next, all, ...ticks);
    els.recordingTimeline.hidden = false;
    return true;
  }

  function recordingStepButton(label, disabled, action) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = "recording-play";
    button.textContent = label;
    button.disabled = disabled;
    button.addEventListener("click", () => {
      stopRecordingPlayback(false);
      action();
    });
    return button;
  }

  function startRecordingPlayback() {
    if (state.live.mode !== "recording" || state.live.recordingEvents.length < 2) return;
    stopRecordingPlayback(false);
    state.live.recordingPlaying = true;
    focusRecordingEvent(0);
    state.live.recordingTimer = window.setInterval(advanceRecordingPlayback, 1700);
  }

  function stopRecordingPlayback(renderAfter = true) {
    if (state.live.recordingTimer) window.clearInterval(state.live.recordingTimer);
    state.live.recordingTimer = null;
    state.live.recordingPlaying = false;
    if (renderAfter) render();
  }

  function advanceRecordingPlayback() {
    if (state.live.mode !== "recording" || !state.live.recordingPlaying) return;
    const next = state.live.recordingFocus + 1;
    if (next >= state.live.recordingEvents.length) {
      stopRecordingPlayback(false);
      state.live.recordingFocus = -1;
      state.relationFocus = null;
      state.groupFocus = null;
      state.selectedEdge = null;
      state.selectedPath = [];
      paintRecordingEntries(recordingEntries(state.live.recordingEvents));
      render();
      return;
    }
    focusRecordingEvent(next);
  }

  function focusRecordingEvent(idx) {
    const event = state.live.recordingEvents[idx];
    if (!event) return;
    state.relationFocus = null;
    state.groupFocus = null;
    state.selectedEdge = null;
    state.selectedPath = [];
    state.live.recordingFocus = idx;
    paintRecordingEntries([{ event, idx }]);
    render();
  }

  function recordingTimelineHeat(event) {
    return recordingFrames(event).length + 1;
  }

  function renderHeatPanel() {
    if (!state.live.enabled) {
      els.heatPanel.replaceChildren();
      return false;
    }
    if (state.live.rateMode) {
      const rateNodes = state.layout.nodes
        .map((node) => ({ node, score: liveRateScore(node), errors: liveErrorScore(node) }))
        .filter((item) => Math.abs(item.score) > 0.05)
        .sort((a, b) => Math.abs(b.score) - Math.abs(a.score))
        .slice(0, 6);
      if (rateNodes.length) {
        const title = document.createElement("div");
        title.className = "heat-title";
        title.textContent = "Rate change";
        const rows = rateNodes.map(({ node, score, errors }) => {
          const button = document.createElement("button");
          button.type = "button";
          button.className = [
            "heat-row",
            score >= 0 ? "rate-up" : "rate-down",
            errors > 0.6 ? "error" : ""
          ].filter(Boolean).join(" ");
          button.title = `${node.id} (${formatRateChange(score)} rate change)`;
          button.addEventListener("click", () => {
            selectNode(node.id, { manual: true });
          });
          const name = document.createElement("strong");
          name.textContent = node.title || node.id;
          const meta = document.createElement("span");
          meta.textContent = `${formatRateChange(score)} · ${node.kind}`;
          button.append(name, meta);
          return button;
        });
        els.heatPanel.replaceChildren(title, ...rows);
        return true;
      }
    }
    const hotNodes = state.layout.nodes
      .map((node) => ({ node, score: liveNodeScore(node), errors: liveErrorScore(node) }))
      .filter((item) => item.score > 0.6)
      .sort((a, b) => b.score - a.score)
      .slice(0, 4);
    if (!hotNodes.length) {
      els.heatPanel.replaceChildren();
      return false;
    }
    const title = document.createElement("div");
    title.className = "heat-title";
    title.textContent = sourceHeatLabel();
    const rows = hotNodes.map(({ node, score, errors }) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = errors > 0.6 ? "heat-row error" : "heat-row";
      button.title = `${node.id} (+${nf.format(Math.round(score))} ${sourceHeatLabel().toLowerCase()})`;
      button.addEventListener("click", () => {
        selectNode(node.id, { manual: true });
      });
      const name = document.createElement("strong");
      name.textContent = node.title || node.id;
      const meta = document.createElement("span");
      meta.textContent = `+${nf.format(Math.round(score))} · ${node.kind}`;
      button.append(name, meta);
      return button;
    });
    els.heatPanel.replaceChildren(title, ...rows);
    return true;
  }

  function sourceHeatLabel() {
    if (state.live.mode === "recording") return "Recorded heat";
    if (state.live.mode === "stack") return "Stack heat";
    return "Live heat";
  }

  function formatRateChange(value) {
    const sign = value >= 0 ? "+" : "-";
    return `${sign}${formatEventRate(Math.abs(value))}`;
  }

  function renderErrorPanel() {
    if (!state.live.enabled) {
      els.errorPanel.replaceChildren();
      return false;
    }
    const errorNodes = state.layout.nodes
      .map((node) => ({ node, score: liveErrorScore(node) }))
      .filter((item) => item.score > 0.6)
      .sort((a, b) => b.score - a.score)
      .slice(0, 4);
    if (!errorNodes.length) {
      els.errorPanel.replaceChildren();
      return false;
    }
    const title = document.createElement("div");
    title.className = "heat-title";
    title.textContent = "Error heat";
    const rows = errorNodes.map(({ node, score }) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "error-row";
      button.title = `${node.id} (+${nf.format(Math.round(score))} error heat)`;
      button.addEventListener("click", () => {
        selectNode(node.id, { manual: true });
      });
      const name = document.createElement("strong");
      name.textContent = node.title || node.id;
      const meta = document.createElement("span");
      meta.textContent = `+${nf.format(Math.round(score))} · ${node.kind}`;
      button.append(name, meta);
      return button;
    });
    els.errorPanel.replaceChildren(title, ...rows);
    return true;
  }

  function renderTracePanel() {
    if (!state.live.enabled) {
      els.tracePanel.classList.remove("event-panel");
      els.tracePanel.replaceChildren();
      return false;
    }
    if (!state.live.traceEdges.size) return renderEventPanel();
    els.tracePanel.classList.remove("event-panel");
    const traces = liveTraceEdges()
      .slice()
      .sort((a, b) => b.count - a.count)
      .slice(0, 4);
    if (!traces.length) {
      els.tracePanel.replaceChildren();
      return false;
    }
    const title = document.createElement("div");
    title.className = "heat-title";
    title.textContent = "Trace routes";
    const rows = traces.map((edge) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = edgeIsSelected(edge, "trace") ? "trace-row active" : "trace-row";
      const path = tracePath(edge);
      button.title =
        `${edge.source} -> ${edge.target} (+${countLabel(edge.count, "sampled frame", "sampled frames")})` +
        tracePathTitle(edge);
      button.addEventListener("click", () => {
        selectNode(edge.target, {
          manual: true,
          edge: {
            source: edge.source,
            target: edge.target,
            count: edge.count || 1,
            kind: "trace"
          }
        });
      });
      const name = document.createElement("strong");
      name.textContent = `${edge.source} -> ${edge.target}`;
      const meta = document.createElement("span");
      meta.textContent = `+${countLabel(edge.count, "sampled frame", "sampled frames")}`;
      button.append(name, meta);
      if (path.length > 1) {
        const route = document.createElement("span");
        route.className = "trace-path";
        route.textContent = path.slice(0, 5).join(" <- ");
        button.append(route);
      }
      return button;
    });
    els.tracePanel.replaceChildren(title, ...rows);
    return true;
  }

  function tracePath(edge) {
    return Array.isArray(edge.path) ? edge.path.filter(Boolean) : [];
  }

  function tracePathTitle(edge) {
    const path = tracePath(edge);
    return path.length > 1 ? `\n${path.join("\n")}` : "";
  }

  function renderProcessPanel() {
    if (state.live.enabled && state.live.mode === "recording") {
      return renderRecordingEventStackPanel();
    }
    if (!state.live.enabled || state.live.mode !== "stack" || !state.live.processSamples.length) {
      els.processPanel.replaceChildren();
      return false;
    }
    const samples = state.live.processSamples
      .slice()
      .sort((a, b) =>
        (b.reductions - a.reductions) ||
        (b.queue - a.queue) ||
        (b.memory - a.memory)
      )
      .slice(0, 4);
    if (!samples.length) {
      els.processPanel.replaceChildren();
      return false;
    }
    const title = document.createElement("div");
    title.className = "heat-title";
    title.textContent = "Processes";
    const rows = samples.map((sample) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = /error|failed|exception|crash/i.test(sample.current) ?
        "process-row error" :
        "process-row";
      const target = liveFrameTarget(sample.currentFrame || sample.current);
      button.disabled = !target;
      if (target) {
        button.addEventListener("click", () => {
          selectNode(target, { manual: true });
        });
      }
      if (Array.isArray(sample.stack) && sample.stack.length > 1) {
        button.title = sample.stack.join("\n");
      }
      const current = document.createElement("strong");
      current.textContent = sample.current;
      const meta = document.createElement("span");
      meta.textContent = [
        sample.name || sample.pid,
        sample.name ? sample.pid : "",
        sample.status,
        `+${nf.format(Math.round(sample.reductions))} red`,
        formatBytes(sample.memory),
        sample.queue ? `q ${nf.format(sample.queue)}` : ""
      ].filter(Boolean).join(" · ");
      button.append(current, meta);
      return button;
    });
    els.processPanel.replaceChildren(title, ...rows);
    return true;
  }

  function renderRecordingEventStackPanel() {
    const event = state.live.recordingEvents[state.live.recordingFocus];
    if (!event) {
      els.processPanel.replaceChildren();
      return false;
    }
    const frames = recordingFrames(event);
    if (!frames.length) {
      els.processPanel.replaceChildren();
      return false;
    }
    const title = document.createElement("div");
    title.className = "heat-title";
    title.textContent = "Event stack";
    const stackTitle = `${recordingEventName(event)}\n${frames.map(frameLabel).join("\n")}`;
    const rows = frames.slice(0, 8).map((frame, idx) => {
      const button = document.createElement("button");
      button.type = "button";
      const traceEdge = recordingFrameTraceEdge(frames, idx);
      button.className = [
        "process-row",
        recordingEventIsError(event) ? "error" : "",
        traceEdge && edgeIsSelected(traceEdge, "trace") ? "active" : ""
      ].filter(Boolean).join(" ");
      button.title = stackTitle;
      const target = liveFrameTarget(frame);
      button.disabled = !target;
      if (target) {
        button.addEventListener("click", () => {
          selectNode(target, { manual: true, edge: traceEdge || undefined });
        });
      }
      const current = document.createElement("strong");
      current.textContent = frameLabel(frame);
      const meta = document.createElement("span");
      meta.textContent = [
        `#${event.sequence || state.live.recordingFocus + 1}`,
        `frame ${idx + 1}/${frames.length}`,
        recordingEventName(event)
      ].join(" · ");
      button.append(current, meta);
      return button;
    });
    els.processPanel.replaceChildren(title, ...rows);
    return true;
  }

  function recordingFrameTraceEdge(frames, idx) {
    const target = liveFrameTarget(frames[idx]);
    if (!target) return null;
    const caller = liveFrameTarget(frames[idx + 1]);
    if (caller && caller !== target) {
      return { source: caller, target, count: 1, kind: "trace" };
    }
    const callee = liveFrameTarget(frames[idx - 1]);
    if (callee && callee !== target) {
      return { source: target, target: callee, count: 1, kind: "trace" };
    }
    return null;
  }

  function renderEventPanel() {
    const events = [...state.live.eventDeltas.entries()]
      .filter(([, delta]) => delta > 0)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 4);
    if (!events.length) {
      els.tracePanel.classList.remove("event-panel");
      els.tracePanel.replaceChildren();
      return false;
    }
    els.tracePanel.classList.add("event-panel");
    const title = document.createElement("div");
    title.className = "heat-title";
    title.textContent = "Event deltas";
    const maxDelta = Math.max(1, ...events.map(([, delta]) => delta));
    const rows = events.map(([key, delta]) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = /error|failed|warning|throw|crash|exception/i.test(key) ?
        "event-row error" :
        "event-row";
      button.title = key;
      button.addEventListener("click", () => {
        const target = liveEventTarget(key);
        if (!target) return;
        selectNode(target, { manual: true });
      });
      const name = document.createElement("strong");
      name.textContent = key;
      const meter = document.createElement("span");
      meter.className = "event-meter";
      meter.style.setProperty(
        "--event-level",
        `${Math.max(7, Math.min(100, (delta / maxDelta) * 100))}%`
      );
      const sparkline = eventSparkline(key);
      const meta = document.createElement("span");
      meta.textContent = `+${nf.format(Math.round(delta))} · ${formatEventRate(state.live.eventRates.get(key) || 0)}`;
      button.append(name, meter, sparkline, meta);
      return button;
    });
    els.tracePanel.replaceChildren(title, ...rows);
    return true;
  }

  function eventSparkline(key, count = 12) {
    const wrap = document.createElement("span");
    wrap.className = "event-sparkline";
    const byTick = new Map((state.live.eventHistory.get(key) || [])
      .map((entry) => [entry.tick, entry.amount]));
    const latest = state.live.eventTick || 0;
    const values = Array.from({ length: count }, (_, idx) => {
      const tick = latest - count + idx + 1;
      return byTick.get(tick) || 0;
    });
    const max = Math.max(1, ...values);
    values.forEach((value) => {
      const bar = document.createElement("i");
      const level = value > 0 ? Math.max(12, Math.min(100, (value / max) * 100)) : 0;
      bar.style.setProperty("--spark-level", `${level}%`);
      if (!value) bar.className = "quiet";
      wrap.append(bar);
    });
    return wrap;
  }

  function liveEventTarget(key) {
    const nodeById = new Map(state.layout.nodes.map((node) => [node.id, node]));
    for (const id of resolveLiveIds(key)) {
      const projected = liveTraceNodeIds(id, nodeById);
      if (projected.length) return projected[0];
    }
    return null;
  }

  function liveFrameTarget(frame) {
    for (const id of resolveFrameIds(frame)) {
      const projected = frameTargetForMode(id);
      if (projected) return projected;
    }
    return null;
  }

  function frameTargetForMode(id) {
    const fun = byFunction.get(id);
    const mod = byModule.get(id) || (fun && byModule.get(fun.module));
    if (!mod) return null;
    if (state.mode === "function") return fun ? fun.id : null;
    if (state.mode === "system") return systemId(mod);
    return mod.id;
  }

  function renderBands() {
    const fragment = document.createDocumentFragment();
    state.layout.bands.forEach((band) => {
      const g = svgEl("g", { class: "band" });
      decorateGroupFrame(g, band);
      g.append(svgEl("rect", {
        x: band.x,
        y: band.y,
        width: band.width,
        height: band.height,
        rx: 8
      }));
      const text = svgEl("text", { x: band.x + 12, y: band.y + 22 });
      text.textContent = band.label;
      g.append(text);
      fragment.append(g);
    });
    state.layout.modules.forEach((mod) => {
      const g = svgEl("g", { class: `module-frame ${mod.role}${mod.map ? " map-region" : ""}` });
      g.dataset.id = mod.id;
      decorateGroupFrame(g, mod);
      if (!mod.map && isDimmed(mod.id)) g.classList.add("dim");
      g.append(svgEl("rect", {
        x: mod.x,
        y: mod.y,
        width: mod.width,
        height: mod.height,
        rx: mod.map ? 18 : 8
      }));
      const text = svgEl("text", { x: mod.x + 10, y: mod.y + 21 });
      text.textContent = mod.title;
      g.append(text);
      fragment.append(g);
    });
    els.bands.replaceChildren(fragment);
  }

  function decorateGroupFrame(el, frame) {
    const nodeIds = groupFrameNodeIds(frame);
    if (frame.id) el.dataset.id = frame.id;
    el.dataset.groupKey = groupFrameKey(frame);
    if (!nodeIds.length || state.mode !== "module") return;
    el.classList.add("selectable");
    el.classList.toggle("selected-group", groupFrameIsSelected(frame));
    el.addEventListener("click", (event) => {
      event.stopPropagation();
      selectGroupFrame(frame, { manual: true });
    });
  }

  function renderEdges() {
    const fragment = document.createDocumentFragment();
    liveTraceEdges().forEach((edge) => {
      fragment.append(edgeHitPath(edge, "trace"));
      const path = svgEl("path", {
        class: `edge trace${edgeIsSelected(edge, "trace") ? " selected-edge" : ""}`,
        d: edgePath(edge)
      });
      path.style.setProperty("--trace-width", `${traceEdgeWidth(edge)}px`);
      path.dataset.source = edge.source;
      path.dataset.target = edge.target;
      fragment.append(path);
    });
    els.edges.replaceChildren(fragment);
    scheduleEdgeCanvasDraw();
  }

  function renderLiveFrame() {
    const started = performance.now();
    perfProbe.liveFrames += 1;
    const followTarget = heatFollowTarget();
    if (followTarget && followTarget !== state.selected) {
      state.selected = followTarget;
      state.relationFocus = null;
      state.groupFocus = null;
      state.focusAfterRender = followTarget;
      render();
      perfProbe.lastLiveFrameMs = performance.now() - started;
      return;
    }
    refreshLiveNodes();
    refreshMinimapLive();
    renderStats(state.visible || { modules: [], functions: [], edges: [] });
    renderTelemetryPanel();
    renderInspector();
    scheduleEdgeCanvasDraw();
    perfProbe.lastLiveFrameMs = performance.now() - started;
  }

  function refreshLiveNodes() {
    if (!state.layout || !state.layout.nodes.length) return;
    const nodeById = new Map(state.layout.nodes.map((node) => [node.id, node]));
    els.nodes.querySelectorAll(".node").forEach((el) => {
      const node = nodeById.get(el.dataset.id);
      if (!node) return;
      el.setAttribute("class", nodeClass(node));
      ["--rate-color", "--rate-width", "--rate-filter"].forEach((key) => {
        el.style.removeProperty(key);
      });
      Object.entries(rateNodeStyle(node)).forEach(([key, value]) => {
        el.style.setProperty(key, value);
      });
      const title = [...el.children].find((child) => child.tagName.toLowerCase() === "title");
      if (title) title.textContent = nodeTooltip(node);
      el.querySelectorAll(".live-ring, .live-badge").forEach((child) => child.remove());
      const liveScore = liveNodeScore(node);
      if (liveScore <= 0.6) return;
      const ring = svgEl("circle", {
        class: "live-ring",
        cx: node.width - 12,
        cy: 12,
        r: Math.min(9, 3.5 + liveScore * 0.28)
      });
      const badge = svgEl("text", {
        class: "live-badge",
        x: node.width - 24,
        y: 15,
        "text-anchor": "end"
      });
      badge.textContent = `+${nf.format(Math.round(liveScore))}`;
      const firstText = [...el.children].find((child) => child.tagName.toLowerCase() === "text");
      if (firstText) {
        el.insertBefore(ring, firstText);
        el.insertBefore(badge, firstText);
      } else {
        el.append(ring, badge);
      }
    });
  }

  function refreshMinimapLive() {
    if (!state.minimap || els.minimap.hidden) return;
    const nodeById = new Map(state.layout.nodes.map((node) => [node.id, node]));
    els.minimapNodes.querySelectorAll(".mini-node").forEach((el) => {
      const node = nodeById.get(el.dataset.id);
      if (!node) return;
      el.setAttribute("class", minimapNodeClass(node));
      const title = [...el.children].find((child) => child.tagName.toLowerCase() === "title");
      if (!title) return;
      const score = liveNodeScore(node);
      const rateScore = liveRateScore(node);
      title.textContent = state.live.rateMode && Math.abs(rateScore) > 0.05 ?
        `${node.id} (${formatRateChange(rateScore)} rate change)` :
        score > 0.6 ?
        `${node.id} (+${nf.format(Math.round(score))} ${sourceHeatLabel().toLowerCase()})` :
        node.id;
    });
  }

  function scheduleEdgeCanvasDraw() {
    if (!els.edgeCanvas) return;
    if (state.edgeDrawFrame) return;
    state.edgeDrawFrame = window.requestAnimationFrame(() => {
      state.edgeDrawFrame = null;
      drawEdgeCanvas();
    });
  }

  function drawEdgeCanvas() {
    const started = performance.now();
    const canvas = els.edgeCanvas;
    if (!canvas) return;
    const rect = els.stage.getBoundingClientRect();
    const width = Math.max(1, Math.round(rect.width));
    const height = Math.max(1, Math.round(rect.height));
    const dpr = Math.max(1, window.devicePixelRatio || 1);
    if (canvas.width !== Math.round(width * dpr) || canvas.height !== Math.round(height * dpr)) {
      canvas.width = Math.round(width * dpr);
      canvas.height = Math.round(height * dpr);
    }
    const ctx = canvas.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, width, height);
    if (!state.layout || !state.layout.edges.length) return;
    const world = canvasWorldBounds(width, height, 180);
    const scores = new Map();
    let drawn = 0;
    ctx.save();
    ctx.translate(state.transform.x, state.transform.y);
    ctx.scale(state.transform.scale, state.transform.scale);
    drawn += drawCanvasEdges(ctx, world, scores, false);
    drawn += drawCanvasEdges(ctx, world, scores, true);
    clearCanvasNodeOcclusion(ctx);
    ctx.restore();
    perfProbe.edgeDraws += 1;
    perfProbe.lastEdgeCount = drawn;
    perfProbe.lastEdgeMs = performance.now() - started;
  }

  function drawCanvasEdges(ctx, world, scores, highlightPass) {
    let drawn = 0;
    state.layout.edges.forEach((edge) => {
      const style = canvasEdgeStyle(edge, scores, highlightPass);
      if (!style) return;
      const curve = edgeCurve(edge);
      if (curveOutside(curve, world)) return;
      ctx.save();
      ctx.globalAlpha = style.alpha;
      ctx.strokeStyle = style.color;
      ctx.lineWidth = style.width;
      ctx.lineCap = "round";
      ctx.lineJoin = "round";
      ctx.setLineDash(style.dash || []);
      drawCanvasCurve(ctx, curve);
      ctx.stroke();
      if (style.arrow) drawCanvasArrow(ctx, curve, style);
      ctx.restore();
      drawn += 1;
    });
    return drawn;
  }

  function canvasEdgeStyle(edge, scores, highlightPass) {
    const focus = relationFocusId();
    const incoming = !!focus && edge.target === focus;
    const outgoing = !!focus && edge.source === focus;
    const selected = edgeIsSelected(edge, "call");
    const path = edgeIsInSelectedPath(edge);
    const liveScore = state.live.rateMode ? 0 : cachedLiveEdgeScore(edge, scores);
    const liveHot = liveScore > 7;
    const liveWarm = liveScore > 0.6;
    const groupHighlighted = groupFocusHasEdge(edge);
    const highlighted = incoming || outgoing || selected || path || liveWarm || liveHot || groupHighlighted;
    if (highlightPass && !highlighted) return null;
    const force = state.layout.force;
    const baseAlpha = force ? 0.34 : Number(edgeOpacity(edge));
    if (!highlightPass) {
      return {
        color: state.mode === "system" && edge.sourceNode.role === edge.targetNode.role ?
          "rgba(0, 0, 0, 0.09)" :
          "rgba(0, 0, 0, 0.18)",
        width: edgeWidth(edge),
        alpha: (focus && !incoming && !outgoing) || (state.groupFocus && !groupHighlighted) ?
          0.055 :
          baseAlpha,
        dash: state.mode === "system" && edge.sourceNode.role === edge.targetNode.role ? [4, 5] : null,
        arrow: !force && !focus && !state.groupFocus && state.layout.edges.length < 900
      };
    }
    if (selected) {
      return {
        color: "rgba(189, 52, 66, 0.98)",
        width: edgeWidth(edge) + 2.3,
        alpha: 1,
        arrow: true
      };
    }
    if (path) {
      return {
        color: "rgba(19, 138, 109, 0.96)",
        width: edgeWidth(edge) + 1.9,
        alpha: 0.98,
        arrow: true
      };
    }
    if (outgoing) {
      return {
        color: "rgba(19, 138, 109, 0.9)",
        width: edgeWidth(edge) + 1.2,
        alpha: 0.96,
        arrow: true
      };
    }
    if (incoming) {
      return {
        color: "rgba(178, 98, 20, 0.9)",
        width: edgeWidth(edge) + 1.2,
        alpha: 0.96,
        arrow: true
      };
    }
    if (groupHighlighted) {
      const internal = groupFocusHasNode(edge.source) && groupFocusHasNode(edge.target);
      return {
        color: internal ? "rgba(19, 138, 109, 0.82)" : "rgba(0, 154, 247, 0.74)",
        width: edgeWidth(edge) + (internal ? 1 : 0.65),
        alpha: internal ? 0.88 : 0.68,
        arrow: true
      };
    }
    if (liveHot) {
      return {
        color: "rgba(189, 52, 66, 0.86)",
        width: edgeWidth(edge) + 1.4,
        alpha: 0.86,
        dash: [8, 5],
        arrow: true
      };
    }
    if (liveWarm) {
      return {
        color: "rgba(105, 86, 197, 0.72)",
        width: edgeWidth(edge) + 0.8,
        alpha: 0.72,
        arrow: true
      };
    }
    return null;
  }

  function cachedLiveEdgeScore(edge, scores) {
    const source = cachedLiveNodeScore(edge.sourceNode, scores);
    const target = cachedLiveNodeScore(edge.targetNode, scores);
    return Math.min(source, target);
  }

  function cachedLiveNodeScore(node, scores) {
    if (!node) return 0;
    if (!scores.has(node.id)) scores.set(node.id, liveNodeScore(node));
    return scores.get(node.id);
  }

  function canvasWorldBounds(width, height, margin) {
    return {
      minX: (0 - state.transform.x) / state.transform.scale - margin,
      minY: (0 - state.transform.y) / state.transform.scale - margin,
      maxX: (width - state.transform.x) / state.transform.scale + margin,
      maxY: (height - state.transform.y) / state.transform.scale + margin
    };
  }

  function curveOutside(curve, bounds) {
    const points = curve.points.filter(Boolean);
    const minX = Math.min(...points.map((point) => point.x));
    const minY = Math.min(...points.map((point) => point.y));
    const maxX = Math.max(...points.map((point) => point.x));
    const maxY = Math.max(...points.map((point) => point.y));
    return maxX < bounds.minX || minX > bounds.maxX || maxY < bounds.minY || minY > bounds.maxY;
  }

  function drawCanvasCurve(ctx, curve) {
    const [start, c1, c2, end] = curve.points;
    ctx.beginPath();
    ctx.moveTo(start.x, start.y);
    if (curve.kind === "quadratic") {
      ctx.quadraticCurveTo(c1.x, c1.y, end.x, end.y);
    } else {
      ctx.bezierCurveTo(c1.x, c1.y, c2.x, c2.y, end.x, end.y);
    }
  }

  function drawCanvasArrow(ctx, curve, style) {
    const end = curvePointAt(curve, 1);
    const before = curvePointAt(curve, 0.985);
    const angle = Math.atan2(end.y - before.y, end.x - before.x);
    const length = Math.max(7, style.width * 3.4);
    const spread = Math.PI / 7;
    ctx.save();
    ctx.fillStyle = style.color;
    ctx.beginPath();
    ctx.moveTo(end.x, end.y);
    ctx.lineTo(
      end.x - Math.cos(angle - spread) * length,
      end.y - Math.sin(angle - spread) * length
    );
    ctx.lineTo(
      end.x - Math.cos(angle + spread) * length,
      end.y - Math.sin(angle + spread) * length
    );
    ctx.closePath();
    ctx.fill();
    ctx.restore();
  }

  function pickCallEdge(event) {
    if (!state.layout || !state.layout.edges.length) return null;
    const point = eventWorldPoint(event);
    const threshold = Math.max(10, 16 / state.transform.scale);
    let best = null;
    state.layout.edges.forEach((edge) => {
      const curve = edgeCurve(edge);
      if (pointOutsideCurve(point, curve, threshold)) return;
      const distance = curveDistance(curve, point);
      if (distance > threshold) return;
      const weight = edgeIsSelected(edge, "call") || edgeIsInSelectedPath(edge) ? 0.65 : 1;
      const score = distance * weight;
      if (!best || score < best.score) best = { edge, score };
    });
    return best && best.edge;
  }

  function eventWorldPoint(event) {
    const rect = els.svg.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const y = event.clientY - rect.top;
    return {
      x: (x - state.transform.x) / state.transform.scale,
      y: (y - state.transform.y) / state.transform.scale
    };
  }

  function pointOutsideCurve(point, curve, margin) {
    const bounds = {
      minX: point.x - margin,
      minY: point.y - margin,
      maxX: point.x + margin,
      maxY: point.y + margin
    };
    return curveOutside(curve, bounds);
  }

  function curveDistance(curve, point) {
    const steps = state.layout.force ? 14 : 18;
    let best = Infinity;
    let prev = curvePointAt(curve, 0);
    for (let idx = 1; idx <= steps; idx += 1) {
      const next = curvePointAt(curve, idx / steps);
      best = Math.min(best, pointSegmentDistance(point, prev, next));
      prev = next;
    }
    return best;
  }

  function curvePointAt(curve, t) {
    const [a, b, c, d] = curve.points;
    if (curve.kind === "quadratic") {
      const inv = 1 - t;
      return {
        x: inv * inv * a.x + 2 * inv * t * b.x + t * t * d.x,
        y: inv * inv * a.y + 2 * inv * t * b.y + t * t * d.y
      };
    }
    const inv = 1 - t;
    return {
      x: inv ** 3 * a.x + 3 * inv * inv * t * b.x + 3 * inv * t * t * c.x + t ** 3 * d.x,
      y: inv ** 3 * a.y + 3 * inv * inv * t * b.y + 3 * inv * t * t * c.y + t ** 3 * d.y
    };
  }

  function pointSegmentDistance(point, a, b) {
    const dx = b.x - a.x;
    const dy = b.y - a.y;
    const lengthSq = dx * dx + dy * dy;
    if (!lengthSq) return Math.hypot(point.x - a.x, point.y - a.y);
    const t = clamp(((point.x - a.x) * dx + (point.y - a.y) * dy) / lengthSq, 0, 1);
    const x = a.x + dx * t;
    const y = a.y + dy * t;
    return Math.hypot(point.x - x, point.y - y);
  }

  function clearCanvasNodeOcclusion(ctx) {
    if (!state.layout || !state.layout.nodes.length) return;
    ctx.save();
    ctx.globalCompositeOperation = "destination-out";
    state.layout.nodes
      .filter((node) => nodeOccludesCanvas(node))
      .forEach((node) => {
        const pad = state.layout.force ? 4 : 6;
        const radius = node.kind === "module" || node.kind === "system" ? 9 : 7;
        roundedCanvasRect(
          ctx,
          node.x - pad,
          node.y - pad,
          node.width + pad * 2,
          node.height + pad * 2,
          radius
        );
        ctx.fill();
      });
    ctx.restore();
  }

  function nodeOccludesCanvas(node) {
    if (state.groupFocus) return groupFocusHasNode(node.id);
    if (!relationFocusId() && !state.selectedEdge && !selectedPathHasNode(node.id)) return true;
    if (node.id === state.selected || node.id === relationFocusId()) return true;
    if (isCaller(node.id) || isCallee(node.id) || selectedPathHasNode(node.id)) return true;
    return !!state.selectedEdge &&
      (state.selectedEdge.source === node.id || state.selectedEdge.target === node.id);
  }

  function roundedCanvasRect(ctx, x, y, width, height, radius) {
    const r = Math.min(radius, width / 2, height / 2);
    ctx.beginPath();
    ctx.moveTo(x + r, y);
    ctx.lineTo(x + width - r, y);
    ctx.quadraticCurveTo(x + width, y, x + width, y + r);
    ctx.lineTo(x + width, y + height - r);
    ctx.quadraticCurveTo(x + width, y + height, x + width - r, y + height);
    ctx.lineTo(x + r, y + height);
    ctx.quadraticCurveTo(x, y + height, x, y + height - r);
    ctx.lineTo(x, y + r);
    ctx.quadraticCurveTo(x, y, x + r, y);
    ctx.closePath();
  }

  function edgeHitPath(edge, kind = "call") {
    const hit = svgEl("path", { class: "edge-hit", d: edgePath(edge) });
    hit.dataset.source = edge.source;
    hit.dataset.target = edge.target;
    hit.addEventListener("click", (event) => {
      event.stopPropagation();
      selectNode(edge.target, {
        manual: true,
        edge: {
          source: edge.source,
          target: edge.target,
          count: edge.count || 1,
          kind
        }
      });
    });
    return hit;
  }

  function liveTraceEdges() {
    if (!state.live.enabled || !state.live.traceEdges.size) return [];
    const nodeById = new Map(state.layout.nodes.map((node) => [node.id, node]));
    const folded = new Map();
    state.live.traceEdges.forEach((trace) => {
      const sources = liveTraceNodeIds(trace.source, nodeById);
      const targets = liveTraceNodeIds(trace.target, nodeById);
      sources.forEach((sourceId) => {
        targets.forEach((targetId) => {
          if (sourceId === targetId) return;
          const key = `${sourceId}->${targetId}`;
          const edge = folded.get(key) || {
            source: sourceId,
            target: targetId,
            sourceNode: nodeById.get(sourceId),
            targetNode: nodeById.get(targetId),
            count: 0,
            path: [],
            pathWeight: 0
          };
          edge.count += trace.count;
          if (
            Array.isArray(trace.path) &&
            trace.path.length &&
            (!edge.path.length || trace.count >= edge.pathWeight)
          ) {
            edge.path = trace.path;
            edge.pathWeight = trace.count;
          }
          folded.set(key, edge);
        });
      });
    });
    return [...folded.values()]
      .filter((edge) => edge.sourceNode && edge.targetNode)
      .sort((a, b) => b.count - a.count)
      .slice(0, 90)
      .sort((a, b) => a.count - b.count);
  }

  function liveTraceNodeIds(id, nodeById) {
    if (nodeById.has(id)) return [id];
    const fun = byFunction.get(id);
    if (fun) {
      return liveTraceModuleIds(fun.module, nodeById);
    }
    return liveTraceModuleIds(id, nodeById);
  }

  function liveTraceModuleIds(id, nodeById) {
    const mod = byModule.get(id);
    if (!mod) return [];
    if (nodeById.has(mod.id)) return [mod.id];
    const sysId = systemId(mod);
    if (nodeById.has(sysId)) return [sysId];
    return [];
  }

  function nodeMatchesSearch(node) {
    if (!state.search) return false;
    if (node.kind === "system") {
      return (node.moduleIds || []).some((moduleId) => {
        const mod = byModule.get(moduleId);
        return mod && moduleMatchesSearch(mod);
      });
    }
    if (node.kind === "module") {
      const mod = byModule.get(node.id);
      return mod ? moduleMatchesSearch(mod) : false;
    }
    const fun = byFunction.get(node.id);
    return fun ? functionSearchText(fun).includes(state.search) : false;
  }

  function moduleMatchesSearch(mod) {
    if (moduleSearchText(mod).includes(state.search)) return true;
    return (functionsByModule.get(mod.id) || [])
      .some((fun) => functionSearchText(fun).includes(state.search));
  }

  function traceEdgeWidth(edge) {
    return Math.min(6.5, 1.7 + Math.log1p(edge.count || 1) * 0.58);
  }

  function edgeWidth(edge) {
    if (state.layout.force) {
      return Math.min(2.3, 0.55 + Math.log1p(edge.count || 1) * 0.2);
    }
    return Math.min(4.2, 0.9 + Math.log1p(edge.count || 1) * 0.36);
  }

  function edgeOpacity(edge) {
    if (state.mode === "system" && edge.sourceNode.role === edge.targetNode.role) {
      return "0.72";
    }
    return "1";
  }

  function renderNodes() {
    const fragment = document.createDocumentFragment();
    state.layout.nodes.forEach((node) => {
      const g = svgEl("g", {
        class: nodeClass(node),
        transform: `translate(${node.x},${node.y})`
      });
      g.dataset.id = node.id;
      g.addEventListener("click", (event) => {
        event.stopPropagation();
        if (event.detail >= 2) {
          drillIntoNode(node);
          return;
        }
        if (state.selected === node.id && state.relationFocus === node.id) {
          clearSelectedNode();
          return;
        }
        selectNode(node.id, { manual: true, relationFocus: true, focus: false });
      });
      g.addEventListener("dblclick", (event) => {
        event.stopPropagation();
        drillIntoNode(node);
      });
      g.addEventListener("mouseenter", () => setHoveredNode(node.id));
      g.addEventListener("mouseleave", () => setHoveredNode(null));
      g.addEventListener("pointerenter", () => setHoveredNode(node.id));
      g.addEventListener("pointerleave", () => setHoveredNode(null));
      Object.entries(rateNodeStyle(node)).forEach(([key, value]) => {
        g.style.setProperty(key, value);
      });
      g.append(svgEl("rect", {
        width: node.width,
        height: node.height,
        rx: node.kind === "module" ? 7 : 5
      }));
      const liveScore = liveNodeScore(node);
      if (liveScore > 0.6) {
        const ring = svgEl("circle", {
          class: "live-ring",
          cx: node.width - 12,
          cy: 12,
          r: Math.min(9, 3.5 + liveScore * 0.28)
        });
        g.append(ring);
        const badge = svgEl("text", {
          class: "live-badge",
          x: node.width - 24,
          y: 15,
          "text-anchor": "end"
        });
        badge.textContent = `+${nf.format(Math.round(liveScore))}`;
        g.append(badge);
      }
      const hasFunctionSubtitle = node.kind === "function" && (node.lens || state.layout.force);
      const title = svgEl("text", {
        x: 9,
        y: node.kind === "module" || node.kind === "system" ? 18 :
          hasFunctionSubtitle ? 15 : 16
      });
      title.textContent = truncateText(node.title, node.width - (liveScore > 0.6 ? 56 : 18), 6.8);
      g.append(title);
      if (node.kind === "module" || node.kind === "system") {
        const sub = svgEl("text", { class: "subtext", x: 9, y: 34 });
        sub.textContent = truncateText(node.subtitle, node.width - 18, 6.2);
        g.append(sub);
      }
      if (hasFunctionSubtitle) {
        const sub = svgEl("text", { class: "subtext", x: 9, y: node.lens ? 27 : 30 });
        sub.textContent = truncateText(node.subtitle, node.width - 18, 6.2);
        g.append(sub);
      }
      if (node.kind === "system") {
        const sub2 = svgEl("text", { class: "subtext", x: 9, y: 54 });
        sub2.textContent = `${nf.format(node.exports)} exports · ${nf.format(node.loc)} LoC`;
        g.append(sub2);
      }
      fragment.append(g);
    });
    els.nodes.replaceChildren(fragment);
  }

  function drillIntoNode(node) {
    state.live.follow = false;
    state.selected = null;
    state.relationFocus = null;
    state.selectedEdge = null;
    state.selectedPath = [];
    state.hovered = null;
    if (node.kind === "system") {
      activateMode("module");
      state.group = node.id;
      els.groupFilter.value = state.group;
      state.search = "";
      els.search.value = "";
    } else if (node.kind === "module") {
      activateMode("function");
      state.group = "";
      els.groupFilter.value = "";
      state.search = node.id.toLowerCase();
      els.search.value = node.id;
    } else if (node.kind === "function") {
      const fun = byFunction.get(node.id);
      if (!fun) return;
      state.search = fun.module.toLowerCase();
      els.search.value = fun.module;
    }
    requestFit();
    render();
    showGraph();
  }

  function applyRelationClasses() {
    els.nodes.querySelectorAll(".node").forEach((el) => {
      const id = el.dataset.id;
      el.classList.toggle("hovered", state.hovered === id);
      el.classList.toggle("selected", state.selected === id);
      el.classList.toggle("group-selected", groupFocusHasNode(id));
      el.classList.toggle("caller", isCaller(id));
      el.classList.toggle("callee", isCallee(id));
      el.classList.toggle("dim", isDimmed(id));
      el.classList.toggle("path-node", selectedPathHasNode(id));
    });
    els.bands.querySelectorAll(".module-frame").forEach((el) => {
      const selectedGroup = !!state.groupFocus && state.groupFocus.id === el.dataset.groupKey;
      el.classList.toggle("dim", !selectedGroup && isDimmed(el.dataset.id));
    });
    els.bands.querySelectorAll(".band.selectable, .module-frame.selectable").forEach((el) => {
      const id = el.dataset.groupKey || el.dataset.id;
      el.classList.toggle("selected-group", !!state.groupFocus && state.groupFocus.id === id);
    });
    els.edges.querySelectorAll(".edge:not(.trace)").forEach((el) => {
      const focus = relationFocusId();
      const outgoing = !!focus && el.dataset.source === focus;
      const incoming = !!focus && el.dataset.target === focus;
      const groupHot = !!state.groupFocus &&
        (groupFocusHasNode(el.dataset.source) || groupFocusHasNode(el.dataset.target));
      el.classList.toggle("outgoing", outgoing);
      el.classList.toggle("incoming", incoming);
      el.classList.toggle("hot", outgoing || incoming || groupHot);
      el.classList.toggle("dim", (!!focus && !outgoing && !incoming) || (!!state.groupFocus && !groupHot));
    });
    scheduleEdgeCanvasDraw();
  }

  function nodeClass(node) {
    const classes = ["node", node.role, node.kind];
    if (node.exported) classes.push("exported");
    if (nodeMatchesSearch(node)) classes.push("search-match");
    if (state.selected === node.id) classes.push("selected");
    if (state.hovered === node.id) classes.push("hovered");
    if (groupFocusHasNode(node.id)) classes.push("group-selected");
    if (isCaller(node.id)) classes.push("caller");
    if (isCallee(node.id)) classes.push("callee");
    if (isDimmed(node.id)) classes.push("dim");
    if (selectedPathHasNode(node.id)) classes.push("path-node");
    const liveScore = liveNodeScore(node);
    if (state.live.rateMode) {
      const rateScore = liveRateScore(node);
      if (rateScore > 0.05) classes.push("rate-up");
      else if (rateScore < -0.05) classes.push("rate-down");
    } else if (liveScore > 7) {
      classes.push("live-hot");
    } else if (liveScore > 0.6) {
      classes.push("live-warm");
    }
    if (liveErrorScore(node) > 0.6) classes.push("live-error");
    return classes.join(" ");
  }

  function rateNodeStyle(node) {
    const score = liveRateScore(node);
    if (!score) return {};
    const magnitude = Math.min(1, Math.log1p(Math.abs(score)) / 4.4);
    const alpha = 0.36 + magnitude * 0.58;
    const width = 1.4 + magnitude * 2.6;
    const glow = 8 + magnitude * 18;
    const color = score > 0 ?
      `rgba(19, 138, 109, ${alpha})` :
      `rgba(189, 52, 66, ${alpha})`;
    const glowColor = score > 0 ?
      `rgba(19, 138, 109, ${0.12 + magnitude * 0.24})` :
      `rgba(189, 52, 66, ${0.12 + magnitude * 0.24})`;
    return {
      "--rate-color": color,
      "--rate-width": `${width}px`,
      "--rate-filter": `drop-shadow(0 0 ${glow}px ${glowColor})`
    };
  }

  function nodeTooltip(node) {
    const liveScore = liveNodeScore(node);
    const errorScore = liveErrorScore(node);
    const rateScore = liveRateScore(node);
    const aliases = eventAliasesForNode(node).slice(0, 4);
    const liveLine = liveScore > 0.6 ?
      `\n${sourceHeatLabel().toLowerCase()} +${Math.round(liveScore)}` :
      "";
    const rateLine = Math.abs(rateScore) > 0.05 ?
      `\nrate change ${formatRateChange(rateScore)}` :
      "";
    const errorLine = errorScore > 0.6 ? `\nerror heat +${Math.round(errorScore)}` : "";
    const aliasLine = aliases.length ? `\nevents ${aliases.join(", ")}` : "";
    if (node.kind === "system") {
      return `${node.title}\n${node.modules} modules\n${node.functions} functions${liveLine}${rateLine}${errorLine}${aliasLine}`;
    }
    if (node.kind === "module") {
      const namespace = node.namespace ? `\n${node.namespace}` : "";
      return `${node.id}\n${node.path}${namespace}\n${node.functions} functions${liveLine}${rateLine}${errorLine}${aliasLine}`;
    }
    const namespace = node.namespace ? `\n${node.namespace}` : "";
    return `${node.id}\n${node.path}:${node.line}${namespace}${liveLine}${rateLine}${errorLine}${aliasLine}`;
  }

  function truncateText(value, width, charWidth) {
    const text = String(value || "");
    const max = Math.max(4, Math.floor(width / charWidth));
    if (text.length <= max) return text;
    return `${text.slice(0, Math.max(1, max - 3))}...`;
  }

  function edgeClass(edge) {
    const classes = ["edge"];
    if (state.mode === "system" && edge.sourceNode.role === edge.targetNode.role) {
      classes.push("internal");
    }
    const focus = relationFocusId();
    if (focus && edge.source === focus) classes.push("outgoing");
    if (focus && edge.target === focus) classes.push("incoming");
    if (focus && (edge.source === focus || edge.target === focus)) {
      classes.push("hot");
    }
    if (focus && edge.source !== focus && edge.target !== focus) {
      classes.push("dim");
    }
    if (groupFocusHasEdge(edge)) classes.push("hot");
    if (state.groupFocus && !groupFocusHasEdge(edge)) classes.push("dim");
    const liveScore = liveEdgeScore(edge);
    if (!state.live.rateMode && liveScore > 7) classes.push("live-hot");
    else if (!state.live.rateMode && liveScore > 0.6) classes.push("live-warm");
    if (edgeIsSelected(edge, "call")) classes.push("selected-edge");
    if (edgeIsInSelectedPath(edge)) classes.push("path-edge");
    return classes.join(" ");
  }

  function selectedPathHasNode(id) {
    return Array.isArray(state.selectedPath) && state.selectedPath.includes(id);
  }

  function edgeIsInSelectedPath(edge) {
    if (!Array.isArray(state.selectedPath) || state.selectedPath.length < 2) return false;
    for (let idx = 0; idx < state.selectedPath.length - 1; idx += 1) {
      if (state.selectedPath[idx] === edge.source && state.selectedPath[idx + 1] === edge.target) {
        return true;
      }
    }
    return false;
  }

  function edgeIsSelected(edge, kind) {
    return !!state.selectedEdge &&
      state.selectedEdge.kind === kind &&
      state.selectedEdge.source === edge.source &&
      state.selectedEdge.target === edge.target;
  }

  function edgePath(edge) {
    const curve = edgeCurve(edge);
    const [start, c1, c2, end] = curve.points;
    if (curve.kind === "quadratic") {
      return `M ${start.x} ${start.y} Q ${c1.x} ${c1.y} ${end.x} ${end.y}`;
    }
    return `M ${start.x} ${start.y} C ${c1.x} ${c1.y}, ${c2.x} ${c2.y}, ${end.x} ${end.y}`;
  }

  function edgeCurve(edge) {
    const s = edge.sourceNode;
    const t = edge.targetNode;
    if (state.layout.force) return forceEdgePath(edge);
    const x1 = s.x + s.width;
    const y1 = s.cy;
    const y2 = t.cy;
    if (t.x <= s.x) {
      const x2 = t.x + t.width;
      const gutter = Math.max(x1, x2) + 34 + Math.min(92, Math.abs(y2 - y1) * 0.16);
      return {
        kind: "cubic",
        points: [
          { x: x1, y: y1 },
          { x: gutter, y: y1 },
          { x: gutter, y: y2 },
          { x: x2, y: y2 }
        ]
      };
    }
    const x2 = t.x;
    const dx = Math.max(70, Math.abs(x2 - x1) * 0.45);
    return {
      kind: "cubic",
      points: [
        { x: x1, y: y1 },
        { x: x1 + dx, y: y1 },
        { x: x2 - dx, y: y2 },
        { x: x2, y: y2 }
      ]
    };
  }

  function forceEdgePath(edge) {
    const s = edge.sourceNode;
    const t = edge.targetNode;
    const start = nodePort(s, t.cx, t.cy);
    const end = nodePort(t, s.cx, s.cy);
    const dx = end.x - start.x;
    const dy = end.y - start.y;
    const distance = Math.max(1, Math.sqrt(dx * dx + dy * dy));
    const direction = hashUnit(`${edge.source}->${edge.target}`) > 0.5 ? 1 : -1;
    const bend = Math.min(180, Math.max(28, distance * 0.15)) * direction;
    const normalX = -dy / distance;
    const normalY = dx / distance;
    const midX = (start.x + end.x) / 2 + normalX * bend;
    const midY = (start.y + end.y) / 2 + normalY * bend;
    return {
      kind: "quadratic",
      points: [start, { x: midX, y: midY }, null, end]
    };
  }

  function nodePort(node, towardX, towardY) {
    const dx = towardX - node.cx;
    const dy = towardY - node.cy;
    if (Math.abs(dx) < 0.001 && Math.abs(dy) < 0.001) {
      return { x: node.cx + node.width / 2, y: node.cy };
    }
    const scaleX = Math.abs(dx) < 0.001 ? Infinity : (node.width / 2) / Math.abs(dx);
    const scaleY = Math.abs(dy) < 0.001 ? Infinity : (node.height / 2) / Math.abs(dy);
    const scale = Math.min(scaleX, scaleY);
    return {
      x: node.cx + dx * scale,
      y: node.cy + dy * scale
    };
  }

  function isCaller(id) {
    const focus = relationFocusId();
    return !!focus && (activeIncoming().get(focus) || []).some((rel) => rel.id === id);
  }

  function isCallee(id) {
    const focus = relationFocusId();
    return !!focus && (activeOutgoing().get(focus) || []).some((rel) => rel.id === id);
  }

  function relationFocusId() {
    return state.relationFocus;
  }

  function activeIncoming() {
    if (state.mode === "system") return layoutRelationMap("target", "source");
    return state.mode === "module" ? moduleIncoming : incoming;
  }

  function activeOutgoing() {
    if (state.mode === "system") return layoutRelationMap("source", "target");
    return state.mode === "module" ? moduleOutgoing : outgoing;
  }

  function layoutRelationMap(fromKey, toKey) {
    const out = new Map();
    state.layout.edges.forEach((edge) => {
      const from = edge[fromKey];
      const to = edge[toKey];
      if (!out.has(from)) out.set(from, []);
      out.get(from).push({ id: to, edge });
    });
    return out;
  }

  function isDimmed(id) {
    if (state.groupFocus) return !groupFocusHasNode(id);
    const focus = relationFocusId();
    if (!focus || id === state.selected) return false;
    return id !== focus && !isCaller(id) && !isCallee(id);
  }

  function renderInspector() {
    const selected = selectedNode();
    els.detailEmpty.hidden = !!selected;
    els.detailView.hidden = !selected;
    if (!selected) {
      if (state.selected) state.selected = null;
      state.relationFocus = null;
      state.selectedEdge = null;
      state.selectedPath = [];
      els.selectionLabel.textContent = state.groupFocus ?
        state.groupFocus.title :
        "No selection";
      els.detailCard.replaceChildren();
      els.callers.replaceChildren();
      els.callees.replaceChildren();
      return;
    }
    els.selectionLabel.textContent = selected.id;
    els.detailCard.replaceChildren(detailCard(selected));
    relationList(els.callers, activeIncoming().get(selected.id) || [], "incoming", selected.id);
    relationList(els.callees, activeOutgoing().get(selected.id) || [], "outgoing", selected.id);
  }

  function selectedNode() {
    if (!state.selected) return null;
    if (state.mode === "system") {
      return state.layout.nodes.find((node) => node.id === state.selected) || null;
    }
    return state.mode === "module" ? byModule.get(state.selected) : byFunction.get(state.selected);
  }

  function detailCard(node) {
    const wrap = document.createElement("div");
    const title = document.createElement("div");
    title.className = "detail-title";
    title.textContent = state.mode === "module" ? node.module : node.id;
    wrap.append(title);
    if (node.doc) {
      const doc = document.createElement("div");
      doc.className = "detail-doc";
      doc.textContent = node.doc;
      wrap.append(doc);
    }
    const grid = document.createElement("div");
    grid.className = "kv-grid";
    const cells = state.mode === "system" ? [
      ["Role", node.role],
      ["Group", node.group],
      ["Modules", nf.format(node.modules)],
      ["Functions", nf.format(node.functions)],
      ["Exports", nf.format(node.exports)],
      ["LoC", nf.format(node.loc)]
    ] : state.mode === "module" ? [
      ["Role", node.role],
      ["Group", node.group],
      ["Namespace", node.namespace || ""],
      ["Category", node.category || ""],
      ["Component", node["component-kind"] || ""],
      ["Functions", nf.format(node.functions)],
      ["Exports", nf.format(node.exports)],
      ["Path", node.path],
      ["LoC", nf.format(node.loc)]
    ] : [
      ["Module", node.module],
      ["Group", node.group],
      ["Namespace", node.namespace || ""],
      ["Component", node["component-kind"] || ""],
      ["Exported", node.exported ? "yes" : "no"],
      ["Line", nf.format(node.line)],
      ["Calls out", nf.format(node["calls-out"])],
      ["Calls in", nf.format(node["calls-in"])]
    ];
    const liveScore = liveNodeScore(node);
    if (liveScore > 0.6) {
      cells.push([sourceHeatLabel(), `+${nf.format(Math.round(liveScore))}`]);
    }
    const errorScore = liveErrorScore(node);
    if (errorScore > 0.6) {
      cells.push(["Error heat", `+${nf.format(Math.round(errorScore))}`]);
    }
    cells.forEach(([key, value]) => grid.append(kv(key, value)));
    wrap.append(grid);
    if (
      state.selectedEdge &&
      (state.selectedEdge.source === node.id || state.selectedEdge.target === node.id)
    ) {
      wrap.append(selectedEdgeCard());
    }
    const devicePaths = devicePathsForNode(node);
    if (devicePaths.length) {
      wrap.append(devicePathSection(devicePaths));
    }
    const refs = node["device-refs"] || [];
    if (refs.length) {
      const pills = document.createElement("div");
      pills.className = "pill-list";
      refs.forEach((ref) => {
        const pill = document.createElement("span");
        pill.className = "ref-pill";
        pill.textContent = `~${ref}`;
        pills.append(pill);
      });
      wrap.append(pills);
    }
    const eventAliases = eventAliasesForNode(node);
    if (eventAliases.length) {
      const eventSection = document.createElement("div");
      eventSection.className = "source-section";
      const eventTitle = document.createElement("h3");
      eventTitle.textContent = "Event aliases";
      const pills = document.createElement("div");
      pills.className = "pill-list";
      eventAliases.slice(0, 24).forEach((alias) => {
        const pill = document.createElement("button");
        pill.type = "button";
        pill.className = "ref-pill event-pill";
        pill.textContent = alias;
        pill.addEventListener("click", () => {
          state.search = alias.toLowerCase();
          els.search.value = alias;
          requestFit();
          render();
          showGraph();
        });
        pills.append(pill);
      });
      eventSection.append(eventTitle, pills);
      wrap.append(eventSection);
    }
    if (isRecorderNode(node)) {
      wrap.append(recorderActions());
    }
    const liveSamples = liveSamplesForNode(node);
    const liveEvents = liveEventsForNode(node);
    const recordingHits = recordingEventsForNode(node);
    if (liveEvents.length) {
      const eventSection = document.createElement("div");
      eventSection.className = "source-section";
      const eventTitle = document.createElement("h3");
      eventTitle.textContent = "Live events";
      const eventList = document.createElement("div");
      eventList.className = "stack-list";
      const maxDelta = Math.max(1, ...liveEvents.map((event) => event.delta));
      liveEvents.forEach((event) => {
        const row = document.createElement("button");
        row.type = "button";
        row.className = /error|failed|warning|throw|crash|exception/i.test(event.key) ?
          "event-sample-row error" :
          "event-sample-row";
        const target = liveEventTarget(event.key);
        row.disabled = !target;
        row.title = event.key;
        if (target) {
          row.addEventListener("click", () => {
            selectNode(target, { manual: true, showGraph: true });
          });
        }
        const name = document.createElement("strong");
        name.textContent = event.key;
        const meter = document.createElement("span");
        meter.className = "event-meter";
        meter.style.setProperty(
          "--event-level",
          `${Math.max(7, Math.min(100, (event.delta / maxDelta) * 100))}%`
        );
        const sparkline = eventSparkline(event.key);
        const meta = document.createElement("span");
        meta.textContent = `+${nf.format(Math.round(event.delta))} recent · ${formatEventRate(event.rate)}`;
        row.append(name, meter, sparkline, meta);
        eventList.append(row);
      });
      eventSection.append(eventTitle, eventList);
      wrap.append(eventSection);
    }
    if (recordingHits.length) {
      const recordingSection = document.createElement("div");
      recordingSection.className = "source-section";
      const recordingTitle = document.createElement("h3");
      recordingTitle.textContent = "Recording events";
      const recordingList = document.createElement("div");
      recordingList.className = "stack-list";
      recordingHits.forEach(({ event, idx, matches, error }) => {
        const row = document.createElement("button");
        row.type = "button";
        row.className = [
          "event-sample-row",
          error ? "error" : "",
          state.live.recordingFocus === idx ? "active" : ""
        ].filter(Boolean).join(" ");
        row.title = `${event.topic || "recording"}/${event.name || "event"}\n${matches.join("\n")}`;
        row.addEventListener("click", () => {
          state.live.follow = false;
          stopRecordingPlayback(false);
          focusRecordingEvent(idx);
          showGraph();
        });
        const name = document.createElement("strong");
        name.textContent = `#${event.sequence || idx + 1} ${event.topic || "recording"}/${event.name || "event"}`;
        const meta = document.createElement("span");
        meta.textContent = `${matches.length} matching frames`;
        row.append(name, meta);
        if (matches.length) {
          const path = document.createElement("span");
          path.className = "stack-path";
          path.textContent = matches.join(" <- ");
          row.append(path);
        }
        recordingList.append(row);
      });
      recordingSection.append(recordingTitle, recordingList);
      wrap.append(recordingSection);
    }
    if (liveSamples.length) {
      const stackSection = document.createElement("div");
      stackSection.className = "source-section";
      const stackTitle = document.createElement("h3");
      stackTitle.textContent = "Live stacks";
      const stackList = document.createElement("div");
      stackList.className = "stack-list";
      liveSamples.forEach((sample) => {
        const row = document.createElement("button");
        row.type = "button";
        row.className = "stack-row";
        const target = liveFrameTarget(sample.currentFrame || sample.current);
        row.disabled = !target;
        if (target) {
          row.addEventListener("click", () => {
            selectNode(target, { manual: true, showGraph: true });
          });
        }
        const current = document.createElement("strong");
        current.textContent = sample.current;
        const meta = document.createElement("span");
        meta.className = "stack-meta";
        meta.textContent = [
          sample.pid,
          sample.name,
          sample.status,
          `+${nf.format(Math.round(sample.reductions))} reductions`,
          formatBytes(sample.memory),
          sample.queue ? `q ${nf.format(sample.queue)}` : ""
        ].filter(Boolean).join(" · ");
        if (Array.isArray(sample.stack) && sample.stack.length > 1) {
          row.title = sample.stack.join("\n");
        }
        row.append(current, meta);
        if (Array.isArray(sample.stack) && sample.stack.length > 1) {
          const path = document.createElement("span");
          path.className = "stack-path";
          path.textContent = sample.stack.slice(0, 6).join(" <- ");
          row.append(path);
        }
        stackList.append(row);
      });
      stackSection.append(stackTitle, stackList);
      wrap.append(stackSection);
    }
    if (state.mode === "function" && node.source) {
      const sourceSection = document.createElement("div");
      sourceSection.className = "source-section";
      const sourceTitle = document.createElement("h3");
      sourceTitle.textContent = "Source";
      const pre = document.createElement("pre");
      pre.className = "source-snippet";
      pre.textContent = node.source;
      sourceSection.append(sourceTitle, pre);
      wrap.append(sourceSection);
    }
    if (state.mode === "module") {
      const functions = (functionsByModule.get(node.id) || [])
        .slice()
        .sort((a, b) => {
          if (a.exported !== b.exported) return a.exported ? -1 : 1;
          return a.label.localeCompare(b.label);
        });
      if (functions.length) {
        const functionSection = document.createElement("div");
        functionSection.className = "source-section";
        const functionTitle = document.createElement("h3");
        functionTitle.textContent = "Functions";
        const functionList = document.createElement("div");
        functionList.className = "relation-list";
        functions.forEach((fun) => {
          const button = document.createElement("button");
          button.type = "button";
          button.textContent = fun.label;
          if (fun.exported) button.classList.add("exported");
          button.addEventListener("click", () => {
            activateMode("function");
            selectNode(fun.id, { manual: true, showGraph: true });
          });
          functionList.append(button);
        });
        functionSection.append(functionTitle, functionList);
        wrap.append(functionSection);
      }
    }
    if (state.mode === "system") {
      const moduleSection = document.createElement("div");
      moduleSection.className = "source-section";
      const moduleTitle = document.createElement("h3");
      moduleTitle.textContent = "Modules";
      const moduleList = document.createElement("div");
      moduleList.className = "relation-list";
      node.moduleIds.slice(0, 40).forEach((moduleId) => {
        const button = document.createElement("button");
        button.type = "button";
        button.textContent = moduleId;
        button.addEventListener("click", () => {
          activateMode("module");
          selectNode(moduleId, { manual: true, showGraph: true });
        });
        moduleList.append(button);
      });
      moduleSection.append(moduleTitle, moduleList);
      wrap.append(moduleSection);
    }
    return wrap;
  }

  function selectedEdgeCard() {
    const edge = state.selectedEdge;
    const section = document.createElement("div");
    section.className = "source-section";
    const heading = document.createElement("h3");
    heading.textContent = edge.kind === "trace" ? "Selected trace" : "Selected call";
    const card = document.createElement("div");
    card.className = "edge-summary";
    const route = document.createElement("strong");
    route.textContent = `${edge.source} -> ${edge.target}`;
    const meta = document.createElement("span");
    meta.textContent = edge.kind === "trace" ?
      countLabel(edge.count, "sampled frame", "sampled frames") :
      countLabel(edge.count, "call", "calls");
    const jumps = document.createElement("div");
    jumps.className = "edge-jumps";
    [
      ["Source", edge.source],
      ["Target", edge.target]
    ].forEach(([label, id]) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = id === state.selected ? "edge-jump active" : "edge-jump";
      button.title = id;
      button.addEventListener("click", () => {
        selectNode(id, { manual: true, showGraph: true, edge });
      });
      const buttonLabel = document.createElement("small");
      buttonLabel.textContent = label;
      const name = document.createElement("strong");
      name.textContent = id;
      button.append(buttonLabel, name);
      jumps.append(button);
    });
    card.append(route, meta, jumps);
    section.append(heading, card);
    return section;
  }

  function devicePathsForNode(node) {
    if (!state.selectedDevices.size || !node || !state.layout.edges.length) return [];
    const nodeById = new Map(state.layout.nodes.map((layoutNode) => [layoutNode.id, layoutNode]));
    if (!nodeById.has(node.id)) return [];
    const starts = state.layout.nodes
      .filter((layoutNode) => layoutNode.id !== node.id && isSelectedDevicePathStart(layoutNode));
    if (!starts.length) return [];
    const adjacency = new Map();
    state.layout.edges.forEach((edge) => {
      if (!nodeById.has(edge.source) || !nodeById.has(edge.target)) return;
      if (!adjacency.has(edge.source)) adjacency.set(edge.source, []);
      adjacency.get(edge.source).push({ id: edge.target, count: edge.count || 1 });
    });
    adjacency.forEach((items) => {
      items.sort((a, b) => (b.count || 0) - (a.count || 0));
    });
    const paths = [];
    starts.forEach((start) => {
      const queue = [{ ids: [start.id], count: 0 }];
      let guard = 0;
      while (queue.length && guard < 160) {
        guard += 1;
        const current = queue.shift();
        const last = current.ids[current.ids.length - 1];
        if (last === node.id) {
          paths.push(current);
          break;
        }
        if (current.ids.length >= 6) continue;
        (adjacency.get(last) || []).slice(0, 10).forEach((next) => {
          if (current.ids.includes(next.id)) return;
          queue.push({
            ids: [...current.ids, next.id],
            count: current.count + next.count
          });
        });
      }
    });
    return paths
      .sort((a, b) => (a.ids.length - b.ids.length) || (b.count - a.count))
      .slice(0, 3)
      .map((path) => ({
        ...path,
        labels: path.ids.map((id) => nodeById.get(id)?.title || id)
      }));
  }

  function isSelectedDevicePathStart(node) {
    if (state.mode === "function") {
      const mod = byModule.get(node.module);
      return !!mod && mod.role === "device" && state.selectedDevices.has(mod.device);
    }
    if (state.mode === "module") {
      return node.role === "device" && state.selectedDevices.has(node.device);
    }
    if (state.mode === "system") {
      return node.role === "device" && (node.moduleIds || [])
        .some((moduleId) => state.selectedDevices.has(byModule.get(moduleId)?.device));
    }
    return false;
  }

  function devicePathSection(paths) {
    const section = document.createElement("div");
    section.className = "source-section";
    const heading = document.createElement("h3");
    heading.textContent = "Device paths";
    const list = document.createElement("div");
    list.className = "stack-list";
    paths.forEach((path) => {
      const row = document.createElement("button");
      row.type = "button";
      row.className = pathIsSelected(path) ? "path-row active" : "path-row";
      row.title = path.ids.join(" -> ");
      row.addEventListener("click", () => {
        state.live.follow = false;
        state.relationFocus = null;
        state.groupFocus = null;
        state.selectedPath = path.ids;
        render();
        focusNode(path.ids[path.ids.length - 1]);
        showGraph();
      });
      const route = document.createElement("strong");
      route.textContent = path.labels.join(" -> ");
      const meta = document.createElement("span");
      meta.textContent = `${countLabel(Math.max(0, path.ids.length - 1), "hop", "hops")} · ${countLabel(path.count, "call", "calls")}`;
      row.append(route, meta);
      list.append(row);
    });
    section.append(heading, list);
    return section;
  }

  function pathIsSelected(path) {
    return Array.isArray(state.selectedPath) &&
      state.selectedPath.length === path.ids.length &&
      state.selectedPath.every((id, idx) => id === path.ids[idx]);
  }

  function eventAliasesForNode(node) {
    if (state.mode === "system") {
      return [...new Set((node.moduleIds || [])
        .flatMap((moduleId) => byModule.get(moduleId)?.["event-topics"] || []))]
        .sort();
    }
    if (state.mode === "module") return (node["event-topics"] || []).slice().sort();
    return (node.events || []).slice().sort();
  }

  function isRecorderNode(node) {
    return node.id === "dev_recorder" ||
      node.device === "recorder@1.0" ||
      (node["device-refs"] || []).includes("recorder@1.0");
  }

  function recorderActions() {
    const section = document.createElement("div");
    section.className = "source-section";
    const title = document.createElement("h3");
    title.textContent = "Recorder black box";
    const actions = document.createElement("div");
    actions.className = "action-row";
    [
      ["Live stacks", () => startLive(defaultStackEndpoint)],
      ["Import", openRecordingImport],
      ["Demo recording", () => applyRecordingReport(demoRecordingReport(), "demo")]
    ].forEach(([label, action]) => {
      const button = document.createElement("button");
      button.type = "button";
      button.textContent = label;
      button.addEventListener("click", () => {
        action();
        showGraph();
      });
      actions.append(button);
    });
    section.append(title, actions);
    return section;
  }

  function kv(key, value) {
    const div = document.createElement("div");
    div.className = "kv";
    const label = document.createElement("span");
    label.textContent = key;
    const strong = document.createElement("strong");
    strong.textContent = value;
    strong.title = String(value);
    div.append(label, strong);
    return div;
  }

  function countLabel(value, singular, plural) {
    const count = Math.round(Number(value) || 0);
    return `${nf.format(count)} ${count === 1 ? singular : plural}`;
  }

  function relationList(root, relations, direction, selectedId) {
    if (!relations.length) {
      const empty = document.createElement("div");
      empty.className = "device-meta";
      empty.textContent = "none";
      root.replaceChildren(empty);
      return;
    }
    const buttons = relations
      .filter((rel) => {
        if (state.mode === "system") {
          return state.layout.nodes.some((node) => node.id === rel.id);
        }
        return state.mode === "function" ? byFunction.has(rel.id) : byModule.has(rel.id);
      })
      .sort((a, b) => {
        const countDelta = (b.edge.count || 0) - (a.edge.count || 0);
        return countDelta || a.id.localeCompare(b.id);
      })
      .slice(0, 80)
      .map((rel) => {
        const button = document.createElement("button");
        button.type = "button";
        const edge = direction === "incoming" ?
          { source: rel.id, target: selectedId, count: rel.edge.count || 1, kind: "call" } :
          { source: selectedId, target: rel.id, count: rel.edge.count || 1, kind: "call" };
        if (
          state.selectedEdge &&
          state.selectedEdge.kind === "call" &&
          state.selectedEdge.source === edge.source &&
          state.selectedEdge.target === edge.target
        ) {
          button.className = "active";
        }
        button.textContent = `${rel.id} · ${nf.format(rel.edge.count || 1)}`;
        button.addEventListener("click", () => {
          selectNode(rel.id, { manual: true, showGraph: true, edge });
        });
        return button;
      });
    root.replaceChildren(...buttons);
  }

  function svgEl(name, attrs) {
    const el = document.createElementNS("http://www.w3.org/2000/svg", name);
    Object.entries(attrs || {}).forEach(([key, value]) => el.setAttribute(key, value));
    return el;
  }

  function renderMinimap() {
    const bounds = state.layout.bounds;
    if (!bounds || !state.layout.nodes.length) {
      state.minimap = null;
      els.minimap.hidden = true;
      els.minimapNodes.replaceChildren();
      return;
    }
    const width = 172;
    const height = 116;
    const pad = 8;
    const scale = Math.min(
      (width - pad * 2) / Math.max(1, bounds.width),
      (height - pad * 2) / Math.max(1, bounds.height)
    );
    state.minimap = { bounds, width, height, pad, scale };
    els.minimapSvg.setAttribute("viewBox", `0 0 ${width} ${height}`);
    const fragment = document.createDocumentFragment();
    state.layout.nodes.forEach((node) => {
      const mini = svgEl("rect", {
        class: minimapNodeClass(node),
        x: minimapX(node.x),
        y: minimapY(node.y),
        width: Math.max(2, node.width * scale),
        height: Math.max(2, node.height * scale),
        rx: 1.5
      });
      mini.dataset.id = node.id;
      fragment.append(mini);
    });
    els.minimapNodes.replaceChildren(fragment);
    els.minimap.hidden = false;
    updateMinimapView();
  }

  function minimapNodeClass(node) {
    const classes = ["mini-node", node.role || "", node.kind || ""];
    if (nodeMatchesSearch(node)) classes.push("search-match");
    const liveScore = liveNodeScore(node);
    if (state.live.rateMode) {
      const rateScore = liveRateScore(node);
      if (rateScore > 0.05) classes.push("rate-up");
      else if (rateScore < -0.05) classes.push("rate-down");
    } else if (liveScore > 7) {
      classes.push("live-hot");
    } else if (liveScore > 0.6) {
      classes.push("live-warm");
    }
    if (liveErrorScore(node) > 0.6) classes.push("live-error");
    if (selectedPathHasNode(node.id)) classes.push("path-node");
    if (node.id === state.selected) classes.push("selected");
    return classes.filter(Boolean).join(" ");
  }

  function updateMinimapView() {
    if (!state.minimap) return;
    const rect = els.stage.getBoundingClientRect();
    const left = (0 - state.transform.x) / state.transform.scale;
    const top = (0 - state.transform.y) / state.transform.scale;
    const right = (rect.width - state.transform.x) / state.transform.scale;
    const bottom = (rect.height - state.transform.y) / state.transform.scale;
    els.minimapView.setAttribute("x", minimapX(left));
    els.minimapView.setAttribute("y", minimapY(top));
    els.minimapView.setAttribute("width", Math.max(4, (right - left) * state.minimap.scale));
    els.minimapView.setAttribute("height", Math.max(4, (bottom - top) * state.minimap.scale));
  }

  function minimapX(x) {
    return state.minimap.pad + (x - state.minimap.bounds.x) * state.minimap.scale;
  }

  function minimapY(y) {
    return state.minimap.pad + (y - state.minimap.bounds.y) * state.minimap.scale;
  }

  function onMinimapClick(event) {
    if (!state.minimap) return;
    const rect = els.minimapSvg.getBoundingClientRect();
    const x = (event.clientX - rect.left) * (state.minimap.width / rect.width);
    const y = (event.clientY - rect.top) * (state.minimap.height / rect.height);
    const graphX =
      state.minimap.bounds.x + (x - state.minimap.pad) / state.minimap.scale;
    const graphY =
      state.minimap.bounds.y + (y - state.minimap.pad) / state.minimap.scale;
    const stageRect = els.stage.getBoundingClientRect();
    state.transform.x = stageRect.width / 2 - graphX * state.transform.scale;
    state.transform.y = stageRect.height / 2 - graphY * state.transform.scale;
    applyTransform();
  }

  function applyTransform() {
    const { x, y, scale } = state.transform;
    els.viewport.setAttribute("transform", `translate(${x},${y}) scale(${scale})`);
    updateMinimapView();
    scheduleEdgeCanvasDraw();
  }

  function showGraph() {
    els.graphPanel.scrollIntoView({ block: "start" });
  }

  function graphViewport() {
    const rect = els.stage.getBoundingClientRect();
    if (!rect.width || !rect.height) {
      return { x: 24, y: 24, width: 760, height: 520 };
    }
    if (window.matchMedia("(max-width: 1100px)").matches) {
      return {
        x: 24,
        y: 24,
        width: Math.max(120, rect.width - 48),
        height: Math.max(120, rect.height - 48)
      };
    }
    const contextRect = els.contextPanel.getBoundingClientRect();
    const detailRect = els.detailPanel.getBoundingClientRect();
    const liveRect = document.querySelector(".live-strip").getBoundingClientRect();
    const compressed = rect.height < 700;
    const left = Math.max(24, contextRect.right - rect.left + 24);
    const right = Math.max(24, rect.right - detailRect.left + 24);
    const top = Math.max(
      24,
      Math.min(rect.height * (compressed ? 0.34 : 0.28), liveRect.bottom - rect.top + (compressed ? 30 : 54))
    );
    const bottom = compressed ? 84 : 150;
    return {
      x: left,
      y: top,
      width: Math.max(160, rect.width - left - right),
      height: Math.max(160, rect.height - top - bottom)
    };
  }

  function fitGraph(preferReadable = false) {
    const bounds = state.layout.bounds;
    if (!bounds) return;
    const viewport = graphViewport();
    const minScale = readableScale(preferReadable);
    const fitScale = Math.min(
      viewport.width / bounds.width,
      viewport.height / bounds.height
    );
    const scale = Math.min(1.4, Math.max(minScale, fitScale));
    if (state.layout.force && scale > fitScale) {
      state.transform = {
        x: viewport.x + viewport.width / 2 - (bounds.x + bounds.width / 2) * scale,
        y: viewport.y + viewport.height / 2 - (bounds.y + bounds.height / 2) * scale,
        scale
      };
    } else {
      state.transform = {
        x: viewport.x - bounds.x * scale,
        y: viewport.y - bounds.y * scale,
        scale
      };
    }
    applyTransform();
  }

  function readableScale(preferReadable) {
    if (state.layout.force) {
      if (!preferReadable) return 0.05;
      const count = state.layout.nodes.length;
      if (count > 640) return 0.2;
      if (count > 220) return 0.32;
      return 0.48;
    }
    if (!preferReadable) {
      if (state.mode === "system") return 0.42;
      if (state.mode === "module") return 0.18;
      return 0.12;
    }
    if (state.mode === "system") return 0.52;
    if (state.mode === "module") return 0.72;
    if (functionOverviewActive()) return 0.26;
    return 0.72;
  }

  function centerNode(id) {
    const node = state.layout.nodes.find((candidate) => candidate.id === id);
    if (!node) return;
    const viewport = graphViewport();
    state.transform.x = viewport.x + viewport.width / 2 - node.cx * state.transform.scale;
    state.transform.y = viewport.y + viewport.height / 2 - node.cy * state.transform.scale;
    applyTransform();
  }

  function focusNode(id) {
    const related = new Set([id]);
    (activeIncoming().get(id) || []).forEach((rel) => related.add(rel.id));
    (activeOutgoing().get(id) || []).forEach((rel) => related.add(rel.id));
    const nodes = state.layout.nodes.filter((node) => related.has(node.id));
    if (nodes.length < 2) {
      centerNode(id);
      return;
    }
    const minX = Math.min(...nodes.map((node) => node.x));
    const minY = Math.min(...nodes.map((node) => node.y));
    const maxX = Math.max(...nodes.map((node) => node.x + node.width));
    const maxY = Math.max(...nodes.map((node) => node.y + node.height));
    const viewport = graphViewport();
    const scale = Math.min(1.15, Math.max(
      state.layout.lens ? 0.52 : readableScale(true),
      Math.min(viewport.width / (maxX - minX), viewport.height / (maxY - minY))
    ));
    state.transform = {
      x: viewport.x + viewport.width / 2 - ((minX + maxX) / 2) * scale,
      y: viewport.y + viewport.height / 2 - ((minY + maxY) / 2) * scale,
      scale
    };
    applyTransform();
  }

  function startWorkspaceResize(event) {
    state.resizing = {
      kind: event.currentTarget.dataset.splitter,
      startX: event.clientX,
      contextWidth: els.contextPanel.getBoundingClientRect().width,
      detailWidth: els.detailPanel.getBoundingClientRect().width,
      workspaceWidth: els.workspace.getBoundingClientRect().width,
      handle: event.currentTarget
    };
    event.currentTarget.classList.add("active");
    document.body.classList.add("resizing");
    event.preventDefault();
  }

  function moveWorkspaceResize(event) {
    if (!state.resizing) return;
    const dx = event.clientX - state.resizing.startX;
    const reserve = 520;
    if (state.resizing.kind === "context") {
      const max = Math.max(260, Math.min(520, state.resizing.workspaceWidth - state.resizing.detailWidth - reserve));
      setWorkspaceWidth("--context-width", clamp(state.resizing.contextWidth + dx, 220, max));
    } else {
      const max = Math.max(280, Math.min(560, state.resizing.workspaceWidth - state.resizing.contextWidth - reserve));
      setWorkspaceWidth("--detail-width", clamp(state.resizing.detailWidth - dx, 240, max));
    }
    updateMinimapView();
  }

  function endWorkspaceResize() {
    if (!state.resizing) return;
    state.resizing.handle.classList.remove("active");
    state.resizing = null;
    document.body.classList.remove("resizing");
  }

  function setWorkspaceWidth(name, width) {
    els.workspace.style.setProperty(name, `${Math.round(width)}px`);
  }

  function clamp(value, min, max) {
    return Math.max(min, Math.min(max, value));
  }

  function onWheel(event) {
    event.preventDefault();
    const oldScale = state.transform.scale;
    const minScale = state.layout.force ? 0.04 : 0.12;
    const nextScale = Math.min(2.2, Math.max(minScale, oldScale * (event.deltaY > 0 ? 0.9 : 1.1)));
    const rect = els.svg.getBoundingClientRect();
    const px = event.clientX - rect.left;
    const py = event.clientY - rect.top;
    state.transform.x = px - ((px - state.transform.x) / oldScale) * nextScale;
    state.transform.y = py - ((py - state.transform.y) / oldScale) * nextScale;
    state.transform.scale = nextScale;
    applyTransform();
  }

  function startPan(event) {
    if (event.target.closest(".node")) return;
    state.dragging = {
      x: event.clientX,
      y: event.clientY,
      tx: state.transform.x,
      ty: state.transform.y,
      moved: false
    };
    els.stage.classList.add("dragging");
  }

  function movePan(event) {
    if (!state.dragging) return;
    const dx = event.clientX - state.dragging.x;
    const dy = event.clientY - state.dragging.y;
    if (Math.abs(dx) > 3 || Math.abs(dy) > 3) state.dragging.moved = true;
    state.transform.x = state.dragging.tx + dx;
    state.transform.y = state.dragging.ty + dy;
    applyTransform();
  }

  function endPan() {
    if (state.dragging && state.dragging.moved) state.ignoreNextClick = true;
    state.dragging = null;
    els.stage.classList.remove("dragging");
  }

  function clearSelectionFromBackground(event) {
    if (state.ignoreNextClick) {
      state.ignoreNextClick = false;
      return;
    }
    if (event.target.closest(".node")) return;
    const edge = pickCallEdge(event);
    if (edge) {
      event.stopPropagation();
      selectNode(edge.target, {
        manual: true,
        focus: false,
        edge: {
          source: edge.source,
          target: edge.target,
          count: edge.count || 1,
          kind: "call"
        }
      });
      return;
    }
    if (!state.selected && !state.hovered && !state.relationFocus && !state.groupFocus) return;
    state.hovered = null;
    if (!state.selected) {
      state.relationFocus = null;
      state.groupFocus = null;
      refreshSelectionState();
      return;
    }
    state.selected = null;
    state.relationFocus = null;
    state.groupFocus = null;
    state.selectedEdge = null;
    state.selectedPath = [];
    state.detailTab = "scope";
    refreshSelectionState();
  }

  init();
}());
