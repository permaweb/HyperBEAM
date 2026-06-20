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
    allDevices: document.getElementById("all-devices"),
    search: document.getElementById("search"),
    groupFilter: document.getElementById("group-filter"),
    edgeFilter: document.getElementById("edge-filter"),
    showPrivate: document.getElementById("show-private"),
    showForge: document.getElementById("show-forge"),
    liveStatus: document.getElementById("live-status"),
    liveEndpoint: document.getElementById("live-endpoint"),
    liveFollow: document.getElementById("live-follow"),
    liveConnect: document.getElementById("live-connect"),
    liveStack: document.getElementById("live-stack"),
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
    heatPanel: document.getElementById("heat-panel"),
    tracePanel: document.getElementById("trace-panel"),
    recordingTimeline: document.getElementById("recording-timeline"),
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
  const defaultLiveEndpoint = "/~hyperbuddy@1.0/events";
  const defaultStackEndpoint = "/~recorder@1.0/live?limit=90&stack-limit=18";

  const state = {
    mode: "system",
    selectedDevices: new Set(),
    selected: null,
    search: "",
    deviceSearch: "",
    group: "",
    edgeMode: "context",
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
    live: {
      enabled: false,
      mode: "off",
      endpoint: "",
      follow: false,
      timer: null,
      previous: new Map(),
      activity: new Map(),
      errors: new Map(),
      samples: new Map(),
      traceEdges: new Map(),
      eventDeltas: new Map(),
      eventRates: new Map(),
      lastCounterAt: 0,
      totalDelta: 0,
      processCount: 0,
      frameCount: 0,
      sourceName: "",
      recordingUrl: "",
      recordingEvents: [],
      recordingFocus: -1,
      recordingPlaying: false,
      recordingTimer: null,
      pendingRecordingFocus: -1,
      lastSeen: 0,
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
    if (params.has("search")) {
      state.search = params.get("search").trim().toLowerCase();
      els.search.value = params.get("search");
    }
    if (params.has("selected")) {
      state.selected = params.get("selected");
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
    if (params.get("follow") === "heat") state.live.follow = true;
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
  }

  function liveParamValue(value) {
    if (!value || value === "true" || value === "1") return defaultLiveEndpoint;
    if (value === "stack") return defaultStackEndpoint;
    return value;
  }

  function bindEvents() {
    document.querySelectorAll("[data-mode]").forEach((button) => {
      button.addEventListener("click", () => {
        activateMode(button.dataset.mode);
        state.selected = null;
        requestFit();
        render();
      });
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
    els.liveFollow.addEventListener("click", () => {
      state.live.follow = !state.live.follow;
      if (state.live.follow) requestFit();
      render();
    });
    els.liveStack.addEventListener("click", () => startLive(defaultStackEndpoint));
    els.recordingImport.addEventListener("click", () => els.recordingFile.click());
    els.recordingFile.addEventListener("change", importRecordingFile);
    els.liveDemo.addEventListener("click", () => startLive("demo"));
    els.liveStop.addEventListener("click", stopLive);
    els.clearDevices.addEventListener("click", () => {
      state.selectedDevices.clear();
      state.selected = null;
      requestFit();
      renderDevices();
      render();
      showGraph();
    });
    els.allDevices.addEventListener("click", () => {
      graph.devices.forEach((device) => state.selectedDevices.add(device.id));
      requestFit();
      renderDevices();
      render();
      showGraph();
    });
    els.fitGraph.addEventListener("click", () => fitGraph(false));
    els.resetGraph.addEventListener("click", () => {
      state.transform = { x: 40, y: 40, scale: 1 };
      applyTransform();
    });
    els.svg.addEventListener("wheel", onWheel, { passive: false });
    els.svg.addEventListener("pointerdown", startPan);
    els.svg.addEventListener("click", clearSelectionFromBackground);
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
      button.textContent = group;
      button.addEventListener("click", () => {
        const devices = graph.devices.filter((device) => device.group === group);
        const allActive = devices.every((device) => state.selectedDevices.has(device.id));
        devices.forEach((device) => {
          if (allActive) {
            state.selectedDevices.delete(device.id);
          } else {
            state.selectedDevices.add(device.id);
          }
        });
        requestFit();
        renderGroupChips();
        renderDevices();
        render();
        showGraph();
      });
      button.classList.toggle(
        "active",
        graph.devices
          .filter((device) => device.group === group)
          .some((device) => state.selectedDevices.has(device.id))
      );
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
    els.deviceCount.textContent = `${nf.format(state.selectedDevices.size)} selected`;
    renderGroupChips();
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
    const followTarget = heatFollowTarget();
    if (followTarget && followTarget !== state.selected) {
      state.selected = followTarget;
      state.focusAfterRender = followTarget;
    }
    const visible = visibleData();
    state.layout = layout(visible);
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

  function syncUrl() {
    const params = new URLSearchParams();
    params.set("mode", state.mode);
    if (state.selectedDevices.size) {
      params.set(
        "devices",
        state.selectedDevices.size === graph.devices.length ?
          "all" :
          [...state.selectedDevices].sort().join(",")
      );
    }
    if (state.selected) params.set("selected", state.selected);
    if (state.search) params.set("search", state.search);
    if (state.group) params.set("group", state.group);
    if (state.edgeMode !== "context") params.set("edges", state.edgeMode);
    if (!state.showPrivate) params.set("private", "false");
    if (state.showForge) params.set("forge", "true");
    if (state.live.enabled && state.live.mode !== "recording") {
      params.set("live", liveUrlParam());
    }
    if (state.live.follow) params.set("follow", "heat");
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

  function visibleData() {
    const activeModules = new Set();
    graph.modules.forEach((mod) => {
      if (mod.role === "kernel") activeModules.add(mod.id);
      if (state.showForge && mod.role === "forge") activeModules.add(mod.id);
    });
    activeDeviceModules().forEach((module) => activeModules.add(module));
    const selectedFun = byFunction.get(state.selected);
    const selectedModule = byModule.get(state.selected) || (selectedFun && byModule.get(selectedFun.module));
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
      return `${fun.id} ${fun.path} ${fun.doc} ${(fun["device-refs"] || []).join(" ")} ${(fun.events || []).join(" ")}`
        .toLowerCase()
        .includes(needle);
    });
    let modules = graph.modules.filter((mod) => {
      if (!moduleInScope(mod)) return false;
      if (!needle) return true;
      return `${mod.id} ${mod.path} ${mod.doc} ${(mod["device-refs"] || []).join(" ")} ${(mod["event-topics"] || []).join(" ")}`
        .toLowerCase()
        .includes(needle) ||
        functions.some((fun) => fun.module === mod.id);
    });
    if (state.selected) {
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

  function activeDeviceModules() {
    const modules = new Set();
    const selectedGroups = new Map();
    graph.devices.forEach((device) => {
      if (state.selectedDevices.has(device.id)) {
        selectedGroups.set(device.id, device.group);
        device.modules.forEach((module) => modules.add(module));
      }
    });
    graph.modules.forEach((mod) => {
      const refs = mod["device-refs"] || [];
      if (refs.some((ref) => selectedGroups.get(ref) === mod.group)) modules.add(mod.id);
    });
    return modules;
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
    if (state.mode === "module" && byModule.has(state.selected)) {
      return selectedModuleLayout(visible);
    }
    if (state.mode === "function" && byFunction.has(state.selected)) {
      return selectedFunctionLayout(visible);
    }
    const nodes = state.mode === "module" ? moduleGraphNodes(visible) : functionGraphNodes(visible);
    const positioned = positionNodes(nodes);
    const nodeById = new Map(positioned.nodes.map((node) => [node.id, node]));
    const edges = filterLayoutEdges(state.mode === "module" ? moduleEdges(visible.edges) : visible.edges)
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
    if (mod.role === "kernel") return `kernel/${mod.group}`;
    if (mod.role === "device") return `devices/${mod.group}`;
    return `${mod.role}/${mod.group}`;
  }

  function functionGraphNodes(visible) {
    return visible.functions.map((fun) => ({
      ...fun,
      kind: "function",
      title: fun.label,
      subtitle: fun.module,
      width: 250,
      height: 24
    }));
  }

  function moduleGraphNodes(visible) {
    return visible.modules.map((mod) => ({
      ...mod,
      kind: "module",
      title: mod.module,
      subtitle: `${mod.functions} functions`,
      width: 270,
      height: 44
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
        label: roleLabels[role] || role
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
          subtitle: module.group
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
    return { nodes: placed, modules: bands, bands: columnBands(columns, maxY), bounds };
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

  function columnBands(columns, maxY) {
    return columns.map((column, idx) => ({
      x: 12 + idx * 330,
      y: 12,
      width: 306,
      height: Math.max(500, maxY),
      label: columnLabel(column)
    }));
  }

  function columnLabel(column) {
    const parts = column.split(":");
    if (parts[1] === "device") return `devices/${parts[2]}`;
    if (parts[1] === "kernel") return `kernel/${parts[2]}`;
    return `${parts[1]}/${parts[2]}`;
  }

  function startLive(endpoint) {
    stopLive({ renderAfter: false });
    const normalized = liveParamValue(endpoint);
    state.live.enabled = true;
    state.live.mode = liveModeForEndpoint(normalized);
    state.live.endpoint = normalized;
    state.live.previous = new Map();
    state.live.activity = new Map();
    state.live.errors = new Map();
    state.live.samples = new Map();
    state.live.traceEdges = new Map();
    state.live.eventDeltas = new Map();
    state.live.eventRates = new Map();
    state.live.lastCounterAt = 0;
    state.live.totalDelta = 0;
    state.live.processCount = 0;
    state.live.frameCount = 0;
    state.live.sourceName = "";
    state.live.recordingUrl = "";
    state.live.recordingEvents = [];
    state.live.recordingFocus = -1;
    state.live.pendingRecordingFocus = -1;
    state.live.lastError = "";
    state.live.demoTick = 0;
    if (normalized !== "demo") els.liveEndpoint.value = normalized;
    if (state.live.mode === "demo") {
      demoLiveTick();
      state.live.timer = window.setInterval(demoLiveTick, 1300);
    } else {
      pollLive();
      state.live.timer = window.setInterval(pollLive, 2200);
    }
    renderLiveControls();
    render();
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
    state.live.previous = new Map();
    state.live.activity = new Map();
    state.live.errors = new Map();
    state.live.samples = new Map();
    state.live.traceEdges = new Map();
    state.live.eventDeltas = new Map();
    state.live.eventRates = new Map();
    state.live.lastCounterAt = 0;
    state.live.totalDelta = 0;
    state.live.processCount = 0;
    state.live.frameCount = 0;
    state.live.sourceName = "";
    state.live.recordingUrl = "";
    state.live.recordingEvents = [];
    state.live.recordingFocus = -1;
    state.live.recordingPlaying = false;
    state.live.recordingTimer = null;
    state.live.pendingRecordingFocus = -1;
    state.live.lastError = "";
    renderLiveControls();
    if (options.renderAfter !== false) render();
  }

  async function pollLive() {
    if (!state.live.enabled || !["events", "stack"].includes(state.live.mode)) return;
    decayLiveActivity();
    try {
      const response = await fetch(state.live.endpoint, {
        cache: "no-store",
        headers: { accept: "application/json, text/plain;q=0.9, */*;q=0.8" }
      });
      const text = await response.text();
      if (!response.ok) throw new Error(`${response.status} ${response.statusText}`);
      let payload = parseLivePayload(text);
      if (linkifiedCounterPayload(payload)) {
        payload = parseLivePayload(await fetchFormattedCounters());
      }
      if (Array.isArray(payload.processes)) {
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
    render();
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
      return parsePrometheusCounters(trimmed);
    }
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
    return flattenCounters(payload).filter(({ key }) => key !== "status").length === 0;
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
      if (delta > 0) {
        totalDelta += delta;
        rememberLiveEvent(key, delta, elapsedSeconds);
        applyLiveKey(key, delta);
      }
    });
    state.live.totalDelta = totalDelta;
    state.live.processCount = 0;
    state.live.frameCount = 0;
    state.live.sourceName = "";
    state.live.samples = new Map();
  }

  function applyLiveProcesses(processes) {
    let totalDelta = 0;
    const nextSamples = new Map();
    state.live.traceEdges = new Map();
    state.live.eventDeltas = new Map();
    state.live.eventRates = new Map();
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
        entry: proc.entry || "unknown",
        current: frameLabel(proc.current),
        stack: [proc.current, ...(Array.isArray(proc.stack) ? proc.stack.slice(0, 12) : [])]
          .filter(Boolean)
          .map(frameLabel),
        status: proc.status || "unknown",
        reductions: delta,
        queue: Number(proc["message-queue-len"] || 0)
      };
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
    stopLive({ renderAfter: false });
    state.live.enabled = true;
    state.live.mode = "recording";
    state.live.endpoint = "";
    state.live.previous = new Map();
    state.live.activity = new Map();
    state.live.errors = new Map();
    state.live.samples = new Map();
    state.live.traceEdges = new Map();
    state.live.eventDeltas = new Map();
    state.live.eventRates = new Map();
    state.live.lastCounterAt = 0;
    state.live.totalDelta = 0;
    state.live.processCount = 0;
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
        bumpLive(id, amount, /error|failed|exception|crash/i.test(sample.entry));
        addLiveSample(samples, id, sample);
      });
    });
    addTraceEdgesForFrames(frames, 3.8);
    resolveFrameIds(recordingEventFrame(event)).forEach((id) => {
      bumpLive(id, 2.2, /error|failed|exception|crash/i.test(sample.entry));
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
    const resolved = frames
      .filter(Boolean)
      .map(primaryTraceIds)
      .filter((ids) => ids.length);
    for (let idx = 0; idx < resolved.length - 1; idx += 1) {
      const targets = resolved[idx];
      const sources = resolved[idx + 1];
      const weight = Math.max(0.35, amount * (1 - idx * 0.07));
      sources.forEach((source) => {
        targets.forEach((target) => addTraceEdge(source, target, weight));
      });
    }
  }

  function primaryTraceIds(frame) {
    const ids = [...resolveFrameIds(frame)];
    const functions = ids.filter((id) => byFunction.has(id));
    if (functions.length) return functions.slice(0, 3);
    return ids.filter((id) => byModule.has(id)).slice(0, 3);
  }

  function addTraceEdge(source, target, amount) {
    if (!source || !target || source === target) return;
    const key = `${source}->${target}`;
    const edge = state.live.traceEdges.get(key) || { source, target, count: 0 };
    edge.count += amount;
    state.live.traceEdges.set(key, edge);
  }

  function demoLiveTick() {
    if (!state.live.enabled || state.live.mode !== "demo") return;
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
    state.live.traceEdges = new Map();
    let totalDelta = 0;
    candidates.slice(0, 7).forEach((_, offset) => {
      const id = candidates[(tick + offset * 2) % candidates.length];
      const amount = 2 + ((tick + offset) % 5);
      totalDelta += amount;
      rememberLiveEvent(`${id}/events`, amount, 1.3);
      bumpLive(id, amount, false);
    });
    if (tick % 5 === 3) {
      bumpLive("warning/process_sampler_failed", 5, true);
      rememberLiveEvent("warning/process_sampler_failed", 5, 1.3);
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
    render();
  }

  function decayLiveActivity() {
    decayMap(state.live.activity, 0.7);
    decayMap(state.live.errors, 0.58);
    decayMap(state.live.eventDeltas, 0.66, 0.45);
    decayMap(state.live.eventRates, 0.66, 0.05);
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

  function rememberLiveEvent(key, amount, seconds = 1) {
    state.live.eventDeltas.set(key, (state.live.eventDeltas.get(key) || 0) + amount);
    if (seconds > 0) state.live.eventRates.set(key, amount / seconds);
  }

  function formatEventRate(rate) {
    if (!Number.isFinite(rate) || rate <= 0) return "0/s";
    const value = rate >= 10 ? Math.round(rate) : Math.round(rate * 10) / 10;
    return `${nf.format(value)}/s`;
  }

  function applyLiveKey(key, amount) {
    const error = /error|failed|warning|throw|crash|exception/i.test(key);
    const ids = resolveLiveIds(key);
    if (!ids.size) return;
    ids.forEach((id) => bumpLive(id, amount, error));
  }

  function bumpLive(id, amount, error) {
    const resolved = byModule.has(id) || byFunction.has(id) ? new Set([id]) : resolveLiveIds(id);
    const targets = resolved.size ? resolved : new Set([id]);
    targets.forEach((target) => {
      state.live.activity.set(target, (state.live.activity.get(target) || 0) + amount);
      if (error) state.live.errors.set(target, (state.live.errors.get(target) || 0) + amount);
    });
  }

  function resolveLiveIds(key) {
    const ids = new Set();
    const raw = String(key || "");
    const pieces = raw
      .split(/[^A-Za-z0-9_@.:'/-]+/)
      .flatMap((piece) => piece.split("/"))
      .filter(Boolean);
    [raw, ...pieces].forEach((piece) => addLiveMatches(ids, piece));
    const mfa = raw.match(/\b([a-z][A-Za-z0-9_]*)(?::|\.)([A-Za-z0-9_'-]+\/\d+)/);
    if (mfa) addLiveMatches(ids, `${mfa[1]}:${mfa[2]}`);
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
    els.context.textContent = state.selectedDevices.size ?
      `${state.selectedDevices.size} devices` :
      "kernel";
    els.graphTitle.textContent =
      state.mode === "system" ? "Subsystem flow map" :
      state.selectedDevices.size ? "Kernel plus device context" : "Kernel call graph";
    els.graphMeta.textContent = [
      `${nf.format(state.layout.edges.length)} visible calls`,
      liveMetaText()
    ].filter(Boolean).join(" · ");
    renderLiveControls();
  }

  function renderGraph() {
    renderBands();
    renderEdges();
    renderNodes();
    renderTelemetryPanel();
    renderMinimap();
    applyTransform();
  }

  function renderTelemetryPanel() {
    if (!state.live.enabled) {
      const hasBridge = renderBridgePanel();
      els.enginePanel.hidden = !hasBridge;
      return;
    }
    const hasHeat = renderHeatPanel();
    const hasTraces = renderTracePanel();
    const hasTimeline = renderRecordingTimeline();
    els.enginePanel.hidden = !hasHeat && !hasTraces && !hasTimeline;
  }

  function renderBridgePanel() {
    if (!state.selectedDevices.size) {
      els.heatPanel.replaceChildren();
      els.tracePanel.replaceChildren();
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
      button.addEventListener("click", () => {
        state.selected = edge.target;
        render();
        focusNode(edge.target);
      });
      const name = document.createElement("strong");
      name.textContent = `${edge.source} -> ${edge.target}`;
      const meta = document.createElement("span");
      meta.textContent = `${nf.format(edge.count || 1)} calls`;
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
      button.addEventListener("click", () => {
        state.selected = node.id;
        render();
        focusNode(node.id);
      });
      const name = document.createElement("strong");
      name.textContent = node.title || node.id;
      const meta = document.createElement("span");
      meta.textContent = `${nf.format(count)} bridge calls · ${node.kind}`;
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
    title.textContent = "Recording timeline";
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
    const all = document.createElement("button");
    all.type = "button";
    all.className = state.live.recordingFocus < 0 ? "recording-tick active" : "recording-tick";
    all.textContent = "All";
    all.addEventListener("click", () => {
      stopRecordingPlayback(false);
      state.live.recordingFocus = -1;
      paintRecordingEntries(recordingEntries(state.live.recordingEvents));
      render();
    });
    const ticks = state.live.recordingEvents.slice(0, 48).map((event, idx) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = state.live.recordingFocus === idx ?
        "recording-tick active" :
        "recording-tick";
      button.textContent = String(event.sequence || idx + 1);
      button.title = `${event.topic || "recording"}/${event.name || "event"}`;
      button.addEventListener("click", () => {
        stopRecordingPlayback(false);
        focusRecordingEvent(idx);
      });
      return button;
    });
    els.recordingTimeline.replaceChildren(title, play, all, ...ticks);
    els.recordingTimeline.hidden = false;
    return true;
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
      paintRecordingEntries(recordingEntries(state.live.recordingEvents));
      render();
      return;
    }
    focusRecordingEvent(next);
  }

  function focusRecordingEvent(idx) {
    const event = state.live.recordingEvents[idx];
    if (!event) return;
    state.live.recordingFocus = idx;
    paintRecordingEntries([{ event, idx }]);
    render();
  }

  function renderHeatPanel() {
    if (!state.live.enabled) {
      els.heatPanel.replaceChildren();
      return false;
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
    title.textContent =
      state.live.mode === "recording" ? "Recorded heat" :
      state.live.mode === "stack" ? "Stack heat" :
      "Live heat";
    const rows = hotNodes.map(({ node, score, errors }) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className = errors > 0.6 ? "heat-row error" : "heat-row";
      button.addEventListener("click", () => {
        state.selected = node.id;
        render();
        focusNode(node.id);
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

  function renderTracePanel() {
    if (!state.live.enabled) {
      els.tracePanel.replaceChildren();
      return false;
    }
    if (!state.live.traceEdges.size) return renderEventPanel();
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
      button.className = "trace-row";
      button.addEventListener("click", () => {
        state.selected = edge.target;
        render();
        focusNode(edge.target);
      });
      const name = document.createElement("strong");
      name.textContent = `${edge.source} -> ${edge.target}`;
      const meta = document.createElement("span");
      meta.textContent = `+${nf.format(Math.round(edge.count))} sampled frames`;
      button.append(name, meta);
      return button;
    });
    els.tracePanel.replaceChildren(title, ...rows);
    return true;
  }

  function renderEventPanel() {
    const events = [...state.live.eventDeltas.entries()]
      .filter(([, delta]) => delta > 0)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 4);
    if (!events.length) {
      els.tracePanel.replaceChildren();
      return false;
    }
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
      button.addEventListener("click", () => {
        const target = liveEventTarget(key);
        if (!target) return;
        state.selected = target;
        render();
        focusNode(target);
      });
      const name = document.createElement("strong");
      name.textContent = key;
      const meter = document.createElement("span");
      meter.className = "event-meter";
      meter.style.setProperty(
        "--event-level",
        `${Math.max(7, Math.min(100, (delta / maxDelta) * 100))}%`
      );
      const meta = document.createElement("span");
      meta.textContent = `+${nf.format(Math.round(delta))} · ${formatEventRate(state.live.eventRates.get(key) || 0)}`;
      button.append(name, meter, meta);
      return button;
    });
    els.tracePanel.replaceChildren(title, ...rows);
    return true;
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
      const g = svgEl("g", { class: `module-frame ${mod.role}` });
      if (isDimmed(mod.id)) g.classList.add("dim");
      g.append(svgEl("rect", {
        x: mod.x,
        y: mod.y,
        width: mod.width,
        height: mod.height,
        rx: 8
      }));
      const text = svgEl("text", { x: mod.x + 10, y: mod.y + 21 });
      text.textContent = mod.title;
      g.append(text);
      fragment.append(g);
    });
    els.bands.replaceChildren(fragment);
  }

  function renderEdges() {
    const fragment = document.createDocumentFragment();
    state.layout.edges.forEach((edge) => {
      const path = svgEl("path", { class: edgeClass(edge), d: edgePath(edge) });
      const width = edgeWidth(edge);
      path.style.setProperty("--edge-width", `${width}px`);
      path.style.setProperty("--edge-hot-width", `${width + 1.25}px`);
      path.style.setProperty("--edge-opacity", edgeOpacity(edge));
      path.dataset.source = edge.source;
      path.dataset.target = edge.target;
      const title = svgEl("title");
      title.textContent = `${edge.source} -> ${edge.target} (${edge.count} calls)`;
      path.append(title);
      fragment.append(path);
    });
    liveTraceEdges().forEach((edge) => {
      const path = svgEl("path", { class: "edge trace", d: edgePath(edge) });
      path.style.setProperty("--trace-width", `${traceEdgeWidth(edge)}px`);
      path.dataset.source = edge.source;
      path.dataset.target = edge.target;
      const title = svgEl("title");
      title.textContent =
        `${edge.source} -> ${edge.target} (${Math.round(edge.count)} sampled stack frames)`;
      path.append(title);
      fragment.append(path);
    });
    els.edges.replaceChildren(fragment);
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
            count: 0
          };
          edge.count += trace.count;
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

  function traceEdgeWidth(edge) {
    return Math.min(6.5, 1.7 + Math.log1p(edge.count || 1) * 0.58);
  }

  function edgeWidth(edge) {
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
        state.selected = node.id;
        render();
        focusNode(node.id);
      });
      const tooltip = svgEl("title");
      tooltip.textContent = nodeTooltip(node);
      g.append(tooltip);
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
      const title = svgEl("text", {
        x: 9,
        y: node.kind === "module" || node.kind === "system" ? 18 : 16
      });
      title.textContent = node.title;
      g.append(title);
      if (node.kind === "module" || node.kind === "system") {
        const sub = svgEl("text", { class: "subtext", x: 9, y: 34 });
        sub.textContent = node.subtitle;
        g.append(sub);
      }
      if (node.kind === "function" && node.lens) {
        const sub = svgEl("text", { class: "subtext", x: 9, y: 27 });
        sub.textContent = node.subtitle;
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

  function nodeClass(node) {
    const classes = ["node", node.role, node.kind];
    if (node.exported) classes.push("exported");
    if (state.selected === node.id) classes.push("selected");
    if (isCaller(node.id)) classes.push("caller");
    if (isCallee(node.id)) classes.push("callee");
    if (isDimmed(node.id)) classes.push("dim");
    const liveScore = liveNodeScore(node);
    if (liveScore > 7) classes.push("live-hot");
    else if (liveScore > 0.6) classes.push("live-warm");
    if (liveErrorScore(node) > 0.6) classes.push("live-error");
    return classes.join(" ");
  }

  function nodeTooltip(node) {
    const liveScore = liveNodeScore(node);
    const liveLine = liveScore > 0.6 ? `\nlive delta ${Math.round(liveScore)}` : "";
    if (node.kind === "system") {
      return `${node.title}\n${node.modules} modules\n${node.functions} functions${liveLine}`;
    }
    if (node.kind === "module") {
      return `${node.id}\n${node.path}\n${node.functions} functions${liveLine}`;
    }
    return `${node.id}\n${node.path}:${node.line}${liveLine}`;
  }

  function edgeClass(edge) {
    const classes = ["edge"];
    if (state.mode === "system" && edge.sourceNode.role === edge.targetNode.role) {
      classes.push("internal");
    }
    if (state.selected && edge.source === state.selected) classes.push("outgoing");
    if (state.selected && edge.target === state.selected) classes.push("incoming");
    if (state.selected && (edge.source === state.selected || edge.target === state.selected)) {
      classes.push("hot");
    }
    if (state.selected && edge.source !== state.selected && edge.target !== state.selected) {
      classes.push("dim");
    }
    const liveScore = liveEdgeScore(edge);
    if (liveScore > 7) classes.push("live-hot");
    else if (liveScore > 0.6) classes.push("live-warm");
    return classes.join(" ");
  }

  function edgePath(edge) {
    const s = edge.sourceNode;
    const t = edge.targetNode;
    const x1 = s.x + s.width;
    const y1 = s.cy;
    const y2 = t.cy;
    if (state.mode === "system" && t.x <= s.x) {
      const x2 = t.x + t.width;
      const gutter = x1 + 30 + Math.min(70, Math.abs(y2 - y1) * 0.18);
      return `M ${x1} ${y1} C ${gutter} ${y1}, ${gutter} ${y2}, ${x2} ${y2}`;
    }
    const x2 = t.x;
    const dx = Math.max(70, Math.abs(x2 - x1) * 0.45);
    return `M ${x1} ${y1} C ${x1 + dx} ${y1}, ${x2 - dx} ${y2}, ${x2} ${y2}`;
  }

  function isCaller(id) {
    return !!state.selected &&
      (activeIncoming().get(state.selected) || []).some((rel) => rel.id === id);
  }

  function isCallee(id) {
    return !!state.selected &&
      (activeOutgoing().get(state.selected) || []).some((rel) => rel.id === id);
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
    if (!state.selected) return false;
    return id !== state.selected && !isCaller(id) && !isCallee(id);
  }

  function renderInspector() {
    const selected = selectedNode();
    els.detailEmpty.hidden = !!selected;
    els.detailView.hidden = !selected;
    if (!selected) {
      if (state.selected) state.selected = null;
      els.selectionLabel.textContent = "No selection";
      return;
    }
    els.selectionLabel.textContent = selected.id;
    els.detailCard.replaceChildren(detailCard(selected));
    relationList(els.callers, activeIncoming().get(selected.id) || []);
    relationList(els.callees, activeOutgoing().get(selected.id) || []);
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
      ["Functions", nf.format(node.functions)],
      ["Exports", nf.format(node.exports)],
      ["Path", node.path],
      ["LoC", nf.format(node.loc)]
    ] : [
      ["Module", node.module],
      ["Group", node.group],
      ["Exported", node.exported ? "yes" : "no"],
      ["Line", nf.format(node.line)],
      ["Calls out", nf.format(node["calls-out"])],
      ["Calls in", nf.format(node["calls-in"])]
    ];
    const liveScore = liveNodeScore(node);
    if (liveScore > 0.6) {
      cells.push(["Live heat", `+${nf.format(Math.round(liveScore))}`]);
    }
    const errorScore = liveErrorScore(node);
    if (errorScore > 0.6) {
      cells.push(["Error heat", `+${nf.format(Math.round(errorScore))}`]);
    }
    cells.forEach(([key, value]) => grid.append(kv(key, value)));
    wrap.append(grid);
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
            state.selected = target;
            render();
            focusNode(target);
            showGraph();
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
        const meta = document.createElement("span");
        meta.textContent = `+${nf.format(Math.round(event.delta))} recent · ${formatEventRate(event.rate)}`;
        row.append(name, meter, meta);
        eventList.append(row);
      });
      eventSection.append(eventTitle, eventList);
      wrap.append(eventSection);
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
        const target = liveFrameTarget(sample.current);
        row.disabled = !target;
        if (target) {
          row.addEventListener("click", () => {
            state.selected = target;
            render();
            focusNode(target);
            showGraph();
          });
        }
        const current = document.createElement("strong");
        current.textContent = sample.current;
        const meta = document.createElement("span");
        meta.textContent =
          `${sample.pid} · ${sample.status} · +${nf.format(Math.round(sample.reductions))} reductions`;
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
        functions.slice(0, 80).forEach((fun) => {
          const button = document.createElement("button");
          button.type = "button";
          button.textContent = fun.label;
          if (fun.exported) button.classList.add("exported");
          button.addEventListener("click", () => {
          activateMode("function");
          state.selected = fun.id;
          render();
          focusNode(fun.id);
          showGraph();
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
          state.selected = moduleId;
          render();
          focusNode(moduleId);
          showGraph();
        });
        moduleList.append(button);
      });
      moduleSection.append(moduleTitle, moduleList);
      wrap.append(moduleSection);
    }
    return wrap;
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
      ["Import", () => els.recordingFile.click()],
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
    div.append(label, strong);
    return div;
  }

  function relationList(root, relations) {
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
        button.textContent = `${rel.id} · ${nf.format(rel.edge.count || 1)}`;
        button.addEventListener("click", () => {
          state.selected = rel.id;
          render();
          focusNode(rel.id);
          showGraph();
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
      fragment.append(svgEl("rect", {
        class: `mini-node ${node.role || ""} ${node.kind || ""}`,
        x: minimapX(node.x),
        y: minimapY(node.y),
        width: Math.max(2, node.width * scale),
        height: Math.max(2, node.height * scale),
        rx: 1.5
      }));
    });
    els.minimapNodes.replaceChildren(fragment);
    els.minimap.hidden = false;
    updateMinimapView();
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
  }

  function showGraph() {
    els.graphPanel.scrollIntoView({ block: "start" });
  }

  function fitGraph(preferReadable = false) {
    const bounds = state.layout.bounds;
    if (!bounds) return;
    const rect = els.stage.getBoundingClientRect();
    const minScale = readableScale(preferReadable);
    const scale = Math.min(1.4, Math.max(minScale, Math.min(
      (rect.width - 48) / bounds.width,
      (rect.height - 48) / bounds.height
    )));
    state.transform = { x: 24, y: 24, scale };
    applyTransform();
  }

  function readableScale(preferReadable) {
    if (!preferReadable) {
      if (state.mode === "system") return 0.42;
      if (state.mode === "module") return 0.18;
      return 0.12;
    }
    if (state.mode === "system") return 0.52;
    if (state.mode === "module") return 0.72;
    return 0.42;
  }

  function centerNode(id) {
    const node = state.layout.nodes.find((candidate) => candidate.id === id);
    if (!node) return;
    const rect = els.stage.getBoundingClientRect();
    state.transform.x = rect.width / 2 - node.cx * state.transform.scale;
    state.transform.y = rect.height / 2 - node.cy * state.transform.scale;
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
    const rect = els.stage.getBoundingClientRect();
    const scale = Math.min(1.15, Math.max(
      state.layout.lens ? 0.52 : readableScale(true),
      Math.min((rect.width - 96) / (maxX - minX), (rect.height - 96) / (maxY - minY))
    ));
    state.transform = {
      x: rect.width / 2 - ((minX + maxX) / 2) * scale,
      y: rect.height / 2 - ((minY + maxY) / 2) * scale,
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
    const nextScale = Math.min(2.2, Math.max(0.12, oldScale * (event.deltaY > 0 ? 0.9 : 1.1)));
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
    if (!state.selected || event.target.closest(".node")) return;
    state.selected = null;
    requestFit();
    render();
  }

  init();
}());
