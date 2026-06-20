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
    fitGraph: document.getElementById("fit-graph"),
    resetGraph: document.getElementById("reset-graph"),
    graphTitle: document.getElementById("graph-title"),
    graphMeta: document.getElementById("graph-meta"),
    graphPanel: document.querySelector(".graph-panel"),
    stage: document.getElementById("graph-stage"),
    svg: document.getElementById("graph"),
    viewport: document.getElementById("viewport"),
    bands: document.getElementById("bands"),
    edges: document.getElementById("edges"),
    nodes: document.getElementById("nodes"),
    detailEmpty: document.getElementById("detail-empty"),
    detailView: document.getElementById("detail-view"),
    detailCard: document.getElementById("detail-card"),
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
    fitAfterRender: true,
    ignoreNextClick: false
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

  function init() {
    applyInitialParams();
    renderGroupFilter();
    renderGroupChips();
    renderDevices();
    bindEvents();
    render();
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
    document.querySelectorAll("[data-mode]").forEach((button) => {
      button.classList.toggle("active", button.dataset.mode === state.mode);
    });
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
    window.addEventListener("pointermove", movePan);
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
      .sort((a, b) => `${a.group}:${a.label}`.localeCompare(`${b.group}:${b.label}`))
      .map((device) => deviceRow(device));
    els.deviceList.replaceChildren(...rows);
    els.deviceCount.textContent = `${nf.format(state.selectedDevices.size)} selected`;
    renderGroupChips();
  }

  function deviceRow(device) {
    const label = document.createElement("label");
    label.className = "device-row";
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
    const visible = visibleData();
    state.layout = layout(visible);
    renderStats(visible);
    renderGraph();
    renderInspector();
    syncUrl();
    if (state.fitAfterRender) {
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
    const query = params.toString();
    const next = `${window.location.pathname}${query ? `?${query}` : ""}`;
    window.history.replaceState(null, "", next);
  }

  function visibleData() {
    const activeModules = new Set();
    graph.modules.forEach((mod) => {
      if (mod.role === "kernel") activeModules.add(mod.id);
      if (state.showForge && mod.role === "forge") activeModules.add(mod.id);
    });
    graph.devices.forEach((device) => {
      if (state.selectedDevices.has(device.id)) {
        device.modules.forEach((module) => activeModules.add(module));
      }
    });

    const needle = state.search;
    const groupFilter = state.group;
    const functionInScope = (fun) => {
      if (!activeModules.has(fun.module)) return false;
      if (!state.showPrivate && !fun.exported) return false;
      if (groupFilter && `${fun.role}:${fun.group}` !== groupFilter) return false;
      return true;
    };
    const moduleInScope = (mod) => {
      if (!activeModules.has(mod.id)) return false;
      if (groupFilter && `${mod.role}:${mod.group}` !== groupFilter) return false;
      return true;
    };
    let functions = graph.functions.filter((fun) => {
      if (!functionInScope(fun)) return false;
      if (!needle) return true;
      return `${fun.id} ${fun.path} ${fun.doc} ${(fun["device-refs"] || []).join(" ")}`
        .toLowerCase()
        .includes(needle);
    });
    let modules = graph.modules.filter((mod) => {
      if (!moduleInScope(mod)) return false;
      if (!needle) return true;
      return `${mod.id} ${mod.path} ${mod.doc} ${(mod["device-refs"] || []).join(" ")}`
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
    els.graphMeta.textContent = `${nf.format(state.layout.edges.length)} visible calls`;
  }

  function renderGraph() {
    renderBands();
    renderEdges();
    renderNodes();
    applyTransform();
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
    els.edges.replaceChildren(fragment);
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
    return classes.join(" ");
  }

  function nodeTooltip(node) {
    if (node.kind === "system") {
      return `${node.title}\n${node.modules} modules\n${node.functions} functions`;
    }
    if (node.kind === "module") {
      return `${node.id}\n${node.path}\n${node.functions} functions`;
    }
    return `${node.id}\n${node.path}:${node.line}`;
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

  function applyTransform() {
    const { x, y, scale } = state.transform;
    els.viewport.setAttribute("transform", `translate(${x},${y}) scale(${scale})`);
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
