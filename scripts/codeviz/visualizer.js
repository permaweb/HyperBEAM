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
  const outgoing = relationMap("source", "target");
  const incoming = relationMap("target", "source");
  const moduleOutgoing = moduleRelationMap("source-module", "target-module");
  const moduleIncoming = moduleRelationMap("target-module", "source-module");

  const state = {
    mode: "function",
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
    dragging: null
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
    const devices = (params.get("devices") || "")
      .split(",")
      .map((value) => value.trim().replace(/^~/, ""))
      .filter(Boolean);
    const knownDevices = new Set(graph.devices.map((device) => device.id));
    devices.forEach((device) => {
      if (knownDevices.has(device)) state.selectedDevices.add(device);
    });
    if (["function", "module"].includes(params.get("mode"))) {
      state.mode = params.get("mode");
    }
    if (params.has("search")) {
      state.search = params.get("search").trim().toLowerCase();
      els.search.value = params.get("search");
    }
    if (params.has("group")) state.group = params.get("group");
    if (["context", "selected", "cross"].includes(params.get("edges"))) {
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
        state.mode = button.dataset.mode;
        document.querySelectorAll("[data-mode]").forEach((el) => {
          el.classList.toggle("active", el === button);
        });
        state.selected = null;
        render();
      });
    });

    els.search.addEventListener("input", () => {
      state.search = els.search.value.trim().toLowerCase();
      render();
    });
    els.deviceSearch.addEventListener("input", () => {
      state.deviceSearch = els.deviceSearch.value.trim().toLowerCase();
      renderDevices();
    });
    els.groupFilter.addEventListener("change", () => {
      state.group = els.groupFilter.value;
      render();
    });
    els.edgeFilter.addEventListener("change", () => {
      state.edgeMode = els.edgeFilter.value;
      render();
    });
    els.showPrivate.addEventListener("change", () => {
      state.showPrivate = els.showPrivate.checked;
      render();
    });
    els.showForge.addEventListener("change", () => {
      state.showForge = els.showForge.checked;
      render();
    });
    els.clearDevices.addEventListener("click", () => {
      state.selectedDevices.clear();
      state.selected = null;
      renderDevices();
      render();
    });
    els.allDevices.addEventListener("click", () => {
      graph.devices.forEach((device) => state.selectedDevices.add(device.id));
      renderDevices();
      render();
    });
    els.fitGraph.addEventListener("click", fitGraph);
    els.resetGraph.addEventListener("click", () => {
      state.transform = { x: 40, y: 40, scale: 1 };
      applyTransform();
    });
    els.svg.addEventListener("wheel", onWheel, { passive: false });
    els.svg.addEventListener("pointerdown", startPan);
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
        renderGroupChips();
        renderDevices();
        render();
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
      renderDevices();
      render();
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
    const functions = graph.functions.filter((fun) => {
      if (!activeModules.has(fun.module)) return false;
      if (!state.showPrivate && !fun.exported) return false;
      if (groupFilter && `${fun.role}:${fun.group}` !== groupFilter) return false;
      if (!needle) return true;
      return `${fun.id} ${fun.path} ${fun.doc} ${(fun["device-refs"] || []).join(" ")}`
        .toLowerCase()
        .includes(needle);
    });
    const functionIds = new Set(functions.map((fun) => fun.id));
    const modules = graph.modules.filter((mod) => {
      if (!activeModules.has(mod.id)) return false;
      if (groupFilter && `${mod.role}:${mod.group}` !== groupFilter) return false;
      if (!needle) return true;
      return `${mod.id} ${mod.path} ${mod.doc} ${(mod["device-refs"] || []).join(" ")}`
        .toLowerCase()
        .includes(needle) ||
        functions.some((fun) => fun.module === mod.id);
    });
    const moduleIds = new Set(modules.map((mod) => mod.id));
    let edges = graph.edges.filter((edge) => {
      if (state.mode === "module") {
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
        if (state.mode === "module") {
          return edge["source-module"] === selected || edge["target-module"] === selected;
        }
        return edge.source === selected || edge.target === selected;
      });
    }
    return { modules, functions, edges };
  }

  function layout(visible) {
    const nodes = state.mode === "module" ? moduleGraphNodes(visible) : functionGraphNodes(visible);
    const positioned = positionNodes(nodes);
    const nodeById = new Map(positioned.nodes.map((node) => [node.id, node]));
    const edges = (state.mode === "module" ? moduleEdges(visible.edges) : visible.edges)
      .map((edge) => {
        const source = nodeById.get(edge.source);
        const target = nodeById.get(edge.target);
        if (!source || !target) return null;
        return { ...edge, sourceNode: source, targetNode: target };
      })
      .filter(Boolean);
    return { ...positioned, edges };
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
    els.graphTitle.textContent = state.selectedDevices.size ?
      "Kernel plus device context" :
      "Kernel call graph";
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
      path.dataset.source = edge.source;
      path.dataset.target = edge.target;
      fragment.append(path);
    });
    els.edges.replaceChildren(fragment);
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
      });
      g.append(svgEl("rect", {
        width: node.width,
        height: node.height,
        rx: node.kind === "module" ? 7 : 5
      }));
      const title = svgEl("text", {
        x: 9,
        y: node.kind === "module" ? 18 : 16
      });
      title.textContent = node.title;
      g.append(title);
      if (node.kind === "module") {
        const sub = svgEl("text", { class: "subtext", x: 9, y: 34 });
        sub.textContent = node.subtitle;
        g.append(sub);
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

  function edgeClass(edge) {
    const classes = ["edge"];
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
    const x2 = t.x;
    const y2 = t.cy;
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
    return state.mode === "module" ? moduleIncoming : incoming;
  }

  function activeOutgoing() {
    return state.mode === "module" ? moduleOutgoing : outgoing;
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
    const cells = state.mode === "module" ? [
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
      .filter((rel) => state.mode === "function" ? byFunction.has(rel.id) : byModule.has(rel.id))
      .slice(0, 80)
      .map((rel) => {
        const button = document.createElement("button");
        button.type = "button";
        button.textContent = rel.id;
        button.addEventListener("click", () => {
          state.selected = rel.id;
          render();
          centerNode(rel.id);
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

  function fitGraph() {
    const bounds = state.layout.bounds;
    if (!bounds) return;
    const rect = els.stage.getBoundingClientRect();
    const scale = Math.min(1.4, Math.max(0.16, Math.min(
      (rect.width - 48) / bounds.width,
      (rect.height - 48) / bounds.height
    )));
    state.transform = { x: 24, y: 24, scale };
    applyTransform();
  }

  function centerNode(id) {
    const node = state.layout.nodes.find((candidate) => candidate.id === id);
    if (!node) return;
    const rect = els.stage.getBoundingClientRect();
    state.transform.x = rect.width / 2 - node.cx * state.transform.scale;
    state.transform.y = rect.height / 2 - node.cy * state.transform.scale;
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
      ty: state.transform.y
    };
    els.stage.classList.add("dragging");
  }

  function movePan(event) {
    if (!state.dragging) return;
    state.transform.x = state.dragging.tx + event.clientX - state.dragging.x;
    state.transform.y = state.dragging.ty + event.clientY - state.dragging.y;
    applyTransform();
  }

  function endPan() {
    state.dragging = null;
    els.stage.classList.remove("dragging");
  }

  init();
}());
