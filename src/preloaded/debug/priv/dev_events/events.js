(function () {
  const els = {
    count: document.getElementById("stat-count"),
    topics: document.getElementById("stat-topics"),
    modules: document.getElementById("stat-modules"),
    duration: document.getElementById("stat-duration"),
    visibleCount: document.getElementById("visible-count"),
    list: document.getElementById("event-list"),
    search: document.getElementById("search"),
    topic: document.getElementById("topic-filter"),
    topicOptions: document.getElementById("topic-options"),
    name: document.getElementById("name-filter"),
    nameOptions: document.getElementById("name-options"),
    module: document.getElementById("module-filter"),
    moduleOptions: document.getElementById("module-options"),
    func: document.getElementById("function-filter"),
    funcOptions: document.getElementById("function-options"),
    detailEmpty: document.getElementById("detail-empty"),
    detailView: document.getElementById("detail-view"),
    detailGrid: document.getElementById("detail-grid"),
    selectedSequence: document.getElementById("selected-sequence"),
    messageView: document.getElementById("message-view"),
    stackView: document.getElementById("stack-view"),
    embedded: document.getElementById("embedded-log")
  };

  const decoder = new TextDecoder();
  const maxRenderedEvents = 1000;
  const maxFilterOptions = 80;
  const clearFilterValue = "Clear";
  const hopHeaders = new Set([
    "cache-control",
    "codec-device",
    "content-digest",
    "content-length",
    "content-type",
    "date",
    "server",
    "signature",
    "signature-input"
  ]);

  const state = {
    report: { events: [] },
    filtered: [],
    selected: null,
    openFilter: null
  };
  const messageKeyOrder = new Map([
    "event",
    "topic",
    "name",
    "module",
    "function",
    "line",
    "stack"
  ].map((key, idx) => [key, idx]));

  function bytesToBinary(bytes) {
    const chunks = [];
    for (let idx = 0; idx < bytes.length; idx += 8192) {
      chunks.push(String.fromCharCode(...bytes.subarray(idx, idx + 8192)));
    }
    return chunks.join("");
  }

  function binaryToBytes(binary) {
    const bytes = new Uint8Array(binary.length);
    for (let idx = 0; idx < binary.length; idx += 1) {
      bytes[idx] = binary.charCodeAt(idx) & 255;
    }
    return bytes;
  }

  function base64ToBytes(value) {
    const clean = (value || "").replace(/\s+/g, "");
    if (!clean || clean.startsWith("{{")) return null;
    const binary = atob(clean);
    return binaryToBytes(binary);
  }

  function bytesToText(bytes) {
    return decoder.decode(bytes);
  }

  function decodeAoKey(key) {
    try {
      return decodeURIComponent(key);
    } catch (_err) {
      return key;
    }
  }

  function splitOutsideQuotes(input, separator) {
    const parts = [];
    let current = "";
    let quoted = false;
    let escaped = false;
    for (const ch of input) {
      if (escaped) {
        current += ch;
        escaped = false;
      } else if (ch === "\\") {
        current += ch;
        escaped = true;
      } else if (ch === "\"") {
        current += ch;
        quoted = !quoted;
      } else if (ch === separator && !quoted) {
        parts.push(current.trim());
        current = "";
      } else {
        current += ch;
      }
    }
    if (current.trim()) parts.push(current.trim());
    return parts;
  }

  function unquote(value) {
    const trimmed = (value || "").trim();
    if (trimmed.startsWith("\"") && trimmed.endsWith("\"")) {
      return trimmed.slice(1, -1).replace(/\\"/g, "\"").replace(/\\\\/g, "\\");
    }
    return trimmed;
  }

  function parseAoTypes(raw) {
    if (!raw) return {};
    const out = {};
    splitOutsideQuotes(String(raw), ",").forEach((member) => {
      const eq = member.indexOf("=");
      if (eq < 0) {
        out["."] = unquote(member);
        return;
      }
      const rawKey = member.slice(0, eq).trim();
      const valuePart = splitOutsideQuotes(member.slice(eq + 1), ";")[0].trim();
      out[decodeAoKey(rawKey)] = unquote(valuePart);
    });
    return out;
  }

  function parseHeaderBlock(block) {
    const headers = {};
    block.split(/\r\n/).forEach((line) => {
      const idx = line.indexOf(":");
      if (idx <= 0) return;
      headers[line.slice(0, idx).trim().toLowerCase()] = line.slice(idx + 1).trim();
    });
    return headers;
  }

  function contentDispositionName(headers) {
    const raw = headers["content-disposition"];
    if (!raw) return null;
    const firstSemi = raw.indexOf(";");
    const disposition = (firstSemi < 0 ? raw : raw.slice(0, firstSemi)).trim().toLowerCase();
    if (disposition === "inline") return headers["ao-body-key"] || "body";
    const params = splitOutsideQuotes(firstSemi < 0 ? "" : raw.slice(firstSemi + 1), ";");
    for (const param of params) {
      const idx = param.indexOf("=");
      if (idx < 0) continue;
      if (param.slice(0, idx).trim().toLowerCase() === "name") {
        return unquote(param.slice(idx + 1));
      }
    }
    return null;
  }

  function boundaryFromContentType(contentType) {
    const match = /(?:^|;)\s*boundary=(?:"([^"]+)"|([^;]+))/i.exec(contentType || "");
    return match ? (match[1] || match[2]).trim() : null;
  }

  function responseHeadersObject(headers) {
    const out = {};
    headers.forEach((value, key) => {
      out[key.toLowerCase()] = value;
    });
    return out;
  }

  function messageHeaders(headers) {
    const out = {};
    Object.entries(headers).forEach(([key, value]) => {
      if (!hopHeaders.has(key)) out[key] = value;
    });
    return out;
  }

  function mergeValue(existing, incoming) {
    if (
      existing &&
      incoming &&
      typeof existing === "object" &&
      typeof incoming === "object" &&
      !Array.isArray(existing) &&
      !Array.isArray(incoming)
    ) {
      return { ...existing, ...incoming };
    }
    return incoming;
  }

  function setPath(root, path, value) {
    let cursor = root;
    path.forEach((part, idx) => {
      const key = decodeAoKey(part);
      if (idx === path.length - 1) {
        cursor[key] = mergeValue(cursor[key], value);
      } else {
        if (!cursor[key] || typeof cursor[key] !== "object" || Array.isArray(cursor[key])) {
          cursor[key] = {};
        }
        cursor = cursor[key];
      }
    });
  }

  function parsePart(part) {
    const split = part.indexOf("\r\n\r\n");
    const headerText = split < 0 ? part : part.slice(0, split);
    let bodyText = split < 0 ? "" : part.slice(split + 4);
    if (bodyText.endsWith("\r\n")) bodyText = bodyText.slice(0, -2);
    const headers = parseHeaderBlock(headerText);
    const name = contentDispositionName(headers);
    if (!name) return null;

    const valueHeaders = {};
    Object.entries(headers).forEach(([key, value]) => {
      if (
        key !== "content-disposition" &&
        key !== "content-digest" &&
        key !== "signature" &&
        key !== "signature-input"
      ) {
        valueHeaders[key] = value;
      }
    });

    const bodyBytes = binaryToBytes(bodyText);
    if (bodyBytes.length === 0) return { name, value: valueHeaders };
    if (Object.keys(valueHeaders).length === 0) {
      return { name, value: bytesToText(bodyBytes) };
    }
    const bodyKey = valueHeaders["ao-body-key"] || "body";
    return {
      name,
      value: {
        ...valueHeaders,
        [bodyKey]: bytesToText(bodyBytes)
      }
    };
  }

  function parseMultipartInto(message, contentType, bodyBytes) {
    const boundary = boundaryFromContentType(contentType);
    if (!boundary) return;
    const raw = bytesToBinary(bodyBytes);
    const marker = `--${boundary}`;
    raw.split(marker).slice(1).forEach((part) => {
      if (part.startsWith("--")) return;
      const normalized = part.replace(/^\r\n/, "").replace(/\r\n$/, "");
      if (!normalized.trim()) return;
      const parsed = parsePart(normalized);
      if (!parsed) return;
      setPath(message, parsed.name.split("/"), parsed.value);
    });
  }

  function parseWire(bytes) {
    const raw = bytesToBinary(bytes);
    const split = raw.indexOf("\r\n\r\n");
    const headerText = split < 0 ? raw : raw.slice(0, split);
    const body = split < 0 ? new Uint8Array() : binaryToBytes(raw.slice(split + 4));
    return parseMessage(parseHeaderBlock(headerText), body);
  }

  function parseMessage(headers, bodyBytes) {
    const message = messageHeaders(headers);
    const contentType = headers["content-type"] || "";
    if (contentType.toLowerCase().startsWith("multipart/")) {
      parseMultipartInto(message, contentType, bodyBytes);
    } else if (bodyBytes.length > 0) {
      message[headers["ao-body-key"] || "body"] = bytesToText(bodyBytes);
    }
    return decodeAoMessage(message);
  }

  function mapToList(value) {
    const entries = Object.entries(value)
      .filter(([key]) => /^\d+$/.test(key))
      .sort((a, b) => Number(a[0]) - Number(b[0]));
    return entries.map(([, item]) => item);
  }

  function decodeTypedValue(type, value) {
    if (type === "integer") return Number(value);
    if (type === "float") return Number(value);
    if (type === "atom") {
      if (value === "true") return true;
      if (value === "false") return false;
      if (value === "undefined") return null;
      return value;
    }
    if (type === "list" && value && typeof value === "object" && !Array.isArray(value)) {
      return mapToList(value);
    }
    if (type === "empty-message") return {};
    if (type === "empty-list") return [];
    if (type === "empty-binary") return "";
    return value;
  }

  function decodeAoMessage(value) {
    if (!value || typeof value !== "object" || Array.isArray(value)) return value;
    const types = parseAoTypes(value["ao-types"]);
    const out = {};
    Object.entries(value).forEach(([rawKey, rawValue]) => {
      if (rawKey === "ao-types") return;
      const key = decodeAoKey(rawKey);
      out[key] = decodeTypedValue(types[key], decodeAoMessage(rawValue));
    });
    Object.entries(types).forEach(([key, type]) => {
      if (key !== "." && !(key in out)) out[key] = decodeTypedValue(type, "");
    });
    if (types["."] === "list") return mapToList(out);
    if (types["."] === "empty-message") return {};
    if (types["."] === "empty-list") return [];
    if (types["."] === "empty-binary") return "";
    return out;
  }

  function parseEmbedded() {
    const bytes = base64ToBytes(els.embedded.textContent || "");
    if (!bytes) return null;
    try {
      return JSON.parse(bytesToText(bytes));
    } catch (_err) {
      return parseWire(bytes);
    }
  }

  function hashSource() {
    const raw = decodeURIComponent((window.location.hash || "").slice(1).trim());
    if (!raw) return null;
    if (raw.includes("=")) {
      const params = new URLSearchParams(raw);
      return params.get("src");
    }
    return raw;
  }

  function reportSource() {
    const params = new URLSearchParams(window.location.search);
    const src = params.get("src");
    if (src) return src;
    return hashSource() || "log?format=json";
  }

  async function fetchReport() {
    const res = await fetch(reportSource(), {
      headers: {
        accept: "application/json, application/httpsig"
      }
    });
    if (!res.ok) throw new Error(`HTTP ${res.status}`);
    const body = new Uint8Array(await res.arrayBuffer());
    const headers = responseHeadersObject(res.headers);
    if ((headers["content-type"] || "").toLowerCase().includes("application/json")) {
      return JSON.parse(bytesToText(body));
    }
    return parseMessage(headers, body);
  }

  function asList(value) {
    if (Array.isArray(value)) return value;
    if (value && typeof value === "object") return mapToList(value);
    return [];
  }

  function displayValue(value) {
    if (value === undefined || value === null) return "";
    if (typeof value === "string") return formatModuleName(value);
    if (typeof value === "number" || typeof value === "boolean") return String(value);
    if (Array.isArray(value)) return `[${value.length}]`;
    return `{${Object.keys(value).length}}`;
  }

  function formatModuleName(value) {
    if (typeof value !== "string" || !value.startsWith("_hb_device_")) return value;
    const [root, ...helperParts] = value.slice("_hb_device_".length).split("__");
    const parts = root.split("_");
    if (parts.length < 2) return value;
    const hash = parts.pop();
    const device = formatDeviceName(parts.join("_"));
    const helper = helperParts.length === 0 ? "" : `/${helperParts.join("__").replace(/_/g, "-")}`;
    return `~${device}${helper}#${shortId(hash)}`;
  }

  function formatDeviceName(name) {
    const parts = name.split("_");
    if (parts.length >= 3) {
      const minor = parts[parts.length - 1];
      const major = parts[parts.length - 2];
      const base = parts.slice(0, -2).join("-");
      if (base && /^\d+$/.test(major) && /^[0-9a-z]+$/.test(minor)) {
        return `${base}@${major}.${minor}`;
      }
    }
    return name.replace(/_/g, "-");
  }

  function shortId(value) {
    return value.length > 10 ? `${value.slice(0, 10)}...` : value;
  }

  function isAtomText(value) {
    return typeof value === "string" && /^[a-z][a-z0-9_@.-]*$/.test(value);
  }

  function isTupleForm(value) {
    return Array.isArray(value) && value.length > 0 && isAtomText(value[0]);
  }

  function valueType(value, key) {
    if (isTupleForm(value)) {
      const type = `tuple (length: ${value.length - 1})`;
      const tag = scalarText(value[0]);
      return key === tag ? type : `${tag} ${type}`;
    }
    if (Array.isArray(value)) return `list (length: ${value.length})`;
    if (value === null) return "null";
    if (value && typeof value === "object") return `message (keys: ${Object.keys(value).length})`;
    return typeof value;
  }

  function searchableText(value) {
    if (value === undefined || value === null) return "";
    if (typeof value === "string" || typeof value === "number" || typeof value === "boolean") {
      const text = String(value);
      const formatted = formatModuleName(text);
      return formatted === text ? text : `${text} ${formatted}`;
    }
    if (Array.isArray(value)) return value.map(searchableText).join(" ");
    if (typeof value === "object") {
      return Object.entries(value)
        .map(([key, item]) => `${key} ${searchableText(item)}`)
        .join(" ");
    }
    return "";
  }

  function scalarText(value) {
    if (value === undefined) return "undefined";
    if (value === null) return "null";
    return formatModuleName(String(value));
  }

  function orderedEntries(value) {
    return Object.entries(value).sort(([left], [right]) => {
      const leftRank = messageKeyOrder.has(left) ? messageKeyOrder.get(left) : 100;
      const rightRank = messageKeyOrder.has(right) ? messageKeyOrder.get(right) : 100;
      if (leftRank !== rightRank) return leftRank - rightRank;
      return left.localeCompare(right);
    });
  }

  function nodeEntries(value) {
    if (!Array.isArray(value)) return orderedEntries(value);
    if (!isTupleForm(value)) return value.map((item, idx) => [String(idx + 1), item]);
    return value.slice(1).map((item, idx) => {
      if (Array.isArray(item) && item.length === 2 && isAtomText(item[0])) {
        return [scalarText(item[0]), item[1]];
      }
      return [`arg ${idx + 1}`, item];
    });
  }

  function scalarType(value, text) {
    if (typeof value === "string") return `string ${text.length}`;
    return typeof value;
  }

  function messageNode(key, value, depth) {
    if (value && typeof value === "object") {
      const details = document.createElement("details");
      details.className = "tree-node";
      details.open = depth === 0 || (depth === 1 && key === "event");

      const summary = document.createElement("summary");
      const keyEl = document.createElement("span");
      keyEl.className = "tree-key";
      keyEl.textContent = key;
      const typeEl = document.createElement("span");
      typeEl.className = "tree-type";
      typeEl.textContent = valueType(value, key);
      summary.append(keyEl, typeEl);
      details.appendChild(summary);

      const children = document.createElement("div");
      children.className = "tree-children";
      const entries = nodeEntries(value);
      if (entries.length === 0) {
        children.appendChild(messageLeaf("(empty)", ""));
      } else {
        entries.forEach(([childKey, childValue]) => {
          children.appendChild(messageNode(childKey, childValue, depth + 1));
        });
      }
      details.appendChild(children);
      return details;
    }
    return messageLeaf(key, value);
  }

  function messageLeaf(key, value) {
    const text = scalarText(value);
    if (text.length > 160 || text.includes("\n")) {
      const details = document.createElement("details");
      details.className = "tree-node scalar-node";
      const summary = document.createElement("summary");
      const keyEl = document.createElement("span");
      keyEl.className = "tree-key";
      keyEl.textContent = key;
      const typeEl = document.createElement("span");
      typeEl.className = "tree-type";
      typeEl.textContent = scalarType(value, text);
      summary.append(keyEl, typeEl);
      const valueEl = document.createElement("div");
      valueEl.className = "tree-scalar";
      valueEl.textContent = text;
      details.append(summary, valueEl);
      return details;
    }

    const row = document.createElement("div");
    row.className = "tree-leaf";
    const keyEl = document.createElement("span");
    keyEl.className = "tree-key";
    keyEl.textContent = key;
    const valueEl = document.createElement("span");
    valueEl.className = "tree-value";
    valueEl.textContent = text;
    row.append(keyEl, valueEl);
    return row;
  }

  function eventPayload(value) {
    const raw = rawMessage(value);
    if (raw && typeof raw === "object" && !Array.isArray(raw) && "event" in raw) {
      return raw.event;
    }
    return raw;
  }

  function renderMessage(value) {
    els.messageView.innerHTML = "";
    const payload = eventPayload(value);
    const key = isTupleForm(payload) ? scalarText(payload[0]) : "event";
    els.messageView.appendChild(messageNode(key, payload, 0));
  }

  function renderStack(value) {
    els.stackView.innerHTML = "";
    const raw = rawMessage(value);
    const stack = asList(raw && raw.stack);
    const root = document.createElement("details");
    root.className = "tree-node";
    root.open = true;

    const summary = document.createElement("summary");
    const keyEl = document.createElement("span");
    keyEl.className = "tree-key";
    keyEl.textContent = "stack";
    const typeEl = document.createElement("span");
    typeEl.className = "tree-type";
    typeEl.textContent = `(length: ${stack.length})`;
    summary.append(keyEl, typeEl);
    root.appendChild(summary);

    const children = document.createElement("div");
    children.className = "tree-children";
    if (stack.length === 0) {
      children.appendChild(messageLeaf("(empty)", ""));
    } else {
      stack.forEach((frame, idx) => children.appendChild(stackFrameNode(frame, idx + 1)));
    }
    root.appendChild(children);
    els.stackView.appendChild(root);
  }

  function stackFrameNode(frame, idx) {
    if (!Array.isArray(frame) || frame.length < 2) return messageNode(String(idx), frame, 1);
    const row = document.createElement("div");
    row.className = "tree-leaf";
    const parts = stackFrameParts(frame);

    const keyEl = document.createElement("span");
    keyEl.className = "tree-key";
    keyEl.textContent = `#${idx}`;

    const valueEl = document.createElement("span");
    valueEl.className = "tree-value";
    valueEl.append(
      stackFilterButton("module", parts.module),
      document.createTextNode(":"),
      stackFilterButton("function", parts.func),
      document.createTextNode(parts.arity ? `/${parts.arity}` : ""),
      document.createTextNode(parts.line ? `:${parts.line}` : "")
    );
    row.append(keyEl, valueEl);
    return row;
  }

  function stackFrameParts(frame) {
    return {
      module: scalarText(frame[0]),
      func: scalarText(frame[1]),
      arity: frame.length >= 3 ? displayValue(frame[2]) : "",
      line: stackFrameLine(frame[3])
    };
  }

  function stackFilterButton(field, value) {
    const button = document.createElement("button");
    button.className = "stack-filter";
    button.type = "button";
    button.textContent = value;
    button.title = `Filter ${field} to ${value}`;
    button.addEventListener("click", () => setStructuredFilter(field, value));
    return button;
  }

  function stackFrameLine(meta) {
    if (Array.isArray(meta)) {
      for (const item of meta) {
        if (Array.isArray(item) && item.length >= 2 && item[0] === "line") {
          return scalarText(item[1]);
        }
      }
    }
    if (meta && typeof meta === "object" && "line" in meta) return scalarText(meta.line);
    return "";
  }

  function rawMessage(event) {
    return event && event.rawMessage ? event.rawMessage : event;
  }

  function normalizeReport(report) {
    const events = asList(report && report.events);
    const normalizedEvents = events.map((event, idx) => {
      const normalized = {
        ...event,
        sequence: Number(event.sequence || idx + 1),
        time: Number.isFinite(Number(event.time)) ? Number(event.time) : null,
        topic: displayValue(event.topic || "unknown"),
        name: displayValue(event.name || "event"),
        module: displayValue(event.module || "unknown"),
        function: displayValue(event.function || "unknown"),
        line: displayValue(event.line || ""),
        stack: asList(event.stack).map(searchableText)
      };
      Object.defineProperty(normalized, "rawMessage", {
        value: event,
        enumerable: false
      });
      Object.defineProperty(normalized, "searchText", {
        value: searchableText(event),
        enumerable: false
      });
      return normalized;
    });
    const firstTime = normalizedEvents.find((event) => event.time !== null)?.time ?? null;
    let previousTime = null;
    normalizedEvents.forEach((event) => {
      if (event.time === null || firstTime === null) {
        event.offsetUs = null;
        event.deltaUs = null;
        return;
      }
      event.offsetUs = event.time - firstTime;
      event.deltaUs = previousTime === null ? 0 : event.time - previousTime;
      previousTime = event.time;
    });
    return {
      ...report,
      events: normalizedEvents
    };
  }

  async function loadInitial() {
    const embedded = parseEmbedded();
    if (window.location.hash || new URLSearchParams(window.location.search).has("src")) {
      try {
        setReport(await fetchReport());
        return;
      } catch (_err) {
        if (embedded) {
          setReport(embedded);
          return;
        }
      }
    }
    if (embedded) {
      setReport(embedded);
      return;
    }
    try {
      setReport(await fetchReport());
    } catch (_err) {
      setReport({ events: [] });
    }
  }

  function setReport(report) {
    state.report = normalizeReport(report || {});
    state.selected = state.report.events[0] || null;
    fillFilters();
    render();
  }

  function unique(field) {
    return Array.from(new Set(state.report.events.map((event) => event[field]).filter(Boolean)))
      .sort((left, right) => left.localeCompare(right));
  }

  function filterEntries() {
    return [
      { input: els.topic, options: els.topicOptions, values: unique("topic") },
      { input: els.name, options: els.nameOptions, values: unique("name") },
      { input: els.module, options: els.moduleOptions, values: unique("module") },
      { input: els.func, options: els.funcOptions, values: unique("function") }
    ];
  }

  function filterOption(label, onClick, className) {
    const option = document.createElement("button");
    option.type = "button";
    option.role = "option";
    option.className = `filter-option${className ? ` ${className}` : ""}`;
    option.textContent = label;
    option.addEventListener("mousedown", (event) => event.preventDefault());
    option.addEventListener("click", onClick);
    return option;
  }

  function fillFilterOptions({ input, options, values }) {
    const current = input.value;
    const needle = current.trim().toLowerCase();
    options.innerHTML = "";
    if (current.trim()) {
      options.appendChild(filterOption(clearFilterValue, () => {
        input.value = "";
        state.openFilter = null;
        render();
      }, "clear"));
    }
    values
      .filter((value) => !needle || value.toLowerCase().startsWith(needle))
      .slice(0, maxFilterOptions)
      .forEach((value) => {
        options.appendChild(filterOption(value, () => {
          input.value = value;
          state.openFilter = null;
          render();
        }));
      });
    input.value = current;
    options.hidden = state.openFilter !== input.id || options.childElementCount === 0;
  }

  function fillFilters() {
    filterEntries().forEach(fillFilterOptions);
  }

  function formatDurationUs(us) {
    if (!Number.isFinite(us)) return "";
    if (Math.abs(us) >= 1000000) return `${(us / 1000000).toFixed(2)}s`;
    if (Math.abs(us) >= 1000) return `${(us / 1000).toFixed(2)}ms`;
    return `${us}us`;
  }

  function reportDuration() {
    const timed = state.report.events.filter((event) => event.time !== null);
    if (timed.length < 2) return 0;
    return timed[timed.length - 1].time - timed[0].time;
  }

  function matches(event) {
    const q = els.search.value.trim().toLowerCase();
    const haystack = [
      event.topic,
      event.name,
      event.module,
      event.function,
      event.line,
      event.searchText,
      ...(event.stack || [])
    ].join(" ").toLowerCase();
    return (!q || haystack.includes(q)) &&
      fieldMatches(event.topic, els.topic.value) &&
      fieldMatches(event.name, els.name.value) &&
      fieldMatches(event.module, els.module.value) &&
      fieldMatches(event.function, els.func.value);
  }

  function fieldMatches(value, filter) {
    const needle = filter.trim().toLowerCase();
    return !needle || String(value).toLowerCase().startsWith(needle);
  }

  function filterInput(field) {
    return {
      topic: els.topic,
      name: els.name,
      module: els.module,
      function: els.func
    }[field];
  }

  function setStructuredFilter(field, value) {
    const input = filterInput(field);
    if (!input) return;
    input.value = value;
    syncFilterOptions();
    render();
  }

  function syncFilterOptions() {
    fillFilters();
  }

  function render() {
    syncFilterOptions();
    state.filtered = state.report.events.filter(matches);
    if (!state.selected && state.filtered.length > 0) {
      state.selected = state.filtered[0];
    } else if (state.selected && !state.filtered.includes(state.selected)) {
      state.selected = state.filtered[0] || null;
    }
    renderStats();
    renderList();
    renderDetail();
  }

  function renderStats() {
    els.count.textContent = state.report.events.length;
    els.topics.textContent = unique("topic").length;
    els.modules.textContent = unique("module").length;
    els.duration.textContent = formatDurationUs(reportDuration()) || "0us";
    els.visibleCount.textContent = state.filtered.length > maxRenderedEvents ?
      `${maxRenderedEvents} of ${state.filtered.length} shown` :
      `${state.filtered.length} shown`;
  }

  function renderList() {
    els.list.innerHTML = "";
    if (state.filtered.length === 0) {
      const empty = document.createElement("div");
      empty.className = "empty-list";
      empty.textContent = "No events matched";
      els.list.appendChild(empty);
      return;
    }

    const frag = document.createDocumentFragment();
    state.filtered.slice(0, maxRenderedEvents).forEach((event) => {
      const row = document.createElement("button");
      row.className = `event-row${event === state.selected ? " active" : ""}`;
      row.type = "button";
      row.setAttribute("role", "listitem");
      row.addEventListener("click", () => {
        state.selected = event;
        render();
      });

      const seq = document.createElement("div");
      seq.className = "seq";
      seq.textContent = `#${event.sequence}`;
      const time = document.createElement("span");
      time.className = "event-time";
      time.textContent = event.offsetUs === null ? "" : `+${formatDurationUs(event.offsetUs)}`;
      seq.appendChild(time);

      const main = document.createElement("div");
      main.className = "event-main";

      const title = document.createElement("div");
      title.className = "event-title";
      const topic = document.createElement("span");
      topic.className = "topic";
      topic.textContent = event.topic;
      const name = document.createElement("span");
      name.className = "event-name";
      name.textContent = event.name;
      title.append(topic, name);

      const sub = document.createElement("div");
      sub.className = "event-sub";
      sub.textContent = `${event.module}:${event.function}:${event.line}`;

      main.append(title, sub);
      row.append(seq, main);
      frag.appendChild(row);
    });
    els.list.appendChild(frag);
  }

  function renderDetail() {
    const event = state.selected;
    els.detailEmpty.hidden = !!event;
    els.detailView.hidden = !event;
    if (!event) {
      els.selectedSequence.textContent = "No selection";
      return;
    }

    els.selectedSequence.textContent = `#${event.sequence}`;
    els.detailGrid.innerHTML = "";
    detailCell("Topic", event.topic);
    detailCell("Name", event.name);
    timeCell(event);

    renderMessage(event);
    renderStack(event);
  }

  function detailCell(label, value) {
    const cell = document.createElement("div");
    cell.className = "kv";
    const key = document.createElement("span");
    key.textContent = label;
    const val = document.createElement("strong");
    val.textContent = value;
    cell.append(key, val);
    els.detailGrid.appendChild(cell);
  }

  function timeCell(event) {
    const cell = document.createElement("div");
    cell.className = "kv";
    const key = document.createElement("span");
    key.textContent = "Time";
    const val = document.createElement("strong");
    val.append(document.createTextNode(formatDurationUs(event.offsetUs)));
    if (event.deltaUs !== null) {
      const delta = document.createElement("span");
      delta.className = "time-delta";
      delta.textContent = ` (${formatSignedDurationUs(event.deltaUs)})`;
      val.appendChild(delta);
    }
    cell.append(key, val);
    els.detailGrid.appendChild(cell);
  }

  function formatSignedDurationUs(us) {
    if (!Number.isFinite(us)) return "";
    const sign = us < 0 ? "-" : "+";
    return `${sign}${formatDurationUs(Math.abs(us))}`;
  }

  function wire() {
    [els.topic, els.name, els.module, els.func].forEach((el) => {
      el.addEventListener("focus", () => {
        state.openFilter = el.id;
        syncFilterOptions();
      });
      el.addEventListener("click", () => {
        state.openFilter = el.id;
        syncFilterOptions();
      });
      el.addEventListener("input", () => {
        state.openFilter = el.id;
        render();
      });
      el.addEventListener("change", () => {
        render();
      });
      el.addEventListener("keydown", (event) => {
        if (event.key === "Escape") {
          state.openFilter = null;
          syncFilterOptions();
        }
      });
    });

    document.addEventListener("pointerdown", (event) => {
      if (!event.target.closest(".filter-combo")) {
        state.openFilter = null;
        syncFilterOptions();
      }
    });

    els.search.addEventListener("input", render);
    els.search.addEventListener("change", render);
  }

  wire();
  loadInitial();
})();
