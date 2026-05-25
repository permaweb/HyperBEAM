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
    topicSelected: document.getElementById("topic-selected"),
    topicOptions: document.getElementById("topic-options"),
    name: document.getElementById("name-filter"),
    nameSelected: document.getElementById("name-selected"),
    nameOptions: document.getElementById("name-options"),
    module: document.getElementById("module-filter"),
    moduleSelected: document.getElementById("module-selected"),
    moduleOptions: document.getElementById("module-options"),
    func: document.getElementById("function-filter"),
    funcSelected: document.getElementById("function-selected"),
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
  const numberFormatter = new Intl.NumberFormat("en-US");
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
    openFilter: null,
    stackFilter: null,
    filters: {
      topic: [],
      name: [],
      module: [],
      function: []
    }
  };
  const splitterKeyPrefix = `recorder:${window.location.pathname}:`;
  const splitters = [
    {
      el: document.querySelector("[data-splitter='timeline']"),
      variable: "--timeline-width",
      storageKey: `${splitterKeyPrefix}timeline-split`,
      minBefore: 280,
      minAfter: 360
    },
    {
      el: document.querySelector("[data-splitter='stack']"),
      variable: "--message-width",
      storageKey: `${splitterKeyPrefix}message-split`,
      minBefore: 260,
      minAfter: 260
    }
  ].filter((splitter) => splitter.el);
  const messageKeyOrder = new Map([
    "commitments",
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
    return hashSource() || "record?format=json";
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
    if (typeof value !== "string") return String(value);
    if (value.length === 43) return `${value.slice(0, 5)}..${value.slice(-5)}`;
    if (value.length === 87) return `${value.slice(0, 5)}..${value.slice(-5)}`;
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
      const type = `tuple (elements: ${value.length - 1})`;
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

  function hiddenMessageKey(value, key, options) {
    if (Array.isArray(value) || !value || typeof value !== "object") return false;
    if ((key === "device" || key === "path") && messageHeadline(value)) return true;
    return false;
  }

  function nodeEntries(value, options = {}) {
    if (!Array.isArray(value)) return orderedEntries(value);
    if (!isTupleForm(value)) return value.map((item, idx) => [String(idx + 1), item]);
    return value.slice(1).map((item, idx) => {
      if (Array.isArray(item) && item.length === 2 && isAtomText(item[0])) {
        return [scalarText(item[0]), item[1]];
      }
      return [null, item];
    });
  }

  function scalarType(value, text) {
    if (typeof value === "string") return `string ${text.length}`;
    return typeof value;
  }

  function isIdText(value) {
    return typeof value === "string" && /^[A-Za-z0-9_-]{43}$|^[A-Za-z0-9_-]{87}$/.test(value);
  }

  function messageHeadline(value) {
    if (Array.isArray(value) || !value || typeof value !== "object") return "";
    const device = value.device ? displayValue(value.device) : "";
    const path = value.path ? displayValue(value.path) : "";
    if (!device && !path) return "";
    if (device && path) return `${device.startsWith("~") ? device : `~${device}`}${path.startsWith("/") ? "" : "/"}${path}`;
    if (device) return device.startsWith("~") ? device : `~${device}`;
    return `Path: ${path}`;
  }

  function commitmentEntryList(commitments) {
    if (!commitments || Array.isArray(commitments) || typeof commitments !== "object") return [];
    return Object.entries(commitments)
      .filter(([id, commitment]) =>
        isIdText(id) &&
        commitment &&
        typeof commitment === "object"
      )
      .sort(([left], [right]) => left.localeCompare(right));
  }

  function commitmentEntries(value) {
    if (Array.isArray(value) || !value || typeof value !== "object") return [];
    return commitmentEntryList(value.commitments);
  }

  function commitmentMetaLabel(commitment) {
    const parts = [];
    if (commitment["commitment-device"]) {
      parts.push(` ~${displayValue(commitment["commitment-device"])}`);
    }
    if (commitment.type) parts.push(`/${displayValue(commitment.type)}`);
    if (commitment.committer) parts.push(` (Sig: ${shortId(displayValue(commitment.committer))})`);
    return parts.join("");
  }

  function commitmentLabel(id, commitment) {
    return `${shortId(id)}${commitmentMetaLabel(commitment)}`;
  }

  function addInlineButton(parent, className, text, title, onClick) {
    const button = document.createElement("button");
    button.type = "button";
    button.className = className;
    button.textContent = text;
    button.title = title;
    button.addEventListener("click", (event) => {
      event.preventDefault();
      event.stopPropagation();
      onClick();
    });
    parent.appendChild(button);
    return button;
  }

  function messageKeyCount(value, key, options) {
    if (key === "commitments") return commitmentEntryList(value).length;
    if (options.hideCommitmentBookkeeping) {
      return Object.keys(value).filter((childKey) =>
        childKey !== "priv" && childKey !== "commitments"
      ).length;
    }
    return Object.keys(value).length;
  }

  function appendType(summary, value, key, details, options) {
    const typeEl = document.createElement("span");
    typeEl.className = "tree-type";
    if (value && typeof value === "object" && !Array.isArray(value)) {
      const commitments = commitmentEntries(value);
      typeEl.append(`message (keys: ${messageKeyCount(value, key, options)}`);
      if (!options.hideCommitmentBookkeeping && key !== "commitments" && commitments.length > 0) {
        typeEl.append(", ");
        addInlineButton(
          typeEl,
          "reserved-link",
          commitments.length === 1 ? "comm.:" : "comms.:",
          "Open commitments",
          () => openCommitments(details)
        );
        commitments.forEach(([id], idx) => {
          typeEl.append(idx === 0 ? " " : ", ");
          addInlineButton(
            typeEl,
            "reserved-link commitment-ref",
            shortId(id),
            id,
            () => openCommitments(details, id)
          );
        });
      }
      typeEl.append(")");
    } else {
      typeEl.textContent = valueType(value, key);
    }
    summary.appendChild(typeEl);
  }

  function appendCommitmentMeta(summary, commitment) {
    if (!commitment || Array.isArray(commitment) || typeof commitment !== "object") return;
    const meta = commitmentMetaLabel(commitment).trim();
    if (!meta) return;
    const pill = document.createElement("span");
    pill.className = "commitment-meta-pill";
    pill.textContent = meta;
    summary.appendChild(pill);
  }

  function appendMessageHeadline(summary, value, details, depth) {
    const headline = messageHeadline(value);
    if (!headline) return;

    const meta = document.createElement("span");
    meta.className = "message-meta";
    const route = document.createElement("span");
    route.className = "message-route";
    route.textContent = headline;
    meta.appendChild(route);
    summary.appendChild(meta);
  }

  function openCommitments(details, selectedId) {
    details.open = true;
    const nodes = Array.from(details.querySelectorAll(":scope > .tree-children details.tree-node"));
    const commitmentsNode = nodes.find((node) => node.dataset.fullKey === "commitments");
    if (!commitmentsNode) return;
    commitmentsNode.open = true;
    if (!selectedId) return;
    const selectedNode = Array.from(commitmentsNode.querySelectorAll("details.tree-node"))
      .find((node) => node.dataset.fullKey === selectedId);
    if (selectedNode) {
      selectedNode.open = true;
      selectedNode.scrollIntoView({ block: "nearest" });
    }
  }

  function childOptions(options, displayKey) {
    if (displayKey === "commitments") {
      return {
        ...options,
        inCommitments: true,
        hideCommitmentBookkeeping: true,
        showHeadline: false,
        showHeadlineKeys: true
      };
    }
    if (options.inCommitments) {
      return {
        ...options,
        inCommitments: false,
        hideCommitmentBookkeeping: true,
        showHeadline: false
      };
    }
    return options;
  }

  function renderKey(key, value, options) {
    if (options.inCommitments && typeof key === "string") return shortId(key);
    if (key === null && isTupleForm(value)) return scalarText(value[0]);
    return key;
  }

  function isReservedKey(key, options) {
    return key === "commitments" || options.inCommitments;
  }

  function markNode(node, key) {
    if (key === null || key === undefined) return;
    node.dataset.fullKey = String(key);
  }

  function appendKey(summary, key, options, title) {
    if (key === null || key === "") {
      summary.className = "tree-summary-unlabeled";
      return;
    }
    const keyEl = document.createElement("span");
    keyEl.className = "tree-key";
    if (isReservedKey(key, options)) keyEl.classList.add("reserved-key");
    if (title) keyEl.title = title;
    keyEl.textContent = key;
    summary.appendChild(keyEl);
  }

  function entryList(value, options) {
    const entries = nodeEntries(value, options)
      .filter(([childKey]) =>
        !hiddenMessageKey(value, childKey, options) ||
        options.showHeadlineKeys
      );
    if (options.inCommitments) {
      return entries.filter(([childKey, childValue]) =>
        isIdText(childKey) &&
        childValue &&
        typeof childValue === "object"
      );
    }
    if (options.hideCommitmentBookkeeping) {
      return entries.filter(([childKey]) => childKey !== "priv" && childKey !== "commitments");
    }
    return entries;
  }

  function appendChildren(children, entries, depth, options) {
    if (entries.length === 0) {
      children.appendChild(messageLeaf("(empty)", ""));
      return;
    }
    entries.forEach(([childKey, childValue]) => {
      children.appendChild(messageNode(childKey, childValue, depth + 1, options));
    });
  }

  function messageNode(key, value, depth, options = {}) {
    if (value && typeof value === "object") {
      const details = document.createElement("details");
      details.className = "tree-node";
      details.open = depth === 0 || (depth === 1 && key === "event");
      markNode(details, key);

      const summary = document.createElement("summary");
      const displayKey = renderKey(key, value, options);
      appendKey(summary, displayKey, options, key !== displayKey ? key : "");
      appendType(summary, value, displayKey, details, options);
      if (options.inCommitments) appendCommitmentMeta(summary, value);
      if (options.showHeadline !== false) {
        appendMessageHeadline(summary, value, details, depth);
      }
      details.appendChild(summary);

      const children = document.createElement("div");
      children.className = "tree-children";
      const nextOptions = childOptions(options, displayKey);
      appendChildren(children, entryList(value, nextOptions), depth, nextOptions);
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
      if (key !== null && key !== "") {
        const keyEl = document.createElement("span");
        keyEl.className = "tree-key";
        keyEl.textContent = key;
        summary.appendChild(keyEl);
      } else {
        summary.className = "tree-summary-unlabeled";
      }
      const typeEl = document.createElement("span");
      typeEl.className = "tree-type";
      typeEl.textContent = scalarType(value, text);
      summary.appendChild(typeEl);
      const valueEl = document.createElement("div");
      valueEl.className = "tree-scalar";
      valueEl.textContent = text;
      details.append(summary, valueEl);
      return details;
    }

    const row = document.createElement("div");
    row.className = "tree-leaf";
    if (key === null || key === "") row.classList.add("unlabeled");
    const keyEl = document.createElement("span");
    keyEl.className = "tree-key";
    keyEl.textContent = key || "";
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
      const activeDepth = activeStackDepth(stack);
      stack.forEach((frame, idx) =>
        children.appendChild(stackFrameNode(frame, idx + 1, stack, activeDepth))
      );
    }
    root.appendChild(children);
    els.stackView.appendChild(root);
  }

  function stackFrameNode(frame, idx, stack, activeDepth) {
    if (!Array.isArray(frame) || frame.length < 2) return messageNode(String(idx), frame, 1);
    const row = document.createElement("div");
    const depth = idx - 1;
    row.className = `tree-leaf stack-frame${
      activeDepth !== null && depth >= activeDepth ? " stack-active" : ""
    }${depth === activeDepth ? " stack-active-top" : ""}`;
    row.title = "Filter timeline to this stack window";
    row.tabIndex = 0;
    row.setAttribute("role", "button");
    row.addEventListener("click", (event) => {
      if (event.target.closest(".stack-filter")) return;
      event.preventDefault();
      selectStackWindow(stack, depth);
    });
    row.addEventListener("keydown", (event) => {
      if (event.key === "Enter" || event.key === " ") {
        event.preventDefault();
        selectStackWindow(stack, depth);
      }
    });
    const parts = stackFrameParts(frame);

    const keyEl = document.createElement("span");
    keyEl.className = "tree-key stack-depth";
    keyEl.textContent = `#${idx}`;
    keyEl.title = "Filter timeline to this stack window";
    keyEl.addEventListener("click", (event) => {
      event.stopPropagation();
      selectStackWindow(stack, depth);
    });

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

  function stackFrameKey(frame) {
    if (!Array.isArray(frame) || frame.length < 2) return searchableText(frame);
    const parts = stackFrameParts(frame);
    return [parts.module, parts.func, parts.arity, parts.line].join("\u001f");
  }

  function stackSignature(stack, depth) {
    return stack.slice(depth).map(stackFrameKey);
  }

  function sameSignature(left, right) {
    return left.length === right.length && left.every((item, idx) => item === right[idx]);
  }

  function stackMatchesSignature(stack, signature) {
    if (stack.length < signature.length) return false;
    const offset = stack.length - signature.length;
    return signature.every((item, idx) => stackFrameKey(stack[offset + idx]) === item);
  }

  function eventStack(event) {
    const raw = rawMessage(event);
    return asList(raw && raw.stack);
  }

  function activeStackDepth(stack) {
    if (!state.stackFilter || !stackMatchesSignature(stack, state.stackFilter.signature)) {
      return null;
    }
    return stack.length - state.stackFilter.signature.length;
  }

  function selectStackWindow(stack, depth) {
    const signature = stackSignature(stack, depth);
    if (state.stackFilter && sameSignature(state.stackFilter.signature, signature)) {
      state.stackFilter = null;
      render({ centerSelected: true });
      return;
    }

    let start = state.selected.eventIndex;
    let end = start;
    while (
      start > 0 &&
      stackMatchesSignature(eventStack(state.report.events[start - 1]), signature)
    ) {
      start -= 1;
    }
    while (
      end < state.report.events.length - 1 &&
      stackMatchesSignature(eventStack(state.report.events[end + 1]), signature)
    ) {
      end += 1;
    }
    state.stackFilter = { signature, start, end };
    render({ centerSelected: true });
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
      Object.defineProperty(normalized, "eventIndex", {
        value: idx,
        enumerable: false
      });
      Object.defineProperty(normalized, "searchText", {
        value: searchableText(event),
        enumerable: false
      });
      Object.defineProperty(normalized, "filterText", {
        value: [
          normalized.topic,
          normalized.name,
          normalized.module,
          normalized.function,
          normalized.line,
          normalized.searchText,
          ...normalized.stack
        ].join(" ").toLowerCase(),
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

  function storedSplitterRatio(splitter) {
    try {
      const ratio = Number.parseFloat(localStorage.getItem(splitter.storageKey));
      return Number.isFinite(ratio) && ratio > 0 && ratio < 1 ? ratio : null;
    } catch (_err) {
      return null;
    }
  }

  function saveSplitterRatio(splitter, ratio) {
    if (!Number.isFinite(ratio)) return;
    try {
      localStorage.setItem(splitter.storageKey, String(ratio));
    } catch (_err) {}
  }

  function splitterMetrics(splitter) {
    const container = splitter.el.parentElement;
    if (!container || container.clientWidth === 0) return null;
    const splitterWidthPx = splitter.el.getBoundingClientRect().width || 10;
    const available = container.clientWidth - splitterWidthPx;
    return available > 0 ? { container, available } : null;
  }

  function setSplitterRatio(splitter, ratio, metrics = splitterMetrics(splitter)) {
    if (!metrics) return;
    const maxBefore = Math.max(
      splitter.minBefore,
      metrics.available - splitter.minAfter
    );
    const minRatio = splitter.minBefore / metrics.available;
    const maxRatio = maxBefore / metrics.available;
    const next = Math.max(minRatio, Math.min(ratio, maxRatio));
    metrics.container.style.setProperty(splitter.variable, `${(next * 100).toFixed(3)}%`);
    return next;
  }

  function restoreSplitters() {
    splitters.forEach((splitter) => {
      const ratio = storedSplitterRatio(splitter);
      if (ratio) {
        setSplitterRatio(splitter, ratio);
      }
    });
  }

  function beginResize(splitter, event) {
    if (event.button !== 0) return;
    event.preventDefault();
    const metrics = splitterMetrics(splitter);
    if (!metrics) return;
    const rect = metrics.container.getBoundingClientRect();
    const beforeRect = splitter.el.previousElementSibling.getBoundingClientRect();
    const initialRatio = beforeRect.width / metrics.available;
    const minRatio = splitter.minBefore / metrics.available;
    const maxRatio = (metrics.available - splitter.minAfter) / metrics.available;
    let nextRatio = initialRatio;
    splitter.el.classList.add("active");
    document.body.classList.add("resizing");
    splitter.el.setPointerCapture(event.pointerId);

    const move = (moveEvent) => {
      const rawRatio = (moveEvent.clientX - rect.left) / metrics.available;
      nextRatio = Math.max(minRatio, Math.min(rawRatio, maxRatio));
      splitter.el.style.transform =
        `translateX(${(nextRatio - initialRatio) * metrics.available}px)`;
    };
    const stop = () => {
      splitter.el.style.transform = "";
      saveSplitterRatio(splitter, setSplitterRatio(splitter, nextRatio));
      splitter.el.classList.remove("active");
      document.body.classList.remove("resizing");
      window.removeEventListener("pointermove", move);
      window.removeEventListener("pointerup", stop);
      window.removeEventListener("pointercancel", stop);
      try {
        splitter.el.releasePointerCapture(event.pointerId);
      } catch (_err) {}
    };

    window.addEventListener("pointermove", move);
    window.addEventListener("pointerup", stop);
    window.addEventListener("pointercancel", stop);
  }

  function wireSplitters() {
    splitters.forEach((splitter) => {
      splitter.el.addEventListener("pointerdown", (event) => beginResize(splitter, event));
    });
    restoreSplitters();
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
      {
        field: "topic",
        input: els.topic,
        selected: els.topicSelected,
        options: els.topicOptions,
        values: unique("topic")
      },
      {
        field: "name",
        input: els.name,
        selected: els.nameSelected,
        options: els.nameOptions,
        values: unique("name")
      },
      {
        field: "module",
        input: els.module,
        selected: els.moduleSelected,
        options: els.moduleOptions,
        values: unique("module")
      },
      {
        field: "function",
        input: els.func,
        selected: els.funcSelected,
        options: els.funcOptions,
        values: unique("function")
      }
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

  function filterPill(field, value) {
    const pill = document.createElement("span");
    pill.className = "filter-pill";
    const label = document.createElement("span");
    label.textContent = value;

    const remove = document.createElement("button");
    remove.type = "button";
    remove.textContent = "x";
    remove.title = `Remove ${value}`;
    remove.addEventListener("click", () => removeFilterValue(field, value));
    pill.append(label, remove);
    return pill;
  }

  function fillFilterOptions({ field, input, selected, options, values }) {
    const current = input.value;
    const needle = current.trim().toLowerCase();
    const chosen = state.filters[field];
    selected.innerHTML = "";
    chosen.forEach((value) => selected.appendChild(filterPill(field, value)));
    options.innerHTML = "";
    if (chosen.length > 0) {
      options.appendChild(filterOption(clearFilterValue, () => {
        state.filters[field] = [];
        input.value = "";
        state.openFilter = null;
        render({ centerSelected: true });
      }, "clear"));
    }
    values
      .filter((value) =>
        !chosen.includes(value) &&
        (!needle || value.toLowerCase().startsWith(needle))
      )
      .slice(0, maxFilterOptions)
      .forEach((value) => {
        options.appendChild(filterOption(value, () => {
          if (!state.filters[field].includes(value)) {
            state.filters[field] = [...state.filters[field], value];
          }
          input.value = "";
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

  function removeFilterValue(field, value) {
    state.filters[field] = state.filters[field].filter((item) => item !== value);
    render({ centerSelected: true });
  }

  function formatInteger(value) {
    return numberFormatter.format(value);
  }

  function formatEventCount(value) {
    return `${formatInteger(value)} ${value === 1 ? "event" : "events"}`;
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
    return stackWindowMatches(event) && nonStackFiltersMatch(event);
  }

  function stackWindowMatches(event) {
    return !state.stackFilter ||
      (
        event.eventIndex >= state.stackFilter.start &&
        event.eventIndex <= state.stackFilter.end
      );
  }

  function nonStackFiltersMatch(event) {
    const q = els.search.value.trim().toLowerCase();
    return (!q || event.filterText.includes(q)) &&
      fieldMatches("topic", event.topic) &&
      fieldMatches("name", event.name) &&
      fieldMatches("module", event.module) &&
      fieldMatches("function", event.function);
  }

  function fieldMatches(field, value) {
    const selected = state.filters[field];
    return selected.length === 0 || selected.includes(String(value));
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
    if (!state.filters[field].includes(value)) {
      state.filters[field] = [...state.filters[field], value];
    }
    input.value = "";
    syncFilterOptions();
    render();
  }

  function clearStackFilter() {
    state.stackFilter = null;
    render({ centerSelected: true });
  }

  function syncFilterOptions() {
    fillFilters();
  }

  function render(opts = {}) {
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
    if (opts.centerSelected) {
      requestAnimationFrame(centerSelectedEvent);
    }
  }

  function renderStats() {
    els.count.textContent = formatInteger(state.report.events.length);
    els.topics.textContent = formatInteger(unique("topic").length);
    els.modules.textContent = formatInteger(unique("module").length);
    renderDurationStat();
    renderVisibleCount();
  }

  function renderDurationStat() {
    els.duration.innerHTML = "";
    const totalDuration = formatDurationUs(reportDuration()) || "0us";
    const selectedDuration = selectedStackDuration();
    if (selectedDuration === null) {
      els.duration.append(document.createTextNode(totalDuration));
      return;
    }
    els.duration.append(document.createTextNode(formatDurationUs(selectedDuration) || "0us"));
    const total = document.createElement("button");
    total.type = "button";
    total.className = "stat-total";
    total.textContent = `(total: ${totalDuration})`;
    total.title = "Clear stack filter";
    total.addEventListener("click", clearStackFilter);
    els.duration.appendChild(total);
  }

  function selectedStackDuration() {
    if (!state.stackFilter) return null;
    const start = state.report.events[state.stackFilter.start];
    const end = state.report.events[state.stackFilter.end];
    if (!start || !end || start.time === null || end.time === null) return null;
    return Math.max(0, end.time - start.time);
  }

  function renderVisibleCount() {
    els.visibleCount.innerHTML = "";
    els.visibleCount.append(document.createTextNode(formatEventCount(state.filtered.length)));
    if (!state.stackFilter) return;
    const total = state.report.events.filter(nonStackFiltersMatch).length;
    const totalButton = document.createElement("button");
    totalButton.type = "button";
    totalButton.className = "timeline-total";
    totalButton.textContent = `(total: ${formatInteger(total)})`;
    totalButton.title = "Clear stack filter";
    totalButton.addEventListener("click", clearStackFilter);
    els.visibleCount.append(document.createTextNode(" "), totalButton);
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
    state.filtered.forEach((event) => {
      const row = document.createElement("button");
      row.className = `event-row${event === state.selected ? " active" : ""}`;
      row.type = "button";
      row.setAttribute("role", "listitem");
      row.dataset.eventIndex = event.eventIndex;
      row.addEventListener("click", () => selectEvent(event));

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

  function selectEvent(event) {
    if (state.selected === event) return;
    const previous = state.selected;
    state.selected = event;
    setEventRowActive(previous, false);
    setEventRowActive(event, true);
    renderDetail();
  }

  function setEventRowActive(event, active) {
    if (!event) return;
    const row = els.list.querySelector(`[data-event-index="${event.eventIndex}"]`);
    if (row) row.classList.toggle("active", active);
  }

  function centerSelectedEvent() {
    if (!state.selected) return;
    const row = els.list.querySelector(".event-row.active");
    if (!row) return;
    const listRect = els.list.getBoundingClientRect();
    const rowRect = row.getBoundingClientRect();
    els.list.scrollTop +=
      rowRect.top - listRect.top - ((els.list.clientHeight - rowRect.height) / 2);
  }

  function renderDetail() {
    const event = state.selected;
    els.detailEmpty.hidden = !!event;
    els.detailView.hidden = !event;
    if (!event) {
      els.selectedSequence.textContent = "No selection";
      return;
    }

    els.selectedSequence.innerHTML = "";
    const anchor = document.createElement("button");
    anchor.className = "selection-anchor";
    anchor.type = "button";
    anchor.textContent = `#${event.sequence}`;
    anchor.title = "Center selected event";
    anchor.addEventListener("click", centerSelectedEvent);
    els.selectedSequence.appendChild(anchor);
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
    wireSplitters();
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
        syncFilterOptions();
      });
      el.addEventListener("change", () => {
        syncFilterOptions();
      });
      el.addEventListener("keydown", (event) => {
        if (event.key === "Escape") {
          el.value = "";
          state.openFilter = null;
          syncFilterOptions();
        } else if (event.key === "Enter") {
          const list = document.getElementById(el.getAttribute("aria-controls"));
          const option = list && list.querySelector(".filter-option:not(.clear)");
          if (option) option.click();
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
