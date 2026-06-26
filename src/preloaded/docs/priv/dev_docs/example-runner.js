(function () {
  'use strict';

  const DEFAULT_NODE = 'http://localhost:8734';
  const MIN_RESULT_DELAY_MS = 480;
  const LOCAL_NODE_PATTERN = /http:\/\/localhost:8734|http:\/\/127\.0\.0\.1:8734|localhost:8734|127\.0\.0\.1:8734/g;
  const FORBIDDEN_HEADERS = new Set([
    'accept-charset',
    'accept-encoding',
    'access-control-request-headers',
    'access-control-request-method',
    'connection',
    'content-length',
    'cookie',
    'cookie2',
    'date',
    'dnt',
    'expect',
    'host',
    'keep-alive',
    'origin',
    'referer',
    'te',
    'trailer',
    'transfer-encoding',
    'upgrade',
    'via'
  ]);

  const ACTION_ICONS = {
    run: '<svg class="hb-code-action-icon" viewBox="0 0 256 256" fill="currentColor" aria-hidden="true"><path d="M240,128a8,8,0,0,1-4.66,7.28l-184,112A8,8,0,0,1,40,240V16a8,8,0,0,1,12.34-6.72l184,112A8,8,0,0,1,240,128Z"/></svg>',
    inspect: '<svg class="hb-code-action-icon" viewBox="0 0 256 256" fill="currentColor" aria-hidden="true"><path d="M229.66,218.34l-50.07-50.06a88.11,88.11,0,1,0-11.31,11.31l50.06,50.07a8,8,0,0,0,11.32-11.32ZM40,112a72,72,0,1,1,72,72A72.08,72.08,0,0,1,40,112Z"/></svg>'
  };

  const STEP_CHECK_ICON = '<svg class="hb-runner-step-check" viewBox="0 0 256 256" fill="currentColor" aria-hidden="true"><path d="M232.49,80.49l-128,128a12,12,0,0,1-17,0l-56-56a12,12,0,1,1,17-17L96,183,215.51,63.51a12,12,0,0,1,17,17Z"/></svg>';
  const CODEC_HELP = 'The runner keeps the request path codec-neutral, then applies this presentation codec when it sends the request.';
  const CODEC_OPTIONS = [
    {
      id: 'none',
      label: 'Node default',
      suffix: '',
      title: 'Send the request without appending a presentation codec.'
    },
    {
      id: 'json',
      label: 'JSON',
      suffix: '/~json@1.0/serialize',
      title: 'Append /~json@1.0/serialize so the response is encoded as JSON.'
    },
    {
      id: 'hyperbuddy',
      label: 'Hyperbuddy',
      suffix: '/format~hyperbuddy@1.0',
      title: 'Append /format~hyperbuddy@1.0 for HyperBEAM human-readable output.'
    }
  ];
  const CODEC_BY_ID = new Map(CODEC_OPTIONS.map((codec) => [codec.id, codec]));
  const PRESENTATION_CODEC_PATTERNS = [
    { id: 'json', pattern: /\/~json@1\.0\/serialize\/?(?=$|[\s"'`)|;&?#])/g },
    { id: 'json', pattern: /\/serialize~json@1\.0\/?(?=$|[\s"'`)|;&?#])/g },
    { id: 'hyperbuddy', pattern: /\/format~hyperbuddy@1\.0\/?(?=$|[\s"'`)|;&?#])/g }
  ];

  function delayRemaining(startedAt, minimumMs = MIN_RESULT_DELAY_MS) {
    const remaining = minimumMs - (performance.now() - startedAt);
    if (remaining <= 0) return Promise.resolve();
    return new Promise((resolve) => window.setTimeout(resolve, remaining));
  }

  function pendingItemFor(request) {
    return {
      method: request.method,
      status: 'Fetching…',
      output: '',
      pending: true
    };
  }

  function codeActionLabel(analysis) {
    if (analysis.requests.length > 1) return { text: 'Run block', icon: 'run' };
    if (analysis.requests.length) return { text: 'Run', icon: 'run' };
    return { text: 'Inspect', icon: 'inspect' };
  }

  function renderCodeActionButton(label) {
    const icon = ACTION_ICONS[label.icon] || ACTION_ICONS.run;
    return `${icon}<span>${label.text}</span>`;
  }

  let activeNode = normalizeNode(readNodeFromUrl()) || DEFAULT_NODE;
  let drawer;
  let nodeInput;
  let codecSelect;
  let selectedCodecId = 'none';
  let selectedRun;
  let runMode = 'all';
  let showResults = false;
  let completedSteps = new Set();
  let drawerOutput = {
    output: 'Select a runnable example.',
    requests: '',
    items: []
  };

  function normalizeNode(raw) {
    if (!raw) return '';
    let value = String(raw).trim();
    if (!value) return '';
    if (/^(localhost|127\.0\.0\.1)(:\d+)?(\/|$)/i.test(value)) value = `http://${value}`;
    if (!/^https?:\/\//i.test(value)) value = `https://${value}`;
    try {
      const url = new URL(value);
      const cleanPath = url.pathname === '/' ? '' : url.pathname.replace(/\/+$/g, '');
      return `${url.origin}${cleanPath}`;
    } catch {
      return '';
    }
  }

  function readNodeFromUrl() {
    const searchNode = new URLSearchParams(window.location.search).get('node');
    if (searchNode) return searchNode;

    const hash = window.location.hash || '';
    const queryIndex = hash.indexOf('?');
    if (queryIndex === -1) return '';
    return new URLSearchParams(hash.slice(queryIndex + 1)).get('node') || '';
  }

  function writeNodeToUrl(node) {
    const url = new URL(window.location.href);
    if (node && node !== DEFAULT_NODE) url.searchParams.set('node', node);
    else url.searchParams.delete('node');

    const next = `${url.pathname}${url.search}${url.hash}`;
    window.history.replaceState(null, '', next);
  }

  function setActiveNode(raw, options = {}) {
    const normalized = normalizeNode(raw) || DEFAULT_NODE;
    activeNode = normalized;
    if (nodeInput) nodeInput.value = activeNode;
    if (options.writeUrl !== false) writeNodeToUrl(activeNode);
    refreshExamples();
    updateDrawerNode();
  }

  function displayUrl(url) {
    return url.replace(LOCAL_NODE_PATTERN, activeNode);
  }

  function stripPresentationCodec(value) {
    let output = String(value || '');
    let codecId = 'none';

    for (const { id, pattern } of PRESENTATION_CODEC_PATTERNS) {
      output = output.replace(pattern, () => {
        codecId = id;
        return '';
      });
    }

    return { value: output, codecId };
  }

  function stripPresentationCodecsFromText(text) {
    return stripPresentationCodec(text).value;
  }

  function displaySnippetText(text) {
    return stripPresentationCodecsFromText(displayUrl(text));
  }

  function appendPresentationCodec(url, codecId = selectedCodecId) {
    const codec = CODEC_BY_ID.get(codecId) || CODEC_BY_ID.get('none');
    const base = stripPresentationCodec(url).value;
    if (!codec?.suffix) return base;

    const hashIndex = base.indexOf('#');
    const hash = hashIndex >= 0 ? base.slice(hashIndex) : '';
    const withoutHash = hashIndex >= 0 ? base.slice(0, hashIndex) : base;
    const queryIndex = withoutHash.indexOf('?');
    const beforeQuery = queryIndex >= 0 ? withoutHash.slice(0, queryIndex) : withoutHash;
    const query = queryIndex >= 0 ? withoutHash.slice(queryIndex) : '';
    const trimmed = beforeQuery.replace(/\/+$/g, '');
    return `${trimmed}${codec.suffix}${query}${hash}`;
  }

  function requestUrlFor(request, codecId = selectedCodecId) {
    return appendPresentationCodec(request.url, codecId);
  }

  function absoluteUrl(target) {
    const rewritten = displayUrl(target);
    if (/^https?:\/\//i.test(rewritten)) return rewritten;
    if (rewritten.startsWith('/')) return `${activeNode}${rewritten}`;
    return `${activeNode}/${rewritten}`;
  }

  function shellSplit(command) {
    const tokens = [];
    let token = '';
    let quote = '';
    let escaped = false;

    for (const char of command) {
      if (escaped) {
        token += char;
        escaped = false;
        continue;
      }
      if (quote) {
        if (char === quote) quote = '';
        else if (quote === '"' && char === '\\') escaped = true;
        else token += char;
        continue;
      }
      if (char === '\\') {
        escaped = true;
        continue;
      }
      if (char === '"' || char === "'") {
        quote = char;
        continue;
      }
      if (/\s/.test(char)) {
        if (token) {
          tokens.push(token);
          token = '';
        }
        continue;
      }
      token += char;
    }

    if (token) tokens.push(token);
    return tokens;
  }

  function splitUnquoted(text, delimiter) {
    const parts = [];
    let part = '';
    let quote = '';
    let escaped = false;

    for (let i = 0; i < text.length; i += 1) {
      const char = text[i];
      if (escaped) {
        part += char;
        escaped = false;
        continue;
      }
      if (char === '\\') {
        part += char;
        escaped = true;
        continue;
      }
      if (quote) {
        part += char;
        if (char === quote) quote = '';
        continue;
      }
      if (char === '"' || char === "'") {
        part += char;
        quote = char;
        continue;
      }
      if (char === delimiter) {
        parts.push(part.trim());
        part = '';
        continue;
      }
      part += char;
    }

    if (part.trim()) parts.push(part.trim());
    return parts;
  }

  function logicalLines(text) {
    const lines = text.replace(/\r\n/g, '\n').split('\n');
    const result = [];
    let current = '';

    for (const line of lines) {
      current = current ? `${current} ${line.trimStart()}` : line;
      if (/\\\s*$/.test(current)) {
        current = current.replace(/\\\s*$/g, '').trimEnd();
      } else {
        result.push(current);
        current = '';
      }
    }

    if (current.trim()) result.push(current);
    return result;
  }

  function stripMatchingQuotes(value) {
    const trimmed = String(value).trim();
    if ((trimmed.startsWith('"') && trimmed.endsWith('"')) || (trimmed.startsWith("'") && trimmed.endsWith("'"))) {
      return trimmed.slice(1, -1);
    }
    return trimmed;
  }

  function isAssignmentToken(token) {
    return /^[A-Za-z_][A-Za-z0-9_]*=/.test(token || '');
  }

  function assignmentParts(token) {
    const match = String(token).match(/^([A-Za-z_][A-Za-z0-9_]*)=(.*)$/);
    if (!match) return null;
    return { name: match[1], value: match[2] };
  }

  function hasPlaceholder(value) {
    return /<[^>]+>/.test(value);
  }

  function expandArithmetic(expression, env) {
    return expression.replace(/\$\(\(([^)]+)\)\)/g, (all, raw) => {
      const expanded = raw.replace(/\b[A-Za-z_][A-Za-z0-9_]*\b/g, (name) => {
        const value = env[name];
        return /^-?\d+$/.test(value || '') ? value : name;
      });
      if (!/^[\d+\-*/% ().]+$/.test(expanded)) return all;
      try {
        const value = Function(`"use strict"; return (${expanded});`)();
        return Number.isFinite(value) ? String(value) : all;
      } catch {
        return all;
      }
    });
  }

  function expandShellValue(value, env) {
    let output = displayUrl(stripMatchingQuotes(value));
    const unresolved = new Set();

    output = expandArithmetic(output, env);
    output = output.replace(/\$\{([A-Za-z_][A-Za-z0-9_]*):-([^}]+)\}/g, (all, name, fallback) => {
      return env[name] || displayUrl(stripMatchingQuotes(fallback));
    });
    output = output.replace(/\$\{([A-Za-z_][A-Za-z0-9_]*)\}/g, (all, name) => {
      if (env[name] !== undefined) return env[name];
      unresolved.add(name);
      return all;
    });
    output = output.replace(/\$([A-Za-z_][A-Za-z0-9_]*)/g, (all, name) => {
      if (env[name] !== undefined) return env[name];
      unresolved.add(name);
      return all;
    });

    return { value: output, unresolved: [...unresolved] };
  }

  function applyAssignment(token, env, notes) {
    const parts = assignmentParts(token);
    if (!parts) return false;
    if (/\$\(|`/.test(parts.value)) {
      notes.push(`${parts.name} uses command substitution, so browser execution cannot derive it.`);
      return false;
    }
    const expanded = expandShellValue(parts.value, env);
    env[parts.name] = expanded.value;
    if (expanded.unresolved.length) {
      notes.push(`${parts.name} references unresolved variable(s): ${expanded.unresolved.join(', ')}.`);
    }
    return true;
  }

  function splitRedirection(command) {
    const parts = splitUnquoted(command, '>');
    if (parts.length === 1) return { command: parts[0], redirect: '' };
    return {
      command: parts[0],
      redirect: parts.slice(1).join('>').trim()
    };
  }

  function parseTransforms(parts, notes) {
    const transforms = [];
    for (const part of parts) {
      const tokens = shellSplit(part);
      if (!tokens.length) continue;
      if (tokens[0] === 'grep') {
        let ignoreCase = false;
        let extended = false;
        let pattern = '';
        for (let i = 1; i < tokens.length; i += 1) {
          const token = tokens[i];
          if (token === '-i') ignoreCase = true;
          else if (token === '-E') extended = true;
          else if (token.startsWith('-') && token.includes('i')) ignoreCase = true;
          else if (token.startsWith('-') && token.includes('E')) extended = true;
          else if (!pattern) pattern = token;
        }
        if (pattern) transforms.push({ type: 'grep', pattern, ignoreCase, extended });
        else notes.push('A grep filter did not include a pattern.');
      } else if (tokens[0] === 'head') {
        let chars = 0;
        let lines = 0;
        for (let i = 1; i < tokens.length; i += 1) {
          const token = tokens[i];
          if (token === '-c') chars = Number(tokens[++i] || 0);
          else if (token === '-n') lines = Number(tokens[++i] || 0);
          else if (/^-\d+$/.test(token)) lines = Math.abs(Number(token));
        }
        if (chars > 0) transforms.push({ type: 'head-chars', count: chars });
        else if (lines > 0) transforms.push({ type: 'head-lines', count: lines });
      } else {
        notes.push(`Pipeline command "${tokens[0]}" is not emulated in the browser runner.`);
      }
    }
    return transforms;
  }

  function applyTransforms(text, transforms = []) {
    let output = text;
    for (const transform of transforms) {
      if (transform.type === 'grep') {
        const flags = transform.ignoreCase ? 'i' : '';
        const regex = new RegExp(transform.pattern, flags);
        output = output.split('\n').filter((line) => regex.test(line)).join('\n');
      } else if (transform.type === 'head-chars') {
        output = output.slice(0, transform.count);
      } else if (transform.type === 'head-lines') {
        output = output.split('\n').slice(0, transform.count).join('\n');
      }
    }
    return output;
  }

  function appendQueryData(url, dataParts) {
    const parsed = new URL(url);
    for (const part of dataParts) {
      const split = part.indexOf('=');
      if (split === -1) parsed.searchParams.append(part, '');
      else parsed.searchParams.append(part.slice(0, split), part.slice(split + 1));
    }
    return parsed.toString();
  }

  function parseCurl(command, env, transforms = [], redirect = '', notes = []) {
    const raw = command.replace(/\\\r?\n\s*/g, ' ').trim();
    const tokens = shellSplit(raw);
    while (tokens.length && isAssignmentToken(tokens[0])) {
      applyAssignment(tokens.shift(), env, notes);
    }

    const unresolved = new Set();
    const expandedTokens = tokens.map((token) => {
      const expanded = expandShellValue(token, env);
      expanded.unresolved.forEach((name) => unresolved.add(name));
      return expanded.value;
    });
    if (unresolved.size) {
      return { unsupported: true, reason: `Unresolved variable(s): ${[...unresolved].join(', ')}.` };
    }

    const clean = expandedTokens.join(' ');
    if (/\$\(|`/.test(clean)) {
      return { unsupported: true, reason: 'This curl block depends on command substitution.' };
    }
    if (hasPlaceholder(clean)) {
      return { unsupported: true, reason: 'This curl block contains placeholder values.' };
    }

    tokens.splice(0, tokens.length, ...expandedTokens);
    if (!tokens.length || tokens[0] !== 'curl') return { unsupported: true, reason: 'No curl command found.' };

    const headers = {};
    let body = '';
    let method = '';
    let url = '';
    let useGet = false;
    let includeHeaders = false;
    const queryData = [];
    const localFiles = [];

    for (let i = 1; i < tokens.length; i += 1) {
      const token = tokens[i];

      if (token === '-X' || token === '--request') {
        method = (tokens[++i] || '').toUpperCase();
      } else if (token.startsWith('-X') && token.length > 2) {
        method = token.slice(2).toUpperCase();
      } else if (token === '-H' || token === '--header') {
        const header = tokens[++i] || '';
        const split = header.indexOf(':');
        if (split > 0) headers[header.slice(0, split).trim()] = header.slice(split + 1).trim();
      } else if (token === '-d' || token === '--data' || token === '--data-raw' || token === '--data-binary' || token === '--data-urlencode') {
        const value = tokens[++i] || '';
        if (value.startsWith('@')) {
          localFiles.push(value);
        } else if (useGet || token === '--data-urlencode') {
          queryData.push(value);
        } else {
          body = body ? `${body}&${value}` : value;
        }
      } else if (token === '-I' || token === '--head') {
        method = 'HEAD';
      } else if (token === '-i' || token === '--include') {
        includeHeaders = true;
      } else if (token === '-G' || token === '--get') {
        useGet = true;
      } else if (token === '--url') {
        url = tokens[++i] || '';
      } else if (token === '-o' || token === '--output' || token === '-w' || token === '--write-out' || token === '--connect-timeout' || token === '--max-time' || token === '-m') {
        i += 1;
      } else if (/^-[A-Za-z]+$/.test(token)) {
        if (token.includes('I')) method = 'HEAD';
        if (token.includes('G')) useGet = true;
        if (token.includes('i')) includeHeaders = true;
      } else if (token.startsWith('-')) {
        continue;
      } else if (!url && (/^https?:\/\//i.test(token) || token.startsWith('/'))) {
        url = token;
      }
    }

    if (!url) return { unsupported: true, reason: 'Could not find a request URL in this curl block.' };
    if (localFiles.length) return { unsupported: true, reason: `This curl block reads local file data (${localFiles.join(', ')}).` };

    let requestUrl = absoluteUrl(url);
    if (queryData.length) requestUrl = appendQueryData(requestUrl, queryData);
    const presentation = stripPresentationCodec(requestUrl);
    const redirectTarget = redirect.replace(/^&?1?\s*/, '').trim();

    const request = {
      method: method || (body ? 'POST' : 'GET'),
      url: presentation.value,
      defaultCodecId: presentation.codecId,
      headers,
      body,
      source: stripPresentationCodecsFromText(clean),
      transforms,
      includeHeaders,
      suppressOutput: /\/dev\/null/.test(redirectTarget),
      redirectTarget
    };
    return { request };
  }

  function parseHttpSnippet(text) {
    const lines = text.trim().split(/\r?\n/);
    const first = lines.find((line) => line.trim() && !line.trim().startsWith('#'));
    if (!first) return null;

    const match = first.trim().match(/^(GET|POST|PUT|PATCH|DELETE|HEAD|OPTIONS)\s+(\S+)/i);
    if (!match) return null;

    const method = match[1].toUpperCase();
    const target = match[2];
    if (/<[^>]+>|\$/.test(target)) {
      return { unsupported: true, reason: 'This HTTP snippet contains placeholders.' };
    }

    const blank = lines.findIndex((line) => !line.trim());
    const body = blank >= 0 ? lines.slice(blank + 1).join('\n').trim() : '';
    const absoluteTarget = absoluteUrl(target);
    const presentation = stripPresentationCodec(absoluteTarget);
    return {
      request: {
        method,
        url: presentation.value,
        defaultCodecId: presentation.codecId,
        headers: {},
        body,
        source: stripPresentationCodecsFromText(`${method} ${absoluteTarget}${body ? `\n\n${body}` : ''}`)
      }
    };
  }

  function analyzeSnippet(text, inheritedEnv = {}) {
    const rewritten = displayUrl(text);
    return analyzeShellBlock(rewritten, inheritedEnv);
  }

  function analyzeShellBlock(text, inheritedEnv = {}) {
    const env = { HB: activeNode, ...inheritedEnv };
    const requests = [];
    const notes = [];

    for (const rawLine of logicalLines(text)) {
      const line = rawLine.trim();
      if (!line || line.startsWith('#')) continue;

      for (const statement of splitUnquoted(line, ';')) {
        if (!statement || statement.startsWith('#')) continue;

        const pipelineParts = splitUnquoted(statement, '|');
        const transformParts = pipelineParts.slice(1);
        const redirection = splitRedirection(pipelineParts[0]);
        const command = redirection.command.trim();
        const transforms = parseTransforms(transformParts, notes);
        const tokens = shellSplit(command);

        if (tokens.length === 1 && isAssignmentToken(tokens[0])) {
          applyAssignment(tokens[0], env, notes);
          continue;
        }

        if (tokens.some((token) => token === 'curl')) {
          const parsed = parseCurl(command, env, transforms, redirection.redirect, notes);
          if (parsed.request) {
            parsed.request.source = stripPresentationCodecsFromText(statement.trim());
            requests.push(parsed.request);
          }
          else if (parsed.reason) notes.push(parsed.reason);
        }
      }
    }

    if (!requests.length) {
      const parsed = parseHttpSnippet(text);
      if (parsed?.request) requests.push(parsed.request);
      else if (parsed?.reason) notes.push(parsed.reason);
    }

    return {
      text,
      requests,
      notes: [...new Set(notes)],
      env
    };
  }

  function commandFor(request) {
    const pieces = ['curl', '-i', '-sS', '-X', request.method];
    for (const [name, value] of Object.entries(request.headers || {})) {
      pieces.push('-H', quote(`${name}: ${value}`));
    }
    if (request.body) pieces.push('--data-raw', quote(request.body));
    pieces.push(quote(requestUrlFor(request)));
    return pieces.join(' ');
  }

  function isRunAllMode(run = selectedRun) {
    return runMode === 'all' && (run?.requests.length || 0) > 1;
  }

  function previewCommands(run = selectedRun) {
    const original = run?.context?.originalText || run?.text || '';
    return displaySnippetText(original).trim();
  }

  function previewOpenUrl(run = selectedRun) {
    if (!run?.requests.length) return '';
    const request = isRunAllMode(run) ? run.requests[0] : run.requests[run.selected];
    if (!['GET', 'HEAD'].includes(request.method)) return '';
    return requestUrlFor(request);
  }

  function quote(value) {
    return `'${String(value).replace(/'/g, "'\\''")}'`;
  }

  function renderCommandPreview(pre, text, lang = 'bash') {
    if (!pre) return;
    let code = pre.querySelector('code');
    if (!code) {
      pre.textContent = '';
      code = document.createElement('code');
      pre.appendChild(code);
    }
    code.className = `language-${lang || 'bash'}`;
    code.textContent = text;
    highlightRunnerCode(code, lang);
  }

  function detectHighlightLanguage(text) {
    const trimmed = String(text || '').trim();
    if (!trimmed) return 'bash';
    if (/^[\[{]/.test(trimmed)) {
      try {
        JSON.parse(trimmed);
        return 'json';
      } catch {
        // fall through
      }
    }
    if (/^curl\s/m.test(trimmed) && /HTTP\/\d/m.test(trimmed)) return 'http';
    if (/^curl\s/m.test(trimmed)) return 'bash';
    if (/HTTP\/\d/m.test(trimmed) || /^(GET|POST|PUT|PATCH|DELETE|HEAD|OPTIONS)\s+/m.test(trimmed)) return 'http';
    return 'bash';
  }

  function highlightRunnerCode(code, lang) {
    if (!code || !window.Prism) return;
    const language = lang || detectHighlightLanguage(code.textContent);
    code.className = `language-${language}`;
    window.Prism.highlightElement(code);
  }

  function buildHighlightedPre(text, className) {
    const lang = detectHighlightLanguage(text);
    return `<pre class="${className}"><code class="language-${lang}">${escapeHtml(text)}</code></pre>`;
  }

  function highlightRunnerBlocks(root) {
    if (!root || !window.Prism) return;
    root.querySelectorAll('code[class*="language-"]').forEach((code) => {
      highlightRunnerCode(code);
    });
  }

  function statusMetaClass(item) {
    if (item.failed) return 'is-failed';
    if (/^2\d{2}/.test(String(item.status || ''))) return 'is-success';
    return '';
  }

  function ensureChrome() {
    if (!drawer) {
      drawer = document.createElement('div');
      drawer.className = 'hb-runner-drawer';
      drawer.setAttribute('role', 'dialog');
      drawer.setAttribute('aria-modal', 'true');
      drawer.setAttribute('aria-labelledby', 'hb-runner-title');
      drawer.setAttribute('aria-hidden', 'true');
      drawer.innerHTML = [
        '<div class="hb-runner-backdrop" data-hb-close aria-hidden="true"></div>',
        '<div class="hb-runner-panel">',
        '  <header class="hb-runner-header">',
        '    <div class="hb-runner-header-text">',
        '      <p class="hb-runner-kicker">Run against node</p>',
        '      <h2 id="hb-runner-title">Commands</h2>',
        '    </div>',
        '    <button type="button" class="hb-runner-close" data-hb-close aria-label="Close modal">' +
        '      <svg viewBox="0 0 256 256" fill="currentColor" aria-hidden="true">' +
        '        <path d="M205.66,194.34a8,8,0,0,1-11.32,11.32L128,139.31,61.66,205.66a8,8,0,0,1-11.32-11.32L116.69,128,50.34,61.66A8,8,0,0,1,61.66,50.34L128,116.69l66.34-66.35a8,8,0,0,1,11.32,11.32L139.31,128Z"/>' +
        '      </svg>' +
        '    </button>',
        '  </header>',
        '  <div class="hb-runner-body">',
        '    <section class="hb-runner-section hb-runner-node-section">',
        '      <div class="hb-runner-node-row">',
        '        <div class="hb-runner-node-field">',
        '          <label class="hb-runner-node-prefix" for="hb-runner-node">Node URL</label>',
        '          <input id="hb-runner-node" class="hb-runner-node-input" type="url" spellcheck="false" autocomplete="off" placeholder="http://localhost:8734">',
        '        </div>',
        '        <button type="button" class="hb-runner-reset" data-hb-reset>Reset</button>',
        '      </div>',
        '      <div class="hb-runner-codec-row" data-hb-codec-controls>',
        '        <label class="hb-runner-codec-label" for="hb-runner-codec">Codec</label>',
        '        <select id="hb-runner-codec" class="hb-runner-codec-select" data-hb-codec-select>',
        CODEC_OPTIONS.map((codec) => (
          `          <option value="${escapeHtml(codec.id)}" title="${escapeHtml(codec.title)}">${escapeHtml(codec.label)}</option>`
        )).join(''),
        '        </select>',
        `        <span class="hb-runner-codec-help" tabindex="0" aria-label="${escapeHtml(CODEC_HELP)}" data-tip="${escapeHtml(CODEC_HELP)}">?</span>`,
        '      </div>',
        '    </section>',
        '    <section class="hb-runner-section hb-runner-steps" data-hb-steps></section>',
        '    <section class="hb-runner-section hb-runner-command">',
        '      <div class="hb-runner-result-shell">',
        '        <pre class="hb-runner-command-preview" data-hb-command><code class="language-bash"></code></pre>',
        '        <div class="hb-output" data-hb-output role="region" aria-label="Results" hidden></div>',
        '      </div>',
        '    </section>',
        '    <div class="hb-runner-actions">',
        '      <div class="hb-runner-action-main">',
        '        <div class="hb-runner-tabs hb-runner-run-tabs" role="tablist" aria-label="Run mode" data-hb-run-tabs>',
        '          <button type="button" class="hb-runner-tab is-active" data-hb-run-mode="all" role="tab" aria-selected="true">Run all</button>',
        '          <button type="button" class="hb-runner-tab" data-hb-run-mode="selected" role="tab" aria-selected="false">Run selected</button>',
        '        </div>',
        '        <button type="button" class="hb-runner-btn hb-runner-btn-primary" data-hb-run>' +
        ACTION_ICONS.run.replace('hb-code-action-icon', 'hb-runner-btn-icon') +
        '<span>Run</span></button>',
        '      </div>',
        '      <div class="hb-runner-actions-trail">',
        '        <span class="hb-runner-status" data-hb-status>Idle</span>',
        '        <a class="hb-runner-open-url" data-hb-open target="_blank" rel="noreferrer" aria-label="Open URL">',
        '          <svg viewBox="0 0 256 256" fill="currentColor" aria-hidden="true"><path d="M128,24A104,104,0,1,0,232,128,104.11,104.11,0,0,0,128,24Zm88,104a87.61,87.61,0,0,1-3.33,24H174.16a157.31,157.31,0,0,0,0-48h38.51A87.61,87.61,0,0,1,216,128ZM102,168H154a115.11,115.11,0,0,1-26,45A115.27,115.27,0,0,1,102,168Zm-3.9-24a140.19,140.19,0,0,1,0-32h59.88a140.19,140.19,0,0,1,0,32ZM40,128a87.61,87.61,0,0,1,3.33-24H81.84a157.31,157.31,0,0,0,0,48H43.33A87.61,87.61,0,0,1,40,128ZM154,88H102a115.11,115.11,0,0,1,26-45A115.27,115.27,0,0,1,154,88Zm52.59,0H170.49a142.84,142.84,0,0,0-20.26-45A88.32,88.32,0,0,1,206.59,88ZM105.77,43A142.84,142.84,0,0,0,85.51,88H49.41A88.32,88.32,0,0,1,105.77,43ZM49.41,168H85.51a142.84,142.84,0,0,0,20.26,45A88.32,88.32,0,0,1,49.41,168Zm100.82,45a142.84,142.84,0,0,0,20.26-45h36.1A88.32,88.32,0,0,1,150.23,213Z"/></svg>',
        '        </a>',
        '      </div>',
        '    </div>',
        '  </div>',
        '</div>'
      ].join('');
      document.body.appendChild(drawer);

      nodeInput = drawer.querySelector('#hb-runner-node');
      codecSelect = drawer.querySelector('[data-hb-codec-select]');
      nodeInput.value = activeNode;
      nodeInput.addEventListener('change', () => setActiveNode(nodeInput.value));
      nodeInput.addEventListener('keydown', (event) => {
        if (event.key === 'Enter') {
          event.preventDefault();
          setActiveNode(nodeInput.value);
        }
      });
      codecSelect.addEventListener('change', () => {
        selectedCodecId = CODEC_BY_ID.has(codecSelect.value) ? codecSelect.value : 'none';
        resetRunState();
        if (selectedRun) renderDrawer(selectedRun);
      });
      drawer.querySelector('[data-hb-reset]').addEventListener('click', () => setActiveNode(DEFAULT_NODE));
      drawer.querySelectorAll('[data-hb-close]').forEach((button) => {
        button.addEventListener('click', closeDrawer);
      });
      document.addEventListener('keydown', (event) => {
        if (event.key === 'Escape' && drawer?.classList.contains('is-open')) closeDrawer();
      });
      drawer.querySelector('[data-hb-run]').addEventListener('click', () => executeRun());
      drawer.querySelectorAll('[data-hb-run-mode]').forEach((button) => {
        button.addEventListener('click', () => {
          const nextMode = button.getAttribute('data-hb-run-mode') || 'selected';
          if (nextMode === runMode) return;
          runMode = nextMode;
          resetRunState();
          if (selectedRun) renderDrawer(selectedRun);
        });
      });
    }
  }

  function updateResultPanelView() {
    if (!drawer) return;
    const preview = drawer.querySelector('[data-hb-command]');
    const output = drawer.querySelector('[data-hb-output]');
    const hasResults = showResults && (drawerOutput.items.length > 0 || Boolean(drawerOutput.output));
    if (preview) preview.hidden = hasResults;
    if (output) output.hidden = !hasResults;
  }

  function updateDrawerNode() {
    if (!drawer) return;
    const input = drawer.querySelector('#hb-runner-node');
    if (input) input.value = activeNode;
    if (selectedRun) {
      const selected = selectedRun.selected;
      const next = analyzeSnippet(selectedRun.context.originalText || selectedRun.text);
      selectedRun = {
        ...next,
        context: selectedRun.context,
        selected: Math.min(selected, Math.max(next.requests.length - 1, 0))
      };
      renderDrawer(selectedRun);
    }
  }

  function defaultRunMode(requestCount) {
    return requestCount > 1 ? 'all' : 'selected';
  }

  function defaultCodecForRun(run) {
    const explicit = (run?.requests || [])
      .map((request) => request.defaultCodecId)
      .find((codecId) => codecId && codecId !== 'none');
    return CODEC_BY_ID.has(explicit) ? explicit : 'none';
  }

  function renderCodecControls(run) {
    if (!drawer) return;
    const controls = drawer.querySelector('[data-hb-codec-controls]');
    if (controls) controls.hidden = !run?.requests.length;
    if (codecSelect) codecSelect.value = CODEC_BY_ID.has(selectedCodecId) ? selectedCodecId : 'none';
  }

  function isSuccessStatus(status) {
    return /^2\d{2}/.test(String(status || ''));
  }

  function completeStepFromResult(stepIndex, result) {
    if (stepIndex === undefined || stepIndex < 0 || !selectedRun) return;
    if (result.failed || !isSuccessStatus(result.status)) completedSteps.delete(stepIndex);
    else completedSteps.add(stepIndex);
    renderSteps(selectedRun);
  }

  function renderSteps(run) {
    if (!drawer || !run?.requests.length) return;
    const steps = drawer.querySelector('[data-hb-steps]');
    if (!steps) return;
    const runAllActive = isRunAllMode(run);

    steps.classList.toggle('is-run-all', runAllActive);
    steps.innerHTML = run.requests.map((request, index) => {
      const active = runAllActive || index === run.selected;
      const complete = completedSteps.has(index);
      const classes = [active ? 'is-active' : '', complete ? 'is-complete' : ''].filter(Boolean).join(' ');
      const indexMarkup = complete ? STEP_CHECK_ICON : String(index + 1);
      return (
        `<button type="button" class="${classes}" data-hb-step="${index}">` +
        `<span class="hb-runner-step-index${complete ? ' is-complete' : ''}">${indexMarkup}</span>` +
        `<span class="hb-runner-step-method">${escapeHtml(request.method)}</span>` +
        `<span class="hb-runner-step-path">${escapeHtml(new URL(request.url).pathname || '/')}</span>` +
        '</button>'
      );
    }).join('');

    steps.querySelectorAll('[data-hb-step]').forEach((button) => {
      button.addEventListener('click', () => {
        run.selected = Number(button.getAttribute('data-hb-step'));
        if (runAllActive) runMode = 'selected';
        renderDrawer(run);
      });
    });
  }

  function renderRunModeTabs() {
    if (!drawer || !selectedRun) return;
    const tabs = drawer.querySelector('[data-hb-run-tabs]');
    const multi = selectedRun.requests.length > 1;
    if (!multi) runMode = 'selected';
    if (tabs) tabs.hidden = !multi;
    drawer.querySelectorAll('[data-hb-run-mode]').forEach((button) => {
      const active = button.getAttribute('data-hb-run-mode') === runMode;
      button.classList.toggle('is-active', active);
      button.setAttribute('aria-selected', active ? 'true' : 'false');
    });
  }

  function resetRunState() {
    showResults = false;
    completedSteps = new Set();
    drawerOutput = {
      output: '',
      requests: '',
      items: []
    };
    setRunnerStatus('Idle', 'muted');
  }

  function executeRun() {
    if (!selectedRun?.requests.length) return;
    completedSteps = new Set();
    renderSteps(selectedRun);
    if (runMode === 'all' && selectedRun.requests.length > 1) runAll();
    else runSelected();
  }

  function openDrawer(analysis, context, autorun) {
    ensureChrome();
    runMode = defaultRunMode(analysis.requests.length);
    resetRunState();
    selectedRun = {
      ...analysis,
      context,
      selected: 0
    };
    selectedCodecId = defaultCodecForRun(selectedRun);
    drawer.classList.add('is-open');
    drawer.setAttribute('aria-hidden', 'false');
    document.body.classList.add('hb-runner-open');
    renderDrawer(selectedRun);
    if (autorun && selectedRun.requests.length) executeRun();
  }

  function closeDrawer() {
    if (!drawer) return;
    drawer.classList.remove('is-open');
    drawer.setAttribute('aria-hidden', 'true');
    document.body.classList.remove('hb-runner-open');
  }

  function setRunnerStatus(text, tone = 'default') {
    if (!drawer) return;
    const status = drawer.querySelector('[data-hb-status]');
    if (!status) return;
    status.textContent = text;
    status.dataset.status = tone;
  }

  function renderDrawer(run) {
    if (!drawer || !run) return;
    showResults = false;
    const steps = drawer.querySelector('[data-hb-steps]');
    const command = drawer.querySelector('[data-hb-command]');
    const open = drawer.querySelector('[data-hb-open]');
    renderCodecControls(run);

    if (!run.requests.length) {
      steps.innerHTML = run.notes.map((note) => `<p>${escapeHtml(note)}</p>`).join('') || '<p>This snippet is not a direct HTTP request.</p>';
      renderCommandPreview(command, stripPresentationCodecsFromText(run.text.trim()), run.context.lang || 'bash');
      open.removeAttribute('href');
      setRunnerStatus('Not runnable', 'muted');
      updateResultPanelView();
      return;
    }

    const runAllActive = isRunAllMode(run);
    renderSteps(run);

    const commandSection = drawer.querySelector('.hb-runner-command');
    if (commandSection) commandSection.classList.toggle('is-run-all', runAllActive);

    renderCommandPreview(command, previewCommands(run), 'bash');
    const openUrl = previewOpenUrl(run);
    if (openUrl) {
      open.href = openUrl;
      open.hidden = false;
    } else {
      open.removeAttribute('href');
      open.hidden = true;
    }
    renderRunModeTabs();
    updateResultPanelView();
  }

  async function runSelected() {
    if (!selectedRun?.requests.length) return;
    const stepIndex = selectedRun.selected;
    await executeRequest(selectedRun.requests[stepIndex], { stepIndex });
  }

  async function runAll() {
    if (!selectedRun?.requests.length) return;
    const runStarted = performance.now();
    const outputChunks = [];
    const requestChunks = [];
    const outputItems = [];

    setDrawerOutput('', '', selectedRun.requests.map(pendingItemFor));

    for (let index = 0; index < selectedRun.requests.length; index += 1) {
      const request = selectedRun.requests[index];
      setRunnerStatus(`Running ${index + 1}/${selectedRun.requests.length}`, 'running');
      const result = await executeRequest(request, { collect: true, stepIndex: index });
      const formatted = formatCollectedOutput(request, result);
      outputChunks.push(formatted.text);
      outputItems.push(formatted.item);
      requestChunks.push(result.detail);
      completeStepFromResult(index, result);
    }
    await delayRemaining(runStarted);
    setRunnerStatus(`Done ${selectedRun.requests.length}`, 'success');
    setDrawerOutput(
      outputChunks.filter(Boolean).join('\n\n') || '[no output]',
      requestChunks.join('\n\n'),
      outputItems
    );
  }

  async function executeRequest(request, options = {}) {
    const runStarted = performance.now();
    setRunnerStatus('Running', 'running');
    if (!options.collect) {
      setDrawerOutput('', '', [pendingItemFor(request)]);
    }

    const headers = {};
    for (const [name, value] of Object.entries(request.headers || {})) {
      if (!FORBIDDEN_HEADERS.has(name.toLowerCase())) headers[name] = value;
    }

    const started = performance.now();
    const finalUrl = requestUrlFor(request);
    try {
      const response = await fetch(finalUrl, {
        method: request.method,
        headers,
        body: request.body && !['GET', 'HEAD'].includes(request.method) ? request.body : undefined,
        mode: 'cors'
      });
      const elapsed = Math.round(performance.now() - started);
      const text = await response.text();
      const headerText = Array.from(response.headers.entries())
        .map(([name, value]) => `${name}: ${value}`)
        .join('\n');
      const body = text.length > 120000 ? `${text.slice(0, 120000)}\n\n[truncated ${text.length - 120000} bytes]` : text;
      const rawOutput = request.method === 'HEAD' || request.includeHeaders
        ? [`HTTP ${response.status} ${response.statusText}`, headerText, request.method === 'HEAD' ? '' : body].filter(Boolean).join('\n')
        : (body || '[empty response body]');
      const usefulOutput = request.suppressOutput ? '' : applyTransforms(rawOutput, request.transforms);
      const transformed = Boolean((request.transforms || []).length);
      const detail = [
        commandFor(request),
        `${request.method} ${finalUrl}`,
        `HTTP ${response.status} ${response.statusText} (${elapsed}ms)`,
        headerText,
        '',
        body || '[empty response body]'
      ].join('\n');
      setRunnerStatus(`${response.status} ${elapsed}ms`, response.ok ? 'success' : 'failed');
      const result = {
        output: usefulOutput || '[no output]',
        detail,
        status: `${response.status} ${elapsed}ms`,
        body,
        rawOutput,
        transformed,
        transforms: request.transforms || [],
        contentType: response.headers.get('content-type') || ''
      };
      if (!options.collect) {
        await delayRemaining(runStarted);
        setDrawerOutput(result.output, result.detail, [outputItemFor(request, result)]);
        if (options.stepIndex !== undefined) completeStepFromResult(options.stepIndex, result);
      }
      return result;
    } catch (error) {
      const detail = [
        commandFor(request),
        `${request.method} ${requestUrlFor(request)}`,
        'Request failed',
        '',
        String(error && error.message ? error.message : error),
        '',
        'If the docs are served over HTTPS and the node is http://localhost, the browser may block the request. Try another HTTPS HyperBEAM node.'
      ].join('\n');
      setRunnerStatus('Failed', 'failed');
      const result = { output: detail, detail, status: 'Failed', failed: true };
      if (!options.collect) {
        await delayRemaining(runStarted);
        setDrawerOutput(result.output, result.detail, [outputItemFor(request, result)]);
        if (options.stepIndex !== undefined) completeStepFromResult(options.stepIndex, result);
      }
      return result;
    }
  }

  function formatCollectedOutput(request, result) {
    const label = commandFor(request);
    const text = !result.output || result.output === '[no output]'
      ? `$ ${label}\n[no output]`
      : `$ ${label}\n${result.output}`;
    return {
      text,
      item: outputItemFor(request, result)
    };
  }

  function outputItemFor(request, result) {
    const output = result.output || '[no output]';
    const outputJson = parseJsonOutput(output);
    const bodyJson = outputJson === null ? parseJsonOutput(result.body) : null;
    const rawOutputJson = outputJson === null && bodyJson === null ? parseJsonOutput(result.rawOutput) : null;
    const json = outputJson ?? bodyJson ?? rawOutputJson;
    const jsonSource = outputJson !== null ? 'output' : (bodyJson !== null ? 'body' : (rawOutputJson !== null ? 'raw' : ''));
    const rawJson = outputJson !== null ? output : (bodyJson !== null ? result.body : (rawOutputJson !== null ? result.rawOutput : ''));

    return {
      command: commandFor(request),
      method: request.method,
      url: requestUrlFor(request),
      status: result.status || '',
      output,
      failed: Boolean(result.failed),
      json,
      jsonSource,
      rawJson,
      commandOutput: output,
      transformed: Boolean(result.transformed),
      transformSummary: describeTransforms(result.transforms || [])
    };
  }

  function describeTransforms(transforms) {
    return transforms.map((transform) => {
      if (transform.type === 'head-chars') return `head -c ${transform.count}`;
      if (transform.type === 'head-lines') return `head -n ${transform.count}`;
      if (transform.type === 'grep') return `grep ${transform.ignoreCase ? '-i ' : ''}${transform.extended ? '-E ' : ''}${transform.pattern}`.trim();
      return transform.type;
    }).filter(Boolean).join(', ');
  }

  function parseJsonOutput(text) {
    const value = String(text || '').trim();
    if (!/^[\[{]/.test(value)) return null;
    try {
      return JSON.parse(value);
    } catch {
      return null;
    }
  }

  function setDrawerOutput(output, requests, items = []) {
    drawerOutput = {
      output: output ?? '',
      requests: requests || '',
      items: Array.isArray(items) ? items : []
    };
    showResults = true;
    renderResults();
  }

  function renderResults() {
    if (!drawer) return;
    updateResultPanelView();
    if (!showResults) return;
    const target = drawer.querySelector('[data-hb-output]');
    if (!target) return;
    target.className = 'hb-output';
    if (drawerOutput.items.length) {
      target.innerHTML = drawerOutput.items.map(renderOutputItem).join('');
      highlightRunnerBlocks(target);
      return;
    }
    target.innerHTML = buildHighlightedPre(drawerOutput.output || '[empty]', 'hb-output-text');
    highlightRunnerBlocks(target);
  }

  function renderOutputItem(item) {
    if (item.pending) {
      return [
        '<section class="hb-output-entry is-pending">',
        '  <div class="hb-runner-result-meta">',
        `    <span class="hb-runner-result-method">${escapeHtml(item.method || 'GET')}</span>`,
        '    <span class="hb-runner-result-status is-pending">',
        '      <span class="hb-runner-result-spinner" aria-hidden="true"></span>',
        '      Fetching…',
        '    </span>',
        '  </div>',
        '  <div class="hb-runner-result-content is-pending">',
        '    <p class="hb-runner-result-loading">Waiting for response…</p>',
        '  </div>',
        '</section>'
      ].join('');
    }

    const result = item.json !== null
      ? renderJsonViewer(item.json, item)
      : buildHighlightedPre(item.output || '[no output]', 'hb-output-text');
    return [
      `<section class="hb-output-entry${item.failed ? ' is-error' : ''}">`,
      '  <div class="hb-runner-result-meta">',
      `    <span class="hb-runner-result-method">${escapeHtml(item.method || 'GET')}</span>`,
      `    <span class="${statusMetaClass(item)}">${escapeHtml(item.status || '')}</span>`,
      '  </div>',
      '  <div class="hb-runner-result-content">',
      result,
      '  </div>',
      '</section>'
    ].join('');
  }

  function renderJsonViewer(value, item) {
    const usedFullResponse = item.jsonSource && item.jsonSource !== 'output';
    const notice = usedFullResponse
      ? [
        '<div class="hb-output-note">',
        `  <span>Formatted from the full response${item.transformSummary ? `; command output used ${escapeHtml(item.transformSummary)}` : ''}.</span>`,
        '</div>',
        '<details class="hb-command-output">',
        '<summary>Command output</summary>',
        buildHighlightedPre(item.commandOutput || '[no output]', 'hb-output-text'),
        '</details>'
      ].join('')
      : '';

    return [
      '<div class="hb-json-viewer">',
      notice,
      renderJsonNode(value),
      '<details class="hb-json-raw">',
      '<summary>Raw JSON</summary>',
      buildHighlightedPre(item.rawJson || item.output || '', 'hb-output-text'),
      '</details>',
      '</div>'
    ].join('');
  }

  function renderJsonNode(value, label = '', depth = 0) {
    const labelHtml = label ? `<span class="hb-json-key">${escapeHtml(JSON.stringify(label))}</span><span class="hb-json-sep">:</span>` : '';
    const open = depth < 2 ? ' open' : '';

    if (Array.isArray(value)) {
      const children = value.map((entry, index) => renderJsonNode(entry, String(index), depth + 1)).join('');
      return [
        `<details class="hb-json-node"${open}>`,
        `<summary>${labelHtml}<span class="hb-json-type">Array</span><span class="hb-json-count">${value.length}</span></summary>`,
        `<div class="hb-json-children">${children || '<span class="hb-json-empty">empty</span>'}</div>`,
        '</details>'
      ].join('');
    }

    if (value && typeof value === 'object') {
      const entries = Object.entries(value);
      const children = entries.map(([key, entry]) => renderJsonNode(entry, key, depth + 1)).join('');
      return [
        `<details class="hb-json-node"${open}>`,
        `<summary>${labelHtml}<span class="hb-json-type">Object</span><span class="hb-json-count">${entries.length}</span></summary>`,
        `<div class="hb-json-children">${children || '<span class="hb-json-empty">empty</span>'}</div>`,
        '</details>'
      ].join('');
    }

    return `<div class="hb-json-line">${labelHtml}${renderJsonPrimitive(value)}</div>`;
  }

  function renderJsonPrimitive(value) {
    if (value === null) return '<span class="hb-json-null">null</span>';
    if (typeof value === 'string') return `<span class="hb-json-string">${escapeHtml(JSON.stringify(value))}</span>`;
    if (typeof value === 'number') return `<span class="hb-json-number">${escapeHtml(String(value))}</span>`;
    if (typeof value === 'boolean') return `<span class="hb-json-boolean">${value}</span>`;
    return `<span>${escapeHtml(String(value))}</span>`;
  }

  function refreshExamples() {
    ensureChrome();
    const pageEnv = {};
    document.querySelectorAll('.markdown-section pre').forEach((pre) => {
      const code = pre.querySelector('code');
      if (!code) return;
      if (!code.dataset.hbOriginal) code.dataset.hbOriginal = code.textContent;

      const lang = (pre.getAttribute('data-lang') || code.className.replace(/^lang(uage)?-/, '') || '').trim();
      const original = code.dataset.hbOriginal;
      const rewritten = displaySnippetText(original);
      if (code.textContent !== rewritten) {
        code.textContent = rewritten;
        if (window.Prism) window.Prism.highlightElement(code);
      }

      pre.querySelector('.hb-code-actions')?.remove();
      const analysis = analyzeSnippet(original, pageEnv);
      Object.assign(pageEnv, analysis.env);
      if (!analysis.requests.length && !analysis.notes.length) return;

      const actions = document.createElement('div');
      actions.className = 'hb-code-actions';
      const button = document.createElement('button');
      button.type = 'button';
      const actionLabel = codeActionLabel(analysis);
      button.innerHTML = renderCodeActionButton(actionLabel);
      button.addEventListener('click', () => {
        openDrawer(analysis, {
          page: (window.location.hash || '#/').replace(/^#\/?/, '/') || '/',
          lang,
          originalText: original
        }, Boolean(analysis.requests.length));
      });
      actions.appendChild(button);
      pre.appendChild(actions);
      pre.classList.add('hb-runnable-pre');
    });
  }

  function escapeHtml(value) {
    return String(value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;');
  }

  window.HBExampleRunner = {
    get node() {
      return activeNode;
    },
    setNode: setActiveNode,
    refresh: refreshExamples
  };
})();
