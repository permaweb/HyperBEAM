// Renders the Markdown of docs pages to HTML, and reads titles, summaries, code
// blocks and spec sections from it.
(function () {
  const cardSummaryMax = 120;
  const recipeCardSummaryMax = 80;
  // The whitespace that trim removes, as Erlang's unicode_util:whitespace()
  // lists it.
  const whitespace = new Set([
    "\r\n", "\t", "\n", "\v", "\f", "\r", " ",
    "\u0085", "\u200e", "\u200f", "\u2028", "\u2029"
  ]);
  const graphemes = new Intl.Segmenter(undefined, { granularity: "grapheme" });
  const utf8Encoder = new TextEncoder();
  // The decoder of cut summaries. With ignoreBOM, a leading U+FEFF is kept as
  // text.
  const utf8Decoder = new TextDecoder("utf-8", { ignoreBOM: true });
  const summarySkipPrefixes = [
    "- **Device name", "- **Depends-on", "- **Status", "Source tests:",
    "Prerequisites:", ">", "<"
  ];
  const inspectOnlyPatterns =
    ["PROCESS_ID", "/path/to/", "BAD_MESSAGE_ID", "$(", "`"];
  const commandPrefixes =
    ["curl ", "GET ", "POST ", "PUT ", "PATCH ", "DELETE ", "HEAD "];
  const rawHtmlLines = new Set([
    "<video class=\"theme-invert-video\" src=\"https://arweave.net/pc73dj9tZtj7AOeIKBGiiOm5ta13FYXzgsqWSePAxiM\" style=\"width: 100%; height: auto; display: block;\" autoplay=\"\" muted=\"\" playsinline=\"\" loop=\"\" controlslist=\"nodownload nofullscreen noremoteplayback\" disablepictureinpicture=\"\" preload=\"auto\"></video>",
    "<div class=\"core-concepts-flex\">",
    "<div class=\"core-concepts-column\">",
    "</div>"
  ]);
  const rawHtmlPrefixes = [
    ["<img class=\"core-concepts-fig ", allowedCoreConceptsImg],
    ["<p class=\"core-concept-header-", allowedCoreConceptHeader],
    ["<span class=\"core-concept-subtitle\">",
      (rest) => rest.endsWith("</span>")],
    ["<p class=\"core-concept-copy\">",
      (rest) => rest.endsWith("</p>") && !rest.includes("<script")]
  ];

  // Returns the Markdown under the first heading titled sectionTitle, up to the
  // next heading of the same or a higher level, with the title as a level-1
  // heading; false when no heading has that title.
  function extractMarkdownSection(markdown, sectionTitle) {
    const lines = markdown.split("\n");
    for (let idx = 0; idx < lines.length; idx += 1) {
      const found = heading(trim(lines[idx]));
      if (found !== false && found[1] === sectionTitle) {
        const body = takeSectionBody(lines, idx + 1, found[0]);
        return [`# ${sectionTitle}`, ...body].join("\n");
      }
    }
    return false;
  }

  // Takes lines from idx up to the next heading whose level number is at most
  // level.
  function takeSectionBody(lines, idx, level) {
    const body = [];
    for (; idx < lines.length; idx += 1) {
      const found = heading(trim(lines[idx]));
      if (found !== false && found[0] <= level) break;
      body.push(lines[idx]);
    }
    return body;
  }

  // Strips leading "N." numbering from a spec section title.
  function specSectionNavLabel(title) {
    let count = 0;
    while (title[count] >= "0" && title[count] <= "9") count += 1;
    if (count > 0 && title[count] === ".") return trim(title.slice(count + 1));
    return title;
  }

  // Lists [id, title] for each level-2 heading of a spec, with ids unique in
  // heading order.
  function specSections(markdown) {
    const usedIds = new Set();
    const sections = [];
    for (const line of markdown.split("\n")) {
      const found = heading(trim(line));
      if (found !== false && found[0] === 2) {
        const plain = stripInlineMarkdown(found[1]);
        sections.push([uniqueHeadingSlug(plain, usedIds), plain]);
      }
    }
    return sections;
  }

  // Returns the [id, title] of the spec section with the id sectionSlug, or
  // undefined.
  function specSectionLookup(markdown, sectionSlug) {
    return specSections(markdown).find(([id]) => id === sectionSlug);
  }

  // Returns the Markdown of the spec section with the id sectionSlug, or ""
  // when there is none.
  function specSectionMarkdown(markdown, sectionSlug) {
    const section = specSectionLookup(markdown, sectionSlug);
    if (section === undefined) return "";
    const sectionMarkdown =
      extractMarkdownSection(dropFirstH1(markdown), section[1]);
    return sectionMarkdown === false ? "" : sectionMarkdown;
  }

  // Renders Markdown to HTML. The options are resolveLink(url, label), which
  // gives the href of a link, stripNumberedHeadings, and html: with false, the
  // allow-listed lines of raw HTML render as text.
  function renderMarkdown(markdown, opts) {
    return renderMarkdownLines(markdown.split("\n"), false, opts || {});
  }

  // Renders Markdown to HTML, with an id on each level-2 heading.
  function renderMarkdownWithHeadingIds(markdown, opts) {
    return renderMarkdownLines(markdown.split("\n"), true, opts || {});
  }

  // Renders lines as HTML blocks, joining the lines between blocks into
  // paragraphs.
  function renderMarkdownLines(lines, addHeadingIds, opts) {
    const acc = [];
    const usedIds = new Set();
    let para = [];
    let idx = 0;
    while (idx < lines.length) {
      const trimmed = trim(lines[idx]);
      const block =
        renderBlock(trimmed, lines, idx + 1, addHeadingIds, usedIds, opts);
      if (block === false) {
        para.push(trimmed);
        idx += 1;
      } else {
        acc.push(flushParagraph(para, opts), block[0]);
        para = [];
        idx = block[1];
      }
    }
    acc.push(flushParagraph(para, opts));
    return acc.join("");
  }

  // Renders the block that starts at a trimmed line as [html, index after the
  // block], or returns false for a paragraph line.
  function renderBlock(trimmed, lines, next, addHeadingIds, usedIds, opts) {
    const lang = fenceLanguage(trimmed);
    if (lang !== false) {
      const [codeLines, after] = takeCodeBlock(lines, next);
      return [renderCodeBlock(lang, codeLines), after];
    }
    const table = tableBlock(trimmed, lines, next, opts);
    if (table !== false) return table;
    if (trimmed === "") return ["", next];
    const quote = blockquoteText(trimmed);
    if (quote !== false) {
      const [quoteLines, after] = takeBlockquote(lines, next, [quote]);
      return [renderBlockquote(quoteLines, opts), after];
    }
    const html = opts.html === false ? false : rawHtmlLine(trimmed);
    if (html !== false) return [html, next];
    const found = heading(trimmed);
    if (found !== false) {
      const [level, text] = found;
      return [renderHeading(level, text, addHeadingIds, usedIds, opts), next];
    }
    const bullet = bulletText(trimmed);
    if (bullet !== false) {
      const [items, after] = takeListBlock(lines, next, "unordered", bullet);
      return [renderList("ul", items, opts), after];
    }
    const numbered = numberedText(trimmed);
    if (numbered !== false) {
      const [items, after] = takeListBlock(lines, next, "ordered", numbered);
      return [renderList("ol", items, opts), after];
    }
    return false;
  }

  // Takes the lines of a code block up to its closing fence, as [lines, index
  // after the fence].
  function takeCodeBlock(lines, idx) {
    const codeLines = [];
    for (; idx < lines.length; idx += 1) {
      if (isFence(lines[idx])) return [codeLines, idx + 1];
      codeLines.push(lines[idx]);
    }
    return [codeLines, idx];
  }

  // Renders paragraph lines as one paragraph, or "" when there are none.
  function flushParagraph(para, opts) {
    if (para.length === 0) return "";
    return `<p>${renderInline(para.join(" "), opts)}</p>`;
  }

  // Renders a heading two levels lower (h3 to h6), with a unique id on a
  // level-2 heading when addHeadingIds is true.
  function renderHeading(level0, text, addHeadingIds, usedIds, opts) {
    const tag = Math.max(3, Math.min(6, level0 + 2));
    const content = renderInline(headingDisplayText(text, opts), opts);
    if (addHeadingIds === true && level0 === 2) {
      const id = uniqueHeadingSlug(stripInlineMarkdown(text), usedIds);
      return `<h${tag} id="${esc(id)}">${content}</h${tag}>`;
    }
    return `<h${tag}>${content}</h${tag}>`;
  }

  // Returns the heading text to show: without backticks and "N." numbering when
  // opts.stripNumberedHeadings is true.
  function headingDisplayText(text, opts) {
    if (opts.stripNumberedHeadings === true) {
      return specSectionNavLabel(stripInlineMarkdown(text));
    }
    return text;
  }

  // Returns the slug of a heading, with a -3, -4, ... suffix when the slug is
  // in usedIds, and adds it to usedIds.
  function uniqueHeadingSlug(text, usedIds) {
    const base = headingSlugBase(text);
    if (base === "") return base;
    let candidate = base;
    for (let suffix = 3; usedIds.has(candidate); suffix += 1) {
      candidate = `${base}-${suffix}`;
    }
    usedIds.add(candidate);
    return candidate;
  }

  // Makes a slug: lowercases text, keeps a-z, 0-9, "_", "-" and spaces, turns
  // spaces into hyphens and runs of hyphens into one.
  function headingSlugBase(text) {
    const kept = toLower(text).replace(/[^a-z0-9_ -]/gu, "");
    return kept.replaceAll(" ", "-").replace(/-{2,}/g, "-");
  }

  // Renders a code block, with its language in the class names and data-lang.
  function renderCodeBlock(lang, lines) {
    const normLang = esc(normalizeLang(lang));
    const code = esc(lines.join("\n"));
    return `<pre class="language-${normLang}" data-lang="${normLang}">` +
      `<code class="language-${normLang}">${code}</code></pre>`;
  }

  // Renders a table when the header line is followed by a separator line, as
  // [html, index after the table], or returns false.
  function tableBlock(header, lines, next, opts) {
    if (next >= lines.length) return false;
    if (!isTableRow(header) || !isTableSeparator(trim(lines[next]))) {
      return false;
    }
    const [rows, after] = takeTableRows(lines, next + 1);
    return [renderTable([header, ...rows], opts), after];
  }

  // Tells whether a trimmed line is a table row: not empty and has a pipe.
  function isTableRow(line) {
    return line !== "" && line.includes("|");
  }

  // Tells whether a line is a table separator: every cell is a separator cell.
  function isTableSeparator(line) {
    const cells = parseTableRow(line);
    return cells.length > 0 && cells.every(isTableSeparatorCell);
  }

  // Tells whether a cell has a hyphen and nothing but hyphens, colons and inner
  // whitespace.
  function isTableSeparatorCell(cell) {
    const trimmed = trim(cell);
    const rest = trimmed.replaceAll(":", "").replaceAll("-", "");
    return trimmed.includes("-") && trim(rest) === "";
  }

  // Takes the trimmed table rows from idx, as [rows, index after them].
  function takeTableRows(lines, idx) {
    const rows = [];
    for (; idx < lines.length; idx += 1) {
      const trimmed = trim(lines[idx]);
      if (!(isTableRow(trimmed) && trimmed !== "")) break;
      rows.push(trimmed);
    }
    return [rows, idx];
  }

  // Renders a table from its header row and body rows.
  function renderTable(rows, opts) {
    const [header, ...body] = rows;
    return [
      "<table><thead>",
      renderTableRow("th", parseTableRow(header), opts),
      "</thead><tbody>",
      ...body.map((row) => renderTableRow("td", parseTableRow(row), opts)),
      "</tbody></table>"
    ].join("");
  }

  // Renders a table row, with each cell in a tag element.
  function renderTableRow(tag, cells, opts) {
    const html =
      cells.map((cell) => `<${tag}>${renderInline(cell, opts)}</${tag}>`);
    return `<tr>${html.join("")}</tr>`;
  }

  // Splits a table row into trimmed cells, without one leading and one trailing
  // pipe.
  function parseTableRow(row) {
    const trimmed = trim(row);
    const withoutLeading = trimmed.startsWith("|") ? trimmed.slice(1) : trimmed;
    const withoutOuter = withoutLeading.endsWith("|")
      ? withoutLeading.slice(0, -1)
      : withoutLeading;
    return withoutOuter.split("|").map((cell) => trim(cell));
  }

  // Renders inline Markdown: links, `code` and **strong** text, escaping all
  // other text.
  function renderInline(text, opts) {
    const acc = [];
    let rest = text;
    while (rest !== "") {
      const markers = inlineMarkers(rest);
      if (markers.length === 0) {
        acc.push(esc(rest));
        break;
      }
      const [pos, kind, payload] = markers[0];
      acc.push(esc(rest.slice(0, pos)));
      if (kind === "link") {
        const [label, url, totalLen] = payload;
        acc.push(renderMarkdownLink(label, url, opts));
        rest = rest.slice(pos + totalLen);
      } else {
        const afterStart = rest.slice(pos + payload.length);
        const endPos = afterStart.indexOf(payload);
        if (endPos === -1) {
          acc.push(esc(payload));
          rest = afterStart;
        } else {
          const inner = afterStart.slice(0, endPos);
          acc.push(kind === "code"
            ? `<code>${esc(inner)}</code>`
            : `<strong>${renderInline(inner, opts)}</strong>`);
          rest = afterStart.slice(endPos + payload.length);
        }
      }
    }
    return acc.join("");
  }

  // Renders a link as an anchor to the href that opts.resolveLink(url, label)
  // returns, or to url without a resolveLink. A result of { code: text } gives
  // the text as code, and an href that is not allowed gives the label as code.
  function renderMarkdownLink(label, url, opts) {
    const target = opts.resolveLink ? opts.resolveLink(url, label) : url;
    if (typeof target !== "string") return `<code>${esc(target.code)}</code>`;
    if (!isAllowedHref(target)) {
      return `<code>${esc(linkCodeLabel(label))}</code>`;
    }
    return `<a href="${esc(target)}">${renderInline(label, opts)}</a>`;
  }

  // Tells whether a browser reads href as a fragment, a relative URL, or an
  // http, https or mailto URL.
  function isAllowedHref(href) {
    // A browser drops leading C0 controls and spaces, and every tab and
    // newline, before it reads the scheme.
    const read = href.replace(/[\t\n\r]/g, "").replace(/^[\x00-\x20]+/, "");
    const scheme = /^([a-z][a-z0-9+.-]*):/i.exec(read);
    if (scheme === null) return true;
    return ["http", "https", "mailto"].includes(scheme[1].toLowerCase());
  }

  // Returns the label of a refused link without backticks, or "reference" when
  // that is empty.
  function linkCodeLabel(label) {
    const text = trim(stripInlineMarkdown(label));
    return text === "" ? "reference" : text;
  }

  // Lists the first "**", the first "`" and the link at the first "[" in text
  // as [pos, kind, payload], by position.
  function inlineMarkers(text) {
    return [
      ...markerMatches(text, "**", "strong"),
      ...markerMatches(text, "`", "code"),
      ...linkMarkerMatches(text)
    ]
      .sort((a, b) => a[0] - b[0]);
  }

  // Lists the link at the first "[" in text as
  // [pos, "link", [label, url, totalLen]], or nothing.
  function linkMarkerMatches(text) {
    const link = parseMarkdownLink(text);
    return link === false ? [] : [[link[0], "link", link.slice(1)]];
  }

  // Parses the "[label](url)" at the first "[" in text as
  // [pos, label, url, totalLen], or returns false.
  function parseMarkdownLink(text) {
    const pos = text.indexOf("[");
    if (pos === -1) return false;
    const afterOpen = text.slice(pos + 1);
    const labelEnd = afterOpen.indexOf("](");
    if (labelEnd === -1) return false;
    const afterParen = afterOpen.slice(labelEnd + 2);
    const urlEnd = afterParen.indexOf(")");
    if (urlEnd === -1) return false;
    const label = afterOpen.slice(0, labelEnd);
    const url = afterParen.slice(0, urlEnd);
    return [pos, label, url, 1 + labelEnd + 2 + urlEnd + 1];
  }

  // Lists the first position of marker in text as [pos, kind, marker], or
  // nothing.
  function markerMatches(text, marker, kind) {
    const pos = text.indexOf(marker);
    return pos === -1 ? [] : [[pos, kind, marker]];
  }

  // Drops the first line of Markdown when it is a level-1 heading.
  function dropFirstH1(markdown) {
    const lines = markdown.split("\n");
    const first = heading(trim(lines[0]));
    if (first === false || first[0] !== 1) return markdown;
    return lines.slice(1).join("\n");
  }

  // Returns the text of the first level-1 heading, or fallback.
  function markdownTitle(markdown, fallback) {
    for (const line of markdown.split("\n")) {
      const found = heading(trim(line));
      if (found !== false && found[0] === 1) return found[1];
    }
    return fallback;
  }

  // Returns the first usable line after a level-1 heading on the first line,
  // without backticks.
  function markdownSummary(markdown) {
    return summaryFromLines(dropFirstH1(markdown).split("\n"));
  }

  // Shortens a summary to its first sentence, cut at 120 UTF-8 bytes.
  function cardSummary(summary) {
    const sentence = firstSentence(trim(String(summary)));
    return truncateCardSummary(sentence, cardSummaryMax);
  }

  // Shortens a recipe's tagline, or else the first clause of its summary, cut
  // at 80 UTF-8 bytes.
  function recipeCardSummary(recipe) {
    const text = recipe.tagline === undefined
      ? firstClause(trim(String(recipe.summary === undefined
        ? ""
        : recipe.summary)))
      : trim(String(recipe.tagline));
    return truncateCardSummary(text, recipeCardSummaryMax);
  }

  // Returns text up to its earliest ". ", ", " or " and ", or its first
  // sentence when it has none past the start.
  function firstClause(text) {
    const pos = earliestClauseSplit(text);
    return pos === undefined ? firstSentence(text) : text.slice(0, pos);
  }

  // Returns the least position of ". ", ", " and " and " in text, or undefined.
  function earliestClauseSplit(text) {
    const positions = [
      clauseSplit(text, ". "),
      clauseSplit(text, ", "),
      clauseSplit(text, " and ")
    ]
      .filter((pos) => pos !== undefined);
    return positions.length === 0 ? undefined : Math.min(...positions);
  }

  // Returns the position of the first sep in text when it is past the start, or
  // undefined.
  function clauseSplit(text, sep) {
    const pos = text.indexOf(sep);
    return pos > 0 ? pos : undefined;
  }

  // Returns text up to and including the period of its first ". ", or all of
  // text.
  function firstSentence(text) {
    const pos = text.indexOf(". ");
    return pos === -1 ? text : text.slice(0, pos + 1);
  }

  // Cuts text longer than max UTF-8 bytes to its first max bytes followed by
  // "...".
  function truncateCardSummary(text, max) {
    const bytes = utf8Encoder.encode(text);
    if (bytes.length <= max) return text;
    return `${utf8Decoder.decode(bytes.subarray(0, max))}...`;
  }

  // Returns the first line, without backticks, that is usable and not a heading
  // or in a code block; "" when there is none.
  function summaryFromLines(lines) {
    let idx = 0;
    while (idx < lines.length) {
      const trimmed = trim(lines[idx]);
      idx += 1;
      if (summaryLineUsable(trimmed)) {
        if (fenceLanguage(trimmed) !== false) {
          idx = takeCodeBlock(lines, idx)[1];
        } else if (heading(trimmed) === false) {
          return stripInlineMarkdown(trimmed);
        }
      }
    }
    return "";
  }

  // Tells whether a trimmed line can be a summary: not empty, and not a
  // metadata line, a quote or HTML.
  function summaryLineUsable(line) {
    if (line === "") return false;
    return !summarySkipPrefixes.some((prefix) => line.startsWith(prefix));
  }

  // Removes the backticks from text.
  function stripInlineMarkdown(text) {
    return text.replaceAll("`", "");
  }

  // Lists the code blocks of Markdown, including an unclosed last block.
  function codeBlocks(markdown) {
    const blocks = [];
    let lang = false;
    let lines = [];
    for (const line of markdown.split("\n")) {
      if (lang === false) {
        lang = fenceLanguage(line);
        lines = [];
      } else if (isFence(line)) {
        blocks.push(codeBlock(blocks.length, lang, lines));
        lang = false;
      } else {
        lines.push(line);
      }
    }
    if (lang !== false) blocks.push(codeBlock(blocks.length, lang, lines));
    return blocks;
  }

  // Describes a code block by its index, language, text, whether it can run,
  // and its command count.
  function codeBlock(index, lang, lines) {
    const text = lines.join("\n");
    const language = normalizeLang(lang);
    return {
      index,
      language,
      text,
      runnable: isRunnableBlock(language, text),
      "command-count": commandCount(text)
    };
  }

  // Counts the lines of text that are curl or HTTP method commands.
  function commandCount(text) {
    return text.split("\n").filter((line) => isCommandLine(trim(line))).length;
  }

  // Tells whether a code block can run: http with a command, or bash with curl,
  // and not inspect-only.
  function isRunnableBlock(lang, text) {
    if (lang === "http") {
      return !inspectOnlyBlock(text) && commandCount(text) > 0;
    }
    if (lang === "bash") {
      return !inspectOnlyBlock(text) && text.includes("curl");
    }
    return false;
  }

  // Tells whether a code block has a placeholder: "<" with ">", PROCESS_ID,
  // /path/to/, BAD_MESSAGE_ID, "$(" or a backtick.
  function inspectOnlyBlock(text) {
    return hasAnglePlaceholder(text) ||
      inspectOnlyPatterns.some((pattern) => text.includes(pattern));
  }

  // Tells whether text has both "<" and ">".
  function hasAnglePlaceholder(text) {
    return text.includes("<") && text.includes(">");
  }

  // Tells whether a trimmed line is "curl", or starts with "curl " or an HTTP
  // method and a space.
  function isCommandLine(line) {
    return line === "curl" ||
      commandPrefixes.some((prefix) => line.startsWith(prefix));
  }

  // Returns [level, text] for a heading line from "# " to "##### ", or false.
  function heading(line) {
    const match = /^(#{1,5}) /.exec(line);
    if (match === null) return false;
    return [match[1].length, line.slice(match[0].length)];
  }

  // Returns the text after "> " or ">" on a quote line, or false.
  function blockquoteText(line) {
    if (line.startsWith("> ")) return line.slice(2);
    if (line.startsWith(">")) return line.slice(1);
    return false;
  }

  // Takes the texts of the quote lines from idx onto acc, as [acc, index after
  // them].
  function takeBlockquote(lines, idx, acc) {
    for (; idx < lines.length; idx += 1) {
      const text = blockquoteText(trim(lines[idx]));
      if (text === false) break;
      acc.push(text);
    }
    return [acc, idx];
  }

  // Renders quote texts as Markdown in a blockquote.
  function renderBlockquote(lines, opts) {
    return `<blockquote>${renderMarkdown(lines.join("\n"), opts)}</blockquote>`;
  }

  // Returns a trimmed line of raw HTML when it is on the allow-list of guide
  // markup, or false.
  function rawHtmlLine(line) {
    if (rawHtmlLines.has(line)) return line;
    for (const [prefix, allowed] of rawHtmlPrefixes) {
      if (line.startsWith(prefix)) {
        return allowed(line.slice(prefix.length)) ? line : false;
      }
    }
    return false;
  }

  // Tells whether the rest of a core-concepts img tag shows one of the three
  // concept figures.
  function allowedCoreConceptsImg(rest) {
    return (
      rest.startsWith("messages\" src=\"/docs/assets/images/aosvg1.svg\"") ||
      rest.startsWith("devices\" src=\"/docs/assets/images/aosvg2.svg\"") ||
      rest.startsWith("paths\" src=\"/docs/assets/images/aosvg3.svg\"")
    ) && rest.endsWith(" alt=\"\" loading=\"lazy\">");
  }

  // Tells whether the rest of a core-concept header starts with one of the
  // three concept headers.
  function allowedCoreConceptHeader(rest) {
    return (
      rest.startsWith("messages\"><b>Messages</b></p>") ||
      rest.startsWith("devices\"><b>Devices</b></p>") ||
      rest.startsWith("paths\"><b>Paths</b></p>")
    );
  }

  // Returns the trimmed text after "- " or "* " on a list line, or false.
  function bulletText(line) {
    if (!line.startsWith("- ") && !line.startsWith("* ")) return false;
    return trim(line.slice(2));
  }

  // Returns the text after "N. " on a numbered list line, or false.
  function numberedText(line) {
    const pos = line.indexOf(". ");
    if (pos <= 0 || !isDigits(line.slice(0, pos))) return false;
    return line.slice(pos + 2);
  }

  // Takes the items of a list from idx, as [items, index after the list]: a
  // marker line starts an item, and an indented line that starts no block
  // continues it.
  function takeListBlock(lines, idx, kind, text) {
    const items = [];
    let current = [text];
    for (; idx < lines.length; idx += 1) {
      const trimmed = trim(lines[idx]);
      if (trimmed === "") {
        items.push(finishListItem(current));
        return [items, idx + 1];
      }
      const marker = listMarker(kind, trimmed);
      if (marker !== false) {
        items.push(finishListItem(current));
        current = [marker];
      } else if (isListContinuation(lines[idx], trimmed)) {
        current.push(trimmed);
      } else {
        break;
      }
    }
    items.push(finishListItem(current));
    return [items, idx];
  }

  // Returns the item text of a trimmed line for a list of the given kind, or
  // false.
  function listMarker(kind, line) {
    return kind === "unordered" ? bulletText(line) : numberedText(line);
  }

  // Tells whether a line continues a list item: indented, and not the start of
  // a heading, fence or list item.
  function isListContinuation(line, trimmed) {
    return isIndented(line) && !isBlockStart(trimmed);
  }

  // Tells whether a line starts with a space or a tab.
  function isIndented(line) {
    return line.startsWith(" ") || line.startsWith("\t");
  }

  // Tells whether a trimmed line is a heading, a fence, or a list item.
  function isBlockStart(trimmed) {
    return heading(trimmed) !== false ||
      fenceLanguage(trimmed) !== false ||
      bulletText(trimmed) !== false ||
      numberedText(trimmed) !== false;
  }

  // Joins the lines of a list item with spaces.
  function finishListItem(lines) {
    return lines.join(" ");
  }

  // Renders list items in a tag element.
  function renderList(tag, items, opts) {
    const html = items.map((item) => `<li>${renderInline(item, opts)}</li>`);
    return `<${tag}>${html.join("")}</${tag}>`;
  }

  // Tells whether text is one or more digits 0-9.
  function isDigits(text) {
    return /^[0-9]+$/.test(text);
  }

  // Returns the normalized language of a "```" fence line, "text" when the line
  // has no language, or false for any other line.
  function fenceLanguage(line) {
    const trimmed = trim(line);
    if (!trimmed.startsWith("```")) return false;
    const lang = trimmed.length === 3 ? "text" : trim(trimmed.slice(3));
    return normalizeLang(lang);
  }

  // Tells whether a line is a fence.
  function isFence(line) {
    return fenceLanguage(line) !== false;
  }

  // Normalizes a code language: "" to "text", "sh" and "shell" to "bash", any
  // other lowercased.
  function normalizeLang(lang) {
    if (lang === "") return "text";
    if (lang === "sh" || lang === "shell") return "bash";
    return toLower(lang);
  }

  // Removes whitespace from both ends of text as Erlang's string:trim/1 does:
  // whole graphemes at the start, code points at the end.
  function trim(text) {
    let start = 0;
    for (const { segment } of graphemes.segment(text)) {
      if (!whitespace.has(segment)) break;
      start += segment.length;
    }
    let end = text.length;
    while (end > start && whitespace.has(text[end - 1])) end -= 1;
    return text.slice(start, end);
  }

  // Lowercases each code point of text on its own, as Erlang's
  // string:lowercase/1 does.
  function toLower(text) {
    return Array.from(text, (char) => char.toLowerCase()).join("");
  }

  // Escapes &, <, >, " and ' in a value for HTML.
  function esc(value) {
    return String(value)
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("\"", "&quot;")
      .replaceAll("'", "&#39;");
  }

  const HBMarkdown = {
    render: renderMarkdown,
    renderWithHeadingIds: renderMarkdownWithHeadingIds,
    dropFirstH1,
    title: markdownTitle,
    summary: markdownSummary,
    cardSummary,
    recipeCardSummary,
    codeBlocks,
    specSections,
    specSectionLookup,
    specSectionMarkdown,
    specSectionNavLabel,
    linkCodeLabel,
    escape: esc
  };
  globalThis.HBMarkdown = HBMarkdown;
  if (typeof module !== "undefined") module.exports = HBMarkdown;
})();
