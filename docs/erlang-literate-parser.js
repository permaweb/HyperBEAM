#!/usr/bin/env node

/**
 * Comprehensive Erlang Literate Documentation Generator
 *
 * Handles all comment types:
 * - %%% Module documentation
 * - %% Function/section documentation
 * - % Inline comments (converted to prose)
 * - @doc, @param, @returns annotations
 */

import fs from 'fs';
import path from 'path';

// Character codes for faster comparisons
const CHAR_CODES = {
    PERCENT: 37,        // '%'
    DASH: 45,          // '-'
    OPEN_PAREN: 40,    // '('
    CLOSE_PAREN: 41,   // ')'
    OPEN_BRACE: 123,   // '{'
    CLOSE_BRACE: 125,  // '}'
    OPEN_BRACKET: 91,  // '['
    CLOSE_BRACKET: 93, // ']'
    DOT: 46            // '.'
};

// String constants to avoid repeated allocations
const STRINGS = {
    EMPTY: '',
    SPACE: ' ',
    NEWLINE: '\n',
    TRIPLE_PERCENT: '%%%',
    DOUBLE_PERCENT: '%%',
    SINGLE_PERCENT: '%',
    SPEC_PREFIX: '-spec ',
    ERLANG: 'erlang',
    BACKTICK: '`',
    PARAM_TAG: '@param',
    RETURNS_TAG: '@returns',
    PARAMETERS_HEADER: '### Parameters',
    RETURNS_HEADER: '### Returns',
    EXPORTED_FUNCTIONS: '## Exported Functions',
    SEPARATOR: '---'
};

// Precompiled regex patterns for performance
const REGEX = {
    MODULE: /^-module\(([^)]+)\)/,
    EXPORT: /^-export\(\[([^\]]+)\]\)/,
    EXPORT_PREFIX: /^-export\(\[/,
    SPEC: /^-spec\s+([a-z][a-z0-9_]*)\s*\(/,
    FUNCTION: /^([a-z][a-z0-9_]*)\s*\(/,
    DOC_BLOCK_START: /^%% @doc/,
    COMMENT_SINGLE: /^\s*%[^%]/,
    COMMENT_DOUBLE: /^\s*%%[^%]/,
    BACKTICK_QUOTE: /`([^']*?)'/g,
    HTML_ENTITIES_LT: /&lt;&lt;/g,
    HTML_ENTITIES_GT: /&gt;&gt;/g,
    PRE_TAG: /<pre>([\s\S]*?)<\/pre>/g,
    // Match numbered list items that start with either "1." or "1:" (allow optional space before the punctuation)
    NUMBERED_LIST: /^\d+\s*[.:]\s/,
    BULLET_LIST: /^[-*]\s/,
    HEADING: /^#{1,6}\s/,
    CODE_FENCE: /^```/,
    RETURNS_TOKENS: /(\{[^}]+\}|\bok\b|\berror\b|\bnot_found\b|\btrue\b|\bfalse\b)/gi,
    LEADING_RETURN_TOKEN: /^(\s*)(\{[^}]+\}|\[[^\]]+\]|ok|error|not_found|true|false)(\b|\s|$)/i,
    STANDALONE_TUPLE: /(^|[^`])(\{[^}]+\})([^`]|$)/g,
    PARAM: /^@param\s+(\S+)\s*(.*)/,
    RETURNS: /^@returns?\s*/,
    OPTION_DEF: /^(`[^`]+`)\s*:\s*(.*)$/,
    DEFINITION: /^(\S+(?:\s*:\s*\S+)?)\s*:\s*(.*)$/,
    MULTIPLE_NEWLINES: /\n\s*\n\s*\n/g,
    TRAILING_SPACES: /[ \t]+$/gm,
    TRIM: /^\s+|\s+$/g,
    RETURNS_LIKE_TUPLE: /^`?\{[^}]+\}`?/,
    RETURNS_LIKE_ATOM: /^`?(ok|error|not_found|true|false)\b/i,
    REMOVE_DOC: /@doc\s*/g,
    WHITESPACE_NORMALIZE: /\s+/g,
    COMMA_START: /^,\s*/,
    COMMA_END: /,\s*$/,
    EMPTY_LINE: /^\s*$/,
    COLON_END: /:\s*$/,
    COMMENT_DOUBLE_PREFIX: /^\s*%%\s?/,
    COMMENT_SINGLE_PREFIX: /^\s*%\s?/
};

class ErlangLiterateParser {
    constructor(options = {}) {
        this.options = {
            githubBase: 'https://github.com/permaweb/HyperBEAM/blob/edge/src',
            verbose: false,
            ...options
        };
        this.reset();
    }

    reset() {
        this.lines = [];
        this.moduleInfo = { name: null, doc: null, exports: null };
        this.functions = [];
        this.currentState = {
            inFunction: false,
            functionName: STRINGS.EMPTY,
            functionSpec: STRINGS.EMPTY,
            functionDoc: STRINGS.EMPTY,
            functionLines: [],
            pendingDoc: STRINGS.EMPTY,
            specFunctionName: STRINGS.EMPTY,
            braceDepth: 0,
            parenDepth: 0,
            inlineDocTags: []
        };
    }

    parseFile(filePath) {
        const content = fs.readFileSync(filePath, 'utf8');
        this.reset();
        this.lines = content.split(STRINGS.NEWLINE);

        this.extractModuleInfo();
        this.processFunctions();

        return this.generateMarkdown(path.basename(filePath));
    }

    extractModuleInfo() {
        const moduleDoc = [];
        let inModuleDoc = false;
        const linesLength = this.lines.length;

        for (let i = 0; i < linesLength; i++) {
            const line = this.lines[i];
            const trimmed = line.trim();

            if (!trimmed) {
                if (inModuleDoc) moduleDoc.push(STRINGS.EMPTY);
                continue;
            }

            // Fast character check before regex
            const firstChar = trimmed.charCodeAt(0);

            // Module name - check for dash first
            if (firstChar === CHAR_CODES.DASH && trimmed.startsWith('-module(')) {
                const moduleMatch = trimmed.match(REGEX.MODULE);
                if (moduleMatch) {
                    this.moduleInfo.name = moduleMatch[1];
                }
                continue;
            }

            // Exports - check for dash first
            if (firstChar === CHAR_CODES.DASH && REGEX.EXPORT_PREFIX.test(trimmed)) {
                const exportMatch = trimmed.match(REGEX.EXPORT);
                if (exportMatch) {
                    this.moduleInfo.exports = exportMatch[1]
                        .split(',')
                        .map(e => e.trim())
                        .filter(Boolean);
                }
                continue;
            }

            // Module documentation - check for percent first
            if (firstChar === CHAR_CODES.PERCENT && trimmed.startsWith(STRINGS.TRIPLE_PERCENT)) {
                inModuleDoc = true;
                let docLine = trimmed.substring(3).trim();
                docLine = docLine.replace(REGEX.REMOVE_DOC, STRINGS.EMPTY);
                moduleDoc.push(docLine);
            } else if (inModuleDoc && (firstChar === CHAR_CODES.PERCENT || firstChar === CHAR_CODES.DASH)) {
                break;
            }
        }

        this.moduleInfo.doc = this.cleanDocumentation(moduleDoc.join(STRINGS.NEWLINE));
    }

    processFunctions() {
        const linesLength = this.lines.length;

        for (let i = 0; i < linesLength; i++) {
            const line = this.lines[i];
            const trimmed = line.trim();

            if (!trimmed) continue;

            // Check for start of function documentation block
            if (REGEX.DOC_BLOCK_START.test(trimmed)) {
                this.collectFunctionDoc(i);
                continue;
            }

            // Check for -spec
            if (trimmed.startsWith(STRINGS.SPEC_PREFIX)) {
                this.collectParamTagsBeforeSpec(i);
                this.collectSpec(i);
                const specMatch = trimmed.match(REGEX.SPEC);
                if (specMatch) {
                    this.currentState.specFunctionName = specMatch[1];
                }
                continue;
            }

            // Check for function start
            const funcMatch = trimmed.match(REGEX.FUNCTION);
            if (funcMatch && !this.currentState.inFunction) {
                if (this.currentState.pendingDoc) {
                    this.currentState.functionDoc = this.currentState.pendingDoc;
                    this.currentState.pendingDoc = STRINGS.EMPTY;
                }

                const functionName = this.currentState.specFunctionName || funcMatch[1];
                this.startFunction(functionName);
                this.currentState.specFunctionName = STRINGS.EMPTY;
            }

            // If in function, collect lines
            if (this.currentState.inFunction) {
                this.collectFunctionLine(line);
                if (this.isFunctionEnd(line)) {
                    this.endFunction();
                }
            }
        }

        if (this.currentState.inFunction) {
            this.endFunction();
        }
    }

    collectFunctionDoc(startIdx) {
        const docLines = [];
        const linesLength = this.lines.length;

        for (let i = startIdx; i < linesLength; i++) {
            const line = this.lines[i];
            const trimmed = line.trim();

            if (trimmed.startsWith(STRINGS.DOUBLE_PERCENT)) {
                let docLine = trimmed.substring(2).trim();
                if (i === startIdx) {
                    docLine = docLine.replace(REGEX.REMOVE_DOC, STRINGS.EMPTY);
                }
                docLines.push(docLine);
            } else if (!trimmed) {
                // Look ahead for more %% comments
                let j = i + 1;
                while (j < linesLength && !this.lines[j].trim()) j++;

                if (j < linesLength && this.lines[j].trim().startsWith(STRINGS.DOUBLE_PERCENT)) {
                    docLines.push(STRINGS.EMPTY);
                } else {
                    break;
                }
            } else if (!trimmed.startsWith(STRINGS.SINGLE_PERCENT)) {
                break;
            }
        }

        this.currentState.pendingDoc = docLines.join(STRINGS.NEWLINE);
    }

    collectParamTagsBeforeSpec(specIdx) {
        const paramLines = [];
        let hitDocBlock = false;

        for (let i = specIdx - 1; i >= 0; i--) {
            const trimmed = this.lines[i].trim();

            if (REGEX.DOC_BLOCK_START.test(trimmed)) {
                hitDocBlock = true;
                break;
            }

            if (!trimmed || (!trimmed.startsWith(STRINGS.DOUBLE_PERCENT) && !trimmed.startsWith(STRINGS.SINGLE_PERCENT))) {
                break;
            }

            if (trimmed.startsWith(STRINGS.DOUBLE_PERCENT) &&
                (trimmed.includes(STRINGS.PARAM_TAG) || trimmed.includes(STRINGS.RETURNS_TAG))) {
                paramLines.unshift(trimmed.substring(2).trim());
            }
        }

        if (paramLines.length > 0 && !hitDocBlock) {
            const existingDoc = this.currentState.pendingDoc;
            const newDoc = paramLines.join(STRINGS.NEWLINE);
            this.currentState.pendingDoc = existingDoc ?
                existingDoc + STRINGS.NEWLINE + newDoc : newDoc;
        }
    }

    collectSpec(startIdx) {
        const specLines = [];
        let depth = 0;
        const linesLength = this.lines.length;

        for (let i = startIdx; i < linesLength; i++) {
            const line = this.lines[i];
            specLines.push(line);

            // Fast character-based depth tracking
            for (let j = 0, len = line.length; j < len; j++) {
                const charCode = line.charCodeAt(j);
                if (charCode === CHAR_CODES.OPEN_PAREN) depth++;
                else if (charCode === CHAR_CODES.CLOSE_PAREN) depth--;
            }

            if (line.trim().endsWith('.') && depth === 0) {
                break;
            }
        }

        this.currentState.functionSpec = specLines.join(STRINGS.NEWLINE);
    }

    startFunction(name) {
        this.currentState.inFunction = true;
        this.currentState.functionName = name;
        this.currentState.functionLines.length = 0;
        this.currentState.braceDepth = 0;
        this.currentState.parenDepth = 0;
        this.currentState.inlineDocTags.length = 0;
    }

    collectFunctionLine(line) {
        this.currentState.functionLines.push(line);

        // Fast character-based depth tracking
        for (let i = 0, len = line.length; i < len; i++) {
            const charCode = line.charCodeAt(i);
            switch (charCode) {
                case CHAR_CODES.OPEN_BRACE:
                case CHAR_CODES.OPEN_BRACKET:
                    this.currentState.braceDepth++;
                    break;
                case CHAR_CODES.CLOSE_BRACE:
                case CHAR_CODES.CLOSE_BRACKET:
                    this.currentState.braceDepth--;
                    break;
                case CHAR_CODES.OPEN_PAREN:
                    this.currentState.parenDepth++;
                    break;
                case CHAR_CODES.CLOSE_PAREN:
                    this.currentState.parenDepth--;
                    break;
            }
        }
    }

    isFunctionEnd(line) {
        const trimmed = line.trim();
        return this.currentState.braceDepth === 0 &&
               this.currentState.parenDepth === 0 &&
               trimmed.charCodeAt(trimmed.length - 1) === CHAR_CODES.DOT &&
               trimmed.charCodeAt(0) !== CHAR_CODES.PERCENT;
    }

    endFunction() {
        const processedBody = this.processFunctionBody(this.currentState.functionLines);

        this.functions.push({
            name: this.currentState.functionName,
            spec: this.currentState.functionSpec,
            doc: this.currentState.functionDoc,
            body: processedBody
        });

        // Reset state efficiently
        this.currentState.inFunction = false;
        this.currentState.functionName = STRINGS.EMPTY;
        this.currentState.functionSpec = STRINGS.EMPTY;
        this.currentState.functionDoc = STRINGS.EMPTY;
        this.currentState.functionLines.length = 0;
        this.currentState.specFunctionName = STRINGS.EMPTY;
        this.currentState.inlineDocTags.length = 0;
    }

    processFunctionBody(lines) {
        const segments = [];
        const currentCode = [];
        const pendingTagLines = [];

        const flushCode = () => {
            if (currentCode.length > 0) {
                segments.push({ type: 'code', content: currentCode.join(STRINGS.NEWLINE) });
                currentCode.length = 0;
            }
        };

        const flushTags = () => {
            if (pendingTagLines.length > 0) {
                const tagText = pendingTagLines.join(STRINGS.NEWLINE);
                const parsed = this.parseDocumentation(tagText);
                const docParts = [];

                if (parsed.params.length > 0) {
                    docParts.push(STRINGS.PARAMETERS_HEADER, STRINGS.EMPTY);
                    parsed.params.forEach(p => {
                        const desc = this.cleanDocumentation(p.description || STRINGS.EMPTY);
                        docParts.push(`- ${STRINGS.BACKTICK}${p.name}${STRINGS.BACKTICK} - ${desc}`);
                    });
                    docParts.push(STRINGS.EMPTY);
                }

                if (parsed.returns.length > 0) {
                    docParts.push(STRINGS.RETURNS_HEADER, STRINGS.EMPTY);
                    const expanded = parsed.returns.flatMap(r => this.splitReturnsIntoOutcomes(r));
                    expanded.forEach(r => docParts.push(`- ${this.formatReturnsText(r)}`));
                    docParts.push(STRINGS.EMPTY);
                }

                if (docParts.length > 0) {
                    segments.push({ type: 'doc', content: docParts.join(STRINGS.NEWLINE) });
                }
                pendingTagLines.length = 0;
            }
        };

        for (const line of lines) {
            const trimmed = line.trim();

            if (REGEX.COMMENT_SINGLE.test(trimmed) || REGEX.COMMENT_DOUBLE.test(trimmed)) {
                flushCode();

                const commentText = REGEX.COMMENT_DOUBLE.test(trimmed)
                    ? line.replace(REGEX.COMMENT_DOUBLE_PREFIX, STRINGS.EMPTY)
                    : line.replace(REGEX.COMMENT_SINGLE_PREFIX, STRINGS.EMPTY);
                const cleaned = this.cleanInlineComment(commentText);

                const returnsLikeTuple = REGEX.RETURNS_LIKE_TUPLE.test(cleaned);
                const returnsLikeAtom = REGEX.RETURNS_LIKE_ATOM.test(cleaned);
                const isTagParam = cleaned.startsWith(STRINGS.PARAM_TAG);
                const isTagReturns = cleaned.startsWith(STRINGS.RETURNS_TAG);

                if (isTagParam || isTagReturns || returnsLikeTuple || returnsLikeAtom) {
                    const lineAsTag = (isTagParam || isTagReturns)
                        ? cleaned.trim()
                        : `${STRINGS.RETURNS_TAG} ${cleaned.trim()}`;
                    pendingTagLines.push(lineAsTag);
                } else if (pendingTagLines.length > 0) {
                    const lastTag = pendingTagLines[pendingTagLines.length - 1];
                    if (lastTag.startsWith(STRINGS.RETURNS_TAG) || lastTag.startsWith(STRINGS.PARAM_TAG)) {
                        pendingTagLines.push(cleaned.trim());
                    } else {
                        flushTags();
                        segments.push({ type: 'comment', content: cleaned });
                    }
                } else {
                    flushTags();
                    segments.push({ type: 'comment', content: cleaned });
                }
            } else {
                flushTags();
                currentCode.push(line);
            }
        }

        flushTags();
        flushCode();
        return segments;
    }

    cleanInlineComment(text) {
        return text.replace(REGEX.BACKTICK_QUOTE, `${STRINGS.BACKTICK}$1${STRINGS.BACKTICK}`).trim();
    }

    cleanDocumentation(text) {
        if (!text) return STRINGS.EMPTY;

        text = text.replace(REGEX.PRE_TAG, (match, content) => this.formatPreContent(content));

        const cleaned = text
            .replace(REGEX.BACKTICK_QUOTE, `${STRINGS.BACKTICK}$1${STRINGS.BACKTICK}`)
            .replace(REGEX.HTML_ENTITIES_LT, '<<')
            .replace(REGEX.HTML_ENTITIES_GT, '>>')
            .replace(REGEX.REMOVE_DOC, STRINGS.EMPTY)
            .replace(REGEX.MULTIPLE_NEWLINES, '\n\n')
            .replace(REGEX.TRAILING_SPACES, STRINGS.EMPTY)
            .replace(REGEX.TRIM, STRINGS.EMPTY);

        return this.reflowNumberedLists(cleaned);
    }

    formatReturnsText(text) {
        if (!text) return STRINGS.EMPTY;
        let result = this.cleanDocumentation(text);

        const leadingMatch = result.match(REGEX.LEADING_RETURN_TOKEN);
        if (leadingMatch) {
            const [, leadSpace, token] = leadingMatch;
            result = leadSpace + STRINGS.BACKTICK + token + STRINGS.BACKTICK +
                    result.slice(leadSpace.length + token.length);
        }

        return result.replace(REGEX.STANDALONE_TUPLE,
            (m, pre, tuple, post) => `${pre}${STRINGS.BACKTICK}${tuple}${STRINGS.BACKTICK}${post}`);
    }

    splitReturnsIntoOutcomes(text) {
        if (!text) return [];
        const s = this.cleanDocumentation(text);
        const matches = [];
        let match;

        // Reset regex lastIndex to avoid issues with global regex
        REGEX.RETURNS_TOKENS.lastIndex = 0;
        while ((match = REGEX.RETURNS_TOKENS.exec(s)) !== null) {
            matches.push({ index: match.index, token: match[0] });
        }

        if (matches.length === 0 || matches[0].index > 0) {
            return [s.trim()];
        }

        const parts = [];
        const matchesLength = matches.length;
        for (let i = 0; i < matchesLength; i++) {
            const start = matches[i].index;
            const nextStart = (i + 1 < matchesLength) ? matches[i + 1].index : s.length;
            let segment = s.slice(start, nextStart).trim()
                .replace(REGEX.COMMA_START, STRINGS.EMPTY)
                .replace(REGEX.COMMA_END, STRINGS.EMPTY);
            if (segment) parts.push(segment.trim());
        }

        return parts.filter(Boolean);
    }

    reflowNumberedLists(text) {
        if (!text) return STRINGS.EMPTY;
        const lines = text.split(STRINGS.NEWLINE);
        const out = [];
        let inNumbered = false;
        let lastNumIndex = -1;

        const linesLength = lines.length;
        for (let i = 0; i < linesLength; i++) {
            const raw = lines[i];
            const trimmed = raw.trim();

            const isNumbered = REGEX.NUMBERED_LIST.test(trimmed);
            const isBullet = REGEX.BULLET_LIST.test(trimmed);
            const isHeading = REGEX.HEADING.test(trimmed);
            const isCodeFence = REGEX.CODE_FENCE.test(trimmed);

            // Ensure a blank line BEFORE any list (numbered or bullet) begins
            if ((isNumbered || isBullet) && out.length > 0) {
                const prev = out[out.length - 1];
                if (prev.trim() !== STRINGS.EMPTY) {
                    out.push(STRINGS.EMPTY);
                }
            }

            if (isNumbered) {
                out.push(trimmed);
                inNumbered = true;
                lastNumIndex = out.length - 1;
                continue;
            }

            // Pass through bullet list lines unchanged (no reflow of bullets for now)
            if (isBullet) {
                out.push(raw);
                continue;
            }

            if (inNumbered) {
                if (!trimmed) {
                    out.push(STRINGS.EMPTY);
                    inNumbered = false;
                    lastNumIndex = -1;
                    continue;
                }
                if (!isNumbered && !isBullet && !isHeading && !isCodeFence) {
                    out[lastNumIndex] = out[lastNumIndex] + STRINGS.SPACE + trimmed;
                    continue;
                }
                inNumbered = false;
                lastNumIndex = -1;
            }

            out.push(raw);
        }

        // Ensure blank line separation
        const separated = [];
        const outLength = out.length;
        for (let i = 0; i < outLength; i++) {
            const cur = out[i];
            const next = i + 1 < outLength ? out[i + 1] : STRINGS.EMPTY;
            separated.push(cur);
            // If a paragraph ends with ':' and is immediately followed by a numbered list,
            // insert a blank line between them to satisfy Markdown list rendering rules.
            if (REGEX.COLON_END.test(cur.trim()) && next && REGEX.NUMBERED_LIST.test(next.trim())) {
                if (separated[separated.length - 1] !== STRINGS.EMPTY) {
                    separated.push(STRINGS.EMPTY);
                }
            }
        }

        return separated.join(STRINGS.NEWLINE);
    }

    formatPreContent(content) {
        const lines = content.trim().split(STRINGS.NEWLINE);
        const formatted = [];

        let i = 0;
        const linesLength = lines.length;
        while (i < linesLength) {
            const line = lines[i].trim();

            if (!line) {
                i++;
                continue;
            }

            const defMatch = line.match(REGEX.DEFINITION);

            if (defMatch) {
                const [, term, initialDesc] = defMatch;
                let fullDescription = initialDesc.trim();

                let j = i + 1;
                while (j < linesLength) {
                    const nextLine = lines[j];

                    if (!nextLine.trim()) {
                        j++;
                        continue;
                    }

                    if (nextLine.trim().match(REGEX.DEFINITION)) {
                        break;
                    }

                    if (nextLine.trim()) {
                        fullDescription += STRINGS.SPACE + nextLine.trim();
                    }
                    j++;
                }

                formatted.push(STRINGS.EMPTY, `**${term.trim()}**`, STRINGS.EMPTY, fullDescription);
                i = j;
            } else {
                if (line.toLowerCase().includes('hyperbeam') && line.includes('options')) {
                    formatted.push(STRINGS.EMPTY, `### ${line}`, STRINGS.EMPTY);
                } else {
                    const optMatch = line.match(REGEX.OPTION_DEF);
                    if (optMatch) {
                        const [, optName, optDesc] = optMatch;
                        formatted.push(STRINGS.EMPTY, `**${optName}**`, STRINGS.EMPTY, optDesc);
                    } else {
                        formatted.push(line);
                    }
                }
                i++;
            }
        }

        return formatted.join(STRINGS.NEWLINE);
    }

    parseDocumentation(docText) {
        const lines = docText.split(STRINGS.NEWLINE);
        const result = {
            description: [],
            params: [],
            returns: []
        };

        let currentSection = 'description';
        let currentParam = null;
        let lastReturnIndex = -1;

        for (const line of lines) {
            const trimmed = line.trim();

            const paramMatch = trimmed.match(REGEX.PARAM);
            if (paramMatch) {
                if (currentParam) {
                    result.params.push(currentParam);
                }
                currentParam = {
                    name: paramMatch[1],
                    description: paramMatch[2] || STRINGS.EMPTY
                };
                currentSection = 'param';
                continue;
            }

            if (REGEX.RETURNS.test(trimmed)) {
                if (currentParam) {
                    result.params.push(currentParam);
                    currentParam = null;
                }
                const returnsText = trimmed.replace(REGEX.RETURNS, STRINGS.EMPTY);
                result.returns.push(returnsText);
                lastReturnIndex = result.returns.length - 1;
                currentSection = 'returns';
                continue;
            }

            if (currentSection === 'description') {
                if (trimmed) {
                    result.description.push(trimmed);
                } else {
                    const last = result.description[result.description.length - 1];
                    if (last !== STRINGS.EMPTY) {
                        result.description.push(STRINGS.EMPTY);
                    }
                }
            } else if (currentSection === 'param' && currentParam && trimmed) {
                currentParam.description += STRINGS.SPACE + trimmed;
            } else if (currentSection === 'returns' && trimmed) {
                if (lastReturnIndex >= 0) {
                    result.returns[lastReturnIndex] =
                        (result.returns[lastReturnIndex] + STRINGS.SPACE + trimmed)
                        .replace(REGEX.WHITESPACE_NORMALIZE, STRINGS.SPACE).trim();
                } else {
                    result.returns.push(trimmed);
                    lastReturnIndex = result.returns.length - 1;
                }
            }
        }

        if (currentParam) {
            result.params.push(currentParam);
        }

        return result;
    }

    generateMarkdown(fileName) {
        const githubUrl = `${this.options.githubBase}/${fileName}`;
        const md = [];

        // Header
        md.push(`# ${this.moduleInfo.name || fileName.replace('.erl', STRINGS.EMPTY)}`);
        md.push(STRINGS.EMPTY);
        md.push(`[View source on GitHub](${githubUrl})`);
        md.push(STRINGS.EMPTY);

        // Module documentation
        if (this.moduleInfo.doc) {
            md.push(this.moduleInfo.doc);
            md.push(STRINGS.EMPTY);
            md.push(STRINGS.SEPARATOR);
            md.push(STRINGS.EMPTY);
        }

        // Exports
        if (this.moduleInfo.exports?.length > 0) {
            md.push(STRINGS.EXPORTED_FUNCTIONS);
            md.push(STRINGS.EMPTY);
            this.moduleInfo.exports.forEach(exp =>
                md.push(`- ${STRINGS.BACKTICK}${exp}${STRINGS.BACKTICK}`));
            md.push(STRINGS.EMPTY);
            md.push(STRINGS.SEPARATOR);
            md.push(STRINGS.EMPTY);
        }

        const groupedFunctions = this.groupFunctionsByName(this.functions);

        for (const group of groupedFunctions) {
            md.push(`## ${group.name}`);
            md.push(STRINGS.EMPTY);

            const combinedDoc = this.combineFunctionDocs(group.functions);
            if (combinedDoc) {
                md.push(combinedDoc);
                md.push(STRINGS.EMPTY);
            }

            for (const func of group.functions) {
                if (func.spec) {
                    md.push(`\`\`\`${STRINGS.ERLANG}`);
                    md.push(func.spec.trim());
                    md.push('```');
                    md.push(STRINGS.EMPTY);
                }

                if (func.body?.length > 0) {
                    md.push(STRINGS.EMPTY);
                    for (const segment of func.body) {
                        if (segment.type === 'comment') {
                            md.push(segment.content);
                            md.push(STRINGS.EMPTY);
                        } else if (segment.type === 'doc') {
                            md.push(segment.content);
                            md.push(STRINGS.EMPTY);
                        } else if (segment.type === 'code') {
                            md.push(`\`\`\`${STRINGS.ERLANG}`);
                            md.push(segment.content.trim());
                            md.push('```');
                            md.push(STRINGS.EMPTY);
                        }
                    }
                }
            }

            md.push(STRINGS.EMPTY);
        }

        md.push(STRINGS.SEPARATOR);
        md.push(STRINGS.EMPTY);
        md.push(`*Generated from [${fileName}](${githubUrl})*`);

        return md.join(STRINGS.NEWLINE);
    }

    groupFunctionsByName(functions) {
        const groups = [];
        let currentGroup = null;

        for (const func of functions) {
            if (!currentGroup || currentGroup.name !== func.name) {
                currentGroup = { name: func.name, functions: [func] };
                groups.push(currentGroup);
            } else {
                currentGroup.functions.push(func);
            }
        }

        return groups;
    }

    combineFunctionDocs(functions) {
        for (const func of functions) {
            if (func.doc) {
                const parsed = this.parseDocumentation(func.doc);
                const combinedDoc = [];

                if (parsed.description.length > 0) {
                    combinedDoc.push(this.cleanDocumentation(parsed.description.join(STRINGS.NEWLINE)));
                    combinedDoc.push(STRINGS.EMPTY);
                }

                if (parsed.params.length > 0) {
                    combinedDoc.push(STRINGS.PARAMETERS_HEADER);
                    combinedDoc.push(STRINGS.EMPTY);
                    parsed.params.forEach(param => {
                        const desc = this.cleanDocumentation(param.description);
                        combinedDoc.push(`- ${STRINGS.BACKTICK}${param.name}${STRINGS.BACKTICK} - ${desc}`);
                    });
                    combinedDoc.push(STRINGS.EMPTY);
                }

                if (parsed.returns.length > 0) {
                    combinedDoc.push(STRINGS.RETURNS_HEADER);
                    combinedDoc.push(STRINGS.EMPTY);
                    const expanded = parsed.returns.flatMap(r => this.splitReturnsIntoOutcomes(r));
                    expanded.forEach(ret => combinedDoc.push(`- ${this.formatReturnsText(ret)}`));
                    combinedDoc.push(STRINGS.EMPTY);
                }

                return combinedDoc.join(STRINGS.NEWLINE);
            }
        }
        return null;
    }
}

// CLI Interface
function main() {
    const args = process.argv.slice(2);
    const verbose = args.includes('-v') || args.includes('--verbose');

    const srcDir = process.env.SRC_DIR || path.join(process.cwd(), 'src');
    const outputDir = process.env.OUTPUT_DIR || path.join(process.cwd(), 'docs/literate-erlang');

    if (!fs.existsSync(outputDir)) {
        fs.mkdirSync(outputDir, { recursive: true });
    }

    const files = fs.readdirSync(srcDir).filter(f => f.endsWith('.erl'));
    const parser = new ErlangLiterateParser({ verbose });

    console.log(`Processing ${files.length} Erlang files...`);

    for (const file of files) {
        if (verbose) console.log(`  Processing ${file}...`);

        try {
            const inputPath = path.join(srcDir, file);
            const outputPath = path.join(outputDir, `${file}.md`);
            const markdown = parser.parseFile(inputPath);
            fs.writeFileSync(outputPath, markdown);
        } catch (error) {
            console.error(`Error processing ${file}:`, error.message);
        }
    }

    console.log(`✓ Generated documentation in ${outputDir}`);
}

if (import.meta.url === `file://${process.argv[1]}`) {
    main();
}

export default ErlangLiterateParser;