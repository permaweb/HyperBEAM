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
    INCLUDE: /^-include(?:_lib)?\(["']([^"']+)["']\)/,
    DEFINE: /^-define\(([^,)]+)(?:,\s*(.*))?\)/,
    BEHAVIOUR: /^-behaviour\(([^)]+)\)/,
    RECORD: /^-record\(([^,)]+),\s*\{/,
    TYPE: /^-type\s+([a-z][a-z0-9_]*)\(/,
    SPEC: /^-spec\s+([a-z][a-z0-9_]*)\s*\(/,
    FUNCTION: /^([a-z][a-z0-9_]*)\s*\(/,
    ATTRIBUTE: /^-([a-z][a-z0-9_]*)\(/,
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
        this.moduleInfo = {
            name: null,
            doc: null,
            exports: [],
            includes: [],
            defines: [],
            behaviours: [],
            records: [],
            types: [],
            specs: [],
            attributes: []
        };
        this.functions = [];
        this.undocumentedFunctions = [];
        this.commentedCodeBlocks = [];
        this.conditionalDirectives = [];
        this.sections = [];
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
        let moduleDocCollected = false;
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

            // Handle all module-level declarations
            if (firstChar === CHAR_CODES.DASH) {
                // Module name
                if (trimmed.startsWith('-module(')) {
                    const moduleMatch = trimmed.match(REGEX.MODULE);
                    if (moduleMatch) {
                        this.moduleInfo.name = moduleMatch[1];
                    }
                    continue;
                }

                // Exports - handle multi-line exports
                if (REGEX.EXPORT_PREFIX.test(trimmed)) {
                    const exportLines = this.collectMultiLineConstruct(i, '[', ']');
                    const fullExport = exportLines.join(' ');
                    const exportMatch = fullExport.match(REGEX.EXPORT);
                    if (exportMatch) {
                        const exports = exportMatch[1]
                            .split(',')
                            .map(e => e.trim())
                            .filter(Boolean);
                        this.moduleInfo.exports.push(...exports);
                    }
                    i += exportLines.length - 1;
                    continue;
                }

                // Includes
                const includeMatch = trimmed.match(REGEX.INCLUDE);
                if (includeMatch) {
                    this.moduleInfo.includes.push({
                        file: includeMatch[1],
                        line: trimmed
                    });
                    continue;
                }

                // Defines
                const defineMatch = trimmed.match(REGEX.DEFINE);
                if (defineMatch) {
                    this.moduleInfo.defines.push({
                        name: defineMatch[1],
                        value: defineMatch[2] || '',
                        line: trimmed
                    });
                    continue;
                }

                // Behaviours
                const behaviourMatch = trimmed.match(REGEX.BEHAVIOUR);
                if (behaviourMatch) {
                    this.moduleInfo.behaviours.push(behaviourMatch[1]);
                    continue;
                }

                // Records
                if (REGEX.RECORD.test(trimmed)) {
                    const recordLines = this.collectMultiLineConstruct(i, '{', '}');
                    const recordMatch = trimmed.match(REGEX.RECORD);
                    if (recordMatch) {
                        this.moduleInfo.records.push({
                            name: recordMatch[1],
                            definition: recordLines.join('\n')
                        });
                    }
                    i += recordLines.length - 1;
                    continue;
                }

                // Types
                const typeMatch = trimmed.match(REGEX.TYPE);
                if (typeMatch) {
                    const typeLines = this.collectMultiLineConstruct(i, '(', ')');
                    this.moduleInfo.types.push({
                        name: typeMatch[1],
                        definition: typeLines.join('\n')
                    });
                    i += typeLines.length - 1;
                    continue;
                }

                // Specs (collect but don't process here)
                if (REGEX.SPEC.test(trimmed)) {
                    const specLines = this.collectMultiLineConstruct(i, '(', ')');
                    const specMatch = trimmed.match(REGEX.SPEC);
                    if (specMatch) {
                        this.moduleInfo.specs.push({
                            function: specMatch[1],
                            definition: specLines.join('\n')
                        });
                    }
                    i += specLines.length - 1;
                    continue;
                }

                // Other attributes
                const attrMatch = trimmed.match(REGEX.ATTRIBUTE);
                if (attrMatch) {
                    this.moduleInfo.attributes.push({
                        name: attrMatch[1],
                        line: trimmed
                    });
                    continue;
                }
            }

            // Module documentation - only collect at the beginning of the file
            if (firstChar === CHAR_CODES.PERCENT && trimmed.startsWith(STRINGS.TRIPLE_PERCENT) && !moduleDocCollected) {
                inModuleDoc = true;
                let docLine = trimmed.substring(3).trim();
                docLine = docLine.replace(REGEX.REMOVE_DOC, STRINGS.EMPTY);

                // Check for termination pattern (%%% ''')
                if (docLine === "'''") {
                    inModuleDoc = false; // End module documentation processing
                    moduleDocCollected = true; // Mark as collected
                    continue; // Continue processing rest of file
                }

                moduleDoc.push(docLine);
            } else if (inModuleDoc && (firstChar === CHAR_CODES.PERCENT || firstChar === CHAR_CODES.DASH)) {
                inModuleDoc = false;
                moduleDocCollected = true; // Mark as collected
                // Don't break - continue processing this line for module declarations
                i--; // Re-process this line outside module doc context
            }
        }

        this.moduleInfo.doc = this.cleanDocumentation(this.fixModuleDocCodeBlocks(moduleDoc.join(STRINGS.NEWLINE)));
    }

    collectMultiLineConstruct(startIdx, openChar, closeChar) {
        const lines = [];
        let depth = 0;
        let found = false;

        for (let i = startIdx; i < this.lines.length; i++) {
            const line = this.lines[i];
            lines.push(line);

            for (let j = 0; j < line.length; j++) {
                const char = line[j];
                if (char === openChar) {
                    depth++;
                    found = true;
                } else if (char === closeChar && found) {
                    depth--;
                    if (depth === 0) {
                        return lines;
                    }
                }
            }

            // Safety check for runaway constructs
            if (i - startIdx > 100) break;
        }

        return lines;
    }

    processFunctions() {
        const linesLength = this.lines.length;
        const processedFunctions = new Set();
        const commentedCodeBlocks = [];
        let currentCommentedBlock = [];
        let inCommentedBlock = false;

        for (let i = 0; i < linesLength; i++) {
            const line = this.lines[i];
            const trimmed = line.trim();

            if (!trimmed) {
                if (inCommentedBlock) {
                    currentCommentedBlock.push(line);
                }
                continue;
            }

            // Check for comment-style section headers
            if (trimmed.startsWith('%%%') && trimmed.match(/^%%%-{10,}$/)) {
                // This is a dash line, check if next line is a header and line after that is also dashes
                if (i + 1 < linesLength && i + 2 < linesLength) {
                    const nextLine = this.lines[i + 1].trim();
                    const afterLine = this.lines[i + 2].trim();

                    if (nextLine.startsWith('%%%') && !nextLine.match(/^%%%-{10,}$/) &&
                        afterLine.match(/^%%%-{10,}$/)) {
                        // Extract header text
                        const headerText = nextLine.replace(/^%%%\s*/, '').trim();
                        if (headerText) {
                            this.sections.push({
                                type: 'section_header',
                                title: headerText,
                                lineNumber: i + 2 // Store line number for sorting later
                            });
                        }
                        // Skip the next two lines
                        i += 2;
                        continue;
                    }
                }
            }

            // Check for commented-out code blocks (lines starting with % but containing code patterns)
            if (trimmed.startsWith('%') && !trimmed.startsWith('%%')) {
                const uncommented = trimmed.substring(1).trim();
                if (this.looksLikeCode(uncommented)) {
                    if (!inCommentedBlock) {
                        inCommentedBlock = true;
                        currentCommentedBlock = [];
                    }
                    currentCommentedBlock.push(line);
                    continue;
                } else if (inCommentedBlock) {
                    // End of commented code block
                    if (currentCommentedBlock.length > 0) {
                        commentedCodeBlocks.push({
                            type: 'commented_code',
                            lines: [...currentCommentedBlock],
                            startLine: i - currentCommentedBlock.length + 1
                        });
                    }
                    inCommentedBlock = false;
                    currentCommentedBlock = [];
                }
            } else if (inCommentedBlock) {
                // End of commented code block
                if (currentCommentedBlock.length > 0) {
                    commentedCodeBlocks.push({
                        type: 'commented_code',
                        lines: [...currentCommentedBlock],
                        startLine: i - currentCommentedBlock.length + 1
                    });
                }
                inCommentedBlock = false;
                currentCommentedBlock = [];
            }

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

            // Check for conditional compilation directives
            if (trimmed.startsWith('-ifdef(') || trimmed.startsWith('-ifndef(') ||
                trimmed.startsWith('-else') || trimmed.startsWith('-endif')) {
                this.addDirectiveToOutput(line, i);
                continue;
            }

            // Check for function start
            const funcMatch = trimmed.match(REGEX.FUNCTION);
            if (funcMatch && !this.currentState.inFunction) {
                const functionName = this.currentState.specFunctionName || funcMatch[1];
                processedFunctions.add(functionName);

                if (this.currentState.pendingDoc) {
                    this.currentState.functionDoc = this.currentState.pendingDoc;
                    this.currentState.pendingDoc = STRINGS.EMPTY;
                    this.startFunction(functionName, i);
                } else {
                    // This is an undocumented function
                    this.startUndocumentedFunction(functionName, i);
                    // Skip ahead to avoid reprocessing
                    while (i < linesLength && !this.isFunctionEnd(this.lines[i])) {
                        i++;
                    }
                }
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

        // Handle remaining commented code block
        if (inCommentedBlock && currentCommentedBlock.length > 0) {
            commentedCodeBlocks.push({
                type: 'commented_code',
                lines: currentCommentedBlock,
                startLine: linesLength - currentCommentedBlock.length + 1
            });
        }

        if (this.currentState.inFunction) {
            this.endFunction();
        }

        // Store commented code blocks for later inclusion
        this.commentedCodeBlocks = commentedCodeBlocks;
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

    startFunction(name, lineNumber = 0) {
        this.currentState.inFunction = true;
        this.currentState.functionName = name;
        this.currentState.functionLineNumber = lineNumber;
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
            body: processedBody,
            hasImplementation: true,
            lineNumber: this.currentState.functionLineNumber
        });

        // Reset state efficiently
        this.currentState.inFunction = false;
        this.currentState.functionName = STRINGS.EMPTY;
        this.currentState.functionLineNumber = 0;
        this.currentState.functionSpec = STRINGS.EMPTY;
        this.currentState.functionDoc = STRINGS.EMPTY;
        this.currentState.functionLines.length = 0;
        this.currentState.specFunctionName = STRINGS.EMPTY;
        this.currentState.inlineDocTags.length = 0;
    }

    startUndocumentedFunction(name, startLine) {
        const functionLines = [];
        let braceDepth = 0;
        let parenDepth = 0;
        let i = startLine;

        // Collect the entire function
        while (i < this.lines.length) {
            const line = this.lines[i];
            functionLines.push(line);

            // Track depth
            for (let j = 0; j < line.length; j++) {
                const charCode = line.charCodeAt(j);
                switch (charCode) {
                    case CHAR_CODES.OPEN_BRACE:
                    case CHAR_CODES.OPEN_BRACKET:
                        braceDepth++;
                        break;
                    case CHAR_CODES.CLOSE_BRACE:
                    case CHAR_CODES.CLOSE_BRACKET:
                        braceDepth--;
                        break;
                    case CHAR_CODES.OPEN_PAREN:
                        parenDepth++;
                        break;
                    case CHAR_CODES.CLOSE_PAREN:
                        parenDepth--;
                        break;
                }
            }

            // Check if function ended
            const trimmed = line.trim();
            if (braceDepth === 0 && parenDepth === 0 &&
                trimmed.charCodeAt(trimmed.length - 1) === CHAR_CODES.DOT &&
                trimmed.charCodeAt(0) !== CHAR_CODES.PERCENT) {
                break;
            }

            i++;
        }

        // Find corresponding spec
        const spec = this.moduleInfo.specs.find(s => s.function === name);

        this.undocumentedFunctions.push({
            name,
            spec: spec ? spec.definition : null,
            body: this.processFunctionBody(functionLines),
            lines: functionLines
        });
    }

    // Helper method to detect if a line looks like code
    looksLikeCode(line) {
        if (!line || line.length === 0) return false;

        // Check for common code patterns
        const codePatterns = [
            /^[a-z][a-z0-9_]*\s*\(/,           // function calls: function(
            /^[A-Z][a-zA-Z0-9_]*\s*=/,         // variable assignments: Var =
            /^\s*\{/,                          // tuples/records: {
            /^\s*\[/,                          // lists: [
            /^\s*case\s+/,                     // case statements
            /^\s*if\s+/,                       // if statements
            /^\s*catch\s+/,                    // catch blocks
            /^\s*after\s+/,                    // after blocks
            /^\s*end[,.]?\s*$/,                // end keywords
            /^\s*ok\s*$/,                      // ok atoms
            /^\s*true\s*$/,                    // boolean atoms
            /^\s*false\s*$/,                   // boolean atoms
            /->\s*$/,                          // arrow operators
            /^\s*\?[A-Z]/,                     // macro usage
            /^\s*[a-z_][a-z0-9_]*\s*\(/,      // function definitions
            /^\s*\d+\s*$/,                     // numbers
            /^\s*".*"\s*$/,                   // strings
            /^\s*<<.*>>\s*$/,                  // binaries
            /^\s*#\w+/,                        // record syntax
            /^\s*receive\s+/,                  // receive blocks
            /^\s*spawn/,                       // spawn calls
            /^\s*gen_server:/,                 // gen_server calls
            /^\s*supervisor:/,                 // supervisor calls
            /\bmatch\b|\bguard\b|\btry\b|\bfun\b/, // erlang keywords
        ];

        return codePatterns.some(pattern => pattern.test(line));
    }

    // Helper method to add conditional compilation directives
    addDirectiveToOutput(line, lineNumber) {
        this.conditionalDirectives.push({
            line: line.trim(),
            lineNumber: lineNumber + 1,
            type: 'conditional_compilation'
        });
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
                        const desc = this.cleanDocumentation(p.description || STRINGS.EMPTY, true);
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

    fixCodeBlocks(text) {
        if (!text) return text;

        const lines = text.split(STRINGS.NEWLINE);
        const result = [];
        let inCodeBlock = false;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmed = line.trim();

            // Check if this is a code block delimiter
            if (REGEX.CODE_FENCE.test(trimmed)) {
                if (!inCodeBlock && trimmed === '```') {
                    // Start of unmarked code block - check if it's empty first
                    let blockContent = [];
                    let j = i + 1;

                    // Collect content until closing ```
                    while (j < lines.length) {
                        const contentLine = lines[j];
                        if (contentLine.trim() === '```') {
                            break;
                        }
                        blockContent.push(contentLine);
                        j++;
                    }

                    // If block is empty, skip it entirely
                    const hasContent = blockContent.some(line => line.trim() !== '');
                    if (!hasContent) {
                        // Skip the empty code block entirely
                        i = j; // Skip to after the closing ```
                        continue;
                    }

                    // Determine appropriate language based on content
                    const nextLine = blockContent.length > 0 ? blockContent[0].trim() : '';
                    let language = 'text'; // default

                    // Heuristics to determine language based on content
                    if (nextLine.startsWith('/') || nextLine.includes('Parameters:') ||
                        nextLine.includes('- `') || nextLine.includes('(optional)')) {
                        language = 'text';
                    } else if (nextLine.includes('#{') || nextLine.includes('<<') ||
                              nextLine.includes('->') || nextLine.match(/^[a-z_]+\(/)) {
                        language = 'erlang';
                    }

                    // For text blocks, add ignore attribute to prevent mdBook testing
                    if (language === 'text') {
                        result.push('```text,ignore');
                    } else {
                        result.push('```' + language);
                    }
                    inCodeBlock = true;
                } else if (inCodeBlock && trimmed === '```') {
                    // End of code block
                    result.push(line);
                    inCodeBlock = false;
                } else {
                    // Already has language specifier or other case
                    result.push(line);
                    if (trimmed.startsWith('```')) {
                        inCodeBlock = !inCodeBlock;
                    }
                }
            } else {
                result.push(line);
            }
        }

        return result.join(STRINGS.NEWLINE);
    }

    fixModuleDocCodeBlocks(text) {
        if (!text) return STRINGS.EMPTY;

        const lines = text.split(STRINGS.NEWLINE);
        const result = [];
        let inCodeBlock = false;
        let codeBlockStart = -1;

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmed = line.trim();

            // Check for code block start
            if (trimmed === '```' && !inCodeBlock) {
                inCodeBlock = true;
                codeBlockStart = i;
                result.push(line);
                continue;
            }

            // Check for code block end
            if (trimmed === '```' && inCodeBlock) {
                inCodeBlock = false;
                result.push(line);
                continue;
            }

            // Check for implicit code block end (new section starting with /)
            if (inCodeBlock && trimmed.startsWith('/')) {
                // Close the previous code block
                result.push('```');
                result.push('');
                inCodeBlock = false;
            }

            result.push(line);
        }

        // Close any unclosed code block at the end
        if (inCodeBlock) {
            result.push('```');
        }

        return result.join(STRINGS.NEWLINE);
    }

    cleanDocumentation(text, skipCodeBlockFix = false) {
        if (!text) return STRINGS.EMPTY;

        text = text.replace(REGEX.PRE_TAG, (match, content) => this.formatPreContent(content));

        // Only fix unmarked code blocks for module-level documentation
        // Skip for function documentation to avoid excessive text,ignore blocks
        if (!skipCodeBlockFix) {
            text = this.fixCodeBlocks(text);
        }

        let cleaned = text
            .replace(REGEX.BACKTICK_QUOTE, `${STRINGS.BACKTICK}$1${STRINGS.BACKTICK}`)
            .replace(REGEX.HTML_ENTITIES_LT, '<<')
            .replace(REGEX.HTML_ENTITIES_GT, '>>')
            .replace(REGEX.REMOVE_DOC, STRINGS.EMPTY)
            .replace(REGEX.MULTIPLE_NEWLINES, '\n\n')
            .replace(REGEX.TRAILING_SPACES, STRINGS.EMPTY)
            .replace(REGEX.TRIM, STRINGS.EMPTY);

        return this.reflowNumberedLists(cleaned);
    }

    convertCommentStyleHeaders(text) {
        if (!text) return text;

        const lines = text.split(STRINGS.NEWLINE);
        const result = [];

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmed = line.trim();

            // Look for pattern: dashes followed by text followed by dashes
            if (trimmed.match(/^-{10,}$/)) {
                // This is a dash line, check if next line is a header
                if (i + 1 < lines.length) {
                    const nextLine = lines[i + 1];
                    const nextTrimmed = nextLine.trim();

                    // Check if the line after is also dashes (closing the header)
                    if (i + 2 < lines.length && lines[i + 2].trim().match(/^-{10,}$/)) {
                        // This is a comment-style header: convert to markdown
                        if (nextTrimmed) {
                            result.push(`## ${nextTrimmed}`);
                            result.push(STRINGS.EMPTY);
                        }
                        // Skip the next two lines (header text and closing dashes)
                        i += 2;
                        continue;
                    }
                }
            }

            result.push(line);
        }

        return result.join(STRINGS.NEWLINE);
    }

    generateInterleavedContent(md) {
        // Create a combined list of sections and functions, sorted by line number
        const contentItems = [];

        // Add sections
        for (const section of this.sections) {
            if (section.type === 'section_header') {
                contentItems.push({
                    type: 'section',
                    lineNumber: section.lineNumber,
                    title: section.title
                });
            }
        }

        // Add functions
        const groupedFunctions = this.groupFunctionsByName(this.functions);
        for (const group of groupedFunctions) {
            // Use the line number of the first function in the group
            const lineNumber = group.functions[0]?.lineNumber || 0;
            contentItems.push({
                type: 'function_group',
                lineNumber: lineNumber,
                group: group
            });
        }

        // Sort by line number
        contentItems.sort((a, b) => a.lineNumber - b.lineNumber);

        // Generate markdown for each item
        for (const item of contentItems) {
            if (item.type === 'section') {
                md.push(`## ${item.title}`);
                md.push(STRINGS.EMPTY);
            } else if (item.type === 'function_group') {
                this.generateFunctionGroupMarkdown(md, item.group);
            }
        }
    }

    generateFunctionGroupMarkdown(md, group) {
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

    formatReturnsText(text) {
        if (!text) return STRINGS.EMPTY;
        let result = this.cleanDocumentation(text, true);

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
        const s = this.cleanDocumentation(text, true);
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

    addSeparator(md) {
        // Only add separator if the last entry is not empty and not already a separator
        if (md.length > 0) {
            const lastLine = md[md.length - 1];
            const prevLine = md.length > 1 ? md[md.length - 2] : '';

            // Don't add separator if last line is already empty and previous is separator
            if (lastLine === STRINGS.EMPTY && prevLine === STRINGS.SEPARATOR) {
                return;
            }

            // Don't add separator if last line is already a separator
            if (lastLine === STRINGS.SEPARATOR) {
                return;
            }

            // Add separator with proper spacing
            if (lastLine !== STRINGS.EMPTY) {
                md.push(STRINGS.EMPTY);
            }
            md.push(STRINGS.SEPARATOR);
            md.push(STRINGS.EMPTY);
        }
    }

    generateMarkdown(fileName) {
        const githubUrl = `${this.options.githubBase}/${fileName}`;
        const md = [];

        // Header
        md.push(`# ${this.moduleInfo.name || fileName.replace('.erl', STRINGS.EMPTY)}`);
        md.push(STRINGS.EMPTY);
        md.push(`[View source on GitHub](${githubUrl})`);
        md.push(STRINGS.EMPTY);

        // Metadata section
        this.generateMetadataSection(md);

        // Module documentation
        if (this.moduleInfo.doc) {
            md.push(this.moduleInfo.doc);
            this.addSeparator(md);
        }

        // Generate interleaved content (sections and functions) sorted by line number
        this.generateInterleavedContent(md);

        // Commented-out code blocks
        if (this.commentedCodeBlocks && this.commentedCodeBlocks.length > 0) {
            md.push('## Commented-Out Code');
            md.push(STRINGS.EMPTY);
            md.push('*The following code blocks are commented out but may contain useful examples:*');
            md.push(STRINGS.EMPTY);

            for (const block of this.commentedCodeBlocks) {
                md.push('```erlang');
                for (const line of block.lines) {
                    md.push(line);
                }
                md.push('```');
                md.push(STRINGS.EMPTY);
            }
        }

        // Conditional compilation directives
        if (this.conditionalDirectives && this.conditionalDirectives.length > 0) {
            md.push('## Conditional Compilation');
            md.push(STRINGS.EMPTY);
            md.push('*The following conditional compilation directives are used in this module:*');
            md.push(STRINGS.EMPTY);

            md.push('```erlang');
            for (const directive of this.conditionalDirectives) {
                md.push(directive.line);
            }
            md.push('```');
            md.push(STRINGS.EMPTY);
        }

        // Undocumented functions section
        if (this.undocumentedFunctions.length > 0) {
            md.push('## Undocumented Functions');
            md.push(STRINGS.EMPTY);
            md.push('*The following functions lack documentation comments but are included for completeness:*');
            md.push(STRINGS.EMPTY);

            for (const func of this.undocumentedFunctions) {
                md.push(`### ${func.name}`);
                md.push(STRINGS.EMPTY);

                if (func.spec) {
                    md.push(`\`\`\`${STRINGS.ERLANG}`);
                    md.push(func.spec.trim());
                    md.push('```');
                    md.push(STRINGS.EMPTY);
                }

                if (func.body?.length > 0) {
                    for (const segment of func.body) {
                        if (segment.type === 'comment') {
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

            this.addSeparator(md);
        }

        this.addSeparator(md);
        md.push(`*Generated from [${fileName}](${githubUrl})*`);

        const finalMarkdown = md.join(STRINGS.NEWLINE);
        return finalMarkdown;
    }

    generateMetadataSection(md) {
        md.push('## Module Metadata');
        md.push(STRINGS.EMPTY);

        // Basic module information
        md.push(`**Module:** \`${this.moduleInfo.name || 'unknown'}\``);
        md.push(`**Exports:** ${this.moduleInfo.exports.length} functions`);

        if (this.moduleInfo.behaviours.length > 0) {
            md.push(`**Behaviours:** ${this.moduleInfo.behaviours.map(b => `\`${b}\``).join(', ')}`);
        }

        if (this.moduleInfo.includes.length > 0) {
            md.push(`**Includes:** ${this.moduleInfo.includes.length} files`);
        }

        if (this.moduleInfo.defines.length > 0) {
            md.push(`**Defines:** ${this.moduleInfo.defines.length} macros`);
        }

        if (this.moduleInfo.records.length > 0) {
            md.push(`**Records:** ${this.moduleInfo.records.length} records`);
        }

        if (this.moduleInfo.types.length > 0) {
            md.push(`**Types:** ${this.moduleInfo.types.length} type definitions`);
        }

        md.push(STRINGS.EMPTY);

        // Exports section
        if (this.moduleInfo.exports.length > 0) {
            md.push('### Exported Functions');
            md.push(STRINGS.EMPTY);
            this.moduleInfo.exports.forEach(exp => {
                md.push(`- \`${exp}\``);
            });
            md.push(STRINGS.EMPTY);
        }

        // Includes section
        if (this.moduleInfo.includes.length > 0) {
            md.push('### Includes');
            md.push(STRINGS.EMPTY);
            md.push('```erlang');
            this.moduleInfo.includes.forEach(inc => {
                md.push(inc.line);
            });
            md.push('```');
            md.push(STRINGS.EMPTY);
        }

        // Defines section
        if (this.moduleInfo.defines.length > 0) {
            md.push('### Macro Definitions');
            md.push(STRINGS.EMPTY);
            md.push('```erlang');
            this.moduleInfo.defines.forEach(def => {
                md.push(def.line);
            });
            md.push('```');
            md.push(STRINGS.EMPTY);
        }

        // Records section
        if (this.moduleInfo.records.length > 0) {
            md.push('### Record Definitions');
            md.push(STRINGS.EMPTY);
            this.moduleInfo.records.forEach(rec => {
                md.push(`#### \`${rec.name}\``);
                md.push(STRINGS.EMPTY);
                md.push('```erlang');
                md.push(rec.definition);
                md.push('```');
                md.push(STRINGS.EMPTY);
            });
        }

        // Types section
        if (this.moduleInfo.types.length > 0) {
            md.push('### Type Definitions');
            md.push(STRINGS.EMPTY);
            this.moduleInfo.types.forEach(type => {
                md.push(`#### \`${type.name}\``);
                md.push(STRINGS.EMPTY);
                md.push('```erlang');
                md.push(type.definition);
                md.push('```');
                md.push(STRINGS.EMPTY);
            });
        }

        this.addSeparator(md);
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
                    combinedDoc.push(this.cleanDocumentation(parsed.description.join(STRINGS.NEWLINE), true));
                    combinedDoc.push(STRINGS.EMPTY);
                }

                if (parsed.params.length > 0) {
                    combinedDoc.push(STRINGS.PARAMETERS_HEADER);
                    combinedDoc.push(STRINGS.EMPTY);
                    parsed.params.forEach(param => {
                        const desc = this.cleanDocumentation(param.description, true);
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
    const outputDir = process.env.OUTPUT_DIR || path.join(process.cwd(), 'docs/book/src');

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