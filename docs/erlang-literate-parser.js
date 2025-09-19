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
import { execSync } from 'child_process';

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
        this.moduleInfo = {};
        this.functions = [];
        this.currentState = {
            inFunction: false,
            functionName: '',
            functionSpec: '',
            functionDoc: '',
            functionLines: [],
            pendingDoc: '',
            specFunctionName: '',
            braceDepth: 0,
            parenDepth: 0,
            inlineDocTags: []
        };
    }

    parseFile(filePath) {
        const content = fs.readFileSync(filePath, 'utf8');
        this.reset();
        this.lines = content.split('\n');

        // Extract module-level information
        this.extractModuleInfo();

        // Process all lines for functions
        this.processFunctions();

        // Generate the markdown
        return this.generateMarkdown(path.basename(filePath));
    }

    extractModuleInfo() {
        let moduleDoc = [];
        let inModuleDoc = false;

        for (let i = 0; i < this.lines.length; i++) {
            const line = this.lines[i];
            const trimmed = line.trim();

            // Module name
            if (trimmed.match(/^-module\(([^)]+)\)/)) {
                this.moduleInfo.name = trimmed.match(/^-module\(([^)]+)\)/)[1];
            }

            // Exports
            if (trimmed.match(/^-export\(\[/)) {
                const exportMatch = trimmed.match(/^-export\(\[([^\]]+)\]\)/);
                if (exportMatch) {
                    this.moduleInfo.exports = exportMatch[1]
                        .split(',')
                        .map(e => e.trim())
                        .filter(e => e);
                }
            }

            // Module documentation (%%% comments at the top)
            if (trimmed.startsWith('%%%')) {
                inModuleDoc = true;
                let docLine = trimmed.substring(3).trim();
                // Remove @doc if present
                docLine = docLine.replace(/^@doc\s*/, '');
                // Always push the line, even if empty (for paragraph breaks)
                moduleDoc.push(docLine);
            } else if (inModuleDoc && trimmed === '') {
                // Empty line in module doc, preserve it for paragraph breaks
                moduleDoc.push('');
            } else if (inModuleDoc && trimmed.startsWith('%')) {
                // Continue with other comment types but end module doc
                inModuleDoc = false;
                break;
            } else if (inModuleDoc && trimmed.startsWith('-')) {
                // Hit a module directive, end of module doc
                break;
            }
        }

        this.moduleInfo.doc = this.cleanDocumentation(moduleDoc.join('\n'));
    }

    processFunctions() {
        for (let i = 0; i < this.lines.length; i++) {
            const line = this.lines[i];
            const trimmed = line.trim();

            // Check for start of function documentation block
            if (trimmed.startsWith('%% @doc')) {
                this.collectFunctionDoc(i);
                continue;
            }

            // Check for -spec
            if (trimmed.startsWith('-spec ')) {
                // Before collecting the spec, check if there are @param/@returns tags immediately before
                this.collectParamTagsBeforeSpec(i);
                this.collectSpec(i);
                // Extract function name from spec line
                const specMatch = trimmed.match(/-spec\s+([a-z][a-z0-9_]*)\s*\(/);
                if (specMatch) {
                    this.currentState.specFunctionName = specMatch[1];
                }
                continue;
            }

            // Check for function start (handles both start-of-line and indented functions)
            const funcMatch = trimmed.match(/^([a-z][a-z0-9_]*)\s*\(/);
            if (funcMatch && !this.currentState.inFunction) {
                // Save any pending doc
                if (this.currentState.pendingDoc) {
                    this.currentState.functionDoc = this.currentState.pendingDoc;
                    this.currentState.pendingDoc = '';
                }

                // Use function name from spec if available, otherwise use detected name
                const functionName = this.currentState.specFunctionName || funcMatch[1];
                this.startFunction(functionName, i);

                // Clear the spec function name after use
                this.currentState.specFunctionName = '';
            }

            // If in function, collect lines
            if (this.currentState.inFunction) {
                this.collectFunctionLine(line, i);

                // Check for function end
                if (this.isFunctionEnd(line)) {
                    this.endFunction();
                }
            }
        }

        // Handle any remaining function
        if (this.currentState.inFunction) {
            this.endFunction();
        }
    }

    isFunctionDoc(line) {
        return line.startsWith('%% @doc') ||
               (line.startsWith('%%') && !line.startsWith('%%%'));
    }

    collectFunctionDoc(startIdx) {
        const docLines = [];

        for (let i = startIdx; i < this.lines.length; i++) {
            const line = this.lines[i];
            const trimmed = line.trim();

            if (trimmed.startsWith('%%')) {
                let docLine = trimmed.substring(2).trim();
                // Remove @doc prefix if on first line
                if (i === startIdx) {
                    docLine = docLine.replace(/^@doc\s*/, '');
                }
                docLines.push(docLine);
            } else if (trimmed === '') {
                // Empty line - might be part of doc block or end
                // Look ahead to see if more %% comments follow
                let j = i + 1;
                let foundMoreDoc = false;
                while (j < this.lines.length && this.lines[j].trim() === '') {
                    j++;
                }
                if (j < this.lines.length && this.lines[j].trim().startsWith('%%')) {
                    // More doc coming, include empty line
                    docLines.push('');
                    foundMoreDoc = true;
                }
                if (!foundMoreDoc) {
                    // End of doc block
                    break;
                }
            } else if (trimmed.startsWith('%')) {
                // Single % comment, continue but don't include
                continue;
            } else {
                // End of doc block
                break;
            }
        }

        this.currentState.pendingDoc = docLines.join('\n');
    }

    collectParamTagsBeforeSpec(specIdx) {
        const paramLines = [];
        let hitDocBlock = false;

        // Look backwards from the -spec line to find @param and @returns tags
        for (let i = specIdx - 1; i >= 0; i--) {
            const line = this.lines[i];
            const trimmed = line.trim();

            // If we hit a @doc line, we already collected this documentation
            if (trimmed.startsWith('%% @doc')) {
                hitDocBlock = true;
                break;
            }

            // If we hit an empty line or a non-comment line, stop looking backwards
            if (trimmed === '' || (!trimmed.startsWith('%%') && !trimmed.startsWith('%'))) {
                break;
            }

            // Check if this line contains @param or @returns
            if (trimmed.startsWith('%%') && (trimmed.includes('@param') || trimmed.includes('@returns'))) {
                let docLine = trimmed.substring(2).trim();
                paramLines.unshift(docLine); // Add to beginning to maintain order
            }
        }

        // Only add the tags if we didn't find a @doc block (meaning these are standalone tags)
        if (paramLines.length > 0 && !hitDocBlock) {
            const existingDoc = this.currentState.pendingDoc || '';
            const newDoc = paramLines.join('\n');

            if (existingDoc) {
                this.currentState.pendingDoc = existingDoc + '\n' + newDoc;
            } else {
                this.currentState.pendingDoc = newDoc;
            }
        }
    }

    collectSpec(startIdx) {
        const specLines = [];
        let depth = 0;

        for (let i = startIdx; i < this.lines.length; i++) {
            const line = this.lines[i];
            specLines.push(line);

            // Track parentheses to handle multi-line specs
            for (const char of line) {
                if (char === '(') depth++;
                if (char === ')') depth--;
            }

            // Check if spec is complete
            if (line.trim().endsWith('.') && depth === 0) {
                break;
            }
        }

        this.currentState.functionSpec = specLines.join('\n');

        // After collecting spec, look for the actual function definition
        // that should follow shortly after
        for (let j = startIdx + specLines.length; j < this.lines.length; j++) {
            const nextLine = this.lines[j].trim();

            // Skip empty lines and comments
            if (nextLine === '' || nextLine.startsWith('%')) {
                continue;
            }

            // Look for function definition
            const funcMatch = nextLine.match(/^([a-z][a-z0-9_]*)\s*\(/);
            if (funcMatch) {
                // This is likely the function that corresponds to this spec
                // But don't start the function here, let the main loop handle it
                break;
            }

            // If we hit another -spec or module directive, stop looking
            if (nextLine.startsWith('-spec') || nextLine.startsWith('-')) {
                break;
            }
        }
    }

    startFunction(name, lineIdx) {
        this.currentState.inFunction = true;
        this.currentState.functionName = name;
        this.currentState.functionLines = [];
        this.currentState.braceDepth = 0;
        this.currentState.parenDepth = 0;
        this.currentState.inlineDocTags = [];
    }

    collectFunctionLine(line, lineIdx) {
        this.currentState.functionLines.push(line);

        // Track depth for function end detection
        for (const char of line) {
            if (char === '{' || char === '[') this.currentState.braceDepth++;
            if (char === '}' || char === ']') this.currentState.braceDepth--;
            if (char === '(') this.currentState.parenDepth++;
            if (char === ')') this.currentState.parenDepth--;
        }
    }

    isFunctionEnd(line) {
        const trimmed = line.trim();

        // Function ends with . at depth 0
        if (this.currentState.braceDepth === 0 &&
            this.currentState.parenDepth === 0 &&
            trimmed.endsWith('.') &&
            !trimmed.startsWith('%')) {
            return true;
        }

        return false;
    }

    endFunction() {
        // Process the function body to extract inline comments
        const processedBody = this.processFunctionBody(this.currentState.functionLines);

        this.functions.push({
            name: this.currentState.functionName,
            spec: this.currentState.functionSpec,
            doc: this.currentState.functionDoc,
            body: processedBody
        });

        // Reset state
        this.currentState.inFunction = false;
        this.currentState.functionName = '';
        this.currentState.functionSpec = '';
        this.currentState.functionDoc = '';
        this.currentState.functionLines = [];
        this.currentState.specFunctionName = '';
        this.currentState.inlineDocTags = [];
    }

    processFunctionBody(lines) {
        const segments = [];
        let currentCode = [];
        let pendingTagLines = [];

        const flushCode = () => {
            if (currentCode.length > 0) {
                segments.push({ type: 'code', content: currentCode.join('\n') });
                currentCode = [];
            }
        };

        const flushTags = () => {
            if (pendingTagLines.length > 0) {
                const tagText = pendingTagLines.join('\n');
                const parsed = this.parseDocumentation(tagText);
                let docParts = [];
                if (parsed.params.length > 0) {
                    docParts.push('### Parameters');
                    docParts.push('');
                    for (const p of parsed.params) {
                        const desc = this.cleanDocumentation(p.description || '');
                        docParts.push(`- \`${p.name}\` - ${desc}`);
                    }
                    docParts.push('');
                }
                if (parsed.returns.length > 0) {
                    docParts.push('### Returns');
                    docParts.push('');
                    const expanded = parsed.returns.flatMap(r => this.splitReturnsIntoOutcomes(r));
                    for (const r of expanded) {
                        docParts.push(`- ${this.formatReturnsText(r)}`);
                    }
                    docParts.push('');
                }
                if (docParts.length > 0) {
                    segments.push({ type: 'doc', content: docParts.join('\n') });
                }
                pendingTagLines = [];
            }
        };

        for (const line of lines) {
            const trimmed = line.trim();

            if (trimmed.match(/^\s*%[^%]/) || trimmed.match(/^\s*%%[^%]/)) {
                // It's a comment line
                // Save any accumulated code block first
                if (currentCode.length > 0) {
                    flushCode();
                }

                // Extract comment text (remove % or %% prefix)
                let commentText;
                if (trimmed.match(/^\s*%%[^%]/)) {
                    commentText = line.replace(/^\s*%%\s?/, '');
                } else {
                    commentText = line.replace(/^\s*%\s?/, '');
                }
                const cleaned = this.cleanInlineComment(commentText);

                // Heuristic: returns-like lines (e.g., `{ok, Binary}` / `{error, Binary}` ...)
                const returnsLikeTuple = /^`?\{[^}]+\}`?/.test(cleaned);
                const returnsLikeAtom = /^`?(ok|error|not_found|true|false)\b/i.test(cleaned);
                const isTagParam = /^\s*@param\b/i.test(cleaned);
                const isTagReturns = /^\s*@returns?\b/i.test(cleaned);

                if (isTagParam || isTagReturns || returnsLikeTuple || returnsLikeAtom) {
                    const lineAsTag = isTagParam || isTagReturns
                        ? cleaned.trim()
                        : `@returns ${cleaned.trim()}`;
                    // Accumulate tag lines
                    pendingTagLines.push(lineAsTag);
                } else if (
                    pendingTagLines.length > 0 &&
                    (pendingTagLines[pendingTagLines.length - 1].startsWith('@returns') ||
                     pendingTagLines[pendingTagLines.length - 1].startsWith('@param'))
                ) {
                    // Continuation of the previous @returns/@param block; append line
                    pendingTagLines.push(cleaned.trim());
                } else {
                    // Flush any pending tag block before emitting a normal comment
                    flushTags();
                    segments.push({ type: 'comment', content: cleaned });
                }
            } else {
                // Non-comment code line; flush any pending tags first, then add code
                flushTags();
                currentCode.push(line);
            }
        }

        // Flush any remaining tag or code blocks
        flushTags();
        flushCode();

        return segments;
    }

    cleanInlineComment(text) {
        // Convert `thing' to `thing`
        return text.replace(/`([^']*?)'/g, '`$1`').trim();
    }

    cleanDocumentation(text) {
        if (!text) return '';

        // Handle <pre> tags with structured content
        text = text.replace(/<pre>([\s\S]*?)<\/pre>/g, (match, content) => {
            return this.formatPreContent(content);
        });

        // Convert Erlang doc syntax to Markdown
        let cleaned = text
            .replace(/`([^']*?)'/g, '`$1`')  // Convert `code' to `code`
            .replace(/&lt;&lt;/g, '<<')       // Fix HTML entities
            .replace(/&gt;&gt;/g, '>>')
            .replace(/@doc\s*/g, '')          // Remove @doc tags
            .replace(/\n\s*\n\s*\n/g, '\n\n')   // Normalize multiple empty lines to double newlines
            .replace(/[ \t]+$/gm, '')               // Trim trailing spaces per line
            .replace(/^\s+|\s+$/g, '');            // Final trim

        // Reflow numbered lists and ensure separation from following headings/labels
        cleaned = this.reflowNumberedLists(cleaned);

        return cleaned;
    }

    formatReturnsText(text) {
        if (!text) return '';
        // First, clean the documentation text
        let result = this.cleanDocumentation(text);

        // Wrap leading return token if it's a tuple/list or common atom
        const leadingMatch = result.match(/^(\s*)(\{[^}]+\}|\[[^\]]+\]|ok|error|not_found|true|false)(\b|\s|$)/i);
        if (leadingMatch) {
            const [, leadSpace, token, trail] = leadingMatch;
            result = leadSpace + '`' + token + '`' + result.slice(leadSpace.length + token.length);
        }

        // Wrap any standalone tuple occurrences not already inside backticks
        result = result.replace(/(^|[^`])(\{[^}]+\})([^`]|$)/g, (m, pre, tuple, post) => {
            return `${pre}\`${tuple}\`${post}`;
        });

        return result;
    }

    splitReturnsIntoOutcomes(text) {
        if (!text) return [];
        const s = this.cleanDocumentation(text);
        const tokenRegex = /(\{[^}]+\}|\bok\b|\berror\b|\bnot_found\b|\btrue\b|\bfalse\b)/gi;
        const parts = [];
        let match;
        const matches = [];
        while ((match = tokenRegex.exec(s)) !== null) {
            matches.push({ index: match.index, token: match[0] });
        }
        // If no tokens or prose exists before the first token, don't split; keep as one descriptive line
        if (matches.length === 0 || (matches.length > 0 && matches[0].index > 0)) {
            return [s.trim()];
        }
        for (let i = 0; i < matches.length; i++) {
            const start = matches[i].index;
            const nextStart = (i + 1 < matches.length) ? matches[i + 1].index : s.length;
            let segment = s.slice(start, nextStart).trim();
            // Remove leading commas that were used as separators
            segment = segment.replace(/^,\s*/, '');
            // If there's trailing comma before next token, trim it but keep sentence end
            segment = segment.replace(/,\s*$/, '');
            if (segment) parts.push(segment.trim());
        }
        // If we accidentally merged two outcomes without clear token boundaries, ensure uniqueness
        return parts.filter(p => p.length > 0);
    }

    reflowNumberedLists(text) {
        if (!text) return '';
        const lines = text.split('\n');
        const out = [];
        let inNumbered = false;
        let lastNumIndex = -1;
        for (let i = 0; i < lines.length; i++) {
            const raw = lines[i];
            const trimmed = raw.trim();

            const isNumbered = /^\d+\.\s/.test(trimmed);
            const isBullet = /^[-*]\s/.test(trimmed);
            const isHeading = /^#{1,6}\s/.test(trimmed);
            const isCodeFence = /^```/.test(trimmed);

            if (isNumbered) {
                out.push(trimmed);
                inNumbered = true;
                lastNumIndex = out.length - 1;
                continue;
            }

            if (inNumbered) {
                if (trimmed === '') {
                    out.push('');
                    inNumbered = false;
                    lastNumIndex = -1;
                    continue;
                }
                if (!isNumbered && !isBullet && !isHeading && !isCodeFence) {
                    // Continuation of previous numbered item; append
                    out[lastNumIndex] = out[lastNumIndex] + ' ' + trimmed;
                    continue;
                }
                // Different kind of line; end numbered block and fall through
                inNumbered = false;
                lastNumIndex = -1;
            }

            out.push(raw);
        }

        // Ensure a blank line between last numbered item and a label/heading line like 'Config options ...:'
        const separated = [];
        for (let i = 0; i < out.length; i++) {
            const cur = out[i];
            const next = i + 1 < out.length ? out[i + 1] : '';
            separated.push(cur);
            if (/^\d+\.\s/.test(cur.trim()) && next && !/^\s*$/.test(next) && /:\s*$/.test(next.trim())) {
                // Insert a blank line if not already present
                if (separated[separated.length - 1] !== '') {
                    separated.push('');
                }
            }
        }

        return separated.join('\n');
    }

    formatPreContent(content) {
        // First, let's look at the actual structure of the content more carefully
        // The issue is that definitions span multiple lines with varying indentation

        const lines = content.trim().split('\n');
        const formatted = [];

        let i = 0;
        while (i < lines.length) {
            const line = lines[i].trim();

            if (!line) {
                i++;
                continue;
            }

            // Look for definition pattern: starts with word(s), colon, then description
            // Pattern: "DevMod:ExportedFunc : Description" or "info/exports : Description"
            const defMatch = line.match(/^(\S+(?:\s*:\s*\S+)?)\s*:\s*(.*)$/);

            if (defMatch) {
                const [, term, initialDesc] = defMatch;
                let fullDescription = initialDesc.trim();

                // Collect continuation lines for this definition
                let j = i + 1;
                while (j < lines.length) {
                    const nextLine = lines[j];

                    // Empty line - check if there's more content
                    if (!nextLine.trim()) {
                        j++;
                        continue;
                    }

                    // If it looks like a new definition, stop
                    if (nextLine.trim().match(/^\S+(?:\s*:\s*\S+)?\s*:\s*/)) {
                        break;
                    }

                    // This is a continuation line - add it to the description
                    if (nextLine.trim()) {
                        fullDescription += ' ' + nextLine.trim();
                    }
                    j++;
                }

                // Format the definition
                formatted.push('');
                formatted.push(`**${term.trim()}**`);
                formatted.push('');
                formatted.push(fullDescription);

                i = j; // Move to the next unprocessed line
            } else {
                // Not a definition - handle as regular content
                if (line.toLowerCase().includes('hyperbeam') && line.includes('options')) {
                    formatted.push('');
                    formatted.push(`### ${line}`);
                    formatted.push('');
                } else if (line.match(/^`[^`]+`\s*:/)) {
                    // Special case for option definitions like `update_hashpath`:
                    const optMatch = line.match(/^(`[^`]+`)\s*:\s*(.*)$/);
                    if (optMatch) {
                        const [, optName, optDesc] = optMatch;
                        formatted.push('');
                        formatted.push(`**${optName}**`);
                        formatted.push('');
                        formatted.push(optDesc);
                    } else {
                        formatted.push(line);
                    }
                } else {
                    formatted.push(line);
                }
                i++;
            }
        }

        return formatted.join('\n');
    }

    parseDocumentation(docText) {
        const lines = docText.split('\n');
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

            // Check for @param
            const paramMatch = trimmed.match(/^@param\s+(\S+)\s*(.*)/);
            if (paramMatch) {
                if (currentParam) {
                    result.params.push(currentParam);
                }
                currentParam = {
                    name: paramMatch[1],
                    description: paramMatch[2] || ''
                };
                currentSection = 'param';
                continue;
            }

            // Check for @returns
            if (trimmed.match(/^@returns?\s/)) {
                if (currentParam) {
                    result.params.push(currentParam);
                    currentParam = null;
                }
                const returnsText = trimmed.replace(/^@returns?\s*/, '');
                result.returns.push(returnsText);
                lastReturnIndex = result.returns.length - 1;
                currentSection = 'returns';
                continue;
            }

            // Add to current section
            if (currentSection === 'description') {
                if (trimmed) {
                    result.description.push(trimmed);
                } else {
                    // Preserve a single blank line to break paragraphs/lists
                    const last = result.description[result.description.length - 1];
                    if (last !== '') {
                        result.description.push('');
                    }
                }
            } else if (currentSection === 'param' && currentParam && trimmed) {
                currentParam.description += ' ' + trimmed;
            } else if (currentSection === 'returns' && trimmed) {
                if (lastReturnIndex >= 0) {
                    // Append continuation text to the last returns entry
                    result.returns[lastReturnIndex] =
                        (result.returns[lastReturnIndex] + ' ' + trimmed).replace(/\s+/g, ' ').trim();
                } else {
                    result.returns.push(trimmed);
                    lastReturnIndex = result.returns.length - 1;
                }
            }
        }

        // Save final param if exists
        if (currentParam) {
            result.params.push(currentParam);
        }

        return result;
    }

    generateMarkdown(fileName) {
        const githubUrl = `${this.options.githubBase}/${fileName}`;
        let md = [];

        // Header
        md.push(`# ${this.moduleInfo.name || fileName.replace('.erl', '')}`);
        md.push('');
        md.push(`[View source on GitHub](${githubUrl})`);
        md.push('');

        // Module documentation
        if (this.moduleInfo.doc) {
            md.push(this.moduleInfo.doc);
            md.push('');
            md.push('---');
            md.push('');
        }

        // Exports
        if (this.moduleInfo.exports && this.moduleInfo.exports.length > 0) {
            md.push('## Exported Functions');
            md.push('');
            for (const exp of this.moduleInfo.exports) {
                md.push(`- \`${exp}\``);
            }
            md.push('');
            md.push('---');
            md.push('');
        }

        // Group functions by name to merge overloaded functions
        const groupedFunctions = this.groupFunctionsByName(this.functions);

        // Functions
        for (const group of groupedFunctions) {
            md.push(`## ${group.name}`);
            md.push('');

            // Combine documentation from all functions in the group
            const combinedDoc = this.combineFunctionDocs(group.functions);
            if (combinedDoc) {
                md.push(combinedDoc);
                md.push('');
            }

            // Add all specs and bodies for the function group
            for (const func of group.functions) {
                // Spec
                if (func.spec) {
                    md.push('```erlang');
                    md.push(func.spec.trim());
                    md.push('```');
                    md.push('');
                }

                // Function body with inline comments
                if (func.body && func.body.length > 0) {
                    md.push('');

                    for (const segment of func.body) {
                        if (segment.type === 'comment') {
                            md.push(segment.content);
                            md.push('');
                        } else if (segment.type === 'doc') {
                            // Insert structured params/returns adjacent to the preceding code
                            md.push(segment.content);
                            md.push('');
                        } else if (segment.type === 'code') {
                            md.push('```erlang');
                            md.push(segment.content.trim());
                            md.push('```');
                            md.push('');
                        }
                    }
                }
            }

            md.push('');
        }

        // Footer
        md.push('---');
        md.push('');
        md.push(`*Generated from [${fileName}](${githubUrl})*`);

        return md.join('\n');
    }

    groupFunctionsByName(functions) {
        const groups = [];
        let currentGroup = null;

        for (const func of functions) {
            if (!currentGroup || currentGroup.name !== func.name) {
                // Start a new group
                currentGroup = {
                    name: func.name,
                    functions: [func]
                };
                groups.push(currentGroup);
            } else {
                // Add to current group
                currentGroup.functions.push(func);
            }
        }

        return groups;
    }

    combineFunctionDocs(functions) {
        // Use the documentation from the first function that has it
        // In practice, usually only the first clause of an overloaded function has detailed docs
        for (const func of functions) {
            if (func.doc) {
                const parsed = this.parseDocumentation(func.doc);
                let combinedDoc = [];

                // Description
                if (parsed.description.length > 0) {
                    combinedDoc.push(this.cleanDocumentation(parsed.description.join('\n')));
                    combinedDoc.push('');
                }

                // Parameters
                if (parsed.params.length > 0) {
                    combinedDoc.push('### Parameters');
                    combinedDoc.push('');
                    for (const param of parsed.params) {
                        const desc = this.cleanDocumentation(param.description);
                        combinedDoc.push(`- \`${param.name}\` - ${desc}`);
                    }
                    combinedDoc.push('');
                }

                // Returns
                if (parsed.returns.length > 0) {
                    combinedDoc.push('### Returns');
                    combinedDoc.push('');
                    const expanded = parsed.returns.flatMap(r => this.splitReturnsIntoOutcomes(r));
                    for (const ret of expanded) {
                        combinedDoc.push(`- ${this.formatReturnsText(ret)}`);
                    }
                    combinedDoc.push('');
                }

                return combinedDoc.join('\n');
            }
        }
        return null;
    }
}

// CLI Interface
function main() {
    const args = process.argv.slice(2);
    const verbose = args.includes('-v') || args.includes('--verbose');

    // Get source directory
    const srcDir = process.env.SRC_DIR || path.join(process.cwd(), 'src');
    const outputDir = process.env.OUTPUT_DIR || path.join(process.cwd(), 'docs/literate-erlang');

    // Ensure output directory exists
    if (!fs.existsSync(outputDir)) {
        fs.mkdirSync(outputDir, { recursive: true });
    }

    // Process all .erl files
    const files = fs.readdirSync(srcDir).filter(f => f.endsWith('.erl'));
    const parser = new ErlangLiterateParser({ verbose });

    console.log(`Processing ${files.length} Erlang files...`);

    for (const file of files) {
        if (verbose) console.log(`  Processing ${file}...`);

        try {
            const inputPath = path.join(srcDir, file);
            const outputPath = path.join(outputDir, file + '.md');

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