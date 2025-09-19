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
            parenDepth: 0
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
                if (docLine) moduleDoc.push(docLine);
            } else if (inModuleDoc && trimmed === '') {
                // Empty line in module doc, continue
                continue;
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
    }

    processFunctionBody(lines) {
        const segments = [];
        let currentCode = [];
        let inCodeBlock = false;

        for (const line of lines) {
            const trimmed = line.trim();

            // Check for inline comment (% or %% but not %%%)
            if (trimmed.match(/^\s*%[^%]/) || trimmed.match(/^\s*%%[^%]/)) {
                // Save any accumulated code
                if (currentCode.length > 0) {
                    segments.push({
                        type: 'code',
                        content: currentCode.join('\n')
                    });
                    currentCode = [];
                }

                // Extract comment text (remove % or %% prefix)
                let commentText;
                if (trimmed.match(/^\s*%%[^%]/)) {
                    commentText = line.replace(/^\s*%%\s?/, '');
                } else {
                    commentText = line.replace(/^\s*%\s?/, '');
                }
                segments.push({
                    type: 'comment',
                    content: this.cleanInlineComment(commentText)
                });
            } else {
                // Regular code line
                currentCode.push(line);
            }
        }

        // Add any remaining code
        if (currentCode.length > 0) {
            segments.push({
                type: 'code',
                content: currentCode.join('\n')
            });
        }

        return segments;
    }

    cleanInlineComment(text) {
        // Convert `thing' to `thing`
        return text.replace(/`([^']*?)'/g, '`$1`').trim();
    }

    cleanDocumentation(text) {
        if (!text) return '';

        // Convert Erlang doc syntax to Markdown
        return text
            .replace(/`([^']*?)'/g, '`$1`')  // Convert `code' to `code`
            .replace(/&lt;&lt;/g, '<<')       // Fix HTML entities
            .replace(/&gt;&gt;/g, '>>')
            .replace(/@doc\s*/g, '')          // Remove @doc tags
            .trim();
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
                currentSection = 'returns';
                continue;
            }

            // Add to current section
            if (currentSection === 'description' && trimmed) {
                result.description.push(trimmed);
            } else if (currentSection === 'param' && currentParam && trimmed) {
                currentParam.description += ' ' + trimmed;
            } else if (currentSection === 'returns' && trimmed) {
                result.returns.push(trimmed);
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

        // Functions
        for (const func of this.functions) {
            md.push(`## ${func.name}`);
            md.push('');

            // Parse and format documentation
            if (func.doc) {
                const parsed = this.parseDocumentation(func.doc);

                // Description
                if (parsed.description.length > 0) {
                    md.push(this.cleanDocumentation(parsed.description.join(' ')));
                    md.push('');
                }

                // Parameters
                if (parsed.params.length > 0) {
                    md.push('### Parameters');
                    md.push('');
                    for (const param of parsed.params) {
                        const desc = this.cleanDocumentation(param.description);
                        md.push(`- \`${param.name}\` - ${desc}`);
                    }
                    md.push('');
                }

                // Returns
                if (parsed.returns.length > 0) {
                    md.push('### Returns');
                    md.push('');
                    for (const ret of parsed.returns) {
                        md.push(`- ${this.cleanDocumentation(ret)}`);
                    }
                    md.push('');
                }
            }

            // Spec
            if (func.spec) {
                md.push('```erlang');
                md.push(func.spec.trim());
                md.push('```');
                md.push('');
            }

            // Function body with inline comments
            if (func.body && func.body.length > 0) {
                // md.push('### Function');
                md.push('');

                for (const segment of func.body) {
                    if (segment.type === 'comment') {
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

            md.push('');
        }

        // Footer
        md.push('---');
        md.push('');
        md.push(`*Generated from [${fileName}](${githubUrl})*`);

        return md.join('\n');
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