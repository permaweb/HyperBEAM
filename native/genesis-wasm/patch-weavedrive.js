#!/usr/bin/env node

const fs = require('node:fs')
const path = require('node:path')

const serverDir = process.argv[2] || process.cwd()
const weavedrivePath = path.join(
  serverDir,
  'node_modules',
  '@permaweb',
  'weavedrive',
  'dist',
  'index.cjs'
)

const createBefore = `const response = await this.customFetch(\`/\${id}\`, { method: "HEAD" });
      if (!response.ok) {
        return "HALT";
      }
      const bytesLength = response.headers.get("Content-Length");
      node.total_size = Number(bytesLength);`

const createRawFirst = `let dataPath = \`/raw/\${id}\`;
      let response = await this.customFetch(dataPath, { method: "HEAD" });
      let bytesLength = response.ok ? response.headers.get("Content-Length") : null;
      if (!response.ok || bytesLength === null || Number.isNaN(Number(bytesLength))) {
        dataPath = \`/\${id}\`;
        response = await this.customFetch(dataPath, { method: "HEAD" });
        bytesLength = response.ok ? response.headers.get("Content-Length") : null;
      }
      if (!response.ok || bytesLength === null || Number.isNaN(Number(bytesLength))) {
        return "HALT";
      }
      node.gatewayPath = dataPath;
      node.total_size = Number(bytesLength);`

const createAfter = `let dataPath = \`/\${id}\`;
      let response = await this.customFetch(dataPath, { method: "HEAD" });
      let bytesLength = response.ok ? response.headers.get("Content-Length") : null;
      if (!response.ok || bytesLength === null || Number.isNaN(Number(bytesLength))) {
        dataPath = \`/raw/\${id}\`;
        response = await this.customFetch(dataPath, { method: "HEAD" });
        bytesLength = response.ok ? response.headers.get("Content-Length") : null;
      }
      if (!response.ok || bytesLength === null || Number.isNaN(Number(bytesLength))) {
        return "HALT";
      }
      node.gatewayPath = dataPath;
      node.total_size = Number(bytesLength);`

const readBefore = 'const response = await this.customFetch(`/${stream.node.name}`, {'
const readAfter = 'const response = await this.customFetch(stream.node.gatewayPath || `/${stream.node.name}`, {'

let source = fs.readFileSync(weavedrivePath, 'utf8')

if (!source.includes(createAfter)) {
  if (source.includes(createRawFirst)) {
    source = source.replace(createRawFirst, createAfter)
  } else if (source.includes(createBefore)) {
    source = source.replace(createBefore, createAfter)
  } else {
    throw new Error(`Could not find WeaveDrive create() block in ${weavedrivePath}`)
  }
}

if (!source.includes(readAfter)) {
  if (!source.includes(readBefore)) {
    throw new Error(`Could not find WeaveDrive read() block in ${weavedrivePath}`)
  }
  source = source.replace(readBefore, readAfter)
}

fs.writeFileSync(weavedrivePath, source)
console.log(`Patched WeaveDrive gateway reads in ${weavedrivePath}`)
