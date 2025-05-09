#!/usr/bin/env node
import { program } from 'commander';
import { connect, createSigner } from '@permaweb/aoconnect';
import fs from 'fs';
import path from 'path';
import os from 'os';
console.log('viksit beam')
program
  .name('beam')
  .description('Hyperbeam CLI')
  .version('1.1.0')
  .usage('[options] <url>')
  .option('-X, --request <method>', 'HTTP method', 'GET')
  .option('-H, --header <header>', 'Custom header', [], (v, p) => [...p, v])
  .option('-d, --data <data>', 'Inline body data')
  .option('-f, --file <file>', 'Body data from file')
  .option('-o, --output <file>', 'Write response to file')
  .option('-v, --verbose', 'Verbose output')
  .option('--raw', 'Print raw response');
program.argument('<url>', 'Target URL').parse();
const opts = program.opts();
const target = program.processedArgs[0];
if (!target) { console.error('URL required'); process.exit(1); }

const WALLET = JSON.parse(fs.readFileSync(path.join('./wallet.json'), 'utf-8'));
// console.log('wallet: ', WALLET);
const { request } = connect({
  MODE: 'mainnet',
  device: 'process@1.0',
  signer: createSigner(WALLET),
  URL: new URL(target).origin
});

const headers = {};
for (const h of opts.header) {
  const [k, ...v] = h.split(':');
  headers[k.trim()] = v.join(':').trim();
}
let body = opts.data ?? null;
if (opts.file) body = fs.readFileSync(path.resolve(opts.file), 'utf-8');

const params = {
  path: new URL(target).pathname + new URL(target).search,
  method: opts.request,
  headers
};
if (body !== null) {
  params.body = body;
  if (!params.headers['content-type']) params.headers['content-type'] = 'application/json';
}

(async () => {
  try {
    if (opts.verbose) console.log('> params', params);
    const res = await request(params);
    if (opts.output) {
      fs.writeFileSync(
        path.resolve(opts.output),
        opts.raw ? res : JSON.stringify(res, null, 2)
      );
    } else {
      console.log(opts.raw ? res : JSON.stringify(res.body, null, 2));
    }
  } catch (e) {
    console.error('Error:', e.message);
    process.exit(1);
  }
})();
