#!/usr/bin/env node

import { createHash } from "node:crypto";
import { readFile, writeFile } from "node:fs/promises";
import { createRequire } from "node:module";

const require = createRequire(import.meta.url);
const deps = "@zondax/ledger-arweave @ledgerhq/hw-transport-node-hid";

main().catch((err) => {
  console.error(err.message);
  process.exit(1);
});

async function main() {
  const [argIn, argOut] = process.argv.slice(2);

  if (process.argv.includes("--help") || process.argv.includes("-h")) {
    return usage(0);
  }

  const input = argIn || process.env.IN;
  const output = argOut || process.env.OUT;

  if (!input || !output) usage(1);

  const Transport = load("@ledgerhq/hw-transport-node-hid");
  const ArweaveApp = load("@zondax/ledger-arweave");
  const unsigned = JSON.parse(await readFile(input, "utf8"));
  const transport = await Transport.open(process.env.LEDGER_HID_PATH || "");

  try {
    const app = new ArweaveApp(transport);
    const address = await app.getAddress();
    ok(address, "get address");

    const tx = ledgerTx(unsigned, address.owner);
    const signed = await quiet(() => app.sign(tx));
    ok(signed, "sign transaction");

    const signature = encode64(signed.signature);
    const id = encode64(createHash("sha256").update(decode64(signature)).digest());
    await writeFile(
      output,
      `${JSON.stringify({ ...unsigned, id, owner: address.owner, signature })}\n`
    );
    console.error(`Signed ${id} with ${address.address}`);
  } finally {
    await transport.close();
  }
}

function load(name) {
  try {
    const mod = require(name);
    return mod.default || mod;
  } catch (err) {
    if (err.code === "MODULE_NOT_FOUND") {
      throw new Error(`Missing ${name}. Run: npm install --no-save ${deps}`);
    }

    throw err;
  }
}

function ledgerTx(json, owner) {
  const tx = {
    ...json,
    owner,
    format: Number(json.format || 2),
    quantity: String(json.quantity || "0"),
    reward: String(json.reward || "0"),
    data_size: String(json.data_size || "0"),
    tags: (json.tags || []).map(ledgerTag),
  };

  tx.get = (field, opts = {}) =>
    opts.decode ? decode64(tx[field] || "") : tx[field] || "";

  return tx;
}

function ledgerTag(tag) {
  return {
    ...tag,
    get(field, opts = {}) {
      return opts.decode ? decode64(this[field] || "") : this[field] || "";
    },
  };
}

function ok(res, action) {
  if (res.returnCode !== 0x9000) {
    throw new Error(
      `Ledger ${action} failed: ${res.errorMessage} ` +
        `(0x${res.returnCode.toString(16)})`
    );
  }
}

async function quiet(fn) {
  if (process.env.LEDGER_ARWEAVE_VERBOSE) return await fn();

  const log = console.log;
  console.log = () => {};

  try {
    return await fn();
  } finally {
    console.log = log;
  }
}

function decode64(value) {
  const base64 = String(value).replace(/-/g, "+").replace(/_/g, "/");
  const padded = base64.padEnd(
    base64.length + ((4 - base64.length % 4) % 4),
    "="
  );
  return Buffer.from(padded, "base64");
}

function encode64(value) {
  return Buffer.from(value)
    .toString("base64")
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/g, "");
}

function usage(code) {
  console.error(`Usage: ledger-sign-arweave.mjs [unsigned-tx.json] [signed-tx.json]

Forge also passes paths as IN and OUT:
  rebar3 device publish --publish-codec tx@1.0 \\
    --external-signer 'node ./scripts/ledger-sign-arweave.mjs "$IN" "$OUT"'

Requires the Ledger Arweave app and:
  npm install --no-save ${deps}`);
  process.exit(code);
}
