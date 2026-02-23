const fs = require("fs");
const path = require("path");
const crypto = require("crypto");
const base64url = require("base64url");

const arb = require("@dha-team/arbundles");
const { ArweaveSigner, createData, DataItem, bundleAndSignData } = arb;
const { createHash } = require("crypto");

// Load a local wallet
const wallet = require("./wallet.json");

function writeOut(name, buf) {
  const out = path.join(__dirname, name);
  fs.writeFileSync(out, buf);
  console.log(`${name}: ${buf.length} bytes written`);
}

async function getUnsignedId(item) {
    const originalOwner = Buffer.from(item.rawOwner);
    const blankOwner = Buffer.alloc(item.ownerLength);
    item.rawOwner = blankOwner;

    const signatureData = await item.getSignatureData();
    const unsignedId = createHash("sha256").update(signatureData).digest();

    item.rawOwner = originalOwner;
    return unsignedId;
}

async function generateSingleItemFile(signer) {
  const item = createData("hello world", signer, {
    target: "eJmUI4azsmhRCZRf3MaX0CFDHwWn9oStIirZma3ql68",
    tags: [
        { name: "Content-Type", value: "text/plain" },
        { name: "App-Name", value: "arbundles-gen" }
    ]
  });
  await item.sign(signer);
  writeOut("ans104-item.bundle", item.getRaw());
}

class NeverSignedDataItem extends DataItem {
    isSigned() {
        return true;
    }
}

async function generateListBundle(signer) {
  const listItems = [
    createData("first", signer, { target: "Tu6LHQdEVK7lNF3AOAHrVBjl2CFvQizd5VaWBvdFRSs", anchor: base64url.toBuffer("N1k7gUBck6EBgmApl58Nxxhe3TTATSHeEyyXhdFVe9A"), tags: [{ name: "Type", value: "list" }, { name: "Index", value: "0" }] }),
    createData("second", signer, { anchor: base64url.toBuffer("fgAVH_xJJU1tkzWSmSfBfb_KBX8sa_FQ2b7YWuE08Ko"), tags: [{ name: "Type", value: "list" }, { name: "Index", value: "1" }] }),
    createData("third", signer, { tags: [{ name: "Type", value: "list" }, { name: "Index", "value": "2" }] }),
  ];
  const signedListItems = await Promise.all(listItems.map(async (i) => { await i.sign(signer); return i; }));

  console.log("\nBuilding list bundle:");
  for (const item of signedListItems) {
      console.log(`  - item: id=${item.id}, size=${item.getRaw().byteLength}`);
  }

  const listBundle = await bundleAndSignData(signedListItems, signer);
  writeOut("ans104-list-bundle.bundle", listBundle.getRaw());
}

async function generateSingleListBundle(signer) {
  const listItem = [
    createData("only", signer, { tags: [{ name: "Type", value: "list" }, { name: "Index", value: "1" }] }),
  ];
  const signedListItem = await Promise.all(listItem.map(async (i) => { await i.sign(signer); return i; }));

  console.log("\nBuilding single list bundle:");
  for (const item of signedListItem) {
      console.log(`  - item: id=${item.id}, size=${item.getRaw().byteLength}`);
  }

  const singleListBundle = await bundleAndSignData(signedListItem, signer);
  writeOut("ans104-single-list-bundle.bundle", singleListBundle.getRaw());
}

(async () => {
  const signer = new ArweaveSigner(wallet);

  await generateSingleItemFile(signer);
  await generateListBundle(signer);
  await generateSingleListBundle(signer);
})();


