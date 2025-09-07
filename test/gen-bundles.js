const fs = require("fs");
const path = require("path");
const crypto = require("crypto");
const base64url = require("base64url");

const arb = require("@dha-team/arbundles");
const { ArweaveSigner, createData, Bundle, DataItem, bundleAndSignData, longTo32ByteArray } = arb;
const { createHash } = require("crypto");

// Load a local wallet
const wallet = require("/Users/piechota/dev/arweave/repos/ao/wallet.json");

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

async function generateListBundle(signer) {
  const listItems = [
    createData("first", signer, { target: base64url.encode(Buffer.alloc(32)), anchor: Buffer.alloc(32), tags: [{ name: "Type", value: "list" }, { name: "Index", value: "0" }] }),
    createData("second", signer, { anchor: Buffer.alloc(32), tags: [{ name: "Type", value: "list" }, { name: "Index", value: "1" }] }),
    createData("third", signer, { tags: [{ name: "Type", value: "list" }, { name: "Index", value: "2" }] }),
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

async function generateMapBundle(signer) {
  const mapItem1 = createData("map-item-1", signer);
  const mapItem2 = createData("map-item-2", signer);

  await mapItem1.sign(signer);
  await mapItem2.sign(signer);

  const unsignedId1 = await getUnsignedId(mapItem1);
  const unsignedId2 = await getUnsignedId(mapItem2);

  const manifestData = {
      "key1": base64url.encode(unsignedId1),
      "key2": base64url.encode(unsignedId2)
  };

  const manifestTags = [
      { name: "data-protocol", value: "bundle-map" },
      { name: "variant", value: "0.0.1" },
  ];
  const manifestItem = createData(JSON.stringify(manifestData), signer, { tags: manifestTags });

  const manifestUnsignedId = await getUnsignedId(manifestItem);

  console.log("\nBuilding map bundle (inner):");
  console.log(`  - manifest: id=${base64url.encode(manifestUnsignedId)}, size=${manifestItem.getRaw().byteLength}`);
  console.log(`  - key1: id=${base64url.encode(unsignedId1)}, size=${mapItem1.getRaw().byteLength}`);
  console.log(`  - key2: id=${base64url.encode(unsignedId2)}, size=${mapItem2.getRaw().byteLength}`);

  const mapBundle = await bundleAndSignData([manifestItem, mapItem1, mapItem2], signer);

  const bundleTags = [
      { name: "bundle-format", value: "binary" },
      { name: "bundle-version", value: "2.0.0" },
      { name: "bundle-map", value: base64url.encode(manifestUnsignedId) },
  ];
  const outerDataItem = createData(mapBundle.getRaw(), signer, { tags: bundleTags });
  await outerDataItem.sign(signer);

  writeOut("ans104-map-bundle.bundle", outerDataItem.getRaw());
}

(async () => {
  const signer = new ArweaveSigner(wallet);

  await generateListBundle(signer);
  await generateSingleListBundle(signer);
  await generateMapBundle(signer);
})();


