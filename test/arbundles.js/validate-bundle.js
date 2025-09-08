const fs = require("fs");
const path = require("path");
const arb = require("@dha-team/arbundles");
const { DataItem, Bundle } = arb;
const { createHash } = require("crypto");
const base64url = require("base64url");

// Helper to calculate unsigned ID, adapted from gen-bundles.js
async function getUnsignedId(item) {
    // This function can mutate the item, so we work on a copy
    const itemCopy = new DataItem(Buffer.from(item.getRaw()));
    const blankOwner = Buffer.alloc(itemCopy.ownerLength);
    itemCopy.rawOwner = blankOwner;

    const signatureData = await itemCopy.getSignatureData();
    return createHash("sha256").update(signatureData).digest();
}

(async () => {
    const filePath = process.argv[2];
    if (!filePath) {
        console.error("Usage: node validate-bundle.js <path_to_bundle_file>");
        process.exit(1);
    }
    const bundlePath = path.resolve(filePath);

    if (!fs.existsSync(bundlePath)) {
        console.error(`Bundle file not found at ${bundlePath}.`);
        return;
    }

    const outerItemBinary = fs.readFileSync(bundlePath);
    const outerItem = new DataItem(outerItemBinary);

    console.log("Validating outer data item...");
    const isOuterValid = await outerItem.isValid();
    console.log(`- Is valid: ${isOuterValid}`);

    if (!isOuterValid) {
        console.error("Outer data item is invalid.");
        return;
    }

    const innerBundle = new Bundle(outerItem.rawData);
    const items = innerBundle.items;

    console.log(`\nFound ${items.length} items in the inner bundle.`);

    const bundleMapTag = outerItem.tags.find(tag => tag.name === 'bundle-map');
    if (!bundleMapTag) {
        console.error("bundle-map tag not found on outer item.");
        return;
    }
    const manifestId = bundleMapTag.value;
    console.log(`\nExpected manifest ID: ${manifestId}`);

    let manifest = null;
    console.log("IDs of items in bundle (unsigned):");
    for (const item of items) {
        const unsignedId = await getUnsignedId(item);
        const unsignedIdB64 = base64url.encode(unsignedId);
        console.log(`- ${unsignedIdB64}`);
        if (unsignedIdB64 === manifestId) {
            manifest = item;
        }
    }

    if (!manifest) {
        console.error("Manifest item not found in the bundle (ID mismatch).");
        return;
    }

    console.log("\nValidating manifest item...");
    const hasSignature = manifest.rawSignature.some(byte => byte !== 0);
    const hasOwner = manifest.rawOwner.some(byte => byte !== 0);

    console.log(`- Has signature: ${hasSignature}`);
    console.log(`- Has owner: ${hasOwner}`);

    if (hasSignature || hasOwner) {
        console.error("Validation failed: Manifest should be unsigned and have no owner.");
    } else {
        console.log("Manifest validation passed (is unsigned with no owner).");
    }
    
    const manifestData = JSON.parse(manifest.rawData.toString());
    const idToKeyMap = new Map();
    for (const [key, id] of Object.entries(manifestData)) {
        idToKeyMap.set(id, key);
    }

    const dataItems = items.filter(i => i !== manifest);

    if (dataItems.length > 0) {
        console.log("\nValidating data items...");
        for (const item of dataItems) {
            const unsignedId = await getUnsignedId(item);
            const unsignedIdB64 = base64url.encode(unsignedId);
            const key = idToKeyMap.get(unsignedIdB64);
            
            console.log(`\n- Key: ${key}`);
            console.log(`  - ID (signed): ${item.id}`);
            console.log(`  - ID (unsigned): ${unsignedIdB64}`);
            console.log(`  - Data: ${item.rawData.toString()}`);
            console.log(`  - Tags: ${JSON.stringify(item.tags)}`);

            const isValid = await item.isValid();
            console.log(`  - Is valid: ${isValid}`);
            if (!isValid) {
                console.error(`Validation failed: Data item with key '${key}' is invalid.`);
            }
        }
    }

})();
