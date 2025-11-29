const fs = require("fs");
const path = require("path");
const { DataItem, Bundle } = require("@dha-team/arbundles");
const { createHash } = require("crypto");
const base64url = require("base64url");
const axios = require("axios");

// ============================================================================
// Helper Functions
// ============================================================================

// Calculate unsigned ID of a data item
async function getUnsignedId(item) {
    const itemCopy = new DataItem(Buffer.from(item.getRaw()));
    const blankOwner = Buffer.alloc(itemCopy.ownerLength);
    itemCopy.rawOwner = blankOwner;
    const signatureData = await itemCopy.getSignatureData();
    return createHash("sha256").update(signatureData).digest();
}

// Check if input looks like a transaction ID
function isTxId(input) {
    return /^[A-Za-z0-9_-]{43}$/.test(input) && !input.includes('/') && !input.includes('.');
}

// Fetch transaction data from Arweave
async function fetchTransactionData(txId, gateway = "https://arweave.net") {
    console.log(`Fetching transaction ${txId} from ${gateway}...`);
    
    try {
        const headerResponse = await axios.get(`${gateway}/tx/${txId}`, {
            timeout: 30000
        });
        console.log(`- Transaction header fetched`);
        console.log(`- Owner: ${headerResponse.data.owner.substring(0, 20)}...`);
        console.log(`- Tags: ${headerResponse.data.tags.length} tags`);
        
        console.log(`Fetching transaction data...`);
        const dataResponse = await axios.get(`${gateway}/${txId}`, {
            responseType: 'arraybuffer',
            timeout: 60000
        });
        console.log(`- Data fetched: ${dataResponse.data.byteLength} bytes\n`);
        
        return {
            header: headerResponse.data,
            data: Buffer.from(dataResponse.data)
        };
    } catch (error) {
        if (error.response) {
            throw new Error(`Failed to fetch transaction: ${error.response.status} ${error.response.statusText}`);
        } else if (error.code === 'ECONNABORTED') {
            throw new Error(`Request timeout while fetching transaction`);
        } else {
            throw new Error(`Network error: ${error.message}`);
        }
    }
}

// ============================================================================
// Bundle Loading Functions
// ============================================================================

// Load bundle from Arweave transaction
async function loadBundleFromTransaction(txId) {
    const tx = await fetchTransactionData(txId);
    
    console.log("Transaction header:");
    console.log(`- ID: ${txId}`);
    console.log(`- Data Size: ${tx.header.data_size} bytes`);
    console.log(`- Tags:`);
    for (const tag of tx.header.tags) {
        const name = Buffer.from(tag.name, 'base64').toString();
        const value = Buffer.from(tag.value, 'base64').toString();
        console.log(`    ${name}: ${value}`);
    }
    console.log("");

    const bundle = new Bundle(tx.data);
    console.log(`Found ${bundle.items.length} items in the bundle.`);

    // Look for bundle-map in transaction tags
    const bundleMapTag = tx.header.tags.find(tag => 
        Buffer.from(tag.name, 'base64').toString() === 'Bundle-Map' ||
        Buffer.from(tag.name, 'base64').toString() === 'bundle-map'
    );
    
    const manifestId = bundleMapTag 
        ? Buffer.from(bundleMapTag.value, 'base64').toString()
        : null;
    
    if (manifestId) {
        console.log(`Expected manifest ID from tx tags: ${manifestId}`);
    } else {
        console.log("Note: No bundle-map tag found in transaction header.");
    }

    return { bundle, manifestId };
}

// Load bundle from file
async function loadBundleFromFile(filePath) {
    const bundlePath = path.resolve(filePath);
    if (!fs.existsSync(bundlePath)) {
        throw new Error(`Bundle file not found at ${bundlePath}.`);
    }
    
    const outerItemBinary = fs.readFileSync(bundlePath);
    console.log(`Reading bundle from file: ${bundlePath}`);
    console.log(`File size: ${outerItemBinary.length} bytes\n`);

    const outerItem = new DataItem(outerItemBinary);
    console.log("Validating outer data item...");
    const isOuterValid = await outerItem.isValid();
    console.log(`- Is valid: ${isOuterValid}`);

    if (!isOuterValid) {
        throw new Error("Outer data item is invalid.");
    }

    const bundle = new Bundle(outerItem.rawData);
    console.log(`\nFound ${bundle.items.length} items in the inner bundle.`);

    // Look for bundle-map in data item tags
    const bundleMapTag = outerItem.tags.find(tag => 
        tag.name === 'bundle-map' || tag.name === 'Bundle-Map'
    );
    
    const manifestId = bundleMapTag ? bundleMapTag.value : null;
    
    if (manifestId) {
        console.log(`Expected manifest ID: ${manifestId}`);
    } else {
        console.log("Note: No bundle-map tag found on outer data item.");
    }

    return { bundle, manifestId };
}

// ============================================================================
// Validation Functions
// ============================================================================

// Verify bundle cryptographically
async function verifyBundle(bundle) {
    console.log("\nVerifying bundle...");
    const isValid = await bundle.verify();
    console.log(`- Bundle verification: ${isValid ? '✓ PASSED' : '✗ FAILED'}`);
    
    if (!isValid) {
        throw new Error("Bundle verification failed!");
    }
    console.log("");
}

// Find and validate manifest
async function validateManifest(items, manifestId) {
    if (!manifestId) {
        console.log("\nNo manifest ID specified, skipping manifest validation.");
        return { manifest: null, idToKeyMap: new Map() };
    }

    let manifest = null;
    const idToKeyMap = new Map();
    
    console.log("\nIDs of items in bundle (unsigned):");
    for (const item of items) {
        const unsignedId = await getUnsignedId(item);
        const unsignedIdB64 = base64url.encode(unsignedId);
        console.log(`- ${unsignedIdB64}`);
        if (unsignedIdB64 === manifestId) {
            manifest = item;
        }
    }

    if (!manifest) {
        console.error("\nManifest item not found in the bundle (ID mismatch).");
        console.log("This might be okay if the bundle doesn't use a manifest structure.");
        return { manifest: null, idToKeyMap };
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
    
    try {
        const manifestData = JSON.parse(manifest.rawData.toString());
        for (const [key, id] of Object.entries(manifestData)) {
            idToKeyMap.set(id, key);
        }
    } catch (e) {
        console.error(`Failed to parse manifest data as JSON: ${e.message}`);
    }

    return { manifest, idToKeyMap };
}

// Validate individual data items
async function validateDataItems(items, manifest, idToKeyMap) {
    const dataItems = manifest ? items.filter(i => i !== manifest) : items;

    if (dataItems.length === 0) {
        return;
    }

    console.log("\nValidating data items...");
    for (let i = 0; i < dataItems.length; i++) {
        const item = dataItems[i];
        const unsignedId = await getUnsignedId(item);
        const unsignedIdB64 = base64url.encode(unsignedId);
        const key = idToKeyMap.get(unsignedIdB64);
        
        console.log(`\n- Item ${i + 1}${key ? ` (Key: ${key})` : ''}:`);
        console.log(`  - ID (signed): ${item.id}`);
        console.log(`  - ID (unsigned): ${unsignedIdB64}`);
        console.log(`  - Data: ${item.rawData.toString().substring(0, 100)}${item.rawData.length > 100 ? '...' : ''}`);
        console.log(`  - Tags: ${JSON.stringify(item.tags)}`);

        const isValid = await item.isValid();
        console.log(`  - Is valid: ${isValid}`);
        if (!isValid) {
            console.error(`  ✗ Validation failed: Data item is invalid.`);
        } else {
            console.log(`  ✓ Validation passed`);
        }
    }
}

// ============================================================================
// Main Function
// ============================================================================

async function main() {
    const input = process.argv[2];
    if (!input) {
        console.error("Usage: node validate-bundle.js <path_to_bundle_file|transaction_id>");
        console.error("");
        console.error("Examples:");
        console.error("  node validate-bundle.js ./bundle.bundle");
        console.error("  node validate-bundle.js xK7gVx6NqPqEMpVrqEqOyqMqQy0tH2rH2rH2rH2rH2r");
        process.exit(1);
    }

    // Load bundle from transaction or file
    const { bundle, manifestId } = isTxId(input)
        ? await loadBundleFromTransaction(input)
        : await loadBundleFromFile(input);

    // Verify bundle
    await verifyBundle(bundle);

    // Validate manifest
    const { manifest, idToKeyMap } = await validateManifest(bundle.items, manifestId);

    // Validate data items
    await validateDataItems(bundle.items, manifest, idToKeyMap);
}

// Run main function
main().catch(error => {
    console.error("\n❌ Error:", error.message);
    process.exit(1);
});
