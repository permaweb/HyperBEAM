const fs = require("fs");
const path = require("path");
const { DataItem } = require("@dha-team/arbundles");
const { createHash } = require("crypto");
const base64url = require("base64url");

/**
 * Verify a serialized ANS-104 data item created by HyperBEAM
 * 
 * This script loads a binary data item file created by the Erlang test
 * and verifies its signature and structure using arbundles.js
 * 
 * Usage: node verify-hyperbeam-item.js [path-to-item.bin]
 *        If no path is provided, defaults to hyperbeam-test-item.bin
 */

async function verifyHyperBeamDataItem(itemPath) {
    // If no path provided, use default
    if (!itemPath) {
        itemPath = path.join(__dirname, "hyperbeam-test-item.bin");
    } else if (!path.isAbsolute(itemPath)) {
        // If relative path, resolve it from current working directory
        itemPath = path.resolve(process.cwd(), itemPath);
    }
    
    console.log("Loading HyperBEAM data item...");
    console.log(`Path: ${itemPath}\n`);
    
    // Check if file exists
    if (!fs.existsSync(itemPath)) {
        console.error(`Error: File not found: ${itemPath}`);
        console.error("\nUsage: node verify-hyperbeam-item.js [path-to-item.bin]");
        console.error("Run the Erlang test first: rebar3 eunit --module=dev_arweave --test=serialize_data_item");
        process.exit(1);
    }
    
    // Read the serialized data item
    const itemBuffer = fs.readFileSync(itemPath);
    console.log(`File size: ${itemBuffer.length} bytes`);
    
    try {
        // Deserialize the data item
        const dataItem = new DataItem(itemBuffer);
        
        console.log("\n=== Data Item Information ===");
        console.log(`ID: ${base64url.encode(dataItem.id)}`);
        console.log(`Owner: ${base64url.encode(dataItem.owner).substring(0, 20)}...`);
        console.log(`Target: ${dataItem.target ? base64url.encode(dataItem.target) : "none"}`);
        console.log(`Anchor: ${dataItem.anchor ? base64url.encode(dataItem.anchor) : "none"}`);
        console.log(`Signature type: ${dataItem.signatureType}`);
        console.log(`Data size: ${dataItem.data.length} bytes`);
        
        // Display tags
        console.log(`\n=== Tags (${dataItem.tags.length}) ===`);
        dataItem.tags.forEach((tag, index) => {
            const name = Buffer.from(tag.name).toString('utf-8');
            const value = Buffer.from(tag.value).toString('utf-8');
            console.log(`  ${index + 1}. ${name}: ${value}`);
        });
        
        // Display data content
        console.log(`\n=== Data Content ===`);
        
        // Try to decode from base64 first, then to UTF-8
        try {
            const decodedData = Buffer.from(dataItem.data.toString('utf-8'), 'base64');
            const decodedStr = decodedData.toString('utf-8');
            console.log(decodedStr);
        } catch (error) {
            // If not base64, just show as UTF-8
            const dataStr = Buffer.from(dataItem.data).toString('utf-8');
            console.log(dataStr);
        }
        
        // Verify the signature
        console.log(`\n=== Verification ===`);
        const isValid = await dataItem.isValid();
        
        if (isValid) {
            console.log("✅ Signature is VALID");
            
            // Calculate and display the unsigned ID
            const unsignedId = await getUnsignedId(dataItem);
            console.log(`\nUnsigned ID: ${base64url.encode(unsignedId)}`);
            
            return true;
        } else {
            console.log("❌ Signature is INVALID");
            return false;
        }
        
    } catch (error) {
        console.error(`\n❌ Error verifying data item: ${error.message}`);
        console.error(error.stack);
        return false;
    }
}

// Helper function to check if string is printable
function isPrintable(str) {
    return /^[\x20-\x7E\s]*$/.test(str);
}

// Calculate unsigned ID of a data item
async function getUnsignedId(item) {
    const itemCopy = new DataItem(Buffer.from(item.getRaw()));
    const blankOwner = Buffer.alloc(itemCopy.ownerLength);
    itemCopy.rawOwner = blankOwner;
    const signatureData = await itemCopy.getSignatureData();
    return createHash("sha256").update(signatureData).digest();
}

// Run the verification
if (require.main === module) {
    // Get file path from command line arguments
    const itemPath = process.argv[2];
    
    verifyHyperBeamDataItem(itemPath)
        .then(success => {
            process.exit(success ? 0 : 1);
        })
        .catch(error => {
            console.error("Fatal error:", error);
            process.exit(1);
        });
}

module.exports = { verifyHyperBeamDataItem };

