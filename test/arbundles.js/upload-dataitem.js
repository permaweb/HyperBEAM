const fs = require("fs");
const path = require("path");
const axios = require("axios");
const { DataItem } = require("@dha-team/arbundles");
const base64url = require("base64url");

/**
 * Upload a serialized ANS-104 data item to up.arweave.net
 * 
 * Usage: node upload-dataitem.js <path-to-item.bin> [gateway-url]
 *        Default gateway: https://up.arweave.net
 */

async function uploadDataItem(itemPath, gatewayUrl = "https://up.arweave.net") {
    console.log("Loading data item...");
    console.log(`Path: ${itemPath}`);
    console.log(`Gateway: ${gatewayUrl}\n`);
    
    // Check if file exists
    if (!fs.existsSync(itemPath)) {
        console.error(`Error: File not found: ${itemPath}`);
        console.error("\nUsage: node upload-dataitem.js <path-to-item.bin> [gateway-url]");
        process.exit(1);
    }
    
    // Read the serialized data item
    const itemBuffer = fs.readFileSync(itemPath);
    console.log(`File size: ${itemBuffer.length} bytes`);
    
    try {
        // Parse the data item to get its ID and verify it
        const dataItem = new DataItem(itemBuffer);
        const itemId = base64url.encode(dataItem.id);
        
        console.log(`Data item ID: ${itemId}`);
        
        // Verify the signature before uploading
        const isValid = await dataItem.isValid();
        if (!isValid) {
            console.error("❌ Error: Data item signature is invalid!");
            console.error("Cannot upload an invalid data item.");
            process.exit(1);
        }
        console.log("✅ Signature verified\n");
        
        // Upload to the gateway
        console.log("Uploading to gateway...");
        const uploadUrl = `${gatewayUrl}/~bundler@1.0/tx`;
        
        const response = await axios.post(uploadUrl, itemBuffer, {
            headers: {
                'Content-Type': 'application/octet-stream',
            },
            maxBodyLength: Infinity,
            maxContentLength: Infinity,
            timeout: 60000, // 60 second timeout
        });
        
        console.log(`\n✅ Upload successful!`);
        console.log(`Status: ${response.status} ${response.statusText}`);
        
        if (response.data) {
            console.log(`Response:`, response.data);
        }
        
        // Use the ID from the response if available, otherwise use calculated ID
        const uploadedId = response.data?.id || itemId;
        
        // Provide the URL where the data item can be accessed
        console.log(`\n=== Access URLs ===`);
        console.log(`Transaction ID: ${uploadedId}`);
        console.log(`View on Arweave: https://arweave.net/${uploadedId}`);
        console.log(`View on ViewBlock: https://viewblock.io/arweave/tx/${uploadedId}`);
        console.log(`Direct data: https://arweave.net/${uploadedId}`);
        
        return {
            success: true,
            id: uploadedId,
            status: response.status,
            data: response.data
        };
        
    } catch (error) {
        console.error(`\n❌ Upload failed!`);
        
        if (error.response) {
            console.error(`Status: ${error.response.status} ${error.response.statusText}`);
            console.error(`Response:`, error.response.data);
        } else if (error.request) {
            console.error(`No response received from server`);
            console.error(`Error: ${error.message}`);
        } else {
            console.error(`Error: ${error.message}`);
        }
        
        if (error.code === 'ECONNABORTED') {
            console.error(`\nThe upload timed out. Try again or use a different gateway.`);
        }
        
        return {
            success: false,
            error: error.message
        };
    }
}

// Run the upload
if (require.main === module) {
    const itemPath = process.argv[2];
    const gatewayUrl = process.argv[3];
    
    if (!itemPath) {
        console.error("Usage: node upload-dataitem.js <path-to-item.bin> [gateway-url]");
        console.error("\nExample:");
        console.error("  node upload-dataitem.js hyperbeam-test-item.bin");
        console.error("  node upload-dataitem.js hyperbeam-test-item.bin https://up.arweave.net");
        process.exit(1);
    }
    
    // Resolve path
    const resolvedPath = path.isAbsolute(itemPath) 
        ? itemPath 
        : path.resolve(process.cwd(), itemPath);
    
    uploadDataItem(resolvedPath, gatewayUrl)
        .then(result => {
            process.exit(result.success ? 0 : 1);
        })
        .catch(error => {
            console.error("Fatal error:", error);
            process.exit(1);
        });
}

module.exports = { uploadDataItem };

