const fs = require("fs");
const path = require("path");
const { ArweaveSigner, createData } = require("@dha-team/arbundles");

// Configuration
const BUNDLER_URL = "http://localhost:8734";
const WALLET_PATH = "wallet.json"; // TODO replace with a local wallet file
let wallet = require(WALLET_PATH);

async function uploadDataItems() {
  try {
    console.log("\n" + "=".repeat(60));
    console.log("Creating 3 signed data items...");
    console.log("=".repeat(60));
    
    const signer = new ArweaveSigner(wallet);
    
    // Create 3 data items with different content
    const dataItems = [
      createData("First data item", signer, {
        tags: [
          { name: "Content-Type", value: "text/plain" },
          { name: "Item-Number", value: "1" },
        ],
      }),
      createData("Second data item", signer, {
        tags: [
          { name: "Content-Type", value: "text/plain" },
          { name: "Item-Number", value: "2" },
        ],
      }),
      createData("Third data item", signer, {
        tags: [
          { name: "Content-Type", value: "text/plain" },
          { name: "Item-Number", value: "3" },
        ],
      }),
    ];

    // Sign all data items
    console.log("\nSigning data items...");
    for (let i = 0; i < dataItems.length; i++) {
      await dataItems[i].sign(signer);
      console.log(`  ✓ Item ${i + 1} signed`);
      console.log(`    - ID: ${dataItems[i].id}`);
      console.log(`    - Size: ${dataItems[i].getRaw().byteLength} bytes`);
    }

    // Upload each data item to the bundler
    const endpoint = `${BUNDLER_URL}/~bundler@1.0/item?codec-device=ans104@1.0`;
    console.log("\n" + "=".repeat(60));
    console.log(`Uploading to: ${endpoint}`);
    console.log("=".repeat(60));
    
    const results = [];

    for (let i = 0; i < dataItems.length; i++) {
      const item = dataItems[i];
      
      console.log(`\nUploading item ${i + 1}...`);
      
      try {
        const response = await fetch(endpoint, {
          method: "POST",
          headers: {
            "Content-Type": "application/octet-stream",
          },
          body: item.getRaw(),
        });

        if (response.ok) {
          const result = await response.text();
          console.log(`  ✓ Upload successful!`);
          console.log(`    - Status: ${response.status}`);
          console.log(`    - Response: ${result}`);
          
          results.push({
            id: item.id,
            size: item.getRaw().byteLength,
            status: response.status,
            success: true,
          });
        } else {
          const errorText = await response.text();
          console.error(`  ✗ Upload failed!`);
          console.error(`    - Status: ${response.status}`);
          console.error(`    - Error: ${errorText}`);
          
          results.push({
            id: item.id,
            status: response.status,
            error: errorText,
            success: false,
          });
        }
      } catch (error) {
        console.error(`  ✗ Upload failed!`);
        console.error(`    - Error: ${error.message}`);
        
        results.push({
          id: item.id,
          error: error.message,
          success: false,
        });
      }
    }

    // Summary
    console.log("\n" + "=".repeat(60));
    console.log("Upload Summary:");
    console.log("=".repeat(60));
    console.log(`Total items: ${dataItems.length}`);
    console.log(`Successful: ${results.filter(r => r.success).length}`);
    console.log(`Failed: ${results.filter(r => !r.success).length}`);
    console.log("=".repeat(60) + "\n");

    return results;
  } catch (error) {
    console.error("\nError during upload process:", error);
    throw error;
  }
}

// Run the upload
if (require.main === module) {
  uploadDataItems()
    .then(() => {
      console.log("✓ Upload process completed\n");
      process.exit(0);
    })
    .catch((err) => {
      console.error("\n✗ Upload process failed:", err);
      process.exit(1);
    });
}

module.exports = { uploadDataItems };

