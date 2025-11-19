const fs = require("fs");
const path = require("path");
const { ArweaveSigner, createData } = require("@dha-team/arbundles");

// Configuration
const BUNDLER_URL = "http://localhost:8734";
const WALLET_PATH = "/Users/piechota/dev/arweave/repos/wallets/4OPKVB3dDtm1SP_8icx5ILil87Bh_JduGHzj8cuI36k.json";
const CONCURRENT_UPLOADS = 100; // Number of parallel uploads

async function performanceTest(itemCount, bytesPerItem = 0) {
  const wallet = require(WALLET_PATH);
  const signer = new ArweaveSigner(wallet);
  const endpoint = `${BUNDLER_URL}/~bundler@1.0/item?codec-device=ans104@1.0`;

  console.log("\n" + "=".repeat(70));
  console.log("ANS-104 Bundle Upload Performance Test");
  console.log("=".repeat(70));
  console.log(`Target:     ${BUNDLER_URL}`);
  console.log(`Items:      ${itemCount}`);
  console.log(`Item Size:  ${bytesPerItem > 0 ? `~${bytesPerItem} bytes` : 'default'}`);
  console.log(`Concurrent: ${CONCURRENT_UPLOADS}`);
  console.log("=".repeat(70) + "\n");

  // Create and sign data items
  console.log("Creating and signing data items...");
  const createStart = Date.now();
  
  const dataItems = [];
  for (let i = 0; i < itemCount; i++) {
    const timestamp = Date.now();
    const baseMessage = `Performance test item ${i} at ${timestamp}`;
    
    // Build data payload to meet size requirement
    let data = baseMessage;
    if (bytesPerItem > 0) {
      while (Buffer.byteLength(data, 'utf8') < bytesPerItem) {
        data += " " + baseMessage;
      }
    }
    
    const item = createData(data, signer, {
      tags: [
        { name: "Content-Type", value: "text/plain" },
        { name: "Item-Number", value: String(i) },
        { name: "Test-Type", value: "performance" },
        { name: "Timestamp", value: String(timestamp) }
      ],
    });
    await item.sign(signer);
    dataItems.push(item);
  }
  
  const createTime = Date.now() - createStart;
  console.log(`✓ Created and signed ${itemCount} items in ${createTime}ms`);
  console.log(`  Average: ${(createTime / itemCount).toFixed(2)}ms per item\n`);

  // Upload data items with concurrency
  console.log("Uploading data items...");
  const uploadStart = Date.now();
  
  let completed = 0;
  let successful = 0;
  let failed = 0;
  
  // Upload in batches
  for (let i = 0; i < dataItems.length; i += CONCURRENT_UPLOADS) {
    const batch = dataItems.slice(i, i + CONCURRENT_UPLOADS);
    
    const uploadPromises = batch.map(async (item) => {
      try {
        const response = await fetch(endpoint, {
          method: "POST",
          headers: {
            "Content-Type": "application/octet-stream",
          },
          body: item.getRaw(),
        });

        if (response.ok) {
          successful++;
          return { success: true, id: item.id };
        } else {
          failed++;
          const errorText = await response.text();
          return { success: false, id: item.id, error: `${response.status}: ${errorText}` };
        }
      } catch (error) {
        failed++;
        return { success: false, id: item.id, error: error.message };
      } finally {
        completed++;
        // Print progress every 10 items or on last batch
        if (completed % 10 === 0 || completed === itemCount) {
          process.stdout.write(`\r  Progress: ${completed}/${itemCount} items (${successful} successful, ${failed} failed)`);
        }
      }
    });

    await Promise.all(uploadPromises);
  }
  
  const uploadTime = Date.now() - uploadStart;
  const totalTime = Date.now() - createStart;
  
  console.log("\n");
  console.log("=".repeat(70));
  console.log("Performance Results:");
  console.log("=".repeat(70));
  console.log(`Total items:        ${itemCount}`);
  console.log(`Successful:         ${successful}`);
  console.log(`Failed:             ${failed}`);
  console.log(`Success rate:       ${((successful / itemCount) * 100).toFixed(2)}%`);
  console.log("");
  console.log(`Create time:        ${createTime}ms (${(createTime / itemCount).toFixed(2)}ms per item)`);
  console.log(`Upload time:        ${uploadTime}ms (${(uploadTime / itemCount).toFixed(2)}ms per item)`);
  console.log(`Total time:         ${totalTime}ms`);
  console.log("");
  console.log(`Upload throughput:  ${(successful / (uploadTime / 1000)).toFixed(2)} items/sec`);
  console.log(`Overall throughput: ${(successful / (totalTime / 1000)).toFixed(2)} items/sec`);
  console.log("=".repeat(70) + "\n");
  
  return {
    itemCount,
    successful,
    failed,
    createTime,
    uploadTime,
    totalTime,
    itemsPerSecond: successful / (uploadTime / 1000),
  };
}

// Main execution
if (require.main === module) {
  const itemCount = parseInt(process.argv[2], 10);
  const bytesPerItem = parseInt(process.argv[3], 10) || 0;
  
  if (!itemCount || itemCount < 1 || isNaN(itemCount)) {
    console.error("Usage: node upload-items.js <number_of_items> [bytes_per_item]");
    console.error("");
    console.error("Arguments:");
    console.error("  number_of_items  - Number of data items to create and upload");
    console.error("  bytes_per_item   - Minimum size of each item in bytes (optional)");
    console.error("");
    console.error("Examples:");
    console.error("  node upload-items.js 100");
    console.error("  node upload-items.js 100 1024     # 100 items, ~1KB each");
    console.error("  node upload-items.js 50 10485760  # 50 items, ~10MB each");
    process.exit(1);
  }

  performanceTest(itemCount, bytesPerItem)
    .then(() => {
      process.exit(0);
    })
    .catch((err) => {
      console.error("\n✗ Performance test failed:", err);
      process.exit(1);
    });
}

module.exports = { performanceTest };
