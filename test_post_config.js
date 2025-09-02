#!/usr/bin/env node

// Simple logger implementation
const logger = {
  info: (message, data) => {
    console.log(`[INFO] ${message}`, data ? JSON.stringify(data, null, 2) : '');
  },
  debug: (message, data) => {
    console.log(`[DEBUG] ${message}`, data ? JSON.stringify(data, null, 2) : '');
  },
  error: (message, error) => {
    console.error(`[ERROR] ${message}`, error);
  }
};

// MetaResponse interface (for documentation)
// interface MetaResponse {
//   success: boolean;
//   status: number;
//   body: string;
// }

async function postConfig(nodeUrl, configContent, device = 'json@1.0') {
  const url = `${nodeUrl}/~meta@1.0/info`;
  
  try {
    logger.info(`Posting config to meta endpoint: ${url}`);
    logger.debug('Config content:', { configContent });

    const response = await fetch(url, {
      method: 'POST',
      headers: {
        'codec-device': device,
        'accept-bundle': 'true',
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(configContent)
    });

    const responseBody = await response.text();
    
    logger.info(`Meta POST response for ${nodeUrl}:`, {
      status: response.status,
      body: responseBody
    });

    return {
      success: response.ok,
      status: response.status,
      body: responseBody
    };

  } catch (error) {
    logger.error(`Failed to post config to ${nodeUrl}:`, error);
    return {
      success: false,
      status: 0,
      body: error.message
    };
  }
}

// Test function
async function runTest() {
  console.log('🚀 Starting postConfig test...\n');

  // Test configuration - modify as needed
  const testConfig = {
    id: "test-node-123",
    port: "8080",
    node_processes: {
      ledger: {
        admin: "test-admin-456",
        authority: "test-authority-789",
        device: "process@1.0",
        module: "test-module-abc"
      }
    },
    on: {
      request: {
        device: "p4@1.0",
        "ledger-device": "lua@5.3a",
        "pricing-device": "simple-pay@1.0",
        "ledger-path": "/ledger~node-process@1.0",
        module: "2MVk_oOeXrQBOl27JQIeFKhwRlfGzFxs5UhrC_dSrJo"
      }
    }
  };

  // Test cases
  const testCases = [
    {
      name: "Test with localhost:8080",
      nodeUrl: "http://localhost:8080",
      config: testConfig,
      device: "json@1.0"
    },
    {
      name: "Test with different port",
      nodeUrl: "http://localhost:3000", 
      config: { ...testConfig, port: "3000" },
      device: "json@1.0"
    },
    {
      name: "Test with different device",
      nodeUrl: "http://localhost:8080",
      config: testConfig,
      device: "structured@1.0"
    }
  ];

  // Run tests
  for (const testCase of testCases) {
    console.log(`\n📋 Running: ${testCase.name}`);
    console.log('=' .repeat(50));
    
    const result = await postConfig(testCase.nodeUrl, testCase.config, testCase.device);
    
    console.log('📊 Test Result:');
    console.log(`   Success: ${result.success}`);
    console.log(`   Status: ${result.status}`);
    console.log(`   Body: ${result.body.substring(0, 200)}${result.body.length > 200 ? '...' : ''}`);
    
    // Add a small delay between requests
    await new Promise(resolve => setTimeout(resolve, 1000));
  }

  console.log('\n✅ Test completed!');
}

// Command line usage
if (require.main === module) {
  // Check if Node.js version supports fetch (Node 18+)
  if (typeof fetch === 'undefined') {
    console.error('❌ This script requires Node.js 18+ or you need to install node-fetch');
    console.error('   Run: npm install node-fetch');
    console.error('   Then add: const fetch = require("node-fetch");');
    process.exit(1);
  }

  // Parse command line arguments
  const args = process.argv.slice(2);
  if (args.length >= 2) {
    const [nodeUrl, configFile, device] = args;
    
    // Load config from file if provided
    if (configFile && require('fs').existsSync(configFile)) {
      const fs = require('fs');
      const configContent = JSON.parse(fs.readFileSync(configFile, 'utf8'));
      
      console.log(`🔧 Using config from: ${configFile}`);
      postConfig(nodeUrl, configContent, device || 'json@1.0')
        .then(result => {
          console.log('📊 Result:', result);
          process.exit(result.success ? 0 : 1);
        })
        .catch(error => {
          console.error('❌ Error:', error);
          process.exit(1);
        });
    } else {
      console.error(`❌ Config file not found: ${configFile}`);
      process.exit(1);
    }
  } else {
    // Run default tests
    runTest().catch(error => {
      console.error('❌ Test failed:', error);
      process.exit(1);
    });
  }
}

// Export for use as module
module.exports = { postConfig, logger };
