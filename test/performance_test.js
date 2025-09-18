#!/usr/bin/env node

/**
 * Performance test script for LLaMA inference endpoints
 * Usage: node performance_test.js [wasi-nn|llamacpp]
 */

const https = require('https');
const http = require('http');

// Configuration
const CONFIG = {
    endpoint: 'llamacpp',
    baseUrl: 'http://localhost:8734',
    testRounds: 20,
    warmupRounds: 1,
    timeout: 60000 // 60 seconds
};

// Test prompts (20 different types)
const PROMPTS = [
    "What is artificial intelligence?",
    "Explain quantum computing in simple terms",
    "Write a short poem about the ocean",
    "How does machine learning work?",
    "What are the benefits of renewable energy?",
    "Describe the process of photosynthesis",
    "What is the capital of France?",
    "Explain the theory of relativity",
    "How do computers store data?",
    "What is the difference between AI and ML?",
    "Write a recipe for chocolate cake",
    "Explain how the internet works",
    "What are the layers of the atmosphere?",
    "Describe the water cycle",
    "How do solar panels generate electricity?",
    "What is blockchain technology?",
    "Explain the concept of gravity",
    "How does DNA replication work?",
    "What are the components of a computer?",
    "Describe the process of digestion"
];

// Colors for console output
const COLORS = {
    RED: '\x1b[31m',
    GREEN: '\x1b[32m',
    YELLOW: '\x1b[33m',
    BLUE: '\x1b[34m',
    RESET: '\x1b[0m'
};

// Logging functions
const log = {
    info: (msg) => console.log(`${COLORS.BLUE}[INFO]${COLORS.RESET} ${msg}`),
    success: (msg) => console.log(`${COLORS.GREEN}[SUCCESS]${COLORS.RESET} ${msg}`),
    warning: (msg) => console.log(`${COLORS.YELLOW}[WARNING]${COLORS.RESET} ${msg}`),
    error: (msg) => console.log(`${COLORS.RED}[ERROR]${COLORS.RESET} ${msg}`)
};

// Function to make HTTP request
function makeRequest(endpoint, prompt, reference, worker) {
    return new Promise((resolve) => {
        const params = new URLSearchParams({
            prompt: prompt,
            reference: reference,
            worker: worker
        });
        
        const url = `${CONFIG.baseUrl}/~${endpoint}@1.0/infer?${params.toString()}`;
        const parsedUrl = new URL(url);
        
        const options = {
            hostname: parsedUrl.hostname,
            port: parsedUrl.port,
            path: parsedUrl.pathname + parsedUrl.search,
            method: 'POST',
            timeout: CONFIG.timeout,
            headers: {
                'Content-Type': 'application/json',
                'User-Agent': 'HyperBEAM-Performance-Test/1.0'
            }
        };
        
        const startTime = process.hrtime.bigint();
        
        const req = http.request(options, (res) => {
            let data = '';
            
            res.on('data', (chunk) => {
                data += chunk;
            });
            
            res.on('end', () => {
                const endTime = process.hrtime.bigint();
                const duration = Number(endTime - startTime) / 1000000; // Convert to milliseconds
                
                if (res.statusCode === 200) {
                    resolve({ success: true, duration, response: data, statusCode: res.statusCode });
                } else {
                    resolve({ success: false, duration, error: `HTTP ${res.statusCode}`, statusCode: res.statusCode });
                }
            });
        });
        
        req.on('error', (error) => {
            const endTime = process.hrtime.bigint();
            const duration = Number(endTime - startTime) / 1000000;
            resolve({ success: false, duration, error: error.message });
        });
        
        req.on('timeout', () => {
            req.destroy();
            const endTime = process.hrtime.bigint();
            const duration = Number(endTime - startTime) / 1000000;
            resolve({ success: false, duration, error: 'Request timeout' });
        });
        
        req.end();
    });
}

// Function to run concurrent requests
async function runConcurrentTest(endpoint, concurrency, testName) {
    log.info(`Running ${testName} with concurrency: ${concurrency}`);
    
    const results = {
        totalRequests: 0,
        successfulRequests: 0,
        failedRequests: 0,
        responseTimes: [],
        errors: []
    };
    
    // Warmup round
    log.info("Performing warmup round...");
    const warmupPromises = [];
    for (let i = 0; i < concurrency; i++) {
        warmupPromises.push(makeRequest(endpoint, PROMPTS[0], `warmup-${i}`, 'worker'));
    }
    await Promise.all(warmupPromises);
    
    // Main test rounds
    for (let round = 1; round <= CONFIG.testRounds; round++) {
        log.info(`Round ${round}/${CONFIG.testRounds}`);
        
        const promises = [];
        
        // Start concurrent requests
        for (let i = 0; i < concurrency; i++) {
            const promptIndex = (round - 1) % PROMPTS.length;
            const prompt = PROMPTS[promptIndex];
            const reference = `test-${round}-${i}-${Date.now()}`;
            const worker = `worker-${i}`;
            
            promises.push(makeRequest(endpoint, prompt, reference, worker));
        }
        
        // Wait for all requests to complete
        const roundResults = await Promise.all(promises);
        
        // Process results
        for (const result of roundResults) {
            results.totalRequests++;
            
            if (result.success) {
                results.successfulRequests++;
                results.responseTimes.push(result.duration);
                console.log(`  Request completed in: ${(result.duration / 1000).toFixed(3)}s`);
            } else {
                results.failedRequests++;
                results.errors.push(result.error);
                log.error(`Request failed: ${result.error} (${(result.duration / 1000).toFixed(3)}s)`);
            }
        }
        
        console.log('');
    }
    
    // Calculate and display statistics
    if (results.responseTimes.length > 0) {
        const sortedTimes = [...results.responseTimes].sort((a, b) => a - b);
        const avgTime = results.responseTimes.reduce((sum, time) => sum + time, 0) / results.responseTimes.length;
        const minTime = sortedTimes[0];
        const maxTime = sortedTimes[sortedTimes.length - 1];
        const p50Time = sortedTimes[Math.floor(sortedTimes.length * 0.5)];
        const p95Time = sortedTimes[Math.floor(sortedTimes.length * 0.95)];
        
        log.success(`${testName} Results:`);
        console.log(`  Total requests: ${results.totalRequests}`);
        console.log(`  Successful: ${results.successfulRequests}`);
        console.log(`  Failed: ${results.failedRequests}`);
        console.log(`  Average response time: ${(avgTime / 1000).toFixed(3)}s`);
        console.log(`  Min response time: ${(minTime / 1000).toFixed(3)}s`);
        console.log(`  Max response time: ${(maxTime / 1000).toFixed(3)}s`);
        console.log(`  P50 response time: ${(p50Time / 1000).toFixed(3)}s`);
        console.log(`  P95 response time: ${(p95Time / 1000).toFixed(3)}s`);
        
        if (results.errors.length > 0) {
            console.log(`  Error types: ${[...new Set(results.errors)].join(', ')}`);
        }
        
        console.log('');
    } else {
        log.error(`No successful requests completed for ${testName}`);
    }
    
    return results;
}

// Function to check endpoint availability
async function checkEndpoint(endpoint) {
    log.info(`Checking if ${endpoint} endpoint is available...`);
    
    try {
        const result = await makeRequest(endpoint, 'test', 'health-check', 'worker');
        if (result.success || result.statusCode < 500) {
            log.success(`${endpoint} endpoint is available`);
            return true;
        } else {
            log.error(`${endpoint} endpoint returned error: ${result.error}`);
            return false;
        }
    } catch (error) {
        log.error(`${endpoint} endpoint is not available: ${error.message}`);
        return false;
    }
}

// Main function
async function main() {
    // Parse command line arguments
    const args = process.argv.slice(2);
    
    if (args.length === 1) {
        if (args[0] === 'wasi-nn' || args[0] === 'llamacpp') {
            CONFIG.endpoint = args[0];
        } else {
            log.error("Invalid endpoint. Use 'wasi-nn' or 'llamacpp'");
            console.log("Usage: node performance_test.js [wasi-nn|llamacpp]");
            process.exit(1);
        }
    } else if (args.length > 1) {
        log.error("Too many arguments");
        console.log("Usage: node performance_test.js [wasi-nn|llamacpp]");
        process.exit(1);
    }
    
    log.info(`Starting performance test for endpoint: ${CONFIG.endpoint}`);
    log.info(`Base URL: ${CONFIG.baseUrl}`);
    log.info(`Test rounds per concurrency level: ${CONFIG.testRounds}`);
    
    // Check endpoint availability
    const isAvailable = await checkEndpoint(CONFIG.endpoint);
    if (!isAvailable) {
        log.warning("Endpoint check failed, but continuing with tests...");
    }
    
    console.log("========================================");
    console.log(`Performance Test Results for ${CONFIG.endpoint}`);
    console.log("========================================");
    console.log("");
    
    // Run tests with different concurrency levels
    try {
        await runConcurrentTest(CONFIG.endpoint, 1, "Sequential Test (Concurrency: 1)");
        await runConcurrentTest(CONFIG.endpoint, 2, "Low Concurrency Test (Concurrency: 2)");
        await runConcurrentTest(CONFIG.endpoint, 4, "Medium Concurrency Test (Concurrency: 4)");
        
        log.success("Performance testing completed!");
    } catch (error) {
        log.error(`Test execution failed: ${error.message}`);
        process.exit(1);
    }
}

// Handle unhandled promise rejections
process.on('unhandledRejection', (reason, promise) => {
    log.error(`Unhandled Rejection at: ${promise}, reason: ${reason}`);
    process.exit(1);
});

// Run the main function
if (require.main === module) {
    main().catch((error) => {
        log.error(`Application error: ${error.message}`);
        process.exit(1);
    });
}

module.exports = { makeRequest, runConcurrentTest, checkEndpoint };
