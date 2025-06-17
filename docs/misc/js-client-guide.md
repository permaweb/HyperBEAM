# JavaScript Client Guide for HyperBEAM

This guide demonstrates how to integrate with HyperBEAM nodes using the an example JavaScript client library.


## Prerequisites

- Node.js
- npm or yarn package manager

## Project Setup

First, create a new Node.js project:

```bash
mkdir my-hyperbeam-project
cd my-hyperbeam-project
npm init -y
```

Next, manually edit your `package.json` file and add the `"type": "module"` property after the "main" line. This is required for using ES modules with import/export syntax.

Then, install the required dependencies:

```bash
npm install @permaweb/aoconnect
```

Download the HyperBEAM client implementation file and save it as `hyperbeam-client.js` in your project directory.

[Download hyperbeam-client.js](../assets/hyperbeam-client.js){ .md-button .md-button--primary download="hyperbeam-client.js" style="display: block; width: 100%; text-align: center;" }

Your final `package.json` should look similar to this:

```json linenums="1"
{
  "name": "my-hyperbeam-project",
  "version": "1.0.0",
  "main": "index.js",
  "type": "module",
  "dependencies": {
    "@permaweb/aoconnect": "^0.0.77"
  }
}
```

## Starter

```javascript linenums="1"
import { HyperBEAMClient } from './hyperbeam-client.js';

// Create a client instance
const client = new HyperBEAMClient({
    nodeUrl: 'http://localhost:10000',  // HyperBEAM node URL
    keyPath: 'wallet.json'              // Path to your wallet key file
});

// Initialize the client
await client.initialize();

// Your Code Here
```

## Extending Stater Examples

### Get Request ***~meta@1.0/info***

```javascript linenums="1"
// Make an unauthenticated GET request
const response = await client.get('/~meta@1.0/info', { debug: true });
console.log(response);
```

### Post Request ***~meta@1.0/info***

```javascript linenums="1"
// Make an signed POST request
const data = {
	"hello": "world",
	"testValue": "example data"
};
const infoResponse = await client.post('/~meta@1.0/info', data, { debug: true });
console.log("Response:", infoResponse);
```

