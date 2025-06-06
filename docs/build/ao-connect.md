## Guide: Spawning and Interacting with a hyperAOS Process

This guide explains how to spawn and interact with a process using HyperBEAM and `aoconnect`.

### Prerequisites
- Node.js environment
- `@permaweb/aoconnect` library
- Wallet file (`wallet.json`) containing your cryptographic keys
- Local Permaweb node running

### Step 1: Environment Setup
Install necessary dependencies:

```bash
npm install @permaweb/aoconnect
```

Ensure your wallet file (`wallet.json`) is correctly formatted and placed in your project directory.

> NOTE: you can create a test wallet using this line:
> `npx -y @permaweb/wallet > wallet.json`

### Step 2: Establish Connection
Create a new JavaScript file (e.g., `index.js`) and set up your Permaweb connection:

```javascript
import { connect, createSigner } from '@permaweb/aoconnect'
import fs from 'node:fs'

const jwk = JSON.parse(fs.readFileSync('wallet.json', 'utf-8'))

const hyperAOS = "XcyVlrOvk4b9XifeUmkbqqGwPwwv1VW94hUhNnE7v14";

async function main() {
  const address = await fetch('http://localhost:10000/~meta@1.0/info/address')
    .then(res => res.text())

  const { request } = await connect({
    MODE: 'mainnet',
    URL: 'http://localhost:10000',
    signer: createSigner(jwk)
  })
```

### Step 3: Spawning a hyperAOS Process
Use the following code snippet to spawn your hyperAOS process:

```javascript
  const processResult = await request({
    method: 'POST',
    device: 'process@1.0',
    path: '/schedule',
    type: 'Process',
    scheduler: address,
    script: hyperAOS,
    'execution-device': 'lua@5.3a',
    'scheduler-device': 'scheduler@1.0',
    authority: address,
    'Data-Protocol': 'ao',
    Variant: 'ao.N.1',
  })
```

### Step 4: Scheduling a Message
Schedule a Lua expression evaluation within your spawned hyperAOS process:

```javascript
  const response = await request({
    method: 'POST',
    type: 'Message',
    path: `/${processResult.process}~process@1.0/schedule`,
    action: 'Eval',
    data: '1 + 1',
    'Data-Protocol': 'ao',
    Variant: 'ao.N.1'
  })
```

### Step 5: Retrieving Results

In this section we are going to construct a hyperPATH to
return the output of our computed evaluation.

```javascript
const hyperPath = 
     `/${processResult.process}~process@1.0/compute` +
      `&slot=${response.slot}/results/output`
```

Fetch the computation results from your scheduled message:

```javascript
  const result = await request({
    method: 'GET',
    path: hyperPath
  })

  console.log(result.body)
}

main()
```

### Running the demo

```
node index.js
```

Output

```
2
```


### Conclusion
Following these steps, you've successfully spawned a process , scheduled a Lua computation, and retrieved its output. This approach allows you to develop dynamic, decentralized applications on the Permaweb using Lua scripting.