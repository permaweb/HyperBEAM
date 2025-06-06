# HyperBEAM from AO Connect
This guide explains how to interact with a process using HyperBEAM and `aoconnect`.

### Prerequisites
- Node.js environment
- `@permaweb/aoconnect` library
- The latest version of `aos`
- Wallet file (`wallet.json`) containing your cryptographic keys
- A `HyperBEAM` node running with the `genesis_wasm` profile. Start it from your HyperBEAM directory with:
  ```bash
  rebar3 as genesis_wasm shell
  ```
- The Process ID for a process created with `genesis_wasm` (this is the default in the latest version of `aos`).

### Step 1: Environment Setup
Install necessary dependencies:

```bash
npm install @permaweb/aoconnect
```

Ensure your wallet file (`wallet.json`) is correctly formatted and placed in your project directory.

> NOTE: you can create a test wallet using this line:
> `npx -y @permaweb/wallet > wallet.json`

### Step 2: Establish Connection
Create a new JavaScript file (e.g., `index.js`) and set up your Permaweb connection. You will need a `processId` of a process that you want to interact with.

```javascript
import { connect, createSigner } from '@permaweb/aoconnect'
import fs from 'node:fs'

const jwk = JSON.parse(fs.readFileSync('wallet.json', 'utf-8'))

// The Process ID to interact with
const processId = "GMajLmn7Hv9dbmEM6-vWDl6chUgDLHYqzwh34CAllAQ";

async function main() {
  const { request } = connect({
    MODE: 'mainnet',
    URL: 'http://localhost:8734',
    signer: createSigner(jwk)
  })
}

### Step 3: Pushing a Message to a Process
Use the `request` function to send a message to the process. In `aoconnect`, this is done by using the `push` hyperpath.

```javascript
  const processResult = await request({
      path: `/${processId}~process@1.0/push/serialize~json@1.0`,
      method: 'POST',
      target: processId,
      "signingFormat": "ANS-104"
    })

  console.log(processResult)
}

main()
```

### Running the demo

To run the full script, combine the snippets from Step 2 and 3 into `index.js`:

```javascript
import { connect, createSigner } from '@permaweb/aoconnect'
import fs from 'node:fs'

const jwk = JSON.parse(fs.readFileSync('wallet.json', 'utf-8'))

const processId = "GMajLmn7Hv9dbmEM6-vWDl6chUgDLHYqzwh34CAllAQ";

async function main() {
  const { request } = connect({
    MODE: 'mainnet',
    URL: 'http://localhost:8734',
    signer: createSigner(jwk)
  })

  const processResult = await request({
      path: `/${processId}~process@1.0/push/serialize~json@1.0`,
      method: 'POST',
      target: processId,
      "signingFormat": "ANS-104"
    })

  console.log(processResult)
}

main()
```

Now, run it:
```bash
node index.js
```

You should see an object logged to the console, containing the ID of the message that was sent.

### Conclusion
Following these steps, you've successfully sent a message to a process. This is a fundamental interaction for building applications on hyperAOS.