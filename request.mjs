import { connect, createSigner } from '@permaweb/aoconnect/node';
import { promises as fs } from 'fs';

const WALLET_PATH = 'wallet.json'
const NODE_URL = 'http://localhost:10000'

const wallet = JSON.parse(await fs.readFile(WALLET_PATH, 'utf8'));

console.log(wallet);

const main = async () => {

  // Create signer
  const signer = createSigner(wallet);

  // Connect to node
  const { request } = connect({
    MODE: 'mainnet', 
    URL: NODE_URL,
    device: '',
    signer
  });

  // Get request
  const metaInfoPath = `/~meta@1.0/info`;

  const getResponse = await fetch(`${NODE_URL}${metaInfoPath}`);
  console.log(getResponse);

  // Post request
  const data = {
    hello: 'world',
    testValue: 'example data'
  };

  const requestParams = {
    path: '/~meta@1.0/info/hello',
    method: 'POST',
    ...data
  };

  const postResponse = await request(requestParams);
  console.log(postResponse);

}

main();
