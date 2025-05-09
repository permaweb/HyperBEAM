#!/usr/bin/env node
import { connect, createSigner } from '@permaweb/aoconnect'
import fs from 'fs'
console.log('t2')
const WALLET = JSON.parse(fs.readFileSync('wallet.json', 'utf-8'))

async function main() {
  console.log('t2')
	try {
		console.log('t2a')
    const _beam = connect({
      MODE: 'mainnet',
      URL: 'http://localhost:10000',
      device: 'process@1.0',
      signer: createSigner(WALLET),
    });

    const token = 'LnHJg3nNeVqIIt_-LyTo94A2mMw5m1warJILsKJRptM';
	console.log('t2b')
    const res = await _beam.request({
      type: 'Message',
      path: `/${token}~process@1.0/push`,
      method: 'POST',
      Action: 'Transfer',
      Recipient: 'vh-NTHVvlKZqRxc8LyyTNok65yQ55a_PJ1zWLb9G2JI',
      Quantity: '1000',
      variant: 'ao.N.1',
      'data-protocol': 'ao',
      target: token,
      data: '1984',
    });
	
	console.log('t2c')
    console.log('res', res);

    const slot = res.slot;
    const _result = await _beam.request({
      path: `/${token}~process@1.0/compute&slot+integer=${slot}/results/json`,
      method: 'GET',
      target: token,
    });
    const out = JSON.parse(_result.body);
    console.log(out);

    if (out.Error) {
      console.log(out.Error);
    } else {
      console.log(out.Output.data);
    }
  } catch (err) {
    console.error('Error:', err);
  }
}

main();
