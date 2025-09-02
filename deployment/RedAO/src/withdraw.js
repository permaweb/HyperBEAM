import fs from 'fs';

import Arweave from 'arweave';
import { connect, createSigner } from '@permaweb/aoconnect';

const NODE_ADMIN_WALLET = JSON.parse(fs.readFileSync(process.env.PATH_TO_NODE_ADMIN_WALLET));

(async function () {
    const config = JSON.parse(fs.readFileSync('config.json'));
    const aoLegacy = connect({ MODE: 'legacy' });
    const aoMainnet = connect({
        MODE: 'mainnet',
        URL: config['withdraw-node'],
        signer: createSigner(NODE_ADMIN_WALLET)
    });

    const nodeAminAddress = await Arweave.init({}).wallets.jwkToAddress(NODE_ADMIN_WALLET);
    console.log(`Node Admin Address: ${nodeAminAddress}`);

    console.log('Getting node subledger address...');
    const subledgerRes = await fetch(`${config['withdraw-node']}/ledger~node-process@1.0/commitments/keys/1`);
    const sublegder = await subledgerRes.text();

    console.log(`Node Sublegder: ${sublegder}`);

    console.log('Getting node admin balance on ledger...');
    const balanceRes = await fetch(`${config['withdraw-node']}/ledger~node-process@1.0/now/balance/${nodeAminAddress}`);
    const balance = await balanceRes.text();

    /* Send a transfer from HB Admin wallet to HB Subledger
       Recipient = Wallet
       Route = Beta Green Zone AO 
    */
    const params = {
        type: 'Message',
        path: `/${sublegder}~process@1.0/push/serialize~json@1.0`,
        method: 'POST',
        'data-protocol': 'ao',
        variant: 'ao.N.1',
        target: sublegder,
        'accept-bundle': 'true',
        'accept-codec': 'httpsig@1.0',
        'signingFormat': 'ANS-104',
        action: 'Transfer',
        Recipient: nodeAminAddress,
        Route: config['beta-gz-ao-token'],
        Quantity: balance
    }

    const res = await aoMainnet.request(params);
    console.log(res);

    /* Send a withdraw request to Beta Green Zone AO, which will then execute a transfer of AO to the admin wallet */
    const withdrawId = await aoLegacy.message({
        process: config['beta-gz-ao-token'],
        tags: [
            { name: 'Action', value: 'Withdraw' },
            { name: 'Quantity', value: balance }
        ],
        signer: createSigner(NODE_ADMIN_WALLET)
    });

    const withdrawResult = await aoLegacy.result({
        process: config['beta-gz-ao-token'],
        message: withdrawId
    });

    console.log(withdrawResult);
})();