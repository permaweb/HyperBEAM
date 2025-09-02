import fs from 'fs';

import Arweave from 'arweave';
import { connect, createSigner } from '@permaweb/aoconnect';

const AO_HOLDER_WALLET = JSON.parse(fs.readFileSync('/home/peterfarber/Workspace/vtlOY-FZjjbj3oO_VRFpQpTsyul_mT9ZqveBVGJCE_s.json'));

(async function () {
    const config = JSON.parse(fs.readFileSync('config.json'));
    const aoLegacy = connect({
        MODE: 'legacy',
        MU_URL: 'https://mu203.ao-testnet.xyz',
        CU_URL: 'https://cu72.ao-testnet.xyz'
    });

    const aoHolderAddress = await Arweave.init({}).wallets.jwkToAddress(AO_HOLDER_WALLET);
    console.log(`AO Holder Address: ${aoHolderAddress}`);

    console.log('Getting Current Beta GZ AO Balance...');
    const currentBetaGZAOBalance = (await aoLegacy.dryrun({
        process: config['beta-gz-ao-token'],
        tags: [
            { name: 'Action', value: 'Balance' },
            { name: 'Recipient', value: aoHolderAddress }
        ]
    })).Messages[0].Data;

    console.log(`Current Beta Green Zone AO Balance: ${currentBetaGZAOBalance}`);

    let recipientAddress = aoHolderAddress;

    console.log(`Recipient Address: ${recipientAddress}`);

    let sendAmount = 1;

    console.log(`Send Amount: ${sendAmount}`);

    console.log('Transferring AO to Beta Green Zone AO...');
    const transferId = await aoLegacy.message({
        process: config['ao-token'],
        tags: [
            { name: 'Action', value: 'Transfer' },
            { name: 'Quantity', value: sendAmount.toString() },
            { name: 'Recipient', value: config['beta-gz-ao-token'] },
        ],
        signer: createSigner(AO_HOLDER_WALLET)
    });

    console.log(`Transfer: ${transferId}`);

    await aoLegacy.result({
        process: config['ao-token'],
        message: transferId
    });

    let updatedBetaGZAOBalance;
    do {
        console.log('Getting Updated Beta Green Zone AO Balance...');

        await new Promise((r) => setTimeout(r, 2000));
        updatedBetaGZAOBalance = (await aoLegacy.dryrun({
            process: config['beta-gz-ao-token'],
            tags: [
                { name: 'Action', value: 'Balance' },
                { name: 'Recipient', value: aoHolderAddress }
            ]
        })).Messages[0].Data;

        console.log(`Updated Beta Green Zone AO Balance: ${updatedBetaGZAOBalance}`);
    }
    while (updatedBetaGZAOBalance === currentBetaGZAOBalance)





    /* Create an even topup split for all nodes based on the holders Beta Green Zone AO Balance */
    const topupAmount = sendAmount;

    const node = 'http://localhost:8734';
    console.log(`Topping up ${node} ledger...`);


    const aoMainnet = connect({
        MODE: 'mainnet',
        URL: node,
        signer: createSigner(AO_HOLDER_WALLET)
    });

    const ledgerAddressRes = await fetch(`${node}/ledger~node-process@1.0/commitments/keys/1`);
    const ledgerAddress = await ledgerAddressRes.text();
    console.log(`Ledger Address: ${ledgerAddress}`);

    console.log('Fetching current node ledger...');
    let ledgerRes = await fetch(`${node}/ledger~node-process@1.0/now/balance/serialize~json@1.0`);
    let ledger = await ledgerRes.json();

    console.log('-'.repeat(20));
    console.log('Current Ledger');
    console.log(ledger);
    console.log('-'.repeat(20));

    const path = `/${config['beta-gz-ao-token']}~process@1.0/push`;

    console.log(`Path: ${path}`);
    console.log(`Ledger Address: ${ledgerAddress}`);
    console.log('-'.repeat(20));

    console.log('Transferring Beta Green Zone AO to Subledger...');

    const transferParams = {
        type: 'Message',
        path: path,
        method: 'POST',
        'data-protocol': 'ao',
        variant: 'ao.N.1',
        target: config['beta-gz-ao-token'],
        'signingFormat': 'ANS-104',
        'accept-bundle': 'true',
        'accept-codec': 'httpsig@1.0',
        action: 'Transfer',
        Recipient: 'vtlOY-FZjjbj3oO_VRFpQpTsyul_mT9ZqveBVGJCE_s',
        Route: ledgerAddress,
        Quantity: topupAmount.toString()
    }

    console.log('Transfer Params: ', transferParams);

    const res = await aoMainnet.request(transferParams);
    console.log(`Transfer status: ${res.status}`);

    ledgerRes = await fetch(`${node}/ledger~node-process@1.0/now/balance/serialize~json@1.0`);
    ledger = await ledgerRes.json();

    console.log('-'.repeat(20))
    console.log('Updated Ledger')
    console.log(ledger);
    console.log('-'.repeat(20))

})();
