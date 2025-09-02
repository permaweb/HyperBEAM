import fs from 'fs'

import Arweave from 'arweave';
import { connect, createSigner } from '@permaweb/aoconnect'

const BETA_GZ_OWNER_WALLET = JSON.parse(fs.readFileSync('/home/peterfarber/Workspace/vtlOY-FZjjbj3oO_VRFpQpTsyul_mT9ZqveBVGJCE_s.json'));

(async function () {
  const configPath = 'config.json';
  const config = JSON.parse(fs.readFileSync(configPath));
  const aoLegacy = connect({ MODE: 'legacy' });

  const gzAOAddress = await Arweave.init({}).wallets.jwkToAddress(BETA_GZ_OWNER_WALLET);

  const authorities = [config['hb-authority'], config['legacy-authority']];

  const processId = await aoLegacy.spawn({
    module: config['process-module'],
    scheduler: config['process-scheduler'],
    signer: createSigner(BETA_GZ_OWNER_WALLET),
    data: '1984',
    tags: [
      { name: 'On-Boot', value: config['beta-gz-ao-src'] },
      { name: 'Authority', value: authorities.join(',') },
      { name: 'ParentToken', value: config['ao-token'] },
      { name: 'Name', value: config['beta-gz-ao-name'] },
      { name: 'Ticker', value: config['beta-gz-ao-ticker'] },
      { name: 'Denomination', value: config['beta-gz-ao-denomination'] },
    ]
  });


  config['beta-gz-ao-token'] = processId;
  fs.writeFileSync(configPath, JSON.stringify(config, null, 2), 'utf8');


  const authoritiesUpdate = await aoLegacy.message({
    process: processId,
    type: 'deploy',
    data: `ao.authorities = {'${authorities[0]}', '${authorities[1]}'}`,
    signer: createSigner(BETA_GZ_OWNER_WALLET),
    tags: [
      { name: 'Action', value: 'Eval' },
    ]
  });

  console.log(`${processId}`);


})();