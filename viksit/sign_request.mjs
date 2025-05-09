#!/usr/bin/env node
import fs       from 'fs';
import crypto   from 'crypto';
import { argv } from 'process';
import { createSigner } from '@permaweb/aoconnect';



// -------------- cli flags --------------
function opts () {
  const o = { path:null, file:null, key:'wallet.json', host:'localhost:10000' };
  for (let i=2;i<argv.length;i+=2) {
    const k=argv[i], v=argv[i+1];
    if (k==='--path')   o.path=v;
    else if (k==='--file') o.file=v;
    else if (k==='--key')  o.key=v;
    else if (k==='--host') o.host=v;
    else { console.error('unknown flag',k); process.exit(1); }
  }
  if (!o.path||!o.file) { console.error('Need --path and --file'); process.exit(1); }
  return o;
}

(async () => {
  const { path, file, key, host } = opts();
  const WALLET  = JSON.parse(fs.readFileSync(key,'utf8'));
  const payload  = fs.readFileSync(file,'utf8');

  // multipart
  const boundary='hb_'+crypto.randomBytes(4).toString('hex');
  const body=`--${boundary}\r
Content-Disposition: form-data; name="body"\r
Content-Type: application/json\r
\r
${payload}\r
--${boundary}--\r
`;

  const digest=`sha-256=:${crypto.createHash('sha256').update(body).digest('base64')}:`;

  // ---- SIGN ----
  const signer  = createSigner(WALLET);         
  const sigBytes = await signer.sign(Buffer.from(digest));  // Binary, not utf‑8 string
  const signature=`sig1=:${Buffer.from(sigBytes).toString('base64')}:`;
  const keyId    = WALLET.n;                               // modulus works for demo
  const sigInput = `sig1=("@content-digest" "@content-type");alg="rsa-pss-sha512";keyid="${keyId}"`;

  // curl
  console.log(`curl -i -X POST http://${host}${path} \\`);
  console.log(`  -H 'Content-Type: multipart/form-data; boundary=${boundary}' \\`);
  console.log(`  -H 'Content-Digest: ${digest}' \\`);
  console.log(`  -H 'Signature-Input: ${sigInput}' \\`);
  console.log(`  -H 'Signature: ${signature}' \\`);
  console.log(`  --data-binary @- <<'__HB__'`);
  process.stdout.write(body);
  console.log(`__HB__`);
})();