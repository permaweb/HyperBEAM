import { program } from 'commander'
import { connect, createSigner } from '@permaweb/aoconnect'
import fs from 'fs';
import path from 'path';
import url from 'url';
import os from 'os';

const WALLET = JSON.parse(fs.readFileSync('./wallet.json', 'utf-8'));
console.log(WALLET);
const parsedUrl = new URL("http://localhost:10000");
const { request } = connect({
	MODE: 'mainnet',
	device: 'process@1.0',
	signer: createSigner(WALLET),
	URL: parsedUrl.origin
  })

// console.log(parsedUrl.origin);
//console.log(request);

// > params { path: '/~meta@1.0/info', method: 'GET', headers: {} }
// const params = { path: '/~meta@1.0/info', method: 'GET', headers: {} }
// const res = await request(params)
// console.log(res)

// read the lua file into a string 
const luaScript = fs.readFileSync('./test.lua', 'utf-8');
// console.log(luaScript);
const base64LuaScript = Buffer.from(luaScript).toString('base64');

//const params1 = { path: '/~process@1.0/info', method: 'POST', headers: {} }
// const res1 = await request(params1)
// console.log(res1)

// schedule the process@1.0 and send the lua file to it
/**
 * const res = await _beam.request({
    type: 'Message',
    path: `/${token}~process@1.0/push`,
    method: 'POST',
    Action: 'Transfer',
    Recipient: 'vh-NTHVvlKZqRxc8LyyTNok65yQ55a_PJ1zWLb9G2JI',
    Quantity: '1000',
    variant: 'ao.N.1',
    'data-protocol': 'ao',
    target: token,
    data: '1984'
  })
 */
// const params2 = {
// 		"type":"Process",
// 		"scheduler-device":"scheduler@1.0",
// 		"execution-device":"lua@5.3a",
// 		"script":{
// 		  "content-type":"application/lua",
// 		  "body": base64LuaScript
// 		},
// 		"authority":[
// 		  "DYohZwJzLcpFwp_4ezx7_8Qf31hYwQpifZ05DkOPwEI",
// 		  "E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ"
// 		],
// 		"scheduler-location":"DYohZwJzLcpFwp_4ezx7_8Qf31hYwQpifZ05DkOPwEI",
// 		"test-random-seed":123,
// }
// const processMsg = {
// 	device            : 'process@1.0',
// 	type              : 'Process',
// 	'scheduler-device': 'scheduler@1.0',
// 	'execution-device': 'lua@5.3a',
// 	script            : {
// 	  'content-type': 'application/lua',
// 	  body          : base64LuaScript
// 	},
// 	authority         : [
// 	  "DYohZwJzLcpFwp_4ezx7_8Qf31hYwQpifZ05DkOPwEI",
// 	  'E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ'
// 	],
// 	'scheduler-location': "DYohZwJzLcpFwp_4ezx7_8Qf31hYwQpifZ05DkOPwEI",
// 	'test-random-seed'  : 123
//   }
//   // path: `/${token}~process@1.0/compute&slot+integer=${slot}/results/json`,
//   const schedRes = await request({
// 	path  : '/~process@1.0/schedule',
// 	method: 'POST',
// 	body  : processMsg
//   })
//   console.log(schedRes);
console.log('t3')
// console.log(luaScript)
// *** ADD THIS LINE ***
console.log('--- DEBUG: Script content before request ---\n', luaScript, '\n--- END DEBUG ---'); 

let res1; 
try {
	res1 = await request({
		path   : '/schedule',           // same endpoint the Erlang test uses
		method : 'POST',
		device : 'process@1.0',
		type   : 'Process',
		variant              : 'ao.N.1',
		  'data-protocol'      : 'ao',
		'scheduler-device' : 'scheduler@1.0',
		'execution-device' : 'lua@5.3a',
		script : { 
			'content-type':'application/lua', 
			body: luaScript 
		},
		'scheduler-location': 'DYohZwJzLcpFwp_4ezx7_8Qf31hYwQpifZ05DkOPwEI',   // critical
		authority: [ 'DYohZwJzLcpFwp_4ezx7_8Qf31hYwQpifZ05DkOPwEI', 'E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ' ]
	  })
	  console.log('t4')
	  console.log(res1);
} catch (error) {
		console.error("!!! ERROR DURING request() CALL !!!");
	console.error("Error Message:", error.message);
  console.error("Error Stack:", error.stack);
}

// 	const params1 = {
// 		'path': '/~process@1.0/schedule',
// 		'method': 'POST',
// 		// The following matches the structure in generate_lua_process:
// 		'device': 'process@1.0',
// 		'type': 'Process',
// 		'scheduler-device': 'scheduler@1.0',
// 		'execution-device': 'lua@5.3a',
// 		script: {
// 		  'content-type': 'application/lua',
// 		  body: luaScript   // Plain text script, not base64 - aoconnect handles encoding
// 		},
// 		authority: [
// 		  ADDRESS,  // Your wallet address 
// 		  'E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ'  // Same secondary authority as test
// 		],
// 		'scheduler-location': ADDRESS,  // Critical - must match signer wallet address
// 		'test-random-seed': Math.floor(Math.random() * 1337),
// }
// const res1 = await request(params1)
// console.log(res1)