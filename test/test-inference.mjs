import { connect, createSigner } from '@permaweb/aoconnect';
import {readFileSync} from 'fs'

const jwk = JSON.parse(readFileSync('../wallet.json'))

const { request } = connect({
    MODE: "mainnet",
    URL: "http://localhost:8734",
    signer: createSigner(jwk),
});

const PID = 'aHzs6qgWnDsxGsOPNt64dQu7LjItiLBVgRRjbwKxKjk';

const sendPrompt = async (prompt) => {
    const ref = `test-${Date.now()}`;
    try {
        // Send message to your AO process using aoconnect.request
        const data = await request({
            type: 'Message',
            path: `/${PID}~process@1.0/push/serialize~json@1.0`,
            method: "POST",
            'data-protocol': 'ao',
            variant: 'ao.N.1',
            "accept-bundle": "true",
            "accept-codec": "httpsig@1.0",
            signingFormat: "ANS-104",
            target: PID,
            Action: "Infer",
            // your tags
            // ...tags.filter(t => t.name !== 'device').reduce((a, t) => assoc(t.name, t.value, a), {}),
            'X-Reference': ref, // Unique reference for this request
            data: prompt, // The AI prompt to send
        });
        console.log(data)

    } catch (error) {
        console.error('Failed to send prompt:', error);
        setAiResult(`Error sending prompt: ${error.message}`);
    }
};

async function main() {
    // for (let i = 0; i < 1; i++) {
        await sendPrompt("Write a poem about a cat in space.")
    // }
}

main().finally(() => {
    process.exit(0);
})