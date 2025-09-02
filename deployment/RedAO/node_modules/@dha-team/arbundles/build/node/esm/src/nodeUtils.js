export { stringToBuffer, concatBuffers } from "arweave/node/lib/utils.js";
export { deepHash } from "./deepHash.js";
import { default as nodeDriver } from "arweave/node/lib/crypto/node-driver.js";
import { createPublicKey } from "crypto";
export { default as Transaction } from "arweave/node/lib/transaction.js";
export { default as Arweave } from "arweave/node/index.js";
const driver = nodeDriver["default"] ? nodeDriver["default"] : nodeDriver;
export class CryptoDriver extends driver {
    getPublicKey(jwk) {
        return createPublicKey({
            key: this.jwkToPem(jwk),
            type: "pkcs1",
            format: "pem",
        })
            .export({
            format: "pem",
            type: "pkcs1",
        })
            .toString();
    }
}
let driverInstance;
export function getCryptoDriver() {
    return (driverInstance ??= new CryptoDriver());
}
//# sourceMappingURL=nodeUtils.js.map