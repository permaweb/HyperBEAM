export { stringToBuffer, concatBuffers } from "arweave/web/lib/utils.js";
export { deepHash } from "./deepHash.js";
import webDriver from "arweave/web/lib/crypto/webcrypto-driver.js";
export { default as Transaction } from "arweave/web/lib/transaction.js";
export { default as Arweave } from "arweave/web/index.js";
const driver = webDriver["default"] ? webDriver["default"] : webDriver;
export class CryptoDriver extends driver {
    getPublicKey(_jwk) {
        throw new Error("Unimplemented");
    }
}
let driverInstance;
export function getCryptoDriver() {
    return (driverInstance ??= new CryptoDriver());
}
//# sourceMappingURL=webUtils.js.map