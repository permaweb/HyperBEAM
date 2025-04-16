export { stringToBuffer, concatBuffers } from "arweave/web/lib/utils";
export { deepHash } from "./deepHash";
import webDriver from "arweave/web/lib/crypto/webcrypto-driver";
import type { JWKInterface } from "./interface-jwk";
export type { CreateTransactionInterface } from "arweave/web/common";
export { default as Transaction } from "arweave/web/lib/transaction";
export { default as Arweave } from "arweave/web";
declare const driver: typeof webDriver;
export declare class CryptoDriver extends driver {
    getPublicKey(_jwk: JWKInterface): string;
}
export declare function getCryptoDriver(): CryptoDriver;
