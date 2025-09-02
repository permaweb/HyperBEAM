export { stringToBuffer, concatBuffers } from "arweave/node/lib/utils";
export { deepHash } from "./deepHash";
import { default as nodeDriver } from "arweave/node/lib/crypto/node-driver";
import type { JWKInterface } from "./interface-jwk";
export type { CreateTransactionInterface } from "arweave/node/common";
export { default as Transaction } from "arweave/node/lib/transaction";
export { default as Arweave } from "arweave/node";
declare const driver: typeof nodeDriver;
export declare class CryptoDriver extends driver {
    getPublicKey(jwk: JWKInterface): string;
}
export declare function getCryptoDriver(): CryptoDriver;
