/**
 * @typedef Context
 * @property {string} module - the id of the module source
 * @property {Function} sign - the signer used to sign the process
 * @property {Tag[]} tags - the additional tags to add to the process
 *
 * @typedef Wallet
 * @property {any} wallet - the read wallet
 *
 * @typedef { Context & Wallet } Result
 *
 * @callback VerifyInputs
 * @param {Context} args
 * @returns {Async<Result>}
 *
 * @param {Env} env
 * @returns {VerifyInputs}
 */
export function verifyInputsWith(env: Env): VerifyInputs;
export type Tag = {
    name: string;
    value: any;
};
export type LoadTransactionMeta = (id: string) => Async<any>;
export type Env = {
    loadTransactionMeta: LoadTransactionMeta;
    logger: any;
};
export type Context = {
    /**
     * - the id of the module source
     */
    module: string;
    /**
     * - the signer used to sign the process
     */
    sign: Function;
    /**
     * - the additional tags to add to the process
     */
    tags: Tag[];
};
export type Wallet = {
    /**
     * - the read wallet
     */
    wallet: any;
};
export type Result = Context & Wallet;
export type VerifyInputs = (args: Context) => Async<Result>;
