/**
 * @typedef Tag3
 * @property {string} name
 * @property {any} value
 *
 * @typedef Context3
 * @property {string} id - the transaction id to be verified
 * @property {any} input
 * @property {any} wallet
 * @property {Tag3[]} tags
 *
 * @typedef Env6
 * @property {any} mu
 */
/**
 * @callback BuildTx
 * @param {Context3} ctx
 * @returns {Async<Context3>}
 *
 * @param {Env6} env
 * @returns {BuildTx}
 */
export function uploadUnmonitorWith(env: Env6): BuildTx;
export type Tag3 = {
    name: string;
    value: any;
};
export type Context3 = {
    /**
     * - the transaction id to be verified
     */
    id: string;
    input: any;
    wallet: any;
    tags: Tag3[];
};
export type Env6 = {
    mu: any;
};
export type BuildTx = (ctx: Context3) => Async<Context3>;
