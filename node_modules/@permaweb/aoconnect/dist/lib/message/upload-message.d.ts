/**
 * @callback BuildTx
 * @param {Context3} ctx
 * @returns {Async<Context3>}
 *
 * @param {Env6} env
 * @returns {BuildTx}
 */
export function uploadMessageWith(env: Env6): BuildTx;
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
export type BuildTags = (ctx: Context3) => Context3;
export type BuildData = (ctx: Context3) => Context3;
export type BuildTx = (ctx: Context3) => Async<Context3>;
