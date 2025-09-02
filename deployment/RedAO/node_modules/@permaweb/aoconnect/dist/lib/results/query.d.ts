/**
 * @typedef Env
 * @property {any} loadState
 *
 * @typedef Context
 * @property {string} id - the transaction id of the process being read
 *
 * @callback Query
 * @param {Context} ctx
 * @returns {Async<Record<string, any>>}
 *
 * @param {Env} env
 * @returns {Query}
 */
export function queryWith({ queryResults }: Env): Query;
export type Env = {
    loadState: any;
};
export type Context = {
    /**
     * - the transaction id of the process being read
     */
    id: string;
};
export type Query = (ctx: Context) => Async<Record<string, any>>;
