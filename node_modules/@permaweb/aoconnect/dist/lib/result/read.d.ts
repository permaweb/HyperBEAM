/**
 * @typedef Env
 * @property {any} loadState
 *
 * @typedef Context
 * @property {string} id - the transaction id of the process being read
 *
 * @callback Read
 * @param {Context} ctx
 * @returns {Async<Record<string, any>>}
 *
 * @param {Env} env
 * @returns {Read}
 */
export function readWith({ loadResult }: Env): Read;
export type Env = {
    loadState: any;
};
export type Context = {
    /**
     * - the transaction id of the process being read
     */
    id: string;
};
export type Read = (ctx: Context) => Async<Record<string, any>>;
