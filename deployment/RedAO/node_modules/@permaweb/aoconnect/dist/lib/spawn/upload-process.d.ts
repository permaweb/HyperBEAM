/**
 * @callback UploadContract
 * @param {Context3} ctx
 * @returns {Async<Context3>}
 *
 * @param {Env6} env
 * @returns {UploadContract}
 */
export function uploadProcessWith(env: Env6): UploadContract;
export type Tag = {
    name: string;
    value: any;
};
export type Context3 = {
    /**
     * - the id of the transactions that contains the xontract source
     */
    module: string;
    /**
     * -the initialState of the contract
     */
    initialState: any;
    tags: Tag[];
    data?: string | ArrayBuffer;
};
export type Env6 = {
    upload: any;
};
export type UploadContract = (ctx: Context3) => Async<Context3>;
