/**
 * @typedef Env3
 * @property {fetch} fetch
 * @property {string} MU_URL
 *
 * @typedef WriteMessageTx
 * @property { any } signedData - DataItem returned from arbundles createData
 *
 * @typedef WriteMessage2Args
 * @property {WriteMessageTx} transaction
 *
 * @callback WriteMessage2
 * @param {WriteMessage2Args} args
 * @returns {Promise<Record<string, any>}
 *
 * @param {Env3} env
 * @returns {WriteMessage2}
 */
export function deployMessageWith({ fetch, MU_URL, logger: _logger }: Env3): WriteMessage2;
/**
 * @typedef Env3
 * @property {fetch} fetch
 * @property {string} MU_URL
 *
 * @typedef RegisterProcess
 * @property { any } signedData - DataItem returned from arbundles createData
 *
 * @callback RegisterProcess
 * @returns {Promise<Record<string, any>}
 *
 * @param {Env3} env
 * @returns {RegisterProcess}
 */
export function deployProcessWith({ fetch, MU_URL, logger: _logger }: Env3): RegisterProcess;
/**
 * @typedef Env4
 * @property {fetch} fetch
 * @property {string} MU_URL
 * @property {Logger} logger
 *
 * @callback MonitorResult
 * @returns {Promise<Record<string, any>}
 * @param {Env4} env
 * @returns {MonitorResult}
 */
export function deployMonitorWith({ fetch, MU_URL, logger: _logger }: Env4): MonitorResult;
/**
 * @typedef Env5
 * @property {fetch} fetch
 * @property {string} MU_URL
 * @property {Logger} logger
 *
 * @callback MonitorResult
 * @returns {Promise<Record<string, any>}
 * @param {Env5} env
 * @returns {MonitorResult}
 */
export function deployUnmonitorWith({ fetch, MU_URL, logger: _logger }: Env5): MonitorResult;
/**
 * @typedef Env6
 * @property {fetch} fetch
 * @property {string} MU_URL
 *
 *
 * @typedef WriteAssignArgs
 * @property {string} process
 * @property {string} message
 * @property {string[]} [exclude]
 * @property {boolean} [baseLayer]
 *
 * @callback WriteAssign
 * @param {WriteAssignArgs} args
 * @returns {Promise<Record<string, any>}
 *
 * @param {Env6} env
 * @returns {WriteAssign}
 */
export function deployAssignWith({ fetch, MU_URL, logger: _logger }: Env6): WriteAssign;
export type Env3 = {
    fetch: typeof globalThis.fetch;
    MU_URL: string;
};
export type WriteMessageTx = {
    /**
     * - DataItem returned from arbundles createData
     */
    signedData: any;
};
export type WriteMessage2Args = {
    transaction: WriteMessageTx;
};
export type WriteMessage2 = (args: WriteMessage2Args) => Promise<Record<string, any>>;
export type RegisterProcess = {
    /**
     * - DataItem returned from arbundles createData
     */
    signedData: any;
};
export type Env4 = {
    fetch: typeof globalThis.fetch;
    MU_URL: string;
    logger: Logger;
};
export type MonitorResult = () => Promise<Record<string, any>>;
export type Env5 = {
    fetch: typeof globalThis.fetch;
    MU_URL: string;
    logger: Logger;
};
export type Env6 = {
    fetch: typeof globalThis.fetch;
    MU_URL: string;
};
export type WriteAssignArgs = {
    process: string;
    message: string;
    exclude?: string[];
    baseLayer?: boolean;
};
export type WriteAssign = (args: WriteAssignArgs) => Promise<Record<string, any>>;
