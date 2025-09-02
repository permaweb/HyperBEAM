/**
 * @typedef Env3
 * @property {fetch} fetch
 * @property {string} CU_URL
 */
/**
 * @typedef Message
 * @property {string} Id
 * @property {string} Target
 * @property {string} Owner
 * @property {string} [Anchor]
 * @property {any} Data
 * @property {Record<name,value>[]} Tags
 *
 * @typedef Result
 * @property {any} Output
 * @property {Message[]} Messages
 * @property {Message[]} Spawns
 * @property {string} [Error]
 *
 * @callback DryrunFetch
 * @param {Message} msg
 * @returns {Result}
 *
 * @param {Env3} env
 * @returns {DryrunFetch}
 */
export function dryrunFetchWith({ fetch, CU_URL, logger }: Env3): DryrunFetch;
/**
 *
 * @typedef LoadResultArgs
 * @property {string} id - the id of the process being read
 *
 * @callback LoadResult
 * @param {LoadResultArgs} args
 * @returns {Promise<Record<string, any>}
 *
 * @param {Env3} env
 * @returns {LoadResult}
 */
export function loadResultWith({ fetch, CU_URL, logger }: Env3): LoadResult;
/**
 * @typedef Env3
 * @property {fetch} fetch
 * @property {string} CU_URL
 *
 * @typedef QueryResultsArgs
 * @property {string} process - the id of the process being read
 * @property {string} from - cursor to start the list of results
 * @property {string} to - cursor to stop the list of results
 * @property {string} sort - "ASC" or "DESC" to describe the order of list
 * @property {number} limit - the number of results to return
 *
 * @callback QueryResults
 * @param {QueryResultsArgs} args
 * @returns {Promise<Record<string, any>}
 *
 * @param {Env3} env
 * @returns {QueryResults}
 */
export function queryResultsWith({ fetch, CU_URL, logger }: Env3): QueryResults;
export type Env3 = {
    fetch: typeof globalThis.fetch;
    CU_URL: string;
};
export type Message = {
    Id: string;
    Target: string;
    Owner: string;
    Anchor?: string;
    Data: any;
    Tags: Record<void, value>[];
};
export type Result = {
    Output: any;
    Messages: Message[];
    Spawns: Message[];
    Error?: string;
};
export type DryrunFetch = (msg: Message) => Result;
export type LoadResultArgs = {
    /**
     * - the id of the process being read
     */
    id: string;
};
export type LoadResult = (args: LoadResultArgs) => Promise<Record<string, any>>;
export type QueryResultsArgs = {
    /**
     * - the id of the process being read
     */
    process: string;
    /**
     * - cursor to start the list of results
     */
    from: string;
    /**
     * - cursor to stop the list of results
     */
    to: string;
    /**
     * - "ASC" or "DESC" to describe the order of list
     */
    sort: string;
    /**
     * - the number of results to return
     */
    limit: number;
};
export type QueryResults = (args: QueryResultsArgs) => Promise<Record<string, any>>;
