/**
 * @typedef MessageResult
 * @property {any} Output
 * @property {any[]} Messages
 * @property {any[]} Spawns
 * @property {any} [Error]
 *
 * @typedef ReadResultArgs
 * @property {string} message - the transaction id of the message
 * @property {string} process - the transaction id of the process that received the message
 *
 * @callback ReadResult
 * @param {ReadResultArgs} args
 * @returns {Promise<MessageResult>} result
 *
 * @returns {ReadResult}
 */
export function resultWith(env: any): ReadResult;
export type MessageResult = {
    Output: any;
    Messages: any[];
    Spawns: any[];
    Error?: any;
};
export type ReadResultArgs = {
    /**
     * - the transaction id of the message
     */
    message: string;
    /**
     * - the transaction id of the process that received the message
     */
    process: string;
};
export type ReadResult = (args: ReadResultArgs) => Promise<MessageResult>;
