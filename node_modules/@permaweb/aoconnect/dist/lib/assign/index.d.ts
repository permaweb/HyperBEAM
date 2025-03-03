/**
 * @typedef Env1
 *
 * @typedef AssignArgs
 * @property {string} process
 * @property {string} message
 * @property {string[]} [exclude]
 * @property {boolean} [baseLayer]
 *
 * @callback Assign
 * @param {AssignArgs} args
 * @returns {Promise<string>} the id of the data item that represents this assignment
 *
 * @param {Env1} - the environment
 * @returns {Assign}
 */
export function assignWith(env: any): Assign;
export type Env1 = any;
export type AssignArgs = {
    process: string;
    message: string;
    exclude?: string[];
    baseLayer?: boolean;
};
export type Assign = (args: AssignArgs) => Promise<string>;
