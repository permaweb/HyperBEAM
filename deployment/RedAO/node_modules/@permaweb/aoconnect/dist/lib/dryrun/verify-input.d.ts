/**
 * @typedef Message
 * @property {string} Id
 * @property {string} Target
 * @property {string} Owner
 * @property {string} [Anchor]
 * @property {any} Data
 * @property {Record<name,value>[]} Tags
 *
 * @callback VerifyInput
 * @param {Message} msg
 * @returns {Message}
 *
 * @returns {VerifyInput}
 */
export function verifyInputWith(): VerifyInput;
export type Message = {
    Id: string;
    Target: string;
    Owner: string;
    Anchor?: string;
    Data: any;
    Tags: Record<void, value>[];
};
export type VerifyInput = (msg: Message) => Message;
