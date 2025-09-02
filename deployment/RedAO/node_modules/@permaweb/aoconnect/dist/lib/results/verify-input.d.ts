/**
 * @callback VerifyInput
 *
 * @returns {VerifyInput}
 */
export function verifyInputWith(): (ctx: any) => any;
export type VerifyInput = () => VerifyInput;
