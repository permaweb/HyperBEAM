/**
 * @typedef Env
 * @property {DryrunFetch} dryrunFetch
 *
 * @typedef Message
 * @property {string} Id
 * @property {string} Target
 * @property {string} Owner
 * @property {string} [Anchor]
 * @property {any} Data
 * @property {Record<name,value>[]} Tags
 *
 * @callback Run
 * @param {Message} msg
 *
 * @param {Env} env
 * @returns {Run}
 */
export function runWith({ dryrunFetch }: {
    dryrunFetch: any;
}): (...args: any[]) => {
    fork: any;
    toPromise: () => any;
    map: (fn: any) => any;
    bimap: (f: any, g: any) => any;
    chain: (fn: any) => any;
    bichain: (f: any, g: any) => any;
    fold: (f: any, g: any) => any;
};
export type Env = {
    dryrunFetch: DryrunFetch;
};
export type Message = {
    Id: string;
    Target: string;
    Owner: string;
    Anchor?: string;
    Data: any;
    Tags: Record<void, value>[];
};
export type Run = (msg: Message, env: Env) => Run;
