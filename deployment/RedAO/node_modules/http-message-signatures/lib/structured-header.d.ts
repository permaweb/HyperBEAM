export declare class Dictionary {
    private readonly parsed;
    private readonly raw;
    constructor(input: string);
    toString(): string;
    serialize(): string;
    has(key: string): boolean;
    get(key: string): string | undefined;
}
export declare class List {
    private readonly parsed;
    private readonly raw;
    constructor(input: string);
    toString(): string;
    serialize(): string;
}
export declare class Item {
    private readonly parsed;
    private readonly raw;
    constructor(input: string);
    toString(): string;
    serialize(): string;
}
export declare function parseHeader(header: string): List | Dictionary | Item;
/**
 * This allows consumers of the library to supply field specifications that aren't
 * strictly "structured fields". Really a string must start with a `"` but that won't
 * tend to happen in our configs.
 *
 * @param {string} input
 * @returns {string}
 */
export declare function quoteString(input: string): string;
