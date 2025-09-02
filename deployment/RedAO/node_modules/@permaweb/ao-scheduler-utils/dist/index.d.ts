export * from "./index.common.js";
export const locate: (process: string, schedulerHint?: string) => Promise<{
    url: string;
    address: string;
}>;
export const validate: (address: string) => Promise<boolean>;
export const raw: (address: string) => Promise<{
    url: string;
}>;
