export function validateWith({ loadScheduler, cache }: {
    loadScheduler: any;
    cache: any;
}): (address: string) => Promise<boolean>;
