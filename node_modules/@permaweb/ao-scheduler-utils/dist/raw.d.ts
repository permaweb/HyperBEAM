export function rawWith({ loadScheduler, cache }: {
    loadScheduler: any;
    cache: any;
}): (address: string) => Promise<{
    url: string;
} | undefined>;
