export function locateWith({ loadProcessScheduler, loadScheduler, cache, followRedirects, checkForRedirect }: {
    loadProcessScheduler: any;
    loadScheduler: any;
    cache: any;
    followRedirects: any;
    checkForRedirect: any;
}): (process: string, schedulerHint?: string) => Promise<{
    url: string;
    address: string;
}>;
