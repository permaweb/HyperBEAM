export function createLruCache({ size }: {
    size: any;
}): LRUCache<{}, {}, unknown>;
/**
 * @param {{ cache: LRUCache }} params
 */
export function getByProcessWith({ cache }: {
    cache: LRUCache<any, any, any>;
}): (process: any) => Promise<any>;
/**
 * @param {{ cache: LRUCache }} params
 */
export function setByProcessWith({ cache }: {
    cache: LRUCache<any, any, any>;
}): (process: any, { url, address }: {
    url: any;
    address: any;
}, ttl: any) => Promise<LRUCache<any, any, any>>;
/**
 * @param {{ cache: LRUCache }} params
 */
export function getByOwnerWith({ cache }: {
    cache: LRUCache<any, any, any>;
}): (owner: any) => Promise<any>;
/**
 * @param {{ cache: LRUCache }} params
 */
export function setByOwnerWith({ cache }: {
    cache: LRUCache<any, any, any>;
}): (owner: any, url: any, ttl: any) => Promise<LRUCache<any, any, any>>;
import { LRUCache } from 'lru-cache';
