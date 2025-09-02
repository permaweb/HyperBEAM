export const checkForRedirectSchema: z.ZodFunction<z.ZodTuple<[z.ZodString, z.ZodString], z.ZodUnknown>, z.ZodPromise<z.ZodString>>;
export const getByProcessSchema: z.ZodFunction<z.ZodTuple<[z.ZodString], z.ZodUnknown>, z.ZodPromise<z.ZodOptional<z.ZodNullable<z.ZodObject<{
    url: z.ZodString;
    address: z.ZodString;
}, "strip", z.ZodTypeAny, {
    url?: string;
    address?: string;
}, {
    url?: string;
    address?: string;
}>>>>>;
export const setByProcessSchema: z.ZodFunction<z.ZodTuple<[z.ZodString, z.ZodObject<{
    url: z.ZodString;
    address: z.ZodString;
}, "strip", z.ZodTypeAny, {
    url?: string;
    address?: string;
}, {
    url?: string;
    address?: string;
}>, z.ZodNumber], z.ZodUnknown>, z.ZodPromise<z.ZodAny>>;
export const getByOwnerSchema: z.ZodFunction<z.ZodTuple<[z.ZodString], z.ZodUnknown>, z.ZodPromise<z.ZodOptional<z.ZodNullable<z.ZodObject<{
    url: z.ZodString;
    address: z.ZodString;
    ttl: z.ZodNumber;
}, "strip", z.ZodTypeAny, {
    url?: string;
    ttl?: number;
    address?: string;
}, {
    url?: string;
    ttl?: number;
    address?: string;
}>>>>>;
export const setByOwnerSchema: z.ZodFunction<z.ZodTuple<[z.ZodString, z.ZodString, z.ZodNumber], z.ZodUnknown>, z.ZodPromise<z.ZodAny>>;
export const loadSchedulerSchema: z.ZodFunction<z.ZodTuple<[z.ZodString], z.ZodUnknown>, z.ZodPromise<z.ZodObject<{
    url: z.ZodString;
    address: z.ZodString;
    ttl: z.ZodNumber;
}, "strip", z.ZodTypeAny, {
    url?: string;
    ttl?: number;
    address?: string;
}, {
    url?: string;
    ttl?: number;
    address?: string;
}>>>;
export const loadProcessSchedulerSchema: z.ZodFunction<z.ZodTuple<[z.ZodString], z.ZodUnknown>, z.ZodPromise<z.ZodObject<{
    url: z.ZodString;
    address: z.ZodString;
    ttl: z.ZodNumber;
}, "strip", z.ZodTypeAny, {
    url?: string;
    ttl?: number;
    address?: string;
}, {
    url?: string;
    ttl?: number;
    address?: string;
}>>>;
import { z } from 'zod';
