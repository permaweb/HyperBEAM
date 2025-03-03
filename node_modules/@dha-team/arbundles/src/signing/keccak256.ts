/* eslint-disable @typescript-eslint/explicit-function-return-type */
import { Buffer } from "buffer";
import createKeccakHash from "keccak";

export function keccak256(value: Buffer | bigint | string | number) {
  value = toBuffer(value);
  return createKeccakHash("keccak256")
    .update(value as Buffer)
    .digest();
}

function toBuffer(value: any) {
  if (!Buffer.isBuffer(value)) {
    if (Array.isArray(value)) {
      value = Buffer.from(value);
    } else if (typeof value === "string") {
      if (isHexString(value)) {
        value = Buffer.from(padToEven(stripHexPrefix(value)), "hex");
      } else {
        value = Buffer.from(value);
      }
    } else if (typeof value === "number") {
      value = intToBuffer(value);
    } else if (typeof value === "bigint") {
      value = bigintToBuffer(value);
    } else if (value === null || value === undefined) {
      value = Buffer.allocUnsafe(0);
    } else {
      throw new Error("invalid type");
    }
  }

  return value;
}

function isHexString(value: any, length?: number) {
  if (typeof value !== "string" || !value.match(/^0x[0-9A-Fa-f]*$/)) {
    return false;
  }

  if (length && value.length !== 2 + 2 * length) {
    return false;
  }

  return true;
}

function padToEven(value: any) {
  if (typeof value !== "string") {
    throw new Error(`while padding to even, value must be string, is currently ${typeof value}, while padToEven.`);
  }

  if (value.length % 2) {
    value = `0${value}`;
  }

  return value;
}

function stripHexPrefix(value: any) {
  if (typeof value !== "string") {
    return value;
  }

  return isHexPrefixed(value) ? value.slice(2) : value;
}

function isHexPrefixed(value: any) {
  if (typeof value !== "string") {
    throw new Error("value must be type 'string', is currently type " + typeof value + ", while checking isHexPrefixed.");
  }

  return value.startsWith("0x");
}

function intToBuffer(i: number) {
  const hex = intToHex(i);
  return Buffer.from(padToEven(hex.slice(2)), "hex");
}

function intToHex(i: number) {
  const hex = i.toString(16);
  return `0x${hex}`;
}

function bigintToBuffer(value: bigint) {
  const hex = value.toString(16);
  return Buffer.from(padToEven(hex), "hex");
}

if (typeof window !== "undefined") {
  (window as any).keccak256 = keccak256;
}

export default keccak256;

export const exportForTesting = {
  intToBuffer,
  intToHex,
  isHexPrefixed,
  stripHexPrefix,
  padToEven,
  isHexString,
  toBuffer,
};
