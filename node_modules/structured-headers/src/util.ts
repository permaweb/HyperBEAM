import { Item, InnerList } from './types';

const asciiRe = /^[\x20-\x7E]*$/;
const tokenRe = /^[a-zA-Z*][:/!#$%&'*+\-.^_`|~A-Za-z0-9]*$/;
const keyRe = /^[a-z*][*\-_.a-z0-9]*$/;

export function isAscii(str: string): boolean {

  return asciiRe.test(str);

}

export function isValidTokenStr(str: string): boolean {

  return tokenRe.test(str);

}

export function isValidKeyStr(str: string): boolean {

  return keyRe.test(str);

}


export function isInnerList(input: Item | InnerList): input is InnerList {

  return Array.isArray(input[0]);

}

export function arrayBufferToBase64(ab: ArrayBuffer): string {

  // Create a Uint8Array to read the ArrayBuffer as bytes
  const bytes = new Uint8Array(ab);
  let binary = '';

  // Convert each byte to a character
  for (const byte of bytes) {
    binary += String.fromCharCode(byte);
  }

  // Encode the binary string as Base64
  return btoa(binary);
}

export function base64ToArrayBuffer(b64: string): ArrayBuffer {

  // Decode the base64 string into a binary string
  const binaryString = atob(b64);

  // Create a new ArrayBuffer with the same length as the binary string
  const len = binaryString.length;
  const bytes = new Uint8Array(len);

  // Convert each character to its corresponding byte
  for (let i = 0; i < len; i++) {
    bytes[i] = binaryString.charCodeAt(i);
  }

  // Return the ArrayBuffer
  return bytes.buffer;

}
