"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.concatBuffers = concatBuffers;
exports.b64UrlToString = b64UrlToString;
exports.bufferToString = bufferToString;
exports.stringToBuffer = stringToBuffer;
exports.stringToB64Url = stringToB64Url;
exports.b64UrlToBuffer = b64UrlToBuffer;
exports.bufferTob64 = bufferTob64;
exports.bufferTob64Url = bufferTob64Url;
exports.b64UrlEncode = b64UrlEncode;
exports.b64UrlDecode = b64UrlDecode;
const B64js = __importStar(require("base64-js"));
function concatBuffers(buffers) {
    let total_length = 0;
    for (let i = 0; i < buffers.length; i++) {
        total_length += buffers[i].byteLength;
    }
    let temp = new Uint8Array(total_length);
    let offset = 0;
    temp.set(new Uint8Array(buffers[0]), offset);
    offset += buffers[0].byteLength;
    for (let i = 1; i < buffers.length; i++) {
        temp.set(new Uint8Array(buffers[i]), offset);
        offset += buffers[i].byteLength;
    }
    return temp;
}
function b64UrlToString(b64UrlString) {
    let buffer = b64UrlToBuffer(b64UrlString);
    return bufferToString(buffer);
}
function bufferToString(buffer) {
    return new TextDecoder("utf-8", { fatal: true }).decode(buffer);
}
function stringToBuffer(string) {
    return new TextEncoder().encode(string);
}
function stringToB64Url(string) {
    return bufferTob64Url(stringToBuffer(string));
}
function b64UrlToBuffer(b64UrlString) {
    return new Uint8Array(B64js.toByteArray(b64UrlDecode(b64UrlString)));
}
function bufferTob64(buffer) {
    return B64js.fromByteArray(new Uint8Array(buffer));
}
function bufferTob64Url(buffer) {
    return b64UrlEncode(bufferTob64(buffer));
}
function b64UrlEncode(b64UrlString) {
    try {
        return b64UrlString
            .replace(/\+/g, "-")
            .replace(/\//g, "_")
            .replace(/\=/g, "");
    }
    catch (error) {
        throw new Error("Failed to encode string", { cause: error });
    }
}
function b64UrlDecode(b64UrlString) {
    try {
        b64UrlString = b64UrlString.replace(/\-/g, "+").replace(/\_/g, "/");
        let padding;
        b64UrlString.length % 4 == 0
            ? (padding = 0)
            : (padding = 4 - (b64UrlString.length % 4));
        return b64UrlString.concat("=".repeat(padding));
    }
    catch (error) {
        throw new Error("Failed to decode string", { cause: error });
    }
}
//# sourceMappingURL=utils.js.map