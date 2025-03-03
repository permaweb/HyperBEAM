"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.getCryptoDriver = exports.CryptoDriver = exports.Arweave = exports.Transaction = exports.deepHash = exports.concatBuffers = exports.stringToBuffer = void 0;
var utils_1 = require("arweave/web/lib/utils");
Object.defineProperty(exports, "stringToBuffer", { enumerable: true, get: function () { return utils_1.stringToBuffer; } });
Object.defineProperty(exports, "concatBuffers", { enumerable: true, get: function () { return utils_1.concatBuffers; } });
var deepHash_1 = require("./deepHash");
Object.defineProperty(exports, "deepHash", { enumerable: true, get: function () { return deepHash_1.deepHash; } });
const webcrypto_driver_1 = __importDefault(require("arweave/web/lib/crypto/webcrypto-driver"));
var transaction_1 = require("arweave/web/lib/transaction");
Object.defineProperty(exports, "Transaction", { enumerable: true, get: function () { return __importDefault(transaction_1).default; } });
var web_1 = require("arweave/web");
Object.defineProperty(exports, "Arweave", { enumerable: true, get: function () { return __importDefault(web_1).default; } });
const driver = webcrypto_driver_1.default["default"] ? webcrypto_driver_1.default["default"] : webcrypto_driver_1.default;
class CryptoDriver extends driver {
    getPublicKey(_jwk) {
        throw new Error("Unimplemented");
    }
}
exports.CryptoDriver = CryptoDriver;
let driverInstance;
function getCryptoDriver() {
    return (driverInstance !== null && driverInstance !== void 0 ? driverInstance : (driverInstance = new CryptoDriver()));
}
exports.getCryptoDriver = getCryptoDriver;
//# sourceMappingURL=webUtils.js.map