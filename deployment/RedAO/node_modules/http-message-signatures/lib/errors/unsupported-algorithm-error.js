"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.UnsupportedAlgorithmError = void 0;
const verification_error_1 = require("./verification-error");
/**
 * Thrown when a key is presented to verify a signature with
 * an algorithm that is not supported
 */
class UnsupportedAlgorithmError extends verification_error_1.VerificationError {
}
exports.UnsupportedAlgorithmError = UnsupportedAlgorithmError;
//# sourceMappingURL=unsupported-algorithm-error.js.map