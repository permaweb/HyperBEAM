import { VerificationError } from './verification-error';
/**
 * Thrown when a key is presented to verify a signature with
 * an algorithm that is not supported
 */
export declare class UnsupportedAlgorithmError extends VerificationError {
}
