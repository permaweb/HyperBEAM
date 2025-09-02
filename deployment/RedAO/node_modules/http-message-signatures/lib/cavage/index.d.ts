import { Request, Response, SignConfig, VerifyConfig } from '../types';
/**
 * Components can be derived from requests or responses (which can also be bound to their request).
 * The signature is essentially (component, signingSubject, supplementaryData)
 *
 * @todo - Allow consumers to register their own component parser somehow
 */
export declare function deriveComponent(component: string, message: Request | Response): string[];
export declare function extractHeader(header: string, { headers }: Request | Response): string[];
export declare function formatSignatureBase(base: [string, string[]][]): string;
export declare function createSigningParameters(config: SignConfig): Map<string, string | number>;
export declare function createSignatureBase(fields: string[], message: Request | Response, signingParameters: Map<string, string | number>): [string, string[]][];
export declare function signMessage<T extends Request | Response = Request | Response>(config: SignConfig, message: T): Promise<T>;
export declare function verifyMessage(config: VerifyConfig, message: Request | Response): Promise<boolean | null>;
