import { BareItem, ByteSequence, Dictionary, InnerList, Item, List, Parameters } from './types';
import { Token } from './token';
export declare class SerializeError extends Error {
}
export declare function serializeList(input: List): string;
export declare function serializeDictionary(input: Dictionary): string;
export declare function serializeItem(input: Item): string;
export declare function serializeInnerList(input: InnerList): string;
export declare function serializeBareItem(input: BareItem): string;
export declare function serializeInteger(input: number): string;
export declare function serializeDecimal(input: number): string;
export declare function serializeString(input: string): string;
export declare function serializeBoolean(input: boolean): string;
export declare function serializeByteSequence(input: ByteSequence): string;
export declare function serializeToken(input: Token): string;
export declare function serializeParameters(input: Parameters): string;
export declare function serializeKey(input: string): string;
