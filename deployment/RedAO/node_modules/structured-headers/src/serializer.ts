import {
  BareItem,
  Dictionary,
  DictionaryObject,
  InnerList,
  Item,
  List,
  Parameters,
} from './types.js';

import { Token } from './token.js';

import { isAscii, isInnerList, isValidKeyStr, arrayBufferToBase64 } from './util.js';
import { DisplayString } from './displaystring.js';

export class SerializeError extends Error {}

export function serializeList(input: List): string {

  return input.map(value => {

    if (isInnerList(value)) {
      return serializeInnerList(value);
    } else {
      return serializeItem(value);
    }

  }).join(', ');

}

export function serializeDictionary(input: Dictionary | DictionaryObject): string {

  const entries: Iterable<[string, BareItem|Item|InnerList]> = input instanceof Map ? input.entries() : Object.entries(input);

  return Array.from(entries).map(([key, entry]) => {

    const keyStr = serializeKey(key);

    if (Array.isArray(entry)) {
      if (entry[0]===true) {
        return keyStr + serializeParameters(entry[1]);
      } else {
        if (isInnerList(entry)) {
          return keyStr + '=' + serializeInnerList(entry);
        } else {
          return keyStr + '=' + serializeItem(entry);
        }
      }
    } else {
      if (entry===true) {
        return keyStr;
      } else {
        return keyStr + '=' + serializeBareItem(entry);
      }
    }

  }).join(', ');

}

/**
 * Serialize a Structured Fields Item.
 *
 * An Item is a standalone value like a string, number of date, followed by
 * an optional set of parameters.
 *
 * You can either pass the value in the first argument and parameters in the second, or pass both as a tuple. The later exists for symmetry with parseItem.
 */
export function serializeItem(input: Item): string;
export function serializeItem(input: BareItem, params?: Parameters): string;
export function serializeItem(input: Item|BareItem, params?: Parameters): string {

  if (Array.isArray(input)) {
    return serializeBareItem(input[0]) + serializeParameters(input[1]);
  } else {
    return serializeBareItem(input) + (params?serializeParameters(params):'');
  }

}

export function serializeInnerList(input: InnerList): string {

  return `(${input[0].map(value => serializeItem(value)).join(' ')})${serializeParameters(input[1])}`;

}


export function serializeBareItem(input: BareItem): string {
  if (typeof input === 'number') {
    if (Number.isInteger(input)) {
      return serializeInteger(input);
    }
    return serializeDecimal(input);
  }
  if (typeof input === 'string') {
    return serializeString(input);
  }
  if (input instanceof Token) {
    return serializeToken(input);
  }
  if (input instanceof ArrayBuffer) {
    return serializeByteSequence(input);
  }
  if (input instanceof DisplayString) {
    return serializeDisplayString(input);
  }
  if (input instanceof Date) {
    return serializeDate(input);
  }
  if (typeof input === 'boolean') {
    return serializeBoolean(input);
  }
  throw new SerializeError(`Cannot serialize values of type ${typeof input}`);
}

export function serializeInteger(input: number): string {

  if (input < -999_999_999_999_999 || input > 999_999_999_999_999) {
    throw new SerializeError('Structured headers can only encode integers in the range range of -999,999,999,999,999 to 999,999,999,999,999 inclusive');
  }
  return input.toString();
}

export function serializeDecimal(input: number): string {
  const out = input.toFixed(3).replace(/0+$/,'');
  const signifantDigits = out.split('.')[0].replace('-','').length;

  if (signifantDigits > 12) {
    throw new SerializeError('Fractional numbers are not allowed to have more than 12 significant digits before the decimal point');
  }
  return out;
}

export function serializeString(input: string): string {
  if (!isAscii(input)) {
    throw new SerializeError('Only ASCII strings may be serialized');
  }
  return `"${input.replace(/("|\\)/g, (v) => '\\' + v)}"`;
}

export function serializeDisplayString(input: DisplayString): string {
  let out = '%"';
  const textEncoder = new TextEncoder();
  for (const char of textEncoder.encode(input.toString())) {
    if (
      char === 0x25 // %
      || char === 0x22 // "
      || char <= 0x1f
      || char >= 0x7f
    ) {
      out += '%' + char.toString(16);
    } else {
      out += String.fromCharCode(char);
    }
  }
  return out + '"';
}

export function serializeBoolean(input: boolean): string {
  return input ? '?1' : '?0';
}

export function serializeByteSequence(input: ArrayBuffer): string {
  return `:${arrayBufferToBase64(input)}:`;
}

export function serializeToken(input: Token): string {
  return input.toString();
}

export function serializeDate(input: Date): string {
  return '@' + Math.floor(input.getTime()/1000);
}

export function serializeParameters(input: Parameters): string {

  return Array.from(input).map(([key, value]) => {

    let out = ';' + serializeKey(key);
    if (value!==true) {
      out+='=' + serializeBareItem(value);
    }
    return out;

  }).join('');

}

export function serializeKey(input: string): string {

  if (!isValidKeyStr(input)) {
    throw new SerializeError('Keys in dictionaries must only contain lowercase letter, numbers, _-*. and must start with a letter or *');
  }
  return input;

}
