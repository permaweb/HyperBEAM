import { Token } from './token.js';
import { isAscii, isInnerList, isValidKeyStr, arrayBufferToBase64 } from './util.js';
import { DisplayString } from './displaystring.js';
export class SerializeError extends Error {
}
export function serializeList(input) {
    return input.map(value => {
        if (isInnerList(value)) {
            return serializeInnerList(value);
        }
        else {
            return serializeItem(value);
        }
    }).join(', ');
}
export function serializeDictionary(input) {
    const entries = input instanceof Map ? input.entries() : Object.entries(input);
    return Array.from(entries).map(([key, entry]) => {
        const keyStr = serializeKey(key);
        if (Array.isArray(entry)) {
            if (entry[0] === true) {
                return keyStr + serializeParameters(entry[1]);
            }
            else {
                if (isInnerList(entry)) {
                    return keyStr + '=' + serializeInnerList(entry);
                }
                else {
                    return keyStr + '=' + serializeItem(entry);
                }
            }
        }
        else {
            if (entry === true) {
                return keyStr;
            }
            else {
                return keyStr + '=' + serializeBareItem(entry);
            }
        }
    }).join(', ');
}
export function serializeItem(input, params) {
    if (Array.isArray(input)) {
        return serializeBareItem(input[0]) + serializeParameters(input[1]);
    }
    else {
        return serializeBareItem(input) + (params ? serializeParameters(params) : '');
    }
}
export function serializeInnerList(input) {
    return `(${input[0].map(value => serializeItem(value)).join(' ')})${serializeParameters(input[1])}`;
}
export function serializeBareItem(input) {
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
export function serializeInteger(input) {
    if (input < -999999999999999 || input > 999999999999999) {
        throw new SerializeError('Structured headers can only encode integers in the range range of -999,999,999,999,999 to 999,999,999,999,999 inclusive');
    }
    return input.toString();
}
export function serializeDecimal(input) {
    const out = input.toFixed(3).replace(/0+$/, '');
    const signifantDigits = out.split('.')[0].replace('-', '').length;
    if (signifantDigits > 12) {
        throw new SerializeError('Fractional numbers are not allowed to have more than 12 significant digits before the decimal point');
    }
    return out;
}
export function serializeString(input) {
    if (!isAscii(input)) {
        throw new SerializeError('Only ASCII strings may be serialized');
    }
    return `"${input.replace(/("|\\)/g, (v) => '\\' + v)}"`;
}
export function serializeDisplayString(input) {
    let out = '%"';
    const textEncoder = new TextEncoder();
    for (const char of textEncoder.encode(input.toString())) {
        if (char === 0x25 // %
            || char === 0x22 // "
            || char <= 0x1f
            || char >= 0x7f) {
            out += '%' + char.toString(16);
        }
        else {
            out += String.fromCharCode(char);
        }
    }
    return out + '"';
}
export function serializeBoolean(input) {
    return input ? '?1' : '?0';
}
export function serializeByteSequence(input) {
    return `:${arrayBufferToBase64(input)}:`;
}
export function serializeToken(input) {
    return input.toString();
}
export function serializeDate(input) {
    return '@' + Math.floor(input.getTime() / 1000);
}
export function serializeParameters(input) {
    return Array.from(input).map(([key, value]) => {
        let out = ';' + serializeKey(key);
        if (value !== true) {
            out += '=' + serializeBareItem(value);
        }
        return out;
    }).join('');
}
export function serializeKey(input) {
    if (!isValidKeyStr(input)) {
        throw new SerializeError('Keys in dictionaries must only contain lowercase letter, numbers, _-*. and must start with a letter or *');
    }
    return input;
}
//# sourceMappingURL=serializer.js.map