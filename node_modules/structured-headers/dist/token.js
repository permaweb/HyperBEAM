import { isValidTokenStr } from './util.js';
export class Token {
    constructor(value) {
        if (!isValidTokenStr(value)) {
            throw new TypeError('Invalid character in Token string. Tokens must start with *, A-Z and the rest of the string may only contain a-z, A-Z, 0-9, :/!#$%&\'*+-.^_`|~');
        }
        this.value = value;
    }
    toString() {
        return this.value;
    }
}
//# sourceMappingURL=token.js.map