"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const _1 = require("./");
const constants_1 = require("../../constants");
class KyveSigner extends _1.EthereumSigner {
    constructor() {
        super(...arguments);
        this.signatureType = constants_1.SignatureConfig.KYVE;
    }
}
exports.default = KyveSigner;
//# sourceMappingURL=KyveSigner.js.map