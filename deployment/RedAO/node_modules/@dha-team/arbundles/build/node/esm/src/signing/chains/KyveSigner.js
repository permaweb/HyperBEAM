import { EthereumSigner } from "./index.js";
import { SignatureConfig } from "../../constants.js";
export default class KyveSigner extends EthereumSigner {
    signatureType = SignatureConfig.KYVE;
}
//# sourceMappingURL=KyveSigner.js.map