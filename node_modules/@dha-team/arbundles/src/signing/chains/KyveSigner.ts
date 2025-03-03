import { EthereumSigner } from "./";
import { SignatureConfig } from "../../constants";

export default class KyveSigner extends EthereumSigner {
  readonly signatureType: SignatureConfig = SignatureConfig.KYVE;
}
