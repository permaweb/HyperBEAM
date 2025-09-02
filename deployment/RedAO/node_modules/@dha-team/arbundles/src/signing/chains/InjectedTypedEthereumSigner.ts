import { SignatureConfig, SIG_CONFIG } from "../../constants";
import { verifyTypedData } from "@ethersproject/wallet";
import { domain, types } from "./TypedEthereumSigner";
// import type { TypedDataDomain, TypedDataField } from "@ethersproject/abstract-signer";
import type { Signer } from "../index";

// EIP-712 Typed Data
// See: https://eips.ethereum.org/EIPS/eip-712

export type Bytes = ArrayLike<number>;

export type BytesLike = Bytes | string;

export type BigNumberish = /* BigNumber */ Bytes | bigint | string | number;

export interface TypedDataDomain {
  name?: string;
  version?: string;
  chainId?: BigNumberish;
  verifyingContract?: string;
  salt?: BytesLike;
}

export interface TypedDataField {
  name: string;
  type: string;
}

export interface InjectedTypedEthereumSignerMinimalSigner {
  getAddress: () => Promise<string>;
  _signTypedData(domain: TypedDataDomain, types: Record<string, TypedDataField[]>, value: Record<string, any>): Promise<string>;
}

export interface InjectedTypedEthereumSignerMinimalProvider {
  getSigner(): InjectedTypedEthereumSignerMinimalSigner;
}

export class InjectedTypedEthereumSigner implements Signer {
  readonly ownerLength: number = SIG_CONFIG[SignatureConfig.TYPEDETHEREUM].pubLength;
  readonly signatureLength: number = SIG_CONFIG[SignatureConfig.TYPEDETHEREUM].sigLength;
  readonly signatureType: SignatureConfig = SignatureConfig.TYPEDETHEREUM;
  private address: string;
  protected signer: InjectedTypedEthereumSignerMinimalSigner;
  public publicKey: Buffer;

  constructor(provider: InjectedTypedEthereumSignerMinimalProvider) {
    this.signer = provider.getSigner();
  }
  async ready(): Promise<void> {
    this.address = (await this.signer.getAddress()).toString().toLowerCase();
    this.publicKey = Buffer.from(this.address); // pk *is* address
  }

  async sign(message: Uint8Array): Promise<Uint8Array> {
    const signature = await this.signer._signTypedData(domain, types, {
      address: this.address,
      "Transaction hash": message,
    });

    return Buffer.from(signature.slice(2), "hex"); // trim leading 0x, convert to hex.
  }

  static verify(pk: string | Buffer, message: Uint8Array, signature: Uint8Array): boolean {
    const address = pk.toString();
    const addr = verifyTypedData(domain, types, { address, "Transaction hash": message }, signature);
    return address.toLowerCase() === addr.toLowerCase();
  }
}
export default InjectedTypedEthereumSigner;
