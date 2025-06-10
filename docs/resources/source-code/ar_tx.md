# [Module ar_tx.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/ar_tx.erl)




The module with utilities for transaction creation, signing, and verification.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#collect_validation_results-2">collect_validation_results/2*</a></td><td></td></tr><tr><td valign="top"><a href="#do_verify-2">do_verify/2*</a></td><td>Verify transaction.</td></tr><tr><td valign="top"><a href="#json_struct_to_tx-1">json_struct_to_tx/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td>Create a new transaction.</td></tr><tr><td valign="top"><a href="#new-5">new/5</a></td><td></td></tr><tr><td valign="top"><a href="#sign-2">sign/2</a></td><td>Cryptographically sign (claim ownership of) a transaction.</td></tr><tr><td valign="top"><a href="#signature_data_segment-1">signature_data_segment/1*</a></td><td>Generate the data segment to be signed for a given TX.</td></tr><tr><td valign="top"><a href="#tx_to_json_struct-1">tx_to_json_struct/1</a></td><td></td></tr><tr><td valign="top"><a href="#verify-1">verify/1</a></td><td>Verify whether a transaction is valid.</td></tr><tr><td valign="top"><a href="#verify_hash-1">verify_hash/1*</a></td><td>Verify that the transaction's ID is a hash of its signature.</td></tr><tr><td valign="top"><a href="#verify_signature-2">verify_signature/2*</a></td><td>Verify the transaction's signature.</td></tr><tr><td valign="top"><a href="#verify_tx_id-2">verify_tx_id/2</a></td><td>Verify the given transaction actually has the given identifier.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="collect_validation_results-2"></a>

### collect_validation_results/2 * ###

`collect_validation_results(TXID, Checks) -> any()`

<a name="do_verify-2"></a>

### do_verify/2 * ###

`do_verify(TX, VerifySignature) -> any()`

Verify transaction.

<a name="json_struct_to_tx-1"></a>

### json_struct_to_tx/1 ###

`json_struct_to_tx(TXStruct) -> any()`

<a name="new-4"></a>

### new/4 ###

`new(Dest, Reward, Qty, Last) -> any()`

Create a new transaction.

<a name="new-5"></a>

### new/5 ###

`new(Dest, Reward, Qty, Last, SigType) -> any()`

<a name="sign-2"></a>

### sign/2 ###

`sign(TX, X2) -> any()`

Cryptographically sign (claim ownership of) a transaction.

<a name="signature_data_segment-1"></a>

### signature_data_segment/1 * ###

`signature_data_segment(TX) -> any()`

Generate the data segment to be signed for a given TX.

<a name="tx_to_json_struct-1"></a>

### tx_to_json_struct/1 ###

`tx_to_json_struct(Tx) -> any()`

<a name="verify-1"></a>

### verify/1 ###

`verify(TX) -> any()`

Verify whether a transaction is valid.

<a name="verify_hash-1"></a>

### verify_hash/1 * ###

`verify_hash(Tx) -> any()`

Verify that the transaction's ID is a hash of its signature.

<a name="verify_signature-2"></a>

### verify_signature/2 * ###

`verify_signature(TX, X2) -> any()`

Verify the transaction's signature.

<a name="verify_tx_id-2"></a>

### verify_tx_id/2 ###

`verify_tx_id(ExpectedID, Tx) -> any()`

Verify the given transaction actually has the given identifier.

