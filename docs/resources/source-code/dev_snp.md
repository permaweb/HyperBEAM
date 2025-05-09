# [Module dev_snp.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_snp.erl)




This device offers an interface for validating AMD SEV-SNP commitments,
as well as generating them, if called in an appropriate environment.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute_is_trusted-3">execute_is_trusted/3*</a></td><td>Ensure that all of the software hashes are trusted.</td></tr><tr><td valign="top"><a href="#generate-3">generate/3</a></td><td>Generate an commitment report and emit it as a message, including all of
the necessary data to generate the nonce (ephemeral node address + node
message ID), as well as the expected measurement (firmware, kernel, and VMSAs
hashes).</td></tr><tr><td valign="top"><a href="#generate_nonce-2">generate_nonce/2*</a></td><td>Generate the nonce to use in the commitment report.</td></tr><tr><td valign="top"><a href="#is_debug-1">is_debug/1*</a></td><td>Ensure that the node's debug policy is disabled.</td></tr><tr><td valign="top"><a href="#real_node_test-0">real_node_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#report_data_matches-3">report_data_matches/3*</a></td><td>Ensure that the report data matches the expected report data.</td></tr><tr><td valign="top"><a href="#trusted-3">trusted/3</a></td><td>Validates if a given message parameter matches a trusted value from the SNP trusted list
Returns {ok, true} if the message is trusted, {ok, false} otherwise.</td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td>Verify an commitment report message; validating the identity of a
remote node, its ephemeral private address, and the integrity of the report.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="execute_is_trusted-3"></a>

### execute_is_trusted/3 * ###

`execute_is_trusted(M1, Msg, NodeOpts) -> any()`

Ensure that all of the software hashes are trusted. The caller may set
a specific device to use for the `is-trusted` key. The device must then
implement the `trusted` resolver.

<a name="generate-3"></a>

### generate/3 ###

`generate(M1, M2, Opts) -> any()`

Generate an commitment report and emit it as a message, including all of
the necessary data to generate the nonce (ephemeral node address + node
message ID), as well as the expected measurement (firmware, kernel, and VMSAs
hashes).

<a name="generate_nonce-2"></a>

### generate_nonce/2 * ###

`generate_nonce(RawAddress, RawNodeMsgID) -> any()`

Generate the nonce to use in the commitment report.

<a name="is_debug-1"></a>

### is_debug/1 * ###

`is_debug(Report) -> any()`

Ensure that the node's debug policy is disabled.

<a name="real_node_test-0"></a>

### real_node_test/0 * ###

`real_node_test() -> any()`

<a name="report_data_matches-3"></a>

### report_data_matches/3 * ###

`report_data_matches(Address, NodeMsgID, ReportData) -> any()`

Ensure that the report data matches the expected report data.

<a name="trusted-3"></a>

### trusted/3 ###

`trusted(Msg1, Msg2, NodeOpts) -> any()`

Validates if a given message parameter matches a trusted value from the SNP trusted list
Returns {ok, true} if the message is trusted, {ok, false} otherwise

<a name="verify-3"></a>

### verify/3 ###

`verify(M1, M2, NodeOpts) -> any()`

Verify an commitment report message; validating the identity of a
remote node, its ephemeral private address, and the integrity of the report.
The checks that must be performed to validate the report are:
1. Verify the address and the node message ID are the same as the ones
used to generate the nonce.
2. Verify the address that signed the message is the same as the one used
to generate the nonce.
3. Verify that the debug flag is disabled.
4. Verify that the firmware, kernel, and OS (VMSAs) hashes, part of the
measurement, are trusted.
5. Verify the measurement is valid.
6. Verify the report's certificate chain to hardware root of trust.

