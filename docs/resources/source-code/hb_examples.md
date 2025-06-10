# [Module hb_examples.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_examples.erl)




This module contains end-to-end tests for Hyperbeam, accessing through
the HTTP interface.

<a name="description"></a>

## Description ##
As well as testing the system, you can use these tests
as examples of how to interact with HyperBEAM nodes.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_schedule_aos2_test_disabled-0">create_schedule_aos2_test_disabled/0*</a></td><td></td></tr><tr><td valign="top"><a href="#paid_wasm_test-0">paid_wasm_test/0*</a></td><td>Gain signed WASM responses from a node and verify them.</td></tr><tr><td valign="top"><a href="#relay_with_payments_test-0">relay_with_payments_test/0*</a></td><td>Start a node running the simple pay meta device, and use it to relay
a message for a client.</td></tr><tr><td valign="top"><a href="#schedule-2">schedule/2*</a></td><td></td></tr><tr><td valign="top"><a href="#schedule-3">schedule/3*</a></td><td></td></tr><tr><td valign="top"><a href="#schedule-4">schedule/4*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create_schedule_aos2_test_disabled-0"></a>

### create_schedule_aos2_test_disabled/0 * ###

`create_schedule_aos2_test_disabled() -> any()`

<a name="paid_wasm_test-0"></a>

### paid_wasm_test/0 * ###

`paid_wasm_test() -> any()`

Gain signed WASM responses from a node and verify them.
1. Start the client with a small balance.
2. Execute a simple WASM function on the host node.
3. Verify the response is correct and signed by the host node.
4. Get the balance of the client and verify it has been deducted.

<a name="relay_with_payments_test-0"></a>

### relay_with_payments_test/0 * ###

`relay_with_payments_test() -> any()`

Start a node running the simple pay meta device, and use it to relay
a message for a client. We must ensure:
1. When the client has no balance, the relay fails.
2. The operator is able to topup for the client.
3. The client has the correct balance after the topup.
4. The relay succeeds when the client has enough balance.
5. The received message is signed by the host using http-sig and validates
correctly.

<a name="schedule-2"></a>

### schedule/2 * ###

`schedule(ProcMsg, Target) -> any()`

<a name="schedule-3"></a>

### schedule/3 * ###

`schedule(ProcMsg, Target, Wallet) -> any()`

<a name="schedule-4"></a>

### schedule/4 * ###

`schedule(ProcMsg, Target, Wallet, Node) -> any()`

