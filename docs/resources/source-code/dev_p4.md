# [Module dev_p4.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_p4.erl)




The HyperBEAM core payment ledger.

<a name="description"></a>

## Description ##

This module allows the operator to
specify another device that can act as a pricing mechanism for transactions
on the node, as well as orchestrating a payment ledger to calculate whether
the node should fulfil services for users.

The device requires the following node message settings in order to function:

- `p4_pricing-device`: The device that will estimate the cost of a request.
- `p4_ledger-device`: The device that will act as a payment ledger.

The pricing device should implement the following keys:

```
<code>GET /estimate?type=pre|post&body=[...]&request=RequestMessage</code><code>GET /price?type=pre|post&body=[...]&request=RequestMessage</code>
```

The `body` key is used to pass either the request or response messages to the
device. The `type` key is used to specify whether the inquiry is for a request
(pre) or a response (post) object. Requests carry lists of messages that will
be executed, while responses carry the results of the execution. The `price`
key may return `infinity` if the node will not serve a user under any
circumstances. Else, the value returned by the `price` key will be passed to
the ledger device as the `amount` key.

A ledger device should implement the following keys:

```
<code>POST /credit?message=PaymentMessage&request=RequestMessage</code><code>POST /debit?amount=PriceMessage&request=RequestMessage</code><code>GET /balance?request=RequestMessage</code>
```

The `type` key is optional and defaults to `pre`. If `type` is set to `post`,
the debit must be applied to the ledger, whereas the `pre` type is used to
check whether the debit would succeed before execution.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#balance-3">balance/3</a></td><td>Get the balance of a user in the ledger.</td></tr><tr><td valign="top"><a href="#faff_test-0">faff_test/0*</a></td><td>Simple test of p4's capabilities with the <code>faff@1.0</code> device.</td></tr><tr><td valign="top"><a href="#is_chargable_req-2">is_chargable_req/2*</a></td><td>The node operator may elect to make certain routes non-chargable, using
the <code>routes</code> syntax also used to declare routes in <code>router@1.0</code>.</td></tr><tr><td valign="top"><a href="#lua_pricing_test-0">lua_pricing_test/0*</a></td><td>Ensure that Lua modules can be used as pricing and ledger devices.</td></tr><tr><td valign="top"><a href="#non_chargable_route_test-0">non_chargable_route_test/0*</a></td><td>Test that a non-chargable route is not charged for.</td></tr><tr><td valign="top"><a href="#request-3">request/3</a></td><td>Estimate the cost of a transaction and decide whether to proceed with
a request.</td></tr><tr><td valign="top"><a href="#response-3">response/3</a></td><td>Postprocess the request after it has been fulfilled.</td></tr><tr><td valign="top"><a href="#test_opts-1">test_opts/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_opts-2">test_opts/2*</a></td><td></td></tr><tr><td valign="top"><a href="#test_opts-3">test_opts/3*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="balance-3"></a>

### balance/3 ###

`balance(X1, Req, NodeMsg) -> any()`

Get the balance of a user in the ledger.

<a name="faff_test-0"></a>

### faff_test/0 * ###

`faff_test() -> any()`

Simple test of p4's capabilities with the `faff@1.0` device.

<a name="is_chargable_req-2"></a>

### is_chargable_req/2 * ###

`is_chargable_req(Req, NodeMsg) -> any()`

The node operator may elect to make certain routes non-chargable, using
the `routes` syntax also used to declare routes in `router@1.0`.

<a name="lua_pricing_test-0"></a>

### lua_pricing_test/0 * ###

`lua_pricing_test() -> any()`

Ensure that Lua modules can be used as pricing and ledger devices. Our
modules come in two parts:
- A `process` module which is executed as a persistent `local-process` on the
node, and which maintains the state of the ledger.
- A `client` module, which is executed as a `p4@1.0` device, marshalling
requests to the `process` module.

<a name="non_chargable_route_test-0"></a>

### non_chargable_route_test/0 * ###

`non_chargable_route_test() -> any()`

Test that a non-chargable route is not charged for.

<a name="request-3"></a>

### request/3 ###

`request(State, Raw, NodeMsg) -> any()`

Estimate the cost of a transaction and decide whether to proceed with
a request. The default behavior if `pricing-device` or `p4_balances` are
not set is to proceed, so it is important that a user initialize them.

<a name="response-3"></a>

### response/3 ###

`response(State, RawResponse, NodeMsg) -> any()`

Postprocess the request after it has been fulfilled.

<a name="test_opts-1"></a>

### test_opts/1 * ###

`test_opts(Opts) -> any()`

<a name="test_opts-2"></a>

### test_opts/2 * ###

`test_opts(Opts, PricingDev) -> any()`

<a name="test_opts-3"></a>

### test_opts/3 * ###

`test_opts(Opts, PricingDev, LedgerDev) -> any()`

