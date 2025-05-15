# [Module dev_simple_pay.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_simple_pay.erl)




A simple device that allows the operator to specify a price for a
request and then charge the user for it, on a per message basis.

<a name="description"></a>

## Description ##
The device's ledger is stored in the node message at `simple_pay_ledger`,
and can be topped-up by either the operator, or an external device. The
price is specified in the node message at `simple_pay_price`.
This device acts as both a pricing device and a ledger device, by p4's
definition.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#balance-3">balance/3</a></td><td>Get the balance of a user in the ledger.</td></tr><tr><td valign="top"><a href="#debit-3">debit/3</a></td><td>Preprocess a request by checking the ledger and charging the user.</td></tr><tr><td valign="top"><a href="#estimate-3">estimate/3</a></td><td>Estimate the cost of a request by counting the number of messages in
the request, then multiplying by the per-message price.</td></tr><tr><td valign="top"><a href="#get_balance-2">get_balance/2*</a></td><td>Get the balance of a user in the ledger.</td></tr><tr><td valign="top"><a href="#get_balance_and_top_up_test-0">get_balance_and_top_up_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#is_operator-2">is_operator/2*</a></td><td>Check if the request is from the operator.</td></tr><tr><td valign="top"><a href="#set_balance-3">set_balance/3*</a></td><td>Adjust a user's balance, normalizing their wallet ID first.</td></tr><tr><td valign="top"><a href="#test_opts-1">test_opts/1*</a></td><td></td></tr><tr><td valign="top"><a href="#topup-3">topup/3</a></td><td>Top up the user's balance in the ledger.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="balance-3"></a>

### balance/3 ###

`balance(X1, RawReq, NodeMsg) -> any()`

Get the balance of a user in the ledger.

<a name="debit-3"></a>

### debit/3 ###

`debit(X1, RawReq, NodeMsg) -> any()`

Preprocess a request by checking the ledger and charging the user. We
can charge the user at this stage because we know statically what the price
will be

<a name="estimate-3"></a>

### estimate/3 ###

`estimate(X1, EstimateReq, NodeMsg) -> any()`

Estimate the cost of a request by counting the number of messages in
the request, then multiplying by the per-message price. The operator does
not pay for their own requests.

<a name="get_balance-2"></a>

### get_balance/2 * ###

`get_balance(Signer, NodeMsg) -> any()`

Get the balance of a user in the ledger.

<a name="get_balance_and_top_up_test-0"></a>

### get_balance_and_top_up_test/0 * ###

`get_balance_and_top_up_test() -> any()`

<a name="is_operator-2"></a>

### is_operator/2 * ###

`is_operator(Req, NodeMsg) -> any()`

Check if the request is from the operator.

<a name="set_balance-3"></a>

### set_balance/3 * ###

`set_balance(Signer, Amount, NodeMsg) -> any()`

Adjust a user's balance, normalizing their wallet ID first.

<a name="test_opts-1"></a>

### test_opts/1 * ###

`test_opts(Ledger) -> any()`

<a name="topup-3"></a>

### topup/3 ###

`topup(X1, Req, NodeMsg) -> any()`

Top up the user's balance in the ledger.

