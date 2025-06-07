# [Module dev_faff.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_faff.erl)




A module that implements a 'friends and family' pricing policy.

<a name="description"></a>

## Description ##

It will allow users to process requests only if their addresses are
in the allow-list for the node.

Fundamentally against the spirit of permissionlessness, but it is useful if
you are running a node for your own purposes and would not like to allow
others to make use of it -- even for a fee. It also serves as a useful
example of how to implement a custom pricing policy, as it implements stubs
for both the pricing and ledger P4 APIs.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#charge-3">charge/3</a></td><td>Charge the user's account if the request is allowed.</td></tr><tr><td valign="top"><a href="#estimate-3">estimate/3</a></td><td>Decide whether or not to service a request from a given address.</td></tr><tr><td valign="top"><a href="#is_admissible-2">is_admissible/2*</a></td><td>Check whether all of the signers of the request are in the allow-list.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="charge-3"></a>

### charge/3 ###

`charge(X1, Req, NodeMsg) -> any()`

Charge the user's account if the request is allowed.

<a name="estimate-3"></a>

### estimate/3 ###

`estimate(X1, Msg, NodeMsg) -> any()`

Decide whether or not to service a request from a given address.

<a name="is_admissible-2"></a>

### is_admissible/2 * ###

`is_admissible(Msg, NodeMsg) -> any()`

Check whether all of the signers of the request are in the allow-list.

