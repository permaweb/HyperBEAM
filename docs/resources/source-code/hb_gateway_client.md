# [Module hb_gateway_client.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_gateway_client.erl)




Implementation of Arweave's GraphQL API to gain access to specific
items of data stored on the network.

<a name="description"></a>

## Description ##
This module must be used to get full HyperBEAM `structured@1.0` form messages
from data items stored on the network, as Arweave gateways do not presently
expose all necessary fields to retrieve this information outside of the
GraphQL API. When gateways integrate serving in `httpsig@1.0` form, this
module will be deprecated.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ans104_no_data_item_test-0">ans104_no_data_item_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#ao_dataitem_test-0">ao_dataitem_test/0*</a></td><td>Test optimistic index.</td></tr><tr><td valign="top"><a href="#data-2">data/2</a></td><td>Get the data associated with a transaction by its ID, using the node's
Arweave <code>gateway</code> peers.</td></tr><tr><td valign="top"><a href="#decode_id_or_null-1">decode_id_or_null/1*</a></td><td></td></tr><tr><td valign="top"><a href="#decode_or_null-1">decode_or_null/1*</a></td><td></td></tr><tr><td valign="top"><a href="#item_spec-0">item_spec/0*</a></td><td>Gives the fields of a transaction that are needed to construct an
ANS-104 message.</td></tr><tr><td valign="top"><a href="#l1_transaction_test-0">l1_transaction_test/0*</a></td><td>Test l1 message from graphql.</td></tr><tr><td valign="top"><a href="#l2_dataitem_test-0">l2_dataitem_test/0*</a></td><td>Test l2 message from graphql.</td></tr><tr><td valign="top"><a href="#normalize_null-1">normalize_null/1*</a></td><td></td></tr><tr><td valign="top"><a href="#query-2">query/2*</a></td><td>Run a GraphQL request encoded as a binary.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Get a data item (including data and tags) by its ID, using the node's
GraphQL peers.</td></tr><tr><td valign="top"><a href="#result_to_message-2">result_to_message/2</a></td><td>Takes a GraphQL item node, matches it with the appropriate data from a
gateway, then returns <code>{ok, ParsedMsg}</code>.</td></tr><tr><td valign="top"><a href="#result_to_message-3">result_to_message/3*</a></td><td></td></tr><tr><td valign="top"><a href="#scheduler_location-2">scheduler_location/2</a></td><td>Find the location of the scheduler based on its ID, through GraphQL.</td></tr><tr><td valign="top"><a href="#scheduler_location_test-0">scheduler_location_test/0*</a></td><td>Test that we can get the scheduler location.</td></tr><tr><td valign="top"><a href="#subindex_to_tags-1">subindex_to_tags/1*</a></td><td>Takes a list of messages with <code>name</code> and <code>value</code> fields, and formats
them as a GraphQL <code>tags</code> argument.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="ans104_no_data_item_test-0"></a>

### ans104_no_data_item_test/0 * ###

`ans104_no_data_item_test() -> any()`

<a name="ao_dataitem_test-0"></a>

### ao_dataitem_test/0 * ###

`ao_dataitem_test() -> any()`

Test optimistic index

<a name="data-2"></a>

### data/2 ###

`data(ID, Opts) -> any()`

Get the data associated with a transaction by its ID, using the node's
Arweave `gateway` peers. The item is expected to be available in its
unmodified (by caches or other proxies) form at the following location:
https://<gateway>/raw/<id>
where `&lt;id&gt;` is the base64-url-encoded transaction ID.

<a name="decode_id_or_null-1"></a>

### decode_id_or_null/1 * ###

`decode_id_or_null(Bin) -> any()`

<a name="decode_or_null-1"></a>

### decode_or_null/1 * ###

`decode_or_null(Bin) -> any()`

<a name="item_spec-0"></a>

### item_spec/0 * ###

`item_spec() -> any()`

Gives the fields of a transaction that are needed to construct an
ANS-104 message.

<a name="l1_transaction_test-0"></a>

### l1_transaction_test/0 * ###

`l1_transaction_test() -> any()`

Test l1 message from graphql

<a name="l2_dataitem_test-0"></a>

### l2_dataitem_test/0 * ###

`l2_dataitem_test() -> any()`

Test l2 message from graphql

<a name="normalize_null-1"></a>

### normalize_null/1 * ###

`normalize_null(Bin) -> any()`

<a name="query-2"></a>

### query/2 * ###

`query(Query, Opts) -> any()`

Run a GraphQL request encoded as a binary. The node message may contain
a list of URLs to use, optionally as a tuple with an additional map of options
to use for the request.

<a name="read-2"></a>

### read/2 ###

`read(ID, Opts) -> any()`

Get a data item (including data and tags) by its ID, using the node's
GraphQL peers.
It uses the following GraphQL schema:
type Transaction {
id: ID!
anchor: String!
signature: String!
recipient: String!
owner: Owner { address: String! key: String! }!
fee: Amount!
quantity: Amount!
data: MetaData!
tags: [Tag { name: String! value: String! }!]!
}
type Amount {
winston: String!
ar: String!
}

<a name="result_to_message-2"></a>

### result_to_message/2 ###

`result_to_message(Item, Opts) -> any()`

Takes a GraphQL item node, matches it with the appropriate data from a
gateway, then returns `{ok, ParsedMsg}`.

<a name="result_to_message-3"></a>

### result_to_message/3 * ###

`result_to_message(ExpectedID, Item, Opts) -> any()`

<a name="scheduler_location-2"></a>

### scheduler_location/2 ###

`scheduler_location(Address, Opts) -> any()`

Find the location of the scheduler based on its ID, through GraphQL.

<a name="scheduler_location_test-0"></a>

### scheduler_location_test/0 * ###

`scheduler_location_test() -> any()`

Test that we can get the scheduler location.

<a name="subindex_to_tags-1"></a>

### subindex_to_tags/1 * ###

`subindex_to_tags(Subindex) -> any()`

Takes a list of messages with `name` and `value` fields, and formats
them as a GraphQL `tags` argument.

