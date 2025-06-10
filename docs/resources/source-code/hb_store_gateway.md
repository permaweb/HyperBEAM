# [Module hb_store_gateway.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_store_gateway.erl)




A store module that reads data from the nodes Arweave gateway and
GraphQL routes, additionally including additional store-specific routes.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cache_read_message_test-0">cache_read_message_test/0*</a></td><td>Ensure that saving to the gateway store works.</td></tr><tr><td valign="top"><a href="#external_http_access_test-0">external_http_access_test/0*</a></td><td>Test that the default node config allows for data to be accessed.</td></tr><tr><td valign="top"><a href="#graphql_as_store_test_-0">graphql_as_store_test_/0*</a></td><td>Store is accessible via the default options.</td></tr><tr><td valign="top"><a href="#graphql_from_cache_test-0">graphql_from_cache_test/0*</a></td><td>Stored messages are accessible via <code>hb_cache</code> accesses.</td></tr><tr><td valign="top"><a href="#list-2">list/2</a></td><td></td></tr><tr><td valign="top"><a href="#manual_local_cache_test-0">manual_local_cache_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_cache-2">maybe_cache/2*</a></td><td>Cache the data if the cache is enabled.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read the data at the given key from the GraphQL route.</td></tr><tr><td valign="top"><a href="#resolve-2">resolve/2</a></td><td></td></tr><tr><td valign="top"><a href="#resolve_on_gateway_test_-0">resolve_on_gateway_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#scope-1">scope/1</a></td><td>The scope of a GraphQL store is always remote, due to performance.</td></tr><tr><td valign="top"><a href="#specific_route_test-0">specific_route_test/0*</a></td><td>Routes can be specified in the options, overriding the default routes.</td></tr><tr><td valign="top"><a href="#store_opts_test-0">store_opts_test/0*</a></td><td>Test to verify store opts is being set for Data-Protocol ao.</td></tr><tr><td valign="top"><a href="#type-2">type/2</a></td><td>Get the type of the data at the given key.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cache_read_message_test-0"></a>

### cache_read_message_test/0 * ###

`cache_read_message_test() -> any()`

Ensure that saving to the gateway store works.

<a name="external_http_access_test-0"></a>

### external_http_access_test/0 * ###

`external_http_access_test() -> any()`

Test that the default node config allows for data to be accessed.

<a name="graphql_as_store_test_-0"></a>

### graphql_as_store_test_/0 * ###

`graphql_as_store_test_() -> any()`

Store is accessible via the default options.

<a name="graphql_from_cache_test-0"></a>

### graphql_from_cache_test/0 * ###

`graphql_from_cache_test() -> any()`

Stored messages are accessible via `hb_cache` accesses.

<a name="list-2"></a>

### list/2 ###

`list(StoreOpts, Key) -> any()`

<a name="manual_local_cache_test-0"></a>

### manual_local_cache_test/0 * ###

`manual_local_cache_test() -> any()`

<a name="maybe_cache-2"></a>

### maybe_cache/2 * ###

`maybe_cache(StoreOpts, Data) -> any()`

Cache the data if the cache is enabled. The `store` option may either
be `false` to disable local caching, or a store definition to use as the
cache.

<a name="read-2"></a>

### read/2 ###

`read(StoreOpts, Key) -> any()`

Read the data at the given key from the GraphQL route. Will only attempt
to read the data if the key is an ID.

<a name="resolve-2"></a>

### resolve/2 ###

`resolve(X1, Key) -> any()`

<a name="resolve_on_gateway_test_-0"></a>

### resolve_on_gateway_test_/0 * ###

`resolve_on_gateway_test_() -> any()`

<a name="scope-1"></a>

### scope/1 ###

`scope(X1) -> any()`

The scope of a GraphQL store is always remote, due to performance.

<a name="specific_route_test-0"></a>

### specific_route_test/0 * ###

`specific_route_test() -> any()`

Routes can be specified in the options, overriding the default routes.
We test this by inversion: If the above cache read test works, then we know
that the default routes allow access to the item. If the test below were to
produce the same result, despite an empty 'only' route list, then we would
know that the module is not respecting the route list.

<a name="store_opts_test-0"></a>

### store_opts_test/0 * ###

`store_opts_test() -> any()`

Test to verify store opts is being set for Data-Protocol ao

<a name="type-2"></a>

### type/2 ###

`type(StoreOpts, Key) -> any()`

Get the type of the data at the given key. We potentially cache the
result, so that we don't have to read the data from the GraphQL route
multiple times.

