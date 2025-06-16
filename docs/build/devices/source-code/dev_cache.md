# [Module dev_cache.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_cache.erl)




A device that looks up an ID from a local store and returns it,
honoring the `accept` key to return the correct format.

<a name="description"></a>

## Description ##
The cache also
supports writing messages to the store, if the node message has the
writer's address in its `cache_writers` key.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cache_write_binary_test-0">cache_write_binary_test/0*</a></td><td>Ensure that we can write direct binaries to the cache.</td></tr><tr><td valign="top"><a href="#cache_write_message_test-0">cache_write_message_test/0*</a></td><td>Test that the cache can be written to and read from using the hb_cache
API.</td></tr><tr><td valign="top"><a href="#is_trusted_writer-2">is_trusted_writer/2*</a></td><td>Verify that the request originates from a trusted writer.</td></tr><tr><td valign="top"><a href="#link-3">link/3</a></td><td>Link a source to a destination in the cache.</td></tr><tr><td valign="top"><a href="#read-3">read/3</a></td><td>Read data from the cache.</td></tr><tr><td valign="top"><a href="#read_from_cache-2">read_from_cache/2*</a></td><td>Read data from the cache via HTTP.</td></tr><tr><td valign="top"><a href="#setup_test_env-0">setup_test_env/0*</a></td><td>Create a test environment with a local store and node.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>Write data to the cache.</td></tr><tr><td valign="top"><a href="#write_single-2">write_single/2*</a></td><td>Helper function to write a single data item to the cache.</td></tr><tr><td valign="top"><a href="#write_to_cache-3">write_to_cache/3*</a></td><td>Write data to the cache via HTTP.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cache_write_binary_test-0"></a>

### cache_write_binary_test/0 * ###

`cache_write_binary_test() -> any()`

Ensure that we can write direct binaries to the cache.

<a name="cache_write_message_test-0"></a>

### cache_write_message_test/0 * ###

`cache_write_message_test() -> any()`

Test that the cache can be written to and read from using the hb_cache
API.

<a name="is_trusted_writer-2"></a>

### is_trusted_writer/2 * ###

`is_trusted_writer(Req, Opts) -> any()`

Verify that the request originates from a trusted writer.
Checks that the single signer of the request is present in the list
of trusted cache writer addresses specified in the options.

<a name="link-3"></a>

### link/3 ###

`link(Base, Req, Opts) -> any()`

Link a source to a destination in the cache.

<a name="read-3"></a>

### read/3 ###

`read(M1, M2, Opts) -> any()`

Read data from the cache.
Retrieves data corresponding to a key from a local store.
The key is extracted from the incoming message under <<"target">>.
The options map may include store configuration.
If the "accept" header is set to <<"application/aos-2">>, the result is
converted to a JSON structure and encoded.

<a name="read_from_cache-2"></a>

### read_from_cache/2 * ###

`read_from_cache(Node, Path) -> any()`

Read data from the cache via HTTP.
Constructs a GET request using the provided path, sends it to the node,
and returns the response.

<a name="setup_test_env-0"></a>

### setup_test_env/0 * ###

`setup_test_env() -> any()`

Create a test environment with a local store and node.
Ensures that the required application is started, configures a local
file-system store, resets the store for a clean state, creates a wallet
for signing requests, and starts a node with the store and trusted cache
writer configuration.

<a name="write-3"></a>

### write/3 ###

`write(M1, M2, Opts) -> any()`

Write data to the cache.
Processes a write request by first verifying that the request comes from a
trusted writer (as defined by the `cache_writers` configuration in the
options). The write type is determined from the message ("single" or "batch")
and the data is stored accordingly.

<a name="write_single-2"></a>

### write_single/2 * ###

`write_single(Msg, Opts) -> any()`

Helper function to write a single data item to the cache.
Extracts the body, location, and operation from the message.
Depending on the type of data (map or binary) or if a link operation is
requested, it writes the data to the store using the appropriate function.

<a name="write_to_cache-3"></a>

### write_to_cache/3 * ###

`write_to_cache(Node, Data, Wallet) -> any()`

Write data to the cache via HTTP.
Constructs a write request message with the provided data, signs it with the
given wallet, sends it to the node, and verifies that the response indicates
a successful write.

