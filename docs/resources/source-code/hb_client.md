# [Module hb_client.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_client.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_route-3">add_route/3</a></td><td></td></tr><tr><td valign="top"><a href="#arweave_timestamp-0">arweave_timestamp/0</a></td><td>Grab the latest block information from the Arweave gateway node.</td></tr><tr><td valign="top"><a href="#prefix_keys-3">prefix_keys/3*</a></td><td></td></tr><tr><td valign="top"><a href="#resolve-4">resolve/4</a></td><td>Resolve a message pair on a remote node.</td></tr><tr><td valign="top"><a href="#routes-2">routes/2</a></td><td></td></tr><tr><td valign="top"><a href="#upload-2">upload/2</a></td><td>Upload a data item to the bundler node.</td></tr><tr><td valign="top"><a href="#upload-3">upload/3*</a></td><td></td></tr><tr><td valign="top"><a href="#upload_empty_message_test-0">upload_empty_message_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#upload_empty_raw_ans104_test-0">upload_empty_raw_ans104_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#upload_raw_ans104_test-0">upload_raw_ans104_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#upload_raw_ans104_with_anchor_test-0">upload_raw_ans104_with_anchor_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#upload_single_layer_message_test-0">upload_single_layer_message_test/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_route-3"></a>

### add_route/3 ###

`add_route(Node, Route, Opts) -> any()`

<a name="arweave_timestamp-0"></a>

### arweave_timestamp/0 ###

`arweave_timestamp() -> any()`

Grab the latest block information from the Arweave gateway node.

<a name="prefix_keys-3"></a>

### prefix_keys/3 * ###

`prefix_keys(Prefix, Message, Opts) -> any()`

<a name="resolve-4"></a>

### resolve/4 ###

`resolve(Node, Msg1, Msg2, Opts) -> any()`

Resolve a message pair on a remote node.
The message pair is first transformed into a singleton request, by
prefixing the keys in both messages for the path segment that they relate to,
and then adjusting the "Path" field from the second message.

<a name="routes-2"></a>

### routes/2 ###

`routes(Node, Opts) -> any()`

<a name="upload-2"></a>

### upload/2 ###

`upload(Msg, Opts) -> any()`

Upload a data item to the bundler node.

<a name="upload-3"></a>

### upload/3 * ###

`upload(Msg, Opts, X3) -> any()`

<a name="upload_empty_message_test-0"></a>

### upload_empty_message_test/0 * ###

`upload_empty_message_test() -> any()`

<a name="upload_empty_raw_ans104_test-0"></a>

### upload_empty_raw_ans104_test/0 * ###

`upload_empty_raw_ans104_test() -> any()`

<a name="upload_raw_ans104_test-0"></a>

### upload_raw_ans104_test/0 * ###

`upload_raw_ans104_test() -> any()`

<a name="upload_raw_ans104_with_anchor_test-0"></a>

### upload_raw_ans104_with_anchor_test/0 * ###

`upload_raw_ans104_with_anchor_test() -> any()`

<a name="upload_single_layer_message_test-0"></a>

### upload_single_layer_message_test/0 * ###

`upload_single_layer_message_test() -> any()`

