# [Module hb_http.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_http.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accept_to_codec-2">accept_to_codec/2</a></td><td>Calculate the codec name to use for a reply given its initiating Cowboy
request, the parsed TABM request, and the response message.</td></tr><tr><td valign="top"><a href="#add_cors_headers-2">add_cors_headers/2*</a></td><td>Add permissive CORS headers to a message, if the message has not already
specified CORS headers.</td></tr><tr><td valign="top"><a href="#allowed_status-2">allowed_status/2*</a></td><td>Check if a status is allowed, according to the configuration.</td></tr><tr><td valign="top"><a href="#ans104_wasm_test-0">ans104_wasm_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#codec_to_content_type-2">codec_to_content_type/2*</a></td><td>Call the <code>content-type</code> key on a message with the given codec, using
a fast-path for options that are not needed for this one-time lookup.</td></tr><tr><td valign="top"><a href="#cors_get_test-0">cors_get_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#default_codec-1">default_codec/1*</a></td><td>Return the default codec for the given options.</td></tr><tr><td valign="top"><a href="#empty_inbox-1">empty_inbox/1*</a></td><td>Empty the inbox of the current process for all messages with the given
reference.</td></tr><tr><td valign="top"><a href="#encode_reply-3">encode_reply/3*</a></td><td>Generate the headers and body for a HTTP response message.</td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Gets a URL via HTTP and returns the resulting message in deserialized
form.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_deep_signed_wasm_state_test-0">get_deep_signed_wasm_state_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#get_deep_unsigned_wasm_state_test-0">get_deep_unsigned_wasm_state_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_response_to_httpsig-4">http_response_to_httpsig/4*</a></td><td>Convert a HTTP response to a httpsig message.</td></tr><tr><td valign="top"><a href="#httpsig_to_tabm_singleton-3">httpsig_to_tabm_singleton/3*</a></td><td>HTTPSig messages are inherently mixed into the transport layer, so they
require special handling in order to be converted to a normalized message.</td></tr><tr><td valign="top"><a href="#maybe_add_unsigned-3">maybe_add_unsigned/3*</a></td><td>Add the method and path to a message, if they are not already present.</td></tr><tr><td valign="top"><a href="#message_to_request-2">message_to_request/2*</a></td><td>Given a message, return the information needed to make the request.</td></tr><tr><td valign="top"><a href="#mime_to_codec-2">mime_to_codec/2*</a></td><td>Find a codec name from a mime-type.</td></tr><tr><td valign="top"><a href="#multirequest-5">multirequest/5*</a></td><td>Dispatch the same HTTP request to many nodes.</td></tr><tr><td valign="top"><a href="#multirequest_opt-5">multirequest_opt/5*</a></td><td>Get a value for a multirequest option from the config or message.</td></tr><tr><td valign="top"><a href="#multirequest_opts-3">multirequest_opts/3*</a></td><td>Get the multirequest options from the config or message.</td></tr><tr><td valign="top"><a href="#nested_ao_resolve_test-0">nested_ao_resolve_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#parallel_multirequest-8">parallel_multirequest/8*</a></td><td>Dispatch the same HTTP request to many nodes in parallel.</td></tr><tr><td valign="top"><a href="#parallel_responses-7">parallel_responses/7*</a></td><td>Collect the necessary number of responses, and stop workers if
configured to do so.</td></tr><tr><td valign="top"><a href="#post-3">post/3</a></td><td>Posts a message to a URL on a remote peer via HTTP.</td></tr><tr><td valign="top"><a href="#post-4">post/4</a></td><td></td></tr><tr><td valign="top"><a href="#prepare_request-6">prepare_request/6*</a></td><td>Turn a set of request arguments into a request message, formatted in the
preferred format.</td></tr><tr><td valign="top"><a href="#remove_unsigned_fields-2">remove_unsigned_fields/2*</a></td><td></td></tr><tr><td valign="top"><a href="#reply-4">reply/4</a></td><td>Reply to the client's HTTP request with a message.</td></tr><tr><td valign="top"><a href="#reply-5">reply/5*</a></td><td></td></tr><tr><td valign="top"><a href="#req_to_tabm_singleton-3">req_to_tabm_singleton/3</a></td><td>Convert a cowboy request to a normalized message.</td></tr><tr><td valign="top"><a href="#request-2">request/2</a></td><td>Posts a binary to a URL on a remote peer via HTTP, returning the raw
binary body.</td></tr><tr><td valign="top"><a href="#request-4">request/4</a></td><td></td></tr><tr><td valign="top"><a href="#request-5">request/5</a></td><td></td></tr><tr><td valign="top"><a href="#route_to_request-3">route_to_request/3*</a></td><td>Parse a <code>dev_router:route</code> response and return a tuple of request
parameters.</td></tr><tr><td valign="top"><a href="#run_wasm_signed_test-0">run_wasm_signed_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#run_wasm_unsigned_test-0">run_wasm_unsigned_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#send_encoded_node_message_test-2">send_encoded_node_message_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#send_flat_encoded_node_message_test-0">send_flat_encoded_node_message_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#send_json_encoded_node_message_test-0">send_json_encoded_node_message_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#send_large_signed_request_test-0">send_large_signed_request_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#serial_multirequest-7">serial_multirequest/7*</a></td><td>Serially request a message, collecting responses until the required
number of responses have been gathered.</td></tr><tr><td valign="top"><a href="#simple_ao_resolve_signed_test-0">simple_ao_resolve_signed_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#simple_ao_resolve_unsigned_test-0">simple_ao_resolve_unsigned_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#wasm_compute_request-3">wasm_compute_request/3*</a></td><td></td></tr><tr><td valign="top"><a href="#wasm_compute_request-4">wasm_compute_request/4*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accept_to_codec-2"></a>

### accept_to_codec/2 ###

`accept_to_codec(TABMReq, Opts) -> any()`

Calculate the codec name to use for a reply given its initiating Cowboy
request, the parsed TABM request, and the response message. The precidence
order for finding the codec is:
1. The `accept-codec` field in the message
2. The `accept` field in the request headers
3. The default codec
Options can be specified in mime-type format (`application/*`) or in
AO device format (`device@1.0`).

<a name="add_cors_headers-2"></a>

### add_cors_headers/2 * ###

`add_cors_headers(Msg, ReqHdr) -> any()`

Add permissive CORS headers to a message, if the message has not already
specified CORS headers.

<a name="allowed_status-2"></a>

### allowed_status/2 * ###

`allowed_status(ResponseMsg, Statuses) -> any()`

Check if a status is allowed, according to the configuration.

<a name="ans104_wasm_test-0"></a>

### ans104_wasm_test/0 * ###

`ans104_wasm_test() -> any()`

<a name="codec_to_content_type-2"></a>

### codec_to_content_type/2 * ###

`codec_to_content_type(Codec, Opts) -> any()`

Call the `content-type` key on a message with the given codec, using
a fast-path for options that are not needed for this one-time lookup.

<a name="cors_get_test-0"></a>

### cors_get_test/0 * ###

`cors_get_test() -> any()`

<a name="default_codec-1"></a>

### default_codec/1 * ###

`default_codec(Opts) -> any()`

Return the default codec for the given options.

<a name="empty_inbox-1"></a>

### empty_inbox/1 * ###

`empty_inbox(Ref) -> any()`

Empty the inbox of the current process for all messages with the given
reference.

<a name="encode_reply-3"></a>

### encode_reply/3 * ###

`encode_reply(TABMReq, Message, Opts) -> any()`

Generate the headers and body for a HTTP response message.

<a name="get-2"></a>

### get/2 ###

`get(Node, Opts) -> any()`

Gets a URL via HTTP and returns the resulting message in deserialized
form.

<a name="get-3"></a>

### get/3 ###

`get(Node, PathBin, Opts) -> any()`

<a name="get_deep_signed_wasm_state_test-0"></a>

### get_deep_signed_wasm_state_test/0 * ###

`get_deep_signed_wasm_state_test() -> any()`

<a name="get_deep_unsigned_wasm_state_test-0"></a>

### get_deep_unsigned_wasm_state_test/0 * ###

`get_deep_unsigned_wasm_state_test() -> any()`

<a name="http_response_to_httpsig-4"></a>

### http_response_to_httpsig/4 * ###

`http_response_to_httpsig(Status, HeaderMap, Body, Opts) -> any()`

Convert a HTTP response to a httpsig message.

<a name="httpsig_to_tabm_singleton-3"></a>

### httpsig_to_tabm_singleton/3 * ###

`httpsig_to_tabm_singleton(Req, Body, Opts) -> any()`

HTTPSig messages are inherently mixed into the transport layer, so they
require special handling in order to be converted to a normalized message.
In particular, the signatures are verified if present and required by the
node configuration. Additionally, non-committed fields are removed from the
message if it is signed, with the exception of the `path` and `method` fields.

<a name="maybe_add_unsigned-3"></a>

### maybe_add_unsigned/3 * ###

`maybe_add_unsigned(Req, Msg, Opts) -> any()`

Add the method and path to a message, if they are not already present.
The precidence order for finding the path is:
1. The path in the message
2. The path in the request URI

<a name="message_to_request-2"></a>

### message_to_request/2 * ###

`message_to_request(M, Opts) -> any()`

Given a message, return the information needed to make the request.

<a name="mime_to_codec-2"></a>

### mime_to_codec/2 * ###

`mime_to_codec(X1, Opts) -> any()`

Find a codec name from a mime-type.

<a name="multirequest-5"></a>

### multirequest/5 * ###

`multirequest(Config, Method, Path, Message, Opts) -> any()`

Dispatch the same HTTP request to many nodes. Can be configured to
await responses from all nodes or just one, and to halt all requests after
after it has received the required number of responses, or to leave all
requests running until they have all completed. Default: Race for first
response.

Expects a config message of the following form:
/Nodes/1..n: Hostname | #{ hostname => Hostname, address => Address }
/Responses: Number of responses to gather
/Stop-After: Should we stop after the required number of responses?
/Parallel: Should we run the requests in parallel?

<a name="multirequest_opt-5"></a>

### multirequest_opt/5 * ###

`multirequest_opt(Key, Config, Message, Default, Opts) -> any()`

Get a value for a multirequest option from the config or message.

<a name="multirequest_opts-3"></a>

### multirequest_opts/3 * ###

`multirequest_opts(Config, Message, Opts) -> any()`

Get the multirequest options from the config or message. The options in
the message take precidence over the options in the config.

<a name="nested_ao_resolve_test-0"></a>

### nested_ao_resolve_test/0 * ###

`nested_ao_resolve_test() -> any()`

<a name="parallel_multirequest-8"></a>

### parallel_multirequest/8 * ###

`parallel_multirequest(Nodes, Responses, StopAfter, Method, Path, Message, Statuses, Opts) -> any()`

Dispatch the same HTTP request to many nodes in parallel.

<a name="parallel_responses-7"></a>

### parallel_responses/7 * ###

`parallel_responses(Res, Procs, Ref, Awaiting, StopAfter, Statuses, Opts) -> any()`

Collect the necessary number of responses, and stop workers if
configured to do so.

<a name="post-3"></a>

### post/3 ###

`post(Node, Message, Opts) -> any()`

Posts a message to a URL on a remote peer via HTTP. Returns the
resulting message in deserialized form.

<a name="post-4"></a>

### post/4 ###

`post(Node, Path, Message, Opts) -> any()`

<a name="prepare_request-6"></a>

### prepare_request/6 * ###

`prepare_request(Format, Method, Peer, Path, RawMessage, Opts) -> any()`

Turn a set of request arguments into a request message, formatted in the
preferred format.

<a name="remove_unsigned_fields-2"></a>

### remove_unsigned_fields/2 * ###

`remove_unsigned_fields(Msg, Opts) -> any()`

<a name="reply-4"></a>

### reply/4 ###

`reply(Req, TABMReq, Message, Opts) -> any()`

Reply to the client's HTTP request with a message.

<a name="reply-5"></a>

### reply/5 * ###

`reply(Req, TABMReq, BinStatus, RawMessage, Opts) -> any()`

<a name="req_to_tabm_singleton-3"></a>

### req_to_tabm_singleton/3 ###

`req_to_tabm_singleton(Req, Body, Opts) -> any()`

Convert a cowboy request to a normalized message.

<a name="request-2"></a>

### request/2 ###

`request(Message, Opts) -> any()`

Posts a binary to a URL on a remote peer via HTTP, returning the raw
binary body.

<a name="request-4"></a>

### request/4 ###

`request(Method, Peer, Path, Opts) -> any()`

<a name="request-5"></a>

### request/5 ###

`request(Method, Config, Path, Message, Opts) -> any()`

<a name="route_to_request-3"></a>

### route_to_request/3 * ###

`route_to_request(M, X2, Opts) -> any()`

Parse a `dev_router:route` response and return a tuple of request
parameters.

<a name="run_wasm_signed_test-0"></a>

### run_wasm_signed_test/0 * ###

`run_wasm_signed_test() -> any()`

<a name="run_wasm_unsigned_test-0"></a>

### run_wasm_unsigned_test/0 * ###

`run_wasm_unsigned_test() -> any()`

<a name="send_encoded_node_message_test-2"></a>

### send_encoded_node_message_test/2 * ###

`send_encoded_node_message_test(Config, Codec) -> any()`

<a name="send_flat_encoded_node_message_test-0"></a>

### send_flat_encoded_node_message_test/0 * ###

`send_flat_encoded_node_message_test() -> any()`

<a name="send_json_encoded_node_message_test-0"></a>

### send_json_encoded_node_message_test/0 * ###

`send_json_encoded_node_message_test() -> any()`

<a name="send_large_signed_request_test-0"></a>

### send_large_signed_request_test/0 * ###

`send_large_signed_request_test() -> any()`

<a name="serial_multirequest-7"></a>

### serial_multirequest/7 * ###

`serial_multirequest(Nodes, Remaining, Method, Path, Message, Statuses, Opts) -> any()`

Serially request a message, collecting responses until the required
number of responses have been gathered. Ensure that the statuses are
allowed, according to the configuration.

<a name="simple_ao_resolve_signed_test-0"></a>

### simple_ao_resolve_signed_test/0 * ###

`simple_ao_resolve_signed_test() -> any()`

<a name="simple_ao_resolve_unsigned_test-0"></a>

### simple_ao_resolve_unsigned_test/0 * ###

`simple_ao_resolve_unsigned_test() -> any()`

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<a name="wasm_compute_request-3"></a>

### wasm_compute_request/3 * ###

`wasm_compute_request(ImageFile, Func, Params) -> any()`

<a name="wasm_compute_request-4"></a>

### wasm_compute_request/4 * ###

`wasm_compute_request(ImageFile, Func, Params, ResultPath) -> any()`

