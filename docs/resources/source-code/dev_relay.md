# [Module dev_relay.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_relay.erl)




This module implements the relay device, which is responsible for
relaying messages between nodes and other HTTP(S) endpoints.

<a name="description"></a>

## Description ##

It can be called in either `call` or `cast` mode. In `call` mode, it
returns a `{ok, Result}` tuple, where `Result` is the response from the
remote peer to the message sent. In `cast` mode, the invocation returns
immediately, and the message is relayed asynchronously. No response is given
and the device returns `{ok, <<"OK">>}`.

Example usage:

```

       curl /~relay@.1.0/call?method=GET?0.path=https://www.arweave.net/
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-3">call/3</a></td><td>Execute a <code>call</code> request using a node's routes.</td></tr><tr><td valign="top"><a href="#call_get_test-0">call_get_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#cast-3">cast/3</a></td><td>Execute a request in the same way as <code>call/3</code>, but asynchronously.</td></tr><tr><td valign="top"><a href="#request-3">request/3</a></td><td>Preprocess a request to check if it should be relayed to a different node.</td></tr><tr><td valign="top"><a href="#request_hook_reroute_to_nearest_test-0">request_hook_reroute_to_nearest_test/0*</a></td><td>Test that the <code>preprocess/3</code> function re-routes a request to remote
peers, according to the node's routing table.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-3"></a>

### call/3 ###

`call(M1, RawM2, Opts) -> any()`

Execute a `call` request using a node's routes.

Supports the following options:
- `target`: The target message to relay. Defaults to the original message.
- `relay-path`: The path to relay the message to. Defaults to the original path.
- `method`: The method to use for the request. Defaults to the original method.
- `requires-sign`: Whether the request requires signing before dispatching.
Defaults to `false`.

<a name="call_get_test-0"></a>

### call_get_test/0 * ###

`call_get_test() -> any()`

<a name="cast-3"></a>

### cast/3 ###

`cast(M1, M2, Opts) -> any()`

Execute a request in the same way as `call/3`, but asynchronously. Always
returns `<<"OK">>`.

<a name="request-3"></a>

### request/3 ###

`request(Msg1, Msg2, Opts) -> any()`

Preprocess a request to check if it should be relayed to a different node.

<a name="request_hook_reroute_to_nearest_test-0"></a>

### request_hook_reroute_to_nearest_test/0 * ###

`request_hook_reroute_to_nearest_test() -> any()`

Test that the `preprocess/3` function re-routes a request to remote
peers, according to the node's routing table.

