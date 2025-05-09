# [Module dev_hook.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_hook.erl)




A generalized interface for `hooking` into HyperBEAM nodes.

<a name="description"></a>

## Description ##

This module allows users to define `hooks` that are executed at various
points in the lifecycle of nodes and message evaluations.

Hooks are maintained in the `node message` options, under the key `on`
key. Each `hook` may have zero or many `handlers` which their request is
executed against. A new `handler` of a hook can be registered by simply
adding a new key to that message. If multiple hooks need to be executed for
a single event, the key's value can be set to a list of hooks.

`hook`s themselves do not need to be added explicitly. Any device can add
a hook by simply executing `dev_hook:on(HookName, Req, Opts)`. This
function is does not affect the hashpath of a message and is not exported on
the device`s API, such that it is not possible to call it directly with
AO-Core resolution.

All handlers are expressed in the form of a message, upon which the hook's
request is evaluated:

AO(HookMsg, Req, Opts) => {Status, Result}

The `Status` and `Result` of the evaluation can be used at the `hook` caller's
discretion. If multiple handlers are to be executed for a single `hook`, the
result of each is used as the input to the next, on the assumption that the
status of the previous is `ok`. If a non-`ok` status is encountered, the
evaluation is halted and the result is returned to the caller. This means
that in most cases, hooks take the form of chainable pipelines of functions,
passing the most pertinent data in the `body` key of both the request and
result. Hook definitions can also set the `hook/result` key to `ignore`, if
the result of the execution should be discarded and the prior value (the
input to the hook) should be used instead. The `hook/commit-request` key can
also be set to `true` if the request should be committed by the node before
execution of the hook.

The default HyperBEAM node implements several useful hooks. They include:

start: Executed when the node starts.
Req/body: The node's initial configuration.
Result/body: The node's possibly updated configuration.
request: Executed when a request is received via the HTTP API.
Req/body: The sequence of messages that the node will evaluate.
Req/request: The raw, unparsed singleton request.
Result/body: The sequence of messages that the node will evaluate.
step: Executed after each message in a sequence has been evaluated.
Req/body: The result of the evaluation.
Result/body: The result of the evaluation.
response: Executed when a response is sent via the HTTP API.
Req/body: The result of the evaluation.
Req/request: The raw, unparsed singleton request that was used to
generate the response.
Result/body: The message to be sent in response to the request.

Additionally, this module implements a traditional device API, allowing the
node operator to register hooks to the node and find those that are
currently active.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#execute_handler-4">execute_handler/4*</a></td><td>Execute a single handler
Handlers are expressed as messages that can be resolved via AO.</td></tr><tr><td valign="top"><a href="#execute_handlers-4">execute_handlers/4*</a></td><td>Execute a list of handlers in sequence.</td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td>Get all handlers for a specific hook from the node message options.</td></tr><tr><td valign="top"><a href="#find-3">find/3</a></td><td></td></tr><tr><td valign="top"><a href="#halt_on_error_test-0">halt_on_error_test/0*</a></td><td>Test that pipeline execution halts on error.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Device API information.</td></tr><tr><td valign="top"><a href="#multiple_handlers_test-0">multiple_handlers_test/0*</a></td><td>Test that multiple handlers form a pipeline.</td></tr><tr><td valign="top"><a href="#no_handlers_test-0">no_handlers_test/0*</a></td><td>Test that hooks with no handlers return the original request.</td></tr><tr><td valign="top"><a href="#on-3">on/3</a></td><td>Execute a named hook with the provided request and options
This function finds all handlers for the hook and evaluates them in sequence.</td></tr><tr><td valign="top"><a href="#single_handler_test-0">single_handler_test/0*</a></td><td>Test that a single handler is executed correctly.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="execute_handler-4"></a>

### execute_handler/4 * ###

`execute_handler(HookName, Handler, Req, Opts) -> any()`

Execute a single handler
Handlers are expressed as messages that can be resolved via AO.

<a name="execute_handlers-4"></a>

### execute_handlers/4 * ###

`execute_handlers(HookName, Rest, Req, Opts) -> any()`

Execute a list of handlers in sequence.
The result of each handler is used as input to the next handler.
If a handler returns a non-ok status, execution is halted.

<a name="find-2"></a>

### find/2 ###

`find(HookName, Opts) -> any()`

Get all handlers for a specific hook from the node message options.
Handlers are stored in the `on` key of this message. The `find/2` variant of
this function only takes a hook name and node message, and is not called
directly via the device API. Instead it is used by `on/3` and other internal
functionality to find handlers when necessary. The `find/3` variant can,
however, be called directly via the device API.

<a name="find-3"></a>

### find/3 ###

`find(Base, Req, Opts) -> any()`

<a name="halt_on_error_test-0"></a>

### halt_on_error_test/0 * ###

`halt_on_error_test() -> any()`

Test that pipeline execution halts on error

<a name="info-1"></a>

### info/1 ###

`info(X1) -> any()`

Device API information

<a name="multiple_handlers_test-0"></a>

### multiple_handlers_test/0 * ###

`multiple_handlers_test() -> any()`

Test that multiple handlers form a pipeline

<a name="no_handlers_test-0"></a>

### no_handlers_test/0 * ###

`no_handlers_test() -> any()`

Test that hooks with no handlers return the original request

<a name="on-3"></a>

### on/3 ###

`on(HookName, Req, Opts) -> any()`

Execute a named hook with the provided request and options
This function finds all handlers for the hook and evaluates them in sequence.
The result of each handler is used as input to the next handler.

<a name="single_handler_test-0"></a>

### single_handler_test/0 * ###

`single_handler_test() -> any()`

Test that a single handler is executed correctly

