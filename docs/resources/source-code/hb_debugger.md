# [Module hb_debugger.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_debugger.erl)




A module that provides bootstrapping interfaces for external debuggers
to connect to HyperBEAM.

<a name="description"></a>

## Description ##

The simplest way to utilize an external graphical debugger is to use the
`erlang-ls` extension for VS Code, Emacs, or other Language Server Protocol
(LSP) compatible editors. This repository contains a `launch.json`
configuration file for VS Code that can be used to spawn a new HyperBEAM,
attach the debugger to it, and execute the specified `Module:Function(Args)`.
Additionally, the node can be started with `rebar3 debugging` in order to
allow access to the console while also allowing the debugger to attach.

Boot time is approximately 10 seconds.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#await_breakpoint-0">await_breakpoint/0</a></td><td>Await a new breakpoint being set by the debugger.</td></tr><tr><td valign="top"><a href="#await_breakpoint-1">await_breakpoint/1*</a></td><td></td></tr><tr><td valign="top"><a href="#await_debugger-0">await_debugger/0*</a></td><td>Await a debugger to be attached to the node.</td></tr><tr><td valign="top"><a href="#await_debugger-1">await_debugger/1*</a></td><td></td></tr><tr><td valign="top"><a href="#interpret-1">interpret/1*</a></td><td>Attempt to interpret a specified module to load it into the debugger.</td></tr><tr><td valign="top"><a href="#is_debugging_node_connected-0">is_debugging_node_connected/0*</a></td><td>Is another Distributed Erlang node connected to us?.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_and_break-2">start_and_break/2</a></td><td>A bootstrapping function to wait for an external debugger to be attached,
then add a breakpoint on the specified <code>Module:Function(Args)</code>, then call it.</td></tr><tr><td valign="top"><a href="#start_and_break-3">start_and_break/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="await_breakpoint-0"></a>

### await_breakpoint/0 ###

`await_breakpoint() -> any()`

Await a new breakpoint being set by the debugger.

<a name="await_breakpoint-1"></a>

### await_breakpoint/1 * ###

`await_breakpoint(N) -> any()`

<a name="await_debugger-0"></a>

### await_debugger/0 * ###

`await_debugger() -> any()`

Await a debugger to be attached to the node.

<a name="await_debugger-1"></a>

### await_debugger/1 * ###

`await_debugger(N) -> any()`

<a name="interpret-1"></a>

### interpret/1 * ###

`interpret(Module) -> any()`

Attempt to interpret a specified module to load it into the debugger.
`int:i/1` seems to have an issue that will cause it to fail sporadically
with `error:undef` on some modules. This error appears not to be catchable
through the normal means. Subsequently, we attempt the load in a separate
process and wait for it to complete. If we do not receive a response in a
reasonable amount of time, we assume that the module failed to load and
return `false`.

<a name="is_debugging_node_connected-0"></a>

### is_debugging_node_connected/0 * ###

`is_debugging_node_connected() -> any()`

Is another Distributed Erlang node connected to us?

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<a name="start_and_break-2"></a>

### start_and_break/2 ###

`start_and_break(Module, Function) -> any()`

A bootstrapping function to wait for an external debugger to be attached,
then add a breakpoint on the specified `Module:Function(Args)`, then call it.

<a name="start_and_break-3"></a>

### start_and_break/3 ###

`start_and_break(Module, Function, Args) -> any()`

