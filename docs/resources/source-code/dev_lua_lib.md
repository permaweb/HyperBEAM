# [Module dev_lua_lib.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_lua_lib.erl)




A module for providing AO library functions to the Lua environment.

<a name="description"></a>

## Description ##

This module contains the implementation of the functions, each by the name
that should be used in the `ao` table in the Lua environment. Every export
is imported into the Lua environment.

Each function adheres closely to the Luerl calling convention, adding the
appropriate node message as a third argument:

fun(Args, State, NodeMsg) -> {ResultTerms, NewState}

As Lua allows for multiple return values, each function returns a list of
terms to grant to the caller. Matching the tuple convention used by AO-Core,
the first term is typically the status, and the second term is the result.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#convert_as-1">convert_as/1*</a></td><td>Converts any <code>as</code> terms from Lua to their HyperBEAM equivalents.</td></tr><tr><td valign="top"><a href="#event-3">event/3</a></td><td>Allows Lua scripts to signal events using the HyperBEAM hosts internal
event system.</td></tr><tr><td valign="top"><a href="#install-3">install/3</a></td><td>Install the library into the given Lua environment.</td></tr><tr><td valign="top"><a href="#resolve-3">resolve/3</a></td><td>A wrapper function for performing AO-Core resolutions.</td></tr><tr><td valign="top"><a href="#return-2">return/2*</a></td><td>Helper function for returning a result from a Lua function.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Wrapper for <code>hb_ao</code>'s <code>set</code> functionality.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="convert_as-1"></a>

### convert_as/1 * ###

`convert_as(Other) -> any()`

Converts any `as` terms from Lua to their HyperBEAM equivalents.

<a name="event-3"></a>

### event/3 ###

`event(X1, ExecState, Opts) -> any()`

Allows Lua scripts to signal events using the HyperBEAM hosts internal
event system.

<a name="install-3"></a>

### install/3 ###

`install(Base, State, Opts) -> any()`

Install the library into the given Lua environment.

<a name="resolve-3"></a>

### resolve/3 ###

`resolve(Msgs, ExecState, ExecOpts) -> any()`

A wrapper function for performing AO-Core resolutions. Offers both the
single-message (using `hb_singleton:from/1` to parse) and multiple-message
(using `hb_ao:resolve_many/2`) variants.

<a name="return-2"></a>

### return/2 * ###

`return(Result, ExecState) -> any()`

Helper function for returning a result from a Lua function.

<a name="set-3"></a>

### set/3 ###

`set(X1, ExecState, ExecOpts) -> any()`

Wrapper for `hb_ao`'s `set` functionality.

