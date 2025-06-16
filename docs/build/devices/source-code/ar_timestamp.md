# [Module ar_timestamp.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/ar_timestamp.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cache-1">cache/1*</a></td><td>Cache the current timestamp from Arweave.</td></tr><tr><td valign="top"><a href="#get-0">get/0</a></td><td>Get the current timestamp from the server, starting the server if it
isn't already running.</td></tr><tr><td valign="top"><a href="#refresher-1">refresher/1*</a></td><td>Refresh the timestamp cache periodically.</td></tr><tr><td valign="top"><a href="#spawn_server-0">spawn_server/0*</a></td><td>Spawn a new server and its refresher.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Check if the server is already running, and if not, start it.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cache-1"></a>

### cache/1 * ###

`cache(Current) -> any()`

Cache the current timestamp from Arweave.

<a name="get-0"></a>

### get/0 ###

`get() -> any()`

Get the current timestamp from the server, starting the server if it
isn't already running.

<a name="refresher-1"></a>

### refresher/1 * ###

`refresher(TSServer) -> any()`

Refresh the timestamp cache periodically.

<a name="spawn_server-0"></a>

### spawn_server/0 * ###

`spawn_server() -> any()`

Spawn a new server and its refresher.

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Check if the server is already running, and if not, start it.

