# [Module dev_genesis_wasm.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_genesis_wasm.erl)




A device that mimics an environment suitable for `legacynet` AO
processes, using HyperBEAM infrastructure.

<a name="description"></a>

## Description ##
This allows existing `legacynet`
AO process definitions to be used in HyperBEAM.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#collect_events-1">collect_events/1*</a></td><td>Collect events from the port and log them.</td></tr><tr><td valign="top"><a href="#collect_events-2">collect_events/2*</a></td><td></td></tr><tr><td valign="top"><a href="#compute-3">compute/3</a></td><td>All the <code>delegated-compute@1.0</code> device to execute the request.</td></tr><tr><td valign="top"><a href="#ensure_started-1">ensure_started/1*</a></td><td>Ensure the local <code>genesis-wasm@1.0</code> is live.</td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>Initialize the device.</td></tr><tr><td valign="top"><a href="#is_genesis_wasm_server_running-1">is_genesis_wasm_server_running/1*</a></td><td>Check if the genesis-wasm server is running, using the cached process ID
if available.</td></tr><tr><td valign="top"><a href="#log_server_events-1">log_server_events/1*</a></td><td>Log lines of output from the genesis-wasm server.</td></tr><tr><td valign="top"><a href="#normalize-3">normalize/3</a></td><td>Normalize the device.</td></tr><tr><td valign="top"><a href="#snapshot-3">snapshot/3</a></td><td>Snapshot the device.</td></tr><tr><td valign="top"><a href="#status-1">status/1*</a></td><td>Check if the genesis-wasm server is running by requesting its status
endpoint.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="collect_events-1"></a>

### collect_events/1 * ###

`collect_events(Port) -> any()`

Collect events from the port and log them.

<a name="collect_events-2"></a>

### collect_events/2 * ###

`collect_events(Port, Acc) -> any()`

<a name="compute-3"></a>

### compute/3 ###

`compute(Msg, Msg2, Opts) -> any()`

All the `delegated-compute@1.0` device to execute the request. We then apply
the `patch@1.0` device, applying any state patches that the AO process may have
requested.

<a name="ensure_started-1"></a>

### ensure_started/1 * ###

`ensure_started(Opts) -> any()`

Ensure the local `genesis-wasm@1.0` is live. If it not, start it.

<a name="init-3"></a>

### init/3 ###

`init(Msg, Msg2, Opts) -> any()`

Initialize the device.

<a name="is_genesis_wasm_server_running-1"></a>

### is_genesis_wasm_server_running/1 * ###

`is_genesis_wasm_server_running(Opts) -> any()`

Check if the genesis-wasm server is running, using the cached process ID
if available.

<a name="log_server_events-1"></a>

### log_server_events/1 * ###

`log_server_events(Bin) -> any()`

Log lines of output from the genesis-wasm server.

<a name="normalize-3"></a>

### normalize/3 ###

`normalize(Msg, Msg2, Opts) -> any()`

Normalize the device.

<a name="snapshot-3"></a>

### snapshot/3 ###

`snapshot(Msg, Msg2, Opts) -> any()`

Snapshot the device.

<a name="status-1"></a>

### status/1 * ###

`status(Opts) -> any()`

Check if the genesis-wasm server is running by requesting its status
endpoint.

