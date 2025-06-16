# [Module hb_http_client.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_http_client.erl)




A wrapper library for gun.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##
This module originates from the Arweave
project, and has been modified for use in HyperBEAM.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#await_response-2">await_response/2*</a></td><td></td></tr><tr><td valign="top"><a href="#dec_prometheus_gauge-1">dec_prometheus_gauge/1*</a></td><td>Safe wrapper for prometheus_gauge:dec/2.</td></tr><tr><td valign="top"><a href="#download_metric-2">download_metric/2*</a></td><td></td></tr><tr><td valign="top"><a href="#get_status_class-1">get_status_class/1*</a></td><td>Return the HTTP status class label for cowboy_requests_total and
gun_requests_total metrics.</td></tr><tr><td valign="top"><a href="#gun_req-3">gun_req/3*</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#httpc_req-3">httpc_req/3*</a></td><td></td></tr><tr><td valign="top"><a href="#inc_prometheus_counter-3">inc_prometheus_counter/3*</a></td><td></td></tr><tr><td valign="top"><a href="#inc_prometheus_gauge-1">inc_prometheus_gauge/1*</a></td><td>Safe wrapper for prometheus_gauge:inc/2.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#init_prometheus-1">init_prometheus/1*</a></td><td></td></tr><tr><td valign="top"><a href="#log-5">log/5*</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_invoke_monitor-2">maybe_invoke_monitor/2*</a></td><td>Invoke the HTTP monitor message with AO-Core, if it is set in the
node message key.</td></tr><tr><td valign="top"><a href="#method_to_bin-1">method_to_bin/1*</a></td><td></td></tr><tr><td valign="top"><a href="#open_connection-2">open_connection/2*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_peer-2">parse_peer/2*</a></td><td></td></tr><tr><td valign="top"><a href="#record_duration-2">record_duration/2*</a></td><td>Record the duration of the request in an async process.</td></tr><tr><td valign="top"><a href="#record_response_status-3">record_response_status/3*</a></td><td></td></tr><tr><td valign="top"><a href="#reply_error-2">reply_error/2*</a></td><td></td></tr><tr><td valign="top"><a href="#req-2">req/2</a></td><td></td></tr><tr><td valign="top"><a href="#req-3">req/3*</a></td><td></td></tr><tr><td valign="top"><a href="#request-3">request/3*</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#upload_metric-1">upload_metric/1*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="await_response-2"></a>

### await_response/2 * ###

`await_response(Args, Opts) -> any()`

<a name="dec_prometheus_gauge-1"></a>

### dec_prometheus_gauge/1 * ###

`dec_prometheus_gauge(Name) -> any()`

Safe wrapper for prometheus_gauge:dec/2.

<a name="download_metric-2"></a>

### download_metric/2 * ###

`download_metric(Data, X2) -> any()`

<a name="get_status_class-1"></a>

### get_status_class/1 * ###

`get_status_class(Data) -> any()`

Return the HTTP status class label for cowboy_requests_total and
gun_requests_total metrics.

<a name="gun_req-3"></a>

### gun_req/3 * ###

`gun_req(Args, ReestablishedConnection, Opts) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Cast, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Message, State) -> any()`

<a name="httpc_req-3"></a>

### httpc_req/3 * ###

`httpc_req(Args, X2, Opts) -> any()`

<a name="inc_prometheus_counter-3"></a>

### inc_prometheus_counter/3 * ###

`inc_prometheus_counter(Name, Labels, Value) -> any()`

<a name="inc_prometheus_gauge-1"></a>

### inc_prometheus_gauge/1 * ###

`inc_prometheus_gauge(Name) -> any()`

Safe wrapper for prometheus_gauge:inc/2.

<a name="init-1"></a>

### init/1 ###

`init(Opts) -> any()`

<a name="init_prometheus-1"></a>

### init_prometheus/1 * ###

`init_prometheus(Opts) -> any()`

<a name="log-5"></a>

### log/5 * ###

`log(Type, Event, X3, Reason, Opts) -> any()`

<a name="maybe_invoke_monitor-2"></a>

### maybe_invoke_monitor/2 * ###

`maybe_invoke_monitor(Details, Opts) -> any()`

Invoke the HTTP monitor message with AO-Core, if it is set in the
node message key. We invoke the given message with the `body` set to a signed
version of the details. This allows node operators to configure their machine
to record duration statistics into customized data stores, computations, or
processes etc. Additionally, we include the `http_reference` value, if set in
the given `opts`.

We use `hb_ao:get` rather than `hb_opts:get`, as settings configured
by the `~router@1.0` route `opts` key are unable to generate atoms.

<a name="method_to_bin-1"></a>

### method_to_bin/1 * ###

`method_to_bin(X1) -> any()`

<a name="open_connection-2"></a>

### open_connection/2 * ###

`open_connection(X1, Opts) -> any()`

<a name="parse_peer-2"></a>

### parse_peer/2 * ###

`parse_peer(Peer, Opts) -> any()`

<a name="record_duration-2"></a>

### record_duration/2 * ###

`record_duration(Details, Opts) -> any()`

Record the duration of the request in an async process. We write the
data to prometheus if the application is enabled, as well as invoking the
`http_monitor` if appropriate.

<a name="record_response_status-3"></a>

### record_response_status/3 * ###

`record_response_status(Method, Path, Response) -> any()`

<a name="reply_error-2"></a>

### reply_error/2 * ###

`reply_error(PendingRequests, Reason) -> any()`

<a name="req-2"></a>

### req/2 ###

`req(Args, Opts) -> any()`

<a name="req-3"></a>

### req/3 * ###

`req(Args, ReestablishedConnection, Opts) -> any()`

<a name="request-3"></a>

### request/3 * ###

`request(PID, Args, Opts) -> any()`

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Opts) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="upload_metric-1"></a>

### upload_metric/1 * ###

`upload_metric(X1) -> any()`

