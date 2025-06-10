# [Module ar_rate_limiter.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/ar_rate_limiter.erl)




__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#cut_trace-4">cut_trace/4*</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#off-0">off/0</a></td><td>Turn rate limiting off.</td></tr><tr><td valign="top"><a href="#on-0">on/0</a></td><td>Turn rate limiting on.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr><tr><td valign="top"><a href="#throttle-3">throttle/3</a></td><td>Hang until it is safe to make another request to the given Peer with the
given Path.</td></tr><tr><td valign="top"><a href="#throttle2-3">throttle2/3*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="cut_trace-4"></a>

### cut_trace/4 * ###

`cut_trace(N, Trace, Now, Opts) -> any()`

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Request, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Cast, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Message, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(Opts) -> any()`

<a name="off-0"></a>

### off/0 ###

`off() -> any()`

Turn rate limiting off.

<a name="on-0"></a>

### on/0 ###

`on() -> any()`

Turn rate limiting on.

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Opts) -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

<a name="throttle-3"></a>

### throttle/3 ###

`throttle(Peer, Path, Opts) -> any()`

Hang until it is safe to make another request to the given Peer with the
given Path. The limits are configured in include/ar_blacklist_middleware.hrl.

<a name="throttle2-3"></a>

### throttle2/3 * ###

`throttle2(Peer, Path, Opts) -> any()`

