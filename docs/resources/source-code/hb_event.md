# [Module hb_event.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_event.erl)




Wrapper for incrementing prometheus counters.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#await_prometheus_started-0">await_prometheus_started/0*</a></td><td>Delay the event server until prometheus is started.</td></tr><tr><td valign="top"><a href="#handle_events-0">handle_events/0*</a></td><td></td></tr><tr><td valign="top"><a href="#handle_tracer-3">handle_tracer/3*</a></td><td></td></tr><tr><td valign="top"><a href="#increment-3">increment/3</a></td><td>Increment the counter for the given topic and message.</td></tr><tr><td valign="top"><a href="#log-1">log/1</a></td><td>Debugging log logging function.</td></tr><tr><td valign="top"><a href="#log-2">log/2</a></td><td></td></tr><tr><td valign="top"><a href="#log-3">log/3</a></td><td></td></tr><tr><td valign="top"><a href="#log-4">log/4</a></td><td></td></tr><tr><td valign="top"><a href="#log-5">log/5</a></td><td></td></tr><tr><td valign="top"><a href="#log-6">log/6</a></td><td></td></tr><tr><td valign="top"><a href="#parse_name-1">parse_name/1*</a></td><td></td></tr><tr><td valign="top"><a href="#server-0">server/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="await_prometheus_started-0"></a>

### await_prometheus_started/0 * ###

`await_prometheus_started() -> any()`

Delay the event server until prometheus is started.

<a name="handle_events-0"></a>

### handle_events/0 * ###

`handle_events() -> any()`

<a name="handle_tracer-3"></a>

### handle_tracer/3 * ###

`handle_tracer(Topic, X, Opts) -> any()`

<a name="increment-3"></a>

### increment/3 ###

`increment(Topic, Message, Opts) -> any()`

Increment the counter for the given topic and message. Registers the
counter if it doesn't exist. If the topic is `global`, the message is ignored.
This means that events must specify a topic if they want to be counted,
filtering debug messages. Similarly, events with a topic that begins with
`debug` are ignored.

<a name="log-1"></a>

### log/1 ###

`log(X) -> any()`

Debugging log logging function. For now, it just prints to standard
error.

<a name="log-2"></a>

### log/2 ###

`log(Topic, X) -> any()`

<a name="log-3"></a>

### log/3 ###

`log(Topic, X, Mod) -> any()`

<a name="log-4"></a>

### log/4 ###

`log(Topic, X, Mod, Func) -> any()`

<a name="log-5"></a>

### log/5 ###

`log(Topic, X, Mod, Func, Line) -> any()`

<a name="log-6"></a>

### log/6 ###

`log(Topic, X, Mod, Func, Line, Opts) -> any()`

<a name="parse_name-1"></a>

### parse_name/1 * ###

`parse_name(Name) -> any()`

<a name="server-0"></a>

### server/0 * ###

`server() -> any()`

