# [Module hb_tracer.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_tracer.erl)




A module for tracing the flow of requests through the system.

<a name="description"></a>

## Description ##
This allows for tracking the lifecycle of a request from HTTP receipt through processing and response.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#checkmark_emoji-0">checkmark_emoji/0*</a></td><td></td></tr><tr><td valign="top"><a href="#failure_emoji-0">failure_emoji/0*</a></td><td></td></tr><tr><td valign="top"><a href="#format_error_trace-1">format_error_trace/1</a></td><td>Format a trace for error in a user-friendly emoji oriented output.</td></tr><tr><td valign="top"><a href="#get_trace-1">get_trace/1</a></td><td>Exports the complete queue of events.</td></tr><tr><td valign="top"><a href="#record_step-2">record_step/2</a></td><td>Register a new step into a tracer.</td></tr><tr><td valign="top"><a href="#stage_to_emoji-1">stage_to_emoji/1*</a></td><td></td></tr><tr><td valign="top"><a href="#start_trace-0">start_trace/0</a></td><td>Start a new tracer acting as queue of events registered.</td></tr><tr><td valign="top"><a href="#trace_loop-1">trace_loop/1*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="checkmark_emoji-0"></a>

### checkmark_emoji/0 * ###

`checkmark_emoji() -> any()`

<a name="failure_emoji-0"></a>

### failure_emoji/0 * ###

`failure_emoji() -> any()`

<a name="format_error_trace-1"></a>

### format_error_trace/1 ###

`format_error_trace(Trace) -> any()`

Format a trace for error in a user-friendly emoji oriented output

<a name="get_trace-1"></a>

### get_trace/1 ###

`get_trace(TracePID) -> any()`

Exports the complete queue of events

<a name="record_step-2"></a>

### record_step/2 ###

`record_step(TracePID, Step) -> any()`

Register a new step into a tracer

<a name="stage_to_emoji-1"></a>

### stage_to_emoji/1 * ###

`stage_to_emoji(Stage) -> any()`

<a name="start_trace-0"></a>

### start_trace/0 ###

`start_trace() -> any()`

Start a new tracer acting as queue of events registered.

<a name="trace_loop-1"></a>

### trace_loop/1 * ###

`trace_loop(Trace) -> any()`

