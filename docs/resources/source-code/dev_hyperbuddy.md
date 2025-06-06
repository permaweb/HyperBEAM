# [Module dev_hyperbuddy.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_hyperbuddy.erl)




A device that renders a REPL-like interface for AO-Core via HTML.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-3">format/3</a></td><td>Employ HyperBEAM's internal pretty printer to format a message.</td></tr><tr><td valign="top"><a href="#info-0">info/0</a></td><td>Export an explicit list of files via http.</td></tr><tr><td valign="top"><a href="#metrics-3">metrics/3</a></td><td>The main HTML page for the REPL device.</td></tr><tr><td valign="top"><a href="#return_file-1">return_file/1*</a></td><td>Read a file from disk and serve it as a static HTML page.</td></tr><tr><td valign="top"><a href="#serve-4">serve/4*</a></td><td>Serve a file from the priv directory.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format-3"></a>

### format/3 ###

`format(Base, X2, X3) -> any()`

Employ HyperBEAM's internal pretty printer to format a message.

<a name="info-0"></a>

### info/0 ###

`info() -> any()`

Export an explicit list of files via http.

<a name="metrics-3"></a>

### metrics/3 ###

`metrics(X1, Req, Opts) -> any()`

The main HTML page for the REPL device.

<a name="return_file-1"></a>

### return_file/1 * ###

`return_file(Name) -> any()`

Read a file from disk and serve it as a static HTML page.

<a name="serve-4"></a>

### serve/4 * ###

`serve(Key, M1, M2, Opts) -> any()`

Serve a file from the priv directory. Only serves files that are explicitly
listed in the `routes` field of the `info/0` return value.

