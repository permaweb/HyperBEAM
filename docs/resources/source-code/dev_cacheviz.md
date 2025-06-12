# [Module dev_cacheviz.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_cacheviz.erl)




A device that generates renders (or renderable dot output) of a node's
cache.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dot-3">dot/3</a></td><td>Output the dot representation of the cache, or a specific path within
the cache set by the <code>target</code> key in the request.</td></tr><tr><td valign="top"><a href="#index-3">index/3</a></td><td>Return a renderer in HTML form for the JSON format.</td></tr><tr><td valign="top"><a href="#js-3">js/3</a></td><td>Return a JS library that can be used to render the JSON format.</td></tr><tr><td valign="top"><a href="#json-3">json/3</a></td><td>Return a JSON representation of the cache graph, suitable for use with
the <code>graph.js</code> library.</td></tr><tr><td valign="top"><a href="#svg-3">svg/3</a></td><td>Output the SVG representation of the cache, or a specific path within
the cache set by the <code>target</code> key in the request.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dot-3"></a>

### dot/3 ###

`dot(X1, Req, Opts) -> any()`

Output the dot representation of the cache, or a specific path within
the cache set by the `target` key in the request.

<a name="index-3"></a>

### index/3 ###

`index(Base, X2, Opts) -> any()`

Return a renderer in HTML form for the JSON format.

<a name="js-3"></a>

### js/3 ###

`js(X1, X2, Opts) -> any()`

Return a JS library that can be used to render the JSON format.

<a name="json-3"></a>

### json/3 ###

`json(Base, Req, Opts) -> any()`

Return a JSON representation of the cache graph, suitable for use with
the `graph.js` library. If the request specifies a `target` key, we use that
target. Otherwise, we generate a new target by writing the message to the
cache and using the ID of the written message.

<a name="svg-3"></a>

### svg/3 ###

`svg(Base, Req, Opts) -> any()`

Output the SVG representation of the cache, or a specific path within
the cache set by the `target` key in the request.

