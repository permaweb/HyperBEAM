# [Module dev_cacheviz.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_cacheviz.erl)




A device that generates renders (or renderable dot output) of a node's
cache.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dot-3">dot/3</a></td><td>Output the dot representation of the cache, or a specific path within
the cache set by the <code>target</code> key in the request.</td></tr><tr><td valign="top"><a href="#svg-3">svg/3</a></td><td>Output the SVG representation of the cache, or a specific path within
the cache set by the <code>target</code> key in the request.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dot-3"></a>

### dot/3 ###

`dot(X1, Req, Opts) -> any()`

Output the dot representation of the cache, or a specific path within
the cache set by the `target` key in the request.

<a name="svg-3"></a>

### svg/3 ###

`svg(Base, Req, Opts) -> any()`

Output the SVG representation of the cache, or a specific path within
the cache set by the `target` key in the request.

