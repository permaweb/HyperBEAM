# [Module dev_manifest.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_manifest.erl)




An Arweave path manifest resolution device.

<a name="description"></a>

## Description ##
Follows the v1 schema:
https://specs.ar.io/?tx=lXLd0OPwo-dJLB_Amz5jgIeDhiOkjXuM3-r0H_aiNj0<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#info-0">info/0</a></td><td>Use the <code>route/4</code> function as the handler for all requests, aside
from <code>keys</code> and <code>set</code>, which are handled by the default resolver.</td></tr><tr><td valign="top"><a href="#manifest-3">manifest/3*</a></td><td>Find and deserialize a manifest from the given base.</td></tr><tr><td valign="top"><a href="#route-4">route/4*</a></td><td>Route a request to the associated data via its manifest.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="info-0"></a>

### info/0 ###

`info() -> any()`

Use the `route/4` function as the handler for all requests, aside
from `keys` and `set`, which are handled by the default resolver.

<a name="manifest-3"></a>

### manifest/3 * ###

`manifest(Base, Req, Opts) -> any()`

Find and deserialize a manifest from the given base.

<a name="route-4"></a>

### route/4 * ###

`route(Key, M1, M2, Opts) -> any()`

Route a request to the associated data via its manifest.

