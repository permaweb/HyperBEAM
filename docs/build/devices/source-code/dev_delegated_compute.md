# [Module dev_delegated_compute.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_delegated_compute.erl)




Simple wrapper module that enables compute on remote machines,
implementing the JSON-Iface.

<a name="description"></a>

## Description ##
This can be used either as a standalone, to
bring trusted results into the local node, or as the `Execution-Device` of
an AO process.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compute-3">compute/3</a></td><td></td></tr><tr><td valign="top"><a href="#do_compute-3">do_compute/3*</a></td><td>Execute computation on a remote machine via relay and the JSON-Iface.</td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>Initialize or normalize the compute-lite device.</td></tr><tr><td valign="top"><a href="#normalize-3">normalize/3</a></td><td></td></tr><tr><td valign="top"><a href="#snapshot-3">snapshot/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compute-3"></a>

### compute/3 ###

`compute(Msg1, Msg2, Opts) -> any()`

<a name="do_compute-3"></a>

### do_compute/3 * ###

`do_compute(ProcID, Msg2, Opts) -> any()`

Execute computation on a remote machine via relay and the JSON-Iface.

<a name="init-3"></a>

### init/3 ###

`init(Msg1, Msg2, Opts) -> any()`

Initialize or normalize the compute-lite device. For now, we don't
need to do anything special here.

<a name="normalize-3"></a>

### normalize/3 ###

`normalize(Msg1, Msg2, Opts) -> any()`

<a name="snapshot-3"></a>

### snapshot/3 ###

`snapshot(Msg1, Msg2, Opts) -> any()`

