# [Module dev_scheduler_cache.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_scheduler_cache.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#latest-2">latest/2</a></td><td>Get the latest assignment from the cache.</td></tr><tr><td valign="top"><a href="#list-2">list/2</a></td><td>Get the assignments for a process.</td></tr><tr><td valign="top"><a href="#read-3">read/3</a></td><td>Get an assignment message from the cache.</td></tr><tr><td valign="top"><a href="#read_location-2">read_location/2</a></td><td>Read the latest known scheduler location for an address.</td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td>Write an assignment message into the cache.</td></tr><tr><td valign="top"><a href="#write_location-2">write_location/2</a></td><td>Write the latest known scheduler location for an address.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="latest-2"></a>

### latest/2 ###

`latest(ProcID, Opts) -> any()`

Get the latest assignment from the cache.

<a name="list-2"></a>

### list/2 ###

`list(ProcID, Opts) -> any()`

Get the assignments for a process.

<a name="read-3"></a>

### read/3 ###

`read(ProcID, Slot, Opts) -> any()`

Get an assignment message from the cache.

<a name="read_location-2"></a>

### read_location/2 ###

`read_location(Address, Opts) -> any()`

Read the latest known scheduler location for an address.

<a name="write-2"></a>

### write/2 ###

`write(Assignment, Opts) -> any()`

Write an assignment message into the cache.

<a name="write_location-2"></a>

### write_location/2 ###

`write_location(LocMsg, Opts) -> any()`

Write the latest known scheduler location for an address.

