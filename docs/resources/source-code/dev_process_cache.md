# [Module dev_process_cache.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_process_cache.erl)




A wrapper around the hb_cache module that provides a more
convenient interface for reading the result of a process at a given slot or
message ID.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#find_latest_outputs-1">find_latest_outputs/1*</a></td><td>Test for retrieving the latest computed output for a process.</td></tr><tr><td valign="top"><a href="#first_with_path-4">first_with_path/4*</a></td><td>Find the latest assignment with the requested path suffix.</td></tr><tr><td valign="top"><a href="#first_with_path-5">first_with_path/5*</a></td><td></td></tr><tr><td valign="top"><a href="#latest-2">latest/2</a></td><td>Retrieve the latest slot for a given process.</td></tr><tr><td valign="top"><a href="#latest-3">latest/3</a></td><td></td></tr><tr><td valign="top"><a href="#latest-4">latest/4</a></td><td></td></tr><tr><td valign="top"><a href="#path-3">path/3*</a></td><td>Calculate the path of a result, given a process ID and a slot.</td></tr><tr><td valign="top"><a href="#path-4">path/4*</a></td><td></td></tr><tr><td valign="top"><a href="#process_cache_suite_test_-0">process_cache_suite_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Read the result of a process at a given slot.</td></tr><tr><td valign="top"><a href="#read-3">read/3</a></td><td></td></tr><tr><td valign="top"><a href="#test_write_and_read_output-1">test_write_and_read_output/1*</a></td><td>Test for writing multiple computed outputs, then getting them by
their slot number and by their signed and unsigned IDs.</td></tr><tr><td valign="top"><a href="#write-4">write/4</a></td><td>Write a process computation result to the cache.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="find_latest_outputs-1"></a>

### find_latest_outputs/1 * ###

`find_latest_outputs(Opts) -> any()`

Test for retrieving the latest computed output for a process.

<a name="first_with_path-4"></a>

### first_with_path/4 * ###

`first_with_path(ProcID, RequiredPath, Slots, Opts) -> any()`

Find the latest assignment with the requested path suffix.

<a name="first_with_path-5"></a>

### first_with_path/5 * ###

`first_with_path(ProcID, Required, Rest, Opts, Store) -> any()`

<a name="latest-2"></a>

### latest/2 ###

`latest(ProcID, Opts) -> any()`

Retrieve the latest slot for a given process. Optionally state a limit
on the slot number to search for, as well as a required path that the slot
must have.

<a name="latest-3"></a>

### latest/3 ###

`latest(ProcID, RequiredPath, Opts) -> any()`

<a name="latest-4"></a>

### latest/4 ###

`latest(ProcID, RawRequiredPath, Limit, Opts) -> any()`

<a name="path-3"></a>

### path/3 * ###

`path(ProcID, Ref, Opts) -> any()`

Calculate the path of a result, given a process ID and a slot.

<a name="path-4"></a>

### path/4 * ###

`path(ProcID, Ref, PathSuffix, Opts) -> any()`

<a name="process_cache_suite_test_-0"></a>

### process_cache_suite_test_/0 * ###

`process_cache_suite_test_() -> any()`

<a name="read-2"></a>

### read/2 ###

`read(ProcID, Opts) -> any()`

Read the result of a process at a given slot.

<a name="read-3"></a>

### read/3 ###

`read(ProcID, SlotRef, Opts) -> any()`

<a name="test_write_and_read_output-1"></a>

### test_write_and_read_output/1 * ###

`test_write_and_read_output(Opts) -> any()`

Test for writing multiple computed outputs, then getting them by
their slot number and by their signed and unsigned IDs.

<a name="write-4"></a>

### write/4 ###

`write(ProcID, Slot, Msg, Opts) -> any()`

Write a process computation result to the cache.

