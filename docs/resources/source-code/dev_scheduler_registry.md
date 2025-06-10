# [Module dev_scheduler_registry.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_scheduler_registry.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#create_and_find_process_test-0">create_and_find_process_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#create_multiple_processes_test-0">create_multiple_processes_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#find-1">find/1</a></td><td>Find a process associated with the processor ID in the local registry
If the process is not found, it will not create a new one.</td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td>Find a process associated with the processor ID in the local registry
If the process is not found and <code>GenIfNotHosted</code> is true, it attemps to create a new one.</td></tr><tr><td valign="top"><a href="#find-3">find/3</a></td><td>Same as <code>find/2</code> but with additional options passed when spawning a new process (if needed).</td></tr><tr><td valign="top"><a href="#find_non_existent_process_test-0">find_non_existent_process_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#get_all_processes_test-0">get_all_processes_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#get_processes-0">get_processes/0</a></td><td>Return a list of all currently registered ProcID.</td></tr><tr><td valign="top"><a href="#get_wallet-0">get_wallet/0</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_new_proc-3">maybe_new_proc/3*</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="create_and_find_process_test-0"></a>

### create_and_find_process_test/0 * ###

`create_and_find_process_test() -> any()`

<a name="create_multiple_processes_test-0"></a>

### create_multiple_processes_test/0 * ###

`create_multiple_processes_test() -> any()`

<a name="find-1"></a>

### find/1 ###

`find(ProcID) -> any()`

Find a process associated with the processor ID in the local registry
If the process is not found, it will not create a new one

<a name="find-2"></a>

### find/2 ###

`find(ProcID, GenIfNotHosted) -> any()`

Find a process associated with the processor ID in the local registry
If the process is not found and `GenIfNotHosted` is true, it attemps to create a new one

<a name="find-3"></a>

### find/3 ###

`find(ProcID, GenIfNotHosted, Opts) -> any()`

Same as `find/2` but with additional options passed when spawning a new process (if needed)

<a name="find_non_existent_process_test-0"></a>

### find_non_existent_process_test/0 * ###

`find_non_existent_process_test() -> any()`

<a name="get_all_processes_test-0"></a>

### get_all_processes_test/0 * ###

`get_all_processes_test() -> any()`

<a name="get_processes-0"></a>

### get_processes/0 ###

`get_processes() -> any()`

Return a list of all currently registered ProcID.

<a name="get_wallet-0"></a>

### get_wallet/0 ###

`get_wallet() -> any()`

<a name="maybe_new_proc-3"></a>

### maybe_new_proc/3 * ###

`maybe_new_proc(ProcID, GenIfNotHosted, Opts) -> any()`

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

