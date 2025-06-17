# [Module dev_wasi.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_wasi.erl)




A virtual filesystem device.

<a name="description"></a>

## Description ##
Implements a file-system-as-map structure, which is traversible externally.
Each file is a binary and each directory is an AO-Core message.
Additionally, this module adds a series of WASI-preview-1 compatible
functions for accessing the filesystem as imported functions by WASM
modules.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#basic_aos_exec_test-0">basic_aos_exec_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#clock_time_get-3">clock_time_get/3</a></td><td></td></tr><tr><td valign="top"><a href="#compute-1">compute/1</a></td><td></td></tr><tr><td valign="top"><a href="#fd_read-3">fd_read/3</a></td><td>Read from a file using the WASI-p1 standard interface.</td></tr><tr><td valign="top"><a href="#fd_read-5">fd_read/5*</a></td><td></td></tr><tr><td valign="top"><a href="#fd_write-3">fd_write/3</a></td><td>WASM stdlib implementation of <code>fd_write</code>, using the WASI-p1 standard
interface.</td></tr><tr><td valign="top"><a href="#fd_write-5">fd_write/5*</a></td><td></td></tr><tr><td valign="top"><a href="#gen_test_aos_msg-1">gen_test_aos_msg/1*</a></td><td></td></tr><tr><td valign="top"><a href="#gen_test_env-0">gen_test_env/0*</a></td><td></td></tr><tr><td valign="top"><a href="#generate_wasi_stack-3">generate_wasi_stack/3*</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0*</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>On-boot, initialize the virtual file system with:
- Empty stdio files
- WASI-preview-1 compatible functions for accessing the filesystem
- File descriptors for those files.</td></tr><tr><td valign="top"><a href="#parse_iovec-2">parse_iovec/2*</a></td><td>Parse an iovec in WASI-preview-1 format.</td></tr><tr><td valign="top"><a href="#path_open-3">path_open/3</a></td><td>Adds a file descriptor to the state message.</td></tr><tr><td valign="top"><a href="#stdout-1">stdout/1</a></td><td>Return the stdout buffer from a state message.</td></tr><tr><td valign="top"><a href="#vfs_is_serializable_test-0">vfs_is_serializable_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#wasi_stack_is_serializable_test-0">wasi_stack_is_serializable_test/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="basic_aos_exec_test-0"></a>

### basic_aos_exec_test/0 * ###

`basic_aos_exec_test() -> any()`

<a name="clock_time_get-3"></a>

### clock_time_get/3 ###

`clock_time_get(Msg1, Msg2, Opts) -> any()`

<a name="compute-1"></a>

### compute/1 ###

`compute(Msg1) -> any()`

<a name="fd_read-3"></a>

### fd_read/3 ###

`fd_read(Msg1, Msg2, Opts) -> any()`

Read from a file using the WASI-p1 standard interface.

<a name="fd_read-5"></a>

### fd_read/5 * ###

`fd_read(S, Instance, X3, BytesRead, Opts) -> any()`

<a name="fd_write-3"></a>

### fd_write/3 ###

`fd_write(Msg1, Msg2, Opts) -> any()`

WASM stdlib implementation of `fd_write`, using the WASI-p1 standard
interface.

<a name="fd_write-5"></a>

### fd_write/5 * ###

`fd_write(S, Instance, X3, BytesWritten, Opts) -> any()`

<a name="gen_test_aos_msg-1"></a>

### gen_test_aos_msg/1 * ###

`gen_test_aos_msg(Command) -> any()`

<a name="gen_test_env-0"></a>

### gen_test_env/0 * ###

`gen_test_env() -> any()`

<a name="generate_wasi_stack-3"></a>

### generate_wasi_stack/3 * ###

`generate_wasi_stack(File, Func, Params) -> any()`

<a name="init-0"></a>

### init/0 * ###

`init() -> any()`

<a name="init-3"></a>

### init/3 ###

`init(M1, M2, Opts) -> any()`

On-boot, initialize the virtual file system with:
- Empty stdio files
- WASI-preview-1 compatible functions for accessing the filesystem
- File descriptors for those files.

<a name="parse_iovec-2"></a>

### parse_iovec/2 * ###

`parse_iovec(Instance, Ptr) -> any()`

Parse an iovec in WASI-preview-1 format.

<a name="path_open-3"></a>

### path_open/3 ###

`path_open(Msg1, Msg2, Opts) -> any()`

Adds a file descriptor to the state message.
path_open(M, Instance, [FDPtr, LookupFlag, PathPtr|_]) ->

<a name="stdout-1"></a>

### stdout/1 ###

`stdout(M) -> any()`

Return the stdout buffer from a state message.

<a name="vfs_is_serializable_test-0"></a>

### vfs_is_serializable_test/0 * ###

`vfs_is_serializable_test() -> any()`

<a name="wasi_stack_is_serializable_test-0"></a>

### wasi_stack_is_serializable_test/0 * ###

`wasi_stack_is_serializable_test() -> any()`

