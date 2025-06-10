# [Module hb_beamr_io.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_beamr_io.erl)




Simple interface for memory management for Beamr instances.

<a name="description"></a>

## Description ##

It allows for reading and writing to memory, as well as allocating and
freeing memory by calling the WASM module's exported malloc and free
functions.

Unlike the majority of HyperBEAM modules, this module takes a defensive
approach to type checking, breaking from the conventional Erlang style,
such that failures are caught in the Erlang-side of functions rather than
in the C/WASM-side.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#do_read_string-3">do_read_string/3*</a></td><td></td></tr><tr><td valign="top"><a href="#free-2">free/2</a></td><td>Free space allocated in the Beamr instance's native memory via a
call to the exported free function from the WASM.</td></tr><tr><td valign="top"><a href="#malloc-2">malloc/2</a></td><td>Allocate space for (via an exported malloc function from the WASM) in
the Beamr instance's native memory.</td></tr><tr><td valign="top"><a href="#malloc_test-0">malloc_test/0*</a></td><td>Test allocating and freeing memory.</td></tr><tr><td valign="top"><a href="#read-3">read/3</a></td><td>Read a binary from the Beamr instance's native memory at a given offset
and of a given size.</td></tr><tr><td valign="top"><a href="#read_string-2">read_string/2</a></td><td>Simple helper function to read a string from the Beamr instance's native
memory at a given offset.</td></tr><tr><td valign="top"><a href="#read_string-3">read_string/3*</a></td><td></td></tr><tr><td valign="top"><a href="#read_test-0">read_test/0*</a></td><td>Test reading memory in and out of bounds.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>Get the size (in bytes) of the native memory allocated in the Beamr
instance.</td></tr><tr><td valign="top"><a href="#size_test-0">size_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#string_write_and_read_test-0">string_write_and_read_test/0*</a></td><td>Write and read strings to memory.</td></tr><tr><td valign="top"><a href="#write-3">write/3</a></td><td>Write a binary to the Beamr instance's native memory at a given offset.</td></tr><tr><td valign="top"><a href="#write_string-2">write_string/2</a></td><td>Simple helper function to allocate space for (via malloc) and write a
string to the Beamr instance's native memory.</td></tr><tr><td valign="top"><a href="#write_test-0">write_test/0*</a></td><td>Test writing memory in and out of bounds.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="do_read_string-3"></a>

### do_read_string/3 * ###

`do_read_string(WASM, Offset, ChunkSize) -> any()`

<a name="free-2"></a>

### free/2 ###

`free(WASM, Ptr) -> any()`

Free space allocated in the Beamr instance's native memory via a
call to the exported free function from the WASM.

<a name="malloc-2"></a>

### malloc/2 ###

`malloc(WASM, Size) -> any()`

Allocate space for (via an exported malloc function from the WASM) in
the Beamr instance's native memory.

<a name="malloc_test-0"></a>

### malloc_test/0 * ###

`malloc_test() -> any()`

Test allocating and freeing memory.

<a name="read-3"></a>

### read/3 ###

`read(WASM, Offset, Size) -> any()`

Read a binary from the Beamr instance's native memory at a given offset
and of a given size.

<a name="read_string-2"></a>

### read_string/2 ###

`read_string(Port, Offset) -> any()`

Simple helper function to read a string from the Beamr instance's native
memory at a given offset. Memory is read by default in chunks of 8 bytes,
but this can be overridden by passing a different chunk size. Strings are
assumed to be null-terminated.

<a name="read_string-3"></a>

### read_string/3 * ###

`read_string(WASM, Offset, ChunkSize) -> any()`

<a name="read_test-0"></a>

### read_test/0 * ###

`read_test() -> any()`

Test reading memory in and out of bounds.

<a name="size-1"></a>

### size/1 ###

`size(WASM) -> any()`

Get the size (in bytes) of the native memory allocated in the Beamr
instance. Note that WASM memory can never be reduced once granted to an
instance (although it can, of course, be reallocated _inside_ the
environment).

<a name="size_test-0"></a>

### size_test/0 * ###

`size_test() -> any()`

<a name="string_write_and_read_test-0"></a>

### string_write_and_read_test/0 * ###

`string_write_and_read_test() -> any()`

Write and read strings to memory.

<a name="write-3"></a>

### write/3 ###

`write(WASM, Offset, Data) -> any()`

Write a binary to the Beamr instance's native memory at a given offset.

<a name="write_string-2"></a>

### write_string/2 ###

`write_string(WASM, Data) -> any()`

Simple helper function to allocate space for (via malloc) and write a
string to the Beamr instance's native memory. This can be helpful for easily
pushing a string into the instance, such that the resulting pointer can be
passed to exported functions from the instance.
Assumes that the input is either an iolist or a binary, adding a null byte
to the end of the string.

<a name="write_test-0"></a>

### write_test/0 * ###

`write_test() -> any()`

Test writing memory in and out of bounds.

