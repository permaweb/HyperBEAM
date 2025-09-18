# hb_beamr_io

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_beamr_io.erl)

Simple interface for memory management for Beamr instances.
It allows for reading and writing to memory, as well as allocating and 
freeing memory by calling the WASM module's exported malloc and free
functions.
Unlike the majority of HyperBEAM modules, this module takes a defensive
approach to type checking, breaking from the conventional Erlang style, 
such that failures are caught in the Erlang-side of functions rather than
in the C/WASM-side.

---

## Exported Functions

- `free/2`
- `malloc/2`
- `read_string/2`
- `read/3`
- `size/1`
- `write_string/2`
- `write/3`

---

### size

Simple interface for memory management for Beamr instances.
Get the size (in bytes) of the native memory allocated in the Beamr

```erlang
size(WASM) when is_pid(WASM) ->
    hb_beamr:wasm_send(WASM, {command, term_to_binary({size})}),
    receive
        {execution_result, Size} ->
            {ok, Size}
    end.
```

### write

Write a binary to the Beamr instance's native memory at a given offset.

```erlang
write(WASM, Offset, Data)
        when is_pid(WASM)
        andalso is_binary(Data)
        andalso is_integer(Offset) ->
    ?event(writing_to_mem),
    hb_beamr:wasm_send(WASM, {command, term_to_binary({write, Offset, Data})}),
    ?event(mem_written),
    receive
        ok -> ok;
        {error, Error} -> {error, Error}
    end.
```

### write_string

Simple helper function to allocate space for (via malloc) and write a

```erlang
write_string(WASM, Data) when is_pid(WASM) andalso is_list(Data) ->
    write_string(WASM, iolist_to_binary(Data));
```

### write_string

Simple helper function to allocate space for (via malloc) and write a

```erlang
write_string(WASM, Data) when is_pid(WASM) andalso is_binary(Data) ->
    DataSize = byte_size(Data) + 1,
    String = <<Data/bitstring, 0:8>>,
    case malloc(WASM, DataSize) of
        {ok, Ptr} ->
            case write(WASM, Ptr, String) of
                ok -> {ok, Ptr};
                {error, Error} -> {error, Error}
            end;
        Error -> Error
    end.
```

### read

Read a binary from the Beamr instance's native memory at a given offset

```erlang
read(WASM, Offset, Size)
        when is_pid(WASM)
        andalso is_integer(Offset)
        andalso is_integer(Size) ->
    ?event({read_request, {port, WASM}, {location, Offset}, {size, Size}}),
    hb_beamr:wasm_send(WASM, {command, term_to_binary({read, Offset, Size})}),
    ?event(read_req_sent),
    receive
        {execution_result, Result} ->
            ?event(
                {read_result,
                    {wasm, WASM},
                    {location, Offset},
                    {size, Size},
                    {result, Result}}),
            {ok, Result};
        {error, Error} ->
            {error, Error}
    end.
```

### read_string

Simple helper function to read a string from the Beamr instance's native

```erlang
read_string(Port, Offset) -> read_string(Port, Offset, 8).
```

### read_string

Simple helper function to read a string from the Beamr instance's native

```erlang
read_string(WASM, Offset, ChunkSize)
        when is_pid(WASM)
        andalso is_integer(Offset)
        andalso is_integer(ChunkSize) ->
    {ok, iolist_to_binary(do_read_string(WASM, Offset, ChunkSize))}.
```

### do_read_string

```erlang
do_read_string(WASM, Offset, ChunkSize) ->
    {ok, Data} = read(WASM, Offset, ChunkSize),
    case binary:split(Data, [<<0>>]) of
        [Data|[]] -> [Data|do_read_string(WASM, Offset + ChunkSize, ChunkSize)];
        [FinalData|_Remainder] -> [FinalData]
    end.
```

### malloc

Allocate space for (via an exported malloc function from the WASM) in 

```erlang
malloc(WASM, Size) when is_pid(WASM) andalso is_integer(Size) ->
    case hb_beamr:call(WASM, "malloc", [Size]) of
        {ok, [0]} ->
            ?event({malloc_failed, Size}),
            {error, malloc_failed};
        {ok, [Ptr]} ->
            ?event({malloc_success, Ptr, Size}),
            {ok, Ptr};
        {error, Error} ->
            {error, Error}
    end.
```

### free

Free space allocated in the Beamr instance's native memory via a

```erlang
free(WASM, Ptr) when is_pid(WASM) andalso is_integer(Ptr) ->
    case hb_beamr:call(WASM, "free", [Ptr]) of
        {ok, Res} ->
            ?event({free_result, Res}),
            ok;
        {error, Error} ->
            {error, Error}
    end.
```

### size_test

```erlang
size_test() ->
    WASMPageSize = 65536,
    File1Pages = 1,
    File2Pages = 193,
    {ok, File} = file:read_file("test/test-print.wasm"),
    {ok, WASM, _Imports, _Exports} = hb_beamr:start(File),
    ?assertEqual({ok, WASMPageSize * File1Pages}, hb_beamr_io:size(WASM)),
    hb_beamr:stop(WASM),
    {ok, File2} = file:read_file("test/aos-2-pure-xs.wasm"),
    {ok, WASM2, _Imports2, _Exports2} = hb_beamr:start(File2),
    ?assertEqual({ok, WASMPageSize * File2Pages}, hb_beamr_io:size(WASM2)),
    hb_beamr:stop(WASM2).
```

### write_test

Test writing memory in and out of bounds.

```erlang
write_test() ->
    % Load the `test-print' WASM module, which has a simple print function.
```

### read_test

Test reading memory in and out of bounds.

```erlang
read_test() ->
    % Our `test-print' module is hand-written in WASM, so we know that it
    % has a `Hello, World!` string at precisely offset 66.
```

### malloc_test

Test allocating and freeing memory.

```erlang
malloc_test() ->
    {ok, File} = file:read_file("test/test-calling.wasm"),
    {ok, WASM, _Imports, _Exports} = hb_beamr:start(File),
    % Check that we can allocate memory inside the bounds of the WASM module.
```

### string_write_and_read_test

Write and read strings to memory.

```erlang
string_write_and_read_test() ->
    {ok, File} = file:read_file("test/test-calling.wasm"),
    {ok, WASM, _Imports, _Exports} = hb_beamr:start(File),
    {ok, Ptr} = write_string(WASM, <<"Hello, World!">>),
    ?assertEqual({ok, <<"Hello, World!">>}, read_string(WASM, Ptr)).
```

---

*Generated from [hb_beamr_io.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_beamr_io.erl)*
