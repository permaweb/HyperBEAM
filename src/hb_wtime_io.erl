-module(hb_wtime_io).
-export([malloc/2, free/2]).
-export([read_string/2, read_string/3, write_string/2]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").


%% @doc Allocate space for (via an exported malloc function from the Instance) in 
%% the Beamr instance's native memory.
malloc(Instance, Size) when is_reference(Instance) andalso is_integer(Size) ->
    case hb_wtime:call_begin(Instance, <<"malloc">>, [Size]) of
        {ok, complete, [0]} ->
            ?event({malloc_failed, Size}),
            {error, malloc_failed};
        {ok, complete, [Ptr]} ->
            ?event({malloc_success, Ptr, Size}),
            {ok, Ptr};
        {ok, [0]} ->
            ?event({malloc_failed, Size}),
            {error, malloc_failed};
        {ok, [Ptr]} ->
            ?event({malloc_success, Ptr, Size}),
            {ok, Ptr};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Free space allocated in the Beamr instance's native memory via a
%% call to the exported free function from the Instance.
free(Instance, Ptr) when is_reference(Instance) andalso is_integer(Ptr) ->
    case hb_wtime:call_begin(Instance, <<"free">>, [Ptr]) of
        {ok, complete, Res} ->
            ?event({free_result, Res}),
            ok;
        {ok, Res} ->
            ?event({free_result, Res}),
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Simple helper function to read a string from the Instance's native
%% memory at a given offset. Memory is read by default in chunks of 8 bytes,
%% but this can be overridden by passing a different chunk size. Strings are 
%% assumed to be null-terminated.
read_string(Instance, Offset) -> read_string(Instance, Offset, 8).
read_string(Instance, Offset, ChunkSize)
        when is_reference(Instance)
        andalso is_integer(Offset)
        andalso is_integer(ChunkSize) ->
    {ok, iolist_to_binary(do_read_string(Instance, Offset, ChunkSize))}.

do_read_string(Instance, Offset, ChunkSize) ->
    {ok, Data} = hb_wtime:mem_read(Instance, Offset, ChunkSize),
    case binary:split(Data, [<<0>>]) of
        [Data|[]] -> [Data|do_read_string(Instance, Offset + ChunkSize, ChunkSize)];
        [FinalData|_Remainder] -> [FinalData]
    end.

%% @doc Simple helper function to allocate space for (via malloc) and write a
%% string to the Beamr instance's native memory. This can be helpful for easily
%% pushing a string into the instance, such that the resulting pointer can be
%% passed to exported functions from the instance.
%% Assumes that the input is either an iolist or a binary, adding a null byte
%% to the end of the string.
write_string(Instance, Data) when is_reference(Instance) andalso is_list(Data) ->
    write_string(Instance, iolist_to_binary(Data));
write_string(Instance, Data) when is_reference(Instance) andalso is_binary(Data) ->
    DataSize = byte_size(Data) + 1,
    String = <<Data/bitstring, 0:8>>,
    case malloc(Instance, DataSize) of
        {ok, Ptr} ->
            case hb_wtime:mem_write(Instance, Ptr, String) of
                ok -> {ok, Ptr};
                {error, Error} -> {error, Error}
            end;
        Error -> Error
    end.


%%% Tests

size_test() ->
    PageSize = 65536, % 64KiB
    File1Pages = 1,
    File2Pages = 1536,
    {ok, File} = file:read_file("test/test-print.wasm"),
    {ok, Instance} = hb_wtime:create(File),
    ?assertEqual({ok, PageSize * File1Pages}, hb_wtime:mem_size(Instance)),
    {ok, File2} = file:read_file("test/aos-new.wasm"),
    {ok, Instance2} = hb_wtime:create(File2),
    ?assertEqual({ok, PageSize * File2Pages}, hb_wtime:mem_size(Instance2)).

%% @doc Test writing memory in and out of bounds.
write_test() ->
    % Load the `test-print' WASM module, which has a simple print function.
    % We do not call the function here, but instead check that we can write
    % to its memory. It has a single page (65,536 bytes) of memory.
    {ok, File} = file:read_file("test/test-print.wasm"),
    {ok, Instance} = hb_wtime:create(File),
    % Check that we can write memory inside the bounds of the WASM module.
    ?assertEqual(ok, hb_wtime:mem_write(Instance, 0, <<"Hello, World!">>)),
    % Check that we can safely handle out-of-bounds writes.
    ?assertMatch({error, _}, hb_wtime:mem_write(Instance, 1000000, <<"Bad hello world!">>)).

%% @doc Test reading memory in and out of bounds.
read_test() ->
    % Our `test-print' module is hand-written in WASM, so we know that it
    % has a `Hello, World!` string at precisely offset 66.
    {ok, File} = file:read_file("test/test-print.wasm"),
    {ok, Instance} = hb_wtime:create(File),
    % Check that we can read memory inside the bounds of the WASM module.
    ?assertEqual({ok, <<"Hello, World!">>}, hb_wtime:mem_read(Instance, 66, 13)),
    % Check that we can safely handle out-of-bounds reads.
    ?assertMatch({error, _}, hb_wtime:mem_read(Instance, 1000000, 13)).

%% @doc Test allocating and freeing memory.
malloc_test() ->
    {ok, File} = file:read_file("test/test-calling.wasm"),
    {ok, Instance} = hb_wtime:create(File),
    % Check that we can allocate memory inside the bounds of the WASM module.
    Result = malloc(Instance, 100),
    ?assertMatch({ok, _}, Result),
    % TODO: Find a module with `free` function
    % {ok, Ptr} = Result,
    % ?assertEqual(ok, free(Instance, Ptr)),
    % Check that we can safely handle out-of-bounds allocations.
    % The WASM module has a maximum of 259 pages (16MB) of memory, so we
    % should not be able to allocate more than that.
    ?assertMatch({error, _}, malloc(Instance, 128 * 1024 * 1024)).

%% @doc Write and read strings to memory.
string_write_and_read_test() ->
    {ok, File} = file:read_file("test/test-calling.wasm"),
    {ok, Instance} = hb_wtime:create(File),
    {ok, Ptr} = write_string(Instance, <<"Hello, World!">>),
    ?assertEqual({ok, <<"Hello, World!">>}, read_string(Instance, Ptr)).
