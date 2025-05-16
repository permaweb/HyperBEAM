-module(kem_nif).
-export([hello/0, execute_kernel/3, adapter_info/0]).

-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

-spec hello() -> binary().
hello() ->
    ?NOT_LOADED.

-spec execute_kernel(binary(), binary(), pos_integer()) -> binary().
execute_kernel(KernelId, InputData, OutputSizeHint) 
    when is_binary(KernelId),
         is_binary(InputData),
         is_integer(OutputSizeHint),
         OutputSizeHint > 0 ->
    ?NOT_LOADED.

-spec adapter_info() -> binary().
adapter_info() ->
    ?NOT_LOADED.


init() ->
    % Get the current directory for debugging
    {ok, Cwd} = file:get_cwd(),
    io:format("Current directory: ~p~n", [Cwd]),
        
    PrivDir = case code:priv_dir(hb) of
        {error, _} -> "priv";
        Dir -> Dir
    end,
    
    % Build the NIF path using the priv directory
    NifPath = filename:join([PrivDir, "crates", "kernel_em_nif", "kernel_em_nif"]),
    io:format("NIF path: ~p~n", [NifPath]),
    
    % Check if the NIF file exists
    NifSoPath = NifPath ++ ".so",
    io:format("NIF .so exists: ~p~n", [filelib:is_file(NifSoPath)]),
    
    % Try to load the NIF
    Result = erlang:load_nif(NifPath, 0),
    io:format("Load result: ~p~n", [Result]),
    
    Result.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
