-module(load_revm_nif).
-export([hello/0, eval_bytecode/3, get_appchain_state/1]).

-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

-spec hello() -> binary().
hello() ->
    ?NOT_LOADED.

-spec eval_bytecode(binary(), binary(), binary()) -> binary().
eval_bytecode(SignedRawTx, State, CoutState) when is_binary(SignedRawTx), is_binary(State), is_binary(CoutState) ->
    ?NOT_LOADED.

-spec get_appchain_state(binary()) -> binary().
get_appchain_state(ChainId) when is_binary(ChainId) ->
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
    NifPath = filename:join([PrivDir, "crates", "load_revm_nif", "load_revm_nif"]),
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
