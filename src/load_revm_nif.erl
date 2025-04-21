-module(load_revm_nif).
-export([hello/0, load_evm/0]).

-on_load(init/0).
-define(NOT_LOADED, not_loaded(?LINE)).

hello() ->
    ?NOT_LOADED.

load_evm() ->
    ?NOT_LOADED.

init() ->
    NifPath = filename:join(filename:dirname(code:which(?MODULE)), "../priv/crates/load_revm_nif/load_revm_nif"),
    erlang:load_nif(NifPath, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
