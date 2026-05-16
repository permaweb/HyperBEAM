%%% @doc WASM image fixtures shared by device tests.
-module(hb_wasm_test_utils).
-export([cache_image/1, cache_image/2]).

%% @doc Cache a WASM image file and return a process-compatible message.
cache_image(Image) ->
    cache_image(Image, #{}).
cache_image(Image, Opts) ->
    {ok, Bin} = file:read_file(Image),
    Msg = #{ <<"body">> => Bin },
    {ok, ID} = hb_cache:write(Msg, Opts),
    #{
        <<"device">> => <<"wasm-64@1.0">>,
        <<"image">> => ID
    }.
