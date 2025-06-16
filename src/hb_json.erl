%%% @doc Wrapper for encoding and decoding JSON. Supports maps and Jiffy's old
%%% `ejson' format. This module abstracts the underlying JSON library, allowing
%%% us to switch between libraries as needed in the future.
-module(hb_json).
-export([encode/1, decode/1, decode/2]).


%% @doc Takes a term in Erlang's native form and encodes it as a JSON string.
encode(Term) ->
    iolist_to_binary(json:encode(Term)).


%% @doc Takes a JSON string and decodes it into an Erlang term.
decode(Bin) -> json:decode(Bin).


decode(Bin, _Opts) -> decode(Bin).
