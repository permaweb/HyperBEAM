%%% @doc NIF-backed ASCII byte transforms for `hb_util' string operations.
%%% Each maps input bytes in a single pass. `lowercase/1' and `key_chars/1'
%%% fold ASCII only and return the atom `non_ascii' the moment they see a byte
%%% >= 0x80, so the Erlang caller can delegate to `string:lowercase' (which does
%%% full Unicode folding and throws on invalid UTF-8). `dash_chars/1' only swaps
%%% the ASCII byte `_'<->`-' and never folds, so it is exact for all input.
-module(hb_util_string).
-export([lowercase/1, key_chars/1, dash_chars/1]).
-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).

init() ->
    SoName = filename:join([code:priv_dir(hb), "hb_util_string"]),
    erlang:load_nif(SoName, 0).

%% @doc ASCII-lowercase a binary, or return `non_ascii' if any byte is >= 0x80.
lowercase(_Bin) ->
    erlang:nif_error(not_loaded).

%% @doc ASCII-lowercase and map `-' to `_' (the `key_to_atom' transform), or
%% return `non_ascii' if any byte is >= 0x80.
key_chars(_Bin) ->
    erlang:nif_error(not_loaded).

%% @doc Map `_' to `-' — the `atom_to_dashed_binary' transform. Exact for all
%% bytes (non-ASCII passes through), so it never returns `non_ascii'.
dash_chars(_Bin) ->
    erlang:nif_error(not_loaded).

%% Tests

lowercase_test() ->
    ?assertEqual(<<"content-type">>, lowercase(<<"Content-Type">>)),
    ?assertEqual(<<>>, lowercase(<<>>)),
    %% any byte >= 0x80 -> bail to `non_ascii' (caller delegates to string:lc)
    ?assertEqual(non_ascii, lowercase(<<"AB", 16#C5>>)),
    ?assertEqual(non_ascii, lowercase(<<16#FF>>)).

key_chars_test() ->
    ?assertEqual(<<"content_type">>, key_chars(<<"Content-Type">>)),
    ?assertEqual(<<"a_b_c">>, key_chars(<<"A-B-C">>)),
    ?assertEqual(<<>>, key_chars(<<>>)),
    ?assertEqual(non_ascii, key_chars(<<"A-B", 16#C5>>)).

dash_chars_test() ->
    ?assertEqual(<<"atom-1">>, dash_chars(<<"atom_1">>)),
    ?assertEqual(<<"a-b-c">>, dash_chars(<<"a_b_c">>)),
    ?assertEqual(<<>>, dash_chars(<<>>)),
    %% non-ASCII passes through (no fold, no bail)
    ?assertEqual(<<"k", 16#FF>>, dash_chars(<<"k", 16#FF>>)).

%% The NIF transforms must equal the Erlang expressions they replace, for all
%% ASCII inputs (the domain of HB keys / atom names).
equivalence_test() ->
    OldLower = fun(K) -> string:lowercase(K) end,
    OldKey = fun(K) ->
        string:lowercase(binary:replace(K, <<"-">>, <<"_">>, [global]))
    end,
    OldDash = fun(B) ->
        re:replace(B, <<"_">>, <<"-">>, [global, {return, binary}])
    end,
    Inputs = [<<"Content-Type">>, <<"X-AO-Data">>, <<"slot">>, <<"a-b_c-D">>,
              <<"ALLCAPS">>, <<"123-456_789">>, <<>>],
    [ ?assertEqual(iolist_to_binary(OldLower(I)), lowercase(I)) || I <- Inputs ],
    [ ?assertEqual(iolist_to_binary(OldKey(I)), key_chars(I)) || I <- Inputs ],
    [ ?assertEqual(iolist_to_binary(OldDash(I)), dash_chars(I)) || I <- Inputs ].
