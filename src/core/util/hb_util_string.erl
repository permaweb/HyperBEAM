%%% @doc NIF-backed ASCII byte transforms for `hb_util' string operations.
%%% Each maps input bytes in a single pass. `lowercase/1' and `key_chars/1'
%%% fold ASCII only and return the atom `non_ascii' the moment they see a byte
%%% >= 0x80, so the Erlang caller can delegate to `string:lowercase' (which does
%%% full Unicode folding and throws on invalid UTF-8). `dash_chars/1' only swaps
%%% the ASCII byte `_'<->`-' and never folds, so it is exact for all input.
-module(hb_util_string).
-export([lowercase/1, key_chars/1, canon_chars/1, dash_chars/1]).
-export([normalize_path/1]).
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

%% @doc ASCII-lowercase and map `_' to `-' (the `hb_opts' `canonical_key'
%% transform), or return `non_ascii' if any byte is >= 0x80.
canon_chars(_Bin) ->
    erlang:nif_error(not_loaded).

%% @doc Map `_' to `-' — the `atom_to_dashed_binary' transform. Exact for all
%% bytes (non-ASCII passes through), so it never returns `non_ascii'.
dash_chars(_Bin) ->
    erlang:nif_error(not_loaded).

%% @doc Collapse runs of `/' to a single `/' and strip leading/trailing `/' in
%% one pass — the `hb_path:to_binary' normalization. Equivalent to
%% `iolist_to_binary(lists:join(<<"/">>,
%%      binary:split(Bin, <<"/">>, [global, trim_all])))'. Exact for all bytes.
normalize_path(_Bin) ->
    erlang:nif_error(not_loaded).

%% Tests

lowercase_test() ->
    ?assertEqual(<<"content-type">>, lowercase(<<"Content-Type">>)),
    ?assertEqual(<<>>, lowercase(<<>>)),
    % any byte >= 0x80 -> bail to `non_ascii' (caller delegates to string:lc)
    ?assertEqual(non_ascii, lowercase(<<"AB", 16#C5>>)),
    ?assertEqual(non_ascii, lowercase(<<16#FF>>)).

key_chars_test() ->
    ?assertEqual(<<"content_type">>, key_chars(<<"Content-Type">>)),
    ?assertEqual(<<"a_b_c">>, key_chars(<<"A-B-C">>)),
    ?assertEqual(<<>>, key_chars(<<>>)),
    ?assertEqual(non_ascii, key_chars(<<"A-B", 16#C5>>)).

canon_chars_test() ->
    ?assertEqual(<<"content-type">>, canon_chars(<<"Content_Type">>)),
    ?assertEqual(<<"a-b-c">>, canon_chars(<<"A_B_C">>)),
    ?assertEqual(<<>>, canon_chars(<<>>)),
    ?assertEqual(non_ascii, canon_chars(<<"A_B", 16#C5>>)).

dash_chars_test() ->
    ?assertEqual(<<"atom-1">>, dash_chars(<<"atom_1">>)),
    ?assertEqual(<<"a-b-c">>, dash_chars(<<"a_b_c">>)),
    ?assertEqual(<<>>, dash_chars(<<>>)),
    % non-ASCII passes through (no fold, no bail)
    ?assertEqual(<<"k", 16#FF>>, dash_chars(<<"k", 16#FF>>)).

normalize_path_test() ->
    ?assertEqual(<<"cache/abc">>, normalize_path(<<"cache/abc">>)),
    ?assertEqual(<<"cache/abc">>, normalize_path(<<"/cache/abc">>)),
    ?assertEqual(<<"cache/abc">>, normalize_path(<<"cache/abc/">>)),
    ?assertEqual(<<"cache/abc">>, normalize_path(<<"//cache//abc//">>)),
    ?assertEqual(<<"a/b/c">>, normalize_path(<<"a/b/c">>)),
    ?assertEqual(<<>>, normalize_path(<<>>)),
    ?assertEqual(<<>>, normalize_path(<<"/">>)),
    ?assertEqual(<<>>, normalize_path(<<"///">>)),
    ?assertEqual(<<"x">>, normalize_path(<<"x">>)),
    % non-ASCII passes through untouched (only `/' positions matter)
    ?assertEqual(<<"k", 16#FF, "/v">>, normalize_path(<<"/k", 16#FF, "//v/">>)).

%% The NIF must equal the Erlang split/join expression it replaces, for both
%% ASCII and arbitrary-byte inputs.
normalize_path_equivalence_test() ->
    Old = fun(B) ->
        iolist_to_binary(
            lists:join(<<"/">>, binary:split(B, <<"/">>, [global, trim_all]))
        )
    end,
    Inputs = [<<"cache/abc">>, <<"/cache/abc">>, <<"cache/abc/">>,
              <<"//cache//abc//">>, <<"a/b/c/d/e">>, <<>>, <<"/">>, <<"///">>,
              <<"no-slashes">>, <<"trailing/">>, <<"/leading">>,
              <<"k", 16#FF, "//v">>],
    [ ?assertEqual(Old(I), normalize_path(I)) || I <- Inputs ].

%% The NIF transforms must equal the Erlang expressions they replace, for all
%% ASCII inputs (the domain of HB keys / atom names).
equivalence_test() ->
    OldLower = fun(K) -> string:lowercase(K) end,
    OldKey = fun(K) ->
        string:lowercase(binary:replace(K, <<"-">>, <<"_">>, [global]))
    end,
    OldCanon = fun(K) ->
        string:lowercase(binary:replace(K, <<"_">>, <<"-">>, [global]))
    end,
    OldDash = fun(B) ->
        re:replace(B, <<"_">>, <<"-">>, [global, {return, binary}])
    end,
    Inputs = [<<"Content-Type">>, <<"X-AO-Data">>, <<"slot">>, <<"a-b_c-D">>,
              <<"ALLCAPS">>, <<"123-456_789">>, <<>>],
    [ ?assertEqual(iolist_to_binary(OldLower(I)), lowercase(I)) || I <- Inputs ],
    [ ?assertEqual(iolist_to_binary(OldKey(I)), key_chars(I)) || I <- Inputs ],
    [ ?assertEqual(iolist_to_binary(OldCanon(I)), canon_chars(I)) || I <- Inputs ],
    [ ?assertEqual(iolist_to_binary(OldDash(I)), dash_chars(I)) || I <- Inputs ].
