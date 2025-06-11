%%% @doc Escape and unescape mixed case values for use in HTTP headers.
%%% This is necessary for encodings of AO-Core messages for transmission in
%%% HTTP/2 and HTTP/3, because uppercase header keys are explicitly disallowed.
%%% While most map keys in HyperBEAM are normalized to lowercase, IDs are not.
%%% Subsequently, we encode all header keys to lowercase %-encoded URI-style
%%% strings because transmission.
-module(hb_escape).
-export([encode/1, decode/1, encode_keys/2, decode_keys/2]).
-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").


%% @doc Encode a binary as a URI-encoded string.
encode(Bin) when is_binary(Bin) ->
    list_to_binary(percent_escape(binary_to_list(Bin))).


%% @doc Decode a URI-encoded string back to a binary.
decode(Bin) when is_binary(Bin) ->
    list_to_binary(percent_unescape(binary_to_list(Bin))).


%% @doc Return a message with all of its keys decoded.
decode_keys(Msg, Opts) when is_map(Msg) ->
    hb_maps:from_list(
      lists:map(
        fun({Key, Value}) -> {decode(Key), Value} end,
        hb_maps:to_list(Msg, Opts)));
decode_keys(Other, _Opts) -> Other.


%% @doc URI encode keys in the base layer of a message. Does not recurse.
encode_keys(Msg, Opts) when is_map(Msg) ->
    hb_maps:from_list(
      lists:map(
        fun({Key, Value}) -> {encode(Key), Value} end,
        hb_maps:to_list(Msg, Opts)));
encode_keys(Other, _Opts) -> Other.


%% @doc Escape a list of characters as a URI-encoded string.
percent_escape([]) -> [];
percent_escape([C | Cs]) when C >= $a, C =< $z -> [C | percent_escape(Cs)];
percent_escape([C | Cs]) when C >= $0, C =< $9 -> [C | percent_escape(Cs)];
percent_escape([C | Cs])
  when C == $.;
       C == $-;
       C == $_;
       C == $/;
       C == $?;
       C == $& ->
    [C | percent_escape(Cs)];
percent_escape([C | Cs]) -> [escape_byte(C) | percent_escape(Cs)].


%% @doc Escape a single byte as a URI-encoded string.
escape_byte(C) when C >= 0, C =< 255 ->
    [$%, hex_digit(C bsr 4), hex_digit(C band 15)].


hex_digit(N) when N >= 0, N =< 9 ->
    N + $0;
hex_digit(N) when N > 9, N =< 15 ->
    N + $a - 10.


%% @doc Unescape a URI-encoded string.
percent_unescape([$%, H1, H2 | Cs]) ->
    Byte = (hex_value(H1) bsl 4) + hex_value(H2),
    [Byte | percent_unescape(Cs)];
percent_unescape([C | Cs]) ->
    [C | percent_unescape(Cs)];
percent_unescape([]) ->
    [].


hex_value(C) when C >= $0, C =< $9 ->
    C - $0;
hex_value(C) when C >= $a, C =< $f ->
    C - $a + 10;
hex_value(C) when C >= $A, C =< $F ->
    C - $A + 10.


%%% Tests


escape_unescape_identity_test() ->
    % Test that unescape(escape(X)) == X for various inputs
    TestCases = [<<"hello">>,
                 <<"hello, world!">>,
                 <<"hello+list">>,
                 <<"special@chars#here">>,
                 <<"UPPERCASE">>,
                 <<"MixedCASEstring">>,
                 <<"12345">>,
                 <<>>  % Empty string
    ],
    ?event(parsing,
           {escape_unescape_identity_test,
            {test_cases,
             [ {Case, {explicit, encode(Case)}}
               || Case <- TestCases ]}}),
    lists:foreach(fun(TestCase) ->
                          ?assertEqual(TestCase, decode(encode(TestCase)))
                  end,
                  TestCases).


unescape_specific_test() ->
    % Test specific unescape cases
    ?assertEqual(<<"a">>, decode(<<"%61">>)),
    ?assertEqual(<<"A">>, decode(<<"%41">>)),
    ?assertEqual(<<"!">>, decode(<<"%21">>)),
    ?assertEqual(<<"hello, World!">>, decode(<<"hello%2c%20%57orld%21">>)),
    ?assertEqual(<<"/">>, decode(<<"%2f">>)),
    ?assertEqual(<<"?">>, decode(<<"%3f">>)).


uppercase_test() ->
    % Test uppercase characters are properly escaped
    ?assertEqual(<<"%41">>, encode(<<"A">>)),
    ?assertEqual(<<"%42">>, encode(<<"B">>)),
    ?assertEqual(<<"%5a">>, encode(<<"Z">>)),
    ?assertEqual(<<"hello%20%57orld">>, encode(<<"hello World">>)),
    ?assertEqual(<<"test%41%42%43">>, encode(<<"testABC">>)).


escape_unescape_special_chars_test() ->
    % Test characters that should be escaped
    SpecialChars = [$@, $#, $", $$, $%, $&, $', $(, $), $*, $+, $,, $/, $:, $;,
                    $<, $=, $>, $?, $[, $\\, $], $^, $`, ${, $|, $}, $~, $\s],
    TestString = list_to_binary(SpecialChars),
    ?assertEqual(TestString, decode(encode(TestString))).
