%%% @doc Functions for escaping and unescaping values for use in HTTP headers,
%%% store keys and path components. Both percent-encoding and escaping of
%%% double-quoted strings (`"' => `\"') are supported.
%%%
%%% `encode/1' writes each byte other than `a-z', `0-9', `.', `-', `_', `/', `?'
%%% and `&' as `%' and two lowercase hex digits, capitals included. The keys of
%%% a message in `structured@1.0' form are lowercase, but an ID keeps its
%%% capitals, and a file system that ignores case would give two IDs that
%%% differ only in case one file.
%%%
%%% Path components have separate escaping for literal `/` and `%` characters.
-module(hb_escape).
-export([encode/1, decode/1]).
-export([encode_path_component/1, decode_path_component/1]).
-export([encode_header/1, decode_header/1]).
-export([encode_quotes/1, decode_quotes/1]).
-export([encode_ampersand/1]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Encode a binary as a URI-encoded string.
encode(Bin) when is_binary(Bin) ->
    case uri_safe(Bin) of
        true -> Bin;
        false -> iolist_to_binary(lists:reverse(percent_escape(Bin, [])))
    end.

%% @doc Decode a URI-encoded string back to a binary.
decode(Bin) when is_binary(Bin) ->
    decode(Bin, Bin).
decode(<<>>, Original) ->
    Original;
decode(<<$%, _/binary>>, Original) ->
    iolist_to_binary(lists:reverse(percent_unescape(Original, [])));
decode(<<_C, Rest/binary>>, Original) ->
    decode(Rest, Original).

%% @doc Escape a literal key as a single path component. The keys `.' and `..'
%% are escaped in full: as path components, they name the current and parent
%% directory.
encode_path_component(<<".">>) -> <<"%2e">>;
encode_path_component(<<"..">>) -> <<"%2e%2e">>;
encode_path_component(Key) ->
    binary:replace(
        binary:replace(Key, <<"%">>, <<"%25">>, [global]),
        <<"/">>, <<"%2f">>, [global]
    ).

%% @doc Restore a key, decoding exactly one layer of path-component escaping.
decode_path_component(<<"%2e">>) -> <<".">>;
decode_path_component(<<"%2e%2e">>) -> <<"..">>;
decode_path_component(Key) ->
    binary:replace(
        binary:replace(Key, <<"%2f">>, <<"/">>, [global]),
        <<"%25">>, <<"%">>, [global]
    ).

%% @doc Escape backslashes before adding escapes for carriage returns and
%% newlines. Each pass copies the binary at most once.
encode_header(Bin) ->
    binary:replace(
        binary:replace(
            binary:replace(Bin, <<"\\">>, <<"\\\\">>, [global]),
            <<"\r">>, <<"\\r">>, [global]
        ),
        <<"\n">>, <<"\\n">>, [global]
    ).

%% @doc Unescape the backslashes, carriage returns and newlines that
%% `encode_header/1' escapes, in one pass. Any other byte is kept as is.
decode_header(Bin) -> decode_header(Bin, <<>>).
decode_header(<<>>, Acc) -> Acc;
decode_header(<<$\\, $\\, Rest/binary>>, Acc) ->
    decode_header(Rest, <<Acc/binary, $\\>>);
decode_header(<<$\\, $r, Rest/binary>>, Acc) ->
    decode_header(Rest, <<Acc/binary, $\r>>);
decode_header(<<$\\, $n, Rest/binary>>, Acc) ->
    decode_header(Rest, <<Acc/binary, $\n>>);
decode_header(<<C, Rest/binary>>, Acc) ->
    decode_header(Rest, <<Acc/binary, C>>).

%% @doc Encode a string with escaped quotes.
encode_quotes(String) when is_binary(String) ->
    list_to_binary(encode_quotes(binary_to_list(String)));
encode_quotes([]) -> [];
encode_quotes([$\" | Rest]) -> [$\\, $\" | encode_quotes(Rest)];
encode_quotes([C | Rest]) -> [C | encode_quotes(Rest)].

%% @doc Decode a string with escaped quotes.
decode_quotes(String) when is_binary(String) ->
    list_to_binary(decode_quotes(binary_to_list(String)));
decode_quotes([]) -> [];
decode_quotes([$\\, $\" | Rest]) -> [$\" | decode_quotes(Rest)];
decode_quotes([$\" | Rest]) -> decode_quotes(Rest);
decode_quotes([C | Rest]) -> [C | decode_quotes(Rest)].

%% @doc Encode ampersands as &amp; for XML output.
encode_ampersand(String) when is_binary(String) ->
    list_to_binary(encode_ampersand(binary_to_list(String)));
encode_ampersand([]) -> [];
encode_ampersand([$& | Rest]) -> [$&, $a, $m, $p, $; | encode_ampersand(Rest)];
encode_ampersand([C | Rest]) -> [C | encode_ampersand(Rest)].

%% @doc Escape a binary as a URI-encoded string.
uri_safe(<<>>) -> true;
uri_safe(<<C, Rest/binary>>) when C >= $a, C =< $z ->
    uri_safe(Rest);
uri_safe(<<C, Rest/binary>>) when C >= $0, C =< $9 ->
    uri_safe(Rest);
uri_safe(<<C, Rest/binary>>) when
        C == $.; C == $-; C == $_; C == $/;
        C == $?; C == $& ->
    uri_safe(Rest);
uri_safe(_) ->
    false.

percent_escape(<<>>, Acc) -> Acc;
percent_escape(<<C, Rest/binary>>, Acc) when C >= $a, C =< $z ->
    percent_escape(Rest, [C | Acc]);
percent_escape(<<C, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    percent_escape(Rest, [C | Acc]);
percent_escape(<<C, Rest/binary>>, Acc) when
        C == $.; C == $-; C == $_; C == $/;
        C == $?; C == $& ->
    percent_escape(Rest, [C | Acc]);
percent_escape(<<C, Rest/binary>>, Acc) ->
    percent_escape(Rest, [escape_byte(C) | Acc]).

%% @doc Escape a single byte as a URI-encoded string.
escape_byte(C) when C >= 0, C =< 255 ->
    [$%, hex_digit(C bsr 4), hex_digit(C band 15)].

hex_digit(N) when N >= 0, N =< 9 ->
    N + $0;
hex_digit(N) when N > 9, N =< 15 ->
    N + $a - 10.

%% @doc Unescape a URI-encoded string.
percent_unescape(<<$%, H1, H2, Rest/binary>>, Acc) ->
    Byte = (hex_value(H1) bsl 4) + hex_value(H2),
    percent_unescape(Rest, [Byte | Acc]);
percent_unescape(<<C, Rest/binary>>, Acc) ->
    percent_unescape(Rest, [C | Acc]);
percent_unescape(<<>>, Acc) ->
    Acc.

hex_value(C) when C >= $0, C =< $9 ->
    C - $0;
hex_value(C) when C >= $a, C =< $f ->
    C - $a + 10;
hex_value(C) when C >= $A, C =< $F ->
    C - $A + 10.

%%% Tests

%% @doc Header escaping preserves every byte and distinguishes literal escapes.
header_escape_test() ->
    ?assertEqual(<<"\\\\r\\r\\\\n\\n">>, encode_header(<<"\\r\r\\n\n">>)),
    Bytes = list_to_binary(lists:seq(0, 255)),
    lists:foreach(
        fun(Bin) -> ?assertEqual(Bin, decode_header(encode_header(Bin))) end,
        % A 1 MiB value decodes within the test timeout.
        [<<>>, Bytes, <<Bytes/binary, Bytes/binary>>, binary:copy(Bytes, 4096)]
    ).

escape_unescape_identity_test() ->
    % Test that unescape(escape(X)) == X for various inputs
    TestCases = [
        <<"hello">>,
        <<"hello, world!">>,
        <<"hello+list">>,
        <<"special@chars#here">>,
        <<"UPPERCASE">>,
        <<"MixedCASEstring">>,
        <<"12345">>,
        <<>> % Empty string
    ],
    ?event(parsing,
        {escape_unescape_identity_test,
            {test_cases,
                [
                        {Case, {explicit, encode(Case)}}
                    ||
                        Case <- TestCases
                ]
            }
        }
    ),
    lists:foreach(fun(TestCase) ->
        ?assertEqual(TestCase, decode(encode(TestCase)))
    end, TestCases).

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
    SpecialChars = [
        $@, $#, $", $$, $%, $&, $', $(, $), $*, $+, $,, $/, $:, $;, 
        $<, $=, $>, $?, $[, $\\, $], $^, $`, ${, $|, $}, $~, $\s
    ],
    TestString = list_to_binary(SpecialChars),
    ?assertEqual(TestString, decode(encode(TestString))).
