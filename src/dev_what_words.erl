%%% @doc Resolve `ar-X[-Y[-Z...]]` subdomain names to Arweave items and
%%% generate human-readable names for Arweave offsets using a hostname-safe
%%% word list.
-module(dev_what_words).
-export([info/1, name/3, offset_to_name/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE_NAME, <<"what-words@1.0">>).
-define(CACHE_KEY, {?MODULE, words}).
-define(WORDS_FILE, "arweave-offset-words.txt.gz").

%% @doc Configure the device to resolve unknown keys through the variable-length
%% offset vocabulary.
info(_Opts) ->
    #{
        default => fun get/4,
        excludes => [<<"keys">>, <<"set">>]
    }.

%% @doc Resolve a human-readable offset name by delegating to the Arweave
%% device with the corresponding byte offset.
get(Key, _, _Req, Opts) ->
    case name_to_offset(Key) of
        {ok, Offset} ->
            hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                hb_util:bin(Offset),
                Opts
            );
        _ ->
            {error, not_found}
    end.

%% @doc Convert a given offset argument into its shortest `ar-X[-Y[-Z...]]`
%% byte-offset name.
name(_Base, Req, Opts) ->
    case normalize_offset(hb_maps:get(<<"offset">>, Req, not_found, Opts)) of
        {ok, Offset} ->
            offset_to_name(Offset);
        _ ->
            {error, not_found}
    end.

%% @doc Convert an Arweave byte offset into its shortest permuted
%% `ar-X[-Y[-Z...]]` name.
offset_to_name(Offset) when is_integer(Offset) andalso Offset >= 0 ->
    Words = words(),
    WordCount = tuple_size(Words),
    {Length, BandStart, _} = band_info_for_index(Offset, WordCount),
    LocalIndex = Offset - BandStart,
    {ok,
        phrase_words_to_name(
            digits_to_words(
                shuffle_digits(
                    phrase_index_to_digits(LocalIndex, Length, WordCount),
                    WordCount
                ),
                Words
            )
        )};
offset_to_name(_) ->
    {error, not_found}.

%% @doc Normalize an offset argument from the request into a non-negative
%% integer.
normalize_offset(Offset) when is_integer(Offset) andalso Offset >= 0 ->
    {ok, Offset};
normalize_offset(Offset) when is_binary(Offset); is_list(Offset) ->
    hb_util:safe_int(Offset);
normalize_offset(_) ->
    error.

%% @doc Convert an `ar-X[-Y[-Z...]]` name into the corresponding permuted
%% byte offset.
name_to_offset(Name) when is_binary(Name) ->
    case binary:split(hb_util:to_lower(Name), <<"-">>, [global]) of
        [<<"ar">> | PhraseWords] when PhraseWords =/= [] ->
            phrase_to_offset(PhraseWords);
        _ ->
            {error, not_found}
    end;
name_to_offset(_) ->
    {error, not_found}.

%% @doc Return the size of the active hostname-safe vocabulary.
count() ->
    tuple_size(words()).

%% @doc Convert a parsed phrase into a permuted byte offset.
phrase_to_offset(PhraseWords) ->
    WordCount = count(),
    Length = erlang:length(PhraseWords),
    {Length, BandStart, _} = band_info_for_length(Length, WordCount),
    case phrase_words_to_index(PhraseWords, WordCount) of
        {ok, PhraseIndex} ->
            {ok,
                BandStart +
                    digits_to_phrase_index(
                        unshuffle_digits(
                            phrase_index_to_digits(
                                PhraseIndex,
                                Length,
                                WordCount
                            ),
                            WordCount
                        ),
                        WordCount
                    )
            };
        _ ->
            {error, not_found}
    end.

%% @doc Return the smallest phrase band that contains an offset index.
band_info_for_index(Offset, WordCount) ->
    band_info_for_index(Offset, WordCount, 1, 0, WordCount).

band_info_for_index(Offset, _WordCount, Length, BandStart, BandSize)
        when Offset < BandStart + BandSize ->
    {Length, BandStart, BandSize};
band_info_for_index(Offset, WordCount, Length, BandStart, BandSize) ->
    band_info_for_index(
        Offset,
        WordCount,
        Length + 1,
        BandStart + BandSize,
        BandSize * WordCount
    ).

%% @doc Return the phrase band metadata for a given phrase length.
band_info_for_length(Length, WordCount) when Length > 0 ->
    band_info_for_length(Length, WordCount, 1, 0, WordCount).

band_info_for_length(Length, _WordCount, Length, BandStart, BandSize) ->
    {Length, BandStart, BandSize};
band_info_for_length(TargetLength, WordCount, Length, BandStart, BandSize) ->
    band_info_for_length(
        TargetLength,
        WordCount,
        Length + 1,
        BandStart + BandSize,
        BandSize * WordCount
    ).

%% @doc Convert a phrase-space index into a fixed-length list of word indices.
phrase_index_to_digits(PhraseIndex, Length, WordCount) ->
    phrase_index_to_digits(PhraseIndex, Length, WordCount, []).

phrase_index_to_digits(_PhraseIndex, 0, _WordCount, Acc) ->
    Acc;
phrase_index_to_digits(PhraseIndex, Length, WordCount, Acc) ->
    phrase_index_to_digits(
        PhraseIndex div WordCount,
        Length - 1,
        WordCount,
        [(PhraseIndex rem WordCount) | Acc]
    ).

%% @doc Convert a list of word indices into the corresponding words.
digits_to_words(Digits, Words) ->
    lists:map(
        fun(Digit) ->
            element(Digit + 1, Words)
        end,
        Digits
    ).

%% @doc Convert a fixed-length phrase back into its band-local index.
phrase_words_to_index(PhraseWords, WordCount) ->
    phrase_words_to_index(PhraseWords, WordCount, 0).

phrase_words_to_index([], _WordCount, Acc) ->
    {ok, Acc};
phrase_words_to_index([Word | Rest], WordCount, Acc) ->
    case word_index(Word) of
        {ok, WordIndex} ->
            phrase_words_to_index(Rest, WordCount, (Acc * WordCount) + WordIndex);
        Error ->
            Error
    end.

%% @doc Convert a fixed-length list of digits back into its band-local index.
digits_to_phrase_index(Digits, WordCount) ->
    lists:foldl(
        fun(Digit, Acc) ->
            (Acc * WordCount) + Digit
        end,
        0,
        Digits
    ).

%% @doc Shuffle the base-WordCount digits with cumulative suffix sums so the
%% least-significant digit affects every output word while remaining invertible.
shuffle_digits(Digits, WordCount) ->
    {MixedDigits, _} =
        lists:foldl(
            fun(Digit, {Acc, Running}) ->
                Mixed = positive_mod(Digit + Running, WordCount),
                {[Mixed | Acc], Mixed}
            end,
            {[], 0},
            lists:reverse(Digits)
        ),
    MixedDigits.

%% @doc Invert the cumulative-sum digit shuffle back to the original digits.
unshuffle_digits([Digit], _WordCount) ->
    [Digit];
unshuffle_digits([Digit, Next | Rest], WordCount) ->
    [positive_mod(Digit - Next, WordCount) |
        unshuffle_digits([Next | Rest], WordCount)];
unshuffle_digits([], _WordCount) ->
    [].

%% @doc Compute a positive modulo result.
positive_mod(Value, Modulus) ->
    ((Value rem Modulus) + Modulus) rem Modulus.

%% @doc Join a list of words into an `ar-...` hostname label.
phrase_words_to_name(PhraseWords) ->
    <<"ar-", (iolist_to_binary(lists:join(<<"-">>, PhraseWords)))/binary>>.

%% @doc Find the zero-based index of a word in the configured lexicon.
word_index(Word) when is_binary(Word) ->
    find_word(hb_util:to_lower(Word), words(), 1, count()).

%% @doc Load the lexicon from the generated word list and cache it for reuse.
words() ->
    case persistent_term:get(?CACHE_KEY, not_found) of
        not_found ->
            Words = load_words(),
            persistent_term:put(?CACHE_KEY, Words),
            Words;
        Words ->
            Words
    end.

%% @doc Read and parse the configured word list asset.
load_words() ->
    PrivDir = code:priv_dir(hb),
    WordListPath = filename:join(PrivDir, ?WORDS_FILE),
    case file:read_file(WordListPath) of
        {ok, EncodedWords} ->
            list_to_tuple(
                binary:split(
                    zlib:gunzip(EncodedWords),
                    <<"\n">>,
                    [global, trim_all]
                )
            );
        {error, Reason} ->
            erlang:error({cannot_load_arweave_words, WordListPath, Reason})
    end.

%% @doc Binary-search for a word inside the sorted tuple of words.
find_word(_Word, _Words, Low, High) when Low > High ->
    {error, not_found};
find_word(Word, Words, Low, High) ->
    Mid = (Low + High) div 2,
    MidWord = element(Mid, Words),
    case Word of
        MidWord ->
            {ok, Mid - 1};
        _ when Word < MidWord ->
            find_word(Word, Words, Low, Mid - 1);
        _ ->
            find_word(Word, Words, Mid + 1, High)
    end.

%% @doc Build isolated opts for testing the name device against a local Arweave
%% index.
test_opts() ->
    TestStore = hb_test_utils:test_store(hb_store_volatile, <<"what-words">>),
    IndexStore =
        #{
            <<"module">> => hb_store_arweave,
            <<"index-store">> => [TestStore]
        },
    #{
        store => [TestStore],
        arweave_index_ids => true,
        arweave_index_store => IndexStore,
        name_resolvers => [#{ <<"device">> => ?DEVICE_NAME }],
        on =>
            #{
                <<"request">> => [#{ <<"device">> => <<"name@1.0">> }]
            }
    }.

%% @doc Import a single known-good block into the local Arweave index.
load_test_block(Opts) ->
    BlockBin = hb_util:bin(1_827_942),
    {ok, _} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&from=",
                BlockBin/binary,
                "&to=",
                BlockBin/binary
            >>,
            Opts
        ),
    ok.

%%% Tests

%% @doc Return the number of words present in a generated hostname label.
phrase_length(Name) ->
    erlang:length(binary:split(Name, <<"-">>, [global])) - 1.

count_test() ->
    ?assert(count() > 150000).

name_to_offset_test() ->
    {ok, OneWordName} = offset_to_name(0),
    ?assertEqual({ok, 0}, name_to_offset(OneWordName)),
    ?assertEqual(
        {error, not_found},
        name_to_offset(<<"ar-definitelynotaword">>)
    ),
    ?assertEqual(
        {error, not_found},
        name_to_offset(<<"definitely-not-a-what-words-name">>)
    ),
    ?assertEqual(
        {error, not_found},
        name_to_offset(<<"ar">>)
    ).

offset_to_name_test() ->
    WordCount = count(),
    ?assertEqual(
        1,
        phrase_length(element(2, offset_to_name(0)))
    ),
    ?assertEqual(
        1,
        phrase_length(element(2, offset_to_name(WordCount - 1)))
    ),
    ?assertEqual(
        2,
        phrase_length(element(2, offset_to_name(WordCount)))
    ),
    ?assertEqual(
        2,
        phrase_length(
            element(
                2,
                offset_to_name(WordCount + (WordCount * WordCount) - 1)
            )
        )
    ),
    ?assertEqual(
        3,
        phrase_length(
            element(
                2,
                offset_to_name(WordCount + (WordCount * WordCount))
            )
        )
    ),
    ?assertMatch(
        {ok, _},
        offset_to_name(
            WordCount +
                (WordCount * WordCount) +
                (WordCount * WordCount * WordCount)
        )
    ).

shuffle_roundtrip_test() ->
    WordCount = count(),
    lists:foreach(
        fun(Offset) ->
            {ok, Name} = offset_to_name(Offset),
            ?assertEqual({ok, Offset}, name_to_offset(Name))
        end,
        [
            0,
            1,
            WordCount - 1,
            WordCount,
            WordCount + 1,
            WordCount + (WordCount * WordCount) - 1,
            WordCount + (WordCount * WordCount),
            WordCount + (WordCount * WordCount) + 17,
            56_789
        ]
    ).

name_key_test() ->
    Offset = count() + 17,
    {ok, Name} = offset_to_name(Offset),
    ?assertEqual(
        {ok, Name},
        name(
            #{},
            #{
                <<"path">> => <<"name">>,
                <<"offset">> => Offset
            },
            #{}
        )
    ),
    ?assertEqual(
        {ok, Name},
        name(
            #{},
            #{
                <<"path">> => <<"name">>,
                <<"offset">> => hb_util:bin(Offset)
            },
            #{}
        )
    ),
    ?assertEqual(
        {error, not_found},
        name(
            #{},
            #{
                <<"path">> => <<"name">>,
                <<"offset">> => <<"not-an-integer">>
            },
            #{}
        )
    ).

resolver_lookup_test() ->
    Opts = test_opts(),
    ok = load_test_block(Opts),
    DataItemID = <<"0vy2Ey8bWkSDcRIvWQJjxDeVGYOrTSmYIIhBILJntY8">>,
    {ok, Head} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"raw">>,
                <<"raw">> => DataItemID,
                <<"method">> => <<"HEAD">>
            },
            Opts
        ),
    {ok, HumanOffset} = hb_maps:find(<<"human-offset">>, Head, Opts),
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, #{ <<"content-type">> := <<"application/json">> }},
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/">>,
                <<"host">> => <<HumanOffset/binary, ".localhost">>
            },
            Opts
        )
    ).

name_http_test() ->
    Offset = count() + 17,
    {ok, Name} = offset_to_name(Offset),
    Opts = #{ store => [hb_test_utils:test_store()] },
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, Name},
        hb_http:get(
            Node,
            #{
                <<"path">> =>
                    <<"/~", ?DEVICE_NAME/binary, "/name&offset=", (hb_util:bin(Offset))/binary>>
            },
            Opts
        )
    ).
