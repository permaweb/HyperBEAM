%%% @doc A battery of test vectors for message codecs, implementing the 
%%% `message@1.0' encoding and commitment APIs. Additionally, this module 
%%% houses tests that ensure the general functioning of the `hb_message' API.
-module(hb_message_test_vectors).

-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").

%% @doc Test invocation function, making it easier to run a specific test.
%% Disable/enable as needed.
run_test() ->
    hb:init(),
    find_multiple_commitments_test().

%% @doc Return a list of codecs to test. Disable these as necessary if you need
%% to test the functionality of a single codec, etc.
test_codecs() ->
    [
        <<"structured@1.0">>,
        <<"httpsig@1.0">>,
        <<"flat@1.0">>,
        <<"ans104@1.0">>,
        <<"json@1.0">>
    ].

%% @doc Return a set of options for testing, taking the codec name as an
%% argument. We do not presently use the codec name in the test, but we may
%% wish to do so in the future.
test_opts(_) ->
    #{
        store =>
            [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"prefix">> => <<"cache-TEST">>
                }
            ],
        priv_wallet => hb:wallet(),
        always_bundle => os:getenv("BUNDLE", false)
    }.

%% @doc Organizes a test battery for the `hb_message' module and its codecs.
suite_test_() ->
    run([
        % Basic operations
        {<<"Binary to binary">>,
            fun binary_to_binary_test/1},
        {<<"Match">>,
            fun match_test/1},
        {<<"Basic message encoding and decoding">>,
            fun basic_message_codec_test/1},
        {<<"Priv survives conversion">>,
            fun priv_survives_conversion_test/1},
        {<<"Message with body">>,
            fun set_body_codec_test/1},
        {<<"Message with large keys">>,
            fun message_with_large_keys_test/1},
        {<<"Structured field atom parsing">>,
            fun structured_field_atom_parsing_test/1},
        {<<"Structured field decimal parsing">>,
            fun structured_field_decimal_parsing_test/1},
        {<<"Unsigned id">>,
            fun unsigned_id_test/1},
        % Nested structures
        {<<"Simple nested message">>,
            fun simple_nested_message_test/1},
        {<<"Message with simple embedded list">>,
            fun message_with_simple_embedded_list_test/1},
        {<<"Nested empty map">>,
            fun nested_empty_map_test/1},
        {<<"Nested structured fields">>,
            fun nested_structured_fields_test/1},
        {<<"Single layer message to encoding">>,
            fun single_layer_message_to_encoding_test/1},
        {<<"Nested body list">>,
            fun nested_body_list_test/1},
        {<<"Empty string in nested tag">>,
            fun empty_string_in_nested_tag_test/1},
        {<<"Deep typed message ID">>,
            fun deep_typed_message_id_test/1},
        {<<"Encode small balance table">>,
            fun encode_small_balance_table_test/1},
        {<<"Encode large balance table">>,
            fun encode_large_balance_table_test/1},
        % Signed messages
        {<<"Signed message to message and back">>,
            fun signed_message_encode_decode_verify_test/1},
        {<<"Signed only committed data field">>,
            fun signed_only_committed_data_field_test/1},
        {<<"Signed nested message">>,
            fun signed_nested_message_with_child_test/1},
        {<<"Committed keys">>,
            fun committed_keys_test/1},
        {<<"Committed empty keys">>,
            fun committed_empty_keys_test/1},
        {<<"Signed list HTTP response">>,
            fun signed_list_test/1},
        {<<"Sign node message">>,
            fun sign_node_message_test/1},
        {<<"Complex signed message">>,
            fun complex_signed_message_test/1},
        {<<"Nested message with large keys">>,
            fun nested_message_with_large_keys_test/1},
        {<<"Nested signed message with typed list">>,
            fun verify_nested_complex_signed_test/1},
        % Complex structures
        {<<"Nested message with large keys and content">>,
            fun nested_message_with_large_keys_and_content_test/1},
        {<<"Nested message with large content">>,
            fun nested_message_with_large_content_test/1},
        {<<"Deeply nested message with content">>,
            fun deeply_nested_message_with_content_test/1},
        {<<"Deeply nested message with only content">>,
            fun deeply_nested_message_with_only_content/1},
        {<<"Signed deep serialize and deserialize">>,
            fun signed_deep_message_test/1},
        {<<"Signed nested data key">>,
            fun signed_nested_data_key_test/1},
        {<<"Signed message with hashpath">>,
            fun hashpath_sign_verify_test/1},
        {<<"Message with derived components">>,
            fun signed_message_with_derived_components_test/1},
        {<<"Large body committed keys">>,
            fun large_body_committed_keys_test/1},
        {<<"Signed with inner signed">>,
            fun signed_with_inner_signed_message_test/1},
        {<<"Recursive nested list">>,
            fun recursive_nested_list_test/1},
        {<<"Sign links">>,
            fun sign_links_test/1},
        {<<"ID of linked message">>,
            fun id_of_linked_message_test/1},
        {<<"Sign deep message from lazy cache read">>,
            fun sign_deep_message_from_lazy_cache_read_test/1},
        {<<"ID of deep message and link message match">>,
            fun id_of_deep_message_and_link_message_match_test/1},
        {<<"Codec round-trip conversion is idempotent">>,
            fun codec_roundtrip_conversion_is_idempotent_test/1}
    ]).

run(Suite) ->
    lists:map(
        fun(CodecName) ->
            {foreach,
                fun() ->
                    CodecOpts = test_opts(CodecName),
                    StoreOpts = maps:get(store, CodecOpts),
                    hb_store:reset(StoreOpts)
                end,
                fun(_) -> ok end,
                [
                    {
                        << CodecName/binary, ": ", Desc/binary >>,
                        fun() -> Test(CodecName) end
                    }
                ||
                    {Desc, Test} <- Suite
                ]
            }
        end,
        test_codecs()
    ).

%%% Codec-specific/misc. tests

%% @doc Tests a message transforming function to ensure that it is idempotent.
%% Runs the conversion a total of 3 times, ensuring that the result remains
%% unchanged. This function takes transformation functions that result in
%% `{ok, Res}`-form messages, as well as bare message results.
is_idempotent(Func, Msg, Opts) ->
    Run = fun(M) -> case Func(M) of {ok, Res} -> Res; Res -> Res end end,
    After1 = Run(Msg),
    After2 = Run(After1),
    After3 = Run(After2),
    hb_message:match(After1, After2, strict, Opts) andalso
        hb_message:match(After2, After3, strict, Opts).

%% @doc Ensure that converting a message to/from TABM multiple times repeatedly 
%% does not alter the message's contents.
tabm_conversion_is_idempotent_test() ->
    Opts = test_opts(<<"structured@1.0">>),
    From = fun(M) -> hb_message:convert(M, <<"structured@1.0">>, tabm, Opts) end,
    To = fun(M) -> hb_message:convert(M, tabm, <<"structured@1.0">>, Opts) end,
    SimpleMsg = #{ <<"a">> => <<"x">>, <<"b">> => <<"y">>, <<"c">> => <<"z">> },
    ComplexMsg =
        #{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                    Signed = hb_message:commit(
                        #{
                            <<"type">> => <<"Message">>,
                            <<"function">> => <<"fac">>,
                            <<"parameters">> => #{
                                <<"a">> => 1
                            },
                            <<"content-type">> => <<"application/html">>,
                            <<"body">> =>
                                <<
                                    """
                                    <html>
                                    <h1>Hello, multiline message</h1>
                                    </html>
                                    """
                                >>
                        },
                        Opts,
                        <<"structured@1.0">>
                    )
            },
    ?assert(is_idempotent(From, SimpleMsg, Opts)),
    ?assert(is_idempotent(From, Signed, Opts)),
    ?assert(is_idempotent(From, ComplexMsg, Opts)),
    ?assert(is_idempotent(To, SimpleMsg, Opts)),
    ?assert(is_idempotent(To, Signed, Opts)),
    ?assert(is_idempotent(To, ComplexMsg, Opts)).

%% @doc Ensure that converting a message to a codec, then back to TABM multiple
%% times results in the same message being returned. This test differs from its
%% TABM form, as it shuttles (`to-from-to-...`), while the TABM test repeatedly
%% encodes in a single direction (`to->to->...`).
codec_roundtrip_conversion_is_idempotent_test(Codec) ->
    Opts = test_opts(<<"structured@1.0">>),
    Roundtrip =
        fun(M) ->
            hb_message:convert(
                hb_message:convert(M, Codec, <<"structured@1.0">>, Opts),
                <<"structured@1.0">>,
                Codec,
                Opts
            )
        end,
    SimpleMsg = #{ <<"a">> => <<"x">>, <<"b">> => <<"y">>, <<"c">> => <<"z">> },
    ComplexMsg =
        #{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                    Signed = hb_message:commit(
                        #{
                            <<"type">> => <<"Message">>,
                            <<"function">> => <<"fac">>,
                            <<"parameters">> => #{
                                <<"a">> => 1
                            },
                            <<"content-type">> => <<"application/html">>,
                            <<"body">> =>
                                <<
                                    """
                                    <html>
                                    <h1>Hello, multiline message</h1>
                                    </html>
                                    """
                                >>
                        },
                        Opts,
                        Codec
                    )
            },
    ?assert(is_idempotent(Roundtrip, SimpleMsg, Opts)),
    ?assert(is_idempotent(Roundtrip, Signed, Opts)),
    ?assert(is_idempotent(Roundtrip, ComplexMsg, Opts)).

%% @doc Test that the filter_default_keys/1 function removes TX fields
%% that have the default values found in the tx record, but not those that
%% have been set by the user.
default_keys_removed_test() ->
    TX = #tx { unsigned_id = << 1:256 >>, last_tx = << 2:256 >> },
    TXMap = #{
        <<"unsigned_id">> => TX#tx.unsigned_id,
        <<"last_tx">> => TX#tx.last_tx,
        <<"owner">> => TX#tx.owner,
        <<"target">> => TX#tx.target,
        <<"data">> => TX#tx.data
    },
    FilteredMap = hb_message:filter_default_keys(TXMap),
    ?assertEqual(<< 1:256 >>, hb_maps:get(<<"unsigned_id">>, FilteredMap)),
    ?assertEqual(<< 2:256 >>, hb_maps:get(<<"last_tx">>, FilteredMap, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"owner">>, FilteredMap, not_found)),
    ?assertEqual(not_found, hb_maps:get(<<"target">>, FilteredMap, not_found)).

minimization_test() ->
    Msg = #{
        <<"unsigned_id">> => << 1:256 >>,
        <<"id">> => << 2:256 >>
    },
    MinimizedMsg = hb_message:minimize(Msg),
    ?event({minimized, MinimizedMsg}),
    ?assertEqual(1, hb_maps:size(MinimizedMsg)).

match_modes_test() ->
    Msg1 = #{ <<"a">> => 1, <<"b">> => 2 },
    Msg2 = #{ <<"a">> => 1 },
    Msg3 = #{ <<"a">> => 1, <<"b">> => 2, <<"c">> => 3 },
    ?assert(hb_message:match(Msg1, Msg2, only_present)),
    ?assert(hb_message:match(Msg2, Msg1, strict) =/= true),
    ?assert(hb_message:match(Msg1, Msg3, primary)),
    ?assert(hb_message:match(Msg3, Msg1, primary) =/= true).

basic_message_codec_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{ <<"normal_key">> => <<"NORMAL_VALUE">> },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

set_body_codec_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{ <<"body">> => <<"NORMAL_VALUE">>, <<"test-key">> => <<"Test-Value">> },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

%% @doc Test that we can convert a message into a tx record and back.
single_layer_message_to_encoding_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"last_tx">> => << 2:256 >>,
        <<"target">> => << 4:256 >>,
        <<"data">> => <<"DATA">>,
        <<"special-key">> => <<"SPECIAL_VALUE">>
    },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    MatchRes = hb_message:match(Msg, Decoded, strict, Opts),
    ?event({match_result, MatchRes}),
    ?assert(MatchRes).

signed_only_committed_data_field_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = hb_message:commit(#{ <<"data">> => <<"DATA">> }, Opts, Codec),
    ?event({signed_msg, Msg}),
    {ok, OnlyCommitted} = hb_message:with_only_committed(Msg, Opts),
    ?event({only_committed, OnlyCommitted}),
    ?assert(hb_message:verify(OnlyCommitted)).

signed_nested_data_key_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"outer-data">> => <<"outer">>,
        <<"body">> =>
            hb_message:commit(
                #{
                    <<"inner-data">> => <<"inner">>,
                    <<"data">> => <<"DATA">>
                },
                Opts,
                Codec
            )
    },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    LoadedMsg = hb_cache:ensure_all_loaded(Decoded, Opts),
    ?event({matching, {input, Msg}, {output, LoadedMsg}}),
    ?assert(hb_message:match(Msg, LoadedMsg, primary, Opts)).

% %% @doc Test that different key encodings are converted to their corresponding
% %% TX fields.
% key_encodings_to_tx_test() ->
%     Msg = #{
%         <<"last_tx">> => << 2:256 >>,
%         <<"owner">> => << 3:4096 >>,
%         <<"target">> => << 4:256 >>
%     },
%     TX = message_to_tx(Msg),
%     ?event({key_encodings_to_tx, {msg, Msg}, {tx, TX}}),
%     ?assertEqual(hb_maps:get(<<"last_tx">>, Msg), TX#tx.last_tx),
%     ?assertEqual(hb_maps:get(<<"owner">>, Msg), TX#tx.owner),
%     ?assertEqual(hb_maps:get(<<"target">>, Msg), TX#tx.target).

%% @doc Test that the message matching function works.
match_test(Codec) ->
    Msg = #{ <<"a">> => 1, <<"b">> => 2 },
    Opts = test_opts(Codec),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

binary_to_binary_test(Codec) ->
    % Serialization must be able to turn a raw binary into a TX, then turn
    % that TX back into a binary and have the result match the original.
    Bin = <<"THIS IS A BINARY, NOT A NORMAL MESSAGE">>,
    Opts = test_opts(Codec),
    Encoded = hb_message:convert(Bin, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?assertEqual(Bin, Decoded).

%% @doc Structured field parsing tests.
structured_field_atom_parsing_test(Codec) ->
    Msg = #{ highly_unusual_http_header => highly_unusual_value },
    Opts = test_opts(Codec),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

structured_field_decimal_parsing_test(Codec) ->
    Msg = #{ integer_field => 1234567890 },
    Opts = test_opts(Codec),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

%% @doc Test that the data field is correctly managed when we have multiple
%% uses for it (the 'data' key itself, as well as keys that cannot fit in
%% tags).
message_with_large_keys_test(Codec) ->
    Msg = #{
        <<"normal_key">> => <<"normal_value">>,
        <<"large_key">> => << 0:((1 + 1024) * 8) >>,
        <<"another_large_key">> => << 0:((1 + 1024) * 8) >>,
        <<"another_normal_key">> => <<"another_normal_value">>
    },
    Opts = test_opts(Codec),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

%% @doc Check that a nested signed message with an embedded typed list can 
%% be further nested and signed. We then encode and decode the message. This
%% tests a large portion of the complex type encodings that HyperBEAM uses
%% together.
verify_nested_complex_signed_test(Codec) ->
    Opts = test_opts(Codec),
    Msg =
        hb_message:commit(#{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                    Inner = hb_message:commit(
                        #{
                            <<"type">> => <<"Message">>,
                            <<"function">> => <<"fac">>,
                            <<"parameters">> => #{
                                <<"a">> => 1
                            },
                            <<"content-type">> => <<"application/html">>,
                            <<"body">> =>
                                <<
                                    """
                                    <html>
                                    <h1>Hello, multiline message</h1>
                                    </html>
                                    """
                                >>
                        },
                        Opts,
                        Codec
                    )
            },
            Opts,
            Codec
        ),
    % Ensure that the messages verify prior to conversion.
    LoadedInitialInner = hb_cache:ensure_all_loaded(Inner, Opts),
    ?assert(hb_message:verify(Inner, all, Opts)),
    ?assert(hb_message:verify(LoadedInitialInner, all, Opts)),
    % % Test encoding and decoding.
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    % % Ensure that the decoded message matches.
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)),
    ?assert(hb_message:verify(Decoded, all, Opts)),
    % % Ensure that both of the messages can be verified (and retreived).
    FoundInner = hb_maps:get(<<"body">>, Msg, not_found, Opts),
    LoadedFoundInner = hb_cache:ensure_all_loaded(FoundInner, Opts),
    % Verify that the fully loaded version of the inner message, and the one
    % gained by applying `hb_maps:get` match and verify.
    ?assert(hb_message:match(Inner, FoundInner, primary, Opts)),
    ?assert(hb_message:match(FoundInner, LoadedFoundInner, primary, Opts)),
    ?assert(hb_message:verify(Inner, all, Opts)),
    ?assert(hb_message:verify(LoadedFoundInner, all, Opts)),
    ?assert(hb_message:verify(FoundInner, all, Opts)).

%% @doc Check that large keys and data fields are correctly handled together.
nested_message_with_large_keys_and_content_test(Codec) ->
    MainBodyKey =
        case Codec of
            <<"ans104@1.0">> -> <<"data">>;
            _ -> <<"body">>
        end,
    Msg = #{
        <<"normal_key">> => <<"normal_value">>,
        <<"large_key">> => << 0:(1024 * 16) >>,
        <<"another_large_key">> => << 0:(1024 * 16) >>,
        <<"another_normal_key">> => <<"another_normal_value">>,
        MainBodyKey => <<"Hey from the data field!">>
    },
    Opts = test_opts(Codec),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

simple_nested_message_test(Codec) ->
    Msg = #{
        <<"a">> => <<"1">>,
        <<"nested">> => #{ <<"b">> => <<"1">> },
        <<"c">> => <<"3">>
    },
    Opts = test_opts(Codec),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

signed_nested_message_with_child_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"outer-a">> => <<"1">>,
        <<"nested">> =>
            hb_message:commit(
                #{ <<"inner-b">> => <<"1">>, <<"inner-list">> => [1, 2, 3] },
                Opts,
                Codec
            ),
        <<"outer-c">> => <<"3">>
    },
    hb_cache:write(Msg, Opts),
    Opts = test_opts(Codec),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(hb_message:match(Msg, Decoded, primary, Opts)).

nested_empty_map_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{ <<"body">> => #{ <<"empty-map-test">> => #{}}},
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

%% @doc Test that the data field is correctly managed when we have multiple
%% uses for it (the 'data' key itself, as well as keys that cannot fit in
%% tags).
nested_message_with_large_content_test(Codec) ->
    Opts = test_opts(Codec),
    MainBodyKey =
        case Codec of
            <<"ans104@1.0">> -> <<"data">>;
            _ -> <<"body">>
        end,
    Msg = #{
        <<"depth">> => <<"outer">>,
        MainBodyKey => #{
            <<"map_item">> =>
                #{
                    <<"depth">> => <<"inner">>,
                    <<"large_data_inner">> => << 0:((1 + 1024) * 8) >>
                },
            <<"large_data_outer">> => << 0:((1 + 1024) * 8) >>
        }
    },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

%% @doc Test that we can convert a 3 layer nested message into a tx record and back.
deeply_nested_message_with_content_test(Codec) ->
    Opts = test_opts(Codec),
    MainBodyKey =
        case Codec of
            <<"ans104@1.0">> -> <<"data">>;
            _ -> <<"body">>
        end,
    Msg = #{
        <<"depth">> => <<"outer">>,
        MainBodyKey => #{
            <<"map_item">> =>
                #{
                    <<"depth">> => <<"inner">>,
                    MainBodyKey => #{
                        <<"depth">> => <<"innermost">>,
                        MainBodyKey => <<"DATA">>
                    }
                }
        }
    },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

deeply_nested_message_with_only_content(Codec) ->
    MainBodyKey =
        case Codec of
            <<"ans104@1.0">> -> <<"data">>;
            _ -> <<"body">>
        end,
    Msg = #{
        <<"depth1">> => <<"outer">>,
        MainBodyKey => #{
            MainBodyKey => #{
                MainBodyKey => <<"depth2-body">>
            }
        }
    },
    Opts = test_opts(Codec),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

nested_structured_fields_test(Codec) ->
    Opts = test_opts(Codec),
    NestedMsg = #{ <<"a">> => #{ <<"b">> => 1 } },
    Encoded = hb_message:convert(NestedMsg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({matching, {input, NestedMsg}, {output, Decoded}}),
    ?assert(hb_message:match(NestedMsg, Decoded, strict, Opts)).

nested_message_with_large_keys_test(Codec) ->
    Msg = #{
        <<"a">> => <<"1">>,
        <<"long_data">> => << 0:((1 + 1024) * 8) >>,
        <<"nested">> => #{ <<"b">> => <<"1">> },
        <<"c">> => <<"3">>
    },
    Opts = test_opts(Codec),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

signed_message_encode_decode_verify_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"test-data">> => <<"TEST DATA">>,
        <<"test-key">> => <<"TEST VALUE">>
    },
    {ok, SignedMsg} =
        dev_message:commit(
            Msg,
            #{ <<"commitment-device">> => Codec },
            Opts
        ),
    ?event({signed_msg, SignedMsg}),
    ?assertEqual(true, hb_message:verify(SignedMsg, all, Opts)),
    Encoded = hb_message:convert(SignedMsg, Codec, <<"structured@1.0">>, Opts),
    ?event({msg_encoded_as_codec, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assertEqual(true, hb_message:verify(Decoded, all, Opts)),
    ?event({matching, {input, SignedMsg}, {encoded, Encoded}, {decoded, Decoded}}),
    MatchRes = hb_message:match(SignedMsg, Decoded, strict, Opts),
    ?event({match_result, MatchRes}),
    ?assert(MatchRes).

complex_signed_message_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"data">> => <<"TEST_DATA">>,
        <<"deep_data">> => #{
            <<"data">> => <<"DEEP_DATA">>,
            <<"complex_key">> => 1337,
            <<"list">> => [1,2,3]
        }
    },
    {ok, SignedMsg} =
        dev_message:commit(
            Msg,
            #{ <<"commitment-device">> => Codec },
            Opts
        ),
    Encoded = hb_message:convert(SignedMsg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assertEqual(true, hb_message:verify(Decoded, all, Opts)),
    ?event({matching, {input, SignedMsg}, {output, Decoded}}),
    ?assert(hb_message:match(SignedMsg, Decoded, strict, Opts)).

% multisignature_test(Codec) ->
%     Wallet1 = ar_wallet:new(),
%     Wallet2 = ar_wallet:new(),
%     Msg = #{
%         <<"data">> => <<"TEST_DATA">>,
%         <<"test_key">> => <<"TEST_VALUE">>
%     },
%     {ok, SignedMsg} =
%         dev_message:commit(
%             Msg,
%             #{ <<"commitment-device">> => Codec },
%             #{ priv_wallet => Wallet1 }
%         ),
%     ?event({signed_msg, SignedMsg}),
%     {ok, MsgSignedTwice} =
%         dev_message:commit(
%             SignedMsg,
%             #{ <<"commitment-device">> => Codec },
%             #{ priv_wallet => Wallet2 }
%         ),
%     ?event({signed_msg_twice, MsgSignedTwice}),
%     ?assert(verify(MsgSignedTwice)),
%     {ok, Committers} = dev_message:committers(MsgSignedTwice),
%     ?event({committers, Committers}),
%     ?assert(lists:member(hb_util:human_id(ar_wallet:to_address(Wallet1)), Committers)),
%     ?assert(lists:member(hb_util:human_id(ar_wallet:to_address(Wallet2)), Committers)).

deep_multisignature_test() ->
    % Only the `httpsig@1.0' codec supports multisignatures.
    Opts = test_opts(<<"httpsig@1.0">>),
    Codec = <<"httpsig@1.0">>,
    Wallet1 = ar_wallet:new(),
    Wallet2 = ar_wallet:new(),
    Msg = #{
        <<"data">> => <<"TEST_DATA">>,
        <<"test_key">> => <<"TEST_VALUE">>,
        <<"body">> => #{
            <<"nested_key">> => <<"NESTED_VALUE">>
        }
    },
    {ok, SignedMsg} =
        dev_message:commit(
            Msg,
            #{ <<"commitment-device">> => Codec },
            Opts#{ priv_wallet => Wallet1 }
        ),
    ?event({signed_msg, SignedMsg}),
    {ok, MsgSignedTwice} =
        dev_message:commit(
            SignedMsg,
            #{ <<"commitment-device">> => Codec },
            Opts#{ priv_wallet => Wallet2 }
        ),
    ?event({signed_msg_twice, MsgSignedTwice}),
    ?assert(hb_message:verify(MsgSignedTwice, all, Opts)),
    Committers = hb_message:signers(MsgSignedTwice, Opts),
    ?event({committers, Committers}),
    ?assert(lists:member(hb_util:human_id(ar_wallet:to_address(Wallet1)), Committers)),
    ?assert(lists:member(hb_util:human_id(ar_wallet:to_address(Wallet2)), Committers)).

deep_typed_message_id_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"data">> => <<"TEST DATA">>,
        <<"deep-data">> => #{
            <<"data">> => <<"DEEP DATA">>,
            <<"complex-key">> => 1337,
            <<"list">> => [1,2,3]
        }
    },
    InitID = hb_message:id(Msg, none, Opts),
    ?event({init_id, InitID}),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    DecodedID = hb_message:id(Decoded, none, Opts),
    ?event({decoded_id, DecodedID}),
    ?event({stages, {init, Msg}, {encoded, Encoded}, {decoded, Decoded}}),
    ?assertEqual(
        InitID,
        DecodedID
    ).

signed_deep_message_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"test_key">> => <<"TEST_VALUE">>,
        <<"body">> => #{
            <<"nested_key">> =>
                #{
                    <<"body">> => <<"NESTED_DATA">>,
                    <<"nested_key">> => <<"NESTED_VALUE">>
                },
            <<"nested_key2">> => <<"NESTED_VALUE2">>
        }
    },
    EncDec =
        hb_message:convert(
            hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
            <<"structured@1.0">>,
            Codec,
            Opts
        ),
    ?event({enc_dec, EncDec}),
    {ok, SignedMsg} =
        dev_message:commit(
            EncDec,
            #{ <<"commitment-device">> => Codec },
            Opts
        ),
    ?event({signed_msg, SignedMsg}),
    {ok, Res} = dev_message:verify(SignedMsg, #{ <<"committers">> => <<"all">>}, Opts),
    ?event({verify_res, Res}),
    ?assertEqual(true, hb_message:verify(SignedMsg, all, Opts)),
    ?event({verified, SignedMsg}),
    Encoded = hb_message:convert(SignedMsg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    {ok, DecodedRes} =
        dev_message:verify(
            Decoded,
            #{ <<"committers">> => <<"all">>},
            Opts
        ),
    ?event({verify_decoded_res, DecodedRes}),
    ?assert(
        hb_message:match(
            SignedMsg,
            Decoded,
            strict,
            Opts
        )
    ).

signed_list_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{ <<"key-with-list">> => [1.0, 2.0, 3.0] },
    Signed = hb_message:commit(Msg, Opts, Codec),
    ?assert(hb_message:verify(Signed, all, Opts)),
    Encoded = hb_message:convert(Signed, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assert(hb_message:verify(Decoded, all, Opts)),
    ?assert(hb_message:match(Signed, Decoded, strict, Opts)).

unsigned_id_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{ <<"data">> => <<"TEST_DATA">> },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?assertEqual(
        dev_message:id(Decoded, #{ <<"committers">> => <<"none">>}, Opts),
        dev_message:id(Msg, #{ <<"committers">> => <<"none">>}, Opts)
    ).

% signed_id_test_disabled() ->
%     TX = #tx {
%         data = <<"TEST_DATA">>,
%         tags = [{<<"TEST_KEY">>, <<"TEST_VALUE">>}]
%     },
%     SignedTX = ar_bundles:sign_item(TX, hb:wallet()),
%     ?assert(ar_bundles:verify_item(SignedTX)),
%     SignedMsg = hb_codec_tx:from(SignedTX),
%     ?assertEqual(
%         hb_util:encode(ar_bundles:id(SignedTX, signed)),
%         hb_util:id(SignedMsg, signed)
%     ).

message_with_simple_embedded_list_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{ <<"list">> => [<<"value-1">>, <<"value-2">>, <<"value-3">>] },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

empty_string_in_nested_tag_test(Codec) ->
    Opts = test_opts(Codec),
    Msg =
        #{
            <<"dev">> =>
                #{
                    <<"stderr">> => <<"aa">>,
                    <<"stdin">> => <<"b">>,
                    <<"stdout">> => <<"c">>
                }
        },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

hashpath_sign_verify_test(Codec) ->
    Opts = test_opts(Codec),
    Msg =
        #{
            <<"test_key">> => <<"TEST_VALUE">>,
            <<"body">> => #{
                <<"nested_key">> =>
                    #{
                        <<"body">> => <<"NESTED_DATA">>,
                        <<"nested_key">> => <<"NESTED_VALUE">>
                    },
                <<"nested_key2">> => <<"NESTED_VALUE2">>
            },
            <<"priv">> => #{
                <<"hashpath">> =>
                    hb_path:hashpath(
                        hb_util:human_id(crypto:strong_rand_bytes(32)),
                        hb_util:human_id(crypto:strong_rand_bytes(32)),
                        fun hb_crypto:sha256_chain/2,
                        #{}
                    )
            }
        },
    ?event({msg, {explicit, Msg}}),
    SignedMsg = hb_message:commit(Msg, Opts, Codec),
    ?event({signed_msg, {explicit, SignedMsg}}),
    {ok, Res} = dev_message:verify(SignedMsg, #{ <<"committers">> => <<"all">>}, Opts),
    ?event({verify_res, {explicit, Res}}),
    ?assert(hb_message:verify(SignedMsg, all, Opts)),
    ?event({verified, {explicit, SignedMsg}}),
    Encoded = hb_message:convert(SignedMsg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assert(hb_message:verify(Decoded, all, Opts)),
    ?assert(
        hb_message:match(
            SignedMsg,
            Decoded,
            strict,
            Opts
        )
    ).

signed_message_with_derived_components_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"path">> => <<"/test">>,
        <<"authority">> => <<"example.com">>,
        <<"scheme">> => <<"https">>,
        <<"method">> => <<"GET">>,
        <<"target-uri">> => <<"/test">>,
        <<"request-target">> => <<"/test">>,
        <<"status">> => <<"200">>,
        <<"reason-phrase">> => <<"OK">>,
        <<"body">> => <<"TEST_DATA">>,
        <<"content-digest">> => <<"TEST_DIGEST">>,
        <<"normal">> => <<"hello">>
    },
    {ok, SignedMsg} =
        dev_message:commit(
            Msg,
            #{ <<"commitment-device">> => Codec },
            Opts
        ),
    ?event({signed_msg, SignedMsg}),
    ?assert(hb_message:verify(SignedMsg, all, Opts)),
    Encoded = hb_message:convert(SignedMsg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assert(hb_message:verify(Decoded, all, Opts)),
    ?assert(
        hb_message:match(
            SignedMsg,
            Decoded,
            strict,
            Opts
        )
    ).

committed_keys_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{ <<"a">> => 1, <<"b">> => 2, <<"c">> => 3 },
    Signed = hb_message:commit(Msg, Opts, Codec),
    CommittedKeys = hb_message:committed(Signed, all, Opts),
    ?event({committed_keys, CommittedKeys}),
    ?assert(hb_message:verify(Signed, all, Opts)),
    ?assert(lists:member(<<"a">>, CommittedKeys)),
    ?assert(lists:member(<<"b">>, CommittedKeys)),
    ?assert(lists:member(<<"c">>, CommittedKeys)),
    MsgToFilter = Signed#{ <<"bad-key">> => <<"BAD VALUE">> },
    ?assert(
        not lists:member(
            <<"bad-key">>,
            hb_message:committed(MsgToFilter, all, Opts)
        )
    ).

committed_empty_keys_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"very">> => <<>>,
        <<"exciting">> => #{},
        <<"values">> => [],
        <<"non-empty">> => <<"TEST">>
    },
    Signed = hb_message:commit(Msg, Opts, Codec),
    ?assert(hb_message:verify(Signed, all, Opts)),
    CommittedKeys = hb_message:committed(Signed, all, Opts),
    ?event({committed_keys, CommittedKeys}),
    ?assert(lists:member(<<"very">>, CommittedKeys)),
    ?assert(lists:member(<<"exciting">>, CommittedKeys)),
    ?assert(lists:member(<<"values">>, CommittedKeys)),
    ?assert(lists:member(<<"non-empty">>, CommittedKeys)).

deeply_nested_committed_keys_test() ->
    Opts = test_opts(<<"httpsig@1.0">>),
    Msg = #{
        <<"a">> => 1,
        <<"b">> => #{ <<"c">> => #{ <<"d">> => <<0:((1 + 1024) * 1024)>> } },
        <<"e">> => <<0:((1 + 1024) * 1024)>>
    },
    Signed = hb_message:commit(Msg, Opts, <<"httpsig@1.0">>),
    {ok, WithOnlyCommitted} = hb_message:with_only_committed(Signed, Opts),
    Committed = hb_message:committed(Signed, all, Opts),
    ToCompare = hb_maps:without([<<"commitments">>], WithOnlyCommitted),
    ?event(
        {msgs,
            {base, Msg},
            {signed, Signed},
            {committed, Committed},
            {with_only_committed, WithOnlyCommitted},
            {to_compare, ToCompare}
        }
    ),
    ?assert(
        hb_message:match(
            Msg,
            ToCompare,
            strict,
            Opts
        )
    ).

signed_with_inner_signed_message_test(Codec) ->
    Opts = test_opts(Codec),
    Msg =
        hb_message:commit(
            #{
                <<"a">> => 1,
                <<"inner">> =>
                    hb_maps:merge(
                        InnerSigned =
                            hb_message:commit(
                                #{
                                    <<"c">> => <<"abc">>,
                                    <<"e">> => 5
                                },
                                Opts,
                                Codec
                            ),
                        % Uncommitted keys that should be ripped out of the inner
                        % message by `with_only_committed'. These should still be
                        % present in the `with_only_committed' outer message. 
                        % For now, only `httpsig@1.0' supports stripping
                        % non-committed keys.
                        case Codec of
                            <<"httpsig@1.0">> ->
                                #{
                                    <<"f">> => 6,
                                    <<"g">> => 7
                                };
                            _ -> #{}
                        end
                    )
            },
            Opts,
            Codec
        ),
    ?event({initial_msg, Msg}),
    % 1. Verify the outer message without changes.
    ?assert(hb_message:verify(Msg, all, Opts)),
    Inner = hb_maps:get(<<"inner">>, Msg, not_found, Opts),
    {ok, CommittedInner} =
        hb_message:with_only_committed(
            Inner,
            Opts
        ),
    ?event({committed_inner, CommittedInner}),
    ?event({inner_committers, hb_message:signers(CommittedInner, Opts)}),
    % 2. Verify the inner message without changes.
    ?assert(hb_message:verify(CommittedInner, signers, Opts)),
    % 3. Convert the message to the format and back.
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    %?event({encoded_body, {string, hb_maps:get(<<"body">>, Encoded)}}, #{}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    % 4. Verify the outer message after decode.
    ?assert(
        hb_message:match(
            InnerSigned,
            hb_maps:get(<<"inner">>, Decoded, not_found, Opts),
            primary,
            Opts
        )
    ),
    ?assert(hb_message:verify(Decoded, all, Opts)),
    % 5. Verify the inner message from the converted message, applying
    % `with_only_committed` first.
    InnerDecoded = hb_maps:get(<<"inner">>, Decoded, not_found, Opts),
    ?event({inner_decoded, InnerDecoded}),
    % Applying `with_only_committed' should verify the inner message.
    {ok, CommittedInnerOnly} =
        hb_message:with_only_committed(
            InnerDecoded,
            Opts
        ),
    ?assert(hb_message:verify(CommittedInnerOnly, signers, Opts)).

large_body_committed_keys_test(Codec) ->
    Opts = test_opts(Codec),
    case Codec of
        <<"httpsig@1.0">> ->
            Msg = #{
                <<"a">> => 1,
                <<"b">> => 2,
                <<"c">> => #{ <<"d">> => << 1:((1 + 1024) * 1024) >> }
            },
            Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
            ?event({encoded, Encoded}),
            Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
            ?event({decoded, Decoded}),
            Signed = hb_message:commit(Decoded, Opts, Codec),
            ?event({signed, Signed}),
            CommittedKeys = hb_message:committed(Signed, all, Opts),
            ?assert(lists:member(<<"a">>, CommittedKeys)),
            ?assert(lists:member(<<"b">>, CommittedKeys)),
            ?assert(lists:member(<<"c">>, CommittedKeys)),
            MsgToFilter = Signed#{ <<"bad-key">> => <<"BAD VALUE">> },
            ?assert(
                not lists:member(
                    <<"bad-key">>,
                    hb_message:committed(MsgToFilter, all, Opts)
                )
            );
        _ ->
            skip
    end.

sign_node_message_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = hb_message:commit(hb_opts:default_message(), Opts, Codec),
    ?event({committed, Msg}),
    ?assert(hb_message:verify(Msg, all, Opts)),
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    MatchRes = hb_message:match(Msg, Decoded, strict, Opts),
    ?assertEqual(true, MatchRes),
    ?assert(hb_message:verify(Decoded, all, Opts)).

nested_body_list_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"body">> =>
            [
                #{
                    <<"test-key">> =>
                        <<"TEST VALUE #", (integer_to_binary(X))/binary>>
                }
            ||
                X <- lists:seq(1, 3)
            ]
    },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    ?event(encoded, {encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

recursive_nested_list_test(Codec) ->
    Opts = test_opts(Codec),
    % This test is to ensure that the codec can handle arbitrarily deep nested
    % lists.
    Msg = #{
        <<"body">> =>
            [
                [
                    [
                        <<
                            "TEST VALUE #",
                            (integer_to_binary(X))/binary,
                            "-",
                            (integer_to_binary(Y))/binary,
                            "-",
                            (integer_to_binary(Z))/binary
                        >>
                    ||
                        Z <- lists:seq(1, 3)
                    ]
                ||
                    Y <- lists:seq(1, 3)
                ]
            ||
                X <- lists:seq(1, 3)
            ]
    },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    ?event(encoded, {encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

priv_survives_conversion_test(<<"ans104@1.0">>) -> skip;
priv_survives_conversion_test(<<"json@1.0">>) -> skip;
priv_survives_conversion_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"data">> => <<"TEST_DATA">>,
        <<"priv">> => #{ <<"test_key">> => <<"TEST_VALUE">> }
    },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, Encoded}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)),
    ?assertMatch(
        #{ <<"test_key">> := <<"TEST_VALUE">> },
        maps:get(<<"priv">>, Decoded)
    ).

encode_balance_table(Size, Codec) ->
    Opts = test_opts(Codec),
    Msg =
        #{
            hb_util:encode(crypto:strong_rand_bytes(32)) =>
                rand:uniform(1_000_000_000_000_000)
        ||
            _ <- lists:seq(1, Size)
        },
    Encoded = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    ?event({encoded, {explicit, Encoded}}),
    Decoded = hb_message:convert(Encoded, <<"structured@1.0">>, Codec, Opts),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, Decoded, strict, Opts)).

encode_small_balance_table_test(Codec) ->
    encode_balance_table(5, Codec).

encode_large_balance_table_test(Codec) ->
    encode_balance_table(1000, Codec).

sign_links_test(Codec) ->
    Opts = test_opts(Codec),
    % Make a message with definitively non-accessible lazy-loadable links. Sign
    % it, ensuring that we can produce signatures and IDs without having the 
    % data directly in memory.
    Msg = #{
        <<"immediate-key">> => <<"immediate-value">>,
        <<"submap+link">> => hb_util:human_id(crypto:strong_rand_bytes(32))
    },
    Signed = hb_message:commit(Msg, Opts, Codec),
    ?event(debug, {signed, Signed}),
    ?assert(hb_message:verify(Signed, all, Opts)).

id_of_linked_message_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"immediate-key">> => <<"immediate-value">>,
        <<"link-key">> =>
            {link, hb_util:human_id(crypto:strong_rand_bytes(32)), #{
                <<"type">> => <<"link">>,
                <<"lazy">> => false
            }}
    },
    UnsignedID = hb_message:id(Msg, Opts),
    ?event({id, UnsignedID}),
    EncMsg = hb_message:convert(Msg, Codec, <<"structured@1.0">>, Opts),
    DecMsg = hb_message:convert(EncMsg, <<"structured@1.0">>, Codec, Opts),
    UnsignedID2 = hb_message:id(DecMsg, Opts),
    ?assertEqual(UnsignedID, UnsignedID2).

sign_deep_message_from_lazy_cache_read_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"immediate-key">> => <<"immediate-value">>,
        <<"link-key">> => #{
            <<"immediate-key-2">> => <<"link-value">>,
            <<"link-key-2">> => #{
                <<"immediate-key-3">> => <<"link-value-2">>
            }
        }
    },
    % Write the message to the store to ensure that we get lazy-loadable links.
    {ok, Path} = hb_cache:write(Msg, Opts),
    {ok, ReadMsg} = hb_cache:read(Path, Opts),
    ?event({read, ReadMsg}),
    Signed = hb_message:commit(ReadMsg, Opts, Codec),
    ?event({signed, Signed}),
    ?assert(
        lists:all(
            fun({_K, Value}) -> not is_map(Value) end,
            maps:to_list(maps:without([<<"commitments">>, <<"priv">>], Signed))
        )
    ),
    ?assert(hb_message:verify(Signed, all, Opts)).

id_of_deep_message_and_link_message_match_test(Codec) ->
    Opts = test_opts(Codec),
    Msg = #{
        <<"immediate-key">> => <<"immediate-value">>,
        <<"link-key">> => #{
            <<"immediate-key-2">> => <<"immediate-value-2">>,
            <<"link-key-2">> => #{
                <<"immediate-key-3">> => <<"immediate-value-3">>
            }
        }
    },
    Linkified = hb_link:normalize(Msg, offload, Opts),
    ?event(linkify, {test_recvd_linkified, {msg, Linkified}}),
    BaseID = hb_message:id(Msg, Opts),
    ?event(linkify, {test_recvd_nonlink_id, {id, BaseID}}),
    LinkID = hb_message:id(Linkified, Opts),
    ?event(linkify, {test_recvd_link_id, {id, LinkID}}),
    ?assertEqual(BaseID, LinkID).

%% Ensure that we can write a message with multiple commitments to the store,
%% then read back all of the written commitments by loading the message's 
%% unsigned ID.
find_multiple_commitments_test() ->
    Opts = test_opts(<<"structured@1.0">>),
    Store = hb_opts:get(store, no_store, Opts),
    hb_store:reset(Store),
    Msg = #{
        <<"a">> => 1,
        <<"b">> => 2,
        <<"c">> => 3
    },
    Sig1 = hb_message:commit(Msg, Opts#{ priv_wallet => ar_wallet:new() }),
    {ok, _} = hb_cache:write(Sig1, Opts),
    Sig2 = hb_message:commit(Msg, Opts#{ priv_wallet => ar_wallet:new() }),
    {ok, _} = hb_cache:write(Sig2, Opts),
    {ok, ReadMsg} = hb_cache:read(hb_message:id(Msg, none, Opts), Opts),
    LoadedCommitments = hb_cache:ensure_all_loaded(ReadMsg, Opts),
    ?event(debug_commitments, {read, LoadedCommitments}),
    ok.
