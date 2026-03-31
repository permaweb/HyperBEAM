%%% @doc Allows Arweave offsets to be used via deterministic hostname-safe
%%% aliases, then delegates the actual load to `arweave@2.9`.
-module(dev_offset_name).
-export([info/1, encode/1, decode/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BASE32_ALPHABET, <<"0123456789abcdefghjkmnpqrstvwxyz">>).
-define(DEFAULT_TARGET_DEVICE, #{ <<"device">> => <<"arweave@2.9">> }).
-define(ADJECTIVES,
    [
        <<"amber">>, <<"brisk">>, <<"calm">>, <<"clever">>,
        <<"copper">>, <<"coral">>, <<"ember">>, <<"feral">>,
        <<"floral">>, <<"gentle">>, <<"golden">>, <<"grand">>,
        <<"hidden">>, <<"lucid">>, <<"mellow">>, <<"misty">>,
        <<"noble">>, <<"polished">>, <<"quiet">>, <<"rapid">>,
        <<"silent">>, <<"silver">>, <<"solar">>, <<"steady">>,
        <<"swift">>, <<"tidal">>, <<"velvet">>, <<"vivid">>,
        <<"wild">>, <<"winter">>, <<"yellow">>, <<"zesty">>
    ]
).
-define(NOUNS,
    [
        <<"anchor">>, <<"beacon">>, <<"brook">>, <<"canyon">>,
        <<"cedar">>, <<"cloud">>, <<"comet">>, <<"creek">>,
        <<"delta">>, <<"dune">>, <<"falcon">>, <<"field">>,
        <<"forest">>, <<"harbor">>, <<"island">>, <<"lagoon">>,
        <<"meadow">>, <<"mesa">>, <<"mist">>, <<"orchid">>,
        <<"pebble">>, <<"prairie">>, <<"quarry">>, <<"river">>,
        <<"rocket">>, <<"shadow">>, <<"summit">>, <<"thunder">>,
        <<"valley">>, <<"willow">>, <<"wind">>, <<"zephyr">>
    ]
).

info(_Opts) ->
    #{
        default => fun get/4,
        excludes => [<<"keys">>, <<"set">>]
    }.

%% @doc Resolve an offset alias by decoding it back to a numeric offset, then
%% delegating to the standard Arweave offset resolver.
get(Key, _, _Req, Opts) ->
    case decode(Key) of
        {ok, Offset} ->
            hb_ao:resolve(target_device(Opts), integer_to_binary(Offset), Opts);
        error ->
            {error, not_found}
    end.

%% @doc Convert an offset into a hostname-safe alias.
encode(Offset) when is_integer(Offset), Offset >= 0 ->
    {Adjective, Noun} = checksum_words(Offset),
    Tail = encode_base32(Offset),
    <<
        Adjective/binary,
        "-",
        Noun/binary,
        "-",
        Tail/binary
    >>;
encode(Offset) when is_binary(Offset) ->
    try encode(hb_util:int(Offset))
    catch _:_ -> error({cannot_encode_offset_alias, Offset})
    end;
encode(Offset) ->
    error({cannot_encode_offset_alias, Offset}).

%% @doc Decode an offset alias back to its canonical integer offset.
decode(Alias) when is_binary(Alias) ->
    case binary:split(Alias, <<"-">>, [global]) of
        [_Adjective, _Noun, Tail] -> decode_canonical_alias(Alias, Tail);
        _ -> error
    end;
decode(_Alias) ->
    error.

target_device(Opts) ->
    hb_opts:get(offset_name_target, ?DEFAULT_TARGET_DEVICE, Opts).

encode_base32(0) ->
    <<"0">>;
encode_base32(Offset) when is_integer(Offset), Offset > 0 ->
    encode_base32(Offset, <<>>).

encode_base32(0, Acc) ->
    Acc;
encode_base32(Offset, Acc) ->
    Digit = binary:at(?BASE32_ALPHABET, Offset rem 32),
    encode_base32(Offset div 32, <<Digit, Acc/binary>>).

decode_base32(<<>>) ->
    error;
decode_base32(AliasTail) when is_binary(AliasTail) ->
    decode_base32(AliasTail, 0).

decode_base32(<<>>, Offset) ->
    {ok, Offset};
decode_base32(<<Char, Rest/binary>>, Offset) ->
    case decode_digit(Char) of
        error ->
            error;
        Value ->
            decode_base32(Rest, Offset * 32 + Value)
    end.

decode_canonical_alias(Alias, Tail) ->
    case decode_base32(Tail) of
        {ok, Offset} ->
            case Alias =:= encode(Offset) of
                true -> {ok, Offset};
                _ -> error
            end;
        error ->
            error
    end.

decode_digit(Char) ->
    Normalized = normalize_char(Char),
    case binary:match(?BASE32_ALPHABET, <<Normalized>>) of
        {Index, 1} -> Index;
        nomatch -> error
    end.

normalize_char(Char) when Char >= $A, Char =< $Z ->
    Char + 32;
normalize_char(Char) ->
    Char.

checksum_words(Offset) ->
    <<AdjByte, NounByte, _/binary>> =
        crypto:hash(sha256, integer_to_binary(Offset)),
    {
        lists:nth((AdjByte rem length(?ADJECTIVES)) + 1, ?ADJECTIVES),
        lists:nth((NounByte rem length(?NOUNS)) + 1, ?NOUNS)
    }.

test_target_device() ->
    #{
        <<"device">> =>
            #{
                info =>
                    fun() ->
                        #{
                            default =>
                                fun(Key, _, _Req, _Opts) ->
                                    {ok, #{ <<"resolved-offset">> => Key }}
                                end
                        }
                    end
            }
    }.

roundtrip_test() ->
    Offset = 152974576623958,
    ?assertEqual(
        {ok, Offset},
        decode(encode(Offset))
    ).

format_shape_test() ->
    [Adjective, Noun, Tail] =
        binary:split(encode(152974576623958), <<"-">>, [global]),
    ?assert(lists:member(Adjective, ?ADJECTIVES)),
    ?assert(lists:member(Noun, ?NOUNS)),
    ?assertMatch({ok, _}, decode(<<Adjective/binary, "-", Noun/binary, "-", Tail/binary>>)).

uppercase_alias_rejected_test() ->
    Alias = encode(152974576623958),
    ?assertEqual(
        error,
        decode(list_to_binary(string:uppercase(binary_to_list(Alias))))
    ).

invalid_alias_test() ->
    [_, Noun, Tail] =
        binary:split(encode(152974576623958), <<"-">>, [global]),
    ?assertEqual(error, decode(<<"wrong-", Noun/binary, "-", Tail/binary>>)),
    ?assertEqual(error, decode(<<"amber-", Noun/binary, "-open">>)),
    ?assertEqual(error, decode(<<"only-two">>)).

leading_zero_tail_rejected_test() ->
    [Adjective, Noun, Tail] =
        binary:split(encode(152974576623958), <<"-">>, [global]),
    ?assertEqual(
        error,
        decode(<<Adjective/binary, "-", Noun/binary, "-0", Tail/binary>>)
    ).

get_delegates_to_offset_target_test() ->
    Offset = 152974576623958,
    Alias = encode(Offset),
    {ok, Resolved} =
        get(
            Alias,
            #{},
            #{},
            #{ offset_name_target => test_target_device() }
        ),
    ?assertEqual(
        integer_to_binary(Offset),
        maps:get(<<"resolved-offset">>, Resolved)
    ).

reverse_alias_resolve_test() ->
    Alias = <<"grand-brook-az9wqrtem2">>,
    {ok, Resolved} =
        hb_ao:resolve(
            #{ <<"device">> => <<"offset-name@1.0">> },
            Alias,
            #{ offset_name_target => test_target_device() }
        ),
    ?assertEqual(
        <<"386268681550466">>,
        maps:get(<<"resolved-offset">>, Resolved)
    ).

name_resolver_lookup_test() ->
    Offset = 152974576623958,
    Alias = encode(Offset),
    {ok, Resolved} =
        hb_ao:resolve_many(
            [
                #{ <<"device">> => <<"name@1.0">> },
                #{ <<"path">> => Alias }
            ],
            #{
                offset_name_target => test_target_device(),
                name_resolvers => [#{ <<"device">> => <<"offset-name@1.0">> }]
            }
        ),
    ?assertEqual(
        integer_to_binary(Offset),
        maps:get(<<"resolved-offset">>, Resolved)
    ).
