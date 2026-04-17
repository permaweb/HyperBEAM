%%% @doc Allows Arweave offsets to be used via deterministic hostname-safe
%%% aliases, then delegates the actual load to `arweave@2.9`.
-module(dev_offset_name).
-export([info/1, alias/3, offset/3, encode/1, decode/1]).
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
        <<"wild">>, <<"winter">>, <<"yellow">>, <<"zesty">>,
        <<"ancient">>, <<"autumn">>, <<"azure">>, <<"bright">>,
        <<"bronze">>, <<"crimson">>, <<"crystal">>, <<"curious">>,
        <<"daring">>, <<"dusky">>, <<"eager">>, <<"frozen">>,
        <<"glimmer">>, <<"hollow">>, <<"iron">>, <<"ivory">>,
        <<"kindred">>, <<"lunar">>, <<"marble">>, <<"midnight">>,
        <<"obsidian">>, <<"ocean">>, <<"opal">>, <<"patient">>,
        <<"royal">>, <<"sacred">>, <<"scarlet">>, <<"shaded">>,
        <<"ashen">>, <<"bitter">>, <<"hazy">>, <<"verdant">>,
        <<"agile">>, <<"alpine">>, <<"apricot">>, <<"bold">>,
        <<"breezy">>, <<"cobalt">>, <<"cosmic">>, <<"desert">>,
        <<"electric">>, <<"emerald">>, <<"faithful">>, <<"fiery">>,
        <<"glassy">>, <<"grassy">>, <<"humble">>, <<"icy">>,
        <<"jolly">>, <<"lucky">>, <<"magnetic">>, <<"neat">>,
        <<"nimble">>, <<"orange">>, <<"pepper">>, <<"rosy">>,
        <<"rustic">>, <<"sandy">>, <<"smoky">>, <<"snowy">>,
        <<"spotted">>, <<"sunny">>, <<"tranquil">>, <<"urban">>,
        <<"warm">>, <<"wise">>, <<"woody">>, <<"young">>
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
        <<"valley">>, <<"willow">>, <<"wind">>, <<"zephyr">>,
        <<"asteroid">>, <<"bay">>, <<"blossom">>, <<"branch">>,
        <<"citadel">>, <<"cliff">>, <<"cove">>, <<"ember">>,
        <<"feather">>, <<"fjord">>, <<"flare">>, <<"garden">>,
        <<"glacier">>, <<"grove">>, <<"horizon">>, <<"jungle">>,
        <<"kingfisher">>, <<"lantern">>, <<"lotus">>, <<"meteor">>,
        <<"monsoon">>, <<"moon">>, <<"morning">>, <<"pine">>,
        <<"reef">>, <<"signal">>, <<"stone">>, <<"sunrise">>,
        <<"aurora">>, <<"bridge">>, <<"cascade">>, <<"voyage">>,
        <<"airship">>, <<"arbor">>, <<"campfire">>, <<"caravan">>,
        <<"chamber">>, <<"compass">>, <<"crown">>, <<"dawn">>,
        <<"estuary">>, <<"firefly">>, <<"fountain">>, <<"galaxy">>,
        <<"gate">>, <<"hearth">>, <<"hill">>, <<"lighthouse">>,
        <<"orchard">>, <<"owl">>, <<"palace">>, <<"path">>,
        <<"pearl">>, <<"planet">>, <<"pond">>, <<"prism">>,
        <<"ridge">>, <<"sail">>, <<"spring">>, <<"star">>,
        <<"stream">>, <<"temple">>, <<"torch">>, <<"tower">>,
        <<"trail">>, <<"wheat">>, <<"wave">>, <<"wing">>
    ]
).

info(_Opts) ->
    #{
        default => fun get/4,
        excludes => [<<"keys">>, <<"set">>]
    }.

%% @doc Route `/~offset-name@1.0/alias/<offset>` to the forward alias lookup.
alias(_Base, _Req, _Opts) ->
    {ok, {as, alias_route_device(), #{}}}.

%% @doc Route `/~offset-name@1.0/offset/<alias>` to the reverse alias lookup.
offset(_Base, _Req, _Opts) ->
    {ok, {as, offset_route_device(), #{}}}.

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

alias_route_device() ->
    #{
        info =>
            fun() ->
                #{
                    default => fun alias_lookup/4,
                    excludes => [<<"keys">>, <<"set">>]
                }
            end
    }.

alias_lookup(Key, _, _Req, _Opts) ->
    encode_result(Key).

offset_route_device() ->
    #{
        info =>
            fun() ->
                #{
                    default => fun offset_lookup/4,
                    excludes => [<<"keys">>, <<"set">>]
                }
            end
    }.

offset_lookup(Key, _, _Req, _Opts) ->
    decode_result(Key).

encode_result(Offset) ->
    try
        {ok, encode(Offset)}
    catch
        _:_ -> {error, not_found}
    end.

decode_result(Alias) ->
    case decode(Alias) of
        {ok, Offset} -> {ok, integer_to_binary(Offset)};
        error -> {error, not_found}
    end.

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

word_pool_size_test() ->
    ?assertEqual(100, length(?ADJECTIVES)),
    ?assertEqual(100, length(?NOUNS)).

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

alias_path_route_test() ->
    Offset = <<"152974576623958">>,
    ?assertEqual(
        {ok, encode(Offset)},
        hb_ao:resolve(
            #{ <<"device">> => <<"offset-name@1.0">> },
            <<"alias/152974576623958">>,
            #{}
        )
    ).

offset_path_route_test() ->
    Alias = encode(152974576623958),
    ?assertEqual(
        {ok, <<"152974576623958">>},
        hb_ao:resolve(
            #{ <<"device">> => <<"offset-name@1.0">> },
            <<"offset/", Alias/binary>>,
            #{}
        )
    ).

reverse_alias_resolve_test() ->
    Offset = 386268681550466,
    Alias = encode(Offset),
    {ok, Resolved} =
        hb_ao:resolve(
            #{ <<"device">> => <<"offset-name@1.0">> },
            Alias,
            #{ offset_name_target => test_target_device() }
        ),
    ?assertEqual(
        integer_to_binary(Offset),
        maps:get(<<"resolved-offset">>, Resolved)
    ).

reverse_known_alias_fixture_test() ->
    {ok, Resolved} =
        hb_ao:resolve(
            #{ <<"device">> => <<"offset-name@1.0">> },
            <<"frozen-campfire-az9wqrtem2">>,
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
