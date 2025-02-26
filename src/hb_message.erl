
%%% @doc This module acts an adapter between messages, as modeled in the
%%% Converge Protocol, and their uderlying binary representations and formats.
%%% 
%%% Unless you are implementing a new message serialization codec, you should
%%% not need to interact with this module directly. Instead, use the
%%% `hb_converge' interfaces to interact with all messages. The `dev_message'
%%% module implements a device interface for abstracting over the different
%%% message formats.
%%% 
%%% `hb_message' and the HyperBEAM caches can interact with multiple different
%%% types of message formats:
%%% 
%%%     - Richly typed Converge structured messages.
%%%     - Arweave transations.
%%%     - ANS-104 data items.
%%%     - HTTP Signed Messages.
%%%     - Flat Maps.
%%% 
%%% This module is responsible for converting between these formats. It does so
%%% by normalizing messages to a common format: `Type Annotated Binary Messages`
%%% (TABM). TABMs are deep Erlang maps with keys than only contain either other
%%% TABMs or binary values. By marshalling all messages into this format, they
%%% can easily be coerced into other output formats. For example, generating a
%%% `HTTP Signed Message` format output from an Arweave transaction. TABM is
%%% also a simple format from a computational perspective (only binary literals
%%% and O(1) access maps), such that operations upon them are efficient.
%%% 
%%% The structure of the conversions is as follows:
%%% 
%%% ```
%%%     Arweave TX/ANS-104 ==> dev_codec_ans104:from/1 ==> TABM
%%%     HTTP Signed Message ==> dev_codec_httpsig_conv:from/1 ==> TABM
%%%     Flat Maps ==> dev_codec_flat:from/1 ==> TABM
%%% 
%%%     TABM ==> dev_codec_structured:to/1 ==> Converge Message
%%%     Converge Message ==> dev_codec_structured:from/1 ==> TABM
%%% 
%%%     TABM ==> dev_codec_ans104:to/1 ==> Arweave TX/ANS-104
%%%     TABM ==> dev_codec_httpsig_conv:to/1 ==> HTTP Signed Message
%%%     TABM ==> dev_codec_flat:to/1 ==> Flat Maps
%%%     ...
%%% '''
%%% 
%%% Additionally, this module provides a number of utility functions for
%%% manipulating messages. For example, `hb_message:sign/2' to sign a message of
%%% arbitrary type, or `hb_message:format/1' to print a Converge/TABM message in
%%% a human-readable format.
%%% 
%%% The `hb_cache' module is responsible for storing and retrieving messages in
%%% the HyperBEAM stores configured on the node. Each store has its own storage
%%% backend, but each works with simple key-value pairs. Subsequently, the 
%%% `hb_cache' module uses TABMs as the internal format for storing and 
%%% retrieving messages.
-module(hb_message).
-export([id/1, id/2, id/3]).
-export([convert/3, convert/4, to_tabm/3, from_tabm/3, unattested/1]).
-export([verify/1, verify/2, attest/2, attest/3, signers/1, type/1, minimize/1]).
-export([attested/1, attested/2, attested/3, with_only_attested/1]).
-export([match/2, match/3, find_target/3]).
%%% Helpers:
-export([default_tx_list/0, filter_default_keys/1]).
%%% Debugging tools:
-export([print/1, format/1, format/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Convert a message from one format to another. Taking a message in the
%% source format, a target format, and a set of opts. If not given, the source
%% is assumed to be `converge`. Additional codecs can be added by ensuring they
%% are part of the `Opts` map -- either globally, or locally for a computation.
%% 
%% The encoding happens in two phases:
%% 1. Convert the message to a TABM.
%% 2. Convert the TABM to the target format.
%% 
%% The conversion to a TABM is done by the `converge' codec, which is always
%% available. The conversion from a TABM is done by the target codec.
convert(Msg, TargetFormat, Opts) ->
    convert(Msg, TargetFormat, <<"structured@1.0">>, Opts).
convert(Msg, TargetFormat, SourceFormat, Opts) ->
    TABM = to_tabm(Msg, SourceFormat, Opts),
    case TargetFormat of
        tabm -> TABM;
        _ -> from_tabm(TABM, TargetFormat, Opts)
    end.

to_tabm(Msg, SourceFormat, Opts) ->
    SourceCodecMod = get_codec(SourceFormat, Opts),
    case SourceCodecMod:from(Msg) of
        TypicalMsg when is_map(TypicalMsg) ->
            TypicalMsg;
        OtherTypeRes -> OtherTypeRes
    end.

from_tabm(Msg, TargetFormat, Opts) ->
    TargetCodecMod = get_codec(TargetFormat, Opts),
    TargetCodecMod:to(Msg).

%% @doc Return the ID of a message.
id(Msg) -> id(Msg, unattested).
id(Msg, Attestors) -> id(Msg, Attestors, #{}).
id(Msg, RawAttestors, Opts) ->
    Attestors =
        case RawAttestors of
            unattested -> <<"none">>;
            unsigned -> <<"none">>;
            none -> <<"none">>;
            all -> <<"all">>;
            signed -> <<"all">>;
            List -> List
        end,
    ?event({getting_id, {msg, Msg}, {attestors, Attestors}}),
    {ok, ID} =
        hb_converge:resolve(
            Msg,
            #{ <<"path">> => <<"id">>, <<"attestors">> => Attestors },
            Opts
        ),
    hb_util:human_id(ID).

%% @doc Return a message with only the attested keys.
with_only_attested(Msg) when is_map(Msg) ->
    case is_map(Msg) andalso maps:is_key(<<"attestations">>, Msg) of
        true ->
            try
                Enc = hb_message:convert(Msg, <<"httpsig@1.0">>, #{}),
                ?event({enc, Enc}),
                Dec = hb_message:convert(Enc, <<"structured@1.0">>, <<"httpsig@1.0">>, #{}),
                ?event({dec, Dec}),
                Attested = hb_message:attested(Dec),
                ?event({attested, Attested}),
                {ok, maps:with(
                    [<<"attestations">>] ++ Attested,
                    Msg
                )}
            catch _:_ ->
                {error, {could_not_normalize, Msg}}
            end;
        false -> {ok, Msg}
    end;
with_only_attested(Msg) ->
    % If the message is not a map, it cannot be signed.
    {ok, Msg}.

%% @doc Sign a message with the given wallet.
attest(Msg, WalletOrOpts) ->
    attest(Msg, WalletOrOpts, <<"httpsig@1.0">>).
attest(Msg, Opts, Format) when is_map(Opts) ->
    attest(Msg, hb_opts:get(priv_wallet, no_viable_wallet, Opts), Format);
attest(Msg, Wallet, Format) ->
    {ok, Signed} =
        dev_message:attest(
            Msg,
            #{ <<"attestation-device">> => Format },
            #{ priv_wallet => Wallet }
        ),
    Signed.

%% @doc Return the list of attested keys from a message.
attested(Msg) -> attested(Msg, #{ <<"attestors">> => <<"all">> }, #{}).
attested(Msg, Opts) -> attested(Msg, Opts, #{}).
attested(Msg, Opts, Format) ->
    {ok, AttestedKeys} = dev_message:attested(Msg, Opts, Format),
    AttestedKeys.

%% @doc wrapper function to verify a message.
verify(Msg) -> verify(Msg, <<"all">>).
verify(Msg, signers) ->
    verify(Msg, hb_message:signers(Msg));
verify(Msg, Attestors) ->
    {ok, Res} = dev_message:verify(Msg, #{ <<"attestors">> => Attestors }, #{}),
    Res.

%% @doc Return the unsigned version of a message in Converge format.
unattested(Bin) when is_binary(Bin) -> Bin;
unattested(Msg) ->
    maps:remove(<<"attestations">>, Msg).

%% @doc Return all of the attestors on a message that have 'normal', 256 bit, 
%% addresses.
signers(Msg) ->
    lists:filter(fun(Signer) -> ?IS_ID(Signer) end,
        hb_converge:get(<<"attestors">>, Msg, #{})).

%% @doc Get a codec from the options.
get_codec(TargetFormat, Opts) ->
    try
        hb_converge:message_to_device(
            #{ <<"device">> => TargetFormat },
            Opts
        )
    catch _:_ ->
        throw({message_codec_not_viable, TargetFormat})
    end.

%% @doc Pretty-print a message.
print(Msg) -> print(Msg, 0).
print(Msg, Indent) ->
    io:format(standard_error, "~s", [lists:flatten(format(Msg, Indent))]).

%% @doc Format a message for printing, optionally taking an indentation level
%% to start from.
format(Item) -> format(Item, 0).
format(Bin, Indent) when is_binary(Bin) ->
    hb_util:format_indented(
        hb_util:format_binary(Bin),
        Indent
    );
format(List, Indent) when is_list(List) ->
    format(lists:map(fun hb_converge:normalize_key/1, List), Indent);
format(Map, Indent) when is_map(Map) ->
    % Define helper functions for formatting elements of the map.
    ValOrUndef =
        fun(<<"hashpath">>) ->
            case Map of
                #{ <<"priv">> := #{ <<"hashpath">> := HashPath } } ->
                    hb_util:short_id(HashPath);
                _ ->
                    undefined
            end;
        (Key) ->
            case dev_message:get(Key, Map) of
                {ok, Val} ->
                    case hb_util:short_id(Val) of
                        undefined -> Val;
                        ShortID -> ShortID
                    end;
                {error, _} -> undefined
            end
        end,
    FilterUndef =
        fun(List) ->
            lists:filter(fun({_, undefined}) -> false; (_) -> true end, List)
        end,
    % Prepare the metadata row for formatting.
    % Note: We try to get the IDs _if_ they are *already* in the map. We do not
    % force calculation of the IDs here because that may cause significant
    % overhead unless the `debug_ids` option is set.
    IDMetadata =
        case hb_opts:get(debug_ids, false, #{}) of
            false ->
                [
                    {<<"#P">>, ValOrUndef(<<"hashpath">>)},
                    {<<"*U">>, ValOrUndef(<<"unsigned_id">>)},
                    {<<"*S">>, ValOrUndef(<<"id">>)}
                ];
            true ->
                {ok, UID} = dev_message:id(Map, #{ <<"attestors">> => <<"none">> }, #{}),
                {ok, ID} =
                    dev_message:id(Map, #{ <<"attestors">> => <<"all">> }, #{}),
                [
                    {<<"#P">>, hb_util:short_id(ValOrUndef(<<"hashpath">>))},
                    {<<"*U">>, hb_util:short_id(UID)}
                ] ++
                case ID of
                    UID -> [];
                    _ -> [{<<"*S">>, hb_util:short_id(ID)}]
                end
        end,
    AttestorMetadata =
        case dev_message:attestors(Map, #{}, #{}) of
            {ok, []} -> [];
            {ok, [Attestor]} ->
                [{<<"Att.">>, hb_util:short_id(Attestor)}];
            {ok, Attestors} ->
                [
                    {
                        <<"Atts.">>,
                        string:join(
                            lists:map(fun(X) -> [hb_util:short_id(X)] end, Attestors),
                            ", "
                        )
                    }
                ]
        end,
    % Concatenate the present metadata rows.
    Metadata = FilterUndef(lists:flatten([IDMetadata, AttestorMetadata])),
    % Format the metadata row.
    Header =
        hb_util:format_indented("Message [~s] {",
            [
                string:join(
                    [
                        io_lib:format("~s: ~s", [Lbl, Val])
                        || {Lbl, Val} <- Metadata
                    ],
                    ", "
                )
            ],
            Indent
        ),
    % Put the path and device rows into the output at the _top_ of the map.
    PriorityKeys = [{<<"path">>, ValOrUndef(<<"path">>)}, {<<"device">>, ValOrUndef(<<"device">>)}],
    FooterKeys =
        case {hb_opts:get(debug_hide_priv, false, #{}), maps:get(<<"priv">>, Map, #{})} of
            {true, _} -> [];
            {false, #{}} -> [];
            {false, Priv} -> [{<<"!Private!">>, Priv}]
        end,
    % Concatenate the path and device rows with the rest of the key values.
    KeyVals =
        FilterUndef(PriorityKeys) ++
        maps:to_list(
            minimize(Map,
                case hb_opts:get(debug_hide_metadata, false, #{}) of
                    true ->
                        [
                            <<"attestations">>,
                            <<"path">>,
                            <<"device">>
                        ];
                    false -> [
                        <<"path">>,
                        <<"device">>
                    ]
                end
            )
        ) ++ FooterKeys,
    % Format the remaining 'normal' keys and values.
    Res = lists:map(
        fun({Key, Val}) ->
            NormKey = hb_converge:normalize_key(Key, #{ error_strategy => ignore }),
            KeyStr = 
                case NormKey of
                    undefined ->
                        io_lib:format("~p [!!! INVALID KEY !!!]", [Key]);
                    _ ->
                        hb_converge:normalize_key(Key)
                end,
            hb_util:format_indented(
                "~s => ~s~n",
                [
                    lists:flatten([KeyStr]),
                    case Val of
                        NextMap when is_map(NextMap) ->
                            hb_util:format_map(NextMap, Indent + 2);
                        _ when (byte_size(Val) == 32) or (byte_size(Val) == 43) ->
                            Short = hb_util:short_id(Val),
                            io_lib:format("~s [*]", [Short]);
                        _ when byte_size(Val) == 87 ->
                            io_lib:format("~s [#p]", [hb_util:short_id(Val)]);
                        Bin when is_binary(Bin) ->
                            hb_util:format_binary(Bin);
                        Other ->
                            io_lib:format("~p", [Other])
                    end
                ],
                Indent + 1
            )
        end,
        KeyVals
    ),
    case Res of
        [] -> lists:flatten(Header ++ " [Empty] }");
        _ ->
            lists:flatten(
                Header ++ ["\n"] ++ Res ++ hb_util:format_indented("}", Indent)
            )
    end;
format(Item, Indent) ->
    % Whatever we have is not a message map.
    hb_util:format_indented("[UNEXPECTED VALUE] ~p", [Item], Indent).

%% @doc Return the type of an encoded message.
type(TX) when is_record(TX, tx) -> tx;
type(Binary) when is_binary(Binary) -> binary;
type(Msg) when is_map(Msg) ->
    IsDeep = lists:any(
        fun({_, Value}) -> is_map(Value) end,
        lists:filter(
            fun({Key, _}) -> not hb_private:is_private(Key) end,
            maps:to_list(Msg)
        )
    ),
    case IsDeep of
        true -> deep;
        false -> shallow
    end.

%% @doc Check if two maps match, including recursively checking nested maps.
%% Takes an optional mode argument to control the matching behavior:
%%      `strict': All keys in both maps be present and match.
%%      `only_present': Only present keys in both maps must match.
%%      `primary': Only the primary map's keys must be present.
match(Map1, Map2) ->
    match(Map1, Map2, strict).
match(Map1, Map2, Mode) ->
     Keys1 =
        maps:keys(
            NormMap1 = minimize(
                normalize(hb_converge:normalize_keys(Map1)),
                [<<"content-type">>, <<"body-keys">>, <<"inline-body-key">>]
            )
        ),
    Keys2 =
        maps:keys(
            NormMap2 = minimize(
                normalize(hb_converge:normalize_keys(Map2)),
                [<<"content-type">>, <<"body-keys">>, <<"inline-body-key">>]
            )
        ),
    PrimaryKeysPresent =
        (Mode == primary) andalso
            lists:all(
                fun(Key) -> lists:member(Key, Keys1) end,
                Keys1
            ),
    case (Keys1 == Keys2) or (Mode == only_present) or PrimaryKeysPresent of
        true ->
            lists:all(
                fun(Key) ->
                    Val1 = maps:get(Key, NormMap1, not_found),
                    Val2 = maps:get(Key, NormMap2, not_found),
                    BothPresent = (Val1 =/= not_found) and (Val2 =/= not_found),
                    case (not BothPresent) and (Mode == only_present) of
                        true -> true;
                        false ->
                            case is_map(Val1) andalso is_map(Val2) of
                                true -> match(Val1, Val2);
                                false ->
                                    case Val1 == Val2 of
                                        true -> true;
                                        false ->
                                            ?event(match,
                                                {value_mismatch,
                                                    {key, Key},
                                                    {val1, Val1},
                                                    {val2, Val2}
                                                }
                                            ),
                                            false
                                    end
                            end
                    end
                end,
                Keys1
            );
        false ->
            ?event(match, {keys_mismatch, {keys1, Keys1}, {keys2, Keys2}}),
            false
    end.
	
matchable_keys(Map) ->
    lists:sort(lists:map(fun hb_converge:normalize_key/1, maps:keys(Map))).

%% @doc Implements a standard pattern in which the target for an operation is
%% found by looking for a `target' key in the request. If the target is `self',
%% or not present, the operation is performed on the original message. Otherwise,
%% the target is expected to be a key in the message, and the operation is
%% performed on the value of that key.
find_target(Self, Req, Opts) ->
	GetOpts = Opts#{ hashpath => ignore },
    {ok,
        case hb_converge:get(<<"target">>, Req, <<"self">>, GetOpts) of
            <<"self">> -> Self;
            Key ->
                hb_converge:get(
                    Key,
                    Req,
                    hb_converge:get(<<"body">>, Req, GetOpts),
                    GetOpts
                )
        end
    }.

%% @doc Remove keys from the map that can be regenerated. Optionally takes an
%% additional list of keys to include in the minimization.
minimize(Msg) -> minimize(Msg, []).
minimize(RawVal, _) when not is_map(RawVal) -> RawVal;
minimize(Map, ExtraKeys) ->
    NormKeys =
        lists:map(fun hb_converge:normalize_key/1, ?REGEN_KEYS)
            ++ lists:map(fun hb_converge:normalize_key/1, ExtraKeys),
    maps:filter(
        fun(Key, _) ->
            (not lists:member(hb_converge:normalize_key(Key), NormKeys))
                andalso (not hb_private:is_private(Key))
        end,
        maps:map(fun(_K, V) -> minimize(V) end, Map)
    ).

%% @doc Return a map with only the keys that necessary, without those that can
%% be regenerated.
normalize(Map) ->
    NormalizedMap = hb_converge:normalize_keys(Map),
    FilteredMap = filter_default_keys(NormalizedMap),
    maps:with(matchable_keys(FilteredMap), FilteredMap).

%% @doc Remove keys from a map that have the default values found in the tx
%% record.
filter_default_keys(Map) ->
    DefaultsMap = default_tx_message(),
    maps:filter(
        fun(Key, Value) ->
            case maps:find(hb_converge:normalize_key(Key), DefaultsMap) of
                {ok, Value} -> false;
                _ -> true
            end
        end,
        Map
    ).

%% @doc Get the normalized fields and default values of the tx record.
default_tx_message() ->
    maps:from_list(default_tx_list()).

%% @doc Get the ordered list of fields as Converge keys and default values of
%% the tx record.
default_tx_list() ->
    Keys = lists:map(fun hb_converge:normalize_key/1, record_info(fields, tx)),
    lists:zip(Keys, tl(tuple_to_list(#tx{}))).

%%% Tests

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
    FilteredMap = filter_default_keys(TXMap),
    ?assertEqual(<< 1:256 >>, maps:get(<<"unsigned_id">>, FilteredMap)),
    ?assertEqual(<< 2:256 >>, maps:get(<<"last_tx">>, FilteredMap, not_found)),
    ?assertEqual(not_found, maps:get(<<"owner">>, FilteredMap, not_found)),
    ?assertEqual(not_found, maps:get(<<"target">>, FilteredMap, not_found)).

minimization_test() ->
    Msg = #{
        <<"unsigned_id">> => << 1:256 >>,
        <<"id">> => << 2:256 >>
    },
    MinimizedMsg = minimize(Msg),
    ?event({minimized, MinimizedMsg}),
    ?assertEqual(1, maps:size(MinimizedMsg)).

match_modes_test() ->
    Msg1 = #{ <<"a">> => 1, <<"b">> => 2 },
    Msg2 = #{ <<"a">> => 1 },
    Msg3 = #{ <<"a">> => 1, <<"b">> => 2, <<"c">> => 3 },
    ?assert(match(Msg1, Msg2, only_present)),
    ?assert(not match(Msg2, Msg1, strict)),
    ?assert(match(Msg1, Msg3, primary)),
    ?assert(not match(Msg3, Msg1, primary)).

basic_map_codec_test(Codec) ->
    Msg = #{ <<"normal_key">> => <<"NORMAL_VALUE">> },
    Encoded = convert(Msg, Codec, <<"structured@1.0">>, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assert(hb_message:match(Msg, Decoded)).

set_body_codec_test(Codec) ->
    Msg = #{ <<"body">> => <<"NORMAL_VALUE">>, <<"test-key">> => <<"Test-Value">> },
    Encoded = convert(Msg, Codec, <<"structured@1.0">>, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(hb_message:match(Msg, Decoded)).

%% @doc Test that we can convert a message into a tx record and back.
single_layer_message_to_encoding_test(Codec) ->
    Msg = #{
        <<"last_tx">> => << 2:256 >>,
        <<"owner">> => << 3:4096 >>,
        <<"target">> => << 4:256 >>,
        <<"data">> => <<"DATA">>,
        <<"special-key">> => <<"SPECIAL_VALUE">>
    },
    Encoded = convert(Msg, Codec, <<"structured@1.0">>, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(hb_message:match(Msg, Decoded)).

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
%     ?assertEqual(maps:get(<<"last_tx">>, Msg), TX#tx.last_tx),
%     ?assertEqual(maps:get(<<"owner">>, Msg), TX#tx.owner),
%     ?assertEqual(maps:get(<<"target">>, Msg), TX#tx.target).

%% @doc Test that the message matching function works.
match_test(Codec) ->
    Msg = #{ <<"a">> => 1, <<"b">> => 2 },
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(match(Msg, Decoded)).

binary_to_binary_test(Codec) ->
    % Serialization must be able to turn a raw binary into a TX, then turn
    % that TX back into a binary and have the result match the original.
    Bin = <<"THIS IS A BINARY, NOT A NORMAL MESSAGE">>,
    Encoded = convert(Bin, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assertEqual(Bin, Decoded).

%% @doc Structured field parsing tests.
structured_field_atom_parsing_test(Codec) ->
    Msg = #{ highly_unusual_http_header => highly_unusual_value },
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(match(Msg, Decoded)).

structured_field_decimal_parsing_test(Codec) ->
    Msg = #{ integer_field => 1234567890 },
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(match(Msg, Decoded)).

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
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(match(Msg, Decoded)).

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
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(match(Msg, Decoded)).

simple_nested_message_test(Codec) ->
    Msg = #{
        <<"a">> => <<"1">>,
        <<"nested">> => #{ <<"b">> => <<"1">> },
        <<"c">> => <<"3">>
    },
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({matching, {input, Msg}, {output, Decoded}}),
    ?assert(
        match(
            Msg,
            Decoded
        )
    ).

%% @doc Test that the data field is correctly managed when we have multiple
%% uses for it (the 'data' key itself, as well as keys that cannot fit in
%% tags).
nested_message_with_large_content_test(Codec) ->
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
    Encoded = convert(Msg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assert(match(Msg, Decoded)).

%% @doc Test that we can convert a 3 layer nested message into a tx record and back.
deeply_nested_message_with_content_test(Codec) ->
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
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(match(Msg, Decoded)).

deeply_nested_message_with_only_content(Codec) ->
    MainBodyKey =
        case Codec of
            <<"ans104@1.0">> -> <<"data">>;
            _ -> <<"body">>
        end,
    Msg = #{
         <<"depth1">> => <<"outer">>,
        MainBodyKey => #{
            <<"depth2">> => <<"middle">>,
            MainBodyKey => #{
                MainBodyKey => <<"DATA">>    
            }
        }
    },
    Encoded = convert(Msg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assert(match(Msg, Decoded)).

nested_structured_fields_test(Codec) ->
    NestedMsg = #{ <<"a">> => #{ <<"b">> => 1 } },
    Encoded = convert(NestedMsg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(match(NestedMsg, Decoded)).

nested_message_with_large_keys_test(Codec) ->
    Msg = #{
        <<"a">> => <<"1">>,
        <<"long_data">> => << 0:((1 + 1024) * 8) >>,
        <<"nested">> => #{ <<"b">> => <<"1">> },
        <<"c">> => <<"3">>
    },
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(match(Msg, Decoded)).

signed_message_encode_decode_verify_test(Codec) ->
    Msg = #{
        <<"test_data">> => <<"TEST_DATA">>,
        <<"test_key">> => <<"TEST_VALUE">>
    },
    {ok, SignedMsg} =
        dev_message:attest(
            Msg,
            #{ <<"attestation-device">> => Codec },
            #{ priv_wallet => hb:wallet() }
        ),
    ?event({signed_msg, {explicit, SignedMsg}}),
    ?assertEqual(true, verify(SignedMsg)),
    ?event(test, {verified, {explicit, SignedMsg}}),
    Encoded = convert(SignedMsg, Codec, #{}),
    ?event(test, {msg_encoded_as_codec, {explicit, Encoded}}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, {explicit, Decoded}}),
    ?assertEqual(true, verify(Decoded)),
    ?assert(match(SignedMsg, Decoded)).

complex_signed_message_test(Codec) ->
    Msg = #{
        <<"data">> => <<"TEST_DATA">>,
        <<"deep_data">> => #{
            <<"data">> => <<"DEEP_DATA">>,
            <<"complex_key">> => 1337,
            <<"list">> => [1,2,3]
        }
    },
    {ok, SignedMsg} =
        dev_message:attest(
            Msg,
            #{ <<"attestation-device">> => Codec },
            #{ priv_wallet => hb:wallet() }
        ),
    Encoded = convert(SignedMsg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assertEqual(true, verify(Decoded)),
    ?assert(match(SignedMsg, Decoded)).

% multisignature_test(Codec) ->
%     Wallet1 = ar_wallet:new(),
%     Wallet2 = ar_wallet:new(),
%     Msg = #{
%         <<"data">> => <<"TEST_DATA">>,
%         <<"test_key">> => <<"TEST_VALUE">>
%     },
%     {ok, SignedMsg} =
%         dev_message:attest(
%             Msg,
%             #{ <<"attestation-device">> => Codec },
%             #{ priv_wallet => Wallet1 }
%         ),
%     ?event({signed_msg, SignedMsg}),
%     {ok, MsgSignedTwice} =
%         dev_message:attest(
%             SignedMsg,
%             #{ <<"attestation-device">> => Codec },
%             #{ priv_wallet => Wallet2 }
%         ),
%     ?event({signed_msg_twice, MsgSignedTwice}),
%     ?assert(verify(MsgSignedTwice)),
%     {ok, Attestors} = dev_message:attestors(MsgSignedTwice),
%     ?event({attestors, Attestors}),
%     ?assert(lists:member(hb_util:human_id(ar_wallet:to_address(Wallet1)), Attestors)),
%     ?assert(lists:member(hb_util:human_id(ar_wallet:to_address(Wallet2)), Attestors)).

deep_multisignature_test() ->
    % Only the `httpsig@1.0` codec supports multisignatures.
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
        dev_message:attest(
            Msg,
            #{ <<"attestation-device">> => Codec },
            #{ priv_wallet => Wallet1 }
        ),
    ?event({signed_msg, SignedMsg}),
    {ok, MsgSignedTwice} =
        dev_message:attest(
            SignedMsg,
            #{ <<"attestation-device">> => Codec },
            #{ priv_wallet => Wallet2 }
        ),
    ?event({signed_msg_twice, MsgSignedTwice}),
    ?assert(verify(MsgSignedTwice)),
    {ok, Attestors} = dev_message:attestors(MsgSignedTwice),
    ?event({attestors, Attestors}),
    ?assert(lists:member(hb_util:human_id(ar_wallet:to_address(Wallet1)), Attestors)),
    ?assert(lists:member(hb_util:human_id(ar_wallet:to_address(Wallet2)), Attestors)).

tabm_converge_ids_equal_test(Codec) ->
    Msg = #{
        <<"data">> => <<"TEST_DATA">>,
        <<"deep_data">> => #{
            <<"data">> => <<"DEEP_DATA">>,
            <<"complex_key">> => 1337,
            <<"list">> => [1,2,3]
        }
    },
    Encoded = convert(Msg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assertEqual(
        dev_message:id(Msg, #{ <<"attestors">> => <<"none">>}, #{}),
        dev_message:id(Decoded, #{ <<"attestors">> => <<"none">>}, #{})
    ).

signed_deep_message_test(Codec) ->
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
    EncDec = convert(convert(Msg, Codec, #{}), <<"structured@1.0">>, Codec, #{}),
    ?event({enc_dec, EncDec}),
    {ok, SignedMsg} =
        dev_message:attest(
            EncDec,
            #{ <<"attestation-device">> => Codec },
            #{ priv_wallet => hb:wallet() }
        ),
    ?event({signed_msg, SignedMsg}),
    {ok, Res} = dev_message:verify(SignedMsg, #{ <<"attestors">> => <<"all">>}, #{}),
    ?event({verify_res, Res}),
    ?assertEqual(true, verify(SignedMsg)),
    ?event({verified, SignedMsg}),
    Encoded = convert(SignedMsg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    {ok, DecodedRes} = dev_message:verify(Decoded, #{ <<"attestors">> => <<"all">>}, #{}),
    ?event({verify_decoded_res, DecodedRes}),
    ?assert(
        match(
            SignedMsg,
            Decoded
        )
    ).

unsigned_id_test(Codec) ->
    Msg = #{ <<"data">> => <<"TEST_DATA">> },
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assertEqual(
        dev_message:id(Decoded, #{ <<"attestors">> => <<"none">>}, #{}),
        dev_message:id(Msg, #{ <<"attestors">> => <<"none">>}, #{})
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

message_with_simple_list_test(Codec) ->
    Msg = #{ <<"a">> => [<<"1">>, <<"2">>, <<"3">>] },
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(match(Msg, Decoded)).

empty_string_in_tag_test(Codec) ->
    Msg =
        #{
            dev =>
                #{
                    <<"stderr">> => <<"">>,
                    <<"stdin">> => <<"b">>,
                    <<"stdout">> => <<"c">>
                }
        },
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assert(match(Msg, Decoded)).

hashpath_sign_verify_test(Codec) ->
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
    {ok, SignedMsg} =
        dev_message:attest(
            Msg,
            #{ <<"attestation-device">> => Codec },
            #{ priv_wallet => hb:wallet() }
        ),
    ?event({signed_msg, {explicit, SignedMsg}}),
    {ok, Res} = dev_message:verify(SignedMsg, #{ <<"attestors">> => [<<"hmac-sha256">>]}, #{}),
    ?event({verify_hmac_res, {explicit, Res}}),
    ?assertEqual(true, verify(SignedMsg)),
    ?event({verified, {explicit, SignedMsg}}),
    Encoded = convert(SignedMsg, Codec, #{}),
    ?event(hmac, {encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event(hmac, {decoded, Decoded}),
    ?assert(verify(Decoded)),
    ?assert(
        match(
            SignedMsg,
            Decoded
        )
    ).

signed_message_with_derived_components_test(Codec) ->
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
        dev_message:attest(
            Msg,
            #{ <<"attestation-device">> => Codec },
            #{ priv_wallet => hb:wallet() }
        ),
    ?event({signed_msg, SignedMsg}),
    ?assert(verify(SignedMsg)),
    Encoded = convert(SignedMsg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assert(verify(Decoded)),
    ?assert(match(SignedMsg, Decoded)).

attested_keys_test(Codec) ->
    Msg = #{ <<"a">> => 1, <<"b">> => 2, <<"c">> => 3 },
    Signed = attest(Msg, hb:wallet(), Codec),
    AttestedKeys = attested(Signed),
    ?assert(lists:member(<<"a">>, AttestedKeys)),
    ?assert(lists:member(<<"b">>, AttestedKeys)),
    ?assert(lists:member(<<"c">>, AttestedKeys)),
    MsgToFilter = Signed#{ <<"bad-key">> => <<"BAD VALUE">> },
    ?assert(not lists:member(<<"bad-key">>, attested(MsgToFilter))).

deeply_nested_attested_keys_test() ->
    Msg = #{
        <<"a">> => 1,
        <<"b">> => #{ <<"c">> => #{ <<"d">> => <<0:((1 + 1024) * 1024)>> } },
        <<"e">> => <<0:((1 + 1024) * 1024)>>
    },
    Signed = attest(Msg, hb:wallet()),
    {ok, WithOnlyAttested} = with_only_attested(Signed),
    ?event({with_only_attested, WithOnlyAttested}),
    ?assert(
        match(
            Msg,
            maps:without([<<"attestations">>], WithOnlyAttested)
        )
    ).

signed_with_inner_signed_message_test(Codec) ->
    Wallet = hb:wallet(),
    Msg = attest(#{
        <<"a">> => 1,
        <<"b">> =>
            maps:merge(
                attest(
                    #{
                        <<"c">> => #{ <<"d">> => << 0:(1024*1024) >> },
                        <<"e">> => 5
                    },
                    Wallet,
                    Codec
                ),
                #{
                    <<"f">> => 6,
                    <<"g">> => 7
                }
            )
    }, Wallet, Codec),
    ?event({msg, Msg}),
    ?assert(verify(Msg)),
    {ok, OnlyAttested} = with_only_attested(Msg),
    ?event({only_attested, OnlyAttested}),
    ?assert(verify(OnlyAttested)).

large_body_attested_keys_test(Codec) ->
    case Codec of
        <<"ans104@1.0">> ->
            skip;
        _ ->
            Msg = #{ <<"a">> => 1, <<"b">> => 2, <<"c">> => #{ <<"d">> => << 1:((1 + 1024) * 1024) >> } },
            Encoded = convert(Msg, <<"httpsig@1.0">>, #{}),
            ?event({encoded, Encoded}),
            Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
            ?event({decoded, Decoded}),
            Signed = attest(Decoded, hb:wallet(), Codec),
            ?event({signed, Signed}),
            AttestedKeys = attested(Signed),
            ?assert(lists:member(<<"a">>, AttestedKeys)),
            ?assert(lists:member(<<"b">>, AttestedKeys)),
            ?assert(lists:member(<<"c">>, AttestedKeys)),
            MsgToFilter = Signed#{ <<"bad-key">> => <<"BAD VALUE">> },
            ?assert(not lists:member(<<"bad-key">>, attested(MsgToFilter)))
    end.

%%% Test helpers

test_codecs() ->
    [<<"structured@1.0">>, <<"httpsig@1.0">>, <<"flat@1.0">>, <<"ans104@1.0">>].

generate_test_suite(Suite) ->
    lists:map(
        fun(CodecName) ->
            {foreach,
                fun() -> ok end,
                fun(_) -> ok end,
                [
                    {
                        << CodecName/binary, ": ", (list_to_binary(Desc))/binary >>,
                        fun() -> Test(CodecName) end
                    }
                ||
                    {Desc, Test} <- Suite
                ]
            }
        end,
        test_codecs()
    ).

message_suite_test_() ->
    generate_test_suite([
        {"basic map codec test", fun basic_map_codec_test/1},
        {"set body codec test", fun set_body_codec_test/1},
        {"match test", fun match_test/1},
        {"single layer message to encoding test",
            fun single_layer_message_to_encoding_test/1},
        {"tabm converge ids equal test", fun tabm_converge_ids_equal_test/1},
        {"message with large keys test", fun message_with_large_keys_test/1},
        {"nested message with large keys and content test",
            fun nested_message_with_large_keys_and_content_test/1},
        {"simple nested message test", fun simple_nested_message_test/1},
        {"nested message with large content test",
            fun nested_message_with_large_content_test/1},
        {"deeply nested message with content test",
            fun deeply_nested_message_with_content_test/1},
        {"deeply nested message with only content test",
            fun deeply_nested_message_with_only_content/1},
        {"structured field atom parsing test",
            fun structured_field_atom_parsing_test/1},
        {"structured field decimal parsing test",
            fun structured_field_decimal_parsing_test/1},
        {"binary to binary test", fun binary_to_binary_test/1},
        {"nested structured fields test", fun nested_structured_fields_test/1},
        {"nested message with large keys test",
            fun nested_message_with_large_keys_test/1},
        {"message with simple list test", fun message_with_simple_list_test/1},
        {"empty string in tag test", fun empty_string_in_tag_test/1},
        {"signed item to message and back test",
            fun signed_message_encode_decode_verify_test/1},
        {"signed deep serialize and deserialize test",
            fun signed_deep_message_test/1},
        {"unsigned id test", fun unsigned_id_test/1},
        {"complex signed message test", fun complex_signed_message_test/1},
        {"signed message with hashpath test", fun hashpath_sign_verify_test/1},
        {"message with derived components test", fun signed_message_with_derived_components_test/1},
        {"attested keys test", fun attested_keys_test/1},
        {"large body attested keys test", fun large_body_attested_keys_test/1}
    ]).