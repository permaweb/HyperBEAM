
%%% @doc This module acts an adapter between messages, as modeled in the
%%% AO-Core protocol, and their uderlying binary representations and formats.
%%% 
%%% Unless you are implementing a new message serialization codec, you should
%%% not need to interact with this module directly. Instead, use the
%%% `hb_ao' interfaces to interact with all messages. The `dev_message'
%%% module implements a device interface for abstracting over the different
%%% message formats.
%%% 
%%% `hb_message' and the HyperBEAM caches can interact with multiple different
%%% types of message formats:
%%% 
%%%     - Richly typed AO-Core structured messages.
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
%%% <pre>
%%%     Arweave TX/ANS-104 ==> dev_codec_ans104:from/1 ==> TABM
%%%     HTTP Signed Message ==> dev_codec_httpsig_conv:from/1 ==> TABM
%%%     Flat Maps ==> dev_codec_flat:from/1 ==> TABM
%%% 
%%%     TABM ==> dev_codec_structured:to/1 ==> AO-Core Message
%%%     AO-Core Message ==> dev_codec_structured:from/1 ==> TABM
%%% 
%%%     TABM ==> dev_codec_ans104:to/1 ==> Arweave TX/ANS-104
%%%     TABM ==> dev_codec_httpsig_conv:to/1 ==> HTTP Signed Message
%%%     TABM ==> dev_codec_flat:to/1 ==> Flat Maps
%%%     ...
%%% </pre>
%%% 
%%% Additionally, this module provides a number of utility functions for
%%% manipulating messages. For example, `hb_message:sign/2' to sign a message of
%%% arbitrary type, or `hb_message:format/1' to print an AO-Core/TABM message in
%%% a human-readable format.
%%% 
%%% The `hb_cache' module is responsible for storing and retrieving messages in
%%% the HyperBEAM stores configured on the node. Each store has its own storage
%%% backend, but each works with simple key-value pairs. Subsequently, the 
%%% `hb_cache' module uses TABMs as the internal format for storing and 
%%% retrieving messages.
-module(hb_message).
-export([id/1, id/2, id/3]).
-export([convert/3, convert/4, uncommitted/1, with_only_committers/2]).
-export([verify/1, verify/2, commit/2, commit/3, signers/1, type/1, minimize/1]).
-export([committed/1, committed/2, committed/3]).
-export([commitment/2, commitment/3]).
-export([with_only_committed/1, with_only_committed/2]).
-export([with_commitments/2, without_commitments/2]).
-export([match/2, match/3, find_target/3]).
%%% Helpers:
-export([default_tx_list/0, filter_default_keys/1]).
%%% Debugging tools:
-export([print/1, format/1, format/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Convert a message from one format to another. Taking a message in the
%% source format, a target format, and a set of opts. If not given, the source
%% is assumed to be `structured@1.0'. Additional codecs can be added by ensuring they
%% are part of the `Opts' map -- either globally, or locally for a computation.
%% 
%% The encoding happens in two phases:
%% 1. Convert the message to a TABM.
%% 2. Convert the TABM to the target format.
%% 
%% The conversion to a TABM is done by the `structured@1.0' codec, which is always
%% available. The conversion from a TABM is done by the target codec.
convert(Msg, TargetFormat, Opts) ->
    convert(Msg, TargetFormat, <<"structured@1.0">>, Opts).
convert(Msg, TargetFormat, SourceFormat, Opts) ->
    OldPriv =
        if is_map(Msg) -> maps:get(<<"priv">>, Msg, #{});
           true -> #{}
        end,
    TABM =
        to_tabm(
            case is_map(Msg) of
                true -> maps:without([<<"priv">>], Msg);
                false -> Msg
            end,
            SourceFormat,
            Opts
        ),
    case TargetFormat of
        tabm -> restore_priv(TABM, OldPriv);
        _ -> from_tabm(TABM, TargetFormat, OldPriv, Opts)
    end.

to_tabm(Msg, SourceFormat, Opts) ->
    SourceCodecMod = get_codec(SourceFormat, Opts),
    case SourceCodecMod:from(Msg) of
        TypicalMsg when is_map(TypicalMsg) ->
            TypicalMsg;
        OtherTypeRes -> OtherTypeRes
    end.

from_tabm(Msg, TargetFormat, OldPriv, Opts) ->
    TargetCodecMod = get_codec(TargetFormat, Opts),
    case TargetCodecMod:to(Msg) of
        TypicalMsg when is_map(TypicalMsg) ->
            restore_priv(TypicalMsg, OldPriv);
        OtherTypeRes -> OtherTypeRes
    end.

%% @doc Add the existing `priv' sub-map back to a converted message, honoring
%% any existing `priv' sub-map that may already be present.
restore_priv(Msg, EmptyPriv) when map_size(EmptyPriv) == 0 -> Msg;
restore_priv(Msg, OldPriv) ->
    MsgPriv = maps:get(<<"priv">>, Msg, #{}),
    ?event({restoring_priv, {msg_priv, MsgPriv}, {old_priv, OldPriv}}),
    NewPriv = hb_util:deep_merge(MsgPriv, OldPriv),
    ?event({new_priv, NewPriv}),
    Msg#{ <<"priv">> => NewPriv }.

%% @doc Return the ID of a message.
id(Msg) -> id(Msg, uncommitted).
id(Msg, Committers) -> id(Msg, Committers, #{}).
id(Msg, RawCommitters, Opts) ->
    Committers =
        case RawCommitters of
            uncommitted -> <<"none">>;
            unsigned -> <<"none">>;
            none -> [];
            all -> <<"all">>;
            signed -> signers(Msg);
            List -> List
        end,
    ?event({getting_id, {msg, Msg}, {committers, Committers}}),
    {ok, ID} =
        hb_ao:resolve(
            Msg,
            #{ <<"path">> => <<"id">>, <<"committers">> => Committers },
            Opts
        ),
    hb_util:human_id(ID).

%% @doc Return a message with only the committed keys. If no commitments are
%% present, the message is returned unchanged. This means that you need to
%% check if the message is:
%% - Committed
%% - Verifies
%% ...before using the output of this function as the 'canonical' message. This
%% is such that expensive operations like signature verification are not
%% performed unless necessary.
with_only_committed(Msg) ->
    with_only_committed(Msg, #{}).
with_only_committed(Msg, Opts) when is_map(Msg) ->
    Comms = maps:get(<<"commitments">>, Msg, not_found),
    case is_map(Msg) andalso Comms /= not_found of
        true ->
            try
                CommittedKeys =
                    hb_message:committed(
                        Msg,
                        #{ <<"commitments">> => <<"all">> },
                        Opts
                    ),
                % Add the inline-body-key to the committed list if it is not
                % already present.
                ?event({committed_keys, CommittedKeys, {msg, Msg}}),
                {ok, maps:with(
                    CommittedKeys ++ [<<"commitments">>],
                    Msg
                )}
            catch _:_:St ->
                {error, {could_not_normalize, Msg, St}}
            end;
        false -> {ok, Msg}
    end;
with_only_committed(Msg, _) ->
    % If the message is not a map, it cannot be signed.
    {ok, Msg}.

%% @doc Return the message with only the specified committers attached.
with_only_committers(Msg, Committers) when is_map(Msg) ->
    NewCommitments =
        maps:filter(
            fun(_, #{ <<"committer">> := Committer }) ->
                lists:member(Committer, Committers);
               (_, _) -> false
            end,
            maps:get(<<"commitments">>, Msg, #{})
        ),
    Msg#{ <<"commitments">> => NewCommitments };
with_only_committers(Msg, _Committers) ->
    throw({unsupported_message_type, Msg}).

%% @doc Sign a message with the given wallet.
commit(Msg, WalletOrOpts) ->
    commit(
        Msg,
        WalletOrOpts,
        hb_opts:get(
            commitment_device,
            no_viable_commitment_device,
            case is_map(WalletOrOpts) of
                true -> WalletOrOpts;
                false -> #{ priv_wallet => WalletOrOpts }
            end
        )
    ).
commit(Msg, Wallet, Format) when not is_map(Wallet) ->
    commit(Msg, #{ priv_wallet => Wallet }, Format);
commit(Msg, Opts, Format) ->
    {ok, Signed} =
        dev_message:commit(
            Msg,
            #{ <<"commitment-device">> => Format },
            Opts
        ),
    Signed.

%% @doc Return the list of committed keys from a message.
committed(Msg) -> committed(Msg, all).
committed(Msg, Committers) -> committed(Msg, Committers, #{}).
committed(Msg, all, Opts) ->
    committed(Msg, #{ <<"commitments">> => <<"all">> }, Opts);
committed(Msg, List, Opts) when is_list(List) ->
    committed(Msg, #{ <<"commitments">> => List }, Opts);
committed(Msg, CommittersMsg, Opts) ->
    {ok, CommittedKeys} = dev_message:committed(Msg, CommittersMsg, Opts),
    CommittedKeys.

%% @doc wrapper function to verify a message.
verify(Msg) -> verify(Msg, <<"all">>).
verify(Msg, signers) -> verify(Msg, hb_message:signers(Msg));
verify(Msg, Committers) ->
    {ok, Res} =
        dev_message:verify(
            Msg,
            #{ <<"committers">> =>
                case ?IS_ID(Committers) of
                    true -> [Committers];
                    false -> Committers
                end
            },
            #{}),
    Res.

%% @doc Return the unsigned version of a message in AO-Core format.
uncommitted(Bin) when is_binary(Bin) -> Bin;
uncommitted(Msg) ->
    maps:remove(<<"commitments">>, Msg).

%% @doc Return all of the committers on a message that have 'normal', 256 bit, 
%% addresses.
signers(Msg) ->
    lists:filter(fun(Signer) -> ?IS_ID(Signer) end,
        hb_ao:get(<<"committers">>, Msg, #{})).

%% @doc Get a codec from the options.
get_codec(TargetFormat, Opts) ->
    try
        hb_ao:message_to_device(
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
    format(lists:map(fun hb_ao:normalize_key/1, List), Indent);
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
    % overhead unless the `debug_ids' option is set.
    IDMetadata =
        case hb_opts:get(debug_ids, false, #{}) of
            false ->
                [
                    {<<"#P">>, ValOrUndef(<<"hashpath">>)},
                    {<<"*U">>, ValOrUndef(<<"unsigned_id">>)},
                    {<<"*S">>, ValOrUndef(<<"id">>)}
                ];
            true ->
                {ok, UID} = dev_message:id(Map, #{}, #{}),
                {ok, ID} =
                    dev_message:id(Map, #{ <<"commitments">> => <<"all">> }, #{}),
                [
                    {<<"#P">>, hb_util:short_id(ValOrUndef(<<"hashpath">>))},
                    {<<"*U">>, hb_util:short_id(UID)}
                ] ++
                case ID of
                    UID -> [];
                    _ -> [{<<"*S">>, hb_util:short_id(ID)}]
                end
        end,
    CommitterMetadata =
        case hb_opts:get(debug_committers, true, #{}) of
            false -> [];
            true ->
                case dev_message:committers(Map) of
                    {ok, []} -> [];
                    {ok, [Committer]} ->
                        [{<<"Comm.">>, hb_util:short_id(Committer)}];
                    {ok, Committers} ->
                        [
                            {
                                <<"Comms.">>,
                                string:join(
                                    lists:map(
                                        fun(X) ->
                                            [hb_util:short_id(X)]
                                        end,
                                        Committers
                                    ),
                                    ", "
                                )
                            }
                        ]
                end
        end,
    % Concatenate the present metadata rows.
    Metadata = FilterUndef(lists:flatten([IDMetadata, CommitterMetadata])),
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
    % Add private keys to the output if they are not hidden. Opt takes 3 forms:
    % 1. `false' -- never show priv
    % 2. `if_present' -- show priv only if there are keys inside
    % 2. `always' -- always show priv
    FooterKeys =
        case {hb_opts:get(debug_show_priv, false, #{}), maps:get(<<"priv">>, Map, #{})} of
            {false, _} -> [];
            {if_present, #{}} -> [];
            {_, Priv} -> [{<<"!Private!">>, Priv}]
        end,
    % Concatenate the path and device rows with the rest of the key values.
    KeyVals =
        FilterUndef(PriorityKeys) ++
        maps:to_list(
            minimize(Map,
                case hb_opts:get(debug_metadata, false, #{}) of
                    false ->
                        [
                            <<"commitments">>,
                            <<"path">>,
                            <<"device">>
                        ];
                    true -> [
                        <<"path">>,
                        <<"device">>
                    ]
                end
            )
        ) ++ FooterKeys,
    % Format the remaining 'normal' keys and values.
    Res = lists:map(
        fun({Key, Val}) ->
            NormKey = hb_ao:normalize_key(Key, #{ error_strategy => ignore }),
            KeyStr = 
                case NormKey of
                    undefined ->
                        io_lib:format("~p [!!! INVALID KEY !!!]", [Key]);
                    _ ->
                        hb_ao:normalize_key(Key)
                end,
            hb_util:format_indented(
                "~s => ~s~n",
                [
                    lists:flatten([KeyStr]),
                    case Val of
                        NextMap when is_map(NextMap) ->
                            hb_util:format_maybe_multiline(NextMap, Indent + 2);
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
                normalize(hb_ao:normalize_keys(Map1)),
                [<<"content-type">>, <<"body-keys">>, <<"inline-body-key">>]
            )
        ),
    Keys2 =
        maps:keys(
            NormMap2 = minimize(
                normalize(hb_ao:normalize_keys(Map2)),
                [<<"content-type">>, <<"body-keys">>, <<"inline-body-key">>]
            )
        ),
    PrimaryKeysPresent =
        (Mode == primary) andalso
            lists:all(
                fun(Key) -> lists:member(Key, Keys1) end,
                Keys1
            ),
    ?event({match, {keys1, Keys1}, {keys2, Keys2}, {mode, Mode}, {primary_keys_present, PrimaryKeysPresent}}),
    case (Keys1 == Keys2) or (Mode == only_present) or PrimaryKeysPresent of
        true ->
            lists:all(
                fun(Key) ->
                    Val1 = hb_ao:normalize_keys(maps:get(Key, NormMap1, not_found)),
                    Val2 = hb_ao:normalize_keys(maps:get(Key, NormMap2, not_found)),
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
    lists:sort(lists:map(fun hb_ao:normalize_key/1, maps:keys(Map))).

%% @doc Filter messages that do not match the 'spec' given. The underlying match
%% is performed in the `only_present' mode, such that match specifications only
%% need to specify the keys that must be present.
with_commitments(Spec, Msg) ->
    with_commitments(Spec, Msg, #{}).
with_commitments(Spec, Msg = #{ <<"commitments">> := Commitments }, _Opts) ->
    ?event({with_commitments, {spec, Spec}, {commitments, Commitments}}),
    FilteredCommitments =
        maps:filter(
            fun(_, CommMsg) ->
                Res = match(Spec, CommMsg, primary),
                ?event({with_commitments, {commitments, CommMsg}, {spec, Spec}, {match, Res}}),
                Res
            end,
            Commitments
        ),
    ?event({with_commitments, {filtered_commitments, FilteredCommitments}}),
    Msg#{ <<"commitments">> => FilteredCommitments };
with_commitments(_Spec, Msg, _Opts) ->
    Msg.

%% @doc Filter messages that match the 'spec' given. Inverts the `with_commitments/2'
%% function, such that only messages that do _not_ match the spec are returned.
without_commitments(Spec, Msg) ->
    without_commitments(Spec, Msg, #{}).
without_commitments(Spec, Msg = #{ <<"commitments">> := Commitments }, _Opts) ->
    ?event({without_commitments, {spec, Spec}, {msg, Msg}, {commitments, Commitments}}),
    FilteredCommitments =
        maps:without(
            maps:keys(
                maps:get(<<"commitments">>, with_commitments(Spec, Msg, #{}), #{})
            ),
            Commitments
        ),
    ?event({without_commitments, {filtered_commitments, FilteredCommitments}}),
    Msg#{ <<"commitments">> => FilteredCommitments };
without_commitments(_Spec, Msg, _Opts) ->
    Msg.

%% @doc Extract a commitment from a message given a `committer' ID, or a spec
%% message to match against. Returns only the first matching commitment, or
%% `not_found'.
commitment(Committer, Msg) ->
    commitment(Committer, Msg, #{}).
commitment(CommitterID, Msg, Opts) when is_binary(CommitterID) ->
    commitment(#{ <<"committer">> => CommitterID }, Msg, Opts);
commitment(Spec, #{ <<"commitments">> := Commitments }, _Opts) ->
    Matches =
        maps:filtermap(
            fun(ID, CommMsg) ->
                case match(Spec, CommMsg, primary) of
                    true -> {true, {ID, CommMsg}};
                    false -> false
                end
            end,
            Commitments
        ),
    case maps:values(Matches) of
        [] -> not_found;
        [{ID, Commitment}] -> {ok, ID, Commitment};
        _ ->
            ?event(commitment, {multiple_matches, {matches, Matches}}),
            multiple_matches
    end;
commitment(_Spec, _Msg, _Opts) ->
    % The message has no commitments, so the spec can never match.
    not_found.

%% @doc Implements a standard pattern in which the target for an operation is
%% found by looking for a `target' key in the request. If the target is `self',
%% or not present, the operation is performed on the original message. Otherwise,
%% the target is expected to be a key in the message, and the operation is
%% performed on the value of that key.
find_target(Self, Req, Opts) ->
	GetOpts = Opts#{
        hashpath => ignore,
        cache_control => [<<"no-cache">>, <<"no-store">>]
    },
    {ok,
        case hb_ao:get(<<"target">>, Req, <<"self">>, GetOpts) of
            <<"self">> -> Self;
            Key ->
                hb_ao:get(
                    Key,
                    Req,
                    hb_ao:get(<<"body">>, Req, GetOpts),
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
        lists:map(fun hb_ao:normalize_key/1, ?REGEN_KEYS)
            ++ lists:map(fun hb_ao:normalize_key/1, ExtraKeys),
    maps:filter(
        fun(Key, _) ->
            (not lists:member(hb_ao:normalize_key(Key), NormKeys))
                andalso (not hb_private:is_private(Key))
        end,
        maps:map(fun(_K, V) -> minimize(V) end, Map)
    ).

%% @doc Return a map with only the keys that necessary, without those that can
%% be regenerated.
normalize(Map) when is_map(Map) orelse is_list(Map) ->
    NormalizedMap = hb_ao:normalize_keys(Map),
    FilteredMap = filter_default_keys(NormalizedMap),
    maps:with(matchable_keys(FilteredMap), FilteredMap);
normalize(Other) ->
    Other.

%% @doc Remove keys from a map that have the default values found in the tx
%% record.
filter_default_keys(Map) ->
    DefaultsMap = default_tx_message(),
    maps:filter(
        fun(Key, Value) ->
            case maps:find(hb_ao:normalize_key(Key), DefaultsMap) of
                {ok, Value} -> false;
                _ -> true
            end
        end,
        Map
    ).

%% @doc Get the normalized fields and default values of the tx record.
default_tx_message() ->
    maps:from_list(default_tx_list()).

%% @doc Get the ordered list of fields as AO-Core keys and default values of
%% the tx record.
default_tx_list() ->
    Keys = lists:map(fun hb_ao:normalize_key/1, record_info(fields, tx)),
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

signed_only_committed_data_field_test(Codec) ->
    Msg = commit(#{ <<"data">> => <<"DATA">> }, hb:wallet(), Codec),
    ?event({signed_msg, Msg}),
    {ok, OnlyCommitted} = with_only_committed(Msg),
    ?event({only_committed, OnlyCommitted}),
    ?assert(verify(OnlyCommitted)).

signed_nested_data_key_test(Codec) ->
    Msg = #{
        <<"layer">> => <<"outer">>,
        <<"body">> =>
            commit(
                #{
                    <<"layer">> => <<"inner">>,
                    <<"data">> => <<"DATA">>
                },
                #{ priv_wallet => hb:wallet() },
                Codec
            )
    },
    Encoded = convert(Msg, Codec, #{}),
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

binary_to_binary_test(<<"flat@1.0">>) -> ok;
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

nested_empty_map_test(Codec) ->
    Msg = #{ <<"body">> => #{ <<"empty-map-test">> => #{}}},
    Encoded = convert(Msg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assert(match(Msg, Decoded)).

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
            MainBodyKey => #{
                MainBodyKey => <<"depth2-body">>
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
        <<"test-data">> => <<"TEST DATA">>,
        <<"test-key">> => <<"TEST VALUE">>
    },
    {ok, SignedMsg} =
        dev_message:commit(
            Msg,
            #{ <<"commitment-device">> => Codec },
            #{ priv_wallet => hb:wallet() }
        ),
    ?event({signed_msg, SignedMsg}),
    ?assertEqual(true, verify(SignedMsg)),
    ?event({verified, SignedMsg}),
    Encoded = convert(SignedMsg, Codec, #{}),
    ?event({msg_encoded_as_codec, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
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
        dev_message:commit(
            Msg,
            #{ <<"commitment-device">> => Codec },
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
            #{ priv_wallet => Wallet1 }
        ),
    ?event({signed_msg, SignedMsg}),
    {ok, MsgSignedTwice} =
        dev_message:commit(
            SignedMsg,
            #{ <<"commitment-device">> => Codec },
            #{ priv_wallet => Wallet2 }
        ),
    ?event({signed_msg_twice, MsgSignedTwice}),
    ?assert(verify(MsgSignedTwice)),
    Committers = hb_message:signers(MsgSignedTwice),
    ?event({committers, Committers}),
    ?assert(lists:member(hb_util:human_id(ar_wallet:to_address(Wallet1)), Committers)),
    ?assert(lists:member(hb_util:human_id(ar_wallet:to_address(Wallet2)), Committers)).

tabm_ao_ids_equal_test(Codec) ->
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
        dev_message:id(Msg, #{ <<"committers">> => <<"none">>}, #{}),
        dev_message:id(Decoded, #{ <<"committers">> => <<"none">>}, #{})
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
        dev_message:commit(
            EncDec,
            #{ <<"commitment-device">> => Codec },
            #{ priv_wallet => hb:wallet() }
        ),
    ?event({signed_msg, SignedMsg}),
    {ok, Res} = dev_message:verify(SignedMsg, #{ <<"committers">> => <<"all">>}, #{}),
    ?event({verify_res, Res}),
    ?assertEqual(true, verify(SignedMsg)),
    ?event({verified, SignedMsg}),
    Encoded = convert(SignedMsg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    {ok, DecodedRes} = dev_message:verify(Decoded, #{ <<"committers">> => <<"all">>}, #{}),
    ?event({verify_decoded_res, DecodedRes}),
    ?assert(
        match(
            SignedMsg,
            Decoded
        )
    ).

signed_list_test(Codec) ->
    Msg = #{ <<"key-with-list">> => [1.0, 2.0, 3.0] },
    Signed = commit(Msg, hb:wallet(), Codec),
    ?assert(verify(Signed)),
    Encoded = convert(Signed, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assert(verify(Decoded)),
    ?assert(match(Signed, Decoded)).

unsigned_id_test(Codec) ->
    Msg = #{ <<"data">> => <<"TEST_DATA">> },
    Encoded = convert(Msg, Codec, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?assertEqual(
        dev_message:id(Decoded, #{ <<"committers">> => <<"none">>}, #{}),
        dev_message:id(Msg, #{ <<"committers">> => <<"none">>}, #{})
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
    SignedMsg = commit(Msg, hb:wallet(), Codec),
    ?event({signed_msg, {explicit, SignedMsg}}),
    {ok, Res} = dev_message:verify(SignedMsg, #{ <<"committers">> => <<"all">>}, #{}),
    ?event({verify_res, {explicit, Res}}),
    ?assert(verify(SignedMsg)),
    ?event({verified, {explicit, SignedMsg}}),
    Encoded = convert(SignedMsg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
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
        dev_message:commit(
            Msg,
            #{ <<"commitment-device">> => Codec },
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

committed_keys_test(Codec) ->
    Msg = #{ <<"a">> => 1, <<"b">> => 2, <<"c">> => 3 },
    Signed = commit(Msg, hb:wallet(), Codec),
    CommittedKeys = committed(Signed),
    ?event({committed_keys, CommittedKeys}),
    ?assert(verify(Signed)),
    ?assert(lists:member(<<"a">>, CommittedKeys)),
    ?assert(lists:member(<<"b">>, CommittedKeys)),
    ?assert(lists:member(<<"c">>, CommittedKeys)),
    MsgToFilter = Signed#{ <<"bad-key">> => <<"BAD VALUE">> },
    ?assert(not lists:member(<<"bad-key">>, committed(MsgToFilter))).

committed_empty_keys_test(Codec) ->
    Msg = #{
        <<"very">> => <<>>,
        <<"exciting">> => #{},
        <<"values">> => [],
        <<"non-empty">> => <<"TEST">>
    },
    Signed = commit(Msg, hb:wallet(), Codec),
    ?assert(verify(Signed)),
    CommittedKeys = committed(Signed),
    ?event({committed_keys, CommittedKeys}),
    ?assert(lists:member(<<"very">>, CommittedKeys)),
    ?assert(lists:member(<<"exciting">>, CommittedKeys)),
    ?assert(lists:member(<<"values">>, CommittedKeys)),
    ?assert(lists:member(<<"non-empty">>, CommittedKeys)).

deeply_nested_committed_keys_test() ->
    Msg = #{
        <<"a">> => 1,
        <<"b">> => #{ <<"c">> => #{ <<"d">> => <<0:((1 + 1024) * 1024)>> } },
        <<"e">> => <<0:((1 + 1024) * 1024)>>
    },
    Signed = commit(Msg, hb:wallet()),
    {ok, WithOnlyCommitted} = with_only_committed(Signed),
    ?event({with_only_committed, WithOnlyCommitted}),
    ?assert(
        match(
            Msg,
            maps:without([<<"commitments">>], WithOnlyCommitted)
        )
    ).

signed_with_inner_signed_message_test(Codec) ->
    Wallet = hb:wallet(),
    Msg = commit(#{
        <<"a">> => 1,
        <<"inner">> =>
            maps:merge(
                commit(
                    #{
                        <<"c">> => <<"abc">>,
                        <<"e">> => 5
                    },
                    Wallet,
                    Codec
                ),
                % Uncommitted keys that should be ripped out of the inner message
                % by `with_only_committed'. These should still be present in the
                % `with_only_committed' outer message. For now, only `httpsig@1.0'
                % supports stripping non-committed keys.
                case Codec of
                    <<"httpsig@1.0">> ->
                        #{
                            <<"f">> => 6,
                            <<"g">> => 7
                        };
                    _ -> #{}
                end
            )
    }, Wallet, Codec),
    ?event({initial_msg, Msg}),
    % 1. Verify the outer message without changes.
    ?assert(verify(Msg)),
    {ok, CommittedInner} = with_only_committed(maps:get(<<"inner">>, Msg)),
    ?event({committed_inner, CommittedInner}),
    ?event({inner_committers, hb_message:signers(CommittedInner)}),
    % 2. Verify the inner message without changes.
    ?assert(verify(CommittedInner, signers)),
    % 3. Convert the message to the format and back.
    Encoded = convert(Msg, Codec, #{}),
    ?event({encoded, Encoded}),
    %?event({encoded_body, {string, maps:get(<<"body">>, Encoded)}}, #{}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    % 4. Verify the outer message after decode.
    ?assert(match(Msg, Decoded)),
    ?assert(verify(Decoded)),
    % 5. Verify the inner message from the converted message, applying
    % `with_only_committed' first.
    InnerDecoded = maps:get(<<"inner">>, Decoded),
    ?event({inner_decoded, InnerDecoded}),
    % Applying `with_only_committed' should verify the inner message.
    {ok, CommittedInnerOnly} = with_only_committed(InnerDecoded),
    ?event({committed_inner_only, CommittedInnerOnly}),
    ?assert(verify(CommittedInnerOnly, signers)).

large_body_committed_keys_test(Codec) ->
    case Codec of
        <<"httpsig@1.0">> ->
            Msg = #{
                <<"a">> => 1,
                <<"b">> => 2,
                <<"c">> => #{ <<"d">> => << 1:((1 + 1024) * 1024) >> }
            },
            Encoded = convert(Msg, Codec, #{}),
            ?event({encoded, Encoded}),
            Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
            ?event({decoded, Decoded}),
            Signed = commit(Decoded, hb:wallet(), Codec),
            ?event({signed, Signed}),
            CommittedKeys = committed(Signed),
            ?assert(lists:member(<<"a">>, CommittedKeys)),
            ?assert(lists:member(<<"b">>, CommittedKeys)),
            ?assert(lists:member(<<"c">>, CommittedKeys)),
            MsgToFilter = Signed#{ <<"bad-key">> => <<"BAD VALUE">> },
            ?assert(not lists:member(<<"bad-key">>, committed(MsgToFilter)));
        _ ->
            skip
    end.

sign_node_message_test(Codec) ->
    Msg = hb_message:commit(hb_opts:default_message(), hb:wallet(), Codec),
    ?event({committed, Msg}),
    ?assert(verify(Msg)),
    Encoded = convert(Msg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assert(match(Msg, Decoded)),
    ?assert(verify(Decoded)).

nested_body_list_test(Codec) ->
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
    Encoded = convert(Msg, Codec, #{}),
    ?event(encoded, {encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assert(match(Msg, Decoded)).

recursive_nested_list_test(Codec) ->
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
    Encoded = convert(Msg, Codec, #{}),
    ?event(encoded, {encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assert(match(Msg, Decoded)).

priv_survives_conversion_test(<<"ans104@1.0">>) -> skip;
priv_survives_conversion_test(<<"json@1.0">>) -> skip;
priv_survives_conversion_test(Codec) ->
    Msg = #{
        <<"data">> => <<"TEST_DATA">>,
        <<"priv">> => #{ <<"test_key">> => <<"TEST_VALUE">> }
    },
    Encoded = convert(Msg, Codec, #{}),
    ?event({encoded, Encoded}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event({decoded, Decoded}),
    ?assert(match(Msg, Decoded)),
    ?assertMatch(
        #{ <<"test_key">> := <<"TEST_VALUE">> },
        maps:get(<<"priv">>, Decoded, #{})
    ).

encode_balance_table(Size, Codec) ->
    Msg =
        #{
            hb_util:encode(crypto:strong_rand_bytes(32)) =>
                rand:uniform(1_000_000_000_000_000)
        ||
            _ <- lists:seq(1, Size)
        },
    Encoded = convert(Msg, Codec, #{}),
    ?event(debug, {encoded, {explicit, Encoded}}),
    Decoded = convert(Encoded, <<"structured@1.0">>, Codec, #{}),
    ?event(debug, {decoded, Decoded}),
    ?assert(match(Msg, Decoded)).

encode_small_balance_table_test(Codec) ->
    encode_balance_table(5, Codec).

encode_large_balance_table_test(Codec) ->
    encode_balance_table(1000, Codec).

%%% Test helpers

test_codecs() ->
    [
        <<"structured@1.0">>,
        <<"httpsig@1.0">>,
        <<"flat@1.0">>,
        <<"ans104@1.0">>,
        <<"json@1.0">>
    ].

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
        {"TABM AO-Core ids equal test", fun tabm_ao_ids_equal_test/1},
        {"message with large keys test", fun message_with_large_keys_test/1},
        {"nested message with large keys and content test",
            fun nested_message_with_large_keys_and_content_test/1},
        {"simple nested message test", fun simple_nested_message_test/1},
        {"nested empty map test", fun nested_empty_map_test/1},
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
        {"message with simple embedded list test",
            fun message_with_simple_embedded_list_test/1},
        {"empty string in tag test", fun empty_string_in_tag_test/1},
        {"signed item to message and back test",
            fun signed_message_encode_decode_verify_test/1},
        {"signed deep serialize and deserialize test",
            fun signed_deep_message_test/1},
        {"nested data key test", fun signed_nested_data_key_test/1},
        {"signed only committed data field test", fun signed_only_committed_data_field_test/1},
        {"unsigned id test", fun unsigned_id_test/1},
        {"complex signed message test", fun complex_signed_message_test/1},
        {"signed message with hashpath test", fun hashpath_sign_verify_test/1},
        {"message with derived components test", fun signed_message_with_derived_components_test/1},
        {"committed keys test", fun committed_keys_test/1},
        {"committed empty keys test", fun committed_empty_keys_test/1},
        {"large body committed keys test", fun large_body_committed_keys_test/1},
        {"signed list http response test", fun signed_list_test/1},
        {"signed with inner signed test", fun signed_with_inner_signed_message_test/1},
        {"priv survives conversion test", fun priv_survives_conversion_test/1},
        {"sign node message test", fun sign_node_message_test/1},
        {"nested list test", fun nested_body_list_test/1},
        {"recursive nested list test", fun recursive_nested_list_test/1},
        {"encode small balance table test", fun encode_small_balance_table_test/1},
        {"encode large balance table test", fun encode_large_balance_table_test/1}
    ]).

run_test() ->
    encode_balance_table(1000, <<"httpsig@1.0">>).