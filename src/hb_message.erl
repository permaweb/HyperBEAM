
%%% @doc This module acts an adapter between messages, as modeled in the
%%% AO-Core protocol, and their underlying binary representations and formats.
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
%%%     - Arweave transactions.
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
%%% arbitrary type, or `hb_formatter:format_msg/1' to print an AO-Core/TABM message in
%%% a human-readable format.
%%% 
%%% The `hb_cache' module is responsible for storing and retrieving messages in
%%% the HyperBEAM stores configured on the node. Each store has its own storage
%%% backend, but each works with simple key-value pairs. Subsequently, the 
%%% `hb_cache' module uses TABMs as the internal format for storing and 
%%% retrieving messages.
%%% 
%%% Test vectors to ensure the functioning of this module and the codecs that
%%% interact with it are found in `hb_message_test_vectors.erl'.
-module(hb_message).
-export([id/1, id/2, id/3]).
-export([convert/3, convert/4, uncommitted/1, uncommitted/2, committed/3]).
-export([with_only_committers/2, with_only_committers/3, commitment_devices/2]).
-export([verify/1, verify/2, verify/3, commit/2, commit/3, signers/2, type/1, minimize/1]).
-export([normalize_commitments/2, normalize_commitments/3, is_signed_key/3]).
-export([commitment/2, commitment/3, commitments/3]).
-export([with_only_committed/2, without_unless_signed/3]).
-export([with_commitments/3, without_commitments/3, remove_all_commitments/2]).
-export([diff/3, match/2, match/3, match/4, find_target/3]).
%%% Helpers:
-export([default_tx_list/0, filter_default_keys/1]).
%%% Debugging tools:
-export([print/1]).
-include("include/hb.hrl").

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
convert(Msg, TargetFormat, tabm, Opts) ->
    OldPriv =
        if is_map(Msg) -> maps:get(<<"priv">>, Msg, #{});
           true -> #{}
        end,
    from_tabm(Msg, TargetFormat, OldPriv, Opts);
convert(Msg, TargetFormat, SourceFormat, Opts) ->
    OldPriv =
        if is_map(Msg) -> maps:get(<<"priv">>, Msg, #{});
           true -> #{}
        end,
    TABM =
        to_tabm(
            case is_map(Msg) of
                true -> hb_maps:without([<<"priv">>], Msg, Opts);
                false -> Msg
            end,
            SourceFormat,
            Opts
        ),
    case TargetFormat of
        tabm -> restore_priv(TABM, OldPriv, Opts);
        _ -> from_tabm(TABM, TargetFormat, OldPriv, Opts)
    end.

to_tabm(Msg, SourceFormat, Opts) ->
    {SourceCodecMod, Params} = conversion_spec_to_req(SourceFormat, Opts),
    % We use _from_ here because the codecs are labelled from the perspective
    % of their own format. `dev_codec_ans104:from/1' will convert _from_
    % an ANS-104 message _into_ a TABM.
    case SourceCodecMod:from(Msg, Params, Opts) of
        {ok, TypicalMsg} when is_map(TypicalMsg) ->
            TypicalMsg;
        {ok, OtherTypeRes} -> OtherTypeRes
    end.

from_tabm(Msg, TargetFormat, OldPriv, Opts) ->
    {TargetCodecMod, Params} = conversion_spec_to_req(TargetFormat, Opts),
    % We use the _to_ function here because each of the codecs we may call in
    % this step are labelled from the perspective of the target format. For 
    % example, `dev_codec_httpsig:to/1' will convert _from_ a TABM to an
    % HTTPSig message.
    case TargetCodecMod:to(Msg, Params, Opts) of
        {ok, TypicalMsg} when is_map(TypicalMsg) ->
            restore_priv(TypicalMsg, OldPriv, Opts);
        {ok, OtherTypeRes} -> OtherTypeRes
    end.

%% @doc Add the existing `priv' sub-map back to a converted message, honoring
%% any existing `priv' sub-map that may already be present.
restore_priv(Msg, EmptyPriv, _Opts) when map_size(EmptyPriv) == 0 -> Msg;
restore_priv(Msg, OldPriv, Opts) ->
    MsgPriv = hb_maps:get(<<"priv">>, Msg, #{}, Opts),
    ?event({restoring_priv, {msg_priv, MsgPriv}, {old_priv, OldPriv}}),
    NewPriv = hb_util:deep_merge(MsgPriv, OldPriv, Opts),
    ?event({new_priv, NewPriv}),
    Msg#{ <<"priv">> => NewPriv }.

%% @doc Get a codec device and request params from the given conversion request. 
%% Expects conversion spec to either be a binary codec name, or a map with a
%% `device' key and other parameters. Additionally honors the `always_bundle'
%% key in the node message if present.
conversion_spec_to_req(Spec, Opts) when is_binary(Spec) or (Spec == tabm) ->
    conversion_spec_to_req(#{ <<"device">> => Spec }, Opts);
conversion_spec_to_req(Spec, Opts) ->
    try
        Device =
            hb_maps:get(
                <<"device">>,
                Spec,
                no_codec_device_in_conversion_spec,
                Opts
            ),
        {
            case Device of
                tabm -> tabm;
                _ ->
                    hb_ao_device:message_to_device(
                        #{
                            <<"device">> => Device
                        },
                        Opts
                    )
            end,
            hb_maps:without([<<"device">>], Spec, Opts)
        }
    catch _:_ ->
        throw({message_codec_not_extractable, Spec})
    end.

%% @doc Return the ID of a message.
id(Msg) -> id(Msg, uncommitted).
id(Msg, Opts) when is_map(Opts) -> id(Msg, uncommitted, Opts);
id(Msg, Committers) -> id(Msg, Committers, #{}).
id(Msg, RawCommitters, Opts) ->
    CommSpec =
        case RawCommitters of
            none -> #{ <<"committers">> => <<"none">> };
            uncommitted -> #{ <<"committers">> => <<"none">> };
            unsigned -> #{ <<"committers">> => <<"none">> };
            all -> #{ <<"committers">> => <<"all">> };
            signed -> #{ <<"committers">> => <<"all">> };
            List when is_list(List) -> #{ <<"committers">> => List }
        end,
    ?event({getting_id, {msg, Msg}, {spec, CommSpec}}),
    {ok, ID} =
        dev_message:id(
            Msg,
            CommSpec#{ <<"path">> => <<"id">> },
            Opts
        ),
    hb_util:human_id(ID).

%% @doc Normalize the IDs in a message, ensuring that there is at least one
%% unsigned ID present. By forcing this work to occur in strategically positioned
%% places, we avoid the need to recalculate the IDs for every `hb_message:id`
%% call.
normalize_commitments(Msg, Opts) ->
    normalize_commitments(Msg, Opts, passive).
normalize_commitments(Msg, Opts, Mode) when is_map(Msg) ->
    ?event(debug_normalize_commitments, {normalize_commitments, {msg, Msg}}),
    NormMsg = 
        maps:map(
            fun(Key, Val) when Key == <<"commitments">> orelse Key == <<"priv">> ->
                Val;
               (_Key, Val) -> normalize_commitments(Val, Opts, Mode)
            end,
            Msg
        ),
    do_normalize_commitments(NormMsg, Opts, Mode);
normalize_commitments(Msg, Opts, Mode) when is_list(Msg) ->
    ?event(debug_normalize_commitments, {normalize_commitments, {list, Msg}}),
    lists:map(fun(X) -> normalize_commitments(X, Opts, Mode) end, Msg);
normalize_commitments(Msg, _Opts, _Mode) ->
    Msg.

do_normalize_commitments(Msg, _Opts, _Mode) when ?IS_EMPTY_MESSAGE(Msg) ->
    Msg;
do_normalize_commitments(Msg, Opts, passive) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    {UnsignedCommitments, SignedCommitments} = 
        lists:partition(
            fun({_, #{ <<"committer">> := _Committer }}) -> false;
               ({_, _}) -> true
            end,
            hb_maps:to_list(Commitments)
        ),
    ?event({do_normalize_commitments,
        {unsigned_commitments, UnsignedCommitments},
        {maybe_signed_commitment, SignedCommitments}
    }),
    case {UnsignedCommitments, SignedCommitments} of
        {[], _} ->
            {ok, #{ <<"commitments">> := NewCommitments }} =
                dev_message:commit(
                    uncommitted(Msg),
                    #{ 
                        <<"type">> => <<"unsigned">>
                    },
                    Opts
                ),
            MergedCommitments = hb_maps:merge(
                NewCommitments,
                hb_maps:from_list(SignedCommitments),
                Opts
            ),
            Msg#{ <<"commitments">> => MergedCommitments };
        _ -> Msg
    end;
do_normalize_commitments(Msg, Opts, verify) ->
    UnsignedCommitment = commitment(#{ <<"type">> => <<"unsigned">> }, Msg, Opts),
    {MaybeUnsignedID, MaybeCommittedSpec} =
        case UnsignedCommitment of
            {ok, ID, #{ <<"committed">> := Committed }} ->
                {ID, #{ <<"committed">> => Committed }};
            _ -> {undefined, #{}}
        end,
    {ok, #{ <<"commitments">> := NormCommitments }} =
        dev_message:commit(
            uncommitted(Msg),
            MaybeCommittedSpec#{ 
                <<"type">> => <<"unsigned">>
            },
            Opts
        ),
    ?event(normalization, {normalizing_commitments, verify}),
    [NormID] = hb_maps:keys(NormCommitments, Opts),
    case {MaybeUnsignedID, NormID} of
        {MatchedID, MatchedID} ->
            Msg;
        {undefined, _NewID} ->
            % We did not have an unsigned ID to begin with, so we need to add it.
            attach_phash2(
                Msg#{
                    <<"commitments">> =>
                        hb_maps:merge(
                            NormCommitments,
                            hb_maps:get(<<"commitments">>, Msg, #{}, Opts)
                        )
                },
                Opts
            );
        {_OldID, _NewID} ->
            {ok, #{ <<"commitments">> := NewCommitments }} = 
                dev_message:commit(
                    uncommitted(Msg),
                    #{ <<"type">> => <<"unsigned">> },
                    Opts
                ),
            % We had an unsigned ID to begin with and the new one is different.
            % This means that the committed keys have changed, so we drop any
            % other commitments and return only the new unsigned one.
            attach_phash2(Msg#{ <<"commitments">> => NewCommitments }, Opts)
    end;
do_normalize_commitments(Msg, Opts, fast) when is_map(Msg) ->
    ExpectedHash = erlang:phash2(hb_private:reset(Msg)),
    ?event(normalization,
        {normalizing_commitments,
            {expected_hash, ExpectedHash},
            {priv, hb_private:from_message(Msg)}
        }
    ),
    case hb_private:get(<<"last-phash2">>, Msg, not_found, Opts) of
        not_found ->
            attach_phash2(Msg, ExpectedHash, Opts);
        ExpectedHash ->
            Msg;
        _DifferingHash ->
            MsgWithHash = attach_phash2(Msg, ExpectedHash, Opts),
            do_normalize_commitments(MsgWithHash, Opts, verify)
    end.

%% @doc Annotate a message with its phash2 value in the `priv' sub-map,
%% calculating it if necessary.
attach_phash2(Msg, Opts) ->
    ExpectedHash = erlang:phash2(hb_private:reset(Msg)),
    attach_phash2(Msg, ExpectedHash, Opts).
attach_phash2(Msg, ExpectedHash, Opts) ->
    hb_private:set(Msg, <<"last-phash2">>, ExpectedHash, Opts).

%% @doc Return a message with only the committed keys. If no commitments are
%% present, the message is returned unchanged. This means that you need to
%% check if the message is:
%% - Committed
%% - Verifies
%% ...before using the output of this function as the 'canonical' message. This
%% is such that expensive operations like signature verification are not
%% performed unless necessary.
with_only_committed(Msg, Opts) when is_map(Msg) ->
    ?event({with_only_committed, {msg, Msg}, {opts, Opts}}),
    Comms = hb_maps:get(<<"commitments">>, Msg, not_found, Opts),
    case is_map(Msg) andalso Comms /= not_found of
        true ->
            try
                CommittedKeys =
                    hb_message:committed(
                        Msg,
                        #{ <<"commitment-ids">> => <<"all">> },
                        Opts
                    ),
                % Add the ao-body-key to the committed list if it is not
                % already present.
                ?event(debug_bundle, {committed_keys, CommittedKeys, {msg, Msg}}),
                {ok,
                    with_links(
                        [<<"commitments">> | CommittedKeys],
                        Msg,
                        Opts
                    )
                }
            catch Class:Reason:St ->
                {error,
                    {could_not_normalize,
                        {class, Class},
                        {reason, Reason},
                        {msg, Msg},
                        {stacktrace, St}
                    }
                }
            end;
        false -> {ok, Msg}
    end;
with_only_committed(Msg, _) ->
    % If the message is not a map, it cannot be signed.
    {ok, Msg}.

%% @doc Filter keys from a map that do not match either the list of keys or
%% their relative `+link` variants.
with_links(Keys, Map, Opts) ->
    hb_maps:with(
        Keys ++
            lists:map(
                fun(Key) ->
                    <<(hb_link:remove_link_specifier(Key))/binary, "+link">>
                end,
                Keys
            ),
        Map,
        Opts
    ).

%% @doc Return the message with only the specified committers attached.
with_only_committers(Msg, Committers) ->
    with_only_committers(Msg, Committers, #{}).
with_only_committers(Msg, Committers, Opts) when is_map(Msg) ->
    NewCommitments =
        hb_maps:filter(
            fun(_, #{ <<"committer">> := Committer }) ->
                lists:member(Committer, Committers);
               (_, _) -> false
            end,
            hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
			Opts
        ),
    Msg#{ <<"commitments">> => NewCommitments };
with_only_committers(Msg, _Committers, _Opts) ->
    throw({unsupported_message_type, Msg}).

%% @doc Determine whether a specific key is part of a message's commitments.
is_signed_key(Key, Msg, Opts) ->
    lists:member(Key, hb_message:committed(Msg, all, Opts)).

%% @doc Remove the any of the given keys that are not signed from a message.
without_unless_signed(Key, Msg, Opts) when not is_list(Key) ->
    without_unless_signed([Key], Msg, Opts);
without_unless_signed(Keys, Msg, Opts) ->
    SignedKeys = hb_message:committed(Msg, all, Opts),
    maps:without(
        lists:filter(fun(K) -> not lists:member(K, SignedKeys) end, Keys),
        Msg
    ).

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
commit(Msg, Opts, CodecName) when is_binary(CodecName) ->
    commit(Msg, Opts, #{ <<"commitment-device">> => CodecName });
commit(Msg, Opts, Spec) ->
    {ok, Signed} =
        dev_message:commit(
            Msg,
            Spec#{
                <<"commitment-device">> =>
                    case hb_maps:get(<<"commitment-device">>, Spec, none, Opts) of
                        none ->
                            case hb_maps:get(<<"device">>, Spec, none, Opts) of
                                none ->
                                    hb_opts:get(
                                        commitment_device,
                                        no_viable_commitment_device,
                                        Opts
                                    );
                                Device -> Device
                            end;
                        CommitmentDevice -> CommitmentDevice
                    end
            },
            Opts
        ),
    Signed.

%% @doc Return the list of committed keys from a message.
committed(Msg, all, Opts) ->
    committed(Msg, #{ <<"committers">> => <<"all">> }, Opts);
committed(Msg, none, Opts) ->
    committed(Msg, #{ <<"committers">> => <<"none">> }, Opts);
committed(Msg, List, Opts) when is_list(List) ->
    committed(Msg, #{ <<"commitment-ids">> => List }, Opts);
committed(Msg, CommittersMsg, Opts) ->
    ?event(
        {committed,
            {msg, {explicit, Msg}},
            {committers_msg, {explicit, CommittersMsg}},
            {opts, Opts}
        }
    ),
    {ok, CommittedKeys} = dev_message:committed(Msg, CommittersMsg, Opts),
    CommittedKeys.

%% @doc wrapper function to verify a message.
verify(Msg) -> verify(Msg, all).
verify(Msg, Committers) ->
    verify(Msg, Committers, #{}).
verify(Msg, all, Opts) ->
    verify(Msg, <<"all">>, Opts);
verify(Msg, signers, Opts) ->
    verify(Msg, hb_message:signers(Msg, Opts), Opts);
verify(Msg, Committers, Opts) when not is_map(Committers) ->
    verify(
        Msg,
        #{
            <<"committers">> =>
                case ?IS_ID(Committers) of
                    true -> [Committers];
                    false -> Committers
                end
        },
        Opts
    );
verify(Msg, Spec, Opts) ->
    ?event(verify, {verify, {spec, Spec}}),
    {ok, Res} =
        dev_message:verify(
            Msg,
            Spec,
            Opts
        ),
    Res.

%% @doc Return the unsigned version of a message in AO-Core format.
uncommitted(Msg) -> uncommitted(Msg, #{}).
uncommitted(Bin, _Opts) when is_binary(Bin) -> Bin;
uncommitted(Msg, Opts) ->
    hb_maps:remove(<<"commitments">>, Msg, Opts).
remove_all_commitments(Msg, Opts) ->
    % Remove commitments at the current level
    MsgWithoutCommitments = hb_maps:remove(<<"commitments">>, Msg, Opts),
    % Recursively remove commitments from nested maps
    maps:map(
        fun(_Key, Value) when is_map(Value) ->
            uncommitted(Value, Opts);
           (_Key, Value) when is_list(Value) ->
            lists:map(
                fun(Item) when is_map(Item) -> uncommitted(Item, Opts);
                   (Item) -> Item
                end,
                Value
            );
           (_Key, Value) ->
            Value
        end,
        MsgWithoutCommitments
    ).

%% @doc Return all of the committers on a message that have 'normal', 256 bit, 
%% addresses.
signers(Msg, Opts) ->
    hb_util:ok(dev_message:committers(Msg, #{}, Opts)).

%% @doc Pretty-print a message.
print(Msg) -> print(Msg, 0).
print(Msg, Indent) ->
    io:format(standard_error, "~s", [lists:flatten(hb_format:message(Msg, #{}, Indent))]).

%% @doc Return the type of an encoded message.
type(TX) when is_record(TX, tx) -> tx;
type(Binary) when is_binary(Binary) -> binary;
type(Msg) when is_map(Msg) ->
    IsDeep = lists:any(
        fun({_, Value}) -> is_map(Value) end,
        lists:filter(
            fun({Key, _}) -> not hb_private:is_private(Key) end,
            hb_maps:to_list(Msg)
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
%% Returns `true` or `{ErrType, Err}`.
match(Map1, Map2) ->
    match(Map1, Map2, strict).
match(Map1, Map2, Mode) ->
    match(Map1, Map2, Mode, #{}).
match(Map1, Map2, Mode, Opts) ->
    try unsafe_match(Map1, Map2, Mode, [], Opts)
    catch
        throw:{mismatch, Type, Path, Val1, Val2} ->
            {mismatch, Type, Path, Val1, Val2};
        _:Details:St -> {error, {Details, {trace, St}}}
    end.

%% @doc Match two maps, returning `true' if they match, or throwing an error
%% if they do not.
unsafe_match(RawMap1, RawMap2, Mode, Path, Opts) ->
    {_, SignedCommitments1} = 
        lists:partition(
            fun({_, #{ <<"committer">> := _Committer }}) -> false;
               ({_, _}) -> true
            end,
            hb_maps:to_list(hb_maps:get(<<"commitments">>, RawMap1, #{}, Opts))
        ),
    {_, SignedCommitments2} = 
        lists:partition(
            fun({_, #{ <<"committer">> := _Committer }}) -> false;
               ({_, _}) -> true
            end,
            hb_maps:to_list(hb_maps:get(<<"commitments">>, RawMap1, #{}, Opts))
        ),
    Map1 = RawMap1#{ <<"commitments">> => SignedCommitments1 },
    Map2 = RawMap2#{ <<"commitments">> => SignedCommitments2 },
    Keys1 =
        hb_maps:keys(
            NormMap1 =
                minimize(
                    normalize(
                        hb_ao:normalize_keys(Map1, Opts),
                        Opts
                    ),
                    [<<"content-type">>, <<"ao-body-key">>]
                )
        ),
    Keys2 =
        hb_maps:keys(
            NormMap2 =
                minimize(
                    normalize(
                        hb_ao:normalize_keys(Map2, Opts),
                        Opts
                    ),
                    [<<"content-type">>, <<"ao-body-key">>]
                )
        ),
    PrimaryKeysPresent =
        (Mode == primary) andalso
            lists:all(
                fun(Key) -> lists:member(Key, Keys1) end,
                Keys1
            ),
    ?event(match,
        {match,
            {keys1, Keys1},
            {keys2, Keys2},
            {mode, Mode},
            {primary_keys_present, PrimaryKeysPresent},
            {base, Map1},
            {req, Map2}
        }
    ),
    case (Keys1 == Keys2) or (Mode == only_present) or PrimaryKeysPresent of
        true ->
            lists:all(
                fun(<<"commitments">>) -> true;
                (Key) ->
                    ?event(match, {matching_key, Key}),
                    Val1 =
                        hb_ao:normalize_keys(
                            hb_maps:get(Key, NormMap1, not_found, Opts),
                            Opts
                        ),
                    Val2 =
                        hb_ao:normalize_keys(
                            hb_maps:get(Key, NormMap2, not_found, Opts),
                            Opts
                        ),
                    BothPresent = (Val1 =/= not_found) and (Val2 =/= not_found),
                    case (not BothPresent) and (Mode == only_present) of
                        true -> true;
                        false ->
                            case is_map(Val1) andalso is_map(Val2) of
                                true ->
                                    unsafe_match(Val1, Val2, Mode, Path ++ [Key], Opts);
                                false ->
                                    case {Val1, Val2} of
                                        {V, V} -> true;
                                        {V, '_'} when V =/= not_found -> true;
                                        {'_', V} when V =/= not_found -> true;
                                        {'_', '_'} -> true;
                                        _ ->
                                            throw(
                                                {mismatch,
                                                    value,
                                                    hb_format:short_id(
                                                        hb_path:to_binary(
                                                            Path ++ [Key]
                                                        )
                                                    ),
                                                    Val1,
                                                    Val2
                                                }
                                            )
                                    end
                            end
                    end
                end,
                Keys1
            );
        false ->
            throw(
                {mismatch,
                    keys,
                    hb_format:short_id(hb_path:to_binary(Path)),
                    Keys1,
                    Keys2
                }
            )
    end.
	
matchable_keys(Map) ->
    lists:sort(lists:map(fun hb_ao:normalize_key/1, hb_maps:keys(Map))).

%% @doc Return the numeric differences between two messages, matching deeply
%% across nested messages. If the values are non-numeric, the new value is 
%% returned if the values are different. Keys found only in the first message
%% are dropped, as they have 'changed' to absence.
diff(Base, Req, Opts) when is_map(Base) andalso is_map(Req) ->
    maps:filtermap(
        fun(Key, Val2) ->
            case hb_maps:get(Key, Base, not_found, Opts) of
                Val2 ->
                    % The key is present in both maps, and the values match.
                    false;
                not_found ->
                    % The key is net-new in Map2.
                    {true, Val2};
                Val1 when is_number(Val1) andalso is_number(Val2) ->
                    % The key is present in both maps, and the values are numbers;
                    % return the difference.
                    {true, Val2 - Val1};
                Val1 when is_map(Val1) andalso is_map(Val2) ->
                    % The key is present in both maps, and the values are maps;
                    % return the difference.
                    {true, diff(Val1, Val2, Opts)};
                _ ->
                    % The key is present in both maps, and the values do not 
                    % match. Return the new value.
                    {true, Val2}
            end
        end,
        Req
    );
diff(_Val1, _Val2, _Opts) ->
    not_found.

%% @doc Filter messages that do not match the 'spec' given. The underlying match
%% is performed in the `only_present' mode, such that match specifications only
%% need to specify the keys that must be present.
with_commitments(ID, Msg, Opts) when ?IS_ID(ID) ->
    with_commitments([ID], Msg, Opts);
with_commitments(Spec, Msg = #{ <<"commitments">> := Commitments }, Opts) ->
    ?event({with_commitments, {spec, Spec}, {commitments, Commitments}}),
    FilteredCommitments =
        hb_maps:filter(
            fun(ID, CommMsg) ->
                if is_list(Spec) ->
                    lists:member(ID, Spec);
                is_map(Spec) ->
                    match(Spec, CommMsg, primary, Opts) == true
                end
            end,
            Commitments,
            Opts
        ),
    ?event({with_commitments, {filtered_commitments, FilteredCommitments}}),
    Msg#{ <<"commitments">> => FilteredCommitments };
with_commitments(_Spec, Msg, _Opts) ->
    Msg.

%% @doc Filter messages that match the 'spec' given. Inverts the `with_commitments/2'
%% function, such that only messages that do _not_ match the spec are returned.
without_commitments(Spec, Msg = #{ <<"commitments">> := Commitments }, Opts) ->
    ?event({without_commitments, {spec, Spec}, {msg, Msg}, {commitments, Commitments}}),
    FilteredCommitments =
        hb_maps:without(
            hb_maps:keys(
                hb_maps:get(
                    <<"commitments">>,
                    with_commitments(Spec, Msg, Opts),
                    #{},
                    Opts
                )
            ),
            Commitments
        ),
    ?event({without_commitments, {filtered_commitments, FilteredCommitments}}),
    Msg#{ <<"commitments">> => FilteredCommitments };
without_commitments(_Spec, Msg, _Opts) ->
    Msg.

%% @doc Extract a commitment from a message given a `committer' or `commitment'
%% ID, or a spec message to match against. Returns only the first matching
%% commitment, or `not_found'.
commitment(ID, Msg) ->
    commitment(ID, Msg, #{}).
commitment(ID, Link, Opts) when ?IS_LINK(Link) ->
    commitment(ID, hb_cache:ensure_loaded(Link, Opts), Opts);
commitment(ID, #{ <<"commitments">> := Commitments }, Opts)
        when is_binary(ID), is_map_key(ID, Commitments) ->
    hb_maps:get(
        ID,
        Commitments,
        not_found,
        Opts
    );
commitment(#{ <<"type">> := <<"unsigned">> }, Msg, Opts) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    UnsignedCommitments =
        hb_maps:filter(
            fun(_, #{ <<"committer">> := _Committer }) -> false;
                (_, _) -> true
            end,
            Commitments,
            Opts
        ),
    if 
        map_size(UnsignedCommitments) == 0 -> not_found;
        map_size(UnsignedCommitments) == 1 ->
            CommID = hd(maps:keys(UnsignedCommitments)),
            {ok, CommID, hb_util:ok(hb_maps:find(CommID, UnsignedCommitments, Opts))};
        true ->
            ?event(commitment, {multiple_matches, {matches, UnsignedCommitments}}),
            multiple_matches    end;
commitment(Spec, Msg, Opts) ->
    Matches = commitments(Spec, Msg, Opts),
    ?event(debug_commitment, {commitment, {spec, Spec}, {matches, Matches}}),
    if
        map_size(Matches) == 0 -> not_found;
        map_size(Matches) == 1 ->
            CommID = hd(hb_maps:keys(Matches)),
            {ok, CommID, hb_util:ok(hb_maps:find(CommID, Matches, Opts))};
        true ->
            ?event(commitment, {multiple_matches, {matches, Matches}}),
            multiple_matches
    end;
commitment(_Spec, _Msg, _Opts) ->
    % The message has no commitments, so the spec can never match.
    not_found.

%% @doc Return a list of all commitments that match the spec.
commitments(ID, Link, Opts) when ?IS_LINK(Link) ->
    commitments(ID, hb_cache:ensure_loaded(Link, Opts), Opts);
commitments(CommitterID, Msg, Opts) when is_binary(CommitterID) ->
    commitments(#{ <<"committer">> => CommitterID }, Msg, Opts);
commitments(Spec, #{ <<"commitments">> := Commitments }, Opts) ->
    hb_maps:filtermap(
        fun(_ID, CommMsg) ->
            case match(Spec, CommMsg, primary, Opts) of
                true -> {true, CommMsg};
                _ -> false
            end
        end,
        Commitments,
        Opts
    );
commitments(_Spec, _Msg, _Opts) ->
    #{}.

%% @doc Return the devices for which there are commitments on a message.
commitment_devices(#{ <<"commitments">> := Commitments }, Opts) ->
    lists:map(
        fun(CommMsg) ->
            hb_ao:get(<<"commitment-device">>, CommMsg, Opts)
        end,
        maps:values(Commitments)
    );
commitment_devices(_Msg, _Opts) ->
    [].

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
        case hb_maps:get(<<"target">>, Req, <<"self">>, GetOpts) of
            <<"self">> -> Self;
            Key ->
                hb_maps:get(
                    Key,
                    Req,
                    hb_maps:get(<<"body">>, Req, GetOpts),
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
normalize(Map, Opts) when is_map(Map) orelse is_list(Map) ->
    NormalizedMap = hb_ao:normalize_keys(Map, Opts),
    FilteredMap = filter_default_keys(NormalizedMap),
    hb_maps:with(matchable_keys(FilteredMap), FilteredMap);
normalize(Other, _Opts) ->
    Other.

%% @doc Remove keys from a map that have the default values found in the tx
%% record.
filter_default_keys(Map) ->
    DefaultsMap = default_tx_message(),
    maps:filter(
        fun(Key, Value) ->
            case hb_maps:find(hb_ao:normalize_key(Key), DefaultsMap) of
                {ok, Value} -> false;
                _ -> true
            end
        end,
        Map
    ).

%% @doc Get the normalized fields and default values of the tx record.
default_tx_message() ->
    hb_maps:from_list(default_tx_list()).

%% @doc Get the ordered list of fields as AO-Core keys and default values of
%% the tx record.
default_tx_list() ->
    Keys = lists:map(fun hb_ao:normalize_key/1, record_info(fields, tx)),
    lists:zip(Keys, tl(tuple_to_list(#tx{}))).
