
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
%%% interact with it are found in `hb_codec_test_vectors.erl'.
-module(hb_message).
-export([id/1, id/2, id/3]).
-export([convert/3, convert/4, uncommitted/1, uncommitted/2, committed/3]).
-export([add_bundle_hint/2, add_bundle_hint/3]).
-export([with_only_committers/2, with_only_committers/3, commitment_devices/2]).
-export([verify/1, verify/2, verify/3, deep_verify/2]).
-export([paranoid_verify/2, paranoid_verify/3]).
-export([commit/2, commit/3, with_committer/2, signers/2, type/1, minimize/1]).
-export([normalize_commitments/2, normalize_commitments/3]).
-export([is_signed_key/3]).
-export([commitment/2, commitment/3, commitments/3]).
-export([with_only_committed/2, with_only_committed/3]).
-export([with_only_committed_deep/2]).
-export([without_unless_signed/3]).
-export([with_commitments/3, without_commitments/3, uncommitted_deep/2]).
-export([diff/3, match/2, match/3, match/4, find_subject/4]).
%%% Debugging tools:
-export([print/1]).
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
            TargetFormat,
            SourceFormat,
            Opts
        ),
    case TargetFormat of
        tabm -> restore_priv(TABM, OldPriv, Opts);
        _ -> from_tabm(TABM, TargetFormat, OldPriv, Opts)
    end.

to_tabm(Msg, TargetFormat, SourceFormat, Opts) ->
    {SourceCodecMod, Params0} = conversion_spec_to_req(SourceFormat, Opts),
    Params = add_bundle_hint(Params0, TargetFormat, Opts),
    % We use _from_ here because the codecs are labelled from the perspective
    % of their own format. `dev_codec_ans104:from/1' will convert _from_
    % an ANS-104 message _into_ a TABM.
    case SourceCodecMod:from(Msg, Params, Opts) of
        {ok, TypicalMsg} when is_map(TypicalMsg) ->
            TypicalMsg;
        {ok, OtherTypeRes} -> OtherTypeRes
    end.

%% @doc Extract the device value from a conversion spec.
conversion_spec_device(Spec, _Default, _Opts)
        when is_binary(Spec) orelse (Spec == tabm) ->
    Spec;
conversion_spec_device(Spec, Default, Opts) when is_map(Spec) ->
    hb_maps:get(<<"device">>, Spec, Default, Opts);
conversion_spec_device(_Spec, Default, _Opts) ->
    Default.

%% @doc Extend a structured->tabm source spec with the `bundle' flag and
%% `hint-device' implied by a hint spec, so the structured codec can decide
%% whether to load or offload children and can call the target codec's
%% `to_hint/3' callback at each node of the tree.
%%
%% `Spec' is the spec being extended (the source spec when converting).
%% `HintSpec' is the spec from which we should infer bundling
%% (target spec when converting).
add_bundle_hint(Spec, Opts) ->
    add_bundle_hint(Spec, Spec, Opts).
add_bundle_hint(Spec, HintSpec, Opts) ->
    WithBundle =
        case maps:is_key(<<"bundle">>, Spec) of
            true ->
                Spec;
            false ->
                case
                    is_map(HintSpec)
                        andalso hb_maps:find(<<"bundle">>, HintSpec, Opts)
                of
                    {ok, Bundle} -> Spec#{ <<"bundle">> => Bundle };
                    _ -> Spec
                end
        end,
    case maps:is_key(<<"hint-device">>, WithBundle) of
        true ->
            WithBundle;
        false ->
            case conversion_spec_device(HintSpec, undefined, Opts) of
                HintDevice when is_binary(HintDevice) ->
                    WithBundle#{ <<"hint-device">> => HintDevice };
                _ ->
                    WithBundle
            end
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
%% any existing `priv' sub-map that may already be present. A `priv' key that
%% contains a literal is not a sub-map, and is not restored.
restore_priv(Msg, OldPriv, _Opts)
        when not is_map(OldPriv); map_size(OldPriv) == 0 ->
    Msg;
restore_priv(Msg, OldPriv, Opts) ->
    MsgPriv = hb_maps:get(<<"priv">>, Msg, #{}, Opts),
    ?event_debug({restoring_priv, {priv_msg, MsgPriv}, {priv_old, OldPriv}}),
    NewPriv = hb_util:deep_merge(MsgPriv, OldPriv, Opts),
    ?event_debug({priv_new, NewPriv}),
    Msg#{ <<"priv">> => NewPriv }.

%% @doc Get a codec device and request params from the given conversion request. 
%% Expects conversion spec to either be a binary codec name, or a map with a
%% `device' key and other parameters. Additionally honors the `always_bundle'
%% key in the node message if present.
conversion_spec_to_req(Spec, Opts) when is_binary(Spec) orelse (Spec == tabm) ->
    conversion_spec_to_req(#{ <<"device">> => Spec }, Opts);
conversion_spec_to_req(Spec, Opts) ->
    try
        Device = conversion_spec_device(
            Spec,
            no_codec_device_in_conversion_spec,
            Opts
        ),
        {
            case Device of
                tabm -> tabm;
                _ ->
                    hb_device:message_to_device(
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
id(Msg, Spec, Opts) when is_map(Spec) ->
    % hb_ao:raw does not vary for us but ~message@1.0/commit|id expects a
    % ~structured@1.0 form, so we force an idempotent 'to' call to ensure it
    {ok, Structured} =
        hb_ao:raw(<<"structured@1.0">>, <<"to">>, Msg, #{}, Opts),
    {ok, ID} =
        hb_ao:raw(<<"message@1.0">>, <<"id">>, Structured, Spec, Opts),
    hb_util:human_id(ID);
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
    ?event_debug({getting_id, {msg, Msg}, {spec, CommSpec}}),
    id(Msg, CommSpec, Opts).

%% @doc Normalize loaded messages bottom-up. `verify' selects the commitments
%% to check: `all', `signed', `unsigned', `passive' (trust existing commitments),
%% or `fast' (all, unless unchanged since the last fast normalization). A false
%% verification removes that commitment; errors propagate. `add-unsigned' adds
%% an unsigned commitment independently if none remains. Defaults: passive, true,
%% `depth => deep'; shallow leaves children untouched. Private state and
%% commitment metadata are not traversed. An atom sets only the verify mode.
normalize_commitments(Msg, Opts) ->
    normalize_commitments(Msg, Opts, passive).
normalize_commitments(Msg, Opts, Mode) when is_atom(Mode) ->
    normalize_commitments(Msg, Opts, #{ <<"verify">> => Mode });
normalize_commitments(Msg, Opts, Settings) when is_map(Msg) ->
    Deep =
        case hb_maps:get(<<"depth">>, Settings, deep, Opts) of
            shallow -> Msg;
            deep -> maps:map(
                fun(Key, Value) ->
                    case Key == <<"commitments">> orelse
                            hb_private:is_private(Key) of
                        true -> Value;
                        false -> normalize_commitments(Value, Opts, Settings)
                    end
                end,
                Msg
            )
        end,
    normalize_message(Deep, Opts, Settings);
normalize_commitments(Values, Opts, Settings) when is_list(Values) ->
    case hb_maps:get(<<"depth">>, Settings, deep, Opts) of
        shallow -> Values;
        deep -> [ normalize_commitments(Value, Opts, Settings) || Value <- Values ]
    end;
normalize_commitments(Value, _Opts, _Settings) -> Value.

%% @doc Check existing commitments before optionally adding an unsigned one.
normalize_message(Msg, _Opts, _Settings) when ?IS_EMPTY_MESSAGE(Msg) -> Msg;
normalize_message(Msg, Opts, Settings = #{ <<"verify">> := fast }) ->
    Mode =
        case hb_private:get(<<"verified-commitments">>, Msg, not_found, Opts)
                == normalization_fast_hash(Msg) of
            true -> passive;
            false -> all
        end,
    Normalized = normalize_message(Msg, Opts, Settings#{ <<"verify">> => Mode }),
    hb_private:set(Normalized, <<"verified-commitments">>,
        normalization_fast_hash(Normalized), Opts);
normalize_message(Msg, Opts, Settings) ->
    Mode = hb_maps:get(<<"verify">>, Settings, passive, Opts),
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    Kept = hb_maps:filter(
        fun(ID, Commitment) ->
            Signed = hb_maps:is_key(<<"committer">>, Commitment, Opts),
            Selected =
                case Mode of
                    all -> true;
                    signed -> Signed;
                    unsigned -> not Signed;
                    passive -> false
                end,
            not Selected orelse verify(
                Msg#{
                    <<"commitments">> =>
                        #{
                            ID => hb_cache:ensure_all_loaded(Commitment, Opts)
                        }
                },
                #{ <<"ids">> => [ID] },
                Opts
            )
        end,
        Commitments,
        Opts
    ),
    Normalized =
        case hb_maps:size(Kept, Opts) of
            0 -> uncommitted(Msg, Opts);
            _ -> Msg#{ <<"commitments">> => Kept }
        end,
    case hb_maps:get(<<"add-unsigned">>, Settings, true, Opts) andalso
            not lists:any(
                fun(C) -> not hb_maps:is_key(<<"committer">>, C, Opts) end,
                hb_maps:values(Kept, Opts)
            ) of
        true ->
            Unsigned = commit(uncommitted(Normalized, Opts), Opts,
                #{ <<"type">> => <<"unsigned">> }),
            Unsigned#{ <<"commitments">> => hb_maps:merge(
                hb_maps:get(<<"commitments">>, Unsigned, #{}, Opts),
                Kept,
                Opts
            ) };
        false -> Normalized
    end.

%% @doc Fingerprint the complete public term, including attached commitments.
normalization_fast_hash(Msg) ->
    crypto:hash(sha256, term_to_binary(hb_private:reset(Msg))).

%% @doc Return a message with only the committed keys. `all', the default, keeps
%% the keys that every commitment covers; `any', the keys that any covers; a
%% list of IDs or a spec, those that `committed/3' gives. If no commitments are
%% present, the message is returned unchanged. This means that you need to
%% check if the message is:
%% - Committed
%% - Verifies
%% ...before using the output of this function as the 'canonical' message. This
%% is such that expensive operations like signature verification are not
%% performed unless necessary.
with_only_committed(Msg, Opts) ->
    with_only_committed(Msg, all, Opts).
with_only_committed(Msg, Mode, Opts) when is_atom(Mode) ->
    with_only_committed(Msg, #{ <<"ids">> => hb_util:bin(Mode) }, Opts);
with_only_committed(Msg, Spec, Opts) when is_map(Msg) ->
    ?event_debug({with_only_committed, {msg, Msg}, {opts, Opts}}),
    Comms = hb_maps:get(<<"commitments">>, Msg, not_found, Opts),
    case is_map(Msg) andalso Comms /= not_found of
        true ->
            try
                CommittedKeys =
                    hb_message:committed(
                        Msg,
                        Spec,
                        Opts
                    ),
                % Add the ao-body-key to the committed list if it is not
                % already present.
                ?event_debug(debug_bundle, {committed_keys, CommittedKeys, {msg, Msg}}),
                {ok,
                    hb_maps:with(
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
with_only_committed(Msg, _, _) ->
    % If the message is not a map, it cannot be signed.
    {ok, Msg}.

%% @doc Return a message with only the committed keys, as
%% `with_only_committed/2' does, and the same for each message inside it. A
%% commitment covers a child message by its ID, which the child's own
%% commitments give, so the child keeps only the keys that they cover. A child
%% that only bundled signed commitments cover is kept as it is.
with_only_committed_deep(Msg, Opts) when is_map(Msg) ->
    {ok, Committed} = with_only_committed(Msg, Opts),
    Bundled = bundled_keys(Committed, Opts),
    maps:map(
        fun(Key, Value) ->
            case lists:member(Key, [<<"commitments">> | Bundled]) of
                true -> Value;
                false -> with_only_committed_deep(Value, Opts)
            end
        end,
        Committed
    );
with_only_committed_deep(Values, Opts) when is_list(Values) ->
    [ with_only_committed_deep(Value, Opts) || Value <- Values ];
with_only_committed_deep(Value, _Opts) ->
    Value.

%% @doc Return the keys whose whole values a message's signed commitments
%% cover: the keys of its bundled signed commitments, except those that a
%% signed commitment that is not bundled also covers, as it covers only an ID.
bundled_keys(Msg, Opts) ->
    Signed =
        [
            {
                hb_util:bool(maps:get(<<"bundle">>, Commitment, false)),
                committed(Msg, [ID], Opts)
            }
        ||
            {ID, Commitment = #{ <<"committer">> := _ }} <-
                maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, Opts))
        ],
    Linked = lists:append([ Keys || {false, Keys} <- Signed ]),
    [
        Key
    ||
        {true, Keys} <- Signed, Key <- Keys, not lists:member(Key, Linked)
    ].

%% @doc Return the message with only the specified committers attached.
with_only_committers(Msg, Committers) ->
    with_only_committers(Msg, Committers, #{}).
with_only_committers(Msg, RawCommitters, Opts) when is_map(Msg) ->
    Committers = [hb_committer:uri(C, Opts) || C <- RawCommitters],
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

%% @doc Sign a message with the `priv-wallet' of `Opts', else of the node.
commit(Msg, Opts) ->
    commit(
        Msg,
        Opts,
        hb_opts:get(
            commitment_device,
            no_viable_commitment_device,
            Opts
        )
    ).
commit(Msg, NotOpts, CodecName) when not is_map(NotOpts) ->
    ?event(error, {deprecated_commit_call, {msg, Msg}, {opts, NotOpts}, {codec, CodecName}}),
    error({deprecated_commit_call, {arg_must_be_node_msg, NotOpts}});
commit(Msg, Opts, CodecName) when is_binary(CodecName) ->
    commit(Msg, Opts, #{ <<"commitment-device">> => CodecName });
commit(Msg, GivenOpts, Spec) ->
    % Only a signed commitment takes the node's key material.
    Opts =
        case Spec of
            #{ <<"type">> := <<"unsigned">> } -> GivenOpts;
            _ -> hb_http_server:with_node_committer(GivenOpts)
        end,
    % hb_ao:raw does not vary for us but ~message@1.0/commit|id expects a
    % ~structured@1.0 form, so we force an idempotent 'to' call to ensure it
    {ok, Structured} =
        hb_ao:raw(<<"structured@1.0">>, <<"to">>, Msg, #{}, Opts),
    {ok, Signed} =
        hb_ao:raw(
            <<"message@1.0">>,
            <<"commit">>,
            Structured,
            Spec#{
                <<"commitment-device">> =>
                    case hb_maps:get(<<"commitment-device">>, Spec, none, Opts) of
                        none ->
                            case hb_maps:get(<<"device">>, Spec, none, Opts) of
                                none ->
                                    FromOpts =
                                        hb_opts:get(
                                            commitment_device,
                                            no_viable_commitment_device,
                                            Opts
                                        ),
                                    case FromOpts of
                                        no_viable_commitment_device ->
                                            throw(
                                                {unset_commitment_device, Spec}
                                            );
                                        Device -> Device
                                    end;
                                Device -> Device
                            end;
                        CommitmentDevice -> CommitmentDevice
                    end
            },
            Opts
        ),
    Signed.

%% @doc Add a committer to a commitment request, if it does not have one. The
%% committer is the `priv-wallet' in `Opts'. Requests for unsigned commitments
%% are returned unchanged.
with_committer(Spec = #{ <<"committer">> := _ }, _Opts) -> Spec;
with_committer(Spec = #{ <<"type">> := <<"unsigned">> }, _Opts) -> Spec;
with_committer(Spec, Opts) ->
    case hb_opts:get(priv_wallet, no_viable_wallet, Opts) of
        no_viable_wallet -> Spec;
        Wallet -> Spec#{ <<"committer">> => Wallet }
    end.

%% @doc Return the list of committed keys from a message.
committed(Msg, all, Opts) ->
    committed(Msg, #{ <<"committers">> => <<"all">> }, Opts);
committed(Msg, none, Opts) ->
    committed(Msg, #{ <<"committers">> => <<"none">> }, Opts);
committed(Msg, List, Opts) when is_list(List) ->
    committed(Msg, #{ <<"ids">> => List }, Opts);
committed(Msg, CommittersMsg, Opts) ->
    ?event(
        {committed,
            {msg, {explicit, Msg}},
            {committers_msg, {explicit, CommittersMsg}},
            {opts, Opts}
        }
    ),
    {ok, CommittedKeys} =
        hb_ao:raw(
            <<"message@1.0">>,
            <<"committed">>,
            Msg,
            CommittersMsg,
            Opts
        ),
    CommittedKeys.

%% @doc wrapper function to verify a message.
verify(Msg) -> verify(Msg, all).
verify(Msg, Committers) ->
    verify(Msg, Committers, #{}).
verify(Msg, all, Opts) ->
    verify(Msg, #{ <<"ids">> => <<"all">> }, Opts);
verify(Msg, none, Opts) ->
    verify(Msg, <<"none">>, Opts);
verify(Msg, signers, Opts) ->
    verify(Msg, #{ <<"committers">> => <<"all">> }, Opts);
verify(Msg, Committers, Opts) when not is_map(Committers) ->
    verify(Msg, #{ <<"committers">> => Committers }, Opts);
verify(Msg, Spec, Opts) ->
    ?event(verify, {verify, {spec, Spec}}),
    {ok, Res} =
        hb_ao:raw(<<"message@1.0">>, <<"verify">>, Msg, Spec, Opts),
    Res.

%% @doc Verify a message and its loaded children, without traversing links.
paranoid_verify(Msg, Opts) ->
    paranoid_verify(default, Msg, Opts).
paranoid_verify(Topic, Msg, Opts) ->
    % Check the `paranoid_verify' flag before any other work: in the default,
    % disabled configuration (`false' or `[]') we short-circuit to `true'
    % without emitting an event or walking a topic list. This path fires
    % twice per `hb_ao:resolve/3', so the event/`lists:member' overhead is
    % noticeable on the hot path.
    case hb_opts:get(paranoid_verify, false, Opts) of
        false -> true;
        [] -> true;
        true ->
            ?event_debug(debug_paranoia, {paranoid_verify_called, Msg}, Opts),
            do_paranoid_verify(Topic, Msg, Opts);
        Topics ->
            case lists:member(Topic, Topics) of
                false -> true;
                true ->
                    ?event_debug(debug_paranoia, {paranoid_verify_called, Msg}, Opts),
                    do_paranoid_verify(Topic, Msg, Opts)
            end
    end.

do_paranoid_verify(Topic, Msg, Opts) ->
    maybe
        true ?= deep_verify(Msg, Opts),
        ?event_debug(debug_paranoia, {paranoid_verify_complete, ok}, Opts),
        true
    else
        {false, Path, FailedMsg} ->
            ?event(error,
                {paranoid_verification_failure,
                    {triggered_by, Topic},
                    {at_path, Path},
                    {failed_message, FailedMsg},
                    {while_verifying, Msg}
                },
                Opts#{ <<"paranoid-verify">> => false }
            ),
            throw({paranoid_verification_failure, Topic, Path, Msg, FailedMsg})
    end.

%% @doc Verify all commitments of a message and its loaded children, without
%% traversing links. Return `true' or `{false, Path, MsgAtFault}'.
deep_verify(Msg, Opts) ->
    deep_verify([], Msg, Opts).
deep_verify(Path, {_Status, Msg}, Opts) ->
    deep_verify(Path, Msg, Opts);
deep_verify(Path, ListMsg, Opts) when is_list(ListMsg) ->
    deep_verify(Path, hb_util:list_to_numbered_message(ListMsg), Opts);
deep_verify(Path, Msg, Opts) when is_map(Msg) ->
    maybe
        true ?=
            maps:fold(
                fun(Key, Value, true) ->
                    deep_verify([Key | Path], Value, Opts);
                   (_, _, Failure) -> Failure
                end,
                true,
                % `verify/3' reads `Msg' with its private keys; the fold skips them.
                maps:filter(
                    fun(Key, _) -> not hb_private:is_private(Key) end,
                    uncommitted(Msg, Opts)
                )
            ),
        Verified =
            try verify(Msg, #{ <<"ids">> => <<"all">> }, Opts)
            catch _:_ -> false
            end,
        case Verified of
            true -> true;
            false -> {false, hb_path:to_binary(lists:reverse(Path)), Msg}
        end
    end;
deep_verify(_Path, _Msg, _Opts) ->
    true.

%% @doc Return the unsigned version of a message in AO-Core format.
uncommitted(Msg) -> uncommitted(Msg, #{}).
uncommitted(Bin, _Opts) when is_binary(Bin) -> Bin;
uncommitted(Msg, Opts) ->
    hb_maps:remove(<<"commitments">>, Msg, Opts).

%% @doc Recursively remove commitments from a message.
uncommitted_deep(Msg, Opts) ->
    % Remove commitments at the current level
    MsgWithoutCommitments = hb_maps:remove(<<"commitments">>, Msg, Opts),
    % Recursively remove commitments from nested maps
    maps:map(
        fun(_Key, Value) when is_map(Value) ->
            uncommitted_deep(Value, Opts);
           (_Key, Value) when is_list(Value) ->
            lists:map(
                fun(Item) when is_map(Item) -> uncommitted_deep(Item, Opts);
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
    hb_util:ok(
        hb_ao:raw(<<"message@1.0">>, <<"committers">>, Msg, #{}, Opts)
    ).

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
    try
        unsafe_match(hb_ao:normalize_keys(Map1, Opts), Map2, Mode, [], Opts)
    catch
        throw:{mismatch, Type, Path, Val1, Val2} ->
            {mismatch, Type, Path, Val1, Val2};
        _:Details:St -> {error, {Details, {trace, St}}}
    end.

%% @doc Match two maps, returning `true' if they match, or throwing an error
%% if they do not.
unsafe_match(#{ <<"match-type">> := Type, <<"body">> := Inner } = RawMap1,
        RawMap2, _Mode, Path, Opts) when map_size(RawMap1) == 2 ->
    case catch hb_util:key_to_atom(Type) of
        Mode when Mode == strict; Mode == primary; Mode == only_present ->
            unsafe_match(
                hb_ao:normalize_keys(Inner, Opts),
                hb_ao:normalize_keys(RawMap2, Opts),
                Mode,
                Path,
                Opts
            );
        _ -> throw(invalid_match_type)
    end;
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
            NormMap1 = minimize(hb_ao:normalize_keys(Map1, Opts))
        ),
    Keys2 =
        hb_maps:keys(
            NormMap2 = minimize(hb_ao:normalize_keys(Map2, Opts))
        ),
    PrimaryKeysPresent =
        (Mode == primary) andalso
            lists:all(
                fun(Key) -> lists:member(Key, Keys1) end,
                Keys1
            ),
    ?event_debug(debug_match,
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
                    ?event_debug(debug_match, {matching_key, Key}),
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
    ?event_debug({with_commitments, {spec, Spec}, {commitments, Commitments}}),
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
    ?event_debug({with_commitments, {filtered_commitments, FilteredCommitments}}),
    Msg#{ <<"commitments">> => FilteredCommitments };
with_commitments(_Spec, Msg, _Opts) ->
    Msg.

%% @doc Filter messages that match the 'spec' given. Inverts the `with_commitments/2'
%% function, such that only messages that do _not_ match the spec are returned.
without_commitments(Spec, Msg = #{ <<"commitments">> := Commitments }, Opts) ->
    ?event_debug({without_commitments, {spec, Spec}, {msg, Msg}, {commitments, Commitments}}),
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
            Commitments,
            Opts
        ),
    ?event_debug({without_commitments, {filtered_commitments, FilteredCommitments}}),
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
commitment(Spec, Msg, Opts) ->
    Matches = commitments(Spec, Msg, Opts),
    ?event_debug(debug_commitment, {commitment, {spec, Spec}, {matches, Matches}}),
    if
        map_size(Matches) == 0 -> not_found;
        map_size(Matches) == 1 ->
            CommID = hd(hb_maps:keys(Matches)),
            {ok, CommID, hb_util:ok(hb_maps:find(CommID, Matches, Opts))};
        true ->
            ?event(commitment, {multiple_matches, {matches, Matches}}),
            multiple_matches
    end.

%% @doc Return a list of all commitments that match the spec.
commitments(ID, Link, Opts) when ?IS_LINK(Link) ->
    commitments(ID, hb_cache:ensure_loaded(Link, Opts), Opts);
commitments(CommitterID, Msg, Opts) when is_binary(CommitterID) ->
    Committer = hb_committer:uri(CommitterID, Opts),
    commitments(#{ <<"committer">> => Committer }, Msg, Opts);
commitments(Spec = #{ <<"type">> := <<"unsigned">> }, Msg, Opts) ->
    % Unsigned is a special case: Match all of the commitments with a committer
    % first, remove them, then call `commitments` again with the remainder of
    % the spec.
    commitments(
        maps:without([<<"type">>], Spec),
        without_commitments(
            #{ <<"committer">> => '_' },
            Msg,
            Opts
        ),
        Opts
    );
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
            hb_maps:get(<<"commitment-device">>, CommMsg, not_found, Opts)
        end,
        maps:values(Commitments)
    );
commitment_devices(_Msg, _Opts) ->
    [].

%% @doc Return the message that an operation acts on. The value of `SubjectKey'
%% in the request is the key of the request that contains the message. If that
%% value is `self', or the request has no `SubjectKey', the message is `Self'.
%% A key that the request lacks gives `{error, not_found}'.
find_subject(SubjectKey, Self, Req, Opts) ->
	GetOpts = Opts#{
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
    },
    case hb_maps:get(SubjectKey, Req, <<"self">>, GetOpts) of
        <<"self">> -> {ok, Self};
        Key ->
            case hb_private:is_private(Key) of
                true -> {error, not_found};
                false ->
                    case hb_maps:find(Key, Req, GetOpts) of
                        {ok, Target} -> {ok, Target};
                        error -> {error, not_found}
                    end
            end
    end.

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

%%% Tests

%% @doc Selective verification retains valid partial commitments and checks
%% every selected ID, including multiple unsigned commitments.
normalize_commitments_modes_test() ->
    Opts = #{ <<"priv-wallet">> => ar_wallet:new() },
    Msg = #{ <<"a">> => 1, <<"b">> => 1 },
    [SignedA, SignedB, UnsignedA, UnsignedB] =
        [ commit(Msg, Opts, #{ <<"type">> => Type, <<"committed">> => [Key] })
        || Type <- [<<"signed">>, <<"unsigned">>], Key <- [<<"a">>, <<"b">>] ],
    % An unsigned commitment needs no listener for the `http-server' of `Opts'.
    ?assertEqual(UnsignedA, commit(Msg, #{ <<"http-server">> => <<"stopped">> },
        #{ <<"type">> => <<"unsigned">>, <<"committed">> => [<<"a">>] })),
    Commitments = lists:foldl(
        fun(M, Acc) -> maps:merge(Acc, maps:get(<<"commitments">>, M)) end,
        #{},
        [SignedA, SignedB, UnsignedA, UnsignedB]
    ),
    Changed = Msg#{ <<"b">> => 2, <<"commitments">> => Commitments },
    lists:foreach(
        fun({Mode, Retained}) ->
            Normalized = normalize_commitments(Changed, Opts,
                #{ <<"verify">> => Mode, <<"add-unsigned">> => false }),
            Expected = lists:sort(lists:append(
                [ maps:keys(maps:get(<<"commitments">>, M)) || M <- Retained ])),
            ?assertEqual(Expected,
                lists:sort(maps:keys(maps:get(<<"commitments">>, Normalized)))),
            ?assertEqual(Changed#{ <<"commitments">> =>
                maps:get(<<"commitments">>, Normalized) }, hb_private:reset(Normalized))
        end,
        [{passive, [SignedA, SignedB, UnsignedA, UnsignedB]},
         {signed, [SignedA, UnsignedA, UnsignedB]},
         {unsigned, [SignedA, SignedB, UnsignedA]},
         {all, [SignedA, UnsignedA]}, {fast, [SignedA, UnsignedA]}]
    ),
    % Adding an unsigned commitment covers all fields, not just signed fields.
    Added = normalize_commitments(SignedA#{ <<"extra">> => 3 }, Opts, all),
    ?assertEqual(id(#{ <<"a">> => 1, <<"b">> => 1, <<"extra">> => 3 }, unsigned, Opts),
        id(Added, unsigned, Opts)),
    ?assert(verify(Added, #{ <<"ids">> => <<"all">> }, Opts)),
    ?assertEqual(SignedA, normalize_commitments(SignedA, Opts,
        #{ <<"verify">> => all, <<"add-unsigned">> => false })),
    % A missing key or a renamed ID cannot survive unsigned verification.
    [UnsignedID] = maps:keys(maps:get(<<"commitments">>, UnsignedA)),
    Renamed = UnsignedA#{ <<"commitments">> => #{
        hb_util:human_id(crypto:strong_rand_bytes(32)) =>
            maps:get(UnsignedID, maps:get(<<"commitments">>, UnsignedA)) } },
    lists:foreach(fun(M) ->
        ?assertEqual(uncommitted(M), normalize_commitments(M, Opts,
            #{ <<"verify">> => unsigned, <<"add-unsigned">> => false }))
    end, [maps:remove(<<"a">>, UnsignedA), Renamed]).

%% @doc Fast verification rechecks mutations and still honors addition. Deep
%% normalization handles map/list children without introducing metadata when
%% addition is disabled; shallow normalization preserves their exact shape.
normalize_commitments_depth_and_fast_test() ->
    Opts = #{ <<"priv-wallet">> => ar_wallet:new() },
    Signed = commit(#{ <<"a">> => 1 }, Opts),
    Settings = #{ <<"verify">> => fast, <<"add-unsigned">> => false },
    Stamped = normalize_commitments(Signed, Opts, Settings),
    ?assertEqual(Stamped, normalize_commitments(Stamped, Opts, Settings)),
    Added = normalize_commitments(Stamped, Opts, fast),
    ?assertEqual(id(#{ <<"a">> => 1 }, unsigned, Opts), id(Added, unsigned, Opts)),
    Changed = Stamped#{ <<"a">> => 2 },
    ?assertEqual(#{ <<"a">> => 2 },
        hb_private:reset(normalize_commitments(Changed, Opts, Settings))),
    Partial = normalize_commitments(Changed, Opts,
        #{ <<"verify">> => unsigned, <<"add-unsigned">> => false }),
    ?assertEqual(#{ <<"a">> => 2 },
        hb_private:reset(normalize_commitments(Partial, Opts, Settings))),
    Nested = #{ <<"children">> => [Changed], <<"plain">> => #{ <<"x">> => 1 } },
    ?assertEqual(Nested, maps:remove(<<"priv">>, normalize_commitments(Nested, Opts,
        Settings#{ <<"depth">> => shallow }))),
    ?assertEqual([Changed], normalize_commitments([Changed], Opts,
        Settings#{ <<"depth">> => shallow })),
    Deep = normalize_commitments(Nested, Opts,
        #{ <<"verify">> => all, <<"add-unsigned">> => false }),
    ?assertEqual(#{ <<"children">> => [#{ <<"a">> => 2 }],
        <<"plain">> => #{ <<"x">> => 1 } }, hb_private:reset(Deep)),
    ?assert(deep_verify(normalize_commitments(Nested, Opts, all), Opts)),
    % `id' and `commit' run `to' first, so they are over lowercase keys.
    ?assertEqual(
        id(#{ <<"a">> => 1 }, all, Opts),
        id(#{ <<"A">> => 1 }, all, Opts)
    ),
    ?assertEqual(
        #{ <<"a">> => 1 },
        uncommitted(commit(#{ <<"A">> => 1 }, Opts))
    ).

%% @doc Commitments should never generally be unloaded, but `hb_message` chooses
%% to be tolerant of issues here regardless.
normalize_linked_commitments_test() ->
    Store = hb_test_utils:test_store(hb_store_volatile),
    ok = hb_store:start(Store),
    Opts = #{ <<"store">> => [Store] },
    Msg = normalize_commitments(#{ <<"a">> => 1 }, Opts),
    Commitments = maps:get(<<"commitments">>, Msg),
    [{ID, Commitment}] = maps:to_list(Commitments),
    {ok, MetadataID} = hb_cache:write(Commitment, Opts),
    {ok, CommitmentsID} = hb_cache:write(Commitments, Opts),
    lists:foreach(
        fun(Linked) ->
            Normalized = normalize_commitments(Msg#{ <<"commitments">> => Linked }, Opts,
                #{ <<"verify">> => all, <<"add-unsigned">> => false }),
            ?assertEqual(Msg, hb_cache:ensure_all_loaded(Normalized, Opts))
        end,
        [#{ ID => {link, MetadataID, #{}} }, {link, CommitmentsID, #{}}]
    ),
    ok = hb_store:stop(Store).
