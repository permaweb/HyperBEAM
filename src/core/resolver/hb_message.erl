
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
-export([verify/1, verify/2, verify/3, verify_all_signed_commitments/2]).
-export([paranoid_verify/2, paranoid_verify/3]).
-export([commit/2, commit/3, signers/2, type/1, minimize/1]).
-export([normalize_commitments/2, normalize_commitments/3, is_signed_key/3]).
-export([commitment/2, commitment/3, commitments/3]).
-export([with_only_committed/2, without_unless_signed/3]).
-export([with_commitments/3, without_commitments/3, uncommitted_deep/2]).
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
    SourceOpts = bundle_target_source_opts(TargetFormat, Opts),
    TABM =
        to_tabm(
            case is_map(Msg) of
                true -> hb_maps:without([<<"priv">>], Msg, SourceOpts);
                false -> Msg
            end,
            TargetFormat,
            SourceFormat,
            SourceOpts
        ),
    case TargetFormat of
        tabm -> restore_priv(TABM, OldPriv, Opts);
        _ -> from_tabm(TABM, TargetFormat, OldPriv, Opts)
    end.

bundle_target_source_opts(TargetFormat, Opts) when is_map(TargetFormat) ->
    case hb_util:atom(hb_maps:get(<<"bundle">>, TargetFormat, false, Opts)) of
        true -> Opts#{ <<"linkify-mode">> => false };
        false -> Opts
    end;
bundle_target_source_opts(_TargetFormat, Opts) ->
    Opts.

to_tabm(RawMsg, TargetFormat, SourceFormat, Opts) ->
    {SourceCodecMod, Params0} = conversion_spec_to_req(SourceFormat, Opts),
    Params = add_bundle_hint(Params0, TargetFormat, Opts),
    % Flatten any message extension (`...') to its concrete content before
    % serialization: a message's normal serialized form is its flattened form.
    % Commitment generation/verification may explicitly preserve the extension
    % edge in order to attest to the derivation itself.
    Msg =
        case is_map(RawMsg) of
            true ->
                case hb_opts:get(<<"preserve-message-extension">>, false, Opts) of
                    true -> RawMsg;
                    false -> hb_maps:flatten(RawMsg, Opts)
                end;
            false -> RawMsg
        end,
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
%% any existing `priv' sub-map that may already be present.
restore_priv(Msg, EmptyPriv, _Opts) when map_size(EmptyPriv) == 0 -> Msg;
restore_priv(Msg, OldPriv, Opts) ->
    MsgPriv = maps:get(<<"priv">>, Msg, #{}),
    ?event({restoring_priv, {priv_msg, MsgPriv}, {priv_old, OldPriv}}),
    NewPriv = hb_util:deep_merge(MsgPriv, OldPriv, Opts),
    ?event({priv_new, NewPriv}),
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
    {ok, ID} =
        hb_ao:raw(<<"message@1.0">>, <<"id">>, Msg, Spec, Opts),
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
    ?event({getting_id, {msg, Msg}, {spec, CommSpec}}),
    {ok, ID} =
        hb_ao:raw(<<"message@1.0">>, <<"id">>, Msg, CommSpec, Opts),
    hb_util:human_id(ID).

%% @doc Normalize the IDs in a message, ensuring that there is at least one
%% unsigned ID present. By forcing this work to occur in strategically positioned
%% places, we avoid the need to recalculate the IDs for every `hb_message:id`
%% call.
normalize_commitments(Msg, Opts) ->
    normalize_commitments(Msg, Opts, passive).
normalize_commitments(RawMsg, Opts, Mode) when is_map(RawMsg) ->
    ?event(debug_normalize_commitments, {normalize_commitments, {msg, RawMsg}}),
    % Preserve message extensions for commitment normalization: the child layer
    % can then carry an unsigned commitment over its own keys and the `...' edge
    % to its parent. A concrete/flattened view is still used by callers that ask
    % for concrete message equality or non-commitment serialization.
	Msg = normalize_commitment_view(RawMsg, Opts),
	ParentHasCommitmentMaterial = has_commitment_material(Msg, Opts),
	NormMsg =
		maps:map(
			fun(Key, Val) when Key == <<"commitments">> orelse Key == <<"priv">> ->
				Val;
			   (_Key, Val) ->
				normalize_child_commitments(
					Val,
					Opts,
					Mode,
					ParentHasCommitmentMaterial
				)
			end,
			Msg
		),
	case is_uncommitted_list_container(NormMsg, Opts) of
		true -> NormMsg;
		false -> do_normalize_commitments(NormMsg, Opts, Mode)
	end;
normalize_commitments(Msg, Opts, Mode) when is_list(Msg) ->
    ?event(debug_normalize_commitments, {normalize_commitments, {list, Msg}}),
    lists:map(fun(X) -> normalize_commitments(X, Opts, Mode) end, Msg);
normalize_commitments(Msg, _Opts, _Mode) ->
	Msg.

normalize_child_commitments(Val, Opts, Mode, ParentHasCommitmentMaterial)
		when is_map(Val) ->
	case (not ParentHasCommitmentMaterial) orelse has_commitment_material(Val, Opts) of
		true -> normalize_commitments(Val, Opts, Mode);
		false -> Val
	end;
normalize_child_commitments(Val, Opts, Mode, ParentHasCommitmentMaterial)
		when is_list(Val) ->
	lists:map(
		fun(X) ->
			normalize_child_commitments(X, Opts, Mode, ParentHasCommitmentMaterial)
		end,
		Val
	);
normalize_child_commitments(Val, _Opts, _Mode, _ParentHasCommitmentMaterial) ->
	Val.

has_commitment_material(Msg, Opts) ->
	has_message_extension(Msg)
		orelse
			case hb_maps:get(<<"commitments">>, Msg, #{}, Opts) of
				Commitments when is_map(Commitments) -> map_size(Commitments) > 0;
				_ -> true
			end.

do_normalize_commitments(Msg, _Opts, _Mode) when ?IS_EMPTY_MESSAGE(Msg) ->
	Msg;
do_normalize_commitments(Msg, Opts, passive) ->
    Commitments = filter_commitments_for_visible_keys(
        Msg,
        maps:get(<<"commitments">>, Msg, #{}),
        Opts
    ),
    MsgWithVisibleCommitments = Msg#{ <<"commitments">> => Commitments },
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
    case needs_unsigned_commitment(Msg, UnsignedCommitments, Opts) of
        true ->
            {ok, #{ <<"commitments">> := NewCommitments }} =
                hb_ao:raw(
                    <<"message@1.0">>,
                    <<"commit">>,
                    uncommitted(MsgWithVisibleCommitments),
                    unsigned_commit_request(MsgWithVisibleCommitments, #{}, Opts),
                    unsigned_commit_opts(MsgWithVisibleCommitments, Opts)
                ),
            MergedCommitments = hb_maps:merge(
                NewCommitments,
                Commitments,
                Opts
                ),
            MsgWithVisibleCommitments#{ <<"commitments">> => MergedCommitments };
        false ->
            MsgWithVisibleCommitments
    end;
do_normalize_commitments(Msg, Opts, verify) ->
    UnsignedCommitments = unsigned_commitments(Msg, Opts),
    {MaybeUnsignedID, MaybeCommittedSpec} =
        case UnsignedCommitments of
            [{ID, #{ <<"committed">> := Committed }}] ->
                {ID, #{ <<"committed">> => Committed }};
            [] -> {undefined, #{}};
            _ -> {multiple_matches, #{}}
        end,
    {ok, #{ <<"commitments">> := NormCommitments }} =
                hb_ao:raw(
                    <<"message@1.0">>,
                    <<"commit">>,
                    uncommitted(Msg),
                    unsigned_commit_request(Msg, MaybeCommittedSpec, Opts),
                    unsigned_commit_opts(Msg, Opts)
                ),
    ?event(normalization, {normalizing_commitments, verify}),
    NormIDs = hb_maps:keys(NormCommitments, Opts),
    case {MaybeUnsignedID, lists:member(MaybeUnsignedID, NormIDs)} of
        {multiple_matches, _} ->
            case unsigned_commitments_match(Msg, UnsignedCommitments, Opts) of
                true -> Msg;
                false ->
                    {ok, #{ <<"commitments">> := NewCommitments }} =
                        hb_ao:raw(
                            <<"message@1.0">>,
                            <<"commit">>,
                            uncommitted(Msg),
                            unsigned_commit_request(Msg, #{}, Opts),
                            unsigned_commit_opts(Msg, Opts)
                        ),
                    refresh_unsigned_commitments(Msg, NewCommitments, Opts)
            end;
        {MatchedID, true} when MatchedID =/= undefined ->
            Msg;
        {undefined, _} ->
            % We did not have an unsigned ID to begin with, so we need to add it.
            attach_phash2(
                Msg#{
                    <<"commitments">> =>
                        hb_maps:merge(
                            NormCommitments,
                            maps:get(<<"commitments">>, Msg, #{})
                        )
                },
                Opts
            );
        {_OldID, _} ->
            {ok, #{ <<"commitments">> := NewCommitments }} =
                hb_ao:raw(
                    <<"message@1.0">>,
                    <<"commit">>,
                    uncommitted(Msg),
                    unsigned_commit_request(Msg, #{}, Opts),
                    unsigned_commit_opts(Msg, Opts)
                ),
            refresh_unsigned_commitments(Msg, NewCommitments, Opts)
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

refresh_unsigned_commitments(Msg, NewCommitments, Opts) ->
    attach_phash2(
        Msg#{
            <<"commitments">> =>
                hb_maps:merge(
                    NewCommitments,
                    valid_signed_commitments(Msg, Opts),
                    Opts
                )
        },
        Opts
    ).

valid_signed_commitments(Msg, Opts) ->
    case verify_all_signed_commitments(Msg, Opts) of
        true -> signed_commitments(Msg, Opts);
        false -> valid_individual_signed_commitments(Msg, Opts)
    end.

valid_individual_signed_commitments(Msg, Opts) ->
    hb_maps:filter(
        fun(ID, Commitment) ->
            case hb_maps:get(<<"committer">>, Commitment, undefined, Opts) of
                undefined -> false;
                _ -> verify_commitment(ID, Msg, Opts)
            end
        end,
        maps:get(<<"commitments">>, Msg, #{}),
        Opts
    ).

signed_commitments(Msg, Opts) ->
    hb_maps:filter(
        fun(_ID, Commitment) ->
            hb_maps:get(<<"committer">>, Commitment, undefined, Opts) =/= undefined
        end,
        maps:get(<<"commitments">>, Msg, #{}),
        Opts
    ).

verify_all_signed_commitments(Msg, Opts) ->
    try verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, Opts) of
        true -> true;
        false -> verify_each_signed_commitment(Msg, Opts)
    catch
        _:{multiple_ans104_commitments_unsupported, _}:_ ->
            verify_each_signed_commitment(Msg, Opts);
        _:_ ->
            false
    end.

verify_each_signed_commitment(Msg, Opts) ->
    case signed_commitments(Msg, Opts) of
        Commitments when is_map(Commitments), map_size(Commitments) > 0 ->
            lists:all(
                fun(ID) -> verify_commitment(ID, Msg, Opts) end,
                hb_maps:keys(Commitments, Opts)
            );
        _ ->
            false
    end.

verify_commitment(ID, Msg, Opts) ->
    try verify(Msg, #{ <<"commitment-ids">> => [ID] }, Opts)
    catch _:_ -> false
    end.

unsigned_commit_opts(Msg, Opts) ->
    case needs_loaded_bundle_view(Msg, Opts) of
        true -> Opts#{ <<"linkify-mode">> => false };
        false -> Opts
    end.

unsigned_commit_request(Msg, Spec, Opts) ->
    Req = Spec#{ <<"type">> => <<"unsigned">> },
    case needs_loaded_bundle_view(Msg, Opts) of
        true -> Req#{ <<"linkify-mode">> => false };
        false -> Req
    end.

needs_loaded_bundle_view(Msg, Opts) ->
    has_bundle_commitment_deep(Msg, Opts)
        andalso (
            not has_bundle_commitment(Msg, Opts)
            orelse has_original_tagged_bundle_commitment(Msg, Opts)
        ).

has_original_tagged_bundle_commitment(Msg, Opts) when is_map(Msg) ->
    lists:any(
        fun({_ID, Commitment}) ->
            hb_util:atom(hb_maps:get(<<"bundle">>, Commitment, false, Opts))
                andalso
                    hb_maps:get(
                        <<"original-tags">>,
                        Commitment,
                        undefined,
                        Opts
                    ) =/= undefined
        end,
        maps:to_list(maps:get(<<"commitments">>, Msg, #{}))
    );
has_original_tagged_bundle_commitment(_Msg, _Opts) ->
    false.

has_bundle_commitment_deep(Msg, Opts) when is_map(Msg) ->
    has_bundle_commitment(Msg, Opts)
        orelse lists:any(
            fun({_Key, Value}) -> has_bundle_commitment_deep(Value, Opts) end,
            maps:to_list(maps:without([<<"commitments">>, <<"priv">>], Msg))
        );
has_bundle_commitment_deep(List, Opts) when is_list(List) ->
    lists:any(fun(Value) -> has_bundle_commitment_deep(Value, Opts) end, List);
has_bundle_commitment_deep(_Msg, _Opts) ->
    false.

has_bundle_commitment(Msg, Opts) when is_map(Msg) ->
    lists:any(
        fun({_ID, Commitment}) ->
            hb_util:atom(hb_maps:get(<<"bundle">>, Commitment, false, Opts))
        end,
        maps:to_list(maps:get(<<"commitments">>, Msg, #{}))
    );
has_bundle_commitment(_Msg, _Opts) ->
    false.

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
with_only_committed(RawMsg, Opts) when is_map(RawMsg) ->
    % If the top layer has its own commitment over the extension edge, filter the
    % layered structure. Otherwise collapse to the inherited concrete content.
    Msg = committed_filter_view(RawMsg, Opts),
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
                OnlyCommitted =
                    with_links(
                        [<<"commitments">> | CommittedKeys],
                        Msg,
                        Opts
                    ),
                VisibleCommitments =
                    filter_commitments_for_visible_keys(
                        OnlyCommitted,
                        hb_maps:get(<<"commitments">>, OnlyCommitted, #{}, Opts),
                        Opts
                    ),
                {ok, OnlyCommitted#{ <<"commitments">> => VisibleCommitments }}
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

normalize_commitment_view(Msg, Opts) ->
    case has_message_extension(Msg) of
        true -> Msg;
        false -> hb_maps:flatten(Msg, Opts)
    end.

committed_filter_view(Msg, Opts) ->
    case has_extension_commitment(Msg, Opts) of
        true -> Msg;
        false -> hb_maps:flatten(Msg, Opts)
    end.

has_extension_commitment(Msg, Opts) ->
    Commitments = maps:get(<<"commitments">>, Msg, #{}),
    lists:any(
        fun({_ID, Commitment}) ->
            lists:member(
                <<"...">>,
                committed_keys_for_commitment(Commitment, Opts)
            )
        end,
        hb_maps:to_list(Commitments, Opts)
    ).

has_message_extension(Msg) when is_map(Msg) ->
	maps:is_key(<<"...">>, Msg) orelse maps:is_key(<<"...+link">>, Msg);
has_message_extension(_Msg) ->
	false.

is_uncommitted_list_container(Msg, Opts) ->
	(not has_message_extension(Msg))
		andalso hb_util:is_ordered_list(Msg, Opts)
		andalso
			case hb_maps:get(<<"commitments">>, Msg, #{}, Opts) of
				Commitments when is_map(Commitments) -> map_size(Commitments) == 0;
				_ -> false
			end.

filter_commitments_for_visible_keys(_Msg, Commitments, _Opts)
		when map_size(Commitments) == 0 ->
	Commitments;
filter_commitments_for_visible_keys(Msg, Commitments, Opts) ->
    hb_maps:filter(
        fun(_ID, Commitment) ->
            lists:all(
                fun(Key) -> has_committed_key_view(Key, Msg, Opts)
                end,
                committed_keys_for_commitment(Commitment, Opts)
            )
        end,
        Commitments,
        Opts
    ).

filter_message_commitments_for_visible_keys(Msg, Opts) ->
    case hb_maps:get(<<"commitments">>, Msg, undefined, Opts) of
        Commitments when is_map(Commitments) ->
            Msg#{
                <<"commitments">> =>
                    filter_commitments_for_visible_keys(Msg, Commitments, Opts)
            };
        _ ->
            Msg
    end.

has_committed_key_view(<<"...">>, Msg, _Opts) ->
    has_message_extension(Msg);
has_committed_key_view(<<"ao-types">>, Msg, Opts) when is_map(Msg) ->
    hb_maps:is_key(<<"ao-types">>, Msg, Opts)
        orelse has_implicit_ao_types(Msg, Opts);
has_committed_key_view(Key, Msg, _Opts) when is_binary(Key), is_map(Msg) ->
    maps:is_key(Key, Msg) orelse
        maps:is_key(<<Key/binary, "+link">>, Msg);
has_committed_key_view(Key, Msg, Opts) ->
    hb_maps:is_key(Key, Msg, Opts).

has_implicit_ao_types(Msg, Opts) ->
    lists:any(
        fun({Key, Value}) ->
            not hb_private:is_private(Key)
                andalso Key =/= <<"commitments">>
                andalso has_implicit_ao_type_value(Value)
        end,
        hb_maps:to_list(Msg, Opts)
    ).

has_implicit_ao_type_value(Value) ->
    is_integer(Value)
        orelse is_float(Value)
        orelse is_atom(Value)
        orelse has_typed_link_value(Value).

has_typed_link_value({link, _ID, LinkOpts}) when is_map(LinkOpts) ->
    case maps:get(<<"type">>, LinkOpts, undefined) of
        undefined -> false;
        <<"link">> -> false;
        _ -> true
    end;
has_typed_link_value(_Value) ->
    false.

needs_unsigned_commitment(_Msg, [], _Opts) ->
    true;
needs_unsigned_commitment(Msg, UnsignedCommitments, Opts) ->
    has_message_extension(Msg)
        andalso not lists:any(
            fun({_ID, Commitment}) ->
                lists:member(
                    <<"...">>,
                    committed_keys_for_commitment(Commitment, Opts)
                )
            end,
            UnsignedCommitments
        ).

committed_keys_for_commitment(Commitment, Opts) ->
    lists:map(
        fun hb_link:remove_link_specifier/1,
        hb_util:message_to_ordered_list(
            hb_maps:get(<<"committed">>, Commitment, [], Opts),
            Opts
        )
    ).

unsigned_commitments(Msg, Opts) ->
    hb_maps:to_list(
        hb_maps:filter(
            fun(_, #{ <<"committer">> := _Committer }) -> false;
               (_, _) -> true
            end,
            maps:get(<<"commitments">>, Msg, #{}),
            Opts
        ),
        Opts
    ).

unsigned_commitments_match(Msg, UnsignedCommitments, Opts) ->
    lists:all(
        fun({ID, Commitment}) ->
            Committed = hb_maps:get(<<"committed">>, Commitment, [], Opts),
            {ok, #{ <<"commitments">> := NormCommitments }} =
                hb_ao:raw(
                    <<"message@1.0">>,
                    <<"commit">>,
                    uncommitted(Msg),
                    unsigned_commit_request(
                        Msg,
                        #{ <<"committed">> => Committed },
                        Opts
                    ),
                    unsigned_commit_opts(Msg, Opts)
                ),
            lists:member(ID, hb_maps:keys(NormCommitments, Opts))
        end,
        UnsignedCommitments
    ).

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
commit(Msg, Opts, Spec) ->
    CommitSpec =
        maybe_deep_bundle_linkify_spec(
            Msg,
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
    {ok, Signed} =
        hb_ao:raw(
            <<"message@1.0">>,
            <<"commit">>,
            Msg,
            CommitSpec,
            maybe_deep_bundle_linkify_opts(Msg, CommitSpec, Opts)
        ),
    Signed.

maybe_deep_bundle_linkify_spec(Msg, Spec, Opts) ->
    case has_bundle_commitment_deep(Msg, Opts)
            andalso not hb_util:atom(maps:get(<<"bundle">>, Spec, false))
            andalso not maps:is_key(<<"linkify-mode">>, Spec) of
        true -> Spec#{ <<"linkify-mode">> => false };
        false -> Spec
    end.

maybe_deep_bundle_linkify_opts(Msg, Spec, Opts) ->
    case has_bundle_commitment_deep(Msg, Opts)
            andalso not hb_util:atom(maps:get(<<"bundle">>, Spec, false)) of
        true -> Opts#{ <<"linkify-mode">> => false };
        false -> Opts
    end.

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
        hb_ao:raw(<<"message@1.0">>, <<"verify">>, Msg, Spec, Opts),
    Res.

%% @doc Verify a message recursively, including all nested messages.
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
            ?event(debug_paranoia, {paranoid_verify_called, Msg}, Opts),
            do_paranoid_verify(Topic, Msg, Opts);
        Topics ->
            case lists:member(Topic, Topics) of
                false -> true;
                true ->
                    ?event(debug_paranoia, {paranoid_verify_called, Msg}, Opts),
                    do_paranoid_verify(Topic, Msg, Opts)
            end
    end.

do_paranoid_verify(Topic, Msg, Opts) ->
    try
        do_paranoid_verify(Topic, [], Msg, Opts),
        ?event(debug_paranoia, {paranoid_verify_complete, ok}, Opts),
        true
    catch
        throw:{verification_failure, _Topic, RawPath, FailedMsg, Details, Stack} ->
            Path = hb_path:to_binary(RawPath),
            ?event(error,
                {paranoid_verification_failure,
                    {triggered_by, Topic},
                    {at_path, Path},
                    {failed_message, FailedMsg},
                    {while_verifying, Msg},
                    {details, Details},
                    {stack, {trace, Stack}}
                },
                Opts#{
                    <<"paranoid-verify">> => false
                }
            ),
            throw({paranoid_verification_failure, Topic, Path, Msg, FailedMsg})
    end.
do_paranoid_verify(Topic, Path, {_Status, Msg}, Opts) ->
    do_paranoid_verify(Topic, Path, Msg, Opts);
do_paranoid_verify(Topic, Path, Link, Opts) when ?IS_LINK(Link) ->
    case hb_opts:get(paranoid_verify_links, true, Opts) of
        false -> true;
        true ->
            do_paranoid_verify(Topic, Path, hb_cache:ensure_loaded(Link, Opts), Opts)
    end;
do_paranoid_verify(Topic, Path, ListMsg, Opts) when is_list(ListMsg) ->
    do_paranoid_verify(Topic, Path, hb_util:list_to_numbered_message(ListMsg), Opts);
do_paranoid_verify(Topic, Path, Msg, Opts) when is_map(Msg) ->
    do_paranoid_verify_children(Topic, Path, Msg, Opts),
    VerifyMsg = paranoid_verify_message(Topic, Msg, Opts),
    try true = paranoid_verify_commitments(VerifyMsg, Opts)
    catch
        _:Details:St ->
            throw({verification_failure, Topic, Path, VerifyMsg, Details, St})
    end;
do_paranoid_verify(_Topic, _Path, _Msg, _Opts) ->
    true.

paranoid_verify_commitments(Msg, Opts) ->
    try verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, Opts) of
        true -> true;
        false -> paranoid_verify_each_commitment(Msg, Opts)
    catch
        _:{multiple_ans104_commitments_unsupported, _}:_ ->
            paranoid_verify_each_commitment(Msg, Opts)
    end.

paranoid_verify_each_commitment(Msg, Opts) ->
    case hb_maps:get(<<"commitments">>, Msg, #{}, Opts) of
        Commitments when is_map(Commitments), map_size(Commitments) > 0 ->
            lists:all(
                fun(ID) ->
                    try verify(Msg, #{ <<"commitment-ids">> => [ID] }, Opts)
                    catch _:_ -> false
                    end
                end,
                hb_maps:keys(Commitments, Opts)
            );
        _ ->
            false
    end.

paranoid_verify_message(cache_read, Msg, Opts) ->
    filter_message_commitments_for_visible_keys(Msg, Opts);
paranoid_verify_message(cache_write, Msg, Opts) ->
    filter_message_commitments_for_visible_keys(Msg, Opts);
paranoid_verify_message(_Topic, Msg, _Opts) ->
    Msg.

do_paranoid_verify_children(Topic, Path, Msg, Opts) ->
    VerifyChild =
        fun(Key, Value) ->
            do_paranoid_verify(Topic, Path ++ [Key], Value, Opts)
        end,
    case has_message_extension(Msg) of
        true ->
            maps:map(
                VerifyChild,
                maps:remove(<<"commitments">>, hb_private:reset(Msg))
            );
        false ->
            hb_maps:map(
                VerifyChild,
                uncommitted(hb_private:reset(Msg), Opts),
                Opts
            )
    end.

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
    % Collapse any message extension first, so matching compares resolved content
    % rather than the layered structure (and so the `...' key never reaches the
    % key-set comparison or normalisation below). Nested extensions are handled by
    % the recursive `unsafe_match' call on submessage values.
    Map1Flat = hb_maps:flatten(RawMap1, Opts),
    Map2Flat = hb_maps:flatten(RawMap2, Opts),
    {_, SignedCommitments1} =
        lists:partition(
            fun({_, #{ <<"committer">> := _Committer }}) -> false;
               ({_, _}) -> true
            end,
            hb_maps:to_list(hb_maps:get(<<"commitments">>, Map1Flat, #{}, Opts))
        ),
    {_, SignedCommitments2} =
        lists:partition(
            fun({_, #{ <<"committer">> := _Committer }}) -> false;
               ({_, _}) -> true
            end,
            hb_maps:to_list(hb_maps:get(<<"commitments">>, Map1Flat, #{}, Opts))
        ),
    Map1 = Map1Flat#{ <<"commitments">> => SignedCommitments1 },
    Map2 = Map2Flat#{ <<"commitments">> => SignedCommitments2 },
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
    ?event(debug_match,
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
                    ?event(debug_match, {matching_key, Key}),
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
commitment(ID, Msg, Opts) when is_binary(ID), is_map(Msg) ->
    Commitments = maps:get(<<"commitments">>, Msg, #{}),
    case hb_maps:find(ID, Commitments, Opts) of
        {ok, Commitment} -> Commitment;
        error -> commitment(#{ <<"committer">> => ID }, Msg, Opts)
    end;
commitment(#{ <<"type">> := <<"unsigned">> }, Msg, Opts) ->
    Commitments = maps:get(<<"commitments">>, Msg, #{}),
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
            multiple_matches
    end;
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
    end.

%% @doc Return a list of all commitments that match the spec.
commitments(ID, Link, Opts) when ?IS_LINK(Link) ->
    commitments(ID, hb_cache:ensure_loaded(Link, Opts), Opts);
commitments(CommitterID, Msg, Opts) when is_binary(CommitterID) ->
    commitments(#{ <<"committer">> => CommitterID }, Msg, Opts);
commitments(Spec, Msg, Opts) when is_map(Msg) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
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
        <<"hashpath">> => ignore,
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
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
