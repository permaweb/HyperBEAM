%%% @doc The identity device: For non-reserved keys, it simply returns a key 
%%% from the message as it is found in the message's underlying Erlang map. 
%%% Private keys (`priv[.*]') are not included.
%%% Reserved keys are: `id', `commitments', `committers', `info', `keys',
%%% `path', `set', `remove', and `verify'. Their function comments describe the
%%% behaviour of the device when these keys are set.
-module(dev_message).
-specification("../../../specs/message@1.0.md").
%%% Base AO-Core reserved keys:
-export([device_info/0, keys/1, keys/2]).
-export([set/3, set_path/3, remove/3, get/4]).
%%% Commitment-specific keys:
-export([id/1, id/2, id/3]).
-export([commit/3, committed/3, committers/1, committers/2, committers/3, verify/3]).
-export([with_commitments/3]).
%%% Non-protocol enforced keys:
-export([index/3, info/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-define(DEFAULT_ID_DEVICE, <<"httpsig@1.0">>).
-define(DEFAULT_ATT_DEVICE, <<"httpsig@1.0">>).

%% The list of keys that are exported by this device.
-define(DEVICE_KEYS, [
    <<"id">>,
    <<"commitments">>,
    <<"committers">>,
    <<"keys">>,
    <<"path">>,
    <<"set">>,
    <<"remove">>,
    <<"verify">>
]).

%% @doc Return the options of the identity device.
device_info() ->
    #{
        default => fun dev_message:get/4
    }.

%% @doc Generate an index page for a message, in the event that the `body' and
%% `content-type' of a message returned to the client are both empty. We do this
%% as follows:
%% 1. Find the `default_index' key of the node message. If it is a binary,
%%    it is assumed to be the name of a device, and we execute the resolution
%%    `as` that ID.
%% 2. Merge the base message with the default index message, favoring the default
%%    index message's keys over those in the base message, unless the default
%%    was a device name.
%% 3. Execute the `default_index_path` (base: `index') upon the message,
%%    giving the rest of the request unchanged.
-spec index(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, _} | {error, _}.
index(Msg, Req, Opts) ->
    case hb_opts:get(default_index, not_found, Opts) of
        not_found ->
            {error, <<"No default index message set.">>};
        DefaultIndex ->
            hb_ao:resolve(
                case is_map(DefaultIndex) of
                    true -> maps:merge(Msg, DefaultIndex);
                    false -> {as, DefaultIndex, Msg}
                end,
                Req#{
                    <<"path">> =>
                        case hb_maps:find(<<"path">>, DefaultIndex, Opts) of
                            {ok, Path} -> Path;
                            _ ->
                                hb_opts:get(default_index_path, <<"index">>, Opts)
                        end
                },
                Opts
            )
    end.

%% @doc Return the info of the device of a message, or of the device in the
%% request's `for' key. The info is a message for the node's `docs-device', so
%% a browser that requests it receives that device's page. It has the schema
%% of the device's module (`hb_types') and the device's signed specification.
%% The info follows the implementation of the device that the node loads, so
%% it is not stored. A device's own `info/3' answers in place of this function.
-spec info(#{ device => _ }, #{ for => binary() }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {error, _}.
info(Base, Req, Opts) ->
    For =
        hb_maps:get(
            <<"for">>,
            Req,
            hb_maps:get(<<"device">>, Base, <<"message@1.0">>, Opts),
            Opts
        ),
    maybe
        {ok, Module} ?= hb_device_load:reference(For, Opts),
        {ok, Schema} ?= hb_device_load:schema(Module, Opts),
        {ok, Spec} ?= hb_device_load:spec(For, Opts),
        {ok,
            #{
                <<"device">> =>
                    hb_opts:get(docs_device, <<"docs@1.0">>, Opts),
                <<"for">> => For,
                <<"schema">> => Schema,
                <<"specification">> => Spec,
                <<"cache-control">> => <<"no-store">>
            }
        }
    end.

%% @doc Return the ID of a message, using the `committers' list if it exists.
%% If the `committers' key is `all', return the ID including all known 
%% commitments -- `none' yields the ID without any commitments. If the 
%% `committers' key is a list/map, return the ID including only the specified 
%% commitments.
%% 
%% The `id-device' key in the message can be used to specify the device that
%% should be used to calculate the ID. If it is not set, the default device
%% (`httpsig@1.0') is used.
%% 
%% Note: This function _does not_ use AO-Core's `get/3' function, as it
%% would require significant computation. We may want to change this
%% if/when non-map message structures are created.
id(Base) -> id(Base, #{}).
id(Base, Req) -> id(Base, Req, #{}).
-spec id(
    binary() | [#{ _ => _ }] | #{ commitments => #{ _ => _ }, _ => _ },
    #{
        committers => _,
        ids => _,
        'id-device' => binary(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, binary()}.
id(Base, _, NodeOpts) when is_binary(Base) ->
    % Return the hashpath of the message in native format, to match the native
    % format of the message ID return.
    {ok, hb_util:human_id(hb_path:hashpath(Base, NodeOpts))};
id(List, Req, NodeOpts) when is_list(List) ->
    % Return the list of IDs for a list of messages, without writing them.
    IDOpts = NodeOpts#{ <<"linkify-mode">> => discard },
    SourceSpec =
        hb_message:add_bundle_hint(
            #{ <<"device">> => <<"structured@1.0">> },
            Req#{ <<"device">> => ?DEFAULT_ID_DEVICE },
            NodeOpts
        ),
    id(hb_message:convert(List, tabm, SourceSpec, IDOpts), Req, NodeOpts);
id(RawBase, Req, NodeOpts) ->
    % Ensure that the base message is normalized before proceeding.
    IDOpts = NodeOpts#{ <<"linkify-mode">> => discard },
    Base = ensure_commitments_loaded(RawBase, NodeOpts),
    % Remove the commitments from the base message if there are none, after
    % filtering for the committers specified in the request.
    #{ <<"commitments">> := Commitments }
        = with_relevant_commitments(Base, Req, IDOpts),
    ?event_debug(debug_id,
        {generating_ids,
            {selected_commitments, Commitments},
            {req, Req},
            {msg, Base}
        }
    ),
    case hb_maps:keys(Commitments) of
        [] ->
            % If there are no commitments, we must (re)calculate the ID.
            ?event_debug(debug_id, regenerating_id),
            calculate_id(hb_maps:without([<<"commitments">>], Base), Req, IDOpts);
        IDs ->
            % A message with one commitment has that commitment's ID. Several
            % are combined whatever their order: the SHA-256 of their IDs,
            % sorted and joined by newlines.
            ?event_debug(debug_id, returning_existing_ids),
            {ok,
                case IDs of
                    [ID] -> ID;
                    _ -> hb_util:human_id(hb_crypto:accumulate(IDs))
                end
            }
    end.

calculate_id(RawBase, Req, NodeOpts) ->
    % Resolve the ID device up-front so we can plumb it as `hint-device' into
    % the structured->tabm conversion below. This keeps the children's load
    % state consistent with what `commit/3' and `verify/3' would produce.
    IDDev =
        case id_device(RawBase, NodeOpts) of
            {ok, Device} -> Device;
            {error, Error} -> throw({id, Error})
        end,
    % Encode to a TABM. The `bundle' flag (when set on the request) is the
    % caller's intent for the top-level message and applies only to the root;
    % `hint-device' lets the structured codec reproduce each nested
    % commitment's own bundle state per-node, so the id is computed over the
    % same shape `commit/3' and `verify/3' would produce.
    SourceSpec =
        hb_message:add_bundle_hint(
            #{ <<"device">> => <<"structured@1.0">> },
            Req#{ <<"device">> => IDDev },
            NodeOpts
        ),
    Base = hb_message:convert(RawBase, tabm, SourceSpec, NodeOpts),
    ?event_debug(debug_id, {calculate_ids, {base, Base}}),
    ?event_debug(debug_id, {generating_id, {id_device, IDDev}, {base, Base}}),
    % Get the commitment device name from the message, or use the default if
    % it is not set. We can tell if the device is not set (or is the default)
    % by checking whether the resolved device module is this module itself.
    % `hb_ao:raw/5' expects a device name, not a resolved module.
    CommitDev =
        case hb_device:message_to_device(#{ <<"device">> => IDDev }, NodeOpts) of
            ?MODULE -> ?DEFAULT_ID_DEVICE;
            _ -> IDDev
        end,
    ?event_debug(debug_id, {called_id_device, CommitDev}, NodeOpts),
    {ok, #{ <<"commitments">> := Comms} } =
        hb_ao:raw(
            CommitDev,
            <<"commit">>,
            Base,
            Req#{ <<"type">> => <<"unsigned">> },
            NodeOpts
        ),
    ?event_debug(debug_id,
        {generated_id,
            {type, unsigned},
            {commitments, maps:keys(Comms)}
        }
    ),
    {ok, hd(maps:keys(Comms))}.

%% @doc Locate the ID device of a message. The ID device is determined the
%% `device' set in _all_ of the commitments. If no commitments are present,
%% the default device (`httpsig@1.0') is used.
id_device(#{ <<"commitments">> := Commitments }, Opts) ->
    % Get the device from the first commitment.
    UnfilteredDevs =
        hb_maps:map(
            fun(_, #{ <<"commitment-device">> := CommitmentDev }) ->
                CommitmentDev;
            (_, _) -> undefined
            end,
            Commitments,
            Opts
        ),
    % Filter out the undefined devices.
    Devs =
        lists:filter(
            fun(Dev) -> Dev =/= undefined end,
            hb_maps:values(UnfilteredDevs, Opts)
        ),
    % If there are no devices, return the default.
    case Devs of
        [] -> {ok, ?DEFAULT_ID_DEVICE};
        [Dev] -> {ok, Dev};
        [FirstDev|Rest] ->
            % If there are multiple devices amongst the set, err.
            MultiDeviceMessage = lists:all(fun(Dev) -> Dev =:= FirstDev end, Rest),
            case MultiDeviceMessage of
                false -> {error, {multiple_id_devices, Devs}};
                true -> {ok, FirstDev}
            end
    end;
id_device(_, _) ->
    {ok, ?DEFAULT_ID_DEVICE}.

%% @doc Return the committers of a message. A committer is listed once, however
%% many commitments it made.
committers(Base) -> committers(Base, #{}).
committers(Base, Req) -> committers(Base, Req, #{}).
-spec committers(
    #{ commitments => #{ _ => _ }, _ => _ },
    #{ _ => _ },
    #{ _ => _ }
) -> {ok, [_]}.
committers(#{ <<"commitments">> := Commitments }, _, NodeOpts) ->
    Committers =
        hb_maps:values(
            hb_maps:filtermap(
                fun(_ID, Commitment) ->
                    case maps:get(<<"committer">>, Commitment, undefined) of
                        undefined -> false;
                        Committer -> {true, Committer}
                    end
                end,
                Commitments,
                NodeOpts
            ),
            NodeOpts
        ),
    {ok, hb_util:unique(Committers)};
committers(_, _, _) ->
    {ok, []}.

%% @doc Commit to a message, using the `commitment-device' key to specify the
%% device that should be used to commit to the message. If the key is not set,
%% the default device (`httpsig@1.0') is used. A request with a `committed'
%% list commits those keys of the message; a request without one commits every
%% key.
-spec commit(
    #{ _ => _ },
    #{ 'commitment-device' => binary(), type => binary(),
        committed => [binary()], _ => _ },
    #{ _ => _ }
) -> {ok, #{ commitments := #{ _ => _ }, _ => _ }}.
commit(Self, Req, Opts) ->
    {ok, Base} =
        hb_message:find_subject(<<"message-subject">>, Self, Req, Opts),
    % No codec carries a commitment to an empty key, so we refuse it.
    case is_map_key(<<>>, Base) of
        true -> throw({invalid_key, <<>>});
        false -> ok
    end,
    AttDev =
        case hb_maps:get(<<"commitment-device">>, Req, not_specified, Opts) of
            not_specified ->
                hb_opts:get(commitment_device, no_viable_commitment_device, Opts);
            Dev -> Dev
        end,
    % We _do not_ set the `device' key in the message, as the device will be
    % part of the commitment. Instead, we find the device module's `commit'
    % function and apply it.
    CommitOpts =
        case hb_maps:get(<<"type">>, Req, <<"signed">>) of
            <<"unsigned">> ->
                Opts#{ <<"linkify-mode">> => discard };
            _ ->
                Opts#{ <<"linkify-mode">> => offload }
        end,
    % Encode to a TABM. The `bundle' flag (when set on the request) is the
    % caller's intent for the top-level commit and applies only to the root
    % message; `hint-device' lets the structured codec preserve each nested
    % commitment's own bundle state per-node.
    SourceSpec =
        hb_message:add_bundle_hint(
            #{ <<"device">> => <<"structured@1.0">> },
            Req#{ <<"device">> => AttDev },
            CommitOpts
        ),
    % A commitment is made over the keys that the request lists, else over the
    % keys that any commitment of the message lists, else over every key of the
    % message. The device is given the message with those keys alone, as
    % `verify/3' gives it.
    Covered =
        case keys_to_commit(Base, Req, Opts) of
            all -> Base;
            Keys ->
                hb_maps:with(
                    [<<"commitments">>, <<"priv">> | Keys],
                    Base,
                    Opts
                )
        end,
    Loaded =
        ensure_commitments_loaded(
            hb_message:convert(Covered, tabm, SourceSpec, CommitOpts),
            Opts
        ),
    {ok, Committed} =
        hb_ao:raw(
            AttDev,
            <<"commit">>,
            Loaded,
            (hb_message:with_committer(Req, Opts))#{
                <<"type">> => maps:get(<<"type">>, Req, <<"signed">>)
            },
            CommitOpts
        ),
    {ok, Base#{ <<"commitments">> => maps:get(<<"commitments">>, Committed) }}.

%% @doc The keys a commitment lists as committed, in their normalized form. A
%% commitment without a `committed' list commits no keys.
committed_keys(Commitment, Opts) ->
    hb_util:message_to_ordered_list(
        maps:get(<<"committed">>, Commitment, []),
        Opts
    ).

%% @doc Return the keys that a commitment is made over: the keys that the
%% request lists, else the keys that any commitment of the message lists, else
%% `all': every key of the message.
keys_to_commit(Base, Req, Opts) ->
    case hb_maps:find(<<"committed">>, Req, Opts) of
        {ok, _} -> committed_keys(Req, Opts);
        error ->
            Committed =
                hb_message:committed(Base, #{ <<"ids">> => <<"any">> }, Opts),
            case Committed of
                [] -> all;
                Keys -> Keys
            end
    end.

%% @doc Verify a message. By default, all commitments are verified. The
%% `committers' key in the request can be used to specify that only the 
%% commitments from specific committers should be verified. Similarly, specific
%% commitments can be specified using the `ids' key. Each takes `all', `none',
%% one address or ID, or a list of them. A message that lacks a committer or
%% commitment the request lists does not verify, nor does `committers=all'
%% on a message without committers. The request is kept whole: its
%% `message-subject' may select any of its keys as the message to verify, and
%% its private element goes to the commitment device.
-spec verify(
    #{ _ => _ },
    #{
        committers => binary() | [binary()],
        ids => binary() | [binary()],
        'message-subject' => binary(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, boolean()}.
verify(Self, Req, Opts) ->
    % Get the target message of the verification request.
    {ok, RawBase} =
        hb_message:find_subject(<<"message-subject">>, Self, Req, Opts),
    CommitmentBase = ensure_commitments_loaded(RawBase, Opts),
    Commitments = maps:get(<<"commitments">>, CommitmentBase, #{}),
    % A request with neither `committers' nor `ids' verifies every
    % commitment of the message. `committers=none' without `ids'
    % verifies every commitment without a committer.
    Selection =
        case maps:with([<<"committers">>, <<"ids">>], Req) of
            None when map_size(None) == 0 ->
                Req#{ <<"ids">> => <<"all">> };
            #{ <<"committers">> := <<"none">> }
                    when not is_map_key(<<"ids">>, Req) ->
                Unsigned =
                    maps:filter(
                        fun(_, Commitment) ->
                            not maps:is_key(<<"committer">>, Commitment)
                        end,
                        Commitments
                    ),
                Req#{ <<"ids">> => maps:keys(Unsigned) };
            _ -> Req
        end,
    case has_named(CommitmentBase, Req, Opts) of
        false -> {ok, false};
        true ->
            verify_ids(
                commitment_ids_from_request(CommitmentBase, Selection, Opts),
                Commitments,
                CommitmentBase,
                Req,
                Opts
            )
    end.

%% @doc Whether a message has what a verification request gives: each
%% committer and commitment it lists, and a committer if it asks for `all'.
has_named(Base, Req, Opts) ->
    {ok, Committers} = committers(Base, #{}, Opts),
    Commitments = maps:get(<<"commitments">>, Base, #{}),
    case maps:get(<<"committers">>, Req, <<"none">>) of
        <<"all">> -> Committers =/= [];
        <<"none">> -> true;
        NamedCommitters ->
            lists:all(
                fun(Committer) ->
                    lists:member(hb_committer:uri(Committer, Opts), Committers)
                end,
                lists:flatten([NamedCommitters])
            )
    end andalso
        case maps:get(<<"ids">>, Req, <<"none">>) of
            <<"all">> -> true;
            <<"none">> -> true;
            NamedIDs ->
                lists:all(
                    fun(ID) -> maps:is_key(ID, Commitments) end,
                    lists:flatten([NamedIDs])
                )
        end.

%% @doc Verify the given commitments of a message.
verify_ids(IDsToVerify, Commitments, CommitmentBase, Req, Opts) ->
    % The commitment device receives the keys of each commitment and the
    % private element of the request. No other key of the request reaches it.
    ReqPriv = hb_private:from_message(Req),
    % Verification derives the IDs of nested messages without writing the
    % messages to the cache.
    VerifyOpts = Opts#{ <<"linkify-mode">> => discard },
    % Verify the commitments. Stop execution if any fail.
    Res =
        lists:all(
            fun(CommitmentID) ->
                Commitment =
                    hb_private:set_priv(
                        maps:get(CommitmentID, Commitments),
                        ReqPriv
                    ),
                % Build the source spec from the commitment device alone: a
                % `hint-device' lets the structured codec reproduce each
                % subtree in the bundle state it was committed in. The verify
                % request's `bundle' is deliberately *not* propagated -- a
                % commitment is always verified in the state it was signed
                % in, so any `bundle' passed by the caller is irrelevant.
                SourceSpec =
                    hb_message:add_bundle_hint(
                        #{ <<"device">> => <<"structured@1.0">> },
                        #{
                            <<"device">> =>
                                maps:get(
                                    <<"commitment-device">>,
                                    Commitment,
                                    undefined
                                ),
                            <<"bundle">> =>
                                hb_util:atom(
                                    maps:get(<<"bundle">>, Commitment, false)
                                )
                        },
                        Opts
                    ),
                % A commitment is verified over the keys it lists and as the
                % only commitment of the base: keys given alongside the
                % committed ones do not enter its signature base, and the
                % bundle state of another commitment does not set the
                % encoding of this one.
                Covered =
                    hb_maps:with(
                        [
                            <<"commitments">>,
                            <<"priv">>
                        |
                            committed_keys(Commitment, Opts)
                        ],
                        CommitmentBase#{
                            <<"commitments">> =>
                                maps:with([CommitmentID], Commitments)
                        },
                        Opts
                    ),
                Base = hb_message:convert(Covered, tabm, SourceSpec, VerifyOpts),
                ?event(verify, {verify, {base_found, Base}}),
                {ok, Res} =
                    verify_commitment(
                        Base,
                        Commitment,
                        Opts
                    ),
                ?event(verify,
                    {verify_commitment_res,
                        {commitment_id, CommitmentID},
                        {res, Res}
                    }),
                Res
            end,
            IDsToVerify
        ),
    ?event(verify, {verify, {res, Res}}),
    {ok, Res}.

%% @doc Execute a function for a single commitment in the context of its
%% parent message. A commitment device whose `verify' is this module's own
%% `verify' has no commitment scheme of its own: the device of a commitment
%% is read from the commitment itself, so dispatching would call this
%% function with the same commitment again, without end. Such a commitment
%% does not verify.
%% Note: Assumes that the `commitments' key has already been removed from the
%% message if applicable.
verify_commitment(Base, Commitment, Opts) ->
    ?event(verify, {verifying_commitment, {commitment, Commitment}, {msg, Base}}),
    AttDev =
        hb_maps:get(
            <<"commitment-device">>,
            Commitment,
            ?DEFAULT_ATT_DEVICE,
            Opts
        ),
    case hb_device:message_to_fun(
        #{ <<"device">> => AttDev },
        <<"verify">>,
        Opts
    ) of
        {_, ?MODULE, _} -> {ok, false};
        _ -> hb_ao:raw(AttDev, <<"verify">>, Base, Commitment, Opts)
    end.

%% @doc Attach to the message the commitments whose IDs are listed in the
%% `with-commitments' key of the request. Each commitment is read by its own
%% ID, and the message read must have the base's unsigned ID: a commitment
%% that is not found, or that is over another message, is refused. On the
%% empty message, the commitments are attached to the message that the first
%% of them is over, with the keys that it lacks from the messages that the
%% others are over.
with_commitments(Base, Req, Opts) ->
    Loaded = ensure_commitments_loaded(Base, Opts),
    read_commitments(
        hb_util:binary_to_strings(
            hb_maps:get(<<"with-commitments">>, Req, <<>>, Opts)
        ),
        Loaded,
        hb_message:id(Loaded, none, Opts),
        Opts
    ).

%% @doc Read each commitment by its ID and attach it to the message, with the
%% keys of the message read that the message lacks. The message read by the ID
%% must have the given unsigned ID, unless that is `any'. The empty message is
%% replaced by the first message read, and the unsigned ID becomes `any'.
read_commitments([], Msg, _UnsignedID, _Opts) -> {ok, Msg};
read_commitments([ID | IDs], Msg, UnsignedID, Opts) ->
    case hb_cache:read(ID, Opts) of
        {ok, Read = #{ <<"commitments">> := #{ ID := Commitment } }} ->
            case hb_message:id(Read, none, Opts) of
                ReadID when ReadID == UnsignedID; UnsignedID == any ->
                    read_commitments(
                        IDs,
                        (maps:merge(Read, Msg))#{
                            <<"commitments">> =>
                                (maps:get(<<"commitments">>, Msg, #{}))#{
                                    ID => Commitment
                                }
                        },
                        UnsignedID,
                        Opts
                    );
                _ when ?IS_EMPTY_MESSAGE(Msg) ->
                    read_commitments(IDs, Read, any, Opts);
                _ ->
                    {error,
                        #{
                            <<"status">> => 400,
                            <<"body">> =>
                                <<"Commitment ", ID/binary,
                                    " is not over this message.">>
                        }
                    }
            end;
        _ ->
            {error,
                #{
                    <<"status">> => 404,
                    <<"body">> => <<"Commitment ", ID/binary, " not found.">>
                }
            }
    end.

%% @doc Return the list of committed keys from a message. With `ids' set to
%% `any', the keys that any selected commitment covers; otherwise the keys
%% that every one covers.
-spec committed(
    #{ _ => _ },
    #{ committers => _, ids => _, _ => _ },
    #{ _ => _ }
) -> {ok, [binary()]}.
committed(Self, Req, Opts) ->
    % Get the target message of the verification request and ensure its 
    % commitments are loaded.
    {ok, RawBase} =
        hb_message:find_subject(
            <<"message-subject">>,
            Self,
            Req,
            Opts
        ),
    Base = ensure_commitments_loaded(RawBase, Opts),
    CommitmentIDs = commitment_ids_from_request(Base, Req, Opts),
    ?event_debug(debug_commitments,
        {calculating_committed,
            {commitment_ids, CommitmentIDs},
            {req, Req}
        }
    ),
    Commitments = maps:get(<<"commitments">>, Base, #{}),
    % Get the list of committed keys from each committer.
    CommitmentKeys =
        lists:map(
            fun(CommitmentID) ->
                committed_keys(maps:get(CommitmentID, Commitments), Opts)
            end,
            CommitmentIDs
        ),
    % Remove commitments that are not in *every* committer's list.
    % To start, we need to create the super-set of committed keys.
    AllCommittedKeys =
        lists:foldr(
            fun(Key, Acc) ->
                case lists:member(Key, Acc) of
                    true -> Acc;
                    false -> [Key | Acc]
                end
            end,
            [],
            lists:flatten(CommitmentKeys)
        ),
    % Next, we filter the list of all committed keys to only include those that
    % are present in every committer's list.
    OnlyCommittedKeys =
        case maps:get(<<"ids">>, Req, <<"none">>) of
            <<"any">> -> AllCommittedKeys;
            _ ->
                lists:filter(
                    fun(Key) ->
                        lists:all(
                            fun(CommittedKeys) -> lists:member(Key, CommittedKeys) end,
                            CommitmentKeys
                        )
                    end,
                    AllCommittedKeys
                )
        end,
    ?event_debug(debug_commitments, {only_committed_keys, OnlyCommittedKeys}),
    {ok, OnlyCommittedKeys}.

%% @doc Return a message with only the relevant commitments for a given request.
%% See `commitment_ids_from_request/3' for more information on the request format.
with_relevant_commitments(Base, Req, Opts) ->
    Commitments = maps:get(<<"commitments">>, Base, #{}),
    CommitmentIDs = commitment_ids_from_request(Base, Req, Opts),
    Base#{ <<"commitments">> => maps:with(CommitmentIDs, Commitments) }.

%% @doc Implements a standardized form of specifying commitment IDs for a
%% message request. The caller may specify a list of committers (by address)
%% or a list of commitment IDs directly. They may specify both, in which case
%% the returned list will be the union of the two lists. In each case, they
%% may specify `all' or `none' for each group. `any' selects every
%% commitment, as `all' does. If no specifiers are provided,
%% the default is `none' for both commitments and committers. With `none' for
%% both, the returned list holds the unsigned commitments of the default
%% commitment device, if there are any.
commitment_ids_from_request(Base, Req, Opts) ->
    Commitments = maps:get(<<"commitments">>, Base, #{}),
    ReqCommitters =
        case maps:get(<<"committers">>, Req, <<"none">>) of
            X when is_list(X) -> X;
            CommitterDescriptor -> hb_ao:normalize_key(CommitterDescriptor)
        end,
    RawReqCommitments = maps:get(<<"ids">>, Req, <<"none">>),
    ReqCommitments =
        case RawReqCommitments of
            X2 when is_list(X2) -> X2;
            CommitmentDescriptor -> hb_ao:normalize_key(CommitmentDescriptor)
        end,
    ?event_debug(debug_commitments,
        {commitment_ids_from_request,
            {req_commitments, ReqCommitments},
            {req_committers, ReqCommitters}}
    ),
    % Get the commitments to verify.
    FromCommitmentIDs =
        case ReqCommitments of
            <<"none">> -> [];
            <<"all">> -> hb_maps:keys(Commitments, Opts);
            <<"any">> -> hb_maps:keys(Commitments, Opts);
            CommitmentIDs ->
                if is_list(CommitmentIDs) -> CommitmentIDs;
                true -> [CommitmentIDs]
                end
        end,
    FromCommitterAddrs =
        case ReqCommitters of
            <<"none">> ->
                ?event_debug(debug_commitments, no_commitment_ids_for_committers),
                [];
            <<"all">> ->
                {ok, Committers} = committers(Base, Req, Opts),
                ?event_debug(debug_commitments, {commitment_ids_from_committers, Committers}),
                commitment_ids_from_committers(Committers, Commitments, Opts);
            RawCommitterAddrs ->
                ?event(
                    debug_commitments,
                    {getting_commitment_ids_for_committers, RawCommitterAddrs}
                ),
                CommitterAddrs =
                    if is_list(RawCommitterAddrs) -> RawCommitterAddrs;
                    true -> [RawCommitterAddrs]
                    end,
                URIs = [hb_committer:uri(C, Opts) || C <- CommitterAddrs],
                commitment_ids_from_committers(URIs, Commitments, Opts)
        end,
    Res =
        case FromCommitterAddrs ++ FromCommitmentIDs of
            [] ->
                % The request is for no committers, and no explicit commitments.
                % Subsequently, we return the commitment using the default
                % commitment device, if it exists.
                lists:filter(
                    fun(CommitmentID) ->
                        Comm = maps:get(CommitmentID, Commitments),
                        Dev = maps:get(<<"commitment-device">>, Comm, undefined),
                        case Dev of
                            ?DEFAULT_ATT_DEVICE ->
                                not hb_maps:is_key(<<"committer">>, Comm);
                            _ -> false
                        end
                    end,
                    maps:keys(Commitments)
                );
            FinalCommitmentIDs -> FinalCommitmentIDs
        end,
    ?event(
        debug_commitments,
        {commitment_ids_from_request, {base, Base}, {req, Req}, {res, Res}}
    ),
    Res.

%% @doc Ensure that the `commitments` submessage of a base message is fully
%% loaded into local memory.
ensure_commitments_loaded(M = #{ <<"commitments">> := L}, Opts) when ?IS_LINK(L) ->
    M#{
        <<"commitments">> => hb_cache:ensure_all_loaded(L, Opts)
    };
ensure_commitments_loaded(M, _Opts) ->
    M.

%% @doc Returns a list of commitment IDs in a commitments map that are relevant
%% for a list of given committer addresses.
commitment_ids_from_committers(CommitterAddrs, Commitments, Opts) ->
    % Get the IDs of all commitments for each committer.
    Comms =
        lists:map(
            fun(RawCommitterAddr) ->
                CommitterAddr = hb_cache:ensure_loaded(RawCommitterAddr, Opts),
                % For each committer, filter the commitments to only
                % include those with the matching committer address.
                IDs = 
                    maps:values(maps:filtermap(
                        fun(ID, Msg) ->
                            % If the committer address matches, return
                            % the ID. If not, ignore the commitment.
                            case hb_maps:get(<<"committer">>, Msg, undefined) of
                                CommitterAddr -> {true, ID};
                                _ -> false
                            end
                        end,
                        Commitments
                    )
                ),
                {CommitterAddr, IDs}
            end,
            CommitterAddrs
        ),
    % Check that each committer has at least one commitment.
    EachCommitterHasCommitment =
        lists:all(fun({_, IDs}) -> IDs =/= [] end, Comms),
    % If all committers have at least one commitment, return the
    % IDs of all commitments. If any committer does not have a
    % commitment, error.
    case EachCommitterHasCommitment of
        true -> lists:flatten([ IDs || {_, IDs} <- Comms ]);
        false ->
            % Get the list of committers that do not have a
            % commitment.
            MissingCommitters =
                [
                    MissingCommitter
                ||
                    {MissingCommitter, []} <- Comms
                ],
            throw(
                {verify,
                    {requested_committers_not_found,
                        {missing_commitments, MissingCommitters}
                    }
                }
            )
    end.

%% @doc Deep merge keys in a message. Takes a map of key-value pairs and sets
%% them in the message, overwriting any existing values.
-spec set(#{ _ => _ }, #{ 'set-mode' => binary(), _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }}.
set(Base, NewValuesMsg, Opts) ->
    OriginalPriv = hb_private:from_message(Base),
	% Filter keys that are in the default device (this one).
    {ok, NewValuesKeys} = keys(NewValuesMsg, Opts),
	KeysToSet =
		lists:filter(
			fun(Key) ->
				not lists:member(Key, ?DEVICE_KEYS ++ [<<"set-mode">>]) andalso
					(hb_maps:get(Key, NewValuesMsg, undefined, Opts) =/= undefined)
					andalso not hb_private:is_private_path(Key, Opts)
			end,
			NewValuesKeys
		),
    UnsetKeys =
        lists:filter(
            fun(Key) ->
                case hb_maps:get(Key, NewValuesMsg, not_found, Opts) of
                    unset -> true;
                    _ -> false
                end
            end,
            hb_maps:keys(Base, Opts)
        ),
    % Base message with keys-to-unset removed
    BaseValues = hb_maps:without(UnsetKeys, Base, Opts),
    ?event_debug(debug_message_set,
        {performing_set,
            {keys_to_unset, UnsetKeys},
            {new_values, NewValuesMsg},
            {original_message, Base}
        }
    ),
    SetMode = maps:get(<<"set-mode">>, NewValuesMsg, <<"deep">>),
    % Create the map of new values. A deep set keeps an `unset' value whose
    % path the base still holds: a nested key, which the deep merge removes.
    NewValues = hb_maps:from_list(
        lists:filtermap(
            fun(Key) ->
                case hb_maps:get(Key, NewValuesMsg, undefined, Opts) of
                    undefined -> false;
                    unset when SetMode =/= <<"explicit">> ->
                        hb_util:deep_get(Key, BaseValues, Opts) =/= not_found
                            andalso {true, {Key, unset}};
                    unset -> false;
                    Value -> {true, {Key, Value}}
                end
            end,
            KeysToSet
        )
    ),
    AddedKeys =
        lists:filter(
            fun(Key) -> not hb_maps:is_key(Key, Base, Opts) end,
            maps:keys(NewValues)
        ),
    % Calculate if the keys to be set conflict with any committed keys.
    % A key that any commitment covers is signed.
    {ok, CommittedKeys} =
        committed(
            Base,
            #{
                <<"ids">> => <<"any">>
            },
            Opts
        ),
    ?event_debug(message_set,
        {setting,
            {committed_keys, CommittedKeys},
            {keys_to_set, KeysToSet},
            {message, Base}
        }
    ),
    OverwrittenCommittedKeys =
        lists:filtermap(
            fun(Key) ->
                NormKey = hb_ao:normalize_key(Key),
                ?event_debug({checking_committed_key, {key, Key}, {norm_key, NormKey}}),
                Res = case lists:member(NormKey, KeysToSet) of
                    true -> {true, NormKey};
                    false -> false
                end,
                Res
            end,
            CommittedKeys
        ),
    ?event_debug({setting, {overwritten_committed_keys, OverwrittenCommittedKeys}}),
    % Combine with deep merge or if `set-mode` is `explicit' then just merge.
    Merged =
        hb_private:set_priv(
            case SetMode of
                <<"explicit">> -> maps:merge(BaseValues, NewValues);
                _ -> do_deep_merge(BaseValues, NewValues, Opts)
            end,
            OriginalPriv
        ),
    case {AddedKeys, OverwrittenCommittedKeys} of
        {[_ | _], _} ->
            {ok, hb_maps:without([<<"commitments">>], Merged, Opts)};
        {[], []} ->
            ?event_debug(message_set, {no_overwritten_committed_keys, {merged, Merged}}),
            {ok, Merged};
        {[], _} ->
            % We did overwrite some keys, but do their values match the original?
            % If not, we must remove the commitments.
            ChangedBaseKeys = hb_maps:with(OverwrittenCommittedKeys, Base, Opts),
            ChangedMergedKeys = hb_maps:with(OverwrittenCommittedKeys, Merged, Opts),
            Matches =
                try
                    ChangedMergedKeys =:= ChangedBaseKeys orelse
                        hb_cache:ensure_all_loaded(ChangedMergedKeys, Opts) =:=
                            hb_cache:ensure_all_loaded(ChangedBaseKeys, Opts)
                catch _:_ -> false
                end,
            case Matches of
                true ->
                    ?event_debug(message_set, {set_keys_matched, {merged, Merged}}),
                    {ok, Merged};
                % {error, {Details, {trace, Stacktrace}}} ->
                %     erlang:raise(error, Details, Stacktrace);
                % {mismatch, Type, Path, Val1, Val2} ->
                %     ?event(
                %         set_conflict,
                %         {set_conflict_removing_commitments,
                %             {merged, Merged},
                %             {mismatch, Type},
                %             {path, Path},
                %             {expected, Val1},
                %             {received, Val2}
                %         }
                %     ),
                _ ->
                    {ok, hb_maps:without([<<"commitments">>], Merged, Opts)}
            end
    end.

%% @doc Deep merge keys in a message, utilizing the set device of any child
%% keys that are themselves messages.
do_deep_merge(BaseValues, NewValues, Opts) ->
    {WithNestedMerges, StillToDeepMerge} =
        maps:fold(
            fun(Key, NewValue, {Acc, ToDeepMerge})
                    when is_map(NewValue)
                    andalso is_map(map_get(Key, Acc)) ->
                        BaseValue = map_get(Key, Acc),
                        NewValueSet = NewValue#{ <<"path">> => <<"set">> },
                        {
                            Acc#{
                                Key =>
                                    hb_util:ok(
                                        hb_ao:resolve(
                                            BaseValue,
                                            NewValueSet,
                                            Opts
                                        ),
                                        Opts
                                    )
                            },
                            ToDeepMerge
                        };
            (Key, NewValue, {Acc, ToDeepMerge})
                    when is_map(NewValue) 
                    andalso ?IS_LINK(map_get(Key, Acc)) ->
                LoadedBaseValue = hb_cache:ensure_loaded(map_get(Key, Acc), Opts),
                case is_map(LoadedBaseValue) of
                    true ->
                        NewValueSet = NewValue#{ <<"path">> => <<"set">> },
                        {
                            Acc#{
                                Key =>
                                    hb_util:ok(
                                        hb_ao:resolve(
                                            LoadedBaseValue,
                                            NewValueSet,
                                            Opts
                                        ),
                                        Opts
                                    )
                            },
                            ToDeepMerge
                        };
                    false -> 
                        {Acc, [Key | ToDeepMerge]}
                end;
            (Key, _, {Acc, ToDeepMerge}) ->
                {Acc, [Key | ToDeepMerge]}
            end,
            {BaseValues, []},
            NewValues
        ),
    hb_util:deep_merge(
        WithNestedMerges,
        maps:with(StillToDeepMerge, NewValues),
        Opts
    ).

%% @doc Special case of `set/3' for setting the `path' key. This cannot be set
%% using the normal `set' function, as the `path' is a reserved key, used to
%% transmit the present key that is being executed. Subsequently, to call `path'
%% we would need to set `path' to `set', removing the ability to specify its 
%% new value.
-spec set_path(#{ path => _, _ => _ }, #{ value => _, _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | #{ _ => _ }.
set_path(Base, #{ <<"value">> := Value }, Opts) ->
    set_path(Base, Value, Opts);
set_path(Base, Value, Opts) when not is_map(Value) ->
    % Determine whether the `path' key is committed. If it is, we remove the
    % commitment if the new value is different. We try to minimize work by
    % doing the `hb_maps:get` first, as it is far cheaper than calculating
    % the committed keys.
    BaseWithCorrectedComms =
        case hb_maps:get(<<"path">>, Base, undefined, Opts) of
            Value -> Base;
            _ ->
                % The new value is different, but is it committed? If so, we
                % must remove the commitments.
                case hb_message:is_signed_key(<<"path">>, Base, Opts) of
                  true -> hb_message:uncommitted(Base, Opts);
                  false -> Base
                end
        end,
    case Value of
        unset ->
            {ok, hb_maps:without([<<"path">>], BaseWithCorrectedComms, Opts)};
        _ ->
            BaseWithCorrectedComms#{ <<"path">> => Value }
    end.

%% @doc Remove a key or keys from a message.
-spec remove(#{ _ => _ }, #{ item => _, items => [_], _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }}.
remove(Base, #{ <<"item">> := Key }, Opts) ->
    remove(Base, #{ <<"items">> => [Key] }, Opts);
remove(Base, #{ <<"items">> := Keys }, Opts) ->
    set(
        Base,
        #{ Key => unset || Key <- Keys },
        Opts
    ).

%% @doc Get the public keys of a message.
keys(Msg) ->
	keys(Msg, #{}).

keys(Msg, Opts) when not is_map(Msg) ->
    case hb_ao:normalize_keys(Msg, Opts) of
        NormMsg when is_map(NormMsg) -> keys(NormMsg, Opts);
        _ -> throw(badarg)
    end;
keys(Msg, Opts) ->
    {
        ok,
        lists:filter(
            fun(Key) -> not hb_private:is_private(Key) end,
            hb_maps:keys(hb_message:uncommitted(Msg, Opts), Opts)
        )
    }.

%% @doc Return the value associated with the key as it exists in the message's
%% underlying Erlang map, if the key is public. The key is lowercased once, as
%% the keys of a message in `structured@1.0' form are, then matched exactly.
get(Key, Msg, _Req, Opts) ->
    case hb_private:is_private(Key) of
        true -> {error, not_found};
        false ->
            case hb_maps:find(hb_util:lowercase_key(Key), Msg, Opts) of
                error -> {error, not_found};
                {ok, Value} -> {ok, Value}
            end
    end.

%%% Tests

%%% Internal module functionality tests:
get_keys_mod_test() ->
    ?assertEqual([a], hb_maps:keys(#{a => 1}, #{})).

list_id_preserves_bundle_hint_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => hb:wallet()
    },
    List = [
        hb_message:commit(
            #{ <<"payload">> => #{ <<"deep">> => <<"value">> } },
            Opts,
            #{ <<"commitment-device">> => <<"httpsig@1.0">>, <<"bundle">> => true }
        )
    ],
    Source = #{
        <<"device">> => <<"structured@1.0">>,
        <<"hint-device">> => ?DEFAULT_ID_DEVICE
    },
    Expected = hb_message:id(
        hb_message:convert(List, tabm, Source, Opts),
        none,
        Opts
    ),
    ?assertEqual(Expected, hb_message:id(List, none, Opts)).

is_private_mod_test() ->
    ?assertEqual(true, hb_private:is_private(<<"private">>)),
    ?assertEqual(true, hb_private:is_private(<<"private.foo">>)),
    ?assertEqual(false, hb_private:is_private(<<"a">>)).

%%% Device functionality tests:

keys_from_device_test() ->
    ?assertEqual({ok, [<<"a">>]}, hb_ao:resolve(#{ <<"a">> => 1 }, keys, #{})).

private_keys_are_filtered_test() ->
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_ao:resolve(#{ <<"a">> => 1, <<"private">> => 2 }, keys, #{})
    ),
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_ao:resolve(#{ <<"a">> => 1, <<"priv_foo">> => 4 }, keys, #{})
    ).

cannot_get_private_keys_test() ->
    ?assertEqual(
        {error, not_found},
        hb_ao:resolve(
            #{ <<"a">> => 1, <<"private_key">> => 2 },
            <<"private_key">>,
            #{ <<"hashpath">> => ignore }
        )
    ).

key_from_device_test() ->
    {ok, ID} = hb_cache:write(#{ <<"a">> => not_found }, #{}),
    ?assertEqual(
        {ok, not_found},
        hb_ao:resolve(<<"ao:", ID/binary>>, <<"a">>, #{})
    ),
    ?assertEqual({ok, 1}, hb_ao:resolve(#{ <<"a">> => 1 }, <<"a">>, #{})),
    % A key named `get' is read like any other.
    ?assertEqual({ok, 2}, hb_ao:resolve(#{ <<"get">> => 2 }, <<"get">>, #{})),
    % A key named `with-commitments' is computed, as with `no-cache'.
    Msg = #{ <<"with-commitments">> => 3 },
    NoCache = #{ <<"cache-control">> => [<<"no-cache">>] },
    ?assertEqual(
        hb_ao:resolve(Msg, <<"with-commitments">>, NoCache),
        hb_ao:resolve(Msg, <<"with-commitments">>, #{})
    ),
    % `with_commitments' maps to the same function, and is computed too.
    Underscored = #{ <<"with_commitments">> => 3 },
    ?assertEqual(
        hb_ao:resolve(Underscored, <<"with_commitments">>, NoCache),
        hb_ao:resolve(Underscored, <<"with_commitments">>, #{})
    ),
    % The device answers the key `info': it is not read from the message.
    ?assertNot(
        hb_device:is_direct_key_access(#{ <<"info">> => 4 }, <<"info">>, #{})
    ),
    % The `info' of a message is the info of its device, for `docs@1.0'.
    ?assertMatch(
        {ok,
            #{ <<"device">> := <<"docs@1.0">>, <<"for">> := <<"message@1.0">> }
        },
        hb_ao:resolve(#{ <<"info">> => <<"literal">> }, <<"info">>, #{})
    ).

remove_test() ->
	Msg = #{ <<"key1">> => <<"Value1">>, <<"key2">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"key2">> := <<"Value2">> }},
		hb_ao:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"item">> => <<"key1">> },
            #{ <<"hashpath">> => ignore }
        )
    ),
	?assertMatch({ok, #{}},
		hb_ao:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"items">> => [<<"key1">>, <<"key2">>] },
            #{ <<"hashpath">> => ignore }
        )
    ).

set_committed_values_test_() ->
    [
        {binary_to_list(Device), fun() ->
            Opts = #{
                <<"store">> => hb_test_utils:test_store(),
                <<"priv-wallet">> => hb:wallet(),
                <<"hashpath">> => ignore
            },
            Msg = hb_message:commit(
                #{
                    <<"content-type">> => <<"text/plain">>,
                    <<"data">> => <<"Original body">>
                },
                Opts,
                Device
            ),
            ?assert(hb_message:verify(Msg, all, Opts)),
            Missing = Msg#{
                <<"data">> => {link, hb_util:human_id(crypto:strong_rand_bytes(32)), #{}}
            },
            {ok, Replaced} = hb_ao:resolve(
                Missing,
                #{
                    <<"path">> => <<"set">>,
                    <<"data">> => <<"Changed body">>,
                    <<"set-mode">> => <<"explicit">>
                },
                Opts
            ),
            ?assertNot(hb_maps:is_key(<<"commitments">>, Replaced)),
            {ok, _} = hb_cache:write(Msg, Opts),
            {ok, Linked} = hb_cache:read(hb_message:id(Msg, signed, Opts), Opts),
            lists:foreach(
                fun(Base) ->
                    lists:foreach(
                        fun({Key, Value}) ->
                            Same = hb_ao:set(Base, #{ Key => Value }, Opts),
                            ?assert(hb_maps:is_key(<<"commitments">>, Same)),
                            ?assert(hb_message:verify(Same, all, Opts)),
                            lists:foreach(
                                fun(NewValue) ->
                                    Changed = hb_ao:set(Base, #{ Key => NewValue }, Opts),
                                    ?assertNot(hb_maps:is_key(<<"commitments">>, Changed))
                                end,
                                [<<"changed">>, '_', unset]
                            )
                        end,
                        [{<<"content-type">>, <<"text/plain">>},
                         {<<"data">>, <<"Original body">>}]
                    )
                end,
                [Msg, Linked]
            )
        end}
    ||
        Device <- [<<"httpsig@1.0">>, <<"ans104@1.0">>, <<"tx@1.0">>]
    ].

set_conflicting_keys_test() ->
	Base = #{ <<"dangerous">> => <<"Value1">> },
	Req = #{ <<"path">> => <<"set">>, <<"dangerous">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"dangerous">> := <<"Value2">> }},
		hb_ao:resolve(Base, Req, #{})).

set_new_key_drops_commitments_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => hb:wallet()
    },
    Signed =
        hb_message:commit(#{ <<"a">> => <<"1">> }, Opts, <<"httpsig@1.0">>),
    {ok, Updated} = hb_ao:resolve(
        Signed,
        #{ <<"path">> => <<"set">>, <<"b">> => <<"2">> },
        Opts
    ),
    ?assertEqual([], hb_message:signers(Updated, Opts)),
    {ok, Canonical} = hb_message:with_only_committed(Updated, Opts),
    ?assertEqual(<<"2">>, maps:get(<<"b">>, Canonical)),
    % Setting or removing a key that only one commitment covers drops both.
    PartiallySigned =
        hb_message:commit(
            hb_message:commit(#{ <<"a">> => <<"1">>, <<"b">> => <<"2">> }, Opts),
            Opts,
            #{ <<"committed">> => [<<"a">>] }
        ),
    ?assertEqual(
        [<<"a">>, <<"b">>],
        lists:sort(
            hb_message:committed(PartiallySigned, #{ <<"ids">> => <<"any">> }, Opts)
        )
    ),
    % `with_only_committed' with `any' keeps both keys and both commitments.
    ?assertEqual(
        {ok, PartiallySigned},
        hb_message:with_only_committed(PartiallySigned, any, Opts)
    ),
    % With a list of IDs, the keys that every listed commitment covers are kept.
    ?assertEqual(
        hb_message:with_only_committed(PartiallySigned, Opts),
        hb_message:with_only_committed(
            PartiallySigned,
            maps:keys(maps:get(<<"commitments">>, PartiallySigned)),
            Opts
        )
    ),
    % With a spec, the keys that `committed/3' gives for it are kept.
    ?assertEqual(
        {ok, PartiallySigned},
        hb_message:with_only_committed(
            PartiallySigned,
            #{ <<"ids">> => <<"any">> },
            Opts
        )
    ),
    % A parent's cache write keeps both keys and both commitments.
    {ok, ParentID} = hb_cache:write(#{ <<"c">> => PartiallySigned }, Opts),
    {ok, Child} = hb_cache:read(<<ParentID/binary, "/c">>, Opts),
    ?assertEqual(
        PartiallySigned,
        hb_private:reset(hb_cache:ensure_all_loaded(Child, Opts))
    ),
    {ok, Set} = hb_ao:resolve(
        PartiallySigned,
        #{ <<"path">> => <<"set">>, <<"b">> => <<"3">> },
        Opts
    ),
    ?assert(hb_message:verify(Set, all, Opts)),
    {ok, Removed} = hb_ao:resolve(
        PartiallySigned,
        #{ <<"path">> => <<"remove">>, <<"item">> => <<"b">> },
        Opts
    ),
    ?assert(hb_message:verify(Removed, all, Opts)).

unset_with_set_test() ->
	Base = #{ <<"dangerous">> => <<"Value1">> },
	Req = #{ <<"path">> => <<"set">>, <<"dangerous">> => unset },
	?assertMatch({ok, Res} when ?IS_EMPTY_MESSAGE(Res),
		hb_ao:resolve(Base, Req, #{ <<"hashpath">> => ignore })).

deep_unset_test() ->
    Opts = #{ <<"hashpath">> => ignore },
    Base = #{
        <<"test-key1">> => <<"Value1">>,
        <<"deep">> => #{
            <<"test-key2">> => <<"Value2">>,
            <<"test-key3">> => <<"Value3">>
        }
    },
    Req = hb_ao:set(Base, #{ <<"deep/test-key2">> => unset }, Opts),
    ?assertEqual(#{
            <<"test-key1">> => <<"Value1">>,
            <<"deep">> => #{ <<"test-key3">> => <<"Value3">> }
        },
        Req
    ),
    Res = hb_ao:set(Req, <<"deep/test-key3">>, unset, Opts),
    ?assertEqual(#{
            <<"test-key1">> => <<"Value1">>,
            <<"deep">> => #{}
        },
        Res
    ),
    Msg4 = hb_ao:set(Res, #{ <<"deep">> => unset }, Opts),
    ?assertEqual(#{ <<"test-key1">> => <<"Value1">> }, Msg4).

set_ignore_undefined_test() ->
	Base = #{ <<"test-key">> => <<"Value1">> },
	Req = #{ <<"path">> => <<"set">>, <<"test-key">> => undefined },
	?assertEqual(#{ <<"test-key">> => <<"Value1">> },
		hb_private:reset(hb_util:ok(set(Base, Req, #{ <<"hashpath">> => ignore })))).

verify_test_() ->
	{foreach, fun () -> ok end, fun (_) -> ok end, [
		{"RSA", fun () -> test_verify(?RSA_KEY_TYPE) end},
		{"EDDSA", fun () -> test_verify(?EDDSA_KEY_TYPE) end},
        {"Solana", fun () -> test_unsupported_key(?SOLANA_KEY_TYPE) end},
        {"Ethereum", fun () -> test_verify(?ETHEREUM_KEY_TYPE) end}
	]}.

%% @doc The commitment spec for a key type: `httpsig@1.0' signs with RSA keys,
%% `ans104@1.0' with the others it supports.
commitment_spec(?RSA_KEY_TYPE) -> #{};
commitment_spec(?EDDSA_KEY_TYPE) ->
    #{ <<"device">> => <<"ans104@1.0">>, <<"type">> => ?EDDSA_SIGN_TYPE };
commitment_spec(?ETHEREUM_KEY_TYPE) ->
    #{ <<"device">> => <<"ans104@1.0">>, <<"type">> => ?ETHEREUM_SIGN_TYPE }.

%% @doc No commitment device signs with the key type, so the commitment is
%% refused rather than made without a verifiable signature.
test_unsupported_key(KeyType) ->
    Wallet = ar_wallet:new(KeyType),
    ?assertThrow(
        {cannot_commit, {error, #{ <<"error">> := <<"unsupported-key-type">> }}, _},
        hb_message:commit(
            #{ <<"a">> => <<"b">> },
            #{ <<"priv-wallet">> => Wallet }
        )
    ).

test_verify(KeyType) ->
    Unsigned = #{ <<"a">> => <<"b">> },
    Wallet = ar_wallet:new(KeyType),
    Signed =
        hb_message:commit(
            Unsigned,
            #{ <<"priv-wallet">> => Wallet },
            commitment_spec(KeyType)
        ),
    ?event_debug({signed, Signed}),
    BadSigned = Signed#{ <<"a">> => <<"c">> },
    ?event_debug({bad_signed, BadSigned}),
    ?assertEqual(false, hb_message:verify(BadSigned)),
    % The message is the target of its own `verify' key, so a request without
    % `committers' verifies every commitment it carries.
    ?assertEqual({ok, true},
        hb_ao:resolve(
            Signed,
            #{ <<"path">> => <<"verify">> },
            #{ <<"hashpath">> => ignore }
        )
    ),
    % A request's `target', such as an item's, does not select the message.
    ?assertEqual({ok, true},
        hb_ao:resolve(
            Signed,
            #{ <<"path">> => <<"verify">>, <<"target">> => <<"process-id">> },
            #{ <<"hashpath">> => ignore }
        )
    ),
    % A `message-subject' that gives a key the request lacks is refused.
    ?assertError(
        {badmatch, {error, not_found}},
        hb_ao:resolve(
            Signed,
            #{
                <<"path">> => <<"verify">>,
                <<"message-subject">> => <<"missing">>
            },
            #{ <<"hashpath">> => ignore }
        )
    ),
    ?assertEqual({ok, false},
        hb_ao:resolve(
            BadSigned,
            #{ <<"path">> => <<"verify">> },
            #{ <<"hashpath">> => ignore }
        )
    ),
    % A commitment relabelled `json@1.0' is verified by the `httpsig@1.0'
    % codec its device names -- `true' for an `httpsig@1.0' signature. A
    % commitment relabelled `message@1.0' or `meta@1.0' has a device
    % with no commitment scheme of its own and does not verify.
    [{ID, Commitment}] = maps:to_list(maps:get(<<"commitments">>, Signed)),
    Relabelled =
        fun(Device) ->
            Signed#{
                <<"commitments">> =>
                    #{ ID => Commitment#{ <<"commitment-device">> => Device } }
            }
        end,
    case maps:get(<<"commitment-device">>, Commitment) of
        <<"httpsig@1.0">> ->
            ?assert(hb_message:verify(Relabelled(<<"json@1.0">>), all));
        _ -> ok
    end,
    lists:foreach(
        fun(Device) ->
            ?assertNot(hb_message:verify(Relabelled(Device), all))
        end,
        [<<"message@1.0">>, <<"meta@1.0">>]
    ).

%% @doc A commitment of no keys verifies after a round trip through
%% `flat@1.0', which writes its empty `committed' list as no key.
verify_without_committed_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Committed = hb_message:commit(#{}, Opts, #{ <<"type">> => <<"unsigned">> }),
    Flat =
        hb_message:convert(
            Committed,
            <<"flat@1.0">>,
            <<"structured@1.0">>,
            Opts
        ),
    Decoded =
        hb_message:convert(Flat, <<"structured@1.0">>, <<"flat@1.0">>, Opts),
    ?assert(hb_message:verify(Decoded, all, Opts)),
    ?assertEqual([], hb_message:committed(Decoded, all, Opts)).

%% @doc A signed commitment over no keys commits none of the keys of a message
%% that it is grafted onto.
vacuous_signed_commitment_test_() ->
    [
        {binary_to_list(Device), fun() ->
            Opts = #{ <<"priv-wallet">> => ar_wallet:new() },
            Committed = hb_message:commit(#{}, Opts, Device),
            Grafted =
                #{
                    <<"data">> => <<"unsigned content">>,
                    <<"commitments">> => maps:get(<<"commitments">>, Committed)
                },
            ?assertEqual([], hb_message:committed(Grafted, all, Opts))
        end}
    ||
        Device <- [<<"httpsig@1.0">>, <<"ans104@1.0">>, <<"tx@1.0">>]
    ].

%% @doc Committing to a message keeps its private element as it is: a link in
%% it is not loaded, even when the node does not have its message.
commit_keeps_private_links_test() ->
    Opts = #{ <<"store">> => hb_test_utils:test_store() },
    Missing =
        {link,
            <<"ao:", (hb_util:human_id(crypto:strong_rand_bytes(32)))/binary>>,
            #{}
        },
    Priv = #{ <<"request">> => #{ <<"x">> => Missing } },
    Committed =
        hb_message:commit(
            #{ <<"a">> => <<"b">>, <<"priv">> => Priv },
            Opts,
            #{ <<"type">> => <<"unsigned">> }
        ),
    ?assertEqual(Priv, maps:get(<<"priv">>, Committed)).

set_nested_link_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store(hb_store_lmdb)] },

    Base = #{
        <<"balances">> => #{
            <<"device">> => <<"trie@1.0">>,
            <<"aa">> => <<"100">>,
            <<"bb">> => <<"200">>,
            <<"cc">> => <<"300">>
        },
        <<"other-key">> => <<"other-value">>
    },
    {ok, Path} = hb_cache:write(Base, Opts),
    {ok, LinkifiedBase} = hb_cache:read(Path, Opts),
    Req = #{
        <<"other-key">> => <<"new-value">>,
        <<"balances">> => #{
            <<"ab">> => <<"150">>
        }
    },
    {ok, Result} = set(LinkifiedBase, Req, Opts),
    Expected =
    #{
        <<"other-key">> => <<"new-value">>,
        <<"balances">> => #{
            <<"device">> => <<"trie@1.0">>,
            <<"a">> => #{
                <<"a">> => <<"100">>,
                <<"b">> => <<"150">>
            },
            <<"bb">> => <<"200">>,
            <<"cc">> => <<"300">>
        }
    },
    Matches = hb_message:match(Expected, Result, strict, Opts),
    ?assert(Matches).
