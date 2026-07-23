%%% @doc The identity device: For non-reserved keys, it simply returns a key 
%%% from the message as it is found in the message's underlying Erlang map. 
%%% Private keys (`priv[.*]') are not included.
%%% Reserved keys are: `id', `commitments', `committers', `keys', `path', 
%%% `set', `remove', `get', and `verify'. Exported handlers or helper APIs
%%% describe the behaviour of these keys when they are set.
-module(dev_message).
%%% Base AO-Core state manipulation functions.
-export([info/0, keys/3, set/3, id/3, vary/3, schema/3]).
%%% Commitments API keys:
-export([commit/3, committed/3, committers/3, verify/3]).
%%% Non-protocol enforced keys:
-export([index/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-define(DEFAULT_ID_DEVICE, <<"httpsig@1.0">>).
-define(DEFAULT_ATT_DEVICE, <<"httpsig@1.0">>).

%% @doc Return the info for the identity device.
info() ->
    #{
        default => fun default_accessor/4
    }.

% router(#{ <<"path">> := Key }, #{ <<"path">> := <<"path">> }, _Opts) -> {ok, Key};
% router(_, #{ <<"path">> := <<"path">> }, _Opts) -> {error, not_found};
% router(Base, Req, Opts) ->
%     case hb_ao:raw(Req, #{ <<"path">> => <<"path">> }, Opts) of
%         {error, not_found} -> set(Base, Req, Opts);
%         {ok, Key} -> default_accessor(Key, Base, Req, Opts)
%     end.

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
id(Base, _, NodeOpts) when is_binary(Base) ->
    % Return the hashpath of the message in native format, to match the native
    % format of the message ID return.
    {ok, hb_util:human_id(hb_path:hashpath(Base, NodeOpts))};
id(List, Req, NodeOpts) when is_list(List) ->
    % Return the list of IDs for a list of messages.
    SourceSpec =
        hb_message:add_bundle_hint(
            #{ <<"device">> => <<"structured@1.0">> },
            Req#{ <<"device">> => ?DEFAULT_ID_DEVICE },
            NodeOpts
        ),
    id(hb_message:convert(List, tabm, SourceSpec, NodeOpts), Req, NodeOpts);
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
            calculate_id(
                hb_maps:without([<<"commitments">>], Base, IDOpts),
                Req,
                IDOpts
            );
        IDs ->
            % Accumulate the relevant IDs into a single value. This is performed 
            % by module arithmetic of each of the IDs. The effect of this is that:
            % 1. New IDs can be added to the combined ID without requiring any
            %    recalculation of other IDs.
            % 2. New IDs can be added in any order, and will compare to the same
            %    value as if they were added in other orders.
            % 3. Subsequently, combined IDs cannot be used to express ordering of
            %    the underlying commitments.
            % This works for single IDs as well as lists of IDs, because the 
            % accumulation function starts with a buffer of zero encoded as a 
            % 256-bit binary. Subsequently, a single ID on its own 'accumulates' 
            % to itself.
            ?event_debug(debug_id, returning_existing_ids),
            {ok,
                hb_util:human_id(
                    hb_crypto:accumulate(
                        lists:map(fun hb_util:native_id/1, IDs)
                    )
                )
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
        case hb_device:module(#{ <<"device">> => IDDev }, NodeOpts) of
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

%% @doc Return the committers of a message that are present in the given request.
committers(#{ <<"commitments">> := Commitments }, _, NodeOpts) ->
    {ok,
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
        )
    };
committers(_, _, _) ->
    {ok, []}.

%% @doc Commit to a message, using the `commitment-device' key to specify the
%% device that should be used to commit to the message. If the key is not set,
%% the default device (`httpsig@1.0') is used.
commit(Self, Req, Opts) ->
    {ok, Base} = hb_message:find_target(Self, Req, Opts),
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
    Loaded =
        ensure_commitments_loaded(
            hb_message:convert(Base, tabm, SourceSpec, CommitOpts),
            Opts
        ),
    {ok, Committed} =
        hb_ao:raw(
            AttDev,
            <<"commit">>,
            Loaded,
            Req#{ <<"type">> => maps:get(<<"type">>, Req, <<"signed">>) },
            CommitOpts
        ),
    {ok, hb_message:convert(Committed, <<"structured@1.0">>, tabm, CommitOpts)}.

%% @doc Verify a message. By default, all commitments are verified. The
%% `committers' key in the request can be used to specify that only the 
%% commitments from specific committers should be verified. Similarly, specific
%% commitments can be specified using the `commitments' key.
verify(Self, Req, Opts) ->
    % Get the target message of the verification request.
    {ok, RawBase} = hb_message:find_target(Self, Req, Opts),
    CommitmentBase = ensure_commitments_loaded(RawBase, Opts),
    Commitments = maps:get(<<"commitments">>, CommitmentBase, #{}),
    IDsToVerify = commitment_ids_from_request(CommitmentBase, Req, Opts),
    % Generate the new commitment request base messsage by removing the keys
    % used by this function (path, committers, commitments) and returning the
    % remaining keys. This message will then be merged with each commitment
    % message to generate the final request, allowing the caller to pass
    % additional keys to the commitment device.
    ReqBase =
        maps:without(
            [
                <<"path">>,
                <<"committers">>,
                <<"commitments">>,
                <<"commitment-ids">>
            ],
            Req
        ),
    % Verify the commitments. Stop execution if any fail.
    Res =
        lists:all(
            fun(CommitmentID) ->
                Commitment = maps:merge(
                    ReqBase,
                    maps:get(CommitmentID, Commitments)
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
                Base = hb_message:convert(
                    CommitmentBase, tabm, SourceSpec, Opts),
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
%% parent message.
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
    hb_ao:raw(AttDev, <<"verify">>, Base, Commitment, Opts).

%% @doc Return the list of committed keys from a message.
committed(Self, Req, Opts) ->
    % Get the target message of the verification request and ensure its 
    % commitments are loaded.
    {ok, RawBase} =
        hb_message:find_target(
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
                Commitment = maps:get(CommitmentID, Commitments),
                % The committed keys will be a TABM encoded numbered map
                % so we must decode it to its underlying list of normalized keys
                % for comparison purposes.
                hb_util:message_to_ordered_list(
                    maps:get(<<"committed">>, Commitment),
                    Opts
                )
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
        lists:filter(
            fun(Key) ->
                lists:all(
                    fun(CommittedKeys) -> lists:member(Key, CommittedKeys) end,
                    CommitmentKeys
                )
            end,
            AllCommittedKeys
        ),
    % Remove any `+link` suffixes from TABM-form committed keys if the `raw` flag
    % is not set. This means that callers to `committed/3' will receive a list of
    % keys that they can match  against the 'normal' representation of the message
    % in devices, etc., without exposure to TABM-specifics. If `raw' is set, the
    % recipient receives the `committed` list in its unprocessed form.
    CommittedNormalizedKeys =
        case maps:get(<<"raw">>, Req, false) of
            true -> OnlyCommittedKeys;
            false ->
                lists:map(
                    fun hb_link:remove_link_specifier/1,
                    OnlyCommittedKeys
                )
        end,
    ?event_debug(debug_commitments, {only_committed_keys, CommittedNormalizedKeys}),
    {ok, CommittedNormalizedKeys}.

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
%% may specify `all' or `none' for each group. If no specifiers are provided,
%% the default is `all' for commitments -- also implying `all' for committers.
commitment_ids_from_request(Base, Req, Opts) ->
    Commitments = maps:get(<<"commitments">>, Base, #{}),
    ReqCommitters =
        case maps:get(<<"committers">>, Req, <<"none">>) of
            X when is_list(X) -> X;
            CommitterDescriptor -> hb_ao:normalize_key(CommitterDescriptor)
        end,
    RawReqCommitments = maps:get(<<"commitment-ids">>, Req, <<"none">>),
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
                commitment_ids_from_committers(CommitterAddrs, Commitments, Opts)
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
set(Base, Req = #{ <<"set">> := <<"deep">> }, Opts) ->
    set_layer(Base, nested_set_values(Base, set_values(Req, Opts), Opts), Opts);
set(Base, NewValues, Opts) ->
    Patch = nested_set_values(Base, set_values(NewValues, Opts), Opts),
    set_layer(Base, Patch, Opts).

set_layer(Base, Patch, Opts) ->
    case lists:partition(fun hb_private:is_private/1, maps:keys(Base)) of
        {[], _} ->
            {ok, Patch#{ <<"...">> => Base }};
        {PrivKeys, _} ->
            {
                ok,
                (maps:merge(Patch, carried_private_keys(Base, PrivKeys)))#{
                    <<"...">> => maps:without(PrivKeys, Base)
                }
            }
    end.

nested_set_values(Base, Patch, Opts) ->
    maps:map(
        fun(Key, Nested) when is_map(Nested) or ?IS_LINK(Nested) ->
            case hb_maps:find(Key, Base, Opts) of
                {ok, NestedBase} when is_map(NestedBase) or ?IS_LINK(NestedBase) ->
                    % Use the child value's device so deep extension works for
                    % message maps, tries, and other nested AO values.
                    hb_ao:deep_set(NestedBase, Nested, Opts);
                _ -> Nested
            end;
            (_Key, NewValue) -> NewValue
        end,
        Patch
    ).

set_values(Req, Opts) ->
    lists:foldl(
        fun(Key, Acc) ->
            case set_value_at(Key, Req, Opts) of
                {ok, undefined} -> Acc;
                {ok, Value} -> patch_set(Key, Value, Acc, Opts);
                error -> Acc
            end
        end,
        #{},
        set_candidate_keys(Req, Opts)
    ).

set_candidate_keys(Req, Opts) when is_map(Req) ->
    Direct =
        [
            Key
        ||
            {Key, _Value} <- hb_maps:to_list(hb_message:uncommitted(Req, Opts), Opts),
            set_value_key(Key)
        ],
    Inherited =
        case hb_maps:find(<<"...">>, Req, Opts) of
            {ok, Ancestor} ->
                case ancestor_message(Ancestor, Opts) of
                    {ok, AncestorMsg} -> set_candidate_keys(AncestorMsg, Opts);
                    error -> []
                end;
            error -> []
        end,
    lists:usort(Direct ++ Inherited);
set_candidate_keys(_Req, _Opts) ->
    [].

set_value_at(Key, Msg, Opts) when is_map(Msg) ->
    case hb_maps:find(Key, Msg, Opts) of
        {ok, Value} ->
            {ok, Value};
        error ->
            case hb_maps:find(<<"...">>, Msg, Opts) of
                {ok, Ancestor} ->
                    case ancestor_message(Ancestor, Opts) of
                        {ok, AncestorMsg} -> set_value_at(Key, AncestorMsg, Opts);
                        error -> error
                    end;
                error -> error
            end
    end;
set_value_at(_Key, _Msg, _Opts) ->
    error.

set_value_key(<<"set">>) -> false;
set_value_key(<<"path">>) -> false;
set_value_key(<<"...">>) -> false;
set_value_key(Key) -> not hb_private:is_private(Key).

patch_set(Key, Value, Acc, Opts) ->
    case hb_path:term_to_path_parts(Key, Opts) of
        undefined -> Acc;
        Parts -> do_patch_set(Parts, Value, Acc)
    end.

do_patch_set([Key], Value, Acc) ->
    Acc#{ Key => Value };
do_patch_set([Key | Rest], Value, Acc) ->
    Child =
        case maps:get(Key, Acc, #{}) of
            Msg when is_map(Msg) -> Msg;
            _ -> #{}
        end,
    Acc#{ Key => do_patch_set(Rest, Value, Child) }.

carried_private_keys(Base, PrivKeys) ->
    lists:foldl(
        fun(Key, Acc) ->
            case carried_private_value(Key, maps:get(Key, Base)) of
                {keep, Value} -> Acc#{ Key => Value };
                drop -> Acc
            end
        end,
        #{},
        PrivKeys
    ).

carried_private_value(<<"priv">>, Priv) when is_map(Priv) ->
    Clean = maps:without([<<"hashpath">>, hashpath], Priv),
    case map_size(Clean) of
        0 -> drop;
        _ -> {keep, Clean}
    end;
carried_private_value(_Key, Value) ->
    {keep, Value}.

%% @doc Get the public keys of a message.
-spec keys(#{ _ => _ }, #{ keys => _, _ => _ }, _) -> {ok, [binary()]}.
keys(Msg, Req, Opts) when is_list(Msg) ->
    case hb_ao:normalize_keys(Msg, Opts) of
        NormMsg when is_map(NormMsg) -> keys(NormMsg, Req, Opts);
        _ -> throw(badarg)
    end;
keys(Msg, #{ <<"keys">> := <<"deep">> }, Opts) when is_map(Msg) ->
    Inherited =
        case hb_maps:find(<<"...">>, Msg, Opts) of
            {ok, Extension} ->
                case ancestor_message(Extension, Opts) of
                    {ok, Ancestor} -> deep_keys(Ancestor, Opts);
                    error -> []
                end;
            error -> []
        end,
    MaskedKeys =
        [
            Key
        ||
            {Key, Value} <- hb_maps:to_list(Msg, Opts),
            unset_value(Value, Opts)
        ],
    Hidden = [<<"commitments">> | MaskedKeys],
    InheritedPublic = [Key || Key <- Inherited, not lists:member(Key, Hidden)],
    DirectKeys =
        [
            Key
        ||
            {Key, Value} <- hb_maps:to_list(Msg, Opts),
            Key =/= <<"...">>,
            visible_key(Key, Value, Opts)
        ],
    {ok, lists:usort(DirectKeys ++ InheritedPublic)};
keys(Msg, _Req, Opts) ->
    {
        ok,
        [
            Key
        ||
            {Key, Value} <-
                hb_maps:to_list(hb_message:uncommitted(Msg, Opts), Opts),
            visible_key(Key, Value, Opts)
        ]
    }.

deep_keys(Msg, Opts) ->
    case keys(Msg, #{ <<"keys">> => <<"deep">> }, Opts) of
        {ok, Keys} -> Keys;
        _ -> []
    end.

visible_key(Key, Value, Opts) ->
    Key =/= <<"...">>
        andalso not hb_private:is_private(Key)
        andalso not unset_value(Value, Opts).

unset_value(unset) -> true;
unset_value(<<"unset">>) -> true;
unset_value(_) -> false.

unset_value(Value, _Opts) when Value =:= unset; Value =:= <<"unset">> ->
    true;
unset_value(Value, Opts) when ?IS_LINK(Value) ->
    try unset_value(hb_cache:ensure_loaded(Value, Opts)) of
        IsUnset -> IsUnset
    catch
        _:_ -> false
    end;
unset_value(_Value, _Opts) ->
    false.

ancestor_message(Ancestor, _Opts) when is_map(Ancestor) ->
    {ok, Ancestor};
ancestor_message(Ancestor, Opts) when ?IS_LINK(Ancestor) ->
    case hb_cache:ensure_loaded(Ancestor, Opts) of
        Msg when is_map(Msg) -> {ok, Msg};
        _ -> error
    end;
ancestor_message(Ancestor, Opts) when is_binary(Ancestor) ->
    case binary:match(Ancestor, <<"/">>) of
        {_, _} -> hashpath_ancestor(Ancestor, Opts);
        nomatch -> cached_ancestor(Ancestor, Opts)
    end;
ancestor_message(_Ancestor, _Opts) ->
    error.

cached_ancestor(Ancestor, Opts) ->
    case hb_cache:read(Ancestor, Opts) of
        {ok, Msg} when is_map(Msg) -> {ok, Msg};
        _ -> error
    end.

hashpath_ancestor(Ancestor, Opts) ->
    case hb_hashpath:load(Ancestor, Opts) of
        {ok, Msg} when is_map(Msg) -> {ok, Msg};
        _ -> error
    end.

%% @doc Return the value associated with the key as it exists in the message's
%% underlying Erlang map. First check the public keys, then check case-
%% insensitively if the key is a binary.
default_accessor(<<"*">>, Msg, _Req, Opts) ->
    MaterializeOpts =
        Opts#{
            <<"hashpath">> => ignore,
            <<"spawn-worker">> => false,
            <<"caching-schema">> => true
        },
    materialized_message(Msg, MaterializeOpts);
default_accessor(Key, Msg, Req, Opts) when is_list(Msg) ->
    case hb_ao:normalize_keys(Msg, Opts) of
        NormMsg when is_map(NormMsg) -> default_accessor(Key, NormMsg, Req, Opts);
        _ -> {error, not_found}
    end;
default_accessor(Key, Msg, Req, Opts) ->
    case hb_private:is_private(Key) of
        true -> {error, not_found};
        false -> message_lookup(Key, Msg, Opts)
    end.

message_lookup(Key, Msg, Opts) ->
    case hb_maps:find(Key, Msg, Opts) of
        {ok, Value} when Value =:= unset; Value =:= <<"unset">> ->
            {error, not_found};
        {ok, Value} ->
            {ok, Value};
        error ->
            case hb_maps:find(<<"...">>, Msg, Opts) of
                {ok, Ancestor} ->
                    case ancestor_message(Ancestor, Opts) of
                        {ok, AncestorMsg} -> message_lookup(Key, AncestorMsg, Opts);
                        error -> {error, not_found}
                    end;
                error -> {error, not_found}
            end
    end.

materialized_message(Msg, Opts) ->
    case keys(Msg, #{ <<"keys">> => <<"deep">> }, Opts) of
        {ok, Keys} ->
            {ok,
                maps:from_list(
                    lists:filtermap(
                        fun(Key) ->
                            case hb_ao:resolve(Msg, Key, Opts) of
                                {ok, Value} -> {true, {Key, materialized_value(Value, Opts)}};
                                _ -> false
                            end
                        end,
                        Keys
                    )
                )
            };
        Error ->
            Error
    end.

materialized_value(Value, Opts) when is_map(Value) ->
    case materialized_message(Value, Opts) of
        {ok, Surface} -> Surface;
        _ -> hb_private:reset(Value)
    end;
materialized_value(Value, _Opts) ->
    hb_private:reset(Value).

%% @doc Determines the schema for the resolution of a `Base/Request` pair and
%% applies it to the inputs. Returns `base` and `request` submessages, as well
%% as the resolvable function Erlang function at `priv/function` if it was found
%% during the process of `vary`ing.
-spec vary(
    #{ _ => _ },
    #{ _ => _ },
    #{}) -> {ok, #{}} | {error, binary()}.
vary(Base, Req, Opts) ->
    maybe
        {ok, Ctx} ?=
            case hb_private:get(<<"function">>, Req, not_found, Opts) of
                not_found ->
                    % No function given: derive the key to resolve one.
                    maybe
                        {ok, Key} ?=
                            case Req of
                                #{ <<"vary">> := KeyToVaryOn } ->
                                    {ok, KeyToVaryOn};
                                #{ <<"path">> := <<"vary">> } ->
                                    {error, <<"Cannot vary the `vary` path.">>};
                                #{ <<"path">> := PathKey } ->
                                    {ok, PathKey};
                                _ ->
                                    {error, <<"invalid-vary-request">>}
                            end,
                        hb_device:add_resolver(
                            #{
                                <<"base">> => Base,
                                <<"key">> => Key,
                                <<"request">> => Req
                            },
                            Opts
                        )
                    end;
                Fun ->
                    % The executor is already known: no key is required.
                    {ok,
                        #{
                            <<"base">> => Base,
                            <<"request">> => Req,
                            <<"priv">> => #{ <<"function">> => Fun }
                        }
                    }
            end,
        hb_types:vary(Ctx, Opts)
    end.

%% @doc Returns the device schema for a `Base` message.
-spec schema(_, _, _) -> {ok, undefined | #{}}.
schema(#{ <<"device">> := Device }, _, Opts) ->
    case hb_types:extract(Device, Opts) of
        {ok, Schema} -> {ok, Schema};
        _ -> {ok, undefined}
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

star_materialization_strips_nested_private_state_test() ->
    Opts = #{ <<"hashpath">> => ignore },
    Msg = #{
        <<"plain">> => <<"yes">>,
        <<"visible">> =>
            hb_private:set_priv(
                #{ <<"public">> => <<"ok">> },
                #{ <<"secret">> => <<"nope">> }
            )
    },
    ?assertEqual(
        {ok, #{
            <<"plain">> => <<"yes">>,
            <<"visible">> => #{ <<"public">> => <<"ok">> }
        }},
        default_accessor(<<"*">>, Msg, #{}, Opts)
    ).

star_materialization_projects_nested_extensions_test() ->
    Opts = #{ <<"hashpath">> => ignore },
    Parent = #{ <<"a">> => 1, <<"b">> => 1 },
    Msg = #{ <<"nested">> => #{ <<"b">> => 2, <<"...">> => Parent } },
    ?assertEqual(
        {ok, #{ <<"nested">> => #{ <<"a">> => 1, <<"b">> => 2 } }},
        default_accessor(<<"*">>, Msg, #{}, Opts)
    ).

key_from_device_test() ->
    ?assertEqual({ok, 1}, hb_ao:resolve(#{ <<"a">> => 1 }, <<"a">>, #{})).

list_key_access_normalizes_to_indexed_message_test() ->
    Opts = #{ <<"hashpath">> => ignore },
    Msg = [<<"A">>, <<"B">>, <<"C">>],
    ?assertEqual(<<"A">>, hb_ao:get(1, Msg, Opts)),
    ?assertEqual(<<"B">>, hb_ao:get(2, Msg, Opts)),
    ?assertEqual(<<"C">>, hb_ao:get(3, Msg, Opts)).

keys_do_not_materialize_lazy_values_test() ->
    Opts = #{ <<"hashpath">> => ignore },
    Msg = #{ <<"lazy">> => {link, <<"missing-id">>, #{}}},
    ?assertEqual({ok, [<<"lazy">>]}, hb_ao:resolve(Msg, keys, Opts)).

remove_test() ->
	Msg = #{ <<"key1">> => <<"Value1">>, <<"key2">> => <<"Value2">> },
    Opts = #{ <<"hashpath">> => ignore },
    RemoveOne = hb_ao:remove(Msg, <<"key1">>, Opts),
	?assertEqual({error, not_found}, hb_ao:resolve(RemoveOne, <<"key1">>, Opts)),
	?assertEqual({ok, <<"Value2">>}, hb_ao:resolve(RemoveOne, <<"key2">>, Opts)),
    RemoveBoth = hb_ao:remove(RemoveOne, <<"key2">>, Opts),
	?assertEqual({ok, []}, hb_ao:resolve(RemoveBoth, keys, Opts)),
	?assertEqual({error, not_found}, hb_ao:resolve(RemoveBoth, <<"key1">>, Opts)),
	?assertEqual({error, not_found}, hb_ao:resolve(RemoveBoth, <<"key2">>, Opts)).

remove_is_not_device_handler_test() ->
    Msg = #{ <<"key1">> => <<"Value1">> },
    Opts = #{ <<"hashpath">> => ignore },
    ?assertEqual(
        {error, not_found},
        hb_ao:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"item">> => <<"key1">> },
            Opts
        )
    ).

set_conflicting_keys_test() ->
	Base = #{ <<"dangerous">> => <<"Value1">> },
	Req = #{ <<"path">> => <<"set">>, <<"dangerous">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"dangerous">> := <<"Value2">> }},
		hb_ao:resolve(Base, Req, #{})).

reserved_set_key_dispatches_operation_test() ->
    Base = #{ <<"set">> => <<"literal">>, <<"x">> => 1 },
    Req = #{ <<"path">> => <<"set">>, <<"x">> => 2 },
    Opts = #{ <<"hashpath">> => ignore },
    {ok, Res} = hb_ao:resolve(Base, Req, Opts),
    ?assertEqual(2, maps:get(<<"x">>, Res)),
    ?assertEqual(Base, maps:get(<<"...">>, Res)).

unset_with_set_test() ->
	Base = #{ <<"dangerous">> => <<"Value1">> },
	Req = #{ <<"path">> => <<"set">>, <<"dangerous">> => unset },
    Opts = #{ <<"hashpath">> => ignore },
    {ok, Res} = hb_ao:resolve(Base, Req, Opts),
	?assertEqual({error, not_found}, hb_ao:resolve(Res, <<"dangerous">>, Opts)),
	?assertEqual({ok, []}, hb_ao:resolve(Res, keys, Opts)).

binary_unset_masks_active_key_test() ->
    Msg = #{ <<"visible">> => <<"ok">>, <<"masked">> => <<"unset">> },
    Opts = #{ <<"hashpath">> => ignore },
    ?assertEqual({error, not_found}, hb_ao:resolve(Msg, <<"masked">>, Opts)),
    ?assertEqual({ok, [<<"visible">>]}, hb_ao:resolve(Msg, keys, Opts)).

set_step_request_uses_inherited_payload_test() ->
    Base = #{ <<"dangerous">> => <<"Value1">> },
    Req = #{
        <<"path">> => <<"set">>,
        <<"...">> => #{ <<"path">> => <<"set">>, <<"dangerous">> => <<"Value2">> }
    },
    ?assertEqual(
        [<<"dangerous">>, <<"path">>],
        lists:sort(hb_util:ok(keys(Req, #{ <<"keys">> => <<"deep">> }, #{ <<"hashpath">> => ignore })))
    ),
    ?assertEqual(
        {ok, <<"Value2">>},
        hb_ao:raw(Req, #{ <<"path">> => <<"dangerous">> }, #{ <<"hashpath">> => ignore })
    ),
    ?assertMatch(
        {ok, #{ <<"dangerous">> := <<"Value2">> }},
        set(Base, Req, #{ <<"hashpath">> => ignore })
    ).

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
    ?assertEqual({ok, <<"Value1">>}, hb_ao:resolve(Req, <<"test-key1">>, Opts)),
    ?assertEqual({error, not_found}, hb_ao:resolve(Req, <<"deep/test-key2">>, Opts)),
    ?assertEqual({ok, <<"Value3">>}, hb_ao:resolve(Req, <<"deep/test-key3">>, Opts)),
    Res = hb_ao:set(Req, <<"deep/test-key3">>, unset, Opts),
    ?assertEqual({ok, <<"Value1">>}, hb_ao:resolve(Res, <<"test-key1">>, Opts)),
    ?assertEqual({error, not_found}, hb_ao:resolve(Res, <<"deep/test-key2">>, Opts)),
    ?assertEqual({error, not_found}, hb_ao:resolve(Res, <<"deep/test-key3">>, Opts)),
    Msg4 = hb_ao:set(Res, #{ <<"deep">> => unset }, Opts),
    ?assertEqual({ok, <<"Value1">>}, hb_ao:resolve(Msg4, <<"test-key1">>, Opts)),
    ?assertEqual({error, not_found}, hb_ao:resolve(Msg4, <<"deep">>, Opts)).

set_ignore_undefined_test() ->
	Base = #{ <<"test-key">> => <<"Value1">> },
	Req = #{ <<"path">> => <<"set">>, <<"test-key">> => undefined },
    Opts = #{ <<"hashpath">> => ignore },
    Res = hb_util:ok(set(Base, Req, Opts)),
	?assertEqual(Base, maps:get(<<"...">>, hb_private:reset(Res))),
	?assertEqual({ok, <<"Value1">>}, hb_ao:resolve(Res, <<"test-key">>, Opts)).

atom_priv_is_private_when_setting_test() ->
    Base = #{ priv => #{ <<"secret">> => true }, <<"x">> => 1 },
    Opts = #{ <<"hashpath">> => ignore },
    Res = hb_util:ok(set(Base, #{ <<"y">> => 2 }, Opts)),
    Public = hb_private:reset(Res),
    ?assertEqual(false, maps:is_key(priv, Public)),
    ?assertEqual(false, maps:is_key(priv, maps:get(<<"...">>, Public))),
    ?assertEqual(#{ <<"secret">> => true }, hb_private:from_message(Res)).

verify_test_() ->
	{foreach, fun () -> ok end, fun (_) -> ok end, [
		{"RSA", fun () -> test_verify(?RSA_KEY_TYPE) end},
		{"EDDSA", fun () -> test_verify(?EDDSA_KEY_TYPE) end},
        {"Solana", fun () -> test_verify(?SOLANA_KEY_TYPE) end},
        {"Ethereum", fun () -> test_verify(?ETHEREUM_KEY_TYPE) end}
	]}.

test_verify(KeyType) ->
    Unsigned = #{ <<"a">> => <<"b">> },
    Wallet = ar_wallet:new(KeyType),
    Signed = hb_message:commit(Unsigned, #{ <<"priv-wallet">> => Wallet }),
    ?event_debug({signed, Signed}),
    BadSigned = Signed#{ <<"a">> => <<"c">> },
    ?event_debug({bad_signed, BadSigned}),
    ?assertEqual(false, hb_message:verify(BadSigned)),
    ?assertEqual({ok, true},
        hb_ao:resolve(
            #{ <<"device">> => <<"message@1.0">> },
            #{ <<"path">> => <<"verify">>, <<"body">> => Signed },
            #{ <<"hashpath">> => ignore }
        )
    ),
    % Test that we can verify a message without specifying the device explicitly.
    ?assertEqual({ok, true},
        hb_ao:resolve(
            #{},
            #{ <<"path">> => <<"verify">>, <<"body">> => Signed },
            #{ <<"hashpath">> => ignore }
        )
    ).

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
    ?assertEqual(true, hb_message:match(Expected, Result, only_present, Opts)).
