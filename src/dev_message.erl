%%% @doc The identity device: For non-reserved keys, it simply returns a key 
%%% from the message as it is found in the message's underlying Erlang map. 
%%% Private keys (`priv[.*]') are not included.
%%% Reserved keys are: `id', `commitments', `committers', `keys', `path', 
%%% `set', `remove', `get', and `verify'. Their function comments describe the 
%%% behaviour of the device when these keys are set.
-module(dev_message).
%%% Base AO-Core reserved keys:
-export([info/0, keys/1]).
-export([set/3, set_path/3, remove/2, get/2, get/3]).
%%% Commitment-specific keys:
-export([id/1, id/2, id/3]).
-export([commit/3, committed/3, committers/1, committers/2, committers/3, verify/3]).
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

%% @doc Return the info for the identity device.
info() ->
    #{
        default => fun get/3
        %exports => ?DEVICE_KEYS
    }.

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
%% as it would require significant computation. We may want to change this
%% if/when non-map message structures are created.
id(Base) -> id(Base, #{}).
id(Base, Req) -> id(Base, Req, #{}).
id(Base, _, NodeOpts) when not is_map(Base) ->
    % Return the hashpath of the message in native format, to match the native
    % format of the message ID return.
    {ok, hb_util:human_id(hb_path:hashpath(Base, NodeOpts))};
id(Base, Req, NodeOpts) ->
    % Remove the commitments from the base message if there are none, after
    % filtering for the committers specified in the request.
    ModBase = #{ <<"commitments">> := Commitments }
        = with_relevant_commitments(Base, Req, NodeOpts),
    case maps:keys(Commitments) of
        [] ->
            % If there are no commitments, we must (re)calculate the ID.
            ?event(ids, no_commitments_found_in_id_call),
            calculate_ids(maps:without([<<"commitments">>], ModBase), Req, NodeOpts);
        [ID] ->
            % If there is only one commitment, return the ID of the message.
            ?event(ids, using_precalculated_id),
            {ok, ID};
        IDs ->
            % If there are multiple commitments, sort them, concatenate them as
            % a structured field string, and return the hash of the result.
            ?event(ids, multiple_commitments_found_in_id_call),
            SortedIDs = lists:sort(IDs),
            IDsLine = iolist_to_binary(lists:join(<<", ">>, SortedIDs)),
            {ok, hb_util:human_id(hb_crypto:sha256(IDsLine))}
    end.

calculate_ids(Base, Req, NodeOpts) ->
    % Find the ID device for the message.
    % Find the ID device for the message.
    IDMod =
        case id_device(Base) of
            {ok, IDDev} -> IDDev;
            {error, Error} -> throw({id, Error})
        end,
    ?event({using_id_device, {idmod, IDMod}, {modbase, Base}}),
    % Get the device module from the message, or use the default if it is not
    % set. We can tell if the device is not set (or is the default) by checking 
    % whether the device module is the same as this module.
    DevMod =
        case hb_ao:message_to_device(#{ <<"device">> => IDMod }, NodeOpts) of
            ?MODULE ->
                hb_ao:message_to_device(
                    #{ <<"device">> => ?DEFAULT_ID_DEVICE },
                    NodeOpts
                );
            Module -> Module
        end,
    % Apply the function's `id' function with the appropriate arguments. If it
    % doesn't exist, error.
    case hb_ao:find_exported_function(Base, DevMod, id, 3, NodeOpts) of
        {ok, Fun} ->
            ?event(id, {called_id_device, IDMod}, NodeOpts),
            apply(Fun, hb_ao:truncate_args(Fun, [Base, Req, NodeOpts]));
        not_found -> throw({id, id_resolver_not_found_for_device, DevMod})
    end.

%% @doc Locate the ID device of a message. The ID device is determined the
%% `device' set in _all_ of the commitments. If no commitments are present,
%% the default device (`httpsig@1.0') is used.
id_device(#{ <<"commitments">> := Commitments }) ->
    % Get the device from the first commitment.
    UnfilteredDevs =
        maps:map(
            fun(_, #{ <<"commitment-device">> := CommitmentDev }) ->
                CommitmentDev;
            (_, _) -> undefined
            end,
            Commitments
        ),
    % Filter out the undefined devices.
    Devs = lists:filter(fun(Dev) -> Dev =/= undefined end, maps:values(UnfilteredDevs)),
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
id_device(_) ->
    {ok, ?DEFAULT_ID_DEVICE}.

%% @doc Return the committers of a message that are present in the given request.
committers(Base) -> committers(Base, #{}).
committers(Base, Req) -> committers(Base, Req, #{}).
committers(#{ <<"commitments">> := Commitments }, _, _NodeOpts) ->
    ?event({commitments, Commitments}),
    {ok,
        maps:values(
            maps:filtermap(
                fun(_ID, Commitment) ->
                    case maps:get(<<"committer">>, Commitment, undefined) of
                        undefined -> false;
                        Committer -> {true, Committer}
                    end
                end,
                Commitments
            )
        )
    };
committers(_, _, _) ->
    {ok, []}.

%% @doc Commit to a message, using the `commitment-device' key to specify the
%% device that should be used to commit to the message. If the key is not set,
%% the default device (`httpsig@1.0') is used.
commit(Self, Req, Opts) ->
    {ok, Base} = hb_message:find_target(Self, Req, Opts),
    % Encode to a TABM.
    AttDev =
        case maps:get(<<"commitment-device">>, Req, not_specified) of
            not_specified ->
                hb_opts:get(commitment_device, no_viable_commitment_device, Opts);
            Dev -> Dev
        end,
    % We _do not_ set the `device' key in the message, as the device will be
    % part of the commitment. Instead, we find the device module's `commit'
    % function and apply it.
    AttMod = hb_ao:message_to_device(#{ <<"device">> => AttDev }, Opts),
    {ok, AttFun} = hb_ao:find_exported_function(Base, AttMod, commit, 3, Opts),
    Encoded = hb_message:convert(Base, tabm, Opts),
    {ok, Committed} = apply(AttFun, hb_ao:truncate_args(AttFun, [Encoded, Req, Opts])),
    {ok, hb_message:convert(Committed, <<"structured@1.0">>, Opts)}.

%% @doc Verify a message. By default, all commitments are verified. The
%% `committers' key in the request can be used to specify that only the 
%% commitments from specific committers should be verified. Similarly, specific
%% commitments can be specified using the `commitments' key.
verify(Self, Req, Opts) ->
    % Get the target message of the verification request.
    {ok, Base} = hb_message:find_target(Self, Req, Opts),
    Commitments = maps:get(<<"commitments">>, Base, #{}),
    IDsToVerify = commitment_ids_from_request(Base, Req, Opts),
    % Remove the commitments from the base message.
    ?event({verifying_commitments, Commitments}),
    % Verify the commitments. Stop execution if any fail.
    Res =
        lists:all(
            fun(CommitmentID) ->
                ?event(
                    {verify_commitment,
                        {commitment_id, CommitmentID},
                        {target, Base}}
                ),
                {ok, Res} = exec_for_commitment(
                    verify,
                    Base,
                    maps:get(CommitmentID, Commitments),
                    Req#{ <<"commitment">> => CommitmentID },
                    Opts
                ),
                ?event({verify_commitment_res, {commitment_id, CommitmentID}, {res, Res}}),
                Res
            end,
            IDsToVerify
        ),
    ?event({verify_res, Res}),
    {ok, Res}.

%% @doc Execute a function for a single commitment in the context of its
%% parent message.
%% Note: Assumes that the `commitments' key has already been removed from the
%% message if applicable.
exec_for_commitment(Func, Base, Commitment, Req, Opts) ->
    ?event({executing_for_commitment, {func, Func}, {base, Base}, {commitment, Commitment}, {req, Req}}),
    CommitmentMessage =
        maps:merge(Base, maps:without([<<"commitment-device">>], Commitment)),
    AttDev =
        maps:get(
            <<"commitment-device">>,
            Commitment,
            ?DEFAULT_ATT_DEVICE
        ),
    AttMod =
        hb_ao:message_to_device(
            #{ <<"device">> => AttDev },
            Opts
        ),
    {ok, AttFun} =
        hb_ao:find_exported_function(
            CommitmentMessage,
            AttMod,
            Func,
            3,
            Opts
        ),
    Encoded = hb_message:convert(CommitmentMessage, tabm, Opts),
    apply(AttFun, [Encoded, Req, Opts]).

%% @doc Return the list of committed keys from a message.
committed(Self, Req, Opts) ->
    % Get the target message of the verification request.
    {ok, Base} = hb_message:find_target(Self, Req, Opts),
    CommitmentIDs = commitment_ids_from_request(Base, Req, Opts),
    Commitments = maps:get(<<"commitments">>, Base, #{}),
    % Get the list of committed keys from each committer.
    CommitmentKeys =
        lists:map(
            fun(CommitmentID) ->
                Commitment = maps:get(CommitmentID, Commitments),
                {ok, CommittedKeys} =
                    exec_for_commitment(
                        committed,
                        Base,
                        Commitment,
                        #{ <<"commitment">> => CommitmentID },
                        Opts
                    ),
                ?event({committed_keys, {commitment_id, CommitmentID}, {keys, CommittedKeys}}),
                CommittedKeys
            end,
            CommitmentIDs
        ),
    % Remove commitments that are not in *every* committer's list.
    % To start, we need to create the super-set of committed keys.
    AllCommittedKeys =
        lists:foldl(
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
    ?event({only_committed_keys, OnlyCommittedKeys}),
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
%% may specify `all' or `none' for each group. If no specifiers are provided,
%% the default is `all' for commitments -- also implying `all' for committers.
commitment_ids_from_request(Base, Req, Opts) ->
    Commitments = maps:get(<<"commitments">>, Base, #{}),
    ReqCommitters =
        case maps:get(<<"committers">>, Req, <<"none">>) of
            X when is_list(X) -> X;
            Descriptor -> hb_ao:normalize_key(Descriptor)
        end,
    RawReqCommitments =
        maps:get(
            <<"commitments">>,
            Req,
            case ReqCommitters of
                <<"none">> -> <<"all">>;
                _ -> <<"none">>
            end
        ),
    ReqCommitments =
        case RawReqCommitments of
            X2 when is_list(X2) -> X2;
            Descriptor2 -> hb_ao:normalize_key(Descriptor2)
        end,
    ?event({commitment_ids_from_request, {req_commitments, ReqCommitments}, {req_committers, ReqCommitters}}),
    % Get the commitments to verify.
    FromCommitmentIDs =
        case ReqCommitments of
            <<"none">> -> [];
            <<"all">> -> maps:keys(Commitments);
            CommitmentIDs ->
                CommitmentIDs =
                    if is_list(CommitmentIDs) -> CommitmentIDs;
                    true -> [CommitmentIDs]
                    end,
                lists:map(
                    fun(CommitmentID) -> maps:get(CommitmentID, Commitments) end,
                    CommitmentIDs
                )
        end,
    FromCommitterAddrs =
        case ReqCommitters of
            <<"none">> ->
                ?event(no_commitment_ids_for_committers),
                [];
            <<"all">> ->
                ?event(getting_commitment_ids_for_all_committers),
                {ok, Committers} = committers(Base, Req, Opts),
                ?event({commitment_ids_from_committers, Committers}),
                commitment_ids_from_committers(Committers, Commitments);
            RawCommitterAddrs ->
                ?event({getting_commitment_ids_for_specific_committers, RawCommitterAddrs}),
                CommitterAddrs =
                    if is_list(RawCommitterAddrs) -> RawCommitterAddrs;
                    true -> [RawCommitterAddrs]
                    end,
                commitment_ids_from_committers(CommitterAddrs, Commitments)
        end,
    Res = FromCommitterAddrs ++ FromCommitmentIDs,
    ?event({commitment_ids_from_request, {base, Base}, {req, Req}, {res, Res}}),
    Res.

%% @doc Returns a list of commitment IDs in a commitments map that are relevant
%% for a list of given committer addresses.
commitment_ids_from_committers(CommitterAddrs, Commitments) ->
    % Get the IDs of all commitments for each committer.
    Comms =
        lists:map(
            fun(CommitterAddr) ->
                % For each committer, filter the commitments to only
                % include those with the matching committer address.
                IDs = 
                    maps:values(maps:filtermap(
                        fun(ID, Msg) ->
                            % If the committer address matches, return
                            % the ID. If not, ignore the commitment.
                            case maps:get(<<"committer">>, Msg, undefined) of
                                CommitterAddr -> {true, ID};
                                _ -> false
                            end
                        end,
                        Commitments
                    )),
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
set(Message1, NewValuesMsg, Opts) ->
    OriginalPriv = hb_private:from_message(Message1),
	% Filter keys that are in the default device (this one).
    {ok, NewValuesKeys} = keys(NewValuesMsg),
	KeysToSet =
		lists:filter(
			fun(Key) ->
				not lists:member(Key, ?DEVICE_KEYS ++ [<<"set-mode">>]) andalso
					(maps:get(Key, NewValuesMsg, undefined) =/= undefined)
			end,
			NewValuesKeys
		),
	% Find keys in the message that are already set (case-insensitive), and 
	% note them for removal.
	ConflictingKeys =
		lists:filter(
			fun(Key) -> lists:member(Key, KeysToSet) end,
			maps:keys(Message1)
		),
    UnsetKeys =
        lists:filter(
            fun(Key) ->
                case maps:get(Key, NewValuesMsg, not_found) of
                    unset -> true;
                    _ -> false
                end
            end,
            maps:keys(Message1)
        ),
    % Base message with keys-to-unset removed
    BaseValues = maps:without(UnsetKeys, Message1),
    ?event(
        {performing_set,
            {conflicting_keys, ConflictingKeys},
            {keys_to_unset, UnsetKeys},
            {new_values, NewValuesMsg},
            {original_message, Message1}
        }
    ),
    % Create the map of new values
    NewValues = maps:from_list(
        lists:filtermap(
            fun(Key) ->
                case maps:get(Key, NewValuesMsg, undefined) of
                    undefined -> false;
                    unset -> false;
                    Value -> {true, {Key, Value}}
                end
            end,
            KeysToSet
        )
    ),
    % Caclulate if the keys to be set conflict with any committed keys.
    {ok, CommittedKeys} =
        committed(
            Message1,
            #{
                <<"committers">> => <<"all">>
            },
            Opts
        ),
    ?event(
        {setting,
            {committed_keys, CommittedKeys},
            {keys_to_set, KeysToSet},
            {message, Message1}
        }),
    OverwrittenCommittedKeys =
        lists:filtermap(
            fun(Key) ->
                NormKey = hb_ao:normalize_key(Key),
                ?event({checking_committed_key, {key, Key}, {norm_key, NormKey}}),
                Res = case lists:member(NormKey, KeysToSet) of
                    true -> {true, NormKey};
                    false -> false
                end,
                Res
            end,
            CommittedKeys
        ),
    ?event({setting, {overwritten_committed_keys, OverwrittenCommittedKeys}}),
    % Combine with deep merge or if `set-mode` is `explicit' then just merge.
    Merged =
        hb_private:set_priv(
            case maps:get(<<"set-mode">>, NewValuesMsg, <<"deep">>) of
                <<"explicit">> -> maps:merge(BaseValues, NewValues);
                _ -> hb_util:deep_merge(BaseValues, NewValues)
            end,
            OriginalPriv
        ),
    case OverwrittenCommittedKeys of
        [] -> {ok, Merged};
        _ ->
            % We did overwrite some keys, but do their values match the original?
            % If not, we must remove the commitments.
            case hb_message:match(Merged, Message1) of
                true -> {ok, Merged};
                false -> {ok, maps:without([<<"commitments">>], Merged)}
            end
    end.

%% @doc Special case of `set/3' for setting the `path' key. This cannot be set
%% using the normal `set' function, as the `path' is a reserved key, necessary 
%% for AO-Core to know the key to evaluate in requests.
set_path(Message1, #{ <<"value">> := Value }, _Opts) ->
    {ok, Message1#{ <<"path">> => Value }}.

%% @doc Remove a key or keys from a message.
remove(Message1, #{ <<"item">> := Key }) ->
    remove(Message1, #{ <<"items">> => [Key] });
remove(Message1, #{ <<"items">> := Keys }) ->
    { ok, maps:without(Keys, Message1) }.

%% @doc Get the public keys of a message.
keys(Msg) when not is_map(Msg) ->
    case hb_ao:normalize_keys(Msg) of
        NormMsg when is_map(NormMsg) -> keys(NormMsg);
        _ -> throw(badarg)
    end;
keys(Msg) ->
    {
        ok,
        lists:filter(
            fun(Key) -> not hb_private:is_private(Key) end,
            maps:keys(Msg)
        )
    }.

%% @doc Return the value associated with the key as it exists in the message's
%% underlying Erlang map. First check the public keys, then check case-
%% insensitively if the key is a binary.
get(Key, Msg) -> get(Key, Msg, #{ <<"path">> => <<"get">> }).
get(Key, Msg, _Msg2) ->
    case hb_private:is_private(Key) of
        true -> {error, not_found};
        false ->
            case maps:get(Key, Msg, not_found) of
                not_found -> case_insensitive_get(Key, Msg);
                Value -> {ok, Value}
            end
    end.

%% @doc Key matching should be case insensitive, following RFC-9110, so we 
%% implement a case-insensitive key lookup rather than delegating to
%% `maps:get/2'. Encode the key to a binary if it is not already.
case_insensitive_get(Key, Msg) ->
    NormKey = hb_ao:normalize_key(Key),
    NormMsg = hb_ao:normalize_keys(Msg),
    case maps:get(NormKey, NormMsg, not_found) of
        not_found -> {error, not_found};
        Value -> {ok, Value}
    end.

%%% Tests

%%% Internal module functionality tests:
get_keys_mod_test() ->
    ?assertEqual([a], maps:keys(#{a => 1})).

is_private_mod_test() ->
    ?assertEqual(true, hb_private:is_private(<<"private">>)),
    ?assertEqual(true, hb_private:is_private(<<"private.foo">>)),
    ?assertEqual(false, hb_private:is_private(<<"a">>)).

%%% Device functionality tests:

keys_from_device_test() ->
    ?assertEqual({ok, [<<"a">>]}, hb_ao:resolve(#{ <<"a">> => 1 }, keys, #{})).

case_insensitive_get_test() ->
	?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{ <<"a">> => 1 })),
	?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{ <<"A">> => 1 })),
	?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{ <<"a">> => 1 })),
	?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{ <<"A">> => 1 })).

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
            #{ hashpath => ignore }
        )
    ).

key_from_device_test() ->
    ?assertEqual({ok, 1}, hb_ao:resolve(#{ <<"a">> => 1 }, <<"a">>, #{})).

remove_test() ->
	Msg = #{ <<"key1">> => <<"Value1">>, <<"key2">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"key2">> := <<"Value2">> }},
		hb_ao:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"item">> => <<"key1">> },
            #{ hashpath => ignore }
        )
    ),
	?assertMatch({ok, #{}},
		hb_ao:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"items">> => [<<"key1">>, <<"key2">>] },
            #{ hashpath => ignore }
        )
    ).

set_conflicting_keys_test() ->
	Msg1 = #{ <<"dangerous">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"dangerous">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"dangerous">> := <<"Value2">> }},
		hb_ao:resolve(Msg1, Msg2, #{})).

unset_with_set_test() ->
	Msg1 = #{ <<"dangerous">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"dangerous">> => unset },
	?assertMatch({ok, Msg3} when ?IS_EMPTY_MESSAGE(Msg3),
		hb_ao:resolve(Msg1, Msg2, #{ hashpath => ignore })).

deep_unset_test() ->
    Opts = #{ hashpath => ignore },
    Msg1 = #{
        <<"test-key1">> => <<"Value1">>,
        <<"deep">> => #{
            <<"test-key2">> => <<"Value2">>,
            <<"test-key3">> => <<"Value3">>
        }
    },
    Msg2 = hb_ao:set(Msg1, #{ <<"deep/test-key2">> => unset }, Opts),
    ?assertEqual(#{
            <<"test-key1">> => <<"Value1">>,
            <<"deep">> => #{ <<"test-key3">> => <<"Value3">> }
        },
        Msg2
    ),
    Msg3 = hb_ao:set(Msg2, <<"deep/test-key3">>, unset, Opts),
    ?assertEqual(#{
            <<"test-key1">> => <<"Value1">>,
            <<"deep">> => #{}
        },
        Msg3
    ),
    Msg4 = hb_ao:set(Msg3, #{ <<"deep">> => unset }, Opts),
    ?assertEqual(#{ <<"test-key1">> => <<"Value1">> }, Msg4).

set_ignore_undefined_test() ->
	Msg1 = #{ <<"test-key">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"test-key">> => undefined },
	?assertEqual(#{ <<"test-key">> => <<"Value1">> },
		hb_private:reset(hb_util:ok(set(Msg1, Msg2, #{ hashpath => ignore })))).

verify_test() ->
    Unsigned = #{ <<"a">> => <<"b">> },
    Signed = hb_message:commit(Unsigned, hb:wallet()),
    ?event({signed, Signed}),
    BadSigned = Signed#{ <<"a">> => <<"c">> },
    ?event({bad_signed, BadSigned}),
    ?assertEqual(false, hb_message:verify(BadSigned)),
    ?assertEqual({ok, true},
        hb_ao:resolve(
            #{ <<"device">> => <<"message@1.0">> },
            #{ <<"path">> => <<"verify">>, <<"body">> => Signed },
            #{ hashpath => ignore }
        )
    ),
    % Test that we can verify a message without specifying the device explicitly.
    ?assertEqual({ok, true},
        hb_ao:resolve(
            #{},
            #{ <<"path">> => <<"verify">>, <<"body">> => Signed },
            #{ hashpath => ignore }
        )
    ).

run_test() ->
    hb_message:deep_multisignature_test().
