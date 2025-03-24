%%% @doc The identity device: For non-reserved keys, it simply returns a key 
%%% from the message as it is found in the message's underlying Erlang map. 
%%% Private keys (`priv[.*]') are not included.
%%% Reserved keys are: `id', `attestations', `attestors', `keys', `path', 
%%% `set', `remove', `get', and `verify'. Their function comments describe the 
%%% behaviour of the device when these keys are set.
-module(dev_message).
%%% Base Converge reserved keys:
-export([info/0, keys/1]).
-export([set/3, set_path/3, remove/2, get/2, get/3]).
%%% Attestation-specific keys:
-export([id/1, id/2, id/3]).
-export([attest/3, attested/3, attestors/1, attestors/2, attestors/3, verify/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-define(DEFAULT_ID_DEVICE, <<"httpsig@1.0">>).
-define(DEFAULT_ATT_DEVICE, <<"httpsig@1.0">>).

%% The list of keys that are exported by this device.
-define(DEVICE_KEYS, [
    <<"id">>,
    <<"attestations">>,
    <<"attestors">>,
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

%% @doc Return the ID of a message, using the `attestors' list if it exists.
%% If the `attestors' key is `all', return the ID including all known 
%% attestations -- `none' yields the ID without any attestations. If the 
%% `attestors' key is a list/map, return the ID including only the specified 
%% attestations.
%% 
%% The `id-device' key in the message can be used to specify the device that
%% should be used to calculate the ID. If it is not set, the default device
%% (`httpsig@1.0') is used.
%% 
%% Note: This function _does not_ use Converge's `get/3' function, as it
%% as it would require significant computation. We may want to change this
%% if/when non-map message structures are created.
id(Base) -> id(Base, #{}).
id(Base, Req) -> id(Base, Req, #{}).
id(Base, _, NodeOpts) when not is_map(Base) ->
    % Return the hashpath of the message in native format, to match the native
    % format of the message ID return.
    {ok, hb_util:native_id(hb_path:hashpath(Base, NodeOpts))};
id(Base, Req, NodeOpts) ->
    ModBase =
        case maps:get(<<"attestors">>, Req, <<"none">>) of
            <<"all">> -> Base;
            <<"none">> -> maps:without([<<"attestations">>], Base);
            [] -> Base;
            RawAttestorIDs ->
                KeepIDs =
                    case is_map(RawAttestorIDs) of
                        true -> maps:keys(RawAttestorIDs);
                        false -> RawAttestorIDs
                    end,
                BaseAttestations = maps:get(<<"attestations">>, Base, #{}),
                % Add attestations with only the keys that are requested.
                BaseWithAttestations = 
                    Base#{
                        <<"attestations">> =>
                            maps:from_list(
                                lists:map(
                                    fun(Att) ->
                                        try {Att, maps:get(Att, BaseAttestations)}
                                        catch _:_ ->
                                            throw(
                                                {
                                                    attestor_not_found,
                                                    Att,
                                                    {attestations, BaseAttestations}
                                                }
                                            )
                                        end
                                    end,
                                    KeepIDs
                                )
                            )
                    },
                % Add the hashpath to the message if it is present.
                case Base of
                    #{ <<"priv">> := #{ <<"hashpath">> := Hashpath }} ->
                        BaseWithAttestations#{
                            <<"hashpath">> => Hashpath
                        };
                    _ ->
                        BaseWithAttestations
                end
        end,
    % Find the ID device for the message.
    IDMod =
        case id_device(ModBase) of
            {ok, IDDev} -> IDDev;
            {error, Error} -> throw({id, Error})
        end,
    ?event({using_id_device, {idmod, IDMod}, {modbase, ModBase}}),
    % Get the device module from the message, or use the default if it is not
    % set. We can tell if the device is not set (or is the default) by checking 
    % whether the device module is the same as this module.
    DevMod =
        case hb_converge:message_to_device(#{ <<"device">> => IDMod }, NodeOpts) of
            ?MODULE ->
                hb_converge:message_to_device(
                    #{ <<"device">> => ?DEFAULT_ID_DEVICE },
                    NodeOpts
                );
            Mod -> Mod
        end,
    % Apply the function's `id' function with the appropriate arguments. If it
    % doesn't exist, error.
    case hb_converge:find_exported_function(ModBase, DevMod, id, 3, NodeOpts) of
        {ok, Fun} -> apply(Fun, hb_converge:truncate_args(Fun, [ModBase, Req, NodeOpts]));
        not_found -> throw({id, id_resolver_not_found_for_device, DevMod})
    end.

%% @doc Locate the ID device of a message. The ID device is determined by the 
%% `id-device` if found in the `/attestations/id/attestation-device` key, or the
%% `device` set in _all_ of the attestations. If no attestations are present,
%% the default device (`httpsig@1.0') is used.
id_device(#{ <<"attestations">> := #{ <<"id">> := #{ <<"attestation-device">> := IDDev } } }) ->
    {ok, IDDev};
id_device(#{ <<"attestations">> := Attestations }) ->
    % Get the device from the first attestation.
    UnfilteredDevs =
        maps:map(
            fun(_, #{ <<"attestation-device">> := AttestationDev }) ->
                AttestationDev;
            (_, _) -> undefined
            end,
            Attestations
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

%% @doc Return the attestors of a message that are present in the given request.
attestors(Base) -> attestors(Base, #{}).
attestors(Base, Req) -> attestors(Base, Req, #{}).
attestors(Base, _, _NodeOpts) ->
    {ok, maps:keys(maps:get(<<"attestations">>, Base, #{}))}.

% %% @doc Return a device that resolves a given path to a specific attestation.
% attestations(Base, _Req, _NodeOpts) ->
%     {ok,
%         Base#{
%             <<"device">> =>
%                 #{
%                     info =>
%                         fun(Msg, MsgWithPath, _NodeOpts2) ->
%                             Attestor = maps:get(<<"path">>, MsgWithPath),
%                             Attestation = maps:get(Attestor, Msg),
%                             % 'Promote' the keys of the attestation to the root
%                             % of the message.
%                             {
%                                 ok,
%                                 maps:merge(
%                                     maps:without([<<"attestations">>], Msg),
%                                     Attestation
%                                 )
%                             }
%                         end
%                 }
%         }
%     }.

%% @doc Attest to a message, using the `attestation-device' key to specify the
%% device that should be used to attest to the message. If the key is not set,
%% the default device (`httpsig@1.0') is used.
attest(Self, Req, Opts) ->
    {ok, Base} = hb_message:find_target(Self, Req, Opts),
    % Encode to a TABM.
    AttDev =
        case maps:get(<<"attestation-device">>, Req, not_specified) of
            not_specified ->
                hb_opts:get(attestation_device, no_viable_attestation_device, Opts);
            Dev -> Dev
        end,
    % We _do not_ set the `device` key in the message, as the device will be
    % part of the attestation. Instead, we find the device module's `attest`
    % function and apply it.
    AttMod = hb_converge:message_to_device(#{ <<"device">> => AttDev }, Opts),
    {ok, AttFun} = hb_converge:find_exported_function(Base, AttMod, attest, 3, Opts),
    Encoded = hb_message:convert(Base, tabm, Opts),
    {ok, Attested} = apply(AttFun, hb_converge:truncate_args(AttFun, [Encoded, Req, Opts])),
    {ok, hb_message:convert(Attested, <<"structured@1.0">>, Opts)}.

%% @doc Verify a message nested in the body. As with `id', the `attestors'
%% key in the request can be used to specify which attestations should be
%% verified. 
verify(Self, Req, Opts) ->
    % Get the target message of the verification request.
    {ok, Base} = hb_message:find_target(Self, Req, Opts),
    % Get the attestations to verify.
    Attestations =
        case maps:get(<<"attestors">>, Req, <<"all">>) of
            <<"none">> -> [];
            <<"all">> -> maps:get(<<"attestations">>, Base, #{});
            AttestorIDs ->
                maps:with(
                    AttestorIDs,
                    maps:get(<<"attestations">>, Base, #{})
                )
        end,
    % Remove the attestations from the base message.
    ?event({verifying_attestations, Attestations}),
    % Verify the attestations. Stop execution if any fail.
    Res =
        lists:all(
            fun(Attestor) ->
                ?event(
                    {verify_attestation,
                        {attestor, Attestor},
                        {target, Base}}
                ),
                {ok, Res} = exec_for_attestation(
                    verify,
                    Base,
                    maps:get(Attestor, Attestations),
                    Req#{ <<"attestor">> => Attestor },
                    Opts
                ),
                ?event({verify_attestation_res, {attestor, Attestor}, {res, Res}}),
                Res
            end,
            maps:keys(Attestations)
        ),
    ?event({verify_res, Res}),
    {ok, Res}.

%% @doc Execute a function for a single attestation in the context of its
%% parent message.
%% Note: Assumes that the `attestations' key has already been removed from the
%% message if applicable.
exec_for_attestation(Func, Base, Attestation, Req, Opts) ->
    ?event({executing_for_attestation, {func, Func}, {base, Base}, {attestation, Attestation}, {req, Req}}),
    AttestionMessage =
        maps:merge(Base, maps:without([<<"attestation-device">>], Attestation)),
    AttDev =
        maps:get(
            <<"attestation-device">>,
            Attestation,
            ?DEFAULT_ATT_DEVICE
        ),
    AttMod =
        hb_converge:message_to_device(
            #{ <<"device">> => AttDev },
            Opts
        ),
    {ok, AttFun} =
        hb_converge:find_exported_function(
            AttestionMessage,
            AttMod,
            Func,
            3,
            Opts
        ),
    Encoded = hb_message:convert(AttestionMessage, tabm, Opts),
    apply(AttFun, [Encoded, Req, Opts]).

%% @doc Return the list of attested keys from a message.
%% @doc Set keys in a message. Takes a map of key-value pairs and sets them in
%% the message, overwriting any existing values.
attested(Self, Req, Opts) ->
    % Get the target message of the verification request.
    {ok, Base} = hb_message:find_target(Self, Req, Opts),
    Attestations = maps:get(<<"attestations">>, Base, #{}),
    Attestors =
        case maps:get(<<"attestors">>, Req, <<"all">>) of
            <<"none">> -> [];
            <<"all">> -> maps:keys(Attestations);
            AttestorIDs -> AttestorIDs
        end,
    % Get the list of attested keys from each attestor.
    AttestationKeys =
        lists:map(
            fun(Attestor) ->
                Attestion = maps:get(Attestor, Attestations),
                {ok, AttestedKeys} =
                    exec_for_attestation(
                        attested,
                        Base,
                        Attestion,
                        #{ <<"attestor">> => Attestor },
                        Opts
                    ),
                AttestedKeys
            end,
            Attestors
        ),
    % Remove attestations that are not in *every* attestor's list.
    % To start, we need to create the super-set of attested keys.
    AllAttestedKeys =
        lists:foldl(
            fun(Key, Acc) ->
                case lists:member(Key, Acc) of
                    true -> Acc;
                    false -> [Key | Acc]
                end
            end,
            [],
            lists:flatten(AttestationKeys)
        ),
    % Next, we filter the list of all attested keys to only include those that
    % are present in every attestor's list.
    OnlyAttestedKeys =
        lists:filter(
            fun(Key) ->
                lists:all(
                    fun(AttestedKeys) -> lists:member(Key, AttestedKeys) end,
                    AttestationKeys
                )
            end,
            AllAttestedKeys
        ),
    ?event({only_attested_keys, OnlyAttestedKeys}),
    {ok, OnlyAttestedKeys}.

set(Message1, NewValuesMsg, _Opts) ->
	% Filter keys that are in the default device (this one).
    {ok, NewValuesKeys} = keys(NewValuesMsg),
	KeysToSet =
		lists:filter(
			fun(Key) ->
				not lists:member(Key, ?DEVICE_KEYS) andalso
					(maps:get(Key, NewValuesMsg, undefined) =/= undefined)
			end,
			NewValuesKeys
		),
	% Find keys in the message that are already set (case-insensitive), and 
	% note them for removal.
	ConflictingKeys =
		lists:filter(
			fun(Key) ->
				lists:member(Key, KeysToSet)
			end,
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
    % Calculate if we need to remove the attestations from the message.
    WithoutAtts =
        case UnsetKeys ++ ConflictingKeys of
            [] -> [];
            _ -> [<<"attestations">>]
        end,
	{
		ok,
		maps:merge(
			maps:without(ConflictingKeys ++ UnsetKeys ++ WithoutAtts, Message1),
			maps:from_list(
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
			)
		)
	}.

%% @doc Special case of `set/3' for setting the `path' key. This cannot be set
%% using the normal `set' function, as the `path' is a reserved key, necessary 
%% for Converge to know the key to evaluate in requests.
set_path(Message1, #{ <<"value">> := Value }, _Opts) ->
    {ok, Message1#{ <<"path">> => Value }}.

%% @doc Remove a key or keys from a message.
remove(Message1, #{ <<"item">> := Key }) ->
    remove(Message1, #{ <<"items">> => [Key] });
remove(Message1, #{ <<"items">> := Keys }) ->
    { ok, maps:without(Keys, Message1) }.

%% @doc Get the public keys of a message.
keys(Msg) when not is_map(Msg) ->
    case hb_converge:normalize_keys(Msg) of
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
    NormKey = hb_converge:normalize_key(Key),
    NormMsg = hb_converge:normalize_keys(Msg),
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
    ?assertEqual({ok, [<<"a">>]}, hb_converge:resolve(#{ <<"a">> => 1 }, keys, #{})).

case_insensitive_get_test() ->
	?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{ <<"a">> => 1 })),
	?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{ <<"A">> => 1 })),
	?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{ <<"a">> => 1 })),
	?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{ <<"A">> => 1 })).

private_keys_are_filtered_test() ->
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_converge:resolve(#{ <<"a">> => 1, <<"private">> => 2 }, keys, #{})
    ),
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_converge:resolve(#{ <<"a">> => 1, <<"priv_foo">> => 4 }, keys, #{})
    ).

cannot_get_private_keys_test() ->
    ?assertEqual(
        {error, not_found},
        hb_converge:resolve(
            #{ <<"a">> => 1, <<"private_key">> => 2 },
            <<"private_key">>,
            #{ hashpath => ignore }
        )
    ).

key_from_device_test() ->
    ?assertEqual({ok, 1}, hb_converge:resolve(#{ <<"a">> => 1 }, <<"a">>, #{})).

remove_test() ->
	Msg = #{ <<"key1">> => <<"Value1">>, <<"key2">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"key2">> := <<"Value2">> }},
		hb_converge:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"item">> => <<"key1">> },
            #{ hashpath => ignore }
        )
    ),
	?assertMatch({ok, #{}},
		hb_converge:resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"items">> => [<<"key1">>, <<"key2">>] },
            #{ hashpath => ignore }
        )
    ).

set_conflicting_keys_test() ->
	Msg1 = #{ <<"dangerous">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"dangerous">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"dangerous">> := <<"Value2">> }},
		hb_converge:resolve(Msg1, Msg2, #{})).

unset_with_set_test() ->
	Msg1 = #{ <<"dangerous">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"dangerous">> => unset },
	?assertMatch({ok, Msg3} when map_size(Msg3) == 0,
		hb_converge:resolve(Msg1, Msg2, #{ hashpath => ignore })).

set_ignore_undefined_test() ->
	Msg1 = #{ <<"test-key">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"test-key">> => undefined },
	?assertEqual({ok, #{ <<"test-key">> => <<"Value1">> }},
		set(Msg1, Msg2, #{ hashpath => ignore })).

verify_test() ->
    Unsigned = #{ <<"a">> => <<"b">> },
    Signed = hb_message:attest(Unsigned, hb:wallet()),
    ?event({signed, Signed}),
    BadSigned = Signed#{ <<"a">> => <<"c">> },
    ?event({bad_signed, BadSigned}),
    ?assertEqual(false, hb_message:verify(BadSigned)),
    ?assertEqual({ok, true},
        hb_converge:resolve(
            #{ <<"device">> => <<"message@1.0">> },
            #{ <<"path">> => <<"verify">>, <<"body">> => Signed },
            #{ hashpath => ignore }
        )
    ),
    % Test that we can verify a message without specifying the device explicitly.
    ?assertEqual({ok, true},
        hb_converge:resolve(
            #{},
            #{ <<"path">> => <<"verify">>, <<"body">> => Signed },
            #{ hashpath => ignore }
        )
    ).

run_test() ->
    hb_message:deep_multisignature_test().
