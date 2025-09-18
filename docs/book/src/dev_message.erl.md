# dev_message

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_message.erl)

The identity device: For non-reserved keys, it simply returns a key 
from the message as it is found in the message's underlying Erlang map. 
Private keys (`priv[.*]`) are not included.
Reserved keys are: `id`, `commitments`, `committers`, `keys`, `path`, 
`set`, `remove`, `get`, and `verify`. Their function comments describe the 
behaviour of the device when these keys are set.

---

## Exported Functions

- `commit/3`
- `committed/3`
- `committers/1`
- `committers/2`
- `committers/3`
- `get/3`
- `get/4`
- `id/1`
- `id/2`
- `id/3`
- `index/3`
- `info/0`
- `keys/1`
- `keys/2`
- `remove/2`
- `remove/3`
- `set_path/3`
- `set/3`
- `verify/3`

---

### info

The identity device: For non-reserved keys, it simply returns a key 
Return the info for the identity device.

```erlang
info() ->
    #{
        default => fun dev_message:get/4
    }.
```

### index

Generate an index page for a message, in the event that the `body` and

```erlang
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
```

### id

Return the ID of a message, using the `committers` list if it exists.

```erlang
id(Base) -> id(Base, #{}).
```

### id

Return the ID of a message, using the `committers` list if it exists.

```erlang
id(Base, Req) -> id(Base, Req, #{}).
```

### id

Return the ID of a message, using the `committers` list if it exists.

```erlang
id(Base, _, NodeOpts) when is_binary(Base) ->
    % Return the hashpath of the message in native format, to match the native
    % format of the message ID return.
```

### id

```erlang
id(RawBase, Req, NodeOpts) ->
    % Ensure that the base message is a normalized before proceeding.
```

### calculate_id

```erlang
calculate_id(Base, Req, NodeOpts) ->
    % Find the ID device for the message.
```

### id_device

Locate the ID device of a message. The ID device is determined the

```erlang
id_device(#{ <<"commitments">> := Commitments }, Opts) ->
    % Get the device from the first commitment.
```

### id_device

```erlang
id_device(_, _) ->
    {ok, ?DEFAULT_ID_DEVICE}.
```

### committers

Return the committers of a message that are present in the given request.

```erlang
committers(Base) -> committers(Base, #{}).
```

### committers

Return the committers of a message that are present in the given request.

```erlang
committers(Base, Req) -> committers(Base, Req, #{}).
```

### committers

Return the committers of a message that are present in the given request.

```erlang
committers(#{ <<"commitments">> := Commitments }, _, NodeOpts) ->
    ?event(debug_commitments, {calculating_committers, {commitments, Commitments}}),
    {ok,
        hb_maps:values(
            hb_maps:filtermap(
                fun(_ID, Commitment) ->
                    Committer = maps:get(<<"committer">>, Commitment, undefined),
                    ?event(debug_commitments, {committers, {committer, Committer}}),
                    case Committer of
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
```

### committers

Return the committers of a message that are present in the given request.

```erlang
committers(_, _, _) ->
    {ok, []}.
```

### commit

Commit to a message, using the `commitment-device` key to specify the

```erlang
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
```

### verify

Verify a message. By default, all commitments are verified. The

```erlang
verify(Self, Req, Opts) ->
    % Get the target message of the verification request.
```

### verify_commitment

Execute a function for a single commitment in the context of its

```erlang
verify_commitment(Base, Commitment, Opts) ->
    ?event(verify, {verifying_commitment, {commitment, Commitment}, {msg, Base}}),
    AttDev =
        hb_maps:get(
            <<"commitment-device">>,
            Commitment,
            ?DEFAULT_ATT_DEVICE,
            Opts
        ),
    AttMod =
        hb_ao:message_to_device(
            #{ <<"device">> => AttDev },
            Opts
        ),
    {ok, AttFun} =
        hb_ao:find_exported_function(
            Base,
            AttMod,
            verify,
            3,
            Opts
        ),
    apply(AttFun, [Base, Commitment, Opts]).
```

### committed

Return the list of committed keys from a message.

```erlang
committed(Self, Req, Opts) ->
    % Get the target message of the verification request and ensure its 
    % commitments are loaded.
```

### with_relevant_commitments

Return a message with only the relevant commitments for a given request.
Implements a standardized form of specifying commitment IDs for a

```erlang
with_relevant_commitments(Base, Req, Opts) ->
    Commitments = maps:get(<<"commitments">>, Base, #{}),
    CommitmentIDs = commitment_ids_from_request(Base, Req, Opts),
    Base#{ <<"commitments">> => maps:with(CommitmentIDs, Commitments) }.
```

### commitment_ids_from_request

Return a message with only the relevant commitments for a given request.
Implements a standardized form of specifying commitment IDs for a

```erlang
commitment_ids_from_request(Base, Req, Opts) ->
    Commitments = maps:get(<<"commitments">>, Base, #{}),
    ReqCommitters =
        case maps:get(<<"committers">>, Req, <<"none">>) of
            X when is_list(X) -> X;
            CommitterDescriptor -> hb_ao:normalize_key(CommitterDescriptor)
        end,
    RawReqCommitments = maps:get(<<"commitments">>, Req, <<"none">>),
    ReqCommitments =
        case RawReqCommitments of
            X2 when is_list(X2) -> X2;
            CommitmentDescriptor -> hb_ao:normalize_key(CommitmentDescriptor)
        end,
    ?event(debug_commitments,
        {commitment_ids_from_request,
            {req_commitments, ReqCommitments},
            {req_committers, ReqCommitters}}
    ),
    % Get the commitments to verify.
```

### ensure_commitments_loaded

Ensure that the `commitments` submessage of a base message is fully

```erlang
ensure_commitments_loaded(NonRelevant, _Opts) when not is_map(NonRelevant) ->
    NonRelevant;
```

### ensure_commitments_loaded

Ensure that the `commitments` submessage of a base message is fully

```erlang
ensure_commitments_loaded(M = #{ <<"commitments">> := Link}, Opts) when ?IS_LINK(Link) ->
    M#{
        <<"commitments">> => hb_cache:ensure_all_loaded(Link, Opts)
    };
```

### ensure_commitments_loaded

Ensure that the `commitments` submessage of a base message is fully

```erlang
ensure_commitments_loaded(M, _Opts) ->
    M.
```

### commitment_ids_from_committers

Returns a list of commitment IDs in a commitments map that are relevant

```erlang
commitment_ids_from_committers(CommitterAddrs, Commitments, Opts) ->
    % Get the IDs of all commitments for each committer.
```

### set

Deep merge keys in a message. Takes a map of key-value pairs and sets

```erlang
set(Message1, NewValuesMsg, Opts) ->
    OriginalPriv = hb_private:from_message(Message1),
	% Filter keys that are in the default device (this one).
```

### set_path

Special case of `set/3` for setting the `path` key. This cannot be set

```erlang
set_path(Base, #{ <<"value">> := Value }, Opts) ->
    set_path(Base, Value, Opts);
```

### set_path

Special case of `set/3` for setting the `path` key. This cannot be set

```erlang
set_path(Base, Value, Opts) when not is_map(Value) ->
    % Determine whether the `path' key is committed. If it is, we remove the
    % commitment if the new value is different. We try to minimize work by
    % doing the `hb_maps:get` first, as it is far cheaper than calculating
    % the committed keys.
```

### remove

Remove a key or keys from a message.

```erlang
remove(Message1, Key) ->
	remove(Message1, Key, #{}).
```

### remove

```erlang
remove(Message1, #{ <<"item">> := Key }, Opts) ->
    remove(Message1, #{ <<"items">> => [Key] }, Opts);
```

### remove

```erlang
remove(Message1, #{ <<"items">> := Keys }, Opts) ->
    { ok, hb_maps:without(Keys, Message1, Opts) }.
```

### keys

Get the public keys of a message.

```erlang
keys(Msg) ->
	keys(Msg, #{}).
```

### keys

```erlang
keys(Msg, Opts) when not is_map(Msg) ->
    case hb_ao:normalize_keys(Msg, Opts) of
        NormMsg when is_map(NormMsg) -> keys(NormMsg, Opts);
        _ -> throw(badarg)
    end;
```

### keys

```erlang
keys(Msg, Opts) ->
    {
        ok,
        lists:filter(
            fun(Key) -> not hb_private:is_private(Key) end,
            hb_maps:keys(Msg, Opts)
        )
    }.
```

### get

Return the value associated with the key as it exists in the message's

```erlang
get(Key, Msg, Opts) -> get(Key, Msg, #{ <<"path">> => <<"get">> }, Opts).
```

### get

Return the value associated with the key as it exists in the message's

```erlang
get(Key, Msg, _Msg2, Opts) ->
    case hb_private:is_private(Key) of
        true -> {error, not_found};
        false ->
            case hb_maps:get(Key, Msg, not_found, Opts) of
                not_found -> case_insensitive_get(Key, Msg, Opts);
                Value -> {ok, Value}
            end
    end.
```

### case_insensitive_get

Key matching should be case insensitive, following RFC-9110, so we 

```erlang
case_insensitive_get(Key, Msg, Opts) ->
    NormKey = hb_util:to_lower(hb_util:bin(Key)),
    NormMsg = hb_ao:normalize_keys(Msg, Opts),
    case hb_maps:get(NormKey, NormMsg, not_found, Opts) of
        not_found -> {error, not_found};
        Value -> {ok, Value}
    end.
```

### get_keys_mod_test

```erlang
get_keys_mod_test() ->
    ?assertEqual([a], hb_maps:keys(#{a => 1}, #{})).
```

### is_private_mod_test

```erlang
is_private_mod_test() ->
    ?assertEqual(true, hb_private:is_private(<<"private">>)),
    ?assertEqual(true, hb_private:is_private(<<"private.foo">>)),
    ?assertEqual(false, hb_private:is_private(<<"a">>)).
%%% Device functionality tests:
```

### keys_from_device_test

```erlang
keys_from_device_test() ->
    ?assertEqual({ok, [<<"a">>]}, hb_ao:resolve(#{ <<"a">> => 1 }, keys, #{})).
```

### case_insensitive_get_test

```erlang
case_insensitive_get_test() ->
	?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{ <<"a">> => 1 }, #{})),
%	?assertEqual({ok, 1}, case_insensitive_get(<<"a">>, #{ <<"A">> => 1 }, #{})),
	?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{ <<"a">> => 1 }, #{})).
	%?assertEqual({ok, 1}, case_insensitive_get(<<"A">>, #{ <<"A">> => 1 }, #{})).
```

### private_keys_are_filtered_test

```erlang
private_keys_are_filtered_test() ->
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_ao:resolve(#{ <<"a">> => 1, <<"private">> => 2 }, keys, #{})
    ),
    ?assertEqual(
        {ok, [<<"a">>]},
        hb_ao:resolve(#{ <<"a">> => 1, <<"priv_foo">> => 4 }, keys, #{})
    ).
```

### cannot_get_private_keys_test

```erlang
cannot_get_private_keys_test() ->
    ?assertEqual(
        {error, not_found},
        hb_ao:resolve(
            #{ <<"a">> => 1, <<"private_key">> => 2 },
            <<"private_key">>,
            #{ hashpath => ignore }
        )
    ).
```

### key_from_device_test

```erlang
key_from_device_test() ->
    ?assertEqual({ok, 1}, hb_ao:resolve(#{ <<"a">> => 1 }, <<"a">>, #{})).
```

### remove_test

```erlang
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
```

### set_conflicting_keys_test

```erlang
set_conflicting_keys_test() ->
	Msg1 = #{ <<"dangerous">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"dangerous">> => <<"Value2">> },
	?assertMatch({ok, #{ <<"dangerous">> := <<"Value2">> }},
		hb_ao:resolve(Msg1, Msg2, #{})).
```

### unset_with_set_test

```erlang
unset_with_set_test() ->
	Msg1 = #{ <<"dangerous">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"dangerous">> => unset },
	?assertMatch({ok, Msg3} when ?IS_EMPTY_MESSAGE(Msg3),
		hb_ao:resolve(Msg1, Msg2, #{ hashpath => ignore })).
```

### deep_unset_test

```erlang
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
```

### set_ignore_undefined_test

```erlang
set_ignore_undefined_test() ->
	Msg1 = #{ <<"test-key">> => <<"Value1">> },
	Msg2 = #{ <<"path">> => <<"set">>, <<"test-key">> => undefined },
	?assertEqual(#{ <<"test-key">> => <<"Value1">> },
		hb_private:reset(hb_util:ok(set(Msg1, Msg2, #{ hashpath => ignore })))).
```

### verify_test

```erlang
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
```

---

*Generated from [dev_message.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_message.erl)*
