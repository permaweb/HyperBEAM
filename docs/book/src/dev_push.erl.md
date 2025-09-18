# dev_push

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_push.erl)

`push@1.0` takes a message or slot number, evaluates it, and recursively
pushes the resulting messages to other processes. The `push`ing mechanism
continues until the there are no remaining messages to push.

---

## Exported Functions

- `push/3`

---

### push

`push@1.0` takes a message or slot number, evaluates it, and recursively
Push either a message or an assigned slot number. If a `Process` is

```erlang
push(Base, Req, Opts) ->
    Process = dev_process:as_process(Base, Opts),
    ?event(push, {push_base, {base, Process}, {req, Req}}, Opts),
    case hb_ao:get(<<"slot">>, {as, <<"message@1.0">>, Req}, no_slot, Opts) of
        no_slot ->
            case schedule_initial_message(Process, Req, Opts) of
                {ok, Assignment} ->
                    case find_type(hb_ao:get(<<"body">>, Assignment, Opts), Opts) of
                        <<"Process">> ->
                            ?event(push,
                                {initializing_process,
                                    {base, Process},
                                    {assignment, Assignment}},
                                Opts
                            ),
                            {ok, Assignment};
                        _ ->
                            ?event(push,
                                {pushing_message,
                                    {base, Process},
                                    {assignment, Assignment}
                                },
                                Opts
                            ),
                            push_with_mode(Process, Assignment, Opts)
                    end;
                {error, Res} -> {error, Res}
            end;
        _ -> push_with_mode(Process, Req, Opts)
    end.
```

### push_with_mode

```erlang
push_with_mode(Process, Req, Opts) ->
    Mode = is_async(Process, Req, Opts),
    case Mode of
        <<"sync">> ->
            do_push(Process, Req, Opts);
        <<"async">> ->
            spawn(fun() -> do_push(Process, Req, Opts) end)
    end.
```

### is_async

Determine if the push is asynchronous.

```erlang
is_async(Process, Req, Opts) ->
    hb_ao:get_first(
        [
            {Req, <<"push-mode">>},
            {Process, <<"push-mode">>},
            {Process, <<"process/push-mode">>}
        ],
        <<"sync">>,
        Opts
    ).
```

### do_push

Push a message or slot number, including its downstream results.

```erlang
do_push(PrimaryProcess, Assignment, Opts) ->
    Slot = hb_ao:get(<<"slot">>, Assignment, Opts),
    ID = dev_process:process_id(PrimaryProcess, #{}, Opts),
    UncommittedID =
        dev_process:process_id(
            PrimaryProcess,
            #{ <<"commitments">> => <<"none">> },
            Opts
        ),
    BaseID = calculate_base_id(PrimaryProcess, Opts),
    ?event(debug,
        {push_computing_outbox,
            {process_id, ID},
            {base_id, BaseID},
            {slot, Slot}
        }
    ),
    ?event(push, {push_computing_outbox, {process_id, ID}, {slot, Slot}}),
    {Status, Result} = hb_ao:resolve(
        {as, <<"process@1.0">>, PrimaryProcess},
        #{ <<"path">> => <<"compute/results">>, <<"slot">> => Slot },
        Opts#{ hashpath => ignore }
    ),
    % Determine if we should include the full compute result in our response.
```

### maybe_evaluate_message

If the outbox message has a path we interpret it as a request to perform

```erlang
maybe_evaluate_message(Message, Opts) ->
    case hb_ao:get(<<"resolve">>, Message, Opts) of
        not_found -> 
            {ok, Message};
        ResolvePath ->
            ReqMsg =
                maps:without(
                    [<<"target">>],
                    Message
                ),
            ResolveOpts = Opts#{ force_message => true },
            case hb_ao:resolve(ReqMsg#{ <<"path">> => ResolvePath }, ResolveOpts) of
                {ok, EvalRes} ->
                    {
                        ok,
                        EvalRes#{
                            <<"target">> =>
                                hb_ao:get(
                                    <<"target">>,
                                    Message,
                                    Opts
                                )
                        }
                    };
                Err -> Err
            end
    end.
```

### push_result_message

Push a downstream message result. The `Origin` map contains information

```erlang
push_result_message(TargetProcess, MsgToPush, Origin, Opts) ->
    NormMsgToPush = hb_util:lower_case_key_map(MsgToPush, Opts),
    case hb_ao:get(<<"target">>, NormMsgToPush, undefined, Opts) of
        undefined ->
            ?event(push,
                {skip_no_target, {msg, MsgToPush}, {origin, Origin}},
                Opts
            ),
            #{};
        TargetID ->
            ?event(push,
                {pushing_child,
                    {target, TargetID},
                    {msg, MsgToPush},
                    {origin, Origin}
                },
                Opts
            ),
            case schedule_result(TargetProcess, MsgToPush, Origin, Opts) of
                {ok, Assignment} ->
                    % Analyze the result of the message push.
```

### normalize_message

Augment the message with from-* keys, if it doesn't already have them.

```erlang
normalize_message(MsgToPush, Opts) ->
    hb_ao:set(
        MsgToPush,
        #{
            <<"target">> => target_process(MsgToPush, Opts)
        },
        Opts#{ hashpath => ignore }
    ).
```

### target_process

Find the target process ID for a message to push.

```erlang
target_process(MsgToPush, Opts) ->
    case hb_ao:get(<<"target">>, MsgToPush, Opts) of
        not_found -> undefined;
        RawTarget -> extract(target, RawTarget)
    end.
```

### extract

Return either the `target` or the `hint`.

```erlang
extract(hint, Raw) ->
    {_, Hint} = split_target(Raw),
    Hint;
```

### extract

Return either the `target` or the `hint`.

```erlang
extract(target, Raw) ->
    {Target, _} = split_target(Raw),
    Target.
```

### split_target

Split the target into the process ID and the optional query string.

```erlang
split_target(RawTarget) ->
    case binary:split(RawTarget, [<<"?">>, <<"&">>]) of
        [Target, QStr] -> {Target, QStr};
        _ -> {RawTarget, <<>>}
    end.
```

### calculate_base_id

Calculate the base ID for a process. The base ID is not just the 

```erlang
calculate_base_id(GivenProcess, Opts) ->
    Process =
        case hb_ao:get(<<"process">>, GivenProcess, Opts#{ hashpath => ignore }) of
            not_found -> GivenProcess;
            Proc -> Proc
        end,
    BaseProcess = maps:without([<<"authority">>, <<"scheduler">>], Process),
    {ok, BaseID} = hb_ao:resolve(
        BaseProcess,
        #{ <<"path">> => <<"id">>, <<"commitments">> => <<"none">> },
        Opts
    ),
    ?event({push_generated_base, {id, BaseID}, {base, BaseProcess}}),
    BaseID.
```

### schedule_result

Add the necessary keys to the message to be scheduled, then schedule it.

```erlang
schedule_result(TargetProcess, MsgToPush, Origin, Opts) ->
    schedule_result(TargetProcess, MsgToPush, <<"httpsig@1.0">>, Origin, Opts).
```

### schedule_result

Add the necessary keys to the message to be scheduled, then schedule it.

```erlang
schedule_result(TargetProcess, MsgToPush, Codec, Origin, Opts) ->
    Target = hb_ao:get(<<"target">>, MsgToPush, Opts),
    ?event(push,
        {push_scheduling_result,
            {target, {string, Target}},
            {target_process, TargetProcess},
            {msg, MsgToPush},
            {codec, Codec},
            {origin, Origin}
        },
        Opts
    ),
    AugmentedMsg = augment_message(Origin, MsgToPush, Opts),
    ?event(push, {prepared_msg, {msg, AugmentedMsg}}, Opts),
    % Load the `accept-id`'d wallet into the `Opts` map, if requested.
```

### augment_message

Set the necessary keys in order for the recipient to know where the

```erlang
augment_message(Origin, ToSched, Opts) ->
    ?event(push, {adding_keys, {origin, Origin}, {to, ToSched}}, Opts),
    hb_message:uncommitted(
        hb_ao:set(
            ToSched,
            #{
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Message">>,
                <<"from-process">> => maps:get(<<"process">>, Origin),
                <<"from-uncommitted">> => maps:get(<<"from-uncommitted">>, Origin),
                <<"from-base">> => maps:get(<<"from-base">>, Origin),
                <<"from-scheduler">> => maps:get(<<"from-scheduler">>, Origin),
                <<"from-authority">> => maps:get(<<"from-authority">>, Origin)
            },
            Opts#{ hashpath => ignore }
        )
    ).
```

### apply_security

Apply the recipient's security policy to the message. Observes the 

```erlang
apply_security(Msg, TargetProcess, Codec, Opts) ->
    apply_security(policy, Msg, TargetProcess, Codec, Opts).
```

### apply_security

```erlang
apply_security(policy, Msg, TargetProcess, Codec, Opts) ->
    case hb_ao:get(<<"policy">>, TargetProcess, not_found, Opts) of
        not_found -> apply_security(authority, Msg, TargetProcess, Codec, Opts);
        Policy ->
            case hb_ao:resolve(Policy, Opts) of
                {ok, PolicyOpts} ->
                    case hb_ao:get(<<"accept-committers">>, PolicyOpts, Opts) of
                        not_found ->
                            apply_security(
                                authority,
                                Msg,
                                TargetProcess,
                                Codec,
                                Opts
                            );
                        Committers ->
                            commit_result(Msg, Committers, Codec, Opts)
                    end;
                {error, Error} ->
                    ?event(push, {policy_error, {error, Error}}, Opts),
                    apply_security(authority, Msg, TargetProcess, Codec, Opts)
            end
    end;
```

### apply_security

```erlang
apply_security(authority, Msg, TargetProcess, Codec, Opts) ->
    case hb_ao:get(<<"authority">>, TargetProcess, Opts) of
        not_found -> apply_security(default, Msg, TargetProcess, Codec, Opts);
    	Authorities when is_list(Authorities) ->
            % The `authority` key has already been parsed into a list of
            % committers. Sign with all local valid keys.
```

### apply_security

```erlang
apply_security(default, Msg, TargetProcess, Codec, Opts) ->
    ?event(push, {default_policy, {target, TargetProcess}}, Opts),
    commit_result(
        Msg,
        [hb_util:human_id(hb_opts:get(priv_wallet, no_viable_wallet, Opts))],
        Codec,
        Opts
    ).
```

### commit_result

Attempt to sign a result message with the given committers.

```erlang
commit_result(Msg, [], Codec, Opts) ->
    case hb_opts:get(push_always_sign, true, Opts) of
        true -> hb_message:commit(hb_message:uncommitted(Msg), Opts, Codec);
        false -> Msg
    end;
```

### commit_result

Attempt to sign a result message with the given committers.

```erlang
commit_result(Msg, Committers, Codec, Opts) ->
    Signed = lists:foldl(
        fun(Committer, Acc) ->
            case hb_opts:as(Committer, Opts) of
                {ok, CommitterOpts} ->
                    ?event(debug_commit, {signing_with_identity, Committer}),
                    hb_message:commit(Acc, CommitterOpts, Codec);
                {error, not_found} ->
                    ?event(debug_commit, desired_signer_not_available_on_node),
                    ?event(push,
                        {policy_warning,
                            {
                                unknown_committer,
                                Committer
                            }
                        },
                        Opts
                    ),
                    Acc
            end
        end,
        hb_message:uncommitted(Msg),
        Committers
    ),
    ?event(debug_commit,
        {signed_message_as, {explicit, hb_message:signers(Signed, Opts)}}
    ),
    case hb_message:signers(Signed, Opts) of
        [] ->
            ?event(debug_commit, signing_with_default_identity),
            commit_result(Msg, [], Codec, Opts);
        _FoundSigners ->
            Signed
    end.
```

### schedule_initial_message

Push a message or a process, prior to pushing the resulting slot number.

```erlang
schedule_initial_message(Base, Req, Opts) ->
    ModReq = Req#{ <<"path">> => <<"schedule">>, <<"method">> => <<"POST">> },
    ?event(push, {initial_push, {base, Base}, {req, ModReq}}, Opts),
    case hb_ao:resolve(Base, ModReq, Opts) of
        {ok, Res} ->
            case hb_ao:get(<<"status">>, Res, 200, Opts) of
                200 -> {ok, Res};
                307 ->
                    Location = hb_ao:get(<<"location">>, Res, Opts),
                    remote_schedule_result(Location, Req, Opts)
            end;
        {error, Res = #{ <<"status">> := 422 }} ->
            ?event(push, {initial_push_wrong_format, {error, Res}}, Opts),
            {error, Res};
        {error, Res} ->
            ?event(push, {initial_push_error, {error, Res}}, Opts),
            {error, Res}
    end.
```

### remote_schedule_result

```erlang
remote_schedule_result(Location, SignedReq, Opts) ->
    ?event(push, {remote_schedule_result, {location, Location}, {req, SignedReq}}, Opts),
    {Node, RedirectPath} = parse_redirect(Location, Opts),
    Path =
        case find_type(SignedReq, Opts) of
            <<"Process">> -> <<"/schedule">>;
            <<"Message">> -> RedirectPath
        end,
    % Store a copy of the message for ourselves.
```

### find_type

```erlang
find_type(Req, Opts) ->
    hb_ao:get_first(
        [
            {Req, <<"type">>},
            {Req, <<"body/type">>}
        ],
        Opts
    ).
```

### parse_redirect

```erlang
parse_redirect(Location, Opts) ->
    Parsed = uri_string:parse(Location),
    Node =
        uri_string:recompose(
            (hb_maps:remove(query, Parsed, Opts))#{
                path => <<"/schedule">>
            }
        ),
    {Node, hb_maps:get(path, Parsed, undefined, Opts)}.
```

### full_push_test_

```erlang
full_push_test_() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Opts = #{
            process_async_cache => false,
            priv_wallet => hb:wallet(),
            cache_control => <<"always">>,
            store => [
                #{ <<"store-module">> => hb_store_fs, <<"name">> => <<"cache-TEST">> },
                #{ <<"store-module">> => hb_store_gateway,
                    <<"store">> => #{
                        <<"store-module">> => hb_store_fs,
                        <<"name">> => <<"cache-TEST">>
                    }
                }
            ]
        },
        Msg1 = dev_process:test_aos_process(Opts),
        hb_cache:write(Msg1, Opts),
        {ok, SchedInit} =
            hb_ao:resolve(Msg1, #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Msg1
            },
            Opts
        ),
        ?event({test_setup, {msg1, Msg1}, {sched_init, SchedInit}}),
        Script = ping_pong_script(2),
        ?event({script, Script}),
        {ok, Msg2} = dev_process:schedule_aos_call(Msg1, Script, Opts),
        ?event({msg_sched_result, Msg2}),
        {ok, StartingMsgSlot} =
            hb_ao:resolve(Msg2, #{ <<"path">> => <<"slot">> }, Opts),
        ?event({starting_msg_slot, StartingMsgSlot}),
        Msg3 =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => StartingMsgSlot
            },
        {ok, _} = hb_ao:resolve(Msg1, Msg3, Opts),
        ?assertEqual(
            {ok, <<"Done.">>},
            hb_ao:resolve(Msg1, <<"now/results/data">>, Opts)
        )
    end}.
```

### push_as_identity_test_

```erlang
push_as_identity_test_() ->
    {timeout, 90, fun() ->
        dev_process:init(),
        % Create a new identity for the scheduler.
```

### multi_process_push_test_

```erlang
multi_process_push_test_() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Opts = #{
            priv_wallet => hb:wallet(),
            cache_control => <<"always">>
        },
        Proc1 = dev_process:test_aos_process(Opts),
        hb_cache:write(Proc1, Opts),
        {ok, _SchedInit1} =
            hb_ao:resolve(Proc1, #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Proc1
            },
            Opts
        ),
        {ok, _} = dev_process:schedule_aos_call(Proc1, reply_script()),
        Proc2 = dev_process:test_aos_process(Opts),
        hb_cache:write(Proc2, Opts),
        {ok, _SchedInit2} =
            hb_ao:resolve(Proc2, #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Proc2
            },
            Opts
        ),
        ProcID1 = hb_message:id(Proc1, all, Opts),
        ProcID2 = hb_message:id(Proc2, all, Opts),
        ?event(push, {testing_with, {proc1_id, ProcID1}, {proc2_id, ProcID2}}),
        {ok, ToPush} = dev_process:schedule_aos_call(
            Proc2,
            <<
                "Handlers.add(\"Pong\",\n"
                "   function (test) return true end,\n"
                "   function(m)\n"
                "       print(\"GOT PONG\")\n"
                "   end\n"
                ")\n"
                "Send({ Target = \"", (ProcID1)/binary, "\", Action = \"Ping\" })"
            >>
        ),
        SlotToPush = hb_ao:get(<<"slot">>, ToPush, Opts),
        ?event(push, {slot_to_push_proc2, SlotToPush}),
        Msg3 =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => SlotToPush,
                <<"result-depth">> => 1
            },
        {ok, PushResult} = hb_ao:resolve(Proc2, Msg3, Opts),
        ?event(push, {push_result_proc2, PushResult}),
        AfterPush = hb_ao:resolve(Proc2, <<"now/results/data">>, Opts),
        ?event(push, {after_push, AfterPush}),
        ?assertEqual({ok, <<"GOT PONG">>}, AfterPush)
    end}.
```

### push_with_redirect_hint_test_disabled

```erlang
push_with_redirect_hint_test_disabled() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Stores =
            [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-TEST">>
                }
            ],
        ExtOpts = #{ priv_wallet => ar_wallet:new(), store => Stores },
        LocalOpts = #{ priv_wallet => hb:wallet(), store => Stores },
        ExtScheduler = hb_http_server:start_node(ExtOpts),
        ?event(push, {external_scheduler, {location, ExtScheduler}}),
        % Create the Pong server and client
        Client = dev_process:test_aos_process(),
        PongServer = dev_process:test_aos_process(ExtOpts),
        % Push the new process that runs on the external scheduler
        {ok, ServerSchedResp} =
            hb_http:post(
                ExtScheduler,
                <<"/push">>,
                PongServer,
                ExtOpts
            ),
        ?event(push, {pong_server_sched_resp, ServerSchedResp}),
        % Get the IDs of the server process
        PongServerID =
            hb_ao:get(
                <<"process/id">>,
                dev_process:ensure_process_key(PongServer, LocalOpts),
                LocalOpts
            ),
        {ok, ServerScriptSchedResp} =
            hb_http:post(
                ExtScheduler,
                <<PongServerID/binary, "/push">>,
                #{
                    <<"body">> =>
                        hb_message:commit(
                            #{
                                <<"target">> => PongServerID,
                                <<"action">> => <<"Eval">>,
                                <<"type">> => <<"Message">>,
                                <<"data">> => reply_script()
                            },
                            ExtOpts
                        )
                },
                ExtOpts
            ),
        ?event(push, {pong_server_script_sched_resp, ServerScriptSchedResp}),
        {ok, ToPush} =
            dev_process:schedule_aos_call(
                Client,
                <<
                    "Handlers.add(\"Pong\",\n"
                    "   function (test) return true end,\n"
                    "   function(m)\n"
                    "       print(\"GOT PONG\")\n"
                    "   end\n"
                    ")\n"
                    "Send({ Target = \"",
                        (PongServerID)/binary, "?hint=",
                        (ExtScheduler)/binary,
                    "\", Action = \"Ping\" })\n"
                >>,
                LocalOpts
            ),
        SlotToPush = hb_ao:get(<<"slot">>, ToPush, LocalOpts),
        ?event(push, {slot_to_push_client, SlotToPush}),
        Msg3 = #{ <<"path">> => <<"push">>, <<"slot">> => SlotToPush },
        {ok, PushResult} = hb_ao:resolve(Client, Msg3, LocalOpts),
        ?event(push, {push_result_client, PushResult}),
        AfterPush = hb_ao:resolve(Client, <<"now/results/data">>, LocalOpts),
        ?event(push, {after_push, AfterPush}),
        % Note: This test currently only gets a reply that the message was not
        % trusted by the process. To fix this, we would have to add another 
        % trusted authority to the `test_aos_process' call. For now, this is 
        % enough to validate that redirects are pushed through correctly.
```

### push_prompts_encoding_change_test_

```erlang
push_prompts_encoding_change_test_() ->
    {timeout, 30, fun push_prompts_encoding_change/0}.
```

### push_prompts_encoding_change

```erlang
push_prompts_encoding_change() ->
    dev_process:init(),
    Opts = #{
        priv_wallet => hb:wallet(),
        cache_control => <<"always">>,
        store =>
            [
                #{ <<"store-module">> => hb_store_fs, <<"name">> => <<"cache-TEST">> },
                % Include a gateway store so that we can get the legacynet 
                % process when needed.
```

### oracle_push_test_

```erlang
oracle_push_test_() -> {timeout, 30, fun oracle_push/0}.
```

### oracle_push

```erlang
oracle_push() ->
    dev_process:init(),
    Client = dev_process:test_aos_process(),
    {ok, _} = hb_cache:write(Client, #{}),
    {ok, _} = dev_process:schedule_aos_call(Client, oracle_script()),
    Msg3 =
        #{
            <<"path">> => <<"push">>,
            <<"slot">> => 0
        },
    {ok, PushResult} = hb_ao:resolve(Client, Msg3, #{ priv_wallet => hb:wallet() }),
    ?event({result, PushResult}),
    ComputeRes =
        hb_ao:resolve(
            Client,
            <<"now/results/data">>,
            #{ priv_wallet => hb:wallet() }
        ),
    ?event({compute_res, ComputeRes}),
    ?assertMatch({ok, _}, ComputeRes).
```

### nested_push_prompts_encoding_change_test_

Test that a message that generates another message which resides on an

```erlang
nested_push_prompts_encoding_change_test_() ->
    {timeout, 30, fun nested_push_prompts_encoding_change/0}.
```

### nested_push_prompts_encoding_change

```erlang
nested_push_prompts_encoding_change() ->
    dev_process:init(),
    Opts = #{
        priv_wallet => hb:wallet(),
        cache_control => <<"always">>,
        store => hb_opts:get(store)
    },
    ?event(push_debug, {opts, Opts}),
    Msg1 = dev_process:test_aos_process(Opts),
    hb_cache:write(Msg1, Opts),
    {ok, SchedInit} =
        hb_ao:resolve(Msg1, #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>,
            <<"body">> => Msg1
        },
        Opts
    ),
    ?event({test_setup, {msg1, Msg1}, {sched_init, SchedInit}}),
    Script = message_to_legacynet_scheduler_script(),
    ?event({script, Script}),
    {ok, Msg2} = dev_process:schedule_aos_call(Msg1, Script),
    ?event(push, {msg_sched_result, Msg2}),
    {ok, StartingMsgSlot} =
        hb_ao:resolve(Msg2, #{ <<"path">> => <<"slot">> }, Opts),
    ?event({starting_msg_slot, StartingMsgSlot}),
    Msg3 =
        #{
            <<"path">> => <<"push">>,
            <<"slot">> => StartingMsgSlot
        },
    {ok, Res} = hb_ao:resolve(Msg1, Msg3, Opts),
    ?event(push, {res, Res}),
    Msg = hb_message:commit(#{
        <<"path">> => <<"push">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:commit(
                #{
                    <<"target">> => hb_message:id(Msg1, all, Opts),
                    <<"action">> => <<"Ping">>
                },
                Opts
            )
    }, Opts),
    ?event(push, {msg1, Msg}),
    Res2 =
        hb_ao:resolve_many(
            [
                hb_message:id(Msg1, all, Opts),
                {as, <<"process@1.0">>, <<>>},
                Msg
            ],
            Opts
        ),
    ?assertMatch({ok, #{ <<"1">> := #{ <<"resulted-in">> := _ }}}, Res2).
-endif.
```

### ping_pong_script

```erlang
ping_pong_script(Limit) ->
    <<
        "Handlers.add(\"Ping\",\n"
        "   function (test) return true end,\n"
        "   function(m)\n"
        "       C = tonumber(m.Count)\n"
        "       if C <= ", (integer_to_binary(Limit))/binary, " then\n"
        "           Send({ Target = ao.id, Action = \"Ping\", Count = C + 1 })\n"
        "           print(\"Ping\", C + 1)\n"
        "       else\n"
        "           print(\"Done.\")\n"
        "       end\n"
        "   end\n"
        ")\n"
        "Send({ Target = ao.id, Action = \"Ping\", Count = 1 })\n"
    >>.
```

### reply_script

```erlang
reply_script() ->
    <<
        """
        Handlers.add("Reply",
           { Action = "Ping" },
           function(m)
               print("Replying to...")
               print(m.From)
               Send({ Target = m.From, Action = "Reply", Message = "Pong!" })
               print("Done.")
           end
        )
        """
    >>.
```

### message_to_legacynet_scheduler_script

```erlang
message_to_legacynet_scheduler_script() ->
    <<
        """
        Handlers.add("Ping",
           { Action = "Ping" },
           function(m)
               print("Pinging...")
               print(m.From)
               Send({
                    Target = "QQiMcAge5ZtxcUV7ruxpi16KYRE8UBP0GAAqCIJPXz0",
                    Action = "Ping"
                })
               print("Done.")
           end
        )
        """
    >>.
```

### oracle_script

```erlang
oracle_script() ->
    <<
        """
        Handlers.add("Oracle",
            function(m)
                return true
            end,
            function(m)
                print(m.Body)
            end
        )
        Send({
            target = ao.id,
            resolve = "/~relay@1.0/call",
            ["relay-path"] = "https://arweave.net"
        })
        """
```

---

*Generated from [dev_push.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_push.erl)*
