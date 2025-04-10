-module(dev_push).
-moduledoc """
`push@1.0` takes a message or slot number, evaluates it, and recursively
pushes the resulting messages to other processes. The `push`ing mechanism
continues until the there are no remaining messages to push.
""".
%%% Public API
-export([push/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-doc """
Push either a message or an assigned slot number.
""".
push(Base, Req, Opts) ->
    ModBase = dev_process:as_process(Base, Opts),
    ?event(push, {push_base, {base, ModBase}, {req, Req}}, Opts),
    case hb_ao:get(<<"slot">>, {as, <<"message@1.0">>, Req}, no_slot, Opts) of
        no_slot ->
            case schedule_initial_message(ModBase, Req, Opts) of
                {ok, Assignment} ->
                    case find_type(hb_ao:get(<<"body">>, Assignment, Opts), Opts) of
                        <<"Message">> ->
                            ?event(push,
                                {pushing_message,
                                    {base, ModBase},
                                    {assignment, Assignment}
                                },
                                Opts
                            ),
                            push_with_mode(ModBase, Assignment, Opts);
                        <<"Process">> ->
                            ?event(push,
                                {initializing_process,
                                    {base, ModBase},
                                    {assignment, Assignment}},
                                Opts
                            ),
                            {ok, Assignment}
                    end;
                {error, Res} -> {error, Res}
            end;
        _ -> push_with_mode(ModBase, Req, Opts)
    end.

push_with_mode(Base, Req, Opts) ->
    Mode = is_async(Base, Req, Opts),
    case Mode of
        <<"sync">> ->
            do_push(Base, Req, Opts);
        <<"async">> ->
            spawn(fun() -> do_push(Base, Req, Opts) end)
    end.

-doc """
Determine if the push is asynchronous.
""".
is_async(Base, Req, Opts) ->
    hb_ao:get_first(
        [
            {Req, <<"push-mode">>},
            {Base, <<"push-mode">>},
            {Base, <<"process/push-mode">>}
        ],
        <<"sync">>,
        Opts
    ).

-doc """
Push a message or slot number.
""".
do_push(Base, Assignment, Opts) ->
    Slot = hb_ao:get(<<"slot">>, Assignment, Opts),
    ID = dev_process:process_id(Base, #{}, Opts),
    ?event(push, {push_computing_outbox, {process_id, ID}, {slot, Slot}}),
    Result = hb_ao:resolve(
        {as, <<"process@1.0">>, Base},
        #{ <<"path">> => <<"compute/results/outbox">>, <<"slot">> => Slot },
        Opts#{ hashpath => ignore }
    ),
    ?event(push, {push_computed, {process, ID}, {slot, Slot}}),
    case Result of
        {ok, NoResults} when ?IS_EMPTY_MESSAGE(NoResults) ->
            ?event(push_short, {push_complete, {process, {string, ID}}, {slot, Slot}}),
            {ok, #{ <<"slot">> => Slot, <<"process">> => ID }};
        {ok, Outbox} ->
            Downstream =
                maps:map(
                    fun(Key, MsgToPush = #{ <<"target">> := Target }) ->
                        case hb_cache:read(Target, Opts) of
                            {ok, PushBase} ->
                                push_result_message(PushBase, Slot, Key, MsgToPush, Opts);
                            not_found ->
                                #{
                                    <<"response">> => <<"error">>,
                                    <<"status">> => 404,
                                    <<"target">> => Target,
                                    <<"reason">> => <<"Could not access target process!">>
                                }
                        end;
                       (Key, Msg) ->
                            #{
                                <<"response">> => <<"error">>,
                                <<"status">> => 422,
                                <<"outbox-index">> => Key,
                                <<"reason">> => <<"No target process found.">>,
                                <<"message">> => Msg
                            }
                    end,
                    Outbox
                ),
            {ok, Downstream#{
                <<"slot">> => Slot,
                <<"process">> => ID
            }};
        {Err, Error} when Err == error; Err == failure -> {error, Error}
    end.

push_result_message(Base, FromSlot, Key, MsgToPush, Opts) ->
    case hb_ao:get(<<"target">>, MsgToPush, undefined, Opts) of
        undefined ->
            ?event(push, {skip_no_target, {key, Key}, MsgToPush}, Opts),
            #{};
        TargetID ->
            ?event(push,
                {pushing_child,
                    {originates_from_slot, FromSlot},
                    {outbox_key, Key},
                    {target_id, TargetID}
                },
                Opts
            ),
            case schedule_result(Base, MsgToPush, Opts) of
                {ok, Assignment} ->
                    NextSlotOnProc = hb_ao:get(<<"slot">>, Assignment, Opts),
                    PushedMsg = hb_ao:get(<<"body">>, Assignment, Opts),
                    PushedMsgID = hb_message:id(PushedMsg, all, Opts),
                    ?event(push_short,
                        {pushed_message_to,
                            {process, TargetID},
                            {slot, NextSlotOnProc}
                        }
                    ),
                    {ok, TargetBase} = hb_cache:read(TargetID, Opts),
                    TargetAsProcess = dev_process:ensure_process_key(TargetBase, Opts),
                    RecvdID = hb_message:id(TargetBase, all),
                    ?event(push, {recvd_id, {id, RecvdID}, {msg, TargetAsProcess}}),
                    Resurse = hb_ao:resolve(
                        {as, <<"process@1.0">>, TargetAsProcess},
                        #{ <<"path">> => <<"push">>, <<"slot">> => NextSlotOnProc },
                        Opts#{ cache_control => <<"always">> }
                    ),
                    case Resurse of
                        {ok, Downstream} ->
                            #{
                                <<"id">> => PushedMsgID,
                                <<"target">> => TargetID,
                                <<"slot">> => NextSlotOnProc,
                                <<"resulted-in">> => Downstream
                            };
                        {error, Error} ->
                            ?event(push, {push_failed, {error, Error}}, Opts),
                            #{
                                <<"response">> => <<"error">>,
                                <<"target">> => TargetID,
                                <<"reason">> => Error
                            }
                    end;
                {error, Error} ->
                    ?event(push, {push_failed, {error, Error}}, Opts),
                    #{
                        <<"response">> => <<"error">>,
                        <<"target">> => TargetID,
                        <<"reason">> => Error
                    }
            end
    end.

-doc """
Augment the message with from-* keys, if it doesn't already have them.
""".
normalize_message(MsgToPush, Opts) ->
    hb_ao:set(
        MsgToPush,
        #{
            <<"target">> => target_process(MsgToPush, Opts)
        },
        Opts#{ hashpath => ignore }
    ).

-doc """
Find the target process ID for a message to push.
""".
target_process(MsgToPush, Opts) ->
    case hb_ao:get(<<"target">>, MsgToPush, Opts) of
        not_found -> undefined;
        RawTarget -> extract(target, RawTarget)
    end.

-doc """
Return either the `target` or the `hint`.
""".
extract(hint, Raw) ->
    {_, Hint} = split_target(Raw),
    Hint;
extract(target, Raw) ->
    {Target, _} = split_target(Raw),
    Target.

split_target(RawTarget) ->
    case binary:split(RawTarget, [<<"?">>, <<"&">>]) of
        [Target, QStr] -> {Target, QStr};
        _ -> {RawTarget, <<>>}
    end.

schedule_result(Base, MsgToPush, Opts) ->
    schedule_result(Base, MsgToPush, <<"httpsig@1.0">>, Opts).
schedule_result(Base, MsgToPush, Codec, Opts) ->
    Target = hb_ao:get(<<"target">>, MsgToPush, Opts),
    ?event(push,
        {push_scheduling_result,
            {target, {string, Target}},
            {target_process, Base},
            {msg, MsgToPush},
            {codec, Codec}
        },
        Opts
    ),
    SignedReq =
        #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>,
            <<"body">> =>
                SignedMsg = hb_message:commit(
                    additional_keys(Base, MsgToPush, Opts),
                    Opts,
                    Codec
                )
        },
    ?event(
        {push_scheduling_result,
            {signed_req, SignedReq},
            {verifies, hb_message:verify(SignedMsg, signers)}
        }
    ),
    {ErlStatus, Res} =
        hb_ao:resolve(
            {as, <<"process@1.0">>, Base},
            SignedReq,
            Opts#{ cache_control => <<"always">> }
        ),
    ?event(push, {push_scheduling_result, {status, ErlStatus}, {response, Res}}, Opts),
    case {ErlStatus, hb_ao:get(<<"status">>, Res, 200, Opts)} of
        {ok, 200} ->
            {ok, Res};
        {ok, 307} ->
            Location = hb_ao:get(<<"location">>, Res, Opts),
            ?event(push, {redirect, {location, {explicit, Location}}}),
            NormMsg = normalize_message(MsgToPush, Opts),
            SignedNormMsg = hb_message:commit(NormMsg, Opts),
            remote_schedule_result(Location, SignedNormMsg, Opts);
        {error, 422} ->
            ?event(push, {received_wrong_format, {422, Res}, {codec, Codec}}, Opts),
            case Codec of
                <<"ans104@1.0">> ->
                    {error, Res};
                <<"httpsig@1.0">> ->
                    ?event(push, {downgrading_to_ans104, {422, Res}, {codec, Codec}}, Opts),
                    schedule_result(Base, MsgToPush, <<"ans104@1.0">>, Opts)
            end;
        {error, _} ->
            {error, Res}
    end.

-doc """
Set the necessary keys in order for the recipient to know where the
message came from.
""".
additional_keys(FromMsg, ToSched, Opts) ->
    hb_ao:set(
        ToSched,
        #{
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"type">> => <<"Message">>,
            <<"from-process">> => hb_message:id(FromMsg, all, Opts)
        },
        Opts#{ hashpath => ignore }
    ).

-doc """
Push a message or a process, prior to pushing the resulting slot number.
""".
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

remote_schedule_result(Location, SignedReq, Opts) ->
    ?event(push, {remote_schedule_result, {location, Location}, {req, SignedReq}}, Opts),
    {Node, RedirectPath} = parse_redirect(Location),
    Path =
        case find_type(SignedReq, Opts) of
            <<"Process">> -> <<"/schedule">>;
            <<"Message">> -> RedirectPath
        end,
    % Store a copy of the message for ourselves.
    {ok, _} = hb_cache:write(SignedReq, Opts),
    ?event(push, {remote_schedule_result, {path, Path}}, Opts),
    case hb_http:post(Node, Path, maps:without([<<"path">>], SignedReq), Opts) of
        {ok, Res} ->
            ?event(push, {remote_schedule_result, {res, Res}}, Opts),
            case hb_ao:get(<<"status">>, Res, 200, Opts) of
                200 -> {ok, Res};
                307 ->
                    NewLocation = hb_ao:get(<<"location">>, Res, Opts),
                    remote_schedule_result(NewLocation, SignedReq, Opts)
            end;
        {error, Res} ->
            {error, Res}
    end.

find_type(Req, Opts) ->
    hb_ao:get_first(
        [
            {Req, <<"type">>},
            {Req, <<"body/type">>}
        ],
        Opts
    ).

parse_redirect(Location) ->
    Parsed = uri_string:parse(Location),
    Node =
        uri_string:recompose(
            (maps:remove(query, Parsed))#{
                path => <<"/schedule">>
            }
        ),
    {Node, maps:get(path, Parsed)}.

%%% Tests

full_push_test_() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Opts = #{
            priv_wallet => hb:wallet(),
            cache_control => <<"always">>,
            store => [
                #{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST">> },
                #{ <<"store-module">> => hb_store_gateway,
                    <<"store">> => #{
                        <<"store-module">> => hb_store_fs,
                        <<"prefix">> => <<"cache-TEST">>
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
        {ok, _} = hb_ao:resolve(Msg1, Msg3, Opts),
        ?assertEqual(
            {ok, <<"Done.">>},
            hb_ao:resolve(Msg1, <<"now/results/data">>, Opts)
        )
    end}.

multi_process_push_test_disabled() ->
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
        ProcID1 =
            hb_ao:get(
                <<"process/id">>,
                dev_process:ensure_process_key(Proc1, Opts),
                Opts
            ),
        ProcID2 =
            hb_ao:get(
                <<"process/id">>,
                dev_process:ensure_process_key(Proc2, Opts),
                Opts
            ),
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
                "Send({ Target = \"", (ProcID1)/binary, "\", Action = \"Ping\" })\n"
            >>
        ),
        SlotToPush = hb_ao:get(<<"slot">>, ToPush, Opts),
        ?event(push, {slot_to_push_proc2, SlotToPush}),
        Msg3 =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => SlotToPush
            },
        {ok, PushResult} = hb_ao:resolve(Proc2, Msg3, Opts),
        ?event(push, {push_result_proc2, PushResult}),
        AfterPush = hb_ao:resolve(Proc2, <<"now/results/data">>, Opts),
        ?event(push, {after_push, AfterPush}),
        ?assertEqual({ok, <<"GOT PONG">>}, AfterPush)
    end}.

push_with_redirect_hint_test_disabled() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Stores = [#{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST">> }],
        ExtOpts = #{ priv_wallet => ar_wallet:new(), store => Stores },
        LocalOpts = #{ priv_wallet => hb:wallet(), store => Stores },
        ExtScheduler = hb_http_server:start_node(ExtOpts),
        ?event(push, {external_scheduler, {location, ExtScheduler}}),
        % Create the Pong server and client
        Client = dev_process:test_aos_process(),
        PongServer = dev_process:test_aos_process(ExtOpts),
        % Push the new process that runs on the external scheduler
        {ok, ServerSchedResp} = hb_http:post(ExtScheduler, <<"/push">>, PongServer, ExtOpts),
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
        % trusted authority to the `test_aos_process` call. For now, this is 
        % enough to validate that redirects are pushed through correctly.
        ?assertEqual({ok, <<"GOT PONG">>}, AfterPush)
    end}.

push_prompts_encoding_change_test() ->
    dev_process:init(),
    Opts = #{
        priv_wallet => hb:wallet(),
        cache_control => <<"always">>,
        store =>
            [
                #{ <<"store-module">> => hb_store_fs, <<"prefix">> => <<"cache-TEST">> },
                % Include a gateway store so that we can get the legacynet 
                % process when needed.
                #{ <<"store-module">> => hb_store_gateway,
                    <<"store">> => #{
                        <<"store-module">> => hb_store_fs,
                        <<"prefix">> => <<"cache-TEST">>
                    }
                }
            ]
    },
    Msg = hb_message:commit(#{
        <<"path">> => <<"push">>,
        <<"method">> => <<"POST">>,
        <<"target">> => <<"QQiMcAge5ZtxcUV7ruxpi16KYRE8UBP0GAAqCIJPXz0">>,
        <<"action">> => <<"Eval">>,
        <<"data">> => <<"print(\"Please ignore!\")">>
    }, Opts),
    ?event(push, {msg1, Msg}),
    Res =
        hb_ao:resolve_many(
            [
                <<"QQiMcAge5ZtxcUV7ruxpi16KYRE8UBP0GAAqCIJPXz0">>,
                {as, <<"process@1.0">>, <<>>},
                Msg
            ],
            Opts
        ),
    ?assertMatch({error, #{ <<"status">> := 422 }}, Res).

%%% Test helpers

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

reply_script() ->
    <<
        "Handlers.add(\"Reply\",\n"
        "   function (test) return true end,\n"
        "   function(m)\n"
        "       print(\"Replying to...\")\n"
        "       print(m.From)\n"
        "       Send({ Target = m.From, Action = \"Reply\", Message = \"Pong!\" })\n"
        "       print(\"Done.\")\n"
        "   end\n"
        ")\n"
    >>.