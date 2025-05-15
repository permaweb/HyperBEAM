%%% @doc `push@1.0' takes a message or slot number, evaluates it, and recursively
%%% pushes the resulting messages to other processes. The `push'ing mechanism
%%% continues until the there are no remaining messages to push.
-module(dev_push).
%%% Public API
-export([push/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Push either a message or an assigned slot number. If a `Process' is
%% provided in the `body' of the request, it will be scheduled (initializing
%% it if it does not exist). Otherwise, the message specified by the given
%% `slot' key will be pushed.
%% 
%% Optional parameters:
%% `/result-depth': The depth to which the full contents of the result
%%                    will be included in the response. Default: 1, returning 
%%                    the full result of the first message, but only the 'tree'
%%                    of downstream messages.
%%  `/push-mode':    Whether or not the push should be done asynchronously.
%%                    Default: `sync', pushing synchronously.
push(Base, Req, Opts) ->
    Process = dev_process:as_process(Base, Opts),
    ?event(push, {push_base, {base, Process}, {req, Req}}, Opts),
    case hb_ao:get(<<"slot">>, {as, <<"message@1.0">>, Req}, no_slot, Opts) of
        no_slot ->
            case schedule_initial_message(Process, Req, Opts) of
                {ok, Assignment} ->
                    case find_type(hb_ao:get(<<"body">>, Assignment, Opts), Opts) of
                        <<"Message">> ->
                            ?event(push,
                                {pushing_message,
                                    {base, Process},
                                    {assignment, Assignment}
                                },
                                Opts
                            ),
                            push_with_mode(Process, Assignment, Opts);
                        <<"Process">> ->
                            ?event(push,
                                {initializing_process,
                                    {base, Process},
                                    {assignment, Assignment}},
                                Opts
                            ),
                            {ok, Assignment}
                    end;
                {error, Res} -> {error, Res}
            end;
        _ -> push_with_mode(Process, Req, Opts)
    end.

push_with_mode(Process, Req, Opts) ->
    Mode = is_async(Process, Req, Opts),
    case Mode of
        <<"sync">> ->
            do_push(Process, Req, Opts);
        <<"async">> ->
            spawn(fun() -> do_push(Process, Req, Opts) end)
    end.

%% @doc Determine if the push is asynchronous.
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

%% @doc Push a message or slot number, including its downstream results.
do_push(Process, Assignment, Opts) ->
    Slot = hb_ao:get(<<"slot">>, Assignment, Opts),
    ID = dev_process:process_id(Process, #{}, Opts),
    ?event(push, {push_computing_outbox, {process_id, ID}, {slot, Slot}}),
    {Status, Result} = hb_ao:resolve(
        {as, <<"process@1.0">>, Process},
        #{ <<"path">> => <<"compute/results">>, <<"slot">> => Slot },
        Opts#{ hashpath => ignore }
    ),
    % Determine if we should include the full compute result in our response.
    IncludeDepth = hb_ao:get(<<"result-depth">>, Assignment, 1, Opts),
    AdditionalRes =
        case IncludeDepth of
            X when X > 0 -> Result;
            _ -> #{}
        end,
    ?event(push_depth, {depth, IncludeDepth, {assignment, Assignment}}),
    ?event(push, {push_computed, {process, ID}, {slot, Slot}}),
    case {Status, hb_ao:get(<<"outbox">>, Result, #{}, Opts)} of
        {ok, NoResults} when ?IS_EMPTY_MESSAGE(NoResults) ->
            ?event(push_short, {done, {process, {string, ID}}, {slot, Slot}}),
            {ok, AdditionalRes#{ <<"slot">> => Slot, <<"process">> => ID }};
        {ok, Outbox} ->
            ?event(push, {push_found_outbox, {outbox, Outbox}}),
            Downstream =
                maps:map(
                    fun(Key, MsgToPush = #{ <<"target">> := Target }) ->
                        case hb_cache:read(Target, Opts) of
                            {ok, PushBase} ->
                                push_result_message(
                                    PushBase,
                                    MsgToPush,
                                    #{
                                        <<"process">> => ID,
                                        <<"slot">> => Slot,
                                        <<"outbox-key">> => Key,
                                        <<"result-depth">> => IncludeDepth
                                    },
                                    Opts
                                );
                            not_found ->
                                #{
                                    <<"response">> => <<"error">>,
                                    <<"status">> => 404,
                                    <<"target">> => Target,
                                    <<"reason">> =>
                                        <<"Could not access target process!">>
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
                    hb_ao:normalize_keys(Outbox)
                ),
            {ok, maps:merge(Downstream, AdditionalRes#{
                <<"slot">> => Slot,
                <<"process">> => ID
            })};
        {Err, Error} when Err == error; Err == failure ->
            ?event(push, {push_failed_to_find_outbox, {error, Error}}, Opts),
            {error, Error}
    end.

%% @doc Push a downstream message result. The `Origin' map contains information
%% about the origin of the message: The process that originated the message,
%% the slot number from which it was sent, and the outbox key of the message,
%% and the depth to which downstream results should be included in the message.
push_result_message(TargetProcess, MsgToPush, Origin, Opts) ->
    case hb_ao:get(<<"target">>, MsgToPush, undefined, Opts) of
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
                    NextSlotOnProc = hb_ao:get(<<"slot">>, Assignment, Opts),
                    PushedMsg = hb_ao:get(<<"body">>, Assignment, Opts),
                    % Get the ID of the message that was pushed. We already have
                    % the 'origin' message, but we need the signed ID.
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
                    % Push the message downstream. We decrease the result-depth.
                    Resurse =
                        hb_ao:resolve(
                            {as, <<"process@1.0">>, TargetAsProcess},
                            #{
                                <<"path">> => <<"push">>,
                                <<"slot">> => NextSlotOnProc,
                                <<"result-depth">> =>
                                    hb_ao:get(
                                        <<"result-depth">>,
                                        Origin,
                                        1,
                                        Opts
                                    ) - 1
                            },
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

%% @doc Augment the message with from-* keys, if it doesn't already have them.
normalize_message(MsgToPush, Opts) ->
    hb_ao:set(
        MsgToPush,
        #{
            <<"target">> => target_process(MsgToPush, Opts)
        },
        Opts#{ hashpath => ignore }
    ).

%% @doc Find the target process ID for a message to push.
target_process(MsgToPush, Opts) ->
    case hb_ao:get(<<"target">>, MsgToPush, Opts) of
        not_found -> undefined;
        RawTarget -> extract(target, RawTarget)
    end.

%% @doc Return either the `target' or the `hint'.
extract(hint, Raw) ->
    {_, Hint} = split_target(Raw),
    Hint;
extract(target, Raw) ->
    {Target, _} = split_target(Raw),
    Target.

%% @doc Split the target into the process ID and the optional query string.
split_target(RawTarget) ->
    case binary:split(RawTarget, [<<"?">>, <<"&">>]) of
        [Target, QStr] -> {Target, QStr};
        _ -> {RawTarget, <<>>}
    end.

%% @doc Add the necessary keys to the message to be scheduled, then schedule it.
%% If the remote scheduler does not support the given codec, it will be
%% downgraded and re-signed.
schedule_result(TargetProcess, MsgToPush, Origin, Opts) ->
    schedule_result(TargetProcess, MsgToPush, <<"httpsig@1.0">>, Origin, Opts).
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
    SignedReq =
        #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>,
            <<"body">> =>
                SignedMsg = hb_message:commit(
                    additional_keys(Origin, MsgToPush, Opts),
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
            {as, <<"process@1.0">>, TargetProcess},
            SignedReq,
            Opts#{ cache_control => <<"always">> }
        ),
    ?event(push, {push_sched_result, {status, ErlStatus}, {response, Res}}, Opts),
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
            ?event(push, {wrong_format, {422, Res}, {codec, Codec}}, Opts),
            case Codec of
                <<"ans104@1.0">> ->
                    {error, Res};
                <<"httpsig@1.0">> ->
                    ?event(push,
                        {downgrading_to_ans104,
                            {422, Res},
                            {codec, Codec},
                            {origin, Origin}
                        },
                        Opts
                    ),
                    schedule_result(
                        TargetProcess,
                        MsgToPush,
                        <<"ans104@1.0">>,
                        Origin,
                        Opts
                    )
            end;
        {error, _} ->
            {error, Res}
    end.

%% @doc Set the necessary keys in order for the recipient to know where the
%% message came from.
additional_keys(Origin, ToSched, Opts) ->
    ?event(push, {adding_keys, {origin, Origin}, {to, ToSched}}, Opts),
    hb_ao:set(
        ToSched,
        #{
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"type">> => <<"Message">>,
            <<"from-process">> => maps:get(<<"process">>, Origin)
        },
        Opts#{ hashpath => ignore }
    ).

%% @doc Push a message or a process, prior to pushing the resulting slot number.
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

multi_process_push_test_() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Opts = #{
            priv_wallet => hb:wallet(),
            cache_control => <<"always">>,
            store => [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"prefix">> => <<"cache-TEST">>
                }
            ]
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

push_with_redirect_hint_test_disabled() ->
    {timeout, 30, fun() ->
        dev_process:init(),
        Stores =
            [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"prefix">> => <<"cache-TEST">>
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