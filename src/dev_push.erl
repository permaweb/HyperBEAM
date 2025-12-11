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
    Process = dev_process_lib:as_process(Base, Opts),
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
do_push(PrimaryProcess, Assignment, Opts) ->
    Slot = hb_ao:get(<<"slot">>, Assignment, Opts),
    ID = dev_process_lib:process_id(PrimaryProcess, #{}, Opts),
    UncommittedID =
        dev_process_lib:process_id(
            PrimaryProcess,
            #{ <<"commitments">> => <<"none">> },
            Opts
        ),
    BaseID = calculate_base_id(PrimaryProcess, Opts),
    ?event(debug,
        {push_computing_outbox,
            {process_id, ID},
            {base_id, BaseID},
            {process_uncommitted_id, UncommittedID},
            {slot, Slot}
        }
    ),
    ?event(push, {push_computing_outbox, {process_id, ID}, {slot, Slot}}),
    {Status, Result} =
        try
            hb_ao:resolve(
                {as, <<"process@1.0">>, PrimaryProcess},
                    #{ <<"path">> => <<"compute/results">>, <<"slot">> => Slot },
                    Opts#{ hashpath => ignore }
                )
        catch
            Class:Reason:Trace ->
                ?event(
                    push,
                    {push_compute_failed,
                        {process, PrimaryProcess},
                        {slot, Slot},
                        {class, Class},
                        {reason, Reason},
                        {stack, {trace, Trace}}
                    },
                    Opts
                ),
                {error,
                    #{
                        <<"body">> =>
                                <<
                                    "Pushing slot ",
                                    (hb_util:bin(Slot))/binary,
                                    " failed on process `",
                                    (hb_util:bin(ID))/binary,
                                    "` with error: ",
                                    (hb_util:bin(hb_format:term(Reason, Opts, 0)))
                                        /binary
                                >>,
                        <<"class">> => Class,
                        <<"reason">> => Reason
                    }
                }
        end,
    % Determine if we should include the full compute result in our response.
    IncludeDepth = hb_ao:get(<<"result-depth">>, Assignment, 1, Opts),
    AdditionalRes =
        case IncludeDepth of
            X when X > 0 -> Result;
            _ -> #{}
        end,
    ?event(push_depth, {depth, IncludeDepth, {assignment, Assignment}}),
    ?event(push,
        {push_compute_result,
            {process, ID},
            {slot, Slot},
            {status, Status}
        }
    ),
    ?event(debug,
        {push_computed,
            {status, Status},
            {assignment, Assignment},
            {request, hb_maps:get(<<"body">>, Assignment, Assignment, Opts)},
            {result,
                if is_list(Result) ->
                    hb_ao:normalize_keys(Result);
                true -> Result
                end
            }
        }),
    case {Status, hb_ao:get(<<"outbox">>, Result, #{}, Opts)} of
        {ok, NoResults} when ?IS_EMPTY_MESSAGE(NoResults) ->
            ?event(push_short, {done, {process, {string, ID}}, {slot, Slot}}),
            {ok, AdditionalRes#{ <<"slot">> => Slot, <<"process">> => ID }};
        {ok, Outbox} ->
            ?event(push, {push_found_outbox, {outbox, Outbox}}),
            Downstream =
                hb_maps:map(
                    fun(Key, RawMsgToPush = #{ <<"target">> := Target }) ->
                        MsgToPush =
                            case maybe_evaluate_message(RawMsgToPush, Opts) of
                                {ok, R} -> R;
                                Err ->
                                    #{
                                        <<"resolve">> => <<"error">>,
                                        <<"target">> => ID,
                                        <<"status">> => 400,
                                        <<"outbox-index">> => Key,
                                        <<"reason">> => Err,
                                        <<"source">> => RawMsgToPush
                                    }
                            end,
                        case hb_cache:read(Target, Opts) of
                            {ok, DownstreamProcess} ->
                                push_result_message(
                                    DownstreamProcess,
                                    MsgToPush,
                                    #{
                                        <<"process">> => ID,
                                        <<"slot">> => Slot,
                                        <<"outbox-key">> => Key,
                                        <<"result-depth">> => IncludeDepth,
                                        <<"from-base">> => BaseID,
                                        <<"from-uncommitted">> => UncommittedID,
                                        <<"from-scheduler">> =>
                                            hb_ao:get(
                                                <<"scheduler">>,
                                                PrimaryProcess,
                                                Opts
                                            ),
                                        <<"from-authority">> =>
                                            hb_ao:get(
                                                <<"authority">>,
                                                PrimaryProcess,
                                                Opts
                                            )
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
                                <<"status">> => 404,
                                <<"outbox-index">> => Key,
                                <<"reason">> =>
                                    <<"Target process not available.">>,
                                <<"message">> => Msg
                            }
                    end,
                    hb_util:lower_case_keys(
                        hb_ao:normalize_keys(hb_private:reset(Outbox)),
                        Opts
                    ),
                    Opts
                ),
            {ok, maps:merge(Downstream, AdditionalRes#{
                <<"slot">> => Slot,
                <<"process">> => ID
            })};
        {Err, Error} when Err == error; Err == failure ->
            ?event(push, {push_failed_to_find_outbox, {error, Error}}, Opts),
            {error, Error}
    end.


%% @doc If the outbox message has a path we interpret it as a request to perform
%% AO-Core eval and schedule the result. Additionally, we  remove the `target` 
%% from the base message before execution and re-add it to the result, such that
%% the target to schedule the execution result upon is not confused with
%% functional components of the evaluation.
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

%% @doc Push a downstream message result. The `Origin' map contains information
%% about the origin of the message: The process that originated the message,
%% the slot number from which it was sent, and the outbox key of the message,
%% and the depth to which downstream results should be included in the message.
push_result_message(TargetProcess, MsgToPush, Origin, Opts) ->
    NormMsgToPush = hb_ao:normalize_keys(MsgToPush, Opts),
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
                    case push_downstream(TargetID, NextSlotOnProc, Origin, Opts) of
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

%% @doc Push a downstream resultant message that has already been scheduled.
%% We determine whether to push the message locally or remotely based on the
%% `push_route_downstream' option.
push_downstream(TargetID, NextSlotOnProc, Origin, Opts) ->
    case hb_opts:get(push_route_downstream, true, Opts) of
        true -> push_downstream_remote(TargetID, NextSlotOnProc, Origin, Opts);
        false -> push_downstream_local(TargetID, NextSlotOnProc, Origin, Opts)
    end.

%% @doc Push a downstream message on a remote node if a route can be found to
%% perform the action. If no route is found, we execute the action locally.
push_downstream_remote(TargetID, NextSlotOnProc, Origin, RawOpts) ->
    Path = <<"/",TargetID/binary, "/push&slot=", (hb_util:bin(NextSlotOnProc))/binary>>,
    RouteReq =
        #{
            <<"path">> => <<"route">>,
            <<"route-path">> => Path
        },
    Opts =
        case dev_whois:ensure_host(RawOpts) of
            {ok, NewOpts} -> NewOpts;
            _ -> RawOpts
        end,
    Self = hb_opts:get(host, host_not_specified, Opts),
    ?event(remote_push,
        {push_downstream_remote,
            {target, TargetID},
            {slot, NextSlotOnProc},
            {origin, Origin},
            {opts, Opts}
        }
    ),
    case hb_ao:resolve(#{ <<"device">> => <<"router@1.0">> }, RouteReq, Opts) of
        {error, no_matches} ->
            ?event(push,
                {no_push_route_found,
                    {target, TargetID},
                    {slot, NextSlotOnProc},
                    {continuing, locally}
                },
                Opts
            ),
            push_downstream_local(TargetID, NextSlotOnProc, Origin, Opts);
        {ok, Self} ->
            % If we matched ourselves as the route, we can just push locally.
            ?event(push,
                {routing_matched_self,
                    {target, TargetID},
                    {slot, NextSlotOnProc},
                    {continuing, locally}
                },
                Opts
            ),
            push_downstream_local(TargetID, NextSlotOnProc, Origin, Opts);
        {ok, Node} ->
            ?event(push,
                {routing_matched_remote,
                    {target, TargetID},
                    {slot, NextSlotOnProc},
                    {node, Node}
                },
                Opts
            ),
            hb_http:post(Node, Path, Opts)
    end.

%% @doc Push a resulting message recursively, executing the action on this node.
push_downstream_local(TargetID, NextSlotOnProc, Origin, Opts) ->
    ?event(push,
        {push_downstream_local,
            {target, TargetID},
            {slot, NextSlotOnProc},
            {origin, Origin}
        }
    ),
    % Push the message downstream. We decrease the result-depth.
    hb_ao:resolve(
        {as, <<"process@1.0">>, TargetID},
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
    ).

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

%% @doc Calculate the base ID for a process. The base ID is not just the 
%% uncommitted process ID. It also excludes the `authority' and `scheduler'
%% keys.
calculate_base_id(GivenProcess, Opts) ->
    Process =
        case hb_ao:get(<<"process">>, GivenProcess, Opts#{ hashpath => ignore }) of
            not_found -> GivenProcess;
            Proc -> Proc
        end,
    BaseProcess =
        hb_ao:set(
            Process,
            #{ <<"authority">> => unset, <<"scheduler">> => unset },
            Opts#{ hashpath => ignore }
        ),
    {ok, BaseID} =
        hb_ao:resolve(
            BaseProcess,
            #{ <<"path">> => <<"id">>, <<"committers">> => <<"none">> },
            Opts
        ),
    ?event(debug_base, {push_generated_base, {id, BaseID}, {base, BaseProcess}}),
    BaseID.

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
    AugmentedMsg = augment_message(Origin, MsgToPush, Opts),
    ?event(push, {prepared_msg, {msg, AugmentedMsg}}, Opts),
    % Load the `accept-id`'d wallet into the `Opts` map, if requested.
    SignedMsg = apply_security(AugmentedMsg, TargetProcess, Codec, Opts),
    ScheduleReq = #{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => SignedMsg
    },
    ?event(push, {schedule_req, {req, ScheduleReq}}, Opts),
    ?event(debug,
        {push_scheduling_result,
            {signed_req, SignedMsg},
            {verifies, hb_message:verify(SignedMsg, signers, Opts)}
        }
    ),
    {ErlStatus, Res} =
        case hb_message:signers(SignedMsg, Opts) of
            [] ->
                {error,
                    <<
                        "Application of security policy failed: ",
                        "No identities matching authority were found."
                    >>
                };
            _Committers ->
                hb_ao:resolve(
                    {as, <<"process@1.0">>, TargetProcess},
                    ScheduleReq,
                    Opts#{ cache_control => <<"always">> }
                )
        end,
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

%% @doc Apply the recipient's security policy to the message. Observes the 
%% following parameters in order to calculate the appropriate security policy:
%% - `policy': A message that generates a security policy message.
%% - `authority': A single committer, or list of comma separated committers.
%% - (Default: Signs with default wallet)
apply_security(Msg, TargetProcess, Codec, Opts) ->
    apply_security(policy, Msg, TargetProcess, Codec, Opts).
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
apply_security(authority, Msg, TargetProcess, Codec, Opts) ->
    case hb_ao:get(<<"authority">>, TargetProcess, Opts) of
        not_found -> apply_security(default, Msg, TargetProcess, Codec, Opts);
    	Authorities when is_list(Authorities) ->
            % The `authority` key has already been parsed into a list of
            % committers. Sign with all local valid keys.
            commit_result(Msg, Authorities, Codec, Opts);
        Authority ->
            % Parse the authority string into a list of committers. Sign with
            % all local valid keys.
            ?event(push, {found_authority, {authority, Authority}}, Opts),
            commit_result(
                Msg,
                hb_util:binary_to_strings(Authority),
                Codec,
                Opts
            )
    end;
apply_security(default, Msg, TargetProcess, Codec, Opts) ->
    ?event(push, {default_policy, {target, TargetProcess}}, Opts),
    commit_result(
        Msg,
        [hb_util:human_id(hb_opts:get(priv_wallet, no_viable_wallet, Opts))],
        Codec,
        Opts
    ).

% @doc Attempt to sign a result message with the given committers.
commit_result(Msg, [], Codec, Opts) ->
    case hb_opts:get(push_always_sign, true, Opts) of
        true -> hb_message:commit(hb_message:uncommitted(Msg), Opts, Codec);
        false -> Msg
    end;
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
    {Node, RedirectPath} = parse_redirect(Location, Opts),
    Path =
        case find_type(SignedReq, Opts) of
            <<"Process">> -> <<"/schedule">>;
            <<"Message">> -> RedirectPath
        end,
    % Store a copy of the message for ourselves.
    {ok, _} = hb_cache:write(SignedReq, Opts),
    ?event(push, {remote_schedule_result, {path, Path}}, Opts),
    case hb_http:post(Node, Path, hb_maps:without([<<"path">>], SignedReq, Opts), Opts) of
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

parse_redirect(Location, Opts) ->
    Parsed = uri_string:parse(Location),
    Node =
        uri_string:recompose(
            (hb_maps:remove(query, Parsed, Opts))#{
                path => <<"/schedule">>
            }
        ),
    {Node, hb_maps:get(path, Parsed, undefined, Opts)}.

%%% Tests

full_push_test_() ->
    {timeout, 30, fun() ->
        dev_process_test_vectors:init(),
        Opts = #{
            process_async_cache => false,
            priv_wallet => hb:wallet(),
            cache_control => <<"always">>
        },
        Base = dev_process_test_vectors:aos_process(Opts),
        hb_cache:write(Base, Opts),
        {ok, SchedInit} =
            hb_ao:resolve(Base, #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Base
            },
            Opts
        ),
        ?event({test_setup, {base, Base}, {sched_init, SchedInit}}),
        Script = ping_pong_script(2),
        ?event({script, Script}),
        {ok, Req} = dev_process_test_vectors:schedule_aos_call(Base, Script, Opts),
        ?event({msg_sched_result, Req}),
        {ok, StartingMsgSlot} =
            hb_ao:resolve(Req, #{ <<"path">> => <<"slot">> }, Opts),
        ?event({starting_msg_slot, StartingMsgSlot}),
        Res =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => StartingMsgSlot
            },
        {ok, _} = hb_ao:resolve(Base, Res, Opts),
        ?assertEqual(
            {ok, <<"Done.">>},
            hb_ao:resolve(Base, <<"now/results/data">>, Opts)
        )
    end}.

push_as_identity_test_() ->
    {timeout, 90, fun() ->
        dev_process_test_vectors:init(),
        % Create a new identity for the scheduler.
        DefaultWallet = hb:wallet(),
        SchedulingWallet = ar_wallet:new(),
        SchedulingID = hb_util:human_id(SchedulingWallet),
        ComputeWallet = ar_wallet:new(),
        ComputeID = hb_util:human_id(ComputeWallet),
        Opts = #{
            priv_wallet => DefaultWallet,
            cache_control => <<"always">>,
            identities => #{
                SchedulingID => #{
                    priv_wallet => SchedulingWallet,
                    store => [hb_test_utils:test_store()]
                },
                ComputeID => #{
                    priv_wallet => ComputeWallet
                }
            }
        },
        % Create a new test AOS process, which will use the given identities as
        % its authority and scheduler.
        Base =
            dev_process_test_vectors:aos_process(
                Opts#{
                    authority => ComputeID,
                    scheduler => [SchedulingID, ComputeID]
                }
            ),
        ?event({base, Base}),
        % Perform the remainder of the test as with `full_push_test_/0'.
        hb_cache:write(Base, Opts),
        {ok, SchedInit} =
            hb_ao:resolve(Base, #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Base
            },
            Opts
        ),
        ?event({test_setup, {base, Base}, {sched_init, SchedInit}}),
        Script = ping_pong_script(2),
        ?event({script, Script}),
        {ok, Req} = dev_process_test_vectors:schedule_aos_call(Base, Script),
        ?event(push, {msg_sched_result, Req}),
        {ok, StartingMsgSlot} =
            hb_ao:resolve(Req, #{ <<"path">> => <<"slot">> }, Opts),
        ?event({starting_msg_slot, StartingMsgSlot}),
        Res =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => StartingMsgSlot
            },
        {ok, _} = hb_ao:resolve(Base, Res, Opts),
        ?assertEqual(
            {ok, <<"Done.">>},
            hb_ao:resolve(Base, <<"now/results/data">>, Opts)
        ),
        % Validate that the scheduler's wallet was used to sign the message.
        Assignment =
            hb_ao:get(
                <<"schedule/assignments/2">>,
                Base,
                Opts
            ),
        Committers = hb_ao:get(
            <<"committers">>,
            hb_cache:read_all_commitments(Assignment, Opts),
            Opts
        ),
        ?assert(lists:member(SchedulingID, Committers)),
        ?assert(lists:member(ComputeID, Committers)),
        % Validate that the compute wallet was used to sign the message.
        ?assertEqual(
            [ComputeID],
            hb_ao:get(<<"schedule/assignments/2/body/committers">>, Base, Opts)
        )
    end}.

multi_process_push_test_() ->
    {timeout, 30, fun() ->
        dev_process_test_vectors:init(),
        Opts = #{
            priv_wallet => hb:wallet(),
            cache_control => <<"always">>
        },
        Proc1 = dev_process_test_vectors:aos_process(Opts),
        hb_cache:write(Proc1, Opts),
        {ok, _SchedInit1} =
            hb_ao:resolve(Proc1, #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Proc1
            },
            Opts
        ),
        {ok, _} = dev_process_test_vectors:schedule_aos_call(Proc1, reply_script()),
        Proc2 = dev_process_test_vectors:aos_process(Opts),
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
        {ok, ToPush} = dev_process_test_vectors:schedule_aos_call(
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
        Res =
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => SlotToPush,
                <<"result-depth">> => 1
            },
        {ok, PushResult} = hb_ao:resolve(Proc2, Res, Opts),
        ?event(push, {push_result_proc2, PushResult}),
        AfterPush = hb_ao:resolve(Proc2, <<"now/results/data">>, Opts),
        ?event(push, {after_push, AfterPush}),
        ?assertEqual({ok, <<"GOT PONG">>}, AfterPush)
    end}.

push_with_redirect_hint_test_disabled() ->
    {timeout, 30, fun() ->
        dev_process_test_vectors:init(),
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
        Client = dev_process_test_vectors:aos_process(),
        PongServer = dev_process_test_vectors:aos_process(ExtOpts),
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
                dev_process_lib:ensure_process_key(PongServer, LocalOpts),
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
            dev_process_test_vectors:schedule_aos_call(
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
        Res = #{ <<"path">> => <<"push">>, <<"slot">> => SlotToPush },
        {ok, PushResult} = hb_ao:resolve(Client, Res, LocalOpts),
        ?event(push, {push_result_client, PushResult}),
        AfterPush = hb_ao:resolve(Client, <<"now/results/data">>, LocalOpts),
        ?event(push, {after_push, AfterPush}),
        % Note: This test currently only gets a reply that the message was not
        % trusted by the process. To fix this, we would have to add another 
        % trusted authority to the `test_aos_process' call. For now, this is 
        % enough to validate that redirects are pushed through correctly.
        ?assertEqual({ok, <<"GOT PONG">>}, AfterPush)
    end}.

push_prompts_encoding_change_test_() ->
    {timeout, 30, fun push_prompts_encoding_change/0}.
push_prompts_encoding_change() ->
    dev_process_test_vectors:init(),
    Opts = #{
        priv_wallet => hb:wallet(),
        cache_control => <<"always">>,
        store =>
            [
                #{ <<"store-module">> => hb_store_fs, <<"name">> => <<"cache-TEST">> },
                % Include a gateway store so that we can get the legacynet 
                % process when needed.
                #{ <<"store-module">> => hb_store_gateway,
                    <<"store">> => #{
                        <<"store-module">> => hb_store_fs,
                        <<"name">> => <<"cache-TEST">>
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
    ?event(push, {base, Msg}),
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

remote_routed_push_test_() ->
    {timeout, 60, fun remote_routed_push/0}.
remote_routed_push() ->
    % Creates a network of nodes and processes with the following structure:
    % Node 1:
    %   - Schedules for process 1.
    %   - Routes requests for process 2 to Node 2.
    % Node 2:
    %   - Schedules for process 2.
    %
    % Process 1:
    %   - Has an `owner` of Node 1's wallet.
    %   - Has both node 1 and node 2 as authorities.
    %   - Pushes a `pong` message to process 2 on recipient of an `action: ping`
    %     message.
    % 
    % Process 2:
    %   - Has an `owner` of Node 2's wallet.
    %   - Has both node 1 and node 2 as authorities.
    %   - Pushes a `pong` message to process 1 on recipient of a message.
    % 
    % After establishing the network, we ensure that a message can be correctly
    % pushed from user to process 1, to process 2, then back to process 1.
    % 
    % We start by generating the isolated wallets and stores for each node.
    N1Wallet = ar_wallet:new(),
    N1Store = [hb_test_utils:test_store()],
    N2Wallet = ar_wallet:new(),
    N2Store = [hb_test_utils:test_store()],
    % Next, create the second node and process. We do this before node 1 such 
    % that the routes of node 1 and the target of process 1's message are known
    % when we create them.
    N2Opts =
        #{
            store => N2Store,
            priv_wallet => N2Wallet
        },
    N2 = hb_http_server:start_node(N2Opts),
    % Create the second process on the second node.
    Proc2 = dev_process_test_vectors:aos_process(N2Opts),
    LoadedProc2 = hb_cache:ensure_all_loaded(Proc2, N2Opts),
    Proc2ID = hb_message:id(Proc2, signed, N2Opts),
    % Next, create the first node and process.
    N1Opts =
        #{
            store => N1Store,
            priv_wallet => N1Wallet,
            routes =>
                [
                    #{
                        <<"template">> => <<Proc2ID/binary, ".*">>,
                        <<"node">> => N2
                    }
                ]
        },
    N1 = hb_http_server:start_node(N1Opts),
    % Sanity check that routing resolves the Proc2ID path to N2 on the first node.
    ?assertMatch(
        {ok, N2},
        hb_http:get(
            N1,
            <<"/~router@1.0/route?route-path=", Proc2ID/binary, "/push&slot=1">>,
            N1Opts
        )
    ),
    % Create the first process on the first node.
    Proc1 = dev_process_test_vectors:aos_process(N1Opts),
    LoadedProc1 = hb_cache:ensure_all_loaded(Proc1, N1Opts),
    Proc1ID = hb_message:id(LoadedProc1, all, N1Opts),
    % Write both processes to each of the nodes' caches, such that both are
    % 'globally' available to each other.
    hb_cache:write(LoadedProc1, N1Opts),
    hb_cache:write(LoadedProc1, N2Opts),
    hb_cache:write(LoadedProc2, N1Opts),
    hb_cache:write(LoadedProc2, N2Opts),
    ?event(debug_test,
        {network_setup, 
            {proc1ID, Proc1ID},
            {proc2ID, Proc2ID},
            {n1, N1},
            {n2, N2},
            {wallet1, ar_wallet:to_address(N1Wallet)},
            {wallet2, ar_wallet:to_address(N2Wallet)}
        }
    ),
    % Set the authorities of the processes to include both wallets.
    SetAuthoritiesCommand =
        <<
            "ao.authorities = { ",
                "\"", (hb_util:human_id(N1Wallet))/binary, "\",",
                "\"", (hb_util:human_id(N2Wallet))/binary, "\"",
            " }; ",
            "ao.addAssignable('foobar', function (msg) return true end); "
            "ao.isAssignable = function(m) return true end"
        >>,
    {ok, SetAuthProc1} =
        dev_process_test_vectors:schedule_aos_call(LoadedProc1, SetAuthoritiesCommand, N1Opts),
    {ok, SetAuthProc2} =
        dev_process_test_vectors:schedule_aos_call(LoadedProc2, SetAuthoritiesCommand, N2Opts),
    ?event(debug_test,
        {set_authorities, 
            {command, {string, SetAuthoritiesCommand}},
            {proc1_result, SetAuthProc1},
            {proc2_result, SetAuthProc2}
        }
    ),
    % Load the scripts into each process. The second process has the base
    % reply script, and the first process has reply script with a trigger to
    % send a message to the second process.
    {ok, P2ScriptLoadRes} =
        dev_process_test_vectors:schedule_aos_call(
            LoadedProc2,
            reply_script(),
            N2Opts
        ),
    {ok, P1ScriptLoadRes} =
        dev_process_test_vectors:schedule_aos_call(
            LoadedProc1,
            reply_script(Proc2ID),
            N1Opts
        ),
    ?event(debug_test,
        {script_load, 
            {proc2_result, P2ScriptLoadRes},
            {proc1_result, P1ScriptLoadRes}
        }
    ),
    % Get the slot of the message to push on process 1.
    SlotP1 = hb_ao:get(<<"slot">>, P1ScriptLoadRes, N1Opts),
    ?event(debug_test, {slot_p1, SlotP1}),
    PushRes =
        hb_http:post(
            N1,
            #{ 
                <<"path">> => <<Proc1ID/binary, "/push">>,
                <<"slot">> => SlotP1
            },
            N1Opts
        ),
    ?event(debug_test, {push_res, PushRes}),
    {ok, SchedResP1} = hb_ao:resolve(LoadedProc1, <<"schedule">>, N1Opts),
    ?event(debug_test, {sched_res_p1, SchedResP1}),
    {ok, SchedResP2} = hb_ao:resolve(LoadedProc2, <<"schedule">>, N2Opts),
    ?event(debug_test, {sched_res_p2, SchedResP2}),
    ?assertEqual(
        {error, not_found},
        hb_ao:resolve_many(
            [
                LoadedProc2,
                #{ <<"path">> => <<"compute">>, <<"init">> => <<"stop">> }
            ],
            N1Opts
        )
    ),
    ?assertMatch(
        {ok, Slot} when Slot > 0,
        hb_ao:resolve(LoadedProc2, <<"now/at-slot">>, N2Opts)
    ).

oracle_push_test_() -> {timeout, 30, fun oracle_push/0}.
oracle_push() ->
    dev_process_test_vectors:init(),
    Client = dev_process_test_vectors:aos_process(),
    {ok, _} = hb_cache:write(Client, #{}),
    {ok, _} = dev_process_test_vectors:schedule_aos_call(Client, oracle_script()),
    Res =
        #{
            <<"path">> => <<"push">>,
            <<"slot">> => 0
        },
    {ok, PushResult} = hb_ao:resolve(Client, Res, #{ priv_wallet => hb:wallet() }),
    ?event({result, PushResult}),
    ComputeRes =
        hb_ao:resolve(
            Client,
            <<"now/results/data">>,
            #{ priv_wallet => hb:wallet() }
        ),
    ?event({compute_res, ComputeRes}),
    ?assertMatch({ok, _}, ComputeRes).

-ifdef(ENABLE_GENESIS_WASM).
%% @doc Test that a message that generates another message which resides on an
%% ANS-104 scheduler leads to `~push@1.0` re-signing the message correctly.
%% Requires `ENABLE_GENESIS_WASM' to be enabled.
nested_push_prompts_encoding_change_test_() ->
    {timeout, 30, fun nested_push_prompts_encoding_change/0}.
nested_push_prompts_encoding_change() ->
    dev_process_test_vectors:init(),
    Opts = #{
        priv_wallet => hb:wallet(),
        cache_control => <<"always">>,
        store => hb_opts:get(store)
    },
    ?event(push_debug, {opts, Opts}),
    Base = dev_process_test_vectors:aos_process(Opts),
    hb_cache:write(Base, Opts),
    {ok, SchedInit} =
        hb_ao:resolve(Base, #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>,
            <<"body">> => Base
        },
        Opts
    ),
    ?event({test_setup, {base, Base}, {sched_init, SchedInit}}),
    Script = message_to_legacynet_scheduler_script(),
    ?event({script, Script}),
    {ok, Req} = dev_process_test_vectors:schedule_aos_call(Base, Script),
    ?event(push, {msg_sched_result, Req}),
    {ok, StartingMsgSlot} =
        hb_ao:resolve(Req, #{ <<"path">> => <<"slot">> }, Opts),
    ?event({starting_msg_slot, StartingMsgSlot}),
    Req2 =
        #{
            <<"path">> => <<"push">>,
            <<"slot">> => StartingMsgSlot
        },
    {ok, Res} = hb_ao:resolve(Base, Req2, Opts),
    ?event(push, {res, Res}),
    Msg = hb_message:commit(#{
        <<"path">> => <<"push">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:commit(
                #{
                    <<"target">> => hb_message:id(Base, all, Opts),
                    <<"action">> => <<"Ping">>
                },
                Opts
            )
    }, Opts),
    ?event(push, {base, Msg}),
    Res2 =
        hb_ao:resolve_many(
            [
                hb_message:id(Base, all, Opts),
                {as, <<"process@1.0">>, <<>>},
                Msg
            ],
            Opts
        ),
    ?assertMatch({ok, #{ <<"1">> := #{ <<"resulted-in">> := _ }}}, Res2).
-endif.
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
reply_script(OtherProcessID) ->
    <<
        (reply_script())/binary, "\n",
        "Send({ Target = \"", (OtherProcessID)/binary, "\", Action = \"Ping\" })\n"
    >>.

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
    >>.