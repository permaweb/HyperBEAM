%%% @doc This module is the root of the device call logic of the 
%%% AO-Core protocol in HyperBEAM.
%%% 
%%% At the implementation level, every message is simply a collection of keys,
%%% dictated by its `Device', that can be resolved in order to yield their
%%% values. Each key may contain a link to another message or a raw value:
%%% 
%%% 	`ao(BaseMessage, RequestMessage) -> {Status, Result}'
%%% 
%%% Under-the-hood, `AO-Core(BaseMessage, RequestMessage)' leads to a lookup of
%%% the `device' key of the base message, followed by the evaluation of
%%% `DeviceMod:PathPart(BaseMessage, RequestMessage)', which defines the user 
%%% compute to be performed. If `BaseMessage' does not specify a device, 
%%% `~message@1.0' is assumed. The key to resolve is specified by the `path' 
%%% field of the message.
%%% 
%%% After each output, the `HashPath' is updated to include the `RequestMessage'
%%% that was executed upon it.
%%% 
%%% Because each message implies a device that can resolve its keys, as well
%%% as generating a merkle tree of the computation that led to the result,
%%% you can see the AO-Core protocol as a system for cryptographically chaining 
%%% the execution of `combinators'. See `docs/ao-core-protocol.md' for more 
%%% information about AO-Core.
%%% 
%%% The `key(BaseMessage, RequestMessage)' pattern is repeated throughout the 
%%% HyperBEAM codebase, sometimes with `BaseMessage' replaced with `Base', `M1'
%%% or similar, and `RequestMessage' replaced with `Req', `M2', etc.
%%% 
%%% The result of any computation can be either a new message or a raw literal 
%%% value (a binary, integer, float, atom, or list of such values).
%%% 
%%% Devices can be expressed as either modules or maps. They can also be 
%%% referenced by an Arweave ID, which can be used to load a device from 
%%% the network when `trusted-device-signers' are configured.
%%% 
%%% HyperBEAM device implementations are defined as follows:
%%% <pre>
%%%     DevMod:ExportedFunc : Key resolution functions. All are assumed to be
%%%                           device keys (thus, present in every message that
%%%                           uses it) unless specified by `DevMod:info()'.
%%%                           Each function takes a set of parameters
%%%                           of the form `DevMod:KeyHandler(Base, Req, Opts)'.
%%%                           Each of these arguments can be ommitted if not
%%%                           needed. Non-exported functions are not assumed
%%%                           to be device keys.
%%%
%%%     DevMod:info : Optional. Returns a map of options for the device. All 
%%%                   options are optional and assumed to be the defaults if 
%%%                   not specified. This function can accept a `Base' as 
%%%                   an argument, allowing it to specify its functionality 
%%%                   based on a specific message if appropriate.
%%% 
%%%     info/exports : Overrides the export list of the Erlang module, such that
%%%                   only the functions in this list are assumed to be device
%%%                   keys. Defaults to all of the functions that DevMod 
%%%                   exports in the Erlang environment.
%%%
%%%     info/excludes : A list of keys that should not be resolved by the device,
%%%                     despite being present in the Erlang module exports list.
%%% 
%%%     info/handler : A function that should be used to handle _all_ keys for 
%%%                    messages using the device.
%%% 
%%%     info/default : A function that should be used to handle all keys that
%%%                    are not explicitly implemented by the device. Defaults to
%%%                    the `dev_message' device, which contains general keys for 
%%%                    interacting with messages.
%%% 
%%%     info/default_mod : A different device module that should be used to
%%%                    handle all keys that are not explicitly implemented
%%%                    by the device. Defaults to the `dev_message' device.
%%% 
%%%     info/grouper : A function that returns the concurrency 'group' name for
%%%                    an execution. Executions with the same group name will
%%%                    be executed by sending a message to the associated process
%%%                    and waiting for a response. This allows you to control 
%%%                    concurrency of execution and to allow executions to share
%%%                    in-memory state as applicable. Default: A derivation of
%%%                    Base+Req. This means that concurrent calls for the same
%%%                    output will lead to only a single execution.
%%% 
%%%     info/worker : A function that should be run as the 'server' loop of
%%%                   the executor for interactions using the device.
%%% 
%%% The HyperBEAM resolver also takes a number of runtime options that change
%%% the way that the environment operates:
%%% 
%%% `update_hashpath':  Whether to add the `Req' to `HashPath' for the `Res'.
%%% 					Default: true.
%%% `add_key':          Whether to add the key to the start of the arguments.
%%% 					Default: `<not set>'.
%%% </pre>
-module(hb_ao).
%%% Main AO-Core API:
-export([resolve/2, resolve/3]).
-export([raw/3, raw/4, raw/5]).
-export([normalize_key/1, normalize_key/2, normalize_keys/1, normalize_keys/2]).
%%% Shortcuts and tools:
-export([keys/2]).
-export([get/3, get/4, get_first/2, get_first/3]).
-export([set/3, set/4, deep_set/3, remove/3]).
%%% Exports for tests in hb_ao_test_vectors.erl:
-include("include/hb.hrl").

-define(
    TEMP_OPTS,
    [
        <<"add-key">>,
        <<"cache-control">>,
        <<"spawn-worker">>,
        <<"only">>,
        <<"prefer">>
    ]
).

%% @doc Get the value of an AO-Core message's key by running its associated device
%% function. Optionally, takes options that control the runtime environment. 
%% This function returns the raw result of the device function call:
%% `{ok | error, NewMessage}.'
%% The resolver is composed of a series of discrete phases:
%%      1: Loading IDs/links.
%%      2: Device or direct key lookup.
%%      3: Vary `Base` and `Request`.
%%      4: Cache lookup
%%      5: Persistent resolver lookup.
%%      6: Execution.
%%      7: Execution of the `step' hook.
%%      8: Result caching.
%%      9: Extension of result upon original base.
%%     10: Notify waiters.
%%     11: Fork worker.
%%     12: Recurse or terminate.
resolve(MsgSeq, Opts) when is_list(MsgSeq) ->
    resolve_many(MsgSeq, Opts);
resolve(Path, Opts) when is_binary(Path) ->
    resolve(#{ <<"path">> => Path }, Opts);
resolve(SingleResult, _Opts)
        when is_map(SingleResult), not is_map_key(<<"path">>, SingleResult) ->
    % Nothing to resolve; return as-is.
    {ok, SingleResult};
resolve(SingletonMsg, Opts) ->
    resolve_many(hb_singleton:from(SingletonMsg, Opts), Opts).

%% @doc Resolve a single AO-Core base and request pair, If the `path` in the
%% request has multiple parts, each is executed in sequence, in the context of
%% the other request keys, over the original base message.
resolve(Base, Path, Opts) when is_binary(Path) ->
    resolve(Base, #{ <<"path">> => Path }, Opts);
resolve(Base, Req, Opts) ->
    MessagesToExec =
        case hb_path:from_message(request, Req, Opts) of
            undefined -> [Req];
            PathParts ->
                [ #{ <<"path">> => Path, <<"...">> => Req } || Path <- PathParts ]
        end,
    ?event_debug(debug_ao_core,
        {stage,
            1,
            prepare_multimessage_resolution,
            {messages_to_exec, MessagesToExec}
        }
    ),
    resolve_many([Base | MessagesToExec], Opts).

%% @doc Invoke only the raw execution of the AO-Core resolution flow, ignoring
%% normalization, cache, hashpath, worker, and other management components.
%% This function comes in `/3`-`/5` variants, allowing the caller to optionally
%% specify that a specific device must be used for the execution (as if
%% `Base/device: Device` was set), or to force a specific key to be used.
%% Critically, the modifiers do _not_ affect the message that the call is
%% executed upon: `Base`, `Req`, and `Opts` are passed directly to the execution
%% function as-is, but the chosen function is altered by these modifiers.
raw(Base, Req, Opts) ->
    raw(undefined, Base, Req, Opts).
raw(Device, Base, Req, Opts) ->
    raw(Device, undefined, Base, Req, Opts).
raw(ForcedDevice, ForcedKey, Base, Req, Opts) ->
    ExecOpts = execution_opts(Opts),
    % If a forced key is provided, use it; otherwise, extract from the request.
    Key =
        if ForcedKey =/= undefined -> ForcedKey;
        true -> hb_path:hd(Req, ExecOpts)
        end,
    % If an explicit device is provided we use it _only on the lookup_ -- not
    % during execution.
    BaseWithDevice =
        case ForcedDevice of
            undefined -> Base;
            _ when is_map(Base) ->
                #{ <<"device">> => ForcedDevice, <<"...">> => Base };
            _ ->
                #{ <<"device">> => ForcedDevice }
        end,
    {ExecFun, PrefixArgs} =
        case hb_device:message_to_fun(BaseWithDevice, Key, ExecOpts) of
            {add_key, _DevMod, Fun} -> {Fun, [Key]};
            {_Status, _DevMod, Fun} -> {Fun, []}
        end,
    % Apply the function and return the result directly, without any further
    % processing. We add the `PrefixArgs` to the list of arguments to be passed
    % to the function to accomodate default handlers, which take the key that
    % was invoked on the device as the first argument (ahead of `Base`, `Req`,
    % and `ExecOpts`).
    apply(
        ExecFun,
        hb_device:truncate_args(ExecFun, PrefixArgs ++ [Base, Req, ExecOpts])
    ).

%% @doc Resolve a list of messages in sequence. Take the output of the first
%% message as the input for the next message. Once the last message is resolved,
%% return the result.
%% A `resolve_many' call with only a single ID will attempt to read the message
%% directly from the store. No execution is performed.
resolve_many([ID], Opts) when ?IS_ID(ID) ->
    % Note: This case is necessary to place specifically here for two reasons:
    % 1. It is not in `do_resolve_many' because we need to handle the case
    %    where a result from a prior invocation is an ID itself. We should not
    %    attempt to resolve such IDs further.
    % 2. The main AO-Core logic looks for linkages between message input
    %    pairs and outputs. With only a single ID, there is not a valid pairing
    %    to use in looking up a cached result.
    ?event_debug(debug_ao_core, {stage, na, resolve_directly_to_id, ID}, Opts),
    hb_cache:read(ID, Opts);
resolve_many(ListMsg, Opts) when is_map(ListMsg) ->
    % We have been given a message rather than a list of messages, so we should
    % convert it to a list, assuming that the message is monotonically numbered.
    ListOfMessages =
        try hb_util:message_to_ordered_list(ListMsg, internal_opts(Opts))
        catch
          Type:Exception:Stacktrace ->
            throw(
                {resolve_many_error,
                    {given_message_not_ordered_list, ListMsg},
                    {type, Type},
                    {exception, Exception},
                    {stacktrace, Stacktrace}
                }
            )
        end,
    resolve_many(ListOfMessages, Opts);
resolve_many(MsgList, Opts) ->
    ?event_debug(debug_ao_core, {resolve_many, MsgList}, Opts),
    Res = do_resolve_many(MsgList, Opts),
    ?event_debug(debug_ao_core, {resolve_many_complete, {res, Res}, {reqs, MsgList}}, Opts),
    Res.
do_resolve_many([], _Opts) ->
    {failure, <<"Attempted to resolve an empty message sequence.">>};
do_resolve_many([Res], Opts) ->
    ?event_debug(debug_ao_core, {stage, 11, resolve_complete, Res}),
    hb_cache:ensure_loaded(Res, Opts);
do_resolve_many([Base, Req | MsgList], Opts) ->
    ?event_debug(debug_ao_core, {stage, 0, resolve_many, {base, Base}, {req, Req}}),
    case resolve_stage(1, Base, Req, Opts) of
        {ok, Res} ->
            ?event_debug(debug_ao_core,
                {
                    stage,
                    13,
                    resolved_step,
                    {res, Res},
                    {opts, Opts}
                },
				Opts
            ),
            do_resolve_many([Res | MsgList], Opts);
        Res ->
            % The result is not a resolvable message. Return it.
            ?event_debug(debug_ao_core, {stage, 13, resolve_many_terminating_early, Res}),
            Res
    end.

resolve_stage(1, Path, Req, Opts) when is_binary(Path) ->
    % The base has been granted to us as a binary. Execute it in and resurse.
    case resolve(Path, Opts) of
        {ok, ResolvedBase} -> resolve_stage(1, ResolvedBase, Req, Opts);
        Other -> Other
    end;
resolve_stage(1, Link, Req, Opts) when ?IS_LINK(Link) ->
    % If the first message is a link, we should load the message and
    % continue with the resolution.
    ?event_debug(debug_ao_core, {stage, 1, resolve_base_link, {link, Link}}, Opts),
    resolve_stage(1, hb_cache:ensure_loaded(Link, Opts), Req, Opts);
resolve_stage(1, Base, Link, Opts) when ?IS_LINK(Link) ->
    % If the second message is a link, we should load the message and
    % continue with the resolution.
    ?event_debug(debug_ao_core, {stage, 1, resolve_req_link, {link, Link}}, Opts),
    resolve_stage(1, Base, hb_cache:ensure_loaded(Link, Opts), Opts);
resolve_stage(1, Base, Req, Opts) when is_list(Base) ->
    % Normalize lists to numbered maps (base=1) if necessary.
    ?event_debug(debug_ao_core, {stage, 1, list_normalize}, Opts),
    resolve_stage(1,
        normalize_keys(Base, Opts),
        Req,
        Opts
    );
resolve_stage(1, Base, Req, Opts) ->
    ?event_debug(debug_ao_core, {stage, 1, normalize_complete}, Opts),
    resolve_stage(2, Base, Req, Opts);
resolve_stage(2, Base, Req, Opts) ->
    ?event_debug(debug_ao_core, {stage, 2, cache_lookup}, Opts),
    % Lookup request in the cache. If we find a result, return it.
    % If we do not find a result, we continue to the next stage,
    % unless the cache lookup returns `halt' (the user has requested that we 
    % only return a result if it is already in the cache).
    case hb_cache_control:maybe_lookup(Base, Req, Opts) of
        {ok, Res} ->
            ?event_debug(debug_ao_core, {stage, 2, cache_hit, {res, Res}}, Opts),
            {ok, Res};
        {continue, NewBase, NewReq} ->
            resolve_stage(3, NewBase, NewReq, Opts);
        {error, CacheResp} -> {error, CacheResp}
    end;
resolve_stage(3, Base, Req, _Opts) when not is_map(Base) or not is_map(Req) ->
    % Validation check: If the messages are not maps, we cannot find a key
    % in them, so return not_found.
    ?event_debug(debug_ao_core, {stage, 3, validation_check_type_error}, _Opts),
    {error, not_found};
resolve_stage(3, Base, Req, Opts) ->
    ?event_debug(debug_ao_core, {stage, 3, validation_check}, Opts),
    % Validation checks: If `paranoid_message_verification' is enabled, we should
    % verify the base and request messages prior to execution.
    hb_message:paranoid_verify(
        pre_resolve,
        #{
            <<"reason">> => <<"AO-Core Pre-Execution Validation">>,
            <<"base">> => Base,
            <<"request">> => Req
        },
        Opts
    ),
    resolve_stage(4, Base, Req, Opts);
resolve_stage(4, Base, Req, Opts) ->
    ?event_debug(debug_ao_core, {stage, 4, persistent_resolver_lookup}, Opts),
    % Persistent-resolver lookup: Search for local (or Distributed
    % Erlang cluster) processes that are already performing the execution.
    % Before we search for a live executor, we check if the device specifies 
    % a function that tailors the 'group' name of the execution. For example, 
    % the `dev_process' device 'groups' all calls to the same process onto
    % calls to a single executor. By default, `{Base, Req}' is used as the
    % group name.
    case hb_persistent:find_or_register(Base, Req, hb_maps:without(?TEMP_OPTS, Opts, Opts)) of
        {leader, ExecName} ->
            % We are the leader for this resolution. Continue to the next stage.
            case hb_opts:get(spawn_worker, false, Opts) of
                true -> ?event(worker_spawns, {will_become, ExecName});
                _ -> ok
            end,
            resolve_stage(5, Base, Req, ExecName, Opts);
        {wait, Leader} ->
            % There is another executor of this resolution in-flight.
            % Bail execution, register to receive the response, then
            % wait.
            case hb_persistent:await(Leader, Base, Req, Opts) of
                {error, leader_died} ->
                    ?event(
                        ao_core,
                        {leader_died_during_wait,
                            {leader, Leader},
                            {base, Base},
                            {req, Req},
                            {opts, Opts}
                        },
                        Opts
                    ),
                    % Re-try again if the group leader has died.
                    resolve_stage(4, Base, Req, Opts);
                Res ->
                    % Now that we have the result, we can skip right to potential
                    % recursion (step 11) in the outer-wrapper.
                    Res
            end;
        {infinite_recursion, GroupName} ->
            % We are the leader for this resolution, but we executing the 
            % computation again. This may plausibly be OK in _some_ cases,
            % but in general it is the sign of a bug.
            ?event(
                ao_core,
                {infinite_recursion,
                    {exec_group, GroupName},
                    {base, Base},
                    {req, Req},
                    {opts, Opts}
                },
                Opts
            ),
            case hb_opts:get(<<"allow-infinite">>, false, Opts) of
                true ->
                    % We are OK with infinite loops, so we just continue.
                    resolve_stage(5, Base, Req, GroupName, Opts);
                false ->
                    % We are not OK with infinite loops, so we raise an error.
                    error_infinite(Base, Req, Opts)
            end
    end.
resolve_stage(5, Base, Req, ExecName, Opts) ->
    ?event_debug(debug_ao_core, {stage, 5, device_lookup}, Opts),
    % Device lookup: Find the Erlang function that should be utilized to 
    % execute Req on Base.
	{ResolvedFunc, NewOpts} =
		try
            UserOpts = hb_maps:without(?TEMP_OPTS, Opts, Opts),
			Key = hb_path:hd(Req, UserOpts),
			% Try to load the device and get the function to call.
            ?event(
                {
                    resolving_key,
                    {key, Key},
                    {base, Base},
                    {req, Req},
                    {opts, Opts}
                }
            ),
			{Status, Device, Func} = hb_device:message_to_fun(Base, Key, UserOpts),
			?event(
				{found_func_for_exec,
                    {key, Key},
                    {device, Device},
					{func, Func},
					{base, Base},
					{req, Req},
					{opts, Opts}
				}
			),
			% Next, add an option to the Opts map to indicate if we should
			% add the key to the start of the arguments.
			{
				Func,
				Opts#{
					<<"add-key">> =>
						case Status of
							add_key -> Key;
							_ -> false
						end
				}
			}
		catch
			Class:Exception:Stacktrace ->
                ?event(
                    ao_result,
                    {
                        load_device_failed,
                        {base, Base},
                        {req, Req},
                        {exec_name, ExecName},
                        {exec_class, Class},
                        {exec_exception, Exception},
                        {exec_stacktrace, Stacktrace},
                        {opts, Opts}
                    },
					Opts
                ),
                % If the device cannot be loaded, we alert the caller.
				error_execution(
                    ExecName,
                    Req,
					loading_device,
					{Class, Exception, Stacktrace},
					Opts
				)
		end,
	resolve_stage(6, ResolvedFunc, Base, Req, ExecName, NewOpts).
resolve_stage(6, Func, Base, Req, ExecName, Opts) ->
    ?event_debug(debug_ao_core, {stage, 6, ExecName, execution}, Opts),
	% Execution.
    ExecOpts = execution_opts(Opts),
	Args =
		case hb_opts:get(<<"add-key">>, false, Opts) of
			false -> [Base, Req, ExecOpts];
			Key -> [Key, Base, Req, ExecOpts]
		end,
    % Try to execute the function.
    Res = 
        try
            TruncatedArgs = hb_device:truncate_args(Func, Args),
            MsgRes = maybe_profiled_apply(Func, TruncatedArgs, Base, Req, Opts),
            ?event(
                debug_ao_result,
                {
                    ao_result,
                    {exec_name, ExecName},
                    {base, Base},
                    {req, Req},
                    {res, MsgRes}
                },
                Opts
            ),
            MsgRes
        catch
            ExecClass:ExecException:ExecStacktrace ->
                ?event(
                    ao_core,
                    {device_call_failed, ExecName, {func, Func}},
                    Opts
                ),
                ?event(
                    ao_result,
                    {
                        exec_failed,
                        {base, Base},
                        {req, Req},
                        {exec_name, ExecName},
                        {func, Func},
                        {exec_class, ExecClass},
                        {exec_exception, ExecException},
                        {exec_stacktrace, erlang:process_info(self(), backtrace)},
                        {opts, Opts}
                    },
					Opts
                ),
                % If the function call fails, we raise an error in the manner
                % indicated by caller's `#Opts'.
                error_execution(
                    ExecName,
                    Req,
                    device_call,
                    {ExecClass, ExecException, ExecStacktrace},
                    Opts
                )
        end,
    hb_message:paranoid_verify(
        post_resolve,
        #{
            <<"reason">> => <<"AO-Core Post-Execution Validation">>,
            <<"base">> => Base,
            <<"request">> => Req,
            <<"result">> => Res
        },
        Opts
    ),
    resolve_stage(7, Base, Req, Res, ExecName, Opts);
resolve_stage(
    7,
    Base,
    Req,
    {St, Res},
    ExecName,
    Opts = #{ <<"on">> := On = #{ <<"step">> := _ }}
) ->
    ?event_debug(debug_ao_core, {stage, 7, ExecName, executing_step_hook, On}, Opts),
    % If the `step' hook is defined, we execute it. Note: This function clause
    % matches directly on the `on' key of the `Opts' map. This is in order to
    % remove the expensive lookup check that would otherwise be performed on every
    % execution.
    HookReq = #{
        <<"base">> => Base,
        <<"request">> => Req,
        <<"status">> => St,
        <<"body">> => Res
    },
    case hb_hook:on(<<"step">>, HookReq, Opts) of
        {ok, #{ <<"status">> := NewStatus, <<"body">> := NewRes }} ->
            resolve_stage(8, Base, Req, {NewStatus, NewRes}, ExecName, Opts);
        Error ->
            ?event(
                ao_core,
                {step_hook_error,
                    {error, Error},
                    {hook_req, HookReq}
                },
                Opts
            ),
            Error
    end;
resolve_stage(7, Base, Req, Res, ExecName, Opts) ->
    ?event_debug(debug_ao_core, {stage, 7, ExecName, no_step_hook}, Opts),
    resolve_stage(8, Base, Req, Res, ExecName, Opts);
resolve_stage(8, Base, Req, {ok, {resolve, Sublist}}, ExecName, Opts) ->
    ?event_debug(debug_ao_core, {stage, 8, ExecName, subresolve_result}, Opts),
    % If the result is a `{resolve, Sublist}' tuple, we need to execute it
    % as a sub-resolution.
    resolve_stage(9, Base, Req, resolve_many(Sublist, Opts), ExecName, Opts);
resolve_stage(8, Base, Req, Res, ExecName, Opts) ->
    ?event_debug(debug_ao_core, {stage, 8, ExecName, no_subresolution_necessary}, Opts),
    resolve_stage(9, Base, Req, Res, ExecName, Opts);
resolve_stage(9, Base, Req, {ok, Res}, ExecName, Opts) when is_map(Res) ->
    ?event_debug(debug_ao_core, {stage, 9, ExecName, generate_hashpath}, Opts),
    % Cryptographic linking. Now that we have generated the result, we
    % need to cryptographically link the output to its input via a hashpath.
    resolve_stage(10, Base, Req,
        case hb_opts:get(<<"hashpath">>, update, Opts#{ <<"only">> => local }) of
            update ->
                NormRes = Res,
                Priv = hb_private:from_message(NormRes),
                HP = hb_path:hashpath(Base, Req, Opts),
                if not is_binary(HP) or not is_map(Priv) ->
                    throw({invalid_hashpath, {hp, HP}, {res, NormRes}});
                true ->
                    {ok, NormRes#{ <<"priv">> => Priv#{ <<"hashpath">> => HP } }}
                end;
            reset ->
                Priv = hb_private:from_message(Res),
                {ok, Res#{ <<"priv">> => hb_maps:without([<<"hashpath">>], Priv, Opts) }};
            ignore ->
                Priv = hb_private:from_message(Res),
                if not is_map(Priv) ->
                    throw({invalid_private_message, {res, Res}});
                true ->
                    {ok, Res}
                end
        end,
        ExecName,
        Opts
    );
resolve_stage(9, Base, Req, {Status, Res}, ExecName, Opts) when is_map(Res) ->
    ?event_debug(debug_ao_core, {stage, 9, ExecName, abnormal_status_reset_hashpath}, Opts),
    ?event(hashpath, {resetting_hashpath_res, {base, Base}, {req, Req}, {opts, Opts}}),
    % Skip cryptographic linking and reset the hashpath if the result is abnormal.
    Priv = hb_private:from_message(Res),
    resolve_stage(
        10, Base, Req,
        {Status, Res#{ <<"priv">> => maps:without([<<"hashpath">>], Priv) }},
        ExecName, Opts);
resolve_stage(9, Base, Req, Res, ExecName, Opts) ->
    ?event_debug(debug_ao_core, {stage, 9, ExecName, non_map_result_skipping_hash_path}, Opts),
    % Skip cryptographic linking and continue if we don't have a map that can have
    % a hashpath at all.
    resolve_stage(10, Base, Req, Res, ExecName, Opts);
resolve_stage(10, Base, Req, {ok, Res}, ExecName, Opts) ->
    ?event_debug(debug_ao_core, {stage, 10, ExecName, result_caching}, Opts),
    % Result caching: Optionally, cache the result of the computation locally.
    hb_cache_control:maybe_store(Base, Req, Res, Opts),
    resolve_stage(11, Base, Req, {ok, Res}, ExecName, Opts);
resolve_stage(10, Base, Req, Res, ExecName, Opts) ->
    ?event_debug(debug_ao_core, {stage, 10, ExecName, abnormal_status_skip_caching}, Opts),
    % Skip result caching if the result is abnormal.
    resolve_stage(11, Base, Req, Res, ExecName, Opts);
resolve_stage(11, Base, Req, Res, ExecName, Opts) ->
    ?event_debug(debug_ao_core, {stage, 11, ExecName}, Opts),
    % Notify processes that requested the resolution while we were executing and
    % unregister ourselves from the group.
    hb_persistent:unregister_notify(ExecName, Req, Res, Opts),
    resolve_stage(12, Base, Req, Res, ExecName, Opts);
resolve_stage(12, _Base, _Req, {ok, Res} = Res, ExecName, Opts) ->
    ?event_debug(debug_ao_core, {stage, 12, ExecName, maybe_spawn_worker}, Opts),
    % Check if we should fork out a new worker process for the current execution
    case
        {is_map(Res), hb_opts:get(spawn_worker, false, Opts#{ <<"prefer">> => local })}
    of
        {A, B} when (A == false) or (B == false) ->
            Res;
        {_, _} ->
            % Spawn a worker for the current execution
            WorkerPID = hb_persistent:start_worker(ExecName, Res, Opts),
            hb_persistent:forward_work(WorkerPID, Opts),
            Res
    end;
resolve_stage(12, _Base, _Req, OtherRes, _ExecName, _Opts) ->
    ?event_debug(debug_ao_core, {stage, 12, _ExecName, abnormal_status_skip_spawning}, _Opts),
    OtherRes.

%% @doc If the `AO_PROFILING' macro is defined (set by building/launching with
%% `rebar3 as ao_profiling') we record statistics about the execution of the
%% function. This is a costly operation, so if it is not defined, we simply
%% apply the function and return the result.
-ifndef(AO_PROFILING).
maybe_profiled_apply(Func, Args, _Base, _Req, _Opts) ->
    apply(Func, Args).
-else.
maybe_profiled_apply(Func, Args, Base, Req, Opts) ->
    CallStack = erlang:get(ao_stack),
    ?event(ao_trace,
        {profiling_apply,
            {func, Func},
            {args, Args},
            {call_stack, CallStack}
        }
    ),
    Key =
        case hb_maps:get(<<"device">>, Base, undefined, Opts) of
            undefined ->
                hb_util:bin(erlang:fun_to_list(Func));
            Device ->
                case hb_maps:get(<<"path">>, Req, undefined, Opts) of
                    undefined ->
                        hb_util:bin(erlang:fun_to_list(Func));
                    Path ->
                        MethodStr =
                            case hb_maps:get(<<"method">>, Req, undefined, Opts) of
                                undefined -> <<"">>;
                                <<"GET">> -> <<"">>;
                                Method -> <<"<", Method/binary, ">">>
                            end,
                        << 
                            (hb_util:bin(Device))/binary,
                            "/",
                            MethodStr/binary,
                            (hb_util:bin(Path))/binary
                        >>
                end
        end,
    put(
        ao_stack,
        case CallStack of
            undefined -> [Key];
            Stack -> [Key | Stack]
        end
    ),
    {ExecMicroSecs, Res} = timer:tc(fun() -> apply(Func, Args) end),
    put(ao_stack, CallStack),
    hb_event:record(<<"ao-call-counts">>, Key, Opts),
    hb_event:record(<<"ao-total-durations">>, Key, Opts, ExecMicroSecs),
    case CallStack of
        undefined -> ok;
        [Caller|_] ->
            hb_event:record(
                <<"ao-callers:", Key/binary>>,
                hb_util:bin(
                    [
                        <<"duration:">>,
                        Caller
                    ]
                ),
                Opts,
                ExecMicroSecs
            ),
            hb_event:record(
                <<"ao-callers:", Key/binary>>,
                hb_util:bin(
                    [
                        <<"calls:">>,
                        Caller
                    ]),
                Opts
            )
    end,
    Res.
-endif.

%% @doc Catch all return if we are in an infinite loop.
error_infinite(Base, Req, Opts) ->
    ?event(
        ao_core,
        {error, {type, infinite_recursion},
            {base, Base},
            {req, Req},
            {opts, Opts}
        },
        Opts
    ),
    ?trace(),
    {
        error,
        #{
            <<"status">> => 508,
            <<"body">> => <<"Request creates infinite recursion.">>
        }
    }.

%% @doc Handle an error in a device call.
error_execution(ExecGroup, Req, Whence, {Class, Exception, Stacktrace}, Opts) ->
    Error = {error, Whence, {Class, Exception, Stacktrace}},
    hb_persistent:unregister_notify(ExecGroup, Req, Error, Opts),
    ?event_debug(debug_ao_core, {handle_error, Error, {opts, Opts}}, Opts),
    case hb_opts:get(error_strategy, throw, Opts) of
        throw -> erlang:raise(Class, Exception, Stacktrace);
        _ -> Error
    end.

%% @doc Shortcut for resolving a key in a message without its status if it is
%% `ok'. This makes it easier to write complex logic on top of messages while
%% maintaining a functional style.
%% 
%% Additionally, this function supports the `{as, Device, Msg}' syntax, which
%% allows the key to be resolved using another device to resolve the key,
%% while maintaining the traceability of the `HashPath' of the output message.
%% 
%% Returns the value of the key if it is found, otherwise returns the default
%% provided by the user, or `not_found' if no default is provided.
get(Path, Msg, Opts) ->
    get(Path, Msg, not_found, Opts).
get(Path, {as, Device, Msg}, Default, Opts) ->
    get(
        Path,
        set(
            Msg,
            #{ <<"device">> => Device },
            internal_opts(Opts)
        ),
        Default,
        Opts
    );
get(Path, Msg, Default, Opts) ->
	case resolve(Msg, #{ <<"path">> => Path }, Opts#{ <<"spawn-worker">> => false }) of
		{ok, Value} -> Value;
		{error, _} -> Default
	end.

%% @doc take a sequence of base messages and paths, then return the value of the
%% first message that can be resolved using a path.
get_first(Paths, Opts) -> get_first(Paths, not_found, Opts).
get_first([], Default, _Opts) -> Default;
get_first([{Base, Path}|Msgs], Default, Opts) ->
    case get(Path, Base, Opts) of
        not_found -> get_first(Msgs, Default, Opts);
        Value -> Value
    end.

%% @doc Shortcut to get the list of keys from a message.
keys(Msg, Opts) ->
    get(<<"keys">>, Msg, Opts).

%% @doc Extend a message using its underlying device's `set' key.
set(Base, Req, Opts) ->
    ?event_debug(ao_internal, {set_called, {base, Base}, {req, Req}}, Opts),
    hb_util:ok(
        raw(undefined, <<"set">>, Base, Req, internal_opts(Opts)),
        Opts
    ).

%% @doc Set an individual (potentially deep) key in a message to a new using
%% the message's set device.
set(Base, Key, Value, Opts) ->
    DeepBase =
        fun Deep([LeafKey]) -> #{ LeafKey => Value };
            Deep([NextKey|Rest]) -> #{ NextKey => Deep(Rest, Value) }
        end,
    deep_set(Base, DeepBase(Key, Value), Opts).

%% @doc Recursively extend nested message values.
deep_set(Base, Req, Opts) when is_map(Req) ->
    hb_util:ok(
        raw(
            undefined,
            <<"set">>,
            Base,
            Req#{ <<"set">> => <<"deep">> },
            internal_opts(Opts)
        ),
        Opts
    ).

%% @doc Remove a key from a message, using its underlying device.
remove(Msg, Key, Opts) -> set(Msg, Key, unset, Opts).

%% @doc Convert a key to a binary in normalized form.
normalize_key(Key) -> normalize_key(Key, #{}).
normalize_key(Key, _Opts) when is_binary(Key) -> Key;
normalize_key(Key, _Opts) when is_atom(Key) -> atom_to_binary(Key);
normalize_key(Key, _Opts) when is_integer(Key) -> integer_to_binary(Key);
normalize_key(Key, _Opts) when is_list(Key) ->
    case hb_util:is_string_list(Key) of
        true -> normalize_key(list_to_binary(Key));
        false ->
            iolist_to_binary(
                lists:join(
                    <<"/">>,
                    lists:map(fun normalize_key/1, Key)
                )
            )
    end.

%% @doc Ensure that a message is processable by the AO-Core resolver: No lists.
%% Fast path: when every key is already a binary, `normalize_key/2' would
%% pass them through unchanged (values are never recursed here), so return
%% the map as-is. This is the overwhelming majority case on the resolve hot
%% path -- `resolve_stage(1, ...)' normalises both `Base' and `Req' on every
%% resolution -- and skips the `hb_maps:to_list'/`from_list' round-trip and
%% per-key dispatch.
normalize_keys(Msg) -> normalize_keys(Msg, #{}).
normalize_keys(Base, Opts) when is_list(Base) ->
    normalize_keys(
		hb_maps:from_list(
        	lists:zip(
            	lists:seq(1, length(Base)),
            	Base
			)
        ),
		Opts
	);
normalize_keys(Map, Opts) when is_map(Map) ->
    case has_only_binary_keys(maps:next(maps:iterator(Map))) of
        true -> Map;
        false -> do_normalize_keys(Map, Opts)
    end;
normalize_keys(Other, _Opts) -> Other.

%% @doc Walk a map iterator directly (avoiding the `maps:keys/1' list
%% allocation that `lists:all/2' would require) and return `true' iff every
%% key is a binary. Bails out on the first non-binary key.
has_only_binary_keys(none) -> true;
has_only_binary_keys({K, _V, Iter}) when is_binary(K) ->
    has_only_binary_keys(maps:next(Iter));
has_only_binary_keys({_K, _V, _Iter}) -> false.

%% @doc The original full-walk body, used when a non-binary key is present.
do_normalize_keys(Map, Opts) ->
    hb_maps:from_list(
        lists:map(
            fun({Key, Value}) when is_map(Value) ->
                {hb_ao:normalize_key(Key), Value};
            ({Key, Value}) ->
                {hb_ao:normalize_key(Key), Value}
            end,
            hb_maps:to_list(Map, Opts)
        )
    ).

%% @doc The execution options that are used internally by this module
%% when calling itself.
internal_opts(Opts) ->
    hb_maps:merge(
        hb_maps:without(?TEMP_OPTS, Opts, Opts),
        #{
            <<"topic">> => hb_opts:get(topic, ao_internal, Opts),
            <<"hashpath">> => ignore,
            <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
            <<"spawn-worker">> => false,
            <<"await-inprogress">> => false
        }
    ).

%% @doc Return the node message that should be used in order to perform
%% recursive executions.
execution_opts(Opts) ->
	% First, determine the arguments to pass to the function.
	% While calculating the arguments we unset the add_key option.
	Opts1 =
        hb_maps:remove(
            <<"trace">>,
            hb_maps:without(?TEMP_OPTS, Opts, Opts),
            Opts
        ),
    % Unless the user has explicitly requested recursive spawning, we
    % unset the spawn-worker option so that we do not spawn a new worker
    % for every resulting execution.
    case hb_opts:get(<<"spawn-worker">>, false, Opts) of
        recursive -> Opts1#{ <<"spawn-worker">> => recursive };
        _ -> Opts1
    end.
