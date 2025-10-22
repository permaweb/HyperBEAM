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
%%% the network (depending on the value of the `load_remote_devices' and 
%%% `trusted_device_signers' environment settings).
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
-export([resolve/2, resolve/3, resolve_many/2]).
-export([normalize_key/1, normalize_key/2, normalize_keys/1, normalize_keys/2]).
-export([force_message/2]).
%%% Shortcuts and tools:
-export([keys/1, keys/2, keys/3]).
-export([get/2, get/3, get/4, get_first/2, get_first/3]).
-export([set/3, set/4, remove/2, remove/3]).
%%% Exports for tests in hb_ao_test_vectors.erl:
-export([deep_set/4]).
-include("include/hb.hrl").

-define(TEMP_OPTS, [add_key, force_message, cache_control, spawn_worker]).

%% @doc Get the value of a message's key by running its associated device
%% function. Optionally, takes options that control the runtime environment. 
%% This function returns the raw result of the device function call:
%% `{ok | error, NewMessage}.'
%% The resolver is composed of a series of discrete phases:
%%      1: Normalization.
%%      2: Cache lookup.
%%      3: Validation check.
%%      4: Persistent-resolver lookup.
%%      5: Device lookup.
%%      6: Execution.
%%      7: Execution of the `step' hook.
%%      8: Subresolution.
%%      9: Cryptographic linking.
%%     10: Result caching.
%%     11: Notify waiters.
%%     12: Fork worker.
%%     13: Recurse or terminate.
resolve(Path, Opts) when is_binary(Path) ->
    resolve(#{ <<"path">> => Path }, Opts);
resolve(SingletonMsg, _Opts)
        when is_map(SingletonMsg), not is_map_key(<<"path">>, SingletonMsg) ->
    {error, <<"Attempted to resolve a message without a path.">>};
resolve(SingletonMsg, Opts) ->
    resolve_many(hb_singleton:from(SingletonMsg, Opts), Opts).

resolve(Base, Path, Opts) when not is_map(Path) ->
    resolve(Base, #{ <<"path">> => Path }, Opts);
resolve(Base, Req, Opts) ->
    PathParts = hb_path:from_message(request, Req, Opts),
    ?event(
        ao_core,
        {stage, 1, prepare_multimessage_resolution, {path_parts, PathParts}}
    ),
    MessagesToExec = [ Req#{ <<"path">> => Path } || Path <- PathParts ],
    ?event(ao_core,
        {stage,
            1,
            prepare_multimessage_resolution,
            {messages_to_exec, MessagesToExec}
        }
    ),
    resolve_many([Base | MessagesToExec], Opts).

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
    ?event(ao_core, {stage, na, resolve_directly_to_id, ID, {opts, Opts}}, Opts),
    try {ok, ensure_message_loaded(ID, Opts)}
    catch _:_:_ -> {error, not_found}
    end;
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
resolve_many({as, DevID, Msg}, Opts) ->
    subresolve(#{}, DevID, Msg, Opts);
resolve_many([{resolve, Subres}], Opts) ->
    resolve_many(Subres, Opts);
resolve_many(MsgList, Opts) ->
    ?event(ao_core, {resolve_many, MsgList}, Opts),
    Res = do_resolve_many(MsgList, Opts),
    ?event(ao_core, {resolve_many_complete, {res, Res}, {req, MsgList}}, Opts),
    Res.
do_resolve_many([], _Opts) ->
    {failure, <<"Attempted to resolve an empty message sequence.">>};
do_resolve_many([Res], Opts) ->
    ?event(ao_core, {stage, 11, resolve_complete, Res}),
    hb_cache:ensure_loaded(maybe_force_message(Res, Opts), Opts);
do_resolve_many([Base, Req | MsgList], Opts) ->
    ?event(ao_core, {stage, 0, resolve_many, {base, Base}, {req, Req}}),
    case resolve_stage(1, Base, Req, Opts) of
        {ok, Res} ->
            ?event(ao_core,
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
            ?event(ao_core, {stage, 13, resolve_many_terminating_early, Res}),
            maybe_force_message(Res, Opts)
    end.

resolve_stage(1, Link, Req, Opts) when ?IS_LINK(Link) ->
    % If the first message is a link, we should load the message and
    % continue with the resolution.
    ?event(ao_core, {stage, 1, resolve_base_link, {link, Link}}, Opts),
    resolve_stage(1, hb_cache:ensure_loaded(Link, Opts), Req, Opts);
resolve_stage(1, Base, Link, Opts) when ?IS_LINK(Link) ->
    % If the second message is a link, we should load the message and
    % continue with the resolution.
    ?event(ao_core, {stage, 1, resolve_req_link, {link, Link}}, Opts),
    resolve_stage(1, Base, hb_cache:ensure_loaded(Link, Opts), Opts);
resolve_stage(1, {as, DevID, Ref}, Req, Opts) when ?IS_ID(Ref) orelse ?IS_LINK(Ref) ->
    % Normalize `as' requests with a raw ID or link as the path. Links will be
    % loaded in following stages.
    resolve_stage(1, {as, DevID, #{ <<"path">> => Ref }}, Req, Opts);
resolve_stage(1, {as, DevID, Link}, Req, Opts) when ?IS_LINK(Link) ->
    % If the first message is an `as' with a link, we should load the message and
    % continue with the resolution.
    ?event(ao_core, {stage, 1, resolve_base_as_link, {link, Link}}, Opts),
    resolve_stage(1, {as, DevID, hb_cache:ensure_loaded(Link, Opts)}, Req, Opts);
resolve_stage(1, {as, DevID, Raw = #{ <<"path">> := ID }}, Req, Opts) when ?IS_ID(ID) ->
    % If the first message is an `as' with an ID, we should load the message and
    % apply the non-path elements of the sub-request to it.
    ?event(ao_core, {stage, 1, subresolving_with_load, {dev, DevID}, {id, ID}}, Opts),
    RemBase = hb_maps:without([<<"path">>], Raw, Opts),
    ?event(subresolution, {loading_message, {id, ID}, {params, RemBase}}, Opts),
    Baseb = ensure_message_loaded(ID, Opts),
    ?event(subresolution, {loaded_message, {msg, Baseb}}, Opts),
    Basec = hb_maps:merge(Baseb, RemBase, Opts),
    ?event(subresolution, {merged_message, {msg, Basec}}, Opts),
    Based = set(Basec, <<"device">>, DevID, Opts),
    ?event(subresolution, {loaded_parameterized_message, {msg, Based}}, Opts),
    resolve_stage(1, Based, Req, Opts);
resolve_stage(1, Raw = {as, DevID, SubReq}, Req, Opts) ->
    % Set the device of the message to the specified one and resolve the sub-path.
    % As this is the first message, we will then continue to execute the request
    % on the result.
    ?event(ao_core, {stage, 1, subresolving_base, {dev, DevID}, {subreq, SubReq}}, Opts),
    ?event(subresolution, {as, {dev, DevID}, {subreq, SubReq}, {req, Req}}),
    case subresolve(SubReq, DevID, SubReq, Opts) of
        {ok, SubRes} ->
            % The subresolution has returned a new message. Continue with it.
            ?event(subresolution,
                {continuing_with_subresolved_message, {base, SubRes}}
            ),
            resolve_stage(1, SubRes, Req, Opts);
        OtherRes ->
            % The subresolution has returned an error. Return it.
            ?event(subresolution,
                {subresolution_error, {base, Raw}, {res, OtherRes}}
            ),
            OtherRes
    end;
resolve_stage(1, RawBase, ReqOuter = #{ <<"path">> := {as, DevID, ReqInner} }, Opts) ->
    % Set the device to the specified `DevID' and resolve the message. Merging
    % the `ReqInner' into the `ReqOuter' message first. We return the result
    % of the sub-resolution directly.
    ?event(ao_core, {stage, 1, subresolving_from_request, {dev, DevID}}, Opts),
    LoadedInner = ensure_message_loaded(ReqInner, Opts),
    Req =
        hb_maps:merge(
            set(ReqOuter, <<"path">>, unset, Opts),
            if is_binary(LoadedInner) -> #{ <<"path">> => LoadedInner };
            true -> LoadedInner
            end,
			Opts
        ),
    ?event(subresolution,
        {subresolving_request_before_execution,
            {dev, DevID},
            {req, Req}
        }
    ),
    subresolve(RawBase, DevID, Req, Opts);
resolve_stage(1, {resolve, Subres}, Req, Opts) ->
    % If the first message is a `{resolve, Subres}' tuple, we should execute it
    % directly, then apply the request to the result.
    ?event(ao_core, {stage, 1, subresolving_base_message, {subres, Subres}}, Opts),
    % Unlike the `request' case for pre-subresolutions, we do not need to unset
    % the `force_message' option, because the result should be a message, anyway.
    % If it is not, it is more helpful to have the message placed into the `body'
    % of a result, which can then be executed upon.
    case resolve_many(Subres, Opts) of
        {ok, Base} ->
            ?event(ao_core, {stage, 1, subresolve_success, {new_base, Base}}, Opts),
            resolve_stage(1, Base, Req, Opts);
        OtherRes ->
            ?event(ao_core,
                {stage,
                    1,
                    subresolve_failed,
                    {subres, Subres},
                    {res, OtherRes}},
                Opts
            ),
            OtherRes
    end;
resolve_stage(1, Base, {resolve, Subres}, Opts) ->
    % If the second message is a `{resolve, Subresolution}' tuple, we should
    % execute the subresolution directly to gain the underlying `Req' for 
    % our execution. We assume that the subresolution is already in a normalized,
    % executable form, so we pass it to `resolve_many' for execution.
    ?event(ao_core, {stage, 1, subresolving_request_message, {subres, Subres}}, Opts),
    % We make sure to unset the `force_message' option so that if the subresolution
    % returns a literal, the rest of `resolve' will normalize it to a path.
    case resolve_many(Subres, maps:without([force_message], Opts)) of
        {ok, Req} ->
            ?event(
                ao_core,
                {stage, 1, request_subresolve_success, {req, Req}},
                Opts
            ),
            resolve_stage(1, Base, Req, Opts);
        OtherRes ->
            ?event(
                ao_core,
                {
                    stage,
                    1,
                    request_subresolve_failed,
                    {subres, Subres},
                    {res, OtherRes}
                },
                Opts
            ),
            OtherRes
    end;
resolve_stage(1, Base, Req, Opts) when is_list(Base) ->
    % Normalize lists to numbered maps (base=1) if necessary.
    ?event(ao_core, {stage, 1, list_normalize}, Opts),
    resolve_stage(1,
        normalize_keys(Base, Opts),
        Req,
        Opts
    );
resolve_stage(1, Base, NonMapReq, Opts) when not is_map(NonMapReq) ->
    ?event(ao_core, {stage, 1, path_normalize}),
    resolve_stage(1, Base, #{ <<"path">> => NonMapReq }, Opts);
resolve_stage(1, RawBase, RawReq, Opts) ->
    % Normalize the path to a private key containing the list of remaining
    % keys to resolve.
    ?event(ao_core, {stage, 1, normalize}, Opts),
    Base = normalize_keys(RawBase, Opts),
    Req = normalize_keys(RawReq, Opts),
    resolve_stage(2, Base, Req, Opts);
resolve_stage(2, Base, Req, Opts) ->
    ?event(ao_core, {stage, 2, cache_lookup}, Opts),
    % Lookup request in the cache. If we find a result, return it.
    % If we do not find a result, we continue to the next stage,
    % unless the cache lookup returns `halt' (the user has requested that we 
    % only return a result if it is already in the cache).
    case hb_cache_control:maybe_lookup(Base, Req, Opts) of
        {ok, Res} ->
            ?event(ao_core, {stage, 2, cache_hit, {res, Res}, {opts, Opts}}, Opts),
            {ok, Res};
        {continue, NewBase, NewReq} ->
            resolve_stage(3, NewBase, NewReq, Opts);
        {error, CacheResp} -> {error, CacheResp}
    end;
resolve_stage(3, Base, Req, Opts) when not is_map(Base) or not is_map(Req) ->
    % Validation check: If the messages are not maps, we cannot find a key
    % in them, so return not_found.
    ?event(ao_core, {stage, 3, validation_check_type_error}, Opts),
    {error, not_found};
resolve_stage(3, Base, Req, Opts) ->
    ?event(ao_core, {stage, 3, validation_check}, Opts),
    % Validation checks: Enable as necessary. We do not presently perform any
    % validity checks mid-execution, however we may wish to do so in the future.
    resolve_stage(4, Base, Req, Opts);
resolve_stage(4, Base, Req, Opts) ->
    ?event(ao_core, {stage, 4, persistent_resolver_lookup}, Opts),
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
            case hb_opts:get(allow_infinite, false, Opts) of
                true ->
                    % We are OK with infinite loops, so we just continue.
                    resolve_stage(5, Base, Req, GroupName, Opts);
                false ->
                    % We are not OK with infinite loops, so we raise an error.
                    error_infinite(Base, Req, Opts)
            end
    end.
resolve_stage(5, Base, Req, ExecName, Opts) ->
    ?event(ao_core, {stage, 5, device_lookup}, Opts),
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
			{Status, _Mod, Func} = hb_ao_device:message_to_fun(Base, Key, UserOpts),
			?event(
				{found_func_for_exec,
                    {key, Key},
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
					add_key =>
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
    ?event(ao_core, {stage, 6, ExecName, execution}, Opts),
	% Execution.
    ExecOpts = execution_opts(Opts),
	Args =
		case hb_maps:get(add_key, Opts, false, Opts) of
			false -> [Base, Req, ExecOpts];
			Key -> [Key, Base, Req, ExecOpts]
		end,
    % Try to execute the function.
    Res = 
        try
            TruncatedArgs = hb_ao_device:truncate_args(Func, Args),
            MsgRes = maybe_profiled_apply(Func, TruncatedArgs, Base, Req, Opts),
            ?event(
                ao_result,
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
    resolve_stage(7, Base, Req, Res, ExecName, Opts);
resolve_stage(7, Base, Req, {St, Res}, ExecName, Opts = #{ on := On = #{ <<"step">> := _ }}) ->
    ?event(ao_core, {stage, 7, ExecName, executing_step_hook, {on, On}}, Opts),
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
    case dev_hook:on(<<"step">>, HookReq, Opts) of
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
    ?event(ao_core, {stage, 7, ExecName, no_step_hook}, Opts),
    resolve_stage(8, Base, Req, Res, ExecName, Opts);
resolve_stage(8, Base, Req, {ok, {resolve, Sublist}}, ExecName, Opts) ->
    ?event(ao_core, {stage, 8, ExecName, subresolve_result}, Opts),
    % If the result is a `{resolve, Sublist}' tuple, we need to execute it
    % as a sub-resolution.
    resolve_stage(9, Base, Req, resolve_many(Sublist, Opts), ExecName, Opts);
resolve_stage(8, Base, Req, Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 8, ExecName, no_subresolution_necessary}, Opts),
    resolve_stage(9, Base, Req, Res, ExecName, Opts);
resolve_stage(9, Base, Req, {ok, Res}, ExecName, Opts) when is_map(Res) ->
    ?event(ao_core, {stage, 9, ExecName, generate_hashpath}, Opts),
    % Cryptographic linking. Now that we have generated the result, we
    % need to cryptographically link the output to its input via a hashpath.
    resolve_stage(10, Base, Req,
        case hb_opts:get(hashpath, update, Opts#{ only => local }) of
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
    ?event(ao_core, {stage, 9, ExecName, abnormal_status_reset_hashpath}, Opts),
    ?event(hashpath, {resetting_hashpath_res, {base, Base}, {req, Req}, {opts, Opts}}),
    % Skip cryptographic linking and reset the hashpath if the result is abnormal.
    Priv = hb_private:from_message(Res),
    resolve_stage(
        10, Base, Req,
        {Status, Res#{ <<"priv">> => maps:without([<<"hashpath">>], Priv) }},
        ExecName, Opts);
resolve_stage(9, Base, Req, Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 9, ExecName, non_map_result_skipping_hash_path}, Opts),
    % Skip cryptographic linking and continue if we don't have a map that can have
    % a hashpath at all.
    resolve_stage(10, Base, Req, Res, ExecName, Opts);
resolve_stage(10, Base, Req, {ok, Res}, ExecName, Opts) ->
    ?event(ao_core, {stage, 10, ExecName, result_caching}, Opts),
    % Result caching: Optionally, cache the result of the computation locally.
    hb_cache_control:maybe_store(Base, Req, Res, Opts),
    resolve_stage(11, Base, Req, {ok, Res}, ExecName, Opts);
resolve_stage(10, Base, Req, Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 10, ExecName, abnormal_status_skip_caching}, Opts),
    % Skip result caching if the result is abnormal.
    resolve_stage(11, Base, Req, Res, ExecName, Opts);
resolve_stage(11, Base, Req, Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 11, ExecName}, Opts),
    % Notify processes that requested the resolution while we were executing and
    % unregister ourselves from the group.
    hb_persistent:unregister_notify(ExecName, Req, Res, Opts),
    resolve_stage(12, Base, Req, Res, ExecName, Opts);
resolve_stage(12, _Base, _Req, {ok, Res} = Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 12, ExecName, maybe_spawn_worker}, Opts),
    % Check if we should fork out a new worker process for the current execution
    case {is_map(Res), hb_opts:get(spawn_worker, false, Opts#{ prefer => local })} of
        {A, B} when (A == false) or (B == false) ->
            Res;
        {_, _} ->
            % Spawn a worker for the current execution
            WorkerPID = hb_persistent:start_worker(ExecName, Res, Opts),
            hb_persistent:forward_work(WorkerPID, Opts),
            Res
    end;
resolve_stage(12, _Base, _Req, OtherRes, ExecName, Opts) ->
    ?event(ao_core, {stage, 12, ExecName, abnormal_status_skip_spawning}, Opts),
    OtherRes.

%% @doc Execute a sub-resolution.
subresolve(RawBase, DevID, ReqPath, Opts) when is_binary(ReqPath) ->
    % If the request is a binary, we assume that it is a path.
    subresolve(RawBase, DevID, #{ <<"path">> => ReqPath }, Opts);
subresolve(RawBase, DevID, Req, Opts) ->
    % First, ensure that the message is loaded from the cache.
    Base = ensure_message_loaded(RawBase, Opts),
    ?event(subresolution,
        {subresolving, {base, Base}, {dev, DevID}, {req, Req}}
    ),
    % Next, set the device ID if it is given.
    Base2 =
        case DevID of
            undefined -> Base;
            _ ->
                set(
                    Base,
                    <<"device">>,
                    DevID,
                    hb_maps:without(?TEMP_OPTS, Opts, Opts)
                )
        end,
    % If there is no path but there are elements to the request, we set these on
    % the base message. If there is a path, we do not modify the base message 
    % and instead apply the request message directly.
    case hb_path:from_message(request, Req, Opts) of
        undefined ->
            Base3 =
                case map_size(hb_maps:without([<<"path">>], Req, Opts)) of
                    0 -> Base2;
                    _ ->
                        set(
							Base2,
							set(Req, <<"path">>, unset, Opts),
							Opts#{ force_message => false }
						)
                end,
            ?event(subresolution,
                {subresolve_modified_base, Base3},
                Opts
            ),
            {ok, Base3};
        Path ->
            ?event(subresolution,
                {exec_subrequest_on_base,
                    {mod_base, Base2},
                    {req, Path},
                    {req, Req}
                }
            ),
            Res = resolve(Base2, Req, Opts),
            ?event(subresolution, {subresolved_with_new_device, {res, Res}}),
            Res
    end.

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
    hb_event:increment(<<"ao-call-counts">>, Key, Opts),
    hb_event:increment(<<"ao-total-durations">>, Key, Opts, ExecMicroSecs),
    case CallStack of
        undefined -> ok;
        [Caller|_] ->
            hb_event:increment(
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
            hb_event:increment(
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

%% @doc Ensure that a message is loaded from the cache if it is an ID, or 
%% a link, such that it is ready for execution.
ensure_message_loaded(MsgID, Opts) when ?IS_ID(MsgID) ->
    case hb_cache:read(MsgID, Opts) of
        {ok, LoadedMsg} ->
            LoadedMsg;
        not_found ->
            throw({necessary_message_not_found, <<"/">>, MsgID})
    end;
ensure_message_loaded(MsgLink, Opts) when ?IS_LINK(MsgLink) ->
    hb_cache:ensure_loaded(MsgLink, Opts);
ensure_message_loaded(Msg, _Opts) ->
    Msg.

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
    ?event(ao_core, {handle_error, Error, {opts, Opts}}, Opts),
    case hb_opts:get(error_strategy, throw, Opts) of
        throw -> erlang:raise(Class, Exception, Stacktrace);
        _ -> Error
    end.

%% @doc Force the result of a device call into a message if the result is not
%% requested by the `Opts'. If the result is a literal, we wrap it in a message
%% and signal the location of the result inside. We also similarly handle ao-result
%% when the result is a single value and an explicit status code.
maybe_force_message({Status, Res}, Opts) ->
    case hb_opts:get(force_message, false, Opts) of
        true -> force_message({Status, Res}, Opts);
        false -> {Status, Res}
    end;
maybe_force_message(Res, Opts) ->
    maybe_force_message({ok, Res}, Opts).

force_message({Status, Res}, Opts) when is_list(Res) ->
    force_message({Status, normalize_keys(Res, Opts)}, Opts);
force_message({Status, Subres = {resolve, _}}, _Opts) ->
    {Status, Subres};
force_message({Status, Literal}, _Opts) when not is_map(Literal) ->
    ?event(encode_result, {force_message_from_literal, Literal}),
    {Status, #{ <<"ao-result">> => <<"body">>, <<"body">> => Literal }};
force_message({Status, M = #{ <<"status">> := Status, <<"body">> := Body }}, _Opts)
        when map_size(M) == 2 ->
    ?event(encode_result, {force_message_from_literal_with_status, M}),
    {Status, #{
        <<"status">> => Status,
        <<"ao-result">> => <<"body">>,
        <<"body">> => Body
    }};
force_message({Status, Map}, _Opts) ->
    ?event(encode_result, {force_message_from_map, Map}),
    {Status, Map}.

%% @doc Shortcut for resolving a key in a message without its status if it is
%% `ok'. This makes it easier to write complex logic on top of messages while
%% maintaining a functional style.
%% 
%% Additionally, this function supports the `{as, Device, Msg}' syntax, which
%% allows the key to be resolved using another device to resolve the key,
%% while maintaining the tracability of the `HashPath' of the output message.
%% 
%% Returns the value of the key if it is found, otherwise returns the default
%% provided by the user, or `not_found' if no default is provided.
get(Path, Msg) ->
    get(Path, Msg, #{}).
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
	case resolve(Msg, #{ <<"path">> => Path }, Opts#{ spawn_worker => false }) of
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
keys(Msg) -> keys(Msg, #{}).
keys(Msg, Opts) -> keys(Msg, Opts, keep).
keys(Msg, Opts, keep) ->
    % There is quite a lot of AO-Core-specific machinery here. We:
    % 1. `get' the keys from the message, via AO-Core in order to trigger the
    %    `keys' function on its device.
    % 2. Ensure that the result is normalized to a message (not just a list)
    %    with `normalize_keys'.
    % 3. Now we have a map of the original keys, so we can use `hb_maps:values' to
    %    get a list of them.
    % 4. Normalize each of those keys in turn.
    try
        lists:map(
            fun normalize_key/1,
            hb_maps:values(
                normalize_keys(
                    hb_private:reset(get(<<"keys">>, Msg, Opts))
                ),
                Opts
            )
        )
    catch
        A:B:St ->
            throw(
                {cannot_get_keys,
                    {msg, Msg},
                    {opts, Opts},
                    {error, {A, B}},
                    {stacktrace, St}
                }
            )
    end;
keys(Msg, Opts, remove) ->
    lists:filter(
        fun(Key) -> not lists:member(Key, ?AO_CORE_KEYS) end,
        keys(Msg, Opts, keep)
    ).

%% @doc Shortcut for setting a key in the message using its underlying device.
%% Like the `get/3' function, this function honors the `error_strategy' option.
%% `set' works with maps and recursive paths while maintaining the appropriate
%% `HashPath' for each step.
set(RawBase, RawReq, Opts) when is_map(RawReq) ->
    Base = normalize_keys(RawBase, Opts),
    Req =
        hb_maps:without(
            [<<"hashpath">>, <<"priv">>],
            normalize_keys(RawReq, Opts),
            Opts
        ),
    ?event(ao_internal, {set_called, {base, Base}, {req, Req}}, Opts),
    % Get the next key to set. 
    case keys(Req, internal_opts(Opts)) of
        [] -> Base;
        [Key|_] ->
            % Get the value to set. Use AO-Core by default, but fall back to
            % getting via `maps' if it is not found.
            Val =
                case get(Key, Req, internal_opts(Opts)) of
                    not_found -> hb_maps:get(Key, Req, undefined, Opts);
                    Body -> Body
                end,
            ?event({got_val_to_set, {key, Key}, {val, Val}, {req, Req}}),
            % Next, set the key and recurse, removing the key from the Req.
            set(
                set(Base, Key, Val, internal_opts(Opts)),
                remove(Req, Key, internal_opts(Opts)),
                Opts
            )
    end.
set(Base, Key, Value, Opts) ->
    % For an individual key, we run deep_set with the key as the path.
    % This handles both the case that the key is a path as well as the case
    % that it is a single key.
    Path = hb_path:term_to_path_parts(Key, Opts),
    % ?event(
    %     {setting_individual_key,
    %         {base, Base},
    %         {key, Key},
    %         {path, Path},
    %         {value, Value}
    %     }
    % ),
    deep_set(Base, Path, Value, Opts).

%% @doc Recursively search a map, resolving keys, and set the value of the key
%% at the given path. This function has special cases for handling `set' calls
%% where the path is an empty list (`/'). In this case, if the value is an 
%% immediate, non-complex term, we can set it directly. Otherwise, we use the
%% device's `set' function to set the value.
deep_set(Msg, [], Value, Opts) when is_map(Msg) or is_list(Msg) ->
    device_set(Msg, <<"/">>, Value, Opts);
deep_set(_Msg, [], Value, _Opts) ->
    Value;
deep_set(Msg, [Key], Value, Opts) ->
    device_set(Msg, Key, Value, Opts);
deep_set(Msg, [Key|Rest], Value, Opts) ->
    case resolve(Msg, Key, Opts) of 
        {ok, SubMsg} ->
            ?event(debug_set,
                {traversing_deeper_to_set,
                    {current_key, Key},
                    {current_value, SubMsg},
                    {rest, Rest}
                },
                Opts
            ),
            Res =
                device_set(
                    Msg,
                    Key,
                    deep_set(SubMsg, Rest, Value, Opts),
                    <<"explicit">>,
                    Opts
                ),
            ?event(debug_set, {deep_set, {msg, Msg}, {key, Key}, {res, Res}}, Opts),
            Res;
        _ ->
            ?event(debug_set,
                {creating_new_map,
                    {current_key, Key},
                    {rest, Rest}
                },
                Opts
            ),
            Msg#{ Key => deep_set(#{}, Rest, Value, Opts) }
    end.

%% @doc Call the device's `set' function.
device_set(Msg, Key, Value, Opts) ->
    device_set(Msg, Key, Value, <<"deep">>, Opts).
device_set(Msg, Key, Value, Mode, Opts) ->
    ReqWithoutMode =
        case Key of
            <<"path">> ->
                #{ <<"path">> => <<"set_path">>, <<"value">> => Value };
            <<"/">> when is_map(Value) ->
                % The value is a map and it is to be `set' at the root of the
                % message. Subsequently, we call the device's `set' function
                % with all of the keys found in the message, leading it to be
                % merged into the message.
                Value#{ <<"path">> => <<"set">> };
            _ ->
                #{ <<"path">> => <<"set">>, Key => Value }
        end,
    Req =
        case Mode of
            <<"deep">> -> ReqWithoutMode;
            <<"explicit">> -> ReqWithoutMode#{ <<"set-mode">> => Mode }
        end,
    ?event(
        debug_set,
        {
            calling_device_set,
            {base, Msg},
            {key, Key},
            {value, Value},
            {full_req, Req}
        },
        Opts
    ),
    Res =
        hb_util:ok(
            resolve(
                Msg,
                Req,
                internal_opts(Opts)
            ),
            internal_opts(Opts)
        ),
    ?event(
        debug_set,
        {device_set_result, Res},
        Opts
    ),
    Res.

%% @doc Remove a key from a message, using its underlying device.
remove(Msg, Key) -> remove(Msg, Key, #{}).
remove(Msg, Key, Opts) ->
	hb_util:ok(
        resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"item">> => Key },
            internal_opts(Opts)
        ),
        Opts
    ).

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
    hb_maps:from_list(
        lists:map(
            fun({Key, Value}) when is_map(Value) ->
                {hb_ao:normalize_key(Key), Value};
            ({Key, Value}) ->
                {hb_ao:normalize_key(Key), Value}
            end,
            hb_maps:to_list(Map, Opts)
        )
    );
normalize_keys(Other, _Opts) -> Other.

%% @doc The execution options that are used internally by this module
%% when calling itself.
internal_opts(Opts) ->
    hb_maps:merge(Opts, #{
        topic => hb_opts:get(topic, ao_internal, Opts),
        hashpath => ignore,
        cache_control => [<<"no-cache">>, <<"no-store">>],
        spawn_worker => false,
        await_inprogress => false
    }).

%% @doc Return the node message that should be used in order to perform
%% recursive executions.
execution_opts(Opts) ->
	% First, determine the arguments to pass to the function.
	% While calculating the arguments we unset the add_key option.
	Opts1 = hb_maps:remove(trace, hb_maps:without(?TEMP_OPTS, Opts, Opts), Opts),
    % Unless the user has explicitly requested recursive spawning, we
    % unset the spawn_worker option so that we do not spawn a new worker
    % for every resulting execution.
    case hb_maps:get(spawn_worker, Opts1, false, Opts) of
        recursive -> Opts1;
        _ -> hb_maps:remove(spawn_worker, Opts1, Opts)
    end.