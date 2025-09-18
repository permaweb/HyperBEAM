# hb_ao

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_ao.erl)

This module is the root of the device call logic of the 
AO-Core protocol in HyperBEAM.
At the implementation level, every message is simply a collection of keys,
dictated by its `Device`, that can be resolved in order to yield their
values. Each key may contain a link to another message or a raw value:
	`ao(BaseMessage, RequestMessage) -> {Status, Result}`
Under-the-hood, `AO-Core(BaseMessage, RequestMessage)` leads to a lookup of
the `device` key of the base message, followed by the evaluation of
`DeviceMod:PathPart(BaseMessage, RequestMessage)`, which defines the user 
compute to be performed. If `BaseMessage` does not specify a device, 
`~message@1.0` is assumed. The key to resolve is specified by the `path` 
field of the message.
After each output, the `HashPath` is updated to include the `RequestMessage`
that was executed upon it.
Because each message implies a device that can resolve its keys, as well
as generating a merkle tree of the computation that led to the result,
you can see the AO-Core protocol as a system for cryptographically chaining 
the execution of `combinators`. See `docs/ao-core-protocol.md` for more 
information about AO-Core.
The `key(BaseMessage, RequestMessage)` pattern is repeated throughout the 
HyperBEAM codebase, sometimes with `BaseMessage` replaced with `Msg1`, `M1`
or similar, and `RequestMessage` replaced with `Msg2`, `M2`, etc.
The result of any computation can be either a new message or a raw literal 
value (a binary, integer, float, atom, or list of such values).
Devices can be expressed as either modules or maps. They can also be 
referenced by an Arweave ID, which can be used to load a device from 
the network (depending on the value of the `load_remote_devices` and 
`trusted_device_signers` environment settings).
HyperBEAM device implementations are defined as follows:
<pre>
    DevMod:ExportedFunc : Key resolution functions. All are assumed to be
                          device keys (thus, present in every message that
                          uses it) unless specified by `DevMod:info()`.
                          Each function takes a set of parameters
                          of the form `DevMod:KeyHandler(Msg1, Msg2, Opts)`.
                          Each of these arguments can be ommitted if not
                          needed. Non-exported functions are not assumed
                          to be device keys.
    DevMod:info : Optional. Returns a map of options for the device. All 
                  options are optional and assumed to be the defaults if 
                  not specified. This function can accept a `Message1` as 
                  an argument, allowing it to specify its functionality 
                  based on a specific message if appropriate.
    info/exports : Overrides the export list of the Erlang module, such that
                  only the functions in this list are assumed to be device
                  keys. Defaults to all of the functions that DevMod 
                  exports in the Erlang environment.
    info/excludes : A list of keys that should not be resolved by the device,
                    despite being present in the Erlang module exports list.
    info/handler : A function that should be used to handle _all_ keys for 
                   messages using the device.
    info/default : A function that should be used to handle all keys that
                   are not explicitly implemented by the device. Defaults to
                   the `dev_message` device, which contains general keys for 
                   interacting with messages.
    info/default_mod : A different device module that should be used to
                   handle all keys that are not explicitly implemented
                   by the device. Defaults to the `dev_message` device.
    info/grouper : A function that returns the concurrency 'group' name for
                   an execution. Executions with the same group name will
                   be executed by sending a message to the associated process
                   and waiting for a response. This allows you to control 
                   concurrency of execution and to allow executions to share
                   in-memory state as applicable. Default: A derivation of
                   Msg1+Msg2. This means that concurrent calls for the same
                   output will lead to only a single execution.
    info/worker : A function that should be run as the 'server' loop of
                  the executor for interactions using the device.
The HyperBEAM resolver also takes a number of runtime options that change
the way that the environment operates:
`update_hashpath`:  Whether to add the `Msg2` to `HashPath` for the `Msg3`.
					Default: true.
`add_key`:          Whether to add the key to the start of the arguments.
					Default: `<not set>`.
</pre>

---

## Exported Functions

- `deep_set/4`
- `find_exported_function/5`
- `force_message/2`
- `get_first/2`
- `get_first/3`
- `get/2`
- `get/3`
- `get/4`
- `info/2`
- `is_exported/4`
- `keys/1`
- `keys/2`
- `keys/3`
- `load_device/2`
- `message_to_device/2`
- `message_to_fun/3`
- `normalize_key/1`
- `normalize_key/2`
- `normalize_keys/1`
- `normalize_keys/2`
- `remove/2`
- `remove/3`
- `resolve_many/2`
- `resolve/2`
- `resolve/3`
- `set/3`
- `set/4`
- `truncate_args/2`

---

### resolve

This module is the root of the device call logic of the 
Get the value of a message's key by running its associated device

```erlang
resolve(Path, Opts) when is_binary(Path) ->
    resolve(#{ <<"path">> => Path }, Opts);
```

### resolve

This module is the root of the device call logic of the 
Get the value of a message's key by running its associated device

```erlang
resolve(SingletonMsg, _Opts)
        when is_map(SingletonMsg), not is_map_key(<<"path">>, SingletonMsg) ->
    {error, <<"Attempted to resolve a message without a path.">>};
```

### resolve

This module is the root of the device call logic of the 
Get the value of a message's key by running its associated device

```erlang
resolve(SingletonMsg, Opts) ->
    resolve_many(hb_singleton:from(SingletonMsg, Opts), Opts).
```

### resolve

```erlang
resolve(Msg1, Path, Opts) when not is_map(Path) ->
    resolve(Msg1, #{ <<"path">> => Path }, Opts);
```

### resolve

```erlang
resolve(Msg1, Msg2, Opts) ->
    PathParts = hb_path:from_message(request, Msg2, Opts),
    ?event(ao_core, {stage, 1, prepare_multimessage_resolution, {path_parts, PathParts}}),
    MessagesToExec = [ Msg2#{ <<"path">> => Path } || Path <- PathParts ],
    ?event(ao_core, {stage, 1, prepare_multimessage_resolution, {messages_to_exec, MessagesToExec}}),
    resolve_many([Msg1 | MessagesToExec], Opts).
```

### resolve_many

Resolve a list of messages in sequence. Take the output of the first

```erlang
resolve_many([ID], Opts) when ?IS_ID(ID) ->
    % Note: This case is necessary to place specifically here for two reasons:
    % 1. It is not in `do_resolve_many' because we need to handle the case
    %    where a result from a prior invocation is an ID itself. We should not
    %    attempt to resolve such IDs further.
```

### resolve_many

```erlang
resolve_many(ListMsg, Opts) when is_map(ListMsg) ->
    % We have been given a message rather than a list of messages, so we should
    % convert it to a list, assuming that the message is monotonically numbered.
```

### resolve_many

```erlang
resolve_many({as, DevID, Msg}, Opts) ->
    subresolve(#{}, DevID, Msg, Opts);
```

### resolve_many

```erlang
resolve_many([{resolve, Subres}], Opts) ->
    resolve_many(Subres, Opts);
```

### resolve_many

```erlang
resolve_many(MsgList, Opts) ->
    ?event(ao_core, {resolve_many, MsgList}, Opts),
    Res = do_resolve_many(MsgList, Opts),
    ?event(ao_core, {resolve_many_complete, {res, Res}, {req, MsgList}}, Opts),
    Res.
```

### do_resolve_many

```erlang
do_resolve_many([], _Opts) ->
    {failure, <<"Attempted to resolve an empty message sequence.">>};
```

### do_resolve_many

```erlang
do_resolve_many([Msg3], Opts) ->
    ?event(ao_core, {stage, 11, resolve_complete, Msg3}),
    {ok, hb_cache:ensure_loaded(Msg3, Opts)};
```

### do_resolve_many

```erlang
do_resolve_many([Msg1, Msg2 | MsgList], Opts) ->
    ?event(ao_core, {stage, 0, resolve_many, {msg1, Msg1}, {msg2, Msg2}}),
    case resolve_stage(1, Msg1, Msg2, Opts) of
        {ok, Msg3} ->
            ?event(ao_core,
                {
                    stage,
                    13,
                    resolved_step,
                    {msg3, Msg3},
                    {opts, Opts}
                },
				Opts
            ),
            do_resolve_many([Msg3 | MsgList], Opts);
        Res ->
            % The result is not a resolvable message. Return it.
```

### resolve_stage

```erlang
resolve_stage(1, Link, Msg2, Opts) when ?IS_LINK(Link) ->
    % If the first message is a link, we should load the message and
    % continue with the resolution.
```

### resolve_stage

```erlang
resolve_stage(1, Msg1, Link, Opts) when ?IS_LINK(Link) ->
    % If the second message is a link, we should load the message and
    % continue with the resolution.
```

### resolve_stage

```erlang
resolve_stage(1, {as, DevID, Ref}, Msg2, Opts) when ?IS_ID(Ref) orelse ?IS_LINK(Ref) ->
    % Normalize `as' requests with a raw ID or link as the path. Links will be
    % loaded in following stages.
```

### resolve_stage

```erlang
resolve_stage(1, {as, DevID, Link}, Msg2, Opts) when ?IS_LINK(Link) ->
    % If the first message is an `as' with a link, we should load the message and
    % continue with the resolution.
```

### resolve_stage

```erlang
resolve_stage(1, {as, DevID, Raw = #{ <<"path">> := ID }}, Msg2, Opts) when ?IS_ID(ID) ->
    % If the first message is an `as' with an ID, we should load the message and
    % apply the non-path elements of the sub-request to it.
```

### resolve_stage

```erlang
resolve_stage(1, Raw = {as, DevID, SubReq}, Msg2, Opts) ->
    % Set the device of the message to the specified one and resolve the sub-path.
```

### resolve_stage

```erlang
resolve_stage(1, RawMsg1, Msg2Outer = #{ <<"path">> := {as, DevID, Msg2Inner} }, Opts) ->
    % Set the device to the specified `DevID' and resolve the message. Merging
    % the `Msg2Inner' into the `Msg2Outer' message first. We return the result
    % of the sub-resolution directly.
```

### resolve_stage

```erlang
resolve_stage(1, {resolve, Subres}, Msg2, Opts) ->
    % If the first message is a `{resolve, Subres}' tuple, we should execute it
    % directly, then apply the request to the result.
```

### resolve_stage

```erlang
resolve_stage(1, Msg1, {resolve, Subres}, Opts) ->
    % If the second message is a `{resolve, Subresolution}' tuple, we should
    % execute the subresolution directly to gain the underlying `Msg2' for 
    % our execution. We assume that the subresolution is already in a normalized,
    % executable form, so we pass it to `resolve_many' for execution.
```

### resolve_stage

```erlang
resolve_stage(1, Msg1, Msg2, Opts) when is_list(Msg1) ->
    % Normalize lists to numbered maps (base=1) if necessary.
```

### resolve_stage

```erlang
resolve_stage(1, Msg1, NonMapMsg2, Opts) when not is_map(NonMapMsg2) ->
    ?event(ao_core, {stage, 1, path_normalize}),
    resolve_stage(1, Msg1, #{ <<"path">> => NonMapMsg2 }, Opts);
```

### resolve_stage

```erlang
resolve_stage(1, RawMsg1, RawMsg2, Opts) ->
    % Normalize the path to a private key containing the list of remaining
    % keys to resolve.
```

### resolve_stage

```erlang
resolve_stage(2, Msg1, Msg2, Opts) ->
    ?event(ao_core, {stage, 2, cache_lookup}, Opts),
    % Lookup request in the cache. If we find a result, return it.
```

### resolve_stage

```erlang
resolve_stage(3, Msg1, Msg2, Opts) when not is_map(Msg1) or not is_map(Msg2) ->
    % Validation check: If the messages are not maps, we cannot find a key
    % in them, so return not_found.
```

### resolve_stage

```erlang
resolve_stage(3, Msg1, Msg2, Opts) ->
    ?event(ao_core, {stage, 3, validation_check}, Opts),
    % Validation check: Check if the message is valid.
```

### resolve_stage

```erlang
resolve_stage(4, Msg1, Msg2, Opts) ->
    ?event(ao_core, {stage, 4, persistent_resolver_lookup}, Opts),
    % Persistent-resolver lookup: Search for local (or Distributed
    % Erlang cluster) processes that are already performing the execution.
```

### resolve_stage

```erlang
resolve_stage(5, Msg1, Msg2, ExecName, Opts) ->
    ?event(ao_core, {stage, 5, device_lookup}, Opts),
    % Device lookup: Find the Erlang function that should be utilized to 
    % execute Msg2 on Msg1.
```

### resolve_stage

```erlang
resolve_stage(6, Func, Msg1, Msg2, ExecName, Opts) ->
    ?event(ao_core, {stage, 6, ExecName, execution}, Opts),
	% Execution.
```

### resolve_stage

```erlang
resolve_stage(7, Msg1, Msg2, {St, Res}, ExecName, Opts = #{ on := On = #{ <<"step">> := _ }}) ->
    ?event(ao_core, {stage, 7, ExecName, executing_step_hook, {on, On}}, Opts),
    % If the `step' hook is defined, we execute it. Note: This function clause
    % matches directly on the `on' key of the `Opts' map. This is in order to
    % remove the expensive lookup check that would otherwise be performed on every
    % execution.
```

### resolve_stage

```erlang
resolve_stage(7, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 7, ExecName, no_step_hook}, Opts),
    resolve_stage(8, Msg1, Msg2, Res, ExecName, Opts);
```

### resolve_stage

```erlang
resolve_stage(8, Msg1, Msg2, {ok, {resolve, Sublist}}, ExecName, Opts) ->
    ?event(ao_core, {stage, 8, ExecName, subresolve_result}, Opts),
    % If the result is a `{resolve, Sublist}' tuple, we need to execute it
    % as a sub-resolution.
```

### resolve_stage

```erlang
resolve_stage(8, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 8, ExecName, no_subresolution_necessary}, Opts),
    resolve_stage(9, Msg1, Msg2, Res, ExecName, Opts);
```

### resolve_stage

```erlang
resolve_stage(9, Msg1, Msg2, {ok, Msg3}, ExecName, Opts) when is_map(Msg3) ->
    ?event(ao_core, {stage, 9, ExecName, generate_hashpath}, Opts),
    % Cryptographic linking. Now that we have generated the result, we
    % need to cryptographically link the output to its input via a hashpath.
```

### resolve_stage

```erlang
resolve_stage(9, Msg1, Msg2, {Status, Msg3}, ExecName, Opts) when is_map(Msg3) ->
    ?event(ao_core, {stage, 9, ExecName, abnormal_status_reset_hashpath}, Opts),
    ?event(hashpath, {resetting_hashpath_msg3, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    % Skip cryptographic linking and reset the hashpath if the result is abnormal.
```

### resolve_stage

```erlang
resolve_stage(9, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 9, ExecName, non_map_result_skipping_hash_path}, Opts),
    % Skip cryptographic linking and continue if we don't have a map that can have
    % a hashpath at all.
```

### resolve_stage

```erlang
resolve_stage(10, Msg1, Msg2, {ok, Msg3}, ExecName, Opts) ->
    ?event(ao_core, {stage, 10, ExecName, result_caching}, Opts),
    % Result caching: Optionally, cache the result of the computation locally.
```

### resolve_stage

```erlang
resolve_stage(10, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 10, ExecName, abnormal_status_skip_caching}, Opts),
    % Skip result caching if the result is abnormal.
```

### resolve_stage

```erlang
resolve_stage(11, Msg1, Msg2, Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 11, ExecName}, Opts),
    % Notify processes that requested the resolution while we were executing and
    % unregister ourselves from the group.
```

### resolve_stage

```erlang
resolve_stage(12, _Msg1, _Msg2, {ok, Msg3} = Res, ExecName, Opts) ->
    ?event(ao_core, {stage, 12, ExecName, maybe_spawn_worker}, Opts),
    % Check if we should spawn a worker for the current execution
    case {is_map(Msg3), hb_opts:get(spawn_worker, false, Opts#{ prefer => local })} of
        {A, B} when (A == false) or (B == false) ->
            Res;
        {_, _} ->
            % Spawn a worker for the current execution
            WorkerPID = hb_persistent:start_worker(ExecName, Msg3, Opts),
            hb_persistent:forward_work(WorkerPID, Opts),
            Res
    end;
```

### resolve_stage

```erlang
resolve_stage(12, _Msg1, _Msg2, OtherRes, ExecName, Opts) ->
    ?event(ao_core, {stage, 12, ExecName, abnormal_status_skip_spawning}, Opts),
    OtherRes.
```

### subresolve

Execute a sub-resolution.

```erlang
subresolve(RawMsg1, DevID, ReqPath, Opts) when is_binary(ReqPath) ->
    % If the request is a binary, we assume that it is a path.
```

### subresolve

```erlang
subresolve(RawMsg1, DevID, Req, Opts) ->
    % First, ensure that the message is loaded from the cache.
```

### maybe_profiled_apply

If the `AO_PROFILING` macro is defined (set by building/launching with

```erlang
maybe_profiled_apply(Func, Args, _Msg1, _Msg2, _Opts) ->
    apply(Func, Args).
```

### maybe_profiled_apply

```erlang
maybe_profiled_apply(Func, Args, Msg1, Msg2, Opts) ->
    CallStack = erlang:get(ao_stack),
    ?event(ao_trace,
        {profiling_apply,
            {func, Func},
            {args, Args},
            {call_stack, CallStack}
        }
    ),
    Key =
        case hb_maps:get(<<"device">>, Msg1, undefined, Opts) of
            undefined ->
                hb_util:bin(erlang:fun_to_list(Func));
            Device ->
                case hb_maps:get(<<"path">>, Msg2, undefined, Opts) of
                    undefined ->
                        hb_util:bin(erlang:fun_to_list(Func));
                    Path ->
                        MethodStr =
                            case hb_maps:get(<<"method">>, Msg2, undefined, Opts) of
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
```

### ensure_message_loaded

Ensure that a message is loaded from the cache if it is an ID, or 

```erlang
ensure_message_loaded(MsgID, Opts) when ?IS_ID(MsgID) ->
    case hb_cache:read(MsgID, Opts) of
        {ok, LoadedMsg} ->
            LoadedMsg;
        not_found ->
            throw({necessary_message_not_found, <<"/">>, MsgID})
    end;
```

### ensure_message_loaded

Ensure that a message is loaded from the cache if it is an ID, or 

```erlang
ensure_message_loaded(MsgLink, Opts) when ?IS_LINK(MsgLink) ->
    hb_cache:ensure_loaded(MsgLink, Opts);
```

### ensure_message_loaded

Ensure that a message is loaded from the cache if it is an ID, or 

```erlang
ensure_message_loaded(Msg, _Opts) ->
    Msg.
```

### error_invalid_message

Catch all return if the message is invalid.

```erlang
error_invalid_message(Msg1, Msg2, Opts) ->
    ?event(
        ao_core,
        {error, {type, invalid_message},
            {msg1, Msg1},
            {msg2, Msg2},
            {opts, Opts}
        },
        Opts
    ),
    {
        error,
        #{
            <<"status">> => 400,
            <<"body">> => <<"Request contains non-verifiable message.">>
        }
    }.
```

### error_infinite

Catch all return if we are in an infinite loop.

```erlang
error_infinite(Msg1, Msg2, Opts) ->
    ?event(
        ao_core,
        {error, {type, infinite_recursion},
            {msg1, Msg1},
            {msg2, Msg2},
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
```

### error_invalid_intermediate_status

```erlang
error_invalid_intermediate_status(Msg1, Msg2, Msg3, RemainingPath, Opts) ->
    ?event(
        ao_core,
        {error, {type, invalid_intermediate_status},
            {msg2, Msg2},
            {msg3, Msg3},
            {remaining_path, RemainingPath},
            {opts, Opts}
        },
        Opts
    ),
    ?event(ao_result, 
        {intermediate_failure, {msg1, Msg1},
            {msg2, Msg2}, {msg3, Msg3},
            {remaining_path, RemainingPath}, {opts, Opts}}),
    {
        error,
        #{
            <<"status">> => 422,
            <<"body">> => Msg3,
            <<"key">> => hb_maps:get(<<"path">>, Msg2, <<"Key unknown.">>, Opts),
            <<"remaining-path">> => RemainingPath
        }
    }.
```

### error_execution

Handle an error in a device call.

```erlang
error_execution(ExecGroup, Msg2, Whence, {Class, Exception, Stacktrace}, Opts) ->
    Error = {error, Whence, {Class, Exception, Stacktrace}},
    hb_persistent:unregister_notify(ExecGroup, Msg2, Error, Opts),
    ?event(ao_core, {handle_error, Error, {opts, Opts}}, Opts),
    case hb_opts:get(error_strategy, throw, Opts) of
        throw -> erlang:raise(Class, Exception, Stacktrace);
        _ -> Error
    end.
```

### maybe_force_message

Force the result of a device call into a message if the result is not

```erlang
maybe_force_message({Status, Res}, Opts) ->
    case hb_opts:get(force_message, false, Opts) of
        true -> force_message({Status, Res}, Opts);
        false -> {Status, Res}
    end;
```

### maybe_force_message

Force the result of a device call into a message if the result is not

```erlang
maybe_force_message(Res, Opts) ->
    maybe_force_message({ok, Res}, Opts).
```

### force_message

```erlang
force_message({Status, Res}, Opts) when is_list(Res) ->
    force_message({Status, normalize_keys(Res, Opts)}, Opts);
```

### force_message

```erlang
force_message({Status, Subres = {resolve, _}}, _Opts) ->
    {Status, Subres};
```

### force_message

```erlang
force_message({Status, Literal}, _Opts) when not is_map(Literal) ->
    ?event({force_message_from_literal, Literal}),
    {Status, #{ <<"ao-result">> => <<"body">>, <<"body">> => Literal }};
```

### force_message

```erlang
force_message({Status, M = #{ <<"status">> := Status, <<"body">> := Body }}, _Opts)
        when map_size(M) == 2 ->
    ?event({force_message_from_literal_with_status, M}),
    {Status, #{
        <<"status">> => Status,
        <<"ao-result">> => <<"body">>,
        <<"body">> => Body
    }};
```

### force_message

```erlang
force_message({Status, Map}, _Opts) ->
    ?event({force_message_from_map, Map}),
    {Status, Map}.
```

### get

Shortcut for resolving a key in a message without its status if it is

```erlang
get(Path, Msg) ->
    get(Path, Msg, #{}).
```

### get

```erlang
get(Path, Msg, Opts) ->
    get(Path, Msg, not_found, Opts).
```

### get

```erlang
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
```

### get

```erlang
get(Path, Msg, Default, Opts) ->
	case resolve(Msg, #{ <<"path">> => Path }, Opts#{ spawn_worker => false }) of
		{ok, Value} -> Value;
		{error, _} -> Default
	end.
```

### get_first

take a sequence of base messages and paths, then return the value of the

```erlang
get_first(Paths, Opts) -> get_first(Paths, not_found, Opts).
```

### get_first

take a sequence of base messages and paths, then return the value of the

```erlang
get_first([], Default, _Opts) -> Default;
```

### get_first

take a sequence of base messages and paths, then return the value of the

```erlang
get_first([{Base, Path}|Msgs], Default, Opts) ->
    case get(Path, Base, Opts) of
        not_found -> get_first(Msgs, Default, Opts);
        Value -> Value
    end.
```

### keys

Shortcut to get the list of keys from a message.

```erlang
keys(Msg) -> keys(Msg, #{}).
```

### keys

Shortcut to get the list of keys from a message.

```erlang
keys(Msg, Opts) -> keys(Msg, Opts, keep).
```

### keys

Shortcut to get the list of keys from a message.

```erlang
keys(Msg, Opts, keep) ->
    % There is quite a lot of AO-Core-specific machinery here. We:
    % 1. `get' the keys from the message, via AO-Core in order to trigger the
    %    `keys' function on its device.
```

### keys

```erlang
keys(Msg, Opts, remove) ->
    lists:filter(
        fun(Key) -> not lists:member(Key, ?AO_CORE_KEYS) end,
        keys(Msg, Opts, keep)
    ).
```

### set

Shortcut for setting a key in the message using its underlying device.

```erlang
set(RawMsg1, RawMsg2, Opts) when is_map(RawMsg2) ->
    Msg1 = normalize_keys(RawMsg1, Opts),
    Msg2 = hb_maps:without([<<"hashpath">>, <<"priv">>], normalize_keys(RawMsg2, Opts), Opts),
    ?event(ao_internal, {set_called, {msg1, Msg1}, {msg2, Msg2}}, Opts),
    % Get the next key to set. 
```

### set

```erlang
set(Msg1, Key, Value, Opts) ->
    % For an individual key, we run deep_set with the key as the path.
```

### deep_set

Recursively search a map, resolving keys, and set the value of the key

```erlang
deep_set(Msg, [], Value, Opts) when is_map(Msg) or is_list(Msg) ->
    device_set(Msg, <<"/">>, Value, Opts);
```

### deep_set

Recursively search a map, resolving keys, and set the value of the key

```erlang
deep_set(_Msg, [], Value, _Opts) ->
    Value;
```

### deep_set

Recursively search a map, resolving keys, and set the value of the key

```erlang
deep_set(Msg, [Key], Value, Opts) ->
    device_set(Msg, Key, Value, Opts);
```

### deep_set

Recursively search a map, resolving keys, and set the value of the key

```erlang
deep_set(Msg, [Key|Rest], Value, Opts) ->
    case resolve(Msg, Key, Opts) of 
        {ok, SubMsg} ->
            ?event(
                {traversing_deeper_to_set,
                    {current_key, Key},
                    {current_value, SubMsg},
                    {rest, Rest}
                }
            ),
            Res = device_set(Msg, Key, deep_set(SubMsg, Rest, Value, Opts), <<"explicit">>, Opts),
            ?event({deep_set_result, {msg, Msg}, {key, Key}, {res, Res}}),
            Res;
        _ ->
            ?event(
                {creating_new_map,
                    {current_key, Key},
                    {rest, Rest}
                }
            ),
            Msg#{ Key => deep_set(#{}, Rest, Value, Opts) }
    end.
```

### device_set

Call the device's `set` function.

```erlang
device_set(Msg, Key, Value, Opts) ->
    device_set(Msg, Key, Value, <<"deep">>, Opts).
```

### device_set

Call the device's `set` function.

```erlang
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
```

### remove

Remove a key from a message, using its underlying device.

```erlang
remove(Msg, Key) -> remove(Msg, Key, #{}).
```

### remove

Remove a key from a message, using its underlying device.

```erlang
remove(Msg, Key, Opts) ->
	hb_util:ok(
        resolve(
            Msg,
            #{ <<"path">> => <<"remove">>, <<"item">> => Key },
            internal_opts(Opts)
        ),
        Opts
    ).
```

### truncate_args

Truncate the arguments of a function to the number of arguments it

```erlang
truncate_args(Fun, Args) ->
    {arity, Arity} = erlang:fun_info(Fun, arity),
    lists:sublist(Args, Arity).
```

### message_to_fun

Calculate the Erlang function that should be called to get a value for

```erlang
message_to_fun(Msg, Key, Opts) ->
    % Get the device module from the message.
```

### message_to_device

Extract the device module from a message.

```erlang
message_to_device(Msg, Opts) ->
    case dev_message:get(<<"device">>, Msg, Opts) of
        {error, not_found} ->
            % The message does not specify a device, so we use the default device.
```

### info_handler_to_fun

Parse a handler key given by a device's `info`.

```erlang
info_handler_to_fun(Handler, _Msg, _Key, _Opts) when is_function(Handler) ->
	{add_key, Handler};
```

### info_handler_to_fun

Parse a handler key given by a device's `info`.

```erlang
info_handler_to_fun(HandlerMap, Msg, Key, Opts) ->
	case hb_maps:find(excludes, HandlerMap, Opts) of
		{ok, Exclude} ->
			case lists:member(Key, Exclude) of
				true ->
					{ok, MsgWithoutDevice} =
						dev_message:remove(Msg, #{ item => device }, Opts),
					message_to_fun(
						MsgWithoutDevice#{ <<"device">> => default_module() },
						Key,
						Opts
					);
				false -> {add_key, hb_maps:get(func, HandlerMap, undefined, Opts)}
			end;
		error -> {add_key, hb_maps:get(func, HandlerMap, undefined, Opts)}
	end.
```

### find_exported_function

Find the function with the highest arity that has the given name, if it

```erlang
find_exported_function(Msg, Dev, Key, MaxArity, Opts) when is_map(Dev) ->
	case hb_maps:get(normalize_key(Key), normalize_keys(Dev, Opts), not_found, Opts) of
		not_found -> not_found;
		Fun when is_function(Fun) ->
			case erlang:fun_info(Fun, arity) of
				{arity, Arity} when Arity =< MaxArity ->
					case is_exported(Msg, Dev, Key, Opts) of
						true -> {ok, Fun};
						false -> not_found
					end;
				_ -> not_found
			end
	end;
```

### find_exported_function

Find the function with the highest arity that has the given name, if it

```erlang
find_exported_function(_Msg, _Mod, _Key, Arity, _Opts) when Arity < 0 ->
    not_found;
```

### find_exported_function

Find the function with the highest arity that has the given name, if it

```erlang
find_exported_function(Msg, Mod, Key, Arity, Opts) when not is_atom(Key) ->
	try hb_util:key_to_atom(Key, false) of
		KeyAtom -> find_exported_function(Msg, Mod, KeyAtom, Arity, Opts)
	catch _:_ -> not_found
	end;
```

### find_exported_function

Find the function with the highest arity that has the given name, if it

```erlang
find_exported_function(Msg, Mod, Key, Arity, Opts) ->
	case erlang:function_exported(Mod, Key, Arity) of
		true ->
			case is_exported(Msg, Mod, Key, Opts) of
				true -> {ok, fun Mod:Key/Arity};
				false -> not_found
			end;
		false ->
			find_exported_function(Msg, Mod, Key, Arity - 1, Opts)
	end.
```

### is_exported

Check if a device is guarding a key via its `exports` list. Defaults to

```erlang
is_exported(_Msg, _Dev, info, _Opts) -> true;
```

### is_exported

Check if a device is guarding a key via its `exports` list. Defaults to

```erlang
is_exported(Msg, Dev, Key, Opts) ->
	is_exported(info(Dev, Msg, Opts), Key, Opts).
```

### is_exported

```erlang
is_exported(_, info, _Opts) -> true;
```

### is_exported

```erlang
is_exported(Info = #{ excludes := Excludes }, Key, Opts) ->
    case lists:member(normalize_key(Key), lists:map(fun normalize_key/1, Excludes)) of
        true -> false;
        false -> is_exported(hb_maps:remove(excludes, Info, Opts), Key, Opts)
    end;
```

### is_exported

```erlang
is_exported(#{ exports := Exports }, Key, _Opts) ->
    lists:member(normalize_key(Key), lists:map(fun normalize_key/1, Exports));
```

### is_exported

Convert a key to a binary in normalized form.

```erlang
is_exported(_Info, _Key, _Opts) -> true.
```

### normalize_key

Convert a key to a binary in normalized form.

```erlang
normalize_key(Key) -> normalize_key(Key, #{}).
```

### normalize_key

Convert a key to a binary in normalized form.

```erlang
normalize_key(Key, _Opts) when is_binary(Key) -> Key;
```

### normalize_key

Convert a key to a binary in normalized form.

```erlang
normalize_key(Key, _Opts) when is_atom(Key) -> atom_to_binary(Key);
```

### normalize_key

Convert a key to a binary in normalized form.

```erlang
normalize_key(Key, _Opts) when is_integer(Key) -> integer_to_binary(Key);
```

### normalize_key

Convert a key to a binary in normalized form.

```erlang
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
```

### normalize_keys

Ensure that a message is processable by the AO-Core resolver: No lists.

```erlang
normalize_keys(Msg) -> normalize_keys(Msg, #{}).
```

### normalize_keys

Ensure that a message is processable by the AO-Core resolver: No lists.

```erlang
normalize_keys(Msg1, Opts) when is_list(Msg1) ->
    normalize_keys(
		hb_maps:from_list(
        	lists:zip(
            	lists:seq(1, length(Msg1)),
            	Msg1
			)
        ),
		Opts
	);
```

### normalize_keys

Ensure that a message is processable by the AO-Core resolver: No lists.

```erlang
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
```

### normalize_keys

Ensure that a message is processable by the AO-Core resolver: No lists.
Load a device module from its name or a message ID.

```erlang
normalize_keys(Other, _Opts) -> Other.
```

### load_device

Ensure that a message is processable by the AO-Core resolver: No lists.
Load a device module from its name or a message ID.

```erlang
load_device(Map, _Opts) when is_map(Map) -> {ok, Map};
```

### load_device

Ensure that a message is processable by the AO-Core resolver: No lists.
Load a device module from its name or a message ID.

```erlang
load_device(ID, _Opts) when is_atom(ID) ->
    try ID:module_info(), {ok, ID}
    catch _:_ -> {error, not_loadable}
    end;
```

### load_device

Ensure that a message is processable by the AO-Core resolver: No lists.
Load a device module from its name or a message ID.

```erlang
load_device(ID, Opts) when ?IS_ID(ID) ->
    ?event(device_load, {requested_load, {id, ID}}, Opts),
	case hb_opts:get(load_remote_devices, false, Opts) of
        false ->
            {error, remote_devices_disabled};
		true ->
            ?event(device_load, {loading_from_cache, {id, ID}}, Opts),
			{ok, Msg} = hb_cache:read(ID, Opts),
            ?event(device_load, {received_device, {id, ID}, {msg, Msg}}, Opts),
            TrustedSigners = hb_opts:get(trusted_device_signers, [], Opts),
			Trusted =
				lists:any(
					fun(Signer) ->
						lists:member(Signer, TrustedSigners)
					end,
					hb_message:signers(Msg, Opts)
				),
            ?event(device_load,
                {verifying_device_trust,
                    {id, ID},
                    {trusted, Trusted},
                    {signers, hb_message:signers(Msg, Opts)}
                },
                Opts
            ),
			case Trusted of
				false -> {error, device_signer_not_trusted};
				true ->
                    ?event(device_load, {loading_device, {id, ID}}, Opts),
					case hb_maps:get(<<"content-type">>, Msg, undefined, Opts) of
						<<"application/beam">> ->
                            case verify_device_compatibility(Msg, Opts) of
                                ok ->
                                    ModName =
                                        hb_util:key_to_atom(
                                            hb_maps:get(
                                                <<"module-name">>,
                                                Msg,
                                                undefined,
                                                Opts
                                            ),
                                            new_atoms
                                        ),
                                    LoadRes = 
                                        erlang:load_module(
                                            ModName,
                                            hb_maps:get(
                                                <<"body">>,
                                                Msg,
                                                undefined,
                                                Opts
                                            )
                                        ),
                                    case LoadRes of
                                        {module, _} ->
                                            {ok, ModName};
                                        {error, Reason} ->
                                            {error, {device_load_failed, Reason}}
                                    end;
                                {error, Reason} ->
                                    {error, {device_load_failed, Reason}}
                            end;
                        Other ->
                            {error,
                                {device_load_failed,
                                    {incompatible_content_type, Other},
                                    {expected, <<"application/beam">>},
                                    {found, Other}
                                }
                            }
                    end
			end
	end;
```

### load_device

Ensure that a message is processable by the AO-Core resolver: No lists.
Load a device module from its name or a message ID.

```erlang
load_device(ID, Opts) ->
    NormKey =
        case is_atom(ID) of
            true -> ID;
            false -> normalize_key(ID)
        end,
    case lists:search(
        fun (#{ <<"name">> := Name }) -> Name =:= NormKey end,
        Preloaded = hb_opts:get(preloaded_devices, [], Opts)
    ) of
        false -> {error, {module_not_admissable, NormKey, Preloaded}};
        {value, #{ <<"module">> := Mod }} -> load_device(Mod, Opts)
    end.
```

### verify_device_compatibility

Verify that a device is compatible with the current machine.

```erlang
verify_device_compatibility(Msg, Opts) ->
    ?event(device_load, {verifying_device_compatibility, {msg, Msg}}, Opts),
    Required =
        lists:filtermap(
            fun({<<"requires-", Key/binary>>, Value}) ->
                {true,
                    {
                        hb_util:key_to_atom(
                            hb_ao:normalize_key(Key),
                            new_atoms
                        ),
                        hb_cache:ensure_loaded(Value, Opts)
                    }
                };
            (_) -> false
            end,
            hb_maps:to_list(Msg, Opts)
        ),
    ?event(device_load,
        {discerned_requirements,
            {required, Required},
            {msg, Msg}
        },
        Opts
    ),
    FailedToMatch =
        lists:filtermap(
            fun({Property, Value}) ->
                % The values of these properties are _not_ 'keys', but we normalize
                % them as such in order to make them comparable.
```

### info

Get the info map for a device, optionally giving it a message if the

```erlang
info(Msg, Opts) ->
    info(message_to_device(Msg, Opts), Msg, Opts).
```

### info

```erlang
info(DevMod, Msg, Opts) ->
	%?event({calculating_info, {dev, DevMod}, {msg, Msg}}),
    case find_exported_function(Msg, DevMod, info, 2, Opts) of
		{ok, Fun} ->
			Res = apply(Fun, truncate_args(Fun, [Msg, Opts])),
			% ?event({
            %     info_result,
            %     {dev, DevMod},
            %     {args, truncate_args(Fun, [Msg])},
            %     {result, Res}
            % }),
			Res;
		not_found -> #{}
	end.
```

### default_module

The default device is the identity device, which simply returns the
The execution options that are used internally by this module

```erlang
default_module() -> dev_message.
```

### internal_opts

The default device is the identity device, which simply returns the
The execution options that are used internally by this module

```erlang
internal_opts(Opts) ->
    hb_maps:merge(Opts, #{
        topic => hb_opts:get(topic, ao_internal, Opts),
        hashpath => ignore,
        cache_control => [<<"no-cache">>, <<"no-store">>],
        spawn_worker => false,
        await_inprogress => false
```

---

*Generated from [hb_ao.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_ao.erl)*
