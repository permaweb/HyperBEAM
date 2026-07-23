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
%%% </pre>
-module(hb_ao).
%%% Main AO-Core API:
-export([resolve/2, resolve/3]).
-export([raw/3, raw/4, raw/5]).
%% Execution-context API used by hashpath verification.
-export([do/1]).
-export([with/3]).
-export([normalize_key/1, normalize_key/2, normalize_keys/1, normalize_keys/2]).
%%% Shortcuts and tools:
-export([keys/2]).
-export([as/3, get/3, get/4, get_first/2, get_first/3]).
-export([set/3, set/4, deep_set/3, remove/3]).
%%% Exports for tests in hb_ao_test_vectors.erl:
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(
    TEMP_OPTS,
    [
        <<"cache-control">>,
        <<"spawn-worker">>,
        <<"only">>,
        <<"prefer">>
    ]
).

%% @doc Small helper to extend a message with a device.
as(Device, Link, Opts) when ?IS_LINK(Link) ->
    as(Device, hb_cache:ensure_loaded(Link, Opts), Opts);
as(Device, Msg, _Opts) when is_map(Msg) ->
    hb_private:set_priv(
        #{
            <<"device">> => Device,
            <<"...">> => Msg
        },
        hb_private:from_message(Msg)
    ).

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
    % ?prim_dbg(
    %     {executing,
    %         {forced_device, ForcedDevice},
    %         {forced_key, ForcedKey}
    %     }
    % ),
    from_context(
        do(
            to_context(
                ForcedDevice,
                ForcedKey,
                Base,
                Req,
                Opts#{
                    <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
                    <<"await-inprogress">> => false,
                    <<"hashpath">> => ignore
                }
            )
        )
    ).

%% @doc Resolve a single AO-Core base and request pair or singleton message.
%% If the `path` in the request has multiple parts, each is executed in sequence,
%% over the original base message.
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
do_resolve_many([Res], _Opts) ->
    ?event_debug(debug_ao_core, {stage, 11, resolve_complete, Res}, _Opts),
    {ok, Res};
do_resolve_many([Base, Req | MsgList], Opts) ->
    ?event_debug(debug_ao_core, {stage, 0, resolve_many, {base, Base}, {req, Req}}),
    case resolve_single(Base, Req, Opts) of
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
            % The result is not a continuable message. Return it.
            ?event_debug(debug_ao_core, {stage, 13, resolve_many_terminating, Res}),
            Res
    end.

%% @doc Resolve only a single, explicit, computation step over a path-normalized
%% (single part) `Base` and `Req` pair.
resolve_single(Base, Req, Opts) ->
    from_context(do(to_context(undefined, undefined, Base, Req, Opts))).

%% @doc Create an execution context from a set of core parameters:
%% `ForcedDevice` (if relevant), `ForcedKey` (if relevant), `Base`, `Req`,
%% and `Opts`.
to_context(FDevice, FKey, Base, Req, Opts) ->
    maps:filter(
        fun(_K, V) -> V =/= undefined end,
        #{
            <<"device">> => FDevice,
            <<"path">> => FKey,
            <<"base">> => Base,
            <<"request">> => Req,
            <<"opts">> => Opts
        }
    ).
    
%% @doc Convert a completed execution context into the caller-facing
%% `{ExecStatus, Result}` form from its stage-bound `context`. 
from_context({ok, Ctx = #{ <<"result">> := Res }}) ->
    {maps:get(<<"status">>, Ctx, ok), Res};
from_context({error, #{ <<"reason">> := Reason }}) ->
    {error, Reason};
from_context({error, Reason}) ->
    {error, Reason};
from_context(Other) ->
    throw({unexpected_context_after_exec, Other}).

%% @doc Ensure that each of the given fields is present in an execution
%% context, deriving those that are absent from the components already known.
%% Derived fields are added to the context, such that repeated demands do not
%% repeat work. A field that is neither present nor derivable yields
%% `{not_found, Field}`.
with([], Ctx, _Opts) -> {ok, Ctx};
with([Field | Fields], Ctx, Opts) when is_map_key(Field, Ctx) ->
    with(Fields, Ctx, Opts);
with([Field | Fields], Ctx, Opts) ->
    case derive(Field, Ctx, Opts) of
        {ok, Value} -> with(Fields, Ctx#{ Field => Value }, Opts);
        _ -> {not_found, Field}
    end.

%% @doc Derive a single context field from its complementary components:
%% messages are read from the store by their IDs, while IDs are calculated
%% from their messages. A request may also be derived from a literal path
%% (e.g. `*`), which names itself. Messages read from the store are lazily
%% loaded: their keys are only read when they are themselves demanded.
derive(<<"base">>, #{ <<"base-id">> := ID }, Opts) ->
    hb_cache:read(ID, Opts);
derive(<<"request">>, #{ <<"request-id">> := ID }, Opts) when ?IS_ID(ID) ->
    hb_cache:read(ID, Opts);
derive(<<"request">>, #{ <<"request-id">> := Path }, _Opts) ->
    {ok, #{ <<"path">> => Path }};
derive(<<"varied-base">>, #{ <<"varied-base-id">> := ID }, Opts) ->
    hb_cache:read(ID, Opts);
derive(<<"varied-request">>, #{ <<"varied-request-id">> := ID }, Opts) ->
    hb_cache:read(ID, Opts);
derive(Field, Ctx, Opts) ->
    % Every `x-id` field is derivable from its `x` message, if present.
    case binary:split(Field, <<"-id">>) of
        [Name, <<>>] when is_map_key(Name, Ctx) ->
            {ok, hb_message:id(maps:get(Name, Ctx), all, Opts)};
        _ -> {not_found, Field}
    end.

%% @doc Resolves a fully normalized execution context. Stages that find the
%% result early set it in the context; later stages skip themselves when they
%% see it. Any non-`{ok, Ctx}` return propagates as the result.
do(Ctx0) ->
    maybe
        % Stage 1: Normalization; device or direct key lookup.
        {ok, Ctx1} ?= stage_1(Ctx0),
        % Stage 2: Function lookup.
        {ok, Ctx2} ?= stage_2(Ctx1),
        % Stage 3: Vary `Base` and `Req`.
        {ok, Ctx3} ?= stage_3(Ctx2),
        % Stage 4: Persistent resolver lookup.
        {ok, Ctx4} ?= stage_4(Ctx3),
        % Stage 5: Cache lookup.
        {ok, Ctx5} ?= stage_5(Ctx4),
        % Stage 6: Execution.
        {ok, Ctx6} ?= stage_6(Ctx5),
        % Stage 7: `Normalizer` application and hashpath generation
        {ok, Ctx7} ?= stage_7(Ctx6),
        % Stage 8: Result caching.
        {ok, Ctx8} ?= stage_8(Ctx7),
        % Stage 9: Notify waiters.
        {ok, Ctx9} ?= stage_9(Ctx8),
        % Stage 10: Execution of the `step' hook.
        {ok, Ctx10} ?= stage_10(Ctx9),
        % Stage 11: Fork worker.
        stage_11(Ctx10)
    else
        {hit, Ctx} ->
            % A stage may choose to return early. If it does, return it in
            % `ok`-form. We assume that the stage in question will have 
            % appropriately cleaned up.
            {ok, Ctx};
        {error, Ctx} ->
            % The stage returned an error. Return to the caller early after
            % unregistering as the active worker, if appropriate. We can call
            % this twice, even if we erred after stage 8, as it is idempotent.
            stage_8(Ctx),
            {error, Ctx}
    end.

%% @doc Normalize the context of an execution request. Ensures that a device and
%% path are available for execution, or that a direct key hit resolves the
%% request without one.
stage_1(Ctx = #{ <<"device">> := _, <<"path">> := _ }) -> {ok, Ctx};
stage_1(Ctx = #{ <<"base">> := Base, <<"path">> := Path, <<"opts">> := Opts }) ->
    case hb_device:id_or_direct_key(Base, Path, Opts) of
        {hit, Res} -> {hit, Ctx#{ <<"status">> => ok, <<"result">> => Res }};
        {ok, Device} -> stage_1(Ctx#{ <<"device">> => Device });
        {error, Reason} ->
            {error, Ctx#{ <<"status">> => error, <<"reason">> => Reason }}
    end;
stage_1(Ctx = #{ <<"request">> := Req, <<"opts">> := Opts }) when is_map(Req) ->
    stage_1(Ctx#{ <<"path">> => hb_path:hd(Req, Opts) });
stage_1(Ctx = #{ <<"request">> := Key }) ->
    % If the request is for a direct key we normalize it to a message with 
    % only a path of that key.
    stage_1(Ctx#{ <<"request">> => #{ <<"path">> => normalize_key(Key) } });
stage_1(Ctx = #{ <<"opts">> := Opts }) ->
    % The context does not carry its messages directly. Derive the `base` and
    % `request` from the components that are given (e.g. their IDs), then
    % normalize as usual.
    case with([<<"base">>, <<"request">>], Ctx, Opts) of
        {ok, Loaded} -> stage_1(Loaded);
        {not_found, Field} ->
            {error, Ctx#{ <<"status">> => error, <<"reason">> => {not_found, Field} }}
    end.

%% @doc Lookup the device and function to use during an execution.
stage_2(
        Ctx = #{
            <<"device">> := Device,
            <<"base">> := Base,
            <<"path">> := Path,
            <<"opts">> := Opts
        }
) ->
    {Status, ExecDev, ExecMod, Function} =
        hb_device:message_to_fun(Device, Base, Path, Opts),
    AddKey =
        case Status of
            add_key -> Path;
            _ -> false
        end,
    hb_types:add_schema(
        Ctx#{
            <<"key">> => Path,
            <<"resolver-device">> => ExecDev,
            <<"add-key">> => AddKey,
            <<"priv">> =>
                #{
                    <<"resolver-module">> => ExecMod,
                    <<"add-key">> => AddKey =/= false,
                    <<"function">> => Function
                }
        },
        Opts
    ).

%% @doc Vary the `Base` and `Req` of the resolution. Stage 2 has already
%% resolved the executor and attached its schema when available, so normal
%% execution varies directly through the schema projector.
stage_3(Ctx = #{ <<"path">> := <<"vary">>, <<"base">> := Base, <<"request">> := Req }) ->
    % We are already varying. Do not recurse.
    hb_types:vary(
        maps:remove(<<"schema">>, Ctx#{
            <<"base">> => Base,
            <<"request">> => Req
        }),
        maps:get(<<"opts">>, Ctx, #{})
    );
stage_3(
        Ctx =
            #{
                <<"base">> := _Base,
                <<"request">> := _Req,
                <<"priv">> := #{ <<"function">> := _Function },
                <<"opts">> := Opts
            }
) ->
    hb_types:vary(Ctx, Opts).

%% @doc Determine if the request is already being resolved right now. If so,
%% await the result rather than resolving again. If not, register as the
%% resolver for the request in question and proceed. If we are executing with
%% `no-cache`, skip the lookup and proceed with the current context.
stage_4(
        Ctx = #{
            <<"varied-base">> := Base,
            <<"varied-request">> := Req,
            <<"opts">> := Opts
        }) ->
    ?event_debug(debug_ao_core, {stage, 4, persistent_resolver_lookup}, Opts),
    % Persistent-resolver lookup: Search for local (or Distributed
    % Erlang cluster) processes that are already performing the execution.
    % Before we search for a live executor, we check if the device specifies
    % a function that tailors the 'group' name of the execution. For example,
    % the `~process@1.0' device 'groups' all calls to the same process onto
    % calls to a single executor. By default, `{Base, Req}' is used as the
    % group name.
    case hb_persistent:find_or_register(Base, Req, Opts) of
        {skip, ungrouped_exec} -> {ok, Ctx};
        {leader, ExecName} -> {ok, Ctx#{ <<"leader">> => ExecName }};
        {wait, Leader} ->
            % There is another executor of this resolution in-flight.
            % register to receive the response, then
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
                    stage_4(Ctx);
                {ok, Result} ->
                    % We received a successful result from another worker. They
                    % will store the result if appropriate, so we skip the store
                    % write.
                    {
                        ok,
                        Ctx#{
                            <<"status">> => ok,
                            <<"varied-result">> => Result
                        }
                    };
                {Status, Result} ->
                    {ok, Ctx#{ <<"status">> => Status, <<"result">> => Result }};
                Other ->
                    % The leader resolved to a non-`ok` result. Propagate it.
                    ?event_debug(debug_ao_core, {unexpected_worker_result, Other}),
                    Other
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
                    % No need to register, as we are the leader.
                    {ok, Ctx};
                false ->
                    % We are not OK with infinite loops, so we raise an error.
                    {
                        error,
                        #{
                            <<"status">> => 508,
                            <<"body">> => <<"Request creates infinite recursion.">>
                        }
                    }
            end
    end.

%% @doc Look up whether the varied base and request pair already has a known
%% result in the cache. If so, set it in the context: subsequent stages skip
%% execution, and stage 8 notifies any waiting callers.
stage_5(Ctx = #{
    <<"varied-base">> := Base,
    <<"varied-request">> := Req,
    <<"opts">> := Opts
}) ->
    case hb_cache_control:maybe_lookup(Base, Req, Opts) of
        {continue, NewBase, NewReq} ->
            {
                ok,
                Ctx#{
                    <<"varied-base">> => NewBase,
                    <<"varied-request">> => NewReq
                }
            };
        {error, not_found} -> {ok, Ctx};
        {ok, Result} ->
            {
                ok,
                Ctx#{
                    <<"varied-result">> => Result,
                    <<"status">> => ok
                }
            };
        {Status, Result} ->
            {ok, Ctx#{ <<"result">> => Result, <<"status">> => Status }}
    end.

%% @doc Perform the actual resolution of an AO-Core request.
stage_6(Ctx = #{ <<"result">> := _ }) -> {ok, Ctx};
stage_6(Ctx = #{ <<"varied-result">> := _ }) -> {ok, Ctx};
stage_6(Ctx = #{
    <<"varied-base">> := Base,
    <<"varied-request">> := Req,
    <<"opts">> := Opts,
    <<"priv">> := #{ <<"function">> := Func }
}) ->
    ExecOpts = execution_opts(Opts),
    ExecName = maps:get(<<"leader">>, Ctx, unnamed),
    ?event_debug(debug_ao_core, {stage, 6, ExecName, execution}, Opts),
    Args =
        case maps:get(<<"add-key">>, Ctx, false) of
            false -> [Base, Req, ExecOpts];
            Key -> [Key, Base, Req, ExecOpts]
        end,
    % Try to execute the function.
    {Status, Res} = maybe_profiled_apply(ExecName, Func, Args, Base, Req, Opts),
    hb_message:paranoid_verify(
        post_resolve,
        #{
            <<"reason">> => <<"AO-Core Post-Execution Validation">>,
            <<"base">> => Base,
            <<"request">> => Req,
            <<"varied-result">> => Res
        },
        Opts
    ),
    {
        ok,
        Ctx#{
            <<"status">> => Status,
            <<"varied-result">> => Res,
            <<"fresh">> => true
        }
    }.

%% @doc When specified in the schema for the device call, normalize the
%% execution's result on top of the `Base` or `Req` message. In cases where
%% hashpath calculation is disabled (`hashpath => ignore` in the node message,
%% etc.), the result is left without a generated private hashpath.
stage_7(Ctx = #{ <<"result">> := _, <<"status">> := Status }) when Status =/= ok ->
    {ok, Ctx};
stage_7(Ctx = #{ <<"varied-result">> := Result, <<"status">> := Status }) when Status =/= ok ->
    {ok, Ctx#{ <<"result">> => Result }};
stage_7(Ctx = #{ <<"base">> := Base, <<"opts">> := Opts }) ->
    maybe
        {ok, Result0} ?= hb_hashpath:result_from_context(Base, Ctx, Opts),
        Result = hb_hashpath:with_context_hashpath(Result0, Ctx, Opts),
        {ok, Ctx#{ <<"result">> => Result }}
    end.

%% @doc Cache the result of an execution if appropriate for the context.
%% Results that were not freshly and successfully executed (cache hits, direct
%% key hits, awaited results, non-`ok` statuses) are not stored.
stage_8(
    Ctx = #{
        <<"status">> := ok,
        <<"fresh">> := true,
        <<"varied-base">> := VariedBase,
        <<"varied-request">> := VariedReq,
        <<"varied-result">> := VariedRes,
        <<"opts">> := Opts
    }
) ->
    hb_cache_control:maybe_store(VariedBase, VariedReq, VariedRes, Opts),
    {ok, Ctx};
stage_8(Ctx) -> {ok, Ctx}.

%% @doc Return the resolved response to any waiting callers.
stage_9(
    Ctx = #{
        <<"leader">> := ExecName,
        <<"varied-request">> := Req,
        <<"varied-result">> := Res,
        <<"status">> := Status,
        <<"opts">> := Opts
    }
) ->
    hb_persistent:unregister_notify(
        ExecName,
        Req,
        {Status, Res},
        Opts
    ),
    {ok, Ctx};
stage_9(Ctx) ->
    % If we are not the leader, we can ignore the unregister step.
    {ok, Ctx}.

%% @doc If a hook has been specified for the `step` action, we call it with our
%% context including the result.
stage_10(Ctx = #{ <<"opts">> := Opts = #{ <<"on">> := #{ <<"step">> := _ }}}) ->
    ?event_debug(debug_ao_core, {stage, 7, executing_step_hook}, Opts),
    % If the `step' hook is defined, we execute it. Note: This function clause
    % matches directly on the `on' key of the `Opts' map. This is in order to
    % remove the expensive lookup check that would otherwise be performed on every
    % execution.
    hb_hook:on(<<"step">>, Ctx, Opts);
stage_10(Ctx) -> {ok, Ctx}.

%% @doc If we have been requested to spawn a worker process to remain active
%% at this stage, with this context in memory, we do so.
stage_11(
    Ctx = #{
        <<"leader">> := ExecName,
        <<"result">> := Res,
        <<"opts">> := Opts
    }) ->
    ?event_debug(debug_ao_core, {stage, 11, maybe_spawn_worker}, Opts),
    % Check if we should fork out a new worker process for the current execution
    case
        {is_map(Res), hb_opts:get(<<"spawn-worker">>, false, Opts#{ <<"prefer">> => local })}
    of
        {A, B} when (A == false) or (B == false) ->
            {ok, Ctx};
        {_, _} ->
            % Spawn a worker for the current execution
            WorkerPID = hb_persistent:start_worker(ExecName, Res, Opts),
            hb_persistent:forward_work(WorkerPID, Opts),
            {ok, Ctx}
    end;
stage_11(Ctx) -> {ok, Ctx}.

%% @doc If the `AO_PROFILING' macro is defined (set by building/launching with
%% `rebar3 as ao_profiling') we record statistics about the execution of the
%% function. This is a costly operation, so if it is not defined, we simply
%% apply the function and return the result.
-ifndef(AO_PROFILING).
maybe_profiled_apply(ExecName, Func, Args, Base, Req, Opts) ->
    do_apply(ExecName, Func, Args, Base, Req, Opts).
-else.
maybe_profiled_apply(ExecName, Func, Args, Base, Req, Opts) ->
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
    {ExecMicroSecs, Res} =
        timer:tc(
            fun() ->
                do_apply(ExecName, Func, Args, Base, Req, Opts)
            end
        ),
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

%% @doc Execute a device call, wrapped with failure handling.
do_apply(ExecName, Func, Args, Base, Req, Opts) ->
    try
        case apply(Func, Truncated = hb_device:truncate_args(Func, Args)) of
            {Status, Res} -> {Status, Res};
            Other ->
                throw(
                    {unexpected_device_call_response,
                        #{
                            <<"exec-name">> => ExecName,
                            <<"args">> => Truncated,
                            <<"result">> => Other,
                            <<"func">> => Func
                        }
                    }
                )
        end
    catch
        Class:Exception:Stacktrace ->
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
                    {exec_class, Class},
                    {exec_exception, Exception},
                    {exec_stacktrace, erlang:process_info(self(), backtrace)},
                    {opts, Opts}
                },
					Opts
            ),
            % If the function call fails, we raise an error in the manner
            % indicated by caller's `#Opts', as well as returning the error to
            % any registered listeners.
            Error =
                {failure,
                    #{
                        <<"class">> => Class,
                        <<"exception">> => Exception,
                        <<"stacktrace">> => Stacktrace
                    }
                },
            hb_persistent:unregister_notify(ExecName, Req, Error, Opts),
            case hb_opts:get(error_strategy, throw, Opts) of
                throw -> erlang:raise(Class, Exception, Stacktrace);
                _ -> Error
            end
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
keys(Msg, Opts) -> get(<<"keys">>, Msg, Opts).

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
            Deep([NextKey|Rest]) -> #{ NextKey => Deep(Rest) }
        end,
    deep_set(Base, DeepBase(hb_path:term_to_path_parts(Key, Opts)), Opts).

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
%% the map as-is. This is the overwhelming majority case, and skips the
%% `hb_maps:to_list'/`from_list' round-trip and per-key dispatch.
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

stage_7_sets_result_hashpath_test() ->
    Opts = #{},
    Base = #{ <<"x">> => 1 },
    Req = #{ <<"path">> => <<"inc-x">> },
    Patch = #{ <<"x">> => 2 },
    Deps = #{ <<"base">> => #{}, <<"request">> => #{} },
    Ctx0 = #{
        <<"base">> => Base,
        <<"request">> => Req,
        <<"varied-base">> => Base,
        <<"varied-request">> => Req,
        <<"dependencies">> => Deps,
        <<"normalizer">> => base,
        <<"varied-result">> => Patch,
        <<"opts">> => Opts
    },
    {ok, Ctx1} = stage_7(Ctx0),
    Result = maps:get(<<"result">>, Ctx1),
    ?assertEqual(
        hb_hashpath:format(Ctx0, Opts),
        maps:get(<<"hashpath">>, hb_private:from_message(Result))
    ),
    ?assertEqual(Patch#{ <<"...">> => Base }, hb_private:reset(Result)).

stage_7_skips_hashpath_without_dependencies_test() ->
    Opts = #{},
    Base = #{ <<"x">> => 1 },
    Req = #{ <<"path">> => <<"inc-x">> },
    Patch = #{ <<"x">> => 2 },
    Ctx0 = #{
        <<"base">> => Base,
        <<"request">> => Req,
        <<"varied-base">> => Base,
        <<"varied-request">> => Req,
        <<"normalizer">> => base,
        <<"varied-result">> => Patch,
        <<"opts">> => Opts
    },
    {ok, Ctx1} = stage_7(Ctx0),
    Result = maps:get(<<"result">>, Ctx1),
    ?assertEqual(
        undefined,
        maps:get(<<"hashpath">>, hb_private:from_message(Result), undefined)
    ).

stage_3_live_schema_generates_dependencies_test() ->
    Opts = #{},
    Base = #{ <<"a">> => 1 },
    Req = #{ <<"path">> => <<"keys">>, <<"keys">> => <<"deep">> },
    {ok, Ctx1} = stage_1(to_context(undefined, undefined, Base, Req, Opts)),
    {ok, Ctx2} = stage_2(Ctx1),
    ?assert(maps:is_key(<<"schema">>, Ctx2)),
    {ok, Ctx3} = stage_3(Ctx2),
    ?assert(maps:is_key(<<"dependencies">>, Ctx3)),
    ?assertMatch(
        #{ <<"base">> := _, <<"request">> := _ },
        maps:get(<<"dependencies">>, Ctx3)
    ).

stage_3_live_schema_records_coerced_dependency_test() ->
    Opts = #{},
    Base = #{ <<"device">> => <<"math@1.0">>, <<"x">> => <<"1">> },
    Req = #{ <<"path">> => <<"add-x">>, <<"add">> => <<"2">> },
    {ok, Ctx1} = stage_1(to_context(undefined, undefined, Base, Req, Opts)),
    {ok, Ctx2} = stage_2(Ctx1),
    {ok, Ctx3} = stage_3(Ctx2),
    BaseID = hb_message:id(Base, all, Opts),
    ReqID = hb_message:id(Req, all, Opts),
    ?assertEqual(
        #{ <<"device">> => <<"math@1.0">>, <<"x">> => 1 },
        maps:get(<<"varied-base">>, Ctx3)
    ),
    ?assertEqual(
        #{ <<"path">> => <<"add-x">>, <<"add">> => 2 },
        maps:get(<<"varied-request">>, Ctx3)
    ),
    ?assertEqual(
        #{
            <<"status">> => found,
            <<"origin">> => <<BaseID/binary, "/x">>,
            <<"observed">> => <<"1">>,
            <<"value">> => 1
        },
        maps:get(<<"x">>, maps:get(<<"base">>, maps:get(<<"dependencies">>, Ctx3)))
    ),
    ?assertEqual(
        #{
            <<"status">> => found,
            <<"origin">> => <<ReqID/binary, "/add">>,
            <<"observed">> => <<"2">>,
            <<"value">> => 2
        },
        maps:get(<<"add">>, maps:get(<<"request">>, maps:get(<<"dependencies">>, Ctx3)))
    ).

stage_3_vary_bypass_generates_dependencies_test() ->
    Opts = #{},
    Base = #{ <<"x">> => 1 },
    Req = #{ <<"path">> => <<"vary">> },
    {ok, Ctx} = stage_3(#{ <<"path">> => <<"vary">>, <<"base">> => Base, <<"request">> => Req, <<"opts">> => Opts }),
    ?assertEqual(Base, maps:get(<<"varied-base">>, Ctx)),
    ?assertEqual(Req, maps:get(<<"varied-request">>, Ctx)),
    ?assertMatch(#{ <<"base">> := _, <<"request">> := _ }, maps:get(<<"dependencies">>, Ctx)).

stage_7_respects_hashpath_ignore_test() ->
    Opts = #{ <<"hashpath">> => ignore },
    Base = #{ <<"x">> => 1 },
    Patch = #{ <<"x">> => 2 },
    Ctx0 = #{
        <<"base">> => Base,
        <<"normalizer">> => base,
        <<"varied-result">> => Patch,
        <<"opts">> => Opts
    },
    {ok, Ctx1} = stage_7(Ctx0),
    Result = maps:get(<<"result">>, Ctx1),
    ?assertEqual(undefined, maps:get(<<"hashpath">>, hb_private:from_message(Result), undefined)),
    ?assertEqual(Patch#{ <<"...">> => Base }, Result).

message_set_drops_parent_hashpath_when_hashpath_ignored_test() ->
    Opts = #{ <<"hashpath">> => ignore },
    Base =
        hb_private:set_priv(
            #{ <<"x">> => 1 },
            #{ <<"hashpath">> => <<"ParentHP">>, <<"token">> => <<"keep">> }
        ),
    Req = #{ <<"path">> => <<"set">>, <<"y">> => 2 },
    {ok, Result} = resolve(Base, Req, Opts),
    Priv = hb_private:from_message(Result),
    ?assertEqual(undefined, maps:get(<<"hashpath">>, Priv, undefined)),
    ?assertEqual(<<"keep">>, maps:get(<<"token">>, Priv)),
    ?assertEqual(#{ <<"x">> => 1 }, maps:get(<<"...">>, hb_private:reset(Result))).

stage_7_preserves_non_ok_materialized_result_test() ->
    Ctx = #{
        <<"base">> => #{ <<"x">> => 1 },
        <<"normalizer">> => base,
        <<"result">> => #{ <<"error">> => true },
        <<"status">> => error,
        <<"opts">> => #{}
    },
    ?assertEqual({ok, Ctx}, stage_7(Ctx)).

stage_7_preserves_non_ok_varied_result_test() ->
    ErrorResult = #{ <<"error">> => true },
    Ctx = #{
        <<"base">> => #{ <<"x">> => 1 },
        <<"normalizer">> => base,
        <<"varied-result">> => ErrorResult,
        <<"status">> => error,
        <<"opts">> => #{}
    },
    ?assertEqual({ok, Ctx#{ <<"result">> => ErrorResult }}, stage_7(Ctx)).

binary_unset_masks_direct_key_test() ->
    Msg = #{ <<"visible">> => <<"ok">>, <<"masked">> => <<"unset">> },
    Opts = #{ <<"hashpath">> => ignore },
    ?assertEqual({error, not_found}, resolve(Msg, <<"masked">>, Opts)),
    ?assertEqual({ok, [<<"visible">>]}, resolve(Msg, keys, Opts)).

unset_masks_inherited_keys_test() ->
    Msg = #{
        <<"visible">> => <<"ok">>,
        <<"masked">> => <<"unset">>,
        <<"...">> => #{ <<"masked">> => <<"ancestor">>, <<"inherited">> => <<"yes">> }
    },
    Opts = #{ <<"hashpath">> => ignore },
    ?assertEqual({error, not_found}, resolve(Msg, <<"masked">>, Opts)),
    ?assertEqual({ok, <<"yes">>}, resolve(Msg, <<"inherited">>, Opts)),
    {ok, Keys} = raw(<<"message@1.0">>, <<"keys">>, Msg, #{ <<"keys">> => <<"deep">> }, Opts),
    ?assertEqual(lists:sort([<<"visible">>, <<"inherited">>]), lists:sort(Keys)).

lazy_unset_masks_inherited_keys_test() ->
    hb:init(),
    Opts = #{ <<"hashpath">> => ignore, <<"store">> => hb_test_utils:test_store() },
    Msg = #{
        <<"visible">> => <<"ok">>,
        <<"masked">> => <<"unset">>,
        <<"...">> => #{
            <<"masked">> => <<"ancestor">>,
            <<"parent">> => <<"yes">>,
            <<"...">> => #{ <<"grandparent">> => <<"yes">> }
        }
    },
    {ok, ID} = hb_cache:write(Msg, Opts),
    {ok, Loaded} = hb_cache:read(ID, Opts),
    {ok, Keys} =
        raw(
            <<"message@1.0">>,
            <<"keys">>,
            Loaded,
            #{ <<"keys">> => <<"deep">> },
            Opts
        ),
    ?assertEqual(
        lists:sort([<<"visible">>, <<"parent">>, <<"grandparent">>]),
        lists:sort(Keys)
    ).

scalar_base_key_resolution_returns_not_found_test() ->
    ?assertEqual({error, not_found}, resolve(1, <<"x">>, #{})).

unset_device_masks_inherited_device_test() ->
    Msg = #{
        <<"device">> => <<"unset">>,
        <<"local">> => <<"value">>,
        <<"...">> => #{ <<"device">> => <<"bad-device@1.0">> }
    },
    ?assertEqual({ok, [<<"local">>]}, resolve(Msg, keys, #{ <<"hashpath">> => ignore })).

cache_hit_applies_normalizer_test() ->
    hb:init(),
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"always">>]
    },
    Base = #{ <<"x">> => 1, <<"keep">> => true },
    Req = #{ <<"path">> => <<"inc-x">> },
    Patch = #{ <<"x">> => 2 },
    hb_cache_control:maybe_store(Base, Req, Patch, Opts),
    {ok, BaseID} = hb_cache:write(Base, Opts),
    {ok, ReqID} = hb_cache:write(Req, Opts),
    Ctx0 = #{
        <<"base">> => Base,
        <<"request">> => Req,
        <<"varied-base">> => BaseID,
        <<"varied-request">> => ReqID,
        <<"normalizer">> => base,
        <<"opts">> => Opts
    },
    {ok, Ctx1} = stage_5(Ctx0),
    ?assert(maps:is_key(<<"varied-result">>, Ctx1)),
    ?assertEqual(false, maps:is_key(<<"result">>, Ctx1)),
    {ok, Ctx2} = stage_6(Ctx1),
    {ok, Ctx3} = stage_7(Ctx2),
    Result = hb_cache:ensure_all_loaded(maps:get(<<"result">>, Ctx3), Opts),
    ?assertEqual(2, maps:get(<<"x">>, Result)),
    ?assertEqual(true, maps:get(<<"keep">>, maps:get(<<"...">>, Result))).

persistent_wait_applies_normalizer_test() ->
    hb:init(),
    Opts = #{ <<"await-inprogress">> => true },
    Base = #{ <<"x">> => 1, <<"keep">> => true },
    Req = #{ <<"path">> => <<"inc-x">> },
    Patch = #{ <<"x">> => 2 },
    GroupName = hb_persistent:group(Base, Req, Opts),
    Leader =
        spawn(
            fun() ->
                receive
                    {resolve, Waiter, GroupName, Req, _WaitOpts} ->
                        Waiter ! {resolved, self(), GroupName, Req, {ok, Patch}}
                end
            end
        ),
    ok = hb_name:register(GroupName, Leader),
    Ctx0 = #{
        <<"base">> => Base,
        <<"request">> => Req,
        <<"varied-base">> => Base,
        <<"varied-request">> => Req,
        <<"normalizer">> => base,
        <<"opts">> => Opts
    },
    {ok, Ctx1} = stage_4(Ctx0),
    hb_name:unregister(GroupName),
    ?assertMatch(#{ <<"varied-result">> := Patch }, Ctx1),
    ?assertEqual(false, maps:is_key(<<"result">>, Ctx1)),
    {ok, Ctx2} = stage_6(Ctx1),
    {ok, Ctx3} = stage_7(Ctx2),
    Result = maps:get(<<"result">>, Ctx3),
    ?assertEqual(2, maps:get(<<"x">>, Result)),
    ?assertEqual(true, maps:get(<<"keep">>, maps:get(<<"...">>, Result))).
