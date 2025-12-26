%%% @doc A library of common functions for building devices that interact with 
%%% the `~process@1.0` meta-device structure.
-module(dev_process_lib).
-include("include/hb.hrl").
-export([new/2, as_process/2, run_as/4, process_id/2, process_id/3]).
-export([set_results/3, ensure_process_key/2]).
-export([subscribe/3, unsubscribe/3]).
%%% Query wrappers.
-export([now/2, id/1, push/3, push/4]).
-export([subscribers/3, subscribers/4]).

new(ProcMap, Opts) ->
    HostWallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Proc =
        hb_message:commit(
            hb_maps:merge(
                #{
                    <<"device">> => <<"process@1.0">>,
                    <<"type">> => <<"Process">>,
                    <<"scheduler-device">> => <<"scheduler@1.0">>,
                    <<"push-device">> => <<"push@1.0">>,
                    <<"scheduler">> => hb_util:human_id(HostWallet),
                    <<"authority">> => hb_util:human_id(HostWallet)
                },
                ProcMap,
                Opts
            ),
            Opts#{ priv_wallet => HostWallet }
        ),
    hb_cache:write(Proc, Opts),
    Proc.

%% @doc Returns the process ID of the current process.
process_id(Base, Opts) ->
    process_id(Base, #{}, Opts).
process_id(Base, Req, Opts) ->
    case hb_ao:get(<<"process">>, Base, Opts#{ hashpath => ignore }) of
        not_found ->
            process_id(ensure_process_key(Base, Opts), Req, Opts);
        Process ->
            Signers = hb_message:signers(Process, Opts),
            case {hb_message:verify(Process, all, Opts), Signers} of
                {false, _} ->
                    ?event({process_not_verified, {process, Process}}),
                    throw({process_not_verified, Process});
                {true, []} ->
                    ?event({process_has_no_signers, {process, Process}}),
                    throw({process_has_no_signers, Process});
                {true, _} ->
                    hb_message:id(
                        Process,
                        hb_util:atom(
                            maps:get(<<"commitments">>, Req, <<"signed">>)
                        ),
                        Opts
                    )
            end
    end.

%% @doc Run a message against Base, with the device being swapped out for
%% the device found at `Key'. After execution, the device is swapped back
%% to the original device if the device is the same as we left it.
run_as(Key, Base, Path, Opts) when not is_map(Path) ->
    run_as(Key, Base, #{ <<"path">> => Path }, Opts);
run_as(Key, Base, Req, Opts) ->
    % Store the original device so we can restore it after execution
    BaseDevice = hb_maps:get(<<"device">>, Base, not_found, Opts),
    ?event(debug_as, {running_as, {key, Key}, {req, Req}}, Opts),
    % Prepare the message with the specialized device configuration.
    % This sets up the device context for the specific operation type.
    {ok, PreparedMsg} = dev_process:as(Base, Key, Opts),
    ?event(
        debug_run_as,
        {before_resolve, {prepared_msg, PreparedMsg}, {req, Req}},
        Opts
    ),
    % Execute the message through the specialized device.
    {Status, BaseResult} =
        hb_ao:resolve(
            PreparedMsg,
            Req,
            Opts
        ),
    ?event(
        debug_run_as,
        {after_resolve, {status, Status}, {base_result, BaseResult}},
        Opts
    ),
    % Restore the original device context after execution.
    % This ensures the process maintains its identity after device delegation.
    case {Status, BaseResult} of
        {ok, #{ <<"device">> := DeviceSet }} ->
            {ok, hb_ao:set(BaseResult, #{ <<"device">> => BaseDevice }, Opts)};
        _ ->
            ?event({returning_base_result, BaseResult}),
            {Status, BaseResult}
    end.

%% @doc Change the message to for that has the device set as this module.
%% In situations where the key that is `run_as' returns a message with a 
%% transformed device, this is useful.
as_process(Base, Opts) ->
    {ok, Proc} = 
        dev_message:set(Base, #{ <<"device">> => <<"process@1.0">> }, Opts),
    Proc.

%% @doc Set the results of the current process.
set_results(State, Results, Opts) ->
    {ok, hb_ao:set(State, #{ <<"results">> => Results }, Opts)}.


%% @doc Helper function to store a copy of the `process' key in the message.
ensure_process_key(Base, Opts) ->
    case hb_maps:get(<<"process">>, Base, not_found, Opts) of
        not_found ->
            % If the message has lost its signers, we need to re-read it from
            % the cache. This can happen if the message was 'cast' to a different
            % device, leading the signers to be unset.
            {ok, Committed} = hb_message:with_only_committed(Base, Opts),
            ?event(
                {process_key_before_set,
                    {base, Base},
                    {process_msg, Base},
                    {committed, Committed}
                }
            ),
            Res =
                hb_ao:set(
                    hb_message:uncommitted(Base, Opts),
                    #{ <<"process">> => Committed },
                    Opts#{ hashpath => ignore }
                ),
            ?event(
                {set_process_key_res,
                    {base, Base},
                    {process_msg, Base},
                    {res, Res}
                }
            ),
            Res;
        _ -> Base
    end.

%% @doc Subscribe to receive notifications upon a given `action' and (optionally)
%% `target' from a given process.
subscribe(ProcMsg, Action, Opts) ->
    subscribe(ProcMsg, Action, <<"broadcast">>, Opts).
subscribe(ProcMsg, Action, Target, Opts) ->
    push(
        ProcMsg,
        #{
            <<"action">> => <<"subscribe">>,
            <<"subscribe-action">> => Action,
            <<"subscribe-target">> => Target
        },
        Opts
    ).

%% @doc Unsubscribe from receiving notifications upon a given `action' and (optionally)
%% `target' from a given process.
unsubscribe(ProcMsg, Action, Opts) ->
    unsubscribe(ProcMsg, Action, <<"broadcast">>, Opts).
unsubscribe(ProcMsg, Action, Target, Opts) ->
    push(
        ProcMsg,
        #{
            <<"action">> => Action,
            <<"subscribe-action">> => Action,
            <<"subscribe-target">> => Target
        },
        Opts
    ).

%% @doc Get the current state of a process.
now(ProcMsg, Opts) ->
    {ok, State} = hb_ao:resolve(ProcMsg, #{ <<"path">> => <<"now">> }, Opts),
    State.

%% @doc Helper function to push a message to a process. Signs the message with the
%% default key in the `Opts'.
push(Process, Msg, RawOpts) ->
    push(Process, Msg, hb_opts:get(priv_wallet, hb:wallet(), RawOpts), RawOpts).
push(Process, Msg, MsgWallet, RawOpts) ->
    UserOpts = RawOpts#{ priv_wallet => MsgWallet },
    SystemOpts =
        RawOpts#{
            priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), RawOpts)
        },
    Req =
        hb_message:commit(
            #{
                <<"path">> => <<"push">>,
                <<"body">> =>
                    hb_message:commit(
                        Msg#{
                            <<"target">> =>
                                if is_binary(Process) ->
                                    Process;
                                true ->
                                    dev_process_lib:process_id(Process, SystemOpts)
                                end
                        },
                        UserOpts
                    )
            },
            UserOpts
        ),
    hb_ao:resolve(Process, Req, SystemOpts).

%% @doc Generate a random ID, or an 'ID' value of the correct length starting
%% with the given binary and padded with zeros.
id(AlreadyID) when is_binary(AlreadyID) -> AlreadyID;
id(Bin) when is_binary(Bin) ->
    BitSize = byte_size(Bin) * 8,
    Suffix = << 0:(256 - BitSize) >>,
    << Bin/binary, Suffix/binary >>;
id(Other) -> hb_util:human_id(Other).

%% @doc Get the subscribers of a process for a given action and target.
subscribers(ProcMsg, Action, Opts) ->
    subscribers(
        ProcMsg,
        Action,
        <<"broadcast">>,
        Opts
    ).
subscribers(ProcMsg, Action, Target, Opts) ->
    dev_process_outbox:subscribers(now(ProcMsg, Opts), Action, Target, Opts).